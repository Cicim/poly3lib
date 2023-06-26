use gba_macro::gba_struct;
use gba_types::GBAIOError;

use crate::{
    refs::{TableInitError, TablePointer},
    rom::{Rom, RomType},
};

use super::connection::MapConnections;
use super::events::MapEvents;

gba_struct!(EmeraldMapHeader {
    void *map_layout;
    struct MapEvents *events;
    void *map_scripts;
    struct MapConnections *connections;
    u16 music;
    u16 map_layout_id;
    u8 region_map_section_id;
    u8 cave;
    u8 weather;
    u8 map_type;
    u8 filler[2];
    u8 allow_cycling:1;
    u8 allow_escaping:1;
    u8 allow_running:1;
    u8 show_map_name:5;
    u8 battle_type;
} PRIVATE);

gba_struct!(MapHeader {
    void *map_layout;
    struct MapEvents *events;
    void *map_scripts;
    struct MapConnections *connections;
    u16 music;
    u16 map_layout_id;        // Has to correspond with the map layout
    u8 region_map_section_id;
    u8 cave;
    u8 weather;
    u8 map_type;
    u8 biking_allowed;
    u8 allow_escaping:1;
    u8 allow_running:1;
    u8 show_map_name:6;
    i8 floor_num;
    u8 battle_type;
});

impl MapHeader {
    /// Reads the [`MapHeader`] taking into account the game version.
    pub fn read(rom: &Rom, offset: usize) -> Result<Self, GBAIOError> {
        let res = match rom.rom_type {
            RomType::FireRed | RomType::LeafGreen => rom.read::<MapHeader>(offset)?,
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => {
                let value = rom.read::<EmeraldMapHeader>(offset)?;
                MapHeader {
                    map_layout: value.map_layout,
                    events: value.events,
                    map_scripts: value.map_scripts,
                    connections: value.connections,
                    music: value.music,
                    map_layout_id: value.map_layout_id,
                    region_map_section_id: value.region_map_section_id,
                    cave: value.cave,
                    weather: value.weather,
                    map_type: value.map_type,
                    biking_allowed: value.allow_cycling,
                    allow_escaping: value.allow_escaping,
                    allow_running: value.allow_running,
                    show_map_name: value.show_map_name,
                    floor_num: 0,
                    battle_type: value.battle_type,
                }
            }
            _ => Err(GBAIOError::Unknown("Invalid ROM type"))?,
        };

        Ok(res)
    }

    /// Writes the [`MapHeader`] taking into account the game version.
    pub fn write(self, rom: &mut Rom, offset: usize) -> Result<(), GBAIOError> {
        match rom.rom_type {
            RomType::FireRed | RomType::LeafGreen => rom.write(offset, self)?,
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => {
                let value = EmeraldMapHeader {
                    map_layout: self.map_layout,
                    events: self.events,
                    map_scripts: self.map_scripts,
                    connections: self.connections,
                    music: self.music,
                    map_layout_id: self.map_layout_id,
                    region_map_section_id: self.region_map_section_id,
                    cave: self.cave,
                    weather: self.weather,
                    map_type: self.map_type,
                    allow_cycling: self.biking_allowed,
                    allow_escaping: self.allow_escaping,
                    allow_running: self.allow_running,
                    show_map_name: self.show_map_name,
                    battle_type: self.battle_type,
                    filler: [0; 2],
                };
                rom.write::<EmeraldMapHeader>(offset, value)?;
            }
            _ => Err(GBAIOError::Unknown("Invalid ROM type"))?,
        };

        Ok(())
    }
}

#[derive(Debug)]
pub enum MapError {
    MapTableNotInitialized,

    InvalidIndex(u8, u8),
    InvalidOffset(u8, u8, u32),

    IoError(GBAIOError),
}

/// Table of map headers. Provides methods for editing the table.
pub struct MapHeadersTable<'rom> {
    pub rom: &'rom mut Rom,
}

impl<'rom> MapHeadersTable<'rom> {
    /// Initialize the map headers table.
    pub fn init(rom: &'rom mut Rom) -> Result<Self, TableInitError> {
        if rom.refs.map_groups.is_none() || rom.refs.map_groups_list.is_none() {
            let (map_groups_table, map_groups) = get_map_groups_table(rom)?;
            rom.refs.map_groups = Some(map_groups_table);
            rom.refs.map_groups_list = Some(map_groups);
        }

        Ok(Self { rom })
    }

    /// Read a [`MapHeader`] from the given offset.
    pub fn read_header(&self, group: u8, index: u8) -> Result<MapHeader, MapError> {
        let group_table = self.get_group_table(group)?;

        if index as usize >= group_table.size {
            return Err(MapError::InvalidIndex(group, index));
        }

        let offset = group_table.offset + index as usize * 4;
        let header_offset = self
            .rom
            .read_ptr(offset)
            .map_err(|_| MapError::InvalidOffset(group, index, offset as u32))?;

        MapHeader::read(self.rom, header_offset).map_err(MapError::IoError)
    }

    /// Returns the [`TablePointer`] for the map groups table.
    pub fn get_table(&self) -> Result<&TablePointer, MapError> {
        if let Some(table) = &self.rom.refs.map_groups {
            Ok(table)
        } else {
            Err(MapError::MapTableNotInitialized)
        }
    }

    /// Returns the [`TablePointer`] for a specific map group.
    pub fn get_group_table(&self, group: u8) -> Result<&TablePointer, MapError> {
        let table = self.get_table()?;
        if group as usize >= table.size {
            return Err(MapError::InvalidIndex(group, 0));
        }

        Ok(&self.rom.refs.map_groups_list.as_ref().unwrap()[group as usize])
    }
}

impl Rom {
    /// Return the [`MapHeadersTable`] for this ROM.
    pub fn map_headers(&mut self) -> Result<MapHeadersTable, TableInitError> {
        MapHeadersTable::init(self)
    }
}

/// Reads the table pointer to the map groups table
/// and to each group of map headers.
fn get_map_groups_table(rom: &Rom) -> Result<(TablePointer, Vec<TablePointer>), TableInitError> {
    let base_offset: usize = match rom.rom_type {
        RomType::FireRed | RomType::LeafGreen => 0x5524C,
        RomType::Emerald | RomType::Ruby | RomType::Sapphire => 0x84AA4,
        _ => return Err(TableInitError::NotImplemented),
    };

    // Read the pointer at the base offset
    let table_offset = rom
        .read_ptr(base_offset)
        .map_err(|_| TableInitError::InvalidTablePointer)?;

    // Find all the map groups
    let mut map_groups = vec![];

    for i in 0..256 {
        let offset = table_offset + i * 4;

        if let Ok(ptr) = rom.read_ptr(offset) {
            map_groups.push(TablePointer {
                offset: ptr,
                size: 0,
                references: vec![offset],
            })
        } else {
            break;
        }
    }

    // Determine the size of each map group
    for gid in 0..map_groups.len() {
        let map_groups: &mut Vec<TablePointer> = &mut map_groups;
        // Get the pointer to the start of i-th map group
        let start_offset = map_groups[gid].offset;

        for mid in 0..256 {
            let curr_ptr_in_table = start_offset + mid * 4;

            // If the current offset is the same as the start offset
            // of any other map group *except* the current one,
            // then we've reached the end of the current map group.
            if map_groups
                .iter()
                .enumerate()
                .any(|(gid2, other_group)| gid != gid2 && other_group.offset == curr_ptr_in_table)
            {
                break;
            }

            // Keep going as long as you read a valid pointer
            if let Ok(_) = rom.read_ptr(curr_ptr_in_table) {
                map_groups[gid].size += 1;
            } else {
                break;
            }
        }
    }

    // Create the table pointer
    let table = TablePointer {
        offset: table_offset,
        size: map_groups.len(),
        references: rom.find_references(table_offset),
    };

    Ok((table, map_groups))
}
