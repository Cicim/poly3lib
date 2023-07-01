use std::fmt::Display;

use gba_macro::gba_struct;
use gba_types::{GBAIOError, GBAType};
use serde::Serialize;

use crate::{
    refs::{TableInitError, TablePointer},
    rom::{Rom, RomType},
};

gba_struct!(EmeraldMapHeader {
    void *map_layout;
    void *events;
    void *map_scripts;
    void *connections;
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
    void *events;
    void *map_scripts;
    void *connections;
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
            } // _ => Err(GBAIOError::Unknown("Invalid ROM type"))?,
        };

        //NOTE - We could check if the map layout offset and id match here

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
            } // _ => Err(GBAIOError::Unknown("Invalid ROM type"))?,
        };

        Ok(())
    }

    /// Clears the [`MapHeader`] and all its attached fields
    /// (everything excluding the map layout), according to the game version.
    pub fn clear(self, _rom: &mut Rom, _offset: usize) -> Result<(), GBAIOError> {
        // rom.clear(offset, MapHeader::size(rom))
        todo!("Implement clear for MapHeader")
    }

    /// Return the size of the [`MapHeader`] struct according to the ROM type.
    fn size(rom: &Rom) -> usize {
        match rom.rom_type {
            RomType::FireRed | RomType::LeafGreen => MapHeader::SIZE,
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => EmeraldMapHeader::SIZE,
            // _ => panic!("Unsupported ROM type"),
        }
    }
}

#[derive(Serialize)]
/// A [`MapHeader`] with its group, index and offset and layout's tilesets.
pub struct MapHeaderDump {
    pub group: u8,
    pub index: u8,
    pub offset: usize,
    pub header: MapHeader,
    pub tileset1: Option<usize>,
    pub tileset2: Option<usize>,
}

#[derive(Debug)]
pub enum MapError {
    MapTableNotInitialized,

    InvalidIndex(u8, u8),
    InvalidOffset(u8, u8, u32),

    InvalidResizeLength(usize),
    InvalidGroupToResize(u8),
    CannotRepointTable,
    CannotRepointHeader,

    IoError(GBAIOError),
    MissingHeader,
}

impl Display for MapError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use MapError::*;

        match self {
            MapTableNotInitialized => write!(f, "Map table not initialized"),

            InvalidIndex(group, index) => {
                write!(f, "{}.{} is not a valid map", group, index)
            }
            InvalidOffset(group, index, offset) => write!(
                f,
                "{}.{} has an invalid offset: ${:06X}",
                group, index, offset
            ),

            InvalidResizeLength(len) => {
                write!(f, "Invalid length to resize map table: {}", len)
            }
            InvalidGroupToResize(group) => {
                write!(f, "Invalid group to resize: {}", group)
            }
            CannotRepointTable => write!(f, "Cannot repoint map table"),
            CannotRepointHeader => write!(f, "Cannot repoint map header"),

            IoError(err) => write!(f, "IO error: {}", err),
            MissingHeader => write!(f, "Missing map header"),
        }
    }
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

    /// Returns a vector [`MapHeaderDump`] structs, which contain
    /// the group, index, offset and header itself.
    pub fn dump_headers(&self) -> Result<Vec<MapHeaderDump>, MapError> {
        // Get the map groups table
        let table = self.get_table()?;
        let mut res = vec![];

        // Iterate over the groups
        for group in 0u8..table.size as u8 {
            // Get the group
            let groups_table = match self.get_group_table(group) {
                Ok(table) => table,
                Err(_) => continue,
            };

            // Iterate over the headers
            for index in 0u8..groups_table.size as u8 {
                // Get the offset if it is valid
                let offset = match self.get_header_offset(group, index) {
                    Ok(offset) => offset as u32,
                    Err(_) => continue,
                } as usize;
                // Get the header if it is valid
                let header = match self.read_header(group, index) {
                    Ok(header) => header,
                    Err(_) => continue,
                };

                // Only if possible, read the tilesets' offsets
                let (tileset1, tileset2) = match header.map_layout.offset() {
                    Some(layout_offset) => {
                        let layout_offset: usize = layout_offset as usize;
                        // Reads directly from the bytes of the MapLayout struct for performance reasons
                        let tileset1 = self.rom.read_ptr(layout_offset + 16).ok();
                        let tileset2 = self.rom.read_ptr(layout_offset + 20).ok();
                        (tileset1, tileset2)
                    }
                    None => (None, None),
                };

                // Add the header to the result
                res.push(MapHeaderDump {
                    group,
                    index,
                    offset,
                    header,
                    tileset1,
                    tileset2,
                });
            }
        }

        Ok(res)
    }

    // ANCHOR Headers
    /// Read a [`MapHeader`] from the given offset.
    pub fn read_header(&self, group: u8, index: u8) -> Result<MapHeader, MapError> {
        let header_offset = self.get_header_offset(group, index)?;
        MapHeader::read(self.rom, header_offset).map_err(MapError::IoError)
    }

    /// Write a [`MapHeader`] to the given offset.
    pub fn write_header(
        &mut self,
        group: u8,
        index: u8,
        header: MapHeader,
    ) -> Result<(), MapError> {
        // Increase the sizes as needed
        self.resize_table(group as usize + 1)?;
        self.resize_group(group, index as usize)?;

        // Check what was there in the old place
        let offset = match self.get_header_offset(group, index) {
            // If there was already an header, overwrite it
            Ok(offset) => offset,
            // If there was an IOError related to the header, find a new place for it
            Err(MapError::IoError(_)) | Err(MapError::MissingHeader) => self
                .rom
                .find_free_space(MapHeader::size(self.rom), 4)
                .ok_or_else(|| MapError::CannotRepointHeader)?,
            Err(err) => return Err(err),
        };

        // Write everything
        header.write(self.rom, offset).map_err(MapError::IoError)?;
        self.write_offset_to_table(group, index, Some(offset))
    }

    /// Deletes the [`MapHeader`] at the given index along with all its attached fields.
    pub fn delete_header(&mut self, group: u8, index: u8) -> Result<(), MapError> {
        // If the header exists, clear it
        if let Ok(header_offset) = self.get_header_offset(group, index) {
            MapHeader::read(self.rom, header_offset)
                .map_err(MapError::IoError)?
                .clear(self.rom, header_offset)
                .map_err(MapError::IoError)?;
        }

        // In any case, clear the header's offset from the table
        self.write_offset_to_table(group, index, None)
    }

    // ANCHOR Header offset
    /// Returns the offset to the [`MapHeader`]
    fn get_header_offset(&self, group: u8, index: u8) -> Result<usize, MapError> {
        let group_table = self.get_group_table(group)?;

        if index as usize >= group_table.size {
            return Err(MapError::InvalidIndex(group, index));
        }

        let offset = group_table.offset + index as usize * 4;

        // If there is a NULL pointer, return a MissingHeader error
        if self.rom.read::<u32>(offset).map_err(MapError::IoError)? == 0 {
            return Err(MapError::MissingHeader);
        }
        let header_offset = self
            .rom
            .read_ptr(offset)
            .map_err(|_| MapError::InvalidOffset(group, index, offset as u32))?;
        Ok(header_offset)
    }

    /// Writes the given offset at the given location in the map groups table.
    fn write_offset_to_table(
        &mut self,
        group: u8,
        index: u8,
        offset: Option<usize>,
    ) -> Result<(), MapError> {
        let group_table = self.get_group_table(group)?;
        if index as usize >= group_table.size {
            return Err(MapError::InvalidIndex(group, index));
        }
        let write_location = group_table.offset + index as usize * 4;

        if let Some(offset) = offset {
            self.rom
                .write_ptr(write_location, offset)
                .map_err(MapError::IoError)?;
        } else {
            self.rom
                .write(write_location, 0u32)
                .map_err(MapError::IoError)?;
        }

        Ok(())
    }

    // ANCHOR Whole table
    /// Increases the size of the map groups table.
    pub fn resize_table(&mut self, new_num: usize) -> Result<(), MapError> {
        if new_num < 1 || new_num > 256 {
            return Err(MapError::InvalidResizeLength(new_num));
        }

        let table = self.get_table()?.clone();

        let old_offset = table.offset;
        let old_num = table.size;

        // Stop if the table is already big enough
        if old_num >= new_num {
            return Ok(());
        }

        // Copy the whole table
        let copy = self.rom.data[old_offset..old_offset + old_num * 4].to_vec();
        // Overwrite the current table
        self.rom
            .clear(old_offset, old_num * 4)
            .map_err(MapError::IoError)?;

        // Find an offset for the new table size
        let new_offset = self
            .rom
            .find_free_space(new_num * 4, 4)
            .ok_or_else(|| MapError::CannotRepointTable)?;
        // Fill the new table with zeros
        self.rom.data[new_offset..new_offset + new_num * 4].fill(0);
        // Copy the first part of the table to the new spot
        self.rom.data[new_offset..new_offset + old_num * 4].copy_from_slice(&copy);

        // Update the table pointer
        let updated_table = table
            .update(&mut self.rom, new_offset, new_num)
            .map_err(|_| MapError::CannotRepointTable)?;
        // Update the map groups table pointer
        self.rom.refs.map_groups = Some(updated_table);

        // Update all the references to all the groups and all the oh my gosh is this a lot of work
        let mut groups = self.rom.refs.map_groups_list.clone().unwrap();
        for i in 0..old_num {
            let new_reference = TablePointer {
                references: vec![new_offset + i * 4],
                ..groups[i]
            };
            groups[i] = new_reference;
        }
        for i in old_num..new_num {
            let new_reference = TablePointer {
                references: vec![new_offset + i * 4],
                offset: 0,
                size: 0,
            };
            groups.push(new_reference);
        }
        self.rom.refs.map_groups_list = Some(groups);

        Ok(())
    }

    /// Increase the size of a single map group.
    ///
    /// You also have to call `increase_groups_number` if you
    /// want to add a new group.
    pub fn resize_group(&mut self, group: u8, new_size: usize) -> Result<(), MapError> {
        // Make sure the new size is valid
        if new_size < 1 || new_size > 256 {
            return Err(MapError::InvalidResizeLength(new_size));
        }
        // If the group has not been initialized yet, fail
        if group > self.get_table()?.size as u8 {
            return Err(MapError::InvalidGroupToResize(group));
        }

        // Get the group you want to modify
        let group_table = self.get_group_table(group)?.clone();

        // Exit if the group is already big enough
        if group_table.size >= new_size {
            return Ok(());
        }

        // Copy the whole group
        let copy =
            self.rom.data[group_table.offset..group_table.offset + group_table.size * 4].to_vec();

        // Overwrite the current group
        self.rom
            .clear(group_table.offset, group_table.size * 4)
            .map_err(MapError::IoError)?;

        // Find an offset for the new group size
        let new_offset = self
            .rom
            .find_free_space(new_size * 4, 4)
            .ok_or_else(|| MapError::CannotRepointTable)?;

        // Fill the new group with zeros
        self.rom.data[new_offset..new_offset + new_size * 4].fill(0);
        // Copy the first part of the group to the new spot
        self.rom.data[new_offset..new_offset + group_table.size * 4].copy_from_slice(&copy);

        // Update the table pointer
        let updated_table = group_table
            .update(&mut self.rom, new_offset, new_size)
            .map_err(|_| MapError::CannotRepointTable)?;
        // Update the map groups table pointer
        self.rom.refs.map_groups_list.as_mut().unwrap()[group as usize] = updated_table;

        Ok(())
    }

    /// Returns the [`TablePointer`] for the map groups table.
    fn get_table(&self) -> Result<&TablePointer, MapError> {
        if let Some(table) = &self.rom.refs.map_groups {
            Ok(table)
        } else {
            Err(MapError::MapTableNotInitialized)
        }
    }

    /// Returns the [`TablePointer`] for a specific map group.
    fn get_group_table(&self, group: u8) -> Result<&TablePointer, MapError> {
        let table = self.get_table()?;
        if group as usize >= table.size {
            return Err(MapError::InvalidIndex(group, 0));
        }

        Ok(&self.rom.refs.map_groups_list.as_ref().unwrap()[group as usize])
    }
}

impl Rom {
    /// Return the [`MapHeadersTable`] for this ROM.
    pub fn map_headers(&mut self) -> MapHeadersTable {
        MapHeadersTable::init(self).unwrap_or_else(|_| {
            panic!("You have to initialize the map headers table before calling rom.map_headers()!")
        })
    }
}

/// Reads the table pointer to the map groups table
/// and to each group of map headers.
fn get_map_groups_table(rom: &Rom) -> Result<(TablePointer, Vec<TablePointer>), TableInitError> {
    let base_offset: usize = match rom.rom_type {
        RomType::FireRed | RomType::LeafGreen => 0x5524C,
        RomType::Emerald | RomType::Ruby | RomType::Sapphire => 0x84AA4,
        // _ => return Err(TableInitError::NotImplemented),
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
