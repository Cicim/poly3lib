use std::collections::HashSet;

use serde::{Deserialize, Serialize};
use thiserror::Error;

use rom_data::{
    types::{RomPointer, RomSizedType},
    Offset, RomBase, RomIoError,
};

use crate::{Rom, RomTable};

mod data;
mod events;
mod header;
mod scripts;

// ANCHOR Re-exported types
pub use data::{MapConnections, MapData};
pub use events::{
    BgEvent, BgEventData, CoordEvent, MapEvents, ObjectEvent, ObjectEventData, WarpEvent,
};
pub use header::{MapHeader, MapHeaderDump};
pub use scripts::{MapScripts, MapScriptsSubTable, ScriptWithVars};

/// Helper type for the result of map header operations.
type MapHeaderResult<T = ()> = Result<T, MapError>;

#[derive(Debug, Error)]
pub enum MapError {
    #[error("Map table not initialized")]
    MapTableNotInitialized,

    #[error("Invalid map index {0}.{1}")]
    InvalidIndex(u8, u8),
    #[error("Invalid offset {2:08x} for map {0}.{1}")]
    InvalidOffset(u8, u8, u32),
    #[error("Map in position {0}.{1} is missing the header")]
    MissingHeader(u8, u8),
    #[error("Invalid map layout id {0}")]
    InvalidLayout(u16),

    #[error("Invalid group selected for resizing: {0}")]
    InvalidGroupToResize(u8),
    #[error("Cannot repoint map table")]
    CannotRepointTable,
    #[error("Cannot repoint map header")]
    CannotRepointHeader,

    #[error(transparent)]
    IoError(#[from] RomIoError),
}

// ANCHOR Methods
impl Rom {
    // ANCHOR Reading
    /// Reads a map header given the group and the index.
    pub fn read_map_header(&self, group: u8, index: u8) -> MapHeaderResult<MapHeader> {
        // Get the pointer to the header offset
        let pointer = get_groups(self)?.get_header_pointer(group, index)?;

        // Return a different result if it is NULL
        if self.data.read_word(pointer)? == 0 {
            return Err(MapError::MissingHeader(group, index));
        }

        let offset = self.data.read_offset(pointer)?;
        let header: MapHeader = self.data.read(offset)?;
        Ok(header)
    }

    /// Reads a [MapData] object from the given group and index
    pub fn read_map(&self, group: u8, index: u8) -> MapHeaderResult<MapData> {
        MapData::read(self, group, index)
    }

    // ANCHOR Writing
    /// Writes a map header to the given group and index,
    /// overwriting the previous one if present.
    ///
    /// Allocates a new space if the spot was empty but
    /// does not extend the table size if out of bounds.
    pub fn write_map_header(&mut self, group: u8, index: u8, header: MapHeader) -> MapHeaderResult {
        // Get the pointer to the header offset
        let pointer = get_groups(self)?.get_header_pointer(group, index)?;

        // Based on whether the header offset is invalid, allocate a new space or don't.
        let header_offset = match self.data.read::<RomPointer>(pointer)?.offset() {
            Some(x) => x,
            None => {
                // Allocate new space
                let header_size = MapHeader::get_size(&self.data);
                self.data.find_free_space(header_size, 4)?
            }
        };

        Ok(self.data.write(header_offset, header)?)
    }

    // ANCHOR Deleting
    /// Deletes a map and the struct it references. Returns a list of all
    /// script offsets referenced by this map to clear them from the ROM.
    pub fn delete_map(&mut self, group: u8, index: u8) -> MapHeaderResult<Vec<Offset>> {
        // Read the position where to read the header from.
        let pointer = get_groups(self)?.get_header_pointer(group, index)?;

        // Read the map data to clear.
        match self.read_map(group, index) {
            Ok(data) => {
                // Read the scripts to clear
                let scripts = data.get_scripts();
                // Clear all the data
                data.clear(self)?;
                // Clear the pointer
                self.data.write_word(pointer, 0)?;

                Ok(scripts)
            }

            // If the map data is not present, return an empty list.
            Err(_) => {
                // Clear the pointer
                self.data.write_word(pointer, 0)?;

                Ok(Vec::new())
            }
        }
    }

    // ANCHOR Creating
    /// Creates a new map at the given group/index if possible
    pub fn create_map(&mut self, group: u8, index: u8) -> MapHeaderResult {
        if group == 255 || index == 255 {
            return Err(MapError::InvalidIndex(255, 255));
        };

        // Increase the number of groups if needed
        resize_table_if_needed(self, group + 1)?;
        // Increase the number of indices in a group if needed
        resize_group_if_needed(self, group, index + 1)?;

        let pointer = get_groups(self)?.get_header_pointer(group, index)?;
        if self.data.read_word(pointer)? != 0 {
            return Err(MapError::InvalidIndex(group, index));
        }

        // Create a new header
        let header = MapHeader {
            layout: RomPointer::Null,
            events: RomPointer::Null,
            scripts: RomPointer::Null,
            connections: RomPointer::Null,
            music: 0xFFFF,
            layout_id: 0,
            mapsec_id: 0,
            cave: 0,
            weather: 0,
            map_type: 0,
            allow_biking: false,
            allow_escaping: false,
            allow_running: false,
            show_map_name: false,
            floor_num: 0,
            battle_type: 0,
        };

        // Allocate new space for the header
        let header_size = MapHeader::get_size(&self.data);
        let header_offset = self.data.find_free_space(header_size, 4)?;
        // Write the header
        self.data.write(header_offset, header)?;

        // Update the pointer
        self.data.write_offset(pointer, header_offset)?;

        Ok(())
    }

    // ANCHOR Dumping
    /// Dumps all the map headers in all groups, along with some extra information.
    pub fn dump_map_headers(&self) -> MapHeaderResult<Vec<MapHeaderDump>> {
        let rom = &self.data;
        let groups_table = get_groups(self)?;

        // Loop through each group and each index
        let mut dump = Vec::new();
        for (group, group_table) in groups_table.groups.iter().enumerate() {
            let group = group as u8;

            for index in 0..group_table.length as u8 {
                // Get the header offset
                let pointer = groups_table.get_header_pointer(group, index)?;

                match rom.read::<RomPointer>(pointer)?.offset() {
                    // Skip invalid pointers
                    None => continue,
                    // Read the header from valid pointers
                    Some(offset) => match MapHeader::read_to_dump(offset, rom, group, index) {
                        Some(header_dump) => dump.push(header_dump),
                        None => continue,
                    },
                }
            }
        }

        Ok(dump)
    }
}

/// Returns the map groups table if present.
fn get_groups(rom: &Rom) -> MapHeaderResult<&MapGroups> {
    rom.refs
        .map_groups
        .as_ref()
        .ok_or(MapError::MapTableNotInitialized)
}

/// Returns the map groups table (mutable)
fn get_groups_mut(rom: &mut Rom) -> MapHeaderResult<&mut MapGroups> {
    rom.refs
        .map_groups
        .as_mut()
        .ok_or(MapError::MapTableNotInitialized)
}

/// Allocate new space in a group if needed
fn resize_group_if_needed(rom: &mut Rom, group: u8, new_length: u8) -> MapHeaderResult {
    // Check if it is needed
    let groups_table = get_groups(rom)?;

    let group = group as usize;
    let new_length = new_length as usize;
    if group >= groups_table.table.length {
        return Err(MapError::InvalidGroupToResize(group as u8));
    }
    if new_length < groups_table.groups[group as usize].length {
        return Ok(());
    }

    // Should correctly update the reference as well.
    let new_table =
        groups_table.groups[group]
            .clone()
            .simple_resize(&mut rom.data, new_length, 4)?;

    get_groups_mut(rom)?.groups[group] = new_table;

    Ok(())
}

/// Allocates new space for new groups if needed
fn resize_table_if_needed(rom: &mut Rom, new_length: u8) -> MapHeaderResult {
    let groups_table = get_groups(rom)?;
    let old_length = groups_table.table.length;

    let new_length = new_length as usize;
    if new_length < old_length {
        return Ok(());
    }

    // Resize the table
    let new_table = groups_table
        .table
        .clone()
        .simple_resize(&mut rom.data, new_length, 4)?;

    let groups_table = get_groups_mut(rom)?;
    groups_table.table = new_table;

    // Add any new element to the table
    for _ in old_length..new_length {
        // Create a new table for the new group
        groups_table.groups.push(RomTable {
            offset: 0,
            length: 0,
            references: vec![],
        });
    }

    // Fix all the old tables with the new reference
    for (index, group) in groups_table.groups.iter_mut().enumerate() {
        let new_group_pointer = groups_table.table.offset + index * 4;

        if !group.references.is_empty() {
            group.references.remove(0);
        }
        group.references.insert(0, new_group_pointer);
    }

    Ok(())
}

// ANCHOR Map Groups
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct MapGroups {
    /// The table of all groups
    pub table: RomTable,
    /// A table for each group
    pub groups: Vec<RomTable>,
}

impl MapGroups {
    /// Returns the number of groups.
    pub fn groups_count(&self) -> u8 {
        return self.table.length as u8;
    }

    /// Returns the number of maps in the given group
    /// if the group is valid.
    pub fn maps_in_group(&self, group: u8) -> Option<u8> {
        self.groups
            .get(group as usize)
            .map(|group| group.length as u8)
    }

    /// Returns the pointer to the map header given the group and index.
    pub fn get_header_pointer(&self, group: u8, index: u8) -> MapHeaderResult<Offset> {
        // Make sure the group is in bounds
        if group > self.groups_count() {
            return Err(MapError::InvalidIndex(group, index));
        }
        // Make sure the index is in bounds
        if index > self.maps_in_group(group).unwrap_or(255) {
            return Err(MapError::InvalidIndex(group, index));
        }

        // Get the group
        let group = &self.groups[group as usize];
        // Get the header pointer
        Ok(group.offset + index as usize * 4)
    }
}

/// Initializes the table of map layouts in the ROM if it is not already initialized.
pub fn init_table(rom: &mut Rom) -> Result<(), RomIoError> {
    // If already initialized, return
    if rom.refs.map_groups.is_some() {
        return Ok(());
    }

    let table = read_table(rom)?;
    rom.refs.map_groups = Some(table);

    Ok(())
}

/// Reads the map groups table from ROM.
pub(crate) fn read_table(rom: &mut Rom) -> Result<MapGroups, RomIoError> {
    let table_offset = match rom.base() {
        RomBase::Emerald => 0x486578,
        RomBase::FireRed => 0x3526a8,
        RomBase::LeafGreen => 0x352688,
        RomBase::Ruby => 0x308588,
        RomBase::Sapphire => 0x308518,
    };

    // Read all the map groups
    let mut groups = vec![];

    for i in 0..256 {
        let offset = table_offset + i * 4;

        // This is the structure at this point
        //  offset            -> | group_i_offset | ...
        //  group_i_offset    -> | header_i_0_offset | header_i_1_offset | ...
        // We can already read group_i_offset, and we want it to be valid
        if let Ok(group_offset) = rom.data.read_offset(offset) {
            if !is_header_pointer_valid(rom, group_offset) {
                break;
            }
            groups.push(RomTable {
                offset: group_offset,
                length: 0,
                references: vec![offset],
            });
        } else {
            // If this offset is already invalid, we can quit
            break;
        }
    }

    compute_groups_sizes(rom, &mut groups);

    Ok(MapGroups {
        table: RomTable {
            offset: table_offset,
            length: groups.len(),
            references: rom.data.find_references(table_offset, 4),
        },
        groups,
    })
}

/// Computes the size of each group by counting the number of
/// valid pointers to headers in it.
///
/// Then, shrinks each group so that only the first group contains a map.
fn compute_groups_sizes(rom: &Rom, groups: &mut [RomTable]) {
    // Create a set with all the groups offsets to understand
    // when a group "bleeds" into another.
    let group_offsets: HashSet<Offset> =
        HashSet::from_iter(groups.iter().map(|group| group.offset));

    for group in groups {
        // Start reading the maps
        for index in 0..256 {
            let curr_offset = group.offset + index * 4;

            // We assume the first offset will always belong to this group,
            // but for any other offset, if it appears in the group_offsets
            // then it must belong to another group.
            if index > 0 && group_offsets.contains(&curr_offset) {
                break;
            }

            // Increase the size while it's valid
            if is_header_pointer_valid(rom, curr_offset) {
                group.length += 1;
            } else {
                break;
            }
        }
    }
}

/// Returns true if an header pointer in a group is valid, after
/// receiving as input the header itself.
fn is_header_pointer_valid(rom: &Rom, pointer_in_group: Offset) -> bool {
    // GROUP TABLE
    // | header_a.b | header_a.c | header a.d
    //              ^ pointer_in_group
    // Read the header_offset in the group
    let header_offset = match rom.data.read::<RomPointer>(pointer_in_group) {
        Ok(x) => x,
        Err(_) => return false,
    };

    // If it is invalid, the table ends here
    if !header_offset.is_valid() {
        return false;
    }

    // If it is not nul
    if let Some(header_offset) = header_offset.offset() {
        // If the map header could be read
        if let Ok(header) = rom.data.read::<MapHeader>(header_offset) {
            header.is_valid(&rom)
        } else {
            // If it could not be read, the groups end here
            false
        }
    } else {
        true
    }
}
