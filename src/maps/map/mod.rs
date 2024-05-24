use serde::{Deserialize, Serialize};
use thiserror::Error;

use rom_data::{
    types::{RomPointer, RomSizedType},
    Offset, RomIoError,
};

use crate::{Rom, RomTable};

mod data;
mod events;
mod header;
pub mod loader;
mod scripts;

// ANCHOR Re-exported types
pub use data::{MapConnections, MapData};
pub use events::{
    BgEvent, BgEventData, CoordEvent, MapEvents, ObjectEvent, ObjectEventData, WarpEvent,
};
pub use header::{MapHeader, MapHeaderAnnotated};
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
    pub fn create_map(&mut self, group: u8, index: u8, layout_id: u16) -> MapHeaderResult {
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

        // Read the layout offset
        let layout_offset = self
            .get_map_layout_offset(layout_id)
            .map_err(|_| MapError::InvalidLayout(layout_id))?;

        // Create a new header
        let header = MapHeader {
            layout: RomPointer::new(layout_offset),
            events: RomPointer::Null,
            scripts: RomPointer::Null,
            connections: RomPointer::Null,
            music: 0xFFFF,
            layout_id,
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

    // ANCHOR Data mining.
    /// Iterates over all the map headers in all groups, returning a [`MapHeaderAnnotated`]
    /// struct which contains group, index and offset together with the map header data.
    ///
    ///
    /// ```no_run
    /// let musics = rom.iter_map_headers().unwrap()
    ///     .map(|MapHeaderAnnotated {group, index, header, ..}| (group, index, header.music))
    ///     .collect::<Vec<_>>();
    /// ```
    pub fn iter_map_headers<'a>(
        &'a self,
    ) -> MapHeaderResult<impl Iterator<Item = MapHeaderAnnotated> + 'a> {
        let groups_table = get_groups(self)?;
        let groups_count = groups_table.groups_count();

        // Loop through each group
        let iter = (0..groups_count).flat_map(move |group| {
            let maps_count = groups_table.maps_in_group(group).unwrap_or(0);

            // Loop through each index
            (0..maps_count)
                .map(move |index| {
                    // Get the header offset
                    groups_table
                        // Get the MapHeader**
                        .get_header_pointer(group, index)
                        .ok()
                        // Then get the MapHeader*
                        .and_then(|pointer| self.data.read_offset(pointer).ok())
                        // The read the map header and return it together with the offset, group and index.
                        .and_then(|offset| {
                            self.data.read::<MapHeader>(offset).ok().map(|header| {
                                MapHeaderAnnotated {
                                    group,
                                    index,
                                    offset,
                                    header,
                                }
                            })
                        })
                })
                .filter_map(|x| x)
        });

        Ok(iter)
    }

    /// Returns a single map header with its group, index and offset.
    pub fn read_annotated_map_header(
        &self,
        group: u8,
        index: u8,
    ) -> MapHeaderResult<MapHeaderAnnotated> {
        // Get the pointer
        let table = get_groups(self)?;
        let pointer = table.get_header_pointer(group, index)?;

        // Read the offset at the pointer
        let offset = self.data.read_offset(pointer)?;

        // Try to read the header
        let header = self.data.read::<MapHeader>(offset)?;

        Ok(MapHeaderAnnotated {
            group,
            index,
            offset,
            header,
        })
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

    let table = loader::find_map_groups_in_rom(rom)?;
    rom.refs.map_groups = Some(table);

    Ok(())
}
