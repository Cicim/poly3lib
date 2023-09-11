use serde::{Deserialize, Serialize};

use rom_data::{rom_struct, types::RomSizedType, Offset, RomData, RomIoError};

use crate::{
    maps::layout::{MapLayoutData, MapLayoutError},
    Rom,
};

use super::{events::MapEvents, get_groups, MapHeader, MapHeaderResult, MapScripts};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct MapData {
    /// The group of the map.
    pub group: u8,
    /// The index of the map in the table.
    pub index: u8,
    /// The offset of the map header.
    pub offset: Offset,

    /// The header of the map.
    pub header: MapHeader,

    /// This map's connections (if present)
    pub connections: Option<MapConnections>,
    /// This map's scripts (if present)
    pub scripts: Option<MapScripts>,
    /// This map's events (if present)
    pub events: Option<MapEvents>,
}

impl MapData {
    /// Reads the map data from the ROM.
    pub fn read(rom: &Rom, group: u8, index: u8) -> MapHeaderResult<MapData> {
        let pointer = get_groups(rom)?.get_header_pointer(group, index)?;
        let offset = rom.data.read_offset(pointer)?;
        let header = rom.read_map_header(group, index)?;

        let events = match header.events.offset() {
            Some(events_offset) => Some(rom.data.read(events_offset)?),
            None => None,
        };

        let connections = match header.connections.offset() {
            Some(connections_offset) => Some(rom.data.read(connections_offset)?),
            None => None,
        };

        let scripts = match header.scripts.offset() {
            Some(scripts_offset) => Some(rom.data.read(scripts_offset)?),
            None => None,
        };

        Ok(Self {
            group,
            index,
            offset,
            header,
            connections,
            scripts,
            events,
        })
    }

    /// Writes the map data to ROM. Reads the previous one if present.
    pub fn write(self, rom: &Rom) -> MapHeaderResult {
        println!("{}", rom);
        Ok(())
    }

    /// Returns all the script resources directly referenced by this map.
    pub fn get_scripts(&self) -> Vec<Offset> {
        let mut scripts_offsets = Vec::new();

        // Offsets in Events
        if let Some(events) = &self.events {
            scripts_offsets.extend(events.get_scripts());
        }
        // Offsets in MapScripts (excluding any table offset)
        if let Some(scripts) = &self.scripts {
            scripts_offsets.extend(scripts.get_scripts());
        }

        scripts_offsets
    }

    /// Clears all the structs referenced by this map.
    pub fn clear(self, rom: &mut Rom) -> Result<(), RomIoError> {
        // Clear the header
        let header_size = MapHeader::get_size(&rom.data);
        rom.data.clear_bytes(self.offset, header_size)?;

        // Clear the connections
        if let Some(connections) = self.connections {
            // SAFETY: if the connections have been loaded, then there was an offset
            let conn_offset = self.header.connections.offset().unwrap();
            connections.clear(&mut rom.data, conn_offset)?;
        }
        // Clear the events
        if let Some(events) = self.events {
            // SAFETY: if the events have been loaded, then there was an offset
            let events_offset = self.header.events.offset().unwrap();
            events.clear(&mut rom.data, events_offset)?;
        }
        // Clear the scripts
        if let Some(scripts) = self.scripts {
            scripts.clear(&mut rom.data)?;
        }

        Ok(())
    }

    /// Returns the layout of the map data in the ROM.
    pub fn read_layout_data(&self, rom: &Rom) -> Result<MapLayoutData, MapLayoutError> {
        self.header.read_layout_data(rom)
    }
}

// ANCHOR Connections
rom_struct!(MapConnections {
    i32 count;
    struct Connection connections{$count};
});
rom_struct!(Connection {
    u8 direction;
    u32 offset;
    u8 group;
    u8 index;
});

impl MapConnections {
    pub fn clear(mut self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        self.connections.to_clear();
        // Write the struct while clearing the inner vector
        rom.write(offset, self)?;
        // Clear the struct
        rom.clear_bytes(offset, Self::get_size(rom))
    }
}
