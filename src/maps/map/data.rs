use rom_data::rom_struct;
use serde::{Deserialize, Serialize};

use crate::Rom;

use super::{event::MapEvents, MapHeader, MapHeaderResult, MapHeaderTable};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct MapData {
    /// The group of the map.
    pub group: u8,
    /// The index of the map in the table.
    pub index: u8,

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
        let header = rom.read_map_header(group, index)?;

        let events = match header.events.offset() {
            Some(events_offset) => Some(rom.data.read(events_offset)?),
            None => None,
        };

        let connections = match header.connections.offset() {
            Some(connections_offset) => Some(rom.data.read(connections_offset)?),
            None => None,
        };

        Ok(Self {
            group,
            index,
            header,
            connections,
            scripts: None,
            events,
        })
    }

    /// Writes the map data to ROM. Reads the previous one if present.
    pub fn write(self, rom: &Rom) -> MapHeaderResult {
        println!("{}", rom);
        Ok(())
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct MapScripts;
