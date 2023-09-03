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
    pub fn read(rom: &Rom, group: u8, index: u8) -> MapHeaderResult<MapData> {
        let header = rom.read_map_header(group, index)?;

        let events = match header.events.offset() {
            Some(events_offset) => Some(rom.data.read(events_offset)?),
            None => None,
        };

        Ok(Self {
            group,
            index,
            header,
            connections: None,
            scripts: None,
            events,
        })
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
    u8 map_group;
    u8 map_index;
});

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct MapScripts;
