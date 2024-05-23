use serde::{Deserialize, Serialize};

use rom_data::{
    rom_struct,
    types::{RomPointer, RomReadableType, RomSizedType, RomWritableType},
    Offset, RomBase, RomData, RomIoError,
};

use crate::{
    maps::layout::{MapLayout, MapLayoutData, MapLayoutError},
    Rom,
};

rom_struct!(RSEMapHeader {
    void *layout;
    void *events;
    void *scripts;
    void *connections;
    u16 music;
    u16 layout_id;
    u8 mapsec_id;
    u8 cave;
    u8 weather;
    u8 map_type;
    u16 filler;
    bool allow_biking:1;
    bool allow_escaping:1;
    bool allow_running:1;
    bool show_map_name:5;
    u8 battle_type;
} priv);

rom_struct!(FrLgMapHeader {
    void *layout;
    void *events;
    void *scripts;
    void *connections;
    u16 music;
    u16 layout_id;
    u8 mapsec_id;
    u8 cave;
    u8 weather;
    u8 map_type;
    bool allow_biking;
    bool allow_escaping:1;
    bool allow_running:1;
    bool show_map_name:6;
    i8 floor_num;
    u8 battle_type;
} priv);

/// Header for a map.
///
/// This struct was not directly constructed using [`rom_struct!`].
#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct MapHeader {
    pub layout: RomPointer,
    pub events: RomPointer,
    pub scripts: RomPointer,
    pub connections: RomPointer,

    pub music: u16,
    pub layout_id: u16,
    pub mapsec_id: u8,
    pub cave: u8,
    pub weather: u8,
    pub map_type: u8,
    pub allow_biking: bool,
    pub allow_escaping: bool,
    pub allow_running: bool,
    pub show_map_name: bool,
    pub floor_num: i8,
    pub battle_type: u8,
}

impl MapHeader {
    /// Checks if the [`MapHeader`] is valid.
    pub fn is_valid(&self, rom: &Rom) -> bool {
        // For a MapHeader to be real it has to pass the following checks:
        if !(
            //  1. All pointers must be valid
            self.connections.is_valid()
            && self.events.is_valid()
            && self.scripts.is_valid()
            //  2. There must be a valid layout id
            && self.layout_id > 0
            //  3. The music should be a reasonable value
            && (self.music & 0x7FFF == 0x7FFF || self.music < 2000)
            // ...
        ) {
            return false;
        }

        //  4. There must be a valid layout offset, which means
        //  4.1  Either it is NULL and the layout_id is 0
        if self.layout.is_null() {
            return self.layout_id == 0;
        }
        // 4.2 The offset is valid, and aligned to 4
        let layout_offset = match self.layout.offset() {
            Some(offset) => {
                if offset % 4 != 0 {
                    return false;
                }
                offset
            }
            None => return false,
        };

        //  5. The used map layout must be valid
        match rom.data.read::<MapLayout>(layout_offset) {
            Ok(layout) => layout.is_valid(),
            Err(_) => false,
        }
    }

    /// Reads the map header at the given offset into a [`MapHeaderDump`] struct.
    pub(crate) fn read_to_dump(
        offset: Offset,
        rom: &RomData,
        group: u8,
        index: u8,
    ) -> Option<MapHeaderDump> {
        // Read the header
        match rom.read::<MapHeader>(offset) {
            Ok(header) => {
                let (tileset1, tileset2) = header.read_tilesets_offsets(rom);

                Some(MapHeaderDump {
                    group,
                    index,
                    offset,
                    header,
                    tileset1,
                    tileset2,
                })
            }
            // Skip invalid headers
            Err(_) => None,
        }
    }

    /// Reads the offsets of the tilesets used by this map's layout.
    ///
    /// Faster, since it does not read the entire layout struct.
    fn read_tilesets_offsets(&self, rom: &RomData) -> (Option<Offset>, Option<Offset>) {
        // Read the tilesets from the layout (only if present)
        if let Some(layout_offset) = self.layout.offset() {
            (
                rom.read_offset(layout_offset + 16).ok(),
                rom.read_offset(layout_offset + 20).ok(),
            )
        } else {
            (None, None)
        }
    }

    /// Read the map layout data for this map.
    pub fn read_layout_data(&self, rom: &Rom) -> Result<MapLayoutData, MapLayoutError> {
        rom.read_map_layout(self.layout_id)
    }
}

#[derive(Debug, Serialize)]
/// A [`MapHeader`] with its group, index and offset and layout's tilesets.
pub struct MapHeaderDump {
    pub group: u8,
    pub index: u8,
    pub offset: Offset,
    pub header: MapHeader,
    pub tileset1: Option<Offset>,
    pub tileset2: Option<Offset>,
}

// ANCHOR RomType implementations
impl RomSizedType for MapHeader {
    fn get_size(rom: &RomData) -> usize {
        match rom.base {
            RomBase::Ruby | RomBase::Sapphire | RomBase::Emerald => RSEMapHeader::get_size(rom),
            RomBase::FireRed | RomBase::LeafGreen => FrLgMapHeader::get_size(rom),
        }
    }
    fn get_alignment(_: &RomData) -> usize {
        4
    }
}

/// Allows copying all fields in any direction between
/// [FrLgMapHeader], [RSEMapHeader] and [MapHeader].
macro_rules! construct {
    ($name:ident, $from:ident, $($y:ident: $x: expr),*) => {
        $name {
            layout: $from.layout,
            events: $from.events,
            scripts: $from.scripts,
            connections: $from.connections,
            music: $from.music,
            layout_id: $from.layout_id,
            mapsec_id: $from.mapsec_id,
            cave: $from.cave,
            weather: $from.weather,
            map_type: $from.map_type,
            allow_biking: $from.allow_biking,
            allow_escaping: $from.allow_escaping,
            allow_running: $from.allow_running,
            show_map_name: $from.show_map_name,
            battle_type: $from.battle_type,

            $($y: $x),*
        }
    };
}

impl RomReadableType for MapHeader {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        Ok(match rom.base {
            RomBase::Ruby | RomBase::Sapphire | RomBase::Emerald => {
                let read: RSEMapHeader = rom.read::<RSEMapHeader>(offset)?;
                construct!(MapHeader, read, floor_num: 0)
            }
            RomBase::FireRed | RomBase::LeafGreen => {
                let read: FrLgMapHeader = rom.read::<FrLgMapHeader>(offset)?;
                construct!(MapHeader, read, floor_num: read.floor_num)
            }
        })
    }
}

impl RomWritableType for MapHeader {
    fn write_to(self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        match rom.base {
            RomBase::Ruby | RomBase::Sapphire | RomBase::Emerald => {
                let header = construct!(RSEMapHeader, self, filler: 0);
                rom.write(offset, header)
            }
            RomBase::FireRed | RomBase::LeafGreen => {
                let header = construct!(FrLgMapHeader, self, floor_num: self.floor_num);
                rom.write(offset, header)
            }
        }
    }
}
