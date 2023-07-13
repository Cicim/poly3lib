use std::collections::{HashMap, HashSet};

use crate::{
    maps::layout::MapLayout,
    maps::tileset::TilesetHeader,
    refs::{TableInitError, TablePointer},
    rom::Rom,
};

pub mod connection;
pub mod events;
pub mod header;
pub mod layout;
pub mod mapsec;
pub mod render;
pub mod tileset;
pub mod wilds;

impl Rom {
    /// Initializes the map groups table and the map groups list.
    pub fn init_map(&mut self) -> Result<(), TableInitError> {
        header::MapHeadersTable::init(self)?;
        layout::MapLayoutsTable::init(self)?;
        mapsec::MapSectionTable::init(self)?;
        wilds::WildsTable::init(self)?;

        if self.refs.tilesets_table.is_none() {
            // TODO Remove this unwrap
            let map_layouts_table = self.refs.map_layouts_table.as_ref().unwrap();
            let tilesets_data = get_tilesets_data(self, &map_layouts_table)?;
            self.refs.tilesets_table = Some(tilesets_data);
        }

        Ok(())
    }
}

/// Reads the tilesets data from the map layouts table.
fn get_tilesets_data(
    rom: &Rom,
    layouts_table: &TablePointer,
) -> Result<HashMap<usize, (usize, bool)>, TableInitError> {
    let mut tileset: HashSet<u32> = HashSet::new();

    // Read all the layouts to extrapolate all tileset offsets
    for i in 0..layouts_table.size {
        let offset = layouts_table.offset + i * 4;
        match rom.read_ptr(offset) {
            Ok(ptr) => {
                let layout: MapLayout = rom.read(ptr).unwrap();
                if let Some(offset) = layout.primary_tileset.offset() {
                    tileset.insert(offset as u32);
                }
                if let Some(offset) = layout.secondary_tileset.offset() {
                    tileset.insert(offset as u32);
                }
            }
            Err(_) => continue,
        }
    }

    let mut tilesets_data: HashMap<usize, (usize, bool)> = HashMap::new();

    // For each tileset you found
    for tileset_offset in tileset {
        // Read the tileset data
        let tileset_header = TilesetHeader::read(rom, tileset_offset as usize)
            .map_err(|_| TableInitError::TableGoesOutOfBounds)?;

        tilesets_data.insert(
            tileset_offset as usize,
            (
                // Size of the tileset in blocks
                tileset_header.get_size(rom),
                // If the tileset is a secondary tileset
                tileset_header.is_secondary != 0,
            ),
        );
    }

    Ok(tilesets_data)
}
