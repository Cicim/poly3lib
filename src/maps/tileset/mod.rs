use std::collections::{HashMap, HashSet};

use rom_data::{rom_struct, Offset, RomIoError};
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::Rom;

use super::{layout::MapLayoutError, ProblemsLog};

mod anim;
mod data;
mod pair;
mod types;

pub use data::TilesetData;
pub use pair::{TilesetNumbers, TilesetPair, TilesetPairRenderingData};
pub use types::{MetaTile, MetatileAttributes, MetatileLayerType};

rom_struct!(TilesetHeader {
    bool is_compressed;
    bool is_secondary;
    void* graphics;
    void* palettes;
    void* metatiles;
    #[for(base(Ruby, Sapphire, Emerald), swap(attributes))]
    void* animations;
    void* attributes;
});

impl TilesetHeader {
    /// Reads the number of metatiles in this tileset or returns the default one.
    pub fn get_metatiles_count(&self) -> u16 {
        self.get_metatiles_count_inner(|_| {})
    }

    /// Read the number of metatiles in this tileset, passing a function to
    /// warn any inconsistencies that may appear.
    fn get_metatiles_count_inner<F>(&self, mut warn: F) -> u16
    where
        F: FnMut(&'static str),
    {
        // Try to read the size of the tileset by getting the difference between
        // the metatiles and attributes offsets. Make sure they exist first.
        if let Some(attributes_offset) = self.attributes.offset() {
            if let Some(metatiles_offset) = self.metatiles.offset() {
                // Compute the size
                let size = ((attributes_offset as isize - metatiles_offset as isize) / 16) as u16;
                // If the size seems valid, add it
                if size > 0 {
                    return size;
                }
            } else {
                warn("cannot read metatiles offset");
            }
        } else {
            warn("cannot read attributes offset");
        }
        warn("cannot determine tileset size");

        // TODO Compute an actually valid default
        128
    }
}

#[derive(Debug, Error)]
pub enum MapTilesetError {
    #[error("Invalid primary tileset offset")]
    InvalidPrimaryOffset,
    #[error("Invalid secondary tileset offset")]
    InvalidSecondaryOffset,

    #[error("Invalid palettes offset")]
    NoPalettes,
    #[error("Invalid attributes offset")]
    NoAttributes,
    #[error("Invalid graphics offset")]
    NoGraphics,
    #[error("Invalid metatiles offset")]
    NoMetatiles,

    #[error("IO error: {0}")]
    IoError(#[from] RomIoError),
}

type MapTilesetResult<T = ()> = Result<T, MapTilesetError>;

// ANCHOR Tileset table initialization
#[derive(Serialize, Deserialize, Clone)]
pub struct TilesetShortInfo {
    /// If the tileset is primary
    pub is_primary: bool,
    /// The size of the tileset in tiles
    pub size: u16,
}

/// Loads all the referenced tilesets with their most important information.
pub fn init_info(rom: &mut Rom, log: &mut ProblemsLog) -> Result<(), MapLayoutError> {
    // Do not reload if already loaded.
    if rom.refs.map_tilesets.is_some() {
        return Ok(());
    }

    use super::layout::MapLayoutTable;

    // Get the tilesets from all the layouts
    let mut tileset_offsets: HashSet<Offset> = HashSet::new();
    for index in rom.dump_map_layouts()? {
        // Read the tileset header
        let (primary, secondary) = match rom.read_map_layout_header(index) {
            Ok(h) => (h.primary_tileset, h.secondary_tileset),
            Err(_) => continue,
        };

        // Save all valid offsets
        if let Some(primary_offset) = primary.offset() {
            tileset_offsets.insert(primary_offset);
        }
        if let Some(secondary_offset) = secondary.offset() {
            tileset_offsets.insert(secondary_offset);
        }
    }

    let mut tileset_infos: HashMap<Offset, TilesetShortInfo> = HashMap::new();
    // For each unique offset, read the information we care about
    for offset in tileset_offsets {
        macro_rules! warn {
            ($($arg:tt)*) => {
                log.push_warning(
                    "map_tilesets",
                    format!("Tileset ${:07X}: {}", offset, format!($($arg)*)),
                )
            };
        }

        // Read the tileset at the given offset
        let header: TilesetHeader = match rom.data.read(offset) {
            Ok(header) => header,
            Err(e) => {
                warn!("cannot read header: {}", e);
                continue;
            }
        };
        let is_primary = !header.is_secondary;
        let size = header.get_metatiles_count_inner(|e| warn!("{}", e));

        // Save the information
        tileset_infos.insert(offset, TilesetShortInfo { is_primary, size });
    }

    // Save the found tilesets
    rom.refs.map_tilesets = Some(tileset_infos);

    Ok(())
}
