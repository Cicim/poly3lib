use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};
use thiserror::Error;

use rom_data::{
    rom_struct,
    types::{RomGraphic, RomPointer, RomSizedType, RomTile},
    Offset, RomIoError,
};

use crate::Rom;

use super::{layout::MapLayoutError, ProblemsLog};

mod anim;
mod data;
mod pair;

pub use anim::{TilesetAnimation, TilesetAnimationError, TilesetAnimationList};
pub use data::{MetaTile, MetatileAttributes, MetatileLayerType, TilesetData};
pub use pair::{TilesetNumbers, TilesetPair, TilesetPairRenderingData};

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

        // TODO Handle uncompressed gfx size
        128
    }
}

/// Helper type for the result of map tileset operations.
type MapTilesetResult<T = ()> = Result<T, MapTilesetError>;

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

    #[error("The tileset table was not initialized")]
    TilesetTableNotInitialized,
    #[error("This tileset is in the table")]
    TilesetNotInTable,

    #[error("IO error: {0}")]
    IoError(#[from] RomIoError),
}

// ANCHOR Methods
impl Rom {
    /// Reads a [`TilesetPair`] from the given offsets.
    ///
    /// Returns an error if either tileset offset is invalid.
    pub fn read_tileset_pair(
        &self,
        primary_offset: Offset,
        secondary_offset: Offset,
    ) -> MapTilesetResult<TilesetPair> {
        // Use the read method
        TilesetPair::read(self, primary_offset, secondary_offset)
    }

    /// Deletes the given tileset from the table and from ROM.
    pub fn delete_tileset(&mut self, offset: Offset) -> MapTilesetResult {
        // Check if the table contains the given offset
        if let Some(TilesetShortInfo { size, .. }) = get_table(self)?.get(&offset) {
            // Read the header
            let header: TilesetHeader = self.data.read(offset)?;
            let size = *size as usize;

            // Read the offset of metatiles and attributes
            if let Some(metatiles_offset) = header.metatiles.offset() {
                self.data.clear_bytes(metatiles_offset, size * 16)?;
            }
            if let Some(attributes_offset) = header.attributes.offset() {
                // TODO Handle different attributes size
                let elsize = MetatileAttributes::get_size(&self.data);
                self.data.clear_bytes(attributes_offset, size * elsize)?;
            }

            // Delete the palettes
            if let Some(palettes_offset) = header.palettes.offset() {
                self.data.clear_bytes(palettes_offset, 16 * 32)?;
            }

            // Delete the tiles
            if let Some(graphics_offset) = header.graphics.offset() {
                if header.is_compressed {
                    // Clear compressed data
                    self.data.clear_compressed_data(graphics_offset)?;
                } else {
                    // TODO Handle uncompressed gfx size
                    self.data.clear_bytes(offset, 128 * 32)?;
                }
            }

            // TODO Handle deleting animations

            // TODO Better clearing interface
            let header_size = TilesetHeader::get_size(&self.data);
            self.data.clear_bytes(offset, header_size)?;

            // Delete the tileset from the table
            get_mut_table(self)?.remove_entry(&offset);

            Ok(())
        } else {
            Err(MapTilesetError::TilesetNotInTable)
        }
    }

    /// Creates a new tileset of the given type (primary or secondary)
    /// with the minimum possible size for everything.
    ///
    /// Returns the offset of the new tileset.
    pub fn create_tileset(&mut self, is_primary: bool) -> MapTilesetResult<Offset> {
        const DEFAULT_METATILES: usize = 8;
        const DEFAULT_TILES: usize = 1;

        // Create the new header
        let header_size = TilesetHeader::get_size(&self.data);
        // Allocate the header
        let header_offset = self.data.find_free_space(header_size, 4)?;
        self.data.allocate(header_offset, header_size)?;

        // Allocate the palettes
        let palettes_offset = self.data.find_free_space(32 * 16, 2)?;
        self.data.allocate(palettes_offset, 32 * 16)?;

        // Write the metatiles and attributes
        let attr_elsize = MetatileAttributes::get_size(&self.data);
        let mtattr_size = (16 + attr_elsize) * DEFAULT_METATILES;

        let metatile_offset = self.data.find_free_space(mtattr_size, 4)?;
        let attributes_offset = metatile_offset + 16 * DEFAULT_METATILES;
        self.data.allocate(metatile_offset, mtattr_size)?;

        // Write the compressed graphics
        let graphics = RomGraphic::New(vec![RomTile::default(); DEFAULT_TILES]);
        let graphics_offset = graphics.write(&mut self.data, true)?;

        self.data.write(
            header_offset,
            TilesetHeader {
                is_compressed: true,
                is_secondary: !is_primary,
                graphics: RomPointer::new(graphics_offset),
                palettes: RomPointer::new(palettes_offset),
                metatiles: RomPointer::new(metatile_offset),
                attributes: RomPointer::new(attributes_offset),
                animations: RomPointer::Null,
            },
        )?;

        Ok(header_offset)
    }
}

/// Returns a reference to the tileset table
fn get_table(rom: &Rom) -> MapTilesetResult<&HashMap<usize, TilesetShortInfo>> {
    rom.refs
        .map_tilesets
        .as_ref()
        .ok_or(MapTilesetError::TilesetTableNotInitialized)
}
/// Returns a mutable reference to the tileset table
fn get_mut_table(rom: &mut Rom) -> MapTilesetResult<&mut HashMap<usize, TilesetShortInfo>> {
    rom.refs
        .map_tilesets
        .as_mut()
        .ok_or(MapTilesetError::TilesetTableNotInitialized)
}

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
