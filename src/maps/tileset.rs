/* When working with tilesets, we don't really have a universal table that
 * contains all of them, so we will refer to rom.refs.tilesets_table
 * to check information such as tileset size, which we will have to
 * diligently synchronize to keep them consistent. */
use gba_macro::gba_struct;

use gba_types::{colors::GBAPalette, tiles::MetaTile};

use crate::rom::Rom;

gba_struct!(TilesetHeader {
    u8 is_compressed;
    u8 is_secondary;
    void* graphics;
    void* palettes;
    void* metatiles;
    void* animations;
    void* behaviors;
});

/**************************************/
/* ANCHOR    Loading/saving functions */
/**************************************/
#[derive(Debug)]
/// Data that can be exported to a program to give a
/// comprehensive view of a tileset.
pub struct TilesetData {
    /// The tileset's header, as read from the ROM
    pub header: TilesetHeader,
    /// The tileset's graphics (stored as a collection of tiles)
    pub graphics: Vec<u8>,
    /// The tileset's palettes
    pub palettes: Vec<GBAPalette>,

    /// The behavior bytes for each block
    // Stored as u32 because they may be different sizes depending on the ROM type
    pub behaviors: Vec<u32>,
    /// The metatiles that make up the tileset
    pub metatiles: Vec<MetaTile>,
    // TODO Handle animations
    /// Warnings that may have been generated during the reading process
    pub warnings: Vec<String>,
}

pub enum TilesetReadingError {
    InvalidTilesetOffset,
    InvalidPaletteOffset,
    InvalidBehaviorOffset,
}

/// Reads the tileset's data from ROM.
pub fn read_tileset_data(
    rom: &Rom,
    header_offset: usize,
) -> Result<TilesetData, TilesetReadingError> {
    let header = rom
        .read(header_offset)
        .map_err(|_| TilesetReadingError::InvalidTilesetOffset)?;

    // let graphics = read_tileset_graphics(rom, &header)?;
    let graphics = vec![];

    let palettes = read_tileset_palettes(rom, &header)?;
    // let behaviors = read_tileset_behaviors(rom, &header)?;
    // let metatiles = read_tileset_metatiles(rom, &header)?;

    Ok(TilesetData {
        header,
        graphics,
        palettes,
        behaviors: vec![],
        metatiles: vec![],
        warnings: vec![],
    })
}

/// Reads the tileset's palettes from ROM.
fn read_tileset_palettes(
    rom: &Rom,
    header: &TilesetHeader,
) -> Result<Vec<GBAPalette>, TilesetReadingError> {
    let palette_offset = header
        .palettes
        .offset()
        .ok_or_else(|| TilesetReadingError::InvalidPaletteOffset)?
        as usize;

    let palettes: [GBAPalette; 16] = rom
        .read(palette_offset)
        .map_err(|_| TilesetReadingError::InvalidPaletteOffset)?;
    let palettes = palettes.to_vec();

    Ok(palettes)
}

/**************************************/
/* ANCHOR    Helper methods ***********/
/**************************************/
