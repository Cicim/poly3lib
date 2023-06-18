/* When working with tilesets, we don't really have a universal table that
 * contains all of them, so we will refer to rom.refs.tilesets_table
 * to check information such as tileset size, which we will have to
 * diligently synchronize to keep them consistent. */
use gba_macro::gba_struct;

use gba_types::{colors::GBAPalette, tiles::MetaTile};

use crate::{graphics::Graphic, rom::Rom};

const DEFAULT_TILESET_BLOCKS: usize = 256;

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
    pub graphics: Graphic,
    /// The tileset's palettes
    pub palettes: Vec<GBAPalette>,

    /// The behavior bytes for each block
    // Stored as u32 because they may be different sizes depending on the ROM type
    pub behaviors: Vec<u32>,
    /// The metatiles that make up the tileset
    pub metatiles: Vec<MetaTile>,
    // TODO Handle animations
}

#[derive(Debug)]
pub enum TilesetReadingError {
    InvalidTilesetOffset,
    InvalidPaletteOffset,
    InvalidBehaviorOffset,
    InvalidGraphicsOffset,
    InvalidMetaTileOffset,
}

impl TilesetData {
    /// Reads the tileset's data from ROM.
    pub fn read(rom: &Rom, tileset_offset: usize) -> Result<TilesetData, TilesetReadingError> {
        let header: TilesetHeader = rom
            .read(tileset_offset)
            .map_err(|_| TilesetReadingError::InvalidTilesetOffset)?;

        let size = match rom.refs.get_tileset_size(tileset_offset) {
            Some(size) => size,
            None => header.get_size(rom),
        };

        let graphics = header.get_graphics(rom)?;
        let palettes = header.get_palettes(rom)?;
        let behaviors = header.get_behaviors(rom, size)?;
        let metatiles = header.get_metatiles(rom, size)?;

        Ok(TilesetData {
            header,
            graphics,
            palettes,
            behaviors,
            metatiles,
        })
    }

    pub fn print_small(&self, col: usize, borders: bool, palette: usize) {
        self.graphics
            .print_small(col, borders, &self.palettes[palette]);
    }
    pub fn print_large(&self, col: usize, headers: bool, palette: usize) {
        self.graphics
            .print_large(col, headers, &self.palettes[palette]);
    }
}

impl TilesetHeader {
    /// Returns the size in blocks of the tileset based on the adjacency
    /// of the blocks and behaviors offsets.
    ///
    /// If that check fails, it uses the default size.
    ///
    /// If that fails too, it returns a default as a last resort.
    pub fn get_size(&self, rom: &Rom) -> usize {
        let is_secondary = self.is_secondary != 0;

        // Make sure both the blocks offset and the behaviors offset are valid
        if let Some(blocks_offset) = self.metatiles.offset() {
            if let Some(behaviors_offset) = self.behaviors.offset() {
                // These should be adjacent to each other
                let size = (behaviors_offset - blocks_offset) / 16;

                if size > 0 {
                    return size as usize;
                }
            }
        }

        // In any other case, resort to using the default size
        println!("[Warning] Cannot determine tileset size for tileset, using default");
        match rom.get_metatiles_count() {
            Ok((primary_size, secondary_size)) => {
                if is_secondary {
                    secondary_size
                } else {
                    primary_size
                }
            }
            Err(e) => {
                println!(
                    "[Error] Cannot determine the default tileset size, using {}",
                    DEFAULT_TILESET_BLOCKS
                );
                println!("[Error] {}", e);
                DEFAULT_TILESET_BLOCKS
            }
        }
    }

    /// Reads this tileset's graphics from ROM.
    fn get_graphics(&self, rom: &Rom) -> Result<Graphic, TilesetReadingError> {
        let gfx_offset =
            self.graphics
                .offset()
                .ok_or_else(|| TilesetReadingError::InvalidGraphicsOffset)? as usize;

        // TODO Handle uncompressed graphics
        let graphics = Graphic::read(rom, gfx_offset, None)
            .map_err(|_| TilesetReadingError::InvalidGraphicsOffset)?;

        Ok(graphics)
    }

    /// Reads this tileset's palettes from ROM.
    fn get_palettes(&self, rom: &Rom) -> Result<Vec<GBAPalette>, TilesetReadingError> {
        let palette_offset =
            self.palettes
                .offset()
                .ok_or_else(|| TilesetReadingError::InvalidPaletteOffset)? as usize;

        let palettes: [GBAPalette; 16] = rom
            .read(palette_offset)
            .map_err(|_| TilesetReadingError::InvalidPaletteOffset)?;
        let palettes = palettes.to_vec();

        Ok(palettes)
    }

    /// Reads this tileset's behaviors from ROM.
    fn get_behaviors(&self, rom: &Rom, size: usize) -> Result<Vec<u32>, TilesetReadingError> {
        let behaviors_offset =
            self.behaviors
                .offset()
                .ok_or_else(|| TilesetReadingError::InvalidBehaviorOffset)? as usize;

        let mut behaviors = Vec::new();

        // TODO Check if they have are of a different size in Emerald
        for i in 0..size {
            let behavior_offset = behaviors_offset + i * 4;
            let behavior: u32 = rom
                .read(behavior_offset)
                .map_err(|_| TilesetReadingError::InvalidBehaviorOffset)?;
            behaviors.push(behavior);
        }

        Ok(behaviors)
    }

    /// Reads the tileset's metatiles from ROM.
    fn get_metatiles(&self, rom: &Rom, size: usize) -> Result<Vec<MetaTile>, TilesetReadingError> {
        let metatiles_offset =
            self.metatiles
                .offset()
                .ok_or_else(|| TilesetReadingError::InvalidMetaTileOffset)? as usize;

        let mut metatiles = Vec::new();

        for i in 0..size {
            let metatile_offset = metatiles_offset + i * 16;
            let metatile: MetaTile = rom
                .read(metatile_offset)
                .map_err(|_| TilesetReadingError::InvalidMetaTileOffset)?;
            metatiles.push(metatile);
        }

        Ok(metatiles)
    }
}
