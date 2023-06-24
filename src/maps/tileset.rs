/* When working with tilesets, we don't really have a universal table that
 * contains all of them, so we will refer to rom.refs.tilesets_table
 * to check information such as tileset size, which we will have to
 * diligently synchronize to keep them consistent. */
use gba_macro::gba_struct;

use gba_types::{colors::GBAPalette, tiles::MetaTile, GBAIOError};

use crate::{
    graphics::Graphic,
    rom::{Rom, RomType},
};

const DEFAULT_TILESET_BLOCKS: usize = 256;

gba_struct!(TilesetHeader {
    u8 is_compressed;
    u8 is_secondary;
    void* graphics;
    void* palettes;
    void* metatiles;
    void* animations;
    void* attributes;
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

    /// The attribute bytes for each block
    ///
    /// Contains behavior, terrain, encounter and layer type information
    pub attributes: Vec<u32>,
    /// The metatiles that make up the tileset
    pub metatiles: Vec<MetaTile>,
    // TODO Handle animations
}

#[derive(Debug)]
pub enum TilesetReadingError {
    InvalidTilesetOffset,
    InvalidPaletteOffset,
    InvalidAttributesOffset,
    InvalidGraphicsOffset,
    InvalidMetaTileOffset,
}

impl TilesetData {
    /// Reads the tileset's data from ROM.
    pub fn read(rom: &Rom, tileset_offset: usize) -> Result<TilesetData, TilesetReadingError> {
        let header = TilesetHeader::read(rom, tileset_offset)
            .map_err(|_| TilesetReadingError::InvalidTilesetOffset)?;

        let size = match rom.refs.get_tileset_size(tileset_offset) {
            Some(size) => size,
            None => header.get_size(rom),
        };

        let graphics = header.get_graphics(rom)?;
        let palettes = header.get_palettes(rom)?;
        let attributes = header.get_attributes(rom, size)?;
        let metatiles = header.get_metatiles(rom, size)?;

        Ok(TilesetData {
            header,
            graphics,
            palettes,
            attributes,
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
    /// Reads the tileset header with the correct format from ROM
    pub fn read(rom: &Rom, offset: usize) -> Result<TilesetHeader, GBAIOError> {
        let mut header: TilesetHeader = rom.read(offset)?;

        // If you are Emerald, you are special
        match rom.rom_type {
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => {
                // Emerald has animations and attributes swapped
                std::mem::swap(&mut header.animations, &mut header.attributes)
            }
            _ => {}
        }

        Ok(header)
    }

    /// Writes the tileset header with the correct format to ROM
    pub fn write(&self, rom: &mut Rom, offset: usize) -> Result<(), GBAIOError> {
        let mut header = self.clone();

        // If you are Emerald, you are special
        match rom.rom_type {
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => {
                // Emerald has animations and attributes swapped
                std::mem::swap(&mut header.animations, &mut header.attributes)
            }
            _ => {}
        }

        rom.write(offset, header)
    }

    /// Returns the size in blocks of the tileset based on the adjacency
    /// of the blocks and attributes offsets.
    ///
    /// If that check fails, it uses the default size.
    ///
    /// If that fails too, it returns a default as a last resort.
    pub fn get_size(&self, rom: &Rom) -> usize {
        let is_secondary = self.is_secondary != 0;

        // Make sure both the blocks offset and the attributes offset are valid
        if let Some(blocks_offset) = self.metatiles.offset() {
            if let Some(attributes_offset) = self.attributes.offset() {
                // These should be adjacent to each other
                let size = (attributes_offset - blocks_offset) / 16;

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

    /// Reads this tileset's attributes from ROM.
    fn get_attributes(&self, rom: &Rom, size: usize) -> Result<Vec<u32>, TilesetReadingError> {
        let attributes_offset =
            self.attributes
                .offset()
                .ok_or_else(|| TilesetReadingError::InvalidAttributesOffset)? as usize;

        let mut attributes = Vec::new();

        // TODO Check if they have are of a different size in Emerald
        for i in 0..size {
            let attribute_offset = attributes_offset + i * 4;
            let attribute: u32 = rom
                .read(attribute_offset)
                .map_err(|_| TilesetReadingError::InvalidAttributesOffset)?;
            attributes.push(attribute);
        }

        Ok(attributes)
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
