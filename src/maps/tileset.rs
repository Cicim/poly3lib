use std::fmt::Display;

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

#[repr(u8)]
#[derive(Debug, Default)]
pub enum MetatileLayerType {
    #[default]
    /// Metatile uses middle and top bg layers
    Normal = 0,
    /// Metatile uses bottom and middle bg layers
    Covered = 1,
    /// Metatile uses bottom and top bg layers
    Split = 2,

    /// Metatile uses bottom, middle and top bg layers
    /// and retrieves the top layer from the next tile
    ///
    /// Only available if the ROM is patched with
    /// the "3 layers" patch
    ThreeLayers = 3,
}

/// The attributes of a metatile organized into a struct
/// for interoperability between versions.
#[derive(Debug, Default)]
pub struct MetatileAttributes {
    /// Specifies the behavior of the tile
    ///
    /// Present in all versions
    pub behavior: u16,
    /// Specifies the terrain type of the tile
    /// NORMAL, GRASS, WATER or WATERFALL
    ///
    /// Present only in FireRed
    pub terrain: u8,
    /// Specifies the encounter type of the tile
    /// NONE, LAND or WATER
    ///
    /// Present only in FireRed
    pub encounter_type: u8,
    /// Specifies whether the player passed behind the top layer
    ///
    /// Present in all versions
    pub layer_type: MetatileLayerType,
}

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

    /// The attributes for each metatile
    pub attributes: Vec<MetatileAttributes>,
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

    CannotReadRomValue,
}

impl Display for TilesetReadingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TilesetReadingError::*;

        match self {
            InvalidTilesetOffset => write!(f, "Invalid tileset offset"),
            InvalidPaletteOffset => write!(f, "Invalid palette offset"),
            InvalidAttributesOffset => write!(f, "Invalid attributes offset"),
            InvalidGraphicsOffset => write!(f, "Invalid graphics offset"),
            InvalidMetaTileOffset => write!(f, "Invalid metatile offset"),
            CannotReadRomValue => write!(f, "Cannot read ROM value"),
        }
    }
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

        let graphics = header.read_graphics(rom)?;
        let palettes = header.read_palettes(rom)?;
        let attributes = header.read_attributes(rom, size)?;
        let metatiles = header.read_metatiles(rom, size)?;

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
    fn read_graphics(&self, rom: &Rom) -> Result<Graphic, TilesetReadingError> {
        let gfx_offset =
            self.graphics
                .offset()
                .ok_or_else(|| TilesetReadingError::InvalidGraphicsOffset)? as usize;

        // Handle compressed and uncompressed graphics
        let size = if self.is_compressed != 0 {
            None
        } else {
            // This 1024 should be stable
            let num_tiles_in_secondary_gfx = 1024
                - rom
                    .get_primary_tiles_count()
                    .map_err(|_| TilesetReadingError::CannotReadRomValue)?;

            Some(num_tiles_in_secondary_gfx)
        };

        let graphics = Graphic::read(rom, gfx_offset, size)
            .map_err(|_| TilesetReadingError::InvalidGraphicsOffset)?;

        Ok(graphics)
    }

    /// Reads this tileset's palettes from ROM.
    fn read_palettes(&self, rom: &Rom) -> Result<Vec<GBAPalette>, TilesetReadingError> {
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
    fn read_attributes(
        &self,
        rom: &Rom,
        size: usize,
    ) -> Result<Vec<MetatileAttributes>, TilesetReadingError> {
        let attributes_offset =
            self.attributes
                .offset()
                .ok_or_else(|| TilesetReadingError::InvalidAttributesOffset)? as usize;

        let mut attributes = Vec::new();

        // Check the Rom type
        for i in 0..size {
            let attr = match rom.rom_type {
                RomType::FireRed | RomType::LeafGreen => {
                    // The attribute is an u32
                    let attribute: u32 = rom
                        .read(attributes_offset + i * 4)
                        .map_err(|_| TilesetReadingError::InvalidAttributesOffset)?;

                    MetatileAttributes {
                        behavior: (attribute & 0x000001ff) as u16,
                        terrain: (attribute & 0x00003e00 >> 9) as u8,
                        encounter_type: ((attribute & 0x07000000) >> 24) as u8,
                        layer_type: (((attribute & 0x60000000) >> 29) as u8).into(),
                    }
                }
                RomType::Emerald | RomType::Ruby | RomType::Sapphire => {
                    // The attribute is an u16
                    let attribute: u16 = rom
                        .read(attributes_offset + i * 2)
                        .map_err(|_| TilesetReadingError::InvalidAttributesOffset)?;

                    MetatileAttributes {
                        behavior: (attribute & 0xff) as u16,
                        terrain: 0,
                        encounter_type: 0,
                        layer_type: (((attribute & 0xf000) >> 12) as u8).into(),
                    }
                } // _ => {
                  //     // If it's not one of the above, we don't know how to read the attributes
                  //     return Err(TilesetReadingError::InvalidAttributesOffset);
                  // }
            };

            attributes.push(attr);
        }

        Ok(attributes)
    }

    /// Reads the tileset's metatiles from ROM.
    fn read_metatiles(&self, rom: &Rom, size: usize) -> Result<Vec<MetaTile>, TilesetReadingError> {
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

impl Into<MetatileLayerType> for u8 {
    fn into(self) -> MetatileLayerType {
        match self % 4 {
            0 => MetatileLayerType::Normal,
            1 => MetatileLayerType::Covered,
            2 => MetatileLayerType::Split,
            3 => MetatileLayerType::ThreeLayers,
            _ => unreachable!("The layer type compared after a modulo 4 operation"),
        }
    }
}
