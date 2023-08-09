use serde::Serialize;
use thiserror::Error;

use gba_macro::gba_struct;
use gba_types::{colors::GBAPalette, tiles::MetaTile, GBAIOError};

use crate::{
    graphics::{Graphic, GraphicTile},
    rom::{Rom, RomType},
    values::ValueGrabError,
};

use super::{render::TilesetsPair, tileset_anims::TilesetAnimationList};

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
    /// Animation data for the tileset
    ///
    /// This is loaded at a later time explicitly
    pub animations: Option<TilesetAnimationList>,
}

#[derive(Debug, Error)]
pub enum TilesetReadingError {
    #[error("Invalid tileset offset")]
    InvalidTilesetOffset,
    #[error("Invalid palette offset")]
    InvalidPaletteOffset,
    #[error("Invalid attributes offset")]
    InvalidAttributesOffset,
    #[error("Invalid graphics offset")]
    InvalidGraphicsOffset,
    #[error("Invalid metatile offset")]
    InvalidMetaTileOffset,

    #[error("Cannot read ROM value: {0}")]
    CannotReadRomValue(#[from] ValueGrabError),
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

            // Loading animations is optional
            animations: None,
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
            let num_tiles_in_secondary_gfx = 1024 - rom.get_primary_tiles_count()?;

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

/// Contains everything that is needed to draw a tileset from scratch.
#[derive(Debug, Serialize)]
pub struct TilesetsRenderData {
    /// Every single tile in both tilesets, including missing tiles
    /// inbetween, which are just filled with all zeros
    pub tiles: Vec<GraphicTile>,
    /// Every single metatile in both tilesets, including missing metatiles
    /// inbetween, which are just filled with all zeros
    pub metatiles: Vec<MetaTile>,
    /// Layer type attribute for every metatile in both tilesets.
    /// Exported as u8 for convenience
    pub metatile_layers: Vec<u8>,
    /// The palettes combined from the two tilesets already
    /// converted to RGBA.
    pub palettes: [GBAPalette; 16],

    /// When the secondary tileset starts
    pub tile_limit: usize,
    /// When the secondary tileset starts
    pub metatile_limit: usize,
}

impl TilesetsRenderData {
    pub fn new(
        rom: &Rom,
        primary: TilesetData,
        secondary: TilesetData,
    ) -> Result<Self, TilesetReadingError> {
        // Read the number of palettes in the primary tileset and the total number of palettes
        let pals_in_primary = rom.get_primary_palettes_count()?;
        let total_pals = rom.get_palettes_count()?;

        /* Palettes */
        // Combine the two palettes
        let mut palettes: [GBAPalette; 16] = [GBAPalette::default(); 16];
        // Read the palettes from the primary tileset
        for i in 0..pals_in_primary {
            palettes[i] = primary.palettes[i];
        }
        // Read the palettes from the secondary tileset
        for i in pals_in_primary..total_pals {
            palettes[i] = secondary.palettes[i];
        }

        /* Tiles */
        // Read the number of tiles after which the secondary tileset starts
        let tile_limit = rom.get_primary_tiles_count()?;
        // Merge the tiles
        let mut tiles = vec![];
        // Get the tiles from the primary tileset
        for tile in primary.graphics.tiles {
            tiles.push(tile);
        }
        // Fill with 0s until you arrive to the second tileset
        while tiles.len() < tile_limit {
            tiles.push([[0; 8]; 8]);
        }
        // Get the rest of the tiles from the secondary tileset
        for tile in secondary.graphics.tiles {
            tiles.push(tile);
        }

        /* Metatiles */
        // Read primary and secondary tileset max sizes
        let (metatile_limit, _) = rom.get_metatiles_count()?;
        // Merge the metatiles
        let mut metatiles = vec![];
        // Merge the metatiles layer types
        let mut metatile_layers = vec![];

        // Get the first part from the primary
        for mt in primary.metatiles {
            metatiles.push(mt);
        }
        for attr in primary.attributes {
            let layer_type = attr.layer_type as u8;
            metatile_layers.push(layer_type);
        }
        // Fill with 0s until you arrive to the second tileset
        while metatiles.len() < metatile_limit {
            metatiles.push(MetaTile::default());
            metatile_layers.push(0);
        }
        // Get the last part from the secondary
        for mt in secondary.metatiles {
            metatiles.push(mt);
        }
        for attr in secondary.attributes {
            let layer_type = attr.layer_type as u8;
            metatile_layers.push(layer_type);
        }

        Ok(Self {
            metatiles,
            metatile_layers,
            palettes,
            tile_limit,
            metatile_limit,
            tiles,
        })
    }
}

impl TilesetsPair {
    /// Get the render data for these tilesets
    pub fn get_render_data(self, rom: &Rom) -> Result<TilesetsRenderData, TilesetReadingError> {
        TilesetsRenderData::new(rom, self.primary, self.secondary)
    }
}
