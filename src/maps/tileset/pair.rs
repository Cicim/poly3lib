use image::RgbaImage;
use rom_data::{
    types::{RomPalette, RomTile},
    Offset, RomBase, RomData, RomValues,
};
use serde::Serialize;

use crate::Rom;

use super::{MapTilesetResult, MetaTile, MetatileLayerType, TilesetData};

#[derive(Debug, RomValues)]
pub struct TilesetNumbers {
    /// `NUM_METATILES_TOTAL`
    #[value(FireRed   0x05a966 MovLsl)]
    #[value(LeafGreen 0x05a966 MovLsl)]
    #[value(Ruby      0x057e3e MovLsl)]
    #[value(Sapphire  0x057e42 MovLsl)]
    #[value(Emerald   0x089f2e MovLsl)]
    #[value(FireRed   0x0590b4 Word Sub(1))]
    #[value(LeafGreen 0x0590b4 Word Sub(1))]
    num_metatiles_total: u16,

    /// `NUM_METATILES_IN_PRIMARY`
    #[value(FireRed   0x05a97c 0x09b808 Word Sub(1))]
    #[value(LeafGreen 0x05a97c 0x09b7dc Word Sub(1))]
    #[value(Ruby      0x057e54          Word Sub(1))]
    #[value(Sapphire  0x057e58          Word Sub(1))]
    #[value(Emerald   0x089f44 0x0e0628 Word Sub(1))]
    #[value(FireRed   0x05a9b0 0x09b84c Word Neg)]
    #[value(LeafGreen 0x05a9b0 0x09b820 Word Neg)]
    #[value(Ruby      0x057e84          Word Neg)]
    #[value(Sapphire  0x057e88          Word Neg)]
    #[value(Emerald   0x089f74 0x0e066c Word Neg)]
    num_metatiles_in_primary: u16,

    /// `NUM_PALS_IN_PRIMARY`
    #[value(FireRed   0x059a22 AddImm8 Mul(32))]
    #[value(LeafGreen 0x059a22 AddImm8 Mul(32))]
    #[value(Ruby      0x056cf4 AddImm8 Mul(32))]
    #[value(Sapphire  0x056cf8 AddImm8 Mul(32))]
    #[value(Emerald   0x088d1e AddImm8 Mul(32))]
    #[value(FireRed   0x059a96 MovImm Mul(32))]
    #[value(LeafGreen 0x059a96 MovImm Mul(32))]
    #[value(Ruby      0x056d52 MovImm Mul(32))]
    #[value(Sapphire  0x056d56 MovImm Mul(32))]
    #[value(Emerald   0x088d92 MovImm Mul(32))]
    #[value(FireRed   0x059aa4 MovImm Mul(16))]
    #[value(LeafGreen 0x059aa4 MovImm Mul(16))]
    #[value(Ruby      0x056d60 MovImm Mul(16))]
    #[value(Sapphire  0x056d64 MovImm Mul(16))]
    #[value(Emerald   0x088da0 MovImm Mul(16))]
    #[value(Emerald   0x085114 MovImm)]
    num_pals_in_primary: u8,

    /// `NUM_PALS_TOTAL - NUM_PALS_IN_PRIMARY`
    #[value(FireRed   0x059aa6 MovImm Mul(32))]
    #[value(LeafGreen 0x059aa6 MovImm Mul(32))]
    #[value(Ruby      0x056d62 MovImm Mul(32))]
    #[value(Sapphire  0x056d66 MovImm Mul(32))]
    #[value(Emerald   0x088da2 MovImm Mul(32))]
    // REVIEW Add num_pals_total here (computed by sum of the two)
    // or by checking offset 0x085120 for Cmp (sub 1) only in emerald
    num_pals_in_secondary: u8,

    /// `NUM_TILES_IN_PRIMARY_VRAM`
    #[value(FireRed   0x059a58 0x059a70 0x059a84 0x059a84 MovLsl)]
    #[value(LeafGreen 0x059a58 0x059a70 0x059a84 0x059a84 MovLsl)]
    // NOTE Some functions need to be patched in Emerald to allow editing the
    // primary tileset size (without editing the secondary tileset size as well)
    #[value(Emerald   0x088d54 0x088d68 0x088d7c 0x088d7c MovLsl)]
    // NOTE: Cannot set a different value from 512 in RS
    // Get the references to the `NUM_TILES_IN_PRIMARY_VRAM` value
    #[value(Ruby      0x056d48 Word Add(0x6000000) Mul(32))]
    #[value(Sapphire  0x056d4c Word Add(0x6000000) Mul(32))]
    num_tiles_in_primary: u16,

    /// `1024 - NUM_TILES_IN_PRIMARY`
    #[value(LeafGreen 0x059a6c 0x059a80 0x059a80 MovLsl)]
    #[value(Emerald   0x088d68 0x088d7c 0x088d7c MovLsl)]
    #[value(FireRed   0x059a6c 0x059a80 0x059a80 MovLsl)]
    #[value(Ruby      0x056d48 Word Add(0x6000000) Mul(32))]
    #[value(Sapphire  0x056d4c Word Add(0x6000000) Mul(32))]
    num_tiles_in_secondary: u16,
}

impl TilesetNumbers {
    /// Try to read the values and return the ROM base's default
    /// values if it fails.
    pub fn read_or_default(data: &RomData) -> Self {
        match Self::read_values(data) {
            Ok(values) => values,
            Err(_) => match data.base {
                RomBase::Ruby | RomBase::Sapphire => TilesetNumbers {
                    num_metatiles_total: 1024,
                    num_metatiles_in_primary: 512,
                    num_pals_in_primary: 6,
                    num_pals_in_secondary: 6,
                    num_tiles_in_primary: 512,
                    num_tiles_in_secondary: 512,
                },
                RomBase::FireRed | RomBase::LeafGreen => TilesetNumbers {
                    num_metatiles_total: 1024,
                    num_metatiles_in_primary: 640,
                    num_pals_in_primary: 7,
                    num_pals_in_secondary: 6,
                    num_tiles_in_primary: 640,
                    num_tiles_in_secondary: 384,
                },
                RomBase::Emerald => TilesetNumbers {
                    num_metatiles_total: 1024,
                    num_metatiles_in_primary: 512,
                    num_pals_in_primary: 6,
                    num_pals_in_secondary: 7,
                    num_tiles_in_primary: 512,
                    num_tiles_in_secondary: 512,
                },
            },
        }
    }
}

// ANCHOR TilesetPair struct
/// Couple of tilesets that are used together.
#[derive(Debug)]
pub struct TilesetPair {
    pub primary: TilesetData,
    pub secondary: TilesetData,

    pub primary_offset: Offset,
    pub secondary_offset: Offset,

    pub numbers: TilesetNumbers,
}

impl TilesetPair {
    pub fn read(
        rom: &Rom,
        primary_offset: Offset,
        secondary_offset: Offset,
    ) -> MapTilesetResult<Self> {
        // Both tilesets need to be read to load the pair
        let primary = TilesetData::read(rom, primary_offset)?;
        let secondary = TilesetData::read(rom, secondary_offset)?;

        // Read the tileset numbers from ROM
        let numbers = TilesetNumbers::read_or_default(&rom.data);

        Ok(Self {
            primary,
            secondary,
            primary_offset,
            secondary_offset,
            numbers,
        })
    }

    /// Transforms this data into a [TilesetPairRenderingData] that can be used
    /// to render the tilesets.
    pub fn into_rendering_data(self) -> TilesetPairRenderingData {
        self.into()
    }
}

impl Into<TilesetPairRenderingData> for TilesetPair {
    fn into(self) -> TilesetPairRenderingData {
        TilesetPairRenderingData::make_from(self)
    }
}

/// Tilesets rendering data read from two tilesets data after loading them.
#[derive(Debug, Serialize)]
pub struct TilesetPairRenderingData {
    /// Palettes from the two tilesets (16 in total)
    /// - `num_pals_in_primary` from the primary tileset
    /// - `num_pals_in_secondary` from the secondary tileset
    /// - the remaining are left blank since they
    ///   cannot be used by the tilesets.
    ///
    /// Palettes are saved as lists of 16 colors (tuples (red, green, blue))
    /// in RGB888 format (values range between 0 and 255).
    pub palettes: Vec<[(u8, u8, u8); 16]>,

    /// The tilesets metatile data (`num_metatiles_total` in total)
    /// - up to `num_metatiles_in_primary` from the primary tileset
    /// - the remainder to arrive to `num_metatiles_primary` before the start
    ///   of the secondary tileset are left blank
    /// - `num_metatiles_in_secondary` from the secondary tileset
    ///
    /// The remainder to arrive to `num_metatiles_total` are left blank.
    pub metatiles: Vec<MetaTile>,

    /// The tilesets tiles data (1024 in total)
    /// - up to `num_tiles_in_primary` from the primary tileset
    /// - the remainder to arrive to `num_tiles_primary` before the start
    ///   of the secondary tileset are left blank
    /// - `num_tiles_in_secondary` from the secondary tileset
    /// - the remainder to arrive to `1024` are left blank.
    pub tiles: Vec<RomTile>,

    /// The metatiles layer types (`num_metatiles_total` in total)
    /// - up to `num_metatiles_in_primary` from the primary tileset
    /// - the remainder to arrive to `num_metatiles_primary` before the start
    ///   of the secondary tileset are left as 0.
    /// - `num_metatiles_in_secondary` from the secondary tileset
    ///
    /// The remainder to arrive to `num_metatiles_total` are left as default.
    pub layer_types: Vec<MetatileLayerType>,
}

impl TilesetPairRenderingData {
    /// Converts the given [TilesetPair] into a [TilesetPairRenderingData],
    pub fn make_from(pair: TilesetPair) -> Self {
        let numbers = pair.numbers;

        // Combine the palettes from the two tilesets
        let mut palettes = Vec::new();
        let num_pals_in_primary = numbers.num_pals_in_primary as usize;
        let num_pals_in_total = numbers.num_pals_in_secondary as usize + num_pals_in_primary;
        palettes.extend(&pair.primary.palettes[0..num_pals_in_primary]);
        palettes.extend(&pair.secondary.palettes[num_pals_in_primary..num_pals_in_total]);
        while palettes.len() < 16 {
            palettes.push(RomPalette::default());
        }
        // Convert the palettes to RGB888 format
        let palettes = palettes.iter().map(RomPalette::to_rgb_colors).collect();

        // Combine the tiles from the two tilesets
        let mut tiles = Vec::new();
        let primary_tiles = pair.primary.graphics.tiles();
        let secondary_tiles = pair.secondary.graphics.tiles();

        let max_tiles_in_primary = numbers.num_tiles_in_primary as usize;
        let actual_num_tiles_in_primary = primary_tiles.len().min(max_tiles_in_primary);
        tiles.extend(&primary_tiles[0..actual_num_tiles_in_primary]);
        while tiles.len() < max_tiles_in_primary {
            tiles.push(RomTile::default());
        }
        tiles.extend(
            secondary_tiles
                .iter()
                .take(numbers.num_tiles_in_secondary as usize),
        );

        // Combine the metatiles from the two tilesets
        let mut metatiles = Vec::new();
        let num_metatiles_in_primary = numbers.num_metatiles_in_primary as usize;
        let num_metatiles_in_secondary =
            numbers.num_metatiles_total as usize - numbers.num_metatiles_in_primary as usize;

        metatiles.extend(pair.primary.metatiles.iter().take(num_metatiles_in_primary));
        while metatiles.len() < num_metatiles_in_primary {
            metatiles.push(MetaTile::default());
        }
        metatiles.extend(
            pair.secondary
                .metatiles
                .iter()
                .take(num_metatiles_in_secondary),
        );

        // Combine the layer types from the two tilesets
        let mut layer_types = Vec::new();

        layer_types.extend(
            pair.primary
                .attributes
                .iter()
                .take(num_metatiles_in_primary)
                .map(|attr| attr.layer_type),
        );
        while layer_types.len() < num_metatiles_in_primary {
            layer_types.push(MetatileLayerType::default());
        }
        layer_types.extend(
            pair.secondary
                .attributes
                .iter()
                .take(num_metatiles_in_secondary)
                .map(|attr| attr.layer_type),
        );

        Self {
            palettes,
            metatiles,
            tiles,
            layer_types,
        }
    }

    /// Returns the number of metatiles in these tilesets.
    #[inline]
    pub fn len(&self) -> usize {
        self.metatiles.len()
    }

    /// Draws the metatile at the given coordinates on the image.
    pub fn draw_metatile(&self, index: usize, buffer: &mut RgbaImage, offx: usize, offy: usize) {
        // Make sure the metatile is within bounds
        if index > self.len() {
            return;
        }

        let palettes = &self.palettes;
        let tiles = &self.tiles;

        let bot = self.metatiles[index].0;
        let mid = self.metatiles[index].1;

        macro_rules! draw_tile {
            ($tile:ident) => {
                $tile[0].draw_on_buffer(palettes, tiles, buffer, offx, offy);
                $tile[1].draw_on_buffer(palettes, tiles, buffer, offx + 8, offy);
                $tile[2].draw_on_buffer(palettes, tiles, buffer, offx, offy + 8);
                $tile[3].draw_on_buffer(palettes, tiles, buffer, offx + 8, offy + 8);
            };
        }

        draw_tile!(bot);
        draw_tile!(mid);
        // For three layers
        if self.layer_types[index] == MetatileLayerType::ThreeLayers {
            // Read the next metatile
            if index + 1 > self.len() {
                return;
            }

            let top = self.metatiles[index + 1].0;
            draw_tile!(top);
        }
    }

    /// Renders the entirety of this metatile into an image (8 blocks wide)
    pub fn render_preview_image(&self) -> RgbaImage {
        const WIDTH: usize = 8;

        // Get the final width and height
        let width = WIDTH * 16;
        // ceil(num_metatiles / 16) * 16
        let height = ((self.len() + WIDTH - 1) / WIDTH) * 16;

        // Create the image buffer
        let mut buffer: _ = RgbaImage::new(width as u32, height as u32);

        for index in 0..self.len() {
            let offx = (index % WIDTH) * 16;
            let offy = (index / WIDTH) * 16;

            self.draw_metatile(index as usize, &mut buffer, offx, offy)
        }

        buffer
    }
}
