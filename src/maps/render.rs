use std::io::Cursor;

use base64::{engine::general_purpose, Engine};
use gba_types::{colors::GBAPalette, pointers::PointedData};
use image::{ImageFormat, Rgb, RgbImage, Rgba, RgbaImage};
use serde::{ser::SerializeTuple, Serialize};

use crate::rom::Rom;

use super::{
    layout::{MapLayout, MapLayoutData},
    tileset::{TilesetData, TilesetReadingError},
};

/// Palette of colors converted to RGBA.
type RgbaPalette = [Rgba<u8>; 16];

/// This struct represents the rendered metatiles for a map.
///
/// The first component is the bottom layer of the metatile, and the second component is the top layer.
pub struct RenderedMetatile(RgbaImage, RgbaImage);

impl Serialize for RenderedMetatile {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let bottom = rgba_image_to_base64(&self.0);
        let top = rgba_image_to_base64(&self.1);

        let mut state = serializer.serialize_tuple(2)?;
        state.serialize_element(&bottom)?;
        state.serialize_element(&top)?;
        state.end()
    }
}

impl RenderedMetatile {
    /// Returns a single color for the given pixel by putting
    /// the top layer first and the bottom layer second.
    fn merge_layers(&self, x: u32, y: u32) -> Rgb<u8> {
        let bot = self.0[(x, y)];
        let top = self.1[(x, y)];

        if top[3] == 0 {
            if bot[3] == 0 {
                Rgb([0, 0, 0])
            } else {
                Rgb([bot[0], bot[1], bot[2]])
            }
        } else {
            Rgb([top[0], top[1], top[2]])
        }
    }

    /// Draws this metatile on the given image at the given coordinates.
    /// The coordinates are in metatiles, not pixels.
    fn draw(&self, image: &mut RgbImage, x: u32, y: u32) {
        for i in 0..16 {
            for j in 0..16 {
                let color = self.merge_layers(i, j);
                image.put_pixel(x * 16 + i, y * 16 + j, color);
            }
        }
    }
}

#[derive(Debug)]
/// This struct contains everything needed to render a tileset.
pub struct TilesetsPair {
    /// The primary tileset
    pub primary: TilesetData,
    /// The secondary tileset
    pub secondary: TilesetData,
    /// The palettes combined from the two tilesets
    pub palettes: Vec<RgbaPalette>,
    /// The index after which the secondary tileset starts
    pub tile_limit: usize,
    /// The index after which the secondary metatileset starts
    pub metatile_limit: usize,
    /// The total number of metatiles in the tileset
    pub metatile_count: usize,
}

impl TilesetsPair {
    pub fn new(rom: &Rom, tileset_1: usize, tileset_2: usize) -> Result<Self, TilesetReadingError> {
        // Read the two tilesets
        let primary = TilesetData::read(rom, tileset_1)?;
        let secondary = TilesetData::read(rom, tileset_2)?;

        // Read the number of palettes in the primary tileset and the total number of palettes
        let pals_in_primary =
            rom.get_primary_palettes_count()
                .map_err(|_| TilesetReadingError::CannotReadRomValue)? as usize;
        let total_pals =
            rom.get_palettes_count()
                .map_err(|_| TilesetReadingError::CannotReadRomValue)? as usize;

        // Combine the two palettes
        let mut palettes: Vec<RgbaPalette> = Vec::with_capacity(16);
        // Read the palettes from the primary tileset
        for i in 0..pals_in_primary {
            let pals = convert_pal_to_rgba(primary.palettes[i]);
            palettes.push(pals);
        }
        for i in pals_in_primary..total_pals {
            let pals = convert_pal_to_rgba(secondary.palettes[i]);
            palettes.push(pals);
        }
        // Fill the rest with an impossible color
        for _ in total_pals..16 {
            palettes.push([Rgba([255, 255, 255, 128]); 16]);
        }

        // Read the number of tiles after which the secondary tileset starts
        let tile_limit = rom
            .get_primary_tiles_count()
            .map_err(|_| TilesetReadingError::CannotReadRomValue)?;

        // Read primary and secondary tileset max sizes
        let (metatile_limit, metatile_count) = rom
            .get_metatiles_count()
            .map_err(|_| TilesetReadingError::CannotReadRomValue)?;

        // Read the number of metatiles after which the secondary tileset starts
        let metatile_limit = metatile_limit as usize;
        // Read the total number of metatiles you have to read
        let metatile_count =
            metatile_limit + metatile_count.min(secondary.metatiles.len()) as usize;

        Ok(Self {
            primary,
            secondary,
            palettes,
            tile_limit,
            metatile_limit,
            metatile_count,
        })
    }

    /// Returns a list of [`RenderedMetatile`]s for both tilesets.
    ///
    /// Never fails, because the missing metatiles are replaced with transparent ones.
    pub fn render(&self) -> Vec<RenderedMetatile> {
        let mut tiles = Vec::with_capacity(self.metatile_count);

        for i in 0..self.metatile_count {
            tiles.push(self.render_metatile(i));
        }

        tiles
    }

    // TODO Incorporate attributes for Cover Layer and Third Layer functionality
    pub fn render_metatile(&self, index: usize) -> RenderedMetatile {
        let mut bottom_layer = RgbaImage::new(16, 16);
        let mut top_layer = RgbaImage::new(16, 16);

        let metatile = if index < self.metatile_limit {
            if index >= self.primary.metatiles.len() {
                return RenderedMetatile(bottom_layer, top_layer);
            }

            &self.primary.metatiles[index]
        } else {
            if index - self.metatile_limit >= self.secondary.metatiles.len() {
                return RenderedMetatile(bottom_layer, top_layer);
            }

            &self.secondary.metatiles[index - self.metatile_limit]
        };

        // For each tile in the bottom layer
        for (i, tile) in (&metatile.tiles[0..4]).iter().enumerate() {
            let tile_index = tile.index() as usize;

            let graphics = if tile_index < self.tile_limit {
                if tile_index >= self.primary.graphics.tiles.len() {
                    continue;
                }
                &self.primary.graphics.tiles[tile_index]
            } else {
                if tile_index - self.tile_limit >= self.secondary.graphics.tiles.len() {
                    continue;
                }
                &self.secondary.graphics.tiles[tile_index - self.tile_limit]
            };

            let xoff = (i as u32 % 2) * 8;
            let yoff = (i as u32 / 2) * 8;

            let palette = tile.palette() as usize;

            for (y, row) in graphics.iter().enumerate() {
                for (x, pixel) in row.iter().enumerate() {
                    let x = if tile.hflip() { 7 - x } else { x } as u32;
                    let y = if tile.vflip() { 7 - y } else { y } as u32;

                    bottom_layer[(x + xoff, y + yoff)] = self.palettes[palette][*pixel as usize];
                }
            }
        }

        // For each tile in the top layer
        for (i, tile) in (&metatile.tiles[4..8]).iter().enumerate() {
            let tile_index = tile.index() as usize;

            let graphics = if tile_index < self.tile_limit {
                if tile_index >= self.primary.graphics.tiles.len() {
                    continue;
                }
                &self.primary.graphics.tiles[tile_index]
            } else {
                if tile_index - self.tile_limit >= self.secondary.graphics.tiles.len() {
                    continue;
                }
                &self.secondary.graphics.tiles[tile_index - self.tile_limit]
            };

            let xoff = (i as u32 % 2) * 8;
            let yoff = (i as u32 / 2) * 8;

            let palette = tile.palette() as usize;

            for (y, row) in graphics.iter().enumerate() {
                for (x, pixel) in row.iter().enumerate() {
                    let x = if tile.hflip() { 7 - x } else { x } as u32;
                    let y = if tile.vflip() { 7 - y } else { y } as u32;

                    // If the color is 0, it's transparent
                    if *pixel == 0 {
                        continue;
                    }
                    top_layer[(x + xoff, y + yoff)] = self.palettes[palette][*pixel as usize];
                }
            }
        }

        RenderedMetatile(bottom_layer, top_layer)
    }

    pub fn print(&self, cols: usize, headers: bool) {
        if self.tile_limit % cols != 0 {
            panic!(
                "The number of columns must be a multiple of the maximum number of tiles in the first tileset"
            );
        }

        // Render all metatiles on the row
        let mut count = 0;

        while count < self.metatile_count {
            if headers && count == self.metatile_limit {
                println!(" ---- Secondary tileset start ---- ");
            }

            // Render the next cols metatiles
            let mut rendered = vec![];
            for i in 0..cols {
                let index = count + i;
                if index >= self.metatile_count {
                    break;
                }

                let metatile = self.render_metatile(index);
                rendered.push(metatile);
            }

            for i in 0..rendered.len() {
                let index = count + i;
                if headers {
                    print!("Metatile    ${:<03X} ", index);
                }
            }
            if headers {
                println!();
            }
            for y in (0..16).step_by(2) {
                for rendered in rendered.iter() {
                    self.print_metatile_row(rendered, y);

                    if headers {
                        print!(" ");
                    }
                }
                println!();
            }

            count += cols;
        }
    }

    pub fn print_metatile(&self, index: usize) {
        let metatile = self.render_metatile(index);

        for y in (0..16).step_by(2) {
            self.print_metatile_row(&metatile, y);
            println!();
        }
    }

    /// Helper method for printing a row of a metatile
    /// (useful for printing the whole tileset to terminal)
    fn print_metatile_row(&self, metatile: &RenderedMetatile, y: u32) {
        use colored::Colorize;

        for x in 0..16u32 {
            // Get the color of the upper block half
            let upper = metatile.merge_layers(x, y);
            let lower = metatile.merge_layers(x, y + 1);

            print!(
                "{}",
                "▄"
                    .truecolor(lower[0] as u8, lower[1] as u8, lower[2] as u8)
                    .on_truecolor(upper[0] as u8, upper[1] as u8, upper[2] as u8)
            );
        }
    }
}

impl MapLayout {
    /// Return the tileset pair of the given layout
    pub fn read_tilesets(&self, rom: &Rom) -> Result<TilesetsPair, TilesetReadingError> {
        let tileset_1 = match self.primary_tileset {
            PointedData::Valid(offset, _) => offset,
            _ => return Err(TilesetReadingError::InvalidTilesetOffset),
        } as usize;
        let tileset_2 = match self.secondary_tileset {
            PointedData::Valid(offset, _) => offset,
            _ => return Err(TilesetReadingError::InvalidTilesetOffset),
        } as usize;

        TilesetsPair::new(rom, tileset_1, tileset_2)
    }
}

impl MapLayoutData {
    /// Return the tileset pair of the given layout
    pub fn read_tilesets(&self, rom: &Rom) -> Result<TilesetsPair, TilesetReadingError> {
        self.header.read_tilesets(rom)
    }

    /// Render the map from the given layout
    pub fn render(&self, rendered_tp: &Vec<RenderedMetatile>) -> RgbImage {
        let width = self.header.width as u32;
        let height = self.header.height as u32;

        // Create an image
        let mut image = RgbImage::new(width * 16, height * 16);

        let tile_mask = (1 << self.bits_per_block) - 1;

        // Loop over the map data
        for x in 0..width {
            for y in 0..height {
                let tile_index = self.map_data[y as usize][x as usize] & tile_mask;
                let tile = &rendered_tp[tile_index as usize];
                tile.draw(&mut image, x, y);
            }
        }

        image
    }

    /// Render the map from the given layout and return it as a base64 string
    /// for use in HTML.
    pub fn render_to_base64(&self, rendered_tp: &Vec<RenderedMetatile>) -> String {
        let image = self.render(rendered_tp);
        rgb_image_to_base64(&image)
    }
}

fn convert_pal_to_rgba(pal: GBAPalette) -> RgbaPalette {
    let mut rgba: RgbaPalette = [Rgba::from([0, 0, 0, 0]); 16];

    for (i, color) in pal.iter().enumerate() {
        // Convert the color to RGBA
        let (r, g, b) = color.to_rgb888();
        let a = 255;
        let rgba_color = Rgba::from([r, g, b, a]);

        // Insert the color into the palette
        rgba[i] = rgba_color;
    }

    rgba
}

/// Serializes a [`RgbaImage`] to a base64 string for use in HTML.
pub fn rgba_image_to_base64(image: &RgbaImage) -> String {
    // Create a buffer that looks like a file (a cursor over a vector)
    let mut buffer = Cursor::new(Vec::new());
    // Write the image as png to that buffer
    image.write_to(&mut buffer, ImageFormat::Png).unwrap();
    // Encode the buffer as base64
    let b64 = general_purpose::STANDARD.encode(&buffer.into_inner());

    // Return the base64 string with the header
    format!("data:image/png;base64,{}", b64)
}

/// Serializes a [`RgbImage`] to a base64 string for use in HTML.
pub fn rgb_image_to_base64(image: &RgbImage) -> String {
    // Same as above
    let mut buffer = Cursor::new(Vec::new());
    image.write_to(&mut buffer, ImageFormat::Png).unwrap();
    let b64 = general_purpose::STANDARD.encode(&buffer.into_inner());
    format!("data:image/png;base64,{}", b64)
}
