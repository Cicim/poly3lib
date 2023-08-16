use gba_types::{colors::GBAPalette, GBAIOError};
use image::RgbaImage;
use serde::{Deserialize, Serialize};

use crate::rom::Rom;

pub type GraphicTile = [[u8; 8]; 8];

#[derive(Debug, Serialize, Deserialize)]
pub struct Graphic {
    /// The offset of the graphic in the ROM
    pub offset: usize,
    /// The tiles in the graphic (empty if you don't want to replace them)
    pub tiles: Vec<GraphicTile>,
    /// Whether the tileset is compressed or not
    pub compressed: bool,
    /// The number of tiles read when reading this from ROM.
    pub read_length: usize,
    /// Whether to replace the tileset
    pub replace: bool,
}

impl Graphic {
    /// Reads a graphic from the ROM.
    ///
    /// If the size is not specified, it is assumed that the graphic is compressed,
    /// otherwise the given number of tiles is read.
    pub fn read(rom: &Rom, offset: usize, size: Option<usize>) -> Result<Graphic, GBAIOError> {
        let tiles = match size {
            Some(num_tiles) => {
                // Read the graphic
                rom.data[offset..offset + num_tiles * 32]
                    .chunks_exact(32)
                    .map(read_tile)
                    .collect::<Vec<GraphicTile>>()
            }
            None => {
                // Read the Lz77-compressed graphic
                rom.read_lz77(offset)?
                    .chunks_exact(32)
                    .map(read_tile)
                    .collect::<Vec<GraphicTile>>()
            }
        };

        Ok(Graphic {
            offset,
            read_length: tiles.len(),
            tiles,
            compressed: size.is_none(),
            replace: false,
        })
    }

    /// Writes the graphic to the ROM.
    ///
    /// It is written differently depending on whether the graphic is compressed or not.
    /// It only considers the length specified in changed when repointing uncompressed graphics.
    pub fn write(&self, rom: &mut Rom) -> Result<usize, GBAIOError> {
        let offset = self.offset;

        // If the graphic did not change, only write the pointer
        if !self.replace {
            return Ok(offset);
        }

        let tiles = &self.tiles;

        // Check if the graphics is compressed
        if self.compressed {
            // Compose the data
            let mut data = vec![0; tiles.len() * 32];
            for (i, tile) in tiles.iter().enumerate() {
                write_tile(tile, &mut data, i * 32);
            }
            rom.replace_lz77_data(offset, &data[..])
        } else {
            // Repoint if needed
            let old_size = self.read_length;
            let new_size = tiles.len();
            let new_offset = rom
                .repoint_offset(offset, old_size, new_size)
                .ok_or_else(|| GBAIOError::RepointingError)?;

            // Write the graphic
            for (i, tile) in tiles.iter().enumerate() {
                write_tile(tile, &mut rom.data, new_offset + i * 32);
            }
            Ok(new_offset)
        }
    }

    /// Draw the graphic onto an Rgba image with the given size.
    pub fn draw(&self, img: &mut RgbaImage, palette: &GBAPalette, hflip: bool, vflip: bool) {
        let tiles = &self.tiles;
        let tile_width = img.width() as usize / 8;
        let tile_height = img.height() as usize / 8;

        for (i, tile) in tiles.iter().enumerate() {
            let mut tx = i % tile_width;
            let mut ty = i / tile_width;

            if hflip {
                tx = tile_width - tx - 1;
            }
            if vflip {
                ty = tile_height - ty - 1;
            }

            for mut y in 0..8 {
                for mut x in 0..8 {
                    let color = tile[y][x] as usize;

                    // Consider transparency
                    if color == 0 {
                        continue;
                    }

                    let (r, g, b) = palette.get(color).to_rgb888();

                    if hflip {
                        x = 7 - x;
                    }
                    if vflip {
                        y = 7 - y;
                    }
                    img.put_pixel(
                        (tx * 8 + x) as u32,
                        (ty * 8 + y) as u32,
                        image::Rgba([r, g, b, 255]),
                    );
                }
            }
        }
    }

    pub fn print_large(&self, col: usize, headers: bool, palette: &GBAPalette) {
        use colored::Colorize;

        let tiles = &self.tiles;
        let mut index = 0;

        // Print the headers
        while index < tiles.len() {
            let last_index = std::cmp::min(index + col, tiles.len());

            for index in index..last_index {
                if headers {
                    let header = format!("Tile {:<4} [${:03X}] ", index, index);
                    print!("{}", header.truecolor(128, 128, 128));
                }
            }
            println!();

            for y in 0..8 {
                for index in index..last_index {
                    for x in 0..8 {
                        let color = tiles[index][y][x] as usize;
                        let string = format!(" {:X}", color);
                        let (r, g, b) = palette.get(color).to_rgb888();
                        let string = string.on_truecolor(r, g, b);
                        print!("{}", string);
                    }
                    print!(" ");
                }
                if y != 7 {
                    println!();
                }
            }

            println!();
            index += col;
        }
    }

    pub fn print_small(&self, col: usize, headers: bool, palette: &GBAPalette) {
        use colored::Colorize;

        let tiles = &self.tiles;
        let mut index = 0;

        // Print the headers
        while index < tiles.len() {
            let last_index = std::cmp::min(index + col, tiles.len());

            for index in index..last_index {
                if headers {
                    let header = format!("Tile {:<03X} ", index);
                    print!("{}", header.truecolor(128, 128, 128));
                }
            }
            println!();

            for y in 0..4 {
                for index in index..last_index {
                    for x in 0..8 {
                        let color1 = tiles[index][2 * y][x] as usize;
                        let color2 = tiles[index][2 * y + 1][x] as usize;
                        let (r, g, b) = palette.get(color1).to_rgb888();
                        let string = "▀".truecolor(r, g, b);
                        let (r, g, b) = palette.get(color2).to_rgb888();
                        let string = string.on_truecolor(r, g, b);
                        print!("{}", string);
                    }
                    if headers {
                        print!(" ");
                    }
                }
                if y != 3 {
                    println!();
                }
            }

            if headers {
                println!();
            }
            index += col;
        }
    }
}

impl std::fmt::Display for Graphic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:#X} ({} tiles, read {})",
            self.offset,
            self.tiles.len(),
            self.read_length
        )
    }
}

fn read_tile(chunk: &[u8]) -> GraphicTile {
    let mut tile = [[0; 8]; 8];
    for y in 0..8 {
        for x in 0..8 {
            let color = chunk[y * 4 + x / 2];
            tile[y][x] = if x % 2 == 0 { color & 0x0F } else { color >> 4 };
        }
    }
    tile
}

fn write_tile(tile: &GraphicTile, bytes: &mut [u8], offset: usize) {
    for y in 0..8 {
        for x in 0..8 {
            let color = tile[y][x];
            if x % 2 == 0 {
                bytes[offset + y * 4 + x / 2] |= color;
            } else {
                bytes[offset + y * 4 + x / 2] |= color << 4;
            }
        }
    }
}
