use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use crate::{
    types::{RomReadableType, RomSizedType, RomWritableType},
    Offset, RomData,
};

/// A tile is a 8x8 pixel square that is used to draw the background and sprites.
///
/// It is composed of 8 rows of 8 pixels each, where each pixel is a color index.
#[derive(Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct RomTile(
    /// The 8 rows of 8 pixels each.
    [[PaletteIndex; 8]; 8],
);

/// An index to a color in a palette (4 bits).
///
/// The value `0` is always transparent.
type PaletteIndex = u8;

impl RomTile {
    /// Returns the color index of the pixel at `(x, y)`.
    #[inline]
    pub fn get_pixel(&self, x: usize, y: usize) -> PaletteIndex {
        self.0[y][x]
    }

    /// Sets the color index of the pixel at `(x, y)`.
    #[inline]
    pub fn set_pixel(&mut self, x: usize, y: usize, value: PaletteIndex) {
        self.0[y][x] = value;
    }

    /// Reads a tile from a packed 32-byte array.
    pub fn read_packed(bytes: &[u8; 32]) -> RomTile {
        let mut tile = RomTile([[0; 8]; 8]);

        for y in 0..8 {
            for x in 0..4 {
                let byte = bytes[y * 4 + x];
                tile.set_pixel(x * 2, y, byte & 0xf);
                tile.set_pixel(x * 2 + 1, y, byte >> 4);
            }
        }

        tile
    }

    /// Writes a tile to a packed 32-byte array.
    pub fn write_packed(&self, bytes: &mut [u8; 32]) {
        for y in 0..8 {
            for x in 0..4 {
                let byte = self.get_pixel(x * 2, y) | (self.get_pixel(x * 2 + 1, y) << 4);
                bytes[y * 4 + x] = byte;
            }
        }
    }
}

impl Debug for RomTile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        for y in 0..8 {
            for x in 0..8 {
                let pal_idx = self.get_pixel(x, y);
                if pal_idx == 0 {
                    write!(f, " •")?;
                    continue;
                }

                // Get the color to show
                let r = 3 * pal_idx;
                let g = 2 * pal_idx;
                let b = 8 * pal_idx + 24;
                // Get the string to format
                let hex = format!(" {:01X}", pal_idx).on_truecolor(r, g, b);

                if pal_idx > 10 {
                    write!(f, "{}", hex.black())?;
                } else {
                    write!(f, "{}", hex.white())?;
                }
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

impl RomSizedType for RomTile {
    fn get_size(_: &RomData) -> usize {
        32
    }
    fn get_alignment(_: &RomData) -> usize {
        32
    }
}
impl RomReadableType for RomTile {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, crate::RomIoError> {
        let bytes: &[u8] = rom.read_slice(offset, 32)?;
        // SAFETY: The slice is exactly 32 bytes long.
        Ok(RomTile::read_packed(bytes.try_into().unwrap()))
    }
}
impl RomWritableType for RomTile {
    fn write_to(self, rom: &mut RomData, offset: Offset) -> Result<(), crate::RomIoError> {
        let mut bytes = [0; 32];
        self.write_packed(&mut bytes);
        rom.write_slice(offset, &bytes)
    }
}
