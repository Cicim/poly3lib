use std::fmt::{self, Display, Formatter};

use crate::rom::{OutOfBoundsError, Rom};

/// A color in the GBA's 15-bit BGR color format.
///
/// The GBA's color format is 15-bit BGR, so each color is represented by 5 bits.
/// + Bits 0-4: red
/// + Bits 5-9: green
/// + Bits 10-14: blue
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct GBAColor(u16);

impl GBAColor {
    /// Creates a new [`GBAColor`].
    pub fn new(r: u8, g: u8, b: u8) -> GBAColor {
        GBAColor((b as u16) << 10 | (g as u16) << 5 | r as u16)
    }

    /// Returns the red component of the color.
    pub fn r(&self) -> u8 {
        (self.0 & 0x1F) as u8
    }

    /// Returns the green component of the color.
    pub fn g(&self) -> u8 {
        ((self.0 >> 5) & 0x1F) as u8
    }

    /// Returns the green component of the color.
    pub fn b(&self) -> u8 {
        ((self.0 >> 10) & 0x1F) as u8
    }

    /// Returns the color as a tuple of (red, green, blue).
    pub fn rgb(&self) -> (u8, u8, u8) {
        (self.r(), self.g(), self.b())
    }

    // Converts the color to a RGB888 color.
    pub fn to_rgb888(&self) -> (u8, u8, u8) {
        let r = self.r() << 3;
        let g = self.g() << 3;
        let b = self.b() << 3;
        (r, g, b)
    }
}

impl Display for GBAColor {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use colored::Colorize;

        let hex = format!("{:04X}", self.0);
        let (r, g, b) = self.to_rgb888();
        let hex_string = format!("{}", hex);
        let mut bg_only = hex_string.on_truecolor(r, g, b);

        if self.0 & 0x8000 != 0 {
            bg_only = bg_only.strikethrough()
        }

        // Determine the color of the text
        if r as u16 + g as u16 + b as u16 > 0x80 {
            write!(f, "{}", bg_only.black())
        } else {
            write!(f, "{}", bg_only.white())
        }
    }
}

impl Into<u16> for GBAColor {
    fn into(self) -> u16 {
        self.0
    }
}

impl From<u16> for GBAColor {
    fn from(color: u16) -> GBAColor {
        GBAColor(color)
    }
}

impl Rom {
    /// Reads a color from the ROM at the given offset.
    pub fn read_color(&self, offset: usize) -> Result<GBAColor, OutOfBoundsError> {
        let color_bytes = self.read_u16(offset)?;
        Ok(GBAColor(color_bytes))
    }

    /// Writes a color to the ROM at the given offset.
    pub fn write_color(&mut self, offset: usize, color: GBAColor) -> Result<(), OutOfBoundsError> {
        self.write_u16(offset, color.into())
    }
}

/// A palette of 16 colors.
///
/// Palettes are stored in the ROM as 32-byte blocks, where each color is stored
/// as a 16-bit value in the GBA's 15-bit BGR color format.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GBAPalette {
    colors: [GBAColor; 16],
}

impl Display for GBAPalette {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Palette: ")?;

        for color in self.colors.iter() {
            write!(f, "{} ", color)?;
        }
        
        Ok(())
    }
}

impl GBAPalette {
    /// Creates a new [`Palette`] from the given colors.
    pub fn new(colors: [GBAColor; 16]) -> GBAPalette {
        GBAPalette { colors }
    }

    /// Returns the color at the given index.
    pub fn get(&self, index: usize) -> GBAColor {
        self.colors[index]
    }

    /// Sets the color at the given index.
    pub fn set(&mut self, index: usize, color: GBAColor) {
        self.colors[index] = color;
    }

    /// Returns an iterator over the colors in the palette.
    pub fn iter(&self) -> impl Iterator<Item = GBAColor> + '_ {
        self.colors.iter().copied()
    }
}

impl Rom {
    /// Read a palette from the ROM at the given offset.
    pub fn read_palette(&self, offset: usize) -> Result<GBAPalette, OutOfBoundsError> {
        let palette_chunk = self.read(offset, 32)?;

        let mut colors = [GBAColor(0); 16];
        for (i, c) in palette_chunk.chunks_exact(2).enumerate() {
            let color = GBAColor(u16::from_le_bytes([c[0], c[1]]));
            colors[i] = color;
        }

        Ok(GBAPalette::new(colors))
    }

    /// Writes a palette to the ROM at the given offset.
    pub fn write_palette(&mut self, offset: usize, palette: GBAPalette) -> Result<(), OutOfBoundsError> {
        let mut palette_chunk = [0; 32];
        for (i, color) in palette.iter().enumerate() {
            let bytes = color.0.to_le_bytes();
            palette_chunk[i * 2] = bytes[0];
            palette_chunk[i * 2 + 1] = bytes[1];
        }

        self.write(offset, &palette_chunk)
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_color() {
        let color = GBAColor::new(0x1F, 0x1F, 0x1F);
        assert_eq!(color.r(), 0x1F);
        assert_eq!(color.g(), 0x1F);
        assert_eq!(color.b(), 0x1F);
        assert_eq!(color.rgb(), (0x1F, 0x1F, 0x1F));
    }

    #[test]
    fn test_palette() {
        let mut palette = GBAPalette::new([GBAColor::new(0x1F, 0x1F, 0x1F); 16]);
        assert_eq!(palette.get(0), GBAColor::new(0x1F, 0x1F, 0x1F));
        palette.set(0, GBAColor::new(0x00, 0x00, 0x00));
        assert_eq!(palette.get(0), GBAColor::new(0x00, 0x00, 0x00));
    }

    #[test]
    fn test_read_palette() {
        let rom = Rom::new(vec![
            0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f,
            0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f,
            0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f,
            0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f,
        ]);
        let palette = GBAPalette::new([GBAColor::new(0x1F, 0x1F, 0x1F); 16]);
        assert_eq!(rom.read_palette(0).unwrap(), palette);
    }

    #[test]
    fn test_palette_read_write() {
        let mut rom = Rom::new(vec![0; 32]);
        let palette = GBAPalette::new([GBAColor::new(0x1F, 0x1F, 0x1F); 16]);
        rom.write_palette(0, palette.clone()).unwrap();
        assert_eq!(rom.read_palette(0).unwrap(), palette);
    }
}