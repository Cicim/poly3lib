use std::fmt::{self, Formatter};

use crate::{GBAIOError, GBAType};

/// A color in the GBA's 15-bit BGR color format.
///
/// The GBA's color format is 15-bit BGR, so each color is represented by 5 bits.
/// + Bits 0-4: red
/// + Bits 5-9: green
/// + Bits 10-14: blue
#[derive(Default, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

impl fmt::Debug for GBAColor {
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

impl GBAType for GBAColor {
    const SIZE: usize = 2;

    fn read_from(bytes: &[u8], offset: usize) -> Result<Self, GBAIOError> {
        let number = u16::read_from(bytes, offset)?;
        Ok(GBAColor(number))
    }

    fn write_to(&self, bytes: &mut [u8], offset: usize) -> Result<(), GBAIOError> {
        self.0.write_to(bytes, offset)
    }
}

/// A palette of 16 colors.
///
/// Palettes are stored in the ROM as 32-byte blocks, where each color is stored
/// as a 16-bit value in the GBA's 15-bit BGR color format.
#[derive(Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct GBAPalette {
    colors: [GBAColor; 16],
}

impl fmt::Debug for GBAPalette {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Palette: [ ")?;
        for color in self.colors.iter() {
            write!(f, "{:?} ", color)?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl Default for GBAPalette {
    fn default() -> Self {
        GBAPalette {
            colors: [GBAColor::default(); 16],
        }
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

impl GBAType for GBAPalette {
    const SIZE: usize = 32;

    fn read_from(bytes: &[u8], offset: usize) -> Result<Self, GBAIOError> {
        let array = <[GBAColor; 16]>::read_from(bytes, offset)?;
        Ok(GBAPalette::new(array))
    }

    fn write_to(&self, bytes: &mut [u8], offset: usize) -> Result<(), GBAIOError> {
        self.colors.write_to(bytes, offset)
    }
}
