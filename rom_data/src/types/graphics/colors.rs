use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use crate::{
    types::{RomReadableType, RomSizedType, RomWritableType},
    Offset, RomData, RomIoError,
};

/// A color in the GBA's 15-bit BGR color format (`0bbbbbgggggrrrrr`).
#[derive(Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct RomColor(u16);

impl RomColor {
    /// Creates a new [`GBAColor`].
    pub fn new(r: u8, g: u8, b: u8) -> RomColor {
        <[u8]>::into_vec(std::boxed::Box::new([1, 2]));
        RomColor((b as u16) << 10 | (g as u16) << 5 | r as u16)
    }

    /// Returns the red component of the color.
    pub fn r(&self) -> u8 {
        (self.0 & 0x1F) as u8
    }

    /// Returns the green component of the color.
    pub fn g(&self) -> u8 {
        ((self.0 >> 5) & 0x1F) as u8
    }

    /// Returns the blue component of the color.
    pub fn b(&self) -> u8 {
        ((self.0 >> 10) & 0x1F) as u8
    }

    /// Returns the color as a tuple of (red, green, blue).
    pub fn to_rgb(&self) -> (u8, u8, u8) {
        (self.r(), self.g(), self.b())
    }

    // Converts the color to a RGB888 (24 bit) color.
    pub fn to_rgb888(&self) -> (u8, u8, u8) {
        let r = self.r() << 3;
        let g = self.g() << 3;
        let b = self.b() << 3;
        (r, g, b)
    }

    /// Returns whether this color can be considered valid.
    pub fn is_valid(&self) -> bool {
        self.0 & 0x8000 == 0
    }
}

impl Debug for RomColor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        let hex = format!("{:04X}", self.0);
        let (r, g, b) = self.to_rgb888();
        let hex_string = format!("{}", hex);

        // If the first bit is set, the color is most likely invalid
        if !self.is_valid() {
            write!(f, "{}", hex_string.strikethrough().red())
        }
        // Else decide whether to draw the text with a black or white foreground
        else if r as u16 + g as u16 + b as u16 > 0x80 {
            write!(f, "{}", hex_string.black().on_truecolor(r, g, b))
        } else {
            write!(f, "{}", hex_string.white().on_truecolor(r, g, b))
        }
    }
}

impl Into<u16> for RomColor {
    fn into(self) -> u16 {
        self.0
    }
}

impl From<u16> for RomColor {
    fn from(color: u16) -> RomColor {
        RomColor(color)
    }
}

impl RomSizedType for RomColor {
    fn get_size(_: &RomData) -> usize {
        2
    }
    fn get_alignment(_: &RomData) -> usize {
        2
    }
}
impl RomReadableType for RomColor {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        Ok(RomColor(rom.read_halfword(offset)?))
    }
}
impl RomWritableType for RomColor {
    fn write_to(self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        rom.write_halfword(offset, self.0)
    }
}

/// A palette of 16 colors.
///
/// Palettes are stored in the ROM as 32-byte blocks, where each color is stored
/// as a 16-bit value in the GBA's 15-bit BGR color format.
#[derive(Default, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct RomPalette([RomColor; 16]);

impl Debug for RomPalette {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ ")?;
        for color in self.0.iter() {
            write!(f, "{:?} ", color)?;
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl RomPalette {
    /// Creates a new [`Palette`] from the given colors.
    pub fn new(colors: [RomColor; 16]) -> RomPalette {
        RomPalette(colors)
    }

    /// Returns the color at the given index.
    pub fn get(&self, index: usize) -> RomColor {
        self.0[index]
    }

    /// Sets the color at the given index.
    pub fn set(&mut self, index: usize, color: RomColor) {
        self.0[index] = color;
    }

    /// Returns an iterator over the colors in the palette.
    pub fn iter(&self) -> impl Iterator<Item = RomColor> + '_ {
        self.0.iter().copied()
    }
}

impl RomSizedType for RomPalette {
    fn get_size(_: &RomData) -> usize {
        32
    }
    fn get_alignment(_: &RomData) -> usize {
        32
    }
}
impl RomReadableType for RomPalette {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        let mut colors = [RomColor::default(); 16];
        for i in 0..16 {
            colors[i] = RomColor::from(rom.read_halfword(offset + i * 2)?);
        }
        Ok(RomPalette(colors))
    }
}
impl RomWritableType for RomPalette {
    fn write_to(self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        for i in 0..16 {
            rom.write_halfword(offset + i * 2, self.0[i].into())?;
        }
        Ok(())
    }
}
