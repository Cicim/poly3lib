static FLIP_CHARS: [char; 4] = [' ', '←', '↑', '𝥮'];

use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use crate::{
    types::{RomReadableType, RomSizedType, RomWritableType},
    Offset, RomData, RomIoError,
};

/// Information for rendering a single tile in a tile map.
///
/// Contains the tile index, palette index, and flip information.
///
/// `tttt tttt ttpp ppHV`
#[derive(Copy, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct RomTileMapEntry(u16);

impl RomTileMapEntry {
    /// Creates a new [`Tile`].
    pub fn new(index: u16, palette: u16, hflip: bool, vflip: bool) -> RomTileMapEntry {
        let mut tile = index & 0x3FF;
        tile |= (palette & 15) << 12;
        tile |= (hflip as u16) << 10;
        tile |= (vflip as u16) << 11;
        RomTileMapEntry(tile)
    }

    /// Returns the tile index.
    pub fn index(&self) -> u16 {
        self.0 & 0x3FF
    }

    /// Returns the palette index.
    pub fn palette(&self) -> u8 {
        ((self.0 >> 12) & 0xF) as u8
    }

    /// Returns whether the tile is horizontally flipped.
    pub fn hflip(&self) -> bool {
        self.0 & 0x400 != 0
    }

    /// Returns whether the tile is vertically flipped.
    pub fn vflip(&self) -> bool {
        self.0 & 0x800 != 0
    }
}

impl From<u16> for RomTileMapEntry {
    fn from(tile: u16) -> RomTileMapEntry {
        RomTileMapEntry(tile)
    }
}

impl Into<u16> for RomTileMapEntry {
    fn into(self) -> u16 {
        self.0
    }
}

impl Debug for RomTileMapEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;

        let tile = format!("T{:03X}", self.index()).cyan().on_black();
        let flip_char = FLIP_CHARS[2 * self.hflip() as usize + self.vflip() as usize];

        let flip = format!("{}", flip_char).white().on_black();
        let palette = format!("p{:02}", self.palette()).yellow().on_black();

        write!(f, "{}{}{:>2}", tile, flip, palette)
    }
}

impl RomSizedType for RomTileMapEntry {
    fn get_size(_: &RomData) -> usize {
        2
    }
    fn get_alignment(_: &RomData) -> usize {
        2
    }
}
impl RomReadableType for RomTileMapEntry {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        Ok(RomTileMapEntry(rom.read_halfword(offset)?))
    }
}
impl RomWritableType for RomTileMapEntry {
    fn write_to(self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        rom.write_halfword(offset, self.0)
    }
}
