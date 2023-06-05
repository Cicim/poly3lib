use std::fmt::Display;

use crate::rom::*;

/// Data for a single 8x8 tile in a map.
///
/// The tile format consists of a 16-bit value containing:
/// + Bits 0-9: tile index
/// + Bit 10-13: palette index
/// + Bit 14: horizontal flip
/// + Bit 15: vertical flip
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Tile(u16);

impl Tile {
    /// Creates a new [`Tile`].
    pub fn new(index: u16, palette: u16, hflip: bool, vflip: bool) -> Tile {
        let mut tile = index & 0x3FF;
        tile |= (palette & 0xF) << 12;
        tile |= (hflip as u16) << 10;
        tile |= (vflip as u16) << 11;
        Tile(tile)
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

impl From<u16> for Tile {
    fn from(tile: u16) -> Tile {
        Tile(tile)
    }
}

impl Into<u16> for Tile {
    fn into(self) -> u16 {
        self.0
    }
}

const FLIP_STRINGS: [&str; 4] = ["□", "◧", "⬒", "◩"];

impl Display for Tile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;
        let tile = format!("T{:03X}", self.index()).cyan().on_black();
        let flip = FLIP_STRINGS[2 * self.hflip() as usize + self.vflip() as usize]
            .white()
            .on_black();
        let palette = format!("p{:02}", self.palette()).yellow().on_black();

        write!(f, "{}{}{:>2}", tile, flip, palette)
    }
}

impl Rom {
    /// Reads a tile from the ROM at the given offset.
    pub fn read_tile(&self, offset: usize) -> Result<Tile, OutOfBoundsError> {
        let tile_bytes = self.read_u16(offset)?;
        Ok(Tile(tile_bytes))
    }

    /// Writes a tile to the ROM at the given offset.
    pub fn write_tile(&mut self, offset: usize, tile: Tile) -> Result<(), OutOfBoundsError> {
        self.write_u16(offset, tile.into())
    }
}

pub struct MetaTile {
    pub tiles: [Tile; 8],
}

impl MetaTile {
    pub fn new(tiles: [Tile; 8]) -> MetaTile {
        MetaTile { tiles }
    }
}

impl Display for MetaTile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MetaTile[")?;
        for (i, tile) in self.tiles.iter().enumerate() {
            write!(f, "{}", tile)?;
            if i != 7 {
                write!(f, " ")?;
            }
        }
        write!(f, "]")
    }
}

impl Rom {
    /// Reads a metatile from the ROM at the given offset.
    pub fn read_metatile(&self, offset: usize) -> Result<MetaTile, OutOfBoundsError> {
        let mut tiles = [Tile(0); 8];
        for i in 0..8 {
            tiles[i] = self.read_tile(offset + 2 * i)?;
        }
        Ok(MetaTile::new(tiles))
    }

    /// Writes a metatile to the ROM at the given offset.
    pub fn write_metatile(
        &mut self,
        offset: usize,
        metatile: MetaTile,
    ) -> Result<(), OutOfBoundsError> {
        for (i, tile) in metatile.tiles.iter().enumerate() {
            self.write_tile(offset + 2 * i, *tile)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_tile() {
        let tile = Tile::new(0x3F1, 0xA, false, true);
        assert_eq!(tile.index(), 0x3F1);
        assert_eq!(tile.palette(), 0xA);
        assert_eq!(tile.hflip(), false);
        assert_eq!(tile.vflip(), true);
    }

    #[test]
    fn new_metatile() {
        let tiles = [
            Tile::new(0x000, 0xA, true, false),
            Tile::new(0x000, 0xF, true, true),
            Tile::new(0x000, 0x8, false, true),
            Tile::new(0x000, 0xA, true, false),
            Tile::new(0x000, 0x8, false, true),
            Tile::new(0x000, 0x3, false, false),
            Tile::new(0x000, 0x3, false, false),
            Tile::new(0x000, 0xF, true, true),
        ];

        let meta_tile = MetaTile::new(tiles);
        assert_eq!(meta_tile.tiles[0].index(), 0x000);
        assert_eq!(meta_tile.tiles[0].palette(), 0xA);
        assert_eq!(meta_tile.tiles[0].hflip(), true);
        assert_eq!(meta_tile.tiles[0].vflip(), false);
        assert_eq!(meta_tile.tiles[1].index(), 0x000);
        assert_eq!(meta_tile.tiles[1].palette(), 0xF);
        assert_eq!(meta_tile.tiles[1].hflip(), true);
        assert_eq!(meta_tile.tiles[1].vflip(), true);
        assert_eq!(meta_tile.tiles[2].index(), 0x000);
        assert_eq!(meta_tile.tiles[2].palette(), 0x8);
        assert_eq!(meta_tile.tiles[2].hflip(), false);
        assert_eq!(meta_tile.tiles[2].vflip(), true);
        assert_eq!(meta_tile.tiles[3].index(), 0x000);
        assert_eq!(meta_tile.tiles[3].palette(), 0xA);
        assert_eq!(meta_tile.tiles[3].hflip(), true);
        assert_eq!(meta_tile.tiles[3].vflip(), false);
        assert_eq!(meta_tile.tiles[4].index(), 0x000);
        assert_eq!(meta_tile.tiles[4].palette(), 0x8);
        assert_eq!(meta_tile.tiles[4].hflip(), false);
        assert_eq!(meta_tile.tiles[4].vflip(), true);
        assert_eq!(meta_tile.tiles[5].index(), 0x000);
        assert_eq!(meta_tile.tiles[5].palette(), 0x3);
        assert_eq!(meta_tile.tiles[5].hflip(), false);
        assert_eq!(meta_tile.tiles[5].vflip(), false);
        assert_eq!(meta_tile.tiles[6].index(), 0x000);
        assert_eq!(meta_tile.tiles[6].palette(), 0x3);
        assert_eq!(meta_tile.tiles[6].hflip(), false);
        assert_eq!(meta_tile.tiles[6].vflip(), false);
        assert_eq!(meta_tile.tiles[7].index(), 0x000);
        assert_eq!(meta_tile.tiles[7].palette(), 0xF);
        assert_eq!(meta_tile.tiles[7].hflip(), true);
        assert_eq!(meta_tile.tiles[7].vflip(), true);
    }

    #[test]
    fn read_tile() {
        let rom = Rom::new(vec![0x4f, 0x3f, 0x1a, 0x00]);
        let tile = rom.read_tile(0).unwrap();
        assert_eq!(tile.index(), 0x34f);
        assert_eq!(tile.palette(), 0x3);
        assert_eq!(tile.hflip(), true);
        assert_eq!(tile.vflip(), true);

        let tile = rom.read_tile(2).unwrap();
        assert_eq!(tile.index(), 0x1A);
        assert_eq!(tile.palette(), 0x0);
        assert_eq!(tile.hflip(), false);
        assert_eq!(tile.vflip(), false);
    }

    #[test]
    fn write_tile() {
        let mut rom = Rom::new(vec![0x00, 0x00, 0x00, 0x00]);
        let tile = Tile::new(0x34f, 0x3, false, true);
        rom.write_tile(0, tile).unwrap();
        assert_eq!(rom.data[0], 0x4F);
        assert_eq!(rom.data[1], 0x3B);
    }
}
