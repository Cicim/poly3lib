use gba_macro::gba_struct;
use gba_types::{GBAIOError, GBAType};
use serde::Serialize;

use crate::rom::{Rom, RomType};

const SQUARE: u16 = 0;
const H_RECTANGLE: u16 = 1;
const V_RECTANGLE: u16 = 2;

gba_struct!(RubySapphireSubsprite {
    u16 x;
    u16 y;
    u16 shape:2;
    u16 size:2;
    u16 tile_offset:10;
    u16 priority:2;
} PRIVATE);
gba_struct!(Subsprite {
    i8 x;
    i8 y;
    u16 shape:2;
    u16 size:2;
    u16 tile_offset:10;
    u16 priority:2;
});
impl Into<Subsprite> for RubySapphireSubsprite {
    fn into(self) -> Subsprite {
        Subsprite {
            x: self.x as u8 as i8,
            y: self.y as u8 as i8,
            shape: self.shape,
            size: self.size,
            tile_offset: self.tile_offset,
            priority: self.priority,
        }
    }
}

impl Subsprite {
    /// Reads a subsprite from the given ROM.
    pub fn read(rom: &Rom, offset: usize) -> Result<Self, GBAIOError> {
        Ok(match rom.rom_type {
            RomType::Ruby | RomType::Sapphire => rom.read::<RubySapphireSubsprite>(offset)?.into(),
            _ => rom.read::<Subsprite>(offset)?,
        })
    }

    /// Returns the width and height of this subsprite in pixels
    pub fn dimensions(&self) -> (u8, u8) {
        get_shape(self.size, self.shape)
    }

    fn size(rom: &Rom) -> usize {
        match rom.rom_type {
            RomType::Ruby | RomType::Sapphire => RubySapphireSubsprite::SIZE,
            _ => Subsprite::SIZE,
        }
    }
}

#[derive(Debug, Serialize)]
/// Object events define their subsprites using one of the predefined subsprites table
/// `gObjectEventSpriteOamTables_AxB`. These are arrays of 6 `SubspriteTable` structs,
/// which have the following shape.
///
/// ```c
/// struct SubspriteTable {
///    u8 subspriteCount;
///    const struct Subsprite *subsprites;
/// };
/// ```
///
/// This object models the `gObjectEventSpriteOamTables_AxB` tables.
pub struct SubspriteTables([Vec<Subsprite>; 6]);

impl SubspriteTables {
    pub fn read(rom: &Rom, mut offset: usize) -> Result<Self, GBAIOError> {
        let mut tables = [vec![], vec![], vec![], vec![], vec![], vec![]];

        for table in tables.iter_mut() {
            let count = rom.read::<u8>(offset)? as usize;
            // Skip empty tables
            if count == 0 {
                offset += 8;
                continue;
            }

            let mut table_ptr = rom.read_ptr(offset + 4)?;

            for _ in 0..count {
                // Read the value pointed by the offset
                let subsprite = Subsprite::read(rom, table_ptr)?;
                table.push(subsprite);

                // Advance the pointer
                table_ptr += Subsprite::size(rom);
            }
            offset += 8;
        }

        Ok(SubspriteTables(tables))
    }
}

impl std::fmt::Display for SubspriteTables {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, table) in self.0.iter().enumerate() {
            if table.is_empty() {
                writeln!(f, "Table {} is empty", i)?;
                continue;
            } else {
                writeln!(f, "{:^54}", format!("Table {}", i))?;
            }

            // Compose it as a table
            writeln!(f, "┏━━━━━┳━━━━━┳━━━━━━━━━━┳━━━━━━━━━━━━━┳━━━━━━━━━━┓")?;
            writeln!(f, "┃  x  ┃  y  ┃   Size   ┃ Tile Offset ┃ Priority ┃")?;
            writeln!(f, "┡━━━━━╇━━━━━╇━━━━━━━━━━╇━━━━━━━━━━━━━╇━━━━━━━━━━┩")?;
            for subsprite in table.iter() {
                let (width, height) = get_shape(subsprite.size, subsprite.shape);
                writeln!(
                    f,
                    "│{:>4} │{:>4} │ {:>8} │ {:>11} │ {:>8} │",
                    subsprite.x,
                    subsprite.y,
                    format!("{}x{}", width, height),
                    subsprite.tile_offset,
                    subsprite.priority
                )?;
            }
            writeln!(f, "└─────┴─────┴──────────┴─────────────┴──────────┘")?;
        }

        Ok(())
    }
}

gba_struct!(OamDataForObjectEvents
{
    u32 _1:14;
    u32 shape:2;
    u32 _2:14;
    u32 size:2;
});

/// Returns the width and height of a subsprite given its size and shape.
pub fn get_shape(size: u16, shape: u16) -> (u8, u8) {
    match (size, shape) {
        (0, SQUARE) => (8, 8),
        (1, SQUARE) => (16, 16),
        (2, SQUARE) => (32, 32),
        (3, SQUARE) => (64, 64),

        (0, H_RECTANGLE) => (16, 8),
        (1, H_RECTANGLE) => (32, 8),
        (2, H_RECTANGLE) => (32, 16),
        (3, H_RECTANGLE) => (64, 32),

        (0, V_RECTANGLE) => (8, 16),
        (1, V_RECTANGLE) => (8, 32),
        (2, V_RECTANGLE) => (16, 32),
        (3, V_RECTANGLE) => (32, 64),

        _ => unreachable!(),
    }
}
