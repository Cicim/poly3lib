use std::fmt::Display;

use crate::rom::{Rom, RomType};

#[derive(Debug)]
pub enum ValueGrabError {
    NotImplemented,
    InvalidOffset,
}

impl Rom {
    /// Get the maximum block size for a primary and a secondary tileset
    pub fn get_metatiles_count(&self) -> Result<(usize, usize), ValueGrabError> {
        // ANCHOR Add support for other rom types
        // TODO This is wrong, read it from the correct place
        let (primary, secondary) = match self.rom_type {
            RomType::FireRed | RomType::LeafGreen => (640, 384),
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => (512, 512),
            // _ => return Err(ValueGrabError::NotImplemented),
        };

        Ok((primary, secondary))
    }

    /// Get the maximum tile count for a primary and a secondary tileset
    pub fn get_primary_tiles_count(&self) -> Result<usize, ValueGrabError> {
        // ANCHOR Add support for other rom types
        // TODO This is wrong, read it from the correct place
        Ok(match self.rom_type {
            RomType::FireRed | RomType::LeafGreen => 640,
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => 512,
            // _ => return Err(ValueGrabError::NotImplemented),
        })
    }

    /// Get the number of bits to index a block in the tilesets for a map data.
    pub fn get_block_index_bits(&self) -> Result<u8, ValueGrabError> {
        // ANCHOR Add support for other rom types
        // TODO This is wrong, read it from the correct place
        Ok(match self.rom_type {
            RomType::FireRed | RomType::LeafGreen => 10,
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => 10,
            // _ => return Err(ValueGrabError::NotImplemented),
        })
    }

    /// Get the number of palettes in the primary tileset
    pub fn get_primary_palettes_count(&self) -> Result<usize, ValueGrabError> {
        Ok(match self.rom_type {
            RomType::FireRed | RomType::LeafGreen => 7,
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => 6,
        })
    }

    /// Get the total number of palettes
    pub fn get_palettes_count(&self) -> Result<usize, ValueGrabError> {
        Ok(match self.rom_type {
            RomType::FireRed | RomType::LeafGreen | RomType::Emerald => 13,
            RomType::Ruby | RomType::Sapphire => 12,
        })
    }
}

impl Display for ValueGrabError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueGrabError::NotImplemented => write!(f, "Rom type not implemented"),
            ValueGrabError::InvalidOffset => write!(f, "Invalid offset"),
        }
    }
}
