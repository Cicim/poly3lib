use crate::rom::Rom;

#[derive(Debug)]
pub enum ValueGrabError {
    NotImplemented,
    InvalidOffset,
}

impl Rom {
    /// Get the maximum size for a primary and a secondary tileset
    pub fn get_maximum_tileset_size(&self) -> Result<(usize, usize), ValueGrabError> {
        use crate::rom::RomType;

        // ANCHOR Add support for other rom types
        // TODO This is wrong, read it from the correct place
        let (primary, secondary) = match self.rom_type {
            RomType::FireRed | RomType::LeafGreen => (640, 384),
            RomType::Emerald | RomType::Ruby | RomType::Sapphire => (512, 512),
            RomType::Unknown => return Err(ValueGrabError::NotImplemented),
        };

        Ok((primary, secondary))
    }
}
