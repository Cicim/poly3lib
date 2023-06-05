use std::fs::File;
use std::io::{Read, Write};

const MAX_ROM_SIZE: usize = 1 << 25;

#[derive(Debug, PartialEq, Eq)]
pub enum RomType {
    FireRed,
    LeafGreen,
    Ruby,
    Sapphire,
    Emerald,
}

/// Represents a game ROM.
#[derive(Debug, PartialEq, Eq)]
pub struct Rom {
    pub data: Vec<u8>,
    pub rom_type: RomType,
}

#[derive(Debug)]
pub enum RomError {
    IoError(std::io::Error),
    InvalidRom,
}

#[derive(Debug, PartialEq, Eq)]
pub struct OutOfBoundsError;

impl Rom {
    // Create a new ROM from the given buffer.
    pub fn new(data: Vec<u8>, rom_type: RomType) -> Self {
        Rom { data, rom_type }
    }

    /// Loads the ROM into memory.
    ///
    /// Returns an error if the ROM is invalid or if there is an IO error.
    pub fn load(path: &str) -> Result<Self, RomError> {
        // Open the file
        let mut file = File::open(path).map_err(RomError::IoError)?;

        // Read the up to 32MB of the ROM into memory
        let mut data = Vec::with_capacity(MAX_ROM_SIZE);
        file.read_to_end(&mut data).map_err(RomError::IoError)?;

        // Determine the ROM type
        let rom_type = match &data[0xAC..0xB0] {
            b"AXVE" => RomType::Ruby,
            b"AXPE" => RomType::Sapphire,
            b"BPRE" => RomType::FireRed,
            b"BPGE" => RomType::LeafGreen,
            b"BPEE" => RomType::Emerald,
            _ => return Err(RomError::InvalidRom),
        };

        Ok(Rom {
            data,
            rom_type,
        })
    }

    /// Saves the ROM to the given path.
    ///
    /// Returns an error if there is an IO error.
    pub fn save(&self, path: &str) -> Result<(), RomError> {
        // Open the file
        let mut file = File::create(path).map_err(RomError::IoError)?;

        // Write the ROM to the file
        file.write_all(&self.data).map_err(RomError::IoError)?;

        Ok(())
    }

    /// Returns the size of the ROM in bytes.
    pub fn size(&self) -> usize {
        self.data.len()
    }

    /// Writes the given data to the ROM at the given offset.
    pub fn write(&mut self, offset: usize, data: &[u8]) -> Result<(), OutOfBoundsError> {
        // Check that the offset is valid
        if offset + data.len() > self.size() {
            return Err(OutOfBoundsError)
        }

        self.data[offset..offset + data.len()].copy_from_slice(data);
        Ok(())
    }

    /// Reads the given number of bytes from the ROM at the given offset.
    pub fn read(&self, offset: usize, size: usize) -> Result<&[u8], OutOfBoundsError> {
        // Check that the offset is valid
        if offset + size > self.size() {
            return Err(OutOfBoundsError)
        }

        Ok(&self.data[offset..offset + size])
    }

}

