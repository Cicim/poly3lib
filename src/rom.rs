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
    Unknown,
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
    pub fn new(data: Vec<u8>) -> Self {
        Rom {
            data,
            rom_type: RomType::Unknown,
        }
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

        Ok(Rom { data, rom_type })
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
            return Err(OutOfBoundsError);
        }

        self.data[offset..offset + data.len()].copy_from_slice(data);
        Ok(())
    }

    /// Reads the given number of bytes from the ROM at the given offset.
    pub fn read(&self, offset: usize, size: usize) -> Result<&[u8], OutOfBoundsError> {
        // Check that the offset is valid
        if offset + size > self.size() {
            return Err(OutOfBoundsError);
        }

        Ok(&self.data[offset..offset + size])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const FIRERED_PATH: &str = "tests/roms/firered.gba";

    #[test]
    fn rom_loading() {
        // Load the ROM
        let rom = Rom::load(FIRERED_PATH);
        assert!(rom.is_ok());
        let rom = rom.unwrap();

        assert_eq!(rom.data.len(), 0x1000000);
        assert_eq!(rom.rom_type, RomType::FireRed);
    }

    #[test]
    fn rom_saving() {
        // Load the ROM
        let rom = Rom::load(FIRERED_PATH).unwrap();
        // Save the ROM to a temporary file
        let tmp_path = "tests/roms/firered.tmp.gba";
        rom.save(tmp_path).unwrap();
        // Load the temporary ROM
        let tmp_rom = Rom::load(tmp_path).unwrap();
        // Delete the temporary file
        std::fs::remove_file(tmp_path).unwrap();
        // Compare the two ROMs
        assert!(rom.data == tmp_rom.data);
    }

    // Read/Write tests
    #[test]
    fn rom_read() {
        // Load the ROM
        let rom = Rom::new(vec![1, 2, 3, 4]);
        // Read the first 4 bytes
        let bytes = rom.read(0, 4);
        assert!(bytes.is_ok());

        if let Ok(bytes) = bytes {
            assert_eq!(bytes, vec![1, 2, 3, 4]);
        }
    }

    #[test]
    fn rom_read_out_of_bounds() {
        // Load the ROM
        let rom = Rom::new(vec![0, 0, 0, 0]);
        // Read 4 bytes starting at the end of the ROM
        let bytes = rom.read(rom.size(), 4);
        assert!(bytes.is_err());

        if let Err(err) = bytes {
            assert_eq!(err, OutOfBoundsError);
        }
    }

    #[test]
    fn rom_write() {
        // Load the ROM
        let mut rom = Rom::new(vec![0, 0, 0, 0]);
        // Write 4 bytes to the ROM
        let bytes = rom.write(0, &[0xff, 0xff, 0xff, 0xff]);
        assert!(bytes.is_ok());

        if let Ok(()) = bytes {
            // Read the first 4 bytes
            let bytes = rom.read(0, 4);
            assert!(bytes.is_ok());

            if let Ok(bytes) = bytes {
                assert_eq!(bytes, vec![0xff, 0xff, 0xff, 0xff]);
            }
        }
    }

    #[test]
    fn rom_write_out_of_bounds() {
        // Load the ROM
        let mut rom = Rom::new(vec![0, 0, 0, 0]);
        // Write 4 bytes starting at the end of the ROM
        let bytes = rom.write(rom.size(), &[0xff, 0xff, 0xff, 0xff]);
        assert!(bytes.is_err());

        if let Err(err) = bytes {
            assert_eq!(err, OutOfBoundsError);
        }
    }
}
