use crate::rom::*;

pub enum PointerError {
    OutOfBounds,
    InvalidPointer,
}

impl Rom {
    // -------------------------
    // --- Unsigned integers ---
    // -------------------------

    /// Reads an unsigned 8-bit integer from the ROM at the given offset.
    pub fn read_u8(&self, offset: usize) -> Result<u8, OutOfBoundsError> {
        let bytes = self.read(offset, 1)?;
        Ok(bytes[0])
    }

    /// Reads an unsigned 16-bit integer from the ROM at the given offset.
    pub fn read_u16(&self, offset: usize) -> Result<u16, OutOfBoundsError> {
        let bytes = self.read(offset, 2)?;
        Ok(u16::from_le_bytes([bytes[0], bytes[1]]))
    }

    /// Reads an unsigned 32-bit integer from the ROM at the given offset.
    pub fn read_u32(&self, offset: usize) -> Result<u32, OutOfBoundsError> {
        let bytes = self.read(offset, 4)?;
        Ok(u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    /// Writes an unsigned 8-bit integer to the ROM at the given offset.
    pub fn write_u8(&mut self, offset: usize, value: u8) -> Result<(), OutOfBoundsError> {
        self.write(offset, &[value])
    }

    /// Writes an unsigned 16-bit integer to the ROM at the given offset.
    pub fn write_u16(&mut self, offset: usize, value: u16) -> Result<(), OutOfBoundsError> {
        self.write(offset, &value.to_le_bytes())
    }

    /// Writes an unsigned 32-bit integer to the ROM at the given offset.
    pub fn write_u32(&mut self, offset: usize, value: u32) -> Result<(), OutOfBoundsError> {
        self.write(offset, &value.to_le_bytes())
    }


    // -----------------------
    // --- Signed integers ---
    // -----------------------
    
    /// Reads a signed 8-bit integer from the ROM at the given offset.
    pub fn read_i8(&self, offset: usize) -> Result<i8, OutOfBoundsError> {
        let bytes = self.read(offset, 1)?;
        Ok(bytes[0] as i8)
    }

    /// Reads a signed 16-bit integer from the ROM at the given offset.
    pub fn read_i16(&self, offset: usize) -> Result<i16, OutOfBoundsError> {
        let bytes = self.read(offset, 2)?;
        Ok(i16::from_le_bytes([bytes[0], bytes[1]]))
    }

    /// Reads a signed 32-bit integer from the ROM at the given offset.
    pub fn read_i32(&self, offset: usize) -> Result<i32, OutOfBoundsError> {
        let bytes = self.read(offset, 4)?;
        Ok(i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    /// Writes a signed 8-bit integer to the ROM at the given offset.
    pub fn write_i8(&mut self, offset: usize, value: i8) -> Result<(), OutOfBoundsError> {
        self.write(offset, &[value as u8])
    }

    /// Writes a signed 16-bit integer to the ROM at the given offset.
    pub fn write_i16(&mut self, offset: usize, value: i16) -> Result<(), OutOfBoundsError> {
        self.write(offset, &value.to_le_bytes())
    }

    /// Writes a signed 32-bit integer to the ROM at the given offset.
    pub fn write_i32(&mut self, offset: usize, value: i32) -> Result<(), OutOfBoundsError> {
        self.write(offset, &value.to_le_bytes())
    }

    // ----------------
    // --- Pointers ---
    // ----------------

    /// Reads a pointer from the ROM at the given offset.
    pub fn read_ptr(&self, offset: usize) -> Result<usize, PointerError> {
        // Read the pointer
        let bytes = self.read_u32(offset).map_err(|_| PointerError::OutOfBounds)?;
        // Subtract the ROM base address
        let offset: i32 = bytes as i32 - 0x08000000;

        // Check if the offset is negative or out of bounds
        if offset < 0 || offset as usize >= self.size() {
            return Err(PointerError::InvalidPointer);
        }

        Ok(bytes as usize)
    }

    /// Writes a pointer to the ROM at the given offset.
    pub fn write_ptr(&mut self, offset: usize, value: usize) -> Result<(), PointerError> {
        if value >= self.size() {
            return Err(PointerError::InvalidPointer);
        }

        // Add the ROM base address
        let value = value as u32 + 0x08000000;
        // Write the pointer
        self.write_u32(offset, value).map_err(|_| PointerError::OutOfBounds)
    }
}



#[cfg(test)]
mod tests {
    use super::*;
    const FIRERED_PATH: &str = "tests/roms/firered.gba";

    #[test]
    fn read_u8() {
        // Load the ROM
        let rom = Rom::load(FIRERED_PATH).unwrap();
        // Read the first byte
        let byte = rom.read_u8(0);
        assert!(byte.is_ok());

        if let Ok(byte) = byte {
            assert_eq!(byte, 0x7f);
        }
    }

    #[test]
    fn read_u16() {
        // Load the ROM
        let rom = Rom::load(FIRERED_PATH).unwrap();
        // Read the first 2 bytes
        let bytes = rom.read_u16(0);
        assert!(bytes.is_ok());

        if let Ok(bytes) = bytes {
            assert_eq!(bytes, 0x007f);
        }
    }

    #[test]
    fn read_u32() {
        // Load the ROM
        let rom = Rom::load(FIRERED_PATH).unwrap();
        // Read the first 4 bytes
        let bytes = rom.read_u32(0);
        assert!(bytes.is_ok());

        if let Ok(bytes) = bytes {
            assert_eq!(bytes, 0xea00007f);
        }
    }

    #[test]
    fn write_u8() {
        // Load the ROM
        let mut rom = Rom::load(FIRERED_PATH).unwrap();
        // Write 0xff to the ROM
        let bytes = rom.write_u8(0, 0xff);
        assert!(bytes.is_ok());

        if let Ok(()) = bytes {
            // Read the first byte
            let byte = rom.read_u8(0);
            assert!(byte.is_ok());

            if let Ok(byte) = byte {
                assert_eq!(byte, 0xff);
            }
        }
    }

    #[test]
    fn write_u16() {
        // Load the ROM
        let mut rom = Rom::load(FIRERED_PATH).unwrap();
        // Write 0xffff to the ROM
        let bytes = rom.write_u16(0, 0xfffe);
        assert!(bytes.is_ok());

        if let Ok(()) = bytes {
            // Read the first 2 bytes
            let bytes = rom.read_u16(0);
            assert!(bytes.is_ok());

            if let Ok(bytes) = bytes {
                assert_eq!(bytes, 0xfffe);
            }
        }
    }

    #[test]
    fn write_u32() {
        // Load the ROM
        let mut rom = Rom::load(FIRERED_PATH).unwrap();
        // Write 0xffffffff to the ROM
        let bytes = rom.write_u32(0, 0xabcdef00);
        assert!(bytes.is_ok());

        if let Ok(()) = bytes {
            // Read the first 4 bytes
            let bytes = rom.read_u32(0);
            assert!(bytes.is_ok());

            if let Ok(bytes) = bytes {
                assert_eq!(bytes, 0xabcdef00);
            }
        }
    }

}
