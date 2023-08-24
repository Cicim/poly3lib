use std::{
    fmt::Display,
    fs::File,
    io::{Read, Write},
};

use colored::Colorize;
use thiserror::Error;

use crate::types::{RomClearableType, RomReadableType, RomWritableType};

/// Maximum space in the ROM section of GBA memory.
const MAX_ROM_SIZE: usize = 1 << 25;

#[derive(Debug, Error)]
pub enum RomFileError {
    #[error("This ROM is not supported: {0}")]
    UnsupportedRomType(String),
    #[error("The file is not a ROM: the identifier is invalid")]
    InvalidRomIdentifier,
    #[error("{0} is not a valid ROM size")]
    InvalidSize(usize),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
}

/// Base ROM type.
///
/// Based on the ROM type some things may be located
/// at different offsets and some structs may be different.
///
/// *May support different revisions in the future*
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum RomBase {
    Ruby,
    Sapphire,
    FireRed,
    LeafGreen,
    Emerald,
}

impl Display for RomBase {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RomBase::*;

        // Different colored strings
        match self {
            Ruby => write!(f, "{}", "Ruby (AXVE)".red()),
            Sapphire => write!(f, "{}", "Sapphire (AXPE)".blue()),
            FireRed => write!(f, "{}", "Fire Red (BPRE)".bright_red()),
            LeafGreen => write!(f, "{}", "Leaf Green (BPGE)".bright_green()),
            Emerald => write!(f, "{}", "Emerald (BPEE)".green()),
        }
    }
}

/// The ROM data.
///
/// This struct contains the ROM type and the bytes of the ROM.
///
/// REVIEW *May have support for custom settable attributes
/// in the future for more options while handling structs,
/// for example a way of knowing whether a patch was applied*
pub struct RomData {
    pub base: RomBase,
    bytes: Vec<u8>,
}

impl Display for RomData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let size = self.size();
        let size = (size as f32) / 1024.0 / 1024.0;
        write!(f, "ROM: {} ({:.2} MB)", self.base, size)
    }
}

// **Note** that in order to run some of the doctests, there needs to
// be a valid `pokefirered.gba` file in the `roms/` folder relative
// to the root of the project.
impl RomData {
    // ANCHOR Creation/loading
    /// Creates a new ROM with the given base and size.
    ///
    /// Available only for internal testing.
    ///
    /// # Example
    /// ```
    /// use rom_data::{RomData, RomBase};
    ///
    /// let data = RomData::new(RomBase::FireRed, 0x100);
    /// assert_eq!(data.base, RomBase::FireRed);
    /// assert_eq!(data.size(), 0x100);
    /// ```
    pub fn new(base: RomBase, size: usize) -> Self {
        // Create the bytes
        let bytes = vec![0xFF; size];

        RomData { base, bytes }
    }

    /// Loads the [`RomData`] into memory from a given file path.
    ///
    /// # Example
    /// ```
    /// use rom_data::{RomData, RomBase};
    ///
    /// let data = RomData::load("roms/pokefirered.gba").unwrap();
    /// assert_eq!(data.base, RomBase::FireRed);
    /// assert_eq!(data.size(), 0x1000000);
    /// ```
    pub fn load(path: &str) -> Result<Self, RomFileError> {
        // Open the file
        let mut file = File::open(path)?;

        // Read the up to 32MB of the ROM into memory
        let mut bytes = Vec::with_capacity(MAX_ROM_SIZE);

        // Make sure you can perform the next check
        let len = file.metadata().unwrap().len() as usize;
        if len < 0xB0 || len > MAX_ROM_SIZE {
            return Err(RomFileError::InvalidSize(len));
        }
        file.read_to_end(&mut bytes)?;

        // Determine the ROM type
        let base = match &bytes[0xAC..0xB0] {
            b"AXVE" => RomBase::Ruby,
            b"AXPE" => RomBase::Sapphire,
            b"BPRE" => RomBase::FireRed,
            b"BPGE" => RomBase::LeafGreen,
            b"BPEE" => RomBase::Emerald,
            code => {
                // If there is any character that is not an ASCII uppercase
                return if code.iter().any(|x| !x.is_ascii_uppercase()) {
                    // This is not a rom
                    Err(RomFileError::InvalidRomIdentifier)
                } else {
                    // Since it's all uppercase, convert the code to a string
                    let code = String::from_utf8_lossy(code).to_string();
                    Err(RomFileError::UnsupportedRomType(code))
                };
            }
        };

        Ok(RomData { bytes, base })
    }

    /// Saves the [`RomData`] to the given path.
    ///
    /// Does not check whether you changed the bytes defining the ROM base.
    pub fn save(&self, path: &str) -> Result<(), std::io::Error> {
        // Open the file
        let mut file = File::create(path)?;
        // Write the ROM to the file
        file.write_all(&self.bytes)?;

        Ok(())
    }

    // ANCHOR Getters
    /// Returns the name of the game as a string.
    ///
    /// # Example
    /// ```
    /// use rom_data::RomData;
    ///
    /// let data = RomData::load("roms/pokefirered.gba").unwrap();
    /// assert_eq!(data.name(), "POKEMON FIRE");
    /// ```
    pub fn name(&self) -> String {
        // The name data is an ASCII string located at 0xA0..0xAC
        let name = &self.bytes[0xA0..0xAC];
        // Convert the name to a string
        String::from_utf8_lossy(name).to_string()
    }

    /// Gets the size of the ROM.
    #[inline]
    pub fn size(&self) -> usize {
        self.bytes.len()
    }

    // ANCHOR Primitive value reading
    /// Reads a single byte at the given offset while checking bounds.
    pub fn read_byte(&self, offset: Offset) -> RomIoResult<u8> {
        // Check bounds
        if !self.in_bounds(offset) {
            return Err(RomIoError::OutOfBounds(offset, 1));
        }

        Ok(self.bytes[offset])
    }

    /// Reads an halfword (16-bit little endian) at the given offset while checking bounds.
    ///
    /// Does not check whether the offset is aligned to 2 bytes.
    pub fn read_halfword(&self, offset: Offset) -> RomIoResult<u16> {
        // Check bounds
        if !self.in_bounds(offset + 1) {
            return Err(RomIoError::OutOfBounds(offset, 2));
        }

        // Read the bytes
        let bytes = &self.bytes[offset..offset + 2];
        // Convert to a halfword
        Ok(u16::from_le_bytes([bytes[0], bytes[1]]))
    }

    /// Reads a word (32-bit little endian) at the given offset while checking bounds.
    ///
    /// Does not check whether the offset is aligned to 4 bytes.
    pub fn read_word(&self, offset: Offset) -> RomIoResult<u32> {
        // Check bounds
        if !self.in_bounds(offset + 3) {
            return Err(RomIoError::OutOfBounds(offset, 4));
        }

        // Read the bytes
        let bytes = &self.bytes[offset..offset + 4];
        // Convert to a word
        Ok(u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    // ANCHOR Primitive value writing
    /// Writes a single byte at the given offset while checking bounds.
    pub fn write_byte(&mut self, offset: Offset, value: u8) -> RomIoResult {
        // Check bounds
        if !self.in_bounds(offset) {
            return Err(RomIoError::OutOfBounds(offset, 1));
        }

        self.bytes[offset] = value;
        Ok(())
    }

    /// Writes an halfword (16-bit little endian) at the given offset while checking bounds.
    ///
    /// Does not check whether the offset is aligned to 2 bytes.
    pub fn write_halfword(&mut self, offset: Offset, value: u16) -> RomIoResult {
        // Check bounds
        if !self.in_bounds(offset + 1) {
            return Err(RomIoError::OutOfBounds(offset, 2));
        }

        // Convert to bytes
        let bytes = value.to_le_bytes();
        // Write the bytes
        self.bytes[offset] = bytes[0];
        self.bytes[offset + 1] = bytes[1];
        Ok(())
    }

    /// Writes a word (32-bit little endian) at the given offset while checking bounds.
    ///
    /// Does not check whether the offset is aligned to 4 bytes.
    pub fn write_word(&mut self, offset: Offset, value: u32) -> RomIoResult {
        // Check bounds
        if !self.in_bounds(offset + 3) {
            return Err(RomIoError::OutOfBounds(offset, 4));
        }

        // Convert to bytes
        let bytes = value.to_le_bytes();
        // Write the bytes
        self.bytes[offset] = bytes[0];
        self.bytes[offset + 1] = bytes[1];
        self.bytes[offset + 2] = bytes[2];
        self.bytes[offset + 3] = bytes[3];
        Ok(())
    }

    // ANCHOR Offset values
    /// Reads a pointer at the given offset while checking bounds.
    /// Converts that pointer to an offset.
    ///
    /// Does not check whether the offset is aligned to 4 bytes.
    pub fn read_offset(&self, offset: Offset) -> RomIoResult<Offset> {
        let word = self.read_word(offset)?;
        let pointer = word as usize;

        // Convert the word to an offset
        if pointer >= 0x08_000_000 && pointer < 0x08_000_000 + self.size() {
            Ok(pointer - 0x08_000_000)
        } else {
            Err(RomIoError::ReadingInvalidPointer(offset, word))
        }
    }

    /// Writes an offset as a pointer at the given offset while checking bounds.
    ///
    /// Does not check whether the offset is aligned to 4 bytes.
    pub fn write_offset(&mut self, offset: Offset, value: Offset) -> RomIoResult {
        // Check bounds
        if !self.in_bounds(offset + 3) {
            return Err(RomIoError::OutOfBounds(offset, 4));
        }

        // Check that the value is in bounds
        if !self.in_bounds(value) {
            return Err(RomIoError::WritingOutOfBoundsOffset(value));
        }

        // Convert the offset to a pointer
        let pointer = value + 0x08_000_000;
        // Convert the pointer to bytes
        let bytes = pointer.to_le_bytes();
        // Write the bytes
        self.bytes[offset] = bytes[0];
        self.bytes[offset + 1] = bytes[1];
        self.bytes[offset + 2] = bytes[2];
        self.bytes[offset + 3] = bytes[3];
        Ok(())
    }

    // ANCHOR Any RomType
    /// Reads a value at the given offset.
    ///
    /// The type of the value being written must implement [`RomReadableType`].
    ///
    /// # Example
    /// You can call it with the turbofish syntax:
    /// ```
    /// use rom_data::{RomData, RomBase};
    ///
    /// // New initializes everything with 0xff
    /// let data = RomData::new(RomBase::FireRed, 0x04);
    /// let halfword = data.read::<u16>(0x00).unwrap();
    /// assert_eq!(halfword, 0xffff);
    ///
    /// let signed = data.read::<i16>(0x00).unwrap();
    /// assert_eq!(signed, -1);
    /// ```
    ///
    /// Or you can use type inference:
    /// ```
    /// use rom_data::{RomData, RomBase};
    ///
    /// let data = RomData::new(RomBase::FireRed, 0x04);
    /// let halfword: u16 = data.read(0x00).unwrap();
    /// assert_eq!(halfword, 0xffff);
    /// ```
    pub fn read<T: RomReadableType>(&self, offset: Offset) -> RomIoResult<T> {
        T::read_from(self, offset)
    }

    /// Writes a value at the given offset.
    ///
    /// The type of the value being written must implement [`RomWritableType`].
    ///
    /// # Example
    /// You can call it with the turbofish syntax:
    /// ```
    /// use rom_data::{RomData, RomBase};
    ///
    /// let mut data = RomData::new(RomBase::FireRed, 0x04);
    /// data.write::<u16>(0x00, 0x1234).unwrap();
    /// assert_eq!(data.read_halfword(0x00).unwrap(), 0x1234);
    /// ```
    ///
    /// Or you can use type inference:
    /// ```
    /// use rom_data::{RomData, RomBase};
    ///
    /// let mut data = RomData::new(RomBase::FireRed, 0x04);
    /// data.write(0x00, 0x1234_u16).unwrap();
    /// assert_eq!(data.read_halfword(0x00).unwrap(), 0x1234);
    /// ```
    pub fn write<T: RomWritableType>(&mut self, offset: Offset, value: T) -> RomIoResult {
        value.write_to(self, offset)
    }

    /// Clears a value at the given offset.
    ///
    /// The type of the value being cleared must implement [`RomClearableType`].
    pub fn clear<T: RomClearableType>(&mut self, offset: Offset) -> RomIoResult {
        T::clear_in(self, offset)
    }

    // ANCHOR Utilities
    #[inline]
    /// Checks if the given offset is in bounds for this ROM.
    ///
    /// # Example
    /// ```
    /// use rom_data::{RomData, RomBase};
    ///
    /// let data = RomData::new(RomBase::FireRed, 0x10);
    /// assert!(data.in_bounds(0x0));
    /// assert!(data.in_bounds(0xA));
    /// assert!(!data.in_bounds(0x10));
    /// assert!(!data.in_bounds(0x14));
    /// ```
    pub fn in_bounds(&self, offset: Offset) -> bool {
        offset < self.size()
    }

    /// Clears the bytes at the given offset and size.
    ///
    /// In the context of this library, "clearing" means setting the bytes to `0xFF`.
    pub fn clear_bytes(&mut self, offset: Offset, size: usize) -> RomIoResult {
        // Check bounds
        if !self.in_bounds(offset + size - 1) {
            return Err(RomIoError::OutOfBounds(offset, size));
        }

        // Clear the bytes
        for i in offset..offset + size {
            self.bytes[i] = 0xFF;
        }

        Ok(())
    }
}

/// Represents an offset in the ROM binary file.
///
/// Ranges between 0 and the given ROM's size.
///
/// When shown in debug functions, it is padded to 7 digits and prepended by `$`
pub type Offset = usize;

/// A pointer is a value as read from ROM. This means that the pointer
/// is relative to the ROM's base address (`0x08_000_000`)
pub type Pointer = u32;

type RomIoResult<T = ()> = Result<T, RomIoError>;

/// An error that occurs when there is a problem writing or
/// reading a value from a loaded [`RomData`].
#[derive(Debug, Error, PartialEq, Eq)]
pub enum RomIoError {
    #[error("A type of size {1} at ${0:07X} will go out of bounds for this ROM")]
    OutOfBounds(Offset, usize),
    #[error("Writing an offset (${0:07X}) that is out of bounds for this ROM")]
    WritingOutOfBoundsOffset(Offset),
    #[error("The offset ${0:07X} is not aligned to {1} bytes")]
    Misaligned(Offset, u8),

    #[error("The pointer read at ${0:07X} (0x{1:08x}) does not point to anything in this ROM")]
    ReadingInvalidPointer(Offset, Pointer),
    #[error("The pointer written at ${0:07X} (0x{1:08x}) does not point to anything in this ROM")]
    WritingInvalidPointer(Offset, Pointer),

    #[error("Writing {0} elements to a RomArray of length {1}")]
    InvalidArrayLength(usize, usize),
}

#[cfg(test)]
mod test_romdata_methods {
    use crate::{RomBase, RomData, RomIoError};

    // Create a standard ROM for testing
    fn create_rom() -> RomData {
        let mut data = RomData::new(RomBase::FireRed, 0x10);
        data.bytes[0] = 0xAB;
        data.bytes[1] = 0xCD;
        data.bytes[2] = 0xEF;
        data.bytes[3] = 0x08;
        data
    }

    #[test]
    fn test_primitive_reading() {
        let data = create_rom();

        assert_eq!(data.read_byte(0).unwrap(), 0xAB);
        assert_eq!(data.read_byte(1).unwrap(), 0xCD);
        assert_eq!(data.read_byte(2).unwrap(), 0xEF);
        assert_eq!(data.read_byte(3).unwrap(), 0x08);

        assert_eq!(data.read_halfword(0).unwrap(), 0xCDAB);
        assert_eq!(data.read_halfword(1).unwrap(), 0xEFCD);
        assert_eq!(data.read_halfword(2).unwrap(), 0x08EF);

        assert_eq!(data.read_word(0).unwrap(), 0x08EFCDAB);
        assert_eq!(data.read_word(1).unwrap(), 0xFF08EFCD);
    }

    #[test]
    fn test_primitive_writing() {
        let mut data = create_rom();

        data.write_byte(0, 0x12).unwrap();
        data.write_byte(1, 0x34).unwrap();
        data.write_byte(2, 0x56).unwrap();
        data.write_byte(3, 0x78).unwrap();

        data.write_halfword(0, 0x1234).unwrap();
        data.write_halfword(1, 0x5678).unwrap();

        data.write_word(0x0, 0x12345678).unwrap();

        assert_eq!(data.bytes[0], 0x78);
        assert_eq!(data.bytes[1], 0x56);
        assert_eq!(data.bytes[2], 0x34);
        assert_eq!(data.bytes[3], 0x12);
    }

    #[test]
    fn test_primitive_errors() {
        let data = create_rom();

        assert_eq!(data.read_byte(0x10), Err(RomIoError::OutOfBounds(0x10, 1)));
        assert_eq!(
            data.read_halfword(0x10),
            Err(RomIoError::OutOfBounds(0x10, 2))
        );
        assert_eq!(data.read_word(0x10), Err(RomIoError::OutOfBounds(0x10, 4)));
    }

    #[test]
    fn test_valid_offset_reading() {
        let mut data = create_rom();
        data.write_word(0, 0x08_000_004).unwrap();

        assert_eq!(data.read_offset(0).unwrap(), 4);
    }

    #[test]
    fn test_invalid_offset_reading() {
        let data = create_rom();

        assert_eq!(
            data.read_offset(0),
            Err(RomIoError::ReadingInvalidPointer(0, 0x08EFCDAB))
        );
        assert_eq!(
            data.read_offset(1),
            Err(RomIoError::ReadingInvalidPointer(1, 0xFF08EFCD))
        );
        assert_eq!(
            data.read_offset(0x100),
            Err(RomIoError::OutOfBounds(0x100, 4))
        );
    }

    #[test]
    fn test_offset_writing() {
        let mut data = create_rom();

        assert_eq!(
            data.write_offset(0, 0x123456),
            Err(RomIoError::WritingOutOfBoundsOffset(0x123456))
        );

        assert_eq!(
            data.write_offset(0x10, 0),
            Err(RomIoError::OutOfBounds(0x10, 4))
        );

        data.write_offset(0, 0x08).unwrap();
        assert_eq!(data.bytes[0], 0x08);
        assert_eq!(data.bytes[1], 0x00);
        assert_eq!(data.bytes[2], 0x00);
        assert_eq!(data.bytes[3], 0x08);

        assert_eq!(data.read_offset(0).unwrap(), 0x08);
    }

    #[test]
    fn test_general_errors() {
        let mut data = create_rom();

        assert_eq!(
            data.clear_bytes(0x10, 0x20),
            Err(RomIoError::OutOfBounds(0x10, 0x20))
        );

        assert_eq!(data.clear_bytes(0x0, 0x10), Ok(()));

        assert_eq!(data.bytes[0], 0xFF);
        assert_eq!(data.bytes[1], 0xFF);
        assert_eq!(data.bytes[2], 0xFF);
        assert_eq!(data.bytes[3], 0xFF);
    }

    #[test]
    fn test_general_read() {
        let rom = create_rom();

        assert_eq!(rom.read::<u8>(0), rom.read_byte(0));
        assert_eq!(rom.read::<u16>(0), rom.read_halfword(0));
        assert_eq!(rom.read::<u32>(0), rom.read_word(0));

        assert_eq!(rom.read::<u16>(0x10), Err(RomIoError::OutOfBounds(0x10, 2)));
        assert_eq!(rom.read::<u16>(1), Err(RomIoError::Misaligned(1, 2)));
    }

    #[test]
    fn test_general_write() {
        let mut rom = create_rom();

        rom.write::<u8>(0, 0x12).unwrap();
        assert_eq!(rom.bytes[0], 0x12);

        rom.write::<u16>(0, 0x1234).unwrap();
        assert_eq!(rom.bytes[0], 0x34);
        assert_eq!(rom.bytes[1], 0x12);

        rom.write::<u32>(0, 0x12345678).unwrap();
        assert_eq!(rom.bytes[0], 0x78);
        assert_eq!(rom.bytes[1], 0x56);
        assert_eq!(rom.bytes[2], 0x34);
        assert_eq!(rom.bytes[3], 0x12);

        assert_eq!(
            rom.write::<u16>(0x10, 0x1234),
            Err(RomIoError::OutOfBounds(0x10, 2))
        );
        assert_eq!(
            rom.write::<u16>(1, 0x1234),
            Err(RomIoError::Misaligned(1, 2))
        );
    }
}
