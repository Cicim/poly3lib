use std::{
    collections::HashMap,
    fmt::Display,
    fs::File,
    io::{Read, Write},
};

use colored::Colorize;
use thiserror::Error;
use thumb::{Instruction, Processor};

use crate::{
    allocation::AllocatedBytes,
    lz77::Lz77Header,
    types::{RomClearableType, RomReadableType, RomSizedType, RomWritableType, TextError},
    Lz77DecompressedData, Lz77DecompressionError, SearchPattern, SearchResult,
};

/// Maximum space in the ROM section of GBA memory.
const MAX_ROM_SIZE: usize = 1 << 25;
/// Base address of the ROM
const ROM_BASE: usize = 0x08_000_000;

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
        // Different colored strings
        use RomBase as B;
        match self {
            B::Ruby => write!(f, "{}", "Ruby".red()),
            B::Sapphire => write!(f, "{}", "Sapphire".blue()),
            B::FireRed => write!(f, "{}", "Fire Red".bright_red()),
            B::LeafGreen => write!(f, "{}", "Leaf Green".bright_green()),
            B::Emerald => write!(f, "{}", "Emerald".green()),
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
#[derive(Clone)]
pub struct RomData {
    pub base: RomBase,

    /// Rom data.
    bytes: Vec<u8>,
    /// Allocated bytes in the ROM (to avoid accidentally overwriting data)
    pub allocated: AllocatedBytes,
}

impl Display for RomData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let size = self.size();
        let size = (size as f32) / 1024.0 / 1024.0;

        let code = self.bytes[0xAC..0xB0]
            .iter()
            .map(|x| *x as char)
            .collect::<String>();
        let name = self.name();

        write!(
            f,
            "ROM: {} ({}, {}) ({:.2} MB)",
            self.base, code, name, size
        )
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

        // Find the bits that are allocated
        let allocated = AllocatedBytes::new(size);

        RomData {
            base,
            bytes,
            allocated,
        }
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
            b"AXVE" | b"AXVD" | b"AXVS" | b"AXVF" | b"AXVI" => RomBase::Ruby,
            b"AXPE" | b"AXPD" | b"AXPS" | b"AXPF" | b"AXPI" => RomBase::Sapphire,
            b"BPRE" | b"BPRD" | b"BPRS" | b"BPRF" | b"BPRI" => RomBase::FireRed,
            b"BPGE" | b"BPGD" | b"BPGS" | b"BPGF" | b"BPGI" => RomBase::LeafGreen,
            b"BPEE" | b"BPED" | b"BPES" | b"BPEF" | b"BPEI" => RomBase::Emerald,
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

        // Load the allocated bytes bitarray
        let mut bits_file_path = std::path::PathBuf::from(path);
        bits_file_path.set_extension("bin");

        let allocated = if let Some(allocated) =
            AllocatedBytes::try_read_from_file(bits_file_path, &bytes, 0xFF)
        {
            allocated
        } else {
            AllocatedBytes::build_from_data(&bytes, 0xFF)
        };

        Ok(RomData {
            bytes,
            base,
            allocated,
        })
    }

    /// Saves the [`RomData`] to the given path.
    ///
    /// Does not check whether you changed the bytes defining the ROM base.
    pub fn save(&self, path: &str) -> Result<(), std::io::Error> {
        // Open the file
        let mut file = File::create(path)?;
        // Write the ROM to the file
        file.write_all(&self.bytes)?;

        // Open the bits file
        let mut bits_file_path = std::path::PathBuf::from(path);
        bits_file_path.set_extension("bin");
        // Write the allocated bytes bitarray to the file
        self.allocated.write_to_file(bits_file_path)?;

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

    // ANCHOR Slice reading
    /// Reads a slice with the given size at the given offset while checking bounds.
    pub fn read_slice(&self, offset: Offset, size: usize) -> RomIoResult<&[u8]> {
        if size == 0 {
            return Ok(&[]);
        }

        // Check bounds
        if !self.in_bounds(offset + size - 1) {
            return Err(RomIoError::OutOfBounds(offset, size));
        }

        Ok(&self.bytes[offset..offset + size])
    }

    /// Returns a slice of the ROM starting from the given offset.
    pub fn read_slice_from(&self, offset: Offset) -> RomIoResult<&[u8]> {
        // Check bounds
        if !self.in_bounds(offset) {
            return Err(RomIoError::OutOfBounds(offset, 1));
        }

        Ok(&self.bytes[offset..])
    }

    /// Writes the given slice at the given offset while checking bounds.
    pub fn write_slice(&mut self, offset: Offset, slice: &[u8]) -> RomIoResult {
        if slice.len() == 0 {
            return Ok(());
        }

        // Check bounds
        if !self.in_bounds(offset + slice.len() - 1) {
            return Err(RomIoError::OutOfBounds(offset, slice.len()));
        }

        // Write the slice
        self.bytes[offset..offset + slice.len()].copy_from_slice(slice);
        Ok(())
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

        self.allocated.set_bit(offset);
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

        self.allocated.set_bit(offset);
        self.allocated.set_bit(offset + 1);
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

        self.allocated.set_consecutive_bits(offset, 4);
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
        if pointer >= ROM_BASE && pointer < ROM_BASE + self.size() {
            Ok(pointer - ROM_BASE)
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
        let pointer = value + ROM_BASE;
        // Convert the pointer to bytes
        let bytes = pointer.to_le_bytes();
        // Write the bytes
        self.bytes[offset] = bytes[0];
        self.bytes[offset + 1] = bytes[1];
        self.bytes[offset + 2] = bytes[2];
        self.bytes[offset + 3] = bytes[3];

        self.allocated.set_consecutive_bits(offset, 4);
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
    pub fn write<T: RomWritableType + RomSizedType>(
        &mut self,
        offset: Offset,
        value: T,
    ) -> RomIoResult {
        value.write_to(self, offset)?;
        self.allocated
            .set_consecutive_bits(offset, T::get_size(self));

        Ok(())
    }

    /// Clears a value at the given offset.
    ///
    /// The type of the value being cleared must implement [`RomClearableType`].
    pub fn clear<T: RomClearableType + RomSizedType>(&mut self, offset: Offset) -> RomIoResult {
        T::clear_in(self, offset)?;

        self.allocated
            .clear_consecutive_bits(offset, T::get_size(self));

        Ok(())
    }

    // ANCHOR Utilities
    /// Checks if the given offset is in bounds for this ROM.
    #[inline]
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

    /// Checks if the given pointer is in bounds for this ROM.
    #[inline]
    pub fn is_pointer_valid(&self, pointer: Pointer) -> bool {
        !(pointer < 0x08_000_000 || pointer > 0x08_000_000 + self.bytes.len() as u32)
    }

    /// Find the offset of the next occurrence of a byte starting from the given offset.
    pub fn find_byte_from(&self, offset: Offset, byte: u8) -> Option<Offset> {
        for i in offset..self.bytes.len() {
            if self.bytes[i] == byte {
                return Some(i);
            }
        }
        None
    }

    /// Finds all offsets in the ROM, whether they are aligned to 4 or not.
    ///
    /// Returns a map with the offset as the key and the number
    /// of times it was found as the value.
    #[must_use = "this operation is very expensive, so its result should be used"]
    pub fn find_all_offsets(&self) -> HashMap<Offset, u32> {
        let mut map = HashMap::new();

        let start_offset = 0;
        let offset = start_offset;

        // Read the first three bytes, then
        let mut b1 = self.bytes[offset];
        let mut b2 = self.bytes[offset + 1];
        let mut b3 = self.bytes[offset + 2];

        for i in 3..self.bytes.len() {
            let b4 = self.bytes[offset + i];

            if b4 == 0x08 || b4 == 0x09 {
                // Compose the whole u32 (little endian)
                let ptr_bytes = [b1, b2, b3, b4];
                let ptr = u32::from_le_bytes(ptr_bytes) as usize;

                if ptr >= ROM_BASE && ptr <= ROM_BASE + self.bytes.len() {
                    let offset = ptr - ROM_BASE;
                    // Increase the counter for that offset
                    let counter = map.entry(offset).or_insert(0);
                    *counter += 1;
                }
            }

            // Shift the bytes
            b1 = b2;
            b2 = b3;
            b3 = b4;
        }

        map
    }

    /// Finds all references to the given offset in the ROM,
    /// returning a vector with the offsets of the references.
    pub fn find_references(&self, offset: Offset, alignment: usize) -> Vec<Offset> {
        let mut references = Vec::new();

        // Make sure the pointer is valid
        if offset > self.bytes.len() {
            return references;
        }

        // Convert the given pointer to a bytearray
        let pointer: u32 = offset as u32 + ROM_BASE as u32;
        let pointer: [u8; 4] = pointer.to_le_bytes();

        // Search for the pointer in the ROM
        for reference in (0..self.bytes.len() - 4).step_by(alignment) {
            if self.bytes[reference..reference + 4] == pointer {
                references.push(reference);
            }
        }

        references
    }

    // ANCHOR Free space functions
    /// Clears the bytes at the given offset and size.
    ///
    /// In the context of this library, "clearing" means setting the bytes to `0xFF`.
    pub fn clear_bytes(&mut self, offset: Offset, size: usize) -> RomIoResult {
        if size == 0 {
            return Ok(());
        }

        // Check bounds
        if !self.in_bounds(offset + size - 1) {
            return Err(RomIoError::OutOfBounds(offset, size));
        }

        // Clear the bytes
        self.bytes[offset..offset + size].fill(0xff);
        self.allocated.clear_consecutive_bits(offset, size);

        Ok(())
    }

    /// Sets the given bytes to zero.
    ///
    /// This is useful in case you want to reserve space from the offset finder.
    pub fn allocate(&mut self, offset: Offset, size: usize) -> RomIoResult {
        // Check bounds
        if !self.in_bounds(offset + size - 1) {
            return Err(RomIoError::OutOfBounds(offset, size));
        }

        // Clear the bytes
        self.bytes[offset..offset + size].fill(0);
        self.allocated.set_consecutive_bits(offset, size);

        Ok(())
    }

    /// Finds the offset to a free space in the ROM with the given size and alignment.
    pub fn find_free_space(&self, size: usize, align: usize) -> RomIoResult<Offset> {
        match self.allocated.find_free_space(size, align) {
            Some(offset) => Ok(offset),
            None => Err(RomIoError::CannotFindSpace(size)),
        }
    }

    /// ANCHOR Search functions
    /// Find all references to the given bytes in the ROM.
    pub fn find_bytes(&self, bytes: &[u8]) -> Vec<Offset> {
        let mut offsets = Vec::new();

        self.bytes
            .windows(bytes.len())
            .enumerate()
            .for_each(|(i, window)| {
                if window == bytes {
                    offsets.push(i);
                }
            });

        offsets
    }

    /// Find the first occurrence of a pattern and return its offset
    /// and the match groups.
    pub fn find_pattern(&self, pattern: &SearchPattern) -> Option<SearchResult> {
        pattern.find(&self.bytes)
    }

    /// Returns free space that can contain `new_size` bytes. If `old_size` is
    /// different from `new_size`, the old space is cleared.
    ///
    /// If necessary, a new space is found and returned.
    pub fn repoint_offset(
        &mut self,
        offset: Offset,
        old_size: usize,
        new_size: usize,
    ) -> RomIoResult<Offset> {
        if old_size == new_size {
            // We can just return the old location
            return Ok(offset);
        }

        if old_size > new_size {
            // We need to clear the excess bytes so that others can use them
            self.clear_bytes(offset + new_size, old_size - new_size)?;
            // And then return the old size
            return Ok(offset);
        }

        // If old_size < new_size
        // We need to clear the old spot
        self.clear_bytes(offset, old_size)?;
        // And find a new space to return
        // REVIEW New offsets in repoint_offset are always aligned to 4 bytes.
        self.find_free_space(new_size, 4)
    }

    // ANCHOR LZ77 Methods
    /// Reads LZ77-compressed bytes from ROM and decompresses them.
    ///
    /// Only returns a [`Vec<u8>`].
    pub fn read_compressed(&self, offset: Offset) -> RomIoResult<Vec<u8>> {
        let read: Lz77DecompressedData = self.read(offset)?;
        Ok(read.data)
    }

    /// Writes the LZ77 header then the data at the given offset.
    pub fn write_compressed(&mut self, offset: Offset, data: &[u8]) -> RomIoResult {
        // Check bounds
        if !self.in_bounds(offset) {
            return Err(RomIoError::OutOfBounds(offset, data.len()));
        }
        // Compress the data
        let compressed = crate::lz77::compress(data);

        // Write the header
        self.write(offset, Lz77Header::new(data.len()))?;

        // Write the compressed data
        self.write_slice(offset + 4, &compressed)
    }

    /// Replaces the LZ77-compressed data at the given offset with new data
    /// to compress. Uses the old space if possible, otherwise reallocates.
    ///
    /// Returns the offset of the new data.
    pub fn repoint_compressed_data(&mut self, offset: Offset, data: &[u8]) -> RomIoResult<Offset> {
        // Compress the data
        let compressed = crate::lz77::compress(data);

        // Get the deflated size of the data
        let new_offset = match crate::lz77::get_deflated_size(self, offset) {
            Ok(deflated_size) => {
                // Find the offset that can contain this data
                let old_size = deflated_size + 4;
                let new_size = compressed.len() + 4;
                self.repoint_offset(offset, old_size, new_size)?
            }

            // If there is an error, then we have to find a new space
            Err(_) => {
                // Allocate enough space for the new compressed data
                self.find_free_space(compressed.len() + 4, 4)?
            }
        };

        // Write header and compressed_data
        self.write(new_offset, Lz77Header::new(data.len()))?;
        self.write_slice(new_offset + 4, &compressed)?;

        Ok(new_offset)
    }

    /// Clears the LZ77-compressed data at the given offset.
    pub fn clear_compressed_data(&mut self, offset: Offset) -> RomIoResult {
        let size = crate::lz77::get_deflated_size(self, offset)?;
        self.clear_bytes(offset, size + 4)
    }

    // ANCHOR Misc
    /// Returns a new processor for this ROM.
    pub fn get_cpu(&self) -> Processor {
        Processor::new(&self.bytes)
    }

    /// Decodes the instruction at the given offset.
    pub fn decode_instruction(&self, offset: Offset) -> RomIoResult<Option<Instruction>> {
        // Read the halfword for the instruction
        let halfword = self.read_halfword(offset)?;
        Ok(Instruction::decode(halfword))
    }

    /// Encodes the given instruction and writes it at the given offset.
    pub fn encode_instruction(&mut self, offset: Offset, instruction: Instruction) -> RomIoResult {
        let halfword: u16 = instruction.into();
        self.write_halfword(offset, halfword)
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

    #[error("Lz77 decompression error: {0}")]
    Lz77DecompressionError(#[from] Lz77DecompressionError),
    #[error("Invalid size of decompressed value: {0}")]
    InvalidDecompressedSize(usize),

    #[error("Writing {0} elements to a RomArray of length {1}")]
    InvalidArrayLength(usize, usize),

    #[error("Text encoding error: {0}")]
    InvalidTextEncoding(#[from] TextError),

    #[error("Cannot find free space of size {0}")]
    CannotFindSpace(usize),
}

type RomIoResult<T = ()> = Result<T, RomIoError>;

#[cfg(test)]
mod test_romdata_methods {
    use crate::{types::RomPointer, RomBase, RomData, RomIoError};

    /// Create a standard ROM for testing.
    ///
    /// The rom is 0x10 bytes long and has the following bytes:
    /// ```no_run
    /// 0xAB 0xCD 0xEF 0x08
    fn create_rom() -> RomData {
        let mut data = RomData::new(RomBase::FireRed, 0x10);
        data.bytes[0] = 0xAB;
        data.bytes[1] = 0xCD;
        data.bytes[2] = 0xEF;
        data.bytes[3] = 0x08;
        data
    }

    #[test]
    fn test_slice_reading() {
        let rom = create_rom();
        assert_eq!(rom.read_slice(0, 4).unwrap(), &[0xAB, 0xCD, 0xEF, 0x08]);
    }

    #[test]
    fn test_slice_writing() {
        let mut rom = create_rom();
        rom.write_slice(0, &[0x12, 0x34, 0x56, 0x78]).unwrap();
        assert_eq!(rom.bytes[0], 0x12);
        assert_eq!(rom.bytes[1], 0x34);
        assert_eq!(rom.bytes[2], 0x56);
        assert_eq!(rom.bytes[3], 0x78);
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

    #[test]
    fn test_find_free_space() {
        let mut rom = RomData::new(RomBase::FireRed, 0x100);

        // Everything is clear, so any size and alignment should work
        assert_eq!(rom.find_free_space(0x10, 1), Ok(0));
        assert_eq!(rom.find_free_space(0x10, 2), Ok(0));
        assert_eq!(rom.find_free_space(0x10, 4), Ok(0));
        assert_eq!(rom.find_free_space(0x100, 4), Ok(0));

        // Add a spot at the beginning
        rom.write_byte(0, 0x00).unwrap();

        assert_eq!(rom.find_free_space(0x10, 1), Ok(1));
        assert_eq!(rom.find_free_space(0x10, 2), Ok(2));
        assert_eq!(rom.find_free_space(0x10, 4), Ok(4));

        // Now, if asking 255 bytes, it should return an error only for alignments of 2 and 4
        assert_eq!(rom.find_free_space(0xFF, 1), Ok(1));
        assert_eq!(
            rom.find_free_space(0xFF, 2),
            Err(RomIoError::CannotFindSpace(0xFF))
        );
        assert_eq!(
            rom.find_free_space(0xFF, 4),
            Err(RomIoError::CannotFindSpace(0xFF))
        );

        // Edge case
        // 00 FF FF FF 00 00 00 00 FF FF FF FF ...
        // The next 4-aligned offset should be 8.
        let mut rom = RomData::new(RomBase::FireRed, 0x10);
        rom.write_byte(0, 5).unwrap();
        rom.write_word(4, 0).unwrap();

        assert_eq!(rom.find_free_space(5, 4), Ok(8));
    }

    #[test]
    fn test_repoint_offset() {
        let old_len = 4;

        // 1. old_len > new_len
        let mut rom = create_rom();
        let new_len = 1;
        let new_offset = rom.repoint_offset(0, old_len, new_len);
        assert_eq!(new_offset, Ok(0));
        assert_eq!(rom.bytes[0], 0xAB);
        assert_eq!(rom.bytes[1], 0xFF);
        assert_eq!(rom.bytes[2], 0xFF);
        assert_eq!(rom.bytes[3], 0xFF);

        // 2. old_len == new_len
        let mut rom = create_rom();
        let new_len = 4;
        let new_offset = rom.repoint_offset(0, old_len, new_len);
        assert_eq!(new_offset, Ok(0));
        assert_eq!(rom.bytes[0], 0xAB);
        assert_eq!(rom.bytes[1], 0xCD);
        assert_eq!(rom.bytes[2], 0xEF);
        assert_eq!(rom.bytes[3], 0x08);

        // 3. old_len < new_len
        let mut rom = create_rom();
        let new_len = 8;
        let new_offset = rom.repoint_offset(0, old_len, new_len);
        assert_eq!(new_offset, Ok(0));
        assert_eq!(rom.bytes[0], 0xFF);
        assert_eq!(rom.bytes[1], 0xFF);
        assert_eq!(rom.bytes[2], 0xFF);
        assert_eq!(rom.bytes[3], 0xFF);
        assert_eq!(rom.bytes[4], 0xFF);
        assert_eq!(rom.bytes[5], 0xFF);
        assert_eq!(rom.bytes[6], 0xFF);
        assert_eq!(rom.bytes[7], 0xFF);
    }

    #[test]
    fn test_indirect_allocation_sets_bits() {
        let mut rom = RomData::new(RomBase::FireRed, 0x100);

        rom.write::<RomPointer<u32>>(0, RomPointer::Valid(4, 0x22))
            .unwrap();

        assert_eq!(rom.read_word(0).unwrap(), 0x08_000_004);
        assert_eq!(rom.read_word(4).unwrap(), 0x22);
        assert!(rom.allocated.get_bit(0));
        assert!(rom.allocated.get_bit(1));
        assert!(rom.allocated.get_bit(2));
        assert!(rom.allocated.get_bit(3));
        assert!(rom.allocated.get_bit(4));
        assert!(rom.allocated.get_bit(5));
        assert!(rom.allocated.get_bit(6));
        assert!(rom.allocated.get_bit(7));
    }
}
