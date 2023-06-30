use std::fmt::Display;
use std::fs::File;
use std::io::{Read, Write};

use gba_types::lz77::*;
use gba_types::{GBAIOError, GBAType};

use crate::refs::Refs;

const MAX_ROM_SIZE: usize = 1 << 25;

#[derive(Debug, PartialEq, Eq)]
pub enum RomType {
    FireRed,
    LeafGreen,
    Ruby,
    Sapphire,
    Emerald,
}

impl Display for RomType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RomType::*;
        write!(
            f,
            "{}",
            match self {
                FireRed => "Fire Red",
                LeafGreen => "Leaf Green",
                Ruby => "Ruby",
                Sapphire => "Sapphire",
                Emerald => "Emerald",
            }
        )
    }
}

/// Represents a game ROM.
#[derive(Debug)]
pub struct Rom {
    /// The actual ROM data.
    pub data: Vec<u8>,
    /// The type of ROM.
    pub rom_type: RomType,
    /// The information about all tables in the ROM.
    pub refs: Refs,
}

#[derive(Debug)]
pub enum RomError {
    IoError(std::io::Error),
    InvalidType([u8; 4]),
    InvalidSize(usize),
}

impl Display for RomError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        use RomError::*;
        match self {
            IoError(e) => write!(f, "IO error: {}", e),
            InvalidType(code) => {
                let a = code[0] as char;
                let b = code[1] as char;
                let c = code[2] as char;
                let d = code[3] as char;

                // If none of them is an uppercase letter
                if !a.is_ascii_uppercase()
                    || !b.is_ascii_uppercase()
                    || !c.is_ascii_uppercase()
                    || !d.is_ascii_uppercase()
                {
                    return write!(f, "The file is not a ROM: the identifier is invalid");
                }

                write!(f, "'{}{}{}{}' is not a supported ROM type", a, b, c, d)
            }
            InvalidSize(size) => {
                let bytes = byte_unit::Byte::from_bytes(*size as u64);
                let bytes = bytes.get_appropriate_unit(true);
                write!(f, "{} is not a valid ROM size", bytes)
            }
        }
    }
}

impl Rom {
    /// Loads the ROM into memory.
    ///
    /// Returns an error if the ROM is invalid or if there is an IO error.
    pub fn load(path: &str) -> Result<Self, RomError> {
        // Open the file
        let mut file = File::open(path).map_err(RomError::IoError)?;

        // Read the up to 32MB of the ROM into memory
        let mut data = Vec::with_capacity(MAX_ROM_SIZE);

        // Make sure you can perform the next check
        let len = file.metadata().unwrap().len() as usize;
        if len < 0xB0 || len > MAX_ROM_SIZE {
            return Err(RomError::InvalidSize(len as usize));
        }

        file.read_to_end(&mut data).map_err(RomError::IoError)?;

        // Determine the ROM type
        let rom_type = match &data[0xAC..0xB0] {
            b"AXVE" => RomType::Ruby,
            b"AXPE" => RomType::Sapphire,
            b"BPRE" => RomType::FireRed,
            b"BPGE" => RomType::LeafGreen,
            b"BPEE" => RomType::Emerald,
            code => return Err(RomError::InvalidType(code.try_into().unwrap())),
        };

        // Check if the reference file exists
        let refs_path = format!("{}.refs.json", path);
        let refs = if let Ok(mut file) = File::open(&refs_path) {
            let mut json = String::new();

            if file.read_to_string(&mut json).is_ok() {
                match serde_json::from_str(&json) {
                    Ok(refs) => refs,
                    Err(_) => Refs::default(),
                }
            } else {
                Refs::default()
            }
        } else {
            Refs::default()
        };

        Ok(Rom {
            data,
            rom_type,
            refs,
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

        // Serialize and write the refs to the file
        let refs_path = format!("{}.refs.json", path);
        let mut file = File::create(refs_path).map_err(RomError::IoError)?;
        let json = serde_json::to_string_pretty(&self.refs).unwrap();
        file.write_all(json.as_bytes()).map_err(RomError::IoError)?;

        Ok(())
    }

    /// Returns the size of the ROM in bytes.
    pub fn size(&self) -> usize {
        self.data.len()
    }

    /// Reads a value of type `T` from the ROM at the given offset.
    ///
    /// # Examples
    /// Read a byte from the ROM at offset 0.
    /// ```
    /// use poly3lib::rom::Rom;
    /// let rom = Rom::load("roms/firered.gba").unwrap();
    /// let byte = rom.read::<u8>(0).unwrap();
    /// assert_eq!(byte, 127);
    ///
    /// // Or, equivalently:
    /// let byte: u8 = rom.read(0).unwrap();
    /// assert_eq!(byte, 127);
    /// ```
    ///
    /// Read an array of i16s from the ROM at offset 0.
    /// ```
    /// use poly3lib::rom::Rom;
    /// let rom = Rom::load("roms/firered.gba").unwrap();
    /// let array = rom.read::<[i16; 4]>(0).unwrap();
    /// assert_eq!(array, [127i16, -5632, -220, 20910]);
    ///
    /// // Or, equivalently
    ///
    /// let array: [i16; 4] = rom.read(0).unwrap();
    /// assert_eq!(array, [127i16, -5632, -220, 20910]);
    /// ```
    pub fn read<T: GBAType>(&self, offset: usize) -> Result<T, GBAIOError> {
        T::read_from(&self.data, offset)
    }

    /// Writes a value of type `T` to the ROM at the given offset.
    ///
    /// # Examples
    /// Write a byte to the ROM at offset 0.
    /// ```
    /// use poly3lib::rom::Rom;
    /// let mut rom = Rom::load("roms/firered.gba").unwrap();
    /// rom.write(0, 0x12_u8).unwrap();
    /// assert_eq!(rom.data[0], 0x12);
    /// ```
    ///
    /// Write an array of i16s to the ROM at offset 0.
    /// ```
    /// use poly3lib::rom::Rom;
    /// let mut rom = Rom::load("roms/firered.gba").unwrap();
    /// rom.write(0, [0x12i16, 0x34i16, 0x56i16, 0x78i16]).unwrap();
    /// assert_eq!(rom.data[0..8], [0x12, 0, 0x34, 0, 0x56, 0, 0x78, 0]);
    /// ```
    pub fn write<T: GBAType>(&mut self, offset: usize, value: T) -> Result<(), GBAIOError> {
        value.write_to(&mut self.data, offset)
    }

    /// Returns whether a pointer is a valid offset into the ROM.
    pub fn is_pointer_valid(&self, pointer: u32) -> bool {
        pointer >= 0x08000000 && pointer < 0x08000000 + self.size() as u32
    }

    /// Returns the byte read from the ROM at the given offset, assuming the offset is valid.
    ///
    /// # Examples
    /// Read a byte from the ROM at offset 0.
    /// ```
    /// use poly3lib::rom::Rom;
    /// let rom = Rom::load("roms/firered.gba").unwrap();
    /// assert_eq!(rom.read_byte(0), 127);
    /// ```
    ///
    /// # Panics
    /// Panics if the offset is not valid.
    pub fn read_byte(&self, offset: usize) -> u8 {
        self.data[offset]
    }

    /// Read a pointer from the ROM at the given offset.
    ///
    /// Converts it from a 0x08000000 base address to a 0x00000000 base address
    /// if it lies in the correct range from 0x08000000 to 0x08000000 + rom.size().
    pub fn read_ptr(&self, offset: usize) -> Result<usize, GBAIOError> {
        let pointer = self.read::<u32>(offset)?;

        match self.is_pointer_valid(pointer) {
            false => Err(GBAIOError::InvalidOffset(pointer)),
            true => Ok(pointer as usize - 0x08000000),
        }
    }

    /// Write a pointer to the ROM at the given offset.s
    ///
    /// Converts it from a 0x00000000 base address to a 0x08000000 base address
    /// only if it lies in the correct range from 0x00000000 to rom.size().
    pub fn write_ptr(&mut self, offset: usize, ptr: usize) -> Result<(), GBAIOError> {
        if ptr >= self.size() {
            return Err(GBAIOError::InvalidOffset(ptr as u32));
        }

        self.write(offset, (ptr + 0x08000000) as u32)
    }

    /// Find a free offset in the ROM of the given size.
    pub fn find_free_space(&self, size: usize, align: usize) -> Option<usize> {
        gba_types::vectors::find_free_space(&self.data, size, align)
    }

    /// Discover if the data needs a new place in ROM and if so, find it.
    /// Return the offset of the data in ROM, whether it changed or not.
    /// In case everything succeeds, clear all the old data.
    pub fn repoint_offset(
        &mut self,
        offset: usize,
        old_size: usize,
        new_size: usize,
    ) -> Option<usize> {
        gba_types::vectors::repoint_offset(&mut self.data, offset, old_size, new_size)
    }

    /// Clears the data in the ROM at the given offset.
    pub fn clear(&mut self, offset: usize, size: usize) -> Result<(), GBAIOError> {
        if offset + size > self.size() {
            return Err(GBAIOError::InvalidOffset(0x08000000 + offset as u32));
        }
        self.data[offset..offset + size].fill(0xFF);
        Ok(())
    }

    /// Read compressed data from the ROM at the given offset.
    pub fn read_lz77(&self, offset: usize) -> Result<Vec<u8>, GBAIOError> {
        gba_types::lz77::read_lz77(&self.data, offset)
    }

    /// Replaces the Lz77 data at the given offset with the given data.
    /// Returns the offset of the new data (since it may have changed).
    pub fn replace_lz77_data(&mut self, offset: usize, data: &[u8]) -> Result<usize, GBAIOError> {
        // Read the header of the old compressed data
        let inflated_size = lz77_read_header(&self.data, offset)?;
        // Find the old compressed data's size
        let old_size = lz77_get_deflated_size(&self.data, inflated_size);

        // Compute the new size of the data
        let data = lz77_compress(&data, &Lz77Options::fastest());
        let new_size = data.len();

        // Repoint the data if necessary (consider the header)
        let new_offset = self
            .repoint_offset(offset, old_size + 4, new_size + 4)
            .ok_or_else(|| GBAIOError::RepointingError)?;

        // Write the header
        self.data[new_offset] = 0x10;
        self.data[new_offset + 1] = (inflated_size & 0xFF) as u8;
        self.data[new_offset + 2] = ((inflated_size >> 8) & 0xFF) as u8;
        self.data[new_offset + 3] = ((inflated_size >> 16) & 0xFF) as u8;
        // Write the data
        self.data[new_offset + 4..new_offset + 4 + new_size].copy_from_slice(&data);

        Ok(new_offset)
    }
}
