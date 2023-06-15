use std::fs::File;
use std::io::{Read, Write};

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
    Unknown,
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
    InvalidRom,
}

impl Rom {
    /// Create a new ROM from the given buffer.
    pub fn new(data: Vec<u8>) -> Self {
        Rom {
            data,
            rom_type: RomType::Unknown,
            refs: Refs::default(),
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

        // Make sure you can perform the next check
        if data.len() < 0xB0 {
            return Err(RomError::InvalidRom);
        }

        // Determine the ROM type
        let rom_type = match &data[0xAC..0xB0] {
            b"AXVE" => RomType::Ruby,
            b"AXPE" => RomType::Sapphire,
            b"BPRE" => RomType::FireRed,
            b"BPGE" => RomType::LeafGreen,
            b"BPEE" => RomType::Emerald,
            _ => return Err(RomError::InvalidRom),
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

    pub fn is_pointer_valid(&self, pointer: u32) -> bool {
        pointer >= 0x08000000 && pointer < 0x08000000 + self.size() as u32
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
}
