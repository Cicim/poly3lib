use gba_types::GBAIOError;
use thiserror::Error;
use thumb::Instruction;

use crate::rom::{Rom, RomType};

#[derive(Debug, Error)]
pub enum ValueGrabError {
    #[error("The value cannot be grabbed on this ROM type")]
    NotImplemented,
    #[error("The offset is invalid")]
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

#[derive(Debug, Error)]
pub enum RomValueError {
    #[error("Value read for {0} is not implemented")]
    NotImplemented(&'static str),

    #[error("Cannot create the value {0} by shifting an 8-bit integer")]
    CannotCreateShiftValue(u32),
    #[error("The instruction at offset {0:#010x} is not an {1} instruction: {2:?}")]
    IncorrectInstruction(usize, &'static str, Option<Instruction>),

    #[error("Not all values were equal when reading a value from the ROM")]
    Mismatch,

    #[error("IO error: {0}")]
    IoError(#[from] GBAIOError),
}

// ANCHOR Functions for reading/writing values in various forms
impl Rom {
    /// Reads the shift value of an LSR instruction at the given offset.
    ///
    /// If the offset does not contain an LSR instruction, an error is returned.
    pub fn read_lsr_shift(&self, offset: usize) -> Result<u8, RomValueError> {
        // Decode the instruction at the offset
        let data: u16 = self.read(offset)?;
        let instr = Instruction::decode(data);
        match instr {
            Some(Instruction::LsrImm { rd: _, rs: _, imm5 }) => Ok(imm5),
            _ => Err(RomValueError::IncorrectInstruction(offset, "LSR", instr)),
        }
    }
    /// Writes the shift value to an already existing LSR instruction at the given offset.
    pub fn write_lsr_shift(&mut self, offset: usize, shift: u8) -> Result<(), RomValueError> {
        // Decode the instruction at the offset
        let data: u16 = self.read(offset)?;
        let instr = Instruction::decode(data);
        match Instruction::decode(data) {
            Some(Instruction::LsrImm { rd, rs, .. }) => {
                // Encode the instruction with the new shift value
                let new_instruction: u16 = Instruction::LsrImm {
                    rd,
                    rs,
                    imm5: shift,
                }
                .into();

                // Write the new instruction
                Ok(self.write(offset, new_instruction)?)
            }
            _ => Err(RomValueError::IncorrectInstruction(offset, "LSR", instr)),
        }
    }

    /// Reads a value in the form of the following couple of instructions
    /// ```asm
    /// mov rx, #A
    /// lsl rx, #B
    /// ```
    /// into the value `A << B`.
    pub fn read_mov_lsl_value(&self, mut offset: usize) -> Result<u32, RomValueError> {
        // Decode the MOV instruction
        let data: u16 = self.read(offset)?;
        let instr = Instruction::decode(data);
        let mov = match instr {
            Some(Instruction::MovImm { rd: _, imm8 }) => imm8,
            _ => return Err(RomValueError::IncorrectInstruction(offset, "MOV", instr)),
        };

        // Decode the LSL instruction
        offset += 2;
        let data: u16 = self.read(offset)?;
        let instr = Instruction::decode(data);
        let lsl = match instr {
            Some(Instruction::LslImm { rd: _, rs: _, imm5 }) => imm5,
            _ => return Err(RomValueError::IncorrectInstruction(offset, "LSL", instr)),
        };

        // Return the value
        Ok((mov as u32) << lsl)
    }
    /// Replaces the two instructions of the following form so that they
    /// compose the value
    /// ```asm
    /// mov rx, #A
    /// lsl rx, #B
    /// ```
    /// given the value `V = A << B`.
    pub fn write_mov_lsl_value(&mut self, offset: usize, value: u32) -> Result<(), RomValueError> {
        // Get the base and shift
        let left_shift = value.trailing_zeros();
        let base = value >> left_shift;
        if base > 0xFF {
            return Err(RomValueError::CannotCreateShiftValue(value));
        }

        // Decode the MOV instruction
        let data: u16 = self.read(offset)?;
        let instr = Instruction::decode(data);
        // Write the new instruction
        match instr {
            Some(Instruction::MovImm { rd, .. }) => {
                let new_instruction: u16 = Instruction::MovImm {
                    rd,
                    imm8: base as u8,
                }
                .into();
                self.write(offset, new_instruction)?;
            }
            _ => return Err(RomValueError::IncorrectInstruction(offset, "MOV", instr)),
        }

        // Decode the LSL instruction
        let data: u16 = self.read(offset + 2)?;
        let instr = Instruction::decode(data);
        // Write the new instruction
        match instr {
            Some(Instruction::LslImm { rd, rs, .. }) => {
                let new_instruction: u16 = Instruction::LslImm {
                    rd,
                    rs,
                    imm5: left_shift as u8,
                }
                .into();
                self.write(offset + 2, new_instruction)?;
            }
            _ => return Err(RomValueError::IncorrectInstruction(offset, "LSL", instr)),
        }

        Ok(())
    }

    /// Reads a word returing `RomValueError`s instead of `GBAIOError`s
    pub fn read_word_value(&self, offset: usize) -> Result<u32, RomValueError> {
        let value: u32 = self.read(offset)?;
        Ok(value)
    }
    /// Writes a word returing `RomValueError`s instead of `GBAIOError`s
    pub fn write_word_value(&mut self, offset: usize, value: u32) -> Result<(), RomValueError> {
        self.write(offset, value)?;
        Ok(())
    }

    /// Reads a values from multiple references.
    ///
    /// If not all references are the same, an error is returned.
    ///
    /// # Example
    /// ```no_run
    /// rom.read_same(vec![0, 4, 8], Rom::read_lsr_shift)
    /// ```
    pub fn read_and_assert_all_equal<T, F>(
        &self,
        refs: &Vec<usize>,
        f: F,
    ) -> Result<T, RomValueError>
    where
        F: Fn(&Rom, usize) -> Result<T, RomValueError>,
        T: Eq + Copy,
    {
        let mut saved_value = None;
        for offset in refs {
            let value = f(self, *offset)?;

            if let Some(last_value) = saved_value {
                if last_value != value {
                    return Err(RomValueError::Mismatch);
                }
            } else {
                saved_value = Some(value);
            }
        }

        Ok(saved_value.unwrap())
    }
    /// Writes a value to multiple locations using the given function
    /// If any of the writes fail, an error is returned.
    ///
    /// # Example
    /// ```no_run
    /// rom.write_same(vec![0, 4, 8], Rom::replace_lsr_shift, 0x1F)
    /// ```
    pub fn write_same<T, F>(
        &mut self,
        refs: &Vec<usize>,
        f: F,
        value: T,
    ) -> Result<(), RomValueError>
    where
        F: Fn(&mut Rom, usize, T) -> Result<(), RomValueError>,
        T: Copy,
    {
        for offset in refs {
            f(self, *offset, value)?;
        }

        Ok(())
    }
}
