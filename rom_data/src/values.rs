use thiserror::Error;
use thumb::Instruction;

use crate::{Offset, RomData, RomIoError};

/// Trait for reading and writing values from the ROM.
pub trait RomValues: Sized {
    /// Reads all references to to these values, returning the
    /// the final value struct only if they all match.
    fn read_values(rom: &RomData) -> Result<Self, RomValueError>;

    /// Updates all references to these values to the given
    /// value struct.
    fn write_values(self, rom: &mut RomData) -> Result<(), RomValueError>;
}

/// Error type for reading and writing values from the ROM.
#[derive(Debug, Error, PartialEq, Eq)]
pub enum RomValueError {
    #[error("Value read not implemented")]
    NotImplemented,

    #[error("Cannot create the value {0} by shifting an 8-bit integer")]
    CannotCreateShiftValue(u32),
    #[error("The instruction at offset {0:#010x} is not an {1} instruction: {2:?}")]
    IncorrectInstruction(usize, &'static str, Option<Instruction>),

    #[error("Not all values were equal when reading a value from the ROM")]
    Mismatch,

    #[error("IO error: {0}")]
    IoError(#[from] RomIoError),
}

#[derive(Debug, Clone, Copy)]
pub enum RomValueType {
    /// A word value, which is 32 bits.
    Word,
    /// A halfword value, which is 16 bits.
    Halfword,
    /// A byte value, which is 8 bits.
    Byte,

    /// A value of the type `A << B` read from the instructions
    /// ```asm
    /// mov rx, #A
    /// lsl rx, #B
    /// ```
    MovLsl,

    /// The value `S` read from the instruction `lsr rx, #S`.
    Lsr,

    /// The value `imm8` of a `mov rx, #imm8` instruction.
    MovImm,
    /// The value `imm8` of a `add rx, #imm8` instruction.
    AddImm8,
}

impl RomValueType {
    /// Read the value of the given type from the ROM.
    pub fn read(self, rom: &RomData, mut offset: Offset) -> Result<u32, RomValueError> {
        use RomValueType::*;

        match self {
            // Easy, just use the builtin function
            Word => Ok(rom.read_word(offset)?),
            Halfword => Ok(rom.read_halfword(offset)? as u32),
            Byte => Ok(rom.read_byte(offset)? as u32),

            MovLsl => {
                // Decode the MOV instruction
                let mov = match rom.decode_instruction(offset)? {
                    Some(Instruction::MovImm { imm8, .. }) => imm8,
                    any => return Err(RomValueError::IncorrectInstruction(offset, "MOV", any)),
                };

                // Decode the LSL instruction
                offset += 2;
                let lsl = match rom.decode_instruction(offset)? {
                    Some(Instruction::LslImm { imm5, .. }) => imm5,
                    any => return Err(RomValueError::IncorrectInstruction(offset, "LSL", any)),
                };

                // Return the value
                Ok((mov as u32) << lsl)
            }
            Lsr => match rom.decode_instruction(offset)? {
                Some(Instruction::LsrImm { imm5, .. }) => Ok(imm5 as u32),
                any => Err(RomValueError::IncorrectInstruction(offset, "LSR", any)),
            },
            AddImm8 => match rom.decode_instruction(offset)? {
                Some(Instruction::AddImm8 { imm8, .. }) => Ok(imm8 as u32),
                any => Err(RomValueError::IncorrectInstruction(offset, "ADD", any)),
            },
            MovImm => match rom.decode_instruction(offset)? {
                Some(Instruction::MovImm { imm8, .. }) => Ok(imm8 as u32),
                any => Err(RomValueError::IncorrectInstruction(offset, "MOV", any)),
            },
        }
    }

    /// Write the value of the given type to the ROM.
    pub fn write(
        self,
        rom: &mut RomData,
        mut offset: Offset,
        value: u32,
    ) -> Result<(), RomValueError> {
        use RomValueType::*;

        match self {
            // Easy, just use the builtin function
            Word => rom.write_word(offset, value)?,
            Halfword => rom.write_halfword(offset, value as u16)?,
            Byte => rom.write_byte(offset, value as u8)?,

            MovLsl => {
                // Get the base and shift
                let left_shift = value.trailing_zeros() as u8;
                let base = value >> left_shift;
                if base > 0xFF {
                    return Err(RomValueError::CannotCreateShiftValue(value));
                }

                // Update the MOV instruction
                match rom.decode_instruction(offset)? {
                    Some(Instruction::MovImm { rd, .. }) => rom.encode_instruction(
                        offset,
                        Instruction::MovImm {
                            rd,
                            imm8: base as u8,
                        },
                    )?,
                    any => Err(RomValueError::IncorrectInstruction(offset, "MOV", any))?,
                }

                // Update the LSL instruction
                offset += 2;
                match rom.decode_instruction(offset)? {
                    Some(Instruction::LslImm { rd, rs, .. }) => rom.encode_instruction(
                        offset,
                        Instruction::LslImm {
                            rd,
                            rs,
                            imm5: left_shift,
                        },
                    )?,
                    any => return Err(RomValueError::IncorrectInstruction(offset, "LSL", any)),
                }
            }
            Lsr => {
                // Can only replace the instruction
                match rom.decode_instruction(offset)? {
                    Some(Instruction::LsrImm { rd, rs, .. }) => rom.encode_instruction(
                        offset,
                        Instruction::LsrImm {
                            rd,
                            rs,
                            imm5: value as u8,
                        },
                    )?,
                    any => Err(RomValueError::IncorrectInstruction(offset, "LSR", any))?,
                };
            }
            AddImm8 => {
                // Can only replace the instruction
                match rom.decode_instruction(offset)? {
                    Some(Instruction::AddImm8 { rd, .. }) => rom.encode_instruction(
                        offset,
                        Instruction::AddImm8 {
                            rd,
                            imm8: value as u8,
                        },
                    )?,
                    any => Err(RomValueError::IncorrectInstruction(offset, "ADD", any))?,
                };
            }
            MovImm => {
                // Can only replace the instruction
                match rom.decode_instruction(offset)? {
                    Some(Instruction::MovImm { rd, .. }) => rom.encode_instruction(
                        offset,
                        Instruction::MovImm {
                            rd,
                            imm8: value as u8,
                        },
                    )?,
                    any => Err(RomValueError::IncorrectInstruction(offset, "MOV", any))?,
                };
            }
        };

        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RomValueTransformation {
    /// Takes 0xABCD as u16 and returns 0xABCDABCD as u32
    StackedHalfwords,

    /// Takes a u32 and inverts only the lower 16 bits
    Not16,
    /// Performs a negation of the word
    Neg,

    /// Adds the given value
    Add(u32),
    /// Subtracts the given value
    Sub(u32),
    /// Multiplies by the given value
    Mul(u32),
}

impl RomValueTransformation {
    /// Apply the transformation before writing
    pub fn apply_transform(self, value: u32) -> u32 {
        use RomValueTransformation::*;

        match self {
            StackedHalfwords => ((value & 0xffff) << 16) | (value & 0xffff),
            Not16 => value ^ 0xFFFF,
            Neg => -(value as i32) as u32,

            Add(other) => value + other,
            Sub(other) => value - other,
            Mul(other) => value * other,
        }
    }

    /// Invert the transformation to read a value
    pub fn invert_transform(self, value: u32) -> u32 {
        use RomValueTransformation::*;

        match self {
            StackedHalfwords => value & 0xFFFF,
            Not16 => value ^ 0xFFFF,
            Neg => -(value as i32) as u32,

            Add(other) => value - other,
            Sub(other) => value + other,
            Mul(other) => value / other,
        }
    }
}
