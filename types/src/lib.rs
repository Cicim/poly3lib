use std::fmt::{Display, Formatter};

pub mod arrays;
pub mod colors;
pub mod integers;
pub mod pointers;
pub mod tiles;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum GBAIOError {
    Unknown(&'static str),
    InvalidOffset(u32),
    MisalignedOffset(u32, u32),
    WritingInvalidPointer(u32),
}

pub trait GBAType: Sized + std::fmt::Debug {
    const SIZE: usize;

    /// Read a value of this type from the given byte slice at the given offset.
    fn read_from(bytes: &[u8], offset: usize) -> Result<Self, GBAIOError>;
    /// Write a value of this type to the given byte slice at the given offset.
    fn write_to(&self, bytes: &mut [u8], offset: usize) -> Result<(), GBAIOError>;
}

impl Display for GBAIOError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use GBAIOError::*;
        match self {
            Unknown(s) => write!(f, "Unknown error: {}", s),
            InvalidOffset(offset) => write!(f, "Invalid offset: ${:07X}", offset),
            MisalignedOffset(offset, alignment) => write!(
                f,
                "Misaligned offset: ${:08X} (alignment: {})",
                offset, alignment
            ),
            WritingInvalidPointer(offset) => write!(f, "Writing invalid pointer: {:#010X}", offset),
        }
    }
}
