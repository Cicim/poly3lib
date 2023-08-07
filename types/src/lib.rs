use thiserror::Error;

/// Provides support for the LZ77 compression algorithm.
pub mod lz77;

/// Provides reading and writing functions for the Rust array type
/// which corresponds to a static-length array in a C-struct.
pub mod arrays;
/// Provides reading and writing integer types (signed and unsigned)
pub mod integers;
/// Provides the [`PointedData`] type, which is a way to read and write
/// data that is behind a dereferencing operation.
pub mod pointers;
/// Provides support for a dynamic-length array, whose size is defined
/// as a function of the container struct's fields.
pub mod vectors;

/// Provides support for colors and palettes
pub mod colors;
/// Provides support for tiles and metatiles
pub mod tiles;

/// An error that may occur when trying to read data from a ROM.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Error)]
pub enum GBAIOError {
    #[error("Invalid offset: ${0:07X}")]
    InvalidOffset(u32),
    #[error("Misaligned offset: ${0:07X} (alignment: {1})")]
    MisalignedOffset(u32, u32),
    #[error("Writing invalid pointer: {0:#010X}")]
    WritingInvalidPointer(u32),
    #[error("Repointing error")]
    RepointingError,
    #[error("Lz77 error: {0}")]
    Lz77Error(#[from] lz77::Lz77ReadingError),
    #[error("Unknown error: {0}")]
    Unknown(&'static str),
}

pub trait GBAType: Sized + std::fmt::Debug {
    const SIZE: usize;

    /// Read a value of this type from the given byte slice at the given offset.
    fn read_from(bytes: &[u8], offset: usize) -> Result<Self, GBAIOError>;
    /// Write a value of this type to the given byte slice at the given offset.
    fn write_to(&self, bytes: &mut [u8], offset: usize) -> Result<(), GBAIOError>;
}
