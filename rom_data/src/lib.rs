//! This crate exports methods needed to write data to and from ROM.

pub(crate) mod data;
pub mod types;

// Re-exports
pub use data::{Offset, Pointer, RomBase, RomData, RomFileError, RomIoError};
