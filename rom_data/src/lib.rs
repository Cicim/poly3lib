//! This crate exports methods needed to write data to and from ROM.

pub(crate) mod data;

// Tests
#[cfg(test)]
mod tests;

// Re-exported modules
pub mod types;

// Re-exports
pub use data::{Offset, Pointer, RomBase, RomData, RomFileError, RomIoError};

// Re-export the struct macro
pub use struct_macro::rom_struct;
