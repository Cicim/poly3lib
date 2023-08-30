//! This crate exports methods needed to write data to and from ROM.

pub(crate) mod data;
pub(crate) mod lz77;

// Tests
#[cfg(test)]
mod tests;

// Re-exported modules
pub mod types;
pub mod values;

// Re-exports
pub use data::{Offset, Pointer, RomBase, RomData, RomFileError, RomIoError};
pub use lz77::{Lz77DecompressedData, Lz77DecompressionError};

// Re-export the struct macro
pub use struct_macro::rom_struct;

// Re-export the RomValues trait (and derive macro)
pub use value_derive::RomValues;
pub use values::RomValues;
