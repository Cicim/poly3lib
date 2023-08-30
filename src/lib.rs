// Internal modules
mod rom;

// Sub-systems
pub mod maps;

// Re-export everything from rom_data as is.
pub use rom_data::*;
// Re-exports
pub use rom::{Rom, RomReferences, RomTable};
