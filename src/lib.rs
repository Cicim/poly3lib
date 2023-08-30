// Internal modules
mod rom;

// Re-export everything from rom_data as is.
pub use rom_data::*;
// Re-exports
pub use rom::{Rom, RomReferences, RomTable};
