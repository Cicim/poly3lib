use gba_types::GBAIOError;

use crate::rom::Rom;

/// Clears the script only if it is safe to do so.
pub fn clear_if_safe(_rom: &mut Rom, _offset: usize) -> Result<(), GBAIOError> {
    todo!();
}
