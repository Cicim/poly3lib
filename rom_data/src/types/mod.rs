use crate::{Offset, RomData, RomIoError};

mod primitives;

/// A type that can be read, written to [`RomData`].
pub trait RomType: RomReadableType + RomWritableType {}

/// A type that can be read from [`RomData`].
///
/// Is always automatically implemented for structs.
pub trait RomReadableType: Sized {
    /// The size of this type in bytes.
    const SIZE: usize;

    /// Read this type from `data` at `offset`.
    fn read(data: &RomData, offset: Offset) -> Result<Self, RomIoError>;
}

/// A type that can be written to [`RomData`].
///
/// Is always automatically implemented for structs.
pub trait RomWritableType {
    /// Write this type to `data` at `offset`.
    fn write(self, data: &mut RomData, offset: Offset) -> Result<(), RomIoError>;
}

/// A type that can be cleared from the ROM.
///
/// A type implementing this is expected to clear its data from the ROM
/// as well as anything else that may be pointed to by the data.
///
/// If necessary, you can expect this method to read a copy of the type
/// before proceeding with clearing everything referenced by it.
///
/// It is automatically implemented for structs only if the flag
/// `CLEAR` is specified.
pub trait RomClearableType {
    /// Clear this type in `data` at `offset`.
    fn clear(data: &mut RomData, offset: Offset) -> Result<(), RomIoError>;
}
