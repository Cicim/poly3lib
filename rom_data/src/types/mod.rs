use crate::{Offset, RomData, RomIoError};

mod arrays;
mod bitfields;
mod pointers;
mod primitives;
mod vectors;

pub use arrays::RomArray;
pub use bitfields::BitFields;
pub use pointers::{RomPointer, Void};
pub use vectors::RomVector;

/// A type that can be read, written to [`RomData`].
pub trait RomType: RomReadableType + RomWritableType {}

/// A type that can be read from [`RomData`].
///
/// Is always automatically implemented for structs.
pub trait RomReadableType: RomSizedType {
    /// Read this type from `data` at `offset`.
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError>;
}

/// A type that can be written to [`RomData`].
///
/// Is always automatically implemented for structs.
pub trait RomWritableType: RomSizedType {
    /// Write this type to `data` at `offset`.
    fn write_to(self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError>;
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
    fn clear_in(rom: &mut RomData, offset: Offset) -> Result<(), RomIoError>;
}

/// Defines a method for obtaining the size of a type.
///
/// The ROM size of a type cannot be known at compile time because of differences
/// in the various ROM bases, however they must still be Sized in Rust.
pub trait RomSizedType: Sized {
    /// Returns the size of the type in bytes based on things it can
    /// read of the ROM. This includes the ROM base.
    fn get_size(rom: &RomData) -> usize;

    /// Returns the alignment of the type in bytes based on things it can
    /// read of the ROM. This includes the ROM base.
    fn get_alignment(rom: &RomData) -> usize;
}
