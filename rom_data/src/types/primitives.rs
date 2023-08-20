//! Provides implemntations of [`RomReadableType`], [`RomWritableType`], and
//! [`RomClearableType`] for primitive types ([u8], [u16], [u32], [i8], [i16], [i32]).

use crate::types::{RomClearableType, RomReadableType, RomSizedType, RomWritableType};
use crate::{Offset, RomData, RomIoError};

macro_rules! get_size {
    ($target:ty) => {
        std::mem::size_of::<$target>()
    };
}

macro_rules! impl_rom_type_for_integer {
    ($target:ty, $unsigned:ty, $read_method:ident, $write_method:ident) => {
        impl RomSizedType for $target {
            fn get_size(_: &RomData) -> usize {
                get_size!($target)
            }
            fn get_alignment(_: &RomData) -> usize {
                get_size!($target)
            }
        }

        impl RomReadableType for $target {
            fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
                if offset % get_size!($target) != 0 {
                    return Err(RomIoError::Misaligned(offset, get_size!($target) as u8));
                }

                Ok(rom.$read_method(offset)? as $target)
            }
        }

        impl RomWritableType for $target {
            fn write_to(self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
                if offset % get_size!($target) != 0 {
                    return Err(RomIoError::Misaligned(offset, get_size!($target) as u8));
                }

                rom.$write_method(offset, self as $unsigned)
            }
        }

        impl RomClearableType for $target {
            fn clear_in(rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
                if offset % get_size!($target) != 0 {
                    return Err(RomIoError::Misaligned(offset, get_size!($target) as u8));
                }

                rom.clear_bytes(offset, get_size!($target))
            }
        }
    };
}

impl_rom_type_for_integer!(u8, u8, read_byte, write_byte);
impl_rom_type_for_integer!(u16, u16, read_halfword, write_halfword);
impl_rom_type_for_integer!(u32, u32, read_word, write_word);

impl_rom_type_for_integer!(i8, u8, read_byte, write_byte);
impl_rom_type_for_integer!(i16, u16, read_halfword, write_halfword);
impl_rom_type_for_integer!(i32, u32, read_word, write_word);

#[cfg(test)]
mod tests {
    use super::*;

    // Everything uses the same read/write methods as [`RomData`] so we only
    // need to check the new alignment error and the signedness.
    #[test]
    fn test_misaligned() {
        let rom = RomData::new(crate::RomBase::FireRed, 0x10);

        assert_eq!(
            u16::read_from(&rom, 0x01).unwrap_err(),
            RomIoError::Misaligned(0x01, 2)
        );
        assert_eq!(
            u32::read_from(&rom, 0x01).unwrap_err(),
            RomIoError::Misaligned(0x01, 4)
        );
    }

    #[test]
    fn test_signed_integers() {
        let mut rom = RomData::new(crate::RomBase::FireRed, 0x10);

        assert_eq!(i8::read_from(&rom, 0x00).unwrap(), -1);
        assert_eq!(i16::read_from(&rom, 0x00).unwrap(), -1);
        assert_eq!(i32::read_from(&rom, 0x00).unwrap(), -1);

        rom.write_word(0, 0xFFFF_FFFE).unwrap();

        assert_eq!(i8::read_from(&rom, 0x00).unwrap(), -2);
    }
}
