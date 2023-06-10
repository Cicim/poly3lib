use crate::{GBAIOError, GBAType};
use std::mem::size_of;

macro_rules! impl_gba_type_for_integer {
    ($t:ty) => {
        impl GBAType for $t {
            const SIZE: usize = size_of::<Self>();

            fn read_from(bytes: &[u8], offset: usize) -> Result<Self, GBAIOError> {
                if offset + size_of::<Self>() > bytes.len() {
                    return Err(GBAIOError::InvalidOffset(offset as u32));
                }
                if offset % size_of::<Self>() != 0 {
                    return Err(GBAIOError::MisalignedOffset(offset as u32, size_of::<Self>() as u32));
                }
                let mut buf = [0; size_of::<Self>()];
                buf.copy_from_slice(&bytes[offset..offset + size_of::<Self>()]);
                Ok(Self::from_le_bytes(buf))
            }

            fn write_to(&self, bytes: &mut [u8], offset: usize) -> Result<(), GBAIOError> {
                if offset + size_of::<Self>() > bytes.len() {
                    return Err(GBAIOError::InvalidOffset(offset as u32));
                }
                if offset % size_of::<Self>() != 0 {
                    return Err(GBAIOError::MisalignedOffset(offset as u32, size_of::<Self>() as u32));
                }
                bytes[offset..offset + size_of::<Self>()].copy_from_slice(&self.to_le_bytes());
                Ok(())
            }
        }
    };
}

impl_gba_type_for_integer!(u8);
impl_gba_type_for_integer!(u16);
impl_gba_type_for_integer!(u32);
impl_gba_type_for_integer!(i8);
impl_gba_type_for_integer!(i16);
impl_gba_type_for_integer!(i32);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_unsigned() {
        let bytes = [0x12, 0x34, 0x56, 0x78];
        assert_eq!(u8::read_from(&bytes, 0).unwrap(), 0x12);
        assert_eq!(u8::read_from(&bytes, 1).unwrap(), 0x34);
        assert_eq!(u8::read_from(&bytes, 2).unwrap(), 0x56);
        assert_eq!(u8::read_from(&bytes, 3).unwrap(), 0x78);
        assert_eq!(u16::read_from(&bytes, 0).unwrap(), 0x3412);
        assert_eq!(u16::read_from(&bytes, 2).unwrap(), 0x7856);
        assert_eq!(u32::read_from(&bytes, 0).unwrap(), 0x78563412);
    }

    #[test]
    fn read_signed() {
        let bytes = [0xFF, 0xFF, 0xFF, 0xFF];
        assert_eq!(i8::read_from(&bytes, 0).unwrap(), -1);
        assert_eq!(i8::read_from(&bytes, 1).unwrap(), -1);
        assert_eq!(i8::read_from(&bytes, 2).unwrap(), -1);
        assert_eq!(i8::read_from(&bytes, 3).unwrap(), -1);
        assert_eq!(i16::read_from(&bytes, 0).unwrap(), -1);
        assert_eq!(i16::read_from(&bytes, 2).unwrap(), -1);
        assert_eq!(i32::read_from(&bytes, 0).unwrap(), -1);
    }

    #[test]
    fn read_out_of_bounds() {
        let bytes = [0x12, 0x34, 0x56, 0x78];
        assert!(u8::read_from(&bytes, 4).is_err());
        assert!(u16::read_from(&bytes, 3).is_err());
        assert!(u32::read_from(&bytes, 2).is_err());
    }

    #[test]
    fn read_misaligned() {
        let bytes = [0x12, 0x34, 0x56, 0x78];
        assert!(u16::read_from(&bytes, 1).is_err());
        assert!(u32::read_from(&bytes, 1).is_err());
        assert!(u32::read_from(&bytes, 2).is_err());
        assert!(u32::read_from(&bytes, 3).is_err());
    }

    #[test]
    fn write_unsigned() {
        let mut bytes = [0; 4];
        u8::write_to(&0x12, &mut bytes, 0).unwrap();
        u8::write_to(&0x34, &mut bytes, 1).unwrap();
        u8::write_to(&0x56, &mut bytes, 2).unwrap();
        u8::write_to(&0x78, &mut bytes, 3).unwrap();
        assert_eq!(bytes, [0x12, 0x34, 0x56, 0x78]);
        u16::write_to(&0xFABE, &mut bytes, 0).unwrap();
        u16::write_to(&0xBEEF, &mut bytes, 2).unwrap();
        assert_eq!(bytes, [0xBE, 0xFA, 0xEF, 0xBE]);
        u32::write_to(&0x12345678, &mut bytes, 0).unwrap();
        assert_eq!(bytes, [0x78, 0x56, 0x34, 0x12]);
    }

    #[test]
    fn write_signed() {
        let mut bytes = [0; 4];
        i8::write_to(&-1, &mut bytes, 0).unwrap();
        i8::write_to(&-1, &mut bytes, 1).unwrap();
        i8::write_to(&-1, &mut bytes, 2).unwrap();
        i8::write_to(&-1, &mut bytes, 3).unwrap();
        assert_eq!(bytes, [0xFF, 0xFF, 0xFF, 0xFF]);
        i16::write_to(&-1, &mut bytes, 0).unwrap();
        i16::write_to(&-1, &mut bytes, 2).unwrap();
        assert_eq!(bytes, [0xFF, 0xFF, 0xFF, 0xFF]);
        i32::write_to(&-1, &mut bytes, 0).unwrap();
        assert_eq!(bytes, [0xFF, 0xFF, 0xFF, 0xFF]);
    }

    #[test]
    fn write_out_of_bounds() {
        let mut bytes = [0; 4];
        assert!(u8::write_to(&0x12, &mut bytes, 4).is_err());
        assert!(u16::write_to(&0x1234, &mut bytes, 3).is_err());
        assert!(u32::write_to(&0x12345678, &mut bytes, 2).is_err());
    }

    #[test]
    fn write_misaligned() {
        let mut bytes = [0; 4];
        assert!(u16::write_to(&0x1234, &mut bytes, 1).is_err());
        assert!(u32::write_to(&0x12345678, &mut bytes, 1).is_err());
        assert!(u32::write_to(&0x12345678, &mut bytes, 2).is_err());
        assert!(u32::write_to(&0x12345678, &mut bytes, 3).is_err());
    }
}
