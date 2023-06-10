use std::fmt::Formatter;

use crate::{GBAIOError, GBAType};

pub type VoidPointer = PointedData<Nothing>;

#[derive(Default, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PointedData<T: GBAType> {
    #[default]
    /// When the pointer is null (0), so the data is not present.
    Null,
    /// When the offset is valid, and you have data to write
    /// or when you read the data.
    Valid(u32, T),
    /// When you have a valid offset to write, but you don't want to write
    /// any data to that offset, even though the pointer has a type.
    /// 
    /// Is never created by reading data
    NoData(u32),
    /// When the pointer was not rebased because it was not a valid offset,
    /// so no data can be read, but you may still want to see the result.
    /// 
    /// Throws an error when writing.
    Invalid(u32),
}

impl<T: GBAType> GBAType for PointedData<T> {
    const SIZE: usize = 4;

    fn read_from(bytes: &[u8], offset: usize) -> Result<Self, GBAIOError> {
        if offset + 4 > bytes.len() {
            return Err(GBAIOError::InvalidOffset(offset as u32));
        }
        if offset % 4 != 0 {
            return Err(GBAIOError::MisalignedOffset(offset as u32, 4));
        }
        let mut buf = [0; 4];
        buf.copy_from_slice(&bytes[offset..offset + 4]);
        let pointer = u32::from_le_bytes(buf);

        // If you read a pointer to 0, it's a null pointer.
        // It's still valid to have a null pointer, but no data will be read.
        if pointer == 0 {
            return Ok(PointedData::Null);
        }

        // Transform the pointer into an offset.
        let offset: i32 = pointer as i32 - 0x08000000;
        if offset < 0 || offset as usize >= bytes.len() {
            return Ok(PointedData::Invalid(pointer));
        }

        // Try to read the data at the offset.
        let data = T::read_from(bytes, offset as usize)?;

        // Return the data.
        Ok(PointedData::Valid(offset as u32, data))
    }

    fn write_to(&self, bytes: &mut [u8], offset: usize) -> Result<(), GBAIOError> {
        if offset + 4 > bytes.len() {
            return Err(GBAIOError::InvalidOffset(offset as u32));
        }
        if offset % 4 != 0 {
            return Err(GBAIOError::MisalignedOffset(offset as u32, 4));
        }
        let pointer = match self {
            PointedData::Null => 0,
            PointedData::Valid(offset, data) => {
                // Write the data at the offset.
                data.write_to(bytes, *offset as usize)?;

                *offset + 0x08000000
            },
            // Write the pointer at the offset, but don't write any data.
            PointedData::NoData(offset) => *offset + 0x08000000,
            PointedData::Invalid(pointer) => {
                return Err(GBAIOError::WritingInvalidPointer(*pointer))
            },
        };

        let buf = pointer.to_le_bytes();
        bytes[offset..offset + 4].copy_from_slice(&buf);
        Ok(())
    }
}

impl<T: GBAType> std::fmt::Debug for PointedData<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use PointedData::*;
        match self {
            Null => write!(f, "NULL"),
            Valid(offset, data) => write!(f, "Valid(offset ${:07X} => {:#?})", offset, data),
            NoData(offset) => write!(f, "Valid(offset ${:07X})", offset),
            Invalid(pointer) => write!(f, "Invalid(pointer 0x{:08X})", pointer),
        }
    }
}

#[derive(Default, Clone, Debug, Copy, PartialEq, Eq, Hash)]
pub struct Nothing;

impl GBAType for Nothing {
    const SIZE: usize = 0;

    fn read_from(_bytes: &[u8], _offset: usize) -> Result<Self, GBAIOError> {
        Ok(Nothing)
    }

    fn write_to(&self, _bytes: &mut [u8], _offset: usize) -> Result<(), GBAIOError> {
        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_u32_pointer() {
        let mut bytes = [0xFF; 8];
        let pointer = PointedData::Valid(0, 0x12345678u32);
        pointer.write_to(&mut bytes, 4).unwrap();
        assert_eq!(bytes, [0x78, 0x56, 0x34, 0x12, 0, 0, 0, 0x08]);
    }

    #[test]
    fn read_u32_pointer() {
        let bytes = [0x78, 0x56, 0x34, 0x12, 0, 0, 0, 0x08];
        let pointer = PointedData::<u32>::read_from(&bytes, 4).unwrap();
        assert_eq!(pointer, PointedData::Valid(0, 0x12345678));
    }

    #[test]
    fn read_invalid_pointer() {
        let bytes = [0x78, 0x56, 0x34, 0x00];
        let pointer = PointedData::<u32>::read_from(&bytes, 0).unwrap();
        assert_eq!(pointer, PointedData::Invalid(0x345678));

        let bytes = [0x78, 0x56, 0x34, 0xA2];
        let pointer = PointedData::<u32>::read_from(&bytes, 0).unwrap();
        assert_eq!(pointer, PointedData::Invalid(0xA2345678));
    }

    #[test]
    fn read_null_pointer() {
        let bytes = [0x00, 0x00, 0x00, 0x00];
        let pointer = PointedData::<u32>::read_from(&bytes, 0).unwrap();
        assert_eq!(pointer, PointedData::Null);
    }

    #[test]
    fn write_null_pointer() {
        let mut bytes = [0xFF; 4];
        let pointer = PointedData::<u32>::Null;
        pointer.write_to(&mut bytes, 0).unwrap();
        assert_eq!(bytes, [0, 0, 0, 0]);
    }

    #[test]
    fn write_invalid_pointer() {
        let mut bytes = [0xFF; 4];
        let pointer = PointedData::<u32>::Invalid(0x12345678);
        assert_eq!(
            pointer.write_to(&mut bytes, 0),
            Err(GBAIOError::WritingInvalidPointer(0x12345678))
        );
        assert_eq!(bytes, [0xFF; 4]);
    }

    #[test]
    fn dereference_multiple_pointers() {
        let bytes = [
            0x04, 0x00, 0x00, 0x08, 0x08, 0x00, 0x00, 0x08, 0x78, 0x56, 0x34, 0x12,
        ];
        let pointer = PointedData::<PointedData<u32>>::read_from(&bytes, 0).unwrap();
        assert_eq!(
            pointer,
            PointedData::Valid(4, PointedData::Valid(8, 0x12345678))
        );
    }

    #[test]
    fn dereference_pointer_to_nothing() {
        let bytes = [0x00, 0x00, 0x00, 0x00];
        let pointer = PointedData::<Nothing>::read_from(&bytes, 0).unwrap();
        assert_eq!(pointer, PointedData::Null);

        let mut bytes = [0xFF; 4];
        let pointer = PointedData::<Nothing>::Null;
        pointer.write_to(&mut bytes, 0).unwrap();
        assert_eq!(bytes, [0, 0, 0, 0]);
    }
}
