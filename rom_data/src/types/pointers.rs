//! Declares a new type [`RomPointer<T>`] that allows to read/write a value of type `T`
//! in the ROM referenced by a pointer.

use serde::{ser::SerializeStruct, Deserialize, Deserializer, Serialize, Serializer};

use crate::{Offset, Pointer, RomData, RomIoError};

use super::{RomClearableType, RomReadableType, RomSizedType, RomWritableType};

/// Allows you to point to a [`RomType`] in the Rom
#[derive(Default, PartialEq, Eq, Clone)]
pub enum RomPointer<T = Void> {
    /// The pointer is **NULL** (`0`), so there is no data attached to it,
    /// as dereferencing it would cause a crash.
    #[default]
    Null,

    /// The pointer does not point to a valid offset in the ROM, so its
    /// data is not read.
    ///
    /// This is useful for reading structs that may have invalid pointers in
    /// them but which we do not want to discard for that reason.
    ///
    /// In any case, this variant cannot be written to the ROM.
    Invalid(Pointer),

    /// The pointer points to a valid offset in the ROM and its data has been read.
    ///
    /// This is the general value returned when reading a [`RomPointer<T>`].
    Valid(Offset, T),

    /// The pointer points to a valid offset in the ROM but its data has not been
    /// read, or this pointer has been created manually just to set a pointer
    /// to a specific offset.
    NoData(Offset),
}

/// The only RomType with size 0.
///
/// Used to avoid recursively reading a type that contains a pointer to itself,
/// or in any other case where we just want to read the pointer and not the data.
#[derive(Serialize, Deserialize, PartialEq, Eq, Clone, Copy, Debug, Default)]
pub struct Void;

impl<T> RomPointer<T> {
    /// Returns an offset read by this pointer only if it is valid.
    pub fn offset(&self) -> Option<Offset> {
        match self {
            RomPointer::Valid(offset, _) => Some(*offset),
            RomPointer::NoData(offset) => Some(*offset),
            _ => None,
        }
    }

    /// Returns a reference to the data read by this pointer only if it exists.
    pub fn data(&self) -> Option<&T> {
        match self {
            RomPointer::Valid(_, data) => Some(data),
            _ => None,
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for RomPointer<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RomPointer::Null => write!(f, "Null"),
            RomPointer::Invalid(pointer) => write!(f, "Invalid(0x{:08x})", pointer),
            RomPointer::Valid(offset, data) => write!(f, "Valid(${:07X}, {:#?})", offset, data),
            RomPointer::NoData(offset) => write!(f, "NoData(${:07X})", offset),
        }
    }
}

impl RomPointer {
    /// Returns a new [`RomPointer`] (of type void) pointing to the given offset.
    pub fn new(offset: usize) -> RomPointer {
        Self::NoData(offset)
    }
}

// ANCHOR RomType impls for RomPointer
impl<T: RomReadableType + RomSizedType> RomReadableType for RomPointer<T> {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        // Read a u32 (checking alignment)
        let word: u32 = rom.read(offset)?;
        let pointer = word as usize;

        // Null pointer
        if pointer == 0 {
            Ok(RomPointer::Null)
        }
        // Valid pointer
        else if pointer >= 0x08_000_000 && pointer < 0x08_000_000 + rom.size() {
            // Convert it to an offset
            let offset: Offset = pointer - 0x08_000_000;

            // If the inner type is not void, read it
            if T::get_size(rom) != 0 {
                let data = T::read_from(rom, offset)?;
                Ok(RomPointer::Valid(offset, data))
            }
            // Else, read without data
            else {
                Ok(RomPointer::NoData(offset))
            }
        }
        // Invalid pointer
        else {
            Ok(RomPointer::Invalid(word))
        }
    }
}

impl<T: RomWritableType> RomWritableType for RomPointer<T> {
    fn write_to(self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        match self {
            // Null pointer (just write 0)
            RomPointer::Null => rom.write(offset, 0_u32),

            // For invalid pointers, you could try to write them, but it would
            // result in an error, so we can just return the error directly.
            RomPointer::Invalid(pointer) => Err(RomIoError::WritingInvalidPointer(offset, pointer)),

            // Valid pointer
            RomPointer::Valid(pointer_offset, data) => {
                // Write the data at the offset
                data.write_to(rom, pointer_offset)?;

                // Write the offset
                rom.write_offset(offset, pointer_offset)
            }

            // No data (just write the offset)
            RomPointer::NoData(pointer_offset) => rom.write_offset(offset, pointer_offset),
        }
    }
}

impl<T: RomSizedType> RomSizedType for RomPointer<T> {
    fn get_size(_: &RomData) -> usize {
        4
    }
    fn get_alignment(_: &RomData) -> usize {
        4
    }
}

impl<T: RomClearableType> RomClearableType for RomPointer<T> {
    fn clear_in(rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        // Read this pointer without data
        let pointer = RomPointer::<Void>::read_from(rom, offset)?;

        // Clear the data if it exists
        if let Some(offset) = pointer.offset() {
            T::clear_in(rom, offset)?;
        }

        // Clear this pointer
        u32::clear_in(rom, offset)?;

        Ok(())
    }
}

// ANCHOR RomType impls for Void
impl RomSizedType for Void {
    fn get_size(_: &RomData) -> usize {
        0
    }
    fn get_alignment(_: &RomData) -> usize {
        0
    }
}
impl RomReadableType for Void {
    fn read_from(_: &RomData, _: Offset) -> Result<Self, RomIoError> {
        Ok(Void)
    }
}
impl RomWritableType for Void {
    fn write_to(self, _: &mut RomData, _: Offset) -> Result<(), RomIoError> {
        Ok(())
    }
}
impl RomClearableType for Void {
    fn clear_in(_: &mut RomData, _: Offset) -> Result<(), RomIoError> {
        Ok(())
    }
}

// ANCHOR Serialize and Deserialize methods
// We define custom ones which are faster to interpret with JavaScript.
impl<T: Serialize> Serialize for RomPointer<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use RomPointer::*;

        match self {
            // Null is serialized as the number 0
            Null => serializer.serialize_u32(0),

            // Invalid is serialized as the pointer itself
            Invalid(pointer) => serializer.serialize_u32(*pointer),

            // A valid pointer with data is serialized as a struct with two fields
            // RomPointer {
            //      offset: u32,        // number in JSON
            //      data: T             // Using the serializer for T
            // }
            Valid(offset, data) => {
                let mut state: _ = serializer.serialize_struct("RomPointer", 2)?;
                state.serialize_field("offset", offset)?;
                state.serialize_field("data", data)?;
                state.end()
            }

            // A valid pointer without data is serialized as a struct with one field
            // RomPointer {
            //      offset: u32,        // number in JSON
            // }
            NoData(offset) => {
                let mut state: _ = serializer.serialize_struct("RomPointer", 2)?;
                state.serialize_field("offset", offset)?;
                state.end()
            }
        }
    }
}

impl<'de, T: for<'a> Deserialize<'a>> Deserialize<'de> for RomPointer<T> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            Data,
            Offset,
        }

        // Visitor for RomPointer
        impl<'de, T: for<'a> Deserialize<'a>> serde::de::Visitor<'de> for RomPointer<T> {
            type Value = RomPointer<T>;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str("valid JSON representation of RomPointer")
            }

            // A direct integer can only be a null or invalid pointer
            fn visit_u64<E: serde::de::Error>(self, v: u64) -> Result<Self::Value, E> {
                if v == 0 {
                    Ok(RomPointer::Null)
                } else {
                    Ok(RomPointer::Invalid(v as u32))
                }
            }

            fn visit_map<A: serde::de::MapAccess<'de>>(
                self,
                mut map: A,
            ) -> Result<Self::Value, A::Error> {
                let mut offset: Option<usize> = None;
                let mut data: Option<T> = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Data => {
                            if data.is_some() {
                                return Err(serde::de::Error::duplicate_field("data"));
                            }
                            data = Some(map.next_value()?);
                        }
                        Field::Offset => {
                            if offset.is_some() {
                                return Err(serde::de::Error::duplicate_field("offset"));
                            }
                            offset = Some(map.next_value()?);
                        }
                    }
                }

                match (offset, data) {
                    (Some(offset), Some(data)) => Ok(RomPointer::Valid(offset, data)),
                    (Some(offset), None) => Ok(RomPointer::NoData(offset)),

                    // The "offset" field is required
                    (None, _) => Err(serde::de::Error::missing_field("offset")),
                }
            }
        }

        deserializer.deserialize_any(RomPointer::Null)
    }
}

// ANCHOR Tests
#[cfg(test)]
mod tests {
    use super::*;

    fn create_rom() -> RomData {
        RomData::new(crate::RomBase::FireRed, 0x20)
    }

    #[test]
    fn test_offset_function() {
        let pointer = RomPointer::<Void>::Null;
        assert_eq!(pointer.offset(), None);

        let pointer = RomPointer::<Void>::Invalid(0x12345678);
        assert_eq!(pointer.offset(), None);

        let pointer = RomPointer::<Void>::Valid(0x12345678, Void);
        assert_eq!(pointer.offset(), Some(0x12345678));

        let pointer = RomPointer::<Void>::NoData(0x12345678);
        assert_eq!(pointer.offset(), Some(0x12345678));
    }

    #[test]
    fn test_data_function() {
        let pointer = RomPointer::<Void>::Null;
        assert_eq!(pointer.data(), None);

        let pointer = RomPointer::<Void>::Invalid(0x12345678);
        assert_eq!(pointer.data(), None);

        let pointer = RomPointer::<i32>::Valid(0x12345678, -1);
        assert_eq!(pointer.data(), Some(&-1));

        let pointer = RomPointer::<Void>::NoData(0x12345678);
        assert_eq!(pointer.data(), None);
    }

    #[test]
    fn test_read_valid_pointer() {
        let mut rom = create_rom();
        // Write a pointer to byte 4 in the first word
        rom.write_offset(0x0, 0x4).unwrap();
        // Write an integer to byte 4
        rom.write_word(0x4, 0x12345678).unwrap();

        // Read a u32 pointer
        let pointer: RomPointer<u32> = RomPointer::read_from(&rom, 0x0).unwrap();
        assert_eq!(pointer, RomPointer::Valid(0x4, 0x12345678));

        // Read a void pointer
        let pointer: RomPointer = RomPointer::read_from(&rom, 0x0).unwrap();
        assert_eq!(pointer, RomPointer::NoData(0x4));

        // Read a null pointer
        rom.write_word(0x0, 0).unwrap();

        // Read a u32 pointer
        let pointer: RomPointer<u32> = RomPointer::read_from(&rom, 0x0).unwrap();
        assert_eq!(pointer, RomPointer::Null);

        // Read a void pointer
        let pointer: RomPointer = RomPointer::read_from(&rom, 0x0).unwrap();
        assert_eq!(pointer, RomPointer::Null);
    }

    #[test]
    fn test_read_invalid_pointer() {
        let mut rom = create_rom();

        // Write an invalid pointer
        rom.write_word(0x0, 0x1234567).unwrap();

        // Read a u32 pointer
        let pointer: RomPointer<u32> = RomPointer::read_from(&rom, 0x0).unwrap();
        assert_eq!(pointer, RomPointer::Invalid(0x1234567));

        // Read a void pointer
        let pointer: RomPointer = RomPointer::read_from(&rom, 0x0).unwrap();
        assert_eq!(pointer, RomPointer::Invalid(0x1234567));
    }

    #[test]
    fn test_read_pointer_error() {
        let rom = create_rom();

        // Read misaligned
        let pointer: Result<RomPointer<u32>, RomIoError> = RomPointer::read_from(&rom, 0x1);
        assert_eq!(pointer, Err(RomIoError::Misaligned(0x1, 4)));

        // Read out of bounds
        let pointer: Result<RomPointer<u32>, RomIoError> = RomPointer::read_from(&rom, 0x20);
        assert_eq!(pointer, Err(RomIoError::OutOfBounds(0x20, 4)));
    }

    #[test]
    fn test_write_valid() {
        let mut rom = create_rom();

        // Write a u32 pointer
        let pointer = RomPointer::Valid(0x4, 0x12345678);
        pointer.write_to(&mut rom, 0x0).unwrap();

        // Assert that the pointer was written
        assert_eq!(rom.read_word(0x0).unwrap(), 0x08_000_004);
        assert_eq!(rom.read_word(0x4).unwrap(), 0x12345678);

        let mut rom = create_rom();
        // Write a pointer with no data
        let pointer: RomPointer<u32> = RomPointer::NoData(0x4);
        pointer.write_to(&mut rom, 0x0).unwrap();

        // Assert that the pointer was written
        assert_eq!(rom.read_word(0x0).unwrap(), 0x08_000_004);
        // Assert that no data was written
        assert_eq!(rom.read_word(0x4).unwrap(), 0xFFFF_FFFF);

        let mut rom = create_rom();
        // Write a null pointer
        let pointer: RomPointer<u32> = RomPointer::Null;
        pointer.write_to(&mut rom, 0x0).unwrap();

        // Assert that the pointer was written
        assert_eq!(rom.read_word(0x0).unwrap(), 0x00_000_000);
    }

    #[test]
    fn test_write_invalid() {
        let mut rom = create_rom();

        // Write a pointer that points outside of the ROM
        let pointer: RomPointer = RomPointer::NoData(0x100);
        assert_eq!(
            rom.write(0, pointer),
            Err(RomIoError::WritingOutOfBoundsOffset(0x100))
        );

        // Write an invalid pointer
        let pointer: RomPointer = RomPointer::Invalid(0x1234567);
        assert_eq!(
            rom.write(0, pointer),
            Err(RomIoError::WritingInvalidPointer(0, 0x1234567))
        );
    }

    #[test]
    fn recursive_offsets() {
        let mut rom = create_rom();

        let inner = RomPointer::<u32>::Valid(0x8, 0x12345678);
        let outer = RomPointer::Valid(0x4, inner);

        rom.write(0, outer).unwrap();

        assert_eq!(rom.read_word(0x0).unwrap(), 0x08_000_004);
        assert_eq!(rom.read_word(0x4).unwrap(), 0x08_000_008);
        assert_eq!(rom.read_word(0x8).unwrap(), 0x12345678);

        // Read outer from rom
        let outer: RomPointer<RomPointer<u32>> = RomPointer::read_from(&rom, 0x0).unwrap();
        assert_eq!(
            outer,
            RomPointer::Valid(0x4, RomPointer::Valid(0x8, 0x12345678))
        );

        // Read a void pointer from rom
        let outer: RomPointer<RomPointer> = RomPointer::read_from(&rom, 0x0).unwrap();
        assert_eq!(outer, RomPointer::Valid(0x4, RomPointer::NoData(0x8)));
    }

    #[test]
    fn test_json_serialize() {
        // A pointer of each type
        let pointer = RomPointer::Valid(0x4, 123);
        let pointer_no_data: RomPointer = RomPointer::NoData(0x4);
        let pointer_null: RomPointer = RomPointer::Null;
        let pointer_invalid: RomPointer = RomPointer::Invalid(123);

        // Serialize each pointer
        let pointer_json = serde_json::to_string(&pointer).unwrap();
        let pointer_no_data_json = serde_json::to_string(&pointer_no_data).unwrap();
        let pointer_null_json = serde_json::to_string(&pointer_null).unwrap();
        let pointer_invalid_json = serde_json::to_string(&pointer_invalid).unwrap();

        // Assert that the serialized pointers are correct
        assert_eq!(pointer_json, r#"{"offset":4,"data":123}"#);
        assert_eq!(pointer_no_data_json, r#"{"offset":4}"#);
        assert_eq!(pointer_null_json, r#"0"#);
        assert_eq!(pointer_invalid_json, r#"123"#);
    }

    #[test]
    fn test_json_deserialize() {
        let pointer_json = r#"{"offset":4,"data":123}"#;
        let pointer_no_data_json = r#"{"offset":4}"#;
        let pointer_null_json = r#"0"#;
        let pointer_invalid_json = r#"123"#;

        // Deserialize each pointer
        let pointer: RomPointer<u32> = serde_json::from_str(pointer_json).unwrap();
        let pointer_no_data: RomPointer = serde_json::from_str(pointer_no_data_json).unwrap();
        let pointer_null: RomPointer = serde_json::from_str(pointer_null_json).unwrap();
        let pointer_invalid: RomPointer = serde_json::from_str(pointer_invalid_json).unwrap();

        // Assert that the deserialized pointers are correct
        assert_eq!(pointer, RomPointer::Valid(0x4, 123));
        assert_eq!(pointer_no_data, RomPointer::NoData(0x4));
        assert_eq!(pointer_null, RomPointer::Null);
        assert_eq!(pointer_invalid, RomPointer::Invalid(123));
    }
}
