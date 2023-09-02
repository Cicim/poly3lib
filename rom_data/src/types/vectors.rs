//! Declares a [`RomVector<T>`] type that can be read as a dynamically sized
//! vector of `T` from the ROM.

use serde::{ser::SerializeStruct, Deserialize, Serialize, Serializer};

use crate::{Offset, Pointer, RomData, RomIoError};

use super::{RomPointer, RomReadableType, RomSizedType, RomWritableType};

/// A pointer to an array of `T` in the ROM with its size.
///
/// The main difference from [`RomPointer<RomArray>`][super::RomPointer] is that
/// the size of this vector does not need to be known at compile time.
///
/// When read correctly, this type will store the length it was when it was read.
/// This way it can be modified and written back to the ROM without having to
/// read the length again.
#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub enum RomVector<T> {
    /// The vector has no data attached to it. In this case, the vector
    /// has both a null pointer and is read with a length of 0.
    #[default]
    Null,

    /// The vector either is an invalid pointer or was a null pointer
    /// but was read with a length greater than 0.
    Invalid(Pointer),

    /// The vector is valid and has data attached to it.
    Valid {
        /// The offset where the array starts.
        offset: Offset,
        /// The length of the array when it was read.
        clear_size: usize,
        /// The data of the array.
        data: Vec<T>,
    },

    /// This variant is built exclusively when the array needs to be cleared.
    ///
    /// It is not possible to read a vector with this variant.
    ///
    /// This variant provides a valid offset and clear size for the vector
    /// to be cleared, so that it can be cleared without having to read
    /// the vector first.
    Clear { offset: Offset, clear_size: usize },

    /// This variant is only built when a new vector is created.
    ///
    /// It is not possible to read a vector with this variant.
    ///
    /// This variant provides the data to write to the ROM, but not the offset
    /// where it should be written. This is because the offset is not known
    /// until the vector is written to the ROM.
    New(Vec<T>),
}

impl<T> RomVector<T> {
    /// Creates a new vector with the given data.
    pub fn new(data: Vec<T>) -> Self {
        Self::New(data)
    }

    /// Transforms this [`RomVector`] into another [`RomVector`]
    /// that can be used to clear the vector when writing to ROM.
    pub fn to_clear(&mut self) {
        *self = match self {
            // If it has a clear size and an offset, convert it to a clear vector
            RomVector::Valid {
                offset, clear_size, ..
            }
            | RomVector::Clear {
                offset, clear_size, ..
            } => RomVector::Clear {
                offset: *offset,
                clear_size: *clear_size,
            },

            // Otherwise, set it to null
            _ => RomVector::Null,
        };
    }

    /// Returns an offset read by this pointer only if it is valid.
    pub fn offset(&self) -> Option<Offset> {
        match self {
            RomVector::Valid { offset, .. } => Some(*offset),
            RomVector::Clear { offset, .. } => Some(*offset),
            _ => None,
        }
    }

    /// Returns a reference to the data read by this pointer only if it exists.
    pub fn data(&self) -> Option<&Vec<T>> {
        match self {
            RomVector::Valid { data, .. } => Some(data),
            RomVector::New(data) => Some(data),
            _ => None,
        }
    }

    /// Returns the clear length of the vector only if it is valid.
    pub fn clear_size(&self) -> Option<usize> {
        match self {
            RomVector::Valid { clear_size, .. } => Some(*clear_size),
            RomVector::Clear { clear_size, .. } => Some(*clear_size),
            _ => None,
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for RomVector<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RomVector::Null => write!(f, "Null"),
            RomVector::Invalid(ptr) => write!(f, "Invalid(0x{:08x})", ptr),
            RomVector::Valid {
                offset,
                clear_size,
                data,
            } => write!(f, "Vector(${:07X}, {}, {:#?})", offset, clear_size, data),
            RomVector::Clear { offset, clear_size } => {
                write!(f, "Vector(clear ${:07X}, {}", offset, clear_size)
            }
            RomVector::New(data) => write!(f, "Vector(missing offset, {:#?})", data),
        }
    }
}

// ANCHOR Read method
impl<T: RomReadableType + RomSizedType> RomVector<T> {
    /// Reads a vector from the given offset in the ROM, with the given initial size.
    pub fn read_from(rom: &RomData, offset: Offset, clear_size: usize) -> Result<Self, RomIoError> {
        // Read the pointer at the given location
        let offset: RomPointer = rom.read(offset)?;

        match offset {
            // If the pointer is null, return according to the rules
            RomPointer::Null if clear_size == 0 => Ok(RomVector::Null),
            RomPointer::Null => Ok(RomVector::Invalid(0)),
            RomPointer::Invalid(read_ptr) => Ok(RomVector::Invalid(read_ptr)),

            // RomPointer::Valid should be unreachable when reading a void pointer,
            // but we'll combine the match arms for the sake of exhaustiveness
            RomPointer::NoData(offset) | RomPointer::Valid(offset, _) => {
                let data_size = T::get_size(rom);

                let mut data = Vec::new();
                let mut curr_offset = offset;

                for _ in 0..clear_size {
                    data.push(rom.read(curr_offset)?);
                    curr_offset += data_size;
                }

                Ok(RomVector::Valid {
                    offset,
                    data,
                    clear_size,
                })
            }
        }
    }
}

// ANCHOR RomType impls
impl<T> RomSizedType for RomVector<T> {
    fn get_size(_: &RomData) -> usize {
        4
    }
    fn get_alignment(_: &RomData) -> usize {
        4
    }
}

impl<T: RomWritableType + RomSizedType> RomWritableType for RomVector<T> {
    fn write_to(self, rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        match self {
            // If we are writing null it's because we either read null or we
            // want to write NULL without touching any previously referenced data.
            RomVector::Null => rom.write_word(offset, 0),

            // Just like with RomPointers, writing invalid pointers results in an error.
            RomVector::Invalid(pointer) => Err(RomIoError::WritingInvalidPointer(offset, pointer)),

            // If the type is clear, we just have to clear the data and the pointer
            RomVector::Clear {
                offset: data_offset,
                clear_size,
            } => {
                // Clear the data
                let type_size = T::get_size(rom);
                let data_size = type_size * clear_size;
                rom.clear_bytes(data_offset, data_size)?;
                // Clear the pointer to the data
                rom.write_word(offset, 0)
            }

            // If the type is valid, we have to repoint if needed.
            RomVector::Valid {
                offset: data_offset,
                clear_size,
                data,
            } => {
                let type_size = T::get_size(rom);

                let old_size = clear_size * type_size;
                let new_size = data.len() * type_size;

                // Set the pointer to null to avoid it being used as free space
                rom.write_word(offset, 0)?;
                // Repoint the vector if necessary
                let data_offset = rom.repoint_offset(data_offset, old_size, new_size)?;

                // Write the data to the vector
                let mut curr_offset = data_offset;
                for field in data {
                    field.write_to(rom, curr_offset)?;
                    curr_offset += type_size;
                }

                // Write the pointer to the vector
                rom.write_offset(offset, data_offset)
            }

            // If the vector has never been written to ROM, we have to find a new
            // space for it and write the data there and the space's offset here.
            RomVector::New(data) => {
                let type_size = T::get_size(rom);
                let data_size = type_size * data.len();

                // Set the pointer to null to avoid it being used as free space
                rom.write_word(offset, 0)?;

                let data_offset = rom.find_free_space(data_size, 4)?;

                let mut curr_offset = data_offset;
                // Write the data to the vector
                for field in data {
                    field.write_to(rom, curr_offset)?;
                    curr_offset += type_size;
                }

                // Write the pointer to the vector
                rom.write_offset(offset, data_offset)
            }
        }
    }
}

// ANCHOR Serialize and Deserialize methods
impl<T: Serialize> Serialize for RomVector<T> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            // NULL is serialized as 0
            RomVector::Null => serializer.serialize_u32(0),

            // Any invalid vector is serialized as the pointer itself
            RomVector::Invalid(ptr) => serializer.serialize_u32(*ptr),

            // A valid vector is serialized as a struct with three fields:
            // RomVector {
            //   offset: u32,               // Number in JSON
            //   clear_size: u32,           // Number in JSON
            //   data: [T],
            // }
            RomVector::Valid {
                offset,
                data,
                clear_size,
            } => {
                let mut state: _ = serializer.serialize_struct("RomVector", 3)?;
                state.serialize_field("offset", offset)?;
                state.serialize_field("clear_size", clear_size)?;
                state.serialize_field("data", data)?;
                state.end()
            }

            // A clear vector is serialized as a struct with two fields:
            // RomVector {
            //   offset: u32,               // Number in JSON
            //   clear_size: u32,           // Number in JSON
            // }
            RomVector::Clear { offset, clear_size } => {
                let mut state: _ = serializer.serialize_struct("RomVector", 2)?;
                state.serialize_field("offset", offset)?;
                state.serialize_field("clear_size", clear_size)?;
                state.end()
            }

            // A new vector is serialized as a struct with one field:
            // RomVector {
            //   data: [T],
            // }
            RomVector::New(data) => {
                let mut state: _ = serializer.serialize_struct("RomVector", 1)?;
                state.serialize_field("data", data)?;
                state.end()
            }
        }
    }
}

impl<'de, T: for<'a> Deserialize<'a>> Deserialize<'de> for RomVector<T> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "snake_case")]
        enum Fields {
            Offset,
            Data,
            ClearSize,
        }

        // Visitor for PointedData
        impl<'de, T: for<'a> Deserialize<'a>> serde::de::Visitor<'de> for RomVector<T> {
            type Value = RomVector<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a valid JSON representation of a RomVector")
            }

            // A direct integer can be either a null pointer or an invalid vector
            fn visit_u64<D: serde::de::Error>(self, value: u64) -> Result<Self::Value, D> {
                if value == 0 {
                    Ok(RomVector::Null)
                } else {
                    Ok(RomVector::Invalid(value as u32))
                }
            }

            // A struct can be either a valid vector, a clear vector or a new vector
            fn visit_map<A: serde::de::MapAccess<'de>>(
                self,
                mut map: A,
            ) -> Result<Self::Value, A::Error> {
                let mut offset: Option<usize> = None;
                let mut data: Option<Vec<T>> = None;
                let mut clear_size: Option<usize> = None;

                // Deserialize the map
                while let Some(key) = map.next_key()? {
                    match key {
                        Fields::Data => {
                            if data.is_some() {
                                return Err(serde::de::Error::duplicate_field("data"));
                            }
                            data = Some(map.next_value()?);
                        }
                        Fields::Offset => {
                            if offset.is_some() {
                                return Err(serde::de::Error::duplicate_field("offset"));
                            }
                            offset = Some(map.next_value()?);
                        }
                        Fields::ClearSize => {
                            if clear_size.is_some() {
                                return Err(serde::de::Error::duplicate_field("clear_size"));
                            }
                            clear_size = Some(map.next_value()?);
                        }
                    }
                }

                match (offset, data, clear_size) {
                    // If all fields are present
                    (Some(offset), Some(data), Some(clear_size)) => Ok(RomVector::Valid {
                        offset,
                        data,
                        clear_size,
                    }),
                    // If only data is present
                    (None, Some(data), None) => Ok(RomVector::New(data)),
                    // If only offset and read_length are present
                    (Some(offset), None, Some(clear_size)) => {
                        Ok(RomVector::Clear { offset, clear_size })
                    }

                    _ => Err(serde::de::Error::missing_field(
                        "Invalid field combination when parsing RomVector",
                    )),
                }
            }
        }

        deserializer.deserialize_any(RomVector::Null)
    }
}

// ANCHOR Tests
#[cfg(test)]
mod test_vectors {
    use crate::{types::RomWritableType, RomData, RomIoError};

    use super::RomVector;

    fn create_rom() -> RomData {
        RomData::new(crate::RomBase::FireRed, 0x100)
    }

    fn create_vectors() -> [RomVector<u8>; 5] {
        [
            RomVector::Null,
            RomVector::Invalid(0xFFFF_FFFF),
            RomVector::Valid {
                offset: 4,
                clear_size: 0x10,
                data: vec![0; 0x10],
            },
            RomVector::Clear {
                offset: 4,
                clear_size: 0x10,
            },
            RomVector::New(vec![0; 0x10]),
        ]
    }

    fn expected_serializations() -> [&'static str; 5] {
        [
            "0",
            "4294967295",
            r#"{"offset":4,"clear_size":16,"data":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}"#,
            r#"{"offset":4,"clear_size":16}"#,
            r#"{"data":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]}"#,
        ]
    }

    #[test]
    fn test_offset() {
        let vectors = create_vectors();

        assert_eq!(vectors[0].offset(), None);
        assert_eq!(vectors[1].offset(), None);
        assert_eq!(vectors[2].offset(), Some(4));
        assert_eq!(vectors[3].offset(), Some(4));
        assert_eq!(vectors[4].offset(), None);
    }

    #[test]
    fn test_data() {
        let vectors = create_vectors();

        assert_eq!(vectors[0].data(), None);
        assert_eq!(vectors[1].data(), None);
        assert_eq!(vectors[2].data(), Some(&vec![0; 0x10]));
        assert_eq!(vectors[3].data(), None);
        assert_eq!(vectors[4].data(), Some(&vec![0; 0x10]));
    }

    #[test]
    fn test_clear_size() {
        let vectors = create_vectors();

        assert_eq!(vectors[0].clear_size(), None);
        assert_eq!(vectors[1].clear_size(), None);
        assert_eq!(vectors[2].clear_size(), Some(0x10));
        assert_eq!(vectors[3].clear_size(), Some(0x10));
        assert_eq!(vectors[4].clear_size(), None);
    }

    #[test]
    fn test_to_clear() {
        let mut vectors = create_vectors();

        vectors[0].to_clear();
        vectors[1].to_clear();
        vectors[2].to_clear();
        vectors[3].to_clear();
        vectors[4].to_clear();

        assert_eq!(vectors[0], RomVector::Null);
        assert_eq!(vectors[1], RomVector::Null);
        assert_eq!(
            vectors[2],
            RomVector::Clear {
                offset: 4,
                clear_size: 0x10,
            }
        );
        assert_eq!(
            vectors[3],
            RomVector::Clear {
                offset: 4,
                clear_size: 0x10,
            }
        );
        assert_eq!(vectors[4], RomVector::Null);
    }

    #[test]
    fn test_read_vector() {
        let mut rom = create_rom();
        let vectors = create_vectors();

        // Null vector
        rom.write_word(0, 0).unwrap();
        let null = RomVector::<u8>::read_from(&rom, 0, 0).unwrap();
        assert_eq!(null, vectors[0]);

        let invalid_null = RomVector::<u8>::read_from(&rom, 0, 1).unwrap();
        assert_eq!(invalid_null, RomVector::Invalid(0));

        // Invalid vector
        rom.write_word(0, 0xFFFF_FFFF).unwrap();
        let invalid = RomVector::<u8>::read_from(&rom, 0, 0).unwrap();
        assert_eq!(invalid, vectors[1]);

        let invalid = RomVector::<u8>::read_from(&rom, 0, 1).unwrap();
        assert_eq!(invalid, vectors[1]);

        // Valid vector
        rom.write_offset(0, 4).unwrap();
        rom.write_word(4, 0).unwrap();
        rom.write_word(8, 0).unwrap();
        rom.write_word(12, 0).unwrap();
        rom.write_word(16, 0).unwrap();
        let valid = RomVector::<u8>::read_from(&rom, 0, 16).unwrap();
        assert_eq!(valid, vectors[2]);

        // Try to read them with varying lengths
        let valid = RomVector::<u8>::read_from(&rom, 0, 0).unwrap();
        assert_eq!(valid.clear_size(), Some(0));

        let valid = RomVector::<u8>::read_from(&rom, 0, 1).unwrap();
        assert_eq!(valid.clear_size(), Some(1));

        let valid = RomVector::<u8>::read_from(&rom, 0, 2).unwrap();
        assert_eq!(valid.clear_size(), Some(2));

        // It is impossible to read clear and new vectors
    }

    #[test]
    fn test_write_vector() {
        // Write a null vector
        let mut rom = create_rom();
        let vector = RomVector::<u8>::Null;
        vector.write_to(&mut rom, 0).unwrap();
        assert_eq!(rom.read_word(0).unwrap(), 0);

        // Write an invalid vector
        let mut rom = create_rom();
        let vector = RomVector::<u8>::Invalid(0xFFFF_FFFF);
        assert_eq!(
            vector.write_to(&mut rom, 0),
            Err(RomIoError::WritingInvalidPointer(0, 0xFFFF_FFFF))
        );

        // Clear a vector
        let mut rom = create_rom();
        // Write something in place of the offset
        rom.write_word(0, 0x4242_4242).unwrap();
        // Write something in place of the data
        rom.write_word(4, 0).unwrap();
        let vector = RomVector::<u8>::Clear {
            offset: 4,
            clear_size: 4,
        };
        vector.write_to(&mut rom, 0).unwrap();

        assert_eq!(rom.read_word(0).unwrap(), 0);
        assert_eq!(rom.read_word(4).unwrap(), 0xFFFF_FFFF);

        // Write a new vector
        let mut rom = create_rom();
        let vector = RomVector::<u8>::New(vec![0xAA; 4]);
        vector.write_to(&mut rom, 0).unwrap();
        assert_eq!(rom.read_offset(0).unwrap(), 4);
        assert_eq!(rom.read_word(4).unwrap(), 0xAAAAAAAA);
    }

    #[test]
    fn test_write_new_vector() {
        let mut rom = create_rom();
        let vector = RomVector::<u8>::new(vec![0xAA; 4]);

        vector.write_to(&mut rom, 0).unwrap();

        assert_eq!(rom.read_offset(0).unwrap(), 4);
        assert_eq!(rom.read_word(4).unwrap(), 0xAAAAAAAA);
    }

    #[test]
    fn test_write_valid_vector() {
        // 1. Write a valid vector with the same size
        let mut rom = create_rom();
        let vector: RomVector<u8> = RomVector::Valid {
            offset: 4,
            clear_size: 4,
            data: vec![0xAA; 4],
        };
        vector.write_to(&mut rom, 0).unwrap();

        assert_eq!(rom.read_offset(0).unwrap(), 4);
        assert_eq!(rom.read_word(4).unwrap(), 0xAAAAAAAA);

        // 2. Write a valid vector with a smaller size
        let mut rom = create_rom();
        let vector: RomVector<u8> = RomVector::Valid {
            offset: 4,
            clear_size: 4,
            data: vec![0xAA; 2],
        };
        // Fill the old data with something to check if it cleared it
        rom.write_word(4, 0x4242_4242).unwrap();

        vector.write_to(&mut rom, 0).unwrap();

        assert_eq!(rom.read_offset(0).unwrap(), 4);
        assert_eq!(rom.read_halfword(4).unwrap(), 0xAAAA);
        assert_eq!(rom.read_halfword(6).unwrap(), 0xFFFF);

        // 3. Write a valid vector with a bigger size
        let mut rom = create_rom();
        let vector: RomVector<u8> = RomVector::Valid {
            offset: 4,
            clear_size: 4,
            data: vec![0xAA; 8],
        };
        // Write something at the end of where the old data was
        // so that you can test if the repointing works as expected
        // 04 00 00 08 01 02 03 04 FF FF FF 01 ...
        // ^---------^ ^---------^ ^---------^
        //   pointer    old_data    something
        rom.write_word(8, 0x01FFFFFF).unwrap();

        vector.write_to(&mut rom, 0).unwrap();

        assert_eq!(rom.read_offset(0).unwrap(), 12);
        assert_eq!(rom.read_word(12).unwrap(), 0xAAAAAAAA);
    }

    #[test]
    fn test_json_serialize() {
        let vectors = create_vectors();
        let ser = expected_serializations();

        assert_eq!(serde_json::to_string(&vectors[0]).unwrap(), ser[0]);
        assert_eq!(serde_json::to_string(&vectors[1]).unwrap(), ser[1]);
        assert_eq!(serde_json::to_string(&vectors[2]).unwrap(), ser[2]);
        assert_eq!(serde_json::to_string(&vectors[3]).unwrap(), ser[3]);
        assert_eq!(serde_json::to_string(&vectors[4]).unwrap(), ser[4]);
    }

    #[test]
    fn test_json_deserialize() {
        let vectors = create_vectors();
        let ser = expected_serializations();

        let deser: _ = |n: usize| -> RomVector<u8> { serde_json::from_str(ser[n]).unwrap() };

        assert_eq!(deser(0), vectors[0]);
        assert_eq!(deser(1), vectors[1]);
        assert_eq!(deser(2), vectors[2]);
        assert_eq!(deser(3), vectors[3]);
        assert_eq!(deser(4), vectors[4]);
    }
}
