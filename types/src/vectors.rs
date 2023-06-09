use fast_ops::{find_free_space, repoint_offset};
use serde::{de, ser::SerializeStruct, Deserialize, Serialize};

use crate::{GBAIOError, GBAType};

#[derive(Clone, Default, PartialEq, Eq, Hash)]
pub enum VectorData<T: GBAType> {
    #[default]
    Null,
    Invalid(u32),
    Valid {
        offset: u32,
        data: Vec<T>,
        read_length: usize,
    },
    Clear {
        offset: u32,
        read_length: usize,
    },
    New(Vec<T>),
}

// Serialization rules for VectorData:
// - Any invalid pointer is serialized as u32
//   (so NULL is serialized as 0)
// - If the vector has been correctly read from ROM, it will be serialized as the object:
//   {offset: number, data: [T], read_length: number}
// - If the vector needs to be cleared by the user, they just have to delete the data field
//   and the offset and read_length fields will be used to clear the vector
// - If the vector has been created by the user, it will be serialized as the object:
//   {data: [T]}
//   And it will be written to ROM at the first available offset
impl<T: GBAType + Serialize> Serialize for VectorData<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            VectorData::Null => serializer.serialize_u32(0),
            VectorData::Invalid(offset) => serializer.serialize_u32(*offset),
            VectorData::Valid {
                offset,
                data,
                read_length,
            } => {
                let mut state = serializer.serialize_struct("Valid", 3)?;
                state.serialize_field("offset", offset)?;
                state.serialize_field("data", data)?;
                state.serialize_field("read_length", read_length)?;
                state.end()
            }
            VectorData::Clear {
                offset,
                read_length,
            } => {
                let mut state = serializer.serialize_struct("Clear", 2)?;
                state.serialize_field("offset", offset)?;
                state.serialize_field("read_length", read_length)?;
                state.end()
            }
            VectorData::New(data) => {
                let mut state = serializer.serialize_struct("New", 1)?;
                state.serialize_field("data", data)?;
                state.end()
            }
        }
    }
}

impl<'de, T: GBAType> Deserialize<'de> for VectorData<T>
where
    T: for<'a> Deserialize<'a>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct VectorDataVisitor<T>(std::marker::PhantomData<T>);

        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "snake_case")]
        enum Fields {
            Offset,
            Data,
            ReadLength,
        }

        // Visitor for PointedData
        impl<'de, T: GBAType> serde::de::Visitor<'de> for VectorDataVisitor<T>
        where
            T: for<'a> Deserialize<'a>,
        {
            type Value = VectorData<T>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a valid JSON representation of a VectorData")
            }

            fn visit_u64<D>(self, value: u64) -> Result<Self::Value, D>
            where
                D: de::Error,
            {
                if value == 0 {
                    Ok(VectorData::Null)
                } else {
                    Ok(VectorData::Invalid(value as u32))
                }
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: de::MapAccess<'de>,
            {
                let mut offset: Option<u32> = None;
                let mut data: Option<Vec<T>> = None;
                let mut read_length: Option<usize> = None;

                // Deserialize the map
                while let Some(key) = map.next_key()? {
                    match key {
                        Fields::Data => {
                            if data.is_some() {
                                return Err(de::Error::duplicate_field("data"));
                            }
                            data = Some(map.next_value()?);
                        }
                        Fields::Offset => {
                            if offset.is_some() {
                                return Err(de::Error::duplicate_field("offset"));
                            }
                            offset = Some(map.next_value()?);
                        }
                        Fields::ReadLength => {
                            if read_length.is_some() {
                                return Err(de::Error::duplicate_field("read_length"));
                            }
                            read_length = Some(map.next_value()?);
                        }
                    }
                }

                match (offset, data, read_length) {
                    // If all fields are present
                    (Some(offset), Some(data), Some(read_length)) => Ok(VectorData::Valid {
                        offset,
                        data,
                        read_length,
                    }),
                    // If only data is present
                    (None, Some(data), None) => Ok(VectorData::New(data)),
                    // If only offset and read_length are present
                    (Some(offset), None, Some(read_length)) => Ok(VectorData::Clear {
                        offset,
                        read_length,
                    }),

                    _ => Err(de::Error::missing_field(
                        "Invalid field combination when parsing VectorData",
                    )),
                }
            }
        }

        deserializer.deserialize_any(VectorDataVisitor::<T>(std::marker::PhantomData))
    }
}

impl<T: GBAType> VectorData<T> {
    /// Returns the data read from the vector, if it exists
    pub fn get_data(&self) -> Option<&Vec<T>> {
        match self {
            VectorData::Valid { data, .. } => Some(data),
            VectorData::New(data) => Some(data),
            _ => None,
        }
    }

    /// Transforms this [`VectorData`] into another [`VectorData`]
    /// that can be used to clear the vector when writing to ROM.
    ///
    /// If this [`VectorData`] was not writable, it simply returns `None`.
    pub fn to_clear(&mut self) {
        use VectorData::*;

        *self = match self {
            // For Valids, just remove the data
            Valid {
                offset,
                read_length,
                ..
            }
            | Clear {
                offset,
                read_length,
            } => Clear {
                offset: *offset,
                read_length: *read_length,
            },
            // Everything else, ignore the dereferencing and just
            // consider it a NULL pointer
            _ => Null,
        }
    }
}

impl<T: GBAType> std::fmt::Debug for VectorData<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use VectorData::*;

        write!(f, "VectorData(")?;
        match self {
            Null => write!(f, "NULL"),
            Invalid(pointer) => write!(f, "Invalid(pointer 0x{:08X})", pointer),

            Clear {
                offset,
                read_length,
            } => write!(f, "clear offset 0x{:08X}, length {}", offset, read_length),

            Valid {
                offset,
                read_length,
                data,
            } => write!(
                f,
                "valid offset 0x{:08X}, length {}, data {:#?}",
                offset, read_length, data
            ),
            New(data) => write!(f, "new data {:#?}", data),
        }?;
        write!(f, ")")
    }
}

/// Reads a [`VectorData`] from of the given length starting from
/// the given offset in the bytes.
///
/// This function is called internally by all structs constructed
/// using `gba_struct!` that contain a vector field.
pub fn read_vector<T: GBAType>(
    bytes: &[u8],
    offset: usize,
    read_length: usize,
) -> Result<VectorData<T>, GBAIOError> {
    // Read the pointer at the offset
    let pointer = u32::read_from(bytes, offset)?;
    // If the pointer is null, return a null vector
    if pointer == 0 {
        return Ok(VectorData::Null);
    }

    // Make sure the pointer is valid
    if pointer < 0x08000000 || pointer >= 0x08000000 + bytes.len() as u32 {
        return Ok(VectorData::Invalid(pointer));
    }
    let pointer = pointer - 0x08000000;

    // Read the data at the pointer
    let mut data = Vec::with_capacity(read_length);
    for i in 0..read_length {
        let field = T::read_from(bytes, pointer as usize + i * T::SIZE)?;
        data.push(field);
    }

    // Return the data
    Ok(VectorData::Valid {
        offset: pointer,
        data,
        read_length,
    })
}

/// Writes a [`VectorData`] to the given bytes at the given offset.
///
/// This function is called internally by all structs constructed
/// using `gba_struct!` that contain a vector field.
pub fn write_vector<T: GBAType>(
    bytes: &mut [u8],
    offset: usize,
    data: &VectorData<T>,
) -> Result<(), GBAIOError> {
    match data {
        VectorData::Null => {
            // If the vector is null, write a null pointer
            u32::write_to(&0, bytes, offset)?;
        }
        VectorData::Invalid(pointer) => {
            // If the vector is invalid, refuse to write it
            return Err(GBAIOError::WritingInvalidPointer(*pointer as u32));
        }
        VectorData::Clear {
            offset: pointer,
            read_length,
        } => {
            // Empty the vector at the offset
            let pointer = *pointer as usize;
            bytes[pointer..pointer + read_length * T::SIZE].fill(0xFF);

            u32::write_to(&0, bytes, offset)?;
        }
        VectorData::Valid {
            offset: pointer,
            data: vector,
            read_length,
        } => {
            // Compare the sizes to check if you have to repoint the vector
            let old_size = *read_length * T::SIZE;
            let new_size = vector.len() * T::SIZE;

            // Replace the pointer with null to avoid it being
            // used as free space for repointing
            u32::write_to(&0, bytes, offset)?;

            // Repoint the vector if necessary
            let pointer = repoint_offset(bytes, *pointer as usize, old_size, new_size)
                .ok_or(GBAIOError::RepointingError)?;

            // Write the pointer to the vector
            u32::write_to(&(pointer as u32 + 0x08000000), bytes, offset)?;

            // Write the data to the vector
            for (i, field) in vector.iter().enumerate() {
                field.write_to(bytes, pointer + i * T::SIZE)?;
            }
        }
        VectorData::New(data) => {
            // Replace the pointer with null to avoid it being
            // used as free space for repointing
            u32::write_to(&0, bytes, offset)?;

            // Find a free space for the vector
            let pointer = find_free_space(bytes, data.len() * T::SIZE, 4)
                .ok_or(GBAIOError::RepointingError)?;

            // Write the pointer to the vector
            u32::write_to(&(pointer as u32 + 0x08000000), bytes, offset)?;

            // Write the data to the vector
            for (i, field) in data.iter().enumerate() {
                field.write_to(bytes, pointer + i * T::SIZE)?;
            }
        }
    }

    Ok(())
}
