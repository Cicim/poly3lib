use crate::{GBAIOError, GBAType};

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub enum VectorData<T: GBAType> {
    #[default]
    Null,
    Invalid(usize),
    Clear {
        offset: usize,
        read_length: usize,
    },
    Valid {
        offset: usize,
        data: Vec<T>,
        read_length: usize,
    },
    New(Vec<T>),
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

    pub fn to_clear(&self) -> Option<VectorData<T>> {
        match self {
            VectorData::Valid {
                offset,
                read_length,
                ..
            } => Some(VectorData::Clear {
                offset: *offset,
                read_length: *read_length,
            }),
            _ => None,
        }
    }
}

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
        return Ok(VectorData::Invalid(pointer as usize));
    }
    let pointer = pointer as usize - 0x08000000;

    // Read the data at the pointer
    let mut data = Vec::with_capacity(read_length);
    for i in 0..read_length {
        let field = T::read_from(bytes, pointer + i * T::SIZE)?;
        data.push(field);
    }

    // Return the data
    Ok(VectorData::Valid {
        offset: pointer,
        data,
        read_length,
    })
}

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
            bytes[*pointer..*pointer + read_length * T::SIZE].fill(0xFF);

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
            let pointer = repoint_offset(bytes, *pointer, old_size, new_size)
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

/// Find a free offset in the ROM of the given size.
pub fn find_free_space(data: &[u8], size: usize, align: usize) -> Option<usize> {
    let mut offset = 0;

    if size - offset >= data.len() {
        return None;
    }

    'outer: while offset < data.len() - size {
        // If this was a possible free space, but the last bit was not 0xFF,
        // then we need to skip ahead because no possible sub-window could
        // be free.
        if data[offset + size - 1] != 0xFF {
            offset += size;
            continue;
        }

        // The window ends with 0xFF
        // Check if the window is free
        for i in 0..size {
            if data[offset + i] != 0xFF {
                // An 0xFF was found in the middle of the window
                // We can keep looking right after it (aligned)
                offset += align.max(i);
                if offset % align != 0 {
                    offset += align - (offset % align);
                }
                continue 'outer;
            }
        }

        return Some(offset);
    }

    None
}

/// Discover if the data needs a new place in ROM and if so, find it.
/// Return the offset of the data in ROM, whether it changed or not.
/// In case everything succeeds, clear all the old data.
pub fn repoint_offset(
    data: &mut [u8],
    offset: usize,
    old_size: usize,
    new_size: usize,
) -> Option<usize> {
    if old_size == new_size {
        // We can just overwrite it
        return Some(offset);
    }

    // If the data is smaller clear everything, then we can just overwrite it
    if old_size > new_size {
        data[offset + new_size..offset + old_size].fill(0xFF);
        return Some(offset);
    }

    // The data is larger, so we need to find a new place for it
    data[offset..offset + old_size].fill(0xFF);
    let new_offset = find_free_space(&data, new_size, 4)?;
    Some(new_offset)
}
