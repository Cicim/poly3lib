use std::collections::HashMap;

const ROM: u32 = 0x08_000_000;

/// Finds all offsets contained within the given data,
/// returning an hashmap with the offset as key and the
/// number of times it was found as value.
pub fn find_all_offsets(data: &[u8]) -> HashMap<u32, u32> {
    let mut map = HashMap::new();

    let start_offset = 0;
    let offset = start_offset;

    // Read the first three bytes, then
    let mut b1 = data[offset];
    let mut b2 = data[offset + 1];
    let mut b3 = data[offset + 2];

    for i in 3..data.len() {
        let b4 = data[offset + i];

        if b4 == 0x08 || b4 == 0x09 {
            // Compose the whole u32 (little endian)
            let ptr_bytes = [b1, b2, b3, b4];
            let ptr = u32::from_le_bytes(ptr_bytes);

            if ptr >= ROM && ptr <= ROM + data.len() as u32 {
                let offset = ptr - ROM;
                // Increse the counter for that offset after subtr
                let counter = map.entry(offset).or_insert(0);
                *counter += 1;
            }
        }

        // Shift the bytes
        b1 = b2;
        b2 = b3;
        b3 = b4;
    }

    map
}

/// Finds all references to the given offset within the given data,
/// returning a vector with the offsets of the references.
pub fn find_references(data: &[u8], offset: usize, alignment: usize) -> Vec<usize> {
    let mut references = Vec::new();

    // Make sure the pointer is valid
    if offset > data.len() {
        return references;
    }

    // Convert the given pointer to a bytearray
    let pointer: u32 = offset as u32 + ROM;
    let pointer: [u8; 4] = pointer.to_le_bytes();

    // Search for the pointer in the ROM
    for reference in (0..data.len() - 4).step_by(alignment) {
        if data[reference..reference + 4] == pointer {
            references.push(reference);
        }
    }

    references
}

/// Find a free offset in the ROM of the given size.
pub fn find_free_space(data: &[u8], size: usize, align: usize) -> Option<usize> {
    let mut offset = 0;

    // If no data of that size can fit in this ROM
    if size + offset >= data.len() {
        return None;
    }

    'outer: while offset <= data.len() - size {
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

/// Find out if the data needs a new place in ROM and if so, find it.
/// Return the offset of the data in ROM, whether it changed or not.
/// In case everything succeeds, clear all the old data.
pub fn repoint_offset(data: &mut [u8], offset: usize, old: usize, new: usize) -> Option<usize> {
    if old == new {
        // We can just overwrite it
        return Some(offset);
    }

    // If the data is smaller clear everything, then we can just overwrite it
    if old > new {
        data[offset + new..offset + old].fill(0xFF);
        return Some(offset);
    }

    // The data is larger, so we need to find a new place for it
    data[offset..offset + old].fill(0xFF);
    let new_offset = find_free_space(&data, new, 4)?;
    Some(new_offset)
}

/// Find the next occurrence of a byte after the given offset.
pub fn find_byte_after(data: &[u8], offset: usize, byte: u8) -> Option<usize> {
    for i in offset..data.len() {
        if data[i] == byte {
            return Some(i);
        }
    }
    None
}

// TODO Move lz77 functions to this module
