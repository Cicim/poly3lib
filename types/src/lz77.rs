use crate::GBAIOError;

// An unreasonable size for a decompressed size of a GBA graphics
const MAX_DECOMPRESSED_SIZE: u32 = 0x080000;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Lz77ReadingError {
    InvalidOffset(usize),
    InvalidMagic,
    InvalidLength,
    LookupOffsetOutOfBounds,
    UnexpectedEndOfInput,
}

/// Reads an Lz77 compressed buffer from ROM.
pub fn read_lz77(data: &[u8], offset: usize) -> Result<Vec<u8>, GBAIOError> {
    // Read the header
    let length = lz77_read_header(data, offset)?;

    // Decompress the buffer
    let buffer = &data[offset + 4..];
    let decompressed = lz77_decompress(buffer, length)?;

    Ok(decompressed)
}

/// Reads the Lz77 header from ROM.
pub fn lz77_read_header(data: &[u8], offset: usize) -> Result<usize, GBAIOError> {
    if offset + 4 > data.len() {
        return Err(GBAIOError::Lz77Error(Lz77ReadingError::InvalidOffset(
            offset,
        )));
    }
    // Read the 32-bit header
    let header = &data[offset..offset + 4];

    let magic = header[0];
    let length = (header[1] as u32) | ((header[2] as u32) << 8) | ((header[3] as u32) << 16);

    if magic != 0x010 {
        return Err(GBAIOError::Lz77Error(Lz77ReadingError::InvalidMagic));
    }
    if length > MAX_DECOMPRESSED_SIZE {
        return Err(GBAIOError::Lz77Error(Lz77ReadingError::InvalidLength));
    }

    Ok(length as usize)
}

/// Decompresses an Lz77 compressed buffer.
pub fn lz77_decompress(data: &[u8], inflated_size: usize) -> Result<Vec<u8>, GBAIOError> {
    // Initialize the destination buffer
    let mut output: Vec<u8> = Vec::with_capacity(inflated_size);
    let mut src_offset = 0;

    while output.len() < inflated_size {
        // Read the flags
        let flags = data[src_offset];
        src_offset += 1;

        let mut flag_index = 0u8;
        while flag_index < 8 && output.len() < inflated_size {
            let look_back = flags & (0x080 >> flag_index) != 0;

            // If you have to look for the data in the bit set
            if look_back {
                let count = (data[src_offset] >> 4) as usize + 3;
                let disp = u16::from_be_bytes([data[src_offset], data[src_offset + 1]]) & 0x0FFF;
                let backwards_offset = disp as usize + 1;
                src_offset += 2;

                for _ in 0..count {
                    let index = match output.len().checked_sub(backwards_offset) {
                        Some(index) => index,
                        None => {
                            return Err(GBAIOError::Lz77Error(
                                Lz77ReadingError::LookupOffsetOutOfBounds,
                            ));
                        }
                    };

                    let data = output[index];
                    output.push(data);
                }
            } else {
                // Read the data
                let data = data.get(src_offset).copied().ok_or(GBAIOError::Lz77Error(
                    Lz77ReadingError::UnexpectedEndOfInput,
                ))?;
                output.push(data);
                src_offset += 1;
            }

            flag_index += 1;
        }
    }

    Ok(output)
}

/// Gets the size of an Lz77 compressed buffer (after the header)
///
/// # Panics
/// If the decompression algorithm ends up reading out of bounds.
/// So, it assumes that the data can be decompressed.
pub fn lz77_get_deflated_size(data: &[u8], inflated_size: usize) -> usize {
    // Initialize the destination buffer
    let mut output_size = 0;
    let mut src_offset = 0;

    while output_size < inflated_size {
        // Read the flags
        let mut flag_index = 0u8;
        let flags = data[src_offset];
        src_offset += 1;

        while flag_index < 8 && output_size < inflated_size {
            if flags & (0x080 >> flag_index) != 0 {
                src_offset += 2;
                output_size += (data[src_offset] >> 4) as usize + 3;
            } else {
                src_offset += 1;
                output_size += 1;
            }
            flag_index += 1;
        }
    }

    // Align the source offset to four bytes
    if src_offset % 4 != 0 {
        src_offset += 4 - (src_offset % 4);
    }

    src_offset as usize
}

pub struct Lz77Options {
    /// The maximum number of bytes to search backwards for a match.
    pub max_search_distance: usize,
    /// The maximum number of bytes to copy in a single match.
    pub max_match_length: usize,
}

impl Lz77Options {
    pub fn new(max_search_dist: usize, max_match_length: usize) -> Self {
        if max_match_length > 18 {
            panic!("max_match_length must be less than 0x012");
        }

        Self {
            max_search_distance: max_search_dist,
            max_match_length,
        }
    }

    pub fn most_compression() -> Self {
        Self::new(0x0800, 0x012)
    }

    pub fn fastest() -> Self {
        Self::new(0x0100, 0x012)
    }
}

/// Compresses a buffer using the Lz77 algorithm.
///
/// Takes a buffer and a set of options.
pub fn lz77_compress(src_buffer: &[u8], options: &Lz77Options) -> Vec<u8> {
    let input_length = src_buffer.len();
    let mut dst_buffer = Vec::with_capacity(input_length);

    let mut input_bytes_processed = 0usize;
    let mut last_flag_position = 0usize;
    let mut num_flag_bits = 0;
    let mut flags = 0u8;

    while input_bytes_processed < input_length {
        if num_flag_bits == 0 {
            last_flag_position = dst_buffer.len();
            dst_buffer.push(0);
        }

        if find_token_with_length(
            src_buffer,
            input_bytes_processed,
            input_length,
            3,
            options.max_search_distance,
        ) != 0
        {
            let mut token_size = 0;
            let mut token_delta = 0;

            for attempt_token_size in 3..=options.max_match_length {
                let attempt_token_delta = find_token_with_length(
                    src_buffer,
                    input_bytes_processed,
                    input_length,
                    attempt_token_size,
                    options.max_search_distance,
                );

                if attempt_token_delta == 0 {
                    break;
                }

                token_size = attempt_token_size;
                token_delta = attempt_token_delta;
            }

            let flipped_token_delta = -token_delta - 1;

            let byte =
                (((token_size as i32 - 3) & 0x0f) << 4) | ((flipped_token_delta & 0x0f00) >> 8);
            dst_buffer.push(byte as u8);

            let byte = (flipped_token_delta & 0x0ff) as u8;
            dst_buffer.push(byte);
            input_bytes_processed += token_size;
            // Set the flag bit
            flags |= 0x080 >> num_flag_bits;
            dst_buffer[last_flag_position] = flags;
        } else {
            let byte = src_buffer[input_bytes_processed];
            dst_buffer.push(byte);
            input_bytes_processed += 1;
            // Don't set the flag bit
        }

        num_flag_bits += 1;
        if num_flag_bits == 8 || input_bytes_processed == input_length {
            num_flag_bits = 0;
            dst_buffer[last_flag_position] = flags;
            flags = 0;
        }
    }

    while dst_buffer.len() % 4 != 0 {
        dst_buffer.push(0);
    }

    dst_buffer
}

fn find_token_with_length(
    buffer: &[u8],
    offset: usize,
    input_length: usize,
    length: usize,
    max_search_dist: usize,
) -> i32 {
    if offset + length > input_length {
        return 0;
    }

    for pos in (0.max(offset as i32 - max_search_dist as i32)..=offset as i32 - 8).rev() {
        let delta: i32 = pos as i32 - offset as i32;

        if delta < -0x01000 {
            return 0;
        }

        let mut found = true;
        for i in 0..length {
            if buffer[pos as usize + i] != buffer[offset + i] {
                found = false;
                break;
            }
        }
        if found {
            return delta;
        }
    }

    0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn compress() {
        // Compress a buffer and make sure it decompresses to the same thing
        let decompressed = &b"Hello, world, this is a test string!"[..];

        let compressed = lz77_compress(&decompressed, &Lz77Options::most_compression());
        let decompressed2 = lz77_decompress(&compressed, decompressed.len()).unwrap();
        assert_eq!(decompressed, decompressed2);
    }
}
