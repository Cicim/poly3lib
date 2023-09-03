//! This module provides support for LZ77 compression and decompression.

use crate::RomIoError;
use thiserror::Error;

use crate::types::{RomClearableType, RomReadableType};
use crate::{Offset, RomData};

/// LZ77-compressed data read from ROM.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Lz77DecompressedData {
    /// Offset of the header for the compressed data
    pub offset: Offset,
    /// Data size after decompression
    pub inflated_size: usize,
    /// Data size before decompression (size of the compressed data in ROM)
    pub deflated_size: usize,

    /// Decompressed data
    pub data: Vec<u8>,
}

// LZ77 header as struct for easy reading
use crate as rom_data;
use struct_macro::rom_struct;
rom_struct!(Lz77Header {
    u32 inflated_size:24;
    u32 magic:8;
});

impl Lz77Header {
    /// Creates a [`Lz77Header`] starting from the inflated size.
    pub fn new(inflated_size: usize) -> Lz77Header {
        Lz77Header {
            magic: 0x10,
            inflated_size: inflated_size as u32,
        }
    }
}

/// Error type for LZ77 decompression.
#[derive(Debug, Error, PartialEq, Eq)]
pub enum Lz77DecompressionError {
    #[error("Invalid LZ77 magic: found {0:02X}, expected 0x10")]
    InvalidMagic(u8),
    #[error("Invalid decompressed length {0}, maximum is 524288")]
    InvalidInflatedLength(u32),
    #[error("Lookup offset out of bounds")]
    LookupOffsetOutOfBounds,
    #[error("Unexpected end of input")]
    UnexpectedEndOfInput,
}

// ANCHOR Lz77DecompressedData impl
impl Lz77DecompressedData {
    /// Decompresses data from the given buffer.
    ///
    /// Returns the decompressed (inflated) data and the number of bytes read from the buffer.
    fn decompress_from_buffer(
        buffer: &[u8],
        inflated_size: usize,
    ) -> Result<(Vec<u8>, usize), Lz77DecompressionError> {
        // Initialize the destination buffer
        let mut output: Vec<u8> = Vec::with_capacity(inflated_size);
        let mut src_offset = 0;

        while output.len() < inflated_size {
            // Read the flags
            let flags = buffer[src_offset];
            src_offset += 1;

            let mut flag_index = 0u8;
            while flag_index < 8 && output.len() < inflated_size {
                let look_back = flags & (0x080 >> flag_index) != 0;

                // If you have to look for the data in the bit set
                if look_back {
                    let count = (buffer[src_offset] >> 4) as usize + 3;
                    let disp =
                        u16::from_be_bytes([buffer[src_offset], buffer[src_offset + 1]]) & 0x0FFF;
                    let backwards_offset = disp as usize + 1;
                    src_offset += 2;

                    for _ in 0..count {
                        let index = match output.len().checked_sub(backwards_offset) {
                            Some(index) => index,
                            None => return Err(Lz77DecompressionError::LookupOffsetOutOfBounds),
                        };

                        let data = output[index];
                        output.push(data);
                    }
                } else {
                    // Read the data
                    let data = *buffer
                        .get(src_offset)
                        .ok_or(Lz77DecompressionError::UnexpectedEndOfInput)?;
                    output.push(data);
                    src_offset += 1;
                }

                flag_index += 1;
            }
        }

        Ok((output, src_offset))
    }

    /// Returns the size of the deflated (compressed) data in ROM given the buffer and
    /// inflated (decompressed) data size read from the header.
    pub fn get_deflated_size_from_buffer(
        buffer: &[u8],
        inflated_size: usize,
    ) -> Result<usize, Lz77DecompressionError> {
        // Initialize the destination buffer
        let mut output_size = 0;
        let mut src_offset = 0;

        while output_size < inflated_size {
            // Read the flags
            let mut flag_index = 0u8;
            let flags = *buffer
                .get(src_offset)
                .ok_or(Lz77DecompressionError::UnexpectedEndOfInput)?;
            src_offset += 1;

            while flag_index < 8 && output_size < inflated_size {
                if flags & (0x080 >> flag_index) != 0 {
                    src_offset += 2;
                    let next_byte = *buffer
                        .get(src_offset)
                        .ok_or(Lz77DecompressionError::UnexpectedEndOfInput)?
                        as usize;
                    output_size += (buffer[next_byte] >> 4) as usize + 3;
                } else {
                    src_offset += 1;
                    output_size += 1;
                }
                flag_index += 1;
            }
        }

        let deflated_size = ((src_offset + 3) & !3) as usize;
        Ok(deflated_size)
    }
}

// ANCHOR RomType impls
impl RomReadableType for Lz77DecompressedData {
    fn read_from(rom: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        // Read the header
        let header = read_header(rom, offset)?;
        // Decompress the buffer
        let inflated_size = header.inflated_size as usize;

        let buffer = rom.read_slice_from(offset + 4)?;
        let (decompressed, deflated_size) =
            Lz77DecompressedData::decompress_from_buffer(buffer, inflated_size)?;

        Ok(Self {
            offset,
            deflated_size,
            inflated_size,
            data: decompressed,
        })
    }
}

impl RomClearableType for Lz77DecompressedData {
    fn clear_in(rom: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        let deflated_size = get_deflated_size(rom, offset)?;

        // Clear the header and the deflated size
        rom.clear_bytes(offset, 4 + deflated_size)
    }
}

/// Reads the header and checks its validity
fn read_header(rom: &RomData, offset: Offset) -> Result<Lz77Header, RomIoError> {
    // Read the header
    let header: Lz77Header = rom.read(offset)?;

    // Make sure the magic is correct
    if header.magic != 0x10 {
        Err(Lz77DecompressionError::InvalidMagic(header.magic as u8))?
    }
    // Make sure the length is valid
    if header.inflated_size > 0x080000 {
        Err(Lz77DecompressionError::InvalidInflatedLength(
            header.inflated_size,
        ))?
    }

    Ok(header)
}

/// Returns the size of the deflated (compressed) data in ROM given the offset.
///
/// Does not include the size of the Lz77 header.
pub fn get_deflated_size(rom: &RomData, header_offset: Offset) -> Result<usize, RomIoError> {
    // Read the header
    let header = read_header(rom, header_offset)?;
    // Get the deflated size
    let inflated_size = header.inflated_size as usize;
    let buffer = rom.read_slice_from(header_offset + 4)?;

    Ok(Lz77DecompressedData::get_deflated_size_from_buffer(
        buffer,
        inflated_size,
    )?)
}

// ANCHOR LZ77 compression
/// Maximum backwards search distance in a window for LZ77 Compression.
const MAX_SEARCH_DISTANCE: i32 = 0x100;

/// Compresses a buffer using the LZ77 algorithm.
///
/// The options are hard-coded for a good balance of performance and compression rate.
pub fn compress(src_buffer: &[u8]) -> Vec<u8> {
    let input_length = src_buffer.len();
    let mut dst_buffer = Vec::with_capacity(input_length);

    let mut offset = 0usize;
    let mut last_flag_position = 0usize;
    let mut num_flag_bits = 0;
    let mut flags = 0u8;

    while offset < input_length {
        if num_flag_bits == 0 {
            last_flag_position = dst_buffer.len();
            dst_buffer.push(0);
        }

        if find_token_with_length(src_buffer, offset, input_length, 3) != 0 {
            let mut token_size = 0;
            let mut token_delta = 0;

            for attempt_token_size in 3..=18 {
                let attempt_token_delta =
                    find_token_with_length(src_buffer, offset, input_length, attempt_token_size);

                if attempt_token_delta == 0 {
                    break;
                }

                token_size = attempt_token_size;
                token_delta = attempt_token_delta;
            }

            let flipped_token_delta = -token_delta - 1;

            let byte =
                (((token_size as i32 - 3) & 15) << 4) | ((flipped_token_delta & 0x0f00) >> 8);
            dst_buffer.push(byte as u8);

            let byte = (flipped_token_delta & 0x0ff) as u8;
            dst_buffer.push(byte);
            offset += token_size;
            // Set the flag bit
            flags |= 0x080 >> num_flag_bits;
            dst_buffer[last_flag_position] = flags;
        } else {
            let byte = src_buffer[offset];
            dst_buffer.push(byte);
            offset += 1;
            // Don't set the flag bit
        }

        num_flag_bits += 1;
        if num_flag_bits == 8 || offset == input_length {
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

fn find_token_with_length(buffer: &[u8], offset: usize, input_length: usize, length: usize) -> i32 {
    if offset + length > input_length {
        return 0;
    }

    for pos in (0.max(offset as i32 - MAX_SEARCH_DISTANCE)..=offset as i32 - 8).rev() {
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
mod test_lz77 {
    use super::compress;
    use crate::{
        lz77::Lz77Header, Lz77DecompressedData, Lz77DecompressionError, RomData, RomIoError,
    };

    fn lz77_compression_test(data: &[u8]) {
        let buffer = compress(&data);
        let (decompressed, _) =
            Lz77DecompressedData::decompress_from_buffer(&buffer, data.len()).unwrap();

        assert_eq!(decompressed, data);
    }

    #[test]
    fn test_lz77_compression() {
        // 1. Uniform zeros
        lz77_compression_test(&[0; 256]);

        // 2. Uniform ones
        lz77_compression_test(&[0xff; 256]);

        // 3. Uniform 0x55
        lz77_compression_test(&[0x55; 256]);

        // 4. Uniform 0xAA
        lz77_compression_test(&[0xAA; 256]);

        // 5. Uniform 0x55, 0xAA
        let data = [0x55, 0xAA];
        lz77_compression_test(&data.repeat(128));

        // 6. 0, 1, 2, 3, 4, ...
        let mut data = vec![0; 256];
        data.iter_mut().enumerate().for_each(|(i, x)| *x = i as u8);
        lz77_compression_test(&data);

        // [0] * 8 + [1] * 8 + [2] * 8 + [3] * 8 + ...
        let mut data = vec![0; 256];
        data.iter_mut()
            .enumerate()
            .for_each(|(i, x)| *x = (i / 8) as u8);
        lz77_compression_test(&data);
    }

    #[test]
    fn test_read_lz77_header() {
        let mut rom = RomData::new(crate::RomBase::FireRed, 0x104);

        let header = Lz77Header {
            magic: 0x10,
            inflated_size: 0x100,
        };
        let data = [0; 0x100];
        let comp = compress(&data);

        rom.write(0, header).unwrap();
        rom.write_slice(4, comp.as_slice()).unwrap();

        let header: Lz77DecompressedData = rom.read(0).unwrap();
        assert_eq!(header.inflated_size, 0x100);
        assert_eq!(header.data, data);
    }

    #[test]
    fn test_decompression_error() {
        let mut rom = RomData::new(crate::RomBase::FireRed, 0x24);

        let header = Lz77Header {
            magic: 0x10,
            inflated_size: 0x100,
        };
        let data = [0; 0x100];
        let comp = compress(&data);

        rom.write(0, header).unwrap();
        rom.write_slice(4, &comp[0..20]).unwrap();

        let result: Result<Lz77DecompressedData, _> = rom.read(0);
        assert_eq!(
            result.unwrap_err(),
            RomIoError::Lz77DecompressionError(Lz77DecompressionError::LookupOffsetOutOfBounds)
        );
    }
}
