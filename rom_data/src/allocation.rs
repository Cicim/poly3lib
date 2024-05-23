use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AllocatedBytes {
    data: Vec<u8>,
}

impl AllocatedBytes {
    /// Create a new BitVector with the given size.
    pub fn new(size: usize) -> Self {
        let mut data = Vec::with_capacity(correct_size(size));
        data.resize(correct_size(size), 0);
        Self { data }
    }

    /// Build the allocation vector from the given data
    /// such that each bit is set to 1 if the corresponding
    /// byte is not the clear byte.
    ///
    /// It also smooths to the nearest word boundary.
    pub fn build_from_data(data: &[u8], clear_byte: u8) -> Self {
        let mut result = Self::new(data.len());

        // This should be done word-wise, so the loop is unrolled
        for (i, bytes) in data.chunks_exact(4).enumerate() {
            let cb = clear_byte;
            let b0 = bytes[0];
            let b1 = bytes[1];
            let b2 = bytes[2];
            let b3 = bytes[3];

            if b0 != cb || b1 != cb || b2 != cb || b3 != cb {
                result.set_bit(i * 4);
                result.set_bit(i * 4 + 1);
                result.set_bit(i * 4 + 2);
                result.set_bit(i * 4 + 3);
            }
        }

        result
    }

    /// Try to read the allocation vector from the given path.
    ///
    /// Makes sure it is of the correct size for the given data.
    ///
    /// Checks that the bits are set for the bytes that are written,
    /// where written means that the data is not the clear byte.
    pub fn try_read_from_file<P: AsRef<Path>>(
        path: P,
        data: &[u8],
        clear_byte: u8,
    ) -> Option<Self> {
        let size = data.len();

        // Try to open the given path to read the allocation vector
        if let Ok(mut file) = File::open(path) {
            // Make sure the read allocation vector is the correct size
            let metadata = file.metadata().ok()?;
            let buffer_size = metadata.len() as usize;
            if buffer_size != correct_size(size) {
                return None;
            }

            let mut allocated_vector = Vec::with_capacity(correct_size(size));
            file.read_to_end(&mut allocated_vector).ok()?;
            let allocated = Self {
                data: allocated_vector,
            };

            // Make sure it is at least valid for the bytes that are written
            for i in 0..size {
                if data[i] != clear_byte && !allocated.get_bit(i) {
                    return None;
                }
            }

            return Some(allocated);
        }

        None
    }

    /// Write the allocation vector to the given path.
    pub fn write_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), std::io::Error> {
        let mut bits_file = File::create(path)?;
        // Write the allocated bytes bitarray to the file
        bits_file.write_all(&self.data)
    }

    /// Set a bit to 1.
    pub fn set_bit(&mut self, bit: usize) {
        self.data[bit / 8] |= 1 << (bit % 8);
    }
    /// Set a set of consecutive bits to 1.
    pub fn set_consecutive_bits(&mut self, offset: usize, size: usize) {
        // TODO Optimize
        for i in offset..offset + size {
            self.set_bit(i);
        }
    }

    /// Clear a bit to 0.
    pub fn clear_bit(&mut self, bit: usize) {
        self.data[bit / 8] &= !(1 << (bit % 8));
    }
    /// Clear a set of consecutive bits to 0.
    pub fn clear_consecutive_bits(&mut self, offset: usize, size: usize) {
        // TODO Optimize
        for i in offset..offset + size {
            self.clear_bit(i);
        }
    }

    /// Get the value of a bit.
    pub fn get_bit(&self, bit: usize) -> bool {
        self.data[bit / 8] & (1 << (bit % 8)) != 0
    }

    /// Find a free space of a given size with the given alignment.
    pub fn find_free_space(&self, size: usize, align: usize) -> Option<usize> {
        let byte_length = self.data.len() * 8;
        let mut offset = 0;

        // If no data of that size can fit in this ROM
        if size + offset > byte_length {
            return None;
        }

        'outer: while offset <= byte_length - size {
            // If this was a possible free space, but the last bit was not 0xFF,
            // then we need to skip ahead because no possible sub-window could
            // be free.
            if self.get_bit(offset + size - 1) {
                offset += size;
                continue;
            }

            // The window ends with 0xFF
            // Check if the window is free
            for i in 0..size {
                if self.get_bit(offset + i) {
                    // An 0xFF was found in the middle of the window
                    // We can keep looking right after it (aligned)
                    offset += 1;
                    offset = (offset + align - 1) & !(align - 1);
                    continue 'outer;
                }
            }

            return Some(offset);
        }

        None
    }
}

/// Returns the correct size for an allocation vector given the size of the data.
#[inline]
fn correct_size(size: usize) -> usize {
    (size + 7) / 8
}
