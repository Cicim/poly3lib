use crate::{GBAIOError, GBAType};

impl <T: GBAType, const N: usize> GBAType for [T; N] {
    const SIZE: usize = T::SIZE * N;

    fn read_from(bytes: &[u8], offset: usize) -> Result<Self, GBAIOError> {
        let mut vec = Vec::new();
        for i in 0..N {
            vec.push(T::read_from(bytes, offset + i * T::SIZE)?);
        }
        let array  = vec.try_into();
        match array {
            Ok(array) => Ok(array),
            Err(_) => Err(GBAIOError::Unknown("Failed to convert vec to array when reading array")),
        }
    }

    fn write_to(&self, bytes: &mut [u8], offset: usize) -> Result<(), GBAIOError> {
        for i in 0..N {
            self[i].write_to(bytes, offset + i * T::SIZE)?;
        }
        Ok(())
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_write_array() {
        let bytes = [0x01, 0x02, 0x03, 0x04];
        let array: [u8; 4] = GBAType::read_from(&bytes, 0).unwrap();
        assert_eq!(array, [0x01, 0x02, 0x03, 0x04]);

        let mut bytes = [0; 4];
        array.write_to(&mut bytes, 0).unwrap();
        assert_eq!(bytes, [0x01, 0x02, 0x03, 0x04]);
    }

    #[test]
    fn test_very_big_array() {
        let bytes = [0x01; 0x1000];
        let array: [u8; 0x1000] = GBAType::read_from(&bytes, 0).unwrap();
        assert_eq!(array, [0x01; 0x1000]);

        let mut bytes = [0; 0x1000];
        array.write_to(&mut bytes, 0).unwrap();
        assert_eq!(bytes, [0x01; 0x1000]);
    }

}
