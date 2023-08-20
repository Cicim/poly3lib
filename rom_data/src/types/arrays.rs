//! Declares a new type [`RomArray<T>`] that can be used to read an array of values of type
//! `T` from the ROM.

use serde::{Deserialize, Serialize};

use crate::{Offset, RomData, RomIoError};

use super::{RomReadableType, RomSizedType, RomWritableType};

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Clone)]
pub struct RomArray<T, const N: usize>(Vec<T>);

impl<T, const N: usize> RomArray<T, N> {
    /// Returns a new [`RomArray`] with the given inner vector.
    ///
    /// # Panics
    /// If the length of the inner vector is not equal to `N`.
    pub fn new(inner: Vec<T>) -> Self {
        assert_eq!(inner.len(), N);
        Self(inner)
    }

    /// Returns a mutable iterator over the inner vector.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.0.iter_mut()
    }
}

// ANCHOR Methods for iteration and indexing
impl<T, const N: usize> std::ops::Index<usize> for RomArray<T, N> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<T, const N: usize> IntoIterator for RomArray<T, N> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T, const N: usize> std::ops::IndexMut<usize> for RomArray<T, N> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

// ANCHOR RomType impls
impl<T: RomReadableType, const N: usize> RomReadableType for RomArray<T, N> {
    fn read(data: &RomData, offset: Offset) -> Result<Self, RomIoError> {
        // Check out of bounds before reading the first element
        // to avoid useless computation reading possibly expensive
        // types if the array is invalid.
        if !data.in_bounds(offset + Self::SIZE - 1) {
            return Err(RomIoError::OutOfBounds(offset, Self::SIZE));
        }

        let mut vec = Vec::with_capacity(N);

        for i in 0..N {
            vec.push(T::read(data, offset + i * T::SIZE)?);
        }

        Ok(Self(vec))
    }
}

impl<T: RomWritableType, const N: usize> RomWritableType for RomArray<T, N> {
    fn write(self, data: &mut RomData, offset: Offset) -> Result<(), RomIoError> {
        if self.0.len() != N {
            return Err(RomIoError::InvalidArrayLength(self.0.len(), N));
        }

        for (i, value) in self.0.into_iter().enumerate() {
            value.write(data, offset + i * T::SIZE)?;
        }

        Ok(())
    }
}

impl<T: RomSizedType, const N: usize> RomSizedType for RomArray<T, N> {
    const SIZE: usize = N * T::SIZE;
}

#[cfg(test)]
mod test_array {
    use super::*;

    pub fn get_rom() -> RomData {
        let mut data = RomData::new(crate::RomBase::FireRed, 0x10);
        data.write_byte(0, 0x10).unwrap();
        data.write_byte(1, 0x11).unwrap();
        data.write_byte(2, 0x12).unwrap();
        data.write_byte(3, 0x13).unwrap();

        data
    }

    #[test]
    fn test_indexing() {
        let array = RomArray::<u8, 4>::new(vec![0x10, 0x11, 0x12, 0x13]);

        assert_eq!(array[0], 0x10);
        assert_eq!(array[1], 0x11);
        assert_eq!(array[2], 0x12);
        assert_eq!(array[3], 0x13);
    }

    #[test]
    fn test_iter() {
        let array = RomArray::<u8, 4>::new(vec![0x10, 0x11, 0x12, 0x13]);

        let mut iter = array.into_iter();

        assert_eq!(iter.next(), Some(0x10));
        assert_eq!(iter.next(), Some(0x11));
        assert_eq!(iter.next(), Some(0x12));
        assert_eq!(iter.next(), Some(0x13));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_new() {
        let array = RomArray::<u8, 4>::new(vec![0x10, 0x11, 0x12, 0x13]);

        assert_eq!(array[0], 0x10);
        assert_eq!(array[1], 0x11);
        assert_eq!(array[2], 0x12);
        assert_eq!(array[3], 0x13);
    }

    #[test]
    fn test_iter_mut() {
        let mut array = RomArray::<u8, 4>::new(vec![0x10, 0x11, 0x12, 0x13]);

        for (i, value) in array.iter_mut().enumerate() {
            *value = i as u8;
        }

        assert_eq!(array[0], 0);
        assert_eq!(array[1], 1);
        assert_eq!(array[2], 2);
        assert_eq!(array[3], 3);
    }

    #[test]
    fn test_read() {
        let rom = get_rom();
        let array = RomArray::<u8, 4>::read(&rom, 0).unwrap();

        assert_eq!(array[0], 0x10);
        assert_eq!(array[1], 0x11);
        assert_eq!(array[2], 0x12);
        assert_eq!(array[3], 0x13);

        let array = RomArray::<i16, 2>::read(&rom, 0).unwrap();
        assert_eq!(array[0], 0x1110);
        assert_eq!(array[1], 0x1312);
    }

    #[test]
    fn test_read_errors() {
        let rom = get_rom();

        assert_eq!(
            RomArray::<u16, 2>::read(&rom, 1),
            Err(RomIoError::Misaligned(1, 2))
        );

        assert_eq!(
            RomArray::<u32, 0x20>::read(&rom, 0),
            Err(RomIoError::OutOfBounds(0, 0x20 * 4))
        )
    }

    #[test]
    fn test_write() {
        let mut rom = get_rom();
        let array = RomArray::<u8, 4>::new(vec![0x20, 0x21, 0x22, 0x23]);

        array.write(&mut rom, 0).unwrap();

        assert_eq!(rom.read_byte(0).unwrap(), 0x20);
        assert_eq!(rom.read_byte(1).unwrap(), 0x21);
        assert_eq!(rom.read_byte(2).unwrap(), 0x22);
        assert_eq!(rom.read_byte(3).unwrap(), 0x23);
    }

    #[test]
    fn test_write_errors() {
        let mut rom = get_rom();
        let array = RomArray::<u16, 1>::new(vec![0x20]);

        assert_eq!(array.write(&mut rom, 1), Err(RomIoError::Misaligned(1, 2)));

        let mut array = RomArray::<u16, 3>::new(vec![0x20, 0x21, 0x22]);
        array.0.pop();

        assert_eq!(
            array.write(&mut rom, 0),
            Err(RomIoError::InvalidArrayLength(2, 3))
        );
    }

    #[test]
    fn test_json_serialize() {
        let array: RomArray<i32, 4> = RomArray::new(vec![1, -2, 3, -4]);
        assert_eq!(serde_json::to_string(&array).unwrap(), "[1,-2,3,-4]");
    }

    #[test]
    fn test_json_deserialize() {
        let array: RomArray<i32, 4> = serde_json::from_str("[1,-2,3,-4]").unwrap();
        assert_eq!(array[0], 1);
        assert_eq!(array[1], -2);
        assert_eq!(array[2], 3);
        assert_eq!(array[3], -4);
    }
}
