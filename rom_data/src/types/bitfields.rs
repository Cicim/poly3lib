//! This module defines a [`BitFields`] type that can be used to read and write
//! bitfields of different integer types to a ROM.

use std::fmt::Display;

use crate::{Offset, RomData, RomIoError};

use super::RomReadableType;

/// A type that can be used to read and write bitfields of different integer types to a ROM.
///
/// This type is generic over the integer type that is used to store the bitfields.
///
/// Because of how it is defined, this struct does not implement the [`RomSizedType`][crate::types::RomSizedType],
/// [`RomReadableType`][crate::types::RomReadableType], [`RomWritableType`][crate::types::RomWritableType], or
/// [`RomClearableType`][crate::types::RomClearableType] traits since it needs to be initialized with a list of
/// bitfield sizes.
pub struct BitFields<T, const N: usize> {
    bitfield_shifts: [u8; N],
    bitfield_sizes: [u8; N],
    bitfield_masks: [u32; N],
    pub(self) _phantom: std::marker::PhantomData<T>,
}

impl<T: Sized, const N: usize> BitFields<T, N> {
    const SIZE: u8 = std::mem::size_of::<T>() as u8 * 8;

    /// Creates a bitfield type with the given bitfield sizes.
    pub fn new(bitfield_sizes: [u8; N]) -> Self {
        let mut bitfield_shifts: [u8; N] = [0; N];
        let mut bitfield_masks: [u32; N] = [0; N];

        debug_assert!(
            bitfield_sizes.iter().sum::<u8>() <= Self::SIZE,
            "The number of bits in a bit field must not exceed the containing type size"
        );

        // Calculate the shifts and masks for each bitfield
        let mut shift: u8 = Self::SIZE;
        for (i, size) in bitfield_sizes.iter().enumerate() {
            shift -= size;
            bitfield_shifts[i] = shift;
            bitfield_masks[i] = ((1 << size) - 1) << shift;
        }

        Self {
            bitfield_sizes,
            bitfield_shifts,
            bitfield_masks,
            _phantom: std::marker::PhantomData,
        }
    }
}

impl<T: Sized, const N: usize> Display for BitFields<T, N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Get the bit size of the bitfield
        let bit_size = std::mem::size_of::<T>() * 8;

        for i in 0..N {
            let size = self.bitfield_sizes[i];
            let mask = self.bitfield_masks[i];

            // Print the bitfield
            if bit_size == 8 {
                write!(f, "Bitfield {:>2} ({:>2} bits): {:08b}\n", i, size, mask)?;
            }
            if bit_size == 16 {
                write!(f, "Bitfield {:>2} ({:>2} bits): {:016b}\n", i, size, mask)?;
            }
            if bit_size == 32 {
                write!(f, "Bitfield {:>2} ({:>2} bits): {:032b}\n", i, size, mask)?;
            }
        }

        Ok(())
    }
}

macro_rules! impl_bitfields_read_write {
    ($base_type:ty, $unsigned_type:ty) => {
        impl<const N: usize> BitFields<$base_type, N> {
            pub fn read_from(
                &self,
                rom: &RomData,
                offset: Offset,
            ) -> Result<[$base_type; N], RomIoError> {
                let mut bitfield_values: [$base_type; N] = [0; N];

                let value = <$unsigned_type>::read_from(rom, offset)?;

                // Read the bitfields
                for i in 0..N {
                    // Here's a little visualization with an 8-bit field
                    // 00011100 (mask)
                    //    --- size = 3
                    //       ^ shift = 2
                    let mask = self.bitfield_masks[i];
                    let shift = self.bitfield_shifts[i];
                    let size = self.bitfield_sizes[i];

                    // Mask the field
                    // abcdefgh -> 000def00
                    let field = value & mask as $unsigned_type;
                    // Convert it to its base type (signed or unsigned)
                    let field = field as $base_type;

                    // Shift it left so that the bits are in the top position
                    // this way we can sign extend it if the type is signed.
                    // 000def00 -> def00000
                    let field = field << (Self::SIZE - shift - size);

                    // Shift it right so that the bits are in the bottom position
                    // 000def00 -> 00000def or ddddddef if signed
                    let field = field >> (Self::SIZE - size);

                    bitfield_values[i] = field;
                }

                Ok(bitfield_values)
            }

            pub fn write_to(
                &self,
                rom: &mut RomData,
                offset: Offset,
                fields: [$base_type; N],
            ) -> Result<(), RomIoError> {
                // Read the base value from ROM
                let base_value: $base_type = rom.read(offset)?;
                // Convert the base value to its signed version
                let mut base_value = base_value as $unsigned_type;

                // Write the bitfields
                for i in 0..N {
                    // Here's a little visualization with the same 8-bit field
                    // 00011100 (mask)
                    //    --- size = 3
                    //       ^ shift = 2
                    let mask = self.bitfield_masks[i];
                    let shift = self.bitfield_shifts[i];

                    // First, we get the value to write there and we convert it to its unsigned form
                    let field = fields[i] as $unsigned_type;
                    // Then we shift it left and we mask it
                    // xxxxxxdef -> 000def00
                    let field = (field << shift) & (mask as $unsigned_type);

                    // Then we clear the bits in the base value
                    // BCDEFGHI -> BCD000HI
                    base_value &= !(mask as $unsigned_type);
                    // Then we OR the field into the base value
                    // BCD000HI | 000def00 -> BCDdefHI
                    base_value |= field;
                }

                // Convert the base value back to the base type
                let base_value = base_value as $base_type;
                // Write the base value to ROM
                rom.write(offset, base_value)?;

                Ok(())
            }
        }
    };
}

impl_bitfields_read_write!(u8, u8);
impl_bitfields_read_write!(u16, u16);
impl_bitfields_read_write!(u32, u32);
impl_bitfields_read_write!(i8, u8);
impl_bitfields_read_write!(i16, u16);
impl_bitfields_read_write!(i32, u32);

#[cfg(test)]
mod test_bitfields {
    use super::*;

    fn create_rom() -> RomData {
        RomData::new(crate::RomBase::FireRed, 0x10)
    }

    #[test]
    fn test_read_u32() {
        let mut rom = create_rom();
        rom.write_word(0, 0x12345678).unwrap();

        let bitfields: BitFields<u32, 8> = BitFields::new([4, 4, 4, 4, 4, 4, 4, 4]);
        for (i, field) in bitfields.read_from(&rom, 0).unwrap().iter().enumerate() {
            assert_eq!(*field, i as u32 + 1);
        }
    }

    #[test]
    fn test_read_unsigned() {
        let mut rom = create_rom();

        rom.write_halfword(0, 0x1234).unwrap();

        let bfu16: BitFields<u16, 4> = BitFields::new([4, 4, 4, 4]);
        let read = bfu16.read_from(&rom, 0).unwrap();
        assert_eq!(read[0], 1);
        assert_eq!(read[1], 2);
        assert_eq!(read[2], 3);
        assert_eq!(read[3], 4);

        let bfu8: BitFields<u8, 4> = BitFields::new([2, 2, 2, 2]);
        let read = bfu8.read_from(&rom, 0).unwrap();
        assert_eq!(read[0], 0);
        assert_eq!(read[1], 3);
        assert_eq!(read[2], 1);
        assert_eq!(read[3], 0);

        let bfu16: BitFields<u16, 3> = BitFields::new([4, 8, 4]);
        let read = bfu16.read_from(&rom, 0).unwrap();
        assert_eq!(read[0], 1);
        assert_eq!(read[1], 0x23);
        assert_eq!(read[2], 4);
    }

    #[test]
    fn test_read_unsigned_incomplete() {
        let mut rom = create_rom();

        rom.write_halfword(0, 0x1234).unwrap();

        let bfu16: BitFields<u16, 3> = BitFields::new([4, 4, 3]);
        let read = bfu16.read_from(&rom, 0).unwrap();
        assert_eq!(read[0], 1);
        assert_eq!(read[1], 2);
        assert_eq!(read[2], 1);
    }

    #[test]
    fn test_read_signed() {
        let rom = create_rom();

        let bfi32: BitFields<i32, 8> = BitFields::new([1, 2, 3, 4, 5, 6, 7, 4]);
        let read = bfi32.read_from(&rom, 0).unwrap();
        assert_eq!(read, [-1; 8]);
    }

    #[test]
    fn test_write_read_consistency() {
        let mut rom = create_rom();

        let bfu16: BitFields<u16, 4> = BitFields::new([4, 4, 4, 4]);
        let fields = [1, 2, 3, 4];
        bfu16.write_to(&mut rom, 0, fields).unwrap();
        let read = bfu16.read_from(&rom, 0).unwrap();
        assert_eq!(read, fields);

        let bfu8: BitFields<u8, 4> = BitFields::new([2, 2, 2, 2]);
        let fields = [0, 1, 0, 2];
        bfu8.write_to(&mut rom, 0, fields).unwrap();
        let read = bfu8.read_from(&rom, 0).unwrap();
        assert_eq!(read, fields);

        let bfu32: BitFields<u32, 3> = BitFields::new([4, 8, 4]);
        let fields = [1, 0x23, 4];
        bfu32.write_to(&mut rom, 0, fields).unwrap();
        let read = bfu32.read_from(&rom, 0).unwrap();
        assert_eq!(read, fields);
    }

    #[test]
    fn test_write_leaves_old_bits_unchanged() {
        let mut rom = create_rom();

        let initial_state = 0b00001111;
        rom.write_byte(0, initial_state).unwrap();

        let bitfields: BitFields<u8, 2> = BitFields::new([2, 2]);
        let fields = [0b01, 0b10];
        bitfields.write_to(&mut rom, 0, fields).unwrap();

        let expected = 0b01101111;
        let actual = rom.read_byte(0).unwrap();
        assert_eq!(actual, expected);
    }
}
