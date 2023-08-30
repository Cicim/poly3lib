use crate::types::{RomPointer, RomVector};

use super::*;

#[test]
fn read_int_structs() {
    let mut rom = create_rom(RomBase::FireRed);

    let single_u8: SingleU8 = rom.read(0x0).unwrap();
    assert_eq!(single_u8.value, 0xff);

    let single_u16: SingleU16 = rom.read(0x0).unwrap();
    assert_eq!(single_u16.value, 0xffff);

    let single_u32: SingleU32 = rom.read(0x0).unwrap();
    assert_eq!(single_u32.value, 0xffffffff);

    let single_i8: SingleI8 = rom.read(0x0).unwrap();
    assert_eq!(single_i8.value, -1);

    let single_i16: SingleI16 = rom.read(0x0).unwrap();
    assert_eq!(single_i16.value, -1);

    let single_i32: SingleI32 = rom.read(0x0).unwrap();
    assert_eq!(single_i32.value, -1);

    let single_bool8: SingleBool8 = rom.read(0x0).unwrap();
    assert_eq!(single_bool8.value, true);

    let single_bool16: SingleBool16 = rom.read(0x0).unwrap();
    assert_eq!(single_bool16.value, true);

    let single_bool32: SingleBool32 = rom.read(0x0).unwrap();
    assert_eq!(single_bool32.value, true);

    rom.write_word(0, 0).unwrap();

    let single_u8: SingleU8 = rom.read(0x0).unwrap();
    assert_eq!(single_u8.value, 0);

    let single_u16: SingleU16 = rom.read(0x0).unwrap();
    assert_eq!(single_u16.value, 0);

    let single_u32: SingleU32 = rom.read(0x0).unwrap();
    assert_eq!(single_u32.value, 0);

    let single_i8: SingleI8 = rom.read(0x0).unwrap();
    assert_eq!(single_i8.value, 0);

    let single_i16: SingleI16 = rom.read(0x0).unwrap();
    assert_eq!(single_i16.value, 0);

    let single_i32: SingleI32 = rom.read(0x0).unwrap();
    assert_eq!(single_i32.value, 0);

    let single_bool8: SingleBool8 = rom.read(0x0).unwrap();
    assert_eq!(single_bool8.value, false);

    let single_bool16: SingleBool16 = rom.read(0x0).unwrap();
    assert_eq!(single_bool16.value, false);

    let single_bool32: SingleBool32 = rom.read(0x0).unwrap();
    assert_eq!(single_bool32.value, false);
}

#[test]
fn read_padded_structs() {
    let mut rom = create_rom(RomBase::FireRed);

    // rom_struct!(U8PaddingU16U8Padding {
    //     u8 value1;
    //     u16 value2;
    //     u8 value3;
    // });
    rom.write_byte(0, 0xA0).unwrap();
    rom.write_halfword(2, 0xB0).unwrap();
    rom.write_byte(4, 0xC0).unwrap();

    let struct_value: U8PaddingU16U8Padding = rom.read(0x0).unwrap();
    assert_eq!(struct_value.value1, 0xA0);
    assert_eq!(struct_value.value2, 0xB0);
    assert_eq!(struct_value.value3, 0xC0);

    // rom_struct!(U16PaddingU32U8Padding {
    //     u16 value1;
    //     u32 value2;
    //     u8 value3;
    // });
    rom.write_halfword(0, 0xD0).unwrap();
    rom.write_word(4, 0xE0).unwrap();
    rom.write_byte(8, 0xF0).unwrap();

    let struct_value: U16PaddingU32U8Padding = rom.read(0x0).unwrap();
    assert_eq!(struct_value.value1, 0xD0);
    assert_eq!(struct_value.value2, 0xE0);
    assert_eq!(struct_value.value3, 0xF0);
}

#[test]
fn read_bitfields_structs() {
    let mut rom = create_rom(RomBase::FireRed);

    // rom_struct!(BitFieldU8 {
    //     u8 value1 : 4;
    //     u8 value2 : 4;
    // });
    rom.write_byte(0, 0x12).unwrap();
    let struct_value: BitFieldU8 = rom.read(0x0).unwrap();
    assert_eq!(struct_value.value1, 0x1);
    assert_eq!(struct_value.value2, 0x2);

    // rom_struct!(BitFieldU16 {
    //     u16 value1 : 4;
    //     u16 value2 : 4;
    //     u16 value3 : 4;
    //     u16 value4 : 4;
    // });
    rom.write_halfword(0, 0x1234).unwrap();
    let struct_value: BitFieldU16 = rom.read(0x0).unwrap();
    assert_eq!(struct_value.value1, 0x1);
    assert_eq!(struct_value.value2, 0x2);
    assert_eq!(struct_value.value3, 0x3);
    assert_eq!(struct_value.value4, 0x4);

    // rom_struct!(BitFieldU32 {
    //     u32 value1 : 8;
    //     u32 value2 : 4;
    //     u32 value3 : 4;
    //     u32 value4 : 8;
    //     u32 value5 : 4;
    //     u32 value6 : 4;
    // });
    rom.write_word(0, 0x12345678).unwrap();
    let struct_value: BitFieldU32 = rom.read(0x0).unwrap();
    assert_eq!(struct_value.value1, 0x12);
    assert_eq!(struct_value.value2, 0x3);
    assert_eq!(struct_value.value3, 0x4);
    assert_eq!(struct_value.value4, 0x56);
    assert_eq!(struct_value.value5, 0x7);
    assert_eq!(struct_value.value6, 0x8);

    // rom_struct!(BitFieldPair {
    //     u8 field1 : 5;
    //     u8 field2 : 4;
    // });
    rom.write_byte(0, 0b01010111).unwrap();
    rom.write_byte(1, 0b10101010).unwrap();
    let struct_value: BitFieldPair = rom.read(0x0).unwrap();
    assert_eq!(struct_value.field1, 0b01010);
    assert_eq!(struct_value.field2, 0b1010);
}

#[test]
fn read_pointer_structs() {
    let mut rom = create_rom(RomBase::FireRed);

    // rom_struct!(SingleU8Pointer { u8 *value; });
    let pointer = RomPointer::Valid(0x4, 0x12);
    rom.write(0, pointer.clone()).unwrap();
    let struct_value: SingleU8Pointer = rom.read(0x0).unwrap();
    assert_eq!(struct_value.value, pointer);

    // rom_struct!(SingleU16Pointer { u16 *value; });
    let pointer = RomPointer::Valid(0x4, 0x1234);
    rom.write(0, pointer.clone()).unwrap();
    let struct_value: SingleU16Pointer = rom.read(0x0).unwrap();
    assert_eq!(struct_value.value, pointer);

    // rom_struct!(SingleU32Pointer { u32 *value; });
    let pointer = RomPointer::Valid(0x4, 0x12345678);
    rom.write(0, pointer.clone()).unwrap();
    let struct_value: SingleU32Pointer = rom.read(0x0).unwrap();
    assert_eq!(struct_value.value, pointer);

    // rom_struct!(MultiplePointerDereference { u8 **value; });
    let pointer = RomPointer::Valid(0x4, RomPointer::Valid(8, 1));
    rom.write(0, pointer.clone()).unwrap();
    let struct_value: MultiplePointerDereference = rom.read(0x0).unwrap();
    assert_eq!(struct_value.value, pointer);

    // rom_struct!(SingleVoidPointer { void *value; });
    let pointer: RomPointer = RomPointer::NoData(0x4);
    rom.write(0, pointer.clone()).unwrap();
    let struct_value: SingleVoidPointer = rom.read(0x0).unwrap();
    assert_eq!(struct_value.value, pointer);
}

#[test]
fn read_complex_struct() {
    let mut rom = create_rom(RomBase::FireRed);

    // rom_struct!(ComplexStruct {
    //  /* 0 */ u8 value1;
    //  /* 2 */ u16 value2;
    //  /* 4 */ u32 value3;
    //  /* 8,9,10 */ u8 value4[3];
    //  /* 12,14,16  */ u16 value5[3];
    //  /* 20,24,28 */ u32 value6[3];
    //  /* 32  */ u8 value7 : 4;
    //  /* 33:7-3  */ u8 value8 : 5;
    //  /* 33:2-0  */ u8 value9 : 3;
    //  /* 34:7-4  */ s8 value10 : 4;
    //  /* 36  */ u8 *value11;
    //  /* 40  */ u16 *value12;
    //  /* 44  */ u32 *value13;
    //  /* 48  */ u32 value14 : 4;
    //  /* 52  */ i32 value15 : 4;
    //  /* 56  */ u8 value16;
    // });
    rom.write_byte(0, 0x1).unwrap();
    rom.write_halfword(2, 0x2).unwrap();
    rom.write_word(4, 0x3).unwrap();
    rom.write_byte(8, 0x40).unwrap();
    rom.write_byte(9, 0x41).unwrap();
    rom.write_byte(10, 0x42).unwrap();
    rom.write_halfword(12, 0x500).unwrap();
    rom.write_halfword(14, 0x501).unwrap();
    rom.write_halfword(16, 0x502).unwrap();
    rom.write_word(20, 0x6000000).unwrap();
    rom.write_word(24, 0x6000001).unwrap();
    rom.write_word(28, 0x6000002).unwrap();
    rom.write_byte(32, 0x73).unwrap(); // 7
    rom.write_byte(33, 0xF8).unwrap(); // 31, 0
    rom.write_byte(34, 0xF0).unwrap(); // -1
    let pointer11 = RomPointer::Valid(72, 0x12);
    rom.write(36, pointer11.clone()).unwrap();
    let pointer12 = RomPointer::Valid(76, 0x1234);
    rom.write(40, pointer12.clone()).unwrap();
    let pointer13 = RomPointer::Valid(80, 0x12345678);
    rom.write(44, pointer13.clone()).unwrap();
    rom.write_word(48, 0x90000000).unwrap(); // 9
    rom.write_word(52, 0x80000000).unwrap(); // -8
    rom.write_byte(56, 0xA0).unwrap();

    let struct_value: ComplexStruct = rom.read(0x0).unwrap();
    assert_eq!(struct_value.value1, 0x1);
    assert_eq!(struct_value.value2, 0x2);
    assert_eq!(struct_value.value3, 0x3);
    assert_eq!(struct_value.value4.as_ref(), [0x40, 0x41, 0x42]);
    assert_eq!(struct_value.value5.as_ref(), [0x500, 0x501, 0x502]);
    assert_eq!(
        struct_value.value6.as_ref(),
        [0x6000000, 0x6000001, 0x6000002]
    );
    assert_eq!(struct_value.value7, 0x7);
    assert_eq!(struct_value.value8, 0x1F);
    assert_eq!(struct_value.value9, 0);
    assert_eq!(struct_value.value10, -1);
    assert_eq!(struct_value.value11, pointer11);
    assert_eq!(struct_value.value12, pointer12);
    assert_eq!(struct_value.value13, pointer13);
    assert_eq!(struct_value.value14, 0x9);
    assert_eq!(struct_value.value15, -8);
    assert_eq!(struct_value.value16, 0xA0);
}

#[test]
fn read_type_attribute() {
    let mut fire = create_rom(RomBase::FireRed);
    let mut ruby = create_rom(RomBase::Ruby);
    let mut emer = create_rom(RomBase::Emerald);

    // rom_struct!(ThreeSizes {
    //     #[for(base(Ruby, Sapphire), type(u8))]
    //     #[for(base(Emerald), type(u16))]
    //     u32 value;
    // });
    fire.write_word(0, 0x1).unwrap();
    ruby.write_byte(0, 0x1).unwrap();
    emer.write_halfword(0, 0x1).unwrap();

    let fire_struct: ThreeSizes = fire.read(0x0).unwrap();
    assert_eq!(fire_struct.value, 0x1);

    let ruby_struct: ThreeSizes = ruby.read(0x0).unwrap();
    assert_eq!(ruby_struct.value, 0x1);

    let emer_struct: ThreeSizes = emer.read(0x0).unwrap();
    assert_eq!(emer_struct.value, 0x1);

    // Then make sure all there structs are equal
    assert_eq!(fire_struct, ruby_struct);
    assert_eq!(ruby_struct, emer_struct);
}

#[test]
fn read_default_attribute() {
    let mut fire = create_rom(RomBase::FireRed);
    let mut emer = create_rom(RomBase::Emerald);

    // rom_struct!(MissingField {
    //     u8 value1;
    //     #[for(base(Emerald), default(128))]
    //     u32 value2;
    // });
    fire.write_byte(0, 10).unwrap();
    emer.write_byte(0, 10).unwrap();

    fire.write_word(4, 20).unwrap();

    let fire_struct: MissingField = fire.read(0x0).unwrap();
    assert_eq!(fire_struct.value1, 10);
    assert_eq!(fire_struct.value2, 20);

    let emer_struct: MissingField = emer.read(0x0).unwrap();
    assert_eq!(emer_struct.value1, 10);
    assert_eq!(emer_struct.value2, 128);
}

#[test]
fn read_vectors() {
    // rom_struct!(U8Vector {
    //     u8 length;
    //     u8 values{$length};
    // });
    let mut rom = create_rom(RomBase::FireRed);
    rom.write_byte(0, 4).unwrap();
    rom.write_offset(4, 8).unwrap();
    rom.write_word(8, 0xAAAAAAAA).unwrap();

    let struct_value: U8Vector = rom.read(0x0).unwrap();
    assert_eq!(struct_value.length, 4);
    assert_eq!(
        struct_value.values,
        RomVector::Valid {
            offset: 8,
            clear_size: 4,
            data: vec![0xAA, 0xAA, 0xAA, 0xAA]
        }
    );

    // Read a null struct with a length (should be invalid)
    rom.write_word(4, 0).unwrap();
    let struct_value: U8Vector = rom.read(0x0).unwrap();
    assert_eq!(struct_value.length, 4);
    assert_eq!(struct_value.values, RomVector::Invalid(0));

    // Now delete the size too and it should be null
    rom.write_byte(0, 0).unwrap();
    let struct_value: U8Vector = rom.read(0x0).unwrap();
    assert_eq!(struct_value.length, 0);
    assert_eq!(struct_value.values, RomVector::Null);

    // rom_struct!(ComplexVector {
    //     u8 data{$length * $typesize};
    //     u32 length;
    //     u32 typesize;
    // });
    let mut rom = create_rom(RomBase::FireRed);
    rom.write_offset(0, 12).unwrap();
    rom.write_word(4, 4).unwrap();
    rom.write_word(8, 2).unwrap();

    // 4 * 2 = 8, so we should read 8 bytes
    rom.write_word(12, 0xAAAAAAAA).unwrap();
    rom.write_word(16, 0xBBBBBBBB).unwrap();

    let struct_value: ComplexVector = rom.read(0x0).unwrap();
    assert_eq!(
        struct_value.data,
        RomVector::Valid {
            offset: 12,
            clear_size: 8,
            data: vec![0xAA, 0xAA, 0xAA, 0xAA, 0xBB, 0xBB, 0xBB, 0xBB]
        }
    );
}
