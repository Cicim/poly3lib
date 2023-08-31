use crate::types::{RomArray, RomPointer, RomVector};

use super::*;

#[test]
fn write_int_struct() {
    let mut rom = create_rom(RomBase::FireRed);

    let single_u8 = SingleU8 { value: 0x1 };
    rom.write(0x0, single_u8).unwrap();
    assert_eq!(rom.read_byte(0).unwrap(), 0x1);

    let single_u16 = SingleU16 { value: 0x2 };
    rom.write(0x0, single_u16).unwrap();
    assert_eq!(rom.read_halfword(0).unwrap(), 0x2);

    let single_u32 = SingleU32 { value: 0x3 };
    rom.write(0x0, single_u32).unwrap();
    assert_eq!(rom.read_word(0).unwrap(), 0x3);

    let single_i8 = SingleI8 { value: -1 };
    rom.write(0x0, single_i8).unwrap();
    assert_eq!(rom.read_byte(0).unwrap(), 0xff);

    let single_i16 = SingleI16 { value: -2 };
    rom.write(0x0, single_i16).unwrap();
    assert_eq!(rom.read_halfword(0).unwrap(), 0xfffe);

    let single_i32 = SingleI32 { value: -3 };
    rom.write(0x0, single_i32).unwrap();
    assert_eq!(rom.read_word(0).unwrap(), 0xfffffffd);

    let single_bool8 = SingleBool8 { value: true };
    rom.write(0x0, single_bool8).unwrap();
    assert_eq!(rom.read_byte(0).unwrap(), 0x1);

    let single_bool16 = SingleBool16 { value: true };
    rom.write(0x0, single_bool16).unwrap();
    assert_eq!(rom.read_halfword(0).unwrap(), 0x1);

    let single_bool32 = SingleBool32 { value: true };
    rom.write(0x0, single_bool32).unwrap();
    assert_eq!(rom.read_word(0).unwrap(), 0x1);
}

#[test]
fn write_padded_structs() {
    let mut rom = create_rom(RomBase::FireRed);

    // rom_struct!(U8PaddingU16U8Padding {
    //     u8 value1;
    //     u16 value2;
    //     u8 value3;
    // });
    let struct_value = U8PaddingU16U8Padding {
        value1: 0xA0,
        value2: 0xB0,
        value3: 0xC0,
    };
    rom.write(0x0, struct_value).unwrap();
    assert_eq!(rom.read_byte(0).unwrap(), 0xA0);
    assert_eq!(rom.read_halfword(2).unwrap(), 0xB0);
    assert_eq!(rom.read_byte(4).unwrap(), 0xC0);

    // rom_struct!(U16PaddingU32U8Padding {
    //     u16 value1;
    //     u32 value2;
    //     u8 value3;
    // });
    let struct_value = U16PaddingU32U8Padding {
        value1: 0xD0,
        value2: 0xE0,
        value3: 0xF0,
    };
    rom.write(0x0, struct_value).unwrap();
    assert_eq!(rom.read_halfword(0).unwrap(), 0xD0);
    assert_eq!(rom.read_word(4).unwrap(), 0xE0);
    assert_eq!(rom.read_byte(8).unwrap(), 0xF0);
}

#[test]
fn write_bitfields_structs() {
    let mut rom = create_rom(RomBase::FireRed);

    // rom_struct!(BitFieldU8 {
    //     u8 value1 : 4;
    //     u8 value2 : 4;
    // });
    let struct_value = BitFieldU8 {
        value1: 0x1,
        value2: 0x2,
    };
    rom.write(0x0, struct_value).unwrap();
    assert_eq!(rom.read_byte(0).unwrap(), 0x12);

    // rom_struct!(BitFieldU16 {
    //     u16 value1 : 4;
    //     u16 value2 : 4;
    //     u16 value3 : 4;
    //     u16 value4 : 4;
    // });
    let struct_value = BitFieldU16 {
        value1: 0x1,
        value2: 0x2,
        value3: 0x3,
        value4: 0x4,
    };
    rom.write(0x0, struct_value).unwrap();
    assert_eq!(rom.read_halfword(0).unwrap(), 0x1234);

    // rom_struct!(BitFieldU32 {
    //     u32 value1 : 8;
    //     u32 value2 : 4;
    //     u32 value3 : 4;
    //     u32 value4 : 8;
    //     u32 value5 : 4;
    //     u32 value6 : 4;
    // });
    let struct_value = BitFieldU32 {
        value1: 0x12,
        value2: 0x3,
        value3: 0x4,
        value4: 0x56,
        value5: 0x7,
        value6: 0x8,
    };
    rom.write(0x0, struct_value).unwrap();
    assert_eq!(rom.read_word(0).unwrap(), 0x12345678);
}

#[test]
fn write_pointer_structs() {
    let mut rom = create_rom(RomBase::FireRed);

    // rom_struct!(SingleU8Pointer { u8 *value; });
    let pointer = RomPointer::Valid(0x4, 0x12);
    let struct_value = SingleU8Pointer {
        value: pointer.clone(),
    };
    rom.write(0x0, struct_value).unwrap();
    let read_pointer: RomPointer<u8> = rom.read(0x0).unwrap();
    assert_eq!(read_pointer, pointer);

    // rom_struct!(SingleU16Pointer { u16 *value; });
    let pointer = RomPointer::Valid(0x4, 0x1234);
    let struct_value = SingleU16Pointer {
        value: pointer.clone(),
    };
    rom.write(0x0, struct_value).unwrap();
    let read_pointer: RomPointer<u16> = rom.read(0x0).unwrap();
    assert_eq!(read_pointer, pointer);

    // rom_struct!(SingleU32Pointer { u32 *value; });
    let pointer = RomPointer::Valid(0x4, 0x12345678);
    let struct_value = SingleU32Pointer {
        value: pointer.clone(),
    };
    rom.write(0x0, struct_value).unwrap();
    let read_pointer: RomPointer<u32> = rom.read(0x0).unwrap();
    assert_eq!(read_pointer, pointer);

    // rom_struct!(MultiplePointerDereference { u8 **value; });
    let pointer = RomPointer::Valid(0x4, RomPointer::Valid(8, 1));
    let struct_value = MultiplePointerDereference {
        value: pointer.clone(),
    };
    rom.write(0x0, struct_value).unwrap();
    let read_pointer: RomPointer<RomPointer<u8>> = rom.read(0x0).unwrap();
    assert_eq!(read_pointer, pointer);

    // rom_struct!(SingleVoidPointer { void *value; });
    let pointer: RomPointer = RomPointer::NoData(0x4);
    let struct_value = SingleVoidPointer {
        value: pointer.clone(),
    };
    rom.write(0x0, struct_value).unwrap();
    let read_pointer: RomPointer = rom.read(0).unwrap();
    assert_eq!(read_pointer, pointer);
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
fn write_complex_struct() {
    let mut rom = create_rom(RomBase::FireRed);

    let pointer11 = RomPointer::Valid(72, 0x12);
    let pointer12 = RomPointer::Valid(76, 0x1234);
    let pointer13 = RomPointer::Valid(80, 0x12345678);

    let struct_value = ComplexStruct {
        value1: 0x1,
        value2: 0x2,
        value3: 0x3,
        value4: RomArray::from([0x40, 0x41, 0x42]),
        value5: RomArray::from([0x500, 0x501, 0x502]),
        value6: RomArray::from([0x6000000, 0x6000001, 0x6000002]),
        value7: 0x7,
        value8: 0x1F,
        value9: 0,
        value10: -1,
        value11: pointer11.clone(),
        value12: pointer12.clone(),
        value13: pointer13.clone(),
        value14: 0x9,
        value15: -8,
        value16: 0xA0,
    };
    rom.write(0, struct_value).unwrap();

    // Take into account the fact that the ROM starts cleared (all 0xFF),
    // so all unset bitfields will be at 1.
    assert_eq!(rom.read_byte(0).unwrap(), 0x1);
    assert_eq!(rom.read_halfword(2).unwrap(), 0x2);
    assert_eq!(rom.read_word(4).unwrap(), 0x3);
    assert_eq!(rom.read_byte(8).unwrap(), 0x40);
    assert_eq!(rom.read_byte(9).unwrap(), 0x41);
    assert_eq!(rom.read_byte(10).unwrap(), 0x42);
    assert_eq!(rom.read_halfword(12).unwrap(), 0x500);
    assert_eq!(rom.read_halfword(14).unwrap(), 0x501);
    assert_eq!(rom.read_halfword(16).unwrap(), 0x502);
    assert_eq!(rom.read_word(20).unwrap(), 0x6000000);
    assert_eq!(rom.read_word(24).unwrap(), 0x6000001);
    assert_eq!(rom.read_word(28).unwrap(), 0x6000002);
    assert_eq!(rom.read_byte(32).unwrap(), 0x70); // 7
    assert_eq!(rom.read_byte(33).unwrap(), 0xF8); // 31, 0
    assert_eq!(rom.read_byte(34).unwrap(), 0xF0); // -1
    let read_pointer: RomPointer<u8> = rom.read(36).unwrap();
    assert_eq!(read_pointer, pointer11);
    let read_pointer: RomPointer<u16> = rom.read(40).unwrap();
    assert_eq!(read_pointer, pointer12);
    let read_pointer: RomPointer<u32> = rom.read(44).unwrap();
    assert_eq!(read_pointer, pointer13);
    assert_eq!(rom.read_word(48).unwrap(), 0x90000000); // 9
    assert_eq!(rom.read_word(52).unwrap(), 0x80000000); // -8
    assert_eq!(rom.read_byte(56).unwrap(), 0xA0);
}

#[test]
fn write_type_attribute() {
    let mut fire = create_rom(RomBase::FireRed);
    let mut ruby = create_rom(RomBase::Ruby);
    let mut emer = create_rom(RomBase::Emerald);

    // rom_struct!(ThreeSizes {
    //     #[for(base(Ruby, Sapphire), type(u8))]
    //     #[for(base(Emerald), type(u16))]
    //     u32 value;
    // });
    let struct_value = ThreeSizes { value: 0x1 };
    fire.write(0x0, struct_value).unwrap();
    assert_eq!(fire.read_word(0).unwrap(), 0x1);

    let struct_value = ThreeSizes { value: 0x2 };
    ruby.write(0x0, struct_value).unwrap();
    assert_eq!(ruby.read_byte(0).unwrap(), 0x2);

    let struct_value = ThreeSizes { value: 0x3 };
    emer.write(0x0, struct_value).unwrap();
    assert_eq!(emer.read_halfword(0).unwrap(), 0x3);
}

#[test]
fn write_default_attribute() {
    let mut fire = create_rom(RomBase::FireRed);
    let mut emer = create_rom(RomBase::Emerald);

    // rom_struct!(MissingField {
    //     u8 value1;
    //     #[for(base(Emerald), default(128))]
    //     u32 value2;
    // });
    let struct_value = MissingField {
        value1: 10,
        value2: 20,
    };
    fire.write(0x0, struct_value).unwrap();
    assert_eq!(fire.read_byte(0).unwrap(), 10);
    assert_eq!(fire.read_word(4).unwrap(), 20);

    let struct_value = MissingField {
        value1: 10,
        value2: 0xA7EE4,
    };
    emer.write(0x0, struct_value).unwrap();
    assert_eq!(emer.read_byte(0).unwrap(), 10);
    assert_eq!(emer.read_word(4).unwrap(), 0xffffffff);
}

#[test]
fn write_swap_attribute() {
    let mut fire = create_rom(RomBase::FireRed);
    let mut emer = create_rom(RomBase::Emerald);

    // rom_struct!(SwapFields {
    //     u8 value1;
    //     #[for(base(Ruby, Sapphire, Emerald), swap(value3))]
    //     void* value2;
    //     void* value3;
    // });
    let struct_value = SwapFields {
        value1: 10,
        value2: RomPointer::NoData(0x10),
        value3: RomPointer::NoData(0x14),
    };
    fire.write(0x0, struct_value.clone()).unwrap();
    emer.write(0x0, struct_value).unwrap();

    assert_eq!(fire.read_byte(0).unwrap(), emer.read_byte(0).unwrap());
    // Assert that the two values are inverted
    assert_eq!(fire.read_offset(4).unwrap(), emer.read_offset(8).unwrap());
    assert_eq!(fire.read_offset(8).unwrap(), emer.read_offset(4).unwrap());
}

#[test]
fn write_vector_structs() {
    // rom_struct!(U8Vector {
    //     u8 length;
    //     u8 values{$length};
    // });
    // Write a vector of length 3
    let mut rom = create_rom(RomBase::FireRed);
    let struct_value = U8Vector {
        length: 5,
        values: RomVector::<u8>::new(vec![0x1, 0x2, 0x3, 0x4, 0x5]),
    };
    rom.write(0x0, struct_value).unwrap();

    assert_eq!(rom.read_byte(0).unwrap(), 5);
    assert_eq!(rom.read_offset(4).unwrap(), 8);

    assert_eq!(rom.read_byte(8).unwrap(), 0x1);
    assert_eq!(rom.read_byte(9).unwrap(), 0x2);
    assert_eq!(rom.read_byte(10).unwrap(), 0x3);
    assert_eq!(rom.read_byte(11).unwrap(), 0x4);
    assert_eq!(rom.read_byte(12).unwrap(), 0x5);

    // rom_struct!(ComplexVector {
    //     u8 data{$length * $typesize};
    //     u32 length;
    //     u32 typesize;
    // });
    let mut rom = create_rom(RomBase::FireRed);
    let struct_value = ComplexVector {
        data: RomVector::Valid {
            offset: 0x40,
            clear_size: 4,
            data: vec![0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF],
        },
        length: 2,
        typesize: 3,
    };
    // Save some data to 0x40 to make sure it is cleared
    rom.write_word(0x40, 0).unwrap();

    rom.write(0, struct_value).unwrap();

    assert_eq!(rom.read_offset(0).unwrap(), 12);
    assert_eq!(rom.read_word(4).unwrap(), 2);
    assert_eq!(rom.read_word(8).unwrap(), 3);

    assert_eq!(rom.read_word(0x40).unwrap(), 0xFFFF_FFFF);

    assert_eq!(rom.read_byte(12).unwrap(), 0xAA);
    assert_eq!(rom.read_byte(13).unwrap(), 0xBB);
    assert_eq!(rom.read_byte(14).unwrap(), 0xCC);
    assert_eq!(rom.read_byte(15).unwrap(), 0xDD);
    assert_eq!(rom.read_byte(16).unwrap(), 0xEE);
    assert_eq!(rom.read_byte(17).unwrap(), 0xFF);
}
