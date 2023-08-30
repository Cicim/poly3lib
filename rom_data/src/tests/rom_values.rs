use crate::{self as rom_data, values::RomValueError};
use rom_data::RomValues;

use super::create_rom;

#[derive(RomValues)]
struct SimpleValues {
    #[value(Emerald 0x0 Word)]
    test: u32,
}

#[test]
fn simple_values() {
    let mut rom = create_rom(rom_data::RomBase::Emerald);

    let values = SimpleValues::read_values(&rom).unwrap();
    assert!(values.test == 0xFFFF_FFFF);

    // Test the write function
    let values = SimpleValues { test: 0xABCDEF08 };
    values.write_values(&mut rom).unwrap();

    assert!(rom.read_word(0).unwrap() == 0xABCDEF08);

    // Make sure that for any other ROM type it returns an error
    let mut rom = create_rom(rom_data::RomBase::FireRed);
    assert!(SimpleValues::read_values(&mut rom).is_err());
}

#[derive(RomValues)]
struct SimpleValuesWithTransform {
    #[value(Emerald 0x0 Word StackedHalfwords)]
    test: u16,
}

#[test]
fn simple_values_with_transform() {
    let mut rom = create_rom(rom_data::RomBase::Emerald);

    rom.write_word(0, 0x0123_0123).unwrap();

    let values = SimpleValuesWithTransform::read_values(&rom).unwrap();
    assert!(values.test == 0x0123);

    // Test write
    let values = SimpleValuesWithTransform { test: 0x4567 };
    values.write_values(&mut rom).unwrap();

    assert!(rom.read_word(0).unwrap() == 0x4567_4567);
}

#[derive(RomValues)]
struct ValuesWithLslMov {
    #[value(Emerald 0x0 MovLsl)]
    test: u32,
}

#[test]
fn values_with_lsl_mov() {
    let mut rom = create_rom(rom_data::RomBase::Emerald);

    // Write mov r1, #42
    rom.write_halfword(0, 0x212A).unwrap();
    // Write lsl r1, r1, #3
    rom.write_halfword(2, 0x00C9).unwrap();

    let values = ValuesWithLslMov::read_values(&rom).unwrap();
    assert!(values.test == 42 << 3);

    // Test write
    let values = ValuesWithLslMov { test: 0x12_000000 };
    values.write_values(&mut rom).unwrap();
    let values = ValuesWithLslMov::read_values(&rom).unwrap();
    assert!(values.test == 0x12_000000);
}

#[derive(RomValues)]
struct TwoRoms {
    #[value(Emerald 0x0 Word)]
    #[value(FireRed 0x4 Word)]
    value: u32,
}

#[test]
fn values_with_two_roms() {
    let mut emer = create_rom(rom_data::RomBase::Emerald);
    emer.write_word(0, 0x1234).unwrap();

    let values = TwoRoms::read_values(&emer).unwrap();
    assert!(values.value == 0x1234);

    emer.base = rom_data::RomBase::FireRed;
    let values = TwoRoms::read_values(&emer).unwrap();
    assert!(values.value == 0xFFFF_FFFF);
}

#[derive(RomValues, Debug)]
struct ValuesCoincide {
    #[value(Emerald 0x0 Word)]
    #[value(Emerald 0x4 Word)]
    value: u32,
}

#[test]
fn value_mismatch() {
    let mut rom = create_rom(rom_data::RomBase::Emerald);

    // Write the value
    rom.write_word(0, 0x1234).unwrap();
    rom.write_word(4, 0x5678).unwrap();

    // Read the value
    let values = ValuesCoincide::read_values(&rom);
    assert_eq!(values.unwrap_err(), RomValueError::Mismatch);

    // Test that it reads correctly if no mismatch
    rom.write_word(4, 0x1234).unwrap();

    let values = ValuesCoincide::read_values(&rom).unwrap();
    assert!(values.value == 0x1234);
}

#[derive(RomValues)]
struct TransformChain {
    #[value(Emerald 0x0 Word StackedHalfwords Not16)]
    value: u16,
}

#[test]
fn values_with_transform_chain() {
    let mut rom = create_rom(rom_data::RomBase::Emerald);

    // Write 0x0FFE_0FFE
    rom.write_word(0, 0x0FFE_0FFE).unwrap();

    // When reading we expect to:
    // 1. Invert the stacked halfwords -> 0x0FFE
    // 2. Invert the not-16 -> 0xF001

    let values = TransformChain::read_values(&rom).unwrap();
    assert_eq!(values.value, 0xF001);

    // Write a value
    let values = TransformChain { value: 0x0001 };
    values.write_values(&mut rom).unwrap();
    assert_eq!(rom.read_word(0).unwrap(), 0xFFFE_FFFE);
}
