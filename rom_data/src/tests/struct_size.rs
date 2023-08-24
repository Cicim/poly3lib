use crate::types::RomSizedType;

use super::*;

#[test]
fn test_sizes() {
    let fire = create_rom(RomBase::FireRed);

    // Single values
    assert_eq!(SingleU8::get_size(&fire), 1);
    assert_eq!(SingleU16::get_size(&fire), 2);
    assert_eq!(SingleU32::get_size(&fire), 4);

    assert_eq!(SingleI8::get_size(&fire), 1);
    assert_eq!(SingleI16::get_size(&fire), 2);
    assert_eq!(SingleI32::get_size(&fire), 4);

    assert_eq!(SingleBool8::get_size(&fire), 1);
    assert_eq!(SingleBool16::get_size(&fire), 2);
    assert_eq!(SingleBool32::get_size(&fire), 4);

    assert_eq!(SingleU8Array::get_size(&fire), 3);
    assert_eq!(SingleU16Array::get_size(&fire), 6);
    assert_eq!(SingleU32Array::get_size(&fire), 12);

    // BitFields
    assert_eq!(U8PaddingU16U8Padding::get_size(&fire), 6);
    assert_eq!(U16PaddingU32U8Padding::get_size(&fire), 12);

    assert_eq!(BitFieldU8::get_size(&fire), 1);
    assert_eq!(BitFieldU16::get_size(&fire), 2);
    assert_eq!(BitFieldU32::get_size(&fire), 4);

    assert_eq!(BitFieldPair::get_size(&fire), 2);

    // Pointers
    assert_eq!(SingleU8Pointer::get_size(&fire), 4);
    assert_eq!(SingleU16Pointer::get_size(&fire), 4);
    assert_eq!(SingleU32Pointer::get_size(&fire), 4);
    assert_eq!(MultiplePointerDereference::get_size(&fire), 4);

    // Complex
    assert_eq!(ComplexStruct::get_size(&fire), 60);

    // Attributes
    let ruby = create_rom(RomBase::Ruby);
    let emer = create_rom(RomBase::Emerald);

    assert_eq!(ThreeSizes::get_size(&ruby), 1);
    assert_eq!(ThreeSizes::get_size(&emer), 2);
    assert_eq!(ThreeSizes::get_size(&fire), 4);

    assert_eq!(ThreeSizesTriple::get_size(&ruby), 3);
    assert_eq!(ThreeSizesTriple::get_size(&emer), 6);
    assert_eq!(ThreeSizesTriple::get_size(&fire), 12);

    assert_eq!(MissingField::get_size(&ruby), 8);
    assert_eq!(MissingField::get_size(&fire), 8);
    assert_eq!(MissingField::get_size(&emer), 1);

    assert_eq!(TwoMissingFields::get_size(&fire), 12);
    assert_eq!(TwoMissingFields::get_size(&ruby), 4);
    assert_eq!(TwoMissingFields::get_size(&emer), 12);
}

#[test]
fn test_alignments() {
    let fire = create_rom(RomBase::FireRed);

    // Single values
    assert_eq!(SingleU8::get_alignment(&fire), 1);
    assert_eq!(SingleU16::get_alignment(&fire), 2);
    assert_eq!(SingleU32::get_alignment(&fire), 4);

    assert_eq!(SingleI8::get_alignment(&fire), 1);
    assert_eq!(SingleI16::get_alignment(&fire), 2);
    assert_eq!(SingleI32::get_alignment(&fire), 4);

    assert_eq!(SingleBool8::get_alignment(&fire), 1);
    assert_eq!(SingleBool16::get_alignment(&fire), 2);
    assert_eq!(SingleBool32::get_alignment(&fire), 4);

    assert_eq!(SingleU8Array::get_alignment(&fire), 1);
    assert_eq!(SingleU16Array::get_alignment(&fire), 2);
    assert_eq!(SingleU32Array::get_alignment(&fire), 4);

    // BitFields
    assert_eq!(U8PaddingU16U8Padding::get_alignment(&fire), 2);
    assert_eq!(U16PaddingU32U8Padding::get_alignment(&fire), 4);

    assert_eq!(BitFieldU8::get_alignment(&fire), 1);
    assert_eq!(BitFieldU16::get_alignment(&fire), 2);
    assert_eq!(BitFieldU32::get_alignment(&fire), 4);

    assert_eq!(BitFieldPair::get_alignment(&fire), 1);

    // Pointers
    assert_eq!(SingleU8Pointer::get_alignment(&fire), 4);
    assert_eq!(SingleU16Pointer::get_alignment(&fire), 4);
    assert_eq!(SingleU32Pointer::get_alignment(&fire), 4);
    assert_eq!(MultiplePointerDereference::get_alignment(&fire), 4);

    // Complex
    assert_eq!(ComplexStruct::get_alignment(&fire), 4);

    // Attributes
    let ruby = create_rom(RomBase::Ruby);
    let emer = create_rom(RomBase::Emerald);

    assert_eq!(ThreeSizes::get_alignment(&ruby), 1);
    assert_eq!(ThreeSizes::get_alignment(&emer), 2);
    assert_eq!(ThreeSizes::get_alignment(&fire), 4);

    assert_eq!(ThreeSizesTriple::get_alignment(&ruby), 1);
    assert_eq!(ThreeSizesTriple::get_alignment(&emer), 2);
    assert_eq!(ThreeSizesTriple::get_alignment(&fire), 4);

    assert_eq!(MissingField::get_alignment(&fire), 4);
    assert_eq!(MissingField::get_alignment(&ruby), 4);
    assert_eq!(MissingField::get_alignment(&emer), 1);

    assert_eq!(TwoMissingFields::get_alignment(&fire), 4);
    assert_eq!(TwoMissingFields::get_alignment(&ruby), 2);
    assert_eq!(TwoMissingFields::get_alignment(&emer), 4);
}
