// Important so that the structs work in this context
use crate::{self as rom_data, RomBase, RomData};

use struct_macro::rom_struct;

// Test files
mod rom_values;
mod struct_read;
mod struct_size;
mod struct_write;

// ANCHOR Utility functions
/// Returns a RomData with the given base.
fn create_rom(base: RomBase) -> RomData {
    RomData::new(base, 128)
}

// Define all the structs used for testing.
// ANCHOR Single types
rom_struct!(SingleU8 { u8 value; });
rom_struct!(SingleU16 { u16 value; });
rom_struct!(SingleU32 { u32 value; });
rom_struct!(SingleI8 { i8 value; });
rom_struct!(SingleI16 { i16 value; });
rom_struct!(SingleI32 { i32 value; });
rom_struct!(SingleBool8 { bool8 value; });
rom_struct!(SingleBool16 { bool16 value; });
rom_struct!(SingleBool32 { bool32 value; });

rom_struct!(SingleU8Array { u8 value[3]; });
rom_struct!(SingleU16Array { u16 value[3]; });
rom_struct!(SingleU32Array { u32 value[3]; });

// ANCHOR Alignment and size test
rom_struct!(U8PaddingU16U8Padding {
    u8 value1;
    u16 value2;
    u8 value3;
});
rom_struct!(U16PaddingU32U8Padding {
    u16 value1;
    u32 value2;
    u8 value3;
});

// ANCHOR BitField tests
rom_struct!(BitFieldU8 {
    u8 value1 : 4;
    u8 value2 : 4;
});
rom_struct!(BitFieldU16 {
    u16 value1 : 4;
    u16 value2 : 4;
    u16 value3 : 4;
    u16 value4 : 4;
});
rom_struct!(BitFieldU32 {
    u32 value1 : 8;
    u32 value2 : 4;
    u32 value3 : 4;
    u32 value4 : 8;
    u32 value5 : 4;
    u32 value6 : 4;
});
rom_struct!(BitFieldPair {
    u8 field1 : 5;
    u8 field2 : 4;
});

// ANCHOR Pointers
rom_struct!(SingleU8Pointer { u8 *value; });
rom_struct!(SingleU16Pointer { u16 *value; });
rom_struct!(SingleU32Pointer { u32 *value; });

rom_struct!(MultiplePointerDereference { u8 **value; });

// ANCHOR Complex examples
rom_struct!(ComplexStruct {
    u8 value1;
    u16 value2;
    u32 value3;
    u8 value4[3];
    u16 value5[3];
    u32 value6[3];
    u8 value7 : 4;
    u8 value8 : 5;
    u8 value9 : 3;
    s8 value10 : 4;

    u8 *value11;
    u16 *value12;
    u32 *value13;

    u32 value14 : 4;
    i32 value15 : 4;

    u8 value16;
});

// ANCHOR Attributes
rom_struct!(ThreeSizes {
    #[for(base(Ruby, Sapphire), type(u8))]
    #[for(base(Emerald), type(u16))]
    u32 value;
});

rom_struct!(ThreeSizesTriple {
    #[for(base(Ruby, Sapphire), type(u8))]
    #[for(base(Emerald), type(u16))]
    u32 value1, value2, value3;
});

rom_struct!(MissingField {
    u8 value1;

    #[for(base(Emerald), default(128))]
    u32 value2;
});

rom_struct!(TwoMissingFields {
    u8 value1;

    #[for(base(Ruby), default(0))]
    u32 value2;

    u8 value3;

    #[for(base(Emerald), default(1))]
    u16 value4;
});

// ANCHOR Vector
rom_struct!(U8Vector {
    u8 length;
    u8 values{$length};
});

rom_struct!(ComplexVector {
    u8 data{$length * $typesize};
    u32 length;
    u32 typesize;
});
