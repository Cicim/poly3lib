use proc_macro2::{Ident, TokenStream};
use quote::quote;

use crate::parser::ValueField;

/// Generates the `RomValues` trait for the given struct.
pub fn generate(name: &Ident, fields: &Vec<ValueField>) -> TokenStream {
    // Generate the three methods
    let read_fast = generate_read_fast(fields);
    let read_all = generate_read_all(fields);
    let write = generate_write(fields);

    quote! {
        impl rom_data::values::RomValues for #name {
            fn read_fast(rom: &rom_data::RomData) -> Result<Self, rom_data::values::RomValueError> {
                #read_fast
            }
            fn read_all(rom: &rom_data::RomData) -> Result<Self, rom_data::values::RomValueError> {
                #read_all
            }
            fn write(self, rom: &rom_data::RomData) -> Result<(), rom_data::values::RomValueError> {
                #write
            }
        }
    }
}

/// Generates the `RomValues::read_fast` method for the given struct.
pub fn generate_read_fast(fields: &Vec<ValueField>) -> TokenStream {
    quote! {
        todo!()
    }
}

/// Generates the `RomValues::read_all` method for the given struct.
pub fn generate_read_all(fields: &Vec<ValueField>) -> TokenStream {
    quote! {
        todo!()
    }
}

/// Generates the `RomValues::write` method for the given struct.
pub fn generate_write(fields: &Vec<ValueField>) -> TokenStream {
    quote! {
        todo!()
    }
}
