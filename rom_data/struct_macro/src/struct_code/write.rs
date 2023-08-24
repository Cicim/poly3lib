use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::parser_types::{
    build_attribute_condition, BaseType, BuildToTokens, DerivedType, GetSizeAndAlignment,
    ParsedStruct, SizedBaseType, StructAttributeAction, StructBasicField, StructBitFields,
    StructField,
};

pub fn generate_writable_implementation(parsed: &ParsedStruct) -> TokenStream {
    let name = &parsed.name;

    let write = generate_write_to(parsed);

    quote! {
        impl rom_data::types::RomWritableType for #name {
            fn write_to(self, rom: &mut rom_data::RomData, offset: usize) -> Result<(), rom_data::RomIoError> {
                #write
            }
        }
    }
}

/// Generates the `RomWritableType::write_to` function for the given struct.
fn generate_write_to(parsed: &ParsedStruct) -> TokenStream {
    let mut write_fields = quote! {
        // Start the offset counter
        let mut offset = offset;
    };

    for (i, field) in parsed.fields.iter().enumerate() {
        // Every field but the first, needs to be aligned to its preferred alignment
        if i != 0 {
            let alignment = field.get_alignment().get_dimension_code();
            write_fields.extend(quote! {
                let align = #alignment;
                // Align the offset
                offset = (offset + align - 1) & !(align - 1);
            });
        }

        let code = match field {
            StructField::Field(field) => build_basic_field(field),
            StructField::BitField(field) => build_bitfields(field),
        };
        write_fields.extend(code);

        // Once the field is written, the offset needs to be incremented by the size of the field
        // (if it is not the last field)
        if i != parsed.fields.len() - 1 {
            let size = field.get_size().get_dimension_code();
            write_fields.extend(quote! {
                offset += #size;
            });
        }
    }

    quote! {
        // Assert the struct alignment
        let struct_alignment = <Self as rom_data::types::RomSizedType>::get_alignment(rom);
        if offset % struct_alignment != 0 {
            return Err(rom_data::RomIoError::Misaligned(offset, struct_alignment as u8))
        }

        #write_fields

        Ok(())
    }
}

fn build_basic_field(field: &StructBasicField) -> TokenStream {
    let write_fn = build_attribute_condition(
        &field.attributes,
        |action| match action {
            StructAttributeAction::Type(ty) => Some(build_base_type(ty)),
            StructAttributeAction::Default(_) => None,
        },
        build_derived_type(&field.ty),
    );

    let name = &field.name;
    quote! {
        let value = self.#name;
        #write_fn
    }
}

fn build_derived_type(ty: &DerivedType) -> TokenStream {
    match ty {
        DerivedType::Base(base_ty) => build_base_type(base_ty),

        ty => {
            let ty = ty.build_to_tokens();
            quote! {
                rom.write::<#ty>(offset, value)?;
            }
        }
    }
}

fn build_base_type(ty: &BaseType) -> TokenStream {
    match ty {
        BaseType::Void => unreachable!("Void should never be written without a pointer"),

        BaseType::Struct(name) => quote!(rom.write::<#name>(offset, value)?;),
        BaseType::Integer(int_ty) => build_int_type(int_ty),
    }
}

fn build_int_type(int_ty: &SizedBaseType) -> TokenStream {
    // Get the function to write
    let write_method = match int_ty.bits() / 8 {
        1 => quote!(rom.write_byte),
        2 => quote!(rom.write_halfword),
        4 => quote!(rom.write_word),
        _ => unreachable!("Invalid integer size"),
    };

    match int_ty {
        SizedBaseType::Unsigned(bits) | SizedBaseType::Signed(bits) => {
            let ty = format_ident!("u{}", bits);
            quote!(#write_method(offset, value as #ty)?;)
        }
        SizedBaseType::Boolean(_) => quote!(#write_method(offset, if value { 1 } else { 0 } )?;),
    }
}

fn build_bitfields(field: &StructBitFields) -> TokenStream {
    // Create the bitfields type
    let bitfields = field.build_to_tokens();

    // Create the array of bitfields
    let mut array_of_values = quote! {};

    // If the type is boolean
    let is_boolean = field.ty.is_boolean();

    for name in field.names.iter() {
        if is_boolean {
            array_of_values.extend(quote! {
                if self.#name { 1 } else { 0 },
            });
        } else {
            array_of_values.extend(quote!(self.#name,));
        }
    }

    quote! {
        #bitfields
        let value = [#array_of_values];
        bitfields.write_to(rom, offset, value)?;
    }
}
