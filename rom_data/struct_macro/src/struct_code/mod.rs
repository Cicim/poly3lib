mod read;
mod sized;
mod write;

use proc_macro2::TokenStream;
use quote::quote;

use crate::parser_types::*;

/// If the given condition is true, it returns the given value, otherwise an empty quote
macro_rules! quote_if {
    ($cond:expr, $val:tt) => {
        if $cond {
            quote! $val
        } else {
            quote! {}
        }
    };
}

/// Builds the code for the struct as well all its trait implementations
pub fn build(parsed: ParsedStruct) -> TokenStream {
    let body = build_struct_body(&parsed);
    let sized = sized::generate_sized_implementation(&parsed);
    let readable = read::generate_readable_implementation(&parsed);
    let writable = write::generate_writable_implementation(&parsed);

    quote! {
        // Struct bodys
        #body

        // RomSizedType impl
        #sized

        // RomReadableType impl
        #readable

        // RomWritableType impl
        #writable
    }
}

/// Builds the struct body itself
pub fn build_struct_body(parsed: &ParsedStruct) -> TokenStream {
    // Build the derives for the struct
    let derive_debug = quote_if!(!parsed.flags.no_debug, { Debug , });
    let derives = quote! {
        #[derive(#derive_debug Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
    };

    // Create the visibility modifier (if allowed)
    let vis = quote_if!(!parsed.flags.private, { pub });
    // Get the struct identifier
    let name = &parsed.name;

    // Build the fields of the struct
    let mut fields = quote! {};

    for field in &parsed.fields {
        fields.extend(build_field(field, &vis));
    }

    quote! {
        #derives
        #vis struct #name {
            #fields
        }
    }
}

/// Builds a struct field (or bitfield)
fn build_field(field: &StructField, vis: &TokenStream) -> TokenStream {
    use StructField::*;
    match field {
        // Build a basic field with a derived type
        Field(StructBasicField {
            name,
            ty,
            attributes: _,
        }) => {
            // Build the type
            let ty = ty.build_to_tokens();

            quote! {
                #vis #name: #ty,
            }
        }

        // Build a bitfield
        BitField(StructBitFields { ty, sizes, names }) => {
            // Build the type
            let ty = ty.build_to_tokens();

            // For each field in the bitfields
            let mut bit_fields = quote! {};
            for (name, _) in names.iter().zip(sizes.iter()) {
                // Build the field
                let field = quote! {
                    #vis #name: #ty,
                };

                // Add the field to the bitfields
                bit_fields.extend(field);
            }

            bit_fields
        }
    }
}
