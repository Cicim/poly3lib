use proc_macro2::TokenStream;
use quote::quote;

use crate::parser_types::{GetSizeAndAlignment, ParsedStruct, TypeDimension};

/// Generates the code for the RomSizedType trait of this struct.
pub fn generate_sized_implementation(parsed: &ParsedStruct) -> TokenStream {
    let name = &parsed.name;

    let get_size = generate_get_size(parsed);
    let get_alignment = generate_get_alignment(parsed);

    quote! {
        impl rom_data::types::RomSizedType for #name {
            fn get_size(rom: &rom_data::RomData) -> usize {
                #get_size
            }

            fn get_alignment(rom: &rom_data::RomData) -> usize {
                #get_alignment
            }
        }
    }
}

/// Generate the body of the `RomSizedType::get_size` function
fn generate_get_size(parsed: &ParsedStruct) -> TokenStream {
    let name = &parsed.name;

    let mut code = quote! { let mut size = 0; };

    // The rule for computing the size of a struct is that for each field, you need
    // to align the current size to the alignment of the field, and then add the size
    // of the field.

    // For each field, you need to:
    // - Align the current size to the alignment of the field
    // - Add the size of the field
    for (i, field) in parsed.fields.iter().enumerate() {
        if i > 0 {
            // Get the alignment code
            let alignment_code = field.get_alignment().get_dimension_code();
            code.extend(quote! { let align = #alignment_code; });

            // Perform the alignment
            code.extend(quote! { size = (size + align - 1) & !(align - 1); });
        }

        // Add the current size
        let size_code = field.get_size().get_dimension_code();
        code.extend(quote! { size += #size_code; });
    }

    // Align to the struct alignment
    code.extend(quote! {
        if size % 4 == 0 {
            return size
        }

        let alignment = <#name as rom_data::types::RomSizedType>::get_alignment(rom);
        size = (size + alignment - 1) & !(alignment - 1);
    });

    quote! {
        #code

        size
    }
}

/// Generate the body of the `RomSizedType::get_alignment` function
fn generate_get_alignment(parsed: &ParsedStruct) -> TokenStream {
    let mut alignment = 1;

    let mut pieces_of_alignment = Vec::new();

    for field in parsed.fields.iter() {
        // Get the alignment of this type
        match field.get_alignment() {
            // If you know the alignment, you can reduce it immediately
            TypeDimension::Known(known) => alignment = alignment.max(known),

            // Push every new piece to obtain that code here. This is done so that the alignment
            // code can be generated only if it is needed
            TypeDimension::Code(code_to_obtain_it) => pieces_of_alignment.push(code_to_obtain_it),
        }

        // If you reach the maximum with the known one, you can stop
        if alignment == 4 {
            break;
        }
    }

    // If you reached anything but the maximum, you need to generate the code to obtain the
    // alignment by reading the rest of the field types.
    if alignment != 4 {
        // If there are no pieces of alignment, it means that all the fields have a known
        // alignment, so you can just return that
        if pieces_of_alignment.is_empty() {
            return quote! { #alignment };
        }

        // Otherwise, you need to generate the code to obtain the alignment
        let mut code = quote! { let mut alignment = #alignment; };

        for next in pieces_of_alignment {
            code = quote! {
                #code

                let next = #next;
                alignment = alignment.max(next);
                if alignment == 4 {
                    return 4;
                }
            }
        }

        return quote! {
            {
                #code
                return alignment
            }
        };
    }

    quote! { #alignment }
}
