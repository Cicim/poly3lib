use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::parser_types::{
    build_attribute_condition, BaseType, BuildToTokens, DerivedType, GetSizeAndAlignment,
    ParsedStruct, SizedBaseType, StructAttributeAction, StructBasicField, StructBitFields,
    StructField,
};

static READ_PREFIX: &str = "struct_value_";

/// Generates the implementation of RomReadableType for the given struct
pub fn generate_readable_implementation(parsed: &ParsedStruct) -> TokenStream {
    let name = &parsed.name;

    let read = generate_read_from(parsed);

    quote! {
        impl rom_data::types::RomReadableType for #name {
            fn read_from(rom: &rom_data::RomData, offset: usize) -> Result<Self, rom_data::RomIoError> {
                #read
            }
        }
    }
}

/// Generates the `RomReadableType::read_from` function for the given struct.
fn generate_read_from(parsed: &ParsedStruct) -> TokenStream {
    let mut read_fields = quote! {
        // Start the offset counter
        let mut offset = offset;
    };

    let mut constructor = quote! {};

    for (i, field) in parsed.fields.iter().enumerate() {
        // Every field but the first, needs to be aligned to its preferred alignment
        if i != 0 {
            let alignment = field.get_alignment().get_dimension_code();
            read_fields.extend(quote! {
                let align = #alignment;
                // Align the offset
                offset = (offset + align - 1) & !(align - 1);
            });
        }

        let code = match field {
            StructField::Field(field) => build_basic_field(field),
            StructField::BitField(field) => build_bitfields(field),
        };
        read_fields.extend(code);

        // Add the field names to the struct
        match field {
            StructField::Field(StructBasicField { name, .. }) => {
                let read_name = format_ident!("{}{}", READ_PREFIX, name);
                constructor.extend(quote! {
                    #name: #read_name,
                })
            }
            StructField::BitField(StructBitFields { names, .. }) => {
                for name in names {
                    let read_name = format_ident!("{}{}", READ_PREFIX, name);
                    constructor.extend(quote! {
                        #name: #read_name,
                    })
                }
            }
        }

        // Once the field is read, the offset needs to be incremented by the size of the field
        // (if it is not the last field)
        if i != parsed.fields.len() - 1 {
            let size = field.get_size().get_dimension_code();
            read_fields.extend(quote! {
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

        #read_fields

        Ok(Self {
            #constructor
        })
    }
}

// ANCHOR Read fields with attributes
/// Builds the code to read a basic struct field with a single derived type
fn build_basic_field(field: &StructBasicField) -> TokenStream {
    // Build the field identifier
    let name = format_ident!("{}{}", READ_PREFIX, field.name);

    // The method slightly changes based on the type.
    let ty = build_derived_type(&field.ty);

    // If there are no attributes you can stop here.
    if field.attributes.is_empty() {
        return quote! {
            let #name = #ty;
        };
    }

    // If there are attributes that can alter the read behavior, we have to consider them
    let attr_if_chain = build_attribute_condition(
        &field.attributes,
        |action| {
            Some(match action {
                // Instead of reading the type with ty, you should read it with other
                StructAttributeAction::Type(other) => {
                    let ty = build_base_type(other);

                    quote! { #ty as _ }
                }

                // Instead of reading the value, set the given default
                StructAttributeAction::Default(value) => quote!(#value),
            })
        },
        ty,
    );

    quote! {
        // Read the field
        let #name = #attr_if_chain;
    }
}

/// Builds the expression necessary to read a derived type.
fn build_derived_type(ty: &DerivedType) -> TokenStream {
    match ty {
        DerivedType::Base(base_ty) => build_base_type(base_ty),

        // For pointers and arrays, just use their ReadSizedType derive
        ty => {
            let ty = ty.build_to_tokens();

            quote! {
                rom.read::<#ty>(offset)?
            }
        }
    }
}

fn build_base_type(base_ty: &BaseType) -> TokenStream {
    match base_ty {
        BaseType::Void => unreachable!("Void should never be read on its own"),
        BaseType::Struct(struct_name) => quote! {
            rom.read_from::<#struct_name>(offset)?;
        },
        BaseType::Integer(int_ty) => {
            let bytes = int_ty.bits() / 8;
            let fn_name = match bytes {
                1 => quote! { read_byte },
                2 => quote! { read_halfword },
                4 => quote! { read_word },
                _ => unreachable!("Invalid integer size"),
            };

            match int_ty {
                SizedBaseType::Unsigned(_) => quote! {
                    rom.#fn_name(offset)?
                },
                SizedBaseType::Signed(_) => {
                    let ty = format_ident!("i{}", int_ty.bits());
                    quote! {
                        rom.#fn_name(offset)? as #ty
                    }
                }
                SizedBaseType::Boolean(_) => quote! {
                    rom.#fn_name(offset)? != 0
                },
            }
        }
    }
}

// ANCHOR Read bitfields
fn build_bitfields(field: &StructBitFields) -> TokenStream {
    // Build the bitfields type
    let bitfields = field.build_to_tokens();

    // Whether to convert to bool
    let convert_to_bool = match field.ty {
        SizedBaseType::Boolean(_) => quote! { != 0 },
        _ => quote! {},
    };

    // Compose all the fields
    let mut assign_to_all = quote! {};
    for (i, name) in field.names.iter().enumerate() {
        let name = format_ident!("{}{}", READ_PREFIX, name);
        assign_to_all.extend(quote! {
            let #name = bitfield_value[#i] #convert_to_bool;
        });
    }

    quote! {
        #bitfields
        let bitfield_value = bitfields.read_from(rom, offset)?;

        #assign_to_all
    }
}
