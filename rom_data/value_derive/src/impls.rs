use proc_macro2::{Ident, TokenStream};
use quote::quote;

use crate::parser::{ValueAttribute, ValueField};

/// Generates the `RomValues` trait for the given struct.
pub fn generate(name: &Ident, fields: &Vec<ValueField>) -> TokenStream {
    // Generate the three methods
    let read_values = generate_read_values(fields);
    let write_values = generate_write_values(fields);

    quote! {
        impl rom_data::values::RomValues for #name {
            fn read_values(rom: &rom_data::RomData) -> Result<Self, rom_data::values::RomValueError> {
                #read_values
            }
            fn write_values(self, rom: &mut rom_data::RomData) -> Result<(), rom_data::values::RomValueError> {
                #write_values
            }
        }
    }
}

// ANCHOR Read
/// Generates the `RomValues::read_values` method for the given struct.
fn generate_read_values(fields: &Vec<ValueField>) -> TokenStream {
    let mut value_reading = quote!();
    let mut struct_composition = quote!();

    for field in fields {
        // Get the name and type of the field to generate the result line
        let name = &field.name;
        let ty = &field.ty;

        // Generate the value reading line
        let code = generate_value_reading_code(&field.attrs);

        value_reading.extend(quote! {
            let read_option = #code;
            let #name = match read_option {
                Some(x) => x as #ty,
                None => return Err(rom_data::values::RomValueError::NotImplemented),
            };
        });

        struct_composition.extend(quote!(#name,));
    }

    quote! {
        fn check(last: Option<u32>, current: u32) -> Result<(), rom_data::values::RomValueError> {
            if let Some(last) = last {
                if last != current {
                    return Err(rom_data::values::RomValueError::Mismatch);
                }
            }
            Ok(())
        }

        #value_reading

        Ok(Self {
            #struct_composition
        })
    }
}

/// Returns the code needed to read a field given said field.
fn generate_value_reading_code(attrs: &Vec<ValueAttribute>) -> TokenStream {
    // Divide the attributes into buckets depending on the base
    let mut buckets = std::collections::HashMap::new();
    for attr in attrs {
        buckets
            .entry(&attr.base)
            .or_insert_with(Vec::new)
            .push(attr);
    }

    let mut match_statement = quote!();

    for (base, attrs) in buckets {
        // Generate the read code for this base
        let code = generate_value_reading_code_for_attributes(&attrs);

        match_statement.extend(quote! {
            rom_data::RomBase::#base => {
                let mut last: Option<u32> = None;

                #code

                last
            }
        });
    }

    quote! {
        match rom.base {
            #match_statement

            _ => return Err(rom_data::values::RomValueError::NotImplemented)
        }
    }
}

/// Generate the lines to read all the attributes with the same base.
fn generate_value_reading_code_for_attributes(attrs: &Vec<&ValueAttribute>) -> TokenStream {
    let mut value_reading = quote!();

    for attr in attrs {
        let mut code_under_if = quote!();

        for offset in attr.offsets.iter() {
            let method = &attr.method;

            // Read operations:
            // 1. Read the current value
            code_under_if.extend(quote! {
                let current = rom_data::values::RomValueType::#method.read(rom, #offset)?;
            });
            // 2. Invert the transformations
            for transform in attr.transforms.iter() {
                code_under_if.extend(quote! {
                    let current = rom_data::values::RomValueTransformation::#transform.invert_transform(current);
                });
            }
            // 3. Check if the value matches the last
            code_under_if.extend(quote! {
                check(last, current)?;
            });
            // 4. Update the last value
            code_under_if.extend(quote! {
                last = Some(current);
            });
        }

        // TODO Check the additional condition
        value_reading.extend(quote! { {#code_under_if} })
    }

    quote! {#value_reading}
}

// ANCHOR Write
/// Generates the `RomValues::write_values` method for the given struct.
fn generate_write_values(fields: &Vec<ValueField>) -> TokenStream {
    let mut write_fields_code = quote!();

    // For each field, write the attribute
    for field in fields {
        let name = &field.name;
        let code = generate_write_value_attributes(&field.attrs);

        write_fields_code.extend(quote! {
            let value = self.#name as u32;

            #code
        });
    }

    quote! {
        #write_fields_code

        Ok(())
    }
}

/// Returns the code needed to write a field given its attributes.
fn generate_write_value_attributes(attrs: &Vec<ValueAttribute>) -> TokenStream {
    // Divide the attributes by rom type
    let mut buckets = std::collections::HashMap::new();
    for attr in attrs {
        buckets
            .entry(&attr.base)
            .or_insert_with(Vec::new)
            .push(attr);
    }

    let mut match_statement = quote!();

    for (base, attrs) in buckets {
        // Generate the read code for this base
        let code = generate_write_value_attributes_for_base(&attrs);

        match_statement.extend(quote! {
            rom_data::RomBase::#base => {
                #code
            }
        });
    }

    quote! {
        match rom.base {
            #match_statement

            _ => return Err(rom_data::values::RomValueError::NotImplemented)
        }
    }
}

/// Generate the lines to write all the attributes with the same base.
fn generate_write_value_attributes_for_base(attrs: &Vec<&ValueAttribute>) -> TokenStream {
    let mut code = quote!();

    for attr in attrs {
        // TODO Check the condition
        let mut code_under_if = quote!();

        for offset in attr.offsets.iter() {
            let method = &attr.method;
            let transforms = &attr.transforms;

            // Write operations:
            // 1. Apply the transformations
            for transform in transforms.iter().rev() {
                code_under_if.extend(quote! {
                    let value = rom_data::values::RomValueTransformation::#transform.apply_transform(value);
                });
            }
            // 2. Write using the method
            code_under_if.extend(quote! {
                rom_data::values::RomValueType::#method.write(rom, #offset, value)?;
            });
        }

        code.extend(quote! { {#code_under_if} })
    }

    code
}
