use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;

mod impls;
mod parser;

// TODO Conditions

/// Derive macro for the [`RomValues`] trait.
///
/// Everything is specified via the `value` attribute, which has the following syntax:
/// ```text
/// #[value($base $offset+ $method $transformations* [if $condition])]
/// ```
///
/// Where:
/// + `$base` is the base rom type for which this value can be read.
/// + `$offset` is the offset (or list of offsets) to read this value from.
/// + `$method` is the method to use to read the value (e.g. `MovLsl` or `Word`)
/// + `$transformations` is a list of transformations to apply to the value after
///   reading or before writing.
///
///   They have to be written in the order in which they
///   are inverted after reading. For example, if a value x is written to x after
///   applying A then B (`y = B(A(x))`), we have to write `B A`.
///
///   Reading them in reverse will result in the correct order when writing.
/// + `$condition` not yet implemented
#[proc_macro_error]
#[proc_macro_derive(RomValues, attributes(value))]
pub fn rom_value_derive(input: TokenStream) -> TokenStream {
    // Parse the syn AST for the struct input
    let ast = syn::parse(input).unwrap();

    // Build the impl
    let fields = parser::parse_fields(&ast);

    // Get the struct name
    let name = &ast.ident;
    // Generate the code
    let code = impls::generate(name, &fields);

    code.into()
}
