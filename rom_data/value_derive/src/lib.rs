use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;

mod impls;
mod parser;

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
