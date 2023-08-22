use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;

mod parser;
mod parser_types;

#[proc_macro]
#[proc_macro_error]
pub fn rom_struct(input: TokenStream) -> TokenStream {
    // Parse the input struct into something you can use
    let parsed = parser::parse(input.into());

    if parsed.flags.print_parser_output {
        eprintln!("{:?}", parsed);
    }

    TokenStream::new()
}