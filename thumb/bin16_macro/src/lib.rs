use proc_macro::{TokenStream, TokenTree};
use proc_macro2::TokenStream as TokenStream2;
use proc_macro_error::{abort, abort_call_site, proc_macro_error};
use quote::{format_ident, quote};

/// Macro to format a value as a 16-bit binary string
///
/// Example:
/// ```norun
/// assert_eq!(bin16!("000{5}00{5}1", 0b11111, 0b11111), 0b0001111100111111);
/// ```
/// The `{:5}` is a placeholder for a 5-bit value.
#[proc_macro]
#[proc_macro_error]
pub fn bin16(stream: TokenStream) -> TokenStream {
    // Get the first element in the stream
    let mut iter = stream.into_iter();

    let format = parse_format_string(&mut iter, 16);
    build_code(&mut iter, &format, 16).into()
}

#[derive(Debug)]
enum FormatElement {
    Zero,
    One,
    Boolean,
    Size(u8),
}

impl FormatElement {
    fn number_of_bits(&self) -> u8 {
        match self {
            FormatElement::Zero => 1,
            FormatElement::One => 1,
            FormatElement::Boolean => 1,
            FormatElement::Size(size) => *size,
        }
    }
}

/// Parses the format string into a vector of FormatElements
fn parse_format_string(
    iter: &mut impl Iterator<Item = TokenTree>,
    expected_size: u8,
) -> Vec<FormatElement> {
    let first = match iter.next() {
        Some(first) => first,
        None => abort_call_site!("Expected at least the format string"),
    };

    // Make sure the first element is a string literal
    let format_string = match first {
        TokenTree::Literal(ref lit) => lit.to_string(),
        _ => abort!(first.span(), "Expected a string literal"),
    };

    // Make sure it is a string (surrounded by quotes)
    let format_string = match format_string.strip_prefix('"').map(|s| s.strip_suffix('"')) {
        Some(Some(format_string)) => format_string,
        _ => abort!(first.span(), "Expected a string literal"),
    };

    let mut result: Vec<FormatElement> = vec![];

    // Last index in the brace
    let mut skip_count = 0;

    for (i, char) in format_string.chars().enumerate() {
        if skip_count > 0 {
            skip_count -= 1;
            continue;
        }

        match char {
            '_' => {}
            '0' => result.push(FormatElement::Zero),
            '1' => result.push(FormatElement::One),
            '{' => {
                // Find the closing brace
                let closing_brace = format_string[i..]
                    .find('}')
                    .unwrap_or_else(|| abort!(first.span(), "Expected a closing brace"));

                if closing_brace == 1 {
                    result.push(FormatElement::Boolean);
                } else {
                    let size = format_string[i + 1..i + closing_brace]
                        .parse()
                        .unwrap_or_else(|_| abort!(first.span(), "Expected a number inside brace"));

                    if size == 0 {
                        abort!(first.span(), "Expected a number greater than 0");
                    }

                    result.push(FormatElement::Size(size));
                }

                skip_count = closing_brace;
            }
            _ => abort!(first.span(), "Expected a 0, 1, or {{, got {}", char),
        }
    }

    // Make sure the number of bits sums to the given size
    let total_size: u8 = result.iter().map(|e| e.number_of_bits()).sum();
    if expected_size != total_size {
        abort!(
            first.span(),
            "Expected a total of {} bits, got {}",
            expected_size,
            total_size
        );
    }

    // Parse the format string
    result
}

/// Creates the base literal to apply variables onto
fn compute_base(format: &Vec<FormatElement>) -> usize {
    let mut result = 0;

    for element in format {
        result <<= 1;

        match element {
            FormatElement::Zero => {}
            FormatElement::One => result |= 1,
            FormatElement::Boolean => {}
            FormatElement::Size(n) => result <<= n - 1,
        }
    }

    result
}

/// Computes the shift and mask value for each user-defined value
fn compute_shifts_and_masks(format: &Vec<FormatElement>) -> Vec<(u8, usize)> {
    let mut res = vec![];

    let mut shift = 0;

    for el in format.iter().rev() {
        match el {
            FormatElement::Zero => {}
            FormatElement::One => {}
            FormatElement::Boolean => res.insert(0, (shift, 1)),
            FormatElement::Size(n) => {
                res.insert(0, (shift, (1 << n) - 1));
                shift += n - 1;
            }
        }

        shift += 1;
    }

    res
}

/// Creates the code to build the string
fn build_code(
    iter: &mut impl Iterator<Item = TokenTree>,
    format: &Vec<FormatElement>,
    expected_size: usize,
) -> TokenStream2 {
    // Get the shifts and masks
    let shifts_masks = compute_shifts_and_masks(format);
    let expected_arguments = shifts_masks.len();

    // If there is another token, it must be a comma
    if let Some(TokenTree::Punct(punct)) = iter.next() {
        if punct.as_char() != ',' {
            abort!(punct.span(), "Expected a comma");
        }
    }

    let mut arguments: Vec<TokenStream> = Vec::new();
    // Get the rest of the comma-separated arguments
    for _ in 0..expected_arguments {
        let argument = match iter.next() {
            Some(TokenTree::Literal(lit)) => lit.to_string(),
            Some(TokenTree::Ident(ident)) => ident.to_string(),
            Some(other) => abort!(other.span(), "Expected a literal or identifier"),
            None => abort_call_site!("Expected a literal or identifier"),
        };

        arguments.push(argument.parse().unwrap());

        if arguments.len() != expected_arguments {
            if let Some(TokenTree::Punct(punct)) = iter.next() {
                if punct.as_char() != ',' {
                    abort!(punct.span(), "Expected a comma");
                }
            }
        }
    }

    // Make sure there are no more arguments
    if let Some(_) = iter.next() {
        abort_call_site!("Expected no more arguments");
    }

    let final_type = format_ident!("u{}", expected_size);

    // Compose the macro
    let base = compute_base(format);

    let mut args = quote! {};

    for (arg, (shift, mask)) in arguments.into_iter().zip(shifts_masks.into_iter()) {
        let mask: TokenStream2 = format!("{}_u{}", mask, expected_size)
            .parse::<TokenStream>()
            .unwrap()
            .into();
        let shift: TokenStream2 = format!("{}_u{}", shift, expected_size)
            .parse::<TokenStream>()
            .unwrap()
            .into();

        let arg: TokenStream2 = arg.into();

        args = quote! {
            #args

            result |= ((#arg as #final_type) & #mask) << #shift;
        };
    }

    quote! {
        {
            let mut result: #final_type = #base as #final_type;

            #args

            result
        }
    }
}
