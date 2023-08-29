use proc_macro2::{Delimiter, Ident, Literal, TokenStream, TokenTree};
use proc_macro_error::{abort, abort_call_site};
use quote::ToTokens;
use syn::DeriveInput;

#[derive(Debug)]
pub struct ValueField {
    /// The name of the field
    name: Ident,
    /// The type of the field (only integers are supported)
    ty: Ident,
    /// The attributes of the field
    attrs: Vec<ValueAttribute>,
}

#[derive(Debug)]
pub struct ValueAttribute {
    /// The base for which this value can be read
    base: Ident,
    /// The offsets where this value can be read
    offsets: Vec<Literal>,
    /// The read/write method
    method: Ident,
    /// The transformation to apply to the value
    transform: TokenStream,
    /// The condition to read this value (if any)
    ///
    /// This condition is in the form of code on configuration attributes.
    condition: Option<TokenStream>,
}

/// Parse all the fields into a struct into a [`ValueField`] vector.
pub fn parse_fields(ast: &DeriveInput) -> Vec<ValueField> {
    // Get the struct fields
    let fields = match &ast.data {
        syn::Data::Struct(syn::DataStruct {
            fields: syn::Fields::Named(syn::FieldsNamed { named: fields, .. }),
            ..
        }) => fields,
        _ => panic!("Only structs with named fields are supported"),
    };

    let mut output = Vec::new();

    // Get the field names and types
    for field in fields {
        let name = field.ident.clone().unwrap();
        let field_type = &field.ty;

        // Assert that all fields are integers
        let ty: Ident = if let syn::Type::Path(type_path) = field_type {
            // Find the internal path
            let path = &type_path.path;

            if !path.is_ident("u8")
                && !path.is_ident("u16")
                && !path.is_ident("u32")
                && !path.is_ident("i8")
                && !path.is_ident("i16")
                && !path.is_ident("i32")
            {
                abort!(path, "All fields must have an integer type");
            }

            // Extract the identifier
            path.segments.first().unwrap().ident.clone()
        } else {
            abort!(field_type, "All fields must have an integer type");
        };

        // Get only the attributes that are of "value" type
        let attributes = &field.attrs;
        let value_attribute = attributes
            .iter()
            .filter(|attribute| attribute.path().is_ident("value"));

        let mut attrs = Vec::new();
        for attr in value_attribute {
            let meta_list = match &attr.meta {
                syn::Meta::List(list) => list,

                _ => abort!(attr, "Invalid attribute"),
            };

            let attribute = parse_attribute(&meta_list.tokens);
            attrs.push(attribute);
        }

        output.push(ValueField { name, ty, attrs })
    }

    output
}

/// Parse a single `#[value(...)]` attribute into a [`ValueAttribute`].
///
/// These attributes have the following syntax:
///
/// `RomBase` `{Offset-or-List}` `Method` `Transformation` \[if `Condition`\]
fn parse_attribute(tokens: &TokenStream) -> ValueAttribute {
    // Get the rom base
    let mut tokens = tokens.clone().into_iter().peekable();

    macro_rules! expect_not_eof {
        ($what:literal) => {
            if tokens.peek().is_none() {
                abort_call_site!("Expected {} but found EOF", $what);
            }
        };
    }

    // Get the rom base
    expect_not_eof!("rom base");
    let base = match tokens.next().unwrap() {
        TokenTree::Ident(ident) => ident,
        t => abort!(t, "Expected rom base"),
    };

    // Get the offsets
    expect_not_eof!("offset or list of offsets");
    let mut offsets = Vec::new();

    while let Some(token) = tokens.peek() {
        match token {
            TokenTree::Literal(literal) => {
                // Make sure this is an offset
                if !literal.to_string().starts_with("0x") {
                    abort!(literal, "Expected offset");
                }
                offsets.push(literal.clone());
                tokens.next();
            }

            _ => break,
        }
    }

    if offsets.is_empty() {
        abort!(base, "Expected offset or list of offsets");
    }

    // Get the method
    expect_not_eof!("method");
    let method = match tokens.next().unwrap() {
        TokenTree::Ident(ident) => ident,
        t => abort!(t, "Expected method"),
    };

    // Get the transformation
    expect_not_eof!("transformation");
    let mut transform = TokenStream::new();
    match tokens.next().unwrap() {
        TokenTree::Ident(ident) => transform.extend(ident.into_token_stream()),
        t => abort!(t, "Expected transformation"),
    };

    // If present, add anything within parentheses
    if let Some(token) = tokens.peek() {
        if let TokenTree::Group(group) = token {
            if group.delimiter() != Delimiter::Parenthesis {
                abort!(group, "Expected parentheses");
            }

            transform.extend(group.to_token_stream());
            tokens.next();
        }
    }

    // Get the condition
    let mut condition = None;

    if let Some(token) = tokens.peek() {
        // Match the if keyword
        if let TokenTree::Ident(ident) = token {
            if ident != "if" {
                abort!(ident, "Expected if keyword");
            }
            tokens.next();
        } else {
            abort!(token, "Expected if keyword");
        }

        // Get the condition
        let mut condition_tokens = Vec::new();
        while let Some(token) = tokens.next() {
            // TODO Transform each ident token to the code to check that condition.
            condition_tokens.push(token);
        }
        condition = Some(TokenStream::from_iter(condition_tokens));
    }

    ValueAttribute {
        base,
        offsets,
        method,
        transform,
        condition,
    }
}
