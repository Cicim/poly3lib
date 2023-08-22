//! First parsing step. Produces a struct containing struct name and flags
//! as well as tokens for each fields as well as annotations.

use proc_macro2::{token_stream::IntoIter, Delimiter, Ident, TokenStream, TokenTree};
use proc_macro_error::{abort, abort_call_site};

/// The parsed data for this struct.
#[derive(Debug)]
pub struct ParsedStruct {
    /// The name of the struct.
    name: Ident,
    /// The fields of the struct.
    fields: Vec<ParsedField>,
    /// The flags for the struct.
    flags: ParsedFlags,
}

/// The parsed data for a field.
#[derive(Debug)]
pub struct ParsedField {
    /// The name of the field.
    name: Ident,
    /// The type of the field.
    ty: String,
    // TODO Attributes
}

#[derive(Default, Debug)]
pub struct ParsedFlags {
    /// Whether to **not** set this struct as public, effectively making it private
    private: bool,
    /// Whether to **not** derive `Debug` for this struct
    no_debug: bool,
}

#[derive(Debug, Clone)]
/// Type that can be found at the start of a type definition
pub enum ParsedBaseType {
    // TODO Support for Unions
    /// A struct with the identifier given as name
    Struct(Ident),

    /// A void type (only used with pointers)
    Void,
    /// A sized integer type (valid for bitfields)
    Integer(IntegerType),
}

#[derive(Debug, Clone, Copy)]
pub enum IntegerType {
    /// An unsigned integer with the given number of bits (8, 16 or 32)
    Unsigned(u8),
    /// A signed integer with the given number of bits (8, 16 or 32)
    Signed(u8),
    /// A boolean with the given number of bits (8, 16 or 32)
    Boolean(u8),
}

impl IntegerType {
    /// Returns the number of bits this integer type has.
    pub fn bits(&self) -> u8 {
        match self {
            IntegerType::Unsigned(x) => *x,
            IntegerType::Signed(x) => *x,
            IntegerType::Boolean(x) => *x,
        }
    }
}

#[derive(Debug)]
pub struct ParsedBitField {
    /// The type of the bitfield
    ty: IntegerType,
    /// The number of bits in the bitfield
    bits: u8,
}

#[derive(Debug)]
pub enum ParsedFieldType {
    // TODO Support for variable-length vectors
    /// A BitField (it is a special case, since it cannot be inside a pointer)
    BitField(ParsedBitField),

    /// A derived type (may also be just a base type)
    Type(ParsedDerivedType),
}

/// A type with modifiers applied to it.
#[derive(Debug)]
pub enum ParsedDerivedType {
    /// A base type with no modifiers
    Base(ParsedBaseType),
    /// A pointer to a base type
    Pointer(Box<ParsedDerivedType>),
    /// An array of a base type with the fixed length
    Array(Box<ParsedDerivedType>, u32),
}

macro_rules! next_not_eof {
    ($tokens:ident, $expected:expr) => {
        match $tokens.next() {
            Some(x) => x,
            None => abort_call_site!("Expected {}, found end of stream", $expected),
        }
    };
}

/// Parses the input tokens from the macro invocation into something useful
/// for parsing the fields.
pub fn parse(stream: TokenStream) -> ParsedStruct {
    // Convert the stream to an iterator so we can consume it.
    let mut stream = stream.into_iter();

    // Consume the first token. This should be the name of the struct.
    let name = match next_not_eof!(stream, "struct name") {
        TokenTree::Ident(x) => x,
        x => abort!(x, "Expected struct name, found `{}`", x),
    };

    // Consume the next token tree. This should be a `{}`.
    let fields = match next_not_eof!(stream, "{") {
        TokenTree::Group(x) => {
            // Make sure the group is delimited by braces
            if x.delimiter() != Delimiter::Brace {
                abort!(
                    x.span_open(),
                    "Expected fields to be delimited by braces (`{{}}`), found `{}`",
                    match x.delimiter() {
                        Delimiter::Parenthesis => "(",
                        Delimiter::Brace => "{",
                        Delimiter::Bracket => "[",
                        Delimiter::None => "none",
                    }
                );
            }

            // Convert the stream to an iterator
            x.stream()
        }
        x => abort!(x, "Expected `{{`, found `{}`", x),
    };

    // Parse the fields.
    let fields = parse_struct_fields(fields);

    // Consume each flag
    let mut flags = ParsedFlags::default();
    while let Some(flag) = stream.next() {
        match flag {
            TokenTree::Ident(x) => match x.to_string().as_str() {
                "priv" | "private" => flags.private = true,
                "no_debug" => flags.no_debug = true,
                x => abort!(x, "Unknown flag `{}`", x),
            },
            x => abort!(x, "Expected flag, found `{}`", x),
        }
    }

    // Return the parsed struct.
    ParsedStruct {
        name,
        fields,
        flags,
    }
}

fn parse_struct_fields(stream: TokenStream) -> Vec<ParsedField> {
    // Convert the stream into an iterator
    let mut stream: IntoIter = stream.into_iter();

    while let Some(field) = stream.next() {
        match field {
            // If this token is a #, then this is an attribute.
            TokenTree::Punct(x) => {
                if x.as_char() != '#' {
                    abort!(x, "Expected `#` or a <TYPE-NAME> found `{}`", x);
                }

                // Parse the next token
                match next_not_eof!(stream, "attribute information") {
                    TokenTree::Group(g) => {
                        // Make sure the group is delimited by brackets
                        if g.delimiter() != Delimiter::Bracket {
                            abort!(
                                g.span_open(),
                                "Expected attribute information to be delimited by brackets (`[]`), found `{}`",
                                match g.delimiter() {
                                    Delimiter::Parenthesis => "(",
                                    Delimiter::Brace => "{",
                                    Delimiter::Bracket => "[",
                                    Delimiter::None => "none",
                                }
                            );
                        }

                        // Parse the attribute
                        parse_attribute(g.stream());
                    }
                    _ => abort!(x, "Expected attribute information, found `{}`", x),
                }
            }

            // If it is an identifier, then we can try to parse it as a type.
            TokenTree::Ident(base_type) => {
                // Call the function to parse the field
                parse_field(base_type, &mut stream)
            }

            // In any other case, we can't parse this.
            _ => abort!(field, "Expected attribute or field, found `{}`", field),
        }
    }

    Vec::new()
}

// ANCHOR Attribute parsing
/// Parses an attribute value: something like `#[...]`, which is used to
/// specify certain conditions under which some fields may change type.
fn parse_attribute(stream: TokenStream) {
    println!("Parsing attribute:: {}", stream);
}

// ANCHOR Field parsing
fn parse_field(field_base_type: Ident, stream: &mut IntoIter) {
    let first_span = field_base_type.span();
    println!("Parsing field:: {}", field_base_type);

    // Try to parse this into a field
    let base_type = parse_base_type(field_base_type, stream);

    // Extract all the tokens up to the next ; (semicolon)
    let mut token_list = Vec::new();
    while let Some(token) = stream.next() {
        match token {
            TokenTree::Punct(x) => {
                if x.as_char() == ';' {
                    // Make sure there is something to work with in the type tokens
                    if token_list.is_empty() {
                        abort!(x, "You must specify a name for the field");
                    }

                    // Terminate here by reading the type
                    let (ty, name) = parse_field_type(base_type, token_list);
                    println!("Field type: {:?}", ty);
                    println!("Field name: {}", name);

                    // Checked the type for its validity
                    match &ty {
                        // Void is only allowed if it is a pointer
                        ParsedFieldType::Type(ParsedDerivedType::Base(ParsedBaseType::Void)) => {
                            abort!(
                                name,
                                "Fields cannot have type `void` unless they are pointers"
                            )
                        }
                        ParsedFieldType::Type(derived) => {
                            assert_derived_type_is_valid(derived, &name)
                        }

                        // Bitfields are validated before being returned
                        ParsedFieldType::BitField(_) => {}
                    };

                    return;
                } else {
                    token_list.push(TokenTree::Punct(x));
                }
            }
            _ => token_list.push(token),
        }
    }

    // Every field must end with a semicolon
    abort!(
        if token_list.is_empty() {
            first_span
        } else {
            token_list.last().unwrap().span()
        },
        "Expected `;` after a field declaration"
    );
}

// ANCHOR Type parsing
/// Parses a derived type from the given tokens, trying some special patters (such as bitfields)
/// first before then relying on the recursive parser.
///
/// TODO Support for vector types should be added here
fn parse_field_type(base_type: ParsedBaseType, list: Vec<TokenTree>) -> (ParsedFieldType, Ident) {
    if list.len() == 3 {
        // If there are only three tokens and we know their shape exactly, then we can
        // directly parse the bitfield, which is not possible in any recursive case.
        // If should be something like <IDENT>:<LITERAL>
        match (&list[0], &list[1], &list[2]) {
            (TokenTree::Ident(name), TokenTree::Punct(colon), TokenTree::Literal(size)) => {
                // If there is a char in the middle of this configuration, it must be a bitfield
                if colon.as_char() == ':' {
                    if let ParsedBaseType::Integer(ty) = base_type {
                        // REVIEW - Check valid identifier?
                        // Parse the size of the bitfield
                        let bits = match size.to_string().parse::<u8>() {
                            Ok(x) => {
                                // Check that the size is valid
                                if x == 0 {
                                    abort!(size, "Bitfield size cannot be 0");
                                }
                                if x > ty.bits() {
                                    abort!(
                                        size,
                                        "Bitfield size cannot be larger than the integer size ({})",
                                        ty.bits()
                                    );
                                }

                                x
                            }
                            Err(_) => abort!(size, "Expected an integer for the bitfield size"),
                        };

                        // Return the bitfield
                        return (
                            ParsedFieldType::BitField(ParsedBitField { ty, bits }),
                            name.clone(),
                        );
                    } else {
                        abort!(
                            name,
                            "Bitfields can only be created with integer or boolean types"
                        );
                    }
                }
            }
            // In case there is no match, just skip to the normal cases
            _ => {}
        };
    }

    // If no special case matched, call the recursive option
    let mut dereferences = vec![];
    let name = parse_derived_type(list, &mut dereferences);

    // Apply the dereferences
    let mut ty = ParsedDerivedType::Base(base_type);
    for count in dereferences.iter().rev() {
        if *count == 0 {
            ty = ParsedDerivedType::Pointer(Box::new(ty));
        } else {
            ty = ParsedDerivedType::Array(Box::new(ty), *count);
        }
    }

    let ty = ParsedFieldType::Type(ty);
    (ty, name)
}

/// Given a non-empty list of tokens from after the base type to before the semicolon, e.g.
/// ```plaintext
/// (*name)[10][20]
/// ```
///
/// It returns the identifier for the name, while also filling the `dereferences` vector with
/// the type of dereferences it encounters (0 for `*` or `[]`, n for `[n]`). These are visited
/// starting by the innermost dereference and going outwards, following the rule of go right if
/// you can, go left if you must.
///
/// # Example
/// ```plaintext
/// (*name)[10][20]
/// ```
///
/// Should be read as *name is an array of 10 arrays of 20 pointers to something.*
///
/// Thus, our algorithm reads [10, 20, **0**] and returns `name`.
fn parse_derived_type(mut list: Vec<TokenTree>, dereferences: &mut Vec<u32>) -> Ident {
    // Count the number of asterisks that will be applied later following
    // the rule of go left if you must.
    let mut asterisks = 0;
    let mut last_asterisk = None;

    while let Some(token) = list.first() {
        match token {
            TokenTree::Punct(x) => {
                if x.as_char() == '*' {
                    asterisks += 1;
                    last_asterisk = Some(list.remove(0));
                } else {
                    abort!(token, "Cannot have `{}` in a field type declaration", x);
                }
            }
            _ => break,
        }
    }

    if list.is_empty() {
        abort!(last_asterisk, "Expected a field name");
    }

    // Now there must be the name of the field (or a recursion)
    let name = match list.remove(0) {
        TokenTree::Ident(x) => x,
        TokenTree::Group(x) => {
            match x.delimiter() {
                // If this is a recursive type
                Delimiter::Parenthesis => {
                    // Get the inner stream of the parenthesis
                    let inner_list: Vec<TokenTree> = x.stream().into_iter().collect();

                    // Call this function to retrieve the identifier
                    parse_derived_type(inner_list, dereferences)
                }
                Delimiter::Bracket => abort!(x.span_open(), "Arrays go after the field name"),
                _ => abort!(
                    x.span_open(),
                    "Expected a field name or a recursive type, found {}",
                    x
                ),
            }
        }

        _ => abort!(last_asterisk, "Expected a field name"),
    };

    // Match the square brackets for arrays. Build the type as you go.
    // Following the go right if you can rule.
    for token in list {
        match token {
            TokenTree::Group(g) => {
                if g.delimiter() == Delimiter::Bracket {
                    // Get the inner stream of the parenthesis
                    let inner_list: Vec<TokenTree> = g.stream().into_iter().collect();

                    if inner_list.len() == 0 {
                        // Dereference as a pointer
                        dereferences.push(0)
                    } else if inner_list.len() > 1 {
                        abort!(g.span_open(), "Expected only the array size");
                    } else {
                        // Get the size of the array
                        let size = match &inner_list[0] {
                            TokenTree::Literal(x) => match x.to_string().parse::<u32>() {
                                Ok(x) => {
                                    if x == 0 {
                                        abort!(x, "Array size cannot be 0");
                                    }

                                    x
                                }
                                Err(_) => abort!(x, "Expected an integer for the array size"),
                            },
                            _ => abort!(inner_list[0], "Expected an integer for the array size"),
                        };

                        // Create the array
                        dereferences.push(size);
                    }
                } else {
                    abort!(g.span_open(), "Expected an array `[]`, found {}", g);
                }
            }

            _ => abort!(
                token,
                "Only arrays are allowed after the field name, found {}",
                token
            ),
        }
    }

    // Apply the asterisks to the innermost part
    for _ in 0..asterisks {
        dereferences.push(0)
    }

    name
}

/// Parses the base type from the first token and the token stream (in case of types that require more than one token)
fn parse_base_type(base_type: Ident, stream: &mut IntoIter) -> ParsedBaseType {
    use ParsedBaseType::*;

    // Try to parse this into a field
    match base_type.to_string().as_str() {
        "u8" => Integer(IntegerType::Unsigned(8)),
        "u16" => Integer(IntegerType::Unsigned(16)),
        "u32" => Integer(IntegerType::Unsigned(32)),
        "i8" | "s8" | "char" => Integer(IntegerType::Signed(8)),
        "i16" | "s16" | "short" => Integer(IntegerType::Signed(16)),
        "i32" | "s32" | "int" => Integer(IntegerType::Signed(32)),
        "bool" | "bool8" => Integer(IntegerType::Boolean(8)),
        "bool16" => Integer(IntegerType::Boolean(16)),
        "bool32" => Integer(IntegerType::Boolean(32)),
        "void" => Void,

        // Support for unsigned prefix (useless, but why not)
        "unsigned" => {
            let message = "Expected C-style integer type (`char`, `short`, `int`)";

            // Consume the next token
            match next_not_eof!(stream, message) {
                TokenTree::Ident(x) => match x.to_string().as_str() {
                    "char" => Integer(IntegerType::Unsigned(8)),
                    "short" => Integer(IntegerType::Unsigned(16)),
                    "int" => Integer(IntegerType::Unsigned(32)),

                    _ => abort!(x, "{}, found `{}`", message, x),
                },
                x => abort!(x, "{}, found `{}`", message, x),
            }
        }

        // Support for structs
        "struct" => {
            // Consume the next token
            match next_not_eof!(stream, "struct name") {
                TokenTree::Ident(x) => Struct(x),
                x => abort!(x, "Expected struct name, found `{}`", x),
            }
        }

        // TODO Unions
        "union" => abort!(base_type, "Unions are not yet supported"),

        _ => abort!(base_type, "Expected <TYPE-NAME>, found `{}`", base_type),
    }
}

/// Applies some simple rules to check if the type is valid (recursively)
fn assert_derived_type_is_valid(derived: &ParsedDerivedType, name: &Ident) {
    use ParsedDerivedType::*;

    match derived {
        Base(_) => return,
        Array(x, _) => {
            if let Base(ParsedBaseType::Void) = x.as_ref() {
                abort!(name, "Cannot have arrays of voids");
            }
            assert_derived_type_is_valid(x, name)
        }
        Pointer(x) => assert_derived_type_is_valid(x, name),
    }
}
