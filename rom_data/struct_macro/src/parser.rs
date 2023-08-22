//! First parsing step. Produces a struct containing struct name and flags
//! as well as tokens for each fields as well as annotations.

use std::collections::HashSet;

use proc_macro2::{
    token_stream::IntoIter, Delimiter, Group, Ident, Literal, Punct, Span, TokenStream, TokenTree,
};
use proc_macro_error::{abort, abort_call_site};

// ANCHOR Exported types
/// The parsed data for this struct.
#[derive(Debug)]
pub struct ParsedStruct {
    /// The name of the struct.
    pub name: Ident,
    /// The fields of the struct.
    pub fields: Vec<StructField>,
    /// The flags for the struct.
    pub flags: StructFlags,
}

/// Flags for the struct.
#[derive(Default, Debug)]
pub struct StructFlags {
    /// Whether to **not** set this struct as public, effectively making it private
    pub private: bool,
    /// Whether to **not** derive `Debug` for this struct
    pub no_debug: bool,
}

#[derive(Debug)]
/// The final field type
pub enum StructField {
    // TODO Add support for Vector fields
    /// A single field with a [`DerivedType`]
    Field(StructBasicField),
    /// A list of fields read from a BitField.
    ///
    /// Bitfields cannot have attributes.
    BitField(StructBitFields),
}

#[derive(Debug)]
pub struct StructBasicField {
    /// The name of the field.
    pub name: Ident,
    /// The type of the field.
    pub ty: DerivedType,
    /// The attributes of the field.
    pub attributes: Vec<StructFieldAttribute>,
}

#[derive(Debug)]
pub struct StructBitFields {
    /// The type of bitfield
    pub ty: SizedBaseType,
    /// The bit sizes of each field
    pub sizes: Vec<u8>,
    /// The names of each field
    pub names: Vec<Ident>,
}

/// An attribute for specifying when some fields are missing or have
/// a different type based on a ROM base or specific configuration
#[derive(Debug, Clone)]
pub struct StructFieldAttribute {
    /// The condition for this attribute to be applied
    pub condition: StructAttributeCondition,
    /// The action to take if the condition is met
    pub action: StructAttributeAction,
}

/// The condition for an attribute to be applied
#[derive(Debug, Clone)]
pub enum StructAttributeCondition {
    /// The attribute is applied if the ROM base is one of the given ones
    ///
    /// ```
    /// #[for(base(Ruby, Sapphire, Emerald), ...)]
    /// ```
    Base(Vec<Ident>),
    /// The attribute is applied if a specific configuration is set for this ROM.
    /// The configuration is specified by the identifier.
    ///
    /// ```
    /// #[for(cfg(remove_teachy_tv), ...)]
    /// ```
    Cfg(Ident),
    /// The attribute is applied if a specific configuration is **not** set for this ROM.
    ///
    /// ```
    /// #[for(cfg(!remove_teachy_tv), ...)]
    /// ```
    NotCfg(Ident),
}

/// The action to take if the condition is met
#[derive(Debug, Clone)]
pub enum StructAttributeAction {
    /// Change the type of the field
    ///
    /// ```
    /// #[for(..., type(u32))]
    /// ```
    Type(BaseType),

    /// Do not read the field and set a default value instead
    ///
    /// ```
    /// #[for(..., default(0))]
    /// ```
    Default(Literal),
}

/// Type that can be found at the start of a type definition
#[derive(Debug, Clone)]
pub enum BaseType {
    // TODO Support for Unions
    /// A struct with the identifier given as name
    Struct(Ident),

    /// A void type (only used with pointers)
    Void,
    /// A sized integer type (valid for bitfields)
    Integer(SizedBaseType),
}

impl BaseType {
    /// Returns whether the type is an integer (excluding boolean)
    pub fn is_integer(&self) -> bool {
        match self {
            BaseType::Integer(SizedBaseType::Boolean(_)) => false,
            BaseType::Integer(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SizedBaseType {
    /// An unsigned integer with the given number of bits (8, 16 or 32)
    Unsigned(u8),
    /// A signed integer with the given number of bits (8, 16 or 32)
    Signed(u8),
    /// A boolean with the given number of bits (8, 16 or 32)
    Boolean(u8),
}

impl SizedBaseType {
    /// Returns the number of bits this integer type has.
    pub fn bits(&self) -> u8 {
        match self {
            SizedBaseType::Unsigned(x) => *x,
            SizedBaseType::Signed(x) => *x,
            SizedBaseType::Boolean(x) => *x,
        }
    }
}

/// A type with modifiers applied to it.
#[derive(Debug)]
pub enum DerivedType {
    /// A base type with no modifiers
    Base(BaseType),
    /// A pointer to a base type
    Pointer(Box<DerivedType>),
    /// An array of a base type with the fixed length
    Array(Box<DerivedType>, u32),
}

impl DerivedType {
    pub fn is_integer(&self) -> bool {
        match self {
            DerivedType::Base(base) => base.is_integer(),
            _ => false,
        }
    }
}

// ANCHOR Internal types
/// The parsed data for a field.
#[derive(Debug)]
struct ParsedField {
    /// The name of the field.
    name: Ident,
    /// The type of the field.
    ty: ParsedFieldType,
    /// The attributes of the field.
    attributes: Vec<StructFieldAttribute>,
}

#[derive(Debug)]
enum ParsedFieldType {
    // TODO Support for variable-length vectors
    /// A BitField (it is a special case, since it cannot be inside a pointer)
    BitField {
        /// The type of the bitfield
        ty: SizedBaseType,
        /// The number of bits in the bitfield
        bits: u8,
    },

    /// A derived type (may also be just a base type)
    Type(DerivedType),
}

// ANCHOR Macros
macro_rules! next_not_eof {
    ($tokens:ident, $expected:expr) => {
        match $tokens.next() {
            Some(x) => x,
            None => abort_call_site!("Expected {}, found end of stream", $expected),
        }
    };

    ($span:ident, $tokens:ident, $expected:expr) => {
        match $tokens.next() {
            Some(x) => x,
            None => abort!($span, "Expected {}, found end of stream", $expected),
        }
    };
}

macro_rules! expect_group_delimiter {
    ($group:ident, $name:literal, $ty:ident) => {
        // Make sure the group is delimited by braces
        if $group.delimiter() != Delimiter::$ty {
            abort!(
                $group.span_open(),
                "Expected fields to be delimited by {}, found {}",
                match Delimiter::$ty {
                    Delimiter::Parenthesis => "parentheses (`()`)",
                    Delimiter::Brace => "braces (`{}`)",
                    Delimiter::Bracket => "brackets (`[]`)",
                    Delimiter::None => "nothing",
                },
                match $group.delimiter() {
                    Delimiter::Parenthesis => "`(`",
                    Delimiter::Brace => "`{`",
                    Delimiter::Bracket => "`[`",
                    Delimiter::None => "an undelimited group",
                }
            );
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
            expect_group_delimiter!(x, "fields", Brace);

            // Convert the stream to an iterator
            x.stream()
        }
        x => abort!(x, "Expected `{{`, found `{}`", x),
    };

    // Parse the fields.
    let fields = parse_struct_fields(fields);

    // Consume each flag
    let mut flags = StructFlags::default();
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

    compose_struct(name, fields, flags)
}

/// Parses the fields of the struct (everything within { ... }) as well
/// as all the attributes that apply to them.
fn parse_struct_fields(stream: TokenStream) -> Vec<ParsedField> {
    // Convert the stream into an iterator
    let mut stream: IntoIter = stream.into_iter();
    // The parsed fields (with associated attributes)
    let mut result = Vec::new();

    // Keep a stack of the current attributes to apply to the next field(s)
    let mut parsed_attributes = Vec::new();

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
                        expect_group_delimiter!(g, "attribute information", Bracket);

                        // Parse the attribute
                        parsed_attributes.extend(parse_attribute(g.stream(), g.span()));
                    }
                    _ => abort!(x, "Expected attribute information, found `{}`", x),
                }
            }

            // If it is an identifier, then we can try to parse it as a type.
            TokenTree::Ident(base_type) => {
                // Call the function to parse the field
                let parsed_fields = parse_field(base_type, &mut stream);

                for (field_type, field_name) in parsed_fields {
                    // Create the field
                    let field = ParsedField {
                        name: field_name,
                        ty: field_type,
                        attributes: parsed_attributes.clone(),
                    };

                    // Make sure that the attributes can be applied to this field
                    assert_valid_attributes(&field);

                    // Add it to the list of fields
                    result.push(field);
                }
            }

            // In any other case, we can't parse this.
            _ => abort!(field, "Expected attribute or field, found `{}`", field),
        }
    }

    result
}

// ANCHOR Attribute parsing
/// Parses an attribute value: something like `#[...]`. Receives the stream
/// of tokens inside the brackets.
fn parse_attribute(stream: TokenStream, attribute_span: Span) -> Vec<StructFieldAttribute> {
    // Convert the token stream into
    let mut stream = stream.into_iter().peekable();

    // Expect there to be at least a token (empty attributes are not allowed)
    if stream.peek().is_none() {
        abort!(attribute_span, "Empty attributes are not allowed");
    }

    // The parsed attributes
    let mut attributes = Vec::new();

    let mut trailing_comma = None;

    // Read each for block
    while let Some(token) = stream.next() {
        // Find the `for` token
        let for_token = match token {
            // If this is a `for` token, then we can parse the for block
            TokenTree::Ident(x) if x.to_string().as_str() == "for" => x,

            _ => abort!(token, "Expected `for`, found `{}`", token),
        };

        // Parse the next token (the for block)
        match next_not_eof!(for_token, stream, "<FOR-BLOCK>") {
            TokenTree::Group(group) => {
                // Expect the group to be delimited by parenthesis
                expect_group_delimiter!(group, "for block", Parenthesis);

                // Parse the for block
                let attr = parse_attribute_for_block(group);

                attributes.push(attr);

                // Forget the trailing comma
                trailing_comma = None;
            }
            x => abort!(x, "Expected for block, found `{}`", x),
        };

        // Parse the comma (if present)
        if let Some(comma) = stream.next() {
            match comma {
                TokenTree::Punct(x) if x.as_char() == ',' => {
                    // Remember the trailing comma
                    trailing_comma = Some(x);
                }
                x => abort!(x, "Expected `,`, found `{}`", x),
            }
        }
    }

    if let Some(comma) = trailing_comma {
        abort!(comma, "Trailing comma is not allowed");
    }

    attributes
}

/// Parses a for block of an attribute (which may contain many for blocks). Obtains the stream
/// of tokens inside the parenthesis.
fn parse_attribute_for_block(group: Group) -> StructFieldAttribute {
    let span = group.span();
    let mut stream = group.stream().into_iter();

    // Parse the condition
    let condition = parse_attribute_condition(span, &mut stream);

    // Parse the comma after the condition
    match next_not_eof!(span, stream, "comma after attribute condition") {
        TokenTree::Punct(x) if x.as_char() == ',' => {}
        x => abort!(x, "Expected `,`, found `{}`", x),
    }

    // Parse the action
    let action = parse_attribute_action(span, &mut stream);

    StructFieldAttribute { condition, action }
}

// Parses an attribute condition
fn parse_attribute_condition(span: Span, stream: &mut IntoIter) -> StructAttributeCondition {
    // Parse the identifier
    let ident = match next_not_eof!(span, stream, "attribute condition name") {
        TokenTree::Ident(x) => x,
        x => abort!(x, "Expected attribute condition name, found `{}`", x),
    };

    // Parse the parenthesis
    let group = match next_not_eof!(ident, stream, "attribute condition arguments") {
        TokenTree::Group(g) => {
            // Expect the group to be delimited by parenthesis
            expect_group_delimiter!(g, "attribute condition", Parenthesis);

            // Obtain the group
            let group: Vec<TokenTree> = g.stream().into_iter().collect();
            group
        }
        x => abort!(x, "Expected attribute condition arguments, found `{}`", x),
    };

    match ident.to_string().as_str() {
        // Parse a cfg(ident) or cfg(!ident) attributes
        "cfg" => {
            let message =
                "A cfg attribute can be either `cfg(<IDENTIFIER>)` or `cfg(!<IDENTIFIER>)`";

            if group.len() == 0 || group.len() > 2 {
                abort!(ident, message);
            }

            let bang = match &group[0] {
                // If you find a bang, set the flag
                TokenTree::Punct(x) if x.as_char() == '!' => true,
                // If you find an identifier, don't set the flag
                TokenTree::Ident(_) => false,

                // Otherwise, abort
                _ => abort!(
                    group[0],
                    "Expected `!` or <IDENTIFIER>, found `{}`",
                    group[0]
                ),
            };

            // Match the second token (or first if there is no bang)
            let ident = match &group.get(if bang { 1 } else { 0 }) {
                Some(TokenTree::Ident(x)) => x,
                Some(x) => abort!(x, "Expected <IDENTIFIER>, found `{}`", x),
                _ => abort!(group[0], "Expected <IDENTIFIER> after `!`"),
            };

            // Create the cfg attribute
            if bang {
                StructAttributeCondition::NotCfg(ident.clone())
            } else {
                StructAttributeCondition::Cfg(ident.clone())
            }
        }

        // Parse the base(ident, ident, ...) attribute
        "base" => {
            let mut identifiers = Vec::new();

            let mut iterator = group.into_iter();
            let mut trailing_comma = None;
            while let Some(token) = iterator.next() {
                match token {
                    TokenTree::Ident(x) => {
                        identifiers.push(x);

                        // Forget about the previous trailing comma
                        trailing_comma = None;
                    }
                    _ => abort!(token, "Expected `type`, found `{}`", token),
                }

                // If there is a comma, continue
                if let Some(TokenTree::Punct(x)) = iterator.next() {
                    if x.as_char() != ',' {
                        abort!(x, "Expected `,`, found `{}`", x);
                    }
                    trailing_comma = Some(x);
                } else {
                    break;
                }
            }

            // If there is a trailing comma, abort
            if let Some(x) = trailing_comma {
                abort!(x, "Trailing commas are not allowed in `base` attribute");
            }

            StructAttributeCondition::Base(identifiers)
        }
        x => abort!(ident, "Unknown attribute condition `{}`", x),
    }
}

// Parses an attribute action
fn parse_attribute_action(span: Span, stream: &mut IntoIter) -> StructAttributeAction {
    // Parse the identifier
    let ident = match next_not_eof!(span, stream, "attribute action name") {
        TokenTree::Ident(x) => x,
        x => abort!(x, "Expected attribute action name, found `{}`", x),
    };

    // Parse the parenthesis
    let mut group = match next_not_eof!(ident, stream, "attribute action arguments") {
        TokenTree::Group(g) => {
            // Expect the group to be delimited by parenthesis
            expect_group_delimiter!(g, "attribute action", Parenthesis);

            // Obtain the group
            g.stream().into_iter()
        }
        x => abort!(x, "Expected attribute action arguments, found `{}`", x),
    };

    match ident.to_string().as_str() {
        // default(Literal)
        "default" => match group.next() {
            Some(TokenTree::Literal(x)) => StructAttributeAction::Default(x),
            Some(x) => abort!(x, "Expected literal, found `{}`", x),
            None => abort!(ident, "Expected literal after `default`"),
        },

        // type(basetype)
        "type" => {
            // Get the base type
            let base_type = match next_not_eof!(ident, group, "base type") {
                TokenTree::Ident(x) => x,
                x => abort!(x, "Expected base type, found `{}`", x),
            };

            // Parse the base type
            let base_type = parse_base_type(base_type, &mut group.into_iter());

            StructAttributeAction::Type(base_type)
        }

        _ => abort!(ident, "Unknown attribute action `{}`", ident),
    }
}

// Asserts that all the attributes on a type, following some rules.
fn assert_valid_attributes(field: &ParsedField) {
    let ParsedField {
        name,
        ty,
        attributes,
    } = field;

    // If there are no attributes, skip the check
    if attributes.len() == 0 {
        return;
    }

    if let ParsedFieldType::Type(derived) = ty {
        if !derived.is_integer() {
            // REVIEW Now the only assumption is that attributes only apply to integers
            abort!(name, "Attributes are only allowed on integer types");
        }

        for attr in attributes {
            match &attr.action {
                StructAttributeAction::Type(ty) => {
                    // Make sure every type is an integer
                    if !ty.is_integer() {
                        // REVIEW Move this check to the action reading function
                        //        if it turns out only attributes are really
                        //        only valid for integers.
                        abort!(name, "A type() action does not contain an integer")
                    }
                }
                StructAttributeAction::Default(_) => {
                    // TODO Make sure the literal is a valid integer or
                    // move this check to the attribute parser.
                }
            }
        }
    }
}

// ANCHOR Field type/name parsing
fn parse_field(field_base_type: Ident, stream: &mut IntoIter) -> Vec<(ParsedFieldType, Ident)> {
    let first_span = field_base_type.span();
    println!("Parsing field:: {}", field_base_type);

    // Try to parse this into a field
    let base_type = parse_base_type(field_base_type, stream);

    // Create the list of fields to return
    let mut parsed_fields = Vec::new();

    // Extract all the tokens up to the next ; (semicolon)
    let mut token_list = Vec::new();
    while let Some(token) = stream.next() {
        match token {
            TokenTree::Punct(end_token) => {
                match end_token.as_char() {
                    ';' => {
                        parsed_fields.push(parse_field_type(base_type, token_list, end_token));

                        return parsed_fields;
                    }
                    ',' => {
                        parsed_fields.push(parse_field_type(
                            base_type.clone(),
                            token_list,
                            end_token,
                        ));
                        // Start a new list
                        token_list = Vec::new();
                    }
                    _ => token_list.push(TokenTree::Punct(end_token)),
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

/// Parses a derived type from the given tokens, trying some special patters (such as bitfields)
/// first before then relying on the recursive parser.
///
/// TODO Support for vector types should be added here
fn parse_field_type(
    base_type: BaseType,
    token_list: Vec<TokenTree>,
    end_token: Punct,
) -> (ParsedFieldType, Ident) {
    // Make sure there is something to work with in the type tokens
    if token_list.is_empty() {
        abort!(end_token, "You must specify a name for the field");
    }

    if token_list.len() == 3 {
        // If there are only three tokens and we know their shape exactly, then we can
        // directly parse the bitfield, which is not possible in any recursive case.
        // If should be something like <IDENT>:<LITERAL>
        match (&token_list[0], &token_list[1], &token_list[2]) {
            (TokenTree::Ident(name), TokenTree::Punct(colon), TokenTree::Literal(size)) => {
                // If there is a char in the middle of this configuration, it must be a bitfield
                if colon.as_char() == ':' {
                    if let BaseType::Integer(ty) = base_type {
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
                        return (ParsedFieldType::BitField { ty, bits }, name.clone());
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
    let name = parse_derived_type(token_list, &mut dereferences);

    // Apply the dereferences
    let mut ty = DerivedType::Base(base_type);
    for count in dereferences.iter().rev() {
        if *count == 0 {
            ty = DerivedType::Pointer(Box::new(ty));
        } else {
            ty = DerivedType::Array(Box::new(ty), *count);
        }
    }

    let ty = ParsedFieldType::Type(ty);

    // Checked the type for its validity
    match &ty {
        // Void is only allowed if it is a pointer
        ParsedFieldType::Type(DerivedType::Base(BaseType::Void)) => {
            abort!(
                name,
                "Fields cannot have type `void` unless they are pointers"
            )
        }
        ParsedFieldType::Type(derived) => assert_derived_type_is_valid(derived, &name),

        // Bitfields are validated before being returned
        ParsedFieldType::BitField { .. } => {}
    };

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
fn parse_base_type(base_type: Ident, stream: &mut IntoIter) -> BaseType {
    use BaseType::*;

    // Try to parse this into a field
    match base_type.to_string().as_str() {
        "u8" => Integer(SizedBaseType::Unsigned(8)),
        "u16" => Integer(SizedBaseType::Unsigned(16)),
        "u32" => Integer(SizedBaseType::Unsigned(32)),
        "i8" | "s8" | "char" => Integer(SizedBaseType::Signed(8)),
        "i16" | "s16" | "short" => Integer(SizedBaseType::Signed(16)),
        "i32" | "s32" | "int" => Integer(SizedBaseType::Signed(32)),
        "bool" | "bool8" => Integer(SizedBaseType::Boolean(8)),
        "bool16" => Integer(SizedBaseType::Boolean(16)),
        "bool32" => Integer(SizedBaseType::Boolean(32)),
        "void" => Void,

        // Support for unsigned prefix (useless, but why not)
        "unsigned" => {
            let message = "Expected C-style integer type (`char`, `short`, `int`)";

            // Consume the next token
            match next_not_eof!(stream, message) {
                TokenTree::Ident(x) => match x.to_string().as_str() {
                    "char" => Integer(SizedBaseType::Unsigned(8)),
                    "short" => Integer(SizedBaseType::Unsigned(16)),
                    "int" => Integer(SizedBaseType::Unsigned(32)),

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
fn assert_derived_type_is_valid(derived: &DerivedType, name: &Ident) {
    use DerivedType::*;

    match derived {
        Base(_) => return,
        Array(x, _) => {
            // Cannot make arrays of voids
            if let Base(BaseType::Void) = x.as_ref() {
                abort!(name, "Cannot have arrays of voids");
            }
            // Cannot make arrays of non-byte boolean types
            if let Base(BaseType::Integer(SizedBaseType::Boolean(x))) = x.as_ref() {
                if *x != 8 {
                    abort!(name, "Cannot have arrays of non-byte boolean types");
                }
            }

            assert_derived_type_is_valid(x, name)
        }
        Pointer(x) => {
            // Cannot have pointers of non-byte boolean types
            if let Base(BaseType::Integer(SizedBaseType::Boolean(x))) = x.as_ref() {
                if *x != 8 {
                    abort!(name, "Cannot have pointers of non-byte boolean types");
                }
            }
            assert_derived_type_is_valid(x, name)
        }
    }
}

// ANCHOR Struct composition
/// Composes the final ParsedStruct type by making the last checks (like)
/// name collisions and joining the bitfields.
fn compose_struct(name: Ident, in_fields: Vec<ParsedField>, flags: StructFlags) -> ParsedStruct {
    // Keep track of the names that are already used to avoid collisions
    let mut appeared_names: HashSet<String> = HashSet::new();

    // Build the final fields
    let mut fields = Vec::new();

    // Keep track of whether a bitfield is currently being built
    let mut bitfield_type: Option<SizedBaseType> = None;
    let mut bitfield_sizes: Vec<u8> = Vec::new();
    let mut bitfield_names: Vec<Ident> = Vec::new();
    let mut bitfield_size = 0;

    for ParsedField {
        name,
        ty,
        attributes,
    } in in_fields
    {
        // Check for name collisions
        let name_str = name.to_string();
        if appeared_names.contains(&name_str) {
            abort!(name, "Name collision for field `{}`", name);
        }
        appeared_names.insert(name_str);

        /*
        Bitfields. This is a state machine. Its transitions depend on two things:
        1. Whether the current field is a bitfield
           <=> if `ty` is `ParsedFieldType::BitField`
        2. Whether the previous field was a bitfield
           <=> if `bitfield_type` is `Some(_)`

        The transitions are:
        1. If the current field is a bitfield and the previous one was not, start a new bitfield
        2. If the current field is a bitfield and the previous one was too:
            a. If the types are different, start a new bitfield
            b. If the types are the same:
                i. If the added size would exceed the type size, start a new bitfield
                ii. If the added size would not exceed the type size, add the size to the current bitfield
        3. If the current field is not a bitfield and the previous one was, end the bitfield
        4. If the current field is not a bitfield and the previous one was not, do nothing

        In any case, after the bitfield transitions have been computed, if the current type is not
        a bitfield, it will be added to the types accordingly.
        */
        macro_rules! append_bitfield {
            () => {{
                // Add the old bitfield to the fields
                fields.push(StructField::BitField(StructBitFields {
                    ty: bitfield_type.unwrap(),
                    names: bitfield_names,
                    sizes: bitfield_sizes,
                }));

                // Start a new bitfield
                bitfield_sizes = Vec::new();
                bitfield_names = Vec::new();
            }};
        }

        match (&ty, bitfield_type) {
            // 1. The current field is a bitfield and the previous one was not
            (ParsedFieldType::BitField { ty, bits }, None) => {
                // Start a new bitfield
                bitfield_type = Some(*ty);
                bitfield_sizes.push(*bits);
                bitfield_names.push(name);
                bitfield_size = *bits;
                // Do nothing else
                continue;
            }
            // 2. The current field is a bitfield and the previous one was too
            (
                ParsedFieldType::BitField {
                    ty: new_base_type,
                    bits,
                },
                Some(base_type),
            ) => {
                // a. If the types are different, start a new bitfield
                if *new_base_type != base_type {
                    append_bitfield!();

                    // Start a new bitfield
                    bitfield_type = Some(*new_base_type);
                    bitfield_sizes.push(*bits);
                    bitfield_names.push(name);
                    bitfield_size = *bits;
                }
                // b. If the types are the same:
                else {
                    // i. If the added size would exceed the type size, start a new bitfield
                    if bitfield_size + bits > base_type.bits() {
                        append_bitfield!();

                        // Start a new bitfield
                        bitfield_type = Some(base_type);
                        bitfield_sizes.push(*bits);
                        bitfield_names.push(name);
                        bitfield_size = *bits;
                    }
                    // ii. If the added size would not exceed the type size, add the size to the current bitfield
                    else {
                        bitfield_sizes.push(*bits);
                        bitfield_names.push(name);
                        bitfield_size += bits;
                    }
                }

                continue;
            }

            // 3. The current field is not a bitfield and the previous one was
            (_, Some(_)) => {
                // Add the old bitfield to the fields
                append_bitfield!();
                bitfield_type = None;
                bitfield_size = 0;
            }

            // 4. The current field is not a bitfield and the previous one was not. Do nothing
            (_, None) => {}
        }

        // Once the bitfields check is completed, add the other field types
        match ty {
            ParsedFieldType::Type(ty) => {
                assert_derived_type_is_valid(&ty, &name);
                fields.push(StructField::Field(StructBasicField {
                    ty,
                    name,
                    attributes,
                }));
            }

            // We've already handled bitfields
            ParsedFieldType::BitField { .. } => unreachable!(),
        }
    }

    // If there is still an open bitfield, add it to the fields
    if let Some(base_type) = bitfield_type {
        fields.push(StructField::BitField(StructBitFields {
            ty: base_type,
            names: bitfield_names,
            sizes: bitfield_sizes,
        }));
    }

    ParsedStruct {
        name,
        fields,
        flags,
    }
}
