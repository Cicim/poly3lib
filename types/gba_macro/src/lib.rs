use std::{collections::HashMap, fmt::Display, sync::Mutex};

use lazy_static::lazy_static;
use proc_macro::{Group, Ident, TokenStream, TokenTree};
use proc_macro2::{Ident as Ident2, TokenStream as TokenStream2};
use proc_macro_error::{abort, proc_macro_error};
use quote::{format_ident, quote};

#[derive(Debug, Default)]
struct StructOptions {
    debug: bool,
    private: bool,
}

lazy_static! {
    static ref GBA_STRUCTS: Mutex<HashMap<String, AStruct>> = {
        let m = HashMap::new();
        Mutex::new(m)
    };
}

#[proc_macro]
#[proc_macro_error]
pub fn gba_struct(stream: TokenStream) -> TokenStream {
    // Extract the name and fields by parsing the struct's tokens
    let (name, fields, options) = parse_struct(stream);
    // Assemble the struct into an AStruct object that can be used
    // to build the necessary code to make it into a GBAType.
    assemble_struct(&name, fields, &options);

    // Build the struct's code. which then needs to be converted
    // from proc_macro2::TokenStream to proc_macro::TokenStream
    build_struct_code(&name, &options).into()
}

// +----------------------------------+ //
// |                                  | //
// |         Struct Parsing           | //
// |                                  | //
// +----------------------------------+ //
/// Parses the struct definition and returns the name and fields
/// - Stops if there a duplicate field name
/// - Stops if there is an invalid field type
fn parse_struct(stream: TokenStream) -> (String, Vec<StructField>, StructOptions) {
    // Consume the first identifier
    let mut stream = stream.into_iter();

    // Consume the first identifier
    let name = stream.next().expect("macro body cannot be empty");
    let name = match name {
        TokenTree::Ident(ident) => ident.to_string(),
        _ => abort!(name.span(), "expected identifier (write the struct name)"),
    };

    // Extract the group
    let group: TokenTree = stream.next().expect("unexpected end of macro body");
    let group = match group {
        TokenTree::Group(group) => {
            if group.delimiter() != proc_macro::Delimiter::Brace {
                abort!(group.span(), "expected struct body (enclosed in braces)");
            }
            group
        }
        _ => abort!(group.span(), "expected struct body"),
    };

    // Extract the fields
    let fields = consume_struct_body(&group);
    if fields.is_empty() {
        abort!(group.span(), "struct body cannot be empty");
    }

    // Get the options
    let mut options = StructOptions::default();
    let mut next_token = stream.next();

    while let Some(token) = next_token {
        match token {
            TokenTree::Ident(ref ident) => match ident.to_string().as_str() {
                "DEBUG" => options.debug = true,
                "PRIVATE" => options.private = true,
                _ => abort!(ident.span(), "expected valid flag"),
            },
            _ => abort!(token.span(), "expected valid flag"),
        };
        // Get the next token
        next_token = stream.next();
    }

    // Consume the struct body
    (name, fields, options)
}

/// Represents an integer type with a size and signedness
#[derive(Debug, PartialEq, Eq)]
struct IntegerType(u32, bool);

#[derive(Debug)]
enum StructFieldType {
    Void,
    Int(IntegerType),
    BitField(IntegerType, u32),
    Array(Box<StructFieldType>, u32),
    Pointer(Box<StructFieldType>),
    Struct(String),
    Vector(Box<StructFieldType>, TokenStream),
}
#[derive(Debug)]
struct StructField {
    name: String,
    ty: StructFieldType,
}

fn consume_struct_body(group: &Group) -> Vec<StructField> {
    let mut fields = vec![];

    // Transform the group into a stream
    let mut stream = group.stream().into_iter();

    loop {
        // This token should indicate the type or the end of the struct
        let token = stream.next();
        if token.is_none() {
            break;
        }

        let token = token.unwrap();

        match token {
            TokenTree::Ident(ident) => fields.push(parse_field(&mut stream, ident)),
            _ => {
                abort!(token.span(), "unexpected token");
            }
        }
    }

    // Check for duplicate fields
    let mut names = vec![];
    for field in &fields {
        if names.contains(&field.name) {
            abort!(group.span(), "duplicate field name: {}", field.name);
        }
        names.push(field.name.clone());
    }

    fields
}

fn parse_field(stream: &mut impl Iterator<Item = TokenTree>, start_token: Ident) -> StructField {
    let mut field_type = match start_token.to_string().as_str() {
        "void" => StructFieldType::Void,
        "i8" => StructFieldType::Int(IntegerType(8, true)),
        "i16" => StructFieldType::Int(IntegerType(16, true)),
        "i32" => StructFieldType::Int(IntegerType(32, true)),
        "u8" => StructFieldType::Int(IntegerType(8, false)),
        "u16" => StructFieldType::Int(IntegerType(16, false)),
        "u32" => StructFieldType::Int(IntegerType(32, false)),
        "struct" => {
            // Find name or group for anonymous struct
            let token = stream.next().expect("unexpected end of macro body");
            match token {
                TokenTree::Ident(ident) => StructFieldType::Struct(ident.to_string()),
                TokenTree::Group(_) =>
                    abort!(token.span(), "anonymous structs are not supported by design. Declare a named struct instead."),
                _ => abort!(token.span(), "expected struct name or body"),
            }
        }
        "const" => abort!(
            start_token.span(),
            "const is not supported by design. Remove it when copying struct definitions from C."
        ),
        _ => abort!(
            start_token.span(),
            "unexpected token: expected type or end of struct"
        ),
    };
    let mut token = stream.next().expect("unexpected end of macro body");

    // Parse any array
    loop {
        if let TokenTree::Group(group) = token {
            match group.delimiter() {
                proc_macro::Delimiter::Bracket => (),
                proc_macro::Delimiter::Parenthesis => {
                    abort!(group.span(), "if you are trying to declare a pointer to an array, use t[n]* x instead of t(*x)[n]")
                }
                _ => abort!(group.span(), "expected array size (enclosed in brackets)"),
            }

            let size = match group.stream().into_iter().next() {
                Some(TokenTree::Literal(literal)) => match literal.to_string().parse::<u32>() {
                    Ok(size) => size,
                    Err(_) => abort!(literal.span(), "expected integer literal"),
                },
                Some(_) => abort!(group.span(), "expected array size"),
                None => abort!(group.span(), "expected array size"),
            };

            field_type = StructFieldType::Array(Box::new(field_type), size);
            token = stream.next().expect("unexpected end of macro body");
        } else {
            break;
        }
    }

    // Count the number of dereferences
    let mut derefs = 0;

    loop {
        match token {
            TokenTree::Punct(ref punct) => {
                if punct.as_char() == '*' {
                    derefs += 1;
                } else {
                    break;
                }
            }
            _ => break,
        }

        token = stream.next().expect("unexpected end of macro body");
    }

    // Parse the field name
    let name = match token {
        TokenTree::Ident(ident) => ident.to_string(),
        _ => abort!(token.span(), "expected identifier"),
    };

    // Build the struct type
    for _ in 0..derefs {
        field_type = StructFieldType::Pointer(Box::new(field_type));
    }

    // Parse the [size] if present
    let mut token = stream.next().expect("unexpected end of macro body");
    loop {
        match token {
            TokenTree::Punct(ref punct) => match punct.as_char() {
                ';' => break,
                ':' => {
                    // Parse the bitfield size
                    let size_token = stream.next().expect("unexpected end of macro body");
                    let size = match &size_token {
                        TokenTree::Literal(lit) => match lit.to_string().parse::<u32>() {
                            Ok(size) => size,
                            Err(_) => abort!(lit.span(), "expected integer literal"),
                        },
                        _ => abort!(punct.span(), "expected integer literal"),
                    };

                    if size == 0 {
                        abort!(punct.span(), "bitfield size cannot be zero");
                    }

                    let integer_type = match field_type {
                        StructFieldType::Int(integer_type) => integer_type,
                        _ => abort!(punct.span(), "bitfields can only be applied to integers"),
                    };

                    if size > integer_type.0 * 8 {
                        abort!(
                            punct.span(),
                            "bitfield size cannot be larger than the integer size"
                        );
                    }

                    field_type = StructFieldType::BitField(integer_type, size);
                }
                _ => abort!(punct.span(), "unexpected token"),
            },
            TokenTree::Group(group) => match group.delimiter() {
                // Handle array
                proc_macro::Delimiter::Bracket => {
                    let size = match group
                        .stream()
                        .into_iter()
                        .next()
                        .expect("unexpected end of macro body")
                    {
                        TokenTree::Literal(lit) => match lit.to_string().parse::<u32>() {
                            Ok(size) => size,
                            Err(_) => abort!(lit.span(), "expected integer literal"),
                        },
                        _ => abort!(group.span(), "expected integer literal"),
                    };

                    if size == 0 {
                        abort!(group.span(), "array size cannot be zero");
                    }

                    field_type = StructFieldType::Array(Box::new(field_type), size);
                }
                // Handle vector
                proc_macro::Delimiter::Brace => {
                    // If the current type is already a vector, abort
                    if let StructFieldType::Vector(_, _) = field_type {
                        abort!(group.span(), "nested vectors are not supported");
                    }

                    // Take the inner tokenstream as is and add it to the vector
                    field_type = StructFieldType::Vector(Box::new(field_type), group.stream());
                }
                _ => {
                    abort!(group.span(), "unexpected token");
                }
            },
            _ => abort!(token.span(), "unexpected token"),
        }

        token = stream.next().expect("unexpected end of macro body");
    }

    StructField {
        name,
        ty: field_type,
    }
}

// +----------------------------------+ //
// |                                  | //
// |        Struct Assembling         | //
// |                                  | //
// +----------------------------------+ //
#[derive(Debug)]
struct AStruct {
    name: String,
    size: u32,
    align: u32,
    fields: Vec<AStructField>,
}

#[derive(Debug)]
struct AStructField {
    name: String,
    offset: u32,
    ty: AFieldType,
}

#[derive(Debug)]
enum AFieldType {
    Void,
    Int(IntegerType),
    Pointer(Box<AFieldType>),
    Vector(Box<AFieldType>, String),
    Array(Box<AFieldType>, u32),
    Struct(String),
    // Vector contains (name, offset, size)
    BitFields(IntegerType, Vec<(String, u32, u32)>),
}

/// Uses the parsed struct fields to assemble the struct into a
/// format that can be used to generate the read/write code.
///
/// Does things like computing fields offsets combining bitfields,
/// and computing the struct size and alignment.
fn assemble_struct(name: &String, fields: Vec<StructField>, options: &StructOptions) {
    let debug = options.debug;

    use StructFieldType::*;

    // Loop over each field in the parsed ones
    let mut offset: u32 = 0;
    let mut alignment_struct: u32 = 0;
    let mut afields = Vec::new();

    // Fields are (name, offset, size)
    let mut bitfields: Vec<(String, u32, u32)> = Vec::new();
    // Fields are (type, offset)
    let mut bitfields_status: Option<(IntegerType, u32)> = None;

    macro_rules! flush_bitfields {
        () => {
            // Add the bitfields to the struct
            let (int_ty, _) = bitfields_status.unwrap();
            let offset_delta = int_ty.0 >> 3;

            align_offset!(offset_delta);
            afields.push(AStructField {
                name: String::new(),
                offset,
                ty: AFieldType::BitFields(int_ty, bitfields),
            });
            advance_offset!(offset_delta);
        };
    }

    macro_rules! align_offset {
        ($alignment:expr) => {
            if offset % $alignment != 0 {
                offset += $alignment - (offset % $alignment);
            }
        };
    }

    macro_rules! advance_offset {
        ($size:expr) => {
            offset += $size;
        };
    }

    for StructField { name, ty } in fields {
        // Align to this type's size
        let alignment = ty.align();
        // The struct alignment is the max of all the fields alignments
        alignment_struct = std::cmp::max(alignment_struct, alignment);

        // Handle the bitfields
        match ty {
            BitField(new_ty, bit_size) => {
                match bitfields_status {
                    Some((prev_ty, bit_offset)) => {
                        if new_ty != prev_ty || bit_offset + bit_size > prev_ty.0 {
                            let offset_delta = prev_ty.0 >> 3;

                            align_offset!(offset_delta);
                            // The bitfield type changed, add the bitfields to the struct
                            afields.push(AStructField {
                                name: String::new(),
                                offset,
                                ty: AFieldType::BitFields(prev_ty, bitfields),
                            });
                            advance_offset!(offset_delta);

                            bitfields = vec![(name, 0, bit_size)];
                            bitfields_status = Some((new_ty, bit_size));
                        } else {
                            // I see another bitfield of the same type
                            bitfields.push((name, bit_offset, bit_size));
                            bitfields_status = Some((new_ty, bit_offset + bit_size));
                        }
                    }
                    None => {
                        // I see the first bitfield since the last non-bitfield field
                        bitfields.push((name, 0, bit_size));
                        bitfields_status = Some((new_ty.into(), bit_size));
                    }
                }
                // Do not continue if you just handled a bitfield
                continue;
            }
            _ => {
                if !bitfields.is_empty() {
                    flush_bitfields!();

                    bitfields = Vec::new();
                }
                // Reset the bitfield status
                bitfields_status = None;
            }
        };

        // Convert the field type to the AFieldType enum
        let ty: AFieldType = ty.into();
        let offset_delta = ty.size();

        align_offset!(alignment);
        // Add the field to the struct
        afields.push(AStructField { name, offset, ty });
        advance_offset!(offset_delta);
    }

    // Add the last bitfields to the struct
    if !bitfields.is_empty() {
        flush_bitfields!();
    }

    // Align the size to the struct align
    align_offset!(alignment_struct);

    let res = AStruct {
        name: name.clone(),
        size: offset,
        align: alignment_struct,
        fields: afields,
    };

    if debug {
        println!(
            "Printing output for struct {} because you added the DEBUG flag",
            name
        );
        println!("{}", res);
    }

    // Add the struct to the global list (do not rStruct(name) => AFieldType::Struct(name),eturn it)
    GBA_STRUCTS.lock().unwrap().insert(name.clone(), res);
}

impl Into<AFieldType> for StructFieldType {
    fn into(self) -> AFieldType {
        use StructFieldType::*;
        match self {
            Void => AFieldType::Void,
            Int(ty) => AFieldType::Int(ty.into()),
            Pointer(ty) => AFieldType::Pointer(Box::new((*ty).into())),
            Array(ty, size) => AFieldType::Array(Box::new((*ty).into()), size),
            Struct(name) => AFieldType::Struct(name),
            Vector(ty, stream) => AFieldType::Vector(Box::new((*ty).into()), stream.to_string()),
            BitField(_, _) => {
                unreachable!("bitfields should have been combined into a single field")
            }
        }
    }
}

impl StructFieldType {
    fn align(&self) -> u32 {
        use StructFieldType::*;
        match self {
            Int(ty) => ty.0 >> 3,
            BitField(ty, _) => ty.0 >> 3,
            Pointer(_) | Vector(_, _) => 4,
            Array(ty, _) => (*ty).align(),
            Struct(name) => {
                let x = GBA_STRUCTS.lock().unwrap();
                let struct_ = x.get(name).unwrap();
                struct_.align
            }
            Void => panic!("something went wrong if you're trying to align void"),
        }
    }
}

impl AFieldType {
    fn size(&self) -> u32 {
        use AFieldType::*;
        match self {
            Int(ty) => ty.0 >> 3,
            Pointer(_) | Vector(_, _) => 4,
            Array(ty, size) => (*ty).size() * size,
            Struct(name) => {
                let x = GBA_STRUCTS.lock().unwrap();
                let struct_ = x.get(name).unwrap();
                struct_.size
            }
            BitFields(ty, _) => ty.0 >> 3,
            Void => panic!("something went wrong if you're trying to get the size of void"),
        }
    }
}

impl Display for IntegerType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", if self.1 { "i" } else { "u" }, self.0)
    }
}

impl Display for AFieldType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use AFieldType::*;
        match self {
            Void => write!(f, "void"),
            Int(ty) => write!(f, "{}", ty),
            Pointer(ty) => write!(f, "{}*", ty),
            Vector(ty, stream) => write!(f, "{}{{{}}}", ty, stream),
            Array(ty, size) => write!(f, "{}[{}]", ty, size),
            Struct(name) => write!(f, "struct {}", name),
            BitFields(ty, fields) => {
                write!(f, "{} {{ ", ty)?;
                for (name, bits, size) in fields {
                    write!(f, "{}:{}-{} ", name, bits, size + bits - 1)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl Display for AStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        writeln!(f, "struct {} {{", self.name)?;
        for field in &self.fields {
            write!(f, "  {:>3}", field.offset)?;

            match &field.ty {
                AFieldType::BitFields(_, _) => writeln!(f, " bitfields {},", field.ty)?,
                _ => writeln!(f, " {} {},", field.ty, field.name)?,
            }
        }
        write!(f, "}} size {} align {}", self.size, self.align)
    }
}

// +----------------------------------+ //
// |                                  | //
// |          Code Building           | //
// |                                  | //
// +----------------------------------+ //
/// Taken the struct's name, this function retrieves the struct
/// from the global list and returns the code necessary to make
/// it into a GBAType, so it implements the Rust struct itself,
/// then the GBAType trait.
fn build_struct_code(name: &String, options: &StructOptions) -> TokenStream2 {
    // Get a reference to the struct to get its fields and their types
    let lock = GBA_STRUCTS.lock().unwrap();
    let struct_ = lock.get(name).unwrap();

    // Build the struct body itself
    let body = build_body(name, struct_, options.private);
    let trait_ = build_trait(name, struct_);

    quote! {
        #body
        #trait_
    }
}

fn build_body(name: &String, struct_: &AStruct, is_private: bool) -> TokenStream2 {
    let mut fields = TokenStream2::new();

    // Extract the field names
    for field in &struct_.fields {
        let field_name = &field.name;
        let field_ty = &field.ty;

        fields.extend(build_field(field_name, field_ty));
    }

    // Add public only if private is not set
    let pub_token = if is_private {
        quote! {}
    } else {
        quote! { pub }
    };

    let name = format_ident!("{}", name);
    quote! {
        #[derive(Debug, Default, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
        #pub_token struct #name {
            #fields
        }
    }
}

fn build_field(name: &String, ty: &AFieldType) -> TokenStream2 {
    // If this is a bitfield, we need to build each field separately
    if let AFieldType::BitFields(ty, fields) = ty {
        let mut res = TokenStream2::new();

        // Build each bit field separately
        for (name, _, _) in fields {
            let ty = AFieldType::Int(IntegerType(ty.0, ty.1));
            let field = build_field(name, &ty);
            res.extend(field);
        }

        return res;
    }

    // Build the type of the field
    let ty = build_type(ty);
    let name = format_ident!("{}", name);
    // Build the field itself
    quote! { pub #name: #ty, }
}

fn build_type(ty: &AFieldType) -> TokenStream2 {
    use AFieldType::*;

    match ty {
        Void => quote! { gba_types::pointers::Nothing },
        Int(ty) => {
            let ty = format_ident!("{}{}", if ty.1 { "i" } else { "u" }, ty.0);
            quote! { #ty }
        }
        Pointer(ty) => {
            let ty = build_type(ty);
            quote! { gba_types::pointers::PointedData<#ty> }
        }
        Array(ty, size) => {
            let ty = build_type(ty);
            let size = *size as usize;
            quote! { [#ty; #size] }
        }
        Struct(name) => {
            let name = format_ident!("{}", name);
            quote! { #name }
        }
        Vector(ty, _) => {
            let ty = build_type(ty);
            quote! { gba_types::vectors::VectorData<#ty> }
        }
        BitFields(_, _) => panic!("bitfields should have been handled as separate fields"),
    }
}

fn build_trait(name: &String, struct_: &AStruct) -> TokenStream2 {
    let name = format_ident!("{}", name);
    let size = struct_.size as usize;

    let read_from = build_read_from(&name, struct_);
    let write_to = build_write_to(struct_);

    quote! {
        impl gba_types::GBAType for #name {
            const SIZE: usize = #size;

            fn read_from(bytes: &[u8], offset: usize) -> Result<Self, gba_types::GBAIOError> {
                #read_from
            }

            fn write_to(&self, bytes: &mut [u8], offset: usize) -> Result<(), gba_types::GBAIOError> {
                #write_to
            }
        }
    }
}

fn build_read_from(name: &Ident2, struct_: &AStruct) -> TokenStream2 {
    let mut read_fields = TokenStream2::new();
    let mut struct_innards = TokenStream2::new();

    for AStructField { name, ty, offset } in &struct_.fields {
        // Fill the struct innards for everything
        if let AFieldType::BitFields(_, fields) = ty {
            for (name, _, _) in fields {
                let bindname = format_ident!("{}_value", name);
                let name = format_ident!("{}", name);
                struct_innards.extend(quote! { #name: #bindname, });
            }
        } else {
            let bindname = format_ident!("{}_value", name);
            let name = format_ident!("{}", name);
            struct_innards.extend(quote! { #name: #bindname, });
        }

        // Call read for everyone now, then we'll handle the vectors later
        if let AFieldType::Vector(_, _) = ty {
            continue;
        }
        read_fields.extend(build_read_field(name, ty, offset));
    }

    // Read the vectors only at the end
    for AStructField { name, ty, offset } in &struct_.fields {
        if let AFieldType::Vector(_, _) = ty {
            read_fields.extend(build_read_field(name, ty, offset));
        }
    }

    quote! {
        // Read each field
        #read_fields

        // Return the struct
        Ok(#name {
            #struct_innards
        })
    }
}

fn build_read_field(name: &String, ty: &AFieldType, offset: &u32) -> TokenStream2 {
    let name = format_ident!("{}_value", name);
    let offset = *offset as usize;

    if let AFieldType::BitFields(ty, fields) = ty {
        // Build the variable from which you will read
        let fields_bind = format_ident!("fields");
        let fields_type = format_ident!("{}{}", if ty.1 { "i" } else { "u" }, ty.0);
        // Read the variable
        let bitfield_read = quote! {
            let #fields_bind: #fields_type = gba_types::GBAType::read_from(bytes, offset + #offset)?;
        };

        let bitlen = ty.0 as usize;

        let mut read_fields = TokenStream2::new();

        for (name, offset, size) in fields {
            let name = format_ident!("{}_value", name);
            let offset = *offset as usize;
            let size = *size as usize;

            let shift_left = bitlen - size - offset;
            let shift_right = bitlen - size;

            read_fields.extend(quote! {
                let #name: #fields_type = (#fields_bind << #shift_left) >> #shift_right;
            });
        }

        quote! {
            #bitfield_read
            #read_fields
        }
    } else if let AFieldType::Vector(inner_ty, stream) = ty {
        let ty = build_type(&ty);

        // Parse the string into a token stream
        let inner_ty = build_type(inner_ty);
        let token_stream = stream.parse::<TokenStream2>().unwrap();
        let output_stream = replace_identifiers(token_stream);

        quote! {
            let #name: #ty = gba_types::vectors::read_vector::<#inner_ty>(bytes, offset + #offset, { #output_stream } as usize)?;
        }
    } else {
        let tty = build_type(&ty);

        quote! {
            let #name: #tty = gba_types::GBAType::read_from(bytes, offset + #offset)?;
        }
    }
}

fn replace_identifiers(input_stream: TokenStream2) -> TokenStream2 {
    let mut output_stream = TokenStream2::new();
    let mut replace_next = false;

    // Replace each identifier with ident_value
    for token in input_stream.into_iter() {
        match token {
            proc_macro2::TokenTree::Ident(ident) => {
                if !replace_next {
                    output_stream.extend(quote! { #ident });
                    continue;
                }
                let ident: Ident2 = format_ident!("{}_value", ident);
                output_stream.extend(quote! { #ident });
                replace_next = false;
            }
            proc_macro2::TokenTree::Group(group) => {
                replace_next = false;
                let group_stream = replace_identifiers(group.stream());
                let group = proc_macro2::Group::new(group.delimiter(), group_stream);
                output_stream.extend(quote! { #group });
            }
            _ => {
                // If the token is a $ sign, the next identifier will be replaced
                if let proc_macro2::TokenTree::Punct(punct) = &token {
                    if punct.as_char() == '$' {
                        replace_next = true;
                        continue;
                    }
                }

                replace_next = false;
                output_stream.extend(quote! { #token })
            }
        }
    }

    output_stream
}

fn build_write_to(struct_: &AStruct) -> TokenStream2 {
    let mut write_fields = TokenStream2::new();

    for AStructField { name, ty, offset } in &struct_.fields {
        let offset = *offset as usize;

        if let AFieldType::BitFields(ty, fields) = ty {
            let fields_name = format_ident!("fields");
            let fields_type = format_ident!("u{}", ty.0);

            write_fields.extend(quote! {
                let mut #fields_name: #fields_type = gba_types::GBAType::read_from(bytes, offset + #offset)?;
            });

            for (name, bitoffset, bitsize) in fields {
                let name = format_ident!("{}", name);
                let bitoffset = *bitoffset as usize;
                let bitsize = *bitsize as usize;

                let mask = (1 << bitsize) - 1;

                write_fields.extend(quote! {
                    #fields_name |= (self.#name as #fields_type & #mask as #fields_type) << #bitoffset;
                });
            }

            write_fields.extend(quote! {
                gba_types::GBAType::write_to(&#fields_name, bytes, offset + #offset)?;
            });
        } else if let AFieldType::Vector(_, _) = ty {
            let name = format_ident!("{}", name);
            write_fields.extend(quote! {
                gba_types::vectors::write_vector(bytes, offset + #offset, &self.#name)?;
            });
        } else {
            let name = format_ident!("{}", name);
            write_fields.extend(quote! {
                gba_types::GBAType::write_to(&self.#name, bytes, offset + #offset)?;
            });
        }
    }

    quote! {
        // Read each field
        #write_fields

        Ok(())
    }
}
