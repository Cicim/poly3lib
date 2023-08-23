//! Contains the types exported by the parser along with their methods.
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote};

// ANCHOR Struct
/// The parsed data for this struct.
pub struct ParsedStruct {
    /// The name of the struct.
    pub name: Ident,
    /// The fields of the struct.
    pub fields: Vec<StructField>,
    /// The flags for the struct.
    pub flags: StructFlags,
}

impl std::fmt::Debug for ParsedStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Struct: {}", self.name)?;
        writeln!(f, "Flags: {:?}", self.flags)?;
        writeln!(f, "Fields:")?;
        for field in &self.fields {
            match field {
                StructField::Field(StructBasicField {
                    name,
                    ty,
                    attributes,
                }) => {
                    let num = attributes.len();
                    let char = if num == 0 { " " } else { "A" };

                    writeln!(
                        f,
                        "    F{} {:>31} {}",
                        char,
                        format!("{:?}", ty),
                        name.to_string()
                    )?;

                    for (i, attr) in attributes.iter().enumerate() {
                        let char = if i == num - 1 { '└' } else { '├' };
                        writeln!(f, "{:>38} {:?}", char, attr)?;
                    }
                }
                StructField::BitField(bitfields) => {
                    let num = bitfields.sizes.len();
                    let ty = format!("{:?}", bitfields.ty);

                    for (i, (name, size)) in bitfields
                        .names
                        .iter()
                        .zip(bitfields.sizes.iter())
                        .enumerate()
                    {
                        let char = if num == 1 {
                            '─'
                        } else {
                            if i == 0 {
                                '┌'
                            } else if i == num - 1 {
                                '└'
                            } else {
                                '├'
                            }
                        };

                        writeln!(f, "    B {:>20} {:>7}:{:>3} {}", char, ty, size, name)?;
                    }
                }
            }
        }
        Ok(())
    }
}

/// Flags for the struct.
#[derive(Default, Debug)]
pub struct StructFlags {
    /// Whether to **not** set this struct as public, effectively making it private
    pub private: bool,
    /// Whether to **not** derive `Debug` for this struct
    pub no_debug: bool,
    /// Whether to print the output of the parser (for debugging)
    pub print_parser_output: bool,
}

// ANCHOR Struct fields
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

// ANCHOR Attributes
/// An attribute for specifying when some fields are missing or have
/// a different type based on a ROM base or specific configuration
#[derive(Clone)]
pub struct StructFieldAttribute {
    /// The condition for this attribute to be applied
    pub condition: StructAttributeCondition,
    /// The action to take if the condition is met
    pub action: StructAttributeAction,
}

impl std::fmt::Debug for StructFieldAttribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#[for({:?}, {:?})", self.condition, self.action)
    }
}

/// The condition for an attribute to be applied
#[derive(Clone)]
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

impl std::fmt::Debug for StructAttributeCondition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StructAttributeCondition::Base(idents) => {
                write!(
                    f,
                    "base({})",
                    idents
                        .iter()
                        .map(|i| i.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            StructAttributeCondition::Cfg(ident) => write!(f, "cfg({})", ident),
            StructAttributeCondition::NotCfg(ident) => write!(f, "cfg(!{})", ident),
        }
    }
}

/// The action to take if the condition is met
#[derive(Clone)]
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

impl std::fmt::Debug for StructAttributeAction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StructAttributeAction::Type(basetype) => write!(f, "type({:?})", basetype),
            StructAttributeAction::Default(literal) => {
                write!(f, "default({})", literal.to_string())
            }
        }
    }
}

// ANCHOR Types

/// A type with modifiers applied to it.
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

impl std::fmt::Debug for DerivedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DerivedType::Base(base) => write!(f, "{:?}", base),
            DerivedType::Pointer(ty) => write!(f, "*{:?}", ty),
            DerivedType::Array(ty, len) => write!(f, "[{}]{:?}", len, ty),
        }
    }
}

/// Type that can be found at the start of a type definition
#[derive(Clone)]
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

impl std::fmt::Debug for BaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseType::Struct(name) => write!(f, "struct {}", name),
            BaseType::Void => write!(f, "void"),
            BaseType::Integer(int) => write!(f, "{:?}", int),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum SizedBaseType {
    /// An unsigned integer with the given number of bits (8, 16 or 32)
    Unsigned(u8),
    /// A signed integer with the given number of bits (8, 16 or 32)
    Signed(u8),
    /// A boolean with the given number of bits (8, 16 or 32)
    Boolean(u8),
}

impl std::fmt::Debug for SizedBaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SizedBaseType::Unsigned(x) => write!(f, "u{}", x),
            SizedBaseType::Signed(x) => write!(f, "i{}", x),
            SizedBaseType::Boolean(x) => write!(f, "bool{}", x),
        }
    }
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

// ANCHOR Convert types to tokens
pub trait BuildToTokens {
    /// Convert this type to the tokens necessary to define it
    /// in the context of the built code.
    fn build_to_tokens(&self) -> TokenStream;
}

impl BuildToTokens for DerivedType {
    fn build_to_tokens(&self) -> TokenStream {
        match self {
            DerivedType::Base(base) => base.build_to_tokens(),
            DerivedType::Pointer(ty) => {
                let inner = ty.build_to_tokens();
                quote! { rom_data::types::RomPointer<#inner> }
            }
            DerivedType::Array(ty, size) => {
                let inner = ty.build_to_tokens();
                let size = Literal::u32_unsuffixed(*size);
                quote! { rom_data::types::RomArray<#inner, #size> }
            }
        }
    }
}

impl BuildToTokens for BaseType {
    fn build_to_tokens(&self) -> TokenStream {
        match self {
            BaseType::Integer(int) => int.build_to_tokens(),
            // For structs, just return the identifier
            BaseType::Struct(ident) => quote! { #ident },

            // Void is only allowed in pointer types, and the reason why you
            // use it is to read a RomPointer without anything, so you can just
            // format it as Void;
            BaseType::Void => quote! { rom_data::types::Void },
        }
    }
}

impl BuildToTokens for SizedBaseType {
    fn build_to_tokens(&self) -> TokenStream {
        match self {
            SizedBaseType::Unsigned(x) => {
                // Build the identifier
                let usomething = format_ident!("u{}", x);
                quote! { #usomething }
            }
            SizedBaseType::Signed(x) => {
                // Build the identifier
                let isomething = format_ident!("i{}", x);
                quote! { #isomething }
            }
            SizedBaseType::Boolean(_) => quote! { bool },
        }
    }
}

// ANCHOR Get the size and alignment of a type if you know it.
pub enum TypeDimension {
    /// This dimension is known a priori
    Known(usize),
    /// This dimension is not known a priori, but can be obtained using the
    /// expression given as token stream
    Code(TokenStream),
}
impl TypeDimension {
    /// Gets the expression that returns the expression
    pub fn get_dimension_code(&self) -> TokenStream {
        match self {
            TypeDimension::Known(value) => quote! { #value },
            TypeDimension::Code(code) => quote! { #code },
        }
    }
}

pub trait GetSizeAndAlignment {
    /// Returns the size of this type in bytes or the tokens needed to read it.
    fn get_size(&self) -> TypeDimension;

    /// Returns the alignment of this type in bytes (or the tokens needed to read it)
    fn get_alignment(&self) -> TypeDimension;
}

impl GetSizeAndAlignment for StructField {
    fn get_size(&self) -> TypeDimension {
        match self {
            // Instead of making things to complicated right now, we can just rely on the Rust
            // compiler to optimize the code if the size is known at compile time.
            StructField::Field(StructBasicField { ty, attributes, .. }) => {
                // You have to consider attributes for this.
                if attributes.len() == 0 {
                    return ty.get_size();
                }

                // Obtain the code to read this stuff
                TypeDimension::Code(build_attribute_condition(
                    attributes,
                    |attr_action| match attr_action {
                        // If it changes in some ROM, return the other size
                        StructAttributeAction::Type(ty) => Some(ty.get_size().get_dimension_code()),
                        // Otherwise, return 0
                        StructAttributeAction::Default(_) => Some(quote! { 0 }),
                    },
                    ty.get_size().get_dimension_code(),
                ))
            }
            StructField::BitField(StructBitFields { ty, .. }) => ty.get_size(),
        }
    }

    fn get_alignment(&self) -> TypeDimension {
        match self {
            StructField::Field(StructBasicField { ty, attributes, .. }) => {
                // You have to consider attributes for this.
                if attributes.len() == 0 {
                    return ty.get_alignment();
                }

                // Obtain the code to read this stuff
                TypeDimension::Code(build_attribute_condition(
                    attributes,
                    |attr_action| {
                        if let StructAttributeAction::Type(ty) = attr_action {
                            Some(ty.get_alignment().get_dimension_code())
                        } else {
                            None
                        }
                    },
                    ty.get_alignment().get_dimension_code(),
                ))
            }
            StructField::BitField(StructBitFields { ty, .. }) => ty.get_alignment(),
        }
    }
}

impl GetSizeAndAlignment for DerivedType {
    fn get_size(&self) -> TypeDimension {
        match self {
            DerivedType::Base(base) => base.get_size(),
            DerivedType::Pointer(_) => TypeDimension::Known(4),
            DerivedType::Array(ty, len) => match ty.get_size() {
                TypeDimension::Known(size) => TypeDimension::Known(size * (*len as usize)),
                // The code needs to be modified to multiply the size by the length
                TypeDimension::Code(code) => TypeDimension::Code(quote! {
                    (#code) * #len
                }),
            },
        }
    }

    fn get_alignment(&self) -> TypeDimension {
        match self {
            DerivedType::Base(base) => base.get_alignment(),
            DerivedType::Pointer(_) => TypeDimension::Known(4),
            DerivedType::Array(ty, _) => ty.get_alignment(),
        }
    }
}

impl GetSizeAndAlignment for BaseType {
    fn get_size(&self) -> TypeDimension {
        match self {
            // You cannot know it for structs (you have to read it)
            BaseType::Struct(ident) => TypeDimension::Code(quote! {
                <#ident as rom_data::types::RomSizedType>::get_size(rom)
            }),

            BaseType::Integer(int) => int.get_size(),

            // Since it is not possible to define a void type that is not
            // behind a pointer, it should be impossible to reach this.
            BaseType::Void => unreachable!(
                "{}{}",
                "Trying to get the size of void, which is impossible ",
                "since a type cannot be instantiated as void without a pointer"
            ),
        }
    }

    fn get_alignment(&self) -> TypeDimension {
        match self {
            // You cannot know it for structs (you have to read it)
            BaseType::Struct(ident) => TypeDimension::Code(quote! {
                <#ident as rom_data::types::RomSizedType>::get_alignment(rom)
            }),

            BaseType::Integer(int) => int.get_alignment(),

            // Since it is not possible to define a void type that is not
            // behind a pointer, it should be impossible to reach this.
            BaseType::Void => unreachable!(
                "{}{}",
                "Trying to get the alignment of void, which is impossible ",
                "since a type cannot be instantiated as void without a pointer"
            ),
        }
    }
}

impl GetSizeAndAlignment for SizedBaseType {
    fn get_size(&self) -> TypeDimension {
        TypeDimension::Known(self.bits() as usize / 8)
    }
    fn get_alignment(&self) -> TypeDimension {
        TypeDimension::Known(self.bits() as usize / 8)
    }
}

// ANCHOR Attribute parsing
/// Takes a list of attributes as input, a function that takes the action of an attribute
/// and may return some code and some fallback code.
///
/// If the function returns some code, then the code for checking the condition is built
/// and that code is put under an if statement with that condition, otherwise the condition
/// is ignored.
///
/// A chain is built, and at the end of it there is an else block with the provided
/// `else_action`, only if there is at least an alternative action to perform, otherwise
/// only the `else_action` code is returned with no if.
pub fn build_attribute_condition(
    attributes: &Vec<StructFieldAttribute>,
    transformer: fn(&StructAttributeAction) -> Option<TokenStream>,
    else_action: TokenStream,
) -> TokenStream {
    // The codes for each attribute in the shape of if #cond { #action }
    let mut codes = Vec::new();

    // Loop over the attributes
    for StructFieldAttribute { condition, action } in attributes.iter() {
        // Build the action code
        let action_code = match transformer(action) {
            Some(code) => code,
            None => continue,
        };

        // Build the condition for the attribute
        let cond_code = build_condition_code(condition);

        // Build the code for this attribute
        codes.push(quote! {
            if #cond_code {
                #action_code
            }
        });
    }

    // If no code was returned, return the else action
    if codes.is_empty() {
        quote! { #else_action; }
    } else {
        let mut result = quote! {};
        // Otherwise, return the code for all the attributes
        for (i, code) in codes.iter().enumerate() {
            if i != 0 {
                result.extend(quote! { else });
            }

            result.extend(quote! { #code });
        }

        quote! {
            #result
            else {
                #else_action
            };
        }
    }
}

fn build_condition_code(condition: &StructAttributeCondition) -> TokenStream {
    use StructAttributeCondition::*;

    match condition {
        Cfg(_) => unimplemented!("cfg not implemented yet"),
        NotCfg(_) => unimplemented!("!cfg not implemented yet"),

        // Builds to matches!(rom.base, rom_data::RomBase::Ruby | rom_data::RomBase::Sapphire)
        Base(tokens) => {
            let mut conditions = quote! {};
            let size = tokens.len();

            for (i, base) in tokens.iter().enumerate() {
                conditions.extend(quote! {
                    rom_data::RomBase::#base
                });

                if i != size - 1 {
                    conditions.extend(quote! { | });
                }
            }

            quote! { matches!(rom.base, #conditions) }
        }
    }
}
