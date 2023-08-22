//! Contains the types exported by the parser along with their methods.
use proc_macro2::{Ident, Literal};

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
