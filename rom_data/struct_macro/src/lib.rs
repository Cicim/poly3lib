use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;

mod parser;
mod parser_types;
mod struct_code;

// TODO RomClearableType implementation

/// Struct for constructing a struct along with its [`RomSizedType`],
/// [`RomReadableType`] and [`RomWritableType`] traits.
///
/// ## C syntax
/// The struct definition follows the same syntax of C structs:
/// + Size and alignment are computed following the same rules as in C.
/// + Derivated types are decoded as in C (with the same limitations as in C,
///   so you cannot have a void field without a pointer or an array of voids)
/// + Bitfields are created with the same rules as in C.
///
/// However, function types are not supported, and should be read as
/// `void*` instead.
///
/// ### Example
/// ```no_run
/// use rom_data::rom_struct;
///
/// rom_struct!(StructName {
///     u8 field1;
///     i32 field2_1:10;
///     i32 field2_2:14;
///     u8 *field3[16];
///     struct OtherStruct field4;
///     ...
/// });
/// ```
///
/// ## Vectors
/// A new functionality has been added to the syntax: vectors, which are
/// pointers to dynamic arrays whose size can be read from some of the fields.
///
/// Vectors are declared by putting a `{...}` block at the end of the type
/// definition (before the semicolon). Inside the block there has to be an
/// expression (that can use field names if prepended with `$`) that can be
/// cast to `usize`.
///
/// ### Example
/// ```no_run
/// rom_struct!(VectorStruct {
///     u8 size;
///     u32 data{$size};
/// });
/// ```
///
/// ## Attributes
/// Another functionality that has been added to the syntax is the ability to
/// define attributes for integer fields. This allows you to apply some differences
/// between ROM bases:
/// + Skipping reading one field in a ROM base by defining a default
/// + Reading a value with a different type (e.g. `u32` instead of `i8`), recomputing
///   size and alignment as necessary.
/// + Swapping two fields of the same type before reading or after writing.
///
/// Attributes are defined by putting a `#[...]` block before the field name. If more
/// than one field is defined in a line, the attributes apply to all fields in that line.
///
/// ### Example
/// ```no_run
/// rom_struct!(AttributeStruct {
///     #[for(base(Ruby, Sapphire), default(0x0))]
///     #[for(base(FireRed), default(0x20))]
///     u8 field1;
///     #[for(base(Emerald), type(u32))]
///     u16 field2, field3;
///     #[for(base(Emerald), swap(field2))]
///     u16 field4
///     ...
/// });
/// ```
///
/// ## Flags
/// After the struct body, some flags can be given to the macro for debugging
/// or other purposes. Here is a list of the available flags:
/// + `private` (or `priv`): does not add the pub keyword to the
///   struct definition and all its fields.
/// + `no_debug`: does not add the #[derive(Debug)] for this struct, allowing
///   the user to add it manually.
/// + `print_parser_output` (or `ppo`): prints the output of the parser to stderr.
///   Useful for debugging.
/// + `readonly`: does not implement `RomWritableType` for this struct.
#[proc_macro]
#[proc_macro_error]
pub fn rom_struct(input: TokenStream) -> TokenStream {
    // Parse the input struct into something you can use
    let parsed = parser::parse(input.into());

    if parsed.flags.print_parser_output {
        eprintln!("{:?}", parsed);
    }

    // Build everything required by the struct
    let code = struct_code::build(parsed);

    // Return the code as a TokenStream
    code.into()
}
