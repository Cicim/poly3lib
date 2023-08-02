use crate::{alu::Alu, memory::Memory};

/// Thumb processor
struct Processor<'rom> {
    /// Arithmetic Logic Unit
    alu: Alu,

    /// Registers
    ///
    /// + R0-R7 are general purpose registers.
    /// + R8-R12 are high registers.
    /// + R13 is the stack pointer.
    /// + R14 is the link register.
    /// + R15 is the program counter.
    registers: [u32; 16],

    /// Memory
    memory: Memory<'rom>,
}
