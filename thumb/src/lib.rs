//! A basic Thumb emulator.
mod alu;
pub(crate) mod asm;
mod instructions;
mod memory;
mod processor;
pub(crate) mod utils;

pub use asm::disassembly::{disassemble, DisassemblyOptions};
pub use instructions::Instruction;
pub use memory::Memory;
pub use processor::{ExecutionError, Processor};
