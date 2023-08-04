//! A basic Thumb emulator.
mod alu;
mod instructions;
mod memory;
mod processor;
pub(crate) mod utils;

pub use instructions::Instruction;
pub use memory::Memory;
pub use processor::{ExecutionError, Processor};
