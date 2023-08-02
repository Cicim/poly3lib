use std::fmt::Display;

use crate::{
    alu::Alu,
    instructions::REGISTER_NAMES,
    memory::{Memory, MemoryError},
    Instruction,
};

/// Thumb processor
pub struct Processor<'rom> {
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

pub enum ExecutionError {
    MemoryError(MemoryError),
    InvalidInstruction,
}

impl<'rom> Processor<'rom> {
    /// Create a new processor
    pub fn new(rom: &'rom [u8]) -> Self {
        let mut cpu = Self {
            alu: Alu::new(),
            registers: [0; 16],
            memory: Memory::new(rom),
        };

        cpu.set_sp(0x03_000_000);
        cpu.set_pc(0x08_000_000);

        cpu
    }

    // ANCHOR Registry operations
    /// Set a register to a value
    pub fn set_register(&mut self, register: u8, value: u32) {
        self.registers[register as usize] = value;
    }
    /// Get a value from a register
    pub fn get_register(&self, register: u8) -> u32 {
        self.registers[register as usize]
    }

    /// Set the program counter
    pub fn set_pc(&mut self, value: u32) {
        self.registers[15] = value;
    }
    /// Get the program counter
    pub fn get_pc(&self) -> u32 {
        self.registers[15]
    }

    /// Set the link register
    pub fn set_lr(&mut self, value: u32) {
        self.registers[14] = value;
    }
    /// Get the link register
    pub fn get_lr(&self) -> u32 {
        self.registers[14]
    }

    /// Set the stack pointer
    pub fn set_sp(&mut self, value: u32) {
        self.registers[13] = value;
    }
    /// Get the stack pointer
    pub fn get_sp(&self) -> u32 {
        self.registers[13]
    }

    // ANCHOR Instructions
    /// Fetches and decodes the next instruction
    pub fn decode(&self) -> Result<Instruction, ExecutionError> {
        // Read the program counter
        let pc = self.get_pc();

        // Fetch the instruction
        let data = self
            .memory
            .read_halfword(pc)
            .map_err(ExecutionError::MemoryError)?;

        // Let someone else update pc

        // Decode the instruction
        Instruction::decode(data).ok_or(ExecutionError::InvalidInstruction)
    }

    /// Executes an instruction
    pub fn execute(&mut self, instruction: Instruction) -> Result<(), ExecutionError> {
        use Instruction::*;

        match instruction {
            LslImm { rd, rs, imm5 } => todo!(),
            LsrImm { rd, rs, imm5 } => todo!(),
            AsrImm { rd, rs, imm5 } => todo!(),
            AddReg { rd, rs, rn } => todo!(),
            AddImm3 { rd, rs, imm3 } => todo!(),
            SubReg { rd, rs, rn } => todo!(),
            SubImm3 { rd, rs, imm3 } => todo!(),
            MovImm { rd, imm8 } => todo!(),
            CmpImm { rd, imm8 } => todo!(),
            AddImm8 { rd, imm8 } => todo!(),
            SubImm8 { rd, imm8 } => todo!(),
            And { rd, rs } => todo!(),
            Eor { rd, rs } => todo!(),
            Lsl { rd, rs } => todo!(),
            Lsr { rd, rs } => todo!(),
            Asr { rd, rs } => todo!(),
            Adc { rd, rs } => todo!(),
            Sbc { rd, rs } => todo!(),
            Ror { rd, rs } => todo!(),
            Tst { rd, rs } => todo!(),
            Neg { rd, rs } => todo!(),
            Cmp { rd, rs } => todo!(),
            Cmn { rd, rs } => todo!(),
            Orr { rd, rs } => todo!(),
            Mul { rd, rs } => todo!(),
            Bic { rd, rs } => todo!(),
            Mvn { rd, rs } => todo!(),
            AddLowHi { rd, hs } => todo!(),
            AddHiLow { hd, rs } => todo!(),
            AddHiHi { hd, hs } => todo!(),
            CmpLowHi { rd, hs } => todo!(),
            CmpHiLow { hd, rs } => todo!(),
            CmpHiHi { hd, hs } => todo!(),
            MovLowHi { rd, hs } => todo!(),
            MovHiLow { hd, rs } => todo!(),
            MovHiHi { hd, hs } => todo!(),
            Bx { rs } => todo!(),
            BxHi { hs } => todo!(),
            LdrPc { rd, imm8 } => todo!(),
            StrReg { rb, ro, rd } => todo!(),
            StrbReg { rb, ro, rd } => todo!(),
            LdrReg { rb, ro, rd } => todo!(),
            LdrbReg { rb, ro, rd } => todo!(),
            StrhReg { rb, ro, rd } => todo!(),
            LdrhReg { rb, ro, rd } => todo!(),
            LdsbReg { rb, ro, rd } => todo!(),
            LdshReg { rb, ro, rd } => todo!(),
            StrImm { rb, imm5, rd } => todo!(),
            LdrImm { rb, imm5, rd } => todo!(),
            StrbImm { rb, imm5, rd } => todo!(),
            LdrbImm { rb, imm5, rd } => todo!(),
            StrhImm { rb, imm5, rd } => todo!(),
            LdrhImm { rb, imm5, rd } => todo!(),
            StrSpImm { imm8, rd } => todo!(),
            LdrSpImm { imm8, rd } => todo!(),
            AddPcImm { imm8, rd } => todo!(),
            AddSpImm { imm8, rd } => todo!(),
            AddSpPosImm { imm7 } => todo!(),
            AddSpNegImm { imm7 } => todo!(),
            Push { rlist } => todo!(),
            PushLr { rlist } => todo!(),
            Pop { rlist } => todo!(),
            PopPc { rlist } => todo!(),
            Stmia { rb, rlist } => todo!(),
            Ldmia { rb, rlist } => todo!(),
            Beq { soffset } => todo!(),
            Bne { soffset } => todo!(),
            Bcs { soffset } => todo!(),
            Bcc { soffset } => todo!(),
            Bmi { soffset } => todo!(),
            Bpl { soffset } => todo!(),
            Bvs { soffset } => todo!(),
            Bvc { soffset } => todo!(),
            Bhi { soffset } => todo!(),
            Bls { soffset } => todo!(),
            Bge { soffset } => todo!(),
            Blt { soffset } => todo!(),
            Bgt { soffset } => todo!(),
            Ble { soffset } => todo!(),
            Swi { imm } => todo!(),
            B { offset11 } => todo!(),
            BlHalf { hi, offset11 } => todo!(),
        }

        Ok(())
    }

    /// Decode and execute a single instruction
    pub fn next(&mut self) -> Result<(), ExecutionError> {
        // Decode the next instruction
        let instruction = self.decode()?;
        // Increase the program counter
        self.set_pc(self.get_pc() + 2);

        // Execute the instruction
        self.execute(instruction)
    }
}

impl<'rom> Display for Processor<'rom> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Flags: {}", self.alu)?;

        // Print the next register
        for (i, reg) in self.registers.iter().enumerate() {
            // Get the register name
            let name = REGISTER_NAMES[i];

            // Print the register name
            write!(f, "{:>3}: 0x{:08x} ", name, reg)?;

            // Print the register name
            if i % 4 == 3 {
                writeln!(f)?;
            }
        }

        write!(f, "\nNext instruction:\n    ")?;
        // Decode the next instruction
        match self.decode() {
            Ok(instruction) => {
                // Read the value at pc
                let value = self.memory.read_halfword(self.get_pc()).unwrap();
                writeln!(f, "{:04X}  {}", value, instruction)
            }

            Err(ExecutionError::MemoryError(err)) => {
                writeln!(f, "Memory error: {}", err)
            }
            Err(ExecutionError::InvalidInstruction) => writeln!(f, "Invalid instruction"),

            _ => writeln!(f, "Cannot fetch next instruction"),
        }
    }
}
