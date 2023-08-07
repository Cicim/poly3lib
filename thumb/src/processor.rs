use std::{collections::HashMap, fmt::Display};

use thiserror::Error;

use crate::{
    alu::Alu,
    instructions::REGISTER_NAMES,
    memory::{Memory, MemoryError},
    utils::*,
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
    pub memory: Memory<'rom>,

    // Debugging interface
    /// Action to perform when the processor is about to call a function
    on_function_call: HashMap<u32, FunctionCallAction>,
    /// Action to perform when the given bank is accessed (first 8 bits in a 32-bit address)
    on_memory_access: HashMap<u8, MemoryAccessAction>,
    /// Whether to log all function calls
    log_all_functions: bool,

    /// Events that happened during execution
    pub logged_events: Vec<LoggedEvent>,
}

type ProcessorCallback = fn(&mut Processor) -> ExecutionResult;

pub enum FunctionCallAction {
    /// Skip over the function
    Skip,
    /// Run the given function before the instruction
    RunBefore(ProcessorCallback),
    /// Replace the instruction with the given function
    RunInstead(ProcessorCallback),

    /// Log the call, then run the function
    Log,
    /// Log the call, then skip over the function
    LogAndSkip,
    /// Log the call, then run the given function before the instruction
    LogAndRunBefore(ProcessorCallback),
    /// Log the call, then replace the instruction with the given function
    LogAndRunInstead(ProcessorCallback),
}

pub enum MemoryAccessAction {
    /// Log the read or written word
    Log,
    /// Log the read word
    LogRead,
    /// Log the written word
    LogWrite,
}

#[derive(Debug)]
pub enum LoggedEvent {
    /// A function with the given offset was called with the given arguments
    FunctionCalled(u32, [u32; 16]),
    /// A word was read from a memory location
    WordRead(u32, u32),
    /// A word was written to a memory location
    WordWritten(u32, u32),
    /// An halfword was read from a memory location
    HalfwordRead(u32, u16),
    /// An halfword was written to a memory location
    HalfwordWritten(u32, u16),
    /// A byte was read from a memory location
    ByteRead(u32, u8),
    /// A byte was written to a memory location
    ByteWritten(u32, u8),
}

impl Display for LoggedEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use LoggedEvent::*;

        match self {
            FunctionCalled(address, args) => {
                let mut args = args.iter().map(|x| format!("{:#010X}", x));
                let args = format!(
                    "{}",
                    (0..args.len())
                        .map(|x| format!("{}: {}", REGISTER_NAMES[x], args.next().unwrap()))
                        .collect::<Vec<_>>()
                        .join(", ")
                );
                write!(
                    f,
                    "Function called at {:#010X} with args: {}",
                    address, args
                )
            }
            WordRead(address, value) => {
                write!(f, "Read word {:#010X} from {:#010X}", value, address)
            }
            WordWritten(address, value) => {
                write!(f, "Written word {:#010X} to {:#010X}", value, address)
            }
            HalfwordRead(address, value) => {
                write!(f, "Read halfword {:#06X} from {:#010X}", value, address)
            }
            HalfwordWritten(address, value) => {
                write!(f, "Written halfword {:#06X} to {:#010X}", value, address)
            }
            ByteRead(address, value) => {
                write!(f, "Read byte {:#04X} from {:#010X}", value, address)
            }
            ByteWritten(address, value) => {
                write!(f, "Written byte {:#04X} to {:#010X}", value, address)
            }
        }
    }
}

#[derive(Debug, Error)]
pub enum ExecutionError {
    #[error("Memory error: {0}")]
    MemoryError(#[from] MemoryError),
    #[error("Invalid instruction {0:#06X}")]
    InvalidInstruction(u16),
    #[error("Jumping to ARM mode is not supported")]
    JumpToArm,

    // TODO Find a better way of stopping the processor
    //      without relying on an error
    #[error("Not an error: the processor terminated")]
    Terminated,
}

type ExecutionResult = Result<(), ExecutionError>;

impl<'rom> Processor<'rom> {
    /// Create a new processor
    pub fn new(rom: &'rom [u8]) -> Self {
        let mut cpu = Self {
            alu: Alu::new(),
            registers: [0; 16],
            memory: Memory::new(rom),

            on_function_call: HashMap::new(),
            on_memory_access: HashMap::new(),
            log_all_functions: false,

            logged_events: vec![],
        };

        cpu.set_sp(0x03_008_000);
        cpu.set_pc(0x08_000_000);
        // When the processor returns here, it terminates
        cpu.set_lr(0xFF_FFF_FFF);

        cpu
    }

    /// Runs a function until it ends
    pub fn run_function(&mut self, function: u32) -> ExecutionResult {
        self.set_lr(0xFFFF_FFFF);
        self.set_sp(0x03_008_000);
        self.set_pc(function & !1);

        self.run()
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

    // ANCHOR Callback configuration
    /// Add an action to perform when the processor is about to call a function
    pub fn add_function_call_action(mut self, address: u32, action: FunctionCallAction) -> Self {
        self.on_function_call.insert(address, action);
        self
    }
    /// Add an action to perform when a specific memory location is accessed
    pub fn add_memory_access_action(mut self, high: u8, action: MemoryAccessAction) -> Self {
        self.on_memory_access.insert(high, action);
        self
    }
    /// Log all function calls
    pub fn log_all_function_calls(mut self) -> Self {
        self.log_all_functions = true;
        self
    }

    // ANCHOR Control functions
    /// Fetches and decodes the next instruction
    pub fn decode(&self) -> Result<Instruction, ExecutionError> {
        // Read the program counter
        let pc = self.get_pc();

        // Fetch the instruction
        let data = self
            .memory
            .read_halfword(pc)
            .map_err(ExecutionError::MemoryError)?;

        // Decode the instruction
        Instruction::decode(data).ok_or(ExecutionError::InvalidInstruction(data))
    }

    /// Decode and execute a single instruction
    pub fn next(&mut self) -> ExecutionResult {
        // Decode the next instruction
        let instruction = self.decode()?;
        // Increase the program counter
        self.set_pc(self.get_pc() + 2);

        // Execute the instruction
        self.execute(instruction)
    }

    // ANCHOR Execute
    /// Executes an instruction
    pub fn execute(&mut self, instruction: Instruction) -> ExecutionResult {
        use Instruction::*;

        /// Alu operations with registers as second operand
        macro_rules! alu_op {
            // Performs rd <- ra OP rb
            ($rd:expr, $ra:expr, $op:ident, REG, $rb:expr) => {{
                let va = self.get_register($ra);
                let vb = self.get_register($rb);
                let result = self.alu.$op(va, vb);
                self.set_register($rd, result);
            }};

            // Performs rd <- ra OP imm
            ($rd:expr, $ra:expr, $op:ident, IMM, $imm:expr) => {{
                let ra = self.get_register($ra);
                let imm = $imm as u32;
                let result = self.alu.$op(ra, imm);
                self.set_register($rd, result);
            }};

            // Performs rd <- OP ra
            ($rd:expr, $op:ident, REG, $ra:expr) => {{
                let va = self.get_register($ra);
                let result = self.alu.$op(va);
                self.set_register($rd, result);
            }};
        }

        /// Update the condition codes with the given operation without
        /// setting the destination register.
        macro_rules! alu_cc {
            ($ra:expr, $op:ident, IMM, $imm:expr) => {{
                let ra = self.get_register($ra);
                let imm = $imm as u32;
                self.alu.$op(ra, imm);
            }};

            ($ra:expr, $op:ident, REG, $rb:expr) => {{
                let ra = self.get_register($ra);
                let rb = self.get_register($rb);
                self.alu.$op(ra, rb);
            }};
        }

        /// Does a conditional jump to the given offset if the condition is true.
        macro_rules! conditional_branch(
            ($cond:ident, $offset:ident) => {{
                if self.alu.$cond() {
                    let offset = extend_8bit_offset($offset);
                    let pc = self.get_pc();
                    self.set_pc(pc.wrapping_add(offset as u32 + 2));
                }
            }};
        );

        /// Log a writing operation if the address is configured to do so
        macro_rules! log_write {
            ($address:expr, $value:expr, $variant:ident) => {{
                match self.get_memory_address_action($address) {
                    Some(MemoryAccessAction::Log) => {
                        self.log(LoggedEvent::$variant($address, $value))
                    }
                    Some(MemoryAccessAction::LogWrite) => {
                        self.log(LoggedEvent::$variant($address, $value))
                    }
                    _ => {}
                }
            }};
        }
        /// Log a reading operation if the address is configured to do so
        macro_rules! log_read {
            ($address:expr, $value:expr, $variant:ident) => {{
                match self.get_memory_address_action($address) {
                    Some(MemoryAccessAction::Log) => {
                        self.log(LoggedEvent::$variant($address, $value))
                    }
                    Some(MemoryAccessAction::LogRead) => {
                        self.log(LoggedEvent::$variant($address, $value))
                    }
                    _ => {}
                }
            }};
        }

        match instruction {
            // Format 1 -- Move shifted register
            LslImm { rd, rs, imm5 } => alu_op!(rd, rs, lsl, IMM, imm5),
            LsrImm { rd, rs, imm5 } => alu_op!(rd, rs, lsr, IMM, imm5),
            AsrImm { rd, rs, imm5 } => alu_op!(rd, rs, asr, IMM, imm5),

            // Format 2 -- Add/subtract
            AddReg { rd, rs, rn } => alu_op!(rd, rs, add, REG, rn),
            AddImm3 { rd, rs, imm3 } => alu_op!(rd, rs, add, IMM, imm3),
            SubReg { rd, rs, rn } => alu_op!(rd, rs, sub, REG, rn),
            SubImm3 { rd, rs, imm3 } => alu_op!(rd, rs, sub, IMM, imm3),

            // Format 3 -- Move/compare/add/subtract immediate
            MovImm { rd, imm8 } => self.set_register(rd, imm8 as u32),
            CmpImm { rd, imm8 } => alu_cc!(rd, sub, IMM, imm8),
            AddImm8 { rd, imm8 } => alu_op!(rd, rd, add, IMM, imm8),
            SubImm8 { rd, imm8 } => alu_op!(rd, rd, sub, IMM, imm8),

            // Format 4 -- ALU operations
            And { rd, rs } => alu_op!(rd, rd, and, REG, rs),
            Eor { rd, rs } => alu_op!(rd, rd, xor, REG, rs),
            Lsl { rd, rs } => alu_op!(rd, rd, lsl, REG, rs),
            Lsr { rd, rs } => alu_op!(rd, rd, lsr, REG, rs),
            Asr { rd, rs } => alu_op!(rd, rd, asr, REG, rs),
            Adc { rd, rs } => alu_op!(rd, rd, adc, REG, rs),
            Sbc { rd, rs } => alu_op!(rd, rd, sbc, REG, rs),
            Ror { rd, rs } => alu_op!(rd, rd, ror, REG, rs),
            Tst { rd, rs } => alu_cc!(rd, and, REG, rs),
            Neg { rd, rs } => alu_op!(rd, neg, REG, rs),
            Cmp { rd, rs } => alu_cc!(rd, sub, REG, rs),
            Cmn { rd, rs } => alu_cc!(rd, add, REG, rs),
            Orr { rd, rs } => alu_op!(rd, rd, or, REG, rs),
            Mul { rd, rs } => alu_op!(rd, rd, mul, REG, rs),
            Bic { rd, rs } => alu_op!(rd, rd, bic, REG, rs),
            Mvn { rd, rs } => alu_op!(rd, not, REG, rs),

            // Format 5 - Hi register operations/branch exchange
            AddLowHi { rd, hs } => alu_op!(rd, rd, add, REG, hs + 8),
            AddHiLow { hd, rs } => alu_op!(hd + 8, hd + 8, add, REG, rs),
            AddHiHi { hd, hs } => alu_op!(hd + 8, hd + 8, add, REG, hs + 8),
            CmpLowHi { rd, hs } => alu_cc!(rd, sub, REG, hs + 8),
            CmpHiLow { hd, rs } => alu_cc!(hd + 8, sub, REG, rs),
            CmpHiHi { hd, hs } => alu_cc!(hd + 8, sub, REG, hs + 8),
            MovLowHi { rd, hs } => self.mov_registers(rd, hs + 8),
            MovHiLow { hd, rs } => self.mov_registers(hd + 8, rs),
            MovHiHi { hd, hs } => self.mov_registers(hd + 8, hs + 8),
            Bx { rs } => self.jump_to(self.get_register(rs))?,
            BxHi { hs } => self.jump_to(self.get_register(hs + 8))?,

            // Format 6 -- PC-relative load
            LdrPc { rd, imm8 } => {
                let offset = (imm8 as u32) << 2;
                let address = self.get_pc() + offset + 2;

                // Make sure the address is word-aligned
                let address = address & !0b10;

                let value = self.memory.read_word(address)?;
                self.set_register(rd, value);
            }

            // Format 7 -- Load/store with register offset
            StrReg { rb, ro, rs } => {
                let offset = self.get_register(ro);
                self.store_word(rs, rb, offset)?;
            }
            StrbReg { rb, ro, rs } => {
                let address = self.get_register(rb) + self.get_register(ro);
                let value = self.get_register(rs) as u8;
                log_write!(address, value, ByteWritten);
                self.memory.write_byte(address, value)?;
            }
            LdrReg { rb, ro, rd } => {
                let offset = self.get_register(ro);
                self.load_word(rd, rb, offset)?;
            }
            LdrbReg { rb, ro, rd } => {
                let address = self.get_register(rb) + self.get_register(ro);
                let value = self.memory.read_byte(address)?;
                log_read!(address, value, ByteRead);
                self.set_register(rd, value as u32);
            }

            // Format 8 -- Load/store sign-extended byte/halfword
            StrhReg { rb, ro, rs } => {
                let address = self.get_register(rb) + self.get_register(ro);
                let value = self.get_register(rs) as u16;
                log_write!(address, value, HalfwordWritten);
                self.memory.write_halfword(address, value)?;
            }
            LdrhReg { rb, ro, rd } => {
                let address = self.get_register(rb) + self.get_register(ro);
                let value = self.memory.read_halfword(address)?;
                log_read!(address, value, HalfwordRead);
                self.set_register(rd, value as u32);
            }
            LdsbReg { rb, ro, rd } => {
                let address = self.get_register(rb) + self.get_register(ro);
                let value = self.memory.read_byte(address)?;
                log_read!(address, value, ByteRead);
                let value = value as i8 as i32 as u32;
                self.set_register(rd, value);
            }
            LdshReg { rb, ro, rd } => {
                let address = self.get_register(rb) + self.get_register(ro);
                let value = self.memory.read_halfword(address)?;
                log_read!(address, value, HalfwordRead);
                let value = value as i16 as i32 as u32;
                self.set_register(rd, value);
            }

            // Format 9 -- Load/store with immediate offset
            StrImm { rb, imm5, rs } => {
                let offset = (imm5 as u32) << 2;
                self.store_word(rs, rb, offset)?;
            }
            LdrImm { rb, imm5, rd } => {
                let offset = (imm5 as u32) << 2;
                self.load_word(rd, rb, offset)?;
            }
            StrbImm { rb, imm5, rs } => {
                let address = self.get_register(rb) + (imm5 as u32);
                let value = self.get_register(rs) as u8;
                log_write!(address, value, ByteWritten);
                self.memory.write_byte(address, value)?;
            }
            LdrbImm { rb, imm5, rd } => {
                let address = self.get_register(rb) + (imm5 as u32);
                let value = self.memory.read_byte(address)?;
                log_read!(address, value, ByteRead);
                self.set_register(rd, value as u32);
            }

            // Format 10 -- Load/store halfword
            StrhImm { rb, imm5, rs } => {
                let address = self.get_register(rb) + ((imm5 as u32) << 1);
                let value = self.get_register(rs) as u16;
                log_write!(address, value, HalfwordWritten);
                self.memory.write_halfword(address, value)?;
            }
            LdrhImm { rb, imm5, rd } => {
                let address = self.get_register(rb) + ((imm5 as u32) << 1);
                let value = self.memory.read_halfword(address)?;
                log_read!(address, value, HalfwordRead);
                self.set_register(rd, value as u32);
            }

            // Format 11 -- SP-relative load/store
            StrSpImm { imm8, rs } => self.store_word(rs, 13, (imm8 as u32) << 2)?,
            LdrSpImm { imm8, rd } => self.load_word(rd, 13, (imm8 as u32) << 2)?,

            // Format 12 -- Load address
            // REVIEW - Is imm really a multiple of 4?
            AddPcImm { imm8, rd } => alu_op!(rd, 15, add, IMM, (imm8 as u32) << 2),
            AddSpImm { imm8, rd } => alu_op!(rd, 13, add, IMM, (imm8 as u32) << 2),

            // Format 13 -- Add offset to Stack Pointer
            AddSpPosImm { imm7 } => alu_op!(13, 13, add, IMM, (imm7 as u32) << 2),
            AddSpNegImm { imm7 } => alu_op!(13, 13, sub, IMM, (imm7 as u32) << 2),

            // Format 14 -- Push/pop registers
            Push { rlist } => get_registers_in_rlist(rlist)
                .iter()
                .map(|r| self.push_register(*r))
                .collect::<ExecutionResult>()?,
            PushLr { rlist } => {
                self.push_register(14)?;
                get_registers_in_rlist(rlist)
                    .iter()
                    .map(|r| self.push_register(*r))
                    .collect::<ExecutionResult>()?;
            }
            Pop { rlist } => get_registers_in_rlist(rlist)
                .iter()
                .rev()
                .map(|r| self.pop_register(*r))
                .collect::<ExecutionResult>()?,
            PopPc { rlist } => {
                get_registers_in_rlist(rlist)
                    .iter()
                    .rev()
                    .map(|r| self.pop_register(*r))
                    .collect::<ExecutionResult>()?;
                self.pop_register(15)?;
            }

            // Format 15 -- Multiple load/store
            Stmia { rb, rlist } => {
                let mut addr = self.get_register(rb);
                for r in get_registers_in_rlist(rlist) {
                    let value = self.get_register(r);
                    self.memory.write_word(addr, value)?;
                    addr += 4;
                }
                self.set_register(rb, addr)
            }
            Ldmia { rb, rlist } => {
                let mut addr = self.get_register(rb);
                for r in get_registers_in_rlist(rlist) {
                    let value = self.memory.read_word(addr)?;
                    self.set_register(r, value);
                    addr += 4;
                }
                self.set_register(rb, addr)
            }

            // Format 16 -- Conditional branch
            Beq { soffset } => conditional_branch!(is_eq, soffset),
            Bne { soffset } => conditional_branch!(is_ne, soffset),
            Bcs { soffset } => conditional_branch!(is_cs, soffset),
            Bcc { soffset } => conditional_branch!(is_cc, soffset),
            Bmi { soffset } => conditional_branch!(is_mi, soffset),
            Bpl { soffset } => conditional_branch!(is_pl, soffset),
            Bvs { soffset } => conditional_branch!(is_vs, soffset),
            Bvc { soffset } => conditional_branch!(is_vc, soffset),
            Bhi { soffset } => conditional_branch!(is_hi, soffset),
            Bls { soffset } => conditional_branch!(is_ls, soffset),
            Bge { soffset } => conditional_branch!(is_ge, soffset),
            Blt { soffset } => conditional_branch!(is_lt, soffset),
            Bgt { soffset } => conditional_branch!(is_gt, soffset),
            Ble { soffset } => conditional_branch!(is_le, soffset),

            // Format 17 -- Software interrupt
            Swi { imm } => println!(
                "[WARNING] SWI #{} was called, but it is not yet implemented",
                imm
            ),

            // Format 18 -- Unconditional branch
            B { offset11 } => {
                let offset = extend_11bit_offset(offset11);
                self.set_pc(self.get_pc().wrapping_add(offset as u32));
            }

            // Format 19 -- Long branch with link
            BlHalf {
                hi: false,
                offset11,
            } => {
                // First part
                let top = (offset11 as u32) << 12;
                self.set_lr(top);
                // Run the second part immediately
                self.next()?;
            }
            BlHalf { hi: true, offset11 } => {
                // Last part
                // The offset11 contains the lower 11 bits of the target address
                let bottom = (offset11 as u32) << 1;
                let jump_to = bottom + self.get_lr();
                // Make the offset into a signed offset
                let jump_to = ((jump_to as i32) << 9) >> 9;
                // Combine into the new offset
                let jump_to = (jump_to as u32).wrapping_add(self.get_pc()) & !1;

                // Get the offset to the instruction right after this one
                let jump_back = self.get_pc() & !1;

                self.set_lr(jump_back + 1);
                self.call_function(jump_to + 1)?;
            }
        }

        Ok(())
    }

    /// Run until the pc reaches 0xFFFF_FFFF
    pub fn run(&mut self) -> ExecutionResult {
        loop {
            if self.get_pc() == 0xFFFF_FFFF {
                return Ok(());
            }
            self.next()?;
        }
    }

    // ANCHOR Memory access
    /// Stores the word in `rs` at the memory location referenced
    /// by the contents of `rs` + `offset`.
    fn store_word(&mut self, rs: u8, rb: u8, offset: u32) -> Result<(), ExecutionError> {
        // Get the address to store the word at
        let address = self.get_register(rb) + offset;
        // Get the word to store
        let word = self.get_register(rs);

        // Store the word
        self.memory.write_word(address, word)?;

        // Log it if needed
        match self.get_memory_address_action(address) {
            Some(MemoryAccessAction::Log) | Some(MemoryAccessAction::LogWrite) => {
                self.log(LoggedEvent::WordWritten(address, word))
            }
            _ => {}
        }

        Ok(())
    }

    /// Loads a word from the memory location referenced by the contents
    /// of `rb` + `offset` into `rd`.
    fn load_word(&mut self, rd: u8, rb: u8, offset: u32) -> ExecutionResult {
        // Get the address to load the word from
        let address = self.get_register(rb) + offset;
        // Get the value
        let value = self.memory.read_word(address)?;
        // Store it
        self.set_register(rd, value);

        // Log it if needed
        match self.get_memory_address_action(address) {
            Some(MemoryAccessAction::Log) | Some(MemoryAccessAction::LogRead) => {
                self.log(LoggedEvent::WordRead(address, value))
            }
            _ => {}
        }

        Ok(())
    }

    /// Get what to do when the given memory address is accessed
    fn get_memory_address_action(&self, addr: u32) -> Option<&MemoryAccessAction> {
        let high = (addr >> 24) as u8;

        if let Some(action) = self.on_memory_access.get(&high) {
            Some(action)
        } else {
            None
        }
    }

    // ANCHOR Instruction helpers
    /// Copies the contents of register `rs` to those of register `rd`
    fn mov_registers(&mut self, rd: u8, rs: u8) {
        let content = self.get_register(rs);
        self.set_register(rd, content);
    }

    /// Pushes a single register on the stack
    fn push_register(&mut self, rs: u8) -> ExecutionResult {
        // Move the stack pointer
        self.set_sp(self.get_sp() - 4);

        // Store the word
        self.store_word(rs, 13, 0)
    }

    /// Pops from stack into a register
    fn pop_register(&mut self, rd: u8) -> ExecutionResult {
        // Read the value on the stack
        self.load_word(rd, 13, 0)?;
        // Update the stack pointer
        self.set_sp(self.get_sp() + 4);

        Ok(())
    }

    /// Updates PC so that the next instruction starts at the given value
    fn jump_to(&mut self, address: u32) -> ExecutionResult {
        // Check if the program has to terminate
        if address == 0xFF_FFF_FFF {
            return Err(ExecutionError::Terminated);
        }
        // Make sure the address has bit 0 set to 1 (Thumb)
        if address & 1 == 0 {
            return Err(ExecutionError::JumpToArm);
        }
        // Clear the last bit since PC is always halfword aligned
        let address = address & !1;

        // Jump to the function
        self.set_pc(address);

        Ok(())
    }

    /// Updates PC so that the next instruction starts at the given value
    fn call_function(&mut self, address: u32) -> ExecutionResult {
        // Check if the program has to terminate
        if address == 0xFF_FFF_FFF {
            return Err(ExecutionError::Terminated);
        }
        // Make sure the address has bit 0 set to 1 (Thumb)
        if address & 1 == 0 {
            return Err(ExecutionError::JumpToArm);
        }
        // Clear the last bit since PC is always halfword aligned
        let address = address & !1;

        // If there is something to do before the function call
        if let Some(action) = self.on_function_call.get(&address) {
            use FunctionCallAction::*;
            match action {
                // Don't do anything
                Skip => {}

                RunBefore(cb) => {
                    // Run the callback
                    cb(self)?;
                    // Then jump to the function
                    self.set_pc(address);
                }

                RunInstead(cb) => {
                    // Run the callback
                    cb(self)?;
                    // Don't jump to the function
                }

                // Log then jump to the function
                Log => {
                    // Log the function call
                    self.log_function_call(address);
                    // Jump to the function
                    self.set_pc(address);
                }

                // Log then don't do anything
                LogAndSkip => self.log_function_call(address),

                LogAndRunBefore(cb) => {
                    let cb = *cb;
                    // Log the function call
                    self.log_function_call(address);
                    // Run the callback
                    cb(self)?;
                    // Then jump to the function
                    self.set_pc(address);
                }
                LogAndRunInstead(cb) => {
                    let cb = *cb;
                    // Log the function call
                    self.log_function_call(address);
                    // Run the callback
                    cb(self)?;
                    // Don't jump to the function
                }
            }
        } else {
            if self.log_all_functions {
                self.log_function_call(address);
            }

            // Jump to the function
            self.set_pc(address);
        }

        Ok(())
    }

    // ANCHOR Logging
    /// Logs an event
    fn log(&mut self, event: LoggedEvent) {
        self.logged_events.push(event);
    }
    /// Logs a function call
    fn log_function_call(&mut self, addr: u32) {
        // Clone the registers
        let registers = self.registers.clone();
        self.log(LoggedEvent::FunctionCalled(addr, registers));
    }
}

impl<'rom> Display for Processor<'rom> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use colored::Colorize;
        writeln!(f, "Flags: {}", self.alu)?;

        // Print the next register
        for (i, reg) in self.registers.iter().enumerate() {
            // Get the register name
            let name = REGISTER_NAMES[i].red();
            let value = format!("0x{:08x}", reg).yellow();

            // Print the register name
            write!(f, "{:>3}: {} ", name, value)?;

            // Print the register name
            if i % 4 == 3 {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}
