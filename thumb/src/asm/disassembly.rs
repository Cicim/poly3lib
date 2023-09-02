use colored::Colorize;
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

use crate::{asm::data::Register, utils::*, Instruction};

use super::data::{AssemblyInstruction, Label, Operand, Operator};

#[derive(Debug)]
pub enum DisassembledLine {
    Instruction {
        instruction: AssemblyInstruction,
        binary: Vec<u8>,
    },
    Label(Label),
    Word(u32),
}

impl DisassembledLine {
    /// Returns a string containing at most 4 bytes
    /// formatted in hexadecimal format, for example
    ///
    /// `12 34 AB CD`
    ///
    /// or
    ///
    /// `AB CD`
    ///
    /// or an empty string
    pub fn binary(&self) -> String {
        use DisassembledLine as L;

        match self {
            L::Instruction { binary, .. } => binary
                .iter()
                .map(|s| format!("{:02X}", s))
                .collect::<Vec<_>>()
                .join(" "),
            L::Word(binary) => {
                let binary = binary.to_le_bytes();

                format!(
                    "{:02X} {:02X} {:02X} {:02X}",
                    binary[0], binary[1], binary[2], binary[3]
                )
            }
            _ => String::new(),
        }
    }
}

#[derive(Default)]
struct DisassemblyOptions<'b> {
    colored_output: bool,
    show_offsets: bool,
    show_bytes: bool,
    recurse_over_function_calls: bool,

    /// Predefined labels (not created by the disassbely process)
    imported_labels: Option<&'b HashMap<u32, String>>,
}

#[derive(Default)]
pub struct Disassembler<'a, 'b> {
    /// The options for disassembly and output formatting
    options: DisassemblyOptions<'b>,
    /// The bytes to read the functions from
    bytes: &'a [u8],

    /// Functions left to visit
    functions_queue: Vec<u32>,
    /// Already visited functions
    visited_functions: HashSet<u32>,
    /// Disassembled data
    disassembled: HashMap<u32, DisassembledLine>,
    /// The map of labels to their offsets
    labels: HashMap<u32, String>,

    /// Offset you are currently visiting
    current_offset: u32,
    /// The current function's label
    current_function_label: String,
    /// The current function's branching sub-labels count
    current_function_branching_sub_count: u32,
}

impl<'a> Disassembler<'a, 'a> {
    /// Initializes the disassembler
    pub fn new(rom: &'a [u8]) -> Self {
        let options = DisassemblyOptions::default();
        Disassembler {
            options,
            bytes: rom,
            ..Default::default()
        }
    }

    /// Import labels for annotation
    pub fn import_labels(mut self, labels: &'a HashMap<u32, String>) -> Self {
        self.options.imported_labels = Some(labels);
        self
    }
    /// Allow recursion when encountering a function
    pub fn allow_recursion(mut self) -> Self {
        self.options.recurse_over_function_calls = true;
        self
    }

    /// Disassembles an entire function
    pub fn start_from_function(mut self, function: u32) -> Self {
        // Add the entry point to the queue
        self.functions_queue.push(function);

        // Disassemble all functions
        while let Some(function) = self.functions_queue.pop() {
            // Make sure the function has not already been visited
            if self.visited_functions.contains(&function) {
                continue;
            }

            // Disassemble the function
            self.do_function(function);
        }
        self
    }
    /// Disassembles `count` instructions starting from `offset`
    pub fn start_from(mut self, offset: u32, count: u32) -> Self {
        self.current_offset = offset;
        for _ in 0..count {
            self.do_instruction();
        }
        self
    }

    /// Shows the offsets for each instruction at the start of each line.
    pub fn show_offsets(mut self) -> Self {
        self.options.show_offsets = true;
        self
    }

    /// Print the output with all special options
    pub fn pretty_string(&mut self) -> String {
        self.options.colored_output = true;
        self.options.show_bytes = true;
        self.options.show_offsets = true;
        self.create_string()
    }

    // ANCHOR Disassembly
    /// Disassembles a function's instructions along with all its referenced data
    /// and its internal labels, then sets this function as visited and adds all
    /// visited functions to its queue.
    fn do_function(&mut self, function: u32) {
        // Add a label for this function
        self.current_function_label = match self.get_label(function) {
            Some(label) => label.clone(),
            None => {
                self.add_label(function, format!("sub_{:08x}", function));
                format!("{:08x}", function)
            }
        };
        self.current_offset = function;
        self.current_function_branching_sub_count = 0;

        // Parse instructions until the function terminates
        while !self.do_instruction() {}

        // Add this function to the list of disassembled ones
        self.visited_functions.insert(function);
    }

    /// Parses a single instruction at the current offset, stores it and updates
    /// the current offset. Returns true if this instruction terminates the
    /// function (e.g. a BX LR or a POP {PC}, etc.)
    fn do_instruction(&mut self) -> bool {
        // If this offset is a word, skip it
        if self.is_word(self.current_offset) {
            self.current_offset += 4;
            return false;
        }

        // Read the next two bytes
        let instruction_offset = self.current_offset;
        let two_bytes = self.read_two_bytes();
        let encoded_instruction = u16::from_le_bytes(two_bytes);
        let mut binary = two_bytes.to_vec();

        // Decode the instruction
        let instruction = Instruction::decode(encoded_instruction);
        if instruction.is_none() {
            // TODO Handle invalid instructions
            return true;
        }
        let instruction = instruction.unwrap();
        // Some instructions
        let mut terminates_function = false;

        // Macros for quicker instruction parsing
        macro_rules! reg {
            (sp) => {
                Register::SP
            };
            (pc) => {
                Register::PC
            };
            ($reg:ident) => {
                Register::from($reg).into()
            };
        }
        macro_rules! hireg {
            ($reg:ident) => {
                Register::from($reg + 8).into()
            };
        }
        macro_rules! imm {
            ($imm:ident) => {
                Operand::Immediate($imm)
            };
            ($imm:ident, $shift:literal) => {
                Operand::Immediate($imm << $shift)
            };
        }
        macro_rules! jump {
            ($name:literal, $offset:ident) => {{
                // Parse the actual signed offset from the instruction
                let signed_offset = extend_8bit_offset($offset);
                // The PC is always 4 bytes ahead of the current instruction due to prefetching
                let target = instruction_offset.wrapping_add(signed_offset as u32) + 4;

                AssemblyInstruction::BranchCond($name, self.get_jmp_label(target))
            }};
        }

        // Match the decoded instruction to obtain an `AssembledInstruction`
        use AssemblyInstruction as A;
        use Instruction as I;
        let instruction = match instruction {
            // Format 1
            I::LslImm { rd, rs, imm5 } => A::RdRsVal("lsl", reg!(rd), reg!(rs), imm!(imm5)),
            I::LsrImm { rd, rs, imm5 } => A::RdRsVal("lsr", reg!(rd), reg!(rs), imm!(imm5)),
            I::AsrImm { rd, rs, imm5 } => A::RdRsVal("asr", reg!(rd), reg!(rs), imm!(imm5)),
            // Format 2
            I::AddReg { rd, rs, rn } => A::RdRsVal("add", reg!(rd), reg!(rs), reg!(rn)),
            I::AddImm3 { rd, rs, imm3 } => A::RdRsVal("add", reg!(rd), reg!(rs), imm!(imm3)),
            I::SubReg { rd, rs, rn } => A::RdRsVal("sub", reg!(rd), reg!(rs), reg!(rn)),
            I::SubImm3 { rd, rs, imm3 } => A::RdRsVal("sub", reg!(rd), reg!(rs), imm!(imm3)),
            // Format 3
            I::MovImm { rd, imm8 } => A::RdVal("mov", reg!(rd), imm!(imm8)),
            I::CmpImm { rd, imm8 } => A::RdVal("cmp", reg!(rd), imm!(imm8)),
            I::AddImm8 { rd, imm8 } => A::RdVal("add", reg!(rd), imm!(imm8)),
            I::SubImm8 { rd, imm8 } => A::RdVal("sub", reg!(rd), imm!(imm8)),
            // Format 4
            I::And { rd, rs } => A::RdVal("and", reg!(rd), reg!(rs)),
            I::Eor { rd, rs } => A::RdVal("eor", reg!(rd), reg!(rs)),
            I::Lsl { rd, rs } => A::RdVal("lsl", reg!(rd), reg!(rs)),
            I::Lsr { rd, rs } => A::RdVal("lsr", reg!(rd), reg!(rs)),
            I::Asr { rd, rs } => A::RdVal("asr", reg!(rd), reg!(rs)),
            I::Adc { rd, rs } => A::RdVal("adc", reg!(rd), reg!(rs)),
            I::Sbc { rd, rs } => A::RdVal("sbc", reg!(rd), reg!(rs)),
            I::Ror { rd, rs } => A::RdVal("ror", reg!(rd), reg!(rs)),
            I::Tst { rd, rs } => A::RdVal("tst", reg!(rd), reg!(rs)),
            I::Neg { rd, rs } => A::RdVal("neg", reg!(rd), reg!(rs)),
            I::Cmp { rd, rs } => A::RdVal("cmp", reg!(rd), reg!(rs)),
            I::Cmn { rd, rs } => A::RdVal("cmn", reg!(rd), reg!(rs)),
            I::Orr { rd, rs } => A::RdVal("orr", reg!(rd), reg!(rs)),
            I::Mul { rd, rs } => A::RdVal("mul", reg!(rd), reg!(rs)),
            I::Bic { rd, rs } => A::RdVal("bic", reg!(rd), reg!(rs)),
            I::Mvn { rd, rs } => A::RdVal("mvn", reg!(rd), reg!(rs)),
            // Format 5
            I::AddLowHi { rd, hs } => A::RdVal("add", reg!(rd), hireg!(hs)),
            I::CmpLowHi { rd, hs } => A::RdVal("cmp", reg!(rd), hireg!(hs)),
            I::MovLowHi { rd, hs } => A::RdVal("mov", reg!(rd), hireg!(hs)),
            I::AddHiLow { hd, rs } => A::RdVal("add", hireg!(hd), reg!(rs)),
            I::MovHiLow { hd, rs } => A::RdVal("mov", hireg!(hd), reg!(rs)),
            I::CmpHiLow { hd, rs } => A::RdVal("cmp", hireg!(hd), reg!(rs)),
            I::AddHiHi { hd, hs } => A::RdVal("add", hireg!(hd), hireg!(hs)),
            I::CmpHiHi { hd, hs } => A::RdVal("cmp", hireg!(hd), hireg!(hs)),
            I::MovHiHi { hd, hs } => A::RdVal("mov", hireg!(hd), hireg!(hs)),
            I::Bx { rs } => {
                terminates_function = true;
                A::Val("bx", reg!(rs))
            }
            I::BxHi { hs } => {
                terminates_function = true;
                A::Val("bx", hireg!(hs))
            }
            // Format 6
            I::LdrPc { rd, imm8 } => {
                // Get the unsigned offset by which to increment the PC
                let unsigned_offset = (imm8 as u32) << 2;
                // Get the target by adding the offset to the PC (which is two steps (4 bytes)
                // ahead of the current instruction due to prefetching.
                let target = instruction_offset.wrapping_add(unsigned_offset + 4);
                // Since the read word must always be byte-aligned, force bit 1 to 0
                let target = target & !0b10;
                // Explore the given target
                self.do_word(target);

                A::LoadLabel(reg!(rd), self.get_dat_label(target))
            }
            // Format 7
            I::StrReg { rb, ro, rs: rd } => A::Mem("str", reg!(rd), reg!(rb), reg!(ro)),
            I::StrbReg { rb, ro, rs: rd } => A::Mem("strb", reg!(rd), reg!(rb), reg!(ro)),
            I::LdrReg { rb, ro, rd } => A::Mem("ldr", reg!(rd), reg!(rb), reg!(ro)),
            I::LdrbReg { rb, ro, rd } => A::Mem("ldrb", reg!(rd), reg!(rb), reg!(ro)),
            // Format 8
            I::StrhReg { rb, ro, rs: rd } => A::Mem("strh", reg!(rd), reg!(rb), reg!(ro)),
            I::LdrhReg { rb, ro, rd } => A::Mem("ldrh", reg!(rd), reg!(rb), reg!(ro)),
            I::LdsbReg { rb, ro, rd } => A::Mem("ldsb", reg!(rd), reg!(rb), reg!(ro)),
            I::LdshReg { rb, ro, rd } => A::Mem("ldsh", reg!(rd), reg!(rb), reg!(ro)),
            // Format 9
            I::StrImm { rb, imm5, rs: rd } => A::Mem("str", reg!(rd), reg!(rb), imm!(imm5, 2)),
            I::LdrImm { rb, imm5, rd } => A::Mem("ldr", reg!(rd), reg!(rb), imm!(imm5, 2)),
            I::StrbImm { rb, imm5, rs: rd } => A::Mem("strb", reg!(rd), reg!(rb), imm!(imm5)),
            I::LdrbImm { rb, imm5, rd } => A::Mem("ldrb", reg!(rd), reg!(rb), imm!(imm5)),
            // Format 10
            I::StrhImm { rb, imm5, rs: rd } => A::Mem("strh", reg!(rd), reg!(rb), imm!(imm5, 1)),
            I::LdrhImm { rb, imm5, rd } => A::Mem("ldrh", reg!(rd), reg!(rb), imm!(imm5, 1)),
            // Format 11
            I::StrSpImm { imm8, rs } => A::Mem("str", reg!(rs), reg!(sp), imm!(imm8, 2)),
            I::LdrSpImm { imm8, rd } => A::Mem("ldr", reg!(rd), reg!(sp), imm!(imm8, 2)),
            // Format 12
            I::AddPcImm { imm8, rd } => A::RdRsVal("add", reg!(rd), reg!(pc), imm!(imm8, 2)),
            I::AddSpImm { imm8, rd } => A::RdRsVal("add", reg!(rd), reg!(sp), imm!(imm8, 2)),
            // Format 13
            I::AddSpPosImm { imm7 } => A::RdVal("add", reg!(sp), imm!(imm7, 2)),
            I::AddSpNegImm { imm7 } => A::RdVal("sub", reg!(sp), imm!(imm7, 2)),
            // Format 14
            I::Push { rlist } => parse_push_or_pop("push", rlist, None),
            I::PushLr { rlist } => parse_push_or_pop("push", rlist, Some(Register::LR)),
            I::Pop { rlist } => parse_push_or_pop("pop", rlist, None),
            I::PopPc { rlist } => {
                // If something is popped onto pc, the control flow switches away
                terminates_function = true;
                parse_push_or_pop("pop", rlist, Some(Register::PC))
            }
            // Format 15
            I::Stmia { rb, rlist } => parse_stmia_or_ldmia("stdmia", rlist, reg!(rb)),
            I::Ldmia { rb, rlist } => parse_stmia_or_ldmia("ldmia", rlist, reg!(rb)),
            // Format 16
            I::Beq { soffset } => jump!("beq", soffset),
            I::Bne { soffset } => jump!("bne", soffset),
            I::Bcs { soffset } => jump!("bcs", soffset),
            I::Bcc { soffset } => jump!("bcc", soffset),
            I::Bmi { soffset } => jump!("bmi", soffset),
            I::Bpl { soffset } => jump!("bpl", soffset),
            I::Bvs { soffset } => jump!("bvs", soffset),
            I::Bvc { soffset } => jump!("bvc", soffset),
            I::Bhi { soffset } => jump!("bhi", soffset),
            I::Bls { soffset } => jump!("bls", soffset),
            I::Bge { soffset } => jump!("bge", soffset),
            I::Blt { soffset } => jump!("blt", soffset),
            I::Bgt { soffset } => jump!("bgt", soffset),
            I::Ble { soffset } => jump!("ble", soffset),
            // Format 17
            I::Swi { imm } => A::Val("swi", imm!(imm)),
            // Format 18
            I::B { offset11 } => {
                // Get the signed 12 bit offset to add to the target
                let signed_offset = extend_11bit_offset(offset11);
                let target = instruction_offset.wrapping_add(signed_offset as u32) + 2;

                A::BranchCond("b", self.get_jmp_label(target))
            }
            // Format 19
            I::BlHalf { hi, offset11 } => {
                // If we find the high instruction on its own, it's pretty likely it is invalid,
                // so we can skip this instruction altogether.
                if hi == true {
                    // TODO Handle invalid instructions
                    return true;
                }

                // Decode the next instruction
                let next_two_bytes = self.read_two_bytes();
                let next_opcode = u16::from_le_bytes(next_two_bytes);
                let next_offset11 = match Instruction::decode(next_opcode) {
                    // The only valid case is a BlHalf with hi set to true
                    Some(I::BlHalf { hi: true, offset11 }) => offset11,
                    // All other cases are invalid
                    _ => {
                        // TODO Handle invalid instructions
                        return true;
                    }
                } as u32;

                // Compose the signed difference
                let offset_bits = ((offset11 as u32) << 12) + (next_offset11 << 1);
                // Then convert make it actually signed
                let signed_offset = offset_bits as i32;
                let signed_offset = (signed_offset << 9) >> 9;

                // Since the instruction offset is still at the previous instruction we have to add 4
                let target = instruction_offset.wrapping_add(signed_offset as u32) + 4;

                // Add the new target to the functions in the queue
                if self.options.recurse_over_function_calls
                    && !self.visited_functions.contains(&target)
                {
                    self.functions_queue.push(target);
                }

                // Fill in the complete instruction bytes
                binary.extend(next_two_bytes);

                // TODO Get the correct function label
                let label = match self.get_label(target) {
                    Some(label) => label.clone(),
                    None => format!("sub_{:08x}", target),
                };

                A::BranchCond("bl", label)
            }
        };

        // Construct the line
        self.disassembled.insert(
            instruction_offset,
            DisassembledLine::Instruction {
                instruction,
                binary,
            },
        );

        terminates_function
    }

    /// Parse a word data and create a line for it
    fn do_word(&mut self, target: u32) {
        let byte1 = self.bytes[target as usize - 0x08_000_000] as u32;
        let byte2 = self.bytes[target as usize + 1 - 0x08_000_000] as u32;
        let byte3 = self.bytes[target as usize + 2 - 0x08_000_000] as u32;
        let byte4 = self.bytes[target as usize + 3 - 0x08_000_000] as u32;
        let word = byte1 | (byte2 << 8) | (byte3 << 16) | (byte4 << 24);

        let word = DisassembledLine::Word(word);
        self.disassembled.insert(target, word);
    }

    // ANCHOR Utilities
    /// Read the next two bytes
    fn read_two_bytes(&mut self) -> [u8; 2] {
        let lower = self.bytes[self.current_offset as usize - 0x08_000_000];
        let upper = self.bytes[self.current_offset as usize - 0x08_000_000 + 1];

        self.current_offset += 2;

        [lower, upper]
    }
    /// Obtain the label name at the given offset
    fn get_label(&'a self, offset: u32) -> Option<&'a String> {
        if let Some(imported) = self.options.imported_labels {
            if let Some(string) = imported.get(&offset) {
                return Some(string);
            }
        }

        self.labels.get(&offset)
    }
    /// Adds a label to the local labels
    fn add_label(&mut self, offset: u32, label: String) {
        self.labels.insert(offset, label);
    }
    /// Returns true if the given offset is already a parsed word
    fn is_word(&mut self, offset: u32) -> bool {
        matches!(
            self.disassembled.get(&offset),
            Some(DisassembledLine::Word(_))
        )
    }
    /// Get the label for the given dat_ reference
    fn get_dat_label(&mut self, target: u32) -> String {
        match self.get_label(target) {
            // If there is already a label with that target, return it
            Some(label) => label.clone(),
            // Otherwise, build a new one
            None => {
                let new_label = format!("dat_{:08x}", target);
                self.add_label(target, new_label.clone());
                new_label
            }
        }
    }
    /// Get the label for the given branch_reference
    fn get_jmp_label(&mut self, target: u32) -> String {
        match self.get_label(target) {
            // If there is already a label with that target, return it
            Some(label) => label.clone(),
            // Otherwise, build a new one
            None => {
                self.current_function_branching_sub_count += 1;
                let new_label = if self.current_function_label == "" {
                    format!("#0x{:08x}", target)
                } else {
                    format!(
                        "lab_{}.{}",
                        self.current_function_label, self.current_function_branching_sub_count
                    )
                };
                self.add_label(target, new_label.clone());
                new_label
            }
        }
    }

    // ANCHOR Printing
    pub fn create_string(&self) -> String {
        let mut res = String::new();
        match self.create_string_inner(&mut res) {
            Ok(_) => res,
            Err(_) => "Formatting error - should not happen".into(),
        }
    }

    fn create_string_inner(&self, f: &mut String) -> std::fmt::Result {
        // Get the keys of the hashmap and sort them
        let mut keys = self.disassembled.keys().collect::<Vec<_>>();
        keys.sort();

        // Iterate over the keys
        for offset in keys {
            // If there is a label to show at this place
            if let Some(label) = self.get_label(*offset) {
                self.print_line(f, offset, &DisassembledLine::Label(label.clone()))?;
            }

            // Get the disassembled line
            let line = self.disassembled.get(offset).unwrap();

            // Print the line
            self.print_line(f, offset, line)?;
        }

        Ok(())
    }

    fn print_line(
        &self,
        f: &mut String,
        offset: &u32,
        line: &DisassembledLine,
    ) -> std::fmt::Result {
        if let DisassembledLine::Label(label) = line {
            if !label.starts_with("lab_") && !label.starts_with("dat_") {
                write!(f, "\n\n")?;
            }
        }

        // Print the offset
        if self.options.show_offsets {
            if self.options.colored_output {
                write!(f, "{}  ", format!("{:08x}", offset).white())?;
            } else {
                write!(f, "{:08x}  ", offset)?;
            }
        }

        // Get the binary representation to print
        if self.options.show_bytes {
            let mut binary = format!("{:>12}", line.binary());
            if self.options.colored_output {
                binary = binary.yellow().to_string();
            }
            write!(f, "{}  ", binary)?;
        }

        match line {
            DisassembledLine::Label(label) => {
                self.print_label(f, label)?;
                write!(f, ":")?;
            }
            DisassembledLine::Instruction { instruction, .. } => {
                write!(f, "    ")?;
                self.print_instruction(f, instruction)?;
            }
            DisassembledLine::Word(word) => {
                write!(f, "    ")?;
                self.print_operator(f, ".word")?;

                if self.options.colored_output {
                    write!(f, "{}", format!("0x{:08x}", word).cyan())?;
                } else {
                    write!(f, "0x{:08x}", word)?;
                }
            }
        }

        // Print a newline
        writeln!(f)
    }

    fn print_instruction(&self, f: &mut String, i: &AssemblyInstruction) -> std::fmt::Result {
        use AssemblyInstruction as A;
        match i {
            A::Mem(op, rd, rb, off) => {
                self.print_operator(f, op)?;
                self.print_register(f, *rd)?;
                write!(f, ", [")?;
                self.print_register(f, *rb)?;
                write!(f, ", ")?;
                self.print_operand(f, off)?;
                write!(f, "]")
            }
            A::RdRsVal(op, rd, rs, val) => {
                self.print_operator(f, op)?;
                self.print_register(f, *rd)?;
                write!(f, ", ")?;
                self.print_register(f, *rs)?;
                write!(f, ", ")?;
                self.print_operand(f, val)
            }
            A::RdVal(op, rd, val) => {
                self.print_operator(f, op)?;
                self.print_register(f, *rd)?;
                write!(f, ", ")?;
                self.print_operand(f, val)
            }
            A::Val(op, val) => {
                self.print_operator(f, op)?;
                self.print_operand(f, val)
            }
            A::BranchCond(op, label) => {
                self.print_operator(f, op)?;
                self.print_label(f, label)
            }
            A::LoadLabel(rd, label) => {
                self.print_operator(f, "mov")?;
                self.print_register(f, *rd)?;
                write!(f, ", =(")?;
                self.print_label(f, label)?;
                write!(f, ")")
            }
            A::PushPop(op, regs) => {
                self.print_operator(f, op)?;
                write!(f, "{{ ")?;
                for (i, r) in regs.iter().enumerate() {
                    self.print_register(f, *r)?;
                    if i != regs.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, " }}")
            }
            A::StmLdmIA(op, r, regs) => {
                self.print_operator(f, op)?;
                self.print_register(f, *r)?;
                write!(f, "!, {{ ")?;
                for (i, r) in regs.iter().enumerate() {
                    self.print_register(f, *r)?;
                    if i != regs.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, " }}")
            }
        }
    }
    fn print_operator(&self, f: &mut String, op: Operator) -> std::fmt::Result {
        if self.options.colored_output {
            write!(f, "{:<8}", op.to_string().yellow())
        } else {
            write!(f, "{:<8}", op.to_string())
        }
    }
    fn print_register(&self, f: &mut String, reg: Register) -> std::fmt::Result {
        if self.options.colored_output {
            write!(f, "{}", reg.to_string().red())
        } else {
            write!(f, "{}", reg.to_string())
        }
    }
    fn print_operand(&self, f: &mut String, op: &Operand) -> std::fmt::Result {
        match op {
            Operand::Immediate(imm) => {
                if self.options.colored_output {
                    write!(f, "{}", format!("#{}", imm).cyan())
                } else {
                    write!(f, "#{}", imm)
                }
            }
            Operand::Register(reg) => self.print_register(f, *reg),
        }
    }
    fn print_label(&self, f: &mut String, label: &Label) -> std::fmt::Result {
        if self.options.colored_output {
            write!(f, "{}", label.green())
        } else {
            write!(f, "{}", label)
        }
    }
}

// ANCHOR Helpers
fn parse_push_or_pop(
    name: &'static str,
    rlist: u8,
    other: Option<Register>,
) -> AssemblyInstruction {
    let mut registers = get_registers_in_rlist(rlist)
        .iter()
        .map(|reg| (*reg).into())
        .collect::<Vec<_>>();

    if let Some(other) = other {
        registers.push(other);
    }

    AssemblyInstruction::PushPop(name, registers)
}

fn parse_stmia_or_ldmia(name: &'static str, rlist: u8, reg: Register) -> AssemblyInstruction {
    let registers = get_registers_in_rlist(rlist)
        .iter()
        .map(|reg| (*reg).into())
        .collect::<Vec<_>>();

    AssemblyInstruction::StmLdmIA(name, reg.into(), registers)
}
