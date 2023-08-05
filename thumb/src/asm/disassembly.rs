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
        use DisassembledLine::*;

        match self {
            Instruction { binary, .. } => binary
                .iter()
                .map(|s| format!("{:02X}", s))
                .collect::<Vec<_>>()
                .join(" "),
            Word(binary) => {
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

pub struct DisassemblyOptions<'b> {
    colored_output: bool,
    show_offsets: bool,
    show_bytes: bool,
    recurse_over_function_calls: bool,

    /// Predefined labels (not created by the disassbely process)
    imported_labels: Option<&'b HashMap<u32, String>>,
    /// Entry point for disassembly (we assume it is a function)
    entry_point: u32,
}

impl<'b> DisassemblyOptions<'b> {
    pub fn new(entry_point: u32, imported_labels: Option<&'b HashMap<u32, String>>) -> Self {
        DisassemblyOptions {
            colored_output: true,
            show_offsets: true,
            show_bytes: true,
            recurse_over_function_calls: true,
            imported_labels,
            entry_point,
        }
    }
}

struct DisassemblyState<'a, 'b> {
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
}

/// Disassembles a function with the given options
pub fn disassemble<'a, 'b>(rom: &'a [u8], options: DisassemblyOptions<'b>) -> String {
    let mut state = DisassemblyState {
        options,
        bytes: rom,
        functions_queue: vec![],
        visited_functions: HashSet::new(),
        disassembled: HashMap::new(),
        labels: HashMap::new(),
    };

    // Add the entry point to the queue
    state.functions_queue.push(state.options.entry_point);

    // Disassemble all functions
    while let Some(function) = state.functions_queue.pop() {
        // Make sure the function has not already been visited
        if state.visited_functions.contains(&function) {
            continue;
        }

        // Disassemble the function
        state.disassemble_function(function);
    }

    state.create_string()
}

impl<'a> DisassemblyState<'a, 'a> {
    // ANCHOR Disassembly
    /// Disassembles a function's instructions along with all its referenced data
    /// and its internal labels, then sets this function as visited and adds all
    /// visited functions to its queue.
    fn disassemble_function(&mut self, function: u32) {
        // Add a label for this function
        let label = match self.get_label(function) {
            Some(label) => format!("FUN_{}", label),
            None => format!("FUN_{:08x}", function),
        };
        self.add_label(function, label.clone());

        let mut sublabels: HashMap<u32, String> = HashMap::new();

        let mut offset = function;
        let mut bl_half = 0;
        let mut finished = false;
        // Until you detect the end of the function
        while !finished {
            if self.is_word(offset) {
                offset += 4;
                continue;
            }

            macro_rules! conditional_jump {
                ($name:literal, $offset:ident) => {{
                    let diff = extend_8bit_offset($offset);
                    let target = offset.wrapping_add(diff as u32) + 2;

                    // Create a label if it does not already exist
                    let label = if let Some(label) = self.get_label(target) {
                        label.clone()
                    } else {
                        let count = sublabels.len() + 1;
                        let new_label = format!("{}.{}", label, count);
                        sublabels.insert(target, new_label.clone());
                        self.add_label(target, new_label.clone());
                        new_label
                    };

                    AssemblyInstruction::BranchCond($name, label)
                }};
            }

            use AssemblyInstruction::*;
            use Instruction::*;
            // Try to decode the instruction at the current offset
            let lower = self.bytes[offset as usize - 0x08_000_000] as u16;
            let upper = self.bytes[offset as usize + 1 - 0x08_000_000] as u16;
            let halfword = lower | (upper << 8);

            let instruction = match Instruction::decode(halfword) {
                Some(instruction) => match instruction {
                    // Format 1
                    LslImm { rd, rs, imm5 } => rdrsimm("lsl", rd, rs, imm5),
                    LsrImm { rd, rs, imm5 } => rdrsimm("lsr", rd, rs, imm5),
                    AsrImm { rd, rs, imm5 } => rdrsimm("asr", rd, rs, imm5),

                    // Format 2
                    AddReg { rd, rs, rn } => rdrsrn("add", rd, rs, rn),
                    AddImm3 { rd, rs, imm3 } => rdrsimm("add", rd, rs, imm3),
                    SubReg { rd, rs, rn } => rdrsrn("sub", rd, rs, rn),
                    SubImm3 { rd, rs, imm3 } => rdrsimm("sub", rd, rs, imm3),

                    // Format 3
                    MovImm { rd, imm8 } => rdimm("mov", rd, imm8),
                    CmpImm { rd, imm8 } => rdimm("cmp", rd, imm8),
                    AddImm8 { rd, imm8 } => rdimm("add", rd, imm8),
                    SubImm8 { rd, imm8 } => rdimm("sub", rd, imm8),

                    // Format 4
                    And { rd, rs } => rdrs("and", rd, rs),
                    Eor { rd, rs } => rdrs("eor", rd, rs),
                    Lsl { rd, rs } => rdrs("lsl", rd, rs),
                    Lsr { rd, rs } => rdrs("lsr", rd, rs),
                    Asr { rd, rs } => rdrs("asr", rd, rs),
                    Adc { rd, rs } => rdrs("adc", rd, rs),
                    Sbc { rd, rs } => rdrs("sbc", rd, rs),
                    Ror { rd, rs } => rdrs("ror", rd, rs),
                    Tst { rd, rs } => rdrs("tst", rd, rs),
                    Neg { rd, rs } => rdrs("neg", rd, rs),
                    Cmp { rd, rs } => rdrs("cmp", rd, rs),
                    Cmn { rd, rs } => rdrs("cmn", rd, rs),
                    Orr { rd, rs } => rdrs("orr", rd, rs),
                    Mul { rd, rs } => rdrs("mul", rd, rs),
                    Bic { rd, rs } => rdrs("bic", rd, rs),
                    Mvn { rd, rs } => rdrs("mvn", rd, rs),

                    // Format 5
                    AddLowHi { rd, hs } => rdrs("add", rd, hs + 8),
                    AddHiLow { hd, rs } => rdrs("add", hd + 8, rs),
                    AddHiHi { hd, hs } => rdrs("add", hd + 8, hs + 8),
                    CmpLowHi { rd, hs } => rdrs("cmp", rd, hs + 8),
                    CmpHiLow { hd, rs } => rdrs("cmp", hd + 8, rs),
                    CmpHiHi { hd, hs } => rdrs("cmp", hd + 8, hs + 8),
                    MovLowHi { rd, hs } => rdrs("mov", rd, hs + 8),
                    MovHiLow { hd, rs } => rdrs("mov", hd + 8, rs),
                    MovHiHi { hd, hs } => rdrs("mov", hd + 8, hs + 8),
                    Bx { rs } => {
                        finished = true;
                        Val("bx", Operand::Register(rs.into()))
                    }
                    BxHi { hs } => {
                        finished = true;
                        Val("bx", Operand::Register((hs + 8).into()))
                    }

                    // Format 6
                    LdrPc { rd, imm8 } => {
                        let off = (imm8 as u32) << 2;
                        println!("{}", imm8);

                        let target = (offset + off + 4) & 0xFFFF_FFFC;

                        let label = if let Some(label) = self.get_label(target) {
                            label.clone()
                        } else {
                            let count = sublabels.len() + 1;
                            let new_label = format!("{}.{}", label, count);
                            sublabels.insert(target, new_label.clone());
                            self.add_label(target, new_label.clone());
                            new_label
                        };

                        // Explore the given target
                        let byte1 = self.bytes[target as usize - 0x08_000_000] as u32;
                        let byte2 = self.bytes[target as usize + 1 - 0x08_000_000] as u32;
                        let byte3 = self.bytes[target as usize + 2 - 0x08_000_000] as u32;
                        let byte4 = self.bytes[target as usize + 3 - 0x08_000_000] as u32;
                        let word = byte1 | (byte2 << 8) | (byte3 << 16) | (byte4 << 24);

                        let word = DisassembledLine::Word(word);
                        self.disassembled.insert(target, word);

                        AssemblyInstruction::LoadLabel(rd.into(), label)
                    }

                    // Format 7
                    StrReg { rb, ro, rd } => memory_ro("str", rd, rb, ro),
                    StrbReg { rb, ro, rd } => memory_ro("strb", rd, rb, ro),
                    LdrReg { rb, ro, rd } => memory_ro("ldr", rd, rb, ro),
                    LdrbReg { rb, ro, rd } => memory_ro("ldrb", rd, rb, ro),

                    // Format 8
                    StrhReg { rb, ro, rd } => memory_ro("strh", rd, rb, ro),
                    LdrhReg { rb, ro, rd } => memory_ro("ldrh", rd, rb, ro),
                    LdsbReg { rb, ro, rd } => memory_ro("ldsb", rd, rb, ro),
                    LdshReg { rb, ro, rd } => memory_ro("ldsh", rd, rb, ro),

                    // Format 9
                    StrImm { rb, imm5, rd } => memory_imm("str", rd, rb, (imm5 as u32) << 2),
                    LdrImm { rb, imm5, rd } => memory_imm("ldr", rd, rb, (imm5 as u32) << 2),
                    StrbImm { rb, imm5, rd } => memory_imm("strb", rd, rb, imm5 as u32),
                    LdrbImm { rb, imm5, rd } => memory_imm("ldrb", rd, rb, imm5 as u32),

                    // Format 10
                    StrhImm { rb, imm5, rd } => memory_imm("strh", rd, rb, (imm5 as u32) << 1),
                    LdrhImm { rb, imm5, rd } => memory_imm("ldrh", rd, rb, (imm5 as u32) << 1),

                    // Format 11
                    StrSpImm { imm8, rs } => memory_imm("str", rs, 13, (imm8 as u32) << 2),
                    LdrSpImm { imm8, rd } => memory_imm("ldr", rd, 13, (imm8 as u32) << 2),

                    // Format 12
                    AddPcImm { imm8, rd } => rdrsimm("add", rd, 13, imm8 << 2),
                    AddSpImm { imm8, rd } => rdrsimm("add", rd, 15, imm8 << 2),

                    // Format 13
                    AddSpPosImm { imm7 } => rdimm("add", 13, imm7 << 2),
                    AddSpNegImm { imm7 } => rdimm("sub", 13, imm7 << 2),

                    // Format 14
                    Push { rlist } => push_pop("push", rlist, None),
                    PushLr { rlist } => push_pop("push", rlist, Some(Register::LR)),
                    Pop { rlist } => push_pop("pop", rlist, None),
                    PopPc { rlist } => push_pop("pop", rlist, Some(Register::PC)),

                    // Format 15
                    Stmia { rb, rlist } => stmldmia("stdmia", rlist, rb.into()),
                    Ldmia { rb, rlist } => stmldmia("ldmia", rlist, rb.into()),

                    // Format 16
                    Beq { soffset } => conditional_jump!("beq", soffset),
                    Bne { soffset } => conditional_jump!("bne", soffset),
                    Bcs { soffset } => conditional_jump!("bcs", soffset),
                    Bcc { soffset } => conditional_jump!("bcc", soffset),
                    Bmi { soffset } => conditional_jump!("bmi", soffset),
                    Bpl { soffset } => conditional_jump!("bpl", soffset),
                    Bvs { soffset } => conditional_jump!("bvs", soffset),
                    Bvc { soffset } => conditional_jump!("bvc", soffset),
                    Bhi { soffset } => conditional_jump!("bhi", soffset),
                    Bls { soffset } => conditional_jump!("bls", soffset),
                    Bge { soffset } => conditional_jump!("bge", soffset),
                    Blt { soffset } => conditional_jump!("blt", soffset),
                    Bgt { soffset } => conditional_jump!("bgt", soffset),
                    Ble { soffset } => conditional_jump!("ble", soffset),

                    // Format 17
                    Swi { imm } => AssemblyInstruction::Val("swi", Operand::Immediate(imm as u32)),

                    // Format 18
                    B { offset11 } => {
                        let diff = extend_11bit_offset(offset11);
                        let target = offset.wrapping_add(diff as u32) + 2;

                        // Create a label if it does not already exist
                        let label = if let Some(label) = self.get_label(target) {
                            label.clone()
                        } else {
                            let count = sublabels.len() + 1;
                            let new_label = format!("{}.{}", label, count);
                            sublabels.insert(target, new_label.clone());
                            self.add_label(target, new_label.clone());
                            new_label
                        };

                        AssemblyInstruction::BranchCond("b", label)
                    }

                    BlHalf { hi, offset11 } => {
                        AssemblyInstruction::Val("bl", Operand::Immediate(6969))
                    }
                },
                None => break,
            };

            // Add a decoded instruction to the given offset
            self.disassembled.insert(
                offset,
                DisassembledLine::Instruction {
                    instruction,
                    binary: vec![lower as u8, upper as u8],
                },
            );

            offset += 2;
        }

        // Add this function to the list of disassembled ones
        self.visited_functions.insert(function);
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

    // ANCHOR Printing
    fn create_string(&self) -> String {
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
                if self.options.colored_output {
                    write!(f, "{}", ".word ".yellow())?;
                } else {
                    write!(f, "{}", ".word ")?;
                }

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
        use AssemblyInstruction::*;

        match i {
            Nop => write!(f, "{:?}", i),
            MemoryOperation(op, rd, rb, off) => {
                self.print_operator(f, op)?;
                write!(f, " ")?;
                self.print_register(f, *rd)?;
                write!(f, ", [")?;
                self.print_register(f, *rb)?;
                write!(f, ", ")?;
                self.print_operand(f, off)?;
                write!(f, "]")
            }
            RdRsVal(op, rd, rs, val) => {
                self.print_operator(f, op)?;
                write!(f, " ")?;
                self.print_register(f, *rd)?;
                write!(f, ", ")?;
                self.print_register(f, *rs)?;
                write!(f, ", ")?;
                self.print_operand(f, val)
            }
            RdVal(op, rd, val) => {
                self.print_operator(f, op)?;
                write!(f, " ")?;
                self.print_register(f, *rd)?;
                write!(f, ", ")?;
                self.print_operand(f, val)
            }
            Val(op, val) => {
                self.print_operator(f, op)?;
                write!(f, " ")?;
                self.print_operand(f, val)
            }
            BranchCond(op, label) => {
                self.print_operator(f, op)?;
                write!(f, " ")?;
                self.print_label(f, label)
            }
            LoadLabel(rd, label) => {
                self.print_operator(f, "mov ")?;
                self.print_register(f, *rd)?;
                write!(f, ", =(")?;
                self.print_label(f, label)?;
                write!(f, ")")
            }
            PushPop(op, regs) => {
                self.print_operator(f, op)?;
                write!(f, " {{ ")?;
                for (i, r) in regs.iter().enumerate() {
                    self.print_register(f, *r)?;
                    if i != regs.len() - 1 {
                        write!(f, ", ")?;
                    }
                }

                write!(f, " }}")
            }
            StmLdmIA(_, _, _) => write!(f, "{:?}", i),
        }
    }
    fn print_operator(&self, f: &mut String, op: Operator) -> std::fmt::Result {
        if self.options.colored_output {
            write!(f, "{}", op.to_string().yellow())
        } else {
            write!(f, "{}", op.to_string())
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
fn rdrsimm(name: &'static str, rd: u8, rs: u8, imm: u8) -> AssemblyInstruction {
    AssemblyInstruction::RdRsVal(name, rd.into(), rs.into(), Operand::Immediate(imm as u32))
}
fn rdrsrn(name: &'static str, rd: u8, rs: u8, rn: u8) -> AssemblyInstruction {
    AssemblyInstruction::RdRsVal(name, rd.into(), rs.into(), Operand::Register(rn.into()))
}
fn rdimm(name: &'static str, rd: u8, imm: u8) -> AssemblyInstruction {
    AssemblyInstruction::RdVal(name, rd.into(), Operand::Immediate(imm as u32))
}
fn rdrs(name: &'static str, rd: u8, rs: u8) -> AssemblyInstruction {
    AssemblyInstruction::RdVal(name, rd.into(), Operand::Register(rs.into()))
}
fn memory_imm(name: &'static str, rd: u8, rb: u8, offset: u32) -> AssemblyInstruction {
    AssemblyInstruction::MemoryOperation(name, rd.into(), rb.into(), Operand::Immediate(offset))
}
fn memory_ro(name: &'static str, rd: u8, rb: u8, ro: u8) -> AssemblyInstruction {
    AssemblyInstruction::MemoryOperation(name, rd.into(), rb.into(), Operand::Register(ro.into()))
}

fn push_pop(name: &'static str, rlist: u8, other: Option<Register>) -> AssemblyInstruction {
    let mut registers = get_registers_in_rlist(rlist)
        .iter()
        .map(|reg| (*reg).into())
        .collect::<Vec<_>>();

    if let Some(other) = other {
        registers.push(other);
    }

    AssemblyInstruction::PushPop(name, registers)
}

fn stmldmia(name: &'static str, rlist: u8, reg: Register) -> AssemblyInstruction {
    let registers = get_registers_in_rlist(rlist)
        .iter()
        .map(|reg| (*reg).into())
        .collect::<Vec<_>>();

    AssemblyInstruction::StmLdmIA(name, reg.into(), registers)
}
