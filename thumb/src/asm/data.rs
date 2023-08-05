use std::fmt::Display;

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    SP,
    LR,
    PC,
}

impl From<u8> for Register {
    fn from(val: u8) -> Self {
        match val {
            0 => Register::R0,
            1 => Register::R1,
            2 => Register::R2,
            3 => Register::R3,
            4 => Register::R4,
            5 => Register::R5,
            6 => Register::R6,
            7 => Register::R7,
            8 => Register::R8,
            9 => Register::R9,
            10 => Register::R10,
            11 => Register::R11,
            12 => Register::R12,
            13 => Register::SP,
            14 => Register::LR,
            15 => Register::PC,
            _ => panic!("Invalid register"),
        }
    }
}
impl From<Register> for u8 {
    fn from(val: Register) -> Self {
        match val {
            Register::R0 => 0,
            Register::R1 => 1,
            Register::R2 => 2,
            Register::R3 => 3,
            Register::R4 => 4,
            Register::R5 => 5,
            Register::R6 => 6,
            Register::R7 => 7,
            Register::R8 => 8,
            Register::R9 => 9,
            Register::R10 => 10,
            Register::R11 => 11,
            Register::R12 => 12,
            Register::SP => 13,
            Register::LR => 14,
            Register::PC => 15,
        }
    }
}

impl Register {
    /// Returns `true` if the register is low (r0-r7)
    pub fn is_low(&self) -> bool {
        let as_u8: u8 = (*self).into();
        return as_u8 <= 7;
    }

    /// Returns `true` if the register is high (r8-r15)
    pub fn is_high(&self) -> bool {
        !self.is_low()
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Register::R0 => write!(f, "r0"),
            Register::R1 => write!(f, "r1"),
            Register::R2 => write!(f, "r2"),
            Register::R3 => write!(f, "r3"),
            Register::R4 => write!(f, "r4"),
            Register::R5 => write!(f, "r5"),
            Register::R6 => write!(f, "r6"),
            Register::R7 => write!(f, "r7"),
            Register::R8 => write!(f, "r8"),
            Register::R9 => write!(f, "r9"),
            Register::R10 => write!(f, "r10"),
            Register::R11 => write!(f, "r11"),
            Register::R12 => write!(f, "r12"),
            Register::SP => write!(f, "sp"),
            Register::LR => write!(f, "lr"),
            Register::PC => write!(f, "pc"),
        }
    }
}

#[derive(Debug)]
pub enum Operand {
    Register(Register),
    Immediate(u32),
}

#[derive(Debug)]
pub enum MemoryOperand {
    Register(Register),
    Immediate(u8),
    Label(Label),
}

pub type Operator = &'static str;
pub type Label = String;

#[derive(Debug)]
/// Instructions as seen by the final user
pub enum AssemblyInstruction {
    /// An assembly instruction that compiles to
    /// ```
    /// LSL r0, r0, #0
    /// ```
    Nop,

    /// ```
    /// OP Rd, [Rb, Ro]
    /// OP Rd, [Rb, #offset]
    /// ```
    MemoryOperation(Operator, Register, Register, Operand),

    /// ```
    /// OP Rd, Rs, Rn
    /// OP Rd, Rs, #immediate
    /// ```
    RdRsVal(Operator, Register, Register, Operand),

    /// ```
    /// OP Rd, Rs
    /// OP Rd, #immediate
    /// ```
    RdVal(Operator, Register, Operand),

    /// ```
    /// OP Rs
    /// OP #immediate
    /// ```
    Val(Operator, Operand),

    /// ```
    /// Bcond label
    /// ```
    BranchCond(Operator, Label),

    /// ```
    /// MOV Rd, =(label)
    /// ```
    LoadLabel(Register, Label),

    /// ```
    /// PUSH {Rd, ...}
    /// POP {Rd, ...}
    /// ```
    PushPop(Operator, Vec<Register>),

    /// ```
    /// STMIA Rb!, {Rd, ...}
    /// LDMIA Rb!, {Rd, ...}
    /// ```
    StmLdmIA(Operator, Register, Vec<Register>),
}
