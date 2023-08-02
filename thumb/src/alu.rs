/// Arithmetic Logic Unit
pub(crate) struct Alu {
    /// Carry flag (C)
    ///
    /// Set if the last arithmetic operation carried (addition) or
    /// borrowed (subtraction) a bit beyond the size of the register.
    carry_flag: bool,

    /// Overflow flag (V)
    ///
    /// Set if the last arithmetic operation resulted in an overflow
    /// of the most significant bit of the result.
    overflow_flag: bool,

    /// Zero flag (Z)
    ///
    /// Set if the result of the last arithmetic operation was zero.
    zero_flag: bool,

    /// Negative flag (N)
    ///
    /// Set if the result of the last arithmetic operation was negative.
    negative_flag: bool,
}

impl Alu {
    /// Create a new ALU
    pub fn init() -> Self {
        Alu {
            carry_flag: false,
            overflow_flag: false,
            zero_flag: false,
            negative_flag: false,
        }
    }

    /// Updates the zero and negative flags
    /// given the operation result.
    fn set_zn_flags(&mut self, value: u32) {
        self.zero_flag = value == 0;
        self.negative_flag = value & 0x8000_0000 != 0;
    }

    // ANCHOR Operations
    /// Addition: `Ret <- A + B`
    pub fn add(&mut self, a: u32, b: u32) -> u32 {
        let (result, carry) = a.overflowing_add(b);
        self.carry_flag = carry;
        self.overflow_flag = (a ^ b) & !(a ^ result) & 0x8000_0000 != 0;
        self.set_zn_flags(result);
        result
    }

    /// Subtraction: `Ret <- A - B`
    pub fn sub(&mut self, a: u32, b: u32) -> u32 {
        let (result, carry) = a.overflowing_sub(b);
        self.carry_flag = carry;
        self.overflow_flag = (a ^ b) & (a ^ result) & 0x8000_0000 != 0;
        self.set_zn_flags(result);
        result
    }

    /// Multiplication: `Ret <- A * B`
    pub fn mul(&mut self, a: u32, b: u32) -> u32 {
        let result = a.wrapping_mul(b);
        self.set_zn_flags(result);
        result
    }

    /// Bitwise AND: `Ret <- A & B`
    pub fn and(&mut self, a: u32, b: u32) -> u32 {
        let result = a & b;
        self.set_zn_flags(result);
        result
    }

    /// Bitwise OR: `Ret <- A | B`
    pub fn or(&mut self, a: u32, b: u32) -> u32 {
        let result = a | b;
        self.set_zn_flags(result);
        result
    }

    /// Bitwise XOR: `Ret <- A ^ B`
    pub fn xor(&mut self, a: u32, b: u32) -> u32 {
        let result = a ^ b;
        self.set_zn_flags(result);
        result
    }

    /// Logical shift left: `Ret <- A << B`
    pub fn lsl(&mut self, a: u32, b: u32) -> u32 {
        let result = a << b;
        self.carry_flag = result & 0x8000_0000 != 0;
        self.set_zn_flags(result);
        result
    }

    /// Logical shift right: `Ret <- A >> B`
    pub fn lsr(&mut self, a: u32, b: u32) -> u32 {
        let result = a >> b;
        self.carry_flag = result & 0x0000_0001 != 0;
        self.set_zn_flags(result);
        result
    }

    /// Arithmetic shift right: `Ret <- A >>> B`
    pub fn asr(&mut self, a: u32, b: u32) -> u32 {
        let result = (a as i32) >> b;
        self.carry_flag = result & 0x0000_0001 != 0;
        self.set_zn_flags(result as u32);
        result as u32
    }

    /// Rotate right: `Ret <- A ROR B`
    pub fn ror(&mut self, a: u32, b: u32) -> u32 {
        let result = a.rotate_right(b);
        self.carry_flag = result & 0x0000_0001 != 0;
        self.set_zn_flags(result);
        result
    }

    /// Add with carry: `Ret <- A + B + C`
    pub fn adc(&mut self, a: u32, b: u32) -> u32 {
        let (result, _) = a.overflowing_add(b);
        let (result, carry) = result.overflowing_add(self.carry_flag as u32);
        self.carry_flag = carry;
        self.overflow_flag = (a ^ b) & !(a ^ result) & 0x8000_0000 != 0;
        self.set_zn_flags(result);
        result
    }

    /// Subtract with carry: `Ret <- A - B - C`
    pub fn sbc(&mut self, a: u32, b: u32) -> u32 {
        let (result, _) = a.overflowing_sub(b);
        let (result, carry) = result.overflowing_sub(self.carry_flag as u32);
        self.carry_flag = carry;
        self.overflow_flag = (a ^ b) & (a ^ result) & 0x8000_0000 != 0;
        self.set_zn_flags(result);
        result
    }

    /// Negate: `Ret <- -A`
    pub fn neg(&mut self, a: u32) -> u32 {
        let (result, carry) = 0u32.overflowing_sub(a);
        self.carry_flag = carry;
        self.overflow_flag = a == 0x8000_0000;
        self.set_zn_flags(result);
        result
    }

    /// Bitwise NOT: `Ret <- !A`
    pub fn not(&mut self, a: u32) -> u32 {
        let result = !a;
        self.set_zn_flags(result);
        result
    }

    /// Bit clear: `Ret <- A & !B`
    pub fn bic(&mut self, a: u32, b: u32) -> u32 {
        let result = a & !b;
        self.set_zn_flags(result);
        result
    }

    // ANCHOR Conditions
    /// Not equal (Z clear)
    pub fn is_ne(&self) -> bool {
        !self.zero_flag
    }
    /// Equal (Z set)
    pub fn is_eq(&self) -> bool {
        self.zero_flag
    }
    /// Unsigned higher or same (C set)
    pub fn is_cs(&self) -> bool {
        self.carry_flag
    }
    /// Unsigned lower (C clear)
    pub fn is_cc(&self) -> bool {
        !self.carry_flag
    }
    /// Negative (N set)
    pub fn is_mi(&self) -> bool {
        self.negative_flag
    }
    /// Positive or zero (N clear)
    pub fn is_pl(&self) -> bool {
        !self.negative_flag
    }
    /// Overflow (V set)
    pub fn is_vs(&self) -> bool {
        self.overflow_flag
    }
    /// No overflow (V clear)
    pub fn is_vc(&self) -> bool {
        !self.overflow_flag
    }
    /// Unsigned higher (C set and Z clear)
    pub fn is_hi(&self) -> bool {
        self.carry_flag && !self.zero_flag
    }
    /// Unsigned lower or same (C clear or Z set)
    pub fn is_ls(&self) -> bool {
        !self.carry_flag || self.zero_flag
    }
    /// Signed greater than or equal (N set and V set, or N clear and V clear)
    pub fn is_ge(&self) -> bool {
        self.negative_flag == self.overflow_flag
    }
    /// Signed less than (N set and V clear, or N clear and V set)
    pub fn is_lt(&self) -> bool {
        self.negative_flag != self.overflow_flag
    }
    /// Greater than (Z clear, and either N set and V set or N clear and V clear)
    pub fn is_gt(&self) -> bool {
        !self.zero_flag && self.negative_flag == self.overflow_flag
    }
    /// Less than or equal (Z set, or N set and V clear, or N clear and V set)
    pub fn is_le(&self) -> bool {
        self.zero_flag || self.negative_flag != self.overflow_flag
    }
}
