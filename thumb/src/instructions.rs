//! Thumb instructions.
//!
//! ARMv7TDMI has 19 different instruction formats in Thumb mode.

use bin16_macro::bin16;

/// A Thumb instruction.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Instruction {
    // ANCHOR 1 -- Move shifted register
    //     000 op2 imm5 Rs Rd where op2 != 0b11
    /// Shift Rs left by a 5-bit immediate value and store the result in Rd.
    ///
    /// `LSL Rd, Rs, #imm5` >
    /// `000` `op=00` **`imm5`** **`Rs`** **`Rd`**
    LslImm { rd: u8, rs: u8, imm5: u8 },
    /// Perform logical shift right on Rs by a 5-bit immediate value and store the result in Rd.
    ///
    /// `LSR Rd, Rs, #imm5` >
    /// `000` `op=01` **`imm5`** **`Rs`** **`Rd`**
    LsrImm { rd: u8, rs: u8, imm5: u8 },
    /// Perform arithmetic shift right on Rs by a 5-bit immediate value and store the result in Rd.
    ///
    /// `ASR Rd, Rs, #imm5` >
    /// `000` `op=10` **`imm5`** **`Rs`** **`Rd`**
    AsrImm { rd: u8, rs: u8, imm5: u8 },

    // ANCHOR 2 -- Add/subtract
    //     000 11 I1 O1 imm3_or_Rn Rs Rd
    /// Add contents of Rn to contents of Rs. Place result in Rd.
    ///
    /// `ADD Rd, Rs, Rn` > `000` `11` `I=0` `O=0` **`Rn`** **`Rs`** **`Rd`**
    AddReg { rd: u8, rs: u8, rn: u8 },
    /// Subtract contents of Rn from contents of Rs. Place result in Rd.
    ///
    /// `SUB Rd, Rs, Rn` > `000` `11` `I=0` `O=1` **`Rn`** **`Rs`** **`Rd`**
    SubReg { rd: u8, rs: u8, rn: u8 },
    /// Add 3-bit immediate value to contents of Rs. Place result in Rd.
    ///
    /// `ADD Rd, Rs, #imm3` > `000` `11` `I=1` `O=0` **`imm3`** **`Rs`** **`Rd`**
    AddImm3 { rd: u8, rs: u8, imm3: u8 },
    /// Subtract 3-bit immediate value from contents of Rs. Place result in Rd.
    ///
    /// `SUB Rd, Rs, #imm3` > `000` `11` `I=1` `O=1` **`imm3`** **`Rs`** **`Rd`**
    SubImm3 { rd: u8, rs: u8, imm3: u8 },

    // ANCHOR 3 -- Move/compare/add/subtract immediate
    //     001 op2 rd imm8
    /// Move 8-bit immediate value into Rd.
    ///
    /// `MOV Rd, #imm8` > `001` `op=00` **`Rd`** **`imm8`**
    MovImm { rd: u8, imm8: u8 },
    /// Compare contents of Rd with 8-bit immediate value.
    ///
    /// `CMP Rd, #imm8` > `001` `op=01` **`Rd`** **`imm8`**
    CmpImm { rd: u8, imm8: u8 },
    /// Add 8-bit immediate value to contents of Rd and place the result in Rd.
    ///
    /// `ADD Rd, #imm8` > `001` `op=10` **`Rd`** **`imm8`**
    AddImm8 { rd: u8, imm8: u8 },
    /// Subtract 8-bit immediate value from contents of Rd and place the result in Rd.
    ///
    /// `SUB Rd, #imm8` > `001` `op=11` **`Rd`** **`imm8`**
    SubImm8 { rd: u8, imm8: u8 },

    // ANCHOR 4 -- ALU operations
    //     010 000 op4 Rs Rd
    /// Rd := Rd AND Rs
    ///
    /// `AND Rd, Rs` > `010000` `op=0000` **`Rs`** **`Rd`**
    And { rd: u8, rs: u8 },
    /// Rd := Rd XOR Rs
    ///
    /// `EOR Rd, Rs` > `010000` `op=0001` **`Rs`** **`Rd`**
    Eor { rd: u8, rs: u8 },
    /// Rd := Rd LSL Rs
    ///
    /// `LSL Rd, Rs` > `010000` `op=0010` **`Rs`** **`Rd`**
    Lsl { rd: u8, rs: u8 },
    /// Rd := Rd LSR Rs
    ///
    /// `LSR Rd, Rs` > `010000` `op=0011` **`Rs`** **`Rd`**
    Lsr { rd: u8, rs: u8 },
    /// Rd := Rd ASR Rs
    ///
    /// `ASR Rd, Rs` > `010000` `op=0100` **`Rs`** **`Rd`**
    Asr { rd: u8, rs: u8 },
    /// Rd := Rd ADC Rs
    ///
    /// `ADC Rd, Rs` > `010000` `op=0101` **`Rs`** **`Rd`**
    Adc { rd: u8, rs: u8 },
    /// Rd := Rd SBC Rs
    ///
    /// `SBC Rd, Rs` > `010000` `op=0110` **`Rs`** **`Rd`**
    Sbc { rd: u8, rs: u8 },
    /// Rd := Rd ROR Rs
    ///
    /// `ROR Rd, Rs` > `010000` `op=0111` **`Rs`** **`Rd`**
    Ror { rd: u8, rs: u8 },
    /// Set condition codes on Rd AND Rs
    ///
    /// `TST Rd, Rs` > `010000` `op=1000` **`Rs`** **`Rd`**
    Tst { rd: u8, rs: u8 },
    /// Rd := -Rs
    ///
    /// `NEG Rd, Rs` > `010000` `op=1001` **`Rs`** **`Rd`**
    Neg { rd: u8, rs: u8 },
    /// Set condition codes on Rd - Rs
    ///
    /// `CMP Rd, Rs` > `010000` `op=1010` **`Rs`** **`Rd`**
    Cmp { rd: u8, rs: u8 },
    /// Set condition codes on Rd + Rs
    ///
    /// `CMN Rd, Rs` > `010000` `op=1011` **`Rs`** **`Rd`**
    Cmn { rd: u8, rs: u8 },
    /// Rd := Rd OR Rs
    ///
    /// `ORR Rd, Rs` > `010000` `op=1100` **`Rs`** **`Rd`**
    Orr { rd: u8, rs: u8 },
    /// Rd := Rd * Rs
    ///
    /// `MUL Rd, Rs` > `010000` `op=1101` **`Rs`** **`Rd`**
    Mul { rd: u8, rs: u8 },
    /// Rd := Rd AND NOT Rs
    ///
    /// `BIC Rd, Rs` > `010000` `op=1110` **`Rs`** **`Rd`**
    Bic { rd: u8, rs: u8 },
    /// Rd := NOT Rs
    ///
    /// `MVN Rd, Rs` > `010000` `op=1111` **`Rs`** **`Rd`**
    Mvn { rd: u8, rs: u8 },

    // ANCHOR 5 -- Hi register operations/branch exchange
    //     010 001 op2 H1-flag H2-flag Rs/Hs Rd/Hd
    /// Add a register in the range 8-15 to a register in the range 0-7.
    ///
    /// `ADD Rd, Hs` > `010001` `op=00` `H1=0` `H2=1` **`Hs`** **`Rd`**
    AddLowHi { rd: u8, hs: u8 },
    /// Add a register in the range 0-7 to a register in the range 8-15.
    ///
    /// `ADD Hd, Rs` > `010001` `op=00` `H1=1` `H2=0` **`Rs`** **`Hd`**
    AddHiLow { hd: u8, rs: u8 },
    /// Add two registers in the range 8-15.
    ///
    /// `ADD Hd, Hs` > `010001` `op=00` `H1=1` `H2=1` **`Hs`** **`Hd`**
    AddHiHi { hd: u8, hs: u8 },
    /// Compare a register in the range 0-7 with a register in the range 8-15.
    /// Set the condition code flags on the result.
    ///
    /// `CMP Rd, Hs` > `010001` `op=01` `H1=0` `H2=1` **`Hs`** **`Rd`**
    CmpLowHi { rd: u8, hs: u8 },
    /// Compare a register in the range 8-15 with a register in the range 0-7.
    /// Set the condition code flags on the result.
    ///
    /// `CMP Hd, Rs` > `010001` `op=01` `H1=1` `H2=0` **`Rs`** **`Hd`**
    CmpHiLow { hd: u8, rs: u8 },
    /// Compare two registers in the range 8-15. Set the condition code flags on the result
    ///
    /// `CMP Hd, Hs` > `010001` `op=01` `H1=1` `H2=1` **`Hs`** **`Hd`**
    CmpHiHi { hd: u8, hs: u8 },
    /// Move a register in the range 8-15 to a register in the range 0-7.
    ///
    /// `MOV Rd, Hs` > `010001` `op=10` `H1=0` `H2=1` **`Hs`** **`Rd`**
    MovLowHi { rd: u8, hs: u8 },
    /// Move a register in the range 0-7 to a register in the range 8-15.
    ///
    /// `MOV Hd, Rs` > `010001` `op=10` `H1=1` `H2=0` **`Rs`** **`Hd`**
    MovHiLow { hd: u8, rs: u8 },
    /// Move a value between two registers in the range 8-15.
    ///
    /// `MOV Hd, Hs` > `010001` `op=10` `H1=1` `H2=1` **`Hs`** **`Hd`**
    MovHiHi { hd: u8, hs: u8 },
    /// Perform branch (plus optional state change) to address in a register in the range 0-7.
    ///
    /// `BX Rs` > `010001` `op=11` `H1=0` `H2=0` **`Rs`** `000`
    Bx { rs: u8 },
    /// Perform branch (plus optional state change) to address in a register in the range 8-15.
    ///
    /// `BX Hs` > `010001` `op=11` `H1=0` `H2=1` **`Hs`** `000`
    BxHi { hs: u8 },

    // ANCHOR 6 -- PC-relative load
    //     010 01 Rd Word8
    /// Add unsigned offset (255 words, 1020 bytes) in Imm to the current value of the PC.
    /// Load the word from the resulting address into Rd.
    ///
    /// `LDR Rd, [PC, #Imm]` > `01001` **`Rd`** **`Imm`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm8` is shifted right by 2.
    LdrPc { rd: u8, imm8: u8 },

    // ANCHOR 7 -- Load/store with register offset
    //     010 1 L B 0 Ro Rn Rd/s
    //     L is 0 for store, 1 for load
    //     B is 0 for word, 1 for byte
    /// Pre-indexed word store: Calculate the target address by
    /// adding together the value in Rb and the value in Ro.
    /// Store the contents of Rs at the address.
    ///
    /// `STR Rs, [Rb, Ro]` > `0101` `L=0` `B=0` `0` **`Ro`** **`Rb`** **`Rs`**
    StrReg { rb: u8, ro: u8, rs: u8 },
    /// Pre-indexed byte store: Calculate the target address by
    /// adding together the value in Rb and the value in Ro.
    /// Store the byte value in Rs at the resulting address.
    ///
    /// `STRB Rs, [Rb, Ro]` > `0101` `L=0` `B=1` `0` **`Ro`** **`Rb`** **`Rs`**
    StrbReg { rb: u8, ro: u8, rs: u8 },
    /// Pre-indexed word load: Calculate the source address by
    /// adding together the value in Rb and the value in Ro.
    /// Load the contents of the address into Rd.
    ///
    /// `LDR Rd, [Rb, Ro]` > `0101` `L=1` `B=0` `0` **`Ro`** **`Rb`** **`Rd`**
    LdrReg { rb: u8, ro: u8, rd: u8 },
    /// Pre-indexed byte load: Calculate the source address by
    /// adding together the value in Rb and the value in Ro.
    /// Load the byte value at the resulting address.
    ///
    /// `LDRB Rd, [Rb, Ro]` > `0101` `L=1` `B=1` `0` **`Ro`** **`Rb`** **`Rd`**
    LdrbReg { rb: u8, ro: u8, rd: u8 },

    // ANCHOR 8 -- Load/store sign-extended byte/halfword
    //     010 1 H S 1 Ro Rn Rd/s
    /// Store halfword: Add Ro to base address in Rb. Store bits 0-15 of Rs at the resulting address.
    ///
    /// `STRH Rs, [Rb, Ro]` > `0101` `H=0` `S=0` `1` **`Ro`** **`Rb`** **`Rs`**
    StrhReg { rb: u8, ro: u8, rs: u8 },
    /// Load halfword: Add Ro to base address in Rb. Load bits 0-15 of Rd from the resulting address,
    /// and set bits 16-31 of Rd to 0.
    ///
    /// `LDRH Rd, [Rb, Ro]` > `0101` `H=1` `S=0` `1` **`Ro`** **`Rb`** **`Rs`**
    LdrhReg { rb: u8, ro: u8, rd: u8 },
    /// Load sign-extended byte: Add Ro to base address in Rb. Load bits 0-7 of Rd from the resulting address,
    /// and set bits 8-31 of Rd to bit 7.
    ///
    /// `LDSB Rd, [Rb, Ro]` > `0101` `H=0` `S=1` `1` **`Ro`** **`Rb`** **`Rd`**
    LdsbReg { rb: u8, ro: u8, rd: u8 },
    /// Load sign-extended halfword: Add Ro to base address in Rb. Load bits 0-15 of Rd from the resulting address,
    /// and set bits 16-31 of Rd to bit 15.
    ///
    /// `LDSH Rd, [Rb, Ro]` > `0101` `H=1` `S=1` `1` **`Ro`** **`Rb`** **`Rd`**
    LdshReg { rb: u8, ro: u8, rd: u8 },

    // ANCHOR 9 -- Load/store with immediate offset
    //     011 B L Imm5 Rb Rd/s
    /// Calculate the target address by adding together the value in Rb and Imm.
    /// Store the contents of Rs at the address.
    ///
    /// `STR Rs, [Rb, #Imm]` > `011` `B=0` `L=0` **`Imm`** **`Rb`** **`Rs`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm5` is shifted right by 2.
    StrImm { rb: u8, imm5: u8, rs: u8 },
    /// Calculate the source address by adding together the value in Rb and Imm.
    /// Load Rd from the address.
    ///
    /// `LDR Rd, [Rb, #Imm]` > `011` `B=0` `L=1` **`Imm`** **`Rb`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm5` is shifted right by 2.
    LdrImm { rb: u8, imm5: u8, rd: u8 },
    /// Calculate the target address by adding together the value in Rb and Imm.
    /// Store the byte value in Rs at the address.
    ///
    /// `STRB Rs, [Rb, #Imm]` > `011` `B=1` `L=0` **`Imm`** **`Rb`** **`Rs`**
    StrbImm { rb: u8, imm5: u8, rs: u8 },
    /// Calculate source address by adding together the value in Rb and Imm.
    /// Load the byte value at the address into Rd.
    ///
    /// `LDRB Rd, [Rb, #Imm]` > `011` `B=1` `L=1` **`Imm`** **`Rb`** **`Rd`**
    LdrbImm { rb: u8, imm5: u8, rd: u8 },

    // ANCHOR 10 -- Load/store halfword
    //     100 0 L Imm5 Rb Rd/s
    /// Add #Imm to base address in Rb and store bits 0-15 of Rs at the resulting address.
    ///
    /// `STRH Rs, [Rb, #Imm]` > `1000` `L=0` **`Imm`** **`Rb`** **`Rs`**
    ///
    /// **Note**: `#Imm` is a multiple of 2, therefore `imm5` is shifted right by 1.
    StrhImm { rb: u8, imm5: u8, rs: u8 },
    /// Add #Imm to base address in Rb. Load bits 0-15 from the resulting address into Rd
    /// and set bits 16-31 to zero.
    ///
    /// `LDRH Rd, [Rb, #Imm]` > `1000` `L=1` **`Imm`** **`Rb`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 2, therefore `imm5` is shifted right by 1.
    LdrhImm { rb: u8, imm5: u8, rd: u8 },

    // ANCHOR 11 -- SP-relative load/store
    //     100 1 L Rd Imm8
    /// Add unsigned offset (255 words, 1020 bytes) in Imm to the current value of the SP (R7).
    /// Store the contents of Rd at the resulting address.
    ///
    /// `STR Rd, [SP, #Imm]` > `1001` `L=0` **`Imm`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm8` is shifted right by 2.
    StrSpImm { imm8: u8, rs: u8 },
    /// Add unsigned offset (255 words, 1020 bytes) in Imm to the current value of the SP (R7).
    /// Load the word from the resulting address into Rd.
    ///
    /// `LDR Rd, [SP, #Imm]` > `1001` `L=1` **`Imm`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm8` is shifted right by 2.
    LdrSpImm { imm8: u8, rd: u8 },

    // ANCHOR 12 -- Load address
    //    101 0 SP Rd Imm8
    //    SP is 0 for SP and 1 for PC
    /// Add #Imm to the current value of the program counter (PC) and load the result into Rd.
    ///
    /// `ADD Rd, PC, #Imm` > `1010` `SP=0` **`Imm`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm8` is shifted right by 2.
    AddPcImm { imm8: u8, rd: u8 },
    /// Add #Imm to the current value of the stack pointer (SP) and load the result into Rd.
    ///
    /// `ADD Rd, SP, #Imm` > `1010` `SP=1` **`Imm`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm8` is shifted right by 2.
    AddSpImm { imm8: u8, rd: u8 },

    // ANCHOR 13 -- Add offset to Stack Pointer
    //    101 1 0000 S Imm7
    //    S is 0 for positive offset and 1 for negative offset
    /// Add #Imm to the stack pointer (SP).
    ///
    /// `ADD SP, #Imm` > `1011` `0000` `S=0` **`Imm`**
    AddSpPosImm { imm7: u8 },
    /// Add #-Imm to the stack pointer (SP)
    ///
    /// `ADD SP, #-Imm` > `1011` `0000` `S=1` **`Imm`**
    AddSpNegImm { imm7: u8 },

    // ANCHOR 14 -- Push/pop registers
    //    101 1 L 1 0 R Rlist8
    //    L is 0 for push and 1 for pop
    //    R enables storing LR/loading PC
    /// Push the registers specified by Rlist onto the stack. Update the stack pointer.
    ///
    /// `PUSH { Rlist }` > `1011` `L=0` `10` `R=0` **`Rlist`**
    Push { rlist: u8 },
    /// Push the Link Register and the registers specified by Rlist (if any)
    /// onto the stack. Update the stack pointer.
    ///
    /// `PUSH { Rlist, LR }` > `1011` `L=0` `10` `R=1` **`Rlist`**
    PushLr { rlist: u8 },
    /// Pop values off the stack into the registers specified by Rlist. Update the stack pointer.
    ///
    /// `POP { Rlist }` > `1011` `L=1` `10` `R=0` **`Rlist`**
    Pop { rlist: u8 },
    /// Pop values off the stack and load into the registers specified by Rlist.
    /// Pop the PC off the stack. Update the stack pointer.
    ///
    /// `POP { Rlist, PC }` > `1011` `L=1` `10` `R=1` **`Rlist`**
    PopPc { rlist: u8 },

    // ANCHOR 15 -- Multiple load/store
    //    1100 L Rb Rlist8
    //    L is 0 for store and 1 for load
    /// Store the registers specified by Rlist, starting at the base address in Rb.
    /// Write back the new base address
    ///
    /// `STMIA Rb!, { Rlist }` > `1100` `L=0` **`Rb`** **`Rlist`**
    Stmia { rb: u8, rlist: u8 },
    /// Load the registers specified by Rlist, starting at the base address in Rb.
    /// Write back the new base address.
    ///
    /// `LDMIA Rb!, { Rlist }` > `1100` `L=1` **`Rb`** **`Rlist`**
    Ldmia { rb: u8, rlist: u8 },

    // ANCHOR 16 -- Conditional branch
    //    1101 Cond4 SOffset8
    // While label specifies a full 9-bit two’s complement address, this must always be
    // halfword-aligned (ie with bit 0 set to 0) since the assembler actually places label >> 1
    // in field SOffset8.
    /// Branch if Z set (equal)
    ///
    /// `BEQ label` > `1101` `Cond=0000` **`SOffset8`**
    Beq { soffset: u8 },
    /// Branch if Z clear (not equal)
    ///
    /// `BNE label` > `1101` `Cond=0001` **`SOffset8`**
    Bne { soffset: u8 },
    /// Branch if C set (unsigned higher or same)
    ///
    /// `BCS label` > `1101` `Cond=0010` **`SOffset8`**
    Bcs { soffset: u8 },
    /// Branch if C clear (unsigned lower)
    ///
    /// `BCC label` > `1101` `Cond=0011` **`SOffset8`**
    Bcc { soffset: u8 },
    /// Branch if N set (negative)
    ///
    /// `BMI label` > `1101` `Cond=0100` **`SOffset8`**
    Bmi { soffset: u8 },
    /// Branch if N clear (positive or zero)
    ///
    /// `BPL label` > `1101` `Cond=0101` **`SOffset8`**
    Bpl { soffset: u8 },
    /// Branch if V set (overflow)
    ///
    /// `BVS label` > `1101` `Cond=0110` **`SOffset8`**
    Bvs { soffset: u8 },
    /// Branch if V clear (no overflow)
    ///
    /// `BVC label` > `1101` `Cond=0111` **`SOffset8`**
    Bvc { soffset: u8 },
    /// Branch if C set and Z clear (unsigned higher)
    ///
    /// `BHI label` > `1101` `Cond=1000` **`SOffset8`**
    Bhi { soffset: u8 },
    /// Branch if C clear or Z set (unsigned lower or same)
    ///
    /// `BLS label` > `1101` `Cond=1001` **`SOffset8`**
    Bls { soffset: u8 },
    /// Branch if N set and V set, or N clear and V clear (signed greater than or equal)
    ///
    /// `BGE label` > `1101` `Cond=1010` **`SOffset8`**
    Bge { soffset: u8 },
    /// Branch if N set and V clear, or N clear and V set (signed less than)
    ///
    /// `BLT label` > `1101` `Cond=1011` **`SOffset8`**
    Blt { soffset: u8 },
    /// Branch if Z clear, and either N set and V set or N clear and V clear (greater than)
    ///
    /// `BGT label` > `1101` `Cond=1100` **`SOffset8`**
    Bgt { soffset: u8 },
    /// Branch if Z set, or N set and V clear, or N clear and V set (less than or equal)
    ///
    /// `BLE label` > `1101` `Cond=1101` **`SOffset8`**
    Ble { soffset: u8 },

    // ANCHOR 17 -- Software interrupt
    //   1101 1111 SvcImm8
    /// Perform Software Interrupt:
    /// Move the address of the next instruction into LR, move CPSR to SPSR,
    /// load the SWI vector address (0x8) into the PC.
    /// Switch to ARM state and enter SVC mode.
    ///
    /// `SWI imm8` > `1101` **`1111`** **`Imm8`**
    Swi { imm: u8 },

    // ANCHOR 18 -- Unconditional branch
    //   11100 Offset11
    /// Branch PC relative +/- Offset11 << 1, where label is PC +/- 2048 bytes.
    ///
    /// `B label` > `11100` **`Offset11`**
    B { offset11: u16 },

    // ANCHOR 19 -- Long branch with link
    //   1111 H Offset11
    /// Long branch with link. This instruction must be called once with H=0
    /// and then once with H=1 to set both halves of the address to jump to.
    ///
    /// `BL label` > `1111` **`H`** **`Offset11`**
    BlHalf { hi: bool, offset11: u16 },
}

impl Into<u16> for Instruction {
    /// Returns the 16-bit representation of the instruction.
    fn into(self) -> u16 {
        use Instruction::*;

        match self {
            // Format 1
            LslImm { rd, rs, imm5 } => bin16!("000_00{5}{3}{3}", imm5, rs, rd),
            LsrImm { rd, rs, imm5 } => bin16!("000_01{5}{3}{3}", imm5, rs, rd),
            AsrImm { rd, rs, imm5 } => bin16!("000_10{5}{3}{3}", imm5, rs, rd),

            // Format 2
            AddReg { rd, rs, rn } => bin16!("00011_00{3}{3}{3}", rn, rs, rd),
            SubReg { rd, rs, rn } => bin16!("00011_01{3}{3}{3}", rn, rs, rd),
            AddImm3 { rd, rs, imm3 } => bin16!("00011_10{3}{3}{3}", imm3, rs, rd),
            SubImm3 { rd, rs, imm3 } => bin16!("00011_11{3}{3}{3}", imm3, rs, rd),

            // Format 3
            MovImm { rd, imm8 } => bin16!("001_00{3}{8}", rd, imm8),
            CmpImm { rd, imm8 } => bin16!("001_01{3}{8}", rd, imm8),
            AddImm8 { rd, imm8 } => bin16!("001_10{3}{8}", rd, imm8),
            SubImm8 { rd, imm8 } => bin16!("001_11{3}{8}", rd, imm8),

            // Format 4
            And { rd, rs } => bin16!("010000_0000{3}{3}", rs, rd),
            Eor { rd, rs } => bin16!("010000_0001{3}{3}", rs, rd),
            Lsl { rd, rs } => bin16!("010000_0010{3}{3}", rs, rd),
            Lsr { rd, rs } => bin16!("010000_0011{3}{3}", rs, rd),
            Asr { rd, rs } => bin16!("010000_0100{3}{3}", rs, rd),
            Adc { rd, rs } => bin16!("010000_0101{3}{3}", rs, rd),
            Sbc { rd, rs } => bin16!("010000_0110{3}{3}", rs, rd),
            Ror { rd, rs } => bin16!("010000_0111{3}{3}", rs, rd),
            Tst { rd, rs } => bin16!("010000_1000{3}{3}", rs, rd),
            Neg { rd, rs } => bin16!("010000_1001{3}{3}", rs, rd),
            Cmp { rd, rs } => bin16!("010000_1010{3}{3}", rs, rd),
            Cmn { rd, rs } => bin16!("010000_1011{3}{3}", rs, rd),
            Orr { rd, rs } => bin16!("010000_1100{3}{3}", rs, rd),
            Mul { rd, rs } => bin16!("010000_1101{3}{3}", rs, rd),
            Bic { rd, rs } => bin16!("010000_1110{3}{3}", rs, rd),
            Mvn { rd, rs } => bin16!("010000_1111{3}{3}", rs, rd),

            // Format 5
            AddLowHi { rd, hs } => bin16!("010001_00_01{3}{3}", hs, rd),
            AddHiLow { hd, rs } => bin16!("010001_00_10{3}{3}", rs, hd),
            AddHiHi { hd, hs } => bin16!("010001_00_11{3}{3}", hs, hd),
            CmpLowHi { rd, hs } => bin16!("010001_01_01{3}{3}", hs, rd),
            CmpHiLow { hd, rs } => bin16!("010001_01_10{3}{3}", rs, hd),
            CmpHiHi { hd, hs } => bin16!("010001_01_11{3}{3}", hs, hd),
            MovLowHi { rd, hs } => bin16!("010001_10_01{3}{3}", hs, rd),
            MovHiLow { hd, rs } => bin16!("010001_10_10{3}{3}", rs, hd),
            MovHiHi { hd, hs } => bin16!("010001_10_11{3}{3}", hs, hd),
            Bx { rs } => bin16!("010001_11_00{3}000", rs),
            BxHi { hs } => bin16!("010001_11_01{3}000", hs),

            // Format 6
            LdrPc { rd, imm8 } => bin16!("01001{3}{8}", rd, imm8),

            // Format 7
            StrReg { rb, ro, rs: rd } => bin16!("0101_00_0{3}{3}{3}", ro, rb, rd),
            StrbReg { rb, ro, rs: rd } => bin16!("0101_01_0{3}{3}{3}", ro, rb, rd),
            LdrReg { rb, ro, rd } => bin16!("0101_10_0{3}{3}{3}", ro, rb, rd),
            LdrbReg { rb, ro, rd } => bin16!("0101_11_0{3}{3}{3}", ro, rb, rd),

            // Format 8
            StrhReg { rb, ro, rs: rd } => bin16!("0101_00_1{3}{3}{3}", ro, rb, rd),
            LdrhReg { rb, ro, rd } => bin16!("0101_10_1{3}{3}{3}", ro, rb, rd),
            LdsbReg { rb, ro, rd } => bin16!("0101_01_1{3}{3}{3}", ro, rb, rd),
            LdshReg { rb, ro, rd } => bin16!("0101_11_1{3}{3}{3}", ro, rb, rd),

            // Format 9
            StrImm { rb, imm5, rs: rd } => bin16!("011_00{5}{3}{3}", imm5, rb, rd),
            LdrImm { rb, imm5, rd } => bin16!("011_01{5}{3}{3}", imm5, rb, rd),
            StrbImm { rb, imm5, rs: rd } => bin16!("011_10{5}{3}{3}", imm5, rb, rd),
            LdrbImm { rb, imm5, rd } => bin16!("011_11{5}{3}{3}", imm5, rb, rd),

            // Format 10
            StrhImm { rb, imm5, rs: rd } => bin16!("1000_0{5}{3}{3}", imm5, rb, rd),
            LdrhImm { rb, imm5, rd } => bin16!("1000_1{5}{3}{3}", imm5, rb, rd),

            // Format 11
            StrSpImm { imm8, rs: rd } => bin16!("1001_0{8}{3}", imm8, rd),
            LdrSpImm { imm8, rd } => bin16!("1001_1{8}{3}", imm8, rd),

            // Format 12
            AddPcImm { imm8, rd } => bin16!("1010_0{8}{3}", imm8, rd),
            AddSpImm { imm8, rd } => bin16!("1010_1{8}{3}", imm8, rd),

            // Format 13
            AddSpPosImm { imm7 } => bin16!("10110000_0{7}", imm7),
            AddSpNegImm { imm7 } => bin16!("10110000_1{7}", imm7),

            // Format 14
            Push { rlist } => bin16!("1011_0_10_0{8}", rlist),
            PushLr { rlist } => bin16!("1011_0_10_1{8}", rlist),
            Pop { rlist } => bin16!("1011_1_10_0{8}", rlist),
            PopPc { rlist } => bin16!("1011_1_10_1{8}", rlist),

            // Format 15
            Stmia { rb, rlist } => bin16!("1100_0{3}{8}", rb, rlist),
            Ldmia { rb, rlist } => bin16!("1100_1{3}{8}", rb, rlist),

            // Format 16
            Beq { soffset } => bin16!("1101_0000{8}", soffset),
            Bne { soffset } => bin16!("1101_0001{8}", soffset),
            Bcs { soffset } => bin16!("1101_0010{8}", soffset),
            Bcc { soffset } => bin16!("1101_0011{8}", soffset),
            Bmi { soffset } => bin16!("1101_0100{8}", soffset),
            Bpl { soffset } => bin16!("1101_0101{8}", soffset),
            Bvs { soffset } => bin16!("1101_0110{8}", soffset),
            Bvc { soffset } => bin16!("1101_0111{8}", soffset),
            Bhi { soffset } => bin16!("1101_1000{8}", soffset),
            Bls { soffset } => bin16!("1101_1001{8}", soffset),
            Bge { soffset } => bin16!("1101_1010{8}", soffset),
            Blt { soffset } => bin16!("1101_1011{8}", soffset),
            Bgt { soffset } => bin16!("1101_1100{8}", soffset),
            Ble { soffset } => bin16!("1101_1101{8}", soffset),

            // Format 17
            Swi { imm } => bin16!("1101_1111{8}", imm),

            // Format 18
            B { offset11 } => bin16!("11100{11}", offset11),

            // Format 19
            BlHalf { hi, offset11 } => bin16!("1111{}{11}", hi, offset11),
        }
    }
}

impl Instruction {
    /// Decodes a 16-bit Thumb instruction.
    pub fn decode(data: u16) -> Option<Self> {
        use Instruction::*;

        // Decoding will be divided into eight sections based on the starting 3 bits.
        Some(match data >> 13 {
            // Formats 1 and 2
            0b000 => {
                // These registers are in common
                let rs = get_rs(data);
                let rd = get_rd(data);

                // Match based on bits 12 and 11.
                match (data >> 11) & 0b11 {
                    // Format 1
                    0b00 => LslImm {
                        rd,
                        rs,
                        imm5: get_imm5(data),
                    },
                    0b01 => LsrImm {
                        rd,
                        rs,
                        imm5: get_imm5(data),
                    },
                    0b10 => AsrImm {
                        rd,
                        rs,
                        imm5: get_imm5(data),
                    },
                    // Format 2
                    0b11 => {
                        let rn = get_rn(data);
                        let imm3 = rn;

                        // Get the OI flag combo.
                        match (data >> 9) & 0b11 {
                            0b00 => AddReg { rd, rs, rn },
                            0b01 => SubReg { rd, rs, rn },
                            0b10 => AddImm3 { rd, rs, imm3 },
                            0b11 => SubImm3 { rd, rs, imm3 },
                            _ => unreachable!(),
                        }
                    }

                    _ => unreachable!(),
                }
            }
            // Format 3
            0b001 => {
                let rd = ((data >> 8) & 0b111) as u8;
                let imm8 = (data & 0xFF) as u8;

                // Match based on bits 12 and 11.
                match (data >> 11) & 0b11 {
                    0b00 => MovImm { rd, imm8 },
                    0b01 => CmpImm { rd, imm8 },
                    0b10 => AddImm8 { rd, imm8 },
                    0b11 => SubImm8 { rd, imm8 },
                    _ => unreachable!(),
                }
            }
            // Formats 4, 5, 6, 7 and 8
            0b010 => {
                // Match the next three bits (bits 12, 11 and 10).
                match (data >> 10) & 0b111 {
                    // Format 4
                    0b000 => {
                        let rs = get_rs(data);
                        let rd = get_rd(data);

                        // Match the opcode
                        match (data >> 6) & 0b1111 {
                            0b0000 => And { rd, rs },
                            0b0001 => Eor { rd, rs },
                            0b0010 => Lsl { rd, rs },
                            0b0011 => Lsr { rd, rs },
                            0b0100 => Asr { rd, rs },
                            0b0101 => Adc { rd, rs },
                            0b0110 => Sbc { rd, rs },
                            0b0111 => Ror { rd, rs },
                            0b1000 => Tst { rd, rs },
                            0b1001 => Neg { rd, rs },
                            0b1010 => Cmp { rd, rs },
                            0b1011 => Cmn { rd, rs },
                            0b1100 => Orr { rd, rs },
                            0b1101 => Mul { rd, rs },
                            0b1110 => Bic { rd, rs },
                            0b1111 => Mvn { rd, rs },

                            _ => unreachable!(),
                        }
                    }
                    // Format 5
                    0b001 => {
                        let rs = get_rs(data);
                        let rd = get_rd(data);
                        let hs = rs;
                        let hd = rd;

                        // Match the opcode
                        match (data >> 6) & 0b1111 {
                            0b0001 => AddLowHi { rd, hs },
                            0b0010 => AddHiLow { hd, rs },
                            0b0011 => AddHiHi { hd, hs },
                            0b0101 => CmpLowHi { rd, hs },
                            0b0110 => CmpHiLow { hd, rs },
                            0b0111 => CmpHiHi { hd, hs },
                            0b1001 => MovLowHi { rd, hs },
                            0b1010 => MovHiLow { hd, rs },
                            0b1011 => MovHiHi { hd, hs },

                            0b1100 => Bx { rs },
                            0b1101 => BxHi { hs },

                            0b0000..=0b1111 => return None,

                            _ => unreachable!(),
                        }
                    }
                    // Format 6
                    0b010 | 0b011 => LdrPc {
                        rd: ((data >> 8) & 0b111) as u8,
                        imm8: (data & 0xff) as u8,
                    },
                    // Formats 7 and 8
                    0b100..=0b111 => {
                        let rd = get_rd(data);
                        let rb = get_rs(data);
                        let ro = get_rn(data);

                        // Match the L and B flags (bits 11 and 10) and
                        // bit 9 differentiating between formats 7 and 8.
                        match (data >> 9) & 0b111 {
                            0b000 => StrReg { rb, ro, rs: rd },
                            0b010 => StrbReg { rb, ro, rs: rd },
                            0b100 => LdrReg { rb, ro, rd },
                            0b110 => LdrbReg { rb, ro, rd },
                            0b001 => StrhReg { rb, ro, rs: rd },
                            0b101 => LdrhReg { rb, ro, rd },
                            0b011 => LdsbReg { rb, ro, rd },
                            0b111 => LdshReg { rb, ro, rd },

                            _ => unreachable!(),
                        }
                    }

                    _ => unreachable!(),
                }
            }
            // Format 9
            0b011 => {
                let rd = get_rd(data);
                let rb = get_rs(data);
                let imm5 = get_imm5(data);

                // Match the B and L flags (bits 11 and 10).
                match (data >> 11) & 0b11 {
                    0b00 => StrImm { rb, imm5, rs: rd },
                    0b01 => LdrImm { rb, imm5, rd },
                    0b10 => StrbImm { rb, imm5, rs: rd },
                    0b11 => LdrbImm { rb, imm5, rd },

                    _ => unreachable!(),
                }
            }
            // Formats 10 and 11
            0b100 => {
                let rd = get_rd(data);
                let next_flag = ((data >> 11) & 1) == 1;

                match ((data >> 12) & 1) == 1 {
                    // Format 10
                    false => {
                        let rb = get_rs(data);
                        let imm5 = get_imm5(data);
                        match next_flag {
                            false => StrhImm { rb, imm5, rs: rd },
                            true => LdrhImm { rb, imm5, rd },
                        }
                    }
                    // Format 11
                    true => {
                        let imm8 = ((data >> 3) & 0xff) as u8;
                        match next_flag {
                            false => StrSpImm { imm8, rs: rd },
                            true => LdrSpImm { imm8, rd },
                        }
                    }
                }
            }
            // Formats 12, 13 and 14
            0b101 => {
                // Get the bit 12
                match (data >> 12) & 1 == 1 {
                    // Format 12
                    false => {
                        let rd = get_rd(data);
                        let imm8 = ((data >> 3) & 0xff) as u8;

                        // Match the bit 11
                        match (data >> 11) & 1 == 1 {
                            false => AddPcImm { imm8, rd },
                            true => AddSpImm { imm8, rd },
                        }
                    }
                    // Formats 13 and 14
                    true => {
                        match (data >> 10) & 0b11 {
                            // Format 13
                            0b00 => {
                                let imm7 = (data & 0x7f) as u8;
                                match (data >> 7) & 1 == 1 {
                                    false => AddSpPosImm { imm7 },
                                    true => AddSpNegImm { imm7 },
                                }
                            }
                            0b10 => return None,
                            // Format 14
                            0b01 | 0b11 => {
                                // Get the instruction type based on bits 11, 10, 9 and 8
                                let rlist = (data & 0xff) as u8;
                                match (data >> 8) & 0b1111 {
                                    0b0100 => Push { rlist },
                                    0b0101 => PushLr { rlist },
                                    0b1100 => Pop { rlist },
                                    0b1101 => PopPc { rlist },

                                    0b0000..=0b1111 => return None,

                                    _ => unreachable!(),
                                }
                            }

                            _ => unreachable!(),
                        }
                    }
                }
            }
            // Formats 15, 16 and 17
            0b110 => {
                let last_eight_bits = (data & 0xff) as u8;

                // Get the bit 12
                match (data >> 12) & 1 == 1 {
                    // Format 15
                    false => {
                        let rb = ((data >> 8) & 0b111) as u8;
                        let rlist = last_eight_bits;

                        // Match bit 11
                        match (data >> 11) & 1 == 1 {
                            false => Stmia { rb, rlist },
                            true => Ldmia { rb, rlist },
                        }
                    }
                    // Formats 16 or 17
                    true => {
                        let soffset = last_eight_bits;
                        let imm = last_eight_bits;

                        // Match the condition
                        match ((data >> 8) & 0xf) as u8 {
                            // Format 16
                            0b0000 => Beq { soffset },
                            0b0001 => Bne { soffset },
                            0b0010 => Bcs { soffset },
                            0b0011 => Bcc { soffset },
                            0b0100 => Bmi { soffset },
                            0b0101 => Bpl { soffset },
                            0b0110 => Bvs { soffset },
                            0b0111 => Bvc { soffset },
                            0b1000 => Bhi { soffset },
                            0b1001 => Bls { soffset },
                            0b1010 => Bge { soffset },
                            0b1011 => Blt { soffset },
                            0b1100 => Bgt { soffset },
                            0b1101 => Ble { soffset },

                            0b1110 => return None,

                            // Format 17
                            0b1111 => Swi { imm },

                            _ => unreachable!(),
                        }
                    }
                }
            }
            // Formats 18 and 19
            0b111 => {
                // Get the next flag
                let hi = ((data >> 11) & 1) == 1;
                let offset11 = data & 0x7ff;

                // Match the bits 12 and 11
                match ((data >> 12) & 1 == 1, hi) {
                    // Format 18
                    (false, false) => B { offset11 },

                    (false, true) => return None,

                    // Format 19
                    (true, _) => BlHalf { hi, offset11 },
                }
            }

            _ => unreachable!(),
        })
    }
}

/// Gets the `rn` register from the instruction data.
#[inline]
fn get_rn(data: u16) -> u8 {
    ((data >> 6) & 0b111) as u8
}

/// Gets the `rs` register from the instruction data.
#[inline]
fn get_rs(data: u16) -> u8 {
    ((data >> 3) & 0b111) as u8
}

/// Gets the `rd` register from the instruction data.
#[inline]
fn get_rd(data: u16) -> u8 {
    (data & 0b111) as u8
}

// Get the `imm5` from the format 1 and 9 instructions.
#[inline]
fn get_imm5(data: u16) -> u8 {
    ((data >> 6) & 0b11111) as u8
}
