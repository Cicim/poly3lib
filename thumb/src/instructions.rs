//! Thumb instructions.
//!
//! ARMv7TDMI has 19 different instruction formats in Thumb mode.

/// A Thumb instruction.
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
    //     000 11 O1 I1 imm3_or_Rn Rs Rd
    /// Add contents of Rn to contents of Rs. Place result in Rd.
    ///
    /// `ADD Rd, Rs, Rn` > `000` `11` `O=0` `I=0` **`Rn`** **`Rs`** **`Rd`**
    AddReg { rd: u8, rs: u8, rn: u8 },
    /// Add 3-bit immediate value to contents of Rs. Place result in Rd.
    ///
    /// `ADD Rd, Rs, #imm3` > `000` `11` `O=0` `I=1` **`imm3`** **`Rs`** **`Rd`**
    AddImm3 { rd: u8, rs: u8, imm3: u8 },
    /// Subtract contents of Rn from contents of Rs. Place result in Rd.
    ///
    /// `SUB Rd, Rs, Rn` > `000` `11` `O=1` `I=0` **`Rn`** **`Rs`** **`Rd`**
    SubReg { rd: u8, rs: u8, rn: u8 },
    /// Subtract 3-bit immediate value from contents of Rs. Place result in Rd.
    ///
    /// `SUB Rd, Rs, #imm3` > `000` `11` `O=1` `I=1` **`imm3`** **`Rs`** **`Rd`**
    SubImm3 { rd: u8, rs: u8, imm3: u8 },

    // ANCHOR 3 -- Move/compare/add/subtract immediate
    //     001 op2 rd imm8
    /// Move 8-bit immediate value into Rd.
    ///
    /// `MOV Rd, #imm8` > `001` `op=00` **`imm8`** **`Rd`**
    MovImm { rd: u8, imm8: u8 },
    /// Compare contents of Rd with 8-bit immediate value.
    ///
    /// `CMP Rd, #imm8` > `001` `op=01` **`imm8`** **`Rd`**
    CmpImm { rd: u8, imm8: u8 },
    /// Add 8-bit immediate value to contents of Rd and place the result in Rd.
    ///
    /// `ADD Rd, #imm8` > `001` `op=10` **`imm8`** **`Rd`**
    AddImm8 { rd: u8, imm8: u8 },
    /// Subtract 8-bit immediate value from contents of Rd and place the result in Rd.
    ///
    /// `SUB Rd, #imm8` > `001` `op=11` **`imm8`** **`Rd`**
    SubImm8 { rd: u8, imm8: u8 },

    // ANCHOR 4 -- ALU operations
    //     010000 op4 Rs Rd
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
    //     010001 op2 H1-flag H2-flag Rs/Hs Rd/Hd
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
    /// `BX Rs` > `010001` `op=11` `H1=0` `H2=1` **`Rs`** `Hd=0` `Hs=0`
    Bx { rs: u8 },
    /// Perform branch (plus optional state change) to address in a register in the range 8-15.
    ///
    /// `BX Hd` > `010001` `op=11` `H1=1` `H2=0` **`Hd`** `Hd=0` `Hs=0`
    BxHi { hd: u8 },

    // ANCHOR 6 -- PC-relative load
    //     01001 Rd Word8
    /// Add unsigned offset (255 words, 1020 bytes) in Imm to the current value of the PC.
    /// Load the word from the resulting address into Rd.
    ///
    /// `LDR Rd, [PC, #Imm]` > `01001` **`Rd`** **`Imm`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm8` is shifted right by 2.
    LdrPc { rd: u8, imm8: u8 },

    // ANCHOR 7 -- Load/store with register offset
    //     0101 L B 0 Ro Rn Rd
    //     L is 0 for store, 1 for load
    //     B is 0 for word, 1 for byte
    /// Pre-indexed word store: Calculate the target address by
    /// adding together the value in Rb and the value in Ro.
    /// Store the contents of Rd at the address.
    ///
    /// `STR Rd, [Rb, Ro]` > `0101` `L=0` `B=0` **`Ro`** **`Rb`** **`Rd`**
    StrReg { rb: u8, ro: u8, rd: u8 },
    /// Pre-indexed byte store: Calculate the target address by
    /// adding together the value in Rb and the value in Ro.
    /// Store the byte value in Rd at the resulting address.
    ///
    /// `STRB Rd, [Rb, Ro]` > `0101` `L=0` `B=1` **`Ro`** **`Rb`** **`Rd`**
    StrbReg { rb: u8, ro: u8, rd: u8 },
    /// Pre-indexed word load: Calculate the source address by
    /// adding together the value in Rb and the value in Ro.
    /// Load the contents of the address into Rd.
    ///
    /// `LDR Rd, [Rb, Ro]` > `0101` `L=1` `B=0` **`Ro`** **`Rb`** **`Rd`**
    LdrReg { rb: u8, ro: u8, rd: u8 },
    /// Pre-indexed byte load: Calculate the source address by
    /// adding together the value in Rb and the value in Ro.
    /// Load the byte value at the resulting address.
    ///
    /// `LDRB Rd, [Rb, Ro]` > `0101` `L=1` `B=1` **`Ro`** **`Rb`** **`Rd`**
    LdrbReg { rb: u8, ro: u8, rd: u8 },

    // ANCHOR 8 -- Load/store sign-extended byte/halfword
    //     0101 H S 1 Ro Rn Rd
    /// Store halfword: Add Ro to base address in Rb. Store bits 0-15 of Rd at the resulting address.
    ///
    /// `STRH Rd, [Rb, Ro]` > `0101` `H=0` `S=0` **`Ro`** **`Rb`** **`Rd`**
    StrhReg { rb: u8, ro: u8, rd: u8 },
    /// Load halfword: Add Ro to base address in Rb. Load bits 0-15 of Rd from the resulting address,
    /// and set bits 16-31 of Rd to 0.
    ///
    /// `LDRH Rd, [Rb, Ro]` > `0101` `H=1` `S=0` **`Ro`** **`Rb`** **`Rd`**
    LdrhReg { rb: u8, ro: u8, rd: u8 },
    /// Load sign-extended byte: Add Ro to base address in Rb. Load bits 0-7 of Rd from the resulting address,
    /// and set bits 8-31 of Rd to bit 7.
    ///
    /// `LDSB Rd, [Rb, Ro]` > `0101` `H=0` `S=1` **`Ro`** **`Rb`** **`Rd`**
    LdsbReg { rb: u8, ro: u8, rd: u8 },
    /// Load sign-extended halfword: Add Ro to base address in Rb. Load bits 0-15 of Rd from the resulting address,
    /// and set bits 16-31 of Rd to bit 15.
    ///
    /// `LDSH Rd, [Rb, Ro]` > `0101` `H=1` `S=1` **`Ro`** **`Rb`** **`Rd`**
    LdshReg { rb: u8, ro: u8, rd: u8 },

    // ANCHOR 9 -- Load/store with immediate offset
    //     011 B L Imm5 Rb Rd
    /// Calculate the target address by adding together the value in Rb and Imm.
    /// Store the contents of Rd at the address.
    ///
    /// `STR Rd, [Rb, #Imm]` > `011` `B=0` `L=0` **`Imm`** **`Rb`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm5` is shifted right by 2.
    StrImm { rb: u8, imm5: u8, rd: u8 },
    /// Calculate the source address by adding together the value in Rb and Imm.
    /// Load Rd from the address.
    ///
    /// `LDR Rd, [Rb, #Imm]` > `011` `B=0` `L=1` **`Imm`** **`Rb`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm5` is shifted right by 2.
    LdrImm { rb: u8, imm5: u8, rd: u8 },
    /// Calculate the target address by adding together the value in Rb and Imm.
    /// Store the byte value in Rd at the address.
    ///
    /// `STRB Rd, [Rb, #Imm]` > `011` `B=1` `L=0` **`Imm`** **`Rb`** **`Rd`**
    StrbImm { rb: u8, imm5: u8, rd: u8 },
    /// Calculate source address by adding together the value in Rb and Imm.
    /// Load the byte value at the address into Rd.
    ///
    /// `LDRB Rd, [Rb, #Imm]` > `011` `B=1` `L=1` **`Imm`** **`Rb`** **`Rd`**
    LdrbImm { rb: u8, imm5: u8, rd: u8 },

    // ANCHOR 10 -- Load/store halfword
    //     1000 L Imm5 Rb Rd
    /// Add #Imm to base address in Rb and store bits 0-15 of Rd at the resulting address.
    ///
    /// `STRH Rd, [Rb, #Imm]` > `1000` `L=0` **`Imm`** **`Rb`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 2, therefore `imm5` is shifted right by 1.
    StrhImm { rb: u8, imm5: u8, rd: u8 },
    /// Add #Imm to base address in Rb. Load bits 0-15 from the resulting address into Rd
    /// and set bits 16-31 to zero.
    ///
    /// `LDRH Rd, [Rb, #Imm]` > `1000` `L=1` **`Imm`** **`Rb`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 2, therefore `imm5` is shifted right by 1.
    LdrhImm { rb: u8, imm5: u8, rd: u8 },

    // ANCHOR 11 -- SP-relative load/store
    //     1001 L Rd Imm8
    /// Add unsigned offset (255 words, 1020 bytes) in Imm to the current value of the SP (R7).
    /// Store the contents of Rd at the resulting address.
    ///
    /// `STR Rd, [SP, #Imm]` > `1001` `L=0` **`Imm`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm8` is shifted right by 2.
    StrSpImm { imm8: u8, rd: u8 },
    /// Add unsigned offset (255 words, 1020 bytes) in Imm to the current value of the SP (R7).
    /// Load the word from the resulting address into Rd.
    ///
    /// `LDR Rd, [SP, #Imm]` > `1001` `L=1` **`Imm`** **`Rd`**
    ///
    /// **Note**: `#Imm` is a multiple of 4, therefore `imm8` is shifted right by 2.
    LdrSpImm { imm8: u8, rd: u8 },

    // ANCHOR 12 -- Load address
    //    1010 SP Rd Imm8
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
    //    1011 0000 S Imm7
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
    //    1011 L 10 R Rlist8
    //    L is 0 for push and 1 for pop
    //    R enables storing LR/loading PC
    /// Push the registers specified by Rlist onto the stack. Update the stack pointer.
    ///
    /// `PUSH { Rlist }` > `1011` `L=0` `10` `R=0` **`Rlist`**
    Push { r: u8, rlist: u8 },
    /// Push the Link Register and the registers specified by Rlist (if any)
    /// onto the stack. Update the stack pointer.
    ///
    /// `PUSH { Rlist, LR }` > `1011` `L=0` `10` `R=1` **`Rlist`**
    PushLr { r: u8, rlist: u8 },
    /// Pop values off the stack into the registers specified by Rlist. Update the stack pointer.
    ///
    /// `POP { Rlist }` > `1011` `L=1` `10` `R=0` **`Rlist`**
    Pop { r: u8, rlist: u8 },
    /// Pop values off the stack and load into the registers specified by Rlist.
    /// Pop the PC off the stack. Update the stack pointer.
    ///
    /// `POP { Rlist, PC }` > `1011` `L=1` `10` `R=1` **`Rlist`**
    PopPc { r: u8, rlist: u8 },

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
