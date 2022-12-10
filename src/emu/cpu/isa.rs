//! Instruction Set Architecture
//! 
//! ## Instruction Types
//! 
//! There are <TBD> types of instructions.
//! ### R-Type Instruction
//! | opcode(5) | Rd(4) | Rd(4) | ext(3) |
//! 
//! ### A-Type Instruction
//! | opcode(5) | Rd(4) |       A(7)     |
//! 
//! ### J-Type Instructions
//! | opcode(5) |        imm(11)         |
//! 
//! ### I-Type Instructions
//! | opcode(5) | Xn(3) |      imm(8)    |
//! 
//! ### B-Type Instructions
//! | opcode(5) | ext(2) |   imm(9)      |
//! 
//! ### E-Type Instructions
//! | opcode(5) | Rd(4) | Rs(4) | ext(3) |
//! |             imm(16)                |
//! 

// R-Type
pub const MASK_OP: u16 = 0xF800;
pub const MASK_RD: u16 = 0x780;
pub const MASK_RS: u16 = 0x78;
pub const MASK_EXT: u16 = 0x7;
pub const MASK_XD: u16 = 0x700;
pub const MASK_IMM7: u16 = 0x7F;
pub const MASK_IMM9: u16 = 0x1FF;
pub const MASK_IMM11: u16 = 0x7FF;
pub const MASK_EXT2: u16 = 0x600;

//use u16 as code_t;
pub type code_t = u16;


// pub fn decode_r(instr: code_t ) -> (u16, u16, u16, u16) {
//     let opcode = (instr & MASK_OP) >> 11;
//     let rd = (instr & MASK_RD) >> 7;
//     let rs = (instr & MASK_RS) >> 3;
//     let ext = instr & MASK_EXT;
//     (opcode,rd,rs,ext)
// }
// pub fn decode_a(instr: code_t) -> (u16, u16, u16) {
//     let opcode = (instr & MASK_OP) >> 11;
//     let rd = (instr & crate::emu::MASK_RD) >> 7;
//     let imm7 = instr & MASK_IMM7;
//     (opcode,rd,imm7)    
// }
// pub fn decode_j(instr: code_t) -> (u16, u16) {
//     let opcode = (instr & MASK_OP) >> 11;
//     let imm11 = instr & MASK_IMM11;
//     (opcode,imm11)
// }
// pub fn decode_i(instr: u16) -> (u16, u16, u16) {
//     let opcode = (instr & MASK_OP) >> 11;
//     let xd = (instr & MASK_XD) >> 8;
//     let imm8: u16 = instr & 0xFF;
//     (opcode,xd,imm8)
// }
// pub fn decode_b(instr: u16) -> (u16, u16, u16) {
//     let opcode = (instr & MASK_OP) >> 11;
//     let ext2 = (instr & MASK_EXT2) >> 9;
//     let imm9: u16 = instr & MASK_IMM9;
//     (opcode,ext2,imm9)

// }
// fn decode_e(&self, instr: code_t) -> (u16, u16, u16, u16) {
//     let (opcode,rd,rs,ext) = decode_r(instr);
//     let imm16 = self.fetch();
//     (opcode,rd,rs,ext,imm16)
// }
mod op {
    pub enum code {
        OG0 = 0x00, // opcode group 0 + ext
        OG1 = 0x01, // opcode group 1 + ext
        OG2 = 0x02, // opcode group 2 + ext
        OG3 = 0x03, // opcode group 3 + ext
    }
}

#[derive(Debug)]
pub enum InstructionType { R, E, I }

use std::collections::HashMap;

use super::register::Register;
use crate::types::Value;
use u8 as K;
use u16 as imm11;

#[derive(Debug)]
pub enum InstructionSet {
    ADD(Register, Register),
    ADDI(Register, Value),
    AND(Register, Register),
    ASR(Register),

    BCLR(u8),
    BREQ(imm11),
    BRGE(imm11),
    BRK,
    BRLO(imm11),
    BRLT(imm11),
    BRNE(imm11),
    BRSH(imm11),
    BSET(u8),

    CALL(imm11),
    CBIO(u8, u8),
    CBR(Register, Value),
    CLR(Register),
    COM(Register),
    CPI(Register, K),
    CPSE(Register, Register),

    DEC(Register),
    DIV(Register, Register),

    HALT,

    IJMP,
    IN(Register, u8),
    INC(Register),

    JMP(imm11),

    LDD(Register, Value),         // Load direct f/ data space
    LDI(Register, Value),           // Load immediate
    LSL(Register),              // Logical Shift Left
    LSR(Register),              // Logical Shift Right

    MOV(Register, Register),
    MUL(Register, Register),

    NEG(Register),
    NOP,                        // pseudocode ADDI R0, 0

    OR(Register, Register),
    ORI(Register, K),
    OUT(u8, Register),

    POP(Register),
    PUSH(Register),

    RCALL(imm11),
    RET,
    RJMP(imm11),

    SBIO(u8, u8),
    SBR(Register, K),
    SET(Register),
    SUB(Register, Register),
    SUBI(Register, K),

    XOR(Register, Register),
 

}
pub trait ISA {
    // the type representing an instruction
    type Item;

    fn version(&self) -> &str;
    fn decode(&self, instr: code_t);
    fn get_opcode(&self, instr: code_t) -> code_t;

    // these are the operations of the ISA
    fn halt(&self, args: &[code_t]);
    fn brk(&self, args: &[code_t]);

    fn add(&mut self, args: &[code_t]);
    fn addi(&mut self, args: &[code_t]);
    fn and(&mut self, args: &[code_t]);
    fn cbr(&mut self, args: &[code_t]);
    fn clr(&mut self, args: &[code_t]);
    fn com(&mut self, args: &[code_t]);
    fn dec(&mut self, args: &[code_t]);
    fn div(&mut self, args: &[code_t]);
    fn inc(&mut self, args: &[code_t]);
    fn ldd(&mut self, args: &[code_t]);
    fn ldi(&mut self, args: &[code_t]);
    fn mov(&mut self, args: &[code_t]);
    fn mul(&mut self, args: &[code_t]);

    fn or(&mut self, args: &[code_t]);
    fn ori(&mut self, args: &[code_t]);

    fn push(&mut self, args: &[code_t]);
    fn pop(&mut self, args: &[code_t]);
    fn sub(&mut self, args: &[code_t]);
    fn subi(&mut self, args: &[code_t]);

    fn xor(&mut self, args: &[code_t]);

    // pseudocodes
    fn nop(&mut self, args: &[code_t]);

}

#[derive(Debug)]
pub struct InstructionRecord {
    pub mnemonic: &'static str,     // "NOOP"
    pub opcode: u8,                 // 0x00
    pub ext: Option<u8>,            // Some(0)
    pub operands: Option<&'static str>,     // None
    pub operation: &'static str,    // ""
    pub flags: Option<&'static str>, // "ZCNVS"
    pub description: &'static str,  // No operation
    pub format: InstructionType,    // R
}

impl InstructionRecord {
    pub fn new() -> InstructionRecord {
        InstructionRecord { mnemonic: "", opcode: 0, ext: None, operands: None, operation: "", flags: None, description: "No operation. Ticks forward a clock cycle.", format: InstructionType::R }
    }
}

#[derive(Debug, Default)]
pub struct InstructionTable(HashMap<u8, InstructionRecord>);

/// Instruction Table
/// 
/// Stores the machine instructions and allows them to be retrieved by
/// name or opcode. Implemented as a `HashMap` behind the scenes.
impl InstructionTable {
    pub fn new() -> InstructionTable {
        InstructionTable(HashMap::new())
    }
    pub fn by_opcode(&self, opcode: u8) -> Option<&InstructionRecord> {
        self.0.get(&opcode)
    }
    pub fn by_name(&self, name: &str) -> Option<&InstructionRecord> {
        self.0.values().find(|instr| instr.mnemonic == name)
    }
    pub fn insert(&mut self, instr: InstructionRecord) {
        self.0.insert(instr.opcode, instr);
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn symbols(&self) -> Vec<(u8, &str)> {
        let mut result = vec![];
        self.0.keys().for_each(|key| {
            let instr = &self.0[key];
            result.push((instr.opcode, instr.mnemonic.clone()));
        });
        result.sort_by(|lhs, rhs| lhs.0.cmp(&rhs.0));
        result
    }
}
