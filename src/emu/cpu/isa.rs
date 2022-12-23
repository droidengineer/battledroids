//! Instruction Set Architecture
//! 
//! ## Instruction Types
//! 
//! There are <TBD> types of instructions.
//! | opcode(8) | Rd(4) | Rs(4) |
//! |           imm(16)         |
//! ### R-Type Instruction
//! ...or
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

use crate::types::{*, to_byte_code::ToByteCode, self};
use num_traits::{FromPrimitive, ToPrimitive};
use regex::Regex;
use rmp::encode;

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

#[inline(always)]
pub fn decode_a(instr: code_t) -> (u16,u16,u16) {
    print!("decode_a> {:0>16b} ", instr);
    let opcode = (instr & 0xFF00) >> 8;
    let rd = (instr & 0xF0) >> 4;
    let rs = instr & 0xF;
    println!(" => ({opcode}, {rd}, {rs})");
    (opcode,rd,rs)
}
#[inline(always)]
pub fn encode_a(op: u16, op1: u16, op2: u16) -> code_t {
    print!("encode_a> op: {op} op1: {op1} op2: {op2} ");
    let code = (op & 0x00FF) << 8 | (op1 & 0x000F) << 4 | (op2 & 0x000F);
    println!(" => {:b}", code);
    code
}
#[inline(always)]
pub fn decode_e(instr: u32) -> (u16,u16,u16,u16) {
    print!("decode_e> {:0>32b} ", instr);
    let opa = ((instr & 0xFFFF0000) >> 16) as u16;
    let imm = (instr & 0x0000FFFF) as u16;
    let (op,rd,rs) = decode_a(opa);
    println!(" => ({op}, {rd}, {rs}, {imm})");
    (op, rd, rs, imm)
}
#[inline(always)]
pub fn encode_e(op: u16, op1: u16, imm: u16) -> u32 {
    print!("encode_e> op: {op} op1: {op1} op2: {imm} ");
    let opa: u32 = encode_a(op,op1,0) as u32;
    let code = (opa & 0x0000FFFF) << 16 | (imm as u32);
    println!(" => {:b}",code);
    code
}

// pub fn decode_r(instr: code_t ) -> (u16, u16, u16, u16) {
//     let opcode = (instr & MASK_OP) >> 11;
//     let rd = (instr & MASK_RD) >> 7;
//     let rs = (instr & MASK_RS) >> 3;
//     let ext = instr & MASK_EXT;
//     (opcode,rd,rs,ext)
// }
// #[inline(always)]
// pub fn decode_a(instr: code_t) -> (u16, u16, u16) {
//     let opcode = (instr & MASK_OP) >> 11;
//     let rd = (instr & MASK_RD) >> 7;
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
// fn decode_e(instr: code_t) -> (u16, u16, u16, u16, u16) {
//     let (opcode,rd,rs,ext) = decode_r(instr);
//    let imm16 = 0; //self.fetch();
//     (opcode,rd,rs,ext,imm16)
// }
pub mod op {
    use std::{str::FromStr, fmt::Error};

    use enum_primitive::FromPrimitive;

    use crate::{emu::{Register, isa::{valid_u16, valid_u8}}, types};

    use super::Instruction;

enum_from_primitive! {
    #[derive(Debug,Copy,Clone, PartialEq, Eq, Hash)]
    #[repr(u8)]
    pub enum code {
        UNDEF,      
        ADD = 1,
        ADDI,
        AND,
        ASR,
        BCLR,
        BREQ,BRGE,
        BRK,
        BRLO,
        BRLT,
        BRNE,
        BRSH,
        BSET,
        CALL,
        CBIO,
        CBR,
        CLR,
        COM,
        CPI,
        CPSE,
        DEC,
        DIV,
        HALT,
        IJMP,
        IN,
        INC,
        JMP,
        LD, LDD, LDI, LDP,
        LSL, LSR,
        MOV,
        MUL,
        NEG,
        NOP,
        OR,
        ORI,
        OUT,
        POP, PUSH,
        RCALL,
        RET,
        RJMP,
        SBIO, SBR, SET,
        ST, STD, STP,
        SUB, SUBI,
        XOR,

    }
}
    impl code {
        pub fn to_instruction(&self, args: &[u16]) -> Instruction {
            todo!()
        }
        pub fn str_to_instruction(&self, args: &Vec<&str>) -> Instruction {
            println!("{:?}::to_instruction({:?})",self, args);

            match self {
                code::ADD   => Instruction::ADD(Register::from(args[1]), Register::from(args[2])),
                code::ADDI  => Instruction::ADDI(Register::from(args[1]), valid_u16(args[2])),
                code::AND   => Instruction::AND(Register::from(args[1]), Register::from(args[2])),
                code::ASR   => Instruction::ASR(Register::from(args[1])),
                code::BRK   => Instruction::BRK,
                code::BCLR  => Instruction::BCLR(valid_u8(args[1])),
                code::BREQ  => Instruction::BREQ(valid_u16(args[1])),
                code::BRGE  => Instruction::BRGE(valid_u16(args[1])),
                code::BRLO  => Instruction::BRLO(valid_u16(args[1])),
                code::BRLT  => Instruction::BRLT(valid_u16(args[1])),
                code::BRNE  => Instruction::BRNE(valid_u16(args[1])),
                code::BRSH  => Instruction::BRSH(valid_u16(args[1])),
                code::BSET  => Instruction::BSET(valid_u8(args[1])),
                code::CALL  => Instruction::CALL(valid_u16(args[1])),
                code::CBIO  => Instruction::CBIO(valid_u16(args[1]), valid_u8(args[2])),
                code::CBR   => Instruction::CBR(Register::from(args[1]), valid_u8(args[2])),
                code::CLR   => Instruction::CLR(Register::from(args[1])),
                code::COM   => Instruction::COM(Register::from(args[1])),
                code::CPI   => Instruction::CPI(Register::from(args[1]), valid_u16(args[2])),
                code::CPSE  => Instruction::CPSE(Register::from(args[1]), Register::from(args[2])),
                code::DEC   => Instruction::DEC(Register::from(args[1])),
                code::DIV   => Instruction::DIV(Register::from(args[1]), Register::from(args[2])),
                code::HALT  => Instruction::HALT,
                code::IJMP  => Instruction::IJMP,
                code::IN    => Instruction::IN(Register::from(args[1]), valid_u8(args[2])),
                code::INC   => Instruction::INC(Register::from(args[1])),
                code::JMP   => Instruction::JMP(valid_u16(args[1])),
                code::LD    => Instruction::LD(Register::from(args[1]), Register::from(args[2])),
                code::LDD   => Instruction::LDD(Register::from(args[1]), valid_u16(args[2])),
                code::LDI   => Instruction::LDI(Register::from(args[1]), valid_u16(args[2])),
                code::LDP   => Instruction::LDP(Register::from(args[1]), valid_u16(args[2])),
                code::LSL   => Instruction::LSL(Register::from(args[1])),
                code::LSR   => Instruction::LSR(Register::from(args[1])),
                code::MOV   => Instruction::MOV(Register::from(args[1]), Register::from(args[2])),
                code::MUL   => Instruction::MUL(Register::from(args[1]), Register::from(args[2])),
                code::NEG   => Instruction::NEG(Register::from(args[1])),
                code::NOP   => Instruction::NOP,
                code::OR    => Instruction::OR(Register::from(args[1]), Register::from(args[2])),
                code::ORI   => Instruction::ORI(Register::from(args[1]), valid_u16(args[2])),
                code::OUT   => Instruction::OUT(valid_u8(args[2]), Register::from(args[1])),
                code::POP   => Instruction::POP(Register::from(args[1])),
                code::PUSH  => Instruction::PUSH(Register::from(args[1])),
                code::RCALL => Instruction::RCALL(valid_u16(args[1])),
                code::RET   => Instruction::RET,
                code::RJMP  => Instruction::RJMP(valid_u16(args[1])),
                code::SBIO  => Instruction::SBIO(valid_u16(args[1]), valid_u8(args[2])),
                code::SBR   => Instruction::SBR(Register::from(args[1]), valid_u8(args[2])),
                code::SET   => Instruction::SET(Register::from(args[1])),
                code::ST    => Instruction::ST(Register::from(args[1]), Register::from(args[2])),
                code::STD   => Instruction::STD(valid_u16(args[1]), Register::from(args[2])),
                code::STP   => Instruction::STP(valid_u16(args[1]), Register::from(args[2])),
                code::SUB   => Instruction::SUB(Register::from(args[1]), Register::from(args[2])),
                code::SUBI  => Instruction::SUBI(Register::from(args[1]), valid_u16(args[2])),
                code::UNDEF => Instruction::UnknownOp(args[0].parse::<u16>().unwrap()),
                code::XOR   => Instruction::XOR(Register::from(args[1]), Register::from(args[2])),

                _ => Instruction::UnknownOp(args[0].parse::<u16>().unwrap()),
            }

        }
        pub fn encode(&self) -> u8 {
            *self as u8
        }
        pub fn to_str(&self) -> &str {
            match *self {
                code::ADD => "ADD",
                code::ADDI => "ADDI",
                code::AND => "AND",
                code::ASR => "ASR",
                code::BCLR => "BCLR",
                code::BREQ => "BREQ",
                code::BRGE => "BRGE",
                code::BRK => "BRK",

                _ => "<TBD>"
            }
        }
    
        pub fn need_word(&self) -> bool {
            match *self {
                code::BREQ | code::BRGE | code::BRLO |
                code::BRLT | code::BRNE | code::BRSH |
                code::CALL | code::CPI | code::JMP | code::LDD |
                code::LDI | code::LDP | code::ORI |
                code::RCALL | code::RJMP | code::STD |
                code::STP | code::SUBI | code::ADDI
                    => true,
                _   => false
            }
        }
    }

    impl From<u16> for code {
        fn from(t: u16) -> Self {
           code::from_u16(t).unwrap()
        }
    }
    impl FromStr for code {
        type Err = Error;
        fn from_str(s: &str) -> Result<code, Error> {
                match s.to_uppercase().as_str() {
                "ADD"   => Ok(code::ADD),
                "ADDI"  => Ok(code::ADDI),
                "AND"   => Ok(code::AND),
                "ASR"   => Ok(code::ASR),
                "BCLR"  => Ok(code::BCLR),
                "BREQ"  => Ok(code::BREQ),
                "BRGE"  => Ok(code::BRGE),
                "BRK"  => Ok(code::BRK),
                "BRLO"  => Ok(code::BRLO),
                "BRLT"  => Ok(code::BRLT),
                "BRNE"  => Ok(code::BRNE),
                "BRSH"  => Ok(code::BRSH),
                "BSET"  => Ok(code::BSET),
                "CALL"  => Ok(code::CALL),
                "CBIO"  => Ok(code::CBIO),
                "CBR"   => Ok(code::CBR),
                "CLR"   => Ok(code::CLR),
                "COM"   => Ok(code::COM),
                "CPI"   => Ok(code::CPI),
                "CPSE"   => Ok(code::CPSE),
                "DEC"  => Ok(code::DEC),
                "DIV"  => Ok(code::DIV),
                "HALT"  => Ok(code::HALT),
                "IJMP"  => Ok(code::IJMP),
                "IN"  => Ok(code::IN),
                "INC"  => Ok(code::INC),
                "JMP"  => Ok(code::JMP),
                "LD"  => Ok(code::LD),
                "LDD"  => Ok(code::LDD),
                "LDI"  => Ok(code::LDI),
                "LDP"  => Ok(code::LDP),
                "LSL"  => Ok(code::LSL),
                "LSR"  => Ok(code::LSR),
                "MOV"  => Ok(code::MOV),
                "MUL"  => Ok(code::MUL),
                "NEG"  => Ok(code::NEG),
                "NOP"  => Ok(code::NOP),
                "OR"  => Ok(code::OR),
                "ORI"  => Ok(code::ORI),
                "OUT"  => Ok(code::OUT),
                "POP"  => Ok(code::POP),
                "PUSH"  => Ok(code::PUSH),
                "RCALL"  => Ok(code::RCALL),
                "RET"  => Ok(code::RET),
                "RJMP"  => Ok(code::RJMP),
                "SBIO"  => Ok(code::SBIO),
                "SBR"  => Ok(code::SBR),
                "SET"  => Ok(code::SET),
                "ST"  => Ok(code::ST),
                "STD"  => Ok(code::STD),
                "STP"  => Ok(code::STP),
                "SUB"  => Ok(code::SUB),
                "SUBI"  => Ok(code::SUBI),
                "XOR"  => Ok(code::XOR),

                &_ => Ok(code::UNDEF)
            }
        }
    }
    // impl From<code> for u8 {
    //     fn from(c: code) -> Self {
    //        c.to_u16().unwrap()
    //     }
    // }
    impl ToString for code {
        fn to_string(&self) -> String {
            match *self {
                code::ADD     => format!("ADD"),
                code::ADDI    => "ADDI".to_owned(),
                code::AND     => "AND".to_string(),
                code::ASR     => format!("ASR"),
                code::BCLR    => format!("BCLR"),
            
                _ => format!("<{:?}> not implemented",self)
            }
        }
    }
}

#[derive(Debug)]
pub enum InstructionType { A, R, E, I }

use std::{collections::HashMap, fmt::Error};

use super::register::Register;
use u8 as bit;
use u16 as imm11;
use op::code;

#[derive(Debug,Copy,Clone,PartialEq,Eq,Hash)]
pub enum Instruction {
    UnknownOp(u16),
    SecondOpWord,

    /// *add rd, rs* |`rd = rd + rs`| **Add** 
    ADD(Register, Register),
    /// *addi rd, imm11* |`rd = rd + imm11`| **Add Immediate**
    ADDI(Register, u16),
    /// *and rd, rs* reg(|`rd = rd & rs`| **Local AND**
    AND(Register, Register),
    ASR(Register),
    /// *bclr bit* |`SREG &= !(1<<bit)`| **Clear bit in `SREG`**
    BCLR(u8),
    BREQ(u16),
    BRGE(u16),
    BRK,
    BRLO(imm11),
    BRLT(imm11),
    BRNE(imm11),
    BRSH(imm11),
    /// *bset bit* |`SREG |= 1<<bit`| **Set bit in `SREG`**
    BSET(u8),
/// Performs a call to a named label/addr
/// 
/// This method is similar to `JMP` except that it records its
/// current instruction pointer and pushes it onto the stack 
/// before jumping to the location.
    CALL(imm11),
    /// *cbio reg_io, bit* |`reg_io = reg_io | (0<<bit)`| **Clear Bit In I/O Register**
    CBIO(u16, bit),
    /// *cbr rd, bit* |`rd &= !1<<bit`| **Clear bit in register
    CBR(Register, bit),
    /// *clr rd* |`SREG <TBD>`
    CLR(Register),
    /// *com rd* |`Rd = $FF - Rd`| **One's complement**
    COM(Register),
    CPI(Register, u16),
    CPSE(Register, Register),
    /// *dec rd* |`Rd = Rd - 1`| **Decrement**
    DEC(Register),
    /// *div rd, rs* |`Rd = Rd ÷ Rs`| **Divide**
    DIV(Register, Register),

    HALT,

    IJMP,
    IN(Register, u8),
    /// *inc rd* |`Rd = Rd + 1`| **Increment**
    INC(Register),
    /// *jmp k* |`PC = k`| **Jump to address**
    JMP(imm11),

    /// *ld rd, rs* |`Rd = data[Rs]`| **Load indirect f/ data space**
    LD(Register, Register),
    /// *ldd rd, k* |`Rd = data[k]`| **Load direct f/ data space**
    LDD(Register, u16),
    /// *ldi rd, k* |`Rd = k`| **Load immediate**
    LDI(Register, u16),
    /// *ldp rd, k* |`Rd = program[k]`| **Load immediate f/ progmem**
    LDP(Register, u16),
    LSL(Register),              // Logical Shift Left
    LSR(Register),              // Logical Shift Right
    /// *mov rd, rs* |`Rd = Rs`| **Copy registers**
    MOV(Register, Register),
    /// *mul rd, rs* |`Rd = Rd ⨯ Rs`| **Multiply**
    MUL(Register, Register),
    /// *neg rd* |`Rd = ~Rd`| **Two's complement**
    NEG(Register),
    NOP,                        // pseudocode ADDI R0, 0
    /// *or rd, rs* |`Rd = Rd v Rs`| **Logical OR**
    OR(Register, Register),
    /// *ori rd, imm11* |`Rd = Rd v imm11`| **Logical OR w/ immediate**    
    ORI(Register, u16),
    OUT(u8, Register),

    POP(Register),
    PUSH(Register),

    RCALL(imm11),
/// Performs a return.
/// 
/// This instruction pops the value from the stack representing the return address and moves the instruction
/// pointer (IP) to the value. If there is a return value it must be pushed onto the stack by the callee.
    RET,
    RJMP(imm11),
    /// *sbio reg_io, bit* |`reg_io = reg_io | (1<<bit)`| **Set Bit(s) in IO Register**
    SBIO(register_t, bit),
    /// *sbr rd, bit* |`Rd[bit] = 1`| **Set bit in register**
    SBR(Register, bit),
    SET(Register),
    /// store indirect to data mem
    ST(Register, Register),     
    /// *stp imm, rd* |`progmem[Rd] = imm`| **Store directo to Data Memory**
    STD(u16, Register),
    /// *stp imm, rd* |`progmem[Rd] = imm`| **Store direct to Program Memory**
    STP(u16, Register),
    /// *sub rd, rs* |`Rd = Rd - Rs`| **Subtract**
    SUB(Register, Register),
    /// *subi rd, imm11* |`Rd = Rd - imm11`| **Subtract w/ immediate**
    SUBI(Register, u16),
    /// *xor rd, rs* |`Rd = Rd ⊕ Rs`| **Logical Exclusive OR**
    XOR(Register, Register),
}
impl Instruction {

    pub fn get_args(&self) -> (u16, u16) {
        match *self {
            Instruction::ADD(rd,rs) => (rd as u16,rs as u16),
            Instruction::ADDI(rd,imm) => (rd as u16,imm),
            Instruction::AND(rd,rs) => (rd as u16,rs as u16),
            Instruction::ASR(rd) => (rd as u16,0),
            Instruction::BCLR(bit) => (bit as u16,0),
            Instruction::BREQ(imm) => (imm,0),
            Instruction::BRGE(imm) => (imm,0),
            Instruction::BRK => (0,0),
            Instruction::BRLO(imm) => (imm,0),
            Instruction::BRLT(imm) => (imm,0),
            Instruction::BRNE(imm) => (imm,0),
            Instruction::BRSH(imm) => (imm,0),
            Instruction::BSET(bit) => (bit as u16,0),
            Instruction::CALL(imm) => (imm,0),
            Instruction::CBIO(io_reg,bit) => (io_reg,bit as u16),


            _ => (0,0)
        }
   }
//     pub fn encode(&self) -> code_t {
//         trace!("Instruction::encode()");
//         match *self {
//             Self::ADD(rd, rs) => 1,
//             _ => 0,
//         }
//    }
   pub fn get_opcode(&self) -> op::code {
    match self {
        UnknownOp(_) => code::UNDEF,
        SecondOpWord => todo!(),
        ADD(_, _) => code::ADD,
        ADDI(_, _) => code::ADDI,
        AND(_, _) => code::AND,
        ASR(_) => code::ASR,
        BCLR(_) => code::BCLR,
        BREQ(_) => code::BREQ,
        BRGE(_) => code::BRGE,
        BRK => code::BRK,
        BRLO(_) => code::BRLO,
        BRLT(_) => code::BRLT,
        BRNE(_) => code::BRNE,
        BRSH(_) => code::BRSH,
        BSET(_) => code::BSET,
        CALL(_) => code::CALL,
        CBIO(_, _) => code::CBIO,
        CBR(_, _) => code::CBR,
        CLR(_) => code::CLR,
        COM(_) => code::COM,
        CPI(_, _) => code::CPI,
        CPSE(_, _) => code::CPSE,
        DEC(_) => code::DEC,
        DIV(_, _) => code::DIV,
        HALT => code::HALT,
        IJMP => code::IJMP,
        IN(_, _) => code::IN,
        INC(_) => code::INC,
        JMP(_) => code::JMP,
        LD(_, _) => code::LD,
        LDD(_, _) => code::LDD,
        LDI(_, _) => code::LDI,
        LDP(_, _) => code::LDP,
        LSL(_) => code::LSL,
        LSR(_) => code::LSR,
        MOV(_, _) => code::MOV,
        MUL(_, _) => code::MUL,
        NEG(_) => code::NEG,
        NOP => code::NOP,
        OR(_, _) => code::OR,
        ORI(_, _) => code::ORI,
        OUT(_, _) => code::OUT,
        POP(_) => code::POP,
        PUSH(_) => code::PUSH,
        RCALL(_) => code::RCALL,
        RET => code::RET,
        RJMP(_) => code::RJMP,
        SBIO(_, _) => code::SBIO,
        SBR(_, _) => code::SBR,
        SET(_) => code::SET,
        ST(_, _) => code::ST,
        STD(_, _) => code::STD,
        STP(_, _) => code::STP,
        SUB(_, _) => code::SUB,
        SUBI(_, _) => code::SUBI,
        XOR(_, _) => code::XOR,
    }
   }
}
/// 
impl ToByteCode for Instruction {
    fn to_byte_code(&self, mut buf: &mut dyn std::io::Write) {
        let (rd,rs) = self.get_args();
        encode::write_u16(&mut buf, encode_a(self.get_opcode() as u16, rd, rs));
    }
}
impl From<Instruction> for u16 {
    fn from(i: Instruction) -> Self {
        
        match i {
            UnknownOp(_) => todo!(),
            SecondOpWord => todo!(),
            ADD(rd_, rs) => todo!(),
            ADDI(rd, i) => todo!(),
            AND(rd, rs) => todo!(),
            ASR(rd) => todo!(),
            BCLR(b) => todo!(),
            BREQ(i) => todo!(),
            BRGE(i) => todo!(),
            BRK => todo!(),
            BRLO(i) => todo!(),
            BRLT(i) => todo!(),
            BRNE(i) => todo!(),
            BRSH(i) => todo!(),
            BSET(b) => todo!(),
            CALL(i) => todo!(),
            CBIO(i, b) => todo!(),
            CBR(rd, b) => todo!(),
            CLR(rd) => todo!(),
            COM(rd) => todo!(),
            CPI(rd, i) => todo!(),
            CPSE(rd, rs) => todo!(),
            DEC(rd) => todo!(),
            DIV(rd, rs) => todo!(),
            HALT => todo!(),
            IJMP => todo!(),
            IN(_, _) => todo!(),
            INC(rd) => todo!(),
            JMP(i) => todo!(),
            LD(rd, rs) => todo!(),
            LDD(rd, i) => todo!(),
            LDI(rd, i) => todo!(),
            LDP(rd, i) => todo!(),
            LSL(rd) => todo!(),
            LSR(rd) => todo!(),
            MOV(rd, rs) => todo!(),
            MUL(rd_, rs) => todo!(),
            NEG(rd) => todo!(),
            NOP => todo!(),
            OR(rd, rs) => todo!(),
            ORI(rd, i) => todo!(),
            OUT(io, rd) => todo!(),
            POP(rd) => todo!(),
            PUSH(rd) => todo!(),
            RCALL(i) => todo!(),
            RET => todo!(),
            RJMP(i) => todo!(),
            SBIO(i, b) => todo!(),
            SBR(rd, b) => todo!(),
            SET(rd) => todo!(),
            ST(rd, rs) => todo!(),
            STD(i, rd) => todo!(),
            STP(i, rd) => todo!(),
            SUB(rd, rs) => todo!(),
            SUBI(rd, i) => todo!(),
            XOR(rd, rs) => todo!(),
        }
    }
}

impl From<u16> for Instruction {
    fn from(i: u16) -> Self {
        let (op, rd, rs) = decode_a(i);
        println!("From<u16>: op({op}) rd({rd}) rs({rs})");
        let opcode = op::code::from(op);

        match opcode {
            code::ADD => Instruction::ADD(Register::from(rd), Register::from(rs)),
            code::AND => Instruction::AND(Register::from(rd), Register::from(rs)),         
            code::ASR => Instruction::ASR(Register::from(rd)),               
            code::BCLR => Instruction::BCLR(rd as u8),
            code::BRK => Instruction::BRK,
            code::BSET => Instruction::BSET(rd as u8),     
            code::CBR => Instruction::CBR(Register::from(rd),rs as u8),
            code::CLR => Instruction::CLR(Register::from(rd)),
            code::COM => Instruction::COM(Register::from(rd)),
            code::CPSE => Instruction::CPSE(Register::from(rd), Register::from(rs)),
            code::DEC => Instruction::DEC(Register::from(rd)),
            code::DIV => Instruction::DIV(Register::from(rd), Register::from(rs)),
            code::HALT => Instruction::HALT,
            code::IJMP => Instruction::IJMP,
            code::INC => Instruction::INC(Register::from(rd)),
            code::LD => Instruction::LD(Register::from(rd), Register::from(rs)),
            code::LSL => Instruction::LSL(Register::from(rd)),
            code::LSR => Instruction::LSR(Register::from(rd)),
            code::MOV => Instruction::MOV(Register::from(rd), Register::from(rs)),
            code::MUL => Instruction::MUL(Register::from(rd), Register::from(rs)),
            code::NEG => Instruction::NEG(Register::from(rd)),
            code::NOP => Instruction::NOP,
            code::OR => Instruction::OR(Register::from(rd), Register::from(rs)),
            code::POP => Instruction::POP(Register::from(rd)),
            code::PUSH => Instruction::PUSH(Register::from(rd)),
            code::RET => Instruction::RET,
            code::SBR => Instruction::SBR(Register::from(rd), rs as u8),
            code::SET => Instruction::SET(Register::from(rd)),
            code::ST => Instruction::ST(Register::from(rd), Register::from(rs)),
            code::SUB => Instruction::SUB(Register::from(rd), Register::from(rs)),
            code::XOR => Instruction::XOR(Register::from(rd), Register::from(rs)),
            
            _ => Instruction::UnknownOp(i)
        }
    }  
}
impl From<u32> for Instruction {
    fn from(i: u32) -> Self {
        // let instr = i as u16;
        // let imm = (i >> 16) as u16;
        // let (op, rd, rs) = decode_a(instr);
        let (op, rd, rs, imm) = decode_e(i);
        let opcode = op::code::from(op);
        println!("From<u32>: op({op}) rd({rd}) rs({rs}) imm({imm})");

        match opcode {
            code::ADDI => Instruction::ADDI(Register::from(rd), imm),
            code::BREQ => Instruction::BREQ(imm),
            code::BRGE => Instruction::BRGE(imm),
            code::BRSH => Instruction::BRSH(imm),
            code::BRLO => Instruction::BRLO(imm),
            code::BRLT => Instruction::BRLT(imm),
            code::BRNE => Instruction::BRNE(imm),
            code::CALL => Instruction::CALL(imm),
            code::CBIO => Instruction::CBIO(imm, rd as u8),
            code::CPI  => Instruction::CPI(Register::from(rd),imm),
            code::IN => Instruction::IN(Register::from(rd), imm as u8),
            code::JMP => Instruction::JMP(imm),
            code::LDD => Instruction::LDD(Register::from(rd),imm),
            code::LDI => Instruction::LDI(Register::from(rd),imm),
            code::LDP => Instruction::LDP(Register::from(rd),imm),
            code::OUT => Instruction::OUT(imm as u8, Register::from(rd)),
            code::ORI => Instruction::ORI(Register::from(rd),imm),
            code::RCALL => Instruction::RCALL(imm),
            code::RJMP => Instruction::RJMP(imm),
            code::SBIO => Instruction::SBIO(imm, rd as u8),
            code::STD => Instruction::STD(imm,Register::from(rd)),
            code::STP => Instruction::STP(imm,Register::from(rd)),
            code::SUBI => Instruction::SUBI(Register::from(rd), imm),

           // _ => Instruction::UnknownOp(op)
           _ => Instruction::from((i >> 16) as u16)
        }
    }
}

use Instruction::*;
#[derive(Debug)]
pub enum ParseInstructionError {
    Format,
    ParseIntError(::std::num::ParseIntError),
}
impl ::std::fmt::Display for ParseInstructionError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Debug::fmt(self,f)
    }   
}
impl ::std::str::FromStr for Instruction {
    type Err = ParseInstructionError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let sep = Regex::new(r"([ ,]+)").expect("Invalid regex");
        let splits: Vec<_> = sep.split(s).into_iter().collect();
//        let mut parts = s.split(|c| c == ' ' || c == ',');
//        trace!("[TRAIT] from_str({}): {:?}",s,parts);
        trace!("Args: {}",splits.len()); 
        let opcode = op::code::from_str(splits[0]);
        let instr = opcode.unwrap().str_to_instruction(&splits);

        Ok(instr)
    }
}
// impl From<&str> for Instruction {
//     fn from(s: &str) -> Self {
//         match s {
//             "ADD" => ADD(Register::R0, Register::R1),

//             _ => Instruction::UnknownOp(0)
//         }
//     }
// }

impl ToString for Instruction {
    fn to_string(&self) -> String {
        match *self {
            Instruction::ADD(rd,rs) => format!("ADD {:?}, {:?}",rd,rs),
            Instruction::ADDI(rd,imm) => format!("ADDI {:?}, {imm}",rd),
            Instruction::AND(rd,rs) => format!("AND {:?}, {:?}",rd,rs),
            Instruction::ASR(rd) => format!("ASR {:?}", rd),
            Instruction::BCLR(b) => format!("BCLR {b}"),
            Instruction::BREQ(imm) => format!("BREQ {imm}"),
            Instruction::BRGE(imm) => format!("BRGE {imm}"),
            Instruction::BRK => format!("BRK"),
            Instruction::BRLO(imm) => format!("BRLO {imm}"),
            Instruction::BRLT(imm) => format!("BRLT {imm}"),
            Instruction::BRNE(imm) => format!("BRNE {imm}"),
            Instruction::BRSH(imm) => format!("BRSH {imm}"),
            Instruction::BSET(b) => format!("BSET {b}"),
            Instruction::CALL(imm) => format!("CALL {imm}"),
            Instruction::CBIO(ioaddr, b) => format!("CBIO {ioaddr}, {b}"),
            Instruction::CBR(rd, b) => format!("CBR {:?}, {b}",rd),
            Instruction::CLR(rd) => format!("CLR {:?}", rd),
            Instruction::COM(rd) => format!("COM {:?}", rd),
            Instruction::CPI(rd, imm) => format!("CPI {:?}, {imm}", rd),
            Instruction::CPSE(rd, rs) => format!("CPSE {:?}, {:?}",rd,rs),
            Instruction::DEC(rd) => format!("DEC {:?}", rd),
            Instruction::DIV(rd, rs) => format!("DIV {:?}, {:?}",rd,rs),
            Instruction::HALT => format!("HALT"),
            Instruction::IJMP => format!("IJMP"),
            Instruction::IN(rd, imm) => format!("IN {:?} {imm}", rd),
            Instruction::INC(rd) => format!("INC {:?}", rd),
            Instruction::JMP(imm) => format!("JMP {imm}"),
            Instruction::LD(rd, rs) => format!("LD {:?}, {:?}",rd,rs),
            Instruction::LDD(rd, imm) => format!("LDD {:?}, {imm}", rd),
            Instruction::LDI(rd, imm) => format!("LDI {:?}, {imm}", rd),
            Instruction::LDP(rd, imm) => format!("LDP {:?}, {imm}", rd),
            Instruction::LSL(rd) => format!("LSL {:?}", rd),
            Instruction::LSR(rd) => format!("LSR {:?}", rd),
            Instruction::MOV(rd, rs) => format!("MOV {:?}, {:?}",rd,rs),
            Instruction::MUL(rd, rs) => format!("MUL {:?}, {:?}",rd,rs),
            Instruction::NEG(rd) => format!("NEG {:?}", rd),
            Instruction::NOP => format!("NOP"),
            Instruction::OR(rd, rs) => format!("OR {:?}, {:?}",rd,rs),
            Instruction::ORI(rd, imm) => format!("ORI {:?}, {imm}", rd),
            Instruction::OUT(imm, rd) => format!("OUT {imm}, {:?}", rd),
            Instruction::POP(rd) => format!("POP {:?}", rd),
            Instruction::PUSH(rd) => format!("PUSH {:?}", rd),
            Instruction::RCALL(imm) => format!("RCALL {imm}"),
            Instruction::RET => format!("RET"),
            Instruction::RJMP(imm) => format!("RJMP {imm}"),
            Instruction::SBIO(ioaddr, b) => format!("SBIO {ioaddr}, {b}"),
            Instruction::SBR(rd, b) => format!("SBR {:?}, {b}",rd),
            Instruction::SET(rd) => format!("SET {:?}", rd),
            Instruction::ST(rd, rs) => format!("ST {:?}, {:?}",rd,rs),
            Instruction::STD(imm, rd) => format!("STD {imm}, {:?}", rd),
            Instruction::STP(imm, rd) => format!("STP {imm}, {:?}",rd),
            Instruction::SUB(rd, rs) => format!("SUB {:?}, {:?}",rd,rs),
            Instruction::SUBI(rd, imm) => format!("SUBI {:?}, {imm}", rd),
            Instruction::XOR(rd, rs) => format!("XOR {:?}, {:?}",rd,rs),
            Instruction::SecondOpWord => format!("SecondOpWord"),
            Instruction::UnknownOp(o) => format!("UnknownOp: {:b} ({:X})",o,o),
            _ => format!("??")
        }
    }
}

pub trait ISA {
    // the type representing an instruction
    type Item;

    fn version(&self) -> &str;
    fn encode(&self, instr: Instruction) -> code_t;
    fn decode(&self, instr: code_t) -> Instruction;
    // fn decode_r(&self, instr: code_t) -> (u8, u8, u8, u8);
    // fn decode_e(&self, instr: code_t) -> (u8, u8, u8);
    fn get_opcode(&self, instr: code_t) -> op::code;
    fn handle_instruction(&mut self, i: Instruction);

    // these are the operations of the ISA
    fn halt(&self, args: &[code_t]);
    fn brk(&self, args: &[code_t]);

    fn add(&mut self, args: &[code_t]);
    fn addi(&mut self, args: &[code_t]);
    fn and(&mut self, args: &[code_t]);
    fn bclr(&mut self, args: &[code_t]);
    fn bset(&mut self, args: &[code_t]);
    fn call(&mut self, args: &[code_t]);
    fn cbio(&mut self, args: &[code_t]);
    fn cbr(&mut self, args: &[code_t]);
    fn clr(&mut self, args: &[code_t]);
    fn com(&mut self, args: &[code_t]);
    fn dec(&mut self, args: &[code_t]);
    fn div(&mut self, args: &[code_t]);
    fn inc(&mut self, args: &[code_t]);
    fn jmp(&mut self, args: &[code_t]);   
    fn ld(&mut self, args: &[code_t]);
    fn ldd(&mut self, args: &[code_t]);
    fn ldi(&mut self, args: &[code_t]);
    fn ldp(&mut self, args: &[code_t]);
    fn mov(&mut self, args: &[code_t]);
    fn mul(&mut self, args: &[code_t]);

    fn or(&mut self, args: &[code_t]);
    fn ori(&mut self, args: &[code_t]);

    fn push(&mut self, args: &[code_t]);
    fn pop(&mut self, args: &[code_t]);
    fn ret(&mut self, args: &[code_t]);
    fn sbio(&mut self, args: &[code_t]);
    fn sbr(&mut self, args: &[code_t]);
    fn st(&mut self, args: &[code_t]);
    fn std(&mut self, args: &[code_t]);
    fn stp(&mut self, args: &[code_t]);
    fn sub(&mut self, args: &[code_t]);
    fn subi(&mut self, args: &[code_t]);

    fn xor(&mut self, args: &[code_t]);

    // pseudocodes
    fn nop(&mut self, args: &[code_t]);
    fn sei(&mut self, args: &[code_t]);
    fn cli(&mut self, args: &[code_t]);
    fn nor(&mut self, args: &[code_t]);

}

fn valid_u16(str: &str) -> u16 {
    types::parse::<u16>(str).unwrap()
}
fn valid_u8(str: &str) -> u8 {
    types::parse::<u8>(str).unwrap()
}

#[derive(Debug, Default)]
pub struct InstructionTable(HashMap<addr_t, Instruction>);

/// Instruction Table
/// 
/// Stores the machine instructions and allows them to be retrieved by
/// name or opcode. Implemented as a `HashMap` behind the scenes.
impl InstructionTable {
    pub fn new() -> InstructionTable {
        InstructionTable(HashMap::new())
    }
    pub fn by_location(&self, opcode: u16) -> Option<&Instruction> {
        self.0.get(&opcode)
    }
    pub fn by_name(&self, name: &str) -> Option<&Instruction> {
        self.0.values().find(|instr| instr.to_string() == name)
    }
    pub fn insert(&mut self, instr: Instruction) {
    //    self.0.insert(instr.get_opcode(), instr);
    todo!()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn symbols(&self) -> Vec<(u16, &str)> {
        let mut result = vec![];
        self.0.keys().for_each(|key| {
            let instr = &self.0[key];
  //          result.push((instr.opcode, instr.to_string().clone()));
            todo!();
        });
       // result.sort_by(|lhs, rhs| lhs.0.cmp(&rhs.0));
        result
    }
}

////////////////////////////////////////////////////////////

pub fn decode<T>(asm: T) -> DecodedIterator<T> where T: Iterator<Item=u8> { DecodedIterator {iter: asm, next_invalid: false} }
pub struct DecodedIterator<T: Iterator<Item=u8>> { iter: T, next_invalid: bool }




#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::Instruction;
    use super::Register;
    use super::op::code;
    use super::decode_e;
    use super::decode_a;
    use super::encode_e;
    use super::encode_a;

    const ADDR0R1: u16 = 0b0000000100000001;
    const DIVR4R7: u16 = 0b0001011001000111;
    const ADDIR1888: u32 = 0b00000010000100000000001101111000;

    #[test]
    fn encode() {
        let code = encode_a(1,0,1);
        println!("ADD R0, R1 = {:0>16b}",code);
        assert_eq!(code, ADDR0R1);
        let code = encode_a(22,4,7);
        assert_eq!(code, DIVR4R7);
        let code = encode_e(2,1,888);
        assert_eq!(code,ADDIR1888);
    }
    #[test]
    fn decode() {
        let (opcode, rd, rs) = decode_a(ADDR0R1);
        assert_eq!((opcode,rd,rs),(1,0,1));        
        let (opcode, rd, rs) = decode_a(DIVR4R7);
        assert_eq!((opcode,rd,rs),(22,4,7));
        let (opcode, rd, rs, imm) = decode_e(ADDIR1888);
        assert_eq!((opcode,rd,rs,imm),(2,1,0,888));
    }
    #[test]
    fn instruction_from_string() {
        let i1 = Instruction::from_str("ADD R0, R1").unwrap();
        assert_eq!(i1,Instruction::ADD(Register::R0, Register::R1));

        let i1 = Instruction::from_str("ADDI R1, 0xfe").unwrap();
        assert_eq!(i1,Instruction::ADDI(Register::R1, 0xfe));   
        let i1 = Instruction::from_str("AND R1, R3").unwrap();
        assert_eq!(i1,Instruction::AND(Register::R1, Register::R3)); 
        let i1 = Instruction::from_str("ASR R8").unwrap();
        assert_eq!(i1,Instruction::ASR(Register::R8));
        let i1 = Instruction::from_str("BCLR 3").unwrap();
        assert_eq!(i1,Instruction::BCLR(3));  
        let i1 = Instruction::from_str("BREQ $dead").unwrap();
        assert_eq!(i1,Instruction::BREQ(0xdead));  
        let i1 = Instruction::from_str("BRGE 0xdead").unwrap();
        assert_eq!(i1,Instruction::BRGE(0xdead));  
        let i1 = Instruction::from_str("BRLO $dead").unwrap();
        assert_eq!(i1,Instruction::BRLO(0xdead));
        let i1 = Instruction::from_str("BRLT $dead").unwrap();
        assert_eq!(i1,Instruction::BRLT(0xdead));  
        let i1 = Instruction::from_str("BRNE $dead").unwrap();
        assert_eq!(i1,Instruction::BRNE(0xdead));  
        let i1 = Instruction::from_str("BRSH $dead").unwrap();
        assert_eq!(i1,Instruction::BRSH(0xdead));  
        let i1 = Instruction::from_str("BSET 5").unwrap();
        assert_eq!(i1,Instruction::BSET(5));  
        let i1 = Instruction::from_str("CALL $dead").unwrap();
        assert_eq!(i1,Instruction::CALL(0xdead)); 
        let i1 = Instruction::from_str("CBIO 0xdead, 15").unwrap();
        assert_eq!(i1,Instruction::CBIO(0xdead,15));
        let i1 = Instruction::from_str("CBR R8, 4").unwrap();
        assert_eq!(i1,Instruction::CBR(Register::R8, 4));
        let i1 = Instruction::from_str("CLR R9").unwrap();
        assert_eq!(i1,Instruction::CLR(Register::R9));  
        let i1 = Instruction::from_str("COM R10").unwrap();
        assert_eq!(i1,Instruction::COM(Register::R10)); 
        let i1 = Instruction::from_str("CPI R11, $dead").unwrap();
        assert_eq!(i1,Instruction::CPI(Register::R11, 0xdead));  
        let i1 = Instruction::from_str("CPSE R12, r2").unwrap();
        assert_eq!(i1,Instruction::CPSE(Register::R12, Register::R2));   
        let i1 = Instruction::from_str("DEC R13").unwrap();
        assert_eq!(i1,Instruction::DEC(Register::R13)); 
        let i1 = Instruction::from_str("DIV R4, R7").unwrap();
        assert_eq!(i1,Instruction::DIV(Register::R4, Register::R7));   
        let i1 = Instruction::from_str("HALT").unwrap();
        assert_eq!(i1,Instruction::HALT); 
        let i1 = Instruction::from_str("IN R14, 35").unwrap();
        assert_eq!(i1,Instruction::IN(Register::R14, 35));   
        let i1 = Instruction::from_str("INC R14").unwrap();
        assert_eq!(i1,Instruction::INC(Register::R14));
        let i1 = Instruction::from_str("JMP $dead").unwrap();
        assert_eq!(i1,Instruction::JMP(0xdead));
        let i1 = Instruction::from_str("LD R4, R7").unwrap();
        assert_eq!(i1,Instruction::LD(Register::R4, Register::R7)); 
        let i1 = Instruction::from_str("LDD R4, 4567").unwrap();
        assert_eq!(i1,Instruction::LDD(Register::R4, 4567));
        let i1 = Instruction::from_str("LDI R4, 9876").unwrap();
        assert_eq!(i1,Instruction::LDI(Register::R4, 9876));
        let i1 = Instruction::from_str("LDP R4, 4567").unwrap();
        assert_eq!(i1,Instruction::LDP(Register::R4, 4567));
        let i1 = Instruction::from_str("LSL R15").unwrap();
        assert_eq!(i1,Instruction::LSL(Register::R15));
        let i1 = Instruction::from_str("LSR R1").unwrap();
        assert_eq!(i1,Instruction::LSR(Register::R1));
        let i1 = Instruction::from_str("MOV R4, R7").unwrap();
        assert_eq!(i1,Instruction::MOV(Register::R4, Register::R7));
        let i1 = Instruction::from_str("MUL R4, R7").unwrap();
        assert_eq!(i1,Instruction::MUL(Register::R4, Register::R7));
        let i1 = Instruction::from_str("NEG R4").unwrap();
        assert_eq!(i1,Instruction::NEG(Register::R4));
        let i1 = Instruction::from_str("NOP").unwrap();
        assert_eq!(i1,Instruction::NOP);
        let i1 = Instruction::from_str("OR R4, R7").unwrap();
        assert_eq!(i1,Instruction::OR(Register::R4, Register::R7));
        let i1 = Instruction::from_str("ORI R4, $FF").unwrap();
        assert_eq!(i1,Instruction::ORI(Register::R4, 255));
        let i1 = Instruction::from_str("OUT 123, R9").unwrap();
        assert_eq!(i1,Instruction::OUT(123,Register::R9));
        let i1 = Instruction::from_str("POP R1").unwrap();
        assert_eq!(i1,Instruction::POP(Register::R1));
        let i1 = Instruction::from_str("PUSH R1").unwrap();
        assert_eq!(i1,Instruction::PUSH(Register::R1));
        let i1 = Instruction::from_str("RCALL 0xdead").unwrap();
        assert_eq!(i1,Instruction::RCALL(0xdead));
        let i1 = Instruction::from_str("ret").unwrap();
        assert_eq!(i1,Instruction::RET);
        let i1 = Instruction::from_str("RJMP 0xdead").unwrap();
        assert_eq!(i1,Instruction::RJMP(0xdead));
        let i1 = Instruction::from_str("SBIO $ffea, 250").unwrap();
        assert_eq!(i1,Instruction::SBIO(0xffea, 250));
        let i1 = Instruction::from_str("SBR R8, 23").unwrap();
        assert_eq!(i1,Instruction::SBR(Register::R8,23));
        let i1 = Instruction::from_str("SET R1").unwrap();
        assert_eq!(i1,Instruction::SET(Register::R1));
        let i1 = Instruction::from_str("ST R1, R9").unwrap();
        assert_eq!(i1,Instruction::ST(Register::R1, Register::R9));
        let i1 = Instruction::from_str("STD $dead, R13").unwrap();
        assert_eq!(i1,Instruction::STD(0xdead, Register::R13));
        let i1 = Instruction::from_str("STP $dead, R12").unwrap();
        assert_eq!(i1,Instruction::STP(0xdead, Register::R12));
        let i1 = Instruction::from_str("SUB R10, R1").unwrap();
        assert_eq!(i1,Instruction::SUB(Register::R10, Register::R1));
        let i1 = Instruction::from_str("SUBI R7, 1000").unwrap();
        assert_eq!(i1,Instruction::SUBI(Register::R7, 1000));
        let i1 = Instruction::from_str("XOR R3, R5").unwrap();
        assert_eq!(i1,Instruction::XOR(Register::R3, Register::R5));

    }
    #[test]
    fn instruction_to_string() {
        println!("{:?}",Instruction::ADD(Register::R7, Register::R3));
        assert_eq!(Instruction::ADD(Register::R7, Register::R3).to_string(), "ADD R7, R3");
        assert_eq!(Instruction::ADDI(Register::R2, 489).to_string(), "ADDI R2, 489");
        assert_eq!(Instruction::AND(Register::R4, Register::R9).to_string(), "AND R4, R9");
        assert_eq!(Instruction::ASR(Register::R4).to_string(), "ASR R4");
        assert_eq!(Instruction::BCLR(3).to_string(), "BCLR 3");
        assert_eq!(Instruction::BREQ(0xff20).to_string(), format!("BREQ {}",0xff20));
        assert_eq!(Instruction::BRGE(0xff20).to_string(), format!("BRGE {}",0xff20));
        assert_eq!(Instruction::BRLO(0xff20).to_string(), format!("BRLO {}",0xff20));
        assert_eq!(Instruction::BRLT(0xff20).to_string(), format!("BRLT {}",0xff20));
        assert_eq!(Instruction::BRNE(0xff20).to_string(), format!("BRNE {}",0xff20));
        assert_eq!(Instruction::BRSH(0xff20).to_string(), format!("BRSH {}",0xff20));
        assert_eq!(Instruction::BRK.to_string(), "BRK");
        assert_eq!(Instruction::CALL(0xdead).to_string(), format!("CALL {}",0xdead));
        assert_eq!(Instruction::CBIO(0xfe14, 12).to_string(), format!("CBIO {}, 12",0xfe14));
        assert_eq!(Instruction::CBR(Register::R10, 5).to_string(), "CBR R10, 5");
        assert_eq!(Instruction::CLR(Register::R4).to_string(), "CLR R4");
        assert_eq!(Instruction::COM(Register::R4).to_string(), "COM R4");
        assert_eq!(Instruction::CPI(Register::R2, 29018).to_string(), "CPI R2, 29018");
        assert_eq!(Instruction::CPSE(Register::R4, Register::R9).to_string(), "CPSE R4, R9");
        assert_eq!(Instruction::DEC(Register::R4).to_string(), "DEC R4");
        assert_eq!(Instruction::DIV(Register::R0, Register::R13).to_string(), "DIV R0, R13");
        assert_eq!(Instruction::HALT.to_string(), "HALT");
        assert_eq!(Instruction::IJMP.to_string(), "IJMP");
        assert_eq!(Instruction::IN(Register::R2, 198).to_string(), "IN R2, 198");
        assert_eq!(Instruction::INC(Register::R4).to_string(), "INC R4");
        assert_eq!(Instruction::JMP(0xdead).to_string(), format!("JMP {}",0xdead));
        assert_eq!(Instruction::LD(Register::R4, Register::R9).to_string(), "LD R4, R9");
        assert_eq!(Instruction::LDD(Register::R2, 29018).to_string(), "LDD R2, 29018");
        assert_eq!(Instruction::LDI(Register::R2, 29018).to_string(), "LDI R2, 29018");
        assert_eq!(Instruction::LDP(Register::R2, 29018).to_string(), "LDP R2, 29018");
        assert_eq!(Instruction::LSL(Register::R7).to_string(), "LSL R7");
        assert_eq!(Instruction::LSR(Register::R14).to_string(), "LSR R14");
        assert_eq!(Instruction::MOV(Register::R4, Register::R9).to_string(), "MOV R4, R9");
        assert_eq!(Instruction::MUL(Register::R4, Register::R9).to_string(), "MUL R4, R9");
        assert_eq!(Instruction::NEG(Register::R14).to_string(), "NEG R14");
        assert_eq!(Instruction::NOP.to_string(), "NOP");
        assert_eq!(Instruction::OR(Register::R7, Register::R3).to_string(), "OR R7, R3");
        assert_eq!(Instruction::ORI(Register::R2, 489).to_string(), "ORI R2, 489");
        // TODO
        assert_eq!(Instruction::SBIO(0xdead, 6).to_string(), format!("SBIO {}, 6", 0xdead)); // internally takes txt hex -> int
        assert_eq!(Instruction::OUT(4, Register::R9).to_string(), "OUT 4, R9");
        assert_eq!(Instruction::PUSH(Register::R12).to_string(), "PUSH R12");
    }
    #[test]
    fn instruction_from_code() {
        let i1 = Instruction::from(ADDR0R1);
        assert_eq!(i1,Instruction::ADD(Register::R0, Register::R1));
        let i1 = Instruction::from(ADDIR1888);
        assert_eq!(i1,Instruction::ADDI(Register::R1, 888));
        let i1 = Instruction::from(encode_a(code::AND as u16, 4, 5));
        assert_eq!(i1,Instruction::AND(Register::R4, Register::R5));
        let i1 = Instruction::from(encode_a(code::ASR as u16, 4, 0));
        assert_eq!(i1,Instruction::ASR(Register::R4));
        let i1 = Instruction::from(encode_a(code::BCLR as u16, 4, 0));
        assert_eq!(i1,Instruction::BCLR(4));
        let i1 = Instruction::from(encode_e(code::BREQ as u16, 4, 0xdead));
        assert_eq!(i1,Instruction::BREQ(0xdead));
        let i1 = Instruction::from(encode_e(code::BRGE as u16, 4, 0xdead));
        assert_eq!(i1,Instruction::BRGE(0xdead));
        let i1 = Instruction::from(encode_e(code::BRLO as u16, 4, 0xdead));
        assert_eq!(i1,Instruction::BRLO(0xdead));
        let i1 = Instruction::from(encode_e(code::BRLT as u16, 4, 0xdead));
        assert_eq!(i1,Instruction::BRLT(0xdead));
        let i1 = Instruction::from(encode_e(code::BRNE as u16, 4, 0xdead));
        assert_eq!(i1,Instruction::BRNE(0xdead));
        let i1 = Instruction::from(encode_e(code::BRSH as u16, 4, 0xdead));
        assert_eq!(i1,Instruction::BRSH(0xdead));
        let i1 = Instruction::from(encode_a(code::BSET as u16, 7, 0));
        assert_eq!(i1,Instruction::BSET(7));
        let i1 = Instruction::from(encode_e(code::CALL as u16, 4, 0xdead));
        assert_eq!(i1,Instruction::CALL(0xdead));
        let i1 = Instruction::from(encode_e(code::CBIO as u16, 5, 0xdead));
        assert_eq!(i1,Instruction::CBIO(0xdead,5));
        let i1 = Instruction::from(encode_a(code::CBR as u16, 8, 4));
        assert_eq!(i1,Instruction::CBR(Register::R8, 4));
        let i1 = Instruction::from(encode_a(code::CLR as u16, 6, 0));
        assert_eq!(i1,Instruction::CLR(Register::R6));
        let i1 = Instruction::from(encode_a(code::COM as u16, 9, 0));
        assert_eq!(i1,Instruction::COM(Register::R9));
        let i1 = Instruction::from(encode_e(code::CPI as u16, 4, 0xdead));
        assert_eq!(i1,Instruction::CPI(Register::R4, 0xdead));
        let i1 = Instruction::from(encode_a(code::CPSE as u16, 0, 11));
        assert_eq!(i1,Instruction::CPSE(Register::R0, Register::R11));
        let i1 = Instruction::from(encode_a(code::DEC as u16, 11, 11));
        assert_eq!(i1,Instruction::DEC(Register::R11));
        let i1 = Instruction::from(encode_a(code::DIV as u16, 6, 9));
        assert_eq!(i1,Instruction::DIV(Register::R6, Register::R9));
        let i1 = Instruction::from(encode_a(code::HALT as u16, 0, 11));
        assert_eq!(i1,Instruction::HALT);
        let i1 = Instruction::from(encode_a(code::IJMP as u16, 0, 11));
        assert_eq!(i1,Instruction::IJMP);
        let i1 = Instruction::from(encode_e(code::IN as u16, 4, 78));
        assert_eq!(i1,Instruction::IN(Register::R4, 78));
        let i1 = Instruction::from(encode_a(code::INC as u16, 3, 11));
        assert_eq!(i1,Instruction::INC(Register::R3));
        let i1 = Instruction::from(encode_e(code::JMP as u16, 0, 0x1a8f));
        assert_eq!(i1,Instruction::JMP(0x1a8f));
        let i1 = Instruction::from(encode_a(code::LD as u16, 11, 12));
        assert_eq!(i1,Instruction::LD(Register::R11, Register::R12));
        let i1 = Instruction::from(encode_e(code::LDD as u16, 11, 11));
        assert_eq!(i1,Instruction::LDD(Register::R11,11));        
        let i1 = Instruction::from(encode_e(code::LDI as u16, 11, 23233));
        assert_eq!(i1,Instruction::LDI(Register::R11,23233));
        let i1 = Instruction::from(encode_e(code::LDP as u16, 12, 16254));
        assert_eq!(i1,Instruction::LDP(Register::R12, 16254));
        let i1 = Instruction::from(encode_a(code::LSL as u16, 11, 11));
        assert_eq!(i1,Instruction::LSL(Register::R11));
        let i1 = Instruction::from(encode_a(code::LSR as u16, 11, 11));
        assert_eq!(i1,Instruction::LSR(Register::R11));
        let i1 = Instruction::from(encode_a(code::MOV as u16, 1, 14));
        assert_eq!(i1,Instruction::MOV(Register::R1, Register::R14));
        let i1 = Instruction::from(encode_a(code::MUL as u16, 8, 3));
        assert_eq!(i1,Instruction::MUL(Register::R8, Register::R3));
        let i1 = Instruction::from(encode_a(code::NEG as u16, 5, 0));
        assert_eq!(i1,Instruction::NEG(Register::R5));
        let i1 = Instruction::from(encode_a(code::NOP as u16, 0, 0));
        assert_eq!(i1,Instruction::NOP);
        let i1 = Instruction::from(encode_a(code::OR as u16, 11, 10));
        assert_eq!(i1,Instruction::OR(Register::R11, Register::R10));
        let i1 = Instruction::from(encode_e(code::ORI as u16, 6, 12697));
        assert_eq!(i1,Instruction::ORI(Register::R6, 12697));
        let i1 = Instruction::from(encode_e(code::OUT as u16, 11, 220));
        assert_eq!(i1,Instruction::OUT(220, Register::R11));
        let i1 = Instruction::from(encode_a(code::POP as u16, 0, 0));
        assert_eq!(i1,Instruction::POP(Register::R0));
        let i1 = Instruction::from(encode_a(code::PUSH as u16, 4, 0));
        assert_eq!(i1,Instruction::PUSH(Register::R4));
        let i1 = Instruction::from(encode_e(code::RCALL as u16, 11, 12300));
        assert_eq!(i1,Instruction::RCALL(12300));
        let i1 = Instruction::from(encode_a(code::RET as u16, 0, 0));
        assert_eq!(i1,Instruction::RET);
        let i1 = Instruction::from(encode_e(code::RJMP as u16, 11, 38989));
        assert_eq!(i1,Instruction::RJMP(38989));
        let i1 = Instruction::from(encode_e(code::SBIO as u16, 11, 10104));
        assert_eq!(i1,Instruction::SBIO(10104, 11));
        let i1 = Instruction::from(encode_a(code::SBR as u16, 11, 8));
        assert_eq!(i1,Instruction::SBR(Register::R11, 8));
        let i1 = Instruction::from(encode_a(code::SET as u16, 11, 0));
        assert_eq!(i1,Instruction::SET(Register::R11));
        let i1 = Instruction::from(encode_a(code::ST as u16, 5, 7));
        assert_eq!(i1,Instruction::ST(Register::R5, Register::R7));
        let i1 = Instruction::from(encode_e(code::STD as u16, 8, 0xdead));
        assert_eq!(i1,Instruction::STD(0xdead, Register::R8));
        let i1 = Instruction::from(encode_e(code::STP as u16, 13, 0xdead));
        assert_eq!(i1,Instruction::STP(0xdead, Register::R13));
        let i1 = Instruction::from(encode_a(code::SUB as u16, 5, 11));
        assert_eq!(i1,Instruction::SUB(Register::R5, Register::R11));
        let i1 = Instruction::from(encode_e(code::SUBI as u16, 3, 0xdead));
        assert_eq!(i1,Instruction::SUBI(Register::R3, 0xdead));
        let i1 = Instruction::from(encode_a(code::XOR as u16, 1, 6));
        assert_eq!(i1,Instruction::XOR(Register::R1, Register::R6));


    }
    #[test]
    fn from_variant_str() {
        let i1 = Instruction::from_str("addi r1, 888").unwrap();
        assert_eq!(i1,Instruction::ADDI(Register::R1, 888));
        let i1 = Instruction::from_str("LDI R10, 0xfe").unwrap();
        assert_eq!(i1,Instruction::ADDI(Register::R1, 0xfe));    
    }

}