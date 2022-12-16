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

use crate::types::*;
use num_traits::{FromPrimitive, ToPrimitive};

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

pub fn decode_a(instr: code_t) -> (u16,u16,u16) {
    let opcode = (instr & 0xFF00);
    let rd = (instr & 0xF0);
    let rs = (instr & 0xF);
    (opcode,rd,rs)
}

pub fn decode_r(instr: code_t ) -> (u16, u16, u16, u16) {
    let opcode = (instr & MASK_OP) >> 11;
    let rd = (instr & MASK_RD) >> 7;
    let rs = (instr & MASK_RS) >> 3;
    let ext = instr & MASK_EXT;
    (opcode,rd,rs,ext)
}
#[inline(always)]
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
pub fn decode_i(instr: u16) -> (u16, u16, u16) {
    let opcode = (instr & MASK_OP) >> 11;
    let xd = (instr & MASK_XD) >> 8;
    let imm8: u16 = instr & 0xFF;
    (opcode,xd,imm8)
}
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
    use enum_primitive::FromPrimitive;

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
    impl From<String> for code {
        fn from(s: String) -> Self {
            match s.as_str() {
                "ADD"   => code::ADD,
                "ADDI"  => code::ADDI,
                "AND"   => code::AND,
                "ASR"   => code::ASR,
                "BCLR"  => code::BCLR,
                "BREQ"  => code::BREQ,
                "BRGE"  => code::BRGE,
                "BRK"  => code::BRK,
                "BRLO"  => code::BRLO,
                "BRLT"  => code::BRLT,
                "BRNE"  => code::BRNE,
                "BRSH"  => code::BRSH,
                "BSET"  => code::BSET,
                "CALL"  => code::CALL,
                "CBIO"  => code::CBIO,
                "DEC"  => code::DEC,
                "DIV"  => code::DIV,
                "HALT"  => code::HALT,
                "IJMP"  => code::IJMP,
                "IN"  => code::IN,
                "INC"  => code::INC,
                "JMP"  => code::JMP,
                "LD"  => code::LD,
                "LDD"  => code::LDD,
                "LDI"  => code::LDI,
                "LDP"  => code::LDP,
                "LSL"  => code::LSL,
                "LSR"  => code::LSR,
                "MOV"  => code::MOV,
                "MUL"  => code::MUL,
                "NEG"  => code::NEG,
                "NOP"  => code::NOP,
                "OR"  => code::OR,
                "ORI"  => code::ORI,
                "OUT"  => code::OUT,
                "POP"  => code::POP,
                "PUSH"  => code::PUSH,
                "RCALL"  => code::RCALL,
                "RET"  => code::RET,
                "RJMP"  => code::RJMP,
                "SBIO"  => code::SBIO,
                "SBR"  => code::SBR,
                "SET"  => code::SET,
                "ST"  => code::ST,
                "STD"  => code::STD,
                "STP"  => code::STP,
                "SUB"  => code::SUB,
                "SUBI"  => code::SUBI,
                "XOR"  => code::XOR,

                &_ => code::HALT
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
                code::ADD     => String::from("ADD"),
                code::ADDI    => "ADDI".to_owned(),
                code::AND     => "AND".to_string(),
                code::ASR     => String::from("ASR"),
                code::BCLR    => String::from("BCLR"),
            
                _ => format!("<{:?}> not implemented",self)
            }
        }
    }
}

#[derive(Debug)]
pub enum InstructionType { R, E, I }

use std::collections::HashMap;

use super::register::Register;
use u8 as bit;
use u16 as imm11;
use op::code::*;

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
    /// *stp imm, rd* |`Rd[bit] = 1`| **Set bit in register**
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
            Instruction::ADD(rd,rs) => (*rd,*rs),
            Instruction::ADDI(rd,imm) => (*rd,imm),
            Instruction::AND(rd,rs) => (*rd,*rs),
            Instruction::ASR(rd) => (*rd,0),
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
    pub fn encode(&self) -> code_t {
        trace!("Instruction::encode()");
        match *self {
            Self::ADD(rd, rs) => 1,
            _ => 0,
        }
   }
   pub fn get_opcode(i: code_t) -> op::code {
        let op = (i & MASK_OP) >> 11;
        op::code::from(op)
   }
}
impl From<u16> for Instruction {
    fn from(i: u16) -> Self {
        let (op, rd, rs) = decode_a(i);
        let opcode = op::code::from(op);

        match opcode {
            ADD => {
                Instruction::ADD(Register::from(rd), Register::from(rs))
            },
            ADDI => {
                let (_,xd,imm) = decode_i(i);
                Instruction::ADDI(Register::from(xd), imm)
            },
            AND => {
                Instruction::AND(Register::from(rd), Register::from(rs))               
            },
            ASR => {
                Instruction::ASR(Register::from(rd))                  
            },
            BCLR => {
                let rd = rd as u8;
                Instruction::BCLR(rd)   
            },
            BREQ => todo!(),
            BRGE => todo!(),
            BRK => todo!(),
            BRLO => todo!(),
            BRLT => todo!(),
            BRNE => todo!(),
            BRSH => todo!(),
            BSET => {
                Instruction::BSET(rd as u8)         
            },
            CALL => todo!(),
            CBIO => todo!(),
            CBR => {
                Instruction::CBR(Register::from(rd),rs.try_into().unwrap())
            },
            CLR => {
                Instruction::CLR(Register::from(rd))
            },
            COM => {
                Instruction::COM(Register::from(rd))
            },
            CPI => todo!(),
            CPSE => {Instruction::CPSE(Register::from(rd), Register::from(rs))},
            DEC => {Instruction::DEC(Register::from(rd))},
            DIV => {Instruction::DIV(Register::from(rd), Register::from(rs))},
            HALT => todo!(),
            IJMP => todo!(),
            IN => todo!(),
            INC => {Instruction::INC(Register::from(rd))},
            JMP => todo!(),
            LD => {Instruction::LD(Register::from(rd), Register::from(rs))},
            LDD => todo!(),
            LDI => todo!(),
            LDP => todo!(),
            LSL => todo!(),
            LSR => todo!(),
            MOV => todo!(),
            MUL => todo!(),
            NEG => todo!(),
            NOP => todo!(),
            OR => todo!(),
            ORI => todo!(),
            OUT => todo!(),
            POP => todo!(),
            PUSH => todo!(),
            RCALL => todo!(),
            RET => todo!(),
            RJMP => todo!(),
            SBIO => todo!(),
            SBR => todo!(),
            SET => todo!(),
            ST => todo!(),
            STD => todo!(),
            STP => todo!(),
            SUB => todo!(),
            SUBI => todo!(),
            XOR => todo!(),
            
            UNDEF => Instruction::UnknownOp(i)
        }
    }  
}
impl From<u32> for Instruction {
    fn from(i: u32) -> Self {
        let instr = i as u16;
        let imm = (i >> 16) as u16;
        let (op, rd, rs) = decode_a(instr);
        let opcode = op::code::from(op);

        match opcode {
            ADDI => Instruction::ADDI(Register::from(rd), imm),
            BREQ => Instruction::BREQ(imm),
            BRGE => Instruction::BRGE(imm),
            BRSH => Instruction::BRSH(imm),
            BRLO => Instruction::BRLO(imm),
            BRLT => Instruction::BRLT(imm),
            BRNE => Instruction::BRNE(imm),
            CALL => Instruction::CALL(imm),
            CPI  => Instruction::CPI(Register::from(rd),imm),
            JMP => Instruction::JMP(imm),
            LDD => Instruction::LDD(Register::from(rd),imm),
            LDI => Instruction::LDI(Register::from(rd),imm),
            LDP => Instruction::LDP(Register::from(rd),imm),
            ORI => Instruction::ORI(Register::from(rd),imm),
            RCALL => Instruction::RCALL(imm),
            RJMP => Instruction::RJMP(imm),
            STD => Instruction::STD(imm,Register::from(rd)),
            STP => Instruction::STP(imm,Register::from(rd)),
            SUBI => Instruction::SUBI(Register::from(rd), imm),


            _ => Instruction::UnknownOp(instr)
        }
    }
}

impl From<String> for Instruction {
    fn from(s: String) -> Self {
        todo!()
    }
}

impl ToString for Instruction {
    fn to_string(&self) -> String {
        match *self {
            Instruction::ADD(rd,rs) => format!("ADD {:?}, {:?}",rd,rs),
            Instruction::ADDI(rd,imm) => format!("ADDI {:?} {imm}",rd),
            Instruction::AND(rd,rs) => format!("AND {:?}, {:?}",rd,rs),
            Instruction::ASR(rd) => format!("ASR {:?}", rd),
            Instruction::BCLR(_) => String::from("BCLR"),
            Instruction::BREQ(_) => String::from("BREQ"),
            Instruction::BRGE(_) => String::from("BRGE"),
            Instruction::BRK => String::from("BRK"),
            Instruction::BRLO(_) => String::from("BRLO"),
            Instruction::BRLT(_) => String::from("BRLT"),
            Instruction::BRNE(_) => String::from("BRNE"),
            Instruction::BRSH(_) => String::from("BRSH"),
            Instruction::BSET(_) => String::from("BSET"),
            Instruction::CALL(_) => String::from("CALL"),
            Instruction::CBIO(_, _) => String::from("CBIO"),
            Instruction::CBR(_, _) => String::from("CBR"),
            Instruction::CLR(_) => String::from("CLR"),
            Instruction::COM(_) => String::from("COM"),
            Instruction::CPI(_, _) => String::from("CPI"),
            Instruction::DEC(_) => String::from("DEC"),
            Instruction::DIV(_, _) => String::from("DIV"),
            Instruction::HALT => String::from("HALT"),
            Instruction::IJMP => String::from("IJMP"),
            Instruction::IN(_, _) => String::from("IN"),
            Instruction::INC(_) => String::from("INC"),
            Instruction::JMP(_) => String::from("JMP"),
            Instruction::LD(_, _) => String::from("LD"),
            Instruction::LDD(_, _) => String::from("LDD"),
            Instruction::LDI(_, _) => String::from("LDI"),
            Instruction::LDP(_, _) => String::from("LDP"),
            Instruction::LSL(_) => String::from("LSL"),
            Instruction::LSR(_) => String::from("LSR"),
            Instruction::MOV(_, _) => String::from("MOV"),
            Instruction::MUL(_, _) => String::from("MUL"),
            Instruction::NEG(_) => String::from("NEG"),
            Instruction::NOP => String::from("NOP"),
            Instruction::OR(_, _) => String::from("OR"),
            Instruction::ORI(_, _) => String::from("ORI"),
            Instruction::OUT(_, _) => String::from("OUT"),
            Instruction::POP(_) => String::from("POP"),
            Instruction::PUSH(_) => String::from("PUSH"),
            Instruction::RCALL(_) => String::from("RCALL"),
            Instruction::RET => String::from("RET"),
            Instruction::RJMP(_) => String::from("RJMP"),
            Instruction::SBIO(_, _) => String::from("SBIO"),
            Instruction::SBR(_, _) => String::from("SBR"),
            Instruction::SET(_) => String::from("SET"),
            Instruction::ST(_, _) => String::from("ST"),
            Instruction::STD(_, _) => String::from("STD"),
            Instruction::STP(_, _) => String::from("STP"),
            Instruction::SUB(_, _) => String::from("SUB"),
            Instruction::SUBI(_, _) => String::from("SUBI"),
            Instruction::XOR(_, _) => String::from("XOR"),
            Instruction::CPSE(_, _) => String::from("CPSE"),
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

#[derive(Debug, Default)]
pub struct InstructionTable(HashMap<u16, Instruction>);

/// Instruction Table
/// 
/// Stores the machine instructions and allows them to be retrieved by
/// name or opcode. Implemented as a `HashMap` behind the scenes.
impl InstructionTable {
    pub fn new() -> InstructionTable {
        InstructionTable(HashMap::new())
    }
    pub fn by_opcode(&self, opcode: u16) -> Option<&Instruction> {
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
