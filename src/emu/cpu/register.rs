//! Implementation of addr_t -size hardware RegisterFile
//!
//! 
use std::ops::{Index, IndexMut, Deref};
use std::str::FromStr;

use enum_primitive::*;
use num_traits::{FromPrimitive, ToPrimitive};

use crate::types::*;

const GPIO_REG_MAX: usize = 16;

enum_from_primitive! {
    #[derive(Debug)]
    /// something
    pub enum SREG {
        /// carry flag
        C = 1,  
        Z,      /// zero flag 0x02
        N,      // negative flag 0x04
        V,      // Overflow 0x08
        S,      // signed 0x10
        H,      // half carry flag 0x20
        T,      // transfer bit 0x40
        I,      // global interrupt enable 0x80
    }
}

#[derive(Debug)]
/// Status Register Masks
pub enum SREG_MASK {
    C = 0x01,
    Z = 0x02,
    N = 0x04,
    V = 0x08,
    S = 0x10,
    H = 0x20,
    T = 0x40,
    I = 0x80,
}
 

enum_from_primitive! {
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[repr(u16)]
pub enum Register {
    R0 = 0x0, R1 = 0x1, R2 = 0x2, R3 = 0x3,
    R4 = 0x4, R5 = 0x5, R6 = 0x6, R7 = 0x7,
    R8 = 0x8, R9 = 0x9, R10 = 0xA, R11 = 0xB,
    R12 = 0xC, R13 = 0xD, R14 = 0xE, R15 = 0xF,

    /// Instruction Register
    IP,
    /// Base Pointer 
    BP, 
    /// Stack Pointer
    SP, 
    /// Mark Stack Pointer
    MP, 
    /// Program Counter
    PC,

    /// **Status Register**
    /// CZNVSHTI
    SREG,

    PE_ID, CU_ID, CG_ID, DEVICE_ID,

    /// **I/O Registers*

    MAX
} }

impl Register {
    pub fn offset(&self) -> usize {
        *self as usize
    }
    pub fn encode(&self) -> u16 {
        *self as u16
    }
 
}

use std::fmt::Error;
impl FromStr for Register {
    type Err = Error;
    fn from_str(s: &str) -> Result<Register, Error> {
        match s.to_uppercase().as_str() {
            "R0" => Ok(Register::R0),
            "R1" => Ok(Register::R1),
            "R2" => Ok(Register::R2),
            "R3" => Ok(Register::R3),
            "R4" => Ok(Register::R4),
            "R5" => Ok(Register::R5),
            "R6" => Ok(Register::R6),
            "R7" => Ok(Register::R7),
            "R8" => Ok(Register::R8),
            "R9" => Ok(Register::R9),
            "R10" => Ok(Register::R10),
            "R11" => Ok(Register::R11),
            "R12" => Ok(Register::R12),
            "R13" => Ok(Register::R13),
            "R14" => Ok(Register::R14),
            "R15" => Ok(Register::R15),
            "IP"  => Ok(Register::IP),
            "BP"  => Ok(Register::BP),
            "SP"  => Ok(Register::SP),
            "MP"  => Ok(Register::MP),
            "PC"  => Ok(Register::PC),
            "SREG" => Ok(Register::SREG),

            _ => Err(Error)
        }
    }
}

// impl std::fmt::Display for Register {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         todo!()
//     }
// }

// impl Deref for Register {
//     type Target = u16;

//     fn deref(&self) -> &u16 {
//         let d = self.offset() as u16;
//         &self.to_u16().unwrap()
//     //    &u16::from(*self)
//     }
// }

impl From<&str> for Register {
    fn from(s: &str) -> Self {
        Register::from_str(s).unwrap()
    }
}

impl From<u16> for Register {
    fn from(t: u16) -> Self {
        Register::from_u16(t).unwrap()
    }
}
// impl From<Register> for u16 {
//     fn from(r: Register) -> Self {
//         r
//         //r.to_u16().unwrap()
//     }
// }

#[derive(Debug,Copy,Clone)]
pub struct RegisterFile([addr_t; Register::MAX as usize]);
impl Default for RegisterFile {
    fn default() -> RegisterFile {
        RegisterFile([0x0000;Register::MAX as usize])
    }
}

impl Index<Register> for RegisterFile {
    type Output = addr_t;

    fn index(&self, r: Register) -> &addr_t {
        &self.0[r as usize]
    }
}
impl IndexMut<Register> for RegisterFile {
    fn index_mut(&mut self, r: Register) -> &mut addr_t {
        &mut self.0[r as usize]
    }
}

#[derive(Debug)]
pub struct ShadowRegister {
    pub bp: addr_t,
    pub sp: addr_t,
    pub mp: addr_t,
    pub pc: addr_t,
}
impl ShadowRegister {
    pub fn new() -> Self {
        ShadowRegister { bp: 0, sp: 0, mp: 0, pc: 0 }
    }
    pub fn get(&self, r: Register) -> u16 {
        match r {
            Register::BP => self.bp,
            Register::MP => self.mp,
            Register::SP => self.sp,
            Register::PC => self.pc,
            _ => todo!()
        }
    }
    pub fn set(&mut self, r: Register, v: register_t) {
        match r {
            Register::BP => self.bp = v,
            Register::MP => self.mp = v,
            Register::SP => self.sp = v,
            Register::PC => self.pc = v,
            _ => todo!()
        }
    }
}