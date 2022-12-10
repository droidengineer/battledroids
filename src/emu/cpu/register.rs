//! Implementation of addr_t -size hardware RegisterFile
//!
//! 
use std::ops::{Index, IndexMut};
use std::str::{FromStr};

use enum_primitive::{*};
use num_traits::FromPrimitive;

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
    IR,
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

    MAX
} }

impl Register {
    pub fn offset(&self) -> usize {
        *self as usize
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
            "IR"  => Ok(Register::IR),
            "BP"  => Ok(Register::BP),
            "SP"  => Ok(Register::SP),
            "MP"  => Ok(Register::MP),
            "PC"  => Ok(Register::PC),
            "SREG" => Ok(Register::SREG),

            _ => Err(Error)
        }
    }
}

impl From<u16> for Register {
    fn from(_: u16) -> Self {
        todo!()
    }
}

#[derive(Debug)]
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

