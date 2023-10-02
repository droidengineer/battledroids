pub mod table;
pub mod from_byte_code;
pub mod to_byte_code;
pub mod stack;
pub mod flaken;

use std::{env::temp_dir, ffi::OsString, str::FromStr, error::Error};
use num_traits::Num;
use crate::emu::{cpu::register::Register, isa::Instruction};

use rand::{thread_rng, Rng, distributions::{Alphanumeric}};

#[allow(non_camel_case_types)]
pub type addr_t = u16;
#[allow(non_camel_case_types)]
pub type code_t = u16;
#[allow(non_camel_case_types)]
pub type register_t = u16;

pub fn parse<T>(value: &str) -> Result<T, Box<dyn Error>>
where 
    T: Num + FromStr, 
    <T as Num>::FromStrRadixErr: Error + 'static,
    <T as FromStr>::Err: Error + 'static, 
{
    let value = value.trim();
    if value.starts_with("0x") {
        Ok(<T>::from_str_radix(value.strip_prefix("0x").unwrap(), 16)?)
    } else if value.starts_with("$") {
        Ok(<T>::from_str_radix(value.strip_prefix("$").unwrap(), 16)?)
    } else if value.starts_with("0b") {
        Ok(<T>::from_str_radix(value.strip_prefix("0b").unwrap(), 2)?)
    }

    else {
        Ok(value.parse::<T>()?)
    }
    
}

pub mod bit {
    use super::addr_t;

    pub fn bit8(b: u8, pos: usize) -> u8 {
        (b >> pos) & 1
    }
    pub fn bit16(b: addr_t, pos: usize) -> u16 {
        (b >> pos) & 1
    }
    /// takes a:
    /// * 16bit word `b`
    /// * `start` is number of bits `b` >> `start` 
    /// * `len` is how many bits to include
    /// returns 
    #[inline(always)]
    pub fn bits(b: u16, start: u8, len: u8) -> u16 {
        b >> start & ((1 << len) - 1)
    }
    pub fn bits8(b: u16, start: u8, len: u8) -> u8 {
        bits(b,start,len) as u8
    }
    #[inline(always)]
    pub fn bitneg(b: u16, pos: usize) -> u16 {
        !bit16(b, pos) & 1
    }
    pub fn bitneg8(b: u8, pos: usize) -> u8 {
        !bit8(b,pos) & 1
    }
    #[inline(always)]
    pub fn set(addr: addr_t, bit: u8) -> addr_t {
        addr | 1 << bit
    }
    #[inline(always)]
    pub fn clr(addr: addr_t, bit: u8) -> addr_t {
        addr & !(1 << bit)
    }
    #[inline(always)]
    pub fn tog(addr: addr_t, bit: u8) -> addr_t {
        addr ^ 1 << bit
    }
    #[inline(always)]
    pub fn check(addr: addr_t, bit: u8) -> addr_t {
        bit16(addr, bit as usize)
        //    addr & 1 << bit != 0
    }
    pub mod mask {
        pub fn set(x: u16, y: u16) -> u16 {
            x | y
        }
        pub fn clr(x: u16, y: u16) -> u16 {
            x & !y
        }
        pub fn tog(x: u16, y: u16) -> u16 {
            x ^ y
        }
        pub fn check(x: u16, y: u16) -> bool {
            x & y != 0
        }
    }
}

pub fn str_to_instruction(str: &str) -> Instruction {
    Instruction::from_str(str).unwrap()
}

pub fn gen_tmp_name(ext: &str) -> OsString {
    let mut rng = thread_rng();
    // let s = rng
    //     .sample_iter(&Alphanumeric)
    //     .take(12)
    //     .collect();

    let ss: String = (0..12).map(|_| rng.sample(Alphanumeric) as char).collect();

    let mut file = temp_dir();
    file.push("vm-".to_string() + &ss + "." + ext);
    file.into_os_string()
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Reg(Register),      // gpio[r]
    AtReg(Register),    // data[gpio[r]]
    AtAddr(addr_t),     // data[a]
    ProgMem(addr_t),    // program[a]
    Peek(addr_t),
    ///  literal is used to give a location for the value. Literals are always encountered 
    /// in the operand field of an instruction.
    Litteral(addr_t),   // 
    Immediate(addr_t),
    IR, BP, SP, MP, PC,
}
impl Value {
    // pub fn value(&self) -> register_t {
    //     match *self {
    //         Value::Reg(r) => todo!(),
    //         Value::AtReg(r) => todo!(),
    //         Value::AtAddr(a) => todo!(),
    //         Value::ProgMem(a) => todo!(),
    //         Value::Peek(a) => todo!(),
    //         Value::Litteral(a) => todo!(),
    //         Value::Immediate(a) => todo!(),
    //         Value::IR => todo!(),
    //         Value::BP => todo!(),
    //         Value::SP => todo!(),
    //         Value::MP => todo!(),
    //         Value::PC => todo!(),
    //     }
    // } 
}