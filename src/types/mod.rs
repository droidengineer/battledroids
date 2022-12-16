pub mod from_byte_code;
pub mod to_byte_code;
pub mod stack;

use crate::emu::cpu::register::Register;

#[allow(non_camel_case_types)]
pub type addr_t = u16;
#[allow(non_camel_case_types)]
pub type code_t = u16;
#[allow(non_camel_case_types)]
pub type register_t = u16;

pub mod bit {
    use super::addr_t;

    #[inline(always)]
    pub fn set(addr: addr_t, bit: u8) -> addr_t {
        (addr | 1 << bit)
    }
    #[inline(always)]
    pub fn clr(addr: addr_t, bit: u8) -> addr_t {
        (addr & !(1 << bit))
    }
    #[inline(always)]
    pub fn tog(addr: addr_t, bit: u8) -> addr_t {
        (addr ^ 1 << bit)
    }
    #[inline(always)]
    pub fn check(addr: addr_t, bit: u8) -> bool {
        addr & 1 << bit != 0
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
        pub fn cheak(x: u16, y: u16) -> bool {
            x & y != 0
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    Reg(Register),      // gpio[r]
    AtReg(Register),    // data[gpio[r]]
    AtAddr(addr_t),     // data[a]
    ProgMem(addr_t),    // program[a]
    Peek(addr_t),
    Litteral(addr_t),   // 
    Immediate(addr_t),
    IR, BP, SP, MP, PC,
}
impl Value {
    pub fn value(&self) -> register_t {
        match *self {
            Value::Reg(r) => todo!(),
            Value::AtReg(r) => todo!(),
            Value::AtAddr(a) => todo!(),
            Value::ProgMem(a) => todo!(),
            Value::Peek(a) => todo!(),
            Value::Litteral(a) => todo!(),
            Value::Immediate(a) => todo!(),
            Value::IR => todo!(),
            Value::BP => todo!(),
            Value::SP => todo!(),
            Value::MP => todo!(),
            Value::PC => todo!(),
        }
    } 
}