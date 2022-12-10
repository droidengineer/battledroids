use std::fmt;
use crate::emu::cpu::register::Register;
pub mod stack;

#[allow(non_camel_case_types)]
pub type addr_t = u16;
#[allow(non_camel_case_types)]
pub type code_t = u16;
#[allow(non_camel_case_types)]
pub type register_t = u16;


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

/// A structure containing runnable or dumpable code
/// from stack-vm
pub struct Code {
    pub symbols: Vec<(usize, String)>,
    pub code: Vec<usize>,
    pub data: Vec<addr_t>,
    pub labels: Vec<(usize, String)>,
}
impl Code {
    pub fn symbols(&self) -> &[(usize, String)] {
        self.symbols.as_slice()
    }
    pub fn code(&self) -> &[usize] {
        self.code.as_slice()
    }
    pub fn data(&self) -> &[addr_t] {
        self.data.as_slice()
    }
    pub fn labels(&self) -> &[(usize, String)] {
        self.labels.as_slice()
    }
    pub fn get_label_ip(&self, name: &str) -> Option<usize> {
        for label in self.labels.as_slice() {
            if label.1 == name {
                return Some(label.0);
            }
        }
        None
    }
}



