//! `emu::cpu` Module
//! 

pub mod register;
pub mod isa;
pub mod pe;
pub mod cu;
pub mod cmp;

use std::ops::Deref;

use enum_primitive::FromPrimitive;

pub use crate::emu::cpu::pe::ProcessingElement;
pub use crate::emu::cpu::cu::ComputeUnit;
pub use crate::emu::cpu::cmp::{ComputeGroup, ComputeDevice};
pub use crate::emu::cpu::register::Register;

use self::register::ShadowRegister;

#[derive(Debug, Default,Copy,Clone, PartialEq)]
pub enum Status {
    #[default] Running, Halted, BadMem, BadData, NoData,
    DivZero, BadOp, BadIdx, BadIPC, BadFct, Deadlock,
    Sleeping
}
enum_from_primitive! {
    #[derive(Debug, Copy, Clone)]
    #[repr(u16)]
    pub enum SREG {
        C = 1,  // carry flag 0x01
        Z,      // zero flag 0x02
        N,      // negative flag 0x04
        V,      // Overflow 0x08
        S,      // signed 0x10
        H,      // half carry flag 0x20
        T,      // transfer bit 0x40
        I,      // global interrupt enable 0x80
    }
}
impl SREG {
    pub fn offset(&self) -> usize {
        *self as usize
    }
}
impl Deref for SREG {
    type Target = u16;
    fn deref(&self) -> &u16 {
        todo!()
        //&(self.offset() as u16)
    }
}
impl From<u16> for SREG {
    fn from(t: u16) -> Self {
        SREG::from_u16(t).unwrap()
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct ProcID {
    pub instance_id: u16,   // instance of running code
    pub pe_id: u16,         // thread ID
    pub cu_id: u16,         // core ID
    pub device_id: u16,     // cpu ID
    pub cg_id: u16,         // workgroup ID
    pub machine_id: u8,     // machine ID

}
pub fn broadcast_pe_id() -> u16 { u16::MAX }
pub fn broadcast_cu_id() -> u16 { u16::MAX }
pub fn broadcast_cg_id() -> u16 { u16::MAX }
pub fn broadcast_device_id() -> u16 { u16::MAX }

impl ProcID {
    pub fn new(instance_id: u16, device_id: u16, machine_id: u8, cg_id: u16, pe_id: u16, cu_id: u16) -> Self {
        ProcID {
            instance_id,
            device_id,
            machine_id,
            cg_id,
            pe_id,
            cu_id
        }
    }
    /// Broadcast to all at the device level (global)
    pub fn broadcast_global(&self) -> ProcID {
        ProcID {
            device_id: broadcast_device_id(),
            ..*self
        }
    }
    /// Broadcast to all peer `ProcessElement`s
    pub fn broadcast_local(&self) -> ProcID {
        ProcID {
            pe_id: broadcast_pe_id(),
            ..*self
        }
    }    /// Broadcast to all peer `ComputUnit`s
    pub fn broadcast_cores(&self) -> ProcID {
        ProcID {
            cu_id: broadcast_cu_id(),
            ..*self
        }
    }
    /// Broadcast to all `ComputeUnit`s in workgroup
    pub fn broadcast_group(&self) -> ProcID {
        ProcID {
            cu_id: broadcast_cu_id(),
            ..*self
        }
    }
    pub fn is_broadcast(&self) -> bool {
        self.pe_id == broadcast_pe_id() ||
        self.cu_id == broadcast_cu_id() ||
        self.cg_id == broadcast_cg_id() ||
        self.device_id == broadcast_device_id()
    }
    pub fn format(&self) -> String {
        format!(
         //   "{}_{}@{}:{}.{}",
         //   "{}_{:X}:{:X}.{:X}@{:X}",
            "Device: {:X} Group: {:X} Unit: {:X} Element: {} Instance: {}",
            self.device_id,
            self.cg_id,
            self.cu_id,
            self.pe_id,
            self.instance_id,
        )
    }
}

impl ::std::fmt::Debug for ProcID {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(
            f,
            "{}_{}@{}:{}.{}",
            self.instance_id,
            self.pe_id,
            self.device_id,
            self.cg_id,
            self.cu_id,           
        )
    }
}
impl ::std::fmt::Display for ProcID {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Debug::fmt(&self, f)
    }
}

#[derive(Debug)]
pub enum ParseProcIDError {
    Format,
    ParseIntError(::std::num::ParseIntError),
}
impl ::std::fmt::Display for ParseProcIDError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        ::std::fmt::Debug::fmt(self,f)
    }
}


impl ::std::str::FromStr for ProcID {
    type Err = ParseProcIDError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split(|c| c == '_' || c == ':' || c == '.' || c == '@');

        match(parts.next(), parts.next(), parts.next(), parts.next(), parts.next()) {
            (Some(instance_part), Some(thread_part), Some(group_part), Some(unit_part), Some(device_part)) => {
                let instance_id = u16::from_str_radix(instance_part, 16).map_err(ParseProcIDError::ParseIntError)?;
                let thread_id = u16::from_str_radix(thread_part, 16).map_err(ParseProcIDError::ParseIntError)?;
                let group_id = u16::from_str_radix(group_part,16).map_err(ParseProcIDError::ParseIntError)?;
                let unit_id = u16::from_str_radix(unit_part,16).map_err(ParseProcIDError::ParseIntError)?;
                let device_id = u16::from_str_radix(device_part,16).map_err(ParseProcIDError::ParseIntError)?;

                Ok(ProcID { 
                    instance_id, 
                    pe_id: thread_id,
                    cg_id: group_id,
                    cu_id: unit_id,
                    device_id,
                    machine_id: 1, 
                })
            }
            _ => Err(ParseProcIDError::Format),
        }
    }
}

// static mut pe_count: u16 = 1;
// pub fn next_pe_id() -> u16 { unsafe { let ret = pe_count; pe_count += 1; ret } }
// static mut cu_count: u16 = 1;
// pub fn next_cu_id() -> u16 { unsafe { let ret = cu_count; cu_count += 1; ret } }
// static mut cg_count: u16 = 1;
// pub fn next_cg_id() -> u16 { unsafe { let ret = cg_count; cg_count += 1; ret } }
static mut device_count: u16 = 1;
pub fn next_device_id() -> u16 { unsafe { let ret = device_count; device_count += 1; ret } }


pub struct ProcessRecord {
    pub reg: ShadowRegister,
    
}