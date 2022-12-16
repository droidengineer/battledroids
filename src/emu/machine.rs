//!
//! 
use crate::emu::mem::SRAM;
use crate::emu::{ComputeDevice, Status};

use super::code::Code;

#[derive(Debug)]
pub struct Machine {
    pub cpu: ComputeDevice,
    pub status: self::Status,
    pub mem: SRAM,
    pub tracing: bool,
    pub curr_tick: usize,
    pub id: u8,
    pub code: Code,
}

impl Machine {
    pub fn new(t: bool) -> Machine {
        Machine {
            cpu: ComputeDevice::new(t),
            status: Status::Waiting,
            mem: SRAM::new(),
            tracing: t,
            curr_tick: 0,
            id: 1,
            code: Code::new(),
        }
    }

    pub fn init(&mut self) -> bool {
        false
    }
    pub fn tick(&mut self) -> Result<Status, &'static str> {
        self.curr_tick += 1;
        trace!("Current Tick: {}",self.curr_tick);
        let r = self.cpu.tick();

        
        r // Ok(Status::Running)
    }
}