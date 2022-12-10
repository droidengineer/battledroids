//!
//! 
use crate::emu::mem::SRAM;
use crate::emu::{ComputeDevice, Status};

#[derive(Debug)]
pub struct Machine {
    pub cpu: ComputeDevice,
    pub status: self::Status,
    pub mem: SRAM,
    pub tracing: bool,
    pub curr_tick: usize,
    pub id: u8,
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
        }
    }

    pub fn init(&mut self) -> bool {
        false
    }
    pub fn tick(&mut self) -> Result<Status, &'static str> {
        let r = self.cpu.tick();

        self.curr_tick += 1;
        
        Ok(Status::Running)
    }

}