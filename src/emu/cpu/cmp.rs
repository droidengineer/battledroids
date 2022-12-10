//! Compute Multi-Processor
//! Integrates multiple `ComputeUnits`
use crate::emu::{Status, next_device_id};

use super::{ComputeUnit, next_cg_id};


#[derive(Debug)]
/// The `ComputeGroup` is the local `ComputeUnit` aggregate
pub struct ComputeGroup {
    pub cores: Vec<ComputeUnit>,
    pub id: u16,
    pub tracing: bool,
    
}
impl ComputeGroup {
    pub fn new(num_cores: usize, t: bool, ids: (u16,u16)) -> ComputeGroup {
        let mut cores = Vec::new();
        let cg_id = ids.1;
        let mut n: usize = 0;
        while n != num_cores {
            cores.push(ComputeUnit::new(crate::emu::CORE_THREADS as u8, t, (ids.0, cg_id, n as u16)));
            println!("<core:{}>",n);
            n += 1;
        }
        println!("Spun up {n} cores");

        ComputeGroup { cores, id: cg_id, tracing: false,  }
    }
    pub fn num_cores(&self) -> usize {
        self.cores.len()
    }
}

#[derive(Debug)]
/// A computational device. Owns one or more `ComputeGroup`
pub struct ComputeDevice {
    pub cpu: Vec<ComputeGroup>,
    pub id: u16,
    pub tracing: bool,
}
impl ComputeDevice {
    pub fn new(t: bool) -> ComputeDevice {
        let mut cpu = Vec::new();
        let id = next_device_id();
        let mut n = 0;
        while n != crate::emu::CPU_MAX {
            let mut cg = ComputeGroup::new(crate::emu::CPU_CORES,t,(id, n as u16));
            cpu.push(cg);
            n += 1;
        }
        println!("Spun up {n} cpu's");

        ComputeDevice { cpu, id , tracing: t}
    }
    pub fn tick(&mut self) -> Result<Status, &'static str> {

        Ok(Status::Running)
    }
    pub fn push(&mut self, cpu: ComputeGroup) {
        self.cpu.push(cpu);
    }
}


