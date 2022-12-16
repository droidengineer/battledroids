//! Compute Multi-Processor
//! Integrates multiple `ComputeUnits`
use std::rc::Rc;

use crate::{emu::{L3_CACHE_MAX, CPU_CORES, Status, }, types::addr_t};

use super::ComputeUnit;


#[derive(Debug)]
/// The `ComputeGroup` is the local `ComputeUnit` aggregate.
/// CPU analog.
/// * `ComputeGroup`->`ComputeUnit`->`ProcessingElement`
pub struct ComputeGroup {
    pub cores: Vec<ComputeUnit>,
    pub id: u16,
    pub tracing: bool,
    pub cache: Rc<[addr_t; L3_CACHE_MAX]>,
}
impl ComputeGroup {
    pub fn new(num_cores: usize, cache: Rc<[addr_t; L3_CACHE_MAX]>,t: bool, ids: (u16,u16)) -> ComputeGroup {
        trace!("ComputeGroup::new({:?}, {}, {} {:?}", num_cores, t, cache.len(), ids);
        let mut cores = Vec::new();
        let cg_id = ids.1;
        let mut n: usize = 0;
        while n != num_cores {
            cores.push(ComputeUnit::new(crate::emu::CORE_THREADS as u8, t, (ids.0, cg_id, n as u16)));
            trace!("<core:{}>",n);
            n += 1;
        }
        info!("Spun up {n} cores");

        ComputeGroup { cores, id: cg_id, tracing: false, cache }
    }
    pub fn tick(&mut self) {
        for core in self.cores.iter_mut() {
            core.tick();
        }
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
    pub cache: Rc<[addr_t; L3_CACHE_MAX]>,
}
impl ComputeDevice {
    pub fn new(t: bool) -> ComputeDevice {
        trace!("ComputeDevice::new({t})");
        let mut cpu = Vec::new();
        let id = super::next_device_id();
        let mut n = 0;
        // let new = ComputeDevice { cpu, id, tracing: t, cache: Self::get_cache(&mut self) };
        let cache = Rc::new([0; L3_CACHE_MAX]);
        while n != crate::emu::CPU_MAX {
            let mut cg = ComputeGroup::new(CPU_CORES,Rc::clone(&cache),t,(id, n as u16));
            cpu.push(cg);
            n += 1;
        }
        info!("Spun up {n} cpu's");

        ComputeDevice { cpu, id, tracing: t, cache }
    }

    pub fn tick(&mut self) -> Result<Status, &'static str> {
        for elem in self.cpu.iter_mut() {
            elem.tick();
        }
        Ok(Status::Running)
    }
    pub fn push(&mut self, cpu: ComputeGroup) {
        self.cpu.push(cpu);
    }
}


