//! Compute Unit
//! 

use crate::types::addr_t;

use super::{ProcessingElement};


#[derive(Debug)]
/// The `ComputeUnit` contains one or more `ProcessingElement`
/// Manages work on the threads
pub struct ComputeUnit {
    pub threads: Vec<ProcessingElement>,
    pub cache: [addr_t; crate::emu::L2_CACHE_MAX],
    pub id: u16,
    pub tracing: bool,
    num_threads: u8,
}
impl ComputeUnit {
    pub fn new(num_elem: u8, t: bool, ids: (u16,u16,u16)) -> ComputeUnit {
        let mut threads = Vec::new();
        let cu_id = ids.2;
        let mut n = 0;
        while n != num_elem {
            let mut pe = ProcessingElement::new(t,(ids.0,ids.1,cu_id,n as u16));
            let proc_id = pe.get_procid();
           println!("{} => {}", proc_id, proc_id.format());
            threads.push(pe);
            n += 1;
        }
        println!("Spun up {n} threads");

        ComputeUnit { threads, cache: [0; crate::emu::L2_CACHE_MAX], id: cu_id, tracing: false, num_threads: num_elem, }
    }
    pub fn next_pe_id(&mut self) -> u16 { self.num_threads += 1; self.num_threads as u16 }
    pub fn add_thread(&mut self, thread: ProcessingElement) {
        self.threads.push(thread);
    }
    pub fn get_cache(&self) -> &[addr_t; crate::emu::L2_CACHE_MAX] {
        &self.cache
    }
    pub fn num_threads(&self) -> usize {
        self.threads.len()
    }
}

