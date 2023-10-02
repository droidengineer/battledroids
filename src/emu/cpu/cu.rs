//! Compute Unit
//! 

use std::rc::Rc;

use crate::{types::addr_t, emu::L2_CACHE_MAX};
use super::{ProcessingElement};


#[derive(Debug)]
/// The `ComputeUnit` contains one or more `ProcessingElement`
/// Manages work on the threads. CPU core analog.
/// Shares an L2 cache among all its `ProcessingElement`s
pub struct ComputeUnit {
    pub threads: Vec<ProcessingElement>,
    pub cache: Rc<[addr_t; L2_CACHE_MAX]>,
    pub id: u16,
    pub tracing: bool,
    num_threads: u8,
}
impl ComputeUnit {
    pub fn new(num_elem: u8, t: bool, ids: (u16,u16,u16)) -> ComputeUnit {
        trace!("ComputeUnit::new({:?}, {}, {:?})", num_elem, t, ids);
        let mut threads = Vec::new();
        let cu_id = ids.2;
        let mut n = 0;
        let cache = Rc::new([0;L2_CACHE_MAX]);
        debug!("L2 Cache References: {}",Rc::strong_count(&cache));
        while n != num_elem {
            debug!("inside loop: {n}, Ref Ct: {}", Rc::strong_count(&cache));
            let mut pe = ProcessingElement::new(Rc::clone(&cache), t,(ids.0,ids.1,cu_id,n as u16));
            let proc_id = pe.get_procid();
           println!("{} => {}", proc_id, proc_id.format());
            threads.push(pe);
            n += 1;
        }
        info!("Spun up {n} threads");
        ComputeUnit {threads,cache,id: cu_id, tracing: t, num_threads: num_elem}
    }
    pub fn tick(&mut self) -> bool {
        for thread in self.threads.iter_mut() {
            thread.tick();
        }
        true      
    }
    pub fn assign_work(&mut self, code: Vec<addr_t>) {

    }
    pub fn postmortem(&self, pc: addr_t) {
        for thread in self.threads.iter() {
            thread.postmortem(pc);
        }
    }
    pub fn refs(&self) -> usize {
        Rc::strong_count(&self.cache)
    }
    pub fn next_pe_id(&mut self) -> u16 { self.num_threads += 1; self.num_threads as u16 }
    pub fn add_thread(&mut self, thread: ProcessingElement ) {
        self.threads.push(thread);
    }
    pub fn get_cache(&self) -> &[addr_t; crate::emu::L2_CACHE_MAX] {
        &self.cache
    }
    pub fn num_threads(&self) -> usize {
        self.threads.len()
    }
}

