//! RAM Memories
//! 
//! 
/////////////////////////////[ SRAM ]

use std::ops::*;
use crate::types::*;
use crate::emu::{Type, SRAM_MAX};
use crate::emu::Memory;

#[derive(Debug)]
pub struct SRAM([addr_t; SRAM_MAX]);

impl SRAM {
    pub fn new() -> SRAM {
        SRAM([0;SRAM_MAX])
    }
    pub fn iter_wrap(&self, offset: addr_t) -> impl Iterator + '_ {
        self.0.iter()
        .skip(offset as usize)
        .chain(self.0.iter()
                    .take(offset as usize))           
    }
}

impl Memory for SRAM {
    fn get_type(&self) -> Type { Type::SRAM }
    fn capacity(&self) -> usize { SRAM_MAX }
    fn used(&self) -> usize { self.0.len()}
    fn init(&mut self) {
        self.0.fill_with(Default::default);
    }
    fn as_mut_ptr(&mut self) -> *mut addr_t {
        self.0.as_mut_ptr()
    }
    fn copy<'a, I: Iterator<Item=&'a addr_t>>(&mut self, items: I, offset: addr_t) {
        let (low, high) = self.0.split_at_mut(offset as usize);
        for (from, to) in items.zip(high.iter_mut().chain(low.iter_mut())) {
            *to = *from;
        }
    }
    
    fn free(&self) -> usize {
        self.capacity() - self.used()
    }
}

impl Deref for SRAM {
    type Target = [addr_t; SRAM_MAX];

    fn deref(&self) -> &[addr_t; SRAM_MAX] {
        &self.0
    }
}
impl DerefMut for SRAM {
    fn deref_mut(&mut self) -> &mut [addr_t; SRAM_MAX] {
        &mut self.0
    }
}
impl Index<addr_t> for SRAM {
    type Output = addr_t;

    fn index(&self, i: addr_t) -> &addr_t {
        &self.0[i as usize]
    }
}



impl IndexMut<addr_t> for SRAM {
    fn index_mut(&mut self, i: addr_t) -> &mut addr_t {
        &mut self.0[i as usize]
    }
}