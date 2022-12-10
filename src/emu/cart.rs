//! A Cartridge implementation
//! 
//! A cartridge is 64KB
use std::ops::*;
use crate::types::*;
use crate::emu::mem::Memory;
use super::CART_MEM_MAX;
use super::mem::Type;

#[derive(Debug)]
pub struct Cartridge([addr_t; CART_MEM_MAX]);

impl Cartridge {
    pub fn new() -> Cartridge {
        Cartridge([0; CART_MEM_MAX])
    }
    pub fn iter_wrap(&self, offset: addr_t) -> impl Iterator + '_ {
        self.0.iter()
        .skip(offset as usize)
        .chain(self.0.iter()
                    .take(offset as usize))           
    }
}
impl Memory for Cartridge {
    fn get_type(&self) -> Type { Type::FLASH }
    fn capacity(&self) -> usize { CART_MEM_MAX }
    fn free(&self) -> usize { self.capacity() - self.0.len()}
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
    
    fn used(&self) -> usize {
        self.capacity() - self.free()
    }
}
impl Deref for Cartridge {
    type Target = [addr_t; CART_MEM_MAX];

    fn deref(&self) -> &[addr_t; CART_MEM_MAX] {
        &self.0
    }
}
impl DerefMut for Cartridge {
    fn deref_mut(&mut self) -> &mut [addr_t; CART_MEM_MAX] {
        &mut self.0
    }
}
impl Index<addr_t> for Cartridge {
    type Output = addr_t;

    fn index(&self, i: addr_t) -> &addr_t {
        &self.0[i as usize]
    }
}



impl IndexMut<addr_t> for Cartridge {
    fn index_mut(&mut self, i: addr_t) -> &mut addr_t {
        &mut self.0[i as usize]
    }
}