//!
//! 

use crate::types::*;
pub use crate::emu::mem::ram::SRAM;

pub mod ram;

#[derive(Debug)]
pub enum Type {
    SRAM, DDRAM, EEPROM, FLASH
}

pub trait Memory {
    fn get_type(&self) -> Type;
    fn capacity(&self) -> usize;
    fn used(&self) -> usize;
    fn free(&self) -> usize {
        self.capacity() - self.used()
    }
    fn init(&mut self);
    fn as_mut_ptr(&mut self) -> *mut addr_t;
    fn copy<'a, I: Iterator<Item=&'a addr_t>>(&mut self, items: I, offset: addr_t);
   // fn iter_wrap<'a, I: Iterator<Item=&'a addr_t>>(&self, offset: addr_t) -> dyn Iterator<Item=&'a addr_t>;
}
