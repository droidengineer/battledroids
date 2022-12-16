pub mod mem;
pub mod cpu;
pub mod code;
pub mod machine;
pub mod cart;

pub use crate::emu::cpu::register::RegisterFile;
pub use crate::emu::cpu::*;
pub use crate::emu::machine::Machine;
pub use crate::emu::mem::{Memory, Type };
pub use crate::emu::mem::ram::SRAM;
pub use crate::emu::cart::Cartridge;

// pub const MASK_OP: u16 = 0xF800;
// pub const MASK_RD: u16 = 0x780;
// pub const MASK_RS: u16 = 0x78;
// pub const MASK_EXT: u16 = 0x7;
// pub const MASK_XD: u16 = 0x700;
// pub const MASK_IMM7: u16 = 0x7F;
// pub const MASK_IMM9: u16 = 0x1FF;
// pub const MASK_IMM11: u16 = 0x7FF;
// pub const MASK_EXT2: u16 = 0x600;

pub const L1_CACHE_MAX: usize = 1024;
pub const L2_CACHE_MAX: usize = 1024 * CORE_THREADS;
pub const L3_CACHE_MAX: usize = L2_CACHE_MAX * CPU_CORES;

pub const SRAM_MAX: usize = 0x8000;    // 32768 (32k)
pub const FLASH_MAX: usize = 0x10000;   // 65536 (64k)
pub const CART_MEM_MAX: usize = 0x10000;
pub const CPU_FREQ: usize = 25_000_000; // 25 MHz
pub const CORE_THREADS: usize = 4;
pub const CPU_CORES: usize = 8;
pub const CPU_MAX: usize = 1;       // num avail cpus
pub const CPU_MEM_MAX: usize = 1024;    // 0x400
pub const PROC_SLICE: usize = 8;
pub const PROC_DISPLAY: usize = 5;  // limit on display
pub const PROC_MAX: usize = 10;     // limit on concurrent processes


#[derive(Debug, Default)]
pub enum Status {
    #[default] Halted, Waiting, Standby, Running, Panic,
}