//!
//! 
use std::vec::Vec;
use std::collections::{VecDeque, HashMap};

use crate::emu::register::{RegisterFile};
use crate::types::*;
use crate::types::isa::{ISA, code_t};
use crate::types::stack::Stack;
use crate::emu::{CPU_FREQ, CPU_CORES, PROC_SLICE, CPU_MEM_MAX};



#[derive(Debug, Default, PartialEq)]
pub enum Status {
   #[default] Halted, Running, Standby, Panic,
}
enum_from_primitive! {
#[derive(Debug)]
    pub enum SREG {
        C = 1,  // carry flag 0x01
        Z,      // zero flag 0x02
        N,      // negative flag 0x04
        V,      // Overflow 0x08
        S,      // signed 0x10
        H,      // half carry flag 0x20
        T,      // transfer bit 0x40
        I,      // global interrupt enable 0x80
    }
}
#[derive(Debug)]
pub struct CallFrame {
    locals: HashMap<String, addr_t>,     // local variables
    pub ra: addr_t,      // return address
}

#[derive(Debug)]
pub struct RegisterBlock {
    pub ir: addr_t,     // instruction register
    pub bp: addr_t,     // base pointer
    pub sp: addr_t,     // stack pointer
    pub mp: addr_t,     // mark stack pointer
    pub pc: addr_t,     // program counter
}
impl Default for RegisterBlock {
    fn default() -> RegisterBlock {
        RegisterBlock { ir: 0, bp: 0, sp: 0, mp: 0, pc: 0, }
    }
}

#[derive(Debug)]
pub struct Processor {
    pub reg: RegisterBlock, // registers
    pub mem: [addr_t; CPU_MEM_MAX], // embedded mem ~ 1KB
    pub stack: Stack<addr_t>,   // cpu stack
    pub ia: addr_t,         // interrupt address
    pub int_queue: VecDeque<addr_t>,    // interrupt vector
    pub sreg: u8,   // status register
    pub gpio: RegisterFile, // general purpose i/o registers
    pub call_stack: Vec<CallFrame>, // call stack
    pub status: Status,     // cpu status
    pub tracing: bool,      // cpu-level tracing
    pub freq: usize,        // cpu frequency
    pub num_cores: usize,   // number of active processes
    pub cores: Vec<ProcessRecord>, // processes/cores
    pub curr_proc: usize,
    pub next_proc: usize,
    pub slice: usize,       // process time slice amount
    pub curr_tick: usize,   // tick count
}

#[derive(Debug)]
pub struct ProcessRecord {
    pub reg: RegisterBlock, // shadow registers

    pub next: usize,        // ring pointer
    pub queue: usize,       // linked, waiting on semaphore
    pub stack_max: addr_t,  // memory limits
    pub stack_min: addr_t,
    pub ready: bool,        // process ready flag
}
impl Default for ProcessRecord {
    fn default() -> ProcessRecord {
        ProcessRecord { reg: RegisterBlock::default(), next: 0, queue: 0, stack_max: 0, stack_min: 0, ready: true }
    }
}

impl Default for Processor {
    fn default() -> Processor {
        let frame: CallFrame = CallFrame::new(0);
        let mut call_stack = Vec::new();
        call_stack.push(frame);
        Processor {
            reg: RegisterBlock::default(),
            mem: [0; CPU_MEM_MAX],
            stack: Stack::new(),
            ia: 0,
            int_queue: VecDeque::new(),
            sreg: 0,
            gpio: RegisterFile::default(),
            call_stack,
            status: Status::Halted,
            tracing: false,
            freq: CPU_FREQ,
            num_cores: CPU_CORES,
            cores: Vec::new(),
            curr_proc: 0, next_proc: 0,
            slice: PROC_SLICE,       // time slice
            curr_tick: 0,
        }
    }
}
//use rand::prelude::*;
impl Processor {
    // From current process, traverse ring of core descriptors to next ready process
    pub fn choose_process(&mut self) {
        if self.slice != 0 { self.slice -= 1; return; }
        while !self.cores[self.next_proc].ready {
            self.next_proc = self.cores[self.next_proc].next;
        }
        if self.next_proc != self.curr_proc { self.swap_registers(); }
        if rand::random() {
            self.slice = (rand::random::<usize>() % PROC_SLICE) + 3;
        }
    }
    // Save current processor registers; restore from next process
    pub fn swap_registers(&mut self) {
        self.cores[self.curr_proc].reg.bp = self.reg.bp; self.reg.bp = self.cores[self.next_proc].reg.bp;
        self.cores[self.curr_proc].reg.mp = self.reg.mp; self.reg.mp = self.cores[self.next_proc].reg.mp;
        self.cores[self.curr_proc].reg.sp = self.reg.sp; self.reg.sp = self.cores[self.next_proc].reg.sp;
        self.cores[self.curr_proc].reg.pc = self.reg.pc; self.reg.pc = self.cores[self.next_proc].reg.pc;
        
    }
    pub fn new(t: bool) -> Processor {
        let mut cpu = Processor::default();
        cpu.tracing = t;
        cpu
    }
    pub fn tick(&mut self) -> Result<Status, &'static str> {
        // fetch
        let instr = self.fetch();

        
        self.curr_tick += 1;

        Ok(Status::Standby)
    }
    // 
    pub fn sreg_is_set(&self, flag: SREG) -> bool {
        self.sreg & (flag as u8) != 0
    }
    fn inbounds(&mut self, idx: addr_t ) -> bool {
        if idx < self.cores[self.curr_proc].stack_min || idx >= CPU_MEM_MAX as u16 { self.status = Status::Panic; }
        return self.status != Status::Panic
    }
    fn fetch(&mut self) -> addr_t {         //{ 0 /*self.stack.0.as_slice()[self.reg.pc as usize]*/ }
        let instr = self.mem[self.reg.pc as usize];
        self.reg.pc += 1;
        instr
    }
    pub fn decode_r(&self, instr: code_t ) -> (u16, u16, u16, u16) {
        let opcode = (instr & isa::MASK_OP) >> 11;
        let rd = (instr & isa::MASK_RD) >> 7;
        let rs = (instr & isa::MASK_RS) >> 3;
        let ext = instr & isa::MASK_EXT;
        (opcode,rd,rs,ext)
    }
    pub fn decode_a(&self, instr: code_t) -> (u16, u16, u16) {
        let opcode = (instr & isa::MASK_OP) >> 11;
        let rd = (instr & isa::MASK_RD) >> 7;
        let imm7 = instr & isa::MASK_IMM7;
        (opcode,rd,imm7)    
    }
    pub fn decode_j(&self, instr: code_t) -> (u16, u16) {
        let opcode = (instr & isa::MASK_OP) >> 11;
        let imm11 = instr & isa::MASK_IMM11;
        (opcode,imm11)
    }
    pub fn decode_i(&self, instr: u16) -> (u16, u16, u16) {
        let opcode = (instr & isa::MASK_OP) >> 11;
        let xd = (instr & isa::MASK_XD) >> 8;
        let imm8: u16 = instr & 0xFF;
        (opcode,xd,imm8)
    }
    pub fn decode_b(&self, instr: u16) -> (u16, u16, u16) {
        let opcode = (instr & isa::MASK_OP) >> 11;
        let ext2 = (instr & isa::MASK_EXT2) >> 9;
        let imm9: u16 = instr & isa::MASK_IMM9;
        (opcode,ext2,imm9)
    }
    pub fn decode_e(&mut self, instr: code_t) -> (u16, u16, u16, u16, u16) {
        let (opcode,rd,rs,ext) = self.decode_r(instr);
        let imm16 = self.fetch();
        (opcode,rd,rs,ext,imm16)
    }
}

impl ISA for Processor {
    type Item = addr_t;

    fn version(&self) -> &str {
        "BattleDroid ISA v1.0"
    }
    fn decode(&self, instr: code_t) -> (u16, u16, u16, u16, u16) {
        let op = self.get_opcode(instr);
        match op {
            0 => {}
        }
  
        let (opcode, xd, imm8) = self.decode_i(instr);
    }
    fn get_opcode(&self, instr: code_t) -> u16 {
        (instr & isa::MASK_OP) >> 11
    }

    fn halt(&self, args: &[u16]) {
        todo!()
    }

    fn brk(&self, args: &[u16]) {
        todo!()
    }

    fn add(&self, args: &[u16]) {
        todo!()
    }

    fn addi(&self, args: &[u16]) {
        todo!()
    }

    fn noop(&self, args: &[u16]) {
        todo!()
    }
}

impl CallFrame {
    pub fn new(ra: addr_t) -> CallFrame {
        CallFrame {
            locals: HashMap::new(),
            ra
        }
    }
    pub fn get_local(&self, name: &str) -> Option<&addr_t> {
        self.locals.get(name)
    }
    pub fn set_local(&mut self, name: &str, value: addr_t) {
        let name = String::from(name);
        self.locals.insert(name,value);
    }
}



