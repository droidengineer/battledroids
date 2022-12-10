//! Processing Element
//! CPU Core thread analog. Unit of computation.
//! 
//! A PE has its own local registers, L1 cache (instruction and data memory),
//! and shares an L2 cache with other PEs in the `ComputeUnit`

use num_traits::FromPrimitive;

use crate::types::{addr_t, Value, register_t};

use super::register::RegisterFile;
use super::{ProcID, Register};
use super::isa::{ISA, code_t };

#[derive(Debug)]
/// The basic unit of work. 
pub struct ProcessingElement {
    pub registers: RegisterFile,
    pub program: [addr_t; crate::emu::L1_CACHE_MAX],
    pub data: [addr_t; crate::emu::L1_CACHE_MAX],
 //   pub cache: & 'static [addr_t; crate::emu::L2_CACHE_MAX],
    pub ids: (u16,u16,u16,u16),
    pub tracing: bool,
}
static mut pid: u16 = 1;
impl ProcessingElement {
    pub fn new(t: bool, ids: (u16,u16,u16, u16)) -> ProcessingElement {
        let id = ids.3;
        ProcessingElement { 
            registers: RegisterFile::default(), 
            program: [0; crate::emu::L1_CACHE_MAX], 
            data: [0; crate::emu::L1_CACHE_MAX],
   //         cache: &[0u16; crate::emu::L2_CACHE_MAX],
            ids: (ids.0, ids.1, ids.2, id),
            tracing: t,
        }
    }
    pub fn get(&mut self, i: Value) -> addr_t {
        match i {
            Value::Reg(r) => self.registers[r],
            Value::AtReg(r) => self.data[self.registers[r] as usize],
            Value::AtAddr(offset) => self.data[offset as usize],
            Value::ProgMem(a) => self.program[a as usize],
            Value::Peek(i) => {
                if i == 0 {
                    self.data[self.registers[Register::SP] as usize]
                } else {
                    self.data[i as usize]
                }
            },
            Value::Immediate(n) => n,
            Value::Litteral(n) => n,
            Value::IR => self.registers[Register::IR],
            Value::BP => self.registers[Register::BP],
            Value::SP => self.registers[Register::SP],
            Value::MP => self.registers[Register::MP],
            Value::PC => self.registers[Register::PC],
            
        }
    }
    pub fn set(&mut self, i: Value, v: addr_t) {
        match i {
            Value::Reg(r) => self.registers[r] = v,
            Value::AtReg(r) => self.data[self.registers[r] as usize] = v,
            Value::AtAddr(offset) => self.data[offset as usize] = v,
            Value::ProgMem(a) => self.program[a as usize] = v,
            Value::Peek(i) => {
                if i == 0 {

                } else {

                }
            },
            Value::Immediate(_) => (),
            Value::Litteral(_) => (),
            Value::IR => self.registers[Register::IR] = v,
            Value::BP => self.registers[Register::BP] = v,
            Value::SP => self.registers[Register::SP] = v,
            Value::MP => self.registers[Register::MP] = v,
            Value::PC => self.registers[Register::PC] = v,

        }
    }
    #[inline(always)]
    pub fn reg(&self, r: Register) -> register_t {
        self.registers[r]
    }    
    #[inline(always)]
    pub fn set_reg(&mut self, r: Register, i: register_t) {
        self.registers[r] = i;
    }
    #[inline(always)]
    pub fn read_program(&self, i: u16) -> addr_t {
        self.program[i as usize]
    }
    fn fetch(&mut self) -> addr_t {
        let instr = self.program[self.registers[Register::PC] as usize];
        self.registers[Register::PC] += 1;
        instr
    }
    pub fn get_procid(&mut self) -> ProcID {

        ProcID {
            instance_id: self.next_proc_id(),
            pe_id: self.ids.3,
            cu_id: self.ids.2,
            cg_id: self.ids.1,
            device_id: self.ids.0,
            machine_id: 1,
        }
    } 
    fn next_proc_id(&mut self) -> u16 {
       unsafe { let p = pid; pid += 1; p }
    }
    #[inline(always)]
    fn sp(&self) -> addr_t {
        self.registers[Register::SP]
    }    
    #[inline(always)]
    fn pc(&self) -> addr_t {
        self.registers[Register::PC]
    }
    #[inline(always)]
    pub fn push(&mut self, v: u16) {
        let sp = self.sp();
        self.data[sp as usize] = v;
        self.registers[Register::SP] = sp.wrapping_sub(1);
    }
    #[inline(always)]
    pub fn pop(&mut self) -> u16 {
        let sp = self.sp().wrapping_add(1);
        let ret = self.data[sp as usize];
        self.registers[Register::SP] = sp;
        ret
    }
    
}

impl ISA for ProcessingElement {
    type Item = code_t;

    fn version(&self) -> &str {
        "BattleDroid ISA v1.0"
    }
    fn get_opcode(&self, instr: code_t) -> code_t {
        (instr & super::isa::MASK_OP) >> 11
    }
    fn decode(&self, instr: code_t) {
        todo!()
    }
    //
    fn halt(&self, args: &[u16]) {
        todo!()
    }
    fn brk(&self, args: &[u16]) {
        todo!()
    }
    #[inline(always)]
    fn add(&mut self, args: &[u16]) {
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();
        self.registers[rd] = self.registers[rd] + self.registers[rs];
    }
    #[inline(always)]
    fn addi(&mut self, args: &[u16]) {  // addi rd, imm11
        let rd = Register::from_u16(args[0]).unwrap();
        self.registers[rd] = self.reg(rd) + args[1];
    }
    #[inline(always)]
    fn and(&mut self, args: &[u16]) {  // and rd, rs
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) & self.reg(rs);      
    }
    #[inline(always)]
    fn cbr(&mut self, args: &[u16]) {  // cbr rd, 3
        let rd = Register::from_u16(args[0]).unwrap();
        todo!()
    }
    #[inline(always)]
    fn clr(&mut self, args: &[u16]) {  // clr rd
        let rd = Register::from_u16(args[0]).unwrap();
        todo!()
    }
    #[inline(always)]
    fn com(&mut self, args: &[u16]) {  // com rd
        let rd = Register::from_u16(args[0]).unwrap();
        todo!()
    }
    #[inline(always)]
    fn dec(&mut self, args: &[u16]) {  // dec rd
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] -= 1;
    }
    #[inline(always)]
    fn div(&mut self, args: &[u16]) {  // div rd, rs
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) / self.reg(rs);      
    }
    #[inline(always)]
    fn inc(&mut self, args: &[u16]) {  // inc rd
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] += 1;
    }
    #[inline(always)]
    fn ldd(&mut self, args: &[u16]) {  // ldd rd, k
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] = self.data[args[1] as usize];
    }
    #[inline(always)]
    fn ldi(&mut self, args: &[u16]) {  // ldi rd, k
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] = args[1];
    }
    #[inline(always)]
    fn mov(&mut self, args: &[u16]) {  // mov rd, rs
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rs);      
    }
    #[inline(always)]
    fn mul(&mut self, args: &[u16]) {  // mul rd, rs
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) * self.reg(rs);      
    }

    #[inline(always)]
    fn or(&mut self, args: &[u16]) {  // or rd, rs
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) | self.reg(rs);      
    }
    #[inline(always)]
    fn ori(&mut self, args: &[u16]) {  // ori rd, imm11
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] = self.reg(rd) | args[1];
    }
    #[inline(always)]
    fn sub(&mut self, args: &[u16]) {  // sub rd, rs
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) - self.reg(rs);      
    }
    #[inline(always)]
    fn subi(&mut self, args: &[u16]) {  // subi rd, imm11
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] = self.reg(rd) - args[1];
    }
    #[inline(always)]
    fn xor(&mut self, args: &[u16]) {  // xor rd, rs
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) ^ self.reg(rs);      
    }
    #[inline(always)]
    fn push(&mut self, args: &[u16]) {
        let sp = self.sp();
        self.data[sp as usize] = args[0];
        self.registers[Register::SP] = sp.wrapping_sub(1);
    }
    #[inline(always)]
    fn pop(&mut self, args: &[u16]) {
        let sp = self.sp().wrapping_add(1);
        self.registers[Register::from_u16(args[0]).unwrap()] = self.data[sp as usize];
        self.registers[Register::SP] = sp;
    }

    // pseudocode
    fn nop(&mut self, args: &[u16]) {
        self.addi(&[0,0]);
    }

}
