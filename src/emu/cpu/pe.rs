//! Processing Element
//! CPU Core thread analog. Unit of computation.
//! 
//! A PE has its own local registers, L1 cache (instruction and data memory),
//! and shares an L2 cache with other PEs in the `ComputeUnit`

use std::arch::asm;
use std::rc::Rc;

use num_traits::FromPrimitive;

use crate::types::{addr_t, Value, register_t, self};
use crate::emu::cpu::SREG;
use super::register::RegisterFile;
use super::{ProcID, Register};
use super::isa::{ISA, code_t };
use super::Status;

#[derive(Debug)]
/// The basic unit of work. 
pub struct ProcessingElement {
    pub registers: RegisterFile,
    pub program: [addr_t; crate::emu::L1_CACHE_MAX],
    pub data: [addr_t; crate::emu::L1_CACHE_MAX],
    pub cache: Rc<[addr_t; crate::emu::L2_CACHE_MAX]>,
    pub ids: (u16,u16,u16,u16),
    pub tracing: bool,
    pub status: Status,
}
static mut pid: u16 = 1;
impl ProcessingElement {
    pub fn new(t: bool, ids: (u16,u16,u16, u16)) -> ProcessingElement {
        let id = ids.3;
        ProcessingElement { 
            registers: RegisterFile::default(), 
            program: [0; crate::emu::L1_CACHE_MAX], 
            data: [0; crate::emu::L1_CACHE_MAX],
            cache: Rc::new([0u16; crate::emu::L2_CACHE_MAX]),
            ids: (ids.0, ids.1, ids.2, id),
            tracing: t,
            status: Status::Running,
        }
    }
    pub fn postmortem(&self, pc: addr_t) {
        let pcstr = format!("at {} in current process", pc);
        match self.status {
            Status::BadData => error!("Invalid data {}", pcstr),
            Status::BadFct  => error!("Function did not return value {}", pcstr),
            Status::BadMem  => error!("Memory violation {}", pcstr),
            Status::BadOp   => error!("Illegal opcode {}", pcstr),
            Status::BadIPC  => error!("Bad IPC operation {}", pcstr),
            Status::BadIdx  => error!("Subscript/index out of range {}", pcstr),
            Status::Deadlock => error!("Deadlock {}", pcstr),
            Status::DivZero => error!("Divide by zero {}", pcstr),
            Status::NoData  => error!("No more data {}", pcstr),

            _ => ()
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
    fn sp(&self) -> addr_t { self.registers[Register::SP] }    
    #[inline(always)]
    fn pc(&self) -> addr_t { self.registers[Register::PC] }
    #[inline(always)]
    fn mp(&self) -> addr_t { self.registers[Register::MP] }
    #[inline(always)]
    fn bp(&self) -> addr_t { self.registers[Register::BP] }
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
    #[inline(always)] /// *add rd, rs* |`rd = rd + rs`| **Add** 
    fn add(&mut self, args: &[u16]) {
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();
        self.registers[rd] = self.registers[rd] + self.registers[rs];
    }
    #[inline(always)] /// *add rd, imm11* |`rd = rd + imm11`| **Add Immediate**
    fn addi(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();
        self.registers[rd] = self.reg(rd) + args[1];
    }
    #[inline(always)] /// *and rd, rs* |`rd = rd & rs`| **Local AND**
    fn and(&mut self, args: &[u16]) {  // and rd, rs
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) & self.reg(rs);      
    }
    #[inline(always)] /// *bclr bit* |`SREG &= !(1<<bit)`| **Clear bit in `SREG`**
    fn bclr(&mut self, args: &[code_t]) {
        types::bit::clr(self.registers[Register::SREG], args[0] as u8);
    }
    #[inline(always)] /// *bset bit* |`SREG |= 1<<bit`| **Set bit in `SREG`**
    fn bset(&mut self, args: &[code_t]) {
        types::bit::set(self.registers[Register::SREG], args[0] as u8);
    }
    #[inline(always)] /// *cbio reg_io, bit* - **Clear Bit In I/O Register**
    fn cbio(&mut self, args: &[code_t]) { 
        todo!()
    }
    #[inline(always)] /// *cbr rd, bit* |`rd &= !1<<bit`| **Clear bit in register
    fn cbr(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();
        types::bit::clr(self.registers[rd], args[0] as u8);
    }
    #[inline(always)] /// *clr rd* |`SREG <TBD>`
    fn clr(&mut self, args: &[u16]) {  // clr rd
        let rd = Register::from_u16(args[0]).unwrap();
        todo!()
    }

    #[inline(always)] /// *com rd* |`Rd = $FF - Rd`| **One's complement**
    fn com(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();
        self.registers[rd] = 0xFFFF - self.registers[rd];
    }
    
    #[inline(always)] /// *dec rd* |`Rd = Rd - 1`| **Decrement**
    fn dec(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] -= 1;
    }
    #[inline(always)] /// *div rd, rs* |`Rd = Rd ÷ Rs`| **Divide**
    fn div(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) / self.reg(rs);      
    }
    #[inline(always)] /// *inc rd* |`Rd = Rd + 1`| **Increment**
    fn inc(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] += 1;
    }
    #[inline(always)] /// *ld rd, rs* |`Rd = data[Rs]`| **Load indirect f/ data space**
    fn ld(&mut self, args: &[code_t]) {
        let rd = Register::from_u16(args[0]).unwrap();   
        let rs = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] = self.data[rs.offset()];
    }
    #[inline(always)] /// *ldd rd, k* |`Rd = data[k]`| **Load direct f/ data space**
    fn ldd(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] = self.data[args[1] as usize];
    }
    #[inline(always)] /// *ldi rd, k* |`Rd = k`| **Load immediate**
    fn ldi(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] = args[1];
    }
    #[inline(always)] /// *ldp rd, k* |`Rd = program[k]`| **Load immediate f/ progmem**
    fn ldp(&mut self, args: &[code_t]) {
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] = self.program[args[1] as usize];
    } 
    #[inline(always)] /// *mov rd, rs* |`Rd = Rs`| **Copy registers**
    fn mov(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rs);      
    }
    #[inline(always)] /// *mul rd, rs* |`Rd = Rd ⨯ Rs`| **Multiply**
    fn mul(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) * self.reg(rs);      
    }

    #[inline(always)] /// *or rd, rs* |`Rd = Rd v Rs`| **Logical OR**
    fn or(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) | self.reg(rs);      
    }
    #[inline(always)] /// *ori rd, imm11* |`Rd = Rd v imm11`| **Logical OR w/ immediate**
    fn ori(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] = self.reg(rd) | args[1];
    }
    #[inline(always)] /// *nor rd, rs* |`Rd = Rd ⊕ Rs`| **Logical Exclusive OR**
    fn nor(&mut self, args: &[code_t]) {
        todo!()
    }
    #[inline(always)] /// *sbio * |``| **Set Bit(s) in IO Register**
    fn sbio(&mut self, args: &[code_t]) {
        todo!()
    }
    #[inline(always)] /// *sbio * |``| **Set Bit(s) in IO Register**
    fn sbr(&mut self, args: &[code_t]) {
        todo!()
    }
    #[inline(always)] /// *sbio * |``| **Set Bit(s) in IO Register**
    fn st(&mut self, args: &[code_t]) {
        todo!()
    }
    #[inline(always)] /// *sbio * |``| **Set Bit(s) in IO Register**
    fn std(&mut self, args: &[code_t]) {
        todo!()
    }
    #[inline(always)] /// *sbio * |``| **Set Bit(s) in IO Register**
    fn stp(&mut self, args: &[code_t]) {
        todo!()
    }
    #[inline(always)] /// *sub rd, rs* |`Rd = Rd - Rs`| **Subtract**
    fn sub(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) - self.reg(rs);      
    }
    #[inline(always)] /// *subi rd, imm11* |`Rd = Rd - imm11`| **Subtract w/ immediate**
    fn subi(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();   
        self.registers[rd] = self.reg(rd) - args[1];
    }
    #[inline(always)] /// *xor rd, rs* |`Rd = Rd ⊕ Rs`| **Logical Exclusive OR**
    fn xor(&mut self, args: &[u16]) {  
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
 //       self.addi(&[0,0]);
        unsafe { asm!("nop       ; NOP"); }
    }
    #[inline(always)] /// *cli* |`Rd = $FF - Rd`| **Clear Interrupt Enabled**
    fn cli(&mut self, args: &[code_t]) {
       //types::bit::clr(self.registers[Register::SREG],args[1] as u8);
        self.bclr(&[u16::from(*SREG::I)]);
    }
    #[inline(always)] /// *sei * |``| **Set Interrupt Enabled**
    fn sei(&mut self, args: &[code_t]) {
        self.bset(&[u16::from(*SREG::I)]);
    }


}
