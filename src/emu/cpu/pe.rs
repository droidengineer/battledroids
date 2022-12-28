//! Processing Element
//! CPU Core thread analog. Unit of computation.
//! 
//! A PE has its own local registers, L1 cache (instruction and data memory),
//! and shares an L2 cache with other PEs in the `ComputeUnit`

use std::arch::asm;
use std::collections::HashMap;
use std::rc::Rc;

use num_traits::FromPrimitive;
use regex::internal::Inst;

use crate::emu::{INSTRUCTIONS_MAX, SRAM_MAX, L2_CACHE_MAX, FLASH_MAX};
use crate::emu::builder::Builder;
use crate::types::*;
use crate::emu::cpu::SREG;
use super::register::RegisterFile;
use super::{ProcID, Register};
use super::isa::{ISA, Instruction, Instruction::*, MASK_OP, MASK_RD, MASK_RS, MASK_EXT, MASK_XD, op, };
use super::Status;

#[derive(Debug)]
/// Used to hold local variables and return address for scopes
pub struct CallFrame {
    locals: HashMap<String, addr_t>,
    pub ret_addr: addr_t,
}
impl CallFrame {
    pub fn new(ret_addr: addr_t) -> Self {
        CallFrame { locals: HashMap::new(), ret_addr }
    }
    pub fn get_local(&self, name: &str) -> Option<&addr_t> {
        self.locals.get(name)
    }
    pub fn set_local(&mut self, name: &str, v: addr_t) {
        self.locals.insert(String::from(name), v);
    }
}

#[derive(Debug,Clone)]
/// The basic unit of work. Analog to core threads.
/// Contains all private memory location environments.
/// 
/// * A `RegisterFile` containing all GPIO and special registers.
/// * A separate program and data space
/// * A shared L2 cache
/// * A tuple of Ownership ID's
/// * A `Status` used for state checking
pub struct ProcessingElement {
    pub registers: RegisterFile,
    pub program: [addr_t; crate::emu::FLASH_MAX],
    pub data: [addr_t; crate::emu::SRAM_MAX],
    pub cache: Rc<[addr_t; crate::emu::L2_CACHE_MAX]>,
    pub code: [Instruction; INSTRUCTIONS_MAX],
    pub tracing: bool,
    pub status: Status,
}
static mut pid: u16 = 1;
impl ProcessingElement {
    pub fn new(cache: Rc<[addr_t; crate::emu::L2_CACHE_MAX]>, t: bool, ids: (u16,u16,u16, u16)) -> ProcessingElement {
        trace!("ProcessingElement::new({}, {}, {:?})", cache.len(), t, ids);
        let mut registers = RegisterFile::default();
        registers[Register::PE_ID] = ids.3;
        registers[Register::CU_ID] = ids.2;
        registers[Register::CG_ID] = ids.1;
        registers[Register::DEVICE_ID] = ids.0;
        ProcessingElement { 
            registers, 
            program: [0; crate::emu::FLASH_MAX], 
            data: [0; crate::emu::SRAM_MAX],
            cache,
            code: [Instruction::HALT; INSTRUCTIONS_MAX],
            tracing: t,
            status: Status::Running,
        }  
    }
    pub fn tick_from_code(&mut self) -> bool {
        if self.status == Status::Halted { return false; }
        else if self.status != Status::Sleeping {
            let instr = self.fetch_from_code();
            println!("fetched: {:?}", instr);
            if instr == Instruction::HALT {
                return false;
            }
            self.handle_instruction(instr);

        }
        true
    }
    pub fn tick(&mut self) -> bool {
        if self.status == Status::Halted { return false; }
        else if self.status != Status::Sleeping {
            let code = self.fetch();
            let mut instr = Instruction::UnknownOp(code);

            if op::code::from(code).need_word() { 
                let imm = self.fetch();
                let code = ((code as u32) << 16) | (imm as u32);
                instr = Instruction::from(code);
            } else {
                instr = Instruction::from(code);
            }

            self.handle_instruction(instr);
        }
        true
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
            Value::IR => self.registers[Register::IP],
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
            Value::IR => self.registers[Register::IP] = v,
            Value::BP => self.registers[Register::BP] = v,
            Value::SP => self.registers[Register::SP] = v,
            Value::MP => self.registers[Register::MP] = v,
            Value::PC => self.registers[Register::PC] = v,

        }
    }

    #[inline(always)]
    pub fn data(&self, i: addr_t) -> addr_t {
        self.data[i as usize]
    }
    #[inline(always)]
    pub fn set_data(&mut self, i: addr_t, v: addr_t) {
        self.data[i as usize] = v;
    }    

    #[inline(always)]
    pub fn reg(&self, r: Register) -> register_t {
        self.registers[r]
    }    
    #[inline(always)]
    pub fn reg_mut(&mut self, r: Register) -> &mut register_t {
        &mut self.registers[r]
    }
    #[inline(always)]
    pub fn set_reg(&mut self, r: Register, i: register_t) {
        self.registers[r] = i;
    }
    #[inline(always)]
    pub fn read_program(&self, i: u16) -> addr_t {
        self.program[i as usize]
    }
    #[inline(always)]
    pub fn gpio(&mut self) -> &mut RegisterFile {
        &mut self.registers
    }
    #[inline(always)]
    fn fetch_from_code(&mut self) -> Instruction {
        trace!("fetch_from_code(): ip: {:X}", self.ip());
        let instr = self.code[self.registers[Register::IP] as usize];
        self.registers[Register::IP] += 1;
        instr
    }
    #[inline(always)]
    fn fetch(&mut self) -> addr_t {
        let instr = self.program[self.registers[Register::PC] as usize];
        self.registers[Register::PC] += 1;
        instr
    }
    pub fn get_procid(&mut self) -> ProcID {

        ProcID {
            instance_id: self.next_proc_id(),
            pe_id: self.registers[Register::PE_ID],
            cu_id: self.registers[Register::CU_ID],
            cg_id: self.registers[Register::CG_ID],
            device_id: self.registers[Register::DEVICE_ID],
            machine_id: 1,
        }
    } 
    fn next_proc_id(&mut self) -> u16 {
       unsafe { 
        static mut pid: u16 = 0;
        let p = pid;
        pid += 1;
        p
       }
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
    fn ip(&self) -> register_t { self.registers[Register::IP] }
    #[inline(always)]
    fn sreg(&mut self) -> &mut register_t { &mut self.registers[Register::SREG] }

    #[inline(always)]
    pub fn push(&mut self, v: u16) {
        let sp = self.registers[Register::SP];
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
    pub fn from_builder(&mut self, builder: &Builder) {
        trace!("from_builder({:?}",builder);
        let mut code = &mut builder.instructions.clone();
        code.resize(INSTRUCTIONS_MAX, NOP);
        self.code.copy_from_slice(&code);
        let mut data = &mut builder.data.clone();
        data.resize(SRAM_MAX, 0);
        self.data.copy_from_slice(&data);
    }
    
}

impl ISA for ProcessingElement {
    type Item = code_t;

    fn version(&self) -> &str {
        "BattleDroid ISA v1.0"
    }
    fn get_opcode(&self, instr: code_t) -> op::code {
        let opcode = op::code::from(instr);
        opcode
    }
    fn encode(&self, instr: Instruction) -> code_t {
        todo!()
    }
    fn decode(&self, instr: code_t) -> Instruction{
        let opcode = self.get_opcode(instr);

        todo!()
    }

    #[inline(always)]
    /// Instruction handler
    fn handle_instruction(&mut self, i: Instruction) {
        trace!("ProcessingElement::handle_instruction({:?})", i);
  //      assert_ne!(i,Instruction::UnknownOp(0));
        debug!("Executing: {:?}", i);
        match i {
            ADD(rd, rs) => {
                trace!("ADD {:?} {:?}", rd,rs);
                self.registers[rd] = self.registers[rd].wrapping_add(self.reg(rs));
                self.registers[Register::IP] += 1;
            },
            ADDI(rd,imm) => {
                trace!("ADDI {:?} {imm}", rd);
                self.registers[rd] = self.registers[rd].wrapping_add(imm);
                self.registers[Register::IP] += 1;
            },
            AND(rd, rs) => {
                trace!("AND {:?} {:?}", rd,rs);
                self.registers[rd] = self.registers[rd] & self.registers[rs];
                self.registers[Register::IP] += 1;
            },
            ASR(rs) => { 
                trace!("ASR {:?}", rs);
                self.registers[rs] = ((self.reg(rs) as i16) >> 1) as register_t;
                self.registers[Register::IP] += 1;
            },
            BCLR(bit) => {
                trace!("BCLR {bit}");
                bit::clr(self.registers[Register::SREG], bit);
                self.registers[Register::IP] += 1;
            },
            BREQ(imm) => { todo!() },
            BRGE(imm) => { todo!() },
            BRK => { todo!() },
            BRLO(imm) => { todo!() },
            BRLT(imm) => { todo!() },
            BRNE(imm) => { todo!() },
            BRSH(imm) => { todo!() },
            BSET(bit) => {
                trace!("BSET {bit}");
                bit::set(self.registers[Register::SREG], bit);
                self.registers[Register::IP] += 1;                
            },
            CALL(imm) => {
                trace!("CALL {imm}");
                let ret = self.ip() + 1;
                self.push(ret);
                // jump
                self.registers[Register::IP] = imm;
            },
            CBIO(addr,bit) => { todo!() },
            CBR(r,v) => { todo!() },
            CLR(r) => { todo!() },
            COM(r) => { 
                trace!("COM {:?}({})",r,self.reg(r));
                self.registers[r] = !self.reg(r);
                self.registers[Register::IP] += 1;
            },
            CPI(r,K) => { todo!() },
            CPSE(rd,rs) => { todo!() },
            DEC(r) => {
                trace!("DEC {:?}",r);
                self.registers[r] = self.reg(r).wrapping_sub(1);
                self.registers[Register::IP] += 1;
            },
            DIV(rd,rs) => {
                trace!("DIV {:?}({}) {:?}({})",rd,self.reg(rd),rs,self.reg(rs));
                if self.reg(rs) == 0 {
                    self.status = Status::DivZero;
                    return;
                }
                self.registers[rd] = self.reg(rd) / self.reg(rs);
                self.registers[Register::IP] += 1;
            },
            HALT => {
                trace!("HALT");
                self.status = Status::Halted;
            },
            IJMP => {
                trace!("<TODO> IJMP");
                todo!()
            },
            IN(rd, imm) => { todo!()},
            INC(r) => {
                trace!("INC {:?}({})",r,self.reg(r));
                if self.reg(r) == register_t::MAX {
                    self.status = Status::BadOp;
                }
                self.registers[r] = self.reg(r).wrapping_add(1);
                self.registers[Register::IP] += 1;            
            },
            JMP(imm) => {
                trace!("JMP {}",imm);
                self.registers[Register::IP] = imm;
            },
            LD(rd,rs) => {
                trace!("LD {:?}({}), {:?}({})",rd,self.reg(rd),rs,self.reg(rs));
                self.registers[rd] = self.data[self.reg(rs) as usize];
                self.registers[Register::IP] += 1; 
            },
            LDD(rd, imm) => {
                debug!("LDD {:?}({}), {}",rd,self.reg(rd),imm);
                self.registers[rd] = self.data[imm as usize];
                self.registers[Register::IP] += 1; 
            },
            LDI(rd, imm) => {
               debug!("LDI {:?}({}), {}",rd,self.reg(rd),imm);
               self.registers[rd] = imm;
               self.registers[Register::IP] += 1; 
            },
            LDP(rd, imm) => {
                trace!("LDP {:?}({}), {}",rd,self.reg(rd),imm);
                self.registers[rd] = self.program[imm as usize];
                self.registers[Register::IP] += 1;            
            },
            LSL(rd) => { 
                trace!("LSL {:?}({})",rd,self.reg(rd));

                todo!();
            },
            LSR(rd) => { 
                trace!("LSR {:?}({})",rd,self.reg(rd));

                todo!(); 
            },
            MOV(rd, rs) => {
                trace!("MOV {:?}({}), {:?}({})",rd,self.reg(rd),rs,self.reg(rs));
                self.registers[rd] = self.registers[rs];
                self.registers[Register::IP] += 1;            
            },
            MUL(rd, rs) => {
                trace!("MUL {:?}({}), {:?}({})",rd,self.reg(rd),rs,self.reg(rs));
                self.registers[rd] = (self.registers[rd] * self.registers[rs]) % u16::MAX;
                self.registers[Register::IP] += 1;            
            },
            NEG(r) => {
                trace!("NEG {:?}({})",r,self.reg(r));
                self.registers[r] = (0 as i16).wrapping_sub(self.reg(r) as i16) as u16;
                self.registers[Register::IP] = self.ip().wrapping_add(1);
            },
            NOP => {
                trace!("NOP");
                self.nop(&[0]);
                self.registers[Register::IP] += 1;
            },
            OR(rd, rs) => {
                trace!("OR {:?}({}), {:?}({})",rd,self.reg(rd),rs,self.reg(rs));
                self.registers[rd] = (self.registers[rd] | self.registers[rs]) % u16::MAX;
                self.registers[Register::IP] += 1;                  
            },
            ORI(rd,imm) => {
                trace!("ORI {:?}({}), {}",rd,self.reg(rd),imm);
                self.registers[rd] = self.registers[rd] | imm as u16;
                self.registers[Register::IP] += 1;               
            },
            OUT(data, rd) => {
                trace!("OUT {data}, {:?}", rd);
                todo!();
            },
            POP(rd) => {
                trace!("POP {:?}({})", rd, self.reg(rd));
                let val = self.pop();
                self.registers[rd] = val;
                self.registers[Register::IP] += 1;               
            },
            PUSH(rd) => {
                trace!("PUSH {:?}({})", rd, self.reg(rd));
                let val = self.registers[rd];
                self.push(val);
                self.registers[Register::IP] += 1;               
            },
            RCALL(imm) => { todo!() },
            RET => {
                trace!("RET");
                self.registers[Register::IP] = self.pop();
            },
            RJMP(imm) => { todo!(); },
            SBIO(addr, bit) => {
                todo!()
            },
            SBR(rd, bit) => {
                trace!("SBR {:?}({}), {bit}", rd, self.reg(rd));
                bit::set(self.registers[rd], bit);
                self.registers[Register::IP] += 1;             
            },
            SET(rd) => {todo!();},
            ST(rd, rs) => {
                trace!("ST {:?}({}), {:?}({})",rd,self.reg(rd),rs,self.reg(rs));
                self.data[self.reg(rs) as usize] = self.reg(rd);
                self.registers[Register::IP] += 1;
            },
            STD(imm, rd) => {
                trace!("STD {imm}, {:?}({})",rd,self.reg(rd));
                self.data[self.reg(rd) as usize] = imm;
                self.registers[Register::IP] += 1;
            },
            STP(imm, rd) => {
                trace!("STP {imm}, {:?}({})",rd,self.reg(rd));
                self.program[self.reg(rd) as usize] = imm;
                self.registers[Register::IP] += 1;                
            },
            SUB(rd, rs) => {
                trace!("SUB {:?}({}), {:?}({})",rd,self.reg(rd),rs,self.reg(rs));
                self.registers[rd] = self.reg(rd).wrapping_sub(self.reg(rs));
                self.registers[Register::IP] += 1;                
            },
            SUBI(rd, imm) => {
                trace!("SUBI {:?}({}), {imm}",rd,self.reg(rd));
                self.registers[rd] = self.reg(rd).wrapping_sub(imm);
                self.registers[Register::IP] += 1;  
            },
            XOR(rd, rs) => {
                trace!("XOR {:?}({}), {:?}({})",rd,self.reg(rd),rs,self.reg(rs));
                self.registers[rd] = self.reg(rd) ^ self.reg(rs);
                self.registers[Register::IP] += 1;                 
            },
            i@_ => panic!("ip: {:#x}, Unknown Instruction: {:?}",self.reg(Register::IP),i)
        }
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
        //self.registers[rd] = self.rd + args[1];
    }
    #[inline(always)] /// *and rd, rs* reg(|`rd = rd & rs`| **Local AND**
    fn and(&mut self, args: &[u16]) {  // and rd, rs
        let rd = Register::from_u16(args[0]).unwrap();
        let rs = Register::from_u16(args[1]).unwrap();  
        self.registers[rd] = self.reg(rd) & self.reg(rs);      
    }
    #[inline(always)] /// *bclr bit* |`SREG &= !(1<<bit)`| **Clear bit in `SREG`**
    fn bclr(&mut self, args: &[code_t]) {
       bit::clr(self.registers[Register::SREG], args[0] as u8);
    }
    #[inline(always)] /// *bset bit* |`SREG |= 1<<bit`| **Set bit in `SREG`**
    fn bset(&mut self, args: &[code_t]) {
        bit::set(self.registers[Register::SREG], args[0] as u8);
    }
    #[inline(always)] /// *call k* |`PC = k`|**Call procedure**
    fn call(&mut self, args: &[code_t]) {
        todo!()
    }
    #[inline(always)] /// *cbio reg_io, bit* - **Clear Bit In I/O Register**
    fn cbio(&mut self, args: &[code_t]) { 
        todo!()
    }
    #[inline(always)] /// *cbr rd, bit* |`rd &= !1<<bit`| **Clear bit in register
    fn cbr(&mut self, args: &[u16]) {  
        let rd = Register::from_u16(args[0]).unwrap();
        bit::clr(self.registers[rd], args[0] as u8);
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
    #[inline(always)] /// *jmp k* |`PC = k`| **Jump to address**
    fn jmp(&mut self, args: &[code_t]) {
        todo!()
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
    #[inline(always)] /// *ret* |`PC = SP`| **Return from procedure**
    fn ret(&mut self, args: &[code_t]) {
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
        trace!("nop");
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
impl Default for ProcessingElement {
    fn default() -> Self {
        trace!("ProcessingElement::default()");
        ProcessingElement::new(Rc::new([0; L2_CACHE_MAX]), true, (1,1,1,1))
        // let mut registers = RegisterFile::default();
        // registers[Register::PE_ID] = 1;
        // registers[Register::CU_ID] = 1;
        // registers[Register::CG_ID] = 1;
        // registers[Register::DEVICE_ID] = 1;        
        // Self { 
        //     registers, 
        //     program: [0; FLASH_MAX], 
        //     data: [0; SRAM_MAX], 
        //     cache: Rc::new([0; L2_CACHE_MAX]), 
        //     code: [NOP; INSTRUCTIONS_MAX], 
        //     tracing: true, 
        //     status: Status::Running 
        // }
    }
}

#[cfg(test)]
mod test {
    use std::str::FromStr;

    use crate::{emu::{builder::Builder, Register, isa::ISA}, asm::Instruction};
    use super::ProcessingElement;

    // from avr-vm
    macro_rules! emulate {
        ($code: expr; reg: $($reg: expr => $regval: expr), *; expect: $($regexp: expr => $regexpval: expr), *; flags: $flagval: expr) => {{
            let mut pe = create($code);
            $( *pe.reg_mut(Register::from($reg)) = $regval;)*
           //$(pe.registers[$reg] = $regval;)*
            while pe.tick_from_code() {}
            $(assert_eq!(pe.reg(Register::from($regexp)), $regexpval);)*
            assert_eq!($flagval, $flagval, "flags: {:#b}", $flagval);
        }};
    }

    #[test]
    fn from_builder() {
        let mut bldr = Builder::new();
        bldr.push("LDI R0, 100");
        bldr.push("LDI R1, 23");
        bldr.push("ADD R0, R1");
        let mut pe = ProcessingElement::default();
        pe.from_builder(&bldr);
        assert_eq!(pe.code[0],super::Instruction::LDI(crate::emu::Register::R0,100));
        assert_eq!(pe.code[1],super::Instruction::LDI(crate::emu::Register::R1,23));
        assert_eq!(pe.code[2],super::Instruction::ADD(crate::emu::Register::R0,crate::emu::Register::R1));
    }
    #[test]
    fn fetch_from_code() {
        let mut bldr = Builder::new();
        bldr.push("LDI R0, 100");
        bldr.push("LDI R1, 23");
        bldr.push("ADD R0, R1");
        let mut pe = ProcessingElement::default();
        pe.from_builder(&bldr);
        let instr = pe.fetch_from_code();
        println!("instr: {:?}", instr);
        let instr = pe.fetch_from_code();
        println!("instr: {:?}", instr);
        let instr = pe.fetch_from_code();
        println!("instr: {:?}", instr);
    }
    #[test]
    fn instruction_test() {
        let mut bldr = Builder::new();
        bldr.push("LDI R0, 100");
        bldr.push("LDI R1, 23");
        bldr.push("ADD R0, R1");
        bldr.push("SUBI R0, 25");
        let mut pe = ProcessingElement::default();
        pe.from_builder(&bldr);
        for i in pe.code {
            println!("instr: {:?}", i);
            if i == Instruction::NOP { break; }            
            pe.handle_instruction(i);
            println!("R0[{}] R1[{}]", pe.reg(Register::R0), pe.reg(Register::R1));


        }
        // loop {
        //     let mut i = pe.fetch_from_code();
        //     while i != Instruction::NOP {
        //         pe.handle_instruction(i);
        //         println!("instr: {:?}", i);
        //         println!("R0[{}] R1[{}]", pe.reg(Register::R0), pe.reg(Register::R1));
        //         i = pe.fetch_from_code();
        //     }
        // } 
    }

    #[test]
    fn add() {
        emulate!("add r0, r1";
                reg: 0 => 10, 1 => 7;
                expect: 0 => 17, 1 => 7;
                flags: 0b00100000
              );
    }
    #[test]
    fn addi() {
        emulate!("addi r4, 23";
                reg: 4 => 100;
                expect: 4 => 123;
                flags: 0b00100000
              );
    }
    #[test]
    fn and() {
        emulate!("and r3, r1";
                reg: 3 => 420, 1 => 10;
                expect: 3 => (420 & 10), 1 => 10;
                flags: 0b00100000
              );
    }
    #[test]
    fn asr() {
        emulate!("asr r0";
                reg: 0 => (-2 as i16) as u16;
                expect: 0 => (-1 as i16) as u16;
                flags: 0b00100000
              );
    }


    fn create(code: &str) -> ProcessingElement {
        let bldr = Builder::from_str(code).unwrap();
        let mut pe = ProcessingElement::default();
        pe.from_builder(&bldr);
        pe
    }
}