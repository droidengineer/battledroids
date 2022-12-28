//! Code/Instruction Builder
//! 
//! Use this module to build code for your machine.
//! Can be converted directly to a `Code` entity that can then
//! be executed by the machine or dumped to bytecode.

use std::fs::File;
use std::ffi::OsString;
use std::io::{Read, Write};
use std::str::FromStr;

use crate::types::table::{Dictionary, Table};
use crate::types::to_byte_code::ToByteCode;
use crate::types::{addr_t, self};
use super::{FLASH_MAX, INSTRUCTIONS_MAX, SRAM_MAX};
use super::isa::{Instruction, ParseInstructionError};

#[derive(Debug)]
/// The Builder struct
/// 
/// Contains:
/// * a list of `Instruction`s that have been pushed into this builder
/// * a jumptable of labels
/// * a list of values to be stored in data space
pub struct Builder {
    pub instructions: Vec<Instruction>, // -> Code.code
    pub data: Vec<addr_t>, // -> Code.data
    pub labels: Dictionary<usize>, // -> Code.labels
}

impl Builder {
    pub fn new() -> Self {

        Builder {
            instructions: vec![Instruction::HALT; INSTRUCTIONS_MAX],
            data: vec![0; SRAM_MAX],
            labels: Dictionary::new(true),
        }
    }
    pub fn push(&mut self, src: &str) {
        self.instructions.push(Instruction::from_str(src).unwrap());
    }
    fn push_data(&mut self, data: addr_t) -> usize {
        let pos = self.data.iter().position(|d| d == &data);
        match pos {
            Some(pos) => pos,
            None => {
                self.data.push(data);
                self.data.len() - 1
            }
        }
    }

    pub fn from_asm_file(&mut self, file: OsString) -> &mut Self {
        let mut file = File::open(file).unwrap();
        //let mut bytes = Vec::new();
        let mut buf = String::new();
        let bytes = file.read_to_string(&mut buf).unwrap();
        trace!("Read {bytes} bytes");
        println!("{}", buf);

        //self = Builder::from_str(buf.as_str()).unwrap();
        let mut lines = buf.lines();
        while let Some(line)= lines.next() {
            println!("Line: {}",line);
            let i = Instruction::from_str(line).unwrap();
            self.instructions.push(i);
        }
        //file.read_to_end(&mut bytes).unwrap();
        //assert!(bytes.len() < FLASH_MAX);
        self
    }
    // avr-vm/util.rs
    pub fn asm_to_file(code: &str) -> OsString {
        let source_name = types::gen_tmp_name("bda");
    //    let object_name = types::gen_tmp_name("obj");
    //    let output_name = types::gen_tmp_name("exe");

        let mut infile = File::create(source_name.clone()).unwrap();
        infile.write_all(code.as_bytes()).unwrap();

        println!("asm_to_file({}): {:?}",code,code.as_bytes());
        println!("created source file: {:?}",source_name);
        source_name
    }
    pub fn label(&mut self, name: &str) {
        let idx = self.instructions.len();
        self.labels.insert(name,idx);
    }

    /// Return the length of the instruction vector
    /// Returns: number of instructions pushed so far
    pub fn len(&self) -> usize {
        self.instructions.len()
    }
    pub fn is_empty(&self) -> bool {
        self.instructions.is_empty()
    }

}

impl ToByteCode for Builder {
    fn to_byte_code(&self, mut buf: &mut dyn Write) {
        todo!()
    }
}

impl FromStr for Builder {
    type Err = ParseInstructionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let buf = String::from(s);
        let mut lines = buf.lines();
        let mut bldr = Builder::new();
        while let Some(line)= lines.next() {
            println!("Line: {}",line);
            let i = Instruction::from_str(line)?;
            bldr.instructions.push(i);
        }
        Ok(bldr)
    }
}

#[cfg(test)]
#[allow(dead_code)]
pub fn assemble(code: &str) -> Vec<u8> {
    let mut output = File::open(Builder::asm_to_file(code)).unwrap();
    let mut assembled = Vec::new();
    output.read_to_end(&mut assembled).unwrap();
    println!("Assembled: {:?}", assembled);
    assembled
}

#[cfg(test)]
mod test {
    use std::env::temp_dir;
    use std::fs::File;
    use std::io::{Write, BufReader, BufRead, Error};
    use crate::{types::table::Table, asm::Instruction};

    use super::Builder;


    #[test]
    fn push() {
        let mut builder = Builder::new();
        builder.push("LDI R0, 20");
        builder.push("LDI R1, 200");
        assert!(builder.len() == 2);
    }
    #[test]
    fn label() {
        let mut builder = Builder::new();
        builder.push("LDI R0, 20");
        builder.label("wow");
        assert_eq!(*builder.labels.get("wow").unwrap(), 2);
    }
    #[test]
    fn load_from_asm_file() -> Result<(), Error> {
        let mut builder = Builder::new();
        let mut file = temp_dir();
        println!("Temporary directory: {}", file.display());
        file.push("test.asm");
        let mut fl = File::create(&file)?;
        write!(fl, "LDI R0, 100\nLDI R1, 23\nADD R0, R1");
        builder.from_asm_file(file.into_os_string());
        assert_eq!(builder.instructions[0],Instruction::LDI(crate::emu::Register::R0,100));
        assert_eq!(builder.instructions[1],Instruction::LDI(crate::emu::Register::R1,23));
        assert_eq!(builder.instructions[2],Instruction::ADD(crate::emu::Register::R0,crate::emu::Register::R1));

        Ok(())
    }
}