//! Code
//! 
//! Represents a chunk of code ready to be executed by
//! the VM. `Code` can be created in one of two ways:
//! * From an instance of `Builder`
//! * From dumped bytecode

use std::{io::{Write,Read}, vec, fmt};

use rmp::{decode,encode};

use crate::types::{from_byte_code::FromByteCode,to_byte_code::ToByteCode, code_t, addr_t, table::Table};

use super::{INSTRUCTIONS_MAX, isa::Instruction, builder::Builder, SRAM_MAX};

#[derive(PartialEq,Debug,Clone)]
pub struct CodeByte(code_t); 

impl FromByteCode for CodeByte {
    fn from_byte_code(mut buf: &mut dyn Read) -> Self {
        let value = decode::read_u16(&mut buf).unwrap();
        CodeByte(value)
    }
}
impl ToByteCode for CodeByte {
    fn to_byte_code(&self, mut buf: &mut dyn Write) {
        encode::write_u16(&mut buf, self.0).unwrap();
    }
}

//#[derive(Debug)]
/// A structure containing runnable or dumpable code
/// from stack-vm
pub struct Code {
    pub symbols: Vec<(usize, String)>,
    pub code: Vec<Instruction>,
    pub data: Vec<CodeByte>,
    pub labels: Vec<(usize, String)>,
}

impl Code {
    pub fn new() -> Self {
        Code {
            symbols: vec![],
            code: vec![Instruction::HALT; INSTRUCTIONS_MAX],
            data: vec![CodeByte(0); SRAM_MAX],
            labels: vec![],
        }
    }

    pub fn symbols(&self) -> &[(usize, String)] {
        self.symbols.as_slice()
    }
    pub fn code(&self) -> &[Instruction] {
        self.code.as_slice()
    }
    pub fn data(&self) -> &[CodeByte] {
        self.data.as_slice()
    }
    pub fn labels(&self) -> &[(usize, String)] {
        self.labels.as_slice()
    }
    pub fn get_label_ip(&self, name: &str) -> Option<usize> {
        for label in self.labels.as_slice() {
            if label.1 == name {
                return Some(label.0);
            }
        }
        None
    }
}

impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut result = String::new();
        // Write out constant data into the header.
        for i in 0..self.data.len() {
            result.push_str(&format!("@{} = {:?}\n", i, self.data[i]));
        }

        // Loop through the code and print out useful stuff.
        let mut ip = 0;
        let len = self.code.len();
        loop {
            // If this IP has a label, then print it out.
            for label in self.labels() {
                if ip == label.0 {
                    result.push_str(&format!("\n.{}:\n", label.1));
                    break;
                }
            }

            if ip == len {
                break;
            }
            let opcode = self.code[ip];
            ip += 1;

            // print this instruction's name
            for symbol in self.symbols() {
                todo!()
                // if opcode. == symbol.0 {
                //     result.push_str(&format!("\t{}", symbol.1));
                //     break;
                // }
            }

            result.push('\n');
            
        }
        write!(f, "{}", result)
    }
}

impl From<Builder> for Code {
    fn from(builder: Builder) -> Code {
        let mut data:Vec<CodeByte> = vec![];
        for d in builder.data {
            data.push(CodeByte(d));
        }
        let mut labels: Vec<(usize, String)> = vec![];
        for key in builder.labels.keys() {
            let idx = builder.labels.get(&key).unwrap();
            labels.push((*idx, key.clone()));
        }
        labels.sort_by(|lhs, rhs| lhs.0.cmp(&rhs.0));

        Code {
            code: builder.instructions,
            data,
            symbols: vec![],
            labels,
        }
    }
}

// impl FromByteCode for Code {
//     fn from_byte_code(mut buf: &mut dyn Read) -> Code {
//         trace!("from_byte_code()");
//         // uses a 4-element map
//         let map_len = decode::read_map_len(&mut buf).unwrap();
//         assert_eq!(map_len, 4);

//         // -> code section
//         let section = read_string(&mut buf);
//         assert_eq!(section, "code");

//         let code_len = decode::read_array_len(&mut buf).unwrap();
//         let mut code: Vec<Instruction> = vec![];
//         for _i in 0..code_len {
//             todo!()
//             //code.push(decode::read_u16(&mut buf).unwrap() as usize);
//         }
//         // data section
//         let section = read_string(&mut buf);
//         assert_eq!(section, "data");
//         let data_len = decode::read_array_len(&mut buf).unwrap();
//         let mut data: Vec<Operand> = vec![];
//         for _i in 0..data_len {
//             data.push(FromByteCode::from_byte_code(&mut buf));
//         }

//         // symbols section
//         let section = read_string(&mut buf);
//         assert_eq!(section, "symbols");
//         let symbol_len = decode::read_array_len(&mut buf).unwrap();
//         let mut symbols: Vec<(usize, String)> = vec![];
//         for _i in 0..symbol_len {
//             let idx = decode::read_int(&mut buf).unwrap();
//             let symbol = read_string(&mut buf);
//             symbols.push((idx,symbol));
//         }
//         // labels section
//         let section = read_string(&mut buf);
//         assert_eq!(section, "labels");
//         let label_len = decode::read_array_len(&mut buf).unwrap();
//         let mut labels: Vec<(usize, String)> = vec![];
//         for _i in 0..label_len {
//             let idx = decode::read_int(&mut buf).unwrap();
//             let label = read_string(&mut buf);
//             labels.push((idx,label));
//         }
//         let mut ca: [Instruction; INSTRUCTIONS_MAX] = [Instruction::NOP; INSTRUCTIONS_MAX];
//         code.resize(INSTRUCTIONS_MAX,Instruction::NOP);
//         ca.copy_from_slice(&code);
//         Code {
//             symbols,
//             code: ca,
//             data,
//             labels,
//         }
//     }
// }


impl ToByteCode for Code {
    /// Create bytecode for this `Code`.
    ///
    /// Encodes into a Map of the following format:
    /// ```json
    /// {
    ///     "code" => [ 0, 1, 0, 0, 1, 1, 1, 0 ],
    ///     "data" => [ 123, 456 ],
    ///     "symbols" => [ 0, "push", 1, "add" ],
    ///     "labels" => [ 0, "main" ]
    /// }
    ///    
    fn to_byte_code(&self, mut buf: &mut dyn Write) {
        trace!("to_byte_code()");
        // Write a 4-element map
        encode::write_map_len(&mut buf, 4).unwrap();

        // code section
        encode::write_str(&mut buf, "code").unwrap();
        encode::write_array_len(&mut buf, self.code.len() as u32).unwrap();
        for operation in self.code() {
            operation.to_byte_code(buf);
            //encode::write_u16(&mut buf, u16::from(*operation)).unwrap();
        }
        // data section
        encode::write_str(&mut buf, "data").unwrap();
        encode::write_array_len(&mut buf, self.data.len() as u32).unwrap();
        for codebyte in self.data() {
            codebyte.to_byte_code(&mut buf);
            //encode::write_u16(&mut buf, operand.0).unwrap();
        }
        // symbols section
        encode::write_str(&mut buf, "symbols").unwrap();
        encode::write_array_len(&mut buf, self.symbols.len() as u32).unwrap();
        for symbol in self.symbols() {
            encode::write_u16(&mut buf, symbol.0 as u16).unwrap();
            encode::write_str(&mut buf, &symbol.1).unwrap();
        }
        // labels section
        encode::write_str(&mut buf, "labels").unwrap();
        encode::write_array_len(&mut buf, self.labels.len() as u32).unwrap();
        for label in self.labels() {
            encode::write_u16(&mut buf, label.0 as u16).unwrap();
            encode::write_str(&mut buf, &label.1).unwrap();
        }
    }
}

fn read_string(mut buf: &mut dyn Read) -> String {
    let len = decode::read_str_len(&mut buf).unwrap();
    let mut strbuf: Vec<u8> = vec![0u8; len as usize];
    buf.read_exact(&mut strbuf).unwrap();
    String::from_utf8(strbuf).unwrap()
}