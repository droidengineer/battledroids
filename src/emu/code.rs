//! Code
//! 
//! Represents a chunk of code ready to be executed by
//! the VM. `Code` can be created in one of two ways:
//! * From an instance of `Builder`
//! * From dumped bytecode

use std::{io::{Write,Read}, vec, fmt};

use rmp::{decode,encode};

use crate::types::{addr_t, from_byte_code::FromByteCode,to_byte_code::ToByteCode};

#[derive(PartialEq,Debug)]
pub struct Operand(addr_t); 
impl FromByteCode for Operand {
    fn from_byte_code(mut buf: &mut dyn Read) -> Self {
        let value = decode::read_u16(&mut buf).unwrap();
        Operand(value)
    }
}
impl ToByteCode for Operand {
    fn to_byte_code(&self, mut buf: &mut dyn Write) {
        encode::write_u16(&mut buf, self.0).unwrap();
    }
}

//#[derive(Debug)]
/// A structure containing runnable or dumpable code
/// from stack-vm
pub struct Code {
    pub symbols: Vec<(usize, String)>,
    pub code: Vec<usize>,
    pub data: Vec<Operand>,
    pub labels: Vec<(usize, String)>,
}

impl Code {
    pub fn new() -> Self {
        Code {
            symbols: vec![],
            code: vec![],
            data: vec![],
            labels: vec![],
        }
    }
    fn empty(&mut self) -> Code {
        Code {
            symbols: vec![],
            code: vec![],
            data: vec![],
            labels: vec![],
        }
    }    
    pub fn symbols(&self) -> &[(usize, String)] {
        self.symbols.as_slice()
    }
    pub fn code(&self) -> &[usize] {
        self.code.as_slice()
    }
    pub fn data(&self) -> &[Operand] {
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
        }
        write!(f, "{}", result)
    }
}
impl FromByteCode for Code {
    fn from_byte_code(mut buf: &mut dyn Read) -> Code {
        // uses a 4-element map
        let map_len = decode::read_map_len(&mut buf).unwrap();
        assert_eq!(map_len, 4);

        // -> code section
        let section = read_string(&mut buf);
        assert_eq!(section, "code");

        let code_len = decode::read_array_len(&mut buf).unwrap();
        let mut code: Vec<usize> = vec![];
        for _i in 0..code_len {
            code.push(decode::read_u16(&mut buf).unwrap() as usize);
        }
        // data section
        let section = read_string(&mut buf);
        assert_eq!(section, "data");
        let data_len = decode::read_array_len(&mut buf).unwrap();
        let mut data: Vec<Operand> = vec![];
        for _i in 0..data_len {
            data.push(FromByteCode::from_byte_code(&mut buf));
        }

        // symbols section
        let section = read_string(&mut buf);
        assert_eq!(section, "symbols");
        let symbol_len = decode::read_array_len(&mut buf).unwrap();
        let mut symbols: Vec<(usize, String)> = vec![];
        for _i in 0..symbol_len {
            let idx = decode::read_int(&mut buf).unwrap();
            let symbol = read_string(&mut buf);
            symbols.push((idx,symbol));
        }
        // labels section
        let section = read_string(&mut buf);
        assert_eq!(section, "labels");
        let label_len = decode::read_array_len(&mut buf).unwrap();
        let mut labels: Vec<(usize, String)> = vec![];
        for _i in 0..label_len {
            let idx = decode::read_int(&mut buf).unwrap();
            let label = read_string(&mut buf);
            labels.push((idx,label));
        }       
        Code {
            symbols,
            code,
            data,
            labels,
        }
    }
}


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
            encode::write_u16(&mut buf, *operation as u16).unwrap();
        }

        // data section
        encode::write_str(&mut buf, "data").unwrap();
        encode::write_array_len(&mut buf, self.data.len() as u32).unwrap();
        for operand in self.data() {
            encode::write_u16(&mut buf, operand.0).unwrap();
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