//! Code deserializer
//!
//! Used to load serialized bytecode into VM to use again
//! Taken in part from stack-vm

use std::io::Read;
use std::fmt;
use super::Code;

/// Convert from bytecode to a type
///
/// This trait represents the ability to load your Operands from bytecode.

pub trait FromByteCode {
    fn from_byte_code(_: &mut dyn Read) -> Self;
}

impl<T: FromByteCode + fmt::Debug> FromByteCode for Code<T> {
    fn from_byte_code(mut buf: &mut dyn Read) -> Code<T> {
        let symbols: Vec<(usize, String)> = vec![];
        let code: Vec<usize> = vec![];
        let data: Vec<T> = vec![];
        let labels: Vec<(usize, String)> = vec![];
        Code {
            symbols,
            code,
            data,
            labels,

        }
    }

}
