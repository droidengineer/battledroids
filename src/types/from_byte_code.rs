//! Code deserializer
//!
//! Used to load serialized bytecode into VM to use again
//! Taken in part from stack-vm

use std::io::Read;

/// Convert from bytecode to a type
///
/// This trait represents the ability to load your Operands from bytecode.

pub trait FromByteCode {
    fn from_byte_code(_: &mut dyn Read) -> Self;
}


#[cfg(test)]
mod test {
    use super::*;
    use rmp;

    #[derive(PartialEq, Debug)]
    struct Operand(i64);

    impl FromByteCode for Operand {
        fn from_byte_code(mut buf: &mut dyn Read) -> Operand {
            let i = rmp::decode::read_int(&mut buf).unwrap();
            Operand(i)
        }
    }

    #[test]
    fn from_byte_code() {
        let bytecode = [0xd];
        assert_eq!(Operand(13), Operand::from_byte_code(&mut &bytecode[..]));
    }
}
