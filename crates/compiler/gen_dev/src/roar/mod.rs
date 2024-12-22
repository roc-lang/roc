//! Roc's abstract representation for the dev backend, optimzed for convertibility and optimizability to machine code
#![warn(missing-docs)]
///Convert mono IR into ROAR
mod convert;
///Implements `std::fmt::Display` for various types
mod display;
mod ops;
///Where procedures get implemented
mod proc;
///All possible values to be used in ROAR
mod storage;

///Just some temporary quick and dirty tests, will be removed
#[cfg(test)]
mod test {
    use std::default;

    use super::{ops::*, proc::*, storage::*};
    #[test]
    fn test_roar_fmt() {
        let value = Proc {
            instructions: vec![
                Operation {
                    output: Output::Register(Register(78)),
                    opcode: OpCode::Add(Sign::Unsigned),
                    inputs: (Input::Register(Register(73)), Input::Empty),
                },
                Operation {
                    output: Output::Null,
                    opcode: OpCode::Sub(Sign::Signed),
                    inputs: (Input::Register(Register(73)), Input::Register(Register(32))),
                },
            ],
            inputs: vec![Output::Register(Register(32))],
            annotations: Annotations::default(),
        };
        println!("{}", value)
    }
}
