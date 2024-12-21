//! Roc's abstract representation for the dev backend, optimzed for convertibility and optimizability to machine code

mod ops;
///All possible values to be used in ROAR
mod storage;
///Implements `std::fmt::Display` for various types 
mod display;
///Where procedures get implemented 
mod proc;

//Just some temporary quick and dirty tests, will be removed
#[cfg(test)]
mod test {
    use std::default;

    use crate::{proc::*,ops::*,storage::*};
    #[test]
    fn test_roar_fmt() {
        let value = Proc {
            instructions : vec![
                Operation {
                    output : Output::Register(Register(78)),
                    opcode : OpCode::Add(Sign::Unsigned),
                    inputs : vec![Input::Register(Register(73)),Input::Register(Register(32))]
                },
                Operation {
                    output : Output::Null,
                    opcode : OpCode::Sub(Sign::Signed),
                    inputs : vec![Input::Register(Register(73)),Input::Register(Register(32))]
                }
            ],
            inputs : vec![Output::Register(Register(32))],
            annotations : Annotations::default(),
        };
        println!("{}",value)
    }
}