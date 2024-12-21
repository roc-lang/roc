

use crate::storage::{Constant,ByteSize,Offset,Input,Output};

#[derive(Clone,Debug)]
pub(crate) enum Sign {
    Signed, 
    Unsigned 
}
#[derive(Clone,Debug)]
pub(crate) enum Flag {
    Neq
}
type Flags = Flag;
#[derive(Clone,Debug)]
///All operations used, arithemetic operations take a signedness flag to increase readability
pub(crate) enum OpCode {
    Add(Sign),
    Sub(Sign),
    Mul(Sign),
    FloatAdd,
    FloatMul,
    FloatDiv,
    FloatSub,
    Jump,
    JumpIf(Flags),
    ///Create a structure reference  
    Create,
    Load(Offset),
    Store(Offset),
    ///Copy `$3` number bytes from offset `$1` to offset `$2`
    Copy(Offset,Offset,ByteSize),
    BitAnd,
    BitOr,
    BitXor,
    LogAnd,
    LogOr,
    LogXor,
    ///Set the highest bit to zero (even if it is unsigned)
    Abs,
    ///Truncate a register, if signed, copy sign bit to new highest point
    Trunc(Sign,ByteSize),
    ///Extend a register, if signed, copy sign bit to new highest point, then zero out the original point
    Extend(Sign,ByteSize),
    ///Convert a floating point register to a regular register
    ToInt,
    ///Convert a regular register to a floating point register
    ToFloat,
    ///Copy bits from a float register to a regular register, without conversion
    ToIntRaw,
    ///Copy bits from a regular register to a floating point register, without conversion 
    ToFloatRaw,
    ///Shift left instruction, whether arithmetic or logical depends on `Sign` flag
    ShiftLeft(Sign,ByteSize),
    ///Shift right instruction, whether arithmetic or logical depends on `Sign` flag
    ShiftRight(Sign,ByteSize),
    ///Basic move instruction 
    Move
}
#[derive(Clone,Debug)]
#[non_exhaustive]
///The actual type of operations in ROAR
pub struct Operation {
    output : Output,
    opcode : OpCode,
    input : Vec<Input>
}


