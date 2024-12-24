use std::mem;

use super::storage::{Args,ByteSize, Constant, Offset, Input, Output,ProcRef,Label};

#[derive(Clone, Debug)]
pub(crate) enum Sign {
    Signed,
    Unsigned,
}
#[derive(Clone, Debug)]
pub(crate) enum Flag {
    Neq,
    Eq
}
type Flags = Flag;
#[derive(Clone, Debug)]
///All operations used, arithemetic operations take a signedness flag to increase readability
pub(crate) enum OpCode {
    Add(Sign),
    Sub(Sign),
    Mul(Sign),
    FloatAdd,
    FloatMul,
    FloatDiv,
    FloatSub,
    Jump(Label),
    JumpIf(Flags, Label),
    ///Create a structure reference  
    Create,
    Load(ByteSize),
    Store(ByteSize),
    ///Copy `$3` number bytes from offset `$1` to offset `$2`
    Copy(ByteSize),
    BitAnd,
    BitOr,
    BitXor,
    LogAnd,
    LogOr,
    LogXor,
    ///Set the highest bit to zero (even if it is unsigned)
    Abs,
    ///Truncate a register, if signed, copy sign bit to new highest point
    Trunc(Sign, ByteSize),
    ///Extend a register, if signed, copy sign bit to new highest point, then zero out the original point
    Extend(Sign, ByteSize),
    ///Convert a floating point register to a regular register
    ToInt,
    ///Convert a regular register to a floating point register
    ToFloat,
    ///Copy bits from a float register to a regular register, without conversion
    ToIntRaw,
    ///Copy bits from a regular register to a floating point register, without conversion
    ToFloatRaw,
    ///Shift left instruction, whether arithmetic or logical depends on `Sign` flag
    ShiftLeft(Sign),
    ///Shift right instruction, whether arithmetic or logical depends on `Sign` flag
    ShiftRight(Sign),
    ///Basic move instruction
    Move,
    ///Get the reference to a given procedure 
    MoveProc(ProcRef),
    ///Call a given procedure
    Call(ProcRef,Box<Args>),
    ///Apply a given procedure, with consant arguements but non-constant pointer 
    Apply(Box<Args>),
    Return
}
#[derive(Clone, Debug)]
#[non_exhaustive]
///The actual type of operations in ROAR
pub struct Operation {
    pub output: Output,
    pub opcode: OpCode,
    ///Every function (except `call`) has (in terms of non-constant arguements) an arity of 2, so only two inputs are allowed
    pub inputs: (Input,Input),
}
///The type of expressions. Not really expressions, just the non-output portion of a ROAR op
pub type Expr = (OpCode,Input,Input);

//TODO ? make a macro for instructions?