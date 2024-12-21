use std::fmt::Display;
use super::{ops::*,storage::*};
fn mnemonic(op_code: OpCode) -> String {
    use Sign::*;
    match op_code {
        OpCode::Add(Signed) => "adds",
        OpCode::Add(Unsigned) => "addu",
        OpCode::Sub(Signed) => "subs",
        OpCode::Sub(Unsigned) => "subu",
        OpCode::Mul(Signed) => "muls",
        OpCode::Mul(Unsigned) => "mulu",
        OpCode::FloatAdd => "fadd",
        OpCode::FloatMul => "fmul",
        OpCode::FloatDiv => "fdiv",
        OpCode::FloatSub => "fsub",
        OpCode::Jump => "jmp",
        OpCode::JumpIf(flag) => match flag {
            Flag::Neq => "jne"
        },
        OpCode::Create => "create",
        OpCode::Load(_) => "load",
        OpCode::Store(_) => "store",
        OpCode::Copy(_, _, byte_size) => "copy",
        OpCode::BitAnd => "bitand",
        OpCode::BitOr => "bitor",
        OpCode::BitXor => "bitxor",
        OpCode::LogAnd => "and",
        OpCode::LogOr => "or",
        OpCode::LogXor => "xor",
        OpCode::Abs => "abs",
        OpCode::ToInt => "toint",
        OpCode::ToFloat => "tofloat",
        OpCode::ToIntRaw => "tointraw",
        OpCode::ToFloatRaw => "tofloatraw",
        //These seem to cause a problem...
        //OpCode::Trunc(Signed, byte_size) => format!("truncs{}",display_size(byte_size)).as_str(),
        //OpCode::Trunc(Unsigned, byte_size) => format!("truncu{}",display_size(byte_size)).as_str(),
        //OpCode::Extend(Signed, byte_size) => format!("extends{}",display_size(byte_size)).as_str(),
        //OpCode::Extend(Unsigned, byte_size) => format!("extendu{}",display_size(byte_size)).as_str(),
        //OpCode::ShiftLeft(Signed, byte_size) => format!("asl{}",display_size(byte_size)).as_str(),
        //OpCode::ShiftLeft(Unsigned, byte_size) => format!("shl{}",display_size(byte_size)).as_str(),
        //OpCode::ShiftRight(Signed, byte_size) => format!("asr{}",display_size(byte_size)).as_str(),
        //OpCode::ShiftRight(Unsigned, byte_size) => format!("shr{}",display_size(byte_size)).as_str(),
        OpCode::Move => "mov",
        _ => todo!()
    }.to_string()
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::Signed(value) => write!(f,"#{:+x}",value),
            LiteralValue::Unsigned(value) => write!(f,"#{:x}",value),
            LiteralValue::Float(value) => write!(f,"!{}",value),
            LiteralValue::Char(char) => write!(f,"\'{}\'",char),
        }
    }
}

impl Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Display for Output {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}