use std::fmt::Display;
use super::{ops::*,storage::*,proc::*};
fn mnemonic(op_code: &OpCode) -> String {
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
impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"{}",self.output);
        write!(f," <- ");
        write!(f,"{} ",mnemonic(&self.opcode));
        for input in &self.inputs {
            write!(f,"{} ",input);
        }
        write!(f,"")
    }
}

impl Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Input::Register(register) => write!(f,"{}",register),
            Input::FloatRegister(register) => write!(f,"{}",register),
            Input::Global(global) => write!(f,"{:?}",global),
            Input::Value(literal_value) => write!(f,"{}",literal_value),
            Input::Data(vec) => write!(f,"{:?}",vec),
        }
    }
}
impl Display for Output {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Output::Register(register) => write!(f,"{}",register),
            Output::FloatRegister(register) => write!(f,"{}",register),
            Output::Global(global) => write!(f,"{:?}",global),
            Output::Null => write!(f,"_")
        }
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"%{}",self.0)
    }
}
impl Display for FloatRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"#{}",self.0)
    }
}
impl Display for Proc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f,"[");
        for input in &self.inputs {
            write!(f,"{} ",input);
        }
        //FIXME do this in a way that doesn't use the ascii backspace...
        write!(f,"\x08] {{\n");
        for op in &self.instructions {
            writeln!(f,"\t{}",op);
        }
        writeln!(f,"}}")
    }
}