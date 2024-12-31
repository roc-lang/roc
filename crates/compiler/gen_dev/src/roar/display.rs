use super::{ops::*, proc::*, storage::*};
use std::fmt::Display;
fn mnemonic(op_code: &OpCode) -> &'static str {
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
        OpCode::Jump(_) => "jmp", //TODO
        OpCode::JumpIf(flag, _) => match flag {
            Flag::Neq => "jne",
            Flag::Eq => "jeq"
        },
        OpCode::Create => "create",
        OpCode::Load(_) => "load",
        OpCode::Store(_) => "store",
        OpCode::Copy(_) => "copy",
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
        _ => todo!(),
    }
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::Signed(value) => write!(f, "#{:+x}", value),
            LiteralValue::Unsigned(value) => write!(f, "#{:x}", value),
            LiteralValue::Float(value) => write!(f, "!{}", value),
        }
    }
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use super::storage::Input::*;
        write!(f, "{}", self.output);
        write!(f, " <- ");
        write!(f, "{} ", mnemonic(&self.opcode));
        match &self.inputs {
            (Null,Null) => write!(f,""),
            (Null,arg) | (arg,Null) => write!(f,"{}",arg),
            (arg_a,arg_b) => write!(f,"{} {}",arg_a,arg_b)
        };
        write!(f, "")
    }
}

impl Display for Input {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Input::Register(register) => write!(f, "{}", register),
            Input::FloatRegister(register) => write!(f, "{}", register),
            Input::Global(global) => write!(f, "{:?}", global),
            Input::Value(literal_value) => write!(f, "{}", literal_value),
            Input::Data(vec) => write!(f, "{:?}", vec),
            Input::Null => write!(f,"_"),
            Input::ByteSize(ByteSize(byte_size)) => write!(f, "#{}", 8*byte_size),
        }
    }
}
impl Display for Output {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Output::Register(register) => write!(f, "{}", register),
            Output::FloatRegister(register) => write!(f, "{}", register),
            Output::Null => write!(f,"_")
        }
    }
}


impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}
impl Display for FloatRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}
impl<'a> Display for Proc<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[");
        for input in &self.inputs {
            write!(f, "{} ", input);
        }
        //FIXME do this in a way that doesn't use the ascii backspace...
        write!(f, "\x08] {{\n");
        for op in &self.instructions {
            writeln!(f, "\t{}", op);
        }
        writeln!(f, "}}")
    }
}
