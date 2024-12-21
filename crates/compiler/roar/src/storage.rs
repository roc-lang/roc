use roc_module::symbol::Symbol;
use std::fmt::{Debug,Display};
///The type used to identify registers
pub(crate) type Id = u32;
#[derive(Clone,Debug)]
///A number of bytes an operation takes
pub struct ByteSize(u32);
///Equivalent to byte size, merely used as to specify the two
pub(crate) type Offset = ByteSize;
pub(crate) const WORD_SIZE : ByteSize = ByteSize(8);
#[derive(Clone,Debug)]
pub(crate) struct Register(Id);
#[derive(Clone,Debug)]
pub(crate) struct FloatRegister(Id);
#[derive(Clone,Debug)]
pub(crate) struct Global(Symbol);
#[derive(Clone,Debug)]
///The type of all literal values that fit inside a word
pub enum LiteralValue {
    Signed(i64),
    Unsigned(u64),
    Float(f64),
    Char(char)
}

///The type of literal data, for instance strings 
pub(crate) type LiteralData = Vec<u8>;
#[derive(Clone,Debug)]
///All valid outputs of an operation
pub enum Output {
    ///`_`, the null register, for things to be discarded (say, subtractions used as compares)
    Null,
    Register(Register),
    FloatRegister(FloatRegister),
    Global(Global)
}
#[derive(Clone,Debug)]
///All valid inputs of an operation 
pub enum Input {
    Register(Register),
    FloatRegister(FloatRegister),
    Global(Global),
    Value(LiteralValue),
    Data(LiteralData)
}
#[derive(Clone,Debug)]
///A values that are valid as constants, for example as offsets
pub enum Constant {
    LiteralValue(LiteralValue),
    ByteSize(ByteSize),
    Offset(Offset)
}

pub fn display_size(ByteSize(byte_size) : ByteSize) -> String {
    (byte_size * 8).to_string()
}