use roc_module::symbol::Symbol;
use roc_mono::layout;
use std::fmt::{Debug, Display};
///The type used to identify registers
pub(crate) type Id = u32;
#[derive(Clone, Debug)]
///A number of bytes an operation takes
pub struct ByteSize(u32);
///Equivalent to byte size, merely used as to specify the two
pub(crate) type Offset = ByteSize;
///The size of a single word
pub(crate) const WORD_SIZE: ByteSize = ByteSize(8);
///A basic register refrence 
#[derive(Clone, Debug, Hash)]
pub(crate) struct Register(pub Id);
#[derive(Clone, Debug, Hash)]
///A basic float register refrence
pub(crate) struct FloatRegister(pub Id);
#[derive(Clone, Debug)]
///A given global
pub(crate) struct Global(Symbol);
#[derive(Clone, Debug)]
#[non_exhaustive]
///The type of all literal values that fit inside a word
pub enum LiteralValue {
    Signed(i64),
    Unsigned(u64),
    Float(f64),
    //Add more in future?
}

///The type of literal data, for instance strings
pub(crate) type LiteralData = Vec<u8>;
#[derive(Clone, Debug, Hash)]
///All valid outputs of an operation
pub enum Output {
    ///`_`, the null register, for things to be discarded (say, subtractions used as compares)
    Null,
    Register(Register),
    FloatRegister(FloatRegister),
}
#[derive(Clone, Debug)]
///All valid inputs of an operation
pub enum Input {
    Register(Register),
    FloatRegister(FloatRegister),
    Global(Global),
    Value(LiteralValue),
    Data(LiteralData),
    ///If a function takes less than three args, use these for remaining
    Empty,
}
#[derive(Clone, Debug)]
///A values that are valid as constants, for example as offsets
pub enum Constant {
    LiteralValue(LiteralValue),
    ByteSize(ByteSize),
    Offset(Offset),
}
#[derive(Clone, Debug)]
///Values that can be given to the jump instruction
pub enum Label {
    Absolute(u32),
}
#[derive(Clone, Debug)]
///Values that can be given to call instructions
pub enum ProcRef {
    ///It's absolute id in the segment
    Absolute(u32),
    ///It's name
    Name(Symbol),
}

pub type Args = Vec<Output>;

pub fn display_size(ByteSize(byte_size): ByteSize) -> String {
    (byte_size * 8).to_string()
}
///Gets simple names to new registers, `%0`, then `%1` etc
pub(super) struct RegisterAllocater {
    highest: u32,
    highest_float: u32,
}
impl RegisterAllocater {
    pub fn new() -> Self {
        Self {
            highest: 0,
            highest_float: 0,
        }
    }
    pub fn new_register(mut self) -> Register {
        let reg = Register(self.highest);
        self.highest += 1;
        return reg;
    }
    pub fn new_float_register(mut self) -> FloatRegister {
        let reg = FloatRegister(self.highest_float);
        self.highest_float += 1;
        return reg;
    }
}
