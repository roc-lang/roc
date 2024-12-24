use roc_module::symbol::Symbol;
use roc_mono::layout;
use std::cell::Cell;
use std::fmt::{Debug, Display};

///The type used to identify registers
pub(crate) type Id = u32;

#[derive(Clone, Copy, Debug)]
///A number of bytes an operation takes
pub struct ByteSize(pub u32);


///Equivalent to byte size, merely used as to specify the two
pub use ByteSize as Offset;
pub fn add_offsets(offset_a : Offset, offset_b : Offset) -> Offset {
    if let (Offset(i_a),Offset(i_b)) = (offset_a,offset_b) {
        return Offset(i_a + i_b);
    } else {
        todo!()
    }
}

pub fn mul_offsets(offset_a : Offset, mult : u32) -> Offset {
    if let Offset(i_a) = offset_a {
        return Offset(i_a * mult);
    } else {
        todo!()
    }
}
///The size of a single word
pub(crate) const WORD_SIZE: ByteSize = ByteSize(8);
///The align of a single word
pub(crate) const WORD_ALIGN: Offset = Offset(8);
///A basic register refrence
#[derive(Clone, Debug, Hash)]
pub(crate) struct Register(pub Id);

#[derive(Clone, Debug, Hash)]
///A basic float register refrence
pub(crate) struct FloatRegister(pub Id);

#[derive(Clone, Debug, Hash)]
///A given global
pub(crate) struct Global(Symbol);

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
///The type of all literal values that fit inside a word
pub enum LiteralValue {
    Signed(i64),
    Unsigned(u64),
    //Float(f64),
    //Add more in future?
}

///The type of literal data, for instance strings
pub(crate) type LiteralData = Vec<u8>;

#[derive(Clone, Debug, Hash)]
//All possible values to be read and written to
pub enum Output {
    Register(Register),
    FloatRegister(FloatRegister),
    Null,
}

#[derive(Clone, Debug)]
pub enum Input {
    Register(Register),
    FloatRegister(FloatRegister),
    Global(Global),
    Value(LiteralValue),
    Data(LiteralData),
    ByteSize(ByteSize),
    ///If a function takes less than three args, use these for remaining
    Null,
}

impl std::convert::Into<Input> for Output {
    ///Convert an output into an input
    fn into(self) -> Input {
        match self {
            Output::Register(register) => Input::Register(register),
            Output::FloatRegister(float_register) => Input::FloatRegister(float_register),
            Output::Null => Input::Null,
        }
    }
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

pub type Arg = Output;

pub type Args = Vec<Arg>;

pub fn display_size(ByteSize(byte_size): ByteSize) -> String {
    (byte_size * 8).to_string()
}

///Gets simple names to new registers, `%0`, then `%1` etc
pub(super) struct RegisterAllocater {
    highest: Cell<u32>,
    highest_float: Cell<u32>,
}

impl RegisterAllocater {
    pub fn new() -> Self {
        Self {
            highest: Cell::new(0),
            highest_float: Cell::new(0),
        }
    }

    pub fn new_register(&self) -> Register {
        let reg = Register(self.highest.get());
        self.highest.set(self.highest.get() + 1);
        return reg;
    }

    pub fn new_float_register(&self) -> FloatRegister {
        let reg = FloatRegister(self.highest_float.get());
        self.highest_float.set(self.highest_float.get() + 1);
        return reg;
    }

}
