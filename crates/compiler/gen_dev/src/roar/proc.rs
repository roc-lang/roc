use super::ops::Operation;
use super::storage::{Args,ByteSize, Constant, FloatRegister, Offset, Value, Register, Global, ProcRef};
#[macro_use]
use bumpalo;
use bumpalo::collections;
use roc_module::symbol;
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Proc<'a> {
    ///The instructions themselves
    pub instructions: collections::Vec<'a,Operation>,
    ///A list of inputs, in order, taken by the function
    pub inputs: Args,
    ///A list of computed registers used by the operation
    pub annotations: Annotations,
}

///Some annotations on the internal level that may or may not be present
#[derive(Clone, Debug)]
pub struct Annotations {
    ///All the registers it uses
    uses: Option<Vec<Value>>,
    ///It's name in the AST
    symbol: Option<symbol::Symbol>,
}
impl Default for Annotations {
    fn default() -> Self {
        Self {
            uses: None::<Vec<Value>>,
            symbol: None::<symbol::Symbol>,
        }
    }
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Section<'a> {
    ///The instructions themselves
    pub procedures: collections::Vec<'a,Proc<'a>>,
    ///A list of inputs, in order, taken by the function
    ///The reason it's type is `Output` and not `Input` is that we want the values to be written from certain registers, not read from them
    pub globals: collections::Vec<'a,Global>,
    ///A list of computed registers used by the operation
    pub annotations: SectionAnnotations,
}
impl<'a> Section<'a> {
    pub fn new(arena : &'a bumpalo::Bump) -> Self {
        Self {
            procedures: bumpalo::vec![in arena;],
            globals: bumpalo::vec![in arena;],
            annotations: SectionAnnotations::default(),
        }
    }
}
///Some annotations on the internal level that may or may not be present
#[derive(Clone, Debug)]
pub struct SectionAnnotations {
    ///It's name in the AST
    symbol: Option<symbol::Symbol>,
    ///entry point for binaries
    entry_point: Option<ProcRef>,
}
impl Default for SectionAnnotations {
    fn default() -> Self {
        Self {
            symbol: None::<symbol::Symbol>,
            entry_point: Some(ProcRef::Absolute(0)),
        }
    }
}

impl<'a> Proc<'a> {
    pub fn new(args: Args,arena : &'a bumpalo::Bump) -> Self {
        Self {
            instructions: bumpalo::vec![in arena;],
            inputs: args,
            annotations: Annotations::default(),
        }
    }

}
