use super::ops::Operation;
use super::storage::{Args,ByteSize, Constant, FloatRegister, Input, Offset, Output, Register, Global, ProcRef};
use roc_module::symbol;
#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Proc {
    ///The instructions themselves
    pub instructions: Vec<Operation>,
    ///A list of inputs, in order, taken by the function
    pub inputs: Args,
    ///A list of computed registers used by the operation
    pub annotations: Annotations,
}

///Some annotations on the internal level that may or may not be present
#[derive(Clone, Debug)]
pub struct Annotations {
    ///All the registers it uses
    uses: Option<Vec<Output>>,
    ///It's name in the AST
    symbol: Option<symbol::Symbol>,
}
impl Default for Annotations {
    fn default() -> Self {
        Self {
            uses: None::<Vec<Output>>,
            symbol: None::<symbol::Symbol>,
        }
    }
}

#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Section {
    ///The instructions themselves
    pub procedures: Vec<Proc>,
    ///A list of inputs, in order, taken by the function
    ///The reason it's type is `Output` and not `Input` is that we want the values to be written from certain registers, not read from them
    pub globals: Vec<Global>,
    ///A list of computed registers used by the operation
    pub annotations: SectionAnnotations,
}
impl Section {
    pub fn new() -> Self {
        Self {
            procedures: vec![],
            globals: vec![],
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

impl Proc {
    pub fn new(args: Args) -> Self {
        Self {
            instructions: vec![],
            inputs: args,
            annotations: Annotations::default(),
        }
    }

}
