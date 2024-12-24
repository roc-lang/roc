use super::ops::Operation;
use super::storage::{
    Args, ByteSize, Constant, FloatRegister, Global, Input, Offset, Output, ProcRef, Register,
};
#[macro_use]
use bumpalo;
use bumpalo::collections;
use bumpalo::collections::CollectIn;

use roc_module::symbol;

#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Proc<'a> {
    ///The instructions themselves
    pub instructions: collections::Vec<'a, Operation>,
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
pub struct Section<'a> {
    ///The instructions themselves
    pub procedures: collections::Vec<'a, Proc<'a>>,
    ///A list of inputs, in order, taken by the function
    ///The reason it's type is `Output` and not `Input` is that we want the values to be written from certain registers, not read from them
    pub globals: collections::Vec<'a, Global>,
    ///A list of computed registers used by the operation
    pub annotations: SectionAnnotations,
    ///bump allocater 
    pub arena : &'a bumpalo::Bump
}

impl<'a> Section<'a> {
    pub fn new(arena: &'a bumpalo::Bump) -> Self {
        Self {
            procedures: bumpalo::vec![in arena;],
            globals: bumpalo::vec![in arena;],
            annotations: SectionAnnotations::default(),
            arena : arena
        }
    }
    ///Returns the given ref to the proccedure 0 indexed. Returns ownership to caller
    pub fn add_proc(mut self, proc: Proc<'a>) -> (Self,ProcRef) {
        self.procedures.push(proc);
        //FIXME make this not a raw cast
        let ret = ProcRef::Absolute((self.procedures.len() - 1) as u32);
        (self,ret)
    }

    ///Adds a list of procedures. Returns ownership to caller
    pub fn add_procs(mut self, procs: collections::Vec<'a, Proc>) -> (Self,collections::Vec<'a,ProcRef>) {
        //TODO find way to make this not clone
        self.procedures.extend(procs.clone());
        
        //FIXME make this not a raw cast
        let ret = (self.procedures.len()-procs.len()..self.procedures.len()).map(|x| ProcRef::Absolute(x as u32)).collect_in(self.arena);
        (self,ret)
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
    pub fn new(args: Args, arena: &'a bumpalo::Bump) -> Self {
        Self {
            instructions: bumpalo::vec![in arena;],
            inputs: args,
            annotations: Annotations::default(),
        }
    }
    pub fn add_ops(mut self,ops : collections::Vec<'a,Operation>) -> Self {
        self.instructions.extend(ops);
        self
    }
}
