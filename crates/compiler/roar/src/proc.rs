use crate::ops::{Operation};
use crate::storage::{Register,FloatRegister,Constant,ByteSize,Offset,Input,Output};
use roc_module::symbol;
#[derive(Clone,Debug)]
#[non_exhaustive]
pub struct Proc {
    ///The instructions themselves
    instructions : Vec<Operation>,
    ///A list of inputs, in order, taken by the function
    ///The reason it's type is `Output` and not `Input` is that we want the values to be written from certain registers, not read from them
    inputs : Vec<Output>,
    ///A list of computed registers used by the operation
    annotations : Annotations
}
///Some annotations on the internal level that may or may not be present
#[derive(Clone,Debug)]
pub struct Annotations {
    ///All the registers it uses
    uses : Option<Vec<Output>>,
    ///It's name in the AST 
    symbol : Option<symbol::Symbol>,
}
impl Default for Annotations {
    fn default() -> Self {
        Self {
            uses : None::<Vec<Output>>,
            symbol : None::<symbol::Symbol>,
        }
    }
}