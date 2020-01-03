use crate::subs::Subs;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;

pub struct Env<'ctx, 'env> {
    pub context: &'ctx Context,
    pub builder: &'env Builder<'ctx>,
    pub module: &'env Module<'ctx>,
    pub subs: Subs,
}
