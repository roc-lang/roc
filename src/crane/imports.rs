use cranelift::prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext};
use cranelift_codegen::ir::entities::FuncRef;
use cranelift_codegen::Context;
use cranelift_module::{Backend, Linkage, Module};

pub fn declare_malloc<B: Backend>(module: &mut Module<B>, ctx: &mut Context) -> FuncRef {
    let mut func_ctx = FunctionBuilderContext::new();
    let mut builder: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
    let block = builder.create_block();

    builder.switch_to_block(block);

    let ptr_size_type = module.target_config().pointer_type();
    let mut sig = module.make_signature();

    sig.params.push(AbiParam::new(ptr_size_type));
    sig.returns.push(AbiParam::new(ptr_size_type));

    let malloc = module
        .declare_function("malloc", Linkage::Import, &sig)
        .expect("declare malloc");

    module.declare_func_in_func(malloc, &mut builder.func)
}
