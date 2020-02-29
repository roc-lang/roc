use cranelift::prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext};
use cranelift_codegen::ir::{ExternalName, Function, InstBuilder, Signature};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::Context;
use cranelift_module::{Backend, FuncId, Linkage, Module};

pub fn declare_malloc_header<B: Backend>(module: &mut Module<B>) -> (FuncId, Signature) {
    let ptr_size_type = module.target_config().pointer_type();
    let sig = Signature {
        params: vec![AbiParam::new(ptr_size_type)],
        returns: vec![AbiParam::new(ptr_size_type)],
        call_conv: CallConv::SystemV, // TODO is this the calling convention we actually want?
    };

    // Declare the wrapper around malloc
    let func_id = module
        .declare_function("roc_malloc", Linkage::Local, &sig)
        .unwrap();

    (func_id, sig)
}

pub fn define_malloc_body<B: Backend>(
    module: &mut Module<B>,
    ctx: &mut Context,
    sig: Signature,
    func_id: FuncId,
) {
    let ptr_size_type = module.target_config().pointer_type();

    ctx.func = Function::with_name_signature(ExternalName::user(0, func_id.as_u32()), sig);

    let mut func_ctx = FunctionBuilderContext::new();

    {
        let mut builder: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        let block = builder.create_block();

        builder.switch_to_block(block);
        builder.append_block_params_for_function_params(block);

        let mut malloc_sig = module.make_signature();

        malloc_sig.params.push(AbiParam::new(ptr_size_type));
        malloc_sig.returns.push(AbiParam::new(ptr_size_type));

        let callee = module
            .declare_function("malloc", Linkage::Import, &malloc_sig)
            .expect("declare malloc");
        let malloc = module.declare_func_in_func(callee, &mut builder.func);
        let size = builder.block_params(block)[0];
        let call = builder.ins().call(malloc, &[size]);
        let ptr = builder.inst_results(call)[0];

        builder.ins().return_(&[ptr]);

        // TODO re-enable this once Switch stops making unsealed blocks, e.g.
        // https://docs.rs/cranelift-frontend/0.59.0/src/cranelift_frontend/switch.rs.html#152
        // builder.seal_block(block);
    }

    module.define_function(func_id, ctx).unwrap();
}

#[test]
fn libcall_function() {
    use cranelift::prelude::*;
    use cranelift_codegen::ir::types::I16;
    use cranelift_codegen::ir::*;
    use cranelift_codegen::isa::CallConv;
    use cranelift_module::*;
    use cranelift_simplejit::*;

    let mut module: Module<SimpleJITBackend> =
        Module::new(SimpleJITBuilder::new(default_libcall_names()));

    // let sig = Signature {
    //     params: vec![],
    //     returns: vec![],
    //     call_conv: CallConv::SystemV,
    // };

    // let func_id = module
    //     .declare_function("function", Linkage::Local, &sig)
    //     .unwrap();

    let mut ctx = module.make_context();
    define_malloc(&mut module, &mut ctx);
    // ctx.func = Function::with_name_signature(ExternalName::user(0, func_id.as_u32()), sig);
    // let mut func_ctx = FunctionBuilderContext::new();
    // {
    //     let mut bcx: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
    //     let block = bcx.create_block();
    //     bcx.switch_to_block(block);

    //     let int = module.target_config().pointer_type();
    //     let zero = bcx.ins().iconst(I16, 0);
    //     let size = bcx.ins().iconst(int, 10);

    //     let mut signature = module.make_signature();
    //     signature.params.push(AbiParam::new(int));
    //     signature.returns.push(AbiParam::new(int));
    //     let callee = module
    //         .declare_function("malloc", Linkage::Import, &signature)
    //         .expect("declare malloc function");
    //     let local_callee = module.declare_func_in_func(callee, &mut bcx.func);
    //     let argument_exprs = vec![size];
    //     let call = bcx.ins().call(local_callee, &argument_exprs);
    //     let buffer = bcx.inst_results(call)[0];

    //     bcx.call_memset(module.target_config(), buffer, zero, size);

    //     bcx.ins().return_(&[]);
    // }

    // module.define_function(func_id, &mut ctx).unwrap();

    module.finalize_definitions();
}

pub fn define_malloc<B: Backend>(module: &mut Module<B>, ctx: &mut Context) -> FuncId {
    // TODO investigate whether we can remove this wrapper function somehow.
    // It may get inlined away, but it seems like it shouldn't be
    // necessary, and we should be able to return the FuncId of the imported malloc.
    let ptr_size_type = module.target_config().pointer_type();
    let sig = Signature {
        params: vec![AbiParam::new(ptr_size_type)],
        returns: vec![AbiParam::new(ptr_size_type)],
        call_conv: CallConv::SystemV, // TODO is this the calling convention we actually want?
    };

    // Declare the wrapper around malloc
    let func_id = module
        .declare_function("roc_malloc", Linkage::Local, &sig)
        .unwrap();

    let ptr_size_type = module.target_config().pointer_type();

    ctx.func = Function::with_name_signature(ExternalName::user(0, func_id.as_u32()), sig);

    let mut func_ctx = FunctionBuilderContext::new();

    {
        let mut builder: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        let block = builder.create_block();

        builder.switch_to_block(block);
        builder.append_block_params_for_function_params(block);

        let mut malloc_sig = module.make_signature();

        malloc_sig.params.push(AbiParam::new(ptr_size_type));
        malloc_sig.returns.push(AbiParam::new(ptr_size_type));

        let callee = module
            .declare_function("malloc", Linkage::Import, &malloc_sig)
            .expect("declare malloc");
        let malloc = module.declare_func_in_func(callee, &mut builder.func);
        let size = builder.block_params(block)[0];
        let call = builder.ins().call(malloc, &[size]);
        let ptr = builder.inst_results(call)[0];

        builder.ins().return_(&[ptr]);

        builder.seal_block(block);
        builder.finalize();
    }

    module.define_function(func_id, ctx).unwrap();
    module.clear_context(ctx);

    func_id
}
