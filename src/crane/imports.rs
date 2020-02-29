use cranelift::prelude::{AbiParam, FunctionBuilder, FunctionBuilderContext};
use cranelift_codegen::ir::entities::FuncRef;
use cranelift_codegen::Context;
use cranelift_module::{Backend, Linkage, Module};

pub fn declare_malloc<B: Backend>(module: &mut Module<B>, ctx: &mut Context) -> FuncRef {
    let mut func_ctx = FunctionBuilderContext::new();

    let sig = Signature {
        params: vec![],
        returns: vec![],
        call_conv: CallConv::SystemV,
    };

    let roc_malloc = module
        .declare_function("roc_malloc", Linkage::Local, &sig)
        .expect("declare roc_malloc");

    // Declare the wrapper function which calls malloc
    {
        let mut builder: FunctionBuilder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);

        let block = builder.create_block();

        builder.switch_to_block(block);

        let ptr_size_type = module.target_config().pointer_type();
        let mut signature = module.make_signature();

        signature.params.push(AbiParam::new(ptr_size_type));
        signature.returns.push(AbiParam::new(ptr_size_type));

        {
            let mut malloc_sig = module.make_signature();

            // Malloc takes a usize and returns a pointer (also usize)
            malloc_sig.params.push(AbiParam::new(ptr_size_type));
            malloc_sig.returns.push(AbiParam::new(ptr_size_type));

            // Import malloc
            let malloc = module
                .declare_function("malloc", Linkage::Import, &malloc_sig)
                .expect("declare malloc");

            let malloc_fn = module.declare_func_in_func(malloc, &mut builder.func);
            // TODO read size out of args
            let call = builder.ins().call(local_callee, &[size]);
            let ptr_to_bytes = builder.inst_results(call)[0];
        }

        builder.ins().return_(&[]);
    }

    // TODO re-enable this once Switch stops making unsealed blocks, e.g.
    // https://docs.rs/cranelift-frontend/0.59.0/src/cranelift_frontend/switch.rs.html#152
    // builder.seal_block(block);
    module.define_function(roc_malloc, &mut ctx).unwrap();

    module.finalize_definitions();
}
