use crate::llvm::build::{add_func, C_CALL_CONV};
use crate::llvm::convert::ptr_int;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::values::BasicValue;
use inkwell::AddressSpace;

/// Define functions for roc_alloc, roc_realloc, and roc_dealloc
/// which use libc implementations (malloc, realloc, and free)
pub fn add_default_roc_externs<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    ptr_bytes: u32,
) {
    let usize_type = ptr_int(ctx, ptr_bytes);
    let i8_ptr_type = ctx.i8_type().ptr_type(AddressSpace::Generic);

    // roc_alloc
    {
        // The type of this function (but not the implementation) should have
        // already been defined by the builtins, which rely on it.
        let fn_val = module.get_function("roc_alloc").unwrap();
        let mut params = fn_val.get_param_iter();
        let size_arg = params.next().unwrap();
        let _alignment_arg = params.next().unwrap();

        debug_assert!(params.next().is_none());

        // Add a basic block for the entry point
        let entry = ctx.append_basic_block(fn_val, "entry");

        builder.position_at_end(entry);

        // Call libc malloc()
        let retval = builder
            .build_array_malloc(ctx.i8_type(), size_arg.into_int_value(), "call_malloc")
            .unwrap();

        builder.build_return(Some(&retval));

        if cfg!(debug_assertions) {
            crate::llvm::build::verify_fn(fn_val);
        }
    }

    // roc_realloc
    {
        let libc_realloc_val = {
            let fn_val = add_func(
                module,
                "realloc",
                i8_ptr_type.fn_type(
                    &[
                        // ptr: *void
                        i8_ptr_type.into(),
                        // size: usize
                        usize_type.into(),
                    ],
                    false,
                ),
                Linkage::External,
                C_CALL_CONV,
            );

            let mut params = fn_val.get_param_iter();
            let ptr_arg = params.next().unwrap();
            let size_arg = params.next().unwrap();

            debug_assert!(params.next().is_none());

            ptr_arg.set_name("ptr");
            size_arg.set_name("size");

            if cfg!(debug_assertions) {
                crate::llvm::build::verify_fn(fn_val);
            }

            fn_val
        };

        // The type of this function (but not the implementation) should have
        // already been defined by the builtins, which rely on it.
        let fn_val = module.get_function("roc_realloc").unwrap();
        let mut params = fn_val.get_param_iter();
        let ptr_arg = params.next().unwrap();
        let new_size_arg = params.next().unwrap();
        let _old_size_arg = params.next().unwrap();
        let _alignment_arg = params.next().unwrap();

        debug_assert!(params.next().is_none());

        // Add a basic block for the entry point
        let entry = ctx.append_basic_block(fn_val, "entry");

        builder.position_at_end(entry);

        // Call libc realloc()
        let call = builder.build_call(
            libc_realloc_val,
            &[ptr_arg, new_size_arg],
            "call_libc_realloc",
        );

        call.set_call_convention(C_CALL_CONV);

        let retval = call.try_as_basic_value().left().unwrap();

        builder.build_return(Some(&retval));

        if cfg!(debug_assertions) {
            crate::llvm::build::verify_fn(fn_val);
        }
    }

    // roc_dealloc
    {
        // The type of this function (but not the implementation) should have
        // already been defined by the builtins, which rely on it.
        let fn_val = module.get_function("roc_dealloc").unwrap();
        let mut params = fn_val.get_param_iter();
        let ptr_arg = params.next().unwrap();
        let _alignment_arg = params.next().unwrap();

        debug_assert!(params.next().is_none());

        // Add a basic block for the entry point
        let entry = ctx.append_basic_block(fn_val, "entry");

        builder.position_at_end(entry);

        // Call libc free()
        builder.build_free(ptr_arg.into_pointer_value());

        builder.build_return(None);

        if cfg!(debug_assertions) {
            crate::llvm::build::verify_fn(fn_val);
        }
    }
}
