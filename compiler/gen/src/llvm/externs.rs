use crate::llvm::build::{set_name, C_CALL_CONV};
use crate::llvm::convert::ptr_int;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
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
    let i32_type = ctx.i32_type();
    let i8_ptr_type = ctx.i8_type().ptr_type(AddressSpace::Generic);

    // roc_alloc
    {
        let fn_val = module.add_function(
            "roc_alloc",
            i8_ptr_type.fn_type(
                &[
                    // alignment: u32
                    i32_type.into(),
                    // size: usize
                    usize_type.into(),
                ],
                false,
            ),
            Some(Linkage::External),
        );

        fn_val.set_call_conventions(C_CALL_CONV);

        let mut params = fn_val.get_param_iter();
        let alignment_arg = params.next().unwrap();
        let size_arg = params.next().unwrap();

        debug_assert!(params.next().is_none());

        set_name(alignment_arg, "alignment");
        set_name(size_arg, "size");

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
            let fn_val = module.add_function(
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
                Some(Linkage::External),
            );

            fn_val.set_call_conventions(C_CALL_CONV);

            let mut params = fn_val.get_param_iter();
            let ptr_arg = params.next().unwrap();
            let size_arg = params.next().unwrap();

            debug_assert!(params.next().is_none());

            set_name(ptr_arg, "ptr");
            set_name(size_arg, "size");

            if cfg!(debug_assertions) {
                crate::llvm::build::verify_fn(fn_val);
            }

            fn_val
        };

        let fn_val = module.add_function(
            "roc_realloc",
            i8_ptr_type.fn_type(
                &[
                    // alignment: u32
                    i32_type.into(),
                    // ptr: *void
                    i8_ptr_type.into(),
                    // old_size: usize
                    usize_type.into(),
                    // new_size: usize
                    usize_type.into(),
                ],
                false,
            ),
            Some(Linkage::External),
        );

        fn_val.set_call_conventions(C_CALL_CONV);

        let mut params = fn_val.get_param_iter();
        let alignment_arg = params.next().unwrap();
        let ptr_arg = params.next().unwrap();
        let old_size_arg = params.next().unwrap();
        let new_size_arg = params.next().unwrap();

        debug_assert!(params.next().is_none());

        set_name(alignment_arg, "alignment");
        set_name(ptr_arg, "ptr");
        set_name(old_size_arg, "old_size");
        set_name(new_size_arg, "new_size");

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
        let fn_val = module.add_function(
            "roc_dealloc",
            ctx.void_type().fn_type(
                &[
                    // alignment: u32
                    i32_type.into(),
                    // ptr: *void
                    i8_ptr_type.into(),
                ],
                false,
            ),
            Some(Linkage::External),
        );

        fn_val.set_call_conventions(C_CALL_CONV);

        let mut params = fn_val.get_param_iter();
        let alignment_arg = params.next().unwrap();
        let ptr_arg = params.next().unwrap();

        debug_assert!(params.next().is_none());

        set_name(alignment_arg, "alignment");
        set_name(ptr_arg, "ptr");

        // Call libc free()
        builder.build_free(ptr_arg.into_pointer_value());

        builder.build_return(None);

        if cfg!(debug_assertions) {
            crate::llvm::build::verify_fn(fn_val);
        }
    }
}
