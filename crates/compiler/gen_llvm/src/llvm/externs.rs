use crate::llvm::bitcode::call_void_bitcode_fn;
use crate::llvm::build::{add_func, get_panic_msg_ptr, get_panic_tag_ptr, C_CALL_CONV};
use crate::llvm::build::{CCReturn, Env, FunctionSpec};
use inkwell::module::Linkage;
use inkwell::types::BasicType;
use inkwell::values::BasicValue;
use inkwell::AddressSpace;
use roc_builtins::bitcode;

use super::build::get_sjlj_buffer;
use super::intrinsics::LLVM_LONGJMP;

/// Define functions for roc_alloc, roc_realloc, and roc_dealloc
/// which use libc implementations (malloc, realloc, and free)
pub fn add_default_roc_externs(env: &Env<'_, '_, '_>) {
    let ctx = env.context;
    let module = env.module;
    let builder = env.builder;

    let usize_type = env.ptr_int();
    let i8_ptr_type = ctx.i8_type().ptr_type(AddressSpace::Generic);

    match env.mode {
        super::build::LlvmBackendMode::CliTest => {
            // expose this function
            if let Some(fn_val) = module.get_function("set_shared_buffer") {
                fn_val.set_linkage(Linkage::External);
            }
        }
        _ => {
            // remove this function from the module
            if let Some(fn_val) = module.get_function("set_shared_buffer") {
                unsafe { fn_val.delete() };
            }
        }
    }

    if !env.mode.has_host() {
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
                let fn_spec = FunctionSpec::cconv(
                    env,
                    CCReturn::Return,
                    Some(i8_ptr_type.as_basic_type_enum()),
                    &[
                        // ptr: *void
                        i8_ptr_type.into(),
                        // size: usize
                        usize_type.into(),
                    ],
                );
                let fn_val = add_func(env.context, module, "realloc", fn_spec, Linkage::External);

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
                &[ptr_arg.into(), new_size_arg.into()],
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

        unreachable_function(env, "roc_getppid");
        unreachable_function(env, "roc_mmap");
        unreachable_function(env, "roc_send_signal");
        unreachable_function(env, "roc_shm_open");

        add_sjlj_roc_panic(env)
    }
}

fn unreachable_function(env: &Env, name: &str) {
    // The type of this function (but not the implementation) should have
    // already been defined by the builtins, which rely on it.
    let fn_val = env.module.get_function(name).unwrap();

    // Add a basic block for the entry point
    let entry = env.context.append_basic_block(fn_val, "entry");

    env.builder.position_at_end(entry);

    env.builder.build_unreachable();

    if cfg!(debug_assertions) {
        crate::llvm::build::verify_fn(fn_val);
    }
}

pub fn add_sjlj_roc_panic(env: &Env<'_, '_, '_>) {
    let ctx = env.context;
    let module = env.module;
    let builder = env.builder;

    // roc_panic
    {
        // The type of this function (but not the implementation) should have
        // already been defined by the builtins, which rely on it.
        let fn_val = module.get_function("roc_panic").unwrap();
        let mut params = fn_val.get_param_iter();
        let roc_str_arg = params.next().unwrap();

        let tag_id_arg = params.next().unwrap();

        debug_assert!(params.next().is_none());

        let subprogram = env.new_subprogram("roc_panic");
        fn_val.set_subprogram(subprogram);

        env.dibuilder.finalize();

        // Add a basic block for the entry point
        let entry = ctx.append_basic_block(fn_val, "entry");

        builder.position_at_end(entry);

        // write our error message to the RocStr pointer
        {
            let loaded_roc_str = match env.target_info.ptr_width() {
                roc_target::PtrWidth::Bytes4 => roc_str_arg,
                // On 64-bit we pass RocStrs by reference internally
                roc_target::PtrWidth::Bytes8 => {
                    builder.build_load(roc_str_arg.into_pointer_value(), "load_roc_str")
                }
            };

            env.builder
                .build_store(get_panic_msg_ptr(env), loaded_roc_str);
        }

        // write the panic tag.
        // increment by 1, since the tag we'll get from the Roc program is 0-based,
        // but we use 0 for marking a successful call.
        {
            let cast_tag_id = builder.build_int_z_extend(
                tag_id_arg.into_int_value(),
                env.context.i64_type(),
                "zext_panic_tag",
            );

            let inc_tag_id = builder.build_int_add(
                cast_tag_id,
                env.context.i64_type().const_int(1, false),
                "inc_panic_tag",
            );

            env.builder.build_store(get_panic_tag_ptr(env), inc_tag_id);
        }

        build_longjmp_call(env);

        builder.build_unreachable();

        if cfg!(debug_assertions) {
            crate::llvm::build::verify_fn(fn_val);
        }
    }
}

pub fn build_longjmp_call(env: &Env) {
    let jmp_buf = get_sjlj_buffer(env);
    if cfg!(target_arch = "aarch64") {
        // Call the Zig-linked longjmp: `void longjmp(i32*, i32)`
        let tag = env.context.i32_type().const_int(1, false);
        let _call =
            call_void_bitcode_fn(env, &[jmp_buf.into(), tag.into()], bitcode::UTILS_LONGJMP);
    } else {
        // Call the LLVM-intrinsic longjmp: `void @llvm.eh.sjlj.longjmp(i8* %setjmp_buf)`
        let jmp_buf_i8p = env.builder.build_bitcast(
            jmp_buf,
            env.context.i8_type().ptr_type(AddressSpace::Generic),
            "jmp_buf i8*",
        );
        let _call = env.build_intrinsic_call(LLVM_LONGJMP, &[jmp_buf_i8p]);
    }
}
