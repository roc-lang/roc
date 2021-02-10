use crate::llvm::build::call_bitcode_fn;
use crate::llvm::build::Env;
use crate::llvm::build_str;
use crate::llvm::convert::basic_type_from_layout;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue, StructValue};
use roc_builtins::bitcode;
use roc_mono::layout::{Builtin, Layout};

pub fn hash<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    seed: IntValue<'ctx>,
    val: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    // NOTE: C and Zig use this value for their initial HashMap seed: 0xc70f6907

    match layout {
        Layout::Builtin(builtin) => hash_builtin(env, seed, val, layout, builtin).into(),

        Layout::Struct(fields) => hash_struct(env, seed, val.into_struct_value(), fields).into(),

        _ => {
            todo!("Implement hash for other layouts")
        }
    }
}

fn hash_builtin<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    seed: IntValue<'ctx>,
    val: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
    builtin: &Builtin<'a>,
) -> IntValue<'ctx> {
    let ptr_bytes = env.ptr_bytes;

    match builtin {
        Builtin::Int128
        | Builtin::Int64
        | Builtin::Int32
        | Builtin::Int16
        | Builtin::Int8
        | Builtin::Int1
        | Builtin::Float64
        | Builtin::Float32
        | Builtin::Float128
        | Builtin::Float16
        | Builtin::Usize => {
            let hash_bytes = store_and_use_as_u8_ptr(env, val.into(), &layout);
            hash_bitcode_fn(env, seed, hash_bytes, layout.stack_size(ptr_bytes))
        }
        Builtin::Str => {
            // let zig deal with big vs small string
            call_bitcode_fn(
                env,
                &[seed.into(), build_str::str_to_i128(env, val).into()],
                &bitcode::DICT_HASH_STR,
            )
            .into_int_value()
        }
        Builtin::EmptyStr | Builtin::EmptyDict | Builtin::EmptyList | Builtin::EmptySet => {
            todo!("These are all the same, so we should just hardcode some hash value")
        }

        Builtin::Dict(_, _) => {
            todo!("Implement hash for Dict")
        }
        Builtin::Set(_) => {
            todo!("Implement Hash for Set")
        }
        Builtin::List(_, _) => {
            todo!("Implement Hash for List")
        }
    }
}

fn hash_struct<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    mut seed: IntValue<'ctx>,
    val: StructValue<'ctx>,
    field_layouts: &[Layout<'a>],
) -> IntValue<'ctx> {
    let ptr_bytes = env.ptr_bytes;

    let layout = Layout::Struct(field_layouts);

    if !layout.contains_refcounted() {
        // this is a struct of only basic types, so we can just hash its bits
        let hash_bytes = store_and_use_as_u8_ptr(env, val.into(), &layout);
        hash_bitcode_fn(env, seed, hash_bytes, layout.stack_size(ptr_bytes))
    } else {
        todo!()
    }
}

fn store_and_use_as_u8_ptr<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: BasicValueEnum<'ctx>,
    layout: &Layout<'a>,
) -> PointerValue<'ctx> {
    let basic_type = basic_type_from_layout(env.arena, env.context, &layout, env.ptr_bytes);
    let alloc = env.builder.build_alloca(basic_type, "store");
    env.builder.build_store(alloc, value);

    env.builder
        .build_bitcast(
            alloc,
            env.context
                .i8_type()
                .ptr_type(inkwell::AddressSpace::Generic),
            "as_u8_ptr",
        )
        .into_pointer_value()
}

fn hash_bitcode_fn<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    seed: IntValue<'ctx>,
    buffer: PointerValue<'ctx>,
    width: u32,
) -> IntValue<'ctx> {
    let num_bytes = env.context.i64_type().const_int(width as u64, false);

    call_bitcode_fn(
        env,
        &[seed.into(), buffer.into(), num_bytes.into()],
        &bitcode::DICT_HASH,
    )
    .into_int_value()
}
