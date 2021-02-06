use crate::llvm::build::{
    call_bitcode_fn, call_void_bitcode_fn, complex_bitcast, load_symbol, load_symbol_and_layout,
    Env, Scope,
};
use crate::llvm::convert::collection;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, IntValue, StructValue};
use inkwell::AddressSpace;
use roc_builtins::bitcode;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout};

pub fn dict_len<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    dict_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let ctx = env.context;

    let (_, dict_layout) = load_symbol_and_layout(scope, &dict_symbol);

    match dict_layout {
        Layout::Builtin(Builtin::Dict(_, _)) => {
            let dict_as_int = dict_symbol_to_i128(env, scope, dict_symbol);

            call_bitcode_fn(env, &[dict_as_int.into()], &bitcode::DICT_LEN)
        }
        Layout::Builtin(Builtin::EmptyDict) => ctx.i64_type().const_zero().into(),
        _ => unreachable!("Invalid layout given to Dict.len : {:?}", dict_layout),
    }
}

pub fn dict_empty<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _scope: &Scope<'a, 'ctx>,
) -> BasicValueEnum<'ctx> {
    // get the RocDict type defined by zig
    let roc_dict_type = env.module.get_struct_type("dict.RocDict").unwrap();

    // we must give a pointer for the bitcode function to write the result into
    let result_alloc = env.builder.build_alloca(roc_dict_type, "dict_empty");

    call_void_bitcode_fn(env, &[result_alloc.into()], &bitcode::DICT_EMPTY);

    let result = env
        .builder
        .build_load(result_alloc, "load_result")
        .into_struct_value();

    zig_dict_to_struct(env, result).into()
}

fn dict_symbol_to_i128<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    symbol: Symbol,
) -> IntValue<'ctx> {
    let dict = load_symbol(scope, &symbol);

    let i128_type = env.context.i128_type().into();

    complex_bitcast(&env.builder, dict, i128_type, "dict_to_i128").into_int_value()
}

fn zig_dict_to_struct<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    zig_dict: StructValue<'ctx>,
) -> StructValue<'ctx> {
    let builder = env.builder;

    // get the RocStr type defined by zig
    let zig_str_type = env.module.get_struct_type("dict.RocDict").unwrap();

    let ret_type = BasicTypeEnum::StructType(collection(env.context, env.ptr_bytes));

    // a roundabout way of casting (LLVM does not accept a standard bitcast)
    let allocation = builder.build_alloca(zig_str_type, "zig_result");

    builder.build_store(allocation, zig_dict);

    let ptr3 = builder
        .build_bitcast(
            allocation,
            env.context.i128_type().ptr_type(AddressSpace::Generic),
            "cast",
        )
        .into_pointer_value();

    let ptr4 = builder
        .build_bitcast(
            ptr3,
            ret_type.into_struct_type().ptr_type(AddressSpace::Generic),
            "cast",
        )
        .into_pointer_value();

    builder.build_load(ptr4, "load").into_struct_value()
}
