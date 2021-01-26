use crate::llvm::build::{
    call_bitcode_fn, call_void_bitcode_fn, ptr_from_symbol, Env, InPlace, Scope,
};
use crate::llvm::build_list::{allocate_list, store_list};
use crate::llvm::convert::collection;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, IntValue, StructValue};
use inkwell::AddressSpace;
use roc_builtins::bitcode;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout};

use super::build::load_symbol;

pub static CHAR_LAYOUT: Layout = Layout::Builtin(Builtin::Int8);

/// Str.split : Str, Str -> List Str
pub fn str_split<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    inplace: InPlace,
    str_symbol: Symbol,
    delimiter_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let str_i128 = str_symbol_to_i128(env, scope, str_symbol);
    let delim_i128 = str_symbol_to_i128(env, scope, delimiter_symbol);

    let segment_count = call_bitcode_fn(
        env,
        &[str_i128.into(), delim_i128.into()],
        &bitcode::STR_COUNT_SEGMENTS,
    )
    .into_int_value();

    // a pointer to the elements
    let ret_list_ptr = allocate_list(env, inplace, &Layout::Builtin(Builtin::Str), segment_count);

    // get the RocStr type defined by zig
    let roc_str_type = env.module.get_struct_type("str.RocStr").unwrap();

    // convert `*mut { *mut u8, i64 }` to `*mut RocStr`
    let ret_list_ptr_zig_rocstr = builder.build_bitcast(
        ret_list_ptr,
        roc_str_type.ptr_type(AddressSpace::Generic),
        "convert_to_zig_rocstr",
    );

    call_void_bitcode_fn(
        env,
        &[ret_list_ptr_zig_rocstr, str_i128.into(), delim_i128.into()],
        &bitcode::STR_STR_SPLIT_IN_PLACE,
    );

    store_list(env, ret_list_ptr, segment_count)
}

fn str_symbol_to_i128<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    symbol: Symbol,
) -> IntValue<'ctx> {
    let str_ptr = ptr_from_symbol(scope, symbol);

    let i128_ptr = env
        .builder
        .build_bitcast(
            *str_ptr,
            env.context.i128_type().ptr_type(AddressSpace::Generic),
            "cast",
        )
        .into_pointer_value();

    env.builder
        .build_load(i128_ptr, "load_as_i128")
        .into_int_value()
}

fn str_to_i128<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: BasicValueEnum<'ctx>,
) -> IntValue<'ctx> {
    let cell = env.builder.build_alloca(value.get_type(), "cell");

    env.builder.build_store(cell, value);

    let i128_ptr = env
        .builder
        .build_bitcast(
            cell,
            env.context.i128_type().ptr_type(AddressSpace::Generic),
            "cast",
        )
        .into_pointer_value();

    env.builder
        .build_load(i128_ptr, "load_as_i128")
        .into_int_value()
}

fn zig_str_to_struct<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    zig_str: StructValue<'ctx>,
) -> StructValue<'ctx> {
    let builder = env.builder;

    // get the RocStr type defined by zig
    let zig_str_type = env.module.get_struct_type("str.RocStr").unwrap();

    let ret_type = BasicTypeEnum::StructType(collection(env.context, env.ptr_bytes));

    // a roundabout way of casting (LLVM does not accept a standard bitcast)
    let allocation = builder.build_alloca(zig_str_type, "zig_result");

    builder.build_store(allocation, zig_str);

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

/// Str.concat : Str, Str -> Str
pub fn str_concat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    inplace: InPlace,
    scope: &Scope<'a, 'ctx>,
    str1_symbol: Symbol,
    str2_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    // swap the arguments; second argument comes before the second in the output string
    let str1_i128 = str_symbol_to_i128(env, scope, str1_symbol);
    let str2_i128 = str_symbol_to_i128(env, scope, str2_symbol);

    let zig_result = call_bitcode_fn(
        env,
        &[
            env.context
                .i8_type()
                .const_int(inplace as u64, false)
                .into(),
            str1_i128.into(),
            str2_i128.into(),
        ],
        &bitcode::STR_CONCAT,
    )
    .into_struct_value();

    zig_str_to_struct(env, zig_result).into()
}

/// Str.join : List Str, Str -> Str
pub fn str_join_with<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _inplace: InPlace,
    scope: &Scope<'a, 'ctx>,
    list_symbol: Symbol,
    str_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    // dirty hack; pretend a `list` is a `str` that works because
    // they have the same stack layout `{ u8*, usize }`
    let list_i128 = str_symbol_to_i128(env, scope, list_symbol);
    let str_i128 = str_symbol_to_i128(env, scope, str_symbol);

    let zig_result = call_bitcode_fn(
        env,
        &[list_i128.into(), str_i128.into()],
        &bitcode::STR_JOIN_WITH,
    )
    .into_struct_value();

    zig_str_to_struct(env, zig_result).into()
}

pub fn str_number_of_bytes<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
) -> IntValue<'ctx> {
    let str_i128 = str_symbol_to_i128(env, scope, str_symbol);

    // the builtin will always return an u64
    let length =
        call_bitcode_fn(env, &[str_i128.into()], &bitcode::STR_NUMBER_OF_BYTES).into_int_value();

    // cast to the appropriate usize of the current build
    env.builder
        .build_int_cast(length, env.ptr_int(), "len_as_usize")
}

/// Str.startsWith : Str, Str -> Bool
pub fn str_starts_with<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
    prefix_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let str_i128 = str_symbol_to_i128(env, scope, str_symbol);
    let prefix_i128 = str_symbol_to_i128(env, scope, prefix_symbol);

    call_bitcode_fn(
        env,
        &[str_i128.into(), prefix_i128.into()],
        &bitcode::STR_STARTS_WITH,
    )
}

/// Str.endsWith : Str, Str -> Bool
pub fn str_ends_with<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
    prefix_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let str_i128 = str_symbol_to_i128(env, scope, str_symbol);
    let prefix_i128 = str_symbol_to_i128(env, scope, prefix_symbol);

    call_bitcode_fn(
        env,
        &[str_i128.into(), prefix_i128.into()],
        &bitcode::STR_ENDS_WITH,
    )
}

/// Str.countGraphemes : Str -> Int
pub fn str_count_graphemes<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let str_i128 = str_symbol_to_i128(env, scope, str_symbol);

    call_bitcode_fn(
        env,
        &[str_i128.into()],
        &bitcode::STR_COUNT_GRAPEHEME_CLUSTERS,
    )
}

/// Str.fromInt : Int -> Str
pub fn str_from_int<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    int_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let int = load_symbol(env, scope, &int_symbol);

    let zig_result = call_bitcode_fn(env, &[int], &bitcode::STR_FROM_INT).into_struct_value();

    zig_str_to_struct(env, zig_result).into()
}

/// Str.equal : Str, Str -> Bool
pub fn str_equal<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value1: BasicValueEnum<'ctx>,
    value2: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let str1_i128 = str_to_i128(env, value1);
    let str2_i128 = str_to_i128(env, value2);

    call_bitcode_fn(
        env,
        &[str1_i128.into(), str2_i128.into()],
        &bitcode::STR_EQUAL,
    )
}
