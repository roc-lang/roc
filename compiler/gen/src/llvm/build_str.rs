use crate::llvm::build::{
    call_bitcode_fn, call_void_bitcode_fn, ptr_from_symbol, Env, InPlace, Scope,
};
use crate::llvm::build_list::{allocate_list, build_basic_phi2, load_list_ptr, store_list};
use crate::llvm::convert::collection;
use inkwell::builder::Builder;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::{AddressSpace, IntPredicate};
use roc_builtins::bitcode;
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout};

pub static CHAR_LAYOUT: Layout = Layout::Builtin(Builtin::Int8);

/// Str.split : Str, Str -> List Str
pub fn str_split<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    inplace: InPlace,
    str_symbol: Symbol,
    delimiter_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let str_ptr = ptr_from_symbol(scope, str_symbol);
    let delimiter_ptr = ptr_from_symbol(scope, delimiter_symbol);

    let str_i128 = str_symbol_to_i128(env, scope, str_symbol);
    let delim_i128 = str_symbol_to_i128(env, scope, delimiter_symbol);

    let str_wrapper_type = BasicTypeEnum::StructType(collection(ctx, env.ptr_bytes));

    load_str(
        env,
        parent,
        *str_ptr,
        str_wrapper_type,
        |str_bytes_ptr, str_len, _str_smallness| {
            load_str(
                env,
                parent,
                *delimiter_ptr,
                str_wrapper_type,
                |delimiter_bytes_ptr, delimiter_len, _delimiter_smallness| {
                    let segment_count = call_bitcode_fn(
                        env,
                        &[str_i128.into(), delim_i128.into()],
                        &bitcode::STR_COUNT_SEGMENTS,
                    )
                    .into_int_value();

                    // a pointer to the elements
                    let ret_list_ptr =
                        allocate_list(env, inplace, &Layout::Builtin(Builtin::Str), segment_count);

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
                        &[
                            ret_list_ptr_zig_rocstr,
                            BasicValueEnum::IntValue(segment_count),
                            str_i128.into(),
                            delim_i128.into(),
                        ],
                        &bitcode::STR_STR_SPLIT_IN_PLACE,
                    );

                    store_list(env, ret_list_ptr, segment_count)
                },
            )
        },
    )
}

/*
fn cast_to_zig_str(
    env: &Env<'a, 'ctx, 'env>,
    str_as_struct: StructValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    // get the RocStr type defined by zig
    let roc_str_type = env.module.get_struct_type("str.RocStr").unwrap();

    // convert `{ *mut u8, i64 }` to `RocStr`
    builder.build_bitcast(str_as_struct, roc_str_type, "convert_to_zig_rocstr");
}

fn cast_from_zig_str(
    env: &Env<'a, 'ctx, 'env>,
    str_as_struct: StructValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let ret_type = BasicTypeEnum::StructType(collection(ctx, env.ptr_bytes));

    // convert `RocStr` to `{ *mut u8, i64 }`
    builder.build_bitcast(str_as_struct, ret_type, "convert_from_zig_rocstr");
}
*/

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
    _parent: FunctionValue<'ctx>,
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
                .i32_type()
                .const_int(env.ptr_bytes as u64, false)
                .into(),
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

/// Obtain the string's length, cast from i8 to usize
fn str_len_from_final_byte<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    final_byte: IntValue<'ctx>,
) -> IntValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;
    let bitmask = ctx.i8_type().const_int(0b0111_1111, false);
    let len_i8 = builder.build_and(final_byte, bitmask, "small_str_length");

    builder.build_int_cast(len_i8, env.ptr_int(), "len_as_usize")
}

/// Used by LowLevel::StrIsEmpty
pub fn str_len<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    wrapper_ptr: PointerValue<'ctx>,
) -> IntValue<'ctx> {
    let builder = env.builder;

    let if_small = |final_byte| {
        let len = str_len_from_final_byte(env, final_byte);

        BasicValueEnum::IntValue(len)
    };

    let if_big = |_| {
        let len = big_str_len(
            builder,
            builder
                .build_load(wrapper_ptr, "big_str")
                .into_struct_value(),
        );

        BasicValueEnum::IntValue(len)
    };

    if_small_str(
        env,
        parent,
        wrapper_ptr,
        if_small,
        if_big,
        BasicTypeEnum::IntType(env.ptr_int()),
    )
    .into_int_value()
}

fn load_str<'a, 'ctx, 'env, Callback>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    wrapper_ptr: PointerValue<'ctx>,
    ret_type: BasicTypeEnum<'ctx>,
    cb: Callback,
) -> BasicValueEnum<'ctx>
where
    Callback: Fn(PointerValue<'ctx>, IntValue<'ctx>, Smallness) -> BasicValueEnum<'ctx>,
{
    let builder = env.builder;

    let if_small = |final_byte| {
        cb(
            cast_str_wrapper_to_array(env, wrapper_ptr),
            str_len_from_final_byte(env, final_byte),
            Smallness::Small,
        )
    };

    let if_big = |wrapper_struct| {
        let list_ptr = load_list_ptr(
            builder,
            wrapper_struct,
            env.context.i8_type().ptr_type(AddressSpace::Generic),
        );

        cb(
            list_ptr,
            big_str_len(builder, wrapper_struct),
            Smallness::Big,
        )
    };

    if_small_str(env, parent, wrapper_ptr, if_small, if_big, ret_type)
}

#[derive(Debug, Copy, Clone)]
enum Smallness {
    Small,
    Big,
}

fn cast_str_wrapper_to_array<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    wrapper_ptr: PointerValue<'ctx>,
) -> PointerValue<'ctx> {
    let array_ptr_type = env.context.i8_type().ptr_type(AddressSpace::Generic);

    env.builder
        .build_bitcast(wrapper_ptr, array_ptr_type, "str_as_array_ptr")
        .into_pointer_value()
}

fn if_small_str<'a, 'ctx, 'env, IfSmallFn, IfBigFn>(
    env: &Env<'a, 'ctx, 'env>,
    parent: FunctionValue<'ctx>,
    wrapper_ptr: PointerValue<'ctx>,
    mut if_small: IfSmallFn,
    mut if_big: IfBigFn,
    ret_type: BasicTypeEnum<'ctx>,
) -> BasicValueEnum<'ctx>
where
    IfSmallFn: FnMut(IntValue<'ctx>) -> BasicValueEnum<'ctx>,
    IfBigFn: FnMut(StructValue<'ctx>) -> BasicValueEnum<'ctx>,
{
    let builder = env.builder;
    let ctx = env.context;
    let byte_array_ptr = cast_str_wrapper_to_array(env, wrapper_ptr);
    let final_byte_ptr = unsafe {
        builder.build_in_bounds_gep(
            byte_array_ptr,
            &[ctx
                .i8_type()
                .const_int(env.small_str_bytes() as u64 - 1, false)],
            "final_byte_ptr",
        )
    };

    let final_byte = builder
        .build_load(final_byte_ptr, "load_final_byte")
        .into_int_value();

    let bitmask = ctx.i8_type().const_int(0b1000_0000, false);

    let is_small_i8 = builder.build_int_compare(
        IntPredicate::NE,
        ctx.i8_type().const_zero(),
        builder.build_and(final_byte, bitmask, "is_small"),
        "is_small_comparison",
    );

    let is_small = builder.build_int_cast(is_small_i8, ctx.bool_type(), "is_small_as_bool");

    build_basic_phi2(
        env,
        parent,
        is_small,
        || if_small(final_byte),
        || {
            if_big(
                builder
                    .build_load(wrapper_ptr, "load_wrapper_struct")
                    .into_struct_value(),
            )
        },
        ret_type,
    )
}

fn big_str_len<'ctx>(builder: &Builder<'ctx>, wrapper_struct: StructValue<'ctx>) -> IntValue<'ctx> {
    builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_LEN, "big_str_len")
        .unwrap()
        .into_int_value()
}

#[allow(dead_code)]
fn str_is_not_empty<'ctx>(env: &Env<'_, 'ctx, '_>, len: IntValue<'ctx>) -> IntValue<'ctx> {
    env.builder.build_int_compare(
        IntPredicate::UGT,
        len,
        env.ptr_int().const_zero(),
        "str_len_is_nonzero",
    )
}

/// Str.startsWith : Str, Str -> Bool
pub fn str_starts_with<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _inplace: InPlace,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    str_symbol: Symbol,
    prefix_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let ctx = env.context;

    let str_ptr = ptr_from_symbol(scope, str_symbol);
    let prefix_ptr = ptr_from_symbol(scope, prefix_symbol);

    let ret_type = BasicTypeEnum::IntType(ctx.bool_type());

    load_str(
        env,
        parent,
        *str_ptr,
        ret_type,
        |str_bytes_ptr, str_len, _str_smallness| {
            load_str(
                env,
                parent,
                *prefix_ptr,
                ret_type,
                |prefix_bytes_ptr, prefix_len, _prefix_smallness| {
                    call_bitcode_fn(
                        env,
                        &[
                            BasicValueEnum::PointerValue(str_bytes_ptr),
                            BasicValueEnum::IntValue(str_len),
                            BasicValueEnum::PointerValue(prefix_bytes_ptr),
                            BasicValueEnum::IntValue(prefix_len),
                        ],
                        &bitcode::STR_STARTS_WITH,
                    )
                },
            )
        },
    )
}

/// Str.countGraphemes : Str -> Int
pub fn str_count_graphemes<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    parent: FunctionValue<'ctx>,
    str_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let ctx = env.context;

    let sym_str_ptr = ptr_from_symbol(scope, str_symbol);
    let ret_type = BasicTypeEnum::IntType(ctx.i64_type());

    load_str(
        env,
        parent,
        *sym_str_ptr,
        ret_type,
        |str_ptr, str_len, _str_smallness| {
            call_bitcode_fn(
                env,
                &[
                    BasicValueEnum::PointerValue(str_ptr),
                    BasicValueEnum::IntValue(str_len),
                ],
                &bitcode::STR_COUNT_GRAPEHEME_CLUSTERS,
            )
        },
    )
}
