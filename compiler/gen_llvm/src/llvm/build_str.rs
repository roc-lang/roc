use crate::llvm::bitcode::{
    call_bitcode_fn, call_list_bitcode_fn, call_str_bitcode_fn, call_void_bitcode_fn,
};
use crate::llvm::build::{complex_bitcast, Env, Scope};
use crate::llvm::build_list::{allocate_list, pass_update_mode, store_list};
use inkwell::builder::Builder;
use inkwell::values::{BasicValueEnum, FunctionValue, IntValue, PointerValue, StructValue};
use inkwell::AddressSpace;
use morphic_lib::UpdateMode;
use roc_builtins::bitcode::{self, IntWidth};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout};
use roc_target::PtrWidth;

use super::build::load_symbol;

pub static CHAR_LAYOUT: Layout = Layout::u8();

/// Str.repeat : Str, Nat -> Str
pub fn str_repeat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
    count_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let str_c_abi = str_symbol_to_c_abi(env, scope, str_symbol);
    let count = load_symbol(scope, &count_symbol);
    call_str_bitcode_fn(env, &[str_c_abi.into(), count], bitcode::STR_REPEAT)
}

/// Str.split : Str, Str -> List Str
pub fn str_split<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
    delimiter_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let str_c_abi = str_symbol_to_c_abi(env, scope, str_symbol);
    let delim_c_abi = str_symbol_to_c_abi(env, scope, delimiter_symbol);

    let segment_count = call_list_bitcode_fn(
        env,
        &[str_c_abi.into(), delim_c_abi.into()],
        bitcode::STR_COUNT_SEGMENTS,
    )
    .into_int_value();

    // a pointer to the elements
    let ret_list_ptr = allocate_list(env, &Layout::Builtin(Builtin::Str), segment_count);

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
            str_c_abi.into(),
            delim_c_abi.into(),
        ],
        bitcode::STR_STR_SPLIT_IN_PLACE,
    );

    store_list(env, ret_list_ptr, segment_count)
}

fn str_symbol_to_c_abi<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    symbol: Symbol,
) -> IntValue<'ctx> {
    let string = load_symbol(scope, &symbol);

    let target_type = match env.target_info.ptr_width() {
        PtrWidth::Bytes8 => env.context.i128_type().into(),
        PtrWidth::Bytes4 => env.context.i64_type().into(),
    };

    complex_bitcast(env.builder, string, target_type, "str_to_c_abi").into_int_value()
}

pub fn str_to_c_abi<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: BasicValueEnum<'ctx>,
) -> IntValue<'ctx> {
    let cell = env.builder.build_alloca(value.get_type(), "cell");

    env.builder.build_store(cell, value);

    let target_type = match env.target_info.ptr_width() {
        PtrWidth::Bytes8 => env.context.i128_type(),
        PtrWidth::Bytes4 => env.context.i64_type(),
    };

    let target_type_ptr = env
        .builder
        .build_bitcast(cell, target_type.ptr_type(AddressSpace::Generic), "cast")
        .into_pointer_value();

    env.builder
        .build_load(target_type_ptr, "load_as_c_abi")
        .into_int_value()
}

pub fn destructure<'ctx>(
    builder: &Builder<'ctx>,
    wrapper_struct: StructValue<'ctx>,
) -> (PointerValue<'ctx>, IntValue<'ctx>) {
    let length = builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_LEN, "list_len")
        .unwrap()
        .into_int_value();

    // a `*mut u8` pointer
    let generic_ptr = builder
        .build_extract_value(wrapper_struct, Builtin::WRAPPER_PTR, "read_list_ptr")
        .unwrap()
        .into_pointer_value();

    (generic_ptr, length)
}

/// Str.concat : Str, Str -> Str
pub fn str_concat<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str1_symbol: Symbol,
    str2_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    // swap the arguments; second argument comes before the second in the output string
    let str1_c_abi = str_symbol_to_c_abi(env, scope, str1_symbol);
    let str2_c_abi = str_symbol_to_c_abi(env, scope, str2_symbol);

    call_str_bitcode_fn(
        env,
        &[str1_c_abi.into(), str2_c_abi.into()],
        bitcode::STR_CONCAT,
    )
}

/// Str.join : List Str, Str -> Str
pub fn str_join_with<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    list_symbol: Symbol,
    str_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    // dirty hack; pretend a `list` is a `str` that works because
    // they have the same stack layout `{ u8*, usize }`
    let list_i128 = str_symbol_to_c_abi(env, scope, list_symbol);
    let str_i128 = str_symbol_to_c_abi(env, scope, str_symbol);

    call_str_bitcode_fn(
        env,
        &[list_i128.into(), str_i128.into()],
        bitcode::STR_JOIN_WITH,
    )
}

pub fn str_number_of_bytes<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
) -> IntValue<'ctx> {
    let str_i128 = str_symbol_to_c_abi(env, scope, str_symbol);

    // the builtin will always return an u64
    let length =
        call_bitcode_fn(env, &[str_i128.into()], bitcode::STR_NUMBER_OF_BYTES).into_int_value();

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
    let str_i128 = str_symbol_to_c_abi(env, scope, str_symbol);
    let prefix_i128 = str_symbol_to_c_abi(env, scope, prefix_symbol);

    call_bitcode_fn(
        env,
        &[str_i128.into(), prefix_i128.into()],
        bitcode::STR_STARTS_WITH,
    )
}

/// Str.startsWithCodePt : Str, U32 -> Bool
pub fn str_starts_with_code_point<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
    prefix_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let str_i128 = str_symbol_to_c_abi(env, scope, str_symbol);
    let prefix = load_symbol(scope, &prefix_symbol);

    call_bitcode_fn(
        env,
        &[str_i128.into(), prefix],
        bitcode::STR_STARTS_WITH_CODE_PT,
    )
}

/// Str.endsWith : Str, Str -> Bool
pub fn str_ends_with<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
    prefix_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let str_i128 = str_symbol_to_c_abi(env, scope, str_symbol);
    let prefix_i128 = str_symbol_to_c_abi(env, scope, prefix_symbol);

    call_bitcode_fn(
        env,
        &[str_i128.into(), prefix_i128.into()],
        bitcode::STR_ENDS_WITH,
    )
}

/// Str.countGraphemes : Str -> Int
pub fn str_count_graphemes<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let str_i128 = str_symbol_to_c_abi(env, scope, str_symbol);

    call_bitcode_fn(
        env,
        &[str_i128.into()],
        bitcode::STR_COUNT_GRAPEHEME_CLUSTERS,
    )
}

/// Str.trim : Str -> Str
pub fn str_trim<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let str_i128 = str_symbol_to_c_abi(env, scope, str_symbol);
    call_str_bitcode_fn(env, &[str_i128.into()], bitcode::STR_TRIM)
}

/// Str.trimLeft : Str -> Str
pub fn str_trim_left<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let str_i128 = str_symbol_to_c_abi(env, scope, str_symbol);
    call_str_bitcode_fn(env, &[str_i128.into()], bitcode::STR_TRIM_LEFT)
}

/// Str.trimRight : Str -> Str
pub fn str_trim_right<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    str_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let str_i128 = str_symbol_to_c_abi(env, scope, str_symbol);
    call_str_bitcode_fn(env, &[str_i128.into()], bitcode::STR_TRIM_RIGHT)
}

/// Str.fromInt : Int -> Str
pub fn str_from_int<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: IntValue<'ctx>,
    int_width: IntWidth,
) -> BasicValueEnum<'ctx> {
    call_str_bitcode_fn(env, &[value.into()], &bitcode::STR_FROM_INT[int_width])
}

/// Str.toUtf8 : Str -> List U8
pub fn str_to_utf8<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    original_wrapper: StructValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let string = complex_bitcast(
        env.builder,
        original_wrapper.into(),
        env.str_list_c_abi().into(),
        "to_utf8",
    );

    call_list_bitcode_fn(env, &[string], bitcode::STR_TO_UTF8)
}

fn decode_from_utf8_result<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    pointer: PointerValue<'ctx>,
) -> StructValue<'ctx> {
    let builder = env.builder;
    let ctx = env.context;

    let fields = match env.target_info.ptr_width() {
        PtrWidth::Bytes4 | PtrWidth::Bytes8 => [
            env.ptr_int().into(),
            super::convert::zig_str_type(env).into(),
            env.context.bool_type().into(),
            ctx.i8_type().into(),
        ],
    };

    let record_type = env.context.struct_type(&fields, false);

    match env.target_info.ptr_width() {
        PtrWidth::Bytes4 | PtrWidth::Bytes8 => {
            let result_ptr_cast = env
                .builder
                .build_bitcast(
                    pointer,
                    record_type.ptr_type(AddressSpace::Generic),
                    "to_unnamed",
                )
                .into_pointer_value();

            builder
                .build_load(result_ptr_cast, "load_utf8_validate_bytes_result")
                .into_struct_value()
        }
    }
}

/// Str.fromUtf8 : List U8, { count : Nat, start : Nat } -> { a : Bool, b : Str, c : Nat, d : I8 }
pub fn str_from_utf8_range<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _parent: FunctionValue<'ctx>,
    list_wrapper: StructValue<'ctx>,
    count_and_start: StructValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let result_type = env.module.get_struct_type("str.FromUtf8Result").unwrap();
    let result_ptr = builder.build_alloca(result_type, "alloca_utf8_validate_bytes_result");

    call_void_bitcode_fn(
        env,
        &[
            complex_bitcast(
                env.builder,
                list_wrapper.into(),
                env.str_list_c_abi().into(),
                "to_i128",
            ),
            complex_bitcast(
                env.builder,
                count_and_start.into(),
                env.str_list_c_abi().into(),
                "to_i128",
            ),
            result_ptr.into(),
        ],
        bitcode::STR_FROM_UTF8_RANGE,
    );

    decode_from_utf8_result(env, result_ptr).into()
}

/// Str.fromUtf8 : List U8 -> { a : Bool, b : Str, c : Nat, d : I8 }
pub fn str_from_utf8<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    _parent: FunctionValue<'ctx>,
    original_wrapper: StructValue<'ctx>,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let result_type = env.module.get_struct_type("str.FromUtf8Result").unwrap();
    let result_ptr = builder.build_alloca(result_type, "alloca_utf8_validate_bytes_result");

    call_void_bitcode_fn(
        env,
        &[
            complex_bitcast(
                env.builder,
                original_wrapper.into(),
                env.str_list_c_abi().into(),
                "to_i128",
            ),
            pass_update_mode(env, update_mode),
            result_ptr.into(),
        ],
        bitcode::STR_FROM_UTF8,
    );

    decode_from_utf8_result(env, result_ptr).into()
}

/// Str.fromFloat : Int -> Str
pub fn str_from_float<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    int_symbol: Symbol,
) -> BasicValueEnum<'ctx> {
    let float = load_symbol(scope, &int_symbol);

    call_str_bitcode_fn(env, &[float], bitcode::STR_FROM_FLOAT)
}

/// Str.equal : Str, Str -> Bool
pub fn str_equal<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value1: BasicValueEnum<'ctx>,
    value2: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let str1_i128 = str_to_c_abi(env, value1);
    let str2_i128 = str_to_c_abi(env, value2);

    call_bitcode_fn(
        env,
        &[str1_i128.into(), str2_i128.into()],
        bitcode::STR_EQUAL,
    )
}
