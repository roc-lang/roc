use crate::llvm::bitcode::{call_bitcode_fn, call_str_bitcode_fn, call_void_bitcode_fn};
use crate::llvm::build::{Env, Scope};
use crate::llvm::build_list::pass_update_mode;
use inkwell::builder::Builder;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue, StructValue};
use inkwell::AddressSpace;
use morphic_lib::UpdateMode;
use roc_builtins::bitcode::{self, IntWidth};
use roc_module::symbol::Symbol;
use roc_mono::layout::{Builtin, Layout};
use roc_target::PtrWidth;

use super::build::{create_entry_block_alloca, load_symbol};
use super::build_list::list_symbol_to_c_abi;

pub static CHAR_LAYOUT: Layout = Layout::u8();

pub fn str_symbol_to_c_abi<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    symbol: Symbol,
) -> PointerValue<'ctx> {
    let string = load_symbol(scope, &symbol);

    str_to_c_abi(env, string)
}

pub fn str_to_c_abi<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    let parent = env
        .builder
        .get_insert_block()
        .and_then(|b| b.get_parent())
        .unwrap();

    let str_type = super::convert::zig_str_type(env);
    let string_alloca = create_entry_block_alloca(env, parent, str_type.into(), "str_alloca");

    env.builder.build_store(string_alloca, value);

    string_alloca
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

/// Str.fromInt : Int -> Str
pub fn str_from_int<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    value: IntValue<'ctx>,
    int_width: IntWidth,
) -> BasicValueEnum<'ctx> {
    call_str_bitcode_fn(env, &[value.into()], &bitcode::STR_FROM_INT[int_width])
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
    scope: &Scope<'a, 'ctx>,
    list: Symbol,
    count_and_start: StructValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let result_type = env.module.get_struct_type("str.FromUtf8Result").unwrap();
    let result_ptr = builder.build_alloca(result_type, "alloca_utf8_validate_bytes_result");

    let count = env
        .builder
        .build_extract_value(count_and_start, 0, "get_count")
        .unwrap();

    let start = env
        .builder
        .build_extract_value(count_and_start, 1, "get_start")
        .unwrap();

    call_void_bitcode_fn(
        env,
        &[
            result_ptr.into(),
            list_symbol_to_c_abi(env, scope, list).into(),
            count,
            start,
        ],
        bitcode::STR_FROM_UTF8_RANGE,
    );

    decode_from_utf8_result(env, result_ptr).into()
}

/// Str.fromUtf8 : List U8 -> { a : Bool, b : Str, c : Nat, d : I8 }
pub fn str_from_utf8<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    scope: &Scope<'a, 'ctx>,
    list: Symbol,
    update_mode: UpdateMode,
) -> BasicValueEnum<'ctx> {
    let builder = env.builder;

    let result_type = env.module.get_struct_type("str.FromUtf8Result").unwrap();
    let result_ptr = builder.build_alloca(result_type, "alloca_utf8_validate_bytes_result");

    call_void_bitcode_fn(
        env,
        &[
            result_ptr.into(),
            list_symbol_to_c_abi(env, scope, list).into(),
            pass_update_mode(env, update_mode),
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
    call_bitcode_fn(env, &[value1, value2], bitcode::STR_EQUAL)
}
