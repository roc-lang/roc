use inkwell::values::{BasicValueEnum, PointerValue};
use roc_builtins::bitcode;
use roc_mono::layout::{InLayout, Layout, LayoutRepr, STLayoutInterner};

use super::bitcode::{
    call_str_bitcode_fn, call_void_bitcode_fn, pass_list_or_string_to_zig_32bit,
    pass_list_to_zig_64bit, pass_list_to_zig_wasm, BitcodeReturns,
};
use super::build::{create_entry_block_alloca, load_roc_value, Env};
use bumpalo::collections::Vec;

pub static CHAR_LAYOUT: InLayout = Layout::U8;

pub(crate) fn call_str_from_utf_bitcode_fn<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    args: &[BasicValueEnum<'ctx>],
    result_struct_name: &str,
    fn_name: &str,
) -> BasicValueEnum<'ctx> {
    let result_type = env.module.get_struct_type(result_struct_name).unwrap();
    let result_ptr = create_entry_block_alloca(env, result_type, "alloca_from_utf_result");
    // FromUtf8Result, FromUtf16Result, FromUtf32Result all have the same layout of
    // - index: u64
    // - string: RocStr
    // - is_ok: bool
    // - problem_code: u8
    let layout =
        LayoutRepr::Struct(
            env.arena
                .alloc([Layout::U64, Layout::STR, Layout::BOOL, Layout::U8]),
        );

    let list = args[0];
    let argn = &args[1..];
    let mut args: Vec<BasicValueEnum<'ctx>> = Vec::with_capacity_in(args.len() + 2, env.arena);
    args.push(result_ptr.into());

    use roc_target::Architecture::*;
    match env.target.architecture() {
        Aarch32 | X86_32 => {
            let (a, b) = pass_list_or_string_to_zig_32bit(env, list.into_struct_value());
            args.push(a.into());
            args.push(b.into());
        }
        Aarch64 | X86_64 => {
            let list = pass_list_to_zig_64bit(env, list);
            args.push(list.into());
        }
        Wasm32 => {
            let list = pass_list_to_zig_wasm(env, list);
            args.push(list.into());
        }
    };

    args.extend(argn);

    call_void_bitcode_fn(env, &args, fn_name);

    load_roc_value(
        env,
        layout_interner,
        layout,
        result_ptr,
        "load_from_utf_result",
    )
}

/// Dec.to_str : Dec -> Str

/// Str.equal : Str, Str -> Bool
pub(crate) fn str_equal<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    value1: BasicValueEnum<'ctx>,
    value2: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    call_str_bitcode_fn(
        env,
        &[value1, value2],
        &[],
        BitcodeReturns::Basic,
        bitcode::STR_EQUAL,
    )
}

// Gets a pointer to just after the refcount for a list or seamless slice.
// The value is just after the refcount so that normal lists and seamless slices can share code paths easily.
pub(crate) fn str_allocation_ptr<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    value: BasicValueEnum<'ctx>,
) -> PointerValue<'ctx> {
    call_str_bitcode_fn(
        env,
        &[value],
        &[],
        BitcodeReturns::Basic,
        bitcode::STR_ALLOCATION_PTR,
    )
    .into_pointer_value()
}
