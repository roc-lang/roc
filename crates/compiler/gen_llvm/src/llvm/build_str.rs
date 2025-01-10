use crate::llvm::build::Env;
use inkwell::values::{BasicValueEnum, PointerValue};
use roc_builtins::bitcode;
use roc_mono::layout::{InLayout, Layout, LayoutRepr, STLayoutInterner};

use super::bitcode::{call_str_bitcode_fn, BitcodeReturns};
use super::build::load_roc_value;

pub static CHAR_LAYOUT: InLayout = Layout::U8;

pub(crate) fn decode_from_utf8_result<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    pointer: PointerValue<'ctx>,
) -> BasicValueEnum<'ctx> {
    let layout =
        LayoutRepr::Struct(
            env.arena
                .alloc([Layout::U64, Layout::STR, Layout::BOOL, Layout::U8]),
        );

    load_roc_value(
        env,
        layout_interner,
        layout,
        pointer,
        "load_decode_from_utf8_result",
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
