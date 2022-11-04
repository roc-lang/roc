use crate::llvm::build::Env;
use inkwell::values::{BasicValueEnum, PointerValue, StructValue};
use inkwell::AddressSpace;
use roc_builtins::bitcode;
use roc_mono::layout::Layout;
use roc_target::PtrWidth;

use super::bitcode::{call_str_bitcode_fn, BitcodeReturns};

pub static CHAR_LAYOUT: Layout = Layout::u8();

pub(crate) fn decode_from_utf8_result<'a, 'ctx, 'env>(
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

/// Dec.toStr : Dec -> Str

/// Str.equal : Str, Str -> Bool
pub(crate) fn str_equal<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
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
