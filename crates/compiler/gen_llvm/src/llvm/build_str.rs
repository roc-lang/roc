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
pub(crate) fn dec_to_str<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    dec: BasicValueEnum<'ctx>,
) -> BasicValueEnum<'ctx> {
    let dec = dec.into_int_value();

    let int_64 = env.context.i128_type().const_int(64, false);
    let int_64_type = env.context.i64_type();

    let dec_right_shift = env
        .builder
        .build_right_shift(dec, int_64, false, "dec_left_bits");

    let right_bits = env.builder.build_int_cast(dec, int_64_type, "");
    let left_bits = env.builder.build_int_cast(dec_right_shift, int_64_type, "");

    call_str_bitcode_fn(
        env,
        &[],
        &[right_bits.into(), left_bits.into()],
        BitcodeReturns::Str,
        bitcode::DEC_TO_STR,
    )
}

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
