use super::build::BuilderExt;
use crate::llvm::build::Env;
use inkwell::values::{BasicValueEnum, IntValue};
use inkwell::IntPredicate;
use roc_builtins::bitcode::IntWidth;
use roc_mono::layout::{
    Builtin, InLayout, LayoutIds, LayoutInterner, LayoutRepr, STLayoutInterner,
};

pub fn generic_compare<'a, 'ctx>(
    env: &Env<'a, 'ctx, '_>,
    layout_interner: &STLayoutInterner<'a>,
    _layout_ids: &mut LayoutIds<'a>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: InLayout<'a>,
    _rhs_layout: InLayout<'a>,
) -> BasicValueEnum<'ctx> {
    let lhs_repr = layout_interner.get_repr(lhs_layout);
    let result = match lhs_repr {
        LayoutRepr::Builtin(Builtin::Int(int_width)) => {
            int_compare(env, lhs_val, rhs_val, int_width)
        }
        LayoutRepr::Builtin(Builtin::Float(_)) => todo!(),
        LayoutRepr::Builtin(Builtin::Bool) => todo!(),
        LayoutRepr::Builtin(Builtin::Decimal) => todo!(),
        LayoutRepr::Builtin(Builtin::Str) => todo!(),
        LayoutRepr::Builtin(Builtin::List(_)) => todo!(),
        LayoutRepr::Struct(_) => todo!(),
        LayoutRepr::LambdaSet(_) => unreachable!("cannot compare closures"),
        LayoutRepr::FunctionPointer(_) => unreachable!("cannot compare function pointers"),
        LayoutRepr::Erased(_) => unreachable!("cannot compare erased types"),
        LayoutRepr::Union(_) => todo!(),
        LayoutRepr::Ptr(_) => todo!(),
        LayoutRepr::RecursivePointer(_) => todo!(),
    };
    BasicValueEnum::IntValue(result)
}

fn int_compare<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: IntWidth,
) -> IntValue<'ctx> {
    // The following calculation will return 0 for equals, 1 for greater than,
    // and 2 for less than.
    // (a > b) + 2 * (a < b);
    let lhs_gt_rhs = int_gt(env, lhs_val, rhs_val, builtin);
    let lhs_lt_rhs = int_lt(env, lhs_val, rhs_val, builtin);
    let two = env.ptr_int().const_int(2, false);
    let lhs_lt_rhs_times_two =
        env.builder
            .new_build_int_mul(lhs_lt_rhs, two, "lhs_lt_rhs_times_two");
    env.builder
        .new_build_int_sub(lhs_gt_rhs, lhs_lt_rhs_times_two, "int_compare")
}

fn int_lt<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: IntWidth,
) -> IntValue<'ctx> {
    use IntWidth::*;
    match builtin {
        I128 => env.builder.new_build_int_compare(
            IntPredicate::SLT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_i28",
        ),
        I64 => env.builder.new_build_int_compare(
            IntPredicate::SLT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_i64",
        ),
        I32 => env.builder.new_build_int_compare(
            IntPredicate::SLT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_i32",
        ),
        I16 => env.builder.new_build_int_compare(
            IntPredicate::SLT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_i16",
        ),
        I8 => env.builder.new_build_int_compare(
            IntPredicate::SLT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_i8",
        ),
        U128 => env.builder.new_build_int_compare(
            IntPredicate::ULT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_u128",
        ),
        U64 => env.builder.new_build_int_compare(
            IntPredicate::ULT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_u64",
        ),
        U32 => env.builder.new_build_int_compare(
            IntPredicate::ULT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_u32",
        ),
        U16 => env.builder.new_build_int_compare(
            IntPredicate::ULT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_u16",
        ),
        U8 => env.builder.new_build_int_compare(
            IntPredicate::ULT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_lt_rhs_u8",
        ),
    }
}

fn int_gt<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: IntWidth,
) -> IntValue<'ctx> {
    use IntWidth::*;
    match builtin {
        I128 => env.builder.new_build_int_compare(
            IntPredicate::SGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_i28",
        ),
        I64 => env.builder.new_build_int_compare(
            IntPredicate::SGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_i64",
        ),
        I32 => env.builder.new_build_int_compare(
            IntPredicate::SGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_i32",
        ),
        I16 => env.builder.new_build_int_compare(
            IntPredicate::SGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_i16",
        ),
        I8 => env.builder.new_build_int_compare(
            IntPredicate::SGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_i8",
        ),
        U128 => env.builder.new_build_int_compare(
            IntPredicate::UGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_u128",
        ),
        U64 => env.builder.new_build_int_compare(
            IntPredicate::UGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_u64",
        ),
        U32 => env.builder.new_build_int_compare(
            IntPredicate::UGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_u32",
        ),
        U16 => env.builder.new_build_int_compare(
            IntPredicate::UGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_u16",
        ),
        U8 => env.builder.new_build_int_compare(
            IntPredicate::UGT,
            lhs_val.into_int_value(),
            rhs_val.into_int_value(),
            "lhs_gt_rhs_u8",
        ),
    }
}
