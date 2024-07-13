use super::build::BuilderExt;
use crate::llvm::build::Env;
use crate::llvm::bitcode::call_bitcode_fn;
use inkwell::values::{BasicValueEnum, IntValue};
use inkwell::{IntPredicate, FloatPredicate};
use roc_builtins::bitcode::{IntWidth, FloatWidth, NUM_LESS_THAN, NUM_GREATER_THAN};
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
        LayoutRepr::Builtin(Builtin::Float(float_width)) => {
            float_cmp(env, lhs_val, rhs_val, float_width)
        }
        LayoutRepr::Builtin(Builtin::Bool) => {
            bool_compare(env, lhs_val, rhs_val)
        }
        LayoutRepr::Builtin(Builtin::Decimal) => {
            dec_compare(env, lhs_val, rhs_val)
        }
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
    let two = env.context.i8_type().const_int(2, false);
    let lhs_lt_rhs_times_two =
        env.builder
            .new_build_int_mul(lhs_lt_rhs, two, "lhs_lt_rhs_times_two");
    env.builder
        .new_build_int_add(lhs_gt_rhs, lhs_lt_rhs_times_two, "int_compare")
}



fn int_lt<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: IntWidth,
) -> IntValue<'ctx> {
    use IntWidth::*;
    let lhs_lt_rhs = match builtin {
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
    };
    env.builder.new_build_int_cast_sign_flag(lhs_lt_rhs, env.context.i8_type(), false, "lhs_lt_rhs_cast")
}

fn int_gt<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    builtin: IntWidth,
) -> IntValue<'ctx> {
    use IntWidth::*;
    let lhs_gt_rhs = match builtin {
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
    };
    env.builder.new_build_int_cast_sign_flag(lhs_gt_rhs, env.context.i8_type(), false, "lhs_gt_rhs_cast")
}


// Return 0 for equals, 1 for greater than, and 2 for less than.
// We consider NaNs to be smaller than non-NaNs
// We use the below expression to calculate this
// (a == a) + 2*(b == b) - (a < b) - 2*(a > b) - 3*(a == b)
fn float_cmp<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    float_width: FloatWidth,
) -> IntValue<'ctx> {
    use FloatWidth::*;
    let type_str = match float_width {
        F64 => "F64",
        F32 => "F32",
    };

    let make_cmp = |operation, a: BasicValueEnum<'ctx>, b: BasicValueEnum<'ctx>, op_name: &str| {
        let full_op_name = format!("{}_{}", op_name, type_str);
        let bool_result = env.builder.new_build_float_compare(
            operation,
            a.into_float_value(),
            b.into_float_value(),
            &full_op_name,
        );
        env.builder.new_build_int_cast_sign_flag(bool_result, env.context.i8_type(), false, &format!("{}_cast", full_op_name))
    };

    let two = env.context.i8_type().const_int(2, false);
    let three = env.context.i8_type().const_int(3, false);

    let lt_test = make_cmp(FloatPredicate::OLT,  lhs_val, rhs_val, "lhs_lt_rhs");
    let gt_test = make_cmp(FloatPredicate::OGT, lhs_val, rhs_val, "lhs_gt_rhs");
    let eq_test = make_cmp(FloatPredicate::OEQ, lhs_val, rhs_val, "lhs_eq_rhs");
    let lhs_not_nan_test = make_cmp(FloatPredicate::OEQ, lhs_val, lhs_val, "lhs_not_NaN");
    let rhs_not_nan_test = make_cmp(FloatPredicate::OEQ, rhs_val, rhs_val, "rhs_not_NaN");

    let rhs_not_nan_scaled = env.builder.new_build_int_mul(two, rhs_not_nan_test, "2 * rhs_not_nan");
    let gt_scaled = env.builder.new_build_int_mul(two, gt_test, "2 * lhs_gt_rhs");
    let eq_scaled = env.builder.new_build_int_mul(three, eq_test, "3 * lhs_eq_rhs");

    let non_nans = env.builder.new_build_int_add(lhs_not_nan_test, rhs_not_nan_scaled, "(a == a) + 2*(b == b))");
    let minus_lt = env.builder.new_build_int_sub(non_nans, lt_test, "(a == a) + 2*(b == b) - (a < b");
    let minus_gt = env.builder.new_build_int_sub(minus_lt, gt_scaled, "(a == a) + 2*(b == b) - (a < b) - 2*(a > b)");
    env.builder.new_build_int_sub(minus_gt, eq_scaled, "float_compare")
}

// 1 1 0
// 0 0 0
// 0 1 1
// 1 0 2
fn bool_compare<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
) -> IntValue<'ctx> {

    // Cast the input bools to ints because int comparison of bools does the opposite of what one would expect.
    // I could just swap the arguments, but I do not want to rely on behavior which seems wrong
    let lhs_byte = env.builder.new_build_int_cast_sign_flag(lhs_val.into_int_value(), env.context.i8_type(), false, "lhs_byte");
    let rhs_byte = env.builder.new_build_int_cast_sign_flag(rhs_val.into_int_value(), env.context.i8_type(), false, "rhs_byte");

    // (a < b)
    let lhs_lt_rhs = env.builder.new_build_int_compare(IntPredicate::SLT, lhs_byte, rhs_byte, "lhs_lt_rhs_bool");
    let lhs_lt_rhs_byte = env.builder.new_build_int_cast_sign_flag(lhs_lt_rhs, env.context.i8_type(), false, "lhs_lt_rhs_byte");
    
    // (a > b)
    let lhs_gt_rhs = env.builder.new_build_int_compare(IntPredicate::SGT, lhs_byte, rhs_byte, "lhs_gt_rhs_bool");
    let lhs_gt_rhs_byte = env.builder.new_build_int_cast_sign_flag(lhs_gt_rhs, env.context.i8_type(), false, "lhs_gt_rhs_byte");

    // (a < b) * 2
    let two = env.context.i8_type().const_int(2, false);
    let lhs_lt_rhs_times_two = env.builder.new_build_int_mul(lhs_lt_rhs_byte, two, "lhs_lt_rhs_times_two");

    // (a > b) + (a < b) * 2
    env.builder.new_build_int_add(lhs_gt_rhs_byte, lhs_lt_rhs_times_two, "bool_compare")
}

fn dec_compare<'ctx>(
    env: &Env<'_, 'ctx, '_>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
) -> IntValue<'ctx> {

    // (a > b)
    let lhs_gt_rhs = call_bitcode_fn(env, &[lhs_val, rhs_val], &NUM_GREATER_THAN[IntWidth::I128]).into_int_value();
    let lhs_gt_rhs_byte = env.builder.new_build_int_cast_sign_flag(lhs_gt_rhs, env.context.i8_type(), false, "lhs_gt_rhs_byte");
    
    // (a < b)
    let lhs_lt_rhs = call_bitcode_fn(env, &[lhs_val, rhs_val], &NUM_LESS_THAN[IntWidth::I128]).into_int_value();
    let lhs_lt_rhs_byte = env.builder.new_build_int_cast_sign_flag(lhs_lt_rhs, env.context.i8_type(), false, "lhs_lt_rhs_byte");

    // (a < b) * 2
    let two = env.context.i8_type().const_int(2, false);
    let lhs_lt_rhs_times_two = env.builder.new_build_int_mul(lhs_lt_rhs_byte, two, "lhs_gt_rhs_times_two");

    // (a > b) + (a < b) * 2
    env.builder.new_build_int_add(lhs_gt_rhs_byte, lhs_lt_rhs_times_two, "bool_compare")
}