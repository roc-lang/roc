use crate::llvm::build::Env;
use inkwell::values::BasicValueEnum;
use inkwell::{FloatPredicate, IntPredicate};
use roc_mono::layout::{Builtin, Layout};

pub fn build_eq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: &Layout<'a>,
    rhs_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    match (lhs_layout, rhs_layout) {
        (Layout::Builtin(lhs_builtin), Layout::Builtin(rhs_builtin)) => {
            let int_cmp = |pred, label| {
                let int_val = env.builder.build_int_compare(
                    pred,
                    lhs_val.into_int_value(),
                    rhs_val.into_int_value(),
                    label,
                );

                BasicValueEnum::IntValue(int_val)
            };

            let float_cmp = |pred, label| {
                let int_val = env.builder.build_float_compare(
                    pred,
                    lhs_val.into_float_value(),
                    rhs_val.into_float_value(),
                    label,
                );

                BasicValueEnum::IntValue(int_val)
            };

            match (lhs_builtin, rhs_builtin) {
                (Builtin::Int128, Builtin::Int128) => int_cmp(IntPredicate::EQ, "eq_i128"),
                (Builtin::Int64, Builtin::Int64) => int_cmp(IntPredicate::EQ, "eq_i64"),
                (Builtin::Int32, Builtin::Int32) => int_cmp(IntPredicate::EQ, "eq_i32"),
                (Builtin::Int16, Builtin::Int16) => int_cmp(IntPredicate::EQ, "eq_i16"),
                (Builtin::Int8, Builtin::Int8) => int_cmp(IntPredicate::EQ, "eq_i8"),
                (Builtin::Int1, Builtin::Int1) => int_cmp(IntPredicate::EQ, "eq_i1"),
                (Builtin::Float64, Builtin::Float64) => float_cmp(FloatPredicate::OEQ, "eq_f64"),
                (Builtin::Float32, Builtin::Float32) => float_cmp(FloatPredicate::OEQ, "eq_f32"),
                (b1, b2) => {
                    todo!("Handle equals for builtin layouts {:?} == {:?}", b1, b2);
                }
            }
        }
        (other1, other2) => {
            // TODO NOTE: This should ultimately have a _ => todo!("type mismatch!") branch
            todo!("implement equals for layouts {:?} == {:?}", other1, other2);
        }
    }
}

pub fn build_neq<'a, 'ctx, 'env>(
    env: &Env<'a, 'ctx, 'env>,
    lhs_val: BasicValueEnum<'ctx>,
    rhs_val: BasicValueEnum<'ctx>,
    lhs_layout: &Layout<'a>,
    rhs_layout: &Layout<'a>,
) -> BasicValueEnum<'ctx> {
    match (lhs_layout, rhs_layout) {
        (Layout::Builtin(lhs_builtin), Layout::Builtin(rhs_builtin)) => {
            let int_cmp = |pred, label| {
                let int_val = env.builder.build_int_compare(
                    pred,
                    lhs_val.into_int_value(),
                    rhs_val.into_int_value(),
                    label,
                );

                BasicValueEnum::IntValue(int_val)
            };

            let float_cmp = |pred, label| {
                let int_val = env.builder.build_float_compare(
                    pred,
                    lhs_val.into_float_value(),
                    rhs_val.into_float_value(),
                    label,
                );

                BasicValueEnum::IntValue(int_val)
            };

            match (lhs_builtin, rhs_builtin) {
                (Builtin::Int128, Builtin::Int128) => int_cmp(IntPredicate::NE, "neq_i128"),
                (Builtin::Int64, Builtin::Int64) => int_cmp(IntPredicate::NE, "neq_i64"),
                (Builtin::Int32, Builtin::Int32) => int_cmp(IntPredicate::NE, "neq_i32"),
                (Builtin::Int16, Builtin::Int16) => int_cmp(IntPredicate::NE, "neq_i16"),
                (Builtin::Int8, Builtin::Int8) => int_cmp(IntPredicate::NE, "neq_i8"),
                (Builtin::Int1, Builtin::Int1) => int_cmp(IntPredicate::NE, "neq_i1"),
                (Builtin::Float64, Builtin::Float64) => float_cmp(FloatPredicate::ONE, "neq_f64"),
                (Builtin::Float32, Builtin::Float32) => float_cmp(FloatPredicate::ONE, "neq_f32"),
                (b1, b2) => {
                    todo!("Handle not equals for builtin layouts {:?} == {:?}", b1, b2);
                }
            }
        }
        (other1, other2) => {
            // TODO NOTE: This should ultimately have a _ => todo!("type mismatch!") branch
            todo!(
                "implement not equals for layouts {:?} == {:?}",
                other1,
                other2
            );
        }
    }
}
