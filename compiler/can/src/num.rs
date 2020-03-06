use crate::env::Env;
use crate::expr::Expr;
use roc_parse::ast::Base;
use roc_problem::can::Problem;
use roc_problem::can::RuntimeError::*;
use roc_types::subs::VarStore;
use std::i64;

#[inline(always)]
pub fn int_expr_from_result(
    var_store: &VarStore,
    result: Result<i64, &str>,
    env: &mut Env,
) -> Expr {
    match result {
        Ok(int) => Expr::Int(var_store.fresh(), int),
        Err(raw) => {
            let runtime_error = IntOutsideRange(raw.into());

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            Expr::RuntimeError(runtime_error)
        }
    }
}

#[inline(always)]
pub fn float_expr_from_result(
    var_store: &VarStore,
    result: Result<f64, &str>,
    env: &mut Env,
) -> Expr {
    match result {
        Ok(float) => Expr::Float(var_store.fresh(), float),
        Err(raw) => {
            let runtime_error = FloatOutsideRange(raw.into());

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            Expr::RuntimeError(runtime_error)
        }
    }
}

#[inline(always)]
pub fn finish_parsing_int(raw: &str) -> Result<i64, &str> {
    // Ignore underscores.
    raw.replace("_", "").parse::<i64>().map_err(|_| raw)
}

#[inline(always)]
pub fn finish_parsing_base(raw: &str, base: Base) -> Result<i64, &str> {
    let radix = match base {
        Base::Hex => 16,
        Base::Octal => 8,
        Base::Binary => 2,
    };

    // Ignore underscores.
    i64::from_str_radix(raw.replace("_", "").as_str(), radix).map_err(|_| raw)
}

#[inline(always)]
pub fn finish_parsing_float(raw: &str) -> Result<f64, &str> {
    // Ignore underscores.
    match raw.replace("_", "").parse::<f64>() {
        Ok(float) if float.is_finite() => Ok(float),
        _ => Err(raw),
    }
}
