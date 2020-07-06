use crate::env::Env;
use crate::expr::Expr;
use roc_parse::ast::Base;
use roc_problem::can::Problem;
use roc_problem::can::RuntimeError::*;
use roc_region::all::Region;
use roc_types::subs::VarStore;
use std::i64;
use std::num::{ParseFloatError, ParseIntError};

// TODO distinguish number parsing failures
//
// We're waiting for rust here, see https://github.com/rust-lang/rust/issues/22639
// There is a nightly API for exposing the parse error.

#[inline(always)]
pub fn num_expr_from_result(
    var_store: &mut VarStore,
    result: Result<i64, (&str, ParseIntError)>,
    region: Region,
    env: &mut Env,
) -> Expr {
    match result {
        Ok(int) => Expr::Num(var_store.fresh(), int),
        Err((raw, _error)) => {
            // (Num *) compiles to Int if it doesn't
            // get specialized to something else first,
            // so use int's overflow bounds here.
            let runtime_error = IntOutsideRange(raw.into(), region);

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            Expr::RuntimeError(runtime_error)
        }
    }
}

#[inline(always)]
pub fn int_expr_from_result(
    var_store: &mut VarStore,
    result: Result<i64, (&str, ParseIntError)>,
    region: Region,
    env: &mut Env,
) -> Expr {
    // Int stores a variable to generate better error messages
    match result {
        Ok(int) => Expr::Int(var_store.fresh(), int),
        Err((raw, _error)) => {
            let runtime_error = IntOutsideRange(raw.into(), region);

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            Expr::RuntimeError(runtime_error)
        }
    }
}

#[inline(always)]
pub fn float_expr_from_result(
    var_store: &mut VarStore,
    result: Result<f64, (&str, ParseFloatError)>,
    region: Region,
    env: &mut Env,
) -> Expr {
    // Float stores a variable to generate better error messages
    match result {
        Ok(float) => Expr::Float(var_store.fresh(), float),
        Err((raw, _error)) => {
            let runtime_error = FloatOutsideRange(raw.into(), region);

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            Expr::RuntimeError(runtime_error)
        }
    }
}

#[inline(always)]
pub fn finish_parsing_int(raw: &str) -> Result<i64, (&str, ParseIntError)> {
    // Ignore underscores.
    raw.replace("_", "").parse::<i64>().map_err(|e| (raw, e))
}

#[inline(always)]
pub fn finish_parsing_base(raw: &str, base: Base) -> Result<i64, (&str, ParseIntError)> {
    let radix = match base {
        Base::Hex => 16,
        Base::Octal => 8,
        Base::Binary => 2,
    };

    // Ignore underscores.
    i64::from_str_radix(raw.replace("_", "").as_str(), radix).map_err(|e| (raw, e))
}

#[inline(always)]
pub fn finish_parsing_float(raw: &str) -> Result<f64, (&str, ParseFloatError)> {
    // Ignore underscores.
    match raw.replace("_", "").parse::<f64>() {
        Ok(float) if float.is_finite() => Ok(float),
        Ok(_float) => panic!("TODO handle infinite float literal"),
        Err(e) => Err((raw, e)),
    }
}
