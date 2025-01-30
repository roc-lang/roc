use crate::env::Env;
use crate::expr::{Expr, IntValue};
use roc_parse::ast::Base;
use roc_problem::can::Problem;
use roc_problem::can::RuntimeError::*;
use roc_problem::can::{FloatErrorKind, IntErrorKind};
use roc_region::all::Region;
pub use roc_types::num::{FloatBound, FloatWidth, IntBound, IntLitWidth, NumBound, SignDemand};
use roc_types::subs::VarStore;

use std::str;

#[inline(always)]
pub fn num_expr_from_result(
    var_store: &mut VarStore,
    result: Result<(&str, ParsedNumResult), (&str, IntErrorKind)>,
    region: Region,
    env: &mut Env,
) -> Expr {
    match result {
        Ok((str, ParsedNumResult::UnknownNum(num, bound))) => {
            Expr::Num(var_store.fresh(), (*str).into(), num, bound)
        }
        Ok((str, ParsedNumResult::Int(num, bound))) => Expr::Int(
            var_store.fresh(),
            var_store.fresh(),
            (*str).into(),
            num,
            bound,
        ),
        Ok((str, ParsedNumResult::Float(num, bound))) => Expr::Float(
            var_store.fresh(),
            var_store.fresh(),
            (*str).into(),
            num,
            bound,
        ),
        Err((raw, error)) => {
            // (Num *) compiles to Int if it doesn't
            // get specialized to something else first,
            // so use int's overflow bounds here.
            let runtime_error = InvalidInt(error, Base::Decimal, region, raw.into());

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            Expr::RuntimeError(runtime_error)
        }
    }
}

#[inline(always)]
pub fn int_expr_from_result(
    var_store: &mut VarStore,
    result: Result<(&str, IntValue, IntBound), (&str, IntErrorKind)>,
    region: Region,
    base: Base,
    env: &mut Env,
) -> Expr {
    // Int stores a variable to generate better error messages
    match result {
        Ok((str, int, bound)) => Expr::Int(
            var_store.fresh(),
            var_store.fresh(),
            (*str).into(),
            int,
            bound,
        ),
        Err((raw, error)) => {
            let runtime_error = InvalidInt(error, base, region, raw.into());

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            Expr::RuntimeError(runtime_error)
        }
    }
}

#[inline(always)]
pub fn float_expr_from_result(
    var_store: &mut VarStore,
    result: Result<(&str, f64, FloatBound), (&str, FloatErrorKind)>,
    region: Region,
    env: &mut Env,
) -> Expr {
    // Float stores a variable to generate better error messages
    match result {
        Ok((str, float, bound)) => Expr::Float(
            var_store.fresh(),
            var_store.fresh(),
            (*str).into(),
            float,
            bound,
        ),
        Err((raw, error)) => {
            let runtime_error = InvalidFloat(error, region, raw.into());

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            Expr::RuntimeError(runtime_error)
        }
    }
}

pub enum ParsedNumResult {
    Int(IntValue, IntBound),
    Float(f64, FloatBound),
    UnknownNum(IntValue, NumBound),
}

#[inline(always)]
pub fn finish_parsing_num(raw: &str) -> Result<(&str, ParsedNumResult), (&str, IntErrorKind)> {
    // Ignore underscores.
    let radix = 10;
    let (_, raw_without_suffix) = parse_literal_suffix(raw);
    match from_str_radix(raw.replace('_', "").as_str(), radix) {
        Ok(result) => Ok((raw_without_suffix, result)),
        Err(e) => Err((raw, e)),
    }
}

#[inline(always)]
pub fn finish_parsing_base(
    raw: &str,
    base: Base,
    is_negative: bool,
) -> Result<(IntValue, IntBound), (&str, IntErrorKind)> {
    let radix = match base {
        Base::Hex => 16,
        Base::Decimal => 10,
        Base::Octal => 8,
        Base::Binary => 2,
    };

    // Ignore underscores, insert - when negative to get correct underflow/overflow behavior
    (if is_negative {
        from_str_radix(format!("-{}", raw.replace('_', "")).as_str(), radix)
    } else {
        from_str_radix(raw.replace('_', "").as_str(), radix)
    })
    .and_then(|parsed| match parsed {
        ParsedNumResult::Float(..) => Err(IntErrorKind::FloatSuffix),
        ParsedNumResult::Int(val, bound) => Ok((val, bound)),
        ParsedNumResult::UnknownNum(val, NumBound::None) => Ok((val, IntBound::None)),
        ParsedNumResult::UnknownNum(val, NumBound::AtLeastIntOrFloat { sign, width }) => {
            Ok((val, IntBound::AtLeast { sign, width }))
        }
    })
    .map_err(|e| (raw, e))
}

#[inline(always)]
pub fn finish_parsing_float(raw: &str) -> Result<(&str, f64, FloatBound), (&str, FloatErrorKind)> {
    let (opt_bound, raw_without_suffix) = parse_literal_suffix(raw);

    let bound = match opt_bound {
        None => FloatBound::None,
        Some(ParsedWidth::Float(fw)) => FloatBound::Exact(fw),
        Some(ParsedWidth::Int(_)) => return Err((raw, FloatErrorKind::IntSuffix)),
    };

    // Ignore underscores.
    match raw_without_suffix.replace('_', "").parse::<f64>() {
        Ok(float) if float.is_finite() => Ok((raw_without_suffix, float, bound)),
        Ok(float) => {
            if float.is_sign_positive() {
                Err((raw, FloatErrorKind::PositiveInfinity))
            } else {
                Err((raw, FloatErrorKind::NegativeInfinity))
            }
        }
        Err(_) => Err((raw, FloatErrorKind::Error)),
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ParsedWidth {
    Int(IntLitWidth),
    Float(FloatWidth),
}

fn parse_literal_suffix(num_str: &str) -> (Option<ParsedWidth>, &str) {
    macro_rules! parse_num_suffix {
        ($($suffix:expr, $width:expr)*) => {$(
            if num_str.ends_with($suffix) {
                return (Some($width), num_str.get(0..num_str.len() - $suffix.len()).unwrap());
            }
        )*}
    }

    parse_num_suffix! {
        "u8",   ParsedWidth::Int(IntLitWidth::U8)
        "u16",  ParsedWidth::Int(IntLitWidth::U16)
        "u32",  ParsedWidth::Int(IntLitWidth::U32)
        "u64",  ParsedWidth::Int(IntLitWidth::U64)
        "u128", ParsedWidth::Int(IntLitWidth::U128)
        "i8",   ParsedWidth::Int(IntLitWidth::I8)
        "i16",  ParsedWidth::Int(IntLitWidth::I16)
        "i32",  ParsedWidth::Int(IntLitWidth::I32)
        "i64",  ParsedWidth::Int(IntLitWidth::I64)
        "i128", ParsedWidth::Int(IntLitWidth::I128)
        "dec",  ParsedWidth::Float(FloatWidth::Dec)
        "f32",  ParsedWidth::Float(FloatWidth::F32)
        "f64",  ParsedWidth::Float(FloatWidth::F64)
    }

    (None, num_str)
}

/// Integer parsing code taken from the rust libcore,
/// pulled in so we can give custom error messages
///
/// The Rust Project is dual-licensed under either Apache 2.0 or MIT,
/// at the user's choice. License information can be found in
/// the LEGAL_DETAILS file in the root directory of this distribution.
///
/// Thanks to the Rust project and its contributors!
fn from_str_radix(src: &str, radix: u32) -> Result<ParsedNumResult, IntErrorKind> {
    use self::IntErrorKind::*;

    assert!(
        (2..=36).contains(&radix),
        "from_str_radix_int: must lie in the range `[2, 36]` - found {radix}"
    );

    let (opt_exact_bound, src) = parse_literal_suffix(src);

    use std::num::IntErrorKind as StdIEK;
    let result = match i128::from_str_radix(src, radix) {
        Ok(result) => IntValue::I128(result.to_ne_bytes()),
        Err(pie) => match pie.kind() {
            StdIEK::Empty => return Err(IntErrorKind::Empty),
            StdIEK::InvalidDigit => return Err(IntErrorKind::InvalidDigit),
            StdIEK::NegOverflow => return Err(IntErrorKind::Underflow),
            StdIEK::PosOverflow => {
                // try a u128
                match u128::from_str_radix(src, radix) {
                    Ok(result) => IntValue::U128(result.to_ne_bytes()),
                    Err(pie) => match pie.kind() {
                        StdIEK::InvalidDigit => return Err(IntErrorKind::InvalidDigit),
                        StdIEK::PosOverflow => return Err(IntErrorKind::Overflow),
                        StdIEK::Empty | StdIEK::Zero | StdIEK::NegOverflow => unreachable!(),
                        _ => unreachable!("I thought all possibilities were exhausted, but std::num added a new one")
                    },
                }
            }
            StdIEK::Zero => unreachable!("Parsed a i128"),
            _ => unreachable!(
                "I thought all possibilities were exhausted, but std::num added a new one"
            ),
        },
    };

    let (lower_bound, is_negative) = match result {
        IntValue::I128(bytes) => {
            let num = i128::from_ne_bytes(bytes);

            (lower_bound_of_int_literal(num), num < 0)
        }
        IntValue::U128(_) => (IntLitWidth::U128, false),
    };

    match opt_exact_bound {
        None => {
            // There's no exact bound, but we do have a lower bound.
            let sign_demand = if is_negative {
                SignDemand::Signed
            } else {
                SignDemand::NoDemand
            };
            Ok(ParsedNumResult::UnknownNum(
                result,
                NumBound::AtLeastIntOrFloat {
                    sign: sign_demand,
                    width: lower_bound,
                },
            ))
        }
        Some(ParsedWidth::Float(fw)) => {
            // For now, assume floats can represent all integers
            // TODO: this is somewhat incorrect, revisit
            Ok(ParsedNumResult::Float(
                match result {
                    IntValue::I128(n) => i128::from_ne_bytes(n) as f64,
                    IntValue::U128(n) => i128::from_ne_bytes(n) as f64,
                },
                FloatBound::Exact(fw),
            ))
        }
        Some(ParsedWidth::Int(exact_width)) => {
            // We need to check if the exact bound >= lower bound.
            if exact_width.is_superset(&lower_bound, is_negative) {
                // Great! Use the exact bound.
                Ok(ParsedNumResult::Int(result, IntBound::Exact(exact_width)))
            } else {
                // This is something like 200i8; the lower bound is u8, which holds strictly more
                // ints on the positive side than i8 does. Report an error depending on which side
                // of the integers we checked.
                let err = if is_negative {
                    UnderflowsSuffix {
                        suffix_type: exact_width.type_str(),
                        min_value: exact_width.min_value(),
                    }
                } else {
                    OverflowsSuffix {
                        suffix_type: exact_width.type_str(),
                        max_value: exact_width.max_value(),
                    }
                };
                Err(err)
            }
        }
    }
}

fn lower_bound_of_int_literal(result: i128) -> IntLitWidth {
    use IntLitWidth::*;
    if result >= 0 {
        // Positive
        let result = result as u128;
        if result > U64.max_value() {
            I128
        } else if result > I64.max_value() {
            U64
        } else if result > F64.max_value() {
            I64
        } else if result > U32.max_value() {
            F64
        } else if result > I32.max_value() {
            U32
        } else if result > F32.max_value() {
            I32
        } else if result > U16.max_value() {
            F32
        } else if result > I16.max_value() {
            U16
        } else if result > U8.max_value() {
            I16
        } else if result > I8.max_value() {
            U8
        } else {
            I8
        }
    } else {
        // Negative
        if result < I64.min_value() {
            I128
        } else if result < F64.min_value() {
            I64
        } else if result < I32.min_value() {
            F64
        } else if result < F32.min_value() {
            I32
        } else if result < I16.min_value() {
            F32
        } else if result < I8.min_value() {
            I16
        } else {
            I8
        }
    }
}
