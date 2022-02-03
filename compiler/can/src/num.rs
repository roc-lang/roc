use crate::env::Env;
use crate::expr::{Expr, IntValue};
use roc_parse::ast::Base;
use roc_problem::can::Problem;
use roc_problem::can::RuntimeError::*;
use roc_problem::can::{FloatErrorKind, IntErrorKind};
use roc_region::all::Region;
use roc_types::subs::VarStore;
use std::i64;
use std::str;

#[inline(always)]
pub fn num_expr_from_result(
    var_store: &mut VarStore,
    result: Result<(&str, ParsedNumResult), (&str, IntErrorKind)>,
    region: Region,
    env: &mut Env,
) -> Expr {
    match result {
        Ok((str, ParsedNumResult::UnknownNum(num))) => {
            Expr::Num(var_store.fresh(), (*str).into(), num, NumericBound::None)
        }
        Ok((str, ParsedNumResult::Int(num, bound))) => Expr::Int(
            var_store.fresh(),
            var_store.fresh(),
            (*str).into(),
            num.into(),
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
    result: Result<(&str, IntValue, NumericBound<IntWidth>), (&str, IntErrorKind)>,
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
    result: Result<(&str, f64, NumericBound<FloatWidth>), (&str, FloatErrorKind)>,
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
    Int(IntValue, NumericBound<IntWidth>),
    Float(f64, NumericBound<FloatWidth>),
    UnknownNum(IntValue),
}

#[inline(always)]
pub fn finish_parsing_num(raw: &str) -> Result<ParsedNumResult, (&str, IntErrorKind)> {
    // Ignore underscores.
    let radix = 10;
    let (num, bound) =
        from_str_radix(raw.replace("_", "").as_str(), radix).map_err(|e| (raw, e))?;
    // Let's try to specialize the number
    Ok(match bound {
        NumericBound::None => ParsedNumResult::UnknownNum(num),
        NumericBound::Exact(NumWidth::Int(iw)) => {
            ParsedNumResult::Int(num, NumericBound::Exact(iw))
        }
        NumericBound::Exact(NumWidth::Float(fw)) => {
            let num = match num {
                IntValue::I128(n) => n as f64,
                IntValue::U128(n) => n as f64,
            };
            ParsedNumResult::Float(num, NumericBound::Exact(fw))
        }
    })
}

#[inline(always)]
pub fn finish_parsing_base(
    raw: &str,
    base: Base,
    is_negative: bool,
) -> Result<(IntValue, NumericBound<IntWidth>), (&str, IntErrorKind)> {
    let radix = match base {
        Base::Hex => 16,
        Base::Decimal => 10,
        Base::Octal => 8,
        Base::Binary => 2,
    };

    // Ignore underscores, insert - when negative to get correct underflow/overflow behavior
    (if is_negative {
        from_str_radix(format!("-{}", raw.replace("_", "")).as_str(), radix)
    } else {
        from_str_radix(raw.replace("_", "").as_str(), radix)
    })
    .and_then(|(n, bound)| {
        let bound = match bound {
            NumericBound::None => NumericBound::None,
            NumericBound::Exact(NumWidth::Int(iw)) => NumericBound::Exact(iw),
            NumericBound::Exact(NumWidth::Float(_)) => return Err(IntErrorKind::FloatSuffix),
        };
        Ok((n, bound))
    })
    .map_err(|e| (raw, e))
}

#[inline(always)]
pub fn finish_parsing_float(
    raw: &str,
) -> Result<(f64, NumericBound<FloatWidth>), (&str, FloatErrorKind)> {
    let (opt_bound, raw_without_suffix) = parse_literal_suffix(raw);

    let bound = match opt_bound {
        None => NumericBound::None,
        Some(NumWidth::Float(fw)) => NumericBound::Exact(fw),
        Some(NumWidth::Int(_)) => return Err((raw, FloatErrorKind::IntSuffix)),
    };

    // Ignore underscores.
    match raw_without_suffix.replace("_", "").parse::<f64>() {
        Ok(float) if float.is_finite() => Ok((float, bound)),
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

fn parse_literal_suffix(num_str: &str) -> (Option<NumWidth>, &str) {
    macro_rules! parse_num_suffix {
        ($($suffix:expr, $width:expr)*) => {$(
            if num_str.ends_with($suffix) {
                return (Some($width), num_str.get(0..num_str.len() - $suffix.len()).unwrap());
            }
        )*}
    }

    parse_num_suffix! {
        "u8",   NumWidth::Int(IntWidth::U8)
        "u16",  NumWidth::Int(IntWidth::U16)
        "u32",  NumWidth::Int(IntWidth::U32)
        "u64",  NumWidth::Int(IntWidth::U64)
        "u128", NumWidth::Int(IntWidth::U128)
        "i8",   NumWidth::Int(IntWidth::I8)
        "i16",  NumWidth::Int(IntWidth::I16)
        "i32",  NumWidth::Int(IntWidth::I32)
        "i64",  NumWidth::Int(IntWidth::I64)
        "i128", NumWidth::Int(IntWidth::I128)
        "nat",  NumWidth::Int(IntWidth::Nat)
        "dec",  NumWidth::Float(FloatWidth::Dec)
        "f32",  NumWidth::Float(FloatWidth::F32)
        "f64",  NumWidth::Float(FloatWidth::F64)
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
fn from_str_radix(
    src: &str,
    radix: u32,
) -> Result<(IntValue, NumericBound<NumWidth>), IntErrorKind> {
    use self::IntErrorKind::*;

    assert!(
        (2..=36).contains(&radix),
        "from_str_radix_int: must lie in the range `[2, 36]` - found {}",
        radix
    );

    let (opt_exact_bound, src) = parse_literal_suffix(src);

    use std::num::IntErrorKind as StdIEK;
    let result = match i128::from_str_radix(src, radix) {
        Ok(result) => IntValue::I128(result),
        Err(pie) => match pie.kind() {
            StdIEK::Empty => return Err(IntErrorKind::Empty),
            StdIEK::InvalidDigit => return Err(IntErrorKind::InvalidDigit),
            StdIEK::NegOverflow => return Err(IntErrorKind::Underflow),
            StdIEK::PosOverflow => {
                // try a u128
                match u128::from_str_radix(src, radix) {
                    Ok(result) => IntValue::U128(result),
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
        IntValue::I128(num) => (lower_bound_of_int(num), num <= 0),
        IntValue::U128(_) => (IntWidth::U128, false),
    };

    match opt_exact_bound {
        None => {
            // TODO: use the lower bound
            Ok((result, NumericBound::None))
        }
        Some(bound @ NumWidth::Float(_)) => {
            // For now, assume floats can represent all integers
            // TODO: this is somewhat incorrect, revisit
            Ok((result, NumericBound::Exact(bound)))
        }
        Some(NumWidth::Int(exact_width)) => {
            // We need to check if the exact bound >= lower bound.
            if exact_width.is_superset(&lower_bound, is_negative) {
                // Great! Use the exact bound.
                Ok((result, NumericBound::Exact(NumWidth::Int(exact_width))))
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

fn lower_bound_of_int(result: i128) -> IntWidth {
    use IntWidth::*;
    if result >= 0 {
        let result = result as u128;
        if result > U64.max_value() {
            I128
        } else if result > I64.max_value() {
            U64
        } else if result > U32.max_value() {
            I64
        } else if result > I32.max_value() {
            U32
        } else if result > U16.max_value() {
            I32
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
        if result < I64.min_value() {
            I128
        } else if result < I32.min_value() {
            I64
        } else if result < I16.min_value() {
            I32
        } else if result < I8.min_value() {
            I16
        } else {
            I8
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum IntSign {
    Unsigned,
    Signed,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntWidth {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    Nat,
}

impl IntWidth {
    /// Returns the `IntSign` and bit width of a variant.
    fn sign_and_width(&self) -> (IntSign, u32) {
        use IntSign::*;
        use IntWidth::*;
        match self {
            U8 => (Unsigned, 8),
            U16 => (Unsigned, 16),
            U32 => (Unsigned, 32),
            U64 => (Unsigned, 64),
            U128 => (Unsigned, 128),
            I8 => (Signed, 8),
            I16 => (Signed, 16),
            I32 => (Signed, 32),
            I64 => (Signed, 64),
            I128 => (Signed, 128),
            // TODO: this is platform specific!
            Nat => (Unsigned, 64),
        }
    }

    fn type_str(&self) -> &'static str {
        use IntWidth::*;
        match self {
            U8 => "U8",
            U16 => "U16",
            U32 => "U32",
            U64 => "U64",
            U128 => "U128",
            I8 => "I8",
            I16 => "I16",
            I32 => "I32",
            I64 => "I64",
            I128 => "I128",
            Nat => "Nat",
        }
    }

    fn max_value(&self) -> u128 {
        use IntWidth::*;
        match self {
            U8 => u8::MAX as u128,
            U16 => u16::MAX as u128,
            U32 => u32::MAX as u128,
            U64 => u64::MAX as u128,
            U128 => u128::MAX,
            I8 => i8::MAX as u128,
            I16 => i16::MAX as u128,
            I32 => i32::MAX as u128,
            I64 => i64::MAX as u128,
            I128 => i128::MAX as u128,
            // TODO: this is platform specific!
            Nat => u64::MAX as u128,
        }
    }

    fn min_value(&self) -> i128 {
        use IntWidth::*;
        match self {
            U8 | U16 | U32 | U64 | U128 | Nat => 0,
            I8 => i8::MIN as i128,
            I16 => i16::MIN as i128,
            I32 => i32::MIN as i128,
            I64 => i64::MIN as i128,
            I128 => i128::MIN,
        }
    }

    /// Checks if `self` represents superset of integers that `lower_bound` represents, on a particular
    /// side of the integers relative to 0.
    ///
    /// If `is_negative` is true, the negative side is checked; otherwise the positive side is checked.
    pub fn is_superset(&self, lower_bound: &Self, is_negative: bool) -> bool {
        use IntSign::*;

        if is_negative {
            match (self.sign_and_width(), lower_bound.sign_and_width()) {
                ((Signed, us), (Signed, lower_bound)) => us >= lower_bound,
                // Unsigned ints can never represent negative numbers; signed (non-zero width)
                // ints always can.
                ((Unsigned, _), (Signed, _)) => false,
                ((Signed, _), (Unsigned, _)) => true,
                // Trivially true; both can only express 0.
                ((Unsigned, _), (Unsigned, _)) => true,
            }
        } else {
            match (self.sign_and_width(), lower_bound.sign_and_width()) {
                ((Signed, us), (Signed, lower_bound))
                | ((Unsigned, us), (Unsigned, lower_bound)) => us >= lower_bound,

                // Unsigned ints with the same bit width as their unsigned counterparts can always
                // express 2x more integers on the positive side as unsigned ints.
                ((Unsigned, us), (Signed, lower_bound)) => us >= lower_bound,

                // ...but that means signed int widths can represent less than their unsigned
                // counterparts, so the below is true iff the bit width is strictly greater. E.g.
                // i16 is a superset of u8, but i16 is not a superset of u16.
                ((Signed, us), (Unsigned, lower_bound)) => us > lower_bound,
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FloatWidth {
    Dec,
    F32,
    F64,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NumWidth {
    Int(IntWidth),
    Float(FloatWidth),
}

/// Describes a bound on the width of a numeric literal.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NumericBound<W>
where
    W: Copy,
{
    /// There is no bound on the width.
    None,
    /// Must have exactly the width `W`.
    Exact(W),
}
