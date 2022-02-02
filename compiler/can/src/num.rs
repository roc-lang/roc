use crate::env::Env;
use crate::expr::Expr;
use roc_module::numeric::{FloatWidth, IntWidth, NumWidth, NumericBound};
use roc_parse::ast::Base;
use roc_problem::can::Problem;
use roc_problem::can::RuntimeError::*;
use roc_problem::can::{FloatErrorKind, IntErrorKind};
use roc_region::all::Region;
use roc_types::subs::VarStore;
use std::i64;
use std::str;

// TODO use rust's integer parsing again
//
// We're waiting for libcore here, see https://github.com/rust-lang/rust/issues/22639
// There is a nightly API for exposing the parse error.

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
    result: Result<(&str, i128, NumericBound<IntWidth>), (&str, IntErrorKind)>,
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
    Int(i64, NumericBound<IntWidth>),
    Float(f64, NumericBound<FloatWidth>),
    UnknownNum(i64),
}

#[inline(always)]
pub fn finish_parsing_num(raw: &str) -> Result<ParsedNumResult, (&str, IntErrorKind)> {
    // Ignore underscores.
    let radix = 10;
    let (num, bound) =
        from_str_radix::<i64>(raw.replace("_", "").as_str(), radix).map_err(|e| (raw, e.kind))?;
    // Let's try to specialize the number
    Ok(match bound {
        NumericBound::None => ParsedNumResult::UnknownNum(num),
        NumericBound::Exact(NumWidth::Int(iw)) => {
            ParsedNumResult::Int(num, NumericBound::Exact(iw))
        }
        NumericBound::Exact(NumWidth::Float(fw)) => {
            ParsedNumResult::Float(num as f64, NumericBound::Exact(fw))
        }
    })
}

#[inline(always)]
pub fn finish_parsing_base(
    raw: &str,
    base: Base,
    is_negative: bool,
) -> Result<(i64, NumericBound<IntWidth>), (&str, IntErrorKind)> {
    let radix = match base {
        Base::Hex => 16,
        Base::Decimal => 10,
        Base::Octal => 8,
        Base::Binary => 2,
    };

    // Ignore underscores, insert - when negative to get correct underflow/overflow behavior
    (if is_negative {
        from_str_radix::<i64>(format!("-{}", raw.replace("_", "")).as_str(), radix)
    } else {
        from_str_radix::<i64>(raw.replace("_", "").as_str(), radix)
    })
    .map_err(|e| e.kind)
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
    let (bound, raw_without_suffix) = parse_literal_suffix(raw.as_bytes());
    // Safety: `raw` is valid UTF8, and `parse_literal_suffix` will only chop UTF8
    // characters off the end, if it chops off anything at all.
    let raw_without_suffix = unsafe { str::from_utf8_unchecked(raw_without_suffix) };

    let bound = match bound {
        NumericBound::None => NumericBound::None,
        NumericBound::Exact(NumWidth::Float(fw)) => NumericBound::Exact(fw),
        NumericBound::Exact(NumWidth::Int(_)) => return Err((raw, FloatErrorKind::IntSuffix)),
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

fn parse_literal_suffix(num_str: &[u8]) -> (NumericBound<NumWidth>, &[u8]) {
    macro_rules! parse_num_suffix {
        ($($suffix:expr, $width:expr)*) => {$(
            if num_str.ends_with($suffix) {
                return (NumericBound::Exact($width), num_str.get(0..num_str.len() - $suffix.len()).unwrap());
            }
        )*}
    }

    parse_num_suffix! {
        b"u8",   NumWidth::Int(IntWidth::U8)
        b"u16",  NumWidth::Int(IntWidth::U16)
        b"u32",  NumWidth::Int(IntWidth::U32)
        b"u64",  NumWidth::Int(IntWidth::U64)
        b"u128", NumWidth::Int(IntWidth::U128)
        b"i8",   NumWidth::Int(IntWidth::I8)
        b"i16",  NumWidth::Int(IntWidth::I16)
        b"i32",  NumWidth::Int(IntWidth::I32)
        b"i64",  NumWidth::Int(IntWidth::I64)
        b"i128", NumWidth::Int(IntWidth::I128)
        b"nat",  NumWidth::Int(IntWidth::Nat)
        b"dec",  NumWidth::Float(FloatWidth::Dec)
        b"f32",  NumWidth::Float(FloatWidth::F32)
        b"f64",  NumWidth::Float(FloatWidth::F64)
    }

    (NumericBound::None, num_str)
}

/// Integer parsing code taken from the rust libcore,
/// pulled in so we can give custom error messages
///
/// The Rust Project is dual-licensed under either Apache 2.0 or MIT,
/// at the user's choice. License information can be found in
/// the LEGAL_DETAILS file in the root directory of this distribution.
///
/// Thanks to the Rust project and its contributors!
trait FromStrRadixHelper: PartialOrd + Copy {
    fn min_value() -> Self;
    fn max_value() -> Self;
    fn from_u32(u: u32) -> Self;
    fn checked_mul(&self, other: u32) -> Option<Self>;
    fn checked_sub(&self, other: u32) -> Option<Self>;
    fn checked_add(&self, other: u32) -> Option<Self>;
}

macro_rules! doit {
    ($($t:ty)*) => ($(impl FromStrRadixHelper for $t {
        #[inline]
        fn min_value() -> Self { Self::min_value() }
        #[inline]
        fn max_value() -> Self { Self::max_value() }
        #[inline]
        fn from_u32(u: u32) -> Self { u as Self }
        #[inline]
        fn checked_mul(&self, other: u32) -> Option<Self> {
            Self::checked_mul(*self, other as Self)
        }
        #[inline]
        fn checked_sub(&self, other: u32) -> Option<Self> {
            Self::checked_sub(*self, other as Self)
        }
        #[inline]
        fn checked_add(&self, other: u32) -> Option<Self> {
            Self::checked_add(*self, other as Self)
        }
    })*)
}
// We only need the i64 implementation, but libcore defines
// doit! { i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize }
doit! { i64 }

fn from_str_radix<T: FromStrRadixHelper>(
    src: &str,
    radix: u32,
) -> Result<(T, NumericBound<NumWidth>), ParseIntError> {
    use self::IntErrorKind::*;
    use self::ParseIntError as PIE;

    assert!(
        (2..=36).contains(&radix),
        "from_str_radix_int: must lie in the range `[2, 36]` - found {}",
        radix
    );

    if src.is_empty() {
        return Err(PIE { kind: Empty });
    }

    let is_signed_ty = T::from_u32(0) > T::min_value();

    // all valid digits are ascii, so we will just iterate over the utf8 bytes
    // and cast them to chars. .to_digit() will safely return None for anything
    // other than a valid ascii digit for the given radix, including the first-byte
    // of multi-byte sequences
    let src = src.as_bytes();

    let (is_positive, digits) = match src[0] {
        b'+' => (true, &src[1..]),
        b'-' if is_signed_ty => (false, &src[1..]),
        _ => (true, src),
    };

    let (bound, digits) = parse_literal_suffix(digits);

    if digits.is_empty() {
        return Err(PIE { kind: Empty });
    }

    let mut result = T::from_u32(0);
    if is_positive {
        // The number is positive
        for &c in digits {
            let x = match (c as char).to_digit(radix) {
                Some(x) => x,
                None => return Err(PIE { kind: InvalidDigit }),
            };
            result = match result.checked_mul(radix) {
                Some(result) => result,
                None => return Err(PIE { kind: Overflow }),
            };
            result = match result.checked_add(x) {
                Some(result) => result,
                None => return Err(PIE { kind: Overflow }),
            };
        }
    } else {
        // The number is negative
        for &c in digits {
            let x = match (c as char).to_digit(radix) {
                Some(x) => x,
                None => return Err(PIE { kind: InvalidDigit }),
            };
            result = match result.checked_mul(radix) {
                Some(result) => result,
                None => return Err(PIE { kind: Underflow }),
            };
            result = match result.checked_sub(x) {
                Some(result) => result,
                None => return Err(PIE { kind: Underflow }),
            };
        }
    }
    Ok((result, bound))
}

/// An error which can be returned when parsing an integer.
///
/// This error is used as the error type for the `from_str_radix()` functions
/// on the primitive integer types, such as [`i8::from_str_radix`].
///
/// # Potential causes
///
/// Among other causes, `ParseIntError` can be thrown because of leading or trailing whitespace
/// in the string e.g., when it is obtained from the standard input.
/// Using the [`str.trim()`] method ensures that no whitespace remains before parsing.
///
/// [`str.trim()`]: ../../std/primitive.str.html#method.trim
/// [`i8::from_str_radix`]: ../../std/primitive.i8.html#method.from_str_radix
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseIntError {
    kind: IntErrorKind,
}

impl ParseIntError {
    /// Outputs the detailed cause of parsing an integer failing.
    pub fn kind(&self) -> &IntErrorKind {
        &self.kind
    }
}
