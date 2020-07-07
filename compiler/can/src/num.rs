use crate::env::Env;
use crate::expr::Expr;
use roc_parse::ast::Base;
use roc_problem::can::Problem;
use roc_problem::can::RuntimeError::*;
use roc_problem::can::{FloatErrorKind, IntErrorKind};
use roc_region::all::Region;
use roc_types::subs::VarStore;
use std::i64;

// TODO distinguish number parsing failures
//
// We're waiting for rust here, see https://github.com/rust-lang/rust/issues/22639
// There is a nightly API for exposing the parse error.

#[inline(always)]
pub fn num_expr_from_result(
    var_store: &mut VarStore,
    result: Result<i64, (&str, IntErrorKind)>,
    region: Region,
    env: &mut Env,
) -> Expr {
    match result {
        Ok(int) => Expr::Num(var_store.fresh(), int),
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
    result: Result<i64, (&str, IntErrorKind)>,
    region: Region,
    base: Base,
    env: &mut Env,
) -> Expr {
    // Int stores a variable to generate better error messages
    match result {
        Ok(int) => Expr::Int(var_store.fresh(), int),
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
    result: Result<f64, (&str, FloatErrorKind)>,
    region: Region,
    env: &mut Env,
) -> Expr {
    // Float stores a variable to generate better error messages
    match result {
        Ok(float) => Expr::Float(var_store.fresh(), float),
        Err((raw, error)) => {
            let runtime_error = InvalidFloat(error, region, raw.into());

            env.problem(Problem::RuntimeError(runtime_error.clone()));

            Expr::RuntimeError(runtime_error)
        }
    }
}

#[inline(always)]
pub fn finish_parsing_int(raw: &str) -> Result<i64, (&str, IntErrorKind)> {
    // Ignore underscores.
    let radix = 10;
    from_str_radix::<i64>(raw.replace("_", "").as_str(), radix).map_err(|e| (raw, e.kind))
}

#[inline(always)]
pub fn finish_parsing_base(
    raw: &str,
    base: Base,
    is_negative: bool,
) -> Result<i64, (&str, IntErrorKind)> {
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
    .map_err(|e| (raw, e.kind))
}

#[inline(always)]
pub fn finish_parsing_float(raw: &str) -> Result<f64, (&str, FloatErrorKind)> {
    // Ignore underscores.
    match raw.replace("_", "").parse::<f64>() {
        Ok(float) if float.is_finite() => Ok(float),
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

/// Integer parsing code taken from the rust stdlib,
/// pulled in so we can give custom error messages

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

fn from_str_radix<T: FromStrRadixHelper>(src: &str, radix: u32) -> Result<T, ParseIntError> {
    use self::IntErrorKind::*;
    use self::ParseIntError as PIE;

    assert!(
        radix >= 2 && radix <= 36,
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
    Ok(result)
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
