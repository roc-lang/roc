use std::cmp::{Eq, Ordering, PartialEq, PartialOrd};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Sub, Mul, Neg};

use std::mem;
use std::fmt;

use std::marker::PhantomData;

pub type Frac = Fraction<Valid>;

#[derive(Clone, Copy)]
pub struct Fraction<T> {
    numerator: i64,

    /// A positive denominator represents a valid, rational Fraction.
    /// A denominator of zero or lower represents an invalid Fraction. All operations
    /// on invalid Fractions have undefined behavior, so be careful not to perform
    /// operations on them if they get into that state!
    ///
    /// This design optimizes for runtime efficiency of Roc code, with the cost
    /// that it makes Frac more error-prone to use outside of Roc.
    ///
    /// * Roc will accept any pattern of bits in this struct, and will type them as
    ///   Result { ok: Frac, err: DivByZero }. Behind the scenes, a pattern match
    ///   on this result will map all positive denominators to Ok and all zero or negative
    ///   denominators to Err, so there is no extra memory cost to this being typed as Result.
    /// * Roc's reciprocal function and division operator return Result values as well. Because
    ///   of the previous point, these operations have no memory overhead either. If the denominator
    ///   ends up being zero after these operations, that will map to an Err value as normal.
    /// * Roc code does not have the expressive power to construct a Frac with a negative
    ///   denominator, so it is safe to assume within Roc code that this will never happen.
    ///
    /// Putting these together, it becomes efficient for Roc to assume any value of type Frac F a positive
    /// denominator, because it cannot be negative, and if it were zero it would have been wrapped in a Result.
    ///
    /// Outside Roc, it is possible to (for example) temporarily have a negative denominator and then later
    /// transition to a positive one. If you know about this and are planning for it, that's fine.
    /// However, it's important to remember if you pass a negative denominator to Roc, it will map to an Err value.
    ///
    /// The single source of truth for whether the fraction is positive or negative lies with the numerator.
    /// This is for two reasons:
    ///
    /// 1. It means the numerator is exactly an i64, so converting from Int -> Frac will never fail or lose precision.
    /// 2. It means checking for an error only needs to invovle the denominator. It's possible to ask "is the denominator positive?"
    ///    and immediately know if the fraction is valid or not.
    ///
    /// Denominator is stored as an i64 because it supports the same range of positive numbers as i64.
    /// This prevents the need for conversions between u64.
    denominator: i64,

    /// The phantom type records whether the Frac is valid.
    /// A Frac with a positive denominator is valid, and all others are invalid.
    phantom: PhantomData<T>
}

pub struct Valid;

/// Returns a new fraction.
///
/// Panics in non-release builds if given a denominator larger than the
/// largest possible i64.
///
/// Internally, Fraction stores the denominator as an i64 that must be positive.
/// The positivity guarantee allows Fraction to do comparisons faster, because
/// it can avoid accounting for the case where one fraction has both its
/// numerator and denominator negated, and the other has neither negated.
///
/// The denominator cannot be higher than std::i64::MAX because certain operations
/// (e.g. `reciprocal` and `div`) require swapping numerator and denominator,
/// and numerator is an i64.
///
/// A Fraction with a denominator of zero is invalid, and this function returns
/// a Fraction<U> because it is unknown whether the given denominator is zero.
/// To convert to a Fraction<Valid>, see `valid_or` and `valid_or_else`.
pub fn new<U>(numerator: i64, denominator: u64) -> Fraction<U> {
    assert!(denominator <= std::i64::MAX as u64);

    Fraction { numerator, denominator: denominator as i64, phantom: PhantomData }
}

/// Returns a new fraction. This is `unsafe` because it assumes, without checking,
/// that it was given a nonzero denominator. Never pass this a zero denominator!
///
/// Panics in non-release builds if given a denominator of zero, or if
/// the denominator is larger than the largest possible i64.
///
/// Internally, Fraction stores the denominator as an i64 that must be positive.
/// The positivity guarantee allows Fraction to do comparisons faster, because
/// it can avoid accounting for the case where one fraction has both its
/// numerator and denominator negated, and the other has neither negated.
///
/// The denominator cannot be higher than std::i64::MAX because certain operations
/// (e.g. `reciprocal` and `div`) require swapping numerator and denominator,
/// and numerator is an i64.
///
/// A Fraction with a denominator of zero is invalid, and this function returns
/// a Fraction<Valid>, so it is important that this function never return a Fraction<Valid>
/// with a zero denominator. That would lead to undefined behavior!
pub unsafe fn unchecked_new(numerator: i64, denominator: u64) -> Fraction<Valid> {
    assert_ne!(denominator, 0);
    assert!(denominator <= std::i64::MAX as u64);

    Fraction { numerator, denominator: denominator as i64, phantom: PhantomData }
}

impl<T> Fraction<T> {
    #[inline]
    /// Reduces the fraction in place.
    pub fn reduced(&self) -> Self {
        let common_divisor = gcd(self.numerator, self.denominator);
        let numerator = self.numerator / common_divisor;
        let denominator = self.denominator / common_divisor;

        Fraction { numerator, denominator, phantom: PhantomData }
    }

    pub fn is_rational(&self) -> bool {
        self.denominator.is_positive()
    }

    #[inline]
    /// Reduces the fraction, then returns true iff the denominator is 1.
    pub fn is_integer(&self) -> bool {
        let common_divisor = gcd(self.numerator, self.denominator);
        let denominator = self.denominator / common_divisor;

        denominator == 1
    }

    #[inline]
    /// Returns true iff the numerator is zero, without reducing first.
    /// This is more efficient than getting the reduced numerator and then
    /// comparing it to 0, because that has to execute the reduce operation.
    pub fn is_zero(self) -> bool {
        self.numerator == 0
    }

    pub fn abs(&self) -> Self {
        match self.numerator.overflowing_abs() {
            (numerator, false) =>
                Fraction { numerator, denominator: self.denominator, phantom: PhantomData },

            (_, true) => {
                // We underflowed, so reduce and try again.
                let reduced_self = self.reduced();

                Fraction {
                    numerator: reduced_self.numerator.abs(),
                    denominator: reduced_self.denominator,
                    phantom: PhantomData
                }
            }
        }
    }

    pub fn checked_abs(&self) -> Option<Self> {
        match self.numerator.overflowing_abs() {
            (numerator, false) =>
                Some(Fraction { numerator, denominator: self.denominator, phantom: PhantomData }),

            (_, true) => {
                // We underflowed, so reduce and try again.
                let reduced_self = self.reduced();

                match reduced_self.numerator.overflowing_abs() {
                    (numerator, false) => {
                        Some(Fraction { numerator, denominator: reduced_self.denominator, phantom: PhantomData })
                    },
                    (_, true) => None
                }
            }
        }
    }

    pub fn overflowing_abs(&self) -> (Self, bool) {
        match self.numerator.overflowing_abs() {
            (numerator, false) =>
                (Fraction { numerator, denominator: self.denominator, phantom: PhantomData }, false),

            (_, true) => {
                let reduced_self = self.reduced();

                let (numerator, underflowed) = reduced_self.numerator.overflowing_abs();

                (Fraction { numerator, denominator: reduced_self.denominator, phantom: PhantomData }, underflowed)
            }
        }
    }

    /// Add two fractions, returning None on overflow. Note: overflow can occur on more than just large numerators!
    pub fn checked_add(&self, other: &Self) -> Option<Self> {
        if self.denominator == other.denominator {
            // Happy path - we get to skip calculating a common denominator!
            match self.numerator.overflowing_add(other.numerator) {
                (numerator, false) => Some(Fraction { numerator, denominator: self.denominator, phantom: PhantomData }),
                (_, true) => self.reducing_checked_add(other)
            }
        } else {
            let common_denom: i64 = self.denominator * other.denominator;

            match (common_denom / self.denominator).overflowing_mul(self.numerator) {
                (self_numer, false) => {
                    match (common_denom / other.denominator).overflowing_mul(other.numerator) {
                        (other_numer, false) => {
                            match self_numer.overflowing_add(other_numer) {
                                (numerator, false) =>
                                    Some(Fraction { numerator, denominator: common_denom, phantom: PhantomData }),

                                (_, true) =>
                                    None
                            }
                        }
                        // Denominator overflowed - try reducing the inputs first.
                        (_, true) => self.reducing_checked_add(other)
                    }
                },
                // Numerator overflowed - try reducing the inputs first.
                (_, true) => self.reducing_checked_add(other)
            }
        }
    }

    /// Add while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_add(&self, other: &Self) -> Self {
        let reduced_self = self.reduced();
        let reduced_other = other.reduced();

        let denominator = lcm(reduced_self.denominator, reduced_other.denominator);
        let numerator =
            (reduced_self.numerator * (denominator / reduced_self.denominator))
                + (reduced_other.numerator * (denominator / reduced_other.denominator));

        Fraction { numerator, denominator, phantom: PhantomData }
    }

    /// Add while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_checked_add(&self, other: &Self) -> Option<Self> {
        let reduced_self = self.reduced();
        let reduced_other = other.reduced();

        let denominator = lcm(reduced_self.denominator, reduced_other.denominator);

        match (denominator / reduced_self.denominator).overflowing_mul(reduced_self.numerator) {
            (self_numer, false) => {
                match (denominator / reduced_other.denominator).overflowing_mul(reduced_other.numerator) {
                    (other_numer, false) => {
                        match self_numer.overflowing_add(other_numer) {
                            (numerator, false) =>
                                Some(Fraction { numerator, denominator, phantom: PhantomData }),

                            (_, true) =>
                                None
                        }
                    },
                    (_, true) => None
                }
            },
            (_, true) => None
        }
    }

    /// Subtract two fractions, returning None on overflow. Note: overflow can occur on more than just large numerators!
    pub fn checked_sub(&self, other: &Self) -> Option<Self> {
        if self.denominator == other.denominator {
            // Happy path - we get to skip calculating a common denominator!
            match self.numerator.overflowing_sub(other.numerator) {
                (numerator, false) => Some(Fraction { numerator, denominator: self.denominator, phantom: PhantomData }),
                (_, true) => self.reducing_checked_sub(other)
            }
        } else {
            let common_denom: i64 = self.denominator * other.denominator;

            match (common_denom / self.denominator).overflowing_mul(self.numerator) {
                (self_numer, false) => {
                    match (common_denom / other.denominator).overflowing_mul(other.numerator) {
                        (other_numer, false) => {
                            match self_numer.overflowing_sub(other_numer) {
                                (numerator, false) =>
                                    Some(Fraction { numerator, denominator: common_denom, phantom: PhantomData }),

                                (_, true) =>
                                    None
                            }
                        }
                        // Denominator overflowed - try reducing the inputs first.
                        (_, true) => self.reducing_checked_sub(other)
                    }
                },
                // Numerator overflowed - try reducing the inputs first.
                (_, true) => self.reducing_checked_sub(other)
            }
        }
    }

    /// Subtract while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_sub(&self, other: &Self) -> Self {
        let reduced_self = self.reduced();
        let reduced_other = other.reduced();

        let denominator = lcm(reduced_self.denominator, reduced_other.denominator);
        let numerator =
            (reduced_self.numerator * (denominator / reduced_self.denominator))
                + (reduced_other.numerator * (denominator / reduced_other.denominator));

        Fraction { numerator, denominator, phantom: PhantomData }
    }

    /// Subtract while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_checked_sub(&self, other: &Self) -> Option<Self> {
        let reduced_self = self.reduced();
        let reduced_other = other.reduced();

        let denominator = lcm(reduced_self.denominator, reduced_other.denominator);

        match (denominator / reduced_self.denominator).overflowing_mul(reduced_self.numerator) {
            (self_numer, false) => {
                match (denominator / reduced_other.denominator).overflowing_mul(reduced_other.numerator) {
                    (other_numer, false) => {
                        match self_numer.overflowing_sub(other_numer) {
                            (numerator, false) =>
                                Some(Fraction { numerator, denominator, phantom: PhantomData }),

                            (_, true) =>
                                None
                        }
                    },
                    (_, true) => None
                }
            },
            (_, true) => None
        }
    }

    /// Multiply two fractions, returning None on overflow. Note: overflow can occur on more than just large numerators!
    pub fn checked_mul(&self, other: &Self) -> Option<Self> {
        match self.numerator.checked_mul(other.numerator) {
            Some(numerator) => {
                // Common denominator is valuable. If we have it, try to preserve it!
                if self.denominator == other.denominator
                    // See if the denominator is evenly divisible by the new numerator.
                    // If it is, we can "pre-reduce" to the original denominator!
                    && numerator.checked_rem(self.denominator)
                        .map(|rem| rem == 0)
                        .unwrap_or(false)
                {
                    // TODO There's probably an optimization opportunity here. Check the
                    // generated instructions - there might be a way to use a division-with-remainder
                    // instruction, grab the remainder value out of the register, then
                    // do the test, and if it passes, grab the existing result of the division
                    // out of the other register without issuing a second division instruction.
                    Some(Fraction {
                        numerator: numerator / self.denominator,
                        denominator: self.denominator,
                        phantom: PhantomData
                    })
                } else {
                    match self.denominator.checked_mul(other.denominator) {
                        Some(denominator) => Some(Fraction { numerator, denominator, phantom: PhantomData }),

                        // Denominator overflowed. See if reducing the inputs helps!
                        None => self.reducing_checked_mul(other)
                    }
                }
            },

            // Numerator overflowed. See if reducing the inputs helps!
            None => self.reducing_checked_mul(other)
        }
    }

    /// Return the fracion with numerator and denominator swapped.
    ///
    /// Returns a Fraction with an unbound type variable because this can make it invalid;
    /// if the numerator was 0 before (which is valid), now the denominator will be 0 (which is invalid).
    pub fn reciprocal<V>(&self) -> Fraction<V> {
        let denominator = self.numerator;
        let numerator = self.denominator;

        // Make sure we don't end up with a negative denominator!
        if denominator.is_negative() {
            Fraction { numerator: -numerator, denominator: -denominator, phantom: PhantomData }
        } else {
            Fraction { numerator, denominator, phantom: PhantomData }
        }
    }

    pub fn checked_div<U, V>(&self, other: &Self) -> Option<Fraction<V>> {
        // We're going to multiply by the reciprocal of `other`, so if its numerator
        // was 0, then the resulting fraction will have 0 for a denominator, so we're done.
        if other.numerator == 0 {
            return None;
        }

        match self.numerator.overflowing_mul(other.denominator) {
            (numerator, false) => {
                match self.denominator.overflowing_mul(other.numerator) {
                    (denominator, false) => {
                        // Make sure we don't end up with a negative denominator!
                        if denominator.is_negative() {
                            Some(Fraction { numerator: -numerator, denominator: -denominator, phantom: PhantomData })
                        } else {
                            Some(Fraction { numerator, denominator, phantom: PhantomData })
                        }
                    },
                    // Denominator overflowed. See if reducing the inputs helps!
                    (_, true) => self.reducing_checked_div(other)
                }
            },

            // Numerator overflowed. See if reducing the inputs helps!
            (_, true) => self.reducing_checked_div(other)
        }
    }

    /// Returns whether the fraction is valid.
    /// For a Frac<Valid>, this will always return true.
    #[inline(always)]
    pub fn is_valid(&self) -> bool {
        self.denominator.is_positive()
    }

    /// If the fraction is valid, return it wrapped in Some.
    /// Otherwise, return None.
    pub fn into_valid<V>(self) -> Option<Fraction<Valid>> {
        if self.is_valid() {
            Some(Fraction {numerator: self.numerator, denominator: self.denominator, phantom: PhantomData})
        } else {
            None
        }
    }

    /// If the fraction is valid, return it with the type variable set accordingly.
    /// Otherwise, return the fallback value.
    pub fn valid_or<V>(self, fallback: V) -> Result<Fraction<Valid>, V> {
        if self.is_valid() {
            Ok(Fraction {numerator: self.numerator, denominator: self.denominator, phantom: PhantomData})
        } else {
            Err(fallback)
        }
    }

    pub fn valid_or_else<F, V>(self, fallback_fn: F) -> Result<Fraction<Valid>, V>
    where F: Fn() -> V
    {
        if self.is_valid() {
            Ok(Fraction {numerator: self.numerator, denominator: self.denominator, phantom: PhantomData})
        } else {
            Err(fallback_fn())
        }
    }

    /// Divide two fractions.
    ///
    /// This returns a Frac with an unbound type parameter because the result may not be valid.
    pub fn div<U, V>(&self, other: &Fraction<U>) -> Fraction<V> {
        match self.numerator.checked_mul(other.denominator) {
            Some(numerator) => {
                match self.denominator.checked_mul(other.numerator) {
                    Some(denominator) => {
                        // Make sure we don't end up with a negative denominator!
                        if denominator.is_negative() {
                            Fraction { numerator: -numerator, denominator: -denominator, phantom: PhantomData }
                        } else {
                            Fraction { numerator, denominator, phantom: PhantomData }
                        }
                    },
                    // Denominator overflowed. See if reducing the inputs helps!
                    None => self.reducing_div(other)
                }
            },

            // Numerator overflowed. See if reducing the inputs helps!
            None => self.reducing_div(other)
        }
    }

    /// Multiply while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_mul(&self, other: &Self) -> Self {
        let reduced_self = self.reduced();
        let reduced_other = other.reduced();

        // Preserving common denominator is out the window at this point.
        let numerator = reduced_self.numerator * reduced_other.numerator;
        let denominator = reduced_self.denominator * reduced_other.denominator;

        Fraction { numerator, denominator, phantom: PhantomData }
    }

    /// Multiply while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_checked_mul(&self, other: &Self) -> Option<Self> {
        let reduced_self = self.reduced();
        let reduced_other = other.reduced();

        // Preserving common denominator is out the window at this point.
        match reduced_self.numerator.overflowing_mul(reduced_other.numerator) {
            (numerator, false) => {
                match reduced_self.denominator.overflowing_mul(reduced_other.denominator) {
                    (denominator, false) =>
                        Some(Fraction { numerator, denominator, phantom: PhantomData }),

                    (_, true) =>
                        None
                }
            },
            (_, true) => None
        }
    }

    /// Divide while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_checked_div<U, V>(&self, other: &Fraction<U>) -> Option<Fraction<V>> {
        let reduced_self = self.reduced();
        let reduced_other = other.reduced();

        // Preserving common denominator is out the window at this point.
        match reduced_self.numerator.overflowing_mul(reduced_other.numerator) {
            (denominator, false) => {
                match reduced_self.denominator.overflowing_mul(reduced_other.denominator) {
                    (numerator, false) =>
                        Some(Fraction { numerator, denominator, phantom: PhantomData }),

                    (_, true) =>
                        None
                }
            },
            (_, true) => None
        }
    }

    /// Divide while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_div<U, V>(&self, other: &Fraction<U>) -> Fraction<V> {
        let reduced_self = self.reduced();
        let reduced_other = other.reduced();

        let numerator = reduced_self.denominator * reduced_other.denominator;
        let denominator = reduced_self.numerator * reduced_other.numerator;

        Fraction { numerator, denominator, phantom: PhantomData }
    }

    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_eq(&self, other: &Self) -> bool {
        let reduced_self = self.reduced();
        let reduced_other = other.reduced();

        let denominator = lcm(reduced_self.denominator, reduced_other.denominator);
        let self_numerator = reduced_self.numerator * (denominator / reduced_self.denominator);
        let other_numerator = reduced_other.numerator * (denominator / reduced_other.denominator);

        self_numerator == other_numerator
    }

    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_cmp(&self, other: &Self) -> Ordering {
        let reduced_self = self.reduced();
        let reduced_other = other.reduced();

        let denominator = lcm(reduced_self.denominator, reduced_other.denominator);
        let self_numerator = reduced_self.numerator * (denominator / reduced_self.denominator);
        let other_numerator = reduced_other.numerator * (denominator / reduced_other.denominator);

        self_numerator.cmp(&other_numerator)
    }

    #[inline(always)]
    pub fn numerator(&self) -> i64 {
        self.numerator
    }

    #[inline(always)]
    pub fn denominator(&self) -> i64 {
        self.denominator
    }
}

impl<T> fmt::Debug for Fraction<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

impl<T> PartialEq for Fraction<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.denominator == other.denominator {
            self.numerator.eq(&other.numerator)
        } else if self.numerator == 0 {
            // If numerator is 0, the whole fraction is 0.
            other.numerator == 0
        } else if other.numerator == 0 {
            // We couldn't have reached this branch if self.numerator == 0
            false
        } else {
            match self.denominator.overflowing_mul(other.denominator) {
                (common_denom, false) => {
                    match (common_denom / self.denominator).overflowing_mul(self.numerator) {
                        (self_numer, false) => {
                            match (common_denom / other.denominator).overflowing_mul(other.numerator) {
                                (other_numer, false) => self_numer.eq(&other_numer),
                                // other.numerator overflowed - try reducing the inputs first.
                                (_, true) => self.reducing_eq(other)
                            }
                        },
                        // self.numerator overflowed - try reducing the inputs first.
                        (_, true) => self.reducing_eq(other)
                    }
                }
                // Common denominator overflowed - try reducing the inputs first.
                (_, true) => self.reducing_eq(other)
            }
        }
    }
}

impl<T> Eq for Fraction<T> {}

/// We only have Ord for valid Fracs because potentially invalid ones are essentially equivalent
/// to Result<Frac, ()>. Defining Ord for that case too would mean all Ord implementations would
/// have to do extra checking for situations where either denominator is 0, which does not
/// seem worth the cost.
impl PartialOrd for Fraction<Valid> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Fraction<Valid> {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.denominator == other.denominator {
            self.numerator.cmp(&other.numerator)
        } else if self.numerator == 0 {
            // If numerator is 0, the whole fraction is 0.
            // Just compare numerators to see if the other one is 0, positive, or negative.
            0.cmp(&other.numerator)
        } else if other.numerator == 0 {
            self.numerator.cmp(&0)
        } else {
            match self.denominator.overflowing_mul(other.denominator) {
                (common_denom, false) => {
                    match (common_denom / self.denominator).overflowing_mul(self.numerator) {
                        (self_numer, false) => {
                            match (common_denom / other.denominator).overflowing_mul(other.numerator) {
                                (other_numer, false) => self_numer.cmp(&other_numer),
                                // other.numerator overflowed - try reducing the inputs first.
                                (_, true) => self.reducing_cmp(other)
                            }
                        },
                        // self.numerator overflowed - try reducing the inputs first.
                        (_, true) => self.reducing_cmp(other)
                    }
                }
                // Common denominator overflowed - try reducing the inputs first.
                (_, true) => self.reducing_cmp(other)
            }
        }
    }
}

impl Add for Fraction<Valid> {
    type Output = Fraction<Valid>;

    /// Add two fractions.
    fn add(self, other: Self) -> Self {
        if self.denominator == other.denominator {
            // Happy path - we get to skip calculating a common denominator!
            match self.numerator.overflowing_add(other.numerator) {
                (numerator, false) => Fraction { numerator, denominator: self.denominator, phantom: PhantomData },
                (_, true) => self.reducing_add(&other)
            }
        } else {
            let common_denom: i64 = self.denominator * other.denominator;

            // This code would look nicer with checked_ instead of overflowing_, but
            // seems likely the perf would be worse.
            match (common_denom / self.denominator).overflowing_mul(self.numerator) {
                (self_numer, false) => {
                    match (common_denom / other.denominator).overflowing_mul(other.numerator) {
                        (other_numer, false) => {
                            match self_numer.overflowing_add(other_numer) {
                                (numerator, false) => Fraction { numerator, denominator: common_denom, phantom: PhantomData },
                                (_, true) => self.reducing_add(&other)
                            }
                        },
                        (_, true) => self.reducing_add(&other)
                    }
                },
                (_, true) => self.reducing_add(&other)
            }
        }
    }
}

impl Mul for Fraction<Valid> {
    type Output = Fraction<Valid>;

    /// Multiply two fractions.
    fn mul(self, other: Self) -> Self {
        match self.numerator.overflowing_mul(other.numerator) {
            (numerator, false) => {
                // Common denominator is valuable. If we have it, try to preserve it!
                if self.denominator == other.denominator
                    // See if the denominator is evenly divisible by the new numerator.
                    // If it is, we can "pre-reduce" to the original denominator!
                    && (numerator.overflowing_rem(self.denominator) == (0, false))
                {
                    // TODO There's probably an optimization opportunity here. Check the
                    // generated instructions - there might be a way to use a division-with-remainder
                    // instruction, grab the remainder value out of the register, then
                    // do the test, and if it passes, grab the existing result of the division
                    // out of the other register without issuing a second division instruction.
                    Fraction {
                        numerator: numerator / self.denominator,
                        denominator: self.denominator,
                        phantom: PhantomData
                    }
                } else {
                    match self.denominator.overflowing_mul(other.denominator) {
                        (denominator, false) => Fraction { numerator, denominator, phantom: PhantomData },

                        // Denominator overflowed. See if reducing the inputs helps!
                        (_, true) => self.reducing_mul(&other)
                    }
                }
            },

            // Numerator overflowed. See if reducing the inputs helps!
            (_, true) => self.reducing_mul(&other)
        }
    }
}

impl Sub for Fraction<Valid> {
    type Output = Fraction<Valid>;

    /// Subtract two fractions.
    fn sub(self, other: Self) -> Self {
        if self.denominator == other.denominator {
            // Happy path - we get to skip calculating a common denominator!
            match self.numerator.overflowing_sub(other.numerator) {
                (numerator, false) => Fraction { numerator, denominator: self.denominator, phantom: PhantomData },
                (_, true) => self.reducing_sub(&other)
            }
        } else {
            let common_denom: i64 = self.denominator * other.denominator;

            // This code would look nicer with checked_ instead of overflowing_, but
            // seems likely the perf would be worse.
            match (common_denom / self.denominator).overflowing_mul(self.numerator) {
                (self_numer, false) => {
                    match (common_denom / other.denominator).overflowing_mul(other.numerator) {
                        (other_numer, false) => {
                            match self_numer.overflowing_sub(other_numer) {
                                (numerator, false) => Fraction { numerator, denominator: common_denom, phantom: PhantomData },
                                (_, true) => self.reducing_sub(&other)
                            }
                        },
                        (_, true) => self.reducing_sub(&other)
                    }
                },
                (_, true) => self.reducing_sub(&other)
            }
        }
    }
}

impl Hash for Fraction<Valid> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let reduced_self = self.reduced();

        reduced_self.numerator.hash(state);
        reduced_self.denominator.hash(state);
    }
}

impl<T> Neg for Fraction<T> {
    type Output = Fraction<T>;

    fn neg(self) -> Self {
        Fraction { numerator: -self.numerator, denominator: self.denominator, phantom: PhantomData }
    }
}

impl<T> From<u8> for Fraction<T> {
    fn from (numerator: u8) -> Fraction<T> {
        Fraction { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<i8> for Fraction<T> {
    fn from (numerator: i8) -> Fraction<T> {
        Fraction { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<u16> for Fraction<T> {
    fn from (numerator: u16) -> Fraction<T> {
        Fraction { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<i16> for Fraction<T> {
    fn from (numerator: i16) -> Fraction<T> {
        Fraction { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<u32> for Fraction<T> {
    fn from (numerator: u32) -> Fraction<T> {
        Fraction { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<i32> for Fraction<T> {
    fn from (numerator: i32) -> Fraction<T> {
        Fraction { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<i64> for Fraction<T> {
    fn from (numerator: i64) -> Fraction<T> {
        Fraction { numerator, denominator: 1, phantom: PhantomData }
    }
}

/// This function was adapted from v0.1.39 of the num-integer crate. Licensed under the
/// Apache License, version 2.0. A full copy of the License can be found here:
/// http://www.apache.org/licenses/LICENSE-2.0
///
/// The original code can be found at:
/// https://docs.rs/num-integer/0.1.41/src/num_integer/lib.rs.html#456-500
///
///
/// Calculates the Greatest Common Divisor (GCD) of the number and
/// `other`. The result is always positive.
#[inline]
fn gcd(me: i64, other: i64) -> i64 {
    // Use Stein's algorithm
    let mut m = me;
    let mut n = other;
    if m == 0 || n == 0 { return (m | n).abs() }

    // find common factors of 2
    let shift = (m | n).trailing_zeros();

    // The algorithm needs positive numbers, but the minimum value
    // can't be represented as a positive one.
    // It's also a power of two, so the gcd can be
    // calculated by bitshifting in that case

    // Assuming two's complement, the number created by the shift
    // is positive for all numbers except gcd = abs(min value)
    // The call to .abs() causes a panic in debug mode
    if m == i64::min_value() || n == i64::min_value() {
        return ((1 << shift) as i64).abs()
    }
    // guaranteed to be positive now, rest like unsigned algorithm
    m = m.abs();
    n = n.abs();

    // divide n and m by 2 until odd
    // m inside loop
    n >>= n.trailing_zeros();

    while m != 0 {
        m >>= m.trailing_zeros();
        if n > m { mem::swap(&mut n, &mut m) }
        m -= n;
    }

    n << shift
}

/// Lowest common multiple
fn lcm(me: i64, other: i64) -> i64 {
    me * (other / gcd(me, other))
}

#[cfg(test)]
mod test_fast_fraction {
    use super::Frac;

    pub fn frac(numerator: i64, denominator: u64) -> Frac {
        super::new(numerator, denominator)
    }

    #[test]
    fn one_plus_one() {
        assert_eq!(
            frac(1, 1) + frac(1, 1),
            frac(2, 1)
        );
    }

    #[test]
    fn point_one_plus_point_two() {
        assert_eq!(
            frac(1, 10) + frac(2, 10),
            frac(3, 10)
        );
    }

    #[test]
    fn one_minus_one() {
        assert_eq!(
            frac(1, 1) - frac(1, 1),
            frac(0, 9999)
        );
    }

    #[test]
    fn multiply() {
        assert_eq!(
            frac(2, 3) * frac(5, 7),
            frac(10, 21)
        );
    }

    #[test]
    fn divide() {
        assert_eq!(
            frac(2, 3).div(&frac(5, 7)),
            frac(14, 15)
        );
    }
}
