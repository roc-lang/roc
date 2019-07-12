use std::cmp::{Eq, Ordering, PartialEq, PartialOrd};
use std::hash::{Hash, Hasher};
use std::ops::Neg;

use std::mem;
use std::fmt;

use std::marker::PhantomData;

#[derive(Clone, Copy)]
pub struct Frac<T> {
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
    ///   Result { ok: Frac, err: DivisionByZero }. Behind the scenes, a pattern match
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

/// Returns a new Fraction where the denominator is guaranteed to be non-negative.
///
/// If the provided denominator is negative, both numerator and denominator will
/// be negated; this will result in a mathematically equivalent fraction, but with the
/// denominator becoming non-negative.
///
/// All other Fraction operations assume a non-negative denominator, so this is an
/// important invariant to maintain!
///
/// Panics in non-release builds if given a denominator of zero.
pub fn new(numerator: i64, denominator: i64) -> Frac<Valid> {
    assert_ne!(denominator, 0);

    if denominator.is_positive() {
        Frac { numerator, denominator, phantom: PhantomData }
    } else {
        // Denominator may never be negative. This lets us avoid -0,
        // get the sign merely by returning the numerator's sign, etc.
        Frac { numerator: -numerator, denominator: -denominator, phantom: PhantomData }
    }
}

impl<T> Frac<T> {

    #[inline]
    /// Reduces the fraction in place.
    pub fn reduce(&mut self) {
        let common_divisor = gcd(self.numerator, self.denominator);

        self.numerator = self.numerator / common_divisor;
        self.denominator = self.denominator / common_divisor;
    }

    #[inline]
    /// Reduces the fraction, then returns the numerator.
    pub fn reduced_numerator(&mut self) -> i64 {
        self.reduce();
        self.numerator
    }

    #[inline]
    /// Reduces the fraction, then returns the denominator.
    pub fn reduced_denominator(&mut self) -> i64 {
        self.reduce();
        self.denominator
    }

    pub fn is_rational(&self) -> bool {
        self.denominator.is_positive()
    }

    #[inline]
    /// Reduces the fraction, then returns a tuple of (numerator, denominator).
    pub fn reduced(&mut self) -> ( i64, i64 ) {
        self.reduce();
        ( self.numerator, self.denominator )
    }

    #[inline]
    /// Reduces the fraction, then returns true iff the denominator is 1.
    pub fn is_integer(&mut self) -> bool {
        self.reduce();
        self.denominator == 1
    }

    #[inline]
    /// Returns true iff the numerator is zero, without reducing first.
    /// This is more efficient than getting the reduced numerator and then
    /// comparing it to 0, because that has to execute the reduce operation.
    pub fn is_zero(self) -> bool {
        self.numerator == 0
    }

    pub fn abs(&mut self) -> Self {
        let (mut numerator, underflowed) = self.numerator.overflowing_abs();

        // If we underflowed, reduce and try again.
        if underflowed {
            self.reduce();

            numerator = self.numerator.abs();
        }

        Frac { numerator, denominator: self.denominator, phantom: PhantomData }
    }


    pub fn checked_abs(&mut self) -> Option<Self> {
        let (numerator, underflowed) = self.numerator.overflowing_abs();

        // If we underflowed, reduce and try again.
        if underflowed {
            self.reduce();

            match self.numerator.overflowing_abs() {
                (numerator, false) => {
                    Some(Frac { numerator, denominator: self.denominator, phantom: PhantomData })
                },
                (_, true) => None
            }
        } else {
            Some(Frac { numerator, denominator: self.denominator, phantom: PhantomData })
        }
    }

    pub fn overflowing_abs(&mut self) -> (Self, bool) {
        let (numerator, underflowed) = self.numerator.overflowing_abs();

        // If we underflowed, reduce and try again.
        if underflowed {
            self.reduce();

            let (numerator, underflowed) = self.numerator.overflowing_abs();

            (Frac { numerator, denominator: self.denominator, phantom: PhantomData }, underflowed)
        } else {
            (Frac { numerator, denominator: self.denominator, phantom: PhantomData }, underflowed)
        }
    }

    /// Add two fractions, returning None on overflow. Note: overflow can occur on more than just large numerators!
    /// This requires mutable references because it may decide to reduce
    /// the fractions mid-operation, and that reduction should persist to avoid having to redo
    /// that calculation later.
    pub fn checked_add(&mut self, other: &mut Self) -> Option<Self> {
        if self.denominator == other.denominator {
            // Happy path - we get to skip calculating a common denominator!
            match self.numerator.overflowing_add(other.numerator) {
                (numerator, false) => Some(Frac { numerator, denominator: self.denominator, phantom: PhantomData }),
                (_, true) => self.reducing_checked_add(other)
            }
        } else {
            let common_denom: i64 = self.denominator * other.denominator;

            (common_denom / self.denominator).checked_mul(self.numerator)
                .and_then(|self_numer| {
                    (common_denom / other.denominator).checked_mul(other.numerator)
                        .and_then(|other_numer| {
                            self_numer.checked_add(other_numer)
                                .map(|numerator| Frac { numerator, denominator: common_denom, phantom: PhantomData })
                        })
                })
                // Something overflowed - try reducing the inputs first.
                .or_else(|| self.reducing_checked_add(other))
        }
    }

    /// Add two fractions. This requires mutable references because it may decide to reduce
    /// the fractions mid-operation, and that reduction should persist to avoid having to redo
    /// that calculation later.
    pub fn add(&mut self, other: &mut Self) -> Self {
        if self.denominator == other.denominator {
            // Happy path - we get to skip calculating a common denominator!
            match self.numerator.overflowing_add(other.numerator) {
                (numerator, false) => Frac { numerator, denominator: self.denominator, phantom: PhantomData },
                (_, true) => self.reducing_add(other)
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
                                (numerator, false) => Frac { numerator, denominator: common_denom, phantom: PhantomData },
                                (_, true) => self.reducing_add(other)
                            }
                        },
                        (_, true) => self.reducing_add(other)
                    }
                },
                (_, true) => self.reducing_add(other)
            }
        }
    }

    /// Add while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_add(&mut self, other: &mut Self) -> Self {
        self.reduce();
        other.reduce();

        let denominator = lcm(self.denominator, other.denominator);
        let numerator =
            (self.numerator * (denominator / self.denominator))
                + (other.numerator * (denominator / other.denominator));

        Frac { numerator, denominator, phantom: PhantomData }
    }

    /// Add while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_checked_add(&mut self, other: &mut Self) -> Option<Self> {
        self.reduce();
        other.reduce();

        let denominator = lcm(self.denominator, other.denominator);

        (denominator / self.denominator).checked_mul(self.numerator).and_then(|self_numerator|
            (denominator / other.denominator).checked_mul(other.numerator).and_then(|other_numerator|
                self_numerator.checked_add(other_numerator).map(|numerator|
                    Frac { numerator, denominator, phantom: PhantomData }
                )
            )
        )
    }

    /// Subtract two fractions, returning None on overflow. Note: overflow can occur on more than just large numerators!
    /// This requires mutable references because it may decide to reduce
    /// the fractions mid-operation, and that reduction should persist to avoid having to redo
    /// that calculation later.
    pub fn checked_sub(&mut self, other: &mut Self) -> Option<Self> {
        if self.denominator == other.denominator {
            match self.numerator.checked_sub(other.numerator) {
                // Happy path - we get to skip calculating a common denominator!
                Some(numerator) => Some(Frac { numerator, denominator: self.denominator, phantom: PhantomData }),
                None => self.reducing_checked_sub(other)
            }
        } else {
            let common_denom: i64 = self.denominator * other.denominator;

            (common_denom / self.denominator).checked_mul(self.numerator)
                .and_then(|self_numer| {
                    (common_denom / other.denominator).checked_mul(other.numerator)
                        .and_then(|other_numer| {
                            self_numer.checked_sub(other_numer)
                                .map(|numerator| Frac { numerator, denominator: common_denom, phantom: PhantomData })
                        })
                })
                // Something overflowed - try reducing the inputs first.
                .or_else(|| self.reducing_checked_sub(other))
        }
    }

    /// Subtract two fractions. This requires mutable references because it may decide to reduce
    /// the fractions mid-operation, and that reduction should persist to avoid having to redo
    /// that calculation later.
    pub fn sub(&mut self, other: &mut Self) -> Self {
        if self.denominator == other.denominator {
            // Happy path - we get to skip calculating a common denominator!
            match self.numerator.overflowing_sub(other.numerator) {
                (numerator, false) => Frac { numerator, denominator: self.denominator, phantom: PhantomData },
                (_, true) => self.reducing_sub(other)
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
                                (numerator, false) => Frac { numerator, denominator: common_denom, phantom: PhantomData },
                                (_, true) => self.reducing_sub(other)
                            }
                        },
                        (_, true) => self.reducing_sub(other)
                    }
                },
                (_, true) => self.reducing_sub(other)
            }
        }
    }

    /// Subtract while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_sub(&mut self, other: &mut Self) -> Self {
        self.reduce();
        other.reduce();

        let denominator = lcm(self.denominator, other.denominator);
        let numerator =
            (self.numerator * (denominator / self.denominator))
                - (other.numerator * (denominator / other.denominator));

        Frac { numerator, denominator, phantom: PhantomData }
    }

    /// Subtract while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_checked_sub(&mut self, other: &mut Self) -> Option<Self> {
        self.reduce();
        other.reduce();

        let denominator = lcm(self.denominator, other.denominator);

        (denominator / self.denominator).checked_mul(self.numerator).and_then(|self_numerator|
            (denominator / other.denominator).checked_mul(other.numerator).and_then(|other_numerator|
                self_numerator.checked_sub(other_numerator).map(|numerator|
                    Frac { numerator, denominator, phantom: PhantomData }
                )
            )
        )
    }

    /// Multiply two fractions, returning None on overflow. Note: overflow can occur on more than just large numerators!
    /// This requires mutable references because it may decide to reduce
    /// the fractions mid-operation, and that reduction should persist to avoid having to redo
    /// that calculation later.
    pub fn checked_mul(&mut self, other: &mut Self) -> Option<Self> {
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
                    Some(Frac {
                        numerator: numerator / self.denominator,
                        denominator: self.denominator,
                        phantom: PhantomData
                    })
                } else {
                    match self.denominator.checked_mul(other.denominator) {
                        Some(denominator) => Some(Frac { numerator, denominator, phantom: PhantomData }),

                        // Denominator overflowed. See if reducing the inputs helps!
                        None => self.reducing_checked_mul(other)
                    }
                }
            },

            // Numerator overflowed. See if reducing the inputs helps!
            None => self.reducing_checked_mul(other)
        }
    }

    /// Multiply two fractions. This requires mutable references because it may decide to reduce
    /// the fractions mid-operation, and that reduction should persist to avoid having to redo
    /// that calculation later.
    pub fn mul(&mut self, other: &mut Self) -> Self {
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
                    Frac {
                        numerator: numerator / self.denominator,
                        denominator: self.denominator,
                        phantom: PhantomData
                    }
                } else {
                    match self.denominator.overflowing_mul(other.denominator) {
                        (denominator, false) => Frac { numerator, denominator, phantom: PhantomData },

                        // Denominator overflowed. See if reducing the inputs helps!
                        (_, true) => self.reducing_mul(other)
                    }
                }
            },

            // Numerator overflowed. See if reducing the inputs helps!
            (_, true) => self.reducing_mul(other)
        }
    }

    /// This requires mutable references because it may decide to reduce
    /// the fractions mid-operation, and that reduction should persist to avoid having to redo
    /// that calculation later.
    pub fn checked_div(&mut self, other: &mut Self) -> Option<Self> {
        // We're going to multiply by the reciprocal of `other`, so if its numerator
        // was 0, then the resulting fraction will have 0 for a denominator, so we're done.
        if other.numerator == 0 {
            return None;
        }

        match self.numerator.checked_mul(other.denominator) {
            Some(numerator) => {
                match self.denominator.checked_mul(other.numerator) {
                    Some(denominator) => Some(Frac { numerator, denominator, phantom: PhantomData }),
                    // Denominator overflowed. See if reducing the inputs helps!
                    None => self.reducing_checked_div(other)
                }
            },

            // Numerator overflowed. See if reducing the inputs helps!
            None => self.reducing_checked_div(other)
        }
    }

    /// If the Frac is valid, returns it wrapped in Some.
    /// Otherwise, returns None.
    pub fn into_valid<V>(self) -> Option<Frac<Valid>> {
        if self.denominator.is_positive() {
            Some(unsafe { std::mem::transmute::<Frac<T>, Frac<Valid>>(self) })
        } else {
            None
        }
    }

    /// If the Frac is valid, returns it with the type variable set accordingly.
    /// Otherwise, returns the fallback value.
    pub fn valid_or<V>(self, fallback: V) -> Result<Frac<Valid>, V> {
        if self.denominator.is_positive() {
            Ok(unsafe { std::mem::transmute::<Frac<T>, Frac<Valid>>(self) })
        } else {
            Err(fallback)
        }
    }

    pub fn valid_or_else<F, V>(self, fallback_fn: F) -> Result<Frac<Valid>, V>
    where F: Fn() -> V
    {
        if self.denominator.is_positive() {
            Ok(unsafe { std::mem::transmute::<Frac<T>, Frac<Valid>>(self) })
        } else {
            Err(fallback_fn())
        }
    }

    /// Divide two fractions.
    ///
    /// This returns a Frac with an unbound type parameter because the result may not be valid.
    ///
    /// This requires mutable references because it may decide to reduce
    /// the fractions mid-operation, and that reduction should persist to avoid having to redo
    /// that calculation later.
    pub fn div<U, V>(&mut self, other: &mut Frac<U>) -> Frac<V> {
        match self.numerator.checked_mul(other.denominator) {
            Some(numerator) => {
                match self.denominator.checked_mul(other.numerator) {
                    Some(denominator) => Frac { numerator, denominator, phantom: PhantomData },
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
    fn reducing_mul(&mut self, other: &mut Self) -> Self {
        self.reduce();
        other.reduce();

        // Preserving common denominator is out the window at this point.
        let numerator = self.numerator * other.numerator;
        let denominator = self.denominator * other.denominator;

        Frac { numerator, denominator, phantom: PhantomData }
    }

    /// Multiply while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_checked_mul(&mut self, other: &mut Self) -> Option<Self> {
        self.reduce();
        other.reduce();

        // Preserving common denominator is out the window at this point.
        self.numerator.checked_mul(other.numerator)
            .and_then(|numerator|
                self.denominator.checked_mul(other.denominator)
                    .map(|denominator| Frac { numerator, denominator, phantom: PhantomData })
            )
    }

    /// Divide while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_checked_div(&mut self, other: &mut Self) -> Option<Self> {
        self.reduce();
        other.reduce();

        self.numerator.checked_mul(other.denominator)
            .and_then(|numerator|
                self.denominator.checked_mul(other.numerator)
                    .map(|denominator| Frac { numerator, denominator, phantom: PhantomData })
            )
    }

    /// Divide while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reducing_div<U, V>(&mut self, other: &mut Frac<U>) -> Frac<V> {
        self.reduce();
        other.reduce();

        let numerator = self.numerator * other.denominator;
        let denominator = self.denominator * other.numerator;

        Frac { numerator, denominator, phantom: PhantomData }
    }

    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reduced_eq(&self, other: &Self) -> bool {
        let mut reduced_self: Frac<Valid> = Frac { numerator: self.numerator, denominator: self.denominator, phantom: PhantomData };
        let mut reduced_other: Frac<Valid> = Frac { numerator: other.numerator, denominator: other.denominator, phantom: PhantomData };

        reduced_self.reduce();
        reduced_other.reduce();

        let denominator = lcm(reduced_self.denominator, reduced_other.denominator);
        let self_numerator = reduced_self.numerator * (denominator / reduced_self.denominator);
        let other_numerator = reduced_other.numerator * (denominator / reduced_other.denominator);

        self_numerator == other_numerator
    }

    #[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
    fn reduced_cmp(&self, other: &Self) -> Ordering {
        let mut reduced_self: Frac<Valid> = Frac { numerator: self.numerator, denominator: self.denominator, phantom: PhantomData };
        let mut reduced_other: Frac<Valid> = Frac { numerator: other.numerator, denominator: other.denominator, phantom: PhantomData };

        reduced_self.reduce();
        reduced_other.reduce();

        let denominator = lcm(reduced_self.denominator, reduced_other.denominator);
        let self_numerator = reduced_self.numerator * (denominator / reduced_self.denominator);
        let other_numerator = reduced_other.numerator * (denominator / reduced_other.denominator);

        self_numerator.cmp(&other_numerator)
    }
}

impl<T> fmt::Debug for Frac<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

impl<T> PartialEq for Frac<T> {
    fn eq(&self, other: &Self) -> bool {
        if self.denominator == other.denominator {
            self.numerator == other.numerator
        } else {
            let common_denom: i64 = self.denominator * other.denominator;

            (common_denom / self.denominator).checked_mul(self.numerator)
                .and_then(|self_numer| {
                    (common_denom / other.denominator).checked_mul(other.numerator)
                        .map(|other_numer| {
                            self_numer == other_numer
                        })
                })
                // Something overflowed - try reducing the inputs first.
                .unwrap_or_else(|| self.reduced_eq(other))
        }
    }
}

impl<T> Eq for Frac<T> {}
impl<T> PartialOrd for Frac<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Frac<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.denominator == other.denominator {
            self.numerator.cmp(&other.numerator)
        } else {
            let common_denom: i64 = self.denominator * other.denominator;

            (common_denom / self.denominator).checked_mul(self.numerator)
                .and_then(|self_numer| {
                    (common_denom / other.denominator).checked_mul(other.numerator)
                        .map(|other_numer| {
                            self_numer.cmp(&other_numer)
                        })
                })
                // Something overflowed - try reducing the inputs first.
                .unwrap_or_else(|| self.reduced_cmp(other))
        }
    }
}

impl<T> Hash for Frac<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut cloned: Frac<Valid> = Frac { numerator: self.numerator, denominator: self.denominator, phantom: PhantomData };

        cloned.reduce();

        cloned.numerator.hash(state);
        cloned.denominator.hash(state);
    }
}

impl<T> Neg for Frac<T> {
    type Output = Frac<T>;

    fn neg(self) -> Self {
        Frac { numerator: -self.numerator, denominator: self.denominator, phantom: PhantomData }
    }
}

impl<T> From<u8> for Frac<T> {
    fn from (numerator: u8) -> Frac<T> {
        Frac { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<i8> for Frac<T> {
    fn from (numerator: i8) -> Frac<T> {
        Frac { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<u16> for Frac<T> {
    fn from (numerator: u16) -> Frac<T> {
        Frac { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<i16> for Frac<T> {
    fn from (numerator: i16) -> Frac<T> {
        Frac { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<u32> for Frac<T> {
    fn from (numerator: u32) -> Frac<T> {
        Frac { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<i32> for Frac<T> {
    fn from (numerator: i32) -> Frac<T> {
        Frac { numerator: numerator as i64, denominator: 1, phantom: PhantomData }
    }
}

impl<T> From<i64> for Frac<T> {
    fn from (numerator: i64) -> Frac<T> {
        Frac { numerator, denominator: 1, phantom: PhantomData }
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
    use super::{Frac, Valid};

    pub fn frac(numerator: i64, denominator: i64) -> Frac<Valid> {
        super::new(numerator, denominator)
    }

    #[test]
    fn one_plus_one() {
        assert_eq!(
            frac(1, 1).add(&mut frac(1, 1)),
            frac(2, 1)
        );
    }

    #[test]
    fn point_one_plus_point_two() {
        assert_eq!(
            frac(1, 10).add(&mut frac(2, 10)),
            frac(3, 10)
        );
    }

    #[test]
    fn multiply() {
        assert_eq!(
            frac(2, 3).mul(&mut frac(5, 7)),
            frac(10, 21)
        );
    }

    #[test]
    fn divide() {
        assert_eq!(
            frac(2, 3).div(&mut frac(5, 7)),
            frac(14, 15)
        );
    }
}
