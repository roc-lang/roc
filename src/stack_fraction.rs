use std::cmp::{Eq, Ordering, PartialEq, PartialOrd};
use std::hash::{Hash, Hasher};
use std::ops::Neg;

use std::mem;
use std::fmt;

#[derive(Clone, Copy)]
pub struct Fraction {
	numerator: i64,
	denominator: i64
}

impl Fraction {
	/// Returns a new Fraction unless the given denominator was zero,
	/// in which case returns None.
	///
	/// For a more efficient (but astronomically less safe) alternative,
	/// see new_prereduced_with_positive_denominator.
	pub fn new(numerator: i64, denominator: i64) -> Option<Fraction> {
		if denominator.is_positive() {
			Some(Fraction { numerator, denominator })
		} else if denominator == 0 {
			None
		} else {
			// Denominator may never be negative. This lets us avoid -0,
			// get the sign merely by returning the numerator's sign, etc.
			Some(Fraction { numerator: -numerator, denominator: -denominator })
		}
	}

	/// Doesn't do any checks in release builds, but panics in debug builds if
	/// the denominator is not positive.
	#[inline]
	pub fn new_from_positive_denominator(numerator: i64, denominator: i64) -> Fraction {
		assert!(denominator > 0);

		Fraction { numerator, denominator }
	}

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

    /// Add two fractions, returning None on overflow. Note: overflow can occur on more than just large numerators!
    /// This requires mutable references because it may decide to reduce
    /// the fractions mid-operation, and that reduction should persist to avoid having to redo
    /// that calculation later.
	pub fn checked_add(&mut self, other: &mut Self) -> Option<Self> {
		if self.denominator == other.denominator {
			match self.numerator.checked_add(other.numerator) {
				// Happy path - we get to skip calculating a common denominator!
				Some(numerator) => Some(Fraction { numerator, denominator: self.denominator }),
				None => self.reducing_checked_add(other)
			}
		} else {
			let common_denom: i64 = self.denominator * other.denominator;

			(common_denom / self.denominator).checked_mul(self.numerator)
				.and_then(|self_numer| {
					(common_denom / other.denominator).checked_mul(other.numerator)
						.and_then(|other_numer| {
							self_numer.checked_add(other_numer)
								.map(|numerator| Fraction { numerator, denominator: common_denom })
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
			match self.numerator.checked_add(other.numerator) {
				// Happy path - we get to skip calculating a common denominator!
				Some(numerator) => Fraction { numerator, denominator: self.denominator },
				None => self.reducing_add(other)
			}
		} else {
			let common_denom: i64 = self.denominator * other.denominator;

			(common_denom / self.denominator).checked_mul(self.numerator)
				.and_then(|self_numer| {
					(common_denom / other.denominator).checked_mul(other.numerator)
						.and_then(|other_numer| {
							self_numer.checked_add(other_numer)
								.map(|numerator| Fraction { numerator, denominator: common_denom })
						})
				})
				// Something overflowed - try reducing the inputs first.
				.unwrap_or_else(|| self.reducing_add(other))
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

		Fraction { numerator, denominator }
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
                    Fraction { numerator, denominator }
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
				Some(numerator) => Some(Fraction { numerator, denominator: self.denominator }),
				None => self.reducing_checked_sub(other)
			}
		} else {
			let common_denom: i64 = self.denominator * other.denominator;

			(common_denom / self.denominator).checked_mul(self.numerator)
				.and_then(|self_numer| {
					(common_denom / other.denominator).checked_mul(other.numerator)
						.and_then(|other_numer| {
							self_numer.checked_sub(other_numer)
								.map(|numerator| Fraction { numerator, denominator: common_denom })
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
			match self.numerator.checked_sub(other.numerator) {
				// Happy path - we get to skip calculating a common denominator!
				Some(numerator) => Fraction { numerator, denominator: self.denominator },
				None => self.reducing_sub(other)
			}
		} else {
			let common_denom: i64 = self.denominator * other.denominator;

			(common_denom / self.denominator).checked_mul(self.numerator)
				.and_then(|self_numer| {
					(common_denom / other.denominator).checked_mul(other.numerator)
						.and_then(|other_numer| {
							self_numer.checked_sub(other_numer)
								.map(|numerator| Fraction { numerator, denominator: common_denom })
						})
				})
				// Something overflowed - try reducing the inputs first.
				.unwrap_or_else(|| self.reducing_sub(other))
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

		Fraction { numerator, denominator }
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
                    Fraction { numerator, denominator }
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
					Some(Fraction {
						numerator: numerator / self.denominator,
						denominator: self.denominator
					})
				} else {
					match self.denominator.checked_mul(other.denominator) {
						Some(denominator) => Some(Fraction { numerator, denominator }),

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
					Fraction {
						numerator: numerator / self.denominator,
						denominator: self.denominator
					}
				} else {
					match self.denominator.checked_mul(other.denominator) {
						Some(denominator) => Fraction { numerator, denominator },

						// Denominator overflowed. See if reducing the inputs helps!
						None => self.reducing_mul(other)
					}
				}
			},

			// Numerator overflowed. See if reducing the inputs helps!
			None => self.reducing_mul(other)
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
					Some(denominator) => Some(Fraction { numerator, denominator }),
					// Denominator overflowed. See if reducing the inputs helps!
					None => self.reducing_checked_div(other)
				}
			},

			// Numerator overflowed. See if reducing the inputs helps!
			None => self.reducing_checked_div(other)
		}
	}

    /// Divide two fractions, panicking if given a 0 denominator.
    /// This requires mutable references because it may decide to reduce
    /// the fractions mid-operation, and that reduction should persist to avoid having to redo
    /// that calculation later.
	pub fn div_or_panic(&mut self, other: &mut Self) -> Self {
		// We're going to multiply by the reciprocal of `other`, so if its numerator
		// was 0, then the resulting fraction will have 0 for a denominator, so we're done.
		if other.numerator == 0 {
            panic!("Division by zero.");
		}

		match self.numerator.checked_mul(other.denominator) {
			Some(numerator) => {
				match self.denominator.checked_mul(other.numerator) {
					Some(denominator) => Fraction { numerator, denominator },
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

		Fraction { numerator, denominator }
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
					.map(|denominator| Fraction { numerator, denominator })
			)
	}

    /// Divide while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
	#[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
	fn reducing_checked_div(&mut self, other: &mut Self) -> Option<Self> {
		// This should have been verified already by this point!
		assert_ne!(other.numerator, 0);

		self.reduce();
		other.reduce();

		self.numerator.checked_mul(other.denominator)
			.and_then(|numerator|
				self.denominator.checked_mul(other.numerator)
					.map(|denominator| Fraction { numerator, denominator })
			)
	}

    /// Divide while sacrificing performance to avoid overflow.
    /// This should only be used as a fallback after an overflow was caught in a higher-perf arithmetic operation.
	#[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
	fn reducing_div(&mut self, other: &mut Self) -> Self {
		// This should have been verified already by this point!
		assert_ne!(other.numerator, 0);

		self.reduce();
		other.reduce();

		let numerator = self.numerator * other.denominator;
		let denominator = self.denominator * other.numerator;

        Fraction { numerator, denominator }
	}

	#[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
	fn reduced_eq(&self, other: &Self) -> bool {
		let mut reduced_self = self.clone();
		let mut reduced_other = other.clone();

		reduced_self.reduce();
		reduced_other.reduce();

		let denominator = lcm(reduced_self.denominator, reduced_other.denominator);
		let self_numerator = reduced_self.numerator * (denominator / reduced_self.denominator);
		let other_numerator = reduced_other.numerator * (denominator / reduced_other.denominator);

		self_numerator == other_numerator
	}

	#[inline(never)] // We don't want to inline this because it should be almost never invoked in practice.
	fn reduced_cmp(&self, other: &Self) -> Ordering {
		let mut reduced_self = self.clone();
		let mut reduced_other = other.clone();

		reduced_self.reduce();
		reduced_other.reduce();

		let denominator = lcm(reduced_self.denominator, reduced_other.denominator);
		let self_numerator = reduced_self.numerator * (denominator / reduced_self.denominator);
		let other_numerator = reduced_other.numerator * (denominator / reduced_other.denominator);

		self_numerator.cmp(&other_numerator)
	}
}

impl fmt::Debug for Fraction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}/{}", self.numerator, self.denominator)
    }
}

impl PartialEq for Fraction {
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

impl Eq for Fraction {}
impl PartialOrd for Fraction {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for Fraction {
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

impl Hash for Fraction {
    fn hash<H: Hasher>(&self, state: &mut H) {
		let (numerator, denominator) = self.clone().reduced();

        numerator.hash(state);
        denominator.hash(state);
    }
}

impl Neg for Fraction {
	type Output = Fraction;

	fn neg(self) -> Self {
		Fraction { numerator: -self.numerator, denominator: self.denominator }
	}
}

impl From<u8> for Fraction {
	fn from (numerator: u8) -> Fraction {
		Fraction { numerator: numerator as i64, denominator: 1 }
	}
}

impl From<i8> for Fraction {
	fn from (numerator: i8) -> Fraction {
		Fraction { numerator: numerator as i64, denominator: 1 }
	}
}

impl From<u16> for Fraction {
	fn from (numerator: u16) -> Fraction {
		Fraction { numerator: numerator as i64, denominator: 1 }
	}
}

impl From<i16> for Fraction {
	fn from (numerator: i16) -> Fraction {
		Fraction { numerator: numerator as i64, denominator: 1 }
	}
}

impl From<u32> for Fraction {
	fn from (numerator: u32) -> Fraction {
		Fraction { numerator: numerator as i64, denominator: 1 }
	}
}

impl From<i32> for Fraction {
	fn from (numerator: i32) -> Fraction {
		Fraction { numerator: numerator as i64, denominator: 1 }
	}
}

impl From<i64> for Fraction {
	fn from (numerator: i64) -> Fraction {
		Fraction { numerator, denominator: 1 }
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
