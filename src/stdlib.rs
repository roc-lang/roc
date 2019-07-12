
use std::marker::PhantomData;

/// Approx is stored as an f64 under the hood.
///
/// However, part of Roc's design is that Roc users never encounter Infinity,
/// -Infinity, NaN, or -0. To Roc application authors, the only difference between
/// Approx and Frac is that Approx supports a few more operations (sqrt,
/// trigonometry, etc) and is potentially imprecise.
///
/// To achieve this, Roc maps all invalid Float values (NaN, Infinity, -Infinity) to
/// Err values. This means that any value which could contain one of these bit patterns
/// is typed as Result { ok: Approx, err: InvalidApprox }, including any Approx values
/// passed into Roc.
///
/// Roc code does not have the expressive power to create NaN, Infinity, or -Infinity,
/// so the Approx type inside Roc represents an f64 that is guaratneed not to be NaN,
/// Infinity, or -Infinity.
///
/// Additionally, the implementation detail of 0 and -0 being different f64 values does
/// not reach Roc code because there is no way to convert an Approx directly to a String.
/// Instead, Approx must go through conversion to either a Frac or an Int, neither of
/// which is capable of representing -0. In f64 operations, 0 and -0 are considered
/// equivalent, so the distinction does not matter there either.
pub struct Approx<T> {
    value: f64,
    phantom: PhantomData<T>
}

/// A plain old i64.
pub struct Int(i64);

/// A plain old bool.
pub type Bool = bool;

fn underflow_panic() -> ! {
    panic!("Underflow!");
}

impl Int {
    pub fn abs(&self) -> Self {
        let Int(int_self) = self;

        let (output, underflowed) = int_self.overflowing_abs();

        if underflowed {
            underflow_panic();
        }

        Int(output)
    }
}

impl<T> Approx<T> {
    pub fn abs(num: &Self) -> Self {
        Approx { value: num.value.abs(), phantom: PhantomData }
    }
}