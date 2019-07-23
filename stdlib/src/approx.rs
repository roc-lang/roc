use std::marker::PhantomData;
use std::hash::{Hash, Hasher};

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
pub struct Approximation<T> {
    value: f64,
    phantom: PhantomData<T>
}

pub struct Valid;

pub type Approx = Approximation<Valid>;

impl Hash for Approx {
    fn hash<H: Hasher>(&self, state: &mut H) {
        panic!("TODO: implement using integer_decode");
        // let (man, exp, sign) = f.integer_decode();

        // if man == 0 {
        //     // Consolidate the representation of zero, whether signed or not
        //     // The IEEE standard considers positive and negative zero to be equal
        //     0
        // } else {
        //     (man ^ ((exp as u64) << 48) ^ sign as u64)
        // }.hash(state)
    }
}

impl<T> Approximation<T> {
    pub fn abs(num: &Self) -> Self {
        Approximation { value: num.value.abs(), phantom: PhantomData }
    }

    /// Returns whether the approximation is valid.
    /// For an Approx<Valid>, this will always return true.
    #[inline(always)]
    pub fn is_valid(&self) -> bool {
        self.value.is_finite()
    }

    /// If the approximation is valid, return it wrapped in Some.
    /// Otherwise, return None.
    pub fn into_valid<V>(self) -> Option<Approximation<Valid>> {
        if self.is_valid() {
            Some(Approximation {value: self.value, phantom: PhantomData})
        } else {
            None
        }
    }

    /// If the approximation is valid, return it with the type variable set accordingly.
    /// Otherwise, return the fallback value.
    pub fn valid_or<V>(self, fallback: V) -> Result<Approximation<Valid>, V> {
        if self.is_valid() {
            Ok(Approximation {value: self.value, phantom: PhantomData})
        } else {
            Err(fallback)
        }
    }

    pub fn valid_or_else<F, V>(self, fallback_fn: F) -> Result<Approximation<Valid>, V>
    where F: Fn() -> V
    {
        if self.is_valid() {
            Ok(Approximation {value: self.value, phantom: PhantomData})
        } else {
            Err(fallback_fn())
        }
    }
}

impl<T> From<f64> for Approximation<T> {
    fn from(num: f64) -> Self {
        Approximation { value: num, phantom: PhantomData }
    }
}

impl<T> From<f32> for Approximation<T> {
    fn from(num: f32) -> Self {
        Approximation { value: num as f64, phantom: PhantomData }
    }
}

impl<T> Into<f64> for Approximation<T> {
    fn into(self) -> f64 {
        self.value
    }
}

impl<T> Into<f32> for Approximation<T> {
    fn into(self) -> f32 {
        self.value as f32
    }
}