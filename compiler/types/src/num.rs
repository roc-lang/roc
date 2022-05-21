#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntSign {
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
    pub fn sign_and_width(&self) -> (IntSign, u32) {
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

    pub fn type_str(&self) -> &'static str {
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

    pub fn max_value(&self) -> u128 {
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

    pub fn min_value(&self) -> i128 {
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
pub enum SignDemand {
    /// Can be signed or unsigned.
    NoDemand,
    /// Must be signed.
    Signed,
}

/// Describes a bound on the width of an integer.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum IntBound {
    /// There is no bound on the width.
    None,
    /// Must have an exact width.
    Exact(IntWidth),
    /// Must have a certain sign and a minimum width.
    AtLeast { sign: SignDemand, width: IntWidth },
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FloatBound {
    None,
    Exact(FloatWidth),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NumBound {
    None,
    /// Must be an integer of a certain size, or any float.
    AtLeastIntOrFloat {
        sign: SignDemand,
        width: IntWidth,
    },
}
