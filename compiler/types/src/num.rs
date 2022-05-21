use crate::subs::Variable;
use roc_module::symbol::Symbol;

/// A bound placed on a number because of its literal value.
/// e.g. `-5` cannot be unsigned, and 300 does not fit in a U8
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NumericRange {
    IntAtLeastSigned(IntWidth),
    IntAtLeastEitherSign(IntWidth),
    NumAtLeastSigned(IntWidth),
    NumAtLeastEitherSign(IntWidth),
}

impl NumericRange {
    pub fn contains_symbol(&self, symbol: Symbol) -> bool {
        match symbol {
            Symbol::NUM_I8 => self.contains_int_width(IntWidth::I8),
            Symbol::NUM_U8 => self.contains_int_width(IntWidth::U8),
            Symbol::NUM_I16 => self.contains_int_width(IntWidth::I16),
            Symbol::NUM_U16 => self.contains_int_width(IntWidth::U16),
            Symbol::NUM_I32 => self.contains_int_width(IntWidth::I32),
            Symbol::NUM_U32 => self.contains_int_width(IntWidth::U32),
            Symbol::NUM_I64 => self.contains_int_width(IntWidth::I64),
            Symbol::NUM_NAT => self.contains_int_width(IntWidth::Nat),
            Symbol::NUM_U64 => self.contains_int_width(IntWidth::U64),
            Symbol::NUM_I128 => self.contains_int_width(IntWidth::I128),
            Symbol::NUM_U128 => self.contains_int_width(IntWidth::U128),

            Symbol::NUM_DEC => self.contains_float_width(FloatWidth::Dec),
            Symbol::NUM_F32 => self.contains_float_width(FloatWidth::F32),
            Symbol::NUM_F64 => self.contains_float_width(FloatWidth::F64),

            Symbol::NUM_NUM | Symbol::NUM_INT | Symbol::NUM_FRAC => {
                // these satisfy any range that they are given
                true
            }

            _ => unreachable!("weird number symbol {:?}", symbol),
        }
    }

    fn contains_float_width(&self, _width: FloatWidth) -> bool {
        // we don't currently check the float width
        true
    }

    fn contains_int_width(&self, width: IntWidth) -> bool {
        use NumericRange::*;

        let (range_signedness, at_least_width) = match self {
            IntAtLeastSigned(width) => (SignDemand::Signed, width),
            IntAtLeastEitherSign(width) => (SignDemand::NoDemand, width),
            NumAtLeastSigned(width) => (SignDemand::Signed, width),
            NumAtLeastEitherSign(width) => (SignDemand::NoDemand, width),
        };

        let (actual_signedness, _) = width.signedness_and_width();

        if let (IntSignedness::Unsigned, SignDemand::Signed) = (actual_signedness, range_signedness)
        {
            return false;
        }

        width.signedness_and_width().1 >= at_least_width.signedness_and_width().1
    }

    pub fn variable_slice(&self) -> &'static [Variable] {
        use NumericRange::*;

        match self {
            IntAtLeastSigned(width) => {
                let target = int_width_to_variable(*width);
                let start = SIGNED_VARIABLES.iter().position(|v| *v == target).unwrap();
                let end = SIGNED_VARIABLES.len() - 3;

                &SIGNED_VARIABLES[start..end]
            }
            IntAtLeastEitherSign(width) => {
                let target = int_width_to_variable(*width);
                let start = ALL_VARIABLES.iter().position(|v| *v == target).unwrap();
                let end = ALL_VARIABLES.len() - 3;

                &ALL_VARIABLES[start..end]
            }
            NumAtLeastSigned(width) => {
                let target = int_width_to_variable(*width);
                let start = SIGNED_VARIABLES.iter().position(|v| *v == target).unwrap();

                &SIGNED_VARIABLES[start..]
            }
            NumAtLeastEitherSign(width) => {
                let target = int_width_to_variable(*width);
                let start = ALL_VARIABLES.iter().position(|v| *v == target).unwrap();

                &ALL_VARIABLES[start..]
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum IntSignedness {
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
    /// Returns the `IntSignedness` and bit width of a variant.
    fn signedness_and_width(&self) -> (IntSignedness, u32) {
        use IntSignedness::*;
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
        use IntSignedness::*;

        if is_negative {
            match (
                self.signedness_and_width(),
                lower_bound.signedness_and_width(),
            ) {
                ((Signed, us), (Signed, lower_bound)) => us >= lower_bound,
                // Unsigned ints can never represent negative numbers; signed (non-zero width)
                // ints always can.
                ((Unsigned, _), (Signed, _)) => false,
                ((Signed, _), (Unsigned, _)) => true,
                // Trivially true; both can only express 0.
                ((Unsigned, _), (Unsigned, _)) => true,
            }
        } else {
            match (
                self.signedness_and_width(),
                lower_bound.signedness_and_width(),
            ) {
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

pub const fn int_width_to_variable(w: IntWidth) -> Variable {
    match w {
        IntWidth::U8 => Variable::U8,
        IntWidth::U16 => Variable::U16,
        IntWidth::U32 => Variable::U32,
        IntWidth::U64 => Variable::U64,
        IntWidth::U128 => Variable::U128,
        IntWidth::I8 => Variable::I8,
        IntWidth::I16 => Variable::I16,
        IntWidth::I32 => Variable::I32,
        IntWidth::I64 => Variable::I64,
        IntWidth::I128 => Variable::I128,
        IntWidth::Nat => Variable::NAT,
    }
}

pub const fn float_width_to_variable(w: FloatWidth) -> Variable {
    match w {
        FloatWidth::Dec => Variable::DEC,
        FloatWidth::F32 => Variable::F32,
        FloatWidth::F64 => Variable::F64,
    }
}

const ALL_VARIABLES: &[Variable] = &[
    Variable::I8,
    Variable::U8,
    Variable::I16,
    Variable::U16,
    Variable::I32,
    Variable::U32,
    Variable::I64,
    Variable::NAT, // FIXME: Nat's order here depends on the platfor,
    Variable::U64,
    Variable::I128,
    Variable::U128,
    Variable::F32,
    Variable::F64,
    Variable::DEC,
];

const SIGNED_VARIABLES: &[Variable] = &[
    Variable::I8,
    Variable::I16,
    Variable::I32,
    Variable::I64,
    Variable::I128,
    Variable::F32,
    Variable::F64,
    Variable::DEC,
];
