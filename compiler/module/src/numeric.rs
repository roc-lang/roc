//! Module `numeric` has utilities for numeric values in the Roc surface syntax.

use std::fmt::Display;

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

impl Display for IntWidth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use IntWidth::*;
        f.write_str(match self {
            U8 => "u8",
            U16 => "u16",
            U32 => "u32",
            U64 => "u64",
            U128 => "u128",
            I8 => "i8",
            I16 => "i16",
            I32 => "i32",
            I64 => "i64",
            I128 => "i128",
            Nat => "nat",
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FloatWidth {
    Dec,
    F32,
    F64,
}

impl Display for FloatWidth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use FloatWidth::*;
        f.write_str(match self {
            Dec => "dec",
            F32 => "f32",
            F64 => "f64",
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NumWidth {
    Int(IntWidth),
    Float(FloatWidth),
}

impl Display for NumWidth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use NumWidth::*;
        match self {
            Int(iw) => iw.fmt(f),
            Float(fw) => fw.fmt(f),
        }
    }
}

/// Describes a bound on the width of a numeric literal.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum NumericBound<W, V>
where
    W: Copy,
    V: Copy,
{
    /// There is no bound on the width.
    None { width_variable: V },
    /// Must have exactly the width `W`.
    Exact(W),
}
