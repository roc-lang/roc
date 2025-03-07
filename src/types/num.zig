//! Useful types for representing numbers in the Roc type system.

const base = @import("../base.zig");

/// A bound placed on a number because of its literal value.
/// e.g. `-5` cannot be unsigned, and 300 does not fit in a U8
pub const NumericRange = union(enum) {
    IntAtLeastSigned: Bound.Int.Width,
    IntAtLeastEitherSign: Bound.Int.Width,
    NumAtLeastSigned: Bound.Int.Width,
    NumAtLeastEitherSign: Bound.Int.Width,
};

/// Whether a number literal constrains its type to be signed.
pub const SignDemand = enum {
    /// Can be signed or unsigned.
    NoDemand,
    /// Must be signed.
    Signed,
};

/// A bound
pub const Bound = struct {
    /// Describes a bound on the width of an integer.
    pub const Int = union(enum) {
        /// There is no bound on the width.
        None,
        /// Must have an exact width.
        Exact: Width,
        /// Must have a certain sign and a minimum width.
        AtLeast: struct {
            sign: SignDemand,
            width: Width,
        },

        // TODO: combine with Primitive.Num?
        pub const Width = enum {
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
            // An int literal can be promoted to an f32/f64/Dec if appropriate. The respective widths for
            // integers that can be stored in these float types without losing precision are:
            //   f32: +/- 2^24
            //   f64: +/- 2^53
            //   dec: Int128::MAX/Int128::MIN
            F32,
            F64,
            Dec,
        };
    };

    pub const Float = union(enum) {
        None,
        Exact: Width,

        pub const Width = enum {
            Dec,
            F32,
            F64,
        };
    };

    pub const Num = union(enum) {
        None,
        /// Must be an integer of a certain size, or any float.
        AtLeastIntOrFloat: struct {
            sign: SignDemand,
            width: Int.Width,
        },
    };

    pub const SingleQuote = union(enum) {
        AtLeast: struct { width: Int.Width },
    };
};
