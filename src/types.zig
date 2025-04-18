pub const num = @import("types/num.zig");

pub const Type = @import("types/type.zig").RType;

// todo -- we probably don't need this anymmore.. remove in follow-up PR
/// Lowest level of the type system, representing the most fundamental or atomic types
pub const Primitive = union(enum) {
    Int: Int,
    Float: Float,
    Bool,
    Str,
    Crash,
};

/// All Roc Int types
pub const Int = enum {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    U128,
    I128,
};

/// All Roc Float types
pub const Float = enum {
    F32,
    F64,
    Dec,
};

/// Roc Num types; Int and Float
pub const Num = union(enum) {
    Int: Int,
    Float: Float,
};

/// Marks whether a when branch is redundant using a variable.
pub const RedundantMark = struct { redundant_mark: Type.Idx };

/// Marks whether a when expression is exhaustive using a variable.
pub const ExhaustiveMark = struct { exhaustive_mark: Type.Idx };
