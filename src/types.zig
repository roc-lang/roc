const type_mod = @import("types/type.zig");

pub const num = @import("types/num.zig");

pub const Type = type_mod.Type;

/// Lowest level of the type system, representing the most fundamental or atomic types
pub const Primitive = union(enum) {
    Int: Int,
    Float: Float,
    Bool,
    Str,
    Crash,

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

    pub const Float = enum {
        F32,
        F64,
        Dec,
    };

    pub const Num = union(enum) {
        Int: Int,
        Float: Float,
    };
};

/// Marks whether a when branch is redundant using a variable.
pub const RedundantMark = struct { redundant_mark: Type.Idx };

/// Marks whether a when expression is exhaustive using a variable.
pub const ExhaustiveMark = struct { exhaustive_mark: Type.Idx };
