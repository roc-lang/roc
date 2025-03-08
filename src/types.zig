/// re-export the number primitive types
pub const num = @import("types/num.zig");

/// re-export the Type primitive
pub const Type = @import("types/type.zig").Type;

// todo -- we probably don't need this anymmore.. remove in follow-up PR
/// Lowest level of the type system, representing the most fundamental or atomic types
pub const Primitive = union(enum) {
    Int: Int,
    Float: Float,
    Bool,
    Str,
    Crash,
};

/// todo
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
/// todo
pub const Float = enum {
    F32,
    F64,
    Dec,
};
/// todo
pub const Num = union(enum) {
    Int: Int,
    Float: Float,
};

/// Marks whether a when branch is redundant using a variable.
pub const RedundantMark = struct { redundant_mark: Type.Idx };

/// Marks whether a when expression is exhaustive using a variable.
pub const ExhaustiveMark = struct { exhaustive_mark: Type.Idx };
