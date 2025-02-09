// TODO: figure out how to combine enums and union(enum)s at comptime
// to avoid them being multilevel

/// Lowest level of the type system, representing the most fundemental or atomic types
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
