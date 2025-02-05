const cols = @import("../collections.zig");

// TODO: figure out how to combine enums and union(enum)s at comptime
// to avoid them being multilevel

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

pub const Literal = union(enum) {
    Int: Int,
    Float: Float,
    Bool: bool,
    Str: cols.LargeStringId,
    Crash: cols.LargeStringId,

    pub const Int = union(enum) {
        I8: i8,
        U8: u8,
        I16: i16,
        U16: u16,
        I32: i32,
        U32: u32,
        I64: i64,
        U64: u64,
        I128: i128,
        U128: u128,
    };

    pub const Float = union(enum) {
        F32: f32,
        F64: f64,
        // We represent Dec as a large int divided by 10^18, which is the maximum
        // number of decimal places that allows lossless conversion of U64 to Dec
        Dec: u128,
    };

    pub const Num = union(enum) {
        Int: Int,
        Float: Float,
    };
};
