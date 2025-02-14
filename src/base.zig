pub const Ident = @import("base/Ident.zig");
pub const Module = @import("base/Module.zig");
pub const Region = @import("base/Region.zig");
pub const Package = @import("base/Package.zig");
pub const TagName = @import("base/TagName.zig");
pub const FieldName = @import("base/FieldName.zig");
pub const ModuleEnv = @import("base/ModuleEnv.zig");
pub const TypeVarName = @import("base/TypeVarName.zig");
pub const StringLiteral = @import("base/StringLiteral.zig");

pub const Recursive = enum {
    NotRecursive,
    Recursive,
    TailRecursive,
};

pub const CalledVia = enum {};

/// Represents a value written as-is in a Roc source file.
pub const Literal = union(enum) {
    Int: Int,
    Float: Float,
    Bool: bool,
    Str: StringLiteral.Idx,
    /// A crash with a textual message describing why a crash occurred.
    Crash: StringLiteral.Idx,

    /// An integer number literal.
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

    /// A fractional number literal.
    pub const Float = union(enum) {
        F32: f32,
        F64: f64,
        // We represent Dec as a large integer divided by 10^18, which is the maximum
        // number of decimal places that allow lossless conversion of U64 to Dec.
        Dec: u128,
    };

    /// An integer or fractional number literal.
    pub const Num = union(enum) {
        Int: Int,
        Float: Float,
    };
};
