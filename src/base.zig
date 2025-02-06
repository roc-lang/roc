pub const Ident = @import("base/Ident.zig");
pub const Module = @import("base/Module.zig");
pub const ModuleEnv = @import("base/ModuleEnv.zig");
pub const Package = @import("base/Package.zig");
pub const Region = @import("base/Region.zig");

const StringLiteral = @import("collections/interner/StringLiteral.zig");

pub const Recursive = enum {
    NotRecursive,
    Recursive,
    TailRecursive,
};

// TODO: can this be smaller than u32?
/// Source of crash, and its runtime representation to roc_panic.
pub const CrashOrigin = enum(u32) {
    /// The crash is due to Roc, either via a builtin or type error.
    Roc = 0,
    /// The crash is user-defined.
    User = 1,
};

pub const LowLevel = .{};

// TODO: move to relevant stages
pub const TypeVar = struct { id: u32 };

/// Represents a value
pub const Literal = union(enum) {
    Int: Int,
    Float: Float,
    Bool: bool,
    Str: StringLiteral,
    Crash: StringLiteral,

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
