//! Basic types that are useful throughout the compiler.
const std = @import("std");

pub const SExprTree = @import("SExprTree.zig");
pub const Ident = @import("Ident.zig");
pub const Region = @import("Region.zig");
pub const StringLiteral = @import("StringLiteral.zig");
pub const RegionInfo = @import("RegionInfo.zig");
pub const Scratch = @import("Scratch.zig").Scratch;
pub const ByteSlices = @import("ByteSlices.zig");
pub const parallel = @import("parallel.zig");
pub const SmallStringInterner = @import("SmallStringInterner.zig");

pub const safe_memory = @import("safe_memory.zig");

pub const target = @import("target.zig");
pub const DataSpan = @import("DataSpan.zig");
pub const PackedDataSpan = @import("PackedDataSpan.zig").PackedDataSpan;
pub const FunctionArgs = @import("PackedDataSpan.zig").FunctionArgs;
pub const SmallCollections = @import("PackedDataSpan.zig").SmallCollections;

pub const CommonEnv = @import("CommonEnv.zig");

test {
    _ = @import("Ident.zig");
    _ = @import("ByteSlices_test.zig");
}

/// Whether a function calls itself.
pub const Recursive = enum {
    NotRecursive,
    Recursive,
    /// Functions that only recurse at the very end of the function body,
    /// meaning they can be converted to loops when compiled.
    TailRecursive,
};

/// The manner in which a function was called, useful for giving better feedback to users.
pub const CalledVia = enum {
    /// Normal function application, e.g. `foo(bar)`
    apply,
    /// Calling with an operator, e.g. `(1 + 2)`
    binop,
    /// Calling with a unary operator, e.g. `!foo` or `-foo`
    unary_op,
    /// This call is the result of desugaring string interpolation,
    /// e.g. "${first} ${last}" is transformed into `Str.concat(Str.concat(first, " "))` last.
    string_interpolation,
    /// This call is the result of desugaring a map2-based Record Builder field. e.g.
    /// ```roc
    /// { Result.parallel <-
    ///     foo: get("a"),
    ///     bar: get("b"),
    /// }
    /// ```
    /// is transformed into
    /// ```roc
    /// Result.parallel(get("a"), get("b"), (|foo, bar | { foo, bar }))
    /// ```
    record_builder,
};

/// Represents a value written as-is in a Roc source file.
pub const Literal = union(enum) {
    Int: IntLiteral,
    Float: FracLiteral,
    Bool: bool,
    Str: StringLiteral.Idx,
    /// A crash with a textual message describing why a crash occurred.
    Crash: StringLiteral.Idx,
};

/// An integer number literal.
pub const IntLiteral = union(enum) {
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
pub const FracLiteral = union(enum) {
    F32: f32,
    F64: f64,
    // We represent Dec as a large integer divided by 10^18, which is the maximum
    // number of decimal places that allow lossless conversion of U64 to Dec.
    Dec: u128,
};

/// An integer or fractional number literal.
pub const NumLiteral = union(enum) {
    Int: IntLiteral,
    Frac: FracLiteral,
};

test "base tests" {
    std.testing.refAllDecls(@import("CommonEnv.zig"));
    std.testing.refAllDecls(@import("DataSpan.zig"));
    std.testing.refAllDecls(@import("Ident.zig"));
    std.testing.refAllDecls(@import("PackedDataSpan.zig"));
    std.testing.refAllDecls(@import("parallel.zig"));
    std.testing.refAllDecls(@import("Region.zig"));
    std.testing.refAllDecls(@import("RegionInfo.zig"));
    std.testing.refAllDecls(@import("safe_memory.zig"));
    std.testing.refAllDecls(@import("Scratch.zig"));
    std.testing.refAllDecls(@import("SExprTree.zig"));
    std.testing.refAllDecls(@import("SmallStringInterner.zig"));
    std.testing.refAllDecls(@import("StringLiteral.zig"));
    std.testing.refAllDecls(@import("target.zig"));
}
