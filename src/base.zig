//! Basic types that are useful throughout the compiler.

const std = @import("std");
const parse = @import("check/parse.zig");
const module_work = @import("base/module_work.zig");

pub const sexpr = @import("base/sexpr.zig");
pub const Ident = @import("base/Ident.zig");
pub const Region = @import("base/Region.zig");
pub const Package = @import("base/Package.zig");
pub const ModuleEnv = @import("base/ModuleEnv.zig");
pub const ModuleImport = @import("base/ModuleImport.zig");
pub const StringLiteral = @import("base/StringLiteral.zig");
pub const DiagnosticPosition = @import("base/DiagnosticPosition.zig");
pub const Scratch = @import("base/Scratch.zig").Scratch;

pub const ModuleWork = module_work.ModuleWork;
pub const ModuleWorkIdx = module_work.ModuleWorkIdx;

/// Whether a function calls itself.
pub const Recursive = enum {
    NotRecursive,
    Recursive,
    /// Functions that only recurse at the very end of the function body,
    /// meaning they can be converted to loops when compiled.
    TailRecursive,
};

/// The manner in which a function was called, useful for giving better feedback to users.
pub const CalledVia = enum {};

/// Represents a value written as-is in a Roc source file.
pub const Literal = union(enum) {
    Int: IntLiteral,
    Float: FloatLiteral,
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
pub const FloatLiteral = union(enum) {
    F32: f32,
    F64: f64,
    // We represent Dec as a large integer divided by 10^18, which is the maximum
    // number of decimal places that allow lossless conversion of U64 to Dec.
    Dec: u128,
};

/// An integer or fractional number literal.
pub const NumLiteral = union(enum) {
    Int: IntLiteral,
    Float: FloatLiteral,
};

/// Just a small struct to take a span of data in an array
pub const DataSpan = struct {
    start: u32,
    len: u32,
};
