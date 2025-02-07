const std = @import("std");
const base = @import("base.zig");
const cols = @import("collections.zig");
const num = @import("types/num.zig");
const primitive = @import("types/primitive.zig");

/// A type variable unique to an entity in Roc code, used
/// for type unification.
pub const TypeVar = enum(u32) { _ };

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

/// Marks whether a when branch is redundant using a variable.
pub const RedundantMark = struct { redundant_mark: TypeVar };

/// Marks whether a when expression is exhaustive using a variable.
pub const ExhaustiveMark = struct { exhaustive_mark: TypeVar };
