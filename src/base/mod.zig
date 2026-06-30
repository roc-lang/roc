//! Basic types that are useful throughout the compiler.
const std = @import("std");
const builtin = @import("builtin");

pub const SExprTree = @import("SExprTree.zig");
pub const Ident = @import("Ident.zig");
pub const Region = @import("Region.zig");
pub const StringLiteral = @import("StringLiteral.zig");
pub const LowLevel = @import("LowLevel.zig").LowLevel;
pub const RegionInfo = @import("RegionInfo.zig");
pub const SourceLoc = @import("source_loc.zig").SourceLoc;
pub const Scratch = @import("Scratch.zig").Scratch;
pub const parallel = @import("parallel.zig");
pub const SmallStringInterner = @import("SmallStringInterner.zig");
pub const SerialStringInterner = @import("SerialStringInterner.zig");

/// Single-threaded arena allocator, re-exported from `collections` for callers
/// that already depend on `base`.
pub const SingleThreadArena = @import("collections").SingleThreadArena;

pub const safe_memory = @import("safe_memory.zig");
pub const signal_handler = @import("signal_handler.zig");
pub const stack_overflow = @import("stack_overflow.zig");
pub const elf_self_relocate = @import("elf_self_relocate.zig");

pub const target = @import("target.zig");
pub const DataSpan = @import("DataSpan.zig").DataSpan;
pub const PackedDataSpan = @import("PackedDataSpan.zig").PackedDataSpan;
pub const FunctionArgs = @import("PackedDataSpan.zig").FunctionArgs;
pub const SmallCollections = @import("PackedDataSpan.zig").SmallCollections;

pub const CommonEnv = @import("CommonEnv.zig");
pub const source_utils = @import("source_utils.zig");
pub const module_path = @import("module_path.zig");
pub const url = @import("url.zig");

/// The default general-purpose allocator for the current target (fast, not leak-checking).
/// Prefers libc's malloc (its ASan/Valgrind/LD_PRELOAD tooling, and on LLVM paths
/// it's the allocator LLVM already uses) — except on musl, whose malloc is slow,
/// where smp_allocator wins. Falls back to smp_allocator without libc, and to
/// wasm_allocator on freestanding.
pub fn defaultGpa() std.mem.Allocator {
    if (builtin.target.os.tag == .freestanding) return std.heap.wasm_allocator;
    if (builtin.link_libc and !builtin.target.abi.isMusl()) return std.heap.c_allocator;
    return std.heap.smp_allocator;
}

test {
    const ident = @import("Ident.zig");
    const module_path_mod = @import("module_path.zig");
    std.testing.refAllDecls(ident);
    std.testing.refAllDecls(module_path_mod);
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
    /// { Try.parallel <-
    ///     foo: get("a"),
    ///     bar: get("b"),
    /// }
    /// ```
    /// is transformed into
    /// ```roc
    /// Try.parallel(get("a"), get("b"), (|foo, bar | { foo, bar }))
    /// ```
    record_builder,
    /// This call is the result of desugaring range syntax,
    /// e.g. `1..<5` becomes `Iter.exclusive_range(1, 5)`.
    range,
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
pub const Numeral = union(enum) {
    Int: IntLiteral,
    Frac: FracLiteral,
};

test "base tests" {
    std.testing.refAllDecls(@import("CommonEnv.zig"));
    std.testing.refAllDecls(@import("DataSpan.zig"));
    std.testing.refAllDecls(@import("Ident.zig"));
    std.testing.refAllDecls(@import("InternedBytes.zig"));
    std.testing.refAllDecls(@import("PackedDataSpan.zig"));
    std.testing.refAllDecls(@import("parallel.zig"));
    std.testing.refAllDecls(@import("Region.zig"));
    std.testing.refAllDecls(@import("RegionInfo.zig"));
    std.testing.refAllDecls(@import("safe_memory.zig"));
    std.testing.refAllDecls(@import("signal_handler.zig"));
    std.testing.refAllDecls(@import("Scratch.zig"));
    std.testing.refAllDecls(@import("SExprTree.zig"));
    std.testing.refAllDecls(@import("SerialStringInterner.zig"));
    std.testing.refAllDecls(@import("SmallStringInterner.zig"));
    std.testing.refAllDecls(@import("source_utils.zig"));
    std.testing.refAllDecls(@import("stack_overflow.zig"));
    std.testing.refAllDecls(@import("StringLiteral.zig"));
    std.testing.refAllDecls(@import("target.zig"));
    std.testing.refAllDecls(@import("url.zig"));
}
