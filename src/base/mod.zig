//! Basic types that are useful throughout the compiler.
const std = @import("std");

pub const SExprTree = @import("SExprTree.zig");
pub const Ident = @import("Ident.zig");
pub const Region = @import("Region.zig");
pub const StringLiteral = @import("StringLiteral.zig");
pub const RegionInfo = @import("RegionInfo.zig");
pub const Scratch = @import("Scratch.zig").Scratch;
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

/// The core allocators for the lifetime of a roc program.
///
/// This structure should be used to pass allocators to most functions in Roc.
/// Data structures should anchor to a generic allocator instead (alloc: Allocator).
/// It is up to the instanciator of the data structure to pick what it will use.
/// Generally speaking though, data structures can realloc and will use the gpa.
///
/// IMPORTANT: After initialization, Allocators must always be passed by pointer (*Allocators),
/// never by value. Passing by value will invalidate the arena allocator pointer!
pub const Allocators = struct {
    /// The gpa is the general purpose allocator. Anything allocated with the gpa must be freed.
    /// the gpa should generally be used for large allocations and things that might get reallocated.
    /// It is best to avoid allocating small or short lived things with the gpa.
    gpa: std.mem.Allocator,

    /// The arena is an arena allocator that is around for the entire roc compilation.
    /// The arena should be used for small and miscellaneous allocations.
    /// Things allocated in arena are expected to never be freed individually.
    ///
    /// IMPORTANT: This field contains a pointer to arena_impl. The struct must not be
    /// moved after initialization, or this pointer will be invalidated.
    arena: std.mem.Allocator,

    /// The underlying arena allocator implementation (stored to enable deinit)
    arena_impl: std.heap.ArenaAllocator,

    // TODO: consider if we want to add scratch. It would be an arena reset between each compilation phase.
    // scratch: ?std.mem.Allocator,

    /// Initialize the Allocators in-place with a general purpose allocator.
    ///
    /// IMPORTANT: This struct must be initialized in its final memory location.
    /// After calling initInPlace(), the struct must only be passed by pointer (*Allocators),
    /// never by value, or the arena allocator pointer will be invalidated.
    pub fn initInPlace(self: *Allocators, gpa: std.mem.Allocator) void {
        self.* = .{
            .gpa = gpa,
            .arena = undefined,
            .arena_impl = std.heap.ArenaAllocator.init(gpa),
        };
        self.arena = self.arena_impl.allocator();
    }

    /// Deinitialize the arena allocator.
    pub fn deinit(self: *Allocators) void {
        self.arena_impl.deinit();
    }
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
