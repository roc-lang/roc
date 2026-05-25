//! Basic types that are useful throughout the compiler.
const std = @import("std");

pub const SExprTree = @import("SExprTree.zig");
pub const Ident = @import("Ident.zig");
pub const Region = @import("Region.zig");
pub const StringLiteral = @import("StringLiteral.zig");
pub const LowLevel = @import("LowLevel.zig").LowLevel;
pub const RegionInfo = @import("RegionInfo.zig");
pub const Scratch = @import("Scratch.zig").Scratch;
pub const parallel = @import("parallel.zig");
pub const SmallStringInterner = @import("SmallStringInterner.zig");

pub const safe_memory = @import("safe_memory.zig");
pub const signal_handler = @import("signal_handler.zig");
pub const stack_overflow = @import("stack_overflow.zig");

pub const target = @import("target.zig");
pub const DataSpan = @import("DataSpan.zig").DataSpan;
pub const PackedDataSpan = @import("PackedDataSpan.zig").PackedDataSpan;
pub const FunctionArgs = @import("PackedDataSpan.zig").FunctionArgs;
pub const SmallCollections = @import("PackedDataSpan.zig").SmallCollections;

pub const CommonEnv = @import("CommonEnv.zig");
pub const source_utils = @import("source_utils.zig");
pub const module_path = @import("module_path.zig");
pub const url = @import("url.zig");

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

/// Memory ownership policy for compiler phases.
pub const CompileMemoryMode = enum {
    /// One-shot compilation. Phase output memory lives until the compilation ends.
    batch,
    /// Incremental use. Phase output memory can be reclaimed module by module.
    module_owned,
};

/// Allocators for one compiler task.
///
/// Phase code should allocate through this explicit bundle instead of choosing
/// global allocators itself. The coordinator decides whether these allocators
/// point at worker-lifetime arenas, module-owned arenas, or ordinary allocators.
pub const TaskMemory = struct {
    /// Coordinator-owned data structures, worker results, and other control data.
    control: std.mem.Allocator,
    /// Data retained as compiler output after the current task returns.
    persistent: std.mem.Allocator,
    /// Temporary data that may be reset after the current task returns.
    scratch: std.mem.Allocator,
    /// Diagnostic data retained until reports are moved or rendered.
    reports: std.mem.Allocator,

    pub fn fromAllocator(allocator: std.mem.Allocator) TaskMemory {
        return .{
            .control = allocator,
            .persistent = allocator,
            .scratch = allocator,
            .reports = allocator,
        };
    }
};

/// Stable memory owned by one module.
///
/// This is the safe retained-data owner while phase internals still allocate
/// through `ModuleEnv.gpa`. The arena object must live at a stable address
/// because allocators handed to ModuleEnv point back to it.
pub const ModuleMemory = struct {
    arena_impl: std.heap.ArenaAllocator,

    pub fn init(backing: std.mem.Allocator) ModuleMemory {
        return .{
            .arena_impl = std.heap.ArenaAllocator.init(backing),
        };
    }

    pub fn allocator(self: *ModuleMemory) std.mem.Allocator {
        return self.arena_impl.allocator();
    }

    pub fn deinit(self: *ModuleMemory) void {
        self.arena_impl.deinit();
    }
};

/// Per-worker memory for batch compilation.
///
/// The scratch arena may be reset after each task. The persistent arena is
/// reserved for future batch paths that can prove retained data does not race
/// across workers; current coordinator-retained module data uses ModuleMemory.
pub const WorkerMemory = struct {
    control: std.mem.Allocator,
    persistent_impl: std.heap.ArenaAllocator,
    scratch_impl: std.heap.ArenaAllocator,

    pub fn init(control: std.mem.Allocator, backing: std.mem.Allocator) WorkerMemory {
        return .{
            .control = control,
            .persistent_impl = std.heap.ArenaAllocator.init(backing),
            .scratch_impl = std.heap.ArenaAllocator.init(backing),
        };
    }

    pub fn deinit(self: *WorkerMemory) void {
        self.scratch_impl.deinit();
        self.persistent_impl.deinit();
    }

    pub fn taskMemory(self: *WorkerMemory) TaskMemory {
        return .{
            .control = self.control,
            .persistent = self.persistent_impl.allocator(),
            .scratch = self.scratch_impl.allocator(),
            .reports = self.control,
        };
    }

    pub fn resetScratch(self: *WorkerMemory) void {
        _ = self.scratch_impl.reset(.retain_capacity);
    }
};

/// The legacy allocator bundle used by parser/canonicalization APIs.
///
/// New compiler pipeline code should receive `TaskMemory` first, then build this
/// adapter only at APIs that still require it.
///
/// IMPORTANT: After initialization, Allocators must always be passed by pointer
/// (*Allocators), never by value. Passing by value invalidates `arena` when this
/// struct owns its local arena.
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

    /// Temporary memory for phase-local work. Older call sites that only use
    /// `initInPlace` get the general allocator here until they are migrated.
    scratch: std.mem.Allocator,

    /// Whether this adapter owns `arena_impl`.
    owns_arena: bool,

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
            .scratch = gpa,
            .owns_arena = true,
        };
        self.arena = self.arena_impl.allocator();
    }

    /// Build an adapter over externally-owned task memory.
    pub fn initFromTaskMemory(self: *Allocators, memory: TaskMemory) void {
        self.* = .{
            .gpa = memory.persistent,
            .arena = memory.persistent,
            .arena_impl = undefined,
            .scratch = memory.scratch,
            .owns_arena = false,
        };
    }

    /// Deinitialize the locally-owned arena allocator, if any.
    pub fn deinit(self: *Allocators) void {
        if (self.owns_arena) {
            self.arena_impl.deinit();
        }
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
    std.testing.refAllDecls(@import("signal_handler.zig"));
    std.testing.refAllDecls(@import("Scratch.zig"));
    std.testing.refAllDecls(@import("SExprTree.zig"));
    std.testing.refAllDecls(@import("SmallStringInterner.zig"));
    std.testing.refAllDecls(@import("source_utils.zig"));
    std.testing.refAllDecls(@import("stack_overflow.zig"));
    std.testing.refAllDecls(@import("StringLiteral.zig"));
    std.testing.refAllDecls(@import("target.zig"));
    std.testing.refAllDecls(@import("url.zig"));
}
