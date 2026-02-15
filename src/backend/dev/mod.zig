//! Development backends for the Roc compiler.
//!
//! These backends generate native machine code directly without LLVM,
//! enabling fast compilation for development workflows.
//!
//! Supported architectures:
//! - x86_64: Linux (System V ABI), macOS (System V ABI), Windows (Fastcall)
//! - aarch64: Linux and macOS (AAPCS64)

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout = @import("layout");
const builtins = @import("builtins");

/// Backend selection for code evaluation
pub const EvalBackend = enum {
    dev,
    interpreter,
    // llvm, // Future: LLVM backend

    pub fn fromString(s: []const u8) ?EvalBackend {
        if (std.mem.eql(u8, s, "dev")) return .dev;
        if (std.mem.eql(u8, s, "interpreter")) return .interpreter;
        // if (std.mem.eql(u8, s, "llvm")) return .llvm;
        return null;
    }
};

pub const x86_64 = @import("x86_64/mod.zig");
pub const aarch64 = @import("aarch64/mod.zig");
pub const object = @import("object/mod.zig");
const relocation_mod = @import("Relocation.zig");
pub const Relocation = relocation_mod.Relocation;
pub const applyRelocations = relocation_mod.applyRelocations;
pub const SymbolResolver = relocation_mod.SymbolResolver;
pub const ValueStorage = @import("ValueStorage.zig");
pub const ObjectWriter = @import("ObjectWriter.zig");

/// Executable memory for running generated code. Uses OS-specific APIs not available on freestanding.
pub const ExecutableMemory = if (builtin.os.tag == .freestanding)
    void
else
    @import("ExecutableMemory.zig").ExecutableMemory;

// Static data interner for string literals and other static data
pub const StaticDataInterner = @import("StaticDataInterner.zig");

// MonoExprCodeGen - parameterized by RocTarget for cross-compilation support
const MonoExprCodeGenMod = @import("MonoExprCodeGen.zig");

// LirCodeGen - LIR-based code generator parameterized by RocTarget
/// LIR code generator parameterized by target
pub const LirCodeGenMod = @import("LirCodeGen.zig");

/// Mono IR code generator parameterized by target (use MonoExprCodeGen(target) to instantiate)
pub const MonoExprCodeGen = MonoExprCodeGenMod.MonoExprCodeGen;

/// Pre-instantiated MonoExprCodeGen for the host platform (the machine running the compiler)
pub const HostMonoExprCodeGen = MonoExprCodeGenMod.HostMonoExprCodeGen;

/// x86_64 Linux with glibc
pub const X64GlibcMonoExprCodeGen = MonoExprCodeGenMod.X64GlibcMonoExprCodeGen;
/// x86_64 Linux with musl
pub const X64MuslMonoExprCodeGen = MonoExprCodeGenMod.X64MuslMonoExprCodeGen;
/// x86_64 Windows
pub const X64WinMonoExprCodeGen = MonoExprCodeGenMod.X64WinMonoExprCodeGen;
/// x86_64 macOS
pub const X64MacMonoExprCodeGen = MonoExprCodeGenMod.X64MacMonoExprCodeGen;
/// ARM64 Linux with glibc
pub const Arm64GlibcMonoExprCodeGen = MonoExprCodeGenMod.Arm64GlibcMonoExprCodeGen;
/// ARM64 Linux with musl
pub const Arm64MuslMonoExprCodeGen = MonoExprCodeGenMod.Arm64MuslMonoExprCodeGen;
/// ARM64 Windows
pub const Arm64WinMonoExprCodeGen = MonoExprCodeGenMod.Arm64WinMonoExprCodeGen;
/// ARM64 macOS
pub const Arm64MacMonoExprCodeGen = MonoExprCodeGenMod.Arm64MacMonoExprCodeGen;
/// ARM64 Linux (generic)
pub const Arm64LinuxMonoExprCodeGen = MonoExprCodeGenMod.Arm64LinuxMonoExprCodeGen;
/// x86_64 FreeBSD
pub const X64FreebsdMonoExprCodeGen = MonoExprCodeGenMod.X64FreebsdMonoExprCodeGen;
/// x86_64 OpenBSD
pub const X64OpenbsdMonoExprCodeGen = MonoExprCodeGenMod.X64OpenbsdMonoExprCodeGen;
/// x86_64 NetBSD
pub const X64NetbsdMonoExprCodeGen = MonoExprCodeGenMod.X64NetbsdMonoExprCodeGen;
/// x86_64 Linux (generic)
pub const X64LinuxMonoExprCodeGen = MonoExprCodeGenMod.X64LinuxMonoExprCodeGen;
/// x86_64 ELF (generic)
pub const X64ElfMonoExprCodeGen = MonoExprCodeGenMod.X64ElfMonoExprCodeGen;

/// Object file compiler for generating object files from Mono IR.
/// Supports cross-compilation to any RocTarget.
/// Only available on non-freestanding targets (uses std.fs)
pub const ObjectFileCompiler = if (builtin.os.tag == .freestanding) void else @import("ObjectFileCompiler.zig").ObjectFileCompiler;
pub const Entrypoint = if (builtin.os.tag == .freestanding) void else @import("ObjectFileCompiler.zig").Entrypoint;
pub const CompilationResult = if (builtin.os.tag == .freestanding) void else @import("ObjectFileCompiler.zig").CompilationResult;

/// Generic development backend parameterized by architecture-specific types.
///
/// This struct provides the common code generation logic shared across
/// all 64-bit architectures. Architecture-specific behavior is provided
/// through the comptime type parameters.
pub fn DevBackend(
    comptime GeneralReg: type,
    comptime FloatReg: type,
) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        target: base.target.Target,

        /// Output buffer for generated machine code
        buf: std.ArrayList(u8),

        /// Relocations that need to be applied during linking
        relocs: std.ArrayList(Relocation),

        /// Storage manager for register allocation
        storage: Storage(GeneralReg, FloatReg),

        /// Layout store for type information
        layout_store: *layout.Store,

        pub fn init(
            allocator: std.mem.Allocator,
            target: base.target.Target,
            layout_store: *layout.Store,
        ) !Self {
            return Self{
                .allocator = allocator,
                .target = target,
                .buf = .{},
                .relocs = .{},
                .storage = Storage(GeneralReg, FloatReg).init(allocator),
                .layout_store = layout_store,
            };
        }

        pub fn deinit(self: *Self) void {
            self.buf.deinit(self.allocator);
            self.relocs.deinit(self.allocator);
            self.storage.deinit();
        }

        /// Reset the backend for generating a new procedure
        pub fn reset(self: *Self) void {
            self.buf.clearRetainingCapacity();
            self.relocs.clearRetainingCapacity();
            self.storage.reset();
        }

        /// Finalize the current procedure and return the generated code
        pub fn finalize(self: *Self) struct { code: []const u8, relocs: []const Relocation } {
            return .{
                .code = self.buf.items,
                .relocs = self.relocs.items,
            };
        }
    };
}

/// Storage manager for register allocation and symbol tracking.
///
/// Tracks which symbols are stored in registers vs on the stack,
/// manages register allocation and spilling.
///
/// Note: When spilling is implemented, this will need access to the Emit module
/// (to emit spill code) and CallConv (to know which registers are callee-saved).
/// For now, those are passed through DevBackend when needed.
pub fn Storage(
    comptime GeneralReg: type,
    comptime FloatReg: type,
) type {
    return struct {
        const Self = @This();

        /// Where a value is stored
        pub const Location = union(enum) {
            /// Value is in a general-purpose register
            general_reg: GeneralReg,
            /// Value is in a floating-point register
            float_reg: FloatReg,
            /// Value is on the stack at the given offset from base pointer
            stack: i32,
            /// Value has no runtime representation (zero-sized type)
            no_data: void,
        };

        allocator: std.mem.Allocator,

        /// Map from symbol to its storage location
        symbol_storage: std.AutoHashMap(u32, Location),

        /// Free general-purpose registers (available for allocation)
        general_free: std.ArrayList(GeneralReg),

        /// Free floating-point registers
        float_free: std.ArrayList(FloatReg),

        /// Current stack frame size
        stack_size: u32,

        pub fn init(allocator: std.mem.Allocator) Self {
            return Self{
                .allocator = allocator,
                .symbol_storage = std.AutoHashMap(u32, Location).init(allocator),
                .general_free = .{},
                .float_free = .{},
                .stack_size = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            self.symbol_storage.deinit();
            self.general_free.deinit(self.allocator);
            self.float_free.deinit(self.allocator);
        }

        pub fn reset(self: *Self) void {
            self.symbol_storage.clearRetainingCapacity();
            self.general_free.clearRetainingCapacity();
            self.float_free.clearRetainingCapacity();
            self.stack_size = 0;
        }

        /// Claim a general-purpose register for a symbol.
        /// Panics if no registers are free (spilling not yet implemented).
        pub fn claimGeneralReg(self: *Self, symbol: u32) !GeneralReg {
            const reg = self.general_free.popOrNull() orelse
                @panic("No free general registers - spilling not implemented");
            try self.symbol_storage.put(symbol, .{ .general_reg = reg });
            return reg;
        }

        /// Claim a floating-point register for a symbol.
        /// Panics if no registers are free (spilling not yet implemented).
        pub fn claimFloatReg(self: *Self, symbol: u32) !FloatReg {
            const reg = self.float_free.popOrNull() orelse
                @panic("No free float registers - spilling not implemented");
            try self.symbol_storage.put(symbol, .{ .float_reg = reg });
            return reg;
        }

        /// Free the storage for a symbol (when it's no longer needed)
        pub fn freeSymbol(self: *Self, symbol: u32) !void {
            if (self.symbol_storage.fetchRemove(symbol)) |entry| {
                switch (entry.value) {
                    .general_reg => |reg| try self.general_free.append(self.allocator, reg),
                    .float_reg => |reg| try self.float_free.append(self.allocator, reg),
                    // Stack slots are reclaimed on function return; no_data has nothing to free
                    .stack, .no_data => {},
                }
            }
        }

        /// Allocate space on the stack
        pub fn allocStack(self: *Self, size: u32, alignment: u32) i32 {
            // Align the stack size
            self.stack_size = std.mem.alignForward(u32, self.stack_size, alignment);
            self.stack_size += size;
            // Return negative offset from base pointer
            return -@as(i32, @intCast(self.stack_size));
        }
    };
}

/// Dev backend for x86_64 Linux using System V ABI.
pub const X86_64LinuxBackend = DevBackend(
    x86_64.GeneralReg,
    x86_64.FloatReg,
);

/// Dev backend for x86_64 macOS (uses System V ABI).
pub const X86_64MacBackend = X86_64LinuxBackend;

/// Dev backend for x86_64 Windows using Fastcall ABI.
pub const X86_64WinBackend = DevBackend(
    x86_64.GeneralReg,
    x86_64.FloatReg,
);

/// Dev backend for aarch64 using AAPCS64 calling convention.
pub const AArch64Backend = DevBackend(
    aarch64.GeneralReg,
    aarch64.FloatReg,
);

/// Resolve builtin function names to their addresses.
/// This is used by ExecutableMemory to patch function call relocations.
///
/// Supported function names:
/// - "incref_data_ptr" -> increfDataPtrC
/// - "decref_data_ptr" -> decrefDataPtrC
/// - "free_data_ptr" -> freeDataPtrC
pub fn resolveBuiltinFunction(name: []const u8) ?usize {
    const utils = builtins.utils;

    if (std.mem.eql(u8, name, "incref_data_ptr")) {
        return @intFromPtr(&utils.increfDataPtrC);
    }
    if (std.mem.eql(u8, name, "decref_data_ptr")) {
        return @intFromPtr(&utils.decrefDataPtrC);
    }
    if (std.mem.eql(u8, name, "free_data_ptr")) {
        return @intFromPtr(&utils.freeDataPtrC);
    }
    // RC pointer functions (for direct refcount manipulation)
    if (std.mem.eql(u8, name, "incref_rc_ptr")) {
        return @intFromPtr(&utils.increfRcPtrC);
    }
    if (std.mem.eql(u8, name, "decref_rc_ptr")) {
        return @intFromPtr(&utils.decrefRcPtrC);
    }

    return null;
}

test "backend module imports" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("CallingConvention.zig"));
    std.testing.refAllDecls(@import("FrameBuilder.zig"));
}

test "resolve builtin functions" {
    // Test that all RC functions resolve
    try std.testing.expect(resolveBuiltinFunction("incref_data_ptr") != null);
    try std.testing.expect(resolveBuiltinFunction("decref_data_ptr") != null);
    try std.testing.expect(resolveBuiltinFunction("free_data_ptr") != null);
    try std.testing.expect(resolveBuiltinFunction("incref_rc_ptr") != null);
    try std.testing.expect(resolveBuiltinFunction("decref_rc_ptr") != null);

    // Test unknown function returns null
    try std.testing.expect(resolveBuiltinFunction("unknown_func") == null);
}
