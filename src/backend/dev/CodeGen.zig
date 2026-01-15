//! Code generation for the dev backend.
//!
//! This module translates Canonical IR (CIR) expressions directly to
//! native machine code using the architecture-specific Emit modules.
//! Unlike the LLVM backend, this generates code directly without an
//! intermediate LLVM IR representation.

const std = @import("std");
const Allocator = std.mem.Allocator;

const Relocation = @import("Relocation.zig").Relocation;

/// Value location - where a computed value is stored
pub const ValueLoc = union(enum) {
    /// Value is in a general-purpose register
    general_reg: u8,
    /// Value is in a floating-point register
    float_reg: u8,
    /// Value is on the stack at offset from frame pointer
    stack: i32,
    /// Value is an immediate constant
    immediate: i64,
    /// Value is a floating-point immediate
    float_immediate: f64,
    /// Value is at a memory address (for large values)
    memory: struct {
        base_reg: u8,
        offset: i32,
    },
};

/// Numeric type kinds (matches CIR.NumKind)
pub const NumKind = enum {
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
    numeral,

    pub fn isFloat(self: NumKind) bool {
        return self == .f32 or self == .f64;
    }

    pub fn isSigned(self: NumKind) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .i128 => true,
            .u8, .u16, .u32, .u64, .u128 => false,
            .f32, .f64, .dec, .numeral => true, // floats are signed
        };
    }

    pub fn byteSize(self: NumKind) u8 {
        return switch (self) {
            .u8, .i8 => 1,
            .u16, .i16 => 2,
            .u32, .i32, .f32 => 4,
            .u64, .i64, .f64, .numeral => 8,
            .u128, .i128, .dec => 16,
        };
    }
};

/// Binary operation types
pub const BinOp = enum {
    add,
    sub,
    mul,
    div,
    rem,
    div_trunc,
    lt,
    gt,
    le,
    ge,
    eq,
    ne,
    @"and",
    @"or",

    pub fn isComparison(self: BinOp) bool {
        return switch (self) {
            .lt, .gt, .le, .ge, .eq, .ne => true,
            else => false,
        };
    }

    pub fn isLogical(self: BinOp) bool {
        return self == .@"and" or self == .@"or";
    }
};

/// Code generator that produces native machine code from high-level operations.
/// This is parameterized by the target architecture's types.
pub fn CodeGen(
    comptime GeneralReg: type,
    comptime FloatReg: type,
    comptime Emit: type,
    comptime CC: type,
) type {
    return struct {
        const Self = @This();

        /// The instruction emitter
        emit: Emit,

        /// Allocator for dynamic allocations
        allocator: Allocator,

        /// Stack frame offset (grows negative from RBP/FP)
        stack_offset: i32,

        /// Current stack alignment requirement
        stack_alignment: u32,

        /// Relocations needed for linking
        relocations: std.ArrayList(Relocation),

        /// Map from local variable indices to their locations
        locals: std.AutoHashMap(u32, ValueLoc),

        /// Free general-purpose registers (as a bitset)
        free_general: u32,

        /// Free floating-point registers (as a bitset)
        free_float: u32,

        /// Initialize a new code generator
        pub fn init(allocator: Allocator) Self {
            return Self{
                .emit = Emit.init(allocator),
                .allocator = allocator,
                .stack_offset = 0,
                .stack_alignment = 16, // Most ABIs require 16-byte alignment
                .relocations = .{},
                .locals = std.AutoHashMap(u32, ValueLoc).init(allocator),
                .free_general = CC.CALLER_SAVED_GENERAL_MASK,
                .free_float = CC.CALLER_SAVED_FLOAT_MASK,
            };
        }

        /// Clean up resources
        pub fn deinit(self: *Self) void {
            self.emit.deinit();
            self.relocations.deinit(self.allocator);
            self.locals.deinit();
        }

        /// Get the generated code
        pub fn getCode(self: *Self) []const u8 {
            return self.emit.buf.items;
        }

        /// Get the current code offset (for calculating relative jumps)
        pub fn currentOffset(self: *Self) usize {
            return self.emit.buf.items.len;
        }

        // Register allocation (simple linear scan)

        /// Allocate a general-purpose register
        pub fn allocGeneral(self: *Self) ?GeneralReg {
            if (self.free_general == 0) return null;
            const bit = @ctz(self.free_general);
            self.free_general &= ~(@as(u32, 1) << @intCast(bit));
            return @enumFromInt(bit);
        }

        /// Free a general-purpose register
        pub fn freeGeneral(self: *Self, reg: GeneralReg) void {
            self.free_general |= @as(u32, 1) << @intFromEnum(reg);
        }

        /// Allocate a floating-point register
        pub fn allocFloat(self: *Self) ?FloatReg {
            if (self.free_float == 0) return null;
            const bit = @ctz(self.free_float);
            self.free_float &= ~(@as(u32, 1) << @intCast(bit));
            return @enumFromInt(bit);
        }

        /// Free a floating-point register
        pub fn freeFloat(self: *Self, reg: FloatReg) void {
            self.free_float |= @as(u32, 1) << @intFromEnum(reg);
        }

        // Stack management

        /// Allocate space on the stack, returns offset from frame pointer
        pub fn allocStack(self: *Self, size: u32) i32 {
            // Align size to 8 bytes
            const aligned_size = (size + 7) & ~@as(u32, 7);
            self.stack_offset -= @intCast(aligned_size);
            return self.stack_offset;
        }

        /// Get total stack space used
        pub fn getStackSize(self: *Self) u32 {
            const size: u32 = @intCast(-self.stack_offset);
            // Align to 16 bytes for ABI compliance
            return (size + 15) & ~@as(u32, 15);
        }

        // Local variable management

        /// Bind a local variable to a location
        pub fn bindLocal(self: *Self, idx: u32, loc: ValueLoc) !void {
            try self.locals.put(idx, loc);
        }

        /// Look up a local variable's location
        pub fn getLocal(self: *Self, idx: u32) ?ValueLoc {
            return self.locals.get(idx);
        }

        // Code emission helpers (architecture-independent interface)

        /// Emit function prologue
        pub fn emitPrologue(_: *Self) !void {
            // This will be specialized per architecture
            @compileError("emitPrologue must be specialized for the target architecture");
        }

        /// Emit function epilogue and return
        pub fn emitEpilogue(_: *Self) !void {
            // This will be specialized per architecture
            @compileError("emitEpilogue must be specialized for the target architecture");
        }
    };
}

// Tests

test "NumKind properties" {
    try std.testing.expect(NumKind.f32.isFloat());
    try std.testing.expect(NumKind.f64.isFloat());
    try std.testing.expect(!NumKind.i32.isFloat());

    try std.testing.expect(NumKind.i32.isSigned());
    try std.testing.expect(!NumKind.u32.isSigned());

    try std.testing.expectEqual(@as(u8, 4), NumKind.i32.byteSize());
    try std.testing.expectEqual(@as(u8, 8), NumKind.i64.byteSize());
    try std.testing.expectEqual(@as(u8, 16), NumKind.i128.byteSize());
}

test "BinOp properties" {
    try std.testing.expect(BinOp.lt.isComparison());
    try std.testing.expect(BinOp.eq.isComparison());
    try std.testing.expect(!BinOp.add.isComparison());

    try std.testing.expect(BinOp.@"and".isLogical());
    try std.testing.expect(BinOp.@"or".isLogical());
    try std.testing.expect(!BinOp.add.isLogical());
}
