//! x86_64-specific code generation.
//!
//! This module provides x86_64-specific code generation including
//! function prologues/epilogues and instruction selection.

const std = @import("std");
const Allocator = std.mem.Allocator;

const Emit = @import("Emit.zig");
const Registers = @import("Registers.zig");
const SystemV = @import("SystemV.zig");
const WindowsFastcall = @import("WindowsFastcall.zig");
const Relocation = @import("../Relocation.zig").Relocation;
const GenericCodeGen = @import("../CodeGen.zig");

const GeneralReg = Registers.GeneralReg;
const FloatReg = Registers.FloatReg;
const RegisterWidth = Registers.RegisterWidth;

/// x86_64 code generator for System V ABI (Linux, macOS, BSD)
pub const SystemVCodeGen = struct {
    const Self = @This();
    const CC = SystemV;

    emit: Emit,
    allocator: Allocator,
    stack_offset: i32,
    relocations: std.ArrayList(Relocation),
    locals: std.AutoHashMap(u32, GenericCodeGen.ValueLoc),
    free_general: u32,
    free_float: u32,
    callee_saved_used: u16, // Bitmask of callee-saved regs we used

    pub fn init(allocator: Allocator) Self {
        return Self{
            .emit = Emit.init(allocator),
            .allocator = allocator,
            .stack_offset = 0,
            .relocations = .{},
            .locals = .{},
            .free_general = CC.CALLER_SAVED_GENERAL_MASK,
            .free_float = CC.CALLER_SAVED_FLOAT_MASK,
            .callee_saved_used = 0,
        };
    }

    pub fn deinit(self: *Self) void {
        self.emit.deinit();
        self.relocations.deinit(self.allocator);
        self.locals.deinit(self.allocator);
    }

    pub fn reset(self: *Self) void {
        self.emit.buf.clearRetainingCapacity();
        self.relocations.clearRetainingCapacity();
        self.locals.clearRetainingCapacity();
        self.stack_offset = 0;
        self.free_general = CC.CALLER_SAVED_GENERAL_MASK;
        self.free_float = CC.CALLER_SAVED_FLOAT_MASK;
        self.callee_saved_used = 0;
    }

    /// Get the generated code
    pub fn getCode(self: *Self) []const u8 {
        return self.emit.buf.items;
    }

    /// Get current code offset
    pub fn currentOffset(self: *Self) usize {
        return self.emit.buf.items.len;
    }

    // =========================================================================
    // Register allocation
    // =========================================================================

    pub fn allocGeneral(self: *Self) ?GeneralReg {
        if (self.free_general == 0) return null;
        const bit: u5 = @intCast(@ctz(self.free_general));
        self.free_general &= ~(@as(u32, 1) << bit);
        return @enumFromInt(bit);
    }

    pub fn freeGeneral(self: *Self, reg: GeneralReg) void {
        self.free_general |= @as(u32, 1) << @intFromEnum(reg);
    }

    pub fn allocFloat(self: *Self) ?FloatReg {
        if (self.free_float == 0) return null;
        const bit: u5 = @intCast(@ctz(self.free_float));
        self.free_float &= ~(@as(u32, 1) << bit);
        return @enumFromInt(bit);
    }

    pub fn freeFloat(self: *Self, reg: FloatReg) void {
        self.free_float |= @as(u32, 1) << @intFromEnum(reg);
    }

    // =========================================================================
    // Stack management
    // =========================================================================

    pub fn allocStack(self: *Self, size: u32) i32 {
        const aligned_size = (size + 7) & ~@as(u32, 7);
        self.stack_offset -= @intCast(aligned_size);
        return self.stack_offset;
    }

    pub fn getStackSize(self: *Self) u32 {
        const size: u32 = @intCast(-self.stack_offset);
        return (size + 15) & ~@as(u32, 15);
    }

    // =========================================================================
    // Function prologue/epilogue
    // =========================================================================

    /// Emit function prologue (called at start of function)
    pub fn emitPrologue(self: *Self) !void {
        // push rbp
        try self.emit.pushReg(.RBP);
        // mov rbp, rsp
        try self.emit.movRegReg(.w64, .RBP, .RSP);
    }

    /// Emit function epilogue and return
    pub fn emitEpilogue(self: *Self) !void {
        // mov rsp, rbp (restore stack pointer)
        try self.emit.movRegReg(.w64, .RSP, .RBP);
        // pop rbp
        try self.emit.popReg(.RBP);
        // ret
        try self.emit.ret();
    }

    /// Emit stack frame setup with given local size
    pub fn emitStackAlloc(self: *Self, size: u32) !void {
        if (size > 0) {
            // sub rsp, size
            try self.emit.subRegImm32(.w64, .RSP, @intCast(size));
        }
    }

    // =========================================================================
    // Integer operations
    // =========================================================================

    /// Emit integer addition: dst = a + b
    pub fn emitAdd(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        if (dst != a) {
            try self.emit.movRegReg(width, dst, a);
        }
        try self.emit.addRegReg(width, dst, b);
    }

    /// Emit integer subtraction: dst = a - b
    pub fn emitSub(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        if (dst != a) {
            try self.emit.movRegReg(width, dst, a);
        }
        try self.emit.subRegReg(width, dst, b);
    }

    /// Emit integer multiplication: dst = a * b
    pub fn emitMul(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        if (dst != a) {
            try self.emit.movRegReg(width, dst, a);
        }
        try self.emit.imulRegReg(width, dst, b);
    }

    /// Emit integer negation: dst = -src
    pub fn emitNeg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
        if (dst != src) {
            try self.emit.movRegReg(width, dst, src);
        }
        try self.emit.negReg(width, dst);
    }

    // =========================================================================
    // Comparison operations
    // =========================================================================

    /// Emit comparison and set condition: dst = (a op b) ? 1 : 0
    pub fn emitCmp(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg, cond: Emit.Condition) !void {
        try self.emit.cmpRegReg(width, a, b);
        try self.emit.setcc(cond, dst);
        // Zero-extend the byte result to full register width
        try self.emit.movRegReg(.w32, dst, dst); // movzx is implied for 32-bit moves
    }

    // =========================================================================
    // Floating-point operations
    // =========================================================================

    /// Emit float64 addition: dst = a + b
    pub fn emitAddF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        if (dst != a) {
            try self.emit.movsdRegReg(dst, a);
        }
        try self.emit.addsdRegReg(dst, b);
    }

    /// Emit float64 subtraction: dst = a - b
    pub fn emitSubF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        if (dst != a) {
            try self.emit.movsdRegReg(dst, a);
        }
        try self.emit.subsdRegReg(dst, b);
    }

    /// Emit float64 multiplication: dst = a * b
    pub fn emitMulF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        if (dst != a) {
            try self.emit.movsdRegReg(dst, a);
        }
        try self.emit.mulsdRegReg(dst, b);
    }

    /// Emit float64 division: dst = a / b
    pub fn emitDivF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        if (dst != a) {
            try self.emit.movsdRegReg(dst, a);
        }
        try self.emit.divsdRegReg(dst, b);
    }

    // =========================================================================
    // Memory operations
    // =========================================================================

    /// Load from stack slot into register
    pub fn emitLoadStack(self: *Self, width: RegisterWidth, dst: GeneralReg, offset: i32) !void {
        try self.emit.movRegMem(width, dst, .RBP, offset);
    }

    /// Store register to stack slot
    pub fn emitStoreStack(self: *Self, width: RegisterWidth, offset: i32, src: GeneralReg) !void {
        try self.emit.movMemReg(width, .RBP, offset, src);
    }

    /// Load float64 from stack slot
    pub fn emitLoadStackF64(self: *Self, dst: FloatReg, offset: i32) !void {
        try self.emit.movsdRegMem(dst, .RBP, offset);
    }

    /// Store float64 to stack slot
    pub fn emitStoreStackF64(self: *Self, offset: i32, src: FloatReg) !void {
        try self.emit.movsdMemReg(.RBP, offset, src);
    }

    // =========================================================================
    // Immediate loading
    // =========================================================================

    /// Load immediate value into register
    pub fn emitLoadImm(self: *Self, dst: GeneralReg, value: i64) !void {
        if (value >= std.math.minInt(i32) and value <= std.math.maxInt(i32)) {
            try self.emit.movRegImm32(dst, @intCast(value));
        } else {
            try self.emit.movRegImm64(dst, value);
        }
    }

    // =========================================================================
    // Control flow
    // =========================================================================

    /// Emit unconditional jump (returns patch location for fixup)
    pub fn emitJump(self: *Self) !usize {
        const patch_loc = self.currentOffset() + 1; // After opcode
        try self.emit.jmpRel32(0); // Placeholder offset
        return patch_loc;
    }

    /// Emit conditional jump (returns patch location for fixup)
    pub fn emitCondJump(self: *Self, cond: Emit.Condition) !usize {
        const patch_loc = self.currentOffset() + 2; // After 0F opcode and condition
        try self.emit.jccRel32(cond, 0); // Placeholder offset
        return patch_loc;
    }

    /// Patch a jump target
    pub fn patchJump(self: *Self, patch_loc: usize, target: usize) void {
        const offset: i32 = @intCast(@as(i64, @intCast(target)) - @as(i64, @intCast(patch_loc + 4)));
        const bytes: [4]u8 = @bitCast(offset);
        @memcpy(self.emit.buf.items[patch_loc..][0..4], &bytes);
    }

    /// Emit function call with relocation
    pub fn emitCall(self: *Self, name: []const u8) !void {
        const offset = self.currentOffset();
        try self.emit.callRel32(0); // Placeholder
        try self.relocations.append(.{
            .linked_function = .{
                .offset = @intCast(offset + 1), // After E8 opcode
                .name = name,
            },
        });
    }
};

// ============================================================================
// Tests
// ============================================================================

test "prologue and epilogue" {
    var cg = SystemVCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitPrologue();
    try cg.emitEpilogue();

    const code = cg.getCode();
    // push rbp: 55
    // mov rbp, rsp: 48 89 E5
    // mov rsp, rbp: 48 89 EC
    // pop rbp: 5D
    // ret: C3
    try std.testing.expectEqual(@as(usize, 10), code.len);
    try std.testing.expectEqual(@as(u8, 0x55), code[0]); // push rbp
    try std.testing.expectEqual(@as(u8, 0xC3), code[code.len - 1]); // ret
}

test "load immediate" {
    var cg = SystemVCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitLoadImm(.RAX, 42);
    const code = cg.getCode();
    try std.testing.expect(code.len > 0);
}

test "integer operations" {
    var cg = SystemVCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitAdd(.w64, .RAX, .RBX, .RCX);
    try cg.emitSub(.w64, .RDX, .RSI, .RDI);
    try cg.emitMul(.w64, .R8, .R9, .R10);

    try std.testing.expect(cg.getCode().len > 0);
}

test "float operations" {
    var cg = SystemVCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitAddF64(.XMM0, .XMM1, .XMM2);
    try cg.emitSubF64(.XMM3, .XMM4, .XMM5);
    try cg.emitMulF64(.XMM6, .XMM7, .XMM8);
    try cg.emitDivF64(.XMM9, .XMM10, .XMM11);

    try std.testing.expect(cg.getCode().len > 0);
}
