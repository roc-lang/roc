//! aarch64-specific code generation.
//!
//! This module provides aarch64-specific code generation including
//! function prologues/epilogues and instruction selection.

const std = @import("std");
const Allocator = std.mem.Allocator;

const Emit = @import("Emit.zig");
const Registers = @import("Registers.zig");
const Call = @import("Call.zig");
const Relocation = @import("../Relocation.zig").Relocation;
const GenericCodeGen = @import("../CodeGen.zig");

const GeneralReg = Registers.GeneralReg;
const FloatReg = Registers.FloatReg;
const RegisterWidth = Registers.RegisterWidth;

/// aarch64 code generator for AAPCS64 (Linux, macOS)
pub const AArch64CodeGen = struct {
    const Self = @This();
    const CC = Call;

    emit: Emit,
    allocator: Allocator,
    stack_offset: i32,
    relocations: std.ArrayList(Relocation),
    locals: std.AutoHashMap(u32, GenericCodeGen.ValueLoc),
    free_general: u32,
    free_float: u32,
    callee_saved_used: u32, // Bitmask of callee-saved regs we used

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
        const aligned_size = (size + 15) & ~@as(u32, 15); // 16-byte align
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
        // stp x29, x30, [sp, #-16]! (pre-index: push FP and LR)
        try self.emit.stpPreIndex(.w64, .FP, .LR, .ZRSP, -2);
        // mov x29, sp (set frame pointer)
        try self.emit.movRegReg(.w64, .FP, .ZRSP);
    }

    /// Emit function epilogue and return
    pub fn emitEpilogue(self: *Self) !void {
        // mov sp, x29 (restore stack pointer) - not needed if we use ldp post-index
        // ldp x29, x30, [sp], #16 (post-index: pop FP and LR)
        try self.emit.ldpPostIndex(.w64, .FP, .LR, .ZRSP, 2);
        // ret
        try self.emit.ret();
    }

    /// Emit stack frame setup with given local size
    pub fn emitStackAlloc(self: *Self, size: u32) !void {
        if (size > 0) {
            // sub sp, sp, #size
            if (size <= 4095) {
                try self.emit.subRegRegImm12(.w64, .ZRSP, .ZRSP, @intCast(size));
            } else {
                // For larger sizes, need to load immediate first
                try self.emit.movRegImm64(.IP0, size);
                try self.emit.subRegRegReg(.w64, .ZRSP, .ZRSP, .IP0);
            }
        }
    }

    // =========================================================================
    // Integer operations
    // =========================================================================

    /// Emit integer addition: dst = a + b
    pub fn emitAdd(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        try self.emit.addRegRegReg(width, dst, a, b);
    }

    /// Emit integer subtraction: dst = a - b
    pub fn emitSub(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        try self.emit.subRegRegReg(width, dst, a, b);
    }

    /// Emit integer multiplication: dst = a * b
    pub fn emitMul(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        try self.emit.mulRegRegReg(width, dst, a, b);
    }

    /// Emit integer negation: dst = -src
    pub fn emitNeg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
        try self.emit.negRegReg(width, dst, src);
    }

    // =========================================================================
    // Comparison operations
    // =========================================================================

    /// Emit comparison and set condition: dst = (a op b) ? 1 : 0
    pub fn emitCmp(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg, cond: Emit.Condition) !void {
        try self.emit.cmpRegReg(width, a, b);
        try self.emit.cset(width, dst, cond);
    }

    // =========================================================================
    // Floating-point operations
    // =========================================================================

    /// Emit float64 addition: dst = a + b
    pub fn emitAddF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        try self.emit.faddRegRegReg(.double, dst, a, b);
    }

    /// Emit float64 subtraction: dst = a - b
    pub fn emitSubF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        try self.emit.fsubRegRegReg(.double, dst, a, b);
    }

    /// Emit float64 multiplication: dst = a * b
    pub fn emitMulF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        try self.emit.fmulRegRegReg(.double, dst, a, b);
    }

    /// Emit float64 division: dst = a / b
    pub fn emitDivF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        try self.emit.fdivRegRegReg(.double, dst, a, b);
    }

    // =========================================================================
    // Memory operations
    // =========================================================================

    /// Load from stack slot into register
    pub fn emitLoadStack(self: *Self, width: RegisterWidth, dst: GeneralReg, offset: i32) !void {
        if (offset >= -256 and offset <= 255) {
            try self.emit.ldurRegMem(width, dst, .FP, @intCast(offset));
        } else {
            // For larger offsets, need scaled unsigned offset
            const uoffset: u12 = @intCast(@as(u32, @bitCast(offset)) >> (if (width == .w64) 3 else 2));
            try self.emit.ldrRegMemUoff(width, dst, .FP, uoffset);
        }
    }

    /// Store register to stack slot
    pub fn emitStoreStack(self: *Self, width: RegisterWidth, offset: i32, src: GeneralReg) !void {
        if (offset >= -256 and offset <= 255) {
            try self.emit.sturRegMem(width, src, .FP, @intCast(offset));
        } else {
            const uoffset: u12 = @intCast(@as(u32, @bitCast(offset)) >> (if (width == .w64) 3 else 2));
            try self.emit.strRegMemUoff(width, src, .FP, uoffset);
        }
    }

    /// Load float64 from stack slot
    pub fn emitLoadStackF64(self: *Self, dst: FloatReg, offset: i32) !void {
        // Use unsigned offset form (scaled by 8 for 64-bit)
        const uoffset: u12 = @intCast(@as(u32, @bitCast(offset)) >> 3);
        try self.emit.fldrRegMemUoff(.double, dst, .FP, uoffset);
    }

    /// Store float64 to stack slot
    pub fn emitStoreStackF64(self: *Self, offset: i32, src: FloatReg) !void {
        const uoffset: u12 = @intCast(@as(u32, @bitCast(offset)) >> 3);
        try self.emit.fstrRegMemUoff(.double, src, .FP, uoffset);
    }

    // =========================================================================
    // Immediate loading
    // =========================================================================

    /// Load immediate value into register
    pub fn emitLoadImm(self: *Self, dst: GeneralReg, value: i64) !void {
        try self.emit.movRegImm64(dst, @bitCast(value));
    }

    // =========================================================================
    // Control flow
    // =========================================================================

    /// Emit unconditional jump (returns patch location for fixup)
    pub fn emitJump(self: *Self) !usize {
        const patch_loc = self.currentOffset();
        try self.emit.b(0); // Placeholder offset
        return patch_loc;
    }

    /// Emit conditional jump (returns patch location for fixup)
    pub fn emitCondJump(self: *Self, cond: Emit.Condition) !usize {
        const patch_loc = self.currentOffset();
        try self.emit.bcond(cond, 0); // Placeholder offset
        return patch_loc;
    }

    /// Patch a jump target
    pub fn patchJump(self: *Self, patch_loc: usize, target: usize) void {
        const offset: i32 = @intCast(@as(i64, @intCast(target)) - @as(i64, @intCast(patch_loc)));
        const offset_words = @divExact(offset, 4);

        // Read existing instruction
        var inst: u32 = std.mem.readInt(u32, self.emit.buf.items[patch_loc..][0..4], .little);

        // Determine instruction type and patch accordingly
        if ((inst >> 26) == 0b000101) {
            // B (unconditional): imm26 in bits [25:0]
            const imm26: u26 = @bitCast(@as(i26, @truncate(offset_words)));
            inst = (inst & 0xFC000000) | imm26;
        } else if ((inst >> 24) == 0b01010100) {
            // B.cond: imm19 in bits [23:5]
            const imm19: u19 = @bitCast(@as(i19, @truncate(offset_words)));
            inst = (inst & 0xFF00001F) | (@as(u32, imm19) << 5);
        }

        std.mem.writeInt(u32, self.emit.buf.items[patch_loc..][0..4], inst, .little);
    }

    /// Emit function call with relocation
    pub fn emitCall(self: *Self, name: []const u8) !void {
        const offset = self.currentOffset();
        try self.emit.bl(0); // Placeholder
        try self.relocations.append(.{
            .linked_function = .{
                .offset = @intCast(offset),
                .name = name,
            },
        });
    }
};

// ============================================================================
// Tests
// ============================================================================

test "prologue and epilogue" {
    var cg = AArch64CodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitPrologue();
    try cg.emitEpilogue();

    const code = cg.getCode();
    // stp x29, x30, [sp, #-16]! (4 bytes)
    // mov x29, sp (4 bytes) - actually orr x29, xzr, sp
    // ldp x29, x30, [sp], #16 (4 bytes)
    // ret (4 bytes)
    try std.testing.expectEqual(@as(usize, 16), code.len);
}

test "load immediate" {
    var cg = AArch64CodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitLoadImm(.X0, 42);
    const code = cg.getCode();
    try std.testing.expect(code.len > 0);
}

test "integer operations" {
    var cg = AArch64CodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitAdd(.w64, .X0, .X1, .X2);
    try cg.emitSub(.w64, .X3, .X4, .X5);
    try cg.emitMul(.w64, .X6, .X7, .XR);

    try std.testing.expect(cg.getCode().len > 0);
}

test "float operations" {
    var cg = AArch64CodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitAddF64(.V0, .V1, .V2);
    try cg.emitSubF64(.V3, .V4, .V5);
    try cg.emitMulF64(.V6, .V7, .V8);
    try cg.emitDivF64(.V9, .V10, .V11);

    try std.testing.expect(cg.getCode().len > 0);
}
