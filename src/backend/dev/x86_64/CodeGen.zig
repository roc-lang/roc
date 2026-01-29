//! x86_64-specific code generation.
//!
//! This module provides x86_64-specific code generation including
//! function prologues/epilogues and instruction selection.

const std = @import("std");
const Allocator = std.mem.Allocator;

const Emit = @import("Emit.zig");
const Registers = @import("Registers.zig");
const SystemV = @import("SystemV.zig");
const Relocation = @import("../Relocation.zig").Relocation;
const GenericCodeGen = @import("../CodeGen.zig");

const GeneralReg = Registers.GeneralReg;
const FloatReg = Registers.FloatReg;
const RegisterWidth = Registers.RegisterWidth;

/// x86_64 code generator for System V ABI (Linux, macOS, BSD)
pub const SystemVCodeGen = struct {
    const Self = @This();
    const CC = SystemV;

    /// Number of general-purpose registers
    const NUM_GENERAL_REGS = 16;
    /// Number of float registers
    const NUM_FLOAT_REGS = 16;

    /// Bitmask of callee-saved general registers available for allocation
    /// RBX, R12, R13, R14, R15 (not RBP - it's the frame pointer)
    const CALLEE_SAVED_GENERAL_MASK: u32 =
        (1 << @intFromEnum(GeneralReg.RBX)) |
        (1 << @intFromEnum(GeneralReg.R12)) |
        (1 << @intFromEnum(GeneralReg.R13)) |
        (1 << @intFromEnum(GeneralReg.R14)) |
        (1 << @intFromEnum(GeneralReg.R15));

    emit: Emit,
    allocator: Allocator,
    stack_offset: i32,
    relocations: std.ArrayList(Relocation),
    locals: std.AutoHashMap(u32, GenericCodeGen.ValueLoc),
    free_general: u32,
    free_float: u32,
    callee_saved_used: u16, // Bitmask of callee-saved regs we used

    /// Remaining callee-saved registers available (used after caller-saved exhausted)
    callee_saved_available: u32,

    /// Reverse mapping: register index -> local that owns it (null if free)
    general_owners: [NUM_GENERAL_REGS]?u32,
    float_owners: [NUM_FLOAT_REGS]?u32,

    pub fn init(allocator: Allocator) Self {
        return Self{
            .emit = Emit.init(allocator),
            .allocator = allocator,
            .stack_offset = 0,
            .relocations = .{},
            .locals = std.AutoHashMap(u32, GenericCodeGen.ValueLoc).init(allocator),
            .free_general = CC.CALLER_SAVED_GENERAL_MASK,
            .free_float = CC.CALLER_SAVED_FLOAT_MASK,
            .callee_saved_used = 0,
            .callee_saved_available = CALLEE_SAVED_GENERAL_MASK,
            .general_owners = [_]?u32{null} ** NUM_GENERAL_REGS,
            .float_owners = [_]?u32{null} ** NUM_FLOAT_REGS,
        };
    }

    pub fn deinit(self: *Self) void {
        self.emit.deinit();
        self.relocations.deinit(self.allocator);
        self.locals.deinit();
    }

    pub fn reset(self: *Self) void {
        self.emit.buf.clearRetainingCapacity();
        self.relocations.clearRetainingCapacity();
        self.locals.clearRetainingCapacity();
        self.stack_offset = 0;
        self.free_general = CC.CALLER_SAVED_GENERAL_MASK;
        self.free_float = CC.CALLER_SAVED_FLOAT_MASK;
        self.callee_saved_used = 0;
        self.callee_saved_available = CALLEE_SAVED_GENERAL_MASK;
        self.general_owners = [_]?u32{null} ** NUM_GENERAL_REGS;
        self.float_owners = [_]?u32{null} ** NUM_FLOAT_REGS;
    }

    /// Get the generated code
    pub fn getCode(self: *Self) []const u8 {
        return self.emit.buf.items;
    }

    /// Get current code offset
    pub fn currentOffset(self: *Self) usize {
        return self.emit.buf.items.len;
    }

    // Register allocation with spilling support

    /// Allocate a general-purpose register for a local variable.
    /// This will try caller-saved registers first, then callee-saved,
    /// and finally spill an existing register if all are in use.
    pub fn allocGeneralFor(self: *Self, local: u32) !GeneralReg {
        // 1. Try caller-saved registers first (preferred - no save/restore needed)
        if (self.allocFromGeneralMask(&self.free_general)) |reg| {
            self.general_owners[@intFromEnum(reg)] = local;
            return reg;
        }

        // 2. Try callee-saved registers (will need save/restore in prologue/epilogue)
        if (self.allocFromGeneralMask(&self.callee_saved_available)) |reg| {
            self.callee_saved_used |= @as(u16, 1) << @intCast(@intFromEnum(reg));
            self.general_owners[@intFromEnum(reg)] = local;
            return reg;
        }

        // 3. All registers in use - must spill one
        return self.spillAndAllocGeneral(local);
    }

    /// Allocate a general-purpose register without associating it with a local.
    /// Returns null if no registers available. Use allocGeneralFor for spill support.
    pub fn allocGeneral(self: *Self) ?GeneralReg {
        // Try caller-saved first
        if (self.allocFromGeneralMask(&self.free_general)) |reg| {
            return reg;
        }
        // Try callee-saved
        if (self.allocFromGeneralMask(&self.callee_saved_available)) |reg| {
            self.callee_saved_used |= @as(u16, 1) << @intCast(@intFromEnum(reg));
            return reg;
        }
        return null;
    }

    fn allocFromGeneralMask(_: *Self, mask: *u32) ?GeneralReg {
        if (mask.* == 0) return null;
        const bit: u5 = @intCast(@ctz(mask.*));
        mask.* &= ~(@as(u32, 1) << bit);
        return @enumFromInt(bit);
    }

    /// Free a general-purpose register, making it available for allocation.
    pub fn freeGeneral(self: *Self, reg: GeneralReg) void {
        const idx = @intFromEnum(reg);
        // Clear ownership
        self.general_owners[idx] = null;
        // Return to appropriate pool
        if ((CALLEE_SAVED_GENERAL_MASK & (@as(u32, 1) << idx)) != 0) {
            self.callee_saved_available |= @as(u32, 1) << idx;
        } else {
            self.free_general |= @as(u32, 1) << idx;
        }
    }

    /// Mark a register as in use so it won't be allocated.
    /// Used for return values from function calls that need to persist.
    pub fn markRegisterInUse(self: *Self, reg: GeneralReg) void {
        const idx = @intFromEnum(reg);
        // Remove from free pool (it's now in use)
        self.free_general &= ~(@as(u16, 1) << @intCast(idx));
        self.callee_saved_available &= ~(@as(u16, 1) << @intCast(idx));
        // Set ownership to a sentinel value (0 = temporary)
        self.general_owners[idx] = 0;
    }

    /// Spill a register to make room and allocate it for the given local.
    fn spillAndAllocGeneral(self: *Self, local: u32) !GeneralReg {
        // Find a register to spill - prefer lowest-numbered for consistency
        // Skip RSP and RBP as they're special
        var victim: ?GeneralReg = null;
        for (0..NUM_GENERAL_REGS) |i| {
            const reg: GeneralReg = @enumFromInt(i);
            // Skip stack/frame pointers
            if (reg == .RSP or reg == .RBP) continue;
            // Skip registers we don't own (they're free)
            if (self.general_owners[i] != null) {
                victim = reg;
                break;
            }
        }

        const reg = victim orelse return error.NoRegisterToSpill;
        const owner = self.general_owners[@intFromEnum(reg)].?;

        // Allocate stack slot for the spilled value
        const slot = self.allocStack(8);

        // Emit store instruction
        try self.emitStoreStack(.w64, slot, reg);

        // Update the owner's location to stack
        try self.locals.put(owner, .{ .stack = slot });

        // Clear old ownership, set new ownership
        self.general_owners[@intFromEnum(reg)] = local;

        return reg;
    }

    /// Reload a spilled value back into a register.
    /// Returns the register it was loaded into.
    pub fn reloadLocal(self: *Self, local: u32) !GeneralReg {
        // Check if it's already in a register
        if (self.locals.get(local)) |loc| {
            switch (loc) {
                .general_reg => |r| return @enumFromInt(r),
                .stack => |slot| {
                    // Allocate a register (might cause another spill)
                    const reg = try self.allocGeneralFor(local);
                    // Load from stack
                    try self.emitLoadStack(.w64, reg, slot);
                    // Update location
                    try self.locals.put(local, .{ .general_reg = @intFromEnum(reg) });
                    return reg;
                },
                else => return error.InvalidLocalLocation,
            }
        }
        return error.LocalNotFound;
    }

    /// Allocate a floating-point register for a local variable.
    pub fn allocFloatFor(self: *Self, local: u32) !FloatReg {
        // Float registers: try caller-saved first (all XMM are caller-saved in System V)
        if (self.allocFromFloatMask(&self.free_float)) |reg| {
            self.float_owners[@intFromEnum(reg)] = local;
            return reg;
        }

        // All registers in use - must spill one
        return self.spillAndAllocFloat(local);
    }

    pub fn allocFloat(self: *Self) ?FloatReg {
        if (self.free_float == 0) return null;
        const bit: u5 = @intCast(@ctz(self.free_float));
        self.free_float &= ~(@as(u32, 1) << bit);
        return @enumFromInt(bit);
    }

    fn allocFromFloatMask(_: *Self, mask: *u32) ?FloatReg {
        if (mask.* == 0) return null;
        const bit: u5 = @intCast(@ctz(mask.*));
        mask.* &= ~(@as(u32, 1) << bit);
        return @enumFromInt(bit);
    }

    pub fn freeFloat(self: *Self, reg: FloatReg) void {
        const idx = @intFromEnum(reg);
        self.float_owners[idx] = null;
        self.free_float |= @as(u32, 1) << idx;
    }

    fn spillAndAllocFloat(self: *Self, local: u32) !FloatReg {
        // Find a float register to spill
        var victim: ?FloatReg = null;
        for (0..NUM_FLOAT_REGS) |i| {
            if (self.float_owners[i] != null) {
                victim = @enumFromInt(i);
                break;
            }
        }

        const reg = victim orelse return error.NoRegisterToSpill;
        const owner = self.float_owners[@intFromEnum(reg)].?;

        // Allocate stack slot (8 bytes for f64)
        const slot = self.allocStack(8);

        // Emit store instruction
        try self.emitStoreStackF64(slot, reg);

        // Update the owner's location to stack
        try self.locals.put(owner, .{ .stack = slot });

        // Update ownership
        self.float_owners[@intFromEnum(reg)] = local;

        return reg;
    }

    /// Reload a spilled float value back into a register.
    pub fn reloadFloatLocal(self: *Self, local: u32) !FloatReg {
        if (self.locals.get(local)) |loc| {
            switch (loc) {
                .float_reg => |r| return @enumFromInt(r),
                .stack => |slot| {
                    const reg = try self.allocFloatFor(local);
                    try self.emitLoadStackF64(reg, slot);
                    try self.locals.put(local, .{ .float_reg = @intFromEnum(reg) });
                    return reg;
                },
                else => return error.InvalidLocalLocation,
            }
        }
        return error.LocalNotFound;
    }

    /// Get the register holding a local, reloading if necessary.
    pub fn getLocalReg(self: *Self, local: u32) !GeneralReg {
        if (self.locals.get(local)) |loc| {
            switch (loc) {
                .general_reg => |r| return @enumFromInt(r),
                .stack => return self.reloadLocal(local),
                else => return error.InvalidLocalLocation,
            }
        }
        return error.LocalNotFound;
    }

    /// Get the float register holding a local, reloading if necessary.
    pub fn getLocalFloatReg(self: *Self, local: u32) !FloatReg {
        if (self.locals.get(local)) |loc| {
            switch (loc) {
                .float_reg => |r| return @enumFromInt(r),
                .stack => return self.reloadFloatLocal(local),
                else => return error.InvalidLocalLocation,
            }
        }
        return error.LocalNotFound;
    }

    // Stack management

    pub fn allocStack(self: *Self, size: u32) i32 {
        const aligned_size = (size + 7) & ~@as(u32, 7);
        self.stack_offset -= @intCast(aligned_size);
        return self.stack_offset;
    }

    /// Alias for allocStack - allocate a stack slot of the given size
    pub fn allocStackSlot(self: *Self, size: u32) i32 {
        return self.allocStack(size);
    }

    pub fn getStackSize(self: *Self) u32 {
        const size: u32 = @intCast(-self.stack_offset);
        return (size + 15) & ~@as(u32, 15);
    }

    // Function prologue/epilogue

    /// Callee-saved registers in the order they should be saved/restored
    const CALLEE_SAVED_ORDER = [_]GeneralReg{ .RBX, .R12, .R13, .R14, .R15 };

    /// Emit function prologue (called at start of function)
    /// Note: Call this AFTER register allocation is complete to know which
    /// callee-saved registers need to be preserved.
    pub fn emitPrologue(self: *Self) !void {
        // push rbp
        try self.emit.pushReg(.RBP);
        // mov rbp, rsp
        try self.emit.movRegReg(.w64, .RBP, .RSP);

        // Push any callee-saved registers we used
        for (CALLEE_SAVED_ORDER) |reg| {
            if ((self.callee_saved_used & (@as(u16, 1) << @intCast(@intFromEnum(reg)))) != 0) {
                try self.emit.pushReg(reg);
            }
        }
    }

    /// Emit function epilogue and return
    pub fn emitEpilogue(self: *Self) !void {
        // Pop callee-saved registers in reverse order
        var i: usize = CALLEE_SAVED_ORDER.len;
        while (i > 0) {
            i -= 1;
            const reg = CALLEE_SAVED_ORDER[i];
            if ((self.callee_saved_used & (@as(u16, 1) << @intCast(@intFromEnum(reg)))) != 0) {
                try self.emit.popReg(reg);
            }
        }

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

    // Integer operations

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

    /// Emit signed integer division: dst = a / b
    /// Uses IDIV which requires dividend in RDX:RAX, result in RAX
    pub fn emitSDiv(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        // IDIV uses RAX for dividend/quotient and RDX for high bits/remainder
        // IMPORTANT: Save b to R11 BEFORE moving a to RAX, in case b is in RAX
        const divisor_reg = if (b == .RAX or b == .RDX) blk: {
            try self.emit.movRegReg(width, .R11, b);
            break :blk .R11;
        } else b;
        // 1. Move dividend to RAX
        if (a != .RAX) {
            try self.emit.movRegReg(width, .RAX, a);
        }
        // 2. Sign-extend RAX into RDX:RAX
        if (width == .w64) {
            try self.emit.cqo();
        } else {
            try self.emit.cdq();
        }
        // 3. Perform IDIV
        try self.emit.idivReg(width, divisor_reg);
        // 4. Quotient is in RAX, move to dst
        if (dst != .RAX) {
            try self.emit.movRegReg(width, dst, .RAX);
        }
    }

    /// Emit unsigned integer division: dst = a / b
    /// Uses DIV which requires dividend in RDX:RAX, result in RAX
    pub fn emitUDiv(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        // DIV uses RAX for dividend/quotient and RDX for high bits/remainder
        // IMPORTANT: Save b to R11 BEFORE moving a to RAX, in case b is in RAX
        const divisor_reg = if (b == .RAX or b == .RDX) blk: {
            try self.emit.movRegReg(width, .R11, b);
            break :blk .R11;
        } else b;
        // 1. Move dividend to RAX
        if (a != .RAX) {
            try self.emit.movRegReg(width, .RAX, a);
        }
        // 2. Zero-extend: set RDX to 0
        try self.emit.xorRegReg(width, .RDX, .RDX);
        // 3. Perform DIV
        try self.emit.divReg(width, divisor_reg);
        // 4. Quotient is in RAX, move to dst
        if (dst != .RAX) {
            try self.emit.movRegReg(width, dst, .RAX);
        }
    }

    /// Emit signed integer modulo: dst = a % b
    /// Uses IDIV which puts remainder in RDX
    pub fn emitSMod(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        // IMPORTANT: Save b to R11 BEFORE moving a to RAX, in case b is in RAX
        const divisor_reg = if (b == .RAX or b == .RDX) blk: {
            try self.emit.movRegReg(width, .R11, b);
            break :blk .R11;
        } else b;
        // 1. Move dividend to RAX
        if (a != .RAX) {
            try self.emit.movRegReg(width, .RAX, a);
        }
        // 2. Sign-extend RAX into RDX:RAX
        if (width == .w64) {
            try self.emit.cqo();
        } else {
            try self.emit.cdq();
        }
        // 3. Perform IDIV
        try self.emit.idivReg(width, divisor_reg);
        // Remainder is in RDX
        if (dst != .RDX) {
            try self.emit.movRegReg(width, dst, .RDX);
        }
    }

    /// Emit unsigned integer modulo: dst = a % b
    /// Uses DIV which puts remainder in RDX
    pub fn emitUMod(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        // IMPORTANT: Save b to R11 BEFORE moving a to RAX, in case b is in RAX
        const divisor_reg = if (b == .RAX or b == .RDX) blk: {
            try self.emit.movRegReg(width, .R11, b);
            break :blk .R11;
        } else b;
        // 1. Move dividend to RAX
        if (a != .RAX) {
            try self.emit.movRegReg(width, .RAX, a);
        }
        // 2. Zero-extend: set RDX to 0
        try self.emit.xorRegReg(width, .RDX, .RDX);
        // 3. Perform DIV
        try self.emit.divReg(width, divisor_reg);
        // Remainder is in RDX
        if (dst != .RDX) {
            try self.emit.movRegReg(width, dst, .RDX);
        }
    }

    /// Emit integer negation: dst = -src
    pub fn emitNeg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
        if (dst != src) {
            try self.emit.movRegReg(width, dst, src);
        }
        try self.emit.negReg(width, dst);
    }

    /// Emit bitwise AND: dst = a & b
    pub fn emitAnd(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        if (dst != a) {
            try self.emit.movRegReg(width, dst, a);
        }
        try self.emit.andRegReg(width, dst, b);
    }

    /// Emit bitwise OR: dst = a | b
    pub fn emitOr(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
        if (dst != a) {
            try self.emit.movRegReg(width, dst, a);
        }
        try self.emit.orRegReg(width, dst, b);
    }

    /// Emit bitwise XOR with immediate: dst = src ^ imm
    pub fn emitXorImm(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, imm: i8) !void {
        if (dst != src) {
            try self.emit.movRegReg(width, dst, src);
        }
        try self.emit.xorRegImm8(width, dst, imm);
    }

    // Comparison operations

    /// Emit comparison and set condition: dst = (a op b) ? 1 : 0
    pub fn emitCmp(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg, cond: Emit.Condition) !void {
        try self.emit.cmpRegReg(width, a, b);
        try self.emit.setcc(cond, dst);
        // SETCC only sets the low byte; AND with 1 to mask the result
        // (matches the approach used by the Rust backend)
        try self.emit.andRegImm8(dst, 1);
    }

    // Floating-point operations

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

    /// Emit float64 negation: dst = -src
    /// Uses XOR with sign bit mask to properly handle -0.0
    pub fn emitNegF64(self: *Self, dst: FloatReg, src: FloatReg) !void {
        // Load sign bit mask (0x8000_0000_0000_0000) into dst
        // Then XOR with src to flip the sign bit
        const sign_bit_mask: i64 = @bitCast(@as(u64, 0x8000_0000_0000_0000));
        const stack_offset: i32 = -16; // Temporary stack location
        try self.emit.movRegImm64(.R11, sign_bit_mask);
        try self.emit.movMemReg(.w64, .RBP, stack_offset, .R11);
        try self.emit.movsdRegMem(dst, .RBP, stack_offset);
        try self.emit.xorpdRegReg(dst, src);
    }

    /// Emit float32 addition: dst = a + b
    pub fn emitAddF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        if (dst != a) {
            try self.emit.movssRegReg(dst, a);
        }
        try self.emit.addssRegReg(dst, b);
    }

    /// Emit float32 subtraction: dst = a - b
    pub fn emitSubF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        if (dst != a) {
            try self.emit.movssRegReg(dst, a);
        }
        try self.emit.subssRegReg(dst, b);
    }

    /// Emit float32 multiplication: dst = a * b
    pub fn emitMulF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        if (dst != a) {
            try self.emit.movssRegReg(dst, a);
        }
        try self.emit.mulssRegReg(dst, b);
    }

    /// Emit float32 division: dst = a / b
    pub fn emitDivF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
        if (dst != a) {
            try self.emit.movssRegReg(dst, a);
        }
        try self.emit.divssRegReg(dst, b);
    }

    /// Emit float32 negation: dst = -src
    /// Uses XOR with sign bit mask to properly handle -0.0
    pub fn emitNegF32(self: *Self, dst: FloatReg, src: FloatReg) !void {
        // Load sign bit mask (0x80000000) into dst
        // Then XOR with src to flip the sign bit
        const sign_bit_mask: i32 = @bitCast(@as(u32, 0x80000000));
        const stack_offset: i32 = -16; // Temporary stack location
        try self.emit.movRegImm32(.w32, .R11, sign_bit_mask);
        try self.emit.movMemReg(.w32, .RBP, stack_offset, .R11);
        try self.emit.movssRegMem(dst, .RBP, stack_offset);
        try self.emit.xorpsRegReg(dst, src);
    }

    // Memory operations

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

    // Immediate loading

    /// Load immediate value into register
    pub fn emitLoadImm(self: *Self, dst: GeneralReg, value: i64) !void {
        if (value >= std.math.minInt(i32) and value <= std.math.maxInt(i32)) {
            try self.emit.movRegImm32(dst, @intCast(value));
        } else {
            try self.emit.movRegImm64(dst, value);
        }
    }

    // Control flow

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

    /// Patch a CALL rel32 instruction's target offset.
    /// The call_site is the offset where the CALL instruction starts (E8 opcode).
    /// The rel_offset is the already-adjusted relative offset to patch in.
    pub fn patchCall(self: *Self, call_site: usize, rel_offset: i32) void {
        // CALL rel32 is: E8 xx xx xx xx
        // The rel32 starts at call_site + 1
        const bytes: [4]u8 = @bitCast(rel_offset);
        @memcpy(self.emit.buf.items[call_site + 1 ..][0..4], &bytes);
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

// Tests

test "prologue and epilogue" {
    var cg = SystemVCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitPrologue();
    try cg.emitEpilogue();

    const code = cg.getCode();
    // push rbp: 55 (1 byte)
    // mov rbp, rsp: 48 89 E5 (3 bytes)
    // mov rsp, rbp: 48 89 EC (3 bytes)
    // pop rbp: 5D (1 byte)
    // ret: C3 (1 byte)
    // Total: 9 bytes
    try std.testing.expectEqual(@as(usize, 9), code.len);
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

test "allocate caller-saved registers first" {
    var cg = SystemVCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Allocate several registers for locals
    const reg1 = try cg.allocGeneralFor(0);
    const reg2 = try cg.allocGeneralFor(1);
    const reg3 = try cg.allocGeneralFor(2);

    // All should be caller-saved (callee_saved_used should be 0)
    try std.testing.expectEqual(@as(u16, 0), cg.callee_saved_used);

    // Registers should be different
    try std.testing.expect(reg1 != reg2);
    try std.testing.expect(reg2 != reg3);
    try std.testing.expect(reg1 != reg3);
}

test "use callee-saved registers when caller-saved exhausted" {
    var cg = SystemVCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Allocate all caller-saved registers (9 in System V: RAX, RCX, RDX, RSI, RDI, R8-R11)
    var regs: [9]GeneralReg = undefined;
    for (0..9) |i| {
        regs[i] = try cg.allocGeneralFor(@intCast(i));
    }

    // callee_saved_used should still be 0 (all were caller-saved)
    try std.testing.expectEqual(@as(u16, 0), cg.callee_saved_used);

    // Next allocation should use a callee-saved register
    const callee_reg = try cg.allocGeneralFor(9);

    // Now callee_saved_used should have a bit set
    try std.testing.expect(cg.callee_saved_used != 0);

    // The register should be one of the callee-saved ones
    try std.testing.expect(SystemV.isCalleeSaved(callee_reg));
}

test "spill register when all exhausted" {
    var cg = SystemVCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Allocate all available registers (9 caller-saved + 5 callee-saved = 14)
    // Note: RSP and RBP are not available
    var regs: [14]GeneralReg = undefined;
    for (0..14) |i| {
        regs[i] = try cg.allocGeneralFor(@intCast(i));
    }

    // At this point all registers should be allocated
    // The next allocation should trigger a spill
    const initial_code_len = cg.getCode().len;
    const spilled_reg = try cg.allocGeneralFor(14);

    // Code should have been emitted (the spill store)
    try std.testing.expect(cg.getCode().len > initial_code_len);

    // The first local (0) should now be on the stack
    const loc0 = cg.locals.get(0);
    try std.testing.expect(loc0 != null);
    try std.testing.expect(loc0.?.stack < 0); // Stack offsets are negative

    // spilled_reg should be valid
    try std.testing.expect(spilled_reg != .RSP);
    try std.testing.expect(spilled_reg != .RBP);
}

test "reload spilled value" {
    var cg = SystemVCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Allocate all registers
    for (0..14) |i| {
        _ = try cg.allocGeneralFor(@intCast(i));
    }

    // Allocate one more to cause a spill
    _ = try cg.allocGeneralFor(14);

    // Local 0 should be on the stack now
    const loc0 = cg.locals.get(0);
    try std.testing.expect(loc0 != null);
    try std.testing.expect(loc0.? == .stack);

    // Free some registers to make room
    cg.freeGeneral(.RAX);
    cg.freeGeneral(.RCX);

    // Record code position before reload
    const code_before = cg.getCode().len;

    // Reload local 0
    const reloaded_reg = try cg.reloadLocal(0);

    // Code should have been emitted (the reload load)
    try std.testing.expect(cg.getCode().len > code_before);

    // Local 0 should now be in a register again
    const loc0_after = cg.locals.get(0);
    try std.testing.expect(loc0_after != null);
    try std.testing.expectEqual(GenericCodeGen.ValueLoc{ .general_reg = @intFromEnum(reloaded_reg) }, loc0_after.?);
}

test "prologue saves callee-saved registers" {
    var cg = SystemVCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Use some callee-saved registers by exhausting caller-saved first
    for (0..11) |i| {
        _ = try cg.allocGeneralFor(@intCast(i));
    }

    // Now emit prologue - it should include pushes for callee-saved regs
    try cg.emitPrologue();

    const code = cg.getCode();
    // Should be longer than basic prologue (push rbp + mov rbp, rsp = 4 bytes)
    try std.testing.expect(code.len > 4);

    // First byte should still be push rbp (0x55)
    try std.testing.expectEqual(@as(u8, 0x55), code[0]);
}

test "free register returns it to correct pool" {
    var cg = SystemVCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Allocate a caller-saved register
    const caller_reg = try cg.allocGeneralFor(0);
    try std.testing.expect(!SystemV.isCalleeSaved(caller_reg));

    // Exhaust caller-saved to get a callee-saved one
    for (1..10) |i| {
        _ = try cg.allocGeneralFor(@intCast(i));
    }
    const callee_reg = try cg.allocGeneralFor(10);
    try std.testing.expect(SystemV.isCalleeSaved(callee_reg));

    // Free both
    cg.freeGeneral(caller_reg);
    cg.freeGeneral(callee_reg);

    // caller_reg should be back in free_general
    try std.testing.expect((cg.free_general & (@as(u32, 1) << @intFromEnum(caller_reg))) != 0);

    // callee_reg should be back in callee_saved_available
    try std.testing.expect((cg.callee_saved_available & (@as(u32, 1) << @intFromEnum(callee_reg))) != 0);
}
