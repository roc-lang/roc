//! aarch64-specific code generation.
//!
//! This module provides aarch64-specific code generation including
//! function prologues/epilogues and instruction selection.

const std = @import("std");
const Allocator = std.mem.Allocator;
const RocTarget = @import("roc_target").RocTarget;

const EmitMod = @import("Emit.zig");
const Registers = @import("Registers.zig");
const Call = @import("Call.zig");
const Relocation = @import("../Relocation.zig").Relocation;
const ValueStorageMod = @import("../ValueStorage.zig");
const FrameBuilderMod = @import("../FrameBuilder.zig");

const GeneralReg = Registers.GeneralReg;
const FloatReg = Registers.FloatReg;
const RegisterWidth = Registers.RegisterWidth;

/// Parameterized aarch64 code generator.
/// All aarch64 targets use the AAPCS64 calling convention.
pub fn CodeGen(comptime target: RocTarget) type {
    // Validate this is an aarch64 target
    const arch = target.toCpuArch();
    if (arch != .aarch64 and arch != .aarch64_be) {
        @compileError("aarch64.CodeGen requires an aarch64 target");
    }

    const Emit = EmitMod.Emit(target);
    const CC = Call; // AAPCS64 for all aarch64 targets

    return struct {
        const Self = @This();

        /// The target this CodeGen was instantiated for
        pub const roc_target = target;

        /// Number of general-purpose registers
        const NUM_GENERAL_REGS = 32;
        /// Number of float registers
        const NUM_FLOAT_REGS = 32;

        /// Size of callee-saved area in bytes (5 pairs * 16 bytes = 80)
        /// Used by MonoExprCodeGen to reserve stack space for callee-saved registers
        pub const CALLEE_SAVED_AREA_SIZE: i32 = 80;

        /// Bitmask of callee-saved general registers available for allocation
        /// X19-X28 (not FP/X29 or LR/X30 - they're special)
        pub const CALLEE_SAVED_GENERAL_MASK: u32 =
            (1 << @intFromEnum(GeneralReg.X19)) |
            (1 << @intFromEnum(GeneralReg.X20)) |
            (1 << @intFromEnum(GeneralReg.X21)) |
            (1 << @intFromEnum(GeneralReg.X22)) |
            (1 << @intFromEnum(GeneralReg.X23)) |
            (1 << @intFromEnum(GeneralReg.X24)) |
            (1 << @intFromEnum(GeneralReg.X25)) |
            (1 << @intFromEnum(GeneralReg.X26)) |
            (1 << @intFromEnum(GeneralReg.X27)) |
            (1 << @intFromEnum(GeneralReg.X28));

        emit: Emit,
        allocator: Allocator,
        stack_offset: i32,
        relocations: std.ArrayList(Relocation),
        locals: std.AutoHashMap(u32, ValueStorageMod.ValueLoc),
        free_general: u32,
        free_float: u32,
        callee_saved_used: u32, // Bitmask of callee-saved regs we used

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
                .locals = std.AutoHashMap(u32, ValueStorageMod.ValueLoc).init(allocator),
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
        ///
        /// INVARIANT: Each local ID must map to at most ONE register. Using the same
        /// local ID for multiple registers corrupts spill tracking. Use unique temp IDs
        /// (starting at 0x8000_0000) for temporaries that don't correspond to real locals.
        pub fn allocGeneralFor(self: *Self, local: u32) !GeneralReg {
            // 1. Try caller-saved registers first (preferred - no save/restore needed)
            if (self.allocFromGeneralMask(&self.free_general)) |reg| {
                // DEBUG: Verify no OTHER register already owns this local
                if (std.debug.runtime_safety) {
                    for (self.general_owners, 0..) |owner, i| {
                        if (owner) |owned_local| {
                            std.debug.assert(owned_local != local or i == @intFromEnum(reg));
                        }
                    }
                }
                self.general_owners[@intFromEnum(reg)] = local;
                return reg;
            }

            // 2. Try callee-saved registers (will need save/restore in prologue/epilogue)
            if (self.allocFromGeneralMask(&self.callee_saved_available)) |reg| {
                self.callee_saved_used |= @as(u32, 1) << @intFromEnum(reg);
                // DEBUG: Verify no OTHER register already owns this local
                if (std.debug.runtime_safety) {
                    for (self.general_owners, 0..) |owner, i| {
                        if (owner) |owned_local| {
                            std.debug.assert(owned_local != local or i == @intFromEnum(reg));
                        }
                    }
                }
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
                self.callee_saved_used |= @as(u32, 1) << @intFromEnum(reg);
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
        /// NOTE: Uses sentinel value 0 for ownership. Callers of allocGeneralFor must NOT use
        /// local ID 0 - use unique temp IDs (starting at 0x8000_0000) for temporaries.
        pub fn markRegisterInUse(self: *Self, reg: GeneralReg) void {
            const idx = @intFromEnum(reg);
            // Remove from free pool (it's now in use)
            self.free_general &= ~(@as(u32, 1) << idx);
            self.callee_saved_available &= ~(@as(u32, 1) << idx);
            // DEBUG: Verify no OTHER register already owns local 0 before assignment
            if (std.debug.runtime_safety) {
                for (self.general_owners, 0..) |owner, i| {
                    if (owner) |owned_local| {
                        std.debug.assert(owned_local != 0 or i == idx);
                    }
                }
            }
            // Set ownership to a sentinel value (0 = temporary)
            self.general_owners[idx] = 0;
        }

        /// Spill a register to make room and allocate it for the given local.
        fn spillAndAllocGeneral(self: *Self, local: u32) !GeneralReg {
            // Find a register to spill - prefer lowest-numbered for consistency
            // Skip special registers (FP, LR, SP/ZR)
            var victim: ?GeneralReg = null;
            for (0..NUM_GENERAL_REGS) |i| {
                const reg: GeneralReg = @enumFromInt(i);
                // Skip special registers
                if (reg == .FP or reg == .LR or reg == .ZRSP or reg == .PR) continue;
                // Skip registers we don't own (they're free)
                if (self.general_owners[i] != null) {
                    victim = reg;
                    break;
                }
            }

            const reg = victim orelse unreachable;
            const owner = self.general_owners[@intFromEnum(reg)].?;

            // Allocate stack slot for the spilled value
            const slot = self.allocStack(8);

            // Emit store instruction
            try self.emitStoreStack(.w64, slot, reg);

            // Update the owner's location to stack
            try self.locals.put(owner, .{ .stack = slot });

            // DEBUG: Verify no OTHER register already owns this local before assignment
            if (std.debug.runtime_safety) {
                for (self.general_owners, 0..) |other_owner, i| {
                    if (other_owner) |owned_local| {
                        std.debug.assert(owned_local != local or i == @intFromEnum(reg));
                    }
                }
            }

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
            // Float registers: try caller-saved first
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

            const reg = victim orelse unreachable;
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
            const aligned_size: i32 = @intCast((size + 15) & ~@as(u32, 15)); // 16-byte align

            // For aarch64 procedure frames, stack_offset is positive and grows upward.
            // We return the current offset and increment for the next allocation.
            // For main expression frames, stack_offset is negative and grows downward.
            if (self.stack_offset >= 0) {
                // Procedure frame: return current offset, then increment
                const offset = self.stack_offset;
                self.stack_offset += aligned_size;
                return offset;
            } else {
                // Main expression frame: decrement, then return (standard downward growth)
                self.stack_offset -= aligned_size;
                return self.stack_offset;
            }
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

        /// Deferred frame builder type for this architecture (mask-based, body generated first)
        pub const DeferredFrameBuilder = FrameBuilderMod.DeferredFrameBuilder(Emit);

        /// Callee-saved registers in pairs for saving/restoring
        /// aarch64 saves registers in pairs for efficiency
        pub const CALLEE_SAVED_PAIRS = DeferredFrameBuilder.CALLEE_SAVED_PAIRS;

        /// Check if any register in a pair is used
        pub fn isPairUsed(self: *Self, pair: [2]GeneralReg) bool {
            const mask1 = @as(u32, 1) << @intFromEnum(pair[0]);
            const mask2 = @as(u32, 1) << @intFromEnum(pair[1]);
            return (self.callee_saved_used & (mask1 | mask2)) != 0;
        }

        /// Emit callee-saved register saves at fixed offsets from FP
        /// Used by MonoExprCodeGen for procedures that pre-allocate the frame
        /// Saves to [FP + 16], [FP + 32], etc. for each used pair
        /// The offset is scaled by 8 for stp/ldp (i.e., offset=2 means 16 bytes)
        pub fn emitSaveCalleeSavedToFrame(self: *Self) !void {
            var builder = DeferredFrameBuilder.init();
            builder.setCalleeSavedMask(self.callee_saved_used);
            try builder.emitSaveCalleeSaved(&self.emit);
        }

        /// Emit callee-saved register restores from fixed offsets from FP
        /// Used by MonoExprCodeGen for procedures that pre-allocate the frame
        /// Restores from [FP + 16], [FP + 32], etc. for each used pair
        /// The offset is scaled by 8 for stp/ldp (i.e., offset=2 means 16 bytes)
        pub fn emitRestoreCalleeSavedFromFrame(self: *Self) !void {
            var builder = DeferredFrameBuilder.init();
            builder.setCalleeSavedMask(self.callee_saved_used);
            try builder.emitRestoreCalleeSaved(&self.emit);
        }

        /// Emit function prologue (called at start of function)
        /// Note: Call this AFTER register allocation is complete to know which
        /// callee-saved registers need to be preserved.
        pub fn emitPrologue(self: *Self) !void {
            var builder = DeferredFrameBuilder.init();
            builder.setCalleeSavedMask(self.callee_saved_used);
            _ = try builder.emitPrologue(&self.emit);
        }

        /// Emit function epilogue and return
        pub fn emitEpilogue(self: *Self) !void {
            var builder = DeferredFrameBuilder.init();
            builder.setCalleeSavedMask(self.callee_saved_used);
            try builder.emitEpilogue(&self.emit);
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

        // Integer operations

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

        /// Emit signed integer division: dst = a / b
        pub fn emitSDiv(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
            try self.emit.sdivRegRegReg(width, dst, a, b);
        }

        /// Emit unsigned integer division: dst = a / b
        pub fn emitUDiv(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
            try self.emit.udivRegRegReg(width, dst, a, b);
        }

        /// Emit signed integer modulo: dst = a % b
        /// Uses SDIV + MSUB: remainder = a - (a/b) * b
        pub fn emitSMod(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
            // We need a temp register for the quotient
            // Since dst might be the same as a or b, we use dst for intermediate if safe
            // quotient = a / b
            try self.emit.sdivRegRegReg(width, dst, a, b);
            // remainder = a - quotient * b
            try self.emit.msubRegRegRegReg(width, dst, dst, b, a);
        }

        /// Emit unsigned integer modulo: dst = a % b
        /// Uses UDIV + MSUB: remainder = a - (a/b) * b
        pub fn emitUMod(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
            // quotient = a / b
            try self.emit.udivRegRegReg(width, dst, a, b);
            // remainder = a - quotient * b
            try self.emit.msubRegRegRegReg(width, dst, dst, b, a);
        }

        /// Emit integer negation: dst = -src
        pub fn emitNeg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) !void {
            try self.emit.negRegReg(width, dst, src);
        }

        /// Emit bitwise AND: dst = a & b
        pub fn emitAnd(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
            try self.emit.andRegRegReg(width, dst, a, b);
        }

        /// Emit bitwise OR: dst = a | b
        pub fn emitOr(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) !void {
            try self.emit.orrRegRegReg(width, dst, a, b);
        }

        /// Emit bitwise XOR with immediate: dst = src ^ imm
        pub fn emitXorImm(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, imm: i8) !void {
            // Load immediate into scratch register and use EOR
            try self.emit.movRegImm32(width, .IP0, imm);
            try self.emit.eorRegRegReg(width, dst, src, .IP0);
        }

        // Comparison operations

        /// Emit comparison and set condition: dst = (a op b) ? 1 : 0
        pub fn emitCmp(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg, cond: Emit.Condition) !void {
            try self.emit.cmpRegReg(width, a, b);
            try self.emit.cset(width, dst, cond);
        }

        /// Emit float64 compare and set: dst = (a cond b) ? 1 : 0
        pub fn emitCmpF64(self: *Self, dst: GeneralReg, a: FloatReg, b: FloatReg, cond: Emit.Condition) !void {
            try self.emit.fcmpRegReg(.double, a, b);
            try self.emit.cset(.w64, dst, cond);
        }

        // Floating-point operations

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

        /// Emit float64 negation: dst = -src
        pub fn emitNegF64(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.emit.fnegRegReg(.double, dst, src);
        }

        /// Emit float32 addition: dst = a + b
        pub fn emitAddF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
            try self.emit.faddRegRegReg(.single, dst, a, b);
        }

        /// Emit float32 subtraction: dst = a - b
        pub fn emitSubF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
            try self.emit.fsubRegRegReg(.single, dst, a, b);
        }

        /// Emit float32 multiplication: dst = a * b
        pub fn emitMulF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
            try self.emit.fmulRegRegReg(.single, dst, a, b);
        }

        /// Emit float32 division: dst = a / b
        pub fn emitDivF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) !void {
            try self.emit.fdivRegRegReg(.single, dst, a, b);
        }

        /// Emit float32 negation: dst = -src
        pub fn emitNegF32(self: *Self, dst: FloatReg, src: FloatReg) !void {
            try self.emit.fnegRegReg(.single, dst, src);
        }

        // Memory operations

        /// Load from stack slot into register
        pub fn emitLoadStack(self: *Self, width: RegisterWidth, dst: GeneralReg, offset: i32) !void {
            if (offset >= -256 and offset <= 255) {
                try self.emit.ldurRegMem(width, dst, .FP, @intCast(offset));
            } else if (offset > 0) {
                // Positive offset - use scaled unsigned form
                const uoffset: u12 = @intCast(@as(u32, @intCast(offset)) >> (if (width == .w64) 3 else 2));
                try self.emit.ldrRegMemUoff(width, dst, .FP, uoffset);
            } else {
                // Large negative offset - add offset to FP, then load
                // Use IP0 as scratch register
                try self.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                try self.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                try self.emit.ldrRegMemUoff(width, dst, .IP0, 0);
            }
        }

        /// Store register to stack slot
        pub fn emitStoreStack(self: *Self, width: RegisterWidth, offset: i32, src: GeneralReg) !void {
            if (offset >= -256 and offset <= 255) {
                try self.emit.sturRegMem(width, src, .FP, @intCast(offset));
            } else if (offset > 0) {
                // Positive offset - use scaled unsigned form
                const uoffset: u12 = @intCast(@as(u32, @intCast(offset)) >> (if (width == .w64) 3 else 2));
                try self.emit.strRegMemUoff(width, src, .FP, uoffset);
            } else {
                // Large negative offset - add offset to FP, then store
                // Use IP0 as scratch register
                try self.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                try self.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                try self.emit.strRegMemUoff(width, src, .IP0, 0);
            }
        }

        /// Store byte to stack slot
        pub fn emitStoreStackByte(self: *Self, offset: i32, src: GeneralReg) !void {
            if (offset >= -256 and offset <= 255) {
                try self.emit.sturbRegMem(src, .FP, @intCast(offset));
            } else {
                // Large offset - add offset to FP, then store
                try self.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                try self.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                try self.emit.strbRegMem(src, .IP0, 0);
            }
        }

        /// Store halfword (2 bytes) to stack slot
        pub fn emitStoreStackHalfword(self: *Self, offset: i32, src: GeneralReg) !void {
            if (offset >= -256 and offset <= 255) {
                try self.emit.sturhRegMem(src, .FP, @intCast(offset));
            } else {
                // Large offset - add offset to FP, then store
                try self.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                try self.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                try self.emit.strhRegMem(src, .IP0, 0);
            }
        }

        /// Load byte (zero-extended) from stack slot
        pub fn emitLoadStackByte(self: *Self, dst: GeneralReg, offset: i32) !void {
            if (offset >= -256 and offset <= 255) {
                try self.emit.ldurbRegMem(dst, .FP, @intCast(offset));
            } else if (offset > 0) {
                // Positive offset - use scaled unsigned form (byte: no shift needed)
                const uoffset: u12 = @intCast(@as(u32, @intCast(offset)));
                try self.emit.ldrbRegMem(dst, .FP, uoffset);
            } else {
                // Large negative offset - add offset to FP, then load
                try self.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                try self.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                try self.emit.ldrbRegMem(dst, .IP0, 0);
            }
        }

        /// Load halfword (zero-extended) from stack slot
        pub fn emitLoadStackHalfword(self: *Self, dst: GeneralReg, offset: i32) !void {
            if (offset >= -256 and offset <= 255) {
                try self.emit.ldurhRegMem(dst, .FP, @intCast(offset));
            } else if (offset > 0) {
                // Positive offset - use scaled unsigned form (halfword: shift by 1)
                const uoffset: u12 = @intCast(@as(u32, @intCast(offset)) >> 1);
                try self.emit.ldrhRegMem(dst, .FP, uoffset);
            } else {
                // Large negative offset - add offset to FP, then load
                try self.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                try self.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                try self.emit.ldrhRegMem(dst, .IP0, 0);
            }
        }

        /// Load float64 from stack slot
        pub fn emitLoadStackF64(self: *Self, dst: FloatReg, offset: i32) !void {
            if (offset >= 0) {
                // Positive offset - use scaled unsigned form
                const uoffset: u12 = @intCast(@as(u32, @intCast(offset)) >> 3);
                try self.emit.fldrRegMemUoff(.double, dst, .FP, uoffset);
            } else {
                // Negative offset - add offset to FP, then load
                try self.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                try self.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                try self.emit.fldrRegMemUoff(.double, dst, .IP0, 0);
            }
        }

        /// Store float64 to stack slot
        pub fn emitStoreStackF64(self: *Self, offset: i32, src: FloatReg) !void {
            if (offset >= 0) {
                // Positive offset - use scaled unsigned form
                const uoffset: u12 = @intCast(@as(u32, @intCast(offset)) >> 3);
                try self.emit.fstrRegMemUoff(.double, src, .FP, uoffset);
            } else {
                // Negative offset - add offset to FP, then store
                try self.emit.movRegImm64(.IP0, @bitCast(@as(i64, offset)));
                try self.emit.addRegRegReg(.w64, .IP0, .FP, .IP0);
                try self.emit.fstrRegMemUoff(.double, src, .IP0, 0);
            }
        }

        // Immediate loading

        /// Load immediate value into register
        pub fn emitLoadImm(self: *Self, dst: GeneralReg, value: i64) !void {
            try self.emit.movRegImm64(dst, @bitCast(value));
        }

        // Control flow

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
        pub fn patchJump(self: *Self, patch_loc: usize, target_loc: usize) void {
            const offset: i32 = @intCast(@as(i64, @intCast(target_loc)) - @as(i64, @intCast(patch_loc)));
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
            } else if (((inst >> 24) & 0b01111111) == 0b0110100 or ((inst >> 24) & 0b01111111) == 0b0110101) {
                // CBZ/CBNZ: sf 011010 x imm19 Rt
                // imm19 is in bits [23:5]
                const imm19: u19 = @bitCast(@as(i19, @truncate(offset_words)));
                inst = (inst & 0xFF00001F) | (@as(u32, imm19) << 5);
            }

            std.mem.writeInt(u32, self.emit.buf.items[patch_loc..][0..4], inst, .little);
        }

        /// Patch a BL (branch with link) instruction to target a specific offset
        pub fn patchBL(self: *Self, patch_loc: usize, offset_words: i32) void {
            // BL uses imm26 encoding: 1 00101 imm26
            const imm26: u26 = @bitCast(@as(i26, @truncate(offset_words)));
            const inst: u32 = (@as(u32, 0b100101) << 26) | imm26;
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
}

// Tests - use explicit target instantiation

const LinuxCodeGen = CodeGen(.arm64linux);
const WinCodeGen = CodeGen(.arm64win);
const MacCodeGen = CodeGen(.arm64mac);

test "prologue and epilogue" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
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
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitLoadImm(.X0, 42);
    const code = cg.getCode();
    try std.testing.expect(code.len > 0);
}

test "integer operations" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitAdd(.w64, .X0, .X1, .X2);
    try cg.emitSub(.w64, .X3, .X4, .X5);
    try cg.emitMul(.w64, .X6, .X7, .XR);

    try std.testing.expect(cg.getCode().len > 0);
}

test "float operations" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitAddF64(.V0, .V1, .V2);
    try cg.emitSubF64(.V3, .V4, .V5);
    try cg.emitMulF64(.V6, .V7, .V8);
    try cg.emitDivF64(.V9, .V10, .V11);

    try std.testing.expect(cg.getCode().len > 0);
}

test "CodeGen works for all aarch64 targets" {
    // All aarch64 targets use AAPCS64, so callee-saved masks should be identical
    try std.testing.expectEqual(LinuxCodeGen.CALLEE_SAVED_GENERAL_MASK, WinCodeGen.CALLEE_SAVED_GENERAL_MASK);
    try std.testing.expectEqual(LinuxCodeGen.CALLEE_SAVED_GENERAL_MASK, MacCodeGen.CALLEE_SAVED_GENERAL_MASK);

    // Verify target is correctly set
    try std.testing.expectEqual(RocTarget.arm64linux, LinuxCodeGen.roc_target);
    try std.testing.expectEqual(RocTarget.arm64win, WinCodeGen.roc_target);
    try std.testing.expectEqual(RocTarget.arm64mac, MacCodeGen.roc_target);
}
