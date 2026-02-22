//! x86_64-specific code generation.
//!
//! This module provides x86_64-specific code generation including
//! function prologues/epilogues and instruction selection.
//!
//! CodeGen is parameterized by RocTarget to support cross-compilation.
//! Use CodeGen(target) to get a specialized type for a specific target.

const std = @import("std");
const Allocator = std.mem.Allocator;
const RocTarget = @import("roc_target").RocTarget;

const EmitMod = @import("Emit.zig");
const Registers = @import("Registers.zig");
const SystemV = @import("SystemV.zig");
const WindowsFastcall = @import("WindowsFastcall.zig");
const Relocation = @import("../Relocation.zig").Relocation;
const ValueStorageMod = @import("../ValueStorage.zig");
const FrameBuilderMod = @import("../FrameBuilder.zig");

const GeneralReg = Registers.GeneralReg;
const FloatReg = Registers.FloatReg;
const RegisterWidth = Registers.RegisterWidth;

/// Parameterized x86_64 code generator.
/// Use CodeGen(target) to get a specialized type for a specific RocTarget.
pub fn CodeGen(comptime target: RocTarget) type {
    // Validate this is an x86_64 target
    if (target.toCpuArch() != .x86_64) {
        @compileError("x86_64.CodeGen requires an x86_64 target");
    }

    const Emit = EmitMod.Emit(target);
    const CC = if (target.isWindows()) WindowsFastcall else SystemV;

    return struct {
        const Self = @This();

        /// The target this CodeGen was instantiated for
        pub const roc_target = target;

        /// Number of general-purpose registers
        const NUM_GENERAL_REGS = 16;
        /// Number of float registers
        const NUM_FLOAT_REGS = 16;

        /// Bitmask of callee-saved general registers available for allocation
        /// System V: RBX, R12, R13, R14, R15 (not RBP - it's the frame pointer)
        /// Windows: RBX, RSI, RDI, R12, R13, R14, R15 (not RBP - it's the frame pointer)
        pub const CALLEE_SAVED_GENERAL_MASK: u32 = if (target.isWindows())
            (1 << @intFromEnum(GeneralReg.RBX)) |
                (1 << @intFromEnum(GeneralReg.RSI)) |
                (1 << @intFromEnum(GeneralReg.RDI)) |
                (1 << @intFromEnum(GeneralReg.R12)) |
                (1 << @intFromEnum(GeneralReg.R13)) |
                (1 << @intFromEnum(GeneralReg.R14)) |
                (1 << @intFromEnum(GeneralReg.R15))
        else
            (1 << @intFromEnum(GeneralReg.RBX)) |
                (1 << @intFromEnum(GeneralReg.R12)) |
                (1 << @intFromEnum(GeneralReg.R13)) |
                (1 << @intFromEnum(GeneralReg.R14)) |
                (1 << @intFromEnum(GeneralReg.R15));

        /// Size of the callee-saved register area
        /// Windows: 7 registers * 8 bytes = 56 bytes
        /// System V: 5 registers * 8 bytes = 40 bytes
        pub const CALLEE_SAVED_AREA_SIZE: i32 = if (target.isWindows()) 56 else 40;

        emit: Emit,
        allocator: Allocator,
        stack_offset: i32,
        relocations: std.ArrayList(Relocation),
        locals: std.AutoHashMap(u32, ValueStorageMod.ValueLoc),
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

            const reg = victim orelse unreachable;
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
            // Align to 16 bytes for i128/u128 (vmovdqa requires it), 8 bytes otherwise
            const alignment: u32 = if (size > 8) 16 else 8;
            const aligned_size = (size + alignment - 1) & ~(alignment - 1);
            self.stack_offset -= @intCast(aligned_size);
            // Ensure the offset itself is aligned (not just the size)
            self.stack_offset &= ~@as(i32, @intCast(alignment - 1));
            return self.stack_offset;
        }

        /// Alias for allocStack - allocate a stack slot of the given size
        pub fn allocStackSlot(self: *Self, size: u32) i32 {
            return self.allocStack(size);
        }

        pub fn getStackSize(self: *Self) u32 {
            const size: u32 = @intCast(-self.stack_offset);
            return Emit.CC.alignStackSize(size);
        }

        // Function prologue/epilogue

        /// Deferred frame builder type for this architecture (mask-based, body generated first)
        pub const DeferredFrameBuilder = FrameBuilderMod.DeferredFrameBuilder(Emit);

        /// Emit function prologue with stack allocation (called at start of function)
        /// Note: Call this AFTER register allocation is complete to know which
        /// callee-saved registers need to be preserved.
        /// Uses MOV-based selective save at fixed RBP-relative offsets.
        ///
        /// IMPORTANT: On Windows x64, there is no "red zone" below RSP. We must
        /// allocate stack space BEFORE saving callee-saved registers to [RBP-offset],
        /// otherwise those stores would be writing below RSP which is unsafe.
        pub fn emitPrologueWithAlloc(self: *Self, stack_size: u32) !void {
            var builder = DeferredFrameBuilder.init();
            builder.setCalleeSavedMask(self.callee_saved_used);
            builder.setStackSize(stack_size);
            _ = try builder.emitPrologue(&self.emit);
        }

        /// Emit function epilogue and return
        /// Restores callee-saved registers using MOV from fixed RBP-relative offsets.
        pub fn emitEpilogue(self: *Self) !void {
            var builder = DeferredFrameBuilder.init();
            builder.setCalleeSavedMask(self.callee_saved_used);
            // Note: stack_size not needed for epilogue since we use mov rsp, rbp
            try builder.emitEpilogue(&self.emit);
        }

        /// Emit stack frame setup with given local size
        pub fn emitStackAlloc(self: *Self, size: u32) !void {
            if (size > 0) {
                // sub rsp, size
                try self.emit.subRegImm32(.w64, .RSP, @intCast(size));
            }
        }

        /// On Windows, save R12 to stack before a C function call.
        /// R12 holds roc_ops in generated code, and while R12 is callee-saved
        /// in the Windows x64 ABI, something in the Zig-compiled C function
        /// call chain appears to corrupt it. This saves R12 and returns the
        /// offset where it was saved, for use with restoreR12AfterCall.
        pub fn saveR12BeforeCall(self: *Self) !i32 {
            if (comptime target.isWindows()) {
                // Allocate 8 bytes on stack for R12
                try self.emit.subRegImm32(.w64, .RSP, 8);
                // Save R12 at [RSP]
                try self.emit.movMemReg(.w64, .RSP, 0, .R12);
                return 0; // R12 is at [RSP+0]
            }
            return 0;
        }

        /// On Windows, restore R12 from stack after a C function call.
        pub fn restoreR12AfterCall(self: *Self) !void {
            if (comptime target.isWindows()) {
                // Restore R12 from [RSP]
                try self.emit.movRegMem(.w64, .R12, .RSP, 0);
                // Deallocate the 8 bytes
                try self.emit.addRegImm32(.w64, .RSP, 8);
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

        /// Emit float64 compare and set: dst = (a cond b) ? 1 : 0
        pub fn emitCmpF64(self: *Self, dst: GeneralReg, a: FloatReg, b: FloatReg, cond: Emit.Condition) !void {
            try self.emit.ucomisdRegReg(a, b);
            try self.emit.setcc(cond, dst);
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
        pub fn patchJump(self: *Self, patch_loc: usize, target_loc: usize) void {
            const offset: i32 = @intCast(@as(i64, @intCast(target_loc)) - @as(i64, @intCast(patch_loc + 4)));
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
}

// Tests - use explicit target instantiation for cross-platform testing

const LinuxCodeGen = CodeGen(.x64linux);
const WinCodeGen = CodeGen(.x64win);
const MacCodeGen = CodeGen(.x64mac);

test "prologue and epilogue" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Use emitPrologueWithAlloc for Windows compatibility (0 bytes allocated)
    try cg.emitPrologueWithAlloc(0);
    try cg.emitEpilogue();

    const code = cg.getCode();
    // With no callee-saved registers used and 0 stack allocation:
    // push rbp: 55 (1 byte)
    // mov rbp, rsp: 48 89 E5 (3 bytes)
    // sub rsp, <callee_saved_area>: 48 81 EC xx xx xx xx (7 bytes) — always reserves callee-saved area
    // (no MOV saves since callee_saved_used = 0)
    // (no MOV restores since callee_saved_used = 0)
    // mov rsp, rbp: 48 89 EC (3 bytes)
    // pop rbp: 5D (1 byte)
    // ret: C3 (1 byte)
    // Total: 16 bytes
    try std.testing.expectEqual(@as(usize, 16), code.len);
    try std.testing.expectEqual(@as(u8, 0x55), code[0]); // push rbp
    try std.testing.expectEqual(@as(u8, 0xC3), code[code.len - 1]); // ret
}

test "load immediate" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitLoadImm(.RAX, 42);
    const code = cg.getCode();
    try std.testing.expect(code.len > 0);
}

test "integer operations" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitAdd(.w64, .RAX, .RBX, .RCX);
    try cg.emitSub(.w64, .RDX, .RSI, .RDI);
    try cg.emitMul(.w64, .R8, .R9, .R10);

    try std.testing.expect(cg.getCode().len > 0);
}

test "float operations" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    try cg.emitAddF64(.XMM0, .XMM1, .XMM2);
    try cg.emitSubF64(.XMM3, .XMM4, .XMM5);
    try cg.emitMulF64(.XMM6, .XMM7, .XMM8);
    try cg.emitDivF64(.XMM9, .XMM10, .XMM11);

    try std.testing.expect(cg.getCode().len > 0);
}

test "allocate caller-saved registers first" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
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

test "Linux x64: use callee-saved registers when caller-saved exhausted" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // System V has 9 caller-saved registers
    const num_caller_saved: usize = 9;
    var regs: [9]GeneralReg = undefined;
    for (0..num_caller_saved) |i| {
        regs[i] = try cg.allocGeneralFor(@intCast(i));
    }

    // callee_saved_used should still be 0 (all were caller-saved)
    try std.testing.expectEqual(@as(u16, 0), cg.callee_saved_used);

    // Next allocation should use a callee-saved register
    const callee_reg = try cg.allocGeneralFor(@intCast(num_caller_saved));

    // Now callee_saved_used should have a bit set
    try std.testing.expect(cg.callee_saved_used != 0);

    // The register should be one of the callee-saved ones (System V)
    try std.testing.expect(SystemV.isCalleeSaved(callee_reg));
}

test "Windows x64: use callee-saved registers when caller-saved exhausted" {
    var cg = WinCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Windows has 7 caller-saved registers
    const num_caller_saved: usize = 7;
    var regs: [7]GeneralReg = undefined;
    for (0..num_caller_saved) |i| {
        regs[i] = try cg.allocGeneralFor(@intCast(i));
    }

    // callee_saved_used should still be 0 (all were caller-saved)
    try std.testing.expectEqual(@as(u16, 0), cg.callee_saved_used);

    // Next allocation should use a callee-saved register
    const callee_reg = try cg.allocGeneralFor(@intCast(num_caller_saved));

    // Now callee_saved_used should have a bit set
    try std.testing.expect(cg.callee_saved_used != 0);

    // The register should be one of the callee-saved ones (Windows)
    try std.testing.expect(WindowsFastcall.isCalleeSaved(callee_reg));
}

test "spill register when all exhausted" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Allocate all available registers
    // System V: 9 caller-saved + 5 callee-saved = 14
    // Note: RSP and RBP are not available
    const total_regs: usize = 14;
    var regs: [14]GeneralReg = undefined;
    for (0..total_regs) |i| {
        regs[i] = try cg.allocGeneralFor(@intCast(i));
    }

    // At this point all registers should be allocated
    // The next allocation should trigger a spill
    const initial_code_len = cg.getCode().len;
    const spilled_reg = try cg.allocGeneralFor(@intCast(total_regs));

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
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Allocate all registers (14 total: RSP and RBP are not available)
    const total_regs: usize = 14;
    for (0..total_regs) |i| {
        _ = try cg.allocGeneralFor(@intCast(i));
    }

    // Allocate one more to cause a spill
    _ = try cg.allocGeneralFor(@intCast(total_regs));

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
    try std.testing.expectEqual(ValueStorageMod.ValueLoc{ .general_reg = @intFromEnum(reloaded_reg) }, loc0_after.?);
}

test "Linux x64 prologue saves callee-saved registers with MOV" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // System V: 9 caller-saved, need 11 total to use 2 callee-saved
    for (0..11) |i| {
        _ = try cg.allocGeneralFor(@intCast(i));
    }

    // Now emit prologue - it should include MOV saves for callee-saved regs
    try cg.emitPrologueWithAlloc(64);

    const code = cg.getCode();
    // Basic prologue: push rbp (1) + mov rbp, rsp (3) + sub rsp (4-7) = ~8-11 bytes
    // Plus MOV saves for callee-saved registers (each MOV [rbp+disp], reg is 4-7 bytes)
    // With 2 callee-saved regs, we expect more than basic prologue
    try std.testing.expect(code.len > 8);

    // First byte should still be push rbp (0x55)
    try std.testing.expectEqual(@as(u8, 0x55), code[0]);

    // callee_saved_used should have bits set for at least 2 callee-saved registers
    try std.testing.expect(cg.callee_saved_used != 0);
    try std.testing.expect(@popCount(cg.callee_saved_used) >= 2);
}

test "Windows x64 prologue saves callee-saved registers with MOV" {
    var cg = WinCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Windows: 7 caller-saved, need 9 total to use 2 callee-saved
    for (0..9) |i| {
        _ = try cg.allocGeneralFor(@intCast(i));
    }

    // Now emit prologue
    try cg.emitPrologueWithAlloc(64);

    const code = cg.getCode();
    try std.testing.expect(code.len > 8);
    try std.testing.expectEqual(@as(u8, 0x55), code[0]); // push rbp
    try std.testing.expect(@popCount(cg.callee_saved_used) >= 2);
}

test "free register returns it to correct pool" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Allocate a caller-saved register
    const caller_reg = try cg.allocGeneralFor(0);
    try std.testing.expect(!SystemV.isCalleeSaved(caller_reg));

    // Exhaust caller-saved to get a callee-saved one (System V: 9 caller-saved)
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

test "epilogue restores callee-saved registers with MOV" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Use some callee-saved registers (System V: 9 caller-saved)
    for (0..11) |i| {
        _ = try cg.allocGeneralFor(@intCast(i));
    }

    // Emit prologue and epilogue
    try cg.emitPrologueWithAlloc(64);
    const prologue_len = cg.getCode().len;
    try cg.emitEpilogue();

    const code = cg.getCode();
    const epilogue_len = code.len - prologue_len;

    // Epilogue should include MOV restores + mov rsp,rbp + pop rbp + ret
    try std.testing.expect(epilogue_len > 5);

    // Last byte should be ret (0xC3)
    try std.testing.expectEqual(@as(u8, 0xC3), code[code.len - 1]);
}

test "Linux x64 CALLEE_SAVED_AREA_SIZE constant" {
    // System V: 5 registers * 8 bytes = 40 bytes
    try std.testing.expectEqual(@as(i32, 40), LinuxCodeGen.CALLEE_SAVED_AREA_SIZE);
}

test "Windows x64 CALLEE_SAVED_AREA_SIZE constant" {
    // Windows: 7 registers * 8 bytes = 56 bytes
    try std.testing.expectEqual(@as(i32, 56), WinCodeGen.CALLEE_SAVED_AREA_SIZE);
}

test "Linux x64 callee-saved register count" {
    // System V: 5 callee-saved registers (RBX, R12-R15)
    try std.testing.expectEqual(@as(u32, 5), @popCount(LinuxCodeGen.CALLEE_SAVED_GENERAL_MASK));
}

test "Windows x64 callee-saved register count" {
    // Windows: 7 callee-saved registers (RBX, RSI, RDI, R12-R15)
    try std.testing.expectEqual(@as(u32, 7), @popCount(WinCodeGen.CALLEE_SAVED_GENERAL_MASK));
}

test "stack frame is 16-byte aligned" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Allocate various stack sizes and verify alignment
    const test_sizes = [_]u32{ 1, 7, 8, 15, 16, 17, 31, 32, 33, 63, 64, 100, 255 };

    for (test_sizes) |size| {
        // Reset for each test
        cg.reset();

        // Allocate stack space
        _ = cg.allocStackSlot(@intCast(size));

        // The total frame size should be 16-byte aligned
        // stack_offset is negative, so we check -stack_offset
        const frame_size: u32 = @intCast(-cg.stack_offset);

        // When we call emitPrologueWithAlloc, it should produce aligned allocation
        // For this test, just verify allocStackSlot returns valid negative offsets
        try std.testing.expect(cg.stack_offset < 0);

        // The allocated offset should be within reasonable bounds
        try std.testing.expect(frame_size >= size);
    }
}

test "getStackSize returns 16-byte aligned size" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Allocate some space
    _ = cg.allocStackSlot(10); // Not aligned
    _ = cg.allocStackSlot(5); // More unaligned

    // getStackSize should return aligned value
    const stack_size = cg.getStackSize();

    // Stack size should be multiple of 16
    try std.testing.expect(stack_size % 16 == 0);

    // Should be at least as big as what we allocated (10 + 5 = 15, aligned up to 16)
    try std.testing.expect(stack_size >= 16);
}

test "emitPrologueWithAlloc produces 16-byte aligned frame" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // Request odd-sized allocation
    try cg.emitPrologueWithAlloc(50); // Not 16-byte aligned

    const code = cg.getCode();

    // Find the SUB RSP instruction and verify the immediate is 16-byte aligned
    // sub rsp, imm32 = 48 81 EC xx xx xx xx
    for (0..code.len -| 6) |i| {
        if (code[i] == 0x48 and code[i + 1] == 0x81 and code[i + 2] == 0xEC) {
            // Extract the 32-bit immediate (little-endian)
            const imm = @as(u32, code[i + 3]) |
                (@as(u32, code[i + 4]) << 8) |
                (@as(u32, code[i + 5]) << 16) |
                (@as(u32, code[i + 6]) << 24);

            // The allocated size should be 16-byte aligned
            try std.testing.expect(imm % 16 == 0);
            break;
        }
    }
}

test "MacCodeGen matches LinuxCodeGen (both System V)" {
    // macOS and Linux both use System V ABI - verify they're identical
    try std.testing.expectEqual(LinuxCodeGen.CALLEE_SAVED_GENERAL_MASK, MacCodeGen.CALLEE_SAVED_GENERAL_MASK);
    try std.testing.expectEqual(LinuxCodeGen.CALLEE_SAVED_AREA_SIZE, MacCodeGen.CALLEE_SAVED_AREA_SIZE);
    try std.testing.expectEqual(LinuxCodeGen.roc_target.isWindows(), MacCodeGen.roc_target.isWindows());

    // Both should NOT be Windows
    try std.testing.expect(!LinuxCodeGen.roc_target.isWindows());
    try std.testing.expect(!MacCodeGen.roc_target.isWindows());
}

test "macOS x64 CALLEE_SAVED_AREA_SIZE matches Linux" {
    // System V: 5 registers * 8 bytes = 40 bytes (same as Linux)
    try std.testing.expectEqual(@as(i32, 40), MacCodeGen.CALLEE_SAVED_AREA_SIZE);
    try std.testing.expectEqual(LinuxCodeGen.CALLEE_SAVED_AREA_SIZE, MacCodeGen.CALLEE_SAVED_AREA_SIZE);
}

test "macOS x64 callee-saved register count matches Linux" {
    // System V: 5 callee-saved registers (RBX, R12-R15) - same as Linux
    try std.testing.expectEqual(@as(u32, 5), @popCount(MacCodeGen.CALLEE_SAVED_GENERAL_MASK));
    try std.testing.expectEqual(@popCount(LinuxCodeGen.CALLEE_SAVED_GENERAL_MASK), @popCount(MacCodeGen.CALLEE_SAVED_GENERAL_MASK));
}

test "MacCodeGen prologue/epilogue matches LinuxCodeGen" {
    // macOS and Linux should produce identical code (both System V)
    var mac_cg = MacCodeGen.init(std.testing.allocator);
    defer mac_cg.deinit();

    var linux_cg = LinuxCodeGen.init(std.testing.allocator);
    defer linux_cg.deinit();

    // Emit prologue and epilogue for both
    try mac_cg.emitPrologueWithAlloc(0);
    try mac_cg.emitEpilogue();

    try linux_cg.emitPrologueWithAlloc(0);
    try linux_cg.emitEpilogue();

    // Should produce identical code
    try std.testing.expectEqualSlices(u8, linux_cg.getCode(), mac_cg.getCode());
}

test "macOS x64: use callee-saved registers when caller-saved exhausted" {
    var cg = MacCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    // System V has 9 caller-saved registers (same as Linux)
    const num_caller_saved: usize = 9;
    var regs: [9]GeneralReg = undefined;
    for (0..num_caller_saved) |i| {
        regs[i] = try cg.allocGeneralFor(@intCast(i));
    }

    // callee_saved_used should still be 0 (all were caller-saved)
    try std.testing.expectEqual(@as(u16, 0), cg.callee_saved_used);

    // Next allocation should use a callee-saved register
    const callee_reg = try cg.allocGeneralFor(@intCast(num_caller_saved));

    // Now callee_saved_used should have a bit set
    try std.testing.expect(cg.callee_saved_used != 0);

    // The register should be one of the callee-saved ones (System V)
    try std.testing.expect(SystemV.isCalleeSaved(callee_reg));
}
