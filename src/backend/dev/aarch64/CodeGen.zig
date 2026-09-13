//! aarch64-specific code generation.
//!
//! This module provides aarch64-specific code generation including
//! function prologues/epilogues and instruction selection.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const RocTarget = @import("roc_target").RocTarget;

const EmitMod = @import("Emit.zig");
const Registers = @import("Registers.zig");
const Call = @import("Call.zig");
const Relocation = @import("../Relocation.zig").IndexedRelocation;
const SymbolTable = @import("../SymbolTable.zig");
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
        const nop_inst: u32 = 0xD503201F;

        /// The target this CodeGen was instantiated for
        pub const roc_target = target;

        /// Number of general-purpose registers
        pub const NUM_GENERAL_REGS = 32;
        /// Number of float registers
        pub const NUM_FLOAT_REGS = 32;

        /// Initial free register masks (caller-saved registers available at function entry)
        pub const INITIAL_FREE_GENERAL: u32 = CC.CALLER_SAVED_GENERAL_MASK;
        pub const INITIAL_FREE_FLOAT: u32 = CC.CALLER_SAVED_FLOAT_MASK;

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
        symbols: SymbolTable.Table = .{},
        free_general: u32,
        free_float: u32,
        callee_saved_used: u32, // Bitmask of callee-saved regs we used

        /// Remaining callee-saved registers available (used after caller-saved exhausted)
        callee_saved_available: u32,

        /// Every branch site emitted so far, in emission order (see "Control flow").
        branch_sites: std.ArrayList(BranchSite),
        /// Patch location -> index into `branch_sites`.
        branch_site_index: std.AutoHashMapUnmanaged(usize, u32),
        /// Every site before this index is resolved or has a veneer.
        branch_open_scan: u32,
        /// Open sites without a veneer.
        branch_open_unveneered: usize,
        /// Resolved sites kept since the last compaction.
        branch_resolved: usize,
        /// Direct reach assumed for island decisions; tests lower it.
        branch_reach_limit: usize,

        pub fn init(allocator: Allocator) Self {
            return Self{
                .emit = Emit.init(allocator),
                .allocator = allocator,
                .stack_offset = 0,
                .relocations = .empty,
                .free_general = CC.CALLER_SAVED_GENERAL_MASK,
                .free_float = CC.CALLER_SAVED_FLOAT_MASK,
                .callee_saved_used = 0,
                .callee_saved_available = CALLEE_SAVED_GENERAL_MASK,
                .branch_sites = .empty,
                .branch_site_index = .empty,
                .branch_open_scan = 0,
                .branch_open_unveneered = 0,
                .branch_resolved = 0,
                .branch_reach_limit = direct_branch_reach_bytes,
            };
        }

        pub fn deinit(self: *Self) void {
            self.emit.deinit();
            self.relocations.deinit(self.allocator);
            self.symbols.deinit(self.allocator);
            self.branch_sites.deinit(self.allocator);
            self.branch_site_index.deinit(self.allocator);
        }

        pub fn reset(self: *Self) void {
            self.emit.buf.clearRetainingCapacity();
            self.relocations.clearRetainingCapacity();
            self.symbols.clearRetainingCapacity();
            self.stack_offset = 0;
            self.free_general = CC.CALLER_SAVED_GENERAL_MASK;
            self.free_float = CC.CALLER_SAVED_FLOAT_MASK;
            self.callee_saved_used = 0;
            self.callee_saved_available = CALLEE_SAVED_GENERAL_MASK;
            self.branch_sites.clearRetainingCapacity();
            self.branch_site_index.clearRetainingCapacity();
            self.branch_open_scan = 0;
            self.branch_open_unveneered = 0;
            self.branch_resolved = 0;
        }

        /// Get the generated code
        pub fn getCode(self: *Self) []const u8 {
            return self.emit.buf.items;
        }

        /// Get current code offset
        pub fn currentOffset(self: *Self) usize {
            return self.emit.buf.items.len;
        }

        // Register allocation. LirCodeGen keeps every semantic local in its
        // authoritative stable-location table; these masks manage only bounded,
        // short-lived instruction-selection temporaries.

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
            // Return to appropriate pool
            if ((CALLEE_SAVED_GENERAL_MASK & (@as(u32, 1) << idx)) != 0) {
                self.callee_saved_available |= @as(u32, 1) << idx;
            } else {
                self.free_general |= @as(u32, 1) << idx;
            }
        }

        /// Mark a fixed ABI register as in use so it won't be allocated.
        pub fn markRegisterInUse(self: *Self, reg: GeneralReg) void {
            const idx = @intFromEnum(reg);
            // Remove from free pool (it's now in use)
            self.free_general &= ~(@as(u32, 1) << idx);
            self.callee_saved_available &= ~(@as(u32, 1) << idx);
        }

        pub fn allocFloat(self: *Self) ?FloatReg {
            if (self.free_float == 0) return null;
            const bit: u5 = @intCast(@ctz(self.free_float));
            self.free_float &= ~(@as(u32, 1) << bit);
            return @enumFromInt(bit);
        }

        pub fn freeFloat(self: *Self, reg: FloatReg) void {
            const idx = @intFromEnum(reg);
            self.free_float |= @as(u32, 1) << idx;
        }

        // Stack management

        pub fn allocStack(self: *Self, size: u32) i32 {
            const aligned_size: i32 = @intCast((size + 15) & ~@as(u32, 15)); // 16-byte align
            const offset = self.stack_offset;
            self.stack_offset += aligned_size;
            return offset;
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
        pub fn emitSaveCalleeSavedToFrame(self: *Self) Allocator.Error!void {
            var builder = DeferredFrameBuilder.init();
            builder.setCalleeSavedMask(self.callee_saved_used);
            try builder.emitSaveCalleeSaved(&self.emit);
        }

        /// Emit callee-saved register restores from fixed offsets from FP
        /// Used by MonoExprCodeGen for procedures that pre-allocate the frame
        /// Restores from [FP + 16], [FP + 32], etc. for each used pair
        /// The offset is scaled by 8 for stp/ldp (i.e., offset=2 means 16 bytes)
        pub fn emitRestoreCalleeSavedFromFrame(self: *Self) Allocator.Error!void {
            var builder = DeferredFrameBuilder.init();
            builder.setCalleeSavedMask(self.callee_saved_used);
            try builder.emitRestoreCalleeSaved(&self.emit);
        }

        /// Emit function prologue (called at start of function)
        /// Note: Call this AFTER register allocation is complete to know which
        /// callee-saved registers need to be preserved.
        pub fn emitPrologue(self: *Self) Allocator.Error!void {
            var builder = DeferredFrameBuilder.init();
            builder.setCalleeSavedMask(self.callee_saved_used);
            _ = try builder.emitPrologue(&self.emit);
        }

        /// Emit function epilogue and return
        pub fn emitEpilogue(self: *Self) Allocator.Error!void {
            var builder = DeferredFrameBuilder.init();
            builder.setCalleeSavedMask(self.callee_saved_used);
            try builder.emitEpilogue(&self.emit);
        }

        /// Emit stack frame setup with given local size
        pub fn emitStackAlloc(self: *Self, size: u32) Allocator.Error!void {
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
        pub fn emitAdd(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) Allocator.Error!void {
            try self.emit.addRegRegReg(width, dst, a, b);
        }

        /// Emit integer subtraction: dst = a - b
        pub fn emitSub(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) Allocator.Error!void {
            try self.emit.subRegRegReg(width, dst, a, b);
        }

        /// Emit integer multiplication: dst = a * b
        pub fn emitMul(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) Allocator.Error!void {
            try self.emit.mulRegRegReg(width, dst, a, b);
        }

        /// Emit signed integer division: dst = a / b
        pub fn emitSDiv(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) Allocator.Error!void {
            try self.emit.sdivRegRegReg(width, dst, a, b);
        }

        /// Emit unsigned integer division: dst = a / b
        pub fn emitUDiv(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) Allocator.Error!void {
            try self.emit.udivRegRegReg(width, dst, a, b);
        }

        /// Emit signed integer modulo: dst = a % b
        /// Uses SDIV + MSUB: remainder = a - (a/b) * b
        pub fn emitSMod(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) Allocator.Error!void {
            // We need a temp register for the quotient
            // Since dst might be the same as a or b, we use dst for intermediate if safe
            // quotient = a / b
            try self.emit.sdivRegRegReg(width, dst, a, b);
            // remainder = a - quotient * b
            try self.emit.msubRegRegRegReg(width, dst, dst, b, a);
        }

        /// Emit unsigned integer modulo: dst = a % b
        /// Uses UDIV + MSUB: remainder = a - (a/b) * b
        pub fn emitUMod(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) Allocator.Error!void {
            // quotient = a / b
            try self.emit.udivRegRegReg(width, dst, a, b);
            // remainder = a - quotient * b
            try self.emit.msubRegRegRegReg(width, dst, dst, b, a);
        }

        /// Emit integer negation: dst = -src
        pub fn emitNeg(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) Allocator.Error!void {
            try self.emit.negRegReg(width, dst, src);
        }

        /// Emit bitwise AND: dst = a & b
        pub fn emitAnd(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) Allocator.Error!void {
            try self.emit.andRegRegReg(width, dst, a, b);
        }

        /// Emit bitwise OR: dst = a | b
        pub fn emitOr(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) Allocator.Error!void {
            try self.emit.orrRegRegReg(width, dst, a, b);
        }

        /// Emit bitwise XOR: dst = a ^ b
        pub fn emitXor(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg) Allocator.Error!void {
            try self.emit.eorRegRegReg(width, dst, a, b);
        }

        /// Emit bitwise NOT: dst = ~src
        pub fn emitNot(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg) Allocator.Error!void {
            // MVN <dst>, <src> is an alias for ORN <dst>, XZR, <src>.
            try self.emit.ornRegRegReg(width, dst, .ZRSP, src);
        }

        /// Emit bitwise XOR with immediate: dst = src ^ imm
        pub fn emitXorImm(self: *Self, width: RegisterWidth, dst: GeneralReg, src: GeneralReg, imm: i8) Allocator.Error!void {
            // Load immediate into scratch register and use EOR
            try self.emit.movRegImm32(width, .IP0, imm);
            try self.emit.eorRegRegReg(width, dst, src, .IP0);
        }

        // Comparison operations

        /// Emit comparison and set condition: dst = (a op b) ? 1 : 0
        pub fn emitCmp(self: *Self, width: RegisterWidth, dst: GeneralReg, a: GeneralReg, b: GeneralReg, cond: Emit.Condition) Allocator.Error!void {
            try self.emit.cmpRegReg(width, a, b);
            try self.emit.cset(width, dst, cond);
        }

        /// Emit float64 compare and set: dst = (a cond b) ? 1 : 0
        pub fn emitCmpF64(self: *Self, dst: GeneralReg, a: FloatReg, b: FloatReg, cond: Emit.Condition) Allocator.Error!void {
            try self.emit.fcmpRegReg(.double, a, b);
            try self.emit.cset(.w64, dst, cond);
        }

        /// Emit float32 compare and set: dst = (a cond b) ? 1 : 0
        pub fn emitCmpF32(self: *Self, dst: GeneralReg, a: FloatReg, b: FloatReg, cond: Emit.Condition) Allocator.Error!void {
            try self.emit.fcmpRegReg(.single, a, b);
            try self.emit.cset(.w64, dst, cond);
        }

        // Floating-point operations

        /// Emit float64 addition: dst = a + b
        pub fn emitAddF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) Allocator.Error!void {
            try self.emit.faddRegRegReg(.double, dst, a, b);
        }

        /// Emit float64 subtraction: dst = a - b
        pub fn emitSubF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) Allocator.Error!void {
            try self.emit.fsubRegRegReg(.double, dst, a, b);
        }

        /// Emit float64 multiplication: dst = a * b
        pub fn emitMulF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) Allocator.Error!void {
            try self.emit.fmulRegRegReg(.double, dst, a, b);
        }

        /// Emit float64 division: dst = a / b
        pub fn emitDivF64(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) Allocator.Error!void {
            try self.emit.fdivRegRegReg(.double, dst, a, b);
        }

        /// Emit float64 negation: dst = -src
        pub fn emitNegF64(self: *Self, dst: FloatReg, src: FloatReg) Allocator.Error!void {
            try self.emit.fnegRegReg(.double, dst, src);
        }

        pub fn emitAbsF64(self: *Self, dst: FloatReg, src: FloatReg) Allocator.Error!void {
            try self.emit.fabsRegReg(.double, dst, src);
        }

        pub fn emitAbsF32(self: *Self, dst: FloatReg, src: FloatReg) Allocator.Error!void {
            try self.emit.fabsRegReg(.single, dst, src);
        }

        /// Emit float32 addition: dst = a + b
        pub fn emitAddF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) Allocator.Error!void {
            try self.emit.faddRegRegReg(.single, dst, a, b);
        }

        /// Emit float32 subtraction: dst = a - b
        pub fn emitSubF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) Allocator.Error!void {
            try self.emit.fsubRegRegReg(.single, dst, a, b);
        }

        /// Emit float32 multiplication: dst = a * b
        pub fn emitMulF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) Allocator.Error!void {
            try self.emit.fmulRegRegReg(.single, dst, a, b);
        }

        /// Emit float32 division: dst = a / b
        pub fn emitDivF32(self: *Self, dst: FloatReg, a: FloatReg, b: FloatReg) Allocator.Error!void {
            try self.emit.fdivRegRegReg(.single, dst, a, b);
        }

        /// Emit float32 negation: dst = -src
        pub fn emitNegF32(self: *Self, dst: FloatReg, src: FloatReg) Allocator.Error!void {
            try self.emit.fnegRegReg(.single, dst, src);
        }

        // Memory operations

        /// Load from stack slot into register
        pub fn emitLoadStack(self: *Self, width: RegisterWidth, dst: GeneralReg, offset: i32) Allocator.Error!void {
            try self.emit.ldrRegMemSoff(width, dst, .FP, offset);
        }

        /// Store register to stack slot
        pub fn emitStoreStack(self: *Self, width: RegisterWidth, offset: i32, src: GeneralReg) Allocator.Error!void {
            try self.emit.strRegMemSoff(width, src, .FP, offset);
        }

        /// Store byte to stack slot
        pub fn emitStoreStackByte(self: *Self, offset: i32, src: GeneralReg) Allocator.Error!void {
            try self.emit.strbRegMemSoff(src, .FP, offset);
        }

        /// Store halfword (2 bytes) to stack slot
        pub fn emitStoreStackHalfword(self: *Self, offset: i32, src: GeneralReg) Allocator.Error!void {
            try self.emit.strhRegMemSoff(src, .FP, offset);
        }

        /// Load byte (zero-extended) from stack slot
        pub fn emitLoadStackByte(self: *Self, dst: GeneralReg, offset: i32) Allocator.Error!void {
            try self.emit.ldrbRegMemSoff(dst, .FP, offset);
        }

        /// Load halfword (zero-extended) from stack slot
        pub fn emitLoadStackHalfword(self: *Self, dst: GeneralReg, offset: i32) Allocator.Error!void {
            try self.emit.ldrhRegMemSoff(dst, .FP, offset);
        }

        /// Load float64 from stack slot
        pub fn emitLoadStackF64(self: *Self, dst: FloatReg, offset: i32) Allocator.Error!void {
            try self.emit.fldrRegMemSoff(.double, dst, .FP, offset);
        }

        /// Store float64 to stack slot
        pub fn emitStoreStackF64(self: *Self, offset: i32, src: FloatReg) Allocator.Error!void {
            try self.emit.fstrRegMemSoff(.double, src, .FP, offset);
        }

        /// Load float32 from stack slot.
        pub fn emitLoadStackF32(self: *Self, dst: FloatReg, offset: i32) Allocator.Error!void {
            try self.emit.fldrRegMemSoff(.single, dst, .FP, offset);
        }

        /// Store float32 to stack slot.
        pub fn emitStoreStackF32(self: *Self, offset: i32, src: FloatReg) Allocator.Error!void {
            try self.emit.fstrRegMemSoff(.single, src, .FP, offset);
        }

        pub fn emitLoadStackV128(self: *Self, dst: FloatReg, offset: i32) Allocator.Error!void {
            try self.emit.ldrQRegMemSoff(dst, .FP, offset);
        }

        pub fn emitStoreStackV128(self: *Self, offset: i32, src: FloatReg) Allocator.Error!void {
            try self.emit.strQRegMemSoff(src, .FP, offset);
        }

        pub fn emitMoveV128(self: *Self, dst: FloatReg, src: FloatReg) Allocator.Error!void {
            if (dst != src) try self.emit.movVectorRegReg(dst, src);
        }

        pub fn emitLoadV128(self: *Self, dst: FloatReg, base: GeneralReg, offset: i32) Allocator.Error!void {
            try self.emit.ldrQRegMemSoff(dst, base, offset);
        }

        pub fn emitStoreV128(self: *Self, base: GeneralReg, offset: i32, src: FloatReg) Allocator.Error!void {
            try self.emit.strQRegMemSoff(src, base, offset);
        }

        // Immediate loading

        /// Load immediate value into register
        pub fn emitLoadImm(self: *Self, dst: GeneralReg, value: i64) Allocator.Error!void {
            try self.emit.movRegImm64(dst, @bitCast(value));
        }

        pub fn emitLoadDataAddress(self: *Self, dst: GeneralReg, symbol: SymbolTable.Id) Allocator.Error!void {
            const page_offset = self.currentOffset();
            try self.emit.adrp(dst);
            const offset12 = self.currentOffset();
            try self.emit.addRegRegImm12(.w64, dst, dst, 0);
            try self.relocations.append(self.allocator, .{
                .linked_data = .{
                    .offset = @intCast(page_offset),
                    .symbol = symbol,
                    .kind = .page21,
                },
            });
            try self.relocations.append(self.allocator, .{
                .linked_data = .{
                    .offset = @intCast(offset12),
                    .symbol = symbol,
                    .kind = .pageoff12,
                },
            });
        }

        // Control flow
        //
        // B and BL carry a signed 26-bit word immediate, so a direct branch
        // reaches +/-128 MiB; B.cond carries 19 bits and reaches +/-1 MiB. A
        // site whose target lies beyond that reach branches to a veneer: the
        // PC-relative address sequence followed by BR, which reaches the whole
        // image. Veneers are grouped in islands behind a B over them, so an
        // island can sit at any instruction boundary. Every branch site is
        // registered when it is emitted: `maybeEmitBranchIsland` gives open
        // sites veneers before they age out of direct reach of the emission
        // point, and `patchJump`/`patchCall` place a veneer on demand when a
        // target that is already known turns out to be far away.

        pub const BranchSiteKind = enum(u8) { jump, cond_jump, call };

        pub const BranchSite = struct {
            loc: usize,
            kind: BranchSiteKind,
            cond: Emit.Condition = .eq,
            veneer: ?usize = null,
            target: ?usize = null,

            fn needsVeneer(self: BranchSite) bool {
                return self.target == null and self.veneer == null;
            }

            /// The word that must reach the target (or the veneer) directly.
            fn directWordLoc(self: BranchSite) usize {
                return switch (self.kind) {
                    .cond_jump => self.loc + 4,
                    .jump, .call => self.loc,
                };
            }
        };

        /// Reach of a B/BL word immediate, in bytes. `branch_reach_limit`
        /// holds the value the decisions below use; tests lower it so small
        /// buffers exercise veneers, and the margins scale with it.
        pub const direct_branch_reach_bytes: usize = 1 << 27;
        /// ADR, MOVZ, MOVK, ADD/SUB, BR.
        pub const veneer_bytes: usize = 5 * 4;

        /// Room kept when deciding that a direct encoding fits, so a prologue
        /// prepended in front of a body afterwards (a few hundred bytes) can
        /// move the site without pushing the encoding out of reach: 1 MiB at
        /// full reach.
        fn branchShiftMargin(self: *const Self) usize {
            return self.branch_reach_limit / 128;
        }

        /// Upper bound on the code emitted between two island checks: 16 MiB
        /// at full reach.
        fn islandGapMargin(self: *const Self) usize {
            return self.branch_reach_limit / 8;
        }

        /// Whether a branch word at `from_loc` may be encoded directly.
        fn directReachable(self: *const Self, from_loc: usize, target_loc: usize) bool {
            const distance = if (target_loc >= from_loc) target_loc - from_loc else from_loc - target_loc;
            return distance < self.branch_reach_limit;
        }

        pub const PcRelParts = struct { lo16: u16, hi16: u16, subtract: bool };

        /// How to reach `target_loc` from an anchor at `anchor`, as the
        /// immediates of the PC-relative address sequence. Shared by every
        /// emitter and patcher so a rewritten sequence is encoded exactly as
        /// a freshly emitted one.
        pub fn pcRelParts(anchor: usize, target_loc: usize) PcRelParts {
            const rel: i64 = @as(i64, @intCast(target_loc)) - @as(i64, @intCast(anchor));
            const subtract = rel < 0;
            const abs_rel: u64 = if (subtract) @intCast(-rel) else @intCast(rel);
            // The sequence carries a 32-bit delta; a single emit buffer past 4 GiB
            // is far beyond any real image, so trap rather than silently encoding
            // the wrong address.
            std.debug.assert(abs_rel < (1 << 32));
            return .{
                .lo16 = @truncate(abs_rel),
                .hi16 = @truncate(abs_rel >> 16),
                .subtract = subtract,
            };
        }

        fn readInst(self: *Self, loc: usize) u32 {
            return std.mem.readInt(u32, self.emit.buf.items[loc..][0..4], .little);
        }

        fn writeInst(self: *Self, loc: usize, inst: u32) void {
            std.mem.writeInt(u32, self.emit.buf.items[loc..][0..4], inst, .little);
        }

        /// Rewrite the PC-relative address sequence at `loc` to reach `target_loc`.
        fn writePcRelSequence(self: *Self, loc: usize, target_loc: usize, dst: GeneralReg, scratch: GeneralReg) void {
            const parts = pcRelParts(loc, target_loc);
            self.writeInst(loc, Emit.encodeAdrZero(dst));
            self.writeInst(loc + 4, Emit.encodeMovz64(scratch, parts.lo16, 0));
            self.writeInst(loc + 8, Emit.encodeMovk64(scratch, parts.hi16, 1));
            self.writeInst(loc + 12, Emit.encodeAddSubRegRegReg64(dst, dst, scratch, parts.subtract));
        }

        fn isBlInst(inst: u32) bool {
            return (inst >> 26) == 0b100101;
        }

        fn isAdrInst(inst: u32) bool {
            return (inst & 0x9F000000) == 0x10000000;
        }

        /// `directReachable` with room for the site to move later.
        fn directBranchFits(self: *const Self, from_loc: usize, target_loc: usize) bool {
            const distance = if (target_loc >= from_loc) target_loc - from_loc else from_loc - target_loc;
            return distance + self.branchShiftMargin() < self.branch_reach_limit;
        }

        fn registerBranchSite(self: *Self, site: BranchSite) Allocator.Error!void {
            const index: u32 = @intCast(self.branch_sites.items.len);
            try self.branch_sites.append(self.allocator, site);
            try self.branch_site_index.put(self.allocator, site.loc, index);
            self.branch_open_unveneered += 1;
        }

        fn branchSiteIndex(self: *Self, loc: usize) u32 {
            return self.branch_site_index.get(loc) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("AArch64 branch patch at 0x{x} names no registered branch site", .{loc});
                }
                unreachable;
            };
        }

        /// Emit unconditional jump (returns patch location for fixup)
        pub fn emitJump(self: *Self) Allocator.Error!usize {
            const patch_loc = self.currentOffset();
            try self.emit.b(0); // Placeholder offset
            try self.registerBranchSite(.{ .loc = patch_loc, .kind = .jump });
            return patch_loc;
        }

        /// Emit conditional jump (returns patch location for fixup)
        pub fn emitCondJump(self: *Self, cond: Emit.Condition) Allocator.Error!usize {
            const patch_loc = self.currentOffset();
            // Reserve enough space for either:
            // - a short conditional branch plus nop, or
            // - an inverted conditional branch over a long unconditional branch.
            //
            // AArch64 B.cond reaches only +/-1 MiB (imm19), while B reaches
            // +/-128 MiB (imm26). The fixed-size placeholder keeps later code
            // offsets stable when a conditional target turns out to be far away.
            try self.emit.bcond(cond, 8);
            try self.emit.b(4);
            try self.registerBranchSite(.{ .loc = patch_loc, .kind = .cond_jump, .cond = cond });
            return patch_loc;
        }

        /// Emit a BL whose target is not known yet (returns the patch location
        /// for `patchCall`).
        pub fn emitCallPlaceholder(self: *Self) Allocator.Error!usize {
            const patch_loc = self.currentOffset();
            try self.emit.bl(0);
            try self.registerBranchSite(.{ .loc = patch_loc, .kind = .call });
            return patch_loc;
        }

        /// Call a target whose code offset is already known: a direct BL when
        /// it is in reach, otherwise the inline PC-relative address sequence
        /// followed by BLR. `patchDirectCall` re-encodes either form.
        pub fn emitDirectCall(self: *Self, target_loc: usize) Allocator.Error!void {
            const loc = self.currentOffset();
            if (self.directBranchFits(loc, target_loc)) {
                try self.emit.bl(@intCast(branchByteOffset(loc, target_loc)));
                return;
            }
            const parts = pcRelParts(loc, target_loc);
            try self.emit.pcRelAddrSequence(.IP0, .IP1, parts.lo16, parts.hi16, parts.subtract);
            try self.emit.blrReg(.IP0);
        }

        /// Re-encode a call emitted by `emitDirectCall` after its site or its
        /// target moved because a prologue was prepended in front of a body.
        pub fn patchDirectCall(self: *Self, loc: usize, target_loc: usize) void {
            const inst = self.readInst(loc);
            if (isBlInst(inst)) {
                const offset_words = @divExact(branchByteOffset(loc, target_loc), 4);
                assertBranchFits(offset_words, 26, "BL");
                self.writeInst(loc, encodeBl(offset_words));
                return;
            }
            std.debug.assert(isAdrInst(inst));
            self.writePcRelSequence(loc, target_loc, .IP0, .IP1);
        }

        /// Patch a jump target
        pub fn patchJump(self: *Self, patch_loc: usize, target_loc: usize) Allocator.Error!void {
            const index = self.branchSiteIndex(patch_loc);
            if (builtin.mode == .Debug and self.branch_sites.items[index].kind == .call) {
                std.debug.panic("AArch64 patchJump called for the call site at 0x{x}", .{patch_loc});
            }
            try self.patchBranchSite(index, target_loc);
        }

        /// Patch a call emitted by `emitCallPlaceholder`.
        pub fn patchCall(self: *Self, patch_loc: usize, target_loc: usize) Allocator.Error!void {
            const index = self.branchSiteIndex(patch_loc);
            if (builtin.mode == .Debug and self.branch_sites.items[index].kind != .call) {
                std.debug.panic("AArch64 patchCall called for the jump site at 0x{x}", .{patch_loc});
            }
            try self.patchBranchSite(index, target_loc);
        }

        fn patchBranchSite(self: *Self, index: u32, target_loc: usize) Allocator.Error!void {
            const site = self.branch_sites.items[index];
            switch (site.kind) {
                .jump, .call => {
                    if (self.directReachable(site.loc, target_loc)) {
                        self.writeInst(site.loc, encodeDirectBranch(site.kind, @divExact(branchByteOffset(site.loc, target_loc), 4)));
                    } else {
                        const veneer = try self.veneerForSite(index);
                        self.writeInst(site.loc, encodeDirectBranch(site.kind, @divExact(branchByteOffset(site.loc, veneer), 4)));
                        self.writePcRelSequence(veneer, target_loc, .IP0, .IP1);
                    }
                },
                .cond_jump => {
                    const offset_words = @divExact(branchByteOffset(site.loc, target_loc), 4);
                    if (fitsSignedBits(offset_words, 19) and self.directReachable(site.loc, target_loc)) {
                        self.writeInst(site.loc, encodeBCond(site.cond, offset_words));
                        self.writeInst(site.loc + 4, nop_inst);
                    } else {
                        // The inverted condition skips the long branch in the reserved slot.
                        self.writeInst(site.loc, encodeBCond(site.cond.invert(), 2));
                        const long_loc = site.loc + 4;
                        if (self.directReachable(long_loc, target_loc)) {
                            self.writeInst(long_loc, encodeB(@divExact(branchByteOffset(long_loc, target_loc), 4)));
                        } else {
                            const veneer = try self.veneerForSite(index);
                            self.writeInst(long_loc, encodeB(@divExact(branchByteOffset(long_loc, veneer), 4)));
                            self.writePcRelSequence(veneer, target_loc, .IP0, .IP1);
                        }
                    }
                },
            }
            self.markBranchSiteResolved(index, target_loc);
        }

        fn markBranchSiteResolved(self: *Self, index: u32, target_loc: usize) void {
            const site = &self.branch_sites.items[index];
            if (site.target == null) {
                if (site.veneer == null) self.branch_open_unveneered -= 1;
                self.branch_resolved += 1;
            }
            site.target = target_loc;
        }

        fn veneerForSite(self: *Self, index: u32) Allocator.Error!usize {
            if (self.branch_sites.items[index].veneer) |veneer| return veneer;
            // An island of one, reachable because `maybeEmitBranchIsland`
            // keeps every open site within direct reach of the emission point.
            try self.emit.b(@intCast(4 + veneer_bytes));
            const veneer = self.currentOffset();
            try self.emitVeneerPlaceholder();
            self.attachVeneer(index, veneer);
            return veneer;
        }

        fn emitVeneerPlaceholder(self: *Self) Allocator.Error!void {
            try self.emit.pcRelAddrSequence(.IP0, .IP1, 0, 0, false);
            try self.emit.brReg(.IP0);
        }

        fn attachVeneer(self: *Self, index: u32, veneer: usize) void {
            const site = &self.branch_sites.items[index];
            std.debug.assert(site.veneer == null);
            if (builtin.mode == .Debug) {
                if (!fitsSignedBits(@divExact(branchByteOffset(site.directWordLoc(), veneer), 4), 26)) {
                    std.debug.panic("AArch64 branch site at 0x{x} cannot reach its veneer at 0x{x}", .{ site.loc, veneer });
                }
            }
            if (site.target == null) self.branch_open_unveneered -= 1;
            site.veneer = veneer;
        }

        fn islandNeeded(self: *const Self, oldest_loc: usize, open_count: usize, limit: usize) bool {
            const age = self.emit.buf.items.len - oldest_loc;
            return age + open_count * veneer_bytes + self.islandGapMargin() + self.branchShiftMargin() >= limit;
        }

        fn oldestOpenUnveneeredSite(self: *Self) ?BranchSite {
            const items = self.branch_sites.items;
            while (self.branch_open_scan < items.len and !items[self.branch_open_scan].needsVeneer()) {
                self.branch_open_scan += 1;
            }
            if (self.branch_open_scan >= items.len) return null;
            return items[self.branch_open_scan];
        }

        /// Call at an instruction boundary, where a jump over an island keeps
        /// control flow intact. Emits an island once the oldest open site,
        /// plus a veneer for every open site and the code that may follow
        /// before the next check, would no longer be within direct reach.
        pub fn maybeEmitBranchIsland(self: *Self) Allocator.Error!void {
            if (self.branch_open_unveneered == 0) return;
            const oldest = self.oldestOpenUnveneeredSite() orelse return;
            if (!self.islandNeeded(oldest.loc, self.branch_open_unveneered, self.branch_reach_limit)) return;
            try self.emitBranchIsland();
        }

        fn emitBranchIsland(self: *Self) Allocator.Error!void {
            // Sites are veneered oldest first until the rest sit within half
            // the reach, so consecutive islands do not chase each other
            // statement by statement.
            var count: usize = 0;
            var remaining = self.branch_open_unveneered;
            var i = self.branch_open_scan;
            while (i < self.branch_sites.items.len) : (i += 1) {
                const site = self.branch_sites.items[i];
                if (!site.needsVeneer()) continue;
                if (!self.islandNeeded(site.loc, remaining, self.branch_reach_limit / 2)) break;
                count += 1;
                remaining -= 1;
            }
            std.debug.assert(count > 0);

            try self.emit.b(@intCast(4 + count * veneer_bytes));
            i = self.branch_open_scan;
            var placed: usize = 0;
            while (placed < count) : (i += 1) {
                if (!self.branch_sites.items[i].needsVeneer()) continue;
                const veneer = self.currentOffset();
                try self.emitVeneerPlaceholder();
                self.attachVeneer(@intCast(i), veneer);
                placed += 1;
            }
        }

        /// The body [body_start, body_end) at the end of the buffer moved
        /// forward by `delta` because a prologue was prepended in front of it.
        /// Moves the records of every site, veneer and resolved target inside
        /// it, and re-encodes a resolved veneered site whose parts ended up on
        /// different sides of the boundary. A target equal to `body_start`
        /// moves only for a site inside the body: from outside, that offset
        /// names the entry, which the prepended prologue now occupies.
        pub fn shiftBranchSites(self: *Self, body_start: usize, body_end: usize, delta: usize) Allocator.Error!void {
            const items = self.branch_sites.items;
            for (items) |site| {
                if (site.loc >= body_start and site.loc < body_end) _ = self.branch_site_index.remove(site.loc);
            }
            for (items, 0..) |*site, index| {
                const loc_moved = site.loc >= body_start and site.loc < body_end;
                if (loc_moved) {
                    site.loc += delta;
                    try self.branch_site_index.put(self.allocator, site.loc, @intCast(index));
                }
                var veneer_moved = false;
                if (site.veneer) |veneer| {
                    if (veneer >= body_start and veneer < body_end) {
                        site.veneer = veneer + delta;
                        veneer_moved = true;
                    }
                }
                var target_moved = false;
                if (site.target) |target_loc| {
                    const past_entry = target_loc > body_start or (target_loc == body_start and loc_moved);
                    if (past_entry and target_loc < body_end) {
                        site.target = target_loc + delta;
                        target_moved = true;
                    }
                }
                const target_loc = site.target orelse continue;
                if (site.veneer) |veneer| {
                    if (loc_moved != veneer_moved) {
                        const word_loc = site.directWordLoc();
                        const words = @divExact(branchByteOffset(word_loc, veneer), 4);
                        const kind: BranchSiteKind = if (site.kind == .cond_jump) .jump else site.kind;
                        self.writeInst(word_loc, encodeDirectBranch(kind, words));
                    }
                    if (veneer_moved != target_moved) self.writePcRelSequence(veneer, target_loc, .IP0, .IP1);
                } else if (loc_moved != target_moved) {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("AArch64 resolved branch at 0x{x} straddles the shifted body [0x{x}, 0x{x})", .{ site.loc, body_start, body_end });
                    }
                    unreachable;
                }
            }
        }

        /// Drop resolved sites. Only valid where nothing can patch them again,
        /// which is between top-level procedures.
        pub fn compactBranchSites(self: *Self) Allocator.Error!void {
            const items = self.branch_sites.items;
            if (self.branch_resolved < 4096 or self.branch_resolved * 2 < items.len) return;
            self.branch_site_index.clearRetainingCapacity();
            var kept: usize = 0;
            for (items) |site| {
                if (site.target != null) continue;
                items[kept] = site;
                try self.branch_site_index.put(self.allocator, site.loc, @intCast(kept));
                kept += 1;
            }
            self.branch_sites.shrinkRetainingCapacity(kept);
            self.branch_open_scan = 0;
            self.branch_resolved = 0;
        }

        fn branchByteOffset(from_loc: usize, target_loc: usize) i64 {
            const offset = @as(i64, @intCast(target_loc)) - @as(i64, @intCast(from_loc));
            std.debug.assert(@mod(offset, 4) == 0);
            return offset;
        }

        fn fitsSignedBits(value: i64, comptime bits: u6) bool {
            const min = -(@as(i64, 1) << (bits - 1));
            const max = (@as(i64, 1) << (bits - 1)) - 1;
            return value >= min and value <= max;
        }

        fn assertBranchFits(offset_words: i64, comptime bits: u6, comptime kind: []const u8) void {
            if (fitsSignedBits(offset_words, bits)) return;

            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "AArch64 {s} target out of range: word offset {d} does not fit in signed {d}-bit immediate",
                    .{ kind, offset_words, bits },
                );
            }
            unreachable;
        }

        fn encodeDirectBranch(kind: BranchSiteKind, offset_words: i64) u32 {
            return switch (kind) {
                .jump => encodeB(offset_words),
                .call => encodeBl(offset_words),
                .cond_jump => unreachable,
            };
        }

        fn encodeB(offset_words: i64) u32 {
            assertBranchFits(offset_words, 26, "B");
            const imm26: u26 = @bitCast(@as(i26, @intCast(offset_words)));
            return (@as(u32, 0b000101) << 26) | imm26;
        }

        fn encodeBl(offset_words: i64) u32 {
            assertBranchFits(offset_words, 26, "BL");
            const imm26: u26 = @bitCast(@as(i26, @intCast(offset_words)));
            return (@as(u32, 0b100101) << 26) | imm26;
        }

        fn encodeBCond(cond: Emit.Condition, offset_words: i64) u32 {
            assertBranchFits(offset_words, 19, "B.cond");
            const imm19: u19 = @bitCast(@as(i19, @intCast(offset_words)));
            return (@as(u32, 0b01010100) << 24) |
                (@as(u32, imm19) << 5) |
                @intFromEnum(cond);
        }
        /// Emit function call with relocation
        pub fn emitCall(self: *Self, symbol: SymbolTable.Id) Allocator.Error!void {
            const offset = self.currentOffset();
            try self.emit.bl(0); // Placeholder
            try self.relocations.append(self.allocator, .{
                .linked_function = .{
                    .offset = @intCast(offset),
                    .symbol = symbol,
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

    try cg.emitAddF32(.V0, .V1, .V2);
    try cg.emitSubF32(.V3, .V4, .V5);
    try cg.emitMulF32(.V6, .V7, .V8);
    try cg.emitDivF32(.V9, .V10, .V11);
    try cg.emitAddF64(.V0, .V1, .V2);
    try cg.emitSubF64(.V3, .V4, .V5);
    try cg.emitMulF64(.V6, .V7, .V8);
    try cg.emitDivF64(.V9, .V10, .V11);

    try std.testing.expect(cg.getCode().len > 0);
}

test "float allocation never relocates an allocated register" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    const count: usize = @popCount(LinuxCodeGen.INITIAL_FREE_FLOAT);
    var regs: [LinuxCodeGen.NUM_FLOAT_REGS]FloatReg = undefined;
    for (0..count) |i| {
        regs[i] = cg.allocFloat().?;
    }

    const stack_offset = cg.stack_offset;
    const code_len = cg.getCode().len;
    try std.testing.expectEqual(@as(?FloatReg, null), cg.allocFloat());
    try std.testing.expectEqual(stack_offset, cg.stack_offset);
    try std.testing.expectEqual(code_len, cg.getCode().len);

    for (regs[0..count]) |reg| cg.freeFloat(reg);
}

test "vector allocation excludes AAPCS64 partial-width callee-saved registers" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    const count: usize = @popCount(LinuxCodeGen.INITIAL_FREE_FLOAT);
    for (0..count) |_| {
        const reg = cg.allocFloat().?;
        const index = @intFromEnum(reg);
        try std.testing.expect(index <= 7 or index >= 16);
    }
    try std.testing.expectEqual(@as(?FloatReg, null), cg.allocFloat());
}

test "general allocation reports exhaustion after all allocatable registers" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    const count: usize = @popCount(LinuxCodeGen.INITIAL_FREE_GENERAL) +
        @popCount(LinuxCodeGen.CALLEE_SAVED_GENERAL_MASK);
    var regs: [LinuxCodeGen.NUM_GENERAL_REGS]GeneralReg = undefined;
    for (0..count) |i| {
        regs[i] = cg.allocGeneral().?;
    }

    try std.testing.expectEqual(@as(?GeneralReg, null), cg.allocGeneral());
    for (regs[0..count]) |reg| cg.freeGeneral(reg);
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

test "patch conditional jump keeps near targets short" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    const patch = try cg.emitCondJump(.ne);
    try cg.emit.movRegImm64(.X0, 1);
    const target = cg.currentOffset();

    try cg.patchJump(patch, target);

    const code = cg.getCode();
    const cond_inst = std.mem.readInt(u32, code[patch..][0..4], .little);
    const reserved_inst = std.mem.readInt(u32, code[patch + 4 ..][0..4], .little);

    try std.testing.expectEqual(@as(u4, @intFromEnum(EmitMod.Emit(.arm64linux).Condition.ne)), @as(u4, @truncate(cond_inst)));
    try std.testing.expectEqual(@as(u19, @intCast(@divExact(@as(i64, @intCast(target - patch)), 4))), @as(u19, @truncate(cond_inst >> 5)));
    try std.testing.expectEqual(@as(u32, 0xD503201F), reserved_inst);
}

test "patch conditional jump expands far targets" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    const patch = try cg.emitCondJump(.ne);
    const target = patch + 0x110000;

    try cg.patchJump(patch, target);

    const code = cg.getCode();
    const skip_inst = std.mem.readInt(u32, code[patch..][0..4], .little);
    const branch_inst = std.mem.readInt(u32, code[patch + 4 ..][0..4], .little);

    try std.testing.expectEqual(@as(u4, @intFromEnum(EmitMod.Emit(.arm64linux).Condition.eq)), @as(u4, @truncate(skip_inst)));
    try std.testing.expectEqual(@as(u19, 2), @as(u19, @truncate(skip_inst >> 5)));
    try std.testing.expectEqual(@as(u6, 0b000101), @as(u6, @truncate(branch_inst >> 26)));
    try std.testing.expectEqual(@as(u26, @intCast(@divExact(@as(i64, @intCast(target - (patch + 4))), 4))), @as(u26, @truncate(branch_inst)));
}

const TestEmit = EmitMod.Emit(.arm64linux);
const test_veneer_bytes = LinuxCodeGen.veneer_bytes;
const br_ip0_inst: u32 = 0xD61F0200;
const blr_ip0_inst: u32 = 0xD63F0200;

fn testInst(cg: *LinuxCodeGen, loc: usize) u32 {
    return std.mem.readInt(u32, cg.getCode()[loc..][0..4], .little);
}

fn testPad(cg: *LinuxCodeGen, bytes: usize) !void {
    try cg.emit.buf.appendNTimes(cg.allocator, 0, bytes);
}

fn expectDirectBranch(comptime opcode: u6, inst: u32, from: usize, to: usize) !void {
    try std.testing.expectEqual(@as(u6, opcode), @as(u6, @truncate(inst >> 26)));
    const words: i64 = @divExact(@as(i64, @intCast(to)) - @as(i64, @intCast(from)), 4);
    try std.testing.expectEqual(@as(u26, @bitCast(@as(i26, @intCast(words)))), @as(u26, @truncate(inst)));
}

fn expectVeneer(cg: *LinuxCodeGen, veneer: usize, target: usize) !void {
    const parts = LinuxCodeGen.pcRelParts(veneer, target);
    try std.testing.expectEqual(TestEmit.encodeAdrZero(.IP0), testInst(cg, veneer));
    try std.testing.expectEqual(TestEmit.encodeMovz64(.IP1, parts.lo16, 0), testInst(cg, veneer + 4));
    try std.testing.expectEqual(TestEmit.encodeMovk64(.IP1, parts.hi16, 1), testInst(cg, veneer + 8));
    try std.testing.expectEqual(TestEmit.encodeAddSubRegRegReg64(.IP0, .IP0, .IP1, parts.subtract), testInst(cg, veneer + 12));
    try std.testing.expectEqual(br_ip0_inst, testInst(cg, veneer + 16));
}

/// Prepend `prologue_bytes` zero bytes in front of the body at the end of the
/// buffer, the way deferred prologue emission moves a finished body.
fn testPrependPrologue(cg: *LinuxCodeGen, body_start: usize, prologue_bytes: usize) !void {
    const body = try std.testing.allocator.dupe(u8, cg.getCode()[body_start..]);
    defer std.testing.allocator.free(body);
    const body_end = cg.currentOffset();
    cg.emit.buf.shrinkRetainingCapacity(body_start);
    try testPad(cg, prologue_bytes);
    try cg.emit.buf.appendSlice(cg.allocator, body);
    try cg.shiftBranchSites(body_start, body_end, prologue_bytes);
}

test "far jump is routed through an on-demand veneer" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();
    cg.branch_reach_limit = 4096;

    const patch = try cg.emitJump();
    try testPad(&cg, 8192);
    const target = cg.currentOffset();
    try cg.patchJump(patch, target);

    const veneer = target + 4;
    try std.testing.expectEqual(veneer + test_veneer_bytes, cg.currentOffset());
    try expectDirectBranch(0b000101, testInst(&cg, target), target, veneer + test_veneer_bytes);
    try expectDirectBranch(0b000101, testInst(&cg, patch), patch, veneer);
    try expectVeneer(&cg, veneer, target);
    try std.testing.expectEqual(@as(?usize, veneer), cg.branch_sites.items[0].veneer);
    try std.testing.expectEqual(@as(?usize, target), cg.branch_sites.items[0].target);
}

test "far conditional jump reaches its target through a veneer" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();
    cg.branch_reach_limit = 4096;

    const patch = try cg.emitCondJump(.ne);
    try testPad(&cg, 8192);
    const target = cg.currentOffset();
    try cg.patchJump(patch, target);

    const veneer = target + 4;
    const skip_inst = testInst(&cg, patch);
    try std.testing.expectEqual(@as(u4, @intFromEnum(TestEmit.Condition.eq)), @as(u4, @truncate(skip_inst)));
    try std.testing.expectEqual(@as(u19, 2), @as(u19, @truncate(skip_inst >> 5)));
    try expectDirectBranch(0b000101, testInst(&cg, patch + 4), patch + 4, veneer);
    try expectVeneer(&cg, veneer, target);
}

test "far call placeholder is patched through a veneer" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();
    cg.branch_reach_limit = 4096;

    const call = try cg.emitCallPlaceholder();
    try testPad(&cg, 8192);
    const target = cg.currentOffset();
    try cg.patchCall(call, target);

    const veneer = target + 4;
    try expectDirectBranch(0b100101, testInst(&cg, call), call, veneer);
    try expectVeneer(&cg, veneer, target);
}

test "direct call uses BL within reach and an address sequence beyond it" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();
    cg.branch_reach_limit = 4096;

    try testPad(&cg, 16);
    const near = cg.currentOffset();
    try cg.emitDirectCall(0);
    try std.testing.expectEqual(near + 4, cg.currentOffset());
    try expectDirectBranch(0b100101, testInst(&cg, near), near, 0);

    try testPad(&cg, 8192);
    const far = cg.currentOffset();
    try cg.emitDirectCall(0);
    try std.testing.expectEqual(far + test_veneer_bytes, cg.currentOffset());
    const parts = LinuxCodeGen.pcRelParts(far, 0);
    try std.testing.expectEqual(TestEmit.encodeAdrZero(.IP0), testInst(&cg, far));
    try std.testing.expectEqual(TestEmit.encodeMovz64(.IP1, parts.lo16, 0), testInst(&cg, far + 4));
    try std.testing.expectEqual(TestEmit.encodeMovk64(.IP1, parts.hi16, 1), testInst(&cg, far + 8));
    try std.testing.expectEqual(TestEmit.encodeAddSubRegRegReg64(.IP0, .IP0, .IP1, parts.subtract), testInst(&cg, far + 12));
    try std.testing.expectEqual(blr_ip0_inst, testInst(&cg, far + 16));

    cg.patchDirectCall(far, 8);
    const moved = LinuxCodeGen.pcRelParts(far, 8);
    try std.testing.expectEqual(TestEmit.encodeMovz64(.IP1, moved.lo16, 0), testInst(&cg, far + 4));
    cg.patchDirectCall(near, 8);
    try expectDirectBranch(0b100101, testInst(&cg, near), near, 8);
}

test "island gives an aging open site a veneer and leaves direct encodings alone" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();
    cg.branch_reach_limit = 65536;

    const patch = try cg.emitJump();
    try testPad(&cg, 40000);
    try cg.maybeEmitBranchIsland();
    try std.testing.expectEqual(@as(usize, 40004), cg.currentOffset());

    try testPad(&cg, 20000);
    const island = cg.currentOffset();
    try cg.maybeEmitBranchIsland();
    try std.testing.expectEqual(island + 4 + test_veneer_bytes, cg.currentOffset());
    try expectDirectBranch(0b000101, testInst(&cg, island), island, island + 4 + test_veneer_bytes);
    try std.testing.expectEqual(@as(?usize, island + 4), cg.branch_sites.items[0].veneer);
    try std.testing.expectEqual(@as(usize, 0), cg.branch_open_unveneered);

    const target = cg.currentOffset();
    try cg.patchJump(patch, target);
    try expectDirectBranch(0b000101, testInst(&cg, patch), patch, target);
    try std.testing.expectEqual(TestEmit.encodeMovz64(.IP1, 0, 0), testInst(&cg, island + 8));
}

test "island veneers only the sites that would otherwise leave reach" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();
    cg.branch_reach_limit = 65536;

    _ = try cg.emitJump();
    try testPad(&cg, 50000);
    _ = try cg.emitJump();
    try testPad(&cg, 12000);
    const island = cg.currentOffset();
    try cg.maybeEmitBranchIsland();

    try std.testing.expectEqual(island + 4 + test_veneer_bytes, cg.currentOffset());
    try std.testing.expectEqual(@as(?usize, island + 4), cg.branch_sites.items[0].veneer);
    try std.testing.expectEqual(@as(?usize, null), cg.branch_sites.items[1].veneer);
    try std.testing.expectEqual(@as(usize, 1), cg.branch_open_unveneered);
}

test "shift re-encodes a veneered site whose veneer moved with the body" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();
    cg.branch_reach_limit = 4096;

    const patch = try cg.emitJump();
    try testPad(&cg, 4092);
    const body_start = cg.currentOffset();
    try testPad(&cg, 64);
    const target = cg.currentOffset();
    try cg.patchJump(patch, target);
    const veneer = target + 4;
    try expectDirectBranch(0b000101, testInst(&cg, patch), patch, veneer);

    try testPrependPrologue(&cg, body_start, 16);

    const site = cg.branch_sites.items[0];
    try std.testing.expectEqual(patch, site.loc);
    try std.testing.expectEqual(@as(?usize, veneer + 16), site.veneer);
    try std.testing.expectEqual(@as(?usize, target + 16), site.target);
    try expectDirectBranch(0b000101, testInst(&cg, patch), patch, veneer + 16);
    try expectVeneer(&cg, veneer + 16, target + 16);
}

test "shift rekeys the sites inside the moved body" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    const patch = try cg.emitJump();
    try testPad(&cg, 12);
    try testPrependPrologue(&cg, 0, 16);

    const target = cg.currentOffset();
    try cg.patchJump(patch + 16, target);
    try expectDirectBranch(0b000101, testInst(&cg, patch + 16), patch + 16, target);
}

test "compaction drops resolved sites and keeps open ones findable" {
    var cg = LinuxCodeGen.init(std.testing.allocator);
    defer cg.deinit();

    var i: usize = 0;
    while (i < 5000) : (i += 1) {
        const patch = try cg.emitJump();
        try cg.patchJump(patch, patch + 4);
    }
    const call = try cg.emitCallPlaceholder();
    try cg.compactBranchSites();
    try std.testing.expectEqual(@as(usize, 1), cg.branch_sites.items.len);

    const target = cg.currentOffset();
    try cg.patchCall(call, target);
    try expectDirectBranch(0b100101, testInst(&cg, call), call, target);
}
