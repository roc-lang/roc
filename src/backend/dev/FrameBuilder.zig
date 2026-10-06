//! Frame builder for prologue/epilogue generation.
//!
//! **DeferredFrameBuilder** serves the deferred prologue pattern where the function
//! body is generated first to determine which callee-saved registers are used,
//! then the prologue is prepended. It takes a bitmask of registers and uses
//! MOV-based saves at fixed RBP offsets.
//! Used by: `compileProcSpec`

const std = @import("std");
const invariant = @import("base").invariant;
const Allocator = std.mem.Allocator;

const stack_probe_page_size: u32 = 0x1000;

/// Policy for whether a function frame should establish a dedicated frame pointer.
pub const FramePointerPolicy = enum {
    /// Always establish a frame pointer in prologue/epilogue.
    always,
    /// Omit frame pointer when no locals or callee-saved slots require frame-relative access.
    omit_if_possible,

    /// Target-default frame pointer policy.
    pub fn forTarget(comptime _: anytype) FramePointerPolicy {
        // Current backend policy is explicit and centralized:
        // all supported targets always use a frame pointer today.
        return .always;
    }

    /// Resolve whether this function must establish a frame pointer.
    pub fn usesFramePointer(self: FramePointerPolicy, stack_size: u32, callee_saved_mask: u32) bool {
        return switch (self) {
            .always => true,
            .omit_if_possible => stack_size > 0 or callee_saved_mask != 0,
        };
    }
};

/// DeferredFrameBuilder - For the mask-based pattern where body is generated first.
///
/// Used when you don't know which callee-saved registers will be used until
/// after generating the function body. The prologue is generated after the body
/// and prepended to the code. Uses MOV-based saves at fixed RBP-relative offsets.
///
/// Usage:
/// ```zig
/// // 1. Generate function body (tracks callee_saved_used)
/// // 2. Create builder and emit prologue
/// var builder = DeferredFrameBuilder(Emit).init();
/// builder.setCalleeSavedMask(codegen.callee_saved_used);
/// builder.setStackSize(@intCast(-codegen.stack_offset));
/// _ = try builder.emitPrologue(&emit);
/// // 3. Prepend prologue to body
/// ```
///
/// Callers: compileProcSpec, x86_64/CodeGen, aarch64/CodeGen
pub fn DeferredFrameBuilder(comptime EmitType: type) type {
    const roc_target = EmitType.roc_target;
    const is_x86_64 = roc_target.toCpuArch() == .x86_64;
    const is_aarch64 = roc_target.toCpuArch() == .aarch64 or roc_target.toCpuArch() == .aarch64_be;
    const is_windows = roc_target.isWindows();

    const GeneralReg = EmitType.GeneralReg;
    const CC = EmitType.CC;

    // Architecture-specific callee-saved register definitions
    const CalleeSavedInfo = if (is_x86_64)
        X86_64CalleeSavedInfo(is_windows, GeneralReg)
    else if (is_aarch64)
        Aarch64CalleeSavedInfo(GeneralReg)
    else
        @compileError("Unsupported architecture for DeferredFrameBuilder");

    return struct {
        const Self = @This();

        /// Bitmask of callee-saved registers that need to be saved/restored.
        /// On x86_64: bits correspond to GeneralReg enum values
        /// On aarch64: bits correspond to GeneralReg enum values
        callee_saved_mask: u32 = 0,

        /// Stack space needed for local variables (in bytes).
        /// This does NOT include space for callee-saved registers.
        stack_size: u32 = 0,

        /// AArch64-only register that should hold the caller's stack-argument
        /// base, which is the value of SP on callee entry. Deferred AArch64
        /// prologues set FP to the bottom of the allocated frame, so incoming
        /// stack arguments are at FP + actual_stack_alloc rather than a fixed
        /// FP offset.
        caller_stack_arg_base_reg: ?GeneralReg = null,

        /// LIR requires this proc's native stack frame to be probed if it is at
        /// least one page. Large actual frames are probed regardless; this bit
        /// records the explicit aggregate-safety contract from LIR.
        stack_probe_required: bool = false,

        /// Frame-pointer strategy for this function.
        frame_pointer_policy: FramePointerPolicy = FramePointerPolicy.forTarget(roc_target),

        /// Bytes of caller-allocated argument block this function removes
        /// from the stack when it returns. An internal Roc procedure owns the
        /// block holding its stack-passed arguments, so a tail call can
        /// replace that block with the next callee's.
        callee_pop: u32 = 0,

        /// Computed values (set by emitPrologue, used by emitEpilogue)
        actual_stack_alloc: u32 = 0,

        /// Initialize a new frame builder with default settings.
        pub fn init() Self {
            return Self{};
        }

        /// Set how many bytes of incoming argument block the epilogue pops.
        pub fn setCalleePop(self: *Self, bytes: u32) void {
            self.callee_pop = bytes;
        }

        /// Set the callee-saved registers that need to be saved/restored.
        /// The mask is a bitmask where bit N indicates register N should be saved.
        pub fn setCalleeSavedMask(self: *Self, mask: u32) void {
            self.callee_saved_mask = mask;
        }

        /// Set the stack space needed for local variables.
        pub fn setStackSize(self: *Self, size: u32) void {
            self.stack_size = size;
        }

        /// Ask the AArch64 prologue to materialize the caller's stack-argument
        /// base into a callee-saved register. The caller is responsible for
        /// marking that register as used so the normal callee-saved save/restore
        /// path preserves the incoming value before this prologue overwrites it.
        pub fn setCallerStackArgBaseReg(self: *Self, reg: GeneralReg) void {
            if (!is_aarch64) {
                if (std.debug.runtime_safety) {
                    invariant("{s}", .{"caller stack-argument base register is only meaningful on aarch64"});
                }
                unreachable;
            }
            self.caller_stack_arg_base_reg = reg;
        }

        pub fn setStackProbeRequired(self: *Self, required: bool) void {
            self.stack_probe_required = required;
        }

        /// Override the frame-pointer strategy for this function.
        pub fn setFramePointerPolicy(self: *Self, policy: FramePointerPolicy) void {
            self.frame_pointer_policy = policy;
        }

        /// Whether this frame must establish a frame pointer.
        pub fn usesFramePointer(self: *const Self) bool {
            return self.frame_pointer_policy.usesFramePointer(self.stack_size, self.callee_saved_mask);
        }

        /// Emit function prologue.
        /// Returns the initial stack_offset for use with stack slot allocation.
        pub fn emitPrologue(self: *Self, emit: *EmitType) Allocator.Error!i32 {
            if (!self.usesFramePointer()) {
                self.actual_stack_alloc = 0;
                return 0;
            }

            if (is_x86_64) {
                return self.emitPrologueX86_64(emit);
            } else if (is_aarch64) {
                return self.emitPrologueAarch64(emit);
            } else {
                unreachable;
            }
        }

        /// Emit function epilogue.
        pub fn emitEpilogue(self: *Self, emit: *EmitType) Allocator.Error!void {
            if (!self.usesFramePointer()) {
                try self.emitReturn(emit);
                return;
            }

            if (is_x86_64) {
                return self.emitEpilogueX86_64(emit);
            } else if (is_aarch64) {
                return self.emitEpilogueAarch64(emit);
            } else {
                unreachable;
            }
        }

        /// Return to the caller, removing this function's argument block.
        fn emitReturn(self: *const Self, emit: *EmitType) Allocator.Error!void {
            if (self.callee_pop == 0) return emit.ret();
            if (is_x86_64) {
                if (self.callee_pop <= std.math.maxInt(u16)) {
                    try emit.retImm16(@intCast(self.callee_pop));
                } else {
                    try emit.pop(.R11);
                    try emit.addRegImm32(.w64, .RSP, @intCast(self.callee_pop));
                    try emit.jmpReg(.R11);
                }
            } else if (is_aarch64) {
                if (self.callee_pop <= 4095) {
                    try emit.addRegRegImm12(.w64, .ZRSP, .ZRSP, @intCast(self.callee_pop));
                } else {
                    try emit.movRegImm64(.IP0, self.callee_pop);
                    try emit.addRegRegReg(.w64, .ZRSP, .ZRSP, .IP0);
                }
                try emit.ret();
            } else {
                unreachable;
            }
        }

        /// AArch64 tail-call exit shared by every frame-replacing call in one
        /// function. On entry `delta_reg` holds the signed distance from this
        /// function's entry stack pointer to the callee's, and `target_reg`
        /// the callee's address. The stack pointer moves from this frame's
        /// base to the callee's entry value in one step, so the argument
        /// block already written for the callee is never below it.
        /// Emit only the callee-saved register restores, for the shared exit
        /// that frame-replacing calls reach before the body's own epilogue.
        pub fn emitRestoreCalleeSaved(self: *const Self, emit: *EmitType) Allocator.Error!void {
            if (is_x86_64) {
                return self.emitRestoreCalleeSavedX86_64(emit);
            } else if (is_aarch64) {
                return self.emitRestoreCalleeSavedAarch64(emit);
            } else {
                unreachable;
            }
        }

        pub fn emitTailCallExitAarch64(self: *Self, emit: *EmitType, delta_reg: GeneralReg, target_reg: GeneralReg) Allocator.Error!void {
            if (!is_aarch64) @compileError("emitTailCallExitAarch64 is only meaningful on aarch64");
            if (self.actual_stack_alloc == 0) {
                const callee_saved_space: u32 = @intCast(CalleeSavedInfo.AREA_SIZE);
                const total_frame: u32 = 16 + callee_saved_space + self.stack_size;
                self.actual_stack_alloc = CC.alignStackSize(total_frame);
            }
            try self.emitRestoreCalleeSavedAarch64(emit);
            try emit.movRegImm64(.IP0, self.actual_stack_alloc);
            try emit.addRegRegReg(.w64, .IP0, .IP0, delta_reg);
            try emit.ldpSignedOffset(.w64, .FP, .LR, .ZRSP, 0);
            try emit.addRegRegReg(.w64, .ZRSP, .ZRSP, .IP0);
            try emit.brReg(target_reg);
        }

        // ==================== x86_64 Implementation ====================

        /// Exact byte count of the inline stack-probe loop emitted by
        /// `emitStackProbeX86_64`. Must stay in sync with that emitter—
        /// `calculatePrologueSize` reports it to deferred-prologue patching.
        const stack_probe_loop_size_x86_64: u32 =
            // mov rax, imm32 (always REX.W + C7 + ModRM + imm32 = 7 bytes)
            7 +
            // sub rsp, 0x1000 (REX.W + 81 + ModRM + imm32 = 7 bytes)
            7 +
            // mov [rsp], eax (89 + ModRM disp32 + SIB + disp32 = 7 bytes)
            7 +
            // sub eax, 0x1000 (RAX short form: 2D + imm32 = 5 bytes)
            5 +
            // cmp eax, 0x1000 (RAX short form: 3D + imm32 = 5 bytes)
            5 +
            // ja rel32 (0F 87 + rel32 = 6 bytes)
            6 +
            // sub rsp, rax (REX.W + 29 + ModRM = 3 bytes)
            3 +
            // mov [rsp], eax (final probe, 7 bytes)
            7;

        /// True when this frame must probe the stack page-by-page. Direct
        /// stack-pointer movement by at least one page can skip guard pages;
        /// probing is required by LIR for large aggregate frames and is also
        /// emitted for any other large actual frame the backend constructs.
        fn needsStackProbe(self: *const Self, aligned_size: u32) bool {
            const backend_requires_probe = aligned_size >= stack_probe_page_size;
            const lir_requires_probe = self.stack_probe_required and backend_requires_probe;
            return lir_requires_probe or backend_requires_probe;
        }

        fn emitPrologueX86_64(self: *Self, emit: *EmitType) Allocator.Error!i32 {
            // 1. push rbp
            try emit.pushReg(.RBP);

            // 2. mov rbp, rsp
            try emit.movRegReg(.w64, .RBP, .RSP);

            // 3. Calculate and allocate stack space
            // CRITICAL: On Windows x64, there's no red zone. We must allocate
            // stack space BEFORE saving callee-saved registers to [RBP-offset].
            // Use full AREA_SIZE because stack_offset is initialized to
            // -CALLEE_SAVED_AREA_SIZE, so locals start after the full reserved area.
            const callee_saved_space: u32 = @intCast(CalleeSavedInfo.AREA_SIZE);
            const total_needed = self.stack_size + callee_saved_space;
            self.actual_stack_alloc = CC.alignStackSize(total_needed);

            if (self.actual_stack_alloc > 0) {
                if (self.needsStackProbe(self.actual_stack_alloc)) {
                    try emitStackProbeX86_64(emit, self.actual_stack_alloc);
                } else {
                    try emit.subRegImm32(.w64, .RSP, @intCast(self.actual_stack_alloc));
                }
            }

            // 4. Save callee-saved registers at fixed RBP offsets
            try self.emitSaveCalleeSavedX86_64(emit);

            // Return initial stack offset (0 for x86_64 since we use negative RBP offsets)
            // Callers use negative offsets from RBP for locals
            return 0;
        }

        /// Emit an inline stack-probe loop. Required for any native frame at
        /// least one page so each guard page is touched in order. A direct
        /// `sub rsp, N` can skip guard pages and yield a delayed fault when a
        /// later access reaches an uncommitted page.
        ///
        /// Layout (must remain byte-exact with `stack_probe_loop_size_x86_64`):
        ///   mov   rax, alloc_size       ; counter—caller-saved on Windows ABI
        /// .loop:
        ///   sub   rsp, 0x1000           ; lower one page
        ///   mov   [rsp], eax            ; touch the new top to commit it
        ///   sub   eax, 0x1000
        ///   cmp   eax, 0x1000
        ///   ja    .loop                 ; rel32—sized in calculatePrologueSize
        ///   sub   rsp, rax              ; allocate remaining bytes
        ///   mov   [rsp], eax            ; probe the final page—MSVC's
        ///                                ; __chkstk probes every page including
        ///                                ; the partial one; without this probe
        ///                                ; the remainder slips past the guard.
        fn emitStackProbeX86_64(emit: *EmitType, alloc_size: u32) Allocator.Error!void {
            const buf_before_emit = emit.buf.items.len;
            try emit.movRegImm32(.RAX, @intCast(alloc_size));
            const loop_start = emit.buf.items.len;
            try emit.subRegImm32(.w64, .RSP, @intCast(stack_probe_page_size));
            try emit.movMemReg(.w32, .RSP, 0, .RAX);
            try emit.subRegImm32(.w32, .RAX, @intCast(stack_probe_page_size));
            try emit.cmpRegImm32(.w32, .RAX, @intCast(stack_probe_page_size));
            // ja loop_start: offset is from end-of-jcc to target.
            const after_ja = emit.buf.items.len + 6; // jccRel32 emits 6 bytes
            const rel: i32 = @intCast(@as(i64, @intCast(loop_start)) - @as(i64, @intCast(after_ja)));
            try emit.jccRel32(.above, rel);
            try emit.subRegReg(.w64, .RSP, .RAX);
            try emit.movMemReg(.w32, .RSP, 0, .RAX);
            std.debug.assert(emit.buf.items.len - buf_before_emit == stack_probe_loop_size_x86_64);
        }

        fn emitEpilogueX86_64(self: *Self, emit: *EmitType) Allocator.Error!void {
            // Recompute if needed (for separate epilogue instances)
            if (self.actual_stack_alloc == 0 and self.stack_size > 0) {
                const callee_saved_space: u32 = @intCast(CalleeSavedInfo.AREA_SIZE);
                const total_needed = self.stack_size + callee_saved_space;
                self.actual_stack_alloc = CC.alignStackSize(total_needed);
            }

            // 1. Restore callee-saved registers
            try self.emitRestoreCalleeSavedX86_64(emit);

            // 2. mov rsp, rbp (restore stack pointer)
            try emit.movRegReg(.w64, .RSP, .RBP);

            // 3. pop rbp
            try emit.popReg(.RBP);

            // 4. ret
            try self.emitReturn(emit);
        }

        fn emitSaveCalleeSavedX86_64(self: *const Self, emit: *EmitType) Allocator.Error!void {
            for (CalleeSavedInfo.SLOTS) |slot| {
                if ((self.callee_saved_mask & (@as(u32, 1) << @intFromEnum(slot.reg))) != 0) {
                    try emit.movMemReg(.w64, .RBP, slot.offset, slot.reg);
                }
            }
        }

        fn emitRestoreCalleeSavedX86_64(self: *const Self, emit: *EmitType) Allocator.Error!void {
            for (CalleeSavedInfo.SLOTS) |slot| {
                if ((self.callee_saved_mask & (@as(u32, 1) << @intFromEnum(slot.reg))) != 0) {
                    try emit.movRegMem(.w64, slot.reg, .RBP, slot.offset);
                }
            }
        }

        // ==================== aarch64 Implementation ====================

        fn emitStackSubAarch64(emit: *EmitType, size: u32) Allocator.Error!void {
            std.debug.assert(size > 0);
            std.debug.assert(size <= stack_probe_page_size);
            if (size == stack_probe_page_size) {
                try emit.subRegRegImm12Shifted(.w64, .ZRSP, .ZRSP, 1, true);
            } else {
                try emit.subRegRegImm12(.w64, .ZRSP, .ZRSP, @intCast(size));
            }
        }

        fn emitStackProbeAarch64(emit: *EmitType, alloc_size: u32) Allocator.Error!void {
            std.debug.assert(alloc_size >= stack_probe_page_size);
            var remaining = alloc_size;
            while (remaining > 0) {
                const chunk = @min(remaining, stack_probe_page_size);
                try emitStackSubAarch64(emit, chunk);
                try emit.strRegMemUoff(.w64, .ZRSP, .ZRSP, 0);
                remaining -= chunk;
            }
        }

        fn emitCallerStackArgBaseAarch64(self: *Self, emit: *EmitType, aligned_frame: u32) Allocator.Error!void {
            const reg = self.caller_stack_arg_base_reg orelse return;
            if (aligned_frame <= 4095) {
                try emit.addRegRegImm12(.w64, reg, .FP, @intCast(aligned_frame));
            } else {
                try emit.movRegImm64(.IP0, aligned_frame);
                try emit.addRegRegReg(.w64, reg, .FP, .IP0);
            }
        }

        fn emitPrologueAarch64(self: *Self, emit: *EmitType) Allocator.Error!i32 {
            // Calculate total frame size.
            // Use the FULL callee-saved area size (not just used pairs) because
            // saves are at fixed offsets: pair 0 at [FP+16], pair 4 at [FP+80], etc.
            const callee_saved_space: u32 = @intCast(CalleeSavedInfo.AREA_SIZE);
            const total_frame: u32 = 16 + callee_saved_space + self.stack_size;
            const aligned_frame = CC.alignStackSize(total_frame);

            // 1. Allocate frame and save FP/LR
            if (aligned_frame <= 504) {
                // Small frame: stp pre-index (scaled offset fits in i7).
                // No probe needed—frame is smaller than one page.
                const scaled_offset: i7 = @intCast(@divExact(-@as(i32, @intCast(aligned_frame)), 8));
                try emit.stpPreIndex(.w64, .FP, .LR, .ZRSP, scaled_offset);
            } else if (self.needsStackProbe(aligned_frame)) {
                // Probe each guard page before committing the large frame.
                try emitStackProbeAarch64(emit, aligned_frame);
                try emit.stpSignedOffset(.w64, .FP, .LR, .ZRSP, 0);
            } else if (aligned_frame <= 4095) {
                // Medium frame (non-Windows, or Windows < one page is impossible here).
                try emit.subRegRegImm12(.w64, .ZRSP, .ZRSP, @intCast(aligned_frame));
                try emit.stpSignedOffset(.w64, .FP, .LR, .ZRSP, 0);
            } else {
                // Large frame (non-Windows): load size into scratch register, sub from sp.
                try emit.movRegImm64(.IP0, aligned_frame);
                try emit.subRegRegReg(.w64, .ZRSP, .ZRSP, .IP0);
                try emit.stpSignedOffset(.w64, .FP, .LR, .ZRSP, 0);
            }

            // 2. mov x29, sp (set frame pointer)
            try emit.movRegReg(.w64, .FP, .ZRSP);

            // 3. Save callee-saved register pairs at fixed FP offsets
            try self.emitSaveCalleeSavedAarch64(emit);

            // 4. The caller's outgoing stack arguments are above this frame.
            // Materialize their base after saving callee-saved registers so the
            // original value of this register is preserved in the save area.
            try self.emitCallerStackArgBaseAarch64(emit, aligned_frame);

            self.actual_stack_alloc = aligned_frame;

            // Return initial stack offset (positive from FP for aarch64)
            // Locals start after FP/LR (16) + callee-saved area
            return 16 + @as(i32, @intCast(callee_saved_space));
        }

        fn emitEpilogueAarch64(self: *Self, emit: *EmitType) Allocator.Error!void {
            // Recompute if needed (use full callee-saved area size for fixed offsets)
            if (self.actual_stack_alloc == 0) {
                const callee_saved_space: u32 = @intCast(CalleeSavedInfo.AREA_SIZE);
                const total_frame: u32 = 16 + callee_saved_space + self.stack_size;
                self.actual_stack_alloc = CC.alignStackSize(total_frame);
            }

            // 1. Restore callee-saved register pairs
            try self.emitRestoreCalleeSavedAarch64(emit);

            // 2. Restore FP/LR and deallocate frame
            if (self.actual_stack_alloc <= 504) {
                // Small frame: ldp post-index (scaled offset fits in i7)
                const scaled_offset: i7 = @intCast(@divExact(@as(i32, @intCast(self.actual_stack_alloc)), 8));
                try emit.ldpPostIndex(.w64, .FP, .LR, .ZRSP, scaled_offset);
            } else if (self.actual_stack_alloc <= 4095) {
                // Medium frame: ldp without writeback, then add sp with imm12
                try emit.ldpSignedOffset(.w64, .FP, .LR, .ZRSP, 0);
                try emit.addRegRegImm12(.w64, .ZRSP, .ZRSP, @intCast(self.actual_stack_alloc));
            } else {
                // Large frame: ldp without writeback, then add sp via scratch register
                try emit.ldpSignedOffset(.w64, .FP, .LR, .ZRSP, 0);
                try emit.movRegImm64(.IP0, self.actual_stack_alloc);
                try emit.addRegRegReg(.w64, .ZRSP, .ZRSP, .IP0);
            }

            // 3. ret
            try self.emitReturn(emit);
        }

        fn emitSaveCalleeSavedAarch64(self: *const Self, emit: *EmitType) Allocator.Error!void {
            // Save at fixed FP offsets: [FP+16], [FP+32], etc.
            var scaled_offset: i7 = 2; // Start at 16 bytes (2 * 8)
            for (CalleeSavedInfo.PAIRS) |pair| {
                if (self.isPairUsed(pair)) {
                    try emit.stpSignedOffset(.w64, pair[0], pair[1], .FP, scaled_offset);
                }
                scaled_offset += 2; // Each pair is 16 bytes
            }
        }

        fn emitRestoreCalleeSavedAarch64(self: *const Self, emit: *EmitType) Allocator.Error!void {
            // Restore from fixed FP offsets: [FP+16], [FP+32], etc.
            var scaled_offset: i7 = 2; // Start at 16 bytes (2 * 8)
            for (CalleeSavedInfo.PAIRS) |pair| {
                if (self.isPairUsed(pair)) {
                    try emit.ldpSignedOffset(.w64, pair[0], pair[1], .FP, scaled_offset);
                }
                scaled_offset += 2;
            }
        }

        fn isPairUsed(self: *const Self, pair: [2]GeneralReg) bool {
            const mask1 = @as(u32, 1) << @intFromEnum(pair[0]);
            const mask2 = @as(u32, 1) << @intFromEnum(pair[1]);
            return (self.callee_saved_mask & (mask1 | mask2)) != 0;
        }

        /// Callee-saved register slots (for direct access if needed)
        pub const CALLEE_SAVED_SLOTS = if (is_x86_64) CalleeSavedInfo.SLOTS else @compileError("CALLEE_SAVED_SLOTS only available on x86_64");

        /// Callee-saved register pairs (for direct access if needed)
        pub const CALLEE_SAVED_PAIRS = if (is_aarch64) CalleeSavedInfo.PAIRS else @compileError("CALLEE_SAVED_PAIRS only available on aarch64");

        /// Size of the callee-saved area when all registers are saved
        pub const CALLEE_SAVED_AREA_SIZE: i32 = CalleeSavedInfo.AREA_SIZE;
    };
}

/// x86_64 callee-saved register information
fn X86_64CalleeSavedInfo(comptime is_windows: bool, comptime GeneralReg: type) type {
    return struct {
        /// Callee-saved register stack slots (relative to RBP)
        pub const SLOTS = if (is_windows)
            [_]struct { reg: GeneralReg, offset: i32 }{
                .{ .reg = .RBX, .offset = -8 },
                .{ .reg = .RSI, .offset = -16 },
                .{ .reg = .RDI, .offset = -24 },
                .{ .reg = .R12, .offset = -32 },
                .{ .reg = .R13, .offset = -40 },
                .{ .reg = .R14, .offset = -48 },
                .{ .reg = .R15, .offset = -56 },
            }
        else
            [_]struct { reg: GeneralReg, offset: i32 }{
                .{ .reg = .RBX, .offset = -8 },
                .{ .reg = .R12, .offset = -16 },
                .{ .reg = .R13, .offset = -24 },
                .{ .reg = .R14, .offset = -32 },
                .{ .reg = .R15, .offset = -40 },
            };

        /// Size of callee-saved area (all registers)
        pub const AREA_SIZE: i32 = if (is_windows) 56 else 40;
    };
}

/// aarch64 callee-saved register information
fn Aarch64CalleeSavedInfo(comptime GeneralReg: type) type {
    return struct {
        /// Callee-saved register pairs for STP/LDP
        pub const PAIRS = [_][2]GeneralReg{
            .{ .X19, .X20 },
            .{ .X21, .X22 },
            .{ .X23, .X24 },
            .{ .X25, .X26 },
            .{ .X27, .X28 },
        };

        /// Size of callee-saved area (5 pairs * 16 bytes)
        pub const AREA_SIZE: i32 = 80;
    };
}

const x86_64 = @import("x86_64/mod.zig");
const aarch64 = @import("aarch64/mod.zig");

// DeferredFrameBuilder tests (mask-based pattern)

test "DeferredFrameBuilder basic prologue/epilogue x86_64" {
    const Emit = x86_64.LinuxEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    frame.setStackSize(64);
    // No callee-saved registers used

    _ = try frame.emitPrologue(&emit);
    try frame.emitEpilogue(&emit);

    // Should have generated: push rbp, mov rbp rsp, sub rsp N, mov rsp rbp, pop rbp, ret
    try std.testing.expect(emit.buf.items.len > 10);

    // Check for push rbp (0x55)
    try std.testing.expectEqual(@as(u8, 0x55), emit.buf.items[0]);
}

test "DeferredFrameBuilder omit_if_possible skips empty frame x86_64" {
    const Emit = x86_64.LinuxEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    frame.setFramePointerPolicy(.omit_if_possible);
    frame.setStackSize(0);
    frame.setCalleeSavedMask(0);

    try std.testing.expect(!frame.usesFramePointer());

    _ = try frame.emitPrologue(&emit);
    try frame.emitEpilogue(&emit);

    // No prologue emitted; epilogue is just `ret`.
    try std.testing.expectEqual(@as(usize, 1), emit.buf.items.len);
    try std.testing.expectEqual(@as(u8, 0xC3), emit.buf.items[0]);
}

test "DeferredFrameBuilder omit_if_possible keeps frame when needed x86_64" {
    const Emit = x86_64.LinuxEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    frame.setFramePointerPolicy(.omit_if_possible);
    frame.setStackSize(16);

    try std.testing.expect(frame.usesFramePointer());
    _ = try frame.emitPrologue(&emit);

    // Prologue starts with push rbp when frame pointer is used.
    try std.testing.expectEqual(@as(u8, 0x55), emit.buf.items[0]);
}

test "DeferredFrameBuilder with callee-saved mask x86_64" {
    const Emit = x86_64.LinuxEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    // Set mask for RBX (bit 3) and R12 (bit 12)
    const rbx_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.RBX);
    const r12_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R12);
    frame.setCalleeSavedMask(rbx_bit | r12_bit);
    frame.setStackSize(128);

    _ = try frame.emitPrologue(&emit);
    try frame.emitEpilogue(&emit);

    // Should include MOV saves/restores for RBX and R12
    try std.testing.expect(emit.buf.items.len > 30);
}

test "DeferredFrameBuilder stack alignment x86_64" {
    const Emit = x86_64.LinuxEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    frame.setStackSize(50); // Not 16-byte aligned

    _ = try frame.emitPrologue(&emit);

    // The actual_stack_alloc should be rounded up to 16-byte alignment
    try std.testing.expect(frame.actual_stack_alloc >= 50);
    try std.testing.expectEqual(@as(u32, 0), frame.actual_stack_alloc % 16);
}

test "DeferredFrameBuilder x86_64 large frame prologue probes" {
    const Emit = x86_64.LinuxEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    frame.setStackSize(4096);

    _ = try frame.emitPrologue(&emit);

    try std.testing.expect(frame.actual_stack_alloc >= 4096);
}

test "DeferredFrameBuilder aarch64 large frame prologue probes" {
    const Emit = aarch64.WinEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    const x19_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X19);
    frame.setCalleeSavedMask(x19_bit);
    frame.setStackSize(4096);

    _ = try frame.emitPrologue(&emit);

    try std.testing.expect(frame.actual_stack_alloc >= 4096);
}

test "DeferredFrameBuilder aarch64 caller stack arg base prologue" {
    const Emit = aarch64.MacEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    const x28_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X28);
    frame.setCalleeSavedMask(x28_bit);
    frame.setStackSize(64);
    frame.setCallerStackArgBaseReg(.X28);

    _ = try frame.emitPrologue(&emit);

    try std.testing.expect(frame.actual_stack_alloc >= 64);
}

// Windows-specific tests

test "DeferredFrameBuilder Windows x86_64 no red zone" {
    const Emit = x86_64.WinEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    // Set mask for R12 (Windows callee-saved)
    const r12_bit = @as(u32, 1) << @intFromEnum(x86_64.GeneralReg.R12);
    frame.setCalleeSavedMask(r12_bit);
    frame.setStackSize(64);

    _ = try frame.emitPrologue(&emit);
    try frame.emitEpilogue(&emit);

    // Windows requires stack allocation BEFORE mov saves
    // Verify we generated code
    try std.testing.expect(emit.buf.items.len > 15);
}
