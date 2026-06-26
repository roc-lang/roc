//! Frame Builders for prologue/epilogue generation.
//!
//! This module provides two builders for generating function prologues/epilogues:
//!
//! 1. **DeferredFrameBuilder** - For the deferred prologue pattern where the function
//!    body is generated first to determine which callee-saved registers are used,
//!    then the prologue is prepended. Uses MOV-based saves at fixed RBP offsets.
//!    Used by: `compileProcSpec`
//!
//! 2. **ForwardFrameBuilder** - For the forward prologue pattern where the prologue
//!    is emitted first with explicit register saves via PUSH.
//!    Used by: `emitMainPrologue`, `emitMainEpilogue`
//!
//! Key differences:
//! - DeferredFrameBuilder: Takes a bitmask of registers, saves at fixed offsets
//! - ForwardFrameBuilder: Takes explicit register list, saves in order specified

const std = @import("std");
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

        /// Computed values (set by emitPrologue, used by emitEpilogue)
        actual_stack_alloc: u32 = 0,

        /// Initialize a new frame builder with default settings.
        pub fn init() Self {
            return Self{};
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
                    @panic("caller stack-argument base register is only meaningful on aarch64");
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

        /// Calculate the prologue size without emitting.
        /// Useful for deferred prologue pattern where body is generated first.
        pub fn calculatePrologueSize(self: *const Self) u32 {
            if (!self.usesFramePointer()) return 0;

            if (is_x86_64) {
                return self.calculatePrologueSizeX86_64();
            } else if (is_aarch64) {
                return self.calculatePrologueSizeAarch64();
            } else {
                unreachable;
            }
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
                try emit.ret();
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

        /// Emit only callee-saved register saves (for pre-allocated frame pattern).
        /// Use this when the frame has already been set up and you just need to
        /// save the callee-saved registers at fixed offsets.
        pub fn emitSaveCalleeSaved(self: *const Self, emit: *EmitType) Allocator.Error!void {
            if (is_x86_64) {
                return self.emitSaveCalleeSavedX86_64(emit);
            } else if (is_aarch64) {
                return self.emitSaveCalleeSavedAarch64(emit);
            } else {
                unreachable;
            }
        }

        /// Emit only callee-saved register restores (for pre-allocated frame pattern).
        pub fn emitRestoreCalleeSaved(self: *const Self, emit: *EmitType) Allocator.Error!void {
            if (is_x86_64) {
                return self.emitRestoreCalleeSavedX86_64(emit);
            } else if (is_aarch64) {
                return self.emitRestoreCalleeSavedAarch64(emit);
            } else {
                unreachable;
            }
        }

        // ==================== x86_64 Implementation ====================

        fn calculatePrologueSizeX86_64(self: *const Self) u32 {
            var size: u32 = 0;

            // push rbp (1 byte)
            size += 1;

            // mov rbp, rsp (3 bytes: 48 89 E5)
            size += 3;

            // Stack allocation. Frames at least one page are probed
            // page-by-page; see emitPrologueX86_64 for details.
            // Use full AREA_SIZE (not just actual callee-saved bytes) because
            // stack_offset is initialized to -CALLEE_SAVED_AREA_SIZE, so locals
            // are allocated after the full reserved area.
            const callee_saved_space: u32 = @intCast(CalleeSavedInfo.AREA_SIZE);
            const total_needed = self.stack_size + callee_saved_space;
            const aligned_size = CC.alignStackSize(total_needed);
            if (aligned_size > 0) {
                if (self.needsStackProbe(aligned_size)) {
                    size += stack_probe_loop_size_x86_64;
                } else {
                    // sub rsp, imm (7 bytes for 32-bit imm: 48 81 EC xx xx xx xx)
                    size += 7;
                }
            }

            // mov [rbp-offset], reg for each callee-saved (4-8 bytes each)
            for (CalleeSavedInfo.SLOTS) |slot| {
                if ((self.callee_saved_mask & (@as(u32, 1) << @intFromEnum(slot.reg))) != 0) {
                    // MOV [rbp+disp8], reg: 3-4 bytes typically
                    // REX.W + MOV r/m64, r64 + ModR/M + disp8
                    if (slot.offset >= -128 and slot.offset < 128) {
                        size += 4; // REX + 89 + ModRM + disp8
                    } else {
                        size += 7; // REX + 89 + ModRM + disp32
                    }
                }
            }

            return size;
        }

        /// Exact byte count of the inline stack-probe loop emitted by
        /// `emitStackProbeX86_64`. Must stay in sync with that emitter —
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
        ///   mov   rax, alloc_size       ; counter — caller-saved on Windows ABI
        /// .loop:
        ///   sub   rsp, 0x1000           ; lower one page
        ///   mov   [rsp], eax            ; touch the new top to commit it
        ///   sub   eax, 0x1000
        ///   cmp   eax, 0x1000
        ///   ja    .loop                 ; rel32 — sized in calculatePrologueSize
        ///   sub   rsp, rax              ; allocate remaining bytes
        ///   mov   [rsp], eax            ; probe the final page — MSVC's
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
            try emit.ret();
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

        fn calculatePrologueSizeAarch64(self: *const Self) u32 {
            var size: u32 = 0;

            const callee_saved_space: u32 = @intCast(CalleeSavedInfo.AREA_SIZE);
            const total_frame: u32 = 16 + callee_saved_space + self.stack_size;
            const aligned_frame = CC.alignStackSize(total_frame);

            if (aligned_frame <= 504) {
                // stp x29, x30, [sp, #-N]! (4 bytes)
                size += 4;
            } else if (self.needsStackProbe(aligned_frame)) {
                size += stackProbeSizeAarch64(aligned_frame);
                // stp x29, x30, [sp]
                size += 4;
            } else {
                // sub sp, sp, #N (4 bytes) + stp x29, x30, [sp] (4 bytes)
                size += 8;
            }

            // mov x29, sp (4 bytes)
            size += 4;

            // stp for each used pair (4 bytes each)
            for (CalleeSavedInfo.PAIRS) |pair| {
                if (self.isPairUsed(pair)) {
                    size += 4;
                }
            }

            if (self.caller_stack_arg_base_reg != null) {
                size += if (aligned_frame <= 4095) 4 else 8;
            }

            return size;
        }

        fn emitStackSubAarch64(emit: *EmitType, size: u32) Allocator.Error!void {
            std.debug.assert(size > 0);
            std.debug.assert(size <= stack_probe_page_size);
            if (size == stack_probe_page_size) {
                try emit.subRegRegImm12Shifted(.w64, .ZRSP, .ZRSP, 1, true);
            } else {
                try emit.subRegRegImm12(.w64, .ZRSP, .ZRSP, @intCast(size));
            }
        }

        fn stackProbeSizeAarch64(alloc_size: u32) u32 {
            std.debug.assert(alloc_size >= stack_probe_page_size);
            return @intCast(((alloc_size + stack_probe_page_size - 1) / stack_probe_page_size) * 8);
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
                // No probe needed — frame is smaller than one page.
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
            try emit.ret();
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

/// ForwardFrameBuilder - For the push-based pattern where prologue is emitted first.
///
/// Used when you know exactly which registers to save before generating the body.
/// Registers are saved via PUSH in the order specified, which is more efficient
/// for a small, known set of registers.
///
/// Usage:
/// ```zig
/// var builder = ForwardFrameBuilder(Emit).init(&emit);
/// builder.saveViaPush(.RBX);
/// builder.saveViaPush(.R12);
/// builder.setStackSize(1024);
/// const initial_offset = try builder.emitPrologue();
/// // ... generate body with stack_offset = initial_offset ...
/// try builder.emitEpilogue();
/// ```
///
/// Callers: emitMainPrologue, emitMainEpilogue
pub fn ForwardFrameBuilder(comptime EmitType: type) type {
    const CC_EMIT = EmitType.CC;
    const GeneralReg = EmitType.GeneralReg;
    const roc_target = EmitType.roc_target;
    const is_aarch64 = roc_target.toCpuArch() == .aarch64 or roc_target.toCpuArch() == .aarch64_be;

    return struct {
        const Self = @This();

        emit: *EmitType,

        // Configuration (set before emitPrologue)
        stack_size: u32 = 0,
        push_regs: [8]?GeneralReg = .{ null, null, null, null, null, null, null, null },
        push_count: u8 = 0,
        stack_probe_required: bool = false,

        // State set by emitPrologue for use by emitEpilogue
        actual_stack_alloc: u32 = 0,

        /// Initialize a new forward frame builder
        pub fn init(emit: *EmitType) Self {
            return Self{ .emit = emit };
        }

        /// Set the stack size needed for local variables.
        /// The actual allocation will be aligned to 16 bytes.
        pub fn setStackSize(self: *Self, size: u32) void {
            self.stack_size = size;
        }

        pub fn setStackProbeRequired(self: *Self, required: bool) void {
            self.stack_probe_required = required;
        }

        /// Mark a register to be saved via PUSH before stack allocation.
        /// Registers are pushed in the order they're added.
        pub fn saveViaPush(self: *Self, reg: GeneralReg) void {
            if (self.push_count < self.push_regs.len) {
                self.push_regs[self.push_count] = reg;
                self.push_count += 1;
            }
        }

        /// Emit function prologue. Returns the initial stack_offset for allocStackSlot.
        ///
        /// Sequence (x86_64):
        /// 1. push rbp; mov rbp, rsp (establish frame pointer)
        /// 2. push <regs> (callee-saved via push, in order added)
        /// 3. sub rsp, <aligned_size> (allocate stack space)
        pub fn emitPrologue(self: *Self) Allocator.Error!i32 {
            if (comptime is_aarch64) {
                return self.emitPrologueAarch64();
            } else {
                return self.emitPrologueX86_64();
            }
        }

        fn emitPrologueX86_64(self: *Self) Allocator.Error!i32 {
            // 1. Establish frame pointer
            try self.emit.pushReg(.RBP);
            try self.emit.movRegReg(.w64, .RBP, .RSP);

            // 2. Push callee-saved registers (in order added)
            for (self.push_regs[0..self.push_count]) |maybe_reg| {
                if (maybe_reg) |reg| {
                    try self.emit.pushReg(reg);
                }
            }

            // Calculate stack offset after pushes (negative offset from RBP)
            const push_bytes: i32 = @as(i32, self.push_count) * 8;

            // 3. Allocate stack space (aligned to 16 bytes)
            // When push_count is odd, RSP is misaligned by 8 after the pushes.
            // computeActualStackAlloc adds 8 bytes of padding to realign RSP.
            self.actual_stack_alloc = self.computeActualStackAlloc();

            if (self.actual_stack_alloc > 0) {
                if (self.needsStackProbe(self.actual_stack_alloc)) {
                    try self.emitStackProbeX86_64(self.actual_stack_alloc);
                } else {
                    try self.emit.subRegImm32(.w64, CC_EMIT.STACK_PTR, @intCast(self.actual_stack_alloc));
                }
            }

            // Return initial stack_offset: accounts for push-saved registers
            // First allocation should be below the pushed registers
            return -push_bytes;
        }

        fn emitPrologueAarch64(self: *Self) Allocator.Error!i32 {
            // aarch64: Use STP for FP/LR and additional register pairs
            // stp x29, x30, [sp, #-16]!  (push FP and LR, pre-decrement)
            try self.emit.stpPreIndex(.w64, .FP, .LR, .ZRSP, -2);
            // mov x29, sp (establish frame pointer)
            try self.emit.movRegReg(.w64, .FP, .ZRSP);

            // Save additional registers via STP (pairs only - aarch64 prefers paired ops)
            // If odd count, the last register is handled via stack allocation + STR offset
            const pair_count = self.push_count / 2;
            var i: u8 = 0;
            while (i < pair_count * 2) : (i += 2) {
                const reg1 = self.push_regs[i] orelse break;
                const reg2 = self.push_regs[i + 1] orelse break;
                try self.emit.stpPreIndex(.w64, reg1, reg2, .ZRSP, -2);
            }

            // Calculate bytes used by STP operations
            const stp_bytes: u32 = @as(u32, pair_count) * 16;

            // Allocate remaining stack space (includes odd register + locals)
            const odd_reg_space: u32 = if (self.push_count % 2 == 1) 16 else 0; // 16 for alignment
            const total_needed = self.stack_size + odd_reg_space;
            self.actual_stack_alloc = CC_EMIT.alignStackSize(total_needed);

            if (self.actual_stack_alloc > 0) {
                if (self.needsStackProbe(self.actual_stack_alloc)) {
                    try self.emitStackProbeAarch64(self.actual_stack_alloc);
                } else if (self.actual_stack_alloc <= 4095) {
                    try self.emit.subRegRegImm12(.w64, .ZRSP, .ZRSP, @intCast(self.actual_stack_alloc));
                } else {
                    try self.emit.movRegImm64(.IP0, self.actual_stack_alloc);
                    try self.emit.subRegRegReg(.w64, .ZRSP, .ZRSP, .IP0);
                }
            }

            // Handle odd register: store at [SP + actual_stack_alloc - 8]
            if (self.push_count % 2 == 1) {
                if (self.push_regs[self.push_count - 1]) |reg| {
                    // STR to a fixed offset within allocated space
                    // strRegMemUoff uses scaled unsigned offset (imm12 * 8 for .w64)
                    const odd_byte_offset: u32 = self.actual_stack_alloc - 8;
                    const odd_offset: u12 = @intCast(odd_byte_offset >> 3);
                    try self.emit.strRegMemUoff(.w64, reg, .ZRSP, odd_offset);
                }
            }

            // Return initial stack_offset: accounts for STP-saved register pairs + odd register.
            const odd_bytes: i32 = if (self.push_count % 2 == 1) 8 else 0;
            return -@as(i32, @intCast(stp_bytes)) - odd_bytes;
        }

        /// Emit function epilogue. Mirrors the prologue automatically.
        ///
        /// Sequence (x86_64):
        /// 1. add rsp, <aligned_size> (deallocate stack)
        /// 2. pop <regs> (restore PUSH-saved registers, reverse order)
        /// 3. pop rbp; ret
        pub fn emitEpilogue(self: *Self) Allocator.Error!void {
            if (comptime is_aarch64) {
                return self.emitEpilogueAarch64();
            } else {
                return self.emitEpilogueX86_64();
            }
        }

        fn emitEpilogueX86_64(self: *Self) Allocator.Error!void {
            // Compute actual_stack_alloc if not already set (allows separate prologue/epilogue instances)
            // Also trigger for odd push_count which needs alignment padding even with stack_size=0
            if (self.actual_stack_alloc == 0 and (self.stack_size > 0 or self.push_count % 2 == 1)) {
                self.actual_stack_alloc = self.computeActualStackAlloc();
            }

            // 1. Deallocate stack space
            if (self.actual_stack_alloc > 0) {
                try self.emit.addRegImm32(.w64, CC_EMIT.STACK_PTR, @intCast(self.actual_stack_alloc));
            }

            // 2. Pop callee-saved registers (reverse order)
            var i: i32 = @as(i32, self.push_count) - 1;
            while (i >= 0) : (i -= 1) {
                if (self.push_regs[@intCast(i)]) |reg| {
                    try self.emit.popReg(reg);
                }
            }

            // 3. Restore frame pointer and return
            try self.emit.popReg(.RBP);
            try self.emit.ret();
        }

        fn emitEpilogueAarch64(self: *Self) Allocator.Error!void {
            // Compute actual_stack_alloc if not already set (allows separate prologue/epilogue instances)
            if (self.actual_stack_alloc == 0 and (self.stack_size > 0 or self.push_count % 2 == 1)) {
                self.actual_stack_alloc = self.computeActualStackAlloc();
            }

            // Restore odd register first (stored at [SP + actual_stack_alloc - 8])
            if (self.push_count % 2 == 1) {
                if (self.push_regs[self.push_count - 1]) |reg| {
                    // ldrRegMemUoff uses scaled unsigned offset (imm12 * 8 for .w64)
                    const odd_byte_offset: u32 = self.actual_stack_alloc - 8;
                    const odd_offset: u12 = @intCast(odd_byte_offset >> 3);
                    try self.emit.ldrRegMemUoff(.w64, reg, .ZRSP, odd_offset);
                }
            }

            // Deallocate stack space
            if (self.actual_stack_alloc > 0) {
                if (self.actual_stack_alloc <= 4095) {
                    try self.emit.addRegRegImm12(.w64, .ZRSP, .ZRSP, @intCast(self.actual_stack_alloc));
                } else {
                    try self.emit.movRegImm64(.IP0, self.actual_stack_alloc);
                    try self.emit.addRegRegReg(.w64, .ZRSP, .ZRSP, .IP0);
                }
            }

            // Restore STP-saved register pairs (reverse order)
            const pair_count = self.push_count / 2;
            var i: i32 = @as(i32, pair_count) * 2 - 2;
            while (i >= 0) : (i -= 2) {
                const reg1 = self.push_regs[@intCast(i)] orelse break;
                const reg2 = self.push_regs[@intCast(i + 1)] orelse break;
                try self.emit.ldpPostIndex(.w64, reg1, reg2, .ZRSP, 2);
            }

            // Restore FP and LR, and return
            try self.emit.ldpPostIndex(.w64, .FP, .LR, .ZRSP, 2);
            try self.emit.ret();
        }

        /// Compute the actual stack allocation size based on configuration.
        /// This is useful when you need to create separate ForwardFrameBuilder instances
        /// for prologue and epilogue (e.g., when storing frame state is impractical).
        pub fn computeActualStackAlloc(self: *const Self) u32 {
            if (comptime is_aarch64) {
                const odd_reg_space: u32 = if (self.push_count % 2 == 1) 16 else 0;
                const total_needed = self.stack_size + odd_reg_space;
                return CC_EMIT.alignStackSize(total_needed);
            } else {
                var alloc = CC_EMIT.alignStackSize(self.stack_size);
                // When push_count is odd, RSP is misaligned by 8 after the pushes
                // (push rbp + odd number of register pushes = even total pushes).
                // Add 8 bytes to realign RSP to 16 bytes.
                if (self.push_count % 2 == 1) alloc += 8;
                return alloc;
            }
        }

        fn needsStackProbe(self: *const Self, aligned_size: u32) bool {
            const backend_requires_probe = aligned_size >= stack_probe_page_size;
            const lir_requires_probe = self.stack_probe_required and backend_requires_probe;
            return lir_requires_probe or backend_requires_probe;
        }

        fn emitStackProbeX86_64(self: *Self, alloc_size: u32) Allocator.Error!void {
            try self.emit.movRegImm32(.RAX, @intCast(alloc_size));
            const loop_start = self.emit.buf.items.len;
            try self.emit.subRegImm32(.w64, .RSP, @intCast(stack_probe_page_size));
            try self.emit.movMemReg(.w32, .RSP, 0, .RAX);
            try self.emit.subRegImm32(.w32, .RAX, @intCast(stack_probe_page_size));
            try self.emit.cmpRegImm32(.w32, .RAX, @intCast(stack_probe_page_size));
            const after_ja = self.emit.buf.items.len + 6;
            const rel: i32 = @intCast(@as(i64, @intCast(loop_start)) - @as(i64, @intCast(after_ja)));
            try self.emit.jccRel32(.above, rel);
            try self.emit.subRegReg(.w64, .RSP, .RAX);
            try self.emit.movMemReg(.w32, .RSP, 0, .RAX);
        }

        fn emitStackProbeAarch64(self: *Self, alloc_size: u32) Allocator.Error!void {
            std.debug.assert(alloc_size >= stack_probe_page_size);
            var remaining = alloc_size;
            while (remaining > 0) {
                const chunk = @min(remaining, stack_probe_page_size);
                if (chunk == stack_probe_page_size) {
                    try self.emit.subRegRegImm12Shifted(.w64, .ZRSP, .ZRSP, 1, true);
                } else {
                    try self.emit.subRegRegImm12(.w64, .ZRSP, .ZRSP, @intCast(chunk));
                }
                try self.emit.strRegMemUoff(.w64, .ZRSP, .ZRSP, 0);
                remaining -= chunk;
            }
        }
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

// ForwardFrameBuilder tests (push-based pattern)

test "ForwardFrameBuilder basic prologue/epilogue x86_64" {
    const Emit = x86_64.LinuxEmit;
    const Builder = ForwardFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init(&emit);
    frame.setStackSize(64);

    const initial_offset = try frame.emitPrologue();
    // With no pushed registers, initial offset should be 0
    try std.testing.expectEqual(@as(i32, 0), initial_offset);

    try frame.emitEpilogue();

    // Should have generated: push rbp, mov rbp rsp, sub rsp N, ..., add rsp N, pop rbp, ret
    try std.testing.expect(emit.buf.items.len > 10);

    // Check for push rbp (0x55)
    try std.testing.expectEqual(@as(u8, 0x55), emit.buf.items[0]);
}

test "ForwardFrameBuilder with pushed registers x86_64" {
    const Emit = x86_64.LinuxEmit;
    const Builder = ForwardFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init(&emit);
    frame.saveViaPush(.RBX);
    frame.saveViaPush(.R12);
    frame.setStackSize(128);

    const initial_offset = try frame.emitPrologue();
    // With 2 pushed registers (16 bytes), initial offset should be -16
    try std.testing.expectEqual(@as(i32, -16), initial_offset);

    try frame.emitEpilogue();

    // Should be longer due to push/pop of RBX and R12
    try std.testing.expect(emit.buf.items.len > 20);
}

test "ForwardFrameBuilder stack alignment x86_64" {
    const Emit = x86_64.LinuxEmit;
    const Builder = ForwardFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init(&emit);
    frame.setStackSize(50); // Not 16-byte aligned

    _ = try frame.emitPrologue();

    // The actual_stack_alloc should be rounded up to 16-byte alignment
    try std.testing.expect(frame.actual_stack_alloc >= 50);
    try std.testing.expectEqual(@as(u32, 0), frame.actual_stack_alloc % 16);
}

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
    try std.testing.expectEqual(@as(u32, 0), frame.calculatePrologueSize());

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

test "DeferredFrameBuilder calculatePrologueSize x86_64" {
    const Emit = x86_64.LinuxEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var frame = Builder.init();
    frame.setStackSize(64);

    const calculated_size = frame.calculatePrologueSize();
    // Basic prologue: push rbp (1) + mov rbp,rsp (3) + sub rsp,N (7) = 11 bytes
    try std.testing.expect(calculated_size >= 11);
}

test "DeferredFrameBuilder x86_64 large frame prologue probes and matches emitted bytes" {
    const Emit = x86_64.LinuxEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    frame.setStackSize(4096);

    const calculated_size = frame.calculatePrologueSize();
    _ = try frame.emitPrologue(&emit);

    try std.testing.expect(frame.actual_stack_alloc >= 4096);
    try std.testing.expect(calculated_size > 11);
    try std.testing.expectEqual(calculated_size, @as(u32, @intCast(emit.buf.items.len)));
}

test "DeferredFrameBuilder aarch64 large frame prologue probes and matches emitted bytes" {
    const Emit = aarch64.WinEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    const x19_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X19);
    frame.setCalleeSavedMask(x19_bit);
    frame.setStackSize(4096);

    const calculated_size = frame.calculatePrologueSize();
    _ = try frame.emitPrologue(&emit);

    try std.testing.expect(frame.actual_stack_alloc >= 4096);
    try std.testing.expect(calculated_size > 16);
    try std.testing.expectEqual(calculated_size, @as(u32, @intCast(emit.buf.items.len)));
}

test "DeferredFrameBuilder aarch64 caller stack arg base prologue size matches emitted bytes" {
    const Emit = aarch64.MacEmit;
    const Builder = DeferredFrameBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Builder.init();
    const x28_bit = @as(u32, 1) << @intFromEnum(aarch64.GeneralReg.X28);
    frame.setCalleeSavedMask(x28_bit);
    frame.setStackSize(64);
    frame.setCallerStackArgBaseReg(.X28);

    const calculated_size = frame.calculatePrologueSize();
    _ = try frame.emitPrologue(&emit);

    try std.testing.expect(frame.actual_stack_alloc >= 64);
    try std.testing.expectEqual(calculated_size, @as(u32, @intCast(emit.buf.items.len)));
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
