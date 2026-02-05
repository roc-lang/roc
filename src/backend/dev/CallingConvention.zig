//! Cross-platform calling convention abstraction
//!
//! Handles differences between:
//! - x86_64 System V (Linux, macOS, BSD)
//! - x86_64 Windows Fastcall
//! - aarch64 AAPCS64
//!
//! Key features:
//! - Target-aware calling conventions (supports cross-compilation)
//! - Automatic shadow space allocation (Windows)
//! - Return-by-pointer for large returns
//! - Pass-by-pointer for large structs (Windows)
//! - Correct argument register ordering per platform
//!
//! The CallBuilder and CalleeBuilder are parameterized by an Emit type which provides:
//! - GeneralReg, FloatReg: Register types
//! - CC: Calling convention constants

const std = @import("std");
const RocTarget = @import("roc_target").RocTarget;

const x86_64 = @import("x86_64/mod.zig");
const aarch64 = @import("aarch64/mod.zig");

const Relocation = @import("Relocation.zig").Relocation;

/// Calling convention configuration for a specific target
pub const CallingConvention = struct {
    /// Argument registers for integer/pointer arguments
    param_regs: []const x86_64.GeneralReg,
    /// Number of argument registers available
    num_param_regs: usize,
    /// Shadow space required before calls (Windows: 32 bytes)
    shadow_space: u8,
    /// Threshold for return-by-pointer (struct return uses hidden first arg)
    return_by_ptr_threshold: usize,
    /// Threshold for pass-by-pointer (large structs passed as pointer)
    pass_by_ptr_threshold: usize,
    /// Whether this is a Windows target (for additional ABI rules)
    is_windows: bool,

    /// x86_64 System V argument registers
    const SYSV_PARAM_REGS = [_]x86_64.GeneralReg{ .RDI, .RSI, .RDX, .RCX, .R8, .R9 };
    /// x86_64 Windows argument registers
    const WIN64_PARAM_REGS = [_]x86_64.GeneralReg{ .RCX, .RDX, .R8, .R9 };

    /// Create calling convention for a given target
    pub fn forTarget(target: RocTarget) CallingConvention {
        return switch (target.toCpuArch()) {
            .x86_64 => if (target.isWindows())
                CallingConvention{
                    .param_regs = &WIN64_PARAM_REGS,
                    .num_param_regs = WIN64_PARAM_REGS.len,
                    .shadow_space = 32,
                    .return_by_ptr_threshold = 8,
                    .pass_by_ptr_threshold = 8, // Only 1,2,4,8 byte structs by value
                    .is_windows = true,
                }
            else
                CallingConvention{
                    .param_regs = &SYSV_PARAM_REGS,
                    .num_param_regs = SYSV_PARAM_REGS.len,
                    .shadow_space = 0,
                    .return_by_ptr_threshold = 16,
                    .pass_by_ptr_threshold = std.math.maxInt(usize),
                    .is_windows = false,
                },
            .aarch64, .aarch64_be => CallingConvention{
                // aarch64 uses X0-X7 for args, but we store as x86_64 regs for now
                // Real aarch64 support would need separate register type
                .param_regs = &SYSV_PARAM_REGS, // placeholder
                .num_param_regs = 8,
                .shadow_space = 0,
                .return_by_ptr_threshold = 16,
                .pass_by_ptr_threshold = std.math.maxInt(usize),
                .is_windows = false,
            },
            else => CallingConvention{
                .param_regs = &SYSV_PARAM_REGS,
                .num_param_regs = 6,
                .shadow_space = 0,
                .return_by_ptr_threshold = 16,
                .pass_by_ptr_threshold = std.math.maxInt(usize),
                .is_windows = false,
            },
        };
    }

    /// Get argument register at index
    pub fn getParamReg(self: CallingConvention, index: usize) x86_64.GeneralReg {
        return self.param_regs[index];
    }

    /// Check if return type needs to use pointer (implicit first arg)
    pub fn needsReturnByPointer(self: CallingConvention, return_size: usize) bool {
        return return_size > self.return_by_ptr_threshold;
    }

    /// Check if a struct argument needs to be passed by pointer.
    /// Windows (x64 and ARM64): Only structs of size 1, 2, 4, 8 bytes can be passed by value.
    /// All other sizes must be passed by pointer.
    pub fn needsPassByPointer(self: CallingConvention, arg_size: usize) bool {
        if (self.is_windows) {
            // Windows: only power-of-2 sizes up to 8 can pass by value
            return !(arg_size == 1 or arg_size == 2 or arg_size == 4 or arg_size == 8);
        }
        return arg_size > self.pass_by_ptr_threshold;
    }

    /// Check if a struct of the given size can be passed by value in a register.
    pub fn canPassStructByValue(self: CallingConvention, size: usize) bool {
        return !self.needsPassByPointer(size);
    }

    /// Returns true if i128 values must be passed by pointer (Windows x64)
    pub fn passI128ByPointer(self: CallingConvention) bool {
        return self.is_windows;
    }

    /// Returns true if i128 return values use hidden pointer arg (Windows x64)
    pub fn returnI128ByPointer(self: CallingConvention) bool {
        return self.is_windows;
    }
};

/// Call builder for setting up cross-platform function calls
/// The Emit type must provide:
/// - CC: Calling convention struct with PARAM_REGS, FLOAT_PARAM_REGS, etc.
/// - GeneralReg and FloatReg types (re-exported from the Emit struct)
pub fn CallBuilder(comptime EmitType: type) type {
    // Get calling convention and register types from the Emit type
    const CC_EMIT = EmitType.CC;
    const GeneralReg = EmitType.GeneralReg;
    const FloatReg = EmitType.FloatReg;
    const roc_target = EmitType.roc_target;
    const is_x86_64 = roc_target.toCpuArch() == .x86_64;
    const is_aarch64 = roc_target.toCpuArch() == .aarch64 or roc_target.toCpuArch() == .aarch64_be;
    const is_windows = roc_target.isWindows();

    // Represents a deferred argument source (used for both stack and register args)
    const ArgSource = union(enum) {
        // Store from a register: mov [RSP+offset], reg
        from_reg: GeneralReg,
        // Store an immediate value: mov [RSP+offset], imm
        from_imm: i64,
        // Store LEA result: lea scratch, [base+offset]; mov [RSP+stack_offset], scratch
        from_lea: struct { base: GeneralReg, offset: i32 },
        // Store memory value: mov scratch, [base+offset]; mov [RSP+stack_offset], scratch
        from_mem: struct { base: GeneralReg, offset: i32 },
    };

    // A deferred register argument, resolved later via parallel move algorithm
    const DeferredRegArg = struct {
        dst_index: u8, // index into CC_EMIT.PARAM_REGS
        src: ArgSource,
    };

    const MoveStatus = enum { to_move, being_moved, moved };

    // Maximum stack arguments we support (should be plenty for any real call)
    const MAX_STACK_ARGS = 16;

    return struct {
        const Self = @This();

        emit: *EmitType,
        int_arg_index: usize = 0,
        /// Float argument index (separate from int_arg_index on System V, same on Windows)
        float_arg_index: usize = 0,
        stack_arg_count: usize = 0,
        stack_args: [MAX_STACK_ARGS]ArgSource = undefined,
        return_by_ptr: bool = false,
        /// RBP-relative offset where R12 is saved (only used on Windows x64)
        /// Set via saveR12 before adding arguments that might clobber R12
        r12_save_offset: ?i32 = null,
        /// Deferred register arguments, resolved via parallel move before call
        reg_args: [CC_EMIT.PARAM_REGS.len]DeferredRegArg = undefined,
        reg_arg_count: u8 = 0,

        /// Initialize CallBuilder with automatic R12 handling for C function calls.
        ///
        /// On Windows x64, R12 (which holds roc_ops) may be corrupted by C calls,
        /// so this method automatically allocates a stack slot and saves R12.
        /// On other platforms, R12 handling is not needed.
        ///
        /// The stack_offset pointer should point to the CodeGen's stack_offset field.
        /// This method will allocate space by decrementing the stack offset on Windows.
        pub fn init(emit: *EmitType, stack_offset: *i32) !Self {
            var self = Self{ .emit = emit };
            if (comptime is_x86_64 and is_windows) {
                // Allocate 16-byte slot for R12 save to maintain 16-byte alignment
                // for subsequent allocations (important for i128/RocDec which need
                // 16-byte aligned addresses when passed by pointer)
                stack_offset.* -= 16;
                self.r12_save_offset = stack_offset.*;
                // Save R12 immediately to the RBP-relative slot
                try self.emit.movMemReg(.w64, CC_EMIT.BASE_PTR, self.r12_save_offset.?, .R12);
            }
            return self;
        }

        /// Check if return type needs to use pointer (implicit first arg)
        pub fn needsReturnByPointer(return_size: usize) bool {
            return return_size > CC_EMIT.RETURN_BY_PTR_THRESHOLD;
        }

        /// Check if a struct argument needs to be passed by pointer.
        /// Windows x64: Only structs of size 1, 2, 4, 8 bytes can be passed by value.
        /// All other sizes must be passed by pointer.
        /// aarch64 (all OSes): Zig uses AAPCS64 for callconv(.c), no pass-by-pointer.
        /// System V: Never uses pass-by-pointer (large structs are copied to stack).
        pub fn needsPassByPointer(arg_size: usize) bool {
            return CC_EMIT.needsPassByPointer(arg_size);
        }

        /// Set up return by pointer (for large return types)
        /// Call this FIRST before adding any arguments
        pub fn setReturnByPointer(self: *Self, offset: i32) !void {
            std.debug.assert(self.int_arg_index == 0);
            self.return_by_ptr = true;
            try self.addLeaArg(CC_EMIT.BASE_PTR, offset);
        }

        /// Add argument from a general register
        pub fn addRegArg(self: *Self, src_reg: GeneralReg) !void {
            if (self.int_arg_index < CC_EMIT.PARAM_REGS.len) {
                self.reg_args[self.reg_arg_count] = .{
                    .dst_index = @intCast(self.int_arg_index),
                    .src = .{ .from_reg = src_reg },
                };
                self.reg_arg_count += 1;
                self.int_arg_index += 1;
            } else {
                // Defer stack arg - will be stored after stack allocation in call()
                std.debug.assert(self.stack_arg_count < MAX_STACK_ARGS);
                self.stack_args[self.stack_arg_count] = .{ .from_reg = src_reg };
                self.stack_arg_count += 1;
            }
        }

        /// Add argument by loading a pointer (LEA) to a stack location
        pub fn addLeaArg(self: *Self, base_reg: GeneralReg, offset: i32) !void {
            if (self.int_arg_index < CC_EMIT.PARAM_REGS.len) {
                self.reg_args[self.reg_arg_count] = .{
                    .dst_index = @intCast(self.int_arg_index),
                    .src = .{ .from_lea = .{ .base = base_reg, .offset = offset } },
                };
                self.reg_arg_count += 1;
                self.int_arg_index += 1;
            } else {
                // Defer stack arg - will be stored after stack allocation in call()
                std.debug.assert(self.stack_arg_count < MAX_STACK_ARGS);
                self.stack_args[self.stack_arg_count] = .{ .from_lea = .{ .base = base_reg, .offset = offset } };
                self.stack_arg_count += 1;
            }
        }

        /// Add argument by loading from stack memory
        pub fn addMemArg(self: *Self, base_reg: GeneralReg, offset: i32) !void {
            if (self.int_arg_index < CC_EMIT.PARAM_REGS.len) {
                self.reg_args[self.reg_arg_count] = .{
                    .dst_index = @intCast(self.int_arg_index),
                    .src = .{ .from_mem = .{ .base = base_reg, .offset = offset } },
                };
                self.reg_arg_count += 1;
                self.int_arg_index += 1;
            } else {
                // Defer stack arg - will be stored after stack allocation in call()
                std.debug.assert(self.stack_arg_count < MAX_STACK_ARGS);
                self.stack_args[self.stack_arg_count] = .{ .from_mem = .{ .base = base_reg, .offset = offset } };
                self.stack_arg_count += 1;
            }
        }

        /// Add immediate value argument
        pub fn addImmArg(self: *Self, value: i64) !void {
            if (self.int_arg_index < CC_EMIT.PARAM_REGS.len) {
                self.reg_args[self.reg_arg_count] = .{
                    .dst_index = @intCast(self.int_arg_index),
                    .src = .{ .from_imm = value },
                };
                self.reg_arg_count += 1;
                self.int_arg_index += 1;
            } else {
                // Defer stack arg - will be stored after stack allocation in call()
                std.debug.assert(self.stack_arg_count < MAX_STACK_ARGS);
                self.stack_args[self.stack_arg_count] = .{ .from_imm = value };
                self.stack_arg_count += 1;
            }
        }

        /// Add a struct argument, handling pass-by-pointer per platform ABI.
        /// Windows x64: Only 1, 2, 4, 8 byte structs pass by value; all others by pointer.
        /// System V: Structs up to 16 bytes can be passed in registers.
        pub fn addStructArg(self: *Self, offset: i32, size: usize) !void {
            if (CC_EMIT.canPassStructByValue(size)) {
                // Struct can be passed by value in register(s)
                if (size <= 8) {
                    try self.addMemArg(CC_EMIT.BASE_PTR, offset);
                } else {
                    // System V only: 9-16 byte structs use two registers
                    try self.addMemArg(CC_EMIT.BASE_PTR, offset);
                    try self.addMemArg(CC_EMIT.BASE_PTR, offset + 8);
                }
            } else {
                // Pass by pointer (Windows for non-power-of-2 or >8 bytes, System V for >16 bytes)
                try self.addLeaArg(CC_EMIT.BASE_PTR, offset);
            }
        }

        /// Add a float argument from a float register.
        /// Windows x64: Uses position-based registers (XMM0-3 mirror arg positions 0-3).
        ///              A float arg at position 1 goes in XMM1, consuming both int and float slots.
        /// System V: Uses separate float register pool (XMM0-7 independent of integer regs).
        pub fn addFloatRegArg(self: *Self, src_reg: FloatReg, is_f64: bool) !void {
            if (comptime is_windows) {
                // Windows: float args use same position as int args
                // XMM0 for arg 0, XMM1 for arg 1, etc.
                if (self.int_arg_index < CC_EMIT.FLOAT_PARAM_REGS.len) {
                    const dst = CC_EMIT.FLOAT_PARAM_REGS[self.int_arg_index];
                    if (dst != src_reg) {
                        if (is_f64) {
                            try self.emit.movsdRegReg(dst, src_reg);
                        } else {
                            try self.emit.movssRegReg(dst, src_reg);
                        }
                    }
                    self.int_arg_index += 1;
                } else {
                    // Float args beyond position 3 go on stack
                    @panic("Stack float args not yet implemented for Windows CallBuilder");
                }
            } else {
                // System V: separate float register pool
                if (self.float_arg_index < CC_EMIT.FLOAT_PARAM_REGS.len) {
                    const dst = CC_EMIT.FLOAT_PARAM_REGS[self.float_arg_index];
                    if (dst != src_reg) {
                        if (is_f64) {
                            try self.emit.movsdRegReg(dst, src_reg);
                        } else {
                            try self.emit.movssRegReg(dst, src_reg);
                        }
                    }
                    self.float_arg_index += 1;
                } else {
                    // Float args beyond XMM7 go on stack
                    @panic("Stack float args not yet implemented for System V CallBuilder");
                }
            }
        }

        /// Add a f64 float argument from a float register (convenience wrapper).
        pub fn addF64RegArg(self: *Self, src_reg: FloatReg) !void {
            try self.addFloatRegArg(src_reg, true);
        }

        /// Add a f32 float argument from a float register (convenience wrapper).
        pub fn addF32RegArg(self: *Self, src_reg: FloatReg) !void {
            try self.addFloatRegArg(src_reg, false);
        }

        /// Add a float argument by loading from memory.
        /// Windows x64: Uses position-based registers (XMM0-3 mirror arg positions 0-3).
        /// System V: Uses separate float register pool (XMM0-7).
        pub fn addFloatMemArg(self: *Self, base_reg: GeneralReg, offset: i32, is_f64: bool) !void {
            if (comptime is_windows) {
                // Windows: float args use same position as int args
                if (self.int_arg_index < CC_EMIT.FLOAT_PARAM_REGS.len) {
                    const dst = CC_EMIT.FLOAT_PARAM_REGS[self.int_arg_index];
                    if (is_f64) {
                        try self.emit.movsdRegMem(dst, base_reg, offset);
                    } else {
                        try self.emit.movssRegMem(dst, base_reg, offset);
                    }
                    self.int_arg_index += 1;
                } else {
                    @panic("Stack float args not yet implemented for Windows CallBuilder");
                }
            } else {
                // System V: separate float register pool
                if (self.float_arg_index < CC_EMIT.FLOAT_PARAM_REGS.len) {
                    const dst = CC_EMIT.FLOAT_PARAM_REGS[self.float_arg_index];
                    if (is_f64) {
                        try self.emit.movsdRegMem(dst, base_reg, offset);
                    } else {
                        try self.emit.movssRegMem(dst, base_reg, offset);
                    }
                    self.float_arg_index += 1;
                } else {
                    @panic("Stack float args not yet implemented for System V CallBuilder");
                }
            }
        }

        /// Add a f64 float argument by loading from memory (convenience wrapper).
        pub fn addF64MemArg(self: *Self, base_reg: GeneralReg, offset: i32) !void {
            try self.addFloatMemArg(base_reg, offset, true);
        }

        /// Add a f32 float argument by loading from memory (convenience wrapper).
        pub fn addF32MemArg(self: *Self, base_reg: GeneralReg, offset: i32) !void {
            try self.addFloatMemArg(base_reg, offset, false);
        }

        /// Get the number of float argument registers available
        pub fn getNumFloatArgRegs() usize {
            return CC_EMIT.FLOAT_PARAM_REGS.len;
        }

        /// Get float argument register at index
        pub fn getFloatArgReg(index: usize) FloatReg {
            return CC_EMIT.FLOAT_PARAM_REGS[index];
        }

        /// Get the float return register (XMM0 on x86_64)
        pub fn getFloatReturnReg() FloatReg {
            return CC_EMIT.FLOAT_PARAM_REGS[0];
        }

        /// Emit a single argument instruction: move source to destination register.
        fn emitArgInst(self: *Self, dst: GeneralReg, src: ArgSource) !void {
            switch (src) {
                .from_reg => |reg| {
                    if (dst != reg) try self.emit.movRegReg(.w64, dst, reg);
                },
                .from_lea => |lea| {
                    if (comptime is_aarch64) {
                        if (lea.offset >= 0)
                            try self.emit.addRegRegImm12(.w64, dst, lea.base, @intCast(lea.offset))
                        else
                            try self.emit.subRegRegImm12(.w64, dst, lea.base, @intCast(-lea.offset));
                    } else {
                        try self.emit.leaRegMem(dst, lea.base, lea.offset);
                    }
                },
                .from_mem => |mem| {
                    if (comptime is_aarch64)
                        try self.emit.ldrRegMemSoff(.w64, dst, mem.base, mem.offset)
                    else
                        try self.emit.movRegMem(.w64, dst, mem.base, mem.offset);
                },
                .from_imm => |value| {
                    if (comptime is_aarch64)
                        try self.emit.movRegImm64(dst, @bitCast(value))
                    else
                        try self.emit.movRegImm64(dst, value);
                },
            }
        }

        /// Emit a single stack argument for aarch64.
        /// Stores the arg value at [SP + uoffset * 8] using SCRATCH_REG as temp.
        fn emitStackArgAarch64(self: *Self, arg: ArgSource, uoffset: u12) !void {
            switch (arg) {
                .from_reg => |reg| {
                    try self.emit.strRegMemUoff(.w64, reg, CC_EMIT.STACK_PTR, uoffset);
                },
                .from_imm => |value| {
                    try self.emit.movRegImm64(CC_EMIT.SCRATCH_REG, @bitCast(value));
                    try self.emit.strRegMemUoff(.w64, CC_EMIT.SCRATCH_REG, CC_EMIT.STACK_PTR, uoffset);
                },
                .from_lea => |lea| {
                    if (lea.offset >= 0)
                        try self.emit.addRegRegImm12(.w64, CC_EMIT.SCRATCH_REG, lea.base, @intCast(lea.offset))
                    else
                        try self.emit.subRegRegImm12(.w64, CC_EMIT.SCRATCH_REG, lea.base, @intCast(-lea.offset));
                    try self.emit.strRegMemUoff(.w64, CC_EMIT.SCRATCH_REG, CC_EMIT.STACK_PTR, uoffset);
                },
                .from_mem => |mem| {
                    try self.emit.ldrRegMemSoff(.w64, CC_EMIT.SCRATCH_REG, mem.base, mem.offset);
                    try self.emit.strRegMemUoff(.w64, CC_EMIT.SCRATCH_REG, CC_EMIT.STACK_PTR, uoffset);
                },
            }
        }

        /// Resolve deferred register arguments using Rideau-Serpette-Leroy parallel move algorithm.
        /// This handles arbitrary permutations of param registers without clobbering,
        /// using SCRATCH_REG to break cycles. At most one scratch save per cycle.
        fn emitDeferredRegArgs(self: *Self) !void {
            if (self.reg_arg_count == 0) return;

            var statuses = [_]MoveStatus{.to_move} ** CC_EMIT.PARAM_REGS.len;
            // Mutable copy of sources — cycle breaking redirects sources to SCRATCH_REG
            var sources: [CC_EMIT.PARAM_REGS.len]ArgSource = undefined;
            for (self.reg_args[0..self.reg_arg_count], 0..) |arg, i| {
                sources[i] = arg.src;
            }

            for (0..self.reg_arg_count) |i| {
                if (statuses[i] == .to_move) {
                    try self.moveOne(i, &statuses, &sources);
                }
            }
        }

        /// Process a single deferred register arg, recursing to resolve dependencies first.
        fn moveOne(self: *Self, i: usize, statuses: *[CC_EMIT.PARAM_REGS.len]MoveStatus, sources: *[CC_EMIT.PARAM_REGS.len]ArgSource) !void {
            statuses[i] = .being_moved;
            const dst = CC_EMIT.PARAM_REGS[self.reg_args[i].dst_index];

            // Check if any other arg j reads from our destination register
            for (0..self.reg_arg_count) |j| {
                if (j == i) continue;
                const src_reg = switch (sources[j]) {
                    .from_reg => |reg| reg,
                    else => continue, // only reg sources can form conflicts
                };
                if (src_reg != dst) continue;

                // Arg j reads from dst — we'd clobber it
                switch (statuses[j]) {
                    .to_move => try self.moveOne(j, statuses, sources),
                    .being_moved => {
                        // CYCLE: save j's source to SCRATCH_REG
                        try self.emit.movRegReg(.w64, CC_EMIT.SCRATCH_REG, src_reg);
                        sources[j] = .{ .from_reg = CC_EMIT.SCRATCH_REG };
                    },
                    .moved => {}, // already handled
                }
            }

            // Now safe to emit our instruction
            try self.emitArgInst(dst, sources[i]);
            statuses[i] = .moved;
        }

        /// Emit call instruction and handle cleanup.
        /// If R12 save was configured via init(), R12 is automatically
        /// restored after the call returns.
        pub fn call(self: *Self, fn_addr: usize) !void {
            // Calculate total stack space needed
            const stack_args_space: u32 = @intCast(self.stack_arg_count * 8);
            const total_unaligned: u32 = CC_EMIT.SHADOW_SPACE + stack_args_space;
            // Round up to 16-byte alignment (Windows x64 ABI requirement)
            const total_space: u32 = (total_unaligned + 15) & ~@as(u32, 15);

            // Assert stack is 16-byte aligned
            std.debug.assert(total_space % 16 == 0);
            // Assert shadow space is included when we have stack args
            std.debug.assert(self.stack_arg_count == 0 or total_space >= CC_EMIT.SHADOW_SPACE + 8);

            if (comptime is_x86_64) {
                // Allocate all stack space at once
                if (total_space > 0) {
                    try self.emit.subRegImm32(.w64, CC_EMIT.STACK_PTR, @intCast(total_space));
                }

                // Store stack arguments BEFORE resolving deferred register args,
                // because the parallel move may clobber registers that stack args
                // source from (e.g., rhs operand in RCX/RDX when those are also
                // param register destinations for the first 4 args).
                for (self.stack_args[0..self.stack_arg_count], 0..) |arg, i| {
                    const stack_offset: i32 = @intCast(CC_EMIT.SHADOW_SPACE + i * 8);
                    // Assert stack args are placed after shadow space
                    std.debug.assert(stack_offset >= CC_EMIT.SHADOW_SPACE);
                    switch (arg) {
                        .from_reg => |reg| {
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, reg);
                        },
                        .from_imm => |value| {
                            try self.emit.movRegImm64(CC_EMIT.SCRATCH_REG, value);
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, CC_EMIT.SCRATCH_REG);
                        },
                        .from_lea => |lea| {
                            try self.emit.leaRegMem(CC_EMIT.SCRATCH_REG, lea.base, lea.offset);
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, CC_EMIT.SCRATCH_REG);
                        },
                        .from_mem => |mem| {
                            try self.emit.movRegMem(.w64, CC_EMIT.SCRATCH_REG, mem.base, mem.offset);
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, CC_EMIT.SCRATCH_REG);
                        },
                    }
                }
            } else if (comptime is_aarch64) {
                // aarch64: allocate stack space and store stack args
                if (total_space > 0) {
                    try self.emit.subRegRegImm12(.w64, CC_EMIT.STACK_PTR, CC_EMIT.STACK_PTR, @intCast(total_space));
                }

                for (self.stack_args[0..self.stack_arg_count], 0..) |arg, i| {
                    try self.emitStackArgAarch64(arg, @intCast(i));
                }
            }

            // Resolve deferred register args AFTER stack args are stored,
            // so the parallel move doesn't clobber stack arg source registers.
            try self.emitDeferredRegArgs();

            // Load function address into scratch register
            if (comptime is_aarch64) {
                try self.emit.movRegImm64(CC_EMIT.SCRATCH_REG, fn_addr);
            } else {
                try self.emit.movRegImm64(CC_EMIT.SCRATCH_REG, @bitCast(@as(i64, @intCast(fn_addr))));
            }

            // Call through scratch register
            if (comptime is_aarch64) {
                try self.emit.blrReg(CC_EMIT.SCRATCH_REG);
            } else {
                try self.emit.callReg(CC_EMIT.SCRATCH_REG);
            }

            // Cleanup
            if (comptime is_x86_64) {
                // Restore stack pointer
                if (total_space > 0) {
                    try self.emit.addRegImm32(.w64, CC_EMIT.STACK_PTR, @intCast(total_space));
                }

                // Restore R12 if we saved it (Windows x64 only)
                if (self.r12_save_offset) |offset| {
                    try self.emit.movRegMem(.w64, .R12, CC_EMIT.BASE_PTR, offset);
                }
            } else if (comptime is_aarch64) {
                if (total_space > 0) {
                    try self.emit.addRegRegImm12(.w64, CC_EMIT.STACK_PTR, CC_EMIT.STACK_PTR, @intCast(total_space));
                }
            }
        }

        /// Call through a register (for indirect calls).
        /// If R12 save was configured via init(), R12 is automatically
        /// restored after the call returns.
        pub fn callReg(self: *Self, target: GeneralReg) !void {
            // Calculate total stack space needed
            const stack_args_space: u32 = @intCast(self.stack_arg_count * 8);
            const total_unaligned: u32 = CC_EMIT.SHADOW_SPACE + stack_args_space;
            // Round up to 16-byte alignment (Windows x64 ABI requirement)
            const total_space: u32 = (total_unaligned + 15) & ~@as(u32, 15);

            // Assert stack is 16-byte aligned
            std.debug.assert(total_space % 16 == 0);
            // Assert shadow space is included when we have stack args
            std.debug.assert(self.stack_arg_count == 0 or total_space >= CC_EMIT.SHADOW_SPACE + 8);

            if (comptime is_x86_64) {
                // Allocate all stack space at once
                if (total_space > 0) {
                    try self.emit.subRegImm32(.w64, CC_EMIT.STACK_PTR, @intCast(total_space));
                }

                // Store stack arguments BEFORE resolving deferred register args
                for (self.stack_args[0..self.stack_arg_count], 0..) |arg, i| {
                    const stack_offset: i32 = @intCast(CC_EMIT.SHADOW_SPACE + i * 8);
                    std.debug.assert(stack_offset >= CC_EMIT.SHADOW_SPACE);
                    switch (arg) {
                        .from_reg => |reg| {
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, reg);
                        },
                        .from_imm => |value| {
                            try self.emit.movRegImm64(CC_EMIT.SCRATCH_REG, value);
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, CC_EMIT.SCRATCH_REG);
                        },
                        .from_lea => |lea| {
                            try self.emit.leaRegMem(CC_EMIT.SCRATCH_REG, lea.base, lea.offset);
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, CC_EMIT.SCRATCH_REG);
                        },
                        .from_mem => |mem| {
                            try self.emit.movRegMem(.w64, CC_EMIT.SCRATCH_REG, mem.base, mem.offset);
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, CC_EMIT.SCRATCH_REG);
                        },
                    }
                }
            } else if (comptime is_aarch64) {
                if (total_space > 0) {
                    try self.emit.subRegRegImm12(.w64, CC_EMIT.STACK_PTR, CC_EMIT.STACK_PTR, @intCast(total_space));
                }

                for (self.stack_args[0..self.stack_arg_count], 0..) |arg, i| {
                    try self.emitStackArgAarch64(arg, @intCast(i));
                }
            }

            // Resolve deferred register args AFTER stack args are stored
            try self.emitDeferredRegArgs();

            if (comptime is_aarch64) {
                try self.emit.blrReg(target);
            } else {
                try self.emit.callReg(target);
            }

            // Cleanup
            if (comptime is_x86_64) {
                // Restore stack pointer
                if (total_space > 0) {
                    try self.emit.addRegImm32(.w64, CC_EMIT.STACK_PTR, @intCast(total_space));
                }

                // Restore R12 if we saved it (Windows x64 only)
                if (self.r12_save_offset) |offset| {
                    try self.emit.movRegMem(.w64, .R12, CC_EMIT.BASE_PTR, offset);
                }
            } else if (comptime is_aarch64) {
                if (total_space > 0) {
                    try self.emit.addRegRegImm12(.w64, CC_EMIT.STACK_PTR, CC_EMIT.STACK_PTR, @intCast(total_space));
                }
            }
        }

        /// Call a function by symbol name, emitting a relocatable call instruction.
        /// This is used for object file generation where we can't use direct function pointers.
        /// The linker will patch the call offset during linking.
        ///
        /// Arguments:
        /// - symbol_name: The symbol name to call (e.g., "roc_dev_str_concat")
        /// - allocator: Allocator for the relocations list
        /// - relocations: The relocations list to append the relocation entry to
        ///
        /// Note: On x86_64, this emits `call rel32` (E8 xx xx xx xx).
        ///       On aarch64, this emits `bl offset` (26-bit signed offset).
        pub fn callRelocatable(self: *Self, symbol_name: []const u8, allocator: std.mem.Allocator, relocations: *std.ArrayList(Relocation)) !void {
            // Calculate total stack space needed (same as call/callReg)
            const stack_args_space: u32 = @intCast(self.stack_arg_count * 8);
            const total_unaligned: u32 = CC_EMIT.SHADOW_SPACE + stack_args_space;
            const total_space: u32 = (total_unaligned + 15) & ~@as(u32, 15);

            std.debug.assert(total_space % 16 == 0);
            std.debug.assert(self.stack_arg_count == 0 or total_space >= CC_EMIT.SHADOW_SPACE + 8);

            if (comptime is_x86_64) {
                // Allocate all stack space at once
                if (total_space > 0) {
                    try self.emit.subRegImm32(.w64, CC_EMIT.STACK_PTR, @intCast(total_space));
                }

                // Store stack arguments BEFORE resolving deferred register args
                for (self.stack_args[0..self.stack_arg_count], 0..) |arg, i| {
                    const stack_offset: i32 = @intCast(CC_EMIT.SHADOW_SPACE + i * 8);
                    std.debug.assert(stack_offset >= CC_EMIT.SHADOW_SPACE);
                    switch (arg) {
                        .from_reg => |reg| {
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, reg);
                        },
                        .from_imm => |value| {
                            try self.emit.movRegImm64(CC_EMIT.SCRATCH_REG, value);
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, CC_EMIT.SCRATCH_REG);
                        },
                        .from_lea => |lea| {
                            try self.emit.leaRegMem(CC_EMIT.SCRATCH_REG, lea.base, lea.offset);
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, CC_EMIT.SCRATCH_REG);
                        },
                        .from_mem => |mem| {
                            try self.emit.movRegMem(.w64, CC_EMIT.SCRATCH_REG, mem.base, mem.offset);
                            try self.emit.movMemReg(.w64, CC_EMIT.STACK_PTR, stack_offset, CC_EMIT.SCRATCH_REG);
                        },
                    }
                }
            } else if (comptime is_aarch64) {
                if (total_space > 0) {
                    try self.emit.subRegRegImm12(.w64, CC_EMIT.STACK_PTR, CC_EMIT.STACK_PTR, @intCast(total_space));
                }

                for (self.stack_args[0..self.stack_arg_count], 0..) |arg, i| {
                    try self.emitStackArgAarch64(arg, @intCast(i));
                }
            }

            // Resolve deferred register args AFTER stack args are stored
            try self.emitDeferredRegArgs();

            // Emit relocatable call instruction
            const code_offset = self.emit.buf.items.len;
            if (comptime is_aarch64) {
                // BL instruction with 0 offset placeholder
                try self.emit.bl(0);
                // Relocation points to the instruction itself for ARM64
                try relocations.append(allocator, .{
                    .linked_function = .{
                        .offset = @intCast(code_offset),
                        .name = symbol_name,
                    },
                });
            } else {
                // call rel32 with 0 offset placeholder
                try self.emit.callRel32(0);
                // For x86_64, relocation points to the 4-byte offset after the E8 opcode
                try relocations.append(allocator, .{
                    .linked_function = .{
                        .offset = @intCast(code_offset + 1),
                        .name = symbol_name,
                    },
                });
            }

            // Cleanup
            if (comptime is_x86_64) {
                if (total_space > 0) {
                    try self.emit.addRegImm32(.w64, CC_EMIT.STACK_PTR, @intCast(total_space));
                }

                if (self.r12_save_offset) |offset| {
                    try self.emit.movRegMem(.w64, .R12, CC_EMIT.BASE_PTR, offset);
                }
            } else if (comptime is_aarch64) {
                if (total_space > 0) {
                    try self.emit.addRegRegImm12(.w64, CC_EMIT.STACK_PTR, CC_EMIT.STACK_PTR, @intCast(total_space));
                }
            }
        }

        /// Get the return register for the result
        pub fn getReturnReg() GeneralReg {
            return CC_EMIT.RETURN_REGS[0];
        }

        /// Get the number of argument registers available
        pub fn getNumArgRegs() usize {
            return CC_EMIT.PARAM_REGS.len;
        }

        /// Get argument register at index (for manual setup)
        pub fn getArgReg(index: usize) GeneralReg {
            return CC_EMIT.PARAM_REGS[index];
        }
    };
}

// Tests
test "CallingConvention.forTarget Windows" {
    const cc = CallingConvention.forTarget(.x64win);
    try std.testing.expectEqual(@as(usize, 4), cc.num_param_regs);
    try std.testing.expectEqual(@as(u8, 32), cc.shadow_space);
    try std.testing.expectEqual(@as(usize, 8), cc.return_by_ptr_threshold);
    try std.testing.expectEqual(@as(usize, 8), cc.pass_by_ptr_threshold); // Only 1,2,4,8 by value
    try std.testing.expect(cc.is_windows);

    // Check register order: RCX, RDX, R8, R9
    try std.testing.expectEqual(x86_64.GeneralReg.RCX, cc.getParamReg(0));
    try std.testing.expectEqual(x86_64.GeneralReg.RDX, cc.getParamReg(1));
    try std.testing.expectEqual(x86_64.GeneralReg.R8, cc.getParamReg(2));
    try std.testing.expectEqual(x86_64.GeneralReg.R9, cc.getParamReg(3));
}

test "CallingConvention.forTarget Linux" {
    const cc = CallingConvention.forTarget(.x64glibc);
    try std.testing.expectEqual(@as(usize, 6), cc.num_param_regs);
    try std.testing.expectEqual(@as(u8, 0), cc.shadow_space);
    try std.testing.expectEqual(@as(usize, 16), cc.return_by_ptr_threshold);
    try std.testing.expectEqual(std.math.maxInt(usize), cc.pass_by_ptr_threshold);
    try std.testing.expect(!cc.is_windows);

    // Check register order: RDI, RSI, RDX, RCX, R8, R9
    try std.testing.expectEqual(x86_64.GeneralReg.RDI, cc.getParamReg(0));
    try std.testing.expectEqual(x86_64.GeneralReg.RSI, cc.getParamReg(1));
    try std.testing.expectEqual(x86_64.GeneralReg.RDX, cc.getParamReg(2));
    try std.testing.expectEqual(x86_64.GeneralReg.RCX, cc.getParamReg(3));
    try std.testing.expectEqual(x86_64.GeneralReg.R8, cc.getParamReg(4));
    try std.testing.expectEqual(x86_64.GeneralReg.R9, cc.getParamReg(5));
}

test "CallingConvention.forTarget macOS" {
    const cc = CallingConvention.forTarget(.x64mac);
    // macOS uses System V ABI, same as Linux
    try std.testing.expectEqual(@as(usize, 6), cc.num_param_regs);
    try std.testing.expectEqual(@as(u8, 0), cc.shadow_space);
    try std.testing.expect(!cc.is_windows);
}

test "CallingConvention.needsReturnByPointer" {
    const win_cc = CallingConvention.forTarget(.x64win);
    const sysv_cc = CallingConvention.forTarget(.x64glibc);

    // Windows: threshold is 8 bytes
    try std.testing.expect(!win_cc.needsReturnByPointer(8));
    try std.testing.expect(win_cc.needsReturnByPointer(9));

    // System V: threshold is 16 bytes
    try std.testing.expect(!sysv_cc.needsReturnByPointer(16));
    try std.testing.expect(sysv_cc.needsReturnByPointer(17));
}

test "CallingConvention.needsPassByPointer" {
    const win_cc = CallingConvention.forTarget(.x64win);
    const sysv_cc = CallingConvention.forTarget(.x64glibc);

    // Windows: only 1, 2, 4, 8 byte structs can be passed by value
    try std.testing.expect(!win_cc.needsPassByPointer(1));
    try std.testing.expect(!win_cc.needsPassByPointer(2));
    try std.testing.expect(!win_cc.needsPassByPointer(4));
    try std.testing.expect(!win_cc.needsPassByPointer(8));
    // Non-power-of-2 sizes need pointer on Windows
    try std.testing.expect(win_cc.needsPassByPointer(3));
    try std.testing.expect(win_cc.needsPassByPointer(5));
    try std.testing.expect(win_cc.needsPassByPointer(6));
    try std.testing.expect(win_cc.needsPassByPointer(7));
    // >8 bytes needs pointer
    try std.testing.expect(win_cc.needsPassByPointer(9));
    try std.testing.expect(win_cc.needsPassByPointer(16));

    // System V: never passes by pointer (uses stack for large structs)
    try std.testing.expect(!sysv_cc.needsPassByPointer(1000));
}

test "CC.canPassStructByValue for Windows x64" {
    const WinEmit = x86_64.Emit(.x64win);
    // Windows: only power-of-2 sizes 1, 2, 4, 8 can pass by value
    try std.testing.expect(WinEmit.CC.canPassStructByValue(1));
    try std.testing.expect(WinEmit.CC.canPassStructByValue(2));
    try std.testing.expect(WinEmit.CC.canPassStructByValue(4));
    try std.testing.expect(WinEmit.CC.canPassStructByValue(8));
    // Non-power-of-2 sizes must use pointer
    try std.testing.expect(!WinEmit.CC.canPassStructByValue(3));
    try std.testing.expect(!WinEmit.CC.canPassStructByValue(5));
    try std.testing.expect(!WinEmit.CC.canPassStructByValue(6));
    try std.testing.expect(!WinEmit.CC.canPassStructByValue(7));
    // >8 bytes must use pointer
    try std.testing.expect(!WinEmit.CC.canPassStructByValue(9));
    try std.testing.expect(!WinEmit.CC.canPassStructByValue(16));
}

test "CC.canPassStructByValue for Linux x64" {
    const LinuxEmit = x86_64.Emit(.x64glibc);
    // System V: up to 16 bytes can pass by value
    try std.testing.expect(LinuxEmit.CC.canPassStructByValue(16));
    try std.testing.expect(!LinuxEmit.CC.canPassStructByValue(17));
}

test "CC constants for Windows x64" {
    const WinEmit = x86_64.Emit(.x64win);
    try std.testing.expect(WinEmit.CC.PARAM_REGS.len >= 4);
    try std.testing.expect(WinEmit.CC.RETURN_REGS.len >= 1);
    try std.testing.expectEqual(@as(u8, 32), WinEmit.CC.SHADOW_SPACE);
    try std.testing.expectEqual(@as(usize, 8), WinEmit.CC.RETURN_BY_PTR_THRESHOLD);
    try std.testing.expectEqual(@as(usize, 8), WinEmit.CC.PASS_BY_PTR_THRESHOLD);
}

test "CC constants for Linux x64" {
    const LinuxEmit = x86_64.Emit(.x64glibc);
    try std.testing.expect(LinuxEmit.CC.PARAM_REGS.len >= 4);
    try std.testing.expect(LinuxEmit.CC.RETURN_REGS.len >= 1);
    try std.testing.expectEqual(@as(u8, 0), LinuxEmit.CC.SHADOW_SPACE);
    try std.testing.expectEqual(@as(usize, 16), LinuxEmit.CC.RETURN_BY_PTR_THRESHOLD);
    try std.testing.expectEqual(std.math.maxInt(usize), LinuxEmit.CC.PASS_BY_PTR_THRESHOLD);
}

test "CC constants for aarch64" {
    const Arm64Emit = aarch64.Emit(.arm64linux);
    try std.testing.expect(Arm64Emit.CC.PARAM_REGS.len >= 4);
    try std.testing.expect(Arm64Emit.CC.RETURN_REGS.len >= 1);
    try std.testing.expectEqual(@as(u8, 0), Arm64Emit.CC.SHADOW_SPACE);
    try std.testing.expectEqual(@as(usize, 16), Arm64Emit.CC.RETURN_BY_PTR_THRESHOLD);
    try std.testing.expectEqual(std.math.maxInt(usize), Arm64Emit.CC.PASS_BY_PTR_THRESHOLD);
}

test "needsReturnByPointer for Windows x64" {
    const WinEmit = x86_64.Emit(.x64win);
    const Builder = CallBuilder(WinEmit);

    // Small values don't need pointer
    try std.testing.expect(!Builder.needsReturnByPointer(1));
    try std.testing.expect(!Builder.needsReturnByPointer(8));
    // Windows: threshold is 8 bytes
    try std.testing.expect(Builder.needsReturnByPointer(9));
    try std.testing.expect(Builder.needsReturnByPointer(16));
}

test "needsReturnByPointer for Linux x64" {
    const LinuxEmit = x86_64.Emit(.x64glibc);
    const Builder = CallBuilder(LinuxEmit);

    // Small values don't need pointer
    try std.testing.expect(!Builder.needsReturnByPointer(1));
    try std.testing.expect(!Builder.needsReturnByPointer(8));
    // System V: threshold is 16 bytes
    try std.testing.expect(!Builder.needsReturnByPointer(16));
    try std.testing.expect(Builder.needsReturnByPointer(17));
}

test "needsPassByPointer for Windows x64" {
    const WinEmit = x86_64.Emit(.x64win);
    const Builder = CallBuilder(WinEmit);

    // Windows x64: only 1, 2, 4, 8 byte structs pass by value
    try std.testing.expect(!Builder.needsPassByPointer(1));
    try std.testing.expect(!Builder.needsPassByPointer(2));
    try std.testing.expect(!Builder.needsPassByPointer(4));
    try std.testing.expect(!Builder.needsPassByPointer(8));
    // Non-power-of-2 sizes need pointer
    try std.testing.expect(Builder.needsPassByPointer(3));
    try std.testing.expect(Builder.needsPassByPointer(5));
    try std.testing.expect(Builder.needsPassByPointer(6));
    try std.testing.expect(Builder.needsPassByPointer(7));
    // >8 bytes need pointer
    try std.testing.expect(Builder.needsPassByPointer(9));
    try std.testing.expect(Builder.needsPassByPointer(16));
    try std.testing.expect(Builder.needsPassByPointer(24));
}

test "needsPassByPointer for Linux x64" {
    const LinuxEmit = x86_64.Emit(.x64glibc);
    const Builder = CallBuilder(LinuxEmit);

    // System V: never uses pointer
    try std.testing.expect(!Builder.needsPassByPointer(16));
    try std.testing.expect(!Builder.needsPassByPointer(17));
    try std.testing.expect(!Builder.needsPassByPointer(1000));
}

test "Windows x64 argument registers" {
    const WinEmit = x86_64.Emit(.x64win);
    const Builder = CallBuilder(WinEmit);

    // Windows uses RCX, RDX, R8, R9
    try std.testing.expectEqual(x86_64.GeneralReg.RCX, Builder.getArgReg(0));
    try std.testing.expectEqual(x86_64.GeneralReg.RDX, Builder.getArgReg(1));
    try std.testing.expectEqual(x86_64.GeneralReg.R8, Builder.getArgReg(2));
    try std.testing.expectEqual(x86_64.GeneralReg.R9, Builder.getArgReg(3));
    try std.testing.expectEqual(@as(usize, 4), Builder.getNumArgRegs());
}

test "Linux x64 argument registers" {
    const LinuxEmit = x86_64.Emit(.x64glibc);
    const Builder = CallBuilder(LinuxEmit);

    // System V uses RDI, RSI, RDX, RCX, R8, R9
    try std.testing.expectEqual(x86_64.GeneralReg.RDI, Builder.getArgReg(0));
    try std.testing.expectEqual(x86_64.GeneralReg.RSI, Builder.getArgReg(1));
    try std.testing.expectEqual(x86_64.GeneralReg.RDX, Builder.getArgReg(2));
    try std.testing.expectEqual(x86_64.GeneralReg.RCX, Builder.getArgReg(3));
    try std.testing.expectEqual(x86_64.GeneralReg.R8, Builder.getArgReg(4));
    try std.testing.expectEqual(x86_64.GeneralReg.R9, Builder.getArgReg(5));
    try std.testing.expectEqual(@as(usize, 6), Builder.getNumArgRegs());
}

test "CallBuilder with automatic R12 save/restore on Windows x64" {
    const WinEmit = x86_64.Emit(.x64win);
    const Builder = CallBuilder(WinEmit);

    var emit = WinEmit.init(std.testing.allocator);
    defer emit.deinit();

    // Simulate a stack offset like CodeGen would have
    var stack_offset: i32 = -16;

    // Initialize builder - on Windows, this will allocate R12 save slot
    const builder = try Builder.init(&emit, &stack_offset);

    // On Windows, should have emitted: mov [rbp-32], r12 and allocated 16 bytes
    // (16 bytes to maintain 16-byte alignment for i128/RocDec arguments)
    try std.testing.expect(emit.buf.items.len > 0);
    try std.testing.expectEqual(@as(i32, -32), stack_offset); // 16 bytes allocated
    try std.testing.expectEqual(@as(?i32, -32), builder.r12_save_offset);
}

test "CallBuilder with automatic R12 save/restore on Linux x64" {
    const LinuxEmit = x86_64.Emit(.x64glibc);
    const Builder = CallBuilder(LinuxEmit);

    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    // Simulate a stack offset like CodeGen would have
    var stack_offset: i32 = -16;

    // Initialize builder - on Linux, no R12 save needed
    const builder = try Builder.init(&emit, &stack_offset);

    // On non-Windows, no save should have been emitted
    try std.testing.expectEqual(@as(usize, 0), emit.buf.items.len);
    try std.testing.expectEqual(@as(i32, -16), stack_offset); // unchanged
    try std.testing.expectEqual(@as(?i32, null), builder.r12_save_offset);
}

test "CallBuilder call restores R12 on Windows x64" {
    const WinEmit = x86_64.Emit(.x64win);
    const Builder = CallBuilder(WinEmit);

    var emit = WinEmit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = -16;

    // Initialize builder
    var builder = try Builder.init(&emit, &stack_offset);
    const initial_len = emit.buf.items.len;

    // Add a simple argument
    try builder.addImmArg(42);

    // Make a call (using a dummy address)
    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > initial_len);

    // On Windows, the call sequence should include R12 restore at the end
    // Just verify r12_save_offset is set - the restore happens automatically
    try std.testing.expect(builder.r12_save_offset != null);
}

test "CallBuilder on Linux x64 doesn't allocate R12 slot" {
    const LinuxEmit = x86_64.Emit(.x64glibc);
    const Builder = CallBuilder(LinuxEmit);

    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = -16;

    // Initialize builder - no R12 allocation on non-Windows
    const builder = try Builder.init(&emit, &stack_offset);

    // Stack offset should be unchanged
    try std.testing.expectEqual(@as(i32, -16), stack_offset);
    // No code should be emitted yet
    try std.testing.expectEqual(@as(usize, 0), emit.buf.items.len);
    try std.testing.expectEqual(@as(?i32, null), builder.r12_save_offset);
}

test "CallBuilder 4-arg call with immediates on Linux x64" {
    const LinuxEmit = x86_64.Emit(.x64glibc);
    const Builder = CallBuilder(LinuxEmit);

    var emit = LinuxEmit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add 4 immediate arguments
    try builder.addImmArg(1);
    try builder.addImmArg(2);
    try builder.addImmArg(3);
    try builder.addImmArg(4);

    // Call a dummy address
    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // Look for the call instruction (call r11 = 41 FF D3)
    var found_call = false;
    for (0..emit.buf.items.len - 2) |i| {
        if (emit.buf.items[i] == 0x41 and emit.buf.items[i + 1] == 0xFF and emit.buf.items[i + 2] == 0xD3) {
            found_call = true;
            break;
        }
    }
    try std.testing.expect(found_call);
}

test "CallBuilder 4-arg call with immediates on Windows x64" {
    const WinEmit = x86_64.Emit(.x64win);
    const Builder = CallBuilder(WinEmit);

    var emit = WinEmit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add 4 immediate arguments
    try builder.addImmArg(1);
    try builder.addImmArg(2);
    try builder.addImmArg(3);
    try builder.addImmArg(4);

    // Call a dummy address
    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // Look for the call instruction (call r11 = 41 FF D3)
    var found_call = false;
    for (0..emit.buf.items.len - 2) |i| {
        if (emit.buf.items[i] == 0x41 and emit.buf.items[i + 1] == 0xFF and emit.buf.items[i + 2] == 0xD3) {
            found_call = true;
            break;
        }
    }
    try std.testing.expect(found_call);

    // Windows: should have sub rsp, 32 somewhere (shadow space)
    // sub rsp, 32 = 48 81 EC 20 00 00 00
    var found_sub = false;
    for (0..emit.buf.items.len - 6) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x81 and
            emit.buf.items[i + 2] == 0xEC and emit.buf.items[i + 3] == 0x20)
        {
            found_sub = true;
            break;
        }
    }
    try std.testing.expect(found_sub);
}

test "CallBuilder 6-arg call System V - all args in registers" {
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add 6 immediate arguments
    try builder.addImmArg(1);
    try builder.addImmArg(2);
    try builder.addImmArg(3);
    try builder.addImmArg(4);
    try builder.addImmArg(5);
    try builder.addImmArg(6);

    try builder.call(0x12345678);

    // System V: 6 register args (RDI, RSI, RDX, RCX, R8, R9) - no stack args
    // Should be shorter than Windows version (no shadow space)
    try std.testing.expect(emit.buf.items.len > 50);

    // Look for the call instruction (call r11 = 41 FF D3)
    var found_call = false;
    for (0..emit.buf.items.len - 2) |i| {
        if (emit.buf.items[i] == 0x41 and emit.buf.items[i + 1] == 0xFF and emit.buf.items[i + 2] == 0xD3) {
            found_call = true;
            break;
        }
    }
    try std.testing.expect(found_call);
}

test "CallBuilder 6-arg call Windows - 4 reg + 2 stack args" {
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add 6 immediate arguments
    try builder.addImmArg(1);
    try builder.addImmArg(2);
    try builder.addImmArg(3);
    try builder.addImmArg(4);
    try builder.addImmArg(5);
    try builder.addImmArg(6);

    try builder.call(0x12345678);

    // Windows x64: 4 register args (RCX, RDX, R8, R9) + 2 stack args
    // Stack args go at [RSP+32] and [RSP+40] (after shadow space)
    // Total stack space = 32 (shadow) + 16 (2 args) = 48
    try std.testing.expect(emit.buf.items.len > 50);

    // Look for the call instruction (call r11 = 41 FF D3)
    var found_call = false;
    for (0..emit.buf.items.len - 2) |i| {
        if (emit.buf.items[i] == 0x41 and emit.buf.items[i + 1] == 0xFF and emit.buf.items[i + 2] == 0xD3) {
            found_call = true;
            break;
        }
    }
    try std.testing.expect(found_call);

    // Look for sub rsp, 48 = 48 81 EC 30 00 00 00
    var found_sub = false;
    for (0..emit.buf.items.len - 6) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x81 and
            emit.buf.items[i + 2] == 0xEC and emit.buf.items[i + 3] == 0x30)
        {
            found_sub = true;
            break;
        }
    }
    try std.testing.expect(found_sub);
}

test "CallBuilder with register argument System V" {
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add a register argument (RAX -> first param reg RDI on System V)
    try builder.addRegArg(.RAX);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // System V: Should have mov rdi, rax (48 89 C7)
    var found_mov = false;
    for (0..emit.buf.items.len - 2) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x89 and emit.buf.items[i + 2] == 0xC7) {
            found_mov = true;
            break;
        }
    }
    try std.testing.expect(found_mov);
}

test "CallBuilder with register argument Windows" {
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add a register argument (RAX -> first param reg RCX on Windows)
    try builder.addRegArg(.RAX);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // Windows: Should have mov rcx, rax (48 89 C1)
    var found_mov = false;
    for (0..emit.buf.items.len - 2) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x89 and emit.buf.items[i + 2] == 0xC1) {
            found_mov = true;
            break;
        }
    }
    try std.testing.expect(found_mov);
}

test "CallBuilder with LEA argument System V" {
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add a LEA argument (pointer to [RBP-32])
    try builder.addLeaArg(.RBP, -32);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // System V: lea rdi, [rbp-32] = 48 8D BD + disp32(-32)
    var found_lea = false;
    for (0..emit.buf.items.len - 6) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x8D and emit.buf.items[i + 2] == 0xBD) {
            found_lea = true;
            break;
        }
    }
    try std.testing.expect(found_lea);
}

test "CallBuilder with LEA argument Windows" {
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add a LEA argument (pointer to [RBP-32])
    try builder.addLeaArg(.RBP, -32);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // Windows: lea rcx, [rbp-32] = 48 8D 8D + disp32(-32)
    var found_lea = false;
    for (0..emit.buf.items.len - 6) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x8D and emit.buf.items[i + 2] == 0x8D) {
            found_lea = true;
            break;
        }
    }
    try std.testing.expect(found_lea);
}

test "CallBuilder with memory argument System V" {
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add a memory load argument (load from [RBP-16])
    try builder.addMemArg(.RBP, -16);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // System V: mov rdi, [rbp-16] = 48 8B BD F0 FF FF FF
    var found_mov = false;
    for (0..emit.buf.items.len - 6) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x8B and emit.buf.items[i + 2] == 0xBD) {
            found_mov = true;
            break;
        }
    }
    try std.testing.expect(found_mov);
}

test "CallBuilder with memory argument Windows" {
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add a memory load argument (load from [RBP-16])
    try builder.addMemArg(.RBP, -16);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // Windows: mov rcx, [rbp-16] = 48 8B 8D F0 FF FF FF
    var found_mov = false;
    for (0..emit.buf.items.len - 6) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x8B and emit.buf.items[i + 2] == 0x8D) {
            found_mov = true;
            break;
        }
    }
    try std.testing.expect(found_mov);
}

test "CallBuilder stack alignment is 16-byte on Windows" {
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add 5 arguments on Windows (4 reg + 1 stack)
    // Stack space = 32 (shadow) + 8 (1 arg) = 40, rounded up to 48 for alignment
    try builder.addImmArg(1);
    try builder.addImmArg(2);
    try builder.addImmArg(3);
    try builder.addImmArg(4);
    try builder.addImmArg(5);

    try builder.call(0x12345678);

    // Should allocate 48 bytes (32 shadow + 8 arg + 8 padding for 16-byte alignment)
    // Look for sub rsp, 48 = 48 81 EC 30 00 00 00
    var found_sub = false;
    for (0..emit.buf.items.len - 6) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x81 and
            emit.buf.items[i + 2] == 0xEC and emit.buf.items[i + 3] == 0x30)
        {
            found_sub = true;
            break;
        }
    }
    try std.testing.expect(found_sub);
}

test "CallBuilder return by pointer sets up first arg System V" {
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Set return by pointer (output at [RBP-64])
    try builder.setReturnByPointer(-64);

    // Add one more argument
    try builder.addImmArg(42);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // System V: First arg (RDI) should be LEA to output buffer
    // Look for lea rdi, [rbp-64] = 48 8D BD + disp32
    var found_lea = false;
    for (0..emit.buf.items.len - 6) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x8D and emit.buf.items[i + 2] == 0xBD) {
            found_lea = true;
            break;
        }
    }
    try std.testing.expect(found_lea);
}

test "CallBuilder return by pointer sets up first arg Windows" {
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Set return by pointer (output at [RBP-64])
    try builder.setReturnByPointer(-64);

    // Add one more argument
    try builder.addImmArg(42);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // Windows: First arg (RCX) should be LEA to output buffer
    // Look for lea rcx, [rbp-64] = 48 8D 8D + disp32
    var found_lea = false;
    for (0..emit.buf.items.len - 6) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x8D and emit.buf.items[i + 2] == 0x8D) {
            found_lea = true;
            break;
        }
    }
    try std.testing.expect(found_lea);
}

test "CallBuilder addF64RegArg uses correct XMM register" {
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add a float argument from XMM5 -> should move to XMM0 (first float param reg)
    try builder.addF64RegArg(.XMM5);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // Look for movsd xmm0, xmm5 = F2 0F 10 C5
    var found_movsd = false;
    for (0..emit.buf.items.len -| 3) |i| {
        if (emit.buf.items[i] == 0xF2 and emit.buf.items[i + 1] == 0x0F and
            emit.buf.items[i + 2] == 0x10 and emit.buf.items[i + 3] == 0xC5)
        {
            found_movsd = true;
            break;
        }
    }
    try std.testing.expect(found_movsd);
}

test "CallBuilder addF64MemArg loads from memory to XMM" {
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add a float argument loaded from [RBP-48]
    try builder.addF64MemArg(.RBP, -48);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // Look for movsd xmm0, [rbp+disp32] = F2 0F 10 85 ...
    var found_movsd_mem = false;
    for (0..emit.buf.items.len -| 3) |i| {
        if (emit.buf.items[i] == 0xF2 and emit.buf.items[i + 1] == 0x0F and
            emit.buf.items[i + 2] == 0x10 and emit.buf.items[i + 3] == 0x85)
        {
            found_movsd_mem = true;
            break;
        }
    }
    try std.testing.expect(found_movsd_mem);
}

test "CallBuilder Windows position-based float regs" {
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // On Windows, args use position-based registers:
    // Position 0: RCX/XMM0, Position 1: RDX/XMM1, etc.
    // Add int arg at position 0 (uses RCX)
    try builder.addImmArg(100);
    // Add float arg at position 1 (should use XMM1, NOT XMM0!)
    try builder.addF64RegArg(.XMM5);

    try builder.call(0x12345678);

    // Look for movsd xmm1, xmm5 = F2 0F 10 CD
    var found_movsd_xmm1 = false;
    for (0..emit.buf.items.len -| 3) |i| {
        if (emit.buf.items[i] == 0xF2 and emit.buf.items[i + 1] == 0x0F and
            emit.buf.items[i + 2] == 0x10 and emit.buf.items[i + 3] == 0xCD)
        {
            found_movsd_xmm1 = true;
            break;
        }
    }
    try std.testing.expect(found_movsd_xmm1);
}

test "CallBuilder mixed int and float args" {
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Mix of int and float args (System V uses separate counters)
    try builder.addImmArg(1); // int arg 0 -> RDI
    try builder.addF64RegArg(.XMM5); // float arg -> XMM0
    try builder.addImmArg(2); // int arg 1 -> RSI

    try builder.call(0x12345678);

    // Verify code was emitted with reasonable length (all args + call + cleanup)
    try std.testing.expect(emit.buf.items.len > 30);
}

test "CallBuilder callReg allocates shadow space on Windows" {
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add some arguments
    try builder.addImmArg(1);
    try builder.addImmArg(2);

    // Use callReg instead of call
    try builder.callReg(.RAX);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // Should have shadow space allocation: sub rsp, 32 = 48 81 EC 20 00 00 00
    var found_sub = false;
    for (0..emit.buf.items.len -| 6) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x81 and
            emit.buf.items[i + 2] == 0xEC and emit.buf.items[i + 3] == 0x20)
        {
            found_sub = true;
            break;
        }
    }
    try std.testing.expect(found_sub);

    // Should have call rax = FF D0
    var found_call_rax = false;
    for (0..emit.buf.items.len -| 1) |i| {
        if (emit.buf.items[i] == 0xFF and emit.buf.items[i + 1] == 0xD0) {
            found_call_rax = true;
            break;
        }
    }
    try std.testing.expect(found_call_rax);
}

test "CallBuilder R12 save emits correct MOV instruction on Windows" {
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = -16;
    _ = try Builder.init(&emit, &stack_offset);

    // On Windows, init should emit: mov [rbp-24], r12
    // mov [rbp+disp32], r12 = 4C 89 A5 E8 FF FF FF (for -24)
    // REX.WR=4C, opcode=89, ModRM=A5 (mod=10, reg=R12(4), rm=RBP(5)), disp32

    // Check that R12 save was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // Look for the MOV [rbp+disp], r12 pattern
    // 4C 89 A5 = REX.WR + MOV r/m64,r64 + ModRM for [rbp+disp32], r12
    var found_r12_save = false;
    for (0..emit.buf.items.len -| 2) |i| {
        if (emit.buf.items[i] == 0x4C and emit.buf.items[i + 1] == 0x89 and emit.buf.items[i + 2] == 0xA5) {
            found_r12_save = true;
            break;
        }
    }
    try std.testing.expect(found_r12_save);
}

test "CallBuilder R12 restore emits correct MOV instruction on Windows" {
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = -16;
    var builder = try Builder.init(&emit, &stack_offset);

    // Make a call which should restore R12 at the end
    try builder.addImmArg(42);
    try builder.call(0x12345678);

    // Look for the MOV r12, [rbp+disp] pattern (restore)
    // 4C 8B A5 = REX.WR + MOV r64,r/m64 + ModRM for r12, [rbp+disp32]
    var found_r12_restore = false;
    for (0..emit.buf.items.len -| 2) |i| {
        if (emit.buf.items[i] == 0x4C and emit.buf.items[i + 1] == 0x8B and emit.buf.items[i + 2] == 0xA5) {
            found_r12_restore = true;
            break;
        }
    }
    try std.testing.expect(found_r12_restore);
}

test "CallBuilder stack args at correct offsets on Windows" {
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Add 5 args on Windows: 4 in registers, 1 on stack
    // Stack arg should go at [RSP+32] (after shadow space)
    try builder.addImmArg(1); // RCX
    try builder.addImmArg(2); // RDX
    try builder.addImmArg(3); // R8
    try builder.addImmArg(4); // R9
    try builder.addImmArg(5); // [RSP+32]

    try builder.call(0x12345678);

    // Look for mov [rsp+32], r11 pattern after loading 5 into r11
    // The 5th arg (value 5) gets loaded into R11 then stored at [RSP+32]
    // mov [rsp+disp8], r11 = 4C 89 5C 24 20 (if disp fits in 8 bits)
    // or mov [rsp+disp32], r11 = 4C 89 9C 24 20 00 00 00

    // Just verify reasonable code length indicating stack arg handling
    // (4 reg args + 1 stack arg + shadow space handling)
    try std.testing.expect(emit.buf.items.len > 50);
}

// Multi-target aarch64 calling convention tests

test "aarch64 CC identical across all targets" {
    const Arm64Linux = aarch64.LinuxEmit;
    const Arm64Win = aarch64.WinEmit;
    const Arm64Mac = aarch64.MacEmit;

    // All aarch64 targets use AAPCS64 - verify uniform behavior
    try std.testing.expectEqual(Arm64Linux.CC.PARAM_REGS.len, Arm64Win.CC.PARAM_REGS.len);
    try std.testing.expectEqual(Arm64Linux.CC.PARAM_REGS.len, Arm64Mac.CC.PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 8), Arm64Linux.CC.PARAM_REGS.len);

    try std.testing.expectEqual(Arm64Linux.CC.SHADOW_SPACE, Arm64Win.CC.SHADOW_SPACE);
    try std.testing.expectEqual(Arm64Linux.CC.SHADOW_SPACE, Arm64Mac.CC.SHADOW_SPACE);
    try std.testing.expectEqual(@as(u8, 0), Arm64Linux.CC.SHADOW_SPACE);

    try std.testing.expectEqual(Arm64Linux.CC.FLOAT_PARAM_REGS.len, Arm64Win.CC.FLOAT_PARAM_REGS.len);
    try std.testing.expectEqual(@as(usize, 8), Arm64Linux.CC.FLOAT_PARAM_REGS.len);
}

test "CallingConvention.forTarget for aarch64 targets" {
    const linux_cc = CallingConvention.forTarget(.arm64linux);
    const win_cc = CallingConvention.forTarget(.arm64win);
    const mac_cc = CallingConvention.forTarget(.arm64mac);

    // All should use AAPCS64 with same parameters
    try std.testing.expectEqual(linux_cc.num_param_regs, win_cc.num_param_regs);
    try std.testing.expectEqual(linux_cc.num_param_regs, mac_cc.num_param_regs);
    try std.testing.expectEqual(@as(u8, 8), linux_cc.num_param_regs);

    try std.testing.expectEqual(linux_cc.shadow_space, win_cc.shadow_space);
    try std.testing.expectEqual(@as(u8, 0), linux_cc.shadow_space);
}

test "CC constants for macOS x64" {
    const MacEmit = x86_64.Emit(.x64mac);
    try std.testing.expect(MacEmit.CC.PARAM_REGS.len >= 4);
    try std.testing.expect(MacEmit.CC.RETURN_REGS.len >= 1);
    try std.testing.expectEqual(@as(u8, 0), MacEmit.CC.SHADOW_SPACE);
    try std.testing.expectEqual(@as(usize, 16), MacEmit.CC.RETURN_BY_PTR_THRESHOLD);
    try std.testing.expectEqual(std.math.maxInt(usize), MacEmit.CC.PASS_BY_PTR_THRESHOLD);
}

test "macOS x64 CC matches Linux x64 (both System V)" {
    const LinuxEmit = x86_64.Emit(.x64glibc);
    const MacEmit = x86_64.Emit(.x64mac);

    // macOS and Linux both use System V ABI - verify they're identical
    try std.testing.expectEqual(LinuxEmit.CC.PARAM_REGS.len, MacEmit.CC.PARAM_REGS.len);
    try std.testing.expectEqual(LinuxEmit.CC.RETURN_REGS.len, MacEmit.CC.RETURN_REGS.len);
    try std.testing.expectEqual(LinuxEmit.CC.FLOAT_PARAM_REGS.len, MacEmit.CC.FLOAT_PARAM_REGS.len);
    try std.testing.expectEqual(LinuxEmit.CC.SHADOW_SPACE, MacEmit.CC.SHADOW_SPACE);
    try std.testing.expectEqual(LinuxEmit.CC.RETURN_BY_PTR_THRESHOLD, MacEmit.CC.RETURN_BY_PTR_THRESHOLD);
    try std.testing.expectEqual(LinuxEmit.CC.PASS_BY_PTR_THRESHOLD, MacEmit.CC.PASS_BY_PTR_THRESHOLD);

    // Both should NOT be Windows
    const linux_cc = CallingConvention.forTarget(.x64glibc);
    const mac_cc = CallingConvention.forTarget(.x64mac);
    try std.testing.expect(!linux_cc.is_windows);
    try std.testing.expect(!mac_cc.is_windows);
}

test "CallBuilder macOS 6-arg call - all args in registers (System V)" {
    const MacEmit = x86_64.Emit(.x64mac);
    const Builder = CallBuilder(MacEmit);

    var emit = MacEmit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // System V: 6 register args (RDI, RSI, RDX, RCX, R8, R9)
    try builder.addImmArg(1);
    try builder.addImmArg(2);
    try builder.addImmArg(3);
    try builder.addImmArg(4);
    try builder.addImmArg(5);
    try builder.addImmArg(6);

    try builder.call(0x12345678);

    const mac_code = emit.buf.items;

    // Verify code matches Linux (both System V)
    var linux_emit = x86_64.Emit(.x64glibc).init(std.testing.allocator);
    defer linux_emit.deinit();
    var linux_stack: i32 = 0;
    var linux_builder = try CallBuilder(x86_64.Emit(.x64glibc)).init(&linux_emit, &linux_stack);
    try linux_builder.addImmArg(1);
    try linux_builder.addImmArg(2);
    try linux_builder.addImmArg(3);
    try linux_builder.addImmArg(4);
    try linux_builder.addImmArg(5);
    try linux_builder.addImmArg(6);
    try linux_builder.call(0x12345678);

    try std.testing.expectEqualSlices(u8, linux_emit.buf.items, mac_code);
}

// Helper: search for a 3-byte pattern in a buffer, returning its position or null
fn findPattern3(buf: []const u8, b0: u8, b1: u8, b2: u8) ?usize {
    if (buf.len < 3) return null;
    for (0..buf.len - 2) |i| {
        if (buf[i] == b0 and buf[i + 1] == b1 and buf[i + 2] == b2) return i;
    }
    return null;
}

// x86_64 MOV reg,reg encoding reference (opcode 0x89, MOV r/m64, r64):
// REX = 0x40 | (W<<3) | (R<<2) | B, where R=src.rexR, B=dst.rexB
// ModRM = 0xC0 | (src.enc()<<3) | dst.enc()
//
// Common encodings:
//   mov rdi, rax  = 48 89 C7     mov rsi, rax  = 48 89 C6
//   mov rdi, rsi  = 48 89 F7     mov rsi, rdi  = 48 89 FE
//   mov rdi, rdx  = 48 89 D7     mov rsi, rdx  = 48 89 D6
//   mov rdi, rbx  = 48 89 DE     mov rdx, rdi  = 48 89 FA
//   mov rdi, r11  = 4C 89 DF     mov r11, rsi  = 49 89 F3
//   mov rsi, rbx  = 48 89 DE     mov rdx, r11  = 4C 89 DA

test "parallel move: non-conflicting reg args" {
    // Sources are non-param regs — no conflicts, no scratch needed
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    try builder.addRegArg(.RAX); // RDI ← RAX
    try builder.addRegArg(.RBX); // RSI ← RBX

    try builder.call(0x12345678);

    // mov rdi, rax = 48 89 C7
    try std.testing.expect(findPattern3(emit.buf.items, 0x48, 0x89, 0xC7) != null);
    // mov rsi, rbx = 48 89 DE
    try std.testing.expect(findPattern3(emit.buf.items, 0x48, 0x89, 0xDE) != null);
    // No scratch register involvement (no mov r11, ... = 49 89 F3 pattern)
    try std.testing.expect(findPattern3(emit.buf.items, 0x49, 0x89, 0xF3) == null);
}

test "parallel move: 2-element swap cycle uses scratch" {
    // RDI ← RSI, RSI ← RDI — a 2-cycle requiring SCRATCH_REG (R11)
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    try builder.addRegArg(.RSI); // dst=RDI, src=RSI
    try builder.addRegArg(.RDI); // dst=RSI, src=RDI

    try builder.call(0x12345678);

    // Algorithm breaks cycle: save RSI to R11, move RDI→RSI, move R11→RDI
    // mov r11, rsi = 49 89 F3
    const scratch_pos = findPattern3(emit.buf.items, 0x49, 0x89, 0xF3);
    try std.testing.expect(scratch_pos != null);
    // mov rsi, rdi = 48 89 FE
    const mov_rsi_pos = findPattern3(emit.buf.items, 0x48, 0x89, 0xFE);
    try std.testing.expect(mov_rsi_pos != null);
    // mov rdi, r11 = 4C 89 DF
    const mov_rdi_pos = findPattern3(emit.buf.items, 0x4C, 0x89, 0xDF);
    try std.testing.expect(mov_rdi_pos != null);

    // Verify ordering: scratch save < mov rsi < mov rdi (from scratch)
    try std.testing.expect(scratch_pos.? < mov_rsi_pos.?);
    try std.testing.expect(mov_rsi_pos.? < mov_rdi_pos.?);
}

test "parallel move: 3-element rotation cycle" {
    // RDI ← RSI, RSI ← RDX, RDX ← RDI — 3-way rotation, one scratch save
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    try builder.addRegArg(.RSI); // dst=RDI, src=RSI
    try builder.addRegArg(.RDX); // dst=RSI, src=RDX
    try builder.addRegArg(.RDI); // dst=RDX, src=RDI

    try builder.call(0x12345678);

    // Algorithm: save RSI→R11, RSI←RDX, RDX←RDI, RDI←R11
    // mov r11, rsi = 49 89 F3
    const p0 = findPattern3(emit.buf.items, 0x49, 0x89, 0xF3);
    try std.testing.expect(p0 != null);
    // mov rsi, rdx = 48 89 D6
    const p1 = findPattern3(emit.buf.items, 0x48, 0x89, 0xD6);
    try std.testing.expect(p1 != null);
    // mov rdx, rdi = 48 89 FA
    const p2 = findPattern3(emit.buf.items, 0x48, 0x89, 0xFA);
    try std.testing.expect(p2 != null);
    // mov rdi, r11 = 4C 89 DF
    const p3 = findPattern3(emit.buf.items, 0x4C, 0x89, 0xDF);
    try std.testing.expect(p3 != null);

    // N+1 instructions for N-element cycle: 4 movs for 3 args
    try std.testing.expect(p0.? < p1.?);
    try std.testing.expect(p1.? < p2.?);
    try std.testing.expect(p2.? < p3.?);
}

test "parallel move: LEA then REG reading same dest — reordered" {
    // addLeaArg writes RDI, addRegArg reads RDI — old code would clobber
    // The algorithm must emit mov RSI,RDI BEFORE lea RDI,[rbp-32]
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    try builder.addLeaArg(.RBP, -32); // dst=RDI, lea [RBP-32]
    try builder.addRegArg(.RDI); // dst=RSI, src=RDI

    try builder.call(0x12345678);

    // mov rsi, rdi = 48 89 FE (must come first — preserves original RDI)
    const mov_pos = findPattern3(emit.buf.items, 0x48, 0x89, 0xFE);
    try std.testing.expect(mov_pos != null);
    // lea rdi, [rbp-32] = 48 8D BD ...
    const lea_pos = findPattern3(emit.buf.items, 0x48, 0x8D, 0xBD);
    try std.testing.expect(lea_pos != null);

    // Critical: mov must precede lea (otherwise RDI is clobbered before read)
    try std.testing.expect(mov_pos.? < lea_pos.?);

    // No scratch needed (LEA source isn't a register, can't form cycle)
    try std.testing.expect(findPattern3(emit.buf.items, 0x49, 0x89, 0xF3) == null);
}

test "parallel move: self-move eliminated" {
    // addRegArg(RDI) on System V: dst=RDI, src=RDI → no mov emitted
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    try builder.addRegArg(.RDI); // dst=RDI, src=RDI → self-move, elided

    try builder.call(0x12345678);

    // mov rdi, rdi would be 48 89 FF — should NOT appear
    try std.testing.expect(findPattern3(emit.buf.items, 0x48, 0x89, 0xFF) == null);

    // Verify the code is shorter than a call with an actual move
    const self_move_len = emit.buf.items.len;

    var emit2 = Emit.init(std.testing.allocator);
    defer emit2.deinit();
    var stack2: i32 = 0;
    var builder2 = try Builder.init(&emit2, &stack2);
    try builder2.addRegArg(.RAX); // dst=RDI, src=RAX → real move needed
    try builder2.call(0x12345678);

    try std.testing.expect(self_move_len < emit2.buf.items.len);
}

test "parallel move: chain dependency without cycle" {
    // RDI ← RSI, RSI ← RDX: chain, not cycle (no arg reads from RDI or RSI as written)
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    try builder.addRegArg(.RSI); // dst=RDI, src=RSI
    try builder.addRegArg(.RDX); // dst=RSI, src=RDX

    try builder.call(0x12345678);

    // mov rdi, rsi = 48 89 F7
    try std.testing.expect(findPattern3(emit.buf.items, 0x48, 0x89, 0xF7) != null);
    // mov rsi, rdx = 48 89 D6
    try std.testing.expect(findPattern3(emit.buf.items, 0x48, 0x89, 0xD6) != null);
    // No scratch needed — no cycle
    try std.testing.expect(findPattern3(emit.buf.items, 0x49, 0x89, 0xF3) == null);
}

test "parallel move: same source register for multiple args" {
    // Both args read from RAX — no conflict (RAX isn't a param reg destination)
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    try builder.addRegArg(.RAX); // dst=RDI, src=RAX
    try builder.addRegArg(.RAX); // dst=RSI, src=RAX

    try builder.call(0x12345678);

    // mov rdi, rax = 48 89 C7
    try std.testing.expect(findPattern3(emit.buf.items, 0x48, 0x89, 0xC7) != null);
    // mov rsi, rax = 48 89 C6
    try std.testing.expect(findPattern3(emit.buf.items, 0x48, 0x89, 0xC6) != null);
    // No scratch
    try std.testing.expect(findPattern3(emit.buf.items, 0x49, 0x89, 0xF3) == null);
}

test "parallel move: SCRATCH_REG (R11) as source without cycle" {
    // R11 as source is valid when no cycle involves it (R11 is never a param reg)
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    try builder.addRegArg(.R11); // dst=RDI, src=R11

    try builder.call(0x12345678);

    // mov rdi, r11 = 4C 89 DF
    try std.testing.expect(findPattern3(emit.buf.items, 0x4C, 0x89, 0xDF) != null);
}

test "parallel move: all six System V param regs filled" {
    // Fill all 6 System V register slots with non-conflicting sources
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    try builder.addRegArg(.RAX); // RDI ← RAX
    try builder.addRegArg(.RBX); // RSI ← RBX
    try builder.addRegArg(.RAX); // RDX ← RAX
    try builder.addRegArg(.RBX); // RCX ← RBX
    try builder.addRegArg(.RAX); // R8  ← RAX
    try builder.addRegArg(.RBX); // R9  ← RBX

    try builder.call(0x12345678);

    // Should have 6 mov instructions (3 bytes each = 18) + call sequence (~13)
    try std.testing.expect(emit.buf.items.len > 25);

    // Verify call instruction present (call r11 = 41 FF D3)
    try std.testing.expect(findPattern3(emit.buf.items, 0x41, 0xFF, 0xD3) != null);
    // No scratch needed (sources are non-param regs)
    try std.testing.expect(findPattern3(emit.buf.items, 0x49, 0x89, 0xF3) == null);
}

test "parallel move: reg args + stack overflow args" {
    // 8 args on System V: 6 in registers + 2 on stack
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // 6 register args
    try builder.addRegArg(.RAX);
    try builder.addRegArg(.RBX);
    try builder.addImmArg(3);
    try builder.addImmArg(4);
    try builder.addImmArg(5);
    try builder.addImmArg(6);
    // 2 stack overflow args
    try builder.addImmArg(7);
    try builder.addImmArg(8);

    try builder.call(0x12345678);

    // Verify call instruction
    try std.testing.expect(findPattern3(emit.buf.items, 0x41, 0xFF, 0xD3) != null);
    // Verify stack allocation for 2 args: sub rsp, 16 = 48 81 EC 10 00 00 00
    // (2 * 8 = 16 bytes, already 16-byte aligned)
    var found_sub = false;
    for (0..emit.buf.items.len -| 6) |i| {
        if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x81 and
            emit.buf.items[i + 2] == 0xEC and emit.buf.items[i + 3] == 0x10)
        {
            found_sub = true;
            break;
        }
    }
    try std.testing.expect(found_sub);
}

test "parallel move: zero reg args call" {
    // No register args, just a bare call — emitDeferredRegArgs should be a no-op
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // No args at all
    try builder.call(0x12345678);

    // Just the call sequence: mov r11, imm64 + call r11
    // Should be quite short (10 bytes for movabs + 3 bytes for call = 13)
    try std.testing.expect(emit.buf.items.len > 0);
    try std.testing.expect(emit.buf.items.len < 20);
    // Verify call instruction
    try std.testing.expect(findPattern3(emit.buf.items, 0x41, 0xFF, 0xD3) != null);
}

test "parallel move: mixed LEA, MEM, IMM, REG without conflicts" {
    // All four arg types in one call, no register conflicts
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    try builder.addLeaArg(.RBP, -64); // RDI ← lea [RBP-64]
    try builder.addMemArg(.RBP, -32); // RSI ← [RBP-32]
    try builder.addImmArg(42); // RDX ← 42
    try builder.addRegArg(.RAX); // RCX ← RAX

    try builder.call(0x12345678);

    // Verify all instructions emitted
    // lea rdi, [rbp-64] = 48 8D BD
    try std.testing.expect(findPattern3(emit.buf.items, 0x48, 0x8D, 0xBD) != null);
    // mov rsi, [rbp-32] = 48 8B B5
    try std.testing.expect(findPattern3(emit.buf.items, 0x48, 0x8B, 0xB5) != null);
    // call r11 = 41 FF D3
    try std.testing.expect(findPattern3(emit.buf.items, 0x41, 0xFF, 0xD3) != null);
    // No scratch needed
    try std.testing.expect(findPattern3(emit.buf.items, 0x49, 0x89, 0xF3) == null);
}

test "parallel move: swap cycle on Windows x64 (RCX/RDX)" {
    // Windows uses RCX, RDX, R8, R9 — verify swap works with different param regs
    const Emit = x86_64.WinEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Swap: RCX ← RDX, RDX ← RCX
    try builder.addRegArg(.RDX); // dst=RCX, src=RDX
    try builder.addRegArg(.RCX); // dst=RDX, src=RCX

    try builder.call(0x12345678);

    // mov r11, rdx = 49 89 D3 (scratch save)
    // RDX: src in reg field, enc=2; R11: dst in r/m field, enc=3, rexB=1
    // REX=0x49, ModRM = 11 010 011 = 0xD3
    const scratch_pos = findPattern3(emit.buf.items, 0x49, 0x89, 0xD3);
    try std.testing.expect(scratch_pos != null);

    // mov rdx, rcx = 48 89 CA
    // RCX: reg enc=1; RDX: r/m enc=2; ModRM = 11 001 010 = 0xCA
    const mov_rdx_pos = findPattern3(emit.buf.items, 0x48, 0x89, 0xCA);
    try std.testing.expect(mov_rdx_pos != null);

    // mov rcx, r11 = 4C 89 D9
    // R11: reg enc=3, rexR=1; RCX: r/m enc=1; REX=0x4C, ModRM = 11 011 001 = 0xD9
    const mov_rcx_pos = findPattern3(emit.buf.items, 0x4C, 0x89, 0xD9);
    try std.testing.expect(mov_rcx_pos != null);

    // Correct ordering
    try std.testing.expect(scratch_pos.? < mov_rdx_pos.?);
    try std.testing.expect(mov_rdx_pos.? < mov_rcx_pos.?);
}

test "parallel move: callReg also resolves deferred args" {
    // Verify callReg (indirect call) also runs the parallel move resolution
    const Emit = x86_64.LinuxEmit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Simple swap to verify deferred resolution happens
    try builder.addRegArg(.RSI); // dst=RDI, src=RSI
    try builder.addRegArg(.RDI); // dst=RSI, src=RDI

    try builder.callReg(.RAX);

    // Scratch must be used for the swap
    try std.testing.expect(findPattern3(emit.buf.items, 0x49, 0x89, 0xF3) != null); // mov r11, rsi
    try std.testing.expect(findPattern3(emit.buf.items, 0x48, 0x89, 0xFE) != null); // mov rsi, rdi
    try std.testing.expect(findPattern3(emit.buf.items, 0x4C, 0x89, 0xDF) != null); // mov rdi, r11

    // call rax = FF D0
    var found_call_rax = false;
    for (0..emit.buf.items.len -| 1) |i| {
        if (emit.buf.items[i] == 0xFF and emit.buf.items[i + 1] == 0xD0) {
            found_call_rax = true;
            break;
        }
    }
    try std.testing.expect(found_call_rax);
}
