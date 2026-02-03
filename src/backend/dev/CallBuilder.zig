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

const std = @import("std");
const builtin = @import("builtin");
const RocTarget = @import("roc_target").RocTarget;

const x86_64 = @import("x86_64/mod.zig");
const aarch64 = @import("aarch64/mod.zig");

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
                    .pass_by_ptr_threshold = 16,
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

    /// Check if argument needs to be passed by pointer
    pub fn needsPassByPointer(self: CallingConvention, arg_size: usize) bool {
        return arg_size > self.pass_by_ptr_threshold;
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

/// Comptime-known calling convention for host-native code generation
/// Use CallingConvention.forTarget() for cross-compilation support
pub const CC = struct {
    pub const PARAM_REGS = switch (builtin.cpu.arch) {
        .aarch64 => [_]aarch64.GeneralReg{
            .X0, .X1, .X2, .X3, .X4, .X5, .X6, .X7,
        },
        .x86_64 => if (builtin.os.tag == .windows)
            [_]x86_64.GeneralReg{ .RCX, .RDX, .R8, .R9 }
        else
            [_]x86_64.GeneralReg{ .RDI, .RSI, .RDX, .RCX, .R8, .R9 },
        else => @compileError("Unsupported architecture"),
    };

    pub const RETURN_REGS = switch (builtin.cpu.arch) {
        .aarch64 => [_]aarch64.GeneralReg{ .X0, .X1 },
        .x86_64 => if (builtin.os.tag == .windows)
            [_]x86_64.GeneralReg{.RAX}
        else
            [_]x86_64.GeneralReg{ .RAX, .RDX },
        else => @compileError("Unsupported architecture"),
    };

    pub const SHADOW_SPACE: u8 = switch (builtin.cpu.arch) {
        .aarch64 => 0,
        .x86_64 => if (builtin.os.tag == .windows) 32 else 0,
        else => 0,
    };

    /// Threshold for "return via pointer" (first arg is output ptr)
    pub const RETURN_BY_PTR_THRESHOLD: usize = switch (builtin.cpu.arch) {
        .aarch64 => 16,
        .x86_64 => if (builtin.os.tag == .windows) 8 else 16,
        else => 16,
    };

    /// Threshold for "pass by pointer" (large struct arg becomes pointer)
    pub const PASS_BY_PTR_THRESHOLD: usize = switch (builtin.cpu.arch) {
        .x86_64 => if (builtin.os.tag == .windows) 16 else std.math.maxInt(usize),
        else => std.math.maxInt(usize),
    };

    pub const SCRATCH_REG = switch (builtin.cpu.arch) {
        .aarch64 => aarch64.GeneralReg.X9,
        .x86_64 => x86_64.GeneralReg.R11,
        else => @compileError("Unsupported architecture"),
    };

    pub const BASE_PTR = switch (builtin.cpu.arch) {
        .aarch64 => aarch64.GeneralReg.FP,
        .x86_64 => x86_64.GeneralReg.RBP,
        else => @compileError("Unsupported architecture"),
    };

    pub const STACK_PTR = switch (builtin.cpu.arch) {
        .aarch64 => aarch64.GeneralReg.ZRSP,
        .x86_64 => x86_64.GeneralReg.RSP,
        else => @compileError("Unsupported architecture"),
    };
};

/// Call builder for setting up cross-platform function calls
pub fn CallBuilder(comptime Emit: type) type {
    const GeneralReg = switch (builtin.cpu.arch) {
        .aarch64 => aarch64.GeneralReg,
        .x86_64 => x86_64.GeneralReg,
        else => @compileError("Unsupported architecture"),
    };

    // Represents a deferred stack argument to be stored after stack allocation
    const StackArg = union(enum) {
        // Store from a register: mov [RSP+offset], reg
        from_reg: GeneralReg,
        // Store an immediate value: mov [RSP+offset], imm
        from_imm: i64,
        // Store LEA result: lea scratch, [base+offset]; mov [RSP+stack_offset], scratch
        from_lea: struct { base: GeneralReg, offset: i32 },
        // Store memory value: mov scratch, [base+offset]; mov [RSP+stack_offset], scratch
        from_mem: struct { base: GeneralReg, offset: i32 },
    };

    // Maximum stack arguments we support (should be plenty for any real call)
    const MAX_STACK_ARGS = 16;

    return struct {
        const Self = @This();

        emit: *Emit,
        int_arg_index: usize = 0,
        stack_arg_count: usize = 0,
        stack_args: [MAX_STACK_ARGS]StackArg = undefined,
        return_by_ptr: bool = false,

        pub fn init(emit: *Emit) Self {
            return .{ .emit = emit };
        }

        /// Check if return type needs to use pointer (implicit first arg)
        pub fn needsReturnByPointer(return_size: usize) bool {
            return return_size > CC.RETURN_BY_PTR_THRESHOLD;
        }

        /// Check if argument needs to be passed by pointer
        pub fn needsPassByPointer(arg_size: usize) bool {
            return arg_size > CC.PASS_BY_PTR_THRESHOLD;
        }

        /// Set up return by pointer (for large return types)
        /// Call this FIRST before adding any arguments
        pub fn setReturnByPointer(self: *Self, offset: i32) !void {
            std.debug.assert(self.int_arg_index == 0);
            self.return_by_ptr = true;
            try self.addLeaArg(CC.BASE_PTR, offset);
        }

        /// Add argument from a general register
        pub fn addRegArg(self: *Self, src_reg: GeneralReg) !void {
            if (self.int_arg_index < CC.PARAM_REGS.len) {
                const dst = CC.PARAM_REGS[self.int_arg_index];
                if (dst != src_reg) {
                    if (comptime builtin.cpu.arch == .aarch64) {
                        try self.emit.movRegReg(.w64, dst, src_reg);
                    } else {
                        try self.emit.movRegReg(.w64, dst, src_reg);
                    }
                }
                self.int_arg_index += 1;
            } else {
                if (comptime builtin.cpu.arch == .aarch64) {
                    // aarch64: push to stack (str with pre-decrement or stp)
                    @panic("Stack args not yet implemented for aarch64 CallBuilder");
                } else {
                    // Defer stack arg - will be stored after stack allocation in call()
                    std.debug.assert(self.stack_arg_count < MAX_STACK_ARGS);
                    self.stack_args[self.stack_arg_count] = .{ .from_reg = src_reg };
                    self.stack_arg_count += 1;
                }
            }
        }

        /// Add argument by loading a pointer (LEA) to a stack location
        pub fn addLeaArg(self: *Self, base_reg: GeneralReg, offset: i32) !void {
            if (self.int_arg_index < CC.PARAM_REGS.len) {
                const dst = CC.PARAM_REGS[self.int_arg_index];
                if (comptime builtin.cpu.arch == .aarch64) {
                    // aarch64: ADD dst, base, #offset
                    try self.emit.addRegRegImm12(.w64, dst, base_reg, @intCast(offset));
                } else {
                    try self.emit.leaRegMem(dst, base_reg, offset);
                }
                self.int_arg_index += 1;
            } else {
                if (comptime builtin.cpu.arch == .aarch64) {
                    @panic("Stack args not yet implemented for aarch64 CallBuilder");
                } else {
                    // Defer stack arg - will be stored after stack allocation in call()
                    std.debug.assert(self.stack_arg_count < MAX_STACK_ARGS);
                    self.stack_args[self.stack_arg_count] = .{ .from_lea = .{ .base = base_reg, .offset = offset } };
                    self.stack_arg_count += 1;
                }
            }
        }

        /// Add argument by loading from stack memory
        pub fn addMemArg(self: *Self, base_reg: GeneralReg, offset: i32) !void {
            if (self.int_arg_index < CC.PARAM_REGS.len) {
                const dst = CC.PARAM_REGS[self.int_arg_index];
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.emit.ldrRegRegImm(.w64, dst, base_reg, offset);
                } else {
                    try self.emit.movRegMem(.w64, dst, base_reg, offset);
                }
                self.int_arg_index += 1;
            } else {
                if (comptime builtin.cpu.arch == .aarch64) {
                    @panic("Stack args not yet implemented for aarch64 CallBuilder");
                } else {
                    // Defer stack arg - will be stored after stack allocation in call()
                    std.debug.assert(self.stack_arg_count < MAX_STACK_ARGS);
                    self.stack_args[self.stack_arg_count] = .{ .from_mem = .{ .base = base_reg, .offset = offset } };
                    self.stack_arg_count += 1;
                }
            }
        }

        /// Add immediate value argument
        pub fn addImmArg(self: *Self, value: i64) !void {
            if (self.int_arg_index < CC.PARAM_REGS.len) {
                const dst = CC.PARAM_REGS[self.int_arg_index];
                if (comptime builtin.cpu.arch == .aarch64) {
                    try self.emit.movRegImm64(dst, @bitCast(value));
                } else {
                    try self.emit.movRegImm64(dst, value);
                }
                self.int_arg_index += 1;
            } else {
                if (comptime builtin.cpu.arch == .aarch64) {
                    @panic("Stack args not yet implemented for aarch64 CallBuilder");
                } else {
                    // Defer stack arg - will be stored after stack allocation in call()
                    std.debug.assert(self.stack_arg_count < MAX_STACK_ARGS);
                    self.stack_args[self.stack_arg_count] = .{ .from_imm = value };
                    self.stack_arg_count += 1;
                }
            }
        }

        /// Add a struct argument, handling pass-by-pointer on Windows
        pub fn addStructArg(self: *Self, offset: i32, size: usize) !void {
            if (size <= 8) {
                try self.addMemArg(CC.BASE_PTR, offset);
            } else if (size <= 16) {
                try self.addMemArg(CC.BASE_PTR, offset);
                try self.addMemArg(CC.BASE_PTR, offset + 8);
            } else if (needsPassByPointer(size)) {
                // Windows: pass pointer to struct
                try self.addLeaArg(CC.BASE_PTR, offset);
            } else {
                // System V: copy to stack (TODO: implement)
                @panic("Large struct stack copy not yet implemented");
            }
        }

        /// Emit call instruction and handle cleanup
        pub fn call(self: *Self, fn_addr: usize) !void {
            // Calculate total stack space needed
            const stack_args_space: u32 = @intCast(self.stack_arg_count * 8);
            // On Windows x64, add 8 bytes for R12 save slot (past shadow space and stack args)
            const r12_save_space: u32 = if (comptime builtin.os.tag == .windows and builtin.cpu.arch == .x86_64) 8 else 0;
            const total_unaligned: u32 = CC.SHADOW_SPACE + stack_args_space + r12_save_space;
            // Round up to 16-byte alignment (Windows x64 ABI requirement)
            const total_space: u32 = (total_unaligned + 15) & ~@as(u32, 15);

            // Calculate R12 save offset - at the end, past shadow space and stack args
            const r12_save_offset: i32 = @intCast(CC.SHADOW_SPACE + stack_args_space);

            if (comptime builtin.cpu.arch == .x86_64) {
                // Allocate all stack space at once
                if (total_space > 0) {
                    try self.emit.subRegImm32(.w64, CC.STACK_PTR, @intCast(total_space));
                }

                // Save R12 to our dedicated slot (past shadow space and stack args)
                // R12 holds roc_ops and while it's callee-saved, Zig-compiled C functions
                // don't always preserve it correctly on Windows x64.
                // We can't use the shadow space because the callee is allowed to use it.
                if (comptime builtin.os.tag == .windows) {
                    try self.emit.movMemReg(.w64, CC.STACK_PTR, r12_save_offset, .R12);
                }

                // Store deferred stack arguments at correct offsets
                // Stack args go after shadow space: [RSP+32], [RSP+40], etc.
                for (self.stack_args[0..self.stack_arg_count], 0..) |arg, i| {
                    const stack_offset: i32 = @intCast(CC.SHADOW_SPACE + i * 8);
                    switch (arg) {
                        .from_reg => |reg| {
                            try self.emit.movMemReg(.w64, CC.STACK_PTR, stack_offset, reg);
                        },
                        .from_imm => |value| {
                            try self.emit.movRegImm64(CC.SCRATCH_REG, value);
                            try self.emit.movMemReg(.w64, CC.STACK_PTR, stack_offset, CC.SCRATCH_REG);
                        },
                        .from_lea => |lea| {
                            try self.emit.leaRegMem(CC.SCRATCH_REG, lea.base, lea.offset);
                            try self.emit.movMemReg(.w64, CC.STACK_PTR, stack_offset, CC.SCRATCH_REG);
                        },
                        .from_mem => |mem| {
                            try self.emit.movRegMem(.w64, CC.SCRATCH_REG, mem.base, mem.offset);
                            try self.emit.movMemReg(.w64, CC.STACK_PTR, stack_offset, CC.SCRATCH_REG);
                        },
                    }
                }
            } else if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64 doesn't need shadow space, but may need stack args
                if (self.stack_arg_count > 0) {
                    @panic("Stack args not yet implemented for aarch64 CallBuilder");
                }
            }

            // Load function address into scratch register
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.emit.movRegImm64(CC.SCRATCH_REG, fn_addr);
            } else {
                try self.emit.movRegImm64(CC.SCRATCH_REG, @bitCast(@as(i64, @intCast(fn_addr))));
            }

            // Call through scratch register
            if (comptime builtin.cpu.arch == .aarch64) {
                try self.emit.blrReg(CC.SCRATCH_REG);
            } else {
                try self.emit.callReg(CC.SCRATCH_REG);
            }

            // Cleanup
            if (comptime builtin.cpu.arch == .x86_64) {
                // Restore R12 from our dedicated slot before restoring stack pointer
                if (comptime builtin.os.tag == .windows) {
                    try self.emit.movRegMem(.w64, .R12, CC.STACK_PTR, r12_save_offset);
                }

                // Restore stack pointer
                if (total_space > 0) {
                    try self.emit.addRegImm32(.w64, CC.STACK_PTR, @intCast(total_space));
                }
            } else if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64: stack cleanup would be different (restore SP)
            }
        }

        /// Call through a register (for indirect calls)
        pub fn callReg(self: *Self, target: GeneralReg) !void {
            // Calculate total stack space needed
            const stack_args_space: u32 = @intCast(self.stack_arg_count * 8);
            // On Windows x64, add 8 bytes for R12 save slot (past shadow space and stack args)
            const r12_save_space: u32 = if (comptime builtin.os.tag == .windows and builtin.cpu.arch == .x86_64) 8 else 0;
            const total_unaligned: u32 = CC.SHADOW_SPACE + stack_args_space + r12_save_space;
            // Round up to 16-byte alignment (Windows x64 ABI requirement)
            const total_space: u32 = (total_unaligned + 15) & ~@as(u32, 15);

            // Calculate R12 save offset - at the end, past shadow space and stack args
            const r12_save_offset: i32 = @intCast(CC.SHADOW_SPACE + stack_args_space);

            if (comptime builtin.cpu.arch == .x86_64) {
                // Allocate all stack space at once
                if (total_space > 0) {
                    try self.emit.subRegImm32(.w64, CC.STACK_PTR, @intCast(total_space));
                }

                // Save R12 to our dedicated slot (past shadow space and stack args)
                // R12 holds roc_ops and while it's callee-saved, Zig-compiled C functions
                // don't always preserve it correctly on Windows x64.
                // We can't use the shadow space because the callee is allowed to use it.
                if (comptime builtin.os.tag == .windows) {
                    try self.emit.movMemReg(.w64, CC.STACK_PTR, r12_save_offset, .R12);
                }

                // Store deferred stack arguments at correct offsets
                // Stack args go after shadow space: [RSP+32], [RSP+40], etc.
                for (self.stack_args[0..self.stack_arg_count], 0..) |arg, i| {
                    const stack_offset: i32 = @intCast(CC.SHADOW_SPACE + i * 8);
                    switch (arg) {
                        .from_reg => |reg| {
                            try self.emit.movMemReg(.w64, CC.STACK_PTR, stack_offset, reg);
                        },
                        .from_imm => |value| {
                            try self.emit.movRegImm64(CC.SCRATCH_REG, value);
                            try self.emit.movMemReg(.w64, CC.STACK_PTR, stack_offset, CC.SCRATCH_REG);
                        },
                        .from_lea => |lea| {
                            try self.emit.leaRegMem(CC.SCRATCH_REG, lea.base, lea.offset);
                            try self.emit.movMemReg(.w64, CC.STACK_PTR, stack_offset, CC.SCRATCH_REG);
                        },
                        .from_mem => |mem| {
                            try self.emit.movRegMem(.w64, CC.SCRATCH_REG, mem.base, mem.offset);
                            try self.emit.movMemReg(.w64, CC.STACK_PTR, stack_offset, CC.SCRATCH_REG);
                        },
                    }
                }
            } else if (comptime builtin.cpu.arch == .aarch64) {
                if (self.stack_arg_count > 0) {
                    @panic("Stack args not yet implemented for aarch64 CallBuilder");
                }
            }

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.emit.blrReg(target);
            } else {
                try self.emit.callReg(target);
            }

            // Cleanup
            if (comptime builtin.cpu.arch == .x86_64) {
                // Restore R12 from our dedicated slot before restoring stack pointer
                if (comptime builtin.os.tag == .windows) {
                    try self.emit.movRegMem(.w64, .R12, CC.STACK_PTR, r12_save_offset);
                }

                // Restore stack pointer
                if (total_space > 0) {
                    try self.emit.addRegImm32(.w64, CC.STACK_PTR, @intCast(total_space));
                }
            }
        }

        /// Get the return register for the result
        pub fn getReturnReg() GeneralReg {
            return CC.RETURN_REGS[0];
        }

        /// Get the number of argument registers available
        pub fn getNumArgRegs() usize {
            return CC.PARAM_REGS.len;
        }

        /// Get argument register at index (for manual setup)
        pub fn getArgReg(index: usize) GeneralReg {
            return CC.PARAM_REGS[index];
        }
    };
}

// Tests
test "CallingConvention.forTarget Windows" {
    const cc = CallingConvention.forTarget(.x64win);
    try std.testing.expectEqual(@as(usize, 4), cc.num_param_regs);
    try std.testing.expectEqual(@as(u8, 32), cc.shadow_space);
    try std.testing.expectEqual(@as(usize, 8), cc.return_by_ptr_threshold);
    try std.testing.expectEqual(@as(usize, 16), cc.pass_by_ptr_threshold);
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

    // Windows: threshold is 16 bytes
    try std.testing.expect(!win_cc.needsPassByPointer(16));
    try std.testing.expect(win_cc.needsPassByPointer(17));

    // System V: never passes by pointer (uses stack)
    try std.testing.expect(!sysv_cc.needsPassByPointer(1000));
}

test "CC constants are consistent" {
    // Basic sanity checks
    try std.testing.expect(CC.PARAM_REGS.len >= 4);
    try std.testing.expect(CC.RETURN_REGS.len >= 1);

    // Shadow space should only exist on Windows x86_64
    if (builtin.cpu.arch == .x86_64) {
        if (builtin.os.tag == .windows) {
            try std.testing.expectEqual(@as(u8, 32), CC.SHADOW_SPACE);
            try std.testing.expectEqual(@as(usize, 8), CC.RETURN_BY_PTR_THRESHOLD);
            try std.testing.expectEqual(@as(usize, 16), CC.PASS_BY_PTR_THRESHOLD);
        } else {
            try std.testing.expectEqual(@as(u8, 0), CC.SHADOW_SPACE);
            try std.testing.expectEqual(@as(usize, 16), CC.RETURN_BY_PTR_THRESHOLD);
            try std.testing.expectEqual(std.math.maxInt(usize), CC.PASS_BY_PTR_THRESHOLD);
        }
    }
}

test "needsReturnByPointer" {
    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    // Small values don't need pointer
    try std.testing.expect(!Builder.needsReturnByPointer(1));
    try std.testing.expect(!Builder.needsReturnByPointer(8));

    // Large values need pointer (threshold is 8 on Windows, 16 on System V)
    if (builtin.os.tag == .windows and builtin.cpu.arch == .x86_64) {
        try std.testing.expect(Builder.needsReturnByPointer(9));
        try std.testing.expect(Builder.needsReturnByPointer(16));
    } else if (builtin.cpu.arch == .x86_64) {
        try std.testing.expect(!Builder.needsReturnByPointer(16));
        try std.testing.expect(Builder.needsReturnByPointer(17));
    }
}

test "needsPassByPointer" {
    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    // Small values don't need pointer
    try std.testing.expect(!Builder.needsPassByPointer(8));
    try std.testing.expect(!Builder.needsPassByPointer(16));

    // On Windows, large structs (>16 bytes) are passed by pointer
    if (builtin.os.tag == .windows and builtin.cpu.arch == .x86_64) {
        try std.testing.expect(Builder.needsPassByPointer(17));
        try std.testing.expect(Builder.needsPassByPointer(24));
    } else if (builtin.cpu.arch == .x86_64) {
        // System V never passes by pointer (uses stack instead)
        try std.testing.expect(!Builder.needsPassByPointer(17));
        try std.testing.expect(!Builder.needsPassByPointer(1000));
    }
}

test "Windows x64 argument registers" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    if (builtin.os.tag == .windows) {
        // Windows uses RCX, RDX, R8, R9
        try std.testing.expectEqual(x86_64.GeneralReg.RCX, Builder.getArgReg(0));
        try std.testing.expectEqual(x86_64.GeneralReg.RDX, Builder.getArgReg(1));
        try std.testing.expectEqual(x86_64.GeneralReg.R8, Builder.getArgReg(2));
        try std.testing.expectEqual(x86_64.GeneralReg.R9, Builder.getArgReg(3));
        try std.testing.expectEqual(@as(usize, 4), Builder.getNumArgRegs());
    } else {
        // System V uses RDI, RSI, RDX, RCX, R8, R9
        try std.testing.expectEqual(x86_64.GeneralReg.RDI, Builder.getArgReg(0));
        try std.testing.expectEqual(x86_64.GeneralReg.RSI, Builder.getArgReg(1));
        try std.testing.expectEqual(x86_64.GeneralReg.RDX, Builder.getArgReg(2));
        try std.testing.expectEqual(x86_64.GeneralReg.RCX, Builder.getArgReg(3));
        try std.testing.expectEqual(x86_64.GeneralReg.R8, Builder.getArgReg(4));
        try std.testing.expectEqual(x86_64.GeneralReg.R9, Builder.getArgReg(5));
        try std.testing.expectEqual(@as(usize, 6), Builder.getNumArgRegs());
    }
}
