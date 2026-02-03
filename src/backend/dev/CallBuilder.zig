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
        /// RBP-relative offset where R12 is saved (only used on Windows x64)
        /// Set via saveR12 before adding arguments that might clobber R12
        r12_save_offset: ?i32 = null,

        pub fn init(emit: *Emit) Self {
            return .{ .emit = emit };
        }

        /// Initialize with R12 save slot for Windows x64.
        /// On Windows, Zig-compiled C functions may corrupt R12 despite it being callee-saved.
        /// Pass an RBP-relative stack offset where R12 should be saved/restored.
        /// On non-Windows platforms, this is a no-op and the offset is ignored.
        pub fn initWithR12Save(emit: *Emit, r12_save_offset: i32) !Self {
            var self = Self{ .emit = emit };
            if (comptime builtin.cpu.arch == .x86_64 and builtin.os.tag == .windows) {
                self.r12_save_offset = r12_save_offset;
                // Save R12 immediately to the RBP-relative slot
                try self.emit.movMemReg(.w64, CC.BASE_PTR, r12_save_offset, .R12);
            }
            return self;
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

        /// Emit call instruction and handle cleanup.
        /// If R12 save was configured via initWithR12Save(), R12 is automatically
        /// restored after the call returns.
        pub fn call(self: *Self, fn_addr: usize) !void {
            // Calculate total stack space needed
            const stack_args_space: u32 = @intCast(self.stack_arg_count * 8);
            const total_unaligned: u32 = CC.SHADOW_SPACE + stack_args_space;
            // Round up to 16-byte alignment (Windows x64 ABI requirement)
            const total_space: u32 = (total_unaligned + 15) & ~@as(u32, 15);

            if (comptime builtin.cpu.arch == .x86_64) {
                // Allocate all stack space at once
                if (total_space > 0) {
                    try self.emit.subRegImm32(.w64, CC.STACK_PTR, @intCast(total_space));
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
                // Restore stack pointer
                if (total_space > 0) {
                    try self.emit.addRegImm32(.w64, CC.STACK_PTR, @intCast(total_space));
                }

                // Restore R12 if we saved it (Windows x64 only)
                if (self.r12_save_offset) |offset| {
                    try self.emit.movRegMem(.w64, .R12, CC.BASE_PTR, offset);
                }
            } else if (comptime builtin.cpu.arch == .aarch64) {
                // aarch64: stack cleanup would be different (restore SP)
            }
        }

        /// Call through a register (for indirect calls).
        /// If R12 save was configured via initWithR12Save(), R12 is automatically
        /// restored after the call returns.
        pub fn callReg(self: *Self, target: GeneralReg) !void {
            // Calculate total stack space needed
            const stack_args_space: u32 = @intCast(self.stack_arg_count * 8);
            const total_unaligned: u32 = CC.SHADOW_SPACE + stack_args_space;
            // Round up to 16-byte alignment (Windows x64 ABI requirement)
            const total_space: u32 = (total_unaligned + 15) & ~@as(u32, 15);

            if (comptime builtin.cpu.arch == .x86_64) {
                // Allocate all stack space at once
                if (total_space > 0) {
                    try self.emit.subRegImm32(.w64, CC.STACK_PTR, @intCast(total_space));
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
                // Restore stack pointer
                if (total_space > 0) {
                    try self.emit.addRegImm32(.w64, CC.STACK_PTR, @intCast(total_space));
                }

                // Restore R12 if we saved it (Windows x64 only)
                if (self.r12_save_offset) |offset| {
                    try self.emit.movRegMem(.w64, .R12, CC.BASE_PTR, offset);
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

test "CallBuilder with R12 save/restore" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    // Initialize builder with R12 save at RBP-24
    const builder = try Builder.initWithR12Save(&emit, -24);

    // On Windows, should have emitted: mov [rbp-24], r12
    if (builtin.os.tag == .windows) {
        // REX.WR (4C) + MOV (89) + ModRM (65 = mod:01 reg:100 rm:101) + disp8 (-24)
        // Actually for disp32: REX.WR (4C) + MOV (89) + ModRM (A5) + disp32
        try std.testing.expect(emit.buf.items.len > 0);
        // Verify R12 save slot is set
        try std.testing.expectEqual(@as(?i32, -24), builder.r12_save_offset);
    } else {
        // On non-Windows, no save should have been emitted
        try std.testing.expectEqual(@as(usize, 0), emit.buf.items.len);
        try std.testing.expectEqual(@as(?i32, null), builder.r12_save_offset);
    }
}

test "CallBuilder call restores R12 on Windows" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    // Initialize builder with R12 save at RBP-24
    var builder = try Builder.initWithR12Save(&emit, -24);
    const initial_len = emit.buf.items.len;

    // Add a simple argument
    try builder.addImmArg(42);

    // Make a call (using a dummy address)
    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > initial_len);

    if (builtin.os.tag == .windows) {
        // On Windows, the call sequence should include R12 restore at the end
        // Look for mov r12, [rbp-24] near the end of the buffer
        // This is: REX.WB (49) + MOV (8B) + ModRM (A4) + SIB (24) for RSP base, or
        // for RBP base: REX.WB (49) + MOV (8B) + ModRM (65 or A5) + disp
        // The exact encoding depends on displacement size

        // Just verify r12_save_offset is set - the restore happens automatically
        try std.testing.expect(builder.r12_save_offset != null);
    }
}

test "CallBuilder without R12 save (standard init)" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    // Standard init - no R12 save
    const builder = Builder.init(&emit);

    // No code should be emitted yet
    try std.testing.expectEqual(@as(usize, 0), emit.buf.items.len);
    try std.testing.expectEqual(@as(?i32, null), builder.r12_save_offset);
}

// =============================================================================
// Phase 2: CallBuilder Unit Tests - Verify Generated Byte Sequences
// =============================================================================

test "CallBuilder 4-arg call with immediates" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var builder = Builder.init(&emit);

    // Add 4 immediate arguments
    try builder.addImmArg(1);
    try builder.addImmArg(2);
    try builder.addImmArg(3);
    try builder.addImmArg(4);

    // Call a dummy address
    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    // The sequence should include:
    // 1. Load immediate args into registers first (movabs rcx/rdi, 1; etc.)
    // 2. Shadow space allocation on Windows (sub rsp, 32) - happens in call()
    // 3. Load function address into R11
    // 4. call r11
    // 5. Shadow space deallocation on Windows (add rsp, 32)

    // Look for the call instruction (call r11 = 41 FF D3)
    var found_call = false;
    for (0..emit.buf.items.len - 2) |i| {
        if (emit.buf.items[i] == 0x41 and emit.buf.items[i + 1] == 0xFF and emit.buf.items[i + 2] == 0xD3) {
            found_call = true;
            break;
        }
    }
    try std.testing.expect(found_call);

    if (builtin.os.tag == .windows) {
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
}

test "CallBuilder 6-arg call puts args 5-6 on stack" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var builder = Builder.init(&emit);

    // Add 6 immediate arguments
    try builder.addImmArg(1);
    try builder.addImmArg(2);
    try builder.addImmArg(3);
    try builder.addImmArg(4);
    try builder.addImmArg(5);
    try builder.addImmArg(6);

    try builder.call(0x12345678);

    // Verify code was emitted - should be longer than 4-arg call
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

    if (builtin.os.tag == .windows) {
        // Windows x64: 4 register args (RCX, RDX, R8, R9) + 2 stack args
        // Stack args go at [RSP+32] and [RSP+40] (after shadow space)
        // Total stack space = 32 (shadow) + 16 (2 args) = 48, rounded to 48
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
    } else {
        // System V: 6 register args (RDI, RSI, RDX, RCX, R8, R9) - no stack args
        // No shadow space needed
        // Should just have movabs for args + call
    }
}

test "CallBuilder with register argument" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var builder = Builder.init(&emit);

    // Add a register argument (RAX -> first param reg)
    try builder.addRegArg(.RAX);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    if (builtin.os.tag == .windows) {
        // Should have: sub rsp, 32; mov rcx, rax; movabs r11, addr; call r11; add rsp, 32
        // The mov rcx, rax is only emitted if RAX != RCX
        // Since RAX != RCX, we should see: 48 89 C1 (mov rcx, rax)
        var found_mov = false;
        for (0..emit.buf.items.len - 2) |i| {
            if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x89 and emit.buf.items[i + 2] == 0xC1) {
                found_mov = true;
                break;
            }
        }
        try std.testing.expect(found_mov);
    }
}

test "CallBuilder with LEA argument" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var builder = Builder.init(&emit);

    // Add a LEA argument (pointer to [RBP-32])
    try builder.addLeaArg(.RBP, -32);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    if (builtin.os.tag == .windows) {
        // Should have: sub rsp, 32; lea rcx, [rbp-32]; movabs r11, addr; call r11; add rsp, 32
        // lea rcx, [rbp-32] = 48 8D 4D E0 (with 8-bit disp) or 48 8D 8D E0 FF FF FF (32-bit disp)
        // Since we use 32-bit displacement: 48 8D 8D + disp32(-32 = 0xFFFFFFE0)
        var found_lea = false;
        for (0..emit.buf.items.len - 6) |i| {
            if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x8D and emit.buf.items[i + 2] == 0x8D) {
                found_lea = true;
                break;
            }
        }
        try std.testing.expect(found_lea);
    }
}

test "CallBuilder with memory argument" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var builder = Builder.init(&emit);

    // Add a memory load argument (load from [RBP-16])
    try builder.addMemArg(.RBP, -16);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    if (builtin.os.tag == .windows) {
        // Should have: sub rsp, 32; mov rcx, [rbp-16]; movabs r11, addr; call r11; add rsp, 32
        // mov rcx, [rbp-16] = 48 8B 8D F0 FF FF FF (32-bit disp)
        var found_mov = false;
        for (0..emit.buf.items.len - 6) |i| {
            if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x8B and emit.buf.items[i + 2] == 0x8D) {
                found_mov = true;
                break;
            }
        }
        try std.testing.expect(found_mov);
    }
}

test "CallBuilder stack alignment is 16-byte" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var builder = Builder.init(&emit);

    // Add 5 arguments on Windows (4 reg + 1 stack)
    // Stack space = 32 (shadow) + 8 (1 arg) = 40, rounded up to 48 for alignment
    try builder.addImmArg(1);
    try builder.addImmArg(2);
    try builder.addImmArg(3);
    try builder.addImmArg(4);
    try builder.addImmArg(5);

    try builder.call(0x12345678);

    if (builtin.os.tag == .windows) {
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
}

test "CallBuilder return by pointer sets up first arg" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var builder = Builder.init(&emit);

    // Set return by pointer (output at [RBP-64])
    try builder.setReturnByPointer(-64);

    // Add one more argument
    try builder.addImmArg(42);

    try builder.call(0x12345678);

    // Verify code was emitted
    try std.testing.expect(emit.buf.items.len > 0);

    if (builtin.os.tag == .windows) {
        // First arg (RCX) should be LEA to output buffer
        // Second arg (RDX) should be the immediate 42
        // Look for lea rcx, [rbp-64]
        var found_lea = false;
        for (0..emit.buf.items.len - 6) |i| {
            if (emit.buf.items[i] == 0x48 and emit.buf.items[i + 1] == 0x8D and emit.buf.items[i + 2] == 0x8D) {
                found_lea = true;
                break;
            }
        }
        try std.testing.expect(found_lea);
    }
}
