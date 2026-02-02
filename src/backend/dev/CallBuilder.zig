//! Cross-platform calling convention abstraction
//!
//! Handles differences between:
//! - x86_64 System V (Linux, macOS, BSD)
//! - x86_64 Windows Fastcall
//! - aarch64 AAPCS64
//!
//! Key features:
//! - Automatic shadow space allocation (Windows)
//! - Return-by-pointer for large returns
//! - Pass-by-pointer for large structs (Windows)
//! - Correct argument register ordering per platform

const std = @import("std");
const builtin = @import("builtin");

const x86_64 = @import("x86_64/mod.zig");
const aarch64 = @import("aarch64/mod.zig");

/// Platform-specific calling convention constants
pub const CC = struct {
    pub const PARAM_REGS = switch (builtin.cpu.arch) {
        .aarch64 => [_]aarch64.Registers.GeneralReg{
            .X0, .X1, .X2, .X3, .X4, .X5, .X6, .X7,
        },
        .x86_64 => if (builtin.os.tag == .windows)
            [_]x86_64.Registers.GeneralReg{ .RCX, .RDX, .R8, .R9 }
        else
            [_]x86_64.Registers.GeneralReg{ .RDI, .RSI, .RDX, .RCX, .R8, .R9 },
        else => @compileError("Unsupported architecture"),
    };

    pub const RETURN_REGS = switch (builtin.cpu.arch) {
        .aarch64 => [_]aarch64.Registers.GeneralReg{ .X0, .X1 },
        .x86_64 => if (builtin.os.tag == .windows)
            [_]x86_64.Registers.GeneralReg{.RAX}
        else
            [_]x86_64.Registers.GeneralReg{ .RAX, .RDX },
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
        .aarch64 => aarch64.Registers.GeneralReg.X9,
        .x86_64 => x86_64.Registers.GeneralReg.R11,
        else => @compileError("Unsupported architecture"),
    };

    pub const BASE_PTR = switch (builtin.cpu.arch) {
        .aarch64 => aarch64.Registers.GeneralReg.FP,
        .x86_64 => x86_64.Registers.GeneralReg.RBP,
        else => @compileError("Unsupported architecture"),
    };

    pub const STACK_PTR = switch (builtin.cpu.arch) {
        .aarch64 => aarch64.Registers.GeneralReg.ZRSP,
        .x86_64 => x86_64.Registers.GeneralReg.RSP,
        else => @compileError("Unsupported architecture"),
    };
};

/// Call builder for setting up cross-platform function calls
pub fn CallBuilder(comptime Emit: type) type {
    const GeneralReg = switch (builtin.cpu.arch) {
        .aarch64 => aarch64.Registers.GeneralReg,
        .x86_64 => x86_64.Registers.GeneralReg,
        else => @compileError("Unsupported architecture"),
    };

    return struct {
        const Self = @This();

        emit: *Emit,
        int_arg_index: usize = 0,
        stack_bytes: usize = 0,
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
                    try self.emit.pushReg(src_reg);
                }
                self.stack_bytes += 8;
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
                    try self.emit.leaRegMem(CC.SCRATCH_REG, base_reg, offset);
                    try self.emit.pushReg(CC.SCRATCH_REG);
                }
                self.stack_bytes += 8;
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
                    try self.emit.movRegMem(.w64, CC.SCRATCH_REG, base_reg, offset);
                    try self.emit.pushReg(CC.SCRATCH_REG);
                }
                self.stack_bytes += 8;
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
                    // x86_64: push immediate as 64-bit value
                    // There's no pushImm64, so we load to scratch first
                    try self.emit.movRegImm64(CC.SCRATCH_REG, value);
                    try self.emit.pushReg(CC.SCRATCH_REG);
                }
                self.stack_bytes += 8;
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
            // Allocate shadow space (Windows only)
            if (CC.SHADOW_SPACE > 0) {
                if (comptime builtin.cpu.arch == .x86_64) {
                    try self.emit.subRegImm32(.w64, CC.STACK_PTR, CC.SHADOW_SPACE);
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

            // Cleanup: shadow space + stack args
            const cleanup = CC.SHADOW_SPACE + @as(u32, @intCast(self.stack_bytes));
            if (cleanup > 0) {
                if (comptime builtin.cpu.arch == .x86_64) {
                    try self.emit.addRegImm32(.w64, CC.STACK_PTR, @intCast(cleanup));
                }
                // aarch64: stack cleanup would be different (restore SP)
            }
        }

        /// Call through a register (for indirect calls)
        pub fn callReg(self: *Self, target: GeneralReg) !void {
            if (CC.SHADOW_SPACE > 0) {
                if (comptime builtin.cpu.arch == .x86_64) {
                    try self.emit.subRegImm32(.w64, CC.STACK_PTR, CC.SHADOW_SPACE);
                }
            }

            if (comptime builtin.cpu.arch == .aarch64) {
                try self.emit.blrReg(target);
            } else {
                try self.emit.callReg(target);
            }

            const cleanup = CC.SHADOW_SPACE + @as(u32, @intCast(self.stack_bytes));
            if (cleanup > 0) {
                if (comptime builtin.cpu.arch == .x86_64) {
                    try self.emit.addRegImm32(.w64, CC.STACK_PTR, @intCast(cleanup));
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
        try std.testing.expectEqual(x86_64.Registers.GeneralReg.RCX, Builder.getArgReg(0));
        try std.testing.expectEqual(x86_64.Registers.GeneralReg.RDX, Builder.getArgReg(1));
        try std.testing.expectEqual(x86_64.Registers.GeneralReg.R8, Builder.getArgReg(2));
        try std.testing.expectEqual(x86_64.Registers.GeneralReg.R9, Builder.getArgReg(3));
        try std.testing.expectEqual(@as(usize, 4), Builder.getNumArgRegs());
    } else {
        // System V uses RDI, RSI, RDX, RCX, R8, R9
        try std.testing.expectEqual(x86_64.Registers.GeneralReg.RDI, Builder.getArgReg(0));
        try std.testing.expectEqual(x86_64.Registers.GeneralReg.RSI, Builder.getArgReg(1));
        try std.testing.expectEqual(x86_64.Registers.GeneralReg.RDX, Builder.getArgReg(2));
        try std.testing.expectEqual(x86_64.Registers.GeneralReg.RCX, Builder.getArgReg(3));
        try std.testing.expectEqual(x86_64.Registers.GeneralReg.R8, Builder.getArgReg(4));
        try std.testing.expectEqual(x86_64.Registers.GeneralReg.R9, Builder.getArgReg(5));
        try std.testing.expectEqual(@as(usize, 6), Builder.getNumArgRegs());
    }
}
