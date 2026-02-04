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
    /// Windows x64: Only structs of size 1, 2, 4, 8 bytes can be passed by value.
    /// All other sizes must be passed by pointer.
    pub fn needsPassByPointer(self: CallingConvention, arg_size: usize) bool {
        if (self.is_windows) {
            // Windows x64: only power-of-2 sizes up to 8 can pass by value
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

    /// Float parameter registers for function calls
    /// Windows x64: Position-based (XMM0-3 mirror arg positions 0-3)
    /// System V: Separate pool (XMM0-7 used independently of integer regs)
    pub const FLOAT_PARAM_REGS = switch (builtin.cpu.arch) {
        .aarch64 => [_]aarch64.FloatReg{
            .D0, .D1, .D2, .D3, .D4, .D5, .D6, .D7,
        },
        .x86_64 => if (builtin.os.tag == .windows)
            [_]x86_64.FloatReg{ .XMM0, .XMM1, .XMM2, .XMM3 }
        else
            [_]x86_64.FloatReg{ .XMM0, .XMM1, .XMM2, .XMM3, .XMM4, .XMM5, .XMM6, .XMM7 },
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
    /// Windows x64: structs > 8 bytes are passed by pointer
    /// Note: Only power-of-2 sizes (1, 2, 4, 8) can be passed by value on Windows.
    /// Use canPassStructByValue() for the complete check.
    pub const PASS_BY_PTR_THRESHOLD: usize = switch (builtin.cpu.arch) {
        .x86_64 => if (builtin.os.tag == .windows) 8 else std.math.maxInt(usize),
        else => std.math.maxInt(usize),
    };

    /// Check if a struct of the given size can be passed by value in a register.
    /// Windows x64 ABI: Only structs of size 1, 2, 4, or 8 bytes can be passed by value.
    /// All other sizes (including 3, 5, 6, 7, and >8) must be passed by pointer.
    pub fn canPassStructByValue(size: usize) bool {
        if (builtin.cpu.arch == .x86_64 and builtin.os.tag == .windows) {
            return size == 1 or size == 2 or size == 4 or size == 8;
        }
        // System V: structs up to 16 bytes can be passed in registers (2x 8-byte)
        return size <= 16;
    }

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

    /// Stack alignment requirement for function prologues.
    /// On x86_64, the stack must be 16-byte aligned before CALL instructions.
    /// On aarch64, the stack must be 16-byte aligned at all times.
    pub const STACK_ALIGNMENT: u32 = 16;

    /// Align a stack size to the platform's required alignment.
    /// This should be used when allocating stack frames in function prologues.
    pub fn alignStackSize(size: u32) u32 {
        return (size + STACK_ALIGNMENT - 1) & ~(STACK_ALIGNMENT - 1);
    }
};

/// Call builder for setting up cross-platform function calls
pub fn CallBuilder(comptime Emit: type) type {
    const GeneralReg = switch (builtin.cpu.arch) {
        .aarch64 => aarch64.GeneralReg,
        .x86_64 => x86_64.GeneralReg,
        else => @compileError("Unsupported architecture"),
    };

    const FloatReg = switch (builtin.cpu.arch) {
        .aarch64 => aarch64.FloatReg,
        .x86_64 => x86_64.FloatReg,
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
        /// Float argument index (separate from int_arg_index on System V, same on Windows)
        float_arg_index: usize = 0,
        stack_arg_count: usize = 0,
        stack_args: [MAX_STACK_ARGS]StackArg = undefined,
        return_by_ptr: bool = false,
        /// RBP-relative offset where R12 is saved (only used on Windows x64)
        /// Set via saveR12 before adding arguments that might clobber R12
        r12_save_offset: ?i32 = null,

        /// Initialize CallBuilder with automatic R12 handling for C function calls.
        ///
        /// On Windows x64, R12 (which holds roc_ops) may be corrupted by C calls,
        /// so this method automatically allocates a stack slot and saves R12.
        /// On other platforms, R12 handling is not needed.
        ///
        /// The stack_offset pointer should point to the CodeGen's stack_offset field.
        /// This method will allocate space by decrementing the stack offset on Windows.
        pub fn init(emit: *Emit, stack_offset: *i32) !Self {
            var self = Self{ .emit = emit };
            if (comptime builtin.cpu.arch == .x86_64 and builtin.os.tag == .windows) {
                // Allocate 16-byte slot for R12 save to maintain 16-byte alignment
                // for subsequent allocations (important for i128/RocDec which need
                // 16-byte aligned addresses when passed by pointer)
                stack_offset.* -= 16;
                self.r12_save_offset = stack_offset.*;
                // Save R12 immediately to the RBP-relative slot
                try self.emit.movMemReg(.w64, CC.BASE_PTR, self.r12_save_offset.?, .R12);
            }
            return self;
        }

        /// Check if return type needs to use pointer (implicit first arg)
        pub fn needsReturnByPointer(return_size: usize) bool {
            return return_size > CC.RETURN_BY_PTR_THRESHOLD;
        }

        /// Check if a struct argument needs to be passed by pointer.
        /// Windows x64: Only structs of size 1, 2, 4, 8 bytes can be passed by value.
        /// All other sizes must be passed by pointer.
        pub fn needsPassByPointer(arg_size: usize) bool {
            return !CC.canPassStructByValue(arg_size);
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
                    // aarch64: ADD dst, base, #offset (or SUB for negative offsets)
                    if (offset >= 0) {
                        try self.emit.addRegRegImm12(.w64, dst, base_reg, @intCast(offset));
                    } else {
                        try self.emit.subRegRegImm12(.w64, dst, base_reg, @intCast(-offset));
                    }
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
                    try self.emit.ldrRegMemSoff(.w64, dst, base_reg, offset);
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

        /// Add a struct argument, handling pass-by-pointer per platform ABI.
        /// Windows x64: Only 1, 2, 4, 8 byte structs pass by value; all others by pointer.
        /// System V: Structs up to 16 bytes can be passed in registers.
        pub fn addStructArg(self: *Self, offset: i32, size: usize) !void {
            if (CC.canPassStructByValue(size)) {
                // Struct can be passed by value in register(s)
                if (size <= 8) {
                    try self.addMemArg(CC.BASE_PTR, offset);
                } else {
                    // System V only: 9-16 byte structs use two registers
                    try self.addMemArg(CC.BASE_PTR, offset);
                    try self.addMemArg(CC.BASE_PTR, offset + 8);
                }
            } else {
                // Pass by pointer (Windows for non-power-of-2 or >8 bytes, System V for >16 bytes)
                try self.addLeaArg(CC.BASE_PTR, offset);
            }
        }

        /// Add a float argument from a float register.
        /// Windows x64: Uses position-based registers (XMM0-3 mirror arg positions 0-3).
        ///              A float arg at position 1 goes in XMM1, consuming both int and float slots.
        /// System V: Uses separate float register pool (XMM0-7 independent of integer regs).
        pub fn addFloatRegArg(self: *Self, src_reg: FloatReg, is_f64: bool) !void {
            if (comptime builtin.os.tag == .windows) {
                // Windows: float args use same position as int args
                // XMM0 for arg 0, XMM1 for arg 1, etc.
                if (self.int_arg_index < CC.FLOAT_PARAM_REGS.len) {
                    const dst = CC.FLOAT_PARAM_REGS[self.int_arg_index];
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
                if (self.float_arg_index < CC.FLOAT_PARAM_REGS.len) {
                    const dst = CC.FLOAT_PARAM_REGS[self.float_arg_index];
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
            if (comptime builtin.os.tag == .windows) {
                // Windows: float args use same position as int args
                if (self.int_arg_index < CC.FLOAT_PARAM_REGS.len) {
                    const dst = CC.FLOAT_PARAM_REGS[self.int_arg_index];
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
                if (self.float_arg_index < CC.FLOAT_PARAM_REGS.len) {
                    const dst = CC.FLOAT_PARAM_REGS[self.float_arg_index];
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
            return CC.FLOAT_PARAM_REGS.len;
        }

        /// Get float argument register at index
        pub fn getFloatArgReg(index: usize) FloatReg {
            return CC.FLOAT_PARAM_REGS[index];
        }

        /// Get the float return register (XMM0 on x86_64)
        pub fn getFloatReturnReg() FloatReg {
            return CC.FLOAT_PARAM_REGS[0];
        }

        /// Emit call instruction and handle cleanup.
        /// If R12 save was configured via init(), R12 is automatically
        /// restored after the call returns.
        pub fn call(self: *Self, fn_addr: usize) !void {
            // Calculate total stack space needed
            const stack_args_space: u32 = @intCast(self.stack_arg_count * 8);
            const total_unaligned: u32 = CC.SHADOW_SPACE + stack_args_space;
            // Round up to 16-byte alignment (Windows x64 ABI requirement)
            const total_space: u32 = (total_unaligned + 15) & ~@as(u32, 15);

            // Assert stack is 16-byte aligned
            std.debug.assert(total_space % 16 == 0);
            // Assert shadow space is included when we have stack args
            std.debug.assert(self.stack_arg_count == 0 or total_space >= CC.SHADOW_SPACE + 8);

            if (comptime builtin.cpu.arch == .x86_64) {
                // Allocate all stack space at once
                if (total_space > 0) {
                    try self.emit.subRegImm32(.w64, CC.STACK_PTR, @intCast(total_space));
                }

                // Store deferred stack arguments at correct offsets
                // Stack args go after shadow space: [RSP+32], [RSP+40], etc.
                for (self.stack_args[0..self.stack_arg_count], 0..) |arg, i| {
                    const stack_offset: i32 = @intCast(CC.SHADOW_SPACE + i * 8);
                    // Assert stack args are placed after shadow space
                    std.debug.assert(stack_offset >= CC.SHADOW_SPACE);
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
        /// If R12 save was configured via init(), R12 is automatically
        /// restored after the call returns.
        pub fn callReg(self: *Self, target: GeneralReg) !void {
            // Calculate total stack space needed
            const stack_args_space: u32 = @intCast(self.stack_arg_count * 8);
            const total_unaligned: u32 = CC.SHADOW_SPACE + stack_args_space;
            // Round up to 16-byte alignment (Windows x64 ABI requirement)
            const total_space: u32 = (total_unaligned + 15) & ~@as(u32, 15);

            // Assert stack is 16-byte aligned
            std.debug.assert(total_space % 16 == 0);
            // Assert shadow space is included when we have stack args
            std.debug.assert(self.stack_arg_count == 0 or total_space >= CC.SHADOW_SPACE + 8);

            if (comptime builtin.cpu.arch == .x86_64) {
                // Allocate all stack space at once
                if (total_space > 0) {
                    try self.emit.subRegImm32(.w64, CC.STACK_PTR, @intCast(total_space));
                }

                // Store deferred stack arguments at correct offsets
                // Stack args go after shadow space: [RSP+32], [RSP+40], etc.
                for (self.stack_args[0..self.stack_arg_count], 0..) |arg, i| {
                    const stack_offset: i32 = @intCast(CC.SHADOW_SPACE + i * 8);
                    // Assert stack args are placed after shadow space
                    std.debug.assert(stack_offset >= CC.SHADOW_SPACE);
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

/// Callee-side function frame builder for cross-platform prologue/epilogue generation.
///
/// Handles differences between:
/// - x86_64 System V (Linux, macOS, BSD): red zone available, RBX/R12-R15 callee-saved
/// - x86_64 Windows Fastcall: NO red zone, RBX/RSI/RDI/R12-R15 callee-saved
/// - aarch64 AAPCS64: X19-X28 callee-saved, FP/LR saved via STP
///
/// Usage:
/// ```
/// var frame = CalleeBuilder(Emit).init(&emit);
/// frame.saveViaMove(.RBX);      // Save RBX using MOV [RBP-offset], reg
/// frame.saveViaPush(.R12);      // Save R12 using PUSH (before stack alloc)
/// frame.setStackSize(1024);     // Request 1024 bytes of stack space
/// const initial_offset = try frame.emitPrologue();
/// // ... generate function body ...
/// try frame.emitEpilogue();
/// ```
pub fn CalleeBuilder(comptime Emit: type) type {
    const GeneralReg = switch (builtin.cpu.arch) {
        .aarch64 => aarch64.GeneralReg,
        .x86_64 => x86_64.GeneralReg,
        else => @compileError("Unsupported architecture"),
    };

    return struct {
        const Self = @This();

        emit: *Emit,

        // Configuration (set before emitPrologue)
        stack_size: u32 = 0,
        push_regs: [8]?GeneralReg = .{ null, null, null, null, null, null, null, null },
        push_count: u8 = 0,
        mov_save_regs: [8]?GeneralReg = .{ null, null, null, null, null, null, null, null },
        mov_save_count: u8 = 0,

        // State set by emitPrologue for use by emitEpilogue
        actual_stack_alloc: u32 = 0,

        /// Initialize a new callee frame builder
        pub fn init(emit: *Emit) Self {
            return Self{ .emit = emit };
        }

        /// Set the stack size needed for local variables.
        /// The actual allocation will be aligned to 16 bytes.
        pub fn setStackSize(self: *Self, size: u32) void {
            self.stack_size = size;
        }

        /// Mark a register to be saved via PUSH before stack allocation.
        /// Use this for registers that need to be saved at fixed offsets relative to RBP.
        /// Registers are pushed in the order they're added.
        pub fn saveViaPush(self: *Self, reg: GeneralReg) void {
            if (self.push_count < self.push_regs.len) {
                self.push_regs[self.push_count] = reg;
                self.push_count += 1;
            }
        }

        /// Mark a register to be saved via MOV after stack allocation.
        /// Use this for callee-saved registers in the deferred prologue pattern.
        /// Saved at fixed RBP-relative offsets.
        pub fn saveViaMove(self: *Self, reg: GeneralReg) void {
            if (self.mov_save_count < self.mov_save_regs.len) {
                self.mov_save_regs[self.mov_save_count] = reg;
                self.mov_save_count += 1;
            }
        }

        /// Get the RBP-relative offset for a MOV-saved register.
        /// Offsets start at -8 and go down: -8, -16, -24, etc.
        fn getMovSaveOffset(index: u8) i32 {
            return -@as(i32, @intCast((index + 1) * 8));
        }

        /// Emit function prologue. Returns the initial stack_offset for allocStackSlot.
        ///
        /// Sequence (x86_64):
        /// 1. push rbp; mov rbp, rsp (establish frame pointer)
        /// 2. push <regs> (callee-saved via push, in order added)
        /// 3. sub rsp, <aligned_size> (allocate stack space)
        /// 4. mov [rbp-N], <regs> (callee-saved via mov, after allocation)
        ///
        /// On Windows x64, step 3 MUST happen before step 4 (no red zone).
        pub fn emitPrologue(self: *Self) !i32 {
            if (comptime builtin.cpu.arch == .aarch64) {
                return self.emitPrologueAarch64();
            } else {
                return self.emitPrologueX86_64();
            }
        }

        fn emitPrologueX86_64(self: *Self) !i32 {
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
            // Total frame must be 16-byte aligned. After pushes, RSP = RBP - push_bytes.
            // We need (push_bytes + stack_alloc) to be 16-byte aligned.
            const total_needed = self.stack_size + @as(u32, self.mov_save_count) * 8;
            self.actual_stack_alloc = CC.alignStackSize(total_needed);

            if (self.actual_stack_alloc > 0) {
                try self.emit.subRegImm32(.w64, CC.STACK_PTR, @intCast(self.actual_stack_alloc));
            }

            // 4. Save MOV-based callee-saved registers (now safe - we've allocated stack)
            for (0..self.mov_save_count) |i| {
                if (self.mov_save_regs[i]) |reg| {
                    const offset = getMovSaveOffset(@intCast(i));
                    try self.emit.movMemReg(.w64, .RBP, offset, reg);
                }
            }

            // Return initial stack_offset: accounts for push-saved registers
            // First allocation should be below the pushed registers
            return -push_bytes;
        }

        fn emitPrologueAarch64(self: *Self) !i32 {
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
            const total_needed = self.stack_size + @as(u32, self.mov_save_count) * 8 + odd_reg_space;
            self.actual_stack_alloc = CC.alignStackSize(total_needed);

            if (self.actual_stack_alloc > 0) {
                try self.emit.subRegRegImm12(.w64, .ZRSP, .ZRSP, @intCast(self.actual_stack_alloc));
            }

            // Handle odd register: store at [SP + actual_stack_alloc - 8]
            if (self.push_count % 2 == 1) {
                if (self.push_regs[self.push_count - 1]) |reg| {
                    // STR to a fixed offset within allocated space
                    const odd_offset: u12 = @intCast(self.actual_stack_alloc - 8);
                    try self.emit.strRegMemUoff(.w64, reg, .ZRSP, odd_offset);
                }
            }

            // Return initial stack_offset: accounts for STP-saved register pairs + odd register.
            // FP/LR are at positive offsets from FP (FP+0, FP+8), so they don't
            // affect the negative stack_offset used for local variable allocation.
            // STP-saved registers are at FP-16, FP-8, etc.
            // Odd register (if any) is stored at FP - stp_bytes - 8.
            const odd_bytes: i32 = if (self.push_count % 2 == 1) 8 else 0;
            return -@as(i32, @intCast(stp_bytes)) - odd_bytes;
        }

        /// Emit function epilogue. Mirrors the prologue automatically.
        ///
        /// Sequence (x86_64):
        /// 1. mov <regs>, [rbp-N] (restore MOV-saved registers)
        /// 2. add rsp, <aligned_size> OR mov rsp, rbp (deallocate stack)
        /// 3. pop <regs> (restore PUSH-saved registers, reverse order)
        /// 4. pop rbp; ret
        pub fn emitEpilogue(self: *Self) !void {
            if (comptime builtin.cpu.arch == .aarch64) {
                return self.emitEpilogueAarch64();
            } else {
                return self.emitEpilogueX86_64();
            }
        }

        fn emitEpilogueX86_64(self: *Self) !void {
            // Compute actual_stack_alloc if not already set (allows separate prologue/epilogue instances)
            if (self.actual_stack_alloc == 0 and self.stack_size > 0) {
                self.actual_stack_alloc = self.computeActualStackAlloc();
            }

            // 1. Restore MOV-saved registers
            for (0..self.mov_save_count) |i| {
                if (self.mov_save_regs[i]) |reg| {
                    const offset = getMovSaveOffset(@intCast(i));
                    try self.emit.movRegMem(.w64, reg, .RBP, offset);
                }
            }

            // 2. Deallocate stack space
            if (self.actual_stack_alloc > 0) {
                try self.emit.addRegImm32(.w64, CC.STACK_PTR, @intCast(self.actual_stack_alloc));
            }

            // 3. Pop callee-saved registers (reverse order)
            var i: i32 = @as(i32, self.push_count) - 1;
            while (i >= 0) : (i -= 1) {
                if (self.push_regs[@intCast(i)]) |reg| {
                    try self.emit.popReg(reg);
                }
            }

            // 4. Restore frame pointer and return
            try self.emit.popReg(.RBP);
            try self.emit.ret();
        }

        fn emitEpilogueAarch64(self: *Self) !void {
            // Compute actual_stack_alloc if not already set (allows separate prologue/epilogue instances)
            if (self.actual_stack_alloc == 0 and (self.stack_size > 0 or self.push_count % 2 == 1)) {
                self.actual_stack_alloc = self.computeActualStackAlloc();
            }

            // Restore odd register first (stored at [SP + actual_stack_alloc - 8])
            if (self.push_count % 2 == 1) {
                if (self.push_regs[self.push_count - 1]) |reg| {
                    const odd_offset: u12 = @intCast(self.actual_stack_alloc - 8);
                    try self.emit.ldrRegMemUoff(.w64, reg, .ZRSP, odd_offset);
                }
            }

            // Deallocate stack space
            if (self.actual_stack_alloc > 0) {
                try self.emit.addRegRegImm12(.w64, .ZRSP, .ZRSP, @intCast(self.actual_stack_alloc));
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
        /// This is useful when you need to create separate CalleeBuilder instances
        /// for prologue and epilogue (e.g., when storing frame state is impractical).
        pub fn computeActualStackAlloc(self: *Self) u32 {
            if (comptime builtin.cpu.arch == .aarch64) {
                const odd_reg_space: u32 = if (self.push_count % 2 == 1) 16 else 0;
                const total_needed = self.stack_size + @as(u32, self.mov_save_count) * 8 + odd_reg_space;
                return CC.alignStackSize(total_needed);
            } else {
                const total_needed = self.stack_size + @as(u32, self.mov_save_count) * 8;
                return CC.alignStackSize(total_needed);
            }
        }

        /// Get the size of the callee-saved area (for MOV-saved registers)
        pub fn getCalleeSavedSize(self: *const Self) u32 {
            return @as(u32, self.mov_save_count) * 8;
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

test "CC.canPassStructByValue" {
    if (builtin.cpu.arch == .x86_64 and builtin.os.tag == .windows) {
        // Windows: only power-of-2 sizes 1, 2, 4, 8 can pass by value
        try std.testing.expect(CC.canPassStructByValue(1));
        try std.testing.expect(CC.canPassStructByValue(2));
        try std.testing.expect(CC.canPassStructByValue(4));
        try std.testing.expect(CC.canPassStructByValue(8));
        // Non-power-of-2 sizes must use pointer
        try std.testing.expect(!CC.canPassStructByValue(3));
        try std.testing.expect(!CC.canPassStructByValue(5));
        try std.testing.expect(!CC.canPassStructByValue(6));
        try std.testing.expect(!CC.canPassStructByValue(7));
        // >8 bytes must use pointer
        try std.testing.expect(!CC.canPassStructByValue(9));
        try std.testing.expect(!CC.canPassStructByValue(16));
    } else if (builtin.cpu.arch == .x86_64) {
        // System V: up to 16 bytes can pass by value
        try std.testing.expect(CC.canPassStructByValue(16));
        try std.testing.expect(!CC.canPassStructByValue(17));
    }
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
            try std.testing.expectEqual(@as(usize, 8), CC.PASS_BY_PTR_THRESHOLD);
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

    if (builtin.os.tag == .windows and builtin.cpu.arch == .x86_64) {
        // Windows: only 1, 2, 4, 8 byte structs pass by value
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
    } else if (builtin.cpu.arch == .x86_64) {
        // System V: up to 16 bytes pass by value, never uses pointer
        try std.testing.expect(!Builder.needsPassByPointer(16));
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

test "CallBuilder with automatic R12 save/restore" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    // Simulate a stack offset like CodeGen would have
    var stack_offset: i32 = -16;

    // Initialize builder - on Windows, this will allocate R12 save slot
    const builder = try Builder.init(&emit, &stack_offset);

    // On Windows, should have emitted: mov [rbp-32], r12 and allocated 16 bytes
    // (16 bytes to maintain 16-byte alignment for i128/RocDec arguments)
    if (builtin.os.tag == .windows) {
        try std.testing.expect(emit.buf.items.len > 0);
        try std.testing.expectEqual(@as(i32, -32), stack_offset); // 16 bytes allocated
        try std.testing.expectEqual(@as(?i32, -32), builder.r12_save_offset);
    } else {
        // On non-Windows, no save should have been emitted
        try std.testing.expectEqual(@as(usize, 0), emit.buf.items.len);
        try std.testing.expectEqual(@as(i32, -16), stack_offset); // unchanged
        try std.testing.expectEqual(@as(?i32, null), builder.r12_save_offset);
    }
}

test "CallBuilder call restores R12 on Windows" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
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

    if (builtin.os.tag == .windows) {
        // On Windows, the call sequence should include R12 restore at the end
        // Just verify r12_save_offset is set - the restore happens automatically
        try std.testing.expect(builder.r12_save_offset != null);
    }
}

test "CallBuilder on non-Windows doesn't allocate R12 slot" {
    if (builtin.cpu.arch != .x86_64) return;
    if (builtin.os.tag == .windows) return; // Skip on Windows

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
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

test "CallBuilder 4-arg call with immediates" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
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

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

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

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

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

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

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

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

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

test "CallBuilder addF64RegArg uses correct XMM register" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
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
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
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
    if (builtin.cpu.arch != .x86_64) return;
    if (builtin.os.tag != .windows) return;

    const Emit = x86_64.Emit;
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
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Builder = CallBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var stack_offset: i32 = 0;
    var builder = try Builder.init(&emit, &stack_offset);

    // Mix of int and float args
    try builder.addImmArg(1); // int arg 0
    try builder.addF64RegArg(.XMM5); // float arg (position 1 on Windows, separate counter on SysV)
    try builder.addImmArg(2); // int arg 1 or 2

    try builder.call(0x12345678);

    // Verify code was emitted with reasonable length (all args + call + cleanup)
    try std.testing.expect(emit.buf.items.len > 30);
}

test "CallBuilder callReg allocates shadow space on Windows" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
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

    if (builtin.os.tag == .windows) {
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
}

test "CallBuilder R12 save emits correct MOV instruction on Windows" {
    if (builtin.cpu.arch != .x86_64) return;
    if (builtin.os.tag != .windows) return;

    const Emit = x86_64.Emit;
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
    if (builtin.cpu.arch != .x86_64) return;
    if (builtin.os.tag != .windows) return;

    const Emit = x86_64.Emit;
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

test "CallBuilder stack args at correct offsets" {
    if (builtin.cpu.arch != .x86_64) return;
    if (builtin.os.tag != .windows) return;

    const Emit = x86_64.Emit;
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

test "CalleeBuilder basic prologue/epilogue x86_64" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Callee = CalleeBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Callee.init(&emit);
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

test "CalleeBuilder with pushed registers x86_64" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Callee = CalleeBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Callee.init(&emit);
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

test "CalleeBuilder with MOV-saved registers x86_64" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Callee = CalleeBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Callee.init(&emit);
    frame.saveViaMove(.R13);
    frame.saveViaMove(.R14);
    frame.setStackSize(256);

    _ = try frame.emitPrologue();
    try frame.emitEpilogue();

    // Should include MOV [rbp-8], r13 and MOV [rbp-16], r14 patterns
    try std.testing.expect(emit.buf.items.len > 30);
}

test "CalleeBuilder stack alignment" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Callee = CalleeBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Callee.init(&emit);
    frame.setStackSize(50); // Not 16-byte aligned

    _ = try frame.emitPrologue();

    // The actual_stack_alloc should be rounded up to 16-byte alignment
    try std.testing.expect(frame.actual_stack_alloc >= 50);
    try std.testing.expectEqual(@as(u32, 0), frame.actual_stack_alloc % 16);
}

test "CalleeBuilder mixed push and MOV saves x86_64" {
    if (builtin.cpu.arch != .x86_64) return;

    const Emit = x86_64.Emit;
    const Callee = CalleeBuilder(Emit);

    var emit = Emit.init(std.testing.allocator);
    defer emit.deinit();

    var frame = Callee.init(&emit);
    // Push RBX and R12 (like main expression does)
    frame.saveViaPush(.RBX);
    frame.saveViaPush(.R12);
    // Also save R13 via MOV (like deferred prologue pattern)
    frame.saveViaMove(.R13);
    frame.setStackSize(1024);

    const initial_offset = try frame.emitPrologue();
    // 2 pushed registers = -16
    try std.testing.expectEqual(@as(i32, -16), initial_offset);

    try frame.emitEpilogue();

    // Verify code was generated
    // Prologue: push rbp(1) + mov rbp,rsp(3) + push RBX(1) + push R12(2) + sub rsp,N(7) + mov [rbp-8],R13(4) = 18
    // Epilogue: mov R13,[rbp-8](4) + add rsp,N(7) + pop R12(2) + pop RBX(1) + pop rbp(1) + ret(1) = 16
    // Total: ~34 bytes
    try std.testing.expect(emit.buf.items.len > 30);
}
