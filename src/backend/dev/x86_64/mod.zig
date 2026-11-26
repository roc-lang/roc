//! x86_64 architecture support for the dev backend.
//!
//! This module provides x86_64-specific:
//! - Register definitions (GeneralReg, FloatReg)
//! - Instruction encoding (Emit)
//! - Calling conventions (SystemV, WindowsFastcall)

const std = @import("std");

pub const GeneralReg = @import("Registers.zig").GeneralReg;
pub const FloatReg = @import("Registers.zig").FloatReg;
pub const RegisterWidth = @import("Registers.zig").RegisterWidth;

/// ABI-parameterized Emit function. Use Emit(abi) to get a specialized type.
pub const Emit = @import("Emit.zig").Emit;

/// Emit type for System V x86_64 targets (Linux, macOS, the BSDs)
pub const LinuxEmit = Emit(.x86_64_sysv);
/// Emit type for Windows x86_64 targets
pub const WinEmit = Emit(.x86_64_windows);

pub const SystemV = @import("SystemV.zig");
pub const WindowsFastcall = @import("WindowsFastcall.zig");

/// ABI-parameterized CodeGen function. Use CodeGen(abi) to get a specialized type.
pub const CodeGen = @import("CodeGen.zig").CodeGen;

/// CodeGen type for System V x86_64 targets (Linux, macOS, the BSDs)
pub const LinuxCodeGen = CodeGen(.x86_64_sysv);
/// CodeGen type for Windows x86_64 targets
pub const WinCodeGen = CodeGen(.x86_64_windows);

test "x86_64 module imports" {
    std.testing.refAllDecls(@This());
}
