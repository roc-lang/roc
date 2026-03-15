//! aarch64 (ARM64) architecture support for the dev backend.
//!
//! This module provides aarch64-specific:
//! - Register definitions (GeneralReg, FloatReg)
//! - Instruction encoding (Emit)
//! - Calling convention (Call - AAPCS64)

const std = @import("std");

pub const GeneralReg = @import("Registers.zig").GeneralReg;
pub const FloatReg = @import("Registers.zig").FloatReg;
pub const RegisterWidth = @import("Registers.zig").RegisterWidth;

/// Target-parameterized Emit function. Use Emit(target) to get a specialized type.
pub const Emit = @import("Emit.zig").Emit;

/// Emit type for Linux aarch64 target
pub const LinuxEmit = Emit(.arm64linux);
/// Emit type for Windows aarch64 target
pub const WinEmit = Emit(.arm64win);
/// Emit type for macOS aarch64 target
pub const MacEmit = Emit(.arm64mac);

pub const Call = @import("Call.zig");

/// Target-parameterized CodeGen function. Use CodeGen(target) to get a specialized type.
pub const CodeGen = @import("CodeGen.zig").CodeGen;

/// CodeGen type for Linux aarch64 target
pub const LinuxCodeGen = CodeGen(.arm64linux);
/// CodeGen type for Windows aarch64 target
pub const WinCodeGen = CodeGen(.arm64win);
/// CodeGen type for macOS aarch64 target
pub const MacCodeGen = CodeGen(.arm64mac);

test "aarch64 module imports" {
    std.testing.refAllDecls(@This());
}
