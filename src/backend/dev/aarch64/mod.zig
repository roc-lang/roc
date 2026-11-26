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

/// ABI-parameterized Emit function. Use Emit(abi) to get a specialized type.
pub const Emit = @import("Emit.zig").Emit;

/// Emit type for AAPCS64 targets (Linux, the BSDs, freestanding ELF)
pub const LinuxEmit = Emit(.aarch64_aapcs);
/// Emit type for Windows aarch64 targets
pub const WinEmit = Emit(.aarch64_windows);
/// Emit type for Apple aarch64 targets
pub const MacEmit = Emit(.aarch64_apple);

pub const Call = @import("Call.zig");

/// ABI-parameterized CodeGen function. Use CodeGen(abi) to get a specialized type.
pub const CodeGen = @import("CodeGen.zig").CodeGen;

/// CodeGen type for AAPCS64 targets (Linux, the BSDs, freestanding ELF)
pub const LinuxCodeGen = CodeGen(.aarch64_aapcs);
/// CodeGen type for Windows aarch64 targets
pub const WinCodeGen = CodeGen(.aarch64_windows);
/// CodeGen type for Apple aarch64 targets
pub const MacCodeGen = CodeGen(.aarch64_apple);

test "aarch64 module imports" {
    std.testing.refAllDecls(@This());
}
