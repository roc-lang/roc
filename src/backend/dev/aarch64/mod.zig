//! aarch64 (ARM64) architecture support for the dev backend.
//!
//! This module provides aarch64-specific:
//! - Register definitions (GeneralReg, FloatReg)
//! - Instruction encoding (Emit)
//! - Calling convention (Call - AAPCS64)

const std = @import("std");
const builtin = @import("builtin");
const RocTarget = @import("roc_target").RocTarget;

pub const GeneralReg = @import("Registers.zig").GeneralReg;
pub const FloatReg = @import("Registers.zig").FloatReg;
pub const RegisterWidth = @import("Registers.zig").RegisterWidth;

/// Target-parameterized Emit function. Use Emit(target) to get a specialized type.
pub const Emit = @import("Emit.zig").Emit;

/// Convenience: native Emit for host compilation (uses arm64linux as fallback for non-aarch64 hosts)
pub const NativeEmit = if (builtin.cpu.arch == .aarch64 or builtin.cpu.arch == .aarch64_be)
    Emit(RocTarget.detectNative())
else
    Emit(.arm64linux);

pub const Call = @import("Call.zig");
pub const CodeGen = @import("CodeGen.zig");

test "aarch64 module imports" {
    std.testing.refAllDecls(@This());
}
