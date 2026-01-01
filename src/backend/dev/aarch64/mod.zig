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
pub const Emit = @import("Emit.zig");
pub const Call = @import("Call.zig");
pub const CodeGen = @import("CodeGen.zig");

test "aarch64 module imports" {
    std.testing.refAllDecls(@This());
}
