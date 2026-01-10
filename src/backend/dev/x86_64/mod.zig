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
pub const Emit = @import("Emit.zig");
pub const SystemV = @import("SystemV.zig");
pub const WindowsFastcall = @import("WindowsFastcall.zig");
pub const CodeGen = @import("CodeGen.zig");

test "x86_64 module imports" {
    std.testing.refAllDecls(@This());
}
