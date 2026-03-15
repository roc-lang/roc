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

/// Target-parameterized Emit function. Use Emit(target) to get a specialized type.
pub const Emit = @import("Emit.zig").Emit;

/// Emit type for Linux x86_64 target
pub const LinuxEmit = Emit(.x64linux);
/// Emit type for Windows x86_64 target
pub const WinEmit = Emit(.x64win);
/// Emit type for macOS x86_64 target
pub const MacEmit = Emit(.x64mac);

pub const SystemV = @import("SystemV.zig");
pub const WindowsFastcall = @import("WindowsFastcall.zig");

/// Target-parameterized CodeGen function. Use CodeGen(target) to get a specialized type.
pub const CodeGen = @import("CodeGen.zig").CodeGen;

/// CodeGen type for Linux x86_64 target
pub const LinuxCodeGen = CodeGen(.x64linux);
/// CodeGen type for Windows x86_64 target
pub const WinCodeGen = CodeGen(.x64win);
/// CodeGen type for macOS x86_64 target
pub const MacCodeGen = CodeGen(.x64mac);

test "x86_64 module imports" {
    std.testing.refAllDecls(@This());
}
