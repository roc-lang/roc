//! LLVM Backend Module
//!
//! This module provides the LLVM code generation backend for the Roc compiler.
//! It includes:
//! 1. Builder - LLVM IR builder for constructing LLVM modules
//! 2. IR types and data structures
//! 3. Bitcode reader/writer for serialization
//!
//! This backend generates LLVM IR that can be compiled to native code.

pub const Builder = @import("Builder.zig");
pub const ir = @import("ir.zig");
pub const bindings = @import("bindings.zig");
pub const bitcode_writer = @import("bitcode_writer.zig");
pub const BitcodeReader = @import("BitcodeReader.zig");
pub const layout_types = @import("layout_types.zig");

/// Mono IR to LLVM code generator (parallel to dev backend's MonoExprCodeGen)
pub const MonoLlvmCodeGen = @import("MonoLlvmCodeGen.zig").MonoLlvmCodeGen;

test "backend llvm tests" {
    const std = @import("std");
    std.testing.refAllDecls(Builder);
    std.testing.refAllDecls(ir);
    std.testing.refAllDecls(bindings);
    std.testing.refAllDecls(bitcode_writer);
    std.testing.refAllDecls(BitcodeReader);
    std.testing.refAllDecls(layout_types);
    std.testing.refAllDecls(@import("MonoLlvmCodeGen.zig"));
}
