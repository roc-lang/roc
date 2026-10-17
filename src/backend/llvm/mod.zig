//! LLVM Backend Module
//!
//! This module provides the LLVM code generation backend for the Roc compiler.
//! It includes:
//! 1. Builder - LLVM IR builder for constructing LLVM modules
//! 2. IR types and data structures
//! 3. Bitcode writer for serialization
//!
//! This backend generates LLVM IR that can be compiled to native code.

pub const Builder = @import("vendor_llvm_ir").Builder;
pub const ir = @import("vendor_llvm_ir").ir;
pub const bitcode_writer = @import("vendor_llvm_ir").bitcode_writer;

/// Mono IR to LLVM code generator (parallel to dev backend's MonoExprCodeGen)
pub const MonoLlvmCodeGen = @import("MonoLlvmCodeGen.zig").MonoLlvmCodeGen;

test "backend llvm tests" {
    const std = @import("std");
    std.testing.refAllDecls(Builder);
    std.testing.refAllDecls(ir);
    std.testing.refAllDecls(bitcode_writer);
    std.testing.refAllDecls(@import("MonoLlvmCodeGen.zig"));
    std.testing.refAllDecls(@import("debugger_constants.zig"));
}
