//! LLVM Compilation Module
//!
//! This module provides LLVM compilation functionality that can be shared
//! between the CLI and eval contexts. It compiles LLVM bitcode to object
//! files using LLVM bindings, both for programs the CLI links and for
//! objects the compiler loads in-process.
//!
//! This module requires LLVM to be linked. Executables that use this module
//! must have `addStaticLlvmOptionsToModule` called on them in build.zig.

pub const bindings = @import("vendor_llvm_compile_bindings");
pub const compile = @import("compile.zig");

// Re-export commonly used functions for linking
pub const compileBitcodeToObject = bindings.compileBitcodeToObject;
pub const disposeMessage = bindings.disposeMessage;

// Re-export LIR to LLVM code generator (provided via "llvm_codegen" module dependency)
pub const MonoLlvmCodeGen = @import("llvm_codegen").MonoLlvmCodeGen;

// Re-export object compilation function
pub const compileToObject = compile.compileToObject;
pub const compileBitcodeModulesToObject = compile.compileBitcodeModulesToObject;
pub const CompileOptions = compile.CompileOptions;
pub const Error = compile.Error;

// Re-export i128 ABI helpers
pub const I128Arg = compile.I128Arg;
pub const normalizeI128Return = compile.normalizeI128Return;
pub const prepareI128Arg = compile.prepareI128Arg;
pub const normalizeU128Return = compile.normalizeU128Return;
pub const prepareU128Arg = compile.prepareU128Arg;
