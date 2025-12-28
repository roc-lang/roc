//! LLVM Compilation Module
//!
//! This module provides LLVM compilation and linking functionality that can be
//! shared between the CLI and eval contexts. It handles:
//! 1. Compiling LLVM bitcode to object files using LLVM bindings
//! 2. Linking object files to executables using LLD
//! 3. Executing compiled programs and capturing output
//!
//! This module requires LLVM to be linked. Executables that use this module
//! must have `addStaticLlvmOptionsToModule` called on them in build.zig.

pub const bindings = @import("bindings.zig");
pub const compile = @import("compile.zig");

// Re-export commonly used functions
pub const compileBitcodeToObject = bindings.compileBitcodeToObject;
pub const disposeMessage = bindings.disposeMessage;
pub const LinkMachO = bindings.LinkMachO;
pub const LinkELF = bindings.LinkELF;
pub const LinkCOFF = bindings.LinkCOFF;
pub const LinkWasm = bindings.LinkWasm;

pub const compileAndExecute = compile.compileAndExecute;
pub const Error = compile.Error;
pub const ResultType = compile.ResultType;
