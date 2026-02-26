//! Code generation backends for the Roc compiler.
//!
//! This module provides various code generation backends:
//! - LLVM: Full-featured backend that generates native code via LLVM
//! - Dev: Fast development backends that generate native code directly (x86_64, aarch64)
//! - Wasm: WebAssembly backend that generates wasm bytecode

pub const dev = @import("dev/mod.zig");
pub const wasm = @import("wasm/mod.zig");

// Re-export dev backend types at top level for backwards compatibility.
// Existing code uses `backend.ExecutableMemory`, `backend.MonoExprCodeGen`, etc.
pub const EvalBackend = dev.EvalBackend;
pub const x86_64 = dev.x86_64;
pub const aarch64 = dev.aarch64;
pub const object = dev.object;
pub const Relocation = dev.Relocation;
pub const applyRelocations = dev.applyRelocations;
pub const SymbolResolver = dev.SymbolResolver;
pub const CodeGen = dev.CodeGen;
pub const Backend = dev.Backend;
pub const ExecutableMemory = dev.ExecutableMemory;
pub const StaticDataInterner = dev.StaticDataInterner;
pub const MonoExprCodeGen = dev.MonoExprCodeGen;
pub const NativeMonoExprCodeGen = dev.NativeMonoExprCodeGen;
pub const HostMonoExprCodeGen = dev.HostMonoExprCodeGen;
pub const DevBackend = dev.DevBackend;
pub const Storage = dev.Storage;
pub const X86_64LinuxBackend = dev.X86_64LinuxBackend;
pub const X86_64MacBackend = dev.X86_64MacBackend;
pub const X86_64WinBackend = dev.X86_64WinBackend;
pub const AArch64Backend = dev.AArch64Backend;
pub const Entrypoint = dev.Entrypoint;
pub const ObjectFileCompiler = dev.ObjectFileCompiler;
pub const CompilationResult = dev.CompilationResult;
pub const resolveBuiltinFunction = dev.resolveBuiltinFunction;

test "backend tests" {
    const std = @import("std");
    std.testing.refAllDecls(dev);
    std.testing.refAllDecls(wasm);
}
