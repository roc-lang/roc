//! Code generation backends for the Roc compiler.
//!
//! This module provides various code generation backends:
//! - LLVM: Full-featured backend that generates native code via LLVM
//! - Dev: Fast development backends that generate native code directly (x86_64, aarch64)
//! - Wasm: WebAssembly backend that generates wasm bytecode

const StructuralTest = @import("structural_test.zig");

pub const dev = @import("dev/mod.zig");
pub const wasm = @import("wasm/mod.zig");

// Re-export dev backend types at top level.
pub const x86_64 = dev.x86_64;
pub const aarch64 = dev.aarch64;
pub const object = dev.object;
pub const Relocation = dev.Relocation;
pub const applyRelocations = dev.applyRelocations;
pub const applyRelocationsWithContext = dev.applyRelocationsWithContext;
pub const SymbolResolver = dev.SymbolResolver;
pub const SymbolResolverContext = dev.SymbolResolverContext;
pub const CodeGen = dev.CodeGen;
pub const Backend = dev.Backend;
pub const ExecutableMemory = dev.ExecutableMemory;
pub const HostLirCodeGen = dev.HostLirCodeGen;
pub const host_lir_codegen_available = dev.host_lir_codegen_available;
pub const LirCodeGenMod = dev.LirCodeGenMod;
pub const DevBackend = dev.DevBackend;
pub const Storage = dev.Storage;
pub const X86_64LinuxBackend = dev.X86_64LinuxBackend;
pub const X86_64MacBackend = dev.X86_64MacBackend;
pub const X86_64WinBackend = dev.X86_64WinBackend;
pub const AArch64Backend = dev.AArch64Backend;
pub const Entrypoint = dev.Entrypoint;
pub const StaticDataExport = dev.StaticDataExport;
pub const StaticDataRelocation = dev.StaticDataRelocation;
pub const StaticStringData = dev.StaticStringData;
pub const RunImage = dev.RunImage;
pub const procSymbolName = dev.procSymbolName;
pub const ObjectFileCompiler = dev.ObjectFileCompiler;
pub const CompilationResult = dev.CompilationResult;
pub const CompilationError = dev.CompilationError;
pub const writeFileWindowsAvSafe = dev.writeFileWindowsAvSafe;
pub const resolveBuiltinFunction = dev.resolveBuiltinFunction;

test "backend tests" {
    const std = @import("std");
    std.testing.refAllDecls(StructuralTest);
    std.testing.refAllDecls(dev);
    std.testing.refAllDecls(wasm);
}
