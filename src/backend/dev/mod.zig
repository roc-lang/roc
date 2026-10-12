//! Development backends for the Roc compiler.
//!
//! These backends generate native machine code directly without LLVM,
//! enabling fast compilation for development workflows.
//!
//! Supported architectures:
//! - x86_64: Linux (System V ABI), macOS (System V ABI), Windows (Fastcall)
//! - aarch64: Linux and macOS (AAPCS64)

/// Exact procedure-local stack lifetime and slot planning.
pub const StackPlan = @import("StackPlan.zig");

const std = @import("std");
const builtin = @import("builtin");

pub const x86_64 = @import("x86_64/mod.zig");
pub const aarch64 = @import("aarch64/mod.zig");
pub const object = @import("object/mod.zig");
const relocation_mod = @import("Relocation.zig");
pub const Relocation = relocation_mod.Relocation;
pub const applyRelocations = relocation_mod.applyRelocations;
pub const applyRelocationsWithContext = relocation_mod.applyRelocationsWithContext;
pub const SymbolResolver = relocation_mod.SymbolResolver;
pub const SymbolResolverContext = relocation_mod.SymbolResolverContext;
pub const ValueStorage = @import("ValueStorage.zig");
pub const ObjectWriter = @import("ObjectWriter.zig");
pub const Dwarf = @import("Dwarf.zig");

/// Executable memory for running generated code. Uses OS-specific APIs not available on freestanding.
pub const ExecutableMemory = if (builtin.os.tag == .freestanding)
    void
else
    @import("ExecutableMemory.zig").ExecutableMemory;

// LirCodeGen - LIR-based code generator parameterized by RocTarget
pub const LirCodeGenMod = @import("LirCodeGen.zig");

/// Pre-instantiated LirCodeGen for the host platform (the machine running the compiler)
pub const HostLirCodeGen = LirCodeGenMod.HostLirCodeGen;

/// Whether the direct dev backend can generate code for the host architecture.
pub const host_lir_codegen_available = LirCodeGenMod.host_lir_codegen_available;

/// Object file compiler for generating object files from Mono IR.
/// Supports cross-compilation to any RocTarget.
/// Only available on non-freestanding targets (uses std.fs)
pub const ObjectFileCompiler = if (builtin.os.tag == .freestanding) void else @import("ObjectFileCompiler.zig").ObjectFileCompiler;
/// Per-region machine-code artifacts and their reassembly.
pub const ProcArtifact = @import("ProcArtifact.zig");
/// Shared native procedure task driver and same-program retained artifacts.
pub const NativeProcCompiler = @import("NativeProcCompiler.zig");
/// An artifact located in a loaded pack.
pub const LocatedArtifact = if (builtin.os.tag == .freestanding) void else @import("ObjectFileCompiler.zig").LocatedArtifact;
/// Where the object compiler splices object-cache procedures from.
pub const SpliceSource = if (builtin.os.tag == .freestanding) void else @import("ObjectFileCompiler.zig").SpliceSource;
/// Place object-cache entries into an open code generator.
pub const spliceExternalProcs = if (builtin.os.tag == .freestanding) void else @import("ObjectFileCompiler.zig").spliceExternalProcs;
/// Links object-cache entries spliced into the compile-time evaluator's image.
pub const HostSplice = if (builtin.os.tag == .freestanding) void else @import("HostSplice.zig").HostSplice;
/// On-disk form of one module's pack of artifacts.
pub const PackFile = @import("PackFile.zig");
pub const Entrypoint = if (builtin.os.tag == .freestanding) void else @import("ObjectFileCompiler.zig").Entrypoint;
pub const StaticDataExport = @import("StaticDataExport.zig").StaticDataExport;
pub const StaticDataRelocation = @import("StaticDataExport.zig").StaticDataRelocation;
pub const StaticDataImage = @import("StaticDataImage.zig").StaticDataImage;
pub const StaticDataImageFunctionResolver = @import("StaticDataImage.zig").FunctionResolver;
pub const StaticStringData = @import("StaticStringData.zig");
pub const RunImage = @import("RunImage.zig");
pub const procSymbolName = @import("StaticDataExport.zig").procSymbolName;
pub const atomicRcHelperSymbolName = @import("StaticDataExport.zig").atomicRcHelperSymbolName;
pub const collectRequiredRcHelpers = @import("StaticDataExport.zig").collectRequiredRcHelpers;
pub const collectReferencedProcs = @import("StaticDataExport.zig").collectReferencedProcs;
pub const CompilationResult = if (builtin.os.tag == .freestanding) void else @import("ObjectFileCompiler.zig").CompilationResult;
pub const CompilationError = if (builtin.os.tag == .freestanding) void else @import("ObjectFileCompiler.zig").CompilationError;
pub const writeFileWindowsAvSafe = @import("ObjectFileCompiler.zig").writeFileWindowsAvSafe;

test "backend module imports" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(@import("CallingConvention.zig"));
    std.testing.refAllDecls(@import("FrameBuilder.zig"));
}
