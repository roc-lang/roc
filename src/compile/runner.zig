//! High-level helpers for running compiled Roc apps through the interpreter.
//!
//! NOTE: In the zig-16 branch, the CIR-level interpreter was replaced by the
//! LIR-level interpreter. This runner.zig provides the same external API as main
//! branch runner.zig, but the interpreter backend path returns error.NotSupported
//! until the interpreter shim is updated to use the LIR pipeline.

const std = @import("std");
const can = @import("can");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");

const builtins = @import("builtins");
const ModuleEnv = can.ModuleEnv;
const BuiltinModules = eval.BuiltinModules;
const RocOps = builtins.host_abi.RocOps;

/// Run a compiled Roc entrypoint expression through the interpreter.
///
/// NOTE: In zig-16 branch, the CIR-level interpreter is not yet available.
/// This function always returns error.NotSupported.
///
/// This API matches main branch runner.zig for compatibility with cli/main.zig.
pub fn runViaInterpreter(
    _: std.mem.Allocator,
    _: *ModuleEnv,
    _: *const BuiltinModules,
    _: []*ModuleEnv,
    _: ?*ModuleEnv,
    _: can.CIR.Expr.Idx,
    _: *RocOps,
    _: *anyopaque,
    _: *anyopaque,
    _: roc_target.RocTarget,
) !void {
    // TODO: Re-implement using LIR pipeline once the CIR→LIR pipeline
    // can be called incrementally from here.
    return error.NotSupported;
}

/// Run a compiled Roc program through the LIR interpreter pipeline.
///
/// This is the zig-16 style API using CheckedArtifact inputs.
/// NOTE: Not yet implemented.
pub fn runViaInterpreterLir(
    _: std.mem.Allocator,
    _: lir.CheckedPipeline.ArtifactSet,
    _: lir.CheckedPipeline.RootRequestSet,
    _: lir.CheckedPipeline.TargetConfig,
) !void {
    // TODO: Implement LIR-based interpreter runner.
    return error.NotSupported;
}
