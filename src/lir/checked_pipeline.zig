//! Public checked-artifact-to-LIR lowering API.
//!
//! This is the only public semantic lowering entrance after type checking. It
//! accepts already-published checked artifacts, explicit root requests, and
//! target configuration. It returns lowered LIR or resource failure only.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const mir = @import("mir");
const ir = @import("ir");

const Arc = @import("arc.zig");
const LowerIr = @import("lower_ir.zig");
const LIR = @import("LIR.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const checked_artifact = check.CheckedArtifact;

pub const LowerResourceError = Allocator.Error;

pub const ArtifactSet = struct {
    root: checked_artifact.LoweringModuleView,
    imports: []const checked_artifact.ImportedModuleView = &.{},
};

pub const RootRequestSet = struct {
    requests: []const checked_artifact.RootRequest,
};

pub const TargetConfig = struct {
    module_envs: []const *const ModuleEnv,
    builtin_str_ident: ?base.Ident.Idx = null,
    target_usize: base.target.TargetUsize = base.target.TargetUsize.native,
};

pub const LoweredProgram = struct {
    lir_result: LowerIr.Result,
    main_proc: LIR.LirProcSpecId,
    target_usize: base.target.TargetUsize,

    pub fn deinit(self: *LoweredProgram) void {
        self.lir_result.deinit();
    }
};

pub fn lowerArtifactsToLir(
    allocator: Allocator,
    artifacts: ArtifactSet,
    roots: RootRequestSet,
    target: TargetConfig,
) LowerResourceError!LoweredProgram {
    artifacts.root.artifact.verifyPublished();

    var mono = try mir.Mono.Specialize.run(allocator, .{
        .root = artifacts.root,
        .imports = artifacts.imports,
    }, roots.requests);
    errdefer mono.deinit();

    var row_finalized = try mir.MonoRow.run(allocator, mono);
    errdefer row_finalized.deinit();

    var lifted = try mir.Lifted.Lift.run(allocator, row_finalized);
    errdefer lifted.deinit();

    var solved = try mir.LambdaSolved.Solve.run(allocator, lifted);
    errdefer solved.deinit();

    var executable = try mir.Executable.Build.run(allocator, solved);
    errdefer executable.deinit();

    var lowered_ir = try ir.Lower.fromExecutable(allocator, executable);
    errdefer lowered_ir.deinit();

    const executable_roots = lowered_ir.root_procs.items;
    const executable_root_metadata = lowered_ir.root_metadata.items;

    var lowered_lir = try LowerIr.run(
        allocator,
        target.module_envs,
        target.builtin_str_ident,
        target.target_usize,
        lowered_ir,
        executable_roots,
        executable_root_metadata,
    );
    errdefer lowered_lir.deinit();

    try Arc.insert(&lowered_lir.store);

    if (lowered_lir.root_procs.items.len == 0) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked pipeline invariant violated: explicit root set produced no LIR roots", .{});
        }
        unreachable;
    }

    return .{
        .lir_result = lowered_lir,
        .main_proc = lowered_lir.root_procs.items[0],
        .target_usize = target.target_usize,
    };
}

test "checked pipeline public API returns resource errors only" {
    std.testing.refAllDecls(@This());
}
