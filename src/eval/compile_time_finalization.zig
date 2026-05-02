//! Compile-time evaluation finalization for checked artifacts.
//!
//! This module owns the post-check work that cannot live in `check` because it
//! must run the MIR-family pipeline, ARC insertion, and the LIR interpreter.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const checked_artifact = check.CheckedArtifact;

pub fn finalizer() checked_artifact.CompileTimeFinalizer {
    return .{ .finalize = finalize };
}

fn finalize(
    _: ?*anyopaque,
    allocator: Allocator,
    artifact: *checked_artifact.CheckedModuleArtifact,
    imports: []const checked_artifact.PublishImportArtifact,
) anyerror!void {
    const compile_time_roots = try compileTimeRootRequests(allocator, artifact.root_requests.requests);
    defer if (compile_time_roots.len > 0) allocator.free(compile_time_roots);

    if (compile_time_roots.len == 0) {
        try artifact.comptime_values.sealBindings();
        return;
    }

    const import_views = try allocator.alloc(checked_artifact.ImportedModuleView, imports.len);
    defer allocator.free(import_views);
    for (imports, 0..) |import, i| {
        import_views[i] = import.view;
    }

    var lowered = try lir.CheckedPipeline.lowerArtifactsToLir(
        allocator,
        .{
            .root = checked_artifact.loweringView(artifact),
            .imports = import_views,
        },
        .{
            .requests = compile_time_roots,
            .purpose = .compile_time,
        },
        .{
            .target_usize = base.target.TargetUsize.native,
            .artifact_state = .checking_finalization,
        },
    );
    defer lowered.deinit();

    _ = lowered;
    compileTimeFinalizationInvariant("compile-time LIR evaluation and value reification are not sealed yet");
}

fn compileTimeRootRequests(
    allocator: Allocator,
    roots: []const checked_artifact.RootRequest,
) Allocator.Error![]checked_artifact.RootRequest {
    var out = std.ArrayList(checked_artifact.RootRequest).empty;
    errdefer out.deinit(allocator);

    for (roots) |root| {
        if (root.abi != .compile_time) continue;
        try out.append(allocator, root);
    }

    return try out.toOwnedSlice(allocator);
}

fn compileTimeFinalizationInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("compile-time finalization invariant violated: " ++ message, .{});
    }
    unreachable;
}

test "compile-time finalization tests" {
    std.testing.refAllDecls(@This());
}
