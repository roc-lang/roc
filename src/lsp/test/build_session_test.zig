//! Build-session allocation failures must not produce completed check results.
const std = @import("std");
const compile = @import("compile");
const lsp = @import("lsp");
const roc_target = @import("roc_target");
const integration_spec = @import("integration_spec.zig");
const test_env = @import("integration_env.zig");

/// Allocation-failure and reportable-source-failure build-session specs.
pub const specs = [_]integration_spec.Spec{
    .{ .name = "build session: input observer allocation failure aborts initialization", .run = observerAllocationFailure },
    .{ .name = "build session: missing dependency remains a reportable failed check", .run = missingDependency },
};

const Failure = struct {
    base: compile.CoreCtx,
    allocator: *std.testing.FailingAllocator,
    armed: bool = false,

    fn readFile(ctx: ?*anyopaque, _: std.Io, path: []const u8, allocator: std.mem.Allocator) compile.CoreCtx.ReadError![]u8 {
        const self: *@This() = @ptrCast(@alignCast(ctx.?));
        if (std.mem.endsWith(u8, path, "missing.roc")) {
            // loadLocalImpl immediately observes this explicit missing input.
            // Fail its path duplication, not a synthetic observer callback.
            self.allocator.fail_index = self.allocator.alloc_index;
            self.armed = true;
            return error.FileNotFound;
        }
        return self.base.readFile(path, allocator);
    }
};

fn observerAllocationFailure() integration_spec.SpecError!void {
    try runMissingDependency(true);
}

fn missingDependency() integration_spec.SpecError!void {
    try runMissingDependency(false);
}

fn runMissingDependency(fail_observer: bool) integration_spec.SpecError!void {
    const allocator = test_env.allocator;
    var tmp = test_env.tmpDir(.{});
    defer tmp.cleanup();
    const disk_source = "package [] {}\n";
    const buffer_source = "package [] { dep: \"./missing.roc\" }\n";
    try tmp.dir.writeFile(test_env.io, .{ .sub_path = "main.roc", .data = disk_source });
    const cwd = try tmp.dir.realPathFileAlloc(test_env.io, ".", allocator);
    defer allocator.free(cwd);
    const path = try std.fs.path.join(allocator, &.{ cwd, "main.roc" });
    defer allocator.free(path);
    const uri = try lsp.uri.pathToUri(allocator, path);
    defer allocator.free(uri);

    var failing = std.testing.FailingAllocator.init(allocator, .{});
    defer failing.fail_index = std.math.maxInt(usize);
    var env = try compile.BuildEnv.init(failing.allocator(), .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, test_env.io);
    defer env.deinit();
    var failure = Failure{ .base = env.filesystem, .allocator = &failing };
    if (fail_observer) {
        env.filesystem.ctx = &failure;
        env.filesystem.vtable.readFile = Failure.readFile;
    }
    const saved_io = env.filesystem;
    if (fail_observer) {
        if (lsp.build_session.BuildSession.init(allocator, test_env.io, &env, uri, buffer_source, null)) |value| {
            var unexpected_session = value;
            unexpected_session.deinit();
            return error.TestUnexpectedResult;
        } else |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
        }
        try std.testing.expect(failure.armed);
        try std.testing.expect(failing.has_induced_failure);
        // No BuildSession was returned to let a caller install the incomplete
        // input set as completed coverage. The observer did not append its input.
        try std.testing.expectEqual(@as(usize, 0), env.resolution_inputs.items.len);
        failing.fail_index = std.math.maxInt(usize);
    } else {
        var session = try lsp.build_session.BuildSession.init(allocator, test_env.io, &env, uri, buffer_source, null);
        defer session.deinit();
        try std.testing.expect(!session.build_succeeded);
        try std.testing.expect(session.drained_reports.?.len > 0);
        const inputs = try env.collectWatchInputStates();
        defer env.freeWatchInputStates(inputs);
        var found_missing = false;
        for (inputs) |input| {
            if (std.mem.endsWith(u8, input.path, "missing.roc")) {
                try std.testing.expect(std.meta.eql(input.state, compile.watch_inputs.State.missing));
                found_missing = true;
            }
        }
        try std.testing.expect(found_missing);
    }
    try std.testing.expectEqual(saved_io.ctx, env.filesystem.ctx);
    try std.testing.expectEqual(saved_io.vtable.readFile, env.filesystem.vtable.readFile);
    const restored = env.filesystem.readFile(path, allocator) catch return error.TestUnexpectedResult;
    defer allocator.free(restored);
    try std.testing.expectEqualStrings(disk_source, restored);
}
