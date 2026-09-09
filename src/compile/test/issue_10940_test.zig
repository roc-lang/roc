//! Regression tests for issue #10940.
//!
//! A default app header may expose `main!` without defining it. Checking
//! reports the missing definition, while explicit-root compilation does not
//! require the app entrypoint. Neither mode treats it as a compiler invariant.

const std = @import("std");
const roc_target = @import("roc_target");

const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

test "issue 10940: a default app that never defines main! reports it as exposed but not defined" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] {}
        \\
        ,
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    try build_env.build(main_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var found_exposed_but_not_defined = false;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Exposed But Not Defined")) {
                found_exposed_but_not_defined = true;
            }
        }
    }
    try std.testing.expect(found_exposed_but_not_defined);
}

test "issue 10940: explicit roots do not require a default app entrypoint" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] {}
        \\
        ,
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    build_env.setRootValidation(.explicit_roots);

    try build_env.build(main_path);

    const artifact = build_env.executableRootCheckedArtifact();
    for (artifact.root_requests.runtime_requests) |root| {
        try std.testing.expect(root.kind != .runtime_entrypoint);
    }

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            try std.testing.expect(!std.mem.eql(u8, report.title, "Exposed But Not Defined"));
        }
    }
}

test "issue 10940: a headerless default app still publishes main!" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "main.roc",
        .data =
        \\main! = |_args| {}
        \\
        ,
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    try build_env.build(main_path);

    const artifact = build_env.executableRootCheckedArtifact();
    var found_runtime_entrypoint = false;
    for (artifact.root_requests.runtime_requests) |root| {
        if (root.kind == .runtime_entrypoint) {
            found_runtime_entrypoint = true;
        }
    }
    try std.testing.expect(found_runtime_entrypoint);
}

test "issue 10940: explicit roots do not automatically publish a defined main!" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "main.roc",
        .data =
        \\main! = |_args| {}
        \\
        ,
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const main_path = try tmp_dir.dir.realPathFileAlloc(io, "main.roc", gpa);
    defer gpa.free(main_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    build_env.setRootValidation(.explicit_roots);

    try build_env.build(main_path);

    const artifact = build_env.executableRootCheckedArtifact();
    for (artifact.root_requests.runtime_requests) |root| {
        try std.testing.expect(root.kind != .runtime_entrypoint);
    }
}
