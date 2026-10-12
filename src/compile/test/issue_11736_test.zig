//! Regression for a rejected platform value whose nominal backing holds a tag alias.

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const RequirementTestError = compile_build.InitError || compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError || std.Io.Dir.RealPathFileAllocError || std.Io.Dir.CreateDirPathError ||
    error{ TestExpectedEqual, TestUnexpectedResult };

test "issue 11736: mistyped requirement containing a tag alias reports a mismatch" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app.roc",
        .data =
        \\app [main] { pf: platform "platform.roc" }
        \\
        \\main = 42
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "platform.roc",
        .data =
        \\platform ""
        \\    requires { main : P }
        \\    exposes [P]
        \\    packages {}
        \\    provides { "roc_main": main_for_host }
        \\
        \\import P
        \\
        \\main_for_host : Str
        \\main_for_host = P.name(main)
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "P.roc",
        .data =
        \\P := { step : Step }.{
        \\    Step : [Run(Str)]
        \\
        \\    name : P -> Str
        \\    name = |_| "p"
        \\}
        ,
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app.roc", gpa);
    defer gpa.free(app_path);
    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    try build_env.build(app_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);
    var mismatches: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |report| {
            if (std.mem.eql(u8, report.title, "Type Mismatch")) mismatches += 1;
        }
    }
    try std.testing.expectEqual(@as(usize, 1), mismatches);
}

test "issue 11736: rejected string requirement propagates through a block with caching" {
    try expectBlockRequirement("\"wrong\"", 1, true);
}

test "issue 11736: valid alias-containing requirement remains evaluable with caching" {
    try expectBlockRequirement("P.{ step: Run(\"ok\") }", 0, true);
}

test "issue 11736: cached runtime body retains rejected requirement divergence" {
    try expectBlockRequirement("\"wrong\"", 1, false);
}

fn expectBlockRequirement(value: []const u8, expected_mismatches: usize, compile_time: bool) RequirementTestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const app_source = try std.fmt.allocPrint(gpa,
        \\app [main] {{ pf: platform "platform.roc" }}
        \\import pf.P
        \\main = {s}
    , .{value});
    defer gpa.free(app_source);
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app.roc", .data = app_source });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "platform.roc",
        .data = if (compile_time)
            \\platform ""
            \\    requires { main : P }
            \\    exposes [P]
            \\    packages {}
            \\    provides { "roc_main": main_for_host }
            \\
            \\import P
            \\
            \\main_for_host : Str
            \\main_for_host = {
            \\    text = P.name(main)
            \\    text
            \\}
        else
            \\platform ""
            \\    requires { main : P }
            \\    exposes [P]
            \\    packages {}
            \\    provides { "roc_main": main_for_host }
            \\
            \\import P
            \\
            \\main_for_host : {} -> Str
            \\main_for_host = |_| {
            \\    text = P.name(main)
            \\    text
            \\}
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "P.roc",
        .data =
        \\P := { step : Step }.{
        \\    Step : [Run(Str)]
        \\
        \\    name : P -> Str
        \\    name = |_| "p"
        \\}
        ,
    });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app.roc", gpa);
    defer gpa.free(app_path);
    try tmp_dir.dir.createDirPath(io, "cache");
    const cache_path = try tmp_dir.dir.realPathFileAlloc(io, "cache", gpa);
    defer gpa.free(cache_path);
    for (0..2) |iteration| {
        var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
        defer build_env.deinit();
        const manager = try gpa.create(@import("../cache_manager.zig").CacheManager);
        manager.* = @import("../cache_manager.zig").CacheManager.init(gpa, .{
            .enabled = true,
            .cache_dir = cache_path,
        }, build_env.filesystem);
        build_env.setCacheManager(manager);
        try build_env.build(app_path);
        const drained = try build_env.drainReports();
        defer build_env.freeDrainedReports(drained);
        errdefer {
            for (drained) |module_reports| {
                for (module_reports.reports) |*report| {
                    var rendered: std.Io.Writer.Allocating = .init(gpa);
                    defer rendered.deinit();
                    report.render(&rendered.writer, .markdown) catch {};
                    std.debug.print("{s}\n", .{rendered.written()});
                }
            }
        }
        if (iteration == 1) try std.testing.expect(manager.getStats().hits > 0);
        const coord = build_env.coordinator.?;
        // Reaching the rejected requirement reports nothing beyond the
        // checker's own diagnostic, so every pairing is cacheable.
        try std.testing.expectEqual(@as(u32, if (iteration == 0) 1 else 0), coord.platform_pairing_count);
        const platform = coord.executableRootCheckedArtifact();
        for (platform.resolved_value_refs.records) |ref| {
            if (ref.ref != .platform_required_checked_error) continue;
            const expr = platform.checked_bodies.stored_exprs.items[@backingInt(ref.expr)];
            try std.testing.expect(expr.data == .runtime_error);
            try std.testing.expect(expr.diverges and expr.diverges_without_inline_expects);
            try std.testing.expect(!expr.evaluation_may_be_elided_for_inspect);
        }
        var divergent_statements: usize = 0;
        for (platform.checked_bodies.stored_statements.items) |statement| {
            if (statement.diverges) {
                try std.testing.expect(statement.diverges_without_inline_expects);
                divergent_statements += 1;
            }
        }
        try std.testing.expectEqual(expected_mismatches != 0, divergent_statements != 0);

        var mismatches: usize = 0;
        for (drained) |module_reports| {
            if (expected_mismatches == 0) try std.testing.expectEqual(@as(usize, 0), module_reports.reports.len);
            for (module_reports.reports) |report| {
                if (std.mem.eql(u8, report.title, "Type Mismatch")) {
                    mismatches += 1;
                } else {
                    try std.testing.expectEqual(.warning, report.severity);
                }
            }
        }
        try std.testing.expectEqual(expected_mismatches, mismatches);
    }
}
