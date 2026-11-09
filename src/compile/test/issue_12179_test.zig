//! Regression coverage for checking an app file that has a syntax error and
//! sits next to a `main.roc` (#12179).
//!
//! The checked file is its own root, so its own parse errors must be reported
//! and the neighboring `main.roc` must not be checked in its place.
//!
//! A root whose header fails to parse has no module path to report under, so
//! each report is attributed by its rendered source snippet: only `main.roc`
//! contains `unclosed in main`, and only `other.roc` contains `hello world`.
const std = @import("std");
const roc_target = @import("roc_target");
const reporting = @import("reporting");
const CoreCtx = @import("ctx").CoreCtx;
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;
const Mode = @import("../compile_package.zig").Mode;

const broken_main_source =
    \\main! = |_args| {
    \\    x = "unclosed in main
    \\    Ok({})
    \\}
    \\
;

const broken_other_source =
    \\main! = |_args| {
    \\    echo!("hello world
    \\    Ok({})
    \\}
    \\
;

test "issue 12179: checking an app with a syntax error reports its own errors, not a sibling main.roc's" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(io, .{ .sub_path = "main.roc", .data = broken_main_source });
    try tmp.dir.writeFile(io, .{ .sub_path = "other.roc", .data = broken_other_source });

    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const other_path = try tmp.dir.realPathFileAlloc(io, "other.roc", gpa);
    defer gpa.free(other_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    build_env.buildResolvingMain(other_path, null) catch {};

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var other_errors: usize = 0;
    var main_errors: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |*report| {
            if (!report.severity.isError()) continue;

            var rendered: std.Io.Writer.Allocating = .init(gpa);
            defer rendered.deinit();
            try reporting.renderReportToPlain(report, &rendered.writer, reporting.ReportingConfig.initForTesting());
            const text = rendered.written();

            if (std.mem.find(u8, text, "unclosed in main") != null) {
                main_errors += 1;
                std.debug.print("unexpected error from the sibling main.roc: {s}\n", .{report.title});
            } else if (std.mem.find(u8, text, "hello world") != null) {
                other_errors += 1;
            }
        }
    }

    try std.testing.expectEqual(@as(usize, 0), main_errors);
    try std.testing.expect(other_errors > 0);
}

/// Counts how many times one file is read, passing every read through.
const ReadCounter = struct {
    base: CoreCtx,
    paths: []const []const u8,
    counts: []usize,

    fn io(self: *ReadCounter) CoreCtx {
        var counted = self.base;
        counted.ctx = @ptrCast(self);
        var vtable = self.base.vtable;
        vtable.readFile = &readFile;
        counted.vtable = vtable;
        return counted;
    }

    fn readFile(ctx: ?*anyopaque, std_io: std.Io, path: []const u8, allocator: std.mem.Allocator) CoreCtx.ReadError![]u8 {
        const self: *ReadCounter = @ptrCast(@alignCast(ctx.?));
        for (self.paths, self.counts) |counted_path, *count| {
            if (std.mem.eql(u8, path, counted_path)) count.* += 1;
        }
        return self.base.vtable.readFile(self.base.ctx, std_io, path, allocator);
    }
};

const ReportCounts = struct {
    titled: usize = 0,
    containing: usize = 0,
};

/// Count error reports titled `title`, and error reports whose rendered text
/// contains `text`.
fn countErrorReports(build_env: *BuildEnv, title: []const u8, text: []const u8) (std.mem.Allocator.Error || error{WriteFailed})!ReportCounts {
    const gpa = std.testing.allocator;
    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var counts: ReportCounts = .{};
    for (drained) |module_reports| {
        for (module_reports.reports) |*report| {
            if (!report.severity.isError()) continue;
            if (std.mem.eql(u8, report.title, title)) counts.titled += 1;

            var rendered: std.Io.Writer.Allocating = .init(gpa);
            defer rendered.deinit();
            try reporting.renderReportToPlain(report, &rendered.writer, reporting.ReportingConfig.initForTesting());
            if (std.mem.find(u8, rendered.written(), text) != null) counts.containing += 1;
        }
    }
    return counts;
}

test "issue 12179: an app root with a syntax error is still checked past parsing" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(io, .{ .sub_path = "other.roc", .data =
        \\main! = |_args| {
        \\    echo!(missing_name)
        \\    Ok({})
        \\}
        \\
        \\broken = "unclosed in other
        \\
    });

    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const other_path = try tmp.dir.realPathFileAlloc(io, "other.roc", gpa);
    defer gpa.free(other_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    build_env.buildResolvingMain(other_path, null) catch {};

    const counts = try countErrorReports(&build_env, "Name Not In Scope", "unclosed in other");
    try std.testing.expectEqual(@as(usize, 1), counts.titled);
    try std.testing.expect(counts.containing > 0);
}

test "issue 12179: a module is checked when its owning main.roc has a syntax error" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(io, .{ .sub_path = "main.roc", .data = broken_main_source });
    try tmp.dir.writeFile(io, .{ .sub_path = "Child.roc", .data =
        \\module [value]
        \\
        \\value = missing_name
        \\
    });

    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const child_path = try tmp.dir.realPathFileAlloc(io, "Child.roc", gpa);
    defer gpa.free(child_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();

    build_env.buildResolvingMain(child_path, null) catch {};

    const counts = try countErrorReports(&build_env, "Name Not In Scope", "unclosed in main");
    try std.testing.expectEqual(@as(usize, 1), counts.titled);
    try std.testing.expect(counts.containing > 0);
}

test "issue 12179: checking reads each file discovery parses once" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.createDirPath(io, "app");
    try tmp.dir.writeFile(io, .{ .sub_path = "app/fixed.roc", .data =
        \\main! = |_args| {
        \\    echo!("hello world")
        \\    Ok({})
        \\}
        \\
    });
    try tmp.dir.createDirPath(io, "owned");
    try tmp.dir.writeFile(io, .{ .sub_path = "owned/main.roc", .data =
        \\main! = |_args| {
        \\    echo!("hello world")
        \\    Ok({})
        \\}
        \\
    });
    try tmp.dir.writeFile(io, .{ .sub_path = "owned/Child.roc", .data =
        \\module [value]
        \\
        \\value = 1
        \\
    });

    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const app_path = try tmp.dir.realPathFileAlloc(io, "app/fixed.roc", gpa);
    defer gpa.free(app_path);
    const main_path = try tmp.dir.realPathFileAlloc(io, "owned/main.roc", gpa);
    defer gpa.free(main_path);
    const child_path = try tmp.dir.realPathFileAlloc(io, "owned/Child.roc", gpa);
    defer gpa.free(child_path);

    for ([_]Mode{ .single_threaded, .multi_threaded }) |mode| {
        // An app is its own root.
        {
            var build_env = try BuildEnv.init(gpa, mode, 2, roc_target.RocTarget.detectNative(), cwd, io);
            defer build_env.deinit();
            var counts = [_]usize{0};
            var counter = ReadCounter{ .base = build_env.filesystem, .paths = &.{app_path}, .counts = &counts };
            build_env.filesystem = counter.io();

            try build_env.buildResolvingMain(app_path, null);
            try std.testing.expectEqual(@as(usize, 1), counts[0]);
        }

        // A module is an entry below the main.roc that roots its build.
        {
            var build_env = try BuildEnv.init(gpa, mode, 2, roc_target.RocTarget.detectNative(), cwd, io);
            defer build_env.deinit();
            var counts = [_]usize{ 0, 0 };
            var counter = ReadCounter{ .base = build_env.filesystem, .paths = &.{ main_path, child_path }, .counts = &counts };
            build_env.filesystem = counter.io();

            try build_env.buildResolvingMain(child_path, null);
            try std.testing.expectEqual(@as(usize, 1), counts[0]);
            try std.testing.expectEqual(@as(usize, 1), counts[1]);
        }
    }
}
