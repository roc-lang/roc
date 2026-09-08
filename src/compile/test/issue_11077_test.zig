//! Regression test for platform-required function return diagnostics.

const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const BuildEnv = compile_build.BuildEnv;

const DiagnosticTestError = compile_build.InitError ||
    compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    std.Io.Writer.Error ||
    error{ TestUnexpectedResult, TestExpectedEqual };

test "issue 11077: platform return mismatch describes the return value" {
    // https://github.com/roc-lang/roc/issues/11077
    // Checking must identify the displayed types as return types of main!.
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "app.roc",
        .data =
        \\app [main!] { pf: platform "./platform.roc" }
        \\
        \\import pf.Echo
        \\
        \\main! = |_| {
        \\    Echo.line!("hello world\n")
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "platform.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try(_, [Exit(I8), ..]) }
        \\    exposes [Echo]
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\    hosted { "roc_echo_line": Echo.line! }
        \\
        \\import Echo
        \\
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args| match main!(args) {
        \\    Ok(_) => 0
        \\    Err(Exit(code)) => code
        \\    Err(_) => 1
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(io, .{
        .sub_path = "Echo.roc",
        .data =
        \\Echo := [].{
        \\    line! : Str => {}
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

    var report_count: usize = 0;
    for (drained) |module_reports| report_count += module_reports.reports.len;
    try std.testing.expectEqual(@as(usize, 1), report_count);

    for (drained) |module_reports| {
        for (module_reports.reports) |*report| {
            try std.testing.expectEqualStrings("Type Mismatch", report.title);
            var rendered: std.Io.Writer.Allocating = .init(gpa);
            defer rendered.deinit();
            try report.render(&rendered.writer, .markdown);
            const message = rendered.written();
            errdefer std.debug.print("{s}\n", .{message});
            try std.testing.expect(std.mem.find(u8, message, "Try(") != null);
            try std.testing.expect(std.mem.find(u8, message, "Exit(I8)") != null);
            try std.testing.expect(std.mem.find(u8, message, "    {}") != null);
            try std.testing.expect(std.mem.find(u8, message, "The requirement is declared here:") != null);
            try std.testing.expect(std.mem.find(u8, message, "The platform requires  main!  to return") != null);
        }
    }
}

test "issue 11077: platform argument mismatch is not described as a return mismatch" {
    try expectPlatformDiagnostic(
        "main! = |{}| Ok({})",
        "List(Str) => Try(_, [Exit(I8), ..])",
        &.{ "The platform requires  main!  to have a specific type.", "List(Str)" },
    );
}

test "issue 11077: platform whole-value mismatch is not described as a return mismatch" {
    try expectPlatformDiagnostic(
        "main! = {}",
        "List(Str) => Try(_, [Exit(I8), ..])",
        &.{ "The platform requires  main!  to have a specific type.", "List(Str) => Try(" },
    );
}

test "issue 11077: platform function arity mismatch retains the whole function type" {
    try expectPlatformDiagnostic(
        "main! = |_, _| Ok({})",
        "List(Str) => Try(_, [Exit(I8), ..])",
        &.{ "The platform requires  main!  to have a specific type.", "List(Str) => Try(" },
    );
}

test "issue 11077: platform return mismatch preserves exact record compatibility" {
    try expectPlatformDiagnostic(
        "main! = |_| { x: 1, y: 2 }",
        "{} -> { x: U64 }",
        &.{ "The platform requires  main!  to return a specific type.", "This function currently returns:", "But the platform requires it to return:" },
    );
}

test "issue 11077: matching platform return type checks without diagnostics" {
    try expectPlatformDiagnostic(
        "main! = |_| Ok({})",
        "List(Str) => Try(_, [Exit(I8), ..])",
        &.{},
    );
}

fn expectPlatformDiagnostic(app_body: []const u8, required_type: []const u8, expected_fragments: []const []const u8) DiagnosticTestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const app_source = try std.fmt.allocPrint(gpa,
        \\app [main!] {{ pf: platform "./platform.roc" }}
        \\
        \\{s}
        \\
    , .{app_body});
    defer gpa.free(app_source);
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app.roc", .data = app_source });

    const platform_source = try std.fmt.allocPrint(gpa,
        \\platform ""
        \\    requires {{}} {{ main! : {s} }}
        \\    exposes []
        \\    packages {{}}
        \\    provides {{ "roc_main": entry }}
        \\
        \\entry : {{}} -> {{}}
        \\entry = |_| {{}}
        \\
    , .{required_type});
    defer gpa.free(platform_source);
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "platform.roc", .data = platform_source });

    const cwd = try tmp_dir.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app.roc", gpa);
    defer gpa.free(app_path);

    var build_env = try BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    try build_env.build(app_path);

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    var report_count: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |*report| {
            report_count += 1;
            try std.testing.expectEqualStrings("Type Mismatch", report.title);
            var rendered: std.Io.Writer.Allocating = .init(gpa);
            defer rendered.deinit();
            try report.render(&rendered.writer, .markdown);
            const message = rendered.written();
            errdefer std.debug.print("{s}\n", .{message});
            for (expected_fragments) |fragment| {
                try std.testing.expect(std.mem.find(u8, message, fragment) != null);
            }
            try std.testing.expect(std.mem.find(u8, message, "The requirement is declared here:") != null);
        }
    }
    try std.testing.expectEqual(@as(usize, if (expected_fragments.len == 0) 0 else 1), report_count);
}
