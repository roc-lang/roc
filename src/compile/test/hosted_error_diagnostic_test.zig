//! Host provenance supplements, rather than replaces, closed-row diagnostics.
//!
//! A wrapper defined as a function whose body forwards a host's closed error
//! row closes the open row its annotation states, and a caller's call widens
//! that row (design.md "Row Subsumption"), so such wrappers are accepted. An
//! alias of a hosted function is not a function definition and keeps the
//! host's fixed row, so its mismatch still names the host function.
const std = @import("std");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");

const DiagnosticTestError = compile_build.InitError ||
    compile_build.BuildRootError ||
    std.Io.Dir.WriteFileError ||
    std.Io.Dir.RealPathFileAllocError ||
    std.Io.Writer.Error ||
    error{ TestUnexpectedResult, TestExpectedEqual };

test "host error diagnostic follows imported function aliases" {
    try checkHostErrorDiagnostic("line! = Host.stdout_line!", .hosted);
}

test "a wrapper returning a host result unchanged is widened at its caller's call" {
    try checkHostErrorDiagnostic("line! = |s| Host.stdout_line!(s)", .accepted);
}

test "a question forwarding wrapper is widened at its caller's call" {
    try checkHostErrorDiagnostic(
        \\line! = |s| {
        \\    Stdout.alias!(s)?
        \\    Ok({})
        \\}
        \\alias! = Host.stdout_line!
    , .accepted);
}

test "a wrapper forwarding a direct hosted question is widened at its caller's call" {
    try checkHostErrorDiagnostic(
        \\line! = |s| {
        \\    Host.stdout_line!(s)?
        \\    Ok({})
        \\}
    , .accepted);
}

test "a wrapper forwarding a saved host result is widened at its caller's call" {
    try checkHostErrorDiagnostic(
        \\line! = |s| {
        \\    result = Host.stdout_line!(s)
        \\    result?
        \\    Ok({})
        \\}
    , .accepted);
}

test "a wrapper forwarding an unrelated closed result is widened at its caller's call" {
    try checkHostErrorDiagnostic(
        \\line! = |s| {
        \\    hosted_result = Host.stdout_line!(s)
        \\    _ = hosted_result
        \\    close : Try({}, [StdoutErr(Str)]) -> Try({}, [StdoutErr(Str)])
        \\    close = |value| value
        \\    result = close(Err(StdoutErr("local")))
        \\    result?
        \\    Ok({})
        \\}
    , .accepted);
}

test "host error diagnostic stops at explicit reconstruction" {
    try checkHostErrorDiagnostic(
        \\line! = |s| match Host.stdout_line!(s) {
        \\    Ok(value) => Ok(value)
        \\    Err(StdoutErr(message)) => Err(StdoutErr(message))
        \\}
    , .accepted);
}

test "a wrapper returning an unrelated closed result is widened at its caller's call" {
    try checkHostErrorDiagnostic(
        \\line! = |s| {
        \\    close : Try({}, [StdoutErr(Str)]) -> Try({}, [StdoutErr(Str)])
        \\    close = |value| value
        \\    _ = Host.stdout_line!(s)
        \\    close(Err(StdoutErr("local")))
        \\}
    , .accepted);
}

const Expected = enum { hosted, ordinary, accepted };

fn checkHostErrorDiagnostic(wrapper: []const u8, expected: Expected) DiagnosticTestError!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const files = .{
        .{
            "app.roc",
            \\app [main!] { pf: platform "./platform.roc" }
            \\import pf.Stdout
            \\main! : List(Str) => Try({}, [Exit(I32), StdoutErr(Str)])
            \\main! = |_args| {
            \\    Stdout.line!("Hello,World!")?
            \\    Ok({})
            \\}
        },
        .{
            "platform.roc",
            \\platform ""
            \\    requires {} { main! : List(Str) => Try({}, [Exit(I32), ..]) }
            \\    exposes [Stdout]
            \\    packages {}
            \\    provides { "roc_main": main_for_host! }
            \\    hosted { "roc_stdout_line": Host.stdout_line! }
            \\import Host
            \\import Stdout
            \\main_for_host! : List(Str) => I32
            \\main_for_host! = |args| match main!(args) {
            \\    Ok({}) => 0
            \\    Err(Exit(code)) => code
            \\    Err(_) => 1
            \\}
        },
        .{
            "Host.roc",
            \\Host := [].{
            \\    stdout_line! : Str => Try({}, [StdoutErr(Str)])
            \\}
        },
    };
    inline for (files) |file| try tmp.dir.writeFile(io, .{ .sub_path = file[0], .data = file[1] });
    const stdout_source = try std.fmt.allocPrint(gpa,
        \\import Host
        \\Stdout := [].{{
        \\    line! : Str => Try({{}}, [StdoutErr(Str)])
        \\    {s}
        \\}}
    , .{wrapper});
    defer gpa.free(stdout_source);
    try tmp.dir.writeFile(io, .{ .sub_path = "Stdout.roc", .data = stdout_source });

    const cwd = try tmp.dir.realPathFileAlloc(io, ".", gpa);
    defer gpa.free(cwd);
    const app_path = try tmp.dir.realPathFileAlloc(io, "app.roc", gpa);
    defer gpa.free(app_path);
    var build_env = try compile_build.BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io);
    defer build_env.deinit();
    try build_env.build(app_path);
    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);
    var count: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |*report| {
            count += 1;
            var rendered: std.Io.Writer.Allocating = .init(gpa);
            defer rendered.deinit();
            try report.render(&rendered.writer, .markdown);
            const message = rendered.written();
            errdefer std.debug.print("{s}\n", .{message});
            try std.testing.expectEqualStrings("Type Mismatch", report.title);
            try std.testing.expectEqual(expected == .hosted, std.mem.find(u8, message, "This error is forwarded from host function") != null);
            if (expected == .hosted) {
                try std.testing.expect(std.mem.find(u8, message, "Host.stdout_line!") != null);
                try std.testing.expect(std.mem.find(u8, message, "fixed representation at the host boundary") != null);
            }
            try std.testing.expect(std.mem.find(u8, message, "This error payload is a closed tag union.") != null);
            try std.testing.expect(std.mem.find(u8, message, "Forwarding the unchanged error payload does not convert it.") != null);
        }
    }
    try std.testing.expectEqual(@as(usize, if (expected == .accepted) 0 else 1), count);
}
