//! Regression test for issue #11286.

const std = @import("std");
const base = @import("base");
const roc_target = @import("roc_target");
const compile_build = @import("../compile_build.zig");
const harness = @import("lower_to_lir_harness.zig");

// repro for https://github.com/roc-lang/roc/issues/11286
//
// The host keeps its closed ABI. A Roc wrapper explicitly reconstructs the
// error before main! combines it with Exit. Both specialization strategies
// must retain the separate value descriptors across that boundary.
const app_source =
    \\app [main!] { pf: platform "./platform.roc" }
    \\
    \\import pf.Echo
    \\
    \\main! : List(Str) => Try({}, [Exit(I8), EchoErr(Str), ..])
    \\main! = |_args| {
    \\    greeting = Echo.line!("hello")?
    \\    if Str.is_empty(greeting) Err(Exit(1)) else Ok({})
    \\}
;

const platform_source =
    \\platform ""
    \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    \\    exposes [Echo]
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\    hosted { "roc_echo_line": Echo.line_internal! }
    \\
    \\import Echo
    \\
    \\main_for_host! : List(Str) => I8
    \\main_for_host! = |args| match main!(args) {
    \\    Ok({}) => 0
    \\    Err(Exit(code)) => code
    \\    Err(_) => 1
    \\}
;

const echo_source =
    \\Echo := [].{
    \\    line_internal! : Str => Try(Str, [EchoErr(Str)])
    \\    line! : Str => Try(Str, [EchoErr(Str)])
    \\    line! = |message| match Echo.line_internal!(message) {
    \\        Ok(value) => Ok(value)
    \\        Err(EchoErr(error)) => Err(EchoErr(error))
    \\    }
    \\}
;

test "issue 11286: an explicitly reconstructed hosted Try result keeps its own descriptor" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app.roc", .data = app_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "platform.roc", .data = platform_source });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Echo.roc", .data = echo_source });

    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app.roc", gpa);
    defer gpa.free(app_path);

    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectAppPathLowersToLirWithOptions(app_path, .{
            .specialization_strategy = strategy,
        });
    }
}

const job_platform_source =
    \\platform ""
    \\    requires {} { main! : List(Str) => Try({}, [Exit(I32), ..]) }
    \\    exposes [Job]
    \\    packages {}
    \\    provides { "roc_main": main_for_host! }
    \\    hosted { "job_status": Job.status! }
    \\import Job
    \\main_for_host! : List(Str) => I32
    \\main_for_host! = |args| match main!(args) {
    \\    Ok({}) => 0
    \\    Err(Exit(code)) => code
    \\    Err(_) => 1
    \\}
;

const job_source =
    \\Job := [Pending].{
    \\    status! : Job => Try({}, [HostErr(Str)])
    \\    checked! : Job => Try({}, [HostErr(Str)])
    \\    checked! = |job| Job.status!(job).map_err(|HostErr(e)| HostErr(e))
    \\}
;

fn setupFailed(err: anytype) error{TestUnexpectedResult} {
    std.debug.print("building the test app failed: {s}\n", .{@errorName(err)});
    return error.TestUnexpectedResult;
}

/// Write the `Job` platform beside `app_source` and return the app's path.
fn writeJobApp(gpa: std.mem.Allocator, io: std.Io, dir: std.Io.Dir, app: []const u8) error{TestUnexpectedResult}![:0]u8 {
    dir.writeFile(io, .{ .sub_path = "app.roc", .data = app }) catch |err| return setupFailed(err);
    dir.writeFile(io, .{ .sub_path = "platform.roc", .data = job_platform_source }) catch |err| return setupFailed(err);
    dir.writeFile(io, .{ .sub_path = "Job.roc", .data = job_source }) catch |err| return setupFailed(err);
    return dir.realPathFileAlloc(io, "app.roc", gpa) catch |err| setupFailed(err);
}

/// Build the `Job` app `app` and expect exactly the reports titled `titles`,
/// in order, the first of which contains every string of `contains`.
fn expectJobAppReports(app: []const u8, titles: []const []const u8, contains: []const []const u8) error{ TestUnexpectedResult, TestExpectedEqual }!void {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const app_path = try writeJobApp(gpa, io, tmp_dir.dir, app);
    defer gpa.free(app_path);
    const cwd = tmp_dir.dir.realPathFileAlloc(io, ".", gpa) catch |err| return setupFailed(err);
    defer gpa.free(cwd);
    var build_env = compile_build.BuildEnv.init(gpa, .single_threaded, 1, roc_target.RocTarget.detectNative(), cwd, io, .lss) catch |err| return setupFailed(err);
    defer build_env.deinit();
    build_env.build(app_path) catch |err| return setupFailed(err);
    const drained = build_env.drainReports() catch |err| return setupFailed(err);
    defer build_env.freeDrainedReports(drained);
    var count: usize = 0;
    for (drained) |module_reports| {
        for (module_reports.reports) |*report| {
            var rendered: std.Io.Writer.Allocating = .init(gpa);
            defer rendered.deinit();
            report.render(&rendered.writer, .markdown) catch |err| return setupFailed(err);
            const message = rendered.written();
            errdefer std.debug.print("{s}\n", .{message});
            try std.testing.expect(count < titles.len);
            try std.testing.expectEqualStrings(titles[count], report.title);
            if (count == 0) {
                for (contains) |expected| try std.testing.expect(std.mem.find(u8, message, expected) != null);
            }
            count += 1;
        }
    }
    try std.testing.expectEqual(titles.len, count);
}

test "a where-method's Try result used at a wider error row without `?` is a mismatch in the body" {
    // The requirement's error row is generated as written, and `Try` is an
    // ordinary nominal type, so `x.checked!()` does not fit a result that
    // lists another error.
    try expectJobAppReports(
        \\app [main!] { pf: platform "./platform.roc" }
        \\import pf.Job
        \\describe! : a => Try({}, [Extra, HostErr(Str)]) where [a.checked! : a => Try({}, [HostErr(Str)])]
        \\describe! = |x| x.checked!()
        \\main! : List(Str) => Try({}, [Exit(I32)])
        \\main! = |_args| {
        \\    _ = describe!(Job.Pending)
        \\    Ok({})
        \\}
    , &.{"Type Mismatch"}, &.{"Try({}, [HostErr(Str)])"});
}

test "hosted generic dispatch at a widened method result is rejected at the call" {
    // `describe!` composes the requirement's error row into
    // `[Extra, HostErr(Str)]` with `?`, and `Job.status!`'s row is the host's,
    // which never widens (design.md "Row Joins"), so selecting the hosted
    // declaration for the requirement is a type error at the call that names
    // it.
    try expectJobAppReports(
        \\app [main!] { pf: platform "./platform.roc" }
        \\import pf.Job
        \\describe! : a => Try({}, [Extra, HostErr(Str)]) where [a.status! : a => Try({}, [HostErr(Str)])]
        \\describe! = |x| Ok(x.status!()?)
        \\main! : List(Str) => Try({}, [Exit(I32)])
        \\main! = |_args| {
        \\    _ = describe!(Job.Pending)
        \\    Ok({})
        \\}
    , &.{"Hosted Function In Where Clause"}, &.{ "status!", "rebuilds each tag" });
}

test "generic dispatch to a Roc method that rebuilds a hosted error composes through `?`" {
    // `Job.checked!` rebuilds the host's error, so its row is its own and the
    // requiring body's `?` composes it into the wider result under both
    // specialization strategies.
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const app_path = try writeJobApp(gpa, io, tmp_dir.dir,
        \\app [main!] { pf: platform "./platform.roc" }
        \\import pf.Job
        \\describe! : a => Try({}, [Extra, HostErr(Str)]) where [a.checked! : a => Try({}, [HostErr(Str)])]
        \\describe! = |x| Ok(x.checked!()?)
        \\main! : List(Str) => Try({}, [Exit(I32)])
        \\main! = |_args| {
        \\    _ = describe!(Job.Pending)
        \\    Ok({})
        \\}
    );
    defer gpa.free(app_path);
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectAppPathLowersToLirWithOptions(app_path, .{
            .specialization_strategy = strategy,
        });
    }
}
