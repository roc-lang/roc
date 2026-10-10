//! Regression test for issue #11286.

const std = @import("std");
const base = @import("base");
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

// Generic method uses have their own output polarity. Selecting a hosted
// implementation must use the general result-row adapter while preserving
// the inner host ABI, even though direct hosted question widening is removed.
test "hosted generic dispatch adapts a widened method result at the declared host ABI" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    try tmp_dir.dir.writeFile(io, .{ .sub_path = "app.roc", .data =
        \\app [main!] { pf: platform "./platform.roc" }
        \\import pf.Job
        \\describe! : a => Try({}, [Extra, HostErr(Str)]) where [a.status! : a => Try({}, [HostErr(Str)])]
        \\describe! = |x| x.status!()
        \\main! : List(Str) => Try({}, [Exit(I32)])
        \\main! = |_args| {
        \\    _ = describe!(Job.Pending)
        \\    Ok({})
        \\}
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "platform.roc", .data =
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
    });
    try tmp_dir.dir.writeFile(io, .{ .sub_path = "Job.roc", .data =
        \\Job := [Pending].{
        \\    status! : Job => Try({}, [HostErr(Str)])
        \\}
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(io, "app.roc", gpa);
    defer gpa.free(app_path);
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        try harness.expectAppPathLowersToLirWithOptions(app_path, .{
            .specialization_strategy = strategy,
        });
    }
}
