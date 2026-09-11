//! Regression test for issue #11286.

const std = @import("std");
const base = @import("base");
const harness = @import("lower_to_lir_harness.zig");

// repro for https://github.com/roc-lang/roc/issues/11286
//
// `Echo.line!` is hosted at the closed error row `[EchoErr(Str)]`, and `?`
// widens that row at the use site into `main!`'s open row (design.md "Hosted
// Try Question Widening"). Boxy lowers the hosted worker at the widened row
// while the host boundary keeps the declared closed row, so the worker return
// and the hosted call result are two values governed by different descriptors
// even though they share a storage layout. Lowering must keep them apart.
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
    \\    hosted { "roc_echo_line": Echo.line! }
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
    \\    line! : Str => Try(Str, [EchoErr(Str)])
    \\}
;

test "issue 11286: a widened hosted Try result keeps its own boxy descriptor" {
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
