//! Regression test for issue #12226: a lambda `??` default on a record type
//! declared in another module must not disturb lowering of an unrelated lambda
//! the app passes in a record.
//! repro for https://github.com/roc-lang/roc/issues/12226

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "issue 12226: an imported record type with a lambda default lowers alongside an app lambda passed in a record" {
    var dir = std.testing.tmpDir(.{});
    defer dir.cleanup();

    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : Str => Str }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": run! }
        \\run! = |input| main!(input)
        ,
    });
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "Wrap.roc",
        .data =
        \\Wrap := [].{
        \\    Config := {
        \\        filter : (Str -> Str) ?? |text| text,
        \\    }
        \\
        \\    init : Config -> Str
        \\    init = |_config| "ok"
        \\}
        ,
    });
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./platform.roc" }
        \\
        \\import Wrap
        \\
        \\call_sleep = |hooks| {
        \\    sleep = hooks.sleep
        \\    sleep(1)
        \\}
        \\
        \\main! = |input| {
        \\    _name = Wrap.init({})
        \\    _slept = call_sleep({ sleep: |_| {} })
        \\    input
        \\}
        ,
    });

    const path = try dir.dir.realPathFileAlloc(std.testing.io, "main.roc", std.testing.allocator);
    defer std.testing.allocator.free(path);
    try harness.expectAppPathLowersToLir(path);
}
