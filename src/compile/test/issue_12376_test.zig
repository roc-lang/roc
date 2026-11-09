//! Regression tests for issue #12376: a derived JSON parser for a tag union
//! whose payloads are an imported opaque type and a second imported opaque
//! type wrapping the first, or whose payloads include an empty record, must
//! lower to LIR.
//! repro for https://github.com/roc-lang/roc/issues/12376

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "issue 12376: derived parser for a tag union with direct and nested imported opaque payloads lowers to LIR" {
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
        .sub_path = "Inner.roc",
        .data =
        \\Inner :: { id : U64 }.{
        \\    parser_for : _
        \\}
        ,
    });
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "Outer.roc",
        .data =
        \\import Inner
        \\Outer :: { inner : Inner }.{
        \\    parser_for : _
        \\}
        ,
    });
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./platform.roc" }
        \\
        \\import Inner
        \\import Outer
        \\
        \\main! = |input| {
        \\    result : Try([Direct(Inner), Nested(Outer)], _)
        \\    result = Json.parse(input)
        \\    match result {
        \\        Err(_) => "err"
        \\        Ok(_) => "ok"
        \\    }
        \\}
        ,
    });

    const path = try dir.dir.realPathFileAlloc(std.testing.io, "main.roc", std.testing.allocator);
    defer std.testing.allocator.free(path);
    try harness.expectAppPathLowersToLir(path);
}

test "issue 12376: derived parser for a tag union with an empty record payload lowers to LIR" {
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
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./platform.roc" }
        \\
        \\main! = |input| {
        \\    result : Try([A({}), B(U64)], _)
        \\    result = Json.parse(input)
        \\    match result {
        \\        Err(_) => "err"
        \\        Ok(_) => "ok"
        \\    }
        \\}
        ,
    });

    const path = try dir.dir.realPathFileAlloc(std.testing.io, "main.roc", std.testing.allocator);
    defer std.testing.allocator.free(path);
    try harness.expectAppPathLowersToLir(path);
}
