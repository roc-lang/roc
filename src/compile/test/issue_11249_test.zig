//! Regression tests for issue #11249.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "issue 11249: a decoded value returned in main!'s Err payload lowers to Monotype" {
    try harness.expectLowersToLirWithOptions(
        \\main! : List(Str) => Try({}, _)
        \\main! = |_args| {
        \\    parsed : Str
        \\    parsed = Json.parse("\"1\"")?
        \\    Err(InvalidSpend(parsed))
        \\}
    , .{ .monotype_only = true });
}

test "issue 11249: a decoded value returned in main!'s Err payload lowers to LIR" {
    try harness.expectLowersToLir(
        \\main! : List(Str) => Try({}, _)
        \\main! = |_args| {
        \\    parsed : Str
        \\    parsed = Json.parse("\"1\"")?
        \\    Err(InvalidSpend(parsed))
        \\}
    );
}

test "issue 11249: a helper's decoded Err payload propagated through main! lowers to LIR" {
    try harness.expectLowersToLir(
        \\get! : {} => Try({}, _)
        \\get! = |{}| {
        \\    parsed : Str
        \\    parsed = Json.parse("\"1\"")?
        \\    Err(InvalidSpend(parsed))
        \\}
        \\
        \\main! : List(Str) => Try({}, _)
        \\main! = |_args| {
        \\    get!({})?
        \\    Ok({})
        \\}
    );
}

test "issue 11249: defaulted decoding retains platform root evidence" {
    try harness.expectLowersToLir(
        \\main! : List(Str) => Try({}, _)
        \\main! = |_args| {
        \\    parsed : Str
        \\    parsed = Json.parse("\"1\"") ?? ""
        \\    Err(InvalidSpend(parsed))
        \\}
    );
}

test "issue 11249: distinct decoded payloads retain their complete root schema" {
    try harness.expectLowersToLir(
        \\main! : List(Str) => Try({}, _)
        \\main! = |_args| {
        \\    text : Str
        \\    text = Json.parse("\"1\"")?
        \\    record : { count : I32 }
        \\    record = Json.parse("{\"count\":2}")?
        \\    Err(InvalidSpend({ text, record }))
        \\}
    );
}

test "issue 11249: an app entrypoint alias retains the evaluated callable's evidence" {
    try harness.expectLowersToLir(
        \\entry! : List(Str) => Try({}, _)
        \\entry! = |_args| {
        \\    parsed : Str
        \\    parsed = Json.parse("\"1\"")?
        \\    Err(InvalidSpend(parsed))
        \\}
        \\
        \\main! = entry!
    );
}

test "issue 11249: a platform passes a constrained app procedure as a value" {
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    try tmp_dir.dir.createDirPath(std.testing.io, "platform");
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./platform/main.roc" }
        \\
        \\main! : List(Str) => Try({}, _)
        \\main! = |_args| {
        \\    parsed : Str
        \\    parsed = Json.parse("\"1\"")?
        \\    Err(InvalidSpend(parsed))
        \\}
        ,
    });
    try tmp_dir.dir.writeFile(std.testing.io, .{
        .sub_path = "platform/main.roc",
        .data =
        \\platform ""
        \\    requires {} { main! : List(Str) => Try({}, [Exit(I8), ..]) }
        \\    exposes []
        \\    packages {}
        \\    provides { "roc_main": main_for_host! }
        \\
        \\main_for_host! : List(Str) => I8
        \\main_for_host! = |args| {
        \\    run! = |entry!, argv| match entry!(argv) {
        \\        Ok({}) => 0
        \\        Err(Exit(code)) => code
        \\        Err(_) => 1
        \\    }
        \\    run!(main!, args)
        \\}
        ,
    });
    const app_path = try tmp_dir.dir.realPathFileAlloc(std.testing.io, "main.roc", std.testing.allocator);
    defer std.testing.allocator.free(app_path);
    try harness.expectAppPathLowersToLir(app_path);
}
