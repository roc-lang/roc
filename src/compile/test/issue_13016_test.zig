//! Regression tests for https://github.com/roc-lang/roc/issues/13016.
//!
//! A function whose annotation names an undeclared type declares no type, so
//! a call of it relates nothing to that annotation. A list bound to a name and
//! passed to such a function keeps its own type, and the program lowers with
//! the call as a checked crash after reporting the undeclared type.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "issue 13016: a named empty list passed to a function annotated with an undeclared type lowers" {
    try harness.expectLowersToLirWithOptions(
        \\taken : List(Time) -> List(Time)
        \\taken = |window| window
        \\
        \\main! = |_args| {
        \\    w0 = []
        \\    _ = taken(w0).len()
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Undeclared Type" });
}

test "issue 13016: a use checked before the function annotated with an undeclared type lowers" {
    try harness.expectLowersToLirWithOptions(
        \\main! = |_args| {
        \\    w0 = []
        \\    _ = taken(w0).len()
        \\    Ok({})
        \\}
        \\
        \\taken : List(Time) -> List(Time)
        \\taken = |window| window
    , .{ .allow_user_errors = true, .expected_report_title = "Undeclared Type" });
}

test "issue 13016: a named empty list passed to an associated function annotated with an undeclared type lowers" {
    try harness.expectLowersToLirWithOptions(
        \\Tk := [Tk].{
        \\    taken : List(Time) -> List(Time)
        \\    taken = |window| window
        \\}
        \\
        \\main! = |_args| {
        \\    w0 = []
        \\    _ = Tk.taken(w0).len()
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Undeclared Type" });
}

test "issue 13016: a named empty list passed to an imported type's function annotated with an undeclared type lowers" {
    try expectImportedTakenLowers(
        \\Tk := [].{
        \\    taken : List(Time) -> List(Time)
        \\    taken = |window| window
        \\}
    );
}

test "issue 13016: a named empty list passed to an imported module's function annotated with an undeclared type lowers" {
    try expectImportedTakenLowers(
        \\module [taken]
        \\
        \\taken : List(Time) -> List(Time)
        \\taken = |window| window
    );
}

test "issue 13016: a named empty list passed to a function with a declared annotation lowers cleanly" {
    try harness.expectLowersToLirWithOptions(
        \\taken : List(U8) -> List(U8)
        \\taken = |window| window
        \\
        \\main! = |_args| {
        \\    w0 = []
        \\    _ = taken(w0).len()
        \\    Ok({})
        \\}
    , .{});
}

fn expectImportedTakenLowers(module_source: []const u8) harness.LowerToLirHarnessError!void {
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
    try dir.dir.writeFile(std.testing.io, .{ .sub_path = "Tk.roc", .data = module_source });
    try dir.dir.writeFile(std.testing.io, .{
        .sub_path = "main.roc",
        .data =
        \\app [main!] { pf: platform "./platform.roc" }
        \\
        \\import Tk
        \\
        \\main! = |input| {
        \\    w0 = []
        \\    _ = Tk.taken(w0).len()
        \\    input
        \\}
        ,
    });

    const path = try dir.dir.realPathFileAlloc(std.testing.io, "main.roc", std.testing.allocator);
    defer std.testing.allocator.free(path);
    try harness.expectAppPathLowersToLirWithOptions(path, .{ .allow_user_errors = true, .expected_report_title = "Undeclared Type" });
}
