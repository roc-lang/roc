//! Regression coverage for https://github.com/roc-lang/roc/issues/11886
//!
//! Deeply nested list, record, and tuple literals and a deeply nested type
//! annotation must check and lower to LIR without any compiler stage's native
//! call depth growing with the nesting. The lowering runs on a thread whose
//! stack is far smaller than the depth times any per-level recursion cost, so
//! a stage that recurses once per nesting level fails here deterministically.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

const depth = 10_000;
const stack_bytes = 8 * 1024 * 1024;

fn lowerOnSmallStack(app_body: []const u8, result: *harness.LowerToLirHarnessError!void) void {
    result.* = harness.expectLowersToLir(app_body);
}

fn expectNestingLowersOnSmallStack(
    prefix: []const u8,
    open: []const u8,
    leaf: []const u8,
    close: []const u8,
    suffix: []const u8,
) !void {
    const gpa = std.testing.allocator;
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(gpa);
    try source.appendSlice(gpa, prefix);
    for (0..depth) |_| try source.appendSlice(gpa, open);
    try source.appendSlice(gpa, leaf);
    for (0..depth) |_| try source.appendSlice(gpa, close);
    try source.appendSlice(gpa, suffix);

    var result: harness.LowerToLirHarnessError!void = {};
    const thread = try std.Thread.spawn(.{ .stack_size = stack_bytes }, lowerOnSmallStack, .{ source.items, &result });
    thread.join();
    try result;
}

const main_prefix = "main! = |args| {\n    _y = ";
const main_suffix = "\n    Ok({})\n}";

test "issue 11886: a deeply nested list literal lowers on a small stack" {
    try expectNestingLowersOnSmallStack(main_prefix, "[", "List.len(args)", "]", main_suffix);
}

test "issue 11886: a deeply nested record literal lowers on a small stack" {
    try expectNestingLowersOnSmallStack(main_prefix, "{ a: ", "List.len(args)", " }", main_suffix);
}

test "issue 11886: a deeply nested tuple literal lowers on a small stack" {
    try expectNestingLowersOnSmallStack(main_prefix, "(1, ", "List.len(args)", ")", main_suffix);
}

test "issue 11886: a deeply nested List annotation lowers on a small stack" {
    try expectNestingLowersOnSmallStack(
        "g : ",
        "List(",
        "U64",
        ")",
        " -> U64\ng = |_| 0\nmain! = |_args| {\n    _y = g([])\n    Ok({})\n}",
    );
}
