//! Regression coverage for https://github.com/roc-lang/roc/issues/11886
//!
//! Parsing and canonicalization are stack-safe, so deeply nested syntax
//! reaches the type checker intact. Each deep shape here is checked on a
//! thread whose stack is far smaller than the nesting depth times any
//! per-level recursion cost, so a checker walk that recurses once per nesting
//! level fails here deterministically.

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

const depth = 100_000;
const stack_bytes = 4 * 1024 * 1024;

fn checkOnSmallStack(source: []const u8, result: *anyerror!void) void {
    result.* = checkNoErrors(source);
}

fn checkNoErrors(source: []const u8) anyerror!void {
    var env = try TestEnv.init("Test", source);
    defer env.deinit();
    try env.assertNoErrors();
}

fn expectChecksOnSmallStack(source: []const u8) !void {
    var result: anyerror!void = {};
    const thread = try std.Thread.spawn(.{ .stack_size = stack_bytes }, checkOnSmallStack, .{ source, &result });
    thread.join();
    try result;
}

fn appendRepeated(gpa: std.mem.Allocator, source: *std.ArrayList(u8), text: []const u8) !void {
    for (0..depth) |_| try source.appendSlice(gpa, text);
}

test "issue 11886: a deeply nested List annotation checks on a small stack" {
    const gpa = std.testing.allocator;
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(gpa);
    try source.appendSlice(gpa, "f : ");
    try appendRepeated(gpa, &source, "List(");
    try source.appendSlice(gpa, "U8");
    try appendRepeated(gpa, &source, ")");
    try source.appendSlice(gpa, " -> {}\nf = |_| {}\n");

    try expectChecksOnSmallStack(source.items);
}

test "issue 11886: a deeply nested list literal checks on a small stack" {
    const gpa = std.testing.allocator;
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(gpa);
    try source.appendSlice(gpa, "f = |_| ");
    try appendRepeated(gpa, &source, "[");
    try source.appendSlice(gpa, "1");
    try appendRepeated(gpa, &source, "]");
    try source.appendSlice(gpa, "\n");

    try expectChecksOnSmallStack(source.items);
}

test "issue 11886: a deeply nested record literal checks on a small stack" {
    const gpa = std.testing.allocator;
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(gpa);
    try source.appendSlice(gpa, "f = |_| ");
    try appendRepeated(gpa, &source, "{ a: ");
    try source.appendSlice(gpa, "1");
    try appendRepeated(gpa, &source, " }");
    try source.appendSlice(gpa, "\n");

    try expectChecksOnSmallStack(source.items);
}

test "issue 11886: a deeply nested tuple literal checks on a small stack" {
    const gpa = std.testing.allocator;
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(gpa);
    try source.appendSlice(gpa, "f = |_| ");
    try appendRepeated(gpa, &source, "(1, ");
    try source.appendSlice(gpa, "1");
    try appendRepeated(gpa, &source, ")");
    try source.appendSlice(gpa, "\n");

    try expectChecksOnSmallStack(source.items);
}

test "issue 11886: deeply nested calls check on a small stack" {
    const gpa = std.testing.allocator;
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(gpa);
    try source.appendSlice(gpa, "id = |x| x\nf = |_| ");
    try appendRepeated(gpa, &source, "id(");
    try source.appendSlice(gpa, "1");
    try appendRepeated(gpa, &source, ")");
    try source.appendSlice(gpa, "\n");

    try expectChecksOnSmallStack(source.items);
}
