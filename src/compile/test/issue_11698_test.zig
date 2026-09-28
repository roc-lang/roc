//! Regression coverage for https://github.com/roc-lang/roc/issues/11698
//!
//! A long left-associative `+` chain on a runtime value must check and lower
//! to LIR without any compiler stage's native call depth growing with the
//! chain. The lowering runs on a thread whose stack is far smaller than the
//! chain length times any per-level recursion cost, so a stage that recurses
//! once per nesting level fails here deterministically.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

const term_count = 10_000;
const stack_bytes = 8 * 1024 * 1024;

fn lowerOnSmallStack(app_body: []const u8, result: *harness.LowerToLirHarnessError!void) void {
    result.* = harness.expectLowersToLir(app_body);
}

test "issue 11698: a long + chain on a runtime value lowers on a small stack" {
    const gpa = std.testing.allocator;
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(gpa);
    try source.appendSlice(gpa,
        \\main! = |args| {
        \\    x = List.len(args)
        \\    _y = x
    );
    for (1..term_count) |_| try source.appendSlice(gpa, " + x");
    try source.appendSlice(gpa,
        \\
        \\    Ok({})
        \\}
    );

    var result: harness.LowerToLirHarnessError!void = {};
    const thread = try std.Thread.spawn(.{ .stack_size = stack_bytes }, lowerOnSmallStack, .{ source.items, &result });
    thread.join();
    try result;
}
