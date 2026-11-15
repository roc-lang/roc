//! Regression coverage for https://github.com/roc-lang/roc/issues/13062.
//! Nested function definitions must lower through Boxy without native
//! call-stack usage growing with lexical depth.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

fn lowerOnSmallStack(source: []const u8, result: *harness.LowerToLirHarnessError!void) void {
    result.* = harness.expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}

test "issue 13062: nested function definitions lower through Boxy on a small stack" {
    const gpa = std.testing.allocator;
    const depth = 1_000;
    var source: std.Io.Writer.Allocating = .init(gpa);
    defer source.deinit();
    for (0..depth) |i| try source.writer.print("f{d} = |_| {{\n", .{i});
    try source.writer.print("f{d} = |_| 1\nf{d}(0)\n", .{ depth, depth });
    var i: usize = depth;
    while (i > 0) {
        i -= 1;
        try source.writer.writeAll("}\n");
        if (i > 0) try source.writer.print("f{d}(0)\n", .{i});
    }
    try source.writer.writeAll("\nmain! = |_args| {\n\techo!(\"${f0(0).to_str()}\")\n\tOk({})\n}\n");

    var result: harness.LowerToLirHarnessError!void = {};
    const thread = try std.Thread.spawn(.{ .stack_size = 2 * 1024 * 1024 }, lowerOnSmallStack, .{ source.written(), &result });
    thread.join();
    try result;
}
