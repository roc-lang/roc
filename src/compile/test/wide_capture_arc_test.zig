//! ARC ownership and deterministic worker emission across aggregate boundaries.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

test "ARC real workers: record field take is deterministic" {
    // Two calls keep the aggregate callee out of single-use inlining. The first
    // borrows the still-live record; the second can take its dying list field.
    try harness.expectArcParallelismDeterministicLir(.{ .app_body =
        \\count_names : { names : List(Str), discarded : List(Str) } -> U64
        \\count_names = |record| record.names.append("added").len()
        \\
        \\main! = |args| {
        \\    record = { names: args, discarded: ["discarded"] }
        \\    first = count_names(record)
        \\    second = count_names(record)
        \\    if first + second == 0 { Err(Empty) } else { Ok({}) }
        \\}
    }, .{}, true);
}

test "ARC wide capture: scalar captures alongside a list lower under both strategies" {
    var source: std.Io.Writer.Allocating = .init(std.testing.allocator);
    defer source.deinit();
    const writer = &source.writer;
    try writer.writeAll("main! = |args| {\n");
    for (0..128) |index| {
        try writer.print("    salt_{d} = args.len() + {d}\n", .{ index, index });
    }
    try writer.writeAll("    reader = |offset| args.len() + offset");
    for (0..128) |index| try writer.print(" + salt_{d}", .{index});
    try writer.writeAll("\n    if reader(0) == 0 { Err(Mismatch) } else { Ok({}) }\n}\n");
    try harness.expectLowersToLirWithOptions(source.written(), .{ .specialization_strategy = .lss });
    try harness.expectLowersToLirWithOptions(source.written(), .{ .specialization_strategy = .boxy });
}
