//! Wide closure captures preserve ownership through ARC certification.

const std = @import("std");
const harness = @import("lower_to_lir_harness.zig");

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
