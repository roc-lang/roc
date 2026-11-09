//! Plain-text output for `roc loc`.
const std = @import("std");
const compile = @import("compile");
const Counts = compile.source_graph.Counts;

/// Print the root row, present dependency categories, and a nonredundant total.
pub fn render(writer: *std.Io.Writer, kind: compile.package_resolution.HeaderKind, counts: [3]Counts) std.Io.Writer.Error!void {
    try row(writer, switch (kind) {
        .app => "Application",
        .package => "Package",
        .platform => "Platform",
        .module => "Module",
    }, counts[0]);
    var total = counts[0];
    if (counts[1].lines != 0) {
        try row(writer, "Local Dependencies", counts[1]);
        total.add(counts[1]);
    }
    if (counts[2].lines != 0) {
        try row(writer, "HTTPS Dependencies", counts[2]);
        total.add(counts[2]);
    }
    if (counts[1].lines != 0 or counts[2].lines != 0) try row(writer, "Total", total);
}

fn row(writer: *std.Io.Writer, label: []const u8, counts: Counts) std.Io.Writer.Error!void {
    try writer.print("{s}: ", .{label});
    try number(writer, counts.lines);
    try writer.writeAll(" | ");
    try number(writer, counts.code);
    try writer.writeAll(" without comments or blank lines\n");
}

fn number(writer: *std.Io.Writer, value: u64) std.Io.Writer.Error!void {
    var buf: [20]u8 = undefined;
    const digits = std.fmt.bufPrint(&buf, "{d}", .{value}) catch unreachable; // A u64 decimal representation fits in 20 bytes.
    for (digits, 0..) |digit, index| {
        if (index > 0 and (digits.len - index) % 3 == 0) try writer.writeByte(',');
        try writer.writeByte(digit);
    }
}

test "loc output separates dependencies and formats totals" {
    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try render(&out.writer, .app, .{
        .{ .lines = 123456, .code = 114567 },
        .{ .lines = 234124, .code = 210135 },
        .{ .lines = 356821, .code = 321432 },
    });
    try std.testing.expectEqualStrings(
        "Application: 123,456 | 114,567 without comments or blank lines\n" ++
            "Local Dependencies: 234,124 | 210,135 without comments or blank lines\n" ++
            "HTTPS Dependencies: 356,821 | 321,432 without comments or blank lines\n" ++
            "Total: 714,401 | 646,134 without comments or blank lines\n",
        out.written(),
    );
}

test "loc output omits absent dependencies and redundant total" {
    var out = std.Io.Writer.Allocating.init(std.testing.allocator);
    defer out.deinit();
    try render(&out.writer, .package, .{ .{ .lines = 1, .code = 1 }, .{}, .{} });
    try std.testing.expectEqualStrings("Package: 1 | 1 without comments or blank lines\n", out.written());
    out.clearRetainingCapacity();
    try render(&out.writer, .platform, .{ .{}, .{}, .{ .lines = 2, .code = 1 } });
    try std.testing.expectEqualStrings(
        "Platform: 0 | 0 without comments or blank lines\n" ++
            "HTTPS Dependencies: 2 | 1 without comments or blank lines\n" ++
            "Total: 2 | 1 without comments or blank lines\n",
        out.written(),
    );
}
