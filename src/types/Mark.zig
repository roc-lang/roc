const std = @import("std");

/// Represents different states
value: i32,

const Mark = @This();

/// Represents an unmarked state
pub const NONE: Mark = .{ .value = 3 };

/// Indicates a variable has been visited during an occurs check
pub const VISITED_IN_OCCURS_CHECK: Mark = .{ .value = 2 };

/// Indicates an occurrence was found during unification
pub const OCCURS: Mark = .{ .value = 1 };

/// Used when collecting variable names during pretty printing
pub const GET_VAR_NAMES: Mark = .{ .value = 0 };

/// Returns a new Mark with a value incremented by 1
pub inline fn next(self: Mark) Mark {
    return .{ .value = self.value + 1 };
}

/// Implements custom formatting for Mark values
/// Displays predefined marks as their semantic names and others as "Mark(value)"
pub fn format(
    self: Mark,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    switch (self.value) {
        NONE.value => try writer.writeAll("none"),
        VISITED_IN_OCCURS_CHECK.value => try writer.writeAll("visited_in_occurs_check"),
        OCCURS.value => try writer.writeAll("occurs"),
        GET_VAR_NAMES.value => try writer.writeAll("get_var_names"),
        else => try std.fmt.format(writer, "Mark({})", .{self.value}),
    }
}

/// Compares two Mark values for equality
pub fn eql(self: Mark, other: Mark) bool {
    return self.value == other.value;
}

/// Generates a hash value for use in hash maps
pub fn hash(self: Mark) u64 {
    return @intCast(self.value);
}

test "Mark constants have correct values" {
    try std.testing.expectEqual(@as(i32, 0), GET_VAR_NAMES.value);
    try std.testing.expectEqual(@as(i32, 1), OCCURS.value);
    try std.testing.expectEqual(@as(i32, 2), VISITED_IN_OCCURS_CHECK.value);
    try std.testing.expectEqual(@as(i32, 3), NONE.value);
}

test "Mark.next increments value" {
    const mark = Mark{ .value = 1 };
    const next_mark = mark.next();
    try std.testing.expectEqual(@as(i32, 2), next_mark.value);
}

test "Mark equality" {
    const mark1 = Mark{ .value = 1 };
    const mark2 = Mark{ .value = 1 };
    const mark3 = Mark{ .value = 2 };

    try std.testing.expect(mark1.eql(mark2));
    try std.testing.expect(!mark1.eql(mark3));
}

test "Mark formatting" {
    var buf: [50]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const writer = fbs.writer();

    // Test all constant values
    try std.fmt.format(writer, "{}", .{NONE});
    try std.testing.expectEqualStrings("none", fbs.getWritten());
    fbs.reset();

    try std.fmt.format(writer, "{}", .{VISITED_IN_OCCURS_CHECK});
    try std.testing.expectEqualStrings("visited_in_occurs_check", fbs.getWritten());
    fbs.reset();

    try std.fmt.format(writer, "{}", .{OCCURS});
    try std.testing.expectEqualStrings("occurs", fbs.getWritten());
    fbs.reset();

    try std.fmt.format(writer, "{}", .{GET_VAR_NAMES});
    try std.testing.expectEqualStrings("get_var_names", fbs.getWritten());
    fbs.reset();

    // Test non-constant value
    const custom_mark = Mark{ .value = 42 };
    try std.fmt.format(writer, "{}", .{custom_mark});
    try std.testing.expectEqualStrings("Mark(42)", fbs.getWritten());
}
