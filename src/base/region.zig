const std = @import("std");

pub const Region = struct {
    start: Position,
    end: Position,

    pub fn format(self: *const Region, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (fmt.len != 0) {
            std.fmt.invalidFmtError(fmt, self);
        }

        if ((self.start == Position.zero()) and (self.end == Position.zero())) {
            // In tests, it's super common to set all Located values to 0.
            // Also in tests, we don't want to bother printing the locations
            // because it makes failed assertions much harder to read.
            return writer.print("…", .{});
        } else {
            return writer.print("@{}-{}", .{ self.start.offset, self.end.offset });
        }
    }
};

pub const Position = struct {
    offset: u32,

    pub fn zero() Position {
        return Position{ .offset = 0 };
    }
};

pub const LineAndColumn = packed struct(u32) {
    line: u20,
    column: u12,
};
