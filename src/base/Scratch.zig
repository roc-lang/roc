const std = @import("std");
const exitOnOom = @import("../collections/utils.zig");

pub fn Scratch(comptime T: type) type {
    return struct {
        items: std.ArrayListUnmanaged(T),

        const Self = @This();

        fn init(gpa: std.mem.Allocator) Self {
            return .{
                .items = std.ArrayListUnmanaged(T).initCapacity(gpa, std.math.ceilPowerOfTwoAssert(usize, 64)),
            };
        }

        /// Returns the start position for a new Span of whereClauseIdxs in scratch
        pub fn top(self: *Self) u32 {
            return @as(u32, @intCast(self.items.len));
        }

        /// Places a new WhereClauseIdx in the scratch.  Will panic on OOM.
        pub fn append(self: *Self, gpa: std.mem.Allocator, idx: T) void {
            self.items.append(gpa, idx) catch |err| exitOnOom(err);
        }

        /// Creates a new span starting at start.  Moves the items from scratch
        /// to extra_data as appropriate.
        pub fn spanFromStart(self: *Self, start: u32, gpa: std.mem.Allocator, data: *std.ArrayListUnmanaged(u32)) DataSpan {
            const end = self.items.len;
            defer self.items.shrinkRetainingCapacity(start);
            var i = @as(usize, @intCast(start));
            const data_start = @as(u32, @intCast(data.items.len));
            while (i < end) {
                data.append(gpa, self.items[i].id) catch |err| exitOnOom(err);
                i += 1;
            }
            return .{ .span = .{ .start = data_start, .len = @as(u32, @intCast(end)) - start } };
        }

        /// Clears any WhereClauseIds added to scratch from start until the end.
        /// Should be used wherever the scratch items will not be used,
        /// as in when parsing fails.
        pub fn clearFrom(self: *Self, start: u32) void {
            self.items.shrinkRetainingCapacity(start);
        }
    };
}
