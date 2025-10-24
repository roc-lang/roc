//! A module that define a stack utility - Scratch - that is helpful
//! when working with recursive operations

const std = @import("std");
const DataSpan = @import("DataSpan.zig");

/// A stack for easily adding and removing index types when doing recursive operations
pub fn Scratch(comptime T: type) type {
    return struct {
        items: std.array_list.Managed(T),

        const Self = @This();
        const ArrayList = std.array_list.Managed(T);

        pub fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!Self {
            const items = try ArrayList.initCapacity(gpa, std.math.ceilPowerOfTwoAssert(usize, 64));
            return .{
                .items = items,
            };
        }

        pub fn deinit(self: *Self) void {
            self.items.deinit();
        }

        /// Returns the start position for a new Span of indexes in scratch
        pub fn top(self: *const Self) u32 {
            return @as(u32, @intCast(self.items.items.len));
        }

        /// Check if a value is in the array
        pub fn contains(self: *const Self, val: T) bool {
            for (self.items.items) |item| {
                if (item == val) {
                    return true;
                }
            }
            return false;
        }

        /// Places a new index of type `T` in the scratch
        pub fn append(self: *Self, idx: T) std.mem.Allocator.Error!void {
            try self.items.append(idx);
        }

        /// Pop an item of the scratch buffer
        pub fn pop(self: *Self) ?T {
            return self.items.pop();
        }

        /// Creates slice from the provided indexes
        pub fn slice(self: *Self, start: u32, end: u32) []T {
            return self.items.items[@intCast(start)..@intCast(end)];
        }

        /// Creates slice from the provided start index
        pub fn sliceFromStart(self: *Self, start: u32) []T {
            return self.items.items[@intCast(start)..];
        }

        /// Creates slice from the provided start index
        pub fn sliceFromSpan(self: *Self, span: DataSpan) []T {
            const start: usize = @intCast(span.start);
            const end: usize = @intCast(span.start + span.len);

            std.debug.assert(start <= end);
            std.debug.assert(end <= self.items.items.len);

            return self.items.items[start..end];
        }

        /// Creates span from the provided start index to the end of the list
        pub fn spanFrom(self: *Self, start: u32) DataSpan {
            return DataSpan{
                .start = start,
                .len = @as(u32, @intCast(self.items.items.len)) - start,
            };
        }

        /// Creates a new span starting at start.  Moves the items from scratch
        /// to extra_data as appropriate.
        pub fn spanFromStart(self: *Self, start: u32, data: *std.array_list.Managed(u32)) std.mem.Allocator.Error!DataSpan {
            const end = self.items.len;
            defer self.items.shrinkRetainingCapacity(start);
            var i = @as(usize, @intCast(start));
            const data_start = @as(u32, @intCast(data.items.len));
            while (i < end) {
                try data.append(self.items[i].id);
                i += 1;
            }
            return .{ .span = .{ .start = data_start, .len = @as(u32, @intCast(end)) - start } };
        }

        /// Clears any ids added to scratch from start until the end.
        /// Should be used wherever the scratch items will not be used,
        /// as in when parsing fails.
        pub fn clearFrom(self: *Self, start: u32) void {
            if (self.items.items.len > start) {
                self.items.shrinkRetainingCapacity(start);
            }
        }
    };
}
