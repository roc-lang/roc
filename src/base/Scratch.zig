//! A module that define a stack utility - Scratch - that is helpful
//! when working with recursive operations

const std = @import("std");
const DataSpan = @import("DataSpan.zig").DataSpan;

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

        /// Check if a value is in the array starting from a given position.
        /// Note: If checking multiple values against the same range, use `setViewFrom()`
        /// to build a SetView once and call `contains()` on it multiple times.
        pub fn containsFrom(self: *const Self, start: u32, val: T) bool {
            const range = self.items.items[@intCast(start)..];
            for (range) |item| {
                if (item == val) {
                    return true;
                }
            }
            return false;
        }

        /// A view into a range of the scratch buffer optimized for membership queries.
        /// For small ranges, uses linear scan. For larger ranges, uses a hash set.
        pub const SetView = struct {
            range: []const T,
            set: ?std.AutoHashMapUnmanaged(T, void),

            const hash_threshold = 16;

            pub fn init(items: []const T) SetView {
                if (items.len <= hash_threshold) {
                    return .{ .range = items, .set = null };
                }
                var set = std.AutoHashMapUnmanaged(T, void){};
                set.ensureTotalCapacity(std.heap.page_allocator, @intCast(items.len)) catch {
                    // Fall back to linear scan on allocation failure
                    return .{ .range = items, .set = null };
                };
                for (items) |item| {
                    set.putAssumeCapacity(item, {});
                }
                return .{ .range = items, .set = set };
            }

            pub fn deinit(self: *SetView) void {
                if (self.set) |*set| {
                    set.deinit(std.heap.page_allocator);
                }
            }

            pub fn contains(self: *const SetView, val: T) bool {
                if (self.set) |set| {
                    return set.contains(val);
                }
                for (self.range) |item| {
                    if (item == val) {
                        return true;
                    }
                }
                return false;
            }
        };

        /// Create a SetView for efficient repeated membership queries on a range.
        /// For small ranges, the SetView uses linear scan.
        /// For larger ranges, it builds a hash set for O(1) lookups.
        /// Remember to call deinit() on the returned SetView when done.
        pub fn setViewFrom(self: *const Self, start: u32) SetView {
            return SetView.init(self.items.items[@intCast(start)..]);
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
            if (span.len == 0) return &.{};
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
