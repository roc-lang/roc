//! Source-ordered scratch for name resolution. A frame is a suffix of the
//! entries, not a set: two active frames may both need the same key, and
//! duplicate declarations within one frame have distinct diagnostic state.

const std = @import("std");
const collections = @import("collections");

/// Index the last occurrence of each key. Unlike a unique-ID stack, appending
/// a key again shadows its old position until the new entry is rolled back.
pub fn IndexedScratch(comptime T: type, comptime K: type, comptime keyOf: fn (T) K) type {
    return struct {
        const Self = @This();
        const Row = struct { value: T, previous: collections.OptionalU32 };

        rows: std.MultiArrayList(Row) = .empty,
        latest: collections.DenseMap(K, u32),

        pub fn init(gpa: std.mem.Allocator) Self {
            return .{ .latest = .init(gpa) };
        }

        pub fn deinit(self: *Self) void {
            self.rows.deinit(self.latest.allocator);
            self.latest.deinit();
        }

        pub fn top(self: *const Self) u32 {
            return @intCast(self.rows.len);
        }

        pub fn sliceFromStart(self: *const Self, start: u32) []T {
            return self.rows.items(.value)[start..];
        }

        pub fn at(self: *Self, position: u32) *T {
            return &self.rows.items(.value)[position];
        }

        pub fn lastIndex(self: *const Self, key: K) ?u32 {
            return self.latest.get(key);
        }

        pub fn containsFrom(self: *const Self, start: u32, key: K) bool {
            const position = self.lastIndex(key) orelse return false;
            return position >= start;
        }

        pub fn append(self: *Self, value: T) std.mem.Allocator.Error!void {
            const position = try collections.nextBiasedId(self.rows.len);
            try self.rows.ensureUnusedCapacity(self.latest.allocator, 1);
            const entry = try self.latest.getOrPut(keyOf(value));
            self.rows.appendAssumeCapacity(.{
                .value = value,
                .previous = if (entry.found_existing) .some(entry.value_ptr.*) else .none,
            });
            entry.value_ptr.* = position;
        }

        pub fn clearFrom(self: *Self, start: u32) void {
            while (self.rows.len > start) {
                const row = self.rows.pop().?;
                const key = keyOf(row.value);
                if (row.previous.get()) |previous| {
                    self.latest.getPtr(key).?.* = previous;
                } else {
                    _ = self.latest.remove(key);
                }
            }
        }
    };
}

test "block inventory: restoring duplicate positions is ordered and allocation-free" {
    const Scenario = struct {
        fn keyOf(id: u32) u32 {
            return id;
        }

        fn run(gpa: std.mem.Allocator) !void {
            var stack = IndexedScratch(u32, u32, keyOf).init(gpa);
            defer stack.deinit();
            try stack.append(9);
            const inner = stack.top();
            defer {
                stack.clearFrom(inner);
                // Even a partial append to the inner frame must not erase the
                // outer entry or leave a stale position in the index.
                std.debug.assert(stack.latest.count() == 1);
                std.debug.assert(stack.latest.get(9).? == 0);
            }
            try stack.append(42);
            try stack.append(9);
            try stack.append(9);
            try std.testing.expectEqualSlices(u32, &.{ 42, 9, 9 }, stack.sliceFromStart(inner));
            try std.testing.expect(stack.containsFrom(inner, 9));
            stack.clearFrom(inner + 2);
            try std.testing.expect(stack.containsFrom(inner, 9));
            stack.clearFrom(inner + 1);
            try std.testing.expect(!stack.containsFrom(inner, 9));
            try std.testing.expect(stack.containsFrom(0, 9));
            // Force another sparse page, including failures after the pushes.
            try stack.append(9000);
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Scenario.run, .{});
}
