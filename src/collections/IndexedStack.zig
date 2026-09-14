//! A unique-ID stack with constant-time position lookup, pop, and reset.
//! Sparse pages map IDs to stack positions. A position is live exactly when
//! it is below the stack length and that entry still contains the same ID.
//! Pop and reset therefore need no sparse writes or epoch counter.
const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn IndexedStack(comptime K: type) type {
    return struct {
        const Self = @This();
        const page_shift = 8;
        const page_len = 1 << page_shift;
        const Page = [page_len]u32;
        const absent = std.math.maxInt(u32);

        allocator: Allocator,
        entries: std.ArrayList(K) = .empty,
        pages: std.ArrayList(?*Page) = .empty,
        page_base: usize = 0,

        pub fn init(allocator: Allocator) Self {
            return .{ .allocator = allocator };
        }

        pub fn deinit(self: *Self) void {
            for (self.pages.items) |page| if (page) |p| self.allocator.destroy(p);
            self.pages.deinit(self.allocator);
            self.entries.deinit(self.allocator);
        }

        fn index(key: K) usize {
            return switch (@typeInfo(K)) {
                .int => @intCast(key),
                .@"enum" => @intCast(@intFromEnum(key)),
                else => @compileError("IndexedStack keys must be integer IDs"),
            };
        }

        pub fn get(self: *const Self, key: K) ?u32 {
            const id = index(key);
            const page_id = id >> page_shift;
            if (page_id < self.page_base) return null;
            const offset = page_id - self.page_base;
            if (offset >= self.pages.items.len) return null;
            const page = self.pages.items[offset] orelse return null;
            const position = page[id & (page_len - 1)];
            if (position >= self.entries.items.len) return null;
            if (self.entries.items[position] != key) return null;
            return position;
        }

        /// Return an existing position, or append the ID and return null.
        /// The common allocated-page path performs only one sparse lookup.
        pub fn getOrPush(self: *Self, key: K) Allocator.Error!?u32 {
            const id = index(key);
            const page_id = id >> page_shift;
            if (page_id >= self.page_base and page_id - self.page_base < self.pages.items.len) {
                if (self.pages.items[page_id - self.page_base]) |page| {
                    const position = page[id & (page_len - 1)];
                    if (position < self.entries.items.len and self.entries.items[position] == key) return position;
                    try self.entries.ensureUnusedCapacity(self.allocator, 1);
                    std.debug.assert(self.entries.items.len < absent);
                    page[id & (page_len - 1)] = @intCast(self.entries.items.len);
                    self.entries.appendAssumeCapacity(key);
                    return null;
                }
            }
            try self.push(key);
            return null;
        }

        pub fn push(self: *Self, key: K) Allocator.Error!void {
            std.debug.assert(self.get(key) == null);
            const id = index(key);
            const page_id = id >> page_shift;
            if (self.pages.items.len == 0) {
                try self.pages.append(self.allocator, null);
                self.page_base = page_id;
            } else if (page_id < self.page_base) {
                const grow = @min(self.page_base, @max(self.page_base - page_id, self.pages.items.len));
                const old_len = self.pages.items.len;
                try self.pages.resize(self.allocator, old_len + grow);
                std.mem.copyBackwards(?*Page, self.pages.items[grow..], self.pages.items[0..old_len]);
                @memset(self.pages.items[0..grow], null);
                self.page_base -= grow;
            } else if (page_id - self.page_base >= self.pages.items.len) {
                const old_len = self.pages.items.len;
                try self.pages.resize(self.allocator, page_id - self.page_base + 1);
                @memset(self.pages.items[old_len..], null);
            }
            const page_slot = &self.pages.items[page_id - self.page_base];
            if (page_slot.* == null) {
                const page = try self.allocator.create(Page);
                @memset(page, absent);
                page_slot.* = page;
            }
            try self.entries.ensureUnusedCapacity(self.allocator, 1);
            std.debug.assert(self.entries.items.len < absent);
            page_slot.*.?[id & (page_len - 1)] = @intCast(self.entries.items.len);
            self.entries.appendAssumeCapacity(key);
        }

        pub fn pop(self: *Self) K {
            return self.entries.pop().?;
        }

        pub fn truncate(self: *Self, len: usize) void {
            std.debug.assert(len <= self.entries.items.len);
            self.entries.items.len = len;
        }

        pub fn clearRetainingCapacity(self: *Self) void {
            self.entries.clearRetainingCapacity();
        }
    };
}

test "indexed stack rejects stale positions after pop, reset, and reuse" {
    var stack = IndexedStack(u32).init(std.testing.allocator);
    defer stack.deinit();
    try stack.push(1000000);
    try stack.push(1);
    try stack.push(4000);
    try std.testing.expectEqual(@as(?u32, 1), stack.get(1));
    try std.testing.expectEqual(@as(u32, 4000), stack.pop());
    try std.testing.expectEqual(@as(?u32, null), stack.get(4000));
    try stack.push(4001);
    try std.testing.expectEqual(@as(?u32, null), stack.get(4000));
    stack.truncate(1);
    try stack.push(4000);
    try std.testing.expectEqual(@as(?u32, null), stack.get(1));
    try std.testing.expectEqual(@as(?u32, 1), stack.get(4000));
    stack.clearRetainingCapacity();
    try stack.push(1);
    try std.testing.expectEqual(@as(?u32, null), stack.get(1000000));
    try std.testing.expectEqual(@as(?u32, null), stack.get(4000));
    try std.testing.expectEqual(@as(?u32, 0), stack.get(1));
}

test "indexed stack allocation failures preserve live entries" {
    const Run = struct {
        fn run(allocator: Allocator) Allocator.Error!void {
            var stack = IndexedStack(u32).init(allocator);
            defer stack.deinit();
            try stack.push(8192);
            errdefer std.debug.assert(stack.get(8192).? == 0);
            try stack.push(0);
            try stack.push(16384);
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Run.run, .{});
}

test "indexed stack combined lookup preserves positions and recovers from allocation failures" {
    const Run = struct {
        fn run(allocator: Allocator) Allocator.Error!void {
            var stack = IndexedStack(u32).init(allocator);
            defer stack.deinit();
            for (0..40) |i| {
                const key: u32 = @intCast(8192 + i);
                const result = try stack.getOrPush(key);
                std.debug.assert(result == null);
                std.debug.assert((try stack.getOrPush(key)).? == i);
            }
            stack.clearRetainingCapacity();
            std.debug.assert(try stack.getOrPush(8193) == null);
            std.debug.assert(stack.get(8192) == null);
            std.debug.assert((try stack.getOrPush(8193)).? == 0);
            errdefer std.debug.assert(stack.get(8193).? == 0);
            std.debug.assert(try stack.getOrPush(0) == null);
            std.debug.assert(try stack.getOrPush(16384) == null);
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Run.run, .{});
}
