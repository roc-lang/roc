const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const erased_calls = @import("erased_calls.zig");

pub const HostValue = u64;

pub const Error = error{
    OutOfMemory,
    InvalidHandle,
    ReleasedHandle,
    UnconsumedHandle,
    MissingTag,
    TagMismatch,
    ConflictingTag,
};

pub fn Registry(comptime TypeTag: type, comptime tags_enabled: bool) type {
    const Cell = if (tags_enabled) struct {
        box: abi.RocBox,
        tag: ?TypeTag,
        last_taken_epoch: u64,
    } else struct {
        box: abi.RocBox,
        last_taken_epoch: u64,
    };

    const Slot = union(enum) {
        vacant: u64,
        occupied: Cell,
    };

    return struct {
        const Self = @This();

        slots: std.ArrayListUnmanaged(Slot) = .empty,
        take_epoch: u64 = 0,

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.slots.deinit(allocator);
        }

        pub fn hasLiveValues(self: *const Self) bool {
            for (self.slots.items) |entry| {
                switch (entry) {
                    .vacant => {},
                    .occupied => return true,
                }
            }
            return false;
        }

        pub fn liveCount(self: *const Self) usize {
            var count: usize = 0;
            for (self.slots.items) |entry| {
                switch (entry) {
                    .vacant => {},
                    .occupied => count += 1,
                }
            }
            return count;
        }

        fn cell(box: abi.RocBox, owned_tag: ?TypeTag, last_taken_epoch: u64) Cell {
            if (comptime tags_enabled) {
                return .{ .box = box, .tag = owned_tag, .last_taken_epoch = last_taken_epoch };
            } else {
                return .{ .box = box, .last_taken_epoch = last_taken_epoch };
            }
        }

        fn slot(self: *Self, value: HostValue) Error!*Slot {
            if (value == 0) return Error.InvalidHandle;
            const index = value - 1;
            if (index >= self.slots.items.len) return Error.InvalidHandle;
            return &self.slots.items[@intCast(index)];
        }

        fn occupiedCell(self: *Self, value: HostValue) Error!Cell {
            return switch ((try self.slot(value)).*) {
                .vacant => Error.ReleasedHandle,
                .occupied => |occupied| occupied,
            };
        }

        pub fn tag(self: *Self, value: HostValue) Error!?TypeTag {
            if (comptime tags_enabled) {
                const occupied = try self.occupiedCell(value);
                return occupied.tag;
            } else {
                _ = try self.occupiedCell(value);
                return null;
            }
        }

        pub fn storeOwnedTag(self: *Self, allocator: std.mem.Allocator, box: abi.RocBox, owned_tag: ?TypeTag, ops: anytype) Error!HostValue {
            const registry_tag: ?TypeTag = if (comptime tags_enabled) owned_tag else blk: {
                if (owned_tag) |tag_value| ops.releaseTag(tag_value);
                break :blk null;
            };

            for (self.slots.items, 0..) |*entry, index| {
                switch (entry.*) {
                    .vacant => |last_taken_epoch| {
                        entry.* = .{ .occupied = cell(box, registry_tag, last_taken_epoch) };
                        return @intCast(index + 1);
                    },
                    .occupied => {},
                }
            }

            self.slots.append(allocator, .{ .occupied = cell(box, registry_tag, 0) }) catch {
                if (comptime tags_enabled) {
                    if (registry_tag) |tag_value| ops.releaseTag(tag_value);
                }
                return Error.OutOfMemory;
            };
            return @intCast(self.slots.items.len);
        }

        pub fn storeRetainedTag(self: *Self, allocator: std.mem.Allocator, box: abi.RocBox, borrowed_tag: ?TypeTag, ops: anytype) Error!HostValue {
            if (comptime tags_enabled) {
                if (borrowed_tag) |tag_value| ops.retainTag(tag_value);
                errdefer if (borrowed_tag) |tag_value| ops.releaseTag(tag_value);
                return try self.storeOwnedTag(allocator, box, borrowed_tag, ops);
            } else {
                return try self.storeOwnedTag(allocator, box, null, ops);
            }
        }

        fn vacantSlotAvailable(self: *const Self) bool {
            for (self.slots.items) |entry| {
                switch (entry) {
                    .vacant => return true,
                    .occupied => {},
                }
            }
            return false;
        }

        fn ensureStoreCapacity(self: *Self, allocator: std.mem.Allocator) Error!void {
            if (self.vacantSlotAvailable()) return;
            self.slots.ensureUnusedCapacity(allocator, 1) catch return Error.OutOfMemory;
        }

        pub fn get(self: *Self, value: HostValue, ops: anytype) Error!abi.RocBox {
            if (comptime !tags_enabled) return Error.MissingTag;
            const entry = try self.slot(value);
            return switch (entry.*) {
                .vacant => Error.ReleasedHandle,
                .occupied => |*occupied| blk: {
                    const tag_value = occupied.tag orelse return Error.MissingTag;
                    const split = ops.splitBox(occupied.box, tag_value);
                    occupied.box = split.@"keep";
                    break :blk split.@"out";
                },
            };
        }

        pub fn getTagged(self: *Self, value: HostValue, expected_tag: TypeTag, ops: anytype) Error!abi.RocBox {
            try self.assertTag(value, expected_tag, ops);
            return try self.get(value, ops);
        }

        pub fn take(self: *Self, value: HostValue, ops: anytype) Error!abi.RocBox {
            const entry = try self.slot(value);
            return switch (entry.*) {
                .vacant => Error.ReleasedHandle,
                .occupied => |occupied| blk: {
                    self.take_epoch +%= 1;
                    entry.* = .{ .vacant = self.take_epoch };
                    if (comptime tags_enabled) {
                        if (occupied.tag) |tag_value| ops.releaseTag(tag_value);
                    }
                    break :blk occupied.box;
                },
            };
        }

        pub fn takeTagged(self: *Self, value: HostValue, expected_tag: TypeTag, ops: anytype) Error!abi.RocBox {
            try self.assertTag(value, expected_tag, ops);
            return try self.take(value, ops);
        }

        pub fn assertReleased(self: *Self, value: HostValue) Error!void {
            const entry = try self.slot(value);
            return switch (entry.*) {
                .vacant => {},
                .occupied => Error.UnconsumedHandle,
            };
        }

        pub fn takeEpoch(self: *const Self) u64 {
            return self.take_epoch;
        }

        pub fn assertTakenAfter(self: *Self, value: HostValue, epoch: u64) Error!void {
            const entry = try self.slot(value);
            const last_taken_epoch = switch (entry.*) {
                .vacant => |last| last,
                .occupied => |occupied| occupied.last_taken_epoch,
            };
            if (last_taken_epoch <= epoch) return Error.UnconsumedHandle;
        }

        pub fn clone(self: *Self, allocator: std.mem.Allocator, value: HostValue, ops: anytype) Error!HostValue {
            if (comptime !tags_enabled) return Error.MissingTag;
            try self.ensureStoreCapacity(allocator);
            const entry = try self.slot(value);
            return switch (entry.*) {
                .vacant => Error.ReleasedHandle,
                .occupied => |*occupied| blk: {
                    const tag_value = occupied.tag orelse return Error.MissingTag;
                    const split = ops.splitBox(occupied.box, tag_value);
                    occupied.box = split.@"keep";
                    ops.retainTag(tag_value);
                    break :blk try self.storeOwnedTag(allocator, split.@"out", tag_value, ops);
                },
            };
        }

        pub fn setTag(self: *Self, value: HostValue, borrowed_tag: TypeTag, ops: anytype) Error!void {
            if (comptime !tags_enabled) return;

            const entry = try self.slot(value);
            switch (entry.*) {
                .vacant => return Error.ReleasedHandle,
                .occupied => |*occupied| {
                    if (occupied.tag) |actual_tag| {
                        if (!tagsMatch(TypeTag, actual_tag, borrowed_tag, ops)) return Error.ConflictingTag;
                        return;
                    }
                    ops.retainTag(borrowed_tag);
                    occupied.tag = borrowed_tag;
                },
            }
        }

        pub fn assertTag(self: *Self, value: HostValue, expected_tag: TypeTag, ops: anytype) Error!void {
            if (comptime !tags_enabled) return;

            const actual_tag = (try self.tag(value)) orelse return Error.MissingTag;
            if (!tagsMatch(TypeTag, actual_tag, expected_tag, ops)) return Error.TagMismatch;
        }
    };
}

pub fn tagsMatch(comptime TypeTag: type, actual_tag: TypeTag, expected_tag: TypeTag, ops: anytype) bool {
    return ops.tagsMatch(actual_tag, expected_tag);
}

const TestTag = *u64;

const TestOps = struct {
    retained_tags: *u64,
    released_tags: *u64,
    split_boxes: *u64,

    pub fn retainTag(self: TestOps, _: TestTag) void {
        self.retained_tags.* += 1;
    }

    pub fn releaseTag(self: TestOps, _: TestTag) void {
        self.released_tags.* += 1;
    }

    pub fn tagId(_: TestOps, tag_value: TestTag) u64 {
        return tag_value.*;
    }

    pub fn tagsMatch(self: TestOps, actual_tag: TestTag, expected_tag: TestTag) bool {
        const actual_id = self.tagId(actual_tag);
        return actual_id != 0 and actual_id == self.tagId(expected_tag);
    }

    pub fn splitBox(self: TestOps, box: abi.RocBox, _: TestTag) erased_calls.RocBoxPair {
        self.split_boxes.* += 1;
        const addr = @intFromPtr(box orelse unreachable);
        return .{
            .keep = @ptrFromInt(addr + 0x1000),
            .out = @ptrFromInt(addr + 0x2000),
        };
    }
};

test "host value registry uses one-based handles and reuses vacant slots" {
    var registry: Registry(TestTag, true) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained_tags: u64 = 0;
    var released_tags: u64 = 0;
    var split_boxes: u64 = 0;
    const ops = TestOps{
        .retained_tags = &retained_tags,
        .released_tags = &released_tags,
        .split_boxes = &split_boxes,
    };

    var tag_a: u64 = 1;
    const first = try registry.storeOwnedTag(std.testing.allocator, @ptrFromInt(0x10), &tag_a, ops);
    const second = try registry.storeOwnedTag(std.testing.allocator, @ptrFromInt(0x20), null, ops);
    try std.testing.expectEqual(@as(HostValue, 1), first);
    try std.testing.expectEqual(@as(HostValue, 2), second);

    try std.testing.expectEqual(@as(usize, 2), registry.slots.items.len);
    try std.testing.expectEqual(@as(abi.RocBox, @ptrFromInt(0x10)), try registry.take(first, ops));
    try std.testing.expectEqual(@as(u64, 1), released_tags);

    const reused = try registry.storeOwnedTag(std.testing.allocator, @ptrFromInt(0x30), null, ops);
    try std.testing.expectEqual(first, reused);
    try std.testing.expectEqual(@as(usize, 2), registry.slots.items.len);
}

test "host value registry reports live count and reaches zero when drained" {
    var registry: Registry(TestTag, false) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained_tags: u64 = 0;
    var released_tags: u64 = 0;
    var split_boxes: u64 = 0;
    const ops = TestOps{
        .retained_tags = &retained_tags,
        .released_tags = &released_tags,
        .split_boxes = &split_boxes,
    };

    try std.testing.expectEqual(@as(usize, 0), registry.liveCount());
    try std.testing.expect(!registry.hasLiveValues());

    const first = try registry.storeOwnedTag(std.testing.allocator, @ptrFromInt(0x10), null, ops);
    const second = try registry.storeOwnedTag(std.testing.allocator, @ptrFromInt(0x20), null, ops);
    try std.testing.expectEqual(@as(usize, 2), registry.liveCount());

    _ = try registry.take(first, ops);
    try std.testing.expectEqual(@as(usize, 1), registry.liveCount());
    try std.testing.expect(registry.hasLiveValues());

    _ = try registry.take(second, ops);
    try std.testing.expectEqual(@as(usize, 0), registry.liveCount());
    try std.testing.expect(!registry.hasLiveValues());
}

test "host value registry asserts consuming callbacks released their handle" {
    var registry: Registry(TestTag, false) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained_tags: u64 = 0;
    var released_tags: u64 = 0;
    var split_boxes: u64 = 0;
    const ops = TestOps{
        .retained_tags = &retained_tags,
        .released_tags = &released_tags,
        .split_boxes = &split_boxes,
    };

    const value = try registry.storeOwnedTag(std.testing.allocator, @ptrFromInt(0x70), null, ops);
    try std.testing.expectError(Error.UnconsumedHandle, registry.assertReleased(value));

    _ = try registry.take(value, ops);
    try registry.assertReleased(value);
    try std.testing.expectError(Error.ReleasedHandle, registry.take(value, ops));
}

test "host value registry clones by splitting retained boxes and tags" {
    var registry: Registry(TestTag, true) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained_tags: u64 = 0;
    var released_tags: u64 = 0;
    var split_boxes: u64 = 0;
    const ops = TestOps{
        .retained_tags = &retained_tags,
        .released_tags = &released_tags,
        .split_boxes = &split_boxes,
    };

    var tag_a: u64 = 1;
    const original = try registry.storeOwnedTag(std.testing.allocator, @ptrFromInt(0x40), &tag_a, ops);
    const cloned = try registry.clone(std.testing.allocator, original, ops);

    try std.testing.expect(cloned != original);
    try std.testing.expectEqual(@as(u64, 1), split_boxes);
    try std.testing.expectEqual(@as(u64, 1), retained_tags);
    try std.testing.expectEqual(@as(abi.RocBox, @ptrFromInt(0x4040)), try registry.getTagged(cloned, &tag_a, ops));
    try std.testing.expectEqual(@as(u64, 2), split_boxes);
}

test "host value registry compares tags by nonzero type id" {
    var registry: Registry(TestTag, true) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained_tags: u64 = 0;
    var released_tags: u64 = 0;
    var split_boxes: u64 = 0;
    const ops = TestOps{
        .retained_tags = &retained_tags,
        .released_tags = &released_tags,
        .split_boxes = &split_boxes,
    };

    var tag_a: u64 = 9;
    var tag_b_same_id: u64 = 9;
    var tag_c_other_id: u64 = 10;
    const value = try registry.storeOwnedTag(std.testing.allocator, @ptrFromInt(0x50), &tag_a, ops);

    try registry.assertTag(value, &tag_b_same_id, ops);
    try std.testing.expectError(Error.TagMismatch, registry.assertTag(value, &tag_c_other_id, ops));
}

test "host value registry can compile without stored tags" {
    var registry: Registry(TestTag, false) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained_tags: u64 = 0;
    var released_tags: u64 = 0;
    var split_boxes: u64 = 0;
    const ops = TestOps{
        .retained_tags = &retained_tags,
        .released_tags = &released_tags,
        .split_boxes = &split_boxes,
    };

    var tag_a: u64 = 1;
    const value = try registry.storeOwnedTag(std.testing.allocator, @ptrFromInt(0x60), &tag_a, ops);
    try std.testing.expectEqual(@as(u64, 1), released_tags);
    try registry.assertTag(value, &tag_a, ops);
    try std.testing.expectEqual(@as(abi.RocBox, @ptrFromInt(0x60)), try registry.take(value, ops));
}
