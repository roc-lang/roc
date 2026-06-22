const std = @import("std");
const abi = @import("roc_platform_abi.zig");

pub const HostValue = u64;

pub const Error = error{
    OutOfMemory,
    InvalidHandle,
    ReleasedHandle,
    MissingTag,
    TagMismatch,
    ConflictingTag,
};

pub fn Registry(comptime TypeTag: type, comptime tags_enabled: bool) type {
    const Cell = if (tags_enabled) struct {
        box: abi.RocBox,
        tag: ?TypeTag,
    } else struct {
        box: abi.RocBox,
    };

    const Slot = union(enum) {
        vacant,
        occupied: Cell,
    };

    return struct {
        const Self = @This();

        slots: std.ArrayListUnmanaged(Slot) = .empty,

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

        fn cell(box: abi.RocBox, owned_tag: ?TypeTag) Cell {
            if (comptime tags_enabled) {
                return .{ .box = box, .tag = owned_tag };
            } else {
                return .{ .box = box };
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
                    .vacant => {
                        entry.* = .{ .occupied = cell(box, registry_tag) };
                        return @intCast(index + 1);
                    },
                    .occupied => {},
                }
            }

            self.slots.append(allocator, .{ .occupied = cell(box, registry_tag) }) catch return Error.OutOfMemory;
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

        pub fn get(self: *Self, value: HostValue, ops: anytype) Error!abi.RocBox {
            const occupied = try self.occupiedCell(value);
            ops.retainBox(occupied.box);
            return occupied.box;
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
                    entry.* = .vacant;
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

        pub fn clone(self: *Self, allocator: std.mem.Allocator, value: HostValue, ops: anytype) Error!HostValue {
            const occupied = try self.occupiedCell(value);
            ops.retainBox(occupied.box);
            errdefer ops.releaseBox(occupied.box);

            if (comptime tags_enabled) {
                if (occupied.tag) |tag_value| {
                    ops.retainTag(tag_value);
                    errdefer ops.releaseTag(tag_value);
                    return try self.storeOwnedTag(allocator, occupied.box, tag_value, ops);
                }
            }

            return try self.storeOwnedTag(allocator, occupied.box, null, ops);
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
    if (actual_tag == expected_tag) return true;
    const actual_id = ops.tagId(actual_tag);
    return actual_id != 0 and actual_id == ops.tagId(expected_tag);
}

const TestTag = *u64;

const TestOps = struct {
    retained_boxes: *u64,
    released_boxes: *u64,
    retained_tags: *u64,
    released_tags: *u64,

    pub fn retainBox(self: TestOps, _: abi.RocBox) void {
        self.retained_boxes.* += 1;
    }

    pub fn releaseBox(self: TestOps, _: abi.RocBox) void {
        self.released_boxes.* += 1;
    }

    pub fn retainTag(self: TestOps, _: TestTag) void {
        self.retained_tags.* += 1;
    }

    pub fn releaseTag(self: TestOps, _: TestTag) void {
        self.released_tags.* += 1;
    }

    pub fn tagId(_: TestOps, tag_value: TestTag) u64 {
        return tag_value.*;
    }
};

test "host value registry uses one-based handles and reuses vacant slots" {
    var registry: Registry(TestTag, true) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained_boxes: u64 = 0;
    var released_boxes: u64 = 0;
    var retained_tags: u64 = 0;
    var released_tags: u64 = 0;
    const ops = TestOps{
        .retained_boxes = &retained_boxes,
        .released_boxes = &released_boxes,
        .retained_tags = &retained_tags,
        .released_tags = &released_tags,
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

test "host value registry clones retained boxes and tags" {
    var registry: Registry(TestTag, true) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained_boxes: u64 = 0;
    var released_boxes: u64 = 0;
    var retained_tags: u64 = 0;
    var released_tags: u64 = 0;
    const ops = TestOps{
        .retained_boxes = &retained_boxes,
        .released_boxes = &released_boxes,
        .retained_tags = &retained_tags,
        .released_tags = &released_tags,
    };

    var tag_a: u64 = 1;
    const original = try registry.storeOwnedTag(std.testing.allocator, @ptrFromInt(0x40), &tag_a, ops);
    const cloned = try registry.clone(std.testing.allocator, original, ops);

    try std.testing.expect(cloned != original);
    try std.testing.expectEqual(@as(u64, 1), retained_boxes);
    try std.testing.expectEqual(@as(u64, 1), retained_tags);
    try std.testing.expectEqual(@as(abi.RocBox, @ptrFromInt(0x40)), try registry.getTagged(cloned, &tag_a, ops));
    try std.testing.expectEqual(@as(u64, 2), retained_boxes);
}

test "host value registry compares tags by nonzero type id" {
    var registry: Registry(TestTag, true) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained_boxes: u64 = 0;
    var released_boxes: u64 = 0;
    var retained_tags: u64 = 0;
    var released_tags: u64 = 0;
    const ops = TestOps{
        .retained_boxes = &retained_boxes,
        .released_boxes = &released_boxes,
        .retained_tags = &retained_tags,
        .released_tags = &released_tags,
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

    var retained_boxes: u64 = 0;
    var released_boxes: u64 = 0;
    var retained_tags: u64 = 0;
    var released_tags: u64 = 0;
    const ops = TestOps{
        .retained_boxes = &retained_boxes,
        .released_boxes = &released_boxes,
        .retained_tags = &retained_tags,
        .released_tags = &released_tags,
    };

    var tag_a: u64 = 1;
    const value = try registry.storeOwnedTag(std.testing.allocator, @ptrFromInt(0x60), &tag_a, ops);
    try std.testing.expectEqual(@as(u64, 1), released_tags);
    try registry.assertTag(value, &tag_a, ops);
    try std.testing.expectEqual(@as(abi.RocBox, @ptrFromInt(0x60)), try registry.take(value, ops));
}
