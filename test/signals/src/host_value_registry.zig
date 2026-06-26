const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const erased_calls = @import("erased_calls.zig");

pub const HostValue = u64;

pub const Error = error{
    OutOfMemory,
    InvalidHandle,
    ReleasedHandle,
    UnconsumedHandle,
    MissingCapability,
    CapabilityMismatch,
    ConflictingCapability,
};

pub fn Registry(comptime Capability: type) type {
    const Cell = struct {
        box: abi.RocBox,
        capability: ?Capability,
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

        fn cell(box: abi.RocBox, owned_capability: ?Capability, last_taken_epoch: u64) Cell {
            return .{ .box = box, .capability = owned_capability, .last_taken_epoch = last_taken_epoch };
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

        pub fn capability(self: *Self, value: HostValue) Error!?Capability {
            const occupied = try self.occupiedCell(value);
            return occupied.capability;
        }

        pub fn storeOwnedCapability(self: *Self, allocator: std.mem.Allocator, box: abi.RocBox, owned_capability: ?Capability, ops: anytype) Error!HostValue {
            for (self.slots.items, 0..) |*entry, index| {
                switch (entry.*) {
                    .vacant => |last_taken_epoch| {
                        entry.* = .{ .occupied = cell(box, owned_capability, last_taken_epoch) };
                        return @intCast(index + 1);
                    },
                    .occupied => {},
                }
            }

            self.slots.append(allocator, .{ .occupied = cell(box, owned_capability, 0) }) catch {
                if (owned_capability) |capability_value| ops.releaseCapability(capability_value);
                return Error.OutOfMemory;
            };
            return @intCast(self.slots.items.len);
        }

        pub fn storeRetainedCapability(self: *Self, allocator: std.mem.Allocator, box: abi.RocBox, borrowed_capability: ?Capability, ops: anytype) Error!HostValue {
            if (borrowed_capability) |capability_value| {
                ops.retainCapability(capability_value);
                errdefer ops.releaseCapability(capability_value);
            }
            return try self.storeOwnedCapability(allocator, box, borrowed_capability, ops);
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

        fn storedCapability(self: *Self, value: HostValue) Error!Capability {
            return (try self.capability(value)) orelse Error.MissingCapability;
        }

        pub fn assertCapability(self: *Self, value: HostValue, expected_capability: Capability, ops: anytype) Error!void {
            const actual_capability = try self.storedCapability(value);
            if (!ops.capabilitiesMatch(actual_capability, expected_capability)) return Error.CapabilityMismatch;
        }

        pub fn assertCapabilitySplit(self: *Self, value: HostValue, expected_split: abi.RocErasedCallable, ops: anytype) Error!void {
            const actual_capability = try self.storedCapability(value);
            if (!ops.capabilityMatchesSplit(actual_capability, expected_split)) return Error.CapabilityMismatch;
        }

        pub fn getWithCapability(self: *Self, value: HostValue, expected_capability: Capability, ops: anytype) Error!abi.RocBox {
            try self.assertCapability(value, expected_capability, ops);
            return try self.getWithStoredCapability(value, ops);
        }

        pub fn get(self: *Self, value: HostValue, ops: anytype) Error!abi.RocBox {
            return try self.getWithStoredCapability(value, ops);
        }

        pub fn getWithSplit(self: *Self, value: HostValue, expected_split: abi.RocErasedCallable, ops: anytype) Error!abi.RocBox {
            try self.assertCapabilitySplit(value, expected_split, ops);
            return try self.getWithSplitUnchecked(value, expected_split, ops);
        }

        fn getWithStoredCapability(self: *Self, value: HostValue, ops: anytype) Error!abi.RocBox {
            const capability_value = try self.storedCapability(value);
            const entry = try self.slot(value);
            return switch (entry.*) {
                .vacant => Error.ReleasedHandle,
                .occupied => |*occupied| blk: {
                    const split = ops.splitBoxWithCapability(occupied.box, capability_value);
                    occupied.box = split.keep;
                    break :blk split.out;
                },
            };
        }

        fn getWithSplitUnchecked(self: *Self, value: HostValue, split_callable: abi.RocErasedCallable, ops: anytype) Error!abi.RocBox {
            const entry = try self.slot(value);
            return switch (entry.*) {
                .vacant => Error.ReleasedHandle,
                .occupied => |*occupied| blk: {
                    const split = ops.splitBoxWithSplit(occupied.box, split_callable);
                    occupied.box = split.keep;
                    break :blk split.out;
                },
            };
        }

        pub fn take(self: *Self, value: HostValue, ops: anytype) Error!abi.RocBox {
            const entry = try self.slot(value);
            return switch (entry.*) {
                .vacant => Error.ReleasedHandle,
                .occupied => |occupied| blk: {
                    self.take_epoch +%= 1;
                    entry.* = .{ .vacant = self.take_epoch };
                    if (occupied.capability) |capability_value| ops.releaseCapability(capability_value);
                    break :blk occupied.box;
                },
            };
        }

        pub fn takeWithCapability(self: *Self, value: HostValue, expected_capability: Capability, ops: anytype) Error!abi.RocBox {
            try self.assertCapability(value, expected_capability, ops);
            return try self.take(value, ops);
        }

        pub fn takeWithSplit(self: *Self, value: HostValue, expected_split: abi.RocErasedCallable, ops: anytype) Error!abi.RocBox {
            try self.assertCapabilitySplit(value, expected_split, ops);
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
            try self.ensureStoreCapacity(allocator);
            const entry = try self.slot(value);
            return switch (entry.*) {
                .vacant => Error.ReleasedHandle,
                .occupied => |*occupied| blk: {
                    const capability_value = occupied.capability orelse return Error.MissingCapability;
                    const split = ops.splitBoxWithCapability(occupied.box, capability_value);
                    occupied.box = split.keep;
                    break :blk try self.storeRetainedCapability(allocator, split.out, capability_value, ops);
                },
            };
        }

        pub fn setCapability(self: *Self, value: HostValue, borrowed_capability: Capability, ops: anytype) Error!void {
            const entry = try self.slot(value);
            switch (entry.*) {
                .vacant => return Error.ReleasedHandle,
                .occupied => |*occupied| {
                    if (occupied.capability) |actual_capability| {
                        if (!ops.capabilitiesMatch(actual_capability, borrowed_capability)) return Error.ConflictingCapability;
                        return;
                    }
                    ops.retainCapability(borrowed_capability);
                    occupied.capability = borrowed_capability;
                },
            }
        }
    };
}

const TestCapability = extern struct {
    split: usize,
    eq: usize,
    drop: usize,
};

const TestOps = struct {
    retained_capabilities: *u64,
    released_capabilities: *u64,
    split_boxes: *u64,

    pub fn retainCapability(self: TestOps, _: TestCapability) void {
        self.retained_capabilities.* += 1;
    }

    pub fn releaseCapability(self: TestOps, _: TestCapability) void {
        self.released_capabilities.* += 1;
    }

    pub fn capabilitiesMatch(_: TestOps, actual: TestCapability, expected: TestCapability) bool {
        return actual.split == expected.split and actual.eq == expected.eq and actual.drop == expected.drop;
    }

    pub fn capabilityMatchesSplit(_: TestOps, actual: TestCapability, expected_split: abi.RocErasedCallable) bool {
        return actual.split == @intFromPtr(expected_split);
    }

    pub fn splitBoxWithCapability(self: TestOps, box: abi.RocBox, capability: TestCapability) erased_calls.RocBoxPair {
        return self.splitBoxWithSplit(box, @ptrFromInt(capability.split));
    }

    pub fn splitBoxWithSplit(self: TestOps, box: abi.RocBox, _: abi.RocErasedCallable) erased_calls.RocBoxPair {
        self.split_boxes.* += 1;
        const addr = @intFromPtr(box orelse unreachable);
        return .{
            .keep = @ptrFromInt(addr + 0x1000),
            .out = @ptrFromInt(addr + 0x2000),
        };
    }
};

fn testOps(retained: *u64, released: *u64, splits: *u64) TestOps {
    return .{
        .retained_capabilities = retained,
        .released_capabilities = released,
        .split_boxes = splits,
    };
}

test "host value registry uses one-based handles and reuses vacant slots" {
    var registry: Registry(TestCapability) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained: u64 = 0;
    var released: u64 = 0;
    var splits: u64 = 0;
    const ops = testOps(&retained, &released, &splits);
    const cap = TestCapability{ .split = 0x100, .eq = 0x101, .drop = 0x102 };

    const first = try registry.storeOwnedCapability(std.testing.allocator, @ptrFromInt(0x10), cap, ops);
    const second = try registry.storeOwnedCapability(std.testing.allocator, @ptrFromInt(0x20), null, ops);
    try std.testing.expectEqual(@as(HostValue, 1), first);
    try std.testing.expectEqual(@as(HostValue, 2), second);

    try std.testing.expectEqual(@as(usize, 2), registry.slots.items.len);
    try std.testing.expectEqual(@as(abi.RocBox, @ptrFromInt(0x10)), try registry.take(first, ops));
    try std.testing.expectEqual(@as(u64, 1), released);

    const reused = try registry.storeOwnedCapability(std.testing.allocator, @ptrFromInt(0x30), null, ops);
    try std.testing.expectEqual(first, reused);
    try std.testing.expectEqual(@as(usize, 2), registry.slots.items.len);
}

test "host value registry reports live count and reaches zero when drained" {
    var registry: Registry(TestCapability) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained: u64 = 0;
    var released: u64 = 0;
    var splits: u64 = 0;
    const ops = testOps(&retained, &released, &splits);
    const cap = TestCapability{ .split = 0x200, .eq = 0x201, .drop = 0x202 };

    try std.testing.expectEqual(@as(usize, 0), registry.liveCount());
    try std.testing.expect(!registry.hasLiveValues());

    const first = try registry.storeOwnedCapability(std.testing.allocator, @ptrFromInt(0x10), cap, ops);
    const second = try registry.storeOwnedCapability(std.testing.allocator, @ptrFromInt(0x20), cap, ops);
    try std.testing.expectEqual(@as(usize, 2), registry.liveCount());

    _ = try registry.take(first, ops);
    try std.testing.expectEqual(@as(usize, 1), registry.liveCount());
    try std.testing.expect(registry.hasLiveValues());

    _ = try registry.take(second, ops);
    try std.testing.expectEqual(@as(usize, 0), registry.liveCount());
    try std.testing.expect(!registry.hasLiveValues());
    try std.testing.expectEqual(@as(u64, 2), released);
}

test "host value registry asserts consuming callbacks released their handle" {
    var registry: Registry(TestCapability) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained: u64 = 0;
    var released: u64 = 0;
    var splits: u64 = 0;
    const ops = testOps(&retained, &released, &splits);
    const value = try registry.storeOwnedCapability(std.testing.allocator, @ptrFromInt(0x70), null, ops);
    try std.testing.expectError(Error.UnconsumedHandle, registry.assertReleased(value));

    _ = try registry.take(value, ops);
    try registry.assertReleased(value);
    try std.testing.expectError(Error.ReleasedHandle, registry.take(value, ops));
}

test "host value registry clones by splitting retained boxes and retaining capabilities" {
    var registry: Registry(TestCapability) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained: u64 = 0;
    var released: u64 = 0;
    var splits: u64 = 0;
    const ops = testOps(&retained, &released, &splits);
    const cap = TestCapability{ .split = 0x300, .eq = 0x301, .drop = 0x302 };
    const original = try registry.storeOwnedCapability(std.testing.allocator, @ptrFromInt(0x40), cap, ops);
    const cloned = try registry.clone(std.testing.allocator, original, ops);

    try std.testing.expect(cloned != original);
    try std.testing.expectEqual(@as(u64, 1), splits);
    try std.testing.expectEqual(@as(u64, 1), retained);
    try std.testing.expectEqual(@as(abi.RocBox, @ptrFromInt(0x4040)), try registry.getWithCapability(cloned, cap, ops));
    try std.testing.expectEqual(@as(u64, 2), splits);
}

test "host value registry enforces full capability identity" {
    var registry: Registry(TestCapability) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained: u64 = 0;
    var released: u64 = 0;
    var splits: u64 = 0;
    const ops = testOps(&retained, &released, &splits);
    const cap_a = TestCapability{ .split = 0x400, .eq = 0x401, .drop = 0x402 };
    const cap_b = TestCapability{ .split = 0x400, .eq = 0x411, .drop = 0x402 };
    const value = try registry.storeOwnedCapability(std.testing.allocator, @ptrFromInt(0x80), cap_a, ops);

    try registry.assertCapability(value, cap_a, ops);
    try std.testing.expectError(Error.CapabilityMismatch, registry.assertCapability(value, cap_b, ops));
    try std.testing.expectError(Error.ConflictingCapability, registry.setCapability(value, cap_b, ops));
    try std.testing.expectError(Error.CapabilityMismatch, registry.getWithCapability(value, cap_b, ops));
    try std.testing.expectEqual(@as(abi.RocBox, @ptrFromInt(0x2080)), try registry.getWithCapability(value, cap_a, ops));
}

test "host value registry permits split-only access for capability thunks" {
    var registry: Registry(TestCapability) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained: u64 = 0;
    var released: u64 = 0;
    var splits: u64 = 0;
    const ops = testOps(&retained, &released, &splits);
    const cap = TestCapability{ .split = 0x500, .eq = 0x501, .drop = 0x502 };
    const value = try registry.storeOwnedCapability(std.testing.allocator, @ptrFromInt(0x90), cap, ops);

    try std.testing.expectEqual(@as(abi.RocBox, @ptrFromInt(0x2090)), try registry.getWithSplit(value, @ptrFromInt(0x500), ops));
    try std.testing.expectError(Error.CapabilityMismatch, registry.getWithSplit(value, @ptrFromInt(0x501), ops));
}

test "host value registry consuming operations release capabilities exactly once" {
    var registry: Registry(TestCapability) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained: u64 = 0;
    var released: u64 = 0;
    var splits: u64 = 0;
    const ops = testOps(&retained, &released, &splits);
    const cap = TestCapability{ .split = 0x600, .eq = 0x601, .drop = 0x602 };
    const value = try registry.storeRetainedCapability(std.testing.allocator, @ptrFromInt(0xa0), cap, ops);
    try std.testing.expectEqual(@as(u64, 1), retained);

    const take_epoch = registry.takeEpoch();
    try std.testing.expectEqual(@as(abi.RocBox, @ptrFromInt(0xa0)), try registry.takeWithCapability(value, cap, ops));
    try registry.assertTakenAfter(value, take_epoch);
    try std.testing.expectEqual(@as(u64, 1), released);
    try std.testing.expectError(Error.ReleasedHandle, registry.takeWithCapability(value, cap, ops));
    try std.testing.expectEqual(@as(u64, 1), released);
}

test "host value registry rejects reads before a capability is assigned" {
    var registry: Registry(TestCapability) = .{};
    defer registry.deinit(std.testing.allocator);

    var retained: u64 = 0;
    var released: u64 = 0;
    var splits: u64 = 0;
    const ops = testOps(&retained, &released, &splits);
    const cap = TestCapability{ .split = 0x700, .eq = 0x701, .drop = 0x702 };
    const value = try registry.storeOwnedCapability(std.testing.allocator, @ptrFromInt(0xb0), null, ops);

    try std.testing.expectError(Error.MissingCapability, registry.getWithCapability(value, cap, ops));
    try registry.setCapability(value, cap, ops);
    try std.testing.expectEqual(@as(u64, 1), retained);
    try std.testing.expectEqual(@as(abi.RocBox, @ptrFromInt(0x20b0)), try registry.getWithCapability(value, cap, ops));
}
