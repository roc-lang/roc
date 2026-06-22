const std = @import("std");

pub const Error = error{
    OutOfMemory,
    DuplicateKey,
    MismatchedExistingHashes,
};

pub const Reuse = struct {
    scope_id: u64,
    existing_index: usize,
};

pub const RowPlan = union(enum) {
    reuse: Reuse,
    create,
};

pub const Plan = struct {
    rows: []RowPlan,
    removed_scope_ids: []u64,
    rows_reused: u64,
    rows_created: u64,
    rows_removed: u64,

    pub fn deinit(self: Plan, allocator: std.mem.Allocator) void {
        allocator.free(self.rows);
        allocator.free(self.removed_scope_ids);
    }
};

const HashBuckets = std.AutoHashMapUnmanaged(u64, std.ArrayListUnmanaged(usize));

fn appendIndexToHashBucket(allocator: std.mem.Allocator, buckets: *HashBuckets, hash: u64, index: usize) Error!void {
    const entry = buckets.getOrPut(allocator, hash) catch return Error.OutOfMemory;
    if (!entry.found_existing) {
        entry.value_ptr.* = .empty;
    }
    entry.value_ptr.append(allocator, index) catch return Error.OutOfMemory;
}

fn deinitHashBuckets(allocator: std.mem.Allocator, buckets: *HashBuckets) void {
    var values = buckets.valueIterator();
    while (values.next()) |bucket| {
        bucket.deinit(allocator);
    }
    buckets.deinit(allocator);
}

pub fn buildPlan(
    allocator: std.mem.Allocator,
    existing_scope_ids: []const u64,
    existing_key_hashes: []const u64,
    next_key_hashes: []const u64,
    comparator: anytype,
) Error!Plan {
    if (existing_scope_ids.len != existing_key_hashes.len) {
        return Error.MismatchedExistingHashes;
    }

    var matched_existing = allocator.alloc(bool, existing_scope_ids.len) catch return Error.OutOfMemory;
    defer allocator.free(matched_existing);
    @memset(matched_existing, false);

    var rows = allocator.alloc(RowPlan, next_key_hashes.len) catch return Error.OutOfMemory;
    errdefer allocator.free(rows);

    var removed_scope_ids: std.ArrayListUnmanaged(u64) = .empty;
    errdefer removed_scope_ids.deinit(allocator);

    var next_key_indexes_by_hash: HashBuckets = .{};
    defer deinitHashBuckets(allocator, &next_key_indexes_by_hash);
    for (next_key_hashes, 0..) |hash, key_index| {
        if (next_key_indexes_by_hash.getPtr(hash)) |bucket| {
            for (bucket.items) |previous_index| {
                if (comparator.nextKeysEqual(previous_index, key_index)) {
                    return Error.DuplicateKey;
                }
            }
        }
        try appendIndexToHashBucket(allocator, &next_key_indexes_by_hash, hash, key_index);
    }

    var existing_scope_indexes_by_hash: HashBuckets = .{};
    defer deinitHashBuckets(allocator, &existing_scope_indexes_by_hash);
    for (existing_key_hashes, 0..) |hash, existing_index| {
        try appendIndexToHashBucket(allocator, &existing_scope_indexes_by_hash, hash, existing_index);
    }

    var rows_reused: u64 = 0;
    var rows_created: u64 = 0;
    for (next_key_hashes, 0..) |hash, key_index| {
        var matched_scope_id: ?u64 = null;
        var matched_index: usize = undefined;
        if (existing_scope_indexes_by_hash.getPtr(hash)) |bucket| {
            for (bucket.items) |existing_index| {
                if (matched_existing[existing_index]) continue;
                if (comparator.existingKeyEquals(existing_index, key_index)) {
                    matched_existing[existing_index] = true;
                    matched_scope_id = existing_scope_ids[existing_index];
                    matched_index = existing_index;
                    break;
                }
            }
        }

        if (matched_scope_id) |scope_id| {
            rows[key_index] = .{ .reuse = .{ .scope_id = scope_id, .existing_index = matched_index } };
            rows_reused += 1;
        } else {
            rows[key_index] = .create;
            rows_created += 1;
        }
    }

    for (existing_scope_ids, 0..) |scope_id, existing_index| {
        if (matched_existing[existing_index]) continue;
        removed_scope_ids.append(allocator, scope_id) catch return Error.OutOfMemory;
    }

    const removed = removed_scope_ids.toOwnedSlice(allocator) catch return Error.OutOfMemory;
    errdefer allocator.free(removed);

    return .{
        .rows = rows,
        .removed_scope_ids = removed,
        .rows_reused = rows_reused,
        .rows_created = rows_created,
        .rows_removed = @intCast(removed.len),
    };
}

const TestComparator = struct {
    existing_keys: []const u64,
    next_keys: []const u64,

    fn nextKeysEqual(self: *const TestComparator, left_index: usize, right_index: usize) bool {
        return self.next_keys[left_index] == self.next_keys[right_index];
    }

    fn existingKeyEquals(self: *const TestComparator, existing_index: usize, next_index: usize) bool {
        return self.existing_keys[existing_index] == self.next_keys[next_index];
    }
};

test "keyed rows diff reuses creates and removes by typed equality" {
    const existing_scope_ids = [_]u64{ 10, 11, 12 };
    const existing_keys = [_]u64{ 1, 2, 3 };
    const existing_hashes = [_]u64{ 1, 2, 3 };
    const next_keys = [_]u64{ 3, 1, 4 };
    const next_hashes = [_]u64{ 3, 1, 4 };
    const comparator = TestComparator{ .existing_keys = &existing_keys, .next_keys = &next_keys };

    const plan = try buildPlan(std.testing.allocator, &existing_scope_ids, &existing_hashes, &next_hashes, &comparator);
    defer plan.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(u64, 2), plan.rows_reused);
    try std.testing.expectEqual(@as(u64, 1), plan.rows_created);
    try std.testing.expectEqual(@as(u64, 1), plan.rows_removed);

    try std.testing.expectEqual(@as(u64, 12), plan.rows[0].reuse.scope_id);
    try std.testing.expectEqual(@as(usize, 2), plan.rows[0].reuse.existing_index);
    try std.testing.expectEqual(@as(u64, 10), plan.rows[1].reuse.scope_id);
    try std.testing.expectEqual(@as(usize, 0), plan.rows[1].reuse.existing_index);
    try std.testing.expectEqual(RowPlan.create, plan.rows[2]);
    try std.testing.expectEqualSlices(u64, &.{11}, plan.removed_scope_ids);
}

test "keyed rows diff rejects duplicate next keys" {
    const existing_scope_ids = [_]u64{};
    const existing_hashes = [_]u64{};
    const next_keys = [_]u64{ 1, 1 };
    const next_hashes = [_]u64{ 7, 7 };
    const comparator = TestComparator{ .existing_keys = &.{}, .next_keys = &next_keys };

    try std.testing.expectError(Error.DuplicateKey, buildPlan(std.testing.allocator, &existing_scope_ids, &existing_hashes, &next_hashes, &comparator));
}

test "keyed rows diff resolves hash collisions with equality" {
    const existing_scope_ids = [_]u64{ 20, 21 };
    const existing_keys = [_]u64{ 1, 2 };
    const existing_hashes = [_]u64{ 0, 0 };
    const next_keys = [_]u64{ 2, 1 };
    const next_hashes = [_]u64{ 0, 0 };
    const comparator = TestComparator{ .existing_keys = &existing_keys, .next_keys = &next_keys };

    const plan = try buildPlan(std.testing.allocator, &existing_scope_ids, &existing_hashes, &next_hashes, &comparator);
    defer plan.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(u64, 2), plan.rows_reused);
    try std.testing.expectEqual(@as(u64, 0), plan.rows_created);
    try std.testing.expectEqual(@as(u64, 0), plan.rows_removed);
    try std.testing.expectEqual(@as(u64, 21), plan.rows[0].reuse.scope_id);
    try std.testing.expectEqual(@as(u64, 20), plan.rows[1].reuse.scope_id);
}
