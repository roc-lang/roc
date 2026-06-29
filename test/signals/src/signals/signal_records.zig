const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const retained = @import("retained_values.zig");

pub const HostValue = retained.HostValue;
pub const HostValueCell = retained.HostValueCell;
pub const HostValueCapability = retained.HostValueCapability;
pub const HostSignalToken = retained.HostSignalToken;

const releaseHostSignalToken = retained.releaseHostSignalToken;
const releaseHostValueCapability = retained.releaseHostValueCapability;

fn u64SliceContains(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

/// Memoized output of a signal record: absent until first evaluated, then a
/// retained cell holding the last computed value plus its capability.
pub const CacheSlot = union(enum) {
    absent,
    present: HostValueCell,

    pub fn deinit(self: *CacheSlot, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
        switch (self.*) {
            .absent => {},
            .present => |*cached| cached.deinit(ctx, roc_host, metrics),
        }
        self.* = .absent;
    }

    pub fn replace(self: *CacheSlot, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, value: HostValue, cap: HostValueCapability) void {
        self.deinit(ctx, roc_host, metrics);
        self.* = .{ .present = HostValueCell.initRetained(value, cap, metrics) };
    }

    pub fn replaceValue(self: *CacheSlot, ctx: anytype, roc_host: *abi.RocHost, value: HostValue) void {
        switch (self.*) {
            .absent => @panic("dirty signal expression was evaluated before its initial value was cached"),
            .present => |*cached| cached.replaceValue(ctx, roc_host, value),
        }
    }

    pub fn cloneRetained(self: CacheSlot, ctx: anytype, metrics: anytype) CacheSlot {
        return switch (self) {
            .absent => .absent,
            .present => |cached| .{ .present = cached.cloneRetained(ctx, metrics) },
        };
    }
};

pub const EvalResult = struct {
    value: HostValue,
    changed: bool,
};

pub const ConstRecord = struct {
    token: HostSignalToken,
    init: abi.RocErasedCallable,
    cap: HostValueCapability,
    cached_value: CacheSlot = .absent,
};

pub const MapRecord = struct {
    token: HostSignalToken,
    input: *Record,
    transform: abi.RocErasedCallable,
    cap: HostValueCapability,
    cached_value: CacheSlot = .absent,
};

pub const Map2Record = struct {
    token: HostSignalToken,
    left: *Record,
    right: *Record,
    transform: abi.RocErasedCallable,
    cap: HostValueCapability,
    cached_value: CacheSlot = .absent,
};

pub const CombineRecord = struct {
    token: HostSignalToken,
    children: []*Record,
    transform: abi.RocErasedCallable,
    cap: HostValueCapability,
    cached_value: CacheSlot = .absent,
};

pub const TaskSourceRecord = struct {
    token: HostSignalToken,
    name: []const u8,
    payload_cap: HostValueCapability,
    initial: abi.RocErasedCallable,
    done: abi.RocErasedCallable,
    failed: abi.RocErasedCallable,
    cap: HostValueCapability,
    reset_on_start: bool,
    cached_value: CacheSlot = .absent,
};

pub const IntervalSourceRecord = struct {
    token: HostSignalToken,
    period_ms: u64,
    initial: abi.RocErasedCallable,
    tick: abi.RocErasedCallable,
    cap: HostValueCapability,
    cached_value: CacheSlot = .absent,
};

pub const Payload = union(enum) {
    ref: u64,
    const_value: ConstRecord,
    map: MapRecord,
    map2: Map2Record,
    combine: CombineRecord,
    task_source: TaskSourceRecord,
    interval_source: IntervalSourceRecord,
};

/// A refcounted, shareable node in the signal graph. Owns its transform/eq/drop
/// thunks plus a memoized cached value; the active graph holds one reference
/// while a record is mounted.
pub const Record = struct {
    ref_count: usize,
    payload: Payload,
    active_graph_id: ?u64 = null,
    active_use_count: usize = 0,
    last_dirty_generation: u64 = 0,
    last_dirty_changed: bool = false,

    pub fn init(allocator: std.mem.Allocator, payload: Payload) *Record {
        const record = allocator.create(Record) catch @panic("out of memory");
        record.* = .{
            .ref_count = 1,
            .payload = payload,
        };
        return record;
    }

    pub fn token(self: *const Record) ?HostSignalToken {
        return switch (self.payload) {
            .ref => null,
            .const_value => |payload| payload.token,
            .map => |payload| payload.token,
            .map2 => |payload| payload.token,
            .combine => |payload| payload.token,
            .task_source => |payload| payload.token,
            .interval_source => |payload| payload.token,
        };
    }

    pub fn retain(self: *Record) *Record {
        self.ref_count += 1;
        return self;
    }

    pub fn release(self: *Record, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
        if (self.ref_count == 0) @panic("host signal record release underflow");
        if (self.ref_count == 1 and self.active_graph_id != null) @panic("active signal graph held the last signal record reference");
        self.ref_count -= 1;
        if (self.ref_count != 0) return;

        switch (self.payload) {
            .ref => {},
            .const_value => |payload| {
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                abi.decrefErasedCallable(payload.init, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 1);
            },
            .map => |payload| {
                payload.input.release(allocator, ctx, roc_host, metrics);
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                abi.decrefErasedCallable(payload.transform, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 1);
            },
            .map2 => |payload| {
                payload.left.release(allocator, ctx, roc_host, metrics);
                payload.right.release(allocator, ctx, roc_host, metrics);
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                abi.decrefErasedCallable(payload.transform, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 1);
            },
            .combine => |payload| {
                for (payload.children) |child| {
                    child.release(allocator, ctx, roc_host, metrics);
                }
                allocator.free(payload.children);
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                abi.decrefErasedCallable(payload.transform, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 1);
            },
            .task_source => |payload| {
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                allocator.free(payload.name);
                releaseHostValueCapability(payload.payload_cap, roc_host, metrics);
                abi.decrefErasedCallable(payload.initial, roc_host);
                abi.decrefErasedCallable(payload.done, roc_host);
                abi.decrefErasedCallable(payload.failed, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 3);
            },
            .interval_source => |payload| {
                var cached_value = payload.cached_value;
                cached_value.deinit(ctx, roc_host, metrics);
                releaseHostSignalToken(payload.token, roc_host);
                abi.decrefErasedCallable(payload.initial, roc_host);
                abi.decrefErasedCallable(payload.tick, roc_host);
                releaseHostValueCapability(payload.cap, roc_host, metrics);
                metrics.bump(.closure_releases, 2);
            },
        }

        allocator.destroy(self);
    }
};

/// A reference to a shared signal record plus the source state-node ids that
/// feed it; the unit a descriptor edge owns.
pub const Binding = struct {
    record: *Record,
    source_node_ids: []u64,

    pub fn cloneRetained(self: Binding, allocator: std.mem.Allocator, metrics: anytype) Binding {
        _ = metrics;
        return .{
            .record = self.record.retain(),
            .source_node_ids = allocator.dupe(u64, self.source_node_ids) catch @panic("out of memory"),
        };
    }

    pub fn deinit(self: *Binding, allocator: std.mem.Allocator, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
        self.record.release(allocator, ctx, roc_host, metrics);
        allocator.free(self.source_node_ids);
    }
};

pub fn validateExistingSignalRecord(record: *Record, expected_tag: std.meta.Tag(Payload)) void {
    if (std.meta.activeTag(record.payload) != expected_tag) {
        @panic("signal token was reused for a different signal expression kind");
    }
}

pub fn appendSignalRecordSourceNodeIds(allocator: std.mem.Allocator, source_node_ids: *std.ArrayListUnmanaged(u64), record: *Record) void {
    switch (record.payload) {
        .ref => |node_id| {
            if (!u64SliceContains(source_node_ids.items, node_id)) {
                source_node_ids.append(allocator, node_id) catch @panic("out of memory");
            }
        },
        .const_value => {},
        .map => |payload| appendSignalRecordSourceNodeIds(allocator, source_node_ids, payload.input),
        .map2 => |payload| {
            appendSignalRecordSourceNodeIds(allocator, source_node_ids, payload.left);
            appendSignalRecordSourceNodeIds(allocator, source_node_ids, payload.right);
        },
        .combine => |payload| {
            for (payload.children) |child| {
                appendSignalRecordSourceNodeIds(allocator, source_node_ids, child);
            }
        },
        .task_source, .interval_source => {},
    }
}

test "appendSignalRecordSourceNodeIds deduplicates source refs" {
    const allocator = std.testing.allocator;
    var left = Record{ .ref_count = 1, .payload = .{ .ref = 10 } };
    var duplicate = Record{ .ref_count = 1, .payload = .{ .ref = 10 } };
    var right = Record{ .ref_count = 1, .payload = .{ .ref = 20 } };
    const children = [_]*Record{ &left, &duplicate, &right };
    var combine = Record{
        .ref_count = 1,
        .payload = .{ .combine = .{
            .token = undefined,
            .children = @constCast(children[0..]),
            .transform = undefined,
            .cap = undefined,
        } },
    };

    var source_node_ids: std.ArrayListUnmanaged(u64) = .empty;
    defer source_node_ids.deinit(allocator);

    appendSignalRecordSourceNodeIds(allocator, &source_node_ids, &combine);
    try std.testing.expectEqualSlices(u64, &.{ 10, 20 }, source_node_ids.items);
}
