const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const retained_values = @import("retained_values.zig");
const signal_records = @import("signal_records.zig");

pub const HostSignalToken = retained_values.HostSignalToken;
pub const HostSignalRecord = signal_records.Record;

pub const PendingTask = struct {
    request_id: u64,
    owner_scope_id: u64,
    task_token: HostSignalToken,
    task_name: []const u8,
    request: []const u8,
    active: bool,
};

pub const ActiveInterval = struct {
    token: u64,
    source_token: HostSignalToken,
    period_ms: u64,
    active: bool,
};

pub const CleanupEvents = std.ArrayListUnmanaged([]const u8);

pub fn appendCleanupEvent(allocator: std.mem.Allocator, events: *CleanupEvents, name: []const u8) void {
    const copy = allocator.dupe(u8, name) catch @panic("out of memory");
    events.append(allocator, copy) catch {
        allocator.free(copy);
        @panic("out of memory");
    };
}

pub fn cleanupEventCount(events: []const []const u8, name: []const u8) u64 {
    var count: u64 = 0;
    for (events) |event_name| {
        if (std.mem.eql(u8, event_name, name)) count += 1;
    }
    return count;
}

pub fn deinitCleanupEvents(allocator: std.mem.Allocator, events: *CleanupEvents) void {
    for (events.items) |name| {
        allocator.free(name);
    }
    events.deinit(allocator);
    events.* = .empty;
}

pub fn activeTaskRecordByToken(active_signal_graph: anytype, token: HostSignalToken) ?*HostSignalRecord {
    for (active_signal_graph) |node| {
        switch (node.record.payload) {
            .task_source => |payload| {
                if (payload.token == token) return node.record;
            },
            .ref, .const_value, .map, .map2, .combine, .interval_source => {},
        }
    }
    return null;
}

pub fn activeTaskRecordByName(active_signal_graph: anytype, name: []const u8) ?*HostSignalRecord {
    var found: ?*HostSignalRecord = null;
    for (active_signal_graph) |node| {
        switch (node.record.payload) {
            .task_source => |payload| {
                if (!std.mem.eql(u8, payload.name, name)) continue;
                if (found != null) @panic("fake task result matched more than one active task source");
                found = node.record;
            },
            .ref, .const_value, .map, .map2, .combine, .interval_source => {},
        }
    }
    return found;
}

pub fn activeIntervalRecordCountByPeriod(active_signal_graph: anytype, period_ms: u64) u64 {
    var count: u64 = 0;
    for (active_signal_graph) |node| {
        switch (node.record.payload) {
            .interval_source => |payload| {
                if (payload.period_ms == period_ms) count += 1;
            },
            .ref, .const_value, .map, .map2, .combine, .task_source => {},
        }
    }
    return count;
}

pub fn activeIntervalRecordByToken(active_signal_graph: anytype, source_token: HostSignalToken) ?*HostSignalRecord {
    var found: ?*HostSignalRecord = null;
    for (active_signal_graph) |node| {
        switch (node.record.payload) {
            .interval_source => |payload| {
                if (payload.token != source_token) continue;
                if (found != null) @panic("interval token matched more than one active interval source");
                found = node.record;
            },
            .ref, .const_value, .map, .map2, .combine, .task_source => {},
        }
    }
    return found;
}

pub fn activeIntervalRecordByPeriod(active_signal_graph: anytype, period_ms: u64) ?*HostSignalRecord {
    var found: ?*HostSignalRecord = null;
    for (active_signal_graph) |node| {
        switch (node.record.payload) {
            .interval_source => |payload| {
                if (payload.period_ms != period_ms) continue;
                if (found != null) @panic("tick_interval matched more than one active interval source");
                found = node.record;
            },
            .ref, .const_value, .map, .map2, .combine, .task_source => {},
        }
    }
    return found;
}

pub fn appendPendingTask(
    allocator: std.mem.Allocator,
    tasks: *std.ArrayListUnmanaged(PendingTask),
    next_task_request_id: *u64,
    roc_host: *abi.RocHost,
    owner_scope_id: u64,
    task_token: HostSignalToken,
    task_name: []const u8,
    request: []const u8,
) u64 {
    if (next_task_request_id.* == std.math.maxInt(u64)) @panic("host task request id overflowed");
    const request_id = next_task_request_id.*;
    next_task_request_id.* += 1;

    _ = retained_values.retainHostSignalToken(task_token);
    const task_name_copy = allocator.dupe(u8, task_name) catch @panic("out of memory");
    const request_copy = allocator.dupe(u8, request) catch {
        allocator.free(task_name_copy);
        @panic("out of memory");
    };
    tasks.append(allocator, .{
        .request_id = request_id,
        .owner_scope_id = owner_scope_id,
        .task_token = task_token,
        .task_name = task_name_copy,
        .request = request_copy,
        .active = true,
    }) catch {
        retained_values.releaseHostSignalToken(task_token, roc_host);
        allocator.free(task_name_copy);
        allocator.free(request_copy);
        @panic("out of memory");
    };
    return request_id;
}

pub fn appendAndStartPendingTask(
    comptime Ctx: type,
    ctx: Ctx.Handle,
    allocator: std.mem.Allocator,
    tasks: *std.ArrayListUnmanaged(PendingTask),
    next_task_request_id: *u64,
    roc_host: *abi.RocHost,
    owner_scope_id: u64,
    task_token: HostSignalToken,
    task_name: []const u8,
    request: []const u8,
) u64 {
    const request_id = appendPendingTask(allocator, tasks, next_task_request_id, roc_host, owner_scope_id, task_token, task_name, request);
    Ctx.sink(ctx).startTask(request_id, task_name, request);
    return request_id;
}

pub fn deinitPendingTask(allocator: std.mem.Allocator, roc_host: *abi.RocHost, task: *PendingTask) void {
    retained_values.releaseHostSignalToken(task.task_token, roc_host);
    allocator.free(task.task_name);
    allocator.free(task.request);
    task.* = undefined;
}

pub fn cancelPendingTask(comptime Ctx: type, ctx: Ctx.Handle, allocator: std.mem.Allocator, roc_host: *abi.RocHost, task: *PendingTask) void {
    if (task.active) {
        Ctx.sink(ctx).cancelTask(task.request_id);
    }
    deinitPendingTask(allocator, roc_host, task);
}

pub fn clearPendingTasks(comptime Ctx: type, ctx: Ctx.Handle, allocator: std.mem.Allocator, tasks: *std.ArrayListUnmanaged(PendingTask), roc_host: ?*abi.RocHost) void {
    const host = roc_host orelse {
        if (tasks.items.len != 0) @panic("pending tasks cannot release tokens without a Roc host");
        return;
    };
    for (tasks.items) |*task| {
        cancelPendingTask(Ctx, ctx, allocator, host, task);
    }
    tasks.items.len = 0;
}

pub fn pendingTaskIndexByName(tasks: []const PendingTask, name: []const u8) ?usize {
    var found: ?usize = null;
    for (tasks, 0..) |task, index| {
        if (!task.active) continue;
        if (!std.mem.eql(u8, task.task_name, name)) continue;
        if (found != null) @panic("fake task result matched more than one pending request");
        found = index;
    }
    return found;
}

pub fn pendingTaskCountByName(tasks: []const PendingTask, name: []const u8) u64 {
    var count: u64 = 0;
    for (tasks) |task| {
        if (task.active and std.mem.eql(u8, task.task_name, name)) count += 1;
    }
    return count;
}

pub fn pendingTaskIndexByRequestId(tasks: []const PendingTask, request_id: u64) ?usize {
    var found: ?usize = null;
    for (tasks, 0..) |task, index| {
        if (!task.active or task.request_id != request_id) continue;
        if (found != null) @panic("task request id matched more than one pending request");
        found = index;
    }
    return found;
}

pub fn removePendingTaskAt(tasks: *std.ArrayListUnmanaged(PendingTask), index: usize) PendingTask {
    if (index >= tasks.items.len) @panic("pending task index is out of bounds");
    const task = tasks.items[index];
    const last_index = tasks.items.len - 1;
    if (index != last_index) {
        tasks.items[index] = tasks.items[last_index];
    }
    tasks.items.len = last_index;
    return task;
}

pub fn cancelPendingTasksByTaskToken(comptime Ctx: type, ctx: Ctx.Handle, allocator: std.mem.Allocator, tasks: *std.ArrayListUnmanaged(PendingTask), roc_host: ?*abi.RocHost, task_token: HostSignalToken) void {
    const host = roc_host orelse {
        for (tasks.items) |task| {
            if (task.active and task.task_token == task_token) @panic("pending task cannot release token without a Roc host");
        }
        return;
    };

    var index: usize = 0;
    while (index < tasks.items.len) {
        if (!tasks.items[index].active or tasks.items[index].task_token != task_token) {
            index += 1;
            continue;
        }

        var task = removePendingTaskAt(tasks, index);
        cancelPendingTask(Ctx, ctx, allocator, host, &task);
    }
}

pub fn cancelPendingTasksInScopeSubtree(comptime Ctx: type, ctx: Ctx.Handle, allocator: std.mem.Allocator, tasks: *std.ArrayListUnmanaged(PendingTask), roc_host: ?*abi.RocHost, scope_id: u64, scope_lookup: anytype) void {
    const host = roc_host orelse {
        for (tasks.items) |task| {
            if (scope_lookup.descendantOrSelf(task.owner_scope_id, scope_id)) @panic("pending task cannot release token without a Roc host");
        }
        return;
    };

    var write_index: usize = 0;
    for (tasks.items) |*task| {
        if (scope_lookup.descendantOrSelf(task.owner_scope_id, scope_id)) {
            cancelPendingTask(Ctx, ctx, allocator, host, task);
            continue;
        }
        tasks.items[write_index] = task.*;
        write_index += 1;
    }
    tasks.items.len = write_index;
}

pub fn activeIntervalSourceTokenByRuntimeToken(intervals: []const ActiveInterval, token: u64) ?HostSignalToken {
    var found: ?HostSignalToken = null;
    for (intervals) |interval| {
        if (!interval.active or interval.token != token) continue;
        if (found != null) @panic("runtime interval token matched more than one active interval");
        found = interval.source_token;
    }
    return found;
}

pub fn activeIntervalBySourceToken(intervals: []ActiveInterval, source_token: HostSignalToken) ?*ActiveInterval {
    var found: ?*ActiveInterval = null;
    for (intervals) |*interval| {
        if (interval.source_token != source_token) continue;
        if (found != null) @panic("interval source token matched more than one runtime interval");
        found = interval;
    }
    return found;
}

pub fn activeIntervalIndexBySourceToken(intervals: []const ActiveInterval, source_token: HostSignalToken) ?usize {
    var found_index: ?usize = null;
    for (intervals, 0..) |interval, index| {
        if (interval.source_token != source_token) continue;
        if (found_index != null) @panic("interval source token matched more than one runtime interval");
        found_index = index;
    }
    return found_index;
}

pub fn markActiveIntervalsInactive(intervals: []ActiveInterval) void {
    for (intervals) |*interval| {
        interval.active = false;
    }
}

pub fn removeActiveIntervalAt(intervals: *std.ArrayListUnmanaged(ActiveInterval), index: usize) ActiveInterval {
    if (index >= intervals.items.len) @panic("active interval index is out of bounds");
    const interval = intervals.items[index];
    const last_index = intervals.items.len - 1;
    if (index != last_index) {
        intervals.items[index] = intervals.items[last_index];
    }
    intervals.items.len = last_index;
    return interval;
}

pub fn clearActiveIntervals(comptime Ctx: type, ctx: Ctx.Handle, intervals: *std.ArrayListUnmanaged(ActiveInterval), roc_host: ?*abi.RocHost) void {
    const host = roc_host orelse {
        if (intervals.items.len != 0) @panic("active intervals cannot release tokens without a Roc host");
        intervals.items.len = 0;
        return;
    };
    for (intervals.items) |interval| {
        if (interval.active) {
            Ctx.sink(ctx).cancelInterval(interval.token);
        }
        retained_values.releaseHostSignalToken(interval.source_token, host);
    }
    intervals.clearRetainingCapacity();
}

pub fn ensureActiveInterval(comptime Ctx: type, ctx: Ctx.Handle, allocator: std.mem.Allocator, intervals: *std.ArrayListUnmanaged(ActiveInterval), next_interval_token: *u64, roc_host: *abi.RocHost, source_token: HostSignalToken, period_ms: u64) void {
    if (activeIntervalBySourceToken(intervals.items, source_token)) |interval| {
        if (interval.period_ms != period_ms) @panic("interval source token changed period");
        interval.active = true;
        return;
    }

    if (next_interval_token.* == std.math.maxInt(u64)) @panic("host interval token overflowed");
    const token = next_interval_token.*;
    next_interval_token.* += 1;
    _ = retained_values.retainHostSignalToken(source_token);
    intervals.append(allocator, .{
        .token = token,
        .source_token = source_token,
        .period_ms = period_ms,
        .active = true,
    }) catch {
        retained_values.releaseHostSignalToken(source_token, roc_host);
        @panic("out of memory");
    };
    Ctx.sink(ctx).startInterval(token, period_ms);
}

pub fn removeActiveIntervalBySourceToken(comptime Ctx: type, ctx: Ctx.Handle, intervals: *std.ArrayListUnmanaged(ActiveInterval), roc_host: *abi.RocHost, source_token: HostSignalToken) void {
    const index = activeIntervalIndexBySourceToken(intervals.items, source_token) orelse @panic("active interval removal missed its source token");
    const interval = removeActiveIntervalAt(intervals, index);
    if (interval.active) {
        Ctx.sink(ctx).cancelInterval(interval.token);
    }
    retained_values.releaseHostSignalToken(interval.source_token, roc_host);
}

pub fn finishActiveIntervalSync(comptime Ctx: type, ctx: Ctx.Handle, intervals: *std.ArrayListUnmanaged(ActiveInterval), roc_host: ?*abi.RocHost) void {
    const host = roc_host orelse {
        for (intervals.items) |interval| {
            if (!interval.active) @panic("inactive interval cannot release token without a Roc host");
        }
        return;
    };

    var write_index: usize = 0;
    for (intervals.items) |interval| {
        if (!interval.active) {
            Ctx.sink(ctx).cancelInterval(interval.token);
            retained_values.releaseHostSignalToken(interval.source_token, host);
            continue;
        }
        intervals.items[write_index] = interval;
        write_index += 1;
    }
    intervals.items.len = write_index;
}

pub fn syncActiveIntervalsFromGraph(
    comptime Ctx: type,
    ctx: Ctx.Handle,
    allocator: std.mem.Allocator,
    intervals: *std.ArrayListUnmanaged(ActiveInterval),
    next_interval_token: *u64,
    roc_host: ?*abi.RocHost,
    active_signal_graph: anytype,
    metrics: anytype,
) void {
    markActiveIntervalsInactive(intervals.items);
    metrics.bump(.active_intervals_synced, @intCast(active_signal_graph.len));

    for (active_signal_graph) |node| {
        switch (node.record.payload) {
            .interval_source => |payload| {
                const host = roc_host orelse @panic("active interval cannot retain token without a Roc host");
                ensureActiveInterval(Ctx, ctx, allocator, intervals, next_interval_token, host, payload.token, payload.period_ms);
            },
            .ref, .const_value, .map, .map2, .combine, .task_source => {},
        }
    }

    finishActiveIntervalSync(Ctx, ctx, intervals, roc_host);
}

const TestActiveNode = struct {
    record: *HostSignalRecord,
};

const TestMetrics = struct {
    active_intervals_synced: u64 = 0,

    pub fn bump(self: *@This(), comptime field: enum { active_intervals_synced }, n: u64) void {
        @field(self, @tagName(field)) += n;
    }
};

const TestIntervalHost = struct {
    start_interval_count: u64 = 0,
    cancel_interval_count: u64 = 0,
};

const TestIntervalSink = struct {
    host: *TestIntervalHost,

    pub fn startInterval(self: @This(), _: u64, _: u64) void {
        self.host.start_interval_count += 1;
    }

    pub fn cancelInterval(self: @This(), _: u64) void {
        self.host.cancel_interval_count += 1;
    }
};

const TestIntervalCtx = struct {
    pub const Handle = *TestIntervalHost;
    pub const Sink = TestIntervalSink;

    pub fn sink(ctx: Handle) Sink {
        return .{ .host = ctx };
    }
};

fn testTaskRecord(token: HostSignalToken, name: []const u8) HostSignalRecord {
    return .{
        .ref_count = 1,
        .payload = .{ .task_source = .{
            .token = token,
            .name = name,
            .payload_cap = undefined,
            .initial = undefined,
            .done = undefined,
            .failed = undefined,
            .cap = undefined,
            .reset_on_start = false,
        } },
    };
}

fn testIntervalRecord(token: HostSignalToken, period_ms: u64) HostSignalRecord {
    return .{
        .ref_count = 1,
        .payload = .{ .interval_source = .{
            .token = token,
            .period_ms = period_ms,
            .initial = undefined,
            .tick = undefined,
            .cap = undefined,
        } },
    };
}

test "effects runtime finds and removes pending tasks" {
    var first_token: u64 = 0;
    var second_token: u64 = 0;
    var tasks: std.ArrayListUnmanaged(PendingTask) = .empty;
    defer tasks.deinit(std.testing.allocator);

    tasks.append(std.testing.allocator, .{
        .request_id = 1,
        .owner_scope_id = 10,
        .task_token = &first_token,
        .task_name = "load",
        .request = "a",
        .active = true,
    }) catch @panic("out of memory");
    tasks.append(std.testing.allocator, .{
        .request_id = 2,
        .owner_scope_id = 11,
        .task_token = &second_token,
        .task_name = "save",
        .request = "b",
        .active = true,
    }) catch @panic("out of memory");

    try std.testing.expectEqual(@as(?usize, 1), pendingTaskIndexByName(tasks.items, "save"));
    const removed = removePendingTaskAt(&tasks, 0);
    try std.testing.expectEqual(@as(u64, 1), removed.request_id);
    try std.testing.expectEqual(@as(usize, 1), tasks.items.len);
    try std.testing.expectEqual(@as(u64, 2), tasks.items[0].request_id);
}

test "effects runtime finds active effect source records" {
    var task_token: u64 = 0;
    var first_interval_token: u64 = 0;
    var second_interval_token: u64 = 0;
    var task_record = testTaskRecord(&task_token, "load");
    var first_interval_record = testIntervalRecord(&first_interval_token, 250);
    var second_interval_record = testIntervalRecord(&second_interval_token, 500);
    var ref_record = HostSignalRecord{
        .ref_count = 1,
        .payload = .{ .ref = 42 },
    };
    const active_nodes = [_]TestActiveNode{
        .{ .record = &ref_record },
        .{ .record = &task_record },
        .{ .record = &first_interval_record },
        .{ .record = &second_interval_record },
    };

    try std.testing.expectEqual(@as(?*HostSignalRecord, &task_record), activeTaskRecordByToken(active_nodes[0..], &task_token));
    try std.testing.expectEqual(@as(?*HostSignalRecord, &task_record), activeTaskRecordByName(active_nodes[0..], "load"));
    try std.testing.expectEqual(@as(u64, 1), activeIntervalRecordCountByPeriod(active_nodes[0..], 250));
    try std.testing.expectEqual(@as(?*HostSignalRecord, &first_interval_record), activeIntervalRecordByToken(active_nodes[0..], &first_interval_token));
    try std.testing.expectEqual(@as(?*HostSignalRecord, &second_interval_record), activeIntervalRecordByPeriod(active_nodes[0..], 500));
    try std.testing.expectEqual(@as(?*HostSignalRecord, null), activeTaskRecordByName(active_nodes[0..], "missing"));
}

test "effects runtime owns cleanup event storage" {
    var events: CleanupEvents = .empty;
    defer deinitCleanupEvents(std.testing.allocator, &events);

    appendCleanupEvent(std.testing.allocator, &events, "close");
    appendCleanupEvent(std.testing.allocator, &events, "close");
    appendCleanupEvent(std.testing.allocator, &events, "flush");

    try std.testing.expectEqual(@as(u64, 2), cleanupEventCount(events.items, "close"));
    try std.testing.expectEqual(@as(u64, 1), cleanupEventCount(events.items, "flush"));
    try std.testing.expectEqual(@as(u64, 0), cleanupEventCount(events.items, "missing"));
}

test "effects runtime indexes pending tasks" {
    var first_token: u64 = 0;
    var second_token: u64 = 0;
    var tasks: std.ArrayListUnmanaged(PendingTask) = .empty;
    defer tasks.deinit(std.testing.allocator);

    tasks.append(std.testing.allocator, .{
        .request_id = 10,
        .owner_scope_id = 1,
        .task_token = &first_token,
        .task_name = "load",
        .request = "a",
        .active = true,
    }) catch @panic("out of memory");
    tasks.append(std.testing.allocator, .{
        .request_id = 11,
        .owner_scope_id = 2,
        .task_token = &second_token,
        .task_name = "load",
        .request = "b",
        .active = false,
    }) catch @panic("out of memory");

    try std.testing.expectEqual(@as(u64, 1), pendingTaskCountByName(tasks.items, "load"));
    try std.testing.expectEqual(@as(?usize, 0), pendingTaskIndexByRequestId(tasks.items, 10));
    try std.testing.expectEqual(@as(?usize, null), pendingTaskIndexByRequestId(tasks.items, 11));
}

test "effects runtime updates active interval table" {
    var first_token: u64 = 0;
    var second_token: u64 = 0;
    var intervals: std.ArrayListUnmanaged(ActiveInterval) = .empty;
    defer intervals.deinit(std.testing.allocator);

    intervals.append(std.testing.allocator, .{
        .token = 10,
        .source_token = &first_token,
        .period_ms = 100,
        .active = true,
    }) catch @panic("out of memory");
    intervals.append(std.testing.allocator, .{
        .token = 11,
        .source_token = &second_token,
        .period_ms = 200,
        .active = true,
    }) catch @panic("out of memory");

    try std.testing.expectEqual(@as(?HostSignalToken, &first_token), activeIntervalSourceTokenByRuntimeToken(intervals.items, 10));
    markActiveIntervalsInactive(intervals.items);
    try std.testing.expect(!intervals.items[0].active);
    try std.testing.expectEqual(@as(?*ActiveInterval, &intervals.items[1]), activeIntervalBySourceToken(intervals.items, &second_token));
    const removed = removeActiveIntervalAt(&intervals, 0);
    try std.testing.expectEqual(@as(u64, 10), removed.token);
    try std.testing.expectEqual(@as(usize, 1), intervals.items.len);
    try std.testing.expectEqual(@as(u64, 11), intervals.items[0].token);
}

test "effects runtime syncs existing active intervals from graph" {
    var source_token: u64 = 0;
    var interval_record = testIntervalRecord(&source_token, 250);
    const active_nodes = [_]TestActiveNode{
        .{ .record = &interval_record },
    };

    var intervals: std.ArrayListUnmanaged(ActiveInterval) = .empty;
    defer intervals.deinit(std.testing.allocator);
    intervals.append(std.testing.allocator, .{
        .token = 10,
        .source_token = &source_token,
        .period_ms = 250,
        .active = true,
    }) catch @panic("out of memory");

    var host = TestIntervalHost{};
    var metrics = TestMetrics{};
    var next_interval_token: u64 = 11;
    var roc_host: abi.RocHost = undefined;

    syncActiveIntervalsFromGraph(TestIntervalCtx, &host, std.testing.allocator, &intervals, &next_interval_token, &roc_host, active_nodes[0..], &metrics);

    try std.testing.expectEqual(@as(usize, 1), intervals.items.len);
    try std.testing.expect(intervals.items[0].active);
    try std.testing.expectEqual(@as(u64, 10), intervals.items[0].token);
    try std.testing.expectEqual(@as(u64, 11), next_interval_token);
    try std.testing.expectEqual(@as(u64, 1), metrics.active_intervals_synced);
    try std.testing.expectEqual(@as(u64, 0), host.start_interval_count);
    try std.testing.expectEqual(@as(u64, 0), host.cancel_interval_count);
}
