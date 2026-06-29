const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const retained_values = @import("retained_values.zig");

pub const HostSignalToken = retained_values.HostSignalToken;

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

pub fn deinitPendingTask(allocator: std.mem.Allocator, roc_host: *abi.RocHost, task: *PendingTask) void {
    retained_values.releaseHostSignalToken(task.task_token, roc_host);
    allocator.free(task.task_name);
    allocator.free(task.request);
    task.* = undefined;
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
