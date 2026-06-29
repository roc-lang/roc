const std = @import("std");

pub const Error = error{
    UnknownNode,
    MissingDependent,
};

pub fn Node(comptime Record: type) type {
    return struct {
        record: *Record,
        rank: u64 = 0,
        dependents: []u64 = &.{},
    };
}

pub fn appendDependent(comptime Record: type, allocator: std.mem.Allocator, nodes: []Node(Record), input_id: u64, dependent_id: u64) (Error || std.mem.Allocator.Error)!void {
    const input_index: usize = @intCast(input_id);
    if (input_index >= nodes.len) return Error.UnknownNode;

    const dependents = &nodes[input_index].dependents;
    if (containsU64(dependents.*, dependent_id)) return;

    const previous_len = dependents.*.len;
    dependents.* = try allocator.realloc(dependents.*, previous_len + 1);
    dependents.*[previous_len] = dependent_id;
}

pub fn removeDependent(comptime Record: type, allocator: std.mem.Allocator, nodes: []Node(Record), input_id: u64, dependent_id: u64) (Error || std.mem.Allocator.Error)!void {
    const input_index: usize = @intCast(input_id);
    if (input_index >= nodes.len) return Error.UnknownNode;

    const dependents = &nodes[input_index].dependents;
    for (dependents.*, 0..) |existing_id, index| {
        if (existing_id != dependent_id) continue;
        std.mem.copyForwards(u64, dependents.*[index..], dependents.*[index + 1 ..]);
        dependents.* = try allocator.realloc(dependents.*, dependents.*.len - 1);
        return;
    }

    return Error.MissingDependent;
}

pub fn replaceDependent(comptime Record: type, nodes: []Node(Record), input_id: u64, old_dependent_id: u64, new_dependent_id: u64) Error!void {
    const input_index: usize = @intCast(input_id);
    if (input_index >= nodes.len) return Error.UnknownNode;

    const dependents = nodes[input_index].dependents;
    for (dependents) |*existing_id| {
        if (existing_id.* != old_dependent_id) continue;
        existing_id.* = new_dependent_id;
        return;
    }

    return Error.MissingDependent;
}

pub fn rank(comptime Record: type, nodes: []const Node(Record), record_id: u64) Error!u64 {
    const index: usize = @intCast(record_id);
    if (index >= nodes.len) return Error.UnknownNode;
    return nodes[index].rank;
}

pub fn dependentIds(comptime Record: type, nodes: []const Node(Record), record_id: u64) Error![]const u64 {
    const index: usize = @intCast(record_id);
    if (index >= nodes.len) return Error.UnknownNode;
    return nodes[index].dependents;
}

pub fn appendReachableDependents(comptime Record: type, allocator: std.mem.Allocator, nodes: []const Node(Record), record_ids: *std.ArrayListUnmanaged(u64), record_id: u64) (Error || std.mem.Allocator.Error)!void {
    try appendUniqueId(allocator, record_ids, record_id);

    var index: usize = 0;
    while (index < record_ids.items.len) : (index += 1) {
        const current_record_id = record_ids.items[index];
        for (try dependentIds(Record, nodes, current_record_id)) |dependent_record_id| {
            try appendUniqueId(allocator, record_ids, dependent_record_id);
        }
    }
}

pub fn sortIdsByRank(comptime Record: type, nodes: []const Node(Record), record_ids: []u64) Error!void {
    var index: usize = 1;
    while (index < record_ids.len) : (index += 1) {
        const value = record_ids[index];
        const value_rank = try rank(Record, nodes, value);
        var insert_index = index;
        while (insert_index > 0) {
            const previous = record_ids[insert_index - 1];
            const previous_rank = try rank(Record, nodes, previous);
            if (previous_rank < value_rank or (previous_rank == value_rank and previous < value)) break;
            record_ids[insert_index] = previous;
            insert_index -= 1;
        }
        record_ids[insert_index] = value;
    }
}

fn appendUniqueId(allocator: std.mem.Allocator, ids: *std.ArrayListUnmanaged(u64), id: u64) std.mem.Allocator.Error!void {
    if (!containsU64(ids.items, id)) {
        try ids.append(allocator, id);
    }
}

fn containsU64(items: []const u64, target: u64) bool {
    for (items) |item| {
        if (item == target) return true;
    }
    return false;
}

const TestRecord = struct {
    id: u64,
};

test "signal graph dependents are unique and mutable" {
    var records = [_]TestRecord{
        .{ .id = 0 },
        .{ .id = 1 },
        .{ .id = 2 },
        .{ .id = 3 },
    };
    var nodes = [_]Node(TestRecord){
        .{ .record = &records[0], .rank = 0 },
        .{ .record = &records[1], .rank = 1 },
        .{ .record = &records[2], .rank = 2 },
        .{ .record = &records[3], .rank = 3 },
    };
    defer {
        for (&nodes) |*node| {
            std.testing.allocator.free(node.dependents);
        }
    }

    try appendDependent(TestRecord, std.testing.allocator, &nodes, 0, 1);
    try appendDependent(TestRecord, std.testing.allocator, &nodes, 0, 1);
    try appendDependent(TestRecord, std.testing.allocator, &nodes, 0, 2);
    try std.testing.expectEqualSlices(u64, &.{ 1, 2 }, nodes[0].dependents);

    try replaceDependent(TestRecord, &nodes, 0, 2, 3);
    try std.testing.expectEqualSlices(u64, &.{ 1, 3 }, nodes[0].dependents);

    try removeDependent(TestRecord, std.testing.allocator, &nodes, 0, 1);
    try std.testing.expectEqualSlices(u64, &.{3}, nodes[0].dependents);
}

test "signal graph collects reachable dependents sorted by rank" {
    var records = [_]TestRecord{
        .{ .id = 0 },
        .{ .id = 1 },
        .{ .id = 2 },
        .{ .id = 3 },
    };
    var nodes = [_]Node(TestRecord){
        .{ .record = &records[0], .rank = 0 },
        .{ .record = &records[1], .rank = 2 },
        .{ .record = &records[2], .rank = 1 },
        .{ .record = &records[3], .rank = 3 },
    };
    defer {
        for (&nodes) |*node| {
            std.testing.allocator.free(node.dependents);
        }
    }

    try appendDependent(TestRecord, std.testing.allocator, &nodes, 0, 1);
    try appendDependent(TestRecord, std.testing.allocator, &nodes, 0, 2);
    try appendDependent(TestRecord, std.testing.allocator, &nodes, 2, 3);

    var ids: std.ArrayListUnmanaged(u64) = .empty;
    defer ids.deinit(std.testing.allocator);
    try appendReachableDependents(TestRecord, std.testing.allocator, &nodes, &ids, 0);
    try std.testing.expectEqualSlices(u64, &.{ 0, 1, 2, 3 }, ids.items);

    try sortIdsByRank(TestRecord, &nodes, ids.items);
    try std.testing.expectEqualSlices(u64, &.{ 0, 2, 1, 3 }, ids.items);
}
