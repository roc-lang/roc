const std = @import("std");

pub const Error = error{
    OutOfMemory,
};

pub const NodeIdentity = struct {
    node_id: u64,
    scope_id: u64,
    ordinal: u64,
    active: bool,
};

pub const DomIdentity = struct {
    elem_id: u64,
    scope_id: u64,
    ordinal: u64,
    active: bool,
};

pub fn internNode(allocator: std.mem.Allocator, identities: *std.ArrayListUnmanaged(NodeIdentity), scope_id: u64, ordinal: u64) Error!u64 {
    for (identities.items) |identity| {
        if (!identity.active) continue;
        if (identity.scope_id == scope_id and identity.ordinal == ordinal) {
            return identity.node_id;
        }
    }

    const node_id: u64 = @intCast(identities.items.len);
    identities.append(allocator, .{
        .node_id = node_id,
        .scope_id = scope_id,
        .ordinal = ordinal,
        .active = true,
    }) catch return Error.OutOfMemory;
    return node_id;
}

pub fn internDom(allocator: std.mem.Allocator, identities: *std.ArrayListUnmanaged(DomIdentity), scope_id: u64, ordinal: u64) Error!u64 {
    for (identities.items) |identity| {
        if (!identity.active) continue;
        if (identity.scope_id == scope_id and identity.ordinal == ordinal) {
            return identity.elem_id;
        }
    }

    const elem_id: u64 = @intCast(identities.items.len + 1);
    identities.append(allocator, .{
        .elem_id = elem_id,
        .scope_id = scope_id,
        .ordinal = ordinal,
        .active = true,
    }) catch return Error.OutOfMemory;
    return elem_id;
}

test "node identities reuse active scope ordinal pairs" {
    var identities: std.ArrayListUnmanaged(NodeIdentity) = .empty;
    defer identities.deinit(std.testing.allocator);

    const first = try internNode(std.testing.allocator, &identities, 7, 0);
    const same = try internNode(std.testing.allocator, &identities, 7, 0);
    const next = try internNode(std.testing.allocator, &identities, 7, 1);

    try std.testing.expectEqual(@as(u64, 0), first);
    try std.testing.expectEqual(first, same);
    try std.testing.expectEqual(@as(u64, 1), next);

    identities.items[@intCast(first)].active = false;
    const recreated = try internNode(std.testing.allocator, &identities, 7, 0);
    try std.testing.expectEqual(@as(u64, 2), recreated);
}

test "dom identities are one-based and reuse active scope ordinal pairs" {
    var identities: std.ArrayListUnmanaged(DomIdentity) = .empty;
    defer identities.deinit(std.testing.allocator);

    const first = try internDom(std.testing.allocator, &identities, 2, 0);
    const same = try internDom(std.testing.allocator, &identities, 2, 0);
    const next = try internDom(std.testing.allocator, &identities, 2, 1);

    try std.testing.expectEqual(@as(u64, 1), first);
    try std.testing.expectEqual(first, same);
    try std.testing.expectEqual(@as(u64, 2), next);
}
