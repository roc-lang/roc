const std = @import("std");
const abi = @import("roc_platform_abi.zig");
const retained_values = @import("retained_values.zig");
const scope_tree = @import("scope_tree.zig");

pub const HostValueCell = retained_values.HostValueCell;

/// Per-row payload carried in an `Ui.each` scope: the row's key and item cells,
/// keyed by the construction-site ordinal.
pub const EachRowScopeStep = struct {
    site_ordinal: u64,
    key_hash: u64,
    key: HostValueCell,
    item: HostValueCell,
};

pub const ScopeStep = scope_tree.Step(EachRowScopeStep);
pub const Scope = scope_tree.Scope(EachRowScopeStep);

pub const EachSite = struct {
    parent_scope_id: u64,
    site_ordinal: u64,
};

/// Drop the retained cells owned by an each-row scope step (no-op for the
/// structural scope kinds, which carry no Roc values).
pub fn deinitScopeStep(step: *ScopeStep, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
    switch (step.*) {
        .each_row => |*row| {
            row.key.deinit(ctx, roc_host, metrics);
            row.item.deinit(ctx, roc_host, metrics);
        },
        .root, .component, .when_branch => {},
    }
}

pub fn disposeSubtree(comptime Row: type, scopes: []scope_tree.Scope(Row), scope_id: u64, hooks: anytype) void {
    if (scope_id >= scopes.len) @panic("scope disposal referenced an unknown scope");
    if (scopes[@intCast(scope_id)].scope_id != scope_id or !scopes[@intCast(scope_id)].active) @panic("scope id has no host scope descriptor");

    var child_index: usize = 0;
    while (child_index < scopes.len) : (child_index += 1) {
        const child = scopes[child_index];
        if (!child.active) continue;
        if (child.parent_scope_id == scope_id) {
            disposeSubtree(Row, scopes, child.scope_id, hooks);
        }
    }

    hooks.deactivateNodeIdentities(scope_id);
    hooks.appendCleanupEvents(scope_id);
    hooks.cancelPendingTasks(scope_id);
    hooks.deactivateDomIdentities(scope_id);

    const scope = &scopes[@intCast(scope_id)];
    switch (scope.step) {
        .each_row => |row| hooks.removeEachRow(scope.scope_id, row.key_hash),
        .root, .component, .when_branch => {},
    }
    hooks.deinitScopeStep(&scope.step);
    scope.active = false;
    hooks.recordScopeDisposed();
}

const TestRow = struct {
    site_ordinal: u64,
    key_hash: u64,
};

const TestDisposeHooks = struct {
    node_deactivations: std.ArrayListUnmanaged(u64) = .empty,
    cleanup_events: std.ArrayListUnmanaged(u64) = .empty,
    task_cancellations: std.ArrayListUnmanaged(u64) = .empty,
    dom_deactivations: std.ArrayListUnmanaged(u64) = .empty,
    removed_rows: std.ArrayListUnmanaged(u64) = .empty,
    deinit_steps: u64 = 0,
    disposed_scopes: u64 = 0,

    fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        self.node_deactivations.deinit(allocator);
        self.cleanup_events.deinit(allocator);
        self.task_cancellations.deinit(allocator);
        self.dom_deactivations.deinit(allocator);
        self.removed_rows.deinit(allocator);
    }

    pub fn deactivateNodeIdentities(self: *@This(), scope_id: u64) void {
        self.node_deactivations.append(std.testing.allocator, scope_id) catch @panic("out of memory");
    }

    pub fn appendCleanupEvents(self: *@This(), scope_id: u64) void {
        self.cleanup_events.append(std.testing.allocator, scope_id) catch @panic("out of memory");
    }

    pub fn cancelPendingTasks(self: *@This(), scope_id: u64) void {
        self.task_cancellations.append(std.testing.allocator, scope_id) catch @panic("out of memory");
    }

    pub fn deactivateDomIdentities(self: *@This(), scope_id: u64) void {
        self.dom_deactivations.append(std.testing.allocator, scope_id) catch @panic("out of memory");
    }

    pub fn removeEachRow(self: *@This(), scope_id: u64, key_hash: u64) void {
        _ = scope_id;
        self.removed_rows.append(std.testing.allocator, key_hash) catch @panic("out of memory");
    }

    pub fn deinitScopeStep(self: *@This(), step: *scope_tree.Step(TestRow)) void {
        switch (step.*) {
            .each_row, .root, .component, .when_branch => {},
        }
        self.deinit_steps += 1;
    }

    pub fn recordScopeDisposed(self: *@This()) void {
        self.disposed_scopes += 1;
    }
};

test "scope runtime disposes active subtrees through explicit hooks" {
    var scopes: std.ArrayListUnmanaged(scope_tree.Scope(TestRow)) = .empty;
    defer scopes.deinit(std.testing.allocator);

    _ = try scope_tree.internRoot(TestRow, std.testing.allocator, &scopes);
    _ = try scope_tree.internComponent(TestRow, std.testing.allocator, &scopes, 0, 1);
    _ = try scope_tree.appendEachRow(TestRow, std.testing.allocator, &scopes, 1, .{ .site_ordinal = 4, .key_hash = 40 });
    _ = try scope_tree.internComponent(TestRow, std.testing.allocator, &scopes, 2, 1);
    _ = try scope_tree.internComponent(TestRow, std.testing.allocator, &scopes, 0, 2);

    var hooks = TestDisposeHooks{};
    defer hooks.deinit(std.testing.allocator);
    disposeSubtree(TestRow, scopes.items, 1, &hooks);

    try std.testing.expect(scopes.items[0].active);
    try std.testing.expect(!scopes.items[1].active);
    try std.testing.expect(!scopes.items[2].active);
    try std.testing.expect(!scopes.items[3].active);
    try std.testing.expect(scopes.items[4].active);
    try std.testing.expectEqualSlices(u64, &.{ 3, 2, 1 }, hooks.node_deactivations.items);
    try std.testing.expectEqualSlices(u64, &.{40}, hooks.removed_rows.items);
    try std.testing.expectEqual(@as(u64, 3), hooks.deinit_steps);
    try std.testing.expectEqual(@as(u64, 3), hooks.disposed_scopes);
}
