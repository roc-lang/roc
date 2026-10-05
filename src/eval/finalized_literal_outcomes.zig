//! Producer-owned literal completion facts retained beyond evaluator teardown.
//!
//! These facts certify evaluation, not object-cache admission. Admission also
//! requires an explicit owning specialization and the existing typed frozen
//! value and relocation contracts. Source identities resolve against the
//! current checked artifacts; no session-local location or pointer persists.

const std = @import("std");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const Facts = lir.LIR.FinalizedLiteralOutcomes;
pub const SourceExprId = Facts.SourceExprId;
pub const RootIdentity = Facts.RootIdentity;
pub const ReportAuthority = Facts.ReportAuthority;
pub const Outcome = Facts.Outcome;
pub const Record = Facts.Record;
pub const DebugEvent = Facts.DebugEvent;

/// Classify the explicit result-plan graph, including nested and recursive
/// containers. Reverse propagation is linear in nodes and edges; shared roots
/// do not repeat walks. Null denotes an incomplete representation proof.
/// Call only when the active runtime cache context retains completion facts.
pub fn callableResultPlans(allocator: Allocator, plans: []const lir.Program.ConstPlan) Allocator.Error![]?bool {
    const states = try allocator.alloc(?bool, plans.len);
    errdefer allocator.free(states);
    const heads = try allocator.alloc(?usize, plans.len);
    defer allocator.free(heads);
    @memset(heads, null);
    const Edge = struct { parent: lir.Program.ConstPlanId, next: ?usize };
    var edges = std.ArrayList(Edge).empty;
    defer edges.deinit(allocator);
    const Parents = struct {
        allocator: Allocator,
        states: []?bool,
        heads: []?usize,
        edges: *std.ArrayList(Edge),
        parent: lir.Program.ConstPlanId,

        fn add(self: *@This(), child: lir.Program.ConstPlanId) Allocator.Error!void {
            const index = @intFromEnum(child);
            if (index >= self.states.len) {
                self.states[@intFromEnum(self.parent)] = null;
                return;
            }
            try self.edges.append(self.allocator, .{ .parent = self.parent, .next = self.heads[index] });
            self.heads[index] = self.edges.items.len - 1;
        }
    };
    for (plans, 0..) |plan, index| {
        states[index] = switch (plan) {
            .pending, .layout_only => null,
            .fn_value, .erased_fn => true,
            else => false,
        };
        var parents = Parents{
            .allocator = allocator,
            .states = states,
            .heads = heads,
            .edges = &edges,
            .parent = @enumFromInt(index),
        };
        switch (plan) {
            .list, .box => |child| try parents.add(child),
            .boxy_box => |box| try parents.add(box.payload),
            .tuple, .record => |children| for (children) |child| try parents.add(child),
            .tag_union => |variants| for (variants) |variant| {
                for (variant.payloads) |child| try parents.add(child);
            },
            .named => |named| try parents.add(named.backing),
            .pending, .layout_only, .zst, .scalar, .str, .fn_value, .erased_fn => {},
        }
    }
    var work = std.ArrayList(lir.Program.ConstPlanId).empty;
    defer work.deinit(allocator);
    for (states, 0..) |state, index| {
        if (state != false) try work.append(allocator, @enumFromInt(index));
    }
    while (work.pop()) |child| {
        const state = states[@intFromEnum(child)];
        var incoming = heads[@intFromEnum(child)];
        while (incoming) |edge_index| {
            const edge = edges.items[edge_index];
            const parent = &states[@intFromEnum(edge.parent)];
            // Callable dominates unknown. Each node changes at most twice,
            // including cycles with an unknown node and a callable descendant.
            if ((state == true and parent.* != true) or (state == null and parent.* == false)) {
                parent.* = state;
                try work.append(allocator, edge.parent);
            }
            incoming = edge.next;
        }
    }
    return states;
}

/// Message slices own separate allocations, so later appends cannot invalidate
/// a completed record. The evaluator's borrowed messages never escape.
pub const Store = struct {
    records: std.ArrayList(Record) = .empty,
    /// Session-local bridge to runtime-use provenance. Never serialized.
    record_roots: std.ArrayList(?lir.LIR.LiteralRootId) = .empty,
    record_owners: std.ArrayList(?Facts.OwnerId) = .empty,
    debug_events: std.ArrayList(DebugEvent) = .empty,
    expect_roots: std.ArrayList(RootIdentity) = .empty,

    pub fn deinit(self: *Store, allocator: Allocator) void {
        for (self.records.items) |record| {
            switch (record.outcome) {
                .rejected => |rejection| allocator.free(rejection.message),
                else => {},
            }
        }
        for (self.debug_events.items) |event| allocator.free(event.message);
        self.records.deinit(allocator);
        self.record_roots.deinit(allocator);
        self.record_owners.deinit(allocator);
        self.debug_events.deinit(allocator);
        self.expect_roots.deinit(allocator);
        self.* = .{};
    }

    pub fn append(self: *Store, allocator: Allocator, input: Record) Allocator.Error!void {
        try self.appendForRoot(allocator, null, null, input);
    }

    pub fn appendForRoot(self: *Store, allocator: Allocator, root: ?lir.LIR.LiteralRootId, owner: ?Facts.OwnerId, input: Record) Allocator.Error!void {
        try self.records.ensureUnusedCapacity(allocator, 1);
        try self.record_roots.ensureUnusedCapacity(allocator, 1);
        try self.record_owners.ensureUnusedCapacity(allocator, 1);
        var record = input;
        record.has_debug_observation = input.has_debug_observation or self.hasDebug(input.root);
        record.has_expect_observation = input.has_expect_observation or self.hasExpect(input.root);
        if (record.outcome == .rejected) {
            record.outcome.rejected.message = try allocator.dupe(u8, input.outcome.rejected.message);
        }
        self.records.appendAssumeCapacity(record);
        self.record_roots.appendAssumeCapacity(root);
        self.record_owners.appendAssumeCapacity(owner);
    }

    pub fn appendDebug(self: *Store, allocator: Allocator, root: RootIdentity, message: []const u8) Allocator.Error!void {
        const owned = try allocator.dupe(u8, message);
        errdefer allocator.free(owned);
        try self.debug_events.append(allocator, .{ .root = root, .message = owned });
    }

    pub fn recordExpect(self: *Store, allocator: Allocator, root: RootIdentity) Allocator.Error!void {
        if (!self.hasExpect(root)) try self.expect_roots.append(allocator, root);
    }

    /// Demand only complete early owners, not an arbitrary owner of a shared
    /// root. One unsupported outcome withholds that owner, never its peers.
    /// `scope` and retained scope tokens borrow the still-live immutable
    /// prepared function table; this method is not valid after its destruction.
    pub fn publicationRequests(self: *const Store, allocator: Allocator, scope: *const anyopaque) Allocator.Error![]Facts.PublicationRequest {
        const State = struct { key: ?[32]u8, eligible: bool };
        var owners = std.AutoHashMap(Facts.OwnerFunctionId, State).init(allocator);
        defer owners.deinit();
        for (self.records.items) |record| {
            if (record.owner_scope != scope) continue;
            const owner = record.owner_fn orelse continue;
            const entry = try owners.getOrPut(owner);
            if (!entry.found_existing) {
                entry.value_ptr.* = .{ .key = record.specialization_key, .eligible = true };
            } else if (!std.meta.eql(entry.value_ptr.key, record.specialization_key)) {
                std.debug.panic("literal owner function changed its declared specialization identity", .{});
            }
            entry.value_ptr.eligible = entry.value_ptr.eligible and record.portableSuccess() != null;
        }
        var requests = std.ArrayList(Facts.PublicationRequest).empty;
        errdefer requests.deinit(allocator);
        var entries = owners.iterator();
        while (entries.next()) |entry| {
            if (entry.value_ptr.eligible) try requests.append(allocator, .{
                .owner_fn = entry.key_ptr.*,
                .owner_scope = scope,
                .specialization_key = entry.value_ptr.key.?,
            });
        }
        std.mem.sort(Facts.PublicationRequest, requests.items, {}, struct {
            fn less(_: void, left: Facts.PublicationRequest, right: Facts.PublicationRequest) bool {
                return @intFromEnum(left.owner_fn) < @intFromEnum(right.owner_fn);
            }
        }.less);
        return requests.toOwnedSlice(allocator);
    }

    fn hasDebug(self: *const Store, root: RootIdentity) bool {
        for (self.debug_events.items) |event| {
            if (event.root.eql(root)) return true;
        }
        return false;
    }

    fn hasExpect(self: *const Store, root: RootIdentity) bool {
        for (self.expect_roots.items) |observed| {
            if (observed.eql(root)) return true;
        }
        return false;
    }
};

test "finalized literal publication demands complete direct owners without withholding shared peers" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testPublicationRequests, .{});
}

fn testPublicationRequests(allocator: Allocator) !void {
    var store = Store{};
    defer store.deinit(allocator);
    const root: RootIdentity = .{
        .source = .{ .module = .{ .bytes = [_]u8{7} ** 32 }, .expr = @enumFromInt(3) },
        .procedure = [_]u8{8} ** 32,
    };
    const scope_marker: u8 = 0;
    const scope: *const anyopaque = &scope_marker;
    var record: Record = .{ .root = root, .owner_fn = @enumFromInt(0), .owner_scope = scope, .specialization_key = [_]u8{9} ** 32, .outcome = .success, .has_callable_result = false };
    try store.append(allocator, record);
    try store.append(allocator, record);
    record.owner_fn = @enumFromInt(1);
    record.specialization_key = [_]u8{10} ** 32;
    try store.append(allocator, record);
    record.has_callable_result = true;
    try store.append(allocator, record);
    record.owner_fn = @enumFromInt(2);
    record.specialization_key = null;
    record.has_callable_result = false;
    try store.append(allocator, record);
    record.owner_fn = @enumFromInt(3);
    record.specialization_key = [_]u8{11} ** 32;
    record.has_debug_observation = true;
    try store.append(allocator, record);
    record.owner_fn = null;
    record.has_debug_observation = false;
    try store.append(allocator, record);
    record.owner_fn = @enumFromInt(0);
    const other_scope_marker: u8 = 1;
    record.owner_scope = &other_scope_marker;
    try store.append(allocator, record);
    record.owner_scope = null;
    try store.append(allocator, record);
    const requests = try store.publicationRequests(allocator, scope);
    defer allocator.free(requests);
    try std.testing.expectEqualDeep(&[_]Facts.PublicationRequest{.{ .owner_fn = @enumFromInt(0), .owner_scope = scope, .specialization_key = [_]u8{9} ** 32 }}, requests);
}

test "finalized literal result proof rejects direct nested cyclic and incomplete callable plans" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testCallableResultPlans, .{});
}

fn testCallableResultPlans(allocator: Allocator) !void {
    const id = struct {
        fn of(index: u32) lir.Program.ConstPlanId {
            return @enumFromInt(index);
        }
    }.of;
    const plans = [_]lir.Program.ConstPlan{
        .scalar,
        .{ .fn_value = @enumFromInt(0) },
        .{ .record = &.{ id(0), id(1) } },
        .{ .list = id(4) },
        .{ .named = .{ .named_type = std.mem.zeroes(@import("check").CheckedModule.ConstNamedType), .backing = id(3) } },
        .{ .tuple = &.{ id(4), id(2) } },
        .pending,
        .{ .box = id(6) },
        .{ .tuple = &.{ id(7), id(1) } },
        .{ .list = id(9) },
        .{ .erased_fn = @enumFromInt(0) },
        .{ .tag_union = &.{.{ .name = "Some", .checked_name = @enumFromInt(0), .discriminant = 0, .payloads = &.{ id(9), id(10) } }} },
        .layout_only,
        .{ .boxy_box = .{ .payload = id(7), .layout_idx = .zst } },
        .{ .list = id(99) },
        .{ .record = &.{ id(16), id(1) } },
        .{ .box = id(15) },
    };
    const result = try callableResultPlans(allocator, &plans);
    defer allocator.free(result);
    try std.testing.expectEqualSlices(?bool, &.{
        false, true, true, false, false, true, null, null, true,
        false, true, true, null,  null,  null, true, true,
    }, result);
}

test "finalized literal outcome facts own messages and distinguish exact producers" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testOwnedFacts, .{});
}

fn testOwnedFacts(allocator: Allocator) !void {
    var store = Store{};
    defer store.deinit(allocator);
    const source: SourceExprId = .{ .module = .{ .bytes = [_]u8{3} ** 32, .source_hash = [_]u8{4} ** 32 }, .expr = @enumFromInt(7) };
    const first: RootIdentity = .{ .source = source, .procedure = [_]u8{1} ** 32 };
    const second: RootIdentity = .{ .source = source, .procedure = [_]u8{2} ** 32 };
    var compact_first = first;
    compact_first.source.module = .{ .bytes = first.source.module.bytes };
    var compact_second = second;
    compact_second.source.module = .{ .bytes = second.source.module.bytes };
    var message = [_]u8{ 'b', 'a', 'd' };
    try store.appendDebug(allocator, compact_first, &message);
    try store.recordExpect(allocator, compact_second);
    try store.recordExpect(allocator, second);
    try store.append(allocator, .{ .root = first, .outcome = .{ .rejected = .{
        .source = source,
        .kind = .quote,
        .message = &message,
        .producer_report_authority = .specialization,
    } } });
    try store.append(allocator, .{ .root = second, .outcome = .success });
    @memset(&message, 'x');
    try std.testing.expectEqualStrings("bad", store.records.items[0].outcome.rejected.message);
    try std.testing.expectEqualStrings("bad", store.debug_events.items[0].message);
    try std.testing.expect(store.records.items[0].has_debug_observation);
    try std.testing.expect(!store.records.items[1].has_debug_observation);
    try std.testing.expect(!store.records.items[0].has_expect_observation);
    try std.testing.expect(store.records.items[1].has_expect_observation);
    try std.testing.expectEqual(@as(usize, 1), store.expect_roots.items.len);
    try std.testing.expect(store.records.items[0].specialization_key == null);
    try std.testing.expect(!std.meta.eql(store.records.items[0].root, store.records.items[1].root));
}

test "finalized failed facts preserve report authority and stable propagation" {
    const allocator = std.testing.allocator;
    var store = Store{};
    defer store.deinit(allocator);
    const source: SourceExprId = .{ .module = .{ .bytes = [_]u8{4} ** 32 }, .expr = @enumFromInt(8) };
    const producer: RootIdentity = .{ .source = source, .procedure = [_]u8{5} ** 32 };
    const consumer: RootIdentity = .{ .source = source, .procedure = [_]u8{6} ** 32 };
    try store.append(allocator, .{ .root = producer, .outcome = .{ .rejected = .{
        .source = source,
        .kind = .quote,
        .message = "rejected",
        .producer_report_authority = .checked_root,
    } } });
    try store.append(allocator, .{ .root = consumer, .outcome = .{ .literal_failure = producer } });
    try std.testing.expectEqual(ReportAuthority.checked_root, store.records.items[0].outcome.rejected.producer_report_authority);
    try std.testing.expectEqualDeep(producer, store.records.items[1].outcome.literal_failure);
}
