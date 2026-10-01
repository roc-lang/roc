//! Sparse directed effect solving over the checker's explicit function formulas.
//!
//! A session borrows an immutable type graph. Discovery assigns dense slots only
//! to reachable variables; reverse edges propagate each positive or unresolved
//! fact to callers. Cycles require no recursion and do not create effects.
//! Reset before type mutations; storage capacity survives between sessions.

const std = @import("std");
const collections = @import("collections");
const types = @import("types");

pub const State = enum(u2) { pure, unresolved, effectful };
const SlotId = enum(u32) { _ };
const no_edge = std.math.maxInt(u32);
const Slot = struct {
    var_: types.Var,
    state: State = .pure,
    callers: u32 = no_edge,
};
const Edge = struct { caller: SlotId, next: u32 };

pub const Solver = struct {
    gpa: std.mem.Allocator,
    slot_by_var: collections.DenseMap(types.Var, SlotId),
    slots: std.ArrayList(Slot) = .empty,
    edges: std.ArrayList(Edge) = .empty,
    worklist: std.ArrayList(SlotId) = .empty,
    discovered: usize = 0,

    pub fn init(gpa: std.mem.Allocator) Solver {
        return .{ .gpa = gpa, .slot_by_var = .init(gpa) };
    }

    pub fn deinit(self: *Solver) void {
        self.slot_by_var.deinit();
        self.slots.deinit(self.gpa);
        self.edges.deinit(self.gpa);
        self.worklist.deinit(self.gpa);
    }

    pub fn reset(self: *Solver) void {
        self.slot_by_var.clearRetainingCapacity();
        self.slots.clearRetainingCapacity();
        self.edges.clearRetainingCapacity();
        self.worklist.clearRetainingCapacity();
        self.discovered = 0;
    }

    /// Read an already solved root from the immutable session snapshot.
    /// Boundary consumers use this after discovery, before resetting the session.
    pub fn solvedState(self: *const Solver, resolved_var: types.Var) State {
        const id = self.slot_by_var.get(resolved_var).?;
        return self.slots.items[@intFromEnum(id)].state;
    }

    fn slot(self: *Solver, store: *const types.Store, var_: types.Var) std.mem.Allocator.Error!SlotId {
        const root = store.resolveVar(var_).var_;
        if (self.slot_by_var.get(root)) |id| return id;
        const id: SlotId = @enumFromInt(self.slots.items.len);
        try self.slots.append(self.gpa, .{ .var_ = root });
        try self.slot_by_var.put(root, id);
        return id;
    }

    fn promote(self: *Solver, id: SlotId, state: State) std.mem.Allocator.Error!void {
        const current = &self.slots.items[@intFromEnum(id)].state;
        if (@intFromEnum(state) <= @intFromEnum(current.*)) return;
        try self.worklist.append(self.gpa, id);
        current.* = state;
    }

    fn depend(self: *Solver, store: *const types.Store, caller: SlotId, var_: types.Var) std.mem.Allocator.Error!void {
        const callee = try self.slot(store, var_);
        const callee_index = @intFromEnum(callee);
        const edge_index: u32 = @intCast(self.edges.items.len);
        try self.edges.append(self.gpa, .{ .caller = caller, .next = self.slots.items[callee_index].callers });
        self.slots.items[callee_index].callers = edge_index;
        try self.promote(caller, self.slots.items[callee_index].state);
    }

    /// Multiple queries share discovery and propagation only while the source
    /// store is unchanged. Allocation failure discards the session as well.
    pub fn resolve(self: *Solver, store: *const types.Store, var_: types.Var) std.mem.Allocator.Error!State {
        errdefer self.reset();
        switch (store.resolveVar(var_).desc.content) {
            .alias => {},
            .err, .field_presence => return .pure,
            .flex, .rigid => return .unresolved,
            .structure => |flat| switch (flat) {
                .fn_pure => |func| {
                    std.debug.assert(func.effect_deps.len() == 0);
                    return .pure;
                },
                .fn_effectful => return .effectful,
                .fn_unbound => |func| {
                    if (func.effect_deps.len() == 0) return .unresolved;
                },
                .record, .tuple, .nominal_type, .empty_record, .tag_union, .empty_tag_union => return .pure,
            },
        }
        const root = try self.slot(store, var_);
        while (self.discovered < self.slots.items.len) {
            const id: SlotId = @enumFromInt(self.discovered);
            const content = store.resolveVar(self.slots.items[self.discovered].var_).desc.content;
            self.discovered += 1;
            switch (content) {
                .alias => |alias| try self.depend(store, id, store.getAliasBackingVar(alias)),
                .flex, .rigid => try self.promote(id, .unresolved),
                .err, .field_presence => {},
                .structure => |flat| switch (flat) {
                    .fn_pure => |func| std.debug.assert(func.effect_deps.len() == 0),
                    .fn_effectful => try self.promote(id, .effectful),
                    .fn_unbound => |func| {
                        if (func.effect_deps.len() == 0) try self.promote(id, .unresolved);
                        for (store.sliceVars(func.effect_deps)) |dep| try self.depend(store, id, dep);
                    },
                    .record, .tuple, .nominal_type, .empty_record, .tag_union, .empty_tag_union => {},
                },
            }
        }
        var next: usize = 0;
        while (next < self.worklist.items.len) : (next += 1) {
            const id = self.worklist.items[next];
            const state = self.slots.items[@intFromEnum(id)].state;
            var edge_index = self.slots.items[@intFromEnum(id)].callers;
            while (edge_index != no_edge) {
                const edge = self.edges.items[edge_index];
                try self.promote(edge.caller, state);
                edge_index = edge.next;
            }
        }
        self.worklist.clearRetainingCapacity();
        return self.slots.items[@intFromEnum(root)].state;
    }
};

test "effect solver propagates through every member of a cycle and keeps callers directed" {
    const gpa = std.testing.allocator;
    var store = try types.Store.init(gpa);
    defer store.deinit();
    const ret = try store.fresh();
    const a = try store.fresh();
    const b = try store.fresh();
    const effect = try store.freshFromContent(try store.mkFuncEffectful(&.{}, ret));
    try store.setVarContent(a, try store.mkFuncUnboundWithEffectDeps(&.{}, ret, &.{ b, effect }));
    try store.setVarContent(b, try store.mkFuncUnboundWithEffectDeps(&.{}, ret, &.{a}));
    var solver = Solver.init(gpa);
    defer solver.deinit();
    try std.testing.expectEqual(State.effectful, try solver.resolve(&store, a));
    try std.testing.expectEqual(State.effectful, try solver.resolve(&store, b));

    const pure = try store.freshFromContent(try store.mkFuncPure(&.{}, ret));
    const caller = try store.freshFromContent(try store.mkFuncUnboundWithEffectDeps(&.{}, ret, &.{ pure, effect }));
    solver.reset();
    try std.testing.expectEqual(State.effectful, try solver.resolve(&store, caller));
    try std.testing.expectEqual(State.pure, try solver.resolve(&store, pure));
}

test "effect solver preserves unresolved cycles and reads mutations only in a new session" {
    const gpa = std.testing.allocator;
    var store = try types.Store.init(gpa);
    defer store.deinit();
    const ret = try store.fresh();
    const a = try store.fresh();
    const b = try store.fresh();
    const unknown = try store.freshFromContent(try store.mkFuncUnbound(&.{}, ret));
    try store.setVarContent(a, try store.mkFuncUnboundWithEffectDeps(&.{}, ret, &.{ b, unknown }));
    try store.setVarContent(b, try store.mkFuncUnboundWithEffectDeps(&.{}, ret, &.{a}));
    var solver = Solver.init(gpa);
    defer solver.deinit();
    try std.testing.expectEqual(State.unresolved, try solver.resolve(&store, a));
    try std.testing.expectEqual(State.unresolved, try solver.resolve(&store, b));
    try store.setVarContent(unknown, try store.mkFuncPure(&.{}, ret));
    solver.reset();
    try std.testing.expectEqual(State.pure, try solver.resolve(&store, a));
    try std.testing.expectEqual(State.pure, try solver.resolve(&store, b));
}

test "effect solver handles deep chains with linear discovery and bounded work" {
    const gpa = std.testing.allocator;
    var store = try types.Store.init(gpa);
    defer store.deinit();
    const ret = try store.fresh();
    var root = try store.freshFromContent(try store.mkFuncEffectful(&.{}, ret));
    for (0..10000) |_| {
        root = try store.freshFromContent(try store.mkFuncUnboundWithEffectDeps(&.{}, ret, &.{root}));
    }
    var solver = Solver.init(gpa);
    defer solver.deinit();
    try std.testing.expectEqual(State.effectful, try solver.resolve(&store, root));
    try std.testing.expectEqual(@as(usize, 10001), solver.slots.items.len);
    try std.testing.expectEqual(@as(usize, 10000), solver.edges.items.len);
    try std.testing.expectEqual(State.effectful, try solver.resolve(&store, root));
    try std.testing.expectEqual(@as(usize, 10000), solver.edges.items.len);
}

test "effect solver terminal queries allocate no scratch" {
    const gpa = std.testing.allocator;
    var store = try types.Store.init(gpa);
    defer store.deinit();
    const ret = try store.fresh();
    const pure = try store.freshFromContent(try store.mkFuncPure(&.{}, ret));
    const effect = try store.freshFromContent(try store.mkFuncEffectful(&.{}, ret));
    const unknown = try store.freshFromContent(try store.mkFuncUnbound(&.{}, ret));
    var failing = std.testing.FailingAllocator.init(gpa, .{ .fail_index = 0 });
    var solver = Solver.init(failing.allocator());
    defer solver.deinit();
    try std.testing.expectEqual(State.pure, try solver.resolve(&store, pure));
    try std.testing.expectEqual(State.effectful, try solver.resolve(&store, effect));
    try std.testing.expectEqual(State.unresolved, try solver.resolve(&store, unknown));
    try std.testing.expectEqual(@as(usize, 0), solver.slots.items.len);
    try std.testing.expect(!failing.has_induced_failure);
}
