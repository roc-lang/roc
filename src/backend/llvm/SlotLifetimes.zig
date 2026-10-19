//! Where a procedure's LLVM local slots begin and end their lifetimes.
//!
//! Every fixed-size slot lives in the entry block, so without lifetime markers
//! LLVM keeps each one for the whole activation and a frame grows with the sum
//! of its slots instead of their peak. LLVM's stack coloring overlaps slots
//! only through `llvm.lifetime.start` / `llvm.lifetime.end`. SROA already
//! removes slots whose address never escapes, and a marker on such a slot is
//! pure optimizer work, so markers go only on the slots of locals whose
//! addresses the backend passes to out-of-line Roc procedure calls.
//!
//! The markers are `lir.StackPlan`'s lifetime boundaries over the explicit LIR
//! control-flow graph, each emitted at the entry of its statement. An end may
//! be reached along a path on which the slot is already dead, which LLVM
//! defines as having no effect.
//!
//! Locals the backend reads or writes outside the statements LIR says access
//! them keep their activation lifetime: procedure arguments, which the
//! prologue writes, and every local that takes part in a deferred string-match
//! capture, which the backend materializes lazily.
const std = @import("std");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const CFStmtId = lir.CFStmtId;
const LocalId = lir.LocalId;
const LirStore = lir.LirStore;
const GuardedList = LirStore.GuardedList;
const StackPlan = lir.StackPlan;
const none = std.math.maxInt(u32);
const Self = @This();

/// One lifetime intrinsic to emit on a local's slot.
pub const Marker = struct {
    kind: StackPlan.Boundary.Kind,
    local: LocalId,
};

const Entry = struct {
    stmt: u32,
    marker: Marker,

    fn lessThan(_: void, a: Entry, b: Entry) bool {
        if (a.stmt != b.stmt) return a.stmt < b.stmt;
        if (a.marker.kind != b.marker.kind) return @backingInt(a.marker.kind) < @backingInt(b.marker.kind);
        return @backingInt(a.marker.local) < @backingInt(b.marker.local);
    }
};

const Span = struct { start: u32, len: u32 };

allocator: Allocator,
indexes: StackPlan.Indexes,
/// `StackPlan`'s local index column, kept empty between procedures.
local_indices: std.ArrayList(u32) = .empty,
/// Locals whose slot addresses reach an out-of-line call.
candidates: std.AutoHashMapUnmanaged(u32, void) = .empty,
/// Locals taking part in deferred string-match captures.
deferred: std.AutoHashMapUnmanaged(u32, void) = .empty,
/// Copies between locals, as (source, target).
copies: std.ArrayList([2]u32) = .empty,
visited: std.AutoHashMapUnmanaged(u32, void) = .empty,
work: std.ArrayList(CFStmtId) = .empty,
planned: std.ArrayList(CFStmtId) = .empty,
entries: std.ArrayList(Entry) = .empty,
markers: std.ArrayList(Marker) = .empty,
by_stmt: std.AutoHashMapUnmanaged(u32, Span) = .empty,
node_stmts: std.ArrayList(u32) = .empty,
boundaries: std.ArrayList(StackPlan.Boundary) = .empty,

pub fn init(allocator: Allocator) Self {
    return .{ .allocator = allocator, .indexes = StackPlan.Indexes.init(allocator) };
}

pub fn deinit(self: *Self) void {
    self.indexes.deinit();
    self.local_indices.deinit(self.allocator);
    self.candidates.deinit(self.allocator);
    self.deferred.deinit(self.allocator);
    self.copies.deinit(self.allocator);
    self.visited.deinit(self.allocator);
    self.work.deinit(self.allocator);
    self.planned.deinit(self.allocator);
    self.entries.deinit(self.allocator);
    self.markers.deinit(self.allocator);
    self.by_stmt.deinit(self.allocator);
    self.node_stmts.deinit(self.allocator);
    self.boundaries.deinit(self.allocator);
}

/// Drop the current procedure's markers.
pub fn clear(self: *Self) void {
    self.markers.clearRetainingCapacity();
    self.by_stmt.clearRetainingCapacity();
}

/// The markers to emit at the entry of `stmt`, ends first.
pub fn at(self: *const Self, stmt: CFStmtId) []const Marker {
    const span = self.by_stmt.get(@backingInt(stmt)) orelse return &.{};
    return self.markers.items[span.start..][0..span.len];
}

/// Plan the markers of the procedure body rooted at `root` whose parameter
/// span is `params`. `calls.appendAddressed(call, list)` appends the locals
/// whose slot addresses one `assign_call` passes to an out-of-line callee, and
/// fails only with `calls.AppendError`.
pub fn plan(self: *Self, store: *const LirStore, root: CFStmtId, params: anytype, calls: anytype) (Allocator.Error || @TypeOf(calls).AppendError)!void {
    self.clear();
    try self.collectCandidates(store, root, params, calls);
    if (self.candidates.count() == 0) return;

    var stack_plan = StackPlan.initWithLocalIndices(self.allocator, self.local_indices);
    self.local_indices = .empty;
    defer self.local_indices = stack_plan.deinitKeepingLocalIndices();
    self.planned.clearRetainingCapacity();
    defer self.indexes.clear(self.planned.items);
    try stack_plan.addProcGraph(store, &self.indexes, root, &self.planned, Tracked{ .candidates = &self.candidates });
    for (stack_plan.values.items) |*value| value.size = 1;
    try stack_plan.solveLifetimes();
    try self.placeMarkers(&stack_plan, root);
}

const Tracked = struct {
    candidates: *const std.AutoHashMapUnmanaged(u32, void),

    pub fn tracks(self: Tracked, local: LocalId) bool {
        return self.candidates.contains(@backingInt(local));
    }

    pub fn statement(_: Tracked, _: *StackPlan, _: u32, _: lir.LIR.CFStmt) Allocator.Error!void {}
};

fn collectCandidates(self: *Self, store: *const LirStore, root: CFStmtId, params: anytype, calls: anytype) (Allocator.Error || @TypeOf(calls).AppendError)!void {
    self.candidates.clearRetainingCapacity();
    self.deferred.clearRetainingCapacity();
    self.copies.clearRetainingCapacity();
    self.visited.clearRetainingCapacity();
    self.work.clearRetainingCapacity();
    var addressed: std.ArrayList(LocalId) = .empty;
    defer addressed.deinit(self.allocator);
    try self.work.append(self.allocator, root);
    while (self.work.pop()) |id| {
        if ((try self.visited.getOrPut(self.allocator, @backingInt(id))).found_existing) continue;
        switch (store.getCFStmt(id)) {
            .assign_call => |call| {
                addressed.clearRetainingCapacity();
                try calls.appendAddressed(call, &addressed);
                for (addressed.items) |local| try self.candidates.put(self.allocator, @backingInt(local), {});
            },
            .assign_ref => |assign| switch (assign.op) {
                .local => |source| try self.copies.append(self.allocator, .{ @backingInt(source), @backingInt(assign.target) }),
                inline .list_reinterpret, .nominal => |ref| try self.copies.append(self.allocator, .{ @backingInt(ref.backing_ref), @backingInt(assign.target) }),
                .discriminant, .field, .tag_payload, .tag_payload_struct => {},
            },
            .set_local => |assign| try self.copies.append(self.allocator, .{ @backingInt(assign.value), @backingInt(assign.target) }),
            .str_match => |s| try self.noteDeferredCaptures(store, s.source, s.steps),
            .str_match_set => |s| {
                const arms = store.getStrMatchArms(s.arms);
                for (0..arms.len) |i| try self.noteDeferredCaptures(store, s.source, GuardedList.at(arms, i).steps);
            },
            .init_uninitialized,
            .assign_literal,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_record_update,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_hash,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .boxy_tag_match,
            .assign_call_dict,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .debug,
            .expect,
            .expect_err,
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .comptime_branch_taken,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            .switch_stmt,
            .switch_initialized_payload,
            .join,
            .jump,
            .ret,
            .crash,
            => {},
        }
        try lir.BodyClone.appendSuccessors(store, &self.work, id, self.allocator);
    }
    if (self.candidates.count() == 0) return;

    // A copy of a deferred capture defers too (`copyLocal`).
    var changed = self.deferred.count() != 0;
    while (changed) {
        changed = false;
        for (self.copies.items) |copy| {
            if (self.deferred.contains(copy[0]) and !(try self.deferred.getOrPut(self.allocator, copy[1])).found_existing) changed = true;
        }
    }
    var it = self.deferred.keyIterator();
    while (it.next()) |local| _ = self.candidates.remove(local.*);
    for (0..params.len) |i| _ = self.candidates.remove(@backingInt(GuardedList.at(params, i)));
}

fn noteDeferredCaptures(self: *Self, store: *const LirStore, source: LocalId, steps: lir.LIR.StrMatchStepSpan) Allocator.Error!void {
    try self.deferred.put(self.allocator, @backingInt(source), {});
    const captures = store.getStrMatchSteps(steps);
    for (0..captures.len) |i| switch (GuardedList.at(captures, i).capture) {
        .discard => {},
        .view => |view| try self.deferred.put(self.allocator, @backingInt(view), {}),
    };
}

fn placeMarkers(self: *Self, stack_plan: *const StackPlan, root: CFStmtId) Allocator.Error!void {
    try self.node_stmts.resize(self.allocator, stack_plan.nodes.items.len);
    @memset(self.node_stmts.items, none);
    for (self.planned.items) |stmt| self.node_stmts.items[self.indexes.nodeOf(stmt).?] = @backingInt(stmt);
    self.boundaries.clearRetainingCapacity();
    try stack_plan.appendLifetimeBoundaries(self.indexes.nodeOf(root).?, &self.boundaries);

    self.entries.clearRetainingCapacity();
    for (self.boundaries.items) |boundary| {
        const local = stack_plan.values.items[boundary.value].local;
        const stmt = switch (boundary.kind) {
            .start => self.node_stmts.items[boundary.node],
            .end => self.statementEntry(stack_plan, boundary.node),
        };
        std.debug.assert(stmt != none);
        try self.entries.append(self.allocator, .{ .stmt = stmt, .marker = .{ .kind = boundary.kind, .local = local } });
    }

    std.mem.sort(Entry, self.entries.items, {}, Entry.lessThan);
    var index: usize = 0;
    while (index < self.entries.items.len) {
        const stmt = self.entries.items[index].stmt;
        const start: u32 = @intCast(self.markers.items.len);
        while (index < self.entries.items.len and self.entries.items[index].stmt == stmt) : (index += 1) {
            const marker = self.entries.items[index].marker;
            if (self.markers.items.len > start and std.meta.eql(self.markers.items[self.markers.items.len - 1], marker)) continue;
            try self.markers.append(self.allocator, marker);
        }
        try self.by_stmt.put(self.allocator, stmt, .{ .start = start, .len = @intCast(self.markers.items.len - start) });
    }
}

/// The statement whose entry stands for `node`'s. A pattern-match capture
/// node accesses no slot that has markers and has its arm's body as its only
/// successor, so its entry is that body's.
fn statementEntry(self: *const Self, stack_plan: *const StackPlan, node: u32) u32 {
    const stmt = self.node_stmts.items[node];
    if (stmt != none) return stmt;
    var successors = stack_plan.nodeSuccessors(node);
    const body = successors.next().?;
    std.debug.assert(successors.next() == null);
    return self.node_stmts.items[body];
}
