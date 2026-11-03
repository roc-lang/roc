//! How often each local of a procedure is written, for a backend deciding
//! whether an `assign_ref.local` may share its source's bytes instead of
//! copying them.
//!
//! A one-definition `ref.local` whose source has at most one definition and
//! is never written in place names the same bytes for the rest of the
//! procedure, so a backend that keeps aggregates in memory may bind the
//! target to the source's address: nothing can change those bytes after the
//! alias is made, and a later source definition that would change them is
//! exactly what the counts rule out. The native dev backend proves the same
//! rule through its stack plan ("Dev Backend Register Lifetimes"); this
//! inventory is the part of that proof the wasm backend needs, from the same
//! shared definition walk (`body_clone.forEachStmtDef`), without lifetimes.
//!
//! A local written by a statement that reuses its storage (`set_local`,
//! `init_uninitialized`, a join parameter, a string-match capture, a dynamic
//! write) is mutable whatever its count, and so is every dynamic local, whose
//! slot holds an address the runtime writes through.

const std = @import("std");
const collections = @import("collections");
const core = @import("lir_core");
const body_clone = @import("body_clone.zig");
const LIR = core.LIR;
const LirStore = core.LirStore;
const LocalId = LIR.LocalId;
const CFStmtId = LIR.CFStmtId;
const Allocator = std.mem.Allocator;

const Self = @This();

/// One local's writes in the procedure.
pub const Writes = struct {
    definitions: u32 = 0,
    mutable: bool = false,
};

allocator: Allocator,
writes: collections.DenseMap(LocalId, Writes),
visited: collections.DenseMap(CFStmtId, void),
work: std.ArrayList(CFStmtId) = .empty,

pub fn init(allocator: Allocator) Self {
    return .{
        .allocator = allocator,
        .writes = collections.DenseMap(LocalId, Writes).init(allocator),
        .visited = collections.DenseMap(CFStmtId, void).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.writes.deinit();
    self.visited.deinit();
    self.work.deinit(self.allocator);
}

/// Inventory the procedure body rooted at `root`, replacing any earlier one.
pub fn collect(self: *Self, store: *const LirStore, root: CFStmtId) Allocator.Error!void {
    self.writes.clearRetainingCapacity();
    self.visited.clearRetainingCapacity();
    self.work.clearRetainingCapacity();
    try self.work.append(self.allocator, root);
    while (self.work.pop()) |id| {
        if ((try self.visited.getOrPut(id)).found_existing) continue;
        const stmt = store.getCFStmt(id);
        var noter: Noter = .{ .inventory = self, .reuses_storage = reusesStorage(stmt) };
        body_clone.forEachStmtDef(store, stmt, &noter, Noter.note);
        if (noter.failure) |err| return err;
        try body_clone.appendSuccessors(store, &self.work, id, self.allocator);
    }
}

/// Whether a statement's definitions write into storage the local already
/// has rather than giving it a fresh value.
fn reusesStorage(stmt: LIR.CFStmt) bool {
    return switch (stmt) {
        .set_local, .init_uninitialized, .join, .str_match, .str_match_set, .dyn => true,
        .assign_ref,
        .assign_literal,
        .assign_packed_erased_fn,
        .assign_low_level,
        .assign_list,
        .assign_struct,
        .assign_tag,
        .assign_call,
        .assign_call_erased,
        .store_struct,
        .store_tag,
        .debug,
        .expect,
        .expect_err,
        .switch_stmt,
        .switch_initialized_payload,
        .ret,
        .crash,
        .incref,
        .decref,
        .decref_if_initialized,
        .free,
        .jump,
        .runtime_error,
        .comptime_exhaustiveness_failed,
        => false,
    };
}

const Noter = struct {
    inventory: *Self,
    reuses_storage: bool,
    failure: ?Allocator.Error = null,

    fn note(self: *Noter, local: LocalId) void {
        if (self.failure != null) return;
        const entry = self.inventory.writes.getOrPut(local) catch |err| {
            self.failure = err;
            return;
        };
        if (!entry.found_existing) entry.value_ptr.* = .{};
        entry.value_ptr.definitions += 1;
        if (self.reuses_storage) entry.value_ptr.mutable = true;
    }
};

/// Whether `target = ref.local source` may name the source's bytes for the
/// rest of the procedure. A parameter has no definition and is never
/// written, so it qualifies as a source.
pub fn immutableAlias(self: *const Self, target: LocalId, source: LocalId) bool {
    const t = self.writes.get(target) orelse return false;
    const s = self.writes.get(source) orelse Writes{};
    return t.definitions == 1 and !t.mutable and s.definitions <= 1 and !s.mutable;
}

test "a one-definition alias of an unwritten source qualifies, a rewritten source does not" {
    const allocator = std.testing.allocator;
    var store = LirStore.init(allocator);
    defer store.deinit();
    const param = try store.addLocal(.{ .layout_idx = .u64 });
    const once = try store.addLocal(.{ .layout_idx = .u64 });
    const alias_of_param = try store.addLocal(.{ .layout_idx = .u64 });
    const alias_of_once = try store.addLocal(.{ .layout_idx = .u64 });
    const ret = try store.addCFStmt(.{ .ret = .{ .value = alias_of_once } }, .test_fixture);
    const rewrite = try store.addCFStmt(.{ .set_local = .{ .target = once, .value = param, .mode = .replace_existing, .next = ret } }, .test_fixture);
    const second = try store.addCFStmt(.{ .assign_ref = .{ .target = alias_of_once, .op = LIR.RefOp.local(once), .next = rewrite } }, .test_fixture);
    const first = try store.addCFStmt(.{ .assign_ref = .{ .target = alias_of_param, .op = LIR.RefOp.local(param), .next = second } }, .test_fixture);
    const define = try store.addCFStmt(.{ .assign_ref = .{ .target = once, .op = LIR.RefOp.local(param), .next = first } }, .test_fixture);

    var inventory = Self.init(allocator);
    defer inventory.deinit();
    try inventory.collect(&store, define);
    try std.testing.expect(inventory.immutableAlias(alias_of_param, param));
    // `once` is written again by the set_local, so an alias of it would see
    // the later value.
    try std.testing.expect(!inventory.immutableAlias(alias_of_once, once));
    // The rewritten local is not a one-definition target either.
    try std.testing.expect(!inventory.immutableAlias(once, param));
}
