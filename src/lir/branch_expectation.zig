//! Marks the cold arm of every branch whose condition carries a
//! `bool_likely` expectation.
//!
//! `bool_likely` is the identity on its Bool operand and a statement about
//! control flow: the branch it decides is expected to take the arm its
//! operand being true selects. A `switch` on such a value, with the one
//! branch for `1` and a default for `0`, therefore has a cold default, which
//! the LLVM backend turns into branch weights. The condition may reach the
//! switch through pure aliases, since lowering binds one alias per use. Only
//! a local with a single definition in the procedure is followed: a
//! reassigned local or a join parameter names different values at different
//! points, and no expectation is attached to those.
const std = @import("std");
const collections = @import("collections");
const core = @import("lir_core");
const BodyClone = @import("body_clone.zig");

const Allocator = std.mem.Allocator;
const LIR = core.LIR;
const LirStore = core.LirStore;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const GuardedList = LirStore.GuardedList;

/// The single defining statement of a local, or null once it has several
/// definitions (including a join declaring it as a parameter).
const Definition = ?CFStmtId;

/// Bound on alias hops from a switch condition back to its definition.
const max_alias_hops: usize = 64;

/// Marks the cold defaults of one procedure's expectation-carrying switches.
pub fn runProc(store: *LirStore, proc_id: LIR.LirProcSpecId, scratch_allocator: Allocator) Allocator.Error!void {
    const body = BodyClone.rewritableProcBody(store, proc_id) orelse return;
    var definitions = collections.DenseMap(LocalId, Definition).init(scratch_allocator);
    defer definitions.deinit();
    var switches: std.ArrayList(CFStmtId) = .empty;
    defer switches.deinit(scratch_allocator);
    var work: std.ArrayList(CFStmtId) = .empty;
    defer work.deinit(scratch_allocator);
    var visited = collections.DenseMap(CFStmtId, void).init(scratch_allocator);
    defer visited.deinit();

    try work.append(scratch_allocator, body);
    while (work.pop()) |stmt_id| {
        if ((try visited.getOrPut(stmt_id)).found_existing) continue;
        try BodyClone.appendSuccessorsWithAllocator(store, &work, stmt_id, scratch_allocator);
        switch (store.getCFStmt(stmt_id)) {
            inline .init_uninitialized,
            .assign_ref,
            .assign_literal,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .assign_boxy_desc_ref,
            .assign_boxy_dict_ref,
            .assign_boxy_box,
            .assign_boxy_reuse_box,
            .assign_boxy_unbox,
            .assign_boxy_adapt,
            .assign_boxy_inspect,
            .assign_boxy_eq,
            .assign_boxy_tag,
            .assign_boxy_tag_payload,
            .assign_call_dict,
            => |s| try define(&definitions, s.target, stmt_id),
            inline .store_struct, .store_tag => |s| try define(&definitions, s.dest, stmt_id),
            // A written local names different values at different points.
            .set_local => |s| try definitions.put(s.target, null),
            .join => |s| {
                const params = store.getLocalSpan(s.params);
                for (0..GuardedList.borrowLen(params)) |i| try definitions.put(GuardedList.at(params, i), null);
            },
            .switch_stmt => try switches.append(scratch_allocator, stmt_id),
            .debug, .expect, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free, .switch_initialized_payload, .str_match, .str_match_set, .boxy_tag_match, .loop_continue, .loop_break, .jump, .ret, .crash => {},
        }
    }

    for (switches.items) |stmt_id| {
        const sw = store.getCFStmt(stmt_id).switch_stmt;
        if (sw.default_is_cold) continue;
        const branches = store.getCFSwitchBranches(sw.branches);
        if (GuardedList.borrowLen(branches) != 1 or GuardedList.at(branches, 0).value != 1) continue;
        if (!carriesExpectation(store, &definitions, sw.cond)) continue;
        store.getCFStmtPtr(stmt_id).switch_stmt.default_is_cold = true;
    }
}

fn define(definitions: *collections.DenseMap(LocalId, Definition), local: LocalId, stmt_id: CFStmtId) Allocator.Error!void {
    const entry = try definitions.getOrPut(local);
    entry.value_ptr.* = if (entry.found_existing) null else stmt_id;
}

/// Whether the local's single definition, through pure aliases, is a
/// `bool_likely` marker.
fn carriesExpectation(store: *const LirStore, definitions: *const collections.DenseMap(LocalId, Definition), cond: LocalId) bool {
    var local = cond;
    var hops: usize = 0;
    while (hops < max_alias_hops) : (hops += 1) {
        const definition = (definitions.get(local) orelse return false) orelse return false;
        switch (store.getCFStmt(definition)) {
            .assign_low_level => |s| return s.op == .bool_likely,
            .assign_ref => |s| switch (s.op) {
                .local => |source| local = source,
                .field, .discriminant, .tag_payload, .tag_payload_struct, .list_reinterpret, .nominal => return false,
            },
            .init_uninitialized, .assign_literal, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_list, .assign_struct, .assign_tag, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_eq, .assign_boxy_tag, .assign_boxy_tag_payload, .assign_call_dict, .store_struct, .store_tag, .set_local, .join, .switch_stmt, .debug, .expect, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free, .switch_initialized_payload, .str_match, .str_match_set, .boxy_tag_match, .loop_continue, .loop_break, .jump, .ret, .crash => return false,
        }
    }
    return false;
}

const testing = std.testing;
const layout_mod = @import("layout");

const Fixture = struct {
    store: LirStore,
    /// The switch built by `build`.
    switch_stmt: CFStmtId = undefined,

    fn init() Fixture {
        return .{ .store = LirStore.init(testing.allocator) };
    }

    fn deinit(self: *Fixture) void {
        self.store.deinit();
    }

    /// A procedure `if <cond> { 1 } else { 0 }` where `cond` is `a < b`,
    /// optionally passed through `bool_likely` and then through an alias.
    fn build(self: *Fixture, likely: bool, alias: bool, reassigned: bool) Allocator.Error!LIR.LirProcSpecId {
        const store = &self.store;
        const a = try store.addLocal(.{ .layout_idx = .u64 });
        const b = try store.addLocal(.{ .layout_idx = .u64 });
        const compared = try store.addLocal(.{ .layout_idx = .bool });
        const marked = try store.addLocal(.{ .layout_idx = .bool });
        const aliased = try store.addLocal(.{ .layout_idx = .bool });
        const result = try store.addLocal(.{ .layout_idx = .u64 });
        const ret = try store.addCFStmt(.{ .ret = .{ .value = result } }, .test_fixture);
        const one = try store.addCFStmt(.{ .assign_literal = .{ .target = result, .value = .{ .i64_literal = .{ .value = 1, .layout_idx = .u64 } }, .next = ret } }, .test_fixture);
        const zero = try store.addCFStmt(.{ .assign_literal = .{ .target = result, .value = .{ .i64_literal = .{ .value = 0, .layout_idx = .u64 } }, .next = ret } }, .test_fixture);
        const cond = if (alias) aliased else if (likely) marked else compared;
        self.switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond,
            .branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = one }}),
            .default_branch = zero,
            .default_is_cold = false,
            .continuation = null,
        } }, .test_fixture);
        var current = self.switch_stmt;
        if (reassigned) {
            current = try store.addCFStmt(.{ .set_local = .{ .target = cond, .value = compared, .mode = .replace_existing, .next = current } }, .test_fixture);
        }
        if (alias) {
            current = try store.addCFStmt(.{ .assign_ref = .{ .target = aliased, .op = .{ .local = if (likely) marked else compared }, .next = current } }, .test_fixture);
        }
        if (likely) {
            current = try store.addCFStmt(.{ .assign_low_level = .{
                .target = marked,
                .op = .bool_likely,
                .rc_effect = .none(),
                .args = try store.addLocalSpan(&.{compared}),
                .next = current,
            } }, .test_fixture);
        }
        current = try store.addCFStmt(.{ .assign_low_level = .{
            .target = compared,
            .op = .num_is_lt,
            .rc_effect = .none(),
            .args = try store.addLocalSpan(&.{ a, b }),
            .next = current,
        } }, .test_fixture);
        return try store.addProcSpec(.{
            .identity = LIR.ProcIdentity.forTest(@intCast(store.procSpecCount())),
            .name = store.freshSyntheticSymbol(),
            .args = try store.addLocalSpan(&.{ a, b }),
            .body = current,
            .ret_layout = .u64,
        }, .none);
    }

    fn coldDefault(self: *const Fixture) bool {
        return self.store.getCFStmt(self.switch_stmt).switch_stmt.default_is_cold;
    }
};

test "a switch on a bool_likely value, directly or through an alias, gets a cold default" {
    for ([_]bool{ false, true }) |alias| {
        var f = Fixture.init();
        defer f.deinit();
        const proc = try f.build(true, alias, false);
        try runProc(&f.store, proc, testing.allocator);
        try testing.expect(f.coldDefault());
    }
}

test "a switch on an unmarked comparison or a reassigned marker keeps a warm default" {
    for ([_][2]bool{ .{ false, false }, .{ false, true }, .{ true, false } }) |case| {
        var f = Fixture.init();
        defer f.deinit();
        // case[0]: alias of the plain comparison; case[1]: the condition is
        // reassigned before the switch, so its definition is not single.
        const likely = !case[0] and case[1];
        const proc = try f.build(likely, case[0], case[1]);
        try runProc(&f.store, proc, testing.allocator);
        try testing.expect(!f.coldDefault());
    }
}
