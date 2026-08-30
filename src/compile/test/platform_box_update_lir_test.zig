//! Regression coverage for platform-provided boxed model update wrappers.

const std = @import("std");
const collections = @import("collections");
const layout = @import("layout");
const lir = @import("lir");

const harness = @import("lower_to_lir_harness.zig");

const Counts = struct {
    prepare_update: usize = 0,
    owned_unbox: usize = 0,
    borrowed_unbox: usize = 0,
    list_set: usize = 0,
    list_replace: usize = 0,
    list_append: usize = 0,
    box_retain: usize = 0,
    box_release: usize = 0,
    list_retain: usize = 0,
    calls_to_unbox_proc: usize = 0,

    fn add(self: *Counts, other: Counts) void {
        inline for (std.meta.fields(Counts)) |field| {
            @field(self, field.name) += @field(other, field.name);
        }
    }
};

test "platform boxed model without inlining transfers checked mutation field ownership" {
    try harness.runAppPathLirInspection(
        "test/box-model-uniqueness/app.roc",
        .{ .inline_mode = .none, .consume_dead_boxes = true, .proc_debug_names = true },
        expectNoInlineOwnership,
    );
}

test "platform boxed model with wrapper inlining consumes each dead Box" {
    try harness.runAppPathLirInspection(
        "test/box-model-uniqueness/app.roc",
        .{ .inline_mode = .wrappers, .consume_dead_boxes = true, .proc_debug_names = true },
        expectWrapperInlineOwnership,
    );
}

fn expectNoInlineOwnership(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    const unbox_proc = findNamedProc(store, "Builtin.Box.unbox") orelse return error.TestUnexpectedResult;
    var total = Counts{};
    var found_unbox_body = false;
    var found_list_set_body = false;

    for (store.getProcSpecs(), 0..) |_, index| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const counts = try countProc(store, layouts, proc_id, unbox_proc, null);
        total.add(counts);

        if (store.procDebugName(proc_id)) |name| {
            if (std.mem.eql(u8, name, "Builtin.Box.unbox")) {
                try std.testing.expectEqual(@as(usize, 0), counts.owned_unbox);
                try std.testing.expectEqual(@as(usize, 1), counts.borrowed_unbox);
                try std.testing.expectEqual(@as(usize, 0), counts.box_retain);
                try std.testing.expectEqual(@as(usize, 1), counts.box_release);
                found_unbox_body = true;
            } else if (std.mem.eql(u8, name, "list_set_unsafe")) {
                try std.testing.expectEqual(@as(usize, 1), counts.list_set);
                found_list_set_body = true;
            }
        }
    }

    try std.testing.expect(found_unbox_body);
    try std.testing.expect(found_list_set_body);
    try std.testing.expectEqual(@as(usize, 0), total.prepare_update);
    try std.testing.expectEqual(@as(usize, 0), total.owned_unbox);
    try std.testing.expect(total.borrowed_unbox >= 1);
    try std.testing.expect(total.box_release >= total.borrowed_unbox);
    try std.testing.expectEqual(@as(usize, 1), total.list_set);
    try std.testing.expectEqual(@as(usize, 1), total.list_replace);
    try std.testing.expectEqual(@as(usize, 1), total.list_append);

    const wrapper_names = [_][]const u8{
        "update_straight_for_host",
        "update_adapter_for_host!",
        "update_append_for_host!",
        "update_pattern_for_host!",
        "update_erased_for_host!",
        "cursor_for_host",
    };
    for (&wrapper_names) |name| {
        const root = findNamedProc(store, name) orelse return error.TestUnexpectedResult;
        const root_counts = try countProc(store, layouts, root, unbox_proc, null);
        try std.testing.expect(root_counts.calls_to_unbox_proc >= 1);
    }

    // With no inlining, the four platform wrappers transfer their Box into
    // the one concrete Box.unbox procedure. In particular, there is no Box
    // retain extending the lender across the update call. Retains elsewhere
    // belong to checked-list ownership schedules, not to the Box lender.
    try std.testing.expectEqual(@as(usize, 0), total.box_retain);
    try std.testing.expect(total.list_retain > 0);

    // This combined graph also reaches unconditional base collection helpers
    // through controls that the runtime allocation loop does not execute, so
    // it intentionally retains their RC statements. Box ownership remains
    // transfer-only throughout.
    const active = try countReachableNamed(
        store,
        layouts,
        unbox_proc,
        &.{ "update_straight_for_host", "update_adapter_for_host!", "cursor_for_host" },
    );

    // The divergent pattern adapter exercises pairwise residual masks: Set
    // consumes points, Append consumes trail, and Other forwards the intact
    // model. Every reachable edge stays retain-free in no-inline mode.
    const pattern_active = try countReachableNamed(
        store,
        layouts,
        unbox_proc,
        &.{"update_pattern_for_host!"},
    );
    try std.testing.expectEqual(@as(usize, 0), active.box_retain);
    try std.testing.expect(active.list_retain > 0);
    try std.testing.expectEqual(@as(usize, 0), pattern_active.box_retain);
    try std.testing.expectEqual(@as(usize, 0), pattern_active.list_retain);

    // List.replace and List.update retain values on other control-flow paths
    // in their unconditional base procedures. The allocation fixture executes
    // their out-of-bounds arms and proves those retains do not poison the list
    // restored to the following append.
    const append_active = try countReachableNamed(
        store,
        layouts,
        unbox_proc,
        &.{"update_append_for_host!"},
    );
    try std.testing.expectEqual(@as(usize, 0), append_active.box_retain);
    try std.testing.expectEqual(@as(usize, 1), append_active.list_replace);
    try std.testing.expectEqual(@as(usize, 1), append_active.list_append);
}

fn expectWrapperInlineOwnership(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    var total = Counts{};

    for (store.getProcSpecs(), 0..) |_, index| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const counts = try countProc(store, layouts, proc_id, null, null);
        total.add(counts);
    }

    // Exact single-use inlining may move every platform update into the host
    // root, so procedure names are not part of this ownership invariant. The
    // complete reachable graph must still contain all six unbox/release pairs
    // and every checked list mutation without adding a defensive retain.
    try std.testing.expectEqual(@as(usize, 0), total.prepare_update);
    try std.testing.expectEqual(@as(usize, 0), total.owned_unbox);
    try std.testing.expectEqual(@as(usize, 6), total.borrowed_unbox);
    try std.testing.expectEqual(@as(usize, 6), total.box_release);
    try std.testing.expectEqual(@as(usize, 5), total.list_set);
    try std.testing.expectEqual(@as(usize, 2), total.list_replace);
    try std.testing.expectEqual(@as(usize, 4), total.list_append);
    try std.testing.expectEqual(@as(usize, 0), total.box_retain);
    try std.testing.expectEqual(@as(usize, 0), total.list_retain);
}

fn findNamedProc(store: *const lir.LirStore, expected_name: []const u8) ?lir.LIR.LirProcSpecId {
    for (store.getProcSpecs(), 0..) |_, index| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const name = store.procDebugName(proc_id) orelse continue;
        if (std.mem.eql(u8, name, expected_name)) return proc_id;
    }
    return null;
}

fn countProc(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    proc_id: lir.LIR.LirProcSpecId,
    unbox_proc: ?lir.LIR.LirProcSpecId,
    called_procs: ?*std.ArrayList(lir.LIR.LirProcSpecId),
) harness.LowerToLirHarnessError!Counts {
    const body = store.getProcSpec(proc_id).body orelse return .{};
    var counts = Counts{};
    var work = std.ArrayList(lir.LIR.CFStmtId).empty;
    defer work.deinit(std.testing.allocator);
    var visited = collections.DenseMap(lir.LIR.CFStmtId, void).init(std.testing.allocator);
    defer visited.deinit();

    try work.append(std.testing.allocator, body);
    while (work.pop()) |stmt_id| {
        const entry = try visited.getOrPut(stmt_id);
        if (entry.found_existing) continue;

        const stmt = store.getCFStmt(stmt_id);
        if (stmt == .assign_call) {
            const call = stmt.assign_call;
            if (unbox_proc != null and call.proc == unbox_proc.?) counts.calls_to_unbox_proc += 1;
            if (called_procs) |proc_work| try proc_work.append(std.testing.allocator, call.proc);
        } else if (stmt == .incref) {
            const retain = stmt.incref;
            const retained_tag = layouts.getLayout(store.getLocal(retain.value).layout_idx).tag;
            if (retained_tag == .box) {
                counts.box_retain += 1;
            } else if (retained_tag == .list) {
                counts.list_retain += 1;
            }
        } else if (stmt == .decref) {
            const release = stmt.decref;
            const released_tag = layouts.getLayout(store.getLocal(release.value).layout_idx).tag;
            if (released_tag == .box) counts.box_release += 1;
        } else if (stmt == .assign_low_level) {
            const low_level = stmt.assign_low_level;
            if (low_level.op == .box_prepare_update) counts.prepare_update += 1;
            if (low_level.op == .box_unbox) counts.owned_unbox += 1;
            if (low_level.op == .box_unbox_borrowed) counts.borrowed_unbox += 1;
            if (low_level.op == .list_set) counts.list_set += 1;
            if (low_level.op == .list_replace_unsafe) counts.list_replace += 1;
            if (low_level.op == .list_append_unsafe) counts.list_append += 1;
        }
        try lir.BodyClone.appendSuccessors(@constCast(store), &work, stmt_id);
    }
    return counts;
}

fn countReachableNamed(
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    unbox_proc: ?lir.LIR.LirProcSpecId,
    names: []const []const u8,
) harness.LowerToLirHarnessError!Counts {
    var work = std.ArrayList(lir.LIR.LirProcSpecId).empty;
    defer work.deinit(std.testing.allocator);
    var visited = collections.DenseMap(lir.LIR.LirProcSpecId, void).init(std.testing.allocator);
    defer visited.deinit();

    for (names) |expected| {
        const root = findNamedProc(store, expected) orelse return error.TestUnexpectedResult;
        try work.append(std.testing.allocator, root);
    }

    var counts = Counts{};
    while (work.pop()) |proc_id| {
        const entry = try visited.getOrPut(proc_id);
        if (entry.found_existing) continue;
        counts.add(try countProc(store, layouts, proc_id, unbox_proc, &work));
    }
    return counts;
}
