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
    box_retain: usize = 0,
    list_retain: usize = 0,
    calls_to_unbox_proc: usize = 0,

    fn add(self: *Counts, other: Counts) void {
        inline for (std.meta.fields(Counts)) |field| {
            @field(self, field.name) += @field(other, field.name);
        }
    }
};

test "platform boxed model without inlining consumes the Box but preserves checked mutation failure ownership" {
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
        const counts = try countProc(store, layouts, proc_id, unbox_proc);
        total.add(counts);

        if (store.procDebugName(proc_id)) |name| {
            if (std.mem.eql(u8, name, "Builtin.Box.unbox")) {
                try std.testing.expectEqual(@as(usize, 1), counts.owned_unbox);
                try std.testing.expectEqual(@as(usize, 0), counts.borrowed_unbox);
                try std.testing.expectEqual(@as(usize, 0), counts.box_retain);
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
    try std.testing.expectEqual(@as(usize, 1), total.owned_unbox);
    try std.testing.expectEqual(@as(usize, 0), total.borrowed_unbox);
    try std.testing.expectEqual(@as(usize, 3), total.calls_to_unbox_proc);
    try std.testing.expectEqual(@as(usize, 1), total.list_set);

    // With no inlining, the three platform wrappers transfer their Box into
    // the one concrete Box.unbox procedure. In particular, there is no Box
    // retain extending the lender across the update call. The two remaining
    // list retains are separate: checked List.set must preserve the original
    // model/list ownership for its Err branch until conditional ownership is
    // represented explicitly in LIR.
    try std.testing.expectEqual(@as(usize, 0), total.box_retain);
    try std.testing.expectEqual(@as(usize, 2), total.list_retain);
}

fn expectWrapperInlineOwnership(store: *const lir.LirStore, layouts: *const layout.Store) harness.LowerToLirHarnessError!void {
    var total = Counts{};
    var found_straight = false;
    var found_adapter = false;
    var found_cursor = false;

    for (store.getProcSpecs(), 0..) |_, index| {
        const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(index)));
        const counts = try countProc(store, layouts, proc_id, null);
        total.add(counts);
        const name = store.procDebugName(proc_id) orelse continue;

        if (std.mem.eql(u8, name, "update_straight_for_host")) {
            try expectProcCounts(counts, 1, 1);
            found_straight = true;
        } else if (std.mem.eql(u8, name, "update_adapter_for_host!")) {
            try expectProcCounts(counts, 1, 3);
            found_adapter = true;
        } else if (std.mem.eql(u8, name, "cursor_for_host")) {
            try expectProcCounts(counts, 1, 0);
            found_cursor = true;
        }
    }

    try std.testing.expect(found_straight);
    try std.testing.expect(found_adapter);
    try std.testing.expect(found_cursor);
    try std.testing.expectEqual(@as(usize, 0), total.prepare_update);
    try std.testing.expectEqual(@as(usize, 3), total.owned_unbox);
    try std.testing.expectEqual(@as(usize, 0), total.borrowed_unbox);
    try std.testing.expectEqual(@as(usize, 4), total.list_set);
    try std.testing.expectEqual(@as(usize, 0), total.box_retain);
    try std.testing.expectEqual(@as(usize, 0), total.list_retain);
}

fn expectProcCounts(counts: Counts, owned_unbox: usize, list_set: usize) !void {
    try std.testing.expectEqual(owned_unbox, counts.owned_unbox);
    try std.testing.expectEqual(@as(usize, 0), counts.borrowed_unbox);
    try std.testing.expectEqual(list_set, counts.list_set);
    try std.testing.expectEqual(@as(usize, 0), counts.box_retain);
    try std.testing.expectEqual(@as(usize, 0), counts.list_retain);
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
        switch (stmt) {
            .assign_call => |call| if (unbox_proc != null and call.proc == unbox_proc.?) {
                counts.calls_to_unbox_proc += 1;
            },
            .incref => |retain| {
                switch (layouts.getLayout(store.getLocal(retain.value).layout_idx).tag) {
                    .box => counts.box_retain += 1,
                    .list => counts.list_retain += 1,
                    else => {},
                }
            },
            .assign_low_level => |low_level| {
                if (low_level.op == .box_prepare_update) counts.prepare_update += 1;
                if (low_level.op == .box_unbox) counts.owned_unbox += 1;
                if (low_level.op == .box_unbox_borrowed) counts.borrowed_unbox += 1;
                if (low_level.op == .list_set) counts.list_set += 1;
            },
            else => {},
        }
        try lir.BodyClone.appendSuccessors(@constCast(store), &work, stmt_id);
    }
    return counts;
}
