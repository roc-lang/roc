//! Final post-ARC guards borrow the immutable compile-time failure image.
const std = @import("std");
const core = @import("lir_core");
const LIR = core.LIR;
const Program = core.Program;
const Body = @import("body_clone.zig");
const GuardedList = core.LirStore.GuardedList;

const Use = struct { proc: LIR.LirProcSpecId, stmt: LIR.CFStmtId, slot: LIR.StaticDataId };
const Guard = struct { locals: [3]LIR.LocalId };

pub fn insert(allocator: std.mem.Allocator, program: *Program.Result) std.mem.Allocator.Error!void {
    std.debug.assert(program.comptime_value_guards.items.len == 0);
    const has_value_slots = for (program.static_data_values.items) |slot| {
        if (slot.compile_time_root) |root| {
            if (root.role == .value) break true;
        }
    } else false;
    if (!has_value_slots) return;
    var uses: std.ArrayList(Use) = .empty;
    defer uses.deinit(allocator);
    var work: std.ArrayList(LIR.CFStmtId) = .empty;
    defer work.deinit(allocator);
    var visited = std.AutoHashMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();
    const store = &program.store;
    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        const proc = store.getProcSpec(proc_id);
        visited.clearRetainingCapacity();
        work.clearRetainingCapacity();
        if (proc.body) |body| try work.append(allocator, body);
        while (work.pop()) |stmt_id| {
            if ((try visited.getOrPut(stmt_id)).found_existing) continue;
            try Body.appendSuccessors(store, &work, stmt_id);
            const assign = switch (store.getCFStmt(stmt_id)) {
                .assign_literal => |a| a,
                else => continue,
            };
            const slot = switch (assign.value) {
                .static_data => |id| id,
                else => continue,
            };
            const root = program.static_data_values.items[@intFromEnum(slot)].compile_time_root orelse continue;
            if (root.role != .value) continue;
            try uses.append(allocator, .{ .proc = proc_id, .stmt = stmt_id, .slot = slot });
        }
    }
    var guards = std.AutoHashMap(LIR.CFStmtId, Guard).init(allocator);
    defer guards.deinit();
    for (uses.items) |use| {
        if (guards.contains(use.stmt)) continue;
        const root = program.static_data_values.items[@intFromEnum(use.slot)].compile_time_root.?;
        const failure_slot = root.role.value.failure_slot;
        const failure = program.static_data_values.items[@intFromEnum(failure_slot)];
        const fields = failure.compile_time_root.?.role.failure_message;
        const record = try store.addLocal(.{ .layout_idx = failure.layout_idx });
        const failed = try store.addLocal(.{ .layout_idx = .u8 });
        const message = try store.addLocal(.{ .layout_idx = .str });
        const success = try store.addCFStmt(store.getCFStmt(use.stmt));
        const crash = try store.addCFStmt(.{ .crash = .{ .msg = .{ .local = message } } });
        const load_message = try store.addCFStmt(.{ .assign_ref = .{
            .target = message,
            .op = .{ .field = .{ .source = record, .field_idx = @intCast(fields.message_field) } },
            .next = crash,
        } });
        const branch = try store.addCFStmt(.{ .switch_stmt = .{
            .cond = failed,
            .branches = try store.addCFSwitchBranches(&.{.{ .value = 0, .body = success }}),
            .default_branch = load_message,
            .default_is_cold = true,
        } });
        const load_failed = try store.addCFStmt(.{ .assign_ref = .{
            .target = failed,
            .op = .{ .field = .{ .source = record, .field_idx = @intCast(fields.failed_field) } },
            .next = branch,
        } });
        store.getCFStmtPtr(use.stmt).* = .{ .assign_literal = .{
            .target = record,
            .value = .{ .static_data = failure_slot },
            .next = load_failed,
        } };
        try program.comptime_value_guards.append(allocator, .{ .entry = use.stmt, .success = success, .value_slot = use.slot, .crash = crash });
        try guards.put(use.stmt, .{ .locals = .{ record, failed, message } });
    }
    var locals: std.ArrayList(LIR.LocalId) = .empty;
    defer locals.deinit(allocator);
    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        locals.clearRetainingCapacity();
        var affected = false;
        for (uses.items) |use| {
            if (use.proc != proc_id) continue;
            affected = true;
            try locals.appendSlice(allocator, &guards.get(use.stmt).?.locals);
        }
        if (!affected) continue;
        const frame = store.getLocalSpan(store.getProcSpec(proc_id).frame_locals);
        for (0..frame.len) |i| try locals.append(allocator, GuardedList.at(frame, i));
        std.mem.sort(LIR.LocalId, locals.items, {}, localLessThan);
        var count: usize = 0;
        for (locals.items) |local| {
            if (count != 0 and locals.items[count - 1] == local) continue;
            locals.items[count] = local;
            count += 1;
        }
        const span = try store.addLocalSpan(locals.items[0..count]);
        store.getProcSpecPtr(proc_id).frame_locals = span;
        if (store.procNeedsStackProbe(&program.layouts, store.getProcSpec(proc_id))) store.getProcSpecPtr(proc_id).stack_probe = .required;
    }
}

fn localLessThan(_: void, a: LIR.LocalId, b: LIR.LocalId) bool {
    return @intFromEnum(a) < @intFromEnum(b);
}

/// Caller has explicit successful evaluation evidence for this root's slot.
pub fn completeSuccessfulSlot(program: *Program.Result, slot: LIR.StaticDataId) void {
    for (program.comptime_value_guards.items) |guard| {
        if (guard.value_slot == slot) program.store.getCFStmtPtr(guard.entry).* = program.store.getCFStmt(guard.success);
    }
}

test "shared compile-time failure guards preserve exact success and shared frame inventory" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const record_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const struct_idx = program.layouts.getLayout(record_layout).getStruct().idx;
    try program.static_data_values.append(allocator, .{
        .initializer = null,
        .layout_idx = record_layout,
        .compile_time_root = .{
            .module = .{},
            .root = @enumFromInt(0),
            .const_locator = null,
            .role = .{ .failure_message = .{
                .failed_field = 0,
                .message_field = 1,
                .failed_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0),
                .message_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1),
            } },
        },
    });
    const slot: LIR.StaticDataId = @enumFromInt(1);
    try program.const_plans.append(allocator, .scalar);
    try program.static_data_values.append(allocator, .{
        .initializer = null,
        .layout_idx = .u8,
        .compile_time_root = .{ .module = .{}, .root = @enumFromInt(0), .const_locator = null, .role = .{ .value = .{ .failure_slot = @enumFromInt(0), .plan = @enumFromInt(0) } } },
    });
    const target = try program.store.addLocal(.{ .layout_idx = .u8 });
    const ret = try program.store.addCFStmt(.{ .ret = .{ .value = target } });
    const load = try program.store.addCFStmt(.{ .assign_literal = .{ .target = target, .value = .{ .static_data = slot }, .next = ret } });
    const frame = try program.store.addLocalSpan(&.{target});
    for (0..2) |i| {
        _ = try program.store.addProcSpec(.{ .name = .fromRaw(i), .args = .empty(), .frame_locals = frame, .body = load, .ret_layout = .u8 });
    }
    try insert(allocator, &program);
    try std.testing.expectEqual(@as(usize, 1), program.comptime_value_guards.items.len);
    const guard = program.comptime_value_guards.items[0];
    try std.testing.expectEqual(load, guard.entry);
    for (0..2) |i| {
        const proc = program.store.getProcSpec(@enumFromInt(@as(u32, @intCast(i))));
        const locals = program.store.getLocalSpan(proc.frame_locals);
        try std.testing.expectEqual(@as(usize, 4), locals.len);
        for (1..locals.len) |j| try std.testing.expect(localLessThan({}, GuardedList.at(locals, j - 1), GuardedList.at(locals, j)));
    }
    // Completion restores the producer-recorded edge, including its original
    // target and suffix, without examining the emitted guard's shape.
    completeSuccessfulSlot(&program, slot);
    const restored = program.store.getCFStmt(load).assign_literal;
    try std.testing.expectEqual(target, restored.target);
    try std.testing.expectEqual(slot, restored.value.static_data);
    try std.testing.expectEqual(ret, restored.next);
}
