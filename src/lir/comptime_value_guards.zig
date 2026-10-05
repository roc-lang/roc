//! Final post-ARC guards borrow the immutable compile-time failure image.
const std = @import("std");
const core = @import("lir_core");
const DenseMap = @import("collections").DenseMap;
const LIR = core.LIR;
const Program = core.Program;
const Body = @import("body_clone.zig");
const GuardedList = core.LirStore.GuardedList;

const Use = struct { proc: LIR.LirProcSpecId, stmt: LIR.CFStmtId, slot: LIR.StaticDataId };
const Guard = struct { locals: [3]LIR.LocalId, success: LIR.CFStmtId, crash: LIR.CFStmtId, checked_crash: LIR.CFStmtId };

/// Insert explicit failure checks before each compile-time value slot read.
/// A program lowered after its compile-time roots completed passes their
/// frozen image, which records each value's outcome: only reads of a value
/// that failed are guarded, and every other read stays as lowered.
pub fn insert(allocator: std.mem.Allocator, program: *Program.Result, completed: ?*const Program.FrozenStaticData) std.mem.Allocator.Error!void {
    std.debug.assert(program.comptime_value_guards.items.len == 0);
    const has_value_slots = for (program.static_data_values.items) |slot| {
        if (slot.compile_time_root) |root| {
            if (root.role == .value) break true;
        }
    } else false;
    if (!has_value_slots) return;
    const completed_exports: []?u32 = if (completed) |frozen| try exportsBySlot(allocator, program, frozen) else &.{};
    defer allocator.free(completed_exports);
    var uses: std.ArrayList(Use) = .empty;
    defer uses.deinit(allocator);
    var work: std.ArrayList(LIR.CFStmtId) = .empty;
    defer work.deinit(allocator);
    var visited = DenseMap(LIR.CFStmtId, void).init(allocator);
    defer visited.deinit();
    const store = &program.store;
    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        const proc = store.getProcSpec(proc_id);
        // A guarded read is a static-data literal, so only a procedure whose
        // shapes record one can hold a use; Debug builds scan the others too
        // and verify they hold none.
        const admitted = proc.shapes.static_literal;
        if (!admitted and @import("builtin").mode != .Debug) continue;
        const uses_before = uses.items.len;
        defer if (!admitted and uses.items.len != uses_before) @panic("compile-time value guards found a use in a procedure whose shapes excluded it");
        visited.clearRetainingCapacity();
        work.clearRetainingCapacity();
        if (proc.body) |body| try work.append(allocator, body);
        while (work.pop()) |stmt_id| {
            if ((try visited.getOrPut(stmt_id)).found_existing) continue;
            try Body.appendSuccessors(store, &work, stmt_id);
            const assign = switch (store.getCFStmt(stmt_id)) {
                .assign_literal => |a| a,
                .init_uninitialized, .assign_ref, .assign_call, .assign_call_erased, .assign_packed_erased_fn, .assign_boxy_desc_ref, .assign_boxy_dict_ref, .assign_boxy_box, .assign_boxy_record_update, .assign_boxy_reuse_box, .assign_boxy_unbox, .assign_boxy_adapt, .assign_boxy_inspect, .assign_boxy_tag, .assign_boxy_tag_payload, .boxy_tag_match, .assign_call_dict, .assign_low_level, .assign_list, .assign_struct, .assign_tag, .store_struct, .store_tag, .set_local, .debug, .expect, .expect_err, .runtime_error, .comptime_exhaustiveness_failed, .comptime_branch_taken, .incref, .decref, .decref_if_initialized, .free, .switch_stmt, .switch_initialized_payload, .str_match, .str_match_set, .loop_continue, .loop_break, .join, .jump, .ret, .crash => continue,
            };
            const slot = switch (assign.value) {
                .static_data => |id| id,
                .i64_literal, .i128_literal, .f64_literal, .f32_literal, .dec_literal, .str_literal, .boxy_dynamic_num_literal, .boxy_dynamic_frac_literal, .bytes_literal, .proc_ref => continue,
            };
            const root = program.static_data_values.items[@intFromEnum(slot)].compile_time_root orelse continue;
            if (root.role != .value) continue;
            if (completed) |frozen| {
                if (!completedValueFailed(program, frozen, completed_exports, root.role.value.failure_slot)) continue;
            }
            try uses.append(allocator, .{ .proc = proc_id, .stmt = stmt_id, .slot = slot });
        }
    }
    if (uses.items.len == 0) return;
    var guards = DenseMap(LIR.CFStmtId, Guard).init(allocator);
    defer guards.deinit();
    try program.comptime_value_guards.ensureUnusedCapacity(allocator, uses.items.len);
    for (uses.items) |use| {
        if (guards.contains(use.stmt)) continue;
        const root = program.static_data_values.items[@intFromEnum(use.slot)].compile_time_root.?;
        const failure_slot = root.role.value.failure_slot;
        const failure = program.static_data_values.items[@intFromEnum(failure_slot)];
        const fields = failure.compile_time_root.?.role.failure_message;
        const record = try store.addLocal(.{ .layout_idx = failure.layout_idx });
        const failed = try store.addLocal(.{ .layout_idx = .u8 });
        const message = try store.addLocal(.{ .layout_idx = .str });
        // The success path is a copy of the guarded use and keeps its origin;
        // the guard glue carries the use's location with the guard kind. The
        // entry statement becomes guard glue; `completeSuccessfulSlot` later
        // restores the use there with the success copy's (the use's) origin.
        const use_origin = store.stmtOrigin(use.stmt);
        var guard_origin = use_origin;
        guard_origin.kind = .comptime_value_guard;
        const success = try store.addCFStmt(store.getCFStmt(use.stmt), use_origin);
        const crash = try store.addCFStmt(.{ .crash = .{ .msg = .{ .local = message } } }, guard_origin);
        const checked_crash = try store.addCFStmt(.{ .crash = .{ .msg = .{ .local = message }, .checked_error = true } }, guard_origin);
        const load_message = try store.addCFStmt(.{ .assign_ref = .{
            .target = message,
            .op = .{ .field = .{ .source = record, .field_idx = @intCast(fields.message_field) } },
            .next = crash,
        } }, guard_origin);
        const load_checked_message = try store.addCFStmt(.{ .assign_ref = .{
            .target = message,
            .op = .{ .field = .{ .source = record, .field_idx = @intCast(fields.message_field) } },
            .next = checked_crash,
        } }, guard_origin);
        const branch = try store.addCFStmt(.{ .switch_stmt = .{
            .cond = failed,
            .branches = try store.addCFSwitchBranches(&.{
                .{ .value = @intFromEnum(Program.ComptimeFailureKind.none), .body = success },
                .{ .value = @intFromEnum(Program.ComptimeFailureKind.checked_error), .body = load_checked_message },
            }),
            .default_branch = load_message,
            .default_is_cold = true,
        } }, guard_origin);
        const load_failed = try store.addCFStmt(.{ .assign_ref = .{
            .target = failed,
            .op = .{ .field = .{ .source = record, .field_idx = @intCast(fields.failed_field) } },
            .next = branch,
        } }, guard_origin);
        try store.replaceCFStmt(use.stmt, .{ .assign_literal = .{
            .target = record,
            .value = .{ .static_data = failure_slot },
            .next = load_failed,
        } }, guard_origin);
        try guards.put(use.stmt, .{ .locals = .{ record, failed, message }, .success = success, .crash = crash, .checked_crash = checked_crash });
    }
    // Preserve each owner discovered before rewriting shared statements.
    for (uses.items) |use| {
        const guard = guards.get(use.stmt).?;
        const slot = &program.static_data_values.items[@intFromEnum(use.slot)];
        const previous = slot.first_comptime_guard;
        slot.first_comptime_guard = @intCast(program.comptime_value_guards.items.len);
        program.comptime_value_guards.appendAssumeCapacity(.{
            .next_for_slot = previous,
            .owner = use.proc,
            .entry = use.stmt,
            .success = guard.success,
            .value_slot = use.slot,
            .crash = guard.crash,
            .checked_crash = guard.checked_crash,
        });
    }
    var locals: std.ArrayList(LIR.LocalId) = .empty;
    defer locals.deinit(allocator);
    // Discovery appends uses in procedure order, so each owner's uses form
    // one contiguous span even when statements are shared between owners.
    var use_index: usize = 0;
    while (use_index < uses.items.len) {
        const proc_id = uses.items[use_index].proc;
        locals.clearRetainingCapacity();
        while (use_index < uses.items.len and uses.items[use_index].proc == proc_id) : (use_index += 1) {
            try locals.appendSlice(allocator, &guards.get(uses.items[use_index].stmt).?.locals);
        }
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

/// The frozen export holding each static data slot's completed bytes.
fn exportsBySlot(allocator: std.mem.Allocator, program: *const Program.Result, frozen: *const Program.FrozenStaticData) std.mem.Allocator.Error![]?u32 {
    const exports = try allocator.alloc(?u32, program.static_data_values.items.len);
    @memset(exports, null);
    for (frozen.exports, 0..) |item, index| {
        const slot = item.value_id orelse continue;
        exports[@intFromEnum(slot)] = @intCast(index);
    }
    return exports;
}

/// Whether the completed value owning `failure_slot` failed, read from the
/// `failed` flag of its failure record in the frozen image.
fn completedValueFailed(program: *const Program.Result, frozen: *const Program.FrozenStaticData, exports: []const ?u32, failure_slot: LIR.StaticDataId) bool {
    const fields = program.static_data_values.items[@intFromEnum(failure_slot)].compile_time_root.?.role.failure_message;
    const index = exports[@intFromEnum(failure_slot)] orelse @panic("completed program omitted a compile-time value's failure record");
    const record = frozen.exports[index];
    return record.bytes[record.symbol_offset + fields.failed_offset] != 0;
}

fn localLessThan(_: void, a: LIR.LocalId, b: LIR.LocalId) bool {
    return @intFromEnum(a) < @intFromEnum(b);
}

/// Caller has explicit successful evaluation evidence for this root's slot.
pub fn completeSuccessfulSlot(program: *Program.Result, slot: LIR.StaticDataId) std.mem.Allocator.Error!void {
    var next = program.static_data_values.items[@intFromEnum(slot)].first_comptime_guard;
    while (next) |index| {
        const guard = &program.comptime_value_guards.items[index];
        next = guard.next_for_slot;
        std.debug.assert(guard.value_slot == slot);
        if (guard.completed) continue;
        try program.store.replaceCFStmt(guard.entry, program.store.getCFStmt(guard.success), program.store.stmtOrigin(guard.success));
        program.store.getProcSpecPtr(guard.owner).native_code_revision += 1;
        guard.completed = true;
    }
}

test "shared compile-time failure guards preserve exact success and shared frame inventory" {
    try testSharedGuards(std.testing.allocator);
}

test "shared compile-time guard owner metadata allocation failure cleanup" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testSharedGuards, .{});
}

fn testSharedGuards(allocator: std.mem.Allocator) (std.mem.Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const record_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const struct_idx = program.layouts.getLayout(record_layout).getStruct().idx;
    const failure_slot: LIR.StaticDataId = @enumFromInt(program.static_data_values.items.len);
    try program.static_data_values.append(allocator, .{
        .initializer = null,
        .layout_idx = record_layout,
        .compile_time_root = .{
            .module = .{},
            .root = undefined, // Guard insertion reads slot roles, never checked-root identities.
            .const_locator = null,
            .role = .{ .failure_message = .{
                .failed_field = 0,
                .message_field = 1,
                .failed_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0),
                .message_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1),
            } },
        },
    });
    const slot: LIR.StaticDataId = @enumFromInt(program.static_data_values.items.len);
    const plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(allocator, .scalar);
    try program.static_data_values.append(allocator, .{
        .initializer = null,
        .layout_idx = .u8,
        .compile_time_root = .{
            .module = .{},
            .root = undefined, // Guard insertion reads slot roles, never checked-root identities.
            .const_locator = null,
            .role = .{ .value = .{ .failure_slot = failure_slot, .plan = plan } },
        },
    });
    const target = try program.store.addLocal(.{ .layout_idx = .u8 });
    const ret = try program.store.addCFStmt(.{ .ret = .{ .value = target } }, .test_fixture);
    const load = try program.store.addCFStmt(.{ .assign_literal = .{ .target = target, .value = .{ .static_data = slot }, .next = ret } }, .test_fixture);
    const frame = try program.store.addLocalSpan(&.{target});
    var owners: [2]LIR.LirProcSpecId = undefined; // Both entries are assigned by addProcSpec before use.
    for (&owners, 0..) |*owner, i| {
        owner.* = try program.store.addProcSpec(.{ .name = .fromRaw(i), .identity = LIR.ProcIdentity.forTest(1), .args = .empty(), .frame_locals = frame, .body = load, .ret_layout = .u8 }, .none);
    }
    const unrelated = try program.store.addProcSpec(.{ .name = .fromRaw(2), .identity = LIR.ProcIdentity.forTest(2), .args = .empty(), .frame_locals = frame, .body = ret, .ret_layout = .u8 }, .none);
    try insert(allocator, &program, null);
    try std.testing.expectEqual(@as(usize, 2), program.comptime_value_guards.items.len);
    const guard = program.comptime_value_guards.items[0];
    try std.testing.expectEqual(load, guard.entry);
    try std.testing.expectEqual(load, program.comptime_value_guards.items[1].entry);
    for (owners) |owner| {
        const proc = program.store.getProcSpec(owner);
        try std.testing.expectEqual(@as(u64, 0), proc.native_code_revision);
        const locals = program.store.getLocalSpan(proc.frame_locals);
        try std.testing.expectEqual(@as(usize, 4), locals.len);
        for (1..locals.len) |j| try std.testing.expect(localLessThan({}, GuardedList.at(locals, j - 1), GuardedList.at(locals, j)));
    }
    // Completion restores the producer-recorded edge, including its original
    // target and suffix, without examining the emitted guard's shape.
    try completeSuccessfulSlot(&program, slot);
    const restored = program.store.getCFStmt(load).assign_literal;
    try std.testing.expectEqual(target, restored.target);
    try std.testing.expectEqual(slot, restored.value.static_data);
    try std.testing.expectEqual(ret, restored.next);
    try completeSuccessfulSlot(&program, slot);
    for (owners) |owner| {
        const proc = program.store.getProcSpec(owner);
        try std.testing.expectEqual(@as(u64, 1), proc.native_code_revision);
        try std.testing.expectEqual(LIR.ProcIdentity.forTest(1), proc.identity);
    }
    try std.testing.expectEqual(@as(u64, 0), program.store.getProcSpec(unrelated).native_code_revision);
    var clone = try program.store.cloneForProcRewrite(allocator, owners[0]);
    defer clone.deinit();
    try std.testing.expectEqual(@as(u64, 1), clone.getProcSpec(owners[0]).native_code_revision);
}

test "completed compile-time values guard only the reads of failed values" {
    try testCompletedGuards(std.testing.allocator);
}

test "completed compile-time value guards allocation failure cleanup" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, testCompletedGuards, .{});
}

fn testCompletedGuards(allocator: std.mem.Allocator) (std.mem.Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    var program = try Program.Result.init(allocator, .u64);
    defer program.deinit();
    const record_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = .u8 }, .{ .index = 1, .layout = .str } });
    const struct_idx = program.layouts.getLayout(record_layout).getStruct().idx;
    const failed_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0);
    const plan: Program.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(allocator, .scalar);
    // The first failure/value slot pair succeeds, the second fails.
    var value_slots: [2]LIR.StaticDataId = undefined; // Both entries are assigned below before use.
    var failure_slots: [2]LIR.StaticDataId = undefined; // Both entries are assigned below before use.
    for (&value_slots, &failure_slots) |*value_slot, *failure_slot_out| {
        const failure_slot: LIR.StaticDataId = @enumFromInt(program.static_data_values.items.len);
        failure_slot_out.* = failure_slot;
        try program.static_data_values.append(allocator, .{
            .initializer = null,
            .layout_idx = record_layout,
            .compile_time_root = .{
                .module = .{},
                .root = undefined, // Guard insertion reads slot roles, never checked-root identities.
                .const_locator = null,
                .role = .{ .failure_message = .{
                    .failed_field = 0,
                    .message_field = 1,
                    .failed_offset = failed_offset,
                    .message_offset = program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1),
                } },
            },
        });
        value_slot.* = @enumFromInt(program.static_data_values.items.len);
        try program.static_data_values.append(allocator, .{
            .initializer = null,
            .layout_idx = .u8,
            .compile_time_root = .{
                .module = .{},
                .root = undefined, // Guard insertion reads slot roles, never checked-root identities.
                .const_locator = null,
                .role = .{ .value = .{ .failure_slot = failure_slot, .plan = plan } },
            },
        });
    }
    var loads: [2]LIR.CFStmtId = undefined; // Both entries are assigned below before use.
    var owners: [2]LIR.LirProcSpecId = undefined; // Both entries are assigned below before use.
    for (value_slots, &loads, &owners, 0..) |slot, *load, *owner, i| {
        const target = try program.store.addLocal(.{ .layout_idx = .u8 });
        const ret = try program.store.addCFStmt(.{ .ret = .{ .value = target } }, .test_fixture);
        load.* = try program.store.addCFStmt(.{ .assign_literal = .{ .target = target, .value = .{ .static_data = slot }, .next = ret } }, .test_fixture);
        owner.* = try program.store.addProcSpec(.{ .name = .fromRaw(i), .identity = LIR.ProcIdentity.forTest(@intCast(i + 1)), .args = .empty(), .frame_locals = try program.store.addLocalSpan(&.{target}), .body = load.*, .ret_layout = .u8 }, .none);
    }
    var succeeded_record = [_]u8{0} ** 32;
    var failed_record = [_]u8{0} ** 32;
    failed_record[failed_offset] = 1;
    const value_bytes = [_]u8{7};
    // The frozen image lists its exports in an order unrelated to slot order.
    var exports = [_]Program.StaticDataExport{
        .{ .symbol_name = "failed_value", .value_id = value_slots[1], .bytes = &value_bytes, .alignment = 1 },
        .{ .symbol_name = "failed_record", .value_id = failure_slots[1], .bytes = &failed_record, .alignment = 8 },
        .{ .symbol_name = "value", .value_id = value_slots[0], .bytes = &value_bytes, .alignment = 1 },
        .{ .symbol_name = "record", .value_id = failure_slots[0], .bytes = &succeeded_record, .alignment = 8 },
    };
    const frozen = Program.FrozenStaticData{ .allocator = allocator, .exports = &exports };
    try insert(allocator, &program, &frozen);

    try std.testing.expectEqual(@as(usize, 1), program.comptime_value_guards.items.len);
    const guard = program.comptime_value_guards.items[0];
    try std.testing.expectEqual(owners[1], guard.owner);
    try std.testing.expectEqual(value_slots[1], guard.value_slot);
    try std.testing.expect(program.store.getCFStmt(loads[1]).assign_literal.value.static_data != value_slots[1]);
    try std.testing.expectEqual(@as(usize, 4), program.store.getLocalSpan(program.store.getProcSpec(owners[1]).frame_locals).len);

    // The successful read is left exactly as lowered.
    const kept = program.store.getCFStmt(loads[0]).assign_literal;
    try std.testing.expectEqual(value_slots[0], kept.value.static_data);
    try std.testing.expectEqual(@as(usize, 1), program.store.getLocalSpan(program.store.getProcSpec(owners[0]).frame_locals).len);
    try std.testing.expectEqual(@as(u64, 0), program.store.getProcSpec(owners[0]).native_code_revision);
}
