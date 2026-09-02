//! Exact LIR shape matching for backend overflow-predicate/result fusion.
//!
//! The public integer helpers compute an overflow predicate, branch on it,
//! and compute the wrapping result on the non-overflow path. Direct backends
//! use this matcher to retain the arithmetic result already produced while
//! computing the predicate instead of emitting the same arithmetic twice.

const std = @import("std");
const core = @import("lir_core");

const LIR = core.LIR;
const LirStore = core.LirStore;
const CheckedArithmetic = core.CheckedArithmetic;
const GuardedList = LirStore.GuardedList;

/// The arithmetic consumer whose result can reuse the predicate computation.
pub const Match = struct {
    consumer_stmt: LIR.CFStmtId,
    result_target: LIR.LocalId,
};

/// Finds the matching arithmetic result in the false arm of an overflow
/// predicate's immediately following Bool switch.
///
/// Only representation-preserving local aliases may separate the false arm
/// from the consumer. That makes the transformation an exact local peephole:
/// no effects or unrelated calculations are moved across the branch.
pub fn findResultConsumer(store: *const LirStore, predicate_stmt: LIR.CFStmtId) ?Match {
    const predicate_cf_stmt = store.getCFStmt(predicate_stmt);
    if (std.meta.activeTag(predicate_cf_stmt) != .assign_low_level) return null;
    const predicate = predicate_cf_stmt.assign_low_level;
    const predicate_entry = CheckedArithmetic.classify(predicate.op) orelse return null;
    if (predicate_entry.mode != .overflows) return null;

    const predicate_args = store.getLocalSpan(predicate.args);
    if (GuardedList.borrowLen(predicate_args) != 2) return null;

    const next_cf_stmt = store.getCFStmt(predicate.next);
    if (std.meta.activeTag(next_cf_stmt) != .switch_stmt) return null;
    const bool_switch = next_cf_stmt.switch_stmt;
    if (bool_switch.cond != predicate.target) return null;

    // Roc Bool uses 0 for False and 1 for True. Lowering commonly emits the
    // True arm explicitly and leaves False as the default, but accept either
    // exact representation.
    var false_arm = bool_switch.default_branch;
    const branches = store.getCFSwitchBranches(bool_switch.branches);
    for (0..GuardedList.borrowLen(branches)) |index| {
        const branch = GuardedList.at(branches, index);
        if (branch.value == 0) false_arm = branch.body;
    }

    var lhs_alias = GuardedList.at(predicate_args, 0);
    var rhs_alias = GuardedList.at(predicate_args, 1);
    var current = false_arm;
    var remaining = store.cfStmtCount();
    while (remaining > 0) : (remaining -= 1) {
        const stmt = store.getCFStmt(current);
        if (std.meta.activeTag(stmt) == .assign_ref) {
            const assign = stmt.assign_ref;
            if (assign.op != .local) return null;
            const source = assign.op.local;
            var followed = false;
            if (source == lhs_alias) {
                lhs_alias = assign.target;
                followed = true;
            }
            if (source == rhs_alias) {
                rhs_alias = assign.target;
                followed = true;
            }
            if (!followed) return null;
            current = assign.next;
            continue;
        }

        if (std.meta.activeTag(stmt) != .assign_low_level) return null;
        const consumer = stmt.assign_low_level;
        const consumer_entry = CheckedArithmetic.classify(consumer.op) orelse return null;
        if (consumer_entry.operation != predicate_entry.operation) return null;
        switch (consumer_entry.mode) {
            .wrap, .crash_on_overflow, .proven_cannot_overflow => {},
            .overflows => return null,
        }

        const consumer_args = store.getLocalSpan(consumer.args);
        if (GuardedList.borrowLen(consumer_args) != 2) return null;
        const consumer_lhs = GuardedList.at(consumer_args, 0);
        const consumer_rhs = GuardedList.at(consumer_args, 1);
        const ordered = consumer_lhs == lhs_alias and consumer_rhs == rhs_alias;
        const swapped = consumer_entry.operation != .sub and
            consumer_lhs == rhs_alias and consumer_rhs == lhs_alias;
        if (!ordered and !swapped) return null;

        return .{ .consumer_stmt = current, .result_target = consumer.target };
    }

    return null;
}

test "finds an overflow result through false-arm aliases" {
    var store = LirStore.init(std.testing.allocator);
    defer store.deinit();

    const lhs = try store.addLocal(.{ .layout_idx = .u64 });
    const rhs = try store.addLocal(.{ .layout_idx = .u64 });
    const overflowed = try store.addLocal(.{ .layout_idx = .bool });
    const lhs_alias = try store.addLocal(.{ .layout_idx = .u64 });
    const rhs_alias = try store.addLocal(.{ .layout_idx = .u64 });
    const result = try store.addLocal(.{ .layout_idx = .u64 });

    const done = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const consumer_args = try store.addLocalSpan(&.{ rhs_alias, lhs_alias });
    const consumer = try store.addCFStmt(.{ .assign_low_level = .{
        .target = result,
        .op = .num_int_add_wrap,
        .rc_effect = .none(),
        .args = consumer_args,
        .next = done,
    } });
    const alias_rhs = try store.addCFStmt(.{ .assign_ref = .{
        .target = rhs_alias,
        .op = .{ .local = rhs },
        .next = consumer,
    } });
    const alias_lhs = try store.addCFStmt(.{ .assign_ref = .{
        .target = lhs_alias,
        .op = .{ .local = lhs },
        .next = alias_rhs,
    } });
    const overflow_path = try store.addCFStmt(.runtime_error);
    const branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = overflow_path }});
    const choose = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = overflowed,
        .branches = branches,
        .default_branch = alias_lhs,
    } });
    const predicate_args = try store.addLocalSpan(&.{ lhs, rhs });
    const predicate = try store.addCFStmt(.{ .assign_low_level = .{
        .target = overflowed,
        .op = .num_int_add_overflows,
        .rc_effect = .none(),
        .args = predicate_args,
        .next = choose,
    } });

    const matched = findResultConsumer(&store, predicate).?;
    try std.testing.expectEqual(consumer, matched.consumer_stmt);
    try std.testing.expectEqual(result, matched.result_target);
}

test "does not commute subtraction operands" {
    var store = LirStore.init(std.testing.allocator);
    defer store.deinit();

    const lhs = try store.addLocal(.{ .layout_idx = .i64 });
    const rhs = try store.addLocal(.{ .layout_idx = .i64 });
    const overflowed = try store.addLocal(.{ .layout_idx = .bool });
    const result = try store.addLocal(.{ .layout_idx = .i64 });

    const done = try store.addCFStmt(.{ .ret = .{ .value = result } });
    const consumer_args = try store.addLocalSpan(&.{ rhs, lhs });
    const consumer = try store.addCFStmt(.{ .assign_low_level = .{
        .target = result,
        .op = .num_int_sub_wrap,
        .rc_effect = .none(),
        .args = consumer_args,
        .next = done,
    } });
    const overflow_path = try store.addCFStmt(.runtime_error);
    const branches = try store.addCFSwitchBranches(&.{.{ .value = 1, .body = overflow_path }});
    const choose = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = overflowed,
        .branches = branches,
        .default_branch = consumer,
    } });
    const predicate_args = try store.addLocalSpan(&.{ lhs, rhs });
    const predicate = try store.addCFStmt(.{ .assign_low_level = .{
        .target = overflowed,
        .op = .num_int_sub_overflows,
        .rc_effect = .none(),
        .args = predicate_args,
        .next = choose,
    } });

    try std.testing.expectEqual(@as(?Match, null), findResultConsumer(&store, predicate));
}
