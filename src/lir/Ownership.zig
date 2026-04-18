//! Ownership fact inference for statement-only LIR.
//!
//! This pass does not invent RC behavior in backends. It computes explicit
//! ownership/provenance facts from the existing statement graph so later
//! lowering passes can emit concrete `incref`/`decref`/`free` statements from a
//! single source of truth.
//!
//! Ownership boundary:
//! - this pass is allowed to reason about ownership because it produces the
//!   explicit ownership facts consumed by later non-builtin stages
//! - backends and the interpreter are not allowed to reconstruct these facts

const std = @import("std");
const layout_mod = @import("layout");
const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const CFStmt = LIR.CFStmt;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const ParamRefContract = LIR.ParamRefContract;
const ProcResultContract = LIR.ProcResultContract;
const ResultSemantics = LIR.ResultSemantics;

/// Public function `inferProcResultContracts`.
pub fn inferProcResultContracts(
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout_mod.Store,
) Allocator.Error!void {
    try propagateRefResultSemantics(allocator, store);
    try normalizeRefLocalOwnership(store, layouts);
    try normalizeAggregateOwnershipFixedPoint(allocator, store, layouts);

    var changed = true;
    while (changed) {
        try propagateCallResultSemantics(allocator, store);
        changed = false;
        const proc_count = store.getProcSpecs().len;
        var proc_index: usize = 0;
        while (proc_index < proc_count) : (proc_index += 1) {
            const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
            const proc = store.getProcSpec(proc_id);
            const inferred = try inferOneProcResultContract(allocator, store, proc);
            if (!procResultContractsEqual(store, proc.result_contract, inferred)) {
                store.getProcSpecPtr(proc_id).result_contract = inferred;
                changed = true;
            }
        }
    }

    try propagateCallResultSemantics(allocator, store);
}

/// Public function `inferProcOwnedParams`.
pub fn inferProcOwnedParams(
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout_mod.Store,
) Allocator.Error!void {
    var changed = true;
    while (changed) {
        changed = false;
        const proc_count = store.getProcSpecs().len;
        var proc_index: usize = 0;
        while (proc_index < proc_count) : (proc_index += 1) {
            const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
            const proc = store.getProcSpec(proc_id);
            const inferred = try inferOneProcOwnedParams(allocator, store, layouts, proc);
            if (!localSpansEqual(store, proc.owned_params, inferred)) {
                store.getProcSpecPtr(proc_id).owned_params = inferred;
                changed = true;
            }
        }
    }
}

fn normalizeAggregateOwnershipFixedPoint(
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout_mod.Store,
) Allocator.Error!void {
    var changed = true;
    while (changed) {
        changed = false;
        const owned_changed = try inferProcOwnedParamsOnePass(allocator, store, layouts);
        const aggregate_changed = try normalizeFreshAggregateOwnership(allocator, store, layouts);
        changed = owned_changed or aggregate_changed;
    }
}

fn inferProcOwnedParamsOnePass(
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout_mod.Store,
) Allocator.Error!bool {
    var changed = false;
    const proc_count = store.getProcSpecs().len;
    var proc_index: usize = 0;
    while (proc_index < proc_count) : (proc_index += 1) {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        const proc = store.getProcSpec(proc_id);
        const inferred = try inferOneProcOwnedParams(allocator, store, layouts, proc);
        if (!localSpansEqual(store, proc.owned_params, inferred)) {
            store.getProcSpecPtr(proc_id).owned_params = inferred;
            changed = true;
        }
    }
    return changed;
}

fn normalizeFreshAggregateOwnership(
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout_mod.Store,
) Allocator.Error!bool {
    var changed = false;
    const proc_count = store.getProcSpecs().len;
    var proc_index: usize = 0;
    while (proc_index < proc_count) : (proc_index += 1) {
        const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
        const proc = store.getProcSpec(proc_id);
        const body = proc.body orelse continue;
        const reachable = try collectReachableStmtIds(allocator, store, body);
        defer allocator.free(reachable);

        for (reachable) |stmt_id| {
            switch (store.getCFStmt(stmt_id)) {
                .assign_list => |assign| {
                    const rewritten = try freshAggregateOwnershipFromInputs(
                        allocator,
                        store,
                        layouts,
                        proc,
                        store.getLocalSpan(assign.elems),
                    );
                    if (ownershipSemanticsEqual(store, assign.ownership, rewritten)) continue;
                    store.getCFStmtPtr(stmt_id).assign_list.ownership = rewritten;
                    changed = true;
                },
                .assign_struct => |assign| {
                    const rewritten = try freshAggregateOwnershipFromInputs(
                        allocator,
                        store,
                        layouts,
                        proc,
                        store.getLocalSpan(assign.fields),
                    );
                    if (ownershipSemanticsEqual(store, assign.ownership, rewritten)) continue;
                    store.getCFStmtPtr(stmt_id).assign_struct.ownership = rewritten;
                    changed = true;
                },
                .assign_tag => |assign| {
                    const payload_slice: []const LocalId = if (assign.payload) |payload| &.{payload} else &.{};
                    const rewritten = try freshAggregateOwnershipFromInputs(
                        allocator,
                        store,
                        layouts,
                        proc,
                        payload_slice,
                    );
                    if (ownershipSemanticsEqual(store, assign.ownership, rewritten)) continue;
                    store.getCFStmtPtr(stmt_id).assign_tag.ownership = rewritten;
                    changed = true;
                },
                else => {},
            }
        }
    }
    return changed;
}

fn freshAggregateOwnershipFromInputs(
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout_mod.Store,
    proc: LIR.LirProcSpec,
    inputs: []const LocalId,
) Allocator.Error!LIR.OwnershipSemantics {
    var consumed_count: usize = 0;
    var retained_count: usize = 0;

    for (inputs) |input| {
        if (!layoutContainsRefcounted(store, layouts, input)) continue;
        switch (try aggregateInputRole(allocator, store, proc, input)) {
            .consume_owned => consumed_count += 1,
            .retain_borrow => retained_count += 1,
        }
    }

    const consumed = try allocator.alloc(LocalId, consumed_count);
    defer allocator.free(consumed);
    const retained = try allocator.alloc(LocalId, retained_count);
    defer allocator.free(retained);

    var consumed_index: usize = 0;
    var retained_index: usize = 0;
    for (inputs) |input| {
        if (!layoutContainsRefcounted(store, layouts, input)) continue;
        switch (try aggregateInputRole(allocator, store, proc, input)) {
            .consume_owned => {
                consumed[consumed_index] = try aggregateConsumedOwner(allocator, store, proc, input);
                consumed_index += 1;
            },
            .retain_borrow => {
                retained[retained_index] = input;
                retained_index += 1;
            },
        }
    }

    return .{
        .materialization = .fresh_aggregate,
        .consumed_owned_inputs = try store.addLocalSpan(consumed),
        .retained_borrows = try store.addLocalSpan(retained),
    };
}

const AggregateInputRole = enum {
    consume_owned,
    retain_borrow,
};

fn aggregateInputRole(
    allocator: Allocator,
    store: *const LirStore,
    proc: LIR.LirProcSpec,
    local: LocalId,
) Allocator.Error!AggregateInputRole {
    var active = std.AutoHashMap(u32, void).init(allocator);
    defer active.deinit();
    return try aggregateInputRoleInner(allocator, store, proc, local, &active);
}

fn aggregateInputRoleInner(
    allocator: Allocator,
    store: *const LirStore,
    proc: LIR.LirProcSpec,
    local: LocalId,
    active: *std.AutoHashMap(u32, void),
) Allocator.Error!AggregateInputRole {
    if (indexOfLocal(store.getLocalSpan(proc.owned_params), local) != null) return .consume_owned;
    if (localResultSemantics(store, local)) |semantics| {
        return switch (semantics) {
            .fresh => .consume_owned,
            .borrow_of => .retain_borrow,
            .alias_of => |alias| if (alias.projections.isEmpty()) .consume_owned else .retain_borrow,
        };
    }

    return (try aggregateJoinParamRole(allocator, store, proc, local, active)) orelse .retain_borrow;
}

fn aggregateConsumedOwner(
    allocator: Allocator,
    store: *const LirStore,
    proc: LIR.LirProcSpec,
    local: LocalId,
) Allocator.Error!LocalId {
    if (indexOfLocal(store.getLocalSpan(proc.owned_params), local) != null) return local;
    if (localResultSemantics(store, local)) |semantics| {
        return switch (semantics) {
            .fresh, .borrow_of => local,
            .alias_of => |alias| if (alias.projections.isEmpty()) alias.owner else local,
        };
    }

    var active = std.AutoHashMap(u32, void).init(allocator);
    defer active.deinit();
    if ((try aggregateJoinParamRole(allocator, store, proc, local, &active)) != null) {
        return local;
    }
    return local;
}

fn aggregateJoinParamRole(
    allocator: Allocator,
    store: *const LirStore,
    proc: LIR.LirProcSpec,
    local: LocalId,
    active: *std.AutoHashMap(u32, void),
) Allocator.Error!?AggregateInputRole {
    const gop = try active.getOrPut(@intFromEnum(local));
    if (gop.found_existing) return null;
    defer _ = active.remove(@intFromEnum(local));

    for (store.cf_stmts.items) |stmt| {
        const join = switch (stmt) {
            .join => |join| join,
            else => continue,
        };

        const params = store.getLocalSpan(join.params);
        const param_index = indexOfLocal(params, local) orelse continue;

        var saw_incoming = false;
        var summary: ?AggregateInputRole = null;
        for (store.cf_stmts.items) |incoming_stmt| {
            const jump = switch (incoming_stmt) {
                .jump => |jump| if (jump.target == join.id) jump else continue,
                else => continue,
            };

            const args = store.getLocalSpan(jump.args);
            if (param_index >= args.len) continue;

            saw_incoming = true;
            const candidate = try aggregateInputRoleInner(allocator, store, proc, args[param_index], active);
            if (summary == null) {
                summary = candidate;
            } else if (summary.? != candidate) {
                return .retain_borrow;
            }
        }

        if (!saw_incoming) return null;
        return summary;
    }

    return null;
}

fn collectReachableStmtIds(
    allocator: Allocator,
    store: *const LirStore,
    body: CFStmtId,
) Allocator.Error![]CFStmtId {
    var ordered = std.ArrayList(CFStmtId).empty;
    defer ordered.deinit(allocator);

    var stack = std.ArrayList(CFStmtId).empty;
    defer stack.deinit(allocator);

    var visited = std.AutoHashMap(u32, void).init(allocator);
    defer visited.deinit();

    try stack.append(allocator, body);
    while (stack.items.len > 0) {
        const stmt_id = stack.pop().?;
        const gop = try visited.getOrPut(@intFromEnum(stmt_id));
        if (gop.found_existing) continue;
        try ordered.append(allocator, stmt_id);

        switch (store.getCFStmt(stmt_id)) {
            .assign_symbol => |assign| try stack.append(allocator, assign.next),
            .assign_ref => |assign| try stack.append(allocator, assign.next),
            .assign_literal => |assign| try stack.append(allocator, assign.next),
            .assign_call => |assign| try stack.append(allocator, assign.next),
            .assign_call_indirect => |assign| try stack.append(allocator, assign.next),
            .assign_low_level => |assign| try stack.append(allocator, assign.next),
            .assign_list => |assign| try stack.append(allocator, assign.next),
            .assign_struct => |assign| try stack.append(allocator, assign.next),
            .assign_tag => |assign| try stack.append(allocator, assign.next),
            .set_local => |assign| try stack.append(allocator, assign.next),
            .debug => |debug_stmt| try stack.append(allocator, debug_stmt.next),
            .expect => |expect_stmt| try stack.append(allocator, expect_stmt.next),
            .incref => |inc| try stack.append(allocator, inc.next),
            .decref => |dec| try stack.append(allocator, dec.next),
            .free => |free_stmt| try stack.append(allocator, free_stmt.next),
            .switch_stmt => |sw| {
                try stack.append(allocator, sw.default_branch);
                for (store.getCFSwitchBranches(sw.branches)) |branch| {
                    try stack.append(allocator, branch.body);
                }
            },
            .borrow_scope => |scope| {
                try stack.append(allocator, scope.body);
                try stack.append(allocator, scope.remainder);
            },
            .for_list => |for_stmt| {
                try stack.append(allocator, for_stmt.body);
                try stack.append(allocator, for_stmt.next);
            },
            .join => |join| {
                try stack.append(allocator, join.body);
                try stack.append(allocator, join.remainder);
            },
            .jump, .loop_continue, .scope_exit, .ret, .runtime_error, .crash => {},
        }
    }

    return ordered.toOwnedSlice(allocator);
}

fn inferOneProcResultContract(
    allocator: Allocator,
    store: *LirStore,
    proc: LIR.LirProcSpec,
) Allocator.Error!ProcResultContract {
    if (proc.body == null) return .fresh;

    const args = store.getLocalSpan(proc.args);
    const owned_params = store.getLocalSpan(proc.owned_params);
    var arg_index_by_local = std.AutoHashMap(u32, usize).init(allocator);
    defer arg_index_by_local.deinit();
    try arg_index_by_local.ensureTotalCapacity(@intCast(args.len));
    for (args, 0..) |arg_local, i| {
        arg_index_by_local.putAssumeCapacity(@intFromEnum(arg_local), i);
    }

    var owned_param_locals = std.AutoHashMap(u32, void).init(allocator);
    defer owned_param_locals.deinit();
    try owned_param_locals.ensureTotalCapacity(@intCast(owned_params.len));
    for (owned_params) |owned_param| {
        owned_param_locals.putAssumeCapacity(@intFromEnum(owned_param), {});
    }

    var visited = std.AutoHashMap(u32, void).init(allocator);
    defer visited.deinit();
    var summaries = std.ArrayList(ProcResultContract).empty;
    defer summaries.deinit(allocator);

    try collectReturnContracts(
        allocator,
        store,
        proc.body.?,
        &arg_index_by_local,
        &owned_param_locals,
        &visited,
        &summaries,
    );

    if (summaries.items.len == 0) return .no_return;

    const first = summaries.items[0];
    for (summaries.items[1..]) |summary| {
        if (!procResultContractsEqual(store, first, summary)) return .fresh;
    }
    return first;
}

fn inferOneProcOwnedParams(
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout_mod.Store,
    proc: LIR.LirProcSpec,
) Allocator.Error!LIR.LocalSpan {
    const args = store.getLocalSpan(proc.args);
    if (args.len == 0) return .empty();
    if (proc.body == null) return proc.owned_params;

    var arg_index_by_local = std.AutoHashMap(u32, usize).init(allocator);
    defer arg_index_by_local.deinit();
    try arg_index_by_local.ensureTotalCapacity(@intCast(args.len));
    for (args, 0..) |arg_local, i| {
        arg_index_by_local.putAssumeCapacity(@intFromEnum(arg_local), i);
    }

    const needs_owned = try allocator.alloc(bool, args.len);
    defer allocator.free(needs_owned);
    @memset(needs_owned, false);

    var visited = std.AutoHashMap(u32, void).init(allocator);
    defer visited.deinit();
    var active = std.AutoHashMap(u32, void).init(allocator);
    defer active.deinit();

    try collectOwnedParams(
        allocator,
        store,
        proc.body.?,
        &arg_index_by_local,
        &visited,
        &active,
        needs_owned,
    );
    visited.clearRetainingCapacity();
    try collectReturnedRefcountedProjectionParams(
        allocator,
        store,
        layouts,
        proc.body.?,
        &arg_index_by_local,
        &visited,
        needs_owned,
    );

    var owned = std.ArrayList(LocalId).empty;
    defer owned.deinit(allocator);
    for (args, 0..) |arg_local, i| {
        if (needs_owned[i]) {
            try owned.append(allocator, arg_local);
        }
    }
    return try store.addLocalSpan(owned.items);
}

fn collectReturnedRefcountedProjectionParams(
    allocator: Allocator,
    store: *LirStore,
    layouts: *const layout_mod.Store,
    stmt_id: CFStmtId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    visited: *std.AutoHashMap(u32, void),
    needs_owned: []bool,
) Allocator.Error!void {
    const gop = try visited.getOrPut(@intFromEnum(stmt_id));
    if (gop.found_existing) return;

    switch (store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, assign.next, arg_index_by_local, visited, needs_owned),
        .assign_ref => |assign| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, assign.next, arg_index_by_local, visited, needs_owned),
        .assign_literal => |assign| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, assign.next, arg_index_by_local, visited, needs_owned),
        .assign_call => |assign| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, assign.next, arg_index_by_local, visited, needs_owned),
        .assign_call_indirect => |assign| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, assign.next, arg_index_by_local, visited, needs_owned),
        .assign_low_level => |assign| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, assign.next, arg_index_by_local, visited, needs_owned),
        .assign_list => |assign| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, assign.next, arg_index_by_local, visited, needs_owned),
        .assign_struct => |assign| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, assign.next, arg_index_by_local, visited, needs_owned),
        .assign_tag => |assign| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, assign.next, arg_index_by_local, visited, needs_owned),
        .set_local => |assign| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, assign.next, arg_index_by_local, visited, needs_owned),
        .debug => |debug_stmt| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, debug_stmt.next, arg_index_by_local, visited, needs_owned),
        .expect => |expect_stmt| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, expect_stmt.next, arg_index_by_local, visited, needs_owned),
        .incref => |inc| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, inc.next, arg_index_by_local, visited, needs_owned),
        .decref => |dec| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, dec.next, arg_index_by_local, visited, needs_owned),
        .free => |free_stmt| try collectReturnedRefcountedProjectionParams(allocator, store, layouts, free_stmt.next, arg_index_by_local, visited, needs_owned),
        .switch_stmt => |sw| {
            for (store.getCFSwitchBranches(sw.branches)) |branch| {
                try collectReturnedRefcountedProjectionParams(allocator, store, layouts, branch.body, arg_index_by_local, visited, needs_owned);
            }
            try collectReturnedRefcountedProjectionParams(allocator, store, layouts, sw.default_branch, arg_index_by_local, visited, needs_owned);
        },
        .borrow_scope => |scope| {
            try collectReturnedRefcountedProjectionParams(allocator, store, layouts, scope.body, arg_index_by_local, visited, needs_owned);
            try collectReturnedRefcountedProjectionParams(allocator, store, layouts, scope.remainder, arg_index_by_local, visited, needs_owned);
        },
        .for_list => |for_stmt| {
            try collectReturnedRefcountedProjectionParams(allocator, store, layouts, for_stmt.body, arg_index_by_local, visited, needs_owned);
            try collectReturnedRefcountedProjectionParams(allocator, store, layouts, for_stmt.next, arg_index_by_local, visited, needs_owned);
        },
        .join => |join| {
            try collectReturnedRefcountedProjectionParams(allocator, store, layouts, join.body, arg_index_by_local, visited, needs_owned);
            try collectReturnedRefcountedProjectionParams(allocator, store, layouts, join.remainder, arg_index_by_local, visited, needs_owned);
        },
        .jump, .runtime_error, .scope_exit, .crash, .loop_continue => {},
        .ret => |ret_stmt| {
            if (!layoutContainsRefcounted(store, layouts, ret_stmt.value)) return;

            var returned_active = std.AutoHashMap(u32, void).init(allocator);
            defer returned_active.deinit();
            try collectReturnedFreshOwnerParams(
                allocator,
                store,
                ret_stmt.value,
                arg_index_by_local,
                &returned_active,
                needs_owned,
            );

            var active = std.AutoHashMap(u32, void).init(allocator);
            defer active.deinit();
            var owned_param_locals = std.AutoHashMap(u32, void).init(allocator);
            defer owned_param_locals.deinit();
            const contract = try inferLocalContract(
                allocator,
                store,
                ret_stmt.value,
                arg_index_by_local,
                &owned_param_locals,
                &active,
            );

            switch (contract) {
                .alias_of_param => |param| {
                    if (!param.projections.isEmpty()) needs_owned[param.param_index] = true;
                },
                .borrow_of_param => |param| {
                    if (!param.projections.isEmpty()) needs_owned[param.param_index] = true;
                },
                else => {},
            }
        },
    }
}

fn collectReturnedFreshOwnerParams(
    allocator: Allocator,
    store: *LirStore,
    local: LocalId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    active: *std.AutoHashMap(u32, void),
    needs_owned: []bool,
) Allocator.Error!void {
    if (arg_index_by_local.contains(@intFromEnum(local))) {
        try markLocalParamOwned(allocator, store, local, arg_index_by_local, active, needs_owned);
        return;
    }

    const gop = try active.getOrPut(@intFromEnum(local));
    if (gop.found_existing) return;
    defer _ = active.remove(@intFromEnum(local));

    if (try collectReturnedJoinOwnerParams(allocator, store, local, arg_index_by_local, active, needs_owned)) return;

    const producer = findProducer(store, local) orelse return;
    switch (producer) {
        .assign_ref => |assign| {
            try markSpanParamsOwned(allocator, store, assign.ownership.consumed_owned_inputs, arg_index_by_local, active, needs_owned);
            try markSpanParamsOwned(allocator, store, assign.ownership.retained_borrows, arg_index_by_local, active, needs_owned);

            switch (assign.result) {
                .alias_of => |alias| try collectReturnedFreshOwnerParams(allocator, store, alias.owner, arg_index_by_local, active, needs_owned),
                .borrow_of => |borrow| try collectReturnedFreshOwnerParams(allocator, store, borrow.owner, arg_index_by_local, active, needs_owned),
                .fresh => {},
            }
        },
        .assign_call => |assign| {
            const callee = store.getProcSpec(assign.proc);
            const owned_params = store.getLocalSpan(callee.owned_params);
            const args = store.getLocalSpan(callee.args);
            const call_args = store.getLocalSpan(assign.args);
            for (owned_params) |owned_param| {
                const owned_index = indexOfLocal(args, owned_param) orelse continue;
                if (owned_index >= call_args.len) continue;
                try markLocalParamOwned(allocator, store, call_args[owned_index], arg_index_by_local, active, needs_owned);
            }

            switch (assign.result) {
                .alias_of => |alias| try collectReturnedFreshOwnerParams(allocator, store, alias.owner, arg_index_by_local, active, needs_owned),
                .borrow_of => |borrow| try collectReturnedFreshOwnerParams(allocator, store, borrow.owner, arg_index_by_local, active, needs_owned),
                .fresh => {},
            }
        },
        .assign_call_indirect => |assign| {
            try markSpanParamsOwned(allocator, store, assign.ownership.consumed_owned_inputs, arg_index_by_local, active, needs_owned);
            try markSpanParamsOwned(allocator, store, assign.ownership.retained_borrows, arg_index_by_local, active, needs_owned);
        },
        .assign_low_level => |assign| {
            try markSpanParamsOwned(allocator, store, assign.ownership.consumed_owned_inputs, arg_index_by_local, active, needs_owned);
            try markSpanParamsOwned(allocator, store, assign.ownership.retained_borrows, arg_index_by_local, active, needs_owned);

            switch (assign.result) {
                .alias_of => |alias| try collectReturnedFreshOwnerParams(allocator, store, alias.owner, arg_index_by_local, active, needs_owned),
                .borrow_of => |borrow| try collectReturnedFreshOwnerParams(allocator, store, borrow.owner, arg_index_by_local, active, needs_owned),
                .fresh => {},
            }
        },
        .assign_list => |assign| {
            try markSpanParamsOwned(allocator, store, assign.ownership.consumed_owned_inputs, arg_index_by_local, active, needs_owned);
            try markSpanParamsOwned(allocator, store, assign.ownership.retained_borrows, arg_index_by_local, active, needs_owned);
            for (store.getLocalSpan(assign.elems)) |elem| {
                try collectReturnedFreshOwnerParams(allocator, store, elem, arg_index_by_local, active, needs_owned);
            }
        },
        .assign_struct => |assign| {
            try markSpanParamsOwned(allocator, store, assign.ownership.consumed_owned_inputs, arg_index_by_local, active, needs_owned);
            try markSpanParamsOwned(allocator, store, assign.ownership.retained_borrows, arg_index_by_local, active, needs_owned);
            for (store.getLocalSpan(assign.fields)) |field| {
                try collectReturnedFreshOwnerParams(allocator, store, field, arg_index_by_local, active, needs_owned);
            }
        },
        .assign_tag => |assign| {
            try markSpanParamsOwned(allocator, store, assign.ownership.consumed_owned_inputs, arg_index_by_local, active, needs_owned);
            try markSpanParamsOwned(allocator, store, assign.ownership.retained_borrows, arg_index_by_local, active, needs_owned);
            if (assign.payload) |payload| {
                try collectReturnedFreshOwnerParams(allocator, store, payload, arg_index_by_local, active, needs_owned);
            }
        },
        .for_list => |for_stmt| {
            try markSpanParamsOwned(allocator, store, for_stmt.elem_ownership.consumed_owned_inputs, arg_index_by_local, active, needs_owned);
            try markSpanParamsOwned(allocator, store, for_stmt.elem_ownership.retained_borrows, arg_index_by_local, active, needs_owned);

            switch (for_stmt.elem_result) {
                .alias_of => |alias| try collectReturnedFreshOwnerParams(allocator, store, alias.owner, arg_index_by_local, active, needs_owned),
                .borrow_of => |borrow| try collectReturnedFreshOwnerParams(allocator, store, borrow.owner, arg_index_by_local, active, needs_owned),
                .fresh => {},
            }
        },
        .set_local => |assign| try collectReturnedFreshOwnerParams(allocator, store, assign.value, arg_index_by_local, active, needs_owned),
        .assign_symbol,
        .assign_literal,
        .debug,
        .expect,
        .incref,
        .decref,
        .free,
        .switch_stmt,
        .borrow_scope,
        .jump,
        .ret,
        .runtime_error,
        .scope_exit,
        .crash,
        .loop_continue,
        => {},
        .join => unreachable,
    }
}

fn collectReturnedJoinOwnerParams(
    allocator: Allocator,
    store: *LirStore,
    local: LocalId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    active: *std.AutoHashMap(u32, void),
    needs_owned: []bool,
) Allocator.Error!bool {
    for (store.cf_stmts.items) |stmt| {
        const join = switch (stmt) {
            .join => |join| join,
            else => continue,
        };

        const params = store.getLocalSpan(join.params);
        const param_index = indexOfLocal(params, local) orelse continue;

        for (store.cf_stmts.items) |incoming_stmt| {
            const jump = switch (incoming_stmt) {
                .jump => |jump| if (jump.target == join.id) jump else continue,
                else => continue,
            };

            const args = store.getLocalSpan(jump.args);
            if (param_index >= args.len) continue;
            try collectReturnedFreshOwnerParams(allocator, store, args[param_index], arg_index_by_local, active, needs_owned);
        }

        return true;
    }

    return false;
}

fn collectOwnedParams(
    allocator: Allocator,
    store: *LirStore,
    stmt_id: CFStmtId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    visited: *std.AutoHashMap(u32, void),
    active: *std.AutoHashMap(u32, void),
    needs_owned: []bool,
) Allocator.Error!void {
    const gop = try visited.getOrPut(@intFromEnum(stmt_id));
    if (gop.found_existing) return;

    switch (store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| try collectOwnedParams(allocator, store, assign.next, arg_index_by_local, visited, active, needs_owned),
        .assign_ref => |assign| try collectOwnedParams(allocator, store, assign.next, arg_index_by_local, visited, active, needs_owned),
        .assign_literal => |assign| try collectOwnedParams(allocator, store, assign.next, arg_index_by_local, visited, active, needs_owned),
        .assign_call => |assign| {
            const callee = store.getProcSpec(assign.proc);
            const call_args = store.getLocalSpan(assign.args);
            const owned_params = store.getLocalSpan(callee.owned_params);
            for (owned_params) |owned_param| {
                const owned_index = indexOfLocal(store.getLocalSpan(callee.args), owned_param) orelse continue;
                if (owned_index >= call_args.len) continue;
                try markLocalParamOwned(allocator, store, call_args[owned_index], arg_index_by_local, active, needs_owned);
            }
            try collectOwnedParams(allocator, store, assign.next, arg_index_by_local, visited, active, needs_owned);
        },
        .assign_call_indirect => |assign| {
            for (store.getLocalSpan(assign.args)) |arg_local| {
                try markLocalParamOwned(allocator, store, arg_local, arg_index_by_local, active, needs_owned);
            }
            try collectOwnedParams(allocator, store, assign.next, arg_index_by_local, visited, active, needs_owned);
        },
        .assign_low_level => |assign| {
            try markSpanParamsOwned(allocator, store, assign.ownership.consumed_owned_inputs, arg_index_by_local, active, needs_owned);
            try collectOwnedParams(allocator, store, assign.next, arg_index_by_local, visited, active, needs_owned);
        },
        .assign_list => |assign| try collectOwnedParams(allocator, store, assign.next, arg_index_by_local, visited, active, needs_owned),
        .assign_struct => |assign| try collectOwnedParams(allocator, store, assign.next, arg_index_by_local, visited, active, needs_owned),
        .assign_tag => |assign| try collectOwnedParams(allocator, store, assign.next, arg_index_by_local, visited, active, needs_owned),
        .set_local => |assign| try collectOwnedParams(allocator, store, assign.next, arg_index_by_local, visited, active, needs_owned),
        .debug => |debug_stmt| try collectOwnedParams(allocator, store, debug_stmt.next, arg_index_by_local, visited, active, needs_owned),
        .expect => |expect_stmt| try collectOwnedParams(allocator, store, expect_stmt.next, arg_index_by_local, visited, active, needs_owned),
        .incref => |inc| try collectOwnedParams(allocator, store, inc.next, arg_index_by_local, visited, active, needs_owned),
        .decref => |dec| try collectOwnedParams(allocator, store, dec.next, arg_index_by_local, visited, active, needs_owned),
        .free => |free_stmt| try collectOwnedParams(allocator, store, free_stmt.next, arg_index_by_local, visited, active, needs_owned),
        .switch_stmt => |sw| {
            for (store.getCFSwitchBranches(sw.branches)) |branch| {
                try collectOwnedParams(allocator, store, branch.body, arg_index_by_local, visited, active, needs_owned);
            }
            try collectOwnedParams(allocator, store, sw.default_branch, arg_index_by_local, visited, active, needs_owned);
        },
        .borrow_scope => |scope| {
            try collectOwnedParams(allocator, store, scope.body, arg_index_by_local, visited, active, needs_owned);
            try collectOwnedParams(allocator, store, scope.remainder, arg_index_by_local, visited, active, needs_owned);
        },
        .for_list => |for_stmt| {
            try collectOwnedParams(allocator, store, for_stmt.body, arg_index_by_local, visited, active, needs_owned);
            try collectOwnedParams(allocator, store, for_stmt.next, arg_index_by_local, visited, active, needs_owned);
        },
        .join => |join| {
            try collectOwnedParams(allocator, store, join.body, arg_index_by_local, visited, active, needs_owned);
            try collectOwnedParams(allocator, store, join.remainder, arg_index_by_local, visited, active, needs_owned);
        },
        .jump, .ret, .runtime_error, .scope_exit, .crash, .loop_continue => {},
    }
}

fn markSpanParamsOwned(
    allocator: Allocator,
    store: *LirStore,
    span: LIR.LocalSpan,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    active: *std.AutoHashMap(u32, void),
    needs_owned: []bool,
) Allocator.Error!void {
    for (store.getLocalSpan(span)) |local| {
        try markLocalParamOwned(allocator, store, local, arg_index_by_local, active, needs_owned);
    }
}

fn markLocalParamOwned(
    allocator: Allocator,
    store: *LirStore,
    local: LocalId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    active: *std.AutoHashMap(u32, void),
    needs_owned: []bool,
) Allocator.Error!void {
    if (try inferOwnedParamSource(allocator, store, local, arg_index_by_local, active)) |param_index| {
        needs_owned[param_index] = true;
    }
}

fn collectReturnContracts(
    allocator: Allocator,
    store: *LirStore,
    stmt_id: CFStmtId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    owned_param_locals: *const std.AutoHashMap(u32, void),
    visited: *std.AutoHashMap(u32, void),
    out: *std.ArrayList(ProcResultContract),
) Allocator.Error!void {
    const gop = try visited.getOrPut(@intFromEnum(stmt_id));
    if (gop.found_existing) return;

    switch (store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, owned_param_locals, visited, out),
        .assign_ref => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, owned_param_locals, visited, out),
        .assign_literal => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, owned_param_locals, visited, out),
        .assign_call => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, owned_param_locals, visited, out),
        .assign_call_indirect => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, owned_param_locals, visited, out),
        .assign_low_level => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, owned_param_locals, visited, out),
        .assign_list => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, owned_param_locals, visited, out),
        .assign_struct => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, owned_param_locals, visited, out),
        .assign_tag => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, owned_param_locals, visited, out),
        .set_local => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, owned_param_locals, visited, out),
        .debug => |debug_stmt| try collectReturnContracts(allocator, store, debug_stmt.next, arg_index_by_local, owned_param_locals, visited, out),
        .expect => |expect_stmt| try collectReturnContracts(allocator, store, expect_stmt.next, arg_index_by_local, owned_param_locals, visited, out),
        .incref => |inc| try collectReturnContracts(allocator, store, inc.next, arg_index_by_local, owned_param_locals, visited, out),
        .decref => |dec| try collectReturnContracts(allocator, store, dec.next, arg_index_by_local, owned_param_locals, visited, out),
        .free => |free_stmt| try collectReturnContracts(allocator, store, free_stmt.next, arg_index_by_local, owned_param_locals, visited, out),
        .switch_stmt => |sw| {
            for (store.getCFSwitchBranches(sw.branches)) |branch| {
                try collectReturnContracts(allocator, store, branch.body, arg_index_by_local, owned_param_locals, visited, out);
            }
            try collectReturnContracts(allocator, store, sw.default_branch, arg_index_by_local, owned_param_locals, visited, out);
        },
        .borrow_scope => |scope| {
            try collectReturnContracts(allocator, store, scope.body, arg_index_by_local, owned_param_locals, visited, out);
            try collectReturnContracts(allocator, store, scope.remainder, arg_index_by_local, owned_param_locals, visited, out);
        },
        .for_list => |for_stmt| {
            try collectReturnContracts(allocator, store, for_stmt.body, arg_index_by_local, owned_param_locals, visited, out);
            try collectReturnContracts(allocator, store, for_stmt.next, arg_index_by_local, owned_param_locals, visited, out);
        },
        .join => |join| {
            try collectReturnContracts(allocator, store, join.body, arg_index_by_local, owned_param_locals, visited, out);
            try collectReturnContracts(allocator, store, join.remainder, arg_index_by_local, owned_param_locals, visited, out);
        },
        .jump, .runtime_error, .scope_exit, .crash, .loop_continue => {},
        .ret => |ret_stmt| {
            try out.append(allocator, try inferReturnedLocalContract(allocator, store, ret_stmt.value, arg_index_by_local, owned_param_locals));
        },
    }
}

fn inferReturnedLocalContract(
    allocator: Allocator,
    store: *LirStore,
    local: LocalId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    owned_param_locals: *const std.AutoHashMap(u32, void),
) Allocator.Error!ProcResultContract {
    var active = std.AutoHashMap(u32, void).init(allocator);
    defer active.deinit();
    return inferLocalContract(allocator, store, local, arg_index_by_local, owned_param_locals, &active);
}

fn inferOwnedParamSource(
    allocator: Allocator,
    store: *LirStore,
    local: LocalId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    active: *std.AutoHashMap(u32, void),
) Allocator.Error!?usize {
    if (arg_index_by_local.get(@intFromEnum(local))) |param_index| {
        return param_index;
    }

    const gop = try active.getOrPut(@intFromEnum(local));
    if (gop.found_existing) return null;
    defer _ = active.remove(@intFromEnum(local));

    if (try inferJoinOwnedParamSource(allocator, store, local, arg_index_by_local, active)) |param_index| {
        return param_index;
    }

    const producer = findProducer(store, local) orelse return null;
    return switch (producer) {
        .assign_ref => |assign| blk: {
            switch (assign.result) {
                .alias_of => |alias| break :blk try inferOwnedParamSource(allocator, store, alias.owner, arg_index_by_local, active),
                .borrow_of => |borrow| break :blk try inferOwnedParamSource(allocator, store, borrow.owner, arg_index_by_local, active),
                .fresh => {},
            }

            if (assign.ownership.materialization != .direct) break :blk null;
            const consumed = store.getLocalSpan(assign.ownership.consumed_owned_inputs);
            if (consumed.len != 1) break :blk null;
            break :blk try inferOwnedParamSource(allocator, store, consumed[0], arg_index_by_local, active);
        },
        .set_local => |assign| try inferOwnedParamSource(allocator, store, assign.value, arg_index_by_local, active),
        .join => null,
        else => null,
    };
}

fn inferJoinOwnedParamSource(
    allocator: Allocator,
    store: *LirStore,
    local: LocalId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    active: *std.AutoHashMap(u32, void),
) Allocator.Error!?usize {
    for (store.cf_stmts.items) |stmt| {
        const join = switch (stmt) {
            .join => |join| join,
            else => continue,
        };

        const params = store.getLocalSpan(join.params);
        const param_index = indexOfLocal(params, local) orelse continue;

        var saw_incoming = false;
        var summary: ?usize = null;

        for (store.cf_stmts.items) |incoming_stmt| {
            const jump = switch (incoming_stmt) {
                .jump => |jump| if (jump.target == join.id) jump else continue,
                else => continue,
            };

            const args = store.getLocalSpan(jump.args);
            if (param_index >= args.len) continue;

            saw_incoming = true;
            const candidate = try inferOwnedParamSource(allocator, store, args[param_index], arg_index_by_local, active);
            if (summary == null) {
                summary = candidate;
            } else if (summary != candidate) {
                return null;
            }
        }

        if (!saw_incoming) return null;
        return summary;
    }

    return null;
}

fn inferLocalContract(
    allocator: Allocator,
    store: *LirStore,
    local: LocalId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    owned_param_locals: *const std.AutoHashMap(u32, void),
    active: *std.AutoHashMap(u32, void),
) Allocator.Error!ProcResultContract {
    if (arg_index_by_local.get(@intFromEnum(local))) |param_index| {
        return if (owned_param_locals.contains(@intFromEnum(local)))
            .{ .alias_of_param = .{ .param_index = @intCast(param_index) } }
        else
            .{ .borrow_of_param = .{ .param_index = @intCast(param_index) } };
    }

    const gop = try active.getOrPut(@intFromEnum(local));
    if (gop.found_existing) return .fresh;
    defer _ = active.remove(@intFromEnum(local));

    if (try inferJoinParamContract(allocator, store, local, arg_index_by_local, owned_param_locals, active)) |contract| {
        return contract;
    }

    const producer = findProducer(store, local) orelse return .fresh;
    return switch (producer) {
        .assign_ref => |assign| try contractFromResultSemantics(allocator, store, assign.result, arg_index_by_local, owned_param_locals, active),
        .assign_call => |assign| if (assign.result == .fresh)
            store.getProcSpec(assign.proc).result_contract
        else
            try contractFromResultSemantics(allocator, store, assign.result, arg_index_by_local, owned_param_locals, active),
        .assign_call_indirect => |assign| try contractFromResultSemantics(allocator, store, assign.result, arg_index_by_local, owned_param_locals, active),
        .assign_low_level => |assign| try contractFromResultSemantics(allocator, store, assign.result, arg_index_by_local, owned_param_locals, active),
        .assign_literal,
        .assign_symbol,
        .assign_list,
        .assign_struct,
        .assign_tag,
        => .fresh,
        .for_list => |for_stmt| try contractFromResultSemantics(allocator, store, for_stmt.elem_result, arg_index_by_local, owned_param_locals, active),
        .set_local,
        .debug,
        .expect,
        .incref,
        .decref,
        .free,
        .switch_stmt,
        .borrow_scope,
        .join,
        .jump,
        .ret,
        .runtime_error,
        .scope_exit,
        .crash,
        .loop_continue,
        => .fresh,
    };
}

fn contractFromResultSemantics(
    allocator: Allocator,
    store: *LirStore,
    semantics: ResultSemantics,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    owned_param_locals: *const std.AutoHashMap(u32, void),
    active: *std.AutoHashMap(u32, void),
) Allocator.Error!ProcResultContract {
    return switch (semantics) {
        .fresh => .fresh,
        .alias_of => |alias| blk: {
            var contract = try inferLocalContract(allocator, store, alias.owner, arg_index_by_local, owned_param_locals, active);
            switch (contract) {
                .alias_of_param => |*param| {
                    param.projections = try appendProjectionSpan(allocator, store, param.projections, alias.projections);
                    break :blk contract;
                },
                .borrow_of_param => |*param| {
                    param.projections = try appendProjectionSpan(allocator, store, param.projections, alias.projections);
                    break :blk contract;
                },
                else => break :blk .fresh,
            }
        },
        .borrow_of => |borrow| blk: {
            var contract = try inferLocalContract(allocator, store, borrow.owner, arg_index_by_local, owned_param_locals, active);
            switch (contract) {
                .alias_of_param => |param| {
                    break :blk .{ .borrow_of_param = .{
                        .param_index = param.param_index,
                        .projections = try appendProjectionSpan(allocator, store, param.projections, borrow.projections),
                    } };
                },
                .borrow_of_param => |*param| {
                    param.projections = try appendProjectionSpan(allocator, store, param.projections, borrow.projections);
                    break :blk contract;
                },
                else => break :blk .fresh,
            }
        },
    };
}

fn inferJoinParamContract(
    allocator: Allocator,
    store: *LirStore,
    local: LocalId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    owned_param_locals: *const std.AutoHashMap(u32, void),
    active: *std.AutoHashMap(u32, void),
) Allocator.Error!?ProcResultContract {
    for (store.cf_stmts.items) |stmt| {
        const join = switch (stmt) {
            .join => |join| join,
            else => continue,
        };

        const params = store.getLocalSpan(join.params);
        const param_index = blk: {
            for (params, 0..) |param, i| {
                if (param == local) break :blk i;
            }
            continue;
        };

        var summary: ?ProcResultContract = null;
        var saw_incoming = false;
        for (store.cf_stmts.items) |incoming_stmt| {
            const jump = switch (incoming_stmt) {
                .jump => |jump| if (jump.target == join.id) jump else continue,
                else => continue,
            };

            const args = store.getLocalSpan(jump.args);
            if (param_index >= args.len) return .fresh;
            saw_incoming = true;
            const candidate = try inferLocalContract(allocator, store, args[param_index], arg_index_by_local, owned_param_locals, active);
            if (summary) |existing| {
                if (!procResultContractsEqual(store, existing, candidate)) return .fresh;
            } else {
                summary = candidate;
            }
        }

        if (!saw_incoming) return .fresh;
        return summary orelse .fresh;
    }

    return null;
}

fn findProducer(store: *const LirStore, target: LocalId) ?CFStmt {
    for (store.cf_stmts.items) |stmt| {
        switch (stmt) {
            .assign_symbol => |assign| if (assign.target == target) return stmt,
            .assign_ref => |assign| if (assign.target == target) return stmt,
            .assign_literal => |assign| if (assign.target == target) return stmt,
            .assign_call => |assign| if (assign.target == target) return stmt,
            .assign_call_indirect => |assign| if (assign.target == target) return stmt,
            .assign_low_level => |assign| if (assign.target == target) return stmt,
            .assign_list => |assign| if (assign.target == target) return stmt,
            .assign_struct => |assign| if (assign.target == target) return stmt,
            .assign_tag => |assign| if (assign.target == target) return stmt,
            .for_list => |for_stmt| if (for_stmt.elem == target) return stmt,
            else => {},
        }
    }
    return null;
}

fn procResultContractsEqual(store: *const LirStore, a: ProcResultContract, b: ProcResultContract) bool {
    return switch (a) {
        .fresh => b == .fresh,
        .no_return => b == .no_return,
        .alias_of_param => |left| switch (b) {
            .alias_of_param => |right| left.param_index == right.param_index and
                refProjectionSpansEqual(store, left.projections, right.projections),
            else => false,
        },
        .borrow_of_param => |left| switch (b) {
            .borrow_of_param => |right| left.param_index == right.param_index and
                refProjectionSpansEqual(store, left.projections, right.projections),
            else => false,
        },
    };
}

fn localSpansEqual(store: *const LirStore, a: LIR.LocalSpan, b: LIR.LocalSpan) bool {
    return std.mem.eql(LocalId, store.getLocalSpan(a), store.getLocalSpan(b));
}

fn indexOfLocal(locals: []const LocalId, target: LocalId) ?usize {
    for (locals, 0..) |local, i| {
        if (local == target) return i;
    }
    return null;
}

fn propagateCallResultSemantics(
    allocator: Allocator,
    store: *LirStore,
) Allocator.Error!void {
    for (store.cf_stmts.items, 0..) |stmt, i| {
        switch (stmt) {
            .assign_call => |assign| {
                const summary = store.getProcSpec(assign.proc).result_contract;
                const next_semantics: ResultSemantics = switch (summary) {
                    .fresh, .no_return => .fresh,
                    .alias_of_param => |contract| try contractResultSemantics(allocator, store, assign.args, contract, false),
                    .borrow_of_param => |contract| try contractResultSemantics(allocator, store, assign.args, contract, true),
                };
                store.cf_stmts.items[i] = .{ .assign_call = .{
                    .target = assign.target,
                    .result = next_semantics,
                    .proc = assign.proc,
                    .args = assign.args,
                    .next = assign.next,
                } };
            },
            else => {},
        }
    }
}

fn propagateRefResultSemantics(
    allocator: Allocator,
    store: *LirStore,
) Allocator.Error!void {
    var changed = true;
    while (changed) {
        changed = false;
        for (store.cf_stmts.items, 0..) |stmt, i| {
            switch (stmt) {
                .assign_ref => |assign| {
                    if (assign.op == .local) continue;
                    const next_semantics = try refOpResultSemantics(allocator, store, assign.op);
                    const next_ownership = try refOpOwnership(store, next_semantics, assign.op);
                    if (resultSemanticsEqual(store, next_semantics, assign.result) and
                        ownershipSemanticsEqual(store, assign.ownership, next_ownership)) continue;
                    store.cf_stmts.items[i] = .{ .assign_ref = .{
                        .target = assign.target,
                        .result = next_semantics,
                        .ownership = next_ownership,
                        .op = assign.op,
                        .next = assign.next,
                    } };
                    changed = true;
                },
                else => {},
            }
        }
    }
}

fn normalizeRefLocalOwnership(
    store: *LirStore,
    layouts: *const layout_mod.Store,
) Allocator.Error!void {
    for (store.cf_stmts.items, 0..) |stmt, i| {
        const assign = switch (stmt) {
            .assign_ref => |assign| assign,
            else => continue,
        };

        const source = switch (assign.op) {
            .local => |source| source,
            else => continue,
        };

        if (assign.result != .fresh) continue;
        if (!layoutContainsRefcounted(store, layouts, assign.target)) {
            const empty = LIR.OwnershipSemantics{};
            if (ownershipSemanticsEqual(store, assign.ownership, empty)) continue;
            store.cf_stmts.items[i] = .{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .ownership = empty,
                .op = assign.op,
                .next = assign.next,
            } };
            continue;
        }

        const rewritten_ownership = blk: {
            const semantics = localResultSemantics(store, source) orelse break :blk LIR.OwnershipSemantics{
                .consumed_owned_inputs = try store.addLocalSpan(&.{source}),
            };

            switch (semantics) {
                .fresh => break :blk LIR.OwnershipSemantics{
                    .consumed_owned_inputs = try store.addLocalSpan(&.{source}),
                },
                .alias_of => |alias| {
                    if (alias.projections.isEmpty()) {
                        break :blk LIR.OwnershipSemantics{
                            .consumed_owned_inputs = try store.addLocalSpan(&.{alias.owner}),
                        };
                    }

                    break :blk LIR.OwnershipSemantics{
                        .materialization = .copy_from_borrowed_input,
                        .retained_borrows = try store.addLocalSpan(&.{source}),
                    };
                },
                .borrow_of => break :blk LIR.OwnershipSemantics{
                    .materialization = .copy_from_borrowed_input,
                    .retained_borrows = try store.addLocalSpan(&.{source}),
                },
            }
        };

        if (ownershipSemanticsEqual(store, assign.ownership, rewritten_ownership)) continue;

        store.cf_stmts.items[i] = .{ .assign_ref = .{
            .target = assign.target,
            .result = assign.result,
            .ownership = rewritten_ownership,
            .op = assign.op,
            .next = assign.next,
        } };
    }
}

fn refOpResultSemantics(allocator: Allocator, store: *LirStore, op: LIR.RefOp) Allocator.Error!ResultSemantics {
    return switch (op) {
        .local => |local| .{ .alias_of = .{ .owner = local } },
        .field => |info| try projectedResultSemantics(allocator, store, info.source, .{ .field = info.field_idx }),
        .tag_payload => |info| try projectedResultSemantics(allocator, store, info.source, .{ .tag_payload = info.payload_idx }),
        .tag_payload_struct => |info| try projectedResultSemantics(allocator, store, info.source, .{ .tag_payload_struct = info.tag_discriminant }),
        .list_reinterpret => |info| projectedResultSemanticsWithoutProjection(store, info.backing_ref),
        .nominal => |info| try projectedResultSemantics(allocator, store, info.backing_ref, .nominal),
        .discriminant => .fresh,
    };
}

fn refOpOwnership(store: *LirStore, result: ResultSemantics, op: LIR.RefOp) Allocator.Error!LIR.OwnershipSemantics {
    if (result != .fresh) return .{};

    return switch (op) {
        .local => |source| .{
            .consumed_owned_inputs = try store.addLocalSpan(&.{source}),
        },
        .field,
        .tag_payload,
        .tag_payload_struct,
        .list_reinterpret,
        .nominal,
        => .{
            .materialization = .copy_from_borrowed_input,
        },
        .discriminant => .{},
    };
}

fn projectedResultSemantics(
    allocator: Allocator,
    store: *LirStore,
    source: LocalId,
    projection: LIR.RefProjection,
) Allocator.Error!ResultSemantics {
    if (localResultSemantics(store, source)) |semantics| {
        return switch (semantics) {
            .alias_of => |alias| .{ .alias_of = .{
                .owner = alias.owner,
                .projections = try appendProjection(allocator, store, alias.projections, projection),
            } },
            .borrow_of => |borrow| .{ .borrow_of = .{
                .owner = borrow.owner,
                .projections = try appendProjection(allocator, store, borrow.projections, projection),
                .region = borrow.region,
            } },
            .fresh => .{ .alias_of = .{
                .owner = source,
                .projections = try appendProjection(allocator, store, .empty(), projection),
            } },
        };
    }

    return .{ .alias_of = .{
        .owner = source,
        .projections = try appendProjection(allocator, store, .empty(), projection),
    } };
}

fn projectedResultSemanticsWithoutProjection(store: *const LirStore, source: LocalId) ResultSemantics {
    const semantics: ResultSemantics = localResultSemantics(store, source) orelse .{ .alias_of = .{ .owner = source } };
    return switch (semantics) {
        .alias_of => |alias| .{ .alias_of = alias },
        .borrow_of => |borrow| .{ .borrow_of = borrow },
        .fresh => .{ .alias_of = .{ .owner = source } },
    };
}

fn contractResultSemantics(
    allocator: Allocator,
    store: *LirStore,
    args_span: LIR.LocalSpan,
    contract: LIR.ParamRefContract,
    borrowed: bool,
) Allocator.Error!ResultSemantics {
    const args = store.getLocalSpan(args_span);
    if (contract.param_index >= args.len) return .fresh;

    const source = args[contract.param_index];
    if (localResultSemantics(store, source)) |semantics| {
        return switch (semantics) {
            .alias_of => |alias| if (borrowed)
                .{ .borrow_of = .{
                    .owner = alias.owner,
                    .projections = try appendProjectionSpan(allocator, store, alias.projections, contract.projections),
                    .region = .proc,
                } }
            else
                .{ .alias_of = .{
                    .owner = alias.owner,
                    .projections = try appendProjectionSpan(allocator, store, alias.projections, contract.projections),
                } },
            .borrow_of => |borrow| .{ .borrow_of = .{
                .owner = borrow.owner,
                .projections = try appendProjectionSpan(allocator, store, borrow.projections, contract.projections),
                .region = borrow.region,
            } },
            .fresh => if (borrowed)
                .{ .borrow_of = .{
                    .owner = source,
                    .projections = contract.projections,
                    .region = .proc,
                } }
            else
                .{ .alias_of = .{
                    .owner = source,
                    .projections = contract.projections,
                } },
        };
    }

    return if (borrowed)
        .{ .borrow_of = .{
            .owner = source,
            .projections = contract.projections,
            .region = .proc,
        } }
    else
        .{ .alias_of = .{
            .owner = source,
            .projections = contract.projections,
        } };
}

fn appendProjection(
    allocator: Allocator,
    store: *LirStore,
    existing_span: LIR.RefProjectionSpan,
    projection: LIR.RefProjection,
) Allocator.Error!LIR.RefProjectionSpan {
    const existing = store.getRefProjectionSpan(existing_span);
    const projections = try allocator.alloc(LIR.RefProjection, existing.len + 1);
    defer allocator.free(projections);
    @memcpy(projections[0..existing.len], existing);
    projections[existing.len] = projection;
    return try store.addRefProjectionSpan(projections);
}

fn appendProjectionSpan(
    allocator: Allocator,
    store: *LirStore,
    left: LIR.RefProjectionSpan,
    right: LIR.RefProjectionSpan,
) Allocator.Error!LIR.RefProjectionSpan {
    if (right.len == 0) return left;
    if (left.len == 0) return right;

    const left_items = store.getRefProjectionSpan(left);
    const right_items = store.getRefProjectionSpan(right);
    const projections = try allocator.alloc(LIR.RefProjection, left_items.len + right_items.len);
    defer allocator.free(projections);
    @memcpy(projections[0..left_items.len], left_items);
    @memcpy(projections[left_items.len..], right_items);
    return try store.addRefProjectionSpan(projections);
}

fn localResultSemantics(store: *const LirStore, local: LocalId) ?ResultSemantics {
    const producer = findProducer(store, local) orelse return null;
    return switch (producer) {
        .assign_ref => |assign| assign.result,
        .assign_literal => |assign| assign.result,
        .assign_call => |assign| assign.result,
        .assign_call_indirect => |assign| assign.result,
        .assign_low_level => |assign| assign.result,
        .assign_list => |assign| assign.result,
        .assign_struct => |assign| assign.result,
        .assign_tag => |assign| assign.result,
        .for_list => |for_stmt| for_stmt.elem_result,
        .assign_symbol => null,
        .set_local => null,
        .debug,
        .expect,
        .runtime_error,
        .incref,
        .decref,
        .free,
        .switch_stmt,
        .borrow_scope,
        .scope_exit,
        .loop_continue,
        .join,
        .jump,
        .ret,
        .crash,
        => null,
    };
}

fn resultSemanticsEqual(store: *const LirStore, a: ResultSemantics, b: ResultSemantics) bool {
    return switch (a) {
        .fresh => b == .fresh,
        .alias_of => |left| switch (b) {
            .alias_of => |right| left.owner == right.owner and
                refProjectionSpansEqual(store, left.projections, right.projections),
            else => false,
        },
        .borrow_of => |left| switch (b) {
            .borrow_of => |right| left.owner == right.owner and
                borrowRegionsEqual(left.region, right.region) and
                refProjectionSpansEqual(store, left.projections, right.projections),
            else => false,
        },
    };
}

fn refProjectionSpansEqual(store: *const LirStore, a: LIR.RefProjectionSpan, b: LIR.RefProjectionSpan) bool {
    const left = store.getRefProjectionSpan(a);
    const right = store.getRefProjectionSpan(b);
    if (left.len != right.len) return false;
    for (left, right) |l, r| {
        if (!refProjectionEqual(l, r)) return false;
    }
    return true;
}

fn refProjectionEqual(a: LIR.RefProjection, b: LIR.RefProjection) bool {
    return switch (a) {
        .field => |left| switch (b) {
            .field => |right| left == right,
            else => false,
        },
        .tag_payload => |left| switch (b) {
            .tag_payload => |right| left == right,
            else => false,
        },
        .tag_payload_struct => |left| switch (b) {
            .tag_payload_struct => |right| left == right,
            else => false,
        },
        .nominal => b == .nominal,
    };
}

fn borrowRegionsEqual(a: LIR.BorrowRegion, b: LIR.BorrowRegion) bool {
    return switch (a) {
        .proc => b == .proc,
        .scope => |left| switch (b) {
            .scope => |right| left == right,
            else => false,
        },
    };
}

fn ownershipSemanticsEqual(store: *const LirStore, a: LIR.OwnershipSemantics, b: LIR.OwnershipSemantics) bool {
    return a.materialization == b.materialization and
        std.mem.eql(LocalId, store.getLocalSpan(a.consumed_owned_inputs), store.getLocalSpan(b.consumed_owned_inputs)) and
        std.mem.eql(LocalId, store.getLocalSpan(a.retained_borrows), store.getLocalSpan(b.retained_borrows));
}

fn layoutContainsRefcounted(store: *const LirStore, layouts: *const layout_mod.Store, local: LocalId) bool {
    const local_layout = store.getLocal(local).layout_idx;
    return layouts.layoutContainsRefcounted(layouts.getLayout(local_layout));
}
