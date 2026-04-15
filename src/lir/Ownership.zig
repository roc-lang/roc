//! Ownership fact inference for statement-only LIR.
//!
//! This pass does not invent RC behavior in backends. It computes explicit
//! ownership/provenance facts from the existing statement graph so later
//! lowering passes can emit concrete `incref`/`decref`/`free` statements from a
//! single source of truth.

const std = @import("std");
const base = @import("base");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const CFStmt = LIR.CFStmt;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const ParamRefContract = LIR.ParamRefContract;
const ProcResultContract = LIR.ProcResultContract;
const ResultSemantics = LIR.ResultSemantics;

pub fn inferProcResultContracts(
    allocator: Allocator,
    store: *LirStore,
) Allocator.Error!void {
    var changed = true;
    while (changed) {
        changed = false;
        const proc_count = store.getProcSpecs().len;
        var proc_index: usize = 0;
        while (proc_index < proc_count) : (proc_index += 1) {
            const proc_id: LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(proc_index)));
            const proc = store.getProcSpec(proc_id);
            const inferred = try inferOneProcResultContract(allocator, store, proc);
            if (!procResultContractsEqual(proc.result_contract, inferred)) {
                store.getProcSpecPtr(proc_id).result_contract = inferred;
                changed = true;
            }
        }
    }

    try propagateCallResultSemantics(allocator, store);
    try propagateRefResultSemantics(allocator, store);
}

fn inferOneProcResultContract(
    allocator: Allocator,
    store: *LirStore,
    proc: LIR.LirProcSpec,
) Allocator.Error!ProcResultContract {
    if (proc.body == null) return .fresh;

    const args = store.getLocalSpan(proc.args);
    var arg_index_by_local = std.AutoHashMap(u32, usize).init(allocator);
    defer arg_index_by_local.deinit();
    try arg_index_by_local.ensureTotalCapacity(@intCast(args.len));
    for (args, 0..) |arg_local, i| {
        arg_index_by_local.putAssumeCapacity(@intFromEnum(arg_local), i);
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
        &visited,
        &summaries,
    );

    if (summaries.items.len == 0) return .no_return;

    const first = summaries.items[0];
    for (summaries.items[1..]) |summary| {
        if (!procResultContractsEqual(first, summary)) return .fresh;
    }
    return first;
}

fn collectReturnContracts(
    allocator: Allocator,
    store: *const LirStore,
    stmt_id: CFStmtId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
    visited: *std.AutoHashMap(u32, void),
    out: *std.ArrayList(ProcResultContract),
) Allocator.Error!void {
    const gop = try visited.getOrPut(@intFromEnum(stmt_id));
    if (gop.found_existing) return;

    switch (store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, visited, out),
        .assign_ref => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, visited, out),
        .assign_literal => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, visited, out),
        .assign_call => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, visited, out),
        .assign_call_indirect => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, visited, out),
        .assign_low_level => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, visited, out),
        .assign_list => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, visited, out),
        .assign_struct => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, visited, out),
        .assign_tag => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, visited, out),
        .set_local => |assign| try collectReturnContracts(allocator, store, assign.next, arg_index_by_local, visited, out),
        .debug => |debug_stmt| try collectReturnContracts(allocator, store, debug_stmt.next, arg_index_by_local, visited, out),
        .expect => |expect_stmt| try collectReturnContracts(allocator, store, expect_stmt.next, arg_index_by_local, visited, out),
        .incref => |inc| try collectReturnContracts(allocator, store, inc.next, arg_index_by_local, visited, out),
        .decref => |dec| try collectReturnContracts(allocator, store, dec.next, arg_index_by_local, visited, out),
        .free => |free_stmt| try collectReturnContracts(allocator, store, free_stmt.next, arg_index_by_local, visited, out),
        .switch_stmt => |sw| {
            for (store.getCFSwitchBranches(sw.branches)) |branch| {
                try collectReturnContracts(allocator, store, branch.body, arg_index_by_local, visited, out);
            }
            try collectReturnContracts(allocator, store, sw.default_branch, arg_index_by_local, visited, out);
        },
        .borrow_scope => |scope| {
            try collectReturnContracts(allocator, store, scope.body, arg_index_by_local, visited, out);
            try collectReturnContracts(allocator, store, scope.remainder, arg_index_by_local, visited, out);
        },
        .for_list => |for_stmt| {
            try collectReturnContracts(allocator, store, for_stmt.body, arg_index_by_local, visited, out);
            try collectReturnContracts(allocator, store, for_stmt.next, arg_index_by_local, visited, out);
        },
        .join => |join| {
            try collectReturnContracts(allocator, store, join.body, arg_index_by_local, visited, out);
            try collectReturnContracts(allocator, store, join.remainder, arg_index_by_local, visited, out);
        },
        .jump, .runtime_error, .scope_exit, .crash, .loop_continue => {},
        .ret => |ret_stmt| {
            try out.append(allocator, inferReturnedLocalContract(store, ret_stmt.value, arg_index_by_local));
        },
    }
}

fn inferReturnedLocalContract(
    store: *const LirStore,
    local: LocalId,
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
) ProcResultContract {
    if (arg_index_by_local.get(@intFromEnum(local))) |param_index| {
        return .{ .alias_of_param = .{ .param_index = @intCast(param_index) } };
    }

    var current = local;
    var hops: usize = 0;
    while (hops < 1024) : (hops += 1) {
        const producer = findProducer(store, current) orelse return .fresh;
        switch (producer) {
            .assign_ref => |assign| switch (assign.result) {
                .alias_of => |alias| {
                    if (alias.projections.len != 0) return .fresh;
                    if (arg_index_by_local.get(@intFromEnum(alias.owner))) |param_index| {
                        return .{ .alias_of_param = .{ .param_index = @intCast(param_index) } };
                    }
                    current = alias.owner;
                    continue;
                },
                .borrow_of => |borrow| {
                    if (borrow.projections.len != 0) return .fresh;
                    if (arg_index_by_local.get(@intFromEnum(borrow.owner))) |param_index| {
                        return .{ .borrow_of_param = .{ .param_index = @intCast(param_index) } };
                    }
                    current = borrow.owner;
                    continue;
                },
                .fresh => return .fresh,
            },
            .assign_call => |assign| return store.getProcSpec(assign.proc).result_contract,
            .assign_low_level => |assign| return lowLevelResultContract(store, assign, arg_index_by_local),
            .assign_call_indirect,
            .assign_symbol,
            .assign_literal,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .set_local,
            .debug,
            .expect,
            .incref,
            .decref,
            .free,
            .switch_stmt,
            .borrow_scope,
            .for_list,
            .join,
            .jump,
            .ret,
            .runtime_error,
            .scope_exit,
            .crash,
            .loop_continue,
            => return .fresh,
        }
    }

    return .fresh;
}

fn lowLevelResultContract(
    store: *const LirStore,
    assign: @FieldType(CFStmt, "assign_low_level"),
    arg_index_by_local: *const std.AutoHashMap(u32, usize),
) ProcResultContract {
    return switch (assign.op.procResultSemantics()) {
        .fresh => .fresh,
        .no_return => .no_return,
        .requires_explicit_summary => .fresh,
        .alias_arg => |arg_index| {
            const args = store.getLocalSpan(assign.args);
            if (arg_index >= args.len) return .fresh;
            if (arg_index_by_local.get(@intFromEnum(args[arg_index]))) |param_index| {
                return .{ .alias_of_param = .{ .param_index = @intCast(param_index) } };
            }
            return .fresh;
        },
        .borrow_arg => |arg_index| {
            const args = store.getLocalSpan(assign.args);
            if (arg_index >= args.len) return .fresh;
            if (arg_index_by_local.get(@intFromEnum(args[arg_index]))) |param_index| {
                return .{ .borrow_of_param = .{ .param_index = @intCast(param_index) } };
            }
            return .fresh;
        },
    };
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
            else => {},
        }
    }
    return null;
}

fn procResultContractsEqual(a: ProcResultContract, b: ProcResultContract) bool {
    return switch (a) {
        .fresh => b == .fresh,
        .no_return => b == .no_return,
        .alias_of_param => |left| switch (b) {
            .alias_of_param => |right| left.param_index == right.param_index and left.projections.start == right.projections.start and left.projections.len == right.projections.len,
            else => false,
        },
        .borrow_of_param => |left| switch (b) {
            .borrow_of_param => |right| left.param_index == right.param_index and left.projections.start == right.projections.start and left.projections.len == right.projections.len,
            else => false,
        },
    };
}

fn propagateCallResultSemantics(
    allocator: Allocator,
    store: *LirStore,
) Allocator.Error!void {
    _ = allocator;
    for (store.cf_stmts.items, 0..) |stmt, i| {
        switch (stmt) {
            .assign_call => |assign| {
                const summary = store.getProcSpec(assign.proc).result_contract;
                const next_semantics: ResultSemantics = switch (summary) {
                    .fresh, .no_return => .fresh,
                    .alias_of_param => |contract| blk: {
                        const args = store.getLocalSpan(assign.args);
                        if (contract.param_index >= args.len) break :blk .fresh;
                        break :blk .{ .alias_of = .{
                            .owner = args[contract.param_index],
                            .projections = contract.projections,
                        } };
                    },
                    .borrow_of_param => |contract| blk: {
                        const args = store.getLocalSpan(assign.args);
                        if (contract.param_index >= args.len) break :blk .fresh;
                        break :blk .{ .borrow_of = .{
                            .owner = args[contract.param_index],
                            .projections = contract.projections,
                            .region = .proc,
                        } };
                    },
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
    _ = allocator;
    for (store.cf_stmts.items, 0..) |stmt, i| {
        switch (stmt) {
            .assign_ref => |assign| {
                if (assign.op == .local) continue;
                const next_semantics = refOpResultSemantics(store, assign.op);
                if (resultSemanticsEqual(next_semantics, assign.result)) continue;
                store.cf_stmts.items[i] = .{ .assign_ref = .{
                    .target = assign.target,
                    .result = next_semantics,
                    .op = assign.op,
                    .next = assign.next,
                } };
            },
            else => {},
        }
    }
}

fn refOpResultSemantics(store: *const LirStore, op: LIR.RefOp) ResultSemantics {
    return switch (op) {
        .local => |local| .{ .alias_of = .{ .owner = local } },
        .field => |info| projectedResultSemantics(store, info.source),
        .tag_payload => |info| projectedResultSemantics(store, info.source),
        .tag_payload_struct => |info| projectedResultSemantics(store, info.source),
        .list_reinterpret => |info| projectedResultSemantics(store, info.backing_ref),
        .nominal => |info| projectedResultSemantics(store, info.backing_ref),
        .discriminant => .fresh,
    };
}

fn projectedResultSemantics(store: *const LirStore, source: LocalId) ResultSemantics {
    if (localResultSemantics(store, source)) |semantics| {
        return switch (semantics) {
            .alias_of => .{ .alias_of = .{ .owner = source } },
            .borrow_of => |borrow| .{ .borrow_of = .{
                .owner = source,
                .region = borrow.region,
            } },
            .fresh => .{ .alias_of = .{ .owner = source } },
        };
    }

    return .{ .alias_of = .{ .owner = source } };
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
        .for_list,
        .loop_continue,
        .join,
        .jump,
        .ret,
        .crash,
        => null,
    };
}

fn resultSemanticsEqual(a: ResultSemantics, b: ResultSemantics) bool {
    return switch (a) {
        .fresh => b == .fresh,
        .alias_of => |left| switch (b) {
            .alias_of => |right| left.owner == right.owner and
                left.projections.start == right.projections.start and
                left.projections.len == right.projections.len,
            else => false,
        },
        .borrow_of => |left| switch (b) {
            .borrow_of => |right| left.owner == right.owner and
                borrowRegionsEqual(left.region, right.region) and
                left.projections.start == right.projections.start and
                left.projections.len == right.projections.len,
            else => false,
        },
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
