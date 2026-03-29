//! Debug/support ownership summary helpers for statement-only LIR.
//!
//! This file is not part of the release lowering pipeline. It exists so
//! debug-only verification can re-derive ownership/proc-result facts from
//! already-lowered LIR and assert that the compiler emitted what it intended.

const std = @import("std");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const CFStmtId = LIR.CFStmtId;
const LirProcSpecId = LIR.LirProcSpecId;
const LocalId = LIR.LocalId;
const RefProjection = LIR.RefProjection;
const RefProjectionSpan = LIR.RefProjectionSpan;

const ParamIndexMap = std.AutoHashMap(u64, u8);
const LocalResultMap = std.AutoHashMap(u64, LocalResultInfo);
const JoinParamMap = std.AutoHashMap(u32, LIR.LocalSpan);
const JoinInputMap = std.AutoHashMap(u64, LocalResultInfo);
const VisitedMap = std.AutoHashMap(u64, void);

const LocalResultInfo = struct {
    semantics: LIR.ResultSemantics,
};

const ResolvedReturnKind = union(enum) {
    fresh,
    alias_of_param: u8,
    borrow_of_param: u8,
};

/// Debug-only ownership summary artifacts derived from lowered LIR.
pub const Result = struct {
    alias_sources: std.AutoHashMap(u64, u64),

    /// Releases all storage owned by this debug summary result.
    pub fn deinit(self: *Result) void {
        self.alias_sources.deinit();
    }
};

/// Re-derives alias-source facts for one lowered statement graph.
pub fn analyze(allocator: Allocator, store: *const LirStore, root: CFStmtId) Allocator.Error!Result {
    var result = Result{
        .alias_sources = std.AutoHashMap(u64, u64).init(allocator),
    };
    try analyzeStmt(&result, store, root);
    return result;
}

/// Re-derives alias-source facts for one lowered proc body.
pub fn analyzeProc(allocator: Allocator, store: *const LirStore, proc_id: LirProcSpecId) Allocator.Error!Result {
    return analyze(allocator, store, store.getProcSpec(proc_id).body);
}

/// Re-derives a proc result contract from a lowered proc body for debug verification.
pub fn resultContractForProc(
    allocator: Allocator,
    store: *LirStore,
    params: LIR.LocalSpan,
    root_stmt: CFStmtId,
) Allocator.Error!LIR.ProcResultContract {
    var param_index_by_symbol = ParamIndexMap.init(allocator);
    defer param_index_by_symbol.deinit();

    for (store.getLocalSpan(params), 0..) |param, i| {
        try param_index_by_symbol.put(localKey(param), @intCast(i));
    }

    var results = LocalResultMap.init(allocator);
    defer results.deinit();
    var join_params = JoinParamMap.init(allocator);
    defer join_params.deinit();
    try collectJoinParams(store, root_stmt, &join_params);

    var join_inputs = JoinInputMap.init(allocator);
    defer join_inputs.deinit();

    var inferred: ?LIR.ProcResultContract = null;
    var saw_return = false;
    try inferReturnContracts(
        allocator,
        store,
        &join_params,
        &join_inputs,
        &param_index_by_symbol,
        &results,
        root_stmt,
        &inferred,
        &saw_return,
    );

    if (saw_return) {
        return inferred orelse std.debug.panic(
            "DebugOwnershipSummary invariant violated: proc had a return path but no result contract was inferred",
            .{},
        );
    }

    return .no_return;
}

fn localKey(local: LocalId) u64 {
    return @intFromEnum(local);
}

fn cloneLocalResultMap(
    allocator: Allocator,
    source: *const LocalResultMap,
) Allocator.Error!LocalResultMap {
    var clone = LocalResultMap.init(allocator);
    var it = source.iterator();
    while (it.next()) |entry| {
        try clone.put(entry.key_ptr.*, entry.value_ptr.*);
    }
    return clone;
}

fn inferReturnContracts(
    allocator: Allocator,
    store: *LirStore,
    join_params: *const JoinParamMap,
    join_inputs: *JoinInputMap,
    param_index_by_symbol: *const ParamIndexMap,
    results: *LocalResultMap,
    stmt_id: CFStmtId,
    inferred: *?LIR.ProcResultContract,
    saw_return: *bool,
) Allocator.Error!void {
    switch (store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = .fresh });
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_ref => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_literal => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_call => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_low_level => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_list => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_struct => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_tag => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .debug => |stmt| try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, stmt.next, inferred, saw_return),
        .expect => |stmt| try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, stmt.next, inferred, saw_return),
        .runtime_error => {},
        .incref => |inc| try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, inc.next, inferred, saw_return),
        .decref => |dec| try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, dec.next, inferred, saw_return),
        .free => |free_stmt| try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, free_stmt.next, inferred, saw_return),
        .switch_stmt => |switch_stmt| {
            var default_results = try cloneLocalResultMap(allocator, results);
            defer default_results.deinit();
            try inferReturnContracts(
                allocator,
                store,
                join_params,
                join_inputs,
                param_index_by_symbol,
                &default_results,
                switch_stmt.default_branch,
                inferred,
                saw_return,
            );

            for (store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                var branch_results = try cloneLocalResultMap(allocator, results);
                defer branch_results.deinit();
                try inferReturnContracts(
                    allocator,
                    store,
                    join_params,
                    join_inputs,
                    param_index_by_symbol,
                    &branch_results,
                    branch.body,
                    inferred,
                    saw_return,
                );
            }
        },
        .join => |join| {
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, results, join.remainder, inferred, saw_return);
            var body_results = try cloneLocalResultMap(allocator, results);
            defer body_results.deinit();
            for (store.getLocalSpan(join.params)) |param| {
                const info = join_inputs.get(localKey(param)) orelse std.debug.panic(
                    "DebugOwnershipSummary invariant violated: join param {d} had no inferred incoming jump semantics",
                    .{@intFromEnum(param)},
                );
                try body_results.put(localKey(param), info);
            }
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, &body_results, join.body, inferred, saw_return);
        },
        .borrow_scope => |scope| {
            var body_results = try cloneLocalResultMap(allocator, results);
            defer body_results.deinit();
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, &body_results, scope.body, inferred, saw_return);
            try discardScopeBoundResults(allocator, &body_results, scope.id);
            try inferReturnContracts(allocator, store, join_params, join_inputs, param_index_by_symbol, &body_results, scope.remainder, inferred, saw_return);
        },
        .scope_exit => {},
        .jump => |jump| {
            const params = store.getLocalSpan(join_params.get(@intFromEnum(jump.target)) orelse std.debug.panic(
                "DebugOwnershipSummary invariant violated: jump target {d} had no recorded join parameters",
                .{@intFromEnum(jump.target)},
            ));
            const args = store.getLocalSpan(jump.args);
            if (args.len != params.len) {
                std.debug.panic(
                    "DebugOwnershipSummary invariant violated: jump target {d} received {d} args for {d} params",
                    .{ @intFromEnum(jump.target), args.len, params.len },
                );
            }

            for (args, params) |arg, param| {
                const arg_info = try resolveJumpArgInfo(param_index_by_symbol, results, arg);
                try mergeJoinInputInfo(store, join_inputs, param, arg_info);
            }
        },
        .ret => |ret| {
            saw_return.* = true;
            var projections = std.ArrayList(RefProjection).empty;
            defer projections.deinit(allocator);

            var visited = VisitedMap.init(allocator);
            defer visited.deinit();

            const kind = try resolveReturnKind(
                allocator,
                store,
                param_index_by_symbol,
                results,
                ret.value,
                &projections,
                &visited,
            );
            const contract = try contractFromResolvedKind(store, kind, projections.items);
            try mergeProcResultContract(store, inferred, contract);
        },
        .crash => {},
    }
}

fn collectJoinParams(
    store: *const LirStore,
    stmt_id: CFStmtId,
    join_params: *JoinParamMap,
) Allocator.Error!void {
    switch (store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| try collectJoinParams(store, assign.next, join_params),
        .assign_ref => |assign| try collectJoinParams(store, assign.next, join_params),
        .assign_literal => |assign| try collectJoinParams(store, assign.next, join_params),
        .assign_call => |assign| try collectJoinParams(store, assign.next, join_params),
        .assign_low_level => |assign| try collectJoinParams(store, assign.next, join_params),
        .assign_list => |assign| try collectJoinParams(store, assign.next, join_params),
        .assign_struct => |assign| try collectJoinParams(store, assign.next, join_params),
        .assign_tag => |assign| try collectJoinParams(store, assign.next, join_params),
        .debug => |stmt| try collectJoinParams(store, stmt.next, join_params),
        .expect => |stmt| try collectJoinParams(store, stmt.next, join_params),
        .runtime_error => {},
        .incref => |inc| try collectJoinParams(store, inc.next, join_params),
        .decref => |dec| try collectJoinParams(store, dec.next, join_params),
        .free => |free_stmt| try collectJoinParams(store, free_stmt.next, join_params),
        .switch_stmt => |switch_stmt| {
            for (store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                try collectJoinParams(store, branch.body, join_params);
            }
            try collectJoinParams(store, switch_stmt.default_branch, join_params);
        },
        .join => |join| {
            const gop = try join_params.getOrPut(@intFromEnum(join.id));
            if (gop.found_existing) {
                if (gop.value_ptr.start != join.params.start or gop.value_ptr.len != join.params.len) {
                    std.debug.panic(
                        "DebugOwnershipSummary invariant violated: join {d} was recorded with incompatible parameter signatures",
                        .{@intFromEnum(join.id)},
                    );
                }
            } else {
                gop.value_ptr.* = join.params;
            }
            try collectJoinParams(store, join.body, join_params);
            try collectJoinParams(store, join.remainder, join_params);
        },
        .borrow_scope => |scope| {
            try collectJoinParams(store, scope.body, join_params);
            try collectJoinParams(store, scope.remainder, join_params);
        },
        .scope_exit, .jump, .ret, .crash => {},
    }
}

fn resolveJumpArgInfo(
    param_index_by_symbol: *const ParamIndexMap,
    results: *const LocalResultMap,
    arg: LocalId,
) Allocator.Error!LocalResultInfo {
    if (param_index_by_symbol.contains(localKey(arg))) {
        return .{ .semantics = .{ .alias_of = .{
            .owner = arg,
            .projections = RefProjectionSpan.empty(),
        } } };
    }

    return results.get(localKey(arg)) orelse std.debug.panic(
        "DebugOwnershipSummary invariant violated: jump arg local {d} had no known result semantics",
        .{@intFromEnum(arg)},
    );
}

fn mergeJoinInputInfo(
    store: *const LirStore,
    join_inputs: *JoinInputMap,
    param: LocalId,
    incoming: LocalResultInfo,
) Allocator.Error!void {
    const gop = try join_inputs.getOrPut(localKey(param));
    if (gop.found_existing) {
        gop.value_ptr.semantics = mergeJoinInputSemantics(store, param, gop.value_ptr.semantics, incoming.semantics);
    } else {
        gop.value_ptr.* = incoming;
    }
}

fn resultSemanticsIsScopedBorrow(semantics: LIR.ResultSemantics) bool {
    return switch (semantics) {
        .borrow_of => |borrowed| switch (borrowed.region) {
            .scope => true,
            .proc => false,
        },
        else => false,
    };
}

fn mergeJoinInputSemantics(
    store: *const LirStore,
    param: LocalId,
    current: LIR.ResultSemantics,
    incoming: LIR.ResultSemantics,
) LIR.ResultSemantics {
    if (resultSemanticsEqual(store, current, incoming)) return current;

    if (resultSemanticsIsScopedBorrow(current) or resultSemanticsIsScopedBorrow(incoming)) {
        std.debug.panic(
            "DebugOwnershipSummary invariant violated: join param {d} merged incompatible scoped-borrow semantics",
            .{@intFromEnum(param)},
        );
    }

    return .fresh;
}

fn discardScopeBoundResults(
    allocator: Allocator,
    results: *LocalResultMap,
    scope_id: LIR.BorrowScopeId,
) Allocator.Error!void {
    var doomed = std.ArrayList(u64).empty;
    defer doomed.deinit(allocator);

    var it = results.iterator();
    while (it.next()) |entry| {
        switch (entry.value_ptr.semantics) {
            .borrow_of => |borrowed| switch (borrowed.region) {
                .scope => |borrowed_scope| if (borrowed_scope == scope_id) {
                    try doomed.append(allocator, entry.key_ptr.*);
                },
                .proc => {},
            },
            else => {},
        }
    }

    for (doomed.items) |key| {
        _ = results.remove(key);
    }
}

fn resolveReturnKind(
    allocator: Allocator,
    store: *const LirStore,
    param_index_by_symbol: *const ParamIndexMap,
    results: *const LocalResultMap,
    local: LocalId,
    projections: *std.ArrayList(RefProjection),
    visited: *VisitedMap,
) Allocator.Error!ResolvedReturnKind {
    const key = localKey(local);
    if (param_index_by_symbol.get(key)) |param_index| {
        return .{ .alias_of_param = param_index };
    }

    const gop = try visited.getOrPut(key);
    if (gop.found_existing) {
        std.debug.panic(
            "DebugOwnershipSummary invariant violated: cyclic return provenance for local {d}",
            .{@intFromEnum(local)},
        );
    }

    const info = results.get(key) orelse std.debug.panic(
        "DebugOwnershipSummary invariant violated: missing result semantics for local {d} in return provenance",
        .{@intFromEnum(local)},
    );
    return switch (info.semantics) {
        .fresh => .fresh,
        .alias_of => |aliased| blk: {
            const base = try resolveReturnKind(allocator, store, param_index_by_symbol, results, aliased.owner, projections, visited);
            switch (base) {
                .alias_of_param, .borrow_of_param => try projections.appendSlice(
                    allocator,
                    store.getRefProjectionSpan(aliased.projections),
                ),
                else => {},
            }
            break :blk base;
        },
        .borrow_of => |borrowed| blk: {
            switch (borrowed.region) {
                .scope => |scope_id| std.debug.panic(
                    "DebugOwnershipSummary invariant violated: scoped borrow from scope {d} escaped via return from local {d}",
                    .{ @intFromEnum(scope_id), @intFromEnum(local) },
                ),
                .proc => {},
            }
            const base = try resolveReturnKind(allocator, store, param_index_by_symbol, results, borrowed.owner, projections, visited);
            const borrowed_base = switch (base) {
                .alias_of_param => |param_index| ResolvedReturnKind{ .borrow_of_param = param_index },
                .borrow_of_param => |param_index| ResolvedReturnKind{ .borrow_of_param = param_index },
                .fresh => std.debug.panic(
                    "DebugOwnershipSummary invariant violated: borrowed return local {d} is not rooted in a proc parameter",
                    .{@intFromEnum(local)},
                ),
            };
            switch (borrowed_base) {
                .borrow_of_param => try projections.appendSlice(
                    allocator,
                    store.getRefProjectionSpan(borrowed.projections),
                ),
                else => {},
            }
            break :blk borrowed_base;
        },
    };
}

fn contractFromResolvedKind(
    store: *LirStore,
    kind: ResolvedReturnKind,
    projections: []const RefProjection,
) Allocator.Error!LIR.ProcResultContract {
    const projection_span = try store.addRefProjectionSpan(projections);
    return switch (kind) {
        .fresh => .fresh,
        .alias_of_param => |param_index| .{ .alias_of_param = .{
            .param_index = param_index,
            .projections = projection_span,
        } },
        .borrow_of_param => |param_index| .{ .borrow_of_param = .{
            .param_index = param_index,
            .projections = projection_span,
        } },
    };
}

fn resultSemanticsEqual(store: *const LirStore, a: LIR.ResultSemantics, b: LIR.ResultSemantics) bool {
    return switch (a) {
        .fresh => b == .fresh,
        .alias_of => |left| switch (b) {
            .alias_of => |right| left.owner == right.owner and projectionSpansEqual(store, left.projections, right.projections),
            else => false,
        },
        .borrow_of => |left| switch (b) {
            .borrow_of => |right| left.owner == right.owner and
                projectionSpansEqual(store, left.projections, right.projections) and
                std.meta.eql(left.region, right.region),
            else => false,
        },
    };
}

fn procContractsEqual(store: *const LirStore, a: LIR.ProcResultContract, b: LIR.ProcResultContract) bool {
    return switch (a) {
        .no_return => b == .no_return,
        .fresh => b == .fresh,
        .alias_of_param => |left| switch (b) {
            .alias_of_param => |right| left.param_index == right.param_index and projectionSpansEqual(store, left.projections, right.projections),
            else => false,
        },
        .borrow_of_param => |left| switch (b) {
            .borrow_of_param => |right| left.param_index == right.param_index and projectionSpansEqual(store, left.projections, right.projections),
            else => false,
        },
    };
}

fn projectionSpansEqual(store: *const LirStore, a: RefProjectionSpan, b: RefProjectionSpan) bool {
    const left = store.getRefProjectionSpan(a);
    const right = store.getRefProjectionSpan(b);
    if (left.len != right.len) return false;
    for (left, right) |lhs, rhs| {
        if (!std.meta.eql(lhs, rhs)) return false;
    }
    return true;
}

fn mergeProcResultContract(
    store: *const LirStore,
    inferred: *?LIR.ProcResultContract,
    next: LIR.ProcResultContract,
) Allocator.Error!void {
    if (inferred.*) |current| {
        if (!procContractsEqual(store, current, next)) {
            std.debug.panic(
                "DebugOwnershipSummary invariant violated: proc returns disagree on result provenance",
                .{},
            );
        }
    } else {
        inferred.* = next;
    }
}

fn analyzeStmt(result: *Result, store: *const LirStore, stmt_id: CFStmtId) Allocator.Error!void {
    switch (store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| {
            try analyzeStmt(result, store, assign.next);
        },
        .assign_ref => |assign| {
            try recordResultSemantics(result, assign.target, assign.result);
            try analyzeStmt(result, store, assign.next);
        },
        .assign_literal => |assign| {
            try recordResultSemantics(result, assign.target, assign.result);
            try analyzeStmt(result, store, assign.next);
        },
        .assign_call => |assign| {
            try recordResultSemantics(result, assign.target, assign.result);
            try analyzeStmt(result, store, assign.next);
        },
        .assign_low_level => |assign| {
            try recordResultSemantics(result, assign.target, assign.result);
            try analyzeStmt(result, store, assign.next);
        },
        .assign_list => |assign| {
            try recordResultSemantics(result, assign.target, assign.result);
            try analyzeStmt(result, store, assign.next);
        },
        .assign_struct => |assign| {
            try recordResultSemantics(result, assign.target, assign.result);
            try analyzeStmt(result, store, assign.next);
        },
        .assign_tag => |assign| {
            try recordResultSemantics(result, assign.target, assign.result);
            try analyzeStmt(result, store, assign.next);
        },
        .debug => |stmt| try analyzeStmt(result, store, stmt.next),
        .expect => |stmt| try analyzeStmt(result, store, stmt.next),
        .runtime_error => {},
        .incref => |inc| try analyzeStmt(result, store, inc.next),
        .decref => |dec| try analyzeStmt(result, store, dec.next),
        .free => |free_stmt| try analyzeStmt(result, store, free_stmt.next),
        .switch_stmt => |switch_stmt| {
            for (store.getCFSwitchBranches(switch_stmt.branches)) |branch| try analyzeStmt(result, store, branch.body);
            try analyzeStmt(result, store, switch_stmt.default_branch);
        },
        .join => |join| {
            try analyzeStmt(result, store, join.body);
            try analyzeStmt(result, store, join.remainder);
        },
        .borrow_scope => |scope| {
            try analyzeStmt(result, store, scope.body);
            try analyzeStmt(result, store, scope.remainder);
        },
        .scope_exit, .jump, .ret, .crash => {},
    }
}

fn recordResultSemantics(
    result: *Result,
    target: LocalId,
    semantics: LIR.ResultSemantics,
) Allocator.Error!void {
    switch (semantics) {
        .fresh => {},
        .alias_of => |source| try result.alias_sources.put(localKey(target), localKey(source.owner)),
        .borrow_of => |borrowed| try result.alias_sources.put(localKey(target), localKey(borrowed.owner)),
    }
}
