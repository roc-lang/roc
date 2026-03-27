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
const LocalRef = LIR.LocalRef;
const RefProjection = LIR.RefProjection;
const RefProjectionSpan = LIR.RefProjectionSpan;

const ParamIndexMap = std.AutoHashMap(u64, u8);
const LocalResultMap = std.AutoHashMap(u64, LocalResultInfo);
const VisitedMap = std.AutoHashMap(u64, void);

const LocalResultInfo = struct {
    semantics: LIR.ResultSemantics,
};

const ResolvedReturnKind = union(enum) {
    fresh,
    alias_of_param: u8,
    borrow_of_param: u8,
};

pub const Result = struct {
    alias_sources: std.AutoHashMap(u64, u64),

    pub fn deinit(self: *Result) void {
        self.alias_sources.deinit();
    }
};

pub fn analyze(allocator: Allocator, store: *const LirStore, root: CFStmtId) Allocator.Error!Result {
    var result = Result{
        .alias_sources = std.AutoHashMap(u64, u64).init(allocator),
    };
    try analyzeStmt(&result, store, root);
    return result;
}

pub fn analyzeProc(allocator: Allocator, store: *const LirStore, proc_id: LirProcSpecId) Allocator.Error!Result {
    return analyze(allocator, store, store.getProcSpec(proc_id).body);
}

pub fn resultContractForProc(
    allocator: Allocator,
    store: *LirStore,
    params: LIR.LocalRefSpan,
    root_stmt: CFStmtId,
) Allocator.Error!LIR.ProcResultContract {
    var param_index_by_symbol = ParamIndexMap.init(allocator);
    defer param_index_by_symbol.deinit();

    for (store.getLocalRefs(params), 0..) |param, i| {
        try param_index_by_symbol.put(localKey(param), @intCast(i));
    }

    var results = LocalResultMap.init(allocator);
    defer results.deinit();

    var inferred: ?LIR.ProcResultContract = null;
    var saw_return = false;
    try inferReturnContracts(
        allocator,
        store,
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

    // A proc with no reachable `ret` cannot return an aliased or borrowed value,
    // so its result contract is explicitly fresh.
    return .fresh;
}

fn localKey(local: LocalRef) u64 {
    return @bitCast(local.symbol);
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
    param_index_by_symbol: *const ParamIndexMap,
    results: *LocalResultMap,
    stmt_id: CFStmtId,
    inferred: *?LIR.ProcResultContract,
    saw_return: *bool,
) Allocator.Error!void {
    switch (store.getCFStmt(stmt_id)) {
        .assign_ref => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_literal => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_call => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_low_level => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_list => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_struct => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .assign_tag => |assign| {
            try results.put(localKey(assign.target), .{ .semantics = assign.result });
            try inferReturnContracts(allocator, store, param_index_by_symbol, results, assign.next, inferred, saw_return);
        },
        .runtime_error => {},
        .incref => |inc| try inferReturnContracts(allocator, store, param_index_by_symbol, results, inc.next, inferred, saw_return),
        .decref => |dec| try inferReturnContracts(allocator, store, param_index_by_symbol, results, dec.next, inferred, saw_return),
        .free => |free_stmt| try inferReturnContracts(allocator, store, param_index_by_symbol, results, free_stmt.next, inferred, saw_return),
        .switch_stmt => |switch_stmt| {
            var default_results = try cloneLocalResultMap(allocator, results);
            defer default_results.deinit();
            try inferReturnContracts(
                allocator,
                store,
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
                    param_index_by_symbol,
                    &branch_results,
                    branch.body,
                    inferred,
                    saw_return,
                );
            }
        },
        .join => |join| {
            var body_results = try cloneLocalResultMap(allocator, results);
            defer body_results.deinit();
            try inferReturnContracts(allocator, store, param_index_by_symbol, &body_results, join.body, inferred, saw_return);
            try inferReturnContracts(allocator, store, param_index_by_symbol, results, join.remainder, inferred, saw_return);
        },
        .borrow_scope => |scope| {
            var body_results = try cloneLocalResultMap(allocator, results);
            defer body_results.deinit();
            try inferReturnContracts(allocator, store, param_index_by_symbol, &body_results, scope.body, inferred, saw_return);
            try discardScopeBoundResults(allocator, &body_results, scope.id);
            try inferReturnContracts(allocator, store, param_index_by_symbol, &body_results, scope.remainder, inferred, saw_return);
        },
        .scope_exit, .jump => {},
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
    local: LocalRef,
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
            .{local.symbol.raw()},
        );
    }

    const info = results.get(key) orelse std.debug.panic(
        "DebugOwnershipSummary invariant violated: missing result semantics for returned local {d}",
        .{local.symbol.raw()},
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
                    .{ @intFromEnum(scope_id), local.symbol.raw() },
                ),
                .proc => {},
            }
            const base = try resolveReturnKind(allocator, store, param_index_by_symbol, results, borrowed.owner, projections, visited);
            const borrowed_base = switch (base) {
                .alias_of_param => |param_index| ResolvedReturnKind{ .borrow_of_param = param_index },
                .borrow_of_param => |param_index| ResolvedReturnKind{ .borrow_of_param = param_index },
                .fresh => std.debug.panic(
                    "DebugOwnershipSummary invariant violated: borrowed return local {d} is not rooted in a proc parameter",
                    .{local.symbol.raw()},
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

fn procContractsEqual(store: *const LirStore, a: LIR.ProcResultContract, b: LIR.ProcResultContract) bool {
    return switch (a) {
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
    target: LocalRef,
    semantics: LIR.ResultSemantics,
) Allocator.Error!void {
    switch (semantics) {
        .fresh => {},
        .alias_of => |source| try result.alias_sources.put(localKey(target), localKey(source.owner)),
        .borrow_of => |borrowed| try result.alias_sources.put(localKey(target), localKey(borrowed.owner)),
    }
}
