//! Debug-only LIR verifier.
//!
//! This file exists only to catch compiler implementation bugs by re-scanning
//! already-lowered LIR and asserting invariants about borrow scopes and proc
//! result contracts.
//!
//! It must remain debug-only:
//! - it performs extra full-LIR scans
//! - it does not contribute to code generation
//! - release compiler builds must not pay for bug-detection passes

const std = @import("std");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");
const DebugOwnershipSummary = @import("DebugOwnershipSummary.zig");

const Allocator = std.mem.Allocator;
const BorrowScopeId = LIR.BorrowScopeId;
const CFStmtId = LIR.CFStmtId;
const JoinPointId = LIR.JoinPointId;
const LocalRef = LIR.LocalRef;
const LocalRefSpan = LIR.LocalRefSpan;
const RefProjectionSpan = LIR.RefProjectionSpan;

const LocalResultMap = std.AutoHashMap(u64, LIR.ResultSemantics);
const JoinMetaMap = std.AutoHashMap(u32, JoinMeta);
const VisitedMap = std.AutoHashMap(u64, void);

const JoinMeta = struct {
    scopes: []BorrowScopeId,
    params: LocalRefSpan,
};

const synthetic_symbol_base: u64 = 0xf000_0000_0000_0000;

/// Re-derives and checks debug-only invariants for one lowered proc body.
pub fn verifyProc(
    allocator: Allocator,
    store: *LirStore,
    params: LIR.LocalRefSpan,
    declared_contract: LIR.ProcResultContract,
    body: CFStmtId,
) Allocator.Error!void {
    const inferred_contract = try DebugOwnershipSummary.resultContractForProc(
        allocator,
        store,
        params,
        body,
    );
    if (!procContractsEqual(store, declared_contract, inferred_contract)) {
        std.debug.panic(
            "DebugVerifyLir invariant violated: proc result contract does not match inferred return provenance",
            .{},
        );
    }

    var join_scopes = JoinScopes.init(allocator);
    defer join_scopes.deinit();

    var active_scopes = std.ArrayList(BorrowScopeId).empty;
    defer active_scopes.deinit(allocator);
    try collectJoinScopes(store, &join_scopes, body, &active_scopes);

    var env = VerifyEnv.init(allocator);
    defer env.deinit();
    try verifyStmt(store, &join_scopes, body, &env);
}

const JoinScopes = struct {
    allocator: Allocator,
    map: JoinMetaMap,

    fn init(allocator: Allocator) JoinScopes {
        return .{
            .allocator = allocator,
            .map = JoinMetaMap.init(allocator),
        };
    }

    fn deinit(self: *JoinScopes) void {
        var it = self.map.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.value_ptr.scopes);
        }
        self.map.deinit();
    }

    fn record(
        self: *JoinScopes,
        join_id: JoinPointId,
        active_scopes: []const BorrowScopeId,
        join_params: LocalRefSpan,
    ) Allocator.Error!void {
        const owned = try self.allocator.alloc(BorrowScopeId, active_scopes.len);
        @memcpy(owned, active_scopes);

        const gop = try self.map.getOrPut(@intFromEnum(join_id));
        if (gop.found_existing) {
            if (!scopePathsEqual(gop.value_ptr.scopes, owned)) {
                std.debug.panic(
                    "DebugVerifyLir invariant violated: join point {d} was recorded with incompatible borrow scope paths",
                    .{@intFromEnum(join_id)},
                );
            }
            if (gop.value_ptr.params.start != join_params.start or gop.value_ptr.params.len != join_params.len) {
                std.debug.panic(
                    "DebugVerifyLir invariant violated: join point {d} was recorded with incompatible parameter signatures",
                    .{@intFromEnum(join_id)},
                );
            }
            self.allocator.free(owned);
        } else {
            gop.value_ptr.* = .{
                .scopes = owned,
                .params = join_params,
            };
        }
    }

    fn containsScope(self: *const JoinScopes, join_id: JoinPointId, scope_id: BorrowScopeId) bool {
        const meta = self.map.get(@intFromEnum(join_id)) orelse return false;
        for (meta.scopes) |scope| {
            if (scope == scope_id) return true;
        }
        return false;
    }

    fn params(self: *const JoinScopes, join_id: JoinPointId) LocalRefSpan {
        const meta = self.map.get(@intFromEnum(join_id)) orelse std.debug.panic(
            "DebugVerifyLir invariant violated: jump target {d} has no recorded join metadata",
            .{@intFromEnum(join_id)},
        );
        return meta.params;
    }

    fn scopePathsEqual(a: []const BorrowScopeId, b: []const BorrowScopeId) bool {
        if (a.len != b.len) return false;
        for (a, b) |lhs, rhs| {
            if (lhs != rhs) return false;
        }
        return true;
    }
};

const VerifyEnv = struct {
    allocator: Allocator,
    results: LocalResultMap,
    active_scopes: std.ArrayList(BorrowScopeId),

    fn init(allocator: Allocator) VerifyEnv {
        return .{
            .allocator = allocator,
            .results = LocalResultMap.init(allocator),
            .active_scopes = std.ArrayList(BorrowScopeId).empty,
        };
    }

    fn deinit(self: *VerifyEnv) void {
        self.results.deinit();
        self.active_scopes.deinit(self.allocator);
    }

    fn clone(self: *const VerifyEnv) Allocator.Error!VerifyEnv {
        var cloned = VerifyEnv.init(self.allocator);
        var it = self.results.iterator();
        while (it.next()) |entry| {
            try cloned.results.put(entry.key_ptr.*, entry.value_ptr.*);
        }
        try cloned.active_scopes.appendSlice(self.allocator, self.active_scopes.items);
        return cloned;
    }
};

fn collectJoinScopes(
    store: *const LirStore,
    join_scopes: *JoinScopes,
    stmt_id: CFStmtId,
    active_scopes: *std.ArrayList(BorrowScopeId),
) Allocator.Error!void {
    switch (store.getCFStmt(stmt_id)) {
        .assign_ref => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_literal => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_call => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_low_level => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_list => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_struct => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_tag => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .runtime_error => {},
        .incref => |inc| try collectJoinScopes(store, join_scopes, inc.next, active_scopes),
        .decref => |dec| try collectJoinScopes(store, join_scopes, dec.next, active_scopes),
        .free => |free_stmt| try collectJoinScopes(store, join_scopes, free_stmt.next, active_scopes),
        .switch_stmt => |switch_stmt| {
            for (store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                try collectJoinScopes(store, join_scopes, branch.body, active_scopes);
            }
            try collectJoinScopes(store, join_scopes, switch_stmt.default_branch, active_scopes);
        },
        .borrow_scope => |scope| {
            try ensureScopeBodyTerminatesWithScopeExit(store, scope.body);
            try active_scopes.append(join_scopes.allocator, scope.id);
            try collectJoinScopes(store, join_scopes, scope.body, active_scopes);
            _ = active_scopes.pop();
            try collectJoinScopes(store, join_scopes, scope.remainder, active_scopes);
        },
        .join => |join| {
            try join_scopes.record(join.id, active_scopes.items, join.params);
            try collectJoinScopes(store, join_scopes, join.body, active_scopes);
            try collectJoinScopes(store, join_scopes, join.remainder, active_scopes);
        },
        .scope_exit, .jump, .ret, .crash => {},
    }
}

fn verifyStmt(
    store: *const LirStore,
    join_scopes: *const JoinScopes,
    stmt_id: CFStmtId,
    env: *VerifyEnv,
) Allocator.Error!void {
    switch (store.getCFStmt(stmt_id)) {
        .assign_ref => |assign| {
            try ensureLocalUsable(env, refOpSource(assign.op));
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, assign.next, env);
        },
        .assign_literal => |assign| {
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, assign.next, env);
        },
        .assign_call => |assign| {
            try ensureLocalsUsable(env, store.getLocalRefs(assign.args));
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, assign.next, env);
        },
        .assign_low_level => |assign| {
            try ensureLocalsUsable(env, store.getLocalRefs(assign.args));
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, assign.next, env);
        },
        .assign_list => |assign| {
            try ensureLocalsUsable(env, store.getLocalRefs(assign.elems));
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, assign.next, env);
        },
        .assign_struct => |assign| {
            try ensureLocalsUsable(env, store.getLocalRefs(assign.fields));
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, assign.next, env);
        },
        .assign_tag => |assign| {
            try ensureLocalsUsable(env, store.getLocalRefs(assign.args));
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, assign.next, env);
        },
        .runtime_error => {},
        .incref => |inc| {
            try ensureLocalUsable(env, inc.value);
            try verifyStmt(store, join_scopes, inc.next, env);
        },
        .decref => |dec| {
            try ensureLocalUsable(env, dec.value);
            try verifyStmt(store, join_scopes, dec.next, env);
        },
        .free => |free_stmt| {
            try ensureLocalUsable(env, free_stmt.value);
            try verifyStmt(store, join_scopes, free_stmt.next, env);
        },
        .switch_stmt => |switch_stmt| {
            try ensureLocalUsable(env, switch_stmt.cond);
            try ensureSwitchBranchesWellFormed(store, switch_stmt.branches);

            var default_env = try env.clone();
            defer default_env.deinit();
            try verifyStmt(store, join_scopes, switch_stmt.default_branch, &default_env);

            for (store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                var branch_env = try env.clone();
                defer branch_env.deinit();
                try verifyStmt(store, join_scopes, branch.body, &branch_env);
            }
        },
        .borrow_scope => |scope| {
            try env.active_scopes.append(env.allocator, scope.id);
            try verifyStmt(store, join_scopes, scope.body, env);
            _ = env.active_scopes.pop();
            try verifyStmt(store, join_scopes, scope.remainder, env);
        },
        .join => |join| {
            var body_env = try env.clone();
            defer body_env.deinit();
            try verifyStmt(store, join_scopes, join.body, &body_env);
            try verifyStmt(store, join_scopes, join.remainder, env);
        },
        .jump => |jump| {
            try ensureJumpArgsMatchTarget(store, join_scopes, jump.target, jump.args);
            for (store.getLocalRefs(jump.args)) |arg| {
                try ensureJumpArgUsable(join_scopes, env, arg, jump.target);
            }
        },
        .ret => |ret| try ensureLocalUsable(env, ret.value),
        .scope_exit, .crash => {},
    }
}

fn ensureLocalsUsable(env: *VerifyEnv, locals: []const LocalRef) Allocator.Error!void {
    for (locals) |local| {
        try ensureLocalUsable(env, local);
    }
}

fn ensureLocalUsable(
    env: *VerifyEnv,
    local: LocalRef,
) Allocator.Error!void {
    if (try resolveBorrowRegion(env.allocator, &env.results, local)) |region| {
        switch (region) {
            .proc => {},
            .scope => |scope_id| if (!scopeIsActive(env.active_scopes.items, scope_id)) {
                std.debug.panic(
                    "DebugVerifyLir invariant violated: borrowed local {d} was used outside borrow scope {d}",
                    .{ local.symbol.raw(), @intFromEnum(scope_id) },
                );
            },
        }
    }
}

fn ensureJumpArgUsable(
    join_scopes: *const JoinScopes,
    env: *VerifyEnv,
    local: LocalRef,
    target: JoinPointId,
) Allocator.Error!void {
    try ensureLocalUsable(env, local);
    if (try resolveBorrowRegion(env.allocator, &env.results, local)) |region| {
        switch (region) {
            .proc => {},
            .scope => |scope_id| if (!join_scopes.containsScope(target, scope_id)) {
                std.debug.panic(
                    "DebugVerifyLir invariant violated: borrowed local {d} from scope {d} jumps to join point {d} outside that scope",
                    .{ local.symbol.raw(), @intFromEnum(scope_id), @intFromEnum(target) },
                );
            },
        }
    }
}

fn ensureJumpArgsMatchTarget(
    store: *const LirStore,
    join_scopes: *const JoinScopes,
    target: JoinPointId,
    args: LocalRefSpan,
) Allocator.Error!void {
    const params = store.getLocalRefs(join_scopes.params(target));
    const jump_args = store.getLocalRefs(args);

    if (params.len != jump_args.len) {
        std.debug.panic(
            "DebugVerifyLir invariant violated: jump to join point {d} passes {d} args but target expects {d}",
            .{ @intFromEnum(target), jump_args.len, params.len },
        );
    }

    for (jump_args, params, 0..) |arg, param, i| {
        if (arg.layout_idx != param.layout_idx) {
            std.debug.panic(
                "DebugVerifyLir invariant violated: jump arg {d} to join point {d} has layout {d}, expected {d}",
                .{ i, @intFromEnum(target), @intFromEnum(arg.layout_idx), @intFromEnum(param.layout_idx) },
            );
        }
    }
}

fn ensureSwitchBranchesWellFormed(
    store: *const LirStore,
    branches_span: LIR.CFSwitchBranchSpan,
) Allocator.Error!void {
    var seen = std.AutoHashMap(u64, void).init(store.allocator);
    defer seen.deinit();

    for (store.getCFSwitchBranches(branches_span)) |branch| {
        const gop = try seen.getOrPut(branch.value);
        if (gop.found_existing) {
            std.debug.panic(
                "DebugVerifyLir invariant violated: switch statement contains duplicate branch value {d}",
                .{branch.value},
            );
        }
    }
}

fn ensureScopeBodyTerminatesWithScopeExit(
    store: *const LirStore,
    stmt_id: CFStmtId,
) Allocator.Error!void {
    switch (store.getCFStmt(stmt_id)) {
        .assign_ref => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_literal => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_call => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_low_level => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_list => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_struct => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_tag => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .incref => |inc| try ensureScopeBodyTerminatesWithScopeExit(store, inc.next),
        .decref => |dec| try ensureScopeBodyTerminatesWithScopeExit(store, dec.next),
        .free => |free_stmt| try ensureScopeBodyTerminatesWithScopeExit(store, free_stmt.next),
        .switch_stmt => |switch_stmt| {
            for (store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                try ensureScopeBodyTerminatesWithScopeExit(store, branch.body);
            }
            try ensureScopeBodyTerminatesWithScopeExit(store, switch_stmt.default_branch);
        },
        .borrow_scope => |scope| {
            try ensureScopeBodyTerminatesWithScopeExit(store, scope.body);
            try ensureScopeBodyTerminatesWithScopeExit(store, scope.remainder);
        },
        .join => |join| {
            try ensureScopeBodyTerminatesWithScopeExit(store, join.body);
            try ensureScopeBodyTerminatesWithScopeExit(store, join.remainder);
        },
        .scope_exit => {},
        .runtime_error, .jump, .ret, .crash => std.debug.panic(
            "DebugVerifyLir invariant violated: borrow_scope body must terminate through scope_exit, not {s}",
            .{@tagName(store.getCFStmt(stmt_id))},
        ),
    }
}

fn resolveBorrowRegion(
    allocator: Allocator,
    results: *const LocalResultMap,
    local: LocalRef,
) Allocator.Error!?LIR.BorrowRegion {
    var visited = VisitedMap.init(allocator);
    defer visited.deinit();
    return resolveBorrowRegionInner(results, local, &visited);
}

fn resolveBorrowRegionInner(
    results: *const LocalResultMap,
    local: LocalRef,
    visited: *VisitedMap,
) Allocator.Error!?LIR.BorrowRegion {
    const key = localKey(local);
    const gop = try visited.getOrPut(key);
    if (gop.found_existing) {
        std.debug.panic(
            "DebugVerifyLir invariant violated: cyclic borrow provenance for local {d}",
            .{local.symbol.raw()},
        );
    }

    const semantics = results.get(key) orelse {
        if (local.symbol.raw() >= synthetic_symbol_base) {
            std.debug.panic(
                "DebugVerifyLir invariant violated: missing result semantics for synthetic local {d}",
                .{local.symbol.raw()},
            );
        }

        // Non-synthetic roots such as proc parameters and global lookups are
        // outside proc-local statement provenance tracking. They can seed alias
        // chains, but they are not themselves borrowed by local statements.
        return null;
    };
    return switch (semantics) {
        .fresh => null,
        .alias_of => |aliased| resolveBorrowRegionInner(results, aliased.owner, visited),
        .borrow_of => |borrowed| borrowed.region,
    };
}

fn refOpSource(op: LIR.RefOp) LocalRef {
    return switch (op) {
        .local => |local| local,
        .discriminant => |disc| disc.source,
        .field => |field| field.source,
        .tag_payload => |payload| payload.source,
        .nominal => |nominal| nominal.backing_ref,
    };
}

fn scopeIsActive(active_scopes: []const BorrowScopeId, scope_id: BorrowScopeId) bool {
    for (active_scopes) |active_scope| {
        if (active_scope == scope_id) return true;
    }
    return false;
}

fn localKey(local: LocalRef) u64 {
    return @bitCast(local.symbol);
}

fn procContractsEqual(
    store: *const LirStore,
    a: LIR.ProcResultContract,
    b: LIR.ProcResultContract,
) bool {
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

fn projectionSpansEqual(
    store: *const LirStore,
    a: RefProjectionSpan,
    b: RefProjectionSpan,
) bool {
    const left = store.getRefProjectionSpan(a);
    const right = store.getRefProjectionSpan(b);
    if (left.len != right.len) return false;
    for (left, right) |lhs, rhs| {
        if (!std.meta.eql(lhs, rhs)) return false;
    }
    return true;
}
