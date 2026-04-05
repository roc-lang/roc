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
const layout = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");
const DebugOwnershipSummary = @import("DebugOwnershipSummary.zig");

const Allocator = std.mem.Allocator;
const BorrowScopeId = LIR.BorrowScopeId;
const CFStmtId = LIR.CFStmtId;
const JoinPointId = LIR.JoinPointId;
const LocalId = LIR.LocalId;
const LocalSpan = LIR.LocalSpan;
const RefProjectionSpan = LIR.RefProjectionSpan;

const LocalResultMap = std.AutoHashMap(u64, LIR.ResultSemantics);
const LocalDefinitionMap = std.AutoHashMap(u64, DefinitionSite);
const JoinMetaMap = std.AutoHashMap(u32, JoinMeta);
const JoinInputMap = std.AutoHashMap(u64, JoinInput);
const VisitedMap = std.AutoHashMap(u64, void);

const DefinitionSite = union(enum) {
    param,
    join_param: JoinPointId,
    stmt: CFStmtId,
};

const JoinMeta = struct {
    scopes: []BorrowScopeId,
    params: LocalSpan,
};

const JoinInput = struct {
    borrow_region: ?LIR.BorrowRegion,
};

/// Re-derives and checks debug-only invariants for one lowered proc body.
pub fn verifyProc(
    allocator: Allocator,
    store: *LirStore,
    layout_store: *const layout.Store,
    owner_kind: []const u8,
    owner_id: u64,
    ret_layout: layout.Idx,
    params: LIR.LocalSpan,
    declared_contract: LIR.ProcResultContract,
    body: CFStmtId,
) Allocator.Error!void {
    if (layout_store.layoutContainsRefcounted(layout_store.getLayout(ret_layout))) {
        const inferred_contract = try DebugOwnershipSummary.resultContractForProc(
            allocator,
            store,
            params,
            body,
        );
        if (!procContractsEqual(store, declared_contract, inferred_contract)) {
            std.debug.panic(
                "DebugVerifyLir invariant violated: {s} {d} proc result contract does not match inferred return provenance; declared={any} inferred={any}",
                .{ owner_kind, owner_id, declared_contract, inferred_contract },
            );
        }
    }

    var join_scopes = JoinScopes.init(allocator);
    defer join_scopes.deinit();
    var join_inputs = JoinInputs.init(allocator);
    defer join_inputs.deinit();
    var local_definitions = LocalDefinitionMap.init(allocator);
    defer local_definitions.deinit();

    var active_scopes = std.ArrayList(BorrowScopeId).empty;
    defer active_scopes.deinit(allocator);
    try collectJoinScopes(store, &join_scopes, body, &active_scopes);

    var env = VerifyEnv.init(allocator);
    defer env.deinit();
    for (store.getLocalSpan(params)) |param| {
        try recordDefinition(&local_definitions, param, .param);
        try env.results.put(localKey(param), .fresh);
    }
    try verifyStmt(store, &join_scopes, &join_inputs, &local_definitions, body, &env, null, null, owner_kind, owner_id);
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
        join_params: LocalSpan,
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

    fn params(self: *const JoinScopes, join_id: JoinPointId) LocalSpan {
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

const JoinInputs = struct {
    allocator: Allocator,
    map: JoinInputMap,

    fn init(allocator: Allocator) JoinInputs {
        return .{
            .allocator = allocator,
            .map = JoinInputMap.init(allocator),
        };
    }

    fn deinit(self: *JoinInputs) void {
        self.map.deinit();
    }

    fn record(
        self: *JoinInputs,
        param: LocalId,
        borrow_region: ?LIR.BorrowRegion,
    ) Allocator.Error!void {
        const key = localKey(param);
        const gop = try self.map.getOrPut(key);
        if (gop.found_existing) {
            gop.value_ptr.borrow_region = mergeBorrowRegions(
                param,
                gop.value_ptr.borrow_region,
                borrow_region,
            );
        } else {
            gop.value_ptr.* = .{ .borrow_region = borrow_region };
        }
    }

    fn get(self: *const JoinInputs, param: LocalId) ?JoinInput {
        return self.map.get(localKey(param));
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
        .assign_symbol => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_ref => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_literal => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_call => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_call_indirect => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_low_level => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_list => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_struct => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .assign_tag => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .set_local => |assign| try collectJoinScopes(store, join_scopes, assign.next, active_scopes),
        .debug => |stmt| try collectJoinScopes(store, join_scopes, stmt.next, active_scopes),
        .expect => |stmt| try collectJoinScopes(store, join_scopes, stmt.next, active_scopes),
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
        .for_list => |for_stmt| {
            try collectJoinScopes(store, join_scopes, for_stmt.body, active_scopes);
            try collectJoinScopes(store, join_scopes, for_stmt.next, active_scopes);
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
        .scope_exit, .jump, .ret, .crash, .loop_continue => {},
    }
}

fn verifyStmt(
    store: *const LirStore,
    join_scopes: *const JoinScopes,
    join_inputs: *JoinInputs,
    local_definitions: *LocalDefinitionMap,
    stmt_id: CFStmtId,
    env: *VerifyEnv,
    current_scope_exit: ?BorrowScopeId,
    scope_exit_envs: ?*std.ArrayList(VerifyEnv),
    owner_kind: []const u8,
    owner_id: u64,
) Allocator.Error!void {
    switch (store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| {
            try recordDefinition(local_definitions, assign.target, .{ .stmt = stmt_id });
            try env.results.put(localKey(assign.target), .fresh);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, assign.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .assign_ref => |assign| {
            try ensureLocalUsable(store, env, refOpSource(assign.op), stmt_id);
            try recordDefinition(local_definitions, assign.target, .{ .stmt = stmt_id });
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, assign.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .assign_literal => |assign| {
            try recordDefinition(local_definitions, assign.target, .{ .stmt = stmt_id });
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, assign.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .assign_call => |assign| {
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.args), stmt_id);
            try recordDefinition(local_definitions, assign.target, .{ .stmt = stmt_id });
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, assign.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .assign_call_indirect => |assign| {
            try ensureLocalUsable(store, env, assign.closure, stmt_id);
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.args), stmt_id);
            try recordDefinition(local_definitions, assign.target, .{ .stmt = stmt_id });
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, assign.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .assign_low_level => |assign| {
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.args), stmt_id);
            try recordDefinition(local_definitions, assign.target, .{ .stmt = stmt_id });
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, assign.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .assign_list => |assign| {
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.elems), stmt_id);
            try recordDefinition(local_definitions, assign.target, .{ .stmt = stmt_id });
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, assign.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .assign_struct => |assign| {
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.fields), stmt_id);
            try recordDefinition(local_definitions, assign.target, .{ .stmt = stmt_id });
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, assign.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .assign_tag => |assign| {
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.args), stmt_id);
            try recordDefinition(local_definitions, assign.target, .{ .stmt = stmt_id });
            try env.results.put(localKey(assign.target), assign.result);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, assign.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .set_local => |assign| {
            if (!env.results.contains(localKey(assign.target))) {
                panicMissingLocalSemantics(store, stmt_id, assign.target);
            }
            try ensureLocalUsable(store, env, assign.value, stmt_id);
            const source_semantics = env.results.get(localKey(assign.value)) orelse std.debug.panic(
                "DebugVerifyLir invariant violated: missing source semantics for set_local value {d} at stmt {d}",
                .{ @intFromEnum(assign.value), @intFromEnum(stmt_id) },
            );
            try env.results.put(localKey(assign.target), source_semantics);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, assign.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .debug => |stmt| {
            try ensureLocalUsable(store, env, stmt.message, stmt_id);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, stmt.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .expect => |stmt| {
            try ensureLocalUsable(store, env, stmt.condition, stmt_id);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, stmt.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .runtime_error => {},
        .incref => |inc| {
            try ensureLocalUsable(store, env, inc.value, stmt_id);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, inc.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .decref => |dec| {
            try ensureLocalUsable(store, env, dec.value, stmt_id);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, dec.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .free => |free_stmt| {
            try ensureLocalUsable(store, env, free_stmt.value, stmt_id);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, free_stmt.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .switch_stmt => |switch_stmt| {
            try ensureLocalUsable(store, env, switch_stmt.cond, stmt_id);
            try ensureSwitchBranchesWellFormed(store, switch_stmt.branches);

            var default_env = try env.clone();
            defer default_env.deinit();
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, switch_stmt.default_branch, &default_env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);

            for (store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                var branch_env = try env.clone();
                defer branch_env.deinit();
                try verifyStmt(store, join_scopes, join_inputs, local_definitions, branch.body, &branch_env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
            }
        },
        .for_list => |for_stmt| {
            try ensureLocalUsable(store, env, for_stmt.iterable, stmt_id);

            var body_env = try env.clone();
            defer body_env.deinit();
            try recordDefinition(local_definitions, for_stmt.elem, .{ .stmt = stmt_id });
            try body_env.results.put(localKey(for_stmt.elem), .fresh);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, for_stmt.body, &body_env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);

            try verifyStmt(store, join_scopes, join_inputs, local_definitions, for_stmt.next, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .borrow_scope => |scope| {
            var body_env = try env.clone();
            defer body_env.deinit();
            try body_env.active_scopes.append(body_env.allocator, scope.id);

            var exit_env_list = std.ArrayList(VerifyEnv).empty;
            defer {
                for (exit_env_list.items) |*exit_env| exit_env.deinit();
                exit_env_list.deinit(env.allocator);
            }

            try verifyStmt(store, join_scopes, join_inputs, local_definitions, scope.body, &body_env, scope.id, &exit_env_list, owner_kind, owner_id);
            if (exit_env_list.items.len == 0) return;

            try mergeScopeExitEnvs(store, env, exit_env_list.items, scope.id);
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, scope.remainder, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .join => |join| {
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, join.remainder, env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
            var body_env = try env.clone();
            defer body_env.deinit();
            for (store.getLocalSpan(join.params)) |param| {
                try recordDefinition(local_definitions, param, .{ .join_param = join.id });
                const input = join_inputs.get(param) orelse std.debug.panic(
                    "DebugVerifyLir invariant violated: join param {d} has no verified incoming jump semantics",
                    .{@intFromEnum(param)},
                );
                const semantics = if (input.borrow_region) |region|
                    LIR.ResultSemantics{ .borrow_of = .{
                        .owner = param,
                        .projections = LIR.RefProjectionSpan.empty(),
                        .region = region,
                    } }
                else
                    LIR.ResultSemantics.fresh;
                try body_env.results.put(localKey(param), semantics);
            }
            try verifyStmt(store, join_scopes, join_inputs, local_definitions, join.body, &body_env, current_scope_exit, scope_exit_envs, owner_kind, owner_id);
        },
        .jump => |jump| {
            try ensureJumpArgsMatchTarget(store, join_scopes, jump.target, jump.args, owner_kind, owner_id, stmt_id);
            const args = store.getLocalSpan(jump.args);
            const params = store.getLocalSpan(join_scopes.params(jump.target));
            for (args, params) |arg, param| {
                try ensureJumpArgUsable(store, join_scopes, env, arg, jump.target, stmt_id);
                try join_inputs.record(param, try resolveBorrowRegion(env.allocator, &env.results, arg, stmt_id));
            }
        },
        .ret => |ret| try ensureLocalUsable(store, env, ret.value, stmt_id),
        .scope_exit => {
            const target_scope = current_scope_exit orelse std.debug.panic(
                "DebugVerifyLir invariant violated: scope_exit reached without active borrow_scope collector",
                .{},
            );
            if (!scopeIsActive(env.active_scopes.items, target_scope)) {
                std.debug.panic(
                    "DebugVerifyLir invariant violated: scope_exit for borrow scope {d} reached outside its active scope stack",
                    .{@intFromEnum(target_scope)},
                );
            }
            const exit_list = scope_exit_envs orelse std.debug.panic(
                "DebugVerifyLir invariant violated: scope_exit reached without exit env sink",
                .{},
            );
            try exit_list.append(env.allocator, try env.clone());
        },
        .crash => {},
        .loop_continue => {},
    }
}

fn recordDefinition(definitions: *LocalDefinitionMap, local: LocalId, site: DefinitionSite) Allocator.Error!void {
    const key = localKey(local);
    const gop = try definitions.getOrPut(key);
    if (!gop.found_existing) {
        gop.value_ptr.* = site;
        return;
    }

    std.debug.panic(
        "DebugVerifyLir invariant violated: local {d} was defined more than once; first={any} second={any}",
        .{ @intFromEnum(local), gop.value_ptr.*, site },
    );
}

fn mergeScopeExitEnvs(
    store: *const LirStore,
    base_env: *VerifyEnv,
    exit_envs: []const VerifyEnv,
    scope_id: BorrowScopeId,
) Allocator.Error!void {
    std.debug.assert(exit_envs.len > 0);

    var it = exit_envs[0].results.iterator();
    while (it.next()) |entry| {
        const key = entry.key_ptr.*;
        const semantics = entry.value_ptr.*;
        var all_have = true;
        for (exit_envs[1..]) |exit_env| {
            const other = exit_env.results.get(key) orelse {
                all_have = false;
                break;
            };
            if (!resultSemanticsEqual(store, semantics, other)) {
                std.debug.panic(
                    "DebugVerifyLir invariant violated: borrow_scope {d} reconverges local key {d} with incompatible result semantics",
                    .{ @intFromEnum(scope_id), key },
                );
            }
        }
        if (!all_have) continue;
        try base_env.results.put(key, semantics);
    }
}

fn ensureLocalsUsable(store: *const LirStore, env: *VerifyEnv, locals: []const LocalId, stmt_id: CFStmtId) Allocator.Error!void {
    for (locals) |local| {
        try ensureLocalUsable(store, env, local, stmt_id);
    }
}

fn ensureLocalUsable(
    store: *const LirStore,
    env: *VerifyEnv,
    local: LocalId,
    stmt_id: CFStmtId,
) Allocator.Error!void {
    if (!env.results.contains(localKey(local))) {
        panicMissingLocalSemantics(store, stmt_id, local);
    }
    if (try resolveBorrowRegion(env.allocator, &env.results, local, stmt_id)) |region| {
        switch (region) {
            .proc => {},
            .scope => |scope_id| if (!scopeIsActive(env.active_scopes.items, scope_id)) {
                std.debug.panic(
                    "DebugVerifyLir invariant violated: borrowed local {d} was used outside borrow scope {d} at stmt {d}",
                    .{ @intFromEnum(local), @intFromEnum(scope_id), @intFromEnum(stmt_id) },
                );
            },
        }
    }
}

fn ensureJumpArgUsable(
    store: *const LirStore,
    join_scopes: *const JoinScopes,
    env: *VerifyEnv,
    local: LocalId,
    target: JoinPointId,
    stmt_id: CFStmtId,
) Allocator.Error!void {
    try ensureLocalUsable(store, env, local, stmt_id);
    if (try resolveBorrowRegion(env.allocator, &env.results, local, stmt_id)) |region| {
        switch (region) {
            .proc => {},
            .scope => |scope_id| if (!join_scopes.containsScope(target, scope_id)) {
                std.debug.panic(
                    "DebugVerifyLir invariant violated: borrowed local {d} from scope {d} jumps to join point {d} outside that scope at stmt {d}",
                    .{ @intFromEnum(local), @intFromEnum(scope_id), @intFromEnum(target), @intFromEnum(stmt_id) },
                );
            },
        }
    }
}

fn ensureJumpArgsMatchTarget(
    store: *const LirStore,
    join_scopes: *const JoinScopes,
    target: JoinPointId,
    args: LocalSpan,
    owner_kind: []const u8,
    owner_id: u64,
    stmt_id: CFStmtId,
) Allocator.Error!void {
    const params = store.getLocalSpan(join_scopes.params(target));
    const jump_args = store.getLocalSpan(args);

    if (params.len != jump_args.len) {
        std.debug.panic(
            "DebugVerifyLir invariant violated: {s} {d} stmt {d} jump to join point {d} passes {d} args but target expects {d}",
            .{ owner_kind, owner_id, @intFromEnum(stmt_id), @intFromEnum(target), jump_args.len, params.len },
        );
    }

    for (jump_args, params, 0..) |arg, param, i| {
        if (store.getLocal(arg).layout_idx != store.getLocal(param).layout_idx) {
            dumpStmtWindow(store, stmt_id);
            std.debug.panic(
                "DebugVerifyLir invariant violated: {s} {d} stmt {d} jump arg {d} local={d} to join point {d} has layout {d}, expected join param local={d} layout {d}",
                .{
                    owner_kind,
                    owner_id,
                    @intFromEnum(stmt_id),
                    i,
                    @intFromEnum(arg),
                    @intFromEnum(target),
                    @intFromEnum(store.getLocal(arg).layout_idx),
                    @intFromEnum(param),
                    @intFromEnum(store.getLocal(param).layout_idx),
                },
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
        .assign_symbol => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_ref => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_literal => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_call => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_call_indirect => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_low_level => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_list => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_struct => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .assign_tag => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .set_local => |assign| try ensureScopeBodyTerminatesWithScopeExit(store, assign.next),
        .debug => |stmt| try ensureScopeBodyTerminatesWithScopeExit(store, stmt.next),
        .expect => |stmt| try ensureScopeBodyTerminatesWithScopeExit(store, stmt.next),
        .incref => |inc| try ensureScopeBodyTerminatesWithScopeExit(store, inc.next),
        .decref => |dec| try ensureScopeBodyTerminatesWithScopeExit(store, dec.next),
        .free => |free_stmt| try ensureScopeBodyTerminatesWithScopeExit(store, free_stmt.next),
        .switch_stmt => |switch_stmt| {
            for (store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                try ensureScopeBodyTerminatesWithScopeExit(store, branch.body);
            }
            try ensureScopeBodyTerminatesWithScopeExit(store, switch_stmt.default_branch);
        },
        .for_list => |for_stmt| {
            try ensureScopeBodyTerminatesWithScopeExit(store, for_stmt.body);
            try ensureScopeBodyTerminatesWithScopeExit(store, for_stmt.next);
        },
        .borrow_scope => |scope| {
            try ensureScopeBodyTerminatesWithScopeExit(store, scope.body);
            try ensureScopeBodyTerminatesWithScopeExit(store, scope.remainder);
        },
        .join => |join| {
            try ensureScopeBodyTerminatesWithScopeExit(store, join.body);
            try ensureScopeBodyTerminatesWithScopeExit(store, join.remainder);
        },
        .scope_exit, .runtime_error, .ret, .crash, .loop_continue => {},
        .jump => std.debug.panic(
            "DebugVerifyLir invariant violated: borrow_scope body must terminate through scope_exit or a terminal statement, not jump",
            .{},
        ),
    }
}

fn resolveBorrowRegion(
    allocator: Allocator,
    results: *const LocalResultMap,
    local: LocalId,
    stmt_id: CFStmtId,
) Allocator.Error!?LIR.BorrowRegion {
    var visited = VisitedMap.init(allocator);
    defer visited.deinit();
    return resolveBorrowRegionInner(results, local, &visited, stmt_id);
}

fn resolveBorrowRegionInner(
    results: *const LocalResultMap,
    local: LocalId,
    visited: *VisitedMap,
    stmt_id: CFStmtId,
) Allocator.Error!?LIR.BorrowRegion {
    const key = localKey(local);
    const gop = try visited.getOrPut(key);
    if (gop.found_existing) {
        // Strongest-form LIR is not SSA. Rebinding through alias-only temps can
        // create finite alias cycles (for example, call-argument temps around a
        // proc that returns one of its inputs). This borrow resolver only needs
        // to know whether any path reaches a scoped borrow. Since `borrow_of`
        // terminates immediately below, reaching an already-visited local means
        // this cycle contains only alias edges and therefore carries no borrow
        // region information.
        return null;
    }

    const semantics = results.get(key) orelse std.debug.panic(
        "DebugVerifyLir invariant violated: missing result semantics for local {d} at stmt {d}",
        .{ @intFromEnum(local), @intFromEnum(stmt_id) },
    );
    return switch (semantics) {
        .fresh => null,
        .alias_of => |aliased| resolveBorrowRegionInner(results, aliased.owner, visited, stmt_id),
        .borrow_of => |borrowed| borrowed.region,
    };
}

fn panicMissingLocalSemantics(
    store: *const LirStore,
    stmt_id: CFStmtId,
    local: LocalId,
) noreturn {
    dumpStmtWindow(store, stmt_id);
    switch (store.getCFStmt(stmt_id)) {
        .assign_low_level => |assign| std.debug.panic(
            "DebugVerifyLir invariant violated: missing result semantics for local {d} at stmt {d} (assign_low_level op={s} args={any})",
            .{
                @intFromEnum(local),
                @intFromEnum(stmt_id),
                @tagName(assign.op),
                store.getLocalSpan(assign.args),
            },
        ),
        .assign_call => |assign| std.debug.panic(
            "DebugVerifyLir invariant violated: missing result semantics for local {d} at stmt {d} (assign_call proc={d} args={any})",
            .{
                @intFromEnum(local),
                @intFromEnum(stmt_id),
                @intFromEnum(assign.proc),
                store.getLocalSpan(assign.args),
            },
        ),
        .assign_list => |assign| std.debug.panic(
            "DebugVerifyLir invariant violated: missing result semantics for local {d} at stmt {d} (assign_list elems={any})",
            .{
                @intFromEnum(local),
                @intFromEnum(stmt_id),
                store.getLocalSpan(assign.elems),
            },
        ),
        .assign_struct => |assign| std.debug.panic(
            "DebugVerifyLir invariant violated: missing result semantics for local {d} at stmt {d} (assign_struct fields={any})",
            .{
                @intFromEnum(local),
                @intFromEnum(stmt_id),
                store.getLocalSpan(assign.fields),
            },
        ),
        .assign_tag => |assign| std.debug.panic(
            "DebugVerifyLir invariant violated: missing result semantics for local {d} at stmt {d} (assign_tag args={any})",
            .{
                @intFromEnum(local),
                @intFromEnum(stmt_id),
                store.getLocalSpan(assign.args),
            },
        ),
        else => std.debug.panic(
            "DebugVerifyLir invariant violated: missing result semantics for local {d} at stmt {d} ({s})",
            .{
                @intFromEnum(local),
                @intFromEnum(stmt_id),
                @tagName(store.getCFStmt(stmt_id)),
            },
        ),
    }
}

fn dumpStmtWindow(store: *const LirStore, center_stmt_id: CFStmtId) void {
    const center = @as(usize, @intFromEnum(center_stmt_id));
    const stmt_count = store.cf_stmts.items.len;
    const start = center -| 5;
    const end = @min(stmt_count, center + 6);

    std.debug.print(
        "DebugVerifyLir stmt window around {d}:\n",
        .{@intFromEnum(center_stmt_id)},
    );
    for (start..end) |i| {
        const id: CFStmtId = @enumFromInt(@as(u32, @intCast(i)));
        std.debug.print(
            "  stmt {d}: {any}\n",
            .{ i, store.getCFStmt(id) },
        );
    }
}

fn refOpSource(op: LIR.RefOp) LocalId {
    return switch (op) {
        .local => |local| local,
        .discriminant => |disc| disc.source,
        .field => |field| field.source,
        .tag_payload => |payload| payload.source,
        .tag_payload_struct => |payload| payload.source,
        .nominal => |nominal| nominal.backing_ref,
    };
}

fn scopeIsActive(active_scopes: []const BorrowScopeId, scope_id: BorrowScopeId) bool {
    for (active_scopes) |active_scope| {
        if (active_scope == scope_id) return true;
    }
    return false;
}

fn localKey(local: LocalId) u64 {
    return @intFromEnum(local);
}

fn mergeBorrowRegions(
    param: LocalId,
    left: ?LIR.BorrowRegion,
    right: ?LIR.BorrowRegion,
) ?LIR.BorrowRegion {
    return switch (left orelse return right) {
        .proc => switch (right orelse return left) {
            .proc => .proc,
            .scope => |scope_id| .{ .scope = scope_id },
        },
        .scope => |left_scope| switch (right orelse return left) {
            .proc => .{ .scope = left_scope },
            .scope => |right_scope| {
                if (left_scope != right_scope) {
                    std.debug.panic(
                        "DebugVerifyLir invariant violated: join param {d} received incoming borrows from incompatible scopes {d} and {d}",
                        .{ @intFromEnum(param), @intFromEnum(left_scope), @intFromEnum(right_scope) },
                    );
                }
                return .{ .scope = left_scope };
            },
        },
    };
}

fn procContractsEqual(
    store: *const LirStore,
    a: LIR.ProcResultContract,
    b: LIR.ProcResultContract,
) bool {
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

fn resultSemanticsEqual(
    store: *const LirStore,
    a: LIR.ResultSemantics,
    b: LIR.ResultSemantics,
) bool {
    return switch (a) {
        .fresh => b == .fresh,
        .alias_of => |left| switch (b) {
            .alias_of => |right| localRefsEqual(left.owner, right.owner) and projectionSpansEqual(store, left.projections, right.projections),
            else => false,
        },
        .borrow_of => |left| switch (b) {
            .borrow_of => |right| localRefsEqual(left.owner, right.owner) and
                projectionSpansEqual(store, left.projections, right.projections) and
                borrowRegionsEqual(left.region, right.region),
            else => false,
        },
    };
}

fn localRefsEqual(a: LocalId, b: LocalId) bool {
    return a == b;
}

fn borrowRegionsEqual(a: LIR.BorrowRegion, b: LIR.BorrowRegion) bool {
    return switch (a) {
        .proc => b == .proc,
        .scope => |left_scope| switch (b) {
            .scope => |right_scope| left_scope == right_scope,
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
