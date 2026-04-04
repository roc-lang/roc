//! Debug-only MIR verifier.
//!
//! Re-scans strongest-form MIR bodies and asserts the local-def graph and CFG
//! invariants that lowering is expected to establish by construction.
//!
//! It must remain debug-only:
//! - it performs extra whole-body MIR scans
//! - it exists only to catch compiler implementation bugs
//! - release compiler builds must not pay for re-verification

const std = @import("std");

const MIR = @import("MIR.zig");

const Allocator = std.mem.Allocator;
const LocalSet = std.AutoHashMap(u32, void);
const JoinMap = std.AutoHashMap(u32, MIR.LocalSpan);
const ActiveJoinSet = std.AutoHashMap(u32, void);
const VisitedMap = std.AutoHashMap(u32, void);

pub fn verifyConst(
    allocator: Allocator,
    store: *const MIR.Store,
    const_id: MIR.ConstDefId,
) Allocator.Error!void {
    const def = store.getConstDef(const_id);

    var joins = JoinMap.init(allocator);
    defer joins.deinit();
    var visited = VisitedMap.init(allocator);
    defer visited.deinit();
    try collectJoins(store, def.body, &joins, &visited);

    var env = LocalSet.init(allocator);
    defer env.deinit();
    var active_joins = ActiveJoinSet.init(allocator);
    defer active_joins.deinit();
    try verifyStmt(allocator, store, def.body, &env, &joins, &active_joins, "const", @intFromEnum(const_id));
}

pub fn verifyLambda(
    allocator: Allocator,
    store: *const MIR.Store,
    lambda_id: MIR.LambdaId,
) Allocator.Error!void {
    const lambda = store.getLambda(lambda_id);

    var joins = JoinMap.init(allocator);
    defer joins.deinit();
    var visited = VisitedMap.init(allocator);
    defer visited.deinit();
    try collectJoins(store, lambda.body, &joins, &visited);

    var env = LocalSet.init(allocator);
    defer env.deinit();
    var active_joins = ActiveJoinSet.init(allocator);
    defer active_joins.deinit();

    for (store.getLocalSpan(lambda.params), 0..) |param, i| {
        switch (store.getLocalDef(param)) {
            .param => |def| {
                if (def.lambda != lambda_id or def.index != @as(u16, @intCast(i))) {
                    std.debug.panic(
                        "DebugVerifyMir invariant violated: lambda {d} param local {d} recorded wrong param def",
                        .{ @intFromEnum(lambda_id), @intFromEnum(param) },
                    );
                }
            },
            else => std.debug.panic(
                "DebugVerifyMir invariant violated: lambda {d} param local {d} does not have a param local def",
                .{ @intFromEnum(lambda_id), @intFromEnum(param) },
            ),
        }
        try ensureCallableLocalInvariant(store, param, "lambda param", lambda_id);
        try env.put(localKey(param), {});
    }

    if (lambda.captures_param) |captures_param| {
        switch (store.getLocalDef(captures_param)) {
            .captures_param => |def| {
                if (def.lambda != lambda_id) {
                    std.debug.panic(
                        "DebugVerifyMir invariant violated: lambda {d} captures_param local {d} recorded wrong lambda owner",
                        .{ @intFromEnum(lambda_id), @intFromEnum(captures_param) },
                    );
                }
            },
            else => std.debug.panic(
                "DebugVerifyMir invariant violated: lambda {d} captures_param local {d} does not have a captures_param def",
                .{ @intFromEnum(lambda_id), @intFromEnum(captures_param) },
            ),
        }
        try ensureCallableLocalInvariant(store, captures_param, "lambda captures_param", lambda_id);
        try env.put(localKey(captures_param), {});
    }

    try verifyStmt(allocator, store, lambda.body, &env, &joins, &active_joins, "lambda", @intFromEnum(lambda_id));
}

fn collectJoins(
    store: *const MIR.Store,
    stmt_id: MIR.CFStmtId,
    joins: *JoinMap,
    visited: *VisitedMap,
) Allocator.Error!void {
    const key = @intFromEnum(stmt_id);
    if (visited.contains(key)) return;
    try visited.put(key, {});

    switch (store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| try collectJoins(store, assign.next, joins, visited),
        .assign_ref => |assign| try collectJoins(store, assign.next, joins, visited),
        .assign_literal => |assign| try collectJoins(store, assign.next, joins, visited),
        .assign_lambda => |assign| try collectJoins(store, assign.next, joins, visited),
        .assign_closure => |assign| try collectJoins(store, assign.next, joins, visited),
        .assign_call => |assign| try collectJoins(store, assign.next, joins, visited),
        .assign_low_level => |assign| try collectJoins(store, assign.next, joins, visited),
        .assign_list => |assign| try collectJoins(store, assign.next, joins, visited),
        .assign_struct => |assign| try collectJoins(store, assign.next, joins, visited),
        .assign_tag => |assign| try collectJoins(store, assign.next, joins, visited),
        .debug => |stmt| try collectJoins(store, stmt.next, joins, visited),
        .expect => |stmt| try collectJoins(store, stmt.next, joins, visited),
        .runtime_error, .scope_exit, .jump, .ret, .crash => {},
        .switch_stmt => |switch_stmt| {
            for (store.getSwitchBranches(switch_stmt.branches)) |branch| {
                try collectJoins(store, branch.body, joins, visited);
            }
            try collectJoins(store, switch_stmt.default_branch, joins, visited);
        },
        .borrow_scope => |scope| {
            try collectJoins(store, scope.body, joins, visited);
            try collectJoins(store, scope.remainder, joins, visited);
        },
        .join => |join| {
            const gop = try joins.getOrPut(@intFromEnum(join.id));
            if (gop.found_existing) {
                if (gop.value_ptr.start != join.params.start or gop.value_ptr.len != join.params.len) {
                    std.debug.panic(
                        "DebugVerifyMir invariant violated: join {d} was recorded with conflicting parameter spans",
                        .{@intFromEnum(join.id)},
                    );
                }
            } else {
                gop.value_ptr.* = join.params;
            }
            try collectJoins(store, join.body, joins, visited);
            try collectJoins(store, join.remainder, joins, visited);
        },
    }
}

fn verifyStmt(
    allocator: Allocator,
    store: *const MIR.Store,
    stmt_id: MIR.CFStmtId,
    env: *LocalSet,
    joins: *const JoinMap,
    active_joins: *ActiveJoinSet,
    owner_kind: []const u8,
    owner_id: u64,
) Allocator.Error!void {
    switch (store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| {
            try ensureStmtDefMatches(store, assign.target, .symbol, stmt_id, owner_kind, owner_id);
            try ensureCallableLocalInvariant(store, assign.target, "stmt target", stmt_id);
            try env.put(localKey(assign.target), {});
            try verifyStmt(allocator, store, assign.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .assign_ref => |assign| {
            try ensureRefSourcesUsable(store, env, assign.op, stmt_id, owner_kind, owner_id);
            try ensureStmtDefMatches(store, assign.target, .ref, stmt_id, owner_kind, owner_id);
            try ensureCallableLocalInvariant(store, assign.target, "stmt target", stmt_id);
            try env.put(localKey(assign.target), {});
            try verifyStmt(allocator, store, assign.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .assign_literal => |assign| {
            try ensureStmtDefMatches(store, assign.target, .literal, stmt_id, owner_kind, owner_id);
            try ensureCallableLocalInvariant(store, assign.target, "stmt target", stmt_id);
            try env.put(localKey(assign.target), {});
            try verifyStmt(allocator, store, assign.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .assign_lambda => |assign| {
            try ensureStmtDefMatches(store, assign.target, .lambda, stmt_id, owner_kind, owner_id);
            try ensureCallableLocalInvariant(store, assign.target, "stmt target", stmt_id);
            try env.put(localKey(assign.target), {});
            try verifyStmt(allocator, store, assign.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .assign_closure => |assign| {
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.captures), stmt_id, owner_kind, owner_id);
            try ensureStmtDefMatches(store, assign.target, .closure, stmt_id, owner_kind, owner_id);
            try ensureCallableLocalInvariant(store, assign.target, "stmt target", stmt_id);
            try env.put(localKey(assign.target), {});
            try verifyStmt(allocator, store, assign.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .assign_call => |assign| {
            try ensureLocalUsable(store, env, assign.callee, stmt_id, owner_kind, owner_id);
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.args), stmt_id, owner_kind, owner_id);
            try ensureStmtDefMatches(store, assign.target, .call, stmt_id, owner_kind, owner_id);
            try ensureCallableLocalInvariant(store, assign.target, "stmt target", stmt_id);
            try env.put(localKey(assign.target), {});
            try verifyStmt(allocator, store, assign.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .assign_low_level => |assign| {
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.args), stmt_id, owner_kind, owner_id);
            try ensureStmtDefMatches(store, assign.target, .low_level, stmt_id, owner_kind, owner_id);
            try ensureCallableLocalInvariant(store, assign.target, "stmt target", stmt_id);
            try env.put(localKey(assign.target), {});
            try verifyStmt(allocator, store, assign.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .assign_list => |assign| {
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.elems), stmt_id, owner_kind, owner_id);
            try ensureStmtDefMatches(store, assign.target, .list, stmt_id, owner_kind, owner_id);
            try ensureCallableLocalInvariant(store, assign.target, "stmt target", stmt_id);
            try env.put(localKey(assign.target), {});
            try verifyStmt(allocator, store, assign.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .assign_struct => |assign| {
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.fields), stmt_id, owner_kind, owner_id);
            try ensureStmtDefMatches(store, assign.target, .struct_, stmt_id, owner_kind, owner_id);
            try ensureCallableLocalInvariant(store, assign.target, "stmt target", stmt_id);
            try env.put(localKey(assign.target), {});
            try verifyStmt(allocator, store, assign.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .assign_tag => |assign| {
            try ensureLocalsUsable(store, env, store.getLocalSpan(assign.args), stmt_id, owner_kind, owner_id);
            try ensureStmtDefMatches(store, assign.target, .tag, stmt_id, owner_kind, owner_id);
            try ensureCallableLocalInvariant(store, assign.target, "stmt target", stmt_id);
            try env.put(localKey(assign.target), {});
            try verifyStmt(allocator, store, assign.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .debug => |stmt| {
            try ensureLocalUsable(store, env, stmt.value, stmt_id, owner_kind, owner_id);
            try verifyStmt(allocator, store, stmt.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .expect => |stmt| {
            try ensureLocalUsable(store, env, stmt.condition, stmt_id, owner_kind, owner_id);
            try verifyStmt(allocator, store, stmt.next, env, joins, active_joins, owner_kind, owner_id);
        },
        .runtime_error, .scope_exit, .crash => {},
        .switch_stmt => |switch_stmt| {
            try ensureLocalUsable(store, env, switch_stmt.scrutinee, stmt_id, owner_kind, owner_id);

            var default_env = try cloneLocalSet(allocator, env);
            defer default_env.deinit();
            try verifyStmt(allocator, store, switch_stmt.default_branch, &default_env, joins, active_joins, owner_kind, owner_id);

            for (store.getSwitchBranches(switch_stmt.branches)) |branch| {
                var branch_env = try cloneLocalSet(allocator, env);
                defer branch_env.deinit();
                try verifyStmt(allocator, store, branch.body, &branch_env, joins, active_joins, owner_kind, owner_id);
            }
        },
        .borrow_scope => |scope| {
            var body_env = try cloneLocalSet(allocator, env);
            defer body_env.deinit();
            try verifyStmt(allocator, store, scope.body, &body_env, joins, active_joins, owner_kind, owner_id);
            try verifyStmt(allocator, store, scope.remainder, env, joins, active_joins, owner_kind, owner_id);
        },
        .join => |join| {
            var body_env = try cloneLocalSet(allocator, env);
            defer body_env.deinit();
            const join_key = @intFromEnum(join.id);
            const gop = try active_joins.getOrPut(join_key);
            if (gop.found_existing) {
                std.debug.panic(
                    "DebugVerifyMir invariant violated: join {d} was active multiple times in {s} {d}",
                    .{ join_key, owner_kind, owner_id },
                );
            }
            defer _ = active_joins.remove(join_key);
            for (store.getLocalSpan(join.params), 0..) |param, i| {
                switch (store.getLocalDef(param)) {
                    .join_param => |def| {
                        if (def.join != join.id or def.index != @as(u16, @intCast(i))) {
                            std.debug.panic(
                                "DebugVerifyMir invariant violated: join {d} param local {d} recorded wrong join-param def in {s} {d}",
                                .{ @intFromEnum(join.id), @intFromEnum(param), owner_kind, owner_id },
                            );
                        }
                    },
                    else => std.debug.panic(
                        "DebugVerifyMir invariant violated: join {d} param local {d} is missing a join_param def in {s} {d}",
                        .{ @intFromEnum(join.id), @intFromEnum(param), owner_kind, owner_id },
                    ),
                }
                try ensureCallableLocalInvariant(store, param, "join param", join.id);
                try body_env.put(localKey(param), {});
            }
            try verifyStmt(allocator, store, join.body, &body_env, joins, active_joins, owner_kind, owner_id);
            try verifyStmt(allocator, store, join.remainder, env, joins, active_joins, owner_kind, owner_id);
        },
        .jump => |jump| {
            const params = joins.get(@intFromEnum(jump.id)) orelse std.debug.panic(
                "DebugVerifyMir invariant violated: jump in {s} {d} targets unknown join {d}",
                .{ owner_kind, owner_id, @intFromEnum(jump.id) },
            );
            if (!active_joins.contains(@intFromEnum(jump.id))) {
                std.debug.panic(
                    "DebugVerifyMir invariant violated: jump in {s} {d} targets out-of-scope join {d}",
                    .{ owner_kind, owner_id, @intFromEnum(jump.id) },
                );
            }
            const args = store.getLocalSpan(jump.args);
            const join_params = store.getLocalSpan(params);
            if (args.len != join_params.len) {
                std.debug.panic(
                    "DebugVerifyMir invariant violated: jump in {s} {d} targets join {d} with arity mismatch args={d} params={d}",
                    .{ owner_kind, owner_id, @intFromEnum(jump.id), args.len, join_params.len },
                );
            }
            try ensureLocalsUsable(store, env, args, stmt_id, owner_kind, owner_id);
        },
        .ret => |ret| try ensureLocalUsable(store, env, ret.value, stmt_id, owner_kind, owner_id),
    }
}

fn cloneLocalSet(allocator: Allocator, set: *const LocalSet) Allocator.Error!LocalSet {
    var clone = LocalSet.init(allocator);
    var it = set.iterator();
    while (it.next()) |entry| {
        try clone.put(entry.key_ptr.*, {});
    }
    return clone;
}

fn ensureStmtDefMatches(
    store: *const MIR.Store,
    local: MIR.LocalId,
    expected_tag: std.meta.Tag(MIR.LocalDefKind),
    stmt_id: MIR.CFStmtId,
    owner_kind: []const u8,
    owner_id: u64,
) Allocator.Error!void {
    const actual = std.meta.activeTag(store.getLocalDef(local));
    if (actual != expected_tag) {
        std.debug.panic(
            "DebugVerifyMir invariant violated: stmt {d} in {s} {d} defines local {d} as {s}, but local_defs records {s}",
            .{
                @intFromEnum(stmt_id),
                owner_kind,
                owner_id,
                @intFromEnum(local),
                @tagName(expected_tag),
                @tagName(actual),
            },
        );
    }
}

fn ensureCallableLocalInvariant(
    store: *const MIR.Store,
    local: MIR.LocalId,
    context: []const u8,
    context_id: anytype,
) Allocator.Error!void {
    const local_data = store.getLocal(local);
    const mono = store.monotype_store.getMonotype(local_data.monotype);
    if (mono == .func) {
        const ref_source_local: ?MIR.LocalId = switch (store.getLocalDef(local)) {
            .ref => |ref_data| switch (ref_data.op) {
                .local => |source_local| source_local,
                .field => |field| field.source,
                .tag_payload => |payload| payload.source,
                .nominal => |nominal| nominal.backing,
                .discriminant => |discriminant| discriminant.source,
            },
            else => null,
        };
        std.debug.panic(
            "DebugVerifyMir invariant violated: executable local {d} in {s} {d} carried non-executable function monotype {d}; mono_value={any} local_def={s} local_def_value={any} callable_resolution={?any} ref_source_local={?d} ref_source_def={?s} ref_source_callable_resolution={?any}",
            .{
                @intFromEnum(local),
                context,
                context_id,
                @intFromEnum(local_data.monotype),
                mono,
                @tagName(store.getLocalDef(local)),
                store.getLocalDef(local),
                store.resolveLocalCallable(local),
                if (ref_source_local) |source_local| @intFromEnum(source_local) else null,
                if (ref_source_local) |source_local| if (store.getLocalDefOpt(source_local)) |source_def| @tagName(source_def) else "none" else null,
                if (ref_source_local) |source_local| store.resolveLocalCallable(source_local) else null,
            },
        );
    }
    const callable = store.resolveLocalCallable(local) orelse return;
    const lambda = store.getLambdaAnyState(callable.lambda);
    if (callable.captures_local != (lambda.captures_param != null)) {
        std.debug.panic(
            "DebugVerifyMir invariant violated: local {d} in {s} {d} had callable resolution inconsistent with lambda {d}; local_mono={d} callable.captures_local={} lambda.captures_param={?d}",
            .{
                @intFromEnum(local),
                context,
                context_id,
                @intFromEnum(callable.lambda),
                @intFromEnum(local_data.monotype),
                callable.captures_local,
                if (lambda.captures_param) |captures_param| @intFromEnum(captures_param) else null,
            },
        );
    }
}

fn ensureRefSourcesUsable(
    store: *const MIR.Store,
    env: *const LocalSet,
    op: MIR.RefOp,
    stmt_id: MIR.CFStmtId,
    owner_kind: []const u8,
    owner_id: u64,
) Allocator.Error!void {
    const source = switch (op) {
        .local => |local| local,
        .discriminant => |disc| disc.source,
        .field => |field| field.source,
        .tag_payload => |payload| payload.source,
        .nominal => |nominal| nominal.backing,
    };
    try ensureLocalUsable(store, env, source, stmt_id, owner_kind, owner_id);
}

fn ensureLocalsUsable(
    store: *const MIR.Store,
    env: *const LocalSet,
    locals: []const MIR.LocalId,
    stmt_id: MIR.CFStmtId,
    owner_kind: []const u8,
    owner_id: u64,
) Allocator.Error!void {
    for (locals) |local| {
        try ensureLocalUsable(store, env, local, stmt_id, owner_kind, owner_id);
    }
}

fn ensureLocalUsable(
    store: *const MIR.Store,
    env: *const LocalSet,
    local: MIR.LocalId,
    stmt_id: MIR.CFStmtId,
    owner_kind: []const u8,
    owner_id: u64,
) Allocator.Error!void {
    if (store.getLocalDefOpt(local) == null) {
        std.debug.panic(
            "DebugVerifyMir invariant violated: stmt {d} in {s} {d} uses local {d} with no recorded local definition",
            .{ @intFromEnum(stmt_id), owner_kind, owner_id, @intFromEnum(local) },
        );
    }
    if (!env.contains(localKey(local))) {
        std.debug.print(
            "DebugVerifyMir missing local context: stmt={d} stmt_tag={s} local={d} local_def={s}\n",
            .{
                @intFromEnum(stmt_id),
                @tagName(store.getCFStmt(stmt_id)),
                @intFromEnum(local),
                @tagName(store.getLocalDef(local)),
            },
        );
        dumpStmtPredecessors(store, stmt_id);
        dumpStmtWindow(store, stmt_id, 8);
        std.debug.panic(
            "DebugVerifyMir invariant violated: stmt {d} in {s} {d} uses local {d} before it is available on that path",
            .{ @intFromEnum(stmt_id), owner_kind, owner_id, @intFromEnum(local) },
        );
    }
}

fn dumpStmtWindow(store: *const MIR.Store, center: MIR.CFStmtId, radius: u32) void {
    const center_idx = @intFromEnum(center);
    const start = center_idx -| radius;
    const end = @min(store.cf_stmts.items.len, center_idx + radius + 1);
    var idx = start;
    while (idx < end) : (idx += 1) {
        const stmt_id: MIR.CFStmtId = @enumFromInt(@as(u32, @intCast(idx)));
        dumpStmt(store, stmt_id);
    }
}

fn dumpStmt(store: *const MIR.Store, stmt_id: MIR.CFStmtId) void {
    switch (store.getCFStmt(stmt_id)) {
        .assign_ref => |assign| std.debug.print(
            "  MIR stmt {d}: assign_ref target={d} op={any} next={d}\n",
            .{ @intFromEnum(stmt_id), @intFromEnum(assign.target), assign.op, @intFromEnum(assign.next) },
        ),
        .assign_low_level => |assign| std.debug.print(
            "  MIR stmt {d}: assign_low_level target={d} op={s} args={any} next={d}\n",
            .{
                @intFromEnum(stmt_id),
                @intFromEnum(assign.target),
                @tagName(assign.op),
                store.getLocalSpan(assign.args),
                @intFromEnum(assign.next),
            },
        ),
        .assign_call => |assign| std.debug.print(
            "  MIR stmt {d}: assign_call target={d} callee={d} args={any} next={d}\n",
            .{
                @intFromEnum(stmt_id),
                @intFromEnum(assign.target),
                @intFromEnum(assign.callee),
                store.getLocalSpan(assign.args),
                @intFromEnum(assign.next),
            },
        ),
        .assign_literal => |assign| std.debug.print(
            "  MIR stmt {d}: assign_literal target={d} literal={any} next={d}\n",
            .{ @intFromEnum(stmt_id), @intFromEnum(assign.target), assign.literal, @intFromEnum(assign.next) },
        ),
        .switch_stmt => |switch_stmt| std.debug.print(
            "  MIR stmt {d}: switch scrutinee={d} branches={d} default={d}\n",
            .{
                @intFromEnum(stmt_id),
                @intFromEnum(switch_stmt.scrutinee),
                store.getSwitchBranches(switch_stmt.branches).len,
                @intFromEnum(switch_stmt.default_branch),
            },
        ),
        .join => |join| std.debug.print(
            "  MIR stmt {d}: join id={d} params={any} body={d} remainder={d}\n",
            .{
                @intFromEnum(stmt_id),
                @intFromEnum(join.id),
                store.getLocalSpan(join.params),
                @intFromEnum(join.body),
                @intFromEnum(join.remainder),
            },
        ),
        .jump => |jump| std.debug.print(
            "  MIR stmt {d}: jump id={d} args={any}\n",
            .{ @intFromEnum(stmt_id), @intFromEnum(jump.id), store.getLocalSpan(jump.args) },
        ),
        else => std.debug.print(
            "  MIR stmt {d}: {any}\n",
            .{ @intFromEnum(stmt_id), store.getCFStmt(stmt_id) },
        ),
    }
}

fn dumpStmtPredecessors(store: *const MIR.Store, target: MIR.CFStmtId) void {
    std.debug.print("  MIR predecessors for stmt {d}:\n", .{@intFromEnum(target)});
    for (store.cf_stmts.items, 0..) |stmt, idx| {
        const stmt_id: MIR.CFStmtId = @enumFromInt(@as(u32, @intCast(idx)));
        switch (stmt) {
            .assign_symbol => |assign| if (assign.next == target) dumpStmt(store, stmt_id),
            .assign_ref => |assign| if (assign.next == target) dumpStmt(store, stmt_id),
            .assign_literal => |assign| if (assign.next == target) dumpStmt(store, stmt_id),
            .assign_lambda => |assign| if (assign.next == target) dumpStmt(store, stmt_id),
            .assign_closure => |assign| if (assign.next == target) dumpStmt(store, stmt_id),
            .assign_call => |assign| if (assign.next == target) dumpStmt(store, stmt_id),
            .assign_low_level => |assign| if (assign.next == target) dumpStmt(store, stmt_id),
            .assign_list => |assign| if (assign.next == target) dumpStmt(store, stmt_id),
            .assign_struct => |assign| if (assign.next == target) dumpStmt(store, stmt_id),
            .assign_tag => |assign| if (assign.next == target) dumpStmt(store, stmt_id),
            .debug => |debug_stmt| if (debug_stmt.next == target) dumpStmt(store, stmt_id),
            .expect => |expect_stmt| if (expect_stmt.next == target) dumpStmt(store, stmt_id),
            .switch_stmt => |switch_stmt| {
                if (switch_stmt.default_branch == target) dumpStmt(store, stmt_id);
                for (store.getSwitchBranches(switch_stmt.branches)) |branch| {
                    if (branch.body == target) {
                        dumpStmt(store, stmt_id);
                        break;
                    }
                }
            },
            .borrow_scope => |scope| {
                if (scope.body == target or scope.remainder == target) dumpStmt(store, stmt_id);
            },
            .join => |join_stmt| {
                if (join_stmt.body == target or join_stmt.remainder == target) dumpStmt(store, stmt_id);
            },
            else => {},
        }
    }
}

fn localKey(local: MIR.LocalId) u32 {
    return @intFromEnum(local);
}
