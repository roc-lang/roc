//! MIR -> strongest-form statement-only LIR lowering.
//!
//! This pass assumes strongest-form statement MIR:
//! - all intermediate values live in explicit MIR locals
//! - control flow is explicit MIR `CFStmt`
//! - lambdas/closures are still explicit MIR concepts
//! - branch tests and destructuring are already explicit `switch_stmt`/`assign_ref`

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const corecir = @import("corecir");
const layout = @import("layout");
const mir_mod = @import("mir");

const MIR = mir_mod.MIR;
const Analyses = mir_mod.Analyses;
const ResultSummary = mir_mod.ResultSummary;

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");
const DebugVerifyLir = @import("DebugVerifyLir.zig");

const Allocator = std.mem.Allocator;
const MirMonotypeIdx = corecir.Monotype.Idx;

const CFStmtId = LIR.CFStmtId;
const JoinPointId = LIR.JoinPointId;
const LirProcSpec = LIR.LirProcSpec;
const LirProcSpecId = LIR.LirProcSpecId;
const LiteralValue = LIR.LiteralValue;
const LirLocalId = LIR.LocalId;
const LirLocalSpan = LIR.LocalSpan;
const ResultSemantics = LIR.ResultSemantics;
const RefOp = LIR.RefOp;
const LoweredLambdaMap = std.AutoHashMap(u32, LirProcSpecId);
const LoweredConstMap = std.AutoHashMap(u32, LirProcSpecId);
const MirToLirLocalMap = std.AutoHashMap(u32, LirLocalId);
const MirToLirLocalScopeStack = std.ArrayListUnmanaged(MirToLirLocalMap);
const dec_literal_scale: i128 = std.math.pow(i128, 10, 18);

const BuilderProcHeader = struct {
    name: LIR.Symbol,
    args: LirLocalSpan,
    ret_layout: layout.Idx,
    result_contract: LIR.ProcResultContract,
    hosted: ?LIR.HostedProc,
};

const BuilderProc = union(enum) {
    unresolved: BuilderProcHeader,
    resolved: LirProcSpec,
};

const MirTargetBinding = struct {
    key: u32,
    target: LirLocalId,
};
const CallableContract = ResultSummary.CallableContract;
const MirOriginOverrideMap = std.AutoHashMap(u32, LoweredOrigin);

const JoinOriginState = struct {
    params: []const MIR.LocalId,
    merged_origins: []?LoweredOrigin,
    merged_layouts: []?layout.Idx,
    saw_incoming: bool,
};

const JoinOverrideInfo = struct {
    origins: MirOriginOverrideMap,
    layouts: []layout.Idx,

    fn deinit(self: *JoinOverrideInfo, allocator: Allocator) void {
        self.origins.deinit();
        allocator.free(self.layouts);
    }
};

const ActiveJoinOriginMap = std.AutoHashMap(u32, JoinOriginState);

const ResolvedCallable = struct {
    lambda: MIR.LambdaId,
    captures_local: ?MIR.LocalId,
};

const LoweredOrigin = union(enum) {
    fresh,
    alias_of_local: struct {
        owner: LirLocalId,
        projections: LIR.RefProjectionSpan = .empty(),
    },
    borrow_of_local: struct {
        owner: LirLocalId,
        projections: LIR.RefProjectionSpan = .empty(),
        region: LIR.BorrowRegion,
    },
    borrow_of_fresh: struct {
        projections: LIR.RefProjectionSpan = .empty(),
        region: LIR.BorrowRegion,
    },
};

const Self = @This();

allocator: Allocator,
mir_store: *const MIR.Store,
lir_store: *LirStore,
layout_store: *layout.Store,
mir_layout_resolver: layout.MirMonotypeLayoutResolver,
analyses: *const Analyses,
lowered_lambdas: LoweredLambdaMap,
lowered_consts: LoweredConstMap,
builder_procs: std.ArrayList(BuilderProc),
current_local_map: ?*MirToLirLocalMap = null,
current_local_scopes: MirToLirLocalScopeStack = .empty,
current_origin_overrides: ?*MirOriginOverrideMap = null,
current_lambda: ?MIR.LambdaId = null,
current_borrow_region: LIR.BorrowRegion = .proc,
current_stmt: ?MIR.CFStmtId = null,
flushed: bool = false,

/// Initializes MIR-to-LIR lowering state over a finished MIR module.
pub fn init(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    lir_store: *LirStore,
    layout_store: *layout.Store,
    analyses: *const Analyses,
) Self {
    return .{
        .allocator = allocator,
        .mir_store = mir_store,
        .lir_store = lir_store,
        .layout_store = layout_store,
        .mir_layout_resolver = layout.MirMonotypeLayoutResolver.init(
            allocator,
            &mir_store.monotype_store,
            layout_store,
        ),
        .analyses = analyses,
        .lowered_lambdas = LoweredLambdaMap.init(allocator),
        .lowered_consts = LoweredConstMap.init(allocator),
        .builder_procs = std.ArrayList(BuilderProc).empty,
    };
}

/// Releases temporary lowering state owned by this translator.
pub fn deinit(self: *Self) void {
    if (builtin.mode == .Debug and self.current_local_scopes.items.len != 0) {
        std.debug.panic(
            "MirToLir invariant violated: local override scopes were not fully unwound before deinit",
            .{},
        );
    }
    for (self.current_local_scopes.items) |*scope| {
        scope.deinit();
    }
    self.current_local_scopes.deinit(self.allocator);
    self.lowered_lambdas.deinit();
    self.lowered_consts.deinit();
    self.builder_procs.deinit(self.allocator);
    self.mir_layout_resolver.deinit();
}

/// Lowers a root MIR constant body into a zero-argument LIR proc and flushes all lowered procs.
pub fn lower(self: *Self, root_const_id: MIR.ConstDefId) Allocator.Error!LirProcSpecId {
    self.ensureCanLowerMoreProcs();

    const root_const = self.mir_store.getConstDef(root_const_id);
    const ret_layout = try self.runtimeValueLayoutFromMirMonotype(root_const.monotype);
    const result_contract = try self.lowerSummaryResultContract(
        self.analyses.getConstResultContract(root_const_id),
    );
    const body = try self.lowerRootBody(root_const.body);

    if (builtin.mode == .Debug) {
        // This verifier exists only to catch compiler implementation bugs by
        // re-scanning already-lowered LIR. It must remain debug-only because
        // release compiler builds must not pay for extra full-LIR verification.
        try DebugVerifyLir.verifyProc(
            self.allocator,
            self.lir_store,
            self.layout_store,
            "root const",
            @intFromEnum(root_const_id),
            ret_layout,
            LirLocalSpan.empty(),
            result_contract,
            body,
        );
    }

    const proc = LirProcSpec{
        .name = self.lir_store.freshSyntheticSymbol(),
        .args = LirLocalSpan.empty(),
        .body = body,
        .ret_layout = ret_layout,
        .result_contract = result_contract,
        .hosted = null,
    };
    const root_proc_id = try self.addResolvedProc(proc);
    try self.flush();
    return root_proc_id;
}

/// Lowers an entrypoint MIR constant body into an explicit-argument LIR proc.
pub fn lowerEntrypointProc(
    self: *Self,
    root_const_id: MIR.ConstDefId,
    arg_layouts: []const layout.Idx,
    ret_layout: layout.Idx,
) Allocator.Error!LirProcSpecId {
    self.ensureCanLowerMoreProcs();

    const args = try self.allocator.alloc(LirLocalId, arg_layouts.len);
    defer self.allocator.free(args);

    for (arg_layouts, 0..) |arg_layout, i| {
        args[i] = try self.freshLocal(arg_layout);
    }

    const arg_span = try self.lir_store.addLocalSpan(args);
    const lowered = try self.lowerEntrypointProcBody(root_const_id, args, ret_layout);

    if (builtin.mode == .Debug) {
        // This verifier exists only to catch compiler implementation bugs by
        // re-scanning already-lowered LIR. It must remain debug-only because
        // release compiler builds must not pay for extra full-LIR verification.
        try DebugVerifyLir.verifyProc(
            self.allocator,
            self.lir_store,
            self.layout_store,
            "entrypoint root const",
            @intFromEnum(root_const_id),
            ret_layout,
            arg_span,
            lowered.result_contract,
            lowered.body,
        );
    }

    const proc = LirProcSpec{
        .name = self.lir_store.freshSyntheticSymbol(),
        .args = arg_span,
        .body = lowered.body,
        .ret_layout = ret_layout,
        .result_contract = lowered.result_contract,
        .hosted = null,
    };
    return self.addResolvedProc(proc);
}

/// Finalizes all builder-owned procs into the destination `LirStore`.
pub fn flush(self: *Self) Allocator.Error!void {
    if (self.flushed) return;
    try self.flushBuilderProcs();
    self.flushed = true;
}

fn ensureCanLowerMoreProcs(self: *const Self) void {
    if (self.flushed) {
        std.debug.panic(
            "MirToLir invariant violated: translator instances are single-use after proc flushing",
            .{},
        );
    }
    if (self.lir_store.getProcSpecs().len != 0) {
        std.debug.panic(
            "MirToLir invariant violated: strongest-form proc flushing requires an empty LirStore proc table",
            .{},
        );
    }
}

fn freshLocal(self: *Self, layout_idx: layout.Idx) Allocator.Error!LirLocalId {
    return self.lir_store.addLocal(.{
        .layout_idx = layout_idx,
    });
}

fn runtimeRepresentationLayoutIdx(self: *Self, layout_idx: layout.Idx) layout.Idx {
    if (@intFromEnum(layout_idx) >= self.layout_store.layouts.len()) return layout_idx;

    const layout_val = self.layout_store.getLayout(layout_idx);
    return switch (layout_val.tag) {
        .closure => self.runtimeRepresentationLayoutIdx(layout_val.data.closure.captures_layout_idx),
        else => layout_idx,
    };
}

fn runtimeValueLayoutFromMirMonotype(self: *Self, monotype: MirMonotypeIdx) Allocator.Error!layout.Idx {
    const resolved = try self.mir_layout_resolver.resolve(monotype, null);
    return self.runtimeRepresentationLayoutIdx(resolved);
}

fn runtimeValueLayoutFromMirLocal(self: *Self, local_id: MIR.LocalId) Allocator.Error!layout.Idx {
    return self.runtimeValueLayoutFromMirMonotype(self.mir_store.getLocal(local_id).monotype);
}

fn runtimeLambdaValueLayout(self: *Self, lambda_id: MIR.LambdaId) Allocator.Error!layout.Idx {
    const lambda = self.mir_store.getLambda(lambda_id);
    if (lambda.captures_param) |captures_param| {
        return self.runtimeValueLayoutFromMirLocal(captures_param);
    }
    return self.runtimeValueLayoutFromMirMonotype(self.mir_store.monotype_store.unit_idx);
}

fn runtimeLambdaReturnLayout(self: *Self, lambda_id: MIR.LambdaId) Allocator.Error!layout.Idx {
    return self.runtimeValueLayoutFromMirMonotype(self.mir_store.getLambda(lambda_id).ret_monotype);
}

fn loweredOriginsEqual(left: LoweredOrigin, right: LoweredOrigin) bool {
    return std.meta.eql(left, right);
}

fn mergeLoweredOrigins(left: LoweredOrigin, right: LoweredOrigin) LoweredOrigin {
    if (loweredOriginsEqual(left, right)) return left;
    return .fresh;
}

fn requireExactCallableValue(self: *Self, local_id: MIR.LocalId) ResolvedCallable {
    const exact_callable = self.mir_store.getLocal(local_id).exact_callable orelse std.debug.panic(
        "MirToLir invariant violated: callable local {d} lacked explicit exact callable metadata",
        .{@intFromEnum(local_id)},
    );
    return .{
        .lambda = exact_callable.lambda,
        .captures_local = if (exact_callable.requires_hidden_capture) local_id else null,
    };
}

fn collectJoinOrigins(
    self: *Self,
    active_joins: *ActiveJoinOriginMap,
    stmt_id: MIR.CFStmtId,
) Allocator.Error!void {
    const stmt = self.mir_store.getCFStmt(stmt_id);
    switch (stmt) {
        .assign_symbol => |assign| try self.collectJoinOrigins(active_joins, assign.next),
        .assign_ref => |assign| try self.collectJoinOrigins(active_joins, assign.next),
        .assign_literal => |assign| try self.collectJoinOrigins(active_joins, assign.next),
        .assign_lambda => |assign| try self.collectJoinOrigins(active_joins, assign.next),
        .assign_closure => |assign| try self.collectJoinOrigins(active_joins, assign.next),
        .assign_call => |assign| try self.collectJoinOrigins(active_joins, assign.next),
        .assign_low_level => |assign| try self.collectJoinOrigins(active_joins, assign.next),
        .assign_list => |assign| try self.collectJoinOrigins(active_joins, assign.next),
        .assign_struct => |assign| try self.collectJoinOrigins(active_joins, assign.next),
        .assign_tag => |assign| try self.collectJoinOrigins(active_joins, assign.next),
        .debug => |debug_stmt| try self.collectJoinOrigins(active_joins, debug_stmt.next),
        .expect => |expect_stmt| try self.collectJoinOrigins(active_joins, expect_stmt.next),
        .runtime_error, .scope_exit, .ret, .crash => {},
        .switch_stmt => |switch_stmt| {
            for (self.mir_store.getSwitchBranches(switch_stmt.branches)) |branch| {
                try self.collectJoinOrigins(active_joins, branch.body);
            }
            try self.collectJoinOrigins(active_joins, switch_stmt.default_branch);
        },
        .borrow_scope => |scope_stmt| {
            try self.collectJoinOrigins(active_joins, scope_stmt.body);
            try self.collectJoinOrigins(active_joins, scope_stmt.remainder);
        },
        .join => |join_stmt| {
            const join_key = @intFromEnum(join_stmt.id);
            const join_params = self.mir_store.getLocalSpan(join_stmt.params);
            const merged_origins = try self.allocator.alloc(?LoweredOrigin, join_params.len);
            defer self.allocator.free(merged_origins);
            @memset(merged_origins, null);
            const merged_layouts = try self.allocator.alloc(?layout.Idx, join_params.len);
            defer self.allocator.free(merged_layouts);
            @memset(merged_layouts, null);
            const gop = try active_joins.getOrPut(join_key);
            if (gop.found_existing) {
                std.debug.panic(
                    "MirToLir invariant violated: nested/duplicate active origin join {d}",
                    .{join_key},
                );
            }
            gop.value_ptr.* = .{
                .params = join_params,
                .merged_origins = merged_origins,
                .merged_layouts = merged_layouts,
                .saw_incoming = false,
            };
            defer _ = active_joins.remove(join_key);

            try self.collectJoinOrigins(active_joins, join_stmt.remainder);
            const join_state = active_joins.getPtr(join_key) orelse unreachable;
            if (!join_state.saw_incoming) return;

            var body_origin_overrides = MirOriginOverrideMap.init(self.allocator);
            defer body_origin_overrides.deinit();
            try body_origin_overrides.ensureTotalCapacity(@intCast(join_params.len));

            for (join_params, merged_origins, 0..) |param, incoming_origin, i| {
                body_origin_overrides.putAssumeCapacity(
                    @as(u32, @intFromEnum(param)),
                    incoming_origin orelse std.debug.panic(
                        "MirToLir invariant violated: join {d} param {d} had no incoming origin",
                        .{ join_key, i },
                    ),
                );
            }

            const saved_origin_overrides = self.current_origin_overrides;
            self.current_origin_overrides = &body_origin_overrides;
            defer self.current_origin_overrides = saved_origin_overrides;

            try self.collectJoinOrigins(active_joins, join_stmt.body);
        },
        .jump => |jump| {
            const join_state = active_joins.getPtr(@intFromEnum(jump.id)) orelse return;
            const args = self.mir_store.getLocalSpan(jump.args);
            if (args.len != join_state.params.len) {
                std.debug.panic(
                    "MirToLir invariant violated: jump to join {d} passed {d} args, expected {d}",
                    .{ @intFromEnum(jump.id), args.len, join_state.params.len },
                );
            }

            join_state.saw_incoming = true;
            for (args, 0..) |arg, i| {
                var visited = std.AutoHashMap(u32, void).init(self.allocator);
                defer visited.deinit();
                const incoming_origin = try self.resolveMirLocalOrigin(arg, &visited);
                if (join_state.merged_origins[i]) |current| {
                    join_state.merged_origins[i] = mergeLoweredOrigins(current, incoming_origin);
                } else {
                    join_state.merged_origins[i] = incoming_origin;
                }

                const incoming_layout = try self.runtimeValueLayoutFromMirLocal(arg);
                if (join_state.merged_layouts[i]) |current_layout| {
                    if (current_layout != incoming_layout) {
                        std.debug.panic(
                            "MirToLir invariant violated: join {d} param {d} received incompatible incoming layouts {d} and {d}",
                            .{
                                @intFromEnum(jump.id),
                                i,
                                @intFromEnum(current_layout),
                                @intFromEnum(incoming_layout),
                            },
                        );
                    }
                } else {
                    join_state.merged_layouts[i] = incoming_layout;
                }
            }
        },
    }
}

fn buildJoinOverrideInfo(
    self: *Self,
    join_stmt: std.meta.TagPayload(MIR.CFStmt, .join),
) Allocator.Error!JoinOverrideInfo {
    var info = JoinOverrideInfo{
        .origins = MirOriginOverrideMap.init(self.allocator),
        .layouts = try self.allocator.alloc(layout.Idx, self.mir_store.getLocalSpan(join_stmt.params).len),
    };
    errdefer info.origins.deinit();
    errdefer self.allocator.free(info.layouts);

    const join_key = @intFromEnum(join_stmt.id);
    const join_params = self.mir_store.getLocalSpan(join_stmt.params);
    const merged_origins = try self.allocator.alloc(?LoweredOrigin, join_params.len);
    defer self.allocator.free(merged_origins);
    @memset(merged_origins, null);
    const merged_layouts = try self.allocator.alloc(?layout.Idx, join_params.len);
    defer self.allocator.free(merged_layouts);
    @memset(merged_layouts, null);

    var active_joins = ActiveJoinOriginMap.init(self.allocator);
    defer active_joins.deinit();

    const gop = try active_joins.getOrPut(join_key);
    if (gop.found_existing) unreachable;
    gop.value_ptr.* = .{
        .params = join_params,
        .merged_origins = merged_origins,
        .merged_layouts = merged_layouts,
        .saw_incoming = false,
    };
    defer _ = active_joins.remove(join_key);

    try self.collectJoinOrigins(&active_joins, join_stmt.remainder);
    const join_state = active_joins.getPtr(join_key) orelse unreachable;

    try info.origins.ensureTotalCapacity(@intCast(join_params.len));
    for (join_params, merged_origins, merged_layouts, 0..) |param, incoming_origin, incoming_layout, i| {
        info.layouts[i] = incoming_layout orelse try self.joinParamLayoutFromCurrentBindingOrMirLocal(param);
        if (incoming_origin == null) continue;
        info.origins.putAssumeCapacity(
            @as(u32, @intFromEnum(param)),
            incoming_origin orelse std.debug.panic(
                "MirToLir invariant violated: join {d} param {d} had no incoming origin",
                .{ join_key, i },
            ),
        );
    }
    _ = join_state;
    return info;
}

fn translateBorrowScopeId(scope_id: MIR.BorrowScopeId) LIR.BorrowScopeId {
    return @enumFromInt(@intFromEnum(scope_id));
}

fn translateJoinPointId(join_id: MIR.JoinPointId) JoinPointId {
    return @enumFromInt(@intFromEnum(join_id));
}

fn currentLocalMap(self: *Self) *MirToLirLocalMap {
    return self.current_local_map orelse std.debug.panic(
        "MirToLir invariant violated: MIR local lowering requires an active local map",
        .{},
    );
}

fn currentLocalOverrideScope(self: *Self) ?*MirToLirLocalMap {
    if (self.current_local_scopes.items.len == 0) return null;
    return &self.current_local_scopes.items[self.current_local_scopes.items.len - 1];
}

fn pushLocalOverrideScope(self: *Self) Allocator.Error!void {
    try self.current_local_scopes.append(self.allocator, MirToLirLocalMap.init(self.allocator));
}

fn popLocalOverrideScope(self: *Self) void {
    if (self.current_local_scopes.items.len == 0) {
        std.debug.panic(
            "MirToLir invariant violated: attempted to pop a missing local override scope",
            .{},
        );
    }
    var scope = self.current_local_scopes.pop().?;
    scope.deinit();
}

fn lookupMirLocalBinding(self: *Self, key: u32) ?LirLocalId {
    var scope_index = self.current_local_scopes.items.len;
    while (scope_index > 0) {
        scope_index -= 1;
        if (self.current_local_scopes.items[scope_index].get(key)) |existing| return existing;
    }
    return self.currentLocalMap().get(key);
}

fn bindMirLocal(self: *Self, key: u32, lir_local: LirLocalId) Allocator.Error!void {
    if (self.currentLocalMap().get(key)) |existing| {
        if (existing != lir_local) {
            std.debug.panic(
                "MirToLir invariant violated: MIR local {d} was rebound from LIR local {d} to {d}",
                .{ key, @intFromEnum(existing), @intFromEnum(lir_local) },
            );
        }
        return;
    }
    try self.currentLocalMap().put(key, lir_local);
}

fn mapMirLocal(self: *Self, mir_local: MIR.LocalId) Allocator.Error!LirLocalId {
    const key = @as(u32, @intFromEnum(mir_local));
    if (self.lookupMirLocalBinding(key)) |existing| return existing;

    if (self.current_stmt) |stmt_id| {
        std.debug.print(
            "MirToLir missing binding: stmt {d} tag={s} local={d} local_def={s}\n",
            .{
                @intFromEnum(stmt_id),
                @tagName(self.mir_store.getCFStmt(stmt_id)),
                @intFromEnum(mir_local),
                @tagName(self.mir_store.getLocalDef(mir_local)),
            },
        );
    } else {
        std.debug.print(
            "MirToLir missing binding: no current stmt local={d} local_def={s}\n",
            .{
                @intFromEnum(mir_local),
                @tagName(self.mir_store.getLocalDef(mir_local)),
            },
        );
    }

    std.debug.panic(
        "MirToLir invariant violated: MIR local {d} has no active LIR binding",
        .{@intFromEnum(mir_local)},
    );
}

fn pushMirTargetBinding(self: *Self, mir_local: MIR.LocalId) Allocator.Error!MirTargetBinding {
    return self.pushMirTargetBindingWithLayout(
        mir_local,
        try self.runtimeValueLayoutFromMirLocal(mir_local),
    );
}

fn pushMirTargetBindingWithLayout(
    self: *Self,
    mir_local: MIR.LocalId,
    layout_idx: layout.Idx,
) Allocator.Error!MirTargetBinding {
    const key = @as(u32, @intFromEnum(mir_local));
    if (self.lookupMirLocalBinding(key) != null) {
        std.debug.panic(
            "MirToLir invariant violated: attempted to create a second LIR binding for MIR local {d}",
            .{@intFromEnum(mir_local)},
        );
    }
    const target = try self.lir_store.addLocal(.{
        .layout_idx = layout_idx,
    });
    try self.currentLocalMap().put(key, target);
    return .{
        .key = key,
        .target = target,
    };
}

fn popMirTargetBinding(self: *Self, binding: MirTargetBinding) void {
    const existing = self.currentLocalMap().get(binding.key) orelse std.debug.panic(
        "MirToLir invariant violated: target binding for MIR local {d} disappeared before use completed",
        .{binding.key},
    );
    if (existing != binding.target) {
        std.debug.panic(
            "MirToLir invariant violated: MIR local {d} changed LIR binding from {d} to {d} during lowering",
            .{ binding.key, @intFromEnum(binding.target), @intFromEnum(existing) },
        );
    }
}

fn mapMirLocalSpan(self: *Self, span: MIR.LocalSpan) Allocator.Error!LirLocalSpan {
    const mir_locals = self.mir_store.getLocalSpan(span);
    const lir_locals = try self.allocator.alloc(LirLocalId, mir_locals.len);
    defer self.allocator.free(lir_locals);

    for (mir_locals, 0..) |mir_local, i| {
        lir_locals[i] = try self.mapMirLocal(mir_local);
    }
    return self.lir_store.addLocalSpan(lir_locals);
}

fn runtimeStructLayoutFromMirFields(
    self: *Self,
    mir_fields: []const MIR.LocalId,
) Allocator.Error!layout.Idx {
    const struct_fields = try self.allocator.alloc(layout.StructField, mir_fields.len);
    defer self.allocator.free(struct_fields);

    for (mir_fields, 0..) |mir_local, i| {
        const field_layout = try self.runtimeValueLayoutFromMirLocal(mir_local);
        struct_fields[i] = .{
            .index = @intCast(i),
            .layout = field_layout,
        };
    }

    return self.layout_store.putStructFields(struct_fields);
}

fn joinParamLayoutFromCurrentBindingOrMirLocal(
    self: *Self,
    mir_local: MIR.LocalId,
) Allocator.Error!layout.Idx {
    const key = @as(u32, @intFromEnum(mir_local));
    if (self.lookupMirLocalBinding(key)) |existing| {
        return self.lir_store.getLocal(existing).layout_idx;
    }
    return self.runtimeValueLayoutFromMirLocal(mir_local);
}

fn singleProjectionSpan(self: *Self, projection: LIR.RefProjection) Allocator.Error!LIR.RefProjectionSpan {
    return self.lir_store.addRefProjectionSpan(&.{projection});
}

fn aliasSemantics(owner: LirLocalId, projections: LIR.RefProjectionSpan) ResultSemantics {
    return .{ .alias_of = .{
        .owner = owner,
        .projections = projections,
    } };
}

fn borrowSemantics(owner: LirLocalId, projections: LIR.RefProjectionSpan, region: LIR.BorrowRegion) ResultSemantics {
    return .{ .borrow_of = .{
        .owner = owner,
        .projections = projections,
        .region = region,
    } };
}

fn lowerSummaryProjectionSpan(
    self: *Self,
    summary_span: ResultSummary.RefProjectionSpan,
) Allocator.Error!LIR.RefProjectionSpan {
    const summary_items = self.analyses.getRefProjectionSpan(summary_span);
    if (summary_items.len == 0) return LIR.RefProjectionSpan.empty();

    const lir_items = try self.allocator.alloc(LIR.RefProjection, summary_items.len);
    defer self.allocator.free(lir_items);

    for (summary_items, 0..) |projection, i| {
        lir_items[i] = switch (projection) {
            .field => |field_idx| .{ .field = field_idx },
            .tag_payload => |payload_idx| .{ .tag_payload = payload_idx },
            .nominal => .nominal,
        };
    }

    return self.lir_store.addRefProjectionSpan(lir_items);
}

fn lowerSummaryParamRefContract(
    self: *Self,
    contract: ResultSummary.ParamRefContract,
) Allocator.Error!LIR.ParamRefContract {
    return .{
        .param_index = contract.param_index,
        .projections = try self.lowerSummaryProjectionSpan(contract.projections),
    };
}

fn lowerSummaryResultContract(
    self: *Self,
    contract: ResultSummary.ResultContract,
) Allocator.Error!LIR.ProcResultContract {
    return switch (contract) {
        .no_return => .no_return,
        .fresh => .fresh,
        .alias_of_param => |param_ref| .{ .alias_of_param = try self.lowerSummaryParamRefContract(param_ref) },
        .borrow_of_param => |param_ref| .{ .borrow_of_param = try self.lowerSummaryParamRefContract(param_ref) },
    };
}

fn existingMappedMirLocal(self: *Self, mir_local: MIR.LocalId) LirLocalId {
    return self.lookupMirLocalBinding(@as(u32, @intFromEnum(mir_local))) orelse std.debug.panic(
        "MirToLir invariant violated: MIR local {d} had no existing LIR binding during provenance resolution",
        .{@intFromEnum(mir_local)},
    );
}

fn concatProjectionSpans(
    self: *Self,
    left: LIR.RefProjectionSpan,
    right: LIR.RefProjectionSpan,
) Allocator.Error!LIR.RefProjectionSpan {
    if (left.isEmpty()) return right;
    if (right.isEmpty()) return left;

    const left_items = self.lir_store.getRefProjectionSpan(left);
    const right_items = self.lir_store.getRefProjectionSpan(right);
    const combined = try self.allocator.alloc(LIR.RefProjection, left_items.len + right_items.len);
    defer self.allocator.free(combined);

    @memcpy(combined[0..left_items.len], left_items);
    @memcpy(combined[left_items.len..], right_items);
    return self.lir_store.addRefProjectionSpan(combined);
}

fn aliasOrigin(
    self: *Self,
    origin: LoweredOrigin,
    extra_projections: LIR.RefProjectionSpan,
) Allocator.Error!LoweredOrigin {
    return switch (origin) {
        .fresh => .fresh,
        .alias_of_local => |aliased| .{ .alias_of_local = .{
            .owner = aliased.owner,
            .projections = try self.concatProjectionSpans(aliased.projections, extra_projections),
        } },
        .borrow_of_local => |borrowed| .{ .borrow_of_local = .{
            .owner = borrowed.owner,
            .projections = try self.concatProjectionSpans(borrowed.projections, extra_projections),
            .region = borrowed.region,
        } },
        .borrow_of_fresh => |borrowed| .{ .borrow_of_fresh = .{
            .projections = try self.concatProjectionSpans(borrowed.projections, extra_projections),
            .region = borrowed.region,
        } },
    };
}

fn borrowOrigin(
    self: *Self,
    origin: LoweredOrigin,
    region: LIR.BorrowRegion,
    extra_projections: LIR.RefProjectionSpan,
) Allocator.Error!LoweredOrigin {
    return switch (origin) {
        .fresh => .{ .borrow_of_fresh = .{
            .projections = extra_projections,
            .region = region,
        } },
        .alias_of_local => |aliased| .{ .borrow_of_local = .{
            .owner = aliased.owner,
            .projections = try self.concatProjectionSpans(aliased.projections, extra_projections),
            .region = region,
        } },
        .borrow_of_local => |borrowed| .{ .borrow_of_local = .{
            .owner = borrowed.owner,
            .projections = try self.concatProjectionSpans(borrowed.projections, extra_projections),
            .region = borrowed.region,
        } },
        .borrow_of_fresh => |borrowed| .{ .borrow_of_fresh = .{
            .projections = try self.concatProjectionSpans(borrowed.projections, extra_projections),
            .region = borrowed.region,
        } },
    };
}

fn loweredOriginToResultSemantics(origin: LoweredOrigin) ResultSemantics {
    return switch (origin) {
        .fresh => .fresh,
        .alias_of_local => |aliased| aliasSemantics(aliased.owner, aliased.projections),
        .borrow_of_local => |borrowed| borrowSemantics(borrowed.owner, borrowed.projections, borrowed.region),
        .borrow_of_fresh => .fresh,
    };
}

fn projectedResultSemantics(
    self: *Self,
    source: MIR.LocalId,
    extra_projections: LIR.RefProjectionSpan,
    ownership: MIR.ProjectionOwnership,
) Allocator.Error!ResultSemantics {
    var visited = std.AutoHashMap(u32, void).init(self.allocator);
    defer visited.deinit();

    const source_origin = try self.resolveMirLocalOrigin(source, &visited);
    return switch (ownership) {
        .borrow => loweredOriginToResultSemantics(
            try self.borrowOrigin(source_origin, self.current_borrow_region, extra_projections),
        ),
        // Partial projections cannot safely forward ownership from a parent
        // aggregate. When the source itself is fresh, keep the borrow rooted in
        // that concrete source local so RC insertion can close only the parent.
        .move => switch (source_origin) {
            .fresh => borrowSemantics(
                try self.mapMirLocal(source),
                extra_projections,
                self.current_borrow_region,
            ),
            else => loweredOriginToResultSemantics(
                try self.borrowOrigin(source_origin, self.current_borrow_region, extra_projections),
            ),
        },
    };
}

fn lowerSummaryCallResultOrigin(
    self: *Self,
    contract: ResultSummary.ResultContract,
    arg_origins: []const LoweredOrigin,
) Allocator.Error!LoweredOrigin {
    return switch (contract) {
        .no_return => std.debug.panic(
            "MirToLir invariant violated: no-return callable must not be lowered as a value-producing call result",
            .{},
        ),
        .fresh => .fresh,
        .alias_of_param => |param_ref| blk: {
            if (param_ref.param_index >= arg_origins.len) {
                std.debug.panic(
                    "MirToLir invariant violated: callable result aliases arg {d}, but call only has {d} args",
                    .{ param_ref.param_index, arg_origins.len },
                );
            }
            break :blk try self.aliasOrigin(
                arg_origins[param_ref.param_index],
                try self.lowerSummaryProjectionSpan(param_ref.projections),
            );
        },
        .borrow_of_param => |param_ref| blk: {
            if (param_ref.param_index >= arg_origins.len) {
                std.debug.panic(
                    "MirToLir invariant violated: callable result borrows arg {d}, but call only has {d} args",
                    .{ param_ref.param_index, arg_origins.len },
                );
            }
            break :blk try self.borrowOrigin(
                arg_origins[param_ref.param_index],
                self.current_borrow_region,
                try self.lowerSummaryProjectionSpan(param_ref.projections),
            );
        },
    };
}

fn resolveMirLocalOrigin(
    self: *Self,
    local_id: MIR.LocalId,
    visited: *std.AutoHashMap(u32, void),
) Allocator.Error!LoweredOrigin {
    const key = @as(u32, @intFromEnum(local_id));
    const gop = try visited.getOrPut(key);
    if (gop.found_existing) {
        std.debug.panic(
            "MirToLir invariant violated: cyclic MIR provenance for local {d}",
            .{@intFromEnum(local_id)},
        );
    }
    defer _ = visited.remove(key);

    return switch (self.mir_store.getLocalDef(local_id)) {
        .param => .{ .alias_of_local = .{
            .owner = self.existingMappedMirLocal(local_id),
        } },
        .captures_param => .{ .borrow_of_local = .{
            .owner = self.existingMappedMirLocal(local_id),
            .region = .proc,
        } },
        .join_param => if (self.current_origin_overrides) |overrides|
            overrides.get(key) orelse .{ .alias_of_local = .{
                .owner = self.existingMappedMirLocal(local_id),
            } }
        else
            .{ .alias_of_local = .{
                .owner = self.existingMappedMirLocal(local_id),
            } },
        .symbol,
        .literal,
        .lambda,
        .closure,
        .struct_,
        .tag,
        .list,
        => .fresh,
        .ref => |ref_op| switch (ref_op) {
            .local => |source| try self.aliasOrigin(
                try self.resolveMirLocalOrigin(source, visited),
                LIR.RefProjectionSpan.empty(),
            ),
            .nominal => |nominal| try self.aliasOrigin(
                try self.resolveMirLocalOrigin(nominal.backing, visited),
                try self.singleProjectionSpan(.nominal),
            ),
            .field => |field| blk: {
                const source_origin = try self.resolveMirLocalOrigin(field.source, visited);
                const projections = try self.singleProjectionSpan(.{ .field = @intCast(field.field_idx) });
                break :blk switch (field.ownership) {
                    .borrow => try self.borrowOrigin(source_origin, self.current_borrow_region, projections),
                    .move => switch (source_origin) {
                        .fresh => .{ .borrow_of_local = .{
                            .owner = self.existingMappedMirLocal(field.source),
                            .projections = projections,
                            .region = self.current_borrow_region,
                        } },
                        else => try self.borrowOrigin(source_origin, self.current_borrow_region, projections),
                    },
                };
            },
            .tag_payload => |payload| blk: {
                const source_origin = try self.resolveMirLocalOrigin(payload.source, visited);
                const projections = try self.singleProjectionSpan(.{ .tag_payload = @intCast(payload.payload_idx) });
                break :blk switch (payload.ownership) {
                    .borrow => try self.borrowOrigin(source_origin, self.current_borrow_region, projections),
                    .move => switch (source_origin) {
                        .fresh => .{ .borrow_of_local = .{
                            .owner = self.existingMappedMirLocal(payload.source),
                            .projections = projections,
                            .region = self.current_borrow_region,
                        } },
                        else => try self.borrowOrigin(source_origin, self.current_borrow_region, projections),
                    },
                };
            },
            .discriminant => .fresh,
        },
        .call => |call_result| blk: {
            const resolved = ResolvedCallable{
                .lambda = call_result.exact_lambda,
                .captures_local = if (call_result.exact_requires_hidden_capture) call_result.callee else null,
            };

            const visible_args = self.mir_store.getLocalSpan(call_result.args);
            const arg_count = visible_args.len + @intFromBool(resolved.captures_local != null);
            const arg_origins = try self.allocator.alloc(LoweredOrigin, arg_count);
            defer self.allocator.free(arg_origins);

            for (visible_args, 0..) |arg, i| {
                arg_origins[i] = try self.resolveMirLocalOrigin(arg, visited);
            }
            if (resolved.captures_local) |captures_local| {
                arg_origins[visible_args.len] = try self.resolveMirLocalOrigin(captures_local, visited);
            }

            break :blk try self.lowerSummaryCallResultOrigin(
                self.analyses.getLambdaResultContract(resolved.lambda),
                arg_origins,
            );
        },
        .low_level => |low_level| blk: {
            const args = self.mir_store.getLocalSpan(low_level.args);
            const arg_origins = try self.allocator.alloc(LoweredOrigin, args.len);
            defer self.allocator.free(arg_origins);

            for (args, 0..) |arg, i| {
                arg_origins[i] = try self.resolveMirLocalOrigin(arg, visited);
            }

            break :blk switch (low_level.op.procResultSemantics()) {
                .fresh => .fresh,
                .alias_arg => |arg_index| {
                    if (arg_index >= arg_origins.len) {
                        std.debug.panic(
                            "MirToLir invariant violated: low-level {s} aliases arg {d}, but call only has {d} args",
                            .{ @tagName(low_level.op), arg_index, arg_origins.len },
                        );
                    }
                    break :blk try self.aliasOrigin(arg_origins[arg_index], LIR.RefProjectionSpan.empty());
                },
                .borrow_arg => |arg_index| {
                    if (arg_index >= arg_origins.len) {
                        std.debug.panic(
                            "MirToLir invariant violated: low-level {s} borrows arg {d}, but call only has {d} args",
                            .{ @tagName(low_level.op), arg_index, arg_origins.len },
                        );
                    }
                    break :blk try self.borrowOrigin(arg_origins[arg_index], self.current_borrow_region, LIR.RefProjectionSpan.empty());
                },
                .no_return => std.debug.panic(
                    "MirToLir invariant violated: no-return low-level {s} must not appear in local-origin resolution",
                    .{@tagName(low_level.op)},
                ),
                .requires_explicit_summary => std.debug.panic(
                    "MirToLir invariant violated: low-level {s} requires explicit provenance summary during local-origin resolution",
                    .{@tagName(low_level.op)},
                ),
            };
        },
    };
}

fn directLambdaCallResultSemantics(
    self: *Self,
    resolved: ResolvedCallable,
    visible_args: []const MIR.LocalId,
) Allocator.Error!ResultSemantics {
    const arg_count = visible_args.len + @intFromBool(resolved.captures_local != null);
    const arg_origins = try self.allocator.alloc(LoweredOrigin, arg_count);
    defer self.allocator.free(arg_origins);

    var visited = std.AutoHashMap(u32, void).init(self.allocator);
    defer visited.deinit();

    for (visible_args, 0..) |arg, i| {
        arg_origins[i] = try self.resolveMirLocalOrigin(arg, &visited);
    }
    if (resolved.captures_local) |captures_local| {
        arg_origins[visible_args.len] = try self.resolveMirLocalOrigin(captures_local, &visited);
    }

    return loweredOriginToResultSemantics(
        try self.lowerSummaryCallResultOrigin(
            self.analyses.getLambdaResultContract(resolved.lambda),
            arg_origins,
        ),
    );
}

fn lowLevelResultSemantics(
    _: *Self,
    region: LIR.BorrowRegion,
    op: LIR.LowLevel,
    args: []const LirLocalId,
) ResultSemantics {
    return switch (op.procResultSemantics()) {
        .fresh => .fresh,
        .alias_arg => |arg_index| blk: {
            if (arg_index >= args.len) {
                std.debug.panic(
                    "MirToLir invariant violated: low-level {s} aliases arg {d}, but call only has {d} args",
                    .{ @tagName(op), arg_index, args.len },
                );
            }
            break :blk aliasSemantics(args[arg_index], LIR.RefProjectionSpan.empty());
        },
        .borrow_arg => |arg_index| blk: {
            if (arg_index >= args.len) {
                std.debug.panic(
                    "MirToLir invariant violated: low-level {s} borrows arg {d}, but call only has {d} args",
                    .{ @tagName(op), arg_index, args.len },
                );
            }
            break :blk borrowSemantics(args[arg_index], LIR.RefProjectionSpan.empty(), region);
        },
        .no_return => std.debug.panic(
            "MirToLir invariant violated: no-return low-level {s} must not be lowered as a value-producing statement",
            .{@tagName(op)},
        ),
        .requires_explicit_summary => std.debug.panic(
            "MirToLir invariant violated: low-level result {s} must be explicitly classified",
            .{@tagName(op)},
        ),
    };
}

fn getResultContract(self: *const Self, proc_id: LirProcSpecId) LIR.ProcResultContract {
    return switch (self.builder_procs.items[@intFromEnum(proc_id)]) {
        .resolved => |proc| proc.result_contract,
        .unresolved => |header| header.result_contract,
    };
}

fn callResultSemantics(self: *Self, proc_id: LirProcSpecId, args: []const LirLocalId) ResultSemantics {
    return switch (self.getResultContract(proc_id)) {
        .no_return => std.debug.panic(
            "MirToLir invariant violated: no-return proc {d} ({d}) must not be lowered as a value-producing call",
            .{
                @intFromEnum(proc_id),
                switch (self.builder_procs.items[@intFromEnum(proc_id)]) {
                    .resolved => |proc| proc.name.raw(),
                    .unresolved => |header| header.name.raw(),
                },
            },
        ),
        .fresh => .fresh,
        .alias_of_param => |param_ref| if (param_ref.param_index < args.len)
            aliasSemantics(args[param_ref.param_index], param_ref.projections)
        else
            std.debug.panic(
                "MirToLir invariant violated: proc {d} aliases param {d}, but call only has {d} args",
                .{ @intFromEnum(proc_id), param_ref.param_index, args.len },
            ),
        .borrow_of_param => |param_ref| if (param_ref.param_index < args.len)
            borrowSemantics(args[param_ref.param_index], param_ref.projections, self.current_borrow_region)
        else
            std.debug.panic(
                "MirToLir invariant violated: proc {d} borrows param {d}, but call only has {d} args",
                .{ @intFromEnum(proc_id), param_ref.param_index, args.len },
            ),
    };
}

fn addUnresolvedProc(self: *Self, header: BuilderProcHeader) Allocator.Error!LirProcSpecId {
    const idx = self.builder_procs.items.len;
    try self.builder_procs.append(self.allocator, .{ .unresolved = header });
    return @enumFromInt(@as(u32, @intCast(idx)));
}

fn addResolvedProc(self: *Self, proc: LirProcSpec) Allocator.Error!LirProcSpecId {
    const idx = self.builder_procs.items.len;
    try self.builder_procs.append(self.allocator, .{ .resolved = proc });
    return @enumFromInt(@as(u32, @intCast(idx)));
}

fn resolveProc(self: *Self, proc_id: LirProcSpecId, proc: LirProcSpec) void {
    const entry = &self.builder_procs.items[@intFromEnum(proc_id)];
    switch (entry.*) {
        .unresolved => entry.* = .{ .resolved = proc },
        .resolved => std.debug.panic(
            "MirToLir invariant violated: proc {d} was resolved twice",
            .{@intFromEnum(proc_id)},
        ),
    }
}

fn flushBuilderProcs(self: *Self) Allocator.Error!void {
    for (self.builder_procs.items, 0..) |entry, i| {
        const proc = switch (entry) {
            .resolved => |proc| proc,
            .unresolved => |header| std.debug.panic(
                "MirToLir invariant violated: proc {d} was flushed before it finished lowering",
                .{header.name.raw()},
            ),
        };

        const flushed_id = try self.lir_store.addProcSpec(proc);
        if (@intFromEnum(flushed_id) != i) {
            std.debug.panic(
                "MirToLir invariant violated: flushed proc ids must preserve builder order",
                .{},
            );
        }
    }
}

fn emitAssignSymbol(
    self: *Self,
    target: LirLocalId,
    symbol: MIR.Symbol,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    return self.lir_store.addCFStmt(.{ .assign_symbol = .{
        .target = target,
        .symbol = symbol,
        .next = next,
    } });
}

fn lowerConstProc(self: *Self, const_id: MIR.ConstDefId) Allocator.Error!LirProcSpecId {
    const const_key = @as(u32, @intFromEnum(const_id));
    if (self.lowered_consts.get(const_key)) |existing| return existing;

    const const_def = self.mir_store.getConstDef(const_id);
    const ret_layout = try self.runtimeValueLayoutFromMirMonotype(const_def.monotype);
    const result_contract = try self.lowerSummaryResultContract(
        self.analyses.getConstResultContract(const_id),
    );

    const proc_id = try self.addUnresolvedProc(.{
        .name = const_def.symbol,
        .args = LirLocalSpan.empty(),
        .ret_layout = ret_layout,
        .result_contract = result_contract,
        .hosted = null,
    });
    try self.lowered_consts.put(const_key, proc_id);

    const body = try self.lowerRootBody(const_def.body);

    if (builtin.mode == .Debug) {
        try DebugVerifyLir.verifyProc(
            self.allocator,
            self.lir_store,
            self.layout_store,
            "const",
            @intFromEnum(const_id),
            ret_layout,
            LirLocalSpan.empty(),
            result_contract,
            body,
        );
    }

    self.resolveProc(proc_id, .{
        .name = const_def.symbol,
        .args = LirLocalSpan.empty(),
        .body = body,
        .ret_layout = ret_layout,
        .result_contract = result_contract,
        .hosted = null,
    });
    return proc_id;
}

fn lowerMirSymbolValue(
    self: *Self,
    target_mir: MIR.LocalId,
    target: LirLocalId,
    symbol: MIR.Symbol,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    if (self.mir_store.getConstDefForSymbol(symbol)) |const_def_id| {
        const proc_id = try self.lowerConstProc(const_def_id);
        const empty_args = LirLocalSpan.empty();
        return self.lir_store.addCFStmt(.{ .assign_call = .{
            .target = target,
            .result = self.callResultSemantics(proc_id, &.{}),
            .proc = proc_id,
            .args = empty_args,
            .next = next,
        } });
    }

    return self.emitAssignSymbol(target, symbol, next);
}

fn emitAssignRef(
    self: *Self,
    target: LirLocalId,
    result: ResultSemantics,
    op: RefOp,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    switch (op) {
        .local => |source| {
            if (target == source) switch (result) {
                .alias_of => |aliased| {
                    if (aliased.owner == source and aliased.projections.isEmpty()) {
                        return next;
                    }
                },
                .borrow_of => |borrowed| {
                    if (borrowed.owner == source and borrowed.projections.isEmpty()) {
                        std.debug.panic(
                            "MirToLir invariant violated: self-local borrow assign_ref must not be emitted",
                            .{},
                        );
                    }
                },
                .fresh => {},
            };
        },
        else => {},
    }

    return self.lir_store.addCFStmt(.{ .assign_ref = .{
        .target = target,
        .result = result,
        .op = op,
        .next = next,
    } });
}

fn emitAssignLiteral(
    self: *Self,
    target: LirLocalId,
    value: LiteralValue,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    return self.lir_store.addCFStmt(.{ .assign_literal = .{
        .target = target,
        .result = .fresh,
        .value = value,
        .next = next,
    } });
}

fn emitAssignUnit(self: *Self, target: LirLocalId, next: CFStmtId) Allocator.Error!CFStmtId {
    return self.lir_store.addCFStmt(.{ .assign_struct = .{
        .target = target,
        .result = .fresh,
        .fields = LirLocalSpan.empty(),
        .next = next,
    } });
}

fn emitRet(self: *Self, value: LirLocalId) Allocator.Error!CFStmtId {
    return self.lir_store.addCFStmt(.{ .ret = .{ .value = value } });
}

fn mirIntValueAsI128(int_value: @import("can").CIR.IntValue) i128 {
    return switch (int_value.kind) {
        .i128 => @as(i128, @bitCast(int_value.bytes)),
        .u128 => blk: {
            const unsigned: u128 = @bitCast(int_value.bytes);
            if (unsigned > @as(u128, @intCast(std.math.maxInt(i128)))) {
                std.debug.panic(
                    "MirToLir invariant violated: unsigned integer literal does not fit in signed i128 while lowering strongest-form literal",
                    .{},
                );
            }
            break :blk @intCast(unsigned);
        },
    };
}

fn mirIntValueAsUnsigned(int_value: @import("can").CIR.IntValue, max_value: u128, layout_name: []const u8) u128 {
    return switch (int_value.kind) {
        .u128 => blk: {
            const unsigned: u128 = @bitCast(int_value.bytes);
            if (unsigned > max_value) {
                std.debug.panic(
                    "MirToLir invariant violated: unsigned integer literal does not fit in {s}",
                    .{layout_name},
                );
            }
            break :blk unsigned;
        },
        .i128 => blk: {
            const signed: i128 = @bitCast(int_value.bytes);
            if (signed < 0 or signed > @as(i128, @intCast(max_value))) {
                std.debug.panic(
                    "MirToLir invariant violated: signed integer literal does not fit in unsigned {s}",
                    .{layout_name},
                );
            }
            break :blk @intCast(signed);
        },
    };
}

fn mirIntValueAsSigned(int_value: @import("can").CIR.IntValue, min_value: i128, max_value: i128, layout_name: []const u8) i128 {
    return switch (int_value.kind) {
        .i128 => blk: {
            const signed: i128 = @bitCast(int_value.bytes);
            if (signed < min_value or signed > max_value) {
                std.debug.panic(
                    "MirToLir invariant violated: integer literal does not fit in signed {s}",
                    .{layout_name},
                );
            }
            break :blk signed;
        },
        .u128 => blk: {
            const unsigned: u128 = @bitCast(int_value.bytes);
            if (unsigned > @as(u128, @intCast(max_value))) {
                std.debug.panic(
                    "MirToLir invariant violated: unsigned integer literal does not fit in signed {s}",
                    .{layout_name},
                );
            }
            break :blk @intCast(unsigned);
        },
    };
}

fn mirIntValueAsI64Bits(int_value: @import("can").CIR.IntValue, target_layout: layout.Idx) i64 {
    return switch (target_layout) {
        .u8 => blk: {
            const value: u8 = @intCast(mirIntValueAsUnsigned(int_value, std.math.maxInt(u8), "u8"));
            const bits: u64 = value;
            break :blk @bitCast(bits);
        },
        .i8 => blk: {
            const value: i8 = @intCast(mirIntValueAsSigned(int_value, std.math.minInt(i8), std.math.maxInt(i8), "i8"));
            const raw: u8 = @bitCast(value);
            const bits: u64 = raw;
            break :blk @bitCast(bits);
        },
        .u16 => blk: {
            const value: u16 = @intCast(mirIntValueAsUnsigned(int_value, std.math.maxInt(u16), "u16"));
            const bits: u64 = value;
            break :blk @bitCast(bits);
        },
        .i16 => blk: {
            const value: i16 = @intCast(mirIntValueAsSigned(int_value, std.math.minInt(i16), std.math.maxInt(i16), "i16"));
            const raw: u16 = @bitCast(value);
            const bits: u64 = raw;
            break :blk @bitCast(bits);
        },
        .u32 => blk: {
            const value: u32 = @intCast(mirIntValueAsUnsigned(int_value, std.math.maxInt(u32), "u32"));
            const bits: u64 = value;
            break :blk @bitCast(bits);
        },
        .i32 => blk: {
            const value: i32 = @intCast(mirIntValueAsSigned(int_value, std.math.minInt(i32), std.math.maxInt(i32), "i32"));
            const raw: u32 = @bitCast(value);
            const bits: u64 = raw;
            break :blk @bitCast(bits);
        },
        .u64 => blk: {
            const value: u64 = @intCast(mirIntValueAsUnsigned(int_value, std.math.maxInt(u64), "u64"));
            break :blk @bitCast(value);
        },
        .i64 => @intCast(mirIntValueAsSigned(int_value, std.math.minInt(i64), std.math.maxInt(i64), "i64")),
        else => std.debug.panic(
            "MirToLir invariant violated: non-i64-sized layout {d} requested raw i64 literal bits",
            .{@intFromEnum(target_layout)},
        ),
    };
}

fn mirLiteralValue(self: *Self, target_layout: layout.Idx, literal: MIR.LiteralValue) LiteralValue {
    return switch (literal) {
        .int => |int_lit| switch (target_layout) {
            .bool => .{ .bool_literal = mirIntValueAsI128(int_lit) != 0 },
            .u128, .i128 => .{ .i128_literal = .{
                .value = @bitCast(int_lit.bytes),
                .layout_idx = target_layout,
            } },
            .dec => blk: {
                const scaled = @mulWithOverflow(mirIntValueAsI128(int_lit), dec_literal_scale);
                if (scaled[1] != 0) {
                    std.debug.panic(
                        "MirToLir invariant violated: integer literal overflowed Dec representation during strongest-form lowering",
                        .{},
                    );
                }
                break :blk .{ .dec_literal = scaled[0] };
            },
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64 => .{ .i64_literal = .{
                .value = mirIntValueAsI64Bits(int_lit, target_layout),
                .layout_idx = target_layout,
            } },
            else => std.debug.panic(
                "MirToLir invariant violated: integer literal lowered to unsupported layout idx {d}",
                .{@intFromEnum(target_layout)},
            ),
        },
        .frac_f32 => |f| .{ .f32_literal = f },
        .frac_f64 => |f| .{ .f64_literal = f },
        .dec => |d| .{ .dec_literal = d.num },
        .str => |s| .{ .str_literal = self.internMirStringLiteral(s) catch unreachable },
    };
}

fn internMirStringLiteral(
    self: *Self,
    mir_idx: base.StringLiteral.Idx,
) Allocator.Error!base.StringLiteral.Idx {
    return self.lir_store.insertString(self.mir_store.getString(mir_idx));
}

fn tagDiscriminantForMonotypeName(
    self: *Self,
    target_mono: MirMonotypeIdx,
    tag_name: corecir.Monotype.Name,
) u16 {
    const mono = self.mir_store.monotype_store.getMonotype(target_mono);
    const tags = switch (mono) {
        .tag_union => |tag_union| self.mir_store.monotype_store.getTags(tag_union.tags),
        else => std.debug.panic(
            "MirToLir invariant violated: tag constructor does not target a tag-union monotype",
            .{},
        ),
    };

    for (tags, 0..) |tag, index| {
        if (tag.name.textEqual(self.analyses.all_module_envs, tag_name)) return @intCast(index);
    }

    std.debug.panic(
        "MirToLir invariant violated: tag constructor missing from target monotype",
        .{},
    );
}

fn lowerSwitchStmt(
    self: *Self,
    switch_stmt: std.meta.TagPayload(MIR.CFStmt, .switch_stmt),
) Allocator.Error!CFStmtId {
    const branches = self.mir_store.getSwitchBranches(switch_stmt.branches);
    const lowered_branches = try self.allocator.alloc(LIR.CFSwitchBranch, branches.len);
    defer self.allocator.free(lowered_branches);

    for (branches, 0..) |branch, i| {
        lowered_branches[i] = blk: {
            try self.pushLocalOverrideScope();
            defer self.popLocalOverrideScope();
            break :blk .{
                .value = branch.value,
                .body = try self.lowerStmt(branch.body),
            };
        };
    }

    const lowered_default = blk: {
        try self.pushLocalOverrideScope();
        defer self.popLocalOverrideScope();
        break :blk try self.lowerStmt(switch_stmt.default_branch);
    };

    return self.lir_store.addCFStmt(.{ .switch_stmt = .{
        .cond = try self.mapMirLocal(switch_stmt.scrutinee),
        .branches = try self.lir_store.addCFSwitchBranches(lowered_branches),
        .default_branch = lowered_default,
    } });
}

fn lowerRootBody(self: *Self, root_body: MIR.CFStmtId) Allocator.Error!CFStmtId {
    var local_map = MirToLirLocalMap.init(self.allocator);
    defer local_map.deinit();
    const prev_local_map = self.current_local_map;
    const prev_origin_overrides = self.current_origin_overrides;
    const prev_lambda = self.current_lambda;
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_origin_overrides = null;
    self.current_lambda = null;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_origin_overrides = prev_origin_overrides;
    defer self.current_lambda = prev_lambda;
    defer self.current_borrow_region = prev_region;

    return self.lowerStmt(root_body);
}

fn lowerStmt(self: *Self, stmt_id: MIR.CFStmtId) Allocator.Error!CFStmtId {
    const prev_stmt = self.current_stmt;
    self.current_stmt = stmt_id;
    defer self.current_stmt = prev_stmt;

    const stmt = self.mir_store.getCFStmt(stmt_id);
    return switch (stmt) {
        .assign_symbol => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            defer self.popMirTargetBinding(target_binding);
            break :blk try self.lowerMirSymbolValue(
                assign.target,
                target_binding.target,
                assign.symbol,
                lowered_next,
            );
        },
        .assign_ref => |assign| blk: {
            if (assign.op == .local) {
                const source = assign.op.local;
                const lowered_source = try self.mapMirLocal(source);
                const source_layout = self.lir_store.getLocal(lowered_source).layout_idx;
                const target_binding = try self.pushMirTargetBindingWithLayout(assign.target, source_layout);
                const lowered_next = try self.lowerStmt(assign.next);
                defer self.popMirTargetBinding(target_binding);
                break :blk try self.emitAssignRef(
                    target_binding.target,
                    aliasSemantics(lowered_source, LIR.RefProjectionSpan.empty()),
                    .{ .local = lowered_source },
                    lowered_next,
                );
            }

            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            defer self.popMirTargetBinding(target_binding);
            break :blk switch (assign.op) {
                .local => unreachable,
                .discriminant => |discriminant| try self.emitAssignRef(
                    target_binding.target,
                    .fresh,
                    .{ .discriminant = .{ .source = try self.mapMirLocal(discriminant.source) } },
                    lowered_next,
                ),
                .field => |field| blk_field: {
                    const lowered_source = try self.mapMirLocal(field.source);
                    break :blk_field try self.emitAssignRef(
                        target_binding.target,
                        try self.projectedResultSemantics(
                            field.source,
                            try self.singleProjectionSpan(.{ .field = @intCast(field.field_idx) }),
                            field.ownership,
                        ),
                        .{ .field = .{
                            .source = lowered_source,
                            .field_idx = @intCast(field.field_idx),
                        } },
                        lowered_next,
                    );
                },
                .tag_payload => |payload| try self.emitAssignRef(
                    target_binding.target,
                    try self.projectedResultSemantics(
                        payload.source,
                        try self.singleProjectionSpan(.{ .tag_payload = @intCast(payload.payload_idx) }),
                        payload.ownership,
                    ),
                    .{ .tag_payload = .{
                        .source = try self.mapMirLocal(payload.source),
                        .payload_idx = @intCast(payload.payload_idx),
                        .tag_discriminant = @intCast(payload.tag_discriminant),
                    } },
                    lowered_next,
                ),
                .nominal => |nominal| try self.emitAssignRef(
                    target_binding.target,
                    aliasSemantics(
                        try self.mapMirLocal(nominal.backing),
                        try self.singleProjectionSpan(.nominal),
                    ),
                    .{ .nominal = .{
                        .backing_ref = try self.mapMirLocal(nominal.backing),
                    } },
                    lowered_next,
                ),
            };
        },
        .assign_literal => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            defer self.popMirTargetBinding(target_binding);
            break :blk try self.emitAssignLiteral(
                target_binding.target,
                self.mirLiteralValue(
                    try self.runtimeValueLayoutFromMirLocal(assign.target),
                    assign.literal,
                ),
                lowered_next,
            );
        },
        .assign_lambda => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            defer self.popMirTargetBinding(target_binding);
            break :blk try self.emitAssignUnit(
                target_binding.target,
                lowered_next,
            );
        },
        .assign_closure => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            defer self.popMirTargetBinding(target_binding);
            const lowered_captures = try self.mapMirLocalSpan(assign.captures);
            break :blk try self.lir_store.addCFStmt(.{ .assign_struct = .{
                .target = target_binding.target,
                .result = .fresh,
                .fields = lowered_captures,
                .next = lowered_next,
            } });
        },
        .assign_call => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            defer self.popMirTargetBinding(target_binding);
            break :blk try self.lowerDirectLambdaCall(
                assign.callee,
                assign.exact_lambda,
                assign.exact_requires_hidden_capture,
                self.mir_store.getLocalSpan(assign.args),
                assign.target,
                target_binding.target,
                lowered_next,
            );
        },
        .assign_low_level => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            defer self.popMirTargetBinding(target_binding);
            const mir_args = self.mir_store.getLocalSpan(assign.args);
            const lir_args = try self.allocator.alloc(LirLocalId, mir_args.len);
            defer self.allocator.free(lir_args);
            for (mir_args, 0..) |mir_arg, i| {
                lir_args[i] = try self.mapMirLocal(mir_arg);
            }
            break :blk try self.lir_store.addCFStmt(.{ .assign_low_level = .{
                .target = target_binding.target,
                .result = lowLevelResultSemantics(self, self.current_borrow_region, assign.op, lir_args),
                .op = assign.op,
                .args = try self.lir_store.addLocalSpan(lir_args),
                .next = lowered_next,
            } });
        },
        .assign_list => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            defer self.popMirTargetBinding(target_binding);
            break :blk try self.lir_store.addCFStmt(.{ .assign_list = .{
                .target = target_binding.target,
                .result = .fresh,
                .elems = try self.mapMirLocalSpan(assign.elems),
                .next = lowered_next,
            } });
        },
        .assign_struct => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            defer self.popMirTargetBinding(target_binding);
            break :blk try self.lir_store.addCFStmt(.{ .assign_struct = .{
                .target = target_binding.target,
                .result = .fresh,
                .fields = try self.mapMirLocalSpan(assign.fields),
                .next = lowered_next,
            } });
        },
        .assign_tag => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            defer self.popMirTargetBinding(target_binding);
            break :blk try self.lir_store.addCFStmt(.{ .assign_tag = .{
                .target = target_binding.target,
                .result = .fresh,
                .discriminant = self.tagDiscriminantForMonotypeName(
                    self.mir_store.getLocal(assign.target).monotype,
                    assign.name,
                ),
                .args = try self.mapMirLocalSpan(assign.args),
                .next = lowered_next,
            } });
        },
        .debug => |debug_stmt| try self.lir_store.addCFStmt(.{ .debug = .{
            .message = try self.mapMirLocal(debug_stmt.value),
            .next = try self.lowerStmt(debug_stmt.next),
        } }),
        .expect => |expect_stmt| try self.lir_store.addCFStmt(.{ .expect = .{
            .condition = try self.mapMirLocal(expect_stmt.condition),
            .next = try self.lowerStmt(expect_stmt.next),
        } }),
        .runtime_error => try self.lir_store.addCFStmt(.{ .runtime_error = {} }),
        .switch_stmt => |switch_stmt| try self.lowerSwitchStmt(switch_stmt),
        .borrow_scope => |scope| try self.lowerBorrowScopeStmt(scope),
        .scope_exit => |scope_exit| try self.lir_store.addCFStmt(.{ .scope_exit = .{
            .id = translateBorrowScopeId(scope_exit.id),
        } }),
        .join => |join_stmt| try self.lowerJoinStmt(join_stmt),
        .jump => |jump_stmt| blk: {
            const lir_args = try self.mapMirLocalSpan(jump_stmt.args);
            break :blk try self.lir_store.addCFStmt(.{ .jump = .{
                .target = translateJoinPointId(jump_stmt.id),
                .args = lir_args,
            } });
        },
        .ret => |ret_stmt| try self.emitRet(try self.mapMirLocal(ret_stmt.value)),
        .crash => |msg| try self.lir_store.addCFStmt(.{ .crash = .{
            .msg = try self.internMirStringLiteral(msg),
        } }),
    };
}

fn lowerBorrowScopeStmt(
    self: *Self,
    scope: std.meta.TagPayload(MIR.CFStmt, .borrow_scope),
) Allocator.Error!CFStmtId {
    const prev_region = self.current_borrow_region;
    self.current_borrow_region = .{ .scope = translateBorrowScopeId(scope.id) };
    defer self.current_borrow_region = prev_region;

    return self.lir_store.addCFStmt(.{ .borrow_scope = .{
        .id = translateBorrowScopeId(scope.id),
        .body = try self.lowerStmt(scope.body),
        .remainder = try self.lowerStmt(scope.remainder),
    } });
}

fn lowerJoinStmt(
    self: *Self,
    join_stmt: std.meta.TagPayload(MIR.CFStmt, .join),
) Allocator.Error!CFStmtId {
    const join_params = self.mir_store.getLocalSpan(join_stmt.params);
    const join_param_locals = try self.allocator.alloc(LirLocalId, join_params.len);
    defer self.allocator.free(join_param_locals);
    const lowered_remainder = try self.lowerStmt(join_stmt.remainder);
    var join_override_info = try self.buildJoinOverrideInfo(join_stmt);
    defer join_override_info.deinit(self.allocator);

    const lowered_body = blk: {
        const prev_origin_overrides = self.current_origin_overrides;
        self.current_origin_overrides = &join_override_info.origins;
        try self.pushLocalOverrideScope();
        defer self.current_origin_overrides = prev_origin_overrides;
        defer self.popLocalOverrideScope();
        for (join_params, 0..) |param_local, i| {
            const key = @as(u32, @intFromEnum(param_local));
            join_param_locals[i] = try self.lir_store.addLocal(.{
                .layout_idx = join_override_info.layouts[i],
            });
            try self.bindMirLocal(key, join_param_locals[i]);
        }

        break :blk try self.lowerStmt(join_stmt.body);
    };

    return self.lir_store.addCFStmt(.{ .join = .{
        .id = translateJoinPointId(join_stmt.id),
        .params = try self.lir_store.addLocalSpan(join_param_locals),
        .body = lowered_body,
        .remainder = lowered_remainder,
    } });
}

fn lowerDirectLambdaCall(
    self: *Self,
    callee_mir: MIR.LocalId,
    exact_lambda: MIR.LambdaId,
    exact_requires_hidden_capture: bool,
    visible_args: []const MIR.LocalId,
    target: MIR.LocalId,
    lowered_target: LirLocalId,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const resolved = ResolvedCallable{
        .lambda = exact_lambda,
        .captures_local = if (exact_requires_hidden_capture) callee_mir else null,
    };
    const lambda = self.mir_store.getLambda(resolved.lambda);
    if (resolved.captures_local == null and lambda.captures_param != null) {
        std.debug.panic(
            "MirToLir invariant violated: callable local {d} resolved to a captured lambda without a closure value",
            .{@intFromEnum(callee_mir)},
        );
    }
    if (resolved.captures_local != null and lambda.captures_param == null) {
        std.debug.panic(
            "MirToLir invariant violated: callable local {d} resolved to a closure value for a lambda without captures",
            .{@intFromEnum(callee_mir)},
        );
    }
    const lowered_proc = try self.lowerLambda(resolved.lambda);
    const callee_ret_layout = self.loweredProcRetLayout(lowered_proc);
    const target_layout = self.lir_store.getLocal(lowered_target).layout_idx;
    if (callee_ret_layout != target_layout) {
        const target_mono = self.mir_store.getLocal(target).monotype;
        const lambda_ret_mono = lambda.ret_monotype;
        std.debug.panic(
            "MirToLir invariant violated: direct call result layout mismatch target_local={d} target_mono={d} target_mono_value={any} target_layout={d} callee_local={d} lambda={d} lambda_ret_mono={d} lambda_ret_mono_value={any} callee_ret_layout={d}",
            .{
                @intFromEnum(target),
                @intFromEnum(target_mono),
                self.mir_store.monotype_store.getMonotype(target_mono),
                @intFromEnum(target_layout),
                @intFromEnum(callee_mir),
                @intFromEnum(resolved.lambda),
                @intFromEnum(lambda_ret_mono),
                self.mir_store.monotype_store.getMonotype(lambda_ret_mono),
                @intFromEnum(callee_ret_layout),
            },
        );
    }
    const call_arg_count = visible_args.len + @intFromBool(resolved.captures_local != null);
    const call_args = try self.allocator.alloc(LirLocalId, call_arg_count);
    defer self.allocator.free(call_args);

    for (visible_args, 0..) |mir_arg, i| {
        call_args[i] = try self.mapMirLocal(mir_arg);
    }
    if (resolved.captures_local) |capture_local| {
        call_args[visible_args.len] = try self.mapMirLocal(capture_local);
    }
    return self.lir_store.addCFStmt(.{ .assign_call = .{
        .target = lowered_target,
        .result = try self.directLambdaCallResultSemantics(resolved, visible_args),
        .proc = lowered_proc,
        .args = try self.lir_store.addLocalSpan(call_args),
        .next = next,
    } });
}

fn loweredProcRetLayout(self: *const Self, proc_id: LirProcSpecId) layout.Idx {
    return switch (self.builder_procs.items[@intFromEnum(proc_id)]) {
        .resolved => |proc| proc.ret_layout,
        .unresolved => |header| header.ret_layout,
    };
}

fn lowerEntrypointCallResultContract(
    self: *Self,
    proc_id: LirProcSpecId,
    visible_arg_count: usize,
) LIR.ProcResultContract {
    return switch (self.getResultContract(proc_id)) {
        .no_return => .no_return,
        .fresh => .fresh,
        .alias_of_param => |param_ref| {
            if (param_ref.param_index >= visible_arg_count) {
                std.debug.panic(
                    "MirToLir invariant violated: entrypoint proc result cannot alias hidden closure captures",
                    .{},
                );
            }
            return .{ .alias_of_param = param_ref };
        },
        .borrow_of_param => |param_ref| {
            if (param_ref.param_index >= visible_arg_count) {
                std.debug.panic(
                    "MirToLir invariant violated: entrypoint proc result cannot borrow hidden closure captures",
                    .{},
                );
            }
            return .{ .borrow_of_param = param_ref };
        },
    };
}

fn emitEntrypointCall(
    self: *Self,
    callable: ResolvedCallable,
    visible_args: []const LirLocalId,
    target: LirLocalId,
    next: CFStmtId,
) Allocator.Error!LoweredEntrypointProcBody {
    const proc_id = try self.lowerLambda(callable.lambda);
    const callee_ret_layout = self.loweredProcRetLayout(proc_id);
    const target_layout = self.lir_store.getLocal(target).layout_idx;
    if (callee_ret_layout != target_layout) {
        std.debug.panic(
            "MirToLir invariant violated: entrypoint wrapper result layout does not match callee result layout",
            .{},
        );
    }

    const call_arg_count = visible_args.len + @intFromBool(callable.captures_local != null);
    const call_args = try self.allocator.alloc(LirLocalId, call_arg_count);
    defer self.allocator.free(call_args);

    @memcpy(call_args[0..visible_args.len], visible_args);
    if (callable.captures_local) |capture_local| {
        call_args[visible_args.len] = try self.mapMirLocal(capture_local);
    }

    return .{
        .body = try self.lir_store.addCFStmt(.{ .assign_call = .{
            .target = target,
            .result = self.callResultSemantics(proc_id, call_args),
            .proc = proc_id,
            .args = try self.lir_store.addLocalSpan(call_args),
            .next = next,
        } }),
        .result_contract = self.lowerEntrypointCallResultContract(proc_id, visible_args.len),
    };
}

const LoweredEntrypointProcBody = struct {
    body: CFStmtId,
    result_contract: LIR.ProcResultContract,
};

fn mergeEntrypointResultContracts(
    left: LIR.ProcResultContract,
    right: LIR.ProcResultContract,
) LIR.ProcResultContract {
    if (left == .no_return) return right;
    if (right == .no_return) return left;
    if (std.meta.eql(left, right)) return left;
    return .fresh;
}

fn lowerEntrypointCallableStmt(
    self: *Self,
    stmt_id: MIR.CFStmtId,
    arg_locals: []const LirLocalId,
    target: LirLocalId,
    next: CFStmtId,
) Allocator.Error!LoweredEntrypointProcBody {
    const stmt = self.mir_store.getCFStmt(stmt_id);
    return switch (stmt) {
        .assign_symbol => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            defer self.popMirTargetBinding(target_binding);
            break :blk .{
                .body = try self.lowerMirSymbolValue(assign.target, target_binding.target, assign.symbol, lowered_next.body),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_ref => |assign| blk: {
            if (assign.op == .local) {
                const source = assign.op.local;
                const lowered_source = try self.mapMirLocal(source);
                const source_layout = self.lir_store.getLocal(lowered_source).layout_idx;
                const target_binding = try self.pushMirTargetBindingWithLayout(assign.target, source_layout);
                const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
                defer self.popMirTargetBinding(target_binding);
                break :blk .{
                    .body = try self.emitAssignRef(
                        target_binding.target,
                        aliasSemantics(lowered_source, LIR.RefProjectionSpan.empty()),
                        .{ .local = lowered_source },
                        lowered_next.body,
                    ),
                    .result_contract = lowered_next.result_contract,
                };
            }

            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            defer self.popMirTargetBinding(target_binding);
            break :blk .{
                .body = switch (assign.op) {
                    .local => unreachable,
                    .discriminant => |discriminant| try self.emitAssignRef(
                        target_binding.target,
                        .fresh,
                        .{ .discriminant = .{ .source = try self.mapMirLocal(discriminant.source) } },
                        lowered_next.body,
                    ),
                    .field => |field| try self.emitAssignRef(
                        target_binding.target,
                        try self.projectedResultSemantics(
                            field.source,
                            try self.singleProjectionSpan(.{ .field = @intCast(field.field_idx) }),
                            field.ownership,
                        ),
                        .{ .field = .{
                            .source = try self.mapMirLocal(field.source),
                            .field_idx = @intCast(field.field_idx),
                        } },
                        lowered_next.body,
                    ),
                    .tag_payload => |payload| try self.emitAssignRef(
                        target_binding.target,
                        try self.projectedResultSemantics(
                            payload.source,
                            try self.singleProjectionSpan(.{ .tag_payload = @intCast(payload.payload_idx) }),
                            payload.ownership,
                        ),
                        .{ .tag_payload = .{
                            .source = try self.mapMirLocal(payload.source),
                            .payload_idx = @intCast(payload.payload_idx),
                            .tag_discriminant = @intCast(payload.tag_discriminant),
                        } },
                        lowered_next.body,
                    ),
                    .nominal => |nominal| try self.emitAssignRef(
                        target_binding.target,
                        aliasSemantics(
                            try self.mapMirLocal(nominal.backing),
                            try self.singleProjectionSpan(.nominal),
                        ),
                        .{ .nominal = .{
                            .backing_ref = try self.mapMirLocal(nominal.backing),
                        } },
                        lowered_next.body,
                    ),
                },
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_literal => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            defer self.popMirTargetBinding(target_binding);
            break :blk .{
                .body = try self.emitAssignLiteral(
                    target_binding.target,
                    self.mirLiteralValue(
                        try self.runtimeValueLayoutFromMirLocal(assign.target),
                        assign.literal,
                    ),
                    lowered_next.body,
                ),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_lambda => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            defer self.popMirTargetBinding(target_binding);
            break :blk .{
                .body = try self.emitAssignUnit(target_binding.target, lowered_next.body),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_closure => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            defer self.popMirTargetBinding(target_binding);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .assign_struct = .{
                    .target = target_binding.target,
                    .result = .fresh,
                    .fields = try self.mapMirLocalSpan(assign.captures),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_call => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            defer self.popMirTargetBinding(target_binding);
            break :blk .{
                .body = try self.lowerDirectLambdaCall(
                    assign.callee,
                    assign.exact_lambda,
                    assign.exact_requires_hidden_capture,
                    self.mir_store.getLocalSpan(assign.args),
                    assign.target,
                    target_binding.target,
                    lowered_next.body,
                ),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_low_level => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            defer self.popMirTargetBinding(target_binding);
            const mir_args = self.mir_store.getLocalSpan(assign.args);
            const lir_args = try self.allocator.alloc(LirLocalId, mir_args.len);
            defer self.allocator.free(lir_args);
            for (mir_args, 0..) |mir_arg, i| {
                lir_args[i] = try self.mapMirLocal(mir_arg);
            }
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .assign_low_level = .{
                    .target = target_binding.target,
                    .result = lowLevelResultSemantics(self, self.current_borrow_region, assign.op, lir_args),
                    .op = assign.op,
                    .args = try self.lir_store.addLocalSpan(lir_args),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_list => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            defer self.popMirTargetBinding(target_binding);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .assign_list = .{
                    .target = target_binding.target,
                    .result = .fresh,
                    .elems = try self.mapMirLocalSpan(assign.elems),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_struct => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            defer self.popMirTargetBinding(target_binding);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .assign_struct = .{
                    .target = target_binding.target,
                    .result = .fresh,
                    .fields = try self.mapMirLocalSpan(assign.fields),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_tag => |assign| blk: {
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            defer self.popMirTargetBinding(target_binding);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .assign_tag = .{
                    .target = target_binding.target,
                    .result = .fresh,
                    .discriminant = self.tagDiscriminantForMonotypeName(
                        self.mir_store.getLocal(assign.target).monotype,
                        assign.name,
                    ),
                    .args = try self.mapMirLocalSpan(assign.args),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .debug => |debug_stmt| blk: {
            const lowered_next = try self.lowerEntrypointCallableStmt(debug_stmt.next, arg_locals, target, next);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .debug = .{
                    .message = try self.mapMirLocal(debug_stmt.value),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .expect => |expect_stmt| blk: {
            const lowered_next = try self.lowerEntrypointCallableStmt(expect_stmt.next, arg_locals, target, next);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .expect = .{
                    .condition = try self.mapMirLocal(expect_stmt.condition),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .runtime_error => .{
            .body = try self.lir_store.addCFStmt(.{ .runtime_error = {} }),
            .result_contract = .no_return,
        },
        .switch_stmt => |switch_stmt| blk: {
            const branches = self.mir_store.getSwitchBranches(switch_stmt.branches);
            const lowered_branches = try self.allocator.alloc(LIR.CFSwitchBranch, branches.len);
            defer self.allocator.free(lowered_branches);

            var merged_contract: LIR.ProcResultContract = .no_return;
            for (branches, 0..) |branch, i| {
                const lowered_branch = blk_branch: {
                    try self.pushLocalOverrideScope();
                    defer self.popLocalOverrideScope();
                    break :blk_branch try self.lowerEntrypointCallableStmt(branch.body, arg_locals, target, next);
                };
                lowered_branches[i] = .{
                    .value = branch.value,
                    .body = lowered_branch.body,
                };
                merged_contract = mergeEntrypointResultContracts(merged_contract, lowered_branch.result_contract);
            }

            const lowered_default = blk_default: {
                try self.pushLocalOverrideScope();
                defer self.popLocalOverrideScope();
                break :blk_default try self.lowerEntrypointCallableStmt(switch_stmt.default_branch, arg_locals, target, next);
            };
            merged_contract = mergeEntrypointResultContracts(merged_contract, lowered_default.result_contract);

            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .switch_stmt = .{
                    .cond = try self.mapMirLocal(switch_stmt.scrutinee),
                    .branches = try self.lir_store.addCFSwitchBranches(lowered_branches),
                    .default_branch = lowered_default.body,
                } }),
                .result_contract = merged_contract,
            };
        },
        .borrow_scope => |scope| blk: {
            const prev_region = self.current_borrow_region;
            self.current_borrow_region = .{ .scope = translateBorrowScopeId(scope.id) };
            const lowered_body = try self.lowerEntrypointCallableStmt(scope.body, arg_locals, target, next);
            self.current_borrow_region = prev_region;

            const lowered_remainder = try self.lowerEntrypointCallableStmt(scope.remainder, arg_locals, target, next);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .borrow_scope = .{
                    .id = translateBorrowScopeId(scope.id),
                    .body = lowered_body.body,
                    .remainder = lowered_remainder.body,
                } }),
                .result_contract = mergeEntrypointResultContracts(
                    lowered_body.result_contract,
                    lowered_remainder.result_contract,
                ),
            };
        },
        .scope_exit => |scope_exit| .{
            .body = try self.lir_store.addCFStmt(.{ .scope_exit = .{
                .id = translateBorrowScopeId(scope_exit.id),
            } }),
            .result_contract = .no_return,
        },
        .join => |join_stmt| blk: {
            const join_params = self.mir_store.getLocalSpan(join_stmt.params);
            const join_param_locals = try self.allocator.alloc(LirLocalId, join_params.len);
            defer self.allocator.free(join_param_locals);
            const lowered_remainder = try self.lowerEntrypointCallableStmt(join_stmt.remainder, arg_locals, target, next);
            var join_override_info = try self.buildJoinOverrideInfo(join_stmt);
            defer join_override_info.deinit(self.allocator);

            const lowered_body = blk_body: {
                const prev_origin_overrides = self.current_origin_overrides;
                self.current_origin_overrides = &join_override_info.origins;
                try self.pushLocalOverrideScope();
                defer self.current_origin_overrides = prev_origin_overrides;
                defer self.popLocalOverrideScope();
                for (join_params, 0..) |param_local, i| {
                    const key = @as(u32, @intFromEnum(param_local));
                    join_param_locals[i] = try self.lir_store.addLocal(.{
                        .layout_idx = join_override_info.layouts[i],
                    });
                    try self.bindMirLocal(key, join_param_locals[i]);
                }

                break :blk_body try self.lowerEntrypointCallableStmt(join_stmt.body, arg_locals, target, next);
            };
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .join = .{
                    .id = translateJoinPointId(join_stmt.id),
                    .params = try self.lir_store.addLocalSpan(join_param_locals),
                    .body = lowered_body.body,
                    .remainder = lowered_remainder.body,
                } }),
                .result_contract = mergeEntrypointResultContracts(
                    lowered_body.result_contract,
                    lowered_remainder.result_contract,
                ),
            };
        },
        .jump => |jump_stmt| .{
            .body = try self.lir_store.addCFStmt(.{ .jump = .{
                .target = translateJoinPointId(jump_stmt.id),
                .args = try self.mapMirLocalSpan(jump_stmt.args),
            } }),
            .result_contract = .no_return,
        },
        .ret => |ret_stmt| try self.emitEntrypointCall(
            self.requireExactCallableValue(ret_stmt.value),
            arg_locals,
            target,
            next,
        ),
        .crash => |msg| .{
            .body = try self.lir_store.addCFStmt(.{ .crash = .{
                .msg = try self.internMirStringLiteral(msg),
            } }),
            .result_contract = .no_return,
        },
    };
}

fn lowerEntrypointProcBody(
    self: *Self,
    root_const_id: MIR.ConstDefId,
    arg_locals: []const LirLocalId,
    ret_layout: layout.Idx,
) Allocator.Error!LoweredEntrypointProcBody {
    const root_const = self.mir_store.getConstDef(root_const_id);
    const must_call = arg_locals.len != 0 or self.analyses.getConstCallableContract(root_const_id) != .no_return;

    if (!must_call) {
        const result_contract = try self.lowerSummaryResultContract(
            self.analyses.getConstResultContract(root_const_id),
        );
        return .{
            .body = try self.lowerRootBody(root_const.body),
            .result_contract = result_contract,
        };
    }

    const target = try self.freshLocal(ret_layout);
    const ret_stmt = try self.emitRet(target);

    var local_map = MirToLirLocalMap.init(self.allocator);
    defer local_map.deinit();
    const prev_local_map = self.current_local_map;
    const prev_origin_overrides = self.current_origin_overrides;
    const prev_lambda = self.current_lambda;
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_origin_overrides = null;
    self.current_lambda = null;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_origin_overrides = prev_origin_overrides;
    defer self.current_lambda = prev_lambda;
    defer self.current_borrow_region = prev_region;

    return try self.lowerEntrypointCallableStmt(root_const.body, arg_locals, target, ret_stmt);
}

fn lowerLambda(self: *Self, lambda_id: MIR.LambdaId) Allocator.Error!LirProcSpecId {
    const proc_key = @as(u32, @intFromEnum(lambda_id));
    if (self.lowered_lambdas.get(proc_key)) |existing| return existing;

    const lambda = self.mir_store.getLambda(lambda_id);
    const ret_layout = try self.runtimeLambdaReturnLayout(lambda_id);
    const value_params = self.mir_store.getLocalSpan(lambda.params);
    const arg_count = value_params.len + @intFromBool(lambda.captures_param != null);
    const args = try self.allocator.alloc(LirLocalId, arg_count);
    defer self.allocator.free(args);

    const prev_lambda = self.current_lambda;
    self.current_lambda = lambda_id;
    defer self.current_lambda = prev_lambda;

    for (value_params, 0..) |param_local, i| {
        args[i] = try self.freshLocal(try self.runtimeValueLayoutFromMirLocal(param_local));
    }
    if (lambda.captures_param) |captures_param| {
        args[value_params.len] = try self.freshLocal(
            try self.runtimeValueLayoutFromMirMonotype(self.mir_store.getLocal(captures_param).monotype),
        );
    }

    const arg_span = try self.lir_store.addLocalSpan(args);
    const result_contract = try self.lowerSummaryResultContract(
        self.analyses.getLambdaResultContract(lambda_id),
    );
    const proc_name = if (lambda.debug_name.isNone())
        self.lir_store.freshSyntheticSymbol()
    else
        lambda.debug_name;

    const lir_proc_id = try self.addUnresolvedProc(.{
        .name = proc_name,
        .args = arg_span,
        .ret_layout = ret_layout,
        .result_contract = result_contract,
        .hosted = if (lambda.hosted) |hosted| .{
            .symbol_name = hosted.symbol_name,
            .index = hosted.index,
        } else null,
    });
    try self.lowered_lambdas.put(proc_key, lir_proc_id);

    var local_map = MirToLirLocalMap.init(self.allocator);
    defer local_map.deinit();
    const prev_local_map = self.current_local_map;
    const prev_origin_overrides = self.current_origin_overrides;
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_origin_overrides = null;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_origin_overrides = prev_origin_overrides;
    defer self.current_borrow_region = prev_region;

    for (value_params, 0..) |param_local, i| {
        try local_map.put(@as(u32, @intFromEnum(param_local)), args[i]);
    }
    if (lambda.captures_param) |captures_param| {
        try local_map.put(@as(u32, @intFromEnum(captures_param)), args[value_params.len]);
    }

    const body = try self.lowerStmt(lambda.body);

    if (builtin.mode == .Debug) {
        // This verifier exists only to catch compiler implementation bugs by
        // re-scanning already-lowered LIR. It must remain debug-only because
        // release compiler builds must not pay for extra full-LIR verification.
        try DebugVerifyLir.verifyProc(
            self.allocator,
            self.lir_store,
            self.layout_store,
            "lambda",
            @intFromEnum(lambda_id),
            ret_layout,
            arg_span,
            result_contract,
            body,
        );
    }

    self.resolveProc(lir_proc_id, .{
        .name = proc_name,
        .args = arg_span,
        .body = body,
        .ret_layout = ret_layout,
        .result_contract = result_contract,
        .hosted = if (lambda.hosted) |hosted| .{
            .symbol_name = hosted.symbol_name,
            .index = hosted.index,
        } else null,
    });
    return lir_proc_id;
}
