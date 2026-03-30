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
const layout = @import("layout");
const mir_mod = @import("mir");

const MIR = mir_mod.MIR;
const Analyses = mir_mod.Analyses;
const ResultSummary = mir_mod.ResultSummary;

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");
const DebugVerifyLir = @import("DebugVerifyLir.zig");

const Allocator = std.mem.Allocator;
const MirMonotypeIdx = mir_mod.Monotype.Idx;

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
const MirToLirStmtMap = std.AutoHashMap(u32, CFStmtId);
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
    previous: ?LirLocalId,
    installed_in_override_scope: bool,
};
const CallableResolution = struct {
    lambda: MIR.LambdaId,
    requires_hidden_capture: bool,
};
const MirCallableResolutionMap = std.AutoHashMap(u32, CallableResolution);

const JoinCallableState = struct {
    params: []const MIR.LocalId,
    merged_callables: []?CallableResolution,
    saw_incoming: bool,
};

const ActiveJoinCallableMap = std.AutoHashMap(u32, JoinCallableState);

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
current_stmt_map: ?*MirToLirStmtMap = null,
current_callable_overrides: ?*MirCallableResolutionMap = null,
current_lambda: ?MIR.LambdaId = null,
current_borrow_region: LIR.BorrowRegion = .proc,
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

fn runtimeCallableValueLayoutFromMirLocal(self: *Self, local_id: MIR.LocalId) Allocator.Error!?layout.Idx {
    const resolved = self.resolveCallableValue(local_id) catch |err| switch (err) {
        error.OutOfMemory => return err,
    };
    return try self.runtimeLambdaValueLayout(resolved.lambda);
}

fn runtimeValueLayoutFromMirLocal(self: *Self, local_id: MIR.LocalId) Allocator.Error!layout.Idx {
    const local = self.mir_store.getLocal(local_id);
    if (self.mir_store.monotype_store.getMonotype(local.monotype) == .func) {
        if (try self.runtimeCallableValueLayoutFromMirLocal(local_id)) |layout_idx| {
            return layout_idx;
        }
        std.debug.panic(
            "MirToLir invariant violated: function-valued local {d} has no exact callable layout source",
            .{@intFromEnum(local_id)},
        );
    }
    return self.runtimeValueLayoutFromMirMonotype(local.monotype);
}

fn runtimeLambdaValueLayout(self: *Self, lambda_id: MIR.LambdaId) Allocator.Error!layout.Idx {
    const lambda = self.mir_store.getLambda(lambda_id);
    if (lambda.captures_param) |captures_param| {
        return self.runtimeValueLayoutFromMirLocal(captures_param);
    }
    return self.runtimeRepresentationLayoutIdx(try self.mir_layout_resolver.resolve(lambda.fn_monotype, null));
}

fn runtimeLambdaReturnLayout(self: *Self, lambda_id: MIR.LambdaId) Allocator.Error!layout.Idx {
    const lambda = self.mir_store.getLambda(lambda_id);
    if (self.mir_store.monotype_store.getMonotype(lambda.ret_monotype) != .func) {
        return self.runtimeValueLayoutFromMirMonotype(lambda.ret_monotype);
    }
    return switch (self.analyses.getLambdaCallableContract(lambda_id)) {
        .no_return => std.debug.panic(
            "MirToLir invariant violated: function-returning lambda {d} has no reachable exact callable return contract",
            .{@intFromEnum(lambda_id)},
        ),
        .exact_lambda => |returned_lambda| self.runtimeLambdaValueLayout(returned_lambda),
        .exact_closure => |returned_lambda| self.runtimeLambdaValueLayout(returned_lambda),
    };
}

fn monotypeMayContainCallable(self: *const Self, mono_idx: MirMonotypeIdx) bool {
    return switch (self.mir_store.monotype_store.getMonotype(mono_idx)) {
        .func => true,
        .record => |record| blk: {
            for (self.mir_store.monotype_store.getFields(record.fields)) |field| {
                if (self.monotypeMayContainCallable(field.type_idx)) break :blk true;
            }
            break :blk false;
        },
        .tuple => |tuple_data| blk: {
            for (self.mir_store.monotype_store.getIdxSpan(tuple_data.elems)) |elem| {
                if (self.monotypeMayContainCallable(elem)) break :blk true;
            }
            break :blk false;
        },
        .tag_union => |tag_union| blk: {
            for (self.mir_store.monotype_store.getTags(tag_union.tags)) |tag| {
                for (self.mir_store.monotype_store.getIdxSpan(tag.payloads)) |payload| {
                    if (self.monotypeMayContainCallable(payload)) break :blk true;
                }
            }
            break :blk false;
        },
        .box => |box_data| self.monotypeMayContainCallable(box_data.inner),
        .list,
        .prim,
        .unit,
        => false,
        .recursive_placeholder => std.debug.panic(
            "MirToLir invariant violated: recursive_placeholder survived monotype construction",
            .{},
        ),
    };
}

fn localMayContainCallable(self: *const Self, local_id: MIR.LocalId) bool {
    return self.monotypeMayContainCallable(self.mir_store.getLocal(local_id).monotype);
}

fn callableResolutionsEqual(left: CallableResolution, right: CallableResolution) bool {
    return left.lambda == right.lambda and left.requires_hidden_capture == right.requires_hidden_capture;
}

fn callableProjectionPathMatches(
    self: *const Self,
    binding_span: MIR.CallableProjectionSpan,
    reversed_path: []const MIR.CallableProjection,
) bool {
    const binding = self.mir_store.getCallableProjectionSpan(binding_span);
    if (binding.len != reversed_path.len) return false;

    var i: usize = 0;
    while (i < binding.len) : (i += 1) {
        const binding_proj = binding[i];
        const path_proj = reversed_path[reversed_path.len - 1 - i];
        switch (binding_proj) {
            .field => |binding_idx| switch (path_proj) {
                .field => |path_idx| if (binding_idx != path_idx) return false,
                else => return false,
            },
            .tag_payload => |binding_idx| switch (path_proj) {
                .tag_payload => |path_idx| if (binding_idx != path_idx) return false,
                else => return false,
            },
            .nominal => switch (path_proj) {
                .nominal => {},
                else => return false,
            },
        }
    }

    return true;
}

fn resolveCallableForParamProjection(
    self: *Self,
    source_param: MIR.LocalId,
    value_local: MIR.LocalId,
    reversed_path: []const MIR.CallableProjection,
) ResolvedCallable {
    const lambda_id = self.current_lambda orelse std.debug.panic(
        "MirToLir TODO: exact callable resolution for function-valued params outside lambda bodies is not implemented yet",
        .{},
    );
    const lambda = self.mir_store.getLambda(lambda_id);

    for (self.mir_store.getCallableBindings(lambda.callable_bindings)) |binding| {
        if (binding.source_param != source_param) continue;
        if (!self.callableProjectionPathMatches(binding.projections, reversed_path)) continue;
        return .{
            .lambda = binding.lambda,
            .captures_local = if (binding.requires_hidden_capture) value_local else null,
        };
    }

    std.debug.panic(
        "MirToLir TODO: missing exact callable binding for parameter local {d}",
        .{@intFromEnum(source_param)},
    );
}

fn lookupCallableOverride(self: *const Self, local_id: MIR.LocalId) ?CallableResolution {
    const overrides = self.current_callable_overrides orelse return null;
    return overrides.get(@as(u32, @intFromEnum(local_id)));
}

fn resolveCallableFromJoinParam(
    self: *Self,
    local_id: MIR.LocalId,
    value_local: MIR.LocalId,
    reversed_path: []const MIR.CallableProjection,
) ResolvedCallable {
    const resolved = self.lookupCallableOverride(local_id) orelse std.debug.panic(
        "MirToLir invariant violated: join-param callable resolution escaped active join overrides",
        .{},
    );
    if (reversed_path.len != 0) {
        std.debug.panic(
            "MirToLir TODO: exact callable resolution through projected join-param values is not implemented yet",
            .{},
        );
    }
    return .{
        .lambda = resolved.lambda,
        .captures_local = if (resolved.requires_hidden_capture) value_local else null,
    };
}

fn resolveCallableValuePath(
    self: *Self,
    local_id: MIR.LocalId,
    value_local: MIR.LocalId,
    reversed_path: *std.ArrayList(MIR.CallableProjection),
) Allocator.Error!ResolvedCallable {
    return switch (self.mir_store.getLocalDef(local_id)) {
        .param, .captures_param => self.resolveCallableForParamProjection(local_id, value_local, reversed_path.items),
        .join_param => self.resolveCallableFromJoinParam(local_id, value_local, reversed_path.items),
        .lambda => |lambda_id| {
            if (reversed_path.items.len != 0) {
                std.debug.panic(
                    "MirToLir invariant violated: callable projections cannot target a direct lambda value",
                    .{},
                );
            }
            return .{
                .lambda = lambda_id,
                .captures_local = null,
            };
        },
        .closure => |closure| {
            if (reversed_path.items.len != 0) {
                std.debug.panic(
                    "MirToLir invariant violated: callable projections cannot target a direct closure value",
                    .{},
                );
            }
            return .{
                .lambda = closure.lambda,
                .captures_local = value_local,
            };
        },
        .ref => |ref_op| switch (ref_op) {
            .local => |source| self.resolveCallableValuePath(source, value_local, reversed_path),
            .nominal => |nominal| blk: {
                try reversed_path.append(self.allocator, .nominal);
                defer _ = reversed_path.pop();
                break :blk try self.resolveCallableValuePath(nominal.backing, value_local, reversed_path);
            },
            .field => |field| blk: {
                switch (self.mir_store.getLocalDef(field.source)) {
                    .struct_ => |fields| {
                        const field_locals = self.mir_store.getLocalSpan(fields);
                        if (field.field_idx >= field_locals.len) {
                            std.debug.panic(
                                "MirToLir invariant violated: callable field index {d} exceeds struct arity {d}",
                                .{ field.field_idx, field_locals.len },
                            );
                        }
                        break :blk try self.resolveCallableValuePath(field_locals[field.field_idx], value_local, reversed_path);
                    },
                    else => {},
                }

                try reversed_path.append(self.allocator, .{ .field = field.field_idx });
                defer _ = reversed_path.pop();
                break :blk try self.resolveCallableValuePath(field.source, value_local, reversed_path);
            },
            .tag_payload => |payload| blk: {
                switch (self.mir_store.getLocalDef(payload.source)) {
                    .tag => |tag_value| {
                        const payload_locals = self.mir_store.getLocalSpan(tag_value.args);
                        if (payload.payload_idx >= payload_locals.len) {
                            std.debug.panic(
                                "MirToLir invariant violated: callable payload index {d} exceeds tag arity {d}",
                                .{ payload.payload_idx, payload_locals.len },
                            );
                        }
                        break :blk try self.resolveCallableValuePath(payload_locals[payload.payload_idx], value_local, reversed_path);
                    },
                    else => {},
                }

                try reversed_path.append(self.allocator, .{ .tag_payload = payload.payload_idx });
                defer _ = reversed_path.pop();
                break :blk try self.resolveCallableValuePath(payload.source, value_local, reversed_path);
            },
            .discriminant => std.debug.panic(
                "MirToLir invariant violated: callable resolution reached a discriminant ref local {d}",
                .{@intFromEnum(local_id)},
            ),
        },
        .symbol => |symbol| std.debug.panic(
            "MirToLir invariant violated: function-valued symbol {d} survived strongest-form MIR callable lowering",
            .{symbol.raw()},
        ),
        .call => |call_result| {
            if (reversed_path.items.len != 0) {
                std.debug.panic(
                    "MirToLir TODO: exact callable resolution through projected call results is not implemented yet",
                    .{},
                );
            }

            const callee = if (call_result.exact_lambda) |lambda_id|
                ResolvedCallable{
                    .lambda = lambda_id,
                    .captures_local = if (call_result.exact_requires_hidden_capture) call_result.callee else null,
                }
            else
                try self.resolveCallableValuePath(call_result.callee, call_result.callee, reversed_path);
            return switch (self.analyses.getLambdaCallableContract(callee.lambda)) {
                .no_return => std.debug.panic(
                    "MirToLir invariant violated: call-result callable resolution reached no-return lambda {d}",
                    .{@intFromEnum(callee.lambda)},
                ),
                .exact_lambda => |lambda_id| .{
                    .lambda = lambda_id,
                    .captures_local = null,
                },
                .exact_closure => |lambda_id| .{
                    .lambda = lambda_id,
                    .captures_local = value_local,
                },
            };
        },
        .struct_ => |fields| try self.resolveCallableFromStructValue(fields, value_local, reversed_path),
        .tag => |tag_value| try self.resolveCallableFromTagValue(tag_value.args, value_local, reversed_path),
        .literal,
        .low_level,
        .list,
        => std.debug.panic(
            "MirToLir invariant violated: callable resolution reached non-callable local def {s} for local {d}",
            .{ @tagName(self.mir_store.getLocalDef(local_id)), @intFromEnum(local_id) },
        ),
    };
}

fn resolveCallableFromStructValue(
    self: *Self,
    fields: MIR.LocalSpan,
    value_local: MIR.LocalId,
    reversed_path: *std.ArrayList(MIR.CallableProjection),
) Allocator.Error!ResolvedCallable {
    if (reversed_path.items.len == 0) {
        std.debug.panic(
            "MirToLir invariant violated: callable resolution reached a non-callable struct value",
            .{},
        );
    }

    const next_projection = reversed_path.items[reversed_path.items.len - 1];
    const field_idx = switch (next_projection) {
        .field => |idx| idx,
        else => std.debug.panic(
            "MirToLir invariant violated: callable struct resolution expected a field projection, found {s}",
            .{@tagName(next_projection)},
        ),
    };

    const field_locals = self.mir_store.getLocalSpan(fields);
    if (field_idx >= field_locals.len) {
        std.debug.panic(
            "MirToLir invariant violated: callable field index {d} exceeds struct arity {d}",
            .{ field_idx, field_locals.len },
        );
    }

    reversed_path.items.len -= 1;
    defer reversed_path.items.len += 1;
    return self.resolveCallableValuePath(field_locals[field_idx], value_local, reversed_path);
}

fn resolveCallableFromTagValue(
    self: *Self,
    args: MIR.LocalSpan,
    value_local: MIR.LocalId,
    reversed_path: *std.ArrayList(MIR.CallableProjection),
) Allocator.Error!ResolvedCallable {
    if (reversed_path.items.len == 0) {
        std.debug.panic(
            "MirToLir invariant violated: callable resolution reached a non-callable tag value",
            .{},
        );
    }

    const next_projection = reversed_path.items[reversed_path.items.len - 1];
    const payload_idx = switch (next_projection) {
        .tag_payload => |idx| idx,
        else => std.debug.panic(
            "MirToLir invariant violated: callable tag resolution expected a payload projection, found {s}",
            .{@tagName(next_projection)},
        ),
    };

    const payload_locals = self.mir_store.getLocalSpan(args);
    if (payload_idx >= payload_locals.len) {
        std.debug.panic(
            "MirToLir invariant violated: callable payload index {d} exceeds tag arity {d}",
            .{ payload_idx, payload_locals.len },
        );
    }

    reversed_path.items.len -= 1;
    defer reversed_path.items.len += 1;
    return self.resolveCallableValuePath(payload_locals[payload_idx], value_local, reversed_path);
}

fn resolveCallableValue(self: *Self, local_id: MIR.LocalId) Allocator.Error!ResolvedCallable {
    var reversed_path = std.ArrayList(MIR.CallableProjection).empty;
    defer reversed_path.deinit(self.allocator);
    return self.resolveCallableValuePath(local_id, local_id, &reversed_path);
}

fn collectJoinCallableResolutions(
    self: *Self,
    active_joins: *ActiveJoinCallableMap,
    stmt_id: MIR.CFStmtId,
) Allocator.Error!void {
    const stmt = self.mir_store.getCFStmt(stmt_id);
    switch (stmt) {
        .assign_symbol => |assign| try self.collectJoinCallableResolutions(active_joins, assign.next),
        .assign_ref => |assign| try self.collectJoinCallableResolutions(active_joins, assign.next),
        .assign_literal => |assign| try self.collectJoinCallableResolutions(active_joins, assign.next),
        .assign_lambda => |assign| try self.collectJoinCallableResolutions(active_joins, assign.next),
        .assign_closure => |assign| try self.collectJoinCallableResolutions(active_joins, assign.next),
        .assign_call => |assign| try self.collectJoinCallableResolutions(active_joins, assign.next),
        .assign_low_level => |assign| try self.collectJoinCallableResolutions(active_joins, assign.next),
        .assign_list => |assign| try self.collectJoinCallableResolutions(active_joins, assign.next),
        .assign_struct => |assign| try self.collectJoinCallableResolutions(active_joins, assign.next),
        .assign_tag => |assign| try self.collectJoinCallableResolutions(active_joins, assign.next),
        .debug => |debug_stmt| try self.collectJoinCallableResolutions(active_joins, debug_stmt.next),
        .expect => |expect_stmt| try self.collectJoinCallableResolutions(active_joins, expect_stmt.next),
        .runtime_error, .scope_exit, .ret, .crash => {},
        .switch_stmt => |switch_stmt| {
            for (self.mir_store.getSwitchBranches(switch_stmt.branches)) |branch| {
                try self.collectJoinCallableResolutions(active_joins, branch.body);
            }
            try self.collectJoinCallableResolutions(active_joins, switch_stmt.default_branch);
        },
        .borrow_scope => |scope_stmt| {
            try self.collectJoinCallableResolutions(active_joins, scope_stmt.body);
            try self.collectJoinCallableResolutions(active_joins, scope_stmt.remainder);
        },
        .join => |join_stmt| {
            const join_key = @intFromEnum(join_stmt.id);
            const join_params = self.mir_store.getLocalSpan(join_stmt.params);
            const merged_callables = try self.allocator.alloc(?CallableResolution, join_params.len);
            defer self.allocator.free(merged_callables);
            @memset(merged_callables, null);

            const gop = try active_joins.getOrPut(join_key);
            if (gop.found_existing) {
                std.debug.panic(
                    "MirToLir invariant violated: nested/duplicate active callable join {d}",
                    .{join_key},
                );
            }
            gop.value_ptr.* = .{
                .params = join_params,
                .merged_callables = merged_callables,
                .saw_incoming = false,
            };
            defer _ = active_joins.remove(join_key);

            try self.collectJoinCallableResolutions(active_joins, join_stmt.remainder);
            if (!gop.value_ptr.saw_incoming) return;

            var body_callable_overrides = MirCallableResolutionMap.init(self.allocator);
            defer body_callable_overrides.deinit();
            try body_callable_overrides.ensureTotalCapacity(@intCast(join_params.len));

            for (join_params, merged_callables, 0..) |param, incoming_callable, i| {
                if (!self.localMayContainCallable(param)) continue;
                body_callable_overrides.putAssumeCapacity(
                    @as(u32, @intFromEnum(param)),
                    incoming_callable orelse std.debug.panic(
                        "MirToLir invariant violated: join {d} param {d} had no incoming exact callable",
                        .{ join_key, i },
                    ),
                );
            }

            const saved_callable_overrides = self.current_callable_overrides;
            self.current_callable_overrides = &body_callable_overrides;
            defer self.current_callable_overrides = saved_callable_overrides;

            try self.collectJoinCallableResolutions(active_joins, join_stmt.body);
        },
        .jump => |jump| {
            const join_state = active_joins.getPtr(@intFromEnum(jump.id)) orelse std.debug.panic(
                "MirToLir invariant violated: jump to unknown active callable join {d}",
                .{@intFromEnum(jump.id)},
            );
            const args = self.mir_store.getLocalSpan(jump.args);
            if (args.len != join_state.params.len) {
                std.debug.panic(
                    "MirToLir invariant violated: jump to join {d} passed {d} args, expected {d}",
                    .{ @intFromEnum(jump.id), args.len, join_state.params.len },
                );
            }

            join_state.saw_incoming = true;
            for (args, 0..) |arg, i| {
                const param = join_state.params[i];
                if (!self.localMayContainCallable(param)) continue;

                var reversed_path = std.ArrayList(MIR.CallableProjection).empty;
                defer reversed_path.deinit(self.allocator);
                const incoming_callable = try self.resolveCallableValuePath(arg, arg, &reversed_path);
                const incoming_resolution = CallableResolution{
                    .lambda = incoming_callable.lambda,
                    .requires_hidden_capture = incoming_callable.captures_local != null,
                };

                if (join_state.merged_callables[i]) |current| {
                    if (!callableResolutionsEqual(current, incoming_resolution)) {
                        std.debug.panic(
                            "MirToLir TODO: merging incompatible exact callable identities across join {d} param {d} is not implemented yet",
                            .{ @intFromEnum(jump.id), i },
                        );
                    }
                } else {
                    join_state.merged_callables[i] = incoming_resolution;
                }
            }
        },
    }
}

fn buildJoinCallableOverrideMap(
    self: *Self,
    join_stmt: std.meta.TagPayload(MIR.CFStmt, .join),
) Allocator.Error!MirCallableResolutionMap {
    var overrides = MirCallableResolutionMap.init(self.allocator);
    errdefer overrides.deinit();

    const join_key = @intFromEnum(join_stmt.id);
    const join_params = self.mir_store.getLocalSpan(join_stmt.params);
    const merged_callables = try self.allocator.alloc(?CallableResolution, join_params.len);
    defer self.allocator.free(merged_callables);
    @memset(merged_callables, null);

    var active_joins = ActiveJoinCallableMap.init(self.allocator);
    defer active_joins.deinit();

    const gop = try active_joins.getOrPut(join_key);
    if (gop.found_existing) unreachable;
    gop.value_ptr.* = .{
        .params = join_params,
        .merged_callables = merged_callables,
        .saw_incoming = false,
    };
    defer _ = active_joins.remove(join_key);

    try self.collectJoinCallableResolutions(&active_joins, join_stmt.remainder);
    if (!gop.value_ptr.saw_incoming) return overrides;

    try overrides.ensureTotalCapacity(@intCast(join_params.len));
    for (join_params, merged_callables, 0..) |param, incoming_callable, i| {
        if (!self.localMayContainCallable(param)) continue;
        overrides.putAssumeCapacity(
            @as(u32, @intFromEnum(param)),
            incoming_callable orelse std.debug.panic(
                "MirToLir invariant violated: join {d} param {d} had no incoming exact callable",
                .{ join_key, i },
            ),
        );
    }
    return overrides;
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

fn currentStmtMap(self: *Self) *MirToLirStmtMap {
    return self.current_stmt_map orelse std.debug.panic(
        "MirToLir invariant violated: MIR stmt lowering requires an active statement map",
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
    if (self.currentLocalOverrideScope()) |scope| {
        try scope.put(key, lir_local);
        return;
    }
    try self.currentLocalMap().put(key, lir_local);
}

fn mapMirLocal(self: *Self, mir_local: MIR.LocalId) Allocator.Error!LirLocalId {
    const key = @as(u32, @intFromEnum(mir_local));
    if (self.lookupMirLocalBinding(key)) |existing| return existing;

    const lir_local = try self.lir_store.addLocal(.{
        .layout_idx = try self.runtimeValueLayoutFromMirLocal(mir_local),
    });
    try self.currentLocalMap().put(key, lir_local);
    return lir_local;
}

fn pushMirTargetBinding(self: *Self, mir_local: MIR.LocalId) Allocator.Error!MirTargetBinding {
    const key = @as(u32, @intFromEnum(mir_local));
    const target = try self.lir_store.addLocal(.{
        .layout_idx = try self.runtimeValueLayoutFromMirLocal(mir_local),
    });

    if (self.currentLocalOverrideScope()) |scope| {
        const previous = scope.get(key);
        try scope.put(key, target);
        return .{
            .key = key,
            .target = target,
            .previous = previous,
            .installed_in_override_scope = true,
        };
    }

    const map = self.currentLocalMap();
    const previous = map.get(key);
    try map.put(key, target);
    return .{
        .key = key,
        .target = target,
        .previous = previous,
        .installed_in_override_scope = false,
    };
}

fn popMirTargetBinding(self: *Self, binding: MirTargetBinding) void {
    if (binding.installed_in_override_scope) {
        const scope = self.currentLocalOverrideScope() orelse std.debug.panic(
            "MirToLir invariant violated: target binding restore expected an active override scope",
            .{},
        );
        if (binding.previous) |previous| {
            scope.put(binding.key, previous) catch @panic("target binding restore failed");
        } else {
            _ = scope.remove(binding.key);
        }
        return;
    }

    const map = self.currentLocalMap();
    if (binding.previous) |previous| {
        map.put(binding.key, previous) catch @panic("target binding restore failed");
    } else {
        _ = map.remove(binding.key);
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
            .tag_payload => .tag_payload,
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
        .param, .captures_param, .join_param => .{ .alias_of_local = .{
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
            .field => |field| try self.borrowOrigin(
                try self.resolveMirLocalOrigin(field.source, visited),
                self.current_borrow_region,
                try self.singleProjectionSpan(.{ .field = @intCast(field.field_idx) }),
            ),
            .tag_payload => |payload| try self.borrowOrigin(
                try self.resolveMirLocalOrigin(payload.source, visited),
                self.current_borrow_region,
                try self.singleProjectionSpan(.tag_payload),
            ),
            .discriminant => .fresh,
        },
        .call => |call_result| blk: {
            const resolved = if (call_result.exact_lambda) |lambda_id|
                ResolvedCallable{
                    .lambda = lambda_id,
                    .captures_local = if (call_result.exact_requires_hidden_capture) call_result.callee else null,
                }
            else blk_resolved: {
                var reversed_path = std.ArrayList(MIR.CallableProjection).empty;
                defer reversed_path.deinit(self.allocator);
                break :blk_resolved try self.resolveCallableValuePath(call_result.callee, call_result.callee, &reversed_path);
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
    if (self.mir_store.monotype_store.getMonotype(self.mir_store.getLocal(target_mir).monotype) == .func) {
        std.debug.panic(
            "MirToLir invariant violated: function-valued symbol {d} survived strongest-form MIR callable lowering",
            .{symbol.raw()},
        );
    }

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
    tag_name: mir_mod.Monotype.Name,
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
    var stmt_map = MirToLirStmtMap.init(self.allocator);
    defer stmt_map.deinit();

    const prev_local_map = self.current_local_map;
    const prev_stmt_map = self.current_stmt_map;
    const prev_callable_overrides = self.current_callable_overrides;
    const prev_lambda = self.current_lambda;
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_stmt_map = &stmt_map;
    self.current_callable_overrides = null;
    self.current_lambda = null;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_stmt_map = prev_stmt_map;
    defer self.current_callable_overrides = prev_callable_overrides;
    defer self.current_lambda = prev_lambda;
    defer self.current_borrow_region = prev_region;

    return self.lowerStmt(root_body);
}

fn lowerStmt(self: *Self, stmt_id: MIR.CFStmtId) Allocator.Error!CFStmtId {
    const key = @as(u32, @intFromEnum(stmt_id));
    const cacheable = self.current_local_scopes.items.len == 0;
    const stmt_map = self.currentStmtMap();
    if (cacheable) {
        if (stmt_map.get(key)) |existing| return existing;
    }

    const stmt = self.mir_store.getCFStmt(stmt_id);
    const lowered = switch (stmt) {
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
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            defer self.popMirTargetBinding(target_binding);
            break :blk switch (assign.op) {
                .local => |source| blk_local: {
                    const lowered_source = try self.mapMirLocal(source);
                    break :blk_local try self.emitAssignRef(
                        target_binding.target,
                        aliasSemantics(lowered_source, LIR.RefProjectionSpan.empty()),
                        .{ .local = lowered_source },
                        lowered_next,
                    );
                },
                .discriminant => |discriminant| try self.emitAssignRef(
                    target_binding.target,
                    .fresh,
                    .{ .discriminant = .{ .source = try self.mapMirLocal(discriminant.source) } },
                    lowered_next,
                ),
                .field => |field| try self.emitAssignRef(
                    target_binding.target,
                    borrowSemantics(
                        try self.mapMirLocal(field.source),
                        try self.singleProjectionSpan(.{ .field = @intCast(field.field_idx) }),
                        self.current_borrow_region,
                    ),
                    .{ .field = .{
                        .source = try self.mapMirLocal(field.source),
                        .field_idx = @intCast(field.field_idx),
                    } },
                    lowered_next,
                ),
                .tag_payload => |payload| try self.emitAssignRef(
                    target_binding.target,
                    borrowSemantics(
                        try self.mapMirLocal(payload.source),
                        try self.singleProjectionSpan(.tag_payload),
                        self.current_borrow_region,
                    ),
                    .{ .tag_payload = .{
                        .source = try self.mapMirLocal(payload.source),
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

    if (cacheable) {
        try stmt_map.put(key, lowered);
    }
    return lowered;
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
    var body_callable_overrides = try self.buildJoinCallableOverrideMap(join_stmt);
    defer body_callable_overrides.deinit();

    const lowered_body = blk: {
        const prev_callable_overrides = self.current_callable_overrides;
        self.current_callable_overrides = &body_callable_overrides;
        try self.pushLocalOverrideScope();
        defer self.current_callable_overrides = prev_callable_overrides;
        defer self.popLocalOverrideScope();
        for (join_params, 0..) |param_local, i| {
            const key = @as(u32, @intFromEnum(param_local));
            const layout_idx = try self.joinParamLayoutFromCurrentBindingOrMirLocal(param_local);
            join_param_locals[i] = try self.lir_store.addLocal(.{
                .layout_idx = layout_idx,
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
    exact_lambda: ?MIR.LambdaId,
    exact_requires_hidden_capture: bool,
    visible_args: []const MIR.LocalId,
    target: MIR.LocalId,
    lowered_target: LirLocalId,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const resolved = if (exact_lambda) |lambda_id|
        ResolvedCallable{
            .lambda = lambda_id,
            .captures_local = if (exact_requires_hidden_capture) callee_mir else null,
        }
    else
        try self.resolveCallableValue(callee_mir);
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
        std.debug.panic(
            "MirToLir invariant violated: direct call result layout mismatch target_local={d} target_mono={d} target_layout={d} callee_local={d} lambda={d} callee_ret_layout={d}",
            .{
                @intFromEnum(target),
                @intFromEnum(self.mir_store.getLocal(target).monotype),
                @intFromEnum(target_layout),
                @intFromEnum(callee_mir),
                @intFromEnum(resolved.lambda),
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
            const target_binding = try self.pushMirTargetBinding(assign.target);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            defer self.popMirTargetBinding(target_binding);
            break :blk .{
                .body = switch (assign.op) {
                    .local => |source| try self.emitAssignRef(
                        target_binding.target,
                        aliasSemantics(try self.mapMirLocal(source), LIR.RefProjectionSpan.empty()),
                        .{ .local = try self.mapMirLocal(source) },
                        lowered_next.body,
                    ),
                    .discriminant => |discriminant| try self.emitAssignRef(
                        target_binding.target,
                        .fresh,
                        .{ .discriminant = .{ .source = try self.mapMirLocal(discriminant.source) } },
                        lowered_next.body,
                    ),
                    .field => |field| try self.emitAssignRef(
                        target_binding.target,
                        borrowSemantics(
                            try self.mapMirLocal(field.source),
                            try self.singleProjectionSpan(.{ .field = @intCast(field.field_idx) }),
                            self.current_borrow_region,
                        ),
                        .{ .field = .{
                            .source = try self.mapMirLocal(field.source),
                            .field_idx = @intCast(field.field_idx),
                        } },
                        lowered_next.body,
                    ),
                    .tag_payload => |payload| try self.emitAssignRef(
                        target_binding.target,
                        borrowSemantics(
                            try self.mapMirLocal(payload.source),
                            try self.singleProjectionSpan(.tag_payload),
                            self.current_borrow_region,
                        ),
                        .{ .tag_payload = .{
                            .source = try self.mapMirLocal(payload.source),
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
            var body_callable_overrides = try self.buildJoinCallableOverrideMap(join_stmt);
            defer body_callable_overrides.deinit();

            const lowered_body = blk_body: {
                const prev_callable_overrides = self.current_callable_overrides;
                self.current_callable_overrides = &body_callable_overrides;
                try self.pushLocalOverrideScope();
                defer self.current_callable_overrides = prev_callable_overrides;
                defer self.popLocalOverrideScope();
                for (join_params, 0..) |param_local, i| {
                    const key = @as(u32, @intFromEnum(param_local));
                    join_param_locals[i] = try self.lir_store.addLocal(.{
                        .layout_idx = try self.joinParamLayoutFromCurrentBindingOrMirLocal(param_local),
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
            try self.resolveCallableValue(ret_stmt.value),
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
    const root_mono = self.mir_store.monotype_store.getMonotype(root_const.monotype);
    const must_call = arg_locals.len != 0 or root_mono == .func;

    if (!must_call) {
        return .{
            .body = try self.lowerRootBody(root_const.body),
            .result_contract = try self.lowerSummaryResultContract(
                self.analyses.getConstResultContract(root_const_id),
            ),
        };
    }

    const target = try self.freshLocal(ret_layout);
    const ret_stmt = try self.emitRet(target);

    var local_map = MirToLirLocalMap.init(self.allocator);
    defer local_map.deinit();
    var stmt_map = MirToLirStmtMap.init(self.allocator);
    defer stmt_map.deinit();
    const prev_local_map = self.current_local_map;
    const prev_stmt_map = self.current_stmt_map;
    const prev_callable_overrides = self.current_callable_overrides;
    const prev_lambda = self.current_lambda;
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_stmt_map = &stmt_map;
    self.current_callable_overrides = null;
    self.current_lambda = null;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_stmt_map = prev_stmt_map;
    defer self.current_callable_overrides = prev_callable_overrides;
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
    var stmt_map = MirToLirStmtMap.init(self.allocator);
    defer stmt_map.deinit();
    const prev_local_map = self.current_local_map;
    const prev_stmt_map = self.current_stmt_map;
    const prev_callable_overrides = self.current_callable_overrides;
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_stmt_map = &stmt_map;
    self.current_callable_overrides = null;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_stmt_map = prev_stmt_map;
    defer self.current_callable_overrides = prev_callable_overrides;
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
