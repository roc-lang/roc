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
const SummaryContract = ResultSummary.ResultContract;
const LoweredLambdaMap = std.AutoHashMap(u32, LirProcSpecId);
const LoweredConstMap = std.AutoHashMap(u32, LirProcSpecId);
const MirToLirLocalMap = std.AutoHashMap(u32, LirLocalId);
const MirToLirStmtMap = std.AutoHashMap(u32, CFStmtId);
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

const ValueDef = union(enum) {
    symbol: MIR.Symbol,
    alias: MIR.LocalId,
    lambda: MIR.LambdaId,
    closure: struct {
        lambda: MIR.LambdaId,
        captures: MIR.LocalSpan,
    },
    tag_value: MIR.LocalSpan,
    struct_value: MIR.LocalSpan,
    field: struct {
        source: MIR.LocalId,
        field_idx: u32,
    },
    tag_payload: struct {
        source: MIR.LocalId,
        payload_idx: u32,
    },
    nominal: MIR.LocalId,
    call_result: struct {
        callee: MIR.LocalId,
    },
};

const ValueDefMap = std.AutoHashMap(u32, ValueDef);

const ResolvedCallable = struct {
    lambda: MIR.LambdaId,
    captures_local: ?MIR.LocalId,
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
current_stmt_map: ?*MirToLirStmtMap = null,
current_value_defs: ?*const ValueDefMap = null,
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
    return self.runtimeRepresentationLayoutIdx(try self.mir_layout_resolver.resolve(monotype, null));
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

    var value_defs = ValueDefMap.init(self.allocator);
    defer value_defs.deinit();
    var value_def_visited = std.AutoHashMap(u32, void).init(self.allocator);
    defer value_def_visited.deinit();
    try self.collectValueDefs(lambda.body, &value_defs, &value_def_visited);

    const prev_value_defs = self.current_value_defs;
    const prev_lambda = self.current_lambda;
    self.current_value_defs = &value_defs;
    self.current_lambda = lambda_id;
    defer self.current_value_defs = prev_value_defs;
    defer self.current_lambda = prev_lambda;

    var visited = std.AutoHashMap(u32, void).init(self.allocator);
    defer visited.deinit();
    var returned_locals = std.ArrayList(MIR.LocalId).empty;
    defer returned_locals.deinit(self.allocator);

    try self.collectReturnedMirLocals(lambda.body, &visited, &returned_locals);
    if (returned_locals.items.len == 0) {
        std.debug.panic(
            "MirToLir invariant violated: function-returning lambda {d} has no reachable return",
            .{@intFromEnum(lambda_id)},
        );
    }

    const first_layout = try self.runtimeValueLayoutFromMirLocal(returned_locals.items[0]);
    for (returned_locals.items[1..]) |returned_local| {
        const other_layout = try self.runtimeValueLayoutFromMirLocal(returned_local);
        if (other_layout != first_layout) {
            std.debug.panic(
                "MirToLir invariant violated: lambda {d} returns callable values with inconsistent runtime layouts",
                .{@intFromEnum(lambda_id)},
            );
        }
    }
    return first_layout;
}

fn collectReturnedMirLocals(
    self: *Self,
    stmt_id: MIR.CFStmtId,
    visited: *std.AutoHashMap(u32, void),
    out: *std.ArrayList(MIR.LocalId),
) Allocator.Error!void {
    const key = @as(u32, @intFromEnum(stmt_id));
    const gop = try visited.getOrPut(key);
    if (gop.found_existing) return;

    switch (self.mir_store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_ref => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_literal => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_lambda => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_closure => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_call => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_low_level => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_list => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_struct => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_tag => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .debug => std.debug.panic(
            "MirToLir TODO: MIR debug statements are not implemented in LIR lowering yet",
            .{},
        ),
        .expect => std.debug.panic(
            "MirToLir TODO: MIR expect statements are not implemented in LIR lowering yet",
            .{},
        ),
        .switch_stmt => |switch_stmt| {
            for (self.mir_store.getSwitchBranches(switch_stmt.branches)) |branch| {
                try self.collectReturnedMirLocals(branch.body, visited, out);
            }
            try self.collectReturnedMirLocals(switch_stmt.default_branch, visited, out);
        },
        .borrow_scope => |scope_stmt| {
            try self.collectReturnedMirLocals(scope_stmt.body, visited, out);
            try self.collectReturnedMirLocals(scope_stmt.remainder, visited, out);
        },
        .join => |join_stmt| {
            try self.collectReturnedMirLocals(join_stmt.body, visited, out);
            try self.collectReturnedMirLocals(join_stmt.remainder, visited, out);
        },
        .ret => |ret_stmt| try out.append(self.allocator, ret_stmt.value),
        .runtime_error,
        .scope_exit,
        .jump,
        .crash,
        => {},
    }
}

fn recordValueDef(
    self: *Self,
    defs: *ValueDefMap,
    local_id: MIR.LocalId,
    def: ValueDef,
) Allocator.Error!void {
    const key = @as(u32, @intFromEnum(local_id));
    const gop = try defs.getOrPut(key);
    if (gop.found_existing) {
        std.debug.panic(
            "MirToLir invariant violated: MIR local {d} had multiple reaching value defs",
            .{@intFromEnum(local_id)},
        );
    }
    gop.value_ptr.* = def;
}

fn collectValueDefs(
    self: *Self,
    stmt_id: MIR.CFStmtId,
    defs: *ValueDefMap,
    visited: *std.AutoHashMap(u32, void),
) Allocator.Error!void {
    const key = @as(u32, @intFromEnum(stmt_id));
    const gop = try visited.getOrPut(key);
    if (gop.found_existing) return;

    switch (self.mir_store.getCFStmt(stmt_id)) {
        .assign_symbol => |assign| {
            try self.recordValueDef(defs, assign.target, .{ .symbol = assign.symbol });
            try self.collectValueDefs(assign.next, defs, visited);
        },
        .assign_ref => |assign| {
            switch (assign.op) {
                .local => |source| try self.recordValueDef(defs, assign.target, .{ .alias = source }),
                .field => |field| try self.recordValueDef(defs, assign.target, .{ .field = .{
                    .source = field.source,
                    .field_idx = field.field_idx,
                } }),
                .tag_payload => |payload| try self.recordValueDef(defs, assign.target, .{ .tag_payload = .{
                    .source = payload.source,
                    .payload_idx = payload.payload_idx,
                } }),
                .nominal => |nominal| try self.recordValueDef(defs, assign.target, .{ .nominal = nominal.backing }),
                .discriminant => {},
            }
            try self.collectValueDefs(assign.next, defs, visited);
        },
        .assign_literal => |assign| try self.collectValueDefs(assign.next, defs, visited),
        .assign_lambda => |assign| {
            try self.recordValueDef(defs, assign.target, .{ .lambda = assign.lambda });
            try self.collectValueDefs(assign.next, defs, visited);
        },
        .assign_closure => |assign| {
            try self.recordValueDef(defs, assign.target, .{ .closure = .{
                .lambda = assign.lambda,
                .captures = assign.captures,
            } });
            try self.collectValueDefs(assign.next, defs, visited);
        },
        .assign_call => |assign| {
            try self.recordValueDef(defs, assign.target, .{ .call_result = .{
                .callee = assign.callee,
            } });
            try self.collectValueDefs(assign.next, defs, visited);
        },
        .assign_low_level => |assign| try self.collectValueDefs(assign.next, defs, visited),
        .assign_list => |assign| try self.collectValueDefs(assign.next, defs, visited),
        .assign_struct => |assign| {
            try self.recordValueDef(defs, assign.target, .{ .struct_value = assign.fields });
            try self.collectValueDefs(assign.next, defs, visited);
        },
        .assign_tag => |assign| {
            try self.recordValueDef(defs, assign.target, .{ .tag_value = assign.args });
            try self.collectValueDefs(assign.next, defs, visited);
        },
        .debug => std.debug.panic(
            "MirToLir TODO: MIR debug statements are not implemented in value-def collection yet",
            .{},
        ),
        .expect => std.debug.panic(
            "MirToLir TODO: MIR expect statements are not implemented in value-def collection yet",
            .{},
        ),
        .runtime_error, .scope_exit, .jump, .ret, .crash => {},
        .switch_stmt => |switch_stmt| {
            for (self.mir_store.getSwitchBranches(switch_stmt.branches)) |branch| {
                try self.collectValueDefs(branch.body, defs, visited);
            }
            try self.collectValueDefs(switch_stmt.default_branch, defs, visited);
        },
        .borrow_scope => |scope_stmt| {
            try self.collectValueDefs(scope_stmt.body, defs, visited);
            try self.collectValueDefs(scope_stmt.remainder, defs, visited);
        },
        .join => |join_stmt| {
            try self.collectValueDefs(join_stmt.body, defs, visited);
            try self.collectValueDefs(join_stmt.remainder, defs, visited);
        },
    }
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

fn resolveCallableValuePath(
    self: *Self,
    local_id: MIR.LocalId,
    value_local: MIR.LocalId,
    reversed_path: *std.ArrayList(MIR.CallableProjection),
) Allocator.Error!ResolvedCallable {
    const defs = self.current_value_defs;
    const def = if (defs) |current_defs|
        current_defs.get(@as(u32, @intFromEnum(local_id)))
    else
        null;
    const resolved_def = def orelse {
        return self.resolveCallableForParamProjection(local_id, value_local, reversed_path.items);
    };

    return switch (resolved_def) {
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
        .alias => |source| self.resolveCallableValuePath(source, value_local, reversed_path),
        .nominal => |backing| self.resolveCallableValuePath(backing, value_local, reversed_path),
        .field => |field| blk: {
            if (defs) |current_defs| {
                if (current_defs.get(@as(u32, @intFromEnum(field.source)))) |source_def| switch (source_def) {
                    .struct_value => |fields| {
                        const field_locals = self.mir_store.getLocalSpan(fields);
                        if (field.field_idx >= field_locals.len) {
                            std.debug.panic(
                                "MirToLir invariant violated: callable field index {d} exceeds struct arity {d}",
                                .{ field.field_idx, field_locals.len },
                            );
                        }
                        break :blk try self.resolveCallableValuePath(field_locals[field.field_idx], value_local, reversed_path);
                    },
                    .symbol, .call_result => {},
                    else => {},
                };
            }

            try reversed_path.append(self.allocator, .{ .field = field.field_idx });
            defer _ = reversed_path.pop();
            break :blk try self.resolveCallableValuePath(field.source, value_local, reversed_path);
        },
        .tag_payload => |payload| blk: {
            if (defs) |current_defs| {
                if (current_defs.get(@as(u32, @intFromEnum(payload.source)))) |source_def| switch (source_def) {
                    .tag_value => |args| {
                        const payload_locals = self.mir_store.getLocalSpan(args);
                        if (payload.payload_idx >= payload_locals.len) {
                            std.debug.panic(
                                "MirToLir invariant violated: callable payload index {d} exceeds tag arity {d}",
                                .{ payload.payload_idx, payload_locals.len },
                            );
                        }
                        break :blk try self.resolveCallableValuePath(payload_locals[payload.payload_idx], value_local, reversed_path);
                    },
                    .symbol, .call_result => {},
                    else => {},
                };
            }

            try reversed_path.append(self.allocator, .{ .tag_payload = payload.payload_idx });
            defer _ = reversed_path.pop();
            break :blk try self.resolveCallableValuePath(payload.source, value_local, reversed_path);
        },
        .symbol => |symbol| std.debug.panic(
            "MirToLir invariant violated: function-valued symbol {d} survived strongest-form MIR callable lowering",
            .{symbol.raw()},
        ),
        .call_result => |call_result| {
            if (reversed_path.items.len != 0) {
                std.debug.panic(
                    "MirToLir TODO: exact callable resolution through projected call results is not implemented yet",
                    .{},
                );
            }

            const callee = try self.resolveCallableValuePath(call_result.callee, call_result.callee, reversed_path);
            return switch (self.analyses.getLambdaCallableContract(callee.lambda)) {
                .no_return => std.debug.panic(
                    "MirToLir invariant violated: call-result callable resolution reached a no-return lambda",
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
        .struct_value => std.debug.panic(
            "MirToLir invariant violated: callable resolution reached a non-callable struct value",
            .{},
        ),
        .tag_value => std.debug.panic(
            "MirToLir invariant violated: callable resolution reached a non-callable tag value",
            .{},
        ),
    };
}

fn resolveCallableValue(self: *Self, local_id: MIR.LocalId) Allocator.Error!ResolvedCallable {
    var reversed_path = std.ArrayList(MIR.CallableProjection).empty;
    defer reversed_path.deinit(self.allocator);
    return self.resolveCallableValuePath(local_id, local_id, &reversed_path);
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

fn mapMirLocal(self: *Self, mir_local: MIR.LocalId) Allocator.Error!LirLocalId {
    const key = @as(u32, @intFromEnum(mir_local));
    const map = self.currentLocalMap();
    if (map.get(key)) |existing| return existing;

    const lir_local = try self.lir_store.addLocal(.{
        .layout_idx = try self.runtimeValueLayoutFromMirLocal(mir_local),
    });
    try map.put(key, lir_local);
    return lir_local;
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

fn lowLevelResultSemantics(
    _: *Self,
    region: LIR.BorrowRegion,
    op: LIR.LowLevel,
    args: []const LirLocalId,
) ResultSemantics {
    return switch (op.procResultSemantics()) {
        .fresh => .fresh,
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
            "MirToLir invariant violated: no-return proc {d} must not be lowered as a value-producing call",
            .{@intFromEnum(proc_id)},
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
    symbol: MIR.Symbol,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const target = try self.mapMirLocal(target_mir);

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
                    if (aliased.owner == source and aliased.projections.isEmpty()) return next;
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
                .value = @intCast(mirIntValueAsI128(int_lit)),
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
        lowered_branches[i] = .{
            .value = branch.value,
            .body = try self.lowerStmt(branch.body),
        };
    }

    return self.lir_store.addCFStmt(.{ .switch_stmt = .{
        .cond = try self.mapMirLocal(switch_stmt.scrutinee),
        .branches = try self.lir_store.addCFSwitchBranches(lowered_branches),
        .default_branch = try self.lowerStmt(switch_stmt.default_branch),
    } });
}

fn lowerRootBody(self: *Self, root_body: MIR.CFStmtId) Allocator.Error!CFStmtId {
    var local_map = MirToLirLocalMap.init(self.allocator);
    defer local_map.deinit();
    var stmt_map = MirToLirStmtMap.init(self.allocator);
    defer stmt_map.deinit();
    var value_defs = ValueDefMap.init(self.allocator);
    defer value_defs.deinit();
    var value_def_visited = std.AutoHashMap(u32, void).init(self.allocator);
    defer value_def_visited.deinit();
    try self.collectValueDefs(root_body, &value_defs, &value_def_visited);

    const prev_local_map = self.current_local_map;
    const prev_stmt_map = self.current_stmt_map;
    const prev_value_defs = self.current_value_defs;
    const prev_lambda = self.current_lambda;
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_stmt_map = &stmt_map;
    self.current_value_defs = &value_defs;
    self.current_lambda = null;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_stmt_map = prev_stmt_map;
    defer self.current_value_defs = prev_value_defs;
    defer self.current_lambda = prev_lambda;
    defer self.current_borrow_region = prev_region;

    return self.lowerStmt(root_body);
}

fn lowerStmt(self: *Self, stmt_id: MIR.CFStmtId) Allocator.Error!CFStmtId {
    const key = @as(u32, @intFromEnum(stmt_id));
    const stmt_map = self.currentStmtMap();
    if (stmt_map.get(key)) |existing| return existing;

    const stmt = self.mir_store.getCFStmt(stmt_id);
    const lowered = switch (stmt) {
        .assign_symbol => |assign| try self.lowerMirSymbolValue(
            assign.target,
            assign.symbol,
            try self.lowerStmt(assign.next),
        ),
        .assign_ref => |assign| blk: {
            const lowered_target = try self.mapMirLocal(assign.target);
            const lowered_next = try self.lowerStmt(assign.next);
            break :blk switch (assign.op) {
                .local => |source| try self.emitAssignRef(
                    lowered_target,
                    aliasSemantics(try self.mapMirLocal(source), LIR.RefProjectionSpan.empty()),
                    .{ .local = try self.mapMirLocal(source) },
                    lowered_next,
                ),
                .discriminant => |discriminant| try self.emitAssignRef(
                    lowered_target,
                    .fresh,
                    .{ .discriminant = .{ .source = try self.mapMirLocal(discriminant.source) } },
                    lowered_next,
                ),
                .field => |field| try self.emitAssignRef(
                    lowered_target,
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
                    lowered_target,
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
                    lowered_target,
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
        .assign_literal => |assign| try self.emitAssignLiteral(
            try self.mapMirLocal(assign.target),
            self.mirLiteralValue(
                try self.runtimeValueLayoutFromMirLocal(assign.target),
                assign.literal,
            ),
            try self.lowerStmt(assign.next),
        ),
        .assign_lambda => |assign| blk: {
            _ = try self.lowerLambda(assign.lambda);
            break :blk try self.emitAssignUnit(
                try self.mapMirLocal(assign.target),
                try self.lowerStmt(assign.next),
            );
        },
        .assign_closure => |assign| blk: {
            _ = try self.lowerLambda(assign.lambda);
            break :blk try self.lir_store.addCFStmt(.{ .assign_struct = .{
                .target = try self.mapMirLocal(assign.target),
                .result = .fresh,
                .fields = try self.mapMirLocalSpan(assign.captures),
                .next = try self.lowerStmt(assign.next),
            } });
        },
        .assign_call => |assign| try self.lowerDirectLambdaCall(
            assign.callee,
            self.mir_store.getLocalSpan(assign.args),
            assign.target,
            try self.lowerStmt(assign.next),
        ),
        .assign_low_level => |assign| blk: {
            const mir_args = self.mir_store.getLocalSpan(assign.args);
            const lir_args = try self.allocator.alloc(LirLocalId, mir_args.len);
            defer self.allocator.free(lir_args);
            for (mir_args, 0..) |mir_arg, i| {
                lir_args[i] = try self.mapMirLocal(mir_arg);
            }
            break :blk try self.lir_store.addCFStmt(.{ .assign_low_level = .{
                .target = try self.mapMirLocal(assign.target),
                .result = lowLevelResultSemantics(self, self.current_borrow_region, assign.op, lir_args),
                .op = assign.op,
                .args = try self.lir_store.addLocalSpan(lir_args),
                .next = try self.lowerStmt(assign.next),
            } });
        },
        .assign_list => |assign| try self.lir_store.addCFStmt(.{ .assign_list = .{
            .target = try self.mapMirLocal(assign.target),
            .result = .fresh,
            .elems = try self.mapMirLocalSpan(assign.elems),
            .next = try self.lowerStmt(assign.next),
        } }),
        .assign_struct => |assign| try self.lir_store.addCFStmt(.{ .assign_struct = .{
            .target = try self.mapMirLocal(assign.target),
            .result = .fresh,
            .fields = try self.mapMirLocalSpan(assign.fields),
            .next = try self.lowerStmt(assign.next),
        } }),
        .assign_tag => |assign| try self.lir_store.addCFStmt(.{ .assign_tag = .{
            .target = try self.mapMirLocal(assign.target),
            .result = .fresh,
            .discriminant = self.tagDiscriminantForMonotypeName(
                self.mir_store.getLocal(assign.target).monotype,
                assign.name,
            ),
            .args = try self.mapMirLocalSpan(assign.args),
            .next = try self.lowerStmt(assign.next),
        } }),
        .debug => std.debug.panic(
            "MirToLir TODO: MIR debug statements are not implemented in LIR lowering yet",
            .{},
        ),
        .expect => std.debug.panic(
            "MirToLir TODO: MIR expect statements are not implemented in LIR lowering yet",
            .{},
        ),
        .runtime_error => try self.lir_store.addCFStmt(.{ .runtime_error = {} }),
        .switch_stmt => |switch_stmt| try self.lowerSwitchStmt(switch_stmt),
        .borrow_scope => |scope| try self.lowerBorrowScopeStmt(scope),
        .scope_exit => |scope_exit| try self.lir_store.addCFStmt(.{ .scope_exit = .{
            .id = translateBorrowScopeId(scope_exit.id),
        } }),
        .join => |join_stmt| try self.lowerJoinStmt(join_stmt),
        .jump => |jump_stmt| try self.lir_store.addCFStmt(.{ .jump = .{
            .target = translateJoinPointId(jump_stmt.id),
            .args = try self.mapMirLocalSpan(jump_stmt.args),
        } }),
        .ret => |ret_stmt| try self.emitRet(try self.mapMirLocal(ret_stmt.value)),
        .crash => |msg| try self.lir_store.addCFStmt(.{ .crash = .{
            .msg = try self.internMirStringLiteral(msg),
        } }),
    };

    try stmt_map.put(key, lowered);
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
    _ = try self.mapMirLocalSpan(join_stmt.params);
    return self.lir_store.addCFStmt(.{ .join = .{
        .id = translateJoinPointId(join_stmt.id),
        .params = try self.mapMirLocalSpan(join_stmt.params),
        .body = try self.lowerStmt(join_stmt.body),
        .remainder = try self.lowerStmt(join_stmt.remainder),
    } });
}

fn lowerDirectLambdaCall(
    self: *Self,
    callee_mir: MIR.LocalId,
    visible_args: []const MIR.LocalId,
    target: MIR.LocalId,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const resolved = try self.resolveCallableValue(callee_mir);
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
        .target = try self.mapMirLocal(target),
        .result = self.callResultSemantics(lowered_proc, call_args),
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
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            break :blk .{
                .body = try self.lowerMirSymbolValue(assign.target, assign.symbol, lowered_next.body),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_ref => |assign| blk: {
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            break :blk .{
                .body = switch (assign.op) {
                    .local => |source| try self.emitAssignRef(
                        try self.mapMirLocal(assign.target),
                        aliasSemantics(try self.mapMirLocal(source), LIR.RefProjectionSpan.empty()),
                        .{ .local = try self.mapMirLocal(source) },
                        lowered_next.body,
                    ),
                    .discriminant => |discriminant| try self.emitAssignRef(
                        try self.mapMirLocal(assign.target),
                        .fresh,
                        .{ .discriminant = .{ .source = try self.mapMirLocal(discriminant.source) } },
                        lowered_next.body,
                    ),
                    .field => |field| try self.emitAssignRef(
                        try self.mapMirLocal(assign.target),
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
                        try self.mapMirLocal(assign.target),
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
                        try self.mapMirLocal(assign.target),
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
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            break :blk .{
                .body = try self.emitAssignLiteral(
                    try self.mapMirLocal(assign.target),
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
            _ = try self.lowerLambda(assign.lambda);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            break :blk .{
                .body = try self.emitAssignUnit(try self.mapMirLocal(assign.target), lowered_next.body),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_closure => |assign| blk: {
            _ = try self.lowerLambda(assign.lambda);
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .assign_struct = .{
                    .target = try self.mapMirLocal(assign.target),
                    .result = .fresh,
                    .fields = try self.mapMirLocalSpan(assign.captures),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_call => |assign| blk: {
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            break :blk .{
                .body = try self.lowerDirectLambdaCall(
                    assign.callee,
                    self.mir_store.getLocalSpan(assign.args),
                    assign.target,
                    lowered_next.body,
                ),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_low_level => |assign| blk: {
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            const mir_args = self.mir_store.getLocalSpan(assign.args);
            const lir_args = try self.allocator.alloc(LirLocalId, mir_args.len);
            defer self.allocator.free(lir_args);
            for (mir_args, 0..) |mir_arg, i| {
                lir_args[i] = try self.mapMirLocal(mir_arg);
            }
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .assign_low_level = .{
                    .target = try self.mapMirLocal(assign.target),
                    .result = lowLevelResultSemantics(self, self.current_borrow_region, assign.op, lir_args),
                    .op = assign.op,
                    .args = try self.lir_store.addLocalSpan(lir_args),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_list => |assign| blk: {
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .assign_list = .{
                    .target = try self.mapMirLocal(assign.target),
                    .result = .fresh,
                    .elems = try self.mapMirLocalSpan(assign.elems),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_struct => |assign| blk: {
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .assign_struct = .{
                    .target = try self.mapMirLocal(assign.target),
                    .result = .fresh,
                    .fields = try self.mapMirLocalSpan(assign.fields),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_tag => |assign| blk: {
            const lowered_next = try self.lowerEntrypointCallableStmt(assign.next, arg_locals, target, next);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .assign_tag = .{
                    .target = try self.mapMirLocal(assign.target),
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
        .debug => std.debug.panic(
            "MirToLir TODO: MIR debug statements are not implemented in LIR entrypoint callable lowering yet",
            .{},
        ),
        .expect => std.debug.panic(
            "MirToLir TODO: MIR expect statements are not implemented in LIR entrypoint callable lowering yet",
            .{},
        ),
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
                const lowered_branch = try self.lowerEntrypointCallableStmt(branch.body, arg_locals, target, next);
                lowered_branches[i] = .{
                    .value = branch.value,
                    .body = lowered_branch.body,
                };
                merged_contract = mergeEntrypointResultContracts(merged_contract, lowered_branch.result_contract);
            }

            const lowered_default = try self.lowerEntrypointCallableStmt(switch_stmt.default_branch, arg_locals, target, next);
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
            _ = try self.mapMirLocalSpan(join_stmt.params);
            const lowered_body = try self.lowerEntrypointCallableStmt(join_stmt.body, arg_locals, target, next);
            const lowered_remainder = try self.lowerEntrypointCallableStmt(join_stmt.remainder, arg_locals, target, next);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .join = .{
                    .id = translateJoinPointId(join_stmt.id),
                    .params = try self.mapMirLocalSpan(join_stmt.params),
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
    var value_defs = ValueDefMap.init(self.allocator);
    defer value_defs.deinit();
    var value_def_visited = std.AutoHashMap(u32, void).init(self.allocator);
    defer value_def_visited.deinit();
    try self.collectValueDefs(root_const.body, &value_defs, &value_def_visited);

    const prev_local_map = self.current_local_map;
    const prev_stmt_map = self.current_stmt_map;
    const prev_value_defs = self.current_value_defs;
    const prev_lambda = self.current_lambda;
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_stmt_map = &stmt_map;
    self.current_value_defs = &value_defs;
    self.current_lambda = null;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_stmt_map = prev_stmt_map;
    defer self.current_value_defs = prev_value_defs;
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
    var value_defs = ValueDefMap.init(self.allocator);
    defer value_defs.deinit();
    var value_def_visited = std.AutoHashMap(u32, void).init(self.allocator);
    defer value_def_visited.deinit();
    try self.collectValueDefs(lambda.body, &value_defs, &value_def_visited);

    const prev_local_map = self.current_local_map;
    const prev_stmt_map = self.current_stmt_map;
    const prev_value_defs = self.current_value_defs;
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_stmt_map = &stmt_map;
    self.current_value_defs = &value_defs;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_stmt_map = prev_stmt_map;
    defer self.current_value_defs = prev_value_defs;
    defer self.current_borrow_region = prev_region;

    for (value_params, 0..) |param_local, i| {
        try local_map.put(@as(u32, @intFromEnum(param_local)), args[i]);
    }
    if (lambda.captures_param) |captures_param| {
        try local_map.put(@as(u32, @intFromEnum(captures_param)), args[value_params.len]);
    }

    var body = try self.lowerStmt(lambda.body);

    if (builtin.mode == .Debug) {
        // This verifier exists only to catch compiler implementation bugs by
        // re-scanning already-lowered LIR. It must remain debug-only because
        // release compiler builds must not pay for extra full-LIR verification.
        try DebugVerifyLir.verifyProc(
            self.allocator,
            self.lir_store,
            self.layout_store,
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
