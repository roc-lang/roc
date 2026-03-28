//! MIR -> strongest-form statement-only LIR lowering.
//!
//! This pass assumes strongest-form statement MIR:
//! - all intermediate values live in explicit MIR locals
//! - control flow is explicit MIR `CFStmt`
//! - lambdas/closures are still explicit MIR concepts
//! - patterns still exist and are lowered here into explicit ref/projection flow

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout = @import("layout");
const mir_mod = @import("mir");

const MIR = mir_mod.MIR;
const Analyses = mir_mod.Analyses;
const ProcResultSummary = mir_mod.ProcResultSummary;

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
const LocalRef = LIR.LocalId;
const LocalRefSpan = LIR.LocalSpan;
const ResultSemantics = LIR.ResultSemantics;
const RefOp = LIR.RefOp;
const SummaryContract = ProcResultSummary.ProcResultContract;
const LoweredLambdaMap = std.AutoHashMap(u32, LirProcSpecId);
const MirToLirLocalMap = std.AutoHashMap(u32, LocalRef);
const MirToLirStmtMap = std.AutoHashMap(u32, CFStmtId);
const dec_literal_scale: i128 = std.math.pow(i128, 10, 18);

const BuilderProcHeader = struct {
    name: LIR.Symbol,
    args: LocalRefSpan,
    ret_layout: layout.Idx,
    result_contract: LIR.ProcResultContract,
    hosted: ?LIR.HostedProc,
};

const BuilderProc = union(enum) {
    unresolved: BuilderProcHeader,
    resolved: LirProcSpec,
};

const EntrypointCallable = union(enum) {
    lambda: MIR.LambdaId,
    closure: struct {
        lambda: MIR.LambdaId,
        captures: MIR.LocalId,
    },
};

const LinearValueDef = union(enum) {
    symbol: MIR.Symbol,
    alias: MIR.LocalId,
    lambda: MIR.LambdaId,
    closure: MIR.LambdaId,
    struct_value: MIR.LocalSpan,
    field: struct {
        source: MIR.LocalId,
        field_idx: u32,
    },
    nominal: MIR.LocalId,
};

const Self = @This();

allocator: Allocator,
mir_store: *const MIR.Store,
lir_store: *LirStore,
layout_store: *layout.Store,
mir_layout_resolver: layout.MirMonotypeLayoutResolver,
analyses: *const Analyses,
lowered_lambdas: LoweredLambdaMap,
builder_procs: std.ArrayList(BuilderProc),
current_local_map: ?*MirToLirLocalMap = null,
current_stmt_map: ?*MirToLirStmtMap = null,
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
        .builder_procs = std.ArrayList(BuilderProc).empty,
    };
}

/// Releases temporary lowering state owned by this translator.
pub fn deinit(self: *Self) void {
    self.lowered_lambdas.deinit();
    self.builder_procs.deinit(self.allocator);
    self.mir_layout_resolver.deinit();
}

/// Lowers a root MIR constant body into a zero-argument LIR proc and flushes all lowered procs.
pub fn lower(self: *Self, root_const_id: MIR.ConstDefId) Allocator.Error!LirProcSpecId {
    self.ensureCanLowerMoreProcs();

    const root_const = self.mir_store.getConstDef(root_const_id);
    const ret_layout = try self.runtimeValueLayoutFromMirMonotype(root_const.monotype);
    const result_contract = try self.lowerSummaryProcResultContract(
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
            LocalRefSpan.empty(),
            result_contract,
            body,
        );
    }

    const proc = LirProcSpec{
        .name = self.lir_store.freshSyntheticSymbol(),
        .args = LocalRefSpan.empty(),
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

    const args = try self.allocator.alloc(LocalRef, arg_layouts.len);
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

fn freshLocal(self: *Self, layout_idx: layout.Idx) Allocator.Error!LocalRef {
    return self.lir_store.addLocal(.{
        .layout_idx = layout_idx,
        .source_symbol = self.lir_store.freshSyntheticSymbol(),
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

fn runtimeValueLayoutFromMirPattern(self: *Self, pattern_id: MIR.PatternId) Allocator.Error!layout.Idx {
    return self.runtimeValueLayoutFromMirMonotype(self.mir_store.patternTypeOf(pattern_id));
}

fn exactCallableSeedForSymbol(self: *const Self, symbol: MIR.Symbol) ?MIR.SeedMember {
    const members = self.mir_store.getSymbolSeedMembers(symbol) orelse return null;
    if (members.len == 0) return null;
    if (members.len != 1) {
        std.debug.panic(
            "MirToLir invariant violated: symbol {d} has {d} callable seeds where exactly one is required for runtime layout",
            .{ symbol.raw(), members.len },
        );
    }
    return members[0];
}

fn runtimeCallableValueLayoutFromSymbol(self: *Self, symbol: MIR.Symbol) Allocator.Error!?layout.Idx {
    if (self.mir_store.getFunctionDefForSymbol(symbol)) |function_def_id| {
        return try self.runtimeLambdaValueLayout(self.mir_store.getFunctionDef(function_def_id).lambda);
    }
    if (self.exactCallableSeedForSymbol(symbol)) |member| {
        return try self.runtimeLambdaValueLayout(member.lambda);
    }
    return null;
}

fn runtimeValueLayoutFromMirLocal(self: *Self, local_id: MIR.LocalId) Allocator.Error!layout.Idx {
    const local = self.mir_store.getLocal(local_id);
    if (self.mir_store.monotype_store.getMonotype(local.monotype) == .func) {
        if (try self.runtimeCallableValueLayoutFromSymbol(local.source_symbol)) |layout_idx| {
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
    if (!lambda.captures_param.isNone()) {
        const layout_idx = try self.runtimeValueLayoutFromMirPattern(lambda.captures_param);
        return layout_idx;
    }
    return self.runtimeRepresentationLayoutIdx(try self.mir_layout_resolver.resolve(lambda.fn_monotype, null));
}

fn runtimeLambdaReturnLayout(self: *Self, lambda_id: MIR.LambdaId) Allocator.Error!layout.Idx {
    const lambda = self.mir_store.getLambda(lambda_id);
    if (self.mir_store.monotype_store.getMonotype(lambda.ret_monotype) != .func) {
        return self.runtimeValueLayoutFromMirMonotype(lambda.ret_monotype);
    }

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
        .assign_alias => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_literal => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_lambda => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_closure => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_call => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_closure_call => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_low_level => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_list => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_struct => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_tag => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_field => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_tag_payload => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_nominal => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .assign_str_escape_and_quote => |assign| try self.collectReturnedMirLocals(assign.next, visited, out),
        .debug => |debug_stmt| try self.collectReturnedMirLocals(debug_stmt.next, visited, out),
        .expect => |expect_stmt| try self.collectReturnedMirLocals(expect_stmt.next, visited, out),
        .match_stmt => |match_stmt| {
            for (self.mir_store.getMatchBranches(match_stmt.branches)) |branch| {
                try self.collectReturnedMirLocals(branch.body, visited, out);
            }
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

fn mapMirLocal(self: *Self, mir_local: MIR.LocalId) Allocator.Error!LocalRef {
    const key = @as(u32, @intFromEnum(mir_local));
    const map = self.currentLocalMap();
    if (map.get(key)) |existing| return existing;

    const mir_local_info = self.mir_store.getLocal(mir_local);
    const lir_local = try self.lir_store.addLocal(.{
        .layout_idx = try self.runtimeValueLayoutFromMirLocal(mir_local),
        .source_symbol = mir_local_info.source_symbol,
    });
    try map.put(key, lir_local);
    return lir_local;
}

fn mapMirLocalSpan(self: *Self, span: MIR.LocalSpan) Allocator.Error!LocalRefSpan {
    const mir_locals = self.mir_store.getLocalSpan(span);
    const lir_locals = try self.allocator.alloc(LocalRef, mir_locals.len);
    defer self.allocator.free(lir_locals);

    for (mir_locals, 0..) |mir_local, i| {
        lir_locals[i] = try self.mapMirLocal(mir_local);
    }
    return self.lir_store.addLocalSpan(lir_locals);
}

fn singleProjectionSpan(self: *Self, projection: LIR.RefProjection) Allocator.Error!LIR.RefProjectionSpan {
    return self.lir_store.addRefProjectionSpan(&.{projection});
}

fn aliasSemantics(owner: LocalRef, projections: LIR.RefProjectionSpan) ResultSemantics {
    return .{ .alias_of = .{
        .owner = owner,
        .projections = projections,
    } };
}

fn borrowSemantics(owner: LocalRef, projections: LIR.RefProjectionSpan, region: LIR.BorrowRegion) ResultSemantics {
    return .{ .borrow_of = .{
        .owner = owner,
        .projections = projections,
        .region = region,
    } };
}

fn lowerSummaryProjectionSpan(
    self: *Self,
    summary_span: ProcResultSummary.RefProjectionSpan,
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
    contract: ProcResultSummary.ParamRefContract,
) Allocator.Error!LIR.ParamRefContract {
    return .{
        .param_index = contract.param_index,
        .projections = try self.lowerSummaryProjectionSpan(contract.projections),
    };
}

fn lowerSummaryProcResultContract(
    self: *Self,
    contract: ProcResultSummary.ProcResultContract,
) Allocator.Error!LIR.ProcResultContract {
    return switch (contract) {
        .fresh => .fresh,
        .alias_of_param => |param_ref| .{ .alias_of_param = try self.lowerSummaryParamRefContract(param_ref) },
        .borrow_of_param => |param_ref| .{ .borrow_of_param = try self.lowerSummaryParamRefContract(param_ref) },
    };
}

fn lowLevelResultSemantics(
    _: *Self,
    region: LIR.BorrowRegion,
    op: LIR.LowLevel,
    args: []const LocalRef,
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

fn getProcResultContract(self: *const Self, proc_id: LirProcSpecId) LIR.ProcResultContract {
    return switch (self.builder_procs.items[@intFromEnum(proc_id)]) {
        .resolved => |proc| proc.result_contract,
        .unresolved => |header| header.result_contract,
    };
}

fn callResultSemantics(self: *Self, proc_id: LirProcSpecId, args: []const LocalRef) ResultSemantics {
    return switch (self.getProcResultContract(proc_id)) {
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
    target: LocalRef,
    symbol: MIR.Symbol,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    return self.lir_store.addCFStmt(.{ .assign_symbol = .{
        .target = target,
        .symbol = symbol,
        .next = next,
    } });
}

fn emitAssignRef(
    self: *Self,
    target: LocalRef,
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
    target: LocalRef,
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

fn emitAssignUnit(self: *Self, target: LocalRef, next: CFStmtId) Allocator.Error!CFStmtId {
    return self.lir_store.addCFStmt(.{ .assign_struct = .{
        .target = target,
        .result = .fresh,
        .fields = LocalRefSpan.empty(),
        .next = next,
    } });
}

fn emitRet(self: *Self, value: LocalRef) Allocator.Error!CFStmtId {
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

fn fieldMonotypeForSource(
    self: *Self,
    source_mono: MirMonotypeIdx,
    field_idx: usize,
) MirMonotypeIdx {
    return switch (self.mir_store.monotype_store.getMonotype(source_mono)) {
        .record => |record| self.mir_store.monotype_store.getFields(record.fields)[field_idx].type_idx,
        .tuple => |tuple| self.mir_store.monotype_store.getIdxSpanItem(tuple.elems, field_idx),
        else => std.debug.panic(
            "MirToLir invariant violated: struct destructure source monotype {s} is not record/tuple",
            .{@tagName(self.mir_store.monotype_store.getMonotype(source_mono))},
        ),
    };
}

const TagVariantInfo = struct {
    discriminant: u16,
    payload_mono: ?MirMonotypeIdx,
};

fn tagVariantInfoForSource(
    self: *Self,
    source_mono: MirMonotypeIdx,
    tag_name: mir_mod.Monotype.Name,
) TagVariantInfo {
    const mono = self.mir_store.monotype_store.getMonotype(source_mono);
    const tags = switch (mono) {
        .tag_union => |tag_union| self.mir_store.monotype_store.getTags(tag_union.tags),
        else => std.debug.panic(
            "MirToLir invariant violated: tag pattern source monotype {s} is not a tag union",
            .{@tagName(mono)},
        ),
    };

    for (tags, 0..) |tag, index| {
        if (!tag.name.textEqual(self.analyses.all_module_envs, tag_name)) continue;

        const payloads = self.mir_store.monotype_store.getIdxSpan(tag.payloads);
        const payload_mono = switch (payloads.len) {
            0 => null,
            1 => payloads[0],
            else => std.debug.panic(
                "MirToLir invariant violated: multi-arg tag payloads must be wrapped before statement-only lowering",
                .{},
            ),
        };

        return .{
            .discriminant = @intCast(index),
            .payload_mono = payload_mono,
        };
    }

    std.debug.panic(
        "MirToLir invariant violated: tag pattern constructor missing from source monotype",
        .{},
    );
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

fn emitSwitchOnValue(
    self: *Self,
    cond: LocalRef,
    match_value: u64,
    on_match: CFStmtId,
    on_fail: CFStmtId,
) Allocator.Error!CFStmtId {
    const branches = try self.lir_store.addCFSwitchBranches(&.{.{
        .value = match_value,
        .body = on_match,
    }});
    return self.lir_store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond,
        .branches = branches,
        .default_branch = on_fail,
    } });
}

fn emitBoolSwitch(
    self: *Self,
    cond: LocalRef,
    on_true: CFStmtId,
    on_false: CFStmtId,
) Allocator.Error!CFStmtId {
    return self.emitSwitchOnValue(cond, 1, on_true, on_false);
}

fn literalPatternValue(
    self: *Self,
    pattern_id: MIR.PatternId,
    target_layout: layout.Idx,
) Allocator.Error!LiteralValue {
    return switch (self.mir_store.getPattern(pattern_id)) {
        .int_literal => |int_lit| mirLiteralValue(self, target_layout, .{ .int = int_lit.value }),
        .str_literal => |str_idx| .{ .str_literal = try self.internMirStringLiteral(str_idx) },
        .dec_literal => |dec_lit| .{ .dec_literal = dec_lit.num },
        .frac_f32_literal => |frac| .{ .f32_literal = frac },
        .frac_f64_literal => |frac| .{ .f64_literal = frac },
        else => std.debug.panic(
            "MirToLir invariant violated: non-literal pattern {s} passed to literalPatternValue",
            .{@tagName(self.mir_store.getPattern(pattern_id))},
        ),
    };
}

fn literalPatternEqOp(self: *Self, pattern_id: MIR.PatternId) LIR.LowLevel {
    return switch (self.mir_store.getPattern(pattern_id)) {
        .int_literal, .dec_literal, .frac_f32_literal, .frac_f64_literal => .num_is_eq,
        .str_literal => .str_is_eq,
        else => std.debug.panic(
            "MirToLir invariant violated: non-literal pattern {s} passed to literalPatternEqOp",
            .{@tagName(self.mir_store.getPattern(pattern_id))},
        ),
    };
}

fn lowerLiteralPatternInto(
    self: *Self,
    source: LocalRef,
    pattern_id: MIR.PatternId,
    on_match: CFStmtId,
    on_fail: CFStmtId,
) Allocator.Error!CFStmtId {
    const literal_local = try self.freshLocal(self.lir_store.getLocal(source).layout_idx);
    const eq_local = try self.freshLocal(.bool);
    const eq_args = try self.lir_store.addLocalSpan(&.{ source, literal_local });
    const eq_switch = try self.emitBoolSwitch(eq_local, on_match, on_fail);
    const eq_stmt = try self.lir_store.addCFStmt(.{ .assign_low_level = .{
        .target = eq_local,
        .result = .fresh,
        .op = self.literalPatternEqOp(pattern_id),
        .args = eq_args,
        .next = eq_switch,
    } });
    return self.emitAssignLiteral(
        literal_local,
        try self.literalPatternValue(pattern_id, self.lir_store.getLocal(source).layout_idx),
        eq_stmt,
    );
}

fn lowerPatternInto(
    self: *Self,
    source: LocalRef,
    source_mono: MirMonotypeIdx,
    pattern_id: MIR.PatternId,
    on_match: CFStmtId,
    on_fail: CFStmtId,
) Allocator.Error!CFStmtId {
    return switch (self.mir_store.getPattern(pattern_id)) {
        .bind => |mir_local| self.emitAssignRef(
            try self.mapMirLocal(mir_local),
            aliasSemantics(source, LIR.RefProjectionSpan.empty()),
            .{ .local = source },
            on_match,
        ),
        .wildcard => on_match,
        .as_pattern => |as_pattern| blk: {
            const inner = try self.lowerPatternInto(source, source_mono, as_pattern.pattern, on_match, on_fail);
            break :blk try self.emitAssignRef(
                try self.mapMirLocal(as_pattern.local),
                aliasSemantics(source, LIR.RefProjectionSpan.empty()),
                .{ .local = source },
                inner,
            );
        },
        .tag => |tag_pattern| blk: {
            const discr_local = try self.freshLocal(.u32);
            const variant = self.tagVariantInfoForSource(source_mono, tag_pattern.name);
            const payload_patterns = self.mir_store.getPatternSpan(tag_pattern.args);

            const matched = blk_matched: {
                if (payload_patterns.len == 0) break :blk_matched on_match;
                if (payload_patterns.len != 1) {
                    std.debug.panic(
                        "MirToLir invariant violated: tag pattern payload arity {d} must be wrapped before statement-only lowering",
                        .{payload_patterns.len},
                    );
                }
                const payload_mono = variant.payload_mono orelse std.debug.panic(
                    "MirToLir invariant violated: zero-payload tag pattern cannot destructure a payload",
                    .{},
                );

                const payload_pattern = payload_patterns[0];
                const payload_local = try self.freshLocal(try self.runtimeValueLayoutFromMirMonotype(payload_mono));
                const payload_match = try self.lowerPatternInto(payload_local, payload_mono, payload_pattern, on_match, on_fail);
                break :blk_matched try self.emitAssignRef(
                    payload_local,
                    borrowSemantics(source, try self.singleProjectionSpan(.tag_payload), self.current_borrow_region),
                    .{ .tag_payload = .{ .source = source } },
                    payload_match,
                );
            };

            const discr_switch = try self.emitSwitchOnValue(discr_local, variant.discriminant, matched, on_fail);
            break :blk try self.emitAssignRef(
                discr_local,
                .fresh,
                .{ .discriminant = .{ .source = source } },
                discr_switch,
            );
        },
        .struct_destructure => |destructure| blk: {
            var entry = on_match;
            const fields = self.mir_store.getPatternSpan(destructure.fields);
            var i = fields.len;
            while (i > 0) {
                i -= 1;
                const field_pattern = fields[i];
                const field_mono = self.fieldMonotypeForSource(source_mono, i);
                const field_local = try self.freshLocal(try self.runtimeValueLayoutFromMirMonotype(field_mono));
                const field_match = try self.lowerPatternInto(field_local, field_mono, field_pattern, entry, on_fail);
                entry = try self.emitAssignRef(
                    field_local,
                    borrowSemantics(
                        source,
                        try self.singleProjectionSpan(.{ .field = @intCast(i) }),
                        self.current_borrow_region,
                    ),
                    .{ .field = .{
                        .source = source,
                        .field_idx = @intCast(i),
                    } },
                    field_match,
                );
            }
            break :blk entry;
        },
        .int_literal,
        .str_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        => self.lowerLiteralPatternInto(source, pattern_id, on_match, on_fail),
        .list_destructure => std.debug.panic(
            "MirToLir invariant violated: list destructure must be lowered before statement-only LIR",
            .{},
        ),
        .runtime_error => std.debug.panic(
            "MirToLir invariant violated: runtime_error pattern must be lowered before statement-only LIR",
            .{},
        ),
    };
}

fn lowerBorrowBindingPattern(
    self: *Self,
    pattern_id: MIR.PatternId,
    source: LocalRef,
    scope_id: LIR.BorrowScopeId,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    return switch (self.mir_store.getPattern(pattern_id)) {
        .bind => |mir_local| self.emitAssignRef(
            try self.mapMirLocal(mir_local),
            borrowSemantics(source, LIR.RefProjectionSpan.empty(), .{ .scope = scope_id }),
            .{ .local = source },
            next,
        ),
        .wildcard => next,
        .as_pattern => |as_pattern| blk: {
            const inner = try self.lowerBorrowBindingPattern(as_pattern.pattern, source, scope_id, next);
            break :blk try self.emitAssignRef(
                try self.mapMirLocal(as_pattern.local),
                borrowSemantics(source, LIR.RefProjectionSpan.empty(), .{ .scope = scope_id }),
                .{ .local = source },
                inner,
            );
        },
        else => std.debug.panic(
            "MirToLir invariant violated: borrow-scope pattern {s} must be lowered before statement-only LIR",
            .{@tagName(self.mir_store.getPattern(pattern_id))},
        ),
    };
}

fn lowerRootBody(self: *Self, root_body: MIR.CFStmtId) Allocator.Error!CFStmtId {
    var local_map = MirToLirLocalMap.init(self.allocator);
    defer local_map.deinit();
    var stmt_map = MirToLirStmtMap.init(self.allocator);
    defer stmt_map.deinit();

    const prev_local_map = self.current_local_map;
    const prev_stmt_map = self.current_stmt_map;
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_stmt_map = &stmt_map;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_stmt_map = prev_stmt_map;
    defer self.current_borrow_region = prev_region;

    return self.lowerStmt(root_body);
}

fn lowerStmt(self: *Self, stmt_id: MIR.CFStmtId) Allocator.Error!CFStmtId {
    const key = @as(u32, @intFromEnum(stmt_id));
    const stmt_map = self.currentStmtMap();
    if (stmt_map.get(key)) |existing| return existing;

    const stmt = self.mir_store.getCFStmt(stmt_id);
    const lowered = switch (stmt) {
        .assign_symbol => |assign| try self.emitAssignSymbol(
            try self.mapMirLocal(assign.target),
            assign.symbol,
            try self.lowerStmt(assign.next),
        ),
        .assign_alias => |assign| try self.emitAssignRef(
            try self.mapMirLocal(assign.target),
            aliasSemantics(try self.mapMirLocal(assign.source), LIR.RefProjectionSpan.empty()),
            .{ .local = try self.mapMirLocal(assign.source) },
            try self.lowerStmt(assign.next),
        ),
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
            assign.lambda,
            null,
            self.mir_store.getLocalSpan(assign.args),
            assign.target,
            try self.lowerStmt(assign.next),
        ),
        .assign_closure_call => |assign| try self.lowerDirectLambdaCall(
            assign.lambda,
            assign.closure,
            self.mir_store.getLocalSpan(assign.args),
            assign.target,
            try self.lowerStmt(assign.next),
        ),
        .assign_low_level => |assign| blk: {
            const mir_args = self.mir_store.getLocalSpan(assign.args);
            const lir_args = try self.allocator.alloc(LocalRef, mir_args.len);
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
        .assign_field => |assign| try self.emitAssignRef(
            try self.mapMirLocal(assign.target),
            borrowSemantics(
                try self.mapMirLocal(assign.source),
                try self.singleProjectionSpan(.{ .field = @intCast(assign.field_idx) }),
                self.current_borrow_region,
            ),
            .{ .field = .{
                .source = try self.mapMirLocal(assign.source),
                .field_idx = @intCast(assign.field_idx),
            } },
            try self.lowerStmt(assign.next),
        ),
        .assign_tag_payload => |assign| try self.emitAssignRef(
            try self.mapMirLocal(assign.target),
            borrowSemantics(
                try self.mapMirLocal(assign.source),
                try self.singleProjectionSpan(.tag_payload),
                self.current_borrow_region,
            ),
            .{ .tag_payload = .{
                .source = try self.mapMirLocal(assign.source),
            } },
            try self.lowerStmt(assign.next),
        ),
        .assign_nominal => |assign| try self.emitAssignRef(
            try self.mapMirLocal(assign.target),
            aliasSemantics(
                try self.mapMirLocal(assign.backing),
                try self.singleProjectionSpan(.nominal),
            ),
            .{ .nominal = .{
                .backing_ref = try self.mapMirLocal(assign.backing),
            } },
            try self.lowerStmt(assign.next),
        ),
        .assign_str_escape_and_quote => |assign| blk: {
            const source = try self.mapMirLocal(assign.source);
            break :blk try self.lir_store.addCFStmt(.{ .assign_low_level = .{
                .target = try self.mapMirLocal(assign.target),
                .result = .fresh,
                .op = .str_inspect,
                .args = try self.lir_store.addLocalSpan(&.{source}),
                .next = try self.lowerStmt(assign.next),
            } });
        },
        .debug => |debug_stmt| blk: {
            _ = try self.mapMirLocal(debug_stmt.value);
            break :blk try self.lowerStmt(debug_stmt.next);
        },
        .expect => |expect_stmt| blk: {
            _ = try self.mapMirLocal(expect_stmt.condition);
            break :blk try self.lowerStmt(expect_stmt.next);
        },
        .runtime_error => try self.lir_store.addCFStmt(.{ .runtime_error = {} }),
        .match_stmt => |match_stmt| try self.lowerMatchStmt(match_stmt),
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

    var body = try self.lowerStmt(scope.body);
    const bindings = self.mir_store.getBorrowBindings(scope.bindings);
    var i = bindings.len;
    while (i > 0) {
        i -= 1;
        body = try self.lowerBorrowBindingPattern(
            bindings[i].pattern,
            try self.mapMirLocal(bindings[i].source),
            translateBorrowScopeId(scope.id),
            body,
        );
    }

    return self.lir_store.addCFStmt(.{ .borrow_scope = .{
        .id = translateBorrowScopeId(scope.id),
        .body = body,
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

fn lowerMatchBranchAlternative(
    self: *Self,
    source: LocalRef,
    source_mono: MirMonotypeIdx,
    pattern_id: MIR.PatternId,
    branch_body: CFStmtId,
    on_fail: CFStmtId,
) Allocator.Error!CFStmtId {
    return self.lowerPatternInto(
        source,
        source_mono,
        pattern_id,
        branch_body,
        on_fail,
    );
}

fn lowerMatchStmt(
    self: *Self,
    match_stmt: std.meta.TagPayload(MIR.CFStmt, .match_stmt),
) Allocator.Error!CFStmtId {
    const cond_local = try self.mapMirLocal(match_stmt.scrutinee);
    const cond_mono = self.mir_store.getLocal(match_stmt.scrutinee).monotype;
    const branches = self.mir_store.getMatchBranches(match_stmt.branches);

    var entry = try self.lir_store.addCFStmt(.{ .runtime_error = {} });
    var i = branches.len;
    while (i > 0) {
        i -= 1;
        const branch = branches[i];
        const branch_patterns = self.mir_store.getBranchPatterns(branch.patterns);
        var branch_entry = entry;
        var pattern_i = branch_patterns.len;
        while (pattern_i > 0) {
            pattern_i -= 1;
            _ = branch_patterns[pattern_i].degenerate;
            branch_entry = try self.lowerMatchBranchAlternative(
                cond_local,
                cond_mono,
                branch_patterns[pattern_i].pattern,
                try self.lowerStmt(branch.body),
                branch_entry,
            );
        }
        entry = branch_entry;
    }

    return entry;
}

fn lowerDirectLambdaCall(
    self: *Self,
    lambda_id: MIR.LambdaId,
    hidden_capture_mir: ?MIR.LocalId,
    visible_args: []const MIR.LocalId,
    target: MIR.LocalId,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const lambda = self.mir_store.getLambda(lambda_id);
    if (hidden_capture_mir == null and !lambda.captures_param.isNone()) {
        std.debug.panic(
            "MirToLir invariant violated: direct MIR lambda call requires a hidden captures argument for captured lambdas",
            .{},
        );
    }
    if (hidden_capture_mir != null and lambda.captures_param.isNone()) {
        std.debug.panic(
            "MirToLir invariant violated: closure call supplied captures to a lambda with no captures param",
            .{},
        );
    }

    const lowered_proc = try self.lowerLambda(lambda_id);
    const call_arg_count = visible_args.len + @intFromBool(hidden_capture_mir != null);
    const call_args = try self.allocator.alloc(LocalRef, call_arg_count);
    defer self.allocator.free(call_args);

    for (visible_args, 0..) |mir_arg, i| {
        call_args[i] = try self.mapMirLocal(mir_arg);
    }
    if (hidden_capture_mir) |capture_local| {
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
    return switch (self.getProcResultContract(proc_id)) {
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
    callable: EntrypointCallable,
    visible_args: []const LocalRef,
    target: LocalRef,
    next: CFStmtId,
) Allocator.Error!LoweredEntrypointProcBody {
    const lambda_id: MIR.LambdaId = switch (callable) {
        .lambda => |lambda_id| lambda_id,
        .closure => |closure| closure.lambda,
    };
    const proc_id = try self.lowerLambda(lambda_id);
    const callee_ret_layout = self.loweredProcRetLayout(proc_id);
    const target_layout = self.lir_store.getLocal(target).layout_idx;
    if (callee_ret_layout != target_layout) {
        std.debug.panic(
            "MirToLir invariant violated: entrypoint wrapper result layout does not match callee result layout",
            .{},
        );
    }

    const has_capture = switch (callable) {
        .lambda => false,
        .closure => true,
    };
    const call_arg_count = visible_args.len + @intFromBool(has_capture);
    const call_args = try self.allocator.alloc(LocalRef, call_arg_count);
    defer self.allocator.free(call_args);

    @memcpy(call_args[0..visible_args.len], visible_args);
    switch (callable) {
        .lambda => {},
        .closure => |closure| call_args[visible_args.len] = try self.mapMirLocal(closure.captures),
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

fn resolveLinearCallableValue(
    self: *Self,
    local_id: MIR.LocalId,
    defs: *std.AutoHashMap(u32, LinearValueDef),
) Allocator.Error!EntrypointCallable {
    const def = defs.get(@as(u32, @intFromEnum(local_id))) orelse std.debug.panic(
        "MirToLir invariant violated: entrypoint callable local {d} has no linear definition",
        .{@intFromEnum(local_id)},
    );

    return switch (def) {
        .lambda => |lambda_id| .{ .lambda = lambda_id },
        .closure => |lambda_id| .{ .closure = .{
            .lambda = lambda_id,
            .captures = local_id,
        } },
        .alias => |source| try self.resolveLinearCallableValue(source, defs),
        .nominal => |backing| try self.resolveLinearCallableValue(backing, defs),
        .symbol => |symbol| blk: {
            if (self.mir_store.getFunctionDefForSymbol(symbol)) |function_def_id| {
                break :blk .{ .lambda = self.mir_store.getFunctionDef(function_def_id).lambda };
            }
            if (self.mir_store.getConstDefForSymbol(symbol)) |const_def_id| {
                break :blk try self.resolveEntrypointCallable(self.mir_store.getConstDef(const_def_id).body);
            }
            std.debug.panic(
                "MirToLir invariant violated: symbol {d} did not resolve to a callable top-level def",
                .{symbol.raw()},
            );
        },
        .field => |field| blk: {
            const source_def = defs.get(@as(u32, @intFromEnum(field.source))) orelse std.debug.panic(
                "MirToLir invariant violated: field source local {d} has no linear definition",
                .{@intFromEnum(field.source)},
            );
            const fields = switch (source_def) {
                .struct_value => |fields| fields,
                else => std.debug.panic(
                    "MirToLir invariant violated: entrypoint callable field resolution requires a struct-producing source local",
                    .{},
                ),
            };
            const field_locals = self.mir_store.getLocalSpan(fields);
            if (field.field_idx >= field_locals.len) {
                std.debug.panic(
                    "MirToLir invariant violated: entrypoint field index {d} is out of bounds for struct arity {d}",
                    .{ field.field_idx, field_locals.len },
                );
            }
            break :blk try self.resolveLinearCallableValue(field_locals[field.field_idx], defs);
        },
        .struct_value => std.debug.panic(
            "MirToLir invariant violated: entrypoint callable resolution reached a non-callable struct value",
            .{},
        ),
    };
}

fn resolveEntrypointCallable(self: *Self, stmt_id: MIR.CFStmtId) Allocator.Error!EntrypointCallable {
    var defs = std.AutoHashMap(u32, LinearValueDef).init(self.allocator);
    defer defs.deinit();

    var current = stmt_id;
    while (true) {
        switch (self.mir_store.getCFStmt(current)) {
            .assign_symbol => |assign| {
                try defs.put(@as(u32, @intFromEnum(assign.target)), .{ .symbol = assign.symbol });
                current = assign.next;
            },
            .assign_alias => |assign| {
                try defs.put(@as(u32, @intFromEnum(assign.target)), .{ .alias = assign.source });
                current = assign.next;
            },
            .assign_lambda => |assign| {
                try defs.put(@as(u32, @intFromEnum(assign.target)), .{ .lambda = assign.lambda });
                current = assign.next;
            },
            .assign_closure => |assign| {
                try defs.put(@as(u32, @intFromEnum(assign.target)), .{ .closure = assign.lambda });
                current = assign.next;
            },
            .assign_struct => |assign| {
                try defs.put(@as(u32, @intFromEnum(assign.target)), .{ .struct_value = assign.fields });
                current = assign.next;
            },
            .assign_field => |assign| {
                try defs.put(@as(u32, @intFromEnum(assign.target)), .{ .field = .{
                    .source = assign.source,
                    .field_idx = assign.field_idx,
                } });
                current = assign.next;
            },
            .assign_nominal => |assign| {
                try defs.put(@as(u32, @intFromEnum(assign.target)), .{ .nominal = assign.backing });
                current = assign.next;
            },
            .assign_literal,
            => |assign| current = assign.next,
            .assign_call => |assign| current = assign.next,
            .assign_closure_call => |assign| current = assign.next,
            .assign_low_level => |assign| current = assign.next,
            .assign_list => |assign| current = assign.next,
            .assign_tag => |assign| current = assign.next,
            .assign_tag_payload => |assign| current = assign.next,
            .assign_str_escape_and_quote => |assign| current = assign.next,
            .debug => |debug_stmt| current = debug_stmt.next,
            .expect => |expect_stmt| current = expect_stmt.next,
            .ret => |ret_stmt| return self.resolveLinearCallableValue(ret_stmt.value, &defs),
            .runtime_error,
            .match_stmt,
            .borrow_scope,
            .scope_exit,
            .join,
            .jump,
            .crash,
            => std.debug.panic(
                "MirToLir invariant violated: entrypoint callable resolution currently requires a linear MIR body",
                .{},
            ),
        }
    }
}

fn lowerLinearEntrypointCallableBody(
    self: *Self,
    stmt_id: MIR.CFStmtId,
    callable: EntrypointCallable,
    arg_locals: []const LocalRef,
    target: LocalRef,
    next: CFStmtId,
) Allocator.Error!LoweredEntrypointProcBody {
    const stmt = self.mir_store.getCFStmt(stmt_id);
    return switch (stmt) {
        .assign_symbol => |assign| blk: {
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
            break :blk .{
                .body = try self.emitAssignSymbol(
                    try self.mapMirLocal(assign.target),
                    assign.symbol,
                    lowered_next.body,
                ),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_alias => |assign| blk: {
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
            break :blk .{
                .body = try self.emitAssignRef(
                    try self.mapMirLocal(assign.target),
                    aliasSemantics(try self.mapMirLocal(assign.source), LIR.RefProjectionSpan.empty()),
                    .{ .local = try self.mapMirLocal(assign.source) },
                    lowered_next.body,
                ),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_literal => |assign| blk: {
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
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
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
            break :blk .{
                .body = try self.emitAssignUnit(try self.mapMirLocal(assign.target), lowered_next.body),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_closure => |assign| blk: {
            _ = try self.lowerLambda(assign.lambda);
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
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
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
            break :blk .{
                .body = try self.lowerDirectLambdaCall(
                    assign.lambda,
                    null,
                    self.mir_store.getLocalSpan(assign.args),
                    assign.target,
                    lowered_next.body,
                ),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_closure_call => |assign| blk: {
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
            break :blk .{
                .body = try self.lowerDirectLambdaCall(
                    assign.lambda,
                    assign.closure,
                    self.mir_store.getLocalSpan(assign.args),
                    assign.target,
                    lowered_next.body,
                ),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_low_level => |assign| blk: {
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
            const mir_args = self.mir_store.getLocalSpan(assign.args);
            const lir_args = try self.allocator.alloc(LocalRef, mir_args.len);
            defer self.allocator.free(lir_args);
            for (mir_args, 0..) |mir_arg, i| lir_args[i] = try self.mapMirLocal(mir_arg);
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
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
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
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
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
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
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
        .assign_field => |assign| blk: {
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
            break :blk .{
                .body = try self.emitAssignRef(
                    try self.mapMirLocal(assign.target),
                    borrowSemantics(
                        try self.mapMirLocal(assign.source),
                        try self.singleProjectionSpan(.{ .field = @intCast(assign.field_idx) }),
                        self.current_borrow_region,
                    ),
                    .{ .field = .{
                        .source = try self.mapMirLocal(assign.source),
                        .field_idx = @intCast(assign.field_idx),
                    } },
                    lowered_next.body,
                ),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_tag_payload => |assign| blk: {
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
            break :blk .{
                .body = try self.emitAssignRef(
                    try self.mapMirLocal(assign.target),
                    borrowSemantics(
                        try self.mapMirLocal(assign.source),
                        try self.singleProjectionSpan(.tag_payload),
                        self.current_borrow_region,
                    ),
                    .{ .tag_payload = .{ .source = try self.mapMirLocal(assign.source) } },
                    lowered_next.body,
                ),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_nominal => |assign| blk: {
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
            break :blk .{
                .body = try self.emitAssignRef(
                    try self.mapMirLocal(assign.target),
                    aliasSemantics(
                        try self.mapMirLocal(assign.backing),
                        try self.singleProjectionSpan(.nominal),
                    ),
                    .{ .nominal = .{ .backing_ref = try self.mapMirLocal(assign.backing) } },
                    lowered_next.body,
                ),
                .result_contract = lowered_next.result_contract,
            };
        },
        .assign_str_escape_and_quote => |assign| blk: {
            const lowered_next = try self.lowerLinearEntrypointCallableBody(assign.next, callable, arg_locals, target, next);
            const source = try self.mapMirLocal(assign.source);
            break :blk .{
                .body = try self.lir_store.addCFStmt(.{ .assign_low_level = .{
                    .target = try self.mapMirLocal(assign.target),
                    .result = .fresh,
                    .op = .str_inspect,
                    .args = try self.lir_store.addLocalSpan(&.{source}),
                    .next = lowered_next.body,
                } }),
                .result_contract = lowered_next.result_contract,
            };
        },
        .debug => |debug_stmt| self.lowerLinearEntrypointCallableBody(debug_stmt.next, callable, arg_locals, target, next),
        .expect => |expect_stmt| self.lowerLinearEntrypointCallableBody(expect_stmt.next, callable, arg_locals, target, next),
        .ret => self.emitEntrypointCall(callable, arg_locals, target, next),
        .runtime_error,
        .match_stmt,
        .borrow_scope,
        .scope_exit,
        .join,
        .jump,
        .crash,
        => std.debug.panic(
            "MirToLir invariant violated: callable entrypoint wrapping currently requires a linear MIR body",
            .{},
        ),
    };
}

fn lowerEntrypointProcBody(
    self: *Self,
    root_const_id: MIR.ConstDefId,
    arg_locals: []const LocalRef,
    ret_layout: layout.Idx,
) Allocator.Error!LoweredEntrypointProcBody {
    const root_const = self.mir_store.getConstDef(root_const_id);
    const root_mono = self.mir_store.monotype_store.getMonotype(root_const.monotype);
    const must_call = arg_locals.len != 0 or root_mono == .func;

    if (!must_call) {
        return .{
            .body = try self.lowerRootBody(root_const.body),
            .result_contract = try self.lowerSummaryProcResultContract(
                self.analyses.getConstResultContract(root_const_id),
            ),
        };
    }

    const target = try self.freshLocal(ret_layout);
    const ret_stmt = try self.emitRet(target);
    const callable = try self.resolveEntrypointCallable(root_const.body);

    var local_map = MirToLirLocalMap.init(self.allocator);
    defer local_map.deinit();
    var stmt_map = MirToLirStmtMap.init(self.allocator);
    defer stmt_map.deinit();

    const prev_local_map = self.current_local_map;
    const prev_stmt_map = self.current_stmt_map;
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_stmt_map = &stmt_map;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_stmt_map = prev_stmt_map;
    defer self.current_borrow_region = prev_region;

    return try self.lowerLinearEntrypointCallableBody(root_const.body, callable, arg_locals, target, ret_stmt);
}

fn lowerLambda(self: *Self, lambda_id: MIR.LambdaId) Allocator.Error!LirProcSpecId {
    const proc_key = @as(u32, @intFromEnum(lambda_id));
    if (self.lowered_lambdas.get(proc_key)) |existing| return existing;

    const lambda = self.mir_store.getLambda(lambda_id);
    const ret_layout = try self.runtimeLambdaReturnLayout(lambda_id);
    const value_params = self.mir_store.getPatternSpan(lambda.params);
    const arg_count = value_params.len + @intFromBool(!lambda.captures_param.isNone());
    const args = try self.allocator.alloc(LocalRef, arg_count);
    defer self.allocator.free(args);

    for (value_params, 0..) |param_pattern, i| {
        args[i] = try self.freshLocal(try self.runtimeValueLayoutFromMirPattern(param_pattern));
    }
    if (!lambda.captures_param.isNone()) {
        args[value_params.len] = try self.freshLocal(try self.runtimeValueLayoutFromMirPattern(lambda.captures_param));
    }

    const arg_span = try self.lir_store.addLocalSpan(args);
    const result_contract = try self.lowerSummaryProcResultContract(
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
    const prev_region = self.current_borrow_region;
    self.current_local_map = &local_map;
    self.current_stmt_map = &stmt_map;
    self.current_borrow_region = .proc;
    defer self.current_local_map = prev_local_map;
    defer self.current_stmt_map = prev_stmt_map;
    defer self.current_borrow_region = prev_region;

    var body = try self.lowerStmt(lambda.body);
    const param_fail = try self.lir_store.addCFStmt(.{ .runtime_error = {} });

    if (!lambda.captures_param.isNone()) {
        body = try self.lowerPatternInto(
            args[value_params.len],
            self.mir_store.patternTypeOf(lambda.captures_param),
            lambda.captures_param,
            body,
            param_fail,
        );
    }

    var i = value_params.len;
    while (i > 0) {
        i -= 1;
        body = try self.lowerPatternInto(
            args[i],
            self.mir_store.patternTypeOf(value_params[i]),
            value_params[i],
            body,
            param_fail,
        );
    }

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
