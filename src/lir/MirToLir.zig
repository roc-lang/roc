//! MIR -> strongest-form statement-only LIR lowering.
//!
//! This pass targets the statement/control-flow IR directly:
//! - every MIR value is lowered into an explicit target local
//! - sequencing is represented only through `next` statement edges
//! - borrow scopes become first-class `CFStmt.borrow_scope` regions
//! - proc result contracts are instantiated as explicit result semantics

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
const LocalRef = LIR.LocalRef;
const LocalRefSpan = LIR.LocalRefSpan;
const ResultSemantics = LIR.ResultSemantics;
const RefOp = LIR.RefOp;
const SummaryContract = ProcResultSummary.ProcResultContract;
const ExprSummaryContract = ProcResultSummary.ExprResultContract;
const LoweredProcMap = std.AutoHashMap(u32, LirProcSpecId);
const LocalSymbolSet = std.AutoHashMapUnmanaged(u64, void);
const dec_literal_scale: i128 = std.math.pow(i128, 10, 18);

const BuilderProcHeader = struct {
    name: LIR.Symbol,
    args: LIR.LocalRefSpan,
    ret_layout: layout.Idx,
    result_contract: LIR.ProcResultContract,
    hosted: ?LIR.HostedProc,
};

const BuilderProc = union(enum) {
    unresolved: BuilderProcHeader,
    resolved: LirProcSpec,
};

const Self = @This();

allocator: Allocator,
mir_store: *const MIR.Store,
lir_store: *LirStore,
layout_store: *layout.Store,
mir_layout_resolver: layout.MirMonotypeLayoutResolver,
analyses: *const Analyses,
lowered_procs: LoweredProcMap,
builder_procs: std.ArrayList(BuilderProc),
active_value_defs: LocalSymbolSet,
current_local_symbols: ?*const LocalSymbolSet = null,
flushed: bool = false,
current_proc_result_summary: ?SummaryContract = null,

next_borrow_scope: u32 = 0,
current_borrow_region: LIR.BorrowRegion = .proc,
next_join_point: u32 = 0,
break_targets: std.ArrayList(JoinPointId) = .empty,

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
        .lowered_procs = LoweredProcMap.init(allocator),
        .builder_procs = std.ArrayList(BuilderProc).empty,
        .active_value_defs = .empty,
        .break_targets = .empty,
    };
}

/// Releases temporary lowering state owned by this translator.
pub fn deinit(self: *Self) void {
    self.lowered_procs.deinit();
    self.builder_procs.deinit(self.allocator);
    self.active_value_defs.deinit(self.allocator);
    self.break_targets.deinit(self.allocator);
    self.mir_layout_resolver.deinit();
}

/// Lowers a root MIR expression into a zero-argument LIR proc and flushes all lowered procs.
pub fn lower(self: *Self, mir_expr_id: MIR.ExprId) Allocator.Error!LirProcSpecId {
    self.ensureCanLowerMoreProcs();

    const ret_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const summary_contract = self.analyses.getRootResultContract(mir_expr_id);
    const body = try self.lowerEntrypointBody(mir_expr_id, ret_layout, summary_contract);
    const args = LIR.LocalRefSpan.empty();
    const result_contract = try self.lowerSummaryProcResultContract(summary_contract);

    if (builtin.mode == .Debug) {
        // This verifier exists only to catch compiler implementation bugs by
        // re-scanning already-lowered LIR. It must remain debug-only because
        // release compiler builds must not pay for extra full-LIR verification.
        try DebugVerifyLir.verifyProc(
            self.allocator,
            self.lir_store,
            args,
            result_contract,
            body,
        );
    }

    const proc = LirProcSpec{
        .name = self.lir_store.freshSyntheticSymbol(),
        .args = args,
        .body = body,
        .ret_layout = ret_layout,
        .result_contract = result_contract,
    };
    const root_proc_id = try self.addResolvedProc(proc);
    try self.flush();
    return root_proc_id;
}

/// Lowers an entrypoint MIR expression into an explicit-argument LIR proc.
pub fn lowerEntrypointProc(
    self: *Self,
    mir_expr_id: MIR.ExprId,
    arg_layouts: []const layout.Idx,
    ret_layout: layout.Idx,
) Allocator.Error!LirProcSpecId {
    self.ensureCanLowerMoreProcs();

    const args = try self.allocator.alloc(LocalRef, arg_layouts.len);
    defer self.allocator.free(args);

    for (arg_layouts, 0..) |arg_layout, i| {
        args[i] = self.freshLocal(arg_layout);
    }

    const arg_span = try self.lir_store.addLocalRefs(args);
    const lowered = try self.lowerEntrypointProcBody(mir_expr_id, args, ret_layout);

    if (builtin.mode == .Debug) {
        // This verifier exists only to catch compiler implementation bugs by
        // re-scanning already-lowered LIR. It must remain debug-only because
        // release compiler builds must not pay for extra full-LIR verification.
        try DebugVerifyLir.verifyProc(
            self.allocator,
            self.lir_store,
            arg_span,
            lowered.result_contract,
            lowered.body,
        );
    }

    return self.addResolvedProc(.{
        .name = self.lir_store.freshSyntheticSymbol(),
        .args = arg_span,
        .body = lowered.body,
        .ret_layout = ret_layout,
        .result_contract = lowered.result_contract,
        .hosted = null,
    });
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

fn freshBorrowScope(self: *Self) LIR.BorrowScopeId {
    defer self.next_borrow_scope += 1;
    return @enumFromInt(self.next_borrow_scope);
}

fn freshJoinPoint(self: *Self) JoinPointId {
    defer self.next_join_point += 1;
    return @enumFromInt(self.next_join_point);
}

fn freshLocal(self: *Self, layout_idx: layout.Idx) LocalRef {
    return .{
        .symbol = self.lir_store.freshSyntheticSymbol(),
        .layout_idx = layout_idx,
    };
}

fn runtimeValueLayoutFromMirExpr(self: *Self, mir_expr_id: MIR.ExprId) Allocator.Error!layout.Idx {
    return self.mir_layout_resolver.resolve(self.mir_store.typeOf(mir_expr_id), null);
}

fn runtimeValueLayoutFromMirPattern(self: *Self, pattern_id: MIR.PatternId) Allocator.Error!layout.Idx {
    return self.mir_layout_resolver.resolve(self.mir_store.patternTypeOf(pattern_id), null);
}

fn runtimeValueLayoutFromMirMonotype(self: *Self, monotype: MirMonotypeIdx) Allocator.Error!layout.Idx {
    return self.mir_layout_resolver.resolve(monotype, null);
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

fn layoutNeedsRc(self: *const Self, layout_idx: layout.Idx) bool {
    return self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(layout_idx));
}

fn summaryContractsEqual(self: *const Self, left: SummaryContract, right: SummaryContract) bool {
    return switch (left) {
        .fresh => right == .fresh,
        .alias_of_param => |left_param| switch (right) {
            .alias_of_param => |right_param| left_param.param_index == right_param.param_index and std.meta.eql(self.analyses.getRefProjectionSpan(left_param.projections), self.analyses.getRefProjectionSpan(right_param.projections)),
            else => false,
        },
        .borrow_of_param => |left_param| switch (right) {
            .borrow_of_param => |right_param| left_param.param_index == right_param.param_index and std.meta.eql(self.analyses.getRefProjectionSpan(left_param.projections), self.analyses.getRefProjectionSpan(right_param.projections)),
            else => false,
        },
    };
}

fn exprSummaryContractsEqual(self: *const Self, left: ExprSummaryContract, right: ExprSummaryContract) bool {
    return switch (left) {
        .no_return => right == .no_return,
        .borrow_of_fresh => right == .borrow_of_fresh,
        .concrete => |left_contract| switch (right) {
            .concrete => |right_contract| self.summaryContractsEqual(left_contract, right_contract),
            else => false,
        },
    };
}

fn lowerExprIntoStmtWithContract(
    self: *Self,
    mir_expr_id: MIR.ExprId,
    target: LocalRef,
    desired_contract: ExprSummaryContract,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const actual_contract = self.analyses.getExprResultContract(mir_expr_id);
    if (actual_contract == .no_return) {
        return self.lowerExprIntoStmt(mir_expr_id, target, next);
    }
    if (self.exprSummaryContractsEqual(actual_contract, desired_contract)) {
        return self.lowerExprIntoStmt(mir_expr_id, target, next);
    }

    if (desired_contract == .no_return) {
        std.debug.panic(
            "MirToLir invariant violated: value-position expr {d} cannot be normalized to no-return",
            .{@intFromEnum(mir_expr_id)},
        );
    }

    switch (desired_contract) {
        .concrete => |desired_proc_contract| if (desired_proc_contract != .fresh) {
            std.debug.panic(
                "MirToLir invariant violated: strongest-form lowering cannot normalize expr {d} from {s} to {s}",
                .{ @intFromEnum(mir_expr_id), @tagName(actual_contract), @tagName(desired_contract) },
            );
        },
        else => std.debug.panic(
            "MirToLir invariant violated: strongest-form lowering cannot normalize expr {d} from {s} to {s}",
            .{ @intFromEnum(mir_expr_id), @tagName(actual_contract), @tagName(desired_contract) },
        ),
    }

    const value_local = self.freshLocal(target.layout_idx);
    var normalized_next = try self.emitAssignRef(
        target,
        .fresh,
        .{ .local = value_local },
        next,
    );
    if (self.layoutNeedsRc(target.layout_idx)) {
        normalized_next = try self.lir_store.addCFStmt(.{ .incref = .{
            .value = value_local,
            .count = 1,
            .next = normalized_next,
        } });
    }
    return self.lowerExprIntoStmt(mir_expr_id, value_local, normalized_next);
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
        .requires_explicit_summary => blk: {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "MirToLir invariant violated: low-level result {s} must be explicitly classified",
                    .{@tagName(op)},
                );
            }
            break :blk .fresh;
        },
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

fn getProcResultContract(self: *const Self, proc_id: LirProcSpecId) LIR.ProcResultContract {
    return switch (self.builder_procs.items[@intFromEnum(proc_id)]) {
        .resolved => |proc| proc.result_contract,
        .unresolved => |header| header.result_contract,
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

fn emitAssignRef(
    self: *Self,
    target: LocalRef,
    result: ResultSemantics,
    op: RefOp,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
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

fn lowerProcBackedValueInto(
    self: *Self,
    mir_expr_id: MIR.ExprId,
    target: LocalRef,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    return switch (self.mir_store.getExpr(mir_expr_id)) {
        .proc_ref => |_| self.emitAssignUnit(target, next),
        .closure_make => |closure| blk: {
            const captures_expr = self.mir_store.getExpr(closure.captures);
            const captures_fields = switch (captures_expr) {
                .struct_ => |struct_| self.mir_store.getExprSpan(struct_.fields),
                else => std.debug.panic(
                    "MirToLir invariant violated: closure captures expr {d} must be a tuple/struct constructor before strongest-form lowering",
                    .{@intFromEnum(closure.captures)},
                ),
            };

            const refs = try self.allocator.alloc(LocalRef, captures_fields.len);
            defer self.allocator.free(refs);
            for (captures_fields, 0..) |field_expr, i| {
                refs[i] = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(field_expr));
            }
            const ref_span = try self.lir_store.addLocalRefs(refs);
            const assign_stmt = try self.lir_store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .result = .fresh,
                .fields = ref_span,
                .next = next,
            } });
            var entry = assign_stmt;
            var i = captures_fields.len;
            while (i > 0) {
                i -= 1;
                entry = try self.lowerExprIntoStmt(captures_fields[i], refs[i], entry);
            }
            break :blk entry;
        },
        .dbg_expr => |dbg_expr| self.lowerProcBackedValueInto(dbg_expr.expr, target, next),
        .expect => |expect| self.lowerProcBackedValueInto(expect.body, target, next),
        .return_expr => |ret| self.lowerProcBackedValueInto(ret.expr, target, next),
        .block => |block| self.lowerBlockInto(block, target, next),
        else => std.debug.panic(
            "MirToLir invariant violated: proc-backed value lowering received non-callable MIR expr {s}",
            .{@tagName(self.mir_store.getExpr(mir_expr_id))},
        ),
    };
}

fn freshHiddenCaptureLocal(self: *Self, mir_proc_id: MIR.ProcId) Allocator.Error!?LocalRef {
    const mir_proc = self.mir_store.getProc(mir_proc_id);
    if (mir_proc.captures_param.isNone()) return null;
    return self.freshLocal(try self.runtimeValueLayoutFromMirPattern(mir_proc.captures_param));
}

fn lowerHiddenCaptureArg(
    self: *Self,
    callee_expr_id: MIR.ExprId,
    hidden_capture_local: LocalRef,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const callable_local = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(callee_expr_id));
    const bind_capture = try self.emitAssignRef(
        hidden_capture_local,
        aliasSemantics(callable_local, LIR.RefProjectionSpan.empty()),
        .{ .local = callable_local },
        next,
    );
    return self.lowerExprIntoStmt(callee_expr_id, callable_local, bind_capture);
}

const LoweredCallee = struct {
    mir_proc_id: MIR.ProcId,
    lir_proc_id: LirProcSpecId,
};

fn emitRet(self: *Self, value: LocalRef) Allocator.Error!CFStmtId {
    return self.lir_store.addCFStmt(.{ .ret = .{ .value = value } });
}

const LoweredEntrypointProcBody = struct {
    body: CFStmtId,
    result_contract: LIR.ProcResultContract,
};

fn lowerEntrypointBody(
    self: *Self,
    mir_expr_id: MIR.ExprId,
    ret_layout: layout.Idx,
    result_contract: SummaryContract,
) Allocator.Error!CFStmtId {
    var local_symbols = LocalSymbolSet{};
    defer local_symbols.deinit(self.allocator);

    const prev_local_symbols = self.current_local_symbols;
    const prev_result_contract = self.current_proc_result_summary;
    self.current_local_symbols = &local_symbols;
    self.current_proc_result_summary = result_contract;
    defer self.current_local_symbols = prev_local_symbols;
    defer self.current_proc_result_summary = prev_result_contract;

    const target = self.freshLocal(ret_layout);
    const ret_stmt = try self.emitRet(target);
    return self.lowerExprIntoStmtWithContract(mir_expr_id, target, .{ .concrete = result_contract }, ret_stmt);
}

fn lowerEntrypointProcBody(
    self: *Self,
    mir_expr_id: MIR.ExprId,
    arg_locals: []const LocalRef,
    ret_layout: layout.Idx,
) Allocator.Error!LoweredEntrypointProcBody {
    var local_symbols = LocalSymbolSet{};
    defer local_symbols.deinit(self.allocator);

    const prev_local_symbols = self.current_local_symbols;
    self.current_local_symbols = &local_symbols;
    defer self.current_local_symbols = prev_local_symbols;

    const mir_mono = self.mir_store.monotype_store.getMonotype(self.mir_store.typeOf(mir_expr_id));
    const must_call = arg_locals.len != 0 or mir_mono == .func;

    const root_result_contract = self.analyses.getRootResultContract(mir_expr_id);

    if (!must_call) {
        return .{
            .body = try self.lowerEntrypointBody(mir_expr_id, ret_layout, root_result_contract),
            .result_contract = try self.lowerSummaryProcResultContract(root_result_contract),
        };
    }

    const target = self.freshLocal(ret_layout);
    const ret_stmt = try self.emitRet(target);
    return self.lowerEntrypointCallableInto(mir_expr_id, arg_locals, target, ret_stmt);
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
    proc_id: LirProcSpecId,
    visible_args: []const LocalRef,
    hidden_capture: ?LocalRef,
    target: LocalRef,
    next: CFStmtId,
) Allocator.Error!LoweredEntrypointProcBody {
    const callee_ret_layout = self.loweredProcRetLayout(proc_id);
    if (callee_ret_layout != target.layout_idx) {
        std.debug.panic(
            "MirToLir invariant violated: entrypoint wrapper result layout does not match callee result layout",
            .{},
        );
    }

    const has_capture = hidden_capture != null;
    const call_arg_count = visible_args.len + @intFromBool(has_capture);
    const call_args = try self.allocator.alloc(LocalRef, call_arg_count);
    defer self.allocator.free(call_args);

    @memcpy(call_args[0..visible_args.len], visible_args);
    if (hidden_capture) |capture_local| {
        call_args[visible_args.len] = capture_local;
    }

    const arg_span = try self.lir_store.addLocalRefs(call_args);
    return .{
        .body = try self.lir_store.addCFStmt(.{ .assign_call = .{
            .target = target,
            .result = self.callResultSemantics(proc_id, call_args),
            .proc = proc_id,
            .args = arg_span,
            .next = next,
        } }),
        .result_contract = self.lowerEntrypointCallResultContract(proc_id, visible_args.len),
    };
}

fn lowerEntrypointCallableInto(
    self: *Self,
    mir_expr_id: MIR.ExprId,
    arg_locals: []const LocalRef,
    target: LocalRef,
    next: CFStmtId,
) Allocator.Error!LoweredEntrypointProcBody {
    return switch (self.mir_store.getExpr(mir_expr_id)) {
        .proc_ref => |proc_id| self.emitEntrypointCall(
            try self.lowerProc(proc_id),
            arg_locals,
            null,
            target,
            next,
        ),
        .closure_make => |closure| blk: {
            const lowered_proc_id = try self.lowerProc(closure.proc);
            const captures_local = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(closure.captures));
            const emitted = try self.emitEntrypointCall(
                lowered_proc_id,
                arg_locals,
                captures_local,
                target,
                next,
            );
            break :blk .{
                .body = try self.lowerExprIntoStmt(closure.captures, captures_local, emitted.body),
                .result_contract = emitted.result_contract,
            };
        },
        .dbg_expr => |dbg_expr| self.lowerEntrypointCallableInto(dbg_expr.expr, arg_locals, target, next),
        .expect => |expect| self.lowerEntrypointCallableInto(expect.body, arg_locals, target, next),
        .return_expr => |ret| self.lowerEntrypointCallableInto(ret.expr, arg_locals, target, next),
        .block => |block| blk: {
            var lowered = try self.lowerEntrypointCallableInto(block.final_expr, arg_locals, target, next);
            const stmts = self.mir_store.getStmts(block.stmts);
            var i = stmts.len;
            while (i > 0) {
                i -= 1;
                lowered.body = try self.lowerStmtInto(stmts[i], lowered.body);
            }
            break :blk lowered;
        },
        else => std.debug.panic(
            "MirToLir invariant violated: entrypoint wrapper requires a proc-backed MIR callable",
            .{},
        ),
    };
}

fn tagDiscriminantForExpr(
    self: *Self,
    mir_expr_id: MIR.ExprId,
    tag_name: base.Ident.Idx,
) u16 {
    const mono = self.mir_store.monotype_store.getMonotype(self.mir_store.typeOf(mir_expr_id));
    const tags = switch (mono) {
        .tag_union => |tag_union| self.mir_store.monotype_store.getTags(tag_union.tags),
        else => std.debug.panic(
            "MirToLir invariant violated: tag expr does not have a tag-union monotype",
            .{},
        ),
    };

    for (tags, 0..) |tag, index| {
        if (tag.name.ident.eql(tag_name)) return @intCast(index);
    }

    std.debug.panic(
        "MirToLir invariant violated: tag constructor missing from result monotype",
        .{},
    );
}

fn lowerBorrowBindingPattern(
    self: *Self,
    pattern_id: MIR.PatternId,
    source: LocalRef,
    scope_id: LIR.BorrowScopeId,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    return switch (self.mir_store.getPattern(pattern_id)) {
        .bind => |symbol| self.emitAssignRef(
            .{
                .symbol = symbol,
                .layout_idx = try self.runtimeValueLayoutFromMirPattern(pattern_id),
            },
            borrowSemantics(source, LIR.RefProjectionSpan.empty(), .{ .scope = scope_id }),
            .{ .local = source },
            next,
        ),
        .wildcard => next,
        .as_pattern => |as_pattern| blk: {
            const inner = try self.lowerBorrowBindingPattern(as_pattern.pattern, source, scope_id, next);
            break :blk try self.emitAssignRef(
                .{
                    .symbol = as_pattern.symbol,
                    .layout_idx = try self.runtimeValueLayoutFromMirPattern(pattern_id),
                },
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

fn lowerBorrowBinding(
    self: *Self,
    binding: MIR.BorrowBinding,
    scope_id: LIR.BorrowScopeId,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const source = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(binding.expr));
    const body = try self.lowerBorrowBindingPattern(binding.pattern, source, scope_id, next);
    return self.lowerExprIntoStmt(binding.expr, source, body);
}

fn lowerStmtBinding(self: *Self, binding: MIR.Stmt.Binding, next: CFStmtId) Allocator.Error!CFStmtId {
    return switch (self.mir_store.getPattern(binding.pattern)) {
        .bind => |symbol| self.lowerExprIntoStmt(
            binding.expr,
            .{
                .symbol = symbol,
                .layout_idx = try self.runtimeValueLayoutFromMirPattern(binding.pattern),
            },
            next,
        ),
        .wildcard => self.lowerExprIntoStmt(
            binding.expr,
            self.freshLocal(try self.runtimeValueLayoutFromMirExpr(binding.expr)),
            next,
        ),
        else => std.debug.panic(
            "MirToLir invariant violated: block binding pattern {s} must be lowered before statement-only LIR",
            .{@tagName(self.mir_store.getPattern(binding.pattern))},
        ),
    };
}

fn lowerStmtInto(self: *Self, stmt: MIR.Stmt, next: CFStmtId) Allocator.Error!CFStmtId {
    return switch (stmt) {
        .decl_const => |binding| self.lowerStmtBinding(binding, next),
        .decl_var => |binding| self.lowerStmtBinding(binding, next),
        .mutate_var => |binding| self.lowerStmtBinding(binding, next),
    };
}

fn rememberStmtBoundLocals(
    self: *Self,
    local_symbols: *LocalSymbolSet,
    stmt: MIR.Stmt,
) Allocator.Error!void {
    switch (stmt) {
        .decl_const => |binding| try self.collectPatternLocalSymbols(local_symbols, binding.pattern),
        .decl_var => |binding| try self.collectPatternLocalSymbols(local_symbols, binding.pattern),
        .mutate_var => {},
    }
}

fn removePatternLocalSymbols(
    self: *Self,
    local_symbols: *LocalSymbolSet,
    pattern_id: MIR.PatternId,
) void {
    switch (self.mir_store.getPattern(pattern_id)) {
        .bind => |symbol| _ = local_symbols.remove(symbol.raw()),
        .wildcard,
        .int_literal,
        .str_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .runtime_error,
        => {},
        .tag => |tag| {
            for (self.mir_store.getPatternSpan(tag.args)) |arg_pattern| {
                self.removePatternLocalSymbols(local_symbols, arg_pattern);
            }
        },
        .struct_destructure => |destructure| {
            for (self.mir_store.getPatternSpan(destructure.fields)) |field_pattern| {
                self.removePatternLocalSymbols(local_symbols, field_pattern);
            }
        },
        .list_destructure => |destructure| {
            for (self.mir_store.getPatternSpan(destructure.patterns)) |elem_pattern| {
                self.removePatternLocalSymbols(local_symbols, elem_pattern);
            }
            if (!destructure.rest_pattern.isNone()) {
                self.removePatternLocalSymbols(local_symbols, destructure.rest_pattern);
            }
        },
        .as_pattern => |as_pattern| {
            _ = local_symbols.remove(as_pattern.symbol.raw());
            self.removePatternLocalSymbols(local_symbols, as_pattern.pattern);
        },
    }
}

fn removeStmtBoundLocals(
    self: *Self,
    local_symbols: *LocalSymbolSet,
    stmt: MIR.Stmt,
) void {
    switch (stmt) {
        .decl_const => |binding| self.removePatternLocalSymbols(local_symbols, binding.pattern),
        .decl_var => |binding| self.removePatternLocalSymbols(local_symbols, binding.pattern),
        .mutate_var => {},
    }
}

fn lowerBlockInto(
    self: *Self,
    block: anytype,
    target: LocalRef,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const stmts = self.mir_store.getStmts(block.stmts);
    var block_scope = if (self.current_local_symbols) |locals|
        try self.cloneLocalSymbolSet(locals)
    else
        LocalSymbolSet{};
    defer block_scope.deinit(self.allocator);

    for (stmts) |stmt| {
        try self.rememberStmtBoundLocals(&block_scope, stmt);
    }

    const prev_local_symbols = self.current_local_symbols;
    self.current_local_symbols = &block_scope;
    defer self.current_local_symbols = prev_local_symbols;

    var entry = try self.lowerExprIntoStmt(block.final_expr, target, next);
    var i = stmts.len;
    while (i > 0) {
        i -= 1;
        self.removeStmtBoundLocals(&block_scope, stmts[i]);
        entry = try self.lowerStmtInto(stmts[i], entry);
    }
    return entry;
}

fn collectPatternLocalSymbols(
    self: *Self,
    local_symbols: *LocalSymbolSet,
    pattern_id: MIR.PatternId,
) Allocator.Error!void {
    switch (self.mir_store.getPattern(pattern_id)) {
        .bind => |symbol| try local_symbols.put(self.allocator, symbol.raw(), {}),
        .wildcard,
        .int_literal,
        .str_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .runtime_error,
        => {},
        .tag => |tag| {
            for (self.mir_store.getPatternSpan(tag.args)) |arg_pattern| {
                try self.collectPatternLocalSymbols(local_symbols, arg_pattern);
            }
        },
        .struct_destructure => |destructure| {
            for (self.mir_store.getPatternSpan(destructure.fields)) |field_pattern| {
                try self.collectPatternLocalSymbols(local_symbols, field_pattern);
            }
        },
        .list_destructure => |destructure| {
            for (self.mir_store.getPatternSpan(destructure.patterns)) |elem_pattern| {
                try self.collectPatternLocalSymbols(local_symbols, elem_pattern);
            }
            if (!destructure.rest_pattern.isNone()) {
                try self.collectPatternLocalSymbols(local_symbols, destructure.rest_pattern);
            }
        },
        .as_pattern => |as_pattern| {
            try local_symbols.put(self.allocator, as_pattern.symbol.raw(), {});
            try self.collectPatternLocalSymbols(local_symbols, as_pattern.pattern);
        },
    }
}

fn cloneLocalSymbolSet(self: *Self, source: *const LocalSymbolSet) Allocator.Error!LocalSymbolSet {
    var clone = LocalSymbolSet{};
    var it = source.iterator();
    while (it.next()) |entry| {
        try clone.put(self.allocator, entry.key_ptr.*, {});
    }
    return clone;
}

const LoopCarrierCollector = struct {
    lowerer: *Self,
    carriers: std.ArrayList(LocalRef),
    carrier_indices: std.AutoHashMapUnmanaged(u64, usize) = .empty,

    fn init(lowerer: *Self) LoopCarrierCollector {
        return .{
            .lowerer = lowerer,
            .carriers = std.ArrayList(LocalRef).empty,
        };
    }

    fn deinit(self: *LoopCarrierCollector) void {
        self.carriers.deinit(self.lowerer.allocator);
        self.carrier_indices.deinit(self.lowerer.allocator);
    }

    fn rememberPatternLocals(
        self: *LoopCarrierCollector,
        local_scope: *LocalSymbolSet,
        pattern_id: MIR.PatternId,
    ) Allocator.Error!void {
        switch (self.lowerer.mir_store.getPattern(pattern_id)) {
            .bind => |symbol| try local_scope.put(self.lowerer.allocator, symbol.raw(), {}),
            .wildcard,
            .int_literal,
            .str_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .runtime_error,
            => {},
            .tag => |tag| {
                for (self.lowerer.mir_store.getPatternSpan(tag.args)) |arg_pattern| {
                    try self.rememberPatternLocals(local_scope, arg_pattern);
                }
            },
            .struct_destructure => |destructure| {
                for (self.lowerer.mir_store.getPatternSpan(destructure.fields)) |field_pattern| {
                    try self.rememberPatternLocals(local_scope, field_pattern);
                }
            },
            .list_destructure => |destructure| {
                for (self.lowerer.mir_store.getPatternSpan(destructure.patterns)) |elem_pattern| {
                    try self.rememberPatternLocals(local_scope, elem_pattern);
                }
                if (!destructure.rest_pattern.isNone()) {
                    try self.rememberPatternLocals(local_scope, destructure.rest_pattern);
                }
            },
            .as_pattern => |as_pattern| {
                try local_scope.put(self.lowerer.allocator, as_pattern.symbol.raw(), {});
                try self.rememberPatternLocals(local_scope, as_pattern.pattern);
            },
        }
    }

    fn rememberCarrierSymbol(
        self: *LoopCarrierCollector,
        symbol: MIR.Symbol,
        layout_idx: layout.Idx,
    ) Allocator.Error!void {
        const symbol_key = symbol.raw();
        if (self.carrier_indices.get(symbol_key)) |existing_index| {
            const existing = self.carriers.items[existing_index];
            if (existing.layout_idx != layout_idx) {
                std.debug.panic(
                    "MirToLir invariant violated: loop-carried symbol {d} was observed with incompatible layouts {d} and {d}",
                    .{ symbol_key, @intFromEnum(existing.layout_idx), @intFromEnum(layout_idx) },
                );
            }
            return;
        }

        try self.carrier_indices.put(self.lowerer.allocator, symbol_key, self.carriers.items.len);
        try self.carriers.append(self.lowerer.allocator, .{
            .symbol = symbol,
            .layout_idx = layout_idx,
        });
    }

    fn collectStmt(self: *LoopCarrierCollector, stmt: MIR.Stmt, local_scope: *LocalSymbolSet) Allocator.Error!void {
        switch (stmt) {
            .decl_const => |binding| {
                try self.collectExpr(binding.expr, local_scope);
                try self.rememberPatternLocals(local_scope, binding.pattern);
            },
            .decl_var => |binding| {
                try self.collectExpr(binding.expr, local_scope);
                try self.rememberPatternLocals(local_scope, binding.pattern);
            },
            .mutate_var => |binding| {
                try self.collectExpr(binding.expr, local_scope);
            },
        }
    }

    fn collectExpr(
        self: *LoopCarrierCollector,
        expr_id: MIR.ExprId,
        local_scope: *LocalSymbolSet,
    ) Allocator.Error!void {
        switch (self.lowerer.mir_store.getExpr(expr_id)) {
            .int,
            .frac_f32,
            .frac_f64,
            .dec,
            .str,
            .proc_ref,
            .runtime_err_can,
            .runtime_err_type,
            .runtime_err_ellipsis,
            .runtime_err_anno_only,
            .crash,
            .break_expr,
            => {},
            .lookup => |symbol| {
                if (!local_scope.contains(symbol.raw()) and self.lowerer.mir_store.isSymbolReassignable(symbol)) {
                    try self.rememberCarrierSymbol(symbol, try self.lowerer.runtimeValueLayoutFromMirExpr(expr_id));
                }
            },
            .closure_make => |closure| try self.collectExpr(closure.captures, local_scope),
            .list => |list_data| {
                for (self.lowerer.mir_store.getExprSpan(list_data.elems)) |elem_expr| {
                    try self.collectExpr(elem_expr, local_scope);
                }
            },
            .struct_ => |struct_data| {
                for (self.lowerer.mir_store.getExprSpan(struct_data.fields)) |field_expr| {
                    try self.collectExpr(field_expr, local_scope);
                }
            },
            .tag => |tag_expr| {
                for (self.lowerer.mir_store.getExprSpan(tag_expr.args)) |arg_expr| {
                    try self.collectExpr(arg_expr, local_scope);
                }
            },
            .match_expr => |match_expr| {
                try self.collectExpr(match_expr.cond, local_scope);
                for (self.lowerer.mir_store.getBranches(match_expr.branches)) |branch| {
                    var branch_scope = try self.lowerer.cloneLocalSymbolSet(local_scope);
                    defer branch_scope.deinit(self.lowerer.allocator);

                    for (self.lowerer.mir_store.getBranchPatterns(branch.patterns)) |branch_pattern| {
                        try self.rememberPatternLocals(&branch_scope, branch_pattern.pattern);
                    }
                    if (!branch.guard.isNone()) {
                        try self.collectExpr(branch.guard, &branch_scope);
                    }
                    try self.collectExpr(branch.body, &branch_scope);
                }
            },
            .call => |call| {
                try self.collectExpr(call.func, local_scope);
                for (self.lowerer.mir_store.getExprSpan(call.args)) |arg_expr| {
                    try self.collectExpr(arg_expr, local_scope);
                }
            },
            .block => |block| {
                var block_scope = try self.lowerer.cloneLocalSymbolSet(local_scope);
                defer block_scope.deinit(self.lowerer.allocator);

                for (self.lowerer.mir_store.getStmts(block.stmts)) |stmt| {
                    try self.collectStmt(stmt, &block_scope);
                }
                try self.collectExpr(block.final_expr, &block_scope);
            },
            .borrow_scope => |scope| {
                var scope_locals = try self.lowerer.cloneLocalSymbolSet(local_scope);
                defer scope_locals.deinit(self.lowerer.allocator);

                for (self.lowerer.mir_store.getBorrowBindings(scope.bindings)) |binding| {
                    try self.collectExpr(binding.expr, &scope_locals);
                    try self.rememberPatternLocals(&scope_locals, binding.pattern);
                }
                try self.collectExpr(scope.body, &scope_locals);
            },
            .struct_access => |access| try self.collectExpr(access.struct_, local_scope),
            .str_escape_and_quote => |inner| try self.collectExpr(inner, local_scope),
            .run_low_level => |ll| {
                for (self.lowerer.mir_store.getExprSpan(ll.args)) |arg_expr| {
                    try self.collectExpr(arg_expr, local_scope);
                }
            },
            .dbg_expr => |dbg_expr| try self.collectExpr(dbg_expr.expr, local_scope),
            .expect => |expect| try self.collectExpr(expect.body, local_scope),
            .loop => |loop_expr| try self.collectExpr(loop_expr.body, local_scope),
            .return_expr => |ret| try self.collectExpr(ret.expr, local_scope),
        }
    }
};

fn collectLoopCarrierLocals(self: *Self, body_expr_id: MIR.ExprId) Allocator.Error!LocalRefSpan {
    var collector = LoopCarrierCollector.init(self);
    defer collector.deinit();

    var local_scope = LocalSymbolSet{};
    defer local_scope.deinit(self.allocator);

    try collector.collectExpr(body_expr_id, &local_scope);
    return self.lir_store.addLocalRefs(collector.carriers.items);
}

fn isCurrentLocalSymbol(self: *const Self, symbol: MIR.Symbol) bool {
    return if (self.current_local_symbols) |locals|
        locals.contains(symbol.raw())
    else
        false;
}

fn lowerNonLocalLookupInto(
    self: *Self,
    symbol: MIR.Symbol,
    target: LocalRef,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const def_expr = self.mir_store.getValueDef(symbol) orelse std.debug.panic(
        "MirToLir invariant violated: non-local lookup symbol {d} has no MIR value definition",
        .{symbol.raw()},
    );

    const gop = try self.active_value_defs.getOrPut(self.allocator, symbol.raw());
    if (gop.found_existing) {
        std.debug.panic(
            "MirToLir invariant violated: cyclic MIR value definition for symbol {d}",
            .{symbol.raw()},
        );
    }
    defer _ = self.active_value_defs.remove(symbol.raw());

    return self.lowerExprIntoStmt(def_expr, target, next);
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
    tag_name: base.Ident.Idx,
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
        if (!tag.name.ident.eql(tag_name)) continue;

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

fn mirIntLiteralValue(layout_idx: layout.Idx, int_value: @import("can").CIR.IntValue) LiteralValue {
    return switch (layout_idx) {
        .u128, .i128 => .{ .i128_literal = .{
            .value = @bitCast(int_value.bytes),
            .layout_idx = layout_idx,
        } },
        .dec => blk: {
            const scaled = @mulWithOverflow(mirIntValueAsI128(int_value), dec_literal_scale);
            if (scaled[1] != 0) {
                std.debug.panic(
                    "MirToLir invariant violated: integer literal overflowed Dec representation during strongest-form lowering",
                    .{},
                );
            }
            break :blk .{ .dec_literal = scaled[0] };
        },
        .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64 => .{ .i64_literal = .{
            .value = @intCast(mirIntValueAsI128(int_value)),
            .layout_idx = layout_idx,
        } },
        else => std.debug.panic(
            "MirToLir invariant violated: integer literal lowered to non-integer layout {s}",
            .{@tagName(layout_idx)},
        ),
    };
}

fn internMirStringLiteral(
    self: *Self,
    mir_idx: base.StringLiteral.Idx,
) Allocator.Error!base.StringLiteral.Idx {
    return self.lir_store.insertString(self.mir_store.getString(mir_idx));
}

fn literalPatternValue(
    self: *Self,
    pattern_id: MIR.PatternId,
    target_layout: layout.Idx,
) Allocator.Error!LiteralValue {
    return switch (self.mir_store.getPattern(pattern_id)) {
        .int_literal => |int_lit| mirIntLiteralValue(target_layout, int_lit.value),
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
    const literal_local = self.freshLocal(source.layout_idx);
    const eq_local = self.freshLocal(.bool);
    const eq_args = try self.lir_store.addLocalRefs(&.{ source, literal_local });
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
        try self.literalPatternValue(pattern_id, source.layout_idx),
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
        .bind => |symbol| self.emitAssignRef(
            .{
                .symbol = symbol,
                .layout_idx = source.layout_idx,
            },
            aliasSemantics(source, LIR.RefProjectionSpan.empty()),
            .{ .local = source },
            on_match,
        ),
        .wildcard => on_match,
        .as_pattern => |as_pattern| blk: {
            const inner = try self.lowerPatternInto(source, source_mono, as_pattern.pattern, on_match, on_fail);
            break :blk try self.emitAssignRef(
                .{
                    .symbol = as_pattern.symbol,
                    .layout_idx = source.layout_idx,
                },
                aliasSemantics(source, LIR.RefProjectionSpan.empty()),
                .{ .local = source },
                inner,
            );
        },
        .tag => |tag_pattern| blk: {
            const discr_local = self.freshLocal(.u32);
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
                const payload_local = self.freshLocal(try self.runtimeValueLayoutFromMirMonotype(payload_mono));
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
                const field_local = self.freshLocal(try self.runtimeValueLayoutFromMirMonotype(field_mono));
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

fn lowerMatchBranchAlternative(
    self: *Self,
    source: LocalRef,
    source_mono: MirMonotypeIdx,
    branch: MIR.Branch,
    pattern_id: MIR.PatternId,
    expr_result_contract: ExprSummaryContract,
    target: LocalRef,
    next: CFStmtId,
    on_fail: CFStmtId,
) Allocator.Error!CFStmtId {
    var branch_scope = if (self.current_local_symbols) |locals|
        try self.cloneLocalSymbolSet(locals)
    else
        LocalSymbolSet{};
    defer branch_scope.deinit(self.allocator);
    try self.collectPatternLocalSymbols(&branch_scope, pattern_id);

    const prev_local_symbols = self.current_local_symbols;
    self.current_local_symbols = &branch_scope;
    defer self.current_local_symbols = prev_local_symbols;

    var on_match = try self.lowerExprIntoStmtWithContract(branch.body, target, expr_result_contract, next);
    if (!branch.guard.isNone()) {
        const guard_local = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(branch.guard));
        const guard_switch = try self.emitBoolSwitch(guard_local, on_match, on_fail);
        on_match = try self.lowerExprIntoStmt(branch.guard, guard_local, guard_switch);
    }
    return self.lowerPatternInto(
        source,
        source_mono,
        pattern_id,
        on_match,
        on_fail,
    );
}

fn lowerMatchExprInto(
    self: *Self,
    expr_id: MIR.ExprId,
    match_expr: std.meta.TagPayload(MIR.Expr, .match_expr),
    target: LocalRef,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const cond_mono = self.mir_store.typeOf(match_expr.cond);
    const cond_local = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(match_expr.cond));
    var entry = try self.lir_store.addCFStmt(.{ .runtime_error = {} });
    const expr_result_contract: ExprSummaryContract = switch (self.analyses.getExprResultContract(expr_id)) {
        .borrow_of_fresh => .borrow_of_fresh,
        .concrete => |contract| .{ .concrete = contract },
        .no_return => std.debug.panic(
            "MirToLir invariant violated: value-position match expr {d} cannot have no-return summary",
            .{@intFromEnum(expr_id)},
        ),
    };

    const branches = self.mir_store.getBranches(match_expr.branches);
    var i = branches.len;
    while (i > 0) {
        i -= 1;
        const branch = branches[i];
        const patterns = self.mir_store.getBranchPatterns(branch.patterns);
        var branch_entry = entry;
        var pattern_i = patterns.len;
        while (pattern_i > 0) {
            pattern_i -= 1;
            branch_entry = try self.lowerMatchBranchAlternative(
                cond_local,
                cond_mono,
                branch,
                patterns[pattern_i].pattern,
                expr_result_contract,
                target,
                next,
                branch_entry,
            );
        }
        entry = branch_entry;
    }

    return self.lowerExprIntoStmt(match_expr.cond, cond_local, entry);
}

fn lowerExprIntoStmt(
    self: *Self,
    mir_expr_id: MIR.ExprId,
    target: LocalRef,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const expr = self.mir_store.getExpr(mir_expr_id);
    const result_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);

    return switch (expr) {
        .lookup => |symbol| if (self.isCurrentLocalSymbol(symbol))
            self.emitAssignRef(
                target,
                aliasSemantics(.{ .symbol = symbol, .layout_idx = result_layout }, LIR.RefProjectionSpan.empty()),
                .{ .local = .{ .symbol = symbol, .layout_idx = result_layout } },
                next,
            )
        else
            self.lowerNonLocalLookupInto(symbol, target, next),
        .block => |block| self.lowerBlockInto(block, target, next),
        .borrow_scope => |scope| blk: {
            const prev_region = self.current_borrow_region;
            const scope_id = self.freshBorrowScope();
            self.current_borrow_region = .{ .scope = scope_id };
            defer self.current_borrow_region = prev_region;

            var scope_locals = if (self.current_local_symbols) |locals|
                try self.cloneLocalSymbolSet(locals)
            else
                LocalSymbolSet{};
            defer scope_locals.deinit(self.allocator);

            const bindings = self.mir_store.getBorrowBindings(scope.bindings);
            for (bindings) |binding| {
                try self.collectPatternLocalSymbols(&scope_locals, binding.pattern);
            }

            const scope_exit = try self.lir_store.addCFStmt(.{ .scope_exit = {} });
            const prev_local_symbols = self.current_local_symbols;
            self.current_local_symbols = &scope_locals;
            defer self.current_local_symbols = prev_local_symbols;
            var body = try self.lowerExprIntoStmt(scope.body, target, scope_exit);
            var i = bindings.len;
            while (i > 0) {
                i -= 1;
                self.removePatternLocalSymbols(&scope_locals, bindings[i].pattern);
                self.current_local_symbols = &scope_locals;
                body = try self.lowerBorrowBinding(bindings[i], scope_id, body);
            }

            break :blk try self.lir_store.addCFStmt(.{ .borrow_scope = .{
                .id = scope_id,
                .body = body,
                .remainder = next,
            } });
        },
        .int => |int_lit| {
            return self.emitAssignLiteral(target, mirIntLiteralValue(result_layout, int_lit.value), next);
        },
        .dec => |dec_lit| self.emitAssignLiteral(target, .{ .dec_literal = dec_lit.num }, next),
        .frac_f32 => |f| self.emitAssignLiteral(target, .{ .f32_literal = f }, next),
        .frac_f64 => |f| self.emitAssignLiteral(target, .{ .f64_literal = f }, next),
        .str => |s| self.emitAssignLiteral(
            target,
            .{ .str_literal = try self.internMirStringLiteral(s) },
            next,
        ),
        .list => |list_data| blk: {
            const elems = self.mir_store.getExprSpan(list_data.elems);
            const refs = try self.allocator.alloc(LocalRef, elems.len);
            defer self.allocator.free(refs);
            for (elems, 0..) |elem_expr, i| refs[i] = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(elem_expr));
            const ref_span = try self.lir_store.addLocalRefs(refs);
            const assign_stmt = try self.lir_store.addCFStmt(.{ .assign_list = .{
                .target = target,
                .result = .fresh,
                .elems = ref_span,
                .next = next,
            } });
            var entry = assign_stmt;
            var i = elems.len;
            while (i > 0) {
                i -= 1;
                entry = try self.lowerExprIntoStmt(elems[i], refs[i], entry);
            }
            break :blk entry;
        },
        .struct_ => |struct_data| blk: {
            const fields = self.mir_store.getExprSpan(struct_data.fields);
            const refs = try self.allocator.alloc(LocalRef, fields.len);
            defer self.allocator.free(refs);
            for (fields, 0..) |field_expr, i| refs[i] = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(field_expr));
            const ref_span = try self.lir_store.addLocalRefs(refs);
            const assign_stmt = try self.lir_store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .result = .fresh,
                .fields = ref_span,
                .next = next,
            } });
            var entry = assign_stmt;
            var i = fields.len;
            while (i > 0) {
                i -= 1;
                entry = try self.lowerExprIntoStmt(fields[i], refs[i], entry);
            }
            break :blk entry;
        },
        .tag => |tag_expr| blk: {
            const args = self.mir_store.getExprSpan(tag_expr.args);
            const refs = try self.allocator.alloc(LocalRef, args.len);
            defer self.allocator.free(refs);
            for (args, 0..) |arg_expr, i| refs[i] = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(arg_expr));
            const ref_span = try self.lir_store.addLocalRefs(refs);
            const assign_stmt = try self.lir_store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .result = .fresh,
                .discriminant = self.tagDiscriminantForExpr(mir_expr_id, tag_expr.name),
                .args = ref_span,
                .next = next,
            } });
            var entry = assign_stmt;
            var i = args.len;
            while (i > 0) {
                i -= 1;
                entry = try self.lowerExprIntoStmt(args[i], refs[i], entry);
            }
            break :blk entry;
        },
        .run_low_level => |ll| blk: {
            const args = self.mir_store.getExprSpan(ll.args);
            const refs = try self.allocator.alloc(LocalRef, args.len);
            defer self.allocator.free(refs);
            for (args, 0..) |arg_expr, i| refs[i] = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(arg_expr));
            const ref_span = try self.lir_store.addLocalRefs(refs);
            const assign_stmt = try self.lir_store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .result = lowLevelResultSemantics(self, self.current_borrow_region, ll.op, refs),
                .op = ll.op,
                .args = ref_span,
                .next = next,
            } });
            var entry = assign_stmt;
            var i = args.len;
            while (i > 0) {
                i -= 1;
                entry = try self.lowerExprIntoStmt(args[i], refs[i], entry);
            }
            break :blk entry;
        },
        .str_escape_and_quote => |inner| blk: {
            const arg = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(inner));
            const args = try self.lir_store.addLocalRefs(&.{arg});
            const assign_stmt = try self.lir_store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .result = .fresh,
                .op = .str_inspect,
                .args = args,
                .next = next,
            } });
            break :blk try self.lowerExprIntoStmt(inner, arg, assign_stmt);
        },
        .struct_access => |access| blk: {
            const source_layout = try self.runtimeValueLayoutFromMirExpr(access.struct_);
            const source = self.freshLocal(source_layout);
            const projection = try self.singleProjectionSpan(.{ .field = @intCast(access.field_idx) });
            const assign_stmt = try self.emitAssignRef(
                target,
                borrowSemantics(source, projection, self.current_borrow_region),
                .{ .field = .{
                    .source = source,
                    .field_idx = @intCast(access.field_idx),
                } },
                next,
            );
            break :blk try self.lowerExprIntoStmt(access.struct_, source, assign_stmt);
        },
        .call => |call| blk: {
            const callee = try self.lowerCalleeToProc(call.func);
            const args = self.mir_store.getExprSpan(call.args);
            const hidden_capture_local = try self.freshHiddenCaptureLocal(callee.mir_proc_id);
            const hidden_capture_count: usize = @intFromBool(hidden_capture_local != null);
            const refs = try self.allocator.alloc(LocalRef, args.len + hidden_capture_count);
            defer self.allocator.free(refs);
            for (args, 0..) |arg_expr, i| refs[i] = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(arg_expr));
            if (hidden_capture_local) |capture_local| {
                refs[args.len] = capture_local;
            }
            const ref_span = try self.lir_store.addLocalRefs(refs);
            const assign_stmt = try self.lir_store.addCFStmt(.{ .assign_call = .{
                .target = target,
                .result = self.callResultSemantics(callee.lir_proc_id, refs),
                .proc = callee.lir_proc_id,
                .args = ref_span,
                .next = next,
            } });
            var entry = assign_stmt;
            if (hidden_capture_local) |capture_local| {
                entry = try self.lowerHiddenCaptureArg(call.func, capture_local, entry);
            }
            var i = args.len;
            while (i > 0) {
                i -= 1;
                entry = try self.lowerExprIntoStmt(args[i], refs[i], entry);
            }
            break :blk entry;
        },
        .dbg_expr => |dbg_expr| self.lowerExprIntoStmt(dbg_expr.expr, target, next),
        .expect => |expect| self.lowerExprIntoStmt(expect.body, target, next),
        .runtime_err_can, .runtime_err_type, .runtime_err_ellipsis, .runtime_err_anno_only => self.lir_store.addCFStmt(.{ .runtime_error = {} }),
        .crash => |msg| self.lir_store.addCFStmt(.{ .crash = .{
            .msg = try self.internMirStringLiteral(msg),
        } }),
        .return_expr => |ret| blk: {
            const ret_local = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(ret.expr));
            const ret_stmt = try self.emitRet(ret_local);
            const proc_result_contract = self.current_proc_result_summary orelse std.debug.panic(
                "MirToLir invariant violated: return_expr lowering requires an active proc result contract",
                .{},
            );
            break :blk try self.lowerExprIntoStmtWithContract(ret.expr, ret_local, .{ .concrete = proc_result_contract }, ret_stmt);
        },
        .proc_ref, .closure_make => self.lowerProcBackedValueInto(mir_expr_id, target, next),
        .match_expr => |match_expr| self.lowerMatchExprInto(mir_expr_id, match_expr, target, next),
        .loop => |loop_expr| blk: {
            const exit_join = self.freshJoinPoint();
            const loop_head = self.freshJoinPoint();
            const carried_args = try self.collectLoopCarrierLocals(loop_expr.body);
            const initial_carried_values = self.lir_store.getLocalRefs(carried_args);
            const loop_params = try self.allocator.alloc(LocalRef, initial_carried_values.len);
            defer self.allocator.free(loop_params);

            for (initial_carried_values, 0..) |carried, i| {
                loop_params[i] = self.freshLocal(carried.layout_idx);
            }
            const loop_param_span = try self.lir_store.addLocalRefs(loop_params);

            const initial_jump = try self.lir_store.addCFStmt(.{ .jump = .{
                .target = loop_head,
                .args = carried_args,
            } });
            const backedge_jump = try self.lir_store.addCFStmt(.{ .jump = .{
                .target = loop_head,
                .args = carried_args,
            } });

            const exit_body = try self.emitAssignUnit(target, next);

            try self.break_targets.append(self.allocator, exit_join);
            defer _ = self.break_targets.pop();

            const body_result = self.freshLocal(result_layout);
            var body = try self.lowerExprIntoStmt(loop_expr.body, body_result, backedge_jump);
            const carried_values = self.lir_store.getLocalRefs(carried_args);
            var i = loop_params.len;
            while (i > 0) {
                i -= 1;
                body = try self.emitAssignRef(
                    carried_values[i],
                    aliasSemantics(loop_params[i], LIR.RefProjectionSpan.empty()),
                    .{ .local = loop_params[i] },
                    body,
                );
            }
            const loop_join = try self.lir_store.addCFStmt(.{ .join = .{
                .id = loop_head,
                .params = loop_param_span,
                .body = body,
                .remainder = initial_jump,
            } });

            break :blk try self.lir_store.addCFStmt(.{ .join = .{
                .id = exit_join,
                .params = LocalRefSpan.empty(),
                .body = exit_body,
                .remainder = loop_join,
            } });
        },
        .break_expr => blk: {
            const exit_join = self.break_targets.getLastOrNull() orelse std.debug.panic(
                "MirToLir invariant violated: break_expr escaped the nearest loop during strongest-form lowering",
                .{},
            );
            break :blk try self.lir_store.addCFStmt(.{ .jump = .{
                .target = exit_join,
                .args = LocalRefSpan.empty(),
            } });
        },
    };
}

fn lowerCalleeToProc(self: *Self, mir_expr_id: MIR.ExprId) Allocator.Error!LoweredCallee {
    const mir_proc_id = self.analyses.resolveCallableProcId(self.mir_store, mir_expr_id) orelse std.debug.panic(
        "MirToLir requires call callees to resolve to a unique MIR proc before strongest-form lowering",
        .{},
    );
    return .{
        .mir_proc_id = mir_proc_id,
        .lir_proc_id = try self.lowerProc(mir_proc_id),
    };
}

fn lowerProc(self: *Self, proc_id: MIR.ProcId) Allocator.Error!LirProcSpecId {
    const proc_key = @as(u32, @intFromEnum(proc_id));
    if (self.lowered_procs.get(proc_key)) |existing| return existing;

    const proc = self.mir_store.getProc(proc_id);

    const ret_layout = try self.mir_layout_resolver.resolve(proc.ret_monotype, null);

    const arg_count = self.mir_store.procValueParamCount(proc);
    const args = try self.allocator.alloc(LocalRef, arg_count);
    defer self.allocator.free(args);

    var local_symbols = LocalSymbolSet{};
    defer local_symbols.deinit(self.allocator);
    for (0..arg_count) |i| {
        const arg_pat = self.mir_store.getProcValueParamPattern(proc, i);
        try self.collectPatternLocalSymbols(&local_symbols, arg_pat);
    }
    for (0..arg_count) |i| {
        const arg_pat = self.mir_store.getProcValueParamPattern(proc, i);
        const arg_layout = try self.runtimeValueLayoutFromMirPattern(arg_pat);
        args[i] = self.freshLocal(arg_layout);
    }

    const arg_span = try self.lir_store.addLocalRefs(args);
    const proc_name = if (proc.debug_name.isNone())
        self.lir_store.freshSyntheticSymbol()
    else
        proc.debug_name;
    const summary_result_contract = self.analyses.getProcResultContract(proc_id);
    const result_contract = try self.lowerSummaryProcResultContract(summary_result_contract);

    const lir_proc_id = try self.addUnresolvedProc(.{
        .name = proc_name,
        .args = arg_span,
        .ret_layout = ret_layout,
        .result_contract = result_contract,
        .hosted = if (proc.hosted) |hosted| .{
            .symbol_name = hosted.symbol_name,
            .index = hosted.index,
        } else null,
    });
    try self.lowered_procs.put(proc_key, lir_proc_id);

    const prev_local_symbols = self.current_local_symbols;
    const prev_result_contract = self.current_proc_result_summary;
    self.current_local_symbols = &local_symbols;
    self.current_proc_result_summary = summary_result_contract;
    defer self.current_local_symbols = prev_local_symbols;
    defer self.current_proc_result_summary = prev_result_contract;

    const target = self.freshLocal(ret_layout);
    const ret_stmt = try self.emitRet(target);
    var body = try self.lowerExprIntoStmtWithContract(proc.body, target, .{ .concrete = summary_result_contract }, ret_stmt);
    const param_fail = try self.lir_store.addCFStmt(.{ .runtime_error = {} });
    var i = arg_count;
    while (i > 0) {
        i -= 1;
        const arg_pat = self.mir_store.getProcValueParamPattern(proc, i);
        body = try self.lowerPatternInto(
            args[i],
            self.mir_store.patternTypeOf(arg_pat),
            arg_pat,
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
        .hosted = if (proc.hosted) |hosted| .{
            .symbol_name = hosted.symbol_name,
            .index = hosted.index,
        } else null,
    });
    return lir_proc_id;
}
