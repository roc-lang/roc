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

const CFStmtId = LIR.CFStmtId;
const LirProcSpec = LIR.LirProcSpec;
const LirProcSpecId = LIR.LirProcSpecId;
const LiteralValue = LIR.LiteralValue;
const LocalRef = LIR.LocalRef;
const ResultSemantics = LIR.ResultSemantics;
const RefOp = LIR.RefOp;
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

next_borrow_scope: u32 = 0,
current_borrow_region: LIR.BorrowRegion = .proc,

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
    };
}

/// Releases temporary lowering state owned by this translator.
pub fn deinit(self: *Self) void {
    self.lowered_procs.deinit();
    self.builder_procs.deinit(self.allocator);
    self.active_value_defs.deinit(self.allocator);
    self.mir_layout_resolver.deinit();
}

/// Lowers a root MIR expression into a zero-argument LIR proc and flushes all lowered procs.
pub fn lower(self: *Self, mir_expr_id: MIR.ExprId) Allocator.Error!LirProcSpecId {
    self.ensureCanLowerMoreProcs();

    const ret_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const body = try self.lowerEntrypointBody(mir_expr_id, ret_layout);
    const args = LIR.LocalRefSpan.empty();
    const result_contract = try self.lowerRootResultContract(mir_expr_id);

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

fn lowerProcResultContractForMirProc(
    self: *Self,
    proc_id: MIR.ProcId,
) Allocator.Error!LIR.ProcResultContract {
    return self.lowerSummaryProcResultContract(
        self.analyses.getProcResultContract(proc_id),
    );
}

fn lowerRootResultContract(
    self: *Self,
    mir_expr_id: MIR.ExprId,
) Allocator.Error!LIR.ProcResultContract {
    const summary_contract = self.analyses.getRootResultContract(mir_expr_id);
    return self.lowerSummaryProcResultContract(summary_contract);
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

fn emitRet(self: *Self, value: LocalRef) Allocator.Error!CFStmtId {
    return self.lir_store.addCFStmt(.{ .ret = .{ .value = value } });
}

const LoweredEntrypointProcBody = struct {
    body: CFStmtId,
    result_contract: LIR.ProcResultContract,
};

fn lowerEntrypointBody(self: *Self, mir_expr_id: MIR.ExprId, ret_layout: layout.Idx) Allocator.Error!CFStmtId {
    var local_symbols = LocalSymbolSet{};
    defer local_symbols.deinit(self.allocator);
    try self.collectExprLocalSymbols(&local_symbols, mir_expr_id);

    const prev_local_symbols = self.current_local_symbols;
    self.current_local_symbols = &local_symbols;
    defer self.current_local_symbols = prev_local_symbols;

    const target = self.freshLocal(ret_layout);
    const ret_stmt = try self.emitRet(target);
    return self.lowerExprIntoStmt(mir_expr_id, target, ret_stmt);
}

fn lowerEntrypointProcBody(
    self: *Self,
    mir_expr_id: MIR.ExprId,
    arg_locals: []const LocalRef,
    ret_layout: layout.Idx,
) Allocator.Error!LoweredEntrypointProcBody {
    var local_symbols = LocalSymbolSet{};
    defer local_symbols.deinit(self.allocator);
    try self.collectExprLocalSymbols(&local_symbols, mir_expr_id);

    const prev_local_symbols = self.current_local_symbols;
    self.current_local_symbols = &local_symbols;
    defer self.current_local_symbols = prev_local_symbols;

    const mir_mono = self.mir_store.monotype_store.getMonotype(self.mir_store.typeOf(mir_expr_id));
    const must_call = arg_locals.len != 0 or mir_mono == .func;

    if (!must_call) {
        return .{
            .body = try self.lowerEntrypointBody(mir_expr_id, ret_layout),
            .result_contract = try self.lowerRootResultContract(mir_expr_id),
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

fn lowerBlockInto(
    self: *Self,
    block: anytype,
    target: LocalRef,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    var entry = try self.lowerExprIntoStmt(block.final_expr, target, next);
    const stmts = self.mir_store.getStmts(block.stmts);
    var i = stmts.len;
    while (i > 0) {
        i -= 1;
        entry = try self.lowerStmtInto(stmts[i], entry);
    }
    return entry;
}

fn recordLocalSymbol(self: *Self, local_symbols: *LocalSymbolSet, symbol: MIR.Symbol) Allocator.Error!void {
    try local_symbols.put(self.allocator, symbol.raw(), {});
}

fn collectPatternLocalSymbols(
    self: *Self,
    local_symbols: *LocalSymbolSet,
    pattern_id: MIR.PatternId,
) Allocator.Error!void {
    switch (self.mir_store.getPattern(pattern_id)) {
        .bind => |symbol| try self.recordLocalSymbol(local_symbols, symbol),
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
            try self.recordLocalSymbol(local_symbols, as_pattern.symbol);
            try self.collectPatternLocalSymbols(local_symbols, as_pattern.pattern);
        },
    }
}

fn collectStmtLocalSymbols(self: *Self, local_symbols: *LocalSymbolSet, stmt: MIR.Stmt) Allocator.Error!void {
    const binding = switch (stmt) {
        .decl_const => |inner| inner,
        .decl_var => |inner| inner,
        .mutate_var => |inner| inner,
    };
    try self.collectPatternLocalSymbols(local_symbols, binding.pattern);
    try self.collectExprLocalSymbols(local_symbols, binding.expr);
}

fn collectExprLocalSymbols(
    self: *Self,
    local_symbols: *LocalSymbolSet,
    expr_id: MIR.ExprId,
) Allocator.Error!void {
    switch (self.mir_store.getExpr(expr_id)) {
        .int,
        .frac_f32,
        .frac_f64,
        .dec,
        .str,
        .lookup,
        .proc_ref,
        .runtime_err_can,
        .runtime_err_type,
        .runtime_err_ellipsis,
        .runtime_err_anno_only,
        .crash,
        .break_expr,
        => {},
        .closure_make => |closure| try self.collectExprLocalSymbols(local_symbols, closure.captures),
        .list => |list_data| {
            for (self.mir_store.getExprSpan(list_data.elems)) |elem_expr| {
                try self.collectExprLocalSymbols(local_symbols, elem_expr);
            }
        },
        .struct_ => |struct_data| {
            for (self.mir_store.getExprSpan(struct_data.fields)) |field_expr| {
                try self.collectExprLocalSymbols(local_symbols, field_expr);
            }
        },
        .tag => |tag_expr| {
            for (self.mir_store.getExprSpan(tag_expr.args)) |arg_expr| {
                try self.collectExprLocalSymbols(local_symbols, arg_expr);
            }
        },
        .match_expr => |match_expr| {
            try self.collectExprLocalSymbols(local_symbols, match_expr.cond);
            for (self.mir_store.getBranches(match_expr.branches)) |branch| {
                for (self.mir_store.getBranchPatterns(branch.patterns)) |branch_pattern| {
                    try self.collectPatternLocalSymbols(local_symbols, branch_pattern.pattern);
                }
                if (!branch.guard.isNone()) {
                    try self.collectExprLocalSymbols(local_symbols, branch.guard);
                }
                try self.collectExprLocalSymbols(local_symbols, branch.body);
            }
        },
        .call => |call| {
            try self.collectExprLocalSymbols(local_symbols, call.func);
            for (self.mir_store.getExprSpan(call.args)) |arg_expr| {
                try self.collectExprLocalSymbols(local_symbols, arg_expr);
            }
        },
        .block => |block| {
            for (self.mir_store.getStmts(block.stmts)) |stmt| {
                try self.collectStmtLocalSymbols(local_symbols, stmt);
            }
            try self.collectExprLocalSymbols(local_symbols, block.final_expr);
        },
        .borrow_scope => |scope| {
            for (self.mir_store.getBorrowBindings(scope.bindings)) |binding| {
                try self.collectPatternLocalSymbols(local_symbols, binding.pattern);
                try self.collectExprLocalSymbols(local_symbols, binding.expr);
            }
            try self.collectExprLocalSymbols(local_symbols, scope.body);
        },
        .struct_access => |access| try self.collectExprLocalSymbols(local_symbols, access.struct_),
        .str_escape_and_quote => |inner| try self.collectExprLocalSymbols(local_symbols, inner),
        .run_low_level => |ll| {
            for (self.mir_store.getExprSpan(ll.args)) |arg_expr| {
                try self.collectExprLocalSymbols(local_symbols, arg_expr);
            }
        },
        .dbg_expr => |dbg_expr| try self.collectExprLocalSymbols(local_symbols, dbg_expr.expr),
        .expect => |expect| try self.collectExprLocalSymbols(local_symbols, expect.body),
        .loop => |loop_expr| try self.collectExprLocalSymbols(local_symbols, loop_expr.body),
        .return_expr => |ret| try self.collectExprLocalSymbols(local_symbols, ret.expr),
    }
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

fn tagDiscriminantForPattern(
    self: *Self,
    pattern_id: MIR.PatternId,
    tag_name: base.Ident.Idx,
) u16 {
    const mono = self.mir_store.monotype_store.getMonotype(self.mir_store.patternTypeOf(pattern_id));
    const tags = switch (mono) {
        .tag_union => |tag_union| self.mir_store.monotype_store.getTags(tag_union.tags),
        else => std.debug.panic(
            "MirToLir invariant violated: tag pattern does not have a tag-union monotype",
            .{},
        ),
    };

    for (tags, 0..) |tag, index| {
        if (tag.name.ident.eql(tag_name)) return @intCast(index);
    }

    std.debug.panic(
        "MirToLir invariant violated: tag pattern constructor missing from pattern monotype",
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
    pattern_id: MIR.PatternId,
    on_match: CFStmtId,
    on_fail: CFStmtId,
) Allocator.Error!CFStmtId {
    return switch (self.mir_store.getPattern(pattern_id)) {
        .bind => |symbol| self.emitAssignRef(
            .{
                .symbol = symbol,
                .layout_idx = try self.runtimeValueLayoutFromMirPattern(pattern_id),
            },
            aliasSemantics(source, LIR.RefProjectionSpan.empty()),
            .{ .local = source },
            on_match,
        ),
        .wildcard => on_match,
        .as_pattern => |as_pattern| blk: {
            const inner = try self.lowerPatternInto(source, as_pattern.pattern, on_match, on_fail);
            break :blk try self.emitAssignRef(
                .{
                    .symbol = as_pattern.symbol,
                    .layout_idx = try self.runtimeValueLayoutFromMirPattern(pattern_id),
                },
                aliasSemantics(source, LIR.RefProjectionSpan.empty()),
                .{ .local = source },
                inner,
            );
        },
        .tag => |tag_pattern| blk: {
            const discr_local = self.freshLocal(.u32);
            const discr_value = self.tagDiscriminantForPattern(pattern_id, tag_pattern.name);
            const payload_patterns = self.mir_store.getPatternSpan(tag_pattern.args);

            const matched = blk_matched: {
                if (payload_patterns.len == 0) break :blk_matched on_match;
                if (payload_patterns.len != 1) {
                    std.debug.panic(
                        "MirToLir invariant violated: tag pattern payload arity {d} must be wrapped before statement-only lowering",
                        .{payload_patterns.len},
                    );
                }

                const payload_pattern = payload_patterns[0];
                const payload_local = self.freshLocal(try self.runtimeValueLayoutFromMirPattern(payload_pattern));
                const payload_match = try self.lowerPatternInto(payload_local, payload_pattern, on_match, on_fail);
                break :blk_matched try self.emitAssignRef(
                    payload_local,
                    borrowSemantics(source, try self.singleProjectionSpan(.tag_payload), self.current_borrow_region),
                    .{ .tag_payload = .{ .source = source } },
                    payload_match,
                );
            };

            const discr_switch = try self.emitSwitchOnValue(discr_local, discr_value, matched, on_fail);
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
                const field_local = self.freshLocal(try self.runtimeValueLayoutFromMirPattern(field_pattern));
                const field_match = try self.lowerPatternInto(field_local, field_pattern, entry, on_fail);
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
    branch: MIR.Branch,
    pattern_id: MIR.PatternId,
    target: LocalRef,
    next: CFStmtId,
    on_fail: CFStmtId,
) Allocator.Error!CFStmtId {
    var on_match = try self.lowerExprIntoStmt(branch.body, target, next);
    if (!branch.guard.isNone()) {
        const guard_local = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(branch.guard));
        const guard_switch = try self.emitBoolSwitch(guard_local, on_match, on_fail);
        on_match = try self.lowerExprIntoStmt(branch.guard, guard_local, guard_switch);
    }
    return self.lowerPatternInto(source, pattern_id, on_match, on_fail);
}

fn lowerMatchExprInto(
    self: *Self,
    match_expr: std.meta.TagPayload(MIR.Expr, .match_expr),
    target: LocalRef,
    next: CFStmtId,
) Allocator.Error!CFStmtId {
    const cond_local = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(match_expr.cond));
    var entry = try self.lir_store.addCFStmt(.{ .runtime_error = {} });

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
                branch,
                patterns[pattern_i].pattern,
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

            const scope_exit = try self.lir_store.addCFStmt(.{ .scope_exit = {} });
            var body = try self.lowerExprIntoStmt(scope.body, target, scope_exit);
            const bindings = self.mir_store.getBorrowBindings(scope.bindings);
            var i = bindings.len;
            while (i > 0) {
                i -= 1;
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
            const proc_id = try self.lowerCalleeToProc(call.func);
            const args = self.mir_store.getExprSpan(call.args);
            const refs = try self.allocator.alloc(LocalRef, args.len);
            defer self.allocator.free(refs);
            for (args, 0..) |arg_expr, i| refs[i] = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(arg_expr));
            const ref_span = try self.lir_store.addLocalRefs(refs);
            const assign_stmt = try self.lir_store.addCFStmt(.{ .assign_call = .{
                .target = target,
                .result = self.callResultSemantics(proc_id, refs),
                .proc = proc_id,
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
        .dbg_expr => |dbg_expr| self.lowerExprIntoStmt(dbg_expr.expr, target, next),
        .expect => |expect| self.lowerExprIntoStmt(expect.body, target, next),
        .runtime_err_can, .runtime_err_type, .runtime_err_ellipsis, .runtime_err_anno_only => self.lir_store.addCFStmt(.{ .runtime_error = {} }),
        .crash => |msg| self.lir_store.addCFStmt(.{ .crash = .{
            .msg = try self.internMirStringLiteral(msg),
        } }),
        .return_expr => |ret| blk: {
            const ret_local = self.freshLocal(try self.runtimeValueLayoutFromMirExpr(ret.expr));
            const ret_stmt = try self.emitRet(ret_local);
            break :blk try self.lowerExprIntoStmt(ret.expr, ret_local, ret_stmt);
        },
        .proc_ref, .closure_make => std.debug.panic(
            "statement-only MirToLir must lower proc-backed values through lowered proc specs before value emission",
            .{},
        ),
        .match_expr => |match_expr| self.lowerMatchExprInto(match_expr, target, next),
        .loop, .break_expr => std.debug.panic(
            "strongest-form MirToLir requires control-flow and destructuring lowering before value lowering for MIR expr tag {s}",
            .{@tagName(expr)},
        ),
    };
}

fn lowerCalleeToProc(self: *Self, mir_expr_id: MIR.ExprId) Allocator.Error!LirProcSpecId {
    switch (self.mir_store.getExpr(mir_expr_id)) {
        .proc_ref, .closure_make, .lookup => {},
        .dbg_expr => |dbg_expr| return self.lowerCalleeToProc(dbg_expr.expr),
        .expect => |expect| return self.lowerCalleeToProc(expect.body),
        .return_expr => |ret| return self.lowerCalleeToProc(ret.expr),
        .block => |block| {
            if (self.mir_store.getStmts(block.stmts).len != 0) {
                std.debug.panic(
                    "MirToLir requires call callees to be direct proc-backed values, not statementful blocks",
                    .{},
                );
            }
            return self.lowerCalleeToProc(block.final_expr);
        },
        else => std.debug.panic("MirToLir requires direct proc-backed callees before strongest-form lowering", .{}),
    }
    const proc_id = self.mir_store.resolveCallableProcId(mir_expr_id) orelse std.debug.panic(
        "MirToLir invariant violated: admissible callee form did not resolve to a MIR proc",
        .{},
    );
    return self.lowerProc(proc_id);
}

fn lowerProc(self: *Self, proc_id: MIR.ProcId) Allocator.Error!LirProcSpecId {
    const proc_key = @as(u32, @intFromEnum(proc_id));
    if (self.lowered_procs.get(proc_key)) |existing| return existing;

    const proc = self.mir_store.getProc(proc_id);

    const ret_layout = try self.mir_layout_resolver.resolve(proc.ret_monotype, null);

    const arg_patterns = self.mir_store.getPatternSpan(proc.params);
    const args = try self.allocator.alloc(LocalRef, arg_patterns.len);
    defer self.allocator.free(args);

    var local_symbols = LocalSymbolSet{};
    defer local_symbols.deinit(self.allocator);
    for (arg_patterns) |arg_pat| {
        try self.collectPatternLocalSymbols(&local_symbols, arg_pat);
    }
    try self.collectExprLocalSymbols(&local_symbols, proc.body);

    for (arg_patterns, 0..) |arg_pat, i| {
        const arg_layout = try self.runtimeValueLayoutFromMirPattern(arg_pat);
        args[i] = switch (self.mir_store.getPattern(arg_pat)) {
            .bind => |symbol| .{ .symbol = symbol, .layout_idx = arg_layout },
            .wildcard => self.freshLocal(arg_layout),
            else => std.debug.panic(
                "MirToLir invariant violated: proc arg pattern {s} must be lowered before statement-only LIR",
                .{@tagName(self.mir_store.getPattern(arg_pat))},
            ),
        };
    }

    const arg_span = try self.lir_store.addLocalRefs(args);
    const proc_name = if (proc.debug_name.isNone())
        self.lir_store.freshSyntheticSymbol()
    else
        proc.debug_name;
    const result_contract = try self.lowerProcResultContractForMirProc(proc_id);

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
    self.current_local_symbols = &local_symbols;
    defer self.current_local_symbols = prev_local_symbols;

    const target = self.freshLocal(ret_layout);
    const ret_stmt = try self.emitRet(target);
    const body = try self.lowerExprIntoStmt(proc.body, target, ret_stmt);

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
