//! Specialization from solved callable semantics to executable structured callable IR.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const ContextMono = @import("ContextMono.zig");
const DispatchSolved = @import("DispatchSolved.zig");
const Lambdasolved = @import("Lambdasolved.zig");
const Monotype = @import("Monotype.zig");
const ValueProjection = @import("ValueProjection.zig");

const Allocator = std.mem.Allocator;
const Region = base.Region;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

/// Identifies one executable callable specialization chosen from solved
/// callable semantics.
pub const CallableInstId = Lambdasolved.CallableInstId;

pub const CallableInst = struct {
    template: Lambdasolved.CallableTemplateId,
    subst: ContextMono.TypeSubstId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    defining_source_context: SourceContext,
    callable_def: CallableDefId,
    runtime_value: RuntimeValue,
    callable_param_specs: CallableParamSpecSpan = .empty(),
};

pub const RuntimeValue = union(enum) {
    direct_lambda,
    closure: struct {
        capture_tuple_monotype: ContextMono.ResolvedMonotype,
    },
};

pub const PackedFn = struct {
    variant_group: CallableVariantGroupId,
    fn_monotype: ContextMono.ResolvedMonotype,
    runtime_monotype: ContextMono.ResolvedMonotype,
};

pub const IndirectCall = struct {
    packed_fn: PackedFn,
};

pub const DispatchIntrinsic = enum {
    negate,
};

pub const DispatchSemantics = union(enum) {
    target: DispatchSolved.DispatchExprTarget,
    intrinsic: DispatchIntrinsic,
};

pub const ExprId = enum(u32) {
    _,
};

pub const ExprRef = struct {
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    projections: ValueProjection.ProjectionSpan = .empty(),
};

pub const ExprIdSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() ExprIdSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: ExprIdSpan) bool {
        return self.len == 0;
    }
};

pub const PatternIdSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() PatternIdSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: PatternIdSpan) bool {
        return self.len == 0;
    }
};

pub const StmtId = enum(u32) {
    _,
};

pub const StmtIdSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() StmtIdSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: StmtIdSpan) bool {
        return self.len == 0;
    }
};

pub const CallableValue = union(enum) {
    direct: CallableInstId,
    packed_fn: PackedFn,
};

pub const CallSite = union(enum) {
    direct: CallableInstId,
    indirect_call: IndirectCall,
    low_level: CIR.Expr.LowLevel,
};

pub fn exactCallableInstFromValue(callable_value: CallableValue) ?CallableInstId {
    return switch (callable_value) {
        .direct => |callable_inst_id| callable_inst_id,
        .packed_fn => null,
    };
}

pub fn exactCallableInstFromCallSite(call_site: CallSite) ?CallableInstId {
    return switch (call_site) {
        .direct => |callable_inst_id| callable_inst_id,
        .indirect_call, .low_level => null,
    };
}

pub const CallableIntro = struct {
    callable_value: CallableValue,
    callable_inst: CallableInstId,
};

pub const ExprCallableSemantics = union(enum) {
    callable: CallableValue,
    intro: CallableIntro,
};

pub const LookupResolution = union(enum) {
    expr: ExprRef,
    def: Lambdasolved.ExternalDefSource,
};

pub const CallableParamProjection = ValueProjection.Projection;
pub const CallableParamProjectionSpan = ValueProjection.ProjectionSpan;

pub const CallableParamSpecEntry = struct {
    param_index: u16,
    projections: CallableParamProjectionSpan = .empty(),
    callable_value: CallableValue,
};

pub const CallableParamSpecSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableParamSpecSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableParamSpecSpan) bool {
        return self.len == 0;
    }
};

pub const CallableVariantSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableVariantSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableVariantSpan) bool {
        return self.len == 0;
    }
};

pub const CallableVariantGroupId = enum(u32) {
    _,
};

pub const ContextExprKey = ContextMono.ContextExprKey;
pub const SourceContext = ContextMono.SourceContext;
const SemanticThread = Lambdasolved.SemanticThread;
pub const BuildStmtKey = struct {
    source_context_kind: ContextMono.SourceContextKind,
    source_context_module_idx: u32,
    source_context_raw: u32,
    module_idx: u32,
    stmt_raw: u32,
};

pub fn buildStmtKey(
    source_context: SourceContext,
    module_idx: u32,
    stmt_idx: CIR.Statement.Idx,
) BuildStmtKey {
    return switch (source_context) {
        .callable_inst => |context_id| .{
            .source_context_kind = .callable_inst,
            .source_context_module_idx = std.math.maxInt(u32),
            .source_context_raw = @intFromEnum(context_id),
            .module_idx = module_idx,
            .stmt_raw = @intFromEnum(stmt_idx),
        },
        .root_expr => |root| .{
            .source_context_kind = .root_expr,
            .source_context_module_idx = root.module_idx,
            .source_context_raw = @intFromEnum(root.expr_idx),
            .module_idx = module_idx,
            .stmt_raw = @intFromEnum(stmt_idx),
        },
        .provenance_expr => |source| .{
            .source_context_kind = .provenance_expr,
            .source_context_module_idx = source.module_idx,
            .source_context_raw = @intFromEnum(source.expr_idx),
            .module_idx = module_idx,
            .stmt_raw = @intFromEnum(stmt_idx),
        },
        .template_expr => |template| .{
            .source_context_kind = .template_expr,
            .source_context_module_idx = template.module_idx,
            .source_context_raw = @intFromEnum(template.expr_idx),
            .module_idx = module_idx,
            .stmt_raw = @intFromEnum(stmt_idx),
        },
    };
}

pub fn calleeUsesFirstClassCallableValuePath(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_lookup_local,
        .e_lookup_external,
        .e_lookup_required,
        .e_lambda,
        .e_closure,
        .e_hosted_lambda,
        => false,
        else => true,
    };
}

pub fn requireProgramExprMonotype(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!ContextMono.ResolvedMonotype {
    return ContextMono.requireRecordedExprMonotypeForSourceContext(
        driver,
        result,
        SemanticThread.trackedThread(source_context),
        source_context,
        module_idx,
        expr_idx,
    );
}

pub fn exprHasExactProgramSemantics(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!bool {
    const thread = SemanticThread.trackedThread(source_context);
    const monotype = try ContextMono.lookupRecordedExprMonotypeIfReadyForSourceContext(
        driver,
        result,
        thread,
        source_context,
        module_idx,
        expr_idx,
    );
    return monotype != null;
}

fn exprNeedsCallableShape(result: anytype, source_context: SourceContext, module_idx: u32, expr_idx: CIR.Expr.Idx) bool {
    const monotype = result.getExprMonotype(source_context, module_idx, expr_idx) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdamono invariant violated: expr ctx={s} module={d} expr={d} had no exact monotype during executable assembly",
                .{ @tagName(source_context), module_idx, @intFromEnum(expr_idx) },
            );
        }
        unreachable;
    };
    return switch (result.context_mono.monotype_store.getMonotype(monotype.idx)) {
        .func => true,
        else => result.getExprTemplateId(source_context, module_idx, expr_idx) != null,
    };
}

fn exprHasCallableShape(result: anytype, source_context: SourceContext, module_idx: u32, expr_idx: CIR.Expr.Idx) bool {
    return result.getExprCallableValue(source_context, module_idx, expr_idx) != null or
        result.getExprIntroCallableInst(source_context, module_idx, expr_idx) != null;
}

pub fn requireProgramExprSemanticShape(
    _: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
) Allocator.Error!void {
    switch (expr) {
        .e_lookup_local => {},
        .e_lookup_external, .e_lookup_required => {
            if (result.getExprLookupResolution(source_context, module_idx, expr_idx) == null) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Lambdamono invariant violated: non-local lookup expr ctx={s} module={d} expr={d} reached assembly without lookup semantics",
                        .{ @tagName(source_context), module_idx, @intFromEnum(expr_idx) },
                    );
                }
                unreachable;
            }
        },
        .e_call => {
            if (result.getExprCallSite(source_context, module_idx, expr_idx) == null) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Lambdamono invariant violated: call expr ctx={s} module={d} expr={d} reached assembly without call-site semantics",
                        .{ @tagName(source_context), module_idx, @intFromEnum(expr_idx) },
                    );
                }
                unreachable;
            }
        },
        .e_type_var_dispatch => {
            if (result.getDispatchExprTarget(source_context, module_idx, expr_idx) == null and
                result.getDispatchExprIntrinsic(source_context, module_idx, expr_idx) == null)
            {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Lambdamono invariant violated: dispatch expr ctx={s} module={d} expr={d} reached assembly without dispatch semantics",
                        .{ @tagName(source_context), module_idx, @intFromEnum(expr_idx) },
                    );
                }
                unreachable;
            }
        },
        .e_dot_access => |dot_expr| {
            if (dot_expr.args != null) {
                if (result.getDispatchExprTarget(source_context, module_idx, expr_idx) == null and
                    result.getDispatchExprIntrinsic(source_context, module_idx, expr_idx) == null)
                {
                    if (std.debug.runtime_safety) {
                        std.debug.panic(
                            "Lambdamono invariant violated: dot-call expr ctx={s} module={d} expr={d} reached assembly without dispatch semantics",
                            .{ @tagName(source_context), module_idx, @intFromEnum(expr_idx) },
                        );
                    }
                    unreachable;
                }
            }
        },
        else => {},
    }

    if (exprNeedsCallableShape(result, source_context, module_idx, expr_idx) and
        !exprHasCallableShape(result, source_context, module_idx, expr_idx))
    {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "Lambdamono invariant violated: function-valued expr ctx={s} module={d} expr={d} reached assembly without callable semantics",
                .{ @tagName(source_context), module_idx, @intFromEnum(expr_idx) },
            );
        }
        unreachable;
    }
}

pub fn requireCallableInstRealized(
    _: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
) Allocator.Error!void {
    if (callableInstHasRealizedRuntimeExpr(result, callable_inst_id)) return;
    if (std.debug.runtime_safety) {
        std.debug.panic(
            "Lambdamono invariant violated: callable inst {d} reached executable assembly without solved runtime semantics",
            .{@intFromEnum(callable_inst_id)},
        );
    }
    unreachable;
}

pub const CaptureValueSource = union(enum) {
    lexical_binding: struct {
        callable_inst: CallableInstId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    },
    bound_expr: struct {
        expr_ref: ExprRef,
    },
};

pub const CaptureStorage = union(enum) {
    runtime_field: struct {
        field_monotype: ContextMono.ResolvedMonotype,
    },
    callable_only,
    recursive_member,
};

pub const CallableDefId = enum(u32) {
    _,
};

pub const CallableRuntimeKind = enum {
    lambda,
    closure,
    hosted_lambda,
};

pub const CaptureField = struct {
    pattern_idx: CIR.Pattern.Idx,
    local_monotype: ContextMono.ResolvedMonotype,
    callable_value: ?CallableValue = null,
    source: CaptureValueSource,
    storage: CaptureStorage,
};

pub const CaptureFieldSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CaptureFieldSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CaptureFieldSpan) bool {
        return self.len == 0;
    }
};

pub const CallableDef = struct {
    module_idx: u32,
    runtime_kind: CallableRuntimeKind,
    arg_patterns: PatternIdSpan,
    runtime_expr: ExprRef,
    body_expr: ExprRef,
    fn_monotype: ContextMono.ResolvedMonotype,
    captures: CaptureFieldSpan = .empty(),
    source_region: Region,
};

pub const ValueOrigin = union(enum) {
    self,
    expr: ExprRef,
};

/// One finalized specialized expr in the executable `Lambdamono.Program`.
///
/// Invariant: every `Expr` stored in `Program.exprs` already has its exact
/// monotype. Finalization/build progress must live in the run-local transform,
/// not in the executable IR itself.
pub const ExprCommon = struct {
    source_context: SourceContext,
    module_idx: u32,
    source_expr: CIR.Expr.Idx,
    monotype: ContextMono.ResolvedMonotype,
    child_exprs: ExprIdSpan = .empty(),
    child_stmts: StmtIdSpan = .empty(),
    origin: ValueOrigin = .self,
    callable: ?ExprCallableSemantics = null,
};

pub const Expr = union(enum) {
    plain: ExprCommon,
    direct_call: struct {
        common: ExprCommon,
        callable_inst: CallableInstId,
    },
    indirect_call: struct {
        common: ExprCommon,
        indirect_call: IndirectCall,
    },
    low_level_call: struct {
        common: ExprCommon,
        low_level: CIR.Expr.LowLevel,
    },
    lookup_expr: struct {
        common: ExprCommon,
        expr_ref: ExprRef,
    },
    lookup_def: struct {
        common: ExprCommon,
        def_source: Lambdasolved.ExternalDefSource,
    },
    dispatch_target: struct {
        common: ExprCommon,
        target: DispatchSolved.DispatchExprTarget,
    },
    dispatch_intrinsic: struct {
        common: ExprCommon,
        intrinsic: DispatchIntrinsic,
    },

    pub fn common(self: *const Expr) *const ExprCommon {
        return switch (self.*) {
            .plain => |*plain_common| plain_common,
            .direct_call => |*call| &call.common,
            .indirect_call => |*call| &call.common,
            .low_level_call => |*call| &call.common,
            .lookup_expr => |*lookup| &lookup.common,
            .lookup_def => |*lookup| &lookup.common,
            .dispatch_target => |*dispatch| &dispatch.common,
            .dispatch_intrinsic => |*dispatch| &dispatch.common,
        };
    }

    pub fn commonMut(self: *Expr) *ExprCommon {
        return switch (self.*) {
            .plain => |*plain_common| plain_common,
            .direct_call => |*call| &call.common,
            .indirect_call => |*call| &call.common,
            .low_level_call => |*call| &call.common,
            .lookup_expr => |*lookup| &lookup.common,
            .lookup_def => |*lookup| &lookup.common,
            .dispatch_target => |*dispatch| &dispatch.common,
            .dispatch_intrinsic => |*dispatch| &dispatch.common,
        };
    }

    pub fn getCallableValue(self: *const Expr) ?CallableValue {
        return switch (self.common().callable orelse return null) {
            .callable => |callable_value| callable_value,
            .intro => |intro| intro.callable_value,
        };
    }

    pub fn getCallableIntro(self: *const Expr) ?CallableIntro {
        return switch (self.common().callable orelse return null) {
            .intro => |intro| intro,
            .callable => null,
        };
    }

    pub fn getCallable(self: *const Expr) ?ExprCallableSemantics {
        return self.common().callable;
    }

    pub fn getDirectCall(self: *const Expr) ?CallableInstId {
        return switch (self.*) {
            .direct_call => |call| call.callable_inst,
            else => null,
        };
    }

    pub fn getIndirectCall(self: *const Expr) ?IndirectCall {
        return switch (self.*) {
            .indirect_call => |call| call.indirect_call,
            else => null,
        };
    }

    pub fn getLowLevelCall(self: *const Expr) ?CIR.Expr.LowLevel {
        return switch (self.*) {
            .low_level_call => |call| call.low_level,
            else => null,
        };
    }

    pub fn getCall(self: *const Expr) ?CallSite {
        return switch (self.*) {
            .direct_call => |call| .{ .direct = call.callable_inst },
            .indirect_call => |call| .{ .indirect_call = call.indirect_call },
            .low_level_call => |call| .{ .low_level = call.low_level },
            else => null,
        };
    }

    pub fn getLookupExpr(self: *const Expr) ?ExprRef {
        return switch (self.*) {
            .lookup_expr => |lookup| lookup.expr_ref,
            else => null,
        };
    }

    pub fn getLookupDef(self: *const Expr) ?Lambdasolved.ExternalDefSource {
        return switch (self.*) {
            .lookup_def => |lookup| lookup.def_source,
            else => null,
        };
    }

    pub fn getLookup(self: *const Expr) ?LookupResolution {
        return switch (self.*) {
            .lookup_expr => |lookup| .{ .expr = lookup.expr_ref },
            .lookup_def => |lookup| .{ .def = lookup.def_source },
            else => null,
        };
    }

    pub fn getDispatchTarget(self: *const Expr) ?DispatchSolved.DispatchExprTarget {
        return switch (self.*) {
            .dispatch_target => |dispatch| dispatch.target,
            else => null,
        };
    }

    pub fn getDispatchIntrinsic(self: *const Expr) ?DispatchIntrinsic {
        return switch (self.*) {
            .dispatch_intrinsic => |dispatch| dispatch.intrinsic,
            else => null,
        };
    }

    pub fn getDispatch(self: *const Expr) ?DispatchSemantics {
        return switch (self.*) {
            .dispatch_target => |dispatch| .{ .target = dispatch.target },
            .dispatch_intrinsic => |dispatch| .{ .intrinsic = dispatch.intrinsic },
            else => null,
        };
    }

    pub fn getOriginExpr(self: *const Expr) ?ExprRef {
        return switch (self.common().origin) {
            .self => null,
            .expr => |origin| origin,
        };
    }
};

pub const Stmt = struct {
    module_idx: u32,
    source_stmt: CIR.Statement.Idx,
    child_exprs: ExprIdSpan = .empty(),
};

/// Final specialized program consumed by later lowering stages.
///
/// Invariant: `exprs` contains only finalized expr records. Any temporary
/// specialization/build state must live outside this struct.
pub const Program = struct {
    exprs: std.ArrayListUnmanaged(Expr),
    // Final lookup index for downstream stages that need the executable expr
    // corresponding to a source-context expr.
    expr_ids_by_context: std.AutoHashMapUnmanaged(ContextExprKey, ExprId),
    expr_child_entries: std.ArrayListUnmanaged(ExprId),
    stmts: std.ArrayListUnmanaged(Stmt),
    stmt_ids_by_key: std.AutoHashMapUnmanaged(BuildStmtKey, StmtId),
    stmt_child_entries: std.ArrayListUnmanaged(StmtId),

    pub fn init() Program {
        return .{
            .exprs = .empty,
            .expr_ids_by_context = .empty,
            .expr_child_entries = .empty,
            .stmts = .empty,
            .stmt_ids_by_key = .empty,
            .stmt_child_entries = .empty,
        };
    }

    pub fn deinit(self: *Program, allocator: Allocator) void {
        self.exprs.deinit(allocator);
        self.expr_ids_by_context.deinit(allocator);
        self.expr_child_entries.deinit(allocator);
        self.stmts.deinit(allocator);
        self.stmt_ids_by_key.deinit(allocator);
        self.stmt_child_entries.deinit(allocator);
    }

    pub fn getExpr(self: *const Program, expr_id: ExprId) *const Expr {
        return &self.exprs.items[@intFromEnum(expr_id)];
    }

    pub fn getExprPtr(self: *Program, expr_id: ExprId) *Expr {
        return &self.exprs.items[@intFromEnum(expr_id)];
    }

    pub fn getExprId(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ExprId {
        return self.expr_ids_by_context.get(ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx));
    }

    pub fn getExprChildren(self: *const Program, span: ExprIdSpan) []const ExprId {
        if (span.len == 0) return &.{};
        return self.expr_child_entries.items[span.start..][0..span.len];
    }

    pub fn getExprChild(self: *const Program, expr_id: ExprId, index: usize) ExprId {
        return self.getExprChildren(self.getExpr(expr_id).common().child_exprs)[index];
    }

    pub fn getStmt(self: *const Program, stmt_id: StmtId) *const Stmt {
        return &self.stmts.items[@intFromEnum(stmt_id)];
    }

    pub fn getStmtChildren(self: *const Program, span: ExprIdSpan) []const ExprId {
        if (span.len == 0) return &.{};
        return self.expr_child_entries.items[span.start..][0..span.len];
    }

    pub fn getBlockStmtChildren(self: *const Program, span: StmtIdSpan) []const StmtId {
        if (span.len == 0) return &.{};
        return self.stmt_child_entries.items[span.start..][0..span.len];
    }
};

fn Transform(comptime ResultPtr: type, comptime Driver: type) type {
    return struct {
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        result: ResultPtr,
        driver: Driver,
        in_progress_exprs: std.AutoHashMapUnmanaged(ContextMono.ContextExprKey, void),
        assembled_exprs: std.AutoHashMapUnmanaged(ContextMono.ContextExprKey, void),
        in_progress_callable_insts: std.AutoHashMapUnmanaged(CallableInstId, void),
        assembled_callable_insts: std.AutoHashMapUnmanaged(CallableInstId, void),

        const Self = @This();

        fn init(
            allocator: Allocator,
            all_module_envs: []const *ModuleEnv,
            result: ResultPtr,
            driver: Driver,
        ) Self {
            return .{
                .allocator = allocator,
                .all_module_envs = all_module_envs,
                .result = result,
                .driver = driver,
                .in_progress_exprs = .empty,
                .assembled_exprs = .empty,
                .in_progress_callable_insts = .empty,
                .assembled_callable_insts = .empty,
            };
        }

        fn deinit(self: *Self) void {
            self.in_progress_exprs.deinit(self.allocator);
            self.assembled_exprs.deinit(self.allocator);
            self.in_progress_callable_insts.deinit(self.allocator);
            self.assembled_callable_insts.deinit(self.allocator);
        }

        fn beginExprAssembly(self: *Self, key: ContextMono.ContextExprKey) Allocator.Error!bool {
            if (self.in_progress_exprs.contains(key)) return false;
            try self.in_progress_exprs.put(self.allocator, key, {});
            return true;
        }

        fn endExprAssembly(self: *Self, key: ContextMono.ContextExprKey) void {
            _ = self.in_progress_exprs.remove(key);
        }

        fn isExprAssembled(self: *const Self, key: ContextMono.ContextExprKey) bool {
            return self.assembled_exprs.contains(key);
        }

        fn markExprAssembled(self: *Self, key: ContextMono.ContextExprKey) Allocator.Error!void {
            try self.assembled_exprs.put(self.allocator, key, {});
        }

        fn beginCallableInstAssembly(self: *Self, callable_inst_id: CallableInstId) Allocator.Error!bool {
            if (self.in_progress_callable_insts.contains(callable_inst_id)) return false;
            try self.in_progress_callable_insts.put(self.allocator, callable_inst_id, {});
            return true;
        }

        fn endCallableInstAssembly(self: *Self, callable_inst_id: CallableInstId) void {
            _ = self.in_progress_callable_insts.remove(callable_inst_id);
        }

        fn isCallableInstAssembled(self: *const Self, callable_inst_id: CallableInstId) bool {
            return self.assembled_callable_insts.contains(callable_inst_id);
        }

        fn markCallableInstAssembled(self: *Self, callable_inst_id: CallableInstId) Allocator.Error!void {
            try self.assembled_callable_insts.put(self.allocator, callable_inst_id, {});
        }

        fn ensureExprRecord(
            self: *Self,
            source_context: SourceContext,
            module_idx: u32,
            expr_idx: CIR.Expr.Idx,
        ) Allocator.Error!*Expr {
            const monotype = try requireProgramExprMonotype(
                self.driver,
                self.result,
                source_context,
                module_idx,
                expr_idx,
            );
            const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
            const expr_id = self.result.lambdamono.expr_ids_by_context.get(key) orelse blk: {
                const new_expr_id: ExprId = @enumFromInt(self.result.lambdamono.exprs.items.len);
                try self.result.lambdamono.exprs.append(self.allocator, .{
                    .plain = .{
                        .source_context = source_context,
                        .module_idx = module_idx,
                        .source_expr = expr_idx,
                        .monotype = monotype,
                        .child_exprs = .empty(),
                        .child_stmts = .empty(),
                        .origin = .self,
                    },
                });
                try self.result.lambdamono.expr_ids_by_context.put(self.allocator, key, new_expr_id);
                break :blk new_expr_id;
            };
            const expr = self.result.lambdamono.getExprPtr(expr_id);
            if (!std.meta.eql(expr.common().monotype, monotype)) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Lambdamono invariant violated: expr monotype changed after reservation",
                        .{},
                    );
                }
                unreachable;
            }
            return expr;
        }

        fn appendExprChildren(self: *Self, child_exprs: []const ExprId) Allocator.Error!ExprIdSpan {
            const start: u32 = @intCast(self.result.lambdamono.expr_child_entries.items.len);
            try self.result.lambdamono.expr_child_entries.appendSlice(self.allocator, child_exprs);
            return .{
                .start = start,
                .len = @intCast(child_exprs.len),
            };
        }

        fn appendStmtChildren(self: *Self, child_stmts: []const StmtId) Allocator.Error!StmtIdSpan {
            const start: u32 = @intCast(self.result.lambdamono.stmt_child_entries.items.len);
            try self.result.lambdamono.stmt_child_entries.appendSlice(self.allocator, child_stmts);
            return .{
                .start = start,
                .len = @intCast(child_stmts.len),
            };
        }

        fn run(self: *Self, current_module_idx: u32, root_exprs: []const CIR.Expr.Idx) Allocator.Error!void {
            for (root_exprs) |expr_idx| {
                _ = try self.assembleProgramExprNode(
                    .{ .root_expr = .{
                        .module_idx = current_module_idx,
                        .expr_idx = expr_idx,
                    } },
                    current_module_idx,
                    expr_idx,
                );
            }
        }

        fn assembleProgramExprNode(
            self: *Self,
            source_context: SourceContext,
            module_idx: u32,
            expr_idx: CIR.Expr.Idx,
        ) Allocator.Error!ExprId {
            return self.assembleProgramExprNodeInternal(source_context, module_idx, expr_idx);
        }

        fn appendProgramExprChildIfPresent(
            self: *Self,
            child_exprs: *std.ArrayList(ExprId),
            source_context: SourceContext,
            module_idx: u32,
            expr_idx: CIR.Expr.Idx,
        ) Allocator.Error!void {
            if (self.result.getExprTemplateId(source_context, module_idx, expr_idx) != null) {
                if (!try exprHasExactProgramSemantics(self.driver, self.result, source_context, module_idx, expr_idx)) return;
            }

            try child_exprs.append(
                self.allocator,
                try self.assembleProgramExprNodeInternal(source_context, module_idx, expr_idx),
            );
        }

        fn assembleProgramExprNodeInternal(
            self: *Self,
            source_context: SourceContext,
            module_idx: u32,
            expr_idx: CIR.Expr.Idx,
        ) Allocator.Error!ExprId {
            const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
            const expr_id = self.result.lambdamono.expr_ids_by_context.get(key) orelse blk: {
                _ = try self.ensureExprRecord(source_context, module_idx, expr_idx);
                break :blk self.result.lambdamono.expr_ids_by_context.get(key).?;
            };
            if (self.isExprAssembled(key)) return expr_id;
            if (!try self.beginExprAssembly(key)) return expr_id;
            defer self.endExprAssembly(key);

            const module_env = self.all_module_envs[module_idx];
            const expr = module_env.store.getExpr(expr_idx);
            try requireProgramExprSemanticShape(self.driver, self.result, source_context, module_idx, expr_idx, expr);
            var child_exprs = std.ArrayList(ExprId).empty;
            defer child_exprs.deinit(self.allocator);
            var child_stmts = std.ArrayList(StmtId).empty;
            defer child_stmts.deinit(self.allocator);

            switch (expr) {
                .e_str => |str_expr| for (module_env.store.sliceExpr(str_expr.span)) |child_expr_idx| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, child_expr_idx);
                },
                .e_list => |list_expr| for (module_env.store.sliceExpr(list_expr.elems)) |child_expr_idx| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, child_expr_idx);
                },
                .e_tuple => |tuple_expr| for (module_env.store.sliceExpr(tuple_expr.elems)) |child_expr_idx| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, child_expr_idx);
                },
                .e_match => |match_expr| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, match_expr.cond);
                    for (module_env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                        const branch = module_env.store.getMatchBranch(branch_idx);
                        if (branch.guard) |guard_expr_idx| {
                            try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, guard_expr_idx);
                        }
                        try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, branch.value);
                    }
                },
                .e_if => |if_expr| {
                    for (module_env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                        const branch = module_env.store.getIfBranch(branch_idx);
                        try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, branch.cond);
                        try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, branch.body);
                    }
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, if_expr.final_else);
                },
                .e_call => |call_expr| {
                    const callee_expr = module_env.store.getExpr(call_expr.func);
                    const include_func_child = if (!calleeUsesFirstClassCallableValuePath(callee_expr))
                        false
                    else if (self.result.lambdamono.getExpr(expr_id).getCall()) |call_site|
                        switch (call_site) {
                            .direct => |callable_inst_id| switch (self.result.getCallableInst(callable_inst_id).runtime_value) {
                                .direct_lambda => false,
                                .closure => true,
                            },
                            .indirect_call => true,
                            .low_level => false,
                        }
                    else
                        true;
                    if (include_func_child) {
                        try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, call_expr.func);
                    }
                    for (module_env.store.sliceExpr(call_expr.args)) |arg_expr_idx| {
                        try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, arg_expr_idx);
                    }
                },
                .e_record => |record_expr| {
                    for (module_env.store.sliceRecordFields(record_expr.fields)) |field_idx| {
                        const field = module_env.store.getRecordField(field_idx);
                        try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, field.value);
                    }
                    if (record_expr.ext) |ext_expr_idx| {
                        try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, ext_expr_idx);
                    }
                },
                .e_block => |block_expr| {
                    for (module_env.store.sliceStatements(block_expr.stmts)) |stmt_idx| {
                        try child_stmts.append(
                            self.allocator,
                            try self.assembleProgramStmtNode(source_context, module_idx, stmt_idx),
                        );
                    }
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, block_expr.final_expr);
                },
                .e_tag => |tag_expr| for (module_env.store.sliceExpr(tag_expr.args)) |child_expr_idx| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, child_expr_idx);
                },
                .e_nominal => |nominal_expr| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, nominal_expr.backing_expr);
                },
                .e_nominal_external => |nominal_expr| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, nominal_expr.backing_expr);
                },
                .e_binop => |binop_expr| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, binop_expr.lhs);
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, binop_expr.rhs);
                },
                .e_unary_minus => |unary_expr| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, unary_expr.expr);
                },
                .e_unary_not => |unary_expr| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, unary_expr.expr);
                },
                .e_dot_access => |dot_expr| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, dot_expr.receiver);
                    if (dot_expr.args) |args| for (module_env.store.sliceExpr(args)) |arg_expr_idx| {
                        try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, arg_expr_idx);
                    };
                },
                .e_tuple_access => |tuple_access| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, tuple_access.tuple);
                },
                .e_dbg => |dbg_expr| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, dbg_expr.expr);
                },
                .e_expect => |expect_expr| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, expect_expr.body);
                },
                .e_return => |return_expr| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, return_expr.expr);
                },
                .e_type_var_dispatch => |dispatch_expr| for (module_env.store.sliceExpr(dispatch_expr.args)) |arg_expr_idx| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, arg_expr_idx);
                },
                .e_for => |for_expr| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, for_expr.expr);
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, for_expr.body);
                },
                .e_run_low_level => |run_low_level| for (module_env.store.sliceExpr(run_low_level.args)) |arg_expr_idx| {
                    try self.appendProgramExprChildIfPresent(&child_exprs, source_context, module_idx, arg_expr_idx);
                },
                else => {},
            }

            const child_expr_span = try self.appendExprChildren(child_exprs.items);
            const child_stmt_span = try self.appendStmtChildren(child_stmts.items);
            const program_expr = self.result.lambdamono.getExprPtr(expr_id);
            const common = program_expr.commonMut();
            common.source_context = source_context;
            common.module_idx = module_idx;
            common.source_expr = expr_idx;
            common.child_exprs = child_expr_span;
            common.child_stmts = child_stmt_span;
            try self.markExprAssembled(key);
            try self.ensureProgramExprDependencyGraphs(expr_id);
            return expr_id;
        }

        fn ensureProgramExprDependencyGraphs(self: *Self, expr_id: ExprId) Allocator.Error!void {
            const expr = self.result.lambdamono.getExpr(expr_id);
            const callable_value = expr.getCallableValue();
            const call_site = expr.getCall();

            if (callable_value) |value| {
                try self.ensureCallableValueBodyGraphs(value);
            }
            if (call_site) |resolved_call_site| {
                try self.ensureCallSiteBodyGraphs(resolved_call_site);
            }
        }

        fn ensureCallableValueBodyGraphs(self: *Self, callable_value: CallableValue) Allocator.Error!void {
            for (self.result.getCallableValueVariants(callable_value)) |callable_inst_id| {
                try self.ensureCallableInstBodyGraph(callable_inst_id);
            }
        }

        fn ensureCallSiteBodyGraphs(self: *Self, call_site: CallSite) Allocator.Error!void {
            for (self.result.getCallSiteVariants(call_site)) |callable_inst_id| {
                try self.ensureCallableInstBodyGraph(callable_inst_id);
            }
        }

        fn ensureCallableInstBodyGraph(self: *Self, callable_inst_id: CallableInstId) Allocator.Error!void {
            if (self.isCallableInstAssembled(callable_inst_id)) return;
            if (!try self.beginCallableInstAssembly(callable_inst_id)) return;
            defer self.endCallableInstAssembly(callable_inst_id);

            try requireCallableInstRealized(self.driver, self.result, callable_inst_id);

            const callable_inst = self.result.getCallableInst(callable_inst_id).*;
            const template = self.result.getCallableTemplate(callable_inst.template);
            switch (template.kind) {
                .lambda, .closure, .hosted_lambda => {},
                .top_level_def => std.debug.panic(
                    "Lambdamono invariant violated: callable inst {d} runtime expr {d} in module {d} retained top_level_def callable template kind after callable registration",
                    .{ @intFromEnum(callable_inst_id), @intFromEnum(template.runtime_expr), template.module_idx },
                ),
            }

            const callable_def = self.result.getCallableDefForInst(callable_inst_id).*;
            _ = try self.assembleProgramExprNode(
                callable_def.body_expr.source_context,
                callable_def.body_expr.module_idx,
                callable_def.body_expr.expr_idx,
            );
            for (self.result.getCaptureFields(callable_def.captures)) |capture_field| {
                switch (capture_field.source) {
                    .bound_expr => |bound_expr| try self.ensureProgramExprRefNode(bound_expr.expr_ref),
                    .lexical_binding => {},
                }
            }

            try self.markCallableInstAssembled(callable_inst_id);
        }

        fn assembleProgramStmtNode(
            self: *Self,
            source_context: SourceContext,
            module_idx: u32,
            stmt_idx: CIR.Statement.Idx,
        ) Allocator.Error!StmtId {
            const key = buildStmtKey(source_context, module_idx, stmt_idx);
            if (self.result.lambdamono.stmt_ids_by_key.get(key)) |existing| return existing;

            const module_env = self.all_module_envs[module_idx];
            const stmt = module_env.store.getStatement(stmt_idx);
            var child_exprs = std.ArrayList(ExprId).empty;
            defer child_exprs.deinit(self.allocator);

            switch (stmt) {
                .s_decl => |decl| {
                    if (try exprHasExactProgramSemantics(self.driver, self.result, source_context, module_idx, decl.expr)) {
                        try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, decl.expr));
                    }
                },
                .s_var => |var_decl| {
                    if (try exprHasExactProgramSemantics(self.driver, self.result, source_context, module_idx, var_decl.expr)) {
                        try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, var_decl.expr));
                    }
                },
                .s_reassign => |reassign| {
                    if (try exprHasExactProgramSemantics(self.driver, self.result, source_context, module_idx, reassign.expr)) {
                        try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, reassign.expr));
                    }
                },
                .s_dbg => |dbg_stmt| try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, dbg_stmt.expr)),
                .s_expr => |expr_stmt| try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, expr_stmt.expr)),
                .s_expect => |expect_stmt| try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, expect_stmt.body)),
                .s_for => |for_stmt| {
                    try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, for_stmt.expr));
                    try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, for_stmt.body));
                },
                .s_while => |while_stmt| {
                    try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, while_stmt.cond));
                    try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, while_stmt.body));
                },
                .s_return => |return_stmt| try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, return_stmt.expr)),
                else => {},
            }

            const stmt_id: StmtId = @enumFromInt(self.result.lambdamono.stmts.items.len);
            try self.result.lambdamono.stmts.append(self.allocator, .{
                .module_idx = module_idx,
                .source_stmt = stmt_idx,
                .child_exprs = try self.appendExprChildren(child_exprs.items),
            });
            try self.result.lambdamono.stmt_ids_by_key.put(self.allocator, key, stmt_id);
            return stmt_id;
        }

        fn ensureProgramExprRefNode(self: *Self, expr_ref: ExprRef) Allocator.Error!void {
            _ = try self.assembleProgramExprNodeInternal(
                expr_ref.source_context,
                expr_ref.module_idx,
                expr_ref.expr_idx,
            );
        }
    };
}

pub fn run(
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    current_module_idx: u32,
    result: anytype,
    driver: anytype,
    root_exprs: []const CIR.Expr.Idx,
) Allocator.Error!void {
    var transform = Transform(@TypeOf(result), @TypeOf(driver)).init(
        allocator,
        all_module_envs,
        result,
        driver,
    );
    defer transform.deinit();
    try transform.run(current_module_idx, root_exprs);
}

fn callableInstHasRealizedRuntimeExpr(result: anytype, callable_inst_id: CallableInstId) bool {
    const callable_def = result.getCallableDefForInst(callable_inst_id);
    return result.getExprCallableValue(
        callable_def.runtime_expr.source_context,
        callable_def.runtime_expr.module_idx,
        callable_def.runtime_expr.expr_idx,
    ) != null;
}
