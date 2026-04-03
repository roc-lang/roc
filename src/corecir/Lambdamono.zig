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
const Ident = base.Ident;
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

const AssemblyState = struct {
    in_progress_exprs: std.AutoHashMapUnmanaged(ContextMono.ContextExprKey, void),
    assembled_exprs: std.AutoHashMapUnmanaged(ContextMono.ContextExprKey, void),

    fn init() AssemblyState {
        return .{
            .in_progress_exprs = .empty,
            .assembled_exprs = .empty,
        };
    }

    fn deinit(self: *AssemblyState, allocator: Allocator) void {
        self.in_progress_exprs.deinit(allocator);
        self.assembled_exprs.deinit(allocator);
    }

    fn beginExprAssembly(
        self: *AssemblyState,
        allocator: Allocator,
        key: ContextMono.ContextExprKey,
    ) Allocator.Error!bool {
        if (self.in_progress_exprs.contains(key)) return false;
        try self.in_progress_exprs.put(allocator, key, {});
        return true;
    }

    fn endExprAssembly(self: *AssemblyState, key: ContextMono.ContextExprKey) void {
        _ = self.in_progress_exprs.remove(key);
    }

    fn isExprAssembled(self: *const AssemblyState, key: ContextMono.ContextExprKey) bool {
        return self.assembled_exprs.contains(key);
    }

    fn markExprAssembled(
        self: *AssemblyState,
        allocator: Allocator,
        key: ContextMono.ContextExprKey,
    ) Allocator.Error!void {
        try self.assembled_exprs.put(allocator, key, {});
    }
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

const CallableVariantGroup = struct {
    variants: CallableVariantSpan,
};

pub const CallableVariantGroupId = enum(u32) {
    _,
};

pub const ContextExprKey = ContextMono.ContextExprKey;
pub const ContextPatternKey = ContextMono.ContextPatternKey;
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
    return driver.requireRecordedExprMonotypeForSourceContext(
        result,
        SemanticThread.trackedThread(source_context),
        source_context,
        module_idx,
        expr_idx,
    );
}

pub fn ensureProgramExpr(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!*Expr {
    const monotype = try requireProgramExprMonotype(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
    );
    return result.lambdamono.ensureExpr(
        driver.allocator,
        source_context,
        module_idx,
        expr_idx,
        monotype,
    );
}

pub fn recordDispatchExprTarget(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    dispatch_target: DispatchSolved.DispatchExprTarget,
) Allocator.Error!void {
    try result.dispatch_solved.recordDispatchExprTarget(
        driver.allocator,
        source_context,
        module_idx,
        expr_idx,
        dispatch_target,
    );
    const semantics = try ensureProgramExpr(driver, result, source_context, module_idx, expr_idx);
    const common = semantics.common().*;
    const next_expr: Expr = .{ .dispatch_target = .{
        .common = common,
        .target = dispatch_target,
    } };
    if (!std.meta.eql(semantics.*, next_expr)) semantics.* = next_expr;
}

pub fn recordDispatchExprIntrinsic(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    dispatch_intrinsic: DispatchIntrinsic,
) Allocator.Error!void {
    const semantics = try ensureProgramExpr(driver, result, source_context, module_idx, expr_idx);
    const common = semantics.common().*;
    const next_expr: Expr = .{ .dispatch_intrinsic = .{
        .common = common,
        .intrinsic = dispatch_intrinsic,
    } };
    if (!std.meta.eql(semantics.*, next_expr)) semantics.* = next_expr;
}

pub fn exprHasExactProgramSemantics(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!bool {
    const thread = SemanticThread.trackedThread(source_context);
    const monotype = try driver.lookupRecordedExprMonotypeIfReadyForSourceContext(
        result,
        thread,
        source_context,
        module_idx,
        expr_idx,
    );
    return monotype != null;
}

pub fn ensureProgramExprSemanticShape(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    expr: CIR.Expr,
) Allocator.Error!void {
    switch (expr) {
        .e_lookup_local, .e_lookup_external, .e_lookup_required => {
            try Lambdasolved.realizeLookupExprSemantics(
                driver,
                result,
                SemanticThread.trackedThread(source_context),
                module_idx,
                expr_idx,
                expr,
            );
            var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
            defer visiting.deinit(driver.allocator);
            try Lambdasolved.realizeStructuredExprCallableSemantics(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                &visiting,
            );
        },
        .e_call => {
            var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
            defer visiting.deinit(driver.allocator);
            try Lambdasolved.realizeStructuredExprCallableSemantics(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                &visiting,
            );
        },
        .e_type_var_dispatch => {
            try Lambdasolved.realizeDispatchExprSemantics(
                driver,
                result,
                SemanticThread.trackedThread(source_context),
                module_idx,
                expr_idx,
                expr,
            );
        },
        .e_dot_access => |dot_expr| {
            if (dot_expr.args != null) {
                try Lambdasolved.realizeDispatchExprSemantics(
                    driver,
                    result,
                    SemanticThread.trackedThread(source_context),
                    module_idx,
                    expr_idx,
                    expr,
                );
            }
        },
        else => if (result.getExprTemplateId(source_context, module_idx, expr_idx) != null) {
            var visiting: std.AutoHashMapUnmanaged(ContextExprKey, void) = .empty;
            defer visiting.deinit(driver.allocator);
            try Lambdasolved.realizeStructuredExprCallableSemantics(
                driver,
                result,
                source_context,
                module_idx,
                expr_idx,
                &visiting,
            );
        },
    }
}

pub fn ensureCallableInstRealized(
    driver: anytype,
    result: anytype,
    callable_inst_id: CallableInstId,
) Allocator.Error!void {
    const callable_inst = result.getCallableInst(callable_inst_id);
    const template = result.getCallableTemplate(callable_inst.template);
    const callable_source_context: SourceContext = .{
        .callable_inst = @enumFromInt(@intFromEnum(callable_inst_id)),
    };
    if (result.getExprCallableValue(
        callable_source_context,
        template.module_idx,
        template.runtime_expr,
    ) != null) return;
    try Lambdasolved.realizeCallableInstWithFreshRootAnalysis(
        driver,
        result,
        callable_inst_id,
    );
}

fn ProgramAssemblyDriver(comptime PassPtr: type) type {
    return struct {
        pass: PassPtr,

        fn ensureCallableInstRealized(
            self: @This(),
            result: anytype,
            callable_inst_id: CallableInstId,
        ) Allocator.Error!void {
            return ensureCallableInstRealized(self.pass, result, callable_inst_id);
        }

        fn ensureProgramExpr(
            self: @This(),
            result: anytype,
            source_context: SourceContext,
            module_idx: u32,
            expr_idx: CIR.Expr.Idx,
        ) Allocator.Error!*Expr {
            return ensureProgramExpr(self.pass, result, source_context, module_idx, expr_idx);
        }

        fn exprHasExactProgramSemantics(
            self: @This(),
            result: anytype,
            source_context: SourceContext,
            module_idx: u32,
            expr_idx: CIR.Expr.Idx,
        ) Allocator.Error!bool {
            return exprHasExactProgramSemantics(self.pass, result, source_context, module_idx, expr_idx);
        }

        fn ensureProgramExprSemanticShape(
            self: @This(),
            result: anytype,
            source_context: SourceContext,
            module_idx: u32,
            expr_idx: CIR.Expr.Idx,
            expr: CIR.Expr,
        ) Allocator.Error!void {
            return ensureProgramExprSemanticShape(
                self.pass,
                result,
                source_context,
                module_idx,
                expr_idx,
                expr,
            );
        }
    };
}

pub fn programAssemblyDriver(pass: anytype) ProgramAssemblyDriver(@TypeOf(pass)) {
    return .{ .pass = pass };
}

pub const CaptureValueSource = union(enum) {
    lexical_pattern: struct {
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    },
    bound_expr: struct {
        binding_source_context: SourceContext,
        binding_module_idx: u32,
        binding_pattern_idx: CIR.Pattern.Idx,
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

pub const BindingId = enum(u32) {
    _,
};

pub const ValueOrigin = union(enum) {
    self,
    expr: ExprRef,
};

/// One finalized specialized pattern binding in the executable
/// `Lambdamono.Program`.
///
/// Invariant: this struct must never be used as mutable staging state during
/// specialization. If a consumer reads a `PatternBinding` from
/// `Program.pattern_bindings`, every recorded fact here is already final.
pub const PatternBinding = struct {
    key: ContextPatternKey,
    callable_value: ?CallableValue = null,
    origin: ValueOrigin = .self,
};

/// One finalized specialized expr in the executable `Lambdamono.Program`.
///
/// Invariant: every `Expr` stored in `Program.exprs` already has its exact
/// monotype. Finalization/build progress must live in `AssemblyState`, not in
/// the executable IR itself.
pub const ExprCommon = struct {
    source_context: SourceContext,
    module_idx: u32,
    source_expr: CIR.Expr.Idx,
    monotype: ContextMono.ResolvedMonotype,
    child_exprs: ExprIdSpan = .empty(),
    child_stmts: StmtIdSpan = .empty(),
    origin: ValueOrigin = .self,
};

pub const Expr = union(enum) {
    plain: ExprCommon,
    callable_value: struct {
        common: ExprCommon,
        callable_value: CallableValue,
    },
    callable_intro: struct {
        common: ExprCommon,
        intro: CallableIntro,
    },
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
            .plain => |*common| common,
            .callable_value => |*value| &value.common,
            .callable_intro => |*intro| &intro.common,
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
            .plain => |*common| common,
            .callable_value => |*value| &value.common,
            .callable_intro => |*intro| &intro.common,
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
        return switch (self.*) {
            .callable_value => |value| value.callable_value,
            .callable_intro => |intro| intro.intro.callable_value,
            else => null,
        };
    }

    pub fn getCallableIntro(self: *const Expr) ?CallableIntro {
        return switch (self.*) {
            .callable_intro => |intro| intro.intro,
            else => null,
        };
    }

    pub fn getCallable(self: *const Expr) ?ExprCallableSemantics {
        return switch (self.*) {
            .callable_value => |value| .{ .callable = value.callable_value },
            .callable_intro => |intro| .{ .intro = intro.intro },
            else => null,
        };
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
/// specialization/build state must live outside this struct. The same is true
/// for `pattern_bindings`.
pub const Program = struct {
    callable_insts: std.ArrayListUnmanaged(CallableInst),
    callable_variant_groups: std.ArrayListUnmanaged(CallableVariantGroup),
    direct_callable_variant_group_ids_by_callable_inst: std.AutoHashMapUnmanaged(CallableInstId, CallableVariantGroupId),
    callable_param_spec_entries: std.ArrayListUnmanaged(CallableParamSpecEntry),
    value_projection_entries: std.ArrayListUnmanaged(CallableParamProjection),
    pattern_entries: std.ArrayListUnmanaged(CIR.Pattern.Idx),
    pattern_bindings: std.ArrayListUnmanaged(PatternBinding),
    pattern_binding_ids_by_key: std.AutoHashMapUnmanaged(ContextPatternKey, BindingId),
    exprs: std.ArrayListUnmanaged(Expr),
    expr_ids_by_key: std.AutoHashMapUnmanaged(ContextExprKey, ExprId),
    expr_child_entries: std.ArrayListUnmanaged(ExprId),
    stmts: std.ArrayListUnmanaged(Stmt),
    stmt_ids_by_key: std.AutoHashMapUnmanaged(BuildStmtKey, StmtId),
    stmt_child_entries: std.ArrayListUnmanaged(StmtId),
    callable_defs: std.ArrayListUnmanaged(CallableDef),
    capture_fields: std.ArrayListUnmanaged(CaptureField),
    callable_variant_entries: std.ArrayListUnmanaged(CallableInstId),

    pub fn init() Program {
        return .{
            .callable_insts = .empty,
            .callable_variant_groups = .empty,
            .direct_callable_variant_group_ids_by_callable_inst = .empty,
            .callable_param_spec_entries = .empty,
            .value_projection_entries = .empty,
            .pattern_entries = .empty,
            .pattern_bindings = .empty,
            .pattern_binding_ids_by_key = .empty,
            .exprs = .empty,
            .expr_ids_by_key = .empty,
            .expr_child_entries = .empty,
            .stmts = .empty,
            .stmt_ids_by_key = .empty,
            .stmt_child_entries = .empty,
            .callable_defs = .empty,
            .capture_fields = .empty,
            .callable_variant_entries = .empty,
        };
    }

    pub fn deinit(self: *Program, allocator: Allocator) void {
        self.callable_insts.deinit(allocator);
        self.callable_variant_groups.deinit(allocator);
        self.direct_callable_variant_group_ids_by_callable_inst.deinit(allocator);
        self.callable_param_spec_entries.deinit(allocator);
        self.value_projection_entries.deinit(allocator);
        self.pattern_entries.deinit(allocator);
        self.pattern_bindings.deinit(allocator);
        self.pattern_binding_ids_by_key.deinit(allocator);
        self.exprs.deinit(allocator);
        self.expr_ids_by_key.deinit(allocator);
        self.expr_child_entries.deinit(allocator);
        self.stmts.deinit(allocator);
        self.stmt_ids_by_key.deinit(allocator);
        self.stmt_child_entries.deinit(allocator);
        self.callable_defs.deinit(allocator);
        self.capture_fields.deinit(allocator);
        self.callable_variant_entries.deinit(allocator);
    }

    pub fn getCallableInst(self: *const Program, callable_inst_id: CallableInstId) *const CallableInst {
        return &self.callable_insts.items[@intFromEnum(callable_inst_id)];
    }

    pub fn internCallableVariantGroup(
        self: *Program,
        allocator: Allocator,
        variants: []const CallableInstId,
    ) Allocator.Error!CallableVariantGroupId {
        for (self.callable_variant_groups.items, 0..) |_, idx| {
            const existing_variants = self.getCallableVariantGroupVariants(@enumFromInt(idx));
            if (existing_variants.len != variants.len) continue;

            var matches = true;
            for (existing_variants, variants) |lhs, rhs| {
                if (lhs != rhs) {
                    matches = false;
                    break;
                }
            }
            if (matches) return @enumFromInt(idx);
        }

        const span: CallableVariantSpan = if (variants.len == 0)
            CallableVariantSpan.empty()
        else blk: {
            const start: u32 = @intCast(self.callable_variant_entries.items.len);
            try self.callable_variant_entries.appendSlice(allocator, variants);
            break :blk .{
                .start = start,
                .len = @intCast(variants.len),
            };
        };

        const variant_group_id: CallableVariantGroupId = @enumFromInt(self.callable_variant_groups.items.len);
        try self.callable_variant_groups.append(allocator, .{ .variants = span });
        return variant_group_id;
    }

    pub fn ensureDirectCallableVariantGroup(
        self: *Program,
        allocator: Allocator,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!void {
        if (self.direct_callable_variant_group_ids_by_callable_inst.contains(callable_inst_id)) return;
        const variant_group_id = try self.internCallableVariantGroup(allocator, &.{callable_inst_id});
        try self.direct_callable_variant_group_ids_by_callable_inst.put(allocator, callable_inst_id, variant_group_id);
    }

    pub fn makePackedFn(
        self: *Program,
        allocator: Allocator,
        callable_inst_ids: []const CallableInstId,
        fn_monotype: ContextMono.ResolvedMonotype,
        runtime_monotype: ContextMono.ResolvedMonotype,
    ) Allocator.Error!PackedFn {
        const variant_group = try self.internCallableVariantGroup(allocator, callable_inst_ids);
        return .{
            .variant_group = variant_group,
            .fn_monotype = fn_monotype,
            .runtime_monotype = runtime_monotype,
        };
    }

    pub fn makeIndirectCall(
        self: *Program,
        allocator: Allocator,
        callable_inst_ids: []const CallableInstId,
        fn_monotype: ContextMono.ResolvedMonotype,
        runtime_monotype: ContextMono.ResolvedMonotype,
    ) Allocator.Error!IndirectCall {
        return .{
            .packed_fn = try self.makePackedFn(
                allocator,
                callable_inst_ids,
                fn_monotype,
                runtime_monotype,
            ),
        };
    }

    pub fn getCallableInstRuntimeMonotype(
        self: *const Program,
        unit_monotype: Monotype.Idx,
        callable_inst_id: CallableInstId,
    ) ContextMono.ResolvedMonotype {
        const callable_inst = self.getCallableInst(callable_inst_id);
        return switch (callable_inst.runtime_value) {
            .direct_lambda => .{
                .idx = unit_monotype,
                .module_idx = callable_inst.fn_monotype_module_idx,
            },
            .closure => |closure| closure.capture_tuple_monotype,
        };
    }

    fn packedCallableTagName(
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        module_idx: u32,
        callable_inst_id: CallableInstId,
    ) Allocator.Error!Monotype.Name {
        var name_buf: [32]u8 = undefined;
        const name_text = std.fmt.bufPrint(&name_buf, "__roc_fn_{d:0>10}", .{@intFromEnum(callable_inst_id)}) catch unreachable;
        const module_env = all_module_envs[module_idx];
        const ident = module_env.common.findIdent(name_text) orelse
            try module_env.common.insertIdent(allocator, Ident.for_text(name_text));
        return .{
            .module_idx = module_idx,
            .ident = ident,
        };
    }

    fn requireUniformPackedCallableFnMonotype(
        self: *const Program,
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        context_mono: *const ContextMono.Result,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!ContextMono.ResolvedMonotype {
        if (callable_inst_ids.len == 0) unreachable;

        const first_callable = self.getCallableInst(callable_inst_ids[0]);
        const first_resolved: ContextMono.ResolvedMonotype = .{
            .idx = first_callable.fn_monotype,
            .module_idx = first_callable.fn_monotype_module_idx,
        };
        for (callable_inst_ids[1..]) |callable_inst_id| {
            const callable_inst = self.getCallableInst(callable_inst_id);
            if (!try context_mono.monotypesStructurallyEqualAcrossModules(
                allocator,
                all_module_envs,
                first_resolved.idx,
                first_resolved.module_idx,
                callable_inst.fn_monotype,
                callable_inst.fn_monotype_module_idx,
            )) {
                std.debug.panic(
                    "Lambdamono invariant violated: callable variant set contained mismatched fn monotypes between callable insts {d} and {d}",
                    .{ @intFromEnum(callable_inst_ids[0]), @intFromEnum(callable_inst_id) },
                );
            }
        }
        return first_resolved;
    }

    fn buildPackedFnRuntimeMonotype(
        self: *const Program,
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        current_module_idx: u32,
        context_mono: *ContextMono.Result,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!ContextMono.ResolvedMonotype {
        if (callable_inst_ids.len == 0) unreachable;

        var tags = std.ArrayList(Monotype.Tag).empty;
        defer tags.deinit(allocator);

        for (callable_inst_ids) |callable_inst_id| {
            const payload_mono = self.getCallableInstRuntimeMonotype(
                context_mono.monotype_store.unit_idx,
                callable_inst_id,
            );
            const payloads = if (payload_mono.isNone() or payload_mono.idx == context_mono.monotype_store.unit_idx)
                Monotype.Span.empty()
            else
                try context_mono.monotype_store.addIdxSpan(allocator, &.{payload_mono.idx});
            try tags.append(allocator, .{
                .name = try packedCallableTagName(allocator, all_module_envs, current_module_idx, callable_inst_id),
                .payloads = payloads,
            });
        }

        std.mem.sortUnstable(Monotype.Tag, tags.items, all_module_envs, Monotype.Tag.sortByNameAsc);
        const tag_span = try context_mono.monotype_store.addTags(allocator, tags.items);
        return .{
            .idx = try context_mono.monotype_store.addMonotype(allocator, .{
                .tag_union = .{ .tags = tag_span },
            }),
            .module_idx = current_module_idx,
        };
    }

    pub fn makePackedFnForCallableInsts(
        self: *Program,
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        current_module_idx: u32,
        context_mono: *ContextMono.Result,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!PackedFn {
        const fn_monotype = try self.requireUniformPackedCallableFnMonotype(
            allocator,
            all_module_envs,
            context_mono,
            callable_inst_ids,
        );
        const runtime_monotype = try self.buildPackedFnRuntimeMonotype(
            allocator,
            all_module_envs,
            current_module_idx,
            context_mono,
            callable_inst_ids,
        );
        return self.makePackedFn(allocator, callable_inst_ids, fn_monotype, runtime_monotype);
    }

    pub fn makeIndirectCallForCallableInsts(
        self: *Program,
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        current_module_idx: u32,
        context_mono: *ContextMono.Result,
        callable_inst_ids: []const CallableInstId,
    ) Allocator.Error!IndirectCall {
        const packed_fn = try self.makePackedFnForCallableInsts(
            allocator,
            all_module_envs,
            current_module_idx,
            context_mono,
            callable_inst_ids,
        );
        return .{ .packed_fn = packed_fn };
    }

    pub fn getCallableVariantGroupVariants(self: *const Program, variant_group_id: CallableVariantGroupId) []const CallableInstId {
        const group = self.callable_variant_groups.items[@intFromEnum(variant_group_id)];
        if (group.variants.len == 0) return &.{};
        return self.callable_variant_entries.items[group.variants.start..][0..group.variants.len];
    }

    pub fn getDirectCallableVariants(self: *const Program, callable_inst_id: CallableInstId) []const CallableInstId {
        const variant_group_id = self.direct_callable_variant_group_ids_by_callable_inst.get(callable_inst_id) orelse unreachable;
        return self.getCallableVariantGroupVariants(variant_group_id);
    }

    pub fn getPackedFnVariants(self: *const Program, packed_fn: PackedFn) []const CallableInstId {
        return self.getCallableVariantGroupVariants(packed_fn.variant_group);
    }

    pub fn getIndirectCallVariants(self: *const Program, indirect_call: IndirectCall) []const CallableInstId {
        return self.getCallableVariantGroupVariants(indirect_call.packed_fn.variant_group);
    }

    pub fn getCallableValueVariants(self: *const Program, callable_value: CallableValue) []const CallableInstId {
        return switch (callable_value) {
            .direct => |callable_inst_id| self.getDirectCallableVariants(callable_inst_id),
            .packed_fn => |packed_fn| self.getPackedFnVariants(packed_fn),
        };
    }

    pub fn getCallSiteVariants(self: *const Program, call_site: CallSite) []const CallableInstId {
        return switch (call_site) {
            .direct => |callable_inst_id| self.getDirectCallableVariants(callable_inst_id),
            .indirect_call => |indirect_call| self.getIndirectCallVariants(indirect_call),
            .low_level => &.{},
        };
    }

    pub fn getCallableParamSpecEntries(
        self: *const Program,
        span: CallableParamSpecSpan,
    ) []const CallableParamSpecEntry {
        if (span.len == 0) return &.{};
        return self.callable_param_spec_entries.items[span.start..][0..span.len];
    }

    pub fn getCallableParamProjectionEntries(
        self: *const Program,
        span: CallableParamProjectionSpan,
    ) []const CallableParamProjection {
        if (span.len == 0) return &.{};
        return self.value_projection_entries.items[span.start..][0..span.len];
    }

    pub fn getValueProjectionEntries(
        self: *const Program,
        span: ValueProjection.ProjectionSpan,
    ) []const ValueProjection.Projection {
        if (span.len == 0) return &.{};
        return self.value_projection_entries.items[span.start..][0..span.len];
    }

    pub fn getPatternBinding(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?*const PatternBinding {
        const binding_id = self.pattern_binding_ids_by_key.get(
            ContextMono.Result.contextPatternKey(source_context, module_idx, pattern_idx),
        ) orelse return null;
        return &self.pattern_bindings.items[@intFromEnum(binding_id)];
    }

    pub fn getPatternCallableValue(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableValue {
        const binding = self.getPatternBinding(source_context, module_idx, pattern_idx) orelse return null;
        return binding.callable_value;
    }

    pub fn getPatternOriginExpr(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ExprRef {
        const binding = self.getPatternBinding(source_context, module_idx, pattern_idx) orelse return null;
        return switch (binding.origin) {
            .self => null,
            .expr => |origin| origin,
        };
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
        return self.expr_ids_by_key.get(ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx));
    }

    pub fn getExprChildren(self: *const Program, span: ExprIdSpan) []const ExprId {
        if (span.len == 0) return &.{};
        return self.expr_child_entries.items[span.start..][0..span.len];
    }

    pub fn getExprChild(self: *const Program, expr_id: ExprId, index: usize) ExprId {
        return self.getExprChildren(self.getExpr(expr_id).common().child_exprs)[index];
    }

    pub fn ensureExpr(
        self: *Program,
        allocator: Allocator,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        monotype: ContextMono.ResolvedMonotype,
    ) Allocator.Error!*Expr {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const expr_id = self.expr_ids_by_key.get(key) orelse blk: {
            const new_expr_id: ExprId = @enumFromInt(self.exprs.items.len);
            try self.exprs.append(allocator, .{
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
            try self.expr_ids_by_key.put(allocator, key, new_expr_id);
            break :blk new_expr_id;
        };
        const expr = self.getExprPtr(expr_id);
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

    pub fn getStmt(self: *const Program, stmt_id: StmtId) *const Stmt {
        return &self.stmts.items[@intFromEnum(stmt_id)];
    }

    pub fn getStmtChildren(self: *const Program, span: ExprIdSpan) []const ExprId {
        if (span.len == 0) return &.{};
        return self.expr_child_entries.items[span.start..][0..span.len];
    }

    pub fn getPatternIds(self: *const Program, span: PatternIdSpan) []const CIR.Pattern.Idx {
        if (span.len == 0) return &.{};
        return self.pattern_entries.items[span.start..][0..span.len];
    }

    pub fn ensurePatternBinding(
        self: *Program,
        allocator: Allocator,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) Allocator.Error!*PatternBinding {
        const key = ContextMono.Result.contextPatternKey(source_context, module_idx, pattern_idx);
        const binding_id = self.pattern_binding_ids_by_key.get(key) orelse blk: {
            const new_binding_id: BindingId = @enumFromInt(self.pattern_bindings.items.len);
            try self.pattern_bindings.append(allocator, .{
                .key = key,
                .callable_value = null,
                .origin = .self,
            });
            try self.pattern_binding_ids_by_key.put(allocator, key, new_binding_id);
            break :blk new_binding_id;
        };
        return &self.pattern_bindings.items[@intFromEnum(binding_id)];
    }

    pub fn appendExprChildren(
        self: *Program,
        allocator: Allocator,
        child_exprs: []const ExprId,
    ) Allocator.Error!ExprIdSpan {
        const start: u32 = @intCast(self.expr_child_entries.items.len);
        try self.expr_child_entries.appendSlice(allocator, child_exprs);
        return .{
            .start = start,
            .len = @intCast(child_exprs.len),
        };
    }

    pub fn getBlockStmtChildren(self: *const Program, span: StmtIdSpan) []const StmtId {
        if (span.len == 0) return &.{};
        return self.stmt_child_entries.items[span.start..][0..span.len];
    }

    pub fn appendStmtChildren(
        self: *Program,
        allocator: Allocator,
        child_stmts: []const StmtId,
    ) Allocator.Error!StmtIdSpan {
        const start: u32 = @intCast(self.stmt_child_entries.items.len);
        try self.stmt_child_entries.appendSlice(allocator, child_stmts);
        return .{
            .start = start,
            .len = @intCast(child_stmts.len),
        };
    }
};

fn ProgramAssembler(comptime ResultPtr: type, comptime Driver: type) type {
    return struct {
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        result: ResultPtr,
        driver: Driver,
        assembly: *AssemblyState,

        const Self = @This();

        fn assembleRoots(self: *Self, current_module_idx: u32, root_exprs: []const CIR.Expr.Idx) Allocator.Error!void {
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

        fn assembleCallableDefExprGraph(self: *Self) Allocator.Error!void {
            var callable_inst_idx: usize = 0;
            while (callable_inst_idx < self.result.lambdamono.callable_insts.items.len) : (callable_inst_idx += 1) {
                const callable_inst_id: CallableInstId = @enumFromInt(callable_inst_idx);
                try self.driver.ensureCallableInstRealized(self.result, callable_inst_id);
                const callable_inst = self.result.getCallableInst(callable_inst_id).*;
                const template = self.result.getCallableTemplate(callable_inst.template);
                switch (template.kind) {
                    .lambda, .closure, .hosted_lambda => {},
                    .top_level_def => std.debug.panic(
                        "Lambdamono invariant violated: callable inst {d} runtime expr {d} in module {d} retained top_level_def callable template kind after callable registration",
                        .{ @intFromEnum(callable_inst_id), @intFromEnum(template.runtime_expr), template.module_idx },
                    ),
                }
                const callable_def = self.result.lambdamono.callable_defs.items[@intFromEnum(callable_inst.callable_def)];
                _ = try self.assembleProgramExprNode(
                    callable_def.body_expr.source_context,
                    callable_def.body_expr.module_idx,
                    callable_def.body_expr.expr_idx,
                );
                for (self.result.getCaptureFields(callable_def.captures)) |capture_field| {
                    switch (capture_field.source) {
                        .bound_expr => |bound_expr| try self.ensureProgramExprRefNode(bound_expr.expr_ref),
                        .lexical_pattern => {},
                    }
                }
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
                if (!try self.driver.exprHasExactProgramSemantics(
                    self.result,
                    source_context,
                    module_idx,
                    expr_idx,
                )) return;
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
            const expr_id = self.result.lambdamono.expr_ids_by_key.get(key) orelse blk: {
                _ = try self.driver.ensureProgramExpr(self.result, source_context, module_idx, expr_idx);
                break :blk self.result.lambdamono.expr_ids_by_key.get(key).?;
            };
            if (self.assembly.isExprAssembled(key)) return expr_id;
            if (!try self.assembly.beginExprAssembly(self.allocator, key)) return expr_id;
            defer self.assembly.endExprAssembly(key);

            const module_env = self.all_module_envs[module_idx];
            const expr = module_env.store.getExpr(expr_idx);
            try self.driver.ensureProgramExprSemanticShape(
                self.result,
                source_context,
                module_idx,
                expr_idx,
                expr,
            );
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

            const child_expr_span = try self.result.lambdamono.appendExprChildren(self.allocator, child_exprs.items);
            const child_stmt_span = try self.result.lambdamono.appendStmtChildren(self.allocator, child_stmts.items);
            const program_expr = self.result.lambdamono.getExprPtr(expr_id);
            const common = program_expr.commonMut();
            common.source_context = source_context;
            common.module_idx = module_idx;
            common.source_expr = expr_idx;
            common.child_exprs = child_expr_span;
            common.child_stmts = child_stmt_span;
            try self.assembly.markExprAssembled(self.allocator, key);
            return expr_id;
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
                    if (try self.driver.exprHasExactProgramSemantics(self.result, source_context, module_idx, decl.expr)) {
                        try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, decl.expr));
                    }
                },
                .s_var => |var_decl| {
                    if (try self.driver.exprHasExactProgramSemantics(self.result, source_context, module_idx, var_decl.expr)) {
                        try child_exprs.append(self.allocator, try self.assembleProgramExprNode(source_context, module_idx, var_decl.expr));
                    }
                },
                .s_reassign => |reassign| {
                    if (try self.driver.exprHasExactProgramSemantics(self.result, source_context, module_idx, reassign.expr)) {
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
                .child_exprs = try self.result.lambdamono.appendExprChildren(self.allocator, child_exprs.items),
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

pub fn assembleRoots(
    allocator: Allocator,
    all_module_envs: []const *ModuleEnv,
    current_module_idx: u32,
    result: anytype,
    driver: anytype,
    root_exprs: []const CIR.Expr.Idx,
) Allocator.Error!void {
    var assembly = AssemblyState.init();
    defer assembly.deinit(allocator);

    var assembler = ProgramAssembler(@TypeOf(result), @TypeOf(driver)){
        .allocator = allocator,
        .all_module_envs = all_module_envs,
        .result = result,
        .driver = driver,
        .assembly = &assembly,
    };
    try assembler.assembleRoots(current_module_idx, root_exprs);
    try assembler.assembleCallableDefExprGraph();
}

pub fn getCallableDef(program: *const Program, callable_def_id: CallableDefId) *const CallableDef {
    return &program.callable_defs.items[@intFromEnum(callable_def_id)];
}

pub fn getCaptureFields(program: *const Program, span: CaptureFieldSpan) []const CaptureField {
    if (span.len == 0) return &.{};
    return program.capture_fields.items[span.start..][0..span.len];
}

pub fn getCallableVariants(program: *const Program, span: CallableVariantSpan) []const CallableInstId {
    if (span.len == 0) return &.{};
    return program.callable_variant_entries.items[span.start..][0..span.len];
}
