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

pub const ExprCallableSemantics = union(enum) {
    callable: CallableValue,
    intro: struct {
        callable_value: CallableValue,
        callable_inst: CallableInstId,
    },
};

pub const LookupResolution = union(enum) {
    expr: ExprRef,
    def: Lambdasolved.ExternalDefSource,
};

pub const ExprRole = union(enum) {
    plain,
    callable: ExprCallableSemantics,
    call: CallSite,
    lookup: LookupResolution,
    dispatch: DispatchSemantics,
};

pub const BuilderState = struct {
    in_progress_exprs: std.AutoHashMapUnmanaged(ContextMono.ContextExprKey, void),
    assembled_exprs: std.AutoHashMapUnmanaged(ContextMono.ContextExprKey, void),

    pub fn init() BuilderState {
        return .{
            .in_progress_exprs = .empty,
            .assembled_exprs = .empty,
        };
    }

    pub fn deinit(self: *BuilderState, allocator: Allocator) void {
        self.in_progress_exprs.deinit(allocator);
        self.assembled_exprs.deinit(allocator);
    }

    pub fn clear(self: *BuilderState) void {
        self.in_progress_exprs.clearRetainingCapacity();
        self.assembled_exprs.clearRetainingCapacity();
    }

    pub fn beginExprAssembly(
        self: *BuilderState,
        allocator: Allocator,
        key: ContextMono.ContextExprKey,
    ) Allocator.Error!bool {
        if (self.in_progress_exprs.contains(key)) return false;
        try self.in_progress_exprs.put(allocator, key, {});
        return true;
    }

    pub fn endExprAssembly(self: *BuilderState, key: ContextMono.ContextExprKey) void {
        _ = self.in_progress_exprs.remove(key);
    }

    pub fn isExprAssembled(self: *const BuilderState, key: ContextMono.ContextExprKey) bool {
        return self.assembled_exprs.contains(key);
    }

    pub fn markExprAssembled(
        self: *BuilderState,
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
pub const BuildStmtKey = struct {
    source_context_kind: ContextMono.SourceContextKind,
    source_context_module_idx: u32,
    source_context_raw: u32,
    module_idx: u32,
    stmt_raw: u32,
};

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
/// monotype. Finalization/build progress must live in `BuilderState`, not in
/// the executable IR itself.
pub const Expr = struct {
    source_context: SourceContext,
    module_idx: u32,
    source_expr: CIR.Expr.Idx,
    monotype: ContextMono.ResolvedMonotype,
    child_exprs: ExprIdSpan = .empty(),
    child_stmts: StmtIdSpan = .empty(),
    role: ExprRole = .plain,
    origin: ValueOrigin = .self,

    pub fn getCallableValue(self: *const Expr) ?CallableValue {
        return switch (self.role) {
            .callable => |callable_semantics| switch (callable_semantics) {
                .callable => |callable_value| callable_value,
                .intro => |intro| intro.callable_value,
            },
            .plain, .call, .lookup, .dispatch => null,
        };
    }

    pub fn getCallableIntro(self: *const Expr) ?struct {
        callable_value: CallableValue,
        callable_inst: CallableInstId,
    } {
        return switch (self.role) {
            .callable => |callable_semantics| switch (callable_semantics) {
                .intro => |intro| intro,
                .callable => null,
            },
            .plain, .call, .lookup, .dispatch => null,
        };
    }

    pub fn getCallable(self: *const Expr) ?ExprCallableSemantics {
        return switch (self.role) {
            .callable => |callable_semantics| callable_semantics,
            .plain, .call, .lookup, .dispatch => null,
        };
    }

    pub fn getDirectCall(self: *const Expr) ?CallableInstId {
        return switch (self.role) {
            .call => |call_site| switch (call_site) {
                .direct => |callable_inst| callable_inst,
                .indirect_call, .low_level => null,
            },
            .plain, .callable, .lookup, .dispatch => null,
        };
    }

    pub fn getIndirectCall(self: *const Expr) ?IndirectCall {
        return switch (self.role) {
            .call => |call_site| switch (call_site) {
                .indirect_call => |indirect_call| indirect_call,
                .direct, .low_level => null,
            },
            .plain, .callable, .lookup, .dispatch => null,
        };
    }

    pub fn getLowLevelCall(self: *const Expr) ?CIR.Expr.LowLevel {
        return switch (self.role) {
            .call => |call_site| switch (call_site) {
                .low_level => |low_level| low_level,
                .direct, .indirect_call => null,
            },
            .plain, .callable, .lookup, .dispatch => null,
        };
    }

    pub fn getCall(self: *const Expr) ?CallSite {
        return switch (self.role) {
            .call => |call_site| call_site,
            .plain, .callable, .lookup, .dispatch => null,
        };
    }

    pub fn getLookupExpr(self: *const Expr) ?ExprRef {
        return switch (self.role) {
            .lookup => |lookup_resolution| switch (lookup_resolution) {
                .expr => |expr_ref| expr_ref,
                .def => null,
            },
            .plain, .callable, .call, .dispatch => null,
        };
    }

    pub fn getLookupDef(self: *const Expr) ?Lambdasolved.ExternalDefSource {
        return switch (self.role) {
            .lookup => |lookup_resolution| switch (lookup_resolution) {
                .def => |def_source| def_source,
                .expr => null,
            },
            .plain, .callable, .call, .dispatch => null,
        };
    }

    pub fn getLookup(self: *const Expr) ?LookupResolution {
        return switch (self.role) {
            .lookup => |lookup_resolution| lookup_resolution,
            .plain, .callable, .call, .dispatch => null,
        };
    }

    pub fn getDispatchTarget(self: *const Expr) ?DispatchSolved.DispatchExprTarget {
        return switch (self.role) {
            .dispatch => |dispatch_semantics| switch (dispatch_semantics) {
                .target => |target| target,
                .intrinsic => null,
            },
            .plain, .callable, .call, .lookup => null,
        };
    }

    pub fn getDispatchIntrinsic(self: *const Expr) ?DispatchIntrinsic {
        return switch (self.role) {
            .dispatch => |dispatch_semantics| switch (dispatch_semantics) {
                .intrinsic => |intrinsic| intrinsic,
                .target => null,
            },
            .plain, .callable, .call, .lookup => null,
        };
    }

    pub fn getDispatch(self: *const Expr) ?DispatchSemantics {
        return switch (self.role) {
            .dispatch => |dispatch_semantics| dispatch_semantics,
            .plain, .callable, .call, .lookup => null,
        };
    }

    pub fn getOriginExpr(self: *const Expr) ?ExprRef {
        return switch (self.origin) {
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

    pub fn getCallableVariantGroupVariants(self: *const Program, variant_group_id: CallableVariantGroupId) []const CallableInstId {
        const group = self.callable_variant_groups.items[@intFromEnum(variant_group_id)];
        if (group.variants.len == 0) return &.{};
        return self.callable_variant_entries.items[group.variants.start..][0..group.variants.len];
    }

    pub fn getDirectCallableVariants(self: *const Program, callable_inst_id: CallableInstId) []const CallableInstId {
        const variant_group_id = self.direct_callable_variant_group_ids_by_callable_inst.get(callable_inst_id) orelse unreachable;
        return self.getCallableVariantGroupVariants(variant_group_id);
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
        return self.getExprChildren(self.getExpr(expr_id).child_exprs)[index];
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
                .source_context = source_context,
                .module_idx = module_idx,
                .source_expr = expr_idx,
                .monotype = monotype,
                .child_exprs = .empty(),
                .child_stmts = .empty(),
                .role = .plain,
                .origin = .self,
            });
            try self.expr_ids_by_key.put(allocator, key, new_expr_id);
            break :blk new_expr_id;
        };
        const expr = self.getExprPtr(expr_id);
        if (!std.meta.eql(expr.monotype, monotype)) {
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
