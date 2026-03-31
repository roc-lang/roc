//! Specialization from solved callable semantics to executable structured callable IR.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const ContextMono = @import("ContextMono.zig");
const Lambdasolved = @import("Lambdasolved.zig");
const Monotype = @import("Monotype.zig");

const Allocator = std.mem.Allocator;
const Region = base.Region;
const CIR = can.CIR;

/// Identifies one executable callable specialization chosen from solved
/// callable semantics.
pub const CallableInstId = enum(u32) {
    _,
};

/// One structural projection applied before demanding executable callable
/// specializations from a higher-order parameter.
pub const CallableParamProjection = union(enum) {
    field: Monotype.Name,
    tuple_elem: u32,
};

pub const CallableParamProjectionSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableParamProjectionSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableParamProjectionSpan) bool {
        return self.len == 0;
    }
};

pub const CallableParamSpecEntry = struct {
    param_index: u16,
    projections: CallableParamProjectionSpan = .empty(),
    callable_member_set_id: CallableMemberSetId,
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

pub const CallableInst = struct {
    template: Lambdasolved.CallableTemplateId,
    subst: ContextMono.TypeSubstId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    defining_context_callable_inst: ?CallableInstId,
    callable_param_specs: CallableParamSpecSpan = .empty(),
    callable_def: CallableDefId,
    runtime_value: RuntimeValue = .direct_lambda,
};

pub const RuntimeValue = union(enum) {
    direct_lambda,
    closure: struct {
        capture_tuple_monotype: ContextMono.ResolvedMonotype,
    },
};

pub const PackedCallableId = enum(u32) {
    _,
};

pub const PackedCallable = struct {
    members: CallableMemberSpan,
};

pub const DispatchCallId = enum(u32) {
    _,
};

pub const DispatchCall = struct {
    members: CallableMemberSpan,
};

pub const ExprId = enum(u32) {
    _,
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

pub const RootExprId = enum(u32) {
    _,
};

pub const CallableValue = union(enum) {
    direct: CallableInstId,
    packed_callable: PackedCallableId,
};

pub const CallDispatch = union(enum) {
    direct: CallableInstId,
    dispatch_call: DispatchCallId,
};

pub const LookupResolution = union(enum) {
    source_expr: Lambdasolved.ExprSource,
    def: Lambdasolved.ExternalDefSource,
};

pub const CallableMemberSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableMemberSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableMemberSpan) bool {
        return self.len == 0;
    }
};

const CallableMemberSet = struct {
    members: CallableMemberSpan,
};

pub const CallableMemberSetId = enum(u32) {
    _,
};

pub const ContextExprKey = ContextMono.ContextExprKey;
pub const ContextPatternKey = ContextMono.ContextPatternKey;
pub const SourceContext = ContextMono.SourceContext;

pub const CaptureValueSource = union(enum) {
    lexical_pattern: struct {
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    },
    source_expr: struct {
        source: Lambdasolved.ExprSource,
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

pub const CaptureField = struct {
    pattern_idx: CIR.Pattern.Idx,
    local_monotype: ContextMono.ResolvedMonotype,
    exact_callable_inst: ?CallableInstId = null,
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
    source_member: Lambdasolved.LambdaSetMemberId,
    module_idx: u32,
    source_expr: CIR.Expr.Idx,
    fn_monotype: ContextMono.ResolvedMonotype,
    param_bindings: CallableParamBindingSpan = .empty(),
    captures: CaptureFieldSpan = .empty(),
    source_region: Region,
};

pub const CallableParamBinding = struct {
    pattern_idx: CIR.Pattern.Idx,
    callable_value: CallableValue,
};

pub const CallableParamBindingSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() CallableParamBindingSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: CallableParamBindingSpan) bool {
        return self.len == 0;
    }
};

pub const ExprSemantics = struct {
    callable_value: ?CallableValue = null,
    call_dispatch: ?CallDispatch = null,
    dispatch_target: ?ContextMono.DispatchExprTarget = null,
    lookup_resolution: ?LookupResolution = null,
};

pub const Expr = struct {
    module_idx: u32,
    source_expr: CIR.Expr.Idx,
    child_exprs: ExprIdSpan = .empty(),
    child_stmts: StmtIdSpan = .empty(),
    semantics: ExprSemantics = .{},
};

pub const Stmt = struct {
    module_idx: u32,
    source_stmt: CIR.Statement.Idx,
    child_exprs: ExprIdSpan = .empty(),
};

pub const RootExpr = struct {
    key: ContextExprKey,
    body_expr: ExprId,
};

pub const Program = struct {
    callable_insts: std.ArrayListUnmanaged(CallableInst),
    callable_inst_ids_by_source_member: std.AutoHashMapUnmanaged(Lambdasolved.LambdaSetMemberId, CallableInstId),
    callable_param_spec_entries: std.ArrayListUnmanaged(CallableParamSpecEntry),
    callable_param_projection_entries: std.ArrayListUnmanaged(CallableParamProjection),
    callable_member_sets: std.ArrayListUnmanaged(CallableMemberSet),
    singleton_callable_member_set_ids_by_callable_inst: std.AutoHashMapUnmanaged(CallableInstId, CallableMemberSetId),
    packed_callable_ids_by_member_set: std.AutoHashMapUnmanaged(CallableMemberSetId, PackedCallableId),
    dispatch_call_ids_by_member_set: std.AutoHashMapUnmanaged(CallableMemberSetId, DispatchCallId),
    callable_param_bindings: std.ArrayListUnmanaged(CallableParamBinding),
    exprs: std.ArrayListUnmanaged(Expr),
    expr_ids_by_key: std.AutoHashMapUnmanaged(ContextExprKey, ExprId),
    expr_child_entries: std.ArrayListUnmanaged(ExprId),
    stmts: std.ArrayListUnmanaged(Stmt),
    stmt_child_entries: std.ArrayListUnmanaged(StmtId),
    root_exprs: std.ArrayListUnmanaged(RootExpr),
    root_expr_ids_by_key: std.AutoHashMapUnmanaged(ContextExprKey, RootExprId),
    callable_defs: std.ArrayListUnmanaged(CallableDef),
    capture_fields: std.ArrayListUnmanaged(CaptureField),
    callable_member_entries: std.ArrayListUnmanaged(CallableInstId),
    packed_callables: std.ArrayListUnmanaged(PackedCallable),
    dispatch_calls: std.ArrayListUnmanaged(DispatchCall),

    pub fn init() Program {
        return .{
            .callable_insts = .empty,
            .callable_inst_ids_by_source_member = .empty,
            .callable_param_spec_entries = .empty,
            .callable_param_projection_entries = .empty,
            .callable_member_sets = .empty,
            .singleton_callable_member_set_ids_by_callable_inst = .empty,
            .packed_callable_ids_by_member_set = .empty,
            .dispatch_call_ids_by_member_set = .empty,
            .callable_param_bindings = .empty,
            .exprs = .empty,
            .expr_ids_by_key = .empty,
            .expr_child_entries = .empty,
            .stmts = .empty,
            .stmt_child_entries = .empty,
            .root_exprs = .empty,
            .root_expr_ids_by_key = .empty,
            .callable_defs = .empty,
            .capture_fields = .empty,
            .callable_member_entries = .empty,
            .packed_callables = .empty,
            .dispatch_calls = .empty,
        };
    }

    pub fn deinit(self: *Program, allocator: Allocator) void {
        self.callable_insts.deinit(allocator);
        self.callable_inst_ids_by_source_member.deinit(allocator);
        self.callable_param_spec_entries.deinit(allocator);
        self.callable_param_projection_entries.deinit(allocator);
        self.callable_member_sets.deinit(allocator);
        self.singleton_callable_member_set_ids_by_callable_inst.deinit(allocator);
        self.packed_callable_ids_by_member_set.deinit(allocator);
        self.dispatch_call_ids_by_member_set.deinit(allocator);
        self.callable_param_bindings.deinit(allocator);
        self.exprs.deinit(allocator);
        self.expr_ids_by_key.deinit(allocator);
        self.expr_child_entries.deinit(allocator);
        self.stmts.deinit(allocator);
        self.stmt_child_entries.deinit(allocator);
        self.root_exprs.deinit(allocator);
        self.root_expr_ids_by_key.deinit(allocator);
        self.callable_defs.deinit(allocator);
        self.capture_fields.deinit(allocator);
        self.callable_member_entries.deinit(allocator);
        self.packed_callables.deinit(allocator);
        self.dispatch_calls.deinit(allocator);
    }

    pub fn getCallableInst(self: *const Program, callable_inst_id: CallableInstId) *const CallableInst {
        return &self.callable_insts.items[@intFromEnum(callable_inst_id)];
    }

    pub fn getCallableInstForSourceMember(
        self: *const Program,
        source_member: Lambdasolved.LambdaSetMemberId,
    ) ?CallableInstId {
        return self.callable_inst_ids_by_source_member.get(source_member);
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
        return self.callable_param_projection_entries.items[span.start..][0..span.len];
    }

    pub fn getCallableMemberSetMembers(self: *const Program, callable_member_set_id: CallableMemberSetId) []const CallableInstId {
        const member_set = self.callable_member_sets.items[@intFromEnum(callable_member_set_id)];
        if (member_set.members.len == 0) return &.{};
        return self.callable_member_entries.items[member_set.members.start..][0..member_set.members.len];
    }

    pub fn getSingletonCallableMemberSetMembers(self: *const Program, callable_inst_id: CallableInstId) []const CallableInstId {
        const callable_member_set_id = self.singleton_callable_member_set_ids_by_callable_inst.get(callable_inst_id) orelse unreachable;
        return self.getCallableMemberSetMembers(callable_member_set_id);
    }

    pub fn getCallableParamBindings(
        self: *const Program,
        span: CallableParamBindingSpan,
    ) []const CallableParamBinding {
        if (span.len == 0) return &.{};
        return self.callable_param_bindings.items[span.start..][0..span.len];
    }

    pub fn getCallableParamBindingValue(
        self: *const Program,
        callable_def_id: CallableDefId,
        pattern_idx: CIR.Pattern.Idx,
    ) ?CallableValue {
        const callable_def = getCallableDef(self, callable_def_id);
        for (self.getCallableParamBindings(callable_def.param_bindings)) |binding| {
            if (binding.pattern_idx == pattern_idx) return binding.callable_value;
        }
        return null;
    }

    pub fn getExpr(self: *const Program, expr_id: ExprId) *const Expr {
        return &self.exprs.items[@intFromEnum(expr_id)];
    }

    pub fn getExprSemantics(
        self: *const Program,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?*const ExprSemantics {
        const expr_id = self.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return &self.exprs.items[@intFromEnum(expr_id)].semantics;
    }

    pub fn getExprSemanticsPtr(
        self: *Program,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?*ExprSemantics {
        const expr_id = self.getExprId(source_context, module_idx, expr_idx) orelse return null;
        return &self.exprs.items[@intFromEnum(expr_id)].semantics;
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

    pub fn getRootExpr(self: *const Program, source_context: SourceContext, module_idx: u32, expr_idx: CIR.Expr.Idx) ?ExprId {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const root_id = self.root_expr_ids_by_key.get(key) orelse return self.getExprId(source_context, module_idx, expr_idx);
        return self.root_exprs.items[@intFromEnum(root_id)].body_expr;
    }

};

pub fn getCallableDef(program: *const Program, callable_def_id: CallableDefId) *const CallableDef {
    return &program.callable_defs.items[@intFromEnum(callable_def_id)];
}

pub fn getCaptureFields(program: *const Program, span: CaptureFieldSpan) []const CaptureField {
    if (span.len == 0) return &.{};
    return program.capture_fields.items[span.start..][0..span.len];
}

pub fn getPackedCallable(program: *const Program, packed_callable_id: PackedCallableId) *const PackedCallable {
    return &program.packed_callables.items[@intFromEnum(packed_callable_id)];
}

pub fn getDispatchCall(program: *const Program, dispatch_call_id: DispatchCallId) *const DispatchCall {
    return &program.dispatch_calls.items[@intFromEnum(dispatch_call_id)];
}

pub fn getCallableMembers(program: *const Program, span: CallableMemberSpan) []const CallableInstId {
    if (span.len == 0) return &.{};
    return program.callable_member_entries.items[span.start..][0..span.len];
}
