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
pub const CallableInstId = Lambdasolved.CallableInstId;

pub const CallableInst = struct {
    template: Lambdasolved.CallableTemplateId,
    subst: ContextMono.TypeSubstId,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
    defining_source_context: SourceContext,
    callable_def: CallableDefId,
    runtime_value: RuntimeValue = .direct_lambda,
};

pub const RuntimeValue = union(enum) {
    direct_lambda,
    closure: struct {
        capture_tuple_monotype: ContextMono.ResolvedMonotype,
    },
};

pub const PackedFnId = enum(u32) {
    _,
};

pub const PackedFn = struct {
    members: CallableMemberSpan,
    fn_monotype: ContextMono.ResolvedMonotype,
};

pub const IndirectCallId = enum(u32) {
    _,
};

pub const IndirectCall = struct {
    members: CallableMemberSpan,
};

pub const ExprId = enum(u32) {
    _,
};

pub const ExprRef = struct {
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
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
    packed_fn: PackedFnId,
};

pub const CallSite = union(enum) {
    direct: CallableInstId,
    indirect_call: IndirectCallId,
};

pub const LookupResolution = union(enum) {
    expr: ExprRef,
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

const CallableMemberGroup = struct {
    members: CallableMemberSpan,
};

pub const CallableMemberGroupId = enum(u32) {
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
    expr: ExprRef,
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
    module_idx: u32,
    runtime_expr: ExprRef,
    body_expr: ExprRef,
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

pub const Expr = struct {
    source_context: SourceContext,
    module_idx: u32,
    source_expr: CIR.Expr.Idx,
    monotype: ContextMono.ResolvedMonotype,
    child_exprs: ExprIdSpan = .empty(),
    child_stmts: StmtIdSpan = .empty(),
    callable_value: ?CallableValue = null,
    call_site: ?CallSite = null,
    dispatch_target: ?ContextMono.DispatchExprTarget = null,
    lookup_resolution: ?LookupResolution = null,
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
    callable_member_groups: std.ArrayListUnmanaged(CallableMemberGroup),
    direct_callable_member_group_ids_by_callable_inst: std.AutoHashMapUnmanaged(CallableInstId, CallableMemberGroupId),
    packed_fn_ids_by_member_group: std.AutoHashMapUnmanaged(CallableMemberGroupId, PackedFnId),
    indirect_call_ids_by_member_group: std.AutoHashMapUnmanaged(CallableMemberGroupId, IndirectCallId),
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
    packed_fns: std.ArrayListUnmanaged(PackedFn),
    indirect_calls: std.ArrayListUnmanaged(IndirectCall),

    pub fn init() Program {
        return .{
            .callable_insts = .empty,
            .callable_member_groups = .empty,
            .direct_callable_member_group_ids_by_callable_inst = .empty,
            .packed_fn_ids_by_member_group = .empty,
            .indirect_call_ids_by_member_group = .empty,
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
            .packed_fns = .empty,
            .indirect_calls = .empty,
        };
    }

    pub fn deinit(self: *Program, allocator: Allocator) void {
        self.callable_insts.deinit(allocator);
        self.callable_member_groups.deinit(allocator);
        self.direct_callable_member_group_ids_by_callable_inst.deinit(allocator);
        self.packed_fn_ids_by_member_group.deinit(allocator);
        self.indirect_call_ids_by_member_group.deinit(allocator);
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
        self.packed_fns.deinit(allocator);
        self.indirect_calls.deinit(allocator);
    }

    pub fn getCallableInst(self: *const Program, callable_inst_id: CallableInstId) *const CallableInst {
        return &self.callable_insts.items[@intFromEnum(callable_inst_id)];
    }

    pub fn getCallableMemberGroupMembers(self: *const Program, member_group_id: CallableMemberGroupId) []const CallableInstId {
        const group = self.callable_member_groups.items[@intFromEnum(member_group_id)];
        if (group.members.len == 0) return &.{};
        return self.callable_member_entries.items[group.members.start..][0..group.members.len];
    }

    pub fn getDirectCallableMembers(self: *const Program, callable_inst_id: CallableInstId) []const CallableInstId {
        const member_group_id = self.direct_callable_member_group_ids_by_callable_inst.get(callable_inst_id) orelse unreachable;
        return self.getCallableMemberGroupMembers(member_group_id);
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

pub fn getPackedFn(program: *const Program, packed_fn_id: PackedFnId) *const PackedFn {
    return &program.packed_fns.items[@intFromEnum(packed_fn_id)];
}

pub fn getIndirectCall(program: *const Program, indirect_call_id: IndirectCallId) *const IndirectCall {
    return &program.indirect_calls.items[@intFromEnum(indirect_call_id)];
}

pub fn getCallableMembers(program: *const Program, span: CallableMemberSpan) []const CallableInstId {
    if (span.len == 0) return &.{};
    return program.callable_member_entries.items[span.start..][0..span.len];
}
