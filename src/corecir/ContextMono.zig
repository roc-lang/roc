//! Exact contextual monotypes for normalized source IR.
//!
//! This stage owns all source-level monotype determination and all language
//! defaulting. No later phase may default unresolved source-level types.

const std = @import("std");
const can = @import("can");
const types = @import("types");
const Monotype = @import("Monotype.zig");

const Allocator = std.mem.Allocator;
const CIR = can.CIR;

pub const ContextId = enum(u32) {
    _,
};

pub const ExprContext = struct {
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
};

pub const RootExprContext = ExprContext;
pub const ProvenanceExprContext = ExprContext;
pub const TemplateExprContext = ExprContext;

pub const SourceContext = union(enum) {
    /// Lowering/scanning inside a specialized callable body.
    callable_inst: ContextId,
    /// A real compilation root expression such as a top-level const body.
    root_expr: RootExprContext,
    /// Re-entering a specific source expression as value provenance, not as a root.
    provenance_expr: ProvenanceExprContext,
    /// Completing/template-analyzing a callable template outside any instantiation.
    template_expr: TemplateExprContext,
};

pub const BoundTypeVarKey = struct {
    module_idx: u32,
    type_var: types.Var,
};

/// A monotype plus the module whose ident namespace the monotype currently uses.
pub const ResolvedMonotype = struct {
    idx: Monotype.Idx,
    module_idx: u32,

    pub fn isNone(self: ResolvedMonotype) bool {
        return self.idx.isNone();
    }
};

pub const TypeSubstEntry = struct {
    key: BoundTypeVarKey,
    monotype: ResolvedMonotype,
};

pub const TypeSubstSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() TypeSubstSpan {
        return .{ .start = 0, .len = 0 };
    }
};

pub const TypeSubstId = enum(u32) {
    _,
};

pub const TypeSubst = struct {
    entries: TypeSubstSpan,
};

pub const ContextExprKey = struct {
    source_context_kind: enum(u2) { callable_inst, root_expr, provenance_expr, template_expr },
    source_context_module_idx: u32,
    source_context_raw: u32,
    module_idx: u32,
    expr_raw: u32,
};

pub const ContextPatternKey = struct {
    source_context_kind: enum(u2) { callable_inst, root_expr, provenance_expr, template_expr },
    source_context_module_idx: u32,
    source_context_raw: u32,
    module_idx: u32,
    pattern_raw: u32,
};

pub const DispatchExprTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

pub const Result = struct {
    monotype_store: Monotype.Store,
    subst_entries: std.ArrayListUnmanaged(TypeSubstEntry),
    substs: std.ArrayListUnmanaged(TypeSubst),
    empty_subst_id: TypeSubstId,
    context_expr_monotypes: std.AutoHashMapUnmanaged(ContextExprKey, ResolvedMonotype),
    context_pattern_monotypes: std.AutoHashMapUnmanaged(ContextPatternKey, ResolvedMonotype),
    type_scope_monotypes: std.AutoHashMapUnmanaged(BoundTypeVarKey, ResolvedMonotype),
    resolved_dispatch_targets: std.AutoHashMapUnmanaged(ContextExprKey, DispatchExprTarget),

    pub fn init(allocator: Allocator) !Result {
        var result: Result = .{
            .monotype_store = try Monotype.Store.init(allocator),
            .subst_entries = .empty,
            .substs = .empty,
            .empty_subst_id = @enumFromInt(0),
            .context_expr_monotypes = .empty,
            .context_pattern_monotypes = .empty,
            .type_scope_monotypes = .empty,
            .resolved_dispatch_targets = .empty,
        };
        try result.substs.append(allocator, .{ .entries = TypeSubstSpan.empty() });
        return result;
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.monotype_store.deinit(allocator);
        self.subst_entries.deinit(allocator);
        self.substs.deinit(allocator);
        self.context_expr_monotypes.deinit(allocator);
        self.context_pattern_monotypes.deinit(allocator);
        self.type_scope_monotypes.deinit(allocator);
        self.resolved_dispatch_targets.deinit(allocator);
    }

    pub fn contextExprKey(source_context: SourceContext, module_idx: u32, expr_idx: CIR.Expr.Idx) ContextExprKey {
        return switch (source_context) {
            .callable_inst => |context_id| .{
                .source_context_kind = .callable_inst,
                .source_context_module_idx = std.math.maxInt(u32),
                .source_context_raw = @intFromEnum(context_id),
                .module_idx = module_idx,
                .expr_raw = @intFromEnum(expr_idx),
            },
            .root_expr => |root| .{
                .source_context_kind = .root_expr,
                .source_context_module_idx = root.module_idx,
                .source_context_raw = @intFromEnum(root.expr_idx),
                .module_idx = module_idx,
                .expr_raw = @intFromEnum(expr_idx),
            },
            .provenance_expr => |source| .{
                .source_context_kind = .provenance_expr,
                .source_context_module_idx = source.module_idx,
                .source_context_raw = @intFromEnum(source.expr_idx),
                .module_idx = module_idx,
                .expr_raw = @intFromEnum(expr_idx),
            },
            .template_expr => |template| .{
                .source_context_kind = .template_expr,
                .source_context_module_idx = template.module_idx,
                .source_context_raw = @intFromEnum(template.expr_idx),
                .module_idx = module_idx,
                .expr_raw = @intFromEnum(expr_idx),
            },
        };
    }

    pub fn contextPatternKey(source_context: SourceContext, module_idx: u32, pattern_idx: CIR.Pattern.Idx) ContextPatternKey {
        return switch (source_context) {
            .callable_inst => |context_id| .{
                .source_context_kind = .callable_inst,
                .source_context_module_idx = std.math.maxInt(u32),
                .source_context_raw = @intFromEnum(context_id),
                .module_idx = module_idx,
                .pattern_raw = @intFromEnum(pattern_idx),
            },
            .root_expr => |root| .{
                .source_context_kind = .root_expr,
                .source_context_module_idx = root.module_idx,
                .source_context_raw = @intFromEnum(root.expr_idx),
                .module_idx = module_idx,
                .pattern_raw = @intFromEnum(pattern_idx),
            },
            .provenance_expr => |source| .{
                .source_context_kind = .provenance_expr,
                .source_context_module_idx = source.module_idx,
                .source_context_raw = @intFromEnum(source.expr_idx),
                .module_idx = module_idx,
                .pattern_raw = @intFromEnum(pattern_idx),
            },
            .template_expr => |template| .{
                .source_context_kind = .template_expr,
                .source_context_module_idx = template.module_idx,
                .source_context_raw = @intFromEnum(template.expr_idx),
                .module_idx = module_idx,
                .pattern_raw = @intFromEnum(pattern_idx),
            },
        };
    }

    pub fn getExprMonotype(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        return self.context_expr_monotypes.get(contextExprKey(source_context, module_idx, expr_idx));
    }

    pub fn getContextPatternMonotype(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ResolvedMonotype {
        return self.context_pattern_monotypes.get(contextPatternKey(source_context, module_idx, pattern_idx));
    }

    pub fn getTypeScopeMonotype(
        self: *const Result,
        module_idx: u32,
        type_var: types.Var,
    ) ?ResolvedMonotype {
        return self.type_scope_monotypes.get(.{
            .module_idx = module_idx,
            .type_var = type_var,
        });
    }

    pub fn getDispatchExprTarget(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        return self.resolved_dispatch_targets.get(contextExprKey(source_context, module_idx, expr_idx));
    }

    pub fn getTypeSubst(self: *const Result, subst_id: TypeSubstId) *const TypeSubst {
        return &self.substs.items[@intFromEnum(subst_id)];
    }

    pub fn getEmptyTypeSubstId(self: *const Result) TypeSubstId {
        return self.empty_subst_id;
    }

    pub fn getTypeSubstEntries(self: *const Result, span: TypeSubstSpan) []const TypeSubstEntry {
        if (span.len == 0) return &.{};
        return self.subst_entries.items[span.start..][0..span.len];
    }
};

pub fn resolvedMonotype(idx: Monotype.Idx, module_idx: u32) ResolvedMonotype {
    return .{ .idx = idx, .module_idx = module_idx };
}
