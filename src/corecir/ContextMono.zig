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

    pub const none: ContextId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: ContextId) bool {
        return self == none;
    }
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

    pub const none: TypeSubstId = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: TypeSubstId) bool {
        return self == none;
    }
};

pub const TypeSubst = struct {
    entries: TypeSubstSpan,
};

pub const ContextExprKey = struct {
    context_id_raw: u32,
    root_source_expr_raw: u32,
    module_idx: u32,
    expr_raw: u32,
};

pub const ContextPatternKey = struct {
    context_id_raw: u32,
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
    context_expr_monotypes: std.AutoHashMapUnmanaged(ContextExprKey, ResolvedMonotype),
    context_pattern_monotypes: std.AutoHashMapUnmanaged(ContextPatternKey, ResolvedMonotype),
    resolved_dispatch_targets: std.AutoHashMapUnmanaged(ContextExprKey, DispatchExprTarget),
    root_module_idx: u32,
    root_source_expr_idx: ?CIR.Expr.Idx,

    pub fn init(allocator: Allocator, root_module_idx: u32, root_source_expr_idx: ?CIR.Expr.Idx) !Result {
        return .{
            .monotype_store = try Monotype.Store.init(allocator),
            .subst_entries = .empty,
            .substs = .empty,
            .context_expr_monotypes = .empty,
            .context_pattern_monotypes = .empty,
            .resolved_dispatch_targets = .empty,
            .root_module_idx = root_module_idx,
            .root_source_expr_idx = root_source_expr_idx,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.monotype_store.deinit(allocator);
        self.subst_entries.deinit(allocator);
        self.substs.deinit(allocator);
        self.context_expr_monotypes.deinit(allocator);
        self.context_pattern_monotypes.deinit(allocator);
        self.resolved_dispatch_targets.deinit(allocator);
    }

    pub fn contextExprKey(
        context_id: ContextId,
        root_source_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ContextExprKey {
        return .{
            .context_id_raw = @intFromEnum(context_id),
            .root_source_expr_raw = if (context_id.isNone() and root_source_expr_context != null)
                @intFromEnum(root_source_expr_context.?)
            else
                std.math.maxInt(u32),
            .module_idx = module_idx,
            .expr_raw = @intFromEnum(expr_idx),
        };
    }

    pub fn contextPatternKey(
        context_id: ContextId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ContextPatternKey {
        return .{
            .context_id_raw = @intFromEnum(context_id),
            .module_idx = module_idx,
            .pattern_raw = @intFromEnum(pattern_idx),
        };
    }

    pub fn getExprMonotype(
        self: *const Result,
        context_id: ContextId,
        root_source_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        return self.context_expr_monotypes.get(contextExprKey(
            context_id,
            root_source_expr_context,
            module_idx,
            expr_idx,
        ));
    }

    pub fn getContextPatternMonotype(
        self: *const Result,
        context_id: ContextId,
        module_idx: u32,
        pattern_idx: CIR.Pattern.Idx,
    ) ?ResolvedMonotype {
        return self.context_pattern_monotypes.get(contextPatternKey(
            context_id,
            module_idx,
            pattern_idx,
        ));
    }

    pub fn getDispatchExprTarget(
        self: *const Result,
        context_id: ContextId,
        root_source_expr_context: ?CIR.Expr.Idx,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        return self.resolved_dispatch_targets.get(contextExprKey(
            context_id,
            root_source_expr_context,
            module_idx,
            expr_idx,
        ));
    }

    pub fn getTypeSubst(self: *const Result, subst_id: TypeSubstId) *const TypeSubst {
        return &self.substs.items[@intFromEnum(subst_id)];
    }

    pub fn getTypeSubstEntries(self: *const Result, span: TypeSubstSpan) []const TypeSubstEntry {
        if (span.len == 0) return &.{};
        return self.subst_entries.items[span.start..][0..span.len];
    }
};

pub fn resolvedMonotype(idx: Monotype.Idx, module_idx: u32) ResolvedMonotype {
    return .{ .idx = idx, .module_idx = module_idx };
}
