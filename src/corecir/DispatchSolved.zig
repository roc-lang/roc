//! Exact static-dispatch sites and normalized resolved targets.
//!
//! Checker storage may describe dispatch requirements, but only this stage may
//! own source-context-scoped exact dispatch resolutions consumed by later
//! stages.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const ContextMono = @import("ContextMono.zig");

const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const Ident = base.Ident;

pub const SourceContext = ContextMono.SourceContext;
pub const ContextExprKey = ContextMono.ContextExprKey;

pub const ConstraintIndex = struct {
    const Key = struct {
        source_expr_idx: u32,
        fn_name: base.Ident.Idx,
    };

    entries_by_key: std.AutoHashMapUnmanaged(Key, std.ArrayListUnmanaged(u32)),

    pub fn init(types_store: *const types.Store, allocator: Allocator) Allocator.Error!ConstraintIndex {
        var index: ConstraintIndex = .{
            .entries_by_key = .empty,
        };
        errdefer index.deinit(allocator);

        for (types_store.static_dispatch_constraints.items.items, 0..) |constraint, raw_idx| {
            if (constraint.source_expr_idx == types.StaticDispatchConstraint.no_source_expr) continue;
            const gop = try index.entries_by_key.getOrPut(allocator, .{
                .source_expr_idx = constraint.source_expr_idx,
                .fn_name = constraint.fn_name,
            });
            if (!gop.found_existing) gop.value_ptr.* = .empty;
            try gop.value_ptr.append(allocator, @intCast(raw_idx));
        }

        return index;
    }

    pub fn deinit(self: *ConstraintIndex, allocator: Allocator) void {
        var it = self.entries_by_key.valueIterator();
        while (it.next()) |entries| {
            entries.deinit(allocator);
        }
        self.entries_by_key.deinit(allocator);
    }

    pub fn getConstraintIndices(
        self: *const ConstraintIndex,
        source_expr_idx: u32,
        fn_name: base.Ident.Idx,
    ) []const u32 {
        const entries = self.entries_by_key.get(.{
            .source_expr_idx = source_expr_idx,
            .fn_name = fn_name,
        }) orelse return &.{};
        return entries.items;
    }
};

pub const DispatchExprTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

pub const ExactDispatchConstraint = struct {
    method_name: Ident.Idx,
    fn_var: types.Var,
    resolved_target: types.StaticDispatchConstraint.ResolvedTarget,
};

pub const Result = struct {
    exact_dispatch_constraints: std.AutoHashMapUnmanaged(ContextExprKey, ExactDispatchConstraint),
    resolved_dispatch_targets: std.AutoHashMapUnmanaged(ContextExprKey, DispatchExprTarget),

    pub fn init() Result {
        return .{
            .exact_dispatch_constraints = .empty,
            .resolved_dispatch_targets = .empty,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.exact_dispatch_constraints.deinit(allocator);
        self.resolved_dispatch_targets.deinit(allocator);
    }

    pub fn getDispatchExprTarget(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?DispatchExprTarget {
        return self.resolved_dispatch_targets.get(ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx));
    }

    pub fn getExactDispatchConstraint(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ExactDispatchConstraint {
        return self.exact_dispatch_constraints.get(ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx));
    }
};
