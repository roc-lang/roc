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

pub const SiteIndex = struct {
    const Key = struct {
        expr_idx: u32,
        fn_name: base.Ident.Idx,
    };

    entries_by_key: std.AutoHashMapUnmanaged(Key, std.ArrayListUnmanaged(u32)),

    pub fn init(types_store: *const types.Store, allocator: Allocator) Allocator.Error!SiteIndex {
        var index: SiteIndex = .{
            .entries_by_key = .empty,
        };
        errdefer index.deinit(allocator);

        for (types_store.static_dispatch_sites.items.items, 0..) |site, raw_idx| {
            const gop = try index.entries_by_key.getOrPut(allocator, .{
                .expr_idx = site.expr_idx,
                .fn_name = site.fn_name,
            });
            if (!gop.found_existing) gop.value_ptr.* = .empty;
            try gop.value_ptr.append(allocator, @intCast(raw_idx));
        }

        return index;
    }

    pub fn deinit(self: *SiteIndex, allocator: Allocator) void {
        var it = self.entries_by_key.valueIterator();
        while (it.next()) |entries| {
            entries.deinit(allocator);
        }
        self.entries_by_key.deinit(allocator);
    }

    pub fn getSiteIndices(
        self: *const SiteIndex,
        expr_idx: u32,
        fn_name: base.Ident.Idx,
    ) []const u32 {
        const entries = self.entries_by_key.get(.{
            .expr_idx = expr_idx,
            .fn_name = fn_name,
        }) orelse return &.{};
        return entries.items;
    }
};

pub const DispatchExprTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

pub const ExactDispatchSite = struct {
    method_name: Ident.Idx,
    fn_var: types.Var,
};

pub const Result = struct {
    exact_dispatch_sites: std.AutoHashMapUnmanaged(ContextExprKey, ExactDispatchSite),
    resolved_dispatch_targets: std.AutoHashMapUnmanaged(ContextExprKey, DispatchExprTarget),

    pub fn init() Result {
        return .{
            .exact_dispatch_sites = .empty,
            .resolved_dispatch_targets = .empty,
        };
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.exact_dispatch_sites.deinit(allocator);
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

    pub fn getExactDispatchSite(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ExactDispatchSite {
        return self.exact_dispatch_sites.get(ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx));
    }

    pub fn recordDispatchExprTarget(
        self: *Result,
        allocator: Allocator,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        target: DispatchExprTarget,
    ) Allocator.Error!void {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const gop = try self.resolved_dispatch_targets.getOrPut(allocator, key);
        if (!gop.found_existing or !std.meta.eql(gop.value_ptr.*, target)) {
            gop.value_ptr.* = target;
        }
    }

    pub fn recordExactDispatchSite(
        self: *Result,
        allocator: Allocator,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
        site: ExactDispatchSite,
    ) Allocator.Error!void {
        const key = ContextMono.Result.contextExprKey(source_context, module_idx, expr_idx);
        const gop = try self.exact_dispatch_sites.getOrPut(allocator, key);
        if (!gop.found_existing or !std.meta.eql(gop.value_ptr.*, site)) {
            gop.value_ptr.* = site;
        }
    }
};
