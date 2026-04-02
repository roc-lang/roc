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
const TemplateCatalog = @import("TemplateCatalog.zig");

const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;

pub const SourceContext = ContextMono.SourceContext;
pub const ContextExprKey = ContextMono.ContextExprKey;

pub const DispatchExprTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

pub const ExactDispatchSite = struct {
    method_name: Ident.Idx,
    fn_var: types.Var,
};

pub const ResolvedDispatchTarget = struct {
    origin: Ident.Idx,
    method_ident: Ident.Idx,
    fn_var: types.Var,
    module_idx: ?u32 = null,
};

fn dispatchTargetMethodText(
    all_module_envs: []const *ModuleEnv,
    source_env: *const ModuleEnv,
    target: ResolvedDispatchTarget,
) ?[]const u8 {
    if (TemplateCatalog.moduleOwnsIdent(source_env, target.method_ident)) {
        return TemplateCatalog.getOwnedIdentText(source_env, target.method_ident);
    }

    if (target.module_idx) |target_module_idx| {
        const target_env = all_module_envs[target_module_idx];
        if (TemplateCatalog.moduleOwnsIdent(target_env, target.method_ident)) {
            return TemplateCatalog.getOwnedIdentText(target_env, target.method_ident);
        }
    }

    return null;
}

pub fn resolveDispatchTargetToExternalDef(
    all_module_envs: []const *ModuleEnv,
    source_module_idx: u32,
    target: ResolvedDispatchTarget,
) struct { module_idx: u32, def_node_idx: u16 } {
    const source_env = all_module_envs[source_module_idx];
    const target_module_idx = target.module_idx orelse TemplateCatalog.findModuleForOrigin(
        all_module_envs,
        source_env,
        target.origin,
    );
    const target_env = all_module_envs[target_module_idx];
    const method_name = dispatchTargetMethodText(all_module_envs, source_env, target) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "DispatchSolved invariant violated: method ident {d} not readable from source module {d} or target module {d}",
                .{ target.method_ident.idx, source_module_idx, target_module_idx },
            );
        }
        unreachable;
    };

    const target_ident = target_env.common.findIdent(method_name) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "DispatchSolved invariant violated: method '{s}' not found in target module {d}",
                .{ method_name, target_module_idx },
            );
        }
        unreachable;
    };
    const target_node_idx = target_env.getExposedNodeIndexById(target_ident) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "DispatchSolved invariant violated: exposed node not found for method '{s}' in module {d}",
                .{ method_name, target_module_idx },
            );
        }
        unreachable;
    };
    if (!target_env.store.isDefNode(target_node_idx)) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "DispatchSolved invariant violated: exposed node {d} for method '{s}' in module {d} was not a def node",
                .{ target_node_idx, method_name, target_module_idx },
            );
        }
        unreachable;
    }

    return .{
        .module_idx = target_module_idx,
        .def_node_idx = target_node_idx,
    };
}

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
