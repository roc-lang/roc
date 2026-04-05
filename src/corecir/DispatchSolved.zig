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
const Monotype = @import("Monotype.zig");
const TemplateCatalog = @import("TemplateCatalog.zig");

const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;

pub const SourceContext = ContextMono.SourceContext;
pub const ContextExprKey = ContextMono.ContextExprKey;
pub const CallableTemplateId = TemplateCatalog.CallableTemplateId;

pub const DispatchExprTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
};

pub const ExactDispatchSite = struct {
    method_name: Ident.Idx,
    fn_var: types.Var,
    fn_monotype: ContextMono.ResolvedMonotype,
};

pub const ResolvedDispatchTarget = struct {
    origin: Ident.Idx,
    method_ident: Ident.Idx,
    fn_var: types.Var,
    module_idx: ?u32 = null,
};

pub const AssociatedMethodTemplate = struct {
    target_env: *const ModuleEnv,
    module_idx: u32,
    template_id: CallableTemplateId,
    type_var: types.Var,
    qualified_method_ident: Ident.Idx,
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

fn dispatchMethodIdentForBinop(module_env: *const ModuleEnv, op: CIR.Expr.Binop.Op) ?Ident.Idx {
    return switch (op) {
        .@"and", .@"or" => null,
        .add => module_env.idents.plus,
        .sub => module_env.idents.minus,
        .mul => module_env.idents.times,
        .div => module_env.idents.div_by,
        .div_trunc => module_env.idents.div_trunc_by,
        .rem => module_env.idents.rem_by,
        .eq, .ne => module_env.idents.is_eq,
        .lt => module_env.idents.is_lt,
        .gt => module_env.idents.is_gt,
        .le => module_env.idents.is_lte,
        .ge => module_env.idents.is_gte,
    };
}

pub fn dispatchConstraintReceiverTypeVar(
    driver: anytype,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
) ?types.Var {
    const module_env = driver.all_module_envs[module_idx];
    return switch (module_env.store.getExpr(expr_idx)) {
        .e_binop => |binop_expr| blk: {
            const binop_method = dispatchMethodIdentForBinop(module_env, binop_expr.op) orelse break :blk null;
            if (!binop_method.eql(method_name)) break :blk null;
            break :blk ModuleEnv.varFrom(binop_expr.lhs);
        },
        .e_unary_minus => |unary_expr| if (method_name.eql(module_env.idents.negate))
            ModuleEnv.varFrom(unary_expr.expr)
        else
            null,
        .e_dot_access => |dot_expr| if (dot_expr.args != null and dot_expr.field_name.eql(method_name))
            ModuleEnv.varFrom(dot_expr.receiver)
        else
            null,
        .e_type_var_dispatch => |dispatch_expr| blk: {
            if (!dispatch_expr.method_name.eql(method_name)) break :blk null;
            const alias_stmt = module_env.store.getStatement(dispatch_expr.type_var_alias_stmt).s_type_var_alias;
            break :blk ModuleEnv.varFrom(alias_stmt.type_var_anno);
        },
        else => null,
    };
}

pub fn dispatchConstraintsForTypeVar(
    driver: anytype,
    module_idx: u32,
    type_var: types.Var,
) []const types.StaticDispatchConstraint {
    const module_env = driver.all_module_envs[module_idx];
    const resolved = module_env.types.resolveVar(type_var);
    return switch (resolved.desc.content) {
        .flex => |flex| module_env.types.sliceStaticDispatchConstraints(flex.constraints),
        .rigid => |rigid| module_env.types.sliceStaticDispatchConstraints(rigid.constraints),
        else => &.{},
    };
}

pub fn getRecordedExactDispatchSiteForExpr(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
) ?ExactDispatchSite {
    if (result.dispatch_solved.getExactDispatchSite(source_context, module_idx, expr_idx)) |site| {
        if (!site.method_name.eql(method_name)) {
            const module_env = driver.all_module_envs[module_idx];
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "DispatchSolved invariant violated: dispatch expr={d} cached method '{s}' but requested '{s}'",
                    .{
                        @intFromEnum(expr_idx),
                        module_env.getIdent(site.method_name),
                        module_env.getIdent(method_name),
                    },
                );
            }
            unreachable;
        }
        return site;
    }
    return null;
}

fn resolveExactFnMonotypeFromFnVarPieces(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    fn_var: types.Var,
) Allocator.Error!?ContextMono.ResolvedMonotype {
    const direct = try ContextMono.resolveTypeVarExactMonotypeResolved(
        driver,
        result,
        thread,
        module_idx,
        fn_var,
    );
    if (!direct.isNone()) {
        return if (ContextMono.resolvedIfFunctionMonotype(result, direct)) |fn_monotype|
            fn_monotype
        else
            null;
    }

    const module_env = driver.all_module_envs[module_idx];
    const resolved_func = ContextMono.resolveFuncTypeInStore(&module_env.types, fn_var) orelse return null;
    var arg_monos = std.ArrayListUnmanaged(Monotype.Idx).empty;
    defer arg_monos.deinit(driver.allocator);

    for (module_env.types.sliceVars(resolved_func.func.args)) |arg_var| {
        const arg_mono = try ContextMono.resolveTypeVarExactMonotypeResolved(
            driver,
            result,
            thread,
            module_idx,
            arg_var,
        );
        if (arg_mono.isNone()) {
            if (std.debug.runtime_safety) {
                std.debug.print(
                    "DEBUG resolveExactFnMonotypeFromFnVarPieces missing arg exact: module={d} fn_var={d} arg_var={d} ctx={s}\n",
                    .{ module_idx, @intFromEnum(fn_var), @intFromEnum(arg_var), @tagName(thread.requireSourceContext()) },
                );
            }
            return null;
        }
        const canonical_arg = if (arg_mono.module_idx == module_idx)
            arg_mono.idx
        else
            try ContextMono.remapMonotypeBetweenModules(
                driver,
                result,
                arg_mono.idx,
                arg_mono.module_idx,
                module_idx,
            );
        try arg_monos.append(driver.allocator, canonical_arg);
    }

    const ret_mono = try ContextMono.resolveTypeVarExactMonotypeResolved(
        driver,
        result,
        thread,
        module_idx,
        resolved_func.func.ret,
    );
    if (ret_mono.isNone()) {
        if (std.debug.runtime_safety) {
            std.debug.print(
                "DEBUG resolveExactFnMonotypeFromFnVarPieces missing ret exact: module={d} fn_var={d} ret_var={d} ctx={s}\n",
                .{ module_idx, @intFromEnum(fn_var), @intFromEnum(resolved_func.func.ret), @tagName(thread.requireSourceContext()) },
            );
        }
        return null;
    }
    const canonical_ret = if (ret_mono.module_idx == module_idx)
        ret_mono.idx
    else
        try ContextMono.remapMonotypeBetweenModules(
            driver,
            result,
            ret_mono.idx,
            ret_mono.module_idx,
            module_idx,
        );

    const args = try result.context_mono.monotype_store.addIdxSpan(driver.allocator, arg_monos.items);
    const fn_monotype = try result.context_mono.monotype_store.addMonotype(driver.allocator, .{ .func = .{
        .args = args,
        .ret = canonical_ret,
        .effectful = resolved_func.effectful,
    } });
    return ContextMono.resolvedMonotype(fn_monotype, module_idx);
}

pub fn extractExactDispatchSiteForExpr(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
) Allocator.Error!?ExactDispatchSite {
    const module_env = driver.all_module_envs[module_idx];
    if (module_env.types.findStaticDispatchSiteRequirement(ModuleEnv.varFrom(expr_idx), method_name)) |requirement| {
        const fn_monotype = (try resolveExactFnMonotypeFromFnVarPieces(
            driver,
            result,
            thread,
            module_idx,
            requirement.fn_var,
        )) orelse return null;
        return .{
            .method_name = requirement.method_name,
            .fn_var = requirement.fn_var,
            .fn_monotype = fn_monotype,
        };
    }

    const receiver_type_var = dispatchConstraintReceiverTypeVar(
        driver,
        module_idx,
        expr_idx,
        method_name,
    ) orelse {
        if (std.debug.runtime_safety) {
            std.debug.print(
                "DEBUG extractExactDispatchSiteForExpr no receiver typevar: module={d} expr={d} method={s} ctx={s}\n",
                .{ module_idx, @intFromEnum(expr_idx), module_env.getIdent(method_name), @tagName(thread.requireSourceContext()) },
            );
        }
        return null;
    };
    const receiver_constraints = dispatchConstraintsForTypeVar(
        driver,
        module_idx,
        receiver_type_var,
    );

    var matched_constraint: ?types.StaticDispatchConstraint = null;
    for (receiver_constraints) |constraint| {
        if (!constraint.fn_name.eql(method_name)) continue;
        if (matched_constraint != null) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "DispatchSolved invariant violated: multiple receiver dispatch constraints matched expr={d} method='{s}' in module {d}",
                    .{
                        @intFromEnum(expr_idx),
                        module_env.getIdent(method_name),
                        module_idx,
                    },
                );
            }
            unreachable;
        }
        matched_constraint = constraint;
    }
    const constraint = matched_constraint orelse {
        if (std.debug.runtime_safety) {
            std.debug.print(
                "DEBUG extractExactDispatchSiteForExpr no matching receiver constraint: module={d} expr={d} method={s} receiver_var={d} ctx={s} constraint_count={d}\n",
                .{ module_idx, @intFromEnum(expr_idx), module_env.getIdent(method_name), @intFromEnum(receiver_type_var), @tagName(thread.requireSourceContext()), receiver_constraints.len },
            );
        }
        return null;
    };
    const fn_monotype = (try resolveExactFnMonotypeFromFnVarPieces(
        driver,
        result,
        thread,
        module_idx,
        constraint.fn_var,
    )) orelse return null;
    return .{
        .method_name = method_name,
        .fn_var = constraint.fn_var,
        .fn_monotype = fn_monotype,
    };
}

pub fn ensureRecordedExactDispatchSiteForExpr(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
) Allocator.Error!void {
    const source_context = thread.requireSourceContext();
    if (getRecordedExactDispatchSiteForExpr(driver, result, source_context, module_idx, expr_idx, method_name) != null) {
        return;
    }
    const exact = try extractExactDispatchSiteForExpr(
        driver,
        result,
        thread,
        module_idx,
        expr_idx,
        method_name,
    ) orelse return;
    try result.dispatch_solved.recordExactDispatchSite(
        driver.allocator,
        source_context,
        module_idx,
        expr_idx,
        exact,
    );
}

pub fn exactDispatchSiteForExpr(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    method_name: Ident.Idx,
) ?ExactDispatchSite {
    return getRecordedExactDispatchSiteForExpr(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        method_name,
    );
}

pub fn lookupResolvedDispatchTarget(
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?DispatchExprTarget {
    return result.dispatch_solved.getDispatchExprTarget(
        source_context,
        module_idx,
        expr_idx,
    );
}

pub fn lookupResolvedDispatchTemplate(
    driver: anytype,
    result: anytype,
    source_module_idx: u32,
    resolved_target: ResolvedDispatchTarget,
) Allocator.Error!CallableTemplateId {
    const target_def = resolveDispatchTargetToExternalDef(
        driver.all_module_envs,
        source_module_idx,
        resolved_target,
    );
    return result.template_catalog.requireExternalCallableTemplate(
        target_def.module_idx,
        target_def.def_node_idx,
        "resolved dispatch target",
    );
}

pub fn builtinPrimForNominal(ident: Ident.Idx, common: ModuleEnv.CommonIdents) ?Monotype.Prim {
    if (ident.eql(common.str)) return .str;
    if (ident.eql(common.u8_type)) return .u8;
    if (ident.eql(common.i8_type)) return .i8;
    if (ident.eql(common.u16_type)) return .u16;
    if (ident.eql(common.i16_type)) return .i16;
    if (ident.eql(common.u32_type)) return .u32;
    if (ident.eql(common.i32_type)) return .i32;
    if (ident.eql(common.u64_type)) return .u64;
    if (ident.eql(common.i64_type)) return .i64;
    if (ident.eql(common.u128_type)) return .u128;
    if (ident.eql(common.i128_type)) return .i128;
    if (ident.eql(common.f32_type)) return .f32;
    if (ident.eql(common.f64_type)) return .f64;
    if (ident.eql(common.dec_type)) return .dec;
    return null;
}

pub fn lookupAssociatedMethodTemplate(
    driver: anytype,
    result: anytype,
    source_module_idx: u32,
    nominal: types.NominalType,
    method_ident: Ident.Idx,
) Allocator.Error!?AssociatedMethodTemplate {
    return lookupAssociatedMethodTemplateByOriginIdent(
        driver,
        result,
        source_module_idx,
        nominal.origin_module,
        nominal.ident.ident_idx,
        method_ident,
    );
}

pub fn lookupAssociatedMethodTemplateForMonotype(
    driver: anytype,
    result: anytype,
    source_module_idx: u32,
    monotype: Monotype.Idx,
    method_ident: Ident.Idx,
) Allocator.Error!?AssociatedMethodTemplate {
    const source_env = driver.all_module_envs[source_module_idx];
    const common = ModuleEnv.CommonIdents.find(&source_env.common);
    const mono = result.context_mono.monotype_store.getMonotype(monotype);

    const type_ident: Ident.Idx = switch (mono) {
        .prim => |prim| switch (prim) {
            .str => common.str,
            .u8 => common.u8_type,
            .i8 => common.i8_type,
            .u16 => common.u16_type,
            .i16 => common.i16_type,
            .u32 => common.u32_type,
            .i32 => common.i32_type,
            .u64 => common.u64_type,
            .i64 => common.i64_type,
            .u128 => common.u128_type,
            .i128 => common.i128_type,
            .f32 => common.f32_type,
            .f64 => common.f64_type,
            .dec => common.dec_type,
        },
        .list => common.list,
        .box => common.box,
        else => return null,
    };

    return lookupAssociatedMethodTemplateByOriginIdent(
        driver,
        result,
        source_module_idx,
        common.builtin_module,
        type_ident,
        method_ident,
    );
}

pub fn lookupAssociatedMethodTemplateByOriginIdent(
    driver: anytype,
    result: anytype,
    source_module_idx: u32,
    origin_module: Ident.Idx,
    type_ident: Ident.Idx,
    method_ident: Ident.Idx,
) Allocator.Error!?AssociatedMethodTemplate {
    const source_env = driver.all_module_envs[source_module_idx];
    const target_module_idx = TemplateCatalog.findModuleForOrigin(
        driver.all_module_envs,
        source_env,
        origin_module,
    );
    const target_env = driver.all_module_envs[target_module_idx];
    const qualified_method_ident = target_env.lookupMethodIdentFromEnvConst(
        source_env,
        type_ident,
        method_ident,
    ) orelse return null;
    const node_idx = target_env.getExposedNodeIndexById(qualified_method_ident) orelse return null;
    if (!target_env.store.isDefNode(node_idx)) return null;

    const def_idx: CIR.Def.Idx = @enumFromInt(node_idx);
    const def = target_env.store.getDef(def_idx);
    const template_id = result.template_catalog.requireExternalCallableTemplate(
        target_module_idx,
        node_idx,
        "associated method lookup",
    );
    return .{
        .target_env = target_env,
        .module_idx = target_module_idx,
        .template_id = template_id,
        .type_var = ModuleEnv.varFrom(def.expr),
        .qualified_method_ident = qualified_method_ident,
    };
}
