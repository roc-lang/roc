//! Checked static-dispatch target registry and normalized dispatch-site records.
//!
//! The registry is built at checked-module publication. Post-check lowering uses
//! it as a target table only; the dispatch-site record chooses the dispatcher
//! type variable explicitly.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const TypedCIR = @import("typed_cir.zig");
const canonical = @import("canonical_names.zig");
const checked_ids = @import("checked_ids.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;
const CheckedTypeId = checked_ids.CheckedTypeId;
const CheckedExprId = checked_ids.CheckedExprId;

/// Public `ProcedureTemplateLookup` declaration.
pub const ProcedureTemplateLookup = struct {
    module_idx: u32,
    by_def: []const ?canonical.ProcedureTemplateRef,

    pub fn templateForDef(self: *const ProcedureTemplateLookup, def_idx: CIR.Def.Idx) ?canonical.ProcedureTemplateRef {
        const raw = @intFromEnum(def_idx);
        if (raw >= self.by_def.len) return null;
        return self.by_def[raw];
    }
};

/// Public `MethodOwner` declaration.
pub const MethodOwner = union(enum) {
    nominal: canonical.NominalTypeKey,
    builtin: BuiltinOwner,
};

/// Public `BuiltinOwner` declaration.
pub const BuiltinOwner = enum {
    list,
    box,
    bool,
    str,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
};

/// Public `MethodKey` declaration.
pub const MethodKey = struct {
    owner: MethodOwner,
    method: canonical.MethodNameId,
};

/// Public `MethodTarget` declaration.
pub const MethodTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    proc: canonical.ProcedureValueRef,
    template: ?canonical.ProcedureTemplateRef,
    callable_ty: CheckedTypeId,
};

/// Public `MethodRegistryEntry` declaration.
pub const MethodRegistryEntry = struct {
    key: MethodKey,
    target: MethodTarget,
};

/// Public `MethodRegistry` declaration.
pub const MethodRegistry = struct {
    entries: []MethodRegistryEntry = &.{},

    pub fn lookup(self: *const MethodRegistry, key: MethodKey) ?MethodTarget {
        for (self.entries) |entry| {
            if (methodKeyEql(entry.key, key)) return entry.target;
        }
        return null;
    }

    pub fn deinit(self: *MethodRegistry, allocator: Allocator) void {
        allocator.free(self.entries);
        self.* = .{};
    }

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        local_templates: *const ProcedureTemplateLookup,
        checked_types: anytype,
    ) Allocator.Error!MethodRegistry {
        var entries = std.ArrayList(MethodRegistryEntry).empty;
        errdefer entries.deinit(allocator);

        const module_idx = module.moduleIndex();
        if (module_idx != local_templates.module_idx) {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic(
                    "checked static dispatch registry invariant violated: template lookup module {d} does not match module {d}",
                    .{ local_templates.module_idx, module_idx },
                );
            }
            unreachable;
        }

        const module_env = module.moduleEnvConst();
        const idents = module.identStoreConst();
        const module_name = try names.internModuleIdent(idents, module.qualifiedModuleIdent());

        for (module.methodIdentEntries()) |entry| {
            const def_node_idx = module_env.getExposedNodeIndexById(entry.value) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch registry invariant violated: method ident {d} has no exposed definition",
                        .{@as(u32, @bitCast(entry.value))},
                    );
                }
                unreachable;
            };
            const def_idx: CIR.Def.Idx = @enumFromInt(@as(u32, @intCast(def_node_idx)));
            const template = local_templates.templateForDef(def_idx) orelse {
                // Associated values without arguments are checked field access,
                // not static-dispatch call targets. The method registry is a
                // procedure-target table for Monotype static dispatch lowering,
                // so only procedure-backed entries belong here.
                continue;
            };
            const export_name = try names.internExportIdent(idents, entry.value);
            const proc_base = try names.internProcBase(.{
                .module_name = module_name,
                .export_name = export_name,
                .kind = .checked_source,
                .ordinal = @intFromEnum(def_idx),
                .source_def_idx = @intFromEnum(def_idx),
            });

            try entries.append(allocator, .{
                .key = .{
                    .owner = try methodOwnerForRegistryEntry(module, names, module_name, entry.key.type_ident),
                    .method = try names.internMethodIdent(idents, entry.key.method_ident),
                },
                .target = .{
                    .module_idx = module_idx,
                    .def_idx = def_idx,
                    .proc = .{ .artifact = template.artifact, .proc_base = proc_base },
                    .template = template,
                    .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.defType(def_idx)),
                },
            });
        }

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }
};

fn methodOwnerForRegistryEntry(
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    module_name: canonical.ModuleNameId,
    type_ident: Ident.Idx,
) Allocator.Error!MethodOwner {
    if (builtinOwnerForRegistryEntry(module, type_ident)) |owner| {
        return .{ .builtin = owner };
    }
    const owner_type_ident = registryNominalOwnerIdent(module, type_ident);
    return .{ .nominal = .{
        .module_name = module_name,
        .type_name = try names.internTypeIdent(module.identStoreConst(), owner_type_ident),
    } };
}

fn registryNominalOwnerIdent(module: TypedCIR.Module, type_ident: Ident.Idx) Ident.Idx {
    const module_env = module.moduleEnvConst();
    const node_idx = module_env.getExposedNodeIndexById(type_ident) orelse return type_ident;
    const stmt_idx: CIR.Statement.Idx = @enumFromInt(@as(u32, @intCast(node_idx)));
    const stmt = module_env.store.getStatement(stmt_idx);
    return switch (stmt) {
        .s_nominal_decl => |nominal| module_env.store.getTypeHeader(nominal.header).relative_name,
        .s_alias_decl => |alias| module_env.store.getTypeHeader(alias.header).relative_name,
        else => type_ident,
    };
}

fn builtinOwnerForRegistryEntry(
    module: TypedCIR.Module,
    type_ident: Ident.Idx,
) ?BuiltinOwner {
    const common = module.moduleEnvConst().idents;
    const module_ident = module.qualifiedModuleIdent();
    const is_builtin_module = module_ident.eql(common.builtin_module) or
        module.identStoreConst().idxTextEql(module_ident, common.builtin_module);
    if (!is_builtin_module) return null;

    if (type_ident.eql(common.bool) or type_ident.eql(common.bool_type)) return .bool;
    if (type_ident.eql(common.str) or type_ident.eql(common.builtin_str)) return .str;
    if (type_ident.eql(common.u8) or type_ident.eql(common.u8_type)) return .u8;
    if (type_ident.eql(common.i8) or type_ident.eql(common.i8_type)) return .i8;
    if (type_ident.eql(common.u16) or type_ident.eql(common.u16_type)) return .u16;
    if (type_ident.eql(common.i16) or type_ident.eql(common.i16_type)) return .i16;
    if (type_ident.eql(common.u32) or type_ident.eql(common.u32_type)) return .u32;
    if (type_ident.eql(common.i32) or type_ident.eql(common.i32_type)) return .i32;
    if (type_ident.eql(common.u64) or type_ident.eql(common.u64_type)) return .u64;
    if (type_ident.eql(common.i64) or type_ident.eql(common.i64_type)) return .i64;
    if (type_ident.eql(common.u128) or type_ident.eql(common.u128_type)) return .u128;
    if (type_ident.eql(common.i128) or type_ident.eql(common.i128_type)) return .i128;
    if (type_ident.eql(common.f32) or type_ident.eql(common.f32_type)) return .f32;
    if (type_ident.eql(common.f64) or type_ident.eql(common.f64_type)) return .f64;
    if (type_ident.eql(common.dec) or type_ident.eql(common.dec_type)) return .dec;
    if (type_ident.eql(common.list)) return .list;
    if (type_ident.eql(common.box)) return .box;
    return null;
}

fn methodKeyEql(a: MethodKey, b: MethodKey) bool {
    return methodOwnerEql(a.owner, b.owner) and a.method == b.method;
}

fn methodOwnerEql(a: MethodOwner, b: MethodOwner) bool {
    return switch (a) {
        .nominal => |a_nominal| switch (b) {
            .nominal => |b_nominal| a_nominal.module_name == b_nominal.module_name and
                a_nominal.type_name == b_nominal.type_name,
            else => false,
        },
        .builtin => |a_builtin| switch (b) {
            .builtin => |b_builtin| a_builtin == b_builtin,
            else => false,
        },
    };
}

/// Public `StaticDispatchResultMode` declaration.
pub const StaticDispatchResultMode = union(enum) {
    value,
    equality: struct {
        structural_allowed: bool,
        negated: bool,
    },
};

/// Public `StaticDispatchCallPlan` declaration.
pub const StaticDispatchCallPlan = struct {
    expr: CheckedExprId,
    method: canonical.MethodNameId,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    args: []const CheckedExprId,
    result_mode: StaticDispatchResultMode,
};

/// Public `StaticDispatchPlanId` declaration.
pub const StaticDispatchPlanId = enum(u32) { _ };

/// Public `IteratorForPlanId` declaration.
pub const IteratorForPlanId = enum(u32) { _ };

/// Public `IteratorDispatchOperand` declaration.
pub const IteratorDispatchOperand = union(enum) {
    checked_expr: CheckedExprId,
    loop_iterator_state,
};

/// Public `IteratorDispatchCall` declaration.
pub const IteratorDispatchCall = struct {
    method: canonical.MethodNameId,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    dispatcher_arg_index: u32,
    args: []const IteratorDispatchOperand,
};

/// Public `IteratorForPlan` declaration.
pub const IteratorForPlan = struct {
    iter: IteratorDispatchCall,
    next: IteratorDispatchCall,
    iterable: CheckedExprId,
    item_ty: CheckedTypeId,
    iterator_ty: CheckedTypeId,
    step_ty: CheckedTypeId,
};

/// Public `StaticDispatchPlanTable` declaration.
pub const StaticDispatchPlanTable = struct {
    plans: []StaticDispatchCallPlan = &.{},
    by_expr: std.AutoHashMapUnmanaged(CIR.Expr.Idx, StaticDispatchPlanId) = .{},
    iterator_for_plans: []IteratorForPlan = &.{},
    iterator_for_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, IteratorForPlanId) = .{},
    template_refs: []StaticDispatchPlanId = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        checked_types: anytype,
        checked_bodies: anytype,
    ) Allocator.Error!StaticDispatchPlanTable {
        var plans = std.ArrayList(StaticDispatchCallPlan).empty;
        errdefer {
            for (plans.items) |plan| allocator.free(plan.args);
            plans.deinit(allocator);
        }
        var by_expr: std.AutoHashMapUnmanaged(CIR.Expr.Idx, StaticDispatchPlanId) = .{};
        errdefer by_expr.deinit(allocator);
        var iterator_for_plans = std.ArrayList(IteratorForPlan).empty;
        errdefer {
            for (iterator_for_plans.items) |plan| {
                allocator.free(plan.iter.args);
                allocator.free(plan.next.args);
            }
            iterator_for_plans.deinit(allocator);
        }
        var iterator_for_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, IteratorForPlanId) = .{};
        errdefer iterator_for_by_node.deinit(allocator);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const tag = module.nodeTag(@enumFromInt(node_idx));
            switch (tag) {
                .expr_dispatch_call,
                .expr_type_dispatch_call,
                .expr_method_eq,
                => {},
                else => continue,
            }

            const expr_idx: CIR.Expr.Idx = @enumFromInt(node_idx);
            const checked_expr = checkedExprIdForSource(checked_bodies, expr_idx);
            const expr = module.expr(expr_idx);
            const idents = module.identStoreConst();
            const plan_id: StaticDispatchPlanId = @enumFromInt(@as(u32, @intCast(plans.items.len)));
            switch (expr.data) {
                .e_dispatch_call => |dispatch_call| {
                    const explicit_args = module.sliceExpr(dispatch_call.args);
                    const args = try allocator.alloc(CheckedExprId, explicit_args.len + 1);
                    args[0] = checkedExprIdForSource(checked_bodies, dispatch_call.receiver);
                    for (explicit_args, 0..) |arg, i| {
                        args[i + 1] = checkedExprIdForSource(checked_bodies, arg);
                    }

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(dispatch_call.receiver)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatch_call.constraint_fn_var),
                        .args = args,
                        .result_mode = staticDispatchResultModeForCheckedValueCall(module, dispatch_call.method_name, dispatch_call.constraint_fn_var),
                    });
                },
                .e_type_dispatch_call => |dispatch_call| {
                    const alias_stmt = module.getStatement(dispatch_call.type_var_alias_stmt);
                    const args = try checkedExprIdsForSlice(allocator, checked_bodies, module.sliceExpr(dispatch_call.args));

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, ModuleEnv.varFrom(alias_stmt.s_type_var_alias.type_var_anno)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatch_call.constraint_fn_var),
                        .args = args,
                        .result_mode = staticDispatchResultModeForCheckedValueCall(module, dispatch_call.method_name, dispatch_call.constraint_fn_var),
                    });
                },
                .e_method_eq => |eq| {
                    const args = try checkedExprIdsForSlice(allocator, checked_bodies, &.{ eq.lhs, eq.rhs });

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, module.commonIdents().is_eq),
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(eq.lhs)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, eq.constraint_fn_var),
                        .args = args,
                        .result_mode = .{ .equality = .{
                            .structural_allowed = true,
                            .negated = eq.negated,
                        } },
                    });
                },
                else => unreachable,
            }
            try by_expr.put(allocator, expr_idx, plan_id);
        }

        const module_env = module.moduleEnvConst();
        for (module_env.for_loop_dispatch_plans.items.items) |for_plan| {
            const for_node_idx: CIR.Node.Idx = @enumFromInt(for_plan.node_idx);
            const pattern_idx: CIR.Pattern.Idx = @enumFromInt(for_plan.pattern_idx);
            const iterable_idx: CIR.Expr.Idx = @enumFromInt(for_plan.iterable_idx);

            const iterable_expr = checkedExprIdForSource(checked_bodies, iterable_idx);
            const item_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.patternType(pattern_idx));
            const iter_callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(for_plan.iter_fn_var));
            const next_callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(for_plan.next_fn_var));
            const iterator_ty = checkedFunctionReturnTypeId(checked_types, iter_callable_ty);
            const step_ty = checkedFunctionReturnTypeId(checked_types, next_callable_ty);

            const iterator_for_id: IteratorForPlanId = @enumFromInt(@as(u32, @intCast(iterator_for_plans.items.len)));
            {
                const iter_args = try allocator.alloc(IteratorDispatchOperand, 1);
                errdefer allocator.free(iter_args);
                iter_args[0] = .{ .checked_expr = iterable_expr };

                const next_args = try allocator.alloc(IteratorDispatchOperand, 1);
                errdefer allocator.free(next_args);
                next_args[0] = .loop_iterator_state;

                try iterator_for_plans.append(allocator, .{
                    .iter = .{
                        .method = try names.internMethodName("iter"),
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(iterable_idx)),
                        .callable_ty = iter_callable_ty,
                        .dispatcher_arg_index = 0,
                        .args = iter_args,
                    },
                    .next = .{
                        .method = try names.internMethodName("next"),
                        .dispatcher_ty = iterator_ty,
                        .callable_ty = next_callable_ty,
                        .dispatcher_arg_index = 0,
                        .args = next_args,
                    },
                    .iterable = iterable_expr,
                    .item_ty = item_ty,
                    .iterator_ty = iterator_ty,
                    .step_ty = step_ty,
                });
            }
            try iterator_for_by_node.put(allocator, for_node_idx, iterator_for_id);
        }

        return .{
            .plans = try plans.toOwnedSlice(allocator),
            .by_expr = by_expr,
            .iterator_for_plans = try iterator_for_plans.toOwnedSlice(allocator),
            .iterator_for_by_node = iterator_for_by_node,
        };
    }

    pub fn lookupByExpr(self: *const StaticDispatchPlanTable, expr: CIR.Expr.Idx) ?StaticDispatchPlanId {
        return self.by_expr.get(expr);
    }

    pub fn lookupIteratorForByNode(self: *const StaticDispatchPlanTable, node: CIR.Node.Idx) ?IteratorForPlanId {
        return self.iterator_for_by_node.get(node);
    }

    pub fn appendTemplateRefSpan(
        self: *StaticDispatchPlanTable,
        allocator: Allocator,
        refs: []const StaticDispatchPlanId,
    ) Allocator.Error!struct { start: u32, len: u32 } {
        const start: u32 = @intCast(self.template_refs.len);
        if (refs.len == 0) return .{ .start = start, .len = 0 };
        const old = self.template_refs;
        const next = try allocator.alloc(StaticDispatchPlanId, old.len + refs.len);
        @memcpy(next[0..old.len], old);
        @memcpy(next[old.len..], refs);
        allocator.free(old);
        self.template_refs = next;
        return .{ .start = start, .len = @intCast(refs.len) };
    }

    pub fn deinit(self: *StaticDispatchPlanTable, allocator: Allocator) void {
        allocator.free(self.template_refs);
        self.by_expr.deinit(allocator);
        self.iterator_for_by_node.deinit(allocator);
        for (self.plans) |plan| allocator.free(plan.args);
        allocator.free(self.plans);
        for (self.iterator_for_plans) |plan| {
            allocator.free(plan.iter.args);
            allocator.free(plan.next.args);
        }
        allocator.free(self.iterator_for_plans);
        self.* = .{};
    }
};

fn staticDispatchResultModeForCheckedValueCall(
    module: TypedCIR.Module,
    method_name: Ident.Idx,
    constraint_fn_var: Var,
) StaticDispatchResultMode {
    const common = module.commonIdents();
    if (!method_name.eql(common.is_eq)) return .value;

    if (staticDispatchConstraintForFnVar(module, constraint_fn_var)) |constraint| {
        if (constraint.origin == .desugared_binop) {
            return .{ .equality = .{
                .structural_allowed = true,
                .negated = constraint.binop_negated,
            } };
        }
    }

    if (sourceCallableHasEqualityShape(module, constraint_fn_var)) {
        return .{ .equality = .{
            .structural_allowed = true,
            .negated = false,
        } };
    }

    return .value;
}

fn staticDispatchConstraintForFnVar(
    module: TypedCIR.Module,
    fn_var: Var,
) ?types.StaticDispatchConstraint {
    const store = module.typeStoreConst();
    for (store.static_dispatch_constraints.items.items) |constraint| {
        if (constraint.fn_var == fn_var) return constraint;
    }
    return null;
}

fn sourceCallableHasEqualityShape(
    module: TypedCIR.Module,
    fn_var: Var,
) bool {
    const store = module.typeStoreConst();
    const resolved = store.resolveVar(fn_var);
    const func = resolved.desc.content.unwrapFunc() orelse return false;
    const args = store.sliceVars(func.args);
    if (args.len != 2) return false;
    if (store.resolveVar(args[0]).var_ != store.resolveVar(args[1]).var_) return false;
    return sourceVarIsBool(module, func.ret);
}

fn sourceVarIsBool(module: TypedCIR.Module, var_: Var) bool {
    const store = module.typeStoreConst();
    var current = var_;
    while (true) {
        const resolved = store.resolveVar(current);
        switch (resolved.desc.content) {
            .structure => |flat| switch (flat) {
                .nominal_type => |nominal| {
                    const common = module.commonIdents();
                    const builtin_origin = nominal.origin_module.eql(common.builtin_module) or
                        module.identStoreConst().idxTextEql(nominal.origin_module, common.builtin_module);
                    return builtin_origin and (nominal.ident.ident_idx.eql(common.bool) or
                        nominal.ident.ident_idx.eql(common.bool_type));
                },
                else => return false,
            },
            .alias => |alias| current = store.getAliasBackingVar(alias),
            .flex,
            .rigid,
            .err,
            => return false,
        }
    }
}

fn checkedTypeIdForVar(
    _: Allocator,
    module: TypedCIR.Module,
    checked_types: anytype,
    var_: Var,
) Allocator.Error!CheckedTypeId {
    return checked_types.rootForSourceVar(module, var_) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: dispatch type root was not published", .{});
        }
        unreachable;
    };
}

fn checkedFunctionReturnTypeId(
    checked_types: anytype,
    callable_ty: CheckedTypeId,
) CheckedTypeId {
    const raw = @intFromEnum(callable_ty);
    if (raw >= checked_types.store.payloads.len) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: callable type root was outside the checked type store", .{});
        }
        unreachable;
    }
    return switch (checked_types.store.payloads[raw]) {
        .function => |func| func.ret,
        else => if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: for-loop dispatch constraint was not a function", .{});
        } else unreachable,
    };
}

fn checkedExprIdsForSlice(
    allocator: Allocator,
    checked_bodies: anytype,
    exprs: []const CIR.Expr.Idx,
) Allocator.Error![]const CheckedExprId {
    if (exprs.len == 0) return &.{};
    const out = try allocator.alloc(CheckedExprId, exprs.len);
    errdefer allocator.free(out);
    for (exprs, 0..) |expr, i| {
        out[i] = checkedExprIdForSource(checked_bodies, expr);
    }
    return out;
}

fn checkedExprIdForSource(checked_bodies: anytype, expr: CIR.Expr.Idx) CheckedExprId {
    return checked_bodies.exprIdForSource(expr) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch invariant violated: dispatch expression {d} has no checked expression id",
                .{@intFromEnum(expr)},
            );
        }
        unreachable;
    };
}

test "method registry can be empty" {
    var registry: MethodRegistry = .{};
    registry.deinit(std.testing.allocator);
}
