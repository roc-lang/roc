//! Checked static-dispatch target registry and normalized dispatch-site records.
//!
//! The registry is built at the checked-artifact boundary. Later MIR stages use
//! it as a target table only; the dispatch-site record chooses the dispatcher
//! type variable explicitly.

const std = @import("std");
const can = @import("can");
const types = @import("types");
const TypedCIR = @import("typed_cir.zig");
const canonical = @import("canonical_names.zig");
const checked_ids = @import("checked_ids.zig");
const canonical_type_keys = @import("canonical_type_keys.zig");

const Allocator = std.mem.Allocator;
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
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch registry invariant violated: method def {d} has no checked procedure template",
                        .{@intFromEnum(def_idx)},
                    );
                }
                unreachable;
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
                    .owner = .{ .nominal = .{
                        .module_name = module_name,
                        .type_name = try names.internTypeIdent(idents, entry.key.type_ident),
                    } },
                    .method = try names.internMethodIdent(idents, entry.key.method_ident),
                },
                .target = .{
                    .module_idx = module_idx,
                    .def_idx = def_idx,
                    .proc = .{ .artifact = template.artifact, .proc_base = proc_base },
                    .template = template,
                    .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, ModuleEnv.varFrom(def_idx)),
                },
            });
        }

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }
};

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

/// Public `StaticDispatchPlanTable` declaration.
pub const StaticDispatchPlanTable = struct {
    plans: []StaticDispatchCallPlan = &.{},
    by_expr: std.AutoHashMapUnmanaged(CIR.Expr.Idx, StaticDispatchPlanId) = .{},
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
                        .result_mode = .value,
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
                        .result_mode = .value,
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

        return .{
            .plans = try plans.toOwnedSlice(allocator),
            .by_expr = by_expr,
        };
    }

    pub fn lookupByExpr(self: *const StaticDispatchPlanTable, expr: CIR.Expr.Idx) ?StaticDispatchPlanId {
        return self.by_expr.get(expr);
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
        for (self.plans) |plan| allocator.free(plan.args);
        allocator.free(self.plans);
        self.* = .{};
    }
};

fn checkedTypeIdForVar(
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: anytype,
    var_: Var,
) Allocator.Error!CheckedTypeId {
    const key = try canonical_type_keys.fromVar(
        allocator,
        module.typeStoreConst(),
        module.identStoreConst(),
        var_,
    );
    return checked_types.rootForKey(key) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: dispatch type root was not published", .{});
        }
        unreachable;
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
