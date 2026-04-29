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

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;

pub const ProcedureTemplateLookup = struct {
    module_idx: u32,
    by_def: []const ?canonical.ProcedureTemplateRef,

    pub fn templateForDef(self: *const ProcedureTemplateLookup, def_idx: CIR.Def.Idx) ?canonical.ProcedureTemplateRef {
        const raw = @intFromEnum(def_idx);
        if (raw >= self.by_def.len) return null;
        return self.by_def[raw];
    }
};

pub const MethodOwner = union(enum) {
    nominal: canonical.NominalTypeKey,
    builtin: BuiltinOwner,
};

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

pub const MethodKey = struct {
    owner: MethodOwner,
    method: canonical.MethodNameId,
};

pub const MethodTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    proc: canonical.ProcedureValueRef,
    template: ?canonical.ProcedureTemplateRef,
    callable_var: Var,
};

pub const MethodRegistryEntry = struct {
    key: MethodKey,
    target: MethodTarget,
};

pub const MethodRegistry = struct {
    entries: []MethodRegistryEntry = &.{},

    pub fn deinit(self: *MethodRegistry, allocator: Allocator) void {
        allocator.free(self.entries);
        self.* = .{};
    }

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        local_templates: *const ProcedureTemplateLookup,
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
                    .callable_var = ModuleEnv.varFrom(def_idx),
                },
            });
        }

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }
};

pub const StaticDispatchResultMode = union(enum) {
    value,
    equality: struct {
        structural_allowed: bool,
        negated: bool,
    },
};

pub const StaticDispatchCallPlan = struct {
    expr: CIR.Expr.Idx,
    method: canonical.MethodNameId,
    dispatcher_var: Var,
    callable_var: Var,
    args: []const CIR.Expr.Idx,
    result_mode: StaticDispatchResultMode,
};

pub const StaticDispatchPlanTable = struct {
    plans: []StaticDispatchCallPlan = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
    ) Allocator.Error!StaticDispatchPlanTable {
        var plans = std.ArrayList(StaticDispatchCallPlan).empty;
        errdefer {
            for (plans.items) |plan| allocator.free(plan.args);
            plans.deinit(allocator);
        }

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
            const expr = module.expr(expr_idx);
            const idents = module.identStoreConst();
            switch (expr.data) {
                .e_dispatch_call => |dispatch_call| {
                    const explicit_args = module.sliceExpr(dispatch_call.args);
                    const args = try allocator.alloc(CIR.Expr.Idx, explicit_args.len + 1);
                    args[0] = dispatch_call.receiver;
                    @memcpy(args[1..], explicit_args);

                    try plans.append(allocator, .{
                        .expr = expr_idx,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher_var = module.exprType(dispatch_call.receiver),
                        .callable_var = dispatch_call.constraint_fn_var,
                        .args = args,
                        .result_mode = .value,
                    });
                },
                .e_type_dispatch_call => |dispatch_call| {
                    const alias_stmt = module.getStatement(dispatch_call.type_var_alias_stmt);
                    const args = try allocator.dupe(CIR.Expr.Idx, module.sliceExpr(dispatch_call.args));

                    try plans.append(allocator, .{
                        .expr = expr_idx,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher_var = ModuleEnv.varFrom(alias_stmt.s_type_var_alias.type_var_anno),
                        .callable_var = dispatch_call.constraint_fn_var,
                        .args = args,
                        .result_mode = .value,
                    });
                },
                .e_method_eq => |eq| {
                    const args = try allocator.dupe(CIR.Expr.Idx, &.{ eq.lhs, eq.rhs });

                    try plans.append(allocator, .{
                        .expr = expr_idx,
                        .method = try names.internMethodIdent(idents, module.commonIdents().is_eq),
                        .dispatcher_var = module.exprType(eq.lhs),
                        .callable_var = eq.constraint_fn_var,
                        .args = args,
                        .result_mode = .{ .equality = .{
                            .structural_allowed = true,
                            .negated = eq.negated,
                        } },
                    });
                },
                else => unreachable,
            }
        }

        return .{ .plans = try plans.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: *StaticDispatchPlanTable, allocator: Allocator) void {
        for (self.plans) |plan| allocator.free(plan.args);
        allocator.free(self.plans);
        self.* = .{};
    }
};

test "method registry can be empty" {
    var registry: MethodRegistry = .{};
    registry.deinit(std.testing.allocator);
}
