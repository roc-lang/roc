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
const CheckedStringLiteralId = checked_ids.CheckedStringLiteralId;
const PatternBinderId = checked_ids.PatternBinderId;

/// Public `ProcedureTemplateLookup` declaration.
pub const ProcedureTemplateLookup = struct {
    module_idx: u32,
    by_def: []const ProcedureTemplateLookupEntry = &.{},

    pub fn templateForDef(self: *const ProcedureTemplateLookup, def_idx: CIR.Def.Idx) ?canonical.ProcedureTemplateRef {
        var lo: usize = 0;
        var hi: usize = self.by_def.len;
        const target = @intFromEnum(def_idx);
        while (lo < hi) {
            const mid = lo + (hi - lo) / 2;
            const candidate = @intFromEnum(self.by_def[mid].def);
            if (candidate < target) {
                lo = mid + 1;
            } else {
                hi = mid;
            }
        }
        if (lo >= self.by_def.len or self.by_def[lo].def != def_idx) return null;
        return self.by_def[lo].template;
    }
};

/// Public `ProcedureTemplateLookupEntry` declaration.
pub const ProcedureTemplateLookupEntry = struct {
    def: CIR.Def.Idx,
    template: canonical.ProcedureTemplateRef,

    pub fn lessThan(_: void, lhs: ProcedureTemplateLookupEntry, rhs: ProcedureTemplateLookupEntry) bool {
        return @intFromEnum(lhs.def) < @intFromEnum(rhs.def);
    }
};

/// Public `MethodOwner` declaration.
pub const MethodOwner = union(enum) {
    nominal: canonical.NominalTypeKey,
    source_decl: struct {
        module_name: canonical.ModuleNameId,
        statement: u32,
    },
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

/// Public `ProcedureMethodTarget` declaration.
pub const ProcedureMethodTarget = struct {
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
};

/// Public `LocalProcedureMethodTarget` declaration.
pub const LocalProcedureMethodTarget = struct {
    binder: PatternBinderId,
    expr: CheckedExprId,
};

/// Public `MethodTargetKind` declaration.
pub const MethodTargetKind = union(enum) {
    procedure: ProcedureMethodTarget,
    local_proc: LocalProcedureMethodTarget,
};

/// Public `MethodTarget` declaration.
pub const MethodTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    kind: MethodTargetKind,
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
        var low: usize = 0;
        var high: usize = self.entries.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            const entry = self.entries[mid];
            switch (methodKeyOrder(entry.key, key)) {
                .eq => return entry.target,
                .lt => low = mid + 1,
                .gt => high = mid,
            }
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
        checked_bodies: anytype,
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

        for (module.methodDefEntries()) |entry| {
            const method_ident = module_env.lookupMethodIdentForOwnerConst(entry.key.owner, entry.key.methodIdent()) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch registry invariant violated: method def for owner {d} method {d} has no method ident",
                        .{ @intFromEnum(entry.key.owner), entry.key.method_ident_bits },
                    );
                }
                unreachable;
            };
            const def_idx = entry.value.def_idx;
            const target_kind: MethodTargetKind = if (local_templates.templateForDef(def_idx)) |template| blk: {
                const export_name = try names.internExportIdent(idents, method_ident);
                const proc_base = try names.internProcBase(.{
                    .module_name = module_name,
                    .export_name = export_name,
                    .kind = .checked_source,
                    .ordinal = @intFromEnum(def_idx),
                    .source_def_idx = @intFromEnum(def_idx),
                });
                break :blk .{ .procedure = .{
                    .proc = .{ .artifact = template.artifact, .proc_base = proc_base },
                    .template = template,
                } };
            } else if (localProcedureTargetForMethodBinding(module, checked_bodies, entry.value)) |local|
                .{ .local_proc = local }
            else
                // Associated values without arguments are checked field access,
                // not static-dispatch call targets. The method registry is a
                // procedure-target table for Monotype static dispatch lowering,
                // so only procedure-backed entries belong here.
                continue;
            const callable_var = methodTargetCallableVar(module, def_idx, entry.value, target_kind);

            try entries.append(allocator, .{
                .key = .{
                    .owner = try methodOwnerForRegistryEntry(module, module_name, entry.key.owner),
                    .method = try names.internMethodIdent(idents, entry.key.methodIdent()),
                },
                .target = .{
                    .module_idx = module_idx,
                    .def_idx = def_idx,
                    .kind = target_kind,
                    .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, callable_var),
                },
            });
        }

        finalizeMethodRegistryEntries(entries.items);

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }
};

fn methodTargetCallableVar(
    module: TypedCIR.Module,
    def_idx: CIR.Def.Idx,
    binding: ModuleEnv.MethodBinding,
    target_kind: MethodTargetKind,
) Var {
    return switch (target_kind) {
        .procedure => module.defType(def_idx),
        .local_proc => blk: {
            const raw_node = @intFromEnum(binding.type_node_idx);
            const statement: CIR.Statement.Idx = @enumFromInt(raw_node);
            const decl = switch (module.getStatement(statement)) {
                .s_decl => |decl| decl,
                else => unreachable,
            };
            break :blk module.exprType(decl.expr);
        },
    };
}

fn localProcedureTargetForMethodBinding(
    module: TypedCIR.Module,
    checked_bodies: anytype,
    binding: ModuleEnv.MethodBinding,
) ?LocalProcedureMethodTarget {
    const raw_node = @intFromEnum(binding.type_node_idx);
    if (raw_node >= module.nodeCount()) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: method binding node {d} is outside the module node store",
                .{raw_node},
            );
        }
        unreachable;
    }
    if (module.nodeTag(binding.type_node_idx) != .statement_decl) return null;

    const statement: CIR.Statement.Idx = @enumFromInt(raw_node);
    const decl = switch (module.getStatement(statement)) {
        .s_decl => |decl| decl,
        else => return null,
    };

    if (!localProcedureExpr(module, decl.expr)) return null;

    const expr = checked_bodies.exprIdForSource(decl.expr) orelse return null;
    const binder = checked_bodies.patternBinderForSource(decl.pattern) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: local method pattern {d} has no checked binder",
                .{@intFromEnum(decl.pattern)},
            );
        }
        unreachable;
    };

    return .{ .binder = binder, .expr = expr };
}

fn localProcedureExpr(module: TypedCIR.Module, expr_idx: CIR.Expr.Idx) bool {
    return switch (module.expr(expr_idx).data) {
        .e_lambda, .e_closure => true,
        else => false,
    };
}

fn methodOwnerForRegistryEntry(
    module: TypedCIR.Module,
    module_name: canonical.ModuleNameId,
    owner_stmt: CIR.Statement.Idx,
) Allocator.Error!MethodOwner {
    if (builtinOwnerForRegistryEntry(module, owner_stmt)) |owner| {
        return .{ .builtin = owner };
    }
    return .{ .source_decl = .{
        .module_name = module_name,
        .statement = @intFromEnum(owner_stmt),
    } };
}

fn builtinOwnerForRegistryEntry(
    module: TypedCIR.Module,
    owner_stmt: CIR.Statement.Idx,
) ?BuiltinOwner {
    const module_env = module.moduleEnvConst();
    const common = module_env.idents;
    if (module_env.module_role != .builtin) return null;

    const stmt = module_env.store.getStatement(owner_stmt);
    const type_ident = switch (stmt) {
        .s_nominal_decl => |nominal| module_env.store.getTypeHeader(nominal.header).name,
        .s_alias_decl => |alias| module_env.store.getTypeHeader(alias.header).name,
        else => return null,
    };

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

    if (type_ident.eql(common.list) or type_ident.eql(common.builtin_list)) return .list;
    if (type_ident.eql(common.box) or type_ident.eql(common.builtin_box)) return .box;
    return null;
}

fn methodRegistryEntryLessThan(_: void, a: MethodRegistryEntry, b: MethodRegistryEntry) bool {
    return methodKeyOrder(a.key, b.key) == .lt;
}

fn finalizeMethodRegistryEntries(entries: []MethodRegistryEntry) void {
    std.mem.sort(MethodRegistryEntry, entries, {}, methodRegistryEntryLessThan);
    assertMethodRegistryKeysUnique(entries);
}

fn assertMethodRegistryKeysUnique(entries: []const MethodRegistryEntry) void {
    if (entries.len < 2) return;
    var i: usize = 1;
    while (i < entries.len) : (i += 1) {
        if (methodKeyOrder(entries[i - 1].key, entries[i].key) != .eq) continue;
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch registry invariant violated: duplicate method registry key", .{});
        }
        unreachable;
    }
}

fn methodKeyOrder(a: MethodKey, b: MethodKey) std.math.Order {
    const owner_order = methodOwnerOrder(a.owner, b.owner);
    if (owner_order != .eq) return owner_order;
    return orderEnum(canonical.MethodNameId, a.method, b.method);
}

fn methodOwnerOrder(a: MethodOwner, b: MethodOwner) std.math.Order {
    const a_tag = methodOwnerTagRank(a);
    const b_tag = methodOwnerTagRank(b);
    if (a_tag != b_tag) return orderU32(a_tag, b_tag);

    return switch (a) {
        .nominal => |a_nominal| switch (b) {
            .nominal => |b_nominal| blk: {
                const module_order = orderEnum(canonical.ModuleNameId, a_nominal.module_name, b_nominal.module_name);
                if (module_order != .eq) break :blk module_order;
                const type_order = orderEnum(canonical.TypeNameId, a_nominal.type_name, b_nominal.type_name);
                if (type_order != .eq) break :blk type_order;
                break :blk orderOptionalU32(a_nominal.source_decl, b_nominal.source_decl);
            },
            else => unreachable,
        },
        .source_decl => |a_decl| switch (b) {
            .source_decl => |b_decl| blk: {
                const module_order = orderEnum(canonical.ModuleNameId, a_decl.module_name, b_decl.module_name);
                if (module_order != .eq) break :blk module_order;
                break :blk orderU32(a_decl.statement, b_decl.statement);
            },
            else => unreachable,
        },
        .builtin => |a_builtin| switch (b) {
            .builtin => |b_builtin| orderEnum(BuiltinOwner, a_builtin, b_builtin),
            else => unreachable,
        },
    };
}

fn methodOwnerTagRank(owner: MethodOwner) u32 {
    return switch (owner) {
        .nominal => 0,
        .source_decl => 1,
        .builtin => 2,
    };
}

fn orderOptionalU32(a: ?u32, b: ?u32) std.math.Order {
    if (a) |a_value| {
        return if (b) |b_value| orderU32(a_value, b_value) else .gt;
    }
    return if (b == null) .eq else .lt;
}

fn orderEnum(comptime T: type, a: T, b: T) std.math.Order {
    return orderU32(@intFromEnum(a), @intFromEnum(b));
}

fn orderU32(a: u32, b: u32) std.math.Order {
    if (a == b) return .eq;
    return if (a < b) .lt else .gt;
}

/// Public `StaticDispatchResultMode` declaration.
pub const StaticDispatchResultMode = union(enum) {
    value,
    equality: struct {
        structural_allowed: bool,
        negated: bool,
    },
    decoder: struct {
        structural_allowed: bool,
    },
};

/// Public `StaticDispatchDispatcher` declaration.
pub const StaticDispatchDispatcher = union(enum) {
    arg: u32,
    type_only,
};

/// Public `StaticDispatchOperand` declaration.
pub const StaticDispatchOperand = union(enum) {
    checked_expr: CheckedExprId,
    generated_numeral: ModuleEnv.NumeralLiteral,
    /// A string literal's post-escape contents, passed to `from_quote` as Str.
    generated_quote: CheckedStringLiteralId,
};

/// Public `StaticDispatchCallPlan` declaration.
pub const StaticDispatchCallPlan = struct {
    expr: CheckedExprId,
    method: canonical.MethodNameId,
    dispatcher: StaticDispatchDispatcher,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    args: []const StaticDispatchOperand,
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
    numeral_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, StaticDispatchPlanId) = .{},
    quote_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, StaticDispatchPlanId) = .{},
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
        var numeral_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, StaticDispatchPlanId) = .{};
        errdefer numeral_by_node.deinit(allocator);
        var quote_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, StaticDispatchPlanId) = .{};
        errdefer quote_by_node.deinit(allocator);
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

        var constraint_index = try StaticDispatchConstraintIndex.fromModule(allocator, module, checked_bodies);
        defer constraint_index.deinit(allocator);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const tag = module.nodeTag(@enumFromInt(node_idx));
            switch (tag) {
                .expr_dispatch_call,
                .expr_interpolation,
                .expr_type_dispatch_call,
                .expr_method_eq,
                => {},
                else => continue,
            }

            const expr_idx: CIR.Expr.Idx = @enumFromInt(node_idx);
            const checked_expr = checked_bodies.exprIdForSource(expr_idx) orelse continue;
            const expr = module.expr(expr_idx);
            const checked_expr_data = checked_bodies.exprs[@intFromEnum(checked_expr)].data;
            const idents = module.identStoreConst();
            const plan_id: StaticDispatchPlanId = @enumFromInt(@as(u32, @intCast(plans.items.len)));
            switch (expr.data) {
                .e_dispatch_call => |dispatch_call| {
                    const explicit_args = module.sliceExpr(dispatch_call.args);
                    const args = try allocator.alloc(StaticDispatchOperand, explicit_args.len + 1);
                    args[0] = .{ .checked_expr = checkedExprIdForSource(checked_bodies, dispatch_call.receiver) };
                    for (explicit_args, 0..) |arg, i| {
                        args[i + 1] = .{ .checked_expr = checkedExprIdForSource(checked_bodies, arg) };
                    }

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher = .{ .arg = 0 },
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(dispatch_call.receiver)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatch_call.constraint_fn_var),
                        .args = args,
                        .result_mode = try staticDispatchResultModeForCheckedValueCall(allocator, module, checked_types, &constraint_index, dispatch_call.method_name, dispatch_call.constraint_fn_var),
                    });
                },
                .e_interpolation => |interpolation| {
                    if (checked_expr_data != .interpolation) continue;
                    const args = try staticDispatchOperandsForSlice(allocator, checked_bodies, &.{ interpolation.first, interpolation.rest });
                    const from_interpolation = try names.internMethodName("from_interpolation");
                    const constraint_fn_var = interpolation.constraint_fn_var orelse unreachable;

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = from_interpolation,
                        .dispatcher = .type_only,
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(expr_idx)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, constraint_fn_var),
                        .args = args,
                        .result_mode = .value,
                    });
                },
                .e_type_dispatch_call => |dispatch_call| {
                    const alias_stmt = module.getStatement(dispatch_call.type_var_alias_stmt);
                    const args = try staticDispatchOperandsForSlice(allocator, checked_bodies, module.sliceExpr(dispatch_call.args));

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher = .type_only,
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, ModuleEnv.varFrom(alias_stmt.s_type_var_alias.type_var_anno)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatch_call.constraint_fn_var),
                        .args = args,
                        .result_mode = try staticDispatchResultModeForCheckedValueCall(allocator, module, checked_types, &constraint_index, dispatch_call.method_name, dispatch_call.constraint_fn_var),
                    });
                },
                .e_method_eq => |eq| {
                    const args = try staticDispatchOperandsForSlice(allocator, checked_bodies, &.{ eq.lhs, eq.rhs });

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, module.commonIdents().is_eq),
                        .dispatcher = .{ .arg = 0 },
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
        for (module_env.numeral_dispatch_plans.items.items) |numeral_plan| {
            const node: CIR.Node.Idx = @enumFromInt(numeral_plan.node_idx);
            const expr_idx: CIR.Expr.Idx = @enumFromInt(numeral_plan.node_idx);
            const checked_expr = checked_bodies.exprIdForSource(expr_idx) orelse
                checked_bodies.numeralConversionExprAtRawNode(numeral_plan.node_idx) orelse
                continue;
            switch (checked_bodies.exprs[@intFromEnum(checked_expr)].data) {
                .num_from_numeral,
                .typed_num_from_numeral,
                => {},
                .num,
                .typed_int,
                .frac_f32,
                .frac_f64,
                .dec,
                .dec_small,
                .typed_frac,
                => continue,
                else => {
                    if (@import("builtin").mode == .Debug) {
                        std.debug.panic(
                            "checked static dispatch invariant violated: numeral dispatch plan {d} points at a non-numeric checked expression",
                            .{numeral_plan.node_idx},
                        );
                    }
                    unreachable;
                },
            }
            const literal = module_env.numeralLiteralForNode(node) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch invariant violated: runtime from_numeral plan {d} has no exact literal",
                        .{numeral_plan.node_idx},
                    );
                }
                unreachable;
            };
            const args = try allocator.alloc(StaticDispatchOperand, 1);
            errdefer allocator.free(args);
            args[0] = .{ .generated_numeral = literal };

            const plan_id: StaticDispatchPlanId = @enumFromInt(@as(u32, @intCast(plans.items.len)));
            try plans.append(allocator, .{
                .expr = checked_expr,
                .method = try names.internMethodName("from_numeral"),
                .dispatcher = .type_only,
                .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(numeral_plan.target_var)),
                .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(numeral_plan.fn_var)),
                .args = args,
                .result_mode = .value,
            });
            try numeral_by_node.put(allocator, node, plan_id);
        }

        for (module_env.quote_dispatch_plans.items.items) |quote_plan| {
            const node: CIR.Node.Idx = @enumFromInt(quote_plan.node_idx);
            const expr_idx: CIR.Expr.Idx = @enumFromInt(quote_plan.node_idx);
            const checked_expr = checked_bodies.exprIdForSource(expr_idx) orelse
                checked_bodies.numeralConversionExprAtRawNode(quote_plan.node_idx) orelse
                continue;
            const literal = switch (checked_bodies.exprs[@intFromEnum(checked_expr)].data) {
                .str_from_quote => |quote| quote.literal,
                // Builtin Str literals keep the direct string encoding.
                .str, .str_segment => continue,
                else => {
                    if (@import("builtin").mode == .Debug) {
                        std.debug.panic(
                            "checked static dispatch invariant violated: quote dispatch plan {d} points at a non-string checked expression",
                            .{quote_plan.node_idx},
                        );
                    }
                    unreachable;
                },
            };
            const args = try allocator.alloc(StaticDispatchOperand, 1);
            errdefer allocator.free(args);
            args[0] = .{ .generated_quote = literal };

            const plan_id: StaticDispatchPlanId = @enumFromInt(@as(u32, @intCast(plans.items.len)));
            try plans.append(allocator, .{
                .expr = checked_expr,
                .method = try names.internMethodName("from_quote"),
                .dispatcher = .type_only,
                .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(quote_plan.target_var)),
                .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(quote_plan.fn_var)),
                .args = args,
                .result_mode = .value,
            });
            try quote_by_node.put(allocator, node, plan_id);
        }

        for (module_env.for_loop_dispatch_plans.items.items) |for_plan| {
            const for_node_idx: CIR.Node.Idx = @enumFromInt(for_plan.node_idx);
            const pattern_idx: CIR.Pattern.Idx = @enumFromInt(for_plan.pattern_idx);
            const iterable_idx: CIR.Expr.Idx = @enumFromInt(for_plan.iterable_idx);

            if (checked_bodies.exprIdForSource(iterable_idx) == null) continue;
            const for_has_checked_node = switch (module.nodeTag(for_node_idx)) {
                .expr_for => checked_bodies.exprIdForSource(@enumFromInt(for_plan.node_idx)) != null,
                .statement_for => checked_bodies.statementIdForSource(@enumFromInt(for_plan.node_idx)) != null,
                else => false,
            };
            if (!for_has_checked_node) continue;

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
            .numeral_by_node = numeral_by_node,
            .quote_by_node = quote_by_node,
            .iterator_for_plans = try iterator_for_plans.toOwnedSlice(allocator),
            .iterator_for_by_node = iterator_for_by_node,
        };
    }

    pub fn lookupByExpr(self: *const StaticDispatchPlanTable, expr: CIR.Expr.Idx) ?StaticDispatchPlanId {
        return self.by_expr.get(expr);
    }

    pub fn lookupNumeralByNode(self: *const StaticDispatchPlanTable, node: CIR.Node.Idx) ?StaticDispatchPlanId {
        return self.numeral_by_node.get(node);
    }

    pub fn lookupQuoteByNode(self: *const StaticDispatchPlanTable, node: CIR.Node.Idx) ?StaticDispatchPlanId {
        return self.quote_by_node.get(node);
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
        self.numeral_by_node.deinit(allocator);
        self.quote_by_node.deinit(allocator);
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

const StaticDispatchConstraintIndex = struct {
    constraints: []const types.StaticDispatchConstraint = &.{},
    by_fn_var: std.AutoHashMapUnmanaged(Var, u32) = .{},

    fn fromModule(allocator: Allocator, module: TypedCIR.Module, checked_bodies: anytype) Allocator.Error!StaticDispatchConstraintIndex {
        const store = module.typeStoreConst();
        var live_fn_vars: std.AutoHashMapUnmanaged(Var, void) = .{};
        defer live_fn_vars.deinit(allocator);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const expr_idx: CIR.Expr.Idx = @enumFromInt(node_idx);
            const constraint_fn_var: ?Var = switch (module.nodeTag(@enumFromInt(node_idx))) {
                .expr_dispatch_call => module.expr(expr_idx).data.e_dispatch_call.constraint_fn_var,
                .expr_interpolation => module.expr(expr_idx).data.e_interpolation.constraint_fn_var,
                .expr_type_dispatch_call => module.expr(expr_idx).data.e_type_dispatch_call.constraint_fn_var,
                .expr_method_eq => module.expr(expr_idx).data.e_method_eq.constraint_fn_var,
                else => null,
            };
            if (constraint_fn_var) |fn_var| {
                const checked_expr = checked_bodies.exprIdForSource(expr_idx) orelse continue;
                if (module.nodeTag(@enumFromInt(node_idx)) == .expr_interpolation and
                    checked_bodies.exprs[@intFromEnum(checked_expr)].data != .interpolation)
                {
                    continue;
                }
                try live_fn_vars.put(allocator, fn_var, {});
            }
        }

        var index = StaticDispatchConstraintIndex{
            .constraints = store.static_dispatch_constraints.items.items,
        };
        errdefer index.deinit(allocator);

        try index.by_fn_var.ensureTotalCapacity(allocator, @intCast(live_fn_vars.count()));
        for (index.constraints, 0..) |constraint, i| {
            if (!live_fn_vars.contains(constraint.fn_var)) continue;
            const entry = try index.by_fn_var.getOrPut(allocator, constraint.fn_var);
            if (entry.found_existing) {
                const existing = index.constraints[entry.value_ptr.*];
                if (staticDispatchConstraintsEquivalent(existing, constraint)) continue;
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch constraint invariant violated: duplicate fn_var {d}; existing idx={d} name={s} origin={s} negated={} new idx={d} name={s} origin={s} negated={}",
                        .{
                            @intFromEnum(constraint.fn_var),
                            entry.value_ptr.*,
                            module.identStoreConst().getText(existing.fn_name),
                            @tagName(existing.origin),
                            existing.binop_negated,
                            i,
                            module.identStoreConst().getText(constraint.fn_name),
                            @tagName(constraint.origin),
                            constraint.binop_negated,
                        },
                    );
                }
                continue;
            }
            entry.value_ptr.* = @intCast(i);
        }

        return index;
    }

    fn lookup(self: *const StaticDispatchConstraintIndex, fn_var: Var) ?types.StaticDispatchConstraint {
        const constraint_idx = self.by_fn_var.get(fn_var) orelse return null;
        return self.constraints[constraint_idx];
    }

    fn deinit(self: *StaticDispatchConstraintIndex, allocator: Allocator) void {
        self.by_fn_var.deinit(allocator);
        self.* = .{};
    }
};

fn staticDispatchConstraintsEquivalent(a: types.StaticDispatchConstraint, b: types.StaticDispatchConstraint) bool {
    return a.fn_name == b.fn_name and
        a.fn_var == b.fn_var and
        a.origin == b.origin and
        a.binop_negated == b.binop_negated and
        std.meta.eql(a.num_literal, b.num_literal);
}

fn staticDispatchResultModeForCheckedValueCall(
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: anytype,
    constraint_index: *const StaticDispatchConstraintIndex,
    method_name: Ident.Idx,
    constraint_fn_var: Var,
) Allocator.Error!StaticDispatchResultMode {
    const common = module.commonIdents();
    if (method_name.eql(common.decoder)) {
        return .{ .decoder = .{
            .structural_allowed = true,
        } };
    }

    if (!method_name.eql(common.is_eq)) return .value;

    if (constraint_index.lookup(constraint_fn_var)) |constraint| {
        if (constraint.origin == .desugared_binop) {
            return .{ .equality = .{
                .structural_allowed = true,
                .negated = constraint.binop_negated,
            } };
        }
    }

    if (try sourceCallableHasEqualityShape(allocator, module, checked_types, constraint_fn_var)) {
        return .{ .equality = .{
            .structural_allowed = true,
            .negated = false,
        } };
    }

    return .value;
}

fn sourceCallableHasEqualityShape(
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: anytype,
    fn_var: Var,
) Allocator.Error!bool {
    const store = module.typeStoreConst();
    const resolved = store.resolveVar(fn_var);
    const func = resolved.desc.content.unwrapFunc() orelse return false;
    const args = store.sliceVars(func.args);
    if (args.len != 2) return false;
    if (store.resolveVar(args[0]).var_ != store.resolveVar(args[1]).var_) return false;
    const ret_ty = try checkedTypeIdForVar(allocator, module, checked_types, func.ret);
    return checkedTypeIsBuiltinBool(checked_types, ret_ty);
}

fn checkedTypeIsBuiltinBool(checked_types: anytype, ty: CheckedTypeId) bool {
    const raw = @intFromEnum(ty);
    if (raw >= checked_types.store.payloads.items.len) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: equality return type root was outside the checked type store", .{});
        }
        unreachable;
    }
    return switch (checked_types.store.payloads.items[raw]) {
        .nominal => |nominal| if (nominal.builtin) |builtin_owner| builtin_owner == .bool else false,
        else => false,
    };
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
    if (raw >= checked_types.store.payloads.items.len) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: callable type root was outside the checked type store", .{});
        }
        unreachable;
    }
    return switch (checked_types.store.payloads.items[raw]) {
        .function => |func| func.ret,
        else => if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: for-loop dispatch constraint was not a function", .{});
        } else unreachable,
    };
}

fn staticDispatchOperandsForSlice(
    allocator: Allocator,
    checked_bodies: anytype,
    exprs: []const CIR.Expr.Idx,
) Allocator.Error![]const StaticDispatchOperand {
    if (exprs.len == 0) return &.{};
    const out = try allocator.alloc(StaticDispatchOperand, exprs.len);
    errdefer allocator.free(out);
    for (exprs, 0..) |expr, i| {
        out[i] = .{ .checked_expr = checkedExprIdForSource(checked_bodies, expr) };
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

test "method registry finalization sorts entries for binary lookup" {
    const allocator = std.testing.allocator;

    const entries = try allocator.alloc(MethodRegistryEntry, 3);
    defer allocator.free(entries);

    entries[0] = .{
        .key = .{ .owner = .{ .builtin = .box }, .method = @enumFromInt(2) },
        .target = testMethodTarget(@enumFromInt(20)),
    };
    entries[1] = .{
        .key = .{ .owner = .{ .builtin = .list }, .method = @enumFromInt(1) },
        .target = testMethodTarget(@enumFromInt(10)),
    };
    entries[2] = .{
        .key = .{ .owner = .{ .builtin = .box }, .method = @enumFromInt(1) },
        .target = testMethodTarget(@enumFromInt(15)),
    };

    finalizeMethodRegistryEntries(entries);

    var registry = MethodRegistry{ .entries = entries };
    const found = registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @enumFromInt(1) }) orelse return error.MissingSortedMethodTarget;
    try std.testing.expectEqual(@as(CIR.Def.Idx, @enumFromInt(15)), found.def_idx);
    try std.testing.expect(registry.lookup(.{ .owner = .{ .builtin = .list }, .method = @enumFromInt(2) }) == null);
}

fn testMethodTarget(def_idx: CIR.Def.Idx) MethodTarget {
    return .{
        .module_idx = 0,
        .def_idx = def_idx,
        .kind = .{
            .local_proc = .{
                .binder = undefined, // The lookup test only asserts def_idx; target kind is never read.
                .expr = undefined, // The lookup test only asserts def_idx; target kind is never read.
            },
        },
        .callable_ty = undefined, // The lookup test only asserts def_idx; callable type is never read.
    };
}
