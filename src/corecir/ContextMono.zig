//! Exact contextual monotypes for normalized source IR.
//!
//! This stage owns all source-level monotype determination and all language
//! defaulting. No later phase may default unresolved source-level types.
//!
//! Source-of-truth rule:
//! `ContextMono` records monotypes only from already-inferred checker types.
//! This stage must never synthesize or "recover" monotypes from declared
//! signatures, annotations, lambda parameter counts, call argument shapes,
//! builtin-specific shortcuts, or any other CIR structure. Later stages must
//! consume these recorded inferred-type results directly; they are not allowed
//! to invent replacement monotypes when a fact is missing.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const Monotype = @import("Monotype.zig");

const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

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
pub const SourceContextKind = enum(u2) { callable_inst, root_expr, provenance_expr, template_expr };

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
    source_context_kind: SourceContextKind,
    source_context_module_idx: u32,
    source_context_raw: u32,
    module_idx: u32,
    expr_raw: u32,
};

pub const ContextPatternKey = struct {
    source_context_kind: SourceContextKind,
    source_context_module_idx: u32,
    source_context_raw: u32,
    module_idx: u32,
    pattern_raw: u32,
};

pub const ContextTypeVarKey = struct {
    source_context_kind: SourceContextKind,
    source_context_module_idx: u32,
    source_context_raw: u32,
    module_idx: u32,
    type_var: types.Var,
};

pub const Result = struct {
    monotype_store: Monotype.Store,
    subst_entries: std.ArrayListUnmanaged(TypeSubstEntry),
    substs: std.ArrayListUnmanaged(TypeSubst),
    empty_subst_id: TypeSubstId,
    context_expr_monotypes: std.AutoHashMapUnmanaged(ContextExprKey, ResolvedMonotype),
    context_pattern_monotypes: std.AutoHashMapUnmanaged(ContextPatternKey, ResolvedMonotype),
    context_type_var_monotypes: std.AutoHashMapUnmanaged(ContextTypeVarKey, ResolvedMonotype),
    type_scope_monotypes: std.AutoHashMapUnmanaged(BoundTypeVarKey, ResolvedMonotype),

    pub fn init(allocator: Allocator) !Result {
        var result: Result = .{
            .monotype_store = try Monotype.Store.init(allocator),
            .subst_entries = .empty,
            .substs = .empty,
            .empty_subst_id = @enumFromInt(0),
            .context_expr_monotypes = .empty,
            .context_pattern_monotypes = .empty,
            .context_type_var_monotypes = .empty,
            .type_scope_monotypes = .empty,
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
        self.context_type_var_monotypes.deinit(allocator);
        self.type_scope_monotypes.deinit(allocator);
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

    pub fn contextTypeVarKey(source_context: SourceContext, module_idx: u32, type_var: types.Var) ContextTypeVarKey {
        return switch (source_context) {
            .callable_inst => |context_id| .{
                .source_context_kind = .callable_inst,
                .source_context_module_idx = std.math.maxInt(u32),
                .source_context_raw = @intFromEnum(context_id),
                .module_idx = module_idx,
                .type_var = type_var,
            },
            .root_expr => |root| .{
                .source_context_kind = .root_expr,
                .source_context_module_idx = root.module_idx,
                .source_context_raw = @intFromEnum(root.expr_idx),
                .module_idx = module_idx,
                .type_var = type_var,
            },
            .provenance_expr => |source| .{
                .source_context_kind = .provenance_expr,
                .source_context_module_idx = source.module_idx,
                .source_context_raw = @intFromEnum(source.expr_idx),
                .module_idx = module_idx,
                .type_var = type_var,
            },
            .template_expr => |template| .{
                .source_context_kind = .template_expr,
                .source_context_module_idx = template.module_idx,
                .source_context_raw = @intFromEnum(template.expr_idx),
                .module_idx = module_idx,
                .type_var = type_var,
            },
        };
    }

    pub fn getContextTypeVarMonotype(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        type_var: types.Var,
    ) ?ResolvedMonotype {
        return self.context_type_var_monotypes.get(contextTypeVarKey(source_context, module_idx, type_var));
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

    pub fn monotypesStructurallyEqual(
        self: *const Result,
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        lhs: Monotype.Idx,
        rhs: Monotype.Idx,
    ) Allocator.Error!bool {
        if (lhs == rhs) return true;

        var seen = std.AutoHashMap(u64, void).init(allocator);
        defer seen.deinit();

        return self.monotypesStructurallyEqualRec(all_module_envs, lhs, rhs, &seen);
    }

    pub fn monotypesStructurallyEqualAcrossModules(
        self: *const Result,
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        lhs: Monotype.Idx,
        lhs_module_idx: u32,
        rhs: Monotype.Idx,
        rhs_module_idx: u32,
    ) Allocator.Error!bool {
        var seen = std.AutoHashMap(u64, void).init(allocator);
        defer seen.deinit();

        return self.monotypesStructurallyEqualAcrossModulesRec(
            allocator,
            all_module_envs,
            lhs,
            lhs_module_idx,
            rhs,
            rhs_module_idx,
            &seen,
        );
    }

    fn monotypesStructurallyEqualRec(
        self: *const Result,
        all_module_envs: []const *ModuleEnv,
        lhs: Monotype.Idx,
        rhs: Monotype.Idx,
        seen: *std.AutoHashMap(u64, void),
    ) Allocator.Error!bool {
        if (lhs == rhs) return true;

        const lhs_u32: u32 = @intFromEnum(lhs);
        const rhs_u32: u32 = @intFromEnum(rhs);
        const key: u64 = (@as(u64, lhs_u32) << 32) | @as(u64, rhs_u32);

        if (seen.contains(key)) return true;
        try seen.put(key, {});

        const lhs_mono = self.monotype_store.getMonotype(lhs);
        const rhs_mono = self.monotype_store.getMonotype(rhs);
        if (std.meta.activeTag(lhs_mono) != std.meta.activeTag(rhs_mono)) return false;

        return switch (lhs_mono) {
            .recursive_placeholder => unreachable,
            .unit => true,
            .prim => |lhs_prim| lhs_prim == rhs_mono.prim,
            .list => |lhs_list| try self.monotypesStructurallyEqualRec(all_module_envs, lhs_list.elem, rhs_mono.list.elem, seen),
            .box => |lhs_box| try self.monotypesStructurallyEqualRec(all_module_envs, lhs_box.inner, rhs_mono.box.inner, seen),
            .tuple => |lhs_tuple| blk: {
                const lhs_elems = self.monotype_store.getIdxSpan(lhs_tuple.elems);
                const rhs_elems = self.monotype_store.getIdxSpan(rhs_mono.tuple.elems);
                if (lhs_elems.len != rhs_elems.len) break :blk false;
                for (lhs_elems, rhs_elems) |lhs_elem, rhs_elem| {
                    if (!try self.monotypesStructurallyEqualRec(all_module_envs, lhs_elem, rhs_elem, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .func => |lhs_func| blk: {
                const rhs_func = rhs_mono.func;
                const lhs_args = self.monotype_store.getIdxSpan(lhs_func.args);
                const rhs_args = self.monotype_store.getIdxSpan(rhs_func.args);
                if (lhs_func.effectful != rhs_func.effectful) break :blk false;
                if (lhs_args.len != rhs_args.len) break :blk false;
                for (lhs_args, rhs_args) |lhs_arg, rhs_arg| {
                    if (!try self.monotypesStructurallyEqualRec(all_module_envs, lhs_arg, rhs_arg, seen)) {
                        break :blk false;
                    }
                }
                break :blk try self.monotypesStructurallyEqualRec(all_module_envs, lhs_func.ret, rhs_func.ret, seen);
            },
            .record => |lhs_record| blk: {
                const lhs_fields = self.monotype_store.getFields(lhs_record.fields);
                const rhs_fields = self.monotype_store.getFields(rhs_mono.record.fields);
                if (lhs_fields.len != rhs_fields.len) break :blk false;
                for (lhs_fields, rhs_fields) |lhs_field, rhs_field| {
                    if (!identsStructurallyEqualAcrossModules(
                        all_module_envs,
                        lhs_field.name.module_idx,
                        lhs_field.name,
                        rhs_field.name.module_idx,
                        rhs_field.name,
                    )) break :blk false;
                    if (!try self.monotypesStructurallyEqualRec(all_module_envs, lhs_field.type_idx, rhs_field.type_idx, seen)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tag_union => |lhs_union| blk: {
                const lhs_tags = self.monotype_store.getTags(lhs_union.tags);
                const rhs_tags = self.monotype_store.getTags(rhs_mono.tag_union.tags);
                if (lhs_tags.len != rhs_tags.len) break :blk false;
                for (lhs_tags, rhs_tags) |lhs_tag, rhs_tag| {
                    const lhs_payloads = self.monotype_store.getIdxSpan(lhs_tag.payloads);
                    const rhs_payloads = self.monotype_store.getIdxSpan(rhs_tag.payloads);
                    if (!identsStructurallyEqualAcrossModules(
                        all_module_envs,
                        lhs_tag.name.module_idx,
                        lhs_tag.name,
                        rhs_tag.name.module_idx,
                        rhs_tag.name,
                    )) break :blk false;
                    if (lhs_payloads.len != rhs_payloads.len) break :blk false;
                    for (lhs_payloads, rhs_payloads) |lhs_payload, rhs_payload| {
                        if (!try self.monotypesStructurallyEqualRec(all_module_envs, lhs_payload, rhs_payload, seen)) {
                            break :blk false;
                        }
                    }
                }
                break :blk true;
            },
        };
    }

    fn monotypesStructurallyEqualAcrossModulesRec(
        self: *const Result,
        allocator: Allocator,
        all_module_envs: []const *ModuleEnv,
        lhs: Monotype.Idx,
        lhs_module_idx: u32,
        rhs: Monotype.Idx,
        rhs_module_idx: u32,
        seen: *std.AutoHashMap(u64, void),
    ) Allocator.Error!bool {
        const lhs_u32: u32 = @intFromEnum(lhs);
        const rhs_u32: u32 = @intFromEnum(rhs);
        const key: u64 = (@as(u64, lhs_u32) << 32) | @as(u64, rhs_u32);
        if (seen.contains(key)) return true;
        try seen.put(key, {});

        const lhs_mono = self.monotype_store.getMonotype(lhs);
        const rhs_mono = self.monotype_store.getMonotype(rhs);
        if (std.meta.activeTag(lhs_mono) != std.meta.activeTag(rhs_mono)) return false;

        return switch (lhs_mono) {
            .recursive_placeholder => unreachable,
            .unit => true,
            .prim => |lhs_prim| lhs_prim == rhs_mono.prim,
            .list => |lhs_list| try self.monotypesStructurallyEqualAcrossModulesRec(
                allocator,
                all_module_envs,
                lhs_list.elem,
                lhs_module_idx,
                rhs_mono.list.elem,
                rhs_module_idx,
                seen,
            ),
            .box => |lhs_box| try self.monotypesStructurallyEqualAcrossModulesRec(
                allocator,
                all_module_envs,
                lhs_box.inner,
                lhs_module_idx,
                rhs_mono.box.inner,
                rhs_module_idx,
                seen,
            ),
            .tuple => |lhs_tuple| blk: {
                const lhs_elems = self.monotype_store.getIdxSpan(lhs_tuple.elems);
                const rhs_elems = self.monotype_store.getIdxSpan(rhs_mono.tuple.elems);
                if (lhs_elems.len != rhs_elems.len) break :blk false;
                for (lhs_elems, rhs_elems) |lhs_elem, rhs_elem| {
                    if (!try self.monotypesStructurallyEqualAcrossModulesRec(
                        allocator,
                        all_module_envs,
                        lhs_elem,
                        lhs_module_idx,
                        rhs_elem,
                        rhs_module_idx,
                        seen,
                    )) break :blk false;
                }
                break :blk true;
            },
            .func => |lhs_func| blk: {
                const rhs_func = rhs_mono.func;
                const lhs_args = self.monotype_store.getIdxSpan(lhs_func.args);
                const rhs_args = self.monotype_store.getIdxSpan(rhs_func.args);
                if (lhs_func.effectful != rhs_func.effectful) break :blk false;
                if (lhs_args.len != rhs_args.len) break :blk false;
                for (lhs_args, rhs_args) |lhs_arg, rhs_arg| {
                    if (!try self.monotypesStructurallyEqualAcrossModulesRec(
                        allocator,
                        all_module_envs,
                        lhs_arg,
                        lhs_module_idx,
                        rhs_arg,
                        rhs_module_idx,
                        seen,
                    )) break :blk false;
                }
                break :blk try self.monotypesStructurallyEqualAcrossModulesRec(
                    allocator,
                    all_module_envs,
                    lhs_func.ret,
                    lhs_module_idx,
                    rhs_func.ret,
                    rhs_module_idx,
                    seen,
                );
            },
            .record => |lhs_record| blk: {
                const lhs_fields = self.monotype_store.getFields(lhs_record.fields);
                const rhs_fields = self.monotype_store.getFields(rhs_mono.record.fields);
                if (lhs_fields.len != rhs_fields.len) break :blk false;

                var rhs_used = std.ArrayListUnmanaged(bool){};
                defer rhs_used.deinit(allocator);
                try rhs_used.resize(allocator, rhs_fields.len);
                @memset(rhs_used.items, false);

                for (lhs_fields) |lhs_field| {
                    var matched = false;
                    for (rhs_fields, 0..) |rhs_field, rhs_i| {
                        if (rhs_used.items[rhs_i]) continue;
                        if (!identsStructurallyEqualAcrossModules(
                            all_module_envs,
                            lhs_module_idx,
                            lhs_field.name,
                            rhs_module_idx,
                            rhs_field.name,
                        )) continue;
                        if (!try self.monotypesStructurallyEqualAcrossModulesRec(
                            allocator,
                            all_module_envs,
                            lhs_field.type_idx,
                            lhs_module_idx,
                            rhs_field.type_idx,
                            rhs_module_idx,
                            seen,
                        )) break :blk false;
                        rhs_used.items[rhs_i] = true;
                        matched = true;
                        break;
                    }
                    if (!matched) break :blk false;
                }
                break :blk true;
            },
            .tag_union => |lhs_union| blk: {
                const lhs_tags = self.monotype_store.getTags(lhs_union.tags);
                const rhs_tags = self.monotype_store.getTags(rhs_mono.tag_union.tags);
                if (lhs_tags.len != rhs_tags.len) break :blk false;

                var rhs_used = std.ArrayListUnmanaged(bool){};
                defer rhs_used.deinit(allocator);
                try rhs_used.resize(allocator, rhs_tags.len);
                @memset(rhs_used.items, false);

                for (lhs_tags) |lhs_tag| {
                    var matched = false;
                    for (rhs_tags, 0..) |rhs_tag, rhs_i| {
                        if (rhs_used.items[rhs_i]) continue;
                        if (!identsStructurallyEqualAcrossModules(
                            all_module_envs,
                            lhs_module_idx,
                            lhs_tag.name,
                            rhs_module_idx,
                            rhs_tag.name,
                        )) continue;

                        const lhs_payloads = self.monotype_store.getIdxSpan(lhs_tag.payloads);
                        const rhs_payloads = self.monotype_store.getIdxSpan(rhs_tag.payloads);
                        if (lhs_payloads.len != rhs_payloads.len) continue;

                        var payloads_equal = true;
                        for (lhs_payloads, rhs_payloads) |lhs_payload, rhs_payload| {
                            if (!try self.monotypesStructurallyEqualAcrossModulesRec(
                                allocator,
                                all_module_envs,
                                lhs_payload,
                                lhs_module_idx,
                                rhs_payload,
                                rhs_module_idx,
                                seen,
                            )) {
                                payloads_equal = false;
                                break;
                            }
                        }
                        if (!payloads_equal) continue;

                        rhs_used.items[rhs_i] = true;
                        matched = true;
                        break;
                    }
                    if (!matched) break :blk false;
                }
                break :blk true;
            },
        };
    }
};

pub fn resolvedMonotype(idx: Monotype.Idx, module_idx: u32) ResolvedMonotype {
    return .{ .idx = idx, .module_idx = module_idx };
}

pub fn resolveFuncTypeInStore(types_store: *const types.Store, type_var: types.Var) ?struct { func: types.Func, effectful: bool } {
    var resolved = types_store.resolveVar(type_var);
    while (resolved.desc.content == .alias) {
        resolved = types_store.resolveVar(types_store.getAliasBackingVar(resolved.desc.content.alias));
    }

    if (resolved.desc.content != .structure) return null;
    return switch (resolved.desc.content.structure) {
        .fn_pure => |func| .{ .func = func, .effectful = false },
        .fn_effectful => |func| .{ .func = func, .effectful = true },
        .fn_unbound => |func| .{ .func = func, .effectful = false },
        else => null,
    };
}

pub fn resolveNominalTypeInStore(types_store: *const types.Store, type_var: types.Var) ?types.NominalType {
    var resolved = types_store.resolveVar(type_var);
    while (resolved.desc.content == .alias) {
        resolved = types_store.resolveVar(types_store.getAliasBackingVar(resolved.desc.content.alias));
    }

    if (resolved.desc.content != .structure) return null;
    return switch (resolved.desc.content.structure) {
        .nominal_type => |nominal| nominal,
        else => null,
    };
}

pub fn mergeResolvedMonotypeMap(
    driver: anytype,
    result: anytype,
    map: anytype,
    key: anytype,
    resolved: ResolvedMonotype,
    comptime label: []const u8,
) Allocator.Error!void {
    const gop = try map.getOrPut(driver.allocator, key);
    if (!gop.found_existing) {
        gop.value_ptr.* = resolved;
        return;
    }

    const existing = gop.value_ptr.*;
    if (try driver.monotypesStructurallyEqualAcrossModules(
        result,
        existing.idx,
        existing.module_idx,
        resolved.idx,
        resolved.module_idx,
    )) {
        return;
    }

    if (std.debug.runtime_safety) {
        std.debug.panic(
            "ContextMono: conflicting {s} monotypes while merging key={any} existing={d}@{d} existing_mono={any} new={d}@{d} new_mono={any}",
            .{
                label,
                key,
                @intFromEnum(existing.idx),
                existing.module_idx,
                result.context_mono.monotype_store.getMonotype(existing.idx),
                @intFromEnum(resolved.idx),
                resolved.module_idx,
                result.context_mono.monotype_store.getMonotype(resolved.idx),
            },
        );
    }
    unreachable;
}

pub fn recordTypeVarMonotypeForSourceContext(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    type_var: types.Var,
    monotype: Monotype.Idx,
    monotype_module_idx: u32,
) Allocator.Error!void {
    if (monotype.isNone()) return;
    const resolved_var = driver.all_module_envs[module_idx].types.resolveVar(type_var).var_;
    const key = Result.contextTypeVarKey(source_context, module_idx, resolved_var);
    try mergeResolvedMonotypeMap(
        driver,
        result,
        &result.context_mono.context_type_var_monotypes,
        key,
        resolvedMonotype(monotype, monotype_module_idx),
        "exact typevar",
    );
}

pub fn mergeContextExprMonotype(
    driver: anytype,
    result: anytype,
    key: ContextExprKey,
    resolved: ResolvedMonotype,
) Allocator.Error!void {
    return mergeResolvedMonotypeMap(
        driver,
        result,
        &result.context_mono.context_expr_monotypes,
        key,
        resolved,
        "exact expr",
    );
}

pub fn mergeContextPatternMonotype(
    driver: anytype,
    result: anytype,
    key: ContextPatternKey,
    resolved: ResolvedMonotype,
) Allocator.Error!void {
    return mergeResolvedMonotypeMap(
        driver,
        result,
        &result.context_mono.context_pattern_monotypes,
        key,
        resolved,
        "exact pattern",
    );
}

pub fn mergeContextTypeVarMonotype(
    driver: anytype,
    result: anytype,
    key: ContextTypeVarKey,
    resolved: ResolvedMonotype,
) Allocator.Error!void {
    return mergeResolvedMonotypeMap(
        driver,
        result,
        &result.context_mono.context_type_var_monotypes,
        key,
        resolved,
        "exact typevar",
    );
}

pub fn recordTypeVarMonotypeForThread(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    type_var: types.Var,
    monotype: Monotype.Idx,
    monotype_module_idx: u32,
) Allocator.Error!void {
    return recordTypeVarMonotypeForSourceContext(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        type_var,
        monotype,
        monotype_module_idx,
    );
}

pub fn recordExprMonotypeResolved(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    monotype: Monotype.Idx,
    monotype_module_idx: u32,
) Allocator.Error!void {
    if (monotype.isNone()) return;
    const key = Result.contextExprKey(source_context, module_idx, expr_idx);
    const resolved = resolvedMonotype(monotype, monotype_module_idx);
    if (result.lambdamono.getExprId(source_context, module_idx, expr_idx)) |expr_id| {
        const program_expr = &result.lambdamono.exprs.items[@intFromEnum(expr_id)];
        const existing_program_mono = program_expr.monotype;
        if (!try driver.monotypesStructurallyEqualAcrossModules(
            result,
            existing_program_mono.idx,
            existing_program_mono.module_idx,
            resolved.idx,
            resolved.module_idx,
        )) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "ContextMono invariant violated: finalized expr monotype disagreed with recorded exact monotype for ctx={s} module={d} expr={d}",
                    .{ @tagName(source_context), module_idx, @intFromEnum(expr_idx) },
                );
            }
            unreachable;
        }
    }
    try recordTypeVarMonotypeForSourceContext(
        driver,
        result,
        source_context,
        module_idx,
        ModuleEnv.varFrom(expr_idx),
        monotype,
        monotype_module_idx,
    );
    if (result.context_mono.context_expr_monotypes.get(key)) |existing| {
        if (try driver.monotypesStructurallyEqualAcrossModules(
            result,
            existing.idx,
            existing.module_idx,
            resolved.idx,
            resolved.module_idx,
        )) {
            return;
        }

        if (std.debug.runtime_safety) {
            const module_env = driver.all_module_envs[module_idx];
            const expr = module_env.store.getExpr(expr_idx);
            const expr_region = module_env.store.getExprRegion(expr_idx);
            const source = module_env.getSourceAll();
            const snippet_start = @min(expr_region.start.offset, source.len);
            const snippet_end = @min(expr_region.end.offset, source.len);
            const context_template = result.getSourceContextTemplateId(source_context);
            std.debug.panic(
                "ContextMono: conflicting exact expr monotypes for ctx={s} module={d} module_name={s} expr={d} kind={s} region={any} snippet=\"{s}\" existing={d}@{d} existing_mono={any} new={d}@{d} new_mono={any} template_expr={d}",
                .{
                    @tagName(source_context),
                    module_idx,
                    module_env.module_name,
                    @intFromEnum(expr_idx),
                    @tagName(expr),
                    expr_region,
                    source[snippet_start..snippet_end],
                    @intFromEnum(existing.idx),
                    existing.module_idx,
                    result.context_mono.monotype_store.getMonotype(existing.idx),
                    @intFromEnum(resolved.idx),
                    resolved.module_idx,
                    result.context_mono.monotype_store.getMonotype(resolved.idx),
                    if (context_template) |template_id| @intFromEnum(result.getCallableTemplate(template_id).cir_expr) else std.math.maxInt(u32),
                },
            );
        }
        unreachable;
    }
    try mergeResolvedMonotypeMap(
        driver,
        result,
        &result.context_mono.context_expr_monotypes,
        key,
        resolved,
        "exact expr",
    );
}

pub fn recordExprMonotypeForSourceContext(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    monotype: Monotype.Idx,
    monotype_module_idx: u32,
) Allocator.Error!void {
    return recordExprMonotypeResolved(
        driver,
        result,
        source_context,
        module_idx,
        expr_idx,
        monotype,
        monotype_module_idx,
    );
}

pub fn recordExprMonotypeForThread(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
    monotype: Monotype.Idx,
    monotype_module_idx: u32,
) Allocator.Error!void {
    return recordExprMonotypeResolved(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
        monotype,
        monotype_module_idx,
    );
}

pub fn lookupRecordedExprMonotypeIfReadyForSourceContext(
    driver: anytype,
    result: anytype,
    thread: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!?ResolvedMonotype {
    if (lookupExprMonotypeForSourceContext(result, source_context, module_idx, expr_idx)) |resolved| {
        return resolved;
    }
    const typevar_resolved = try resolveTypeVarMonotypeResolved(
        driver,
        result,
        thread,
        module_idx,
        ModuleEnv.varFrom(expr_idx),
    );
    if (!typevar_resolved.isNone()) {
        try recordExprMonotypeResolved(
            driver,
            result,
            source_context,
            module_idx,
            expr_idx,
            typevar_resolved.idx,
            typevar_resolved.module_idx,
        );
        return typevar_resolved;
    }
    return null;
}

pub fn requireRecordedExprMonotypeForSourceContext(
    driver: anytype,
    result: anytype,
    thread: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!ResolvedMonotype {
    if (try lookupRecordedExprMonotypeIfReadyForSourceContext(
        driver,
        result,
        thread,
        source_context,
        module_idx,
        expr_idx,
    )) |resolved| {
        return resolved;
    }

    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    const expr_region = module_env.store.getExprRegion(expr_idx);
    const source = module_env.getSourceAll();
    const snippet_start = @min(expr_region.start.offset, source.len);
    const snippet_end = @min(expr_region.end.offset, source.len);
    const origin = result.getExprOriginExpr(source_context, module_idx, expr_idx);
    std.debug.panic(
        "ContextMono invariant violated: missing exact contextual monotype for expr {d} kind={s} region={any} snippet=\"{s}\" in module {d} under source context {s} origin_ctx={s} origin_module={d} origin_expr={d}",
        .{
            @intFromEnum(expr_idx),
            @tagName(expr),
            expr_region,
            source[snippet_start..snippet_end],
            module_idx,
            @tagName(source_context),
            if (origin) |expr_ref| @tagName(expr_ref.source_context) else "none",
            if (origin) |expr_ref| expr_ref.module_idx else std.math.maxInt(u32),
            if (origin) |expr_ref| @intFromEnum(expr_ref.expr_idx) else std.math.maxInt(u32),
        },
    );
}

pub fn lookupExprMonotypeForThread(
    result: anytype,
    thread: anytype,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?ResolvedMonotype {
    return lookupExprMonotypeForSourceContext(
        result,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
    );
}

pub fn lookupExprMonotypeForSourceContext(
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) ?ResolvedMonotype {
    return result.context_mono.getExprMonotype(source_context, module_idx, expr_idx);
}

pub fn resolvedIfFunctionMonotype(
    result: anytype,
    resolved: ?ResolvedMonotype,
) ?ResolvedMonotype {
    const resolved_mono = resolved orelse return null;
    return switch (result.context_mono.monotype_store.getMonotype(resolved_mono.idx)) {
        .func => resolved_mono,
        else => null,
    };
}

pub fn seedRecordedContextTypeVarSpecializations(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    specializations: *std.AutoHashMap(types.Var, Monotype.Idx),
) Allocator.Error!void {
    switch (source_context) {
        .callable_inst => |context_id| {
            const callable_inst = result.getCallableInst(@enumFromInt(@intFromEnum(context_id)));
            const subst = result.getTypeSubst(callable_inst.subst);
            for (result.getTypeSubstEntries(subst.entries)) |entry| {
                if (entry.key.module_idx != module_idx) continue;
                const resolved_var = driver.all_module_envs[module_idx].types.resolveVar(entry.key.type_var).var_;
                const canonical_mono = if (entry.monotype.module_idx == module_idx)
                    entry.monotype.idx
                else
                    try driver.remapMonotypeBetweenModules(
                        result,
                        entry.monotype.idx,
                        entry.monotype.module_idx,
                        module_idx,
                    );
                try specializations.put(resolved_var, canonical_mono);
            }
        },
        .root_expr, .provenance_expr, .template_expr => {},
    }

    var it = result.context_mono.context_type_var_monotypes.iterator();
    while (it.next()) |entry| {
        const key = entry.key_ptr.*;
        if (key.module_idx != module_idx) continue;
        if (!driver.contextTypeVarKeyMatchesSourceContext(source_context, key)) continue;
        if (entry.value_ptr.module_idx != module_idx) continue;
        try specializations.put(key.type_var, entry.value_ptr.idx);
    }
}

pub fn monotypeFromTypeVarInSourceContext(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    store_types: *const types.Store,
    var_: types.Var,
) Allocator.Error!Monotype.Idx {
    var extra_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(driver.allocator);
    defer extra_bindings.deinit();
    return monotypeFromTypeVarInSourceContextWithExtraBindings(
        driver,
        result,
        source_context,
        module_idx,
        store_types,
        var_,
        &extra_bindings,
    );
}

pub fn monotypeFromTypeVarInSourceContextWithExtraBindings(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    store_types: *const types.Store,
    var_: types.Var,
    extra_bindings: *const std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
) Allocator.Error!Monotype.Idx {
    var exact_specializations = std.AutoHashMap(types.Var, Monotype.Idx).init(driver.allocator);
    defer exact_specializations.deinit();

    try seedRecordedContextTypeVarSpecializations(driver, result, source_context, module_idx, &exact_specializations);
    try driver.seedRecordedTypeScopeSpecializations(result, module_idx, &exact_specializations);

    var extra_it = extra_bindings.iterator();
    while (extra_it.next()) |entry| {
        if (entry.key_ptr.module_idx != module_idx) continue;
        const canonical_mono = if (entry.value_ptr.module_idx == module_idx)
            entry.value_ptr.idx
        else
            try driver.remapMonotypeBetweenModules(
                result,
                entry.value_ptr.idx,
                entry.value_ptr.module_idx,
                module_idx,
            );
        try exact_specializations.put(entry.key_ptr.type_var, canonical_mono);
    }

    var nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(driver.allocator);
    defer nominal_cycle_breakers.deinit();

    var scratches = try Monotype.Store.Scratches.init(driver.allocator);
    defer scratches.deinit();

    const module_env = driver.all_module_envs[module_idx];
    scratches.ident_store = module_env.getIdentStoreConst();
    scratches.module_env = module_env;
    scratches.module_idx = module_idx;
    scratches.all_module_envs = driver.all_module_envs;
    return result.context_mono.monotype_store.fromTypeVarExact(
        driver.allocator,
        store_types,
        var_,
        module_env.idents,
        &exact_specializations,
        &nominal_cycle_breakers,
        &scratches,
    );
}

pub fn monotypeFromTypeVarWithLanguageDefaultingInSourceContext(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    store_types: *const types.Store,
    var_: types.Var,
) Allocator.Error!Monotype.Idx {
    var extra_bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(driver.allocator);
    defer extra_bindings.deinit();
    return monotypeFromTypeVarWithLanguageDefaultingInSourceContextWithExtraBindings(
        driver,
        result,
        source_context,
        module_idx,
        store_types,
        var_,
        &extra_bindings,
    );
}

pub fn monotypeFromTypeVarWithLanguageDefaultingInSourceContextWithExtraBindings(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    store_types: *const types.Store,
    var_: types.Var,
    extra_bindings: *const std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
) Allocator.Error!Monotype.Idx {
    var exact_specializations = std.AutoHashMap(types.Var, Monotype.Idx).init(driver.allocator);
    defer exact_specializations.deinit();

    try seedRecordedContextTypeVarSpecializations(driver, result, source_context, module_idx, &exact_specializations);
    try driver.seedRecordedTypeScopeSpecializations(result, module_idx, &exact_specializations);

    var extra_it = extra_bindings.iterator();
    while (extra_it.next()) |entry| {
        if (entry.key_ptr.module_idx != module_idx) continue;
        const canonical_mono = if (entry.value_ptr.module_idx == module_idx)
            entry.value_ptr.idx
        else
            try driver.remapMonotypeBetweenModules(
                result,
                entry.value_ptr.idx,
                entry.value_ptr.module_idx,
                module_idx,
            );
        try exact_specializations.put(entry.key_ptr.type_var, canonical_mono);
    }

    var nominal_cycle_breakers = std.AutoHashMap(types.Var, Monotype.Idx).init(driver.allocator);
    defer nominal_cycle_breakers.deinit();

    var scratches = try Monotype.Store.Scratches.init(driver.allocator);
    defer scratches.deinit();

    const module_env = driver.all_module_envs[module_idx];
    scratches.ident_store = module_env.getIdentStoreConst();
    scratches.module_env = module_env;
    scratches.module_idx = module_idx;
    scratches.all_module_envs = driver.all_module_envs;
    return result.context_mono.monotype_store.fromTypeVar(
        driver.allocator,
        store_types,
        var_,
        module_env.idents,
        &exact_specializations,
        &nominal_cycle_breakers,
        &scratches,
    );
}

pub fn resolveTypeVarMonotypeResolved(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    var_: types.Var,
) Allocator.Error!ResolvedMonotype {
    const module_env = driver.all_module_envs[module_idx];
    const resolved_var = module_env.types.resolveVar(var_).var_;
    if (lookupContextTypeVarMonotype(driver, result, thread.requireSourceContext(), module_idx, resolved_var)) |mono| {
        return mono;
    }
    const mono = try monotypeFromTypeVarInSourceContext(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        &module_env.types,
        resolved_var,
    );
    if (!mono.isNone()) {
        try recordTypeVarMonotypeForThread(
            driver,
            result,
            thread,
            module_idx,
            resolved_var,
            mono,
            module_idx,
        );
        return resolvedMonotype(mono, module_idx);
    }
    const defaulted = try monotypeFromTypeVarWithLanguageDefaultingInSourceContext(
        driver,
        result,
        thread.requireSourceContext(),
        module_idx,
        &module_env.types,
        resolved_var,
    );
    if (!defaulted.isNone()) {
        try recordTypeVarMonotypeForThread(
            driver,
            result,
            thread,
            module_idx,
            resolved_var,
            defaulted,
            module_idx,
        );
    }
    return resolvedMonotype(defaulted, module_idx);
}

pub fn resolveTypeVarMonotype(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    var_: types.Var,
) Allocator.Error!Monotype.Idx {
    return (try resolveTypeVarMonotypeResolved(driver, result, thread, module_idx, var_)).idx;
}

pub fn resolveExprMonotypeResolved(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!?ResolvedMonotype {
    const source_context = thread.requireSourceContext();
    const module_env = driver.all_module_envs[module_idx];
    const expr = module_env.store.getExpr(expr_idx);
    const expr_var = ModuleEnv.varFrom(expr_idx);
    if (lookupExprMonotypeForSourceContext(result, source_context, module_idx, expr_idx)) |resolved| {
        return resolved;
    }
    const typevar_resolved = try resolveTypeVarMonotypeResolved(
        driver,
        result,
        thread,
        module_idx,
        expr_var,
    );
    if (!typevar_resolved.isNone()) {
        try recordExprMonotypeResolved(
            driver,
            result,
            source_context,
            module_idx,
            expr_idx,
            typevar_resolved.idx,
            typevar_resolved.module_idx,
        );
        return typevar_resolved;
    }

    if (std.debug.runtime_safety) {
        switch (expr) {
            .e_tag, .e_empty_list, .e_lambda, .e_closure, .e_hosted_lambda => {
                const resolved_var = module_env.types.resolveVar(ModuleEnv.varFrom(expr_idx));
                std.debug.print(
                    "[contextmono] resolveExprMonotypeResolved none expr={d} kind={s} ctx={s} typevar_content={s}\n",
                    .{
                        @intFromEnum(expr_idx),
                        @tagName(expr),
                        @tagName(source_context),
                        @tagName(resolved_var.desc.content),
                    },
                );
            },
            else => {},
        }
    }

    return try lookupRecordedExprMonotypeIfReadyForSourceContext(
        driver,
        result,
        thread,
        source_context,
        module_idx,
        expr_idx,
    );
}

pub fn resolveExprMonotype(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!?Monotype.Idx {
    return if (try resolveExprMonotypeResolved(driver, result, thread, module_idx, expr_idx)) |resolved|
        resolved.idx
    else
        null;
}

pub fn resolvePatternMonotypeResolved(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    pattern_idx: CIR.Pattern.Idx,
) Allocator.Error!ResolvedMonotype {
    const source_context = thread.requireSourceContext();
    if (result.getContextPatternMonotype(source_context, module_idx, pattern_idx)) |resolved| {
        return resolved;
    }
    if (result.getContextPatternSourceExpr(source_context, module_idx, pattern_idx)) |source| {
        const SourceContextThread = struct {
            source_context: SourceContext,

            pub fn requireSourceContext(self: @This()) SourceContext {
                return self.source_context;
            }
        };

        var resolved = try requireRecordedExprMonotypeForSourceContext(
            driver,
            result,
            SourceContextThread{ .source_context = source.source_context },
            source.source_context,
            source.module_idx,
            source.expr_idx,
        );
        for (result.lambdamono.getValueProjectionEntries(source.projections)) |projection| {
            resolved = try driver.projectResolvedMonotypeByValueProjection(result, resolved, projection);
        }
        try mergeContextPatternMonotype(
            driver,
            result,
            Result.contextPatternKey(source_context, module_idx, pattern_idx),
            resolved,
        );
        return resolved;
    }
    if (lookupContextTypeVarMonotype(
        driver,
        result,
        source_context,
        module_idx,
        ModuleEnv.varFrom(pattern_idx),
    )) |resolved| {
        try mergeContextPatternMonotype(
            driver,
            result,
            Result.contextPatternKey(source_context, module_idx, pattern_idx),
            resolved,
        );
        return resolved;
    }
    const resolved = try resolveTypeVarMonotypeResolved(
        driver,
        result,
        thread,
        module_idx,
        ModuleEnv.varFrom(pattern_idx),
    );
    if (!resolved.isNone()) {
        try mergeContextPatternMonotype(
            driver,
            result,
            Result.contextPatternKey(source_context, module_idx, pattern_idx),
            resolved,
        );
    }
    return resolved;
}

pub fn lookupContextTypeVarMonotype(
    driver: anytype,
    result: anytype,
    source_context: SourceContext,
    module_idx: u32,
    var_: types.Var,
) ?ResolvedMonotype {
    const resolved_var = driver.all_module_envs[module_idx].types.resolveVar(var_).var_;
    return result.context_mono.getContextTypeVarMonotype(source_context, module_idx, resolved_var) orelse
        result.context_mono.getTypeScopeMonotype(module_idx, resolved_var);
}

pub fn monotypesStructurallyEqual(
    driver: anytype,
    result: anytype,
    lhs: Monotype.Idx,
    rhs: Monotype.Idx,
) Allocator.Error!bool {
    return result.context_mono.monotypesStructurallyEqual(
        driver.allocator,
        driver.all_module_envs,
        lhs,
        rhs,
    );
}

pub fn monotypesStructurallyEqualAcrossModules(
    driver: anytype,
    result: anytype,
    lhs: Monotype.Idx,
    lhs_module_idx: u32,
    rhs: Monotype.Idx,
    rhs_module_idx: u32,
) Allocator.Error!bool {
    return result.context_mono.monotypesStructurallyEqualAcrossModules(
        driver.allocator,
        driver.all_module_envs,
        lhs,
        lhs_module_idx,
        rhs,
        rhs_module_idx,
    );
}

pub fn labelTextAcrossModules(all_module_envs: []const *ModuleEnv, module_idx: u32, label: anytype) []const u8 {
    return switch (@TypeOf(label)) {
        base.Ident.Idx => all_module_envs[module_idx].getIdent(label),
        Monotype.Name => label.text(all_module_envs),
        else => @compileError("unsupported label type"),
    };
}

pub fn identsStructurallyEqualAcrossModules(
    all_module_envs: []const *ModuleEnv,
    lhs_module_idx: u32,
    lhs: anytype,
    rhs_module_idx: u32,
    rhs: anytype,
) bool {
    if (@TypeOf(lhs) == base.Ident.Idx and @TypeOf(rhs) == base.Ident.Idx and lhs_module_idx == rhs_module_idx and lhs == rhs) {
        return true;
    }
    if (@TypeOf(lhs) == Monotype.Name and @TypeOf(rhs) == Monotype.Name and lhs.eql(rhs)) {
        return true;
    }

    const lhs_text = labelTextAcrossModules(all_module_envs, lhs_module_idx, lhs);
    const rhs_text = labelTextAcrossModules(all_module_envs, rhs_module_idx, rhs);
    return std.mem.eql(u8, lhs_text, rhs_text);
}
