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
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const DispatchSolved = @import("DispatchSolved.zig");
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

fn requireSourceContextFromThreadLike(
    thread: anytype,
    module_idx: u32,
    var_: ?types.Var,
) SourceContext {
    return switch (@typeInfo(@TypeOf(thread))) {
        .optional => if (thread) |tracked_thread|
            tracked_thread.requireSourceContext()
        else {
            if (std.debug.runtime_safety) {
                if (var_) |resolved_var| {
                    std.debug.panic(
                        "ContextMono invariant violated: missing source context for module={d} var={d}",
                        .{ module_idx, @intFromEnum(resolved_var) },
                    );
                } else {
                    std.debug.panic(
                        "ContextMono invariant violated: missing source context for module={d}",
                        .{module_idx},
                    );
                }
            }
            unreachable;
        },
        else => thread.requireSourceContext(),
    };
}

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
    context_type_var_monotypes: std.AutoHashMapUnmanaged(ContextTypeVarKey, ResolvedMonotype),
    type_scope_monotypes: std.AutoHashMapUnmanaged(BoundTypeVarKey, ResolvedMonotype),

    pub fn init(allocator: Allocator) !Result {
        var result: Result = .{
            .monotype_store = try Monotype.Store.init(allocator),
            .subst_entries = .empty,
            .substs = .empty,
            .empty_subst_id = undefined,
            .context_expr_monotypes = .empty,
            .context_type_var_monotypes = .empty,
            .type_scope_monotypes = .empty,
        };
        try result.substs.append(allocator, .{ .entries = TypeSubstSpan.empty() });
        result.empty_subst_id = @enumFromInt(result.substs.items.len - 1);
        return result;
    }

    pub fn deinit(self: *Result, allocator: Allocator) void {
        self.monotype_store.deinit(allocator);
        self.subst_entries.deinit(allocator);
        self.substs.deinit(allocator);
        self.context_expr_monotypes.deinit(allocator);
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

    pub fn getExprMonotype(
        self: *const Result,
        source_context: SourceContext,
        module_idx: u32,
        expr_idx: CIR.Expr.Idx,
    ) ?ResolvedMonotype {
        return self.context_expr_monotypes.get(contextExprKey(source_context, module_idx, expr_idx));
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

    pub fn contextTypeVarKeyMatchesSourceContext(
        source_context: SourceContext,
        key: ContextTypeVarKey,
    ) bool {
        return switch (source_context) {
            .callable_inst => |context_id| key.source_context_kind == .callable_inst and
                key.source_context_module_idx == std.math.maxInt(u32) and
                key.source_context_raw == @intFromEnum(context_id),
            .root_expr => |root| key.source_context_kind == .root_expr and
                key.source_context_module_idx == root.module_idx and
                key.source_context_raw == @intFromEnum(root.expr_idx),
            .provenance_expr => |source| key.source_context_kind == .provenance_expr and
                key.source_context_module_idx == source.module_idx and
                key.source_context_raw == @intFromEnum(source.expr_idx),
            .template_expr => |template| key.source_context_kind == .template_expr and
                key.source_context_module_idx == template.module_idx and
                key.source_context_raw == @intFromEnum(template.expr_idx),
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
    const gop = try @constCast(map).getOrPut(driver.allocator, key);
    if (!gop.found_existing) {
        gop.value_ptr.* = resolved;
        return;
    }

    const existing = gop.value_ptr.*;
    if (try monotypesStructurallyEqualAcrossModules(
        driver,
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

pub fn recordTypeScopeMonotype(
    driver: anytype,
    result: anytype,
    module_idx: u32,
    type_var: types.Var,
    monotype: Monotype.Idx,
    monotype_module_idx: u32,
) Allocator.Error!void {
    if (monotype.isNone()) return;
    const resolved_var = driver.all_module_envs[module_idx].types.resolveVar(type_var).var_;
    const key: BoundTypeVarKey = .{
        .module_idx = module_idx,
        .type_var = resolved_var,
    };
    try mergeResolvedMonotypeMap(
        driver,
        result,
        &result.context_mono.type_scope_monotypes,
        key,
        resolvedMonotype(monotype, monotype_module_idx),
        "type-scope",
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
        requireSourceContextFromThreadLike(thread, module_idx, type_var),
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
        if (try monotypesStructurallyEqualAcrossModules(
            driver,
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

pub fn projectResolvedMonotypeByValueProjection(
    driver: anytype,
    result: anytype,
    source_monotype: ResolvedMonotype,
    projection: anytype,
) Allocator.Error!ResolvedMonotype {
    const mono = result.context_mono.monotype_store.getMonotype(source_monotype.idx);
    return switch (projection) {
        .field => |field_name| blk: {
            const record = switch (mono) {
                .record => |record| record,
                else => std.debug.panic(
                    "ContextMono invariant violated: record field projection expected record monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            for (result.context_mono.monotype_store.getFields(record.fields)) |field| {
                if (identsStructurallyEqualAcrossModules(
                    driver.all_module_envs,
                    field.name.module_idx,
                    field.name.ident,
                    field_name.module_idx,
                    field_name.ident,
                )) {
                    break :blk resolvedMonotype(field.type_idx, source_monotype.module_idx);
                }
            }
            std.debug.panic(
                "ContextMono invariant violated: record field projection could not find field in monotype",
                .{},
            );
        },
        .tuple_elem => |elem_index| blk: {
            const tuple = switch (mono) {
                .tuple => |tuple| tuple,
                else => std.debug.panic(
                    "ContextMono invariant violated: tuple projection expected tuple monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            const elems = result.context_mono.monotype_store.getIdxSpan(tuple.elems);
            if (builtin.mode == .Debug and elem_index >= elems.len) {
                std.debug.panic(
                    "ContextMono invariant violated: tuple projection elem_index {d} out of bounds for tuple arity {d}",
                    .{ elem_index, elems.len },
                );
            }
            break :blk resolvedMonotype(elems[elem_index], source_monotype.module_idx);
        },
        .tag_payload => |payload| blk: {
            const tags = switch (mono) {
                .tag_union => |tag_union| result.context_mono.monotype_store.getTags(tag_union.tags),
                else => std.debug.panic(
                    "ContextMono invariant violated: tag payload projection expected tag_union monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            for (tags) |tag| {
                if (!identsStructurallyEqualAcrossModules(
                    driver.all_module_envs,
                    tag.name.module_idx,
                    tag.name.ident,
                    payload.tag_name.module_idx,
                    payload.tag_name.ident,
                )) continue;
                const payload_monos = result.context_mono.monotype_store.getIdxSpan(tag.payloads);
                if (builtin.mode == .Debug and payload.payload_index >= payload_monos.len) {
                    std.debug.panic(
                        "ContextMono invariant violated: tag payload projection index {d} out of bounds for payload arity {d}",
                        .{ payload.payload_index, payload_monos.len },
                    );
                }
                break :blk resolvedMonotype(payload_monos[payload.payload_index], source_monotype.module_idx);
            }
            std.debug.panic(
                "ContextMono invariant violated: tag payload projection could not find tag in monotype",
                .{},
            );
        },
        .list_elem => |elem_index| blk: {
            _ = elem_index;
            const list = switch (mono) {
                .list => |list| list,
                else => std.debug.panic(
                    "ContextMono invariant violated: list elem projection expected list monotype, found '{s}'",
                    .{@tagName(mono)},
                ),
            };
            break :blk resolvedMonotype(list.elem, source_monotype.module_idx);
        },
    };
}

pub fn requireFullyBoundExprMonotype(
    driver: anytype,
    result: anytype,
    thread: anytype,
    module_idx: u32,
    expr_idx: CIR.Expr.Idx,
) Allocator.Error!ResolvedMonotype {
    const resolved = (try lookupRecordedExprMonotypeIfReadyForSourceContext(
        driver,
        result,
        thread,
        thread.requireSourceContext(),
        module_idx,
        expr_idx,
    )) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "ContextMono invariant violated: missing fully-bound monotype for expr {d} ({s}) in module {d} under source context {s}/{d}/{d}",
                .{
                    @intFromEnum(expr_idx),
                    @tagName(driver.all_module_envs[module_idx].store.getExpr(expr_idx)),
                    module_idx,
                    @tagName(thread.requireSourceContext()),
                    switch (thread.requireSourceContext()) {
                        .callable_inst => |context_id| @intFromEnum(context_id),
                        .root_expr, .provenance_expr, .template_expr => std.math.maxInt(u32),
                    },
                    switch (thread.requireSourceContext()) {
                        .root_expr => |root| @intFromEnum(root.expr_idx),
                        .provenance_expr => |source| @intFromEnum(source.expr_idx),
                        .template_expr => |template_ctx| @intFromEnum(template_ctx.expr_idx),
                        .callable_inst => std.math.maxInt(u32),
                    },
                },
            );
        }
        unreachable;
    };
    return resolved;
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
                    try remapMonotypeBetweenModules(driver, result,
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
        if (!Result.contextTypeVarKeyMatchesSourceContext(source_context, key)) continue;
        if (entry.value_ptr.module_idx != module_idx) continue;
        try specializations.put(key.type_var, entry.value_ptr.idx);
    }
}

pub fn seedRecordedTypeScopeSpecializations(
    _: anytype,
    result: anytype,
    module_idx: u32,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
) Allocator.Error!void {
    var it = result.context_mono.type_scope_monotypes.iterator();
    while (it.next()) |entry| {
        if (entry.key_ptr.module_idx != module_idx) continue;
        if (entry.value_ptr.module_idx != module_idx) {
            std.debug.panic(
                "ContextMono invariant violated: type-scope monotype module mismatch key_module={d} value_module={d} type_var={d}",
                .{
                    module_idx,
                    entry.value_ptr.module_idx,
                    @intFromEnum(entry.key_ptr.type_var),
                },
            );
        }
        try bindings.put(entry.key_ptr.type_var, entry.value_ptr.idx);
    }
}

pub fn monotypeFromTypeVarInStore(
    driver: anytype,
    result: anytype,
    module_idx: u32,
    store_types: *const types.Store,
    var_: types.Var,
) Allocator.Error!Monotype.Idx {
    var specializations = std.AutoHashMap(types.Var, Monotype.Idx).init(driver.allocator);
    defer specializations.deinit();

    try seedRecordedTypeScopeSpecializations(driver, result, module_idx, &specializations);

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
        &specializations,
        &nominal_cycle_breakers,
        &scratches,
    );
}

pub fn resolveTypeVarMonotypeWithBindings(
    driver: anytype,
    result: anytype,
    module_idx: u32,
    store_types: *const types.Store,
    var_: types.Var,
    bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
) Allocator.Error!Monotype.Idx {
    var exact_specializations = std.AutoHashMap(types.Var, Monotype.Idx).init(driver.allocator);
    defer exact_specializations.deinit();

    var bindings_it = bindings.iterator();
    while (bindings_it.next()) |entry| {
        if (entry.key_ptr.module_idx != module_idx) continue;

        if (exact_specializations.get(entry.key_ptr.type_var)) |existing| {
            if (entry.value_ptr.module_idx != module_idx or
                !try monotypesStructurallyEqual(driver, result, existing, entry.value_ptr.idx))
            {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "ContextMono: conflicting exact binding for type var root {d} in module {d}",
                        .{ @intFromEnum(entry.key_ptr.type_var), module_idx },
                    );
                }
                unreachable;
            }
            continue;
        }

        try exact_specializations.put(entry.key_ptr.type_var, entry.value_ptr.idx);
    }

    try seedRecordedTypeScopeSpecializations(driver, result, module_idx, &exact_specializations);

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

pub fn remapMonotypeBetweenModules(
    driver: anytype,
    result: anytype,
    monotype: Monotype.Idx,
    from_module_idx: u32,
    to_module_idx: u32,
) Allocator.Error!Monotype.Idx {
    if (monotype.isNone() or from_module_idx == to_module_idx) return monotype;

    var remapped = std.AutoHashMap(Monotype.Idx, Monotype.Idx).init(driver.allocator);
    defer remapped.deinit();

    var scratches = try Monotype.Store.Scratches.init(driver.allocator);
    defer scratches.deinit();
    scratches.ident_store = driver.all_module_envs[to_module_idx].getIdentStoreConst();
    scratches.module_env = driver.all_module_envs[to_module_idx];
    scratches.module_idx = to_module_idx;
    scratches.all_module_envs = driver.all_module_envs;

    return remapMonotypeBetweenModulesRec(
        driver,
        result,
        monotype,
        from_module_idx,
        to_module_idx,
        &remapped,
        &scratches,
    );
}

fn remapMonotypeBetweenModulesRec(
    driver: anytype,
    result: anytype,
    monotype: Monotype.Idx,
    from_module_idx: u32,
    to_module_idx: u32,
    remapped: *std.AutoHashMap(Monotype.Idx, Monotype.Idx),
    scratches: *Monotype.Store.Scratches,
) Allocator.Error!Monotype.Idx {
    if (monotype.isNone() or from_module_idx == to_module_idx) return monotype;
    if (remapped.get(monotype)) |existing| return existing;

    const mono = result.context_mono.monotype_store.getMonotype(monotype);
    switch (mono) {
        .unit => return result.context_mono.monotype_store.unit_idx,
        .prim => |prim| return result.context_mono.monotype_store.primIdx(prim),
        .recursive_placeholder => {
            if (builtin.mode == .Debug) {
                std.debug.panic("remapMonotypeBetweenModules: unexpected recursive_placeholder", .{});
            }
            unreachable;
        },
        .list, .box, .tuple, .func, .record, .tag_union => {},
    }

    const placeholder = try (@constCast(&result.context_mono.monotype_store)).addMonotype(driver.allocator, .recursive_placeholder);
    try remapped.put(monotype, placeholder);

    const mapped_mono: Monotype.Monotype = switch (mono) {
        .list => |list_mono| .{ .list = .{
            .elem = try remapMonotypeBetweenModulesRec(
                driver,
                result,
                list_mono.elem,
                from_module_idx,
                to_module_idx,
                remapped,
                scratches,
            ),
        } },
        .box => |box_mono| .{ .box = .{
            .inner = try remapMonotypeBetweenModulesRec(
                driver,
                result,
                box_mono.inner,
                from_module_idx,
                to_module_idx,
                remapped,
                scratches,
            ),
        } },
        .tuple => |tuple_mono| blk: {
            const idx_top = scratches.idxs.top();
            defer scratches.idxs.clearFrom(idx_top);

            var elem_i: usize = 0;
            while (elem_i < tuple_mono.elems.len) : (elem_i += 1) {
                const elem_mono = result.context_mono.monotype_store.getIdxSpanItem(tuple_mono.elems, elem_i);
                try scratches.idxs.append(try remapMonotypeBetweenModulesRec(
                    driver,
                    result,
                    elem_mono,
                    from_module_idx,
                    to_module_idx,
                    remapped,
                    scratches,
                ));
            }

            const mapped_elems = try (@constCast(&result.context_mono.monotype_store)).addIdxSpan(
                driver.allocator,
                scratches.idxs.sliceFromStart(idx_top),
            );
            break :blk .{ .tuple = .{ .elems = mapped_elems } };
        },
        .func => |func_mono| blk: {
            const idx_top = scratches.idxs.top();
            defer scratches.idxs.clearFrom(idx_top);

            var arg_i: usize = 0;
            while (arg_i < func_mono.args.len) : (arg_i += 1) {
                const arg_mono = result.context_mono.monotype_store.getIdxSpanItem(func_mono.args, arg_i);
                try scratches.idxs.append(try remapMonotypeBetweenModulesRec(
                    driver,
                    result,
                    arg_mono,
                    from_module_idx,
                    to_module_idx,
                    remapped,
                    scratches,
                ));
            }
            const mapped_args = try (@constCast(&result.context_mono.monotype_store)).addIdxSpan(
                driver.allocator,
                scratches.idxs.sliceFromStart(idx_top),
            );

            const mapped_ret = try remapMonotypeBetweenModulesRec(
                driver,
                result,
                func_mono.ret,
                from_module_idx,
                to_module_idx,
                remapped,
                scratches,
            );

            break :blk .{ .func = .{
                .args = mapped_args,
                .ret = mapped_ret,
                .effectful = func_mono.effectful,
            } };
        },
        .record => |record_mono| blk: {
            const fields_top = scratches.fields.top();
            defer scratches.fields.clearFrom(fields_top);

            var field_i: usize = 0;
            while (field_i < record_mono.fields.len) : (field_i += 1) {
                const field = result.context_mono.monotype_store.getFieldItem(record_mono.fields, field_i);
                try scratches.fields.append(.{
                    .name = field.name,
                    .type_idx = try remapMonotypeBetweenModulesRec(
                        driver,
                        result,
                        field.type_idx,
                        from_module_idx,
                        to_module_idx,
                        remapped,
                        scratches,
                    ),
                });
            }

            const mapped_fields = try (@constCast(&result.context_mono.monotype_store)).addFields(
                driver.allocator,
                scratches.fields.sliceFromStart(fields_top),
            );
            break :blk .{ .record = .{ .fields = mapped_fields } };
        },
        .tag_union => |tag_union_mono| blk: {
            const tags_top = scratches.tags.top();
            defer scratches.tags.clearFrom(tags_top);

            var tag_i: usize = 0;
            while (tag_i < tag_union_mono.tags.len) : (tag_i += 1) {
                const tag = result.context_mono.monotype_store.getTagItem(tag_union_mono.tags, tag_i);
                const payload_top = scratches.idxs.top();
                defer scratches.idxs.clearFrom(payload_top);

                var payload_i: usize = 0;
                while (payload_i < tag.payloads.len) : (payload_i += 1) {
                    const payload_mono = result.context_mono.monotype_store.getIdxSpanItem(tag.payloads, payload_i);
                    try scratches.idxs.append(try remapMonotypeBetweenModulesRec(
                        driver,
                        result,
                        payload_mono,
                        from_module_idx,
                        to_module_idx,
                        remapped,
                        scratches,
                    ));
                }

                const mapped_payloads = try (@constCast(&result.context_mono.monotype_store)).addIdxSpan(
                    driver.allocator,
                    scratches.idxs.sliceFromStart(payload_top),
                );
                try scratches.tags.append(.{
                    .name = tag.name,
                    .payloads = mapped_payloads,
                });
            }

            const mapped_tags = try (@constCast(&result.context_mono.monotype_store)).addTags(
                driver.allocator,
                scratches.tags.sliceFromStart(tags_top),
            );
            break :blk .{ .tag_union = .{ .tags = mapped_tags } };
        },
        .unit, .prim, .recursive_placeholder => unreachable,
    };

    result.context_mono.monotype_store.monotypes.items[@intFromEnum(placeholder)] = mapped_mono;
    return placeholder;
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
    try seedRecordedTypeScopeSpecializations(driver, result, module_idx, &exact_specializations);

    var extra_it = extra_bindings.iterator();
    while (extra_it.next()) |entry| {
        if (entry.key_ptr.module_idx != module_idx) continue;
        const canonical_mono = if (entry.value_ptr.module_idx == module_idx)
            entry.value_ptr.idx
        else
            try remapMonotypeBetweenModules(driver, result,
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
    return (@constCast(&result.context_mono.monotype_store)).fromTypeVarExact(
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
    try seedRecordedTypeScopeSpecializations(driver, result, module_idx, &exact_specializations);

    var extra_it = extra_bindings.iterator();
    while (extra_it.next()) |entry| {
        if (entry.key_ptr.module_idx != module_idx) continue;
        const canonical_mono = if (entry.value_ptr.module_idx == module_idx)
            entry.value_ptr.idx
        else
            try remapMonotypeBetweenModules(driver, result,
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
    return (@constCast(&result.context_mono.monotype_store)).fromTypeVar(
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
    const source_context = requireSourceContextFromThreadLike(thread, module_idx, var_);
    const module_env = driver.all_module_envs[module_idx];
    const resolved_var = module_env.types.resolveVar(var_).var_;
    if (lookupContextTypeVarMonotype(driver, result, source_context, module_idx, resolved_var)) |mono| {
        return mono;
    }
    const mono = try monotypeFromTypeVarInSourceContext(
        driver,
        result,
        source_context,
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
        source_context,
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
    const Thread = @TypeOf(thread);
    const pattern_source = blk: {
        if (@hasDecl(Thread, "lookupValueBinding")) {
            if (thread.lookupValueBinding(module_idx, pattern_idx)) |source| switch (source) {
                .bound_expr => |bound_expr| break :blk bound_expr.expr_ref,
                .lexical_binding => |lexical_binding| {
                    const SourceContextThread = struct {
                        source_context: SourceContext,

                        pub fn requireSourceContext(self: @This()) SourceContext {
                            return self.source_context;
                        }
                    };
                    return resolveTypeVarMonotypeResolved(
                        driver,
                        result,
                        SourceContextThread{ .source_context = .{
                            .callable_inst = @enumFromInt(@intFromEnum(lexical_binding.callable_inst)),
                        } },
                        lexical_binding.module_idx,
                        ModuleEnv.varFrom(lexical_binding.pattern_idx),
                    );
                },
            };
        }
        break :blk null;
    };
    if (pattern_source) |source| {
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
        for (result.lambdasolved.getValueProjectionEntries(source.projections)) |projection| {
            resolved = try projectResolvedMonotypeByValueProjection(driver, result, resolved, projection);
        }
        return resolved;
    }
    if (lookupContextTypeVarMonotype(
        driver,
        result,
        source_context,
        module_idx,
        ModuleEnv.varFrom(pattern_idx),
    )) |resolved| {
        return resolved;
    }
    return resolveTypeVarMonotypeResolved(
        driver,
        result,
        thread,
        module_idx,
        ModuleEnv.varFrom(pattern_idx),
    );
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

pub fn ensureTypeSubst(
    driver: anytype,
    result: anytype,
    template: anytype,
    fn_monotype: Monotype.Idx,
    fn_monotype_module_idx: u32,
) Allocator.Error!TypeSubstId {
    var bindings = std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype).init(driver.allocator);
    defer bindings.deinit();

    var ordered_entries = std.ArrayList(TypeSubstEntry).empty;
    defer ordered_entries.deinit(driver.allocator);

    try bindTypeVarMonotypes(
        driver,
        result,
        template.module_idx,
        &driver.all_module_envs[template.module_idx].types,
        &bindings,
        &ordered_entries,
        template.type_root,
        fn_monotype,
        fn_monotype_module_idx,
    );
    for (result.context_mono.substs.items, 0..) |existing_subst, idx| {
        if (try typeSubstEntriesEqual(
            driver,
            result,
            result.getTypeSubstEntries(existing_subst.entries),
            ordered_entries.items,
        )) {
            return @enumFromInt(idx);
        }
    }

    const entries_span: TypeSubstSpan = if (ordered_entries.items.len == 0)
        TypeSubstSpan.empty()
    else blk: {
        const start: u32 = @intCast(result.context_mono.subst_entries.items.len);
        try result.context_mono.subst_entries.appendSlice(driver.allocator, ordered_entries.items);
        break :blk TypeSubstSpan{
            .start = start,
            .len = @as(u16, @intCast(ordered_entries.items.len)),
        };
    };

    const subst_id: TypeSubstId = @enumFromInt(result.context_mono.substs.items.len);
    try result.context_mono.substs.append(driver.allocator, .{ .entries = entries_span });
    return subst_id;
}

fn typeSubstEntriesEqual(
    driver: anytype,
    result: anytype,
    lhs: []const TypeSubstEntry,
    rhs: []const TypeSubstEntry,
) Allocator.Error!bool {
    if (lhs.len != rhs.len) return false;

    for (lhs, rhs) |lhs_entry, rhs_entry| {
        if (!std.meta.eql(lhs_entry.key, rhs_entry.key)) return false;
        if (lhs_entry.monotype.module_idx != rhs_entry.monotype.module_idx) return false;
        if (!try monotypesStructurallyEqual(driver, result, lhs_entry.monotype.idx, rhs_entry.monotype.idx)) {
            return false;
        }
    }

    return true;
}

pub fn bindTypeVarMonotypesInStore(
    driver: anytype,
    result: anytype,
    module_idx: u32,
    store_types: *const types.Store,
    common_idents: ModuleEnv.CommonIdents,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
    type_var: types.Var,
    monotype: Monotype.Idx,
) Allocator.Error!void {
    if (monotype.isNone()) return;

    const resolved = store_types.resolveVar(type_var);
    if (bindings.get(resolved.var_)) |existing| {
        if (!(try monotypesStructurallyEqual(driver, result, existing, monotype))) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "ContextMono: conflicting monotype binding for type var root {d}",
                    .{@intFromEnum(resolved.var_)},
                );
            }
            unreachable;
        }
        return;
    }

    switch (resolved.desc.content) {
        .flex, .rigid => try bindings.put(resolved.var_, monotype),
        .alias => |alias| try bindTypeVarMonotypesInStore(
            driver,
            result,
            module_idx,
            store_types,
            common_idents,
            bindings,
            store_types.getAliasBackingVar(alias),
            monotype,
        ),
        .structure => |flat_type| {
            try bindings.put(resolved.var_, monotype);
            try bindFlatTypeMonotypesInStore(
                driver,
                result,
                module_idx,
                store_types,
                common_idents,
                bindings,
                flat_type,
                monotype,
            );
        },
        .err => {},
    }
}

fn flatRecordRepresentsEmpty(store_types: *const types.Store, record: types.Record) bool {
    var current_row = record;

    rows: while (true) {
        if (store_types.getRecordFieldsSlice(current_row.fields).len != 0) return false;

        var ext_var = current_row.ext;
        while (true) {
            const ext_resolved = store_types.resolveVar(ext_var);
            switch (ext_resolved.desc.content) {
                .alias => |alias| {
                    ext_var = store_types.getAliasBackingVar(alias);
                    continue;
                },
                .structure => |ext_flat| switch (ext_flat) {
                    .record => |next_row| {
                        current_row = next_row;
                        continue :rows;
                    },
                    .record_unbound => |fields_range| return store_types.getRecordFieldsSlice(fields_range).len == 0,
                    .empty_record => return true,
                    else => return false,
                },
                .flex, .rigid, .err => return false,
            }
        }
    }
}

fn bindFlatTypeMonotypesInStore(
    driver: anytype,
    result: anytype,
    module_idx: u32,
    store_types: *const types.Store,
    common_idents: ModuleEnv.CommonIdents,
    bindings: *std.AutoHashMap(types.Var, Monotype.Idx),
    flat_type: types.FlatType,
    monotype: Monotype.Idx,
) Allocator.Error!void {
    if (monotype.isNone()) return;

    const mono = result.context_mono.monotype_store.getMonotype(monotype);

    switch (flat_type) {
        .fn_pure, .fn_effectful, .fn_unbound => |func| {
            const mfunc = switch (mono) {
                .func => |mfunc| mfunc,
                else => unreachable,
            };

            const type_args = store_types.sliceVars(func.args);
            const mono_args = result.context_mono.monotype_store.getIdxSpan(mfunc.args);
            if (type_args.len != mono_args.len) unreachable;
            for (type_args, 0..) |arg_var, i| {
                try bindTypeVarMonotypesInStore(driver, result, module_idx, store_types, common_idents, bindings, arg_var, mono_args[i]);
            }
            try bindTypeVarMonotypesInStore(driver, result, module_idx, store_types, common_idents, bindings, func.ret, mfunc.ret);
        },
        .nominal_type => |nominal| {
            const ident = nominal.ident.ident_idx;
            const origin = nominal.origin_module;

            if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.list)) {
                const mlist = switch (mono) {
                    .list => |mlist| mlist,
                    else => unreachable,
                };
                const type_args = store_types.sliceNominalArgs(nominal);
                if (type_args.len != 1) unreachable;
                try bindTypeVarMonotypesInStore(driver, result, module_idx, store_types, common_idents, bindings, type_args[0], mlist.elem);
                return;
            }

            if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.box)) {
                const mbox = switch (mono) {
                    .box => |mbox| mbox,
                    else => unreachable,
                };
                const type_args = store_types.sliceNominalArgs(nominal);
                if (type_args.len != 1) unreachable;
                try bindTypeVarMonotypesInStore(driver, result, module_idx, store_types, common_idents, bindings, type_args[0], mbox.inner);
                return;
            }

            if (origin.eql(common_idents.builtin_module) and DispatchSolved.builtinPrimForNominal(ident, common_idents) != null) {
                switch (mono) {
                    .prim => {},
                    else => unreachable,
                }
                return;
            }

            try bindTypeVarMonotypesInStore(
                driver,
                result,
                module_idx,
                store_types,
                common_idents,
                bindings,
                store_types.getNominalBackingVar(nominal),
                monotype,
            );
        },
        .record => |record| {
            const mrec = switch (mono) {
                .record => |mrec| mrec,
                .unit => {
                    if (flatRecordRepresentsEmpty(store_types, record)) return;
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "ContextMono invariant violated: non-empty store record matched unit monotype (module={d}, monotype={d})",
                            .{ module_idx, @intFromEnum(monotype) },
                        );
                    }
                    unreachable;
                },
                else => {
                    if (builtin.mode == .Debug) {
                        std.debug.panic(
                            "ContextMono invariant violated: expected record monotype for store record, got {s} (module={d}, monotype={d})",
                            .{ @tagName(mono), module_idx, @intFromEnum(monotype) },
                        );
                    }
                    unreachable;
                },
            };
            const mono_fields = result.context_mono.monotype_store.getFields(mrec.fields);
            var seen_field_indices: std.ArrayListUnmanaged(u32) = .empty;
            defer seen_field_indices.deinit(driver.allocator);

            var current_row = record;
            rows: while (true) {
                const fields_slice = store_types.getRecordFieldsSlice(current_row.fields);
                const field_names = fields_slice.items(.name);
                const field_vars = fields_slice.items(.var_);
                for (field_names, field_vars) |field_name, field_var| {
                    const field_idx = recordFieldIndexByName(
                        driver,
                        module_idx,
                        field_name,
                        module_idx,
                        mono_fields,
                    );
                    try appendSeenIndex(driver.allocator, &seen_field_indices, field_idx);
                    try bindTypeVarMonotypesInStore(
                        driver,
                        result,
                        module_idx,
                        store_types,
                        common_idents,
                        bindings,
                        field_var,
                        mono_fields[field_idx].type_idx,
                    );
                }

                var ext_var = current_row.ext;
                while (true) {
                    const ext_resolved = store_types.resolveVar(ext_var);
                    switch (ext_resolved.desc.content) {
                        .alias => |alias| {
                            ext_var = store_types.getAliasBackingVar(alias);
                            continue;
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .record => |next_row| {
                                current_row = next_row;
                                continue :rows;
                            },
                            .record_unbound => |fields_range| {
                                const ext_fields = store_types.getRecordFieldsSlice(fields_range);
                                const ext_names = ext_fields.items(.name);
                                const ext_vars = ext_fields.items(.var_);
                                for (ext_names, ext_vars) |field_name, field_var| {
                                    const field_idx = recordFieldIndexByName(
                                        driver,
                                        module_idx,
                                        field_name,
                                        module_idx,
                                        mono_fields,
                                    );
                                    if (seenIndex(seen_field_indices.items, field_idx)) continue;
                                    try bindTypeVarMonotypesInStore(
                                        driver,
                                        result,
                                        module_idx,
                                        store_types,
                                        common_idents,
                                        bindings,
                                        field_var,
                                        mono_fields[field_idx].type_idx,
                                    );
                                }
                                return;
                            },
                            .empty_record => return,
                            else => unreachable,
                        },
                        .flex, .rigid, .err => return,
                    }
                }
            }
        },
        .record_unbound => |fields_range| {
            const mrec = switch (mono) {
                .record => |mrec| mrec,
                .unit => {
                    if (store_types.getRecordFieldsSlice(fields_range).len == 0) return;
                    unreachable;
                },
                else => unreachable,
            };
            const mono_fields = result.context_mono.monotype_store.getFields(mrec.fields);
            const fields = store_types.getRecordFieldsSlice(fields_range);
            for (fields.items(.name), fields.items(.var_)) |field_name, field_var| {
                const field_idx = recordFieldIndexByName(driver, module_idx, field_name, module_idx, mono_fields);
                try bindTypeVarMonotypesInStore(
                    driver,
                    result,
                    module_idx,
                    store_types,
                    common_idents,
                    bindings,
                    field_var,
                    mono_fields[field_idx].type_idx,
                );
            }
        },
        .tuple => |tuple| {
            const mtup = switch (mono) {
                .tuple => |mtup| mtup,
                else => unreachable,
            };
            const mono_elems = result.context_mono.monotype_store.getIdxSpan(mtup.elems);
            const elem_vars = store_types.sliceVars(tuple.elems);
            if (mono_elems.len != elem_vars.len) unreachable;
            for (elem_vars, mono_elems) |elem_var, elem_mono| {
                try bindTypeVarMonotypesInStore(driver, result, module_idx, store_types, common_idents, bindings, elem_var, elem_mono);
            }
        },
        .tag_union => |tag_union| {
            const mtag = switch (mono) {
                .tag_union => |mtag| mtag,
                else => unreachable,
            };
            const mono_tags = result.context_mono.monotype_store.getTags(mtag.tags);
            const tags = store_types.getTagsSlice(tag_union.tags);
            const tag_args = tags.items(.args);
            if (tag_args.len != mono_tags.len) unreachable;
            for (tag_args, mono_tags) |args_range, mono_tag| {
                const payload_vars = store_types.sliceVars(args_range);
                const mono_payloads = result.context_mono.monotype_store.getIdxSpan(mono_tag.payloads);
                if (payload_vars.len != mono_payloads.len) unreachable;
                for (payload_vars, mono_payloads) |payload_var, payload_mono| {
                    try bindTypeVarMonotypesInStore(driver, result, module_idx, store_types, common_idents, bindings, payload_var, payload_mono);
                }
            }
        },
        .empty_record => switch (mono) {
            .unit, .record => {},
            else => unreachable,
        },
        .empty_tag_union => switch (mono) {
            .tag_union => {},
            else => unreachable,
        },
    }
}

pub fn recordFieldIndexByName(
    driver: anytype,
    template_module_idx: u32,
    field_name: base.Ident.Idx,
    mono_module_idx: u32,
    mono_fields: []const Monotype.Field,
) u32 {
    for (mono_fields, 0..) |mono_field, field_idx| {
        if (identsStructurallyEqualAcrossModules(
            driver.all_module_envs,
            template_module_idx,
            field_name,
            mono_module_idx,
            mono_field.name,
        )) {
            return @intCast(field_idx);
        }
    }

    if (std.debug.runtime_safety) {
        std.debug.panic(
            "ContextMono: record field '{s}' missing from monotype (template_module={d}, mono_module={d})",
            .{ driver.all_module_envs[template_module_idx].getIdent(field_name), template_module_idx, mono_module_idx },
        );
    }
    unreachable;
}

pub fn recordFieldIndexByNameInSpan(
    driver: anytype,
    result: anytype,
    template_module_idx: u32,
    field_name: base.Ident.Idx,
    mono_module_idx: u32,
    mono_fields: Monotype.FieldSpan,
) u32 {
    var field_i: usize = 0;
    while (field_i < mono_fields.len) : (field_i += 1) {
        const mono_field = result.context_mono.monotype_store.getFieldItem(mono_fields, field_i);
        if (identsStructurallyEqualAcrossModules(
            driver.all_module_envs,
            template_module_idx,
            field_name,
            mono_module_idx,
            mono_field.name,
        )) {
            return @intCast(field_i);
        }
    }

    if (std.debug.runtime_safety) {
        std.debug.panic(
            "ContextMono: record field '{s}' missing from monotype (template_module={d}, mono_module={d})",
            .{ driver.all_module_envs[template_module_idx].getIdent(field_name), template_module_idx, mono_module_idx },
        );
    }
    unreachable;
}

pub fn tagIndexByNameInSpan(
    driver: anytype,
    result: anytype,
    template_module_idx: u32,
    tag_name: base.Ident.Idx,
    mono_module_idx: u32,
    mono_tags: Monotype.TagSpan,
) u32 {
    var tag_i: usize = 0;
    while (tag_i < mono_tags.len) : (tag_i += 1) {
        const mono_tag = result.context_mono.monotype_store.getTagItem(mono_tags, tag_i);
        if (identsStructurallyEqualAcrossModules(
            driver.all_module_envs,
            template_module_idx,
            tag_name,
            mono_module_idx,
            mono_tag.name,
        )) {
            return @intCast(tag_i);
        }
    }

    if (std.debug.runtime_safety) {
        std.debug.panic(
            "ContextMono: tag '{s}' missing from monotype",
            .{driver.all_module_envs[template_module_idx].getIdent(tag_name)},
        );
    }
    unreachable;
}

fn seenIndex(seen_indices: []const u32, idx: u32) bool {
    for (seen_indices) |seen_idx| {
        if (seen_idx == idx) return true;
    }
    return false;
}

fn appendSeenIndex(
    allocator: Allocator,
    seen_indices: *std.ArrayListUnmanaged(u32),
    idx: u32,
) Allocator.Error!void {
    if (seenIndex(seen_indices.items, idx)) return;
    try seen_indices.append(allocator, idx);
}

fn remainingRecordTailMonotype(
    driver: anytype,
    result: anytype,
    mono_fields: []const Monotype.Field,
    seen_indices: []const u32,
) Allocator.Error!Monotype.Idx {
    var remaining_fields: std.ArrayListUnmanaged(Monotype.Field) = .empty;
    defer remaining_fields.deinit(driver.allocator);

    for (mono_fields, 0..) |field, field_idx| {
        if (seenIndex(seen_indices, @intCast(field_idx))) continue;
        try remaining_fields.append(driver.allocator, field);
    }

    if (remaining_fields.items.len == 0) {
        return result.context_mono.monotype_store.unit_idx;
    }

    const field_span = try result.context_mono.monotype_store.addFields(driver.allocator, remaining_fields.items);
    return try result.context_mono.monotype_store.addMonotype(driver.allocator, .{ .record = .{ .fields = field_span } });
}

fn remainingTagUnionTailMonotype(
    driver: anytype,
    result: anytype,
    mono_tags: []const Monotype.Tag,
    seen_indices: []const u32,
) Allocator.Error!Monotype.Idx {
    var remaining_tags: std.ArrayListUnmanaged(Monotype.Tag) = .empty;
    defer remaining_tags.deinit(driver.allocator);

    for (mono_tags, 0..) |tag, tag_idx| {
        if (seenIndex(seen_indices, @intCast(tag_idx))) continue;
        try remaining_tags.append(driver.allocator, tag);
    }

    const tag_span = try result.context_mono.monotype_store.addTags(driver.allocator, remaining_tags.items);
    return try result.context_mono.monotype_store.addMonotype(driver.allocator, .{ .tag_union = .{ .tags = tag_span } });
}

fn bindRecordRowTail(
    driver: anytype,
    result: anytype,
    template_module_idx: u32,
    template_types: *const types.Store,
    bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ordered_entries: *std.ArrayList(TypeSubstEntry),
    ext_var: types.Var,
    mono_fields: []const Monotype.Field,
    seen_indices: []const u32,
    mono_module_idx: u32,
    comptime allow_failure: bool,
) Allocator.Error!bool {
    const tail_mono = try remainingRecordTailMonotype(driver, result, mono_fields, seen_indices);
    return bindTypeVarMonotypesMode(
        driver,
        result,
        template_module_idx,
        template_types,
        bindings,
        ordered_entries,
        ext_var,
        tail_mono,
        mono_module_idx,
        allow_failure,
    );
}

fn bindTagUnionRowTail(
    driver: anytype,
    result: anytype,
    template_module_idx: u32,
    template_types: *const types.Store,
    bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ordered_entries: *std.ArrayList(TypeSubstEntry),
    ext_var: types.Var,
    mono_tags: []const Monotype.Tag,
    seen_indices: []const u32,
    mono_module_idx: u32,
    comptime allow_failure: bool,
) Allocator.Error!bool {
    const tail_mono = try remainingTagUnionTailMonotype(driver, result, mono_tags, seen_indices);
    return bindTypeVarMonotypesMode(
        driver,
        result,
        template_module_idx,
        template_types,
        bindings,
        ordered_entries,
        ext_var,
        tail_mono,
        mono_module_idx,
        allow_failure,
    );
}

fn bindTagPayloadsByName(
    driver: anytype,
    result: anytype,
    template_module_idx: u32,
    template_types: *const types.Store,
    bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ordered_entries: *std.ArrayList(TypeSubstEntry),
    tag_name: base.Ident.Idx,
    payload_vars: []const types.Var,
    mono_tags: Monotype.TagSpan,
    mono_module_idx: u32,
    comptime allow_failure: bool,
) Allocator.Error!bool {
    var tag_i: usize = 0;
    while (tag_i < mono_tags.len) : (tag_i += 1) {
        const mono_tag = result.context_mono.monotype_store.getTagItem(mono_tags, tag_i);
        if (!identsStructurallyEqualAcrossModules(
            driver.all_module_envs,
            template_module_idx,
            tag_name,
            mono_module_idx,
            mono_tag.name,
        )) continue;

        if (payload_vars.len != mono_tag.payloads.len) {
            if (allow_failure) return false;
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "ContextMono: payload arity mismatch for tag '{s}'",
                    .{driver.all_module_envs[template_module_idx].getIdent(tag_name)},
                );
            }
            unreachable;
        }

        for (payload_vars, 0..) |payload_var, i| {
            const mono_payload = result.context_mono.monotype_store.getIdxSpanItem(mono_tag.payloads, i);
            if (!try bindTypeVarMonotypesMode(
                driver,
                result,
                template_module_idx,
                template_types,
                bindings,
                ordered_entries,
                payload_var,
                mono_payload,
                mono_module_idx,
                allow_failure,
            )) return false;
        }
        return true;
    }

    if (allow_failure) return false;
    if (std.debug.runtime_safety) {
        std.debug.panic(
            "ContextMono: tag '{s}' missing from monotype",
            .{driver.all_module_envs[template_module_idx].getIdent(tag_name)},
        );
    }
    unreachable;
}

pub fn bindTypeVarMonotypes(
    driver: anytype,
    result: anytype,
    template_module_idx: u32,
    template_types: *const types.Store,
    bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ordered_entries: *std.ArrayList(TypeSubstEntry),
    type_var: types.Var,
    monotype: Monotype.Idx,
    mono_module_idx: u32,
) Allocator.Error!void {
    const ok = try bindTypeVarMonotypesMode(
        driver,
        result,
        template_module_idx,
        template_types,
        bindings,
        ordered_entries,
        type_var,
        monotype,
        mono_module_idx,
        false,
    );
    if (!ok) unreachable;
}

pub fn tryBindTypeVarMonotypes(
    driver: anytype,
    result: anytype,
    template_module_idx: u32,
    template_types: *const types.Store,
    bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ordered_entries: *std.ArrayList(TypeSubstEntry),
    type_var: types.Var,
    monotype: Monotype.Idx,
    mono_module_idx: u32,
) Allocator.Error!bool {
    return bindTypeVarMonotypesMode(
        driver,
        result,
        template_module_idx,
        template_types,
        bindings,
        ordered_entries,
        type_var,
        monotype,
        mono_module_idx,
        true,
    );
}

fn bindTypeVarMonotypesMode(
    driver: anytype,
    result: anytype,
    template_module_idx: u32,
    template_types: *const types.Store,
    bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ordered_entries: *std.ArrayList(TypeSubstEntry),
    type_var: types.Var,
    monotype: Monotype.Idx,
    mono_module_idx: u32,
    comptime allow_failure: bool,
) Allocator.Error!bool {
    if (monotype.isNone()) return true;
    const normalized_mono = if (mono_module_idx == template_module_idx)
        monotype
    else
        try remapMonotypeBetweenModules(driver, result, monotype, mono_module_idx, template_module_idx);
    const resolved_mono = resolvedMonotype(normalized_mono, template_module_idx);

    const resolved_key = boundTypeVarKey(template_module_idx, template_types, type_var);
    if (bindings.get(resolved_key)) |existing| {
        if (existing.module_idx != resolved_mono.module_idx or
            !try monotypesStructurallyEqual(driver, result, existing.idx, resolved_mono.idx))
        {
            if (allow_failure) return false;
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "ContextMono: conflicting monotype binding for type var root {d} in module {d} existing={d}@{d} existing_mono={any} new={d}@{d} new_mono={any}",
                    .{
                        @intFromEnum(resolved_key.type_var),
                        resolved_key.module_idx,
                        @intFromEnum(existing.idx),
                        existing.module_idx,
                        result.context_mono.monotype_store.getMonotype(existing.idx),
                        @intFromEnum(resolved_mono.idx),
                        resolved_mono.module_idx,
                        result.context_mono.monotype_store.getMonotype(resolved_mono.idx),
                    },
                );
            }
            unreachable;
        }
        return true;
    }

    const resolved = template_types.resolveVar(type_var);

    switch (resolved.desc.content) {
        .flex, .rigid => {
            try bindings.put(resolved_key, resolved_mono);
            try ordered_entries.append(driver.allocator, .{
                .key = resolved_key,
                .monotype = resolved_mono,
            });
        },
        .alias => |alias| {
            if (!try bindTypeVarMonotypesMode(
                driver,
                result,
                template_module_idx,
                template_types,
                bindings,
                ordered_entries,
                template_types.getAliasBackingVar(alias),
                normalized_mono,
                template_module_idx,
                allow_failure,
            )) return false;
        },
        .structure => |flat_type| {
            try bindings.put(resolved_key, resolved_mono);
            try ordered_entries.append(driver.allocator, .{
                .key = resolved_key,
                .monotype = resolved_mono,
            });
            if (!try bindFlatTypeMonotypesMode(
                driver,
                result,
                template_module_idx,
                template_types,
                bindings,
                ordered_entries,
                flat_type,
                normalized_mono,
                template_module_idx,
                allow_failure,
            )) return false;
        },
        .err => {},
    }

    return true;
}

fn bindFlatTypeMonotypesMode(
    driver: anytype,
    result: anytype,
    template_module_idx: u32,
    template_types: *const types.Store,
    bindings: *std.AutoHashMap(BoundTypeVarKey, ResolvedMonotype),
    ordered_entries: *std.ArrayList(TypeSubstEntry),
    flat_type: types.FlatType,
    monotype: Monotype.Idx,
    mono_module_idx: u32,
    comptime allow_failure: bool,
) Allocator.Error!bool {
    if (monotype.isNone()) return true;

    const mono = result.context_mono.monotype_store.getMonotype(monotype);
    const common_idents = ModuleEnv.CommonIdents.find(&driver.all_module_envs[template_module_idx].common);

    switch (flat_type) {
        .fn_pure, .fn_effectful, .fn_unbound => |func| {
            const mfunc = switch (mono) {
                .func => |mfunc| mfunc,
                else => {
                    return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                },
            };

            const type_args = template_types.sliceVars(func.args);
            if (type_args.len != mfunc.args.len) unreachable;

            for (type_args, 0..) |arg_var, i| {
                const mono_arg = result.context_mono.monotype_store.getIdxSpanItem(mfunc.args, i);
                if (!try bindTypeVarMonotypesMode(
                    driver,
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    arg_var,
                    mono_arg,
                    mono_module_idx,
                    allow_failure,
                )) return false;
            }
            if (!try bindTypeVarMonotypesMode(
                driver,
                result,
                template_module_idx,
                template_types,
                bindings,
                ordered_entries,
                func.ret,
                mfunc.ret,
                mono_module_idx,
                allow_failure,
            )) return false;
        },
        .nominal_type => |nominal| {
            const ident = nominal.ident.ident_idx;
            const origin = nominal.origin_module;

            if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.list)) {
                const mlist = switch (mono) {
                    .list => |mlist| mlist,
                    else => {
                        return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    },
                };
                const type_args = template_types.sliceNominalArgs(nominal);
                if (type_args.len != 1) unreachable;
                if (!try bindTypeVarMonotypesMode(
                    driver,
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    type_args[0],
                    mlist.elem,
                    mono_module_idx,
                    allow_failure,
                )) return false;
                return true;
            }

            if (origin.eql(common_idents.builtin_module) and ident.eql(common_idents.box)) {
                const mbox = switch (mono) {
                    .box => |mbox| mbox,
                    else => {
                        return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    },
                };
                const type_args = template_types.sliceNominalArgs(nominal);
                if (type_args.len != 1) unreachable;
                if (!try bindTypeVarMonotypesMode(
                    driver,
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    type_args[0],
                    mbox.inner,
                    mono_module_idx,
                    allow_failure,
                )) return false;
                return true;
            }

            if (origin.eql(common_idents.builtin_module) and DispatchSolved.builtinPrimForNominal(ident, common_idents) != null) {
                switch (mono) {
                    .prim => {},
                    else => {
                        return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                    },
                }
                return true;
            }

            if (!try bindTypeVarMonotypesMode(
                driver,
                result,
                template_module_idx,
                template_types,
                bindings,
                ordered_entries,
                template_types.getNominalBackingVar(nominal),
                monotype,
                mono_module_idx,
                allow_failure,
            )) return false;
        },
        .record => |record| {
            const mrec = switch (mono) {
                .record => |mrec| mrec,
                .unit => {
                    if (flatRecordRepresentsEmpty(template_types, record)) return true;
                    return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                },
                else => {
                    return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                },
            };
            var seen_field_indices: std.ArrayListUnmanaged(u32) = .empty;
            defer seen_field_indices.deinit(driver.allocator);

            var current_row = record;
            rows: while (true) {
                const fields_slice = template_types.getRecordFieldsSlice(current_row.fields);
                const field_names = fields_slice.items(.name);
                const field_vars = fields_slice.items(.var_);
                for (field_names, field_vars) |field_name, field_var| {
                    const field_idx = recordFieldIndexByNameInSpan(
                        driver,
                        result,
                        template_module_idx,
                        field_name,
                        mono_module_idx,
                        mrec.fields,
                    );
                    try appendSeenIndex(driver.allocator, &seen_field_indices, field_idx);
                    const mono_field = result.context_mono.monotype_store.getFieldItem(mrec.fields, field_idx);
                    if (!try bindTypeVarMonotypesMode(
                        driver,
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        field_var,
                        mono_field.type_idx,
                        mono_module_idx,
                        allow_failure,
                    )) return false;
                }

                var ext_var = current_row.ext;
                while (true) {
                    const ext_resolved = template_types.resolveVar(ext_var);
                    switch (ext_resolved.desc.content) {
                        .alias => |alias| {
                            ext_var = template_types.getAliasBackingVar(alias);
                            continue;
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .record => |next_row| {
                                current_row = next_row;
                                continue :rows;
                            },
                            .record_unbound => |fields_range| {
                                const ext_fields = template_types.getRecordFieldsSlice(fields_range);
                                const ext_names = ext_fields.items(.name);
                                const ext_vars = ext_fields.items(.var_);
                                for (ext_names, ext_vars) |field_name, field_var| {
                                    const field_idx = recordFieldIndexByNameInSpan(
                                        driver,
                                        result,
                                        template_module_idx,
                                        field_name,
                                        mono_module_idx,
                                        mrec.fields,
                                    );
                                    try appendSeenIndex(driver.allocator, &seen_field_indices, field_idx);
                                    const mono_field = result.context_mono.monotype_store.getFieldItem(mrec.fields, field_idx);
                                    if (!try bindTypeVarMonotypesMode(
                                        driver,
                                        result,
                                        template_module_idx,
                                        template_types,
                                        bindings,
                                        ordered_entries,
                                        field_var,
                                        mono_field.type_idx,
                                        mono_module_idx,
                                        allow_failure,
                                    )) return false;
                                }
                                break :rows;
                            },
                            .empty_record => break :rows,
                            else => {
                                return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                            },
                        },
                        .flex, .rigid => {
                            if (!try bindRecordRowTail(
                                driver,
                                result,
                                template_module_idx,
                                template_types,
                                bindings,
                                ordered_entries,
                                ext_var,
                                result.context_mono.monotype_store.getFields(mrec.fields),
                                seen_field_indices.items,
                                mono_module_idx,
                                allow_failure,
                            )) return false;
                            break :rows;
                        },
                        .err => {
                            return bindFlatTypeErrorTail(allow_failure, flat_type, template_module_idx, mono_module_idx, monotype);
                        },
                    }
                }
                return true;
            }
        },
        .record_unbound => |fields_range| {
            const mrec = switch (mono) {
                .record => |mrec| mrec,
                .unit => {
                    if (template_types.getRecordFieldsSlice(fields_range).len == 0) return true;
                    return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                },
                else => {
                    return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                },
            };
            const fields_slice = template_types.getRecordFieldsSlice(fields_range);
            const field_names = fields_slice.items(.name);
            const field_vars = fields_slice.items(.var_);
            for (field_names, field_vars) |field_name, field_var| {
                const field_idx = recordFieldIndexByNameInSpan(
                    driver,
                    result,
                    template_module_idx,
                    field_name,
                    mono_module_idx,
                    mrec.fields,
                );
                const mono_field = result.context_mono.monotype_store.getFieldItem(mrec.fields, field_idx);
                if (!try bindTypeVarMonotypesMode(
                    driver,
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    field_var,
                    mono_field.type_idx,
                    mono_module_idx,
                    allow_failure,
                )) return false;
            }
        },
        .tuple => |tuple| {
            const mtup = switch (mono) {
                .tuple => |mtup| mtup,
                else => {
                    return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                },
            };
            const elem_vars = template_types.sliceVars(tuple.elems);
            if (elem_vars.len != mtup.elems.len) unreachable;
            for (elem_vars, 0..) |elem_var, i| {
                const elem_mono = result.context_mono.monotype_store.getIdxSpanItem(mtup.elems, i);
                if (!try bindTypeVarMonotypesMode(
                    driver,
                    result,
                    template_module_idx,
                    template_types,
                    bindings,
                    ordered_entries,
                    elem_var,
                    elem_mono,
                    mono_module_idx,
                    allow_failure,
                )) return false;
            }
        },
        .tag_union => |tag_union| {
            const mtag = switch (mono) {
                .tag_union => |mtag| mtag,
                else => {
                    return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                },
            };
            var seen_tag_indices: std.ArrayListUnmanaged(u32) = .empty;
            defer seen_tag_indices.deinit(driver.allocator);

            var current_row = tag_union;
            rows: while (true) {
                const type_tags = template_types.getTagsSlice(current_row.tags);
                const tag_names = type_tags.items(.name);
                const tag_args = type_tags.items(.args);
                for (tag_names, tag_args) |tag_name, args_range| {
                    const tag_idx = tagIndexByNameInSpan(
                        driver,
                        result,
                        template_module_idx,
                        tag_name,
                        mono_module_idx,
                        mtag.tags,
                    );
                    try appendSeenIndex(driver.allocator, &seen_tag_indices, tag_idx);
                    if (!try bindTagPayloadsByName(
                        driver,
                        result,
                        template_module_idx,
                        template_types,
                        bindings,
                        ordered_entries,
                        tag_name,
                        template_types.sliceVars(args_range),
                        mtag.tags,
                        mono_module_idx,
                        allow_failure,
                    )) return false;
                }

                var ext_var = current_row.ext;
                while (true) {
                    const ext_resolved = template_types.resolveVar(ext_var);
                    switch (ext_resolved.desc.content) {
                        .alias => |alias| {
                            ext_var = template_types.getAliasBackingVar(alias);
                            continue;
                        },
                        .structure => |ext_flat| switch (ext_flat) {
                            .tag_union => |next_row| {
                                current_row = next_row;
                                continue :rows;
                            },
                            .empty_tag_union => break :rows,
                            else => {
                                return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
                            },
                        },
                        .flex, .rigid => {
                            if (!try bindTagUnionRowTail(
                                driver,
                                result,
                                template_module_idx,
                                template_types,
                                bindings,
                                ordered_entries,
                                ext_var,
                                result.context_mono.monotype_store.getTags(mtag.tags),
                                seen_tag_indices.items,
                                mono_module_idx,
                                allow_failure,
                            )) return false;
                            break :rows;
                        },
                        .err => {
                            return bindFlatTypeErrorTail(allow_failure, flat_type, template_module_idx, mono_module_idx, monotype);
                        },
                    }
                }
            }
        },
        .empty_record => switch (mono) {
            .unit, .record => {},
            else => {
                return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
            },
        },
        .empty_tag_union => switch (mono) {
            .tag_union => {},
            else => {
                return bindFlatTypeMismatch(allow_failure, flat_type, mono, template_module_idx, mono_module_idx, monotype);
            },
        },
    }

    return true;
}

fn bindFlatTypeMismatch(
    comptime allow_failure: bool,
    flat_type: types.FlatType,
    mono: Monotype.Monotype,
    template_module_idx: u32,
    mono_module_idx: u32,
    monotype: Monotype.Idx,
) bool {
    if (allow_failure) return false;
    if (std.debug.runtime_safety) {
        std.debug.panic(
            "ContextMono bindFlatTypeMonotypes mismatch: flat_type={s} mono={s} template_module={d} mono_module={d} monotype={d}",
            .{
                @tagName(flat_type),
                @tagName(mono),
                template_module_idx,
                mono_module_idx,
                @intFromEnum(monotype),
            },
        );
    }
    unreachable;
}

fn bindFlatTypeErrorTail(
    comptime allow_failure: bool,
    flat_type: types.FlatType,
    template_module_idx: u32,
    mono_module_idx: u32,
    monotype: Monotype.Idx,
) bool {
    if (allow_failure) return false;
    if (std.debug.runtime_safety) {
        std.debug.panic(
            "ContextMono bindFlatTypeMonotypes hit err tail: flat_type={s} template_module={d} mono_module={d} monotype={d}",
            .{
                @tagName(flat_type),
                template_module_idx,
                mono_module_idx,
                @intFromEnum(monotype),
            },
        );
    }
    unreachable;
}

fn boundTypeVarKey(
    module_idx: u32,
    store_types: *const types.Store,
    var_: types.Var,
) BoundTypeVarKey {
    return .{
        .module_idx = module_idx,
        .type_var = store_types.resolveVar(var_).var_,
    };
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
