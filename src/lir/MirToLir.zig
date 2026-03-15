//! MIR → LIR translation pass.
//!
//! Translates monomorphic, desugared MIR expressions into layout-annotated LIR
//! expressions suitable for code generation. The main job is:
//!
//! - Converting Monotype.Idx → layout.Idx for every expression/pattern
//! - Resolving tag names to numeric discriminants
//! - Splitting MIR `lambda` into LIR `lambda` (no captures) vs `closure` (with captures)
//! - Mapping MIR's `match_expr` to LIR's `match_expr`
//! - Mapping MIR low-level ops to LIR low-level ops
//!
//! Ordinary data layout is always resolved through the shared layout subsystem.
//! This pass must not reintroduce generic record/tuple/list/tag-union layout
//! construction. The only lowering-local layout exception is closure capture
//! discovery: `.func` monotypes encode call signatures, not hidden environments,
//! so MirToLir still computes capture payload layouts for closures and then
//! hands those ordinary-data layouts back to the shared layout/RC machinery.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout = @import("layout");
const mir_mod = @import("mir");

const MIR = mir_mod.MIR;
const Monotype = mir_mod.Monotype;
const LambdaSet = mir_mod.LambdaSet;

const LIR = @import("LIR.zig");
const LirExprStore = @import("LirExprStore.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const StringLiteral = base.StringLiteral;

const LirExpr = LIR.LirExpr;
const LirExprId = LIR.LirExprId;
const LirExprSpan = LIR.LirExprSpan;
const LirPatternId = LIR.LirPatternId;
const LirPatternSpan = LIR.LirPatternSpan;
const LirStmt = LIR.LirStmt;
const LirMatchBranch = LIR.LirMatchBranch;
const LirCapture = LIR.LirCapture;
const Symbol = LIR.Symbol;
const Self = @This();

const DeferredListRestBinding = struct {
    source_symbol: Symbol,
    list_layout: layout.Idx,
    target_pattern: MIR.PatternId,
    discard_symbol: Symbol = Symbol.none,
    prefix_count: u32,
    suffix_count: u32,
};

const LoweredBindingPattern = struct {
    pattern: LirPatternId,
    deferred_rest_start: usize,
    deferred_rest_len: usize,
};

const TopLevelRestBindingRewrite = struct {
    source_pattern: LirPatternId,
    destructure_pattern: LirPatternId,
    source_symbol: Symbol,
    source_layout: layout.Idx,
};

const BindingOwnershipMode = enum {
    owned,
    borrowed,
};

const DirectCallSpecialization = struct {
    symbol: Symbol,
    param_layouts: []const layout.Idx,
    ret_layout: layout.Idx,
};

const SavedMonotypeLayout = struct {
    mono_key: u32,
    previous: ?layout.Idx,
};

allocator: Allocator,
mir_store: *const MIR.Store,
lir_store: *LirExprStore,
layout_store: *layout.Store,
lambda_set_store: *LambdaSet.Store,

/// Ident index for the `True` tag — needed to resolve Bool discriminants
/// (Bool lowers as the ordinary `[False, True]` tag union monotype).
true_tag: Ident.Idx,

/// Counter for generating unique synthetic symbols (used by ANF let-binding).
next_synthetic_id: u29 = 0,

/// Canonical resolver for ordinary MIR monotype layouts.
/// This is the only path ordinary MIR data should use to obtain layout ids.
monotype_layout_resolver: layout.MirMonotypeLayoutResolver,

/// Stable runtime captures payload layouts keyed by closure_member.
/// Closure captures remain the one lowering-local exception because `.func`
/// monotypes do not describe hidden environments.
capture_layout_cache: std.AutoHashMap(u32, layout.Idx),

/// Stable runtime closure value layouts keyed by lambda set.
closure_value_layout_cache: std.AutoHashMap(u32, layout.Idx),

/// Recursion guard for propagating symbol defs from MIR to LIR
propagating_defs: std.AutoHashMap(u64, void),

/// Maps symbol → layout for lambda parameters and let-bindings,
/// so captured variables can find their layout even when not in symbol_defs.
symbol_layouts: std.AutoHashMap(u64, layout.Idx),

/// Tracks whether the current binding for a symbol is owned or borrowed.
symbol_binding_modes: std.AutoHashMap(u64, BindingOwnershipMode),

/// Recursion guard for computing runtime lambda-set payload layouts.
computing_lambda_set_layouts: std.AutoHashMap(u32, void),

/// Recursion guard for computing runtime layouts of direct call results by
/// Cache of direct-call specializations keyed by semantic callee identity + runtime param layouts.
specialized_direct_callees: std.StringHashMap(DirectCallSpecialization),

/// Recursion guard for lowering specialized direct-call definitions.
specializing_direct_callees: std.AutoHashMap(u64, void),

/// Instantiated monotype -> runtime layout overrides active while lowering a
/// specialized direct callee.
specialized_monotype_layouts: std.AutoHashMap(u32, layout.Idx),

/// Specialized-environment variants of capture and closure layout caches.
specialized_capture_layout_cache: std.AutoHashMap(u32, layout.Idx),
specialized_closure_value_layout_cache: std.AutoHashMap(u32, layout.Idx),

/// Scratch buffer for ANF Let-binding accumulation
scratch_anf_stmts: std.ArrayList(LirStmt),

/// Scratch buffers for building spans
scratch_lir_expr_ids: std.ArrayList(LirExprId),
scratch_lir_pattern_ids: std.ArrayList(LirPatternId),
scratch_lir_stmts: std.ArrayList(LirStmt),
scratch_lir_match_branches: std.ArrayList(LirMatchBranch),
scratch_lir_captures: std.ArrayList(LirCapture),
scratch_deferred_list_rest_bindings: std.ArrayList(DeferredListRestBinding),

/// Scratch buffers for layout building (reused across layoutFrom* calls)
scratch_layouts: std.ArrayList(layout.Layout),
scratch_layout_idxs: std.ArrayList(layout.Idx),

/// Scratch buffer for building specialization cache keys.
scratch_specialization_key: std.ArrayList(u8),

pub fn init(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    lir_store: *LirExprStore,
    layout_store: *layout.Store,
    lambda_set_store: *LambdaSet.Store,
    true_tag: Ident.Idx,
) Self {
    return .{
        .allocator = allocator,
        .mir_store = mir_store,
        .lir_store = lir_store,
        .layout_store = layout_store,
        .lambda_set_store = lambda_set_store,
        .true_tag = true_tag,
        .monotype_layout_resolver = layout.MirMonotypeLayoutResolver.init(allocator, &mir_store.monotype_store, layout_store),
        .capture_layout_cache = std.AutoHashMap(u32, layout.Idx).init(allocator),
        .closure_value_layout_cache = std.AutoHashMap(u32, layout.Idx).init(allocator),
        .propagating_defs = std.AutoHashMap(u64, void).init(allocator),
        .symbol_layouts = std.AutoHashMap(u64, layout.Idx).init(allocator),
        .symbol_binding_modes = std.AutoHashMap(u64, BindingOwnershipMode).init(allocator),
        .computing_lambda_set_layouts = std.AutoHashMap(u32, void).init(allocator),
        .specialized_direct_callees = std.StringHashMap(DirectCallSpecialization).init(allocator),
        .specializing_direct_callees = std.AutoHashMap(u64, void).init(allocator),
        .specialized_monotype_layouts = std.AutoHashMap(u32, layout.Idx).init(allocator),
        .specialized_capture_layout_cache = std.AutoHashMap(u32, layout.Idx).init(allocator),
        .specialized_closure_value_layout_cache = std.AutoHashMap(u32, layout.Idx).init(allocator),
        .scratch_anf_stmts = std.ArrayList(LirStmt).empty,
        .scratch_lir_expr_ids = std.ArrayList(LirExprId).empty,
        .scratch_lir_pattern_ids = std.ArrayList(LirPatternId).empty,
        .scratch_lir_stmts = std.ArrayList(LirStmt).empty,
        .scratch_lir_match_branches = std.ArrayList(LirMatchBranch).empty,
        .scratch_lir_captures = std.ArrayList(LirCapture).empty,
        .scratch_deferred_list_rest_bindings = std.ArrayList(DeferredListRestBinding).empty,
        .scratch_layouts = std.ArrayList(layout.Layout).empty,
        .scratch_layout_idxs = std.ArrayList(layout.Idx).empty,
        .scratch_specialization_key = std.ArrayList(u8).empty,
    };
}

pub fn deinit(self: *Self) void {
    self.monotype_layout_resolver.deinit();
    self.capture_layout_cache.deinit();
    self.closure_value_layout_cache.deinit();
    self.propagating_defs.deinit();
    self.symbol_layouts.deinit();
    self.symbol_binding_modes.deinit();
    self.computing_lambda_set_layouts.deinit();
    {
        var it_vals = self.specialized_direct_callees.valueIterator();
        while (it_vals.next()) |value| {
            self.allocator.free(value.param_layouts);
        }
    }
    {
        var it = self.specialized_direct_callees.keyIterator();
        while (it.next()) |key_ptr| self.allocator.free(key_ptr.*);
    }
    self.specialized_direct_callees.deinit();
    self.specializing_direct_callees.deinit();
    self.specialized_monotype_layouts.deinit();
    self.specialized_capture_layout_cache.deinit();
    self.specialized_closure_value_layout_cache.deinit();
    self.scratch_anf_stmts.deinit(self.allocator);
    self.scratch_lir_expr_ids.deinit(self.allocator);
    self.scratch_lir_pattern_ids.deinit(self.allocator);
    self.scratch_lir_stmts.deinit(self.allocator);
    self.scratch_lir_match_branches.deinit(self.allocator);
    self.scratch_lir_captures.deinit(self.allocator);
    self.scratch_deferred_list_rest_bindings.deinit(self.allocator);
    self.scratch_layouts.deinit(self.allocator);
    self.scratch_layout_idxs.deinit(self.allocator);
    self.scratch_specialization_key.deinit(self.allocator);
}

/// Lower a MIR expression to a LIR expression.
pub fn lower(self: *Self, mir_expr_id: MIR.ExprId) Allocator.Error!LirExprId {
    const lowered = try self.lowerExpr(mir_expr_id);
    self.verifyFunctionLayouts(lowered);
    return lowered;
}

/// Copy a string literal from the source CIR module env to the LIR store.
/// Copy a string from MIR's string store into LIR's string store.
/// MIR already owns its own copy of all string data (copied from CIR
/// during MIR lowering), so this is a simple store-to-store transfer.
fn copyStringToLir(self: *Self, mir_str_idx: StringLiteral.Idx) Allocator.Error!StringLiteral.Idx {
    if (mir_str_idx == .none) return .none;
    const str_bytes = self.mir_store.getString(mir_str_idx);
    return self.lir_store.strings.insert(self.allocator, str_bytes);
}

/// Convert a Monotype.Idx to a layout.Idx, using a cache.
fn layoutFromMonotype(self: *Self, mono_idx: Monotype.Idx) Allocator.Error!layout.Idx {
    return self.monotype_layout_resolver.resolve(
        mono_idx,
        if (self.specialized_monotype_layouts.count() == 0) null else &self.specialized_monotype_layouts,
    );
}

fn saveMonotypeOverrideIfNeeded(
    self: *Self,
    saved: *std.ArrayList(SavedMonotypeLayout),
    mono_key: u32,
) Allocator.Error!void {
    for (saved.items) |entry| {
        if (entry.mono_key == mono_key) return;
    }

    try saved.append(self.allocator, .{
        .mono_key = mono_key,
        .previous = self.specialized_monotype_layouts.get(mono_key),
    });
}

fn restoreMonotypeOverrides(self: *Self, saved: []const SavedMonotypeLayout) void {
    var i = saved.len;
    while (i > 0) {
        i -= 1;
        const entry = saved[i];
        if (entry.previous) |layout_idx| {
            self.specialized_monotype_layouts.put(entry.mono_key, layout_idx) catch unreachable;
        } else {
            _ = self.specialized_monotype_layouts.remove(entry.mono_key);
        }
    }
    self.monotype_layout_resolver.clearOverrideCache();
    self.specialized_capture_layout_cache.clearRetainingCapacity();
    self.specialized_closure_value_layout_cache.clearRetainingCapacity();
}

fn registerSpecializedMonotypeLayout(
    self: *Self,
    mono_idx: Monotype.Idx,
    layout_idx: layout.Idx,
    saved: *std.ArrayList(SavedMonotypeLayout),
) Allocator.Error!void {
    if (mono_idx.isNone()) return;

    const mono_key = @intFromEnum(mono_idx);
    try self.saveMonotypeOverrideIfNeeded(saved, mono_key);
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);

    if (self.specialized_monotype_layouts.get(mono_key)) |existing| {
        if (existing == layout_idx) return;
        if (builtin.mode == .Debug) {
            const existing_layout = self.layout_store.getLayout(existing);
            const new_layout = self.layout_store.getLayout(layout_idx);
            std.debug.panic(
                "MirToLir specialized monotype layout mismatch: mono_idx={d} mono={any} existing={d}/{s} new={d}/{s}",
                .{
                    mono_key,
                    monotype,
                    @intFromEnum(existing),
                    @tagName(existing_layout.tag),
                    @intFromEnum(layout_idx),
                    @tagName(new_layout.tag),
                },
            );
        }
        return;
    }

    try self.specialized_monotype_layouts.put(mono_key, layout_idx);
    self.monotype_layout_resolver.clearOverrideCache();
    self.specialized_capture_layout_cache.clearRetainingCapacity();
    self.specialized_closure_value_layout_cache.clearRetainingCapacity();
    const layout_val = self.layout_store.getLayout(layout_idx);

    switch (monotype) {
        .recursive_placeholder, .unit, .prim => {},
        .func => {},
        .box => |b| {
            if (layout_val.tag == .box) {
                try self.registerSpecializedMonotypeLayout(b.inner, layout_val.data.box, saved);
            }
        },
        .list => |l| switch (layout_val.tag) {
            .list => try self.registerSpecializedMonotypeLayout(l.elem, layout_val.data.list, saved),
            .list_of_zst => try self.registerSpecializedMonotypeLayout(
                l.elem,
                try self.zeroSizedSpecializationLayoutFromMonotype(l.elem),
                saved,
            ),
            else => {},
        },
        .tuple => |t| {
            const elems = self.mir_store.monotype_store.getIdxSpan(t.elems);
            if (elems.len == 0) return;
            if (builtin.mode == .Debug and layout_val.tag != .struct_) {
                std.debug.panic(
                    "MirToLir invariant violated: non-empty tuple monotype must specialize to struct_ layout, got mono_idx={d} mono={any} layout_idx={d} tag={s}",
                    .{ mono_key, monotype, @intFromEnum(layout_idx), @tagName(layout_val.tag) },
                );
            }
            if (layout_val.tag != .struct_) return;

            const struct_data = self.layout_store.getStructData(layout_val.data.struct_.idx);
            const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
            for (elems, 0..) |elem_mono_idx, semantic_index| {
                for (0..layout_fields.len) |li| {
                    const layout_field = layout_fields.get(li);
                    if (layout_field.index != semantic_index) continue;
                    try self.registerSpecializedMonotypeLayout(elem_mono_idx, layout_field.layout, saved);
                    break;
                }
            }
        },
        .record => |r| {
            const fields = self.mir_store.monotype_store.getFields(r.fields);
            if (fields.len == 0) return;
            if (builtin.mode == .Debug and layout_val.tag != .struct_) {
                std.debug.panic(
                    "MirToLir invariant violated: non-empty record monotype must specialize to struct_ layout, got mono_idx={d} mono={any} layout_idx={d} tag={s}",
                    .{ mono_key, monotype, @intFromEnum(layout_idx), @tagName(layout_val.tag) },
                );
            }
            if (layout_val.tag != .struct_) return;

            const struct_data = self.layout_store.getStructData(layout_val.data.struct_.idx);
            const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
            for (fields, 0..) |field, semantic_index| {
                for (0..layout_fields.len) |li| {
                    const layout_field = layout_fields.get(li);
                    if (layout_field.index != semantic_index) continue;
                    try self.registerSpecializedMonotypeLayout(field.type_idx, layout_field.layout, saved);
                    break;
                }
            }
        },
        .tag_union => |tu| {
            const tags = self.mir_store.monotype_store.getTags(tu.tags);
            if (tags.len == 0) return;
            if (builtin.mode == .Debug and layout_val.tag != .tag_union) {
                std.debug.panic(
                    "MirToLir invariant violated: non-empty tag union monotype must specialize to tag_union layout, got mono_idx={d} mono={any} layout_idx={d} tag={s}",
                    .{ mono_key, monotype, @intFromEnum(layout_idx), @tagName(layout_val.tag) },
                );
            }
            if (layout_val.tag != .tag_union) return;

            const union_data = self.layout_store.getTagUnionData(layout_val.data.tag_union.idx);
            const union_layouts = self.layout_store.getTagUnionVariants(union_data);
            for (tags, 0..) |tag, i| {
                if (i >= union_layouts.len) break;
                const payload_layout_idx = union_layouts.get(i).payload_layout;
                const payloads = self.mir_store.monotype_store.getIdxSpan(tag.payloads);
                if (payloads.len == 0) continue;
                const payload_layout_val = self.layout_store.getLayout(payload_layout_idx);
                if (payload_layout_val.tag != .struct_) continue;
                const struct_data = self.layout_store.getStructData(payload_layout_val.data.struct_.idx);
                const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
                for (payloads, 0..) |payload_mono_idx, semantic_index| {
                    for (0..layout_fields.len) |li| {
                        const layout_field = layout_fields.get(li);
                        if (layout_field.index != semantic_index) continue;
                        try self.registerSpecializedMonotypeLayout(payload_mono_idx, layout_field.layout, saved);
                        break;
                    }
                }
            }
        },
    }
}

fn zeroSizedSpecializationLayoutFromMonotype(self: *Self, mono_idx: Monotype.Idx) Allocator.Error!layout.Idx {
    const canonical = try self.layoutFromMonotype(mono_idx);
    if (self.layout_store.layoutSize(self.layout_store.getLayout(canonical)) == 0) {
        return canonical;
    }
    return .zst;
}

fn isFunctionLayout(self: *Self, layout_idx: layout.Idx) bool {
    if (layout_idx == layout.Idx.none) return false;
    if (layout_idx == layout.Idx.named_fn) return true;
    const idx_int = @intFromEnum(layout_idx);
    if (idx_int >= self.layout_store.layouts.len()) unreachable;
    return self.layout_store.getLayout(layout_idx).tag == .closure;
}

fn isCallableExpr(self: *Self, expr_id: LirExprId, depth: u16) bool {
    if (depth > 256) std.debug.panic(
        "isCallableExpr: debug-only heuristic detected likely infinite recursion (depth > 256) at expr {}",
        .{@intFromEnum(expr_id)},
    );
    const expr = self.lir_store.getExpr(expr_id);
    return switch (expr) {
        .lambda => true,
        .nominal => |nom| self.isCallableExpr(nom.backing_expr, depth + 1),
        .block => |b| self.isCallableExpr(b.final_expr, depth + 1),
        .lookup => |lk| blk: {
            const def_id = self.lir_store.getSymbolDef(lk.symbol) orelse break :blk false;
            break :blk self.isCallableExpr(def_id, depth + 1);
        },
        else => false,
    };
}

fn verifyFunctionLayouts(self: *Self, _: LirExprId) void {
    var i: usize = 0;
    const count = self.lir_store.exprCount();
    while (i < count) : (i += 1) {
        const expr_id: LirExprId = @enumFromInt(@as(u32, @intCast(i)));
        const expr = self.lir_store.getExpr(expr_id);
        switch (expr) {
            .call => |c| {
                if (!self.isFunctionLayout(c.fn_layout)) {
                    std.debug.panic("MirToLir invariant violated: non-callable call.fn_layout at expr {}", .{i});
                }
            },
            .lambda => |lam| {
                if (!self.isFunctionLayout(lam.fn_layout)) {
                    std.debug.panic("MirToLir invariant violated: non-callable lambda.fn_layout at expr {}", .{i});
                }
            },
            .lookup => {},
            else => {},
        }
    }
}

fn runtimeTupleLayoutFromExprs(self: *Self, mir_expr_ids: []const MIR.ExprId) Allocator.Error!layout.Idx {
    if (mir_expr_ids.len == 0) return .zst;

    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);

    for (mir_expr_ids) |mir_expr_id| {
        const elem_layout_idx = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
        try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(elem_layout_idx));
    }

    return self.layout_store.putTuple(self.scratch_layouts.items[save_layouts..]);
}

fn layoutFromPayloadMonotypes(self: *Self, payloads: []const Monotype.Idx) Allocator.Error!layout.Idx {
    if (payloads.len == 0) return .zst;

    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);

    for (payloads) |payload_mono| {
        const payload_layout_idx = try self.layoutFromMonotype(payload_mono);
        try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(payload_layout_idx));
    }

    return self.layout_store.putTuple(self.scratch_layouts.items[save_layouts..]);
}

fn runtimeListLayoutFromExprs(
    self: *Self,
    elem_mono_idx: Monotype.Idx,
    elem_exprs: []const MIR.ExprId,
) Allocator.Error!layout.Idx {
    if (elem_exprs.len == 0) {
        return self.layout_store.insertList(try self.layoutFromMonotype(elem_mono_idx));
    }

    const elem_layout_idx = try self.runtimeValueLayoutFromMirExpr(elem_exprs[0]);
    return self.layout_store.insertList(elem_layout_idx);
}

fn tagPayloadMonotypes(self: *Self, union_mono_idx: Monotype.Idx, tag_name: Ident.Idx) []const Monotype.Idx {
    const union_mono = self.mir_store.monotype_store.getMonotype(union_mono_idx);
    const tags = switch (union_mono) {
        .tag_union => |tu| self.mir_store.monotype_store.getTags(tu.tags),
        else => unreachable,
    };

    for (tags) |tag| {
        if (self.identsTextEqual(tag.name, tag_name)) {
            return self.mir_store.monotype_store.getIdxSpan(tag.payloads);
        }
    }

    return &.{};
}

fn tagPayloadExprs(self: *Self, union_mono_idx: Monotype.Idx, tag_name: Ident.Idx, args: MIR.ExprSpan) []const MIR.ExprId {
    const outer_args = self.mir_store.getExprSpan(args);
    const payloads = self.tagPayloadMonotypes(union_mono_idx, tag_name);
    if (payloads.len > 1 and outer_args.len == 1) {
        switch (self.mir_store.getExpr(outer_args[0])) {
            .struct_ => |struct_| return self.mir_store.getExprSpan(struct_.fields),
            else => {},
        }
    }
    return outer_args;
}

fn tagPayloadPatterns(self: *Self, union_mono_idx: Monotype.Idx, tag_name: Ident.Idx, args: MIR.PatternSpan) []const MIR.PatternId {
    const outer_args = self.mir_store.getPatternSpan(args);
    const payloads = self.tagPayloadMonotypes(union_mono_idx, tag_name);
    if (payloads.len > 1 and outer_args.len == 1) {
        switch (self.mir_store.getPattern(outer_args[0])) {
            .struct_destructure => |struct_| return self.mir_store.getPatternSpan(struct_.fields),
            else => {},
        }
    }
    return outer_args;
}

fn runtimeTagLayoutFromExpr(
    self: *Self,
    tag_data: anytype,
    union_mono_idx: Monotype.Idx,
) Allocator.Error!layout.Idx {
    if (self.specialized_monotype_layouts.get(@intFromEnum(union_mono_idx))) |layout_idx| {
        return layout_idx;
    }

    const tags = switch (self.mir_store.monotype_store.getMonotype(union_mono_idx)) {
        .tag_union => |tu| self.mir_store.monotype_store.getTags(tu.tags),
        else => unreachable,
    };
    const mir_args = self.tagPayloadExprs(union_mono_idx, tag_data.name, tag_data.args);

    if (tags.len == 0) return .zst;

    const zst_idx = try self.layout_store.ensureZstLayout();
    const save_idxs = self.scratch_layout_idxs.items.len;
    defer self.scratch_layout_idxs.shrinkRetainingCapacity(save_idxs);

    var found_active = false;
    for (tags) |tag| {
        if (self.identsTextEqual(tag.name, tag_data.name)) {
            found_active = true;
            if (mir_args.len == 0) {
                try self.scratch_layout_idxs.append(self.allocator, zst_idx);
            } else {
                try self.scratch_layout_idxs.append(self.allocator, try self.runtimeTupleLayoutFromExprs(mir_args));
            }
            continue;
        }

        const payloads = self.mir_store.monotype_store.getIdxSpan(tag.payloads);
        if (payloads.len == 0) {
            try self.scratch_layout_idxs.append(self.allocator, zst_idx);
        } else {
            try self.scratch_layout_idxs.append(self.allocator, try self.layoutFromPayloadMonotypes(payloads));
        }
    }

    if (builtin.mode == .Debug and !found_active) {
        std.debug.panic(
            "MirToLir invariant violated: active tag ident idx {d} missing from tag union mono_idx={d}",
            .{ tag_data.name.idx, @intFromEnum(union_mono_idx) },
        );
    }

    return self.layout_store.putTagUnion(self.scratch_layout_idxs.items[save_idxs..]);
}

fn runtimeRecordLayoutFromExprs(
    self: *Self,
    field_exprs: []const MIR.ExprId,
) Allocator.Error!layout.Idx {
    if (field_exprs.len == 0) return self.layout_store.getEmptyRecordLayout();

    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);

    for (field_exprs) |field_expr_id| {
        const field_layout_idx = try self.runtimeValueLayoutFromMirExpr(field_expr_id);
        try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(field_layout_idx));
    }

    return self.layout_store.putRecord(self.scratch_layouts.items[save_layouts..]);
}

fn runtimeLayoutFromPattern(self: *Self, mir_pat_id: MIR.PatternId) Allocator.Error!layout.Idx {
    const pat = self.mir_store.getPattern(mir_pat_id);
    const mono_idx = self.mir_store.patternTypeOf(mir_pat_id);

    return switch (pat) {
        .bind => |sym| blk: {
            const mono = self.mir_store.monotype_store.getMonotype(mono_idx);
            if (mono == .func) {
                if (self.lambda_set_store.getSymbolLambdaSet(sym)) |ls_idx| {
                    break :blk try self.closureValueLayoutFromLambdaSet(ls_idx);
                }
            }
            break :blk try self.layoutFromMonotype(mono_idx);
        },
        .wildcard,
        .int_literal,
        .str_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .runtime_error,
        => self.layoutFromMonotype(mono_idx),
        .as_pattern => |ap| self.runtimeLayoutFromPattern(ap.pattern),
        .struct_destructure => |sd| switch (self.mir_store.monotype_store.getMonotype(mono_idx)) {
            .record => self.runtimeRecordLayoutFromPattern(mono_idx, self.mir_store.getPatternSpan(sd.fields)),
            .tuple => self.runtimeTupleLayoutFromPatternSpan(self.mir_store.getPatternSpan(sd.fields)),
            else => self.layoutFromMonotype(mono_idx),
        },
        .tag,
        .list_destructure,
        => self.layoutFromMonotype(mono_idx),
    };
}

fn runtimeTupleLayoutFromPatternSpan(self: *Self, mir_pat_ids: []const MIR.PatternId) Allocator.Error!layout.Idx {
    if (mir_pat_ids.len == 0) return .zst;

    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);

    for (mir_pat_ids) |mir_pat_id| {
        const pat_layout_idx = try self.runtimeLayoutFromPattern(mir_pat_id);
        try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(pat_layout_idx));
    }

    return self.layout_store.putTuple(self.scratch_layouts.items[save_layouts..]);
}

fn runtimeRecordLayoutFromPattern(
    self: *Self,
    mono_idx: Monotype.Idx,
    mir_patterns: []const MIR.PatternId,
) Allocator.Error!layout.Idx {
    const mono = self.mir_store.monotype_store.getMonotype(mono_idx);
    const record = switch (mono) {
        .record => |r| r,
        else => return self.layoutFromMonotype(mono_idx),
    };

    const all_fields = self.mir_store.monotype_store.getFields(record.fields);
    if (all_fields.len == 0) return try self.layout_store.getEmptyRecordLayout();

    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);
    for (all_fields, 0..) |field, field_idx| {
        const field_layout_idx = if (field_idx < mir_patterns.len)
            try self.runtimeLayoutFromPattern(mir_patterns[field_idx])
        else
            try self.layoutFromMonotype(field.type_idx);
        try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(field_layout_idx));
    }

    return self.layout_store.putRecord(self.scratch_layouts.items[save_layouts..]);
}

fn capturesLayoutForMember(self: *Self, member: LambdaSet.Member) Allocator.Error!layout.Idx {
    if (member.closure_member.isNone()) return .zst;

    const cache_key = @intFromEnum(member.closure_member);
    if (self.specialized_monotype_layouts.count() != 0) {
        if (self.specialized_capture_layout_cache.get(cache_key)) |cached| return cached;
    } else {
        if (self.capture_layout_cache.get(cache_key)) |cached| return cached;
    }

    const closure_member = self.mir_store.getClosureMember(member.closure_member);
    const capture_bindings = self.mir_store.getCaptureBindings(closure_member.capture_bindings);
    if (capture_bindings.len == 0) return .zst;

    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);

    for (capture_bindings) |binding| {
        const field_layout_idx = try self.runtimeValueLayoutFromMirExpr(binding.source_expr);
        try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(field_layout_idx));
    }
    const captures_layout = try self.layout_store.putTuple(self.scratch_layouts.items[save_layouts..]);
    if (self.specialized_monotype_layouts.count() != 0) {
        try self.specialized_capture_layout_cache.put(cache_key, captures_layout);
    } else {
        try self.capture_layout_cache.put(cache_key, captures_layout);
    }
    return captures_layout;
}

/// Compute the runtime value layout for a lambda set.
/// Single-member sets use the captures payload layout directly.
/// Multi-member sets use a tag union over per-member captures payload layouts.
fn closureValueLayoutFromLambdaSet(self: *Self, ls_idx: LambdaSet.Idx) Allocator.Error!layout.Idx {
    const ls_key = @intFromEnum(ls_idx);
    if (self.specialized_monotype_layouts.count() != 0) {
        if (self.specialized_closure_value_layout_cache.get(ls_key)) |cached| return cached;
    } else {
        if (self.closure_value_layout_cache.get(ls_key)) |cached| return cached;
    }

    if (self.computing_lambda_set_layouts.contains(ls_key)) {
        const ls_fallback = self.lambda_set_store.getLambdaSet(ls_idx);
        const members_fallback = self.lambda_set_store.getMembers(ls_fallback.members);
        if (members_fallback.len == 1 and !members_fallback[0].closure_member.isNone()) {
            const closure_member = self.mir_store.getClosureMember(members_fallback[0].closure_member);
            const bindings = self.mir_store.getCaptureBindings(closure_member.capture_bindings);
            if (bindings.len == 0) return .zst;

            const save_layouts = self.scratch_layouts.items.len;
            defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);
            for (bindings) |binding| {
                const fallback_layout = try self.layoutFromMonotype(binding.monotype);
                try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(fallback_layout));
            }
            return self.layout_store.putTuple(self.scratch_layouts.items[save_layouts..]);
        }
    }

    try self.computing_lambda_set_layouts.put(ls_key, {});
    defer _ = self.computing_lambda_set_layouts.remove(ls_key);

    const ls = self.lambda_set_store.getLambdaSet(ls_idx);
    const members = self.lambda_set_store.getMembers(ls.members);
    if (members.len == 0) unreachable;

    if (members.len == 1) {
        const member = members[0];
        const closure_layout = try self.capturesLayoutForMember(member);
        if (self.specialized_monotype_layouts.count() != 0) {
            try self.specialized_closure_value_layout_cache.put(ls_key, closure_layout);
        } else {
            try self.closure_value_layout_cache.put(ls_key, closure_layout);
        }
        return closure_layout;
    }

    const save = self.scratch_layout_idxs.items.len;
    defer self.scratch_layout_idxs.shrinkRetainingCapacity(save);
    for (members) |member| {
        const payload_layout = try self.capturesLayoutForMember(member);
        try self.scratch_layout_idxs.append(self.allocator, payload_layout);
    }
    const closure_layout = try self.layout_store.putTagUnion(self.scratch_layout_idxs.items[save..]);
    if (self.specialized_monotype_layouts.count() != 0) {
        try self.specialized_closure_value_layout_cache.put(ls_key, closure_layout);
    } else {
        try self.closure_value_layout_cache.put(ls_key, closure_layout);
    }
    return closure_layout;
}

fn closureVariantPayloadLayout(
    self: *Self,
    closure_layout: layout.Idx,
    discriminant: usize,
) Allocator.Error!layout.Idx {
    const closure_layout_val = self.layout_store.getLayout(closure_layout);
    if (builtin.mode == .Debug and closure_layout_val.tag != .tag_union) {
        std.debug.panic(
            "MirToLir invariant violated: expected tag_union closure layout, got {s}",
            .{@tagName(closure_layout_val.tag)},
        );
    }

    const union_data = self.layout_store.getTagUnionData(closure_layout_val.data.tag_union.idx);
    const variants = self.layout_store.getTagUnionVariants(union_data);
    if (builtin.mode == .Debug and discriminant >= variants.len) {
        std.debug.panic(
            "MirToLir invariant violated: closure discriminant {d} out of bounds for layout {d} with {d} variants",
            .{ discriminant, @intFromEnum(closure_layout), variants.len },
        );
    }

    return variants.get(discriminant).payload_layout;
}

fn lambdaSetForExpr(self: *Self, mir_expr_id: MIR.ExprId) ?LambdaSet.Idx {
    if (self.lambda_set_store.getExprLambdaSet(mir_expr_id)) |ls_idx| return ls_idx;

    return switch (self.mir_store.getExpr(mir_expr_id)) {
        .lookup => |sym| blk: {
            if (self.lambda_set_store.getSymbolLambdaSet(sym)) |ls_idx| break :blk ls_idx;
            const def_expr = self.mir_store.getSymbolDef(sym) orelse break :blk null;
            break :blk self.lambdaSetForExpr(def_expr);
        },
        .block => |block| self.lambdaSetForExpr(block.final_expr),
        .borrow_scope => |scope| self.lambdaSetForExpr(scope.body),
        .struct_access => |sa| self.lambdaSetForStructField(sa.struct_, sa.field_idx),
        .dbg_expr => |dbg_expr| self.lambdaSetForExpr(dbg_expr.expr),
        .expect => |expect| self.lambdaSetForExpr(expect.body),
        .return_expr => |ret| self.lambdaSetForExpr(ret.expr),
        else => null,
    };
}

fn lambdaSetForStructField(self: *Self, expr_id: MIR.ExprId, field_idx: u32) ?LambdaSet.Idx {
    return switch (self.mir_store.getExpr(expr_id)) {
        .struct_ => |struct_| blk: {
            const fields = self.mir_store.getExprSpan(struct_.fields);
            if (field_idx >= fields.len) break :blk null;
            break :blk self.lambdaSetForExpr(fields[field_idx]);
        },
        .lookup => |symbol| blk: {
            const def_expr = self.mir_store.getSymbolDef(symbol) orelse break :blk null;
            break :blk self.lambdaSetForStructField(def_expr, field_idx);
        },
        .block => |block| self.lambdaSetForStructField(block.final_expr, field_idx),
        else => null,
    };
}

fn runtimeClosureDispatchLayoutForExpr(
    self: *Self,
    expr_id: MIR.ExprId,
    ls_idx: LambdaSet.Idx,
) Allocator.Error!layout.Idx {
    const mono_idx = self.mir_store.typeOf(expr_id);
    const candidate = switch (self.mir_store.getExpr(expr_id)) {
        .lookup => |sym| blk: {
            if (self.symbol_layouts.get(sym.raw())) |layout_idx| {
                break :blk try self.runtimeLayoutForBindingSymbol(sym, mono_idx, layout_idx);
            }
            break :blk try self.closureValueLayoutFromLambdaSet(ls_idx);
        },
        .block => |block| blk: {
            if (self.lambdaSetForExpr(block.final_expr)) |inner_ls_idx| {
                break :blk try self.runtimeClosureDispatchLayoutForExpr(block.final_expr, inner_ls_idx);
            }
            break :blk try self.closureValueLayoutFromLambdaSet(ls_idx);
        },
        .struct_access => |sa| blk: {
            if (try self.runtimeLayoutForStructField(sa.struct_, sa.field_idx)) |layout_idx| {
                break :blk layout_idx;
            }
            break :blk try self.closureValueLayoutFromLambdaSet(ls_idx);
        },
        else => try self.closureValueLayoutFromLambdaSet(ls_idx),
    };

    const members = self.lambda_set_store.getMembers(self.lambda_set_store.getLambdaSet(ls_idx).members);
    if (members.len > 1 and self.layout_store.getLayout(candidate).tag != .tag_union) {
        return self.closureValueLayoutFromLambdaSet(ls_idx);
    }

    return candidate;
}

fn runtimeLayoutForStructField(self: *Self, expr_id: MIR.ExprId, field_idx: u32) Allocator.Error!?layout.Idx {
    return switch (self.mir_store.getExpr(expr_id)) {
        .struct_ => |struct_| blk: {
            const fields = self.mir_store.getExprSpan(struct_.fields);
            if (field_idx >= fields.len) break :blk null;
            break :blk try self.runtimeValueLayoutFromMirExpr(fields[field_idx]);
        },
        .lookup => |symbol| blk: {
            if (self.symbol_layouts.get(symbol.raw())) |struct_layout| {
                const struct_layout_val = self.layout_store.getLayout(struct_layout);
                const struct_mono = self.mir_store.monotype_store.getMonotype(self.mir_store.typeOf(expr_id));
                switch (struct_mono) {
                    .record, .tuple => {},
                    else => {},
                }
                if (struct_layout_val.tag == .struct_) {
                    const field_info = self.structFieldInfoByOriginalIndex(struct_layout, field_idx) orelse break :blk null;
                    break :blk field_info.field_layout;
                }
            }
            const def_expr = self.mir_store.getSymbolDef(symbol) orelse break :blk null;
            break :blk try self.runtimeLayoutForStructField(def_expr, field_idx);
        },
        .block => |block| try self.runtimeLayoutForStructField(block.final_expr, field_idx),
        else => null,
    };
}

/// Compute the runtime value layout for a MIR expression.
/// Function-typed values use lambda-set runtime layouts rather than the generic
/// function monotype layout, because lifted closures are represented by their
/// captures payloads (or a tag union over payloads), not by callable code pointers.
fn runtimeValueLayoutFromMirExpr(self: *Self, mir_expr_id: MIR.ExprId) Allocator.Error!layout.Idx {
    const mono_idx = self.mir_store.typeOf(mir_expr_id);
    const mono = self.mir_store.monotype_store.getMonotype(mono_idx);
    const expr = self.mir_store.getExpr(mir_expr_id);
    if (mono == .func) {
        switch (expr) {
            .block => |block| return self.runtimeLayoutForBlockFinal(block),
            .lookup => |sym| {
                if (self.symbol_layouts.get(sym.raw())) |layout_idx| {
                    return try self.runtimeLayoutForBindingSymbol(sym, mono_idx, layout_idx);
                }
                if (self.lambda_set_store.getSymbolLambdaSet(sym)) |ls_idx| {
                    return self.closureValueLayoutFromLambdaSet(ls_idx);
                }
                if (self.mir_store.getSymbolDef(sym)) |def_expr_id| {
                    return self.runtimeValueLayoutFromMirExpr(def_expr_id);
                }
            },
            else => {},
        }
        if (expr == .call) {
            if (try self.runtimeLayoutFromSpecializedDirectCall(expr.call.func, self.mir_store.getExprSpan(expr.call.args))) |layout_idx| {
                return layout_idx;
            }
        }
        if (self.mir_store.getExprClosureMember(mir_expr_id)) |closure_member_id| {
            const closure_member = self.mir_store.getClosureMember(closure_member_id);
            return self.capturesLayoutForMember(.{
                .fn_symbol = closure_member.fn_symbol,
                .closure_member = closure_member_id,
            });
        }
        if (self.lambdaSetForExpr(mir_expr_id)) |ls_idx| {
            return self.closureValueLayoutFromLambdaSet(ls_idx);
        }
        switch (self.mir_store.getExpr(mir_expr_id)) {
            .struct_access => |sa| {
                if (try self.runtimeLayoutForStructField(sa.struct_, sa.field_idx)) |layout_idx| return layout_idx;
            },
            .lambda, .hosted => return .zst,
            else => {},
        }
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "MirToLir: missing eager lambda set for function expression {} (expr={s})",
                .{ @intFromEnum(mir_expr_id), @tagName(expr) },
            );
        }
        unreachable;
    }

    switch (expr) {
        .call => |call_data| {
            const func_expr = self.mir_store.getExpr(call_data.func);
            if (func_expr == .lookup) {
                if (self.mir_store.getSymbolDef(func_expr.lookup)) |def_expr_id| {
                    if (self.mir_store.getExpr(def_expr_id) == .runtime_err_anno_only) {
                        if (try self.annotationOnlyIntrinsicForFunc(self.mir_store.typeOf(call_data.func))) |intrinsic| {
                            return self.layoutFromMonotype(intrinsic.result_mono);
                        }
                    }
                }
            }
            if (try self.runtimeLayoutFromSpecializedDirectCall(call_data.func, self.mir_store.getExprSpan(call_data.args))) |layout_idx| {
                return layout_idx;
            }
        },
        .list => |list_data| {
            if (!(try self.monotypeContainsFunctionValue(mono_idx))) {
                return self.layoutFromMonotype(mono_idx);
            }
            return self.runtimeListLayoutFromExprs(
                mono.list.elem,
                self.mir_store.getExprSpan(list_data.elems),
            );
        },
        .tag => |tag_data| {
            if (mono == .tag_union) {
                if (!(try self.monotypeContainsFunctionValue(mono_idx))) {
                    return self.layoutFromMonotype(mono_idx);
                }
                return self.runtimeTagLayoutFromExpr(tag_data, mono_idx);
            }
            return self.layoutFromMonotype(mono_idx);
        },
        .run_low_level => |ll| {
            if (ll.op == .list_get_unsafe) {
                if (self.specialized_monotype_layouts.get(@intFromEnum(mono_idx))) |layout_idx| {
                    return layout_idx;
                }
                const args = self.mir_store.getExprSpan(ll.args);
                if (builtin.mode == .Debug and args.len == 0) {
                    std.debug.panic("MirToLir invariant violated: list_get_unsafe missing list argument", .{});
                }
                return self.runtimeListElemLayoutFromMirExpr(args[0]);
            }
        },
        .struct_ => |struct_| switch (mono) {
            .tuple => {
                if (!(try self.monotypeContainsFunctionValue(mono_idx))) {
                    return self.layoutFromMonotype(mono_idx);
                }
                return self.runtimeTupleLayoutFromExprs(self.mir_store.getExprSpan(struct_.fields));
            },
            .record => {
                if (!(try self.monotypeContainsFunctionValue(mono_idx))) {
                    return self.layoutFromMonotype(mono_idx);
                }
                return self.runtimeRecordLayoutFromExprs(self.mir_store.getExprSpan(struct_.fields));
            },
            else => {},
        },
        .lookup => |sym| {
            if (self.symbol_layouts.get(sym.raw())) |layout_idx| {
                return try self.runtimeLayoutForBindingSymbol(sym, mono_idx, layout_idx);
            }
            if (self.mir_store.getSymbolDef(sym)) |def_expr_id| {
                return self.runtimeValueLayoutFromMirExpr(def_expr_id);
            }
            return self.layoutFromMonotype(mono_idx);
        },
        .struct_access => |sa| {
            if (try self.runtimeLayoutForStructField(sa.struct_, sa.field_idx)) |layout_idx| {
                return layout_idx;
            }
        },
        .block => |block| return self.runtimeLayoutForBlockFinal(block),
        else => {},
    }

    return self.layoutFromMonotype(mono_idx);
}

fn appendUniquePatternSymbolKey(self: *Self, out: *std.ArrayList(u64), sym: Symbol) Allocator.Error!void {
    const sym_key: u64 = @bitCast(sym);
    for (out.items) |existing| {
        if (existing == sym_key) return;
    }
    try out.append(self.allocator, sym_key);
}

fn collectPatternBindingSymbolKeys(
    self: *Self,
    mir_pat_id: MIR.PatternId,
    out: *std.ArrayList(u64),
) Allocator.Error!void {
    const pat = self.mir_store.getPattern(mir_pat_id);
    switch (pat) {
        .bind => |sym| try self.appendUniquePatternSymbolKey(out, sym),
        .wildcard,
        .int_literal,
        .str_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .runtime_error,
        => {},
        .as_pattern => |as_pat| {
            try self.appendUniquePatternSymbolKey(out, as_pat.symbol);
            try self.collectPatternBindingSymbolKeys(as_pat.pattern, out);
        },
        .tag => |tag_pat| {
            for (self.mir_store.getPatternSpan(tag_pat.args)) |arg_pattern_id| {
                try self.collectPatternBindingSymbolKeys(arg_pattern_id, out);
            }
        },
        .struct_destructure => |sd| {
            for (self.mir_store.getPatternSpan(sd.fields)) |field_pattern_id| {
                try self.collectPatternBindingSymbolKeys(field_pattern_id, out);
            }
        },
        .list_destructure => |ld| {
            for (self.mir_store.getPatternSpan(ld.patterns)) |elem_pattern_id| {
                try self.collectPatternBindingSymbolKeys(elem_pattern_id, out);
            }
            if (!ld.rest_pattern.isNone()) {
                try self.collectPatternBindingSymbolKeys(ld.rest_pattern, out);
            }
        },
    }
}

fn resolveToLambda(self: *Self, expr_id: MIR.ExprId) ?struct { params: MIR.PatternSpan, body: MIR.ExprId } {
    const expr = self.mir_store.getExpr(expr_id);
    return switch (expr) {
        .lambda => |lam| .{ .params = lam.params, .body = lam.body },
        .block => |block| self.resolveToLambda(block.final_expr),
        .lookup => |sym| blk: {
            const def_expr_id = self.mir_store.getSymbolDef(sym) orelse break :blk null;
            break :blk self.resolveToLambda(def_expr_id);
        },
        else => null,
    };
}

fn resolveToLambdaExprId(self: *Self, expr_id: MIR.ExprId) ?MIR.ExprId {
    const expr = self.mir_store.getExpr(expr_id);
    return switch (expr) {
        .lambda => expr_id,
        .block => |block| self.resolveToLambdaExprId(block.final_expr),
        .lookup => |sym| blk: {
            const def_expr_id = self.mir_store.getSymbolDef(sym) orelse break :blk null;
            break :blk self.resolveToLambdaExprId(def_expr_id);
        },
        else => null,
    };
}

fn specializationKeyBytes(self: *Self, callee_key: u64, param_layouts: []const layout.Idx) Allocator.Error![]const u8 {
    self.scratch_specialization_key.clearRetainingCapacity();

    try self.scratch_specialization_key.appendSlice(self.allocator, std.mem.asBytes(&callee_key));

    for (param_layouts) |layout_idx| {
        const raw_layout: u32 = @intCast(@intFromEnum(layout_idx));
        try self.scratch_specialization_key.appendSlice(self.allocator, std.mem.asBytes(&raw_layout));
    }

    return self.scratch_specialization_key.items;
}

fn ensureSpecializedDirectCallee(
    self: *Self,
    callee_key: u64,
    original_fn_symbol: Symbol,
    lifted_def: MIR.ExprId,
    param_layouts: []const layout.Idx,
) Allocator.Error!DirectCallSpecialization {
    const key_bytes = try self.specializationKeyBytes(callee_key, param_layouts);
    var specialization = self.specialized_direct_callees.get(key_bytes);

    if (specialization == null) {
        const owned_key = try self.allocator.dupe(u8, key_bytes);
        errdefer self.allocator.free(owned_key);
        const owned_param_layouts = try self.allocator.dupe(layout.Idx, param_layouts);
        errdefer self.allocator.free(owned_param_layouts);

        const fresh_symbol = self.freshSymbol();
        try self.specialized_direct_callees.put(owned_key, .{
            .symbol = fresh_symbol,
            .param_layouts = owned_param_layouts,
            .ret_layout = .none,
        });
        specialization = self.specialized_direct_callees.get(owned_key).?;
    }

    const resolved_symbol = specialization.?.symbol;
    if (self.lir_store.getSymbolDef(resolved_symbol) != null) return specialization.?;
    if (self.specializing_direct_callees.contains(resolved_symbol.raw())) return specialization.?;

    const lambda_expr_id = self.resolveToLambdaExprId(lifted_def) orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "MirToLir invariant violated: direct-call specialization target is not a lambda expr for key {d}",
                .{callee_key},
            );
        }
        unreachable;
    };

    try self.specializing_direct_callees.put(resolved_symbol.raw(), {});
    defer _ = self.specializing_direct_callees.remove(resolved_symbol.raw());

    if (!original_fn_symbol.isNone()) {
        try self.prepareLiftedDefCaptureLayout(original_fn_symbol, lifted_def);
    }

    const inferred_ret_layout = try self.runtimeLayoutForLambdaBodyWithParamLayouts(
        lifted_def,
        specialization.?.param_layouts,
    );
    if (builtin.mode == .Debug and inferred_ret_layout == null) {
        std.debug.panic(
            "MirToLir invariant violated: could not infer specialized return layout for direct callee key {d}",
            .{callee_key},
        );
    }
    const specialization_ret_layout = inferred_ret_layout orelse unreachable;

    const refreshed_key_before_lower = try self.specializationKeyBytes(callee_key, param_layouts);
    if (self.specialized_direct_callees.getPtr(refreshed_key_before_lower)) |entry| {
        entry.ret_layout = specialization_ret_layout;
    } else unreachable;

    const lambda_expr = self.mir_store.getExpr(lambda_expr_id);
    if (builtin.mode == .Debug and lambda_expr != .lambda) {
        std.debug.panic(
            "MirToLir invariant violated: resolved specialized direct-call expr {} is not lambda",
            .{@intFromEnum(lambda_expr_id)},
        );
    }
    const lir_def = try self.lowerLambdaWithParamLayouts(
        lambda_expr.lambda,
        self.mir_store.typeOf(lambda_expr_id),
        specialization.?.param_layouts,
        Region.zero(),
    );
    try self.lir_store.registerSymbolDef(resolved_symbol, lir_def);

    const lowered = self.lir_store.getExpr(lir_def);
    const lowered_ret_layout = switch (lowered) {
        .lambda => |lam| lam.ret_layout,
        .nominal => |nom| blk: {
            const inner = self.lir_store.getExpr(nom.backing_expr);
            if (inner == .lambda) break :blk inner.lambda.ret_layout;
            break :blk specialization_ret_layout;
        },
        else => specialization_ret_layout,
    };

    const refreshed_key_bytes = try self.specializationKeyBytes(callee_key, param_layouts);
    if (self.specialized_direct_callees.getPtr(refreshed_key_bytes)) |entry| {
        entry.ret_layout = lowered_ret_layout;
        return entry.*;
    }
    unreachable;
}

fn runtimeLayoutForLambdaBodyWithParamLayouts(
    self: *Self,
    callee_expr_id: MIR.ExprId,
    param_layouts: []const layout.Idx,
) Allocator.Error!?layout.Idx {
    const resolved = self.resolveToLambda(callee_expr_id) orelse return null;
    const params = self.mir_store.getPatternSpan(resolved.params);
    if (params.len != param_layouts.len) return null;
    const fn_mono_idx = self.mir_store.typeOf(callee_expr_id);
    const fn_mono = self.mir_store.monotype_store.getMonotype(fn_mono_idx);
    const func_args = switch (fn_mono) {
        .func => |f| self.mir_store.monotype_store.getIdxSpan(f.args),
        else => return null,
    };
    if (func_args.len != param_layouts.len) return null;

    var saved_layouts = std.ArrayList(struct {
        sym_key: u64,
        previous: ?layout.Idx,
    }).empty;
    defer saved_layouts.deinit(self.allocator);
    var saved_monotype_layouts = std.ArrayList(SavedMonotypeLayout).empty;
    defer saved_monotype_layouts.deinit(self.allocator);

    var bound_symbols = std.ArrayList(u64).empty;
    defer bound_symbols.deinit(self.allocator);
    for (params) |param_pat_id| {
        try self.collectPatternBindingSymbolKeys(param_pat_id, &bound_symbols);
    }

    for (bound_symbols.items) |sym_key| {
        try saved_layouts.append(self.allocator, .{
            .sym_key = sym_key,
            .previous = self.symbol_layouts.get(sym_key),
        });
        _ = self.symbol_layouts.remove(sym_key);
    }
    defer {
        for (saved_layouts.items) |saved| {
            if (saved.previous) |layout_idx| {
                self.symbol_layouts.put(saved.sym_key, layout_idx) catch unreachable;
            } else {
                _ = self.symbol_layouts.remove(saved.sym_key);
            }
        }
    }

    for (params, param_layouts) |param_pat_id, param_layout| {
        try self.registerBindingPatternSymbols(param_pat_id, param_layout);
    }
    for (func_args, param_layouts) |param_mono_idx, param_layout| {
        try self.registerSpecializedMonotypeLayout(param_mono_idx, param_layout, &saved_monotype_layouts);
    }
    defer self.restoreMonotypeOverrides(saved_monotype_layouts.items);

    const inferred = try self.runtimeValueLayoutFromMirExpr(resolved.body);
    return inferred;
}

fn runtimeLayoutForBlockFinal(self: *Self, block: anytype) Allocator.Error!layout.Idx {
    const SavedSymbolLayout = struct {
        sym_key: u64,
        previous: ?layout.Idx,
    };

    var saved_layouts = std.ArrayList(SavedSymbolLayout).empty;
    defer saved_layouts.deinit(self.allocator);

    const mir_stmts = self.mir_store.getStmts(block.stmts);
    for (mir_stmts) |stmt| {
        const binding = switch (stmt) {
            .decl_const, .decl_var, .mutate_var => |b| b,
        };
        const runtime_layout = try self.runtimeValueLayoutFromMirExpr(binding.expr);
        switch (stmt) {
            .decl_const => {
                var bound_symbols = std.ArrayList(u64).empty;
                defer bound_symbols.deinit(self.allocator);
                try self.collectPatternBindingSymbolKeys(binding.pattern, &bound_symbols);
                for (bound_symbols.items) |sym_key| {
                    try saved_layouts.append(self.allocator, .{
                        .sym_key = sym_key,
                        .previous = self.symbol_layouts.get(sym_key),
                    });
                }
                try self.registerBindingPatternSymbols(binding.pattern, runtime_layout);
            },
            .decl_var, .mutate_var => {
                const cell_symbol = bindingPatternSymbol(self.mir_store, binding.pattern) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("mutable MIR binding requires bind/as_pattern in runtime layout inference", .{});
                    }
                    unreachable;
                };
                try saved_layouts.append(self.allocator, .{
                    .sym_key = cell_symbol.raw(),
                    .previous = self.symbol_layouts.get(cell_symbol.raw()),
                });
                try self.symbol_layouts.put(cell_symbol.raw(), runtime_layout);
            },
        }
    }
    defer {
        var i = saved_layouts.items.len;
        while (i > 0) {
            i -= 1;
            const saved = saved_layouts.items[i];
            if (saved.previous) |layout_idx| {
                self.symbol_layouts.put(saved.sym_key, layout_idx) catch unreachable;
            } else {
                _ = self.symbol_layouts.remove(saved.sym_key);
            }
        }
    }

    return try self.runtimeValueLayoutFromMirExpr(block.final_expr);
}

fn specializationIdentityForLambdaExpr(lambda_expr_id: MIR.ExprId) u64 {
    return (@as(u64, 1) << 63) | @as(u64, @intFromEnum(lambda_expr_id));
}

fn runtimeLayoutFromSpecializedDirectCall(
    self: *Self,
    callee_expr_id: MIR.ExprId,
    call_args: []const MIR.ExprId,
) Allocator.Error!?layout.Idx {
    if (self.lambdaSetForExpr(callee_expr_id)) |callee_ls_idx| {
        var members = try self.snapshotLambdaSetMembers(callee_ls_idx);
        defer members.deinit(self.allocator);
        if (members.items.len == 0) return null;

        const callee_runtime_layout = try self.runtimeClosureDispatchLayoutForExpr(callee_expr_id, callee_ls_idx);

        const save_layouts = self.scratch_layout_idxs.items.len;
        defer self.scratch_layout_idxs.shrinkRetainingCapacity(save_layouts);
        for (call_args) |arg_expr_id| {
            try self.scratch_layout_idxs.append(self.allocator, try self.runtimeValueLayoutFromMirExpr(arg_expr_id));
        }

        var resolved_ret_layout: ?layout.Idx = null;

        for (members.items, 0..) |member, branch_index| {
            const lifted_def = self.mir_store.getSymbolDef(member.fn_symbol) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "MirToLir invariant violated: missing def for specialized direct-call member symbol {d}",
                        .{member.fn_symbol.raw()},
                    );
                }
                unreachable;
            };

            const member_save = self.scratch_layout_idxs.items.len;
            if (!member.closure_member.isNone()) {
                const payload_layout = if (members.items.len == 1)
                    callee_runtime_layout
                else
                    try self.closureVariantPayloadLayout(callee_runtime_layout, branch_index);
                try self.scratch_layout_idxs.append(self.allocator, payload_layout);
            }

            const specialization = try self.ensureSpecializedDirectCallee(
                member.fn_symbol.raw(),
                member.fn_symbol,
                lifted_def,
                self.scratch_layout_idxs.items[save_layouts..],
            );
            self.scratch_layout_idxs.shrinkRetainingCapacity(member_save);

            if (resolved_ret_layout == null) {
                resolved_ret_layout = specialization.ret_layout;
            }
        }

        return resolved_ret_layout;
    }

    const lambda_expr_id = self.resolveToLambdaExprId(callee_expr_id) orelse return null;
    const save_layouts = self.scratch_layout_idxs.items.len;
    defer self.scratch_layout_idxs.shrinkRetainingCapacity(save_layouts);
    for (call_args) |arg_expr_id| {
        try self.scratch_layout_idxs.append(self.allocator, try self.runtimeValueLayoutFromMirExpr(arg_expr_id));
    }
    const specialization = try self.ensureSpecializedDirectCallee(
        specializationIdentityForLambdaExpr(lambda_expr_id),
        Symbol.none,
        lambda_expr_id,
        self.scratch_layout_idxs.items[save_layouts..],
    );
    return specialization.ret_layout;
}

fn runtimeListElemLayoutFromMirExpr(self: *Self, list_mir_expr_id: MIR.ExprId) Allocator.Error!layout.Idx {
    const list_mono_idx = self.mir_store.typeOf(list_mir_expr_id);
    const list_mono = self.mir_store.monotype_store.getMonotype(list_mono_idx);

    switch (self.mir_store.getExpr(list_mir_expr_id)) {
        .list => |list_data| {
            const elems = self.mir_store.getExprSpan(list_data.elems);
            if (elems.len > 0) return self.runtimeValueLayoutFromMirExpr(elems[0]);
        },
        .run_low_level => |ll| switch (ll.op) {
            .list_append_unsafe => {
                const args = self.mir_store.getExprSpan(ll.args);
                if (args.len >= 2) return self.runtimeValueLayoutFromMirExpr(args[1]);
            },
            .list_concat => {
                const args = self.mir_store.getExprSpan(ll.args);
                if (args.len >= 1) return self.runtimeListElemLayoutFromMirExpr(args[0]);
            },
            else => {},
        },
        .lookup => |symbol| {
            if (self.mir_store.getSymbolDef(symbol)) |def_expr_id| {
                return self.runtimeListElemLayoutFromMirExpr(def_expr_id);
            }
        },
        .block => |block| return self.runtimeListElemLayoutFromMirExpr(block.final_expr),
        else => {},
    }

    const list_layout_idx = try self.runtimeValueLayoutFromMirExpr(list_mir_expr_id);
    const list_layout = self.layout_store.getLayout(list_layout_idx);

    return switch (list_layout.tag) {
        .list => list_layout.data.list,
        .list_of_zst => switch (list_mono) {
            .list => |l| try self.zeroSizedSpecializationLayoutFromMonotype(l.elem),
            else => {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "MirToLir invariant violated: expected list monotype for list_get_unsafe source, got {s}",
                        .{@tagName(list_mono)},
                    );
                }
                unreachable;
            },
        },
        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "MirToLir invariant violated: expected list layout for list_get_unsafe source, got {s}",
                    .{@tagName(list_layout.tag)},
                );
            }
            unreachable;
        },
    };
}

/// Adapt a function-typed value to the runtime layout of a target lambda set.
/// This is used at control-flow join points where branch bodies may each produce
/// singleton closures, but the enclosing expression has a wider multi-member
/// lambda set.
fn adaptFunctionValueToLambdaSet(
    self: *Self,
    value_lir_expr: LirExprId,
    value_mir_expr: MIR.ExprId,
    target_ls_idx: LambdaSet.Idx,
    region: Region,
) Allocator.Error!LirExprId {
    var target_members = try self.snapshotLambdaSetMembers(target_ls_idx);
    defer target_members.deinit(self.allocator);
    if (target_members.items.len <= 1) return value_lir_expr;

    const arg_ls_idx = self.lambdaSetForExpr(value_mir_expr) orelse return value_lir_expr;
    if (arg_ls_idx == target_ls_idx) return value_lir_expr;

    const arg_members = self.lambda_set_store.getMembers(self.lambda_set_store.getLambdaSet(arg_ls_idx).members);
    if (arg_members.len != 1) {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "MirToLir invariant violated: unhandled closure value coercion from lambda-set {d} to {d}",
                .{ @intFromEnum(arg_ls_idx), @intFromEnum(target_ls_idx) },
            );
        }
        return value_lir_expr;
    }

    const arg_member = arg_members[0];
    var target_discriminant: ?u16 = null;
    for (target_members.items, 0..) |target_member, i| {
        if (target_member.fn_symbol.eql(arg_member.fn_symbol)) {
            target_discriminant = @intCast(i);
            break;
        }
    }

    const discr = target_discriminant orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "MirToLir invariant violated: closure value member fn={d} not present in target lambda-set {d}",
                .{ arg_member.fn_symbol.raw(), @intFromEnum(target_ls_idx) },
            );
        }
        unreachable;
    };

    const target_layout = try self.closureValueLayoutFromLambdaSet(target_ls_idx);
    if (arg_member.closure_member.isNone()) {
        return self.lir_store.addExpr(.{ .zero_arg_tag = .{
            .discriminant = discr,
            .union_layout = target_layout,
        } }, region);
    }

    const target_payload_layout = try self.closureVariantPayloadLayout(target_layout, discr);
    const adapted_payload = try self.adaptConcreteClosureMemberPayload(
        value_lir_expr,
        arg_member,
        target_payload_layout,
        region,
    );
    const payload_args = try self.lir_store.addExprSpan(&.{adapted_payload});
    return self.lir_store.addExpr(.{ .tag = .{
        .discriminant = discr,
        .union_layout = target_layout,
        .args = payload_args,
    } }, region);
}

fn snapshotLambdaSetMembers(
    self: *Self,
    ls_idx: LambdaSet.Idx,
) Allocator.Error!std.ArrayListUnmanaged(LambdaSet.Member) {
    var snapshot: std.ArrayListUnmanaged(LambdaSet.Member) = .empty;
    errdefer snapshot.deinit(self.allocator);

    const ls = self.lambda_set_store.getLambdaSet(ls_idx);
    try snapshot.appendSlice(self.allocator, self.lambda_set_store.getMembers(ls.members));
    return snapshot;
}

fn prepareLiftedDefCaptureLayout(self: *Self, fn_symbol: Symbol, def_expr_id: MIR.ExprId) Allocator.Error!void {
    const closure_member_id = self.mir_store.getClosureMemberForFn(fn_symbol) orelse return;
    const def_expr = self.mir_store.getExpr(def_expr_id);
    if (def_expr != .lambda) return;

    const params = self.mir_store.getPatternSpan(def_expr.lambda.params);
    if (params.len == 0) return;

    const captures_layout = try self.capturesLayoutForMember(.{
        .fn_symbol = fn_symbol,
        .closure_member = closure_member_id,
    });

    const last_pat = self.mir_store.getPattern(params[params.len - 1]);
    switch (last_pat) {
        .bind => |sym| {
            try self.symbol_layouts.put(sym.raw(), captures_layout);
        },
        else => {},
    }
}

fn identTextIfOwnedBy(env: anytype, ident: Ident.Idx) ?[]const u8 {
    const ident_store = env.getIdentStoreConst();
    const bytes = ident_store.interner.bytes.items.items;
    const start: usize = @intCast(ident.idx);
    if (start >= bytes.len) return null;

    const tail = bytes[start..];
    const end_rel = std.mem.indexOfScalar(u8, tail, 0) orelse return null;
    const text = tail[0..end_rel];

    const roundtrip = ident_store.findByString(text) orelse return null;
    if (roundtrip.idx != ident.idx) return null;
    return text;
}

fn identMatchesText(self: *const Self, ident: Ident.Idx, expected: []const u8) bool {
    if (identTextIfOwnedBy(self.layout_store.currentEnv(), ident)) |text| {
        if (std.mem.eql(u8, text, expected)) return true;
    }

    for (self.layout_store.moduleEnvs()) |env| {
        if (identTextIfOwnedBy(env, ident)) |text| {
            if (std.mem.eql(u8, text, expected)) return true;
        }
    }

    return false;
}

fn identsTextEqual(self: *const Self, lhs: Ident.Idx, rhs: Ident.Idx) bool {
    if (lhs.eql(rhs)) return true;

    if (identTextIfOwnedBy(self.layout_store.currentEnv(), lhs)) |lhs_text| {
        if (self.identMatchesText(rhs, lhs_text)) return true;
    }

    for (self.layout_store.moduleEnvs()) |env| {
        if (identTextIfOwnedBy(env, lhs)) |lhs_text| {
            if (self.identMatchesText(rhs, lhs_text)) return true;
        }
    }

    return false;
}

/// Given a tag name and the monotype of the containing tag union,
/// return the discriminant (sorted index of the tag name).
fn tagDiscriminant(self: *const Self, tag_name: Ident.Idx, union_mono_idx: Monotype.Idx) u16 {
    const monotype = self.mir_store.monotype_store.getMonotype(union_mono_idx);
    switch (monotype) {
        .tag_union => |tu| {
            const tags = self.mir_store.monotype_store.getTags(tu.tags);

            for (tags, 0..) |tag, i| {
                if (self.identsTextEqual(tag.name, tag_name)) {
                    return @intCast(i);
                }
            }
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "MirToLir invariant violated: tag ident idx {d} not found in tag union mono_idx={d}",
                    .{ tag_name.idx, @intFromEnum(union_mono_idx) },
                );
            }
            unreachable;
        },
        .prim, .unit, .record, .tuple, .list, .box, .func, .recursive_placeholder => {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "tagDiscriminant expected tag_union; got {s} for tag ident idx {d} mono_idx={d}",
                    .{ @tagName(std.meta.activeTag(monotype)), tag_name.idx, @intFromEnum(union_mono_idx) },
                );
            }
            unreachable;
        },
    }
}

/// ANF Let-binding accumulator. Accumulates Let-bindings for compound
/// sub-expressions, then wraps the result in a block if any bindings were needed.
const LetAccumulator = struct {
    parent: *Self,
    save_len: usize,

    /// If `expr_id` is atomic (lookup or literal), return it as-is.
    /// Otherwise, Let-bind it to a fresh symbol and return a lookup to that symbol.
    fn ensureSymbol(acc: *LetAccumulator, expr_id: LirExprId, expr_layout: layout.Idx, region: Region) Allocator.Error!LirExprId {
        const expr = acc.parent.lir_store.getExpr(expr_id);
        if (isAtomicExpr(expr)) return expr_id;

        return acc.parent.appendBindingStmt(
            &acc.parent.scratch_anf_stmts,
            expr_id,
            expr_layout,
            acc.parent.bindingSemanticsForExpr(expr_id, expr_layout),
            region,
        );
    }

    fn bindBorrow(acc: *LetAccumulator, expr_id: LirExprId, expr_layout: layout.Idx, region: Region) Allocator.Error!LirExprId {
        return acc.parent.appendBindingStmt(
            &acc.parent.scratch_anf_stmts,
            expr_id,
            expr_layout,
            acc.parent.borrowBindingSemanticsForExpr(expr_id, expr_layout),
            region,
        );
    }

    fn bindRetained(acc: *LetAccumulator, expr_id: LirExprId, expr_layout: layout.Idx, region: Region) Allocator.Error!LirExprId {
        return acc.parent.materializeRetainedBinding(&acc.parent.scratch_anf_stmts, expr_id, expr_layout, region);
    }

    /// Wrap `result_expr` in a block with accumulated Let-bindings, or return it directly
    /// if no bindings were accumulated.
    fn finish(acc: *LetAccumulator, result_expr: LirExprId, result_layout: layout.Idx, region: Region) Allocator.Error!LirExprId {
        const stmts_slice = acc.parent.scratch_anf_stmts.items[acc.save_len..];
        defer acc.parent.scratch_anf_stmts.shrinkRetainingCapacity(acc.save_len);
        if (stmts_slice.len == 0) return result_expr;
        const lir_stmts = try acc.parent.lir_store.addStmts(stmts_slice);
        return acc.parent.lir_store.addExpr(.{ .block = .{
            .stmts = lir_stmts,
            .final_expr = result_expr,
            .result_layout = result_layout,
        } }, region);
    }
};

fn startLetAccumulator(self: *Self) LetAccumulator {
    return .{ .parent = self, .save_len = self.scratch_anf_stmts.items.len };
}

/// Returns true if the expression is already atomic (a lookup or literal)
/// and doesn't need Let-binding for ANF.
fn isAtomicExpr(expr: LirExpr) bool {
    return switch (expr) {
        .lookup,
        .i64_literal,
        .i128_literal,
        .f64_literal,
        .f32_literal,
        .dec_literal,
        .str_literal,
        .bool_literal,
        .zero_arg_tag,
        .empty_list,
        .runtime_error,
        // Lambdas are treated as atomic because generateCall
        // handles them specially.
        .lambda,
        => true,

        .call,
        .list,
        .struct_,
        .struct_access,
        .tag,
        .if_then_else,
        .match_expr,
        .block,
        .early_return,
        .break_expr,
        .low_level,
        .dbg,
        .expect,
        .crash,
        .nominal,
        .cell_load,
        .str_concat,
        .int_to_str,
        .float_to_str,
        .dec_to_str,
        .str_escape_and_quote,
        .discriminant_switch,
        .tag_payload_access,
        .for_loop,
        .while_loop,
        .incref,
        .decref,
        .free,
        .hosted_call,
        => false,
    };
}

fn isBorrowAtomicExpr(self: *const Self, expr_id: LirExprId) bool {
    const expr = self.lir_store.getExpr(expr_id);
    return switch (expr) {
        .str_literal => |str_idx| self.lir_store.getString(str_idx).len < (3 * @sizeOf(usize)),
        else => isAtomicExpr(expr),
    };
}

fn lirExprResultLayout(self: *const Self, expr_id: LirExprId) layout.Idx {
    const expr = self.lir_store.getExpr(expr_id);
    return switch (expr) {
        .lookup => |lookup| lookup.layout_idx,
        .cell_load => |load| load.layout_idx,
        .lambda => |lam| lam.fn_layout,
        .block => |block| block.result_layout,
        .if_then_else => |ite| ite.result_layout,
        .match_expr => |match_expr| match_expr.result_layout,
        .call => |call| call.ret_layout,
        .low_level => |ll| ll.ret_layout,
        .hosted_call => |hc| hc.ret_layout,
        .list => |list| list.list_layout,
        .empty_list => |list| list.list_layout,
        .struct_ => |s| s.struct_layout,
        .tag => |tag| tag.union_layout,
        .zero_arg_tag => |tag| tag.union_layout,
        .struct_access => |sa| sa.field_layout,
        .tag_payload_access => |tpa| tpa.payload_layout,
        .nominal => |nom| nom.nominal_layout,
        .dbg => |dbg_expr| dbg_expr.result_layout,
        .expect => |expect| expect.result_layout,
        .early_return => |ret| ret.ret_layout,
        .discriminant_switch => |ds| ds.result_layout,
        .i64_literal => |int| int.layout_idx,
        .i128_literal => |int| int.layout_idx,
        .f64_literal => .f64,
        .f32_literal => .f32,
        .dec_literal => .dec,
        .str_literal => .str,
        .bool_literal => .bool,
        .str_concat,
        .int_to_str,
        .float_to_str,
        .dec_to_str,
        .str_escape_and_quote,
        => .str,
        .for_loop,
        .while_loop,
        .incref,
        .decref,
        .free,
        .break_expr,
        => .zst,
        .crash => |crash_expr| crash_expr.ret_layout,
        .runtime_error => |runtime_error_expr| runtime_error_expr.ret_layout,
    };
}

fn appendArgLayoutsForSpan(self: *Self, span: LirExprSpan) Allocator.Error!void {
    for (self.lir_store.getExprSpan(span)) |arg_id| {
        try self.scratch_layout_idxs.append(self.allocator, self.lirExprResultLayout(arg_id));
    }
}

fn exprResolvesToLookup(self: *const Self, expr_id: LirExprId) bool {
    const expr = self.lir_store.getExpr(expr_id);
    return switch (expr) {
        .lookup => true,
        .block => |block| self.exprResolvesToLookup(block.final_expr),
        .dbg => |dbg_expr| self.exprResolvesToLookup(dbg_expr.expr),
        .nominal => |nominal| self.exprResolvesToLookup(nominal.backing_expr),
        else => false,
    };
}

fn lowLevelExprBorrowsFromLookup(self: *const Self, expr_id: LirExprId) bool {
    const expr = self.lir_store.getExpr(expr_id);
    return switch (expr) {
        .low_level => |ll| blk: {
            if (ll.op != .list_get_unsafe) break :blk false;
            const args = self.lir_store.getExprSpan(ll.args);
            if (args.len == 0) break :blk false;
            break :blk self.exprResolvesToLookup(args[0]);
        },
        .block => |block| self.lowLevelExprBorrowsFromLookup(block.final_expr),
        .dbg => |dbg_expr| self.lowLevelExprBorrowsFromLookup(dbg_expr.expr),
        .nominal => |nominal| self.lowLevelExprBorrowsFromLookup(nominal.backing_expr),
        else => false,
    };
}

fn exprAliasesManagedRef(self: *const Self, expr_id: LirExprId, expr_layout: layout.Idx) bool {
    if (!self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(expr_layout))) return false;

    const expr = self.lir_store.getExpr(expr_id);
    return switch (expr) {
        .lookup => self.symbol_binding_modes.get(expr.lookup.symbol.raw()) == .borrowed,
        .cell_load => true,
        .low_level => self.lowLevelExprBorrowsFromLookup(expr_id),
        .block => |block| self.exprAliasesManagedRef(block.final_expr, expr_layout),
        .dbg => |dbg_expr| self.exprAliasesManagedRef(dbg_expr.expr, expr_layout),
        .nominal => |nominal| self.exprAliasesManagedRef(nominal.backing_expr, expr_layout),
        else => false,
    };
}

fn bindingModeForSemantics(semantics: LirStmt.BindingSemantics) BindingOwnershipMode {
    return switch (semantics) {
        .owned, .retained => .owned,
        .borrow_alias, .scoped_borrow => .borrowed,
    };
}

fn bindingSemanticsForExpr(self: *const Self, expr_id: LirExprId, expr_layout: layout.Idx) LirStmt.BindingSemantics {
    if (!self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(expr_layout))) return .owned;
    if (self.exprAliasesManagedRef(expr_id, expr_layout)) return .borrow_alias;
    return .owned;
}

fn borrowBindingSemanticsForExpr(self: *const Self, expr_id: LirExprId, expr_layout: layout.Idx) LirStmt.BindingSemantics {
    if (!self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(expr_layout))) return .owned;
    if (self.exprAliasesManagedRef(expr_id, expr_layout)) return .borrow_alias;
    return .scoped_borrow;
}

fn appendBindingStmt(
    self: *Self,
    stmts: *std.ArrayList(LirStmt),
    expr_id: LirExprId,
    expr_layout: layout.Idx,
    semantics: LirStmt.BindingSemantics,
    region: Region,
) Allocator.Error!LirExprId {
    const bp = try self.freshBindPattern(expr_layout, false, region);
    try self.symbol_layouts.put(bp.symbol.raw(), expr_layout);
    try self.symbol_binding_modes.put(bp.symbol.raw(), bindingModeForSemantics(semantics));
    try stmts.append(self.allocator, .{ .decl = .{
        .pattern = bp.pattern,
        .expr = expr_id,
        .semantics = semantics,
    } });
    return self.lir_store.addExpr(.{ .lookup = .{
        .symbol = bp.symbol,
        .layout_idx = expr_layout,
    } }, region);
}

fn forceOwnedBinding(
    self: *Self,
    stmts: *std.ArrayList(LirStmt),
    expr_id: LirExprId,
    expr_layout: layout.Idx,
    region: Region,
) Allocator.Error!LirExprId {
    return self.appendBindingStmt(stmts, expr_id, expr_layout, .owned, region);
}

fn materializeRetainedBinding(
    self: *Self,
    stmts: *std.ArrayList(LirStmt),
    expr_id: LirExprId,
    expr_layout: layout.Idx,
    region: Region,
) Allocator.Error!LirExprId {
    if (!self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(expr_layout))) return expr_id;

    const source_expr = if (self.exprAliasesManagedRef(expr_id, expr_layout))
        expr_id
    else
        try self.forceOwnedBinding(stmts, expr_id, expr_layout, region);

    return self.appendBindingStmt(stmts, source_expr, expr_layout, .retained, region);
}

fn updatePatternBindingMode(self: *Self, pat_id: LirPatternId, ownership_mode: BindingOwnershipMode) Allocator.Error!void {
    if (pat_id.isNone()) return;

    switch (self.lir_store.getPattern(pat_id)) {
        .bind => |bind| {
            try self.symbol_binding_modes.put(bind.symbol.raw(), ownership_mode);
        },
        .as_pattern => |as_pat| {
            try self.symbol_binding_modes.put(as_pat.symbol.raw(), ownership_mode);
            try self.updatePatternBindingMode(as_pat.inner, ownership_mode);
        },
        .tag => |tag_pat| {
            for (self.lir_store.getPatternSpan(tag_pat.args)) |arg_pat| {
                try self.updatePatternBindingMode(arg_pat, ownership_mode);
            }
        },
        .struct_ => |struct_pat| {
            for (self.lir_store.getPatternSpan(struct_pat.fields)) |field_pat| {
                try self.updatePatternBindingMode(field_pat, ownership_mode);
            }
        },
        .list => |list_pat| {
            for (self.lir_store.getPatternSpan(list_pat.prefix)) |elem_pat| {
                try self.updatePatternBindingMode(elem_pat, ownership_mode);
            }
            try self.updatePatternBindingMode(list_pat.rest, ownership_mode);
            for (self.lir_store.getPatternSpan(list_pat.suffix)) |elem_pat| {
                try self.updatePatternBindingMode(elem_pat, ownership_mode);
            }
        },
        .wildcard, .int_literal, .float_literal, .str_literal => {},
    }
}

/// Lower a span of MIR expressions, ensuring each is atomic (symbol/literal) via the accumulator.
fn lowerAnfSpan(self: *Self, acc: *LetAccumulator, mir_expr_ids: []const MIR.ExprId, region: Region) Allocator.Error!LirExprSpan {
    const save_len = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_len);
    for (mir_expr_ids) |mir_id| {
        const lowered = try self.lowerExpr(mir_id);
        const lir_id = try self.adaptExprToRuntimeLayout(mir_id, lowered, region);
        const arg_layout = try self.runtimeValueLayoutFromMirExpr(mir_id);
        const ensured = try acc.ensureSymbol(lir_id, arg_layout, region);
        const owned = if (self.exprAliasesManagedRef(ensured, arg_layout))
            try acc.bindRetained(ensured, arg_layout, region)
        else
            ensured;
        try self.scratch_lir_expr_ids.append(self.allocator, owned);
    }
    return self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_len..]);
}

fn lowerExpr(self: *Self, mir_expr_id: MIR.ExprId) Allocator.Error!LirExprId {
    const expr = self.mir_store.getExpr(mir_expr_id);
    const region = self.mir_store.getRegion(mir_expr_id);
    const mono_idx = self.mir_store.typeOf(mir_expr_id);

    return switch (expr) {
        .int => |i| self.lowerInt(i, mono_idx, region),
        .frac_f32 => |v| self.lir_store.addExpr(.{ .f32_literal = v }, region),
        .frac_f64 => |v| self.lir_store.addExpr(.{ .f64_literal = v }, region),
        .dec => |v| self.lir_store.addExpr(.{ .dec_literal = v.num }, region),
        .str => |s| blk: {
            const lir_str_idx = try self.copyStringToLir(s);
            break :blk self.lir_store.addExpr(.{ .str_literal = lir_str_idx }, region);
        },
        .list => |l| self.lowerList(l, mir_expr_id, region),
        .struct_ => |s| switch (self.mir_store.monotype_store.getMonotype(mono_idx)) {
            .record => self.lowerRecord(s.fields, mono_idx, mir_expr_id, region),
            .tuple => self.lowerTuple(s.fields, mono_idx, mir_expr_id, region),
            .unit => self.lowerRecord(s.fields, mono_idx, mir_expr_id, region),
            else => {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "MirToLir invariant violated: MIR struct_ has unexpected monotype {s} at expr {d}",
                        .{ @tagName(self.mir_store.monotype_store.getMonotype(mono_idx)), @intFromEnum(mir_expr_id) },
                    );
                }
                unreachable;
            },
        },
        .tag => |t| self.lowerTag(t, mono_idx, mir_expr_id, region),
        .lookup => |sym| self.lowerLookup(sym, mono_idx, mir_expr_id, region),
        .match_expr => |m| self.lowerMatch(m, mir_expr_id, region),
        .lambda => |l| self.lowerLambda(l, mono_idx, mir_expr_id, region),
        .call => |c| self.lowerCall(c, mir_expr_id, region),
        .block => |b| self.lowerBlock(b, mir_expr_id, region),
        .borrow_scope => |b| self.lowerBorrowScope(b, mir_expr_id, region),
        .struct_access => |sa| switch (self.mir_store.monotype_store.getMonotype(self.mir_store.typeOf(sa.struct_))) {
            .record => self.lowerRecordAccess(sa.struct_, sa.field_idx, mir_expr_id, region),
            .tuple => self.lowerTupleAccess(sa.struct_, sa.field_idx, mir_expr_id, region),
            else => unreachable,
        },
        .str_escape_and_quote => |s| blk: {
            if (builtin.mode == .Debug) {
                const arg_mono = self.mir_store.typeOf(s);
                const arg_type = self.mir_store.monotype_store.getMonotype(arg_mono);
                const ret_type = self.mir_store.monotype_store.getMonotype(mono_idx);
                if (!(arg_type == .prim and arg_type.prim == .str and ret_type == .prim and ret_type.prim == .str)) {
                    std.debug.panic("MIR invariant violated: str_escape_and_quote must be Str -> Str", .{});
                }
            }
            const lowered = try self.lowerExpr(s);
            var acc = self.startLetAccumulator();
            const arg = try acc.ensureSymbol(lowered, .str, region);
            const result = try self.lir_store.addExpr(.{ .str_escape_and_quote = arg }, region);
            break :blk try acc.finish(result, .str, region);
        },
        .run_low_level => |ll| self.lowerLowLevel(ll, mir_expr_id, region),
        .hosted => |h| self.lowerHosted(h, mono_idx, region),
        .runtime_err_can, .runtime_err_type, .runtime_err_ellipsis, .runtime_err_anno_only => {
            const ret_layout = try self.layoutFromMonotype(mono_idx);
            return self.lir_store.addExpr(.{ .runtime_error = .{ .ret_layout = ret_layout } }, region);
        },
        .crash => |s| blk: {
            const lir_str_idx = try self.copyStringToLir(s);
            const ret_layout = try self.layoutFromMonotype(mono_idx);
            break :blk self.lir_store.addExpr(.{ .crash = .{
                .msg = lir_str_idx,
                .ret_layout = ret_layout,
            } }, region);
        },
        .dbg_expr => |d| self.lowerDbg(d, mir_expr_id, region),
        .expect => |e| self.lowerExpect(e, mono_idx, region),
        .for_loop => |f| self.lowerForLoop(f, mono_idx, region),
        .while_loop => |w| self.lowerWhileLoop(w, mono_idx, region),
        .return_expr => |r| self.lowerReturn(r, mir_expr_id, region),
        .break_expr => self.lir_store.addExpr(.break_expr, region),
    };
}

fn adaptExprToRuntimeLayout(
    self: *Self,
    mir_expr_id: MIR.ExprId,
    lir_expr: LirExprId,
    region: Region,
) Allocator.Error!LirExprId {
    const target_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const source_layout = self.lirExprResultLayout(lir_expr);
    return self.adaptValueLayout(
        lir_expr,
        self.mir_store.typeOf(mir_expr_id),
        source_layout,
        target_layout,
        region,
    );
}

fn lowerInt(self: *Self, int_data: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    // Use the monotype to determine the concrete integer layout.
    // For 128-bit types, always emit i128_literal even if the value fits in i64,
    // so codegen receives the correct ABI width and signedness.
    const target_layout = try self.layoutFromMonotype(mono_idx);

    // Dec: integer literals with Dec type must be scaled by 10^18 (RocDec representation).
    // The MIR stores the raw integer value; we convert to Dec here.
    if (target_layout == .dec) {
        const val = int_data.value.toI128();
        const one_point_zero: i128 = 1_000_000_000_000_000_000;
        return self.lir_store.addExpr(.{ .dec_literal = val * one_point_zero }, region);
    }

    const needs_128 = target_layout == .i128 or target_layout == .u128;

    switch (int_data.value.kind) {
        .u128 => {
            const val: u128 = @bitCast(int_data.value.bytes);
            if (!needs_128 and val <= std.math.maxInt(i64)) {
                return self.lir_store.addExpr(.{ .i64_literal = .{
                    .value = @intCast(val),
                    .layout_idx = target_layout,
                } }, region);
            }
            return self.lir_store.addExpr(.{ .i128_literal = .{
                .value = @bitCast(val),
                .layout_idx = target_layout,
            } }, region);
        },
        .i128 => {
            const val = int_data.value.toI128();
            if (!needs_128 and val >= std.math.minInt(i64) and val <= std.math.maxInt(i64)) {
                return self.lir_store.addExpr(.{ .i64_literal = .{
                    .value = @intCast(val),
                    .layout_idx = target_layout,
                } }, region);
            }
            return self.lir_store.addExpr(.{ .i128_literal = .{
                .value = val,
                .layout_idx = target_layout,
            } }, region);
        },
    }
}

fn lowerList(self: *Self, list_data: anytype, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const list_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const elem_layout = try self.runtimeListElemLayoutFromMirExpr(mir_expr_id);

    const mir_elems = self.mir_store.getExprSpan(list_data.elems);
    if (mir_elems.len == 0) {
        return self.lir_store.addExpr(.{ .empty_list = .{
            .list_layout = list_layout,
            .elem_layout = elem_layout,
        } }, region);
    }

    var acc = self.startLetAccumulator();
    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);
    for (mir_elems) |mir_elem| {
        const lowered = try self.lowerExpr(mir_elem);
        const elem_runtime_layout = try self.runtimeValueLayoutFromMirExpr(mir_elem);
        const retained = try acc.bindRetained(lowered, elem_runtime_layout, region);
        const ensured = try acc.ensureSymbol(retained, elem_runtime_layout, region);
        try self.scratch_lir_expr_ids.append(self.allocator, ensured);
    }
    const lir_elems = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);
    const list_expr = try self.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = elem_layout,
        .elems = lir_elems,
    } }, region);
    return acc.finish(list_expr, list_layout, region);
}

fn lowerRecord(self: *Self, fields: MIR.ExprSpan, _: Monotype.Idx, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const mir_fields = self.mir_store.getExprSpan(fields);
    const record_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    if (mir_fields.len == 0) {
        return self.lir_store.addExpr(.{ .struct_ = .{
            .struct_layout = record_layout,
            .fields = LirExprSpan.empty(),
        } }, region);
    }

    const record_layout_val = self.layout_store.getLayout(record_layout);
    if (builtin.mode == .Debug and record_layout_val.tag != .struct_) {
        std.debug.panic(
            "MirToLir invariant violated: non-empty record expression must lower to struct_ layout, got {s}",
            .{@tagName(record_layout_val.tag)},
        );
    }
    if (record_layout_val.tag != .struct_) unreachable;

    var acc = self.startLetAccumulator();

    // MIR fields are in source/alphabetical order, but the layout store sorts
    // fields by alignment descending then alphabetically. Reorder expressions
    // to match layout order so codegen can use positional field indices.
    const record_data = self.layout_store.getStructData(record_layout_val.data.struct_.idx);
    const layout_fields = self.layout_store.struct_fields.sliceRange(record_data.getFields());

    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

    for (0..layout_fields.len) |li| {
        const semantic_index = layout_fields.get(li).index;
        const lir_expr = try self.lowerExpr(mir_fields[semantic_index]);
        const field_layout = try self.runtimeValueLayoutFromMirExpr(mir_fields[semantic_index]);
        const ensured = try acc.ensureSymbol(lir_expr, field_layout, region);
        try self.scratch_lir_expr_ids.append(self.allocator, ensured);
    }

    const lir_fields = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);

    const struct_expr = try self.lir_store.addExpr(.{ .struct_ = .{
        .struct_layout = record_layout,
        .fields = lir_fields,
    } }, region);
    return acc.finish(struct_expr, record_layout, region);
}

fn lowerTuple(self: *Self, fields: MIR.ExprSpan, _: Monotype.Idx, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const tuple_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const mir_elems = self.mir_store.getExprSpan(fields);
    const tuple_layout_val = self.layout_store.getLayout(tuple_layout);
    if (builtin.mode == .Debug and mir_elems.len != 0 and tuple_layout_val.tag != .struct_) {
        std.debug.panic(
            "MirToLir invariant violated: non-empty tuple expression must lower to struct_ layout, got {s}",
            .{@tagName(tuple_layout_val.tag)},
        );
    }

    if (self.mir_store.getExprClosureMember(mir_expr_id) != null) {
        const save_exprs = self.scratch_lir_expr_ids.items.len;
        defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

        const save_stmt_len = self.scratch_lir_stmts.items.len;
        defer self.scratch_lir_stmts.shrinkRetainingCapacity(save_stmt_len);

        const appendCapture = struct {
            fn append(
                self_: *Self,
                mir_elem: MIR.ExprId,
                region_: Region,
            ) Allocator.Error!void {
                const lir_expr = try self_.lowerExpr(mir_elem);
                const elem_layout = try self_.runtimeValueLayoutFromMirExpr(mir_elem);
                const source_expr = if (self_.isBorrowAtomicExpr(lir_expr))
                    lir_expr
                else blk: {
                    break :blk try self_.appendBindingStmt(
                        &self_.scratch_lir_stmts,
                        lir_expr,
                        elem_layout,
                        self_.borrowBindingSemanticsForExpr(lir_expr, elem_layout),
                        region_,
                    );
                };

                if (!self_.layout_store.layoutContainsRefcounted(self_.layout_store.getLayout(elem_layout))) {
                    try self_.scratch_lir_expr_ids.append(self_.allocator, source_expr);
                    return;
                }

                const retained_lookup = try self_.materializeRetainedBinding(
                    &self_.scratch_lir_stmts,
                    source_expr,
                    elem_layout,
                    region_,
                );
                try self_.scratch_lir_expr_ids.append(self_.allocator, retained_lookup);
            }
        }.append;

        if (tuple_layout_val.tag != .struct_) unreachable;

        const struct_data = self.layout_store.getStructData(tuple_layout_val.data.struct_.idx);
        const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());

        for (0..layout_fields.len) |li| {
            const original_index = layout_fields.get(li).index;
            try appendCapture(self, mir_elems[original_index], region);
        }

        const final_expr = blk: {
            const lir_fields = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);
            break :blk try self.lir_store.addExpr(.{ .struct_ = .{
                .struct_layout = tuple_layout,
                .fields = lir_fields,
            } }, region);
        };

        if (self.scratch_lir_stmts.items.len == save_stmt_len) {
            return final_expr;
        }

        const stmts = try self.lir_store.addStmts(self.scratch_lir_stmts.items[save_stmt_len..]);
        return self.lir_store.addExpr(.{ .block = .{
            .stmts = stmts,
            .final_expr = final_expr,
            .result_layout = tuple_layout,
        } }, region);
    }

    var acc = self.startLetAccumulator();

    if (tuple_layout_val.tag != .struct_) unreachable;

    // MIR elements are in source order (.0, .1, .2, ...) but the layout store
    // sorts fields by alignment. Reorder to match layout order.
    const struct_data = self.layout_store.getStructData(tuple_layout_val.data.struct_.idx);
    const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());

    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

    for (0..layout_fields.len) |li| {
        const original_index = layout_fields.get(li).index;
        const lir_expr = try self.lowerExpr(mir_elems[original_index]);
        const elem_layout = try self.runtimeValueLayoutFromMirExpr(mir_elems[original_index]);
        const ensured = try acc.ensureSymbol(lir_expr, elem_layout, region);
        try self.scratch_lir_expr_ids.append(self.allocator, ensured);
    }

    const lir_fields = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);

    const struct_expr = try self.lir_store.addExpr(.{ .struct_ = .{
        .struct_layout = tuple_layout,
        .fields = lir_fields,
    } }, region);
    return acc.finish(struct_expr, tuple_layout, region);
}

fn lowerTag(self: *Self, tag_data: anytype, mono_idx: Monotype.Idx, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const mir_args = self.tagPayloadExprs(mono_idx, tag_data.name, tag_data.args);

    const union_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const discriminant = self.tagDiscriminant(tag_data.name, mono_idx);
    const union_layout_val = self.layout_store.getLayout(union_layout);
    if (union_layout_val.tag == .scalar or union_layout_val.tag == .zst) {
        var acc = self.startLetAccumulator();
        for (mir_args) |mir_arg| {
            const lowered_arg = try self.lowerExpr(mir_arg);
            const arg_layout = try self.runtimeValueLayoutFromMirExpr(mir_arg);
            _ = try acc.ensureSymbol(lowered_arg, arg_layout, region);
        }
        const zero_arg_tag = try self.lir_store.addExpr(.{ .zero_arg_tag = .{
            .discriminant = discriminant,
            .union_layout = union_layout,
        } }, region);
        return acc.finish(zero_arg_tag, union_layout, region);
    }

    const variant_payload_layout: ?layout.Idx = if (union_layout_val.tag == .tag_union) blk: {
        const tu_data = self.layout_store.getTagUnionData(union_layout_val.data.tag_union.idx);
        const variants = self.layout_store.getTagUnionVariants(tu_data);
        break :blk if (discriminant < variants.len) variants.get(discriminant).payload_layout else null;
    } else null;

    if (mir_args.len == 0) {
        return self.lir_store.addExpr(.{ .zero_arg_tag = .{
            .discriminant = discriminant,
            .union_layout = union_layout,
        } }, region);
    }

    var acc = self.startLetAccumulator();
    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);
    for (mir_args, 0..) |mir_arg, arg_index| {
        const lowered_arg = try self.lowerExpr(mir_arg);
        const arg_layout = try self.runtimeValueLayoutFromMirExpr(mir_arg);
        const ensured_source = try acc.ensureSymbol(lowered_arg, arg_layout, region);
        const owned_arg = if (self.exprAliasesManagedRef(ensured_source, arg_layout))
            try acc.bindRetained(ensured_source, arg_layout, region)
        else
            ensured_source;
        const target_arg_layout = blk: {
            if (variant_payload_layout == null) break :blk arg_layout;
            const payload_layout_val = self.layout_store.getLayout(variant_payload_layout.?);
            if (payload_layout_val.tag != .struct_) break :blk variant_payload_layout.?;
            const field = self.structFieldInfoByOriginalIndex(variant_payload_layout.?, @intCast(arg_index)) orelse break :blk arg_layout;
            break :blk field.field_layout;
        };
        const adapted_arg = try self.adaptValueLayout(
            owned_arg,
            self.mir_store.typeOf(mir_arg),
            arg_layout,
            target_arg_layout,
            region,
        );
        const ensured = try acc.ensureSymbol(adapted_arg, target_arg_layout, region);
        try self.scratch_lir_expr_ids.append(self.allocator, ensured);
    }
    const lir_args = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);
    const tag_expr = try self.lir_store.addExpr(.{ .tag = .{
        .discriminant = discriminant,
        .union_layout = union_layout,
        .args = lir_args,
    } }, region);
    return acc.finish(tag_expr, union_layout, region);
}

fn lowerLookup(self: *Self, sym: Symbol, mono_idx: Monotype.Idx, _: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    // Propagate MIR symbol definition to LIR store (if exists and not already done)
    if (self.lir_store.getSymbolDef(sym) == null) {
        if (self.mir_store.getSymbolDef(sym)) |mir_def_id| {
            const key: u64 = @bitCast(sym);
            if (!self.propagating_defs.contains(key)) {
                try self.propagating_defs.put(key, {});
                defer _ = self.propagating_defs.remove(key);
                try self.prepareLiftedDefCaptureLayout(sym, mir_def_id);
                const lir_def_id = try self.lowerExpr(mir_def_id);
                try self.lir_store.registerSymbolDef(sym, lir_def_id);
            }
        }
    }

    const layout_idx = blk: {
        if (self.symbol_layouts.get(sym.raw())) |binding_layout| {
            break :blk try self.runtimeLayoutForBindingSymbol(sym, mono_idx, binding_layout);
        }
        if (self.mir_store.monotype_store.getMonotype(mono_idx) == .func) {
            if (self.lambda_set_store.getSymbolLambdaSet(sym)) |ls_idx| {
                break :blk try self.closureValueLayoutFromLambdaSet(ls_idx);
            }
        }
        if (self.mir_store.getSymbolDef(sym)) |mir_def_id| {
            break :blk try self.runtimeValueLayoutFromMirExpr(mir_def_id);
        }
        if (self.mir_store.monotype_store.getMonotype(mono_idx) == .func) {
            break :blk try self.layoutFromMonotype(mono_idx);
        }
        break :blk try self.layoutFromMonotype(mono_idx);
    };

    if (self.mir_store.isSymbolReassignable(sym)) {
        return self.lir_store.addExpr(.{ .cell_load = .{
            .cell = sym,
            .layout_idx = layout_idx,
        } }, region);
    }

    return self.lir_store.addExpr(.{ .lookup = .{ .symbol = sym, .layout_idx = layout_idx } }, region);
}

fn lowerMatch(self: *Self, match_data: anytype, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    var acc = self.startLetAccumulator();
    const cond_raw = try self.lowerExpr(match_data.cond);
    const value_layout = try self.runtimeValueLayoutFromMirExpr(match_data.cond);
    const cond_id = try acc.ensureSymbol(cond_raw, value_layout, region);
    const result_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const result_ls_idx = self.lambdaSetForExpr(mir_expr_id) orelse LambdaSet.Idx.none;

    const mir_branches = self.mir_store.getBranches(match_data.branches);

    const save_len = self.scratch_lir_match_branches.items.len;
    const save_rest_bindings = self.scratch_deferred_list_rest_bindings.items.len;
    defer self.scratch_lir_match_branches.shrinkRetainingCapacity(save_len);
    defer self.scratch_deferred_list_rest_bindings.shrinkRetainingCapacity(save_rest_bindings);
    for (mir_branches) |branch| {
        const branch_patterns = self.mir_store.getBranchPatterns(branch.patterns);
        if (branch_patterns.len == 0) continue;
        for (branch_patterns) |bp| {
            try self.registerBindingPatternSymbols(bp.pattern, value_layout);
        }

        // OR-patterns: a single MIR branch may have multiple patterns.
        // We lower the body once and share the LIR body ID across all
        // resulting LIR branches. This is safe because RC insertion runs
        // at the MIR level (rc_insert.zig processMatch), where each MIR
        // branch gets its own RC wrapper — so by this point, RC ops are
        // already embedded in the body expression.
        var lir_body = try self.lowerExpr(branch.body);
        if (!result_ls_idx.isNone()) {
            lir_body = try self.adaptFunctionValueToLambdaSet(lir_body, branch.body, result_ls_idx, region);
        }
        const guard = if (branch.guard.isNone())
            LirExprId.none
        else
            try self.lowerExpr(branch.guard);

        for (branch_patterns) |bp| {
            const lowered_pat = try self.lowerBindingPatternForRuntimeLayout(bp.pattern, value_layout, .borrowed, region);
            const rewrite = try self.rewriteTopLevelRestBinding(lowered_pat, value_layout, .borrowed, region);
            const branch_body = if (rewrite) |rw| blk: {
                const save_stmt_len = self.scratch_lir_stmts.items.len;
                defer self.scratch_lir_stmts.shrinkRetainingCapacity(save_stmt_len);
                const source_semantics = if (self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(value_layout)))
                    LirStmt.BindingSemantics.retained
                else
                    LirStmt.BindingSemantics.owned;
                try self.updatePatternBindingMode(rw.source_pattern, bindingModeForSemantics(source_semantics));
                try self.scratch_lir_stmts.append(self.allocator, .{ .decl = .{
                    .pattern = rw.source_pattern,
                    .expr = cond_id,
                    .semantics = source_semantics,
                } });
                try self.appendDeferredListRestBindingDecls(
                    lowered_pat.deferred_rest_start,
                    lowered_pat.deferred_rest_len,
                    region,
                );
                const stmts = try self.lir_store.addStmts(self.scratch_lir_stmts.items[save_stmt_len..]);
                break :blk try self.lir_store.addExpr(.{ .block = .{
                    .stmts = stmts,
                    .final_expr = lir_body,
                    .result_layout = result_layout,
                } }, region);
            } else blk: {
                break :blk try self.wrapExprWithDeferredListRestBindings(
                    lir_body,
                    result_layout,
                    lowered_pat.deferred_rest_start,
                    lowered_pat.deferred_rest_len,
                    region,
                );
            };
            try self.scratch_lir_match_branches.append(self.allocator, .{
                .pattern = if (rewrite) |rw| rw.destructure_pattern else lowered_pat.pattern,
                .guard = guard,
                .body = branch_body,
            });
        }
    }

    const match_branches = try self.lir_store.addMatchBranches(self.scratch_lir_match_branches.items[save_len..]);
    const match_expr = try self.lir_store.addExpr(.{ .match_expr = .{
        .value = cond_id,
        .value_layout = value_layout,
        .branches = match_branches,
        .result_layout = result_layout,
    } }, region);
    return acc.finish(match_expr, result_layout, region);
}

fn lowerLambda(self: *Self, lam: anytype, mono_idx: Monotype.Idx, _: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    const func_args = switch (monotype) {
        .func => |f| self.mir_store.monotype_store.getIdxSpan(f.args),
        .prim, .unit, .record, .tuple, .tag_union, .list, .box, .recursive_placeholder => unreachable,
    };
    const save_layouts = self.scratch_layout_idxs.items.len;
    defer self.scratch_layout_idxs.shrinkRetainingCapacity(save_layouts);
    for (func_args) |arg_mono_idx| {
        try self.scratch_layout_idxs.append(self.allocator, try self.layoutFromMonotype(arg_mono_idx));
    }
    return self.lowerLambdaWithParamLayouts(lam, mono_idx, self.scratch_layout_idxs.items[save_layouts..], region);
}

fn lowerLambdaWithParamLayouts(
    self: *Self,
    lam: anytype,
    mono_idx: Monotype.Idx,
    param_layouts: []const layout.Idx,
    region: Region,
) Allocator.Error!LirExprId {
    const SavedSymbolLayout = struct {
        sym_key: u64,
        previous: ?layout.Idx,
    };
    const SavedBindingMode = struct {
        sym_key: u64,
        previous: ?BindingOwnershipMode,
    };

    const fn_layout = try self.layoutFromMonotype(mono_idx);
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    const mir_params = self.mir_store.getPatternSpan(lam.params);
    if (builtin.mode == .Debug and mir_params.len != param_layouts.len) {
        std.debug.panic(
            "MirToLir invariant violated: lambda param layout count mismatch ({d} params, {d} layouts)",
            .{ mir_params.len, param_layouts.len },
        );
    }
    const func_args = switch (monotype) {
        .func => |f| self.mir_store.monotype_store.getIdxSpan(f.args),
        .prim, .unit, .record, .tuple, .tag_union, .list, .box, .recursive_placeholder => unreachable,
    };
    if (builtin.mode == .Debug and func_args.len != param_layouts.len) {
        std.debug.panic(
            "MirToLir invariant violated: function monotype arg/layout mismatch ({d} args, {d} layouts)",
            .{ func_args.len, param_layouts.len },
        );
    }
    const save_rest_bindings = self.scratch_deferred_list_rest_bindings.items.len;
    defer self.scratch_deferred_list_rest_bindings.shrinkRetainingCapacity(save_rest_bindings);
    var param_infos = std.ArrayList(LoweredBindingPattern).empty;
    defer param_infos.deinit(self.allocator);
    var param_rewrites = std.ArrayList(?TopLevelRestBindingRewrite).empty;
    defer param_rewrites.deinit(self.allocator);
    var saved_monotype_layouts = std.ArrayList(SavedMonotypeLayout).empty;
    defer saved_monotype_layouts.deinit(self.allocator);
    defer self.restoreMonotypeOverrides(saved_monotype_layouts.items);
    var saved_symbol_layouts = std.ArrayList(SavedSymbolLayout).empty;
    defer saved_symbol_layouts.deinit(self.allocator);
    var saved_binding_modes = std.ArrayList(SavedBindingMode).empty;
    defer saved_binding_modes.deinit(self.allocator);

    const save_param_patterns = self.scratch_lir_pattern_ids.items.len;
    defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_param_patterns);
    var bound_symbols = std.ArrayList(u64).empty;
    defer bound_symbols.deinit(self.allocator);
    for (mir_params) |mir_param_id| {
        try self.collectPatternBindingSymbolKeys(mir_param_id, &bound_symbols);
    }
    for (bound_symbols.items) |sym_key| {
        try saved_symbol_layouts.append(self.allocator, .{
            .sym_key = sym_key,
            .previous = self.symbol_layouts.get(sym_key),
        });
        try saved_binding_modes.append(self.allocator, .{
            .sym_key = sym_key,
            .previous = self.symbol_binding_modes.get(sym_key),
        });
        _ = self.symbol_layouts.remove(sym_key);
        _ = self.symbol_binding_modes.remove(sym_key);
    }
    defer {
        for (saved_binding_modes.items) |saved| {
            if (saved.previous) |mode| {
                self.symbol_binding_modes.put(saved.sym_key, mode) catch unreachable;
            } else {
                _ = self.symbol_binding_modes.remove(saved.sym_key);
            }
        }
        for (saved_symbol_layouts.items) |saved| {
            if (saved.previous) |layout_idx| {
                self.symbol_layouts.put(saved.sym_key, layout_idx) catch unreachable;
            } else {
                _ = self.symbol_layouts.remove(saved.sym_key);
            }
        }
    }

    for (func_args, param_layouts) |param_mono_idx, param_layout| {
        try self.registerSpecializedMonotypeLayout(param_mono_idx, param_layout, &saved_monotype_layouts);
    }
    for (mir_params, 0..) |mir_param_id, i| {
        const param_layout = param_layouts[i];
        try self.registerBindingPatternSymbols(mir_param_id, param_layout);
        const lowered = try self.lowerBindingPatternForRuntimeLayout(mir_param_id, param_layout, .owned, region);
        try param_infos.append(self.allocator, lowered);
        const rewrite = try self.rewriteTopLevelRestBinding(lowered, param_layout, .owned, region);
        try param_rewrites.append(self.allocator, rewrite);
        try self.scratch_lir_pattern_ids.append(self.allocator, if (rewrite) |rw| rw.source_pattern else lowered.pattern);
    }
    const lir_params = try self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_param_patterns..]);
    if (builtin.mode == .Debug) {
        for (self.lir_store.getPatternSpan(lir_params)) |pat_id| {
            const pat_index = @intFromEnum(pat_id);
            if (pat_index >= self.lir_store.patterns.items.len) {
                std.debug.panic(
                    "MirToLir invariant violated: lambda params contain invalid pattern id {d} (patterns_len={d}, mir_params={d}, layouts={d})",
                    .{ pat_index, self.lir_store.patterns.items.len, mir_params.len, param_layouts.len },
                );
            }
        }
    }
    const ret_layout = switch (monotype) {
        .func => |f| blk: {
            const inferred = try self.runtimeValueLayoutFromMirExpr(lam.body);
            try self.registerSpecializedMonotypeLayout(f.ret, inferred, &saved_monotype_layouts);
            break :blk inferred;
        },
        .prim, .unit, .record, .tuple, .tag_union, .list, .box, .recursive_placeholder => unreachable, // Lambda expressions always have .func monotype
    };

    var lir_body = try self.lowerExpr(lam.body);
    var lambda_param_idx = param_infos.items.len;
    while (lambda_param_idx > 0) {
        lambda_param_idx -= 1;
        const info = param_infos.items[lambda_param_idx];
        lir_body = try self.wrapExprWithTopLevelRestBindingPrelude(
            lir_body,
            ret_layout,
            param_rewrites.items[lambda_param_idx],
            info.deferred_rest_start,
            info.deferred_rest_len,
            region,
        );
    }

    // After lambda lifting (Phase 2a), all lambdas reaching MirToLir have empty captures.
    // Closures are lifted to top-level functions with explicit captures tuple params;
    // dispatch is generated separately using lambda set info.
    if (std.debug.runtime_safety and !lam.captures.isEmpty()) {
        std.debug.panic("MirToLir invariant: lambda with non-empty captures (should have been lifted)", .{});
    }

    return self.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = fn_layout,
        .params = lir_params,
        .body = lir_body,
        .ret_layout = ret_layout,
    } }, region);
}

fn lowerCall(self: *Self, call_data: anytype, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const mono_idx = self.mir_store.typeOf(mir_expr_id);
    const ret_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);

    // Some annotation-only methods are compiler intrinsics. Lower those directly.
    const func_mir_expr = self.mir_store.getExpr(call_data.func);
    if (func_mir_expr == .lookup) {
        const sym = func_mir_expr.lookup;
        if (self.mir_store.getSymbolDef(sym)) |def_expr_id| {
            if (self.mir_store.getExpr(def_expr_id) == .runtime_err_anno_only) {
                if (try self.lowerAnnotationOnlyIntrinsicCall(call_data, mono_idx, region)) |lowered| {
                    return lowered;
                }
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "MirToLir unsupported: call to annotation-only symbol key={d}",
                        .{sym.raw()},
                    );
                }
                unreachable;
            }
        }
    }

    // Closure dispatch is driven by lambda sets for arbitrary callee expressions,
    // not just direct symbol lookups. This is required for nested calls where
    // intermediate call results are themselves closures.
    if (self.lambdaSetForExpr(call_data.func)) |callee_ls_idx| {
        const callee_symbol = if (func_mir_expr == .lookup) func_mir_expr.lookup else Symbol.none;
        return self.lowerClosureCall(call_data, callee_ls_idx, callee_symbol, ret_layout, region);
    }

    if (self.resolveToLambdaExprId(call_data.func)) |lambda_expr_id| {
        var acc = self.startLetAccumulator();
        const mir_args = self.mir_store.getExprSpan(call_data.args);
        const lir_args = try self.lowerAnfSpan(&acc, mir_args, region);
        const fn_mono = self.mir_store.typeOf(call_data.func);
        const fn_layout = try self.layoutFromMonotype(fn_mono);

        const save_layouts = self.scratch_layout_idxs.items.len;
        defer self.scratch_layout_idxs.shrinkRetainingCapacity(save_layouts);
        for (mir_args) |mir_arg| {
            try self.scratch_layout_idxs.append(self.allocator, try self.runtimeValueLayoutFromMirExpr(mir_arg));
        }

        const specialization = try self.ensureSpecializedDirectCallee(
            specializationIdentityForLambdaExpr(lambda_expr_id),
            Symbol.none,
            lambda_expr_id,
            self.scratch_layout_idxs.items[save_layouts..],
        );
        const fn_expr = try self.lir_store.addExpr(.{ .lookup = .{
            .symbol = specialization.symbol,
            .layout_idx = fn_layout,
        } }, region);

        const call_expr = try self.lir_store.addExpr(.{ .call = .{
            .fn_expr = fn_expr,
            .fn_layout = fn_layout,
            .args = lir_args,
            .ret_layout = specialization.ret_layout,
            .called_via = .apply,
        } }, region);
        return acc.finish(call_expr, specialization.ret_layout, region);
    }

    // Direct function call — only for inline lambda calls or HOF parameters (which
    // have no symbol_defs entry). After lambda set unification, all lookup callees
    // with lambda defs should have lambda sets and go through lowerClosureCall.
    if (func_mir_expr == .lookup and std.debug.runtime_safety) {
        const sym = func_mir_expr.lookup;
        if (self.mir_store.getSymbolDef(sym)) |def_id| {
            if (LambdaSet.isLambdaExpr(self.mir_store, def_id)) {
                std.debug.panic("MirToLir: lookup callee with lambda def reached direct call fallback, symbol key={d}", .{sym.raw()});
            }
        }
    }

    var acc = self.startLetAccumulator();
    const fn_expr_raw = try self.lowerExpr(call_data.func);
    const fn_mono = self.mir_store.typeOf(call_data.func);
    const fn_layout = try self.layoutFromMonotype(fn_mono);
    const fn_expr = try acc.ensureSymbol(fn_expr_raw, fn_layout, region);

    const mir_args = self.mir_store.getExprSpan(call_data.args);
    const lir_args = try self.lowerAnfSpan(&acc, mir_args, region);

    const call_expr = try self.lir_store.addExpr(.{ .call = .{
        .fn_expr = fn_expr,
        .fn_layout = fn_layout,
        .args = lir_args,
        .ret_layout = ret_layout,
        .called_via = .apply,
    } }, region);
    return acc.finish(call_expr, ret_layout, region);
}

const AnnotationOnlyIntrinsic = struct {
    op: LirExpr.LowLevel,
    result_mono: Monotype.Idx,
};

fn annotationOnlyIntrinsicForFunc(
    self: *Self,
    func_mono_idx: Monotype.Idx,
) Allocator.Error!?AnnotationOnlyIntrinsic {
    const func_mono = self.mir_store.monotype_store.getMonotype(func_mono_idx);
    if (func_mono != .func) return null;

    const fn_args = self.mir_store.monotype_store.getIdxSpan(func_mono.func.args);
    if (fn_args.len != 1) return null;
    const arg_mono = fn_args[0];
    const ret_mono = func_mono.func.ret;

    const arg_ty = self.mir_store.monotype_store.getMonotype(arg_mono);
    const ret_ty = self.mir_store.monotype_store.getMonotype(ret_mono);

    if (ret_ty == .box) {
        if (try self.monotypesStructurallyEqual(arg_mono, ret_ty.box.inner)) {
            return .{ .op = .box_box, .result_mono = ret_mono };
        }
    }
    if (arg_ty == .box) {
        if (try self.monotypesStructurallyEqual(arg_ty.box.inner, ret_mono)) {
            return .{ .op = .box_unbox, .result_mono = ret_mono };
        }
    }

    return null;
}

/// Lower known annotation-only intrinsics by recognizing their monomorphic signatures.
/// Supported:
/// - a -> Box(a)      => box_box
/// - Box(a) -> a      => box_unbox
fn lowerAnnotationOnlyIntrinsicCall(
    self: *Self,
    call_data: anytype,
    _: Monotype.Idx,
    region: Region,
) Allocator.Error!?LirExprId {
    const func_mono_idx = self.mir_store.typeOf(call_data.func);
    const intrinsic = (try self.annotationOnlyIntrinsicForFunc(func_mono_idx)) orelse return null;

    const mir_args = self.mir_store.getExprSpan(call_data.args);
    if (mir_args.len != 1) return null;

    var acc = self.startLetAccumulator();
    const lir_args = try self.lowerAnfSpan(&acc, mir_args, region);
    const ret_layout = try self.layoutFromMonotype(intrinsic.result_mono);
    const ll_expr = try self.lir_store.addExpr(.{ .low_level = .{
        .op = intrinsic.op,
        .args = lir_args,
        .ret_layout = ret_layout,
    } }, region);
    return try acc.finish(ll_expr, ret_layout, region);
}

fn monotypesStructurallyEqual(self: *Self, lhs: Monotype.Idx, rhs: Monotype.Idx) Allocator.Error!bool {
    if (lhs == rhs) return true;
    var seen = std.AutoHashMap(u64, void).init(self.allocator);
    defer seen.deinit();
    return try self.monotypesStructurallyEqualRec(lhs, rhs, &seen);
}

fn monotypesStructurallyEqualRec(
    self: *Self,
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

    const lhs_mono = self.mir_store.monotype_store.getMonotype(lhs);
    const rhs_mono = self.mir_store.monotype_store.getMonotype(rhs);
    if (std.meta.activeTag(lhs_mono) != std.meta.activeTag(rhs_mono)) return false;

    return switch (lhs_mono) {
        .recursive_placeholder => unreachable,
        .unit => true,
        .prim => |p| p == rhs_mono.prim,
        .list => |l| try self.monotypesStructurallyEqualRec(l.elem, rhs_mono.list.elem, seen),
        .box => |b| try self.monotypesStructurallyEqualRec(b.inner, rhs_mono.box.inner, seen),
        .tuple => |t| blk: {
            const lhs_elems = self.mir_store.monotype_store.getIdxSpan(t.elems);
            const rhs_elems = self.mir_store.monotype_store.getIdxSpan(rhs_mono.tuple.elems);
            if (lhs_elems.len != rhs_elems.len) break :blk false;
            for (lhs_elems, rhs_elems) |lhs_elem, rhs_elem| {
                if (!try self.monotypesStructurallyEqualRec(lhs_elem, rhs_elem, seen)) break :blk false;
            }
            break :blk true;
        },
        .func => |f| blk: {
            const rf = rhs_mono.func;
            if (f.effectful != rf.effectful) break :blk false;
            const lhs_args = self.mir_store.monotype_store.getIdxSpan(f.args);
            const rhs_args = self.mir_store.monotype_store.getIdxSpan(rf.args);
            if (lhs_args.len != rhs_args.len) break :blk false;
            for (lhs_args, rhs_args) |lhs_arg, rhs_arg| {
                if (!try self.monotypesStructurallyEqualRec(lhs_arg, rhs_arg, seen)) break :blk false;
            }
            break :blk try self.monotypesStructurallyEqualRec(f.ret, rf.ret, seen);
        },
        .record => |r| blk: {
            const lhs_fields = self.mir_store.monotype_store.getFields(r.fields);
            const rhs_fields = self.mir_store.monotype_store.getFields(rhs_mono.record.fields);
            if (lhs_fields.len != rhs_fields.len) break :blk false;
            for (lhs_fields, rhs_fields) |lhs_field, rhs_field| {
                if (!lhs_field.name.eql(rhs_field.name)) break :blk false;
                if (!try self.monotypesStructurallyEqualRec(lhs_field.type_idx, rhs_field.type_idx, seen)) {
                    break :blk false;
                }
            }
            break :blk true;
        },
        .tag_union => |tu| blk: {
            const lhs_tags = self.mir_store.monotype_store.getTags(tu.tags);
            const rhs_tags = self.mir_store.monotype_store.getTags(rhs_mono.tag_union.tags);
            if (lhs_tags.len != rhs_tags.len) break :blk false;
            for (lhs_tags, rhs_tags) |lhs_tag, rhs_tag| {
                if (!lhs_tag.name.eql(rhs_tag.name)) break :blk false;
                const lhs_payloads = self.mir_store.monotype_store.getIdxSpan(lhs_tag.payloads);
                const rhs_payloads = self.mir_store.monotype_store.getIdxSpan(rhs_tag.payloads);
                if (lhs_payloads.len != rhs_payloads.len) break :blk false;
                for (lhs_payloads, rhs_payloads) |lhs_payload, rhs_payload| {
                    if (!try self.monotypesStructurallyEqualRec(lhs_payload, rhs_payload, seen)) break :blk false;
                }
            }
            break :blk true;
        },
    };
}

/// Follow lookups/blocks/closure origins to recover the lambda parameter span.
fn resolveToLambdaParams(self: *Self, expr_id: MIR.ExprId) ?MIR.PatternSpan {
    const expr = self.mir_store.getExpr(expr_id);
    return switch (expr) {
        .lambda => |lam| lam.params,
        .block => |block| self.resolveToLambdaParams(block.final_expr),
        .lookup => |sym| blk: {
            const def_expr_id = self.mir_store.getSymbolDef(sym) orelse break :blk null;
            break :blk self.resolveToLambdaParams(def_expr_id);
        },
        else => null,
    };
}

fn functionParamSymbol(self: *Self, param_pat_id: MIR.PatternId) ?MIR.Symbol {
    const param_pat = self.mir_store.getPattern(param_pat_id);
    return switch (param_pat) {
        .bind => |sym| sym,
        .as_pattern => |as_pat| as_pat.symbol,
        .wildcard,
        .tag,
        .int_literal,
        .str_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .struct_destructure,
        .list_destructure,
        .runtime_error,
        => null,
    };
}

/// Adapt a function-typed call argument to the callee parameter's lambda-set layout.
/// This handles singleton closure values flowing into multi-member lambda-set params.
fn adaptFunctionArgToParamLambdaSet(
    self: *Self,
    arg_lir_expr: LirExprId,
    arg_mir_expr: MIR.ExprId,
    param_pat_id: MIR.PatternId,
    region: Region,
) Allocator.Error!LirExprId {
    const param_symbol = self.functionParamSymbol(param_pat_id) orelse return arg_lir_expr;
    const param_mono = self.mir_store.patternTypeOf(param_pat_id);
    if (self.mir_store.monotype_store.getMonotype(param_mono) != .func) return arg_lir_expr;

    const param_ls_idx = self.lambda_set_store.getSymbolLambdaSet(param_symbol) orelse return arg_lir_expr;
    var param_members = try self.snapshotLambdaSetMembers(param_ls_idx);
    defer param_members.deinit(self.allocator);
    if (param_members.items.len <= 1) return arg_lir_expr;

    const arg_ls_idx = self.lambdaSetForExpr(arg_mir_expr) orelse return arg_lir_expr;

    const arg_members = self.lambda_set_store.getMembers(self.lambda_set_store.getLambdaSet(arg_ls_idx).members);
    if (arg_members.len != 1) {
        if (std.debug.runtime_safety and arg_ls_idx != param_ls_idx) {
            std.debug.panic(
                "MirToLir invariant violated: unhandled closure arg coercion from lambda-set {d} to {d}",
                .{ @intFromEnum(arg_ls_idx), @intFromEnum(param_ls_idx) },
            );
        }
        return arg_lir_expr;
    }

    const arg_member = arg_members[0];
    var target_discriminant: ?u16 = null;
    for (param_members.items, 0..) |param_member, i| {
        if (param_member.fn_symbol.eql(arg_member.fn_symbol)) {
            target_discriminant = @intCast(i);
            break;
        }
    }

    const discr = target_discriminant orelse {
        if (std.debug.runtime_safety) {
            std.debug.panic(
                "MirToLir invariant violated: closure arg member fn={d} not present in param symbol={d} lambda set",
                .{ arg_member.fn_symbol.raw(), param_symbol.raw() },
            );
        }
        unreachable;
    };

    const target_layout = try self.closureValueLayoutFromLambdaSet(param_ls_idx);
    if (arg_member.closure_member.isNone()) {
        return self.lir_store.addExpr(.{ .zero_arg_tag = .{
            .discriminant = discr,
            .union_layout = target_layout,
        } }, region);
    }

    const target_payload_layout = try self.closureVariantPayloadLayout(target_layout, discr);
    const adapted_payload = try self.adaptConcreteClosureMemberPayload(
        arg_lir_expr,
        arg_member,
        target_payload_layout,
        region,
    );
    const payload_args = try self.lir_store.addExprSpan(&.{adapted_payload});
    return self.lir_store.addExpr(.{ .tag = .{
        .discriminant = discr,
        .union_layout = target_layout,
        .args = payload_args,
    } }, region);
}

/// Rewrite call arguments to match callee parameter lambda-set layouts when needed.
fn adaptClosureCallArgsToParams(
    self: *Self,
    callee_def_expr: MIR.ExprId,
    mir_args: []const MIR.ExprId,
    lir_user_args: LirExprSpan,
    region: Region,
) Allocator.Error!LirExprSpan {
    const params = self.resolveToLambdaParams(callee_def_expr) orelse return lir_user_args;
    const param_ids = self.mir_store.getPatternSpan(params);
    if (param_ids.len == 0 or mir_args.len == 0) return lir_user_args;

    const user_arg_ids = self.lir_store.getExprSpan(lir_user_args);
    if (user_arg_ids.len == 0) return lir_user_args;

    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

    // Copy existing args by index to avoid borrowing a slice that can be invalidated.
    for (user_arg_ids) |arg_id| {
        try self.scratch_lir_expr_ids.append(self.allocator, arg_id);
    }
    const input_len = user_arg_ids.len;
    const output_start = self.scratch_lir_expr_ids.items.len;

    var changed = false;
    for (0..input_len) |i| {
        const arg_id = self.scratch_lir_expr_ids.items[save_exprs + i];
        const adapted = if (i < mir_args.len and i < param_ids.len)
            try self.adaptFunctionArgToParamLambdaSet(arg_id, mir_args[i], param_ids[i], region)
        else
            arg_id;
        if (adapted != arg_id) changed = true;
        try self.scratch_lir_expr_ids.append(self.allocator, adapted);
    }

    if (!changed) return lir_user_args;
    return self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[output_start..]);
}

/// Generate dispatch for a call to a closure value using lambda set information.
/// For single-member lambda sets: direct call with captures as extra arg.
/// For multi-member lambda sets: discriminant_switch dispatching to each member.
fn lowerClosureCall(
    self: *Self,
    call_data: anytype,
    ls_idx: LambdaSet.Idx,
    callee_symbol: Symbol,
    ret_layout: layout.Idx,
    region: Region,
) Allocator.Error!LirExprId {
    var members = try self.snapshotLambdaSetMembers(ls_idx);
    defer members.deinit(self.allocator);

    if (members.items.len == 0) {
        if (std.debug.runtime_safety) {
            std.debug.panic("MirToLir: empty lambda set for symbol key={d}", .{callee_symbol.raw()});
        }
        unreachable;
    }

    // Lower user arguments (shared across all dispatch branches)
    var acc = self.startLetAccumulator();
    const mir_args = self.mir_store.getExprSpan(call_data.args);
    const lir_user_args = try self.lowerAnfSpan(&acc, mir_args, region);

    if (members.items.len == 1) {
        const member = members.items[0];
        const lifted_def = self.mir_store.getSymbolDef(member.fn_symbol) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic("MirToLir: missing def for lifted fn symbol key={d}", .{member.fn_symbol.raw()});
            }
            unreachable;
        };
        const lifted_mono = self.mir_store.typeOf(lifted_def);
        const lifted_layout = try self.layoutFromMonotype(lifted_mono);
        const call_user_args = try self.adaptClosureCallArgsToParams(lifted_def, mir_args, lir_user_args, region);
        const save_layouts = self.scratch_layout_idxs.items.len;
        defer self.scratch_layout_idxs.shrinkRetainingCapacity(save_layouts);
        try self.appendArgLayoutsForSpan(call_user_args);

        if (member.closure_member.isNone()) {
            const specialization = try self.ensureSpecializedDirectCallee(
                member.fn_symbol.raw(),
                member.fn_symbol,
                lifted_def,
                self.scratch_layout_idxs.items[save_layouts..],
            );
            const fn_expr = try self.lir_store.addExpr(.{ .lookup = .{
                .symbol = specialization.symbol,
                .layout_idx = lifted_layout,
            } }, region);
            // Zero-capture lambda: call with just user args, no extra captures param
            const call_expr = try self.lir_store.addExpr(.{ .call = .{
                .fn_expr = fn_expr,
                .fn_layout = lifted_layout,
                .args = call_user_args,
                .ret_layout = specialization.ret_layout,
                .called_via = .apply,
            } }, region);
            return acc.finish(call_expr, specialization.ret_layout, region);
        }

        // Has captures: lower closure val and append as extra arg
        const closure_val_raw = try self.lowerExpr(call_data.func);
        const closure_layout = try self.runtimeValueLayoutFromMirExpr(call_data.func);
        const closure_val = try acc.ensureSymbol(closure_val_raw, closure_layout, region);
        const captures_arg = try acc.bindRetained(closure_val, closure_layout, region);
        try self.scratch_layout_idxs.append(self.allocator, self.lirExprResultLayout(captures_arg));
        const specialization = try self.ensureSpecializedDirectCallee(
            member.fn_symbol.raw(),
            member.fn_symbol,
            lifted_def,
            self.scratch_layout_idxs.items[save_layouts..],
        );
        const fn_expr = try self.lir_store.addExpr(.{ .lookup = .{
            .symbol = specialization.symbol,
            .layout_idx = lifted_layout,
        } }, region);

        // Build args: [user_args..., closure_val]
        // Re-read the span after lowering closure_val; lowerExpr may append to
        // extra_data and invalidate previously borrowed slices.
        const user_arg_ids = self.lir_store.getExprSpan(call_user_args);
        const save_exprs = self.scratch_lir_expr_ids.items.len;
        defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);
        for (user_arg_ids) |arg_id| {
            try self.scratch_lir_expr_ids.append(self.allocator, arg_id);
        }
        try self.scratch_lir_expr_ids.append(self.allocator, captures_arg);
        const all_args = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);

        const call_expr = try self.lir_store.addExpr(.{ .call = .{
            .fn_expr = fn_expr,
            .fn_layout = lifted_layout,
            .args = all_args,
            .ret_layout = specialization.ret_layout,
            .called_via = .apply,
        } }, region);
        return acc.finish(call_expr, specialization.ret_layout, region);
    }

    // Multi-member lambda set: discriminant_switch on the tag union closure value
    const closure_val_raw = try self.lowerExpr(call_data.func);
    const closure_layout = try self.runtimeClosureDispatchLayoutForExpr(call_data.func, ls_idx);
    const closure_val = try acc.ensureSymbol(closure_val_raw, closure_layout, region);

    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

    for (members.items, 0..) |member, branch_index| {
        const lifted_def = self.mir_store.getSymbolDef(member.fn_symbol) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic("MirToLir: missing def for lifted fn symbol key={d}", .{member.fn_symbol.raw()});
            }
            unreachable;
        };
        const lifted_mono = self.mir_store.typeOf(lifted_def);
        const lifted_layout = try self.layoutFromMonotype(lifted_mono);
        const branch_user_args = try self.adaptClosureCallArgsToParams(lifted_def, mir_args, lir_user_args, region);
        const save_layouts = self.scratch_layout_idxs.items.len;
        defer self.scratch_layout_idxs.shrinkRetainingCapacity(save_layouts);
        try self.appendArgLayoutsForSpan(branch_user_args);

        // Re-read on each iteration: lowering lifted defs can append to
        // extra_data and invalidate previously borrowed slices.
        const user_arg_ids = self.lir_store.getExprSpan(branch_user_args);
        const inner_save = self.scratch_lir_expr_ids.items.len;
        for (user_arg_ids) |arg_id| {
            try self.scratch_lir_expr_ids.append(self.allocator, arg_id);
        }

        var branch_acc = self.startLetAccumulator();
        if (!member.closure_member.isNone()) {
            // Extract payload (captures) from the tag union
            const captures_layout = try self.closureVariantPayloadLayout(closure_layout, branch_index);
            const payload_expr = try self.lir_store.addExpr(.{ .tag_payload_access = .{
                .value = closure_val,
                .union_layout = closure_layout,
                .payload_layout = captures_layout,
            } }, region);
            const payload_arg = try branch_acc.bindRetained(payload_expr, captures_layout, region);
            try self.scratch_lir_expr_ids.append(self.allocator, payload_arg);
            try self.scratch_layout_idxs.append(self.allocator, self.lirExprResultLayout(payload_arg));
        }

        const branch_args = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[inner_save..]);
        self.scratch_lir_expr_ids.shrinkRetainingCapacity(inner_save);
        const specialization = try self.ensureSpecializedDirectCallee(
            member.fn_symbol.raw(),
            member.fn_symbol,
            lifted_def,
            self.scratch_layout_idxs.items[save_layouts..],
        );
        const fn_lookup = try self.lir_store.addExpr(.{ .lookup = .{
            .symbol = specialization.symbol,
            .layout_idx = lifted_layout,
        } }, region);

        const branch_call = try self.lir_store.addExpr(.{ .call = .{
            .fn_expr = fn_lookup,
            .fn_layout = lifted_layout,
            .args = branch_args,
            .ret_layout = specialization.ret_layout,
            .called_via = .apply,
        } }, region);

        const branch_expr = try branch_acc.finish(branch_call, specialization.ret_layout, region);
        try self.scratch_lir_expr_ids.append(self.allocator, branch_expr);
    }

    const branch_exprs = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);
    const switch_expr = try self.lir_store.addExpr(.{ .discriminant_switch = .{
        .value = closure_val,
        .union_layout = closure_layout,
        .branches = branch_exprs,
        .result_layout = ret_layout,
    } }, region);
    return acc.finish(switch_expr, ret_layout, region);
}

fn lowerBlock(self: *Self, block_data: anytype, _: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const LoweredStmtInfo = struct {
        pattern: LirPatternId = LirPatternId.none,
        deferred_rest_start: usize = 0,
        deferred_rest_len: usize = 0,
        rewrite: ?TopLevelRestBindingRewrite = null,
        cell_symbol: Symbol = Symbol.none,
        cell_layout: layout.Idx = .none,
    };

    const result_layout = try self.runtimeLayoutForBlockFinal(block_data);

    const mir_stmts = self.mir_store.getStmts(block_data.stmts);
    const save_stmts_len = self.scratch_lir_stmts.items.len;
    const save_pattern_ids_len = self.scratch_lir_pattern_ids.items.len;
    const save_rest_bindings = self.scratch_deferred_list_rest_bindings.items.len;
    var binding_infos = std.ArrayList(LoweredStmtInfo).empty;
    defer binding_infos.deinit(self.allocator);
    defer self.scratch_lir_stmts.shrinkRetainingCapacity(save_stmts_len);
    defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_pattern_ids_len);
    defer self.scratch_deferred_list_rest_bindings.shrinkRetainingCapacity(save_rest_bindings);

    // Pass 1: Lower all binding patterns first so symbol->layout registrations
    // are available to all statement expressions (including forward captures in
    // mutually-recursive closures inside the same block).
    for (mir_stmts) |stmt| {
        const binding = switch (stmt) {
            .decl_const, .decl_var, .mutate_var => |b| b,
        };
        switch (stmt) {
            .decl_const => {
                const runtime_layout = try self.runtimeValueLayoutFromMirExpr(binding.expr);
                try self.registerBindingPatternSymbols(binding.pattern, runtime_layout);
                const lowered = try self.lowerBindingPatternForRuntimeLayout(binding.pattern, runtime_layout, .owned, region);
                const rewrite = try self.rewriteTopLevelRestBinding(lowered, runtime_layout, .owned, region);
                try binding_infos.append(self.allocator, .{
                    .pattern = if (rewrite) |rw| rw.source_pattern else lowered.pattern,
                    .deferred_rest_start = lowered.deferred_rest_start,
                    .deferred_rest_len = lowered.deferred_rest_len,
                    .rewrite = rewrite,
                });
                try self.scratch_lir_pattern_ids.append(self.allocator, lowered.pattern);
            },
            .decl_var => {
                const runtime_layout = try self.runtimeValueLayoutFromMirExpr(binding.expr);
                try self.registerBindingPatternSymbols(binding.pattern, runtime_layout);
                const cell_symbol = bindingPatternSymbol(self.mir_store, binding.pattern) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("mutable MIR binding requires bind/as_pattern, got {s}", .{@tagName(self.mir_store.getPattern(binding.pattern))});
                    }
                    unreachable;
                };
                try self.symbol_layouts.put(cell_symbol.raw(), runtime_layout);
                try binding_infos.append(self.allocator, .{
                    .cell_symbol = cell_symbol,
                    .cell_layout = runtime_layout,
                });
            },
            .mutate_var => {
                const cell_symbol = bindingPatternSymbol(self.mir_store, binding.pattern) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("mutable MIR binding requires bind/as_pattern, got {s}", .{@tagName(self.mir_store.getPattern(binding.pattern))});
                    }
                    unreachable;
                };
                const cell_layout = self.symbol_layouts.get(cell_symbol.raw()) orelse try self.runtimeValueLayoutFromMirExpr(binding.expr);
                try self.symbol_layouts.put(cell_symbol.raw(), cell_layout);
                try binding_infos.append(self.allocator, .{
                    .cell_symbol = cell_symbol,
                    .cell_layout = cell_layout,
                });
            },
        }
    }

    // Pass 2: Lower expressions and assemble statements using cached patterns.
    for (mir_stmts, 0..) |stmt, i| {
        const binding = switch (stmt) {
            .decl_const, .decl_var, .mutate_var => |b| b,
        };
        const lowered_expr = try self.lowerExpr(binding.expr);
        const lir_expr = try self.adaptExprToRuntimeLayout(binding.expr, lowered_expr, region);
        const binding_semantics = if (stmt == .decl_const)
            self.bindingSemanticsForExpr(
                lir_expr,
                try self.runtimeValueLayoutFromMirExpr(binding.expr),
            )
        else
            LirStmt.BindingSemantics.owned;
        if (stmt == .decl_const) {
            const ownership_mode = bindingModeForSemantics(binding_semantics);
            try self.updatePatternBindingMode(binding_infos.items[i].pattern, ownership_mode);

            const def_expr = self.lir_store.getExpr(lir_expr);
            const is_callable_def = switch (def_expr) {
                .lambda => true,
                else => false,
            };
            if (is_callable_def) {
                const mir_pat = self.mir_store.getPattern(binding.pattern);
                switch (mir_pat) {
                    .bind => |sym| {
                        if (self.lir_store.getSymbolDef(sym) == null) {
                            try self.lir_store.registerSymbolDef(sym, lir_expr);
                        }
                    },
                    .as_pattern => |as_pat| {
                        if (self.lir_store.getSymbolDef(as_pat.symbol) == null) {
                            try self.lir_store.registerSymbolDef(as_pat.symbol, lir_expr);
                        }
                    },
                    // Lambda defs bound to destructuring patterns, wildcards, or literals
                    // don't need symbol registration — there's no named symbol to look up.
                    .wildcard,
                    .tag,
                    .int_literal,
                    .str_literal,
                    .dec_literal,
                    .frac_f32_literal,
                    .frac_f64_literal,
                    .struct_destructure,
                    .list_destructure,
                    .runtime_error,
                    => {},
                }
            }
        }
        switch (stmt) {
            .decl_const => {
                const lir_binding: LirStmt.Binding = .{
                    .pattern = binding_infos.items[i].pattern,
                    .expr = lir_expr,
                    .semantics = binding_semantics,
                };
                try self.scratch_lir_stmts.append(self.allocator, .{ .decl = lir_binding });
                if (binding_infos.items[i].rewrite) |rw| {
                    const source_lookup = try self.lir_store.addExpr(.{ .lookup = .{
                        .symbol = rw.source_symbol,
                        .layout_idx = rw.source_layout,
                    } }, region);
                    try self.scratch_lir_stmts.append(self.allocator, .{ .decl = .{
                        .pattern = rw.destructure_pattern,
                        .expr = source_lookup,
                        .semantics = .borrow_alias,
                    } });
                }
                try self.appendDeferredListRestBindingDecls(
                    binding_infos.items[i].deferred_rest_start,
                    binding_infos.items[i].deferred_rest_len,
                    region,
                );
            },
            .decl_var => {
                try self.scratch_lir_stmts.append(self.allocator, .{ .cell_init = .{
                    .cell = binding_infos.items[i].cell_symbol,
                    .layout_idx = binding_infos.items[i].cell_layout,
                    .expr = lir_expr,
                } });
            },
            .mutate_var => {
                try self.scratch_lir_stmts.append(self.allocator, .{ .cell_store = .{
                    .cell = binding_infos.items[i].cell_symbol,
                    .layout_idx = binding_infos.items[i].cell_layout,
                    .expr = lir_expr,
                } });
            },
        }
    }

    const lir_stmts = try self.lir_store.addStmts(self.scratch_lir_stmts.items[save_stmts_len..]);
    const lowered_final = try self.lowerExpr(block_data.final_expr);
    const lir_final = try self.adaptExprToRuntimeLayout(block_data.final_expr, lowered_final, region);

    return self.lir_store.addExpr(.{ .block = .{
        .stmts = lir_stmts,
        .final_expr = lir_final,
        .result_layout = result_layout,
    } }, region);
}

fn bindingPatternSymbol(mir_store: *const MIR.Store, pattern_id: MIR.PatternId) ?Symbol {
    return switch (mir_store.getPattern(pattern_id)) {
        .bind => |sym| sym,
        .as_pattern => |as_pat| as_pat.symbol,
        else => null,
    };
}

fn lowerBorrowScope(self: *Self, scope_data: anytype, _: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const result_layout = try self.runtimeValueLayoutFromMirExpr(scope_data.body);
    const mir_bindings = self.mir_store.getBorrowBindings(scope_data.bindings);

    const save_len = self.scratch_lir_stmts.items.len;
    const save_rest_bindings = self.scratch_deferred_list_rest_bindings.items.len;
    var binding_infos = std.ArrayList(LoweredBindingPattern).empty;
    defer binding_infos.deinit(self.allocator);
    defer self.scratch_lir_stmts.shrinkRetainingCapacity(save_len);
    defer self.scratch_deferred_list_rest_bindings.shrinkRetainingCapacity(save_rest_bindings);

    for (mir_bindings) |binding| {
        const runtime_layout = try self.runtimeValueLayoutFromMirExpr(binding.expr);
        try self.registerBindingPatternSymbols(binding.pattern, runtime_layout);
        const lowered = try self.lowerBindingPatternForRuntimeLayout(binding.pattern, runtime_layout, .borrowed, region);
        const lowered_expr = try self.lowerExpr(binding.expr);
        const lir_expr = try self.adaptExprToRuntimeLayout(binding.expr, lowered_expr, region);
        const source_semantics = self.borrowBindingSemanticsForExpr(lir_expr, runtime_layout);
        const rewrite = (try self.rewriteTopLevelRestBinding(lowered, runtime_layout, .borrowed, region)) orelse
            if (source_semantics == .scoped_borrow)
                try self.rewriteBorrowedBindingSource(lowered, runtime_layout, region)
            else
                null;
        try binding_infos.append(self.allocator, lowered);
        try self.scratch_lir_stmts.append(self.allocator, .{ .decl = .{
            .pattern = if (rewrite) |rw| rw.source_pattern else lowered.pattern,
            .expr = lir_expr,
            .semantics = source_semantics,
        } });
        if (rewrite) |rw| {
            const source_lookup = try self.lir_store.addExpr(.{ .lookup = .{
                .symbol = rw.source_symbol,
                .layout_idx = rw.source_layout,
            } }, region);
            if (!rw.destructure_pattern.isNone()) {
                try self.scratch_lir_stmts.append(self.allocator, .{ .decl = .{
                    .pattern = rw.destructure_pattern,
                    .expr = source_lookup,
                    .semantics = .borrow_alias,
                } });
            }
        }
    }

    const lowered_body = try self.lowerExpr(scope_data.body);
    var lir_body = try self.adaptExprToRuntimeLayout(scope_data.body, lowered_body, region);
    var borrow_binding_idx = binding_infos.items.len;
    while (borrow_binding_idx > 0) {
        borrow_binding_idx -= 1;
        const info = binding_infos.items[borrow_binding_idx];
        lir_body = try self.wrapExprWithDeferredListRestBindings(
            lir_body,
            result_layout,
            info.deferred_rest_start,
            info.deferred_rest_len,
            region,
        );
    }
    const lir_stmts = self.scratch_lir_stmts.items[save_len..];
    if (lir_stmts.len == 0) return lir_body;
    return self.lir_store.addExpr(.{ .block = .{
        .stmts = try self.lir_store.addStmts(lir_stmts),
        .final_expr = lir_body,
        .result_layout = result_layout,
    } }, region);
}

fn lowerRecordAccess(self: *Self, struct_expr: MIR.ExprId, field_idx: u32, _: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const struct_layout = try self.runtimeValueLayoutFromMirExpr(struct_expr);
    const struct_layout_val = self.layout_store.getLayout(struct_layout);
    if (builtin.mode == .Debug and struct_layout_val.tag != .struct_) {
        std.debug.panic(
            "MirToLir invariant violated: record_access expects struct_ runtime layout, got {s}",
            .{@tagName(struct_layout_val.tag)},
        );
    }
    if (struct_layout_val.tag != .struct_) unreachable;

    var acc = self.startLetAccumulator();
    const lir_struct_raw = try self.lowerExpr(struct_expr);
    const lir_struct = try acc.ensureSymbol(lir_struct_raw, struct_layout, region);
    const field_info = self.structFieldInfoByOriginalIndex(struct_layout, field_idx) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "MirToLir invariant violated: record_access field index {d} missing from runtime layout {d}",
                .{ field_idx, @intFromEnum(struct_layout) },
            );
        }
        unreachable;
    };

    const access_expr = try self.lir_store.addExpr(.{ .struct_access = .{
        .struct_expr = lir_struct,
        .struct_layout = struct_layout,
        .field_layout = field_info.field_layout,
        .field_idx = field_info.field_idx,
    } }, region);
    return acc.finish(access_expr, field_info.field_layout, region);
}

fn lowerTupleAccess(self: *Self, struct_expr: MIR.ExprId, field_idx: u32, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const struct_layout = try self.runtimeValueLayoutFromMirExpr(struct_expr);
    const struct_layout_val = self.layout_store.getLayout(struct_layout);
    if (builtin.mode == .Debug and struct_layout_val.tag != .struct_) {
        std.debug.panic(
            "MirToLir invariant violated: tuple_access expects struct_ runtime layout, got {s}",
            .{@tagName(struct_layout_val.tag)},
        );
    }
    if (struct_layout_val.tag != .struct_) unreachable;

    var acc = self.startLetAccumulator();
    const lir_struct_raw = try self.lowerExpr(struct_expr);
    const lir_struct = try acc.ensureSymbol(lir_struct_raw, struct_layout, region);
    const field_info = self.structFieldInfoByOriginalIndex(struct_layout, field_idx) orelse StructFieldInfo{
        .field_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id),
        .field_idx = 0,
    };

    const access_expr = try self.lir_store.addExpr(.{ .struct_access = .{
        .struct_expr = lir_struct,
        .struct_layout = struct_layout,
        .field_layout = field_info.field_layout,
        .field_idx = field_info.field_idx,
    } }, region);
    return acc.finish(access_expr, field_info.field_layout, region);
}

const StructFieldInfo = struct {
    field_layout: layout.Idx,
    field_idx: u16,
};

fn structFieldInfoByOriginalIndex(self: *Self, struct_layout: layout.Idx, original_index: u32) ?StructFieldInfo {
    const struct_layout_val = self.layout_store.getLayout(struct_layout);
    if (struct_layout_val.tag != .struct_) {
        if (original_index == 0) {
            return .{ .field_layout = struct_layout, .field_idx = 0 };
        }
        return null;
    }

    const struct_data = self.layout_store.getStructData(struct_layout_val.data.struct_.idx);
    const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
    for (0..layout_fields.len) |li| {
        const layout_field = layout_fields.get(li);
        if (layout_field.index != original_index) continue;
        return .{
            .field_layout = layout_field.layout,
            .field_idx = @intCast(li),
        };
    }
    return null;
}

fn adaptValueLayout(
    self: *Self,
    value_expr: LirExprId,
    mono_idx: Monotype.Idx,
    source_layout: layout.Idx,
    target_layout: layout.Idx,
    region: Region,
) Allocator.Error!LirExprId {
    if (source_layout == target_layout) return value_expr;

    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    return switch (monotype) {
        .record => |record| self.adaptRecordValueLayout(value_expr, record, source_layout, target_layout, region),
        .tuple => |tuple| self.adaptTupleValueLayout(value_expr, tuple, source_layout, target_layout, region),
        .func => self.adaptLayoutByStructure(value_expr, source_layout, target_layout, region),
        .prim, .unit, .tag_union, .list, .box, .recursive_placeholder => value_expr,
    };
}

fn adaptLayoutByStructure(
    self: *Self,
    value_expr: LirExprId,
    source_layout: layout.Idx,
    target_layout: layout.Idx,
    region: Region,
) Allocator.Error!LirExprId {
    if (source_layout == target_layout) return value_expr;

    switch (self.lir_store.getExpr(value_expr)) {
        .lookup => |lookup| {
            if (self.lir_store.getSymbolDef(lookup.symbol)) |def_expr_id| {
                return self.adaptLayoutByStructure(
                    def_expr_id,
                    self.lirExprResultLayout(def_expr_id),
                    target_layout,
                    region,
                );
            }
        },
        .block => |block| {
            return self.adaptLayoutByStructure(
                block.final_expr,
                self.lirExprResultLayout(block.final_expr),
                target_layout,
                region,
            );
        },
        .nominal => |nominal| {
            return self.adaptLayoutByStructure(
                nominal.backing_expr,
                self.lirExprResultLayout(nominal.backing_expr),
                target_layout,
                region,
            );
        },
        else => {},
    }

    const source_layout_val = self.layout_store.getLayout(source_layout);
    const target_layout_val = self.layout_store.getLayout(target_layout);
    if (source_layout_val.tag != .struct_ or target_layout_val.tag != .struct_) return value_expr;

    var acc = self.startLetAccumulator();
    const source_value = try acc.ensureSymbol(value_expr, source_layout, region);

    const target_struct_data = self.layout_store.getStructData(target_layout_val.data.struct_.idx);
    const target_fields = self.layout_store.struct_fields.sliceRange(target_struct_data.getFields());

    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

    for (0..target_fields.len) |li| {
        const target_field = target_fields.get(li);
        const source_field = self.structFieldInfoByOriginalIndex(source_layout, target_field.index) orelse return value_expr;
        const field_expr = try self.lir_store.addExpr(.{ .struct_access = .{
            .struct_expr = source_value,
            .struct_layout = source_layout,
            .field_layout = source_field.field_layout,
            .field_idx = source_field.field_idx,
        } }, region);
        const adapted_field = try self.adaptLayoutByStructure(
            field_expr,
            source_field.field_layout,
            target_field.layout,
            region,
        );
        const ensured = try acc.ensureSymbol(adapted_field, target_field.layout, region);
        try self.scratch_lir_expr_ids.append(self.allocator, ensured);
    }

    const lir_fields = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);
    const struct_expr = try self.lir_store.addExpr(.{ .struct_ = .{
        .struct_layout = target_layout,
        .fields = lir_fields,
    } }, region);
    return acc.finish(struct_expr, target_layout, region);
}

const UnwrappedClosurePayload = struct {
    expr: LirExprId,
    layout: layout.Idx,
};

fn unwrapClosurePayloadExpr(self: *Self, value_expr: LirExprId) UnwrappedClosurePayload {
    return switch (self.lir_store.getExpr(value_expr)) {
        .lookup => |lookup| blk: {
            if (self.lir_store.getSymbolDef(lookup.symbol)) |def_expr_id| {
                break :blk self.unwrapClosurePayloadExpr(def_expr_id);
            }
            break :blk .{ .expr = value_expr, .layout = self.lirExprResultLayout(value_expr) };
        },
        .block => |block| self.unwrapClosurePayloadExpr(block.final_expr),
        .nominal => |nominal| self.unwrapClosurePayloadExpr(nominal.backing_expr),
        else => .{ .expr = value_expr, .layout = self.lirExprResultLayout(value_expr) },
    };
}

fn adaptConcreteClosureMemberPayload(
    self: *Self,
    value_expr: LirExprId,
    member: LambdaSet.Member,
    target_layout: layout.Idx,
    region: Region,
) Allocator.Error!LirExprId {
    if (member.closure_member.isNone()) return value_expr;

    const closure_member = self.mir_store.getClosureMember(member.closure_member);
    const capture_bindings = self.mir_store.getCaptureBindings(closure_member.capture_bindings);
    if (capture_bindings.len == 0) return value_expr;

    const payload = self.unwrapClosurePayloadExpr(value_expr);
    if (payload.layout == target_layout) return payload.expr;

    if (capture_bindings.len == 1) {
        const structurally_adapted = try self.adaptLayoutByStructure(
            payload.expr,
            payload.layout,
            target_layout,
            region,
        );
        if (self.lirExprResultLayout(structurally_adapted) == target_layout) {
            return structurally_adapted;
        }

        return self.adaptValueLayout(
            payload.expr,
            capture_bindings[0].monotype,
            payload.layout,
            target_layout,
            region,
        );
    }

    const source_layout_val = self.layout_store.getLayout(payload.layout);
    const target_layout_val = self.layout_store.getLayout(target_layout);
    if (source_layout_val.tag != .struct_ or target_layout_val.tag != .struct_) return payload.expr;

    var acc = self.startLetAccumulator();
    const source_value = try acc.ensureSymbol(payload.expr, payload.layout, region);

    const target_struct_data = self.layout_store.getStructData(target_layout_val.data.struct_.idx);
    const target_fields = self.layout_store.struct_fields.sliceRange(target_struct_data.getFields());

    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

    for (capture_bindings, 0..) |capture_binding, semantic_index| {
        var target_field_layout: layout.Idx = target_layout;
        for (0..target_fields.len) |li| {
            const target_field = target_fields.get(li);
            if (target_field.index != semantic_index) continue;
            target_field_layout = target_field.layout;
            break;
        }
        const source_field = self.structFieldInfoByOriginalIndex(payload.layout, @intCast(semantic_index)) orelse return payload.expr;
        const field_expr = try self.lir_store.addExpr(.{ .struct_access = .{
            .struct_expr = source_value,
            .struct_layout = payload.layout,
            .field_layout = source_field.field_layout,
            .field_idx = source_field.field_idx,
        } }, region);
        const adapted_field = try self.adaptValueLayout(
            field_expr,
            capture_binding.monotype,
            source_field.field_layout,
            target_field_layout,
            region,
        );
        const ensured = try acc.ensureSymbol(adapted_field, target_field_layout, region);
        try self.scratch_lir_expr_ids.append(self.allocator, ensured);
    }

    const lir_fields = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);
    const struct_expr = try self.lir_store.addExpr(.{ .struct_ = .{
        .struct_layout = target_layout,
        .fields = lir_fields,
    } }, region);
    return acc.finish(struct_expr, target_layout, region);
}

fn adaptRecordValueLayout(
    self: *Self,
    value_expr: LirExprId,
    record: anytype,
    source_layout: layout.Idx,
    target_layout: layout.Idx,
    region: Region,
) Allocator.Error!LirExprId {
    const fields = self.mir_store.monotype_store.getFields(record.fields);
    if (fields.len == 0) return value_expr;
    const source_layout_val = self.layout_store.getLayout(source_layout);
    const target_layout_val = self.layout_store.getLayout(target_layout);
    if (builtin.mode == .Debug and (source_layout_val.tag != .struct_ or target_layout_val.tag != .struct_)) {
        std.debug.panic(
            "MirToLir invariant violated: non-empty record layout adaptation requires struct_ source/target layouts, got {s} -> {s}",
            .{ @tagName(source_layout_val.tag), @tagName(target_layout_val.tag) },
        );
    }
    if (source_layout_val.tag != .struct_ or target_layout_val.tag != .struct_) return value_expr;

    var acc = self.startLetAccumulator();
    const source_value = try acc.ensureSymbol(value_expr, source_layout, region);
    const target_struct_data = self.layout_store.getStructData(target_layout_val.data.struct_.idx);
    const target_fields = self.layout_store.struct_fields.sliceRange(target_struct_data.getFields());

    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

    for (0..target_fields.len) |li| {
        const target_field = target_fields.get(li);
        const semantic_index = target_field.index;
        const source_field = self.structFieldInfoByOriginalIndex(source_layout, semantic_index) orelse unreachable;
        const field_expr = try self.lir_store.addExpr(.{ .struct_access = .{
            .struct_expr = source_value,
            .struct_layout = source_layout,
            .field_layout = source_field.field_layout,
            .field_idx = source_field.field_idx,
        } }, region);
        const adapted_field = try self.adaptValueLayout(
            field_expr,
            fields[semantic_index].type_idx,
            source_field.field_layout,
            target_field.layout,
            region,
        );
        const ensured = try acc.ensureSymbol(adapted_field, target_field.layout, region);
        try self.scratch_lir_expr_ids.append(self.allocator, ensured);
    }

    const lir_fields = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);
    const struct_expr = try self.lir_store.addExpr(.{ .struct_ = .{
        .struct_layout = target_layout,
        .fields = lir_fields,
    } }, region);
    return acc.finish(struct_expr, target_layout, region);
}

fn adaptTupleValueLayout(
    self: *Self,
    value_expr: LirExprId,
    tuple: anytype,
    source_layout: layout.Idx,
    target_layout: layout.Idx,
    region: Region,
) Allocator.Error!LirExprId {
    const elems = self.mir_store.monotype_store.getIdxSpan(tuple.elems);
    if (elems.len == 0) return value_expr;
    const source_layout_val = self.layout_store.getLayout(source_layout);
    const target_layout_val = self.layout_store.getLayout(target_layout);
    if (builtin.mode == .Debug and (source_layout_val.tag != .struct_ or target_layout_val.tag != .struct_)) {
        std.debug.panic(
            "MirToLir invariant violated: non-empty tuple layout adaptation requires struct_ source/target layouts, got {s} -> {s}",
            .{ @tagName(source_layout_val.tag), @tagName(target_layout_val.tag) },
        );
    }
    if (source_layout_val.tag != .struct_ or target_layout_val.tag != .struct_) return value_expr;

    var acc = self.startLetAccumulator();
    const source_value = try acc.ensureSymbol(value_expr, source_layout, region);
    const target_struct_data = self.layout_store.getStructData(target_layout_val.data.struct_.idx);
    const target_fields = self.layout_store.struct_fields.sliceRange(target_struct_data.getFields());

    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

    for (0..target_fields.len) |li| {
        const target_field = target_fields.get(li);
        const semantic_index = target_field.index;
        const source_field = self.structFieldInfoByOriginalIndex(source_layout, semantic_index) orelse unreachable;
        const field_expr = try self.lir_store.addExpr(.{ .struct_access = .{
            .struct_expr = source_value,
            .struct_layout = source_layout,
            .field_layout = source_field.field_layout,
            .field_idx = source_field.field_idx,
        } }, region);
        const adapted_field = try self.adaptValueLayout(
            field_expr,
            elems[semantic_index],
            source_field.field_layout,
            target_field.layout,
            region,
        );
        const ensured = try acc.ensureSymbol(adapted_field, target_field.layout, region);
        try self.scratch_lir_expr_ids.append(self.allocator, ensured);
    }

    const lir_fields = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);
    const struct_expr = try self.lir_store.addExpr(.{ .struct_ = .{
        .struct_layout = target_layout,
        .fields = lir_fields,
    } }, region);
    return acc.finish(struct_expr, target_layout, region);
}

fn lowerLowLevel(self: *Self, ll: anytype, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const ret_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const mir_args = self.mir_store.getExprSpan(ll.args);
    const low_level_ret_layout = switch (ll.op) {
        .list_get_unsafe => try self.runtimeListElemLayoutFromMirExpr(mir_args[0]),
        else => ret_layout,
    };
    var acc = self.startLetAccumulator();
    const arg_ownership = ll.op.getArgOwnership();
    if (builtin.mode == .Debug and arg_ownership.len != mir_args.len) {
        std.debug.panic(
            "MIR->LIR invariant violated: low-level {s} expected {d} ownership entries for {d} args",
            .{ @tagName(ll.op), arg_ownership.len, mir_args.len },
        );
    }

    const save_expr_len = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_expr_len);

    for (mir_args, 0..) |mir_arg, i| {
        const lowered_arg = try self.lowerExpr(mir_arg);
        const arg_layout = try self.runtimeValueLayoutFromMirExpr(mir_arg);
        const ownership = arg_ownership[i];

        const ensured_arg = switch (ownership) {
            .borrow => blk: {
                if (self.isBorrowAtomicExpr(lowered_arg)) break :blk lowered_arg;
                break :blk try acc.bindBorrow(lowered_arg, arg_layout, region);
            },
            .consume => blk: {
                const source_arg = try acc.ensureSymbol(lowered_arg, arg_layout, region);
                const owned_arg = if (self.exprAliasesManagedRef(source_arg, arg_layout))
                    try acc.bindRetained(source_arg, arg_layout, region)
                else
                    source_arg;
                break :blk owned_arg;
            },
        };

        try self.scratch_lir_expr_ids.append(self.allocator, ensured_arg);
    }

    const lir_args = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_expr_len..]);

    // str_inspekt should have been fully expanded during CIR->MIR lowering.
    // MIR uses an explicit `str_escape_and_quote` expression for string quoting.
    if (ll.op == .str_inspekt) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "MIR->LIR invariant violated: run_low_level(str_inspekt) should never appear after CIR->MIR lowering",
                .{},
            );
        }
        unreachable;
    }

    // *_to_str → typed int_to_str / float_to_str / dec_to_str expressions
    const result = switch (ll.op) {
        .u8_to_str, .i8_to_str, .u16_to_str, .i16_to_str, .u32_to_str, .i32_to_str, .u64_to_str, .i64_to_str, .u128_to_str, .i128_to_str => blk: {
            const args_slice = self.lir_store.getExprSpan(lir_args);
            const precision: @import("types").Int.Precision = switch (ll.op) {
                .u8_to_str => .u8,
                .i8_to_str => .i8,
                .u16_to_str => .u16,
                .i16_to_str => .i16,
                .u32_to_str => .u32,
                .i32_to_str => .i32,
                .u64_to_str => .u64,
                .i64_to_str => .i64,
                .u128_to_str => .u128,
                .i128_to_str => .i128,
                else => unreachable,
            };
            break :blk try self.lir_store.addExpr(.{ .int_to_str = .{
                .value = args_slice[0],
                .int_precision = precision,
            } }, region);
        },
        .f32_to_str, .f64_to_str => blk: {
            const args_slice = self.lir_store.getExprSpan(lir_args);
            const precision: @import("types").Frac.Precision = switch (ll.op) {
                .f32_to_str => .f32,
                .f64_to_str => .f64,
                else => unreachable,
            };
            break :blk try self.lir_store.addExpr(.{ .float_to_str = .{
                .value = args_slice[0],
                .float_precision = precision,
            } }, region);
        },
        .dec_to_str => blk: {
            const args_slice = self.lir_store.getExprSpan(lir_args);
            break :blk try self.lir_store.addExpr(.{ .dec_to_str = args_slice[0] }, region);
        },
        else => blk: {
            break :blk try self.lir_store.addExpr(.{ .low_level = .{
                .op = ll.op,
                .args = lir_args,
                .ret_layout = low_level_ret_layout,
            } }, region);
        },
    };
    const final_result = result;
    const adapted_result = if (low_level_ret_layout == ret_layout)
        final_result
    else
        try self.adaptValueLayout(final_result, self.mir_store.typeOf(mir_expr_id), low_level_ret_layout, ret_layout, region);

    return acc.finish(adapted_result, ret_layout, region);
}

fn lowerHosted(self: *Self, h: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    // h.body and h.symbol_name are intentionally unused. The MIR hosted body
    // just forwards parameters to a host call — we reconstruct that directly
    // as a hosted_call expression using h.index (the host function's dispatch
    // index). The string name is only needed for linking, handled separately.
    const fn_layout = try self.layoutFromMonotype(mono_idx);
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    const ret_layout = switch (monotype) {
        .func => |f| try self.layoutFromMonotype(f.ret),
        .prim, .unit, .record, .tuple, .tag_union, .list, .box, .recursive_placeholder => unreachable, // Hosted expressions always have .func monotype
    };

    const func_args = switch (monotype) {
        .func => |f| self.mir_store.monotype_store.getIdxSpan(f.args),
        .prim, .unit, .record, .tuple, .tag_union, .list, .box, .recursive_placeholder => unreachable,
    };
    const mir_params = self.mir_store.getPatternSpan(h.params);
    const save_rest_bindings = self.scratch_deferred_list_rest_bindings.items.len;
    defer self.scratch_deferred_list_rest_bindings.shrinkRetainingCapacity(save_rest_bindings);

    var param_infos = std.ArrayList(LoweredBindingPattern).empty;
    defer param_infos.deinit(self.allocator);
    var param_rewrites = std.ArrayList(?TopLevelRestBindingRewrite).empty;
    defer param_rewrites.deinit(self.allocator);

    const save_param_patterns = self.scratch_lir_pattern_ids.items.len;
    defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_param_patterns);
    for (mir_params, 0..) |mir_param_id, i| {
        const param_layout = if (i < func_args.len)
            try self.layoutFromMonotype(func_args[i])
        else
            unreachable;
        try self.registerBindingPatternSymbols(mir_param_id, param_layout);
        const lowered = try self.lowerBindingPatternForRuntimeLayout(mir_param_id, param_layout, .owned, region);
        try param_infos.append(self.allocator, lowered);
        const rewrite = try self.rewriteTopLevelRestBinding(lowered, param_layout, .owned, region);
        try param_rewrites.append(self.allocator, rewrite);
        try self.scratch_lir_pattern_ids.append(self.allocator, if (rewrite) |rw| rw.source_pattern else lowered.pattern);
    }
    const lir_params = try self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_param_patterns..]);
    if (builtin.mode == .Debug) {
        for (self.lir_store.getPatternSpan(lir_params)) |pat_id| {
            const pat_index = @intFromEnum(pat_id);
            if (pat_index >= self.lir_store.patterns.items.len) {
                std.debug.panic(
                    "MirToLir invariant violated: hosted params contain invalid pattern id {d} (patterns_len={d}, mir_params={d}, func_args={d})",
                    .{ pat_index, self.lir_store.patterns.items.len, mir_params.len, func_args.len },
                );
            }
        }
    }

    const save_len = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_len);

    for (mir_params, 0..) |mir_param_id, i| {
        const mir_pat = self.mir_store.getPattern(mir_param_id);
        const symbol = switch (mir_pat) {
            .bind => |sym| sym,
            .wildcard => continue, // unit () params are zero-sized, skip
            .tag, .int_literal, .str_literal, .dec_literal, .frac_f32_literal, .frac_f64_literal, .struct_destructure, .list_destructure, .as_pattern, .runtime_error => unreachable,
        };
        const param_layout = if (i < func_args.len)
            try self.layoutFromMonotype(func_args[i])
        else
            unreachable;

        const lookup = try self.lir_store.addExpr(.{ .lookup = .{
            .symbol = symbol,
            .layout_idx = param_layout,
        } }, region);
        try self.scratch_lir_expr_ids.append(self.allocator, lookup);
    }
    const lir_args = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_len..]);

    // Create hosted_call as the lambda body
    var hosted_call = try self.lir_store.addExpr(.{ .hosted_call = .{
        .index = h.index,
        .args = lir_args,
        .ret_layout = ret_layout,
    } }, region);
    var hosted_param_idx = param_infos.items.len;
    while (hosted_param_idx > 0) {
        hosted_param_idx -= 1;
        const info = param_infos.items[hosted_param_idx];
        hosted_call = try self.wrapExprWithTopLevelRestBindingPrelude(
            hosted_call,
            ret_layout,
            param_rewrites.items[hosted_param_idx],
            info.deferred_rest_start,
            info.deferred_rest_len,
            region,
        );
    }

    // Wrap in lambda (hosted lambdas are function values)
    return self.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = fn_layout,
        .params = lir_params,
        .body = hosted_call,
        .ret_layout = ret_layout,
    } }, region);
}

fn lowerDbg(self: *Self, d: anytype, _: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const result_layout = try self.runtimeValueLayoutFromMirExpr(d.expr);
    const lir_expr = try self.lowerExpr(d.expr);

    return self.lir_store.addExpr(.{ .dbg = .{
        .expr = lir_expr,
        .result_layout = result_layout,
    } }, region);
}

fn lowerExpect(self: *Self, e: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const result_layout = try self.layoutFromMonotype(mono_idx);
    const lir_cond = try self.lowerExpr(e.body);

    // The MIR expect body is the boolean condition to assert.
    // After the assertion, the result is empty_record (unit).
    const lir_body = try self.lir_store.addExpr(.{ .struct_ = .{ .struct_layout = .zst, .fields = LirExprSpan.empty() } }, region);

    return self.lir_store.addExpr(.{ .expect = .{
        .cond = lir_cond,
        .body = lir_body,
        .result_layout = result_layout,
    } }, region);
}

fn lowerForLoop(self: *Self, f: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    std.debug.assert(mono_idx == self.mir_store.monotype_store.unit_idx);
    var acc = self.startLetAccumulator();
    const list_layout = try self.runtimeValueLayoutFromMirExpr(f.list);
    const lir_list_raw = try self.lowerExpr(f.list);
    const lir_list = if (self.isBorrowAtomicExpr(lir_list_raw))
        lir_list_raw
    else blk: {
        break :blk try acc.bindBorrow(lir_list_raw, list_layout, region);
    };

    const elem_layout = try self.runtimeListElemLayoutFromMirExpr(f.list);
    const save_rest_bindings = self.scratch_deferred_list_rest_bindings.items.len;
    defer self.scratch_deferred_list_rest_bindings.shrinkRetainingCapacity(save_rest_bindings);
    try self.registerBindingPatternSymbols(f.elem_pattern, elem_layout);
    const lowered_pat = try self.lowerBindingPatternForRuntimeLayout(f.elem_pattern, elem_layout, .borrowed, region);
    const elem_rewrite = try self.rewriteTopLevelRestBinding(lowered_pat, elem_layout, .borrowed, region);
    const raw_body = try self.lowerExpr(f.body);
    const lir_body = try self.wrapExprWithTopLevelRestBindingPrelude(
        raw_body,
        .zst,
        elem_rewrite,
        lowered_pat.deferred_rest_start,
        lowered_pat.deferred_rest_len,
        region,
    );

    const for_expr = try self.lir_store.addExpr(.{ .for_loop = .{
        .list_expr = lir_list,
        .elem_layout = elem_layout,
        .elem_pattern = if (elem_rewrite) |rw| rw.source_pattern else lowered_pat.pattern,
        .body = lir_body,
    } }, region);
    return acc.finish(for_expr, .zst, region);
}

fn lowerWhileLoop(self: *Self, w: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    std.debug.assert(mono_idx == self.mir_store.monotype_store.unit_idx);
    const lir_cond = try self.lowerExpr(w.cond);
    const lir_body = try self.lowerExpr(w.body);

    return self.lir_store.addExpr(.{ .while_loop = .{
        .cond = lir_cond,
        .body = lir_body,
    } }, region);
}

fn lowerReturn(self: *Self, r: anytype, _: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const ret_layout = try self.runtimeValueLayoutFromMirExpr(r.expr);
    const lir_expr = try self.lowerExpr(r.expr);

    return self.lir_store.addExpr(.{ .early_return = .{
        .expr = lir_expr,
        .ret_layout = ret_layout,
    } }, region);
}

fn runtimeLayoutForBindingSymbol(
    self: *Self,
    sym: Symbol,
    mono_idx: Monotype.Idx,
    fallback_layout: layout.Idx,
) Allocator.Error!layout.Idx {
    const existing_layout = self.symbol_layouts.get(sym.raw());
    const mono = self.mir_store.monotype_store.getMonotype(mono_idx);

    if (mono != .func and !(try self.monotypeContainsFunctionValue(mono_idx))) {
        return self.layoutFromMonotype(mono_idx);
    }

    var layout_idx = existing_layout orelse fallback_layout;
    const has_callable_def = if (self.mir_store.getSymbolDef(sym)) |def_id|
        LambdaSet.isLambdaExpr(self.mir_store, def_id)
    else
        false;

    if (mono == .func and !has_callable_def and existing_layout == null) {
        const generic_fn_layout = try self.monotype_layout_resolver.resolve(mono_idx, null);
        if (fallback_layout == generic_fn_layout) {
            if (self.lambda_set_store.getSymbolLambdaSet(sym)) |ls_idx| {
                layout_idx = try self.closureValueLayoutFromLambdaSet(ls_idx);
            }
        }
    }

    return layout_idx;
}

fn monotypeContainsFunctionValue(self: *Self, mono_idx: Monotype.Idx) Allocator.Error!bool {
    var visited = std.AutoHashMap(u32, void).init(self.allocator);
    defer visited.deinit();
    return self.monotypeContainsFunctionValueInner(mono_idx, &visited);
}

fn monotypeContainsFunctionValueInner(
    self: *Self,
    mono_idx: Monotype.Idx,
    visited: *std.AutoHashMap(u32, void),
) Allocator.Error!bool {
    const mono_key = @intFromEnum(mono_idx);
    if (visited.contains(mono_key)) return false;
    try visited.put(mono_key, {});

    return switch (self.mir_store.monotype_store.getMonotype(mono_idx)) {
        .func => true,
        .record => |record| blk: {
            const fields = self.mir_store.monotype_store.getFields(record.fields);
            for (fields) |field| {
                if (try self.monotypeContainsFunctionValueInner(field.type_idx, visited)) break :blk true;
            }
            break :blk false;
        },
        .tuple => |tuple| blk: {
            const elems = self.mir_store.monotype_store.getIdxSpan(tuple.elems);
            for (elems) |elem_mono| {
                if (try self.monotypeContainsFunctionValueInner(elem_mono, visited)) break :blk true;
            }
            break :blk false;
        },
        .tag_union => |tu| blk: {
            const tags = self.mir_store.monotype_store.getTags(tu.tags);
            for (tags) |tag| {
                const payloads = self.mir_store.monotype_store.getIdxSpan(tag.payloads);
                for (payloads) |payload_mono| {
                    if (try self.monotypeContainsFunctionValueInner(payload_mono, visited)) break :blk true;
                }
            }
            break :blk false;
        },
        .list => |list| self.monotypeContainsFunctionValueInner(list.elem, visited),
        .box => |box| self.monotypeContainsFunctionValueInner(box.inner, visited),
        .prim, .unit, .recursive_placeholder => false,
    };
}

fn registerPatternSymbolLayout(self: *Self, sym: Symbol, mono_idx: Monotype.Idx, runtime_layout: layout.Idx) Allocator.Error!void {
    const sym_key: u64 = @bitCast(sym);
    const resolved_layout = try self.runtimeLayoutForBindingSymbol(sym, mono_idx, runtime_layout);
    try self.symbol_layouts.put(sym_key, resolved_layout);
}

fn runtimeTagPayloadArgLayout(
    self: *Self,
    mono_idx: Monotype.Idx,
    tag_name: Ident.Idx,
    union_runtime_layout: layout.Idx,
    arg_count: usize,
    arg_index: usize,
) Allocator.Error!layout.Idx {
    std.debug.assert(arg_count > 0);

    const payload_layout = try self.runtimeTagPayloadLayout(mono_idx, tag_name, union_runtime_layout, arg_count);
    const payload_layout_val = self.layout_store.getLayout(payload_layout);
    if (payload_layout_val.tag == .struct_) {
        return self.layout_store.getStructFieldLayoutByOriginalIndex(payload_layout_val.data.struct_.idx, @intCast(arg_index));
    }

    if (builtin.mode == .Debug and arg_count != 1) {
        std.debug.panic(
            "MirToLir invariant violated: non-struct tag payload runtime layout can only bind a single arg, got {s} with {d} args",
            .{ @tagName(payload_layout_val.tag), arg_count },
        );
    }
    return payload_layout;
}

fn runtimeTagPayloadLayout(
    self: *Self,
    mono_idx: Monotype.Idx,
    tag_name: Ident.Idx,
    union_runtime_layout: layout.Idx,
    arg_count: usize,
) Allocator.Error!layout.Idx {
    std.debug.assert(arg_count > 0);

    const union_layout = self.layout_store.getLayout(union_runtime_layout);
    const discriminant = self.tagDiscriminant(tag_name, mono_idx);
    return switch (union_layout.tag) {
        .tag_union => blk: {
            const tu_data = self.layout_store.getTagUnionData(union_layout.data.tag_union.idx);
            const variants = self.layout_store.getTagUnionVariants(tu_data);
            if (builtin.mode == .Debug and discriminant >= variants.len) {
                std.debug.panic(
                    "MirToLir invariant violated: tag discriminant {d} out of bounds for runtime tag_union layout",
                    .{discriminant},
                );
            }
            break :blk variants.get(discriminant).payload_layout;
        },
        .box => blk: {
            const inner_layout = self.layout_store.getLayout(union_layout.data.box);
            if (builtin.mode == .Debug and inner_layout.tag != .tag_union) {
                std.debug.panic(
                    "MirToLir invariant violated: boxed tag-pattern runtime layout must wrap tag_union, got {s}",
                    .{@tagName(inner_layout.tag)},
                );
            }
            const tu_data = self.layout_store.getTagUnionData(inner_layout.data.tag_union.idx);
            const variants = self.layout_store.getTagUnionVariants(tu_data);
            if (builtin.mode == .Debug and discriminant >= variants.len) {
                std.debug.panic(
                    "MirToLir invariant violated: tag discriminant {d} out of bounds for boxed runtime tag_union layout",
                    .{discriminant},
                );
            }
            break :blk variants.get(discriminant).payload_layout;
        },
        .scalar, .zst => blk: {
            if (builtin.mode == .Debug and arg_count != 1) {
                std.debug.panic(
                    "MirToLir invariant violated: scalar/zst tag-pattern runtime layout can only have a single ZST payload arg, found {d}",
                    .{arg_count},
                );
            }
            break :blk .zst;
        },
        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "MirToLir invariant violated: tag-pattern runtime layout must be tag_union/box/scalar/zst, got {s}",
                    .{@tagName(union_layout.tag)},
                );
            }
            unreachable;
        },
    };
}

fn registerBindingPatternSymbols(
    self: *Self,
    mir_pat_id: MIR.PatternId,
    runtime_layout: layout.Idx,
) Allocator.Error!void {
    const pat = self.mir_store.getPattern(mir_pat_id);
    const mono_idx = self.mir_store.patternTypeOf(mir_pat_id);

    switch (pat) {
        .bind => |sym| try self.registerPatternSymbolLayout(sym, mono_idx, runtime_layout),
        .wildcard,
        .int_literal,
        .str_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .runtime_error,
        => {},
        .as_pattern => |as_pat| {
            try self.registerPatternSymbolLayout(as_pat.symbol, mono_idx, runtime_layout);
            try self.registerBindingPatternSymbols(as_pat.pattern, runtime_layout);
        },
        .tag => |tag_pat| {
            const arg_patterns = self.tagPayloadPatterns(mono_idx, tag_pat.name, tag_pat.args);
            for (arg_patterns, 0..) |arg_pattern_id, arg_index| {
                const arg_runtime = try self.runtimeTagPayloadArgLayout(
                    mono_idx,
                    tag_pat.name,
                    runtime_layout,
                    arg_patterns.len,
                    arg_index,
                );
                try self.registerBindingPatternSymbols(arg_pattern_id, arg_runtime);
            }
        },
        .struct_destructure => |sd| {
            const mir_patterns = self.mir_store.getPatternSpan(sd.fields);
            if (mir_patterns.len == 0) return;
            switch (self.mir_store.monotype_store.getMonotype(mono_idx)) {
                .record => |record_mono| {
                    const all_fields = self.mir_store.monotype_store.getFields(record_mono.fields);
                    const record_layout_val = self.layout_store.getLayout(runtime_layout);
                    if (builtin.mode == .Debug and all_fields.len != 0 and record_layout_val.tag != .struct_) {
                        std.debug.panic(
                            "MirToLir invariant violated: non-empty record binding pattern expects struct_ runtime layout, got {s}",
                            .{@tagName(record_layout_val.tag)},
                        );
                    }
                    if (record_layout_val.tag == .struct_) {
                        const record_data = self.layout_store.getStructData(record_layout_val.data.struct_.idx);
                        const layout_fields = self.layout_store.struct_fields.sliceRange(record_data.getFields());

                        for (0..layout_fields.len) |li| {
                            const semantic_index = layout_fields.get(li).index;
                            try self.registerBindingPatternSymbols(mir_patterns[semantic_index], layout_fields.get(li).layout);
                        }
                    }
                },
                .tuple => {
                    const tuple_layout_val = self.layout_store.getLayout(runtime_layout);
                    if (builtin.mode == .Debug and mir_patterns.len != 0 and tuple_layout_val.tag != .struct_) {
                        std.debug.panic(
                            "MirToLir invariant violated: non-empty tuple binding pattern expects struct_ runtime layout, got {s}",
                            .{@tagName(tuple_layout_val.tag)},
                        );
                    }
                    if (tuple_layout_val.tag == .struct_) {
                        const struct_data = self.layout_store.getStructData(tuple_layout_val.data.struct_.idx);
                        const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
                        for (0..layout_fields.len) |li| {
                            const original_index = layout_fields.get(li).index;
                            try self.registerBindingPatternSymbols(mir_patterns[original_index], layout_fields.get(li).layout);
                        }
                    }
                },
                else => unreachable,
            }
        },
        .list_destructure => |ld| {
            const list_layout = try self.layoutFromMonotype(mono_idx);
            const elem_mono = switch (self.mir_store.monotype_store.getMonotype(mono_idx)) {
                .list => |list_mono| list_mono.elem,
                else => unreachable,
            };
            const elem_layout = try self.layoutFromMonotype(elem_mono);
            const all_patterns = self.mir_store.getPatternSpan(ld.patterns);

            if (ld.rest_index.isNone()) {
                for (all_patterns) |elem_pattern_id| {
                    try self.registerBindingPatternSymbols(elem_pattern_id, elem_layout);
                }
            } else {
                const rest_idx: u32 = @intFromEnum(ld.rest_index);
                for (all_patterns[0..rest_idx]) |elem_pattern_id| {
                    try self.registerBindingPatternSymbols(elem_pattern_id, elem_layout);
                }
                for (all_patterns[rest_idx..]) |elem_pattern_id| {
                    try self.registerBindingPatternSymbols(elem_pattern_id, elem_layout);
                }
                if (!ld.rest_pattern.isNone()) {
                    try self.registerBindingPatternSymbols(ld.rest_pattern, list_layout);
                }
            }
        },
    }
}

fn lowerBindingPatternForRuntimeLayout(
    self: *Self,
    mir_pat_id: MIR.PatternId,
    runtime_layout: layout.Idx,
    ownership_mode: BindingOwnershipMode,
    region: Region,
) Allocator.Error!LoweredBindingPattern {
    const save_rest_len = self.scratch_deferred_list_rest_bindings.items.len;
    const pattern = try self.lowerPatternInternal(mir_pat_id, runtime_layout, true, ownership_mode, region);
    return .{
        .pattern = pattern,
        .deferred_rest_start = save_rest_len,
        .deferred_rest_len = self.scratch_deferred_list_rest_bindings.items.len - save_rest_len,
    };
}

fn lowerWildcardBindingPattern(
    self: *Self,
    runtime_layout: layout.Idx,
    ownership_mode: BindingOwnershipMode,
    region: Region,
) Allocator.Error!LirPatternId {
    if (ownership_mode == .borrowed or !self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(runtime_layout))) {
        return self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = runtime_layout } }, region);
    }

    const symbol = self.freshSymbol();
    try self.symbol_layouts.put(symbol.raw(), runtime_layout);
    try self.symbol_binding_modes.put(symbol.raw(), .owned);
    return self.lir_store.addPattern(.{ .bind = .{
        .symbol = symbol,
        .layout_idx = runtime_layout,
        .reassignable = false,
    } }, region);
}

fn lowerPatternInternal(
    self: *Self,
    mir_pat_id: MIR.PatternId,
    runtime_layout: layout.Idx,
    collect_rest_bindings: bool,
    ownership_mode: BindingOwnershipMode,
    region: Region,
) Allocator.Error!LirPatternId {
    const pat = self.mir_store.getPattern(mir_pat_id);
    const mono_idx = self.mir_store.patternTypeOf(mir_pat_id);

    return switch (pat) {
        .bind => |sym| blk: {
            const layout_idx = try self.runtimeLayoutForBindingSymbol(sym, mono_idx, runtime_layout);
            const reassignable = self.mir_store.isSymbolReassignable(sym);
            const sym_key: u64 = @bitCast(sym);
            try self.symbol_layouts.put(sym_key, layout_idx);
            try self.symbol_binding_modes.put(sym_key, ownership_mode);
            break :blk self.lir_store.addPattern(.{ .bind = .{
                .symbol = sym,
                .layout_idx = layout_idx,
                .reassignable = reassignable,
            } }, region);
        },
        .wildcard => self.lowerWildcardBindingPattern(runtime_layout, ownership_mode, region),
        .tag => |t| blk: {
            const mir_pat_args = self.tagPayloadPatterns(mono_idx, t.name, t.args);

            const union_layout = runtime_layout;
            const discriminant = self.tagDiscriminant(t.name, mono_idx);

            const save_len = self.scratch_lir_pattern_ids.items.len;
            defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);

            for (mir_pat_args, 0..) |mir_arg_pat, arg_index| {
                const arg_layout = try self.runtimeTagPayloadArgLayout(
                    mono_idx,
                    t.name,
                    runtime_layout,
                    mir_pat_args.len,
                    arg_index,
                );
                const lir_pat = try self.lowerPatternInternal(mir_arg_pat, arg_layout, collect_rest_bindings, ownership_mode, region);
                try self.scratch_lir_pattern_ids.append(self.allocator, lir_pat);
            }

            const lir_args = try self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_len..]);
            break :blk self.lir_store.addPattern(.{ .tag = .{
                .discriminant = discriminant,
                .union_layout = union_layout,
                .args = lir_args,
            } }, region);
        },
        .int_literal => |i| self.lir_store.addPattern(.{ .int_literal = .{
            .value = i.value.toI128(),
            .layout_idx = runtime_layout,
        } }, region),
        .str_literal => |s| blk: {
            const lir_str_idx = try self.copyStringToLir(s);
            break :blk self.lir_store.addPattern(.{ .str_literal = lir_str_idx }, region);
        },
        .dec_literal => |d| self.lir_store.addPattern(.{ .int_literal = .{
            .value = d.num,
            .layout_idx = runtime_layout,
        } }, region),
        .frac_f32_literal => |v| self.lir_store.addPattern(.{ .float_literal = .{
            .value = @floatCast(v),
            .layout_idx = runtime_layout,
        } }, region),
        .frac_f64_literal => |v| self.lir_store.addPattern(.{ .float_literal = .{
            .value = v,
            .layout_idx = runtime_layout,
        } }, region),
        .struct_destructure => |sd| blk: {
            const struct_layout = runtime_layout;
            const mir_patterns = self.mir_store.getPatternSpan(sd.fields);
            if (mir_patterns.len == 0) {
                break :blk self.lowerWildcardBindingPattern(struct_layout, ownership_mode, region);
            }

            switch (self.mir_store.monotype_store.getMonotype(mono_idx)) {
                .record => |record_mono| {
                    const all_fields = self.mir_store.monotype_store.getFields(record_mono.fields);

                    if (all_fields.len == 0) {
                        break :blk self.lowerWildcardBindingPattern(struct_layout, ownership_mode, region);
                    }

                    const record_layout_val = self.layout_store.getLayout(struct_layout);
                    if (builtin.mode == .Debug and record_layout_val.tag != .struct_) {
                        std.debug.panic(
                            "MirToLir invariant violated: non-empty record destructure expects struct_ runtime layout, got {s}",
                            .{@tagName(record_layout_val.tag)},
                        );
                    }

                    if (record_layout_val.tag == .struct_) {
                        const record_data = self.layout_store.getStructData(record_layout_val.data.struct_.idx);
                        const layout_fields = self.layout_store.struct_fields.sliceRange(record_data.getFields());
                        const save_len = self.scratch_lir_pattern_ids.items.len;
                        defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);

                        for (0..layout_fields.len) |li| {
                            const semantic_index = layout_fields.get(li).index;
                            const lir_pat = try self.lowerPatternInternal(
                                mir_patterns[semantic_index],
                                layout_fields.get(li).layout,
                                collect_rest_bindings,
                                ownership_mode,
                                region,
                            );
                            try self.scratch_lir_pattern_ids.append(self.allocator, lir_pat);
                        }

                        const lir_fields = try self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_len..]);
                        break :blk self.lir_store.addPattern(.{ .struct_ = .{
                            .struct_layout = struct_layout,
                            .fields = lir_fields,
                        } }, region);
                    }

                    break :blk self.lowerWildcardBindingPattern(struct_layout, ownership_mode, region);
                },
                .tuple => {
                    const struct_layout_val = self.layout_store.getLayout(struct_layout);
                    if (builtin.mode == .Debug and mir_patterns.len != 0 and struct_layout_val.tag != .struct_) {
                        std.debug.panic(
                            "MirToLir invariant violated: non-empty tuple destructure expects struct_ runtime layout, got {s}",
                            .{@tagName(struct_layout_val.tag)},
                        );
                    }

                    if (struct_layout_val.tag == .struct_) {
                        const struct_data = self.layout_store.getStructData(struct_layout_val.data.struct_.idx);
                        const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());

                        const save_len = self.scratch_lir_pattern_ids.items.len;
                        defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);

                        for (0..layout_fields.len) |li| {
                            const original_index = layout_fields.get(li).index;
                            const lir_pat = try self.lowerPatternInternal(
                                mir_patterns[original_index],
                                layout_fields.get(li).layout,
                                collect_rest_bindings,
                                ownership_mode,
                                region,
                            );
                            try self.scratch_lir_pattern_ids.append(self.allocator, lir_pat);
                        }

                        const lir_fields = try self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_len..]);
                        break :blk self.lir_store.addPattern(.{ .struct_ = .{
                            .struct_layout = struct_layout,
                            .fields = lir_fields,
                        } }, region);
                    }

                    break :blk self.lowerWildcardBindingPattern(struct_layout, ownership_mode, region);
                },
                else => unreachable,
            }
        },
        .list_destructure => |ld| blk: {
            const list_layout = try self.layoutFromMonotype(mono_idx);
            const list_monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
            const elem_layout = switch (list_monotype) {
                .list => |l| try self.layoutFromMonotype(l.elem),
                .prim, .unit, .record, .tuple, .tag_union, .box, .func, .recursive_placeholder => unreachable,
            };
            const all_patterns = self.mir_store.getPatternSpan(ld.patterns);
            const has_rest = !ld.rest_index.isNone();
            const needs_owned_rest_discard =
                has_rest and
                ld.rest_pattern.isNone() and
                collect_rest_bindings and
                ownership_mode == .owned and
                self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(list_layout));
            var deferred_rest_source_symbol = Symbol.none;

            const rest_pat: LirPatternId = if (!has_rest) rest_blk: {
                break :rest_blk LirPatternId.none;
            } else if (ld.rest_pattern.isNone()) rest_blk: {
                if (!needs_owned_rest_discard) {
                    break :rest_blk try self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = list_layout } }, region);
                }

                deferred_rest_source_symbol = self.freshSymbol();
                const discard_symbol = self.freshSymbol();
                try self.scratch_deferred_list_rest_bindings.append(self.allocator, .{
                    .source_symbol = deferred_rest_source_symbol,
                    .list_layout = list_layout,
                    .target_pattern = MIR.PatternId.none,
                    .discard_symbol = discard_symbol,
                    .prefix_count = if (ld.rest_index.isNone()) 0 else @intFromEnum(ld.rest_index),
                    .suffix_count = if (ld.rest_index.isNone()) 0 else @as(u32, @intCast(all_patterns.len - @as(usize, @intFromEnum(ld.rest_index)))),
                });
                break :rest_blk try self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = list_layout } }, region);
            } else if (!collect_rest_bindings) {
                if (builtin.mode == .Debug) {
                    std.debug.panic("MirToLir invariant violated: bound list rest pattern must be lowered as an explicit binding", .{});
                }
                unreachable;
            } else rest_blk: {
                const source_symbol = self.freshSymbol();
                deferred_rest_source_symbol = source_symbol;
                try self.scratch_deferred_list_rest_bindings.append(self.allocator, .{
                    .source_symbol = source_symbol,
                    .list_layout = list_layout,
                    .target_pattern = ld.rest_pattern,
                    .prefix_count = if (ld.rest_index.isNone()) 0 else @intFromEnum(ld.rest_index),
                    .suffix_count = if (ld.rest_index.isNone()) 0 else @as(u32, @intCast(all_patterns.len - @as(usize, @intFromEnum(ld.rest_index)))),
                });
                break :rest_blk try self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = list_layout } }, region);
            };

            const list_pattern = if (ld.rest_index.isNone()) blk2: {
                const lir_prefix = try self.lowerPatternSpanWithLayoutInternal(all_patterns, elem_layout, collect_rest_bindings, .borrowed, region);
                break :blk2 try self.lir_store.addPattern(.{ .list = .{
                    .list_layout = list_layout,
                    .elem_layout = elem_layout,
                    .prefix = lir_prefix,
                    .rest = rest_pat,
                    .suffix = LirPatternSpan.empty(),
                } }, region);
            } else blk2: {
                const rest_idx: u32 = @intFromEnum(ld.rest_index);
                const lir_prefix = try self.lowerPatternSpanWithLayoutInternal(all_patterns[0..rest_idx], elem_layout, collect_rest_bindings, .borrowed, region);
                const lir_suffix = try self.lowerPatternSpanWithLayoutInternal(all_patterns[rest_idx..], elem_layout, collect_rest_bindings, .borrowed, region);
                break :blk2 try self.lir_store.addPattern(.{ .list = .{
                    .list_layout = list_layout,
                    .elem_layout = elem_layout,
                    .prefix = lir_prefix,
                    .rest = rest_pat,
                    .suffix = lir_suffix,
                } }, region);
            };

            if (ld.rest_pattern.isNone() and !needs_owned_rest_discard) break :blk list_pattern;

            const synthetic_sym_key: u64 = @bitCast(deferred_rest_source_symbol);
            try self.symbol_layouts.put(synthetic_sym_key, list_layout);
            break :blk try self.lir_store.addPattern(.{ .as_pattern = .{
                .symbol = deferred_rest_source_symbol,
                .layout_idx = list_layout,
                .reassignable = false,
                .inner = list_pattern,
            } }, region);
        },
        .as_pattern => |ap| blk: {
            const layout_idx = try self.runtimeLayoutForBindingSymbol(ap.symbol, mono_idx, runtime_layout);
            const inner = try self.lowerPatternInternal(ap.pattern, runtime_layout, collect_rest_bindings, .borrowed, region);
            const reassignable = self.mir_store.isSymbolReassignable(ap.symbol);
            const sym_key: u64 = @bitCast(ap.symbol);
            try self.symbol_layouts.put(sym_key, layout_idx);
            try self.symbol_binding_modes.put(sym_key, ownership_mode);
            break :blk self.lir_store.addPattern(.{ .as_pattern = .{
                .symbol = ap.symbol,
                .layout_idx = layout_idx,
                .reassignable = reassignable,
                .inner = inner,
            } }, region);
        },
        .runtime_error => self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, region),
    };
}

fn lowerPatternSpanWithLayoutInternal(
    self: *Self,
    mir_pat_ids: []const MIR.PatternId,
    runtime_layout: layout.Idx,
    collect_rest_bindings: bool,
    ownership_mode: BindingOwnershipMode,
    region: Region,
) Allocator.Error!LirPatternSpan {
    const save_len = self.scratch_lir_pattern_ids.items.len;
    defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);
    for (mir_pat_ids) |mir_id| {
        const lir_id = try self.lowerPatternInternal(mir_id, runtime_layout, collect_rest_bindings, ownership_mode, region);
        try self.scratch_lir_pattern_ids.append(self.allocator, lir_id);
    }
    return self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_len..]);
}

fn lowerPattern(self: *Self, mir_pat_id: MIR.PatternId) Allocator.Error!LirPatternId {
    return self.lowerPatternInternal(
        mir_pat_id,
        try self.layoutFromMonotype(self.mir_store.patternTypeOf(mir_pat_id)),
        false,
        .borrowed,
        Region.zero(),
    );
}

fn buildListRestBindingExpr(
    self: *Self,
    binding: DeferredListRestBinding,
    region: Region,
) Allocator.Error!LirExprId {
    const source_lookup = try self.lir_store.addExpr(.{ .lookup = .{
        .symbol = binding.source_symbol,
        .layout_idx = binding.list_layout,
    } }, region);

    const count_layout: layout.Idx = .u64;

    const emitCount = struct {
        fn literal(self_: *Self, value: u32, region_: Region) Allocator.Error!LirExprId {
            return self_.lir_store.addExpr(.{ .i64_literal = .{
                .value = @intCast(value),
                .layout_idx = count_layout,
            } }, region_);
        }
    }.literal;

    const drop_first_expr = if (binding.prefix_count == 0)
        source_lookup
    else blk: {
        const count_expr = try emitCount(self, binding.prefix_count, region);
        const args = try self.lir_store.addExprSpan(&.{ source_lookup, count_expr });
        break :blk try self.lir_store.addExpr(.{ .low_level = .{
            .op = .list_drop_first,
            .args = args,
            .ret_layout = binding.list_layout,
        } }, region);
    };

    if (binding.suffix_count == 0) {
        return drop_first_expr;
    }

    const suffix_expr = try emitCount(self, binding.suffix_count, region);
    const args = try self.lir_store.addExprSpan(&.{ drop_first_expr, suffix_expr });
    return self.lir_store.addExpr(.{ .low_level = .{
        .op = .list_drop_last,
        .args = args,
        .ret_layout = binding.list_layout,
    } }, region);
}

fn appendDeferredListRestBindingDecls(
    self: *Self,
    deferred_start: usize,
    deferred_len: usize,
    region: Region,
) Allocator.Error!void {
    var i: usize = 0;
    while (i < deferred_len) : (i += 1) {
        const binding = self.scratch_deferred_list_rest_bindings.items[deferred_start + i];
        const binding_expr = try self.buildListRestBindingExpr(binding, region);
        if (binding.target_pattern.isNone()) {
            std.debug.assert(!binding.discard_symbol.isNone());
            const discard_pattern = try self.lir_store.addPattern(.{ .bind = .{
                .symbol = binding.discard_symbol,
                .layout_idx = binding.list_layout,
                .reassignable = false,
            } }, region);
            try self.scratch_lir_stmts.append(self.allocator, .{ .decl = .{
                .pattern = discard_pattern,
                .expr = binding_expr,
            } });
        } else {
            try self.registerBindingPatternSymbols(binding.target_pattern, binding.list_layout);
            const lowered = try self.lowerBindingPatternForRuntimeLayout(binding.target_pattern, binding.list_layout, .owned, region);
            try self.scratch_lir_stmts.append(self.allocator, .{ .decl = .{
                .pattern = lowered.pattern,
                .expr = binding_expr,
            } });
            try self.appendDeferredListRestBindingDecls(lowered.deferred_rest_start, lowered.deferred_rest_len, region);
        }
    }
}

fn rewriteTopLevelRestBinding(
    self: *Self,
    lowered: LoweredBindingPattern,
    runtime_layout: layout.Idx,
    ownership_mode: BindingOwnershipMode,
    region: Region,
) Allocator.Error!?TopLevelRestBindingRewrite {
    const pat = self.lir_store.getPattern(lowered.pattern);
    if (pat == .as_pattern) {
        if (lowered.deferred_rest_len == 0 and ownership_mode != .owned) return null;

        const source_pattern = try self.lir_store.addPattern(.{ .bind = .{
            .symbol = pat.as_pattern.symbol,
            .layout_idx = pat.as_pattern.layout_idx,
            .reassignable = pat.as_pattern.reassignable,
        } }, region);

        return .{
            .source_pattern = source_pattern,
            .destructure_pattern = pat.as_pattern.inner,
            .source_symbol = pat.as_pattern.symbol,
            .source_layout = pat.as_pattern.layout_idx,
        };
    }

    if (ownership_mode != .owned) return null;

    switch (pat) {
        .list, .struct_, .tag => {},
        else => return null,
    }

    const source = try self.freshBindPattern(runtime_layout, false, region);
    try self.symbol_layouts.put(source.symbol.raw(), runtime_layout);
    try self.symbol_binding_modes.put(source.symbol.raw(), ownership_mode);
    return .{
        .source_pattern = source.pattern,
        .destructure_pattern = lowered.pattern,
        .source_symbol = source.symbol,
        .source_layout = runtime_layout,
    };
}

fn rewriteBorrowedBindingSource(
    self: *Self,
    lowered: LoweredBindingPattern,
    runtime_layout: layout.Idx,
    region: Region,
) Allocator.Error!?TopLevelRestBindingRewrite {
    const pat = self.lir_store.getPattern(lowered.pattern);
    switch (pat) {
        .list, .struct_, .tag, .wildcard => {},
        else => return null,
    }

    const source = try self.freshBindPattern(runtime_layout, false, region);
    try self.symbol_layouts.put(source.symbol.raw(), runtime_layout);
    try self.symbol_binding_modes.put(source.symbol.raw(), .borrowed);
    return .{
        .source_pattern = source.pattern,
        .destructure_pattern = if (pat == .wildcard) LirPatternId.none else lowered.pattern,
        .source_symbol = source.symbol,
        .source_layout = runtime_layout,
    };
}

fn wrapExprWithTopLevelRestBindingPrelude(
    self: *Self,
    body: LirExprId,
    body_layout: layout.Idx,
    rewrite: ?TopLevelRestBindingRewrite,
    deferred_rest_start: usize,
    deferred_rest_len: usize,
    region: Region,
) Allocator.Error!LirExprId {
    if (rewrite == null and deferred_rest_len == 0) return body;

    const save_len = self.scratch_lir_stmts.items.len;
    defer self.scratch_lir_stmts.shrinkRetainingCapacity(save_len);

    if (rewrite) |binding_rewrite| {
        const source_lookup = try self.lir_store.addExpr(.{ .lookup = .{
            .symbol = binding_rewrite.source_symbol,
            .layout_idx = binding_rewrite.source_layout,
        } }, region);
        if (!binding_rewrite.destructure_pattern.isNone()) {
            try self.scratch_lir_stmts.append(self.allocator, .{ .decl = .{
                .pattern = binding_rewrite.destructure_pattern,
                .expr = source_lookup,
                .semantics = .borrow_alias,
            } });
        }
    }

    try self.appendDeferredListRestBindingDecls(deferred_rest_start, deferred_rest_len, region);
    const stmts = try self.lir_store.addStmts(self.scratch_lir_stmts.items[save_len..]);
    return self.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = body,
        .result_layout = body_layout,
    } }, region);
}

fn wrapExprWithDeferredListRestBindings(
    self: *Self,
    body: LirExprId,
    body_layout: layout.Idx,
    deferred_rest_start: usize,
    deferred_rest_len: usize,
    region: Region,
) Allocator.Error!LirExprId {
    if (deferred_rest_len == 0) return body;

    const save_len = self.scratch_lir_stmts.items.len;
    defer self.scratch_lir_stmts.shrinkRetainingCapacity(save_len);

    try self.appendDeferredListRestBindingDecls(deferred_rest_start, deferred_rest_len, region);
    const stmts = try self.lir_store.addStmts(self.scratch_lir_stmts.items[save_len..]);
    return self.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = body,
        .result_layout = body_layout,
    } }, region);
}

/// Create a fresh synthetic symbol for generated code (ANF bindings).
/// Uses a reserved high 32-bit namespace to avoid colliding with lowered symbols.
fn freshSymbol(self: *Self) Symbol {
    const id = self.next_synthetic_id;
    self.next_synthetic_id += 1;
    const raw = (@as(u64, std.math.maxInt(u32)) << 32) | @as(u64, id);
    return Symbol.fromRaw(raw);
}

/// Create a bind pattern for a fresh symbol.
fn freshBindPattern(self: *Self, layout_idx: layout.Idx, reassignable: bool, region: Region) Allocator.Error!struct { symbol: Symbol, pattern: LirPatternId } {
    const sym = self.freshSymbol();
    const pat = try self.lir_store.addPattern(.{ .bind = .{
        .symbol = sym,
        .layout_idx = layout_idx,
        .reassignable = reassignable,
    } }, region);
    return .{ .symbol = sym, .pattern = pat };
}

// --- Tests ---

const testing = std.testing;

fn testInit() !struct { mir_store: MIR.Store, lir_store: LirExprStore, layout_store: layout.Store, lambda_set_store: LambdaSet.Store, module_env: @import("can").ModuleEnv, module_env_ptrs: [1]*const @import("can").ModuleEnv } {
    const allocator = testing.allocator;
    var result: @TypeOf(testInit() catch unreachable) = undefined;
    result.module_env = try @import("can").ModuleEnv.init(allocator, "");
    result.mir_store = try MIR.Store.init(allocator);
    result.lir_store = LirExprStore.init(allocator);
    result.lambda_set_store = LambdaSet.Store.init();
    // Must set module_env_ptrs AFTER struct is in final location
    return result;
}

fn testInitLayoutStore(self: *@TypeOf(testInit() catch unreachable)) !void {
    self.module_env_ptrs[0] = &self.module_env;
    self.layout_store = try layout.Store.init(&self.module_env_ptrs, null, testing.allocator, @import("base").target.TargetUsize.native);
}

fn testDeinit(self: *@TypeOf(testInit() catch unreachable)) void {
    self.layout_store.deinit();
    self.lambda_set_store.deinit(testing.allocator);
    self.lir_store.deinit();
    self.mir_store.deinit(testing.allocator);
    self.module_env.deinit();
}

fn testSymbolFromIdent(ident: Ident.Idx) Symbol {
    return Symbol.fromRaw(@as(u64, @as(u32, @bitCast(ident))));
}

fn testMirSymbol(mir_store: *MIR.Store, allocator: Allocator, ident: Ident.Idx) !Symbol {
    const sym = testSymbolFromIdent(ident);
    try mir_store.registerSymbolReassignable(allocator, sym, ident.attributes.reassignable);
    return sym;
}

test "ANF: list of calls Let-binds each call to a symbol" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    // Build MIR: a list containing two calls, each with one argument.
    // With ANF, each call should be Let-bound to a fresh symbol,
    // and the list elements should be lookups to those symbols.

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const list_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .list = .{ .elem = i64_mono } });

    // func_args_mono: (I64) -> I64
    const func_arg_span = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{i64_mono});
    const func_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .func = .{
        .args = func_arg_span,
        .ret = i64_mono,
        .effectful = false,
    } });

    const ident_f = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_f = try testMirSymbol(&env.mir_store, allocator, ident_f);

    // func_lookup: lookup of `f`
    const func_lookup = try env.mir_store.addExpr(allocator, .{ .lookup = sym_f }, func_mono, Region.zero());

    // arg0 and arg1: integer literals
    const arg0 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 10)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const arg1 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 20)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    // call0: f(10)
    const call0_args = try env.mir_store.addExprSpan(allocator, &.{arg0});
    const call0 = try env.mir_store.addExpr(allocator, .{ .call = .{ .func = func_lookup, .args = call0_args } }, i64_mono, Region.zero());

    // call1: f(20)
    const call1_args = try env.mir_store.addExprSpan(allocator, &.{arg1});
    const call1 = try env.mir_store.addExpr(allocator, .{ .call = .{ .func = func_lookup, .args = call1_args } }, i64_mono, Region.zero());

    // list: [f(10), f(20)]
    const list_elems = try env.mir_store.addExprSpan(allocator, &.{ call0, call1 });
    const list_expr = try env.mir_store.addExpr(allocator, .{ .list = .{ .elems = list_elems } }, list_mono, Region.zero());

    // Lower MIR -> LIR
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(list_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // ANF wraps compound sub-expressions in a block with Let-bindings
    try testing.expect(lir_expr == .block);
    const inner = env.lir_store.getExpr(lir_expr.block.final_expr);
    try testing.expect(inner == .list);
    const elems = env.lir_store.getExprSpan(inner.list.elems);
    try testing.expectEqual(@as(usize, 2), elems.len);

    // Both elements should now be lookups (the calls are Let-bound)
    const elem0 = env.lir_store.getExpr(elems[0]);
    const elem1 = env.lir_store.getExpr(elems[1]);
    try testing.expect(elem0 == .lookup);
    try testing.expect(elem1 == .lookup);
}

test "MIR int literal lowers to LIR i64_literal" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    const int_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(int_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);
    try testing.expect(lir_expr == .i64_literal);
    try testing.expectEqual(@as(i64, 42), lir_expr.i64_literal.value);
    try testing.expectEqual(layout.Idx.i64, lir_expr.i64_literal.layout_idx);
}

test "MIR zero-arg tag lowers to LIR zero_arg_tag" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    // Create a single-tag union monotype: [MyTag]
    const tag_name = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const tag_span = try env.mir_store.monotype_store.addTags(allocator, &.{
        .{ .name = tag_name, .payloads = Monotype.Span.empty() },
    });
    const union_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .tag_union = .{ .tags = tag_span } });

    const tag_expr = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = tag_name,
        .args = MIR.ExprSpan.empty(),
    } }, union_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(tag_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);
    try testing.expect(lir_expr == .zero_arg_tag);
    try testing.expectEqual(@as(u16, 0), lir_expr.zero_arg_tag.discriminant);
}

test "MIR empty list lowers to LIR empty_list" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const list_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .list = .{ .elem = i64_mono } });

    const list_expr = try env.mir_store.addExpr(allocator, .{ .list = .{
        .elems = MIR.ExprSpan.empty(),
    } }, list_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(list_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);
    try testing.expect(lir_expr == .empty_list);
}

test "MIR lookup lowers to LIR lookup" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    const ident_x = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_x = try testMirSymbol(&env.mir_store, allocator, ident_x);

    const lookup_expr = try env.mir_store.addExpr(allocator, .{ .lookup = sym_x }, i64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(lookup_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);
    try testing.expect(lir_expr == .lookup);
    try testing.expect(lir_expr.lookup.symbol.eql(sym_x));
}

test "MIR block lowers to LIR block" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Create a simple block: { x = 42; x }
    const ident_x = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_x = try testMirSymbol(&env.mir_store, allocator, ident_x);

    const int_42 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    const pat_x = try env.mir_store.addPattern(allocator, .{ .bind = sym_x }, i64_mono);
    const stmts = try env.mir_store.addStmts(allocator, &.{.{ .decl_const = .{ .pattern = pat_x, .expr = int_42 } }});

    const lookup_x = try env.mir_store.addExpr(allocator, .{ .lookup = sym_x }, i64_mono, Region.zero());

    const block_expr = try env.mir_store.addExpr(allocator, .{ .block = .{
        .stmts = stmts,
        .final_expr = lookup_x,
    } }, i64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(block_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);
    try testing.expect(lir_expr == .block);

    // The block should have 1 statement and a final lookup
    const lir_stmts = env.lir_store.getStmts(lir_expr.block.stmts);
    try testing.expectEqual(@as(usize, 1), lir_stmts.len);

    const final = env.lir_store.getExpr(lir_expr.block.final_expr);
    try testing.expect(final == .lookup);
}

test "MIR match with pattern alternatives lowers to multiple LIR match-branches" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Condition: integer literal 1
    const cond = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    // Body for the alternatives branch: integer literal 99
    const body1 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 99)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    // Body for the wildcard branch: integer literal 0
    const body2 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    // Two wildcard patterns simulating `_ | _ => 99` (2 alternatives in one branch)
    const pat1 = try env.mir_store.addPattern(allocator, .wildcard, i64_mono);
    const pat2 = try env.mir_store.addPattern(allocator, .wildcard, i64_mono);
    const bp_multi = try env.mir_store.addBranchPatterns(allocator, &.{
        .{ .pattern = pat1, .degenerate = false },
        .{ .pattern = pat2, .degenerate = false },
    });

    // Wildcard fallback branch: `_ => 0`
    const pat3 = try env.mir_store.addPattern(allocator, .wildcard, i64_mono);
    const bp_single = try env.mir_store.addBranchPatterns(allocator, &.{
        .{ .pattern = pat3, .degenerate = false },
    });

    // Two MIR branches: one with 2 alternatives, one with 1
    const branches = try env.mir_store.addBranches(allocator, &.{
        .{ .patterns = bp_multi, .body = body1, .guard = MIR.ExprId.none },
        .{ .patterns = bp_single, .body = body2, .guard = MIR.ExprId.none },
    });

    const match_expr = try env.mir_store.addExpr(allocator, .{ .match_expr = .{
        .cond = cond,
        .branches = branches,
    } }, i64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(match_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Should be a when expression
    try testing.expect(lir_expr == .match_expr);

    // Should have 3 LIR branches: 2 from alternatives + 1 from wildcard
    const lir_branches = env.lir_store.getMatchBranches(lir_expr.match_expr.branches);
    try testing.expectEqual(@as(usize, 3), lir_branches.len);

    // The first two branches should share the same body
    try testing.expectEqual(lir_branches[0].body, lir_branches[1].body);

    // The third branch should have a different body
    try testing.expect(lir_branches[2].body != lir_branches[0].body);
}

test "MIR multi-tag union produces proper tag_union layout" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Create a 2-tag union: [Foo I64, Bar]
    // Tags are sorted alphabetically: Bar < Foo, so Bar=0, Foo=1
    const tag_bar = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const tag_foo = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };

    const foo_payloads = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{i64_mono});

    const tag_span = try env.mir_store.monotype_store.addTags(allocator, &.{
        .{ .name = tag_bar, .payloads = Monotype.Span.empty() },
        .{ .name = tag_foo, .payloads = foo_payloads },
    });
    const union_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .tag_union = .{ .tags = tag_span } });
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const layout_idx = try translator.layoutFromMonotype(union_mono);
    const result_layout = env.layout_store.getLayout(layout_idx);

    // Should be a proper tag_union, not a tuple
    try testing.expect(result_layout.tag == .tag_union);

    // Check tag union data
    const tu_data = env.layout_store.getTagUnionData(result_layout.data.tag_union.idx);

    // 2 tags → discriminant_size should be 1
    try testing.expectEqual(@as(u8, 1), tu_data.discriminant_size);

    // Max payload is I64 (8 bytes), so discriminant_offset >= 8
    try testing.expect(tu_data.discriminant_offset >= 8);

    // Check that we have 2 variants
    const variants = env.layout_store.getTagUnionVariants(tu_data);
    try testing.expectEqual(@as(usize, 2), variants.len);
}

test "MIR multi-tag union tags get correct discriminants" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Create a 2-tag union: [Bar, Foo I64]
    // Sorted alphabetically by ident idx: Bar (idx=1) < Foo (idx=2)
    const tag_bar = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const tag_foo = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };

    const foo_payloads = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{i64_mono});

    const tag_span = try env.mir_store.monotype_store.addTags(allocator, &.{
        .{ .name = tag_bar, .payloads = Monotype.Span.empty() },
        .{ .name = tag_foo, .payloads = foo_payloads },
    });
    const union_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .tag_union = .{ .tags = tag_span } });

    // Lower a Bar tag (zero-arg, discriminant should be 0)
    const bar_expr = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = tag_bar,
        .args = MIR.ExprSpan.empty(),
    } }, union_mono, Region.zero());

    // Lower a Foo 42 tag (with payload, discriminant should be 1)
    const int_42 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const foo_args = try env.mir_store.addExprSpan(allocator, &.{int_42});
    const foo_expr = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = tag_foo,
        .args = foo_args,
    } }, union_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    // Check Bar → discriminant 0
    const lir_bar = try translator.lower(bar_expr);
    const bar_lir_expr = env.lir_store.getExpr(lir_bar);
    try testing.expect(bar_lir_expr == .zero_arg_tag);
    try testing.expectEqual(@as(u16, 0), bar_lir_expr.zero_arg_tag.discriminant);

    // Check Foo → discriminant 1
    const lir_foo = try translator.lower(foo_expr);
    const foo_lir_expr = env.lir_store.getExpr(lir_foo);
    try testing.expect(foo_lir_expr == .tag);
    try testing.expectEqual(@as(u16, 1), foo_lir_expr.tag.discriminant);
}

test "MIR function monotype lowers to closure layout" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const arg_span = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{i64_mono});
    const fn_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .func = .{
        .args = arg_span,
        .ret = i64_mono,
        .effectful = false,
    } });
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const fn_layout = try translator.layoutFromMonotype(fn_mono);
    try testing.expectEqual(layout.LayoutTag.closure, env.layout_store.getLayout(fn_layout).tag);
}

test "MIR record access finds correct field index for non-first field" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Create field name idents: a (idx=1), b (idx=2), c (idx=3)
    const field_a = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const field_b = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const field_c = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 3 };

    // Create record monotype: { a: I64, b: I64, c: I64 }
    const record_fields = try env.mir_store.monotype_store.addFields(allocator, &.{
        .{ .name = field_a, .type_idx = i64_mono },
        .{ .name = field_b, .type_idx = i64_mono },
        .{ .name = field_c, .type_idx = i64_mono },
    });
    const record_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .record = .{ .fields = record_fields } });

    // Create a record literal: { a: 1, b: 2, c: 3 }
    const int_1 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const int_2 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 2)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const int_3 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 3)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const field_exprs = try env.mir_store.addExprSpan(allocator, &.{ int_1, int_2, int_3 });
    const record_expr = try env.mir_store.addExpr(allocator, .{ .struct_ = .{
        .fields = field_exprs,
    } }, record_mono, Region.zero());

    // Access field c (third field, index 2)
    const access_expr = try env.mir_store.addExpr(allocator, .{ .struct_access = .{
        .struct_ = record_expr,
        .field_idx = 2,
    } }, i64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(access_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // ANF Let-binds the struct before accessing it
    try testing.expect(lir_expr == .block);
    const inner = env.lir_store.getExpr(lir_expr.block.final_expr);
    try testing.expect(inner == .struct_access);
    // Field c is at index 2 in the record's sorted field list
    try testing.expectEqual(@as(u16, 2), inner.struct_access.field_idx);
}

test "MIR single-field record lowers as struct_ and preserves field access" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const field_only = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };

    const record_fields = try env.mir_store.monotype_store.addFields(allocator, &.{
        .{ .name = field_only, .type_idx = i64_mono },
    });
    const record_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .record = .{ .fields = record_fields } });

    const int_1 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const field_exprs = try env.mir_store.addExprSpan(allocator, &.{int_1});
    const record_expr = try env.mir_store.addExpr(allocator, .{ .struct_ = .{
        .fields = field_exprs,
    } }, record_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lowered_record_id = try translator.lower(record_expr);
    const lowered_record = env.lir_store.getExpr(lowered_record_id);
    try testing.expect(lowered_record == .struct_);
    const lowered_record_layout = env.layout_store.getLayout(lowered_record.struct_.struct_layout);
    try testing.expectEqual(layout.LayoutTag.struct_, lowered_record_layout.tag);
    try testing.expectEqual(@as(usize, 1), env.lir_store.getExprSpan(lowered_record.struct_.fields).len);

    const access_expr = try env.mir_store.addExpr(allocator, .{ .struct_access = .{
        .struct_ = record_expr,
        .field_idx = 0,
    } }, i64_mono, Region.zero());
    const lowered_access_id = try translator.lower(access_expr);
    const lowered_access = env.lir_store.getExpr(lowered_access_id);
    try testing.expect(lowered_access == .block);
    const lowered_access_inner = env.lir_store.getExpr(lowered_access.block.final_expr);
    try testing.expect(lowered_access_inner == .struct_access);
    try testing.expectEqual(@as(u16, 0), lowered_access_inner.struct_access.field_idx);
}

test "MIR tuple access preserves element index" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const bool_mono = try env.mir_store.monotype_store.addBoolTagUnion(allocator, env.module_env.idents);

    // Create tuple monotype: (I64, Bool, I64)
    const tuple_elems = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{ i64_mono, bool_mono, i64_mono });
    const tuple_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .tuple = .{ .elems = tuple_elems } });

    // Create a tuple literal: (1, true, 2)
    const int_1 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const bool_true = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, bool_mono, Region.zero());
    const int_2 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 2)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const elem_exprs = try env.mir_store.addExprSpan(allocator, &.{ int_1, bool_true, int_2 });
    const tuple_expr = try env.mir_store.addExpr(allocator, .{ .struct_ = .{
        .fields = elem_exprs,
    } }, tuple_mono, Region.zero());

    // Access element at index 2 (third element, I64)
    const access_expr = try env.mir_store.addExpr(allocator, .{ .struct_access = .{
        .struct_ = tuple_expr,
        .field_idx = 2,
    } }, i64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(access_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // ANF Let-binds the struct before accessing it
    try testing.expect(lir_expr == .block);
    const inner = env.lir_store.getExpr(lir_expr.block.final_expr);
    try testing.expect(inner == .struct_access);
    // Original tuple element 2 (I64) is at sorted position 1 (sorted: I64@0, I64@2, Bool@1)
    try testing.expectEqual(@as(u16, 1), inner.struct_access.field_idx);
}

test "MIR single-element tuple lowers as struct_ and preserves field access" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const tuple_elems = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{i64_mono});
    const tuple_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .tuple = .{ .elems = tuple_elems } });

    const int_1 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const elem_exprs = try env.mir_store.addExprSpan(allocator, &.{int_1});
    const tuple_expr = try env.mir_store.addExpr(allocator, .{ .struct_ = .{
        .fields = elem_exprs,
    } }, tuple_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lowered_tuple_id = try translator.lower(tuple_expr);
    const lowered_tuple = env.lir_store.getExpr(lowered_tuple_id);
    try testing.expect(lowered_tuple == .struct_);
    const lowered_tuple_layout = env.layout_store.getLayout(lowered_tuple.struct_.struct_layout);
    try testing.expectEqual(layout.LayoutTag.struct_, lowered_tuple_layout.tag);
    try testing.expectEqual(@as(usize, 1), env.lir_store.getExprSpan(lowered_tuple.struct_.fields).len);

    const access_expr = try env.mir_store.addExpr(allocator, .{ .struct_access = .{
        .struct_ = tuple_expr,
        .field_idx = 0,
    } }, i64_mono, Region.zero());
    const lowered_access_id = try translator.lower(access_expr);
    const lowered_access = env.lir_store.getExpr(lowered_access_id);
    try testing.expect(lowered_access == .block);
    const lowered_access_inner = env.lir_store.getExpr(lowered_access.block.final_expr);
    try testing.expect(lowered_access_inner == .struct_access);
    try testing.expectEqual(@as(u16, 0), lowered_access_inner.struct_access.field_idx);
}

test "MIR lookup propagates symbol def to LIR store" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Register a symbol def in MIR: x = 42
    const ident_x = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_x = try testMirSymbol(&env.mir_store, allocator, ident_x);

    const int_42 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    try env.mir_store.registerSymbolDef(allocator, sym_x, int_42);

    // LIR store should NOT have the def yet
    try testing.expect(env.lir_store.getSymbolDef(sym_x) == null);

    // Lower a lookup to x
    const lookup_expr = try env.mir_store.addExpr(allocator, .{ .lookup = sym_x }, i64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(lookup_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);
    try testing.expect(lir_expr == .lookup);

    // LIR store should now have the propagated def
    const lir_def = env.lir_store.getSymbolDef(sym_x);
    try testing.expect(lir_def != null);

    // The propagated def should be an i64 literal (the lowered form of the int 42)
    const def_expr = env.lir_store.getExpr(lir_def.?);
    try testing.expect(def_expr == .i64_literal);
    try testing.expectEqual(@as(i64, 42), def_expr.i64_literal.value);
    try testing.expectEqual(layout.Idx.i64, def_expr.i64_literal.layout_idx);
}

test "MIR single-tag union with one payload emits tag layout" {
    // Canonical ordinary-data layouts preserve single-tag unions as tag unions.
    // Lowering should therefore keep the .tag node instead of collapsing to payload.
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Create a single-tag union monotype: [Ok I64]
    const tag_ok = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const ok_payloads = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{i64_mono});
    const tag_span = try env.mir_store.monotype_store.addTags(allocator, &.{
        .{ .name = tag_ok, .payloads = ok_payloads },
    });
    const union_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .tag_union = .{ .tags = tag_span } });

    // Create expression: Ok 42
    const int_42 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const ok_args = try env.mir_store.addExprSpan(allocator, &.{int_42});
    const tag_expr = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = tag_ok,
        .args = ok_args,
    } }, union_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(tag_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .tag);
    try testing.expectEqual(@as(u16, 0), lir_expr.tag.discriminant);

    const union_layout = env.layout_store.getLayout(lir_expr.tag.union_layout);
    try testing.expectEqual(layout.LayoutTag.tag_union, union_layout.tag);
    const tu_data = env.layout_store.getTagUnionData(union_layout.data.tag_union.idx);
    const variants = env.layout_store.getTagUnionVariants(tu_data);
    try testing.expectEqual(@as(usize, 1), variants.len);
    const payload_layout = env.layout_store.getLayout(variants.get(0).payload_layout);
    try testing.expectEqual(layout.LayoutTag.struct_, payload_layout.tag);
    const payload_data = env.layout_store.getStructData(payload_layout.data.struct_.idx);
    const payload_fields = env.layout_store.struct_fields.sliceRange(payload_data.getFields());
    try testing.expectEqual(@as(usize, 1), payload_fields.len);
    try testing.expectEqual(@as(u16, 0), payload_fields.get(0).index);
    try testing.expectEqual(layout.Idx.i64, payload_fields.get(0).layout);
}

test "MIR single-tag union with zero args emits zero_arg_tag" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    // Create a single zero-arg tag union: [Unit]
    const tag_unit = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const empty_payloads = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{});
    const tag_span = try env.mir_store.monotype_store.addTags(allocator, &.{
        .{ .name = tag_unit, .payloads = empty_payloads },
    });
    const union_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .tag_union = .{ .tags = tag_span } });

    // Create expression: Unit
    const empty_args = try env.mir_store.addExprSpan(allocator, &.{});
    const tag_expr = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = tag_unit,
        .args = empty_args,
    } }, union_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(tag_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .zero_arg_tag);
    try testing.expectEqual(@as(u16, 0), lir_expr.zero_arg_tag.discriminant);
    const union_layout = env.layout_store.getLayout(lir_expr.zero_arg_tag.union_layout);
    try testing.expectEqual(layout.LayoutTag.tag_union, union_layout.tag);
    const tu_data = env.layout_store.getTagUnionData(union_layout.data.tag_union.idx);
    const variants = env.layout_store.getTagUnionVariants(tu_data);
    try testing.expectEqual(@as(usize, 1), variants.len);
    try testing.expectEqual(layout.Idx.zst, variants.get(0).payload_layout);
}

test "MIR single-tag union with multiple payloads emits tag layout" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const bool_mono = try env.mir_store.monotype_store.addBoolTagUnion(allocator, env.module_env.idents);

    // Create a single-tag union with multiple payloads: [Pair I64 Bool]
    const tag_pair = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const pair_payloads = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{ i64_mono, bool_mono });
    const tag_span = try env.mir_store.monotype_store.addTags(allocator, &.{
        .{ .name = tag_pair, .payloads = pair_payloads },
    });
    const union_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .tag_union = .{ .tags = tag_span } });

    // Create expression: Pair 42 true
    const int_42 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const bool_true = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, bool_mono, Region.zero());
    const pair_args = try env.mir_store.addExprSpan(allocator, &.{ int_42, bool_true });
    const tag_expr = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = tag_pair,
        .args = pair_args,
    } }, union_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(tag_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .tag);
    try testing.expectEqual(@as(u16, 0), lir_expr.tag.discriminant);

    const union_layout = env.layout_store.getLayout(lir_expr.tag.union_layout);
    try testing.expectEqual(layout.LayoutTag.tag_union, union_layout.tag);
    const tu_data = env.layout_store.getTagUnionData(union_layout.data.tag_union.idx);
    const variants = env.layout_store.getTagUnionVariants(tu_data);
    try testing.expectEqual(@as(usize, 1), variants.len);
    const payload_layout = env.layout_store.getLayout(variants.get(0).payload_layout);
    try testing.expectEqual(layout.LayoutTag.struct_, payload_layout.tag);
}

test "MIR single-tag union pattern with one arg preserves tag pattern" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Create a single-tag union monotype: [Ok I64]
    const tag_ok = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const ok_payloads = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{i64_mono});
    const tag_span = try env.mir_store.monotype_store.addTags(allocator, &.{
        .{ .name = tag_ok, .payloads = ok_payloads },
    });
    const union_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .tag_union = .{ .tags = tag_span } });

    // Create pattern: Ok x (bind the payload)
    const ident_x = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const sym_x = try testMirSymbol(&env.mir_store, allocator, ident_x);
    const bind_pat = try env.mir_store.addPattern(allocator, .{ .bind = sym_x }, i64_mono);
    const pat_args = try env.mir_store.addPatternSpan(allocator, &.{bind_pat});
    const tag_pat = try env.mir_store.addPattern(allocator, .{ .tag = .{
        .name = tag_ok,
        .args = pat_args,
    } }, union_mono);
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_pat_id = try translator.lowerPattern(tag_pat);
    const lir_pat = env.lir_store.getPattern(lir_pat_id);

    try testing.expect(lir_pat == .tag);
    try testing.expectEqual(@as(u16, 0), lir_pat.tag.discriminant);
    try testing.expectEqual(@as(usize, 1), env.lir_store.getPatternSpan(lir_pat.tag.args).len);
}

test "MIR hosted lambda lowers to LIR lambda wrapping hosted_call" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Create function type: (I64, I64) -> I64
    const func_arg_span = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{ i64_mono, i64_mono });
    const func_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .func = .{
        .args = func_arg_span,
        .ret = i64_mono,
        .effectful = false,
    } });

    // Create parameter patterns (two binds)
    const ident_a = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_a = try testMirSymbol(&env.mir_store, allocator, ident_a);
    const ident_b = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const sym_b = try testMirSymbol(&env.mir_store, allocator, ident_b);

    const pat_a = try env.mir_store.addPattern(allocator, .{ .bind = sym_a }, i64_mono);
    const pat_b = try env.mir_store.addPattern(allocator, .{ .bind = sym_b }, i64_mono);
    const param_span = try env.mir_store.addPatternSpan(allocator, &.{ pat_a, pat_b });

    // Hosted body is a crash placeholder (not used by new lowerHosted)
    const crash_body = try env.mir_store.addExpr(allocator, .runtime_err_ellipsis, i64_mono, Region.zero());

    // Create hosted expression
    const ident_name = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 3 };
    const hosted_expr = try env.mir_store.addExpr(allocator, .{ .hosted = .{
        .symbol_name = ident_name,
        .index = 7,
        .params = param_span,
        .body = crash_body,
    } }, func_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(hosted_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Should be a lambda wrapping a hosted_call
    try testing.expect(lir_expr == .lambda);

    // The body of the lambda should be a hosted_call
    const body_expr = env.lir_store.getExpr(lir_expr.lambda.body);
    try testing.expect(body_expr == .hosted_call);

    // The hosted_call should have 2 args (one per parameter)
    const args = env.lir_store.getExprSpan(body_expr.hosted_call.args);
    try testing.expectEqual(@as(usize, 2), args.len);

    // Each arg should be a lookup
    const arg0 = env.lir_store.getExpr(args[0]);
    const arg1 = env.lir_store.getExpr(args[1]);
    try testing.expect(arg0 == .lookup);
    try testing.expect(arg1 == .lookup);

    // The hosted_call index should be preserved
    try testing.expectEqual(@as(u32, 7), body_expr.hosted_call.index);
}

test "lambdaSetForExpr unwraps dbg_expr wrapper" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    const func_arg_span = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{i64_mono});
    const func_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .func = .{
        .args = func_arg_span,
        .ret = i64_mono,
        .effectful = false,
    } });

    const ident_arg = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_arg = try testMirSymbol(&env.mir_store, allocator, ident_arg);
    const pat_arg = try env.mir_store.addPattern(allocator, .{ .bind = sym_arg }, i64_mono);
    const params = try env.mir_store.addPatternSpan(allocator, &.{pat_arg});

    const body = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const lambda_expr = try env.mir_store.addExpr(allocator, .{ .lambda = .{
        .params = params,
        .body = body,
        .captures = MIR.CaptureSpan.empty(),
    } }, func_mono, Region.zero());
    const dbg_expr = try env.mir_store.addExpr(allocator, .{ .dbg_expr = .{
        .expr = lambda_expr,
    } }, func_mono, Region.zero());

    const members = try env.lambda_set_store.addMembers(allocator, &.{.{
        .fn_symbol = Symbol.none,
        .closure_member = .none,
    }});
    const ls_idx = try env.lambda_set_store.addLambdaSet(allocator, .{ .members = members });
    try env.lambda_set_store.expr_lambda_sets.put(allocator, @intFromEnum(lambda_expr), ls_idx);

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    try testing.expectEqual(ls_idx, translator.lambdaSetForExpr(dbg_expr).?);
}

test "MIR function lookup uses symbol lambda set before wrapper def layout" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    const func_arg_span = try env.mir_store.monotype_store.addIdxSpan(allocator, &.{i64_mono});
    const func_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .func = .{
        .args = func_arg_span,
        .ret = i64_mono,
        .effectful = false,
    } });

    const ident_arg = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_arg = try testMirSymbol(&env.mir_store, allocator, ident_arg);
    const pat_arg = try env.mir_store.addPattern(allocator, .{ .bind = sym_arg }, i64_mono);
    const params = try env.mir_store.addPatternSpan(allocator, &.{pat_arg});

    const crash_body = try env.mir_store.addExpr(allocator, .runtime_err_ellipsis, i64_mono, Region.zero());
    const hosted_name = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const hosted_expr = try env.mir_store.addExpr(allocator, .{ .hosted = .{
        .symbol_name = hosted_name,
        .index = 42,
        .params = params,
        .body = crash_body,
    } }, func_mono, Region.zero());

    const ident_f = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 3 };
    const sym_f = try testMirSymbol(&env.mir_store, allocator, ident_f);
    try env.mir_store.registerSymbolDef(allocator, sym_f, hosted_expr);

    const members = try env.lambda_set_store.addMembers(allocator, &.{.{
        .fn_symbol = sym_f,
        .closure_member = .none,
    }});
    const ls_idx = try env.lambda_set_store.addLambdaSet(allocator, .{ .members = members });
    try env.lambda_set_store.symbol_lambda_sets.put(allocator, sym_f.raw(), ls_idx);

    const lookup_expr = try env.mir_store.addExpr(allocator, .{ .lookup = sym_f }, func_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(lookup_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .lookup);
    try testing.expectEqual(layout.Idx.zst, lir_expr.lookup.layout_idx);
}

test "MIR block with decl_var and mutate_var lowers to LIR decl and mutate" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Build MIR: { var s = 1; s = 2; s }
    const ident_s = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = true }, .idx = 1 };
    const sym_s = try testMirSymbol(&env.mir_store, allocator, ident_s);

    const int_1 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    const int_2 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 2)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    const pat_s_decl = try env.mir_store.addPattern(allocator, .{ .bind = sym_s }, i64_mono);
    const pat_s_mut = try env.mir_store.addPattern(allocator, .{ .bind = sym_s }, i64_mono);

    const stmts = try env.mir_store.addStmts(allocator, &.{
        .{ .decl_var = .{ .pattern = pat_s_decl, .expr = int_1 } },
        .{ .mutate_var = .{ .pattern = pat_s_mut, .expr = int_2 } },
    });

    const lookup_s = try env.mir_store.addExpr(allocator, .{ .lookup = sym_s }, i64_mono, Region.zero());

    const block_expr = try env.mir_store.addExpr(allocator, .{ .block = .{
        .stmts = stmts,
        .final_expr = lookup_s,
    } }, i64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(block_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);
    try testing.expect(lir_expr == .block);

    // The block should have 2 statements
    const lir_stmts = env.lir_store.getStmts(lir_expr.block.stmts);
    try testing.expectEqual(@as(usize, 2), lir_stmts.len);

    // Mutable bindings lower through explicit cell ops.
    try testing.expect(lir_stmts[0] == .cell_init);
    try testing.expect(lir_stmts[1] == .cell_store);

    // Final expression reads the cell.
    const final = env.lir_store.getExpr(lir_expr.block.final_expr);
    try testing.expect(final == .cell_load);
}

test "MIR for_loop lowers to LIR for_loop" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const list_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .list = .{ .elem = i64_mono } });
    const unit_mono = env.mir_store.monotype_store.unit_idx;

    // Build MIR: for elem in list { body }
    // list expression: empty list of I64
    const list_expr = try env.mir_store.addExpr(allocator, .{ .list = .{
        .elems = MIR.ExprSpan.empty(),
    } }, list_mono, Region.zero());

    // elem pattern: wildcard
    const elem_pat = try env.mir_store.addPattern(allocator, .{ .wildcard = {} }, i64_mono);

    // body: integer literal 0 (just a placeholder body)
    const body_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    // for_loop expression
    const for_expr = try env.mir_store.addExpr(allocator, .{ .for_loop = .{
        .list = list_expr,
        .elem_pattern = elem_pat,
        .body = body_expr,
    } }, unit_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(for_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .for_loop);
}

test "MIR while_loop lowers to LIR while_loop" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const bool_mono = try env.mir_store.monotype_store.addBoolTagUnion(allocator, env.module_env.idents);
    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const unit_mono = env.mir_store.monotype_store.unit_idx;

    // Build MIR: while cond { body }
    // cond: a bool placeholder (int literal used as stand-in)
    const cond_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, bool_mono, Region.zero());

    // body: integer literal
    const body_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    // while_loop expression
    const while_expr = try env.mir_store.addExpr(allocator, .{ .while_loop = .{
        .cond = cond_expr,
        .body = body_expr,
    } }, unit_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(while_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .while_loop);
}

test "MIR dbg_expr lowers to LIR dbg" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Build MIR: dbg(42)
    const inner_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    const dbg_expr = try env.mir_store.addExpr(allocator, .{ .dbg_expr = .{
        .expr = inner_expr,
    } }, i64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(dbg_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .dbg);
}

test "MIR expect lowers to LIR expect" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const bool_mono = try env.mir_store.monotype_store.addBoolTagUnion(allocator, env.module_env.idents);
    const unit_mono = env.mir_store.monotype_store.unit_idx;

    // Build MIR: expect (true)
    const cond_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, bool_mono, Region.zero());

    const expect_expr = try env.mir_store.addExpr(allocator, .{ .expect = .{
        .body = cond_expr,
    } }, unit_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(expect_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .expect);
}

test "MIR crash lowers to LIR crash" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const unit_mono = env.mir_store.monotype_store.unit_idx;

    // Build MIR: crash "msg"
    const crash_expr = try env.mir_store.addExpr(allocator, .{
        .crash = StringLiteral.Idx.none,
    }, unit_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(crash_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .crash);
}

test "MIR return_expr lowers to LIR early_return" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Build MIR: return 42
    const inner_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    const return_expr = try env.mir_store.addExpr(allocator, .{ .return_expr = .{
        .expr = inner_expr,
    } }, i64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(return_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .early_return);
}

test "MIR break_expr lowers to LIR break_expr" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const unit_mono = env.mir_store.monotype_store.unit_idx;

    // Build MIR: break
    const break_expr_id = try env.mir_store.addExpr(allocator, .{
        .break_expr = {},
    }, unit_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(break_expr_id);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .break_expr);
}

test "MIR num_plus low-level lowers to low-level" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Build MIR: run_low_level(.num_plus, [10, 20])
    const arg0 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 10)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const arg1 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 20)), .kind = .i128 },
    } }, i64_mono, Region.zero());

    const args = try env.mir_store.addExprSpan(allocator, &.{ arg0, arg1 });

    const ll_expr = try env.mir_store.addExpr(allocator, .{ .run_low_level = .{
        .op = .num_plus,
        .args = args,
    } }, i64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(ll_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .low_level);
    try testing.expect(lir_expr.low_level.op == .num_plus);
}

test "borrowed low-level temp arg lowers through explicit block binding" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const list_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .list = .{ .elem = i64_mono } });

    const elem0 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 3)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const elem1 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 4)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const list_elems = try env.mir_store.addExprSpan(allocator, &.{ elem0, elem1 });
    const list_expr = try env.mir_store.addExpr(allocator, .{ .list = .{ .elems = list_elems } }, list_mono, Region.zero());

    const index_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const args = try env.mir_store.addExprSpan(allocator, &.{ list_expr, index_expr });
    const ll_expr = try env.mir_store.addExpr(allocator, .{ .run_low_level = .{
        .op = .list_get_unsafe,
        .args = args,
    } }, i64_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(ll_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .block);
    const stmts = env.lir_store.getStmts(lir_expr.block.stmts);
    try testing.expectEqual(@as(usize, 1), stmts.len);
    try testing.expect(env.lir_store.getExpr(stmts[0].binding().expr) == .list);
    try testing.expectEqual(LirStmt.BindingSemantics.scoped_borrow, stmts[0].binding().semantics);

    const body = env.lir_store.getExpr(lir_expr.block.final_expr);
    try testing.expect(body == .low_level);
    try testing.expect(body.low_level.op == .list_get_unsafe);
}

test "borrowed low-level large string literal lowers through explicit block binding" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.str)];
    const u64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.u64)];

    const large_text =
        "This string is deliberately longer than RocStr small-string storage";
    const str_idx = try env.mir_store.strings.insert(allocator, large_text);
    const str_expr = try env.mir_store.addExpr(allocator, .{ .str = str_idx }, str_mono, Region.zero());
    const args = try env.mir_store.addExprSpan(allocator, &.{str_expr});
    const ll_expr = try env.mir_store.addExpr(allocator, .{ .run_low_level = .{
        .op = .str_count_utf8_bytes,
        .args = args,
    } }, u64_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(ll_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .block);
    const stmts = env.lir_store.getStmts(lir_expr.block.stmts);
    try testing.expectEqual(@as(usize, 1), stmts.len);
    try testing.expect(env.lir_store.getExpr(stmts[0].binding().expr) == .str_literal);
    try testing.expectEqual(LirStmt.BindingSemantics.scoped_borrow, stmts[0].binding().semantics);

    const body = env.lir_store.getExpr(lir_expr.block.final_expr);
    try testing.expect(body == .low_level);
    try testing.expect(body.low_level.op == .str_count_utf8_bytes);
}

test "borrow-only low-level lookup arg stays as plain low_level" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const list_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .list = .{ .elem = i64_mono } });

    const ident_list = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const sym_list = try testMirSymbol(&env.mir_store, allocator, ident_list);
    const list_lookup = try env.mir_store.addExpr(allocator, .{ .lookup = sym_list }, list_mono, Region.zero());

    const index_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 0)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const args = try env.mir_store.addExprSpan(allocator, &.{ list_lookup, index_expr });
    const ll_expr = try env.mir_store.addExpr(allocator, .{ .run_low_level = .{
        .op = .list_get_unsafe,
        .args = args,
    } }, i64_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(ll_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .low_level);
    try testing.expect(lir_expr.low_level.op == .list_get_unsafe);
}

test "MIR large unsigned int (U64 max) lowers to LIR i128_literal" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const u64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.u64)];

    // U64 max value = 18446744073709551615, which exceeds maxInt(i64)
    // so it should lower to i128_literal, not i64_literal
    const int_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(u128, std.math.maxInt(u64))), .kind = .u128 },
    } }, u64_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(int_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .i128_literal);
    try testing.expectEqual(layout.Idx.u64, lir_expr.i128_literal.layout_idx);
}

test "record access uses layout field order not monotype alphabetical order" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const u8_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.u8)];
    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Field names: age (idx=1), name (idx=2), score (idx=3)
    const field_age = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const field_name = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const field_score = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 3 };

    // Monotype fields are alphabetical: { age: U8, name: I64, score: I64 }
    const record_fields = try env.mir_store.monotype_store.addFields(allocator, &.{
        .{ .name = field_age, .type_idx = u8_mono },
        .{ .name = field_name, .type_idx = i64_mono },
        .{ .name = field_score, .type_idx = i64_mono },
    });
    const record_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .record = .{ .fields = record_fields } });

    // Create a record literal
    const int_1 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, u8_mono, Region.zero());
    const int_2 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 2)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const int_3 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 3)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    const field_exprs = try env.mir_store.addExprSpan(allocator, &.{ int_1, int_2, int_3 });
    const record_expr = try env.mir_store.addExpr(allocator, .{ .struct_ = .{
        .fields = field_exprs,
    } }, record_mono, Region.zero());

    // Access field "age" (U8) — alphabetically first but should be last in layout order
    // (I64 fields sorted before U8 by alignment)
    const access_expr = try env.mir_store.addExpr(allocator, .{ .struct_access = .{
        .struct_ = record_expr,
        .field_idx = 0,
    } }, u8_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(access_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // ANF Let-binds the struct before accessing it
    try testing.expect(lir_expr == .block);
    const inner = env.lir_store.getExpr(lir_expr.block.final_expr);
    try testing.expect(inner == .struct_access);
    // In layout order, I64 fields (name, score) come before U8 (age), so age is at index 2
    try testing.expectEqual(@as(u16, 2), inner.struct_access.field_idx);
}

test "record destructure wildcard gets actual field layout not zst" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const u8_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.u8)];
    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    const field_a = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const field_b = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const field_c = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 3 };

    // Record: { a: U8, b: I64, c: I64 }
    const record_fields = try env.mir_store.monotype_store.addFields(allocator, &.{
        .{ .name = field_a, .type_idx = u8_mono },
        .{ .name = field_b, .type_idx = i64_mono },
        .{ .name = field_c, .type_idx = i64_mono },
    });
    const record_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .record = .{ .fields = record_fields } });

    // Create a full-arity canonical destructure pattern that only binds field "a" (U8).
    // In canonical MIR order this is [a, b, c], but in layout order it becomes [b, c, a].
    // The wildcard entries must still carry the correct I64 layouts.
    const ident_a = field_a;
    const sym_a = try testMirSymbol(&env.mir_store, allocator, ident_a);
    const bind_pat = try env.mir_store.addPattern(allocator, .{ .bind = sym_a }, u8_mono);
    const wildcard_b = try env.mir_store.addPattern(allocator, .wildcard, i64_mono);
    const wildcard_c = try env.mir_store.addPattern(allocator, .wildcard, i64_mono);
    const destructs = try env.mir_store.addPatternSpan(allocator, &.{ bind_pat, wildcard_b, wildcard_c });
    const destruct_pat = try env.mir_store.addPattern(allocator, .{ .struct_destructure = .{
        .fields = destructs,
    } }, record_mono);
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_pat_id = try translator.lowerPattern(destruct_pat);
    const lir_pat = env.lir_store.getPattern(lir_pat_id);

    try testing.expect(lir_pat == .struct_);
    const field_pats = env.lir_store.getPatternSpan(lir_pat.struct_.fields);
    // Layout order: [b: I64, c: I64, a: U8] → 3 patterns
    try testing.expectEqual(@as(usize, 3), field_pats.len);

    // First two are wildcards for b and c (I64 layout)
    const pat0 = env.lir_store.getPattern(field_pats[0]);
    try testing.expect(pat0 == .wildcard);
    try testing.expectEqual(layout.Idx.i64, pat0.wildcard.layout_idx);

    const pat1 = env.lir_store.getPattern(field_pats[1]);
    try testing.expect(pat1 == .wildcard);
    try testing.expectEqual(layout.Idx.i64, pat1.wildcard.layout_idx);

    // Third is the bind for a (U8 layout)
    const pat2 = env.lir_store.getPattern(field_pats[2]);
    try testing.expect(pat2 == .bind);
}

test "MIR small i128 value emits i128_literal not i64_literal" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i128_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i128)];

    // Value 42 fits in i64, but monotype is i128.
    // Must emit i128_literal so LIR carries the correct 128-bit literal layout.
    const int_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i128_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(int_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .i128_literal);
    try testing.expectEqual(@as(i128, 42), lir_expr.i128_literal.value);
    try testing.expectEqual(layout.Idx.i128, lir_expr.i128_literal.layout_idx);
}

// --- Bool.not LIR structural tests ---
// Verify that a Bool tag-union match (like negBool produces) gets correct discriminants:
// True pattern → discriminant 1, False body → discriminant 0.

test "LIR Bool match: True pattern gets discriminant 1, False body gets discriminant 0" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const bool_mono = try env.mir_store.monotype_store.addBoolTagUnion(allocator, env.module_env.idents);
    const true_tag = env.module_env.idents.true_tag;
    const false_tag = env.module_env.idents.false_tag;

    // Build MIR: match <scrutinee> { True => False, _ => True }
    // This is exactly what negBool produces.

    // Scrutinee: Bool.True tag with ordinary Bool tag_union monotype
    const scrutinee = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = true_tag,
        .args = MIR.ExprSpan.empty(),
    } }, bool_mono, Region.zero());

    // Branch 0 body: Bool.False
    const false_expr = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = false_tag,
        .args = MIR.ExprSpan.empty(),
    } }, bool_mono, Region.zero());

    // Branch 1 body: Bool.True
    const true_expr = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = true_tag,
        .args = MIR.ExprSpan.empty(),
    } }, bool_mono, Region.zero());

    // Branch 0 pattern: True tag
    const true_pat = try env.mir_store.addPattern(allocator, .{ .tag = .{
        .name = true_tag,
        .args = MIR.PatternSpan.empty(),
    } }, bool_mono);

    // Branch 1 pattern: wildcard
    const wildcard_pat = try env.mir_store.addPattern(allocator, .wildcard, bool_mono);

    const bp0 = try env.mir_store.addBranchPatterns(allocator, &.{.{ .pattern = true_pat, .degenerate = false }});
    const bp1 = try env.mir_store.addBranchPatterns(allocator, &.{.{ .pattern = wildcard_pat, .degenerate = false }});

    const branches = try env.mir_store.addBranches(allocator, &.{
        .{ .patterns = bp0, .body = false_expr, .guard = MIR.ExprId.none },
        .{ .patterns = bp1, .body = true_expr, .guard = MIR.ExprId.none },
    });

    const match_expr = try env.mir_store.addExpr(allocator, .{ .match_expr = .{
        .cond = scrutinee,
        .branches = branches,
    } }, bool_mono, Region.zero());

    // Lower MIR → LIR
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(match_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);
    try testing.expect(lir_expr == .match_expr);

    const lir_branches = env.lir_store.getMatchBranches(lir_expr.match_expr.branches);
    try testing.expectEqual(@as(usize, 2), lir_branches.len);

    // Branch 0: True pattern → discriminant must be 1
    const lir_pat0 = env.lir_store.getPattern(lir_branches[0].pattern);
    try testing.expect(lir_pat0 == .tag);
    try testing.expectEqual(@as(u16, 1), lir_pat0.tag.discriminant);

    // Branch 0 body: False → discriminant must be 0
    const lir_body0 = env.lir_store.getExpr(lir_branches[0].body);
    try testing.expect(lir_body0 == .zero_arg_tag);
    try testing.expectEqual(@as(u16, 0), lir_body0.zero_arg_tag.discriminant);

    // Branch 1: wildcard pattern
    const lir_pat1 = env.lir_store.getPattern(lir_branches[1].pattern);
    try testing.expect(lir_pat1 == .wildcard);

    // Branch 1 body: True → discriminant must be 1
    const lir_body1 = env.lir_store.getExpr(lir_branches[1].body);
    try testing.expect(lir_body1 == .zero_arg_tag);
    try testing.expectEqual(@as(u16, 1), lir_body1.zero_arg_tag.discriminant);

    // Scrutinee should be True with discriminant 1
    const lir_scrutinee = env.lir_store.getExpr(lir_expr.match_expr.value);
    try testing.expect(lir_scrutinee == .zero_arg_tag);
    try testing.expectEqual(@as(u16, 1), lir_scrutinee.zero_arg_tag.discriminant);
}

test "LIR Bool match: False scrutinee gets discriminant 0" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const bool_mono = try env.mir_store.monotype_store.addBoolTagUnion(allocator, env.module_env.idents);
    const true_tag = env.module_env.idents.true_tag;
    const false_tag = env.module_env.idents.false_tag;

    // Build: match False { True => False, _ => True }
    const scrutinee = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = false_tag,
        .args = MIR.ExprSpan.empty(),
    } }, bool_mono, Region.zero());

    const false_expr = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = false_tag,
        .args = MIR.ExprSpan.empty(),
    } }, bool_mono, Region.zero());

    const true_expr = try env.mir_store.addExpr(allocator, .{ .tag = .{
        .name = true_tag,
        .args = MIR.ExprSpan.empty(),
    } }, bool_mono, Region.zero());

    const true_pat = try env.mir_store.addPattern(allocator, .{ .tag = .{
        .name = true_tag,
        .args = MIR.PatternSpan.empty(),
    } }, bool_mono);
    const wildcard_pat = try env.mir_store.addPattern(allocator, .wildcard, bool_mono);

    const bp0 = try env.mir_store.addBranchPatterns(allocator, &.{.{ .pattern = true_pat, .degenerate = false }});
    const bp1 = try env.mir_store.addBranchPatterns(allocator, &.{.{ .pattern = wildcard_pat, .degenerate = false }});

    const branches = try env.mir_store.addBranches(allocator, &.{
        .{ .patterns = bp0, .body = false_expr, .guard = MIR.ExprId.none },
        .{ .patterns = bp1, .body = true_expr, .guard = MIR.ExprId.none },
    });

    const match_expr = try env.mir_store.addExpr(allocator, .{ .match_expr = .{
        .cond = scrutinee,
        .branches = branches,
    } }, bool_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(match_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);
    try testing.expect(lir_expr == .match_expr);

    // Scrutinee: False → discriminant 0
    const lir_scrutinee = env.lir_store.getExpr(lir_expr.match_expr.value);
    try testing.expect(lir_scrutinee == .zero_arg_tag);
    try testing.expectEqual(@as(u16, 0), lir_scrutinee.zero_arg_tag.discriminant);

    // Branch 0: True pattern → discriminant 1
    const lir_branches = env.lir_store.getMatchBranches(lir_expr.match_expr.branches);
    const lir_pat0 = env.lir_store.getPattern(lir_branches[0].pattern);
    try testing.expect(lir_pat0 == .tag);
    try testing.expectEqual(@as(u16, 1), lir_pat0.tag.discriminant);

    // Body 0: False → discriminant 0
    const lir_body0 = env.lir_store.getExpr(lir_branches[0].body);
    try testing.expect(lir_body0 == .zero_arg_tag);
    try testing.expectEqual(@as(u16, 0), lir_body0.zero_arg_tag.discriminant);

    // Body 1: True → discriminant 1
    const lir_body1 = env.lir_store.getExpr(lir_branches[1].body);
    try testing.expect(lir_body1 == .zero_arg_tag);
    try testing.expectEqual(@as(u16, 1), lir_body1.zero_arg_tag.discriminant);
}

test "MIR small u128 value emits i128_literal not i64_literal" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const u128_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.u128)];

    // Value 7 fits in i64, but monotype is u128.
    // Must emit i128_literal so LIR carries the correct 128-bit literal layout.
    const int_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(u128, 7)), .kind = .u128 },
    } }, u128_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(int_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .i128_literal);
    try testing.expectEqual(@as(i128, 7), lir_expr.i128_literal.value);
    try testing.expectEqual(layout.Idx.u128, lir_expr.i128_literal.layout_idx);
}
