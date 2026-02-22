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

const std = @import("std");
const base = @import("base");
const layout = @import("layout");
const mir_mod = @import("mir");

const MIR = mir_mod.MIR;
const Monotype = mir_mod.Monotype;

const LIR = @import("LIR.zig");
const LirExprStore = @import("LirExprStore.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const StringLiteral = base.StringLiteral;
const CIR = @import("can").CIR;

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

allocator: Allocator,
mir_store: *const MIR.Store,
lir_store: *LirExprStore,
layout_store: *layout.Store,

/// Cache: Monotype.Idx → layout.Idx (avoid recomputation)
layout_cache: std.AutoHashMap(u32, layout.Idx),

/// Recursion guard for propagating symbol defs from MIR to LIR
propagating_defs: std.AutoHashMap(u64, void),

/// Scratch buffers for building spans
scratch_lir_expr_ids: std.ArrayList(LirExprId),
scratch_lir_pattern_ids: std.ArrayList(LirPatternId),
scratch_lir_stmts: std.ArrayList(LirStmt),
scratch_lir_match_branches: std.ArrayList(LirMatchBranch),
scratch_lir_captures: std.ArrayList(LirCapture),

/// Scratch buffers for layout building (reused across layoutFrom* calls)
scratch_layouts: std.ArrayList(layout.Layout),
scratch_layout_idxs: std.ArrayList(layout.Idx),
scratch_field_names: std.ArrayList(Ident.Idx),

pub fn init(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    lir_store: *LirExprStore,
    layout_store: *layout.Store,
) Self {
    return .{
        .allocator = allocator,
        .mir_store = mir_store,
        .lir_store = lir_store,
        .layout_store = layout_store,
        .layout_cache = std.AutoHashMap(u32, layout.Idx).init(allocator),
        .propagating_defs = std.AutoHashMap(u64, void).init(allocator),
        .scratch_lir_expr_ids = std.ArrayList(LirExprId).empty,
        .scratch_lir_pattern_ids = std.ArrayList(LirPatternId).empty,
        .scratch_lir_stmts = std.ArrayList(LirStmt).empty,
        .scratch_lir_match_branches = std.ArrayList(LirMatchBranch).empty,
        .scratch_lir_captures = std.ArrayList(LirCapture).empty,
        .scratch_layouts = std.ArrayList(layout.Layout).empty,
        .scratch_layout_idxs = std.ArrayList(layout.Idx).empty,
        .scratch_field_names = std.ArrayList(Ident.Idx).empty,
    };
}

pub fn deinit(self: *Self) void {
    self.layout_cache.deinit();
    self.propagating_defs.deinit();
    self.scratch_lir_expr_ids.deinit(self.allocator);
    self.scratch_lir_pattern_ids.deinit(self.allocator);
    self.scratch_lir_stmts.deinit(self.allocator);
    self.scratch_lir_match_branches.deinit(self.allocator);
    self.scratch_lir_captures.deinit(self.allocator);
    self.scratch_layouts.deinit(self.allocator);
    self.scratch_layout_idxs.deinit(self.allocator);
    self.scratch_field_names.deinit(self.allocator);
}

/// Lower a MIR expression to a LIR expression.
pub fn lower(self: *Self, mir_expr_id: MIR.ExprId) Allocator.Error!LirExprId {
    return self.lowerExpr(mir_expr_id);
}

/// Convert a Monotype.Idx to a layout.Idx, using a cache.
fn layoutFromMonotype(self: *Self, mono_idx: Monotype.Idx) Allocator.Error!layout.Idx {
    std.debug.assert(!mono_idx.isNone());
    const key = @intFromEnum(mono_idx);
    if (self.layout_cache.get(key)) |cached| return cached;

    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    const result = try self.layoutFromMonotypeInner(monotype);
    try self.layout_cache.put(key, result);
    return result;
}

fn layoutFromMonotypeInner(self: *Self, monotype: Monotype.Monotype) Allocator.Error!layout.Idx {
    return switch (monotype) {
        .unit => .zst,
        .prim => |p| switch (p) {
            .bool => .bool,
            .str => .str,
            .u8 => .u8,
            .i8 => .i8,
            .u16 => .u16,
            .i16 => .i16,
            .u32 => .u32,
            .i32 => .i32,
            .u64 => .u64,
            .i64 => .i64,
            .u128 => .u128,
            .i128 => .i128,
            .f32 => .f32,
            .f64 => .f64,
            .dec => .dec,
        },
        .list => |l| blk: {
            const elem_layout = try self.layoutFromMonotype(l.elem);
            break :blk try self.layout_store.insertList(elem_layout);
        },
        .box => |b| blk: {
            const inner_layout = try self.layoutFromMonotype(b.inner);
            break :blk try self.layout_store.insertBox(inner_layout);
        },
        .func => layout.Idx.named_fn,
        .record => |r| try self.layoutFromRecord(r),
        .tuple => |t| try self.layoutFromTuple(t),
        .tag_union => |tu| try self.layoutFromTagUnion(tu),
    };
}

fn layoutFromRecord(self: *Self, record: anytype) Allocator.Error!layout.Idx {
    const fields = self.mir_store.monotype_store.getFields(record.fields);
    if (fields.len == 0) return .zst;

    const env = self.layout_store.currentEnv();
    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);
    const save_names = self.scratch_field_names.items.len;
    defer self.scratch_field_names.shrinkRetainingCapacity(save_names);

    for (fields) |field| {
        const field_layout_idx = try self.layoutFromMonotype(field.type_idx);
        const field_layout = self.layout_store.getLayout(field_layout_idx);
        try self.scratch_layouts.append(self.allocator, field_layout);
        try self.scratch_field_names.append(self.allocator, field.name);
    }

    return self.layout_store.putRecord(env, self.scratch_layouts.items[save_layouts..], self.scratch_field_names.items[save_names..]);
}

fn layoutFromTuple(self: *Self, tup: anytype) Allocator.Error!layout.Idx {
    const elems = self.mir_store.monotype_store.getIdxSpan(tup.elems);
    if (elems.len == 0) return .zst;

    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);

    for (elems) |elem_mono_idx| {
        const elem_layout_idx = try self.layoutFromMonotype(elem_mono_idx);
        const elem_layout = self.layout_store.getLayout(elem_layout_idx);
        try self.scratch_layouts.append(self.allocator, elem_layout);
    }

    return self.layout_store.putTuple(self.scratch_layouts.items[save_layouts..]);
}

fn layoutFromTagUnion(self: *Self, tu: anytype) Allocator.Error!layout.Idx {
    const tags = self.mir_store.monotype_store.getTags(tu.tags);
    if (tags.len == 0) return .zst;

    // Single tag → no discriminant needed
    if (tags.len == 1) {
        const payloads = self.mir_store.monotype_store.getIdxSpan(tags[0].payloads);
        if (payloads.len == 0) return .zst;
        // Single tag with payload: return just the payload layout (no discriminant)
        if (payloads.len == 1) {
            return self.layoutFromMonotype(payloads[0]);
        }
        // Multiple payload fields: wrap in a tuple
        const save_layouts = self.scratch_layouts.items.len;
        defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);
        for (payloads) |p| {
            const p_idx = try self.layoutFromMonotype(p);
            try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(p_idx));
        }
        return self.layout_store.putTuple(self.scratch_layouts.items[save_layouts..]);
    }

    // Multi-tag union: build per-variant payload layouts
    const zst_idx = try self.layout_store.ensureZstLayout();

    const save_idxs = self.scratch_layout_idxs.items.len;
    defer self.scratch_layout_idxs.shrinkRetainingCapacity(save_idxs);

    for (tags) |tag| {
        const payloads = self.mir_store.monotype_store.getIdxSpan(tag.payloads);
        if (payloads.len == 0) {
            try self.scratch_layout_idxs.append(self.allocator, zst_idx);
        } else if (payloads.len == 1) {
            try self.scratch_layout_idxs.append(self.allocator, try self.layoutFromMonotype(payloads[0]));
        } else {
            const save_layouts = self.scratch_layouts.items.len;
            defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);
            for (payloads) |p| {
                const p_idx = try self.layoutFromMonotype(p);
                try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(p_idx));
            }
            try self.scratch_layout_idxs.append(self.allocator, try self.layout_store.putTuple(self.scratch_layouts.items[save_layouts..]));
        }
    }

    return self.layout_store.putTagUnion(self.scratch_layout_idxs.items[save_idxs..]);
}

/// Check if a monotype is a single-tag union (exactly one variant).
fn isSingleTagUnion(self: *const Self, mono_idx: Monotype.Idx) bool {
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    return switch (monotype) {
        .tag_union => |tu| self.mir_store.monotype_store.getTags(tu.tags).len == 1,
        else => false,
    };
}

/// Given a tag name and the monotype of the containing tag union,
/// return the discriminant (sorted index of the tag name).
fn tagDiscriminant(self: *const Self, tag_name: Ident.Idx, union_mono_idx: Monotype.Idx) u16 {
    const monotype = self.mir_store.monotype_store.getMonotype(union_mono_idx);
    switch (monotype) {
        .tag_union => |tu| {
            const tags = self.mir_store.monotype_store.getTags(tu.tags);

            if (std.debug.runtime_safety) {
                for (tags[0..tags.len -| 1], tags[1..]) |a, b| {
                    std.debug.assert(@as(u32, @bitCast(a.name)) < @as(u32, @bitCast(b.name)));
                }
            }

            for (tags, 0..) |tag, i| {
                if (@as(u32, @bitCast(tag.name)) == @as(u32, @bitCast(tag_name))) {
                    return @intCast(i);
                }
            }
            unreachable; // compiler bug: tag name not in tag union
        },
        else => unreachable, // compiler bug: expected tag_union monotype
    }
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
        .str => |s| self.lir_store.addExpr(.{ .str_literal = s }, region),
        .list => |l| self.lowerList(l, mono_idx, region),
        .record => |r| self.lowerRecord(r, mono_idx, region),
        .tuple => |t| self.lowerTuple(t, mono_idx, region),
        .tag => |t| self.lowerTag(t, mono_idx, region),
        .lookup => |sym| self.lowerLookup(sym, mono_idx, region),
        .match_expr => |m| self.lowerMatch(m, mono_idx, region),
        .lambda => |l| self.lowerLambda(l, mono_idx, region),
        .call => |c| self.lowerCall(c, mono_idx, region),
        .block => |b| self.lowerBlock(b, mono_idx, region),
        .record_access => |ra| self.lowerRecordAccess(ra, mir_expr_id, region),
        .tuple_access => |ta| self.lowerTupleAccess(ta, mir_expr_id, region),
        .run_low_level => |ll| self.lowerLowLevel(ll, mono_idx, region),
        .hosted => |h| self.lowerHosted(h, mono_idx, region),
        .runtime_err_can, .runtime_err_type, .runtime_err_ellipsis, .runtime_err_anno_only => {
            return self.lir_store.addExpr(.runtime_error, region);
        },
        .crash => |s| self.lir_store.addExpr(.{ .crash = .{ .msg = s } }, region),
        .dbg_expr => |d| self.lowerDbg(d, mono_idx, region),
        .expect => |e| self.lowerExpect(e, mono_idx, region),
        .for_loop => |f| self.lowerForLoop(f, mono_idx, region),
        .while_loop => |w| self.lowerWhileLoop(w, mono_idx, region),
        .return_expr => |r| self.lowerReturn(r, mono_idx, region),
        .break_expr => self.lir_store.addExpr(.break_expr, region),
    };
}

fn lowerInt(self: *Self, int_data: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    // Use the monotype to determine if this is a 128-bit type.
    // For 128-bit types, always emit i128_literal even if the value fits in i64,
    // because getExprLayout uses the literal type to determine size, and emitting
    // i64_literal for a u128/i128 variable would cause wrong-sized allocations.
    const target_layout = try self.layoutFromMonotype(mono_idx);
    const needs_128 = target_layout == .i128 or target_layout == .u128 or target_layout == .dec;

    switch (int_data.value.kind) {
        .u128 => {
            const val: u128 = @bitCast(int_data.value.bytes);
            if (!needs_128 and val <= std.math.maxInt(i64)) {
                return self.lir_store.addExpr(.{ .i64_literal = @intCast(val) }, region);
            }
            return self.lir_store.addExpr(.{ .i128_literal = @bitCast(val) }, region);
        },
        .i128 => {
            const val = int_data.value.toI128();
            if (!needs_128 and val >= std.math.minInt(i64) and val <= std.math.maxInt(i64)) {
                return self.lir_store.addExpr(.{ .i64_literal = @intCast(val) }, region);
            }
            return self.lir_store.addExpr(.{ .i128_literal = val }, region);
        },
    }
}

fn lowerList(self: *Self, list_data: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    const elem_layout = switch (monotype) {
        .list => |l| try self.layoutFromMonotype(l.elem),
        else => unreachable,
    };

    const mir_elems = self.mir_store.getExprSpan(list_data.elems);
    if (mir_elems.len == 0) {
        return self.lir_store.addExpr(.{ .empty_list = .{ .elem_layout = elem_layout } }, region);
    }

    const lir_elems = try self.lowerExprSpan(mir_elems);
    return self.lir_store.addExpr(.{ .list = .{ .elem_layout = elem_layout, .elems = lir_elems } }, region);
}

fn lowerRecord(self: *Self, rec: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const mir_fields = self.mir_store.getExprSpan(rec.fields);
    if (mir_fields.len == 0) {
        return self.lir_store.addExpr(.empty_record, region);
    }

    const record_layout = try self.layoutFromMonotype(mono_idx);
    const mir_field_names = self.mir_store.getFieldNameSpan(rec.field_names);
    const record_layout_val = self.layout_store.getLayout(record_layout);

    if (record_layout_val.tag == .record) {
        // MIR fields are in source/alphabetical order, but the layout store sorts
        // fields by alignment descending then alphabetically. Reorder expressions
        // to match layout order so codegen can use positional field indices.
        const record_data = self.layout_store.getRecordData(record_layout_val.data.record.idx);
        const layout_fields = self.layout_store.record_fields.sliceRange(record_data.getFields());

        const save_exprs = self.scratch_lir_expr_ids.items.len;
        defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);
        const save_names = self.scratch_field_names.items.len;
        defer self.scratch_field_names.shrinkRetainingCapacity(save_names);

        for (0..layout_fields.len) |li| {
            const layout_field_name = layout_fields.get(li).name;
            var found = false;
            for (mir_field_names, 0..) |mir_name, mi| {
                if (@as(u32, @bitCast(mir_name)) == @as(u32, @bitCast(layout_field_name))) {
                    const lir_expr = try self.lowerExpr(mir_fields[mi]);
                    try self.scratch_lir_expr_ids.append(self.allocator, lir_expr);
                    try self.scratch_field_names.append(self.allocator, mir_name);
                    found = true;
                    break;
                }
            }
            std.debug.assert(found);
        }

        const lir_fields = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);
        const lir_field_names = try self.lir_store.addFieldNameSpan(self.scratch_field_names.items[save_names..]);

        return self.lir_store.addExpr(.{ .record = .{
            .record_layout = record_layout,
            .fields = lir_fields,
            .field_names = lir_field_names,
        } }, region);
    } else {
        // Non-record layout (e.g. single-field optimization or ZST) — pass through directly
        const lir_fields = try self.lowerExprSpan(mir_fields);
        const lir_field_names = try self.lir_store.addFieldNameSpan(mir_field_names);

        return self.lir_store.addExpr(.{ .record = .{
            .record_layout = record_layout,
            .fields = lir_fields,
            .field_names = lir_field_names,
        } }, region);
    }
}

fn lowerTuple(self: *Self, tup: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const tuple_layout = try self.layoutFromMonotype(mono_idx);
    const mir_elems = self.mir_store.getExprSpan(tup.elems);
    const lir_elems = try self.lowerExprSpan(mir_elems);
    return self.lir_store.addExpr(.{ .tuple = .{ .tuple_layout = tuple_layout, .elems = lir_elems } }, region);
}

fn lowerTag(self: *Self, tag_data: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const mir_args = self.mir_store.getExprSpan(tag_data.args);

    // Single-tag unions are optimized to just their payload layout (no discriminant),
    // so we must emit the payload directly instead of a .tag LIR node.
    if (self.isSingleTagUnion(mono_idx)) {
        if (mir_args.len == 0) {
            // Zero-arg single tag → ZST, emit zero_arg_tag as before
            const union_layout = try self.layoutFromMonotype(mono_idx);
            return self.lir_store.addExpr(.{ .zero_arg_tag = .{
                .discriminant = 0,
                .union_layout = union_layout,
            } }, region);
        } else if (mir_args.len == 1) {
            // Single payload → layout is just the payload type, emit it directly
            return self.lowerExpr(mir_args[0]);
        } else {
            // Multiple payloads → layout is a tuple, emit a tuple expression
            const tuple_layout = try self.layoutFromMonotype(mono_idx);
            const lir_elems = try self.lowerExprSpan(mir_args);
            return self.lir_store.addExpr(.{ .tuple = .{ .tuple_layout = tuple_layout, .elems = lir_elems } }, region);
        }
    }

    const union_layout = try self.layoutFromMonotype(mono_idx);
    const discriminant = self.tagDiscriminant(tag_data.name, mono_idx);

    if (mir_args.len == 0) {
        return self.lir_store.addExpr(.{ .zero_arg_tag = .{
            .discriminant = discriminant,
            .union_layout = union_layout,
        } }, region);
    }

    const lir_args = try self.lowerExprSpan(mir_args);
    return self.lir_store.addExpr(.{ .tag = .{
        .discriminant = discriminant,
        .union_layout = union_layout,
        .args = lir_args,
    } }, region);
}

fn lowerLookup(self: *Self, sym: Symbol, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const layout_idx = try self.layoutFromMonotype(mono_idx);

    // Propagate MIR symbol definition to LIR store (if exists and not already done)
    if (self.lir_store.getSymbolDef(sym) == null) {
        if (self.mir_store.getSymbolDef(sym)) |mir_def_id| {
            const key: u64 = @bitCast(sym);
            if (!self.propagating_defs.contains(key)) {
                try self.propagating_defs.put(key, {});
                defer _ = self.propagating_defs.remove(key);
                const lir_def_id = try self.lowerExpr(mir_def_id);
                try self.lir_store.registerSymbolDef(sym, lir_def_id);
            }
        }
    }

    return self.lir_store.addExpr(.{ .lookup = .{ .symbol = sym, .layout_idx = layout_idx } }, region);
}

fn lowerMatch(self: *Self, match_data: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const cond_id = try self.lowerExpr(match_data.cond);
    const cond_mono = self.mir_store.typeOf(match_data.cond);
    const value_layout = try self.layoutFromMonotype(cond_mono);
    const result_layout = try self.layoutFromMonotype(mono_idx);

    const mir_branches = self.mir_store.getBranches(match_data.branches);

    const save_len = self.scratch_lir_match_branches.items.len;
    defer self.scratch_lir_match_branches.shrinkRetainingCapacity(save_len);
    for (mir_branches) |branch| {
        const branch_patterns = self.mir_store.getBranchPatterns(branch.patterns);
        if (branch_patterns.len == 0) continue;

        // OR-patterns: a single MIR branch may have multiple patterns.
        // We lower the body once and share the LIR body ID across all
        // resulting LIR branches. This is safe because RC insertion runs
        // at the MIR level (rc_insert.zig processMatch), where each MIR
        // branch gets its own RC wrapper — so by this point, RC ops are
        // already embedded in the body expression.
        const lir_body = try self.lowerExpr(branch.body);
        const guard = if (branch.guard.isNone())
            LirExprId.none
        else
            try self.lowerExpr(branch.guard);

        for (branch_patterns) |bp| {
            const lir_pat = try self.lowerPattern(bp.pattern);
            try self.scratch_lir_match_branches.append(self.allocator, .{
                .pattern = lir_pat,
                .guard = guard,
                .body = lir_body,
            });
        }
    }

    const match_branches = try self.lir_store.addMatchBranches(self.scratch_lir_match_branches.items[save_len..]);
    return self.lir_store.addExpr(.{ .match_expr = .{
        .value = cond_id,
        .value_layout = value_layout,
        .branches = match_branches,
        .result_layout = result_layout,
    } }, region);
}

fn lowerLambda(self: *Self, lam: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const fn_layout = try self.layoutFromMonotype(mono_idx);
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    const ret_layout = switch (monotype) {
        .func => |f| try self.layoutFromMonotype(f.ret),
        else => unreachable, // Lambda expressions always have .func monotype
    };

    const lir_params = try self.lowerPatternSpan(self.mir_store.getPatternSpan(lam.params));
    const lir_body = try self.lowerExpr(lam.body);

    // Check if this lambda has captures → closure
    const mir_captures = self.mir_store.getCaptures(lam.captures);
    if (mir_captures.len > 0) {
        // Build capture list with layouts
        const save_captures_len = self.scratch_lir_captures.items.len;
        defer self.scratch_lir_captures.shrinkRetainingCapacity(save_captures_len);
        for (mir_captures) |cap| {
            // Look up the captured symbol's type by searching symbol_defs
            const cap_layout = if (self.mir_store.getSymbolDef(cap.symbol)) |def_id|
                try self.layoutFromMonotype(self.mir_store.typeOf(def_id))
            else
                unreachable;

            try self.scratch_lir_captures.append(self.allocator, .{
                .symbol = cap.symbol,
                .layout_idx = cap_layout,
            });
        }

        const lir_captures = try self.lir_store.addCaptures(self.scratch_lir_captures.items[save_captures_len..]);

        // Create the lambda expression first
        const lambda_expr = try self.lir_store.addExpr(.{ .lambda = .{
            .fn_layout = fn_layout,
            .params = lir_params,
            .body = lir_body,
            .ret_layout = ret_layout,
        } }, region);

        // Compute closure layout and representation based on capture count
        const capture_items = self.scratch_lir_captures.items[save_captures_len..];
        if (capture_items.len == 1) {
            // Single capture: unwrapped_capture (zero overhead)
            const cap_layout = capture_items[0].layout_idx;
            const closure_data_id = try self.lir_store.addClosureData(.{
                .closure_layout = cap_layout,
                .lambda = lambda_expr,
                .captures = lir_captures,
                .representation = .{ .unwrapped_capture = .{
                    .capture_layout = cap_layout,
                } },
                // Recursion flags default to not_recursive here. The TailRecursion pass
                // (run after lowering, before RC insertion) analyzes the call graph and
                // updates these flags for self-recursive closures.
                .recursion = .not_recursive,
                .self_recursive = .not_self_recursive,
                .is_bound_to_variable = false,
            });
            return self.lir_store.addExpr(.{ .closure = closure_data_id }, region);
        } else {
            // Multiple captures: struct_captures with a tuple layout (positional, no names needed)
            var cap_layout_idxs = std.ArrayList(layout.Idx).empty;
            defer cap_layout_idxs.deinit(self.allocator);
            for (capture_items) |cap| {
                try cap_layout_idxs.append(self.allocator, cap.layout_idx);
            }
            const closure_layout = try self.layout_store.putCaptureStruct(cap_layout_idxs.items);
            const closure_data_id = try self.lir_store.addClosureData(.{
                .closure_layout = closure_layout,
                .lambda = lambda_expr,
                .captures = lir_captures,
                .representation = .{ .struct_captures = .{
                    .captures = lir_captures,
                    .struct_layout = closure_layout,
                } },
                // Recursion flags default to not_recursive here. The TailRecursion pass
                // (run after lowering, before RC insertion) analyzes the call graph and
                // updates these flags for self-recursive closures.
                .recursion = .not_recursive,
                .self_recursive = .not_self_recursive,
                .is_bound_to_variable = false,
            });
            return self.lir_store.addExpr(.{ .closure = closure_data_id }, region);
        }
    }

    return self.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = fn_layout,
        .params = lir_params,
        .body = lir_body,
        .ret_layout = ret_layout,
    } }, region);
}

fn lowerCall(self: *Self, call_data: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const fn_expr = try self.lowerExpr(call_data.func);
    const fn_mono = self.mir_store.typeOf(call_data.func);
    const fn_layout = try self.layoutFromMonotype(fn_mono);
    const ret_layout = try self.layoutFromMonotype(mono_idx);

    const mir_args = self.mir_store.getExprSpan(call_data.args);
    const lir_args = try self.lowerExprSpan(mir_args);

    return self.lir_store.addExpr(.{ .call = .{
        .fn_expr = fn_expr,
        .fn_layout = fn_layout,
        .args = lir_args,
        .ret_layout = ret_layout,
        .called_via = .apply,
    } }, region);
}

fn lowerBlock(self: *Self, block_data: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const result_layout = try self.layoutFromMonotype(mono_idx);

    const mir_stmts = self.mir_store.getStmts(block_data.stmts);
    const save_stmts_len = self.scratch_lir_stmts.items.len;
    defer self.scratch_lir_stmts.shrinkRetainingCapacity(save_stmts_len);
    for (mir_stmts) |stmt| {
        const binding = switch (stmt) {
            .decl_const, .decl_var, .mutate_var => |b| b,
        };
        const lir_pat = try self.lowerPattern(binding.pattern);
        const lir_expr = try self.lowerExpr(binding.expr);
        const lir_binding: LirStmt.Binding = .{ .pattern = lir_pat, .expr = lir_expr };
        try self.scratch_lir_stmts.append(self.allocator, switch (stmt) {
            .decl_const, .decl_var => .{ .decl = lir_binding },
            .mutate_var => .{ .mutate = lir_binding },
        });
    }

    const lir_stmts = try self.lir_store.addStmts(self.scratch_lir_stmts.items[save_stmts_len..]);
    const lir_final = try self.lowerExpr(block_data.final_expr);

    return self.lir_store.addExpr(.{ .block = .{
        .stmts = lir_stmts,
        .final_expr = lir_final,
        .result_layout = result_layout,
    } }, region);
}

fn lowerRecordAccess(self: *Self, ra: anytype, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const lir_record = try self.lowerExpr(ra.record);
    const record_mono = self.mir_store.typeOf(ra.record);
    const record_layout = try self.layoutFromMonotype(record_mono);
    const result_mono = self.mir_store.typeOf(mir_expr_id);
    const field_layout = try self.layoutFromMonotype(result_mono);

    // Find the field index from the layout's field list (alignment-sorted order),
    // not the monotype's field list (alphabetical order), so codegen accesses
    // the correct physical offset.
    var field_idx: ?u16 = null;
    const record_layout_val = self.layout_store.getLayout(record_layout);
    if (record_layout_val.tag == .record) {
        const record_data = self.layout_store.getRecordData(record_layout_val.data.record.idx);
        const layout_fields = self.layout_store.record_fields.sliceRange(record_data.getFields());
        for (0..layout_fields.len) |li| {
            if (@as(u32, @bitCast(layout_fields.get(li).name)) == @as(u32, @bitCast(ra.field_name))) {
                field_idx = @intCast(li);
                break;
            }
        }
    } else {
        // Single-field record optimized to non-record layout; field_idx 0
        field_idx = 0;
    }

    return self.lir_store.addExpr(.{ .field_access = .{
        .record_expr = lir_record,
        .record_layout = record_layout,
        .field_layout = field_layout,
        .field_idx = field_idx orelse unreachable,
        .field_name = ra.field_name,
    } }, region);
}

fn lowerTupleAccess(self: *Self, ta: anytype, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const lir_tuple = try self.lowerExpr(ta.tuple);
    const tuple_mono = self.mir_store.typeOf(ta.tuple);
    const tuple_layout = try self.layoutFromMonotype(tuple_mono);
    const result_mono = self.mir_store.typeOf(mir_expr_id);
    const elem_layout = try self.layoutFromMonotype(result_mono);

    return self.lir_store.addExpr(.{ .tuple_access = .{
        .tuple_expr = lir_tuple,
        .tuple_layout = tuple_layout,
        .elem_layout = elem_layout,
        .elem_idx = @intCast(ta.elem_index),
    } }, region);
}

fn lowerLowLevel(self: *Self, ll: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const ret_layout = try self.layoutFromMonotype(mono_idx);
    const mir_args = self.mir_store.getExprSpan(ll.args);
    const lir_args = try self.lowerExprSpan(mir_args);

    // Check if this maps to a .binop (comparisons, div_trunc, rem)
    if (lowLevelToBinop(ll.op)) |binop| {
        const args_slice = self.lir_store.getExprSpan(lir_args);
        const operand_layout = try self.layoutFromMonotype(self.mir_store.typeOf(mir_args[0]));
        return self.lir_store.addExpr(.{ .binop = .{
            .op = binop,
            .lhs = args_slice[0],
            .rhs = args_slice[1],
            .result_layout = ret_layout,
            .operand_layout = operand_layout,
        } }, region);
    }

    // num_is_negative/num_is_positive → comparison with 0
    if (ll.op == .num_is_negative or ll.op == .num_is_positive) {
        const args_slice = self.lir_store.getExprSpan(lir_args);
        const zero = try self.emitZeroLiteral(mir_args[0], region);
        const cmp_op: LirExpr.BinOp = if (ll.op == .num_is_negative) .lt else .gt;
        const operand_layout = try self.layoutFromMonotype(self.mir_store.typeOf(mir_args[0]));
        return self.lir_store.addExpr(.{ .binop = .{
            .op = cmp_op,
            .lhs = args_slice[0],
            .rhs = zero,
            .result_layout = ret_layout,
            .operand_layout = operand_layout,
        } }, region);
    }

    // num_negate → unary_minus expression
    if (ll.op == .num_negate) {
        const args_slice = self.lir_store.getExprSpan(lir_args);
        return self.lir_store.addExpr(.{ .unary_minus = .{
            .expr = args_slice[0],
            .result_layout = ret_layout,
        } }, region);
    }

    // num_is_zero → comparison with 0
    if (ll.op == .num_is_zero) {
        const args_slice = self.lir_store.getExprSpan(lir_args);
        const zero = try self.emitZeroLiteral(mir_args[0], region);
        const operand_layout = try self.layoutFromMonotype(self.mir_store.typeOf(mir_args[0]));
        return self.lir_store.addExpr(.{ .binop = .{
            .op = .eq,
            .lhs = args_slice[0],
            .rhs = zero,
            .result_layout = ret_layout,
            .operand_layout = operand_layout,
        } }, region);
    }

    const lir_op = mapLowLevel(ll.op) orelse {
        std.debug.panic("MirToLir: unmapped CIR low-level op: {s}", .{@tagName(ll.op)});
    };

    return self.lir_store.addExpr(.{ .low_level = .{
        .op = lir_op,
        .args = lir_args,
        .ret_layout = ret_layout,
    } }, region);
}

/// Emit a zero literal matching the type of the given MIR expression.
/// For float/dec/i128 types, emits the correct typed zero instead of i64.
fn emitZeroLiteral(self: *Self, mir_expr: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const arg_mono_idx = self.mir_store.typeOf(mir_expr);
    const arg_monotype = self.mir_store.monotype_store.getMonotype(arg_mono_idx);
    return self.lir_store.addExpr(switch (arg_monotype) {
        .prim => |p| switch (p) {
            .f32 => LirExpr{ .f32_literal = 0.0 },
            .f64 => LirExpr{ .f64_literal = 0.0 },
            .dec => LirExpr{ .dec_literal = 0 },
            .i128, .u128 => LirExpr{ .i128_literal = 0 },
            .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64 => LirExpr{ .i64_literal = 0 },
        },
        .func, .tag_union, .record, .tuple, .list, .box, .unit => unreachable,
    }, region);
}

fn lowLevelToBinop(op: CIR.Expr.LowLevel) ?LirExpr.BinOp {
    return switch (op) {
        // Arithmetic
        .num_plus => .add,
        .num_minus => .sub,
        .num_times => .mul,
        .num_div_by => .div,
        .num_div_trunc_by => .div_trunc,
        .num_rem_by => .rem,
        .num_mod_by => .mod,

        // Bitwise shifts
        .num_shift_left_by => .shl,
        .num_shift_right_by => .shr,
        .num_shift_right_zf_by => .shr_zf,

        // Comparison
        .num_is_eq, .bool_is_eq => .eq,
        .num_is_gt => .gt,
        .num_is_gte => .gte,
        .num_is_lt => .lt,
        .num_is_lte => .lte,

        // String operations
        .str_is_empty,
        .str_is_eq,
        .str_concat,
        .str_contains,
        .str_trim,
        .str_trim_start,
        .str_trim_end,
        .str_caseless_ascii_equals,
        .str_with_ascii_lowercased,
        .str_with_ascii_uppercased,
        .str_starts_with,
        .str_ends_with,
        .str_repeat,
        .str_with_prefix,
        .str_drop_prefix,
        .str_drop_suffix,
        .str_count_utf8_bytes,
        .str_with_capacity,
        .str_reserve,
        .str_release_excess_capacity,
        .str_to_utf8,
        .str_from_utf8_lossy,
        .str_from_utf8,
        .str_split_on,
        .str_join_with,
        .str_inspekt,
        // List operations
        .list_len,
        .list_is_empty,
        .list_get_unsafe,
        .list_append_unsafe,
        .list_concat,
        .list_with_capacity,
        .list_sort_with,
        .list_drop_at,
        .list_sublist,
        .list_append,
        // Numeric ops (handled as special cases or low_level, not binop)
        .num_abs,
        .num_from_numeral,
        .num_from_str,
        .num_abs_diff,
        .num_is_negative,
        .num_is_positive,
        .num_negate,
        .num_is_zero,
        // Numeric to_str operations
        .u8_to_str,
        .i8_to_str,
        .u16_to_str,
        .i16_to_str,
        .u32_to_str,
        .i32_to_str,
        .u64_to_str,
        .i64_to_str,
        .u128_to_str,
        .i128_to_str,
        .dec_to_str,
        .f32_to_str,
        .f64_to_str,
        // Type conversions — u8
        .u8_to_i8_wrap,
        .u8_to_i8_try,
        .u8_to_i16,
        .u8_to_i32,
        .u8_to_i64,
        .u8_to_i128,
        .u8_to_u16,
        .u8_to_u32,
        .u8_to_u64,
        .u8_to_u128,
        .u8_to_f32,
        .u8_to_f64,
        .u8_to_dec,
        // Type conversions — i8
        .i8_to_i16,
        .i8_to_i32,
        .i8_to_i64,
        .i8_to_i128,
        .i8_to_u8_wrap,
        .i8_to_u8_try,
        .i8_to_u16_wrap,
        .i8_to_u16_try,
        .i8_to_u32_wrap,
        .i8_to_u32_try,
        .i8_to_u64_wrap,
        .i8_to_u64_try,
        .i8_to_u128_wrap,
        .i8_to_u128_try,
        .i8_to_f32,
        .i8_to_f64,
        .i8_to_dec,
        // Type conversions — u16
        .u16_to_i8_wrap,
        .u16_to_i8_try,
        .u16_to_i16_wrap,
        .u16_to_i16_try,
        .u16_to_i32,
        .u16_to_i64,
        .u16_to_i128,
        .u16_to_u8_wrap,
        .u16_to_u8_try,
        .u16_to_u32,
        .u16_to_u64,
        .u16_to_u128,
        .u16_to_f32,
        .u16_to_f64,
        .u16_to_dec,
        // Type conversions — i16
        .i16_to_i8_wrap,
        .i16_to_i8_try,
        .i16_to_i32,
        .i16_to_i64,
        .i16_to_i128,
        .i16_to_u8_wrap,
        .i16_to_u8_try,
        .i16_to_u16_wrap,
        .i16_to_u16_try,
        .i16_to_u32_wrap,
        .i16_to_u32_try,
        .i16_to_u64_wrap,
        .i16_to_u64_try,
        .i16_to_u128_wrap,
        .i16_to_u128_try,
        .i16_to_f32,
        .i16_to_f64,
        .i16_to_dec,
        // Type conversions — u32
        .u32_to_i8_wrap,
        .u32_to_i8_try,
        .u32_to_i16_wrap,
        .u32_to_i16_try,
        .u32_to_i32_wrap,
        .u32_to_i32_try,
        .u32_to_i64,
        .u32_to_i128,
        .u32_to_u8_wrap,
        .u32_to_u8_try,
        .u32_to_u16_wrap,
        .u32_to_u16_try,
        .u32_to_u64,
        .u32_to_u128,
        .u32_to_f32,
        .u32_to_f64,
        .u32_to_dec,
        // Type conversions — i32
        .i32_to_i8_wrap,
        .i32_to_i8_try,
        .i32_to_i16_wrap,
        .i32_to_i16_try,
        .i32_to_i64,
        .i32_to_i128,
        .i32_to_u8_wrap,
        .i32_to_u8_try,
        .i32_to_u16_wrap,
        .i32_to_u16_try,
        .i32_to_u32_wrap,
        .i32_to_u32_try,
        .i32_to_u64_wrap,
        .i32_to_u64_try,
        .i32_to_u128_wrap,
        .i32_to_u128_try,
        .i32_to_f32,
        .i32_to_f64,
        .i32_to_dec,
        // Type conversions — u64
        .u64_to_i8_wrap,
        .u64_to_i8_try,
        .u64_to_i16_wrap,
        .u64_to_i16_try,
        .u64_to_i32_wrap,
        .u64_to_i32_try,
        .u64_to_i64_wrap,
        .u64_to_i64_try,
        .u64_to_i128,
        .u64_to_u8_wrap,
        .u64_to_u8_try,
        .u64_to_u16_wrap,
        .u64_to_u16_try,
        .u64_to_u32_wrap,
        .u64_to_u32_try,
        .u64_to_u128,
        .u64_to_f32,
        .u64_to_f64,
        .u64_to_dec,
        // Type conversions — i64
        .i64_to_i8_wrap,
        .i64_to_i8_try,
        .i64_to_i16_wrap,
        .i64_to_i16_try,
        .i64_to_i32_wrap,
        .i64_to_i32_try,
        .i64_to_i128,
        .i64_to_u8_wrap,
        .i64_to_u8_try,
        .i64_to_u16_wrap,
        .i64_to_u16_try,
        .i64_to_u32_wrap,
        .i64_to_u32_try,
        .i64_to_u64_wrap,
        .i64_to_u64_try,
        .i64_to_u128_wrap,
        .i64_to_u128_try,
        .i64_to_f32,
        .i64_to_f64,
        .i64_to_dec,
        // Type conversions — u128
        .u128_to_i8_wrap,
        .u128_to_i8_try,
        .u128_to_i16_wrap,
        .u128_to_i16_try,
        .u128_to_i32_wrap,
        .u128_to_i32_try,
        .u128_to_i64_wrap,
        .u128_to_i64_try,
        .u128_to_i128_wrap,
        .u128_to_i128_try,
        .u128_to_u8_wrap,
        .u128_to_u8_try,
        .u128_to_u16_wrap,
        .u128_to_u16_try,
        .u128_to_u32_wrap,
        .u128_to_u32_try,
        .u128_to_u64_wrap,
        .u128_to_u64_try,
        .u128_to_f32,
        .u128_to_f64,
        .u128_to_dec_try_unsafe,
        // Type conversions — i128
        .i128_to_i8_wrap,
        .i128_to_i8_try,
        .i128_to_i16_wrap,
        .i128_to_i16_try,
        .i128_to_i32_wrap,
        .i128_to_i32_try,
        .i128_to_i64_wrap,
        .i128_to_i64_try,
        .i128_to_u8_wrap,
        .i128_to_u8_try,
        .i128_to_u16_wrap,
        .i128_to_u16_try,
        .i128_to_u32_wrap,
        .i128_to_u32_try,
        .i128_to_u64_wrap,
        .i128_to_u64_try,
        .i128_to_u128_wrap,
        .i128_to_u128_try,
        .i128_to_f32,
        .i128_to_f64,
        .i128_to_dec_try_unsafe,
        // Type conversions — f32
        .f32_to_i8_trunc,
        .f32_to_i8_try_unsafe,
        .f32_to_i16_trunc,
        .f32_to_i16_try_unsafe,
        .f32_to_i32_trunc,
        .f32_to_i32_try_unsafe,
        .f32_to_i64_trunc,
        .f32_to_i64_try_unsafe,
        .f32_to_i128_trunc,
        .f32_to_i128_try_unsafe,
        .f32_to_u8_trunc,
        .f32_to_u8_try_unsafe,
        .f32_to_u16_trunc,
        .f32_to_u16_try_unsafe,
        .f32_to_u32_trunc,
        .f32_to_u32_try_unsafe,
        .f32_to_u64_trunc,
        .f32_to_u64_try_unsafe,
        .f32_to_u128_trunc,
        .f32_to_u128_try_unsafe,
        .f32_to_f64,
        // Type conversions — f64
        .f64_to_i8_trunc,
        .f64_to_i8_try_unsafe,
        .f64_to_i16_trunc,
        .f64_to_i16_try_unsafe,
        .f64_to_i32_trunc,
        .f64_to_i32_try_unsafe,
        .f64_to_i64_trunc,
        .f64_to_i64_try_unsafe,
        .f64_to_i128_trunc,
        .f64_to_i128_try_unsafe,
        .f64_to_u8_trunc,
        .f64_to_u8_try_unsafe,
        .f64_to_u16_trunc,
        .f64_to_u16_try_unsafe,
        .f64_to_u32_trunc,
        .f64_to_u32_try_unsafe,
        .f64_to_u64_trunc,
        .f64_to_u64_try_unsafe,
        .f64_to_u128_trunc,
        .f64_to_u128_try_unsafe,
        .f64_to_f32_wrap,
        .f64_to_f32_try_unsafe,
        // Type conversions — dec
        .dec_to_i8_trunc,
        .dec_to_i8_try_unsafe,
        .dec_to_i16_trunc,
        .dec_to_i16_try_unsafe,
        .dec_to_i32_trunc,
        .dec_to_i32_try_unsafe,
        .dec_to_i64_trunc,
        .dec_to_i64_try_unsafe,
        .dec_to_i128_trunc,
        .dec_to_i128_try_unsafe,
        .dec_to_u8_trunc,
        .dec_to_u8_try_unsafe,
        .dec_to_u16_trunc,
        .dec_to_u16_try_unsafe,
        .dec_to_u32_trunc,
        .dec_to_u32_try_unsafe,
        .dec_to_u64_trunc,
        .dec_to_u64_try_unsafe,
        .dec_to_u128_trunc,
        .dec_to_u128_try_unsafe,
        .dec_to_f32_wrap,
        .dec_to_f32_try_unsafe,
        .dec_to_f64,
        => null,
    };
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
        else => unreachable, // Hosted expressions always have .func monotype
    };

    // Lower parameter patterns
    const mir_params = self.mir_store.getPatternSpan(h.params);
    const lir_params = try self.lowerPatternSpan(mir_params);

    // Build lookup args from parameters (one lookup per param)
    const func_args = switch (monotype) {
        .func => |f| self.mir_store.monotype_store.getIdxSpan(f.args),
        else => unreachable,
    };

    const save_len = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_len);

    for (mir_params, 0..) |mir_param_id, i| {
        const mir_pat = self.mir_store.getPattern(mir_param_id);
        const symbol = switch (mir_pat) {
            .bind => |sym| sym,
            else => unreachable, // hosted params should always be simple binds
        };
        const param_layout = if (i < func_args.len)
            try self.layoutFromMonotype(func_args[i])
        else
            layout.Idx.zst;

        const lookup = try self.lir_store.addExpr(.{ .lookup = .{
            .symbol = symbol,
            .layout_idx = param_layout,
        } }, region);
        try self.scratch_lir_expr_ids.append(self.allocator, lookup);
    }
    const lir_args = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_len..]);

    // Create hosted_call as the lambda body
    const hosted_call = try self.lir_store.addExpr(.{ .hosted_call = .{
        .index = h.index,
        .args = lir_args,
        .ret_layout = ret_layout,
    } }, region);

    // Wrap in lambda (hosted lambdas are function values)
    return self.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = fn_layout,
        .params = lir_params,
        .body = hosted_call,
        .ret_layout = ret_layout,
    } }, region);
}

fn lowerDbg(self: *Self, d: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const result_layout = try self.layoutFromMonotype(mono_idx);
    const lir_expr = try self.lowerExpr(d.expr);

    return self.lir_store.addExpr(.{ .dbg = .{
        .msg = StringLiteral.Idx.none,
        .expr = lir_expr,
        .result_layout = result_layout,
    } }, region);
}

fn lowerExpect(self: *Self, e: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const result_layout = try self.layoutFromMonotype(mono_idx);
    const lir_cond = try self.lowerExpr(e.body);

    // The MIR expect body is the boolean condition to assert.
    // After the assertion, the result is empty_record (unit).
    const lir_body = try self.lir_store.addExpr(.{ .empty_record = {} }, region);

    return self.lir_store.addExpr(.{ .expect = .{
        .cond = lir_cond,
        .body = lir_body,
        .result_layout = result_layout,
    } }, region);
}

fn lowerForLoop(self: *Self, f: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    std.debug.assert(mono_idx == self.mir_store.monotype_store.unit_idx);
    const lir_list = try self.lowerExpr(f.list);
    const list_mono = self.mir_store.typeOf(f.list);
    const list_monotype = self.mir_store.monotype_store.getMonotype(list_mono);
    const elem_layout = switch (list_monotype) {
        .list => |l| try self.layoutFromMonotype(l.elem),
        else => unreachable,
    };
    const lir_pat = try self.lowerPattern(f.elem_pattern);
    const lir_body = try self.lowerExpr(f.body);

    return self.lir_store.addExpr(.{ .for_loop = .{
        .list_expr = lir_list,
        .elem_layout = elem_layout,
        .elem_pattern = lir_pat,
        .body = lir_body,
    } }, region);
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

fn lowerReturn(self: *Self, r: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const ret_layout = try self.layoutFromMonotype(mono_idx);
    const lir_expr = try self.lowerExpr(r.expr);

    return self.lir_store.addExpr(.{ .early_return = .{
        .expr = lir_expr,
        .ret_layout = ret_layout,
    } }, region);
}

fn lowerPattern(self: *Self, mir_pat_id: MIR.PatternId) Allocator.Error!LirPatternId {
    const pat = self.mir_store.getPattern(mir_pat_id);
    const mono_idx = self.mir_store.patternTypeOf(mir_pat_id);
    const region = Region.zero(); // MIR patterns don't carry regions separately

    return switch (pat) {
        .bind => |sym| blk: {
            const layout_idx = try self.layoutFromMonotype(mono_idx);
            break :blk self.lir_store.addPattern(.{ .bind = .{ .symbol = sym, .layout_idx = layout_idx } }, region);
        },
        .wildcard => blk: {
            const layout_idx = try self.layoutFromMonotype(mono_idx);
            break :blk self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = layout_idx } }, region);
        },
        .tag => |t| blk: {
            const mir_pat_args = self.mir_store.getPatternSpan(t.args);

            // Single-tag unions are optimized to just their payload layout,
            // so emit a payload pattern directly instead of a .tag pattern.
            if (self.isSingleTagUnion(mono_idx)) {
                if (mir_pat_args.len == 0) {
                    const layout_idx = try self.layoutFromMonotype(mono_idx);
                    break :blk self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = layout_idx } }, region);
                } else if (mir_pat_args.len == 1) {
                    break :blk self.lowerPattern(mir_pat_args[0]);
                } else {
                    const tuple_layout = try self.layoutFromMonotype(mono_idx);
                    const lir_elems = try self.lowerPatternSpan(mir_pat_args);
                    break :blk self.lir_store.addPattern(.{ .tuple = .{
                        .tuple_layout = tuple_layout,
                        .elems = lir_elems,
                    } }, region);
                }
            }

            const union_layout = try self.layoutFromMonotype(mono_idx);
            const discriminant = self.tagDiscriminant(t.name, mono_idx);
            const lir_args = try self.lowerPatternSpan(mir_pat_args);
            break :blk self.lir_store.addPattern(.{ .tag = .{
                .discriminant = discriminant,
                .union_layout = union_layout,
                .args = lir_args,
            } }, region);
        },
        .int_literal => |i| blk: {
            const layout_idx = try self.layoutFromMonotype(mono_idx);
            break :blk self.lir_store.addPattern(.{ .int_literal = .{
                .value = i.value.toI128(),
                .layout_idx = layout_idx,
            } }, region);
        },
        .str_literal => |s| self.lir_store.addPattern(.{ .str_literal = s }, region),
        .dec_literal => |d| blk: {
            const layout_idx = try self.layoutFromMonotype(mono_idx);
            // Represent decimal pattern as an int literal with Dec layout
            break :blk self.lir_store.addPattern(.{ .int_literal = .{
                .value = d.num,
                .layout_idx = layout_idx,
            } }, region);
        },
        .frac_f32_literal => |v| blk: {
            const layout_idx = try self.layoutFromMonotype(mono_idx);
            break :blk self.lir_store.addPattern(.{ .float_literal = .{
                .value = @floatCast(v),
                .layout_idx = layout_idx,
            } }, region);
        },
        .frac_f64_literal => |v| blk: {
            const layout_idx = try self.layoutFromMonotype(mono_idx);
            break :blk self.lir_store.addPattern(.{ .float_literal = .{
                .value = v,
                .layout_idx = layout_idx,
            } }, region);
        },
        .record_destructure => |rd| blk: {
            const record_layout = try self.layoutFromMonotype(mono_idx);

            // MIR destructs are in alphabetical (field-name) order, but the layout
            // store sorts fields by alignment. Reorder patterns to match layout order
            // so codegen can use positional field indices.
            const mir_patterns = self.mir_store.getPatternSpan(rd.destructs);
            const mir_field_names = self.mir_store.getFieldNameSpan(rd.field_names);
            const record_layout_val = self.layout_store.getLayout(record_layout);

            if (record_layout_val.tag == .record) {
                const record_data = self.layout_store.getRecordData(record_layout_val.data.record.idx);
                const layout_fields = self.layout_store.record_fields.sliceRange(record_data.getFields());

                // For each layout field (in alignment-sorted order), find the matching
                // MIR pattern by field name and lower it
                const save_len = self.scratch_lir_pattern_ids.items.len;
                defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);

                for (0..layout_fields.len) |li| {
                    const layout_field_name = layout_fields.get(li).name;
                    // Find the MIR pattern with this field name
                    var found = false;
                    for (mir_field_names, 0..) |mir_name, mi| {
                        if (@as(u32, @bitCast(mir_name)) == @as(u32, @bitCast(layout_field_name))) {
                            const lir_pat = try self.lowerPattern(mir_patterns[mi]);
                            try self.scratch_lir_pattern_ids.append(self.allocator, lir_pat);
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        // Field exists in layout but not in destructure (wildcard) —
                        // use the actual field layout so codegen skips the right number of bytes.
                        const field_layout_idx = layout_fields.get(li).layout;
                        const lir_pat = try self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = field_layout_idx } }, region);
                        try self.scratch_lir_pattern_ids.append(self.allocator, lir_pat);
                    }
                }

                const lir_fields = try self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_len..]);
                break :blk self.lir_store.addPattern(.{ .record = .{
                    .record_layout = record_layout,
                    .fields = lir_fields,
                } }, region);
            } else {
                // Non-record layout (e.g. ZST) — just lower patterns directly
                const lir_fields = try self.lowerPatternSpan(mir_patterns);
                break :blk self.lir_store.addPattern(.{ .record = .{
                    .record_layout = record_layout,
                    .fields = lir_fields,
                } }, region);
            }
        },
        .tuple_destructure => |td| blk: {
            const tuple_layout = try self.layoutFromMonotype(mono_idx);
            const lir_elems = try self.lowerPatternSpan(self.mir_store.getPatternSpan(td.elems));
            break :blk self.lir_store.addPattern(.{ .tuple = .{
                .tuple_layout = tuple_layout,
                .elems = lir_elems,
            } }, region);
        },
        .list_destructure => |ld| blk: {
            const list_monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
            const elem_layout = switch (list_monotype) {
                .list => |l| try self.layoutFromMonotype(l.elem),
                else => unreachable,
            };
            const all_patterns = self.mir_store.getPatternSpan(ld.patterns);
            const rest_pat = if (ld.rest_pattern.isNone())
                LirPatternId.none
            else
                try self.lowerPattern(ld.rest_pattern);

            // Split patterns into prefix (before ..) and suffix (after ..)
            if (ld.rest_index.isNone()) {
                // No rest pattern: all patterns are prefix
                const lir_prefix = try self.lowerPatternSpan(all_patterns);
                break :blk self.lir_store.addPattern(.{ .list = .{
                    .elem_layout = elem_layout,
                    .prefix = lir_prefix,
                    .rest = rest_pat,
                    .suffix = LirPatternSpan.empty(),
                } }, region);
            } else {
                const rest_idx: u32 = @intFromEnum(ld.rest_index);
                const lir_prefix = try self.lowerPatternSpan(all_patterns[0..rest_idx]);
                const lir_suffix = try self.lowerPatternSpan(all_patterns[rest_idx..]);
                break :blk self.lir_store.addPattern(.{ .list = .{
                    .elem_layout = elem_layout,
                    .prefix = lir_prefix,
                    .rest = rest_pat,
                    .suffix = lir_suffix,
                } }, region);
            }
        },
        .as_pattern => |ap| blk: {
            const layout_idx = try self.layoutFromMonotype(mono_idx);
            const inner = try self.lowerPattern(ap.pattern);
            break :blk self.lir_store.addPattern(.{ .as_pattern = .{
                .symbol = ap.symbol,
                .layout_idx = layout_idx,
                .inner = inner,
            } }, region);
        },
        .runtime_error => self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, region),
    };
}

fn lowerExprSpan(self: *Self, mir_expr_ids: []const MIR.ExprId) Allocator.Error!LirExprSpan {
    const save_len = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_len);
    for (mir_expr_ids) |mir_id| {
        const lir_id = try self.lowerExpr(mir_id);
        try self.scratch_lir_expr_ids.append(self.allocator, lir_id);
    }
    return self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_len..]);
}

fn lowerPatternSpan(self: *Self, mir_pat_ids: []const MIR.PatternId) Allocator.Error!LirPatternSpan {
    const save_len = self.scratch_lir_pattern_ids.items.len;
    defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);
    for (mir_pat_ids) |mir_id| {
        const lir_id = try self.lowerPattern(mir_id);
        try self.scratch_lir_pattern_ids.append(self.allocator, lir_id);
    }
    return self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_len..]);
}

fn mapLowLevel(cir_op: CIR.Expr.LowLevel) ?LirExpr.LowLevel {
    return switch (cir_op) {
        // String operations (same name in both CIR and LIR)
        .str_is_empty => .str_is_empty,
        .str_is_eq => .str_is_eq,
        .str_concat => .str_concat,
        .str_contains => .str_contains,
        .str_trim => .str_trim,
        .str_trim_start => .str_trim_start,
        .str_trim_end => .str_trim_end,
        .str_caseless_ascii_equals => .str_caseless_ascii_equals,
        .str_with_ascii_lowercased => .str_with_ascii_lowercased,
        .str_with_ascii_uppercased => .str_with_ascii_uppercased,
        .str_starts_with => .str_starts_with,
        .str_ends_with => .str_ends_with,
        .str_repeat => .str_repeat,
        .str_with_prefix => .str_with_prefix,
        .str_drop_prefix => .str_drop_prefix,
        .str_drop_suffix => .str_drop_suffix,
        .str_count_utf8_bytes => .str_count_utf8_bytes,
        .str_with_capacity => .str_with_capacity,
        .str_reserve => .str_reserve,
        .str_release_excess_capacity => .str_release_excess_capacity,
        .str_to_utf8 => .str_to_utf8,
        .str_from_utf8_lossy => .str_from_utf8_lossy,
        .str_from_utf8 => .str_from_utf8,
        .str_join_with => .str_join_with,
        .str_split_on => .str_split,
        .str_inspekt => .str_inspekt,

        // List operations
        .list_len => .list_len,
        .list_is_empty => .list_is_empty,
        .list_get_unsafe => .list_get,
        .list_append_unsafe => .list_append,
        .list_append => .list_append,
        .list_concat => .list_concat,
        .list_with_capacity => .list_with_capacity,
        .list_sort_with => .list_sort_with,
        .list_drop_at => .list_drop_at,
        .list_sublist => .list_sublist,

        // Numeric operations still routed through LIR LowLevel
        // (num_plus/minus/times/div_by/mod_by/shifts/negate/is_zero are now handled
        // by lowLevelToBinop or special cases in lowerLowLevel)
        .num_abs => .num_abs,
        .num_from_numeral => .num_from_numeral,
        .num_from_str => .num_from_str,
        .num_abs_diff => .num_abs_diff,

        // Type conversions (same name in both CIR and LIR)
        .u8_to_i8_wrap => .u8_to_i8_wrap,
        .u8_to_i8_try => .u8_to_i8_try,
        .u8_to_i16 => .u8_to_i16,
        .u8_to_i32 => .u8_to_i32,
        .u8_to_i64 => .u8_to_i64,
        .u8_to_i128 => .u8_to_i128,
        .u8_to_u16 => .u8_to_u16,
        .u8_to_u32 => .u8_to_u32,
        .u8_to_u64 => .u8_to_u64,
        .u8_to_u128 => .u8_to_u128,
        .u8_to_f32 => .u8_to_f32,
        .u8_to_f64 => .u8_to_f64,
        .u8_to_dec => .u8_to_dec,
        .i8_to_i16 => .i8_to_i16,
        .i8_to_i32 => .i8_to_i32,
        .i8_to_i64 => .i8_to_i64,
        .i8_to_i128 => .i8_to_i128,
        .i8_to_u8_wrap => .i8_to_u8_wrap,
        .i8_to_u8_try => .i8_to_u8_try,
        .i8_to_u16_wrap => .i8_to_u16_wrap,
        .i8_to_u16_try => .i8_to_u16_try,
        .i8_to_u32_wrap => .i8_to_u32_wrap,
        .i8_to_u32_try => .i8_to_u32_try,
        .i8_to_u64_wrap => .i8_to_u64_wrap,
        .i8_to_u64_try => .i8_to_u64_try,
        .i8_to_u128_wrap => .i8_to_u128_wrap,
        .i8_to_u128_try => .i8_to_u128_try,
        .i8_to_f32 => .i8_to_f32,
        .i8_to_f64 => .i8_to_f64,
        .i8_to_dec => .i8_to_dec,
        .u16_to_i8_wrap => .u16_to_i8_wrap,
        .u16_to_i8_try => .u16_to_i8_try,
        .u16_to_i16_wrap => .u16_to_i16_wrap,
        .u16_to_i16_try => .u16_to_i16_try,
        .u16_to_i32 => .u16_to_i32,
        .u16_to_i64 => .u16_to_i64,
        .u16_to_i128 => .u16_to_i128,
        .u16_to_u8_wrap => .u16_to_u8_wrap,
        .u16_to_u8_try => .u16_to_u8_try,
        .u16_to_u32 => .u16_to_u32,
        .u16_to_u64 => .u16_to_u64,
        .u16_to_u128 => .u16_to_u128,
        .u16_to_f32 => .u16_to_f32,
        .u16_to_f64 => .u16_to_f64,
        .u16_to_dec => .u16_to_dec,
        .i16_to_i8_wrap => .i16_to_i8_wrap,
        .i16_to_i8_try => .i16_to_i8_try,
        .i16_to_i32 => .i16_to_i32,
        .i16_to_i64 => .i16_to_i64,
        .i16_to_i128 => .i16_to_i128,
        .i16_to_u8_wrap => .i16_to_u8_wrap,
        .i16_to_u8_try => .i16_to_u8_try,
        .i16_to_u16_wrap => .i16_to_u16_wrap,
        .i16_to_u16_try => .i16_to_u16_try,
        .i16_to_u32_wrap => .i16_to_u32_wrap,
        .i16_to_u32_try => .i16_to_u32_try,
        .i16_to_u64_wrap => .i16_to_u64_wrap,
        .i16_to_u64_try => .i16_to_u64_try,
        .i16_to_u128_wrap => .i16_to_u128_wrap,
        .i16_to_u128_try => .i16_to_u128_try,
        .i16_to_f32 => .i16_to_f32,
        .i16_to_f64 => .i16_to_f64,
        .i16_to_dec => .i16_to_dec,
        .u32_to_i8_wrap => .u32_to_i8_wrap,
        .u32_to_i8_try => .u32_to_i8_try,
        .u32_to_i16_wrap => .u32_to_i16_wrap,
        .u32_to_i16_try => .u32_to_i16_try,
        .u32_to_i32_wrap => .u32_to_i32_wrap,
        .u32_to_i32_try => .u32_to_i32_try,
        .u32_to_i64 => .u32_to_i64,
        .u32_to_i128 => .u32_to_i128,
        .u32_to_u8_wrap => .u32_to_u8_wrap,
        .u32_to_u8_try => .u32_to_u8_try,
        .u32_to_u16_wrap => .u32_to_u16_wrap,
        .u32_to_u16_try => .u32_to_u16_try,
        .u32_to_u64 => .u32_to_u64,
        .u32_to_u128 => .u32_to_u128,
        .u32_to_f32 => .u32_to_f32,
        .u32_to_f64 => .u32_to_f64,
        .u32_to_dec => .u32_to_dec,
        .i32_to_i8_wrap => .i32_to_i8_wrap,
        .i32_to_i8_try => .i32_to_i8_try,
        .i32_to_i16_wrap => .i32_to_i16_wrap,
        .i32_to_i16_try => .i32_to_i16_try,
        .i32_to_i64 => .i32_to_i64,
        .i32_to_i128 => .i32_to_i128,
        .i32_to_u8_wrap => .i32_to_u8_wrap,
        .i32_to_u8_try => .i32_to_u8_try,
        .i32_to_u16_wrap => .i32_to_u16_wrap,
        .i32_to_u16_try => .i32_to_u16_try,
        .i32_to_u32_wrap => .i32_to_u32_wrap,
        .i32_to_u32_try => .i32_to_u32_try,
        .i32_to_u64_wrap => .i32_to_u64_wrap,
        .i32_to_u64_try => .i32_to_u64_try,
        .i32_to_u128_wrap => .i32_to_u128_wrap,
        .i32_to_u128_try => .i32_to_u128_try,
        .i32_to_f32 => .i32_to_f32,
        .i32_to_f64 => .i32_to_f64,
        .i32_to_dec => .i32_to_dec,
        .u64_to_i8_wrap => .u64_to_i8_wrap,
        .u64_to_i8_try => .u64_to_i8_try,
        .u64_to_i16_wrap => .u64_to_i16_wrap,
        .u64_to_i16_try => .u64_to_i16_try,
        .u64_to_i32_wrap => .u64_to_i32_wrap,
        .u64_to_i32_try => .u64_to_i32_try,
        .u64_to_i64_wrap => .u64_to_i64_wrap,
        .u64_to_i64_try => .u64_to_i64_try,
        .u64_to_i128 => .u64_to_i128,
        .u64_to_u8_wrap => .u64_to_u8_wrap,
        .u64_to_u8_try => .u64_to_u8_try,
        .u64_to_u16_wrap => .u64_to_u16_wrap,
        .u64_to_u16_try => .u64_to_u16_try,
        .u64_to_u32_wrap => .u64_to_u32_wrap,
        .u64_to_u32_try => .u64_to_u32_try,
        .u64_to_u128 => .u64_to_u128,
        .u64_to_f32 => .u64_to_f32,
        .u64_to_f64 => .u64_to_f64,
        .u64_to_dec => .u64_to_dec,
        .i64_to_i8_wrap => .i64_to_i8_wrap,
        .i64_to_i8_try => .i64_to_i8_try,
        .i64_to_i16_wrap => .i64_to_i16_wrap,
        .i64_to_i16_try => .i64_to_i16_try,
        .i64_to_i32_wrap => .i64_to_i32_wrap,
        .i64_to_i32_try => .i64_to_i32_try,
        .i64_to_i128 => .i64_to_i128,
        .i64_to_u8_wrap => .i64_to_u8_wrap,
        .i64_to_u8_try => .i64_to_u8_try,
        .i64_to_u16_wrap => .i64_to_u16_wrap,
        .i64_to_u16_try => .i64_to_u16_try,
        .i64_to_u32_wrap => .i64_to_u32_wrap,
        .i64_to_u32_try => .i64_to_u32_try,
        .i64_to_u64_wrap => .i64_to_u64_wrap,
        .i64_to_u64_try => .i64_to_u64_try,
        .i64_to_u128_wrap => .i64_to_u128_wrap,
        .i64_to_u128_try => .i64_to_u128_try,
        .i64_to_f32 => .i64_to_f32,
        .i64_to_f64 => .i64_to_f64,
        .i64_to_dec => .i64_to_dec,
        .u128_to_i8_wrap => .u128_to_i8_wrap,
        .u128_to_i8_try => .u128_to_i8_try,
        .u128_to_i16_wrap => .u128_to_i16_wrap,
        .u128_to_i16_try => .u128_to_i16_try,
        .u128_to_i32_wrap => .u128_to_i32_wrap,
        .u128_to_i32_try => .u128_to_i32_try,
        .u128_to_i64_wrap => .u128_to_i64_wrap,
        .u128_to_i64_try => .u128_to_i64_try,
        .u128_to_i128_wrap => .u128_to_i128_wrap,
        .u128_to_i128_try => .u128_to_i128_try,
        .u128_to_u8_wrap => .u128_to_u8_wrap,
        .u128_to_u8_try => .u128_to_u8_try,
        .u128_to_u16_wrap => .u128_to_u16_wrap,
        .u128_to_u16_try => .u128_to_u16_try,
        .u128_to_u32_wrap => .u128_to_u32_wrap,
        .u128_to_u32_try => .u128_to_u32_try,
        .u128_to_u64_wrap => .u128_to_u64_wrap,
        .u128_to_u64_try => .u128_to_u64_try,
        .u128_to_f32 => .u128_to_f32,
        .u128_to_f64 => .u128_to_f64,
        .u128_to_dec_try_unsafe => .u128_to_dec_try_unsafe,
        .i128_to_i8_wrap => .i128_to_i8_wrap,
        .i128_to_i8_try => .i128_to_i8_try,
        .i128_to_i16_wrap => .i128_to_i16_wrap,
        .i128_to_i16_try => .i128_to_i16_try,
        .i128_to_i32_wrap => .i128_to_i32_wrap,
        .i128_to_i32_try => .i128_to_i32_try,
        .i128_to_i64_wrap => .i128_to_i64_wrap,
        .i128_to_i64_try => .i128_to_i64_try,
        .i128_to_u8_wrap => .i128_to_u8_wrap,
        .i128_to_u8_try => .i128_to_u8_try,
        .i128_to_u16_wrap => .i128_to_u16_wrap,
        .i128_to_u16_try => .i128_to_u16_try,
        .i128_to_u32_wrap => .i128_to_u32_wrap,
        .i128_to_u32_try => .i128_to_u32_try,
        .i128_to_u64_wrap => .i128_to_u64_wrap,
        .i128_to_u64_try => .i128_to_u64_try,
        .i128_to_u128_wrap => .i128_to_u128_wrap,
        .i128_to_u128_try => .i128_to_u128_try,
        .i128_to_f32 => .i128_to_f32,
        .i128_to_f64 => .i128_to_f64,
        .i128_to_dec_try_unsafe => .i128_to_dec_try_unsafe,
        .f32_to_i8_trunc => .f32_to_i8_trunc,
        .f32_to_i8_try_unsafe => .f32_to_i8_try_unsafe,
        .f32_to_i16_trunc => .f32_to_i16_trunc,
        .f32_to_i16_try_unsafe => .f32_to_i16_try_unsafe,
        .f32_to_i32_trunc => .f32_to_i32_trunc,
        .f32_to_i32_try_unsafe => .f32_to_i32_try_unsafe,
        .f32_to_i64_trunc => .f32_to_i64_trunc,
        .f32_to_i64_try_unsafe => .f32_to_i64_try_unsafe,
        .f32_to_i128_trunc => .f32_to_i128_trunc,
        .f32_to_i128_try_unsafe => .f32_to_i128_try_unsafe,
        .f32_to_u8_trunc => .f32_to_u8_trunc,
        .f32_to_u8_try_unsafe => .f32_to_u8_try_unsafe,
        .f32_to_u16_trunc => .f32_to_u16_trunc,
        .f32_to_u16_try_unsafe => .f32_to_u16_try_unsafe,
        .f32_to_u32_trunc => .f32_to_u32_trunc,
        .f32_to_u32_try_unsafe => .f32_to_u32_try_unsafe,
        .f32_to_u64_trunc => .f32_to_u64_trunc,
        .f32_to_u64_try_unsafe => .f32_to_u64_try_unsafe,
        .f32_to_u128_trunc => .f32_to_u128_trunc,
        .f32_to_u128_try_unsafe => .f32_to_u128_try_unsafe,
        .f32_to_f64 => .f32_to_f64,
        .f64_to_i8_trunc => .f64_to_i8_trunc,
        .f64_to_i8_try_unsafe => .f64_to_i8_try_unsafe,
        .f64_to_i16_trunc => .f64_to_i16_trunc,
        .f64_to_i16_try_unsafe => .f64_to_i16_try_unsafe,
        .f64_to_i32_trunc => .f64_to_i32_trunc,
        .f64_to_i32_try_unsafe => .f64_to_i32_try_unsafe,
        .f64_to_i64_trunc => .f64_to_i64_trunc,
        .f64_to_i64_try_unsafe => .f64_to_i64_try_unsafe,
        .f64_to_i128_trunc => .f64_to_i128_trunc,
        .f64_to_i128_try_unsafe => .f64_to_i128_try_unsafe,
        .f64_to_u8_trunc => .f64_to_u8_trunc,
        .f64_to_u8_try_unsafe => .f64_to_u8_try_unsafe,
        .f64_to_u16_trunc => .f64_to_u16_trunc,
        .f64_to_u16_try_unsafe => .f64_to_u16_try_unsafe,
        .f64_to_u32_trunc => .f64_to_u32_trunc,
        .f64_to_u32_try_unsafe => .f64_to_u32_try_unsafe,
        .f64_to_u64_trunc => .f64_to_u64_trunc,
        .f64_to_u64_try_unsafe => .f64_to_u64_try_unsafe,
        .f64_to_u128_trunc => .f64_to_u128_trunc,
        .f64_to_u128_try_unsafe => .f64_to_u128_try_unsafe,
        .f64_to_f32_wrap => .f64_to_f32_wrap,
        .f64_to_f32_try_unsafe => .f64_to_f32_try_unsafe,
        .dec_to_i8_trunc => .dec_to_i8_trunc,
        .dec_to_i8_try_unsafe => .dec_to_i8_try_unsafe,
        .dec_to_i16_trunc => .dec_to_i16_trunc,
        .dec_to_i16_try_unsafe => .dec_to_i16_try_unsafe,
        .dec_to_i32_trunc => .dec_to_i32_trunc,
        .dec_to_i32_try_unsafe => .dec_to_i32_try_unsafe,
        .dec_to_i64_trunc => .dec_to_i64_trunc,
        .dec_to_i64_try_unsafe => .dec_to_i64_try_unsafe,
        .dec_to_i128_trunc => .dec_to_i128_trunc,
        .dec_to_i128_try_unsafe => .dec_to_i128_try_unsafe,
        .dec_to_u8_trunc => .dec_to_u8_trunc,
        .dec_to_u8_try_unsafe => .dec_to_u8_try_unsafe,
        .dec_to_u16_trunc => .dec_to_u16_trunc,
        .dec_to_u16_try_unsafe => .dec_to_u16_try_unsafe,
        .dec_to_u32_trunc => .dec_to_u32_trunc,
        .dec_to_u32_try_unsafe => .dec_to_u32_try_unsafe,
        .dec_to_u64_trunc => .dec_to_u64_trunc,
        .dec_to_u64_try_unsafe => .dec_to_u64_try_unsafe,
        .dec_to_u128_trunc => .dec_to_u128_trunc,
        .dec_to_u128_try_unsafe => .dec_to_u128_try_unsafe,
        .dec_to_f32_wrap => .dec_to_f32_wrap,
        .dec_to_f32_try_unsafe => .dec_to_f32_try_unsafe,
        .dec_to_f64 => .dec_to_f64,

        // Numeric to_str operations
        .u8_to_str,
        .i8_to_str,
        .u16_to_str,
        .i16_to_str,
        .u32_to_str,
        .i32_to_str,
        .u64_to_str,
        .i64_to_str,
        .u128_to_str,
        .i128_to_str,
        .dec_to_str,
        .f32_to_str,
        .f64_to_str,
        => .num_to_str,

        // Ops handled by lowLevelToBinop (emitted as .binop)
        .num_plus,
        .num_minus,
        .num_times,
        .num_div_by,
        .num_div_trunc_by,
        .num_rem_by,
        .num_mod_by,
        .num_shift_left_by,
        .num_shift_right_by,
        .num_shift_right_zf_by,
        .num_is_eq,
        .bool_is_eq,
        .num_is_gt,
        .num_is_gte,
        .num_is_lt,
        .num_is_lte,
        // Ops handled by special cases in lowerLowLevel
        .num_is_negative,
        .num_is_positive,
        .num_negate,
        .num_is_zero,
        => null,
    };
}

// --- Tests ---

const testing = std.testing;

fn testInit() !struct { mir_store: MIR.Store, lir_store: LirExprStore, layout_store: layout.Store, module_env: @import("can").ModuleEnv, module_env_ptrs: [1]*const @import("can").ModuleEnv } {
    const allocator = testing.allocator;
    var result: @TypeOf(testInit() catch unreachable) = undefined;
    result.module_env = try @import("can").ModuleEnv.init(allocator, "");
    result.mir_store = try MIR.Store.init(allocator);
    result.lir_store = LirExprStore.init(allocator);
    // Must set module_env_ptrs AFTER struct is in final location
    return result;
}

fn testInitLayoutStore(self: *@TypeOf(testInit() catch unreachable)) !void {
    self.module_env_ptrs[0] = &self.module_env;
    self.layout_store = try layout.Store.init(&self.module_env_ptrs, null, testing.allocator, @import("base").target.TargetUsize.native);
}

fn testDeinit(self: *@TypeOf(testInit() catch unreachable)) void {
    self.layout_store.deinit();
    self.lir_store.deinit();
    self.mir_store.deinit(testing.allocator);
    self.module_env.deinit();
}

test "lowerExprSpan re-entrancy: list of calls preserves all elements" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    // Build MIR: a list containing two calls, each with one argument.
    // This triggers lowerExprSpan re-entrancy:
    //   lowerList -> lowerExprSpan([call0, call1])
    //     for call0: lowerExpr -> lowerCall -> lowerExprSpan([arg0])  <-- re-entrant!
    //     for call1: lowerExpr -> lowerCall -> lowerExprSpan([arg1])  <-- re-entrant!

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
    const sym_f = Symbol{ .module_idx = 0, .ident_idx = ident_f };

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
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(list_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // The list should have exactly 2 elements
    try testing.expect(lir_expr == .list);
    const elems = env.lir_store.getExprSpan(lir_expr.list.elems);
    try testing.expectEqual(@as(usize, 2), elems.len);

    // Both elements should be calls
    const elem0 = env.lir_store.getExpr(elems[0]);
    const elem1 = env.lir_store.getExpr(elems[1]);
    try testing.expect(elem0 == .call);
    try testing.expect(elem1 == .call);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(int_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);
    try testing.expect(lir_expr == .i64_literal);
    try testing.expectEqual(@as(i64, 42), lir_expr.i64_literal);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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
    const sym_x = Symbol{ .module_idx = 0, .ident_idx = ident_x };

    const lookup_expr = try env.mir_store.addExpr(allocator, .{ .lookup = sym_x }, i64_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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
    const sym_x = Symbol{ .module_idx = 0, .ident_idx = ident_x };

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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

test "MIR lambda with single capture lowers to closure with unwrapped_capture" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Create function type: () -> I64
    const func_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .func = .{
        .args = Monotype.Span.empty(),
        .ret = i64_mono,
        .effectful = false,
    } });

    // Define captured symbol: x = 42
    const ident_x = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_x = Symbol{ .module_idx = 0, .ident_idx = ident_x };

    const int_42 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    try env.mir_store.registerSymbolDef(allocator, sym_x, int_42);

    // Lambda body: just looks up x
    const body_lookup = try env.mir_store.addExpr(allocator, .{ .lookup = sym_x }, i64_mono, Region.zero());

    // Lambda with one capture: \{} -> x
    const captures = try env.mir_store.addCaptures(allocator, &.{.{ .symbol = sym_x }});
    const lambda_expr = try env.mir_store.addExpr(allocator, .{ .lambda = .{
        .params = MIR.PatternSpan.empty(),
        .body = body_lookup,
        .captures = captures,
    } }, func_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(lambda_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Should produce a closure, not a plain lambda
    try testing.expect(lir_expr == .closure);
    // Single capture → unwrapped_capture representation
    const clo = env.lir_store.getClosureData(lir_expr.closure);
    try testing.expect(clo.representation == .unwrapped_capture);
}

test "MIR lambda with multiple captures lowers to closure with struct_captures" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Create function type: () -> I64
    const func_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .func = .{
        .args = Monotype.Span.empty(),
        .ret = i64_mono,
        .effectful = false,
    } });

    // Define captured symbols: x = 42, y = 99
    const ident_x = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_x = Symbol{ .module_idx = 0, .ident_idx = ident_x };
    const ident_y = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const sym_y = Symbol{ .module_idx = 0, .ident_idx = ident_y };

    const int_42 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    try env.mir_store.registerSymbolDef(allocator, sym_x, int_42);

    const int_99 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 99)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    try env.mir_store.registerSymbolDef(allocator, sym_y, int_99);

    // Lambda body: just looks up x
    const body_lookup = try env.mir_store.addExpr(allocator, .{ .lookup = sym_x }, i64_mono, Region.zero());

    // Lambda with two captures: \{} -> x (captures x and y)
    const captures = try env.mir_store.addCaptures(allocator, &.{
        .{ .symbol = sym_x },
        .{ .symbol = sym_y },
    });
    const lambda_expr = try env.mir_store.addExpr(allocator, .{ .lambda = .{
        .params = MIR.PatternSpan.empty(),
        .body = body_lookup,
        .captures = captures,
    } }, func_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(lambda_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Should produce a closure, not a plain lambda
    try testing.expect(lir_expr == .closure);
    // Multiple captures → struct_captures representation
    const clo = env.lir_store.getClosureData(lir_expr.closure);
    try testing.expect(clo.representation == .struct_captures);
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
    const field_names = try env.mir_store.addFieldNameSpan(allocator, &.{ field_a, field_b, field_c });
    const record_expr = try env.mir_store.addExpr(allocator, .{ .record = .{
        .fields = field_exprs,
        .field_names = field_names,
    } }, record_mono, Region.zero());

    // Access field c (third field, index 2)
    const access_expr = try env.mir_store.addExpr(allocator, .{ .record_access = .{
        .record = record_expr,
        .field_name = field_c,
    } }, i64_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(access_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .field_access);
    // Field c is at index 2 in the record's field list
    try testing.expectEqual(@as(u16, 2), lir_expr.field_access.field_idx);
}

test "MIR tuple access preserves element index" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const bool_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.bool)];

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
    const tuple_expr = try env.mir_store.addExpr(allocator, .{ .tuple = .{
        .elems = elem_exprs,
    } }, tuple_mono, Region.zero());

    // Access element at index 2 (third element, I64)
    const access_expr = try env.mir_store.addExpr(allocator, .{ .tuple_access = .{
        .tuple = tuple_expr,
        .elem_index = 2,
    } }, i64_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(access_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .tuple_access);
    try testing.expectEqual(@as(u16, 2), lir_expr.tuple_access.elem_idx);
}

test "MIR lookup propagates symbol def to LIR store" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Register a symbol def in MIR: x = 42
    const ident_x = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_x = Symbol{ .module_idx = 0, .ident_idx = ident_x };

    const int_42 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    try env.mir_store.registerSymbolDef(allocator, sym_x, int_42);

    // LIR store should NOT have the def yet
    try testing.expect(env.lir_store.getSymbolDef(sym_x) == null);

    // Lower a lookup to x
    const lookup_expr = try env.mir_store.addExpr(allocator, .{ .lookup = sym_x }, i64_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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
    try testing.expectEqual(@as(i64, 42), def_expr.i64_literal);
}

test "MIR single-tag union with one payload emits payload directly (P0 fix)" {
    // Regression test for P0: single-tag unions with payloads (e.g. [Ok I64])
    // must emit the payload directly, not a .tag LIR node, because the layout
    // is the payload type (not a tag_union).
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(tag_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Should NOT be a .tag — should be the payload directly
    try testing.expect(lir_expr != .tag);
    // Should be an i64 literal (the payload was emitted directly)
    try testing.expect(lir_expr == .i64_literal);
    try testing.expectEqual(@as(i64, 42), lir_expr.i64_literal);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(tag_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Zero-arg single tag → zero_arg_tag with ZST layout
    try testing.expect(lir_expr == .zero_arg_tag);
    try testing.expectEqual(@as(u16, 0), lir_expr.zero_arg_tag.discriminant);
}

test "MIR single-tag union with multiple payloads emits tuple" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const bool_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.bool)];

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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(tag_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Multiple payloads → should be a tuple, not a .tag
    try testing.expect(lir_expr != .tag);
    try testing.expect(lir_expr == .tuple);
}

test "MIR single-tag union pattern with one arg emits payload pattern directly" {
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
    const sym_x = Symbol{ .module_idx = 0, .ident_idx = ident_x };
    const bind_pat = try env.mir_store.addPattern(allocator, .{ .bind = sym_x }, i64_mono);
    const pat_args = try env.mir_store.addPatternSpan(allocator, &.{bind_pat});
    const tag_pat = try env.mir_store.addPattern(allocator, .{ .tag = .{
        .name = tag_ok,
        .args = pat_args,
    } }, union_mono);

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_pat_id = try translator.lowerPattern(tag_pat);
    const lir_pat = env.lir_store.getPattern(lir_pat_id);

    // Should NOT be a .tag pattern — should be the payload pattern directly
    try testing.expect(lir_pat != .tag);
    // Should be a .bind pattern (the payload binding)
    try testing.expect(lir_pat == .bind);
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
    const sym_a = Symbol{ .module_idx = 0, .ident_idx = ident_a };
    const ident_b = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const sym_b = Symbol{ .module_idx = 0, .ident_idx = ident_b };

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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

test "MIR block with decl_var and mutate_var lowers to LIR decl and mutate" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];

    // Build MIR: { var s = 1; s = 2; s }
    const ident_s = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = true }, .idx = 1 };
    const sym_s = Symbol{ .module_idx = 0, .ident_idx = ident_s };

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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(block_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);
    try testing.expect(lir_expr == .block);

    // The block should have 2 statements
    const lir_stmts = env.lir_store.getStmts(lir_expr.block.stmts);
    try testing.expectEqual(@as(usize, 2), lir_stmts.len);

    // First statement is .decl (from decl_var)
    try testing.expect(lir_stmts[0] == .decl);
    // Second statement is .mutate (from mutate_var)
    try testing.expect(lir_stmts[1] == .mutate);

    // Final expression is a lookup
    const final = env.lir_store.getExpr(lir_expr.block.final_expr);
    try testing.expect(final == .lookup);
}

test "MIR lambda with heterogeneous captures (I64 + Str) lowers to closure with struct_captures" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i64)];
    const str_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.str)];

    // Create function type: () -> I64
    const func_mono = try env.mir_store.monotype_store.addMonotype(allocator, .{ .func = .{
        .args = Monotype.Span.empty(),
        .ret = i64_mono,
        .effectful = false,
    } });

    // Define captured symbols: x = 42 (I64), s = "hello" (Str)
    const ident_x = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_x = Symbol{ .module_idx = 0, .ident_idx = ident_x };
    const ident_s = Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 2 };
    const sym_s = Symbol{ .module_idx = 0, .ident_idx = ident_s };

    const int_42 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i64_mono, Region.zero());
    try env.mir_store.registerSymbolDef(allocator, sym_x, int_42);

    const str_hello = try env.mir_store.addExpr(allocator, .{ .str = StringLiteral.Idx.none }, str_mono, Region.zero());
    try env.mir_store.registerSymbolDef(allocator, sym_s, str_hello);

    // Lambda body: just looks up x
    const body_lookup = try env.mir_store.addExpr(allocator, .{ .lookup = sym_x }, i64_mono, Region.zero());

    // Lambda with two heterogeneous captures: x (I64) and s (Str)
    const captures = try env.mir_store.addCaptures(allocator, &.{
        .{ .symbol = sym_x },
        .{ .symbol = sym_s },
    });
    const lambda_expr = try env.mir_store.addExpr(allocator, .{ .lambda = .{
        .params = MIR.PatternSpan.empty(),
        .body = body_lookup,
        .captures = captures,
    } }, func_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(lambda_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Should produce a closure
    try testing.expect(lir_expr == .closure);
    // Multiple captures → struct_captures representation
    const clo = env.lir_store.getClosureData(lir_expr.closure);
    try testing.expect(clo.representation == .struct_captures);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

    const bool_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.bool)];
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

    const bool_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.bool)];
    const unit_mono = env.mir_store.monotype_store.unit_idx;

    // Build MIR: expect (true)
    const cond_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, bool_mono, Region.zero());

    const expect_expr = try env.mir_store.addExpr(allocator, .{ .expect = .{
        .body = cond_expr,
    } }, unit_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(break_expr_id);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .break_expr);
}

test "MIR num_plus low-level lowers to LIR binop add" {
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(ll_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .binop);
    try testing.expect(lir_expr.binop.op == .add);
}

test "MIR num_is_zero with f64 operand emits f64 zero literal" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const f64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.f64)];
    const bool_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.bool)];

    // Build MIR: run_low_level(.num_is_zero, [3.14])
    const arg0 = try env.mir_store.addExpr(allocator, .{ .frac_f64 = 3.14 }, f64_mono, Region.zero());
    const args = try env.mir_store.addExprSpan(allocator, &.{arg0});
    const ll_expr = try env.mir_store.addExpr(allocator, .{ .run_low_level = .{
        .op = .num_is_zero,
        .args = args,
    } }, bool_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(ll_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Should be a binop eq comparing the f64 operand with a zero literal
    try testing.expect(lir_expr == .binop);
    try testing.expect(lir_expr.binop.op == .eq);

    // The RHS (zero literal) should be an f64_literal, not i64_literal
    const rhs_expr = env.lir_store.getExpr(lir_expr.binop.rhs);
    try testing.expect(rhs_expr == .f64_literal);
}

test "MIR num_is_zero with i128 operand emits i128 zero literal" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i128_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.i128)];
    const bool_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.bool)];

    // Build MIR: run_low_level(.num_is_zero, [42_i128])
    const arg0 = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(u128, 42)), .kind = .i128 },
    } }, i128_mono, Region.zero());
    const args = try env.mir_store.addExprSpan(allocator, &.{arg0});
    const ll_expr = try env.mir_store.addExpr(allocator, .{ .run_low_level = .{
        .op = .num_is_zero,
        .args = args,
    } }, bool_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(ll_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Should be a binop eq comparing the i128 operand with a zero literal
    try testing.expect(lir_expr == .binop);
    try testing.expect(lir_expr.binop.op == .eq);

    // The RHS (zero literal) should be an i128_literal, not i64_literal
    const rhs_expr = env.lir_store.getExpr(lir_expr.binop.rhs);
    try testing.expect(rhs_expr == .i128_literal);
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

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(int_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .i128_literal);
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
    const field_names_span = try env.mir_store.addFieldNameSpan(allocator, &.{ field_age, field_name, field_score });
    const record_expr = try env.mir_store.addExpr(allocator, .{ .record = .{
        .fields = field_exprs,
        .field_names = field_names_span,
    } }, record_mono, Region.zero());

    // Access field "age" (U8) — alphabetically first but should be last in layout order
    // (I64 fields sorted before U8 by alignment)
    const access_expr = try env.mir_store.addExpr(allocator, .{ .record_access = .{
        .record = record_expr,
        .field_name = field_age,
    } }, u8_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(access_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .field_access);
    // In layout order, I64 fields (name, score) come before U8 (age), so age is at index 2
    try testing.expectEqual(@as(u16, 2), lir_expr.field_access.field_idx);
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

    // Create a destructure pattern that only binds field "a" (U8).
    // In MIR, field_names are alphabetical: [a].
    // In layout order: [b, c, a]. So b and c should get wildcard patterns
    // with I64 layout, NOT .zst.
    const ident_a = field_a;
    const sym_a = Symbol{ .module_idx = 0, .ident_idx = ident_a };
    const bind_pat = try env.mir_store.addPattern(allocator, .{ .bind = sym_a }, u8_mono);
    const destructs = try env.mir_store.addPatternSpan(allocator, &.{bind_pat});
    const destruct_field_names = try env.mir_store.addFieldNameSpan(allocator, &.{field_a});
    const destruct_pat = try env.mir_store.addPattern(allocator, .{ .record_destructure = .{
        .destructs = destructs,
        .field_names = destruct_field_names,
    } }, record_mono);

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_pat_id = try translator.lowerPattern(destruct_pat);
    const lir_pat = env.lir_store.getPattern(lir_pat_id);

    try testing.expect(lir_pat == .record);
    const field_pats = env.lir_store.getPatternSpan(lir_pat.record.fields);
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
    // Must emit i128_literal so getExprLayout returns the correct 128-bit layout.
    const int_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i128_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(int_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .i128_literal);
    try testing.expectEqual(@as(i128, 42), lir_expr.i128_literal);
}

test "MIR small u128 value emits i128_literal not i64_literal" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const u128_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.u128)];

    // Value 7 fits in i64, but monotype is u128.
    // Must emit i128_literal so getExprLayout returns the correct 128-bit layout.
    const int_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(u128, 7)), .kind = .u128 },
    } }, u128_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store);
    defer translator.deinit();

    const lir_id = try translator.lower(int_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .i128_literal);
    try testing.expectEqual(@as(i128, 7), lir_expr.i128_literal);
}
