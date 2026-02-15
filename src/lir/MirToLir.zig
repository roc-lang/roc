//! MIR → LIR translation pass.
//!
//! Translates monomorphic, desugared MIR expressions into layout-annotated LIR
//! expressions suitable for code generation. The main job is:
//!
//! - Converting Monotype.Idx → layout.Idx for every expression/pattern
//! - Resolving tag names to numeric discriminants
//! - Splitting MIR `lambda` into LIR `lambda` (no captures) vs `closure` (with captures)
//! - Mapping MIR's `match_expr` to LIR's `when`
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
const CalledVia = base.CalledVia;
const CIR = @import("can").CIR;

const LirExpr = LIR.LirExpr;
const LirExprId = LIR.LirExprId;
const LirExprSpan = LIR.LirExprSpan;
const LirPattern = LIR.LirPattern;
const LirPatternId = LIR.LirPatternId;
const LirPatternSpan = LIR.LirPatternSpan;
const LirStmt = LIR.LirStmt;
const LirStmtSpan = LIR.LirStmtSpan;
const LirWhenBranch = LIR.LirWhenBranch;
const LirCapture = LIR.LirCapture;
const LirCaptureSpan = LIR.LirCaptureSpan;
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
scratch_lir_when_branches: std.ArrayList(LirWhenBranch),
scratch_lir_captures: std.ArrayList(LirCapture),
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
        .scratch_lir_when_branches = std.ArrayList(LirWhenBranch).empty,
        .scratch_lir_captures = std.ArrayList(LirCapture).empty,
        .scratch_field_names = std.ArrayList(Ident.Idx).empty,
    };
}

pub fn deinit(self: *Self) void {
    self.layout_cache.deinit();
    self.propagating_defs.deinit();
    self.scratch_lir_expr_ids.deinit(self.allocator);
    self.scratch_lir_pattern_ids.deinit(self.allocator);
    self.scratch_lir_stmts.deinit(self.allocator);
    self.scratch_lir_when_branches.deinit(self.allocator);
    self.scratch_lir_captures.deinit(self.allocator);
    self.scratch_field_names.deinit(self.allocator);
}

// ── Public API ──────────────────────────────────────────────────────

/// Lower a MIR expression to a LIR expression.
pub fn lower(self: *Self, mir_expr_id: MIR.ExprId) Allocator.Error!LirExprId {
    return self.lowerExpr(mir_expr_id);
}

// ── Monotype → Layout conversion ────────────────────────────────────

/// Convert a Monotype.Idx to a layout.Idx, using a cache.
fn layoutFromMonotype(self: *Self, mono_idx: Monotype.Idx) Allocator.Error!layout.Idx {
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

    // Build field layouts and names, then use the layout store's putRecord-like logic
    const env = self.layout_store.all_module_envs[0];
    var field_layouts = std.ArrayList(layout.Layout).empty;
    defer field_layouts.deinit(self.allocator);
    var field_names = std.ArrayList(Ident.Idx).empty;
    defer field_names.deinit(self.allocator);

    for (fields) |field| {
        const field_layout_idx = try self.layoutFromMonotype(field.type_idx);
        const field_layout = self.layout_store.getLayout(field_layout_idx);
        try field_layouts.append(self.allocator, field_layout);
        try field_names.append(self.allocator, field.name);
    }

    return self.layout_store.putRecord(env, field_layouts.items, field_names.items);
}

fn layoutFromTuple(self: *Self, tup: anytype) Allocator.Error!layout.Idx {
    const elems = self.mir_store.monotype_store.getIdxSpan(tup.elems);
    if (elems.len == 0) return .zst;

    var elem_layouts = std.ArrayList(layout.Layout).empty;
    defer elem_layouts.deinit(self.allocator);

    for (elems) |elem_mono_idx| {
        const elem_layout_idx = try self.layoutFromMonotype(elem_mono_idx);
        const elem_layout = self.layout_store.getLayout(elem_layout_idx);
        try elem_layouts.append(self.allocator, elem_layout);
    }

    return self.layout_store.putTuple(elem_layouts.items);
}

fn layoutFromTagUnion(self: *Self, tu: anytype) Allocator.Error!layout.Idx {
    const tags = self.mir_store.monotype_store.getTags(tu.tags);
    if (tags.len == 0) return .zst;

    // Single tag with no payload → ZST
    if (tags.len == 1) {
        const payloads = self.mir_store.monotype_store.getIdxSpan(tags[0].payloads);
        if (payloads.len == 0) return .zst;
    }

    // For now, represent tag unions as a tuple: [u64 discriminant, max-payload]
    // This is a simplified representation; the dev backend handles the actual layout.
    // We compute the max payload size across all variants.
    var max_payload_layout: ?layout.Idx = null;
    var max_payload_size: u32 = 0;

    for (tags) |tag| {
        const payloads = self.mir_store.monotype_store.getIdxSpan(tag.payloads);
        if (payloads.len == 0) continue;

        // For a single payload, use it directly; for multiple, build a tuple
        var payload_idx: layout.Idx = undefined;
        if (payloads.len == 1) {
            payload_idx = try self.layoutFromMonotype(payloads[0]);
        } else {
            var elem_layouts = std.ArrayList(layout.Layout).empty;
            defer elem_layouts.deinit(self.allocator);
            for (payloads) |p| {
                const p_idx = try self.layoutFromMonotype(p);
                try elem_layouts.append(self.allocator, self.layout_store.getLayout(p_idx));
            }
            payload_idx = try self.layout_store.putTuple(elem_layouts.items);
        }
        const sa = self.layout_store.layoutSizeAlign(self.layout_store.getLayout(payload_idx));
        if (sa.size > max_payload_size) {
            max_payload_size = sa.size;
            max_payload_layout = payload_idx;
        }
    }

    // Build a tuple of [discriminant, payload]
    var elems = std.ArrayList(layout.Layout).empty;
    defer elems.deinit(self.allocator);
    try elems.append(self.allocator, layout.Layout.int(.u64)); // discriminant
    if (max_payload_layout) |pl| {
        try elems.append(self.allocator, self.layout_store.getLayout(pl));
    }
    return self.layout_store.putTuple(elems.items);
}

// ── Tag name → discriminant ─────────────────────────────────────────

/// Given a tag name and the monotype of the containing tag union,
/// return the discriminant (sorted index of the tag name).
fn tagDiscriminant(self: *const Self, tag_name: Ident.Idx, union_mono_idx: Monotype.Idx) u16 {
    const monotype = self.mir_store.monotype_store.getMonotype(union_mono_idx);
    switch (monotype) {
        .tag_union => |tu| {
            const tags = self.mir_store.monotype_store.getTags(tu.tags);
            for (tags, 0..) |tag, i| {
                if (@as(u32, @bitCast(tag.name)) == @as(u32, @bitCast(tag_name))) {
                    return @intCast(i);
                }
            }
        },
        else => {},
    }
    return 0;
}

// ── Expression lowering ─────────────────────────────────────────────

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
        .while_loop => |w| self.lowerWhileLoop(w, region),
        .return_expr => |r| self.lowerReturn(r, mono_idx, region),
        .break_expr => self.lir_store.addExpr(.runtime_error, region),
    };
}

fn lowerInt(self: *Self, int_data: anytype, _: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const val = int_data.value.toI128();
    if (val >= std.math.minInt(i64) and val <= std.math.maxInt(i64)) {
        return self.lir_store.addExpr(.{ .i64_literal = @intCast(val) }, region);
    }
    return self.lir_store.addExpr(.{ .i128_literal = val }, region);
}

fn lowerList(self: *Self, list_data: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    const elem_layout = switch (monotype) {
        .list => |l| try self.layoutFromMonotype(l.elem),
        else => layout.Idx.zst,
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
    const lir_fields = try self.lowerExprSpan(mir_fields);

    const mir_field_names = self.mir_store.getFieldNameSpan(rec.field_names);
    const lir_field_names = try self.lir_store.addFieldNameSpan(mir_field_names);

    return self.lir_store.addExpr(.{ .record = .{
        .record_layout = record_layout,
        .fields = lir_fields,
        .field_names = lir_field_names,
    } }, region);
}

fn lowerTuple(self: *Self, tup: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const tuple_layout = try self.layoutFromMonotype(mono_idx);
    const mir_elems = self.mir_store.getExprSpan(tup.elems);
    const lir_elems = try self.lowerExprSpan(mir_elems);
    return self.lir_store.addExpr(.{ .tuple = .{ .tuple_layout = tuple_layout, .elems = lir_elems } }, region);
}

fn lowerTag(self: *Self, tag_data: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const union_layout = try self.layoutFromMonotype(mono_idx);
    const discriminant = self.tagDiscriminant(tag_data.name, mono_idx);

    const mir_args = self.mir_store.getExprSpan(tag_data.args);
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

    self.scratch_lir_when_branches.clearRetainingCapacity();
    for (mir_branches) |branch| {
        // Use the first pattern of the branch (MIR branches can have alternatives via |)
        const branch_patterns = self.mir_store.getBranchPatterns(branch.patterns);
        if (branch_patterns.len == 0) continue;

        const lir_pat = try self.lowerPattern(branch_patterns[0].pattern);
        const lir_body = try self.lowerExpr(branch.body);
        const guard = if (branch.guard.isNone())
            LirExprId.none
        else
            try self.lowerExpr(branch.guard);

        try self.scratch_lir_when_branches.append(self.allocator, .{
            .pattern = lir_pat,
            .guard = guard,
            .body = lir_body,
        });
    }

    const when_branches = try self.lir_store.addWhenBranches(self.scratch_lir_when_branches.items);
    return self.lir_store.addExpr(.{ .when = .{
        .value = cond_id,
        .value_layout = value_layout,
        .branches = when_branches,
        .result_layout = result_layout,
    } }, region);
}

fn lowerLambda(self: *Self, lam: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const fn_layout = try self.layoutFromMonotype(mono_idx);
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    const ret_layout = switch (monotype) {
        .func => |f| try self.layoutFromMonotype(f.ret),
        else => try self.layoutFromMonotype(mono_idx),
    };

    const lir_params = try self.lowerPatternSpan(self.mir_store.getPatternSpan(lam.params));
    const lir_body = try self.lowerExpr(lam.body);

    // Check if this lambda has captures → closure
    const mir_captures = self.mir_store.getCaptures(lam.captures);
    if (mir_captures.len > 0) {
        // Build capture list with layouts
        self.scratch_lir_captures.clearRetainingCapacity();
        for (mir_captures) |cap| {
            // Look up the captured symbol's type by searching symbol_defs
            const cap_layout = if (self.mir_store.getSymbolDef(cap.symbol)) |def_id|
                try self.layoutFromMonotype(self.mir_store.typeOf(def_id))
            else
                layout.Idx.zst;

            try self.scratch_lir_captures.append(self.allocator, .{
                .symbol = cap.symbol,
                .layout_idx = cap_layout,
            });
        }

        const lir_captures = try self.lir_store.addCaptures(self.scratch_lir_captures.items);

        // Create the lambda expression first
        const lambda_expr = try self.lir_store.addExpr(.{ .lambda = .{
            .fn_layout = fn_layout,
            .params = lir_params,
            .body = lir_body,
            .ret_layout = ret_layout,
        } }, region);

        return self.lir_store.addExpr(.{ .closure = .{
            .closure_layout = fn_layout,
            .lambda = lambda_expr,
            .captures = lir_captures,
            .representation = .{ .struct_captures = .{
                .captures = lir_captures,
                .struct_layout = fn_layout,
            } },
            .recursion = .not_recursive,
            .self_recursive = .not_self_recursive,
            .is_bound_to_variable = false,
        } }, region);
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
    self.scratch_lir_stmts.clearRetainingCapacity();
    for (mir_stmts) |stmt| {
        const lir_pat = try self.lowerPattern(stmt.decl_var.pattern);
        const lir_expr = try self.lowerExpr(stmt.decl_var.expr);
        try self.scratch_lir_stmts.append(self.allocator, .{ .pattern = lir_pat, .expr = lir_expr });
    }

    const lir_stmts = try self.lir_store.addStmts(self.scratch_lir_stmts.items);
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

    // Find the field index from the monotype's field list
    var field_idx: u16 = 0;
    const monotype = self.mir_store.monotype_store.getMonotype(record_mono);
    switch (monotype) {
        .record => |r| {
            const fields = self.mir_store.monotype_store.getFields(r.fields);
            for (fields, 0..) |f, i| {
                if (@as(u32, @bitCast(f.name)) == @as(u32, @bitCast(ra.field_name))) {
                    field_idx = @intCast(i);
                    break;
                }
            }
        },
        else => {},
    }

    return self.lir_store.addExpr(.{ .field_access = .{
        .record_expr = lir_record,
        .record_layout = record_layout,
        .field_layout = field_layout,
        .field_idx = field_idx,
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
        return self.lir_store.addExpr(.{ .binop = .{
            .op = binop,
            .lhs = args_slice[0],
            .rhs = args_slice[1],
            .result_layout = ret_layout,
        } }, region);
    }

    const lir_op = mapLowLevel(ll.op) orelse {
        // Unsupported low-level op → runtime error
        return self.lir_store.addExpr(.runtime_error, region);
    };

    return self.lir_store.addExpr(.{ .low_level = .{
        .op = lir_op,
        .args = lir_args,
        .ret_layout = ret_layout,
    } }, region);
}

fn lowLevelToBinop(op: CIR.Expr.LowLevel) ?LirExpr.BinOp {
    return switch (op) {
        .num_is_eq, .bool_is_eq => .eq,
        .num_is_gt => .gt,
        .num_is_gte => .gte,
        .num_is_lt => .lt,
        .num_is_lte => .lte,
        .num_div_trunc_by => .div_trunc,
        .num_rem_by => .mod,
        else => null,
    };
}

fn lowerHosted(self: *Self, h: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const ret_layout = try self.layoutFromMonotype(mono_idx);

    // Lower the body to get the arguments
    const lir_body = try self.lowerExpr(h.body);

    // Build an args span from the body (hosted calls pass their args directly)
    const lir_args = try self.lir_store.addExprSpan(&.{lir_body});

    return self.lir_store.addExpr(.{ .hosted_call = .{
        .index = h.index,
        .args = lir_args,
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
    const lir_body = try self.lowerExpr(e.body);

    return self.lir_store.addExpr(.{ .expect = .{
        .cond = lir_body,
        .body = lir_body,
        .result_layout = result_layout,
    } }, region);
}

fn lowerForLoop(self: *Self, f: anytype, _: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const lir_list = try self.lowerExpr(f.list);
    const list_mono = self.mir_store.typeOf(f.list);
    const list_monotype = self.mir_store.monotype_store.getMonotype(list_mono);
    const elem_layout = switch (list_monotype) {
        .list => |l| try self.layoutFromMonotype(l.elem),
        else => layout.Idx.zst,
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

fn lowerWhileLoop(self: *Self, w: anytype, region: Region) Allocator.Error!LirExprId {
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

// ── Pattern lowering ────────────────────────────────────────────────

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
            const union_layout = try self.layoutFromMonotype(mono_idx);
            const discriminant = self.tagDiscriminant(t.name, mono_idx);
            const lir_args = try self.lowerPatternSpan(self.mir_store.getPatternSpan(t.args));
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
            const lir_fields = try self.lowerPatternSpan(self.mir_store.getPatternSpan(rd.destructs));
            break :blk self.lir_store.addPattern(.{ .record = .{
                .record_layout = record_layout,
                .fields = lir_fields,
            } }, region);
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
                else => layout.Idx.zst,
            };
            const lir_prefix = try self.lowerPatternSpan(self.mir_store.getPatternSpan(ld.patterns));
            const rest_pat = if (ld.rest_pattern.isNone())
                LirPatternId.none
            else
                try self.lowerPattern(ld.rest_pattern);
            break :blk self.lir_store.addPattern(.{ .list = .{
                .elem_layout = elem_layout,
                .prefix = lir_prefix,
                .rest = rest_pat,
            } }, region);
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

// ── Span helpers ────────────────────────────────────────────────────

fn lowerExprSpan(self: *Self, mir_expr_ids: []const MIR.ExprId) Allocator.Error!LirExprSpan {
    self.scratch_lir_expr_ids.clearRetainingCapacity();
    for (mir_expr_ids) |mir_id| {
        const lir_id = try self.lowerExpr(mir_id);
        try self.scratch_lir_expr_ids.append(self.allocator, lir_id);
    }
    return self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items);
}

fn lowerPatternSpan(self: *Self, mir_pat_ids: []const MIR.PatternId) Allocator.Error!LirPatternSpan {
    self.scratch_lir_pattern_ids.clearRetainingCapacity();
    for (mir_pat_ids) |mir_id| {
        const lir_id = try self.lowerPattern(mir_id);
        try self.scratch_lir_pattern_ids.append(self.allocator, lir_id);
    }
    return self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items);
}

// ── Low-level op mapping ────────────────────────────────────────────

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

        // List operations
        .list_len => .list_len,
        .list_is_empty => .list_is_empty,
        .list_get_unsafe => .list_get,
        .list_append_unsafe => .list_append,
        .list_append => .list_append,
        .list_concat => .list_concat,
        .list_with_capacity => .list_with_capacity,

        // Numeric operations (CIR names differ from LIR names)
        .num_plus => .num_add,
        .num_minus => .num_sub,
        .num_times => .num_mul,
        .num_div_by => .num_div,
        .num_negate => .num_neg,
        .num_abs => .num_abs,
        .num_mod_by => .num_mod,
        .num_from_numeral => .num_from_numeral,
        .num_from_str => .num_from_str,

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

        // CIR ops that don't have a direct LIR equivalent
        else => null,
    };
}
