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
lambda_set_store: *const LambdaSet.Store,

/// Ident index for the `True` tag — needed to resolve Bool discriminants
/// (Bool is `prim.bool`, not a `tag_union`, so we can't look up tags in the monotype).
true_tag: Ident.Idx,

/// Counter for generating unique synthetic symbols (used by ANF let-binding).
next_synthetic_id: u29 = 0,

/// Cache: Monotype.Idx → layout.Idx (avoid recomputation)
layout_cache: std.AutoHashMap(u32, layout.Idx),

/// Recursion guard for propagating symbol defs from MIR to LIR
propagating_defs: std.AutoHashMap(u64, void),

/// Maps symbol → layout for lambda parameters and let-bindings,
/// so captured variables can find their layout even when not in symbol_defs.
symbol_layouts: std.AutoHashMap(u64, layout.Idx),

/// Scratch buffer for ANF Let-binding accumulation
scratch_anf_stmts: std.ArrayList(LirStmt),

/// Scratch buffers for building spans
scratch_lir_expr_ids: std.ArrayList(LirExprId),
scratch_lir_pattern_ids: std.ArrayList(LirPatternId),
scratch_lir_stmts: std.ArrayList(LirStmt),
scratch_lir_match_branches: std.ArrayList(LirMatchBranch),
scratch_lir_captures: std.ArrayList(LirCapture),

/// Scratch buffers for layout building (reused across layoutFrom* calls)
scratch_layouts: std.ArrayList(layout.Layout),
scratch_layout_idxs: std.ArrayList(layout.Idx),

pub fn init(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    lir_store: *LirExprStore,
    layout_store: *layout.Store,
    lambda_set_store: *const LambdaSet.Store,
    true_tag: Ident.Idx,
) Self {
    return .{
        .allocator = allocator,
        .mir_store = mir_store,
        .lir_store = lir_store,
        .layout_store = layout_store,
        .lambda_set_store = lambda_set_store,
        .true_tag = true_tag,
        .layout_cache = std.AutoHashMap(u32, layout.Idx).init(allocator),
        .propagating_defs = std.AutoHashMap(u64, void).init(allocator),
        .symbol_layouts = std.AutoHashMap(u64, layout.Idx).init(allocator),
        .scratch_anf_stmts = std.ArrayList(LirStmt).empty,
        .scratch_lir_expr_ids = std.ArrayList(LirExprId).empty,
        .scratch_lir_pattern_ids = std.ArrayList(LirPatternId).empty,
        .scratch_lir_stmts = std.ArrayList(LirStmt).empty,
        .scratch_lir_match_branches = std.ArrayList(LirMatchBranch).empty,
        .scratch_lir_captures = std.ArrayList(LirCapture).empty,
        .scratch_layouts = std.ArrayList(layout.Layout).empty,
        .scratch_layout_idxs = std.ArrayList(layout.Idx).empty,
    };
}

pub fn deinit(self: *Self) void {
    self.layout_cache.deinit();
    self.propagating_defs.deinit();
    self.symbol_layouts.deinit();
    self.scratch_anf_stmts.deinit(self.allocator);
    self.scratch_lir_expr_ids.deinit(self.allocator);
    self.scratch_lir_pattern_ids.deinit(self.allocator);
    self.scratch_lir_stmts.deinit(self.allocator);
    self.scratch_lir_match_branches.deinit(self.allocator);
    self.scratch_lir_captures.deinit(self.allocator);
    self.scratch_layouts.deinit(self.allocator);
    self.scratch_layout_idxs.deinit(self.allocator);
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
        .recursive_placeholder => unreachable,
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
        .func => blk: {
            // Function values use closure layout semantics after MIR lowering.
            const empty_captures = try self.layout_store.getEmptyRecordLayout();
            break :blk try self.layout_store.insertLayout(layout.Layout.closure(empty_captures));
        },
        .record => |r| try self.layoutFromRecord(r),
        .tuple => |t| try self.layoutFromTuple(t),
        .tag_union => |tu| try self.layoutFromTagUnion(tu),
    };
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
            .lookup => |lk| {
                if (self.lir_store.getSymbolDef(lk.symbol)) |def_id| {
                    if (self.isCallableExpr(def_id, 0) and !self.isFunctionLayout(lk.layout_idx)) {
                        std.debug.panic("MirToLir invariant violated: callable lookup has non-callable layout at expr {}", .{i});
                    }
                }
            },
            else => {},
        }
    }
}

fn layoutFromRecord(self: *Self, record: anytype) Allocator.Error!layout.Idx {
    const fields = self.mir_store.monotype_store.getFields(record.fields);
    if (fields.len == 0) return .zst;
    if (fields.len == 1) return self.layoutFromMonotype(fields[0].type_idx);

    const env = self.layout_store.currentEnv();
    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);
    var scratch_names = std.ArrayList(Ident.Idx).empty;
    defer scratch_names.deinit(self.allocator);

    for (fields) |field| {
        const field_layout_idx = try self.layoutFromMonotype(field.type_idx);
        const field_layout = self.layout_store.getLayout(field_layout_idx);
        try self.scratch_layouts.append(self.allocator, field_layout);
        try scratch_names.append(self.allocator, field.name);
    }

    return self.layout_store.putRecord(env, self.scratch_layouts.items[save_layouts..], scratch_names.items);
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

    // Bool-like: exactly 2 tags, both zero-payload → bool layout
    if (tags.len == 2) {
        const p0 = self.mir_store.monotype_store.getIdxSpan(tags[0].payloads);
        const p1 = self.mir_store.monotype_store.getIdxSpan(tags[1].payloads);
        if (p0.len == 0 and p1.len == 0) return .bool;
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
        .prim => |p| {
            // Bool is a primitive, not a tag_union. Its discriminants are:
            // False = 0, True = 1 (sorted alphabetically).
            std.debug.assert(p == .bool);
            return if (tag_name.eql(self.true_tag)) 1 else 0;
        },
        .tag_union => |tu| {
            const tags = self.mir_store.monotype_store.getTags(tu.tags);

            // Bool tag_union (True/False): use the same convention as prim.bool.
            // Only apply this to actual Bool, not arbitrary 2-tag zero-payload unions
            // like Ok/Err — those must use the general sorted-tag-order discriminant.
            if (tags.len == 2) {
                const p0 = self.mir_store.monotype_store.getIdxSpan(tags[0].payloads);
                const p1 = self.mir_store.monotype_store.getIdxSpan(tags[1].payloads);
                if (p0.len == 0 and p1.len == 0) {
                    if (tags[0].name.eql(self.true_tag) or
                        tags[1].name.eql(self.true_tag))
                    {
                        return if (tag_name.eql(self.true_tag)) 1 else 0;
                    }
                }
            }

            // Only direct Ident.Idx equality is supported.
            for (tags, 0..) |tag, i| {
                if (tag.name.eql(tag_name)) {
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
        .unit, .record, .tuple, .list, .box, .func, .recursive_placeholder => {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "tagDiscriminant expected tag_union/Bool; got {s} for tag ident idx {d} mono_idx={d}",
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

        const bp = try acc.parent.freshBindPattern(expr_layout, false, region);
        try acc.parent.scratch_anf_stmts.append(acc.parent.allocator, .{ .decl = .{
            .pattern = bp.pattern,
            .expr = expr_id,
        } });

        return acc.parent.lir_store.addExpr(.{ .lookup = .{
            .symbol = bp.symbol,
            .layout_idx = expr_layout,
        } }, region);
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

/// Lower a span of MIR expressions, ensuring each is atomic (symbol/literal) via the accumulator.
fn lowerAnfSpan(self: *Self, acc: *LetAccumulator, mir_expr_ids: []const MIR.ExprId, region: Region) Allocator.Error!LirExprSpan {
    const save_len = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_len);
    for (mir_expr_ids) |mir_id| {
        const lir_id = try self.lowerExpr(mir_id);
        const mono = self.mir_store.typeOf(mir_id);
        const arg_layout = try self.layoutFromMonotype(mono);
        const ensured = try acc.ensureSymbol(lir_id, arg_layout, region);
        try self.scratch_lir_expr_ids.append(self.allocator, ensured);
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
        .run_low_level => |ll| self.lowerLowLevel(ll, mono_idx, region),
        .hosted => |h| self.lowerHosted(h, mono_idx, region),
        .runtime_err_can, .runtime_err_type, .runtime_err_ellipsis, .runtime_err_anno_only => {
            return self.lir_store.addExpr(.runtime_error, region);
        },
        .crash => |s| blk: {
            const lir_str_idx = try self.copyStringToLir(s);
            break :blk self.lir_store.addExpr(.{ .crash = .{ .msg = lir_str_idx } }, region);
        },
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
        .prim, .unit, .record, .tuple, .tag_union, .box, .func, .recursive_placeholder => unreachable,
    };

    const mir_elems = self.mir_store.getExprSpan(list_data.elems);
    if (mir_elems.len == 0) {
        return self.lir_store.addExpr(.{ .empty_list = .{ .elem_layout = elem_layout } }, region);
    }

    var acc = self.startLetAccumulator();
    const lir_elems = try self.lowerAnfSpan(&acc, mir_elems, region);
    const list_expr = try self.lir_store.addExpr(.{ .list = .{ .elem_layout = elem_layout, .elems = lir_elems } }, region);
    return acc.finish(list_expr, try self.layoutFromMonotype(mono_idx), region);
}

fn lowerRecord(self: *Self, rec: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const mir_fields = self.mir_store.getExprSpan(rec.fields);
    if (mir_fields.len == 0) {
        // Empty record: zero-field struct
        const record_layout = try self.layoutFromMonotype(mono_idx);
        return self.lir_store.addExpr(.{ .struct_ = .{
            .struct_layout = record_layout,
            .fields = LirExprSpan.empty(),
        } }, region);
    }

    // 1-field records are layout-unwrapped, so construction is just the field value.
    if (mir_fields.len == 1) {
        return self.lowerExpr(mir_fields[0]);
    }

    const record_layout = try self.layoutFromMonotype(mono_idx);
    const mir_field_names = self.mir_store.getFieldNameSpan(rec.field_names);
    const record_layout_val = self.layout_store.getLayout(record_layout);
    std.debug.assert(record_layout_val.tag == .struct_);

    var acc = self.startLetAccumulator();

    // MIR fields are in source/alphabetical order, but the layout store sorts
    // fields by alignment descending then alphabetically. Reorder expressions
    // to match layout order so codegen can use positional field indices.
    const record_data = self.layout_store.getStructData(record_layout_val.data.struct_.idx);
    const layout_fields = self.layout_store.struct_fields.sliceRange(record_data.getFields());

    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

    for (0..layout_fields.len) |li| {
        const layout_field_name = layout_fields.get(li).name;
        var found = false;
        for (mir_field_names, 0..) |mir_name, mi| {
            if (mir_name.eql(layout_field_name)) {
                const lir_expr = try self.lowerExpr(mir_fields[mi]);
                const field_mono = self.mir_store.typeOf(mir_fields[mi]);
                const field_layout = try self.layoutFromMonotype(field_mono);
                const ensured = try acc.ensureSymbol(lir_expr, field_layout, region);
                try self.scratch_lir_expr_ids.append(self.allocator, ensured);
                found = true;
                break;
            }
        }
        std.debug.assert(found);
    }

    const lir_fields = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);

    const struct_expr = try self.lir_store.addExpr(.{ .struct_ = .{
        .struct_layout = record_layout,
        .fields = lir_fields,
    } }, region);
    return acc.finish(struct_expr, record_layout, region);
}

fn lowerTuple(self: *Self, tup: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const tuple_layout = try self.layoutFromMonotype(mono_idx);
    const mir_elems = self.mir_store.getExprSpan(tup.elems);
    const tuple_layout_val = self.layout_store.getLayout(tuple_layout);

    var acc = self.startLetAccumulator();

    if (tuple_layout_val.tag == .struct_) {
        // MIR elements are in source order (.0, .1, .2, ...) but the layout store
        // sorts fields by alignment. Reorder to match layout order.
        const struct_data = self.layout_store.getStructData(tuple_layout_val.data.struct_.idx);
        const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());

        const save_exprs = self.scratch_lir_expr_ids.items.len;
        defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

        for (0..layout_fields.len) |li| {
            const original_index = layout_fields.get(li).index;
            const lir_expr = try self.lowerExpr(mir_elems[original_index]);
            const elem_mono = self.mir_store.typeOf(mir_elems[original_index]);
            const elem_layout = try self.layoutFromMonotype(elem_mono);
            const ensured = try acc.ensureSymbol(lir_expr, elem_layout, region);
            try self.scratch_lir_expr_ids.append(self.allocator, ensured);
        }

        const lir_fields = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);

        const struct_expr = try self.lir_store.addExpr(.{ .struct_ = .{
            .struct_layout = tuple_layout,
            .fields = lir_fields,
        } }, region);
        return acc.finish(struct_expr, tuple_layout, region);
    } else {
        // Non-struct layout (e.g. single-element optimization) — pass through directly
        const lir_elems = try self.lowerAnfSpan(&acc, mir_elems, region);
        const struct_expr = try self.lir_store.addExpr(.{ .struct_ = .{
            .struct_layout = tuple_layout,
            .fields = lir_elems,
        } }, region);
        return acc.finish(struct_expr, tuple_layout, region);
    }
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
            // Multiple payloads → layout is a struct, emit struct expression
            const struct_layout = try self.layoutFromMonotype(mono_idx);
            var acc = self.startLetAccumulator();
            const lir_elems = try self.lowerAnfSpan(&acc, mir_args, region);
            const struct_expr = try self.lir_store.addExpr(.{ .struct_ = .{ .struct_layout = struct_layout, .fields = lir_elems } }, region);
            return acc.finish(struct_expr, struct_layout, region);
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

    var acc = self.startLetAccumulator();
    const lir_args = try self.lowerAnfSpan(&acc, mir_args, region);
    const tag_expr = try self.lir_store.addExpr(.{ .tag = .{
        .discriminant = discriminant,
        .union_layout = union_layout,
        .args = lir_args,
    } }, region);
    return acc.finish(tag_expr, union_layout, region);
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
    var acc = self.startLetAccumulator();
    const cond_raw = try self.lowerExpr(match_data.cond);
    const cond_mono = self.mir_store.typeOf(match_data.cond);
    const value_layout = try self.layoutFromMonotype(cond_mono);
    const cond_id = try acc.ensureSymbol(cond_raw, value_layout, region);
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
    const match_expr = try self.lir_store.addExpr(.{ .match_expr = .{
        .value = cond_id,
        .value_layout = value_layout,
        .branches = match_branches,
        .result_layout = result_layout,
    } }, region);
    return acc.finish(match_expr, result_layout, region);
}

fn lowerLambda(self: *Self, lam: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const fn_layout = try self.layoutFromMonotype(mono_idx);
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    const ret_layout = switch (monotype) {
        .func => |f| try self.layoutFromMonotype(f.ret),
        .prim, .unit, .record, .tuple, .tag_union, .list, .box, .recursive_placeholder => unreachable, // Lambda expressions always have .func monotype
    };

    const lir_params = try self.lowerPatternSpan(self.mir_store.getPatternSpan(lam.params));
    const lir_body = try self.lowerExpr(lam.body);

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

fn lowerCall(self: *Self, call_data: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    // Annotation-only methods (e.g. Box.box/Box.unbox) are intentionally unsupported.
    const func_mir_expr = self.mir_store.getExpr(call_data.func);
    if (func_mir_expr == .lookup) {
        const sym = func_mir_expr.lookup;
        if (self.mir_store.getSymbolDef(sym)) |def_expr_id| {
            if (self.mir_store.getExpr(def_expr_id) == .runtime_err_anno_only) {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "MirToLir unsupported: call to annotation-only symbol key={d}",
                        .{sym.raw()},
                    );
                }
                unreachable;
            }
        }

        // Check if the callee has a lambda set (i.e., it's a closure value)
        if (self.lambda_set_store.getSymbolLambdaSet(sym)) |ls_idx| {
            return self.lowerClosureCall(call_data, ls_idx, sym, mono_idx, region);
        }
    }

    // Direct function call (non-closure)
    var acc = self.startLetAccumulator();
    const fn_expr_raw = try self.lowerExpr(call_data.func);
    const fn_mono = self.mir_store.typeOf(call_data.func);
    const fn_layout = try self.layoutFromMonotype(fn_mono);
    const fn_expr = try acc.ensureSymbol(fn_expr_raw, fn_layout, region);
    const ret_layout = try self.layoutFromMonotype(mono_idx);

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

/// Generate dispatch for a call to a closure value using lambda set information.
/// For single-member lambda sets: direct call with captures as extra arg.
/// For multi-member lambda sets: discriminant_switch dispatching to each member.
fn lowerClosureCall(
    self: *Self,
    call_data: anytype,
    ls_idx: LambdaSet.Idx,
    callee_symbol: Symbol,
    mono_idx: Monotype.Idx,
    region: Region,
) Allocator.Error!LirExprId {
    const ls = self.lambda_set_store.getLambdaSet(ls_idx);
    const members = self.lambda_set_store.getMembers(ls.members);
    const ret_layout = try self.layoutFromMonotype(mono_idx);

    if (members.len == 0) {
        if (std.debug.runtime_safety) {
            std.debug.panic("MirToLir: empty lambda set for symbol key={d}", .{callee_symbol.raw()});
        }
        unreachable;
    }

    // Lower user arguments (shared across all dispatch branches)
    var acc = self.startLetAccumulator();
    const mir_args = self.mir_store.getExprSpan(call_data.args);
    const lir_user_args = try self.lowerAnfSpan(&acc, mir_args, region);
    const user_arg_ids = self.lir_store.getExprSpan(lir_user_args);

    // Lower the closure value (captures tuple/tag union)
    const closure_val_raw = try self.lowerExpr(call_data.func);
    const closure_mono = self.mir_store.typeOf(call_data.func);
    const closure_layout = try self.layoutFromMonotype(closure_mono);
    const closure_val = try acc.ensureSymbol(closure_val_raw, closure_layout, region);

    if (members.len == 1) {
        // Single-member lambda set: direct call with captures as extra arg
        const member = members[0];
        const lifted_def = self.mir_store.getSymbolDef(member.fn_symbol) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic("MirToLir: missing def for lifted fn symbol key={d}", .{member.fn_symbol.raw()});
            }
            unreachable;
        };
        const lifted_mono = self.mir_store.typeOf(lifted_def);
        const lifted_layout = try self.layoutFromMonotype(lifted_mono);

        // Propagate the lifted function def to LIR
        if (self.lir_store.getSymbolDef(member.fn_symbol) == null) {
            const key: u64 = member.fn_symbol.raw();
            if (!self.propagating_defs.contains(key)) {
                try self.propagating_defs.put(key, {});
                defer _ = self.propagating_defs.remove(key);
                const lir_def = try self.lowerExpr(lifted_def);
                try self.lir_store.registerSymbolDef(member.fn_symbol, lir_def);
            }
        }

        const fn_expr = try self.lir_store.addExpr(.{ .lookup = .{
            .symbol = member.fn_symbol,
            .layout_idx = lifted_layout,
        } }, region);

        // Build args: [user_args..., closure_val]
        const save_exprs = self.scratch_lir_expr_ids.items.len;
        defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);
        for (user_arg_ids) |arg_id| {
            try self.scratch_lir_expr_ids.append(self.allocator, arg_id);
        }
        try self.scratch_lir_expr_ids.append(self.allocator, closure_val);
        const all_args = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);

        const call_expr = try self.lir_store.addExpr(.{ .call = .{
            .fn_expr = fn_expr,
            .fn_layout = lifted_layout,
            .args = all_args,
            .ret_layout = ret_layout,
            .called_via = .apply,
        } }, region);
        return acc.finish(call_expr, ret_layout, region);
    }

    // Multi-member lambda set: discriminant_switch on the tag union closure value
    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

    for (members) |member| {
        const lifted_def = self.mir_store.getSymbolDef(member.fn_symbol) orelse {
            if (std.debug.runtime_safety) {
                std.debug.panic("MirToLir: missing def for lifted fn symbol key={d}", .{member.fn_symbol.raw()});
            }
            unreachable;
        };
        const lifted_mono = self.mir_store.typeOf(lifted_def);
        const lifted_layout = try self.layoutFromMonotype(lifted_mono);

        // Propagate the lifted function def to LIR
        if (self.lir_store.getSymbolDef(member.fn_symbol) == null) {
            const key: u64 = member.fn_symbol.raw();
            if (!self.propagating_defs.contains(key)) {
                try self.propagating_defs.put(key, {});
                defer _ = self.propagating_defs.remove(key);
                const lir_def = try self.lowerExpr(lifted_def);
                try self.lir_store.registerSymbolDef(member.fn_symbol, lir_def);
            }
        }

        const fn_lookup = try self.lir_store.addExpr(.{ .lookup = .{
            .symbol = member.fn_symbol,
            .layout_idx = lifted_layout,
        } }, region);

        // Extract payload (captures) from the tag union
        const captures_layout = try self.layoutFromMonotype(member.captures_monotype);
        const payload_expr = try self.lir_store.addExpr(.{ .tag_payload_access = .{
            .value = closure_val,
            .union_layout = closure_layout,
            .payload_layout = captures_layout,
        } }, region);

        // Build args: [user_args..., payload]
        const inner_save = self.scratch_lir_expr_ids.items.len;
        for (user_arg_ids) |arg_id| {
            try self.scratch_lir_expr_ids.append(self.allocator, arg_id);
        }
        try self.scratch_lir_expr_ids.append(self.allocator, payload_expr);
        const branch_args = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[inner_save..]);
        self.scratch_lir_expr_ids.shrinkRetainingCapacity(inner_save);

        const branch_call = try self.lir_store.addExpr(.{ .call = .{
            .fn_expr = fn_lookup,
            .fn_layout = lifted_layout,
            .args = branch_args,
            .ret_layout = ret_layout,
            .called_via = .apply,
        } }, region);

        try self.scratch_lir_expr_ids.append(self.allocator, branch_call);
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

fn lowerBlock(self: *Self, block_data: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const result_layout = try self.layoutFromMonotype(mono_idx);

    const mir_stmts = self.mir_store.getStmts(block_data.stmts);
    const save_stmts_len = self.scratch_lir_stmts.items.len;
    const save_pattern_ids_len = self.scratch_lir_pattern_ids.items.len;
    defer self.scratch_lir_stmts.shrinkRetainingCapacity(save_stmts_len);
    defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_pattern_ids_len);

    // Pass 1: Lower all binding patterns first so symbol->layout registrations
    // are available to all statement expressions (including forward captures in
    // mutually-recursive closures inside the same block).
    for (mir_stmts) |stmt| {
        const binding = switch (stmt) {
            .decl_const, .decl_var, .mutate_var => |b| b,
        };
        const lir_pat = try self.lowerPattern(binding.pattern);
        try self.scratch_lir_pattern_ids.append(self.allocator, lir_pat);
    }

    // Pass 2: Lower expressions and assemble statements using cached patterns.
    for (mir_stmts, 0..) |stmt, i| {
        const binding = switch (stmt) {
            .decl_const, .decl_var, .mutate_var => |b| b,
        };
        const lir_expr = try self.lowerExpr(binding.expr);
        if (stmt == .decl_const or stmt == .decl_var) {
            const def_expr = self.lir_store.getExpr(lir_expr);
            const is_callable_def = switch (def_expr) {
                .lambda => true,
                else => false,
            };
            const pat_layout_idx = try self.layoutFromMonotype(self.mir_store.patternTypeOf(binding.pattern));
            const should_register_def = is_callable_def or self.isFunctionLayout(pat_layout_idx);
            const mir_pat = self.mir_store.getPattern(binding.pattern);
            switch (mir_pat) {
                .bind => |sym| {
                    if (should_register_def and self.lir_store.getSymbolDef(sym) == null) {
                        try self.lir_store.registerSymbolDef(sym, lir_expr);
                    }
                },
                .as_pattern => |as_pat| {
                    if (should_register_def and self.lir_store.getSymbolDef(as_pat.symbol) == null) {
                        try self.lir_store.registerSymbolDef(as_pat.symbol, lir_expr);
                    }
                },
                else => {},
            }
        }
        const lir_binding: LirStmt.Binding = .{
            .pattern = self.scratch_lir_pattern_ids.items[save_pattern_ids_len + i],
            .expr = lir_expr,
        };
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
    const struct_mono = self.mir_store.typeOf(ra.record);
    const struct_monotype = self.mir_store.monotype_store.getMonotype(struct_mono);
    if (struct_monotype == .record) {
        const fields = self.mir_store.monotype_store.getFields(struct_monotype.record.fields);
        if (fields.len == 1) {
            // 1-field records are layout-unwrapped: `record.field` is just `record`.
            return self.lowerExpr(ra.record);
        }
    }

    var acc = self.startLetAccumulator();
    const lir_struct_raw = try self.lowerExpr(ra.record);
    const struct_layout = try self.layoutFromMonotype(struct_mono);
    const lir_struct = try acc.ensureSymbol(lir_struct_raw, struct_layout, region);
    const result_mono = self.mir_store.typeOf(mir_expr_id);
    const field_layout = try self.layoutFromMonotype(result_mono);

    // Find the sorted field index from the layout's field list (alignment-sorted order),
    // not the monotype's field list (alphabetical order).
    var field_idx: ?u16 = null;
    const struct_layout_val = self.layout_store.getLayout(struct_layout);
    if (struct_layout_val.tag == .struct_) {
        const struct_data = self.layout_store.getStructData(struct_layout_val.data.struct_.idx);
        const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
        for (0..layout_fields.len) |li| {
            if (layout_fields.get(li).name.eql(ra.field_name)) {
                field_idx = @intCast(li);
                break;
            }
        }
    }

    const access_expr = try self.lir_store.addExpr(.{ .struct_access = .{
        .struct_expr = lir_struct,
        .struct_layout = struct_layout,
        .field_layout = field_layout,
        .field_idx = field_idx orelse unreachable,
    } }, region);
    return acc.finish(access_expr, field_layout, region);
}

fn lowerTupleAccess(self: *Self, ta: anytype, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    var acc = self.startLetAccumulator();
    const lir_struct_raw = try self.lowerExpr(ta.tuple);
    const struct_mono = self.mir_store.typeOf(ta.tuple);
    const struct_layout = try self.layoutFromMonotype(struct_mono);
    const lir_struct = try acc.ensureSymbol(lir_struct_raw, struct_layout, region);
    const result_mono = self.mir_store.typeOf(mir_expr_id);
    const field_layout = try self.layoutFromMonotype(result_mono);

    // Find the sorted field index for this tuple element's original index.
    var field_idx: ?u16 = null;
    const struct_layout_val = self.layout_store.getLayout(struct_layout);
    if (struct_layout_val.tag == .struct_) {
        const struct_data = self.layout_store.getStructData(struct_layout_val.data.struct_.idx);
        const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
        for (0..layout_fields.len) |li| {
            if (layout_fields.get(li).index == ta.elem_index) {
                field_idx = @intCast(li);
                break;
            }
        }
    } else {
        // Single-element tuple optimized to non-struct layout; field_idx 0
        field_idx = 0;
    }

    const access_expr = try self.lir_store.addExpr(.{ .struct_access = .{
        .struct_expr = lir_struct,
        .struct_layout = struct_layout,
        .field_layout = field_layout,
        .field_idx = field_idx orelse unreachable,
    } }, region);
    return acc.finish(access_expr, field_layout, region);
}

fn lowerLowLevel(self: *Self, ll: anytype, mono_idx: Monotype.Idx, region: Region) Allocator.Error!LirExprId {
    const ret_layout = try self.layoutFromMonotype(mono_idx);
    const mir_args = self.mir_store.getExprSpan(ll.args);
    var acc = self.startLetAccumulator();
    const lir_args = try self.lowerAnfSpan(&acc, mir_args, region);

    // num_is_negative/num_is_positive → comparison with 0
    if (ll.op == .num_is_negative or ll.op == .num_is_positive) {
        const args_slice = self.lir_store.getExprSpan(lir_args);
        const zero = try self.emitZeroLiteral(mir_args[0], region);
        const lir_op: LirExpr.LowLevel = if (ll.op == .num_is_negative) .num_is_lt else .num_is_gt;
        const new_args = try self.lir_store.addExprSpan(&.{ args_slice[0], zero });
        const result = try self.lir_store.addExpr(.{ .low_level = .{
            .op = lir_op,
            .args = new_args,
            .ret_layout = ret_layout,
        } }, region);
        return acc.finish(result, ret_layout, region);
    }

    // num_is_zero → num_is_eq with 0
    if (ll.op == .num_is_zero) {
        const args_slice = self.lir_store.getExprSpan(lir_args);
        const zero = try self.emitZeroLiteral(mir_args[0], region);
        const new_args = try self.lir_store.addExprSpan(&.{ args_slice[0], zero });
        const result = try self.lir_store.addExpr(.{ .low_level = .{
            .op = .num_is_eq,
            .args = new_args,
            .ret_layout = ret_layout,
        } }, region);
        return acc.finish(result, ret_layout, region);
    }

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
            const lir_op = mapLowLevel(ll.op) orelse {
                std.debug.panic("MirToLir: unmapped CIR low-level op: {s}", .{@tagName(ll.op)});
            };
            break :blk try self.lir_store.addExpr(.{ .low_level = .{
                .op = lir_op,
                .args = lir_args,
                .ret_layout = ret_layout,
            } }, region);
        },
    };
    return acc.finish(result, ret_layout, region);
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
        .func, .tag_union, .record, .tuple, .list, .box, .unit, .recursive_placeholder => unreachable,
    }, region);
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

    // Lower parameter patterns
    const mir_params = self.mir_store.getPatternSpan(h.params);
    const lir_params = try self.lowerPatternSpan(mir_params);

    // Build lookup args from parameters (one lookup per param)
    const func_args = switch (monotype) {
        .func => |f| self.mir_store.monotype_store.getIdxSpan(f.args),
        .prim, .unit, .record, .tuple, .tag_union, .list, .box, .recursive_placeholder => unreachable,
    };

    const save_len = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_len);

    for (mir_params, 0..) |mir_param_id, i| {
        const mir_pat = self.mir_store.getPattern(mir_param_id);
        const symbol = switch (mir_pat) {
            .bind => |sym| sym,
            .wildcard => continue, // unit () params are zero-sized, skip
            .tag, .int_literal, .str_literal, .dec_literal, .frac_f32_literal, .frac_f64_literal, .record_destructure, .tuple_destructure, .list_destructure, .as_pattern, .runtime_error => unreachable,
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
    const lir_list = try self.lowerExpr(f.list);
    const list_mono = self.mir_store.typeOf(f.list);
    const list_monotype = self.mir_store.monotype_store.getMonotype(list_mono);
    const elem_layout = switch (list_monotype) {
        .list => |l| try self.layoutFromMonotype(l.elem),
        .prim, .unit, .record, .tuple, .tag_union, .box, .func, .recursive_placeholder => unreachable,
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
            const reassignable = self.mir_store.isSymbolReassignable(sym);
            // Register symbol → layout so captured variables can find their layout
            const sym_key: u64 = @bitCast(sym);
            try self.symbol_layouts.put(sym_key, layout_idx);
            break :blk self.lir_store.addPattern(.{ .bind = .{
                .symbol = sym,
                .layout_idx = layout_idx,
                .reassignable = reassignable,
            } }, region);
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
                    break :blk self.lir_store.addPattern(.{ .struct_ = .{
                        .struct_layout = tuple_layout,
                        .fields = lir_elems,
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
        .str_literal => |s| blk: {
            const lir_str_idx = try self.copyStringToLir(s);
            break :blk self.lir_store.addPattern(.{ .str_literal = lir_str_idx }, region);
        },
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
            const mono = self.mir_store.monotype_store.getMonotype(mono_idx);
            const record = switch (mono) {
                .record => |r| r,
                else => unreachable,
            };
            const all_fields = self.mir_store.monotype_store.getFields(record.fields);

            if (all_fields.len == 1) {
                if (mir_patterns.len == 0) {
                    const field_layout = try self.layoutFromMonotype(all_fields[0].type_idx);
                    break :blk self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = field_layout } }, region);
                }
                break :blk try self.lowerPattern(mir_patterns[0]);
            }

            const record_layout_val = self.layout_store.getLayout(record_layout);

            if (record_layout_val.tag == .struct_) {
                const record_data = self.layout_store.getStructData(record_layout_val.data.struct_.idx);
                const layout_fields = self.layout_store.struct_fields.sliceRange(record_data.getFields());

                // For each layout field (in alignment-sorted order), find the matching
                // MIR pattern by field name and lower it
                const save_len = self.scratch_lir_pattern_ids.items.len;
                defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);

                for (0..layout_fields.len) |li| {
                    const layout_field_name = layout_fields.get(li).name;
                    // Find the MIR pattern with this field name
                    var found = false;
                    for (mir_field_names, 0..) |mir_name, mi| {
                        if (mir_name.eql(layout_field_name)) {
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
                break :blk self.lir_store.addPattern(.{ .struct_ = .{
                    .struct_layout = record_layout,
                    .fields = lir_fields,
                } }, region);
            } else {
                // Non-struct layout (e.g. ZST) — just lower patterns directly
                const lir_fields = try self.lowerPatternSpan(mir_patterns);
                break :blk self.lir_store.addPattern(.{ .struct_ = .{
                    .struct_layout = record_layout,
                    .fields = lir_fields,
                } }, region);
            }
        },
        .tuple_destructure => |td| blk: {
            const struct_layout = try self.layoutFromMonotype(mono_idx);
            const mir_patterns = self.mir_store.getPatternSpan(td.elems);
            const struct_layout_val = self.layout_store.getLayout(struct_layout);

            if (struct_layout_val.tag == .struct_) {
                // Reorder patterns from source order to layout (alignment-sorted) order
                const struct_data = self.layout_store.getStructData(struct_layout_val.data.struct_.idx);
                const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());

                const save_len = self.scratch_lir_pattern_ids.items.len;
                defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);

                for (0..layout_fields.len) |li| {
                    const original_index = layout_fields.get(li).index;
                    const lir_pat = try self.lowerPattern(mir_patterns[original_index]);
                    try self.scratch_lir_pattern_ids.append(self.allocator, lir_pat);
                }

                const lir_fields = try self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_len..]);
                break :blk self.lir_store.addPattern(.{ .struct_ = .{
                    .struct_layout = struct_layout,
                    .fields = lir_fields,
                } }, region);
            } else {
                // Non-struct layout — pass through directly
                const lir_elems = try self.lowerPatternSpan(mir_patterns);
                break :blk self.lir_store.addPattern(.{ .struct_ = .{
                    .struct_layout = struct_layout,
                    .fields = lir_elems,
                } }, region);
            }
        },
        .list_destructure => |ld| blk: {
            const list_monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
            const elem_layout = switch (list_monotype) {
                .list => |l| try self.layoutFromMonotype(l.elem),
                .prim, .unit, .record, .tuple, .tag_union, .box, .func, .recursive_placeholder => unreachable,
            };
            const all_patterns = self.mir_store.getPatternSpan(ld.patterns);
            const has_rest = !ld.rest_index.isNone();
            const rest_pat: LirPatternId = if (!has_rest) rest_blk: {
                break :rest_blk LirPatternId.none;
            } else if (!ld.rest_pattern.isNone()) rest_blk: {
                break :rest_blk try self.lowerPattern(ld.rest_pattern);
            } else rest_blk: {
                // Preserve `..` even when it doesn't bind (`[a, ..]`) so lowering
                // can distinguish it from exact patterns like `[a]`.
                const list_layout = try self.layoutFromMonotype(mono_idx);
                break :rest_blk try self.lir_store.addPattern(.{ .wildcard = .{
                    .layout_idx = list_layout,
                } }, region);
            };

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
            const reassignable = self.mir_store.isSymbolReassignable(ap.symbol);
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

        // *_to_str ops are handled by lowerLowLevel before reaching mapLowLevel
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
        => unreachable,

        // Arithmetic ops
        .num_plus => .num_add,
        .num_minus => .num_sub,
        .num_times => .num_mul,
        .num_div_by => .num_div,
        .num_div_trunc_by => .num_div_trunc,
        .num_rem_by => .num_rem,
        .num_mod_by => .num_mod,
        .num_negate => .num_neg,

        // Shift ops
        .num_shift_left_by => .num_shift_left_by,
        .num_shift_right_by => .num_shift_right_by,
        .num_shift_right_zf_by => .num_shift_right_zf_by,

        // Comparison ops
        .num_is_eq => .num_is_eq,
        .num_is_gt => .num_is_gt,
        .num_is_gte => .num_is_gte,
        .num_is_lt => .num_is_lt,
        .num_is_lte => .num_is_lte,

        // Boolean ops
        .bool_is_eq => .bool_is_eq,

        // Handled by special cases in lowerLowLevel
        .num_is_negative,
        .num_is_positive,
        .num_is_zero,
        .str_inspekt,
        => null,
    };
}

/// Create a fresh synthetic symbol for generated code (ANF bindings).
/// Uses a reserved high 32-bit namespace to avoid colliding with lowered symbols.
fn freshSymbol(self: *Self, reassignable: bool) Symbol {
    _ = reassignable;
    const id = self.next_synthetic_id;
    self.next_synthetic_id += 1;
    const raw = (@as(u64, std.math.maxInt(u32)) << 32) | @as(u64, id);
    return Symbol.fromRaw(raw);
}

/// Create a bind pattern for a fresh symbol.
fn freshBindPattern(self: *Self, layout_idx: layout.Idx, reassignable: bool, region: Region) Allocator.Error!struct { symbol: Symbol, pattern: LirPatternId } {
    const sym = self.freshSymbol(reassignable);
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
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
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
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
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
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(tag_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Multiple payloads → should be a struct, not a .tag
    try testing.expect(lir_expr != .tag);
    try testing.expect(lir_expr == .struct_);
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

    // First statement is .decl (from decl_var)
    try testing.expect(lir_stmts[0] == .decl);
    // Second statement is .mutate (from mutate_var)
    try testing.expect(lir_stmts[1] == .mutate);

    // Final expression is a lookup
    const final = env.lir_store.getExpr(lir_expr.block.final_expr);
    try testing.expect(final == .lookup);
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

    const bool_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.bool)];
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
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(ll_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .low_level);
    try testing.expect(lir_expr.low_level.op == .num_add);
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
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(ll_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Should be a low_level num_is_eq comparing the f64 operand with a zero literal
    try testing.expect(lir_expr == .low_level);
    try testing.expect(lir_expr.low_level.op == .num_is_eq);

    // The RHS (zero literal) should be an f64_literal, not i64_literal
    const ll_args = env.lir_store.getExprSpan(lir_expr.low_level.args);
    const rhs_expr = env.lir_store.getExpr(ll_args[1]);
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
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(ll_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    // Should be a low_level num_is_eq comparing the i128 operand with a zero literal
    try testing.expect(lir_expr == .low_level);
    try testing.expect(lir_expr.low_level.op == .num_is_eq);

    // The RHS (zero literal) should be an i128_literal, not i64_literal
    const ll_args = env.lir_store.getExprSpan(lir_expr.low_level.args);
    const rhs_expr = env.lir_store.getExpr(ll_args[1]);
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
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
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

    // Create a destructure pattern that only binds field "a" (U8).
    // In MIR, field_names are alphabetical: [a].
    // In layout order: [b, c, a]. So b and c should get wildcard patterns
    // with I64 layout, NOT .zst.
    const ident_a = field_a;
    const sym_a = try testMirSymbol(&env.mir_store, allocator, ident_a);
    const bind_pat = try env.mir_store.addPattern(allocator, .{ .bind = sym_a }, u8_mono);
    const destructs = try env.mir_store.addPatternSpan(allocator, &.{bind_pat});
    const destruct_field_names = try env.mir_store.addFieldNameSpan(allocator, &.{field_a});
    const destruct_pat = try env.mir_store.addPattern(allocator, .{ .record_destructure = .{
        .destructs = destructs,
        .field_names = destruct_field_names,
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
    // Must emit i128_literal so getExprLayout returns the correct 128-bit layout.
    const int_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, i128_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(int_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .i128_literal);
    try testing.expectEqual(@as(i128, 42), lir_expr.i128_literal);
}

// --- Bool.not LIR structural tests ---
// Verify that a prim.bool match (like negBool produces) gets correct discriminants:
// True pattern → discriminant 1, False body → discriminant 0.

test "LIR Bool match: True pattern gets discriminant 1, False body gets discriminant 0" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const bool_mono = env.mir_store.monotype_store.primIdx(.bool);
    const true_tag = env.module_env.idents.true_tag;
    const false_tag = env.module_env.idents.false_tag;

    // Build MIR: match <scrutinee> { True => False, _ => True }
    // This is exactly what negBool produces.

    // Scrutinee: Bool.True tag with prim.bool monotype
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

    const bool_mono = env.mir_store.monotype_store.primIdx(.bool);
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
    // Must emit i128_literal so getExprLayout returns the correct 128-bit layout.
    const int_expr = try env.mir_store.addExpr(allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(u128, 7)), .kind = .u128 },
    } }, u128_mono, Region.zero());
    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(int_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .i128_literal);
    try testing.expectEqual(@as(i128, 7), lir_expr.i128_literal);
}
