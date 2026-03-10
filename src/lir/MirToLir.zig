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
    prefix_count: u32,
    suffix_count: u32,
};

const LoweredBindingPattern = struct {
    pattern: LirPatternId,
    deferred_start: usize,
    deferred_len: usize,
};

const TopLevelRestBindingRewrite = struct {
    source_pattern: LirPatternId,
    destructure_pattern: LirPatternId,
    source_symbol: Symbol,
    source_layout: layout.Idx,
};

const DirectCallSpecialization = struct {
    symbol: Symbol,
    ret_layout: layout.Idx,
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

/// Cache: Monotype.Idx → layout.Idx (avoid recomputation)
layout_cache: std.AutoHashMap(u32, layout.Idx),

/// Recursion guard for propagating symbol defs from MIR to LIR
propagating_defs: std.AutoHashMap(u64, void),

/// Maps symbol → layout for lambda parameters and let-bindings,
/// so captured variables can find their layout even when not in symbol_defs.
symbol_layouts: std.AutoHashMap(u64, layout.Idx),

/// Recursion guard for computing runtime lambda-set payload layouts.
computing_lambda_set_layouts: std.AutoHashMap(u32, void),

/// Recursion guard for computing runtime layouts of direct call results by
/// Cache of direct-call specializations keyed by semantic callee identity + runtime param layouts.
specialized_direct_callees: std.StringHashMap(DirectCallSpecialization),

/// Recursion guard for lowering specialized direct-call definitions.
specializing_direct_callees: std.AutoHashMap(u64, void),

/// Temporary runtime-layout overrides for specific MIR lambda exprs during specialization.
specialized_lambda_param_layouts: std.AutoHashMap(u32, []const layout.Idx),

/// Scratch buffer for ANF Let-binding accumulation
scratch_anf_stmts: std.ArrayList(LirStmt),

/// Scratch buffers for building spans
scratch_lir_expr_ids: std.ArrayList(LirExprId),
scratch_lir_pattern_ids: std.ArrayList(LirPatternId),
scratch_lir_stmts: std.ArrayList(LirStmt),
scratch_lir_borrow_bindings: std.ArrayList(LIR.LirBorrowBinding),
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
        .layout_cache = std.AutoHashMap(u32, layout.Idx).init(allocator),
        .propagating_defs = std.AutoHashMap(u64, void).init(allocator),
        .symbol_layouts = std.AutoHashMap(u64, layout.Idx).init(allocator),
        .computing_lambda_set_layouts = std.AutoHashMap(u32, void).init(allocator),
        .specialized_direct_callees = std.StringHashMap(DirectCallSpecialization).init(allocator),
        .specializing_direct_callees = std.AutoHashMap(u64, void).init(allocator),
        .specialized_lambda_param_layouts = std.AutoHashMap(u32, []const layout.Idx).init(allocator),
        .scratch_anf_stmts = std.ArrayList(LirStmt).empty,
        .scratch_lir_expr_ids = std.ArrayList(LirExprId).empty,
        .scratch_lir_pattern_ids = std.ArrayList(LirPatternId).empty,
        .scratch_lir_stmts = std.ArrayList(LirStmt).empty,
        .scratch_lir_borrow_bindings = std.ArrayList(LIR.LirBorrowBinding).empty,
        .scratch_lir_match_branches = std.ArrayList(LirMatchBranch).empty,
        .scratch_lir_captures = std.ArrayList(LirCapture).empty,
        .scratch_deferred_list_rest_bindings = std.ArrayList(DeferredListRestBinding).empty,
        .scratch_layouts = std.ArrayList(layout.Layout).empty,
        .scratch_layout_idxs = std.ArrayList(layout.Idx).empty,
        .scratch_specialization_key = std.ArrayList(u8).empty,
    };
}

pub fn deinit(self: *Self) void {
    self.layout_cache.deinit();
    self.propagating_defs.deinit();
    self.symbol_layouts.deinit();
    self.computing_lambda_set_layouts.deinit();
    {
        var it = self.specialized_direct_callees.keyIterator();
        while (it.next()) |key_ptr| self.allocator.free(key_ptr.*);
    }
    self.specialized_direct_callees.deinit();
    self.specializing_direct_callees.deinit();
    {
        var it = self.specialized_lambda_param_layouts.iterator();
        while (it.next()) |entry| self.allocator.free(entry.value_ptr.*);
    }
    self.specialized_lambda_param_layouts.deinit();
    self.scratch_anf_stmts.deinit(self.allocator);
    self.scratch_lir_expr_ids.deinit(self.allocator);
    self.scratch_lir_pattern_ids.deinit(self.allocator);
    self.scratch_lir_stmts.deinit(self.allocator);
    self.scratch_lir_borrow_bindings.deinit(self.allocator);
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
        .borrow_scope => |b| self.isCallableExpr(b.body, depth + 1),
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
                _ = lk;
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

fn runtimeTagLayoutFromExpr(
    self: *Self,
    tag_data: anytype,
    union_mono_idx: Monotype.Idx,
) Allocator.Error!layout.Idx {
    const tags = switch (self.mir_store.monotype_store.getMonotype(union_mono_idx)) {
        .tag_union => |tu| self.mir_store.monotype_store.getTags(tu.tags),
        else => unreachable,
    };
    const mir_args = self.mir_store.getExprSpan(tag_data.args);

    if (tags.len == 0) return .zst;

    if (tags.len == 1) {
        if (mir_args.len == 0) return .zst;
        if (mir_args.len == 1) return self.runtimeValueLayoutFromMirExpr(mir_args[0]);
        return self.runtimeTupleLayoutFromExprs(mir_args);
    }

    if (tags.len == 2) {
        const p0 = self.mir_store.monotype_store.getIdxSpan(tags[0].payloads);
        const p1 = self.mir_store.monotype_store.getIdxSpan(tags[1].payloads);
        if (p0.len == 0 and p1.len == 0) return .bool;
    }

    const zst_idx = try self.layout_store.ensureZstLayout();
    const save_idxs = self.scratch_layout_idxs.items.len;
    defer self.scratch_layout_idxs.shrinkRetainingCapacity(save_idxs);

    var found_active = false;
    for (tags) |tag| {
        if (tag.name.eql(tag_data.name)) {
            found_active = true;
            if (mir_args.len == 0) {
                try self.scratch_layout_idxs.append(self.allocator, zst_idx);
            } else if (mir_args.len == 1) {
                try self.scratch_layout_idxs.append(self.allocator, try self.runtimeValueLayoutFromMirExpr(mir_args[0]));
            } else {
                try self.scratch_layout_idxs.append(self.allocator, try self.runtimeTupleLayoutFromExprs(mir_args));
            }
            continue;
        }

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
    field_names: []const Ident.Idx,
    field_exprs: []const MIR.ExprId,
) Allocator.Error!layout.Idx {
    if (field_exprs.len == 0) return .zst;
    if (field_exprs.len == 1) return self.runtimeValueLayoutFromMirExpr(field_exprs[0]);

    const env = self.layout_store.currentEnv();
    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);

    for (field_exprs) |field_expr_id| {
        const field_layout_idx = try self.runtimeValueLayoutFromMirExpr(field_expr_id);
        try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(field_layout_idx));
    }

    return self.layout_store.putRecord(env, self.scratch_layouts.items[save_layouts..], field_names);
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
        .tuple_destructure => |td| self.runtimeTupleLayoutFromPatternSpan(self.mir_store.getPatternSpan(td.elems)),
        .record_destructure => |rd| self.runtimeRecordLayoutFromPattern(mono_idx, rd),
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

fn runtimeRecordLayoutFromPattern(self: *Self, mono_idx: Monotype.Idx, rd: anytype) Allocator.Error!layout.Idx {
    const mono = self.mir_store.monotype_store.getMonotype(mono_idx);
    const record = switch (mono) {
        .record => |r| r,
        else => return self.layoutFromMonotype(mono_idx),
    };

    const all_fields = self.mir_store.monotype_store.getFields(record.fields);
    if (all_fields.len == 0) return try self.layout_store.getEmptyRecordLayout();
    if (all_fields.len == 1) {
        const mir_patterns = self.mir_store.getPatternSpan(rd.destructs);
        if (mir_patterns.len == 0) {
            return self.layoutFromMonotype(all_fields[0].type_idx);
        }
        return self.runtimeLayoutFromPattern(mir_patterns[0]);
    }

    const env = self.layout_store.currentEnv();
    const mir_patterns = self.mir_store.getPatternSpan(rd.destructs);
    const mir_field_names = self.mir_store.getFieldNameSpan(rd.field_names);
    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);
    var scratch_names = std.ArrayList(Ident.Idx).empty;
    defer scratch_names.deinit(self.allocator);

    for (all_fields) |field| {
        var field_layout_idx = try self.layoutFromMonotype(field.type_idx);
        for (mir_field_names, 0..) |mir_name, mi| {
            if (mir_name.eql(field.name)) {
                field_layout_idx = try self.runtimeLayoutFromPattern(mir_patterns[mi]);
                break;
            }
        }
        try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(field_layout_idx));
        try scratch_names.append(self.allocator, field.name);
    }

    return self.layout_store.putRecord(env, self.scratch_layouts.items[save_layouts..], scratch_names.items);
}

fn capturesLayoutForMember(self: *Self, member: LambdaSet.Member) Allocator.Error!layout.Idx {
    if (member.closure_member.isNone()) return .zst;

    const closure_member = self.mir_store.getClosureMember(member.closure_member);
    const capture_bindings = self.mir_store.getCaptureBindings(closure_member.capture_bindings);
    if (capture_bindings.len == 0) return .zst;

    const save_layouts = self.scratch_layouts.items.len;
    defer self.scratch_layouts.shrinkRetainingCapacity(save_layouts);

    for (capture_bindings) |binding| {
        const field_layout_idx = try self.runtimeValueLayoutFromMirExpr(binding.source_expr);
        try self.scratch_layouts.append(self.allocator, self.layout_store.getLayout(field_layout_idx));
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

/// Compute the runtime value layout for a lambda set.
/// Single-member sets use the captures payload layout directly.
/// Multi-member sets use a tag union over per-member captures payload layouts.
fn closureValueLayoutFromLambdaSet(self: *Self, ls_idx: LambdaSet.Idx) Allocator.Error!layout.Idx {
    const ls_key = @intFromEnum(ls_idx);
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
        return self.capturesLayoutForMember(member);
    }

    const save = self.scratch_layout_idxs.items.len;
    defer self.scratch_layout_idxs.shrinkRetainingCapacity(save);
    for (members) |member| {
        const payload_layout = try self.capturesLayoutForMember(member);
        try self.scratch_layout_idxs.append(self.allocator, payload_layout);
    }
    return self.layout_store.putTagUnion(self.scratch_layout_idxs.items[save..]);
}

fn lambdaSetForExpr(self: *Self, mir_expr_id: MIR.ExprId) ?LambdaSet.Idx {
    if (self.lambda_set_store.getExprLambdaSet(mir_expr_id)) |ls_idx| return ls_idx;

    return switch (self.mir_store.getExpr(mir_expr_id)) {
        .lookup => |sym| self.lambda_set_store.getSymbolLambdaSet(sym),
        .block => |block| self.lambda_set_store.getExprLambdaSet(block.final_expr),
        .record_access => |ra| self.lambdaSetForRecordField(ra.record, ra.field_name),
        .tuple_access => |ta| self.lambdaSetForTupleElem(ta.tuple, ta.elem_index),
        else => null,
    };
}

fn lambdaSetForRecordField(self: *Self, expr_id: MIR.ExprId, field_name: Ident.Idx) ?LambdaSet.Idx {
    return switch (self.mir_store.getExpr(expr_id)) {
        .record => |record| blk: {
            const field_names = self.mir_store.getFieldNameSpan(record.field_names);
            const fields = self.mir_store.getExprSpan(record.fields);
            for (field_names, 0..) |name, i| {
                if (!self.identsTextEqual(name, field_name)) continue;
                break :blk self.lambdaSetForExpr(fields[i]);
            }
            break :blk null;
        },
        .lookup => |symbol| blk: {
            const def_expr = self.mir_store.getSymbolDef(symbol) orelse break :blk null;
            break :blk self.lambdaSetForRecordField(def_expr, field_name);
        },
        .block => |block| self.lambdaSetForRecordField(block.final_expr, field_name),
        else => null,
    };
}

fn lambdaSetForTupleElem(self: *Self, expr_id: MIR.ExprId, elem_index: u32) ?LambdaSet.Idx {
    return switch (self.mir_store.getExpr(expr_id)) {
        .tuple => |tuple| blk: {
            const elems = self.mir_store.getExprSpan(tuple.elems);
            if (elem_index >= elems.len) break :blk null;
            break :blk self.lambdaSetForExpr(elems[elem_index]);
        },
        .lookup => |symbol| blk: {
            const def_expr = self.mir_store.getSymbolDef(symbol) orelse break :blk null;
            break :blk self.lambdaSetForTupleElem(def_expr, elem_index);
        },
        .block => |block| self.lambdaSetForTupleElem(block.final_expr, elem_index),
        else => null,
    };
}

fn runtimeLayoutForRecordField(self: *Self, expr_id: MIR.ExprId, field_name: Ident.Idx) Allocator.Error!?layout.Idx {
    return switch (self.mir_store.getExpr(expr_id)) {
        .record => |record| blk: {
            const field_names = self.mir_store.getFieldNameSpan(record.field_names);
            const fields = self.mir_store.getExprSpan(record.fields);
            for (field_names, 0..) |name, i| {
                if (!self.identsTextEqual(name, field_name)) continue;
                break :blk try self.runtimeValueLayoutFromMirExpr(fields[i]);
            }
            break :blk null;
        },
        .lookup => |symbol| blk: {
            const def_expr = self.mir_store.getSymbolDef(symbol) orelse break :blk null;
            break :blk try self.runtimeLayoutForRecordField(def_expr, field_name);
        },
        .block => |block| try self.runtimeLayoutForRecordField(block.final_expr, field_name),
        else => null,
    };
}

fn runtimeLayoutForTupleElem(self: *Self, expr_id: MIR.ExprId, elem_index: u32) Allocator.Error!?layout.Idx {
    return switch (self.mir_store.getExpr(expr_id)) {
        .tuple => |tuple| blk: {
            const elems = self.mir_store.getExprSpan(tuple.elems);
            if (elem_index >= elems.len) break :blk null;
            break :blk try self.runtimeValueLayoutFromMirExpr(elems[elem_index]);
        },
        .lookup => |symbol| blk: {
            if (self.symbol_layouts.get(symbol.raw())) |tuple_layout| {
                const tuple_layout_val = self.layout_store.getLayout(tuple_layout);
                if (tuple_layout_val.tag == .struct_) {
                    const struct_data = self.layout_store.getStructData(tuple_layout_val.data.struct_.idx);
                    const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
                    for (0..layout_fields.len) |li| {
                        if (layout_fields.get(li).index != elem_index) continue;
                        break :blk layout_fields.get(li).layout;
                    }
                }
            }
            const def_expr = self.mir_store.getSymbolDef(symbol) orelse break :blk null;
            break :blk try self.runtimeLayoutForTupleElem(def_expr, elem_index);
        },
        .block => |block| try self.runtimeLayoutForTupleElem(block.final_expr, elem_index),
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
        if (self.lambdaSetForExpr(mir_expr_id)) |ls_idx| {
            return self.closureValueLayoutFromLambdaSet(ls_idx);
        }
        if (self.mir_store.getExprClosureMember(mir_expr_id)) |closure_member_id| {
            const closure_member = self.mir_store.getClosureMember(closure_member_id);
            return self.capturesLayoutForMember(.{
                .fn_symbol = closure_member.fn_symbol,
                .closure_member = closure_member_id,
            });
        }
        switch (self.mir_store.getExpr(mir_expr_id)) {
            .record_access => |ra| {
                if (try self.runtimeLayoutForRecordField(ra.record, ra.field_name)) |layout_idx| return layout_idx;
            },
            .tuple_access => |ta| {
                if (try self.runtimeLayoutForTupleElem(ta.tuple, ta.elem_index)) |layout_idx| return layout_idx;
            },
            .lambda => return .zst,
            else => {},
        }
        if (std.debug.runtime_safety) {
            std.debug.panic("MirToLir: missing eager lambda set for function expression {}", .{@intFromEnum(mir_expr_id)});
        }
        unreachable;
    }

    switch (expr) {
        .call => |call_data| {
            if (try self.runtimeLayoutFromSpecializedDirectCall(call_data.func, self.mir_store.getExprSpan(call_data.args))) |layout_idx| {
                return layout_idx;
            }
        },
        .list => |list_data| {
            return self.runtimeListLayoutFromExprs(
                mono.list.elem,
                self.mir_store.getExprSpan(list_data.elems),
            );
        },
        .tag => |tag_data| {
            if (mono == .tag_union) {
                return self.runtimeTagLayoutFromExpr(tag_data, mono_idx);
            }
            return self.layoutFromMonotype(mono_idx);
        },
        .run_low_level => |ll| {
            if (ll.op == .list_get_unsafe) {
                const args = self.mir_store.getExprSpan(ll.args);
                if (builtin.mode == .Debug and args.len == 0) {
                    std.debug.panic("MirToLir invariant violated: list_get_unsafe missing list argument", .{});
                }
                return self.runtimeListElemLayoutFromMirExpr(args[0]);
            }
        },
        .tuple => |tup| return self.runtimeTupleLayoutFromExprs(self.mir_store.getExprSpan(tup.elems)),
        .record => |rec| return self.runtimeRecordLayoutFromExprs(
            self.mir_store.getFieldNameSpan(rec.field_names),
            self.mir_store.getExprSpan(rec.fields),
        ),
        .lookup => |sym| {
            if (self.symbol_layouts.get(sym.raw())) |layout_idx| {
                return try self.runtimeLayoutForBindingSymbol(sym, mono_idx, layout_idx);
            }
            if (self.mir_store.getSymbolDef(sym)) |def_expr_id| {
                return self.runtimeValueLayoutFromMirExpr(def_expr_id);
            }
            return self.layoutFromMonotype(mono_idx);
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
        .record_destructure => |rd| {
            for (self.mir_store.getPatternSpan(rd.destructs)) |field_pattern_id| {
                try self.collectPatternBindingSymbolKeys(field_pattern_id, out);
            }
        },
        .tuple_destructure => |td| {
            for (self.mir_store.getPatternSpan(td.elems)) |elem_pattern_id| {
                try self.collectPatternBindingSymbolKeys(elem_pattern_id, out);
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

        const fresh_symbol = self.freshSymbol();
        try self.specialized_direct_callees.put(owned_key, .{
            .symbol = fresh_symbol,
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

    const owned_param_layouts = try self.allocator.dupe(layout.Idx, param_layouts);
    errdefer self.allocator.free(owned_param_layouts);

    try self.specializing_direct_callees.put(resolved_symbol.raw(), {});
    defer _ = self.specializing_direct_callees.remove(resolved_symbol.raw());

    const lambda_key = @intFromEnum(lambda_expr_id);
    if (builtin.mode == .Debug and self.specialized_lambda_param_layouts.contains(lambda_key)) {
        std.debug.panic(
            "MirToLir invariant violated: duplicate specialized lambda param layout override for expr {}",
            .{@intFromEnum(lambda_expr_id)},
        );
    }
    try self.specialized_lambda_param_layouts.put(lambda_key, owned_param_layouts);
    defer {
        const removed = self.specialized_lambda_param_layouts.fetchRemove(lambda_key) orelse unreachable;
        self.allocator.free(removed.value);
    }

    if (!original_fn_symbol.isNone()) {
        try self.prepareLiftedDefCaptureLayout(original_fn_symbol, lifted_def);
    }

    const inferred_ret_layout = try self.runtimeLayoutForLambdaBodyWithParamLayouts(
        lifted_def,
        param_layouts,
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

    const lir_def = try self.lowerExpr(lifted_def);
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

    var saved_layouts = std.ArrayList(struct {
        sym_key: u64,
        previous: ?layout.Idx,
    }).empty;
    defer saved_layouts.deinit(self.allocator);

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

    return try self.runtimeValueLayoutFromMirExpr(resolved.body);
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

        const save_layouts = self.scratch_layout_idxs.items.len;
        defer self.scratch_layout_idxs.shrinkRetainingCapacity(save_layouts);
        for (call_args) |arg_expr_id| {
            try self.scratch_layout_idxs.append(self.allocator, try self.runtimeValueLayoutFromMirExpr(arg_expr_id));
        }

        var resolved_ret_layout: ?layout.Idx = null;
        const closure_layout: ?layout.Idx = if (members.items.len == 1 and !members.items[0].closure_member.isNone())
            try self.runtimeValueLayoutFromMirExpr(callee_expr_id)
        else
            null;

        for (members.items) |member| {
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
                if (members.items.len == 1) {
                    try self.scratch_layout_idxs.append(self.allocator, closure_layout.?);
                } else {
                    try self.scratch_layout_idxs.append(self.allocator, try self.capturesLayoutForMember(member));
                }
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
            } else if (builtin.mode == .Debug and resolved_ret_layout.? != specialization.ret_layout) {
                std.debug.panic(
                    "MirToLir invariant violated: specialized direct call ret_layout mismatch for callee expr {} ({d} vs {d})",
                    .{
                        @intFromEnum(callee_expr_id),
                        @intFromEnum(resolved_ret_layout.?),
                        @intFromEnum(specialization.ret_layout),
                    },
                );
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
    const list_layout_idx = try self.runtimeValueLayoutFromMirExpr(list_mir_expr_id);
    const list_layout = self.layout_store.getLayout(list_layout_idx);

    return switch (list_layout.tag) {
        .list => list_layout.data.list,
        .list_of_zst => .zst,
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

    const payload_args = try self.lir_store.addExprSpan(&.{value_lir_expr});
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

/// Check if a monotype is a single-tag union (exactly one variant).
fn isSingleTagUnion(self: *const Self, mono_idx: Monotype.Idx) bool {
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    return switch (monotype) {
        .tag_union => |tu| self.mir_store.monotype_store.getTags(tu.tags).len == 1,
        else => false,
    };
}

fn isTrueTag(self: *const Self, tag_name: Ident.Idx) bool {
    if (tag_name.eql(self.true_tag)) return true;

    for (self.layout_store.moduleEnvs()) |env| {
        if (env.getIdentStoreConst().findByString("True")) |true_ident| {
            if (tag_name.eql(true_ident)) return true;
        }
    }

    return false;
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
    if (!roundtrip.eql(ident)) return null;
    return text;
}

fn identText(self: *const Self, ident: Ident.Idx) ?[]const u8 {
    if (identTextIfOwnedBy(self.layout_store.currentEnv(), ident)) |text| return text;

    for (self.layout_store.moduleEnvs()) |env| {
        if (identTextIfOwnedBy(env, ident)) |text| return text;
    }

    return null;
}

fn identsTextEqual(self: *const Self, lhs: Ident.Idx, rhs: Ident.Idx) bool {
    if (lhs.eql(rhs)) return true;

    const lhs_text = self.identText(lhs) orelse return false;
    const rhs_text = self.identText(rhs) orelse return false;
    return std.mem.eql(u8, lhs_text, rhs_text);
}

const RecordFieldResolution = struct {
    original_index: u16,
    type_idx: Monotype.Idx,
};

fn resolveRecordField(self: *const Self, record_mono_idx: Monotype.Idx, field_name: Ident.Idx) ?RecordFieldResolution {
    const mono = self.mir_store.monotype_store.getMonotype(record_mono_idx);
    switch (mono) {
        .record => |record| {
            const fields = self.mir_store.monotype_store.getFields(record.fields);
            for (fields, 0..) |field, i| {
                if (!self.identsTextEqual(field.name, field_name)) continue;
                return .{
                    .original_index = @intCast(i),
                    .type_idx = field.type_idx,
                };
            }
        },
        else => {},
    }
    return null;
}

const RuntimeStructFieldResolution = struct {
    sorted_index: u16,
    field_layout: layout.Idx,
};

fn resolveRuntimeStructFieldByOriginalIndex(
    self: *const Self,
    runtime_layout: layout.Idx,
    original_index: u16,
) ?RuntimeStructFieldResolution {
    const layout_val = self.layout_store.getLayout(runtime_layout);
    if (layout_val.tag != .struct_) return null;

    const struct_data = self.layout_store.getStructData(layout_val.data.struct_.idx);
    const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
    for (0..layout_fields.len) |li| {
        const field = layout_fields.get(li);
        if (field.index != original_index) continue;
        return .{
            .sorted_index = @intCast(li),
            .field_layout = field.layout,
        };
    }
    return null;
}

/// Given a tag name and the monotype of the containing tag union,
/// return the discriminant (sorted index of the tag name).
fn tagDiscriminant(self: *const Self, tag_name: Ident.Idx, union_mono_idx: Monotype.Idx) u16 {
    const monotype = self.mir_store.monotype_store.getMonotype(union_mono_idx);
    switch (monotype) {
        .tag_union => |tu| {
            const tags = self.mir_store.monotype_store.getTags(tu.tags);

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
        .borrow_scope,
        .early_return,
        .break_expr,
        .semantic_low_level,
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

/// Lower a span of MIR expressions, ensuring each is atomic (symbol/literal) via the accumulator.
fn lowerAnfSpan(self: *Self, acc: *LetAccumulator, mir_expr_ids: []const MIR.ExprId, region: Region) Allocator.Error!LirExprSpan {
    const save_len = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_len);
    for (mir_expr_ids) |mir_id| {
        const lir_id = try self.lowerExpr(mir_id);
        const arg_layout = try self.runtimeValueLayoutFromMirExpr(mir_id);
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
        .list => |l| self.lowerList(l, mono_idx, mir_expr_id, region),
        .record => |r| self.lowerRecord(r, mono_idx, mir_expr_id, region),
        .tuple => |t| self.lowerTuple(t, mono_idx, mir_expr_id, region),
        .tag => |t| self.lowerTag(t, mono_idx, mir_expr_id, region),
        .lookup => |sym| self.lowerLookup(sym, mono_idx, mir_expr_id, region),
        .match_expr => |m| self.lowerMatch(m, mir_expr_id, region),
        .lambda => |l| self.lowerLambda(l, mono_idx, mir_expr_id, region),
        .call => |c| self.lowerCall(c, mir_expr_id, region),
        .block => |b| self.lowerBlock(b, mir_expr_id, region),
        .borrow_scope => |b| self.lowerBorrowScope(b, mir_expr_id, region),
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

fn lowerList(self: *Self, list_data: anytype, mono_idx: Monotype.Idx, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    _ = mono_idx;
    const list_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const list_layout_val = self.layout_store.getLayout(list_layout);
    const elem_layout = switch (list_layout_val.tag) {
        .list => list_layout_val.data.list,
        .list_of_zst => .zst,
        .scalar, .box, .box_of_zst, .struct_, .closure, .zst, .tag_union => unreachable,
    };

    const mir_elems = self.mir_store.getExprSpan(list_data.elems);
    if (mir_elems.len == 0) {
        return self.lir_store.addExpr(.{ .empty_list = .{
            .list_layout = list_layout,
            .elem_layout = elem_layout,
        } }, region);
    }

    var acc = self.startLetAccumulator();
    const lir_elems = try self.lowerAnfSpan(&acc, mir_elems, region);
    const list_expr = try self.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = elem_layout,
        .elems = lir_elems,
    } }, region);
    return acc.finish(list_expr, list_layout, region);
}

fn lowerRecord(self: *Self, rec: anytype, _: Monotype.Idx, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const mir_fields = self.mir_store.getExprSpan(rec.fields);
    if (mir_fields.len == 0) {
        // Empty record: zero-field struct
        const record_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
        return self.lir_store.addExpr(.{ .struct_ = .{
            .struct_layout = record_layout,
            .fields = LirExprSpan.empty(),
        } }, region);
    }

    // 1-field records are layout-unwrapped, so construction is just the field value.
    if (mir_fields.len == 1) {
        return self.lowerExpr(mir_fields[0]);
    }

    const record_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
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
                const field_layout = try self.runtimeValueLayoutFromMirExpr(mir_fields[mi]);
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

fn lowerTuple(self: *Self, tup: anytype, _: Monotype.Idx, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const tuple_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const mir_elems = self.mir_store.getExprSpan(tup.elems);
    const tuple_layout_val = self.layout_store.getLayout(tuple_layout);

    if (self.mir_store.getExprClosureMember(mir_expr_id) != null) {
        const save_exprs = self.scratch_lir_expr_ids.items.len;
        defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

        const save_borrow_len = self.scratch_lir_borrow_bindings.items.len;
        defer self.scratch_lir_borrow_bindings.shrinkRetainingCapacity(save_borrow_len);

        if (tuple_layout_val.tag == .struct_) {
            const struct_data = self.layout_store.getStructData(tuple_layout_val.data.struct_.idx);
            const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());

            for (0..layout_fields.len) |li| {
                const original_index = layout_fields.get(li).index;
                const lir_expr = try self.lowerExpr(mir_elems[original_index]);
                const elem_layout = try self.runtimeValueLayoutFromMirExpr(mir_elems[original_index]);
                const ensured = if (self.isBorrowAtomicExpr(lir_expr))
                    lir_expr
                else blk: {
                    const borrow_bind = try self.freshBindPattern(elem_layout, false, region);
                    try self.scratch_lir_borrow_bindings.append(self.allocator, .{
                        .pattern = borrow_bind.pattern,
                        .expr = lir_expr,
                    });
                    break :blk try self.lir_store.addExpr(.{ .lookup = .{
                        .symbol = borrow_bind.symbol,
                        .layout_idx = elem_layout,
                    } }, region);
                };
                try self.scratch_lir_expr_ids.append(self.allocator, ensured);
            }
        } else {
            for (mir_elems) |mir_elem| {
                const lir_expr = try self.lowerExpr(mir_elem);
                const elem_layout = try self.runtimeValueLayoutFromMirExpr(mir_elem);
                const ensured = if (self.isBorrowAtomicExpr(lir_expr))
                    lir_expr
                else blk: {
                    const borrow_bind = try self.freshBindPattern(elem_layout, false, region);
                    try self.scratch_lir_borrow_bindings.append(self.allocator, .{
                        .pattern = borrow_bind.pattern,
                        .expr = lir_expr,
                    });
                    break :blk try self.lir_store.addExpr(.{ .lookup = .{
                        .symbol = borrow_bind.symbol,
                        .layout_idx = elem_layout,
                    } }, region);
                };
                try self.scratch_lir_expr_ids.append(self.allocator, ensured);
            }
        }

        const lir_fields = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_exprs..]);
        const struct_expr = try self.lir_store.addExpr(.{ .struct_ = .{
            .struct_layout = tuple_layout,
            .fields = lir_fields,
        } }, region);

        if (save_borrow_len == self.scratch_lir_borrow_bindings.items.len) {
            return struct_expr;
        }

        const borrow_bindings = try self.lir_store.addBorrowBindings(self.scratch_lir_borrow_bindings.items[save_borrow_len..]);
        const result_bind = try self.freshBindPattern(tuple_layout, false, region);
        return self.lir_store.addExpr(.{ .borrow_scope = .{
            .bindings = borrow_bindings,
            .result_symbol = result_bind.symbol,
            .result_pattern = result_bind.pattern,
            .body = struct_expr,
            .result_layout = tuple_layout,
        } }, region);
    }

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

fn lowerTag(self: *Self, tag_data: anytype, mono_idx: Monotype.Idx, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
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

    const union_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
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
        if (self.mir_store.getSymbolDef(sym)) |mir_def_id| {
            break :blk try self.runtimeValueLayoutFromMirExpr(mir_def_id);
        }
        if (self.mir_store.monotype_store.getMonotype(mono_idx) == .func) {
            if (self.lambda_set_store.getSymbolLambdaSet(sym)) |ls_idx| {
                break :blk try self.closureValueLayoutFromLambdaSet(ls_idx);
            }
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
            const lowered_pat = try self.lowerBindingPatternForRuntimeLayout(bp.pattern, value_layout, region);
            const rewrite = try self.rewriteTopLevelRestBinding(lowered_pat, region);
            const branch_body = if (rewrite) |rw| blk: {
                const save_stmt_len = self.scratch_lir_stmts.items.len;
                defer self.scratch_lir_stmts.shrinkRetainingCapacity(save_stmt_len);
                const owned_source = try self.lir_store.addExpr(.{ .incref = .{
                    .value = cond_id,
                    .layout_idx = value_layout,
                    .count = 1,
                } }, region);
                try self.scratch_lir_stmts.append(self.allocator, .{ .decl = .{
                    .pattern = rw.source_pattern,
                    .expr = owned_source,
                } });
                try self.appendDeferredListRestBindingDecls(
                    lowered_pat.deferred_start,
                    lowered_pat.deferred_len,
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
                    lowered_pat.deferred_start,
                    lowered_pat.deferred_len,
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

fn lowerLambda(self: *Self, lam: anytype, mono_idx: Monotype.Idx, mir_lambda_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    if (self.specialized_lambda_param_layouts.get(@intFromEnum(mir_lambda_expr_id))) |param_layouts| {
        return self.lowerLambdaWithParamLayouts(lam, mono_idx, param_layouts, region);
    }

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
    const fn_layout = try self.layoutFromMonotype(mono_idx);
    const monotype = self.mir_store.monotype_store.getMonotype(mono_idx);
    const mir_params = self.mir_store.getPatternSpan(lam.params);
    if (builtin.mode == .Debug and mir_params.len != param_layouts.len) {
        std.debug.panic(
            "MirToLir invariant violated: lambda param layout count mismatch ({d} params, {d} layouts)",
            .{ mir_params.len, param_layouts.len },
        );
    }
    const save_rest_bindings = self.scratch_deferred_list_rest_bindings.items.len;
    defer self.scratch_deferred_list_rest_bindings.shrinkRetainingCapacity(save_rest_bindings);
    var param_infos = std.ArrayList(LoweredBindingPattern).empty;
    defer param_infos.deinit(self.allocator);
    var param_rewrites = std.ArrayList(?TopLevelRestBindingRewrite).empty;
    defer param_rewrites.deinit(self.allocator);

    const save_param_patterns = self.scratch_lir_pattern_ids.items.len;
    defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_param_patterns);
    for (mir_params, 0..) |mir_param_id, i| {
        const param_layout = param_layouts[i];
        try self.registerBindingPatternSymbols(mir_param_id, param_layout);
        const lowered = try self.lowerBindingPatternForRuntimeLayout(mir_param_id, param_layout, region);
        try param_infos.append(self.allocator, lowered);
        const rewrite = try self.rewriteTopLevelRestBinding(lowered, region);
        try param_rewrites.append(self.allocator, rewrite);
        try self.scratch_lir_pattern_ids.append(self.allocator, if (rewrite) |rw| rw.source_pattern else lowered.pattern);
    }
    const lir_params = try self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_param_patterns..]);
    const ret_layout = switch (monotype) {
        .func => |_| try self.runtimeValueLayoutFromMirExpr(lam.body),
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
            info.deferred_start,
            info.deferred_len,
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

/// Lower known annotation-only intrinsics by recognizing their monomorphic signatures.
/// Supported:
/// - a -> Box(a)      => box_box
/// - Box(a) -> a      => box_unbox
fn lowerAnnotationOnlyIntrinsicCall(
    self: *Self,
    call_data: anytype,
    mono_idx: Monotype.Idx,
    region: Region,
) Allocator.Error!?LirExprId {
    const func_mono_idx = self.mir_store.typeOf(call_data.func);
    const func_mono = self.mir_store.monotype_store.getMonotype(func_mono_idx);
    if (func_mono != .func) return null;

    const fn_args = self.mir_store.monotype_store.getIdxSpan(func_mono.func.args);
    if (fn_args.len != 1) return null;
    const arg_mono = fn_args[0];
    const ret_mono = func_mono.func.ret;

    const arg_ty = self.mir_store.monotype_store.getMonotype(arg_mono);
    const ret_ty = self.mir_store.monotype_store.getMonotype(ret_mono);

    const ll_op: ?LirExpr.LowLevel = blk: {
        if (ret_ty == .box) {
            if (try self.monotypesStructurallyEqual(arg_mono, ret_ty.box.inner)) break :blk .box_box;
        }
        if (arg_ty == .box) {
            if (try self.monotypesStructurallyEqual(arg_ty.box.inner, ret_mono)) break :blk .box_unbox;
        }
        break :blk null;
    };

    const op = ll_op orelse return null;

    const mir_args = self.mir_store.getExprSpan(call_data.args);
    if (mir_args.len != 1) return null;

    var acc = self.startLetAccumulator();
    const lir_args = try self.lowerAnfSpan(&acc, mir_args, region);
    const ret_layout = try self.layoutFromMonotype(mono_idx);
    const ll_expr = try self.lir_store.addExpr(.{ .low_level = .{
        .op = op,
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
        .record_destructure,
        .tuple_destructure,
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

    const payload_args = try self.lir_store.addExprSpan(&.{arg_lir_expr});
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
        for (mir_args) |mir_arg| {
            try self.scratch_layout_idxs.append(self.allocator, try self.runtimeValueLayoutFromMirExpr(mir_arg));
        }

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
                .ret_layout = ret_layout,
                .called_via = .apply,
            } }, region);
            return acc.finish(call_expr, ret_layout, region);
        }

        // Has captures: lower closure val and append as extra arg
        const closure_val_raw = try self.lowerExpr(call_data.func);
        const closure_layout = try self.runtimeValueLayoutFromMirExpr(call_data.func);
        const closure_val = try acc.ensureSymbol(closure_val_raw, closure_layout, region);
        try self.scratch_layout_idxs.append(self.allocator, closure_layout);
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
    const closure_val_raw = try self.lowerExpr(call_data.func);
    const closure_layout = try self.closureValueLayoutFromLambdaSet(ls_idx);
    const closure_val = try acc.ensureSymbol(closure_val_raw, closure_layout, region);

    const save_exprs = self.scratch_lir_expr_ids.items.len;
    defer self.scratch_lir_expr_ids.shrinkRetainingCapacity(save_exprs);

    for (members.items) |member| {
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
        for (mir_args) |mir_arg| {
            try self.scratch_layout_idxs.append(self.allocator, try self.runtimeValueLayoutFromMirExpr(mir_arg));
        }

        // Re-read on each iteration: lowering lifted defs can append to
        // extra_data and invalidate previously borrowed slices.
        const user_arg_ids = self.lir_store.getExprSpan(branch_user_args);
        const inner_save = self.scratch_lir_expr_ids.items.len;
        for (user_arg_ids) |arg_id| {
            try self.scratch_lir_expr_ids.append(self.allocator, arg_id);
        }

        if (!member.closure_member.isNone()) {
            // Extract payload (captures) from the tag union
            const captures_layout = try self.capturesLayoutForMember(member);
            const payload_expr = try self.lir_store.addExpr(.{ .tag_payload_access = .{
                .value = closure_val,
                .union_layout = closure_layout,
                .payload_layout = captures_layout,
            } }, region);
            try self.scratch_lir_expr_ids.append(self.allocator, payload_expr);
            try self.scratch_layout_idxs.append(self.allocator, captures_layout);
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

fn lowerBlock(self: *Self, block_data: anytype, _: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const LoweredStmtInfo = struct {
        pattern: LirPatternId = LirPatternId.none,
        deferred_start: usize = 0,
        deferred_len: usize = 0,
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
        const runtime_layout = try self.runtimeValueLayoutFromMirExpr(binding.expr);
        try self.registerBindingPatternSymbols(binding.pattern, runtime_layout);
        switch (stmt) {
            .decl_const => {
                const lowered = try self.lowerBindingPatternForRuntimeLayout(binding.pattern, runtime_layout, region);
                try binding_infos.append(self.allocator, .{
                    .pattern = lowered.pattern,
                    .deferred_start = lowered.deferred_start,
                    .deferred_len = lowered.deferred_len,
                });
                try self.scratch_lir_pattern_ids.append(self.allocator, lowered.pattern);
            },
            .decl_var, .mutate_var => {
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
        }
    }

    // Pass 2: Lower expressions and assemble statements using cached patterns.
    for (mir_stmts, 0..) |stmt, i| {
        const binding = switch (stmt) {
            .decl_const, .decl_var, .mutate_var => |b| b,
        };
        const lir_expr = try self.lowerExpr(binding.expr);
        if (stmt == .decl_const) {
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
                    .record_destructure,
                    .tuple_destructure,
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
                };
                try self.scratch_lir_stmts.append(self.allocator, .{ .decl = lir_binding });
                try self.appendDeferredListRestBindingDecls(
                    binding_infos.items[i].deferred_start,
                    binding_infos.items[i].deferred_len,
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
    const lir_final = try self.lowerExpr(block_data.final_expr);

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

    const save_len = self.scratch_lir_borrow_bindings.items.len;
    const save_rest_bindings = self.scratch_deferred_list_rest_bindings.items.len;
    var binding_infos = std.ArrayList(LoweredBindingPattern).empty;
    defer binding_infos.deinit(self.allocator);
    defer self.scratch_lir_borrow_bindings.shrinkRetainingCapacity(save_len);
    defer self.scratch_deferred_list_rest_bindings.shrinkRetainingCapacity(save_rest_bindings);

    for (mir_bindings) |binding| {
        const runtime_layout = try self.runtimeValueLayoutFromMirExpr(binding.expr);
        try self.registerBindingPatternSymbols(binding.pattern, runtime_layout);
        const lowered = try self.lowerBindingPatternForRuntimeLayout(binding.pattern, runtime_layout, region);
        const lir_expr = try self.lowerExpr(binding.expr);
        try binding_infos.append(self.allocator, lowered);
        try self.scratch_lir_borrow_bindings.append(self.allocator, .{
            .pattern = lowered.pattern,
            .expr = lir_expr,
        });
    }

    const lir_bindings = try self.lir_store.addBorrowBindings(self.scratch_lir_borrow_bindings.items[save_len..]);
    var lir_body = try self.lowerExpr(scope_data.body);
    var borrow_binding_idx = binding_infos.items.len;
    while (borrow_binding_idx > 0) {
        borrow_binding_idx -= 1;
        const info = binding_infos.items[borrow_binding_idx];
        lir_body = try self.wrapExprWithDeferredListRestBindings(
            lir_body,
            result_layout,
            info.deferred_start,
            info.deferred_len,
            region,
        );
    }
    // This temp must survive cleanup emitted after the scope body, so give it
    // stable storage rather than letting later codegen keep it only in a register.
    const result_bind = try self.freshBindPattern(result_layout, false, region);

    return self.lir_store.addExpr(.{ .borrow_scope = .{
        .bindings = lir_bindings,
        .result_symbol = result_bind.symbol,
        .result_pattern = result_bind.pattern,
        .body = lir_body,
        .result_layout = result_layout,
    } }, region);
}

fn lowerBindingPatternForExpr(self: *Self, mir_pat_id: MIR.PatternId, mir_expr_id: MIR.ExprId) Allocator.Error!LirPatternId {
    const runtime_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const lowered = try self.lowerBindingPatternForRuntimeLayout(mir_pat_id, runtime_layout, Region.zero());
    if (builtin.mode == .Debug and lowered.deferred_len != 0) {
        std.debug.panic("MirToLir invariant violated: binding rest patterns must be expanded by the caller", .{});
    }
    return lowered.pattern;
}

fn lowerRecordAccess(self: *Self, ra: anytype, _: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
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
    const struct_layout = try self.runtimeValueLayoutFromMirExpr(ra.record);
    const lir_struct = try acc.ensureSymbol(lir_struct_raw, struct_layout, region);
    var field_layout: ?layout.Idx = null;
    var field_idx: ?u16 = null;
    const struct_layout_val = self.layout_store.getLayout(struct_layout);
    if (struct_layout_val.tag == .struct_) {
        const struct_data = self.layout_store.getStructData(struct_layout_val.data.struct_.idx);
        const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
        for (0..layout_fields.len) |li| {
            if (self.identsTextEqual(layout_fields.get(li).name, ra.field_name)) {
                field_idx = @intCast(li);
                field_layout = layout_fields.get(li).layout;
                break;
            }
        }
    }
    const resolved_field_idx = field_idx orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "MirToLir invariant violated: record_access field ident idx {d} missing from runtime layout {d} (tag={s})",
                .{ ra.field_name.idx, @intFromEnum(struct_layout), @tagName(struct_layout_val.tag) },
            );
        }
        unreachable;
    };

    const resolved_field_layout = field_layout orelse unreachable;

    const access_expr = try self.lir_store.addExpr(.{ .struct_access = .{
        .struct_expr = lir_struct,
        .struct_layout = struct_layout,
        .field_layout = resolved_field_layout,
        .field_idx = resolved_field_idx,
    } }, region);
    return acc.finish(access_expr, resolved_field_layout, region);
}

fn lowerTupleAccess(self: *Self, ta: anytype, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    var acc = self.startLetAccumulator();
    const lir_struct_raw = try self.lowerExpr(ta.tuple);
    const struct_layout = try self.runtimeValueLayoutFromMirExpr(ta.tuple);
    const lir_struct = try acc.ensureSymbol(lir_struct_raw, struct_layout, region);
    var field_layout: layout.Idx = undefined;
    var field_layout_found = false;

    // Find the sorted field index for this tuple element's original index.
    var field_idx: ?u16 = null;
    const struct_layout_val = self.layout_store.getLayout(struct_layout);
    if (struct_layout_val.tag == .struct_) {
        const struct_data = self.layout_store.getStructData(struct_layout_val.data.struct_.idx);
        const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
        for (0..layout_fields.len) |li| {
            if (layout_fields.get(li).index == ta.elem_index) {
                field_idx = @intCast(li);
                field_layout = layout_fields.get(li).layout;
                field_layout_found = true;
                break;
            }
        }
    } else {
        // Single-element tuple optimized to non-struct layout; field_idx 0
        field_idx = 0;
        field_layout = struct_layout;
        field_layout_found = true;
    }
    if (!field_layout_found) {
        field_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    }

    const access_expr = try self.lir_store.addExpr(.{ .struct_access = .{
        .struct_expr = lir_struct,
        .struct_layout = struct_layout,
        .field_layout = field_layout,
        .field_idx = field_idx orelse unreachable,
    } }, region);
    return acc.finish(access_expr, field_layout, region);
}

fn lowerLowLevel(self: *Self, ll: anytype, mir_expr_id: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const ret_layout = try self.runtimeValueLayoutFromMirExpr(mir_expr_id);
    const mir_args = self.mir_store.getExprSpan(ll.args);
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

    const save_borrow_len = self.scratch_lir_borrow_bindings.items.len;
    defer self.scratch_lir_borrow_bindings.shrinkRetainingCapacity(save_borrow_len);

    for (mir_args, 0..) |mir_arg, i| {
        const lowered_arg = try self.lowerExpr(mir_arg);
        const arg_layout = try self.layoutFromMonotype(self.mir_store.typeOf(mir_arg));
        const ownership = arg_ownership[i];

        const ensured_arg = switch (ownership) {
            .borrow => blk: {
                if (self.isBorrowAtomicExpr(lowered_arg)) break :blk lowered_arg;

                const borrow_bind = try self.freshBindPattern(arg_layout, false, region);
                try self.scratch_lir_borrow_bindings.append(self.allocator, .{
                    .pattern = borrow_bind.pattern,
                    .expr = lowered_arg,
                });

                break :blk try self.lir_store.addExpr(.{ .lookup = .{
                    .symbol = borrow_bind.symbol,
                    .layout_idx = arg_layout,
                } }, region);
            },
            .consume => try acc.ensureSymbol(lowered_arg, arg_layout, region),
        };

        try self.scratch_lir_expr_ids.append(self.allocator, ensured_arg);
    }

    const lir_args = try self.lir_store.addExprSpan(self.scratch_lir_expr_ids.items[save_expr_len..]);

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
            break :blk try self.lir_store.addExpr(.{ .semantic_low_level = .{
                .op = ll.op,
                .args = lir_args,
                .ret_layout = ret_layout,
            } }, region);
        },
    };
    const final_result = if (self.scratch_lir_borrow_bindings.items.len == save_borrow_len)
        result
    else blk: {
        const borrow_bindings = try self.lir_store.addBorrowBindings(self.scratch_lir_borrow_bindings.items[save_borrow_len..]);
        const result_bind = try self.freshBindPattern(ret_layout, false, region);
        break :blk try self.lir_store.addExpr(.{ .borrow_scope = .{
            .bindings = borrow_bindings,
            .result_symbol = result_bind.symbol,
            .result_pattern = result_bind.pattern,
            .body = result,
            .result_layout = ret_layout,
        } }, region);
    };

    return acc.finish(final_result, ret_layout, region);
}

/// Emit a zero literal matching the type of the given MIR expression.
/// For float/dec/i128 types, emits the correct typed zero instead of i64.
fn emitZeroLiteral(self: *Self, mir_expr: MIR.ExprId, region: Region) Allocator.Error!LirExprId {
    const arg_mono_idx = self.mir_store.typeOf(mir_expr);
    const arg_layout_idx = try self.layoutFromMonotype(arg_mono_idx);
    const arg_monotype = self.mir_store.monotype_store.getMonotype(arg_mono_idx);
    return self.lir_store.addExpr(switch (arg_monotype) {
        .prim => |p| switch (p) {
            .f32 => LirExpr{ .f32_literal = 0.0 },
            .f64 => LirExpr{ .f64_literal = 0.0 },
            .dec => LirExpr{ .dec_literal = 0 },
            .i128, .u128 => LirExpr{ .i128_literal = .{ .value = 0, .layout_idx = arg_layout_idx } },
            .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64 => LirExpr{ .i64_literal = .{ .value = 0, .layout_idx = arg_layout_idx } },
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
        const lowered = try self.lowerBindingPatternForRuntimeLayout(mir_param_id, param_layout, region);
        try param_infos.append(self.allocator, lowered);
        const rewrite = try self.rewriteTopLevelRestBinding(lowered, region);
        try param_rewrites.append(self.allocator, rewrite);
        try self.scratch_lir_pattern_ids.append(self.allocator, if (rewrite) |rw| rw.source_pattern else lowered.pattern);
    }
    const lir_params = try self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_param_patterns..]);

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
            info.deferred_start,
            info.deferred_len,
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
    const list_layout = try self.runtimeValueLayoutFromMirExpr(f.list);
    const lir_list_raw = try self.lowerExpr(f.list);
    const save_borrow_len = self.scratch_lir_borrow_bindings.items.len;
    defer self.scratch_lir_borrow_bindings.shrinkRetainingCapacity(save_borrow_len);

    const lir_list = if (self.isBorrowAtomicExpr(lir_list_raw))
        lir_list_raw
    else blk: {
        const borrow_bind = try self.freshBindPattern(list_layout, false, region);
        try self.scratch_lir_borrow_bindings.append(self.allocator, .{
            .pattern = borrow_bind.pattern,
            .expr = lir_list_raw,
        });

        break :blk try self.lir_store.addExpr(.{ .lookup = .{
            .symbol = borrow_bind.symbol,
            .layout_idx = list_layout,
        } }, region);
    };

    const elem_layout = try self.runtimeListElemLayoutFromMirExpr(f.list);
    const save_rest_bindings = self.scratch_deferred_list_rest_bindings.items.len;
    defer self.scratch_deferred_list_rest_bindings.shrinkRetainingCapacity(save_rest_bindings);
    try self.registerBindingPatternSymbols(f.elem_pattern, elem_layout);
    const lowered_pat = try self.lowerBindingPatternForRuntimeLayout(f.elem_pattern, elem_layout, region);
    const elem_rewrite = try self.rewriteTopLevelRestBinding(lowered_pat, region);
    const raw_body = try self.lowerExpr(f.body);
    const lir_body = try self.wrapExprWithTopLevelRestBindingPrelude(
        raw_body,
        .zst,
        elem_rewrite,
        lowered_pat.deferred_start,
        lowered_pat.deferred_len,
        region,
    );

    const for_expr = try self.lir_store.addExpr(.{ .for_loop = .{
        .list_expr = lir_list,
        .elem_layout = elem_layout,
        .elem_pattern = if (elem_rewrite) |rw| rw.source_pattern else lowered_pat.pattern,
        .body = lir_body,
    } }, region);

    if (self.scratch_lir_borrow_bindings.items.len == save_borrow_len) {
        return for_expr;
    }

    const borrow_bindings = try self.lir_store.addBorrowBindings(self.scratch_lir_borrow_bindings.items[save_borrow_len..]);
    const result_bind = try self.freshBindPattern(.zst, false, region);
    return self.lir_store.addExpr(.{ .borrow_scope = .{
        .bindings = borrow_bindings,
        .result_symbol = result_bind.symbol,
        .result_pattern = result_bind.pattern,
        .body = for_expr,
        .result_layout = .zst,
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
    var layout_idx = self.symbol_layouts.get(sym.raw()) orelse fallback_layout;
    const mono = self.mir_store.monotype_store.getMonotype(mono_idx);
    const has_callable_def = if (self.mir_store.getSymbolDef(sym)) |def_id|
        LambdaSet.isLambdaExpr(self.mir_store, def_id)
    else
        false;

    if (mono == .func and !has_callable_def) {
        if (self.lambda_set_store.getSymbolLambdaSet(sym)) |ls_idx| {
            layout_idx = try self.closureValueLayoutFromLambdaSet(ls_idx);
        }
    }

    return layout_idx;
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

    if (self.isSingleTagUnion(mono_idx)) {
        if (arg_count == 1) return union_runtime_layout;

        const tuple_layout = self.layout_store.getLayout(union_runtime_layout);
        if (builtin.mode == .Debug and tuple_layout.tag != .struct_) {
            std.debug.panic(
                "MirToLir invariant violated: single-tag payload runtime layout must be struct_ for multi-arg pattern, got {s}",
                .{@tagName(tuple_layout.tag)},
            );
        }
        return self.layout_store.getStructFieldLayoutByOriginalIndex(tuple_layout.data.struct_.idx, @intCast(arg_index));
    }

    const union_layout = self.layout_store.getLayout(union_runtime_layout);
    if (builtin.mode == .Debug and union_layout.tag != .tag_union) {
        std.debug.panic(
            "MirToLir invariant violated: tag-pattern runtime layout must be tag_union, got {s}",
            .{@tagName(union_layout.tag)},
        );
    }

    const discriminant = self.tagDiscriminant(tag_name, mono_idx);
    const tu_data = self.layout_store.getTagUnionData(union_layout.data.tag_union.idx);
    const variants = self.layout_store.getTagUnionVariants(tu_data);
    if (builtin.mode == .Debug and discriminant >= variants.len) {
        std.debug.panic(
            "MirToLir invariant violated: tag discriminant {d} out of bounds for runtime tag_union layout",
            .{discriminant},
        );
    }

    const payload_layout = variants.get(discriminant).payload_layout;
    if (arg_count == 1) return payload_layout;

    const payload_layout_val = self.layout_store.getLayout(payload_layout);
    if (builtin.mode == .Debug and payload_layout_val.tag != .struct_) {
        std.debug.panic(
            "MirToLir invariant violated: multi-arg tag payload runtime layout must be struct_, got {s}",
            .{@tagName(payload_layout_val.tag)},
        );
    }

    return self.layout_store.getStructFieldLayoutByOriginalIndex(payload_layout_val.data.struct_.idx, @intCast(arg_index));
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
            const arg_patterns = self.mir_store.getPatternSpan(tag_pat.args);
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
        .record_destructure => |rd| {
            const mir_patterns = self.mir_store.getPatternSpan(rd.destructs);
            const mir_field_names = self.mir_store.getFieldNameSpan(rd.field_names);
            const mono = switch (self.mir_store.monotype_store.getMonotype(mono_idx)) {
                .record => |r| r,
                else => unreachable,
            };
            const all_fields = self.mir_store.monotype_store.getFields(mono.fields);

            if (all_fields.len == 1) {
                if (mir_patterns.len != 0) {
                    try self.registerBindingPatternSymbols(mir_patterns[0], runtime_layout);
                }
                return;
            }

            const record_layout_val = self.layout_store.getLayout(runtime_layout);
            if (record_layout_val.tag == .struct_) {
                const record_data = self.layout_store.getStructData(record_layout_val.data.struct_.idx);
                const layout_fields = self.layout_store.struct_fields.sliceRange(record_data.getFields());

                for (0..layout_fields.len) |li| {
                    const layout_field_name = layout_fields.get(li).name;
                    for (mir_field_names, 0..) |mi_name, mi| {
                        if (!self.identsTextEqual(mi_name, layout_field_name)) continue;
                        try self.registerBindingPatternSymbols(mir_patterns[mi], layout_fields.get(li).layout);
                        break;
                    }
                }
            }
        },
        .tuple_destructure => |td| {
            const elem_patterns = self.mir_store.getPatternSpan(td.elems);
            const tuple_layout_val = self.layout_store.getLayout(runtime_layout);
            if (tuple_layout_val.tag == .struct_) {
                const struct_data = self.layout_store.getStructData(tuple_layout_val.data.struct_.idx);
                const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
                for (0..layout_fields.len) |li| {
                    const original_index = layout_fields.get(li).index;
                    try self.registerBindingPatternSymbols(elem_patterns[original_index], layout_fields.get(li).layout);
                }
            } else if (elem_patterns.len == 1) {
                try self.registerBindingPatternSymbols(elem_patterns[0], runtime_layout);
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
    region: Region,
) Allocator.Error!LoweredBindingPattern {
    const save_len = self.scratch_deferred_list_rest_bindings.items.len;
    const pattern = try self.lowerPatternInternal(mir_pat_id, runtime_layout, true, region);
    return .{
        .pattern = pattern,
        .deferred_start = save_len,
        .deferred_len = self.scratch_deferred_list_rest_bindings.items.len - save_len,
    };
}

fn lowerPatternInternal(
    self: *Self,
    mir_pat_id: MIR.PatternId,
    runtime_layout: layout.Idx,
    collect_rest_bindings: bool,
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
            break :blk self.lir_store.addPattern(.{ .bind = .{
                .symbol = sym,
                .layout_idx = layout_idx,
                .reassignable = reassignable,
            } }, region);
        },
        .wildcard => self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = runtime_layout } }, region),
        .tag => |t| blk: {
            const mir_pat_args = self.mir_store.getPatternSpan(t.args);

            if (self.isSingleTagUnion(mono_idx)) {
                if (mir_pat_args.len == 0) {
                    break :blk self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = runtime_layout } }, region);
                } else if (mir_pat_args.len == 1) {
                    break :blk self.lowerPatternInternal(mir_pat_args[0], runtime_layout, collect_rest_bindings, region);
                } else {
                    const tuple_layout = try self.layoutFromMonotype(mono_idx);
                    const tuple_layout_val = self.layout_store.getLayout(tuple_layout);
                    if (tuple_layout_val.tag == .struct_) {
                        const struct_data = self.layout_store.getStructData(tuple_layout_val.data.struct_.idx);
                        const layout_fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
                        const save_len = self.scratch_lir_pattern_ids.items.len;
                        defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);

                        for (0..layout_fields.len) |li| {
                            const original_index = layout_fields.get(li).index;
                            const lir_pat = try self.lowerPatternInternal(
                                mir_pat_args[original_index],
                                layout_fields.get(li).layout,
                                collect_rest_bindings,
                                region,
                            );
                            try self.scratch_lir_pattern_ids.append(self.allocator, lir_pat);
                        }

                        const lir_elems = try self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_len..]);
                        break :blk self.lir_store.addPattern(.{ .struct_ = .{
                            .struct_layout = tuple_layout,
                            .fields = lir_elems,
                        } }, region);
                    } else {
                        const lir_elems = try self.lowerPatternSpanInternal(mir_pat_args, collect_rest_bindings, region);
                        break :blk self.lir_store.addPattern(.{ .struct_ = .{
                            .struct_layout = tuple_layout,
                            .fields = lir_elems,
                        } }, region);
                    }
                }
            }

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
                const lir_pat = try self.lowerPatternInternal(mir_arg_pat, arg_layout, collect_rest_bindings, region);
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
        .record_destructure => |rd| blk: {
            const record_layout = runtime_layout;

            const mir_patterns = self.mir_store.getPatternSpan(rd.destructs);
            const mir_field_names = self.mir_store.getFieldNameSpan(rd.field_names);
            const mono = switch (self.mir_store.monotype_store.getMonotype(mono_idx)) {
                .record => |r| r,
                else => unreachable,
            };
            const all_fields = self.mir_store.monotype_store.getFields(mono.fields);

            if (all_fields.len == 1) {
                if (mir_patterns.len == 0) {
                    break :blk self.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = record_layout } }, region);
                }
                break :blk try self.lowerPatternInternal(
                    mir_patterns[0],
                    record_layout,
                    collect_rest_bindings,
                    region,
                );
            }

            const record_layout_val = self.layout_store.getLayout(record_layout);

            if (record_layout_val.tag == .struct_) {
                const record_data = self.layout_store.getStructData(record_layout_val.data.struct_.idx);
                const layout_fields = self.layout_store.struct_fields.sliceRange(record_data.getFields());
                const save_len = self.scratch_lir_pattern_ids.items.len;
                defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);

                for (0..layout_fields.len) |li| {
                    const layout_field_name = layout_fields.get(li).name;
                    var found = false;
                    for (mir_field_names, 0..) |mir_name, mi| {
                        if (self.identsTextEqual(mir_name, layout_field_name)) {
                            const lir_pat = try self.lowerPatternInternal(
                                mir_patterns[mi],
                                layout_fields.get(li).layout,
                                collect_rest_bindings,
                                region,
                            );
                            try self.scratch_lir_pattern_ids.append(self.allocator, lir_pat);
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
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
                const lir_fields = try self.lowerPatternSpanInternal(mir_patterns, collect_rest_bindings, region);
                break :blk self.lir_store.addPattern(.{ .struct_ = .{
                    .struct_layout = record_layout,
                    .fields = lir_fields,
                } }, region);
            }
        },
        .tuple_destructure => |td| blk: {
            const struct_layout = runtime_layout;
            const mir_patterns = self.mir_store.getPatternSpan(td.elems);
            const struct_layout_val = self.layout_store.getLayout(struct_layout);

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
                        region,
                    );
                    try self.scratch_lir_pattern_ids.append(self.allocator, lir_pat);
                }

                const lir_fields = try self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_len..]);
                break :blk self.lir_store.addPattern(.{ .struct_ = .{
                    .struct_layout = struct_layout,
                    .fields = lir_fields,
                } }, region);
            } else {
                const lir_elems = try self.lowerPatternSpanInternal(mir_patterns, collect_rest_bindings, region);
                break :blk self.lir_store.addPattern(.{ .struct_ = .{
                    .struct_layout = struct_layout,
                    .fields = lir_elems,
                } }, region);
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

            const rest_pat: LirPatternId = if (!has_rest) rest_blk: {
                break :rest_blk LirPatternId.none;
            } else if (ld.rest_pattern.isNone()) rest_blk: {
                break :rest_blk try self.lir_store.addPattern(.{ .wildcard = .{
                    .layout_idx = list_layout,
                } }, region);
            } else if (!collect_rest_bindings) {
                if (builtin.mode == .Debug) {
                    std.debug.panic("MirToLir invariant violated: bound list rest pattern must be lowered as an explicit binding", .{});
                }
                unreachable;
            } else rest_blk: {
                const source_symbol = self.freshSymbol();
                try self.scratch_deferred_list_rest_bindings.append(self.allocator, .{
                    .source_symbol = source_symbol,
                    .list_layout = list_layout,
                    .target_pattern = ld.rest_pattern,
                    .prefix_count = if (ld.rest_index.isNone()) 0 else @intFromEnum(ld.rest_index),
                    .suffix_count = if (ld.rest_index.isNone()) 0 else @as(u32, @intCast(all_patterns.len - @as(usize, @intFromEnum(ld.rest_index)))),
                });
                break :rest_blk try self.lir_store.addPattern(.{ .wildcard = .{
                    .layout_idx = list_layout,
                } }, region);
            };

            const list_pattern = if (ld.rest_index.isNone()) blk2: {
                const lir_prefix = try self.lowerPatternSpanWithLayoutInternal(all_patterns, elem_layout, collect_rest_bindings, region);
                break :blk2 try self.lir_store.addPattern(.{ .list = .{
                    .list_layout = list_layout,
                    .elem_layout = elem_layout,
                    .prefix = lir_prefix,
                    .rest = rest_pat,
                    .suffix = LirPatternSpan.empty(),
                } }, region);
            } else blk2: {
                const rest_idx: u32 = @intFromEnum(ld.rest_index);
                const lir_prefix = try self.lowerPatternSpanWithLayoutInternal(all_patterns[0..rest_idx], elem_layout, collect_rest_bindings, region);
                const lir_suffix = try self.lowerPatternSpanWithLayoutInternal(all_patterns[rest_idx..], elem_layout, collect_rest_bindings, region);
                break :blk2 try self.lir_store.addPattern(.{ .list = .{
                    .list_layout = list_layout,
                    .elem_layout = elem_layout,
                    .prefix = lir_prefix,
                    .rest = rest_pat,
                    .suffix = lir_suffix,
                } }, region);
            };

            if (ld.rest_pattern.isNone()) break :blk list_pattern;

            const synthetic_sym_key: u64 = @bitCast(self.scratch_deferred_list_rest_bindings.items[self.scratch_deferred_list_rest_bindings.items.len - 1].source_symbol);
            try self.symbol_layouts.put(synthetic_sym_key, list_layout);
            break :blk try self.lir_store.addPattern(.{ .as_pattern = .{
                .symbol = self.scratch_deferred_list_rest_bindings.items[self.scratch_deferred_list_rest_bindings.items.len - 1].source_symbol,
                .layout_idx = list_layout,
                .reassignable = false,
                .inner = list_pattern,
            } }, region);
        },
        .as_pattern => |ap| blk: {
            const layout_idx = try self.runtimeLayoutForBindingSymbol(ap.symbol, mono_idx, runtime_layout);
            const inner = try self.lowerPatternInternal(ap.pattern, runtime_layout, collect_rest_bindings, region);
            const reassignable = self.mir_store.isSymbolReassignable(ap.symbol);
            const sym_key: u64 = @bitCast(ap.symbol);
            try self.symbol_layouts.put(sym_key, layout_idx);
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

fn lowerPatternSpanInternal(
    self: *Self,
    mir_pat_ids: []const MIR.PatternId,
    collect_rest_bindings: bool,
    region: Region,
) Allocator.Error!LirPatternSpan {
    const save_len = self.scratch_lir_pattern_ids.items.len;
    defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);
    for (mir_pat_ids) |mir_id| {
        const pat_layout = try self.layoutFromMonotype(self.mir_store.patternTypeOf(mir_id));
        const lir_id = try self.lowerPatternInternal(mir_id, pat_layout, collect_rest_bindings, region);
        try self.scratch_lir_pattern_ids.append(self.allocator, lir_id);
    }
    return self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_len..]);
}

fn lowerPatternSpanWithLayoutInternal(
    self: *Self,
    mir_pat_ids: []const MIR.PatternId,
    runtime_layout: layout.Idx,
    collect_rest_bindings: bool,
    region: Region,
) Allocator.Error!LirPatternSpan {
    const save_len = self.scratch_lir_pattern_ids.items.len;
    defer self.scratch_lir_pattern_ids.shrinkRetainingCapacity(save_len);
    for (mir_pat_ids) |mir_id| {
        const lir_id = try self.lowerPatternInternal(mir_id, runtime_layout, collect_rest_bindings, region);
        try self.scratch_lir_pattern_ids.append(self.allocator, lir_id);
    }
    return self.lir_store.addPatternSpan(self.scratch_lir_pattern_ids.items[save_len..]);
}

fn lowerPattern(self: *Self, mir_pat_id: MIR.PatternId) Allocator.Error!LirPatternId {
    return self.lowerPatternInternal(
        mir_pat_id,
        try self.layoutFromMonotype(self.mir_store.patternTypeOf(mir_pat_id)),
        false,
        Region.zero(),
    );
}

fn lowerPatternSpan(self: *Self, mir_pat_ids: []const MIR.PatternId) Allocator.Error!LirPatternSpan {
    return self.lowerPatternSpanInternal(mir_pat_ids, false, Region.zero());
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
        try self.registerBindingPatternSymbols(binding.target_pattern, binding.list_layout);
        const lowered = try self.lowerBindingPatternForRuntimeLayout(binding.target_pattern, binding.list_layout, region);
        try self.scratch_lir_stmts.append(self.allocator, .{ .decl = .{
            .pattern = lowered.pattern,
            .expr = binding_expr,
        } });
        try self.appendDeferredListRestBindingDecls(lowered.deferred_start, lowered.deferred_len, region);
    }
}

fn rewriteTopLevelRestBinding(
    self: *Self,
    lowered: LoweredBindingPattern,
    region: Region,
) Allocator.Error!?TopLevelRestBindingRewrite {
    if (lowered.deferred_len == 0) return null;
    const pat = self.lir_store.getPattern(lowered.pattern);
    if (pat != .as_pattern) return null;

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

fn wrapExprWithTopLevelRestBindingPrelude(
    self: *Self,
    body: LirExprId,
    body_layout: layout.Idx,
    rewrite: ?TopLevelRestBindingRewrite,
    deferred_start: usize,
    deferred_len: usize,
    region: Region,
) Allocator.Error!LirExprId {
    if (rewrite == null and deferred_len == 0) return body;

    const save_len = self.scratch_lir_stmts.items.len;
    defer self.scratch_lir_stmts.shrinkRetainingCapacity(save_len);

    if (rewrite) |binding_rewrite| {
        const source_lookup = try self.lir_store.addExpr(.{ .lookup = .{
            .symbol = binding_rewrite.source_symbol,
            .layout_idx = binding_rewrite.source_layout,
        } }, region);
        try self.scratch_lir_stmts.append(self.allocator, .{ .decl = .{
            .pattern = binding_rewrite.destructure_pattern,
            .expr = source_lookup,
        } });
    }

    try self.appendDeferredListRestBindingDecls(deferred_start, deferred_len, region);
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
    deferred_start: usize,
    deferred_len: usize,
    region: Region,
) Allocator.Error!LirExprId {
    if (deferred_len == 0) return body;

    const save_len = self.scratch_lir_stmts.items.len;
    defer self.scratch_lir_stmts.shrinkRetainingCapacity(save_len);

    try self.appendDeferredListRestBindingDecls(deferred_start, deferred_len, region);
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
    try testing.expectEqual(@as(i64, 42), def_expr.i64_literal.value);
    try testing.expectEqual(layout.Idx.i64, def_expr.i64_literal.layout_idx);
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
    try testing.expectEqual(@as(i64, 42), lir_expr.i64_literal.value);
    try testing.expectEqual(layout.Idx.i64, lir_expr.i64_literal.layout_idx);
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

test "MIR num_plus low-level lowers to semantic low-level" {
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

    try testing.expect(lir_expr == .semantic_low_level);
    try testing.expect(lir_expr.semantic_low_level.op == .num_plus);
}

test "MIR num_is_zero with f64 operand emits f64 zero literal" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const f64_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.f64)];
    const bool_mono = try env.mir_store.monotype_store.addBoolTagUnion(allocator, env.module_env.idents);

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
    const bool_mono = try env.mir_store.monotype_store.addBoolTagUnion(allocator, env.module_env.idents);

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
    try testing.expectEqual(layout.Idx.i128, rhs_expr.i128_literal.layout_idx);
}

test "borrowed low-level temp arg lowers through LIR borrow_scope" {
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

    try testing.expect(lir_expr == .borrow_scope);
    const bindings = env.lir_store.getBorrowBindings(lir_expr.borrow_scope.bindings);
    try testing.expectEqual(@as(usize, 1), bindings.len);
    try testing.expect(env.lir_store.getExpr(bindings[0].expr) == .list);

    const body = env.lir_store.getExpr(lir_expr.borrow_scope.body);
    try testing.expect(body == .semantic_low_level);
    try testing.expect(body.semantic_low_level.op == .list_get_unsafe);
}

test "borrowed low-level large string literal lowers through LIR borrow_scope" {
    const allocator = testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_mono = env.mir_store.monotype_store.prim_idxs[@intFromEnum(Monotype.Prim.str)];
    const bool_mono = try env.mir_store.monotype_store.addBoolTagUnion(allocator, env.module_env.idents);

    const large_text =
        "This string is deliberately longer than RocStr small-string storage";
    const str_idx = try env.mir_store.strings.insert(allocator, large_text);
    const str_expr = try env.mir_store.addExpr(allocator, .{ .str = str_idx }, str_mono, Region.zero());
    const args = try env.mir_store.addExprSpan(allocator, &.{str_expr});
    const ll_expr = try env.mir_store.addExpr(allocator, .{ .run_low_level = .{
        .op = .str_is_empty,
        .args = args,
    } }, bool_mono, Region.zero());

    var translator = Self.init(allocator, &env.mir_store, &env.lir_store, &env.layout_store, &env.lambda_set_store, env.module_env.idents.true_tag);
    defer translator.deinit();

    const lir_id = try translator.lower(ll_expr);
    const lir_expr = env.lir_store.getExpr(lir_id);

    try testing.expect(lir_expr == .borrow_scope);
    const bindings = env.lir_store.getBorrowBindings(lir_expr.borrow_scope.bindings);
    try testing.expectEqual(@as(usize, 1), bindings.len);
    try testing.expect(env.lir_store.getExpr(bindings[0].expr) == .str_literal);

    const body = env.lir_store.getExpr(lir_expr.borrow_scope.body);
    try testing.expect(body == .semantic_low_level);
    try testing.expect(body.semantic_low_level.op == .str_is_empty);
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

    try testing.expect(lir_expr == .semantic_low_level);
    try testing.expect(lir_expr.semantic_low_level.op == .list_get_unsafe);
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
