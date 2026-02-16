//! LIR-level Reference Counting Insertion Pass
//!
//! This pass walks LirExpr trees after lowering from CIR and inserts
//! `incref`, `decref`, and `free` operations based on Perceus-inspired
//! ownership tracking.
//!
//! Key insight: process code bottom-up (continuation-first), tracking which
//! symbols are consumed. Insert increfs for multi-use and decrefs for last-use.
//!
//! This operates on LirExprStore where every expression carries a concrete
//! `layout_idx`, eliminating the type-variable corruption issues that
//! plagued the previous CIR-level RC pass.
//!
//! ## Known limitation: global use counts
//!
//! Use counts are global across the entire expression tree, not scoped to
//! control flow. A symbol used once in each branch of an `if` gets count=2,
//! causing an unnecessary `incref` (only one branch executes at runtime).
//! This is conservative (leak-safe, never frees too early) but not optimal —
//! it can cause memory to be retained longer than necessary. A full fix
//! requires per-scope tracking (Perceus-style).

const std = @import("std");
const Allocator = std.mem.Allocator;
const layout_mod = @import("layout");
const base = @import("base");
const LIR = @import("LIR.zig");
const LirExprStore = @import("LirExprStore.zig");

const LirExprId = LIR.LirExprId;
const Symbol = LIR.Symbol;
const LirStmt = LIR.LirStmt;
const LirStmtSpan = LIR.LirStmtSpan;
const LirPatternId = LIR.LirPatternId;
const LirIfBranch = LIR.LirIfBranch;
const LirWhenBranch = LIR.LirWhenBranch;
const LayoutIdx = layout_mod.Idx;
const Region = base.Region;

/// Inserts reference counting operations (incref/decref) into the mono IR.
pub const RcInsertPass = struct {
    allocator: Allocator,
    store: *LirExprStore,
    layout_store: *const layout_mod.Store,

    /// Tracks how many times each symbol is used in the expression tree.
    /// Keyed by the raw u64 representation of Symbol.
    symbol_use_counts: std.AutoHashMap(u64, u32),

    /// Tracks layout for each symbol (for generating incref/decref with correct layout).
    symbol_layouts: std.AutoHashMap(u64, LayoutIdx),

    /// Temporary buffer for building statement lists.
    stmt_buf: std.ArrayList(LirStmt),

    pub fn init(allocator: Allocator, store: *LirExprStore, layout_store: *const layout_mod.Store) RcInsertPass {
        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .symbol_use_counts = std.AutoHashMap(u64, u32).init(allocator),
            .symbol_layouts = std.AutoHashMap(u64, LayoutIdx).init(allocator),
            .stmt_buf = std.ArrayList(LirStmt).empty,
        };
    }

    pub fn deinit(self: *RcInsertPass) void {
        self.symbol_use_counts.deinit();
        self.symbol_layouts.deinit();
        self.stmt_buf.deinit(self.allocator);
    }

    /// Main entry point: insert RC operations into a LirExpr tree.
    /// Returns a new LirExprId for the RC-annotated tree.
    pub fn insertRcOps(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!LirExprId {
        // Phase 1: Count all symbol uses and record layouts
        try self.countUses(expr_id);

        // Phase 2: Walk the tree and insert RC operations
        return self.processExpr(expr_id);
    }

    /// Count how many times each symbol is referenced in the expression tree.
    /// Also records the layout for each symbol found in bind patterns.
    fn countUses(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!void {
        if (expr_id.isNone()) return;

        const expr = self.store.getExpr(expr_id);
        switch (expr) {
            .lookup => |lookup| {
                if (!lookup.symbol.isNone()) {
                    const key = @as(u64, @bitCast(lookup.symbol));
                    const gop = try self.symbol_use_counts.getOrPut(key);
                    if (gop.found_existing) {
                        gop.value_ptr.* += 1;
                    } else {
                        gop.value_ptr.* = 1;
                    }
                    if (!self.symbol_layouts.contains(key)) {
                        try self.symbol_layouts.put(key, lookup.layout_idx);
                    }
                }
            },
            .block => |block| {
                const stmts = self.store.getStmts(block.stmts);
                for (stmts) |stmt| {
                    try self.registerPatternSymbol(stmt.pattern);
                    try self.countUses(stmt.expr);
                }
                try self.countUses(block.final_expr);
            },
            .call => |call| {
                try self.countUses(call.fn_expr);
                const args = self.store.getExprSpan(call.args);
                for (args) |arg_id| {
                    try self.countUses(arg_id);
                }
            },
            .if_then_else => |ite| {
                const branches = self.store.getIfBranches(ite.branches);
                for (branches) |branch| {
                    try self.countUses(branch.cond);
                    try self.countUses(branch.body);
                }
                try self.countUses(ite.final_else);
            },
            .when => |w| {
                try self.countUses(w.value);
                const branches = self.store.getWhenBranches(w.branches);
                for (branches) |branch| {
                    try self.registerPatternSymbol(branch.pattern);
                    try self.countUses(branch.body);
                }
            },
            .lambda => |lam| {
                const params = self.store.getPatternSpan(lam.params);
                for (params) |pat_id| {
                    try self.registerPatternSymbol(pat_id);
                }
                try self.countUses(lam.body);
            },
            .closure => |clo| {
                try self.countUses(clo.lambda);
            },
            .list => |list| {
                const elems = self.store.getExprSpan(list.elems);
                for (elems) |elem_id| {
                    try self.countUses(elem_id);
                }
            },
            .record => |rec| {
                const fields = self.store.getExprSpan(rec.fields);
                for (fields) |field_id| {
                    try self.countUses(field_id);
                }
            },
            .tuple => |tup| {
                const elems = self.store.getExprSpan(tup.elems);
                for (elems) |elem_id| {
                    try self.countUses(elem_id);
                }
            },
            .tag => |t| {
                const args = self.store.getExprSpan(t.args);
                for (args) |arg_id| {
                    try self.countUses(arg_id);
                }
            },
            .field_access => |fa| {
                try self.countUses(fa.record_expr);
            },
            .tuple_access => |ta| {
                try self.countUses(ta.tuple_expr);
            },
            .binop => |b| {
                try self.countUses(b.lhs);
                try self.countUses(b.rhs);
            },
            .unary_minus => |u| {
                try self.countUses(u.expr);
            },
            .unary_not => |u| {
                try self.countUses(u.expr);
            },
            .nominal => |n| {
                try self.countUses(n.backing_expr);
            },
            .early_return => |ret| {
                try self.countUses(ret.expr);
            },
            .dbg => |d| {
                try self.countUses(d.expr);
            },
            .expect => |e| {
                try self.countUses(e.cond);
                try self.countUses(e.body);
            },
            .low_level => |ll| {
                const args = self.store.getExprSpan(ll.args);
                for (args) |arg_id| {
                    try self.countUses(arg_id);
                }
            },
            .hosted_call => |hc| {
                const args = self.store.getExprSpan(hc.args);
                for (args) |arg_id| {
                    try self.countUses(arg_id);
                }
            },
            .str_concat => |span| {
                const parts = self.store.getExprSpan(span);
                for (parts) |part_id| {
                    try self.countUses(part_id);
                }
            },
            .int_to_str => |its| {
                try self.countUses(its.value);
            },
            .float_to_str => |fts| {
                try self.countUses(fts.value);
            },
            .dec_to_str => |d| {
                try self.countUses(d);
            },
            .str_escape_and_quote => |s| {
                try self.countUses(s);
            },
            .discriminant_switch => |ds| {
                try self.countUses(ds.value);
                const branches = self.store.getExprSpan(ds.branches);
                for (branches) |br_id| {
                    try self.countUses(br_id);
                }
            },
            .tag_payload_access => |tpa| {
                try self.countUses(tpa.value);
            },
            .for_loop => |fl| {
                try self.countUses(fl.list_expr);
                try self.registerPatternSymbol(fl.elem_pattern);
                try self.countUses(fl.body);
            },
            .while_loop => |wl| {
                try self.countUses(wl.cond);
                try self.countUses(wl.body);
            },
            // RC ops themselves and terminals don't need counting
            .incref, .decref, .free => {},
            .i64_literal,
            .i128_literal,
            .f64_literal,
            .f32_literal,
            .dec_literal,
            .str_literal,
            .bool_literal,
            .empty_list,
            .empty_record,
            .zero_arg_tag,
            .crash,
            .runtime_error,
            => {},
        }
    }

    /// Register a pattern's bound symbol with its layout.
    fn registerPatternSymbol(self: *RcInsertPass, pat_id: LirPatternId) Allocator.Error!void {
        if (pat_id.isNone()) return;
        const pat = self.store.getPattern(pat_id);
        switch (pat) {
            .bind => |bind| {
                if (!bind.symbol.isNone()) {
                    const key = @as(u64, @bitCast(bind.symbol));
                    try self.symbol_layouts.put(key, bind.layout_idx);
                    if (!self.symbol_use_counts.contains(key)) {
                        try self.symbol_use_counts.put(key, 0);
                    }
                }
            },
            .as_pattern => |as_pat| {
                if (!as_pat.symbol.isNone()) {
                    const key = @as(u64, @bitCast(as_pat.symbol));
                    try self.symbol_layouts.put(key, as_pat.layout_idx);
                    if (!self.symbol_use_counts.contains(key)) {
                        try self.symbol_use_counts.put(key, 0);
                    }
                }
                try self.registerPatternSymbol(as_pat.inner);
            },
            .tag => |t| {
                for (self.store.getPatternSpan(t.args)) |arg_pat| {
                    try self.registerPatternSymbol(arg_pat);
                }
            },
            .record => |r| {
                for (self.store.getPatternSpan(r.fields)) |field_pat| {
                    try self.registerPatternSymbol(field_pat);
                }
            },
            .tuple => |t| {
                for (self.store.getPatternSpan(t.elems)) |elem_pat| {
                    try self.registerPatternSymbol(elem_pat);
                }
            },
            .list => |l| {
                for (self.store.getPatternSpan(l.prefix)) |pre_pat| {
                    try self.registerPatternSymbol(pre_pat);
                }
                try self.registerPatternSymbol(l.rest);
            },
            .wildcard, .int_literal, .float_literal, .str_literal => {},
        }
    }

    /// Check if a layout needs reference counting (directly or transitively).
    fn layoutNeedsRc(self: *const RcInsertPass, layout_idx: LayoutIdx) bool {
        const l = self.layout_store.getLayout(layout_idx);
        return self.layout_store.layoutContainsRefcounted(l);
    }

    /// Process an expression, inserting RC operations as needed.
    /// Returns a (possibly new) expression ID for the transformed tree.
    fn processExpr(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!LirExprId {
        if (expr_id.isNone()) return expr_id;

        const expr = self.store.getExpr(expr_id);
        const region = self.store.getExprRegion(expr_id);

        return switch (expr) {
            .block => |block| self.processBlock(block.stmts, block.final_expr, block.result_layout, region),
            .if_then_else => |ite| self.processIfThenElse(ite.branches, ite.final_else, ite.result_layout, region),
            .when => |w| self.processWhen(w.value, w.value_layout, w.branches, w.result_layout, region),
            // For all other expressions, return as-is.
            // RC operations are inserted at block boundaries, not inside
            // individual expressions.
            else => expr_id,
        };
    }

    /// Process a block, inserting decrefs for symbols that go out of scope
    /// and increfs for multi-use symbols.
    fn processBlock(
        self: *RcInsertPass,
        stmts_span: LirStmtSpan,
        final_expr: LirExprId,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        const stmts = self.store.getStmts(stmts_span);

        // Clear the temp buffer
        self.stmt_buf.clearRetainingCapacity();

        var rc_ops_added: u32 = 0;

        // Process each statement
        for (stmts) |stmt| {
            // Add the original statement
            try self.stmt_buf.append(self.allocator, stmt);

            // Check if the bound pattern is a refcounted symbol
            if (!stmt.pattern.isNone()) {
                const pat = self.store.getPattern(stmt.pattern);
                switch (pat) {
                    .bind => |bind| {
                        if (!bind.symbol.isNone() and self.layoutNeedsRc(bind.layout_idx)) {
                            const key = @as(u64, @bitCast(bind.symbol));
                            const use_count = self.symbol_use_counts.get(key) orelse 0;

                            if (use_count > 1) {
                                // Multi-use: insert incref with count N-1
                                try self.emitIncref(bind.symbol, bind.layout_idx, @intCast(use_count - 1), region);
                                rc_ops_added += 1;
                            }
                        }
                    },
                    else => {},
                }
            }
        }

        // Recursively process the final expression
        const new_final = try self.processExpr(final_expr);

        // Insert decrefs for refcounted symbols bound in this block that are never used
        for (stmts) |stmt| {
            if (!stmt.pattern.isNone()) {
                const pat = self.store.getPattern(stmt.pattern);
                switch (pat) {
                    .bind => |bind| {
                        if (!bind.symbol.isNone() and self.layoutNeedsRc(bind.layout_idx)) {
                            const key = @as(u64, @bitCast(bind.symbol));
                            const use_count = self.symbol_use_counts.get(key) orelse 0;
                            if (use_count == 0) {
                                try self.emitDecref(bind.symbol, bind.layout_idx, region);
                                rc_ops_added += 1;
                            }
                        }
                    },
                    else => {},
                }
            }
        }

        // If nothing changed, return a block with the original data
        if (rc_ops_added == 0 and new_final == final_expr) {
            return self.store.addExpr(.{ .block = .{
                .stmts = stmts_span,
                .final_expr = final_expr,
                .result_layout = result_layout,
            } }, region);
        }

        // Build the new statement span
        const new_stmts = try self.store.addStmts(self.stmt_buf.items);

        return self.store.addExpr(.{ .block = .{
            .stmts = new_stmts,
            .final_expr = new_final,
            .result_layout = result_layout,
        } }, region);
    }

    /// Process an if-then-else expression.
    /// Each branch is processed independently.
    fn processIfThenElse(
        self: *RcInsertPass,
        branches_span: LIR.LirIfBranchSpan,
        final_else_id: LirExprId,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        const branches = self.store.getIfBranches(branches_span);
        var changed = false;

        var new_branches = std.ArrayList(LirIfBranch).empty;
        defer new_branches.deinit(self.allocator);

        for (branches) |branch| {
            const new_body = try self.processExpr(branch.body);
            if (new_body != branch.body) changed = true;
            try new_branches.append(self.allocator, .{
                .cond = branch.cond,
                .body = new_body,
            });
        }

        const new_else = try self.processExpr(final_else_id);
        if (new_else != final_else_id) changed = true;

        if (!changed) {
            return self.store.addExpr(.{ .if_then_else = .{
                .branches = branches_span,
                .final_else = final_else_id,
                .result_layout = result_layout,
            } }, region);
        }

        const new_branch_span = try self.store.addIfBranches(new_branches.items);
        return self.store.addExpr(.{ .if_then_else = .{
            .branches = new_branch_span,
            .final_else = new_else,
            .result_layout = result_layout,
        } }, region);
    }

    /// Process a when expression.
    /// Each branch body is processed independently.
    fn processWhen(
        self: *RcInsertPass,
        value: LirExprId,
        value_layout: LayoutIdx,
        branches_span: LIR.LirWhenBranchSpan,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        const branches = self.store.getWhenBranches(branches_span);
        var changed = false;

        var new_branches = std.ArrayList(LirWhenBranch).empty;
        defer new_branches.deinit(self.allocator);

        for (branches) |branch| {
            const new_body = try self.processExpr(branch.body);
            if (new_body != branch.body) changed = true;
            try new_branches.append(self.allocator, .{
                .pattern = branch.pattern,
                .guard = branch.guard,
                .body = new_body,
            });
        }

        if (!changed) {
            return self.store.addExpr(.{ .when = .{
                .value = value,
                .value_layout = value_layout,
                .branches = branches_span,
                .result_layout = result_layout,
            } }, region);
        }

        const new_branch_span = try self.store.addWhenBranches(new_branches.items);
        return self.store.addExpr(.{ .when = .{
            .value = value,
            .value_layout = value_layout,
            .branches = new_branch_span,
            .result_layout = result_layout,
        } }, region);
    }

    /// Emit an incref statement into the statement buffer.
    fn emitIncref(self: *RcInsertPass, symbol: Symbol, layout_idx: LayoutIdx, count: u16, region: Region) Allocator.Error!void {
        const lookup_id = try self.store.addExpr(.{ .lookup = .{
            .symbol = symbol,
            .layout_idx = layout_idx,
        } }, region);

        const incref_id = try self.store.addExpr(.{ .incref = .{
            .value = lookup_id,
            .layout_idx = layout_idx,
            .count = count,
        } }, region);

        const wildcard = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = layout_idx } }, region);
        try self.stmt_buf.append(self.allocator, .{
            .pattern = wildcard,
            .expr = incref_id,
        });
    }

    /// Emit a decref statement into the statement buffer.
    fn emitDecref(self: *RcInsertPass, symbol: Symbol, layout_idx: LayoutIdx, region: Region) Allocator.Error!void {
        const lookup_id = try self.store.addExpr(.{ .lookup = .{
            .symbol = symbol,
            .layout_idx = layout_idx,
        } }, region);

        const decref_id = try self.store.addExpr(.{ .decref = .{
            .value = lookup_id,
            .layout_idx = layout_idx,
        } }, region);

        const wildcard = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = layout_idx } }, region);
        try self.stmt_buf.append(self.allocator, .{
            .pattern = wildcard,
            .expr = decref_id,
        });
    }
};

test "RcInsertPass compiles" {
    const T = RcInsertPass;
    std.debug.assert(@sizeOf(T) > 0);
}
