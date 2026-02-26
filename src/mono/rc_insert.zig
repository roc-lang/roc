//! Mono IR-level Reference Counting Insertion Pass
//!
//! This pass walks MonoExpr trees after lowering from CIR and inserts
//! `incref`, `decref`, and `free` operations based on Perceus-inspired
//! ownership tracking.
//!
//! Key insight: process code bottom-up (continuation-first), tracking which
//! symbols are consumed. Insert increfs for multi-use and decrefs for last-use.
//!
//! This operates on MonoExprStore where every expression carries a concrete
//! `layout_idx`, eliminating the type-variable corruption issues that
//! plagued the previous CIR-level RC pass.
//!
//! ## Branch-aware RC: "branch-owns-its-RC" model
//!
//! For branching constructs (match/if), use counts are scoped per-branch.
//! The enclosing scope provides exactly 1 reference per branching construct
//! that uses a symbol. Each branch then adjusts at entry:
//! - used 0 times: decref (release inherited ref)
//! - used 1 time: no action (consumes inherited ref)
//! - used N>1 times: incref(N-1)

const std = @import("std");
const Allocator = std.mem.Allocator;
const layout_mod = @import("layout");
const base = @import("base");
const MonoIR = @import("MonoIR.zig");
const MonoExprStore = @import("MonoExprStore.zig");

const MonoExprId = MonoIR.MonoExprId;
const Symbol = MonoIR.Symbol;
const MonoStmt = MonoIR.MonoStmt;
const MonoStmtSpan = MonoIR.MonoStmtSpan;
const MonoPatternId = MonoIR.MonoPatternId;
const MonoIfBranch = MonoIR.MonoIfBranch;
const MonoMatchBranch = MonoIR.MonoMatchBranch;
const LayoutIdx = layout_mod.Idx;
const Region = base.Region;

/// Inserts reference counting operations (incref/decref) into the mono IR.
pub const RcInsertPass = struct {
    allocator: Allocator,
    store: *MonoExprStore,
    layout_store: *const layout_mod.Store,

    /// Tracks how many times each symbol is used in the expression tree.
    /// Keyed by the raw u64 representation of Symbol.
    symbol_use_counts: std.AutoHashMap(u64, u32),

    /// Tracks layout for each symbol (for generating incref/decref with correct layout).
    symbol_layouts: std.AutoHashMap(u64, LayoutIdx),

    /// Tracks live RC symbols across blocks for early_return cleanup.
    live_rc_symbols: std.ArrayList(LiveRcSymbol),

    /// Base index into live_rc_symbols for the current function scope.
    /// early_return only cleans up symbols from early_return_scope_base onward.
    early_return_scope_base: usize,

    /// Tracks how many uses of each RC symbol have been consumed so far
    /// in the current block (by already-processed statements).
    block_consumed_uses: std.AutoHashMap(u64, u32),

    /// Cumulative consumed uses across all enclosing blocks.
    cumulative_consumed_uses: std.AutoHashMap(u64, u32),

    /// Reusable scratch buffer for collecting HashMap keys before sorting.
    /// Used by wrapBranchWithRcOps and wrapGuardWithIncref to ensure
    /// deterministic RC op ordering regardless of HashMap iteration order.
    scratch_keys: base.Scratch(u64),

    const LiveRcSymbol = struct {
        symbol: Symbol,
        layout_idx: LayoutIdx,
    };

    pub fn init(allocator: Allocator, store: *MonoExprStore, layout_store: *const layout_mod.Store) Allocator.Error!RcInsertPass {
        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .symbol_use_counts = std.AutoHashMap(u64, u32).init(allocator),
            .symbol_layouts = std.AutoHashMap(u64, LayoutIdx).init(allocator),
            .live_rc_symbols = std.ArrayList(LiveRcSymbol).empty,
            .early_return_scope_base = 0,
            .block_consumed_uses = std.AutoHashMap(u64, u32).init(allocator),
            .cumulative_consumed_uses = std.AutoHashMap(u64, u32).init(allocator),
            .scratch_keys = try base.Scratch(u64).init(allocator),
        };
    }

    pub fn deinit(self: *RcInsertPass) void {
        self.symbol_use_counts.deinit();
        self.symbol_layouts.deinit();
        self.live_rc_symbols.deinit(self.allocator);
        self.block_consumed_uses.deinit();
        self.cumulative_consumed_uses.deinit();
        self.scratch_keys.deinit();
    }

    /// Main entry point: insert RC operations into a MonoExpr tree.
    /// Returns a new MonoExprId for the RC-annotated tree.
    pub fn insertRcOps(self: *RcInsertPass, expr_id: MonoExprId) Allocator.Error!MonoExprId {
        // Phase 1: Count all symbol uses and record layouts
        try self.countUses(expr_id);

        // Phase 2: Walk the tree and insert RC operations
        return self.processExpr(expr_id);
    }

    /// Count how many times each symbol is referenced in the expression tree.
    /// Also records the layout for each symbol found in bind patterns.
    /// Wrapper around countUsesInto that targets self.symbol_use_counts.
    fn countUses(self: *RcInsertPass, expr_id: MonoExprId) Allocator.Error!void {
        try self.countUsesInto(expr_id, &self.symbol_use_counts);
    }

    /// Count uses into a fresh local map and return it. Caller owns the map.
    fn countUsesLocal(self: *RcInsertPass, expr_id: MonoExprId) Allocator.Error!std.AutoHashMap(u64, u32) {
        var local = std.AutoHashMap(u64, u32).init(self.allocator);
        errdefer local.deinit();
        try self.countUsesInto(expr_id, &local);
        return local;
    }

    /// Count how many times each symbol is referenced, writing into `target`.
    /// Also records the layout for each symbol found in bind patterns.
    fn countUsesInto(self: *RcInsertPass, expr_id: MonoExprId, target: *std.AutoHashMap(u64, u32)) Allocator.Error!void {
        if (expr_id.isNone()) return;

        const expr = self.store.getExpr(expr_id);
        switch (expr) {
            .lookup => |lookup| {
                if (!lookup.symbol.isNone()) {
                    const key = @as(u64, @bitCast(lookup.symbol));
                    const gop = try target.getOrPut(key);
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
                    try self.registerPatternSymbolInto(stmt.pattern, target);
                    try self.countUsesInto(stmt.expr, target);
                }
                try self.countUsesInto(block.final_expr, target);
            },
            .call => |call| {
                try self.countUsesInto(call.fn_expr, target);
                const args = self.store.getExprSpan(call.args);
                for (args) |arg_id| {
                    try self.countUsesInto(arg_id, target);
                }
            },
            .if_then_else => |ite| {
                // Count condition uses directly into target
                const branches = self.store.getIfBranches(ite.branches);
                for (branches) |branch| {
                    try self.countUsesInto(branch.cond, target);
                }
                // Count branch body uses into local maps; each branching construct
                // contributes 1 use per symbol to the enclosing scope.
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                for (branches) |branch| {
                    local.clearRetainingCapacity();
                    try self.countUsesInto(branch.body, &local);
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                {
                    local.clearRetainingCapacity();
                    try self.countUsesInto(ite.final_else, &local);
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| {
                    const gop = try target.getOrPut(key.*);
                    if (gop.found_existing) gop.value_ptr.* += 1 else gop.value_ptr.* = 1;
                }
            },
            .match_expr => |w| {
                try self.countUsesInto(w.value, target);
                const branches = self.store.getMatchBranches(w.branches);
                // Count branch body uses into local maps; each branching construct
                // contributes 1 use per symbol to the enclosing scope.
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                for (branches) |branch| {
                    try self.registerPatternSymbolInto(branch.pattern, target);
                    local.clearRetainingCapacity();
                    try self.countUsesInto(branch.guard, &local);
                    try self.countUsesInto(branch.body, &local);
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| {
                    const gop = try target.getOrPut(key.*);
                    if (gop.found_existing) gop.value_ptr.* += 1 else gop.value_ptr.* = 1;
                }
            },
            .lambda => |lam| {
                // Lambda bodies are a separate scope. Count uses locally, then
                // attribute 1 use per captured symbol to the outer scope, and
                // ensure body-local symbols are in self.symbol_use_counts so
                // processBlock can emit RC ops for them.
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();

                const params = self.store.getPatternSpan(lam.params);
                for (params) |pat_id| {
                    try self.registerPatternSymbolInto(pat_id, &local);
                }
                try self.countUsesInto(lam.body, &local);

                var it = local.iterator();
                while (it.next()) |entry| {
                    const key = entry.key_ptr.*;
                    if (target.contains(key)) {
                        // Symbol exists in outer scope — it's a capture.
                        // Attribute exactly 1 use to the outer scope.
                        const gop = try target.getOrPut(key);
                        if (gop.found_existing) gop.value_ptr.* += 1 else gop.value_ptr.* = 1;
                    }
                    // Always ensure body symbols are in self.symbol_use_counts
                    // so processBlock can emit RC ops for body-local bindings.
                    const global_gop = try self.symbol_use_counts.getOrPut(key);
                    if (!global_gop.found_existing) {
                        global_gop.value_ptr.* = entry.value_ptr.*;
                    }
                }
            },
            .closure => |clo| {
                try self.countUsesInto(clo.lambda, target);
                // Each capture consumes a reference to the captured symbol.
                const captures = self.store.getCaptures(clo.captures);
                for (captures) |cap| {
                    if (!cap.symbol.isNone()) {
                        const key = @as(u64, @bitCast(cap.symbol));
                        const gop = try target.getOrPut(key);
                        if (gop.found_existing) {
                            gop.value_ptr.* += 1;
                        } else {
                            gop.value_ptr.* = 1;
                        }
                        if (!self.symbol_layouts.contains(key)) {
                            try self.symbol_layouts.put(key, cap.layout_idx);
                        }
                    }
                }
            },
            .list => |list| {
                const elems = self.store.getExprSpan(list.elems);
                for (elems) |elem_id| {
                    try self.countUsesInto(elem_id, target);
                }
            },
            .record => |rec| {
                const fields = self.store.getExprSpan(rec.fields);
                for (fields) |field_id| {
                    try self.countUsesInto(field_id, target);
                }
            },
            .tuple => |tup| {
                const elems = self.store.getExprSpan(tup.elems);
                for (elems) |elem_id| {
                    try self.countUsesInto(elem_id, target);
                }
            },
            .tag => |t| {
                const args = self.store.getExprSpan(t.args);
                for (args) |arg_id| {
                    try self.countUsesInto(arg_id, target);
                }
            },
            .field_access => |fa| {
                try self.countUsesInto(fa.record_expr, target);
            },
            .tuple_access => |ta| {
                try self.countUsesInto(ta.tuple_expr, target);
            },
            .binop => |b| {
                try self.countUsesInto(b.lhs, target);
                try self.countUsesInto(b.rhs, target);
            },
            .unary_minus => |u| {
                try self.countUsesInto(u.expr, target);
            },
            .unary_not => |u| {
                try self.countUsesInto(u.expr, target);
            },
            .nominal => |n| {
                try self.countUsesInto(n.backing_expr, target);
            },
            .early_return => |ret| {
                try self.countUsesInto(ret.expr, target);
            },
            .dbg => |d| {
                try self.countUsesInto(d.expr, target);
            },
            .expect => |e| {
                try self.countUsesInto(e.cond, target);
                try self.countUsesInto(e.body, target);
            },
            .low_level => |ll| {
                const args = self.store.getExprSpan(ll.args);
                for (args) |arg_id| {
                    try self.countUsesInto(arg_id, target);
                }
            },
            .hosted_call => |hc| {
                const args = self.store.getExprSpan(hc.args);
                for (args) |arg_id| {
                    try self.countUsesInto(arg_id, target);
                }
            },
            .str_concat => |span| {
                const parts = self.store.getExprSpan(span);
                for (parts) |part_id| {
                    try self.countUsesInto(part_id, target);
                }
            },
            .int_to_str => |its| {
                try self.countUsesInto(its.value, target);
            },
            .float_to_str => |fts| {
                try self.countUsesInto(fts.value, target);
            },
            .dec_to_str => |d| {
                try self.countUsesInto(d, target);
            },
            .str_escape_and_quote => |s| {
                try self.countUsesInto(s, target);
            },
            .discriminant_switch => |ds| {
                try self.countUsesInto(ds.value, target);
                const branches = self.store.getExprSpan(ds.branches);
                for (branches) |br_id| {
                    try self.countUsesInto(br_id, target);
                }
            },
            .tag_payload_access => |tpa| {
                try self.countUsesInto(tpa.value, target);
            },
            .for_loop => |fl| {
                try self.countUsesInto(fl.list_expr, target);
                try self.registerPatternSymbolInto(fl.elem_pattern, target);
                try self.countUsesInto(fl.body, target);
            },
            .while_loop => |wl| {
                try self.countUsesInto(wl.cond, target);
                try self.countUsesInto(wl.body, target);
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

    /// Register a pattern's bound symbol with its layout (into self.symbol_use_counts).
    /// Register a pattern's bound symbol with its layout into a given target map.
    fn registerPatternSymbolInto(self: *RcInsertPass, pat_id: MonoPatternId, target: *std.AutoHashMap(u64, u32)) Allocator.Error!void {
        if (pat_id.isNone()) return;
        const pat = self.store.getPattern(pat_id);
        switch (pat) {
            .bind => |bind| {
                if (!bind.symbol.isNone()) {
                    const key = @as(u64, @bitCast(bind.symbol));
                    try self.symbol_layouts.put(key, bind.layout_idx);
                    if (!target.contains(key)) {
                        try target.put(key, 0);
                    }
                }
            },
            .as_pattern => |as_pat| {
                if (!as_pat.symbol.isNone()) {
                    const key = @as(u64, @bitCast(as_pat.symbol));
                    try self.symbol_layouts.put(key, as_pat.layout_idx);
                    if (!target.contains(key)) {
                        try target.put(key, 0);
                    }
                }
                try self.registerPatternSymbolInto(as_pat.inner, target);
            },
            .tag => |t| {
                for (self.store.getPatternSpan(t.args)) |arg_pat| {
                    try self.registerPatternSymbolInto(arg_pat, target);
                }
            },
            .record => |r| {
                for (self.store.getPatternSpan(r.fields)) |field_pat| {
                    try self.registerPatternSymbolInto(field_pat, target);
                }
            },
            .tuple => |t| {
                for (self.store.getPatternSpan(t.elems)) |elem_pat| {
                    try self.registerPatternSymbolInto(elem_pat, target);
                }
            },
            .list => |l| {
                for (self.store.getPatternSpan(l.prefix)) |pre_pat| {
                    try self.registerPatternSymbolInto(pre_pat, target);
                }
                try self.registerPatternSymbolInto(l.rest, target);
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
    fn processExpr(self: *RcInsertPass, expr_id: MonoExprId) Allocator.Error!MonoExprId {
        if (expr_id.isNone()) return expr_id;

        const expr = self.store.getExpr(expr_id);
        const region = self.store.getExprRegion(expr_id);

        return switch (expr) {
            .block => |block| self.processBlock(block.stmts, block.final_expr, block.result_layout, region),
            .if_then_else => |ite| self.processIfThenElse(ite.branches, ite.final_else, ite.result_layout, region),
            .match_expr => |w| self.processMatch(w.value, w.value_layout, w.branches, w.result_layout, region),
            .lambda => |lam| self.processLambda(lam, region, expr_id),
            .closure => |clo| self.processClosure(clo, region, expr_id),
            .for_loop => |fl| self.processForLoop(fl, region, expr_id),
            .while_loop => |wl| self.processWhileLoop(wl, region, expr_id),
            .early_return => |ret| return self.processEarlyReturn(ret, region, expr_id),
            // For all other expressions, return as-is.
            // RC operations are inserted at block boundaries, not inside
            // individual expressions.
            .i64_literal,
            .i128_literal,
            .f64_literal,
            .f32_literal,
            .dec_literal,
            .str_literal,
            .bool_literal,
            .lookup,
            .call,
            .empty_list,
            .list,
            .empty_record,
            .record,
            .tuple,
            .field_access,
            .tuple_access,
            .zero_arg_tag,
            .tag,
            .binop,
            .unary_minus,
            .unary_not,
            .low_level,
            .dbg,
            .expect,
            .crash,
            .runtime_error,
            .nominal,
            .str_concat,
            .int_to_str,
            .float_to_str,
            .dec_to_str,
            .str_escape_and_quote,
            .discriminant_switch,
            .tag_payload_access,
            .hosted_call,
            .incref,
            .decref,
            .free,
            => expr_id,
        };
    }

    /// Process a block, inserting decrefs for symbols that go out of scope
    /// and increfs for multi-use symbols.
    fn processBlock(
        self: *RcInsertPass,
        stmts_span: MonoStmtSpan,
        final_expr: MonoExprId,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!MonoExprId {
        const stmts = self.store.getStmts(stmts_span);

        // Save live_rc_symbols depth so nested blocks restore on exit.
        const saved_live_len = self.live_rc_symbols.items.len;
        defer self.live_rc_symbols.shrinkRetainingCapacity(saved_live_len);

        // Save and reset block_consumed_uses for this block scope.
        // Accumulate current block's consumed uses into cumulative map first.
        {
            var it = self.block_consumed_uses.iterator();
            while (it.next()) |entry| {
                const gop = try self.cumulative_consumed_uses.getOrPut(entry.key_ptr.*);
                if (gop.found_existing) {
                    gop.value_ptr.* += entry.value_ptr.*;
                } else {
                    gop.value_ptr.* = entry.value_ptr.*;
                }
            }
        }
        var saved_cumulative_additions = std.AutoHashMap(u64, u32).init(self.allocator);
        {
            var it = self.block_consumed_uses.iterator();
            while (it.next()) |entry| {
                try saved_cumulative_additions.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }
        const saved_consumed = self.block_consumed_uses.move();
        defer {
            var sit = saved_cumulative_additions.iterator();
            while (sit.next()) |entry| {
                if (self.cumulative_consumed_uses.getPtr(entry.key_ptr.*)) |cum_ptr| {
                    cum_ptr.* -|= entry.value_ptr.*;
                }
            }
            saved_cumulative_additions.deinit();
            self.block_consumed_uses.deinit();
            self.block_consumed_uses = saved_consumed;
        }

        // Use a local buffer to avoid reentrancy issues — processExpr may
        // recurse into another processBlock (e.g. lambda/loop bodies).
        var stmt_buf = std.ArrayList(MonoStmt).empty;
        defer stmt_buf.deinit(self.allocator);

        var changed = false;

        // Process each statement
        for (stmts) |stmt| {
            // Recursively process the statement's expression
            const new_expr = try self.processExpr(stmt.expr);
            if (new_expr != stmt.expr) changed = true;

            // Track uses consumed by this statement's expression
            {
                var stmt_uses = try self.countUsesLocal(stmt.expr);
                defer stmt_uses.deinit();
                var use_it = stmt_uses.iterator();
                while (use_it.next()) |entry| {
                    const gop = try self.block_consumed_uses.getOrPut(entry.key_ptr.*);
                    if (gop.found_existing) {
                        gop.value_ptr.* += entry.value_ptr.*;
                    } else {
                        gop.value_ptr.* = entry.value_ptr.*;
                    }
                }
            }

            // Add the (possibly updated) statement
            try stmt_buf.append(self.allocator, .{ .pattern = stmt.pattern, .expr = new_expr });

            // Track live RC symbols bound by this statement's pattern
            try self.trackLiveRcSymbolsFromPattern(stmt.pattern);

            // Emit increfs for multi-use refcounted symbols bound by this pattern
            {
                const before_len = stmt_buf.items.len;
                try self.emitBlockIncrefsForPattern(stmt.pattern, region, &stmt_buf);
                if (stmt_buf.items.len > before_len) changed = true;
            }
        }

        // Recursively process the final expression
        const new_final = try self.processExpr(final_expr);
        if (new_final != final_expr) changed = true;

        // Insert decrefs for refcounted symbols bound in this block that are never used
        {
            const before_len = stmt_buf.items.len;
            for (stmts) |stmt| {
                try self.emitBlockDecrefsForPattern(stmt.pattern, region, &stmt_buf);
            }
            if (stmt_buf.items.len > before_len) changed = true;
        }

        // If nothing changed, return a block with the original data
        if (!changed) {
            return self.store.addExpr(.{ .block = .{
                .stmts = stmts_span,
                .final_expr = final_expr,
                .result_layout = result_layout,
            } }, region);
        }

        // Build the new statement span
        const new_stmts = try self.store.addStmts(stmt_buf.items);

        return self.store.addExpr(.{ .block = .{
            .stmts = new_stmts,
            .final_expr = new_final,
            .result_layout = result_layout,
        } }, region);
    }

    /// Process an early_return expression.
    /// Emits cleanup decrefs for all live RC symbols in enclosing blocks
    /// that are NOT consumed by the return value expression itself.
    fn processEarlyReturn(self: *RcInsertPass, ret: anytype, region: Region, expr_id: MonoExprId) Allocator.Error!MonoExprId {
        const new_expr = try self.processExpr(ret.expr);

        var ret_uses = try self.countUsesLocal(ret.expr);
        defer ret_uses.deinit();

        var cleanup_stmts = std.ArrayList(MonoStmt).empty;
        defer cleanup_stmts.deinit(self.allocator);

        const live_syms = self.live_rc_symbols.items[self.early_return_scope_base..];
        for (live_syms) |live| {
            const key = @as(u64, @bitCast(live.symbol));
            const ret_use_count: u32 = ret_uses.get(key) orelse 0;
            const global_count = self.symbol_use_counts.get(key) orelse 1;
            const consumed_before = (self.cumulative_consumed_uses.get(key) orelse 0) +
                (self.block_consumed_uses.get(key) orelse 0);
            const total_consumed = consumed_before + ret_use_count;
            if (total_consumed >= global_count) continue;
            const remaining = global_count - total_consumed;
            var i: u32 = 0;
            while (i < remaining) : (i += 1) {
                try self.emitDecrefInto(live.symbol, live.layout_idx, region, &cleanup_stmts);
            }
        }

        if (cleanup_stmts.items.len == 0 and new_expr == ret.expr) return expr_id;

        const early_ret_id = try self.store.addExpr(.{ .early_return = .{
            .expr = new_expr,
            .ret_layout = ret.ret_layout,
        } }, region);

        if (cleanup_stmts.items.len == 0) return early_ret_id;

        // Wrap: block { decref(a); decref(b); early_return(new_expr) }
        const wildcard = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, region);
        try cleanup_stmts.append(self.allocator, .{
            .pattern = wildcard,
            .expr = early_ret_id,
        });

        const stmts_span = try self.store.addStmts(cleanup_stmts.items);
        return self.store.addExpr(.{ .block = .{
            .stmts = stmts_span,
            .final_expr = early_ret_id,
            .result_layout = ret.ret_layout,
        } }, region);
    }

    /// Track RC symbols bound by a pattern, adding them to live_rc_symbols.
    fn trackLiveRcSymbolsFromPattern(self: *RcInsertPass, pat_id: MonoPatternId) Allocator.Error!void {
        if (pat_id.isNone()) return;
        const pat = self.store.getPattern(pat_id);
        switch (pat) {
            .bind => |bind| {
                const key = @as(u64, @bitCast(bind.symbol));
                if (self.symbol_layouts.get(key)) |lay| {
                    if (self.layoutNeedsRc(lay)) {
                        try self.live_rc_symbols.append(self.allocator, .{
                            .symbol = bind.symbol,
                            .layout_idx = lay,
                        });
                    }
                }
            },
            .wildcard => {},
            .tag => |t| {
                const args = self.store.getPatternSpan(t.args);
                for (args) |arg_id| try self.trackLiveRcSymbolsFromPattern(arg_id);
            },
            .record => |r| {
                const fields = self.store.getPatternSpan(r.fields);
                for (fields) |field_id| try self.trackLiveRcSymbolsFromPattern(field_id);
            },
            .tuple => |t| {
                const elems = self.store.getPatternSpan(t.elems);
                for (elems) |elem_id| try self.trackLiveRcSymbolsFromPattern(elem_id);
            },
            .list => |l| {
                const prefix = self.store.getPatternSpan(l.prefix);
                for (prefix) |p_id| try self.trackLiveRcSymbolsFromPattern(p_id);
                if (!l.rest.isNone()) try self.trackLiveRcSymbolsFromPattern(l.rest);
            },
            .as_pattern => |ap| {
                const key = @as(u64, @bitCast(ap.symbol));
                if (self.symbol_layouts.get(key)) |lay| {
                    if (self.layoutNeedsRc(lay)) {
                        try self.live_rc_symbols.append(self.allocator, .{
                            .symbol = ap.symbol,
                            .layout_idx = lay,
                        });
                    }
                }
                try self.trackLiveRcSymbolsFromPattern(ap.inner);
            },
            .int_literal, .float_literal, .str_literal => {},
        }
    }

    /// Process an if-then-else expression.
    /// Each branch gets per-branch RC ops based on local use counts.
    fn processIfThenElse(
        self: *RcInsertPass,
        branches_span: MonoIR.MonoIfBranchSpan,
        final_else_id: MonoExprId,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!MonoExprId {
        const branches = self.store.getIfBranches(branches_span);

        // Collect symbols bound within branch bodies — these are local to their
        // defining branch and must NOT get per-branch RC ops.
        var body_bound = std.AutoHashMap(u64, void).init(self.allocator);
        defer body_bound.deinit();
        for (branches) |branch| {
            try self.collectExprBoundSymbols(branch.body, &body_bound);
        }
        try self.collectExprBoundSymbols(final_else_id, &body_bound);

        // Collect per-branch local use counts and union of all refcounted symbols
        var branch_use_maps = std.ArrayList(std.AutoHashMap(u64, u32)).empty;
        defer {
            for (branch_use_maps.items) |*m| m.deinit();
            branch_use_maps.deinit(self.allocator);
        }
        var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
        defer symbols_in_any_branch.deinit();

        for (branches) |branch| {
            var local = try self.countUsesLocal(branch.body);
            var it = local.keyIterator();
            while (it.next()) |key| {
                const k = key.*;
                if (body_bound.contains(k)) continue;
                if (self.symbol_layouts.get(k)) |lay| {
                    if (self.layoutNeedsRc(lay)) try symbols_in_any_branch.put(k, {});
                }
            }
            try branch_use_maps.append(self.allocator, local);
        }
        // else branch
        var else_uses = try self.countUsesLocal(final_else_id);
        defer else_uses.deinit();
        {
            var it = else_uses.keyIterator();
            while (it.next()) |key| {
                const k = key.*;
                if (body_bound.contains(k)) continue;
                if (self.symbol_layouts.get(k)) |lay| {
                    if (self.layoutNeedsRc(lay)) try symbols_in_any_branch.put(k, {});
                }
            }
        }

        var new_branches = std.ArrayList(MonoIfBranch).empty;
        defer new_branches.deinit(self.allocator);

        for (branches, 0..) |branch, i| {
            const processed_body = try self.processExpr(branch.body);
            const new_body = try self.wrapBranchWithRcOps(processed_body, &branch_use_maps.items[i], &symbols_in_any_branch, result_layout, region);
            try new_branches.append(self.allocator, .{
                .cond = branch.cond,
                .body = new_body,
            });
        }

        const processed_else = try self.processExpr(final_else_id);
        const new_else = try self.wrapBranchWithRcOps(processed_else, &else_uses, &symbols_in_any_branch, result_layout, region);

        const new_branch_span = try self.store.addIfBranches(new_branches.items);
        return self.store.addExpr(.{ .if_then_else = .{
            .branches = new_branch_span,
            .final_else = new_else,
            .result_layout = result_layout,
        } }, region);
    }

    /// Process a match expression.
    /// Each branch gets per-branch RC ops based on local use counts.
    fn processMatch(
        self: *RcInsertPass,
        value: MonoExprId,
        value_layout: LayoutIdx,
        branches_span: MonoIR.MonoMatchBranchSpan,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!MonoExprId {
        const branches = self.store.getMatchBranches(branches_span);

        // Collect symbols bound by branch patterns — these are local to each branch
        // and must NOT get per-branch RC ops from the enclosing scope.
        var pattern_bound = std.AutoHashMap(u64, void).init(self.allocator);
        defer pattern_bound.deinit();
        for (branches) |branch| {
            try self.collectPatternSymbols(branch.pattern, &pattern_bound);
        }

        // Collect symbols bound within branch bodies — these are local to their
        // defining branch and must NOT get per-branch RC ops.
        var body_bound = std.AutoHashMap(u64, void).init(self.allocator);
        defer body_bound.deinit();
        for (branches) |branch| {
            try self.collectExprBoundSymbols(branch.body, &body_bound);
        }

        // Collect per-branch body and guard use counts separately.
        // Both contribute to symbols_in_any_branch (so unused branches decref),
        // but body uses drive wrapBranchWithRcOps while guard uses get
        // borrow-style increfs via wrapGuardWithIncref.
        var body_use_maps = std.ArrayList(std.AutoHashMap(u64, u32)).empty;
        defer {
            for (body_use_maps.items) |*m| m.deinit();
            body_use_maps.deinit(self.allocator);
        }
        var guard_use_maps = std.ArrayList(std.AutoHashMap(u64, u32)).empty;
        defer {
            for (guard_use_maps.items) |*m| m.deinit();
            guard_use_maps.deinit(self.allocator);
        }
        var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
        defer symbols_in_any_branch.deinit();

        for (branches) |branch| {
            var body_local = try self.countUsesLocal(branch.body);
            var guard_local = try self.countUsesLocal(branch.guard);
            // Both guard and body symbols contribute to symbols_in_any_branch
            var it = body_local.keyIterator();
            while (it.next()) |key| {
                const k = key.*;
                if (pattern_bound.contains(k)) continue;
                if (body_bound.contains(k)) continue;
                if (self.symbol_layouts.get(k)) |lay| {
                    if (self.layoutNeedsRc(lay)) try symbols_in_any_branch.put(k, {});
                }
            }
            var it2 = guard_local.keyIterator();
            while (it2.next()) |key| {
                const k = key.*;
                if (pattern_bound.contains(k)) continue;
                if (self.symbol_layouts.get(k)) |lay| {
                    if (self.layoutNeedsRc(lay)) try symbols_in_any_branch.put(k, {});
                }
            }
            try body_use_maps.append(self.allocator, body_local);
            try guard_use_maps.append(self.allocator, guard_local);
        }

        var new_branches = std.ArrayList(MonoMatchBranch).empty;
        defer new_branches.deinit(self.allocator);

        for (branches, 0..) |branch, i| {
            const processed_body = try self.processExpr(branch.body);
            const processed_guard = try self.processExpr(branch.guard);
            // Body gets per-branch RC ops based on body-only uses
            const new_body = try self.wrapBranchWithRcOps(processed_body, &body_use_maps.items[i], &symbols_in_any_branch, result_layout, region);
            // Guard gets borrow-style increfs for symbols it uses
            const new_guard = try self.wrapGuardWithIncref(processed_guard, &guard_use_maps.items[i], &pattern_bound, region);
            try new_branches.append(self.allocator, .{
                .pattern = branch.pattern,
                .guard = new_guard,
                .body = new_body,
            });
        }

        const new_branch_span = try self.store.addMatchBranches(new_branches.items);
        return self.store.addExpr(.{ .match_expr = .{
            .value = value,
            .value_layout = value_layout,
            .branches = new_branch_span,
            .result_layout = result_layout,
        } }, region);
    }

    /// Process a lambda expression.
    /// Lambda bodies are independent scopes — the calling convention provides
    /// 1 reference per parameter. We recurse into the body and emit RC ops
    /// for lambda parameters based on body-local use counts.
    fn processLambda(self: *RcInsertPass, lam: anytype, region: Region, expr_id: MonoExprId) Allocator.Error!MonoExprId {
        const new_body = try self.processExpr(lam.body);

        // Count uses locally within the lambda body
        var local_uses = try self.countUsesLocal(lam.body);
        defer local_uses.deinit();

        // Emit RC ops for lambda parameters
        var rc_stmts = std.ArrayList(MonoStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        const params = self.store.getPatternSpan(lam.params);
        for (params) |pat_id| {
            try self.emitRcOpsForPatternInto(pat_id, &local_uses, region, &rc_stmts);
        }

        // If RC ops needed, wrap body in a block with RC stmts prepended
        var final_body = new_body;
        if (rc_stmts.items.len > 0) {
            const stmts_span = try self.store.addStmts(rc_stmts.items);
            final_body = try self.store.addExpr(.{ .block = .{
                .stmts = stmts_span,
                .final_expr = new_body,
                .result_layout = lam.ret_layout,
            } }, region);
        }

        if (final_body != lam.body) {
            return self.store.addExpr(.{ .lambda = .{
                .fn_layout = lam.fn_layout,
                .params = lam.params,
                .body = final_body,
                .ret_layout = lam.ret_layout,
            } }, region);
        }
        return expr_id;
    }

    /// Process a closure expression.
    /// A closure wraps a lambda — just recurse into the inner lambda.
    fn processClosure(self: *RcInsertPass, clo: anytype, region: Region, expr_id: MonoExprId) Allocator.Error!MonoExprId {
        const new_lambda = try self.processExpr(clo.lambda);
        if (new_lambda != clo.lambda) {
            return self.store.addExpr(.{ .closure = .{
                .closure_layout = clo.closure_layout,
                .lambda = new_lambda,
                .captures = clo.captures,
                .representation = clo.representation,
                .recursion = clo.recursion,
                .self_recursive = clo.self_recursive,
                .is_bound_to_variable = clo.is_bound_to_variable,
            } }, region);
        }
        return expr_id;
    }

    /// Process a for loop expression.
    /// For loops bind an element via elem_pattern each iteration.
    /// The loop provides 1 reference per element. We emit body-local RC ops
    /// for the element binding similar to lambda params.
    fn processForLoop(self: *RcInsertPass, fl: anytype, region: Region, expr_id: MonoExprId) Allocator.Error!MonoExprId {
        const new_body = try self.processExpr(fl.body);

        // Count uses locally within the loop body
        var local_uses = try self.countUsesLocal(fl.body);
        defer local_uses.deinit();

        // Emit RC ops for the elem_pattern
        var rc_stmts = std.ArrayList(MonoStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        try self.emitRcOpsForPatternInto(fl.elem_pattern, &local_uses, region, &rc_stmts);

        // If RC ops needed, wrap body in a block with RC stmts prepended
        var final_body = new_body;
        if (rc_stmts.items.len > 0) {
            const stmts_span = try self.store.addStmts(rc_stmts.items);
            final_body = try self.store.addExpr(.{ .block = .{
                .stmts = stmts_span,
                .final_expr = new_body,
                .result_layout = .zst,
            } }, region);
        }

        if (final_body != fl.body) {
            return self.store.addExpr(.{ .for_loop = .{
                .list_expr = fl.list_expr,
                .elem_layout = fl.elem_layout,
                .elem_pattern = fl.elem_pattern,
                .body = final_body,
            } }, region);
        }
        return expr_id;
    }

    /// Process a while loop expression.
    /// While loops don't bind new symbols — just recurse into cond and body.
    fn processWhileLoop(self: *RcInsertPass, wl: anytype, region: Region, expr_id: MonoExprId) Allocator.Error!MonoExprId {
        const new_cond = try self.processExpr(wl.cond);
        const new_body = try self.processExpr(wl.body);
        if (new_cond != wl.cond or new_body != wl.body) {
            return self.store.addExpr(.{ .while_loop = .{
                .cond = new_cond,
                .body = new_body,
            } }, region);
        }
        return expr_id;
    }

    /// Collect all symbols bound by a pattern into a set.
    fn collectPatternSymbols(self: *const RcInsertPass, pat_id: MonoPatternId, set: *std.AutoHashMap(u64, void)) Allocator.Error!void {
        if (pat_id.isNone()) return;
        const pat = self.store.getPattern(pat_id);
        switch (pat) {
            .bind => |bind| {
                if (!bind.symbol.isNone()) {
                    try set.put(@as(u64, @bitCast(bind.symbol)), {});
                }
            },
            .as_pattern => |as_pat| {
                if (!as_pat.symbol.isNone()) {
                    try set.put(@as(u64, @bitCast(as_pat.symbol)), {});
                }
                try self.collectPatternSymbols(as_pat.inner, set);
            },
            .tag => |t| {
                for (self.store.getPatternSpan(t.args)) |arg_pat| {
                    try self.collectPatternSymbols(arg_pat, set);
                }
            },
            .record => |r| {
                for (self.store.getPatternSpan(r.fields)) |field_pat| {
                    try self.collectPatternSymbols(field_pat, set);
                }
            },
            .tuple => |t| {
                for (self.store.getPatternSpan(t.elems)) |elem_pat| {
                    try self.collectPatternSymbols(elem_pat, set);
                }
            },
            .list => |l| {
                for (self.store.getPatternSpan(l.prefix)) |pre_pat| {
                    try self.collectPatternSymbols(pre_pat, set);
                }
                try self.collectPatternSymbols(l.rest, set);
            },
            .wildcard, .int_literal, .float_literal, .str_literal => {},
        }
    }

    /// Collect all symbols bound by patterns within an expression tree.
    /// Used to identify locally-defined symbols for branch-aware RC filtering.
    fn collectExprBoundSymbols(self: *const RcInsertPass, expr_id: MonoExprId, set: *std.AutoHashMap(u64, void)) Allocator.Error!void {
        if (expr_id.isNone()) return;
        const expr = self.store.getExpr(expr_id);
        switch (expr) {
            .block => |block| {
                const stmts = self.store.getStmts(block.stmts);
                for (stmts) |stmt| {
                    try self.collectPatternSymbols(stmt.pattern, set);
                    try self.collectExprBoundSymbols(stmt.expr, set);
                }
                try self.collectExprBoundSymbols(block.final_expr, set);
            },
            .if_then_else => |ite| {
                const ibs = self.store.getIfBranches(ite.branches);
                for (ibs) |branch| {
                    try self.collectExprBoundSymbols(branch.cond, set);
                    try self.collectExprBoundSymbols(branch.body, set);
                }
                try self.collectExprBoundSymbols(ite.final_else, set);
            },
            .match_expr => |w| {
                try self.collectExprBoundSymbols(w.value, set);
                const mbs = self.store.getMatchBranches(w.branches);
                for (mbs) |branch| {
                    try self.collectPatternSymbols(branch.pattern, set);
                    try self.collectExprBoundSymbols(branch.guard, set);
                    try self.collectExprBoundSymbols(branch.body, set);
                }
            },
            .lambda => |lam| {
                // In mono, lambda params and body are counted into target
                const params = self.store.getPatternSpan(lam.params);
                for (params) |pat_id| {
                    try self.collectPatternSymbols(pat_id, set);
                }
                try self.collectExprBoundSymbols(lam.body, set);
            },
            .closure => |clo| {
                try self.collectExprBoundSymbols(clo.lambda, set);
            },
            .for_loop => |fl| {
                try self.collectPatternSymbols(fl.elem_pattern, set);
                try self.collectExprBoundSymbols(fl.list_expr, set);
                try self.collectExprBoundSymbols(fl.body, set);
            },
            .while_loop => |wl| {
                try self.collectExprBoundSymbols(wl.cond, set);
                try self.collectExprBoundSymbols(wl.body, set);
            },
            .expect => |e| {
                try self.collectExprBoundSymbols(e.cond, set);
                try self.collectExprBoundSymbols(e.body, set);
            },
            // Expressions with sub-expressions but no pattern bindings
            .call => |call| {
                try self.collectExprBoundSymbols(call.fn_expr, set);
                const args = self.store.getExprSpan(call.args);
                for (args) |arg_id| try self.collectExprBoundSymbols(arg_id, set);
            },
            .list => |l| {
                const elems = self.store.getExprSpan(l.elems);
                for (elems) |elem_id| try self.collectExprBoundSymbols(elem_id, set);
            },
            .record => |rec| {
                const fields = self.store.getExprSpan(rec.fields);
                for (fields) |field_id| try self.collectExprBoundSymbols(field_id, set);
            },
            .tuple => |tup| {
                const elems = self.store.getExprSpan(tup.elems);
                for (elems) |elem_id| try self.collectExprBoundSymbols(elem_id, set);
            },
            .tag => |t| {
                const args = self.store.getExprSpan(t.args);
                for (args) |arg_id| try self.collectExprBoundSymbols(arg_id, set);
            },
            .field_access => |fa| try self.collectExprBoundSymbols(fa.record_expr, set),
            .tuple_access => |ta| try self.collectExprBoundSymbols(ta.tuple_expr, set),
            .binop => |b| {
                try self.collectExprBoundSymbols(b.lhs, set);
                try self.collectExprBoundSymbols(b.rhs, set);
            },
            .unary_minus => |u| try self.collectExprBoundSymbols(u.expr, set),
            .unary_not => |u| try self.collectExprBoundSymbols(u.expr, set),
            .nominal => |n| try self.collectExprBoundSymbols(n.backing_expr, set),
            .early_return => |ret| try self.collectExprBoundSymbols(ret.expr, set),
            .dbg => |d| try self.collectExprBoundSymbols(d.expr, set),
            .low_level => |ll| {
                const args = self.store.getExprSpan(ll.args);
                for (args) |arg_id| try self.collectExprBoundSymbols(arg_id, set);
            },
            .hosted_call => |hc| {
                const args = self.store.getExprSpan(hc.args);
                for (args) |arg_id| try self.collectExprBoundSymbols(arg_id, set);
            },
            .str_concat => |span| {
                const parts = self.store.getExprSpan(span);
                for (parts) |part_id| try self.collectExprBoundSymbols(part_id, set);
            },
            .int_to_str => |its| try self.collectExprBoundSymbols(its.value, set),
            .float_to_str => |fts| try self.collectExprBoundSymbols(fts.value, set),
            .dec_to_str => |d| try self.collectExprBoundSymbols(d, set),
            .str_escape_and_quote => |s| try self.collectExprBoundSymbols(s, set),
            .discriminant_switch => |ds| {
                try self.collectExprBoundSymbols(ds.value, set);
                const dbs = self.store.getExprSpan(ds.branches);
                for (dbs) |br_id| try self.collectExprBoundSymbols(br_id, set);
            },
            .tag_payload_access => |tpa| try self.collectExprBoundSymbols(tpa.value, set),
            // Terminals and RC ops — no sub-expressions, no bindings
            .lookup,
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
            .incref,
            .decref,
            .free,
            => {},
        }
    }

    /// Wrap a branch body with per-branch RC operations.
    /// Each branch inherits 1 reference per symbol from the enclosing scope.
    /// - local_count == 0: emit decref (release inherited ref)
    /// - local_count == 1: no action (consumes the inherited ref)
    /// - local_count > 1: emit incref(count - 1)
    fn wrapBranchWithRcOps(
        self: *RcInsertPass,
        body: MonoExprId,
        local_uses: *const std.AutoHashMap(u64, u32),
        symbols_in_any_branch: *const std.AutoHashMap(u64, void),
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!MonoExprId {
        var rc_stmts = std.ArrayList(MonoStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        // Collect keys and sort for deterministic RC op ordering
        const keys_start = self.scratch_keys.top();
        defer self.scratch_keys.clearFrom(keys_start);
        {
            var it = symbols_in_any_branch.keyIterator();
            while (it.next()) |key_ptr| {
                try self.scratch_keys.append(key_ptr.*);
            }
        }
        const sorted_keys = self.scratch_keys.sliceFromStart(keys_start);
        std.mem.sort(u64, sorted_keys, {}, std.sort.asc(u64));

        for (sorted_keys) |key| {
            const layout_idx = self.symbol_layouts.get(key) orelse continue;
            const symbol: Symbol = @bitCast(key);
            const local_count = local_uses.get(key) orelse 0;

            if (local_count == 0) {
                try self.emitDecrefInto(symbol, layout_idx, region, &rc_stmts);
            } else if (local_count > 1) {
                try self.emitIncrefInto(symbol, layout_idx, @intCast(local_count - 1), region, &rc_stmts);
            }
            // local_count == 1: no action needed
        }

        if (rc_stmts.items.len == 0) return body;

        // Wrap body in a block with RC stmts prepended
        const stmts_span = try self.store.addStmts(rc_stmts.items);
        return self.store.addExpr(.{ .block = .{
            .stmts = stmts_span,
            .final_expr = body,
            .result_layout = result_layout,
        } }, region);
    }

    /// Wrap a match guard with borrow-style increfs.
    /// Guard uses are "borrowed" — we incref(count) rather than incref(count-1)
    /// to preserve the inherited ref for the body or fallthrough path.
    fn wrapGuardWithIncref(
        self: *RcInsertPass,
        guard: MonoExprId,
        guard_uses: *const std.AutoHashMap(u64, u32),
        pattern_bound: *const std.AutoHashMap(u64, void),
        region: Region,
    ) Allocator.Error!MonoExprId {
        if (guard.isNone()) return guard;

        var rc_stmts = std.ArrayList(MonoStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        // Collect keys and sort for deterministic RC op ordering
        const keys_start = self.scratch_keys.top();
        defer self.scratch_keys.clearFrom(keys_start);
        {
            var it = guard_uses.iterator();
            while (it.next()) |entry| {
                try self.scratch_keys.append(entry.key_ptr.*);
            }
        }
        const sorted_keys = self.scratch_keys.sliceFromStart(keys_start);
        std.mem.sort(u64, sorted_keys, {}, std.sort.asc(u64));

        for (sorted_keys) |key| {
            const count = guard_uses.get(key) orelse 0;
            if (count == 0) continue;
            if (pattern_bound.contains(key)) continue;
            const layout_idx = self.symbol_layouts.get(key) orelse continue;
            if (!self.layoutNeedsRc(layout_idx)) continue;
            const symbol: Symbol = @bitCast(key);
            try self.emitIncrefInto(symbol, layout_idx, @intCast(count), region, &rc_stmts);
        }

        if (rc_stmts.items.len == 0) return guard;

        const stmts_span = try self.store.addStmts(rc_stmts.items);
        return self.store.addExpr(.{ .block = .{
            .stmts = stmts_span,
            .final_expr = guard,
            .result_layout = .bool,
        } }, region);
    }

    /// Recursively emit increfs for multi-use refcounted symbols bound by a pattern.
    /// Uses self.symbol_use_counts (global counts) — for use in processBlock.
    fn emitBlockIncrefsForPattern(
        self: *RcInsertPass,
        pat_id: MonoPatternId,
        region: Region,
        stmts: *std.ArrayList(MonoStmt),
    ) Allocator.Error!void {
        if (pat_id.isNone()) return;
        const pat = self.store.getPattern(pat_id);
        switch (pat) {
            .bind => |bind| {
                if (!bind.symbol.isNone() and self.layoutNeedsRc(bind.layout_idx)) {
                    const key = @as(u64, @bitCast(bind.symbol));
                    const use_count = self.symbol_use_counts.get(key) orelse 0;
                    if (use_count > 1) {
                        try self.emitIncrefInto(bind.symbol, bind.layout_idx, @intCast(use_count - 1), region, stmts);
                    }
                }
            },
            .as_pattern => |as_pat| {
                if (!as_pat.symbol.isNone() and self.layoutNeedsRc(as_pat.layout_idx)) {
                    const key = @as(u64, @bitCast(as_pat.symbol));
                    const use_count = self.symbol_use_counts.get(key) orelse 0;
                    if (use_count > 1) {
                        try self.emitIncrefInto(as_pat.symbol, as_pat.layout_idx, @intCast(use_count - 1), region, stmts);
                    }
                }
                try self.emitBlockIncrefsForPattern(as_pat.inner, region, stmts);
            },
            .tag => |t| {
                for (self.store.getPatternSpan(t.args)) |arg_pat| {
                    try self.emitBlockIncrefsForPattern(arg_pat, region, stmts);
                }
            },
            .record => |r| {
                for (self.store.getPatternSpan(r.fields)) |field_pat| {
                    try self.emitBlockIncrefsForPattern(field_pat, region, stmts);
                }
            },
            .tuple => |t| {
                for (self.store.getPatternSpan(t.elems)) |elem_pat| {
                    try self.emitBlockIncrefsForPattern(elem_pat, region, stmts);
                }
            },
            .list => |l| {
                for (self.store.getPatternSpan(l.prefix)) |pre_pat| {
                    try self.emitBlockIncrefsForPattern(pre_pat, region, stmts);
                }
                try self.emitBlockIncrefsForPattern(l.rest, region, stmts);
            },
            .wildcard, .int_literal, .float_literal, .str_literal => {},
        }
    }

    /// Recursively emit decrefs for unused refcounted symbols bound by a pattern.
    /// Uses self.symbol_use_counts (global counts) — for use in processBlock.
    fn emitBlockDecrefsForPattern(
        self: *RcInsertPass,
        pat_id: MonoPatternId,
        region: Region,
        stmts: *std.ArrayList(MonoStmt),
    ) Allocator.Error!void {
        if (pat_id.isNone()) return;
        const pat = self.store.getPattern(pat_id);
        switch (pat) {
            .bind => |bind| {
                if (!bind.symbol.isNone() and self.layoutNeedsRc(bind.layout_idx)) {
                    const key = @as(u64, @bitCast(bind.symbol));
                    const use_count = self.symbol_use_counts.get(key) orelse 0;
                    if (use_count == 0) {
                        try self.emitDecrefInto(bind.symbol, bind.layout_idx, region, stmts);
                    }
                }
            },
            .as_pattern => |as_pat| {
                if (!as_pat.symbol.isNone() and self.layoutNeedsRc(as_pat.layout_idx)) {
                    const key = @as(u64, @bitCast(as_pat.symbol));
                    const use_count = self.symbol_use_counts.get(key) orelse 0;
                    if (use_count == 0) {
                        try self.emitDecrefInto(as_pat.symbol, as_pat.layout_idx, region, stmts);
                    }
                }
                try self.emitBlockDecrefsForPattern(as_pat.inner, region, stmts);
            },
            .tag => |t| {
                for (self.store.getPatternSpan(t.args)) |arg_pat| {
                    try self.emitBlockDecrefsForPattern(arg_pat, region, stmts);
                }
            },
            .record => |r| {
                for (self.store.getPatternSpan(r.fields)) |field_pat| {
                    try self.emitBlockDecrefsForPattern(field_pat, region, stmts);
                }
            },
            .tuple => |t| {
                for (self.store.getPatternSpan(t.elems)) |elem_pat| {
                    try self.emitBlockDecrefsForPattern(elem_pat, region, stmts);
                }
            },
            .list => |l| {
                for (self.store.getPatternSpan(l.prefix)) |pre_pat| {
                    try self.emitBlockDecrefsForPattern(pre_pat, region, stmts);
                }
                try self.emitBlockDecrefsForPattern(l.rest, region, stmts);
            },
            .wildcard, .int_literal, .float_literal, .str_literal => {},
        }
    }

    /// Recursively emit RC ops for all symbols bound by a pattern.
    /// For each bound symbol with a refcounted layout:
    /// - use_count == 0: emit decref (release the provided reference)
    /// - use_count == 1: no action (consumes the provided reference)
    /// - use_count > 1: emit incref(count - 1)
    fn emitRcOpsForPatternInto(
        self: *RcInsertPass,
        pat_id: MonoPatternId,
        local_uses: *const std.AutoHashMap(u64, u32),
        region: Region,
        rc_stmts: *std.ArrayList(MonoStmt),
    ) Allocator.Error!void {
        if (pat_id.isNone()) return;
        const pat = self.store.getPattern(pat_id);
        switch (pat) {
            .bind => |bind| {
                if (!bind.symbol.isNone() and self.layoutNeedsRc(bind.layout_idx)) {
                    const key = @as(u64, @bitCast(bind.symbol));
                    const use_count = local_uses.get(key) orelse 0;
                    if (use_count == 0) {
                        try self.emitDecrefInto(bind.symbol, bind.layout_idx, region, rc_stmts);
                    } else if (use_count > 1) {
                        try self.emitIncrefInto(bind.symbol, bind.layout_idx, @intCast(use_count - 1), region, rc_stmts);
                    }
                }
            },
            .as_pattern => |as_pat| {
                if (!as_pat.symbol.isNone() and self.layoutNeedsRc(as_pat.layout_idx)) {
                    const key = @as(u64, @bitCast(as_pat.symbol));
                    const use_count = local_uses.get(key) orelse 0;
                    if (use_count == 0) {
                        try self.emitDecrefInto(as_pat.symbol, as_pat.layout_idx, region, rc_stmts);
                    } else if (use_count > 1) {
                        try self.emitIncrefInto(as_pat.symbol, as_pat.layout_idx, @intCast(use_count - 1), region, rc_stmts);
                    }
                }
                try self.emitRcOpsForPatternInto(as_pat.inner, local_uses, region, rc_stmts);
            },
            .tag => |t| {
                for (self.store.getPatternSpan(t.args)) |arg_pat| {
                    try self.emitRcOpsForPatternInto(arg_pat, local_uses, region, rc_stmts);
                }
            },
            .record => |r| {
                for (self.store.getPatternSpan(r.fields)) |field_pat| {
                    try self.emitRcOpsForPatternInto(field_pat, local_uses, region, rc_stmts);
                }
            },
            .tuple => |t| {
                for (self.store.getPatternSpan(t.elems)) |elem_pat| {
                    try self.emitRcOpsForPatternInto(elem_pat, local_uses, region, rc_stmts);
                }
            },
            .list => |l| {
                for (self.store.getPatternSpan(l.prefix)) |pre_pat| {
                    try self.emitRcOpsForPatternInto(pre_pat, local_uses, region, rc_stmts);
                }
                try self.emitRcOpsForPatternInto(l.rest, local_uses, region, rc_stmts);
            },
            .wildcard, .int_literal, .float_literal, .str_literal => {},
        }
    }

    /// Emit an incref statement into a given statement list.
    fn emitIncrefInto(self: *RcInsertPass, symbol: Symbol, layout_idx: LayoutIdx, count: u16, region: Region, stmts: *std.ArrayList(MonoStmt)) Allocator.Error!void {
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
        try stmts.append(self.allocator, .{
            .pattern = wildcard,
            .expr = incref_id,
        });
    }

    /// Emit a decref statement into a given statement list.
    fn emitDecrefInto(self: *RcInsertPass, symbol: Symbol, layout_idx: LayoutIdx, region: Region, stmts: *std.ArrayList(MonoStmt)) Allocator.Error!void {
        const lookup_id = try self.store.addExpr(.{ .lookup = .{
            .symbol = symbol,
            .layout_idx = layout_idx,
        } }, region);

        const decref_id = try self.store.addExpr(.{ .decref = .{
            .value = lookup_id,
            .layout_idx = layout_idx,
        } }, region);

        const wildcard = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = layout_idx } }, region);
        try stmts.append(self.allocator, .{
            .pattern = wildcard,
            .expr = decref_id,
        });
    }
};

test "RcInsertPass compiles" {
    const T = RcInsertPass;
    std.debug.assert(@sizeOf(T) > 0);
}

// --- Test helpers ---

fn testInit() !struct { store: MonoExprStore, layout_store: layout_mod.Store, module_env: @import("can").ModuleEnv, module_env_ptrs: [1]*const @import("can").ModuleEnv } {
    const allocator = std.testing.allocator;
    var result: @TypeOf(testInit() catch unreachable) = undefined;
    result.module_env = try @import("can").ModuleEnv.init(allocator, "");
    result.store = MonoExprStore.init(allocator);
    return result;
}

fn testInitLayoutStore(self: *@TypeOf(testInit() catch unreachable)) !void {
    self.module_env_ptrs[0] = &self.module_env;
    self.layout_store = try layout_mod.Store.init(&self.module_env_ptrs, null, std.testing.allocator, @import("base").target.TargetUsize.native);
}

fn testDeinit(self: *@TypeOf(testInit() catch unreachable)) void {
    self.layout_store.deinit();
    self.store.deinit();
    self.module_env.deinit();
}

fn makeSymbol(idx: u29) Symbol {
    return .{
        .module_idx = 0,
        .ident_idx = base.Ident.Idx{
            .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
            .idx = idx,
        },
    };
}

const RcOpCounts = struct { increfs: u32, decrefs: u32 };

fn countRcOps(store: *const MonoExprStore, expr_id: MonoExprId) RcOpCounts {
    if (expr_id.isNone()) return .{ .increfs = 0, .decrefs = 0 };
    const expr = store.getExpr(expr_id);
    var increfs: u32 = 0;
    var decrefs: u32 = 0;
    switch (expr) {
        .incref => increfs += 1,
        .decref => decrefs += 1,
        .block => |block| {
            const stmts = store.getStmts(block.stmts);
            for (stmts) |stmt| {
                const sub = countRcOps(store, stmt.expr);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
            const sub = countRcOps(store, block.final_expr);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .lambda => |lam| {
            const sub = countRcOps(store, lam.body);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .closure => |clo| {
            const sub = countRcOps(store, clo.lambda);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .match_expr => |w| {
            const branches = store.getMatchBranches(w.branches);
            for (branches) |branch| {
                const guard_sub = countRcOps(store, branch.guard);
                increfs += guard_sub.increfs;
                decrefs += guard_sub.decrefs;
                const sub = countRcOps(store, branch.body);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
        },
        .if_then_else => |ite| {
            const branches = store.getIfBranches(ite.branches);
            for (branches) |branch| {
                const sub = countRcOps(store, branch.body);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
            const sub = countRcOps(store, ite.final_else);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .for_loop => |fl| {
            const sub = countRcOps(store, fl.body);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        else => {},
    }
    return .{ .increfs = increfs, .decrefs = decrefs };
}

test "RC pass-through: non-refcounted i64 block unchanged" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    // Build: { x = 42; x }
    const i64_layout: LayoutIdx = .i64;

    const sym_x = makeSymbol(1);

    const int_lit = try env.store.addExpr(.{ .i64_literal = 42 }, Region.zero());
    const pat_x = try env.store.addPattern(.{ .bind = .{ .symbol = sym_x, .layout_idx = i64_layout } }, Region.zero());
    const stmts = try env.store.addStmts(&.{.{ .pattern = pat_x, .expr = int_lit }});
    const lookup_x = try env.store.addExpr(.{ .lookup = .{ .symbol = sym_x, .layout_idx = i64_layout } }, Region.zero());

    const block_expr = try env.store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup_x,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const result_expr = env.store.getExpr(result);
    try std.testing.expect(result_expr == .block);

    // No RC ops should have been added — statement count should be 1
    const result_stmts = env.store.getStmts(result_expr.block.stmts);
    try std.testing.expectEqual(@as(usize, 1), result_stmts.len);
}

test "RC: string binding used twice gets incref" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    // Build: { s = "hello"; _ = s; s }
    // where s is used twice (the use(s) stmt + final lookup)
    const str_layout: LayoutIdx = .str;

    const sym_s = makeSymbol(1);

    const str_lit = try env.store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    // Two lookups to get use_count = 2
    const lookup1 = try env.store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup2 = try env.store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    // Statement: s = "hello"
    // Statement: _ = s (use to bump count)
    const wildcard = try env.store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.store.addStmts(&.{
        .{ .pattern = pat_s, .expr = str_lit },
        .{ .pattern = wildcard, .expr = lookup1 },
    });

    const block_expr = try env.store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup2,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const rc = countRcOps(&env.store, result);

    // s used twice => incref(1) should be inserted
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
}

test "RC lambda: body uses don't inflate outer scope" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    // Build: { s = "hello"; f = |x| { _ = s; _ = s; s }; s }
    // s is used 3 times inside lambda body, but the lambda only captures
    // s once. The outer scope should see 2 uses of s (1 capture + 1
    // final_expr), NOT 4 uses (3 body + 1 final_expr).
    // Bug #13: without proper scoping, outer scope saw 4 uses and would
    // emit incref(3) instead of the correct incref(1).
    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;

    const sym_s = makeSymbol(1);
    const sym_x = makeSymbol(2);
    const sym_f = makeSymbol(3);

    // s = "hello"
    const str_lit = try env.store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    // Lambda body: { _ = s; _ = s; s } — uses s three times
    const lookup_s_1 = try env.store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s_2 = try env.store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s_3 = try env.store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const wild1 = try env.store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const wild2 = try env.store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const body_stmts = try env.store.addStmts(&.{
        .{ .pattern = wild1, .expr = lookup_s_1 },
        .{ .pattern = wild2, .expr = lookup_s_2 },
    });
    const lambda_body = try env.store.addExpr(.{ .block = .{
        .stmts = body_stmts,
        .final_expr = lookup_s_3,
        .result_layout = str_layout,
    } }, Region.zero());

    // Lambda: |x| { _ = s; _ = s; s }
    const pat_x = try env.store.addPattern(.{ .bind = .{ .symbol = sym_x, .layout_idx = i64_layout } }, Region.zero());
    const params = try env.store.addPatternSpan(&.{pat_x});
    const lambda_expr = try env.store.addExpr(.{ .lambda = .{
        .fn_layout = i64_layout,
        .params = params,
        .body = lambda_body,
        .ret_layout = str_layout,
    } }, Region.zero());

    // f = |x| { _ = s; _ = s; s }
    const pat_f = try env.store.addPattern(.{ .bind = .{ .symbol = sym_f, .layout_idx = i64_layout } }, Region.zero());

    // Final expr: s (one use in outer scope)
    const lookup_s_outer = try env.store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    const stmts = try env.store.addStmts(&.{
        .{ .pattern = pat_s, .expr = str_lit },
        .{ .pattern = pat_f, .expr = lambda_expr },
    });

    const block_expr = try env.store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup_s_outer,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const rc = countRcOps(&env.store, result);

    // Correct scoping: outer scope sees 2 uses of s (1 capture + 1 final_expr)
    // => incref(1) at binding site. Lambda body doesn't independently RC the
    // captured s (it's not locally bound), so total is 1 incref.
    // Without scoping fix (bug #13): outer scope would see 4 uses (3 body
    // uses leaked into outer scope + 1 final_expr) and emit incref(3).
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
}
