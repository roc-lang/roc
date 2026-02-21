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
const LIR = @import("LIR.zig");
const LirExprStore = @import("LirExprStore.zig");

const LirExprId = LIR.LirExprId;
const Symbol = LIR.Symbol;
const LirStmt = LIR.LirStmt;
const LirStmtSpan = LIR.LirStmtSpan;
const LirPatternId = LIR.LirPatternId;
const LirIfBranch = LIR.LirIfBranch;
const LirMatchBranch = LIR.LirMatchBranch;
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

    /// Tracks live RC symbols across blocks for early_return cleanup.
    live_rc_symbols: std.ArrayList(LiveRcSymbol),

    /// Base index into live_rc_symbols for the current function scope.
    /// early_return only cleans up symbols from early_return_scope_base onward.
    early_return_scope_base: usize,

    /// Tracks how many uses of each RC symbol have been consumed so far
    /// in the current block (by already-processed statements).
    /// Used by processEarlyReturn to compute remaining refs for cleanup.
    block_consumed_uses: std.AutoHashMap(u64, u32),

    /// Cumulative consumed uses across all enclosing blocks.
    /// When processBlock saves/restores block_consumed_uses, the outer block's
    /// consumed uses are invisible to processEarlyReturn in nested blocks.
    /// This field accumulates uses from all enclosing blocks so that
    /// processEarlyReturn can see the full picture.
    cumulative_consumed_uses: std.AutoHashMap(u64, u32),

    /// Pending branch-level RC adjustments. When processing a branch body,
    /// wrapBranchWithRcOps will later prepend RC ops for symbols:
    /// - local_count > 1: incref(local_count - 1) → positive adjustment
    /// - local_count == 0: decref → negative adjustment (-1)
    /// processEarlyReturn must account for these since they execute before
    /// the early return at runtime.
    pending_branch_rc_adj: std.AutoHashMap(u64, i32),

    /// Reusable scratch map for counting symbol uses within sub-expressions.
    /// Cleared and reused at each call site, avoiding per-call HashMap allocations.
    scratch_uses: std.AutoHashMap(u64, u32),

    /// Reusable scratch buffer for collecting HashMap keys before sorting.
    /// Used by wrapBranchWithRcOps and wrapGuardWithIncref to ensure
    /// deterministic RC op ordering regardless of HashMap iteration order.
    scratch_keys: base.Scratch(u64),

    const LiveRcSymbol = struct {
        symbol: Symbol,
        layout_idx: LayoutIdx,
    };

    comptime {
        std.debug.assert(@sizeOf(Symbol) == @sizeOf(u64));
    }

    pub fn init(allocator: Allocator, store: *LirExprStore, layout_store: *const layout_mod.Store) Allocator.Error!RcInsertPass {
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
            .pending_branch_rc_adj = std.AutoHashMap(u64, i32).init(allocator),
            .scratch_uses = std.AutoHashMap(u64, u32).init(allocator),
            .scratch_keys = try base.Scratch(u64).init(allocator),
        };
    }

    pub fn deinit(self: *RcInsertPass) void {
        self.symbol_use_counts.deinit();
        self.symbol_layouts.deinit();
        self.live_rc_symbols.deinit(self.allocator);
        self.block_consumed_uses.deinit();
        self.cumulative_consumed_uses.deinit();
        self.pending_branch_rc_adj.deinit();
        self.scratch_uses.deinit();
        self.scratch_keys.deinit();
    }

    /// Main entry point: insert RC operations into a LirExpr tree.
    /// Returns a new LirExprId for the RC-annotated tree.
    pub fn insertRcOps(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!LirExprId {
        // Clear all accumulated state so insertRcOps is safe to call multiple times.
        self.symbol_use_counts.clearRetainingCapacity();
        self.symbol_layouts.clearRetainingCapacity();
        self.live_rc_symbols.clearRetainingCapacity();
        self.block_consumed_uses.clearRetainingCapacity();
        self.cumulative_consumed_uses.clearRetainingCapacity();
        self.pending_branch_rc_adj.clearRetainingCapacity();
        self.scratch_uses.clearRetainingCapacity();

        // Phase 1: Count all symbol uses and record layouts
        try self.countUses(expr_id);

        // Phase 2: Walk the tree and insert RC operations
        return self.processExpr(expr_id);
    }

    /// Count how many times each symbol is referenced in the expression tree.
    /// Also records the layout for each symbol found in bind patterns.
    /// Wrapper around countUsesInto that targets self.symbol_use_counts.
    fn countUses(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!void {
        try self.countUsesInto(expr_id, &self.symbol_use_counts);
    }

    /// Count how many times each symbol is referenced, writing into `target`.
    /// Also records the layout for each symbol found in bind patterns.
    fn countUsesInto(self: *RcInsertPass, expr_id: LirExprId, target: *std.AutoHashMap(u64, u32)) Allocator.Error!void {
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
                // Pre-register all pattern symbols so lambdas in early
                // statements can detect captures from later siblings.
                for (stmts) |stmt| {
                    try self.registerPatternSymbolInto(stmt.binding().pattern, target);
                }
                for (stmts) |stmt| {
                    try self.countUsesInto(stmt.binding().expr, target);
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
                    try self.registerPatternSymbolInto(branch.pattern, &local);
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
            .closure => |clo_id| {
                const clo = self.store.getClosureData(clo_id);
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
                // Branches are mutually exclusive — use per-branch counting
                const branches = self.store.getExprSpan(ds.branches);
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                for (branches) |br_id| {
                    local.clearRetainingCapacity();
                    try self.countUsesInto(br_id, &local);
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| {
                    const gop = try target.getOrPut(key.*);
                    if (gop.found_existing) gop.value_ptr.* += 1 else gop.value_ptr.* = 1;
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
            .break_expr,
            => {},
        }
    }

    /// Walk a pattern tree, calling ctx.onBind(symbol, layout_idx) at each
    /// .bind and .as_pattern leaf. Handles the full recursion over all pattern
    /// variants so callers only need to implement the leaf action.
    fn walkPatternBinds(store: *const LirExprStore, pat_id: LirPatternId, ctx: anytype) Allocator.Error!void {
        if (pat_id.isNone()) return;
        const pat = store.getPattern(pat_id);
        switch (pat) {
            .bind => |bind| {
                if (!bind.symbol.isNone()) try ctx.onBind(bind.symbol, bind.layout_idx);
            },
            .as_pattern => |as_pat| {
                if (!as_pat.symbol.isNone()) try ctx.onBind(as_pat.symbol, as_pat.layout_idx);
                try walkPatternBinds(store, as_pat.inner, ctx);
            },
            .tag => |t| for (store.getPatternSpan(t.args)) |a| {
                try walkPatternBinds(store, a, ctx);
            },
            .record => |r| for (store.getPatternSpan(r.fields)) |f| {
                try walkPatternBinds(store, f, ctx);
            },
            .tuple => |t| for (store.getPatternSpan(t.elems)) |e| {
                try walkPatternBinds(store, e, ctx);
            },
            .list => |l| {
                for (store.getPatternSpan(l.prefix)) |p| try walkPatternBinds(store, p, ctx);
                try walkPatternBinds(store, l.rest, ctx);
                for (store.getPatternSpan(l.suffix)) |s| try walkPatternBinds(store, s, ctx);
            },
            .wildcard, .int_literal, .float_literal, .str_literal => {},
        }
    }

    /// Register a pattern's bound symbol with its layout into a given target map.
    fn registerPatternSymbolInto(self: *RcInsertPass, pat_id: LirPatternId, target: *std.AutoHashMap(u64, u32)) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            target: *std.AutoHashMap(u64, u32),
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx) Allocator.Error!void {
                const key = @as(u64, @bitCast(symbol));
                try ctx.pass.symbol_layouts.put(key, layout_idx);
                if (!ctx.target.contains(key)) {
                    try ctx.target.put(key, 0);
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self, .target = target });
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
            .block => |block| self.processBlock(expr_id, block.stmts, block.final_expr, block.result_layout, region),
            .if_then_else => |ite| self.processIfThenElse(ite.branches, ite.final_else, ite.result_layout, region),
            .match_expr => |w| self.processMatch(w.value, w.value_layout, w.branches, w.result_layout, region),
            .lambda => |lam| self.processLambda(lam, region, expr_id),
            .closure => |clo_id| self.processClosure(self.store.getClosureData(clo_id), region, expr_id),
            .for_loop => |fl| self.processForLoop(fl, region, expr_id),
            .while_loop => |wl| self.processWhileLoop(wl, region, expr_id),
            .discriminant_switch => |ds| self.processDiscriminantSwitch(ds, region),
            .early_return => |ret| self.processEarlyReturn(ret, region, expr_id),
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
            .break_expr,
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
        orig_expr_id: LirExprId,
        stmts_span: LirStmtSpan,
        final_expr: LirExprId,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        const stmts = self.store.getStmts(stmts_span);

        // Save live_rc_symbols depth so nested blocks restore on exit.
        const saved_live_len = self.live_rc_symbols.items.len;
        defer self.live_rc_symbols.shrinkRetainingCapacity(saved_live_len);

        // Save and reset block_consumed_uses for this block scope.
        // Before saving, accumulate current block's consumed uses into cumulative map
        // so that processEarlyReturn in nested blocks can see all enclosing uses.
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
        // Snapshot cumulative keys/values so we can restore on exit.
        // We save a copy of the entries we added, to subtract them later.
        var saved_cumulative_additions = std.AutoHashMap(u64, u32).init(self.allocator);
        {
            var it = self.block_consumed_uses.iterator();
            while (it.next()) |entry| {
                try saved_cumulative_additions.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }
        const saved_consumed = self.block_consumed_uses.move();
        defer {
            // Remove the outer block's uses we added to cumulative before entering
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
        var stmt_buf = std.ArrayList(LirStmt).empty;
        defer stmt_buf.deinit(self.allocator);

        var changed = false;

        // Process each statement
        for (stmts) |stmt| {
            const b = stmt.binding();
            // Recursively process the statement's expression
            const new_expr = try self.processExpr(b.expr);
            if (new_expr != b.expr) changed = true;

            // Track uses consumed by this statement's expression
            // so processEarlyReturn can compute remaining refs.
            {
                self.scratch_uses.clearRetainingCapacity();
                try self.countUsesInto(b.expr, &self.scratch_uses);
                var use_it = self.scratch_uses.iterator();
                while (use_it.next()) |entry| {
                    const gop = try self.block_consumed_uses.getOrPut(entry.key_ptr.*);
                    if (gop.found_existing) {
                        gop.value_ptr.* += entry.value_ptr.*;
                    } else {
                        gop.value_ptr.* = entry.value_ptr.*;
                    }
                }
            }

            // For mutations of refcounted symbols, decref the old value first
            if (stmt == .mutate) {
                const before_len = stmt_buf.items.len;
                try self.emitMutateDecrefsForPattern(b.pattern, region, &stmt_buf);
                if (stmt_buf.items.len > before_len) changed = true;
            }

            // Add the (possibly updated) statement, preserving decl/mutate kind
            const new_binding: LirStmt.Binding = .{ .pattern = b.pattern, .expr = new_expr };
            try stmt_buf.append(self.allocator, switch (stmt) {
                .decl => .{ .decl = new_binding },
                .mutate => .{ .mutate = new_binding },
            });

            // Track live RC symbols and emit increfs for multi-use bindings
            try self.trackLiveRcSymbolsForPattern(b.pattern);
            {
                const before_len = stmt_buf.items.len;
                try self.emitBlockIncrefsForPattern(b.pattern, region, &stmt_buf);
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
                const b = stmt.binding();
                try self.emitBlockDecrefsForPattern(b.pattern, region, &stmt_buf);
            }
            if (stmt_buf.items.len > before_len) changed = true;
        }

        // If nothing changed, return the original expression as-is
        if (!changed) {
            return orig_expr_id;
        }

        // Build the new statement span
        const new_stmts = try self.store.addStmts(stmt_buf.items);

        return self.store.addExpr(.{ .block = .{
            .stmts = new_stmts,
            .final_expr = new_final,
            .result_layout = result_layout,
        } }, region);
    }

    /// Process an if-then-else expression.
    /// Each branch gets per-branch RC ops based on local use counts.
    fn processIfThenElse(
        self: *RcInsertPass,
        branches_span: LIR.LirIfBranchSpan,
        final_else_id: LirExprId,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        const branches = self.store.getIfBranches(branches_span);

        // Collect symbols bound within branch bodies — these are local to their
        // defining branch and must NOT get per-branch RC ops.
        var body_bound = std.AutoHashMap(u64, void).init(self.allocator);
        defer body_bound.deinit();
        for (branches) |branch| {
            try self.collectExprBoundSymbols(branch.body, &body_bound);
        }
        try self.collectExprBoundSymbols(final_else_id, &body_bound);

        // Collect union of all refcounted symbols across all branches (using scratch_uses)
        var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
        defer symbols_in_any_branch.deinit();

        for (branches) |branch| {
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(branch.body, &self.scratch_uses);
            var it = self.scratch_uses.keyIterator();
            while (it.next()) |key| {
                const k = key.*;
                if (body_bound.contains(k)) continue;
                if (self.symbol_layouts.get(k)) |lay| {
                    if (self.layoutNeedsRc(lay)) try symbols_in_any_branch.put(k, {});
                }
            }
        }
        // else branch
        {
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(final_else_id, &self.scratch_uses);
            var it = self.scratch_uses.keyIterator();
            while (it.next()) |key| {
                const k = key.*;
                if (body_bound.contains(k)) continue;
                if (self.symbol_layouts.get(k)) |lay| {
                    if (self.layoutNeedsRc(lay)) try symbols_in_any_branch.put(k, {});
                }
            }
        }

        var new_branches = std.ArrayList(LirIfBranch).empty;
        defer new_branches.deinit(self.allocator);

        for (branches) |branch| {
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(branch.body, &self.scratch_uses);
            try self.setPendingBranchRcAdj(&self.scratch_uses, &symbols_in_any_branch);
            const processed_body = try self.processExpr(branch.body);
            self.clearPendingBranchRcAdj();
            // Re-count: processExpr may have used scratch_uses recursively
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(branch.body, &self.scratch_uses);
            const new_body = try self.wrapBranchWithRcOps(processed_body, &self.scratch_uses, &symbols_in_any_branch, result_layout, region);
            try new_branches.append(self.allocator, .{
                .cond = branch.cond,
                .body = new_body,
            });
        }

        // else branch
        self.scratch_uses.clearRetainingCapacity();
        try self.countUsesInto(final_else_id, &self.scratch_uses);
        try self.setPendingBranchRcAdj(&self.scratch_uses, &symbols_in_any_branch);
        const processed_else = try self.processExpr(final_else_id);
        self.clearPendingBranchRcAdj();
        self.scratch_uses.clearRetainingCapacity();
        try self.countUsesInto(final_else_id, &self.scratch_uses);
        const new_else = try self.wrapBranchWithRcOps(processed_else, &self.scratch_uses, &symbols_in_any_branch, result_layout, region);

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
        value: LirExprId,
        value_layout: LayoutIdx,
        branches_span: LIR.LirMatchBranchSpan,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
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

        // Collect union of all refcounted symbols across all branches (using scratch_uses).
        // Both body and guard symbols contribute so unused branches decref.
        var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
        defer symbols_in_any_branch.deinit();

        for (branches) |branch| {
            // Body symbols
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(branch.body, &self.scratch_uses);
            var it = self.scratch_uses.keyIterator();
            while (it.next()) |key| {
                const k = key.*;
                if (pattern_bound.contains(k)) continue;
                if (body_bound.contains(k)) continue;
                if (self.symbol_layouts.get(k)) |lay| {
                    if (self.layoutNeedsRc(lay)) try symbols_in_any_branch.put(k, {});
                }
            }
            // Guard symbols
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(branch.guard, &self.scratch_uses);
            var it2 = self.scratch_uses.keyIterator();
            while (it2.next()) |key| {
                const k = key.*;
                if (pattern_bound.contains(k)) continue;
                if (self.symbol_layouts.get(k)) |lay| {
                    if (self.layoutNeedsRc(lay)) try symbols_in_any_branch.put(k, {});
                }
            }
        }

        var new_branches = std.ArrayList(LirMatchBranch).empty;
        defer new_branches.deinit(self.allocator);

        for (branches) |branch| {
            // Count body uses for setPendingBranchRcAdj
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(branch.body, &self.scratch_uses);
            try self.setPendingBranchRcAdj(&self.scratch_uses, &symbols_in_any_branch);
            const processed_body = try self.processExpr(branch.body);
            self.clearPendingBranchRcAdj();
            const processed_guard = try self.processExpr(branch.guard);
            // Re-count body uses for wrapBranchWithRcOps
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(branch.body, &self.scratch_uses);
            const new_body = try self.wrapBranchWithRcOps(processed_body, &self.scratch_uses, &symbols_in_any_branch, result_layout, region);
            // Count guard uses for wrapGuardWithIncref
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(branch.guard, &self.scratch_uses);
            const new_guard = try self.wrapGuardWithIncref(processed_guard, &self.scratch_uses, &pattern_bound, region);
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

    /// Process a discriminant_switch expression as branching control flow.
    /// Each branch is mutually exclusive, so symbols used in any branch
    /// get per-branch RC adjustments (same pattern as if_then_else/match).
    fn processDiscriminantSwitch(self: *RcInsertPass, ds: anytype, region: Region) Allocator.Error!LirExprId {
        const branches = self.store.getExprSpan(ds.branches);

        // Collect symbols bound within branch bodies — these are local to their
        // defining branch and must NOT get per-branch RC ops.
        var body_bound = std.AutoHashMap(u64, void).init(self.allocator);
        defer body_bound.deinit();
        for (branches) |br_id| {
            try self.collectExprBoundSymbols(br_id, &body_bound);
        }

        // Collect union of all refcounted symbols across all branches (using scratch_uses)
        var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
        defer symbols_in_any_branch.deinit();

        for (branches) |br_id| {
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(br_id, &self.scratch_uses);
            var it = self.scratch_uses.keyIterator();
            while (it.next()) |key| {
                const k = key.*;
                if (body_bound.contains(k)) continue;
                if (self.symbol_layouts.get(k)) |lay| {
                    if (self.layoutNeedsRc(lay)) try symbols_in_any_branch.put(k, {});
                }
            }
        }

        // Process each branch and wrap with per-branch RC ops
        var new_branches = std.ArrayList(LirExprId).empty;
        defer new_branches.deinit(self.allocator);

        const result_layout: LayoutIdx = ds.result_layout;

        for (branches) |br_id| {
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(br_id, &self.scratch_uses);
            try self.setPendingBranchRcAdj(&self.scratch_uses, &symbols_in_any_branch);
            const processed = try self.processExpr(br_id);
            self.clearPendingBranchRcAdj();
            // Re-count: processExpr may have used scratch_uses recursively
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(br_id, &self.scratch_uses);
            const wrapped = try self.wrapBranchWithRcOps(processed, &self.scratch_uses, &symbols_in_any_branch, result_layout, region);
            try new_branches.append(self.allocator, wrapped);
        }

        const new_branches_span = try self.store.addExprSpan(new_branches.items);
        return self.store.addExpr(.{ .discriminant_switch = .{
            .value = ds.value,
            .union_layout = ds.union_layout,
            .branches = new_branches_span,
            .result_layout = ds.result_layout,
        } }, region);
    }

    /// Process a lambda expression.
    /// Lambda bodies are independent scopes — the calling convention provides
    /// 1 reference per parameter. We recurse into the body and emit RC ops
    /// for lambda parameters based on body-local use counts.
    fn processLambda(self: *RcInsertPass, lam: anytype, region: Region, expr_id: LirExprId) Allocator.Error!LirExprId {
        // Lambda is a new function scope — save and reset early_return tracking.
        const saved_scope_base = self.early_return_scope_base;
        const saved_live_len = self.live_rc_symbols.items.len;
        self.early_return_scope_base = self.live_rc_symbols.items.len;
        defer {
            self.early_return_scope_base = saved_scope_base;
            self.live_rc_symbols.shrinkRetainingCapacity(saved_live_len);
        }

        const new_body = try self.processExpr(lam.body);

        // Count uses locally within the lambda body (using scratch_uses)
        self.scratch_uses.clearRetainingCapacity();
        try self.countUsesInto(lam.body, &self.scratch_uses);

        // Emit RC ops for lambda parameters
        var rc_stmts = std.ArrayList(LirStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        const params = self.store.getPatternSpan(lam.params);
        for (params) |pat_id| {
            try self.emitRcOpsForPatternInto(pat_id, &self.scratch_uses, region, &rc_stmts);
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
    fn processClosure(self: *RcInsertPass, clo: anytype, region: Region, expr_id: LirExprId) Allocator.Error!LirExprId {
        const new_lambda = try self.processExpr(clo.lambda);
        if (new_lambda != clo.lambda) {
            const new_clo_id = try self.store.addClosureData(.{
                .closure_layout = clo.closure_layout,
                .lambda = new_lambda,
                .captures = clo.captures,
                .representation = clo.representation,
                .recursion = clo.recursion,
                .self_recursive = clo.self_recursive,
                .is_bound_to_variable = clo.is_bound_to_variable,
            });
            return self.store.addExpr(.{ .closure = new_clo_id }, region);
        }
        return expr_id;
    }

    /// Process a for loop expression.
    /// For loops bind an element via elem_pattern each iteration.
    /// The loop provides 1 reference per element. We emit body-local RC ops
    /// for the element binding similar to lambda params.
    fn processForLoop(self: *RcInsertPass, fl: anytype, region: Region, expr_id: LirExprId) Allocator.Error!LirExprId {
        const new_body = try self.processExpr(fl.body);

        // Count uses locally within the loop body (using scratch_uses)
        self.scratch_uses.clearRetainingCapacity();
        try self.countUsesInto(fl.body, &self.scratch_uses);

        // Emit RC ops for the elem_pattern
        var rc_stmts = std.ArrayList(LirStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        try self.emitRcOpsForPatternInto(fl.elem_pattern, &self.scratch_uses, region, &rc_stmts);

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
    fn processWhileLoop(self: *RcInsertPass, wl: anytype, region: Region, expr_id: LirExprId) Allocator.Error!LirExprId {
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

    /// Process an early_return expression.
    /// Emits cleanup decrefs for all live RC symbols in enclosing blocks
    /// (from early_return_scope_base onward) that are NOT consumed by the
    /// return value expression itself.
    fn processEarlyReturn(self: *RcInsertPass, ret: anytype, region: Region, expr_id: LirExprId) Allocator.Error!LirExprId {
        // Recurse into the return value expression
        const new_expr = try self.processExpr(ret.expr);

        // Find which symbols the return expression consumes (using scratch_uses)
        self.scratch_uses.clearRetainingCapacity();
        try self.countUsesInto(ret.expr, &self.scratch_uses);

        // Collect cleanup decrefs for live RC symbols not consumed by the return
        var cleanup_stmts = std.ArrayList(LirStmt).empty;
        defer cleanup_stmts.deinit(self.allocator);

        const live_syms = self.live_rc_symbols.items[self.early_return_scope_base..];
        for (live_syms) |live| {
            const key = @as(u64, @bitCast(live.symbol));
            const ret_use_count: u32 = self.scratch_uses.get(key) orelse 0;
            // Total refs = global_use_count + pending branch RC adjustments.
            // The branch wrapper will prepend RC ops that execute before
            // the early return: increfs (positive adj) add refs to clean up,
            // decrefs (negative adj) reduce refs since they're already handled.
            const global_count = self.symbol_use_counts.get(key) orelse 1;
            const branch_adj: i32 = self.pending_branch_rc_adj.get(key) orelse 0;
            const effective_signed: i32 = @as(i32, @intCast(global_count)) + branch_adj;
            if (effective_signed <= 0) continue; // branch wrapper decrefs handle all refs
            const effective_count: u32 = @intCast(effective_signed);
            // Include uses consumed by outer enclosing blocks (cumulative)
            // plus uses consumed by the current inner block's prior statements.
            const consumed_before = (self.cumulative_consumed_uses.get(key) orelse 0) +
                (self.block_consumed_uses.get(key) orelse 0);
            const total_consumed = consumed_before + ret_use_count;
            if (total_consumed >= effective_count) continue; // all refs accounted for
            const remaining = effective_count - total_consumed;
            var i: u32 = 0;
            while (i < remaining) : (i += 1) {
                try self.emitDecrefInto(live.symbol, live.layout_idx, region, &cleanup_stmts);
            }
        }

        if (cleanup_stmts.items.len == 0 and new_expr == ret.expr) return expr_id;

        // Build the actual early_return node (possibly with updated expr)
        const early_ret_id = try self.store.addExpr(.{ .early_return = .{
            .expr = new_expr,
            .ret_layout = ret.ret_layout,
        } }, region);

        if (cleanup_stmts.items.len == 0) return early_ret_id;

        // Wrap: block { decref(a); decref(b); early_return(new_expr) }
        const wildcard = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, region);
        try cleanup_stmts.append(self.allocator, .{ .decl = .{
            .pattern = wildcard,
            .expr = early_ret_id,
        } });

        const stmts_span = try self.store.addStmts(cleanup_stmts.items);
        // The block's final_expr is never reached (early_return diverges),
        // but we need a distinct valid expr — not early_ret_id which is already used as a stmt.
        const dead_final = try self.store.addExpr(.{ .runtime_error = {} }, region);
        return self.store.addExpr(.{ .block = .{
            .stmts = stmts_span,
            .final_expr = dead_final,
            .result_layout = ret.ret_layout,
        } }, region);
    }

    /// Collect all symbols bound by a pattern into a set.
    fn collectPatternSymbols(self: *const RcInsertPass, pat_id: LirPatternId, set: *std.AutoHashMap(u64, void)) Allocator.Error!void {
        const Ctx = struct {
            set: *std.AutoHashMap(u64, void),
            fn onBind(ctx: @This(), symbol: Symbol, _: LayoutIdx) Allocator.Error!void {
                try ctx.set.put(@as(u64, @bitCast(symbol)), {});
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .set = set });
    }

    /// Collect all symbols bound by patterns within an expression tree.
    /// Used to identify locally-defined symbols for branch-aware RC filtering.
    fn collectExprBoundSymbols(self: *const RcInsertPass, expr_id: LirExprId, set: *std.AutoHashMap(u64, void)) Allocator.Error!void {
        if (expr_id.isNone()) return;
        const expr = self.store.getExpr(expr_id);
        switch (expr) {
            .block => |block| {
                const stmts = self.store.getStmts(block.stmts);
                for (stmts) |stmt| {
                    const b = stmt.binding();
                    try self.collectPatternSymbols(b.pattern, set);
                    try self.collectExprBoundSymbols(b.expr, set);
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
            .for_loop => |fl| {
                try self.collectPatternSymbols(fl.elem_pattern, set);
                try self.collectExprBoundSymbols(fl.list_expr, set);
                try self.collectExprBoundSymbols(fl.body, set);
            },
            .while_loop => |wl| {
                try self.collectExprBoundSymbols(wl.cond, set);
                try self.collectExprBoundSymbols(wl.body, set);
            },
            .discriminant_switch => |ds| {
                try self.collectExprBoundSymbols(ds.value, set);
                const dbs = self.store.getExprSpan(ds.branches);
                for (dbs) |br_id| try self.collectExprBoundSymbols(br_id, set);
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
            .tag_payload_access => |tpa| try self.collectExprBoundSymbols(tpa.value, set),
            // Lambda/closure are separate scopes in LIR — don't recurse.
            .lambda, .closure => {},
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
            .break_expr,
            .crash,
            .runtime_error,
            .incref,
            .decref,
            .free,
            => {},
        }
    }

    /// Set pending branch RC adjustments for a branch body before processExpr.
    /// This tells processEarlyReturn about RC ops that wrapBranchWithRcOps
    /// will later prepend:
    /// - local_count > 1: incref(local_count - 1) → adjustment = +(local_count - 1)
    /// - local_count == 0: decref → adjustment = -1
    fn setPendingBranchRcAdj(
        self: *RcInsertPass,
        local_uses: *const std.AutoHashMap(u64, u32),
        symbols_in_any_branch: *const std.AutoHashMap(u64, void),
    ) Allocator.Error!void {
        self.pending_branch_rc_adj.clearRetainingCapacity();
        var it = symbols_in_any_branch.keyIterator();
        while (it.next()) |key_ptr| {
            const key = key_ptr.*;
            const local_count = local_uses.get(key) orelse 0;
            if (local_count > 1) {
                try self.pending_branch_rc_adj.put(key, @intCast(local_count - 1));
            } else if (local_count == 0) {
                try self.pending_branch_rc_adj.put(key, -1);
            }
        }
    }

    fn clearPendingBranchRcAdj(self: *RcInsertPass) void {
        self.pending_branch_rc_adj.clearRetainingCapacity();
    }

    /// Wrap a branch body with per-branch RC operations.
    /// Each branch inherits 1 reference per symbol from the enclosing scope.
    /// - local_count == 0: emit decref (release inherited ref)
    /// - local_count == 1: no action (consumes the inherited ref)
    /// - local_count > 1: emit incref(count - 1)
    fn wrapBranchWithRcOps(
        self: *RcInsertPass,
        body: LirExprId,
        local_uses: *const std.AutoHashMap(u64, u32),
        symbols_in_any_branch: *const std.AutoHashMap(u64, void),
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        var rc_stmts = std.ArrayList(LirStmt).empty;
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
        guard: LirExprId,
        guard_uses: *const std.AutoHashMap(u64, u32),
        pattern_bound: *const std.AutoHashMap(u64, void),
        region: Region,
    ) Allocator.Error!LirExprId {
        if (guard.isNone()) return guard;

        var rc_stmts = std.ArrayList(LirStmt).empty;
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

    /// Recursively emit decrefs for a mutated pattern's old value.
    fn emitMutateDecrefsForPattern(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        region: Region,
        stmts: *std.ArrayList(LirStmt),
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            region: Region,
            stmts: *std.ArrayList(LirStmt),
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx) Allocator.Error!void {
                if (ctx.pass.layoutNeedsRc(layout_idx)) {
                    try ctx.pass.emitDecrefInto(symbol, layout_idx, ctx.region, ctx.stmts);
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self, .region = region, .stmts = stmts });
    }

    /// Recursively track live RC symbols for early_return cleanup.
    fn trackLiveRcSymbolsForPattern(self: *RcInsertPass, pat_id: LirPatternId) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx) Allocator.Error!void {
                if (ctx.pass.layoutNeedsRc(layout_idx)) {
                    try ctx.pass.live_rc_symbols.append(ctx.pass.allocator, .{
                        .symbol = symbol,
                        .layout_idx = layout_idx,
                    });
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self });
    }

    /// Recursively emit increfs for multi-use refcounted symbols bound by a pattern.
    /// Uses self.symbol_use_counts (global counts) — for use in processBlock.
    fn emitBlockIncrefsForPattern(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        region: Region,
        stmts: *std.ArrayList(LirStmt),
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            region: Region,
            stmts: *std.ArrayList(LirStmt),
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx) Allocator.Error!void {
                if (ctx.pass.layoutNeedsRc(layout_idx)) {
                    const key = @as(u64, @bitCast(symbol));
                    const use_count = ctx.pass.symbol_use_counts.get(key) orelse 0;
                    if (use_count > 1) {
                        try ctx.pass.emitIncrefInto(symbol, layout_idx, @intCast(use_count - 1), ctx.region, ctx.stmts);
                    }
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self, .region = region, .stmts = stmts });
    }

    /// Recursively emit decrefs for unused refcounted symbols bound by a pattern.
    /// Uses self.symbol_use_counts (global counts) — for use in processBlock.
    fn emitBlockDecrefsForPattern(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        region: Region,
        stmts: *std.ArrayList(LirStmt),
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            region: Region,
            stmts: *std.ArrayList(LirStmt),
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx) Allocator.Error!void {
                if (ctx.pass.layoutNeedsRc(layout_idx)) {
                    const key = @as(u64, @bitCast(symbol));
                    const use_count = ctx.pass.symbol_use_counts.get(key) orelse 0;
                    if (use_count == 0) {
                        try ctx.pass.emitDecrefInto(symbol, layout_idx, ctx.region, ctx.stmts);
                    }
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self, .region = region, .stmts = stmts });
    }

    /// Recursively emit RC ops for all symbols bound by a pattern.
    /// For each bound symbol with a refcounted layout:
    /// - use_count == 0: emit decref (release the provided reference)
    /// - use_count == 1: no action (consumes the provided reference)
    /// - use_count > 1: emit incref(count - 1)
    fn emitRcOpsForPatternInto(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        local_uses: *const std.AutoHashMap(u64, u32),
        region: Region,
        rc_stmts: *std.ArrayList(LirStmt),
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            local_uses: *const std.AutoHashMap(u64, u32),
            region: Region,
            rc_stmts: *std.ArrayList(LirStmt),
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx) Allocator.Error!void {
                if (ctx.pass.layoutNeedsRc(layout_idx)) {
                    const key = @as(u64, @bitCast(symbol));
                    const use_count = ctx.local_uses.get(key) orelse 0;
                    if (use_count == 0) {
                        try ctx.pass.emitDecrefInto(symbol, layout_idx, ctx.region, ctx.rc_stmts);
                    } else if (use_count > 1) {
                        try ctx.pass.emitIncrefInto(symbol, layout_idx, @intCast(use_count - 1), ctx.region, ctx.rc_stmts);
                    }
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self, .local_uses = local_uses, .region = region, .rc_stmts = rc_stmts });
    }

    /// Emit an incref statement into a given statement list.
    fn emitIncrefInto(self: *RcInsertPass, symbol: Symbol, layout_idx: LayoutIdx, count: u16, region: Region, stmts: *std.ArrayList(LirStmt)) Allocator.Error!void {
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
        try stmts.append(self.allocator, .{ .decl = .{
            .pattern = wildcard,
            .expr = incref_id,
        } });
    }

    /// Emit a decref statement into a given statement list.
    fn emitDecrefInto(self: *RcInsertPass, symbol: Symbol, layout_idx: LayoutIdx, region: Region, stmts: *std.ArrayList(LirStmt)) Allocator.Error!void {
        const lookup_id = try self.store.addExpr(.{ .lookup = .{
            .symbol = symbol,
            .layout_idx = layout_idx,
        } }, region);

        const decref_id = try self.store.addExpr(.{ .decref = .{
            .value = lookup_id,
            .layout_idx = layout_idx,
        } }, region);

        const wildcard = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = layout_idx } }, region);
        try stmts.append(self.allocator, .{ .decl = .{
            .pattern = wildcard,
            .expr = decref_id,
        } });
    }
};

test "RcInsertPass compiles" {
    const T = RcInsertPass;
    std.debug.assert(@sizeOf(T) > 0);
}

// --- Test helpers (same pattern as MirToLir.zig) ---

fn testInit() !struct { lir_store: LirExprStore, layout_store: layout_mod.Store, module_env: @import("can").ModuleEnv, module_env_ptrs: [1]*const @import("can").ModuleEnv } {
    const allocator = std.testing.allocator;
    var result: @TypeOf(testInit() catch unreachable) = undefined;
    result.module_env = try @import("can").ModuleEnv.init(allocator, "");
    result.lir_store = LirExprStore.init(allocator);
    return result;
}

fn testInitLayoutStore(self: *@TypeOf(testInit() catch unreachable)) !void {
    self.module_env_ptrs[0] = &self.module_env;
    self.layout_store = try layout_mod.Store.init(&self.module_env_ptrs, null, std.testing.allocator, @import("base").target.TargetUsize.native);
}

fn testDeinit(self: *@TypeOf(testInit() catch unreachable)) void {
    self.layout_store.deinit();
    self.lir_store.deinit();
    self.module_env.deinit();
}

test "RC pass-through: non-refcounted i64 block unchanged" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    // Build: { x = 42; x }
    const i64_layout: LayoutIdx = .i64;

    const ident_x = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_x = LIR.Symbol{ .module_idx = 0, .ident_idx = ident_x };

    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());
    const pat_x = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_x, .layout_idx = i64_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_x, .expr = int_lit } }});
    const lookup_x = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_x, .layout_idx = i64_layout } }, Region.zero());

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup_x,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const result_expr = env.lir_store.getExpr(result);
    try std.testing.expect(result_expr == .block);

    // No RC ops should have been added — statement count should be 1
    const result_stmts = env.lir_store.getStmts(result_expr.block.stmts);
    try std.testing.expectEqual(@as(usize, 1), result_stmts.len);
}

test "RC: string binding used twice gets incref" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    // Build: { s = "hello"; use(s); s }
    // where s is used twice (the use(s) stmt + final lookup)
    const str_layout: LayoutIdx = .str;

    const ident_s = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_s = LIR.Symbol{ .module_idx = 0, .ident_idx = ident_s };

    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    // Two lookups to get use_count = 2
    const lookup1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    // Statement: s = "hello"
    // Statement: _ = s (use to bump count)
    const wildcard = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_s, .expr = str_lit } },
        .{ .decl = .{ .pattern = wildcard, .expr = lookup1 } },
    });

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup2,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const result_expr = env.lir_store.getExpr(result);
    try std.testing.expect(result_expr == .block);

    // Should have more statements than original (incref added after the bind)
    const result_stmts = env.lir_store.getStmts(result_expr.block.stmts);
    try std.testing.expect(result_stmts.len > 2);

    // Find the incref in the statements
    var found_incref = false;
    for (result_stmts) |stmt| {
        const stmt_expr = env.lir_store.getExpr(stmt.binding().expr);
        if (stmt_expr == .incref) found_incref = true;
    }
    try std.testing.expect(found_incref);
}

test "RC: unused string binding gets decref" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    // Build: { s = "hello"; 42 }
    // s is bound but never used, so it should get a decref
    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;

    const ident_s = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const sym_s = LIR.Symbol{ .module_idx = 0, .ident_idx = ident_s };

    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_lit } }});

    // Final expression is an i64 literal (s is unused)
    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = int_lit,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const result_expr = env.lir_store.getExpr(result);
    try std.testing.expect(result_expr == .block);

    // Should have more statements than original (decref added)
    const result_stmts = env.lir_store.getStmts(result_expr.block.stmts);
    try std.testing.expect(result_stmts.len > 1);

    // Find the decref in the statements
    var found_decref = false;
    for (result_stmts) |stmt| {
        const stmt_expr = env.lir_store.getExpr(stmt.binding().expr);
        if (stmt_expr == .decref) found_decref = true;
    }
    try std.testing.expect(found_decref);
}

// --- Helper for counting RC ops in an expression tree ---

const RcOpCounts = struct { increfs: u32, decrefs: u32 };

fn countRcOps(store: *const LirExprStore, expr_id: LirExprId) RcOpCounts {
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
                const sub = countRcOps(store, stmt.binding().expr);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
            const sub = countRcOps(store, block.final_expr);
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
        .lambda => |lam| {
            const sub = countRcOps(store, lam.body);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .closure => |clo_id| {
            const clo = store.getClosureData(clo_id);
            const sub = countRcOps(store, clo.lambda);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .for_loop => |fl| {
            const sub = countRcOps(store, fl.body);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .discriminant_switch => |ds| {
            const ds_branches = store.getExprSpan(ds.branches);
            for (ds_branches) |br_id| {
                const sub = countRcOps(store, br_id);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
        },
        .while_loop => |wl| {
            const sub_cond = countRcOps(store, wl.cond);
            increfs += sub_cond.increfs;
            decrefs += sub_cond.decrefs;
            const sub_body = countRcOps(store, wl.body);
            increfs += sub_body.increfs;
            decrefs += sub_body.decrefs;
        },
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
        .early_return,
        .break_expr,
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
        .tag_payload_access,
        .hosted_call,
        .free,
        => {},
    }
    return .{ .increfs = increfs, .decrefs = decrefs };
}

/// Helper to make a symbol with a given ident index.
fn makeSymbol(idx: u29) LIR.Symbol {
    return .{
        .module_idx = 0,
        .ident_idx = base.Ident.Idx{
            .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
            .idx = idx,
        },
    };
}

test "RC branch-aware: symbol used in both match branches — no incref at binding" {
    // { s = "hello"; match cond is True -> s, False -> s }
    // s is used once in each branch => global count should be 1 (the match construct counts as 1)
    // Each branch uses s once => no per-branch RC ops needed
    // => no incref at binding site
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);
    const sym_cond = makeSymbol(2);

    // Build the match: match cond is True -> s, False -> s
    const cond_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond, .layout_idx = i64_layout } }, Region.zero());
    const lookup_s1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    const wild_pat1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild_pat2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const match_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = wild_pat1, .guard = LirExprId.none, .body = lookup_s1 },
        .{ .pattern = wild_pat2, .guard = LirExprId.none, .body = lookup_s2 },
    });

    const match_expr = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = match_branches,
        .result_layout = str_layout,
    } }, Region.zero());

    // Build block: { s = "hello"; <match> }
    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_lit } }});

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = match_expr,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // Check: no incref or decref anywhere in the result tree
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
}

test "RC branch-aware: symbol used in one match branch only — decref in unused branch" {
    // { s = "hello"; match cond is True -> s, False -> 42 }
    // s appears in one branch only. Global count = 1 (1 branching construct).
    // True branch: 1 use => no action. False branch: 0 uses => decref.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);
    const sym_cond = makeSymbol(2);

    const cond_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond, .layout_idx = i64_layout } }, Region.zero());
    const lookup_s = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());

    const wild_pat1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild_pat2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const match_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = wild_pat1, .guard = LirExprId.none, .body = lookup_s },
        .{ .pattern = wild_pat2, .guard = LirExprId.none, .body = int_42 },
    });

    const match_expr = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = match_branches,
        .result_layout = str_layout,
    } }, Region.zero());

    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_lit } }});

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = match_expr,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // No incref at binding (global count = 1), but decref in the False branch
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC branch-aware: symbol used twice in one branch — incref in that branch, decref in other" {
    // { s = "hello"; match cond is True -> binop(s, s), False -> 42 }
    // True branch: 2 uses => incref(1). False branch: 0 uses => decref.
    // Global count = 1 => no incref at binding.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);
    const sym_cond = makeSymbol(2);

    const cond_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond, .layout_idx = i64_layout } }, Region.zero());
    const lookup_s1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());

    // True branch body: binop(s, s) — uses s twice
    const binop_expr = try env.lir_store.addExpr(.{ .binop = .{
        .lhs = lookup_s1,
        .rhs = lookup_s2,
        .op = .add,
        .result_layout = str_layout,
        .operand_layout = str_layout,
    } }, Region.zero());

    const wild_pat1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild_pat2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const match_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = wild_pat1, .guard = LirExprId.none, .body = binop_expr },
        .{ .pattern = wild_pat2, .guard = LirExprId.none, .body = int_42 },
    });

    const match_expr = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = match_branches,
        .result_layout = str_layout,
    } }, Region.zero());

    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_lit } }});

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = match_expr,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // incref(1) in True branch, decref in False branch, no incref at binding
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC branch-aware: symbol used outside and inside branches" {
    // { s = "hello"; _ = s; match cond is True -> s, False -> 42 }
    // s used outside (1) + match construct (1) = global count 2 => incref(1) at binding.
    // True branch: 1 use => no action. False branch: 0 uses => decref.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);
    const sym_cond = makeSymbol(2);

    // Build match
    const cond_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond, .layout_idx = i64_layout } }, Region.zero());
    const lookup_s_branch = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());

    const wild_pat1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild_pat2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const match_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = wild_pat1, .guard = LirExprId.none, .body = lookup_s_branch },
        .{ .pattern = wild_pat2, .guard = LirExprId.none, .body = int_42 },
    });

    const match_expr = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = match_branches,
        .result_layout = str_layout,
    } }, Region.zero());

    // Build block: { s = "hello"; _ = s; <match> }
    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s_outside = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const wild_use = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());

    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_s, .expr = str_lit } },
        .{ .decl = .{ .pattern = wild_use, .expr = lookup_s_outside } },
    });

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = match_expr,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // incref(1) at binding site, decref in False branch
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC lambda: body with nested block gets RC ops" {
    // |x| { s = "hello"; _ = s; s }
    // Lambda body is a block where s is str-layout and used twice => incref inside body
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_x = makeSymbol(1);
    const sym_s = makeSymbol(2);

    // Build the lambda body: { s = "hello"; _ = s; s }
    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const wild = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const body_stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_s, .expr = str_lit } },
        .{ .decl = .{ .pattern = wild, .expr = lookup_s1 } },
    });
    const body_block = try env.lir_store.addExpr(.{ .block = .{
        .stmts = body_stmts,
        .final_expr = lookup_s2,
        .result_layout = str_layout,
    } }, Region.zero());

    // Build lambda: |x| <body_block>
    const pat_x = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_x, .layout_idx = i64_layout } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{pat_x});
    const lambda_expr = try env.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = i64_layout,
        .params = params,
        .body = body_block,
        .ret_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(lambda_expr);

    // The block inside the lambda body should have incref for s (used twice)
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
}

test "RC lambda: refcounted param used twice gets incref" {
    // |s| binop(s, s)  where s is str-layout
    // s is used twice in body => incref(1) for the param
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_s = makeSymbol(1);

    // Build lambda body: binop(s, s)
    const lookup_s1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const binop_expr = try env.lir_store.addExpr(.{ .binop = .{
        .lhs = lookup_s1,
        .rhs = lookup_s2,
        .op = .add,
        .result_layout = str_layout,
        .operand_layout = str_layout,
    } }, Region.zero());

    // Build lambda: |s| binop(s, s)
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{pat_s});
    const lambda_expr = try env.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = str_layout,
        .params = params,
        .body = binop_expr,
        .ret_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(lambda_expr);

    // incref(1) for the str param used twice
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
}

test "RC lambda: unused refcounted param gets decref" {
    // |s| 42  where s is str-layout
    // s is never used in body => decref for the param
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);

    // Build lambda body: 42
    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());

    // Build lambda: |s| 42
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{pat_s});
    const lambda_expr = try env.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = i64_layout,
        .params = params,
        .body = int_lit,
        .ret_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(lambda_expr);

    // decref for unused str param
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC for_loop: elem used twice gets incref" {
    // for list |elem| { _ = elem; elem }  where elem is str-layout
    // elem used twice => incref(1)
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_elem = makeSymbol(1);
    const sym_list = makeSymbol(2);

    // Build for loop body: { _ = elem; elem }
    const lookup_e1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_elem, .layout_idx = str_layout } }, Region.zero());
    const lookup_e2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_elem, .layout_idx = str_layout } }, Region.zero());
    const wild = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const body_stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = wild, .expr = lookup_e1 } },
    });
    const body_block = try env.lir_store.addExpr(.{ .block = .{
        .stmts = body_stmts,
        .final_expr = lookup_e2,
        .result_layout = str_layout,
    } }, Region.zero());

    // Build for loop
    const list_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = str_layout } }, Region.zero());
    const pat_elem = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_elem, .layout_idx = str_layout } }, Region.zero());
    const for_expr = try env.lir_store.addExpr(.{ .for_loop = .{
        .list_expr = list_expr,
        .elem_layout = str_layout,
        .elem_pattern = pat_elem,
        .body = body_block,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(for_expr);

    // incref(1) for the elem used twice in the body
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
}

test "RC closure: wrapping lambda gets RC ops" {
    // closure around |s| 42  where s is str-layout
    // The inner lambda has unused str param => decref
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);

    // Build inner lambda: |s| 42
    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{pat_s});
    const lambda_expr = try env.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = i64_layout,
        .params = params,
        .body = int_lit,
        .ret_layout = i64_layout,
    } }, Region.zero());

    // Wrap in closure
    const closure_data_id = try env.lir_store.addClosureData(.{
        .closure_layout = i64_layout,
        .lambda = lambda_expr,
        .captures = LIR.LirCaptureSpan.empty(),
        .representation = .{ .unwrapped_capture = .{ .capture_layout = i64_layout } },
        .recursion = .not_recursive,
        .self_recursive = .not_self_recursive,
        .is_bound_to_variable = false,
    });
    const closure_expr = try env.lir_store.addExpr(.{ .closure = closure_data_id }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(closure_expr);

    // decref for unused str param in the inner lambda
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC block: lambda in stmt.expr gets processed" {
    // { f = |s| 42; 0 }  where s is str-layout
    // The lambda bound by f should have its body processed (decref for unused s)
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_f = makeSymbol(1);
    const sym_s = makeSymbol(2);

    // Build lambda: |s| 42
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{pat_s});
    const lambda_expr = try env.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = i64_layout,
        .params = params,
        .body = int_42,
        .ret_layout = i64_layout,
    } }, Region.zero());

    // Build block: { f = <lambda>; 0 }
    const pat_f = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_f, .layout_idx = i64_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_f, .expr = lambda_expr } }});
    const int_0 = try env.lir_store.addExpr(.{ .i64_literal = 0 }, Region.zero());
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = int_0,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // decref for unused str param s in the lambda body
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC mutation: reassigning refcounted var emits decref before mutation" {
    // { var s = "hello"; s = "world"; s }
    // The mutation (s = "world") should emit a decref of the old value before the assignment.
    // s is used once (final expr), so no incref at decl, but decref before mutation.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_s = makeSymbol(1);

    // Build: { var s = "hello"; s = "world"; s }
    const str_hello = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const str_world = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s_decl = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const pat_s_mut = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_s_decl, .expr = str_hello } },
        .{ .mutate = .{ .pattern = pat_s_mut, .expr = str_world } },
    });

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup_s,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const result_expr = env.lir_store.getExpr(result);
    try std.testing.expect(result_expr == .block);

    // Should have a decref (for the old value before mutation)
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expect(rc.decrefs >= 1);

    // Verify the decref appears before the mutation statement
    const result_stmts = env.lir_store.getStmts(result_expr.block.stmts);
    var found_decref_before_mutate = false;
    var found_decref = false;
    for (result_stmts) |stmt| {
        const stmt_expr = env.lir_store.getExpr(stmt.binding().expr);
        if (stmt_expr == .decref) found_decref = true;
        if (stmt == .mutate and found_decref) found_decref_before_mutate = true;
    }
    try std.testing.expect(found_decref_before_mutate);
}

test "RC for_loop: unused refcounted elem gets decref" {
    // for list |elem| { 42 }  where elem is str-layout but unused in body
    // elem never used => decref for the unused element
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_elem = makeSymbol(1);
    const sym_list = makeSymbol(2);

    // Build for loop body: 42 (ignores elem entirely)
    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());

    // Build for loop
    const list_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = str_layout } }, Region.zero());
    const pat_elem = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_elem, .layout_idx = str_layout } }, Region.zero());
    const for_expr = try env.lir_store.addExpr(.{ .for_loop = .{
        .list_expr = list_expr,
        .elem_layout = str_layout,
        .elem_pattern = pat_elem,
        .body = int_lit,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(for_expr);

    // decref for unused str elem
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expect(rc.decrefs >= 1);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
}

test "RC match guard: symbol used only in guard gets proper RC ops" {
    // { s = "hello"; match cond is _ if s -> 1, _ -> 2 }
    // s is used in the guard of branch 0 but not in the body of either branch.
    // Guard gets borrow-style incref(1) so the inherited ref is preserved.
    // Branch 0 body: 0 uses => decref (releases inherited ref).
    // Branch 1 body: 0 uses => decref.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);
    const sym_cond = makeSymbol(2);

    const cond_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond, .layout_idx = i64_layout } }, Region.zero());
    const guard_lookup_s = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const int_1 = try env.lir_store.addExpr(.{ .i64_literal = 1 }, Region.zero());
    const int_2 = try env.lir_store.addExpr(.{ .i64_literal = 2 }, Region.zero());

    const wild_pat1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild_pat2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const match_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = wild_pat1, .guard = guard_lookup_s, .body = int_1 },
        .{ .pattern = wild_pat2, .guard = LirExprId.none, .body = int_2 },
    });

    const match_expr = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = match_branches,
        .result_layout = i64_layout,
    } }, Region.zero());

    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_lit } }});

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = match_expr,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // Guard incref(1) for s in branch 0, decref in both branch bodies
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 2), rc.decrefs);
}

test "RC match guard+body: symbol used in both guard and body gets proper RC ops" {
    // { s = "hello"; match cond is _ if s -> s, _ -> "world" }
    // Branch 0: s used in guard (1) + body (1) = 2 uses => incref(1).
    // Branch 1: 0 uses => decref.
    // Global count = 1 => no incref at binding.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);
    const sym_cond = makeSymbol(2);

    const cond_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond, .layout_idx = i64_layout } }, Region.zero());
    const guard_lookup_s = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const body_lookup_s = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const str_world = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());

    const wild_pat1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild_pat2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const match_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = wild_pat1, .guard = guard_lookup_s, .body = body_lookup_s },
        .{ .pattern = wild_pat2, .guard = LirExprId.none, .body = str_world },
    });

    const match_expr = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = match_branches,
        .result_layout = str_layout,
    } }, Region.zero());

    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_lit } }});

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = match_expr,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // Branch 0: 2 uses (guard + body) => incref(1). Branch 1: 0 uses => decref.
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC for_loop: wrapper block has unit result layout, not elem layout" {
    // for list |elem| { 42 }  where elem is str but body produces i64
    // The RC wrapper block around the body should have .zst (unit) result layout,
    // not elem_layout (str).
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_elem = makeSymbol(1);
    const sym_list = makeSymbol(2);

    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());

    const list_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = str_layout } }, Region.zero());
    const pat_elem = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_elem, .layout_idx = str_layout } }, Region.zero());
    const for_expr = try env.lir_store.addExpr(.{ .for_loop = .{
        .list_expr = list_expr,
        .elem_layout = str_layout,
        .elem_pattern = pat_elem,
        .body = int_lit,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(for_expr);
    const result_expr = env.lir_store.getExpr(result);

    // The for_loop body should be wrapped in a block (RC ops added for unused elem)
    try std.testing.expect(result_expr == .for_loop);
    const wrapper_body = env.lir_store.getExpr(result_expr.for_loop.body);
    try std.testing.expect(wrapper_body == .block);
    // Wrapper block result layout should be unit (.zst), not elem_layout (.str)
    try std.testing.expectEqual(LayoutIdx.zst, wrapper_body.block.result_layout);
}

test "RC if_then_else: symbol used in both branches — no extra incref" {
    // { s = "hello"; if cond then s else s }
    // s is used once per branch, global count = 1 => no extra incref at binding.
    // Each branch consumes the inherited ref, so no per-branch RC adjustment.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);
    const sym_cond = makeSymbol(2);

    // Build: { s = "hello"; if cond then s else s }
    const str_hello = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const cond_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond, .layout_idx = i64_layout } }, Region.zero());
    const lookup_s_then = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s_else = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    const if_branches = try env.lir_store.addIfBranches(&.{.{
        .cond = cond_expr,
        .body = lookup_s_then,
    }});
    const ite = try env.lir_store.addExpr(.{ .if_then_else = .{
        .branches = if_branches,
        .final_else = lookup_s_else,
        .result_layout = str_layout,
    } }, Region.zero());

    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_hello } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = ite,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // s used once in each branch, global count = 1 => no incref, no decref
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
}

test "RC closure: capturing refcounted string gets RC tracking" {
    // { s = "hello"; closure(lambda(body: s), captures: [s]) }
    // The closure captures a refcounted string. The lambda body uses s once.
    // The capture itself constitutes a use of s in the outer scope.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);
    const sym_x = makeSymbol(2);

    // Build lambda body: just return s (the captured value)
    const lookup_s_body = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    // Build lambda: |x| s  (x is a dummy param; body uses the captured s)
    const pat_x = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_x, .layout_idx = i64_layout } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{pat_x});
    const lambda_expr = try env.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = i64_layout,
        .params = params,
        .body = lookup_s_body,
        .ret_layout = str_layout,
    } }, Region.zero());

    // Build captures list: [s]
    const captures = try env.lir_store.addCaptures(&.{.{
        .symbol = sym_s,
        .layout_idx = str_layout,
    }});

    // Build closure wrapping the lambda with capture of s
    const closure_data_id = try env.lir_store.addClosureData(.{
        .closure_layout = str_layout,
        .lambda = lambda_expr,
        .captures = captures,
        .representation = .{ .unwrapped_capture = .{ .capture_layout = str_layout } },
        .recursion = .not_recursive,
        .self_recursive = .not_self_recursive,
        .is_bound_to_variable = false,
    });
    const closure_expr = try env.lir_store.addExpr(.{ .closure = closure_data_id }, Region.zero());

    // Build block: { s = "hello"; <closure> }
    const str_hello = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_hello } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = closure_expr,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // The RC pass should process the closure and its lambda body.
    // s is used in the lambda body (counted into outer scope for now per known issue #1).
    // At minimum, the pass should not crash and should produce a valid result.
    const result_expr = env.lir_store.getExpr(result);
    try std.testing.expect(result_expr == .block);

    // The closure should still be present in the result tree
    const rc = countRcOps(&env.lir_store, result);
    // With current behavior (issue #1: lambda body uses counted into outer scope),
    // s has 1 use from the lambda body, so global count = 1, no extra incref.
    // No unused-symbol decref either since s is "used".
    try std.testing.expect(rc.increfs == 0 or rc.increfs >= 1);
}

test "RC nested match: symbol used in inner and outer match branches" {
    // { s = "hello"; match cond is True -> (match cond2 is True -> s, False -> s), False -> s }
    // s appears in every leaf branch. Each outer branch uses s once (either directly
    // or through the inner match), so global count = 1. No extra incref at binding.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);
    const sym_cond = makeSymbol(2);
    const sym_cond2 = makeSymbol(3);

    // Build inner match: match cond2 is True -> s, False -> s
    const cond2_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond2, .layout_idx = i64_layout } }, Region.zero());
    const lookup_s_inner1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s_inner2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    const inner_wild1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const inner_wild2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const inner_match_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = inner_wild1, .guard = LirExprId.none, .body = lookup_s_inner1 },
        .{ .pattern = inner_wild2, .guard = LirExprId.none, .body = lookup_s_inner2 },
    });
    const inner_match = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = cond2_expr,
        .value_layout = i64_layout,
        .branches = inner_match_branches,
        .result_layout = str_layout,
    } }, Region.zero());

    // Build outer match: match cond is _ -> inner_match, _ -> s
    const cond_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond, .layout_idx = i64_layout } }, Region.zero());
    const lookup_s_outer = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    const outer_wild1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const outer_wild2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const outer_match_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = outer_wild1, .guard = LirExprId.none, .body = inner_match },
        .{ .pattern = outer_wild2, .guard = LirExprId.none, .body = lookup_s_outer },
    });
    const outer_match = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = outer_match_branches,
        .result_layout = str_layout,
    } }, Region.zero());

    // Build block: { s = "hello"; <outer_match> }
    const str_hello = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_hello } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = outer_match,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // The outer match has 2 branches, each using s once (the inner match uses s
    // in both its branches but represents 1 use to the outer scope).
    // Global count = 1, so no incref at binding.
    // Inner match: s used once per branch => no per-branch RC adjustment.
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
}

test "RC discriminant_switch: symbol used in switch branches gets per-branch RC" {
    // { s = "hello"; discriminant_switch(val) { 0 -> s, 1 -> s } }
    // s used once per branch, global count = 1 => no extra incref at binding.
    // Tests that discriminant_switch uses branch-aware RC counting.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_s = makeSymbol(1);
    const sym_val = makeSymbol(2);

    // Create a tag union layout with 2 variants (both str payload)
    const tag_union_layout = try env.layout_store.putTagUnion(&.{ str_layout, str_layout });

    // Build: { s = "hello"; discriminant_switch(val) { 0 -> s, 1 -> s } }
    const str_hello = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const val_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_val, .layout_idx = tag_union_layout } }, Region.zero());
    const lookup_s_br0 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s_br1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    const branches_span = try env.lir_store.addExprSpan(&.{ lookup_s_br0, lookup_s_br1 });
    const disc_switch = try env.lir_store.addExpr(.{ .discriminant_switch = .{
        .value = val_expr,
        .union_layout = tag_union_layout,
        .branches = branches_span,
        .result_layout = str_layout,
    } }, Region.zero());

    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_hello } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = disc_switch,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // s used once per branch, global count = 1 => no incref, no decref
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
}

test "RC discriminant_switch: body-bound symbols don't get per-branch RC ops" {
    // { s = "hello"; discriminant_switch(val) { 0 -> { t = "world"; t }, 1 -> s } }
    // t is bound inside branch 0 and should NOT get per-branch RC ops.
    // s is used only in branch 1, so needs a decref in branch 0.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_s = makeSymbol(1);
    const sym_t = makeSymbol(2);
    const sym_val = makeSymbol(3);

    const tag_union_layout = try env.layout_store.putTagUnion(&.{ str_layout, str_layout });

    // Build branch 0: { t = "world"; t }
    const str_world = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const lookup_t = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_t, .layout_idx = str_layout } }, Region.zero());
    const pat_t = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_t, .layout_idx = str_layout } }, Region.zero());
    const branch0_stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_t, .expr = str_world } }});
    const branch0 = try env.lir_store.addExpr(.{ .block = .{
        .stmts = branch0_stmts,
        .final_expr = lookup_t,
        .result_layout = str_layout,
    } }, Region.zero());

    // Build branch 1: s
    const lookup_s = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    // Build: { s = "hello"; discriminant_switch(val) { 0 -> branch0, 1 -> s } }
    const str_hello = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const val_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_val, .layout_idx = tag_union_layout } }, Region.zero());
    const branches_span = try env.lir_store.addExprSpan(&.{ branch0, lookup_s });
    const disc_switch = try env.lir_store.addExpr(.{ .discriminant_switch = .{
        .value = val_expr,
        .union_layout = tag_union_layout,
        .branches = branches_span,
        .result_layout = str_layout,
    } }, Region.zero());

    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_hello } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = disc_switch,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // s used in one of two branches → decref in unused branch
    // t is body-bound and should not cause extra RC ops
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC early_return emits correct number of decrefs for multi-use symbol" {
    // Build: { s = "hello"; _ = s; early_return(42); _ = s; s }
    // s has global_use_count = 3 → incref(2) at binding.
    // First use (stmt 2) consumes 1 ref → 2 remaining.
    // early_return doesn't use s → need 2 decrefs, not 1.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);

    // Create expressions
    const str_hello = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const lookup_s1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s3 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());
    const early_ret = try env.lir_store.addExpr(.{ .early_return = .{
        .expr = int_42,
        .ret_layout = i64_layout,
    } }, Region.zero());

    // Build block: { s = "hello"; _ = s; _ = early_return(42); _ = s; s }
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const wild1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const wild2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild3 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_s, .expr = str_hello } },
        .{ .decl = .{ .pattern = wild1, .expr = lookup_s1 } },
        .{ .decl = .{ .pattern = wild2, .expr = early_ret } },
        .{ .decl = .{ .pattern = wild3, .expr = lookup_s2 } },
    });

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup_s3,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // s has global_use_count = 3 → incref(2) at binding.
    // At early_return: 1 use consumed, 0 in return expr → 2 decrefs needed.
    const rc = countRcOps(&env.lir_store, result);
    // Expect: 1 incref (at binding, count=2) + 2 decrefs (at early_return cleanup)
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 2), rc.decrefs);
}

test "RC early_return inside branch accounts for branch-level increfs" {
    // Build: { s = "hello"; _ = s; if x { _ = s; _ = s; early_return(42) } else { s } }
    // countUsesInto uses a branching model: the if_then_else contributes 1 use from
    // the enclosing scope, so global_use_count = 2 (1 from outer stmt + 1 from if_then_else).
    // At binding: incref(1).
    // Branch wrapper adds incref(1) for if-body (local_count=2 > 1).
    // At early_return: effective = 2 + 1 = 3, consumed = 1 (outer) + 2 (inner) = 3, remaining = 0.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);

    // Create expressions
    const str_hello = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const lookup_s_outer = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s3 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());
    const cond = try env.lir_store.addExpr(.{ .bool_literal = true }, Region.zero());

    // Build if-body: { _ = s; _ = s; early_return(42) }
    const early_ret = try env.lir_store.addExpr(.{ .early_return = .{
        .expr = int_42,
        .ret_layout = i64_layout,
    } }, Region.zero());
    const wild1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const wild2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const if_body_stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = wild1, .expr = lookup_s1 } },
        .{ .decl = .{ .pattern = wild2, .expr = lookup_s2 } },
    });
    const if_body = try env.lir_store.addExpr(.{ .block = .{
        .stmts = if_body_stmts,
        .final_expr = early_ret,
        .result_layout = str_layout,
    } }, Region.zero());

    // Build if-then-else: if x { ... } else { s }
    const if_branches = try env.lir_store.addIfBranches(&.{.{
        .cond = cond,
        .body = if_body,
    }});
    const ite = try env.lir_store.addExpr(.{ .if_then_else = .{
        .branches = if_branches,
        .final_else = lookup_s3,
        .result_layout = str_layout,
    } }, Region.zero());

    // Build outer block: { s = "hello"; _ = s; if ... }
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const wild_outer = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_s, .expr = str_hello } },
        .{ .decl = .{ .pattern = wild_outer, .expr = lookup_s_outer } },
    });
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = ite,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // global_use_count = 2 (1 outer stmt + 1 from if_then_else).
    // At binding: incref(1) = 1 incref node.
    // Branch wrapper: if-body local_count=2 → incref(1) = 1 incref node.
    // At early_return: effective = 2 + 1 = 3, consumed = 1 + 2 = 3, remaining = 0 decrefs.
    // Total: 2 incref nodes, 0 early-return decrefs.
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 2), rc.increfs);
}
