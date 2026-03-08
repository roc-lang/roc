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
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const layout_mod = @import("layout");
const base = @import("base");
const LIR = @import("LIR.zig");
const LirExprStore = @import("LirExprStore.zig");
const LowLevelMap = @import("LowLevelMap.zig");

const LirExprId = LIR.LirExprId;
const Symbol = LIR.Symbol;
const LirStmt = LIR.LirStmt;
const LirStmtSpan = LIR.LirStmtSpan;
const LirPatternId = LIR.LirPatternId;
const LirIfBranch = LIR.LirIfBranch;
const LirMatchBranch = LIR.LirMatchBranch;
const LirExprSpan = LIR.LirExprSpan;
const LayoutIdx = layout_mod.Idx;
const Region = base.Region;

/// Inserts reference counting operations (incref/decref) into the mono IR.
pub const RcInsertPass = struct {
    allocator: Allocator,
    store: *LirExprStore,
    layout_store: *const layout_mod.Store,

    /// Tracks how many times each symbol is referenced in the expression tree.
    /// Keyed by the raw u64 representation of Symbol.
    symbol_use_counts: std.AutoHashMap(u64, u32),

    /// Tracks how many times each symbol's owned reference is actually consumed.
    /// Borrowed positions do not contribute here.
    symbol_consumed_counts: std.AutoHashMap(u64, u32),

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

    /// Reusable scratch map for counting consumed symbol uses within sub-expressions.
    scratch_consumed_uses: std.AutoHashMap(u64, u32),

    /// Reusable scratch buffer for collecting HashMap keys before sorting.
    /// Used by wrapBranchWithRcOps and wrapGuardWithIncref to ensure
    /// deterministic RC op ordering regardless of HashMap iteration order.
    scratch_keys: base.Scratch(u64),

    /// Synthetic symbol source for RC-inserted result temporaries.
    next_synthetic_symbol: u64,

    const LiveRcSymbol = struct {
        symbol: Symbol,
        layout_idx: LayoutIdx,
        reassignable: bool,
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
            .symbol_consumed_counts = std.AutoHashMap(u64, u32).init(allocator),
            .symbol_layouts = std.AutoHashMap(u64, LayoutIdx).init(allocator),
            .live_rc_symbols = std.ArrayList(LiveRcSymbol).empty,
            .early_return_scope_base = 0,
            .block_consumed_uses = std.AutoHashMap(u64, u32).init(allocator),
            .cumulative_consumed_uses = std.AutoHashMap(u64, u32).init(allocator),
            .pending_branch_rc_adj = std.AutoHashMap(u64, i32).init(allocator),
            .scratch_uses = std.AutoHashMap(u64, u32).init(allocator),
            .scratch_consumed_uses = std.AutoHashMap(u64, u32).init(allocator),
            .scratch_keys = try base.Scratch(u64).init(allocator),
            .next_synthetic_symbol = 0xf000_0000_0000_0000,
        };
    }

    pub fn deinit(self: *RcInsertPass) void {
        self.symbol_use_counts.deinit();
        self.symbol_consumed_counts.deinit();
        self.symbol_layouts.deinit();
        self.live_rc_symbols.deinit(self.allocator);
        self.block_consumed_uses.deinit();
        self.cumulative_consumed_uses.deinit();
        self.pending_branch_rc_adj.deinit();
        self.scratch_uses.deinit();
        self.scratch_consumed_uses.deinit();
        self.scratch_keys.deinit();
    }

    /// Main entry point: insert RC operations into a LirExpr tree.
    /// Returns a new LirExprId for the RC-annotated tree.
    pub fn insertRcOps(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!LirExprId {
        // Clear all accumulated state so insertRcOps is safe to call multiple times.
        self.symbol_use_counts.clearRetainingCapacity();
        self.symbol_consumed_counts.clearRetainingCapacity();
        self.symbol_layouts.clearRetainingCapacity();
        self.live_rc_symbols.clearRetainingCapacity();
        self.block_consumed_uses.clearRetainingCapacity();
        self.cumulative_consumed_uses.clearRetainingCapacity();
        self.pending_branch_rc_adj.clearRetainingCapacity();
        self.scratch_uses.clearRetainingCapacity();
        self.scratch_consumed_uses.clearRetainingCapacity();
        self.next_synthetic_symbol = 0xf000_0000_0000_0000;

        // Phase 1: Count symbol references and symbol ownership consumption.
        try self.countUses(expr_id);
        try self.countConsumedUses(expr_id);

        // Phase 2: Walk the tree and insert RC operations
        return self.processExpr(expr_id);
    }

    /// Count how many times each symbol is referenced in the expression tree.
    /// Also records the layout for each symbol found in bind patterns.
    /// Wrapper around countUsesInto that targets self.symbol_use_counts.
    fn countUses(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!void {
        try self.countUsesInto(expr_id, &self.symbol_use_counts);
    }

    fn countConsumedUses(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!void {
        try self.countConsumedValueInto(expr_id, &self.symbol_consumed_counts);
    }

    fn bumpUseCount(target: *std.AutoHashMap(u64, u32), key: u64) Allocator.Error!void {
        const gop = try target.getOrPut(key);
        if (gop.found_existing) {
            gop.value_ptr.* += 1;
        } else {
            gop.value_ptr.* = 1;
        }
    }

    fn freshSyntheticSymbol(self: *RcInsertPass) Symbol {
        const symbol = Symbol.fromRaw(self.next_synthetic_symbol);
        self.next_synthetic_symbol += 1;
        return symbol;
    }

    fn freshResultPattern(self: *RcInsertPass, layout_idx: LayoutIdx, region: Region) Allocator.Error!struct { symbol: Symbol, pattern: LirPatternId } {
        const symbol = self.freshSyntheticSymbol();
        const pattern = try self.store.addPattern(.{ .bind = .{
            .symbol = symbol,
            .layout_idx = layout_idx,
            .reassignable = false,
        } }, region);
        return .{ .symbol = symbol, .pattern = pattern };
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
                    try bumpUseCount(target, key);
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
            .borrow_scope => |scope| {
                const bindings = self.store.getBorrowBindings(scope.bindings);
                for (bindings) |binding| {
                    try self.registerPatternSymbolInto(binding.pattern, target);
                }
                for (bindings) |binding| {
                    try self.countUsesInto(binding.expr, target);
                }
                try self.countUsesInto(scope.body, target);
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
            .list => |list| {
                const elems = self.store.getExprSpan(list.elems);
                for (elems) |elem_id| {
                    try self.countUsesInto(elem_id, target);
                }
            },
            .struct_ => |s| {
                const fields = self.store.getExprSpan(s.fields);
                for (fields) |field_id| {
                    try self.countUsesInto(field_id, target);
                }
            },
            .tag => |t| {
                const args = self.store.getExprSpan(t.args);
                for (args) |arg_id| {
                    try self.countUsesInto(arg_id, target);
                }
            },
            .struct_access => |sa| {
                try self.countUsesInto(sa.struct_expr, target);
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
            .semantic_low_level => |ll| {
                const args = self.store.getExprSpan(ll.args);
                for (args) |arg_id| {
                    try self.countUsesInto(arg_id, target);
                }
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
            .zero_arg_tag,
            .crash,
            .runtime_error,
            .break_expr,
            => {},
        }
    }

    /// Count symbol consumption within an expression whose *result* is borrowed.
    /// This records ownership transfers that happen internally, but a bare lookup
    /// does not consume the current scope's owned reference.
    fn countConsumedUsesInto(
        self: *RcInsertPass,
        expr_id: LirExprId,
        target: *std.AutoHashMap(u64, u32),
    ) Allocator.Error!void {
        if (expr_id.isNone()) return;

        const expr = self.store.getExpr(expr_id);
        switch (expr) {
            .block => |block| {
                const stmts = self.store.getStmts(block.stmts);
                for (stmts) |stmt| {
                    try self.registerPatternSymbolInto(stmt.binding().pattern, target);
                }
                for (stmts) |stmt| {
                    try self.countConsumedValueInto(stmt.binding().expr, target);
                }
                try self.countConsumedUsesInto(block.final_expr, target);
            },
            .borrow_scope => |scope| {
                const bindings = self.store.getBorrowBindings(scope.bindings);
                for (bindings) |binding| {
                    try self.registerPatternSymbolInto(binding.pattern, target);
                }
                for (bindings) |binding| {
                    try self.countConsumedValueInto(binding.expr, target);
                }
                try self.countConsumedUsesInto(scope.body, target);
            },
            .if_then_else => |ite| {
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                const branches = self.store.getIfBranches(ite.branches);
                for (branches) |branch| {
                    try self.countConsumedValueInto(branch.cond, target);
                    local.clearRetainingCapacity();
                    try self.countConsumedUsesInto(branch.body, &local);
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                local.clearRetainingCapacity();
                try self.countConsumedUsesInto(ite.final_else, &local);
                {
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            .match_expr => |w| {
                try self.countConsumedUsesInto(w.value, target);
                const branches = self.store.getMatchBranches(w.branches);
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                for (branches) |branch| {
                    local.clearRetainingCapacity();
                    try self.registerPatternSymbolInto(branch.pattern, &local);
                    try self.countConsumedUsesInto(branch.guard, &local);
                    try self.countConsumedUsesInto(branch.body, &local);
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            .discriminant_switch => |ds| {
                try self.countConsumedUsesInto(ds.value, target);
                const branches = self.store.getExprSpan(ds.branches);
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                for (branches) |branch| {
                    local.clearRetainingCapacity();
                    try self.countConsumedUsesInto(branch, &local);
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            .struct_access => |sa| {
                try self.countConsumedValueInto(sa.struct_expr, target);
            },
            .tag_payload_access => |tpa| {
                try self.countConsumedUsesInto(tpa.value, target);
            },
            .lambda => |lam| {
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();

                const params = self.store.getPatternSpan(lam.params);
                for (params) |pat_id| {
                    try self.registerPatternSymbolInto(pat_id, &local);
                }
                try self.countConsumedValueInto(lam.body, &local);

                var it = local.iterator();
                while (it.next()) |entry| {
                    const key = entry.key_ptr.*;
                    if (target.contains(key)) {
                        try bumpUseCount(target, key);
                    }
                    const global_gop = try self.symbol_consumed_counts.getOrPut(key);
                    if (!global_gop.found_existing) {
                        global_gop.value_ptr.* = entry.value_ptr.*;
                    }
                }
            },
            .call => |call| {
                try self.countConsumedUsesInto(call.fn_expr, target);
                const args = self.store.getExprSpan(call.args);
                for (args) |arg_id| {
                    try self.countConsumedValueInto(arg_id, target);
                }
            },
            .list => |list| {
                const elems = self.store.getExprSpan(list.elems);
                for (elems) |elem_id| {
                    try self.countConsumedUsesInto(elem_id, target);
                }
            },
            .struct_ => |s| {
                const fields = self.store.getExprSpan(s.fields);
                for (fields) |field_id| {
                    try self.countConsumedValueInto(field_id, target);
                }
            },
            .tag => |t| {
                const args = self.store.getExprSpan(t.args);
                for (args) |arg_id| {
                    try self.countConsumedValueInto(arg_id, target);
                }
            },
            .nominal => |n| try self.countConsumedUsesInto(n.backing_expr, target),
            .early_return => |ret| try self.countConsumedValueInto(ret.expr, target),
            .dbg => |d| try self.countConsumedUsesInto(d.expr, target),
            .expect => |e| {
                try self.countConsumedValueInto(e.cond, target);
                try self.countConsumedUsesInto(e.body, target);
            },
            .semantic_low_level => |ll| {
                const args = self.store.getExprSpan(ll.args);
                const arg_ownership = ll.op.getArgOwnership();
                if (builtin.mode == .Debug and arg_ownership.len != args.len) {
                    std.debug.panic(
                        "RC invariant violated: low-level {s} expected {d} ownership entries for {d} args",
                        .{ @tagName(ll.op), arg_ownership.len, args.len },
                    );
                }

                for (args, 0..) |arg_id, i| {
                    const ownership = arg_ownership[i];
                    switch (ownership) {
                        .consume => try self.countConsumedValueInto(arg_id, target),
                        .borrow => switch (ll.op) {
                            .str_is_eq, .str_caseless_ascii_equals => try self.countConsumedValueInto(arg_id, target),
                            else => try self.countConsumedUsesInto(arg_id, target),
                        },
                    }
                }
            },
            .low_level => |ll| {
                const args = self.store.getExprSpan(ll.args);
                for (args) |arg_id| {
                    try self.countConsumedValueInto(arg_id, target);
                }
            },
            .hosted_call => |hc| {
                const args = self.store.getExprSpan(hc.args);
                for (args) |arg_id| {
                    try self.countConsumedValueInto(arg_id, target);
                }
            },
            .str_concat => |span| {
                const parts = self.store.getExprSpan(span);
                for (parts) |part_id| {
                    try self.countConsumedUsesInto(part_id, target);
                }
            },
            .int_to_str => |its| try self.countConsumedUsesInto(its.value, target),
            .float_to_str => |fts| try self.countConsumedUsesInto(fts.value, target),
            .dec_to_str => |d| try self.countConsumedUsesInto(d, target),
            .str_escape_and_quote => |s| try self.countConsumedUsesInto(s, target),
            .for_loop => |fl| {
                try self.countConsumedUsesInto(fl.list_expr, target);
                try self.registerPatternSymbolInto(fl.elem_pattern, target);
                try self.countConsumedUsesInto(fl.body, target);
            },
            .while_loop => |wl| {
                try self.countConsumedValueInto(wl.cond, target);
                try self.countConsumedUsesInto(wl.body, target);
            },
            .lookup,
            .incref,
            .decref,
            .free,
            .i64_literal,
            .i128_literal,
            .f64_literal,
            .f32_literal,
            .dec_literal,
            .str_literal,
            .bool_literal,
            .empty_list,
            .zero_arg_tag,
            .crash,
            .runtime_error,
            .break_expr,
            => {},
        }
    }

    /// Count symbol consumption within an expression whose result is consumed by
    /// the current scope. A bare lookup transfers one owned reference.
    fn countConsumedValueInto(
        self: *RcInsertPass,
        expr_id: LirExprId,
        target: *std.AutoHashMap(u64, u32),
    ) Allocator.Error!void {
        if (expr_id.isNone()) return;

        switch (self.store.getExpr(expr_id)) {
            .lookup => |lookup| {
                if (!lookup.symbol.isNone()) {
                    const key = @as(u64, @bitCast(lookup.symbol));
                    try bumpUseCount(target, key);
                    if (!self.symbol_layouts.contains(key)) {
                        try self.symbol_layouts.put(key, lookup.layout_idx);
                    }
                }
            },
            .nominal => |n| try self.countConsumedValueInto(n.backing_expr, target),
            .block => |block| {
                const stmts = self.store.getStmts(block.stmts);
                for (stmts) |stmt| {
                    try self.registerPatternSymbolInto(stmt.binding().pattern, target);
                }
                for (stmts) |stmt| {
                    try self.countConsumedValueInto(stmt.binding().expr, target);
                }
                try self.countConsumedValueInto(block.final_expr, target);
            },
            .borrow_scope => |scope| {
                const bindings = self.store.getBorrowBindings(scope.bindings);
                for (bindings) |binding| {
                    try self.registerPatternSymbolInto(binding.pattern, target);
                }
                for (bindings) |binding| {
                    try self.countConsumedValueInto(binding.expr, target);
                }
                try self.countConsumedValueInto(scope.body, target);
            },
            .if_then_else => |ite| {
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                const branches = self.store.getIfBranches(ite.branches);
                for (branches) |branch| {
                    try self.countConsumedValueInto(branch.cond, target);
                    local.clearRetainingCapacity();
                    try self.countConsumedValueInto(branch.body, &local);
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                local.clearRetainingCapacity();
                try self.countConsumedValueInto(ite.final_else, &local);
                {
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            .match_expr => |w| {
                try self.countConsumedUsesInto(w.value, target);
                const branches = self.store.getMatchBranches(w.branches);
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                for (branches) |branch| {
                    local.clearRetainingCapacity();
                    try self.registerPatternSymbolInto(branch.pattern, &local);
                    try self.countConsumedUsesInto(branch.guard, &local);
                    try self.countConsumedValueInto(branch.body, &local);
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            .discriminant_switch => |ds| {
                try self.countConsumedUsesInto(ds.value, target);
                const branches = self.store.getExprSpan(ds.branches);
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                for (branches) |branch| {
                    local.clearRetainingCapacity();
                    try self.countConsumedValueInto(branch, &local);
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            else => try self.countConsumedUsesInto(expr_id, target),
        }
    }

    /// Walk a pattern tree, calling ctx.onBind(symbol, layout_idx, reassignable) at each
    /// .bind and .as_pattern leaf. Handles the full recursion over all pattern
    /// variants so callers only need to implement the leaf action.
    fn walkPatternBinds(store: *const LirExprStore, pat_id: LirPatternId, ctx: anytype) Allocator.Error!void {
        if (pat_id.isNone()) return;
        const pat = store.getPattern(pat_id);
        switch (pat) {
            .bind => |bind| {
                if (!bind.symbol.isNone()) try ctx.onBind(bind.symbol, bind.layout_idx, bind.reassignable);
            },
            .as_pattern => |as_pat| {
                if (!as_pat.symbol.isNone()) try ctx.onBind(as_pat.symbol, as_pat.layout_idx, as_pat.reassignable);
                try walkPatternBinds(store, as_pat.inner, ctx);
            },
            .tag => |t| for (store.getPatternSpan(t.args)) |a| {
                try walkPatternBinds(store, a, ctx);
            },
            .struct_ => |s| for (store.getPatternSpan(s.fields)) |f| {
                try walkPatternBinds(store, f, ctx);
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
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
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
        // Guard against sentinel/out-of-range layout indices (e.g., named_fn, none)
        // which can appear for function-typed symbols that don't need RC.
        const idx_int = @intFromEnum(layout_idx);
        if (idx_int >= self.layout_store.layouts.len()) unreachable;
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
            .borrow_scope => |scope| self.processBorrowScope(scope, region),
            .if_then_else => |ite| self.processIfThenElse(ite.branches, ite.final_else, ite.result_layout, region),
            .match_expr => |w| self.processMatch(w.value, w.value_layout, w.branches, w.result_layout, region),
            .lambda => |lam| self.processLambda(lam, region, expr_id),
            .for_loop => |fl| self.processForLoop(fl, region, expr_id),
            .while_loop => |wl| self.processWhileLoop(wl, region, expr_id),
            .discriminant_switch => |ds| self.processDiscriminantSwitch(ds, region),
            .early_return => |ret| self.processEarlyReturn(ret, region, expr_id),
            .call => |call| {
                const new_fn_expr = try self.processExpr(call.fn_expr);
                const args_res = try self.processExprSpan(call.args);
                if (new_fn_expr == call.fn_expr and !args_res.changed) return expr_id;
                return self.store.addExpr(.{ .call = .{
                    .fn_expr = new_fn_expr,
                    .fn_layout = call.fn_layout,
                    .args = args_res.span,
                    .ret_layout = call.ret_layout,
                    .called_via = call.called_via,
                } }, region);
            },
            .list => |list| {
                const elems_res = try self.processExprSpan(list.elems);
                if (!elems_res.changed) return expr_id;
                return self.store.addExpr(.{ .list = .{
                    .list_layout = list.list_layout,
                    .elem_layout = list.elem_layout,
                    .elems = elems_res.span,
                } }, region);
            },
            .struct_ => |s| {
                const fields_res = try self.processExprSpan(s.fields);
                if (!fields_res.changed) return expr_id;
                return self.store.addExpr(.{ .struct_ = .{
                    .struct_layout = s.struct_layout,
                    .fields = fields_res.span,
                } }, region);
            },
            .struct_access => |sa| {
                const new_struct_expr = try self.processExpr(sa.struct_expr);
                if (new_struct_expr == sa.struct_expr) return expr_id;
                return self.store.addExpr(.{ .struct_access = .{
                    .struct_expr = new_struct_expr,
                    .struct_layout = sa.struct_layout,
                    .field_layout = sa.field_layout,
                    .field_idx = sa.field_idx,
                } }, region);
            },
            .tag => |tag| {
                const args_res = try self.processExprSpan(tag.args);
                if (!args_res.changed) return expr_id;
                return self.store.addExpr(.{ .tag = .{
                    .discriminant = tag.discriminant,
                    .union_layout = tag.union_layout,
                    .args = args_res.span,
                } }, region);
            },
            .semantic_low_level => |ll| {
                const args_res = try self.processExprSpan(ll.args);
                const backend_op = LowLevelMap.semanticToBackend(ll.op) orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("RC legalization invariant violated: semantic low-level {s} was not lowered earlier", .{@tagName(ll.op)});
                    }
                    unreachable;
                };
                if (!args_res.changed) {
                    return self.store.addExpr(.{ .low_level = .{
                        .op = backend_op,
                        .args = ll.args,
                        .ret_layout = ll.ret_layout,
                    } }, region);
                }
                return self.store.addExpr(.{ .low_level = .{
                    .op = backend_op,
                    .args = args_res.span,
                    .ret_layout = ll.ret_layout,
                } }, region);
            },
            .low_level => |ll| {
                const args_res = try self.processExprSpan(ll.args);
                if (!args_res.changed) return expr_id;
                return self.store.addExpr(.{ .low_level = .{
                    .op = ll.op,
                    .args = args_res.span,
                    .ret_layout = ll.ret_layout,
                } }, region);
            },
            .dbg => |d| {
                const new_expr = try self.processExpr(d.expr);
                if (new_expr == d.expr) return expr_id;
                return self.store.addExpr(.{ .dbg = .{
                    .expr = new_expr,
                    .result_layout = d.result_layout,
                } }, region);
            },
            .expect => |e| {
                const new_cond = try self.processExpr(e.cond);
                const new_body = try self.processExpr(e.body);
                if (new_cond == e.cond and new_body == e.body) return expr_id;
                return self.store.addExpr(.{ .expect = .{
                    .cond = new_cond,
                    .body = new_body,
                    .result_layout = e.result_layout,
                } }, region);
            },
            .nominal => |n| {
                const new_backing = try self.processExpr(n.backing_expr);
                if (new_backing == n.backing_expr) return expr_id;
                return self.store.addExpr(.{ .nominal = .{
                    .backing_expr = new_backing,
                    .nominal_layout = n.nominal_layout,
                } }, region);
            },
            .str_concat => |parts| {
                const parts_res = try self.processExprSpan(parts);
                if (!parts_res.changed) return expr_id;
                return self.store.addExpr(.{ .str_concat = parts_res.span }, region);
            },
            .int_to_str => |its| {
                const new_value = try self.processExpr(its.value);
                if (new_value == its.value) return expr_id;
                return self.store.addExpr(.{ .int_to_str = .{
                    .value = new_value,
                    .int_precision = its.int_precision,
                } }, region);
            },
            .float_to_str => |fts| {
                const new_value = try self.processExpr(fts.value);
                if (new_value == fts.value) return expr_id;
                return self.store.addExpr(.{ .float_to_str = .{
                    .value = new_value,
                    .float_precision = fts.float_precision,
                } }, region);
            },
            .dec_to_str => |dec_expr| {
                const new_expr = try self.processExpr(dec_expr);
                if (new_expr == dec_expr) return expr_id;
                return self.store.addExpr(.{ .dec_to_str = new_expr }, region);
            },
            .str_escape_and_quote => |str_expr| {
                const new_expr = try self.processExpr(str_expr);
                if (new_expr == str_expr) return expr_id;
                return self.store.addExpr(.{ .str_escape_and_quote = new_expr }, region);
            },
            .tag_payload_access => |tpa| {
                const new_value = try self.processExpr(tpa.value);
                if (new_value == tpa.value) return expr_id;
                return self.store.addExpr(.{ .tag_payload_access = .{
                    .value = new_value,
                    .union_layout = tpa.union_layout,
                    .payload_layout = tpa.payload_layout,
                } }, region);
            },
            .hosted_call => |hc| {
                const args_res = try self.processExprSpan(hc.args);
                if (!args_res.changed) return expr_id;
                return self.store.addExpr(.{ .hosted_call = .{
                    .index = hc.index,
                    .args = args_res.span,
                    .ret_layout = hc.ret_layout,
                } }, region);
            },
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
            .empty_list,
            .zero_arg_tag,
            .break_expr,
            .crash,
            .runtime_error,
            .incref,
            .decref,
            .free,
            => expr_id,
        };
    }

    fn processExprSpan(self: *RcInsertPass, span: LirExprSpan) Allocator.Error!struct { span: LirExprSpan, changed: bool } {
        const exprs = self.store.getExprSpan(span);
        if (exprs.len == 0) return .{ .span = span, .changed = false };

        var new_exprs = std.ArrayList(LirExprId).empty;
        defer new_exprs.deinit(self.allocator);

        var changed = false;
        for (exprs) |expr_id| {
            const new_expr = try self.processExpr(expr_id);
            if (new_expr != expr_id) changed = true;
            try new_exprs.append(self.allocator, new_expr);
        }

        if (!changed) return .{ .span = span, .changed = false };

        return .{
            .span = try self.store.addExprSpan(new_exprs.items),
            .changed = true,
        };
    }

    /// Temporarily add an expression's consumed uses into `block_consumed_uses`.
    /// Returns the exact per-symbol deltas so callers can roll them back.
    fn pushExprUsesToBlockConsumed(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!std.AutoHashMap(u64, u32) {
        var added = std.AutoHashMap(u64, u32).init(self.allocator);

        self.scratch_consumed_uses.clearRetainingCapacity();
        try self.countConsumedValueInto(expr_id, &self.scratch_consumed_uses);

        var it = self.scratch_consumed_uses.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            const amount = entry.value_ptr.*;

            const gop = try self.block_consumed_uses.getOrPut(key);
            if (gop.found_existing) {
                gop.value_ptr.* += amount;
            } else {
                gop.value_ptr.* = amount;
            }

            try added.put(key, amount);
        }

        return added;
    }

    fn pushBorrowedExprUsesToBlockConsumed(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!std.AutoHashMap(u64, u32) {
        var added = std.AutoHashMap(u64, u32).init(self.allocator);

        self.scratch_consumed_uses.clearRetainingCapacity();
        try self.countConsumedUsesInto(expr_id, &self.scratch_consumed_uses);

        var it = self.scratch_consumed_uses.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            const amount = entry.value_ptr.*;

            const gop = try self.block_consumed_uses.getOrPut(key);
            if (gop.found_existing) {
                gop.value_ptr.* += amount;
            } else {
                gop.value_ptr.* = amount;
            }

            try added.put(key, amount);
        }

        return added;
    }

    fn popExprUsesFromBlockConsumed(self: *RcInsertPass, added: *const std.AutoHashMap(u64, u32)) void {
        var it = added.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            const amount = entry.value_ptr.*;

            if (self.block_consumed_uses.getPtr(key)) |cur| {
                cur.* -|= amount;
                if (cur.* == 0) {
                    _ = self.block_consumed_uses.remove(key);
                }
            }
        }
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
        // Copy stmts to avoid iterator invalidation from nested processExpr
        // calls that may grow the stmts ArrayList.
        const stmts = try self.allocator.dupe(LirStmt, self.store.getStmts(stmts_span));
        defer self.allocator.free(stmts);

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

        // Track the latest declaration layout for each symbol in this block so we can
        // decref older decl bindings when the same symbol is rebound via another decl.
        var block_decl_layouts = std.AutoHashMap(u64, LayoutIdx).init(self.allocator);
        defer block_decl_layouts.deinit();

        var changed = false;

        // Process each statement
        for (stmts, 0..) |stmt, stmt_index| {
            const b = stmt.binding();
            // Recursively process the statement's expression
            const new_expr = try self.processExpr(b.expr);
            if (new_expr != b.expr) changed = true;

            // Track uses consumed by this statement's expression
            // so processEarlyReturn can compute remaining refs.
            {
                self.scratch_consumed_uses.clearRetainingCapacity();
                try self.countConsumedValueInto(b.expr, &self.scratch_consumed_uses);
                var use_it = self.scratch_consumed_uses.iterator();
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
            // unless the RHS reads the same symbol (e.g. `x = f(x)`), in which
            // case RHS evaluation is responsible for ownership transfer.
            if (stmt == .mutate) {
                const rhs_reads_mutated_symbol = try self.exprUsesPatternSymbol(b.expr, b.pattern);
                if (!rhs_reads_mutated_symbol) {
                    const before_len = stmt_buf.items.len;
                    try self.emitMutateDecrefsForPattern(b.pattern, region, &stmt_buf);
                    if (stmt_buf.items.len > before_len) changed = true;
                }
            }

            // Shadowing via repeated decls must release the previous binding.
            if (stmt == .decl) {
                const before_len = stmt_buf.items.len;
                try self.emitDeclShadowDecrefsForPattern(b.pattern, &block_decl_layouts, region, &stmt_buf);
                if (stmt_buf.items.len > before_len) changed = true;
            }

            // Add the (possibly updated) statement, preserving decl/mutate kind
            const new_binding: LirStmt.Binding = .{ .pattern = b.pattern, .expr = new_expr };
            try stmt_buf.append(self.allocator, switch (stmt) {
                .decl => .{ .decl = new_binding },
                .mutate => .{ .mutate = new_binding },
            });

            // Track live RC symbols and emit per-use ownership for new bindings.
            try self.trackLiveRcSymbolsForPattern(b.pattern);
            const before_len = stmt_buf.items.len;
            switch (stmt) {
                .decl => try self.emitBlockIncrefsForPattern(b.pattern, region, &stmt_buf, stmts, stmt_index, final_expr),
                .mutate => try self.emitBlockIncrefsForPattern(b.pattern, region, &stmt_buf, stmts, stmt_index, final_expr),
            }
            if (stmt_buf.items.len > before_len) changed = true;

            // Shadowed decls can introduce a fresh binding for a symbol whose
            // global uses were all consumed by the previous binding generation.
            // Release such unused new bindings immediately.
            if (stmt == .decl) {
                const before_shadow_unused = stmt_buf.items.len;
                try self.emitDeclShadowedUnusedDecrefsForPattern(
                    b.pattern,
                    &block_decl_layouts,
                    region,
                    &stmt_buf,
                    stmts,
                    stmt_index,
                    final_expr,
                );
                if (stmt_buf.items.len > before_shadow_unused) changed = true;
            }

            // Update latest decl layout map after processing this binding.
            try self.recordPatternLayouts(b.pattern, &block_decl_layouts);
        }

        // Recursively process the final expression
        const processed_final = try self.processExpr(final_expr);
        var new_final = processed_final;
        if (processed_final != final_expr) changed = true;

        // Insert decrefs for refcounted symbols bound in this block that are never used
        {
            const before_len = stmt_buf.items.len;
            for (stmts, 0..) |stmt, stmt_index| {
                const b = stmt.binding();
                switch (stmt) {
                    .decl => try self.emitBlockDecrefsForPattern(
                        b.pattern,
                        region,
                        &stmt_buf,
                        stmts,
                        stmt_index,
                        final_expr,
                    ),
                    .mutate => {},
                }
            }
            if (stmt_buf.items.len > before_len) changed = true;
        }

        var tail_decref_stmts = std.ArrayList(LirStmt).empty;
        defer tail_decref_stmts.deinit(self.allocator);
        for (stmts, 0..) |stmt, stmt_index| {
            const b = stmt.binding();
            switch (stmt) {
                .decl => try self.emitBlockTailDecrefsForPattern(
                    b.pattern,
                    final_expr,
                    region,
                    &tail_decref_stmts,
                    stmts,
                    stmt_index,
                ),
                .mutate => {},
            }
        }
        if (tail_decref_stmts.items.len > 0) {
            new_final = try self.wrapExprWithTailStmts(new_final, result_layout, tail_decref_stmts.items, region);
            changed = true;
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

    fn processBorrowScope(
        self: *RcInsertPass,
        scope: anytype,
        region: Region,
    ) Allocator.Error!LirExprId {
        var stmt_buf = std.ArrayList(LirStmt).empty;
        defer stmt_buf.deinit(self.allocator);

        var borrow_consumed_uses = std.AutoHashMap(u64, u32).init(self.allocator);
        defer borrow_consumed_uses.deinit();
        try self.countConsumedValueInto(scope.body, &borrow_consumed_uses);

        const bindings = self.store.getBorrowBindings(scope.bindings);
        for (bindings) |binding| {
            var scratch = std.AutoHashMap(u64, u32).init(self.allocator);
            defer scratch.deinit();
            try self.registerPatternSymbolInto(binding.pattern, &scratch);
            const new_expr = try self.processExpr(binding.expr);
            try stmt_buf.append(self.allocator, .{ .decl = .{
                .pattern = binding.pattern,
                .expr = new_expr,
            } });
            try self.emitBorrowRcOpsForPatternInto(binding.pattern, &borrow_consumed_uses, region, &stmt_buf);
        }

        const new_body = try self.processExpr(scope.body);
        try stmt_buf.append(self.allocator, .{ .decl = .{
            .pattern = scope.result_pattern,
            .expr = new_body,
        } });

        for (bindings) |binding| {
            try self.emitBlockBorrowDecrefsForPattern(binding.pattern, region, &stmt_buf, &.{}, 0, LirExprId.none);
        }

        const final_lookup = try self.store.addExpr(.{ .lookup = .{
            .symbol = scope.result_symbol,
            .layout_idx = scope.result_layout,
        } }, region);
        const stmts = try self.store.addStmts(stmt_buf.items);
        return self.store.addExpr(.{ .block = .{
            .stmts = stmts,
            .final_expr = final_lookup,
            .result_layout = scope.result_layout,
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
        // Copy branches to avoid iterator invalidation from nested processExpr
        // calls that may grow the if_branches ArrayList.
        const branches = try self.allocator.dupe(LirIfBranch, self.store.getIfBranches(branches_span));
        defer self.allocator.free(branches);

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
            self.scratch_consumed_uses.clearRetainingCapacity();
            try self.countConsumedValueInto(branch.body, &self.scratch_consumed_uses);
            var it = self.scratch_consumed_uses.keyIterator();
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
            self.scratch_consumed_uses.clearRetainingCapacity();
            try self.countConsumedValueInto(final_else_id, &self.scratch_consumed_uses);
            var it = self.scratch_consumed_uses.keyIterator();
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
            // Branch condition is evaluated before branch body.
            var cond_added = try self.pushExprUsesToBlockConsumed(branch.cond);

            self.scratch_consumed_uses.clearRetainingCapacity();
            try self.countConsumedValueInto(branch.body, &self.scratch_consumed_uses);
            try self.setPendingBranchRcAdj(&self.scratch_consumed_uses, &symbols_in_any_branch);
            const processed_body = try self.processExpr(branch.body);
            self.clearPendingBranchRcAdj();

            self.popExprUsesFromBlockConsumed(&cond_added);
            cond_added.deinit();

            // Re-count: processExpr may have used scratch_uses recursively
            self.scratch_consumed_uses.clearRetainingCapacity();
            try self.countConsumedValueInto(branch.body, &self.scratch_consumed_uses);
            const new_body = try self.wrapBranchWithRcOps(processed_body, &self.scratch_consumed_uses, &symbols_in_any_branch, result_layout, region);
            try new_branches.append(self.allocator, .{
                .cond = branch.cond,
                .body = new_body,
            });
        }

        // else branch
        self.scratch_consumed_uses.clearRetainingCapacity();
        try self.countConsumedValueInto(final_else_id, &self.scratch_consumed_uses);
        try self.setPendingBranchRcAdj(&self.scratch_consumed_uses, &symbols_in_any_branch);
        const processed_else = try self.processExpr(final_else_id);
        self.clearPendingBranchRcAdj();
        self.scratch_consumed_uses.clearRetainingCapacity();
        try self.countConsumedValueInto(final_else_id, &self.scratch_consumed_uses);
        const new_else = try self.wrapBranchWithRcOps(processed_else, &self.scratch_consumed_uses, &symbols_in_any_branch, result_layout, region);

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
        // Copy branches to a local buffer to avoid iterator invalidation.
        // processExpr can recursively process nested match expressions, which
        // appends to self.store.match_branches and may reallocate the backing
        // buffer, invalidating any slices obtained from getMatchBranches.
        const branches_slice = self.store.getMatchBranches(branches_span);
        const branches = try self.allocator.dupe(LirMatchBranch, branches_slice);
        defer self.allocator.free(branches);

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
            self.scratch_consumed_uses.clearRetainingCapacity();
            try self.countConsumedValueInto(branch.body, &self.scratch_consumed_uses);
            var it = self.scratch_consumed_uses.keyIterator();
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

        // Match scrutinee is evaluated before guards/bodies in any taken branch.
        var value_added = try self.pushBorrowedExprUsesToBlockConsumed(value);
        defer {
            self.popExprUsesFromBlockConsumed(&value_added);
            value_added.deinit();
        }

        for (branches) |branch| {
            // Guard is evaluated before body for the taken branch.
            var guard_added = try self.pushExprUsesToBlockConsumed(branch.guard);

            self.scratch_consumed_uses.clearRetainingCapacity();
            try self.countConsumedValueInto(branch.body, &self.scratch_consumed_uses);
            try self.setPendingBranchRcAdj(&self.scratch_consumed_uses, &symbols_in_any_branch);
            const processed_body = try self.processExpr(branch.body);
            self.clearPendingBranchRcAdj();

            self.popExprUsesFromBlockConsumed(&guard_added);
            guard_added.deinit();

            const processed_guard = try self.processExpr(branch.guard);
            // Re-count body uses for branch RC wrappers
            self.scratch_consumed_uses.clearRetainingCapacity();
            try self.countConsumedValueInto(branch.body, &self.scratch_consumed_uses);
            const outer_rc_body = try self.wrapBranchWithRcOps(processed_body, &self.scratch_consumed_uses, &symbols_in_any_branch, result_layout, region);
            const new_body = try self.wrapExprWithPatternBorrowRcOps(outer_rc_body, branch.pattern, &self.scratch_consumed_uses, result_layout, region);
            // Count guard uses for wrapGuardWithIncref
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(branch.guard, &self.scratch_uses);
            const new_guard = try self.wrapGuardWithIncref(processed_guard, &self.scratch_uses, region);
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
        // Switch discriminant is evaluated before any branch body.
        var value_added = try self.pushBorrowedExprUsesToBlockConsumed(ds.value);
        defer {
            self.popExprUsesFromBlockConsumed(&value_added);
            value_added.deinit();
        }

        // Copy branches to avoid iterator invalidation from nested processExpr
        // calls that may grow the exprs ArrayList.
        const branches = try self.allocator.dupe(LirExprId, self.store.getExprSpan(ds.branches));
        defer self.allocator.free(branches);

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
            self.scratch_consumed_uses.clearRetainingCapacity();
            try self.countConsumedValueInto(br_id, &self.scratch_consumed_uses);
            var it = self.scratch_consumed_uses.keyIterator();
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
            self.scratch_consumed_uses.clearRetainingCapacity();
            try self.countConsumedValueInto(br_id, &self.scratch_consumed_uses);
            try self.setPendingBranchRcAdj(&self.scratch_consumed_uses, &symbols_in_any_branch);
            const processed = try self.processExpr(br_id);
            self.clearPendingBranchRcAdj();
            // Re-count: processExpr may have used scratch_uses recursively
            self.scratch_consumed_uses.clearRetainingCapacity();
            try self.countConsumedValueInto(br_id, &self.scratch_consumed_uses);
            const wrapped = try self.wrapBranchWithRcOps(processed, &self.scratch_consumed_uses, &symbols_in_any_branch, result_layout, region);
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

        // Lambda parameters are live bindings for the whole body and must be
        // included in early_return cleanup.
        const params = self.store.getPatternSpan(lam.params);
        for (params) |pat_id| {
            try self.trackLiveRcSymbolsForPattern(pat_id);
        }

        const new_body = try self.processExpr(lam.body);

        // Count consumed uses locally within the lambda body.
        self.scratch_consumed_uses.clearRetainingCapacity();
        try self.countConsumedValueInto(lam.body, &self.scratch_consumed_uses);

        // Emit pre-body RC ops for lambda parameters.
        var rc_stmts = std.ArrayList(LirStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        for (params) |pat_id| {
            try self.emitRcOpsForPatternInto(pat_id, &self.scratch_consumed_uses, region, &rc_stmts);
        }

        var final_body = new_body;
        if (rc_stmts.items.len > 0) {
            const stmts_span = try self.store.addStmts(rc_stmts.items);
            final_body = try self.store.addExpr(.{ .block = .{
                .stmts = stmts_span,
                .final_expr = new_body,
                .result_layout = lam.ret_layout,
            } }, region);
        }

        // The original parameter reference must survive borrowed-only uses until
        // after the body result has been materialized.
        var tail_stmts = std.ArrayList(LirStmt).empty;
        defer tail_stmts.deinit(self.allocator);
        for (params) |pat_id| {
            try self.emitTailDecrefsForPatternInto(pat_id, &self.scratch_consumed_uses, region, &tail_stmts);
        }
        final_body = try self.wrapExprWithTailStmts(final_body, lam.ret_layout, tail_stmts.items, region);

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

    /// Process a for loop expression.
    /// For loops bind an element via elem_pattern each iteration.
    /// The loop provides 1 reference per element. We emit body-local RC ops
    /// for the element binding similar to lambda params.
    fn processForLoop(self: *RcInsertPass, fl: anytype, region: Region, expr_id: LirExprId) Allocator.Error!LirExprId {
        // The loop element binding is live while processing the body and must
        // be considered by early_return cleanup.
        const saved_live_len = self.live_rc_symbols.items.len;
        defer self.live_rc_symbols.shrinkRetainingCapacity(saved_live_len);
        try self.trackLiveRcSymbolsForPattern(fl.elem_pattern);

        // The loop consumes its input list when iteration completes.
        // Early returns in the body must therefore treat the source expression's
        // ownership as already transferred into the loop.
        var list_added = try self.pushExprUsesToBlockConsumed(fl.list_expr);
        defer {
            self.popExprUsesFromBlockConsumed(&list_added);
            list_added.deinit();
        }

        const new_body = try self.processExpr(fl.body);

        // Count uses locally within the loop body (using scratch_uses)
        self.scratch_consumed_uses.clearRetainingCapacity();
        try self.countConsumedValueInto(fl.body, &self.scratch_consumed_uses);

        // Emit pre-body RC ops for the elem_pattern.
        var rc_stmts = std.ArrayList(LirStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        try self.emitRcOpsForPatternInto(fl.elem_pattern, &self.scratch_consumed_uses, region, &rc_stmts);

        var final_body = new_body;
        if (rc_stmts.items.len > 0) {
            const stmts_span = try self.store.addStmts(rc_stmts.items);
            final_body = try self.store.addExpr(.{ .block = .{
                .stmts = stmts_span,
                .final_expr = new_body,
                .result_layout = .zst,
            } }, region);
        }

        var tail_stmts = std.ArrayList(LirStmt).empty;
        defer tail_stmts.deinit(self.allocator);
        try self.emitTailDecrefsForPatternInto(fl.elem_pattern, &self.scratch_consumed_uses, region, &tail_stmts);
        final_body = try self.wrapExprWithTailStmts(final_body, .zst, tail_stmts.items, region);

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
        self.scratch_consumed_uses.clearRetainingCapacity();
        try self.countConsumedValueInto(ret.expr, &self.scratch_consumed_uses);

        // Collect cleanup decrefs for live RC symbols not consumed by the return
        var cleanup_stmts = std.ArrayList(LirStmt).empty;
        defer cleanup_stmts.deinit(self.allocator);

        const live_syms = self.live_rc_symbols.items[self.early_return_scope_base..];
        for (live_syms) |live| {
            const key = @as(u64, @bitCast(live.symbol));
            const ret_use_count: u32 = self.scratch_consumed_uses.get(key) orelse 0;
            // Total refs = global_use_count + pending branch RC adjustments.
            // The branch wrapper will prepend RC ops that execute before
            // the early return: increfs (positive adj) add refs to clean up,
            // decrefs (negative adj) reduce refs since they're already handled.
            const global_count = self.symbol_consumed_counts.get(key) orelse unreachable;
            // Mutable bindings are re-bound over time; global symbol use counts
            // over-approximate live refs for the current value at an early_return.
            // Treat the current mutable binding as owning exactly one live ref.
            const base_count: u32 = if (live.reassignable) 1 else global_count;
            const branch_adj: i32 = self.pending_branch_rc_adj.get(key) orelse 0;
            const effective_signed: i32 = @as(i32, @intCast(base_count)) + branch_adj;
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
        const dead_final = try self.store.addExpr(.{ .runtime_error = .{ .ret_layout = ret.ret_layout } }, region);
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
            fn onBind(ctx: @This(), symbol: Symbol, _: LayoutIdx, _: bool) Allocator.Error!void {
                try ctx.set.put(@as(u64, @bitCast(symbol)), {});
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .set = set });
    }

    fn exprUsesPatternSymbol(self: *RcInsertPass, expr_id: LirExprId, pat_id: LirPatternId) Allocator.Error!bool {
        var pattern_symbols = std.AutoHashMap(u64, void).init(self.allocator);
        defer pattern_symbols.deinit();
        try self.collectPatternSymbols(pat_id, &pattern_symbols);
        if (pattern_symbols.count() == 0) return false;

        self.scratch_uses.clearRetainingCapacity();
        defer self.scratch_uses.clearRetainingCapacity();
        try self.countUsesInto(expr_id, &self.scratch_uses);

        var it = self.scratch_uses.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.* == 0) continue;
            if (pattern_symbols.contains(entry.key_ptr.*)) return true;
        }
        return false;
    }

    fn exprUsesSymbol(self: *RcInsertPass, expr_id: LirExprId, symbol: Symbol) Allocator.Error!bool {
        self.scratch_uses.clearRetainingCapacity();
        defer self.scratch_uses.clearRetainingCapacity();
        try self.countUsesInto(expr_id, &self.scratch_uses);
        const key = @as(u64, @bitCast(symbol));
        return (self.scratch_uses.get(key) orelse 0) > 0;
    }

    fn exprConsumesSymbol(self: *RcInsertPass, expr_id: LirExprId, symbol: Symbol) Allocator.Error!bool {
        self.scratch_consumed_uses.clearRetainingCapacity();
        defer self.scratch_consumed_uses.clearRetainingCapacity();
        try self.countConsumedValueInto(expr_id, &self.scratch_consumed_uses);
        const key = @as(u64, @bitCast(symbol));
        return (self.scratch_consumed_uses.get(key) orelse 0) > 0;
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
            .borrow_scope => |scope| {
                const bindings = self.store.getBorrowBindings(scope.bindings);
                for (bindings) |binding| {
                    try self.collectPatternSymbols(binding.pattern, set);
                    try self.collectExprBoundSymbols(binding.expr, set);
                }
                try self.collectPatternSymbols(scope.result_pattern, set);
                try self.collectExprBoundSymbols(scope.body, set);
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
            .struct_ => |s| {
                const fields = self.store.getExprSpan(s.fields);
                for (fields) |field_id| try self.collectExprBoundSymbols(field_id, set);
            },
            .tag => |t| {
                const args = self.store.getExprSpan(t.args);
                for (args) |arg_id| try self.collectExprBoundSymbols(arg_id, set);
            },
            .struct_access => |sa| try self.collectExprBoundSymbols(sa.struct_expr, set),
            .nominal => |n| try self.collectExprBoundSymbols(n.backing_expr, set),
            .early_return => |ret| try self.collectExprBoundSymbols(ret.expr, set),
            .dbg => |d| try self.collectExprBoundSymbols(d.expr, set),
            .semantic_low_level => |ll| {
                const args = self.store.getExprSpan(ll.args);
                for (args) |arg_id| try self.collectExprBoundSymbols(arg_id, set);
            },
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
            // Lambda is a separate scope in LIR — don't recurse.
            .lambda => {},
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
            const layout_idx = self.symbol_layouts.get(key) orelse unreachable;
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
            const layout_idx = self.symbol_layouts.get(key) orelse unreachable;
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

    /// Wrap an expression with borrow-style RC ops for symbols introduced by a pattern.
    /// Match-pattern bindings borrow from the scrutinee, so each use requires an incref.
    fn wrapExprWithPatternBorrowRcOps(
        self: *RcInsertPass,
        expr: LirExprId,
        pat_id: LirPatternId,
        local_uses: *const std.AutoHashMap(u64, u32),
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        var rc_stmts = std.ArrayList(LirStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        try self.emitBorrowRcOpsForPatternInto(pat_id, local_uses, region, &rc_stmts);
        if (rc_stmts.items.len == 0) return expr;

        const stmts_span = try self.store.addStmts(rc_stmts.items);
        return self.store.addExpr(.{ .block = .{
            .stmts = stmts_span,
            .final_expr = expr,
            .result_layout = result_layout,
        } }, region);
    }

    fn wrapExprWithTailStmts(
        self: *RcInsertPass,
        expr: LirExprId,
        result_layout: LayoutIdx,
        tail_stmts: []const LirStmt,
        region: Region,
    ) Allocator.Error!LirExprId {
        if (tail_stmts.len == 0) return expr;

        const result_bind = try self.freshResultPattern(result_layout, region);
        var stmts = std.ArrayList(LirStmt).empty;
        defer stmts.deinit(self.allocator);

        try stmts.append(self.allocator, .{ .decl = .{
            .pattern = result_bind.pattern,
            .expr = expr,
        } });
        try stmts.appendSlice(self.allocator, tail_stmts);

        const final_lookup = try self.store.addExpr(.{ .lookup = .{
            .symbol = result_bind.symbol,
            .layout_idx = result_layout,
        } }, region);
        const stmts_span = try self.store.addStmts(stmts.items);
        return self.store.addExpr(.{ .block = .{
            .stmts = stmts_span,
            .final_expr = final_lookup,
            .result_layout = result_layout,
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
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = @as(u64, @bitCast(symbol));
                const resolved_layout = ctx.pass.symbol_layouts.get(key) orelse layout_idx;
                if (ctx.pass.layoutNeedsRc(resolved_layout)) {
                    try ctx.pass.emitDecrefInto(symbol, resolved_layout, ctx.region, ctx.stmts);
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self, .region = region, .stmts = stmts });
    }

    /// Emit decrefs for prior decl bindings shadowed by this new decl pattern.
    fn emitDeclShadowDecrefsForPattern(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        prior_decl_layouts: *const std.AutoHashMap(u64, LayoutIdx),
        region: Region,
        stmts: *std.ArrayList(LirStmt),
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            prior_decl_layouts: *const std.AutoHashMap(u64, LayoutIdx),
            region: Region,
            stmts: *std.ArrayList(LirStmt),
            fn onBind(ctx: @This(), symbol: Symbol, _: LayoutIdx, _: bool) Allocator.Error!void {
                const key = @as(u64, @bitCast(symbol));
                const previous_layout = ctx.prior_decl_layouts.get(key) orelse return;
                // If prior statements already consumed this symbol, shadowing should not
                // decref the old binding again.
                if ((ctx.pass.block_consumed_uses.get(key) orelse 0) > 0) return;
                if (ctx.pass.layoutNeedsRc(previous_layout)) {
                    try ctx.pass.emitDecrefInto(symbol, previous_layout, ctx.region, ctx.stmts);
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{
            .pass = self,
            .prior_decl_layouts = prior_decl_layouts,
            .region = region,
            .stmts = stmts,
        });
    }

    /// Emit decrefs for newly shadowed decl bindings that are immediately unused.
    /// This handles symbol-key conflation across shadowed generations by using
    /// consumed-uses progress at the shadow point.
    fn emitDeclShadowedUnusedDecrefsForPattern(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        prior_decl_layouts: *const std.AutoHashMap(u64, LayoutIdx),
        region: Region,
        stmts: *std.ArrayList(LirStmt),
        block_stmts: []const LirStmt,
        stmt_index: usize,
        final_expr: LirExprId,
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            prior_decl_layouts: *const std.AutoHashMap(u64, LayoutIdx),
            region: Region,
            stmts: *std.ArrayList(LirStmt),
            block_stmts: []const LirStmt,
            stmt_index: usize,
            final_expr: LirExprId,
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = @as(u64, @bitCast(symbol));
                if (!ctx.prior_decl_layouts.contains(key)) return; // not a shadowing bind

                const resolved_layout = ctx.pass.symbol_layouts.get(key) orelse layout_idx;
                if (!ctx.pass.layoutNeedsRc(resolved_layout)) return;
                if (try ctx.pass.hasLaterBlockUse(ctx.block_stmts, ctx.stmt_index, ctx.final_expr, symbol)) return;

                const consumed = ctx.pass.block_consumed_uses.get(key) orelse 0;
                const total = ctx.pass.symbol_consumed_counts.get(key) orelse 0;
                if (consumed >= total) {
                    try ctx.pass.emitDecrefInto(symbol, resolved_layout, ctx.region, ctx.stmts);
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{
            .pass = self,
            .prior_decl_layouts = prior_decl_layouts,
            .region = region,
            .stmts = stmts,
            .block_stmts = block_stmts,
            .stmt_index = stmt_index,
            .final_expr = final_expr,
        });
    }

    /// Record (or update) the resolved layout for each symbol bound by this pattern.
    fn recordPatternLayouts(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        target: *std.AutoHashMap(u64, LayoutIdx),
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            target: *std.AutoHashMap(u64, LayoutIdx),
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = @as(u64, @bitCast(symbol));
                const resolved_layout = ctx.pass.symbol_layouts.get(key) orelse layout_idx;
                try ctx.target.put(key, resolved_layout);
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self, .target = target });
    }

    /// Recursively track live RC symbols for early_return cleanup.
    fn trackLiveRcSymbolsForPattern(self: *RcInsertPass, pat_id: LirPatternId) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, reassignable: bool) Allocator.Error!void {
                if (ctx.pass.layoutNeedsRc(layout_idx)) {
                    // Mutable reassignments re-bind the same symbol. Keep a single
                    // live entry per symbol so early_return cleanup does not double-decref.
                    for (ctx.pass.live_rc_symbols.items) |*live| {
                        if (std.meta.eql(live.symbol, symbol)) {
                            live.layout_idx = layout_idx;
                            live.reassignable = reassignable;
                            return;
                        }
                    }
                    try ctx.pass.live_rc_symbols.append(ctx.pass.allocator, .{
                        .symbol = symbol,
                        .layout_idx = layout_idx,
                        .reassignable = reassignable,
                    });
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self });
    }

    /// Recursively emit increfs for multi-consumed refcounted symbols bound by a pattern.
    /// Uses self.symbol_consumed_counts (global counts) — for use in processBlock.
    fn emitBlockIncrefsForPattern(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        region: Region,
        rc_stmts: *std.ArrayList(LirStmt),
        _: []const LirStmt,
        _: usize,
        _: LirExprId,
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            region: Region,
            rc_stmts: *std.ArrayList(LirStmt),
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, reassignable: bool) Allocator.Error!void {
                const key = @as(u64, @bitCast(symbol));
                const resolved_layout = ctx.pass.symbol_layouts.get(key) orelse layout_idx;
                if (ctx.pass.layoutNeedsRc(resolved_layout)) {
                    if (reassignable) return;
                    const use_count = ctx.pass.symbol_consumed_counts.get(key) orelse 0;
                    if (use_count > 1) {
                        try ctx.pass.emitIncrefInto(symbol, resolved_layout, @intCast(use_count - 1), ctx.region, ctx.rc_stmts);
                    }
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{
            .pass = self,
            .region = region,
            .rc_stmts = rc_stmts,
        });
    }

    /// Recursively emit decrefs for refcounted symbols whose owned reference is never consumed.
    /// Uses self.symbol_consumed_counts (global counts) — for use in processBlock.
    fn emitBlockDecrefsForPattern(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        region: Region,
        stmts: *std.ArrayList(LirStmt),
        block_stmts: []const LirStmt,
        stmt_index: usize,
        final_expr: LirExprId,
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            region: Region,
            stmts: *std.ArrayList(LirStmt),
            block_stmts: []const LirStmt,
            stmt_index: usize,
            final_expr: LirExprId,
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, reassignable: bool) Allocator.Error!void {
                const key = @as(u64, @bitCast(symbol));
                const resolved_layout = ctx.pass.symbol_layouts.get(key) orelse layout_idx;
                if (!ctx.pass.layoutNeedsRc(resolved_layout)) return;
                if (!reassignable and try ctx.pass.hasLaterShadowingDecl(ctx.block_stmts, ctx.stmt_index, symbol)) return;

                if (reassignable) {
                    if (try ctx.pass.exprUsesSymbol(ctx.final_expr, symbol)) return;
                    try ctx.pass.emitDecrefInto(symbol, resolved_layout, ctx.region, ctx.stmts);
                    return;
                }

                const use_count = ctx.pass.symbol_consumed_counts.get(key) orelse 0;
                if (use_count == 0) {
                    const final_reads_symbol = try ctx.pass.exprUsesSymbol(ctx.final_expr, symbol);
                    if (final_reads_symbol) return;
                    try ctx.pass.emitDecrefInto(symbol, resolved_layout, ctx.region, ctx.stmts);
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{
            .pass = self,
            .region = region,
            .stmts = stmts,
            .block_stmts = block_stmts,
            .stmt_index = stmt_index,
            .final_expr = final_expr,
        });
    }

    fn emitBlockTailDecrefsForPattern(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        final_expr: LirExprId,
        region: Region,
        stmts: *std.ArrayList(LirStmt),
        block_stmts: []const LirStmt,
        stmt_index: usize,
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            final_expr: LirExprId,
            region: Region,
            stmts: *std.ArrayList(LirStmt),
            block_stmts: []const LirStmt,
            stmt_index: usize,
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, reassignable: bool) Allocator.Error!void {
                const key = @as(u64, @bitCast(symbol));
                const resolved_layout = ctx.pass.symbol_layouts.get(key) orelse layout_idx;
                if (!ctx.pass.layoutNeedsRc(resolved_layout)) return;
                if (!reassignable and try ctx.pass.hasLaterShadowingDecl(ctx.block_stmts, ctx.stmt_index, symbol)) return;
                const final_reads_symbol = try ctx.pass.exprUsesSymbol(ctx.final_expr, symbol);
                if (!final_reads_symbol) return;
                if (reassignable) {
                    if (try ctx.pass.exprConsumesSymbol(ctx.final_expr, symbol)) return;
                    try ctx.pass.emitDecrefInto(symbol, resolved_layout, ctx.region, ctx.stmts);
                    return;
                }
                const use_count = ctx.pass.symbol_consumed_counts.get(key) orelse 0;
                if (use_count != 0) return;
                try ctx.pass.emitDecrefInto(symbol, resolved_layout, ctx.region, ctx.stmts);
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{
            .pass = self,
            .final_expr = final_expr,
            .region = region,
            .stmts = stmts,
            .block_stmts = block_stmts,
            .stmt_index = stmt_index,
        });
    }

    fn hasLaterShadowingDecl(self: *RcInsertPass, block_stmts: []const LirStmt, stmt_index: usize, symbol: Symbol) Allocator.Error!bool {
        if (stmt_index + 1 >= block_stmts.len) return false;

        const target_key = @as(u64, @bitCast(symbol));
        var found = false;

        const Ctx = struct {
            target_key: u64,
            found: *bool,
            fn onBind(ctx: @This(), bind_symbol: Symbol, _: LayoutIdx, _: bool) Allocator.Error!void {
                if (ctx.found.*) return;
                if (@as(u64, @bitCast(bind_symbol)) == ctx.target_key) {
                    ctx.found.* = true;
                }
            }
        };

        for (block_stmts[stmt_index + 1 ..]) |later_stmt| {
            if (later_stmt != .decl) continue;
            try walkPatternBinds(self.store, later_stmt.decl.pattern, Ctx{
                .target_key = target_key,
                .found = &found,
            });
            if (found) return true;
        }

        return false;
    }

    fn hasLaterBlockUse(
        self: *RcInsertPass,
        block_stmts: []const LirStmt,
        stmt_index: usize,
        final_expr: LirExprId,
        symbol: Symbol,
    ) Allocator.Error!bool {
        if (stmt_index + 1 < block_stmts.len) {
            for (block_stmts[stmt_index + 1 ..]) |later_stmt| {
                if (try self.exprUsesSymbol(later_stmt.binding().expr, symbol)) return true;
            }
        }

        return self.exprUsesSymbol(final_expr, symbol);
    }

    /// Recursively emit cleanup decrefs for borrowed temps bound in a block.
    /// These bindings own exactly one original produced reference until block exit.
    fn emitBlockBorrowDecrefsForPattern(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        region: Region,
        stmts: *std.ArrayList(LirStmt),
        _: []const LirStmt,
        _: usize,
        _: LirExprId,
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            region: Region,
            stmts: *std.ArrayList(LirStmt),
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = @as(u64, @bitCast(symbol));
                const resolved_layout = ctx.pass.symbol_layouts.get(key) orelse layout_idx;
                if (ctx.pass.layoutNeedsRc(resolved_layout)) {
                    try ctx.pass.emitDecrefInto(symbol, resolved_layout, ctx.region, ctx.stmts);
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{
            .pass = self,
            .region = region,
            .stmts = stmts,
        });
    }

    /// Recursively emit pre-body RC ops for all symbols bound by a pattern.
    /// For each bound symbol with a refcounted layout:
    /// - use_count <= 1: no pre-body action
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
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                if (ctx.pass.layoutNeedsRc(layout_idx)) {
                    const key = @as(u64, @bitCast(symbol));
                    const use_count = ctx.local_uses.get(key) orelse 0;
                    if (use_count > 1) {
                        try ctx.pass.emitIncrefInto(symbol, layout_idx, @intCast(use_count - 1), ctx.region, ctx.rc_stmts);
                    }
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self, .local_uses = local_uses, .region = region, .rc_stmts = rc_stmts });
    }

    fn emitTailDecrefsForPatternInto(
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
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                if (!ctx.pass.layoutNeedsRc(layout_idx)) return;
                const key = @as(u64, @bitCast(symbol));
                const use_count = ctx.local_uses.get(key) orelse 0;
                if (use_count == 0) {
                    try ctx.pass.emitDecrefInto(symbol, layout_idx, ctx.region, ctx.rc_stmts);
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{
            .pass = self,
            .local_uses = local_uses,
            .region = region,
            .rc_stmts = rc_stmts,
        });
    }

    /// Recursively emit borrow-style RC ops for symbols bound by a pattern.
    /// Pattern-bound values in `match` are borrowed from the scrutinee, so:
    /// - use_count == 0: no action
    /// - use_count > 0: emit incref(use_count)
    fn emitBorrowRcOpsForPatternInto(
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
            fn onBind(ctx: @This(), symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                if (ctx.pass.layoutNeedsRc(layout_idx)) {
                    const key = @as(u64, @bitCast(symbol));
                    const use_count = ctx.local_uses.get(key) orelse 0;
                    if (use_count > 0) {
                        try ctx.pass.emitIncrefInto(symbol, layout_idx, @intCast(use_count), ctx.region, ctx.rc_stmts);
                    }
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{
            .pass = self,
            .local_uses = local_uses,
            .region = region,
            .rc_stmts = rc_stmts,
        });
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
    const sym_x = LIR.Symbol.fromRaw(@as(u64, @as(u32, @bitCast(ident_x))));

    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, Region.zero());
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
    const sym_s = LIR.Symbol.fromRaw(@as(u64, @as(u32, @bitCast(ident_s))));

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
    const sym_s = LIR.Symbol.fromRaw(@as(u64, @as(u32, @bitCast(ident_s))));

    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_lit } }});

    // Final expression is an i64 literal (s is unused)
    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, Region.zero());

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
        .borrow_scope => |scope| {
            const bindings = store.getBorrowBindings(scope.bindings);
            for (bindings) |binding| {
                const sub = countRcOps(store, binding.expr);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
            const sub = countRcOps(store, scope.body);
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
        .for_loop => |fl| {
            const sub_list = countRcOps(store, fl.list_expr);
            increfs += sub_list.increfs;
            decrefs += sub_list.decrefs;
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
        .call => |c| {
            const sub_fn = countRcOps(store, c.fn_expr);
            increfs += sub_fn.increfs;
            decrefs += sub_fn.decrefs;
            const args = store.getExprSpan(c.args);
            for (args) |arg_id| {
                const sub = countRcOps(store, arg_id);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
        },
        .semantic_low_level => |ll| {
            const args = store.getExprSpan(ll.args);
            for (args) |arg_id| {
                const sub = countRcOps(store, arg_id);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
        },
        .low_level => |ll| {
            const args = store.getExprSpan(ll.args);
            for (args) |arg_id| {
                const sub = countRcOps(store, arg_id);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
        },
        .hosted_call => |hc| {
            const args = store.getExprSpan(hc.args);
            for (args) |arg_id| {
                const sub = countRcOps(store, arg_id);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
        },
        .list => |list| {
            const elems = store.getExprSpan(list.elems);
            for (elems) |elem_id| {
                const sub = countRcOps(store, elem_id);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
        },
        .struct_ => |s| {
            const fields = store.getExprSpan(s.fields);
            for (fields) |field_id| {
                const sub = countRcOps(store, field_id);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
        },
        .struct_access => |sa| {
            const sub = countRcOps(store, sa.struct_expr);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .tag => |t| {
            const args = store.getExprSpan(t.args);
            for (args) |arg_id| {
                const sub = countRcOps(store, arg_id);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
        },
        .early_return => |ret| {
            const sub = countRcOps(store, ret.expr);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .dbg => |d| {
            const sub = countRcOps(store, d.expr);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .expect => |e| {
            const cond_sub = countRcOps(store, e.cond);
            increfs += cond_sub.increfs;
            decrefs += cond_sub.decrefs;
            const body_sub = countRcOps(store, e.body);
            increfs += body_sub.increfs;
            decrefs += body_sub.decrefs;
        },
        .nominal => |n| {
            const sub = countRcOps(store, n.backing_expr);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .str_concat => |span| {
            const parts = store.getExprSpan(span);
            for (parts) |part_id| {
                const sub = countRcOps(store, part_id);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
        },
        .int_to_str => |its| {
            const sub = countRcOps(store, its.value);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .float_to_str => |fts| {
            const sub = countRcOps(store, fts.value);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .dec_to_str => |d| {
            const sub = countRcOps(store, d);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .str_escape_and_quote => |s| {
            const sub = countRcOps(store, s);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .tag_payload_access => |tpa| {
            const sub = countRcOps(store, tpa.value);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .i64_literal,
        .i128_literal,
        .f64_literal,
        .f32_literal,
        .dec_literal,
        .str_literal,
        .bool_literal,
        .lookup,
        .empty_list,
        .zero_arg_tag,
        .break_expr,
        .crash,
        .runtime_error,
        .free,
        => {},
    }
    return .{ .increfs = increfs, .decrefs = decrefs };
}

fn countDecrefsForSymbol(store: *const LirExprStore, expr_id: LirExprId, symbol: LIR.Symbol) u32 {
    if (expr_id.isNone()) return 0;

    const key = @as(u64, @bitCast(symbol));
    const expr = store.getExpr(expr_id);
    return switch (expr) {
        .block => |block| blk: {
            var total: u32 = 0;
            for (store.getStmts(block.stmts)) |stmt| {
                total += countDecrefsForSymbol(store, stmt.binding().expr, symbol);
            }
            total += countDecrefsForSymbol(store, block.final_expr, symbol);
            break :blk total;
        },
        .borrow_scope => |scope| blk: {
            var total: u32 = 0;
            for (store.getBorrowBindings(scope.bindings)) |binding| {
                total += countDecrefsForSymbol(store, binding.expr, symbol);
            }
            total += countDecrefsForSymbol(store, scope.body, symbol);
            break :blk total;
        },
        .if_then_else => |ite| blk: {
            var total: u32 = 0;
            for (store.getIfBranches(ite.branches)) |branch| {
                total += countDecrefsForSymbol(store, branch.cond, symbol);
                total += countDecrefsForSymbol(store, branch.body, symbol);
            }
            total += countDecrefsForSymbol(store, ite.final_else, symbol);
            break :blk total;
        },
        .match_expr => |m| blk: {
            var total: u32 = countDecrefsForSymbol(store, m.value, symbol);
            for (store.getMatchBranches(m.branches)) |branch| {
                total += countDecrefsForSymbol(store, branch.guard, symbol);
                total += countDecrefsForSymbol(store, branch.body, symbol);
            }
            break :blk total;
        },
        .lambda => |lam| countDecrefsForSymbol(store, lam.body, symbol),
        .for_loop => |fl| countDecrefsForSymbol(store, fl.list_expr, symbol) + countDecrefsForSymbol(store, fl.body, symbol),
        .while_loop => |wl| countDecrefsForSymbol(store, wl.cond, symbol) + countDecrefsForSymbol(store, wl.body, symbol),
        .discriminant_switch => |ds| blk: {
            var total: u32 = countDecrefsForSymbol(store, ds.value, symbol);
            for (store.getExprSpan(ds.branches)) |branch_id| {
                total += countDecrefsForSymbol(store, branch_id, symbol);
            }
            break :blk total;
        },
        .call => |call| blk: {
            var total: u32 = countDecrefsForSymbol(store, call.fn_expr, symbol);
            for (store.getExprSpan(call.args)) |arg_id| {
                total += countDecrefsForSymbol(store, arg_id, symbol);
            }
            break :blk total;
        },
        .semantic_low_level => |ll| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(ll.args)) |arg_id| {
                total += countDecrefsForSymbol(store, arg_id, symbol);
            }
            break :blk total;
        },
        .low_level => |ll| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(ll.args)) |arg_id| {
                total += countDecrefsForSymbol(store, arg_id, symbol);
            }
            break :blk total;
        },
        .list => |l| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(l.elems)) |elem_id| {
                total += countDecrefsForSymbol(store, elem_id, symbol);
            }
            break :blk total;
        },
        .struct_ => |s| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(s.fields)) |field_id| {
                total += countDecrefsForSymbol(store, field_id, symbol);
            }
            break :blk total;
        },
        .tag => |t| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(t.args)) |arg_id| {
                total += countDecrefsForSymbol(store, arg_id, symbol);
            }
            break :blk total;
        },
        .struct_access => |sa| countDecrefsForSymbol(store, sa.struct_expr, symbol),
        .nominal => |n| countDecrefsForSymbol(store, n.backing_expr, symbol),
        .early_return => |ret| countDecrefsForSymbol(store, ret.expr, symbol),
        .dbg => |d| countDecrefsForSymbol(store, d.expr, symbol),
        .str_concat => |parts| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(parts)) |part_id| {
                total += countDecrefsForSymbol(store, part_id, symbol);
            }
            break :blk total;
        },
        .int_to_str => |its| countDecrefsForSymbol(store, its.value, symbol),
        .float_to_str => |fts| countDecrefsForSymbol(store, fts.value, symbol),
        .dec_to_str => |d| countDecrefsForSymbol(store, d, symbol),
        .str_escape_and_quote => |s| countDecrefsForSymbol(store, s, symbol),
        .tag_payload_access => |tpa| countDecrefsForSymbol(store, tpa.value, symbol),
        .decref => |dec| blk: {
            const dec_expr = store.getExpr(dec.value);
            break :blk if (dec_expr == .lookup and @as(u64, @bitCast(dec_expr.lookup.symbol)) == key) 1 else 0;
        },
        else => 0,
    };
}

/// Helper to make a symbol with a given ident index.
fn makeSymbol(idx: u29) LIR.Symbol {
    const ident = base.Ident.Idx{
        .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
        .idx = idx,
    };
    return LIR.Symbol.fromRaw(@as(u64, @as(u32, @bitCast(ident))));
}

test "RC borrowed string expression releases original temporary binding" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_tmp = makeSymbol(1);

    const large_text =
        "This string is deliberately longer than RocStr small-string storage";
    const str_idx = try env.lir_store.insertString(large_text);
    const str_lit = try env.lir_store.addExpr(.{ .str_literal = str_idx }, Region.zero());
    const lookup_tmp = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_tmp, .layout_idx = str_layout } }, Region.zero());
    const escaped = try env.lir_store.addExpr(.{ .str_escape_and_quote = lookup_tmp }, Region.zero());
    const pat_tmp = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_tmp, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_tmp, .expr = str_lit } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = escaped,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const rc = countRcOps(&env.lir_store, result);

    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC borrowed list element keeps outer binding cleanup" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const inner_list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const outer_list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(inner_list_layout));
    const sym_inner = makeSymbol(1);
    const sym_outer = makeSymbol(2);

    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());
    const inner_elems = try env.lir_store.addExprSpan(&.{ one, two });
    const inner_list = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = inner_list_layout,
        .elem_layout = i64_layout,
        .elems = inner_elems,
    } }, Region.zero());

    const lookup_inner = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_inner, .layout_idx = inner_list_layout } }, Region.zero());
    const outer_elems = try env.lir_store.addExprSpan(&.{lookup_inner});
    const outer_list = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = outer_list_layout,
        .elem_layout = inner_list_layout,
        .elems = outer_elems,
    } }, Region.zero());
    const lookup_outer = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_outer, .layout_idx = outer_list_layout } }, Region.zero());

    const pat_inner = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_inner, .layout_idx = inner_list_layout } }, Region.zero());
    const pat_outer = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_outer, .layout_idx = outer_list_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_inner, .expr = inner_list } },
        .{ .decl = .{ .pattern = pat_outer, .expr = outer_list } },
    });
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup_outer,
        .result_layout = outer_list_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const rc = countRcOps(&env.lir_store, result);

    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC if result matched later tail-cleans matched binding" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_x = makeSymbol(1);
    const sym_result = makeSymbol(2);

    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());
    const elems = try env.lir_store.addExprSpan(&.{ one, two });
    const x_list = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());
    const lookup_x_then = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_x, .layout_idx = list_layout } }, Region.zero());
    const lookup_x_else = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_x, .layout_idx = list_layout } }, Region.zero());
    const cond = try env.lir_store.addExpr(.{ .bool_literal = true }, Region.zero());
    const if_branches = try env.lir_store.addIfBranches(&.{.{
        .cond = cond,
        .body = lookup_x_then,
    }});
    const if_expr = try env.lir_store.addExpr(.{ .if_then_else = .{
        .branches = if_branches,
        .final_else = lookup_x_else,
        .result_layout = list_layout,
    } }, Region.zero());

    const lookup_result = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_result, .layout_idx = list_layout } }, Region.zero());
    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = i64_layout } }, Region.zero());
    const wildcard = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = list_layout } }, Region.zero());
    const match_branches = try env.lir_store.addMatchBranches(&.{.{
        .pattern = wildcard,
        .guard = LirExprId.none,
        .body = zero,
    }});
    const match_expr = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = lookup_result,
        .value_layout = list_layout,
        .branches = match_branches,
        .result_layout = i64_layout,
    } }, Region.zero());

    const pat_x = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_x, .layout_idx = list_layout } }, Region.zero());
    const pat_result = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_result, .layout_idx = list_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_x, .expr = x_list } },
        .{ .decl = .{ .pattern = pat_result, .expr = if_expr } },
    });
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = match_expr,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const rc = countRcOps(&env.lir_store, result);

    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
    try std.testing.expectEqual(@as(u32, 0), countDecrefsForSymbol(&env.lir_store, result, sym_x));
    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_result));
}

test "RC lambda loop source is consumed by the loop itself" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_list = makeSymbol(1);
    const sym_elem = makeSymbol(2);

    const lookup_list = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const body = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const pat_elem = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_elem, .layout_idx = i64_layout } }, Region.zero());
    const for_expr = try env.lir_store.addExpr(.{ .for_loop = .{
        .list_expr = lookup_list,
        .elem_layout = i64_layout,
        .elem_pattern = pat_elem,
        .body = body,
    } }, Region.zero());
    const pat_list = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{pat_list});
    const lam = try env.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = .none,
        .params = params,
        .body = for_expr,
        .ret_layout = .zst,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(lam);
    const rc = countRcOps(&env.lir_store, result);

    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
}

test "RC fold-style lambda body does not tail-decref consumed list param" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_list = makeSymbol(1);
    const sym_init = makeSymbol(2);
    const sym_elem = makeSymbol(3);

    const lookup_list = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const lookup_init = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_init, .layout_idx = i64_layout } }, Region.zero());
    const body = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const pat_elem = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_elem, .layout_idx = i64_layout } }, Region.zero());
    const for_expr = try env.lir_store.addExpr(.{ .for_loop = .{
        .list_expr = lookup_list,
        .elem_layout = i64_layout,
        .elem_pattern = pat_elem,
        .body = body,
    } }, Region.zero());
    const wild = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, Region.zero());
    const block_stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = wild, .expr = for_expr } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = block_stmts,
        .final_expr = lookup_init,
        .result_layout = i64_layout,
    } }, Region.zero());

    const pat_list = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const pat_init = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_init, .layout_idx = i64_layout } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{ pat_list, pat_init });
    const lam = try env.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = .none,
        .params = params,
        .body = block_expr,
        .ret_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(lam);

    try std.testing.expectEqual(@as(u32, 0), countDecrefsForSymbol(&env.lir_store, result, sym_list));
}

test "RC builtin-fold shape does not decref consumed list param" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const fn_layout: LayoutIdx = .none;

    const sym_list = makeSymbol(1);
    const sym_init = makeSymbol(2);
    const sym_step = makeSymbol(3);
    const sym_state = makeSymbol(4);
    const sym_elem = makeSymbol(5);

    const lookup_list = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const lookup_init = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_init, .layout_idx = i64_layout } }, Region.zero());
    const lookup_step = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_step, .layout_idx = fn_layout } }, Region.zero());
    const lookup_state = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_state, .layout_idx = i64_layout } }, Region.zero());
    const lookup_elem = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_elem, .layout_idx = i64_layout } }, Region.zero());

    const step_args = try env.lir_store.addExprSpan(&.{ lookup_state, lookup_elem });
    const step_call = try env.lir_store.addExpr(.{ .call = .{
        .fn_expr = lookup_step,
        .fn_layout = fn_layout,
        .args = step_args,
        .ret_layout = i64_layout,
        .called_via = .apply,
    } }, Region.zero());

    const pat_state = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_state,
        .layout_idx = i64_layout,
        .reassignable = true,
    } }, Region.zero());
    const loop_body_stmts = try env.lir_store.addStmts(&.{.{ .mutate = .{
        .pattern = pat_state,
        .expr = step_call,
    } }});
    const unit = try env.lir_store.addExpr(.{ .struct_ = .{
        .struct_layout = .none,
        .fields = try env.lir_store.addExprSpan(&.{}),
    } }, Region.zero());
    const loop_body = try env.lir_store.addExpr(.{ .block = .{
        .stmts = loop_body_stmts,
        .final_expr = unit,
        .result_layout = .zst,
    } }, Region.zero());

    const pat_elem = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_elem, .layout_idx = i64_layout } }, Region.zero());
    const for_expr = try env.lir_store.addExpr(.{ .for_loop = .{
        .list_expr = lookup_list,
        .elem_layout = i64_layout,
        .elem_pattern = pat_elem,
        .body = loop_body,
    } }, Region.zero());

    const wild_zst = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, Region.zero());
    const body_stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_state, .expr = lookup_init } },
        .{ .decl = .{ .pattern = wild_zst, .expr = for_expr } },
    });
    const body_block = try env.lir_store.addExpr(.{ .block = .{
        .stmts = body_stmts,
        .final_expr = lookup_state,
        .result_layout = i64_layout,
    } }, Region.zero());

    const pat_list = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const pat_init = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_init, .layout_idx = i64_layout } }, Region.zero());
    const pat_step = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_step, .layout_idx = fn_layout } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{ pat_list, pat_init, pat_step });
    const lam = try env.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = fn_layout,
        .params = params,
        .body = body_block,
        .ret_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(lam);

    try std.testing.expectEqual(@as(u32, 0), countDecrefsForSymbol(&env.lir_store, result, sym_list));
}

test "RC mutable list binding tail-cleans borrowed final use" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_acc = makeSymbol(1);

    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = i64_layout } }, Region.zero());
    const elems = try env.lir_store.addExprSpan(&.{zero});
    const acc_init = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());
    const lookup_acc = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_acc, .layout_idx = list_layout } }, Region.zero());
    const args = try env.lir_store.addExprSpan(&.{lookup_acc});
    const len_expr = try env.lir_store.addExpr(.{ .semantic_low_level = .{
        .op = .list_len,
        .args = args,
        .ret_layout = i64_layout,
    } }, Region.zero());

    const pat_acc = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_acc,
        .layout_idx = list_layout,
        .reassignable = true,
    } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_acc, .expr = acc_init } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = len_expr,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const rc = countRcOps(&env.lir_store, result);

    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC mutable list loop accumulator tail-cleans current binding after borrowed final use" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_acc = makeSymbol(1);
    const sym_elem = makeSymbol(2);
    const sym_src = makeSymbol(3);

    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = i64_layout } }, Region.zero());
    const acc_init_elems = try env.lir_store.addExprSpan(&.{zero});
    const acc_init = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = acc_init_elems,
    } }, Region.zero());

    const lookup_acc_for_append = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_acc, .layout_idx = list_layout } }, Region.zero());
    const lookup_elem = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_elem, .layout_idx = i64_layout } }, Region.zero());
    const append_args = try env.lir_store.addExprSpan(&.{ lookup_acc_for_append, lookup_elem });
    const append_expr = try env.lir_store.addExpr(.{ .semantic_low_level = .{
        .op = .list_append,
        .args = append_args,
        .ret_layout = list_layout,
    } }, Region.zero());

    const pat_acc = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_acc,
        .layout_idx = list_layout,
        .reassignable = true,
    } }, Region.zero());
    const pat_elem = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_elem,
        .layout_idx = i64_layout,
    } }, Region.zero());

    const loop_body_stmts = try env.lir_store.addStmts(&.{.{ .mutate = .{
        .pattern = pat_acc,
        .expr = append_expr,
    } }});
    const unit = try env.lir_store.addExpr(.{ .struct_ = .{
        .struct_layout = .none,
        .fields = try env.lir_store.addExprSpan(&.{}),
    } }, Region.zero());
    const loop_body = try env.lir_store.addExpr(.{ .block = .{
        .stmts = loop_body_stmts,
        .final_expr = unit,
        .result_layout = .zst,
    } }, Region.zero());

    const lookup_src = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_src, .layout_idx = list_layout } }, Region.zero());
    const for_expr = try env.lir_store.addExpr(.{ .for_loop = .{
        .list_expr = lookup_src,
        .elem_layout = i64_layout,
        .elem_pattern = pat_elem,
        .body = loop_body,
    } }, Region.zero());

    const lookup_acc_for_len = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_acc, .layout_idx = list_layout } }, Region.zero());
    const len_args = try env.lir_store.addExprSpan(&.{lookup_acc_for_len});
    const len_expr = try env.lir_store.addExpr(.{ .semantic_low_level = .{
        .op = .list_len,
        .args = len_args,
        .ret_layout = i64_layout,
    } }, Region.zero());

    const pat_src = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_src,
        .layout_idx = list_layout,
    } }, Region.zero());
    const empty_src = try env.lir_store.addExpr(.{ .empty_list = .{
        .elem_layout = i64_layout,
        .list_layout = list_layout,
    } }, Region.zero());
    const wild_zst = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, Region.zero());

    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_src, .expr = empty_src } },
        .{ .decl = .{ .pattern = pat_acc, .expr = acc_init } },
        .{ .decl = .{ .pattern = wild_zst, .expr = for_expr } },
    });
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = len_expr,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_acc));
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
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, Region.zero());

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
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, Region.zero());

    // True branch body: low_level num_add(s, s) — uses s twice
    const ll_args = try env.lir_store.addExprSpan(&.{ lookup_s1, lookup_s2 });
    const ll_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .num_add,
        .args = ll_args,
        .ret_layout = str_layout,
    } }, Region.zero());

    const wild_pat1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild_pat2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const match_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = wild_pat1, .guard = LirExprId.none, .body = ll_expr },
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
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, Region.zero());

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

    // Build lambda body: low_level num_add(s, s)
    const lookup_s1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const ll_args = try env.lir_store.addExprSpan(&.{ lookup_s1, lookup_s2 });
    const ll_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .num_add,
        .args = ll_args,
        .ret_layout = str_layout,
    } }, Region.zero());

    // Build lambda: |s| low_level(s, s)
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{pat_s});
    const lambda_expr = try env.lir_store.addExpr(.{ .lambda = .{
        .fn_layout = str_layout,
        .params = params,
        .body = ll_expr,
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
    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, Region.zero());

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
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, Region.zero());
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
    const int_0 = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = .i64 } }, Region.zero());
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

test "RC shadowed list decl only cleans latest generation at block tail" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_list = makeSymbol(1);

    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());
    const first_elems = try env.lir_store.addExprSpan(&.{one});
    const second_elems = try env.lir_store.addExprSpan(&.{two});
    const first_list = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = first_elems,
    } }, Region.zero());
    const second_list = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = second_elems,
    } }, Region.zero());
    const lookup_list = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const len_args = try env.lir_store.addExprSpan(&.{lookup_list});
    const len_expr = try env.lir_store.addExpr(.{ .semantic_low_level = .{
        .op = .list_len,
        .args = len_args,
        .ret_layout = i64_layout,
    } }, Region.zero());

    const pat_first = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_list,
        .layout_idx = list_layout,
    } }, Region.zero());
    const pat_second = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_list,
        .layout_idx = list_layout,
    } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_first, .expr = first_list } },
        .{ .decl = .{ .pattern = pat_second, .expr = second_list } },
    });
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = len_expr,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);
    const rc = countRcOps(&env.lir_store, result);

    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 2), rc.decrefs);
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
    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, Region.zero());

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
    const int_1 = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = .i64 } }, Region.zero());
    const int_2 = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = .i64 } }, Region.zero());

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

    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, Region.zero());

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
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, Region.zero());
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
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 42, .layout_idx = .i64 } }, Region.zero());
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

test "RC early_return nested in call arguments gets cleanup decrefs" {
    // Build: { s = "hello"; _ = str_concat(s, early_return("x")); s }
    // s has global_use_count = 2 (call arg + final lookup) -> incref(1) at binding.
    // early_return doesn't use s and happens before call completes, so both refs
    // must be decref'd by early_return cleanup.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_s = makeSymbol(1);

    const str_hello = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const str_x = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const lookup_s_call = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s_final = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const early_ret = try env.lir_store.addExpr(.{ .early_return = .{
        .expr = str_x,
        .ret_layout = str_layout,
    } }, Region.zero());

    const call_args = try env.lir_store.addExprSpan(&.{ lookup_s_call, early_ret });
    const str_concat_call = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .str_concat,
        .args = call_args,
        .ret_layout = str_layout,
    } }, Region.zero());

    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const wild = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_s, .expr = str_hello } },
        .{ .decl = .{ .pattern = wild, .expr = str_concat_call } },
    });

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup_s_final,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 2), rc.decrefs);
}
