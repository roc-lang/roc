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
const OwnershipNormalize = @import("OwnershipNormalize.zig");

const LirExprId = LIR.LirExprId;
const Symbol = LIR.Symbol;
const LirStmt = LIR.LirStmt;
const LirStmtSpan = LIR.LirStmtSpan;
const LirPatternId = LIR.LirPatternId;
const LirPatternSpan = LIR.LirPatternSpan;
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

    /// Tracks consumed-owner counts that have been rewritten to explicit
    /// retained temporaries, so the original owner is not preserved twice.
    retained_owner_use_debits: std.AutoHashMap(u64, u32),

    /// Tracks layout for each symbol (for generating incref/decref with correct layout).
    symbol_layouts: std.AutoHashMap(u64, LayoutIdx),

    /// Tracks live RC symbols across blocks for early_return cleanup.
    live_rc_symbols: std.ArrayList(LiveRcSymbol),

    /// Tracks live mutable cells across blocks for normal and early-return cleanup.
    live_cells: std.ArrayList(LiveCell),

    /// Base index into live_rc_symbols for the current function scope.
    /// early_return only cleans up symbols from early_return_scope_base onward.
    early_return_scope_base: usize,

    /// Base index into live_cells for the current function scope.
    /// early_return only cleans up cells from early_return_cell_scope_base onward.
    early_return_cell_scope_base: usize,

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

    ownership: ?OwnershipNormalize.Result,

    const LiveRcSymbol = struct {
        key: u64,
        symbol: Symbol,
        layout_idx: LayoutIdx,
        reassignable: bool,
        owned_ref_count: u32,
    };

    const LiveCell = struct {
        cell: Symbol,
        layout_idx: LayoutIdx,
    };

    const ExprOwnership = enum {
        consume,
        borrow,
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
            .retained_owner_use_debits = std.AutoHashMap(u64, u32).init(allocator),
            .symbol_layouts = std.AutoHashMap(u64, LayoutIdx).init(allocator),
            .live_rc_symbols = std.ArrayList(LiveRcSymbol).empty,
            .live_cells = std.ArrayList(LiveCell).empty,
            .early_return_scope_base = 0,
            .early_return_cell_scope_base = 0,
            .block_consumed_uses = std.AutoHashMap(u64, u32).init(allocator),
            .cumulative_consumed_uses = std.AutoHashMap(u64, u32).init(allocator),
            .pending_branch_rc_adj = std.AutoHashMap(u64, i32).init(allocator),
            .scratch_uses = std.AutoHashMap(u64, u32).init(allocator),
            .scratch_consumed_uses = std.AutoHashMap(u64, u32).init(allocator),
            .scratch_keys = try base.Scratch(u64).init(allocator),
            .ownership = null,
        };
    }

    pub fn deinit(self: *RcInsertPass) void {
        if (self.ownership) |*ownership| ownership.deinit();
        self.symbol_use_counts.deinit();
        self.symbol_consumed_counts.deinit();
        self.retained_owner_use_debits.deinit();
        self.symbol_layouts.deinit();
        self.live_rc_symbols.deinit(self.allocator);
        self.live_cells.deinit(self.allocator);
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
        const loop_normalized_expr = try self.normalizeBorrowedLoopSources(expr_id);
        if (builtin.mode == .Debug) {
            try self.validateExprTreeIds(loop_normalized_expr);
        }
        const normalized_expr = try self.materializeRcCellLoadOperands(loop_normalized_expr);
        if (builtin.mode == .Debug) {
            try self.validateExprTreeIds(normalized_expr);
        }

        // Clear all accumulated state so insertRcOps is safe to call multiple times.
        self.symbol_use_counts.clearRetainingCapacity();
        self.symbol_consumed_counts.clearRetainingCapacity();
        self.retained_owner_use_debits.clearRetainingCapacity();
        self.symbol_layouts.clearRetainingCapacity();
        self.live_rc_symbols.clearRetainingCapacity();
        self.live_cells.clearRetainingCapacity();
        self.block_consumed_uses.clearRetainingCapacity();
        self.cumulative_consumed_uses.clearRetainingCapacity();
        self.pending_branch_rc_adj.clearRetainingCapacity();
        self.scratch_uses.clearRetainingCapacity();
        self.scratch_consumed_uses.clearRetainingCapacity();
        self.early_return_scope_base = 0;
        self.early_return_cell_scope_base = 0;
        if (self.ownership) |*ownership| ownership.deinit();
        try self.uniquifyBindingPatterns(normalized_expr);
        self.ownership = try OwnershipNormalize.analyze(self.allocator, self.store, normalized_expr);

        // Phase 1: Count symbol references and symbol ownership consumption.
        try self.countUses(normalized_expr);
        try self.countConsumedUses(normalized_expr);

        // Phase 2: Walk the tree and insert RC operations
        return self.processExpr(normalized_expr);
    }

    pub fn insertRcOpsForProcBody(
        self: *RcInsertPass,
        body_expr_id: LirExprId,
        params: LirPatternSpan,
        ret_layout: LayoutIdx,
    ) Allocator.Error!LirExprId {
        const loop_normalized_expr = try self.normalizeBorrowedLoopSources(body_expr_id);
        if (builtin.mode == .Debug) {
            try self.validateExprTreeIds(loop_normalized_expr);
        }
        const normalized_expr = try self.materializeRcCellLoadOperands(loop_normalized_expr);
        if (builtin.mode == .Debug) {
            try self.validateExprTreeIds(normalized_expr);
        }

        self.symbol_use_counts.clearRetainingCapacity();
        self.symbol_consumed_counts.clearRetainingCapacity();
        self.retained_owner_use_debits.clearRetainingCapacity();
        self.symbol_layouts.clearRetainingCapacity();
        self.live_rc_symbols.clearRetainingCapacity();
        self.live_cells.clearRetainingCapacity();
        self.block_consumed_uses.clearRetainingCapacity();
        self.cumulative_consumed_uses.clearRetainingCapacity();
        self.pending_branch_rc_adj.clearRetainingCapacity();
        self.scratch_uses.clearRetainingCapacity();
        self.scratch_consumed_uses.clearRetainingCapacity();
        self.early_return_scope_base = 0;
        self.early_return_cell_scope_base = 0;
        if (self.ownership) |*ownership| ownership.deinit();
        try self.uniquifyBindingPatterns(normalized_expr);
        self.ownership = try OwnershipNormalize.analyzeWithParams(
            self.allocator,
            self.store,
            params,
            normalized_expr,
        );

        try self.countUses(normalized_expr);
        try self.countConsumedUses(normalized_expr);

        const region = self.store.getExprRegion(normalized_expr);
        return self.processOwnedBodyWithParams(normalized_expr, params, ret_layout, region);
    }

    fn validateExprId(self: *const RcInsertPass, expr_id: LirExprId, context: []const u8) void {
        if (expr_id.isNone()) return;
        const idx = @intFromEnum(expr_id);
        if (idx >= self.store.exprs.items.len) {
            std.debug.panic("RC invariant violated: invalid expr id {d} in {s} (expr count {d})", .{ idx, context, self.store.exprs.items.len });
        }
    }

    fn validateExprTreeIds(self: *const RcInsertPass, expr_id: LirExprId) Allocator.Error!void {
        self.validateExprId(expr_id, "root");
        if (expr_id.isNone()) return;

        switch (self.store.getExpr(expr_id)) {
            .block => |block| {
                for (self.store.getStmts(block.stmts)) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| {
                            self.validateExprId(binding.expr, @tagName(stmt));
                            try self.validateExprTreeIds(binding.expr);
                        },
                        .cell_init, .cell_store => |binding| {
                            self.validateExprId(binding.expr, @tagName(stmt));
                            try self.validateExprTreeIds(binding.expr);
                        },
                        .cell_drop => {},
                    }
                }
                self.validateExprId(block.final_expr, "block.final_expr");
                try self.validateExprTreeIds(block.final_expr);
            },
            .if_then_else => |ite| {
                for (self.store.getIfBranches(ite.branches)) |branch| {
                    try self.validateExprTreeIds(branch.cond);
                    try self.validateExprTreeIds(branch.body);
                }
                try self.validateExprTreeIds(ite.final_else);
            },
            .match_expr => |match_expr| {
                try self.validateExprTreeIds(match_expr.value);
                for (self.store.getMatchBranches(match_expr.branches)) |branch| {
                    try self.validateExprTreeIds(branch.guard);
                    try self.validateExprTreeIds(branch.body);
                }
            },
            .for_loop => |fl| {
                try self.validateExprTreeIds(fl.list_expr);
                try self.validateExprTreeIds(fl.body);
            },
            .while_loop => |wl| {
                try self.validateExprTreeIds(wl.cond);
                try self.validateExprTreeIds(wl.body);
            },
            .proc_call => |call| {
                for (self.store.getExprSpan(call.args)) |arg| try self.validateExprTreeIds(arg);
            },
            .low_level => |ll| for (self.store.getExprSpan(ll.args)) |arg| try self.validateExprTreeIds(arg),
            .hosted_call => |hc| for (self.store.getExprSpan(hc.args)) |arg| try self.validateExprTreeIds(arg),
            .list => |list_expr| for (self.store.getExprSpan(list_expr.elems)) |elem| try self.validateExprTreeIds(elem),
            .struct_ => |struct_expr| for (self.store.getExprSpan(struct_expr.fields)) |field| try self.validateExprTreeIds(field),
            .tag => |tag_expr| for (self.store.getExprSpan(tag_expr.args)) |arg| try self.validateExprTreeIds(arg),
            .struct_access => |sa| try self.validateExprTreeIds(sa.struct_expr),
            .tag_payload_access => |tpa| try self.validateExprTreeIds(tpa.value),
            .nominal => |nominal| try self.validateExprTreeIds(nominal.backing_expr),
            .dbg => |dbg_expr| try self.validateExprTreeIds(dbg_expr.expr),
            .expect => |expect_expr| {
                try self.validateExprTreeIds(expect_expr.cond);
                try self.validateExprTreeIds(expect_expr.body);
            },
            .early_return => |ret| try self.validateExprTreeIds(ret.expr),
            .str_concat => |parts| for (self.store.getExprSpan(parts)) |part| try self.validateExprTreeIds(part),
            .int_to_str => |its| try self.validateExprTreeIds(its.value),
            .float_to_str => |fts| try self.validateExprTreeIds(fts.value),
            .dec_to_str => |value| try self.validateExprTreeIds(value),
            .str_escape_and_quote => |value| try self.validateExprTreeIds(value),
            .discriminant_switch => |ds| {
                try self.validateExprTreeIds(ds.value);
                for (self.store.getExprSpan(ds.branches)) |branch| try self.validateExprTreeIds(branch);
            },
            .lookup,
            .cell_load,
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

    fn lookupKey(self: *const RcInsertPass, expr_id: LirExprId, symbol: Symbol) u64 {
        if (self.ownership) |ownership| {
            if (ownership.getLookupRef(expr_id)) |ref_id| {
                return @intFromEnum(ref_id);
            }
        }
        return @as(u64, @bitCast(symbol));
    }

    fn patternKey(self: *const RcInsertPass, pat_id: LirPatternId, symbol: Symbol) u64 {
        if (self.ownership) |ownership| {
            if (ownership.getPatternRef(pat_id)) |ref_id| {
                return @intFromEnum(ref_id);
            }
        }
        return @as(u64, @bitCast(symbol));
    }

    fn keyInfo(self: *const RcInsertPass, key: u64) ?OwnershipNormalize.RefInfo {
        if (self.ownership) |ownership| {
            const idx: usize = @intCast(key);
            if (idx < ownership.ref_infos.items.len) {
                return ownership.ref_infos.items[idx];
            }
        }
        return null;
    }

    fn keyUseMode(self: *const RcInsertPass, key: u64) OwnershipNormalize.UseMode {
        if (self.keyInfo(key)) |info| return info.use_mode;
        return .consume;
    }

    fn keyIntroducesOwner(self: *const RcInsertPass, key: u64) bool {
        if (self.keyInfo(key)) |info| {
            return switch (info.owner_kind) {
                .owned, .retained => true,
                .borrowed, .unmanaged => false,
            };
        }
        return true;
    }

    fn keySymbol(self: *const RcInsertPass, key: u64, fallback: Symbol) Symbol {
        if (self.keyInfo(key)) |info| return info.symbol;
        return fallback;
    }

    fn keyLayout(self: *const RcInsertPass, key: u64, fallback: LayoutIdx) LayoutIdx {
        if (self.keyInfo(key)) |info| return info.layout_idx;
        return self.symbol_layouts.get(key) orelse fallback;
    }

    fn keyReassignable(self: *const RcInsertPass, key: u64, fallback: bool) bool {
        if (self.keyInfo(key)) |info| return info.reassignable;
        return fallback;
    }

    fn liveOwnedRefCountFromUseCount(use_count: u32, reassignable: bool) u32 {
        if (reassignable) return 1;
        return if (use_count == 0) 1 else use_count;
    }

    fn consumedOwnerKey(self: *const RcInsertPass, key: u64) ?u64 {
        var current = key;
        while (self.keyInfo(current)) |info| {
            switch (info.owner_kind) {
                .borrowed => |owner_ref| current = @intFromEnum(owner_ref),
                .owned, .retained => return current,
                .unmanaged => return null,
            }
        }
        return current;
    }

    fn ownerUseCountFromMap(self: *const RcInsertPass, uses: *const std.AutoHashMap(u64, u32), key: u64) u32 {
        const owner_key = self.consumedOwnerKey(key) orelse return 0;
        if (owner_key == key and self.keyInfo(key) == null) {
            return uses.get(key) orelse 0;
        }

        var total: u32 = 0;
        var it = uses.iterator();
        while (it.next()) |entry| {
            const entry_owner = self.consumedOwnerKey(entry.key_ptr.*) orelse continue;
            if (entry_owner == owner_key) {
                total += entry.value_ptr.*;
            }
        }
        return total;
    }

    fn liveOwnedRefCountForKey(self: *const RcInsertPass, key: u64, reassignable: bool) u32 {
        if (self.keyInfo(key)) |info| {
            switch (info.owner_kind) {
                .borrowed, .unmanaged => return 0,
                .owned, .retained => {},
            }
        }
        const use_count = self.effectiveGlobalOwnerUseCount(key);
        return liveOwnedRefCountFromUseCount(use_count, reassignable);
    }

    fn effectiveGlobalOwnerUseCount(self: *const RcInsertPass, key: u64) u32 {
        const raw = self.ownerUseCountFromMap(&self.symbol_consumed_counts, key);
        const owner_key = self.consumedOwnerKey(key) orelse return 0;
        const debit = self.retained_owner_use_debits.get(owner_key) orelse 0;
        return raw -| debit;
    }

    fn liveOwnedRefCountFromLocalUses(
        self: *const RcInsertPass,
        key: u64,
        reassignable: bool,
        local_uses: *const std.AutoHashMap(u64, u32),
    ) u32 {
        if (self.keyInfo(key)) |info| {
            switch (info.owner_kind) {
                .borrowed, .unmanaged => return 0,
                .owned, .retained => {},
            }
        }
        const use_count = self.ownerUseCountFromMap(local_uses, key);
        return liveOwnedRefCountFromUseCount(use_count, reassignable);
    }

    fn freshSyntheticSymbol(self: *RcInsertPass) Symbol {
        return self.store.freshSyntheticSymbol();
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

    fn removeLiveCell(self: *RcInsertPass, cell: Symbol) void {
        var i = self.live_cells.items.len;
        while (i > 0) {
            i -= 1;
            if (self.live_cells.items[i].cell == cell) {
                _ = self.live_cells.orderedRemove(i);
                return;
            }
        }
    }

    fn bindExprToFreshLookupWithSemantics(
        self: *RcInsertPass,
        stmts: *std.ArrayList(LirStmt),
        expr_id: LirExprId,
        layout_idx: LayoutIdx,
        semantics: LirStmt.BindingSemantics,
        region: Region,
    ) Allocator.Error!struct { symbol: Symbol, lookup: LirExprId } {
        const temp = try self.freshResultPattern(layout_idx, region);
        try stmts.append(self.allocator, .{ .decl = .{
            .pattern = temp.pattern,
            .expr = expr_id,
            .semantics = semantics,
        } });
        const lookup = try self.store.addExpr(.{ .lookup = .{
            .symbol = temp.symbol,
            .layout_idx = layout_idx,
        } }, region);
        return .{ .symbol = temp.symbol, .lookup = lookup };
    }

    fn bindExprToFreshLookup(
        self: *RcInsertPass,
        stmts: *std.ArrayList(LirStmt),
        expr_id: LirExprId,
        layout_idx: LayoutIdx,
        region: Region,
    ) Allocator.Error!struct { symbol: Symbol, lookup: LirExprId } {
        const temp = try self.bindExprToFreshLookupWithSemantics(stmts, expr_id, layout_idx, .owned, region);
        return .{ .symbol = temp.symbol, .lookup = temp.lookup };
    }

    fn rebuildProcCall(
        self: *RcInsertPass,
        call: anytype,
        args: LirExprSpan,
        region: Region,
    ) Allocator.Error!LirExprId {
        return self.store.addExpr(.{ .proc_call = .{
            .proc = call.proc,
            .args = args,
            .ret_layout = call.ret_layout,
            .called_via = call.called_via,
        } }, region);
    }

    fn exprAliasesManagedRef(self: *const RcInsertPass, expr_id: LirExprId, layout_idx: LayoutIdx) bool {
        if (!self.layoutNeedsRc(layout_idx)) return false;
        if (expr_id.isNone()) return false;

        return switch (self.store.getExpr(expr_id)) {
            .lookup => |lookup| !lookup.symbol.isNone(),
            .block => |block| self.exprAliasesManagedRef(block.final_expr, layout_idx),
            .dbg => |dbg_expr| self.exprAliasesManagedRef(dbg_expr.expr, layout_idx),
            .nominal => |nominal| self.exprAliasesManagedRef(nominal.backing_expr, layout_idx),
            else => false,
        };
    }

    fn exprResultLayout(self: *const RcInsertPass, expr_id: LirExprId) LayoutIdx {
        const expr = self.store.getExpr(expr_id);
        return switch (expr) {
            .block => |b| b.result_layout,
            .if_then_else => |ite| ite.result_layout,
            .match_expr => |w| w.result_layout,
            .dbg => |d| d.result_layout,
            .expect => |e| e.result_layout,
            .proc_call => |c| c.ret_layout,
            .low_level => |ll| ll.ret_layout,
            .early_return => |er| er.ret_layout,
            .lookup => |l| l.layout_idx,
            .cell_load => |l| l.layout_idx,
            .struct_ => |s| s.struct_layout,
            .tag => |t| t.union_layout,
            .zero_arg_tag => |z| z.union_layout,
            .struct_access => |sa| sa.field_layout,
            .nominal => |n| n.nominal_layout,
            .discriminant_switch => |ds| ds.result_layout,
            .f64_literal => .f64,
            .f32_literal => .f32,
            .bool_literal => .bool,
            .dec_literal => .dec,
            .str_literal => .str,
            .i64_literal => |i| i.layout_idx,
            .i128_literal => |i| i.layout_idx,
            .list => |l| l.list_layout,
            .empty_list => |l| l.list_layout,
            .hosted_call => |hc| hc.ret_layout,
            .tag_payload_access => |tpa| tpa.payload_layout,
            .for_loop, .while_loop, .incref, .decref, .free, .break_expr => .zst,
            .crash => |c| c.ret_layout,
            .runtime_error => |re| re.ret_layout,
            .str_concat,
            .int_to_str,
            .float_to_str,
            .dec_to_str,
            .str_escape_and_quote,
            => .str,
        };
    }

    fn shouldMaterializeBorrowedLoopSource(self: *const RcInsertPass, expr_id: LirExprId) bool {
        if (expr_id.isNone()) return false;
        if (self.store.getExpr(expr_id) == .lookup) return false;
        return self.layoutNeedsRc(self.exprResultLayout(expr_id));
    }

    fn hasReusableBorrowOwner(self: *const RcInsertPass, expr_id: LirExprId) bool {
        if (expr_id.isNone()) return false;

        const layout_idx = self.exprResultLayout(expr_id);
        if (!self.layoutNeedsRc(layout_idx)) return false;

        return switch (self.store.getExpr(expr_id)) {
            .lookup => |lookup| !lookup.symbol.isNone() and self.store.getSymbolDef(lookup.symbol) == null,
            .nominal => |nominal| self.hasReusableBorrowOwner(nominal.backing_expr),
            .struct_access => |access| self.hasReusableBorrowOwner(access.struct_expr),
            .tag_payload_access => |access| self.hasReusableBorrowOwner(access.value),
            .dbg => |dbg_expr| self.hasReusableBorrowOwner(dbg_expr.expr),
            else => false,
        };
    }

    fn shouldMaterializeBorrowedUse(self: *const RcInsertPass, expr_id: LirExprId) bool {
        if (expr_id.isNone()) return false;

        const layout_idx = self.exprResultLayout(expr_id);
        if (!self.layoutNeedsRc(layout_idx)) return false;

        return !self.hasReusableBorrowOwner(expr_id);
    }

    fn materializeBorrowedUse(
        self: *RcInsertPass,
        expr_id: LirExprId,
        region: Region,
        prelude: *std.ArrayList(LirStmt),
    ) Allocator.Error!LirExprId {
        if (!self.shouldMaterializeBorrowedUse(expr_id)) return expr_id;

        const layout_idx = self.exprResultLayout(expr_id);
        const temp = try self.bindExprToFreshLookup(prelude, expr_id, layout_idx, region);
        return temp.lookup;
    }

    fn shouldMaterializeRcCellLoad(self: *const RcInsertPass, expr_id: LirExprId) bool {
        if (expr_id.isNone()) return false;
        if (@intFromEnum(expr_id) >= self.store.exprs.items.len) return false;
        return switch (self.store.getExpr(expr_id)) {
            .cell_load => |load| self.layoutNeedsRc(load.layout_idx),
            else => false,
        };
    }

    fn materializeRcCellLoadOperand(
        self: *RcInsertPass,
        expr_id: LirExprId,
        region: Region,
        prelude: *std.ArrayList(LirStmt),
    ) Allocator.Error!LirExprId {
        if (!self.shouldMaterializeRcCellLoad(expr_id)) return expr_id;

        const layout_idx = self.exprResultLayout(expr_id);
        const temp = try self.freshResultPattern(layout_idx, region);
        try prelude.append(self.allocator, .{ .decl = .{
            .pattern = temp.pattern,
            .expr = expr_id,
            .semantics = .retained,
        } });
        return self.store.addExpr(.{ .lookup = .{
            .symbol = temp.symbol,
            .layout_idx = layout_idx,
        } }, region);
    }

    fn wrapRetainedCellLoad(
        self: *RcInsertPass,
        expr_id: LirExprId,
        layout_idx: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        var prelude = std.ArrayList(LirStmt).empty;
        defer prelude.deinit(self.allocator);

        const retained_lookup = try self.materializeRcCellLoadOperand(expr_id, region, &prelude);
        return self.wrapPreludeAroundExpr(retained_lookup, layout_idx, region, prelude.items);
    }

    fn ownerKeyForAliasedExpr(self: *RcInsertPass, expr_id: LirExprId) ?u64 {
        if (expr_id.isNone()) return null;

        return switch (self.store.getExpr(expr_id)) {
            .lookup => |lookup| blk: {
                if (lookup.symbol.isNone()) break :blk null;
                const key = self.lookupKey(expr_id, lookup.symbol);
                break :blk self.consumedOwnerKey(key);
            },
            .block => |block| self.ownerKeyForAliasedExpr(block.final_expr),
            .dbg => |dbg_expr| self.ownerKeyForAliasedExpr(dbg_expr.expr),
            .nominal => |nominal| self.ownerKeyForAliasedExpr(nominal.backing_expr),
            else => null,
        };
    }

    fn retainOperandIfNeededForLaterUse(
        self: *RcInsertPass,
        operand_id: LirExprId,
        later_uses: *const std.AutoHashMap(u64, u32),
        region: Region,
        prelude: *std.ArrayList(LirStmt),
    ) Allocator.Error!LirExprId {
        const owner_key = self.ownerKeyForAliasedExpr(operand_id) orelse return operand_id;
        if (self.ownerUseCountFromMap(later_uses, owner_key) == 0) return operand_id;

        const layout_idx = self.exprResultLayout(operand_id);
        if (!self.layoutNeedsRc(layout_idx)) return operand_id;

        const temp = try self.bindExprToFreshLookupWithSemantics(prelude, operand_id, layout_idx, .retained, region);
        try self.emitIncrefInto(temp.symbol, layout_idx, 1, region, prelude);
        const gop = try self.retained_owner_use_debits.getOrPut(owner_key);
        if (gop.found_existing) {
            gop.value_ptr.* += 1;
        } else {
            gop.value_ptr.* = 1;
        }
        return temp.lookup;
    }

    fn retainConsumedOperandsForLaterUse(
        self: *RcInsertPass,
        expr_id: LirExprId,
        later_uses: *const std.AutoHashMap(u64, u32),
        region: Region,
    ) Allocator.Error!LirExprId {
        if (expr_id.isNone()) return expr_id;

        var prelude = std.ArrayList(LirStmt).empty;
        defer prelude.deinit(self.allocator);

        const rewritten = switch (self.store.getExpr(expr_id)) {
            .proc_call => |call| blk: {
                const args = self.store.getExprSpan(call.args);
                var new_args = std.ArrayList(LirExprId).empty;
                defer new_args.deinit(self.allocator);

                var changed = false;
                for (args) |arg_id| {
                    const retained_arg = try self.retainOperandIfNeededForLaterUse(
                        arg_id,
                        later_uses,
                        region,
                        &prelude,
                    );
                    if (retained_arg != arg_id) changed = true;
                    try new_args.append(self.allocator, retained_arg);
                }

                if (!changed) break :blk expr_id;
                break :blk try self.rebuildProcCall(call, try self.store.addExprSpan(new_args.items), region);
            },
            .low_level => |ll| blk: {
                const args = self.store.getExprSpan(ll.args);
                const arg_ownership = ll.op.getArgOwnership();

                var new_args = std.ArrayList(LirExprId).empty;
                defer new_args.deinit(self.allocator);

                var changed = false;
                for (args, 0..) |arg_id, i| {
                    const new_arg = switch (arg_ownership[i]) {
                        .consume => try self.retainOperandIfNeededForLaterUse(
                            arg_id,
                            later_uses,
                            region,
                            &prelude,
                        ),
                        .borrow => arg_id,
                    };
                    if (new_arg != arg_id) changed = true;
                    try new_args.append(self.allocator, new_arg);
                }

                if (!changed) break :blk expr_id;
                break :blk try self.store.addExpr(.{ .low_level = .{
                    .op = ll.op,
                    .args = try self.store.addExprSpan(new_args.items),
                    .ret_layout = ll.ret_layout,
                    .callable_proc = ll.callable_proc,
                } }, region);
            },
            .hosted_call => |_| expr_id,
            else => expr_id,
        };

        return self.wrapPreludeAroundExpr(rewritten, self.exprResultLayout(expr_id), region, prelude.items);
    }

    fn wrapPreludeAroundExpr(
        self: *RcInsertPass,
        expr_id: LirExprId,
        result_layout: LayoutIdx,
        region: Region,
        prelude: []const LirStmt,
    ) Allocator.Error!LirExprId {
        if (prelude.len == 0) return expr_id;
        return self.store.addExpr(.{ .block = .{
            .stmts = try self.store.addStmts(prelude),
            .final_expr = expr_id,
            .result_layout = result_layout,
        } }, region);
    }

    fn materializeRcCellLoadSpan(
        self: *RcInsertPass,
        span: LirExprSpan,
        region: Region,
        prelude: *std.ArrayList(LirStmt),
    ) Allocator.Error!struct { span: LirExprSpan, changed: bool } {
        const exprs = self.store.getExprSpan(span);
        if (exprs.len == 0) return .{ .span = span, .changed = false };

        var changed = false;
        var new_exprs = std.ArrayList(LirExprId).empty;
        defer new_exprs.deinit(self.allocator);

        for (exprs) |expr_id| {
            const new_expr = try self.materializeRcCellLoadOperand(expr_id, region, prelude);
            changed = changed or new_expr != expr_id;
            try new_exprs.append(self.allocator, new_expr);
        }

        if (!changed) return .{ .span = span, .changed = false };
        return .{ .span = try self.store.addExprSpan(new_exprs.items), .changed = true };
    }

    fn materializeRcCellLoadOperands(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!LirExprId {
        if (expr_id.isNone()) return expr_id;
        if (@intFromEnum(expr_id) >= self.store.exprs.items.len) return expr_id;

        const region = self.store.getExprRegion(expr_id);
        return switch (self.store.getExpr(expr_id)) {
            .block => |block| {
                const stmts = try self.allocator.dupe(LirStmt, self.store.getStmts(block.stmts));
                defer self.allocator.free(stmts);

                var changed = false;
                var new_stmts = std.ArrayList(LirStmt).empty;
                defer new_stmts.deinit(self.allocator);

                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl => |binding| {
                            const new_expr = try self.materializeRcCellLoadOperands(binding.expr);
                            changed = changed or new_expr != binding.expr;
                            try new_stmts.append(self.allocator, .{ .decl = .{
                                .pattern = binding.pattern,
                                .expr = new_expr,
                                .semantics = binding.semantics,
                            } });
                        },
                        .mutate => |binding| {
                            const new_expr = try self.materializeRcCellLoadOperands(binding.expr);
                            changed = changed or new_expr != binding.expr;
                            try new_stmts.append(self.allocator, .{ .mutate = .{
                                .pattern = binding.pattern,
                                .expr = new_expr,
                                .semantics = binding.semantics,
                            } });
                        },
                        .cell_init => |binding| {
                            const new_expr = try self.materializeRcCellLoadOperands(binding.expr);
                            if (builtin.mode == .Debug and !new_expr.isNone() and @intFromEnum(new_expr) >= self.store.exprs.items.len) {
                                std.debug.panic(
                                    "RC invariant violated: invalid expr id {d} returned for cell_init child tag {s} (expr count {d})",
                                    .{ @intFromEnum(new_expr), @tagName(self.store.getExpr(binding.expr)), self.store.exprs.items.len },
                                );
                            }
                            changed = changed or new_expr != binding.expr;
                            if (self.shouldMaterializeRcCellLoad(new_expr)) {
                                const temp = try self.freshResultPattern(binding.layout_idx, region);
                                const temp_lookup = try self.store.addExpr(.{ .lookup = .{
                                    .symbol = temp.symbol,
                                    .layout_idx = binding.layout_idx,
                                } }, region);
                                try new_stmts.append(self.allocator, .{ .decl = .{
                                    .pattern = temp.pattern,
                                    .expr = new_expr,
                                    .semantics = .retained,
                                } });
                                try new_stmts.append(self.allocator, .{ .cell_init = .{
                                    .cell = binding.cell,
                                    .layout_idx = binding.layout_idx,
                                    .expr = temp_lookup,
                                } });
                                changed = true;
                            } else {
                                try new_stmts.append(self.allocator, .{ .cell_init = .{
                                    .cell = binding.cell,
                                    .layout_idx = binding.layout_idx,
                                    .expr = new_expr,
                                } });
                            }
                        },
                        .cell_store => |binding| {
                            const new_expr = try self.materializeRcCellLoadOperands(binding.expr);
                            if (builtin.mode == .Debug and !new_expr.isNone() and @intFromEnum(new_expr) >= self.store.exprs.items.len) {
                                std.debug.panic(
                                    "RC invariant violated: invalid expr id {d} returned for cell_store child tag {s} (expr count {d})",
                                    .{ @intFromEnum(new_expr), @tagName(self.store.getExpr(binding.expr)), self.store.exprs.items.len },
                                );
                            }
                            changed = changed or new_expr != binding.expr;
                            if (self.shouldMaterializeRcCellLoad(new_expr)) {
                                const temp = try self.freshResultPattern(binding.layout_idx, region);
                                const temp_lookup = try self.store.addExpr(.{ .lookup = .{
                                    .symbol = temp.symbol,
                                    .layout_idx = binding.layout_idx,
                                } }, region);
                                try new_stmts.append(self.allocator, .{ .decl = .{
                                    .pattern = temp.pattern,
                                    .expr = new_expr,
                                    .semantics = .retained,
                                } });
                                try new_stmts.append(self.allocator, .{ .cell_store = .{
                                    .cell = binding.cell,
                                    .layout_idx = binding.layout_idx,
                                    .expr = temp_lookup,
                                } });
                                changed = true;
                            } else {
                                try new_stmts.append(self.allocator, .{ .cell_store = .{
                                    .cell = binding.cell,
                                    .layout_idx = binding.layout_idx,
                                    .expr = new_expr,
                                } });
                            }
                        },
                        .cell_drop => |drop| try new_stmts.append(self.allocator, .{ .cell_drop = drop }),
                    }
                }

                const new_final = try self.materializeRcCellLoadOperands(block.final_expr);
                changed = changed or new_final != block.final_expr;
                if (!changed) return expr_id;
                return self.store.addExpr(.{ .block = .{
                    .stmts = try self.store.addStmts(new_stmts.items),
                    .final_expr = new_final,
                    .result_layout = block.result_layout,
                } }, region);
            },
            .if_then_else => |ite| {
                const branches = try self.allocator.dupe(LirIfBranch, self.store.getIfBranches(ite.branches));
                defer self.allocator.free(branches);

                var changed = false;
                var new_branches = std.ArrayList(LIR.LirIfBranch).empty;
                defer new_branches.deinit(self.allocator);
                for (branches) |branch| {
                    const new_cond_raw = try self.materializeRcCellLoadOperands(branch.cond);
                    var cond_prelude = std.ArrayList(LirStmt).empty;
                    defer cond_prelude.deinit(self.allocator);
                    const new_cond = try self.materializeRcCellLoadOperand(new_cond_raw, region, &cond_prelude);

                    const new_body = try self.materializeRcCellLoadOperands(branch.body);
                    changed = changed or new_cond != branch.cond or new_body != branch.body or cond_prelude.items.len > 0;
                    try new_branches.append(self.allocator, .{
                        .cond = try self.wrapPreludeAroundExpr(new_cond, .bool, region, cond_prelude.items),
                        .body = new_body,
                    });
                }
                const new_else = try self.materializeRcCellLoadOperands(ite.final_else);
                changed = changed or new_else != ite.final_else;
                if (!changed) return expr_id;
                return self.store.addExpr(.{ .if_then_else = .{
                    .branches = try self.store.addIfBranches(new_branches.items),
                    .final_else = new_else,
                    .result_layout = ite.result_layout,
                } }, region);
            },
            .match_expr => |match_expr| {
                const branches = try self.allocator.dupe(LirMatchBranch, self.store.getMatchBranches(match_expr.branches));
                defer self.allocator.free(branches);

                var changed = false;
                const new_value_raw = try self.materializeRcCellLoadOperands(match_expr.value);
                var value_prelude = std.ArrayList(LirStmt).empty;
                defer value_prelude.deinit(self.allocator);
                const new_value = try self.materializeRcCellLoadOperand(new_value_raw, region, &value_prelude);
                changed = changed or new_value != match_expr.value or value_prelude.items.len > 0;

                var new_branches = std.ArrayList(LIR.LirMatchBranch).empty;
                defer new_branches.deinit(self.allocator);
                for (branches) |branch| {
                    const new_guard_raw = try self.materializeRcCellLoadOperands(branch.guard);
                    var guard_prelude = std.ArrayList(LirStmt).empty;
                    defer guard_prelude.deinit(self.allocator);
                    const new_guard = try self.materializeRcCellLoadOperand(new_guard_raw, region, &guard_prelude);

                    const new_body = try self.materializeRcCellLoadOperands(branch.body);
                    changed = changed or new_guard != branch.guard or new_body != branch.body or guard_prelude.items.len > 0;
                    try new_branches.append(self.allocator, .{
                        .pattern = branch.pattern,
                        .guard = try self.wrapPreludeAroundExpr(new_guard, .bool, region, guard_prelude.items),
                        .body = new_body,
                    });
                }

                const rebuilt = try self.store.addExpr(.{ .match_expr = .{
                    .value = new_value,
                    .value_layout = match_expr.value_layout,
                    .branches = try self.store.addMatchBranches(new_branches.items),
                    .result_layout = match_expr.result_layout,
                } }, region);
                if (!changed) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, match_expr.result_layout, region, value_prelude.items);
            },
            .for_loop => |fl| {
                const new_list = try self.materializeRcCellLoadOperands(fl.list_expr);
                const new_body = try self.materializeRcCellLoadOperands(fl.body);
                if (new_list == fl.list_expr and new_body == fl.body) return expr_id;
                return self.store.addExpr(.{ .for_loop = .{
                    .list_expr = new_list,
                    .elem_layout = fl.elem_layout,
                    .elem_pattern = fl.elem_pattern,
                    .body = new_body,
                } }, region);
            },
            .while_loop => |wl| {
                const new_cond_raw = try self.materializeRcCellLoadOperands(wl.cond);
                var cond_prelude = std.ArrayList(LirStmt).empty;
                defer cond_prelude.deinit(self.allocator);
                const new_cond = try self.materializeRcCellLoadOperand(new_cond_raw, region, &cond_prelude);
                const new_body = try self.materializeRcCellLoadOperands(wl.body);
                const rebuilt = try self.store.addExpr(.{ .while_loop = .{
                    .cond = new_cond,
                    .body = new_body,
                } }, region);
                if (new_cond == wl.cond and new_body == wl.body and cond_prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, .zst, region, cond_prelude.items);
            },
            .proc_call => |call| {
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);

                const args_res = try self.materializeRcCellLoadSpan(call.args, region, &prelude);
                const rebuilt = try self.rebuildProcCall(call, args_res.span, region);
                if (!args_res.changed and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, call.ret_layout, region, prelude.items);
            },
            .list => |list_expr| {
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const elems_res = try self.materializeRcCellLoadSpan(list_expr.elems, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .list = .{
                    .list_layout = list_expr.list_layout,
                    .elem_layout = list_expr.elem_layout,
                    .elems = elems_res.span,
                } }, region);
                if (!elems_res.changed and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, list_expr.list_layout, region, prelude.items);
            },
            .struct_ => |struct_expr| {
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const fields_res = try self.materializeRcCellLoadSpan(struct_expr.fields, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .struct_ = .{
                    .struct_layout = struct_expr.struct_layout,
                    .fields = fields_res.span,
                } }, region);
                if (!fields_res.changed and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, struct_expr.struct_layout, region, prelude.items);
            },
            .tag => |tag_expr| {
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const args_res = try self.materializeRcCellLoadSpan(tag_expr.args, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .tag = .{
                    .discriminant = tag_expr.discriminant,
                    .union_layout = tag_expr.union_layout,
                    .args = args_res.span,
                } }, region);
                if (!args_res.changed and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, tag_expr.union_layout, region, prelude.items);
            },
            .struct_access => |sa| {
                const new_struct_raw = try self.materializeRcCellLoadOperands(sa.struct_expr);
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const new_struct = try self.materializeRcCellLoadOperand(new_struct_raw, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .struct_access = .{
                    .struct_expr = new_struct,
                    .struct_layout = sa.struct_layout,
                    .field_idx = sa.field_idx,
                    .field_layout = sa.field_layout,
                } }, region);
                if (new_struct == sa.struct_expr and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, sa.field_layout, region, prelude.items);
            },
            .nominal => |n| {
                const new_backing_raw = try self.materializeRcCellLoadOperands(n.backing_expr);
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const new_backing = try self.materializeRcCellLoadOperand(new_backing_raw, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .nominal = .{
                    .backing_expr = new_backing,
                    .nominal_layout = n.nominal_layout,
                } }, region);
                if (new_backing == n.backing_expr and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, n.nominal_layout, region, prelude.items);
            },
            .early_return => |ret| {
                const new_expr_raw = try self.materializeRcCellLoadOperands(ret.expr);
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const new_expr = try self.materializeRcCellLoadOperand(new_expr_raw, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .early_return = .{
                    .expr = new_expr,
                    .ret_layout = ret.ret_layout,
                } }, region);
                if (new_expr == ret.expr and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, ret.ret_layout, region, prelude.items);
            },
            .dbg => |d| {
                const new_expr_raw = try self.materializeRcCellLoadOperands(d.expr);
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const new_expr = try self.materializeRcCellLoadOperand(new_expr_raw, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .dbg = .{
                    .expr = new_expr,
                    .result_layout = d.result_layout,
                } }, region);
                if (new_expr == d.expr and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, d.result_layout, region, prelude.items);
            },
            .expect => |e| {
                const new_cond_raw = try self.materializeRcCellLoadOperands(e.cond);
                var cond_prelude = std.ArrayList(LirStmt).empty;
                defer cond_prelude.deinit(self.allocator);
                const new_cond = try self.materializeRcCellLoadOperand(new_cond_raw, region, &cond_prelude);
                const new_body = try self.materializeRcCellLoadOperands(e.body);
                const rebuilt = try self.store.addExpr(.{ .expect = .{
                    .cond = new_cond,
                    .body = new_body,
                    .result_layout = e.result_layout,
                } }, region);
                if (new_cond == e.cond and new_body == e.body and cond_prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, e.result_layout, region, cond_prelude.items);
            },
            .low_level => |ll| {
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const args_res = try self.materializeRcCellLoadSpan(ll.args, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .low_level = .{
                    .op = ll.op,
                    .args = args_res.span,
                    .ret_layout = ll.ret_layout,
                    .callable_proc = ll.callable_proc,
                } }, region);
                if (!args_res.changed and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, ll.ret_layout, region, prelude.items);
            },
            .hosted_call => |hc| {
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const args_res = try self.materializeRcCellLoadSpan(hc.args, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .hosted_call = .{
                    .index = hc.index,
                    .args = args_res.span,
                    .ret_layout = hc.ret_layout,
                } }, region);
                if (!args_res.changed and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, hc.ret_layout, region, prelude.items);
            },
            .str_concat => |parts| {
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const parts_res = try self.materializeRcCellLoadSpan(parts, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .str_concat = parts_res.span }, region);
                if (!parts_res.changed and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, .str, region, prelude.items);
            },
            .int_to_str => |its| {
                const new_value_raw = try self.materializeRcCellLoadOperands(its.value);
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const new_value = try self.materializeRcCellLoadOperand(new_value_raw, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .int_to_str = .{
                    .value = new_value,
                    .int_precision = its.int_precision,
                } }, region);
                if (new_value == its.value and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, .str, region, prelude.items);
            },
            .float_to_str => |fts| {
                const new_value_raw = try self.materializeRcCellLoadOperands(fts.value);
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const new_value = try self.materializeRcCellLoadOperand(new_value_raw, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .float_to_str = .{
                    .value = new_value,
                    .float_precision = fts.float_precision,
                } }, region);
                if (new_value == fts.value and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, .str, region, prelude.items);
            },
            .dec_to_str => |value| {
                const new_value_raw = try self.materializeRcCellLoadOperands(value);
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const new_value = try self.materializeRcCellLoadOperand(new_value_raw, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .dec_to_str = new_value }, region);
                if (new_value == value and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, .str, region, prelude.items);
            },
            .str_escape_and_quote => |value| {
                const new_value_raw = try self.materializeRcCellLoadOperands(value);
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const new_value = try self.materializeRcCellLoadOperand(new_value_raw, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .str_escape_and_quote = new_value }, region);
                if (new_value == value and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, .str, region, prelude.items);
            },
            .discriminant_switch => |ds| {
                const new_value_raw = try self.materializeRcCellLoadOperands(ds.value);
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const new_value = try self.materializeRcCellLoadOperand(new_value_raw, region, &prelude);
                const branches_res = try self.materializeRcCellLoadSpan(ds.branches, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .discriminant_switch = .{
                    .value = new_value,
                    .union_layout = ds.union_layout,
                    .branches = branches_res.span,
                    .result_layout = ds.result_layout,
                } }, region);
                if (new_value == ds.value and !branches_res.changed and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, ds.result_layout, region, prelude.items);
            },
            .tag_payload_access => |tpa| {
                const new_value_raw = try self.materializeRcCellLoadOperands(tpa.value);
                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);
                const new_value = try self.materializeRcCellLoadOperand(new_value_raw, region, &prelude);
                const rebuilt = try self.store.addExpr(.{ .tag_payload_access = .{
                    .value = new_value,
                    .union_layout = tpa.union_layout,
                    .payload_layout = tpa.payload_layout,
                } }, region);
                if (new_value == tpa.value and prelude.items.len == 0) return expr_id;
                return self.wrapPreludeAroundExpr(rebuilt, tpa.payload_layout, region, prelude.items);
            },
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
            => expr_id,
            .cell_load => |load| {
                if (!self.layoutNeedsRc(load.layout_idx)) return expr_id;
                return self.wrapRetainedCellLoad(expr_id, load.layout_idx, region);
            },
        };
    }

    fn normalizeBorrowedLoopSources(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!LirExprId {
        if (expr_id.isNone()) return expr_id;

        const region = self.store.getExprRegion(expr_id);
        return switch (self.store.getExpr(expr_id)) {
            .block => |block| {
                const stmts = try self.allocator.dupe(LirStmt, self.store.getStmts(block.stmts));
                defer self.allocator.free(stmts);

                var changed = false;
                var new_stmts = std.ArrayList(LirStmt).empty;
                defer new_stmts.deinit(self.allocator);

                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl => |binding| {
                            const new_expr = try self.normalizeBorrowedLoopSources(binding.expr);
                            changed = changed or new_expr != binding.expr;
                            try new_stmts.append(self.allocator, .{ .decl = .{ .pattern = binding.pattern, .expr = new_expr, .semantics = binding.semantics } });
                        },
                        .mutate => |binding| {
                            const new_expr = try self.normalizeBorrowedLoopSources(binding.expr);
                            changed = changed or new_expr != binding.expr;
                            try new_stmts.append(self.allocator, .{ .mutate = .{ .pattern = binding.pattern, .expr = new_expr, .semantics = binding.semantics } });
                        },
                        .cell_init => |binding| {
                            const new_expr = try self.normalizeBorrowedLoopSources(binding.expr);
                            changed = changed or new_expr != binding.expr;
                            try new_stmts.append(self.allocator, .{ .cell_init = .{ .cell = binding.cell, .layout_idx = binding.layout_idx, .expr = new_expr } });
                        },
                        .cell_store => |binding| {
                            const new_expr = try self.normalizeBorrowedLoopSources(binding.expr);
                            changed = changed or new_expr != binding.expr;
                            try new_stmts.append(self.allocator, .{ .cell_store = .{ .cell = binding.cell, .layout_idx = binding.layout_idx, .expr = new_expr } });
                        },
                        .cell_drop => |drop| try new_stmts.append(self.allocator, .{ .cell_drop = drop }),
                    }
                }

                const new_final = try self.normalizeBorrowedLoopSources(block.final_expr);
                changed = changed or new_final != block.final_expr;
                if (!changed) return expr_id;

                return self.store.addExpr(.{ .block = .{
                    .stmts = try self.store.addStmts(new_stmts.items),
                    .final_expr = new_final,
                    .result_layout = block.result_layout,
                } }, region);
            },
            .cell_load => expr_id,
            .if_then_else => |ite| {
                const branches = try self.allocator.dupe(LirIfBranch, self.store.getIfBranches(ite.branches));
                defer self.allocator.free(branches);

                var changed = false;
                var new_branches = std.ArrayList(LIR.LirIfBranch).empty;
                defer new_branches.deinit(self.allocator);
                for (branches) |branch| {
                    const new_cond = try self.normalizeBorrowedLoopSources(branch.cond);
                    const new_body = try self.normalizeBorrowedLoopSources(branch.body);
                    changed = changed or new_cond != branch.cond or new_body != branch.body;
                    try new_branches.append(self.allocator, .{
                        .cond = new_cond,
                        .body = new_body,
                    });
                }
                const new_else = try self.normalizeBorrowedLoopSources(ite.final_else);
                changed = changed or new_else != ite.final_else;
                if (!changed) return expr_id;
                return self.store.addExpr(.{ .if_then_else = .{
                    .branches = try self.store.addIfBranches(new_branches.items),
                    .final_else = new_else,
                    .result_layout = ite.result_layout,
                } }, region);
            },
            .match_expr => |match_expr| {
                const branches = try self.allocator.dupe(LirMatchBranch, self.store.getMatchBranches(match_expr.branches));
                defer self.allocator.free(branches);

                var changed = false;
                const new_value = try self.normalizeBorrowedLoopSources(match_expr.value);
                changed = changed or new_value != match_expr.value;
                var new_branches = std.ArrayList(LIR.LirMatchBranch).empty;
                defer new_branches.deinit(self.allocator);
                for (branches) |branch| {
                    const new_guard = try self.normalizeBorrowedLoopSources(branch.guard);
                    const new_body = try self.normalizeBorrowedLoopSources(branch.body);
                    changed = changed or new_guard != branch.guard or new_body != branch.body;
                    try new_branches.append(self.allocator, .{
                        .pattern = branch.pattern,
                        .guard = new_guard,
                        .body = new_body,
                    });
                }
                if (!changed) return expr_id;
                return self.store.addExpr(.{ .match_expr = .{
                    .value = new_value,
                    .value_layout = match_expr.value_layout,
                    .branches = try self.store.addMatchBranches(new_branches.items),
                    .result_layout = match_expr.result_layout,
                } }, region);
            },
            .for_loop => |fl| {
                const new_list = try self.normalizeBorrowedLoopSources(fl.list_expr);
                const new_body = try self.normalizeBorrowedLoopSources(fl.body);

                if (self.shouldMaterializeBorrowedLoopSource(new_list)) {
                    const list_layout = self.exprResultLayout(new_list);
                    const list_bind = try self.freshResultPattern(list_layout, region);
                    const list_lookup = try self.store.addExpr(.{ .lookup = .{
                        .symbol = list_bind.symbol,
                        .layout_idx = list_layout,
                    } }, region);
                    const loop_expr = try self.store.addExpr(.{ .for_loop = .{
                        .list_expr = list_lookup,
                        .elem_layout = fl.elem_layout,
                        .elem_pattern = fl.elem_pattern,
                        .body = new_body,
                    } }, region);
                    const stmts = try self.store.addStmts(&.{
                        .{ .decl = .{
                            .pattern = list_bind.pattern,
                            .expr = new_list,
                        } },
                    });
                    return self.store.addExpr(.{ .block = .{
                        .stmts = stmts,
                        .final_expr = loop_expr,
                        .result_layout = .zst,
                    } }, region);
                }

                if (new_list == fl.list_expr and new_body == fl.body) return expr_id;
                return self.store.addExpr(.{ .for_loop = .{
                    .list_expr = new_list,
                    .elem_layout = fl.elem_layout,
                    .elem_pattern = fl.elem_pattern,
                    .body = new_body,
                } }, region);
            },
            .while_loop => |wl| {
                const new_cond = try self.normalizeBorrowedLoopSources(wl.cond);
                const new_body = try self.normalizeBorrowedLoopSources(wl.body);
                if (new_cond == wl.cond and new_body == wl.body) return expr_id;
                return self.store.addExpr(.{ .while_loop = .{
                    .cond = new_cond,
                    .body = new_body,
                } }, region);
            },
            .proc_call => |call| {
                const args_res = try self.normalizeBorrowedLoopSourceSpan(call.args);
                if (!args_res.changed) return expr_id;
                return self.rebuildProcCall(call, args_res.span, region);
            },
            .list => |list_expr| {
                const elems_res = try self.normalizeBorrowedLoopSourceSpan(list_expr.elems);
                if (!elems_res.changed) return expr_id;
                return self.store.addExpr(.{ .list = .{
                    .list_layout = list_expr.list_layout,
                    .elem_layout = list_expr.elem_layout,
                    .elems = elems_res.span,
                } }, region);
            },
            .struct_ => |struct_expr| {
                const fields_res = try self.normalizeBorrowedLoopSourceSpan(struct_expr.fields);
                if (!fields_res.changed) return expr_id;
                return self.store.addExpr(.{ .struct_ = .{
                    .struct_layout = struct_expr.struct_layout,
                    .fields = fields_res.span,
                } }, region);
            },
            .tag => |tag_expr| {
                const args_res = try self.normalizeBorrowedLoopSourceSpan(tag_expr.args);
                if (!args_res.changed) return expr_id;
                return self.store.addExpr(.{ .tag = .{
                    .discriminant = tag_expr.discriminant,
                    .union_layout = tag_expr.union_layout,
                    .args = args_res.span,
                } }, region);
            },
            .struct_access => |sa| {
                const new_struct = try self.normalizeBorrowedLoopSources(sa.struct_expr);
                if (new_struct == sa.struct_expr) return expr_id;
                return self.store.addExpr(.{ .struct_access = .{
                    .struct_expr = new_struct,
                    .struct_layout = sa.struct_layout,
                    .field_idx = sa.field_idx,
                    .field_layout = sa.field_layout,
                } }, region);
            },
            .nominal => |n| {
                const new_backing = try self.normalizeBorrowedLoopSources(n.backing_expr);
                if (new_backing == n.backing_expr) return expr_id;
                return self.store.addExpr(.{ .nominal = .{
                    .backing_expr = new_backing,
                    .nominal_layout = n.nominal_layout,
                } }, region);
            },
            .early_return => |ret| {
                const new_expr = try self.normalizeBorrowedLoopSources(ret.expr);
                if (new_expr == ret.expr) return expr_id;
                return self.store.addExpr(.{ .early_return = .{
                    .expr = new_expr,
                    .ret_layout = ret.ret_layout,
                } }, region);
            },
            .dbg => |d| {
                const new_expr = try self.normalizeBorrowedLoopSources(d.expr);
                if (new_expr == d.expr) return expr_id;
                return self.store.addExpr(.{ .dbg = .{
                    .expr = new_expr,
                    .result_layout = d.result_layout,
                } }, region);
            },
            .expect => |e| {
                const new_cond = try self.normalizeBorrowedLoopSources(e.cond);
                const new_body = try self.normalizeBorrowedLoopSources(e.body);
                if (new_cond == e.cond and new_body == e.body) return expr_id;
                return self.store.addExpr(.{ .expect = .{
                    .cond = new_cond,
                    .body = new_body,
                    .result_layout = e.result_layout,
                } }, region);
            },
            .low_level => |ll| {
                const args_res = try self.normalizeBorrowedLoopSourceSpan(ll.args);
                const args = self.store.getExprSpan(args_res.span);
                const arg_ownership = ll.op.getArgOwnership();

                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);

                var changed = args_res.changed;
                var new_args = std.ArrayList(LirExprId).empty;
                defer new_args.deinit(self.allocator);

                for (args, 0..) |arg_id, i| {
                    var new_arg = arg_id;
                    if (arg_ownership[i] == .borrow) {
                        new_arg = try self.materializeBorrowedUse(arg_id, region, &prelude);
                    }
                    changed = changed or new_arg != arg_id;
                    try new_args.append(self.allocator, new_arg);
                }

                if (!changed and prelude.items.len == 0) return expr_id;

                const rebuilt = try self.store.addExpr(.{ .low_level = .{
                    .op = ll.op,
                    .args = try self.store.addExprSpan(new_args.items),
                    .ret_layout = ll.ret_layout,
                    .callable_proc = ll.callable_proc,
                } }, region);
                return self.wrapPreludeAroundExpr(rebuilt, ll.ret_layout, region, prelude.items);
            },
            .hosted_call => |hc| {
                const args_res = try self.normalizeBorrowedLoopSourceSpan(hc.args);
                const args = self.store.getExprSpan(args_res.span);

                var prelude = std.ArrayList(LirStmt).empty;
                defer prelude.deinit(self.allocator);

                var changed = args_res.changed;
                var new_args = std.ArrayList(LirExprId).empty;
                defer new_args.deinit(self.allocator);

                for (args) |arg_id| {
                    const new_arg = try self.materializeBorrowedUse(arg_id, region, &prelude);
                    changed = changed or new_arg != arg_id;
                    try new_args.append(self.allocator, new_arg);
                }

                if (!changed and prelude.items.len == 0) return expr_id;

                const rebuilt = try self.store.addExpr(.{ .hosted_call = .{
                    .index = hc.index,
                    .args = try self.store.addExprSpan(new_args.items),
                    .ret_layout = hc.ret_layout,
                } }, region);
                return self.wrapPreludeAroundExpr(rebuilt, hc.ret_layout, region, prelude.items);
            },
            .str_concat => |parts| {
                const parts_res = try self.normalizeBorrowedLoopSourceSpan(parts);
                if (!parts_res.changed) return expr_id;
                return self.store.addExpr(.{ .str_concat = parts_res.span }, region);
            },
            .int_to_str => |its| {
                const new_value = try self.normalizeBorrowedLoopSources(its.value);
                if (new_value == its.value) return expr_id;
                return self.store.addExpr(.{ .int_to_str = .{
                    .value = new_value,
                    .int_precision = its.int_precision,
                } }, region);
            },
            .float_to_str => |fts| {
                const new_value = try self.normalizeBorrowedLoopSources(fts.value);
                if (new_value == fts.value) return expr_id;
                return self.store.addExpr(.{ .float_to_str = .{
                    .value = new_value,
                    .float_precision = fts.float_precision,
                } }, region);
            },
            .dec_to_str => |value| {
                const new_value = try self.normalizeBorrowedLoopSources(value);
                if (new_value == value) return expr_id;
                return self.store.addExpr(.{ .dec_to_str = new_value }, region);
            },
            .str_escape_and_quote => |value| {
                const new_value = try self.normalizeBorrowedLoopSources(value);
                if (new_value == value) return expr_id;
                return self.store.addExpr(.{ .str_escape_and_quote = new_value }, region);
            },
            .discriminant_switch => |ds| {
                const new_value = try self.normalizeBorrowedLoopSources(ds.value);
                const branches_res = try self.normalizeBorrowedLoopSourceSpan(ds.branches);
                if (new_value == ds.value and !branches_res.changed) return expr_id;
                return self.store.addExpr(.{ .discriminant_switch = .{
                    .value = new_value,
                    .union_layout = ds.union_layout,
                    .branches = branches_res.span,
                    .result_layout = ds.result_layout,
                } }, region);
            },
            .tag_payload_access => |tpa| {
                const new_value = try self.normalizeBorrowedLoopSources(tpa.value);
                if (new_value == tpa.value) return expr_id;
                return self.store.addExpr(.{ .tag_payload_access = .{
                    .value = new_value,
                    .union_layout = tpa.union_layout,
                    .payload_layout = tpa.payload_layout,
                } }, region);
            },
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
            => expr_id,
        };
    }

    fn normalizeBorrowedLoopSourceSpan(self: *RcInsertPass, span: LirExprSpan) Allocator.Error!struct { span: LirExprSpan, changed: bool } {
        const exprs = self.store.getExprSpan(span);
        if (exprs.len == 0) return .{ .span = span, .changed = false };

        var changed = false;
        var new_exprs = std.ArrayList(LirExprId).empty;
        defer new_exprs.deinit(self.allocator);

        for (exprs) |expr_id| {
            const new_expr = try self.normalizeBorrowedLoopSources(expr_id);
            changed = changed or new_expr != expr_id;
            try new_exprs.append(self.allocator, new_expr);
        }

        if (!changed) return .{ .span = span, .changed = false };
        return .{ .span = try self.store.addExprSpan(new_exprs.items), .changed = true };
    }

    fn clonePatternTree(self: *RcInsertPass, pat_id: LirPatternId) Allocator.Error!LirPatternId {
        if (pat_id.isNone()) return pat_id;

        const region = self.store.getPatternRegion(pat_id);
        return switch (self.store.getPattern(pat_id)) {
            .bind => |bind| self.store.addPattern(.{ .bind = bind }, region),
            .wildcard => |wildcard| self.store.addPattern(.{ .wildcard = wildcard }, region),
            .int_literal => |int_lit| self.store.addPattern(.{ .int_literal = int_lit }, region),
            .float_literal => |float_lit| self.store.addPattern(.{ .float_literal = float_lit }, region),
            .str_literal => |str_lit| self.store.addPattern(.{ .str_literal = str_lit }, region),
            .as_pattern => |as_pat| {
                const new_inner = try self.clonePatternTree(as_pat.inner);
                return self.store.addPattern(.{ .as_pattern = .{
                    .symbol = as_pat.symbol,
                    .layout_idx = as_pat.layout_idx,
                    .reassignable = as_pat.reassignable,
                    .inner = new_inner,
                } }, region);
            },
            .tag => |tag_pat| {
                const old_args = self.store.getPatternSpan(tag_pat.args);
                var new_args = std.ArrayList(LirPatternId).empty;
                defer new_args.deinit(self.allocator);
                for (old_args) |arg| try new_args.append(self.allocator, try self.clonePatternTree(arg));
                const new_args_span = try self.store.addPatternSpan(new_args.items);
                return self.store.addPattern(.{ .tag = .{
                    .discriminant = tag_pat.discriminant,
                    .union_layout = tag_pat.union_layout,
                    .args = new_args_span,
                } }, region);
            },
            .struct_ => |struct_pat| {
                const old_fields = self.store.getPatternSpan(struct_pat.fields);
                var new_fields = std.ArrayList(LirPatternId).empty;
                defer new_fields.deinit(self.allocator);
                for (old_fields) |field| try new_fields.append(self.allocator, try self.clonePatternTree(field));
                const new_fields_span = try self.store.addPatternSpan(new_fields.items);
                return self.store.addPattern(.{ .struct_ = .{
                    .struct_layout = struct_pat.struct_layout,
                    .fields = new_fields_span,
                } }, region);
            },
            .list => |list_pat| {
                const old_prefix = self.store.getPatternSpan(list_pat.prefix);
                var new_prefix = std.ArrayList(LirPatternId).empty;
                defer new_prefix.deinit(self.allocator);
                for (old_prefix) |prefix| try new_prefix.append(self.allocator, try self.clonePatternTree(prefix));

                const old_suffix = self.store.getPatternSpan(list_pat.suffix);
                var new_suffix = std.ArrayList(LirPatternId).empty;
                defer new_suffix.deinit(self.allocator);
                for (old_suffix) |suffix| try new_suffix.append(self.allocator, try self.clonePatternTree(suffix));

                return self.store.addPattern(.{ .list = .{
                    .elem_layout = list_pat.elem_layout,
                    .list_layout = list_pat.list_layout,
                    .prefix = try self.store.addPatternSpan(new_prefix.items),
                    .rest = try self.clonePatternTree(list_pat.rest),
                    .suffix = try self.store.addPatternSpan(new_suffix.items),
                } }, region);
            },
        };
    }

    fn uniquifyBindingPatterns(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!void {
        if (expr_id.isNone()) return;

        const expr_ptr = self.store.getExprPtr(expr_id);
        switch (expr_ptr.*) {
            .block => |*block| {
                for (self.store.getStmtsMut(block.stmts)) |*stmt| {
                    switch (stmt.*) {
                        .decl => |*binding| {
                            binding.pattern = try self.clonePatternTree(binding.pattern);
                            try self.uniquifyBindingPatterns(binding.expr);
                        },
                        .mutate => |*binding| {
                            binding.pattern = try self.clonePatternTree(binding.pattern);
                            try self.uniquifyBindingPatterns(binding.expr);
                        },
                        .cell_init => |*binding| try self.uniquifyBindingPatterns(binding.expr),
                        .cell_store => |*binding| try self.uniquifyBindingPatterns(binding.expr),
                        .cell_drop => {},
                    }
                }
                try self.uniquifyBindingPatterns(block.final_expr);
            },
            .cell_load => {},
            .match_expr => |*match_expr| {
                try self.uniquifyBindingPatterns(match_expr.value);
                for (self.store.getMatchBranchesMut(match_expr.branches)) |*branch| {
                    branch.pattern = try self.clonePatternTree(branch.pattern);
                    try self.uniquifyBindingPatterns(branch.guard);
                    try self.uniquifyBindingPatterns(branch.body);
                }
            },
            .if_then_else => |*ite| {
                for (self.store.getIfBranchesMut(ite.branches)) |*branch| {
                    try self.uniquifyBindingPatterns(branch.cond);
                    try self.uniquifyBindingPatterns(branch.body);
                }
                try self.uniquifyBindingPatterns(ite.final_else);
            },
            .for_loop => |*fl| {
                fl.elem_pattern = try self.clonePatternTree(fl.elem_pattern);
                try self.uniquifyBindingPatterns(fl.list_expr);
                try self.uniquifyBindingPatterns(fl.body);
            },
            .while_loop => |*wl| {
                try self.uniquifyBindingPatterns(wl.cond);
                try self.uniquifyBindingPatterns(wl.body);
            },
            .discriminant_switch => |*ds| {
                try self.uniquifyBindingPatterns(ds.value);
                for (self.store.getExprSpan(ds.branches)) |branch| try self.uniquifyBindingPatterns(branch);
            },
            .proc_call => |call| {
                for (self.store.getExprSpan(call.args)) |arg| try self.uniquifyBindingPatterns(arg);
            },
            .list => |list_expr| {
                for (self.store.getExprSpan(list_expr.elems)) |elem| try self.uniquifyBindingPatterns(elem);
            },
            .struct_ => |struct_expr| {
                for (self.store.getExprSpan(struct_expr.fields)) |field| try self.uniquifyBindingPatterns(field);
            },
            .tag => |tag_expr| {
                for (self.store.getExprSpan(tag_expr.args)) |arg| try self.uniquifyBindingPatterns(arg);
            },
            .struct_access => |sa| try self.uniquifyBindingPatterns(sa.struct_expr),
            .nominal => |n| try self.uniquifyBindingPatterns(n.backing_expr),
            .early_return => |ret| try self.uniquifyBindingPatterns(ret.expr),
            .dbg => |d| try self.uniquifyBindingPatterns(d.expr),
            .expect => |e| {
                try self.uniquifyBindingPatterns(e.cond);
                try self.uniquifyBindingPatterns(e.body);
            },
            .low_level => |ll| {
                for (self.store.getExprSpan(ll.args)) |arg| try self.uniquifyBindingPatterns(arg);
            },
            .hosted_call => |hc| {
                for (self.store.getExprSpan(hc.args)) |arg| try self.uniquifyBindingPatterns(arg);
            },
            .str_concat => |parts| {
                for (self.store.getExprSpan(parts)) |part| try self.uniquifyBindingPatterns(part);
            },
            .int_to_str => |its| try self.uniquifyBindingPatterns(its.value),
            .float_to_str => |fts| try self.uniquifyBindingPatterns(fts.value),
            .dec_to_str => |value| try self.uniquifyBindingPatterns(value),
            .str_escape_and_quote => |value| try self.uniquifyBindingPatterns(value),
            .tag_payload_access => |tpa| try self.uniquifyBindingPatterns(tpa.value),
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

    /// Count how many times each symbol is referenced, writing into `target`.
    /// Also records the layout for each symbol found in bind patterns.
    fn countUsesInto(self: *RcInsertPass, expr_id: LirExprId, target: *std.AutoHashMap(u64, u32)) Allocator.Error!void {
        if (expr_id.isNone()) return;

        const expr = self.store.getExpr(expr_id);
        switch (expr) {
            .lookup => |lookup| {
                if (!lookup.symbol.isNone()) {
                    const key = self.lookupKey(expr_id, lookup.symbol);
                    try bumpUseCount(target, key);
                    try self.symbol_layouts.put(key, self.keyLayout(key, lookup.layout_idx));
                }
            },
            .cell_load => {},
            .block => |block| {
                const stmts = self.store.getStmts(block.stmts);
                // Pre-register all pattern symbols so lambdas in early
                // statements can detect captures from later siblings.
                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| try self.registerPatternSymbolInto(binding.pattern, target),
                        .cell_init, .cell_store, .cell_drop => {},
                    }
                }
                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| try self.countUsesInto(binding.expr, target),
                        .cell_init, .cell_store => |binding| try self.countUsesInto(binding.expr, target),
                        .cell_drop => {},
                    }
                }
                try self.countUsesInto(block.final_expr, target);
            },
            .proc_call => |call| {
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
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                {
                    local.clearRetainingCapacity();
                    try self.countUsesInto(ite.final_else, &local);
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
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
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| {
                    const gop = try target.getOrPut(key.*);
                    if (gop.found_existing) gop.value_ptr.* += 1 else gop.value_ptr.* = 1;
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
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
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
            .incref => |inc| try self.countUsesInto(inc.value, target),
            .decref => |dec| try self.countUsesInto(dec.value, target),
            .free => |free| try self.countUsesInto(free.value, target),
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
            .cell_load => {},
            .block => |block| {
                const stmts = self.store.getStmts(block.stmts);
                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| try self.registerPatternSymbolInto(binding.pattern, target),
                        .cell_init, .cell_store, .cell_drop => {},
                    }
                }
                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| try self.countBindingExprConsumptionInto(binding, target),
                        .cell_init, .cell_store => |binding| try self.countConsumedValueInto(binding.expr, target),
                        .cell_drop => {},
                    }
                }
                try self.countConsumedUsesInto(block.final_expr, target);
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
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                local.clearRetainingCapacity();
                try self.countConsumedUsesInto(ite.final_else, &local);
                {
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
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
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
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
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            .struct_access => |sa| {
                if (self.exprAliasesManagedRef(sa.struct_expr, sa.struct_layout)) {
                    try self.countConsumedUsesInto(sa.struct_expr, target);
                } else {
                    try self.countConsumedValueInto(sa.struct_expr, target);
                }
            },
            .tag_payload_access => |tpa| {
                if (self.exprAliasesManagedRef(tpa.value, tpa.union_layout)) {
                    try self.countConsumedUsesInto(tpa.value, target);
                } else {
                    try self.countConsumedValueInto(tpa.value, target);
                }
            },
            .proc_call => |call| {
                const args = self.store.getExprSpan(call.args);
                for (args) |arg_id| {
                    try self.countConsumedValueInto(arg_id, target);
                }
            },
            .list => |list| {
                const elems = self.store.getExprSpan(list.elems);
                for (elems) |elem_id| {
                    try self.countConsumedValueInto(elem_id, target);
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
            .low_level => |ll| {
                const args = self.store.getExprSpan(ll.args);
                const arg_ownership = ll.op.getArgOwnership();
                if (builtin.mode == .Debug and arg_ownership.len != args.len) {
                    std.debug.panic(
                        "RC invariant violated: low-level {s} expected {d} ownership entries for {d} args",
                        .{ @tagName(ll.op), arg_ownership.len, args.len },
                    );
                }
                for (args, 0..) |arg_id, i| {
                    switch (arg_ownership[i]) {
                        .consume => try self.countConsumedValueInto(arg_id, target),
                        .borrow => try self.countConsumedUsesInto(arg_id, target),
                    }
                }
            },
            .hosted_call => |hc| {
                const args = self.store.getExprSpan(hc.args);
                for (args) |arg_id| {
                    try self.countConsumedUsesInto(arg_id, target);
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
                // Loop sources are borrowed. Any retained owner used to keep the
                // source alive across the loop is introduced explicitly outside
                // the loop, so the loop itself must not mark the original source
                // binding as consumed.
                try self.countConsumedUsesInto(fl.list_expr, target);
                try self.registerPatternSymbolInto(fl.elem_pattern, target);
                try self.countConsumedUsesInto(fl.body, target);
            },
            .while_loop => |wl| {
                // Loop-carried outer bindings must survive across iterations.
                // Any consuming use inside the condition/body should therefore
                // be satisfied by per-iteration retained owners, not by
                // consuming the original outer binding once at the loop site.
                try self.countConsumedUsesInto(wl.cond, target);
                try self.countConsumedUsesInto(wl.body, target);
            },
            .lookup => {},
            .incref => |inc| try self.countConsumedUsesInto(inc.value, target),
            .decref => |dec| try self.countConsumedValueInto(dec.value, target),
            .free => |free| try self.countConsumedValueInto(free.value, target),
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

    /// Count how many independent owned refs borrowed bindings need to supply
    /// for an expression whose result is itself only borrowed. Unlike ordinary
    /// consumption counting, bare lookups in borrow mode do not transfer an
    /// owned reference here.
    fn countBorrowOwnerDemandUsesInto(
        self: *RcInsertPass,
        expr_id: LirExprId,
        target: *std.AutoHashMap(u64, u32),
    ) Allocator.Error!void {
        if (expr_id.isNone()) return;

        const expr = self.store.getExpr(expr_id);
        switch (expr) {
            .lookup => {},
            .cell_load => {},
            .block => |block| {
                const stmts = self.store.getStmts(block.stmts);
                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| try self.registerPatternSymbolInto(binding.pattern, target),
                        .cell_init, .cell_store, .cell_drop => {},
                    }
                }
                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| try self.countBindingBorrowOwnerDemandInto(binding, target),
                        .cell_init, .cell_store => |binding| try self.countBorrowOwnerDemandValueInto(binding.expr, target),
                        .cell_drop => {},
                    }
                }
                try self.countBorrowOwnerDemandUsesInto(block.final_expr, target);
            },
            .if_then_else => |ite| {
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                const branches = self.store.getIfBranches(ite.branches);
                for (branches) |branch| {
                    try self.countBorrowOwnerDemandValueInto(branch.cond, target);
                    local.clearRetainingCapacity();
                    try self.countBorrowOwnerDemandUsesInto(branch.body, &local);
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                local.clearRetainingCapacity();
                try self.countBorrowOwnerDemandUsesInto(ite.final_else, &local);
                {
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            .match_expr => |w| {
                try self.countBorrowOwnerDemandUsesInto(w.value, target);
                const branches = self.store.getMatchBranches(w.branches);
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                for (branches) |branch| {
                    local.clearRetainingCapacity();
                    try self.registerPatternSymbolInto(branch.pattern, &local);
                    try self.countBorrowOwnerDemandUsesInto(branch.guard, &local);
                    try self.countBorrowOwnerDemandUsesInto(branch.body, &local);
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            .discriminant_switch => |ds| {
                try self.countBorrowOwnerDemandUsesInto(ds.value, target);
                const branches = self.store.getExprSpan(ds.branches);
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                for (branches) |branch| {
                    local.clearRetainingCapacity();
                    try self.countBorrowOwnerDemandUsesInto(branch, &local);
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            .struct_access => |sa| {
                if (self.exprAliasesManagedRef(sa.struct_expr, sa.struct_layout)) {
                    try self.countBorrowOwnerDemandUsesInto(sa.struct_expr, target);
                } else {
                    try self.countBorrowOwnerDemandValueInto(sa.struct_expr, target);
                }
            },
            .tag_payload_access => |tpa| {
                if (self.exprAliasesManagedRef(tpa.value, tpa.union_layout)) {
                    try self.countBorrowOwnerDemandUsesInto(tpa.value, target);
                } else {
                    try self.countBorrowOwnerDemandValueInto(tpa.value, target);
                }
            },
            .proc_call => |call| {
                const args = self.store.getExprSpan(call.args);
                for (args) |arg_id| {
                    try self.countBorrowOwnerDemandValueInto(arg_id, target);
                }
            },
            .list => |list| {
                const elems = self.store.getExprSpan(list.elems);
                for (elems) |elem_id| {
                    try self.countBorrowOwnerDemandValueInto(elem_id, target);
                }
            },
            .struct_ => |s| {
                const fields = self.store.getExprSpan(s.fields);
                for (fields) |field_id| {
                    try self.countBorrowOwnerDemandValueInto(field_id, target);
                }
            },
            .tag => |t| {
                const args = self.store.getExprSpan(t.args);
                for (args) |arg_id| {
                    try self.countBorrowOwnerDemandValueInto(arg_id, target);
                }
            },
            .nominal => |n| try self.countBorrowOwnerDemandUsesInto(n.backing_expr, target),
            .early_return => |ret| try self.countBorrowOwnerDemandValueInto(ret.expr, target),
            .dbg => |d| try self.countBorrowOwnerDemandUsesInto(d.expr, target),
            .expect => |e| {
                try self.countBorrowOwnerDemandValueInto(e.cond, target);
                try self.countBorrowOwnerDemandUsesInto(e.body, target);
            },
            .low_level => |ll| {
                const args = self.store.getExprSpan(ll.args);
                const arg_ownership = ll.op.getArgOwnership();
                if (builtin.mode == .Debug and arg_ownership.len != args.len) {
                    std.debug.panic(
                        "RC invariant violated: low-level {s} expected {d} ownership entries for {d} args",
                        .{ @tagName(ll.op), arg_ownership.len, args.len },
                    );
                }
                for (args, 0..) |arg_id, i| {
                    switch (arg_ownership[i]) {
                        .consume => try self.countBorrowOwnerDemandValueInto(arg_id, target),
                        .borrow => {
                            if (self.lowLevelBorrowedArgNeedsOwnerForResult(ll, i)) {
                                try self.countBorrowOwnerDemandValueInto(arg_id, target);
                            } else {
                                try self.countBorrowOwnerDemandUsesInto(arg_id, target);
                            }
                        },
                    }
                }
            },
            .hosted_call => |hc| {
                const args = self.store.getExprSpan(hc.args);
                for (args) |arg_id| {
                    try self.countBorrowOwnerDemandUsesInto(arg_id, target);
                }
            },
            .str_concat => |span| {
                const parts = self.store.getExprSpan(span);
                for (parts) |part_id| {
                    try self.countBorrowOwnerDemandUsesInto(part_id, target);
                }
            },
            .int_to_str => |its| try self.countBorrowOwnerDemandUsesInto(its.value, target),
            .float_to_str => |fts| try self.countBorrowOwnerDemandUsesInto(fts.value, target),
            .dec_to_str => |d| try self.countBorrowOwnerDemandUsesInto(d, target),
            .str_escape_and_quote => |s| try self.countBorrowOwnerDemandUsesInto(s, target),
            .for_loop => |fl| {
                try self.countBorrowOwnerDemandUsesInto(fl.list_expr, target);
                try self.registerPatternSymbolInto(fl.elem_pattern, target);
                try self.countBorrowOwnerDemandUsesInto(fl.body, target);
            },
            .while_loop => |wl| {
                try self.countBorrowOwnerDemandUsesInto(wl.cond, target);
                try self.countBorrowOwnerDemandUsesInto(wl.body, target);
            },
            .incref => |inc| try self.countBorrowOwnerDemandUsesInto(inc.value, target),
            .decref => |dec| try self.countBorrowOwnerDemandValueInto(dec.value, target),
            .free => |free| try self.countBorrowOwnerDemandValueInto(free.value, target),
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

    /// Count how many independent owned refs borrowed bindings need to supply
    /// for an expression whose result is consumed by the current scope. Unlike
    /// ordinary consumption counting, a bare lookup contributes here even when
    /// the binding is borrow-only, because producing an owned result requires a
    /// retained owner from that borrowed source.
    fn countBorrowOwnerDemandValueInto(
        self: *RcInsertPass,
        expr_id: LirExprId,
        target: *std.AutoHashMap(u64, u32),
    ) Allocator.Error!void {
        if (expr_id.isNone()) return;

        switch (self.store.getExpr(expr_id)) {
            .lookup => |lookup| {
                if (!lookup.symbol.isNone()) {
                    const key = self.lookupKey(expr_id, lookup.symbol);
                    try self.symbol_layouts.put(key, self.keyLayout(key, lookup.layout_idx));
                    try bumpUseCount(target, key);
                }
            },
            .nominal => |n| try self.countBorrowOwnerDemandValueInto(n.backing_expr, target),
            .block => |block| {
                const stmts = self.store.getStmts(block.stmts);
                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| try self.registerPatternSymbolInto(binding.pattern, target),
                        .cell_init, .cell_store, .cell_drop => {},
                    }
                }
                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| try self.countBindingBorrowOwnerDemandInto(binding, target),
                        .cell_init, .cell_store => |binding| try self.countBorrowOwnerDemandValueInto(binding.expr, target),
                        .cell_drop => {},
                    }
                }
                try self.countBorrowOwnerDemandValueInto(block.final_expr, target);
            },
            .if_then_else => |ite| {
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                const branches = self.store.getIfBranches(ite.branches);
                for (branches) |branch| {
                    try self.countBorrowOwnerDemandValueInto(branch.cond, target);
                    local.clearRetainingCapacity();
                    try self.countBorrowOwnerDemandValueInto(branch.body, &local);
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                local.clearRetainingCapacity();
                try self.countBorrowOwnerDemandValueInto(ite.final_else, &local);
                {
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            .match_expr => |w| {
                try self.countBorrowOwnerDemandUsesInto(w.value, target);
                const branches = self.store.getMatchBranches(w.branches);
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                for (branches) |branch| {
                    local.clearRetainingCapacity();
                    try self.registerPatternSymbolInto(branch.pattern, &local);
                    try self.countBorrowOwnerDemandUsesInto(branch.guard, &local);
                    try self.countBorrowOwnerDemandValueInto(branch.body, &local);
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            .discriminant_switch => |ds| {
                try self.countBorrowOwnerDemandUsesInto(ds.value, target);
                const branches = self.store.getExprSpan(ds.branches);
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                var local = std.AutoHashMap(u64, u32).init(self.allocator);
                defer local.deinit();
                for (branches) |branch| {
                    local.clearRetainingCapacity();
                    try self.countBorrowOwnerDemandValueInto(branch, &local);
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                var it = symbols_in_any_branch.keyIterator();
                while (it.next()) |key| try bumpUseCount(target, key.*);
            },
            else => try self.countBorrowOwnerDemandUsesInto(expr_id, target),
        }
    }

    fn lowLevelBorrowedArgNeedsOwnerForResult(self: *RcInsertPass, ll: anytype, arg_index: usize) bool {
        if (!ll.op.borrowedArgNeededForResult(arg_index)) return false;

        return switch (ll.op) {
            .list_get_unsafe => self.layoutNeedsRc(ll.ret_layout),
            else => true,
        };
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
                    const key = self.lookupKey(expr_id, lookup.symbol);
                    try self.symbol_layouts.put(key, self.keyLayout(key, lookup.layout_idx));
                    if (self.keyUseMode(key) == .borrow) return;
                    try bumpUseCount(target, key);
                }
            },
            .nominal => |n| try self.countConsumedValueInto(n.backing_expr, target),
            .block => |block| {
                const stmts = self.store.getStmts(block.stmts);
                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| try self.registerPatternSymbolInto(binding.pattern, target),
                        .cell_init, .cell_store, .cell_drop => {},
                    }
                }
                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| try self.countBindingExprConsumptionInto(binding, target),
                        .cell_init, .cell_store => |binding| try self.countConsumedValueInto(binding.expr, target),
                        .cell_drop => {},
                    }
                }
                try self.countConsumedValueInto(block.final_expr, target);
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
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
                }
                local.clearRetainingCapacity();
                try self.countConsumedValueInto(ite.final_else, &local);
                {
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
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
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
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
                    var it = local.iterator();
                    while (it.next()) |entry| {
                        if (entry.value_ptr.* == 0) continue;
                        try symbols_in_any_branch.put(entry.key_ptr.*, {});
                    }
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
                if (!bind.symbol.isNone()) try ctx.onBind(pat_id, bind.symbol, bind.layout_idx, bind.reassignable);
            },
            .as_pattern => |as_pat| {
                if (!as_pat.symbol.isNone()) try ctx.onBind(pat_id, as_pat.symbol, as_pat.layout_idx, as_pat.reassignable);
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
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                try ctx.pass.symbol_layouts.put(key, ctx.pass.keyLayout(key, layout_idx));
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
        if (idx_int >= self.layout_store.layouts.len()) return false;
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
            .for_loop => |fl| self.processForLoop(fl, region, expr_id),
            .while_loop => |wl| self.processWhileLoop(wl, region, expr_id),
            .discriminant_switch => |ds| self.processDiscriminantSwitch(ds, region),
            .early_return => |ret| self.processEarlyReturn(ret, region, expr_id),
            .cell_load => expr_id,
            .proc_call => |call| {
                const args_res = try self.processExprSpanSequenced(call.args, .consume);
                if (!args_res.changed) return expr_id;
                return self.rebuildProcCall(call, args_res.span, region);
            },
            .list => |list| {
                const elems_res = try self.processExprSpanSequenced(list.elems, .consume);
                if (!elems_res.changed) return expr_id;
                return self.store.addExpr(.{ .list = .{
                    .list_layout = list.list_layout,
                    .elem_layout = list.elem_layout,
                    .elems = elems_res.span,
                } }, region);
            },
            .struct_ => |s| {
                const fields_res = try self.processExprSpanSequenced(s.fields, .consume);
                if (!fields_res.changed) return expr_id;
                return self.store.addExpr(.{ .struct_ = .{
                    .struct_layout = s.struct_layout,
                    .fields = fields_res.span,
                } }, region);
            },
            .struct_access => |sa| {
                const new_struct_expr = try self.processExpr(sa.struct_expr);

                var stmts = std.ArrayList(LirStmt).empty;
                defer stmts.deinit(self.allocator);

                const parent_semantics: LirStmt.BindingSemantics = if (self.exprAliasesManagedRef(new_struct_expr, sa.struct_layout))
                    .retained
                else
                    .owned;
                const parent = try self.bindExprToFreshLookupWithSemantics(&stmts, new_struct_expr, sa.struct_layout, parent_semantics, region);
                if (parent_semantics == .retained) {
                    // This projection block is synthesized after ownership normalization,
                    // so introduce the retained owner explicitly here instead of relying
                    // on a later block pass to legalize the binding semantics.
                    try self.emitIncrefInto(parent.symbol, sa.struct_layout, 1, region, &stmts);
                }
                const raw_field = try self.store.addExpr(.{ .struct_access = .{
                    .struct_expr = parent.lookup,
                    .struct_layout = sa.struct_layout,
                    .field_layout = sa.field_layout,
                    .field_idx = sa.field_idx,
                } }, region);
                const field = try self.bindExprToFreshLookup(&stmts, raw_field, sa.field_layout, region);

                if (self.layoutNeedsRc(sa.field_layout)) {
                    try self.emitIncrefInto(field.symbol, sa.field_layout, 1, region, &stmts);
                }
                if (self.layoutNeedsRc(sa.struct_layout)) {
                    try self.emitDecrefInto(parent.symbol, sa.struct_layout, region, &stmts);
                }

                return self.store.addExpr(.{ .block = .{
                    .stmts = try self.store.addStmts(stmts.items),
                    .final_expr = field.lookup,
                    .result_layout = sa.field_layout,
                } }, region);
            },
            .tag => |tag| {
                const args_res = try self.processExprSpanSequenced(tag.args, .consume);
                if (!args_res.changed) return expr_id;
                return self.store.addExpr(.{ .tag = .{
                    .discriminant = tag.discriminant,
                    .union_layout = tag.union_layout,
                    .args = args_res.span,
                } }, region);
            },
            .low_level => |ll| {
                const args = self.store.getExprSpan(ll.args);
                const arg_ownership = ll.op.getArgOwnership();
                if (builtin.mode == .Debug and arg_ownership.len != args.len) {
                    std.debug.panic(
                        "RC invariant violated: low-level {s} expected {d} ownership entries for {d} args",
                        .{ @tagName(ll.op), arg_ownership.len, args.len },
                    );
                }

                var new_args = std.ArrayList(LirExprId).empty;
                defer new_args.deinit(self.allocator);

                var added_maps = std.ArrayList(std.AutoHashMap(u64, u32)).empty;
                defer {
                    var i = added_maps.items.len;
                    while (i > 0) {
                        i -= 1;
                        self.popExprUsesFromBlockConsumed(&added_maps.items[i]);
                        added_maps.items[i].deinit();
                    }
                    added_maps.deinit(self.allocator);
                }

                var changed = false;
                for (args, 0..) |arg_id, i| {
                    const new_arg = try self.processExpr(arg_id);
                    if (new_arg != arg_id) changed = true;
                    try new_args.append(self.allocator, new_arg);

                    if (i + 1 < args.len) {
                        const ownership: ExprOwnership = switch (arg_ownership[i]) {
                            .consume => .consume,
                            .borrow => .borrow,
                        };
                        const added = try self.pushExprUsesForOwnership(arg_id, ownership);
                        try added_maps.append(self.allocator, added);
                    }
                }

                if (!changed) return expr_id;
                const new_args_span = try self.store.addExprSpan(new_args.items);
                return self.store.addExpr(.{ .low_level = .{
                    .op = ll.op,
                    .args = new_args_span,
                    .ret_layout = ll.ret_layout,
                    .callable_proc = ll.callable_proc,
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
                const live_len = self.live_rc_symbols.items.len;
                const processed_cond = try self.processExpr(e.cond);
                self.scratch_uses.clearRetainingCapacity();
                try self.countUsesInto(e.cond, &self.scratch_uses);
                const new_cond = try self.wrapExprWithLiveBorrowRcOps(
                    processed_cond,
                    live_len,
                    &self.scratch_uses,
                    .bool,
                    region,
                );
                var cond_added = try self.pushExprUsesToBlockConsumed(e.cond);
                defer {
                    self.popExprUsesFromBlockConsumed(&cond_added);
                    cond_added.deinit();
                }
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
                const parts_res = try self.processExprSpanSequenced(parts, .borrow);
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

                var stmts = std.ArrayList(LirStmt).empty;
                defer stmts.deinit(self.allocator);

                const parent_semantics: LirStmt.BindingSemantics = if (self.exprAliasesManagedRef(new_value, tpa.union_layout))
                    .retained
                else
                    .owned;
                const parent = try self.bindExprToFreshLookupWithSemantics(&stmts, new_value, tpa.union_layout, parent_semantics, region);
                if (parent_semantics == .retained) {
                    try self.emitIncrefInto(parent.symbol, tpa.union_layout, 1, region, &stmts);
                }
                const raw_payload = try self.store.addExpr(.{ .tag_payload_access = .{
                    .value = parent.lookup,
                    .union_layout = tpa.union_layout,
                    .payload_layout = tpa.payload_layout,
                } }, region);
                const payload = try self.bindExprToFreshLookup(&stmts, raw_payload, tpa.payload_layout, region);

                if (self.layoutNeedsRc(tpa.payload_layout)) {
                    try self.emitIncrefInto(payload.symbol, tpa.payload_layout, 1, region, &stmts);
                }
                if (self.layoutNeedsRc(tpa.union_layout)) {
                    try self.emitDecrefInto(parent.symbol, tpa.union_layout, region, &stmts);
                }

                return self.store.addExpr(.{ .block = .{
                    .stmts = try self.store.addStmts(stmts.items),
                    .final_expr = payload.lookup,
                    .result_layout = tpa.payload_layout,
                } }, region);
            },
            .hosted_call => |hc| {
                const args_res = try self.processExprSpanSequenced(hc.args, .borrow);
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

    fn pushExprUsesForOwnership(
        self: *RcInsertPass,
        expr_id: LirExprId,
        ownership: ExprOwnership,
    ) Allocator.Error!std.AutoHashMap(u64, u32) {
        return switch (ownership) {
            .consume => self.pushExprUsesToBlockConsumed(expr_id),
            .borrow => self.pushBorrowedExprUsesToBlockConsumed(expr_id),
        };
    }

    fn countBindingExprConsumptionInto(
        self: *RcInsertPass,
        binding: LirStmt.Binding,
        target: *std.AutoHashMap(u64, u32),
    ) Allocator.Error!void {
        return switch (binding.semantics) {
            .owned, .scoped_borrow => self.countConsumedValueInto(binding.expr, target),
            .borrow_alias, .retained => self.countConsumedUsesInto(binding.expr, target),
        };
    }

    fn countBindingBorrowOwnerDemandInto(
        self: *RcInsertPass,
        binding: LirStmt.Binding,
        target: *std.AutoHashMap(u64, u32),
    ) Allocator.Error!void {
        return switch (binding.semantics) {
            .owned, .scoped_borrow => self.countBorrowOwnerDemandValueInto(binding.expr, target),
            .borrow_alias, .retained => self.countBorrowOwnerDemandUsesInto(binding.expr, target),
        };
    }

    fn countBindingUsesInto(
        self: *RcInsertPass,
        binding: LirStmt.Binding,
        target: *std.AutoHashMap(u64, u32),
    ) Allocator.Error!void {
        try self.countUsesInto(binding.expr, target);
    }

    fn emitBindingIntroductionRcOps(
        self: *RcInsertPass,
        binding: LirStmt.Binding,
        region: Region,
        rc_stmts: *std.ArrayList(LirStmt),
    ) Allocator.Error!void {
        switch (binding.semantics) {
            .owned, .borrow_alias, .scoped_borrow => {},
            .retained => try self.emitRetainedIncrefsForPattern(binding.pattern, region, rc_stmts),
        }
    }

    fn processExprSpanSequenced(
        self: *RcInsertPass,
        span: LirExprSpan,
        ownership: ExprOwnership,
    ) Allocator.Error!struct { span: LirExprSpan, changed: bool } {
        const exprs = self.store.getExprSpan(span);
        if (exprs.len == 0) return .{ .span = span, .changed = false };

        var new_exprs = std.ArrayList(LirExprId).empty;
        defer new_exprs.deinit(self.allocator);

        var added_maps = std.ArrayList(std.AutoHashMap(u64, u32)).empty;
        defer {
            var i = added_maps.items.len;
            while (i > 0) {
                i -= 1;
                self.popExprUsesFromBlockConsumed(&added_maps.items[i]);
                added_maps.items[i].deinit();
            }
            added_maps.deinit(self.allocator);
        }

        var changed = false;
        for (exprs, 0..) |expr_id, i| {
            const new_expr = try self.processExpr(expr_id);
            if (new_expr != expr_id) changed = true;
            try new_exprs.append(self.allocator, new_expr);

            if (i + 1 < exprs.len) {
                const added = try self.pushExprUsesForOwnership(expr_id, ownership);
                try added_maps.append(self.allocator, added);
            }
        }

        if (!changed) return .{ .span = span, .changed = false };

        return .{
            .span = try self.store.addExprSpan(new_exprs.items),
            .changed = true,
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
        // Copy stmts to avoid iterator invalidation from nested processExpr
        // calls that may grow the stmts ArrayList.
        const stmts = try self.allocator.dupe(LirStmt, self.store.getStmts(stmts_span));
        defer self.allocator.free(stmts);

        // Save live_rc_symbols depth so nested blocks restore on exit.
        const saved_live_len = self.live_rc_symbols.items.len;
        defer self.live_rc_symbols.shrinkRetainingCapacity(saved_live_len);
        const saved_live_cells_len = self.live_cells.items.len;
        defer self.live_cells.shrinkRetainingCapacity(saved_live_cells_len);

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

        const PendingBindingIncrefs = struct {
            pattern: LirPatternId,
            insert_idx: usize,
        };
        var pending_binding_increfs = std.ArrayList(PendingBindingIncrefs).empty;
        defer pending_binding_increfs.deinit(self.allocator);

        var later_uses = std.ArrayList(std.AutoHashMap(u64, u32)).empty;
        defer {
            for (later_uses.items) |*use_map| {
                use_map.deinit();
            }
            later_uses.deinit(self.allocator);
        }
        try later_uses.resize(self.allocator, stmts.len);
        {
            var running_uses = std.AutoHashMap(u64, u32).init(self.allocator);
            defer running_uses.deinit();

            try self.countUsesInto(final_expr, &running_uses);

            var rev_i = stmts.len;
            while (rev_i > 0) {
                rev_i -= 1;

                var cloned = std.AutoHashMap(u64, u32).init(self.allocator);
                var use_it = running_uses.iterator();
                while (use_it.next()) |entry| {
                    try cloned.put(entry.key_ptr.*, entry.value_ptr.*);
                }
                later_uses.items[rev_i] = cloned;

                switch (stmts[rev_i]) {
                    .decl, .mutate => |binding| try self.countBindingUsesInto(binding, &running_uses),
                    .cell_init, .cell_store => |binding| try self.countUsesInto(binding.expr, &running_uses),
                    .cell_drop => {},
                }
            }
        }

        var changed = false;

        // Process each statement
        for (stmts, 0..) |stmt, stmt_index| {
            switch (stmt) {
                .decl, .mutate => |b| {
                    const processed_expr = try self.processExpr(b.expr);
                    const new_expr = try self.retainConsumedOperandsForLaterUse(
                        processed_expr,
                        &later_uses.items[stmt_index],
                        region,
                    );
                    if (new_expr != b.expr) changed = true;

                    self.scratch_consumed_uses.clearRetainingCapacity();
                    try self.countBindingExprConsumptionInto(
                        .{
                            .pattern = b.pattern,
                            .expr = new_expr,
                            .semantics = b.semantics,
                        },
                        &self.scratch_consumed_uses,
                    );
                    var use_it = self.scratch_consumed_uses.iterator();
                    while (use_it.next()) |entry| {
                        const gop = try self.block_consumed_uses.getOrPut(entry.key_ptr.*);
                        if (gop.found_existing) {
                            gop.value_ptr.* += entry.value_ptr.*;
                        } else {
                            gop.value_ptr.* = entry.value_ptr.*;
                        }
                    }

                    if (stmt == .mutate) {
                        const rhs_reads_mutated_symbol = try self.exprUsesPatternSymbol(b.expr, b.pattern);
                        if (!rhs_reads_mutated_symbol) {
                            const before_len = stmt_buf.items.len;
                            try self.emitMutateDecrefsForPattern(b.pattern, region, &stmt_buf);
                            if (stmt_buf.items.len > before_len) changed = true;
                        }
                    }

                    if (stmt == .decl) {
                        const before_len = stmt_buf.items.len;
                        try self.emitDeclShadowDecrefsForPattern(b.pattern, &block_decl_layouts, region, &stmt_buf);
                        if (stmt_buf.items.len > before_len) changed = true;
                    }

                    const new_binding: LirStmt.Binding = .{ .pattern = b.pattern, .expr = new_expr, .semantics = b.semantics };
                    try stmt_buf.append(self.allocator, switch (stmt) {
                        .decl => .{ .decl = new_binding },
                        .mutate => .{ .mutate = new_binding },
                        else => unreachable,
                    });

                    const before_binding_intro = stmt_buf.items.len;
                    try self.emitBindingIntroductionRcOps(new_binding, region, &stmt_buf);
                    if (stmt_buf.items.len > before_binding_intro) changed = true;

                    try self.trackLiveRcSymbolsForPattern(b.pattern, null);
                    try pending_binding_increfs.append(self.allocator, .{
                        .pattern = b.pattern,
                        .insert_idx = stmt_buf.items.len,
                    });

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

                    try self.recordPatternLayouts(b.pattern, &block_decl_layouts);
                },
                .cell_init => |cell| {
                    const processed_expr = try self.processExpr(cell.expr);
                    const new_expr = try self.retainConsumedOperandsForLaterUse(
                        processed_expr,
                        &later_uses.items[stmt_index],
                        region,
                    );
                    if (new_expr != cell.expr) changed = true;

                    self.scratch_consumed_uses.clearRetainingCapacity();
                    try self.countConsumedValueInto(new_expr, &self.scratch_consumed_uses);
                    var use_it = self.scratch_consumed_uses.iterator();
                    while (use_it.next()) |entry| {
                        const gop = try self.block_consumed_uses.getOrPut(entry.key_ptr.*);
                        if (gop.found_existing) {
                            gop.value_ptr.* += entry.value_ptr.*;
                        } else {
                            gop.value_ptr.* = entry.value_ptr.*;
                        }
                    }

                    try stmt_buf.append(self.allocator, .{ .cell_init = .{
                        .cell = cell.cell,
                        .layout_idx = cell.layout_idx,
                        .expr = new_expr,
                    } });
                    try self.live_cells.append(self.allocator, .{
                        .cell = cell.cell,
                        .layout_idx = cell.layout_idx,
                    });
                },
                .cell_store => |cell| {
                    const processed_expr = try self.processExpr(cell.expr);
                    const new_expr = try self.retainConsumedOperandsForLaterUse(
                        processed_expr,
                        &later_uses.items[stmt_index],
                        region,
                    );
                    if (new_expr != cell.expr) changed = true;

                    self.scratch_consumed_uses.clearRetainingCapacity();
                    try self.countConsumedValueInto(new_expr, &self.scratch_consumed_uses);
                    var use_it = self.scratch_consumed_uses.iterator();
                    while (use_it.next()) |entry| {
                        const gop = try self.block_consumed_uses.getOrPut(entry.key_ptr.*);
                        if (gop.found_existing) {
                            gop.value_ptr.* += entry.value_ptr.*;
                        } else {
                            gop.value_ptr.* = entry.value_ptr.*;
                        }
                    }

                    if (self.layoutNeedsRc(cell.layout_idx)) {
                        const temp = try self.freshResultPattern(cell.layout_idx, region);
                        const temp_lookup = try self.store.addExpr(.{ .lookup = .{
                            .symbol = temp.symbol,
                            .layout_idx = cell.layout_idx,
                        } }, region);

                        try stmt_buf.append(self.allocator, .{ .decl = .{
                            .pattern = temp.pattern,
                            .expr = new_expr,
                        } });

                        const before_len = stmt_buf.items.len;
                        try self.emitCellDecrefInto(cell.cell, cell.layout_idx, region, &stmt_buf);
                        if (stmt_buf.items.len > before_len) changed = true;
                        try stmt_buf.append(self.allocator, .{ .cell_store = .{
                            .cell = cell.cell,
                            .layout_idx = cell.layout_idx,
                            .expr = temp_lookup,
                        } });
                        changed = true;
                    } else {
                        try stmt_buf.append(self.allocator, .{ .cell_store = .{
                            .cell = cell.cell,
                            .layout_idx = cell.layout_idx,
                            .expr = new_expr,
                        } });
                    }
                },
                .cell_drop => |cell| {
                    if (self.layoutNeedsRc(cell.layout_idx)) {
                        const before_len = stmt_buf.items.len;
                        try self.emitCellDecrefInto(cell.cell, cell.layout_idx, region, &stmt_buf);
                        if (stmt_buf.items.len > before_len) changed = true;
                    }
                    try stmt_buf.append(self.allocator, .{ .cell_drop = cell });
                    self.removeLiveCell(cell.cell);
                },
            }
        }

        // Recursively process the final expression
        const processed_final = try self.processExpr(final_expr);
        var new_final = processed_final;
        if (processed_final != final_expr) changed = true;

        var pending_idx = pending_binding_increfs.items.len;
        while (pending_idx > 0) {
            pending_idx -= 1;
            const pending = pending_binding_increfs.items[pending_idx];

            var rc_stmts = std.ArrayList(LirStmt).empty;
            defer rc_stmts.deinit(self.allocator);
            try self.emitBlockIncrefsForPattern(
                pending.pattern,
                region,
                &rc_stmts,
                stmts,
                0,
                processed_final,
            );

            if (rc_stmts.items.len == 0) continue;
            try stmt_buf.insertSlice(self.allocator, pending.insert_idx, rc_stmts.items);
            changed = true;
        }

        // Insert decrefs for refcounted symbols bound in this block that are never used
        {
            const before_len = stmt_buf.items.len;
            for (stmts, 0..) |stmt, stmt_index| {
                switch (stmt) {
                    .decl => |b| try self.emitBlockDecrefsForPattern(
                        b.pattern,
                        region,
                        &stmt_buf,
                        stmts,
                        stmt_index,
                        processed_final,
                    ),
                    .mutate, .cell_init, .cell_store, .cell_drop => {},
                }
            }
            if (stmt_buf.items.len > before_len) changed = true;
        }

        var tail_decref_stmts = std.ArrayList(LirStmt).empty;
        defer tail_decref_stmts.deinit(self.allocator);
        for (stmts, 0..) |stmt, stmt_index| {
            switch (stmt) {
                .decl => |b| try self.emitBlockTailDecrefsForPattern(
                    b.pattern,
                    processed_final,
                    region,
                    &tail_decref_stmts,
                    stmts,
                    stmt_index,
                ),
                .mutate, .cell_init, .cell_store, .cell_drop => {},
            }
        }
        if (tail_decref_stmts.items.len > 0) {
            new_final = try self.wrapExprWithTailStmts(new_final, result_layout, tail_decref_stmts.items, region);
            changed = true;
        }

        var tail_cell_stmts = std.ArrayList(LirStmt).empty;
        defer tail_cell_stmts.deinit(self.allocator);
        var live_cell_index = self.live_cells.items.len;
        while (live_cell_index > saved_live_cells_len) {
            live_cell_index -= 1;
            const live_cell = self.live_cells.items[live_cell_index];
            if (self.layoutNeedsRc(live_cell.layout_idx)) {
                try self.emitCellDecrefInto(live_cell.cell, live_cell.layout_idx, region, &tail_cell_stmts);
            }
            try tail_cell_stmts.append(self.allocator, .{ .cell_drop = .{
                .cell = live_cell.cell,
                .layout_idx = live_cell.layout_idx,
            } });
        }
        if (tail_cell_stmts.items.len > 0) {
            new_final = try self.wrapExprWithTailStmts(new_final, result_layout, tail_cell_stmts.items, region);
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

    /// Process an if-then-else expression.
    /// Each branch gets per-branch RC ops based on local use counts.
    fn processIfThenElse(
        self: *RcInsertPass,
        branches_span: LIR.LirIfBranchSpan,
        final_else_id: LirExprId,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        const live_len = self.live_rc_symbols.items.len;

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
            const processed_cond = try self.processExpr(branch.cond);
            self.scratch_uses.clearRetainingCapacity();
            try self.countUsesInto(branch.cond, &self.scratch_uses);
            const new_cond = try self.wrapExprWithLiveBorrowRcOps(
                processed_cond,
                live_len,
                &self.scratch_uses,
                .bool,
                region,
            );

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
                .cond = new_cond,
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

        const new_value = try self.processExpr(value);

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
            self.scratch_uses.clearRetainingCapacity();
            try self.countBorrowOwnerDemandValueInto(branch.body, &self.scratch_uses);
            const new_body = try self.wrapExprWithPatternBorrowRcOps(outer_rc_body, branch.pattern, &self.scratch_uses, result_layout, region);
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
            .value = new_value,
            .value_layout = value_layout,
            .branches = new_branch_span,
            .result_layout = result_layout,
        } }, region);
    }

    /// Process a discriminant_switch expression as branching control flow.
    /// Each branch is mutually exclusive, so symbols used in any branch
    /// get per-branch RC adjustments (same pattern as if_then_else/match).
    fn processDiscriminantSwitch(self: *RcInsertPass, ds: anytype, region: Region) Allocator.Error!LirExprId {
        const new_value = try self.processExpr(ds.value);

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
            .value = new_value,
            .union_layout = ds.union_layout,
            .branches = new_branches_span,
            .result_layout = ds.result_layout,
        } }, region);
    }

    fn processOwnedBodyWithParams(
        self: *RcInsertPass,
        body_expr_id: LirExprId,
        params_span: LirPatternSpan,
        ret_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        const saved_scope_base = self.early_return_scope_base;
        const saved_cell_scope_base = self.early_return_cell_scope_base;
        const saved_live_len = self.live_rc_symbols.items.len;
        const saved_live_cells_len = self.live_cells.items.len;
        self.early_return_scope_base = self.live_rc_symbols.items.len;
        self.early_return_cell_scope_base = self.live_cells.items.len;
        defer {
            self.early_return_scope_base = saved_scope_base;
            self.early_return_cell_scope_base = saved_cell_scope_base;
            self.live_rc_symbols.shrinkRetainingCapacity(saved_live_len);
            self.live_cells.shrinkRetainingCapacity(saved_live_cells_len);
        }

        var local_consumed_uses = std.AutoHashMap(u64, u32).init(self.allocator);
        defer local_consumed_uses.deinit();
        try self.countConsumedValueInto(body_expr_id, &local_consumed_uses);

        // Proc/lambda parameters are live bindings for the whole body and must be
        // included in early_return cleanup.
        const params = try self.allocator.dupe(LirPatternId, self.store.getPatternSpan(params_span));
        defer self.allocator.free(params);

        for (params) |pat_id| {
            try self.trackLiveRcSymbolsForPattern(pat_id, &local_consumed_uses);
        }

        const new_body = try self.processExpr(body_expr_id);

        var final_local_consumed_uses = std.AutoHashMap(u64, u32).init(self.allocator);
        defer final_local_consumed_uses.deinit();
        try self.countConsumedValueInto(new_body, &final_local_consumed_uses);

        // Emit pre-body RC ops for the parameters.
        var rc_stmts = std.ArrayList(LirStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        for (params) |pat_id| {
            try self.emitRcOpsForPatternInto(pat_id, &final_local_consumed_uses, region, &rc_stmts);
        }

        var final_body = new_body;
        if (rc_stmts.items.len > 0) {
            const stmts_span = try self.store.addStmts(rc_stmts.items);
            final_body = try self.store.addExpr(.{ .block = .{
                .stmts = stmts_span,
                .final_expr = new_body,
                .result_layout = ret_layout,
            } }, region);
        }

        // The original parameter reference must survive borrowed-only uses until
        // after the body result has been materialized.
        var tail_stmts = std.ArrayList(LirStmt).empty;
        defer tail_stmts.deinit(self.allocator);
        for (params) |pat_id| {
            try self.emitTailDecrefsForPatternInto(pat_id, &final_local_consumed_uses, region, &tail_stmts);
        }
        final_body = try self.wrapExprWithTailStmts(final_body, ret_layout, tail_stmts.items, region);

        return final_body;
    }

    /// Process a for loop expression.
    /// For loops bind an element via elem_pattern each iteration.
    /// The loop provides 1 reference per element. We emit body-local RC ops
    /// for the element binding similar to lambda params.
    fn processForLoop(self: *RcInsertPass, fl: anytype, region: Region, expr_id: LirExprId) Allocator.Error!LirExprId {
        var local_consumed_uses = std.AutoHashMap(u64, u32).init(self.allocator);
        defer local_consumed_uses.deinit();
        try self.countConsumedValueInto(fl.body, &local_consumed_uses);
        var local_demands = std.AutoHashMap(u64, u32).init(self.allocator);
        defer local_demands.deinit();
        try self.countBorrowOwnerDemandValueInto(fl.body, &local_demands);

        // The loop element binding is live while processing the body and must
        // be considered by early_return cleanup.
        const saved_live_len = self.live_rc_symbols.items.len;
        defer self.live_rc_symbols.shrinkRetainingCapacity(saved_live_len);
        try self.trackLiveRcSymbolsForPattern(fl.elem_pattern, &local_consumed_uses);

        var list_added = try self.pushBorrowedExprUsesToBlockConsumed(fl.list_expr);
        defer {
            self.popExprUsesFromBlockConsumed(&list_added);
            list_added.deinit();
        }
        const new_body = try self.processExpr(fl.body);

        // Emit pre-body RC ops for the elem_pattern.
        var rc_stmts = std.ArrayList(LirStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        try self.emitBorrowRcOpsForPatternInto(fl.elem_pattern, &local_demands, region, &rc_stmts);

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
        try self.emitTailDecrefsForPatternInto(fl.elem_pattern, &local_consumed_uses, region, &tail_stmts);
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
        var cond_demands = std.AutoHashMap(u64, u32).init(self.allocator);
        defer cond_demands.deinit();
        try self.countBorrowOwnerDemandUsesInto(wl.cond, &cond_demands);

        var cond_consumed = std.AutoHashMap(u64, u32).init(self.allocator);
        defer cond_consumed.deinit();
        try self.countConsumedUsesInto(wl.cond, &cond_consumed);

        var body_demands = std.AutoHashMap(u64, u32).init(self.allocator);
        defer body_demands.deinit();
        try self.countBorrowOwnerDemandUsesInto(wl.body, &body_demands);

        var body_consumed = std.AutoHashMap(u64, u32).init(self.allocator);
        defer body_consumed.deinit();
        try self.countConsumedUsesInto(wl.body, &body_consumed);

        const live_len = self.live_rc_symbols.items.len;

        var loop_carried_keys = std.AutoHashMap(u64, void).init(self.allocator);
        defer loop_carried_keys.deinit();
        for (self.live_rc_symbols.items[0..live_len]) |live| {
            if (!live.reassignable) continue;
            if (try self.exprMutatesSymbol(wl.body, live.symbol)) {
                try loop_carried_keys.put(live.key, {});
            }
        }

        const processed_cond = try self.processExpr(wl.cond);
        const new_cond = try self.wrapExprWithLiveDemandRcOps(
            processed_cond,
            live_len,
            &cond_demands,
            &cond_consumed,
            null,
            self.exprResultLayout(wl.cond),
            region,
        );

        var cond_added = try self.pushBorrowedExprUsesToBlockConsumed(wl.cond);
        defer {
            self.popExprUsesFromBlockConsumed(&cond_added);
            cond_added.deinit();
        }

        const processed_body = try self.processExpr(wl.body);
        const new_body = try self.wrapExprWithLiveDemandRcOps(
            processed_body,
            live_len,
            &body_demands,
            &body_consumed,
            &loop_carried_keys,
            self.exprResultLayout(wl.body),
            region,
        );

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
            const key = live.key;
            const ret_use_count = self.ownerUseCountFromMap(&self.scratch_consumed_uses, key);
            // Total refs = live owner refs for this binding + pending branch RC adjustments.
            // The branch wrapper will prepend RC ops that execute before
            // the early return: increfs (positive adj) add refs to clean up,
            // decrefs (negative adj) reduce refs since they're already handled.
            const branch_adj: i32 = self.pending_branch_rc_adj.get(key) orelse 0;
            const effective_signed: i32 = @as(i32, @intCast(live.owned_ref_count)) + branch_adj;
            if (effective_signed <= 0) continue; // branch wrapper decrefs handle all refs
            const effective_count: u32 = @intCast(effective_signed);
            // Include uses consumed by outer enclosing blocks (cumulative)
            // plus uses consumed by the current inner block's prior statements.
            const consumed_before = self.ownerUseCountFromMap(&self.cumulative_consumed_uses, key) +
                self.ownerUseCountFromMap(&self.block_consumed_uses, key);
            const total_consumed = consumed_before + ret_use_count;
            if (total_consumed >= effective_count) continue; // all refs accounted for
            const remaining = effective_count - total_consumed;
            var i: u32 = 0;
            while (i < remaining) : (i += 1) {
                try self.emitDecrefInto(live.symbol, live.layout_idx, region, &cleanup_stmts);
            }
        }

        const live_cells = self.live_cells.items[self.early_return_cell_scope_base..];
        var cell_index = live_cells.len;
        while (cell_index > 0) {
            cell_index -= 1;
            const live_cell = live_cells[cell_index];
            if (!self.layoutNeedsRc(live_cell.layout_idx)) continue;
            try self.emitCellDecrefInto(live_cell.cell, live_cell.layout_idx, region, &cleanup_stmts);
        }

        if (cleanup_stmts.items.len == 0 and new_expr == ret.expr) return expr_id;

        // Preserve the return value before cleanup so early-return RC cleanup
        // cannot free the value before it is actually evaluated.
        const ret_temp = try self.freshResultPattern(ret.ret_layout, region);
        const ret_lookup = try self.store.addExpr(.{ .lookup = .{
            .symbol = ret_temp.symbol,
            .layout_idx = ret.ret_layout,
        } }, region);

        var stmts = std.ArrayList(LirStmt).empty;
        defer stmts.deinit(self.allocator);
        try stmts.append(self.allocator, .{ .decl = .{
            .pattern = ret_temp.pattern,
            .expr = new_expr,
        } });
        try stmts.appendSlice(self.allocator, cleanup_stmts.items);

        const early_ret_id = try self.store.addExpr(.{ .early_return = .{
            .expr = ret_lookup,
            .ret_layout = ret.ret_layout,
        } }, region);

        const wildcard = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, region);
        try stmts.append(self.allocator, .{ .decl = .{
            .pattern = wildcard,
            .expr = early_ret_id,
        } });

        // The block's final_expr is never reached (early_return diverges),
        // but we need a distinct valid expr — not early_ret_id which is already used as a stmt.
        const dead_final = try self.store.addExpr(.{ .runtime_error = .{ .ret_layout = ret.ret_layout } }, region);
        return self.store.addExpr(.{ .block = .{
            .stmts = try self.store.addStmts(stmts.items),
            .final_expr = dead_final,
            .result_layout = ret.ret_layout,
        } }, region);
    }

    /// Collect all symbols bound by a pattern into a set.
    fn collectPatternSymbols(self: *const RcInsertPass, pat_id: LirPatternId, set: *std.AutoHashMap(u64, void)) Allocator.Error!void {
        const Ctx = struct {
            pass: *const RcInsertPass,
            set: *std.AutoHashMap(u64, void),
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, _: LayoutIdx, _: bool) Allocator.Error!void {
                try ctx.set.put(ctx.pass.patternKey(bind_pat_id, symbol), {});
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self, .set = set });
    }

    fn exprUsesPatternSymbol(self: *RcInsertPass, expr_id: LirExprId, pat_id: LirPatternId) Allocator.Error!bool {
        var pattern_symbols = std.AutoHashMap(u64, void).init(self.allocator);
        defer pattern_symbols.deinit();
        try self.collectPatternSymbols(pat_id, &pattern_symbols);
        if (pattern_symbols.count() == 0) return false;

        var shadowed = std.ArrayList(u64).empty;
        defer shadowed.deinit(self.allocator);
        var it_shadow = pattern_symbols.keyIterator();
        while (it_shadow.next()) |key_ptr| {
            var current = key_ptr.*;
            while (self.keyInfo(current)) |info| {
                if (info.shadowed_ref.isNone()) break;
                current = @intFromEnum(info.shadowed_ref);
                if (!pattern_symbols.contains(current)) {
                    try shadowed.append(self.allocator, current);
                }
            }
        }
        for (shadowed.items) |shadow_key| {
            try pattern_symbols.put(shadow_key, {});
        }

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

    fn exprNeedsTailCleanupKey(self: *RcInsertPass, expr_id: LirExprId, key: u64, symbol: Symbol) Allocator.Error!bool {
        if (expr_id.isNone()) return false;

        return (try self.exprUsesKey(expr_id, key)) or (try self.exprUsesSymbol(expr_id, symbol));
    }

    fn exprUsesKey(self: *RcInsertPass, expr_id: LirExprId, key: u64) Allocator.Error!bool {
        self.scratch_uses.clearRetainingCapacity();
        defer self.scratch_uses.clearRetainingCapacity();
        try self.countUsesInto(expr_id, &self.scratch_uses);
        return (self.scratch_uses.get(key) orelse 0) > 0;
    }

    fn exprUsesSymbol(self: *RcInsertPass, expr_id: LirExprId, symbol: Symbol) Allocator.Error!bool {
        return self.exprUsesKey(expr_id, @as(u64, @bitCast(symbol)));
    }

    fn patternBindsSymbol(self: *const RcInsertPass, pat_id: LirPatternId, symbol: Symbol) Allocator.Error!bool {
        var found = false;
        const Ctx = struct {
            found: *bool,
            wanted: Symbol,
            fn onBind(ctx: @This(), _: LirPatternId, bind_symbol: Symbol, _: LayoutIdx, _: bool) Allocator.Error!void {
                if (bind_symbol == ctx.wanted) {
                    ctx.found.* = true;
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{
            .found = &found,
            .wanted = symbol,
        });
        return found;
    }

    fn exprMutatesSymbol(self: *const RcInsertPass, expr_id: LirExprId, symbol: Symbol) Allocator.Error!bool {
        if (expr_id.isNone()) return false;

        const expr = self.store.getExpr(expr_id);
        switch (expr) {
            .block => |block| {
                for (self.store.getStmts(block.stmts)) |stmt| {
                    switch (stmt) {
                        .decl => |binding| {
                            if (try self.exprMutatesSymbol(binding.expr, symbol)) return true;
                        },
                        .mutate => |binding| {
                            if (try self.patternBindsSymbol(binding.pattern, symbol)) return true;
                            if (try self.exprMutatesSymbol(binding.expr, symbol)) return true;
                        },
                        .cell_init, .cell_store => |binding| {
                            if (try self.exprMutatesSymbol(binding.expr, symbol)) return true;
                        },
                        .cell_drop => {},
                    }
                }
                return self.exprMutatesSymbol(block.final_expr, symbol);
            },
            .if_then_else => |ite| {
                for (self.store.getIfBranches(ite.branches)) |branch| {
                    if (try self.exprMutatesSymbol(branch.cond, symbol)) return true;
                    if (try self.exprMutatesSymbol(branch.body, symbol)) return true;
                }
                return self.exprMutatesSymbol(ite.final_else, symbol);
            },
            .match_expr => |match_expr| {
                if (try self.exprMutatesSymbol(match_expr.value, symbol)) return true;
                for (self.store.getMatchBranches(match_expr.branches)) |branch| {
                    if (try self.exprMutatesSymbol(branch.guard, symbol)) return true;
                    if (try self.exprMutatesSymbol(branch.body, symbol)) return true;
                }
                return false;
            },
            .for_loop => |for_loop| {
                if (try self.exprMutatesSymbol(for_loop.list_expr, symbol)) return true;
                return self.exprMutatesSymbol(for_loop.body, symbol);
            },
            .while_loop => |while_loop| {
                if (try self.exprMutatesSymbol(while_loop.cond, symbol)) return true;
                return self.exprMutatesSymbol(while_loop.body, symbol);
            },
            .discriminant_switch => |switch_expr| {
                if (try self.exprMutatesSymbol(switch_expr.value, symbol)) return true;
                for (self.store.getExprSpan(switch_expr.branches)) |branch_id| {
                    if (try self.exprMutatesSymbol(branch_id, symbol)) return true;
                }
                return false;
            },
            .expect => |expect_expr| {
                if (try self.exprMutatesSymbol(expect_expr.cond, symbol)) return true;
                return self.exprMutatesSymbol(expect_expr.body, symbol);
            },
            .proc_call => |call| {
                for (self.store.getExprSpan(call.args)) |arg_id| {
                    if (try self.exprMutatesSymbol(arg_id, symbol)) return true;
                }
                return false;
            },
            .low_level => |low_level| {
                for (self.store.getExprSpan(low_level.args)) |arg_id| {
                    if (try self.exprMutatesSymbol(arg_id, symbol)) return true;
                }
                return false;
            },
            .hosted_call => |hosted_call| {
                for (self.store.getExprSpan(hosted_call.args)) |arg_id| {
                    if (try self.exprMutatesSymbol(arg_id, symbol)) return true;
                }
                return false;
            },
            .list => |list_expr| {
                for (self.store.getExprSpan(list_expr.elems)) |elem_id| {
                    if (try self.exprMutatesSymbol(elem_id, symbol)) return true;
                }
                return false;
            },
            .struct_ => |struct_expr| {
                for (self.store.getExprSpan(struct_expr.fields)) |field_id| {
                    if (try self.exprMutatesSymbol(field_id, symbol)) return true;
                }
                return false;
            },
            .tag => |tag_expr| {
                for (self.store.getExprSpan(tag_expr.args)) |arg_id| {
                    if (try self.exprMutatesSymbol(arg_id, symbol)) return true;
                }
                return false;
            },
            .str_concat => |parts| {
                for (self.store.getExprSpan(parts)) |part_id| {
                    if (try self.exprMutatesSymbol(part_id, symbol)) return true;
                }
                return false;
            },
            .struct_access => |sa| return self.exprMutatesSymbol(sa.struct_expr, symbol),
            .nominal => |nominal| return self.exprMutatesSymbol(nominal.backing_expr, symbol),
            .early_return => |ret| return self.exprMutatesSymbol(ret.expr, symbol),
            .dbg => |dbg_expr| return self.exprMutatesSymbol(dbg_expr.expr, symbol),
            .int_to_str => |its| return self.exprMutatesSymbol(its.value, symbol),
            .float_to_str => |fts| return self.exprMutatesSymbol(fts.value, symbol),
            .dec_to_str => |dec_expr| return self.exprMutatesSymbol(dec_expr, symbol),
            .str_escape_and_quote => |str_expr| return self.exprMutatesSymbol(str_expr, symbol),
            .tag_payload_access => |payload_access| return self.exprMutatesSymbol(payload_access.value, symbol),
            .i64_literal,
            .i128_literal,
            .f64_literal,
            .f32_literal,
            .dec_literal,
            .str_literal,
            .bool_literal,
            .lookup,
            .cell_load,
            .empty_list,
            .zero_arg_tag,
            .break_expr,
            .crash,
            .runtime_error,
            .incref,
            .decref,
            .free,
            => return false,
        }
    }

    fn exprConsumesKey(self: *RcInsertPass, expr_id: LirExprId, key: u64) Allocator.Error!bool {
        self.scratch_consumed_uses.clearRetainingCapacity();
        defer self.scratch_consumed_uses.clearRetainingCapacity();
        try self.countConsumedValueInto(expr_id, &self.scratch_consumed_uses);
        return (self.scratch_consumed_uses.get(key) orelse 0) > 0;
    }

    /// Collect all symbols bound by patterns within an expression tree.
    /// Used to identify locally-defined symbols for branch-aware RC filtering.
    fn collectExprBoundSymbols(self: *const RcInsertPass, expr_id: LirExprId, set: *std.AutoHashMap(u64, void)) Allocator.Error!void {
        if (expr_id.isNone()) return;
        const expr = self.store.getExpr(expr_id);
        switch (expr) {
            .cell_load => {},
            .block => |block| {
                const stmts = self.store.getStmts(block.stmts);
                for (stmts) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |b| {
                            try self.collectPatternSymbols(b.pattern, set);
                            try self.collectExprBoundSymbols(b.expr, set);
                        },
                        .cell_init, .cell_store => |b| try self.collectExprBoundSymbols(b.expr, set),
                        .cell_drop => {},
                    }
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
            .proc_call => |call| {
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
            const symbol = self.keySymbol(key, Symbol.none);
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
            const symbol = self.keySymbol(key, Symbol.none);
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
    /// Match-pattern bindings borrow from the scrutinee, so each owned-demanding
    /// use requires one explicit retained owner.
    fn wrapExprWithPatternBorrowRcOps(
        self: *RcInsertPass,
        expr: LirExprId,
        pat_id: LirPatternId,
        local_demands: *const std.AutoHashMap(u64, u32),
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        var rc_stmts = std.ArrayList(LirStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        try self.emitBorrowRcOpsForPatternInto(pat_id, local_demands, region, &rc_stmts);
        if (rc_stmts.items.len == 0) return expr;

        const stmts_span = try self.store.addStmts(rc_stmts.items);
        return self.store.addExpr(.{ .block = .{
            .stmts = stmts_span,
            .final_expr = expr,
            .result_layout = result_layout,
        } }, region);
    }

    fn wrapExprWithLiveBorrowRcOps(
        self: *RcInsertPass,
        expr: LirExprId,
        live_len: usize,
        local_demands: *const std.AutoHashMap(u64, u32),
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        var rc_stmts = std.ArrayList(LirStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        try self.emitBorrowRcOpsForLiveSymbolsInto(live_len, local_demands, region, &rc_stmts);
        return self.wrapPreludeAroundExpr(expr, result_layout, region, rc_stmts.items);
    }

    fn wrapExprWithLiveDemandRcOps(
        self: *RcInsertPass,
        expr: LirExprId,
        live_len: usize,
        local_demands: *const std.AutoHashMap(u64, u32),
        local_consumed: *const std.AutoHashMap(u64, u32),
        loop_carried_keys: ?*const std.AutoHashMap(u64, void),
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        var prelude_stmts = std.ArrayList(LirStmt).empty;
        defer prelude_stmts.deinit(self.allocator);
        try self.emitLiveDemandRcOpsForSymbolsInto(live_len, local_demands, local_consumed, loop_carried_keys, region, &prelude_stmts);

        var tail_stmts = std.ArrayList(LirStmt).empty;
        defer tail_stmts.deinit(self.allocator);
        try self.emitLiveDemandRcTailDecrefsForSymbolsInto(live_len, local_demands, local_consumed, region, &tail_stmts);

        const with_prelude = try self.wrapPreludeAroundExpr(expr, result_layout, region, prelude_stmts.items);
        return self.wrapExprWithTailStmts(with_prelude, result_layout, tail_stmts.items, region);
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
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                const resolved_layout = ctx.pass.keyLayout(key, layout_idx);
                if (!ctx.pass.keyIntroducesOwner(key)) return;
                if (ctx.pass.layoutNeedsRc(resolved_layout)) {
                    try ctx.pass.emitDecrefInto(ctx.pass.keySymbol(key, symbol), resolved_layout, ctx.region, ctx.stmts);
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
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, _: LayoutIdx, _: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                const previous_key = if (ctx.pass.keyInfo(key)) |info|
                    if (!info.shadowed_ref.isNone()) @intFromEnum(info.shadowed_ref) else key
                else
                    key;
                if (!ctx.pass.keyIntroducesOwner(previous_key)) return;
                const previous_layout = ctx.prior_decl_layouts.get(previous_key) orelse return;
                // If prior statements already consumed this symbol, shadowing should not
                // decref the old binding again.
                if ((ctx.pass.block_consumed_uses.get(previous_key) orelse 0) > 0) return;
                if (ctx.pass.layoutNeedsRc(previous_layout)) {
                    try ctx.pass.emitDecrefInto(ctx.pass.keySymbol(previous_key, symbol), previous_layout, ctx.region, ctx.stmts);
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
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                if (!ctx.pass.keyIntroducesOwner(key)) return;
                if (!ctx.prior_decl_layouts.contains(key)) return; // not a shadowing bind

                const resolved_layout = ctx.pass.keyLayout(key, layout_idx);
                if (!ctx.pass.layoutNeedsRc(resolved_layout)) return;
                if (try ctx.pass.hasLaterBlockUseKey(ctx.block_stmts, ctx.stmt_index, ctx.final_expr, key)) return;

                const consumed = ctx.pass.block_consumed_uses.get(key) orelse 0;
                const total = ctx.pass.effectiveGlobalOwnerUseCount(key);
                if (consumed >= total) {
                    try ctx.pass.emitDecrefInto(ctx.pass.keySymbol(key, symbol), resolved_layout, ctx.region, ctx.stmts);
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
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                const resolved_layout = ctx.pass.keyLayout(key, layout_idx);
                try ctx.target.put(key, resolved_layout);
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self, .target = target });
    }

    /// Recursively track live RC symbols for early_return cleanup.
    fn trackLiveRcSymbolsForPattern(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        local_uses: ?*const std.AutoHashMap(u64, u32),
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            local_uses: ?*const std.AutoHashMap(u64, u32),
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, reassignable: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                const resolved_layout = ctx.pass.keyLayout(key, layout_idx);
                const resolved_reassignable = ctx.pass.keyReassignable(key, reassignable);
                if (!ctx.pass.keyIntroducesOwner(key)) return;
                const owned_ref_count = if (ctx.local_uses) |uses|
                    ctx.pass.liveOwnedRefCountFromLocalUses(key, resolved_reassignable, uses)
                else
                    ctx.pass.liveOwnedRefCountForKey(key, resolved_reassignable);
                if (ctx.pass.layoutNeedsRc(resolved_layout)) {
                    for (ctx.pass.live_rc_symbols.items) |*live| {
                        if (live.key == key) {
                            live.layout_idx = resolved_layout;
                            live.reassignable = resolved_reassignable;
                            live.owned_ref_count = owned_ref_count;
                            return;
                        }
                    }
                    try ctx.pass.live_rc_symbols.append(ctx.pass.allocator, .{
                        .key = key,
                        .symbol = ctx.pass.keySymbol(key, symbol),
                        .layout_idx = resolved_layout,
                        .reassignable = resolved_reassignable,
                        .owned_ref_count = owned_ref_count,
                    });
                }
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{ .pass = self, .local_uses = local_uses });
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
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, reassignable: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                const resolved_layout = ctx.pass.keyLayout(key, layout_idx);
                const resolved_reassignable = ctx.pass.keyReassignable(key, reassignable);
                if (!ctx.pass.keyIntroducesOwner(key)) return;
                if (ctx.pass.layoutNeedsRc(resolved_layout)) {
                    if (resolved_reassignable) return;
                    const use_count = ctx.pass.effectiveGlobalOwnerUseCount(key);
                    if (use_count > 1) {
                        try ctx.pass.emitIncrefInto(ctx.pass.keySymbol(key, symbol), resolved_layout, @intCast(use_count - 1), ctx.region, ctx.rc_stmts);
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
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, reassignable: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                const resolved_layout = ctx.pass.keyLayout(key, layout_idx);
                const resolved_reassignable = ctx.pass.keyReassignable(key, reassignable);
                if (!ctx.pass.keyIntroducesOwner(key)) return;
                if (!ctx.pass.layoutNeedsRc(resolved_layout)) return;
                if (!resolved_reassignable and try ctx.pass.hasLaterShadowingDecl(ctx.block_stmts, ctx.stmt_index, symbol)) return;

                if (resolved_reassignable) {
                    if (try ctx.pass.exprUsesKey(ctx.final_expr, key)) return;
                    try ctx.pass.emitDecrefInto(ctx.pass.keySymbol(key, symbol), resolved_layout, ctx.region, ctx.stmts);
                    return;
                }

                const use_count = ctx.pass.effectiveGlobalOwnerUseCount(key);
                if (use_count == 0) {
                    const final_reads_symbol = try ctx.pass.exprUsesKey(ctx.final_expr, key);
                    if (final_reads_symbol) return;
                    try ctx.pass.emitDecrefInto(ctx.pass.keySymbol(key, symbol), resolved_layout, ctx.region, ctx.stmts);
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
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, reassignable: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                const resolved_layout = ctx.pass.keyLayout(key, layout_idx);
                const resolved_reassignable = ctx.pass.keyReassignable(key, reassignable);
                if (!ctx.pass.keyIntroducesOwner(key)) return;
                if (!ctx.pass.layoutNeedsRc(resolved_layout)) return;
                if (!resolved_reassignable and try ctx.pass.hasLaterShadowingDecl(ctx.block_stmts, ctx.stmt_index, symbol)) return;
                const final_reads_symbol = try ctx.pass.exprNeedsTailCleanupKey(ctx.final_expr, key, symbol);
                if (!final_reads_symbol) return;
                if (resolved_reassignable) {
                    if (try ctx.pass.exprConsumesKey(ctx.final_expr, key)) return;
                    try ctx.pass.emitDecrefInto(ctx.pass.keySymbol(key, symbol), resolved_layout, ctx.region, ctx.stmts);
                    return;
                }
                const use_count = ctx.pass.effectiveGlobalOwnerUseCount(key);
                if (use_count != 0) return;
                try ctx.pass.emitDecrefInto(ctx.pass.keySymbol(key, symbol), resolved_layout, ctx.region, ctx.stmts);
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
            fn onBind(ctx: @This(), _: LirPatternId, bind_symbol: Symbol, _: LayoutIdx, _: bool) Allocator.Error!void {
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

    fn hasLaterBlockUseKey(
        self: *RcInsertPass,
        block_stmts: []const LirStmt,
        stmt_index: usize,
        final_expr: LirExprId,
        key: u64,
    ) Allocator.Error!bool {
        if (stmt_index + 1 < block_stmts.len) {
            for (block_stmts[stmt_index + 1 ..]) |later_stmt| {
                switch (later_stmt) {
                    .decl, .mutate => |binding| {
                        if (try self.exprUsesKey(binding.expr, key)) return true;
                    },
                    .cell_init, .cell_store => |binding| {
                        if (try self.exprUsesKey(binding.expr, key)) return true;
                    },
                    .cell_drop => {},
                }
            }
        }

        return self.exprUsesKey(final_expr, key);
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
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                const resolved_layout = ctx.pass.keyLayout(key, layout_idx);
                if (!ctx.pass.keyIntroducesOwner(key)) return;
                if (ctx.pass.layoutNeedsRc(resolved_layout)) {
                    const use_count = ctx.pass.ownerUseCountFromMap(ctx.local_uses, key);
                    if (use_count > 1) {
                        try ctx.pass.emitIncrefInto(ctx.pass.keySymbol(key, symbol), resolved_layout, @intCast(use_count - 1), ctx.region, ctx.rc_stmts);
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
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                const resolved_layout = ctx.pass.keyLayout(key, layout_idx);
                if (!ctx.pass.keyIntroducesOwner(key)) return;
                if (!ctx.pass.layoutNeedsRc(resolved_layout)) return;
                const use_count = ctx.pass.ownerUseCountFromMap(ctx.local_uses, key);
                if (use_count == 0) {
                    try ctx.pass.emitDecrefInto(ctx.pass.keySymbol(key, symbol), resolved_layout, ctx.region, ctx.rc_stmts);
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
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                const resolved_layout = ctx.pass.keyLayout(key, layout_idx);
                if (ctx.pass.layoutNeedsRc(resolved_layout)) {
                    const use_count = ctx.local_uses.get(key) orelse 0;
                    if (use_count > 0) {
                        try ctx.pass.emitIncrefInto(ctx.pass.keySymbol(key, symbol), resolved_layout, @intCast(use_count), ctx.region, ctx.rc_stmts);
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

    fn emitBorrowRcOpsForLiveSymbolsInto(
        self: *RcInsertPass,
        live_len: usize,
        local_uses: *const std.AutoHashMap(u64, u32),
        region: Region,
        rc_stmts: *std.ArrayList(LirStmt),
    ) Allocator.Error!void {
        const keys_start = self.scratch_keys.top();
        defer self.scratch_keys.clearFrom(keys_start);

        for (self.live_rc_symbols.items[0..live_len]) |live| {
            if (self.ownerUseCountFromMap(local_uses, live.key) == 0) continue;
            try self.scratch_keys.append(live.key);
        }

        const sorted_keys = self.scratch_keys.sliceFromStart(keys_start);
        std.mem.sort(u64, sorted_keys, {}, std.sort.asc(u64));

        for (sorted_keys) |key| {
            const use_count = self.ownerUseCountFromMap(local_uses, key);
            if (use_count == 0) continue;

            for (self.live_rc_symbols.items[0..live_len]) |live| {
                if (live.key != key) continue;
                try self.emitIncrefInto(live.symbol, live.layout_idx, @intCast(use_count), region, rc_stmts);
                break;
            }
        }
    }

    fn emitLiveDemandRcOpsForSymbolsInto(
        self: *RcInsertPass,
        live_len: usize,
        local_demands: *const std.AutoHashMap(u64, u32),
        local_consumed: *const std.AutoHashMap(u64, u32),
        loop_carried_keys: ?*const std.AutoHashMap(u64, void),
        region: Region,
        rc_stmts: *std.ArrayList(LirStmt),
    ) Allocator.Error!void {
        const keys_start = self.scratch_keys.top();
        defer self.scratch_keys.clearFrom(keys_start);

        for (self.live_rc_symbols.items[0..live_len]) |live| {
            const demand_count = self.ownerUseCountFromMap(local_demands, live.key);
            const consumed_count = self.ownerUseCountFromMap(local_consumed, live.key);
            const is_loop_carried = if (loop_carried_keys) |keys| keys.contains(live.key) else false;
            const preserve_count: u32 = if (is_loop_carried)
                if (consumed_count > 0) consumed_count - 1 else 0
            else
                consumed_count;
            const borrow_only_count: u32 = if (demand_count > consumed_count)
                demand_count - consumed_count
            else
                0;

            if (preserve_count == 0 and borrow_only_count == 0) continue;
            try self.scratch_keys.append(live.key);
        }

        const sorted_keys = self.scratch_keys.sliceFromStart(keys_start);
        std.mem.sort(u64, sorted_keys, {}, std.sort.asc(u64));

        for (sorted_keys) |key| {
            const demand_count = self.ownerUseCountFromMap(local_demands, key);
            const consumed_count = self.ownerUseCountFromMap(local_consumed, key);
            const is_loop_carried = if (loop_carried_keys) |keys| keys.contains(key) else false;
            const preserve_count: u32 = if (is_loop_carried)
                if (consumed_count > 0) consumed_count - 1 else 0
            else
                consumed_count;
            const borrow_only_count: u32 = if (demand_count > consumed_count)
                demand_count - consumed_count
            else
                0;
            const emit_count = preserve_count + borrow_only_count;
            if (emit_count == 0) continue;

            for (self.live_rc_symbols.items[0..live_len]) |live| {
                if (live.key != key) continue;
                try self.emitIncrefInto(live.symbol, live.layout_idx, @intCast(emit_count), region, rc_stmts);
                break;
            }
        }
    }

    fn emitLiveDemandRcTailDecrefsForSymbolsInto(
        self: *RcInsertPass,
        live_len: usize,
        local_demands: *const std.AutoHashMap(u64, u32),
        local_consumed: *const std.AutoHashMap(u64, u32),
        region: Region,
        rc_stmts: *std.ArrayList(LirStmt),
    ) Allocator.Error!void {
        const keys_start = self.scratch_keys.top();
        defer self.scratch_keys.clearFrom(keys_start);

        for (self.live_rc_symbols.items[0..live_len]) |live| {
            const demand_count = self.ownerUseCountFromMap(local_demands, live.key);
            const consumed_count = self.ownerUseCountFromMap(local_consumed, live.key);
            const borrow_only_count: u32 = if (demand_count > consumed_count)
                demand_count - consumed_count
            else
                0;
            if (borrow_only_count == 0) continue;
            try self.scratch_keys.append(live.key);
        }

        const sorted_keys = self.scratch_keys.sliceFromStart(keys_start);
        std.mem.sort(u64, sorted_keys, {}, std.sort.asc(u64));

        for (sorted_keys) |key| {
            const demand_count = self.ownerUseCountFromMap(local_demands, key);
            const consumed_count = self.ownerUseCountFromMap(local_consumed, key);
            const emit_count: u32 = if (demand_count > consumed_count)
                demand_count - consumed_count
            else
                0;
            if (emit_count == 0) continue;

            for (self.live_rc_symbols.items[0..live_len]) |live| {
                if (live.key != key) continue;
                try self.emitDecrefCountInto(live.symbol, live.layout_idx, emit_count, region, rc_stmts);
                break;
            }
        }
    }

    fn emitRetainedIncrefsForPattern(
        self: *RcInsertPass,
        pat_id: LirPatternId,
        region: Region,
        rc_stmts: *std.ArrayList(LirStmt),
    ) Allocator.Error!void {
        const Ctx = struct {
            pass: *RcInsertPass,
            region: Region,
            rc_stmts: *std.ArrayList(LirStmt),
            fn onBind(ctx: @This(), bind_pat_id: LirPatternId, symbol: Symbol, layout_idx: LayoutIdx, _: bool) Allocator.Error!void {
                const key = ctx.pass.patternKey(bind_pat_id, symbol);
                const resolved_layout = ctx.pass.keyLayout(key, layout_idx);
                if (!ctx.pass.keyIntroducesOwner(key)) return;
                if (!ctx.pass.layoutNeedsRc(resolved_layout)) return;
                try ctx.pass.emitIncrefInto(ctx.pass.keySymbol(key, symbol), resolved_layout, 1, ctx.region, ctx.rc_stmts);
            }
        };
        try walkPatternBinds(self.store, pat_id, Ctx{
            .pass = self,
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

        const wildcard = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, region);
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

        const wildcard = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, region);
        try stmts.append(self.allocator, .{ .decl = .{
            .pattern = wildcard,
            .expr = decref_id,
        } });
    }

    fn emitDecrefCountInto(
        self: *RcInsertPass,
        symbol: Symbol,
        layout_idx: LayoutIdx,
        count: u32,
        region: Region,
        stmts: *std.ArrayList(LirStmt),
    ) Allocator.Error!void {
        var i: u32 = 0;
        while (i < count) : (i += 1) {
            try self.emitDecrefInto(symbol, layout_idx, region, stmts);
        }
    }

    fn emitCellDecrefInto(self: *RcInsertPass, cell: Symbol, layout_idx: LayoutIdx, region: Region, stmts: *std.ArrayList(LirStmt)) Allocator.Error!void {
        const cell_load_id = try self.store.addExpr(.{ .cell_load = .{
            .cell = cell,
            .layout_idx = layout_idx,
        } }, region);

        const decref_id = try self.store.addExpr(.{ .decref = .{
            .value = cell_load_id,
            .layout_idx = layout_idx,
        } }, region);

        const wildcard = try self.store.addPattern(.{ .wildcard = .{ .layout_idx = layout_idx } }, region);
        try stmts.append(self.allocator, .{ .decl = .{
            .pattern = wildcard,
            .expr = decref_id,
        } });
    }
};

/// Lifted LIR procedures must go through the same RC insertion pass as the
/// entry/root expression. Callers that only annotate entrypoints will miss the
/// real function bodies for most compiled apps. This must stay scoped to
/// self-contained callable bodies; local alias defs can depend on ambient
/// bindings and cannot be rewritten correctly out of context.
pub fn insertRcOpsIntoSymbolDefsBestEffort(
    allocator: Allocator,
    store: *LirExprStore,
    layout_store: *const layout_mod.Store,
) void {
    var def_iter = store.symbol_defs.iterator();
    while (def_iter.next()) |entry| {
        var fn_rc = RcInsertPass.init(allocator, store, layout_store) catch continue;
        defer fn_rc.deinit();
        entry.value_ptr.* = fn_rc.insertRcOps(entry.value_ptr.*) catch entry.value_ptr.*;
    }
}

test "RcInsertPass compiles" {
    const T = RcInsertPass;
    std.debug.assert(@sizeOf(T) > 0);
}

fn firstBlockBindSymbol(store: *const LirExprStore, expr_id: LirExprId) ?Symbol {
    if (expr_id.isNone()) return null;
    const expr = store.getExpr(expr_id);
    if (expr != .block) return null;

    for (store.getStmts(expr.block.stmts)) |stmt| {
        switch (stmt) {
            .decl => |binding| {
                const pattern = store.getPattern(binding.pattern);
                if (pattern == .bind) return pattern.bind.symbol;
            },
            .mutate, .cell_init, .cell_store, .cell_drop => {},
        }
    }

    return null;
}

fn firstRcWildcardLayout(store: *const LirExprStore, expr_id: LirExprId) ?LayoutIdx {
    if (expr_id.isNone()) return null;

    const expr = store.getExpr(expr_id);
    switch (expr) {
        .block => |block| {
            for (store.getStmts(block.stmts)) |stmt| {
                switch (stmt) {
                    .decl => |binding| {
                        const binding_expr = store.getExpr(binding.expr);
                        if (binding_expr == .incref or binding_expr == .decref or binding_expr == .free) {
                            const pattern = store.getPattern(binding.pattern);
                            if (pattern == .wildcard) return pattern.wildcard.layout_idx;
                        }

                        if (firstRcWildcardLayout(store, binding.expr)) |layout_idx| return layout_idx;
                    },
                    .mutate => |binding| {
                        if (firstRcWildcardLayout(store, binding.expr)) |layout_idx| return layout_idx;
                    },
                    .cell_init, .cell_store => |binding| {
                        if (firstRcWildcardLayout(store, binding.expr)) |layout_idx| return layout_idx;
                    },
                    .cell_drop => {},
                }
            }

            return firstRcWildcardLayout(store, block.final_expr);
        },
        else => return null,
    }
}

fn findFirstLowLevelExpr(store: *const LirExprStore, expr_id: LirExprId) ?LirExprId {
    if (expr_id.isNone()) return null;

    const expr = store.getExpr(expr_id);
    switch (expr) {
        .low_level => return expr_id,
        .block => |block| {
            for (store.getStmts(block.stmts)) |stmt| {
                switch (stmt) {
                    .decl, .mutate => |binding| {
                        if (findFirstLowLevelExpr(store, binding.expr)) |found| return found;
                    },
                    .cell_init, .cell_store => |binding| {
                        if (findFirstLowLevelExpr(store, binding.expr)) |found| return found;
                    },
                    .cell_drop => {},
                }
            }

            return findFirstLowLevelExpr(store, block.final_expr);
        },
        else => return null,
    }
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

test "RC: unused list binding gets decref" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_list = makeSymbol(1);

    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());
    const elems = try env.lir_store.addExprSpan(&.{ one, two });
    const list_expr = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());
    const pat_list = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_list, .expr = list_expr } }});

    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = i64_layout } }, Region.zero());
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = zero,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_list));
}

test "RC: borrowed list statement keeps later block decref" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_list = makeSymbol(1);
    const sym_len = makeSymbol(2);

    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());
    const elems = try env.lir_store.addExprSpan(&.{ one, two });
    const list_expr = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());

    const lookup_list = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const len_args = try env.lir_store.addExprSpan(&.{lookup_list});
    const len_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .list_len,
        .args = len_args,
        .ret_layout = i64_layout,
    } }, Region.zero());
    const lookup_len = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_len, .layout_idx = i64_layout } }, Region.zero());

    const pat_list = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const pat_len = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_len, .layout_idx = i64_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_list, .expr = list_expr } },
        .{ .decl = .{ .pattern = pat_len, .expr = len_expr } },
    });
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup_len,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_list));
}

test "RC: consuming stmt preserves list owner for later list_get_unsafe" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_list = makeSymbol(1);
    const sym_arg = makeSymbol(2);

    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = i64_layout } }, Region.zero());
    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());

    const elems = try env.lir_store.addExprSpan(&.{ one, two });
    const list_expr = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());

    const callee_proc = try makeProc(&env.lir_store, sym_arg, .bool);
    const lookup_list_call = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const call_args = try env.lir_store.addExprSpan(&.{lookup_list_call});
    const call_expr = try env.lir_store.addExpr(.{ .proc_call = .{
        .proc = callee_proc,
        .args = call_args,
        .ret_layout = .bool,
        .called_via = .apply,
    } }, Region.zero());

    const lookup_list_first = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const first_args = try env.lir_store.addExprSpan(&.{ lookup_list_first, zero });
    const first_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .list_get_unsafe,
        .args = first_args,
        .ret_layout = i64_layout,
    } }, Region.zero());

    const pat_list = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const wild_bool = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = .bool } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_list, .expr = list_expr } },
        .{ .decl = .{ .pattern = wild_bool, .expr = call_expr } },
    });
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = first_expr,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), countIncrefsForSymbol(&env.lir_store, result, sym_list));
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
        .proc_call => |c| {
            const args = store.getExprSpan(c.args);
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
        .cell_load,
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
        .for_loop => |fl| countDecrefsForSymbol(store, fl.list_expr, symbol) + countDecrefsForSymbol(store, fl.body, symbol),
        .while_loop => |wl| countDecrefsForSymbol(store, wl.cond, symbol) + countDecrefsForSymbol(store, wl.body, symbol),
        .discriminant_switch => |ds| blk: {
            var total: u32 = countDecrefsForSymbol(store, ds.value, symbol);
            for (store.getExprSpan(ds.branches)) |branch_id| {
                total += countDecrefsForSymbol(store, branch_id, symbol);
            }
            break :blk total;
        },
        .proc_call => |call| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(call.args)) |arg_id| {
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

fn countIncrefsForSymbol(store: *const LirExprStore, expr_id: LirExprId, symbol: LIR.Symbol) u32 {
    if (expr_id.isNone()) return 0;

    const key = @as(u64, @bitCast(symbol));
    const expr = store.getExpr(expr_id);
    return switch (expr) {
        .block => |block| blk: {
            var total: u32 = 0;
            for (store.getStmts(block.stmts)) |stmt| {
                total += countIncrefsForSymbol(store, stmt.binding().expr, symbol);
            }
            total += countIncrefsForSymbol(store, block.final_expr, symbol);
            break :blk total;
        },
        .if_then_else => |ite| blk: {
            var total: u32 = 0;
            for (store.getIfBranches(ite.branches)) |branch| {
                total += countIncrefsForSymbol(store, branch.cond, symbol);
                total += countIncrefsForSymbol(store, branch.body, symbol);
            }
            total += countIncrefsForSymbol(store, ite.final_else, symbol);
            break :blk total;
        },
        .match_expr => |m| blk: {
            var total: u32 = countIncrefsForSymbol(store, m.value, symbol);
            for (store.getMatchBranches(m.branches)) |branch| {
                total += countIncrefsForSymbol(store, branch.guard, symbol);
                total += countIncrefsForSymbol(store, branch.body, symbol);
            }
            break :blk total;
        },
        .for_loop => |fl| countIncrefsForSymbol(store, fl.list_expr, symbol) + countIncrefsForSymbol(store, fl.body, symbol),
        .while_loop => |wl| countIncrefsForSymbol(store, wl.cond, symbol) + countIncrefsForSymbol(store, wl.body, symbol),
        .discriminant_switch => |ds| blk: {
            var total: u32 = countIncrefsForSymbol(store, ds.value, symbol);
            for (store.getExprSpan(ds.branches)) |branch_id| {
                total += countIncrefsForSymbol(store, branch_id, symbol);
            }
            break :blk total;
        },
        .proc_call => |call| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(call.args)) |arg_id| {
                total += countIncrefsForSymbol(store, arg_id, symbol);
            }
            break :blk total;
        },
        .low_level => |ll| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(ll.args)) |arg_id| {
                total += countIncrefsForSymbol(store, arg_id, symbol);
            }
            break :blk total;
        },
        .list => |l| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(l.elems)) |elem_id| {
                total += countIncrefsForSymbol(store, elem_id, symbol);
            }
            break :blk total;
        },
        .struct_ => |s| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(s.fields)) |field_id| {
                total += countIncrefsForSymbol(store, field_id, symbol);
            }
            break :blk total;
        },
        .tag => |t| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(t.args)) |arg_id| {
                total += countIncrefsForSymbol(store, arg_id, symbol);
            }
            break :blk total;
        },
        .struct_access => |sa| countIncrefsForSymbol(store, sa.struct_expr, symbol),
        .nominal => |n| countIncrefsForSymbol(store, n.backing_expr, symbol),
        .early_return => |ret| countIncrefsForSymbol(store, ret.expr, symbol),
        .dbg => |d| countIncrefsForSymbol(store, d.expr, symbol),
        .str_concat => |parts| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(parts)) |part_id| {
                total += countIncrefsForSymbol(store, part_id, symbol);
            }
            break :blk total;
        },
        .int_to_str => |its| countIncrefsForSymbol(store, its.value, symbol),
        .float_to_str => |fts| countIncrefsForSymbol(store, fts.value, symbol),
        .dec_to_str => |d| countIncrefsForSymbol(store, d, symbol),
        .str_escape_and_quote => |s| countIncrefsForSymbol(store, s, symbol),
        .tag_payload_access => |tpa| countIncrefsForSymbol(store, tpa.value, symbol),
        .hosted_call => |hc| blk: {
            var total: u32 = 0;
            for (store.getExprSpan(hc.args)) |arg_id| {
                total += countIncrefsForSymbol(store, arg_id, symbol);
            }
            break :blk total;
        },
        .incref => |inc| blk: {
            const inc_expr = store.getExpr(inc.value);
            break :blk if (inc_expr == .lookup and @as(u64, @bitCast(inc_expr.lookup.symbol)) == key) 1 else 0;
        },
        else => 0,
    };
}

fn findFirstIfThenElseExpr(store: *const LirExprStore, expr_id: LirExprId) ?LirExprId {
    if (expr_id.isNone()) return null;

    const expr = store.getExpr(expr_id);
    switch (expr) {
        .if_then_else => return expr_id,
        .block => |block| {
            for (store.getStmts(block.stmts)) |stmt| {
                const nested = switch (stmt) {
                    .decl, .mutate => |binding| findFirstIfThenElseExpr(store, binding.expr),
                    .cell_init, .cell_store => |binding| findFirstIfThenElseExpr(store, binding.expr),
                    .cell_drop => null,
                };
                if (nested) |found| return found;
            }

            return findFirstIfThenElseExpr(store, block.final_expr);
        },
        .match_expr => |m| {
            if (findFirstIfThenElseExpr(store, m.value)) |found| return found;
            for (store.getMatchBranches(m.branches)) |branch| {
                if (findFirstIfThenElseExpr(store, branch.guard)) |found| return found;
                if (findFirstIfThenElseExpr(store, branch.body)) |found| return found;
            }
            return null;
        },
        .discriminant_switch => |ds| {
            if (findFirstIfThenElseExpr(store, ds.value)) |found| return found;
            for (store.getExprSpan(ds.branches)) |branch_id| {
                if (findFirstIfThenElseExpr(store, branch_id)) |found| return found;
            }
            return null;
        },
        .for_loop => |fl| {
            if (findFirstIfThenElseExpr(store, fl.list_expr)) |found| return found;
            return findFirstIfThenElseExpr(store, fl.body);
        },
        .while_loop => |wl| {
            if (findFirstIfThenElseExpr(store, wl.cond)) |found| return found;
            return findFirstIfThenElseExpr(store, wl.body);
        },
        .proc_call => |call| {
            for (store.getExprSpan(call.args)) |arg_id| {
                if (findFirstIfThenElseExpr(store, arg_id)) |found| return found;
            }
            return null;
        },
        .low_level => |ll| {
            for (store.getExprSpan(ll.args)) |arg_id| {
                if (findFirstIfThenElseExpr(store, arg_id)) |found| return found;
            }
            return null;
        },
        .hosted_call => |hc| {
            for (store.getExprSpan(hc.args)) |arg_id| {
                if (findFirstIfThenElseExpr(store, arg_id)) |found| return found;
            }
            return null;
        },
        .list => |l| {
            for (store.getExprSpan(l.elems)) |elem_id| {
                if (findFirstIfThenElseExpr(store, elem_id)) |found| return found;
            }
            return null;
        },
        .struct_ => |s| {
            for (store.getExprSpan(s.fields)) |field_id| {
                if (findFirstIfThenElseExpr(store, field_id)) |found| return found;
            }
            return null;
        },
        .tag => |t| {
            for (store.getExprSpan(t.args)) |arg_id| {
                if (findFirstIfThenElseExpr(store, arg_id)) |found| return found;
            }
            return null;
        },
        .struct_access => |sa| return findFirstIfThenElseExpr(store, sa.struct_expr),
        .tag_payload_access => |tpa| return findFirstIfThenElseExpr(store, tpa.value),
        .nominal => |n| return findFirstIfThenElseExpr(store, n.backing_expr),
        .early_return => |ret| return findFirstIfThenElseExpr(store, ret.expr),
        .dbg => |d| return findFirstIfThenElseExpr(store, d.expr),
        .expect => |e| {
            if (findFirstIfThenElseExpr(store, e.cond)) |found| return found;
            return findFirstIfThenElseExpr(store, e.body);
        },
        .str_concat => |parts| {
            for (store.getExprSpan(parts)) |part_id| {
                if (findFirstIfThenElseExpr(store, part_id)) |found| return found;
            }
            return null;
        },
        .int_to_str => |its| return findFirstIfThenElseExpr(store, its.value),
        .float_to_str => |fts| return findFirstIfThenElseExpr(store, fts.value),
        .dec_to_str => |d| return findFirstIfThenElseExpr(store, d),
        .str_escape_and_quote => |s| return findFirstIfThenElseExpr(store, s),
        .incref => |inc| return findFirstIfThenElseExpr(store, inc.value),
        .decref => |dec| return findFirstIfThenElseExpr(store, dec.value),
        .free => |free| return findFirstIfThenElseExpr(store, free.value),
        else => return null,
    }
}

fn findFirstWhileExpr(store: *const LirExprStore, expr_id: LirExprId) ?LirExprId {
    if (expr_id.isNone()) return null;

    const expr = store.getExpr(expr_id);
    switch (expr) {
        .while_loop => return expr_id,
        .block => |block| {
            for (store.getStmts(block.stmts)) |stmt| {
                const nested = switch (stmt) {
                    .decl, .mutate => |binding| findFirstWhileExpr(store, binding.expr),
                    .cell_init, .cell_store => |binding| findFirstWhileExpr(store, binding.expr),
                    .cell_drop => null,
                };
                if (nested) |found| return found;
            }

            return findFirstWhileExpr(store, block.final_expr);
        },
        .match_expr => |m| {
            if (findFirstWhileExpr(store, m.value)) |found| return found;
            for (store.getMatchBranches(m.branches)) |branch| {
                if (findFirstWhileExpr(store, branch.guard)) |found| return found;
                if (findFirstWhileExpr(store, branch.body)) |found| return found;
            }
            return null;
        },
        .discriminant_switch => |ds| {
            if (findFirstWhileExpr(store, ds.value)) |found| return found;
            for (store.getExprSpan(ds.branches)) |branch_id| {
                if (findFirstWhileExpr(store, branch_id)) |found| return found;
            }
            return null;
        },
        .for_loop => |fl| {
            if (findFirstWhileExpr(store, fl.list_expr)) |found| return found;
            return findFirstWhileExpr(store, fl.body);
        },
        .proc_call => |call| {
            for (store.getExprSpan(call.args)) |arg_id| {
                if (findFirstWhileExpr(store, arg_id)) |found| return found;
            }
            return null;
        },
        .low_level => |ll| {
            for (store.getExprSpan(ll.args)) |arg_id| {
                if (findFirstWhileExpr(store, arg_id)) |found| return found;
            }
            return null;
        },
        .hosted_call => |hc| {
            for (store.getExprSpan(hc.args)) |arg_id| {
                if (findFirstWhileExpr(store, arg_id)) |found| return found;
            }
            return null;
        },
        .list => |l| {
            for (store.getExprSpan(l.elems)) |elem_id| {
                if (findFirstWhileExpr(store, elem_id)) |found| return found;
            }
            return null;
        },
        .struct_ => |s| {
            for (store.getExprSpan(s.fields)) |field_id| {
                if (findFirstWhileExpr(store, field_id)) |found| return found;
            }
            return null;
        },
        .tag => |t| {
            for (store.getExprSpan(t.args)) |arg_id| {
                if (findFirstWhileExpr(store, arg_id)) |found| return found;
            }
            return null;
        },
        .struct_access => |sa| return findFirstWhileExpr(store, sa.struct_expr),
        .tag_payload_access => |tpa| return findFirstWhileExpr(store, tpa.value),
        .nominal => |n| return findFirstWhileExpr(store, n.backing_expr),
        .early_return => |ret| return findFirstWhileExpr(store, ret.expr),
        .dbg => |d| return findFirstWhileExpr(store, d.expr),
        .expect => |e| {
            if (findFirstWhileExpr(store, e.cond)) |found| return found;
            return findFirstWhileExpr(store, e.body);
        },
        .str_concat => |parts| {
            for (store.getExprSpan(parts)) |part_id| {
                if (findFirstWhileExpr(store, part_id)) |found| return found;
            }
            return null;
        },
        .int_to_str => |its| return findFirstWhileExpr(store, its.value),
        .float_to_str => |fts| return findFirstWhileExpr(store, fts.value),
        .dec_to_str => |d| return findFirstWhileExpr(store, d),
        .str_escape_and_quote => |s| return findFirstWhileExpr(store, s),
        .incref => |inc| return findFirstWhileExpr(store, inc.value),
        .decref => |dec| return findFirstWhileExpr(store, dec.value),
        .free => |free| return findFirstWhileExpr(store, free.value),
        else => return null,
    }
}

/// Helper to make a symbol with a given ident index.
fn makeSymbol(idx: u29) LIR.Symbol {
    const ident = base.Ident.Idx{
        .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
        .idx = idx,
    };
    return LIR.Symbol.fromRaw(@as(u64, @as(u32, @bitCast(ident))));
}

fn makeProc(store: *LirExprStore, symbol: LIR.Symbol, ret_layout: LayoutIdx) !LIR.LirProcSpecId {
    return store.addProcSpec(.{
        .name = symbol,
        .args = LIR.LirPatternSpan.empty(),
        .arg_layouts = LIR.LayoutIdxSpan.empty(),
        .body = .none,
        .ret_layout = ret_layout,
        .closure_data_layout = null,
        .is_self_recursive = .not_self_recursive,
    });
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

test "RC borrow-only low-level materializes refcounted top-level lookup" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const u64_layout: LayoutIdx = .u64;

    const sym_data = makeSymbol(100);
    const str_idx = try env.lir_store.insertString("this is definitely longer than a small string");
    const def_expr = try env.lir_store.addExpr(.{ .str_literal = str_idx }, Region.zero());
    try env.lir_store.registerSymbolDef(sym_data, def_expr);

    const lookup_data = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_data,
        .layout_idx = str_layout,
    } }, Region.zero());

    const len_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .str_count_utf8_bytes,
        .args = try env.lir_store.addExprSpan(&.{lookup_data}),
        .ret_layout = u64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(len_expr);
    const root = env.lir_store.getExpr(result);
    try std.testing.expect(root == .block);

    const stmts = env.lir_store.getStmts(root.block.stmts);
    try std.testing.expectEqual(@as(usize, 1), stmts.len);
    try std.testing.expect(stmts[0] == .decl);
    try std.testing.expectEqual(lookup_data, stmts[0].decl.expr);

    const low_level_id = findFirstLowLevelExpr(&env.lir_store, root.block.final_expr) orelse return error.TestUnexpectedResult;
    const final_expr = env.lir_store.getExpr(low_level_id);
    try std.testing.expect(final_expr == .low_level);
    const final_args = env.lir_store.getExprSpan(final_expr.low_level.args);
    try std.testing.expectEqual(@as(usize, 1), final_args.len);
    try std.testing.expect(final_args[0] != lookup_data);
    try std.testing.expect(env.lir_store.getExpr(final_args[0]) == .lookup);
}

test "RC borrow-only low-level materializes refcounted proc_call result" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_proc = makeSymbol(200);

    const proc_id = try makeProc(&env.lir_store, sym_proc, list_layout);
    const call_expr = try env.lir_store.addExpr(.{ .proc_call = .{
        .proc = proc_id,
        .args = LIR.LirExprSpan.empty(),
        .ret_layout = list_layout,
        .called_via = .apply,
    } }, Region.zero());
    const len_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .list_len,
        .args = try env.lir_store.addExprSpan(&.{call_expr}),
        .ret_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(len_expr);
    const root = env.lir_store.getExpr(result);
    try std.testing.expect(root == .block);

    const stmts = env.lir_store.getStmts(root.block.stmts);
    try std.testing.expectEqual(@as(usize, 1), stmts.len);
    try std.testing.expect(stmts[0] == .decl);
    try std.testing.expectEqual(call_expr, stmts[0].decl.expr);

    const low_level_id = findFirstLowLevelExpr(&env.lir_store, root.block.final_expr) orelse return error.TestUnexpectedResult;
    const final_expr = env.lir_store.getExpr(low_level_id);
    try std.testing.expect(final_expr == .low_level);
    const final_args = env.lir_store.getExprSpan(final_expr.low_level.args);
    try std.testing.expectEqual(@as(usize, 1), final_args.len);
    try std.testing.expect(final_args[0] != call_expr);
    try std.testing.expect(env.lir_store.getExpr(final_args[0]) == .lookup);

    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC synthetic symbols stay unique across passes on one store" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const u64_layout: LayoutIdx = .u64;

    const sym_data = makeSymbol(100);
    const str_idx = try env.lir_store.insertString("this is definitely longer than a small string");
    const def_expr = try env.lir_store.addExpr(.{ .str_literal = str_idx }, Region.zero());
    try env.lir_store.registerSymbolDef(sym_data, def_expr);

    const lookup_one = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_data,
        .layout_idx = str_layout,
    } }, Region.zero());
    const expr_one = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .str_count_utf8_bytes,
        .args = try env.lir_store.addExprSpan(&.{lookup_one}),
        .ret_layout = u64_layout,
    } }, Region.zero());

    const lookup_two = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_data,
        .layout_idx = str_layout,
    } }, Region.zero());
    const expr_two = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .str_count_utf8_bytes,
        .args = try env.lir_store.addExprSpan(&.{lookup_two}),
        .ret_layout = u64_layout,
    } }, Region.zero());

    var pass_one = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass_one.deinit();
    const result_one = try pass_one.insertRcOps(expr_one);

    var pass_two = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass_two.deinit();
    const result_two = try pass_two.insertRcOps(expr_two);

    const symbol_one = firstBlockBindSymbol(&env.lir_store, result_one) orelse return error.TestUnexpectedResult;
    const symbol_two = firstBlockBindSymbol(&env.lir_store, result_two) orelse return error.TestUnexpectedResult;

    try std.testing.expect(symbol_one != symbol_two);
}

test "RC op wildcard decls use zst layout" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const u64_layout: LayoutIdx = .u64;

    const sym_data = makeSymbol(100);
    const str_idx = try env.lir_store.insertString("this is definitely longer than a small string");
    const def_expr = try env.lir_store.addExpr(.{ .str_literal = str_idx }, Region.zero());
    try env.lir_store.registerSymbolDef(sym_data, def_expr);

    const lookup_data = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_data,
        .layout_idx = str_layout,
    } }, Region.zero());
    const len_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .str_count_utf8_bytes,
        .args = try env.lir_store.addExprSpan(&.{lookup_data}),
        .ret_layout = u64_layout,
    } }, Region.zero());
    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = .i64 } }, Region.zero());
    const pat_len = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = makeSymbol(101),
        .layout_idx = u64_layout,
    } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_len, .expr = len_expr } },
    });
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = zero,
        .result_layout = .i64,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();
    const result = try pass.insertRcOps(block_expr);

    try std.testing.expectEqual(LayoutIdx.zst, firstRcWildcardLayout(&env.lir_store, result).?);
}

test "RC explicit retained list element keeps outer binding cleanup" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const inner_list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const outer_list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(inner_list_layout));
    const sym_inner = makeSymbol(1);
    const sym_outer = makeSymbol(2);
    const sym_inner_retained = makeSymbol(3);

    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());
    const inner_elems = try env.lir_store.addExprSpan(&.{ one, two });
    const inner_list = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = inner_list_layout,
        .elem_layout = i64_layout,
        .elems = inner_elems,
    } }, Region.zero());

    const lookup_inner = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_inner, .layout_idx = inner_list_layout } }, Region.zero());
    const lookup_inner_retained = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_inner_retained, .layout_idx = inner_list_layout } }, Region.zero());
    const outer_elems = try env.lir_store.addExprSpan(&.{lookup_inner_retained});
    const outer_list = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = outer_list_layout,
        .elem_layout = inner_list_layout,
        .elems = outer_elems,
    } }, Region.zero());
    const lookup_outer = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_outer, .layout_idx = outer_list_layout } }, Region.zero());

    const pat_inner = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_inner, .layout_idx = inner_list_layout } }, Region.zero());
    const pat_inner_retained = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_inner_retained, .layout_idx = inner_list_layout } }, Region.zero());
    const pat_outer = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_outer, .layout_idx = outer_list_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_inner, .expr = inner_list } },
        .{ .decl = .{ .pattern = pat_inner_retained, .expr = lookup_inner, .semantics = .retained } },
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

    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
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

test "RC identity call result matched later tail-cleans matched binding" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_x = makeSymbol(1);
    const sym_arg = makeSymbol(2);
    const sym_result = makeSymbol(3);

    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());
    const elems = try env.lir_store.addExprSpan(&.{ one, two });
    const x_list = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());

    const identity_proc = try makeProc(&env.lir_store, sym_arg, list_layout);
    const lookup_x_call = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_x,
        .layout_idx = list_layout,
    } }, Region.zero());
    const call_args = try env.lir_store.addExprSpan(&.{lookup_x_call});
    const call_expr = try env.lir_store.addExpr(.{ .proc_call = .{
        .proc = identity_proc,
        .args = call_args,
        .ret_layout = list_layout,
        .called_via = .apply,
    } }, Region.zero());

    const lookup_result = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_result,
        .layout_idx = list_layout,
    } }, Region.zero());
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

    const pat_x = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_x,
        .layout_idx = list_layout,
    } }, Region.zero());
    const pat_result = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_result,
        .layout_idx = list_layout,
    } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_x, .expr = x_list } },
        .{ .decl = .{ .pattern = pat_result, .expr = call_expr } },
    });
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = match_expr,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    try std.testing.expectEqual(@as(u32, 0), countDecrefsForSymbol(&env.lir_store, result, sym_x));
    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_result));
}

test "RC repeated identity call tail-cleans the unused second result" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_x = makeSymbol(1);
    const sym_arg = makeSymbol(2);
    const sym_a = makeSymbol(3);
    const sym_b = makeSymbol(4);

    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());
    const elems = try env.lir_store.addExprSpan(&.{ one, two });
    const x_list = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());

    const identity_proc = try makeProc(&env.lir_store, sym_arg, list_layout);
    const lookup_x_call_a = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_x,
        .layout_idx = list_layout,
    } }, Region.zero());
    const call_args_a = try env.lir_store.addExprSpan(&.{lookup_x_call_a});
    const call_expr_a = try env.lir_store.addExpr(.{ .proc_call = .{
        .proc = identity_proc,
        .args = call_args_a,
        .ret_layout = list_layout,
        .called_via = .apply,
    } }, Region.zero());

    const lookup_x_call_b = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_x,
        .layout_idx = list_layout,
    } }, Region.zero());
    const call_args_b = try env.lir_store.addExprSpan(&.{lookup_x_call_b});
    const call_expr_b = try env.lir_store.addExpr(.{ .proc_call = .{
        .proc = identity_proc,
        .args = call_args_b,
        .ret_layout = list_layout,
        .called_via = .apply,
    } }, Region.zero());

    const lookup_a = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_a,
        .layout_idx = list_layout,
    } }, Region.zero());
    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = i64_layout } }, Region.zero());
    const wildcard = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = list_layout } }, Region.zero());
    const match_branches = try env.lir_store.addMatchBranches(&.{.{
        .pattern = wildcard,
        .guard = LirExprId.none,
        .body = zero,
    }});
    const match_expr = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = lookup_a,
        .value_layout = list_layout,
        .branches = match_branches,
        .result_layout = i64_layout,
    } }, Region.zero());

    const pat_x = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_x,
        .layout_idx = list_layout,
    } }, Region.zero());
    const pat_a = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_a,
        .layout_idx = list_layout,
    } }, Region.zero());
    const pat_b = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_b,
        .layout_idx = list_layout,
    } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_x, .expr = x_list } },
        .{ .decl = .{ .pattern = pat_a, .expr = call_expr_a } },
        .{ .decl = .{ .pattern = pat_b, .expr = call_expr_b } },
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

    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 2), rc.decrefs);
    try std.testing.expectEqual(@as(u32, 0), countDecrefsForSymbol(&env.lir_store, result, sym_x));
    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_a));
    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_b));
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
    const len_expr = try env.lir_store.addExpr(.{ .low_level = .{
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
    const append_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .list_append_unsafe,
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
    const len_expr = try env.lir_store.addExpr(.{ .low_level = .{
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

    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_src));
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
    // { s = "hello"; match cond is True -> [s, s], False -> [] }
    // True branch: 2 consumed uses => incref(1). False branch: 0 uses => decref.
    // Global count = 1 => no incref at binding.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(str_layout));
    const sym_s = makeSymbol(1);
    const sym_cond = makeSymbol(2);

    const cond_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond, .layout_idx = i64_layout } }, Region.zero());
    const lookup_s1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const empty = try env.lir_store.addExpr(.{ .empty_list = .{ .list_layout = list_layout, .elem_layout = str_layout } }, Region.zero());

    // True branch body: list [s, s] — consumes s twice
    const list_elems = try env.lir_store.addExprSpan(&.{ lookup_s1, lookup_s2 });
    const list_expr = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = str_layout,
        .elems = list_elems,
    } }, Region.zero());

    const wild_pat1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild_pat2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const match_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = wild_pat1, .guard = LirExprId.none, .body = list_expr },
        .{ .pattern = wild_pat2, .guard = LirExprId.none, .body = empty },
    });

    const match_expr = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = match_branches,
        .result_layout = list_layout,
    } }, Region.zero());

    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_s, .expr = str_lit } }});

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = match_expr,
        .result_layout = list_layout,
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

test "RC proc body: returning refcounted param does not tail-decref it" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_s = makeSymbol(1);

    const lookup_s = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_s,
        .layout_idx = str_layout,
    } }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_s,
        .layout_idx = str_layout,
    } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{pat_s});

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOpsForProcBody(lookup_s, params, str_layout);

    try std.testing.expectEqual(@as(u32, 0), countDecrefsForSymbol(&env.lir_store, result, sym_s));
}

test "RC proc body: returning list param does not tail-decref it" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_list = makeSymbol(1);

    const lookup_list = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_list,
        .layout_idx = list_layout,
    } }, Region.zero());
    const pat_list = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_list,
        .layout_idx = list_layout,
    } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{pat_list});

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOpsForProcBody(lookup_list, params, list_layout);

    try std.testing.expectEqual(@as(u32, 0), countDecrefsForSymbol(&env.lir_store, result, sym_list));
}

test "RC proc_call caller: consumed refcounted arg is not tail-decref'd by caller" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_s = makeSymbol(1);
    const sym_id = makeSymbol(2);

    const lookup_s = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_s,
        .layout_idx = str_layout,
    } }, Region.zero());
    const proc_id = try makeProc(&env.lir_store, sym_id, str_layout);
    const call_args = try env.lir_store.addExprSpan(&.{lookup_s});
    const call_expr = try env.lir_store.addExpr(.{ .proc_call = .{
        .proc = proc_id,
        .args = call_args,
        .ret_layout = str_layout,
        .called_via = .apply,
    } }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_s,
        .layout_idx = str_layout,
    } }, Region.zero());
    const str_idx = try env.lir_store.insertString("this is definitely larger than a small string");
    const str_lit = try env.lir_store.addExpr(.{ .str_literal = str_idx }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{
        .pattern = pat_s,
        .expr = str_lit,
    } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = call_expr,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    try std.testing.expectEqual(@as(u32, 0), countDecrefsForSymbol(&env.lir_store, result, sym_s));
}

test "RC proc_call caller: consumed list arg is not tail-decref'd by caller" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_list = makeSymbol(1);
    const sym_id = makeSymbol(2);

    const lookup_list = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_list,
        .layout_idx = list_layout,
    } }, Region.zero());
    const proc_id = try makeProc(&env.lir_store, sym_id, list_layout);
    const call_args = try env.lir_store.addExprSpan(&.{lookup_list});
    const call_expr = try env.lir_store.addExpr(.{ .proc_call = .{
        .proc = proc_id,
        .args = call_args,
        .ret_layout = list_layout,
        .called_via = .apply,
    } }, Region.zero());
    const pat_list = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_list,
        .layout_idx = list_layout,
    } }, Region.zero());
    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const elems = try env.lir_store.addExprSpan(&.{one});
    const list_lit = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{
        .pattern = pat_list,
        .expr = list_lit,
    } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = call_expr,
        .result_layout = list_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    try std.testing.expectEqual(@as(u32, 0), countDecrefsForSymbol(&env.lir_store, result, sym_list));
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

    // incref(1) for the two borrow-only uses. The loop element is borrowed from
    // the source list, so the loop must not tail-decref it.
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
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
    const len_expr = try env.lir_store.addExpr(.{ .low_level = .{
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

test "RC mutation: final use of reassignable refcounted var emits tail decref" {
    // { var s = "hello"; s = "world"; s }
    // The old value should decref before mutation, and the final use of the
    // latest value should still get a tail decref at block exit.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const sym_s = makeSymbol(1);

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
    const rc = countRcOps(&env.lir_store, result);

    try std.testing.expectEqual(@as(u32, 2), rc.decrefs);
}

test "RC for_loop: unused refcounted elem does not decref borrowed element" {
    // for list |elem| { 42 } where elem is str-layout but unused in body.
    // The loop element is borrowed from the source list, so ignoring it should
    // not emit any element decref.
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

    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
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

    // Guard incref(1) for s in branch 0, decref in the taken branch body,
    // plus one outer tail decref after the match to release the inherited ref.
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 3), rc.decrefs);
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
    // If RC ops are needed, the wrapper block around the body should have .zst
    // (unit) result layout, not elem_layout (str). Borrowed loop elements that
    // need no RC ops may leave the body unwrapped.
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

    try std.testing.expect(result_expr == .for_loop);
    const wrapper_body = env.lir_store.getExpr(result_expr.for_loop.body);
    if (wrapper_body == .block) {
        try std.testing.expectEqual(LayoutIdx.zst, wrapper_body.block.result_layout);
    } else {
        try std.testing.expectEqualDeep(env.lir_store.getExpr(int_lit), wrapper_body);
    }
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

test "RC if_then_else: condition preserves live list owner for branch body" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_list = makeSymbol(1);

    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = i64_layout } }, Region.zero());
    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());

    const elems = try env.lir_store.addExprSpan(&.{ one, two });
    const list_expr = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());

    const lookup_list_len = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const len_args = try env.lir_store.addExprSpan(&.{lookup_list_len});
    const len_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .list_len,
        .args = len_args,
        .ret_layout = i64_layout,
    } }, Region.zero());

    const eq_args = try env.lir_store.addExprSpan(&.{ len_expr, zero });
    const cond_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .num_is_eq,
        .args = eq_args,
        .ret_layout = .bool,
    } }, Region.zero());

    const lookup_list_first = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const first_args = try env.lir_store.addExprSpan(&.{ lookup_list_first, zero });
    const first_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .list_get_unsafe,
        .args = first_args,
        .ret_layout = i64_layout,
    } }, Region.zero());

    const if_branches = try env.lir_store.addIfBranches(&.{.{
        .cond = cond_expr,
        .body = first_expr,
    }});
    const ite = try env.lir_store.addExpr(.{ .if_then_else = .{
        .branches = if_branches,
        .final_else = zero,
        .result_layout = i64_layout,
    } }, Region.zero());

    const pat_list = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_list, .layout_idx = list_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{ .pattern = pat_list, .expr = list_expr } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = ite,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    const transformed_if = findFirstIfThenElseExpr(&env.lir_store, result) orelse unreachable;
    const ite_result = env.lir_store.getExpr(transformed_if).if_then_else;
    const branch = env.lir_store.getIfBranches(ite_result.branches)[0];

    try std.testing.expectEqual(@as(u32, 1), countIncrefsForSymbol(&env.lir_store, branch.cond, sym_list));
    try std.testing.expectEqual(@as(u32, 0), countIncrefsForSymbol(&env.lir_store, branch.body, sym_list));
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

test "RC match rest prelude tail-cleans outer scrutinee binding" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_scrutinee = makeSymbol(1);
    const sym_branch_source = makeSymbol(2);
    const sym_rest = makeSymbol(3);

    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());
    const three = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 3, .layout_idx = i64_layout } }, Region.zero());
    const four = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 4, .layout_idx = i64_layout } }, Region.zero());
    const elems = try env.lir_store.addExprSpan(&.{ one, two, three, four });
    const list_expr = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());

    const lookup_scrutinee_for_match = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_scrutinee,
        .layout_idx = list_layout,
    } }, Region.zero());
    const lookup_scrutinee_for_prelude = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_scrutinee,
        .layout_idx = list_layout,
    } }, Region.zero());

    const lookup_branch_source = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_branch_source,
        .layout_idx = list_layout,
    } }, Region.zero());
    const drop_count = try env.lir_store.addExpr(.{ .i64_literal = .{
        .value = 1,
        .layout_idx = .u64,
    } }, Region.zero());
    const drop_args = try env.lir_store.addExprSpan(&.{ lookup_branch_source, drop_count });
    const rest_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .list_drop_first,
        .args = drop_args,
        .ret_layout = list_layout,
    } }, Region.zero());

    const lookup_rest = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_rest,
        .layout_idx = list_layout,
    } }, Region.zero());
    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = i64_layout } }, Region.zero());
    const inner_wild = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = list_layout } }, Region.zero());
    const inner_branches = try env.lir_store.addMatchBranches(&.{.{
        .pattern = inner_wild,
        .guard = LirExprId.none,
        .body = zero,
    }});
    const inner_match = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = lookup_rest,
        .value_layout = list_layout,
        .branches = inner_branches,
        .result_layout = i64_layout,
    } }, Region.zero());

    const pat_branch_source = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_branch_source,
        .layout_idx = list_layout,
    } }, Region.zero());
    const pat_rest = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_rest,
        .layout_idx = list_layout,
    } }, Region.zero());
    const branch_stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_branch_source, .expr = lookup_scrutinee_for_prelude, .semantics = .retained } },
        .{ .decl = .{ .pattern = pat_rest, .expr = rest_expr } },
    });
    const branch_body = try env.lir_store.addExpr(.{ .block = .{
        .stmts = branch_stmts,
        .final_expr = inner_match,
        .result_layout = i64_layout,
    } }, Region.zero());

    const outer_wild = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = list_layout } }, Region.zero());
    const outer_branches = try env.lir_store.addMatchBranches(&.{.{
        .pattern = outer_wild,
        .guard = LirExprId.none,
        .body = branch_body,
    }});
    const outer_match = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = lookup_scrutinee_for_match,
        .value_layout = list_layout,
        .branches = outer_branches,
        .result_layout = i64_layout,
    } }, Region.zero());

    const pat_scrutinee = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_scrutinee,
        .layout_idx = list_layout,
    } }, Region.zero());
    const outer_stmts = try env.lir_store.addStmts(&.{.{ .decl = .{
        .pattern = pat_scrutinee,
        .expr = list_expr,
    } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = outer_stmts,
        .final_expr = outer_match,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_scrutinee));
    try std.testing.expectEqual(@as(u32, 0), countDecrefsForSymbol(&env.lir_store, result, sym_branch_source));
    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_rest));
}

test "RC nested list-pattern match tail-cleans rest binding" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_rest = makeSymbol(1);
    const sym_second = makeSymbol(2);

    const lookup_rest = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_rest,
        .layout_idx = list_layout,
    } }, Region.zero());
    const lookup_second = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_second,
        .layout_idx = i64_layout,
    } }, Region.zero());
    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = i64_layout } }, Region.zero());

    const pat_second = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_second,
        .layout_idx = i64_layout,
    } }, Region.zero());
    const wildcard_rest = try env.lir_store.addPattern(.{ .wildcard = .{
        .layout_idx = list_layout,
    } }, Region.zero());
    const prefix = try env.lir_store.addPatternSpan(&.{pat_second});
    const empty_suffix = LIR.LirPatternSpan.empty();
    const list_pat = try env.lir_store.addPattern(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .prefix = prefix,
        .rest = wildcard_rest,
        .suffix = empty_suffix,
    } }, Region.zero());
    const inner_wild = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = list_layout } }, Region.zero());

    const inner_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = list_pat, .guard = LirExprId.none, .body = lookup_second },
        .{ .pattern = inner_wild, .guard = LirExprId.none, .body = zero },
    });
    const inner_match = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = lookup_rest,
        .value_layout = list_layout,
        .branches = inner_branches,
        .result_layout = i64_layout,
    } }, Region.zero());

    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());
    const three = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 3, .layout_idx = i64_layout } }, Region.zero());
    const elems = try env.lir_store.addExprSpan(&.{ one, two, three });
    const rest_expr = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());
    const pat_rest = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_rest,
        .layout_idx = list_layout,
    } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{
        .pattern = pat_rest,
        .expr = rest_expr,
    } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = inner_match,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_rest));
}

test "RC combined match rest prelude with nested list pattern cleans both owners" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const i64_layout: LayoutIdx = .i64;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(i64_layout));
    const sym_scrutinee = makeSymbol(1);
    const sym_branch_source = makeSymbol(2);
    const sym_rest = makeSymbol(3);
    const sym_second = makeSymbol(4);

    const one = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 1, .layout_idx = i64_layout } }, Region.zero());
    const two = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 2, .layout_idx = i64_layout } }, Region.zero());
    const three = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 3, .layout_idx = i64_layout } }, Region.zero());
    const four = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 4, .layout_idx = i64_layout } }, Region.zero());
    const elems = try env.lir_store.addExprSpan(&.{ one, two, three, four });
    const list_expr = try env.lir_store.addExpr(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .elems = elems,
    } }, Region.zero());

    const lookup_scrutinee_for_match = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_scrutinee,
        .layout_idx = list_layout,
    } }, Region.zero());
    const lookup_scrutinee_for_prelude = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_scrutinee,
        .layout_idx = list_layout,
    } }, Region.zero());

    const lookup_branch_source = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_branch_source,
        .layout_idx = list_layout,
    } }, Region.zero());
    const drop_count = try env.lir_store.addExpr(.{ .i64_literal = .{
        .value = 1,
        .layout_idx = .u64,
    } }, Region.zero());
    const drop_args = try env.lir_store.addExprSpan(&.{ lookup_branch_source, drop_count });
    const rest_expr = try env.lir_store.addExpr(.{ .low_level = .{
        .op = .list_drop_first,
        .args = drop_args,
        .ret_layout = list_layout,
    } }, Region.zero());

    const lookup_rest = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_rest,
        .layout_idx = list_layout,
    } }, Region.zero());
    const lookup_second = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_second,
        .layout_idx = i64_layout,
    } }, Region.zero());
    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{ .value = 0, .layout_idx = i64_layout } }, Region.zero());

    const pat_second = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_second,
        .layout_idx = i64_layout,
    } }, Region.zero());
    const wildcard_rest = try env.lir_store.addPattern(.{ .wildcard = .{
        .layout_idx = list_layout,
    } }, Region.zero());
    const prefix = try env.lir_store.addPatternSpan(&.{pat_second});
    const list_pat = try env.lir_store.addPattern(.{ .list = .{
        .list_layout = list_layout,
        .elem_layout = i64_layout,
        .prefix = prefix,
        .rest = wildcard_rest,
        .suffix = LIR.LirPatternSpan.empty(),
    } }, Region.zero());
    const inner_wild = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = list_layout } }, Region.zero());
    const inner_branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = list_pat, .guard = LirExprId.none, .body = lookup_second },
        .{ .pattern = inner_wild, .guard = LirExprId.none, .body = zero },
    });
    const inner_match = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = lookup_rest,
        .value_layout = list_layout,
        .branches = inner_branches,
        .result_layout = i64_layout,
    } }, Region.zero());

    const pat_branch_source = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_branch_source,
        .layout_idx = list_layout,
    } }, Region.zero());
    const pat_rest = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_rest,
        .layout_idx = list_layout,
    } }, Region.zero());
    const branch_stmts = try env.lir_store.addStmts(&.{
        .{ .decl = .{ .pattern = pat_branch_source, .expr = lookup_scrutinee_for_prelude, .semantics = .retained } },
        .{ .decl = .{ .pattern = pat_rest, .expr = rest_expr } },
    });
    const branch_body = try env.lir_store.addExpr(.{ .block = .{
        .stmts = branch_stmts,
        .final_expr = inner_match,
        .result_layout = i64_layout,
    } }, Region.zero());

    const outer_wild = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = list_layout } }, Region.zero());
    const outer_branches = try env.lir_store.addMatchBranches(&.{.{
        .pattern = outer_wild,
        .guard = LirExprId.none,
        .body = branch_body,
    }});
    const outer_match = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = lookup_scrutinee_for_match,
        .value_layout = list_layout,
        .branches = outer_branches,
        .result_layout = i64_layout,
    } }, Region.zero());

    const pat_scrutinee = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_scrutinee,
        .layout_idx = list_layout,
    } }, Region.zero());
    const outer_stmts = try env.lir_store.addStmts(&.{.{ .decl = .{
        .pattern = pat_scrutinee,
        .expr = list_expr,
    } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = outer_stmts,
        .final_expr = outer_match,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_scrutinee));
    try std.testing.expectEqual(@as(u32, 0), countDecrefsForSymbol(&env.lir_store, result, sym_branch_source));
    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_rest));
}

test "RC tag-pattern match tail-cleans outer scrutinee binding with refcounted payload" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const list_layout = try env.layout_store.insertLayout(layout_mod.Layout.list(str_layout));
    const payload_layout = try env.layout_store.putStructFields(&.{
        .{ .index = 0, .layout = str_layout },
        .{ .index = 1, .layout = list_layout },
    });
    const union_layout = try env.layout_store.putTagUnion(&.{ .zst, payload_layout });

    const sym_scrutinee = makeSymbol(1);

    const hello = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const empty_list = try env.lir_store.addExpr(.{ .empty_list = .{
        .list_layout = list_layout,
        .elem_layout = str_layout,
    } }, Region.zero());
    const payload_args = try env.lir_store.addExprSpan(&.{ hello, empty_list });
    const scrutinee_expr = try env.lir_store.addExpr(.{ .tag = .{
        .discriminant = 1,
        .union_layout = union_layout,
        .args = payload_args,
    } }, Region.zero());

    const lookup_scrutinee = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_scrutinee,
        .layout_idx = union_layout,
    } }, Region.zero());
    const zero = try env.lir_store.addExpr(.{ .i64_literal = .{
        .value = 0,
        .layout_idx = .i64,
    } }, Region.zero());

    const wild_tag = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());
    const wild_children = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = list_layout } }, Region.zero());
    const tag_args = try env.lir_store.addPatternSpan(&.{ wild_tag, wild_children });
    const tag_pat = try env.lir_store.addPattern(.{ .tag = .{
        .discriminant = 1,
        .union_layout = union_layout,
        .args = tag_args,
    } }, Region.zero());
    const outer_wild = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = union_layout } }, Region.zero());
    const branches = try env.lir_store.addMatchBranches(&.{
        .{ .pattern = tag_pat, .guard = LirExprId.none, .body = zero },
        .{ .pattern = outer_wild, .guard = LirExprId.none, .body = zero },
    });
    const match_expr = try env.lir_store.addExpr(.{ .match_expr = .{
        .value = lookup_scrutinee,
        .value_layout = union_layout,
        .branches = branches,
        .result_layout = .i64,
    } }, Region.zero());

    const pat_scrutinee = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_scrutinee,
        .layout_idx = union_layout,
    } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .decl = .{
        .pattern = pat_scrutinee,
        .expr = scrutinee_expr,
    } }});
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = match_expr,
        .result_layout = .i64,
    } }, Region.zero());

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    try std.testing.expectEqual(@as(u32, 0), countIncrefsForSymbol(&env.lir_store, result, sym_scrutinee));
    try std.testing.expectEqual(@as(u32, 1), countDecrefsForSymbol(&env.lir_store, result, sym_scrutinee));
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

test "RC tag_payload_access: retained parent temp is released after extraction" {
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const tag_union_layout = try env.layout_store.putTagUnion(&.{ str_layout, str_layout });
    const sym_val = makeSymbol(1);

    const lookup_val = try env.lir_store.addExpr(.{ .lookup = .{
        .symbol = sym_val,
        .layout_idx = tag_union_layout,
    } }, Region.zero());
    const payload_expr = try env.lir_store.addExpr(.{ .tag_payload_access = .{
        .value = lookup_val,
        .union_layout = tag_union_layout,
        .payload_layout = str_layout,
    } }, Region.zero());

    const pat_val = try env.lir_store.addPattern(.{ .bind = .{
        .symbol = sym_val,
        .layout_idx = tag_union_layout,
    } }, Region.zero());
    const params = try env.lir_store.addPatternSpan(&.{pat_val});

    var pass = try RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOpsForProcBody(payload_expr, params, str_layout);
    const rc = countRcOps(&env.lir_store, result);

    try std.testing.expectEqual(@as(u32, 2), rc.increfs);
    try std.testing.expectEqual(@as(u32, 2), rc.decrefs);
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
    // The str_concat argument uses s in borrow mode, so the original owner stays
    // live until the final lookup. early_return happens before that final consume,
    // so cleanup only needs to decref the single remaining owner.
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
    const str_concat_call = try env.lir_store.addExpr(.{ .str_concat = call_args }, Region.zero());

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
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}
