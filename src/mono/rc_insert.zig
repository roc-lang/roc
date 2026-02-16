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
//! For branching constructs (when/if), use counts are scoped per-branch.
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
const MonoWhenBranch = MonoIR.MonoWhenBranch;
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

    /// Temporary buffer for building statement lists.
    stmt_buf: std.ArrayList(MonoStmt),

    pub fn init(allocator: Allocator, store: *MonoExprStore, layout_store: *const layout_mod.Store) RcInsertPass {
        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .symbol_use_counts = std.AutoHashMap(u64, u32).init(allocator),
            .symbol_layouts = std.AutoHashMap(u64, LayoutIdx).init(allocator),
            .stmt_buf = std.ArrayList(MonoStmt).empty,
        };
    }

    pub fn deinit(self: *RcInsertPass) void {
        self.symbol_use_counts.deinit();
        self.symbol_layouts.deinit();
        self.stmt_buf.deinit(self.allocator);
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
                for (branches) |branch| {
                    var local = std.AutoHashMap(u64, u32).init(self.allocator);
                    defer local.deinit();
                    try self.countUsesInto(branch.body, &local);
                    var it = local.keyIterator();
                    while (it.next()) |key| try symbols_in_any_branch.put(key.*, {});
                }
                {
                    var local = std.AutoHashMap(u64, u32).init(self.allocator);
                    defer local.deinit();
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
            .when => |w| {
                try self.countUsesInto(w.value, target);
                const branches = self.store.getWhenBranches(w.branches);
                // Count branch body uses into local maps; each branching construct
                // contributes 1 use per symbol to the enclosing scope.
                var symbols_in_any_branch = std.AutoHashMap(u64, void).init(self.allocator);
                defer symbols_in_any_branch.deinit();
                for (branches) |branch| {
                    try self.registerPatternSymbolInto(branch.pattern, target);
                    var local = std.AutoHashMap(u64, u32).init(self.allocator);
                    defer local.deinit();
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
                const params = self.store.getPatternSpan(lam.params);
                for (params) |pat_id| {
                    try self.registerPatternSymbolInto(pat_id, target);
                }
                try self.countUsesInto(lam.body, target);
            },
            .closure => |clo| {
                try self.countUsesInto(clo.lambda, target);
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
    fn registerPatternSymbol(self: *RcInsertPass, pat_id: MonoPatternId) Allocator.Error!void {
        try self.registerPatternSymbolInto(pat_id, &self.symbol_use_counts);
    }

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
        stmts_span: MonoStmtSpan,
        final_expr: MonoExprId,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!MonoExprId {
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
    /// Each branch gets per-branch RC ops based on local use counts.
    fn processIfThenElse(
        self: *RcInsertPass,
        branches_span: MonoIR.MonoIfBranchSpan,
        final_else_id: MonoExprId,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!MonoExprId {
        const branches = self.store.getIfBranches(branches_span);

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

    /// Process a when expression.
    /// Each branch gets per-branch RC ops based on local use counts.
    fn processWhen(
        self: *RcInsertPass,
        value: MonoExprId,
        value_layout: LayoutIdx,
        branches_span: MonoIR.MonoWhenBranchSpan,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!MonoExprId {
        const branches = self.store.getWhenBranches(branches_span);

        // Collect symbols bound by branch patterns — these are local to each branch
        // and must NOT get per-branch RC ops from the enclosing scope.
        var pattern_bound = std.AutoHashMap(u64, void).init(self.allocator);
        defer pattern_bound.deinit();
        for (branches) |branch| {
            try self.collectPatternSymbols(branch.pattern, &pattern_bound);
        }

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
                // Skip pattern-bound symbols — they're local to their branch
                if (pattern_bound.contains(k)) continue;
                if (self.symbol_layouts.get(k)) |lay| {
                    if (self.layoutNeedsRc(lay)) try symbols_in_any_branch.put(k, {});
                }
            }
            try branch_use_maps.append(self.allocator, local);
        }

        var new_branches = std.ArrayList(MonoWhenBranch).empty;
        defer new_branches.deinit(self.allocator);

        for (branches, 0..) |branch, i| {
            const processed_body = try self.processExpr(branch.body);
            const new_body = try self.wrapBranchWithRcOps(processed_body, &branch_use_maps.items[i], &symbols_in_any_branch, result_layout, region);
            try new_branches.append(self.allocator, .{
                .pattern = branch.pattern,
                .guard = branch.guard,
                .body = new_body,
            });
        }

        const new_branch_span = try self.store.addWhenBranches(new_branches.items);
        return self.store.addExpr(.{ .when = .{
            .value = value,
            .value_layout = value_layout,
            .branches = new_branch_span,
            .result_layout = result_layout,
        } }, region);
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

        var it = symbols_in_any_branch.keyIterator();
        while (it.next()) |key_ptr| {
            const key = key_ptr.*;
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

    /// Emit an incref statement into the statement buffer.
    fn emitIncref(self: *RcInsertPass, symbol: Symbol, layout_idx: LayoutIdx, count: u16, region: Region) Allocator.Error!void {
        try self.emitIncrefInto(symbol, layout_idx, count, region, &self.stmt_buf);
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

    /// Emit a decref statement into the statement buffer.
    fn emitDecref(self: *RcInsertPass, symbol: Symbol, layout_idx: LayoutIdx, region: Region) Allocator.Error!void {
        try self.emitDecrefInto(symbol, layout_idx, region, &self.stmt_buf);
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
