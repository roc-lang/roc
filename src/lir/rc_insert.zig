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

    pub fn init(allocator: Allocator, store: *LirExprStore, layout_store: *const layout_mod.Store) RcInsertPass {
        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .symbol_use_counts = std.AutoHashMap(u64, u32).init(allocator),
            .symbol_layouts = std.AutoHashMap(u64, LayoutIdx).init(allocator),
        };
    }

    pub fn deinit(self: *RcInsertPass) void {
        self.symbol_use_counts.deinit();
        self.symbol_layouts.deinit();
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
    /// Wrapper around countUsesInto that targets self.symbol_use_counts.
    fn countUses(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!void {
        try self.countUsesInto(expr_id, &self.symbol_use_counts);
    }

    /// Count uses into a fresh local map and return it. Caller owns the map.
    fn countUsesLocal(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!std.AutoHashMap(u64, u32) {
        var local = std.AutoHashMap(u64, u32).init(self.allocator);
        errdefer local.deinit();
        try self.countUsesInto(expr_id, &local);
        return local;
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
            .break_expr,
            => {},
        }
    }

    /// Register a pattern's bound symbol with its layout (into self.symbol_use_counts).
    fn registerPatternSymbol(self: *RcInsertPass, pat_id: LirPatternId) Allocator.Error!void {
        try self.registerPatternSymbolInto(pat_id, &self.symbol_use_counts);
    }

    /// Register a pattern's bound symbol with its layout into a given target map.
    fn registerPatternSymbolInto(self: *RcInsertPass, pat_id: LirPatternId, target: *std.AutoHashMap(u64, u32)) Allocator.Error!void {
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
    fn processExpr(self: *RcInsertPass, expr_id: LirExprId) Allocator.Error!LirExprId {
        if (expr_id.isNone()) return expr_id;

        const expr = self.store.getExpr(expr_id);
        const region = self.store.getExprRegion(expr_id);

        return switch (expr) {
            .block => |block| self.processBlock(block.stmts, block.final_expr, block.result_layout, region),
            .if_then_else => |ite| self.processIfThenElse(ite.branches, ite.final_else, ite.result_layout, region),
            .when => |w| self.processWhen(w.value, w.value_layout, w.branches, w.result_layout, region),
            .lambda => |lam| self.processLambda(lam, region, expr_id),
            .closure => |clo| self.processClosure(clo, region, expr_id),
            .for_loop => |fl| self.processForLoop(fl, region, expr_id),
            .while_loop => |wl| self.processWhileLoop(wl, region, expr_id),
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
        stmts_span: LirStmtSpan,
        final_expr: LirExprId,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        const stmts = self.store.getStmts(stmts_span);

        // Use a local buffer to avoid reentrancy issues — processExpr may
        // recurse into another processBlock (e.g. lambda/loop bodies).
        var stmt_buf = std.ArrayList(LirStmt).empty;
        defer stmt_buf.deinit(self.allocator);

        var changed = false;

        // Process each statement
        for (stmts) |stmt| {
            // Recursively process the statement's expression
            const new_expr = try self.processExpr(stmt.expr);
            if (new_expr != stmt.expr) changed = true;

            // Add the (possibly updated) statement
            try stmt_buf.append(self.allocator, .{ .pattern = stmt.pattern, .expr = new_expr });

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
                                try self.emitIncrefInto(bind.symbol, bind.layout_idx, @intCast(use_count - 1), region, &stmt_buf);
                                changed = true;
                            }
                        }
                    },
                    else => {},
                }
            }
        }

        // Recursively process the final expression
        const new_final = try self.processExpr(final_expr);
        if (new_final != final_expr) changed = true;

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
                                try self.emitDecrefInto(bind.symbol, bind.layout_idx, region, &stmt_buf);
                                changed = true;
                            }
                        }
                    },
                    else => {},
                }
            }
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

        var new_branches = std.ArrayList(LirIfBranch).empty;
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
        value: LirExprId,
        value_layout: LayoutIdx,
        branches_span: LIR.LirWhenBranchSpan,
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
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

        var new_branches = std.ArrayList(LirWhenBranch).empty;
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

    /// Process a lambda expression.
    /// Lambda bodies are independent scopes — the calling convention provides
    /// 1 reference per parameter. We recurse into the body and emit RC ops
    /// for lambda parameters based on body-local use counts.
    fn processLambda(self: *RcInsertPass, lam: anytype, region: Region, expr_id: LirExprId) Allocator.Error!LirExprId {
        const new_body = try self.processExpr(lam.body);

        // Count uses locally within the lambda body
        var local_uses = try self.countUsesLocal(lam.body);
        defer local_uses.deinit();

        // Emit RC ops for lambda parameters
        var rc_stmts = std.ArrayList(LirStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        const params = self.store.getPatternSpan(lam.params);
        for (params) |pat_id| {
            if (pat_id.isNone()) continue;
            const pat = self.store.getPattern(pat_id);
            switch (pat) {
                .bind => |bind| {
                    if (!bind.symbol.isNone() and self.layoutNeedsRc(bind.layout_idx)) {
                        const key = @as(u64, @bitCast(bind.symbol));
                        const use_count = local_uses.get(key) orelse 0;
                        if (use_count == 0) {
                            try self.emitDecrefInto(bind.symbol, bind.layout_idx, region, &rc_stmts);
                        } else if (use_count > 1) {
                            try self.emitIncrefInto(bind.symbol, bind.layout_idx, @intCast(use_count - 1), region, &rc_stmts);
                        }
                    }
                },
                else => {},
            }
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
    fn processForLoop(self: *RcInsertPass, fl: anytype, region: Region, expr_id: LirExprId) Allocator.Error!LirExprId {
        const new_body = try self.processExpr(fl.body);

        // Count uses locally within the loop body
        var local_uses = try self.countUsesLocal(fl.body);
        defer local_uses.deinit();

        // Emit RC ops for the elem_pattern
        var rc_stmts = std.ArrayList(LirStmt).empty;
        defer rc_stmts.deinit(self.allocator);

        if (!fl.elem_pattern.isNone()) {
            const pat = self.store.getPattern(fl.elem_pattern);
            switch (pat) {
                .bind => |bind| {
                    if (!bind.symbol.isNone() and self.layoutNeedsRc(bind.layout_idx)) {
                        const key = @as(u64, @bitCast(bind.symbol));
                        const use_count = local_uses.get(key) orelse 0;
                        if (use_count == 0) {
                            try self.emitDecrefInto(bind.symbol, bind.layout_idx, region, &rc_stmts);
                        } else if (use_count > 1) {
                            try self.emitIncrefInto(bind.symbol, bind.layout_idx, @intCast(use_count - 1), region, &rc_stmts);
                        }
                    }
                },
                else => {},
            }
        }

        // If RC ops needed, wrap body in a block with RC stmts prepended
        var final_body = new_body;
        if (rc_stmts.items.len > 0) {
            const stmts_span = try self.store.addStmts(rc_stmts.items);
            final_body = try self.store.addExpr(.{ .block = .{
                .stmts = stmts_span,
                .final_expr = new_body,
                .result_layout = fl.elem_layout,
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

    /// Collect all symbols bound by a pattern into a set.
    fn collectPatternSymbols(self: *const RcInsertPass, pat_id: LirPatternId, set: *std.AutoHashMap(u64, void)) Allocator.Error!void {
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
        body: LirExprId,
        local_uses: *const std.AutoHashMap(u64, u32),
        symbols_in_any_branch: *const std.AutoHashMap(u64, void),
        result_layout: LayoutIdx,
        region: Region,
    ) Allocator.Error!LirExprId {
        var rc_stmts = std.ArrayList(LirStmt).empty;
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
        try stmts.append(self.allocator, .{
            .pattern = wildcard,
            .expr = incref_id,
        });
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
    const stmts = try env.lir_store.addStmts(&.{.{ .pattern = pat_x, .expr = int_lit }});
    const lookup_x = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_x, .layout_idx = i64_layout } }, Region.zero());

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup_x,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
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
        .{ .pattern = pat_s, .expr = str_lit },
        .{ .pattern = wildcard, .expr = lookup1 },
    });

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = lookup2,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
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
        const stmt_expr = env.lir_store.getExpr(stmt.expr);
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
    const stmts = try env.lir_store.addStmts(&.{.{ .pattern = pat_s, .expr = str_lit }});

    // Final expression is an i64 literal (s is unused)
    const int_lit = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = int_lit,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
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
        const stmt_expr = env.lir_store.getExpr(stmt.expr);
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
                const sub = countRcOps(store, stmt.expr);
                increfs += sub.increfs;
                decrefs += sub.decrefs;
            }
            const sub = countRcOps(store, block.final_expr);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .when => |w| {
            const branches = store.getWhenBranches(w.branches);
            for (branches) |branch| {
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
        .closure => |clo| {
            const sub = countRcOps(store, clo.lambda);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
        },
        .for_loop => |fl| {
            const sub = countRcOps(store, fl.body);
            increfs += sub.increfs;
            decrefs += sub.decrefs;
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
        .discriminant_switch,
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

test "RC branch-aware: symbol used in both when branches — no incref at binding" {
    // { s = "hello"; when cond is True -> s, False -> s }
    // s is used once in each branch => global count should be 1 (the when construct counts as 1)
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

    // Build the when: when cond is True -> s, False -> s
    const cond_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond, .layout_idx = i64_layout } }, Region.zero());
    const lookup_s1 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s2 = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());

    const wild_pat1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild_pat2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const when_branches = try env.lir_store.addWhenBranches(&.{
        .{ .pattern = wild_pat1, .guard = LirExprId.none, .body = lookup_s1 },
        .{ .pattern = wild_pat2, .guard = LirExprId.none, .body = lookup_s2 },
    });

    const when_expr = try env.lir_store.addExpr(.{ .when = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = when_branches,
        .result_layout = str_layout,
    } }, Region.zero());

    // Build block: { s = "hello"; <when> }
    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .pattern = pat_s, .expr = str_lit }});

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = when_expr,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // Check: no incref or decref anywhere in the result tree
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 0), rc.decrefs);
}

test "RC branch-aware: symbol used in one when branch only — decref in unused branch" {
    // { s = "hello"; when cond is True -> s, False -> 42 }
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

    const when_branches = try env.lir_store.addWhenBranches(&.{
        .{ .pattern = wild_pat1, .guard = LirExprId.none, .body = lookup_s },
        .{ .pattern = wild_pat2, .guard = LirExprId.none, .body = int_42 },
    });

    const when_expr = try env.lir_store.addExpr(.{ .when = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = when_branches,
        .result_layout = str_layout,
    } }, Region.zero());

    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .pattern = pat_s, .expr = str_lit }});

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = when_expr,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // No incref at binding (global count = 1), but decref in the False branch
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC branch-aware: symbol used twice in one branch — incref in that branch, decref in other" {
    // { s = "hello"; when cond is True -> binop(s, s), False -> 42 }
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
    } }, Region.zero());

    const wild_pat1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild_pat2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const when_branches = try env.lir_store.addWhenBranches(&.{
        .{ .pattern = wild_pat1, .guard = LirExprId.none, .body = binop_expr },
        .{ .pattern = wild_pat2, .guard = LirExprId.none, .body = int_42 },
    });

    const when_expr = try env.lir_store.addExpr(.{ .when = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = when_branches,
        .result_layout = str_layout,
    } }, Region.zero());

    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const stmts = try env.lir_store.addStmts(&.{.{ .pattern = pat_s, .expr = str_lit }});

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = when_expr,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // incref(1) in True branch, decref in False branch, no incref at binding
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 1), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}

test "RC branch-aware: symbol used outside and inside branches" {
    // { s = "hello"; _ = s; when cond is True -> s, False -> 42 }
    // s used outside (1) + when construct (1) = global count 2 => incref(1) at binding.
    // True branch: 1 use => no action. False branch: 0 uses => decref.
    const allocator = std.testing.allocator;

    var env = try testInit();
    try testInitLayoutStore(&env);
    defer testDeinit(&env);

    const str_layout: LayoutIdx = .str;
    const i64_layout: LayoutIdx = .i64;
    const sym_s = makeSymbol(1);
    const sym_cond = makeSymbol(2);

    // Build when
    const cond_expr = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_cond, .layout_idx = i64_layout } }, Region.zero());
    const lookup_s_branch = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const int_42 = try env.lir_store.addExpr(.{ .i64_literal = 42 }, Region.zero());

    const wild_pat1 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());
    const wild_pat2 = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = i64_layout } }, Region.zero());

    const when_branches = try env.lir_store.addWhenBranches(&.{
        .{ .pattern = wild_pat1, .guard = LirExprId.none, .body = lookup_s_branch },
        .{ .pattern = wild_pat2, .guard = LirExprId.none, .body = int_42 },
    });

    const when_expr = try env.lir_store.addExpr(.{ .when = .{
        .value = cond_expr,
        .value_layout = i64_layout,
        .branches = when_branches,
        .result_layout = str_layout,
    } }, Region.zero());

    // Build block: { s = "hello"; _ = s; <when> }
    const str_lit = try env.lir_store.addExpr(.{ .str_literal = base.StringLiteral.Idx.none }, Region.zero());
    const pat_s = try env.lir_store.addPattern(.{ .bind = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const lookup_s_outside = try env.lir_store.addExpr(.{ .lookup = .{ .symbol = sym_s, .layout_idx = str_layout } }, Region.zero());
    const wild_use = try env.lir_store.addPattern(.{ .wildcard = .{ .layout_idx = str_layout } }, Region.zero());

    const stmts = try env.lir_store.addStmts(&.{
        .{ .pattern = pat_s, .expr = str_lit },
        .{ .pattern = wild_use, .expr = lookup_s_outside },
    });

    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = when_expr,
        .result_layout = str_layout,
    } }, Region.zero());

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
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
        .{ .pattern = pat_s, .expr = str_lit },
        .{ .pattern = wild, .expr = lookup_s1 },
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

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
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

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
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

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
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
        .{ .pattern = wild, .expr = lookup_e1 },
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

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
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
    const closure_expr = try env.lir_store.addExpr(.{ .closure = .{
        .closure_layout = i64_layout,
        .lambda = lambda_expr,
        .captures = LIR.LirCaptureSpan.empty(),
        .representation = .{ .unwrapped_capture = .{ .capture_layout = i64_layout } },
        .recursion = .not_recursive,
        .self_recursive = .not_self_recursive,
        .is_bound_to_variable = false,
    } }, Region.zero());

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
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
    const stmts = try env.lir_store.addStmts(&.{.{ .pattern = pat_f, .expr = lambda_expr }});
    const int_0 = try env.lir_store.addExpr(.{ .i64_literal = 0 }, Region.zero());
    const block_expr = try env.lir_store.addExpr(.{ .block = .{
        .stmts = stmts,
        .final_expr = int_0,
        .result_layout = i64_layout,
    } }, Region.zero());

    var pass = RcInsertPass.init(allocator, &env.lir_store, &env.layout_store);
    defer pass.deinit();

    const result = try pass.insertRcOps(block_expr);

    // decref for unused str param s in the lambda body
    const rc = countRcOps(&env.lir_store, result);
    try std.testing.expectEqual(@as(u32, 0), rc.increfs);
    try std.testing.expectEqual(@as(u32, 1), rc.decrefs);
}
