//! Assigns stable RC reference identities to bindings, patterns, and lookups in LIR.
const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const lir = @import("LIR.zig");
const LirExprStore = @import("LirExprStore.zig");

const Allocator = std.mem.Allocator;

const LirExprId = lir.LirExprId;
const LirPatternId = lir.LirPatternId;
const LirStmt = lir.LirStmt;
const Symbol = lir.Symbol;
const LayoutIdx = @import("layout").Idx;

/// Stable identity for one RC-relevant binding/reference in normalized LIR.
pub const RefId = enum(u32) {
    _,

    /// Sentinel used for unassigned lookup/pattern slots.
    pub const none: RefId = @enumFromInt(std.math.maxInt(u32));

    /// Whether this ref slot is unassigned.
    pub fn isNone(self: RefId) bool {
        return self == none;
    }
};

/// How a normalized binding owns, borrows, or aliases a value.
pub const OwnerKind = union(enum) {
    unmanaged,
    owned,
    borrowed: RefId,
    retained: RefId,
};

/// RC-relevant metadata for one normalized binding/reference.
pub const RefInfo = struct {
    symbol: Symbol,
    layout_idx: LayoutIdx,
    reassignable: bool,
    is_local_bind: bool,
    owner_kind: OwnerKind,
    shadowed_ref: RefId,
};

/// Output of ownership normalization for one lowered LIR expression tree.
pub const Result = struct {
    ref_infos: std.ArrayList(RefInfo),
    lookup_ref_ids: std.ArrayList(RefId),
    pattern_ref_ids: std.ArrayList(RefId),
    allocator: Allocator,

    /// Release normalization side tables.
    pub fn deinit(self: *Result) void {
        self.ref_infos.deinit(self.allocator);
        self.lookup_ref_ids.deinit(self.allocator);
        self.pattern_ref_ids.deinit(self.allocator);
    }

    /// Get the normalized reference identity for a lookup expression, if any.
    pub fn getLookupRef(self: *const Result, expr_id: LirExprId) ?RefId {
        const idx = @intFromEnum(expr_id);
        if (idx >= self.lookup_ref_ids.items.len) return null;
        const ref_id = self.lookup_ref_ids.items[idx];
        return if (ref_id.isNone()) null else ref_id;
    }

    /// Get the normalized reference identity for a pattern node, if any.
    pub fn getPatternRef(self: *const Result, pat_id: LirPatternId) ?RefId {
        const idx = @intFromEnum(pat_id);
        if (idx >= self.pattern_ref_ids.items.len) return null;
        const ref_id = self.pattern_ref_ids.items[idx];
        return if (ref_id.isNone()) null else ref_id;
    }

    /// Read binding metadata for a normalized reference identity.
    pub fn getRefInfo(self: *const Result, ref_id: RefId) RefInfo {
        return self.ref_infos.items[@intFromEnum(ref_id)];
    }
};

const ScopeChange = struct {
    symbol_key: u64,
    previous_ref: RefId,
    had_previous: bool,
};

const Analyzer = struct {
    allocator: Allocator,
    store: *const LirExprStore,
    result: Result,
    visible_refs: std.AutoHashMap(u64, RefId),
    scope_changes: std.ArrayList(ScopeChange),
    external_refs: std.AutoHashMap(u64, RefId),

    fn init(allocator: Allocator, store: *const LirExprStore) Allocator.Error!Analyzer {
        var result = Result{
            .ref_infos = std.ArrayList(RefInfo).empty,
            .lookup_ref_ids = std.ArrayList(RefId).empty,
            .pattern_ref_ids = std.ArrayList(RefId).empty,
            .allocator = allocator,
        };
        try result.lookup_ref_ids.resize(allocator, store.exprs.items.len);
        @memset(result.lookup_ref_ids.items, RefId.none);
        try result.pattern_ref_ids.resize(allocator, store.patterns.items.len);
        @memset(result.pattern_ref_ids.items, RefId.none);

        return .{
            .allocator = allocator,
            .store = store,
            .result = result,
            .visible_refs = std.AutoHashMap(u64, RefId).init(allocator),
            .scope_changes = std.ArrayList(ScopeChange).empty,
            .external_refs = std.AutoHashMap(u64, RefId).init(allocator),
        };
    }

    fn deinit(self: *Analyzer) void {
        self.visible_refs.deinit();
        self.scope_changes.deinit(self.allocator);
        self.external_refs.deinit();
    }

    fn finish(self: *Analyzer) Result {
        return self.result;
    }

    fn pushScope(self: *Analyzer) usize {
        return self.scope_changes.items.len;
    }

    fn popScope(self: *Analyzer, mark: usize) void {
        while (self.scope_changes.items.len > mark) {
            const change = self.scope_changes.pop().?;
            if (change.had_previous) {
                self.visible_refs.put(change.symbol_key, change.previous_ref) catch unreachable;
            } else {
                _ = self.visible_refs.remove(change.symbol_key);
            }
        }
    }

    fn addLocalRef(
        self: *Analyzer,
        symbol: Symbol,
        layout_idx: LayoutIdx,
        reassignable: bool,
        owner_kind: OwnerKind,
    ) Allocator.Error!RefId {
        const ref_id: RefId = @enumFromInt(@as(u32, @intCast(self.result.ref_infos.items.len)));
        const symbol_key: u64 = @bitCast(symbol);
        const previous = self.visible_refs.get(symbol_key);
        try self.scope_changes.append(self.allocator, .{
            .symbol_key = symbol_key,
            .previous_ref = previous orelse RefId.none,
            .had_previous = previous != null,
        });
        try self.visible_refs.put(symbol_key, ref_id);
        try self.result.ref_infos.append(self.allocator, .{
            .symbol = symbol,
            .layout_idx = layout_idx,
            .reassignable = reassignable,
            .is_local_bind = true,
            .owner_kind = owner_kind,
            .shadowed_ref = previous orelse RefId.none,
        });
        return ref_id;
    }

    fn getOrCreateExternalRef(
        self: *Analyzer,
        symbol: Symbol,
        layout_idx: LayoutIdx,
    ) Allocator.Error!RefId {
        const symbol_key: u64 = @bitCast(symbol);
        if (self.external_refs.get(symbol_key)) |ref_id| {
            return ref_id;
        }

        const ref_id: RefId = @enumFromInt(@as(u32, @intCast(self.result.ref_infos.items.len)));
        try self.external_refs.put(symbol_key, ref_id);
        try self.result.ref_infos.append(self.allocator, .{
            .symbol = symbol,
            .layout_idx = layout_idx,
            .reassignable = false,
            .is_local_bind = false,
            .owner_kind = .owned,
            .shadowed_ref = RefId.none,
        });
        return ref_id;
    }

    fn resolveLookup(self: *Analyzer, expr_id: LirExprId, symbol: Symbol, layout_idx: LayoutIdx) Allocator.Error!void {
        const symbol_key: u64 = @bitCast(symbol);
        const ref_id = if (self.visible_refs.get(symbol_key)) |local_ref|
            local_ref
        else
            try self.getOrCreateExternalRef(symbol, layout_idx);
        self.result.lookup_ref_ids.items[@intFromEnum(expr_id)] = ref_id;
    }

    fn registerPattern(
        self: *Analyzer,
        pat_id: LirPatternId,
        owner_kind: OwnerKind,
    ) Allocator.Error!void {
        if (pat_id.isNone()) return;

        switch (self.store.getPattern(pat_id)) {
            .bind => |bind| {
                if (bind.symbol.isNone()) return;
                const ref_id = try self.addLocalRef(bind.symbol, bind.layout_idx, bind.reassignable, owner_kind);
                self.result.pattern_ref_ids.items[@intFromEnum(pat_id)] = ref_id;
            },
            .as_pattern => |as_pat| {
                if (!as_pat.symbol.isNone()) {
                    const ref_id = try self.addLocalRef(as_pat.symbol, as_pat.layout_idx, as_pat.reassignable, owner_kind);
                    self.result.pattern_ref_ids.items[@intFromEnum(pat_id)] = ref_id;
                }
                try self.registerPattern(as_pat.inner, owner_kind);
            },
            .tag => |tag_pat| {
                for (self.store.getPatternSpan(tag_pat.args)) |arg| {
                    try self.registerPattern(arg, owner_kind);
                }
            },
            .struct_ => |struct_pat| {
                for (self.store.getPatternSpan(struct_pat.fields)) |field| {
                    try self.registerPattern(field, owner_kind);
                }
            },
            .list => |list_pat| {
                for (self.store.getPatternSpan(list_pat.prefix)) |prefix| {
                    try self.registerPattern(prefix, owner_kind);
                }
                try self.registerPattern(list_pat.rest, owner_kind);
                for (self.store.getPatternSpan(list_pat.suffix)) |suffix| {
                    try self.registerPattern(suffix, owner_kind);
                }
            },
            .wildcard, .int_literal, .float_literal, .str_literal => {},
        }
    }

    fn ownerKindForOwnedExpr(self: *Analyzer, expr_id: LirExprId) OwnerKind {
        return switch (self.store.getExpr(expr_id)) {
            .block => |block| self.ownerKindForOwnedExpr(block.final_expr),
            .borrow_scope => |scope| self.ownerKindForOwnedExpr(scope.body),
            .dbg => |dbg_expr| self.ownerKindForOwnedExpr(dbg_expr.expr),
            .nominal => |nominal| self.ownerKindForOwnedExpr(nominal.backing_expr),
            .incref => |inc| switch (self.store.getExpr(inc.value)) {
                .lookup => blk: {
                    const ref_id = self.result.lookup_ref_ids.items[@intFromEnum(inc.value)];
                    break :blk if (ref_id.isNone()) .owned else OwnerKind{ .retained = ref_id };
                },
                else => .owned,
            },
            .struct_access => |sa| switch (self.store.getExpr(sa.struct_expr)) {
                .lookup => blk: {
                    const ref_id = self.result.lookup_ref_ids.items[@intFromEnum(sa.struct_expr)];
                    break :blk if (ref_id.isNone()) .owned else OwnerKind{ .borrowed = ref_id };
                },
                else => .owned,
            },
            .tag_payload_access => |tpa| switch (self.store.getExpr(tpa.value)) {
                .lookup => blk: {
                    const ref_id = self.result.lookup_ref_ids.items[@intFromEnum(tpa.value)];
                    break :blk if (ref_id.isNone()) .owned else OwnerKind{ .borrowed = ref_id };
                },
                else => .owned,
            },
            .semantic_low_level => |ll| switch (ll.op) {
                .list_get_unsafe => blk: {
                    const args = self.store.getExprSpan(ll.args);
                    if (args.len == 0) break :blk .owned;
                    break :blk switch (self.store.getExpr(args[0])) {
                        .lookup => blk2: {
                            const ref_id = self.result.lookup_ref_ids.items[@intFromEnum(args[0])];
                            break :blk2 if (ref_id.isNone()) .owned else OwnerKind{ .borrowed = ref_id };
                        },
                        else => .owned,
                    };
                },
                else => .owned,
            },
            .low_level => |ll| switch (ll.op) {
                .list_get_unsafe => blk: {
                    const args = self.store.getExprSpan(ll.args);
                    if (args.len == 0) break :blk .owned;
                    break :blk switch (self.store.getExpr(args[0])) {
                        .lookup => blk2: {
                            const ref_id = self.result.lookup_ref_ids.items[@intFromEnum(args[0])];
                            break :blk2 if (ref_id.isNone()) .owned else OwnerKind{ .borrowed = ref_id };
                        },
                        else => .owned,
                    };
                },
                else => .owned,
            },
            else => .owned,
        };
    }

    fn analyzeStmtOwned(self: *Analyzer, stmt: LirStmt) Allocator.Error!void {
        switch (stmt) {
            .decl, .mutate => |binding| {
                try self.analyzeExpr(binding.expr);
                const owner_kind = self.ownerKindForOwnedExpr(binding.expr);
                if (builtin.mode == .Debug) {
                    switch (self.store.getPattern(binding.pattern)) {
                        .bind => |bind| {
                            const expr_tag = self.store.getExpr(binding.expr);
                            const raw = bind.symbol.raw();
                            if (raw == 4294967064 or raw == 4294967096 or raw == 4294967104) {
                                std.debug.print(
                                    "normalize bind symbol={d} layout={d} expr_tag={s} owner_kind={s}\n",
                                    .{ bind.symbol.raw(), @intFromEnum(bind.layout_idx), @tagName(expr_tag), @tagName(owner_kind) },
                                );
                            }
                        },
                        else => {},
                    }
                }
                try self.registerPattern(binding.pattern, owner_kind);
            },
            .cell_init, .cell_store => |binding| {
                try self.analyzeExpr(binding.expr);
            },
            .cell_drop => {},
        }
    }

    fn analyzeBorrowBinding(self: *Analyzer, binding: lir.LirBorrowBinding) Allocator.Error!void {
        try self.analyzeExpr(binding.expr);
        const owner_kind = switch (self.store.getExpr(binding.expr)) {
            .lookup => |lookup| blk: {
                const ref_id = self.result.lookup_ref_ids.items[@intFromEnum(binding.expr)];
                _ = lookup;
                break :blk OwnerKind{ .borrowed = ref_id };
            },
            .cell_load => .owned,
            else => .unmanaged,
        };
        try self.registerPattern(binding.pattern, owner_kind);
    }

    fn analyzeExpr(self: *Analyzer, expr_id: LirExprId) Allocator.Error!void {
        if (expr_id.isNone()) return;

        switch (self.store.getExpr(expr_id)) {
            .lookup => |lookup| {
                if (!lookup.symbol.isNone()) {
                    try self.resolveLookup(expr_id, lookup.symbol, lookup.layout_idx);
                }
            },
            .cell_load => {},
            .call => |call| {
                try self.analyzeExpr(call.fn_expr);
                for (self.store.getExprSpan(call.args)) |arg| try self.analyzeExpr(arg);
            },
            .lambda => |lam| {
                const mark = self.pushScope();
                defer self.popScope(mark);
                for (self.store.getPatternSpan(lam.params)) |param| {
                    if (builtin.mode == .Debug) {
                        switch (self.store.getPattern(param)) {
                            .bind => |bind| {
                                const raw = bind.symbol.raw();
                                if (raw == 4294967064 or raw == 4294967096 or raw == 4294967104) {
                                    std.debug.print(
                                        "normalize lambda param symbol={d} layout={d}\n",
                                        .{ bind.symbol.raw(), @intFromEnum(bind.layout_idx) },
                                    );
                                }
                            },
                            else => {},
                        }
                    }
                    try self.registerPattern(param, .owned);
                }
                try self.analyzeExpr(lam.body);
            },
            .list => |list_expr| {
                for (self.store.getExprSpan(list_expr.elems)) |elem| try self.analyzeExpr(elem);
            },
            .struct_ => |struct_expr| {
                for (self.store.getExprSpan(struct_expr.fields)) |field| try self.analyzeExpr(field);
            },
            .tag => |tag_expr| {
                for (self.store.getExprSpan(tag_expr.args)) |arg| try self.analyzeExpr(arg);
            },
            .if_then_else => |ite| {
                for (self.store.getIfBranches(ite.branches)) |branch| {
                    try self.analyzeExpr(branch.cond);
                    const mark = self.pushScope();
                    defer self.popScope(mark);
                    try self.analyzeExpr(branch.body);
                }
                const else_mark = self.pushScope();
                defer self.popScope(else_mark);
                try self.analyzeExpr(ite.final_else);
            },
            .match_expr => |match_expr| {
                try self.analyzeExpr(match_expr.value);
                const scrutinee_ref = self.result.getLookupRef(match_expr.value);
                for (self.store.getMatchBranches(match_expr.branches)) |branch| {
                    const mark = self.pushScope();
                    defer self.popScope(mark);
                    try self.registerPattern(branch.pattern, if (scrutinee_ref) |ref_id| OwnerKind{ .borrowed = ref_id } else .unmanaged);
                    try self.analyzeExpr(branch.guard);
                    try self.analyzeExpr(branch.body);
                }
            },
            .block => |block| {
                const mark = self.pushScope();
                defer self.popScope(mark);
                for (self.store.getStmts(block.stmts)) |stmt| {
                    try self.analyzeStmtOwned(stmt);
                }
                try self.analyzeExpr(block.final_expr);
            },
            .borrow_scope => |scope| {
                const mark = self.pushScope();
                defer self.popScope(mark);
                for (self.store.getBorrowBindings(scope.bindings)) |binding| {
                    try self.analyzeBorrowBinding(binding);
                }
                try self.analyzeExpr(scope.body);
            },
            .early_return => |ret| try self.analyzeExpr(ret.expr),
            .semantic_low_level => |ll| {
                for (self.store.getExprSpan(ll.args)) |arg| try self.analyzeExpr(arg);
            },
            .low_level => |ll| {
                for (self.store.getExprSpan(ll.args)) |arg| try self.analyzeExpr(arg);
            },
            .dbg => |d| try self.analyzeExpr(d.expr),
            .expect => |e| {
                try self.analyzeExpr(e.cond);
                try self.analyzeExpr(e.body);
            },
            .nominal => |n| try self.analyzeExpr(n.backing_expr),
            .str_concat => |parts| {
                for (self.store.getExprSpan(parts)) |part| try self.analyzeExpr(part);
            },
            .int_to_str => |its| try self.analyzeExpr(its.value),
            .float_to_str => |fts| try self.analyzeExpr(fts.value),
            .dec_to_str => |value| try self.analyzeExpr(value),
            .str_escape_and_quote => |value| try self.analyzeExpr(value),
            .discriminant_switch => |ds| {
                try self.analyzeExpr(ds.value);
                for (self.store.getExprSpan(ds.branches)) |branch| try self.analyzeExpr(branch);
            },
            .tag_payload_access => |tpa| try self.analyzeExpr(tpa.value),
            .for_loop => |loop_expr| {
                try self.analyzeExpr(loop_expr.list_expr);
                const mark = self.pushScope();
                defer self.popScope(mark);
                const owner_kind = if (self.result.getLookupRef(loop_expr.list_expr)) |ref_id|
                    OwnerKind{ .borrowed = ref_id }
                else
                    .unmanaged;
                try self.registerPattern(loop_expr.elem_pattern, owner_kind);
                try self.analyzeExpr(loop_expr.body);
            },
            .while_loop => |wl| {
                try self.analyzeExpr(wl.cond);
                const mark = self.pushScope();
                defer self.popScope(mark);
                try self.analyzeExpr(wl.body);
            },
            .hosted_call => |hc| {
                for (self.store.getExprSpan(hc.args)) |arg| try self.analyzeExpr(arg);
            },
            .struct_access => |sa| try self.analyzeExpr(sa.struct_expr),
            .incref => |op| try self.analyzeExpr(op.value),
            .decref => |op| try self.analyzeExpr(op.value),
            .free => |op| try self.analyzeExpr(op.value),
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
};

/// Normalize bindings and lookups into stable RC reference identities.
pub fn analyze(allocator: Allocator, store: *const LirExprStore, root_expr: LirExprId) Allocator.Error!Result {
    var analyzer = try Analyzer.init(allocator, store);
    errdefer {
        var tmp = analyzer.finish();
        tmp.deinit();
    }
    defer analyzer.deinit();

    try analyzer.analyzeExpr(root_expr);
    return analyzer.finish();
}

fn testSymbolFromIdent(ident: base.Ident.Idx) Symbol {
    return Symbol.fromRaw(@as(u64, @as(u32, @bitCast(ident))));
}

test "ownership normalization distinguishes shadowed binds and resolves lookups" {
    const Region = base.Region;
    var store = LirExprStore.init(std.testing.allocator);
    defer store.deinit();

    const ident_x = base.Ident.Idx{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = 1 };
    const symbol_x = testSymbolFromIdent(ident_x);

    const pat1 = try store.addPattern(.{ .bind = .{ .symbol = symbol_x, .layout_idx = .str, .reassignable = false } }, Region.zero());
    const pat2 = try store.addPattern(.{ .bind = .{ .symbol = symbol_x, .layout_idx = .str, .reassignable = false } }, Region.zero());
    const lit = try store.addExpr(.{ .str_literal = try store.strings.insert(std.testing.allocator, "hi") }, Region.zero());
    const lookup = try store.addExpr(.{ .lookup = .{ .symbol = symbol_x, .layout_idx = .str } }, Region.zero());
    const stmts = try store.addStmts(&.{
        .{ .decl = .{ .pattern = pat1, .expr = lit } },
        .{ .decl = .{ .pattern = pat2, .expr = lit } },
    });
    const block = try store.addExpr(.{ .block = .{ .stmts = stmts, .final_expr = lookup, .result_layout = .str } }, Region.zero());

    var result = try analyze(std.testing.allocator, &store, block);
    defer result.deinit();

    const first_ref = result.getPatternRef(pat1).?;
    const second_ref = result.getPatternRef(pat2).?;
    try std.testing.expect(first_ref != second_ref);
    try std.testing.expect(result.getLookupRef(lookup).? == second_ref);
    try std.testing.expect(result.getRefInfo(second_ref).shadowed_ref == first_ref);
}
