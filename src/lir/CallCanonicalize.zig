const std = @import("std");
const base = @import("base");

const LIR = @import("LIR.zig");
const LirExprStore = @import("LirExprStore.zig");

const Allocator = std.mem.Allocator;
const CallTarget = LIR.CallTarget;
const CFStmtId = LIR.CFStmtId;
const LirExprId = LIR.LirExprId;
const LirPatternId = LIR.LirPatternId;
const Symbol = LIR.Symbol;
const Region = base.Region;

const CachedCallableDef = union(enum) {
    resolving,
    resolved: ?LirExprId,
};

const CachedDirectSymbol = union(enum) {
    resolving,
    resolved: ?Symbol,
};

const LocalLookup = union(enum) {
    shadowed,
    target: CallTarget,
};

pub const CallCanonicalizePass = struct {
    allocator: Allocator,
    store: *LirExprStore,
    local_bindings: std.ArrayList(LocalBinding),
    contextual_symbol_stack: std.ArrayList(u64),
    seed_callable_defs: std.AutoHashMap(u64, LirExprId),
    callable_def_cache: std.AutoHashMap(u64, CachedCallableDef),
    direct_symbol_cache: std.AutoHashMap(u64, CachedDirectSymbol),

    const Self = @This();

    const LocalBinding = struct {
        symbol: Symbol,
        target: ?CallTarget,
    };

    pub fn init(allocator: Allocator, store: *LirExprStore) Self {
        return .{
            .allocator = allocator,
            .store = store,
            .local_bindings = .empty,
            .contextual_symbol_stack = .empty,
            .seed_callable_defs = std.AutoHashMap(u64, LirExprId).init(allocator),
            .callable_def_cache = std.AutoHashMap(u64, CachedCallableDef).init(allocator),
            .direct_symbol_cache = std.AutoHashMap(u64, CachedDirectSymbol).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.local_bindings.deinit(self.allocator);
        self.contextual_symbol_stack.deinit(self.allocator);
        self.seed_callable_defs.deinit();
        self.callable_def_cache.deinit();
        self.direct_symbol_cache.deinit();
    }

    pub fn run(self: *Self, roots: []const LirExprId) Allocator.Error!void {
        try self.rebuildCallableDefs();

        for (roots) |root| {
            const mark = self.local_bindings.items.len;
            try self.visitExpr(root, mark);
            self.local_bindings.shrinkRetainingCapacity(mark);
        }

        var def_iter = self.store.symbol_defs.iterator();
        while (def_iter.next()) |entry| {
            const mark = self.local_bindings.items.len;
            try self.visitExpr(entry.value_ptr.*, mark);
            self.local_bindings.shrinkRetainingCapacity(mark);
        }

        for (self.store.getProcs()) |proc| {
            const mark = self.local_bindings.items.len;
            for (self.store.getPatternSpan(proc.args)) |arg_pat| {
                try self.recordPatternBindings(arg_pat, null);
            }
            try self.visitCFStmt(proc.body, mark);
            self.local_bindings.shrinkRetainingCapacity(mark);
        }
    }

    fn rebuildCallableDefs(self: *Self) Allocator.Error!void {
        self.seed_callable_defs.clearRetainingCapacity();
        var seed_iter = self.store.callable_defs.iterator();
        while (seed_iter.next()) |entry| {
            try self.seed_callable_defs.put(entry.key_ptr.*, entry.value_ptr.*);
        }

        self.store.callable_defs.clearRetainingCapacity();
        self.callable_def_cache.clearRetainingCapacity();
        self.direct_symbol_cache.clearRetainingCapacity();

        var def_iter = self.store.symbol_defs.iterator();
        while (def_iter.next()) |entry| {
            const symbol: Symbol = @bitCast(entry.key_ptr.*);
            if (self.resolveCallableDefForGlobalSymbol(symbol)) |callable_def| {
                try self.store.setCallableDef(symbol, callable_def);
            }
        }
    }

    fn visitCFStmt(self: *Self, stmt_id: CFStmtId, scope_mark: usize) Allocator.Error!void {
        if (stmt_id.isNone()) return;

        switch (self.store.getCFStmt(stmt_id)) {
            .let_stmt => |let_stmt| {
                try self.visitExpr(let_stmt.value, scope_mark);
                const inner_mark = self.local_bindings.items.len;
                try self.recordPatternBindings(let_stmt.pattern, self.resolveCanonicalCallee(let_stmt.value, scope_mark));
                try self.visitCFStmt(let_stmt.next, scope_mark);
                self.local_bindings.shrinkRetainingCapacity(inner_mark);
            },
            .join => |join| {
                const body_mark = self.local_bindings.items.len;
                for (self.store.getPatternSpan(join.params)) |param_pat| {
                    try self.recordPatternBindings(param_pat, null);
                }
                try self.visitCFStmt(join.body, body_mark);
                self.local_bindings.shrinkRetainingCapacity(body_mark);

                try self.visitCFStmt(join.remainder, scope_mark);
            },
            .jump => |jump| {
                for (self.store.getExprSpan(jump.args)) |arg| try self.visitExpr(arg, scope_mark);
            },
            .ret => |ret_stmt| try self.visitExpr(ret_stmt.value, scope_mark),
            .expr_stmt => |expr_stmt| {
                try self.visitExpr(expr_stmt.value, scope_mark);
                try self.visitCFStmt(expr_stmt.next, scope_mark);
            },
            .switch_stmt => |switch_stmt| {
                try self.visitExpr(switch_stmt.cond, scope_mark);
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.visitCFStmt(branch.body, scope_mark);
                }
                try self.visitCFStmt(switch_stmt.default_branch, scope_mark);
            },
            .match_stmt => |match_stmt| {
                try self.visitExpr(match_stmt.value, scope_mark);
                for (self.store.getCFMatchBranches(match_stmt.branches)) |branch| {
                    const branch_mark = self.local_bindings.items.len;
                    try self.recordPatternBindings(branch.pattern, null);
                    if (!branch.guard.isNone()) try self.visitExpr(branch.guard, scope_mark);
                    try self.visitCFStmt(branch.body, scope_mark);
                    self.local_bindings.shrinkRetainingCapacity(branch_mark);
                }
            },
        }
    }

    fn visitExpr(self: *Self, expr_id: LirExprId, scope_mark: usize) Allocator.Error!void {
        if (expr_id.isNone()) return;

        const expr_ptr = self.store.getExprPtr(expr_id);
        switch (expr_ptr.*) {
            .block => |block| {
                const block_mark = self.local_bindings.items.len;
                for (self.store.getStmts(block.stmts)) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| {
                            try self.visitExpr(binding.expr, scope_mark);
                            try self.recordPatternBindings(binding.pattern, self.resolveCanonicalCallee(binding.expr, scope_mark));
                        },
                        .cell_init, .cell_store => |binding| try self.visitExpr(binding.expr, scope_mark),
                        .cell_drop => {},
                    }
                }
                try self.visitExpr(block.final_expr, scope_mark);
                self.local_bindings.shrinkRetainingCapacity(block_mark);
            },
            .lambda => |lambda| {
                const lambda_mark = self.local_bindings.items.len;
                for (self.store.getPatternSpan(lambda.params)) |param_pat| {
                    try self.recordPatternBindings(param_pat, null);
                }
                try self.visitExpr(lambda.body, scope_mark);
                self.local_bindings.shrinkRetainingCapacity(lambda_mark);
            },
            .if_then_else => |ite| {
                for (self.store.getIfBranches(ite.branches)) |branch| {
                    try self.visitExpr(branch.cond, scope_mark);
                    try self.visitExpr(branch.body, scope_mark);
                }
                try self.visitExpr(ite.final_else, scope_mark);
            },
            .match_expr => |match_expr| {
                try self.visitExpr(match_expr.value, scope_mark);
                for (self.store.getMatchBranches(match_expr.branches)) |branch| {
                    const branch_mark = self.local_bindings.items.len;
                    try self.recordPatternBindings(branch.pattern, null);
                    if (!branch.guard.isNone()) try self.visitExpr(branch.guard, scope_mark);
                    try self.visitExpr(branch.body, scope_mark);
                    self.local_bindings.shrinkRetainingCapacity(branch_mark);
                }
            },
            .for_loop => |for_loop| {
                try self.visitExpr(for_loop.list_expr, scope_mark);
                const loop_mark = self.local_bindings.items.len;
                try self.recordPatternBindings(for_loop.elem_pattern, null);
                try self.visitExpr(for_loop.body, scope_mark);
                self.local_bindings.shrinkRetainingCapacity(loop_mark);
            },
            .while_loop => |while_loop| {
                try self.visitExpr(while_loop.cond, scope_mark);
                try self.visitExpr(while_loop.body, scope_mark);
            },
            .call => |*call| {
                switch (call.callee) {
                    .expr => |callee_expr| {
                        try self.visitExpr(callee_expr, scope_mark);
                        if (self.resolveCanonicalCallee(callee_expr, scope_mark)) |target| {
                            call.callee = target;
                        }
                    },
                    .direct => {},
                }
                for (self.store.getExprSpan(call.args)) |arg| try self.visitExpr(arg, scope_mark);
            },
            .low_level => |ll| for (self.store.getExprSpan(ll.args)) |arg| try self.visitExpr(arg, scope_mark),
            .hosted_call => |hc| for (self.store.getExprSpan(hc.args)) |arg| try self.visitExpr(arg, scope_mark),
            .list => |list_expr| for (self.store.getExprSpan(list_expr.elems)) |elem| try self.visitExpr(elem, scope_mark),
            .struct_ => |struct_expr| for (self.store.getExprSpan(struct_expr.fields)) |field| try self.visitExpr(field, scope_mark),
            .tag => |tag_expr| for (self.store.getExprSpan(tag_expr.args)) |arg| try self.visitExpr(arg, scope_mark),
            .struct_access => |sa| try self.visitExpr(sa.struct_expr, scope_mark),
            .tag_payload_access => |tpa| try self.visitExpr(tpa.value, scope_mark),
            .nominal => |nominal| try self.visitExpr(nominal.backing_expr, scope_mark),
            .dbg => |dbg_expr| try self.visitExpr(dbg_expr.expr, scope_mark),
            .expect => |expect_expr| {
                try self.visitExpr(expect_expr.cond, scope_mark);
                try self.visitExpr(expect_expr.body, scope_mark);
            },
            .early_return => |ret_expr| try self.visitExpr(ret_expr.expr, scope_mark),
            .str_concat => |parts| for (self.store.getExprSpan(parts)) |part| try self.visitExpr(part, scope_mark),
            .int_to_str => |its| try self.visitExpr(its.value, scope_mark),
            .float_to_str => |fts| try self.visitExpr(fts.value, scope_mark),
            .dec_to_str => |value| try self.visitExpr(value, scope_mark),
            .str_escape_and_quote => |value| try self.visitExpr(value, scope_mark),
            .discriminant_switch => |ds| {
                try self.visitExpr(ds.value, scope_mark);
                for (self.store.getExprSpan(ds.branches)) |branch| try self.visitExpr(branch, scope_mark);
            },
            .incref => |op| try self.visitExpr(op.value, scope_mark),
            .decref => |op| try self.visitExpr(op.value, scope_mark),
            .free => |op| try self.visitExpr(op.value, scope_mark),
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
            => {},
        }
    }

    fn resolveCanonicalCallee(self: *Self, expr_id: LirExprId, scope_mark: usize) ?CallTarget {
        if (expr_id.isNone()) return null;

        return switch (self.store.getExpr(expr_id)) {
            .lambda => .{ .expr = expr_id },
            .nominal => |nominal| blk: {
                const backing_target = self.resolveCanonicalCallee(nominal.backing_expr, scope_mark) orelse break :blk null;
                break :blk switch (backing_target) {
                    .direct => |symbol| .{ .direct = symbol },
                    .expr => .{ .expr = expr_id },
                };
            },
            .lookup => |lookup| blk: {
                if (self.resolveLocalBinding(lookup.symbol, scope_mark)) |local| {
                    break :blk switch (local) {
                        .shadowed => null,
                        .target => |target| target,
                    };
                }
                if (self.resolveCanonicalCalleeFromSymbolDef(lookup.symbol, scope_mark)) |target| {
                    break :blk target;
                }
                if (self.resolveDirectSymbolForGlobalSymbol(lookup.symbol)) |symbol| {
                    break :blk .{ .direct = symbol };
                }
                break :blk null;
            },
            .block => |block| blk: {
                const block_mark = self.local_bindings.items.len;
                defer self.local_bindings.shrinkRetainingCapacity(block_mark);

                for (self.store.getStmts(block.stmts)) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| self.recordPatternBindings(binding.pattern, self.resolveCanonicalCallee(binding.expr, scope_mark)) catch return null,
                        .cell_init, .cell_store, .cell_drop => {},
                    }
                }

                break :blk self.resolveCanonicalCallee(block.final_expr, scope_mark);
            },
            .struct_access => |sa| self.resolveCanonicalStructField(sa.struct_expr, sa.field_idx, scope_mark),
            else => null,
        };
    }

    fn resolveCanonicalStructField(
        self: *Self,
        expr_id: LirExprId,
        field_idx: u16,
        scope_mark: usize,
    ) ?CallTarget {
        if (expr_id.isNone()) return null;

        return switch (self.store.getExpr(expr_id)) {
            .struct_ => |struct_expr| blk: {
                const fields = self.store.getExprSpan(struct_expr.fields);
                if (field_idx >= fields.len) break :blk null;
                break :blk self.resolveCanonicalCallee(fields[field_idx], scope_mark);
            },
            .nominal => |nominal| self.resolveCanonicalStructField(nominal.backing_expr, field_idx, scope_mark),
            .block => |block| blk: {
                const block_mark = self.local_bindings.items.len;
                defer self.local_bindings.shrinkRetainingCapacity(block_mark);

                for (self.store.getStmts(block.stmts)) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| self.recordPatternBindings(binding.pattern, self.resolveCanonicalCallee(binding.expr, scope_mark)) catch return null,
                        .cell_init, .cell_store, .cell_drop => {},
                    }
                }

                break :blk self.resolveCanonicalStructField(block.final_expr, field_idx, scope_mark);
            },
            .lookup => |lookup| blk: {
                if (self.resolveLocalBinding(lookup.symbol, scope_mark)) |local| {
                    break :blk switch (local) {
                        .shadowed => null,
                        .target => null,
                    };
                }

                const global_scope_mark = self.local_bindings.items.len;
                const def_expr_id = self.store.getSymbolDef(lookup.symbol) orelse break :blk null;
                break :blk self.resolveCanonicalStructField(def_expr_id, field_idx, global_scope_mark);
            },
            else => null,
        };
    }

    fn resolveLocalBinding(self: *Self, symbol: Symbol, scope_mark: usize) ?LocalLookup {
        var i = self.local_bindings.items.len;
        while (i > scope_mark) {
            i -= 1;
            const binding = self.local_bindings.items[i];
            if (!binding.symbol.eql(symbol)) continue;

            if (binding.target) |target| {
                return .{ .target = target };
            }
            return .shadowed;
        }
        return null;
    }

    fn resolveCanonicalCalleeFromSymbolDef(self: *Self, symbol: Symbol, scope_mark: usize) ?CallTarget {
        const key: u64 = @bitCast(symbol);
        for (self.contextual_symbol_stack.items) |active_key| {
            if (active_key == key) return null;
        }

        const def_expr_id = self.store.getSymbolDef(symbol) orelse return null;
        self.contextual_symbol_stack.append(self.allocator, key) catch return null;
        defer _ = self.contextual_symbol_stack.pop();

        return self.resolveCanonicalCallee(def_expr_id, scope_mark);
    }

    fn resolveCallableDefForGlobalSymbol(self: *Self, symbol: Symbol) ?LirExprId {
        const key: u64 = @bitCast(symbol);
        if (self.callable_def_cache.get(key)) |entry| {
            return switch (entry) {
                .resolving => self.seed_callable_defs.get(key),
                .resolved => |resolved| resolved,
            };
        }

        self.callable_def_cache.put(key, .resolving) catch return null;

        const scope_mark = self.local_bindings.items.len;
        const result = blk: {
            const def_expr_id = self.store.getSymbolDef(symbol) orelse break :blk null;
            break :blk self.resolveCallableDefExpr(def_expr_id, scope_mark);
        } orelse self.seed_callable_defs.get(key);

        self.callable_def_cache.put(key, .{ .resolved = result }) catch return null;
        return result;
    }

    fn resolveCallableDefExpr(self: *Self, expr_id: LirExprId, scope_mark: usize) ?LirExprId {
        if (expr_id.isNone()) return null;

        return switch (self.store.getExpr(expr_id)) {
            .lambda => expr_id,
            .nominal => |nominal| blk: {
                const backing_target = self.resolveCanonicalCallee(nominal.backing_expr, scope_mark) orelse break :blk null;
                break :blk switch (backing_target) {
                    .direct => |symbol| self.resolveCallableDefForGlobalSymbol(symbol),
                    .expr => |target_expr| if (self.resolveCallableDefExpr(target_expr, scope_mark) != null) expr_id else null,
                };
            },
            .block => |block| blk: {
                const block_mark = self.local_bindings.items.len;
                defer self.local_bindings.shrinkRetainingCapacity(block_mark);

                for (self.store.getStmts(block.stmts)) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| self.recordPatternBindings(binding.pattern, self.resolveCanonicalCallee(binding.expr, scope_mark)) catch return null,
                        .cell_init, .cell_store, .cell_drop => {},
                    }
                }

                break :blk self.resolveCallableDefExpr(block.final_expr, scope_mark);
            },
            .lookup => |lookup| blk: {
                if (self.resolveLocalBinding(lookup.symbol, scope_mark)) |local| {
                    break :blk switch (local) {
                        .shadowed => null,
                        .target => |target| switch (target) {
                            .direct => |symbol| self.resolveCallableDefForGlobalSymbol(symbol),
                            .expr => |target_expr| self.resolveCallableDefExpr(target_expr, scope_mark),
                        },
                    };
                }
                break :blk self.resolveCallableDefForGlobalSymbol(lookup.symbol);
            },
            .struct_access => |sa| blk: {
                const field_target = self.resolveCanonicalStructField(sa.struct_expr, sa.field_idx, scope_mark) orelse break :blk null;
                break :blk switch (field_target) {
                    .direct => |symbol| self.resolveCallableDefForGlobalSymbol(symbol),
                    .expr => |target_expr| self.resolveCallableDefExpr(target_expr, scope_mark),
                };
            },
            else => null,
        };
    }

    fn resolveDirectSymbolForGlobalSymbol(self: *Self, symbol: Symbol) ?Symbol {
        const key: u64 = @bitCast(symbol);
        if (self.direct_symbol_cache.get(key)) |entry| {
            return switch (entry) {
                .resolving => null,
                .resolved => |resolved| resolved,
            };
        }

        self.direct_symbol_cache.put(key, .resolving) catch return null;

        const scope_mark = self.local_bindings.items.len;
        const result = blk: {
            const def_expr_id = self.store.getSymbolDef(symbol) orelse break :blk null;
            if (self.resolveUnderlyingDirectSymbol(def_expr_id, scope_mark)) |underlying| break :blk underlying;
            if (self.resolveCallableDefForGlobalSymbol(symbol) != null) break :blk symbol;
            break :blk null;
        };

        self.direct_symbol_cache.put(key, .{ .resolved = result }) catch return null;
        return result;
    }

    fn resolveUnderlyingDirectSymbol(self: *Self, expr_id: LirExprId, scope_mark: usize) ?Symbol {
        if (expr_id.isNone()) return null;

        return switch (self.store.getExpr(expr_id)) {
            .nominal => |nominal| self.resolveUnderlyingDirectSymbol(nominal.backing_expr, scope_mark),
            .lookup => |lookup| blk: {
                if (self.resolveLocalBinding(lookup.symbol, scope_mark)) |local| {
                    break :blk switch (local) {
                        .shadowed => null,
                        .target => |target| switch (target) {
                            .direct => |symbol| symbol,
                            .expr => |target_expr| self.resolveUnderlyingDirectSymbol(target_expr, scope_mark),
                        },
                    };
                }
                break :blk self.resolveDirectSymbolForGlobalSymbol(lookup.symbol);
            },
            .block => |block| blk: {
                const block_mark = self.local_bindings.items.len;
                defer self.local_bindings.shrinkRetainingCapacity(block_mark);

                for (self.store.getStmts(block.stmts)) |stmt| {
                    switch (stmt) {
                        .decl, .mutate => |binding| self.recordPatternBindings(binding.pattern, self.resolveCanonicalCallee(binding.expr, scope_mark)) catch return null,
                        .cell_init, .cell_store, .cell_drop => {},
                    }
                }

                break :blk self.resolveUnderlyingDirectSymbol(block.final_expr, scope_mark);
            },
            .struct_access => |sa| blk: {
                const field_target = self.resolveCanonicalStructField(sa.struct_expr, sa.field_idx, scope_mark) orelse break :blk null;
                break :blk switch (field_target) {
                    .direct => |symbol| symbol,
                    .expr => |target_expr| self.resolveUnderlyingDirectSymbol(target_expr, scope_mark),
                };
            },
            else => null,
        };
    }

    fn recordPatternBindings(
        self: *Self,
        pattern_id: LirPatternId,
        target: ?CallTarget,
    ) Allocator.Error!void {
        const pattern = self.store.getPattern(pattern_id);
        switch (pattern) {
            .bind => |bind| try self.local_bindings.append(self.allocator, .{ .symbol = bind.symbol, .target = target }),
            .as_pattern => |as_pattern| {
                try self.local_bindings.append(self.allocator, .{ .symbol = as_pattern.symbol, .target = target });
                try self.recordPatternBindings(as_pattern.inner, null);
            },
            .tag => |tag_pat| for (self.store.getPatternSpan(tag_pat.args)) |arg_pat| try self.recordPatternBindings(arg_pat, null),
            .struct_ => |struct_pat| for (self.store.getPatternSpan(struct_pat.fields)) |field_pat| try self.recordPatternBindings(field_pat, null),
            .list => |list_pat| {
                for (self.store.getPatternSpan(list_pat.prefix)) |prefix_pat| try self.recordPatternBindings(prefix_pat, null);
                if (!list_pat.rest.isNone()) try self.recordPatternBindings(list_pat.rest, null);
                for (self.store.getPatternSpan(list_pat.suffix)) |suffix_pat| try self.recordPatternBindings(suffix_pat, null);
            },
            .wildcard,
            .int_literal,
            .float_literal,
            .str_literal,
            => {},
        }
    }
};

pub fn canonicalizeDirectCalls(
    allocator: Allocator,
    store: *LirExprStore,
    root_exprs: []const LirExprId,
) Allocator.Error!void {
    var pass = CallCanonicalizePass.init(allocator, store);
    defer pass.deinit();
    try pass.run(root_exprs);
}

test "canonicalizes local lambda alias call to lambda expr" {
    var store = LirExprStore.init(std.testing.allocator);
    defer store.deinit();

    const wildcard_pat = try store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, Region.zero());
    const lambda_params = try store.addPatternSpan(&.{wildcard_pat});
    const unit_expr = try store.addExpr(.{ .struct_ = .{ .struct_layout = .zst, .fields = LIR.LirExprSpan.empty() } }, Region.zero());
    const lambda_expr = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = lambda_params,
        .body = unit_expr,
        .ret_layout = .zst,
    } }, Region.zero());

    const tmp_symbol = Symbol.fromRaw(1);
    const tmp_pat = try store.addPattern(.{ .bind = .{ .symbol = tmp_symbol, .layout_idx = .zst } }, Region.zero());
    const tmp_lookup = try store.addExpr(.{ .lookup = .{ .symbol = tmp_symbol, .layout_idx = .zst } }, Region.zero());
    const call_expr = try store.addExpr(.{ .call = .{
        .callee = .{ .expr = tmp_lookup },
        .fn_layout = .zst,
        .args = LIR.LirExprSpan.empty(),
        .ret_layout = .zst,
        .called_via = .apply,
    } }, Region.zero());

    const block_stmts = try store.addStmts(&.{
        .{ .decl = .{ .pattern = tmp_pat, .expr = lambda_expr } },
    });
    const root_expr = try store.addExpr(.{ .block = .{
        .stmts = block_stmts,
        .final_expr = call_expr,
        .result_layout = .zst,
    } }, Region.zero());

    try canonicalizeDirectCalls(std.testing.allocator, &store, &.{root_expr});

    const canonical_call = store.getExpr(call_expr).call;
    try std.testing.expect(canonical_call.callee == .expr);
    try std.testing.expectEqual(lambda_expr, canonical_call.callee.expr);
}

test "canonicalizes record field call to direct symbol" {
    var store = LirExprStore.init(std.testing.allocator);
    defer store.deinit();

    const wildcard_pat = try store.addPattern(.{ .wildcard = .{ .layout_idx = .zst } }, Region.zero());
    const lambda_params = try store.addPatternSpan(&.{wildcard_pat});
    const unit_expr = try store.addExpr(.{ .struct_ = .{ .struct_layout = .zst, .fields = LIR.LirExprSpan.empty() } }, Region.zero());
    const lambda_expr = try store.addExpr(.{ .lambda = .{
        .fn_layout = .zst,
        .params = lambda_params,
        .body = unit_expr,
        .ret_layout = .zst,
    } }, Region.zero());

    const fn_symbol = Symbol.fromRaw(10);
    try store.registerSymbolDef(fn_symbol, lambda_expr);

    const fn_lookup = try store.addExpr(.{ .lookup = .{ .symbol = fn_symbol, .layout_idx = .zst } }, Region.zero());
    const record_expr = try store.addExpr(.{ .struct_ = .{
        .struct_layout = .zst,
        .fields = try store.addExprSpan(&.{fn_lookup}),
    } }, Region.zero());

    const record_symbol = Symbol.fromRaw(11);
    const record_pat = try store.addPattern(.{ .bind = .{ .symbol = record_symbol, .layout_idx = .zst } }, Region.zero());
    const record_lookup = try store.addExpr(.{ .lookup = .{ .symbol = record_symbol, .layout_idx = .zst } }, Region.zero());
    const field_expr = try store.addExpr(.{ .struct_access = .{
        .struct_expr = record_lookup,
        .struct_layout = .zst,
        .field_layout = .zst,
        .field_idx = 0,
    } }, Region.zero());
    const call_expr = try store.addExpr(.{ .call = .{
        .callee = .{ .expr = field_expr },
        .fn_layout = .zst,
        .args = LIR.LirExprSpan.empty(),
        .ret_layout = .zst,
        .called_via = .apply,
    } }, Region.zero());

    const block_stmts = try store.addStmts(&.{
        .{ .decl = .{ .pattern = record_pat, .expr = record_expr } },
    });
    const root_expr = try store.addExpr(.{ .block = .{
        .stmts = block_stmts,
        .final_expr = call_expr,
        .result_layout = .zst,
    } }, Region.zero());

    try canonicalizeDirectCalls(std.testing.allocator, &store, &.{root_expr});

    const canonical_call = store.getExpr(call_expr).call;
    try std.testing.expect(canonical_call.callee == .direct);
    try std.testing.expect(canonical_call.callee.direct.eql(fn_symbol));
    try std.testing.expectEqual(lambda_expr, store.getCallableDef(fn_symbol).?);
}
