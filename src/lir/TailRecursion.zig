//! Tail Recursion Detection and Transformation
//!
//! This module implements Roc-style tail recursion optimization by:
//! 1. Detecting calls in tail position
//! 2. Transforming tail-recursive calls into jumps to join points
//!
//! A call is in tail position if and only if:
//! - It's the value being returned: `let x = f(...) in ret x`
//! - It's in a branch of a conditional where the conditional is in tail position
//! - It's in a branch of a switch where the switch is in tail position
//!
//! NOT in tail position:
//! - `n * f(n - 1)` - the call result is used in a multiplication
//! - `let x = f(...) in x + 1` - the result is used in an addition
//! - `let x = f(...) in let y = x in ret y` - there's an intervening let
//!
//! The transformation creates:
//! - A Join point that marks the loop entry
//! - Jump instructions that replace tail calls
//!
//! Example transformation:
//! ```
//! # Before (recursive)
//! factorial = |n, acc|
//!     if n <= 1 then
//!         acc
//!     else
//!         factorial(n - 1, n * acc)
//!
//! # After (with join/jump)
//! factorial = |n, acc|
//!     join loop(n, acc) =
//!         if n <= 1 then
//!             ret acc
//!         else
//!             jump loop(n - 1, n * acc)
//!     in
//!     jump loop(n, acc)
//! ```

const std = @import("std");
const ir = @import("LIR.zig");
const store_mod = @import("LirExprStore.zig");

const LirExprStore = store_mod;
const LirExprId = ir.LirExprId;
const LirExprSpan = ir.LirExprSpan;
const LirPatternSpan = ir.LirPatternSpan;
const Symbol = ir.Symbol;
const JoinPointId = ir.JoinPointId;
const CFStmtId = ir.CFStmtId;
const CFSwitchBranch = ir.CFSwitchBranch;
const CFMatchBranch = ir.CFMatchBranch;
const LayoutIdxSpan = ir.LayoutIdxSpan;

const Region = @import("base").Region;
const Allocator = std.mem.Allocator;

/// Transforms tail-recursive calls into loops using join points
pub const TailRecursionPass = struct {
    store: *LirExprStore,
    target_symbol: Symbol,
    join_point_id: JoinPointId,
    found_tail_call: bool,
    allocator: Allocator,

    pub fn init(
        store: *LirExprStore,
        target_symbol: Symbol,
        join_point_id: JoinPointId,
        allocator: Allocator,
    ) TailRecursionPass {
        return .{
            .store = store,
            .target_symbol = target_symbol,
            .join_point_id = join_point_id,
            .found_tail_call = false,
            .allocator = allocator,
        };
    }

    /// Check if an expression is a call to the target function.
    /// Returns the arguments if it is, null otherwise.
    fn isTailCallToTarget(self: *TailRecursionPass, expr_id: LirExprId) ?LirExprSpan {
        const expr = self.store.getExpr(expr_id);
        switch (expr) {
            .call => |call| {
                const fn_expr = self.store.getExpr(call.fn_expr);
                switch (fn_expr) {
                    .lookup => |lookup| {
                        if (lookup.symbol.eql(self.target_symbol)) {
                            return call.args;
                        }
                    },
                    .closure => |closure_id| {
                        const closure = self.store.getClosureData(closure_id);
                        // Check if this closure is the recursive one
                        switch (closure.self_recursive) {
                            .self_recursive => |jp_id| {
                                if (jp_id == self.join_point_id) {
                                    return call.args;
                                }
                            },
                            .not_self_recursive => {},
                        }
                    },
                    else => {},
                }
            },
            else => {},
        }
        return null;
    }

    /// Transform a control flow statement, converting tail calls to jumps.
    /// Returns the transformed statement ID.
    pub fn transformStmt(self: *TailRecursionPass, stmt_id: CFStmtId) !CFStmtId {
        const stmt = self.store.getCFStmt(stmt_id);

        switch (stmt) {
            .let_stmt => |let_s| {
                // Check for pattern: let x = f(...) in ret x
                // This is the key pattern for tail calls
                const next_stmt = self.store.getCFStmt(let_s.next);
                if (next_stmt == .ret) {
                    const ret_expr = self.store.getExpr(next_stmt.ret.value);
                    if (ret_expr == .lookup) {
                        // Check if the pattern binds to the same symbol as the return
                        const pattern = self.store.getPattern(let_s.pattern);
                        if (pattern == .bind) {
                            if (ret_expr.lookup.symbol.eql(pattern.bind.symbol)) {
                                // This IS the tail call pattern!
                                if (self.isTailCallToTarget(let_s.value)) |args| {
                                    self.found_tail_call = true;
                                    // Replace with Jump
                                    return try self.store.addCFStmt(.{
                                        .jump = .{
                                            .target = self.join_point_id,
                                            .args = args,
                                        },
                                    });
                                }
                            }
                        }
                    }
                }

                // Not a tail call - recurse into next
                const new_next = try self.transformStmt(let_s.next);
                if (new_next != let_s.next) {
                    return try self.store.addCFStmt(.{
                        .let_stmt = .{
                            .pattern = let_s.pattern,
                            .value = let_s.value,
                            .next = new_next,
                        },
                    });
                }
                return stmt_id;
            },

            .switch_stmt => |sw| {
                // Transform each branch
                var changed = false;
                var new_branches = std.ArrayList(CFSwitchBranch).empty;
                defer new_branches.deinit(self.allocator);

                const branches = self.store.getCFSwitchBranches(sw.branches);
                for (branches) |branch| {
                    const new_body = try self.transformStmt(branch.body);
                    if (new_body != branch.body) changed = true;
                    try new_branches.append(self.allocator, .{ .value = branch.value, .body = new_body });
                }

                const new_default = try self.transformStmt(sw.default_branch);
                if (new_default != sw.default_branch) changed = true;

                if (changed) {
                    const branch_span = try self.store.addCFSwitchBranches(new_branches.items);
                    return try self.store.addCFStmt(.{
                        .switch_stmt = .{
                            .cond = sw.cond,
                            .cond_layout = sw.cond_layout,
                            .branches = branch_span,
                            .default_branch = new_default,
                            .ret_layout = sw.ret_layout,
                        },
                    });
                }
                return stmt_id;
            },

            .join => |j| {
                // Recurse into both body and remainder
                const new_body = try self.transformStmt(j.body);
                const new_remainder = try self.transformStmt(j.remainder);
                if (new_body != j.body or new_remainder != j.remainder) {
                    return try self.store.addCFStmt(.{
                        .join = .{
                            .id = j.id,
                            .params = j.params,
                            .param_layouts = j.param_layouts,
                            .body = new_body,
                            .remainder = new_remainder,
                        },
                    });
                }
                return stmt_id;
            },

            .ret => |r| {
                // Check for direct tail call: ret f(...)
                if (self.isTailCallToTarget(r.value)) |args| {
                    self.found_tail_call = true;
                    return try self.store.addCFStmt(.{
                        .jump = .{
                            .target = self.join_point_id,
                            .args = args,
                        },
                    });
                }
                // Terminal statement - no transformation
                return stmt_id;
            },

            .jump => {
                // Already a jump - no transformation
                return stmt_id;
            },

            .expr_stmt => |e| {
                const new_next = try self.transformStmt(e.next);
                if (new_next != e.next) {
                    return try self.store.addCFStmt(.{
                        .expr_stmt = .{ .value = e.value, .next = new_next },
                    });
                }
                return stmt_id;
            },

            .match_stmt => |ms| {
                // Transform each branch body
                var changed = false;
                var new_branches = std.ArrayList(CFMatchBranch).empty;
                defer new_branches.deinit(self.allocator);

                const branches = self.store.getCFMatchBranches(ms.branches);
                for (branches) |branch| {
                    const new_body = try self.transformStmt(branch.body);
                    if (new_body != branch.body) changed = true;
                    try new_branches.append(self.allocator, .{
                        .pattern = branch.pattern,
                        .guard = branch.guard,
                        .body = new_body,
                    });
                }

                if (changed) {
                    const branch_span = try self.store.addCFMatchBranches(new_branches.items);
                    return try self.store.addCFStmt(.{
                        .match_stmt = .{
                            .value = ms.value,
                            .value_layout = ms.value_layout,
                            .branches = branch_span,
                            .ret_layout = ms.ret_layout,
                        },
                    });
                }
                return stmt_id;
            },
        }
    }
};

/// Apply tail recursion optimization to a procedure body.
/// Returns the transformed body wrapped in a Join, or null if no tail calls found.
///
/// The transformation:
/// 1. Create a Join point with the function's parameters
/// 2. Transform all tail-recursive calls into Jumps to the Join
/// 3. Wrap the body: `join id(params) = transformed_body in jump id(initial_args)`
pub fn makeTailRecursive(
    store: *LirExprStore,
    proc_symbol: Symbol,
    join_point_id: JoinPointId,
    body: CFStmtId,
    params: LirPatternSpan,
    param_layouts: LayoutIdxSpan,
    allocator: Allocator,
) !?CFStmtId {
    var pass = TailRecursionPass.init(store, proc_symbol, join_point_id, allocator);

    const transformed_body = try pass.transformStmt(body);

    if (!pass.found_tail_call) {
        return null; // No tail calls found - don't transform
    }

    // Wrap in Join: join id(params) = transformed_body in jump id(initial_args)

    // Create initial jump with original parameter symbols as arguments
    const param_patterns = store.getPatternSpan(params);
    var initial_args = std.ArrayList(LirExprId).empty;
    defer initial_args.deinit(allocator);

    for (param_patterns) |pattern_id| {
        const pattern = store.getPattern(pattern_id);
        switch (pattern) {
            .bind => |bind| {
                // Create lookup expression for this parameter
                const lookup_id = try store.addExpr(.{
                    .lookup = .{
                        .symbol = bind.symbol,
                        .layout_idx = bind.layout_idx,
                    },
                }, Region.zero());
                try initial_args.append(allocator, lookup_id);
            },
            .wildcard => {
                // Wildcard parameter: pass a zero placeholder to maintain arity.
                // rebindJoinPointParams skips non-bind patterns, so this value
                // is never stored or used at runtime.
                const placeholder_id = try store.addExpr(.{ .i64_literal = 0 }, Region.zero());
                try initial_args.append(allocator, placeholder_id);
            },
            else => unreachable, // Only bind and wildcard should appear as function params after desugaring
        }
    }

    const args_span = try store.addExprSpan(initial_args.items);

    const initial_jump = try store.addCFStmt(.{
        .jump = .{
            .target = join_point_id,
            .args = args_span,
        },
    });

    const join_stmt = try store.addCFStmt(.{
        .join = .{
            .id = join_point_id,
            .params = params,
            .param_layouts = param_layouts,
            .body = transformed_body,
            .remainder = initial_jump,
        },
    });

    return join_stmt;
}

test "TailRecursionPass initialization" {
    const allocator = std.testing.allocator;
    var store = LirExprStore.init(allocator);
    defer store.deinit();

    const ident = @import("base").Ident.Idx{
        .attributes = .{ .effectful = false, .ignored = false, .reassignable = false },
        .idx = 42,
    };
    const symbol = Symbol{ .module_idx = 0, .ident_idx = ident };
    const join_id: JoinPointId = @enumFromInt(1);

    var pass = TailRecursionPass.init(&store, symbol, join_id, allocator);

    try std.testing.expect(!pass.found_tail_call);
    try std.testing.expect(pass.target_symbol.eql(symbol));
}

test "TailRecursionPass: tail call is transformed to jump" {
    // Build: let result = f(arg) in ret result
    // where f is the target symbol => should become a jump
    const allocator = std.testing.allocator;
    const base = @import("base");

    var store = LirExprStore.init(allocator);
    defer store.deinit();

    const makeIdent = struct {
        fn f(idx: u29) base.Ident.Idx {
            return .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = idx };
        }
    }.f;

    const target_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(1) };
    const result_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(2) };
    const arg_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(3) };
    const join_id: JoinPointId = @enumFromInt(1);
    const i64_layout = @import("layout").Idx.i64;

    // Build: f(arg)
    const fn_lookup = try store.addExpr(.{ .lookup = .{ .symbol = target_sym, .layout_idx = i64_layout } }, Region.zero());
    const arg_lookup = try store.addExpr(.{ .lookup = .{ .symbol = arg_sym, .layout_idx = i64_layout } }, Region.zero());
    const call_args = try store.addExprSpan(&.{arg_lookup});
    const call_expr = try store.addExpr(.{ .call = .{
        .fn_expr = fn_lookup,
        .args = call_args,
        .fn_layout = i64_layout,
        .ret_layout = i64_layout,
        .called_via = .apply,
    } }, Region.zero());

    // Build: ret result
    const ret_lookup = try store.addExpr(.{ .lookup = .{ .symbol = result_sym, .layout_idx = i64_layout } }, Region.zero());
    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = ret_lookup } });

    // Build: let result = f(arg) in ret result
    const bind_pat = try store.addPattern(.{ .bind = .{ .symbol = result_sym, .layout_idx = i64_layout } }, Region.zero());
    const let_stmt = try store.addCFStmt(.{ .let_stmt = .{
        .pattern = bind_pat,
        .value = call_expr,
        .next = ret_stmt,
    } });

    var pass = TailRecursionPass.init(&store, target_sym, join_id, allocator);
    const transformed = try pass.transformStmt(let_stmt);

    // Should detect the tail call
    try std.testing.expect(pass.found_tail_call);

    // The result should be a jump to the join point
    const result_stmt = store.getCFStmt(transformed);
    try std.testing.expect(result_stmt == .jump);
    try std.testing.expectEqual(join_id, result_stmt.jump.target);

    // Jump args should match the original call args
    const jump_args = store.getExprSpan(result_stmt.jump.args);
    try std.testing.expectEqual(@as(usize, 1), jump_args.len);
}

test "TailRecursionPass: non-tail call is not transformed" {
    // Build: let result = f(arg) in ret other_symbol
    // result != other_symbol, so this is NOT a tail call
    const allocator = std.testing.allocator;
    const base = @import("base");

    var store = LirExprStore.init(allocator);
    defer store.deinit();

    const makeIdent = struct {
        fn f(idx: u29) base.Ident.Idx {
            return .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = idx };
        }
    }.f;

    const target_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(1) };
    const result_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(2) };
    const other_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(3) };
    const arg_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(4) };
    const join_id: JoinPointId = @enumFromInt(1);
    const i64_layout = @import("layout").Idx.i64;

    // Build: f(arg)
    const fn_lookup = try store.addExpr(.{ .lookup = .{ .symbol = target_sym, .layout_idx = i64_layout } }, Region.zero());
    const arg_lookup = try store.addExpr(.{ .lookup = .{ .symbol = arg_sym, .layout_idx = i64_layout } }, Region.zero());
    const call_args = try store.addExprSpan(&.{arg_lookup});
    const call_expr = try store.addExpr(.{ .call = .{
        .fn_expr = fn_lookup,
        .args = call_args,
        .fn_layout = i64_layout,
        .ret_layout = i64_layout,
        .called_via = .apply,
    } }, Region.zero());

    // Build: ret other_symbol (different from result)
    const ret_lookup = try store.addExpr(.{ .lookup = .{ .symbol = other_sym, .layout_idx = i64_layout } }, Region.zero());
    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = ret_lookup } });

    // Build: let result = f(arg) in ret other_symbol
    const bind_pat = try store.addPattern(.{ .bind = .{ .symbol = result_sym, .layout_idx = i64_layout } }, Region.zero());
    const let_stmt = try store.addCFStmt(.{ .let_stmt = .{
        .pattern = bind_pat,
        .value = call_expr,
        .next = ret_stmt,
    } });

    var pass = TailRecursionPass.init(&store, target_sym, join_id, allocator);
    const transformed = try pass.transformStmt(let_stmt);

    // Should NOT detect a tail call
    try std.testing.expect(!pass.found_tail_call);

    // The statement should be unchanged
    try std.testing.expectEqual(let_stmt, transformed);
}

test "makeTailRecursive: end-to-end transforms tail-recursive body" {
    // Build a function body: let result = f(arg) in ret result
    // makeTailRecursive should wrap it in join/jump
    const allocator = std.testing.allocator;
    const base = @import("base");

    const layout = @import("layout");

    var store = LirExprStore.init(allocator);
    defer store.deinit();

    const makeIdent = struct {
        fn f(idx: u29) base.Ident.Idx {
            return .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = idx };
        }
    }.f;

    const target_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(1) };
    const result_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(2) };
    const param_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(3) };
    const join_id: JoinPointId = @enumFromInt(1);
    const i64_layout = layout.Idx.i64;

    // Build function parameter
    const param_pat = try store.addPattern(.{ .bind = .{ .symbol = param_sym, .layout_idx = i64_layout } }, Region.zero());
    const params = try store.addPatternSpan(&.{param_pat});
    const param_layouts = try store.addLayoutIdxSpan(&.{i64_layout});

    // Build: f(param)
    const fn_lookup = try store.addExpr(.{ .lookup = .{ .symbol = target_sym, .layout_idx = i64_layout } }, Region.zero());
    const arg_lookup = try store.addExpr(.{ .lookup = .{ .symbol = param_sym, .layout_idx = i64_layout } }, Region.zero());
    const call_args = try store.addExprSpan(&.{arg_lookup});
    const call_expr = try store.addExpr(.{ .call = .{
        .fn_expr = fn_lookup,
        .args = call_args,
        .fn_layout = i64_layout,
        .ret_layout = i64_layout,
        .called_via = .apply,
    } }, Region.zero());

    // Build: ret result
    const ret_lookup = try store.addExpr(.{ .lookup = .{ .symbol = result_sym, .layout_idx = i64_layout } }, Region.zero());
    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = ret_lookup } });

    // Build: let result = f(param) in ret result
    const bind_pat = try store.addPattern(.{ .bind = .{ .symbol = result_sym, .layout_idx = i64_layout } }, Region.zero());
    const body = try store.addCFStmt(.{ .let_stmt = .{
        .pattern = bind_pat,
        .value = call_expr,
        .next = ret_stmt,
    } });

    // Run makeTailRecursive
    const result = try makeTailRecursive(&store, target_sym, join_id, body, params, param_layouts, allocator);

    // Should return non-null (tail call was found)
    try std.testing.expect(result != null);

    // The result should be a join statement
    const join_stmt = store.getCFStmt(result.?);
    try std.testing.expect(join_stmt == .join);
    try std.testing.expectEqual(join_id, join_stmt.join.id);

    // The join's body should contain the transformed statement (a jump)
    const transformed_body = store.getCFStmt(join_stmt.join.body);
    try std.testing.expect(transformed_body == .jump);
    try std.testing.expectEqual(join_id, transformed_body.jump.target);

    // The join's remainder should be an initial jump with the original param
    const remainder = store.getCFStmt(join_stmt.join.remainder);
    try std.testing.expect(remainder == .jump);
    try std.testing.expectEqual(join_id, remainder.jump.target);
    const initial_args = store.getExprSpan(remainder.jump.args);
    try std.testing.expectEqual(@as(usize, 1), initial_args.len);
}

test "TailRecursionPass: tail call inside switch_stmt branch is transformed" {
    // Build: switch cond { 0 -> (let result = f(arg) in ret result), default -> ret 42 }
    // Branch 0 contains a tail call to f. After transformation, it should become a jump.
    const allocator = std.testing.allocator;
    const base = @import("base");

    var store = LirExprStore.init(allocator);
    defer store.deinit();

    const makeIdent = struct {
        fn f(idx: u29) base.Ident.Idx {
            return .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = idx };
        }
    }.f;

    const target_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(1) };
    const result_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(2) };
    const arg_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(3) };
    const cond_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(4) };
    const join_id: JoinPointId = @enumFromInt(1);
    const i64_layout = @import("layout").Idx.i64;

    // Build branch 0 body: let result = f(arg) in ret result
    const fn_lookup = try store.addExpr(.{ .lookup = .{ .symbol = target_sym, .layout_idx = i64_layout } }, Region.zero());
    const arg_lookup = try store.addExpr(.{ .lookup = .{ .symbol = arg_sym, .layout_idx = i64_layout } }, Region.zero());
    const call_args = try store.addExprSpan(&.{arg_lookup});
    const call_expr = try store.addExpr(.{ .call = .{
        .fn_expr = fn_lookup,
        .args = call_args,
        .fn_layout = i64_layout,
        .ret_layout = i64_layout,
        .called_via = .apply,
    } }, Region.zero());

    const ret_lookup = try store.addExpr(.{ .lookup = .{ .symbol = result_sym, .layout_idx = i64_layout } }, Region.zero());
    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = ret_lookup } });

    const bind_pat = try store.addPattern(.{ .bind = .{ .symbol = result_sym, .layout_idx = i64_layout } }, Region.zero());
    const branch0_body = try store.addCFStmt(.{ .let_stmt = .{
        .pattern = bind_pat,
        .value = call_expr,
        .next = ret_stmt,
    } });

    // Build default branch body: ret 42
    const lit_42 = try store.addExpr(.{ .i64_literal = 42 }, Region.zero());
    const default_body = try store.addCFStmt(.{ .ret = .{ .value = lit_42 } });

    // Build switch statement
    const cond_expr = try store.addExpr(.{ .lookup = .{ .symbol = cond_sym, .layout_idx = i64_layout } }, Region.zero());
    const branch_bodies = [_]CFSwitchBranch{
        .{ .value = 0, .body = branch0_body },
    };
    const branch_span = try store.addCFSwitchBranches(&branch_bodies);
    const switch_stmt = try store.addCFStmt(.{ .switch_stmt = .{
        .cond = cond_expr,
        .cond_layout = i64_layout,
        .branches = branch_span,
        .default_branch = default_body,
        .ret_layout = i64_layout,
    } });

    var pass = TailRecursionPass.init(&store, target_sym, join_id, allocator);
    const transformed = try pass.transformStmt(switch_stmt);

    // Should detect the tail call in branch 0
    try std.testing.expect(pass.found_tail_call);

    // The result should still be a switch_stmt (the switch itself is not replaced)
    const result_stmt = store.getCFStmt(transformed);
    try std.testing.expect(result_stmt == .switch_stmt);

    // Branch 0 should now be a jump (tail call was transformed)
    const result_branches = store.getCFSwitchBranches(result_stmt.switch_stmt.branches);
    try std.testing.expectEqual(@as(usize, 1), result_branches.len);
    const branch0_result = store.getCFStmt(result_branches[0].body);
    try std.testing.expect(branch0_result == .jump);
    try std.testing.expectEqual(join_id, branch0_result.jump.target);

    // Default branch should be unchanged (ret 42, not a tail call)
    const default_result = store.getCFStmt(result_stmt.switch_stmt.default_branch);
    try std.testing.expect(default_result == .ret);
}

test "TailRecursionPass: direct ret f(...) is transformed to jump" {
    // Build: ret f(arg) — the ret's value IS a call to the target function.
    // The transformStmt .ret case handles this directly.
    const allocator = std.testing.allocator;
    const base = @import("base");

    var store = LirExprStore.init(allocator);
    defer store.deinit();

    const makeIdent = struct {
        fn f(idx: u29) base.Ident.Idx {
            return .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = idx };
        }
    }.f;

    const target_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(1) };
    const arg_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(2) };
    const join_id: JoinPointId = @enumFromInt(1);
    const i64_layout = @import("layout").Idx.i64;

    // Build: f(arg)
    const fn_lookup = try store.addExpr(.{ .lookup = .{ .symbol = target_sym, .layout_idx = i64_layout } }, Region.zero());
    const arg_lookup = try store.addExpr(.{ .lookup = .{ .symbol = arg_sym, .layout_idx = i64_layout } }, Region.zero());
    const call_args = try store.addExprSpan(&.{arg_lookup});
    const call_expr = try store.addExpr(.{ .call = .{
        .fn_expr = fn_lookup,
        .args = call_args,
        .fn_layout = i64_layout,
        .ret_layout = i64_layout,
        .called_via = .apply,
    } }, Region.zero());

    // Build: ret f(arg)
    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = call_expr } });

    var pass = TailRecursionPass.init(&store, target_sym, join_id, allocator);
    const transformed = try pass.transformStmt(ret_stmt);

    // Should detect the tail call
    try std.testing.expect(pass.found_tail_call);

    // The result should be a jump to the join point
    const result_stmt = store.getCFStmt(transformed);
    try std.testing.expect(result_stmt == .jump);
    try std.testing.expectEqual(join_id, result_stmt.jump.target);

    // Jump args should match the original call args (1 arg)
    const jump_args = store.getExprSpan(result_stmt.jump.args);
    try std.testing.expectEqual(@as(usize, 1), jump_args.len);
}

test "TailRecursionPass: call to non-target function is not detected as tail call" {
    // Build: let result = g(arg) in ret result
    // where g is a DIFFERENT function from target_sym => should NOT be a tail call
    const allocator = std.testing.allocator;
    const base = @import("base");

    var store = LirExprStore.init(allocator);
    defer store.deinit();

    const makeIdent = struct {
        fn f(idx: u29) base.Ident.Idx {
            return .{ .attributes = .{ .effectful = false, .ignored = false, .reassignable = false }, .idx = idx };
        }
    }.f;

    const target_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(1) };
    const other_fn_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(2) };
    const result_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(3) };
    const arg_sym = Symbol{ .module_idx = 0, .ident_idx = makeIdent(4) };
    const join_id: JoinPointId = @enumFromInt(1);
    const i64_layout = @import("layout").Idx.i64;

    // Build: g(arg) — calling other_fn_sym, NOT target_sym
    const fn_lookup = try store.addExpr(.{ .lookup = .{ .symbol = other_fn_sym, .layout_idx = i64_layout } }, Region.zero());
    const arg_lookup = try store.addExpr(.{ .lookup = .{ .symbol = arg_sym, .layout_idx = i64_layout } }, Region.zero());
    const call_args = try store.addExprSpan(&.{arg_lookup});
    const call_expr = try store.addExpr(.{ .call = .{
        .fn_expr = fn_lookup,
        .args = call_args,
        .fn_layout = i64_layout,
        .ret_layout = i64_layout,
        .called_via = .apply,
    } }, Region.zero());

    // Build: ret result
    const ret_lookup = try store.addExpr(.{ .lookup = .{ .symbol = result_sym, .layout_idx = i64_layout } }, Region.zero());
    const ret_stmt = try store.addCFStmt(.{ .ret = .{ .value = ret_lookup } });

    // Build: let result = g(arg) in ret result
    const bind_pat = try store.addPattern(.{ .bind = .{ .symbol = result_sym, .layout_idx = i64_layout } }, Region.zero());
    const let_stmt = try store.addCFStmt(.{ .let_stmt = .{
        .pattern = bind_pat,
        .value = call_expr,
        .next = ret_stmt,
    } });

    var pass = TailRecursionPass.init(&store, target_sym, join_id, allocator);
    const transformed = try pass.transformStmt(let_stmt);

    // Should NOT detect a tail call (g != target_sym)
    try std.testing.expect(!pass.found_tail_call);

    // The statement should be unchanged
    try std.testing.expectEqual(let_stmt, transformed);
}
