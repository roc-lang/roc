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
const ir = @import("MonoIR.zig");
const store_mod = @import("MonoExprStore.zig");

const MonoExprStore = store_mod;
const MonoExprId = ir.MonoExprId;
const MonoExprSpan = ir.MonoExprSpan;
const MonoPatternSpan = ir.MonoPatternSpan;
const Symbol = ir.Symbol;
const JoinPointId = ir.JoinPointId;
const CFStmtId = ir.CFStmtId;
const CFSwitchBranch = ir.CFSwitchBranch;
const CFMatchBranch = ir.CFMatchBranch;
const LayoutIdxSpan = ir.LayoutIdxSpan;

const Allocator = std.mem.Allocator;

/// Transforms tail-recursive calls into loops using join points
pub const TailRecursionPass = struct {
    store: *MonoExprStore,
    target_symbol: Symbol,
    join_point_id: JoinPointId,
    found_tail_call: bool,
    allocator: Allocator,

    pub fn init(
        store: *MonoExprStore,
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
    fn isTailCallToTarget(self: *TailRecursionPass, expr_id: MonoExprId) ?MonoExprSpan {
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
                    .closure => |closure| {
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
                if (@intFromEnum(new_next) != @intFromEnum(let_s.next)) {
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
                    if (@intFromEnum(new_body) != @intFromEnum(branch.body)) changed = true;
                    try new_branches.append(self.allocator, .{ .value = branch.value, .body = new_body });
                }

                const new_default = try self.transformStmt(sw.default_branch);
                if (@intFromEnum(new_default) != @intFromEnum(sw.default_branch)) changed = true;

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
                if (@intFromEnum(new_body) != @intFromEnum(j.body) or @intFromEnum(new_remainder) != @intFromEnum(j.remainder)) {
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
                if (@intFromEnum(new_next) != @intFromEnum(e.next)) {
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
                    if (@intFromEnum(new_body) != @intFromEnum(branch.body)) changed = true;
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
    store: *MonoExprStore,
    proc_symbol: Symbol,
    join_point_id: JoinPointId,
    body: CFStmtId,
    params: MonoPatternSpan,
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
    var initial_args = std.ArrayList(MonoExprId).empty;
    defer initial_args.deinit(allocator);

    const proc_param_layouts = store.getLayoutIdxSpan(param_layouts);
    for (param_patterns, 0..) |pattern_id, param_idx| {
        const pattern = store.getPattern(pattern_id);
        switch (pattern) {
            .bind => |bind| {
                // Create lookup expression for this parameter
                const lookup_id = try store.addExpr(.{
                    .lookup = .{
                        .symbol = bind.symbol,
                        .layout_idx = bind.layout_idx,
                    },
                }, @import("base").Region.zero());
                try initial_args.append(allocator, lookup_id);
            },
            .wildcard => {
                // Wildcard params need a placeholder to maintain arity.
                // Use layout-aware zero to avoid type mismatch.
                const param_layout = proc_param_layouts[param_idx];
                const placeholder_id = if (param_layout == .i128 or param_layout == .u128 or param_layout == .dec)
                    try store.addExpr(.{ .i128_literal = 0 }, @import("base").Region.zero())
                else
                    try store.addExpr(.{ .i64_literal = 0 }, @import("base").Region.zero());
                try initial_args.append(allocator, placeholder_id);
            },
            else => unreachable,
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
    var store = MonoExprStore.init(allocator);
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
