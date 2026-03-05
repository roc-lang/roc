//! Lambda Set Inference for MIR
//!
//! Determines which lifted functions can flow to each function-typed symbol.
//! After CIR→MIR lowering, closures have been lifted to top-level functions
//! and their use sites produce captures tuples. This pass traces which lifted
//! function(s) each symbol could refer to.
//!
//! Results are stored in a LambdaSetStore, consumed by MirToLir for dispatch.

const std = @import("std");
const MIR = @import("MIR.zig");
const Monotype = @import("Monotype.zig");

const Allocator = std.mem.Allocator;

/// Index into the lambda_sets array.
pub const Idx = enum(u32) {
    _,

    pub const none: Idx = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: Idx) bool {
        return self == none;
    }
};

/// A member of a lambda set: one possible function that could be called.
pub const Member = struct {
    /// Symbol of the lifted function
    fn_symbol: MIR.Symbol,
    /// Monotype of the captures struct (tuple), or Monotype.Idx.none if no captures
    captures_monotype: Monotype.Idx,
};

/// Span of Member values in the members array.
pub const MemberSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() MemberSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: MemberSpan) bool {
        return self.len == 0;
    }
};

/// A lambda set: the set of possible functions at a given call site.
pub const LambdaSet = struct {
    members: MemberSpan,
};

/// Storage for all lambda sets and the symbol→lambda_set mapping.
pub const Store = struct {
    /// All lambda sets
    lambda_sets: std.ArrayListUnmanaged(LambdaSet),
    /// All members (referenced by MemberSpan)
    members: std.ArrayListUnmanaged(Member),
    /// Map from MIR.Symbol (as u64) to its LambdaSet.Idx
    symbol_lambda_sets: std.AutoHashMapUnmanaged(u64, Idx),

    pub fn init() Store {
        return .{
            .lambda_sets = .empty,
            .members = .empty,
            .symbol_lambda_sets = .empty,
        };
    }

    pub fn deinit(self: *Store, allocator: Allocator) void {
        self.lambda_sets.deinit(allocator);
        self.members.deinit(allocator);
        self.symbol_lambda_sets.deinit(allocator);
    }

    pub fn addMembers(self: *Store, allocator: Allocator, member_list: []const Member) !MemberSpan {
        if (member_list.len == 0) return MemberSpan.empty();
        const start: u32 = @intCast(self.members.items.len);
        try self.members.appendSlice(allocator, member_list);
        return .{ .start = start, .len = @intCast(member_list.len) };
    }

    pub fn getMembers(self: *const Store, span: MemberSpan) []const Member {
        if (span.len == 0) return &.{};
        return self.members.items[span.start..][0..span.len];
    }

    pub fn addLambdaSet(self: *Store, allocator: Allocator, ls: LambdaSet) !Idx {
        const idx: u32 = @intCast(self.lambda_sets.items.len);
        try self.lambda_sets.append(allocator, ls);
        return @enumFromInt(idx);
    }

    pub fn getLambdaSet(self: *const Store, idx: Idx) LambdaSet {
        return self.lambda_sets.items[@intFromEnum(idx)];
    }

    /// Look up the lambda set for a symbol, if any.
    pub fn getSymbolLambdaSet(self: *const Store, symbol: MIR.Symbol) ?Idx {
        return self.symbol_lambda_sets.get(symbol.raw());
    }
};

/// Check if an expression is directly a `.lambda`, following through
/// `.block` wrappers only. Does NOT follow `.lookup` chains — aliases
/// get their lambda sets through capture alias propagation in `infer()`.
pub fn isLambdaExpr(mir_store: *const MIR.Store, expr_id: MIR.ExprId) bool {
    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .block => |block| isLambdaExpr(mir_store, block.final_expr),
        .lambda => true,
        else => false,
    };
}

/// Run lambda set inference on a completed MIR store.
/// Traces closure origins through symbol definitions to determine
/// which lifted function(s) each function-typed symbol could refer to.
pub fn infer(allocator: Allocator, mir_store: *const MIR.Store) Allocator.Error!Store {
    var store = Store.init();
    errdefer store.deinit(allocator);

    // For each symbol definition, check if it originates from a closure.
    // If so, assign a singleton lambda set.
    var sym_it = mir_store.symbol_defs.iterator();
    while (sym_it.next()) |entry| {
        const symbol_key = entry.key_ptr.*;
        const expr_id = entry.value_ptr.*;
        const symbol = MIR.Symbol.fromRaw(symbol_key);

        const ls_idx = try resolveExprLambdaSet(allocator, mir_store, &store, expr_id);
        if (!ls_idx.isNone()) {
            try store.symbol_lambda_sets.put(allocator, symbol.raw(), ls_idx);
        } else if (isLambdaExpr(mir_store, expr_id)) {
            // Plain lambda (no captures) — create a singleton lambda set so that
            // call dispatch goes through lowerClosureCall uniformly.
            const member_span = try store.addMembers(allocator, &.{.{
                .fn_symbol = symbol,
                .captures_monotype = Monotype.Idx.none,
            }});
            const plain_ls = try store.addLambdaSet(allocator, .{ .members = member_span });
            try store.symbol_lambda_sets.put(allocator, symbol.raw(), plain_ls);
        }
    }

    // Propagate lambda sets from call-site arguments to callee parameters.
    // When `f(g)` is called and `g` has a lambda set, propagate it to f's
    // corresponding parameter so that calls to that parameter inside f can
    // dispatch through the lambda set.
    try propagateCallArgs(allocator, mir_store, &store);

    // Propagate lambda sets through capture aliases. A capture-local inside
    // a lifted function body aliases an outer-scope symbol; it should have
    // the same lambda set so closure call dispatch works.
    var alias_it = mir_store.capture_symbol_aliases.iterator();
    while (alias_it.next()) |entry| {
        const capture_local_key = entry.key_ptr.*;
        const canonical_key = entry.value_ptr.*;
        if (store.symbol_lambda_sets.get(canonical_key)) |ls_idx| {
            try store.symbol_lambda_sets.put(allocator, capture_local_key, ls_idx);
        }
    }

    return store;
}

/// Propagate lambda sets from call arguments to callee parameters.
/// Uses pre-recorded HofCallArg entries from lowering — no expression scanning needed.
fn propagateCallArgs(allocator: Allocator, mir_store: *const MIR.Store, ls_store: *Store) Allocator.Error!void {
    // Iterate to a fixpoint so newly learned lambda sets can unlock additional
    // propagation through other HOF edges in the same MIR graph.
    var changed = true;
    while (changed) {
        changed = false;

        for (mir_store.hof_call_args.items) |hof| {
            // Resolve the arg's lambda set — works for both lookups (hoisted lambdas)
            // and direct closure expressions (captures tuples with closure_origins).
            const arg_ls = getArgLambdaSet(allocator, mir_store, ls_store, hof.arg_expr) orelse continue;
            const arg_index: usize = @intCast(hof.arg_index);

            // Fast path: callee resolves directly to a lambda/lookup-with-def.
            if (resolveToLambdaParams(mir_store, hof.call_func)) |params| {
                changed = (try propagateArgIntoParams(allocator, mir_store, ls_store, params, arg_index, arg_ls)) or changed;
                continue;
            }

            // Fallback: callee may be a lookup to a symbol that has a lambda set
            // but no direct symbol_def (e.g. higher-order parameter). In that case,
            // propagate into each member function's corresponding parameter.
            const call_expr = mir_store.getExpr(hof.call_func);
            if (call_expr == .lookup) {
                changed = (try propagateArgIntoCalleeMembers(
                    allocator,
                    mir_store,
                    ls_store,
                    call_expr.lookup,
                    arg_index,
                    arg_ls,
                )) or changed;
            }
        }
    }
}

fn propagateArgIntoParams(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    ls_store: *Store,
    params: MIR.PatternSpan,
    arg_index: usize,
    arg_ls: Idx,
) Allocator.Error!bool {
    const param_ids = mir_store.getPatternSpan(params);
    if (arg_index >= param_ids.len) return false;

    const param_pat = mir_store.getPattern(param_ids[arg_index]);
    if (param_pat != .bind) return false;

    return mergeIntoSymbol(allocator, ls_store, param_pat.bind, arg_ls);
}

fn propagateArgIntoCalleeMembers(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    ls_store: *Store,
    callee_symbol: MIR.Symbol,
    arg_index: usize,
    arg_ls: Idx,
) Allocator.Error!bool {
    const callee_ls_idx = ls_store.getSymbolLambdaSet(callee_symbol) orelse return false;
    const callee_ls = ls_store.getLambdaSet(callee_ls_idx);
    const members = ls_store.getMembers(callee_ls.members);

    var changed = false;
    for (members) |member| {
        const def_expr_id = mir_store.getSymbolDef(member.fn_symbol) orelse continue;
        const params = resolveToLambdaParams(mir_store, def_expr_id) orelse continue;
        const did_change = try propagateArgIntoParams(allocator, mir_store, ls_store, params, arg_index, arg_ls);
        changed = did_change or changed;
    }
    return changed;
}

/// Get the lambda set for a call argument expression.
/// Handles lookups (check symbol_lambda_sets) and direct expressions (resolveExprLambdaSet).
fn getArgLambdaSet(allocator: Allocator, mir_store: *const MIR.Store, ls_store: *Store, arg_expr_id: MIR.ExprId) ?Idx {
    const arg_expr = mir_store.getExpr(arg_expr_id);
    if (arg_expr == .lookup) {
        return ls_store.symbol_lambda_sets.get(arg_expr.lookup.raw());
    }
    const ls = resolveExprLambdaSet(allocator, mir_store, ls_store, arg_expr_id) catch return null;
    return if (ls.isNone()) null else ls;
}

/// Follow lookups, blocks, and closure origins to find the callee lambda's parameter list.
fn resolveToLambdaParams(mir_store: *const MIR.Store, expr_id: MIR.ExprId) ?MIR.PatternSpan {
    // Check closure origins first — captures tuples map to their lifted lambdas
    if (mir_store.closure_origins.get(@intFromEnum(expr_id))) |lifted_idx| {
        const lifted = mir_store.lifted_lambdas.items[lifted_idx];
        const lifted_def = mir_store.getSymbolDef(lifted.fn_symbol) orelse return null;
        return resolveToLambdaParams(mir_store, lifted_def);
    }

    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .lambda => |l| l.params,
        .block => |block| resolveToLambdaParams(mir_store, block.final_expr),
        .lookup => |sym| {
            const def_expr_id = mir_store.getSymbolDef(sym) orelse return null;
            return resolveToLambdaParams(mir_store, def_expr_id);
        },
        else => null,
    };
}

/// Assign or merge a lambda set into a symbol's existing lambda set.
/// Returns true if the symbol's lambda set changed (new assignment or new members added).
fn mergeIntoSymbol(allocator: Allocator, ls_store: *Store, sym: MIR.Symbol, new_ls_idx: Idx) Allocator.Error!bool {
    const existing = ls_store.symbol_lambda_sets.get(sym.raw());
    if (existing == null) {
        // No existing lambda set — assign directly
        try ls_store.symbol_lambda_sets.put(allocator, sym.raw(), new_ls_idx);
        return true;
    }

    // Merge: union the member lists, dedup by fn_symbol
    const existing_ls = ls_store.getLambdaSet(existing.?);
    const new_ls = ls_store.getLambdaSet(new_ls_idx);
    const existing_members = ls_store.getMembers(existing_ls.members);
    const new_members = ls_store.getMembers(new_ls.members);

    var merged: std.ArrayListUnmanaged(Member) = .empty;
    defer merged.deinit(allocator);
    try merged.appendSlice(allocator, existing_members);

    var changed = false;
    for (new_members) |m| {
        var found = false;
        for (existing_members) |existing_m| {
            if (existing_m.fn_symbol.eql(m.fn_symbol)) {
                found = true;
                break;
            }
        }
        if (!found) {
            try merged.append(allocator, m);
            changed = true;
        }
    }

    if (changed) {
        const member_span = try ls_store.addMembers(allocator, merged.items);
        const merged_ls = try ls_store.addLambdaSet(allocator, .{ .members = member_span });
        try ls_store.symbol_lambda_sets.put(allocator, sym.raw(), merged_ls);
    }
    return changed;
}

/// Resolve the lambda set for an expression, following through blocks and branches.
fn resolveExprLambdaSet(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    ls_store: *Store,
    expr_id: MIR.ExprId,
) Allocator.Error!Idx {
    // Direct closure origin — singleton lambda set
    if (mir_store.closure_origins.get(@intFromEnum(expr_id))) |lifted_idx| {
        const lifted = mir_store.lifted_lambdas.items[lifted_idx];
        const member_span = try ls_store.addMembers(allocator, &.{.{
            .fn_symbol = lifted.fn_symbol,
            .captures_monotype = lifted.captures_monotype,
        }});
        return try ls_store.addLambdaSet(allocator, .{ .members = member_span });
    }

    const expr = mir_store.getExpr(expr_id);
    switch (expr) {
        // Block: the lambda set is the lambda set of the final expression
        .block => |block| {
            return resolveExprLambdaSet(allocator, mir_store, ls_store, block.final_expr);
        },
        // Match: merge lambda sets from all branches
        .match_expr => |match| {
            const branches = mir_store.getBranches(match.branches);
            var all_members: std.ArrayListUnmanaged(Member) = .empty;
            defer all_members.deinit(allocator);

            for (branches) |branch| {
                const branch_ls = try resolveExprLambdaSet(allocator, mir_store, ls_store, branch.body);
                if (!branch_ls.isNone()) {
                    const branch_members = ls_store.getMembers(ls_store.getLambdaSet(branch_ls).members);
                    for (branch_members) |m| {
                        // Deduplicate by fn_symbol
                        var found = false;
                        for (all_members.items) |existing| {
                            if (existing.fn_symbol.eql(m.fn_symbol)) {
                                found = true;
                                break;
                            }
                        }
                        if (!found) {
                            try all_members.append(allocator, m);
                        }
                    }
                }
            }

            if (all_members.items.len == 0) return Idx.none;
            const member_span = try ls_store.addMembers(allocator, all_members.items);
            return try ls_store.addLambdaSet(allocator, .{ .members = member_span });
        },
        // Lookup: follow the symbol's definition
        .lookup => |symbol| {
            if (mir_store.getSymbolDef(symbol)) |def_expr| {
                return resolveExprLambdaSet(allocator, mir_store, ls_store, def_expr);
            }
            return Idx.none;
        },
        // Everything else: not a closure value
        else => return Idx.none,
    }
}
