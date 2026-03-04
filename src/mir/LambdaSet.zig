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
        }
    }

    return store;
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
            var all_members = std.ArrayList(Member).init(allocator);
            defer all_members.deinit();

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
                            try all_members.append(m);
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
