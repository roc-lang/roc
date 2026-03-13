//! Eager lambda-set inference for MIR.
//!
//! The pass computes one authoritative lambda-set result for every relevant
//! expression, symbol, and callable member. Downstream consumers should query
//! the store directly; they must not recover closure identity by walking MIR.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const MIR = @import("MIR.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;

/// Index into the lambda_sets array.
pub const Idx = enum(u32) {
    _,

    /// Sentinel used when no lambda set has been assigned.
    pub const none: Idx = @enumFromInt(std.math.maxInt(u32));

    /// Whether this index is the sentinel empty value.
    pub fn isNone(self: Idx) bool {
        return self == none;
    }
};

/// One possible callable member of a lambda set.
pub const Member = struct {
    fn_symbol: MIR.Symbol,
    closure_member: MIR.ClosureMemberId,
};

/// Span of Member values in the members array.
pub const MemberSpan = extern struct {
    start: u32,
    len: u16,

    /// The empty span.
    pub fn empty() MemberSpan {
        return .{ .start = 0, .len = 0 };
    }

    /// Whether this span contains no members.
    pub fn isEmpty(self: MemberSpan) bool {
        return self.len == 0;
    }
};

/// One deduplicated lambda-set entry.
pub const LambdaSet = struct {
    members: MemberSpan,
};

/// Eagerly-computed lambda-set facts for expressions, symbols, and callable members.
pub const Store = struct {
    lambda_sets: std.ArrayListUnmanaged(LambdaSet),
    members: std.ArrayListUnmanaged(Member),
    symbol_lambda_sets: std.AutoHashMapUnmanaged(u64, Idx),
    expr_lambda_sets: std.AutoHashMapUnmanaged(u32, Idx),
    member_return_lambda_sets: std.AutoHashMapUnmanaged(u64, Idx),

    /// Create an empty lambda-set store.
    pub fn init() Store {
        return .{
            .lambda_sets = .empty,
            .members = .empty,
            .symbol_lambda_sets = .empty,
            .expr_lambda_sets = .empty,
            .member_return_lambda_sets = .empty,
        };
    }

    /// Release all lambda-set analysis storage.
    pub fn deinit(self: *Store, allocator: Allocator) void {
        self.lambda_sets.deinit(allocator);
        self.members.deinit(allocator);
        self.symbol_lambda_sets.deinit(allocator);
        self.expr_lambda_sets.deinit(allocator);
        self.member_return_lambda_sets.deinit(allocator);
    }

    /// Append callable members and return their span.
    pub fn addMembers(self: *Store, allocator: Allocator, member_list: []const Member) !MemberSpan {
        if (member_list.len == 0) return MemberSpan.empty();
        const start: u32 = @intCast(self.members.items.len);
        try self.members.appendSlice(allocator, member_list);
        return .{ .start = start, .len = @intCast(member_list.len) };
    }

    /// Resolve a member span into the underlying member slice.
    pub fn getMembers(self: *const Store, span: MemberSpan) []const Member {
        if (span.len == 0) return &.{};
        return self.members.items[span.start..][0..span.len];
    }

    /// Intern one lambda-set entry and return its index.
    pub fn addLambdaSet(self: *Store, allocator: Allocator, ls: LambdaSet) !Idx {
        const idx: u32 = @intCast(self.lambda_sets.items.len);
        try self.lambda_sets.append(allocator, ls);
        return @enumFromInt(idx);
    }

    /// Read one interned lambda-set entry.
    pub fn getLambdaSet(self: *const Store, idx: Idx) LambdaSet {
        return self.lambda_sets.items[@intFromEnum(idx)];
    }

    /// Get the lambda set inferred for a symbol, if one exists.
    pub fn getSymbolLambdaSet(self: *const Store, symbol: MIR.Symbol) ?Idx {
        return self.symbol_lambda_sets.get(symbol.raw());
    }

    /// Get the lambda set inferred for an expression, if one exists.
    pub fn getExprLambdaSet(self: *const Store, expr_id: MIR.ExprId) ?Idx {
        return self.expr_lambda_sets.get(@intFromEnum(expr_id));
    }

    /// Get the inferred return lambda set for a callable member, if one exists.
    pub fn getMemberReturnLambdaSet(self: *const Store, fn_symbol: MIR.Symbol) ?Idx {
        return self.member_return_lambda_sets.get(fn_symbol.raw());
    }
};

/// Whether an expression evaluates directly to a lambda, ignoring block wrappers.
pub fn isLambdaExpr(mir_store: *const MIR.Store, expr_id: MIR.ExprId) bool {
    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .block => |block| isLambdaExpr(mir_store, block.final_expr),
        .lambda => true,
        else => false,
    };
}

/// Infer authoritative lambda sets for all reachable MIR expressions and symbols.
pub fn infer(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    module_envs: []const *ModuleEnv,
) Allocator.Error!Store {
    var store = Store.init();
    errdefer store.deinit(allocator);

    var lambda_expr_symbols = std.AutoHashMapUnmanaged(u32, MIR.Symbol).empty;
    defer lambda_expr_symbols.deinit(allocator);

    try seedClosureMembers(allocator, mir_store, &store);
    try seedSymbolDefs(allocator, mir_store, &store, &lambda_expr_symbols);

    var changed = true;
    while (changed) {
        changed = false;
        changed = (try propagateExprAndBindingSets(allocator, mir_store, module_envs, &store, &lambda_expr_symbols)) or changed;
        changed = (try propagateCallArgs(allocator, mir_store, &store)) or changed;
        changed = (try propagateCapturedFunctionLocals(allocator, mir_store, &store)) or changed;
        changed = (try propagateMemberReturnSets(allocator, mir_store, &store)) or changed;
    }

    return store;
}

fn seedClosureMembers(allocator: Allocator, mir_store: *const MIR.Store, store: *Store) Allocator.Error!void {
    var it = mir_store.expr_closure_members.iterator();
    while (it.next()) |entry| {
        const expr_id: MIR.ExprId = @enumFromInt(entry.key_ptr.*);
        const member = memberFromClosureMember(mir_store, entry.value_ptr.*);
        const singleton = try singletonLambdaSet(allocator, store, member);
        try store.expr_lambda_sets.put(allocator, @intFromEnum(expr_id), singleton);
    }
}

fn seedSymbolDefs(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
    lambda_expr_symbols: *std.AutoHashMapUnmanaged(u32, MIR.Symbol),
) Allocator.Error!void {
    var it = mir_store.symbol_defs.iterator();
    while (it.next()) |entry| {
        const symbol = MIR.Symbol.fromRaw(entry.key_ptr.*);
        const expr_id = entry.value_ptr.*;

        if (mir_store.getExprClosureMember(expr_id)) |closure_member| {
            const member = memberFromClosureMember(mir_store, closure_member);
            const singleton = try singletonLambdaSet(allocator, store, member);
            try store.symbol_lambda_sets.put(allocator, symbol.raw(), singleton);
            _ = try mergeExprLambdaSet(allocator, store, expr_id, singleton);
            continue;
        }

        if (isLambdaExpr(mir_store, expr_id)) {
            const member = plainLambdaMember(symbol);
            const singleton = try singletonLambdaSet(allocator, store, member);
            try store.symbol_lambda_sets.put(allocator, symbol.raw(), singleton);
            try store.expr_lambda_sets.put(allocator, @intFromEnum(expr_id), singleton);
            try lambda_expr_symbols.put(allocator, @intFromEnum(expr_id), symbol);
        }
    }
}

fn propagateExprAndBindingSets(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    module_envs: []const *ModuleEnv,
    store: *Store,
    lambda_expr_symbols: *const std.AutoHashMapUnmanaged(u32, MIR.Symbol),
) Allocator.Error!bool {
    var changed = false;
    var expr_index: u32 = 0;
    while (expr_index < mir_store.exprs.items.len) : (expr_index += 1) {
        const expr_id: MIR.ExprId = @enumFromInt(expr_index);
        changed = (try propagateExprAndBindingSetsForExpr(
            allocator,
            mir_store,
            module_envs,
            store,
            lambda_expr_symbols,
            expr_id,
        )) or changed;
    }
    return changed;
}

fn propagateExprAndBindingSetsForExpr(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    module_envs: []const *ModuleEnv,
    store: *Store,
    lambda_expr_symbols: *const std.AutoHashMapUnmanaged(u32, MIR.Symbol),
    expr_id: MIR.ExprId,
) Allocator.Error!bool {
    var changed = false;

    if (lambda_expr_symbols.get(@intFromEnum(expr_id))) |symbol| {
        const singleton = try singletonLambdaSet(allocator, store, plainLambdaMember(symbol));
        changed = (try mergeExprLambdaSet(allocator, store, expr_id, singleton)) or changed;
    }

    switch (mir_store.getExpr(expr_id)) {
        .block => |block| {
            if (store.getExprLambdaSet(block.final_expr)) |ls_idx| {
                changed = (try mergeExprLambdaSet(allocator, store, expr_id, ls_idx)) or changed;
            }
            const stmts = mir_store.getStmts(block.stmts);
            for (stmts) |stmt| {
                const binding = switch (stmt) {
                    .decl_const, .decl_var, .mutate_var => |b| b,
                };
                if (store.getExprLambdaSet(binding.expr)) |ls_idx| {
                    if (patternBoundSymbol(mir_store, binding.pattern)) |symbol| {
                        changed = (try mergeIntoSymbol(allocator, store, symbol, ls_idx)) or changed;
                    }
                }
            }
        },
        .borrow_scope => |scope| {
            if (store.getExprLambdaSet(scope.body)) |ls_idx| {
                changed = (try mergeExprLambdaSet(allocator, store, expr_id, ls_idx)) or changed;
            }
            for (mir_store.getBorrowBindings(scope.bindings)) |binding| {
                if (store.getExprLambdaSet(binding.expr)) |ls_idx| {
                    if (patternBoundSymbol(mir_store, binding.pattern)) |symbol| {
                        changed = (try mergeIntoSymbol(allocator, store, symbol, ls_idx)) or changed;
                    }
                }
            }
        },
        .match_expr => |match_expr| {
            var merged: std.ArrayListUnmanaged(Member) = .empty;
            defer merged.deinit(allocator);
            for (mir_store.getBranches(match_expr.branches)) |branch| {
                if (store.getExprLambdaSet(branch.body)) |ls_idx| {
                    try appendMembersDedup(allocator, &merged, store.getMembers(store.getLambdaSet(ls_idx).members));
                }
            }
            if (merged.items.len > 0) {
                const merged_ls = try internLambdaSet(allocator, store, merged.items);
                changed = (try mergeExprLambdaSet(allocator, store, expr_id, merged_ls)) or changed;
            }
        },
        .call => |call| {
            if (store.getExprLambdaSet(call.func)) |callee_ls| {
                var merged: std.ArrayListUnmanaged(Member) = .empty;
                defer merged.deinit(allocator);
                for (store.getMembers(store.getLambdaSet(callee_ls).members)) |member| {
                    if (store.getMemberReturnLambdaSet(member.fn_symbol)) |ret_ls| {
                        try appendMembersDedup(allocator, &merged, store.getMembers(store.getLambdaSet(ret_ls).members));
                    }
                }
                if (merged.items.len > 0) {
                    const merged_ls = try internLambdaSet(allocator, store, merged.items);
                    changed = (try mergeExprLambdaSet(allocator, store, expr_id, merged_ls)) or changed;
                }
            }
        },
        .lookup => |symbol| {
            if (store.getSymbolLambdaSet(symbol)) |ls_idx| {
                changed = (try mergeExprLambdaSet(allocator, store, expr_id, ls_idx)) or changed;
            }
        },
        .record_access => |ra| {
            if (resolveRecordFieldLambdaSet(mir_store, module_envs, store, ra.record, ra.field_name)) |ls_idx| {
                changed = (try mergeExprLambdaSet(allocator, store, expr_id, ls_idx)) or changed;
            }
        },
        .tuple_access => |ta| {
            if (resolveTupleElemLambdaSet(mir_store, store, ta.tuple, ta.elem_index)) |ls_idx| {
                changed = (try mergeExprLambdaSet(allocator, store, expr_id, ls_idx)) or changed;
            }
        },
        .hosted => |hosted| {
            if (store.getExprLambdaSet(hosted.body)) |ls_idx| {
                changed = (try mergeExprLambdaSet(allocator, store, expr_id, ls_idx)) or changed;
            }
        },
        .dbg_expr => |dbg_expr| {
            if (store.getExprLambdaSet(dbg_expr.expr)) |ls_idx| {
                changed = (try mergeExprLambdaSet(allocator, store, expr_id, ls_idx)) or changed;
            }
        },
        .expect => |expect| {
            if (store.getExprLambdaSet(expect.body)) |ls_idx| {
                changed = (try mergeExprLambdaSet(allocator, store, expr_id, ls_idx)) or changed;
            }
        },
        .return_expr => |ret| {
            if (store.getExprLambdaSet(ret.expr)) |ls_idx| {
                changed = (try mergeExprLambdaSet(allocator, store, expr_id, ls_idx)) or changed;
            }
        },
        else => {},
    }

    return changed;
}

fn propagateCallArgs(allocator: Allocator, mir_store: *const MIR.Store, store: *Store) Allocator.Error!bool {
    var changed = false;
    var expr_index: u32 = 0;
    while (expr_index < mir_store.exprs.items.len) : (expr_index += 1) {
        const expr = mir_store.getExpr(@enumFromInt(expr_index));
        if (expr != .call) continue;

        const call = expr.call;
        const callee_ls = store.getExprLambdaSet(call.func) orelse continue;
        const args = mir_store.getExprSpan(call.args);
        for (args, 0..) |arg_expr, arg_index| {
            const arg_ls = store.getExprLambdaSet(arg_expr) orelse continue;
            for (store.getMembers(store.getLambdaSet(callee_ls).members)) |member| {
                const params = paramsForMember(mir_store, member) orelse continue;
                const param_ids = mir_store.getPatternSpan(params);
                if (arg_index >= param_ids.len) continue;
                const param_symbol = patternBoundSymbol(mir_store, param_ids[arg_index]) orelse continue;
                changed = (try mergeIntoSymbol(allocator, store, param_symbol, arg_ls)) or changed;
            }
        }
    }
    return changed;
}

fn propagateCapturedFunctionLocals(allocator: Allocator, mir_store: *const MIR.Store, store: *Store) Allocator.Error!bool {
    var changed = false;
    for (mir_store.closure_members.items) |closure_member| {
        for (mir_store.getCaptureBindings(closure_member.capture_bindings)) |binding| {
            const ls_idx = store.getExprLambdaSet(binding.source_expr) orelse continue;
            changed = (try mergeIntoSymbol(allocator, store, binding.local_symbol, ls_idx)) or changed;
        }
    }
    return changed;
}

fn propagateMemberReturnSets(allocator: Allocator, mir_store: *const MIR.Store, store: *Store) Allocator.Error!bool {
    var changed = false;

    var it = mir_store.symbol_defs.iterator();
    while (it.next()) |entry| {
        const fn_symbol = MIR.Symbol.fromRaw(entry.key_ptr.*);
        const def_expr_id = entry.value_ptr.*;
        if (resolveToLambdaParams(mir_store, def_expr_id) == null) continue;
        const body = resolveToLambdaBody(mir_store, def_expr_id) orelse continue;
        const body_ls = store.getExprLambdaSet(body) orelse continue;
        changed = (try mergeMemberReturnLambdaSet(allocator, store, fn_symbol, body_ls)) or changed;
    }

    return changed;
}

fn paramsForMember(mir_store: *const MIR.Store, member: Member) ?MIR.PatternSpan {
    const def_expr_id = mir_store.getSymbolDef(member.fn_symbol) orelse return null;
    return resolveToLambdaParams(mir_store, def_expr_id);
}

fn resolveToLambdaParams(mir_store: *const MIR.Store, expr_id: MIR.ExprId) ?MIR.PatternSpan {
    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .lambda => |lam| lam.params,
        .block => |block| resolveToLambdaParams(mir_store, block.final_expr),
        else => null,
    };
}

fn resolveToLambdaBody(mir_store: *const MIR.Store, expr_id: MIR.ExprId) ?MIR.ExprId {
    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .lambda => |lam| lam.body,
        .block => |block| resolveToLambdaBody(mir_store, block.final_expr),
        else => null,
    };
}

fn identTextIfOwnedBy(env: *const ModuleEnv, ident: Ident.Idx) ?[]const u8 {
    const ident_store = env.getIdentStoreConst();
    const bytes = ident_store.interner.bytes.items.items;
    const start: usize = @intCast(ident.idx);
    if (start >= bytes.len) return null;

    const tail = bytes[start..];
    const end_rel = std.mem.indexOfScalar(u8, tail, 0) orelse return null;
    const text = tail[0..end_rel];

    const roundtrip = ident_store.findByString(text) orelse return null;
    if (roundtrip.idx != ident.idx) return null;
    return text;
}

fn identsTextEqual(module_envs: []const *ModuleEnv, lhs: Ident.Idx, rhs: Ident.Idx) bool {
    if (lhs.eql(rhs)) return true;

    for (module_envs) |lhs_env| {
        const lhs_text = identTextIfOwnedBy(lhs_env, lhs) orelse continue;
        for (module_envs) |rhs_env| {
            const rhs_text = identTextIfOwnedBy(rhs_env, rhs) orelse continue;
            if (std.mem.eql(u8, lhs_text, rhs_text)) return true;
        }
    }

    return false;
}

fn resolveRecordFieldLambdaSet(
    mir_store: *const MIR.Store,
    module_envs: []const *ModuleEnv,
    store: *const Store,
    expr_id: MIR.ExprId,
    field_name: Ident.Idx,
) ?Idx {
    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .record => |record| blk: {
            const field_names = mir_store.getFieldNameSpan(record.field_names);
            const fields = mir_store.getExprSpan(record.fields);
            for (field_names, 0..) |name, i| {
                if (!identsTextEqual(module_envs, name, field_name)) continue;
                break :blk store.getExprLambdaSet(fields[i]);
            }
            break :blk null;
        },
        .lookup => |symbol| blk: {
            const def_expr = mir_store.getSymbolDef(symbol) orelse break :blk null;
            break :blk resolveRecordFieldLambdaSet(mir_store, module_envs, store, def_expr, field_name);
        },
        .block => |block| resolveRecordFieldLambdaSet(mir_store, module_envs, store, block.final_expr, field_name),
        else => null,
    };
}

fn resolveTupleElemLambdaSet(mir_store: *const MIR.Store, store: *const Store, expr_id: MIR.ExprId, elem_index: u32) ?Idx {
    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .tuple => |tuple| blk: {
            const elems = mir_store.getExprSpan(tuple.elems);
            if (elem_index >= elems.len) break :blk null;
            break :blk store.getExprLambdaSet(elems[elem_index]);
        },
        .lookup => |symbol| blk: {
            const def_expr = mir_store.getSymbolDef(symbol) orelse break :blk null;
            break :blk resolveTupleElemLambdaSet(mir_store, store, def_expr, elem_index);
        },
        .block => |block| resolveTupleElemLambdaSet(mir_store, store, block.final_expr, elem_index),
        else => null,
    };
}

fn patternBoundSymbol(mir_store: *const MIR.Store, pat_id: MIR.PatternId) ?MIR.Symbol {
    return switch (mir_store.getPattern(pat_id)) {
        .bind => |sym| sym,
        .as_pattern => |as_pat| as_pat.symbol,
        else => null,
    };
}

fn plainLambdaMember(symbol: MIR.Symbol) Member {
    return .{
        .fn_symbol = symbol,
        .closure_member = .none,
    };
}

fn memberFromClosureMember(mir_store: *const MIR.Store, closure_member_id: MIR.ClosureMemberId) Member {
    const closure_member = mir_store.getClosureMember(closure_member_id);
    return .{
        .fn_symbol = closure_member.fn_symbol,
        .closure_member = closure_member_id,
    };
}

fn singletonLambdaSet(allocator: Allocator, store: *Store, member: Member) Allocator.Error!Idx {
    return internLambdaSet(allocator, store, &.{member});
}

fn internLambdaSet(allocator: Allocator, store: *Store, members: []const Member) Allocator.Error!Idx {
    const span = try store.addMembers(allocator, members);
    return store.addLambdaSet(allocator, .{ .members = span });
}

fn appendMembersDedup(allocator: Allocator, dest: *std.ArrayListUnmanaged(Member), src: []const Member) Allocator.Error!void {
    for (src) |candidate| {
        if (containsMember(dest.items, candidate)) continue;
        try dest.append(allocator, candidate);
    }
}

fn containsMember(existing: []const Member, candidate: Member) bool {
    for (existing) |member| {
        if (member.fn_symbol.eql(candidate.fn_symbol)) return true;
    }
    return false;
}

fn mergeIntoSymbol(allocator: Allocator, store: *Store, symbol: MIR.Symbol, new_ls_idx: Idx) Allocator.Error!bool {
    const existing = store.symbol_lambda_sets.get(symbol.raw());
    if (existing == null) {
        try store.symbol_lambda_sets.put(allocator, symbol.raw(), new_ls_idx);
        return true;
    }
    return mergeLambdaSetEntries(allocator, store, &store.symbol_lambda_sets, symbol.raw(), existing.?, new_ls_idx);
}

fn mergeExprLambdaSet(allocator: Allocator, store: *Store, expr_id: MIR.ExprId, new_ls_idx: Idx) Allocator.Error!bool {
    const expr_key = @intFromEnum(expr_id);
    const existing = store.expr_lambda_sets.get(expr_key);
    if (existing == null) {
        try store.expr_lambda_sets.put(allocator, expr_key, new_ls_idx);
        return true;
    }
    return mergeLambdaSetEntries(allocator, store, &store.expr_lambda_sets, expr_key, existing.?, new_ls_idx);
}

fn mergeMemberReturnLambdaSet(allocator: Allocator, store: *Store, fn_symbol: MIR.Symbol, new_ls_idx: Idx) Allocator.Error!bool {
    const existing = store.member_return_lambda_sets.get(fn_symbol.raw());
    if (existing == null) {
        try store.member_return_lambda_sets.put(allocator, fn_symbol.raw(), new_ls_idx);
        return true;
    }
    return mergeLambdaSetEntries(allocator, store, &store.member_return_lambda_sets, fn_symbol.raw(), existing.?, new_ls_idx);
}

fn mergeLambdaSetEntries(
    allocator: Allocator,
    store: *Store,
    map: anytype,
    key: anytype,
    existing_ls_idx: Idx,
    new_ls_idx: Idx,
) Allocator.Error!bool {
    if (existing_ls_idx == new_ls_idx) return false;

    const existing_members = store.getMembers(store.getLambdaSet(existing_ls_idx).members);
    const new_members = store.getMembers(store.getLambdaSet(new_ls_idx).members);

    var merged: std.ArrayListUnmanaged(Member) = .empty;
    defer merged.deinit(allocator);
    try merged.appendSlice(allocator, existing_members);
    try appendMembersDedup(allocator, &merged, new_members);

    if (merged.items.len == existing_members.len) return false;
    const merged_ls = try internLambdaSet(allocator, store, merged.items);
    try map.put(allocator, key, merged_ls);
    return true;
}
