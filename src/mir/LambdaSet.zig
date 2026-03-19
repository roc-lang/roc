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
    proc: MIR.ProcId,
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
    symbol_source_exprs: std.AutoHashMapUnmanaged(u64, MIR.ExprId),
    symbol_lambda_sets: std.AutoHashMapUnmanaged(u64, Idx),
    symbol_field_lambda_sets: std.AutoHashMapUnmanaged(u128, Idx),
    symbol_field_exprs: std.AutoHashMapUnmanaged(u128, MIR.ExprId),
    symbol_tag_payload_exprs: std.AutoHashMapUnmanaged(u128, MIR.ExprId),
    symbol_list_elem_exprs: std.AutoHashMapUnmanaged(u128, MIR.ExprId),
    symbol_list_lengths: std.AutoHashMapUnmanaged(u64, u32),
    expr_lambda_sets: std.AutoHashMapUnmanaged(u32, Idx),
    member_return_lambda_sets: std.AutoHashMapUnmanaged(u32, Idx),
    member_return_field_lambda_sets: std.AutoHashMapUnmanaged(u64, Idx),

    /// Create an empty lambda-set store.
    pub fn init() Store {
        return .{
            .lambda_sets = .empty,
            .members = .empty,
            .symbol_source_exprs = .empty,
            .symbol_lambda_sets = .empty,
            .symbol_field_lambda_sets = .empty,
            .symbol_field_exprs = .empty,
            .symbol_tag_payload_exprs = .empty,
            .symbol_list_elem_exprs = .empty,
            .symbol_list_lengths = .empty,
            .expr_lambda_sets = .empty,
            .member_return_lambda_sets = .empty,
            .member_return_field_lambda_sets = .empty,
        };
    }

    /// Release all lambda-set analysis storage.
    pub fn deinit(self: *Store, allocator: Allocator) void {
        self.lambda_sets.deinit(allocator);
        self.members.deinit(allocator);
        self.symbol_source_exprs.deinit(allocator);
        self.symbol_lambda_sets.deinit(allocator);
        self.symbol_field_lambda_sets.deinit(allocator);
        self.symbol_field_exprs.deinit(allocator);
        self.symbol_tag_payload_exprs.deinit(allocator);
        self.symbol_list_elem_exprs.deinit(allocator);
        self.symbol_list_lengths.deinit(allocator);
        self.expr_lambda_sets.deinit(allocator);
        self.member_return_lambda_sets.deinit(allocator);
        self.member_return_field_lambda_sets.deinit(allocator);
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

    pub fn getSymbolSourceExpr(self: *const Store, symbol: MIR.Symbol) ?MIR.ExprId {
        return self.symbol_source_exprs.get(symbol.raw());
    }

    /// Get the lambda set inferred for one function-typed field of a symbol.
    pub fn getSymbolFieldLambdaSet(self: *const Store, symbol: MIR.Symbol, field_idx: u32) ?Idx {
        return self.symbol_field_lambda_sets.get(structFieldKey(symbol, field_idx));
    }

    pub fn getSymbolFieldExpr(self: *const Store, symbol: MIR.Symbol, field_idx: u32) ?MIR.ExprId {
        return self.symbol_field_exprs.get(structFieldKey(symbol, field_idx));
    }

    pub fn getSymbolTagPayloadExpr(self: *const Store, symbol: MIR.Symbol, tag_name: Ident.Idx, payload_idx: u32) ?MIR.ExprId {
        return self.symbol_tag_payload_exprs.get(tagPayloadKey(symbol, tag_name, payload_idx));
    }

    pub fn getSymbolListElemExpr(self: *const Store, symbol: MIR.Symbol, elem_idx: u32) ?MIR.ExprId {
        return self.symbol_list_elem_exprs.get(listElemKey(symbol, elem_idx));
    }

    pub fn getSymbolListLength(self: *const Store, symbol: MIR.Symbol) ?u32 {
        return self.symbol_list_lengths.get(symbol.raw());
    }

    /// Get the lambda set inferred for an expression, if one exists.
    pub fn getExprLambdaSet(self: *const Store, expr_id: MIR.ExprId) ?Idx {
        return self.expr_lambda_sets.get(@intFromEnum(expr_id));
    }

    /// Get the inferred return lambda set for a callable member, if one exists.
    pub fn getMemberReturnLambdaSet(self: *const Store, proc: MIR.ProcId) ?Idx {
        return self.member_return_lambda_sets.get(procKey(proc));
    }

    /// Get the inferred return lambda set for one function-typed field of a member.
    pub fn getMemberReturnFieldLambdaSet(self: *const Store, proc: MIR.ProcId, field_idx: u32) ?Idx {
        return self.member_return_field_lambda_sets.get(procFieldKey(proc, field_idx));
    }
};

/// Whether an expression evaluates directly to a proc-backed callable, ignoring wrappers.
pub fn isLambdaExpr(mir_store: *const MIR.Store, expr_id: MIR.ExprId) bool {
    return resolveDirectProcMember(mir_store, expr_id) != null;
}

/// Infer authoritative lambda sets for all reachable MIR expressions and symbols.
pub fn infer(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    _: []const *ModuleEnv,
) Allocator.Error!Store {
    var store = Store.init();
    errdefer store.deinit(allocator);

    try seedSymbolSources(allocator, mir_store, &store);
    try seedClosureMembers(allocator, mir_store, &store);
    try seedExactSymbolProcSets(allocator, mir_store, &store);
    try seedValueDefs(allocator, mir_store, &store);

    var changed = true;
    while (changed) {
        changed = false;
        changed = (try propagateExprAndBindingSets(allocator, mir_store, &store)) or changed;
        changed = (try propagateMatchPatternBindings(allocator, mir_store, &store)) or changed;
        changed = (try propagateCallArgs(allocator, mir_store, &store)) or changed;
        changed = (try propagateCapturedFunctionLocals(allocator, mir_store, &store)) or changed;
        changed = (try propagateMemberReturnSets(allocator, mir_store, &store)) or changed;
    }

    return store;
}

fn seedSymbolSources(allocator: Allocator, mir_store: *const MIR.Store, store: *Store) Allocator.Error!void {
    var visiting = std.AutoHashMap(u64, void).init(allocator);
    defer visiting.deinit();

    var it = mir_store.value_defs.iterator();
    while (it.next()) |entry| {
        const symbol = MIR.Symbol.fromRaw(entry.key_ptr.*);
        try seedSymbolSource(allocator, mir_store, store, symbol, entry.value_ptr.*, &visiting);
    }
}

fn seedSymbolSource(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
    symbol: MIR.Symbol,
    expr_id: MIR.ExprId,
    visiting: *std.AutoHashMap(u64, void),
) Allocator.Error!void {
    const key = symbol.raw();
    if (store.symbol_source_exprs.contains(key)) return;
    if (visiting.contains(key)) return;

    try visiting.put(key, {});
    defer _ = visiting.remove(key);

    try store.symbol_source_exprs.put(allocator, key, expr_id);
    try seedCompositeSymbolSourcesFromExpr(allocator, mir_store, store, symbol, expr_id, visiting);
}

fn seedCompositeSymbolSourcesFromExpr(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
    symbol: MIR.Symbol,
    expr_id: MIR.ExprId,
    visiting: *std.AutoHashMap(u64, void),
) Allocator.Error!void {
    switch (mir_store.getExpr(expr_id)) {
        .struct_ => |struct_| {
            const fields = mir_store.getExprSpan(struct_.fields);
            for (fields, 0..) |field_expr, field_idx| {
                try store.symbol_field_exprs.put(
                    allocator,
                    structFieldKey(symbol, @intCast(field_idx)),
                    field_expr,
                );
            }
        },
        .tag => |tag_expr| {
            const payloads = mir_store.getExprSpan(tag_expr.args);
            for (payloads, 0..) |payload_expr, payload_idx| {
                try store.symbol_tag_payload_exprs.put(
                    allocator,
                    tagPayloadKey(symbol, tag_expr.name, @intCast(payload_idx)),
                    payload_expr,
                );
            }
        },
        .list => |list_expr| {
            const elems = mir_store.getExprSpan(list_expr.elems);
            try store.symbol_list_lengths.put(allocator, symbol.raw(), @intCast(elems.len));
            for (elems, 0..) |elem_expr, elem_idx| {
                try store.symbol_list_elem_exprs.put(
                    allocator,
                    listElemKey(symbol, @intCast(elem_idx)),
                    elem_expr,
                );
            }
        },
        .lookup => |source_symbol| {
            _ = store.getSymbolSourceExpr(source_symbol) orelse blk: {
                const source_def = mir_store.getValueDef(source_symbol) orelse return;
                try seedSymbolSource(allocator, mir_store, store, source_symbol, source_def, visiting);
                break :blk store.getSymbolSourceExpr(source_symbol) orelse return;
            };

            if (structFieldArityForExpr(mir_store, expr_id)) |field_count| {
                var field_idx: u32 = 0;
                while (field_idx < field_count) : (field_idx += 1) {
                    const field_expr = store.getSymbolFieldExpr(source_symbol, field_idx) orelse continue;
                    try store.symbol_field_exprs.put(allocator, structFieldKey(symbol, field_idx), field_expr);
                }
            }

            const mono = mir_store.monotype_store.getMonotype(mir_store.typeOf(expr_id));
            if (mono == .tag_union) {
                for (mir_store.monotype_store.getTags(mono.tag_union.tags)) |tag| {
                    const payloads = mir_store.monotype_store.getIdxSpan(tag.payloads);
                    for (payloads, 0..) |_, payload_idx| {
                        const payload_expr = store.getSymbolTagPayloadExpr(source_symbol, tag.name.ident, @intCast(payload_idx)) orelse continue;
                        try store.symbol_tag_payload_exprs.put(
                            allocator,
                            tagPayloadKey(symbol, tag.name.ident, @intCast(payload_idx)),
                            payload_expr,
                        );
                    }
                }
            }

            if (store.getSymbolListLength(source_symbol)) |list_len| {
                try store.symbol_list_lengths.put(allocator, symbol.raw(), list_len);
                var elem_idx: u32 = 0;
                while (elem_idx < list_len) : (elem_idx += 1) {
                    const elem_expr = store.getSymbolListElemExpr(source_symbol, elem_idx) orelse continue;
                    try store.symbol_list_elem_exprs.put(allocator, listElemKey(symbol, elem_idx), elem_expr);
                }
            }
        },
        .block => |block| try seedCompositeSymbolSourcesFromExpr(allocator, mir_store, store, symbol, block.final_expr, visiting),
        .dbg_expr => |dbg_expr| try seedCompositeSymbolSourcesFromExpr(allocator, mir_store, store, symbol, dbg_expr.expr, visiting),
        .expect => |expect| try seedCompositeSymbolSourcesFromExpr(allocator, mir_store, store, symbol, expect.body, visiting),
        .return_expr => |ret| try seedCompositeSymbolSourcesFromExpr(allocator, mir_store, store, symbol, ret.expr, visiting),
        .struct_access => |sa| {
            const field_expr = resolveStructFieldExpr(mir_store, store, sa.struct_, sa.field_idx) orelse return;
            try seedCompositeSymbolSourcesFromExpr(allocator, mir_store, store, symbol, field_expr, visiting);
        },
        else => {},
    }
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

fn seedExactSymbolProcSets(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
) Allocator.Error!void {
    var it = mir_store.symbol_seed_proc_sets.iterator();
    while (it.next()) |entry| {
        const symbol = MIR.Symbol.fromRaw(entry.key_ptr.*);
        const proc_ids = mir_store.getProcSpan(entry.value_ptr.*);

        var members = std.ArrayListUnmanaged(Member){};
        defer members.deinit(allocator);
        for (proc_ids) |proc_id| {
            const member = if (mir_store.getClosureMemberForProc(proc_id)) |closure_member_id|
                memberFromClosureMember(mir_store, closure_member_id)
            else
                plainProcMember(proc_id);
            try appendMembersDedup(allocator, &members, &.{member});
        }

        if (members.items.len == 0) continue;
        const ls_idx = try internLambdaSet(allocator, store, members.items);
        _ = try mergeIntoSymbol(allocator, store, symbol, ls_idx);
    }
}

fn seedValueDefs(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
) Allocator.Error!void {
    var it = mir_store.value_defs.iterator();
    while (it.next()) |entry| {
        const symbol = MIR.Symbol.fromRaw(entry.key_ptr.*);
        const expr_id = entry.value_ptr.*;

        if (resolveDirectProcMember(mir_store, expr_id)) |member| {
            const singleton = try singletonLambdaSet(allocator, store, member);
            try store.symbol_lambda_sets.put(allocator, symbol.raw(), singleton);
            _ = try mergeExprLambdaSet(allocator, store, expr_id, singleton);
        }
    }
}

fn propagateExprAndBindingSets(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
) Allocator.Error!bool {
    var changed = false;
    var expr_index: u32 = 0;
    while (expr_index < mir_store.exprs.items.len) : (expr_index += 1) {
        const expr_id: MIR.ExprId = @enumFromInt(expr_index);
        changed = (try propagateExprAndBindingSetsForExpr(
            allocator,
            mir_store,
            store,
            expr_id,
        )) or changed;
    }
    return changed;
}

fn propagateExprAndBindingSetsForExpr(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
    expr_id: MIR.ExprId,
) Allocator.Error!bool {
    var changed = false;

    if (resolveDirectProcMember(mir_store, expr_id)) |member| {
        const singleton = try singletonLambdaSet(allocator, store, member);
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
                    if (store.getMemberReturnLambdaSet(member.proc)) |ret_ls| {
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
        .struct_access => |sa| {
            if (resolveStructFieldLambdaSet(mir_store, store, sa.struct_, sa.field_idx)) |ls_idx| {
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
            for (store.getMembers(store.getLambdaSet(callee_ls).members)) |member| {
                const params = paramsForMember(mir_store, member) orelse continue;
                const param_ids = mir_store.getPatternSpan(params);
                if (arg_index >= param_ids.len) continue;
                const param_symbol = patternBoundSymbol(mir_store, param_ids[arg_index]) orelse continue;
                if (store.getExprLambdaSet(arg_expr)) |arg_ls| {
                    changed = (try mergeIntoSymbol(allocator, store, param_symbol, arg_ls)) or changed;
                }
                changed = (try propagateStructFieldLambdaSetsToSymbol(
                    allocator,
                    mir_store,
                    store,
                    arg_expr,
                    param_symbol,
                )) or changed;
                changed = (try propagateCallReturnFieldLambdaSetsToSymbol(
                    allocator,
                    mir_store,
                    store,
                    arg_expr,
                    param_symbol,
                )) or changed;
            }
        }
    }
    return changed;
}

fn propagateMatchPatternBindings(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
) Allocator.Error!bool {
    var changed = false;
    var expr_index: u32 = 0;
    while (expr_index < mir_store.exprs.items.len) : (expr_index += 1) {
        const expr = mir_store.getExpr(@enumFromInt(expr_index));
        if (expr != .match_expr) continue;

        const match_expr = expr.match_expr;
        for (mir_store.getBranches(match_expr.branches)) |branch| {
            for (mir_store.getBranchPatterns(branch.patterns)) |branch_pattern| {
                changed = (try propagatePatternBindingsFromExpr(
                    allocator,
                    mir_store,
                    store,
                    match_expr.cond,
                    branch_pattern.pattern,
                )) or changed;
            }
        }
    }
    return changed;
}

fn propagatePatternBindingsFromExpr(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
    source_expr: MIR.ExprId,
    pattern_id: MIR.PatternId,
) Allocator.Error!bool {
    var changed = false;

    switch (mir_store.getPattern(pattern_id)) {
        .bind => |symbol| {
            if (store.getExprLambdaSet(source_expr)) |ls_idx| {
                changed = (try mergeIntoSymbol(allocator, store, symbol, ls_idx)) or changed;
            }
        },
        .as_pattern => |as_pat| {
            if (store.getExprLambdaSet(source_expr)) |ls_idx| {
                changed = (try mergeIntoSymbol(allocator, store, as_pat.symbol, ls_idx)) or changed;
            }
            changed = (try propagatePatternBindingsFromExpr(
                allocator,
                mir_store,
                store,
                source_expr,
                as_pat.pattern,
            )) or changed;
        },
        .tag => |tag_pat| {
            const arg_patterns = mir_store.getPatternSpan(tag_pat.args);
            for (arg_patterns, 0..) |arg_pattern_id, arg_index| {
                const payload_expr = resolveTagPayloadExpr(mir_store, store, source_expr, tag_pat.name, @intCast(arg_index)) orelse continue;
                changed = (try propagatePatternBindingsFromExpr(
                    allocator,
                    mir_store,
                    store,
                    payload_expr,
                    arg_pattern_id,
                )) or changed;
            }
        },
        .struct_destructure => |destructure| {
            const field_patterns = mir_store.getPatternSpan(destructure.fields);
            for (field_patterns, 0..) |field_pattern_id, field_index| {
                const field_expr = resolveStructFieldExpr(mir_store, store, source_expr, @intCast(field_index)) orelse continue;
                changed = (try propagatePatternBindingsFromExpr(
                    allocator,
                    mir_store,
                    store,
                    field_expr,
                    field_pattern_id,
                )) or changed;
            }
        },
        .list_destructure => |destructure| {
            const elem_patterns = mir_store.getPatternSpan(destructure.patterns);
            for (elem_patterns, 0..) |elem_pattern_id, elem_index| {
                const elem_expr = resolveListElementExpr(mir_store, store, source_expr, @intCast(elem_index)) orelse continue;
                changed = (try propagatePatternBindingsFromExpr(
                    allocator,
                    mir_store,
                    store,
                    elem_expr,
                    elem_pattern_id,
                )) or changed;
            }
        },
        .wildcard,
        .int_literal,
        .str_literal,
        .dec_literal,
        .frac_f32_literal,
        .frac_f64_literal,
        .runtime_error,
        => {},
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

    for (mir_store.getProcs(), 0..) |proc, proc_idx| {
        const proc_id: MIR.ProcId = @enumFromInt(proc_idx);
        if (store.getExprLambdaSet(proc.body)) |body_ls| {
            changed = (try mergeMemberReturnLambdaSet(allocator, store, proc_id, body_ls)) or changed;
        }
        changed = (try propagateReturnFieldLambdaSetsToMember(
            allocator,
            mir_store,
            store,
            proc.body,
            proc_id,
        )) or changed;
    }

    return changed;
}

fn paramsForMember(mir_store: *const MIR.Store, member: Member) ?MIR.PatternSpan {
    if (member.proc.isNone()) return null;
    return mir_store.getProc(member.proc).params;
}

fn resolveStructFieldLambdaSet(
    mir_store: *const MIR.Store,
    store: *const Store,
    expr_id: MIR.ExprId,
    field_idx: u32,
) ?Idx {
    const expr = mir_store.getExpr(expr_id);
    if (expr == .lookup) {
        if (store.getSymbolFieldLambdaSet(expr.lookup, field_idx)) |ls_idx| return ls_idx;
    }
    const field_expr = resolveStructFieldExpr(mir_store, store, expr_id, field_idx) orelse return null;
    return store.getExprLambdaSet(field_expr);
}

fn resolveStructFieldExpr(
    mir_store: *const MIR.Store,
    store: *const Store,
    expr_id: MIR.ExprId,
    field_idx: u32,
) ?MIR.ExprId {
    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .struct_ => |struct_| blk: {
            const fields = mir_store.getExprSpan(struct_.fields);
            if (field_idx >= fields.len) break :blk null;
            break :blk fields[field_idx];
        },
        .lookup => |symbol| store.getSymbolFieldExpr(symbol, field_idx),
        .block => |block| resolveStructFieldExpr(mir_store, store, block.final_expr, field_idx),
        .dbg_expr => |dbg_expr| resolveStructFieldExpr(mir_store, store, dbg_expr.expr, field_idx),
        .expect => |expect| resolveStructFieldExpr(mir_store, store, expect.body, field_idx),
        .return_expr => |ret| resolveStructFieldExpr(mir_store, store, ret.expr, field_idx),
        .struct_access => |sa| blk: {
            const base_field = resolveStructFieldExpr(mir_store, store, sa.struct_, sa.field_idx) orelse break :blk null;
            break :blk resolveStructFieldExpr(mir_store, store, base_field, field_idx);
        },
        else => null,
    };
}

fn structFieldKey(symbol: MIR.Symbol, field_idx: u32) u128 {
    return (@as(u128, symbol.raw()) << 32) | @as(u128, field_idx);
}

fn tagPayloadKey(symbol: MIR.Symbol, tag_name: Ident.Idx, payload_idx: u32) u128 {
    return (@as(u128, symbol.raw()) << 64) |
        (@as(u128, tag_name.idx) << 32) |
        @as(u128, payload_idx);
}

fn listElemKey(symbol: MIR.Symbol, elem_idx: u32) u128 {
    return (@as(u128, symbol.raw()) << 32) | @as(u128, elem_idx);
}

fn structFieldArityForExpr(mir_store: *const MIR.Store, expr_id: MIR.ExprId) ?u32 {
    return structFieldArityForMonotype(mir_store, mir_store.typeOf(expr_id));
}

fn structFieldArityForMonotype(mir_store: *const MIR.Store, mono_idx: anytype) ?u32 {
    const mono = mir_store.monotype_store.getMonotype(mono_idx);
    return switch (mono) {
        .record => |record| @intCast(mir_store.monotype_store.getFields(record.fields).len),
        .tuple => |tuple| @intCast(mir_store.monotype_store.getIdxSpan(tuple.elems).len),
        .box => |box| structFieldArityForMonotype(mir_store, box.inner),
        else => null,
    };
}

fn resolveTagPayloadExpr(
    mir_store: *const MIR.Store,
    store: *const Store,
    expr_id: MIR.ExprId,
    tag_name: Ident.Idx,
    payload_idx: u32,
) ?MIR.ExprId {
    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .tag => |tag_expr| blk: {
            if (!tag_expr.name.eql(tag_name)) break :blk null;
            const args = mir_store.getExprSpan(tag_expr.args);
            if (payload_idx >= args.len) break :blk null;
            break :blk args[payload_idx];
        },
        .lookup => |symbol| store.getSymbolTagPayloadExpr(symbol, tag_name, payload_idx),
        .block => |block| resolveTagPayloadExpr(mir_store, store, block.final_expr, tag_name, payload_idx),
        .dbg_expr => |dbg_expr| resolveTagPayloadExpr(mir_store, store, dbg_expr.expr, tag_name, payload_idx),
        .expect => |expect| resolveTagPayloadExpr(mir_store, store, expect.body, tag_name, payload_idx),
        .return_expr => |ret| resolveTagPayloadExpr(mir_store, store, ret.expr, tag_name, payload_idx),
        else => null,
    };
}

fn resolveListElementExpr(
    mir_store: *const MIR.Store,
    store: *const Store,
    expr_id: MIR.ExprId,
    elem_idx: u32,
) ?MIR.ExprId {
    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .list => |list_expr| blk: {
            const elems = mir_store.getExprSpan(list_expr.elems);
            if (elem_idx >= elems.len) break :blk null;
            break :blk elems[elem_idx];
        },
        .lookup => |symbol| store.getSymbolListElemExpr(symbol, elem_idx),
        .block => |block| resolveListElementExpr(mir_store, store, block.final_expr, elem_idx),
        .dbg_expr => |dbg_expr| resolveListElementExpr(mir_store, store, dbg_expr.expr, elem_idx),
        .expect => |expect| resolveListElementExpr(mir_store, store, expect.body, elem_idx),
        .return_expr => |ret| resolveListElementExpr(mir_store, store, ret.expr, elem_idx),
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

fn plainProcMember(proc: MIR.ProcId) Member {
    return .{
        .proc = proc,
        .closure_member = .none,
    };
}

fn memberFromClosureMember(mir_store: *const MIR.Store, closure_member_id: MIR.ClosureMemberId) Member {
    const closure_member = mir_store.getClosureMember(closure_member_id);
    return .{
        .proc = closure_member.proc,
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
        if (member.proc == candidate.proc) return true;
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

fn mergeIntoSymbolField(
    allocator: Allocator,
    store: *Store,
    symbol: MIR.Symbol,
    field_idx: u32,
    new_ls_idx: Idx,
) Allocator.Error!bool {
    const key = structFieldKey(symbol, field_idx);
    const existing = store.symbol_field_lambda_sets.get(key);
    if (existing == null) {
        try store.symbol_field_lambda_sets.put(allocator, key, new_ls_idx);
        return true;
    }
    return mergeLambdaSetEntries(allocator, store, &store.symbol_field_lambda_sets, key, existing.?, new_ls_idx);
}

fn propagateStructFieldLambdaSetsToSymbol(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
    expr_id: MIR.ExprId,
    symbol: MIR.Symbol,
) Allocator.Error!bool {
    const field_count = structFieldArityForExpr(mir_store, expr_id) orelse return false;

    var changed = false;
    var field_idx: u32 = 0;
    while (field_idx < field_count) : (field_idx += 1) {
        const field_ls = resolveStructFieldLambdaSet(mir_store, store, expr_id, field_idx) orelse continue;
        changed = (try mergeIntoSymbolField(allocator, store, symbol, field_idx, field_ls)) or changed;
    }

    return changed;
}

fn propagateReturnFieldLambdaSetsToMember(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
    expr_id: MIR.ExprId,
    proc: MIR.ProcId,
) Allocator.Error!bool {
    const field_count = structFieldArityForExpr(mir_store, expr_id) orelse return false;

    var changed = false;
    var field_idx: u32 = 0;
    while (field_idx < field_count) : (field_idx += 1) {
        const field_ls = resolveStructFieldLambdaSet(mir_store, store, expr_id, field_idx) orelse continue;
        changed = (try mergeMemberReturnFieldLambdaSet(allocator, store, proc, field_idx, field_ls)) or changed;
    }

    return changed;
}

fn propagateCallReturnFieldLambdaSetsToSymbol(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    store: *Store,
    expr_id: MIR.ExprId,
    symbol: MIR.Symbol,
) Allocator.Error!bool {
    const expr = mir_store.getExpr(expr_id);
    if (expr != .call) return false;

    const field_count = structFieldArityForExpr(mir_store, expr_id) orelse return false;
    const callee_ls = store.getExprLambdaSet(expr.call.func) orelse return false;

    var changed = false;
    for (store.getMembers(store.getLambdaSet(callee_ls).members)) |member| {
        var field_idx: u32 = 0;
        while (field_idx < field_count) : (field_idx += 1) {
            const field_ls = store.getMemberReturnFieldLambdaSet(member.proc, field_idx) orelse continue;
            changed = (try mergeIntoSymbolField(allocator, store, symbol, field_idx, field_ls)) or changed;
        }
    }

    return changed;
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

fn mergeMemberReturnLambdaSet(allocator: Allocator, store: *Store, proc: MIR.ProcId, new_ls_idx: Idx) Allocator.Error!bool {
    const key = procKey(proc);
    const existing = store.member_return_lambda_sets.get(key);
    if (existing == null) {
        try store.member_return_lambda_sets.put(allocator, key, new_ls_idx);
        return true;
    }
    return mergeLambdaSetEntries(allocator, store, &store.member_return_lambda_sets, key, existing.?, new_ls_idx);
}

fn mergeMemberReturnFieldLambdaSet(
    allocator: Allocator,
    store: *Store,
    proc: MIR.ProcId,
    field_idx: u32,
    new_ls_idx: Idx,
) Allocator.Error!bool {
    const key = procFieldKey(proc, field_idx);
    const existing = store.member_return_field_lambda_sets.get(key);
    if (existing == null) {
        try store.member_return_field_lambda_sets.put(allocator, key, new_ls_idx);
        return true;
    }
    return mergeLambdaSetEntries(allocator, store, &store.member_return_field_lambda_sets, key, existing.?, new_ls_idx);
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

fn resolveDirectProcMember(mir_store: *const MIR.Store, expr_id: MIR.ExprId) ?Member {
    if (mir_store.getExprClosureMember(expr_id)) |closure_member_id| {
        return memberFromClosureMember(mir_store, closure_member_id);
    }

    return switch (mir_store.getExpr(expr_id)) {
        .proc_ref => |proc| plainProcMember(proc),
        .closure_make => |closure| .{
            .proc = closure.proc,
            .closure_member = .none,
        },
        .block => |block| resolveDirectProcMember(mir_store, block.final_expr),
        .dbg_expr => |dbg_expr| resolveDirectProcMember(mir_store, dbg_expr.expr),
        .expect => |expect| resolveDirectProcMember(mir_store, expect.body),
        .return_expr => |ret| resolveDirectProcMember(mir_store, ret.expr),
        else => null,
    };
}

fn procKey(proc: MIR.ProcId) u32 {
    return @intFromEnum(proc);
}

fn procFieldKey(proc: MIR.ProcId, field_idx: u32) u64 {
    return (@as(u64, procKey(proc)) << 32) | @as(u64, field_idx);
}
