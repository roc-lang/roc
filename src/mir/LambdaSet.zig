//! Lambda Set Inference for MIR
//!
//! Determines which lifted functions can flow to each function-typed symbol.
//! After CIR→MIR lowering, closures have been lifted to top-level functions
//! and their use sites produce captures tuples. This pass traces which lifted
//! function(s) each symbol could refer to.
//!
//! Results are stored in a LambdaSetStore, consumed by MirToLir for dispatch.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const MIR = @import("MIR.zig");
const Monotype = @import("Monotype.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;

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
pub fn infer(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    all_module_envs: []const *ModuleEnv,
) Allocator.Error!Store {
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

    // Propagate lambda sets across both HOF call edges and capture aliases until
    // stable. These relations depend on each other (a callee lambda set may come
    // from aliasing, and that can unlock additional call-arg propagation).
    var changed = true;
    while (changed) {
        changed = false;
        changed = (try propagateCallArgs(allocator, mir_store, &store, all_module_envs)) or changed;
        changed = (try propagateCaptureAliases(allocator, mir_store, &store)) or changed;
    }

    return store;
}

/// Propagate lambda sets from call arguments to callee parameters.
/// Uses pre-recorded HofCallArg entries from lowering — no expression scanning needed.
fn propagateCallArgs(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    ls_store: *Store,
    all_module_envs: []const *ModuleEnv,
) Allocator.Error!bool {
    var changed = false;

    for (mir_store.hof_call_args.items) |hof| {
        // Resolve the arg's lambda set — works for both lookups (hoisted lambdas)
        // and direct closure expressions (captures tuples with closure_origins).
        const arg_ls = getArgLambdaSet(allocator, mir_store, ls_store, hof.arg_expr) orelse continue;
        const arg_index: usize = @intCast(hof.arg_index);
        const call_expr = mir_store.getExpr(hof.call_func);
        const owner_fn_symbol = if (call_expr == .lookup) call_expr.lookup else MIR.Symbol.none;

        // Fast path: callee resolves directly to a lambda/lookup-with-def.
        if (resolveToLambdaParams(mir_store, hof.call_func)) |params| {
            changed = (try propagateArgIntoParams(
                allocator,
                mir_store,
                ls_store,
                all_module_envs,
                params,
                arg_index,
                arg_ls,
                owner_fn_symbol,
            )) or changed;
            continue;
        }

        // Fallback: callee may be a lookup to a symbol that has a lambda set
        // but no direct symbol_def (e.g. higher-order parameter). In that case,
        // propagate into each member function's corresponding parameter.
        if (call_expr == .lookup) {
            changed = (try propagateArgIntoCalleeMembers(
                allocator,
                mir_store,
                ls_store,
                all_module_envs,
                call_expr.lookup,
                arg_index,
                arg_ls,
            )) or changed;
        }
    }

    return changed;
}

/// Propagate lambda sets through capture aliases. A capture-local inside
/// a lifted function body aliases an outer-scope symbol; it should have
/// the same lambda set so closure call dispatch works.
fn propagateCaptureAliases(allocator: Allocator, mir_store: *const MIR.Store, ls_store: *Store) Allocator.Error!bool {
    var changed = false;
    var alias_it = mir_store.capture_symbol_aliases.iterator();
    while (alias_it.next()) |entry| {
        const capture_local_key = entry.key_ptr.*;
        const canonical_key = entry.value_ptr.*;
        if (ls_store.symbol_lambda_sets.get(canonical_key)) |ls_idx| {
            const did_change = try mergeIntoSymbol(
                allocator,
                ls_store,
                MIR.Symbol.fromRaw(capture_local_key),
                ls_idx,
            );
            changed = did_change or changed;
        }
    }
    return changed;
}

fn propagateArgIntoParams(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    ls_store: *Store,
    all_module_envs: []const *ModuleEnv,
    params: MIR.PatternSpan,
    arg_index: usize,
    arg_ls: Idx,
    owner_fn_symbol: MIR.Symbol,
) Allocator.Error!bool {
    const param_ids = mir_store.getPatternSpan(params);
    if (arg_index >= param_ids.len) return false;

    const param_id = param_ids[arg_index];
    const param_pat = mir_store.getPattern(param_id);
    if (param_pat != .bind) return false;

    const param_mono = mir_store.patternTypeOf(param_id);
    if (std.debug.runtime_safety) {
        assertLambdaSetCompatibleForParamType(
            mir_store,
            ls_store,
            all_module_envs,
            arg_ls,
            param_mono,
            param_pat.bind,
            owner_fn_symbol,
            arg_index,
        );
    }

    return mergeIntoSymbol(allocator, ls_store, param_pat.bind, arg_ls);
}

fn propagateArgIntoCalleeMembers(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    ls_store: *Store,
    all_module_envs: []const *ModuleEnv,
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
        const did_change = try propagateArgIntoParams(
            allocator,
            mir_store,
            ls_store,
            all_module_envs,
            params,
            arg_index,
            arg_ls,
            member.fn_symbol,
        );
        changed = did_change or changed;
    }
    return changed;
}

fn symbolModuleIdx(sym: MIR.Symbol) u32 {
    const raw = sym.raw();
    return @intCast((raw >> 32) & 0x7fff_ffff);
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
    if (!roundtrip.eql(ident)) return null;
    return text;
}

fn identTextInModule(
    all_module_envs: []const *ModuleEnv,
    module_idx: u32,
    ident: Ident.Idx,
) ?[]const u8 {
    if (module_idx >= all_module_envs.len) return null;
    return identTextIfOwnedBy(all_module_envs[module_idx], ident);
}

fn tagNameTextEquivalent(lhs: []const u8, rhs: []const u8) bool {
    if (std.mem.eql(u8, lhs, rhs)) return true;
    const lhs_last = if (std.mem.lastIndexOfScalar(u8, lhs, '.')) |dot| lhs[dot + 1 ..] else lhs;
    const rhs_last = if (std.mem.lastIndexOfScalar(u8, rhs, '.')) |dot| rhs[dot + 1 ..] else rhs;
    return std.mem.eql(u8, lhs_last, rhs_last);
}

fn monotypesCompatible(
    mir_store: *const MIR.Store,
    all_module_envs: []const *ModuleEnv,
    lhs_mono: Monotype.Idx,
    lhs_module_idx: u32,
    rhs_mono: Monotype.Idx,
    rhs_module_idx: u32,
) bool {
    var seen = std.AutoHashMap(u128, void).init(std.heap.page_allocator);
    defer seen.deinit();
    return monotypesCompatibleRec(
        mir_store,
        all_module_envs,
        lhs_mono,
        lhs_module_idx,
        rhs_mono,
        rhs_module_idx,
        &seen,
    );
}

fn monotypesCompatibleRec(
    mir_store: *const MIR.Store,
    all_module_envs: []const *ModuleEnv,
    lhs_mono: Monotype.Idx,
    lhs_module_idx: u32,
    rhs_mono: Monotype.Idx,
    rhs_module_idx: u32,
    seen: *std.AutoHashMap(u128, void),
) bool {
    if (lhs_module_idx == rhs_module_idx and lhs_mono == rhs_mono) return true;

    const lhs_u32: u32 = @intFromEnum(lhs_mono);
    const rhs_u32: u32 = @intFromEnum(rhs_mono);
    const key: u128 = (@as(u128, lhs_module_idx) << 96) |
        (@as(u128, lhs_u32) << 64) |
        (@as(u128, rhs_module_idx) << 32) |
        @as(u128, rhs_u32);

    if (seen.contains(key)) return true;
    _ = seen.put(key, {}) catch return false;

    const lhs = mir_store.monotype_store.getMonotype(lhs_mono);
    const rhs = mir_store.monotype_store.getMonotype(rhs_mono);
    if (std.meta.activeTag(lhs) != std.meta.activeTag(rhs)) return false;

    return switch (lhs) {
        .recursive_placeholder => false,
        .unit => true,
        .prim => |lhs_prim| lhs_prim == rhs.prim,
        .list => |lhs_list| monotypesCompatibleRec(
            mir_store,
            all_module_envs,
            lhs_list.elem,
            lhs_module_idx,
            rhs.list.elem,
            rhs_module_idx,
            seen,
        ),
        .box => |lhs_box| monotypesCompatibleRec(
            mir_store,
            all_module_envs,
            lhs_box.inner,
            lhs_module_idx,
            rhs.box.inner,
            rhs_module_idx,
            seen,
        ),
        .tuple => |lhs_tuple| blk: {
            const lhs_elems = mir_store.monotype_store.getIdxSpan(lhs_tuple.elems);
            const rhs_elems = mir_store.monotype_store.getIdxSpan(rhs.tuple.elems);
            if (lhs_elems.len != rhs_elems.len) break :blk false;
            for (lhs_elems, rhs_elems) |lhs_elem, rhs_elem| {
                if (!monotypesCompatibleRec(
                    mir_store,
                    all_module_envs,
                    lhs_elem,
                    lhs_module_idx,
                    rhs_elem,
                    rhs_module_idx,
                    seen,
                )) {
                    break :blk false;
                }
            }
            break :blk true;
        },
        .func => |lhs_func| blk: {
            const rhs_func = rhs.func;
            if (lhs_func.effectful != rhs_func.effectful) break :blk false;

            const lhs_args = mir_store.monotype_store.getIdxSpan(lhs_func.args);
            const rhs_args = mir_store.monotype_store.getIdxSpan(rhs_func.args);
            if (lhs_args.len != rhs_args.len) break :blk false;
            for (lhs_args, rhs_args) |lhs_arg, rhs_arg| {
                if (!monotypesCompatibleRec(
                    mir_store,
                    all_module_envs,
                    lhs_arg,
                    lhs_module_idx,
                    rhs_arg,
                    rhs_module_idx,
                    seen,
                )) {
                    break :blk false;
                }
            }

            break :blk monotypesCompatibleRec(
                mir_store,
                all_module_envs,
                lhs_func.ret,
                lhs_module_idx,
                rhs_func.ret,
                rhs_module_idx,
                seen,
            );
        },
        .record => |lhs_record| blk: {
            const lhs_fields = mir_store.monotype_store.getFields(lhs_record.fields);
            const rhs_fields = mir_store.monotype_store.getFields(rhs.record.fields);
            if (lhs_fields.len != rhs_fields.len) break :blk false;

            for (lhs_fields, rhs_fields) |lhs_field, rhs_field| {
                const lhs_name = identTextInModule(all_module_envs, lhs_module_idx, lhs_field.name) orelse return false;
                const rhs_name = identTextInModule(all_module_envs, rhs_module_idx, rhs_field.name) orelse return false;
                if (!std.mem.eql(u8, lhs_name, rhs_name)) break :blk false;

                if (!monotypesCompatibleRec(
                    mir_store,
                    all_module_envs,
                    lhs_field.type_idx,
                    lhs_module_idx,
                    rhs_field.type_idx,
                    rhs_module_idx,
                    seen,
                )) {
                    break :blk false;
                }
            }

            break :blk true;
        },
        .tag_union => |lhs_union| blk: {
            const lhs_tags = mir_store.monotype_store.getTags(lhs_union.tags);
            const rhs_tags = mir_store.monotype_store.getTags(rhs.tag_union.tags);
            if (lhs_tags.len != rhs_tags.len) break :blk false;

            for (lhs_tags, rhs_tags) |lhs_tag, rhs_tag| {
                const lhs_name = identTextInModule(all_module_envs, lhs_module_idx, lhs_tag.name) orelse return false;
                const rhs_name = identTextInModule(all_module_envs, rhs_module_idx, rhs_tag.name) orelse return false;
                if (!tagNameTextEquivalent(lhs_name, rhs_name)) break :blk false;

                const lhs_payloads = mir_store.monotype_store.getIdxSpan(lhs_tag.payloads);
                const rhs_payloads = mir_store.monotype_store.getIdxSpan(rhs_tag.payloads);
                if (lhs_payloads.len != rhs_payloads.len) break :blk false;
                for (lhs_payloads, rhs_payloads) |lhs_payload, rhs_payload| {
                    if (!monotypesCompatibleRec(
                        mir_store,
                        all_module_envs,
                        lhs_payload,
                        lhs_module_idx,
                        rhs_payload,
                        rhs_module_idx,
                        seen,
                    )) {
                        break :blk false;
                    }
                }
            }

            break :blk true;
        },
    };
}

fn debugPrintMonotypeShallow(mir_store: *const MIR.Store, mono_idx: Monotype.Idx) void {
    const mono = mir_store.monotype_store.getMonotype(mono_idx);
    switch (mono) {
        .unit => std.debug.print("mono {d}: unit\n", .{@intFromEnum(mono_idx)}),
        .prim => |p| std.debug.print("mono {d}: prim.{s}\n", .{ @intFromEnum(mono_idx), @tagName(p) }),
        .list => |l| std.debug.print("mono {d}: list elem={d}\n", .{ @intFromEnum(mono_idx), @intFromEnum(l.elem) }),
        .box => |b| std.debug.print("mono {d}: box inner={d}\n", .{ @intFromEnum(mono_idx), @intFromEnum(b.inner) }),
        .tuple => |t| {
            const elems = mir_store.monotype_store.getIdxSpan(t.elems);
            std.debug.print("mono {d}: tuple(", .{@intFromEnum(mono_idx)});
            for (elems, 0..) |elem, i| {
                if (i != 0) std.debug.print(", ", .{});
                std.debug.print("{d}", .{@intFromEnum(elem)});
            }
            std.debug.print(")\n", .{});
        },
        .func => |f| {
            const args = mir_store.monotype_store.getIdxSpan(f.args);
            std.debug.print("mono {d}: func(", .{@intFromEnum(mono_idx)});
            for (args, 0..) |arg, i| {
                if (i != 0) std.debug.print(", ", .{});
                std.debug.print("{d}", .{@intFromEnum(arg)});
            }
            std.debug.print(") -> {d} effectful={}\n", .{ @intFromEnum(f.ret), f.effectful });
        },
        .record => |r| {
            const fields = mir_store.monotype_store.getFields(r.fields);
            std.debug.print("mono {d}: record{{", .{@intFromEnum(mono_idx)});
            for (fields, 0..) |field, i| {
                if (i != 0) std.debug.print(", ", .{});
                std.debug.print("{d}:{d}", .{ field.name.idx, @intFromEnum(field.type_idx) });
            }
            std.debug.print("}}\n", .{});
        },
        .tag_union => |u| {
            const tags = mir_store.monotype_store.getTags(u.tags);
            std.debug.print("mono {d}: tag_union[", .{@intFromEnum(mono_idx)});
            for (tags, 0..) |tag, i| {
                if (i != 0) std.debug.print(", ", .{});
                std.debug.print("{d}(", .{tag.name.idx});
                const payloads = mir_store.monotype_store.getIdxSpan(tag.payloads);
                for (payloads, 0..) |p, j| {
                    if (j != 0) std.debug.print(", ", .{});
                    std.debug.print("{d}", .{@intFromEnum(p)});
                }
                std.debug.print(")", .{});
            }
            std.debug.print("]\n", .{});
        },
        .recursive_placeholder => std.debug.print("mono {d}: recursive_placeholder\n", .{@intFromEnum(mono_idx)}),
    }
}

/// Debug-only assertion: every lambda-set member propagated into a function-typed
/// parameter must be signature-compatible with that parameter type.
fn assertLambdaSetCompatibleForParamType(
    mir_store: *const MIR.Store,
    ls_store: *Store,
    all_module_envs: []const *ModuleEnv,
    arg_ls_idx: Idx,
    param_mono: Monotype.Idx,
    param_symbol: MIR.Symbol,
    owner_fn_symbol: MIR.Symbol,
    arg_index: usize,
) void {
    const param_mono_val = mir_store.monotype_store.getMonotype(param_mono);
    if (param_mono_val != .func) return;
    const param_func = param_mono_val.func;
    const param_args = mir_store.monotype_store.getIdxSpan(param_func.args);
    const param_module_idx = symbolModuleIdx(param_symbol);

    const arg_ls = ls_store.getLambdaSet(arg_ls_idx);
    const members = ls_store.getMembers(arg_ls.members);
    for (members) |member| {
        const member_module_idx = symbolModuleIdx(member.fn_symbol);
        const def_expr_id = mir_store.getSymbolDef(member.fn_symbol) orelse {
            std.debug.panic(
                "LambdaSet compatibility failure: missing def for member fn_symbol={d}",
                .{member.fn_symbol.raw()},
            );
        };
        const member_mono = mir_store.typeOf(def_expr_id);
        const member_mono_val = mir_store.monotype_store.getMonotype(member_mono);
        if (member_mono_val != .func) {
            std.debug.panic(
                "LambdaSet compatibility failure: member fn_symbol={d} has non-func monotype={d}",
                .{ member.fn_symbol.raw(), @intFromEnum(member_mono) },
            );
        }

        const member_func = member_mono_val.func;
        const member_args = mir_store.monotype_store.getIdxSpan(member_func.args);
        const expected_args = param_args.len + @as(usize, if (member.captures_monotype.isNone()) 0 else 1);
        if (member_args.len != expected_args or !monotypesCompatible(
            mir_store,
            all_module_envs,
            param_func.ret,
            param_module_idx,
            member_func.ret,
            member_module_idx,
        )) {
            std.debug.print(
                "LambdaSet compatibility mismatch: owner fn={d} arg_index={d} param_mono={d} param_symbol={d}\n",
                .{
                    owner_fn_symbol.raw(),
                    arg_index,
                    @intFromEnum(param_mono),
                    param_symbol.raw(),
                },
            );
            if (!owner_fn_symbol.isNone()) {
                if (mir_store.getSymbolDef(owner_fn_symbol)) |owner_def_expr_id| {
                    const owner_mono = mir_store.typeOf(owner_def_expr_id);
                    const owner_mono_val = mir_store.monotype_store.getMonotype(owner_mono);
                    if (owner_mono_val == .func) {
                        const owner_args = mir_store.monotype_store.getIdxSpan(owner_mono_val.func.args);
                        std.debug.print("  owner type args:", .{});
                        for (owner_args) |a| {
                            std.debug.print(" {d}", .{@intFromEnum(a)});
                        }
                        std.debug.print(" -> {d}\n", .{@intFromEnum(owner_mono_val.func.ret)});
                    } else {
                        std.debug.print("  owner type mono={d} (non-func)\n", .{@intFromEnum(owner_mono)});
                    }
                } else {
                    std.debug.print("  owner has no symbol def\n", .{});
                }
            }
            std.debug.print(
                "  incoming member fn={d} captures={d}\n",
                .{
                    member.fn_symbol.raw(),
                    if (member.captures_monotype.isNone()) @as(u32, std.math.maxInt(u32)) else @intFromEnum(member.captures_monotype),
                },
            );
            std.debug.print("  param args:", .{});
            for (param_args) |a| {
                std.debug.print(" {d}", .{@intFromEnum(a)});
            }
            std.debug.print(" -> {d}\n", .{@intFromEnum(param_func.ret)});

            std.debug.print("  member args:", .{});
            for (member_args) |a| {
                std.debug.print(" {d}", .{@intFromEnum(a)});
            }
            std.debug.print(" -> {d}\n", .{@intFromEnum(member_func.ret)});
            std.debug.print("  module contexts: param_module={d}, member_module={d}\n", .{ param_module_idx, member_module_idx });

            std.debug.print("  param func monotype detail:\n", .{});
            debugPrintMonotypeShallow(mir_store, param_mono);
            for (param_args) |a| debugPrintMonotypeShallow(mir_store, a);
            debugPrintMonotypeShallow(mir_store, param_func.ret);

            std.debug.print("  member func monotype detail:\n", .{});
            debugPrintMonotypeShallow(mir_store, member_mono);
            for (member_args) |a| debugPrintMonotypeShallow(mir_store, a);
            debugPrintMonotypeShallow(mir_store, member_func.ret);

            std.debug.panic(
                "LambdaSet propagation type mismatch for fn-typed parameter symbol={d}",
                .{param_symbol.raw()},
            );
        }

        for (param_args, 0..) |param_arg_mono, i| {
            if (!monotypesCompatible(
                mir_store,
                all_module_envs,
                param_arg_mono,
                param_module_idx,
                member_args[i],
                member_module_idx,
            )) {
                std.debug.print(
                    "LambdaSet compatibility mismatch: owner fn={d} arg_index={d} param_symbol={d} param_arg #{d} expected mono={d}, got mono={d}\n",
                    .{ owner_fn_symbol.raw(), arg_index, param_symbol.raw(), i, @intFromEnum(param_arg_mono), @intFromEnum(member_args[i]) },
                );
                std.debug.print("  param args:", .{});
                for (param_args) |a| {
                    std.debug.print(" {d}", .{@intFromEnum(a)});
                }
                std.debug.print(" -> {d}\n", .{@intFromEnum(param_func.ret)});

                std.debug.print("  member fn={d} args:", .{member.fn_symbol.raw()});
                for (member_args) |a| {
                    std.debug.print(" {d}", .{@intFromEnum(a)});
                }
                std.debug.print(" -> {d}\n", .{@intFromEnum(member_func.ret)});

                debugPrintMonotypeShallow(mir_store, param_arg_mono);
                debugPrintMonotypeShallow(mir_store, member_args[i]);

                std.debug.panic(
                    "LambdaSet propagation type mismatch for fn-typed parameter symbol={d}",
                    .{param_symbol.raw()},
                );
            }
        }
    }
}

/// Get the lambda set for a call argument expression.
/// Handles lookups (check symbol_lambda_sets) and direct expressions (resolveExprLambdaSet).
fn getArgLambdaSet(allocator: Allocator, mir_store: *const MIR.Store, ls_store: *Store, arg_expr_id: MIR.ExprId) ?Idx {
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

fn addMemberDedup(allocator: Allocator, members: *std.ArrayListUnmanaged(Member), candidate: Member) Allocator.Error!void {
    for (members.items) |existing| {
        if (existing.fn_symbol.eql(candidate.fn_symbol)) return;
    }
    try members.append(allocator, candidate);
}

/// Resolve the lambda set for an expression, following through blocks, lookups,
/// and closure-returning calls.
pub fn resolveExprLambdaSet(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    ls_store: *Store,
    expr_id: MIR.ExprId,
) Allocator.Error!Idx {
    var visiting = std.AutoHashMapUnmanaged(u32, void).empty;
    defer visiting.deinit(allocator);
    return resolveExprLambdaSetRec(allocator, mir_store, ls_store, expr_id, &visiting);
}

fn resolveExprLambdaSetRec(
    allocator: Allocator,
    mir_store: *const MIR.Store,
    ls_store: *Store,
    expr_id: MIR.ExprId,
    visiting: *std.AutoHashMapUnmanaged(u32, void),
) Allocator.Error!Idx {
    const expr_key = @intFromEnum(expr_id);
    if (visiting.contains(expr_key)) return Idx.none;
    try visiting.put(allocator, expr_key, {});
    defer _ = visiting.remove(expr_key);

    // Direct closure origin — singleton lambda set
    if (mir_store.closure_origins.get(expr_key)) |lifted_idx| {
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
            return resolveExprLambdaSetRec(allocator, mir_store, ls_store, block.final_expr, visiting);
        },
        // Match: merge lambda sets from all branches
        .match_expr => |match| {
            const branches = mir_store.getBranches(match.branches);
            var all_members: std.ArrayListUnmanaged(Member) = .empty;
            defer all_members.deinit(allocator);

            for (branches) |branch| {
                const branch_ls = try resolveExprLambdaSetRec(allocator, mir_store, ls_store, branch.body, visiting);
                if (!branch_ls.isNone()) {
                    const branch_members = ls_store.getMembers(ls_store.getLambdaSet(branch_ls).members);
                    for (branch_members) |m| {
                        try addMemberDedup(allocator, &all_members, m);
                    }
                }
            }

            if (all_members.items.len == 0) return Idx.none;
            const member_span = try ls_store.addMembers(allocator, all_members.items);
            return try ls_store.addLambdaSet(allocator, .{ .members = member_span });
        },
        // Call: infer the return lambda set from each callee member's lambda body.
        .call => |call| {
            const callee_expr = mir_store.getExpr(call.func);
            if (callee_expr == .lambda) {
                return resolveExprLambdaSetRec(
                    allocator,
                    mir_store,
                    ls_store,
                    callee_expr.lambda.body,
                    visiting,
                );
            }

            const callee_ls = try resolveExprLambdaSetRec(allocator, mir_store, ls_store, call.func, visiting);
            if (callee_ls.isNone()) return Idx.none;

            const callee_members = ls_store.getMembers(ls_store.getLambdaSet(callee_ls).members);
            var returned_members: std.ArrayListUnmanaged(Member) = .empty;
            defer returned_members.deinit(allocator);

            for (callee_members) |callee_member| {
                const callee_def = mir_store.getSymbolDef(callee_member.fn_symbol) orelse continue;
                const callee_def_expr = mir_store.getExpr(callee_def);
                if (callee_def_expr != .lambda) continue;

                const body_ls = try resolveExprLambdaSetRec(
                    allocator,
                    mir_store,
                    ls_store,
                    callee_def_expr.lambda.body,
                    visiting,
                );
                if (body_ls.isNone()) continue;

                const body_members = ls_store.getMembers(ls_store.getLambdaSet(body_ls).members);
                for (body_members) |m| {
                    try addMemberDedup(allocator, &returned_members, m);
                }
            }

            if (returned_members.items.len == 0) return Idx.none;
            const member_span = try ls_store.addMembers(allocator, returned_members.items);
            return try ls_store.addLambdaSet(allocator, .{ .members = member_span });
        },
        // Lookup: prefer known symbol lambda set, then follow definition.
        .lookup => |symbol| {
            if (ls_store.getSymbolLambdaSet(symbol)) |ls_idx| return ls_idx;
            if (mir_store.getSymbolDef(symbol)) |def_expr| {
                return resolveExprLambdaSetRec(allocator, mir_store, ls_store, def_expr, visiting);
            }
            return Idx.none;
        },
        // Everything else: not a closure value
        else => return Idx.none,
    }
}
