//! Tests for MIR Store, Monotype Store, and Lower initialization.

const std = @import("std");
const testing = std.testing;

const base = @import("base");
const can = @import("can");
const types = @import("types");
const MIR = @import("../MIR.zig");
const LambdaSet = @import("../LambdaSet.zig");
const Monotype = @import("../Monotype.zig");
const Monomorphize = @import("../Monomorphize.zig");
const Lower = @import("../Lower.zig");
const MirTestEnv = @import("MirTestEnv.zig");

const ModuleEnv = can.ModuleEnv;
const Region = base.Region;
const Ident = base.Ident;

const test_allocator = testing.allocator;

fn testSymbolFromIdent(ident: Ident.Idx) MIR.Symbol {
    return MIR.Symbol.fromRaw(@as(u64, @as(u32, @bitCast(ident))));
}

fn procIdFromExpr(mir_store: *const MIR.Store, expr_id: MIR.ExprId) ?MIR.ProcId {
    return switch (mir_store.getExpr(expr_id)) {
        .proc_ref => |proc_id| proc_id,
        .closure_make => |closure| closure.proc,
        .block => |block| procIdFromExpr(mir_store, block.final_expr),
        .dbg_expr => |dbg_expr| procIdFromExpr(mir_store, dbg_expr.expr),
        .expect => |expect| procIdFromExpr(mir_store, expect.body),
        .return_expr => |ret| procIdFromExpr(mir_store, ret.expr),
        else => null,
    };
}

fn procIdFromValueDef(mir_store: *const MIR.Store, symbol: MIR.Symbol) ?MIR.ProcId {
    const def_expr = mir_store.getValueDef(symbol) orelse return null;
    return procIdFromExpr(mir_store, def_expr);
}

fn procIdFromCallableExpr(mir_store: *const MIR.Store, expr_id: MIR.ExprId) ?MIR.ProcId {
    const expr = mir_store.getExpr(expr_id);
    return switch (expr) {
        .lookup => |sym| procIdFromValueDef(mir_store, sym),
        else => procIdFromExpr(mir_store, expr_id),
    };
}

fn firstCalledProcInStmts(mir_store: *const MIR.Store, stmts: []const MIR.Stmt) ?MIR.ProcId {
    for (stmts) |stmt| {
        const binding = switch (stmt) {
            .decl_const, .decl_var, .mutate_var => |b| b,
        };
        if (mir_store.getExpr(binding.expr) != .call) continue;
        if (procIdFromCallableExpr(mir_store, mir_store.getExpr(binding.expr).call.func)) |proc_id| {
            return proc_id;
        }
    }

    return null;
}

const ForeignParamLookup = struct {
    expr_id: MIR.ExprId,
    symbol: MIR.Symbol,
    owner_proc: MIR.ProcId,
};

fn dumpMirExpr(mir_store: *const MIR.Store, expr_id: MIR.ExprId, depth: usize) void {
    const indent = depth * 2;
    for (0..indent) |_| std.debug.print(" ", .{});

    const expr = mir_store.getExpr(expr_id);
    std.debug.print("expr {d}: {s}", .{ @intFromEnum(expr_id), @tagName(expr) });

    switch (expr) {
        .lookup => |symbol| {
            std.debug.print(" symbol={d}\n", .{symbol.raw()});
        },
        .proc_ref => |proc_id| {
            std.debug.print(" proc={d}\n", .{@intFromEnum(proc_id)});
        },
        .closure_make => |closure| {
            std.debug.print(" proc={d}\n", .{@intFromEnum(closure.proc)});
            dumpMirExpr(mir_store, closure.captures, depth + 1);
        },
        .call => |call| {
            std.debug.print("\n", .{});
            dumpMirExpr(mir_store, call.func, depth + 1);
            for (mir_store.getExprSpan(call.args)) |arg| dumpMirExpr(mir_store, arg, depth + 1);
        },
        .block => |block| {
            std.debug.print("\n", .{});
            for (mir_store.getStmts(block.stmts), 0..) |stmt, stmt_i| {
                for (0..indent + 2) |_| std.debug.print(" ", .{});
                std.debug.print("stmt {d}: {s}\n", .{ stmt_i, @tagName(stmt) });
                switch (stmt) {
                    .decl_const, .decl_var, .mutate_var => |binding| dumpMirExpr(mir_store, binding.expr, depth + 2),
                }
            }
            dumpMirExpr(mir_store, block.final_expr, depth + 1);
        },
        .struct_access => |access| {
            std.debug.print(" field={d}\n", .{access.field_idx});
            dumpMirExpr(mir_store, access.struct_, depth + 1);
        },
        .struct_ => |struct_| {
            std.debug.print("\n", .{});
            for (mir_store.getExprSpan(struct_.fields)) |field| dumpMirExpr(mir_store, field, depth + 1);
        },
        .for_loop => |for_loop| {
            std.debug.print("\n", .{});
            dumpMirExpr(mir_store, for_loop.list, depth + 1);
            dumpMirExpr(mir_store, for_loop.body, depth + 1);
        },
        .while_loop => |while_loop| {
            std.debug.print("\n", .{});
            dumpMirExpr(mir_store, while_loop.cond, depth + 1);
            dumpMirExpr(mir_store, while_loop.body, depth + 1);
        },
        .match_expr => |match_expr| {
            std.debug.print("\n", .{});
            dumpMirExpr(mir_store, match_expr.cond, depth + 1);
            for (mir_store.getBranches(match_expr.branches), 0..) |branch, branch_i| {
                for (0..indent + 2) |_| std.debug.print(" ", .{});
                std.debug.print("branch {d}\n", .{branch_i});
                if (!branch.guard.isNone()) dumpMirExpr(mir_store, branch.guard, depth + 2);
                dumpMirExpr(mir_store, branch.body, depth + 2);
            }
        },
        else => {
            std.debug.print("\n", .{});
        },
    }
}

fn firstForeignParamLookup(
    mir_store: *const MIR.Store,
    expr_id: MIR.ExprId,
    proc_id: MIR.ProcId,
    all_param_symbols: *const std.AutoHashMap(MIR.Symbol, MIR.ProcId),
) ?ForeignParamLookup {
    const expr = mir_store.getExpr(expr_id);
    switch (expr) {
        .lookup => |symbol| {
            if (all_param_symbols.get(symbol)) |owner_proc| {
                if (owner_proc != proc_id) {
                    return .{
                        .expr_id = expr_id,
                        .symbol = symbol,
                        .owner_proc = owner_proc,
                    };
                }
            }
            return null;
        },
        .list => |list| {
            for (mir_store.getExprSpan(list.elems)) |elem| {
                if (firstForeignParamLookup(mir_store, elem, proc_id, all_param_symbols)) |found| return found;
            }
            return null;
        },
        .struct_ => |struct_| {
            for (mir_store.getExprSpan(struct_.fields)) |field| {
                if (firstForeignParamLookup(mir_store, field, proc_id, all_param_symbols)) |found| return found;
            }
            return null;
        },
        .tag => |tag| {
            for (mir_store.getExprSpan(tag.args)) |arg| {
                if (firstForeignParamLookup(mir_store, arg, proc_id, all_param_symbols)) |found| return found;
            }
            return null;
        },
        .match_expr => |match_expr| {
            if (firstForeignParamLookup(mir_store, match_expr.cond, proc_id, all_param_symbols)) |found| return found;
            for (mir_store.getBranches(match_expr.branches)) |branch| {
                if (!branch.guard.isNone()) {
                    if (firstForeignParamLookup(mir_store, branch.guard, proc_id, all_param_symbols)) |found| return found;
                }
                if (firstForeignParamLookup(mir_store, branch.body, proc_id, all_param_symbols)) |found| return found;
            }
            return null;
        },
        .proc_ref,
        .runtime_err_can,
        .runtime_err_type,
        .runtime_err_ellipsis,
        .runtime_err_anno_only,
        .int,
        .frac_f32,
        .frac_f64,
        .dec,
        .str,
        .crash,
        .break_expr,
        => return null,
        .closure_make => |closure| return firstForeignParamLookup(mir_store, closure.captures, proc_id, all_param_symbols),
        .call => |call| {
            if (firstForeignParamLookup(mir_store, call.func, proc_id, all_param_symbols)) |found| return found;
            for (mir_store.getExprSpan(call.args)) |arg| {
                if (firstForeignParamLookup(mir_store, arg, proc_id, all_param_symbols)) |found| return found;
            }
            return null;
        },
        .block => |block| {
            for (mir_store.getStmts(block.stmts)) |stmt| {
                switch (stmt) {
                    .decl_const, .decl_var, .mutate_var => |binding| {
                        if (firstForeignParamLookup(mir_store, binding.expr, proc_id, all_param_symbols)) |found| return found;
                    },
                }
            }
            return firstForeignParamLookup(mir_store, block.final_expr, proc_id, all_param_symbols);
        },
        .borrow_scope => |borrow_scope| {
            for (mir_store.getBorrowBindings(borrow_scope.bindings)) |binding| {
                if (firstForeignParamLookup(mir_store, binding.expr, proc_id, all_param_symbols)) |found| return found;
            }
            return firstForeignParamLookup(mir_store, borrow_scope.body, proc_id, all_param_symbols);
        },
        .struct_access => |access| return firstForeignParamLookup(mir_store, access.struct_, proc_id, all_param_symbols),
        .str_escape_and_quote => |inner| return firstForeignParamLookup(mir_store, inner, proc_id, all_param_symbols),
        .run_low_level => |low_level| {
            for (mir_store.getExprSpan(low_level.args)) |arg| {
                if (firstForeignParamLookup(mir_store, arg, proc_id, all_param_symbols)) |found| return found;
            }
            return null;
        },
        .dbg_expr => |dbg_expr| return firstForeignParamLookup(mir_store, dbg_expr.expr, proc_id, all_param_symbols),
        .expect => |expect| return firstForeignParamLookup(mir_store, expect.body, proc_id, all_param_symbols),
        .for_loop => |for_loop| {
            if (firstForeignParamLookup(mir_store, for_loop.list, proc_id, all_param_symbols)) |found| return found;
            return firstForeignParamLookup(mir_store, for_loop.body, proc_id, all_param_symbols);
        },
        .while_loop => |while_loop| {
            if (firstForeignParamLookup(mir_store, while_loop.cond, proc_id, all_param_symbols)) |found| return found;
            return firstForeignParamLookup(mir_store, while_loop.body, proc_id, all_param_symbols);
        },
        .return_expr => |ret| return firstForeignParamLookup(mir_store, ret.expr, proc_id, all_param_symbols),
    }
}

// --- MIR Store tests ---

test "MIR Store: add and get expression" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const monotype = store.monotype_store.primIdx(.i64);
    const expr_id = try store.addExpr(test_allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, monotype, Region.zero());

    const retrieved = store.getExpr(expr_id);
    switch (retrieved) {
        .int => |int_val| try testing.expectEqual(@as(i128, 42), int_val.value.toI128()),
        else => return error.TestUnexpectedResult,
    }

    try testing.expectEqual(monotype, store.typeOf(expr_id));
}

test "MIR Store: add and get pattern" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const monotype = store.monotype_store.primIdx(.i64);
    const symbol = testSymbolFromIdent(Ident.Idx.NONE);
    const pat_id = try store.addPattern(test_allocator, .{ .bind = symbol }, monotype);

    const retrieved = store.getPattern(pat_id);
    switch (retrieved) {
        .bind => |sym| try testing.expect(!sym.isNone()),
        else => return error.TestUnexpectedResult,
    }

    try testing.expectEqual(monotype, store.patternTypeOf(pat_id));
}

test "MIR Store: expression spans" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const monotype = store.monotype_store.primIdx(.i64);
    const e1 = try store.addExpr(test_allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, monotype, Region.zero());
    const e2 = try store.addExpr(test_allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 2)), .kind = .i128 },
    } }, monotype, Region.zero());
    const e3 = try store.addExpr(test_allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 3)), .kind = .i128 },
    } }, monotype, Region.zero());

    const span = try store.addExprSpan(test_allocator, &.{ e1, e2, e3 });
    try testing.expectEqual(@as(u16, 3), span.len);

    const retrieved = store.getExprSpan(span);
    try testing.expectEqual(@as(usize, 3), retrieved.len);
    try testing.expectEqual(e1, retrieved[0]);
    try testing.expectEqual(e2, retrieved[1]);
    try testing.expectEqual(e3, retrieved[2]);
}

test "MIR Store: empty spans" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const empty_expr_span = MIR.ExprSpan.empty();
    try testing.expect(empty_expr_span.isEmpty());
    try testing.expectEqual(@as(usize, 0), store.getExprSpan(empty_expr_span).len);

    const empty_pat_span = MIR.PatternSpan.empty();
    try testing.expect(empty_pat_span.isEmpty());
    try testing.expectEqual(@as(usize, 0), store.getPatternSpan(empty_pat_span).len);
}

test "MIR Store: branches" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const monotype = store.monotype_store.primIdx(.i64);
    const body1 = try store.addExpr(test_allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 1)), .kind = .i128 },
    } }, monotype, Region.zero());
    const body2 = try store.addExpr(test_allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 2)), .kind = .i128 },
    } }, monotype, Region.zero());

    const pat1 = try store.addPattern(test_allocator, .wildcard, monotype);
    const pat2 = try store.addPattern(test_allocator, .wildcard, monotype);

    const bp1 = try store.addBranchPatterns(test_allocator, &.{.{ .pattern = pat1, .degenerate = false }});
    const bp2 = try store.addBranchPatterns(test_allocator, &.{.{ .pattern = pat2, .degenerate = false }});

    const branch_span = try store.addBranches(test_allocator, &.{
        .{ .patterns = bp1, .body = body1, .guard = MIR.ExprId.none },
        .{ .patterns = bp2, .body = body2, .guard = MIR.ExprId.none },
    });

    const branches = store.getBranches(branch_span);
    try testing.expectEqual(@as(usize, 2), branches.len);
    try testing.expectEqual(body1, branches[0].body);
    try testing.expectEqual(body2, branches[1].body);
}

test "MIR Store: statements" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const monotype = store.monotype_store.primIdx(.i64);
    const expr = try store.addExpr(test_allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, monotype, Region.zero());
    const symbol = testSymbolFromIdent(Ident.Idx.NONE);
    const pat = try store.addPattern(test_allocator, .{ .bind = symbol }, monotype);

    const stmt_span = try store.addStmts(test_allocator, &.{.{ .decl_const = .{ .pattern = pat, .expr = expr } }});
    const stmts = store.getStmts(stmt_span);
    try testing.expectEqual(@as(usize, 1), stmts.len);
    try testing.expectEqual(pat, stmts[0].decl_const.pattern);
    try testing.expectEqual(expr, stmts[0].decl_const.expr);
}

test "MIR Store: symbol def registration" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const monotype = store.monotype_store.primIdx(.i64);
    const expr_id = try store.addExpr(test_allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, monotype, Region.zero());
    const symbol = testSymbolFromIdent(Ident.Idx.NONE);

    try store.registerValueDef(test_allocator, symbol, expr_id);
    const result = store.getValueDef(symbol);
    try testing.expect(result != null);
    try testing.expectEqual(expr_id, result.?);
}

test "MIR Store: multiple expressions round trip" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const i64_type = store.monotype_store.primIdx(.i64);
    const str_type = store.monotype_store.primIdx(.str);
    const pattern_type = store.monotype_store.primIdx(.i64);

    // Add int
    const int_id = try store.addExpr(test_allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 99)), .kind = .i128 },
    } }, i64_type, Region.zero());

    // Add string
    // undefined is fine here: we're testing the store, not reading the string literal index
    const str_id = try store.addExpr(test_allocator, .{ .str = undefined }, str_type, Region.zero());

    // Add list with the int as element
    const list_span = try store.addExprSpan(test_allocator, &.{int_id});
    const list_id = try store.addExpr(test_allocator, .{ .list = .{ .elems = list_span } }, i64_type, Region.zero());

    // Add wildcard pattern
    const wild_id = try store.addPattern(test_allocator, .wildcard, pattern_type);

    // Verify types
    try testing.expectEqual(i64_type, store.typeOf(int_id));
    try testing.expectEqual(str_type, store.typeOf(str_id));
    try testing.expectEqual(i64_type, store.typeOf(list_id));
    try testing.expectEqual(pattern_type, store.patternTypeOf(wild_id));

    // Verify expressions
    switch (store.getExpr(int_id)) {
        .int => |v| try testing.expectEqual(@as(i128, 99), v.value.toI128()),
        else => return error.TestUnexpectedResult,
    }
    switch (store.getExpr(str_id)) {
        .str => {},
        else => return error.TestUnexpectedResult,
    }
    switch (store.getExpr(list_id)) {
        .list => |l| {
            try testing.expectEqual(@as(u16, 1), l.elems.len);
            const elems = store.getExprSpan(l.elems);
            try testing.expectEqual(int_id, elems[0]);
        },
        else => return error.TestUnexpectedResult,
    }
    switch (store.getPattern(wild_id)) {
        .wildcard => {},
        else => return error.TestUnexpectedResult,
    }
}

// --- Monotype Store tests ---

test "Monotype Store: primitive types" {
    var store = try Monotype.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    try testing.expectEqual(Monotype.Prim.str, store.getMonotype(store.primIdx(.str)).prim);
    try testing.expectEqual(Monotype.Prim.i64, store.getMonotype(store.primIdx(.i64)).prim);
}

test "Monotype Store: unit type" {
    var store = try Monotype.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    try testing.expect(store.getMonotype(store.unit_idx) == .unit);
}

test "Monotype Store: list type" {
    var store = try Monotype.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const elem = store.primIdx(.str);
    const list = try store.addMonotype(test_allocator, .{ .list = .{ .elem = elem } });

    const retrieved = store.getMonotype(list);
    try testing.expectEqual(elem, retrieved.list.elem);
}

test "Monotype Store: func type" {
    var store = try Monotype.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const arg1 = store.primIdx(.i64);
    const arg2 = store.primIdx(.str);
    const ret = store.primIdx(.i64);

    const args_span = try store.addIdxSpan(test_allocator, &.{ arg1, arg2 });
    const func = try store.addMonotype(test_allocator, .{ .func = .{
        .args = args_span,
        .ret = ret,
        .effectful = false,
    } });

    const retrieved = store.getMonotype(func);
    try testing.expectEqual(ret, retrieved.func.ret);
    try testing.expectEqual(false, retrieved.func.effectful);
}

test "Monotype Store: record type" {
    var store = try Monotype.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const field1_type = store.primIdx(.i64);
    const field2_type = store.primIdx(.str);

    const field_span = try store.addFields(test_allocator, &.{
        .{ .name = .{ .module_idx = 0, .ident = Ident.Idx.NONE }, .type_idx = field1_type },
        .{ .name = .{ .module_idx = 0, .ident = Ident.Idx.NONE }, .type_idx = field2_type },
    });
    const record = try store.addMonotype(test_allocator, .{ .record = .{ .fields = field_span } });

    const retrieved = store.getMonotype(record);
    try testing.expect(retrieved == .record);
}

test "Monotype Store: tag union type" {
    var store = try Monotype.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const payload_type = store.primIdx(.str);
    const payload_span = try store.addIdxSpan(test_allocator, &.{payload_type});

    const tag_span = try store.addTags(test_allocator, &.{
        .{ .name = .{ .module_idx = 0, .ident = Ident.Idx.NONE }, .payloads = payload_span },
    });
    const tag_union = try store.addMonotype(test_allocator, .{ .tag_union = .{ .tags = tag_span } });

    const retrieved = store.getMonotype(tag_union);
    try testing.expect(retrieved == .tag_union);
}

test "Monotype Store: box type" {
    var store = try Monotype.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const inner = store.primIdx(.i64);
    const boxed = try store.addMonotype(test_allocator, .{ .box = .{ .inner = inner } });

    const retrieved = store.getMonotype(boxed);
    try testing.expectEqual(inner, retrieved.box.inner);
}

test "Monotype Store: tuple type" {
    var store = try Monotype.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const elem1 = store.primIdx(.i64);
    const elem2 = store.primIdx(.str);

    const elems_span = try store.addIdxSpan(test_allocator, &.{ elem1, elem2 });
    const tuple = try store.addMonotype(test_allocator, .{ .tuple = .{ .elems = elems_span } });

    const retrieved = store.getMonotype(tuple);
    try testing.expect(retrieved == .tuple);
}

test "Monotype Store: all primitive types" {
    var store = try Monotype.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const prims = [_]Monotype.Prim{
        .str,
        .u8,
        .i8,
        .u16,
        .i16,
        .u32,
        .i32,
        .u64,
        .i64,
        .u128,
        .i128,
        .f32,
        .f64,
        .dec,
    };

    for (prims) |p| {
        const idx = store.primIdx(p);
        try testing.expectEqual(p, store.getMonotype(idx).prim);
    }
}

// --- Symbol tests ---

test "Symbol: equality and hashing" {
    const s1 = MIR.Symbol.fromRaw(1);
    const s2 = MIR.Symbol.fromRaw(1);
    const s3 = MIR.Symbol.fromRaw(2);

    try testing.expect(s1.eql(s2));
    try testing.expect(!s1.eql(s3));
}

test "Symbol: none sentinel" {
    const none = MIR.Symbol.none;
    try testing.expect(none.isNone());

    const some = MIR.Symbol.fromRaw(0);
    try testing.expect(!some.isNone());
}

// --- Lower init/deinit tests ---

test "Lower: init and deinit" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    var module_env = try test_allocator.create(ModuleEnv);
    module_env.* = try ModuleEnv.init(test_allocator, "test");
    defer {
        module_env.deinit();
        test_allocator.destroy(module_env);
    }

    const all_module_envs = [_]*ModuleEnv{module_env};

    var monomorphization = try Monomorphize.Result.init(test_allocator, 0, null);
    defer monomorphization.deinit(test_allocator);

    var lower = try Lower.init(
        test_allocator,
        &store,
        &monomorphization,
        @as([]const *ModuleEnv, &all_module_envs),
        &module_env.types,
        0,
        null,
    );
    defer lower.deinit();

    // Verify initial state
    try testing.expectEqual(@as(u32, 0), lower.current_module_idx);
}

// --- Integration tests: source → parse → canonicalize → type-check → MIR lower ---

test "lowerExpr: integer literal" {
    var env = try MirTestEnv.initExpr("42");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    try testing.expect(env.mir_store.getExpr(expr) == .int);
}

test "lowerExpr: float literal" {
    var env = try MirTestEnv.initExpr("3.14f64");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    try testing.expect(env.mir_store.getExpr(expr) == .frac_f64);
}

test "lowerExpr: string literal" {
    var env = try MirTestEnv.initExpr(
        \\"hello"
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    try testing.expect(env.mir_store.getExpr(expr) == .str);
}

test "lowerExpr: empty string" {
    var env = try MirTestEnv.initExpr(
        \\""
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .str);
    try testing.expect(!result.str.isNone());
}

test "lowerExpr: empty list" {
    var env = try MirTestEnv.initExpr("[]");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .list);
    try testing.expectEqual(@as(u16, 0), result.list.elems.len);
}

test "lowerExpr: list literal" {
    var env = try MirTestEnv.initExpr("[1, 2, 3]");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .list);
    try testing.expectEqual(@as(u16, 3), result.list.elems.len);
    // Each element should be an int
    const elems = env.mir_store.getExprSpan(result.list.elems);
    for (elems) |elem| {
        try testing.expect(env.mir_store.getExpr(elem) == .int);
    }
}

test "lowerExpr: tag" {
    var env = try MirTestEnv.initExpr("Ok");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .tag);
    try testing.expectEqual(@as(u16, 0), result.tag.args.len);
}

test "lowerExpr: if-else desugars to match" {
    var env = try MirTestEnv.initExpr("if True 1 else 2");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    try testing.expect(env.mir_store.getExpr(expr) == .match_expr);
}

test "lowerExpr: binop on defaulted numeral dispatches to method call" {
    var env = try MirTestEnv.initExpr("1 + 2");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // Numeric literals default to Dec, so binop dispatches to Dec.plus
    try testing.expect(result == .call);
}

test "lowerExpr: unary minus on defaulted numeral dispatches to method call" {
    // Use a block so that `-x` produces an e_unary_minus (bare `-1` is parsed as a negative literal)
    var env = try MirTestEnv.initExpr(
        \\{
        \\    x = 1
        \\    -x
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    // The block's final expression should be the lowered unary minus
    const block = env.mir_store.getExpr(expr).block;
    const result = env.mir_store.getExpr(block.final_expr);
    // Numeric literals default to Dec, so unary minus dispatches to Dec.negate
    try testing.expect(result == .call);
}

test "lowerExpr: block with decl_const" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    x = 1
        \\    x
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .block);
    try testing.expectEqual(@as(u16, 1), result.block.stmts.len);
    // The statement should be a decl_const
    const stmts = env.mir_store.getStmts(result.block.stmts);
    try testing.expect(stmts[0] == .decl_const);
    // The final expression should be a lookup
    try testing.expect(env.mir_store.getExpr(result.block.final_expr) == .lookup);
}

test "lowerExpr: block local closure call has resolvable symbol def and lambda set" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    x = 10.I64
        \\    f = |y| x + y
        \\    f(5.I64)
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const block = env.mir_store.getExpr(expr);
    try testing.expect(block == .block);

    var final_expr_id = block.block.final_expr;
    while (env.mir_store.getExpr(final_expr_id) == .block) {
        final_expr_id = env.mir_store.getExpr(final_expr_id).block.final_expr;
    }
    const final_expr = env.mir_store.getExpr(final_expr_id);
    try testing.expect(final_expr == .call);

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    const callee_ls = ls_store.getExprLambdaSet(final_expr.call.func) orelse return error.TestUnexpectedResult;
    const members = ls_store.getMembers(ls_store.getLambdaSet(callee_ls).members);
    try testing.expectEqual(@as(usize, 1), members.len);
    try testing.expect(!members[0].proc.isNone());
    const closure_member = env.mir_store.getClosureMember(members[0].closure_member);
    try testing.expectEqual(@as(usize, 1), env.mir_store.getCaptureBindings(closure_member.capture_bindings).len);
}

test "lambda set: closure-returning binding keeps resolvable lifted member" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5.I64)
        \\    add5(10.I64)
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const block = env.mir_store.getExpr(expr);
    try testing.expect(block == .block);

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    var final_expr_id = block.block.final_expr;
    while (env.mir_store.getExpr(final_expr_id) == .block) {
        final_expr_id = env.mir_store.getExpr(final_expr_id).block.final_expr;
    }
    const final_expr = env.mir_store.getExpr(final_expr_id);
    try testing.expect(final_expr == .call);

    const callee_ls = ls_store.getExprLambdaSet(final_expr.call.func) orelse return error.TestUnexpectedResult;

    const callee_members = ls_store.getMembers(ls_store.getLambdaSet(callee_ls).members);
    try testing.expectEqual(@as(usize, 1), callee_members.len);
    try testing.expect(!callee_members[0].proc.isNone());
    const returned_closure_member = env.mir_store.getClosureMember(callee_members[0].closure_member);
    try testing.expectEqual(@as(usize, 1), env.mir_store.getCaptureBindings(returned_closure_member.capture_bindings).len);
}

test "lambda set: higher-order closure param propagation keeps member defs stable" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    wrap = |f| |x| f(x)
        \\    y = 10.I64
        \\    add_y = |x| x + y
        \\    wrap(add_y)(5.I64)
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const block = env.mir_store.getExpr(expr);
    try testing.expect(block == .block);

    var final_expr_id = block.block.final_expr;
    while (env.mir_store.getExpr(final_expr_id) == .block) {
        final_expr_id = env.mir_store.getExpr(final_expr_id).block.final_expr;
    }
    const final_expr = env.mir_store.getExpr(final_expr_id);
    try testing.expect(final_expr == .call);

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    const callee_ls = ls_store.getExprLambdaSet(final_expr.call.func) orelse return error.TestUnexpectedResult;

    const members = ls_store.getMembers(ls_store.getLambdaSet(callee_ls).members);
    try testing.expectEqual(@as(usize, 1), members.len);
    try testing.expect(!members[0].proc.isNone());
}

test "lambda set: exact closure factory program keeps symbol defs stable" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    a = add5(10)
        \\    b = add5(20)
        \\    a + b
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const block = env.mir_store.getExpr(expr);
    try testing.expect(block == .block);

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    try testing.expect(ls_store.lambda_sets.items.len > 0);
    try testing.expect(ls_store.members.items.len > 0);
    try testing.expect(!ls_store.members.items[0].proc.isNone());
}

test "lambda set: factory-produced closure alias keeps captured symbol lambda set" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    double_add5 = |x| add5(x) * 2
        \\    double_add5(10)
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const block = env.mir_store.getExpr(expr);
    try testing.expect(block == .block);

    const stmts = env.mir_store.getStmts(block.block.stmts);
    var add5_sym: ?MIR.Symbol = null;
    for (stmts) |stmt| {
        if (stmt != .decl_const) continue;
        const stmt_expr = env.mir_store.getExpr(stmt.decl_const.expr);
        if (stmt_expr != .call) continue;
        const pat = env.mir_store.getPattern(stmt.decl_const.pattern);
        if (pat != .bind) continue;
        add5_sym = pat.bind;
        break;
    }
    const resolved_add5_sym = add5_sym orelse return error.TestUnexpectedResult;

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    const add5_ls = ls_store.getSymbolLambdaSet(resolved_add5_sym) orelse return error.TestUnexpectedResult;
    const add5_members = ls_store.getMembers(ls_store.getLambdaSet(add5_ls).members);
    try testing.expectEqual(@as(usize, 1), add5_members.len);

    var found_capture_lookup = false;
    for (env.mir_store.closure_members.items) |closure_member| {
        const capture_bindings = env.mir_store.getCaptureBindings(closure_member.capture_bindings);
        for (capture_bindings) |binding| {
            const source_expr = env.mir_store.getExpr(binding.source_expr);
            if (source_expr != .lookup or !source_expr.lookup.eql(resolved_add5_sym)) continue;
            found_capture_lookup = true;
            const capture_ls = ls_store.getSymbolLambdaSet(binding.local_symbol) orelse return error.TestUnexpectedResult;
            try testing.expectEqual(add5_ls, capture_ls);
        }
    }
    try testing.expect(found_capture_lookup);
}

test "lambda set: closure extracted from record field keeps member defs stable" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    y = 10
        \\    rec = { f: |x| x + y }
        \\    f = rec.f
        \\    f(5)
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const block = env.mir_store.getExpr(expr);
    try testing.expect(block == .block);

    const stmts = env.mir_store.getStmts(block.block.stmts);
    var extracted_sym: ?MIR.Symbol = null;
    for (stmts) |stmt| {
        if (stmt != .decl_const) continue;
        const stmt_expr = env.mir_store.getExpr(stmt.decl_const.expr);
        if (stmt_expr != .struct_access) continue;
        const pat = env.mir_store.getPattern(stmt.decl_const.pattern);
        if (pat != .bind) continue;
        extracted_sym = pat.bind;
        break;
    }
    const resolved_f_sym = extracted_sym orelse return error.TestUnexpectedResult;

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    const f_ls = ls_store.getSymbolLambdaSet(resolved_f_sym) orelse return error.TestUnexpectedResult;
    const members = ls_store.getMembers(ls_store.getLambdaSet(f_ls).members);
    try testing.expectEqual(@as(usize, 1), members.len);
    try testing.expect(!members[0].proc.isNone());
    const closure_member = env.mir_store.getClosureMember(members[0].closure_member);
    try testing.expectEqual(@as(usize, 1), env.mir_store.getCaptureBindings(closure_member.capture_bindings).len);
}

test "lambda set: plain lambda extracted from record field gets symbol identity" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    rec = { f: |x| x + 1 }
        \\    f = rec.f
        \\    f(5)
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const block = env.mir_store.getExpr(expr);
    try testing.expect(block == .block);

    const stmts = env.mir_store.getStmts(block.block.stmts);
    var extracted_sym: ?MIR.Symbol = null;
    for (stmts) |stmt| {
        if (stmt != .decl_const) continue;
        const stmt_expr = env.mir_store.getExpr(stmt.decl_const.expr);
        if (stmt_expr != .struct_access) continue;
        const pat = env.mir_store.getPattern(stmt.decl_const.pattern);
        if (pat != .bind) continue;
        extracted_sym = pat.bind;
        break;
    }
    const resolved_f_sym = extracted_sym orelse return error.TestUnexpectedResult;

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    const f_ls = ls_store.getSymbolLambdaSet(resolved_f_sym) orelse return error.TestUnexpectedResult;
    const members = ls_store.getMembers(ls_store.getLambdaSet(f_ls).members);
    try testing.expectEqual(@as(usize, 1), members.len);
    try testing.expect(members[0].closure_member.isNone());
    try testing.expect(!members[0].proc.isNone());
    try testing.expect(env.mir_store.getProc(members[0].proc).capture_bindings.isEmpty());
}

test "lambda set: plain lambda extracted from tuple field gets symbol identity" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    tup = (|x| x + 1, 42)
        \\    f = tup.0
        \\    f(5)
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const block = env.mir_store.getExpr(expr);
    try testing.expect(block == .block);

    const stmts = env.mir_store.getStmts(block.block.stmts);
    var extracted_sym: ?MIR.Symbol = null;
    for (stmts) |stmt| {
        if (stmt != .decl_const) continue;
        const stmt_expr = env.mir_store.getExpr(stmt.decl_const.expr);
        if (stmt_expr != .struct_access) continue;
        const pat = env.mir_store.getPattern(stmt.decl_const.pattern);
        if (pat != .bind) continue;
        extracted_sym = pat.bind;
        break;
    }
    const resolved_f_sym = extracted_sym orelse return error.TestUnexpectedResult;

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    const f_ls = ls_store.getSymbolLambdaSet(resolved_f_sym) orelse return error.TestUnexpectedResult;
    const members = ls_store.getMembers(ls_store.getLambdaSet(f_ls).members);
    try testing.expectEqual(@as(usize, 1), members.len);
    try testing.expect(members[0].closure_member.isNone());
    try testing.expect(!members[0].proc.isNone());
    try testing.expect(env.mir_store.getProc(members[0].proc).capture_bindings.isEmpty());
}

test "lambda set: plain lambda extracted from tag payload gets symbol identity" {
    var env = try MirTestEnv.initExpr(
        \\match Ok(|x| x + 1) { Ok(f) => f(5), Err(_) => 0 }
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .match_expr);

    const branches = env.mir_store.getBranches(result.match_expr.branches);
    var extracted_sym: ?MIR.Symbol = null;
    for (branches) |branch| {
        const branch_patterns = env.mir_store.getBranchPatterns(branch.patterns);
        for (branch_patterns) |branch_pattern| {
            const pat = env.mir_store.getPattern(branch_pattern.pattern);
            if (pat != .tag) continue;
            const args = env.mir_store.getPatternSpan(pat.tag.args);
            if (args.len != 1) continue;
            const arg_pat = env.mir_store.getPattern(args[0]);
            if (arg_pat != .bind) continue;
            extracted_sym = arg_pat.bind;
            break;
        }
        if (extracted_sym != null) break;
    }
    const resolved_f_sym = extracted_sym orelse return error.TestUnexpectedResult;

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    const f_ls = ls_store.getSymbolLambdaSet(resolved_f_sym) orelse return error.TestUnexpectedResult;
    const members = ls_store.getMembers(ls_store.getLambdaSet(f_ls).members);
    try testing.expectEqual(@as(usize, 1), members.len);
    try testing.expect(members[0].closure_member.isNone());
    try testing.expect(!members[0].proc.isNone());
    try testing.expect(env.mir_store.getProc(members[0].proc).capture_bindings.isEmpty());
}

test "lambda set: plain lambda extracted from list element gets symbol identity" {
    var env = try MirTestEnv.initExpr(
        \\match [|x| x + 1] { [f] => f(5), _ => 0 }
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .match_expr);

    const branches = env.mir_store.getBranches(result.match_expr.branches);
    var extracted_sym: ?MIR.Symbol = null;
    for (branches) |branch| {
        const branch_patterns = env.mir_store.getBranchPatterns(branch.patterns);
        for (branch_patterns) |branch_pattern| {
            const pat = env.mir_store.getPattern(branch_pattern.pattern);
            if (pat != .list_destructure) continue;
            const elems = env.mir_store.getPatternSpan(pat.list_destructure.patterns);
            if (elems.len != 1) continue;
            const elem_pat = env.mir_store.getPattern(elems[0]);
            if (elem_pat != .bind) continue;
            extracted_sym = elem_pat.bind;
            break;
        }
        if (extracted_sym != null) break;
    }
    const resolved_f_sym = extracted_sym orelse return error.TestUnexpectedResult;

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    const f_ls = ls_store.getSymbolLambdaSet(resolved_f_sym) orelse return error.TestUnexpectedResult;
    const members = ls_store.getMembers(ls_store.getLambdaSet(f_ls).members);
    try testing.expectEqual(@as(usize, 1), members.len);
    try testing.expect(members[0].closure_member.isNone());
    try testing.expect(!members[0].proc.isNone());
    try testing.expect(env.mir_store.getProc(members[0].proc).capture_bindings.isEmpty());
}

test "lambda set: higher-order param receives both closure members" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    apply = |f, x| f(x)
        \\    a = 10
        \\    b = 20
        \\    r1 = apply(|x| x + a, 5)
        \\    r2 = apply(|x| x + b, 5)
        \\    r1 + r2
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const block = env.mir_store.getExpr(expr);
    try testing.expect(block == .block);

    const stmts = env.mir_store.getStmts(block.block.stmts);
    const apply_proc_id = firstCalledProcInStmts(env.mir_store, stmts) orelse return error.TestUnexpectedResult;
    const apply_proc = env.mir_store.getProc(apply_proc_id);
    const apply_params = env.mir_store.getPatternSpan(apply_proc.params);
    try testing.expectEqual(@as(usize, 2), apply_params.len);
    const f_pat = env.mir_store.getPattern(apply_params[0]);
    try testing.expect(f_pat == .bind);
    const f_sym = f_pat.bind;

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    const f_ls = ls_store.getSymbolLambdaSet(f_sym) orelse return error.TestUnexpectedResult;
    const members = ls_store.getMembers(ls_store.getLambdaSet(f_ls).members);
    try testing.expectEqual(@as(usize, 2), members.len);
    for (members) |member| {
        const closure_member = env.mir_store.getClosureMember(member.closure_member);
        try testing.expectEqual(@as(usize, 1), env.mir_store.getCaptureBindings(closure_member.capture_bindings).len);
    }
}

test "lambda set: higher-order closure captures monotype stays numeric tuple" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    apply = |f, x| f(x)
        \\    a = 10
        \\    b = 20
        \\    r1 = apply(|x| x + a, 5)
        \\    r2 = apply(|x| x + b, 5)
        \\    r1 + r2
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    const block = env.mir_store.getExpr(expr);
    try testing.expect(block == .block);

    const stmts = env.mir_store.getStmts(block.block.stmts);
    const apply_proc_id = firstCalledProcInStmts(env.mir_store, stmts) orelse return error.TestUnexpectedResult;
    const apply_proc = env.mir_store.getProc(apply_proc_id);
    const apply_params = env.mir_store.getPatternSpan(apply_proc.params);
    const f_pat = env.mir_store.getPattern(apply_params[0]);
    try testing.expect(f_pat == .bind);
    const f_sym = f_pat.bind;

    const f_ls = ls_store.getSymbolLambdaSet(f_sym) orelse return error.TestUnexpectedResult;
    const members = ls_store.getMembers(ls_store.getLambdaSet(f_ls).members);
    try testing.expectEqual(@as(usize, 2), members.len);

    const closure_member = env.mir_store.getClosureMember(members[0].closure_member);
    const capture_bindings = env.mir_store.getCaptureBindings(closure_member.capture_bindings);
    try testing.expectEqual(@as(usize, 1), capture_bindings.len);
    const elem_mono = env.mir_store.monotype_store.getMonotype(capture_bindings[0].monotype);
    try testing.expect(elem_mono == .prim);
    try testing.expectEqual(Monotype.Prim.dec, elem_mono.prim);
}

test "lambda set: imported List.any receives predicate lambda set" {
    var env = try MirTestEnv.initExpr("List.any([1.I64, 2.I64, 3.I64], |_x| True)");
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const root_expr = env.mir_store.getExpr(expr);
    try testing.expect(root_expr == .call);

    const root_args = env.mir_store.getExprSpan(root_expr.call.args);
    try testing.expectEqual(@as(usize, 2), root_args.len);

    const any_proc_id = procIdFromCallableExpr(env.mir_store, root_expr.call.func) orelse return error.TestUnexpectedResult;
    const any_proc = env.mir_store.getProc(any_proc_id);
    const params = any_proc.params;
    const lambda_body = any_proc.body;

    const param_ids = env.mir_store.getPatternSpan(params);
    try testing.expectEqual(@as(usize, 2), param_ids.len);

    const list_param_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.patternTypeOf(param_ids[0]));
    try testing.expect(list_param_mono == .list);
    const list_elem_mono = env.mir_store.monotype_store.getMonotype(list_param_mono.list.elem);
    try testing.expect(list_elem_mono == .prim);
    try testing.expectEqual(Monotype.Prim.i64, list_elem_mono.prim);

    const predicate_param_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.patternTypeOf(param_ids[1]));
    try testing.expect(predicate_param_mono == .func);
    const predicate_arg_monos = env.mir_store.monotype_store.getIdxSpan(predicate_param_mono.func.args);
    try testing.expectEqual(@as(usize, 1), predicate_arg_monos.len);
    const predicate_arg_mono = env.mir_store.monotype_store.getMonotype(predicate_arg_monos[0]);
    try testing.expect(predicate_arg_mono == .prim);
    try testing.expectEqual(Monotype.Prim.i64, predicate_arg_mono.prim);
    const predicate_ret_mono = env.mir_store.monotype_store.getMonotype(predicate_param_mono.func.ret);
    try testing.expect(predicate_ret_mono == .tag_union);

    const predicate_pat = env.mir_store.getPattern(param_ids[1]);
    try testing.expect(predicate_pat == .bind);
    const predicate_sym = predicate_pat.bind;

    var body_expr = lambda_body;
    var loop_item_pat: ?MIR.PatternId = null;
    while (true) {
        switch (env.mir_store.getExpr(body_expr)) {
            .block => |block| {
                const stmts = env.mir_store.getStmts(block.stmts);
                if (stmts.len > 0) {
                    const stmt_expr_id = switch (stmts[0]) {
                        .decl_const => |binding| binding.expr,
                        .decl_var => |binding| binding.expr,
                        .mutate_var => |binding| binding.expr,
                    };
                    const stmt_expr = env.mir_store.getExpr(stmt_expr_id);
                    if (stmt_expr == .for_loop) {
                        loop_item_pat = stmt_expr.for_loop.elem_pattern;
                        break;
                    }
                }
                body_expr = block.final_expr;
            },
            .for_loop => |loop| {
                loop_item_pat = loop.elem_pattern;
                break;
            },
            else => return error.TestUnexpectedResult,
        }
    }

    const loop_item_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.patternTypeOf(loop_item_pat.?));
    try testing.expect(loop_item_mono == .prim);
    try testing.expectEqual(Monotype.Prim.i64, loop_item_mono.prim);

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    const arg_ls = ls_store.getExprLambdaSet(root_args[1]) orelse return error.TestUnexpectedResult;
    const arg_members = ls_store.getMembers(ls_store.getLambdaSet(arg_ls).members);
    try testing.expectEqual(@as(usize, 1), arg_members.len);

    const predicate_ls = ls_store.getSymbolLambdaSet(predicate_sym) orelse return error.TestUnexpectedResult;
    const members = ls_store.getMembers(ls_store.getLambdaSet(predicate_ls).members);
    try testing.expectEqual(@as(usize, 1), members.len);
    try testing.expectEqual(arg_members[0].proc, members[0].proc);
    try testing.expect(!members[0].proc.isNone());
    try testing.expect(members[0].closure_member.isNone());
}

test "lowerExpr: lambda" {
    var env = try MirTestEnv.initFull("Test",
        \\main : U64 -> U64
        \\main = |x| x
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .proc_ref);
    try testing.expectEqual(@as(u16, 1), env.mir_store.getProc(result.proc_ref).params.len);
}

test "lowerExpr: Bool.and short-circuit desugars to match" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    x = True
        \\    y = False
        \\    x and y
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // The block's final expression (x and y) should desugar to a match
    try testing.expect(result == .block);
    try testing.expect(env.mir_store.getExpr(result.block.final_expr) == .match_expr);
}

// --- Gap #23: fromTypeVar monotype resolution tests ---

test "fromTypeVar: int with suffix resolves to prim i64" {
    var env = try MirTestEnv.initExpr("42.I64");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.i64, monotype.prim);
}

test "fromTypeVar: string resolves to valid monotype" {
    var env = try MirTestEnv.initExpr(
        \\"hello"
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    // The expression itself is a string
    try testing.expect(env.mir_store.getExpr(expr) == .str);
    // The monotype should be resolved (not unit placeholder)
    const type_idx = env.mir_store.typeOf(expr);
    try testing.expect(!type_idx.isNone());
}

test "fromTypeVar: list resolves to list monotype" {
    var env = try MirTestEnv.initExpr("[1.I64, 2.I64, 3.I64]");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    // The expression itself is a list
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .list);
    try testing.expectEqual(@as(u16, 3), result.list.elems.len);
    // The monotype must be .list (not .tag_union or anything else).
    // This catches the cross-module ident mismatch bug where
    // fromNominalType fails to recognize List because the Builtin
    // module's Ident.Idx for "List" differs from the current module's.
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .list);
}

test "fromTypeVar: record resolves to record with fields" {
    var env = try MirTestEnv.initExpr("{ x: 1, y: 2 }");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .record);
    try testing.expectEqual(@as(u16, 2), monotype.record.fields.len);
}

test "fromTypeVar: lambda resolves to func type" {
    var env = try MirTestEnv.initFull("Test",
        \\main : U64 -> U64
        \\main = |x| x
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .func);
    try testing.expectEqual(@as(u16, 1), monotype.func.args.len);
}

test "fromTypeVar: tag resolves to tag_union" {
    var env = try MirTestEnv.initExpr("Ok(42)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "fromTypeVar: tuple resolves to tuple type" {
    var env = try MirTestEnv.initExpr(
        \\(1, "hello")
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tuple);
    try testing.expectEqual(@as(u16, 2), monotype.tuple.elems.len);
}

// --- Gap #25: Recursive types in fromTypeVar ---

test "fromTypeVar: recursive linked list type completes without hanging" {
    var env = try MirTestEnv.initModule("ConsList",
        \\ConsList := [Nil, Cons(U64, ConsList)]
        \\
        \\x : ConsList
        \\x = ConsList.Cons(1, ConsList.Nil)
    );
    defer env.deinit();
    const expr = try env.lowerNamedDef("x");
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .tag);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "fromTypeVar: recursive binary tree type completes without hanging" {
    var env = try MirTestEnv.initModule("Tree",
        \\Tree := [Empty, Node({ value: U64, left: Tree, right: Tree })]
        \\
        \\x : Tree
        \\x = Tree.Empty
    );
    defer env.deinit();
    const expr = try env.lowerNamedDef("x");
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .tag);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "fromTypeVar: polymorphic opaque with function field propagates nominal args into backing type" {
    var env = try MirTestEnv.initModule("Test",
        \\W(a) := { f : {} -> [V(a)] }.{
        \\    mk : a -> W(a)
        \\    mk = |val| { f: |_| V(val) }
        \\}
        \\
        \\w : W(Str)
        \\w = W.mk("x")
    );
    defer env.deinit();

    const expr = try env.lowerNamedDef("w");
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));

    try testing.expect(monotype == .record);

    const fields = env.mir_store.monotype_store.getFields(monotype.record.fields);
    try testing.expectEqual(@as(usize, 1), fields.len);

    const field_type = env.mir_store.monotype_store.getMonotype(fields[0].type_idx);
    try testing.expect(field_type == .func);

    const ret_type = env.mir_store.monotype_store.getMonotype(field_type.func.ret);
    try testing.expect(ret_type == .tag_union);

    const tags = env.mir_store.monotype_store.getTags(ret_type.tag_union.tags);
    try testing.expectEqual(@as(usize, 1), tags.len);
    try testing.expectEqual(@as(usize, 1), tags[0].payloads.len);

    const payloads = env.mir_store.monotype_store.getIdxSpan(tags[0].payloads);
    const payload_type = env.mir_store.monotype_store.getMonotype(payloads[0]);
    try testing.expect(payload_type == .prim);
    try testing.expectEqual(Monotype.Prim.str, payload_type.prim);
}

test "lambda set: opaque function field call through param gets field lambda set" {
    var env = try MirTestEnv.initModule("Test",
        \\W(a) := { f : {} -> [V(a)] }.{
        \\    run : W(a) -> [V(a)]
        \\    run = |w| (w.f)({})
        \\
        \\    mk : a -> W(a)
        \\    mk = |val| { f: |_| V(val) }
        \\}
        \\
        \\result = W.run(W.mk("x"))
    );
    defer env.deinit();

    _ = try env.lowerNamedDef("result");

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    var saw_field_call = false;
    var saw_field_call_with_lambda_set = false;
    var saw_param_field_lambda_set = false;

    var expr_index: u32 = 0;
    while (expr_index < env.mir_store.exprs.items.len) : (expr_index += 1) {
        const expr_id: MIR.ExprId = @enumFromInt(expr_index);
        const expr = env.mir_store.getExpr(expr_id);
        if (expr != .call) continue;
        const func_expr = env.mir_store.getExpr(expr.call.func);

        var field_access_expr_id = expr.call.func;
        var field_access_expr = func_expr;

        if (field_access_expr == .lookup) {
            const def_expr_id = env.mir_store.getValueDef(field_access_expr.lookup) orelse continue;
            const def_expr = env.mir_store.getExpr(def_expr_id);
            if (def_expr != .struct_access) continue;
            field_access_expr_id = def_expr_id;
            field_access_expr = def_expr;
        } else if (field_access_expr != .struct_access) {
            continue;
        }

        const access = field_access_expr.struct_access;
        const struct_expr = env.mir_store.getExpr(access.struct_);
        if (struct_expr != .lookup) continue;

        saw_field_call = true;
        if (ls_store.getExprLambdaSet(field_access_expr_id) != null) {
            saw_field_call_with_lambda_set = true;
        }
        if (ls_store.getSymbolFieldLambdaSet(struct_expr.lookup, access.field_idx) != null) {
            saw_param_field_lambda_set = true;
        }
    }

    try testing.expect(saw_field_call);
    try testing.expect(saw_field_call_with_lambda_set);
    try testing.expect(saw_param_field_lambda_set);
}

// --- Gap #26: lowerExternalDef recursion guard ---

test "lowerExternalDef: recursion guard returns lookup placeholder" {
    var env = try MirTestEnv.initModule("Test",
        \\my_val = 42
    );
    defer env.deinit();
    const def_info = try env.getDefExprByName("my_val");
    const symbol = try env.lower.makeSymbol(1, def_info.ident_idx);

    // Manually insert the symbol into in_progress_defs to simulate recursion
    const symbol_key: u64 = @bitCast(symbol);
    try env.lower.in_progress_defs.put(symbol_key, {});

    const result = try env.lower.lowerExternalDef(symbol, def_info.expr_idx);
    const expr = env.mir_store.getExpr(result);
    // The recursion guard should return a lookup placeholder
    try testing.expect(expr == .lookup);
    // The recursion placeholder should preserve a concrete monotype, not unit,
    // so downstream lowering/codegen never sees a fake unit function type.
    try testing.expect(env.mir_store.typeOf(result) != env.mir_store.monotype_store.unit_idx);
}

test "lowerExternalDef: caching returns same ExprId on second call" {
    var env = try MirTestEnv.initModule("Test",
        \\my_val = 42
    );
    defer env.deinit();
    const def_info = try env.getDefExprByName("my_val");
    const symbol = try env.lower.makeSymbol(1, def_info.ident_idx);

    const first = try env.lower.lowerExternalDef(symbol, def_info.expr_idx);
    const second = try env.lower.lowerExternalDef(symbol, def_info.expr_idx);
    try testing.expectEqual(first, second);
}

// --- Gap #24: Cross-module MIR lowering ---

test "cross-module: type module import lowers tag constructor" {
    // Module A defines a nominal type
    var env_a = try MirTestEnv.initModule("A",
        \\A := [A(U64)].{
        \\  get_value : A -> U64
        \\  get_value = |A.A(val)| val
        \\}
    );
    defer env_a.deinit();

    // Module B imports A and constructs a value
    var env_b = try MirTestEnv.initWithImport("B",
        \\import A
        \\
        \\main = A.A(42)
    , "A", &env_a);
    defer env_b.deinit();

    const expr = try env_b.lowerFirstDef();
    // The expression should lower successfully (not be a runtime error)
    const result = env_b.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: type module method call lowers successfully" {
    // Module A defines a nominal type with a method
    var env_a = try MirTestEnv.initModule("A",
        \\A := [A(U64)].{
        \\  get_value : A -> U64
        \\  get_value = |A.A(val)| val
        \\}
    );
    defer env_a.deinit();

    // Module B imports A and calls a method
    var env_b = try MirTestEnv.initWithImport("B",
        \\import A
        \\
        \\main = A.get_value(A.A(42))
    , "A", &env_a);
    defer env_b.deinit();

    const expr = try env_b.lowerFirstDef();
    // The expression should lower successfully (not be a runtime error)
    const result = env_b.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: imported value call uses target def identity" {
    var env_color = try MirTestEnv.initModule("Color",
        \\Color := [Red, Green, Blue].{
        \\    red : Color
        \\    red = Red
        \\
        \\    green : Color
        \\    green = Green
        \\
        \\    blue : Color
        \\    blue = Blue
        \\
        \\    to_str : Color -> Str
        \\    to_str = |color|
        \\        match color {
        \\            Red => "red"
        \\            Green => "green"
        \\            Blue => "blue"
        \\        }
        \\}
    );
    defer env_color.deinit();

    var env_app = try MirTestEnv.initWithImport("App",
        \\import Color
        \\
        \\main = Color.to_str(Color.red)
    , "Color", &env_color);
    defer env_app.deinit();

    const expr = try env_app.lowerFirstDef();
    const result = env_app.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);

    try testing.expect(result == .call);
    const proc_id = procIdFromCallableExpr(env_app.mir_store, result.call.func) orelse return error.TestUnexpectedResult;
    try testing.expect(!proc_id.isNone());
}

test "cross-module: imported top-level value call uses target def identity" {
    var env_color = try MirTestEnv.initModule("Color",
        \\module [Color, red, green, blue, to_str]
        \\
        \\Color : [Red, Green, Blue]
        \\
        \\red : Color
        \\red = Red
        \\
        \\green : Color
        \\green = Green
        \\
        \\blue : Color
        \\blue = Blue
        \\
        \\to_str : Color -> Str
        \\to_str = |color|
        \\    match color {
        \\        Red => "red"
        \\        Green => "green"
        \\        Blue => "blue"
        \\    }
    );
    defer env_color.deinit();

    var env_app = try MirTestEnv.initWithImport("App",
        \\import Color
        \\
        \\main = Color.to_str(Color.red)
    , "Color", &env_color);
    defer env_app.deinit();

    const expr = try env_app.lowerFirstDef();
    const result = env_app.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);

    try testing.expect(result == .call);
    const proc_id = procIdFromCallableExpr(env_app.mir_store, result.call.func) orelse return error.TestUnexpectedResult;
    try testing.expect(!proc_id.isNone());
}

// --- Additional Lower code path tests ---

test "lowerExpr: unary not desugars to match" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = !True
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    // !True desugars via negBool to: match True { True => False, _ => True }
    try testing.expect(env.mir_store.getExpr(expr) == .match_expr);
}

test "lowerExpr: Bool.or short-circuit desugars to match" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    x = True
        \\    y = False
        \\    x or y
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .block);
    try testing.expect(env.mir_store.getExpr(result.block.final_expr) == .match_expr);
}

test "lowerExpr: != desugars through negBool to match" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    x = 1
        \\    y = 2
        \\    x != y
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .block);
    try testing.expect(env.mir_store.getExpr(result.block.final_expr) == .match_expr);
}

test "lowerExpr: for loop" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    var $x = 0.I64
        \\    for item in [1.I64, 2.I64, 3.I64] {
        \\        $x = item
        \\    }
        \\    $x
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .block);
    // The block should contain a for_loop in its statements
    // (expression stmts are lowered as `_ = expr`, i.e. decl_const with wildcard)
    const stmts = env.mir_store.getStmts(result.block.stmts);
    var found_for = false;
    for (stmts) |stmt| {
        switch (stmt) {
            .decl_const => |dc| {
                if (env.mir_store.getExpr(dc.expr) == .for_loop) found_for = true;
            },
            else => {},
        }
    }
    try testing.expect(found_for);
}

test "lowerExpr: multi-segment string interpolation produces str_concat" {
    var env = try MirTestEnv.initFull("Test",
        \\main = {
        \\    x = "world"
        \\    "hello ${x}!"
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .block);
    // The final expression should be a run_low_level(str_concat, ...) from the left-fold
    try testing.expect(env.mir_store.getExpr(result.block.final_expr) == .run_low_level);
}

test "lowerExpr: record destructure pattern in match" {
    var env = try MirTestEnv.initExpr(
        \\match { x: 1, y: 2 } { { x, y } => x, _ => 0 }
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    try testing.expect(env.mir_store.getExpr(expr) == .match_expr);
}

test "lowerExpr: tuple destructure pattern in match" {
    var env = try MirTestEnv.initExpr(
        \\match (1, 2) { (a, b) => a, _ => 0 }
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    try testing.expect(env.mir_store.getExpr(expr) == .match_expr);
}

test "lowerExpr: list destructure pattern in match" {
    var env = try MirTestEnv.initExpr(
        \\match [1, 2, 3] { [a, b, c] => a, _ => 0 }
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    try testing.expect(env.mir_store.getExpr(expr) == .match_expr);
}

test "lowerExpr: match with pattern alternatives preserves all patterns" {
    var env = try MirTestEnv.initExpr(
        \\match Ok(1) { Ok(x) | Err(x) => x, _ => 0 }
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .match_expr);

    // The first branch should have 2 patterns (Ok(x) and Err(x))
    const branches = env.mir_store.getBranches(result.match_expr.branches);
    try testing.expect(branches.len >= 1);
    const first_branch_patterns = env.mir_store.getBranchPatterns(branches[0].patterns);
    try testing.expect(first_branch_patterns.len >= 2);
}

test "lowerExpr: tuple access" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    t = (1, 2)
        \\    t.0
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .block);
    try testing.expect(env.mir_store.getExpr(result.block.final_expr) == .struct_access);
}

test "lowerExpr: typed F64 fractional via dot syntax" {
    var env = try MirTestEnv.initExpr("3.14.F64");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    try testing.expect(env.mir_store.getExpr(expr) == .frac_f64);
}

test "cross-module: dot-access method call ensures method is lowered" {
    // Module A defines a nominal type with a method
    var env_a = try MirTestEnv.initModule("A",
        \\A := [A(U64)].{
        \\  get_value : A -> U64
        \\  get_value = |A.A(val)| val
        \\}
    );
    defer env_a.deinit();

    // Module B imports A and calls a method via dot-access syntax
    // This exercises lowerDotAccess (not lowerBinop) which was missing specializeMethod
    var env_b = try MirTestEnv.initWithImport("B",
        \\import A
        \\
        \\main = A.A(42).get_value()
    , "A", &env_a);
    defer env_b.deinit();

    const expr = try env_b.lowerFirstDef();
    const result = env_b.mir_store.getExpr(expr);

    // Should lower successfully (not a runtime error)
    try testing.expect(result != .runtime_err_type);

    // The result should be a call to a cross-module method
    try testing.expect(result == .call);
    const method_proc = procIdFromCallableExpr(env_b.mir_store, result.call.func) orelse return error.TestUnexpectedResult;
    try testing.expect(!method_proc.isNone());
}

// --- Gap #10: Recursive symbol monotype patching ---

test "lowerExternalDef: mutually recursive defs get monotypes patched (not left as unit)" {
    var env = try MirTestEnv.initModule("Test",
        \\is_even : U64 -> Bool
        \\is_even = |n| if n == 0 True else is_odd(n - 1)
        \\
        \\is_odd : U64 -> Bool
        \\is_odd = |n| if n == 0 False else is_even(n - 1)
    );
    defer env.deinit();

    // Lower both defs
    const even_expr = try env.lowerNamedDef("is_even");
    const odd_expr = try env.lowerNamedDef("is_odd");

    // Both should lower successfully (not be runtime errors)
    try testing.expect(env.mir_store.getExpr(even_expr) != .runtime_err_type);
    try testing.expect(env.mir_store.getExpr(odd_expr) != .runtime_err_type);

    // The monotypes should NOT be left as unit (the recursion placeholder default)
    const even_type = env.mir_store.typeOf(even_expr);
    const odd_type = env.mir_store.typeOf(odd_expr);
    try testing.expect(even_type != env.mir_store.monotype_store.unit_idx);
    try testing.expect(odd_type != env.mir_store.monotype_store.unit_idx);

    // Both should resolve to func types (U64 -> Bool)
    const even_mono = env.mir_store.monotype_store.getMonotype(even_type);
    const odd_mono = env.mir_store.monotype_store.getMonotype(odd_type);
    try testing.expect(even_mono == .func);
    try testing.expect(odd_mono == .func);
}

test "lowerExpr: mutually recursive local closures lower to a finite recursive proc set" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    is_even = |n| if (n == 0) True else is_odd(n - 1)
        \\    is_odd = |n| if (n == 0) False else is_even(n - 1)
        \\    if (is_even(4)) 1 else 0
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    try testing.expect(env.mir_store.getExpr(expr) != .runtime_err_type);

    var recursive_proc_count: usize = 0;
    for (env.mir_store.getProcs()) |proc| {
        if (proc.recursion != .recursive) continue;
        try testing.expectEqual(@as(usize, 0), env.mir_store.getCaptureBindings(proc.capture_bindings).len);
        recursive_proc_count += 1;
    }

    try testing.expectEqual(@as(usize, 2), recursive_proc_count);
}

// --- Cross-module builtin call lowering ---
//
// These tests verify that calls to Builtin module functions (List.map, List.get,
// Num.to, etc.) lower correctly through MIR. Each test corresponds to one or more
// snapshot failures where the dev backend panics with "generateLookupCall: symbol
// not found" or "index out of bounds: index 268435454" — both symptoms of
// cross-module resolution failing during MIR lowering.

// -- List.map (snapshot: list_map.md, list_map_empty.md) --

test "cross-module: List.map lowers without error" {
    var env = try MirTestEnv.initExpr("List.map([2.I64, 4.I64, 6.I64], |val| val * 2)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: List.map on empty list lowers without error" {
    var env = try MirTestEnv.initExpr("List.map([], |_| 0)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- List.keep_if / List.drop_if (snapshots: list_keep_if*.md, list_drop_if.md) --

test "cross-module: List.keep_if lowers without error" {
    var env = try MirTestEnv.initExpr("List.keep_if([1.I64, 2.I64, 3.I64, 4.I64, 5.I64], |x| x > 2)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: proc bodies do not directly lookup foreign proc params" {
    var env = try MirTestEnv.initExpr("List.contains([1.I64, 2.I64, 3.I64, 4.I64, 5.I64], 3.I64)");
    defer env.deinit();
    _ = try env.lowerFirstDef();

    var all_param_symbols = std.AutoHashMap(MIR.Symbol, MIR.ProcId).init(test_allocator);
    defer all_param_symbols.deinit();

    for (env.mir_store.getProcs(), 0..) |proc, proc_idx_usize| {
        const proc_id: MIR.ProcId = @enumFromInt(proc_idx_usize);
        for (env.mir_store.getPatternSpan(proc.params)) |param_id| {
            const symbol = switch (env.mir_store.getPattern(param_id)) {
                .bind => |sym| sym,
                .as_pattern => |as_pat| as_pat.symbol,
                else => continue,
            };
            try all_param_symbols.put(symbol, proc_id);
        }
    }

    for (env.mir_store.getProcs(), 0..) |proc, proc_idx_usize| {
        const proc_id: MIR.ProcId = @enumFromInt(proc_idx_usize);
        if (proc.body.isNone()) continue;
        if (firstForeignParamLookup(env.mir_store, proc.body, proc_id, &all_param_symbols)) |found| {
            std.debug.print("proc {d} body={d} captures_param={d} capture_bindings={d}\n", .{
                proc_idx_usize,
                @intFromEnum(proc.body),
                @intFromEnum(proc.captures_param),
                proc.capture_bindings.len,
            });
            for (env.mir_store.getPatternSpan(proc.params), 0..) |param_id, param_i| {
                const symbol = switch (env.mir_store.getPattern(param_id)) {
                    .bind => |sym| sym,
                    .as_pattern => |as_pat| as_pat.symbol,
                    else => continue,
                };
                std.debug.print("  param {d} symbol={d}\n", .{ param_i, symbol.raw() });
            }
            for (env.mir_store.getCaptureBindings(proc.capture_bindings), 0..) |binding, binding_i| {
                std.debug.print(
                    "  capture_binding {d} local_symbol={d} source_expr={d}\n",
                    .{ binding_i, binding.local_symbol.raw(), @intFromEnum(binding.source_expr) },
                );
            }
            dumpMirExpr(env.mir_store, proc.body, 1);
            std.debug.print(
                "foreign param lookup in proc {d}: symbol={d} owner_proc={d} expr={d}\n",
                .{
                    proc_idx_usize,
                    found.symbol.raw(),
                    @intFromEnum(found.owner_proc),
                    @intFromEnum(found.expr_id),
                },
            );
            return error.TestUnexpectedResult;
        }
    }
}

test "cross-module: List.keep_if seeds lambda sets for reachable callable params" {
    var env = try MirTestEnv.initExpr("List.keep_if([1.I64, 2.I64, 3.I64, 4.I64, 5.I64], |x| x > 2)");
    defer env.deinit();
    _ = try env.lowerFirstDef();

    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, env.lower.all_module_envs);
    defer ls_store.deinit(test_allocator);

    for (env.mir_store.getProcs(), 0..) |proc, proc_idx| {
        for (env.mir_store.getPatternSpan(proc.params)) |param_id| {
            const param_mono = env.mir_store.patternTypeOf(param_id);
            if (env.mir_store.monotype_store.getMonotype(param_mono) != .func) continue;
            const symbol = switch (env.mir_store.getPattern(param_id)) {
                .bind => |sym| sym,
                .as_pattern => |as_pat| as_pat.symbol,
                else => continue,
            };
            if (ls_store.getSymbolLambdaSet(symbol) == null) {
                std.debug.print(
                    "missing lambda set for proc {d} callable param symbol={d}\n",
                    .{ proc_idx, symbol.raw() },
                );
            }
            try testing.expect(ls_store.getSymbolLambdaSet(symbol) != null);
        }
    }
}

test "cross-module runExpr: List.keep_if callable lookup sites retain lambda sets" {
    var env = try MirTestEnv.initExpr("List.keep_if([1.I64, 2.I64, 3.I64, 4.I64, 5.I64], |x| x > 2)");
    defer env.deinit();

    const defs = env.module_env.store.sliceDefs(env.module_env.all_defs);
    const main_def = env.module_env.store.getDef(defs[0]);
    const all_module_envs = [_]*ModuleEnv{ @constCast(env.builtin_module.env), env.module_env };

    var monomorphization = try Monomorphize.runExpr(
        test_allocator,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
        main_def.expr,
    );
    defer monomorphization.deinit(test_allocator);

    var mir_store = try MIR.Store.init(test_allocator);
    defer mir_store.deinit(test_allocator);

    var lower = try Lower.init(
        test_allocator,
        &mir_store,
        &monomorphization,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
    );
    defer lower.deinit();

    _ = try lower.lowerExpr(main_def.expr);

    var ls_store = try LambdaSet.infer(test_allocator, &mir_store, @as([]const *ModuleEnv, all_module_envs[0..]));
    defer ls_store.deinit(test_allocator);

    for (mir_store.exprs.items) |expr| {
        if (expr != .call) continue;
        const func_expr_id = expr.call.func;
        const func_expr = mir_store.getExpr(func_expr_id);
        if (func_expr != .lookup) continue;
        if (mir_store.monotype_store.getMonotype(mir_store.typeOf(func_expr_id)) != .func) continue;
        try testing.expect(
            ls_store.getExprLambdaSet(func_expr_id) != null or
                ls_store.getSymbolLambdaSet(func_expr.lookup) != null,
        );
    }
}

test "runExpr: top-level local function lookup materializes callable def proc inst" {
    var env = try MirTestEnv.initFull("Test",
        \\fn0 = |a| a + 1
        \\main = fn0(10)
    );
    defer env.deinit();

    const def = try env.getDefExprByName("main");
    const all_module_envs = [_]*ModuleEnv{ @constCast(env.builtin_module.env), env.module_env };

    var monomorphization = try Monomorphize.runExpr(
        test_allocator,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
        def.expr_idx,
    );
    defer monomorphization.deinit(test_allocator);

    var mir_store = try MIR.Store.init(test_allocator);
    defer mir_store.deinit(test_allocator);

    var lower = try Lower.init(
        test_allocator,
        &mir_store,
        &monomorphization,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
    );
    defer lower.deinit();

    const expr = try lower.lowerExpr(def.expr_idx);
    const result = mir_store.getExpr(expr);
    try testing.expect(result == .call);

    const proc_id = procIdFromCallableExpr(&mir_store, result.call.func) orelse return error.TestUnexpectedResult;
    try testing.expect(!proc_id.isNone());
}

test "runExpr: block-carried local function lookup materializes callable stmt proc inst" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    fn0 = |a| a + 1
        \\    fn0(10)
        \\}
    );
    defer env.deinit();

    const defs = env.module_env.store.sliceDefs(env.module_env.all_defs);
    const main_def = env.module_env.store.getDef(defs[0]);
    const all_module_envs = [_]*ModuleEnv{ @constCast(env.builtin_module.env), env.module_env };

    var monomorphization = try Monomorphize.runExpr(
        test_allocator,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
        main_def.expr,
    );
    defer monomorphization.deinit(test_allocator);

    var mir_store = try MIR.Store.init(test_allocator);
    defer mir_store.deinit(test_allocator);

    var lower = try Lower.init(
        test_allocator,
        &mir_store,
        &monomorphization,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
    );
    defer lower.deinit();

    const expr = try lower.lowerExpr(main_def.expr);
    const result = mir_store.getExpr(expr);
    try testing.expect(result == .block);

    const final_expr = mir_store.getExpr(result.block.final_expr);
    try testing.expect(final_expr == .call);

    const proc_id = procIdFromCallableExpr(&mir_store, final_expr.call.func) orelse return error.TestUnexpectedResult;
    try testing.expect(!proc_id.isNone());
}

test "runExpr: block-carried arrow call materializes callable stmt proc inst" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    fn0 = |a| a + 1
        \\    10->fn0
        \\}
    );
    defer env.deinit();

    const defs = env.module_env.store.sliceDefs(env.module_env.all_defs);
    const main_def = env.module_env.store.getDef(defs[0]);
    const all_module_envs = [_]*ModuleEnv{ @constCast(env.builtin_module.env), env.module_env };

    var monomorphization = try Monomorphize.runExpr(
        test_allocator,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
        main_def.expr,
    );
    defer monomorphization.deinit(test_allocator);

    var mir_store = try MIR.Store.init(test_allocator);
    defer mir_store.deinit(test_allocator);

    var lower = try Lower.init(
        test_allocator,
        &mir_store,
        &monomorphization,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
    );
    defer lower.deinit();

    const expr = try lower.lowerExpr(main_def.expr);
    const result = mir_store.getExpr(expr);
    try testing.expect(result == .block);

    const final_expr = mir_store.getExpr(result.block.final_expr);
    try testing.expect(final_expr == .call);

    const proc_id = procIdFromCallableExpr(&mir_store, final_expr.call.func) orelse return error.TestUnexpectedResult;
    try testing.expect(!proc_id.isNone());
}

test "runExpr: proc-backed closure capture sources retain lambda sets" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    append_one = |acc, x| List.append(acc, x)
        \\    clone_via_fold = |xs| xs.fold(List.with_capacity(1), append_one)
        \\    _first_len = clone_via_fold([1.I64, 2.I64]).len()
        \\    clone_via_fold([[1.I64, 2.I64], [3.I64, 4.I64]]).len()
        \\}
    );
    defer env.deinit();

    const defs = env.module_env.store.sliceDefs(env.module_env.all_defs);
    const main_def = env.module_env.store.getDef(defs[0]);
    const all_module_envs = [_]*ModuleEnv{ @constCast(env.builtin_module.env), env.module_env };

    var monomorphization = try Monomorphize.runExpr(
        test_allocator,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
        main_def.expr,
    );
    defer monomorphization.deinit(test_allocator);

    var mir_store = try MIR.Store.init(test_allocator);
    defer mir_store.deinit(test_allocator);

    var lower = try Lower.init(
        test_allocator,
        &mir_store,
        &monomorphization,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
    );
    defer lower.deinit();

    _ = try lower.lowerExpr(main_def.expr);

    var ls_store = try LambdaSet.infer(test_allocator, &mir_store, @as([]const *ModuleEnv, all_module_envs[0..]));
    defer ls_store.deinit(test_allocator);

    for (mir_store.closure_members.items) |closure_member| {
        for (mir_store.getCaptureBindings(closure_member.capture_bindings)) |binding| {
            const source_expr = mir_store.getExpr(binding.source_expr);
            if (source_expr != .lookup) continue;
            if (mir_store.monotype_store.getMonotype(binding.monotype) != .func) continue;
            try testing.expect(
                ls_store.getExprLambdaSet(binding.source_expr) != null or
                    ls_store.getSymbolLambdaSet(source_expr.lookup) != null,
            );
        }
    }
}

test "runExpr: repl-style multi-definition arrow call materializes callable stmt proc inst" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    fn0 = |a| a + 1
        \\    fn1 = |a, b| a + b
        \\    fn2 = |a, b, c| a + b + c
        \\    fn3 = |a, b, c, d| a + b + c + d
        \\    10->fn0
        \\}
    );
    defer env.deinit();

    const defs = env.module_env.store.sliceDefs(env.module_env.all_defs);
    const main_def = env.module_env.store.getDef(defs[0]);
    const all_module_envs = [_]*ModuleEnv{ @constCast(env.builtin_module.env), env.module_env };

    var monomorphization = try Monomorphize.runExpr(
        test_allocator,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
        main_def.expr,
    );
    defer monomorphization.deinit(test_allocator);

    var mir_store = try MIR.Store.init(test_allocator);
    defer mir_store.deinit(test_allocator);

    var lower = try Lower.init(
        test_allocator,
        &mir_store,
        &monomorphization,
        @as([]const *ModuleEnv, all_module_envs[0..]),
        &env.module_env.types,
        1,
        null,
    );
    defer lower.deinit();

    const expr = try lower.lowerExpr(main_def.expr);
    const result = mir_store.getExpr(expr);
    try testing.expect(result == .block);

    const final_expr = mir_store.getExpr(result.block.final_expr);
    try testing.expect(final_expr == .call);

    const proc_id = procIdFromCallableExpr(&mir_store, final_expr.call.func) orelse return error.TestUnexpectedResult;
    try testing.expect(!proc_id.isNone());
}

test "cross-module: List.keep_if with always-false predicate lowers without error" {
    var env = try MirTestEnv.initExpr("List.keep_if([1.I64, 2.I64, 3.I64], |_| Bool.False)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: List.drop_if lowers without error" {
    var env = try MirTestEnv.initExpr("List.drop_if([1.I64, 2.I64, 3.I64, 4.I64, 5.I64], |x| x > 2)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- List.count_if (snapshots: list_count_if*.md) --

test "cross-module: List.count_if lowers without error" {
    var env = try MirTestEnv.initExpr("List.count_if([1.I64, 2.I64, 3.I64, 4.I64, 5.I64], |x| x > 2)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: List.count_if on empty list lowers without error" {
    var env = try MirTestEnv.initExpr("List.count_if([], |x| x > 2.I64)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- List.get (snapshot: list_get.md) --

test "cross-module: List.get lowers without error" {
    var env = try MirTestEnv.initExpr("List.get([1.I64, 2.I64, 3.I64], 0)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- List.first (snapshots: list_first.md, double_question_list_first.md) --

test "cross-module: List.first lowers without error" {
    var env = try MirTestEnv.initExpr("List.first([1.I64, 2.I64, 3.I64])");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- List.last (snapshot: list_last.md) --

test "cross-module: List.last lowers without error" {
    var env = try MirTestEnv.initExpr("List.last([1.I64, 2.I64, 3.I64])");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- List.repeat (snapshot: list_repeat.md) --

test "cross-module: List.repeat lowers without error" {
    var env = try MirTestEnv.initExpr("List.repeat(4.I64, 7)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- List.sublist (snapshots: list_sublist_nested.md, list_tags.md) --

test "cross-module: List.sublist lowers without error" {
    var env = try MirTestEnv.initExpr("List.sublist([1.I64, 2.I64, 3.I64, 4.I64], {start: 1, len: 2})");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- List.fold_rev (snapshots: list_fold_rev_basic.md, list_fold_rev_subtract.md, list_fold_rev_empty.md) --

test "cross-module: List.fold_rev lowers without error" {
    var env = try MirTestEnv.initExpr("List.fold_rev([1.I64, 2.I64, 3.I64], 0.I64, |x, acc| acc * 10 + x)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: List.fold_rev on empty list lowers without error" {
    var env = try MirTestEnv.initExpr("List.fold_rev([], 42.I64, |x, acc| x + acc)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- List.join_with (snapshot: list_join_with.md) --

test "cross-module: List.join_with lowers without error" {
    var env = try MirTestEnv.initExpr(
        \\List.join_with(["hello", "world"], " ")
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- List.sort_with (snapshot: list_sort_with.md) --

test "cross-module: List.sort_with lowers without error" {
    var env = try MirTestEnv.initExpr("List.sort_with([3.I64, 1.I64, 2.I64], |a, b| if a < b LT else if a > b GT else EQ)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- Num.to / Num.until range methods (snapshots: *_range_to.md, *_range_until.md) --
// All numeric range methods exercise the same cross-module dispatch path.
// We test representative types: U8, I64, U64, Dec.

test "cross-module: I64.to range method lowers without error" {
    var env = try MirTestEnv.initExpr("1.I64.to(5.I64)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: I64.until range method lowers without error" {
    var env = try MirTestEnv.initExpr("1.I64.until(5.I64)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: U8.to range method lowers without error" {
    var env = try MirTestEnv.initExpr("1.U8.to(5.U8)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: U8.until range method lowers without error" {
    var env = try MirTestEnv.initExpr("0.U8.until(3.U8)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: U16.to range method lowers without error" {
    var env = try MirTestEnv.initExpr("1.U16.to(5.U16)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: U16.until range method lowers without error" {
    var env = try MirTestEnv.initExpr("0.U16.until(3.U16)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: I16.to range method lowers without error" {
    var env = try MirTestEnv.initExpr("1.I16.to(5.I16)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: I16.until range method lowers without error" {
    var env = try MirTestEnv.initExpr("0.I16.until(3.I16)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: U32.to range method lowers without error" {
    var env = try MirTestEnv.initExpr("1.U32.to(5.U32)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: U32.until range method lowers without error" {
    var env = try MirTestEnv.initExpr("0.U32.until(3.U32)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: I32.to range method lowers without error" {
    var env = try MirTestEnv.initExpr("1.I32.to(5.I32)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: I32.until range method lowers without error" {
    var env = try MirTestEnv.initExpr("0.I32.until(3.I32)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: U64.to range method lowers without error" {
    var env = try MirTestEnv.initExpr("1.U64.to(5.U64)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: U64.until range method lowers without error" {
    var env = try MirTestEnv.initExpr("0.U64.until(3.U64)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: I128.to range method lowers without error" {
    var env = try MirTestEnv.initExpr("1.I128.to(5.I128)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: I128.until range method lowers without error" {
    var env = try MirTestEnv.initExpr("0.I128.until(3.I128)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: U128.to range method lowers without error" {
    var env = try MirTestEnv.initExpr("1.U128.to(5.U128)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: U128.until range method lowers without error" {
    var env = try MirTestEnv.initExpr("0.U128.until(3.U128)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: I8.to range method lowers without error" {
    var env = try MirTestEnv.initExpr("1.I8.to(5.I8)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: I8.until range method lowers without error" {
    var env = try MirTestEnv.initExpr("0.I8.until(3.I8)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: Dec.to range method lowers without error" {
    var env = try MirTestEnv.initFull("Test",
        \\main = 0.5.to(2.5)
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: Dec.until range method lowers without error" {
    var env = try MirTestEnv.initFull("Test",
        \\main = 0.5.until(3.5)
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- Method on literal (snapshots: method_on_int_literal.md, method_on_float_literal.md) --

test "cross-module: method call on int literal lowers without error" {
    var env = try MirTestEnv.initExpr("35.abs()");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: method call on float literal lowers without error" {
    var env = try MirTestEnv.initExpr("12.34.abs()");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- Try.is_eq / equality on Result (snapshot: try_is_eq.md) --

test "cross-module: Try equality lowers without error" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = Try.Ok(1) == Try.Ok(1)
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// -- Deeply nested polymorphic functions (snapshot: deeply_nested_polymorphic_functions.md) --

test "cross-module: deeply nested polymorphic HOF lowers without error" {
    var env = try MirTestEnv.initExpr("(|twice, identity| { a: twice(identity, 42.I64), b: twice(|x| x + 1, 100.I64) })(|f, val| f(f(val)), |x| x)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: deeply nested polymorphic HOF nested lookup callee keeps both members" {
    var env = try MirTestEnv.initExpr("(|twice, identity| { a: twice(identity, 42.I64), b: twice(|x| x + 1, 100.I64) })(|f, val| f(f(val)), |x| x)");
    defer env.deinit();
    _ = try env.lowerFirstDef();

    const all_module_envs = [_]*ModuleEnv{
        @constCast(env.builtin_module.env),
        env.module_env,
    };
    var ls_store = try LambdaSet.infer(test_allocator, env.mir_store, all_module_envs[0..]);
    defer ls_store.deinit(test_allocator);

    var nested_lookup_count: usize = 0;
    for (env.mir_store.exprs.items) |expr| {
        if (expr != .call) continue;
        const func_expr_id = expr.call.func;
        const func_expr = env.mir_store.getExpr(func_expr_id);
        if (func_expr != .lookup) continue;

        const expr_ls = ls_store.getExprLambdaSet(func_expr_id) orelse continue;
        const symbol_ls = ls_store.getSymbolLambdaSet(func_expr.lookup) orelse continue;
        const expr_members = ls_store.getMembers(ls_store.getLambdaSet(expr_ls).members);
        const symbol_members = ls_store.getMembers(ls_store.getLambdaSet(symbol_ls).members);
        if (expr_members.len != 2 or symbol_members.len != 2) continue;

        try testing.expectEqual(@as(usize, 2), expr_members.len);
        try testing.expectEqual(@as(usize, 2), symbol_members.len);
        nested_lookup_count += 1;
    }

    try testing.expectEqual(@as(usize, 2), nested_lookup_count);
}

// -- Fibonacci / recursive with cross-module numeric ops (snapshot: fibonacci.md) --

test "cross-module: recursive fibonacci with numeric ops lowers without error" {
    var env = try MirTestEnv.initFull("Test",
        \\fib : I64 -> I64
        \\fib = |n| if n <= 1 n else fib(n - 1) + fib(n - 2)
        \\
        \\main = fib(5)
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: repl-style recursive fibonacci block materializes one proc inst" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    fib = |n| if n <= 1 n else fib(n - 1) + fib(n - 2)
        \\    fib(5)
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .block);

    var user_template_count: usize = 0;
    var user_proc_inst_count: usize = 0;
    for (env.monomorphization.proc_templates.items) |template| {
        if (template.module_idx != 1) continue;
        user_template_count += 1;
    }
    for (env.monomorphization.proc_insts.items) |proc_inst| {
        const template = env.monomorphization.getProcTemplate(proc_inst.template);
        if (template.module_idx != 1) continue;
        user_proc_inst_count += 1;
    }

    try testing.expectEqual(@as(usize, 2), user_template_count);
    try testing.expectEqual(@as(usize, 1), user_proc_inst_count);
}

// -- String equality (snapshot: string_equality_basic.md) --

test "cross-module: string equality lowers without error" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = "hello" == "hello"
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

test "cross-module: string inequality lowers without error" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = "hello" != "world"
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
}

// --- Bool diagnostic MIR tests ---
// These tests verify MIR lowering of Bool expressions from failing REPL snapshots.
// If all pass, the Bool inversion bug is in LIR lowering or codegen, not MIR.

test "Bool diagnostic MIR: Bool.True lowers to tag with tag_union" {
    var env = try MirTestEnv.initExpr("Bool.True");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // Bool.True should lower to a .tag expression
    try testing.expect(result == .tag);
    try testing.expectEqual(@as(u16, 0), result.tag.args.len);
    // Check the tag name is "True"
    const tag_name = env.module_env.getIdent(result.tag.name);
    try testing.expectEqualStrings("True", tag_name);
    // Monotype should be tag_union
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Bool diagnostic MIR: Bool.False lowers to tag with tag_union" {
    var env = try MirTestEnv.initExpr("Bool.False");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .tag);
    try testing.expectEqual(@as(u16, 0), result.tag.args.len);
    const tag_name = env.module_env.getIdent(result.tag.name);
    try testing.expectEqualStrings("False", tag_name);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Bool diagnostic MIR: Bool.not(True) lowers with tag_union type" {
    var env = try MirTestEnv.initExpr("Bool.not(True)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // Bool.not may lower as a cross-module call or inlined match
    try testing.expect(result != .runtime_err_type);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Bool diagnostic MIR: Bool.not(False) lowers with tag_union type" {
    var env = try MirTestEnv.initExpr("Bool.not(False)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Bool diagnostic MIR: !Bool.True lowers to match_expr with tag_union" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = !Bool.True
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    // !Bool.True desugars via negBool to a match expression
    try testing.expect(env.mir_store.getExpr(expr) == .match_expr);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Bool diagnostic MIR: !Bool.False lowers to match_expr with tag_union" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = !Bool.False
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    try testing.expect(env.mir_store.getExpr(expr) == .match_expr);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Bool diagnostic MIR: Bool.True and Bool.False lowers to match_expr with tag_union" {
    var env = try MirTestEnv.initExpr("Bool.True and Bool.False");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // `and` short-circuit desugars to a match expression
    try testing.expect(result == .match_expr);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Bool diagnostic MIR: !Bool.True or !Bool.True lowers with tag_union" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = !Bool.True or !Bool.True
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // `or` short-circuit desugars to a match expression
    try testing.expect(result == .match_expr);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Bool diagnostic MIR: lambda negation applied to Bool.True lowers with tag_union" {
    var env = try MirTestEnv.initExpr("(|x| !x)(Bool.True)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // A lambda call should produce a .call expression
    try testing.expect(result != .runtime_err_type);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

// --- Bool.not structural tests ---
// Verify the MIR match structure produced by negBool and Bool.not calls.

test "Bool.not MIR: !Bool.True match has True pattern -> False body, wildcard -> True body" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = !Bool.True
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // !Bool.True desugars to: match Bool.True { True => False, _ => True }
    try testing.expect(result == .match_expr);

    // Check the condition is a tag expression (Bool.True)
    const cond = env.mir_store.getExpr(result.match_expr.cond);
    try testing.expect(cond == .tag);
    const cond_tag_name = env.module_env.getIdent(cond.tag.name);
    try testing.expectEqualStrings("True", cond_tag_name);

    // Check condition monotype is tag_union
    const cond_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(result.match_expr.cond));
    try testing.expect(cond_mono == .tag_union);

    // Check there are 2 branches
    const branches = env.mir_store.getBranches(result.match_expr.branches);
    try testing.expectEqual(@as(usize, 2), branches.len);

    // Branch 0: True => False
    const bp0 = env.mir_store.getBranchPatterns(branches[0].patterns);
    try testing.expectEqual(@as(usize, 1), bp0.len);
    const pat0 = env.mir_store.getPattern(bp0[0].pattern);
    try testing.expect(pat0 == .tag);
    const pat0_name = env.module_env.getIdent(pat0.tag.name);
    try testing.expectEqualStrings("True", pat0_name);
    // Pattern monotype should be tag_union
    const pat0_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.patternTypeOf(bp0[0].pattern));
    try testing.expect(pat0_mono == .tag_union);
    // Body should be False tag
    const body0 = env.mir_store.getExpr(branches[0].body);
    try testing.expect(body0 == .tag);
    const body0_name = env.module_env.getIdent(body0.tag.name);
    try testing.expectEqualStrings("False", body0_name);

    // Branch 1: _ => True
    const bp1 = env.mir_store.getBranchPatterns(branches[1].patterns);
    try testing.expectEqual(@as(usize, 1), bp1.len);
    const pat1 = env.mir_store.getPattern(bp1[0].pattern);
    try testing.expect(pat1 == .wildcard);
    // Body should be True tag
    const body1 = env.mir_store.getExpr(branches[1].body);
    try testing.expect(body1 == .tag);
    const body1_name = env.module_env.getIdent(body1.tag.name);
    try testing.expectEqualStrings("True", body1_name);
}

test "Bool.not MIR: !Bool.False match has True pattern -> False body, wildcard -> True body" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = !Bool.False
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .match_expr);

    // Condition should be Bool.False
    const cond = env.mir_store.getExpr(result.match_expr.cond);
    try testing.expect(cond == .tag);
    const cond_tag_name = env.module_env.getIdent(cond.tag.name);
    try testing.expectEqualStrings("False", cond_tag_name);
    const cond_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(result.match_expr.cond));
    try testing.expect(cond_mono == .tag_union);

    // Branch structure should be identical: True => False, _ => True
    const branches = env.mir_store.getBranches(result.match_expr.branches);
    try testing.expectEqual(@as(usize, 2), branches.len);

    const bp0 = env.mir_store.getBranchPatterns(branches[0].patterns);
    const pat0 = env.mir_store.getPattern(bp0[0].pattern);
    try testing.expect(pat0 == .tag);
    try testing.expectEqualStrings("True", env.module_env.getIdent(pat0.tag.name));
    const body0 = env.mir_store.getExpr(branches[0].body);
    try testing.expect(body0 == .tag);
    try testing.expectEqualStrings("False", env.module_env.getIdent(body0.tag.name));

    const bp1 = env.mir_store.getBranchPatterns(branches[1].patterns);
    const pat1 = env.mir_store.getPattern(bp1[0].pattern);
    try testing.expect(pat1 == .wildcard);
    const body1 = env.mir_store.getExpr(branches[1].body);
    try testing.expect(body1 == .tag);
    try testing.expectEqualStrings("True", env.module_env.getIdent(body1.tag.name));
}

test "Bool.not MIR: Bool.not(True) is a call with tag_union return type" {
    var env = try MirTestEnv.initExpr("Bool.not(True)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // Bool.not(True) should be a call expression (cross-module method call)
    try testing.expect(result == .call);
    // Return type should be tag_union
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
    // The argument should be a tag (True) with tag_union type
    const args = env.mir_store.getExprSpan(result.call.args);
    try testing.expectEqual(@as(usize, 1), args.len);
    const arg = env.mir_store.getExpr(args[0]);
    try testing.expect(arg == .tag);
    try testing.expectEqualStrings("True", env.module_env.getIdent(arg.tag.name));
    const arg_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(args[0]));
    try testing.expect(arg_mono == .tag_union);
}

test "Bool.not MIR: Bool.not(False) is a call with tag_union return type" {
    var env = try MirTestEnv.initExpr("Bool.not(False)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .call);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
    const args = env.mir_store.getExprSpan(result.call.args);
    try testing.expectEqual(@as(usize, 1), args.len);
    const arg = env.mir_store.getExpr(args[0]);
    try testing.expect(arg == .tag);
    try testing.expectEqualStrings("False", env.module_env.getIdent(arg.tag.name));
    const arg_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(args[0]));
    try testing.expect(arg_mono == .tag_union);
}

// --- Nominal Bool vs structural tag union MIR tests ---
// CRITICAL DISTINCTION: Bare tags like `True` and `False` without a Bool annotation
// must lower to `.tag_union` monotype, and nominal `Bool` now does too.
// See also: corresponding type-checking tests in type_checking_integration.zig.

test "Nominal Bool MIR: annotated True lowers with tag_union" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = True
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Nominal Bool MIR: annotated False lowers with tag_union" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = False
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Structural tag MIR: bare True lowers as tag_union" {
    var env = try MirTestEnv.initExpr("True");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Structural tag MIR: bare False lowers as tag_union" {
    var env = try MirTestEnv.initExpr("False");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Structural tag MIR: if True True else False lowers as tag_union" {
    var env = try MirTestEnv.initExpr("if True True else False");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

// --- Cross-module type resolution: method dispatch resolves concrete types ---

test "cross-module type resolution: U32.to dispatches with concrete U32 function type" {
    // `1.U32.to(5.U32)` dispatches to Builtin's `to` method.
    // The lowered call's function must have monotype `func(U32, U32) -> List(U32)`,
    // NOT `func(unit, unit) -> List(unit)` (which would happen if flex vars in the
    // Builtin module resolve to unit instead of the caller's concrete types).
    var env = try MirTestEnv.initExpr("1.U32.to(5.U32)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();

    // The top-level expression should be a call
    const top = env.mir_store.getExpr(expr);
    try testing.expect(top == .call);

    // The call's function should be a lookup whose monotype is a func
    const proc_id = procIdFromCallableExpr(env.mir_store, top.call.func) orelse return error.TestUnexpectedResult;
    const func_monotype_idx = env.mir_store.typeOf(top.call.func);
    const func_mono = env.mir_store.monotype_store.getMonotype(func_monotype_idx);
    try testing.expect(func_mono == .func);

    // The function's return type must be List(U32), not List(unit)
    const ret_mono = env.mir_store.monotype_store.getMonotype(func_mono.func.ret);
    try testing.expect(ret_mono == .list);

    const elem_mono = env.mir_store.monotype_store.getMonotype(ret_mono.list.elem);
    try testing.expectEqual(Monotype.Monotype{ .prim = .u32 }, elem_mono);

    // Verify the specialized proc body was lowered with concrete types.
    const proc = env.mir_store.getProc(proc_id);
    const def_mono = env.mir_store.monotype_store.getMonotype(proc.fn_monotype);
    try testing.expect(def_mono == .func);
    const def_ret_mono = env.mir_store.monotype_store.getMonotype(def_mono.func.ret);
    try testing.expect(def_ret_mono == .list);
    const def_elem = env.mir_store.monotype_store.getMonotype(def_ret_mono.list.elem);
    try testing.expectEqual(Monotype.Monotype{ .prim = .u32 }, def_elem);
}

// --- Polymorphic numeric specialization tests ---
// These tests verify that polymorphic lambdas in blocks get the correct
// monotype when called with concrete numeric types (not defaulting to Dec).

test "polymorphic lambda in block: sum called with U64 gets U64 monotype, not Dec" {
    // This is the core polymorphic numeric specialization bug:
    // `sum = |a, b| a + b + 0` is polymorphic (Num * => * -> * -> *)
    // When called as `sum(240.U64, 20.U64)`, the call and its args must be U64.
    // BUG: The lambda body gets lowered first with Dec-defaulted types,
    // then the call reuses the Dec-typed version.
    var env = try MirTestEnv.initExpr(
        \\{
        \\    sum = |a, b| a + b + 0
        \\    sum(240.U64, 20.U64)
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .block);

    // The final expression is `sum(240.U64, 20.U64)` — a call
    const final_expr = env.mir_store.getExpr(result.block.final_expr);
    try testing.expect(final_expr == .call);

    // The call's return type must be U64, not Dec
    const call_monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(result.block.final_expr));
    try testing.expectEqual(Monotype.Prim.u64, call_monotype.prim);

    // The call's arguments must be U64
    const args = env.mir_store.getExprSpan(final_expr.call.args);
    for (args) |arg| {
        const arg_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(arg));
        try testing.expect(arg_mono == .prim);
        try testing.expectEqual(Monotype.Prim.u64, arg_mono.prim);
    }

    // The called proc itself should have U64 params and return type.
    const sum_proc_id = procIdFromCallableExpr(env.mir_store, final_expr.call.func) orelse return error.TestUnexpectedResult;
    const lambda_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(final_expr.call.func));
    try testing.expect(lambda_mono == .func);
    const ret_mono = env.mir_store.monotype_store.getMonotype(lambda_mono.func.ret);
    try testing.expect(ret_mono == .prim);
    try testing.expectEqual(Monotype.Prim.u64, ret_mono.prim);

    const sum_proc = env.mir_store.getProc(sum_proc_id);
    for (env.mir_store.getPatternSpan(sum_proc.params)) |param_id| {
        const param = env.mir_store.getPattern(param_id);
        try testing.expect(param == .bind);
        const param_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.patternTypeOf(param_id));
        try testing.expect(param_mono == .prim);
        try testing.expectEqual(Monotype.Prim.u64, param_mono.prim);
    }
}

test "polymorphic lambda in block: fn called via arrow syntax gets correct type" {
    // `fn1 = |a, b| a + b`, `10.U64->fn1(20.U64)` should dispatch as U64
    var env = try MirTestEnv.initExpr(
        \\{
        \\    fn1 = |a, b| a + b
        \\    fn1(10.U64, 20.U64)
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .block);

    // The final expression's return type must be U64
    const call_monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(result.block.final_expr));
    try testing.expect(call_monotype == .prim);
    try testing.expectEqual(Monotype.Prim.u64, call_monotype.prim);
}

test "polymorphic lambda with literal in body: a + b + 0 called with U64" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    sum = |a, b| a + b + 0
        \\    sum(240.U64, 20.U64)
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .block);

    // The call's return type must be U64
    const call_monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(result.block.final_expr));
    try testing.expectEqual(Monotype.Prim.u64, call_monotype.prim);

    const final_expr = env.mir_store.getExpr(result.block.final_expr);
    try testing.expect(final_expr == .call);
    const sum_proc_id = procIdFromCallableExpr(env.mir_store, final_expr.call.func) orelse return error.TestUnexpectedResult;
    const decl_proc = env.mir_store.getProc(sum_proc_id);

    // Lambda return must be U64
    const lambda_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(final_expr.call.func));
    try testing.expect(lambda_mono == .func);
    const ret_mono = env.mir_store.monotype_store.getMonotype(lambda_mono.func.ret);
    try testing.expectEqual(Monotype.Prim.u64, ret_mono.prim);

    // Check that the lambda body's subexpressions are all U64, not Dec
    // The body is `a + b + 0` which desugars to `(a + b) + 0`
    // The body is either a call or run_low_level for the outer `+`
    const body = env.mir_store.getExpr(decl_proc.body);
    const body_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(decl_proc.body));
    try testing.expectEqual(Monotype.Prim.u64, body_mono.prim);

    // The outer `+` has args: (a + b) and 0
    // Check that the 0 literal has U64 monotype
    if (body == .run_low_level) {
        const ll_args = env.mir_store.getExprSpan(body.run_low_level.args);
        if (ll_args.len == 2) {
            const zero_expr = env.mir_store.getExpr(ll_args[1]);
            try testing.expect(zero_expr == .int);
            const zero_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(ll_args[1]));
            try testing.expectEqual(Monotype.Prim.u64, zero_mono.prim);
        }
    } else if (body == .call) {
        const call_args = env.mir_store.getExprSpan(body.call.args);
        if (call_args.len == 2) {
            const zero_expr = env.mir_store.getExpr(call_args[1]);
            try testing.expect(zero_expr == .int);
        }
    }
}

// --- Structural equality tests ---
// Verify that == / != on structural types (records, tuples, tag unions, lists)
// is decomposed into field-level primitive comparisons in MIR.

test "structural equality: record == produces match_expr (field-by-field)" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = { x: 1u64, y: 2u64 } == { x: 1u64, y: 2u64 }
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .borrow_scope);
    const bindings = env.mir_store.getBorrowBindings(result.borrow_scope.bindings);
    try testing.expectEqual(@as(usize, 2), bindings.len);
    try testing.expect(env.mir_store.getExpr(result.borrow_scope.body) == .match_expr);
    // Return type should be Bool
    const mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(mono == .tag_union);
}

test "structural equality: record != produces negated match_expr" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = { x: 1u64 } != { x: 2u64 }
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // != wraps the == result in negBool (match True => False, _ => True)
    try testing.expect(result == .match_expr);
    const mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(mono == .tag_union);
}

test "structural equality: empty record == is True" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = {} == {}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // Empty record equality is always True (a tag)
    try testing.expect(result == .tag);
    try testing.expectEqualStrings("True", env.module_env.getIdent(result.tag.name));
}

test "structural equality: tuple == produces match_expr" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = (1u64, 2u64) == (1u64, 2u64)
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .borrow_scope);
    const bindings = env.mir_store.getBorrowBindings(result.borrow_scope.bindings);
    try testing.expectEqual(@as(usize, 2), bindings.len);
    try testing.expect(env.mir_store.getExpr(result.borrow_scope.body) == .match_expr);
    const mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(mono == .tag_union);
}

test "structural equality: tag union == produces nested match_expr" {
    // Bare tags (`True`/`False`) infer an anonymous tag union in this context,
    // so equality lowers through structural tag-union decomposition.
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = True == False
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .borrow_scope);
    const bindings = env.mir_store.getBorrowBindings(result.borrow_scope.bindings);
    try testing.expectEqual(@as(usize, 2), bindings.len);
    try testing.expect(env.mir_store.getExpr(result.borrow_scope.body) == .match_expr);
    const mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(mono == .tag_union);
}

test "structural equality: single-field record produces run_low_level (no match)" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = { x: 1u64 } == { x: 1u64 }
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .borrow_scope);
    const bindings = env.mir_store.getBorrowBindings(result.borrow_scope.bindings);
    try testing.expectEqual(@as(usize, 2), bindings.len);
    try testing.expect(env.mir_store.getExpr(result.borrow_scope.body) == .run_low_level);
}

test "structural equality: nested record produces match_expr" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = { x: { y: 1u64 } } == { x: { y: 1u64 } }
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .borrow_scope);
    const bindings = env.mir_store.getBorrowBindings(result.borrow_scope.bindings);
    try testing.expectEqual(@as(usize, 2), bindings.len);
    try testing.expect(env.mir_store.getExpr(result.borrow_scope.body) == .block or env.mir_store.getExpr(result.borrow_scope.body) == .borrow_scope);
}

test "structural equality: list == produces block with length check" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = [1u64, 2u64] == [1u64, 2u64]
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .borrow_scope);
    const bindings = env.mir_store.getBorrowBindings(result.borrow_scope.bindings);
    try testing.expectEqual(@as(usize, 2), bindings.len);
    const mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(mono == .tag_union);
}

test "Dec.abs lowers to num_abs with Dec monotype, not unit" {
    var env = try MirTestEnv.initExpr("(-3.14).abs()");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const top = env.mir_store.getExpr(expr);

    // The result monotype must be Dec, not unit
    const mono_idx = env.mir_store.typeOf(expr);
    const mono = env.mir_store.monotype_store.getMonotype(mono_idx);
    try testing.expectEqual(Monotype.Monotype{ .prim = .dec }, mono);

    // If it's a call, check the function's monotype too
    if (top == .call) {
        const func_mono_idx = env.mir_store.typeOf(top.call.func);
        const func_mono = env.mir_store.monotype_store.getMonotype(func_mono_idx);
        try testing.expect(func_mono == .func);
        const ret = env.mir_store.monotype_store.getMonotype(func_mono.func.ret);
        try testing.expectEqual(Monotype.Monotype{ .prim = .dec }, ret);
    }
}
