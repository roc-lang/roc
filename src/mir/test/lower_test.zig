//! Tests for MIR Store, Monotype Store, and Lower initialization.

const std = @import("std");
const testing = std.testing;

const base = @import("base");
const can = @import("can");
const types = @import("types");

const MIR = @import("../MIR.zig");
const Monotype = @import("../Monotype.zig");
const Lower = @import("../Lower.zig");
const MirTestEnv = @import("MirTestEnv.zig");

const ModuleEnv = can.ModuleEnv;
const Region = base.Region;
const Ident = base.Ident;

const test_allocator = testing.allocator;

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

    const monotype = store.monotype_store.primIdx(.bool);
    const symbol = MIR.Symbol{ .module_idx = 0, .ident_idx = Ident.Idx.NONE };
    const pat_id = try store.addPattern(test_allocator, .{ .bind = symbol }, monotype);

    const retrieved = store.getPattern(pat_id);
    switch (retrieved) {
        .bind => |sym| try testing.expect(sym.module_idx == 0),
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

    const monotype = store.monotype_store.primIdx(.bool);
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
    const symbol = MIR.Symbol{ .module_idx = 0, .ident_idx = Ident.Idx.NONE };
    const pat = try store.addPattern(test_allocator, .{ .bind = symbol }, monotype);

    const stmt_span = try store.addStmts(test_allocator, &.{.{ .decl_const = .{ .pattern = pat, .expr = expr } }});
    const stmts = store.getStmts(stmt_span);
    try testing.expectEqual(@as(usize, 1), stmts.len);
    try testing.expectEqual(pat, stmts[0].decl_const.pattern);
    try testing.expectEqual(expr, stmts[0].decl_const.expr);
}

test "MIR Store: field name spans" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const name1 = Ident.Idx.NONE;
    const name2 = Ident.Idx.NONE;

    const span = try store.addFieldNameSpan(test_allocator, &.{ name1, name2 });
    try testing.expectEqual(@as(u16, 2), span.len);

    const retrieved = store.getFieldNameSpan(span);
    try testing.expectEqual(@as(usize, 2), retrieved.len);
}

test "MIR Store: symbol def registration" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const monotype = store.monotype_store.primIdx(.i64);
    const expr_id = try store.addExpr(test_allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, monotype, Region.zero());
    const symbol = MIR.Symbol{ .module_idx = 0, .ident_idx = Ident.Idx.NONE };

    try store.registerSymbolDef(test_allocator, symbol, expr_id);
    const result = store.getSymbolDef(symbol);
    try testing.expect(result != null);
    try testing.expectEqual(expr_id, result.?);
}

test "MIR Store: multiple expressions round trip" {
    var store = try MIR.Store.init(test_allocator);
    defer store.deinit(test_allocator);

    const i64_type = store.monotype_store.primIdx(.i64);
    const str_type = store.monotype_store.primIdx(.str);
    const bool_type = store.monotype_store.primIdx(.bool);

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
    const wild_id = try store.addPattern(test_allocator, .wildcard, bool_type);

    // Verify types
    try testing.expectEqual(i64_type, store.typeOf(int_id));
    try testing.expectEqual(str_type, store.typeOf(str_id));
    try testing.expectEqual(i64_type, store.typeOf(list_id));
    try testing.expectEqual(bool_type, store.patternTypeOf(wild_id));

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

    try testing.expectEqual(Monotype.Prim.bool, store.getMonotype(store.primIdx(.bool)).prim);
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
    const ret = store.primIdx(.bool);

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
        .{ .name = Ident.Idx.NONE, .type_idx = field1_type },
        .{ .name = Ident.Idx.NONE, .type_idx = field2_type },
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
        .{ .name = Ident.Idx.NONE, .payloads = payload_span },
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
        .bool, .str,
        .u8,   .i8,
        .u16,  .i16,
        .u32,  .i32,
        .u64,  .i64,
        .u128, .i128,
        .f32,  .f64,
        .dec,
    };

    for (prims) |p| {
        const idx = store.primIdx(p);
        try testing.expectEqual(p, store.getMonotype(idx).prim);
    }
}

// --- Symbol tests ---

test "Symbol: equality and hashing" {
    const s1 = MIR.Symbol{ .module_idx = 0, .ident_idx = Ident.Idx.NONE };
    const s2 = MIR.Symbol{ .module_idx = 0, .ident_idx = Ident.Idx.NONE };
    const s3 = MIR.Symbol{ .module_idx = 1, .ident_idx = Ident.Idx.NONE };

    try testing.expect(s1.eql(s2));
    try testing.expect(!s1.eql(s3));
}

test "Symbol: none sentinel" {
    const none = MIR.Symbol.none;
    try testing.expect(none.isNone());

    const some = MIR.Symbol{ .module_idx = 0, .ident_idx = Ident.Idx.NONE };
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

    var lower = try Lower.init(
        test_allocator,
        &store,
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

test "lowerExpr: lambda" {
    var env = try MirTestEnv.initFull("Test",
        \\main : U64 -> U64
        \\main = |x| x
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .lambda);
    try testing.expectEqual(@as(u16, 1), result.lambda.params.len);
}

test "lowerExpr: record field call lowers to call(record_access(...), ...)" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    y = 10
        \\    rec = { f: |x| x + y }
        \\    rec.f(5)
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const top = env.mir_store.getExpr(expr);
    try testing.expect(top == .block);

    const final_expr = env.mir_store.getExpr(top.block.final_expr);
    try testing.expect(final_expr == .call);

    const call_func = env.mir_store.getExpr(final_expr.call.func);
    try testing.expect(call_func == .record_access);

    const func_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(final_expr.call.func));
    try testing.expect(func_mono == .func);
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

// --- Gap #26: lowerExternalDef recursion guard ---

test "lowerExternalDef: recursion guard returns lookup placeholder" {
    var env = try MirTestEnv.initModule("Test",
        \\my_val = 42
    );
    defer env.deinit();
    const def_info = try env.getDefExprByName("my_val");
    const symbol = MIR.Symbol{ .module_idx = 1, .ident_idx = def_info.ident_idx };

    // Manually insert the symbol into in_progress_defs to simulate recursion
    const symbol_key: u64 = @bitCast(symbol);
    try env.lower.in_progress_defs.put(symbol_key, {});

    const result = try env.lower.lowerExternalDef(symbol, def_info.expr_idx);
    const expr = env.mir_store.getExpr(result);
    // The recursion guard should return a lookup placeholder
    try testing.expect(expr == .lookup);
    // Initially unit_idx; in real usage, lowerExternalDef patches this to the
    // resolved monotype after lowerExpr completes.
    try testing.expectEqual(env.mir_store.monotype_store.unit_idx, env.mir_store.typeOf(result));
}

test "lowerExternalDef: caching returns same ExprId on second call" {
    var env = try MirTestEnv.initModule("Test",
        \\my_val = 42
    );
    defer env.deinit();
    const def_info = try env.getDefExprByName("my_val");
    const symbol = MIR.Symbol{ .module_idx = 1, .ident_idx = def_info.ident_idx };

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

    const pat0 = env.mir_store.getPattern(first_branch_patterns[0].pattern);
    const pat1 = env.mir_store.getPattern(first_branch_patterns[1].pattern);
    try testing.expect(pat0 == .tag);
    try testing.expect(pat1 == .tag);

    const pat0_args = env.mir_store.getPatternSpan(pat0.tag.args);
    const pat1_args = env.mir_store.getPatternSpan(pat1.tag.args);
    try testing.expectEqual(@as(usize, 1), pat0_args.len);
    try testing.expectEqual(@as(usize, 1), pat1_args.len);

    const arg0 = pat0_args[0];
    const arg1 = pat1_args[0];
    const arg0_pat = env.mir_store.getPattern(arg0);
    const arg1_pat = env.mir_store.getPattern(arg1);
    try testing.expect(arg0_pat == .bind);
    try testing.expect(arg1_pat == .bind);
    try testing.expect(arg0_pat.bind.eql(arg1_pat.bind));

    const arg0_mono = env.mir_store.patternTypeOf(arg0);
    const arg1_mono = env.mir_store.patternTypeOf(arg1);
    try testing.expectEqual(arg0_mono, arg1_mono);
    try testing.expect(env.mir_store.monotype_store.getMonotype(arg0_mono) != .unit);

    const body_expr = env.mir_store.getExpr(branches[0].body);
    try testing.expect(body_expr == .lookup);
    const body_mono = env.mir_store.typeOf(branches[0].body);
    try testing.expectEqual(arg0_mono, body_mono);
}

test "lowerExpr: match alternatives keep payload bind monotypes for Err scrutinee" {
    var env = try MirTestEnv.initExpr(
        \\match Err(42) { Ok(x) | Err(x) => x, _ => 0 }
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .match_expr);

    const branches = env.mir_store.getBranches(result.match_expr.branches);
    try testing.expect(branches.len >= 1);
    const first_branch_patterns = env.mir_store.getBranchPatterns(branches[0].patterns);
    try testing.expect(first_branch_patterns.len >= 2);

    const pat0 = env.mir_store.getPattern(first_branch_patterns[0].pattern);
    const pat1 = env.mir_store.getPattern(first_branch_patterns[1].pattern);
    try testing.expect(pat0 == .tag);
    try testing.expect(pat1 == .tag);

    const pat0_args = env.mir_store.getPatternSpan(pat0.tag.args);
    const pat1_args = env.mir_store.getPatternSpan(pat1.tag.args);
    try testing.expectEqual(@as(usize, 1), pat0_args.len);
    try testing.expectEqual(@as(usize, 1), pat1_args.len);

    const arg0 = pat0_args[0];
    const arg1 = pat1_args[0];
    const arg0_pat = env.mir_store.getPattern(arg0);
    const arg1_pat = env.mir_store.getPattern(arg1);
    try testing.expect(arg0_pat == .bind);
    try testing.expect(arg1_pat == .bind);
    try testing.expect(arg0_pat.bind.eql(arg1_pat.bind));

    const arg0_mono = env.mir_store.patternTypeOf(arg0);
    const arg1_mono = env.mir_store.patternTypeOf(arg1);
    try testing.expectEqual(arg0_mono, arg1_mono);

    const body_expr = env.mir_store.getExpr(branches[0].body);
    try testing.expect(body_expr == .lookup);
    const body_mono = env.mir_store.typeOf(branches[0].body);
    try testing.expectEqual(arg0_mono, body_mono);
    try testing.expect(env.mir_store.monotype_store.getMonotype(arg0_mono) != .unit);
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
    try testing.expect(env.mir_store.getExpr(result.block.final_expr) == .tuple_access);
}

test "lowerExpr: closure forwarding keeps inner/outer symbols callable" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    y = 5
        \\    inner = |x| x + y
        \\    outer = |x| inner(x)
        \\    outer(10)
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const top = env.mir_store.getExpr(expr);
    try testing.expect(top == .block);

    const stmts = env.mir_store.getStmts(top.block.stmts);
    try testing.expect(stmts.len >= 3);

    var inner_sym: ?MIR.Symbol = null;
    var outer_sym: ?MIR.Symbol = null;

    for (stmts) |stmt| {
        const binding = switch (stmt) {
            .decl_const => |b| b,
            .decl_var, .mutate_var => continue,
        };
        const pat = env.mir_store.getPattern(binding.pattern);
        if (pat != .bind) continue;

        const rhs = env.mir_store.getExpr(binding.expr);
        if (rhs == .lambda) {
            const rhs_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(binding.expr));
            try testing.expect(rhs_mono == .func);

            const lam = rhs.lambda;
            const captures = env.mir_store.getCaptures(lam.captures);
            if (captures.len == 1) {
                const body = env.mir_store.getExpr(lam.body);
                if (body == .call) {
                    // outer = |x| inner(x)
                    outer_sym = pat.bind;
                    const fn_expr = env.mir_store.getExpr(body.call.func);
                    try testing.expect(fn_expr == .lookup);
                    inner_sym = fn_expr.lookup;
                } else {
                    // inner = |x| x + y
                    inner_sym = pat.bind;
                }
            }
        }
    }

    try testing.expect(inner_sym != null);
    try testing.expect(outer_sym != null);
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
    // This exercises lowerDotAccess (not lowerBinop) which was missing ensureMethodLowered
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
    const func = env_b.mir_store.getExpr(result.call.func);
    try testing.expect(func == .lookup);
    const method_sym = func.lookup;

    // The method is from module A (idx 1), not module B (idx 2)
    try testing.expect(method_sym.module_idx != 2);

    // The cross-module method body must have been lowered into the MIR store
    try testing.expect(env_b.mir_store.getSymbolDef(method_sym) != null);
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

test "Bool diagnostic MIR: Bool.True lowers to tag with prim.bool" {
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
    // Monotype should be prim.bool
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
}

test "Bool diagnostic MIR: Bool.False lowers to tag with prim.bool" {
    var env = try MirTestEnv.initExpr("Bool.False");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .tag);
    try testing.expectEqual(@as(u16, 0), result.tag.args.len);
    const tag_name = env.module_env.getIdent(result.tag.name);
    try testing.expectEqualStrings("False", tag_name);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
}

test "Bool diagnostic MIR: Bool.not(True) lowers with prim.bool type" {
    var env = try MirTestEnv.initExpr("Bool.not(True)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // Bool.not may lower as a cross-module call or inlined match
    try testing.expect(result != .runtime_err_type);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
}

test "Bool diagnostic MIR: Bool.not(False) lowers with prim.bool type" {
    var env = try MirTestEnv.initExpr("Bool.not(False)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result != .runtime_err_type);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
}

test "Bool diagnostic MIR: !Bool.True lowers to match_expr with prim.bool" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = !Bool.True
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    // !Bool.True desugars via negBool to a match expression
    try testing.expect(env.mir_store.getExpr(expr) == .match_expr);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
}

test "Bool diagnostic MIR: !Bool.False lowers to match_expr with prim.bool" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = !Bool.False
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    try testing.expect(env.mir_store.getExpr(expr) == .match_expr);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
}

test "Bool diagnostic MIR: Bool.True and Bool.False lowers to match_expr with prim.bool" {
    var env = try MirTestEnv.initExpr("Bool.True and Bool.False");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // `and` short-circuit desugars to a match expression
    try testing.expect(result == .match_expr);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
}

test "Bool diagnostic MIR: !Bool.True or !Bool.True lowers with prim.bool" {
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
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
}

test "Bool diagnostic MIR: lambda negation applied to Bool.True lowers with prim.bool" {
    var env = try MirTestEnv.initExpr("(|x| !x)(Bool.True)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // A lambda call should produce a .call expression
    try testing.expect(result != .runtime_err_type);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
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

    // Check condition monotype is prim.bool
    const cond_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(result.match_expr.cond));
    try testing.expect(cond_mono == .prim);
    try testing.expectEqual(Monotype.Prim.bool, cond_mono.prim);

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
    // Pattern monotype should be prim.bool
    const pat0_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.patternTypeOf(bp0[0].pattern));
    try testing.expect(pat0_mono == .prim);
    try testing.expectEqual(Monotype.Prim.bool, pat0_mono.prim);
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
    try testing.expect(cond_mono == .prim);
    try testing.expectEqual(Monotype.Prim.bool, cond_mono.prim);

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

test "Bool.not MIR: Bool.not(True) is a call with prim.bool return type" {
    var env = try MirTestEnv.initExpr("Bool.not(True)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    // Bool.not(True) should be a call expression (cross-module method call)
    try testing.expect(result == .call);
    // Return type should be prim.bool
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
    // The argument should be a tag (True) with prim.bool type
    const args = env.mir_store.getExprSpan(result.call.args);
    try testing.expectEqual(@as(usize, 1), args.len);
    const arg = env.mir_store.getExpr(args[0]);
    try testing.expect(arg == .tag);
    try testing.expectEqualStrings("True", env.module_env.getIdent(arg.tag.name));
    const arg_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(args[0]));
    try testing.expect(arg_mono == .prim);
    try testing.expectEqual(Monotype.Prim.bool, arg_mono.prim);
}

test "Bool.not MIR: Bool.not(False) is a call with prim.bool return type" {
    var env = try MirTestEnv.initExpr("Bool.not(False)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .call);
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
    const args = env.mir_store.getExprSpan(result.call.args);
    try testing.expectEqual(@as(usize, 1), args.len);
    const arg = env.mir_store.getExpr(args[0]);
    try testing.expect(arg == .tag);
    try testing.expectEqualStrings("False", env.module_env.getIdent(arg.tag.name));
    const arg_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(args[0]));
    try testing.expect(arg_mono == .prim);
    try testing.expectEqual(Monotype.Prim.bool, arg_mono.prim);
}

test "Bool.not MIR: callee symbol lowers to lambda with match body" {
    var env = try MirTestEnv.initExpr("Bool.not(True)");
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const call_expr = env.mir_store.getExpr(expr);
    try testing.expect(call_expr == .call);

    const func_expr = env.mir_store.getExpr(call_expr.call.func);
    try testing.expect(func_expr == .lookup);

    const callee_def_id = env.mir_store.getSymbolDef(func_expr.lookup) orelse return error.TestUnexpectedResult;
    const callee_def = env.mir_store.getExpr(callee_def_id);
    try testing.expect(callee_def == .lambda);

    const body = env.mir_store.getExpr(callee_def.lambda.body);
    try testing.expect(body == .match_expr);
    const branches = env.mir_store.getBranches(body.match_expr.branches);
    try testing.expectEqual(@as(usize, 2), branches.len);

    const bp0 = env.mir_store.getBranchPatterns(branches[0].patterns);
    const pat0 = env.mir_store.getPattern(bp0[0].pattern);
    try testing.expect(pat0 == .tag);
    try testing.expectEqualStrings("True", env.module_env.getIdent(pat0.tag.name));

    const body0 = env.mir_store.getExpr(branches[0].body);
    try testing.expect(body0 == .tag);
    try testing.expectEqualStrings("False", env.module_env.getIdent(body0.tag.name));
}

// --- Nominal Bool vs structural tag union MIR tests ---
// CRITICAL DISTINCTION: Bare tags like `True` and `False` without a Bool annotation
// must lower to `.tag_union` monotype, NOT `prim.bool`. Only nominal `Bool` (via
// qualified `Bool.True` or explicit `: Bool` annotation) gets `prim.bool`.
// See also: corresponding type-checking tests in type_checking_integration.zig.

test "Nominal Bool MIR: annotated True lowers with prim.bool" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = True
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
}

test "Nominal Bool MIR: annotated False lowers with prim.bool" {
    var env = try MirTestEnv.initFull("Test",
        \\main : Bool
        \\main = False
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .prim);
    try testing.expectEqual(Monotype.Prim.bool, monotype.prim);
}

test "Structural tag MIR: bare True lowers as tag_union not prim.bool" {
    var env = try MirTestEnv.initExpr("True");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Structural tag MIR: bare False lowers as tag_union not prim.bool" {
    var env = try MirTestEnv.initExpr("False");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const monotype = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(expr));
    try testing.expect(monotype == .tag_union);
}

test "Structural tag MIR: if True True else False lowers as tag_union not prim.bool" {
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
    const func_expr = env.mir_store.getExpr(top.call.func);
    try testing.expect(func_expr == .lookup);

    const func_monotype_idx = env.mir_store.typeOf(top.call.func);
    const func_mono = env.mir_store.monotype_store.getMonotype(func_monotype_idx);
    try testing.expect(func_mono == .func);

    // The function's return type must be List(U32), not List(unit)
    const ret_mono = env.mir_store.monotype_store.getMonotype(func_mono.func.ret);
    try testing.expect(ret_mono == .list);

    const elem_mono = env.mir_store.monotype_store.getMonotype(ret_mono.list.elem);
    try testing.expectEqual(Monotype.Monotype{ .prim = .u32 }, elem_mono);

    // Verify the function DEFINITION body was also lowered with concrete types.
    const method_symbol = func_expr.lookup;
    const method_env = env.lower.all_module_envs[method_symbol.module_idx];
    try testing.expectEqualStrings("Builtin.Num.U32.to", method_env.getIdent(method_symbol.ident_idx));
    const sym_key: u64 = @bitCast(method_symbol);
    const def_expr_id = env.mir_store.symbol_defs.get(sym_key) orelse
        return error.TestUnexpectedResult;
    const def_mono_idx = env.mir_store.typeOf(def_expr_id);
    const def_mono = env.mir_store.monotype_store.getMonotype(def_mono_idx);
    try testing.expect(def_mono == .func);
    const def_ret_mono = env.mir_store.monotype_store.getMonotype(def_mono.func.ret);
    try testing.expect(def_ret_mono == .list);
    const def_elem = env.mir_store.monotype_store.getMonotype(def_ret_mono.list.elem);
    try testing.expectEqual(Monotype.Monotype{ .prim = .u32 }, def_elem);
}

test "cross-module type resolution: U32.to body calls concrete U32 range_to" {
    var env = try MirTestEnv.initExpr("1.U32.to(5.U32)");
    defer env.deinit();
    const expr = try env.lowerFirstDef();

    const top = env.mir_store.getExpr(expr);
    try testing.expect(top == .call);

    const to_lookup_expr = env.mir_store.getExpr(top.call.func);
    try testing.expect(to_lookup_expr == .lookup);
    const to_symbol = to_lookup_expr.lookup;
    const to_sym_key: u64 = @bitCast(to_symbol);
    const to_def_expr_id = env.mir_store.symbol_defs.get(to_sym_key) orelse
        return error.TestUnexpectedResult;

    const to_def_expr = env.mir_store.getExpr(to_def_expr_id);
    try testing.expect(to_def_expr == .lambda);

    const to_body_expr = env.mir_store.getExpr(to_def_expr.lambda.body);
    try testing.expect(to_body_expr == .call);

    const range_lookup_expr = env.mir_store.getExpr(to_body_expr.call.func);
    try testing.expect(range_lookup_expr == .lookup);
    const range_symbol = range_lookup_expr.lookup;

    const range_func_mono_idx = env.mir_store.typeOf(to_body_expr.call.func);
    const range_func_mono = env.mir_store.monotype_store.getMonotype(range_func_mono_idx);
    try testing.expect(range_func_mono == .func);

    const range_arg_monos = env.mir_store.monotype_store.getIdxSpan(range_func_mono.func.args);
    try testing.expectEqual(@as(usize, 2), range_arg_monos.len);
    try testing.expectEqual(Monotype.Monotype{ .prim = .u32 }, env.mir_store.monotype_store.getMonotype(range_arg_monos[0]));
    try testing.expectEqual(Monotype.Monotype{ .prim = .u32 }, env.mir_store.monotype_store.getMonotype(range_arg_monos[1]));

    const range_ret_mono = env.mir_store.monotype_store.getMonotype(range_func_mono.func.ret);
    try testing.expect(range_ret_mono == .list);
    try testing.expectEqual(Monotype.Monotype{ .prim = .u32 }, env.mir_store.monotype_store.getMonotype(range_ret_mono.list.elem));

    // The lowered definition behind range_to must also be concretely specialized.
    const range_sym_key: u64 = @bitCast(range_symbol);
    const range_def_expr_id = env.mir_store.symbol_defs.get(range_sym_key) orelse
        return error.TestUnexpectedResult;
    const range_def_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(range_def_expr_id));
    try testing.expect(range_def_mono == .func);
    const range_def_args = env.mir_store.monotype_store.getIdxSpan(range_def_mono.func.args);
    try testing.expectEqual(@as(usize, 2), range_def_args.len);
    try testing.expectEqual(Monotype.Monotype{ .prim = .u32 }, env.mir_store.monotype_store.getMonotype(range_def_args[0]));
    try testing.expectEqual(Monotype.Monotype{ .prim = .u32 }, env.mir_store.monotype_store.getMonotype(range_def_args[1]));
    const range_def_ret = env.mir_store.monotype_store.getMonotype(range_def_mono.func.ret);
    try testing.expect(range_def_ret == .list);
    try testing.expectEqual(Monotype.Monotype{ .prim = .u32 }, env.mir_store.monotype_store.getMonotype(range_def_ret.list.elem));

    var found_u32_list_append = false;
    const Walker = struct {
        fn visit(envp: *MirTestEnv, expr_id: MIR.ExprId, found: *bool) !void {
            const node = envp.mir_store.getExpr(expr_id);
            switch (node) {
                .run_low_level => |ll| {
                    if (ll.op == .list_append or ll.op == .list_append_unsafe) {
                        const ll_args = envp.mir_store.getExprSpan(ll.args);
                        if (ll_args.len == 2) {
                            const ret_mono = envp.mir_store.monotype_store.getMonotype(envp.mir_store.typeOf(expr_id));
                            const arg0_mono = envp.mir_store.monotype_store.getMonotype(envp.mir_store.typeOf(ll_args[0]));
                            const arg1_mono = envp.mir_store.monotype_store.getMonotype(envp.mir_store.typeOf(ll_args[1]));
                            if (ret_mono == .list and arg0_mono == .list and arg1_mono == .prim and arg1_mono.prim == .u32) {
                                const ret_elem = envp.mir_store.monotype_store.getMonotype(ret_mono.list.elem);
                                const arg0_elem = envp.mir_store.monotype_store.getMonotype(arg0_mono.list.elem);
                                if (ret_elem == .prim and arg0_elem == .prim and ret_elem.prim == .u32 and arg0_elem.prim == .u32) {
                                    found.* = true;
                                }
                            }
                        }
                    }
                    const ll_args = envp.mir_store.getExprSpan(ll.args);
                    for (ll_args) |arg| {
                        try visit(envp, arg, found);
                    }
                },
                .block => |b| {
                    const stmts = envp.mir_store.getStmts(b.stmts);
                    for (stmts) |stmt| {
                        switch (stmt) {
                            .decl_const => |binding| try visit(envp, binding.expr, found),
                            .decl_var => |binding| try visit(envp, binding.expr, found),
                            .mutate_var => |binding| try visit(envp, binding.expr, found),
                        }
                    }
                    try visit(envp, b.final_expr, found);
                },
                .call => |c| {
                    const fn_expr = envp.mir_store.getExpr(c.func);
                    if (fn_expr == .lookup) {
                        const fn_symbol = fn_expr.lookup;
                        const fn_env = envp.lower.all_module_envs[fn_symbol.module_idx];
                        const fn_name = fn_env.getIdent(fn_symbol.ident_idx);
                        if (std.mem.endsWith(u8, fn_name, "append")) {
                            const fn_mono = envp.mir_store.monotype_store.getMonotype(envp.mir_store.typeOf(c.func));
                            const call_args = envp.mir_store.getExprSpan(c.args);
                            if (fn_mono == .func and call_args.len == 2) {
                                const ret_mono = envp.mir_store.monotype_store.getMonotype(envp.mir_store.typeOf(expr_id));
                                const arg0_mono = envp.mir_store.monotype_store.getMonotype(envp.mir_store.typeOf(call_args[0]));
                                const arg1_mono = envp.mir_store.monotype_store.getMonotype(envp.mir_store.typeOf(call_args[1]));
                                if (ret_mono == .list and arg0_mono == .list and arg1_mono == .prim and arg1_mono.prim == .u32) {
                                    const ret_elem = envp.mir_store.monotype_store.getMonotype(ret_mono.list.elem);
                                    const arg0_elem = envp.mir_store.monotype_store.getMonotype(arg0_mono.list.elem);
                                    if (ret_elem == .prim and arg0_elem == .prim and ret_elem.prim == .u32 and arg0_elem.prim == .u32) {
                                        found.* = true;
                                    }
                                }
                            }
                        }
                    }
                    try visit(envp, c.func, found);
                    const args = envp.mir_store.getExprSpan(c.args);
                    for (args) |arg| {
                        try visit(envp, arg, found);
                    }
                },
                .lambda => |lam| try visit(envp, lam.body, found),
                .match_expr => |m| {
                    try visit(envp, m.cond, found);
                    const branches = envp.mir_store.getBranches(m.branches);
                    for (branches) |br| {
                        if (!br.guard.isNone()) {
                            try visit(envp, br.guard, found);
                        }
                        try visit(envp, br.body, found);
                    }
                },
                .while_loop => |w| {
                    try visit(envp, w.cond, found);
                    try visit(envp, w.body, found);
                },
                .for_loop => |f| {
                    try visit(envp, f.list, found);
                    try visit(envp, f.body, found);
                },
                .record => |r| {
                    const fields = envp.mir_store.getExprSpan(r.fields);
                    for (fields) |field| {
                        try visit(envp, field, found);
                    }
                },
                .tuple => |t| {
                    const elems = envp.mir_store.getExprSpan(t.elems);
                    for (elems) |elem| {
                        try visit(envp, elem, found);
                    }
                },
                .tag => |t| {
                    const args = envp.mir_store.getExprSpan(t.args);
                    for (args) |arg| {
                        try visit(envp, arg, found);
                    }
                },
                .record_access => |ra| try visit(envp, ra.record, found),
                .tuple_access => |ta| try visit(envp, ta.tuple, found),
                .hosted => |h| try visit(envp, h.body, found),
                .dbg_expr => |d| try visit(envp, d.expr, found),
                .expect => |e| try visit(envp, e.body, found),
                .return_expr => |r| try visit(envp, r.expr, found),
                else => {},
            }
        }
    };
    try Walker.visit(&env, range_def_expr_id, &found_u32_list_append);
    try testing.expect(found_u32_list_append);
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

    // The lambda itself (first stmt's expr) should have params typed as U64
    const stmts = env.mir_store.getStmts(result.block.stmts);
    try testing.expect(stmts.len >= 1);
    const decl_expr = env.mir_store.getExpr(stmts[0].decl_const.expr);
    try testing.expect(decl_expr == .lambda);
    const lambda_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(stmts[0].decl_const.expr));
    try testing.expect(lambda_mono == .func);
    // Return type of the lambda must be U64
    const ret_mono = env.mir_store.monotype_store.getMonotype(lambda_mono.func.ret);
    try testing.expect(ret_mono == .prim);
    try testing.expectEqual(Monotype.Prim.u64, ret_mono.prim);
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

test "repl arrow desugaring: full fn0/fn1/fn2/fn3 program lowers to ordinary calls" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    fn0 = |a| a + 1
        \\    fn1 = |a, b| a + b
        \\    fn2 = |a, b, c| a + b + c
        \\    fn3 = |a, b, c, d| a + b + c + d
        \\
        \\    r0 = 10->fn0
        \\    r1 = 10->fn1(20)
        \\    r2 = 10->fn2(20, 30)
        \\    r3 = 10->fn3(20, 30, 40)
        \\
        \\    (r0, r1, r2, r3)
        \\}
    );
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    const top = env.mir_store.getExpr(expr);
    try testing.expect(top == .block);

    const stmts = env.mir_store.getStmts(top.block.stmts);
    try testing.expectEqual(@as(usize, 8), stmts.len);

    const expected_plus_call_counts = [_]usize{ 1, 1, 2, 3 };
    const expected_call_arg_lens = [_]usize{ 1, 2, 3, 4 };
    var fn_symbols: [4]MIR.Symbol = undefined;

    // fn0..fn3 definitions: each should be a lambda whose `+` chain lowers as call(s)
    for (0..4) |i| {
        const stmt = stmts[i];
        try testing.expect(stmt == .decl_const);
        const dc = stmt.decl_const;

        const pat = env.mir_store.getPattern(dc.pattern);
        try testing.expect(pat == .bind);
        fn_symbols[i] = pat.bind;

        const def_expr = env.mir_store.getExpr(dc.expr);
        try testing.expect(def_expr == .lambda);

        const call_count = try struct {
            fn count(env_: *MirTestEnv, expr_id: MIR.ExprId) !usize {
                const e = env_.mir_store.getExpr(expr_id);
                return switch (e) {
                    .call => |c| blk: {
                        const fn_expr = env_.mir_store.getExpr(c.func);
                        try testing.expect(fn_expr == .lookup);
                        const args = env_.mir_store.getExprSpan(c.args);
                        try testing.expectEqual(@as(usize, 2), args.len);

                        // For this test, default numerals in `+` should resolve to Dec.
                        const ret_mono = env_.mir_store.monotype_store.getMonotype(env_.mir_store.typeOf(expr_id));
                        try testing.expect(ret_mono == .prim);
                        try testing.expectEqual(Monotype.Prim.dec, ret_mono.prim);

                        var total: usize = 1;
                        for (args) |arg| {
                            if (env_.mir_store.getExpr(arg) == .call) {
                                total += try count(env_, arg);
                            }
                        }
                        break :blk total;
                    },
                    else => error.TestUnexpectedResult,
                };
            }
        }.count(&env, def_expr.lambda.body);

        try testing.expectEqual(expected_plus_call_counts[i], call_count);
    }

    // r0..r3: each arrow form should desugar to a plain function call.
    for (0..4) |i| {
        const stmt = stmts[4 + i];
        try testing.expect(stmt == .decl_const);
        const dc = stmt.decl_const;
        const call_expr = env.mir_store.getExpr(dc.expr);
        try testing.expect(call_expr == .call);

        const fn_expr = env.mir_store.getExpr(call_expr.call.func);
        try testing.expect(fn_expr == .lookup);
        try testing.expectEqual(fn_symbols[i], fn_expr.lookup);

        const call_args = env.mir_store.getExprSpan(call_expr.call.args);
        try testing.expectEqual(expected_call_arg_lens[i], call_args.len);

        const ret_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(dc.expr));
        try testing.expect(ret_mono == .prim);
        try testing.expectEqual(Monotype.Prim.dec, ret_mono.prim);
    }

    // Final expression is the tuple of the four call results.
    const final_expr = env.mir_store.getExpr(top.block.final_expr);
    try testing.expect(final_expr == .tuple);
    const final_elems = env.mir_store.getExprSpan(final_expr.tuple.elems);
    try testing.expectEqual(@as(usize, 4), final_elems.len);
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

    // Check the lambda body
    const stmts = env.mir_store.getStmts(result.block.stmts);
    try testing.expect(stmts.len >= 1);
    const decl_expr = env.mir_store.getExpr(stmts[0].decl_const.expr);
    try testing.expect(decl_expr == .lambda);

    // Lambda return must be U64
    const lambda_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(stmts[0].decl_const.expr));
    try testing.expect(lambda_mono == .func);
    const ret_mono = env.mir_store.monotype_store.getMonotype(lambda_mono.func.ret);
    try testing.expectEqual(Monotype.Prim.u64, ret_mono.prim);

    // Check that the lambda body's subexpressions are all U64, not Dec
    // The body is `a + b + 0` which desugars to `(a + b) + 0`
    // The body is either a call or run_low_level for the outer `+`
    const body = env.mir_store.getExpr(decl_expr.lambda.body);
    const body_mono = env.mir_store.monotype_store.getMonotype(env.mir_store.typeOf(decl_expr.lambda.body));
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

test "polymorphic block call specialization: clone_via_fold gets distinct call monotypes" {
    var env = try MirTestEnv.initExpr(
        \\{
        \\    append_one = |acc, x| List.append(acc, x)
        \\    clone_via_fold = |xs| xs.fold(List.with_capacity(1), append_one)
        \\    first_len = clone_via_fold([1i64, 2i64]).len()
        \\    clone_via_fold([[1i64, 2i64], [3i64, 4i64]]).len()
        \\}
    );
    defer env.deinit();

    const expr = try env.lowerFirstDef();
    const top = env.mir_store.getExpr(expr);
    try testing.expect(top == .block);

    const stmts = env.mir_store.getStmts(top.block.stmts);
    try testing.expect(stmts.len >= 3);
    const first_len_expr = stmts[2].decl_const.expr;
    const final_expr = top.block.final_expr;

    const CallInfo = struct {
        symbol: MIR.Symbol,
        func_mono: Monotype.Idx,
    };

    const Finder = struct {
        fn find(envp: *MirTestEnv, expr_id: MIR.ExprId) ?CallInfo {
            const expr_node = envp.mir_store.getExpr(expr_id);
            switch (expr_node) {
                .call => |call_node| {
                    const func_mono_idx = envp.mir_store.typeOf(call_node.func);
                    const func_mono = envp.mir_store.monotype_store.getMonotype(func_mono_idx);
                    if (func_mono == .func) {
                        const args_mono = envp.mir_store.monotype_store.getIdxSpan(func_mono.func.args);
                        const ret_mono = envp.mir_store.monotype_store.getMonotype(func_mono.func.ret);
                        if (args_mono.len == 1 and ret_mono == .list) {
                            const fn_expr = envp.mir_store.getExpr(call_node.func);
                            if (fn_expr == .lookup) {
                                return .{
                                    .symbol = fn_expr.lookup,
                                    .func_mono = func_mono_idx,
                                };
                            }
                        }
                    }

                    if (find(envp, call_node.func)) |found| return found;
                    const args = envp.mir_store.getExprSpan(call_node.args);
                    for (args) |arg| {
                        if (find(envp, arg)) |found| return found;
                    }
                },
                .block => |block_node| {
                    const block_stmts = envp.mir_store.getStmts(block_node.stmts);
                    for (block_stmts) |stmt| {
                        switch (stmt) {
                            .decl_const => |dc| {
                                if (find(envp, dc.expr)) |found| return found;
                            },
                            .decl_var => |dv| {
                                if (find(envp, dv.expr)) |found| return found;
                            },
                            .mutate_var => |mv| {
                                if (find(envp, mv.expr)) |found| return found;
                            },
                        }
                    }
                    if (find(envp, block_node.final_expr)) |found| return found;
                },
                .match_expr => |match_node| {
                    if (find(envp, match_node.cond)) |found| return found;
                    const branches = envp.mir_store.getBranches(match_node.branches);
                    for (branches) |branch| {
                        if (!branch.guard.isNone()) {
                            if (find(envp, branch.guard)) |found| return found;
                        }
                        if (find(envp, branch.body)) |found| return found;
                    }
                },
                .tuple => |tuple_node| {
                    const elems = envp.mir_store.getExprSpan(tuple_node.elems);
                    for (elems) |elem| {
                        if (find(envp, elem)) |found| return found;
                    }
                },
                .record => |record_node| {
                    const fields = envp.mir_store.getExprSpan(record_node.fields);
                    for (fields) |field_expr| {
                        if (find(envp, field_expr)) |found| return found;
                    }
                },
                .tag => |tag_node| {
                    const args = envp.mir_store.getExprSpan(tag_node.args);
                    for (args) |arg| {
                        if (find(envp, arg)) |found| return found;
                    }
                },
                .record_access => |ra| return find(envp, ra.record),
                .tuple_access => |ta| return find(envp, ta.tuple),
                .lambda => |lam| return find(envp, lam.body),
                .for_loop => |fl| {
                    if (find(envp, fl.list)) |found| return found;
                    if (find(envp, fl.body)) |found| return found;
                },
                .while_loop => |wl| {
                    if (find(envp, wl.cond)) |found| return found;
                    if (find(envp, wl.body)) |found| return found;
                },
                .dbg_expr => |dbg_node| return find(envp, dbg_node.expr),
                .expect => |expect_node| return find(envp, expect_node.body),
                .return_expr => |ret_node| return find(envp, ret_node.expr),
                .run_low_level => |ll| {
                    const args = envp.mir_store.getExprSpan(ll.args);
                    for (args) |arg| {
                        if (find(envp, arg)) |found| return found;
                    }
                },
                else => {},
            }
            return null;
        }
    };

    const call_a = Finder.find(&env, first_len_expr) orelse return error.TestUnexpectedResult;
    const call_b = Finder.find(&env, final_expr) orelse return error.TestUnexpectedResult;

    try testing.expect(call_a.func_mono != call_b.func_mono);
    try testing.expect(call_a.symbol != call_b.symbol);
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
