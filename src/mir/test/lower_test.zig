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

const CIR = can.CIR;
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
        std.mem.zeroes(CIR.BuiltinIndices),
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

test "fromTypeVar: list resolves to valid monotype" {
    var env = try MirTestEnv.initExpr("[1.I64, 2.I64, 3.I64]");
    defer env.deinit();
    const expr = try env.lowerFirstDef();
    // The expression itself is a list
    const result = env.mir_store.getExpr(expr);
    try testing.expect(result == .list);
    try testing.expectEqual(@as(u16, 3), result.list.elems.len);
    // The monotype should be resolved (not unit placeholder)
    const type_idx = env.mir_store.typeOf(expr);
    try testing.expect(!type_idx.isNone());
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
