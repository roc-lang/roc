//! Tests for MIR Store, Monotype Store, and Lower initialization.

const std = @import("std");
const testing = std.testing;

const base = @import("base");
const can = @import("can");
const types = @import("types");

const MIR = @import("../MIR.zig");
const Monotype = @import("../Monotype.zig");
const Lower = @import("../Lower.zig");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const Region = base.Region;
const Ident = base.Ident;

const test_allocator = testing.allocator;

// --- MIR Store tests ---

test "MIR Store: add and get expression" {
    var store = MIR.Store.init();
    defer store.deinit(test_allocator);

    const monotype = try store.monotype_store.addMonotype(test_allocator, .{ .prim = .i64 });
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
    var store = MIR.Store.init();
    defer store.deinit(test_allocator);

    const monotype = try store.monotype_store.addMonotype(test_allocator, .{ .prim = .bool });
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
    var store = MIR.Store.init();
    defer store.deinit(test_allocator);

    const monotype = try store.monotype_store.addMonotype(test_allocator, .{ .prim = .i64 });
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
    var store = MIR.Store.init();
    defer store.deinit(test_allocator);

    const empty_expr_span = MIR.ExprSpan.empty();
    try testing.expect(empty_expr_span.isEmpty());
    try testing.expectEqual(@as(usize, 0), store.getExprSpan(empty_expr_span).len);

    const empty_pat_span = MIR.PatternSpan.empty();
    try testing.expect(empty_pat_span.isEmpty());
    try testing.expectEqual(@as(usize, 0), store.getPatternSpan(empty_pat_span).len);
}

test "MIR Store: branches" {
    var store = MIR.Store.init();
    defer store.deinit(test_allocator);

    const monotype = try store.monotype_store.addMonotype(test_allocator, .{ .prim = .bool });
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
    var store = MIR.Store.init();
    defer store.deinit(test_allocator);

    const monotype = try store.monotype_store.addMonotype(test_allocator, .{ .prim = .i64 });
    const expr = try store.addExpr(test_allocator, .{ .int = .{
        .value = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 },
    } }, monotype, Region.zero());
    const symbol = MIR.Symbol{ .module_idx = 0, .ident_idx = Ident.Idx.NONE };
    const pat = try store.addPattern(test_allocator, .{ .bind = symbol }, monotype);

    const stmt_span = try store.addStmts(test_allocator, &.{.{ .pattern = pat, .expr = expr }});
    const stmts = store.getStmts(stmt_span);
    try testing.expectEqual(@as(usize, 1), stmts.len);
    try testing.expectEqual(pat, stmts[0].pattern);
    try testing.expectEqual(expr, stmts[0].expr);
}

test "MIR Store: field name spans" {
    var store = MIR.Store.init();
    defer store.deinit(test_allocator);

    const name1 = Ident.Idx.NONE;
    const name2 = Ident.Idx.NONE;

    const span = try store.addFieldNameSpan(test_allocator, &.{ name1, name2 });
    try testing.expectEqual(@as(u16, 2), span.len);

    const retrieved = store.getFieldNameSpan(span);
    try testing.expectEqual(@as(usize, 2), retrieved.len);
}

test "MIR Store: symbol def registration" {
    var store = MIR.Store.init();
    defer store.deinit(test_allocator);

    const monotype = try store.monotype_store.addMonotype(test_allocator, .{ .prim = .i64 });
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
    var store = MIR.Store.init();
    defer store.deinit(test_allocator);

    const i64_type = try store.monotype_store.addMonotype(test_allocator, .{ .prim = .i64 });
    const str_type = try store.monotype_store.addMonotype(test_allocator, .{ .prim = .str });
    const bool_type = try store.monotype_store.addMonotype(test_allocator, .{ .prim = .bool });

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
    var store = Monotype.Store.init();
    defer store.deinit(test_allocator);

    const bool_idx = try store.addMonotype(test_allocator, .{ .prim = .bool });
    const str_idx = try store.addMonotype(test_allocator, .{ .prim = .str });
    const i64_idx = try store.addMonotype(test_allocator, .{ .prim = .i64 });

    try testing.expectEqual(Monotype.Prim.bool, store.getMonotype(bool_idx).prim);
    try testing.expectEqual(Monotype.Prim.str, store.getMonotype(str_idx).prim);
    try testing.expectEqual(Monotype.Prim.i64, store.getMonotype(i64_idx).prim);
}

test "Monotype Store: unit type" {
    var store = Monotype.Store.init();
    defer store.deinit(test_allocator);

    const unit_idx = try store.addMonotype(test_allocator, .unit);

    try testing.expect(store.getMonotype(unit_idx) == .unit);
}

test "Monotype Store: list type" {
    var store = Monotype.Store.init();
    defer store.deinit(test_allocator);

    const elem = try store.addMonotype(test_allocator, .{ .prim = .str });
    const list = try store.addMonotype(test_allocator, .{ .list = .{ .elem = elem } });

    const retrieved = store.getMonotype(list);
    try testing.expectEqual(elem, retrieved.list.elem);
}

test "Monotype Store: func type" {
    var store = Monotype.Store.init();
    defer store.deinit(test_allocator);

    const arg1 = try store.addMonotype(test_allocator, .{ .prim = .i64 });
    const arg2 = try store.addMonotype(test_allocator, .{ .prim = .str });
    const ret = try store.addMonotype(test_allocator, .{ .prim = .bool });

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
    var store = Monotype.Store.init();
    defer store.deinit(test_allocator);

    const field1_type = try store.addMonotype(test_allocator, .{ .prim = .i64 });
    const field2_type = try store.addMonotype(test_allocator, .{ .prim = .str });

    const field_span = try store.addFields(test_allocator, &.{
        .{ .name = Ident.Idx.NONE, .type_idx = field1_type },
        .{ .name = Ident.Idx.NONE, .type_idx = field2_type },
    });
    const record = try store.addMonotype(test_allocator, .{ .record = .{ .fields = field_span } });

    const retrieved = store.getMonotype(record);
    try testing.expect(retrieved == .record);
}

test "Monotype Store: tag union type" {
    var store = Monotype.Store.init();
    defer store.deinit(test_allocator);

    const payload_type = try store.addMonotype(test_allocator, .{ .prim = .str });
    const payload_span = try store.addIdxSpan(test_allocator, &.{payload_type});

    const tag_span = try store.addTags(test_allocator, &.{
        .{ .name = Ident.Idx.NONE, .payloads = payload_span },
    });
    const tag_union = try store.addMonotype(test_allocator, .{ .tag_union = .{ .tags = tag_span } });

    const retrieved = store.getMonotype(tag_union);
    try testing.expect(retrieved == .tag_union);
}

test "Monotype Store: box type" {
    var store = Monotype.Store.init();
    defer store.deinit(test_allocator);

    const inner = try store.addMonotype(test_allocator, .{ .prim = .i64 });
    const boxed = try store.addMonotype(test_allocator, .{ .box = .{ .inner = inner } });

    const retrieved = store.getMonotype(boxed);
    try testing.expectEqual(inner, retrieved.box.inner);
}

test "Monotype Store: tuple type" {
    var store = Monotype.Store.init();
    defer store.deinit(test_allocator);

    const elem1 = try store.addMonotype(test_allocator, .{ .prim = .i64 });
    const elem2 = try store.addMonotype(test_allocator, .{ .prim = .str });

    const elems_span = try store.addIdxSpan(test_allocator, &.{ elem1, elem2 });
    const tuple = try store.addMonotype(test_allocator, .{ .tuple = .{ .elems = elems_span } });

    const retrieved = store.getMonotype(tuple);
    try testing.expect(retrieved == .tuple);
}

test "Monotype Store: all primitive types" {
    var store = Monotype.Store.init();
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
        const idx = try store.addMonotype(test_allocator, .{ .prim = p });
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
    var store = MIR.Store.init();
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
    );
    defer lower.deinit();

    // Verify initial state
    try testing.expectEqual(@as(u32, 0), lower.current_module_idx);
}
