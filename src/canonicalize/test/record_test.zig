//! Tests for Records

const std = @import("std");
const parse = @import("parse");
const base = @import("base");
const ModuleEnv = @import("../ModuleEnv.zig");
const Can = @import("../Can.zig");
const BuiltinTestContext = @import("./BuiltinTestContext.zig").BuiltinTestContext;

const CoreCtx = @import("ctx").CoreCtx;
const Ident = base.Ident;

test "record literal uses record_unbound" {
    const gpa = std.testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(gpa);
    defer builtin_ctx.deinit();

    // Test a simple record literal
    {
        const source = "{ x: 42, y: \"hello\" }";

        var env = try ModuleEnv.init(gpa, source);
        defer env.deinit();

        try env.initCIRFields("test");

        const roc_ctx = CoreCtx.testing(gpa, gpa);

        const ast = try parse.expr(gpa, &env.common);
        defer ast.deinit();

        var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            return error.CanonicalizeError;
        };

        const canonical_expr = env.store.getExpr(canonical_expr_idx.idx);
        // Check that it's a record
        if (canonical_expr != .e_record) return error.ExpectedRecord;
        try std.testing.expect(canonical_expr.e_record.fields.span.len == 2);
    }

    // Test an empty record literal
    {
        const source2 = "{}";

        var env = try ModuleEnv.init(gpa, source2);
        defer env.deinit();

        try env.initCIRFields("test");

        const roc_ctx = CoreCtx.testing(gpa, gpa);

        const ast = try parse.expr(gpa, &env.common);
        defer ast.deinit();

        var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            return error.CanonicalizeError;
        };

        const canonical_expr = env.store.getExpr(canonical_expr_idx.idx);
        // Check that it's an empty_record
        if (canonical_expr != .e_empty_record) return error.ExpectedEmptyRecord;
    }

    // Test a record with a single field
    // Test a nested record literal
    {
        const source3 = "{ value: 123 }";

        var env = try ModuleEnv.init(gpa, source3);
        defer env.deinit();

        try env.initCIRFields("test");

        const roc_ctx = CoreCtx.testing(gpa, gpa);

        const ast = try parse.expr(gpa, &env.common);
        defer ast.deinit();

        var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
        defer can.deinit();

        const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
            return error.CanonicalizeError;
        };

        const canonical_expr = env.store.getExpr(canonical_expr_idx.idx);
        // Check that it's a record
        if (canonical_expr != .e_record) return error.ExpectedRecord;
        const record = canonical_expr.e_record;
        try std.testing.expect(record.fields.span.len == 1);

        const cir_fields = env.store.sliceRecordFields(record.fields);
        const cir_field = env.store.getRecordField(cir_fields[0]);
        const field_name = env.getIdent(cir_field.name);
        try std.testing.expectEqualStrings("value", field_name);
    }
}

test "record pattern destructuring" {
    const gpa = std.testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(gpa);
    defer builtin_ctx.deinit();

    // Test simple record destructuring: { x, y } = { x: 1, y: 2 }
    const source = "{ x, y } = { x: 1, y: 2 }";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    const roc_ctx = CoreCtx.testing(gpa, gpa);

    const ast = try parse.statement(gpa, &env.common);
    defer ast.deinit();

    var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer can.deinit();

    // Enter a function scope so we can have local bindings
    try can.scopeEnter(gpa, true);

    const stmt_idx: parse.AST.Statement.Idx = @enumFromInt(ast.root_node_idx);
    const stmt = ast.store.getStatement(stmt_idx);

    // The statement should be a declaration
    if (stmt != .decl) return error.ExpectedDecl;
    const decl = stmt.decl;
    // Get the pattern from the declaration
    const pattern_idx = decl.pattern;
    const canonical_pattern_idx = try can.canonicalizePattern(pattern_idx) orelse {
        return error.CanonicalizePatternError;
    };

    const canonical_pattern = env.store.getPattern(canonical_pattern_idx);

    // Check that it's a record_destructure pattern
    if (canonical_pattern != .record_destructure) return error.ExpectedRecordDestructure;
    const rd = canonical_pattern.record_destructure;
    // Get the destructs
    const destructs = env.store.sliceRecordDestructs(rd.destructs);
    try std.testing.expect(destructs.len == 2);

    // Check the first destruct (x)
    const destruct_x = env.store.getRecordDestruct(destructs[0]);
    try std.testing.expectEqualStrings("x", env.getIdent(destruct_x.label));

    // Check the second destruct (y)
    const destruct_y = env.store.getRecordDestruct(destructs[1]);
    try std.testing.expectEqualStrings("y", env.getIdent(destruct_y.label));

    // Verify that x and y are now in scope
    const x_ident = try env.insertIdent(Ident.for_text("x"));
    const y_ident = try env.insertIdent(Ident.for_text("y"));

    const x_lookup = can.scopeLookup(.ident, x_ident);
    const y_lookup = can.scopeLookup(.ident, y_ident);

    // Both should be found in scope
    if (x_lookup != .found) return error.XNotInScope;
    if (y_lookup != .found) return error.YNotInScope;
}

test "record pattern with sub-patterns" {
    const gpa = std.testing.allocator;
    var builtin_ctx = try BuiltinTestContext.init(gpa);
    defer builtin_ctx.deinit();

    // Test record destructuring with sub-patterns: { name: n, age: a } = person
    const source = "{ name: n, age: a } = person";

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    const roc_ctx = CoreCtx.testing(gpa, gpa);

    const ast = try parse.statement(gpa, &env.common);
    defer ast.deinit();

    var can = try Can.initModule(roc_ctx, &env, ast, builtin_ctx.canInitContext());
    defer can.deinit();

    // Enter a function scope so we can have local bindings
    try can.scopeEnter(gpa, true);

    const stmt_idx: parse.AST.Statement.Idx = @enumFromInt(ast.root_node_idx);
    const stmt = ast.store.getStatement(stmt_idx);

    // The statement should be a declaration
    if (stmt != .decl) return error.ExpectedDecl;
    const decl = stmt.decl;
    // Get the pattern from the declaration
    const pattern_idx = decl.pattern;
    const canonical_pattern_idx = try can.canonicalizePattern(pattern_idx) orelse {
        return error.CanonicalizePatternError;
    };

    const canonical_pattern = env.store.getPattern(canonical_pattern_idx);

    // Check that it's a record_destructure pattern
    if (canonical_pattern != .record_destructure) return error.ExpectedRecordDestructure;
    const rd = canonical_pattern.record_destructure;
    // Get the destructs
    const destructs = env.store.sliceRecordDestructs(rd.destructs);
    try std.testing.expect(destructs.len == 2);

    // Check the first destruct (name: n)
    const destruct_name = env.store.getRecordDestruct(destructs[0]);
    try std.testing.expectEqualStrings("name", env.getIdent(destruct_name.label));
    // The ident should be the sub-pattern variable name
    try std.testing.expectEqualStrings("name", env.getIdent(destruct_name.ident));
    // Should have a SubPattern kind
    if (destruct_name.kind != .SubPattern) return error.ExpectedSubPattern;

    // Check the second destruct (age: a)
    const destruct_age = env.store.getRecordDestruct(destructs[1]);
    try std.testing.expectEqualStrings("age", env.getIdent(destruct_age.label));

    // Verify that n and a are now in scope (the sub-pattern bindings)
    const n_ident = try env.insertIdent(Ident.for_text("n"));
    const a_ident = try env.insertIdent(Ident.for_text("a"));

    const n_lookup = can.scopeLookup(.ident, n_ident);
    const a_lookup = can.scopeLookup(.ident, a_ident);

    // Both should be found in scope
    if (n_lookup != .found) return error.NNotInScope;
    if (a_lookup != .found) return error.ANotInScope;
}
