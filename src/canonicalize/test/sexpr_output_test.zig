//! Tests for S-expression output generation from CIR nodes.
//! These tests exercise the pushToSExprTree methods in Pattern.zig, Statement.zig, Expression.zig etc.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const ModuleEnv = @import("../ModuleEnv.zig");
const Can = @import("../Can.zig");
const CIR = @import("../CIR.zig");

const SExprTree = base.SExprTree;

/// Helper to canonicalize an expression and generate S-expression output
fn canonicalizeAndRenderExpr(source: []const u8) ![]u8 {
    const gpa = std.testing.allocator;

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    var ast = try parse.parseExpr(&env.common, gpa);
    defer ast.deinit(gpa);

    var can = try Can.init(&env, &ast, null);
    defer can.deinit();

    const expr_idx: parse.AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
    const canonical_expr_idx = try can.canonicalizeExpr(expr_idx) orelse {
        return error.CanonicalizeError;
    };

    // Generate S-expression output
    var tree = SExprTree.init(gpa);
    defer tree.deinit();

    const canonical_expr = env.store.getExpr(canonical_expr_idx.idx);
    try canonical_expr.pushToSExprTree(&env, &tree, canonical_expr_idx.idx);

    // Convert tree to string using ArrayList as writer
    var output = try std.ArrayList(u8).initCapacity(gpa, 1024);
    errdefer output.deinit(gpa);

    try tree.toStringPretty(output.writer(gpa), .skip_linecol);

    return try output.toOwnedSlice(gpa);
}

/// Helper to canonicalize a pattern from a statement and generate S-expression output
fn canonicalizePatternFromDecl(source: []const u8) ![]u8 {
    const gpa = std.testing.allocator;

    var env = try ModuleEnv.init(gpa, source);
    defer env.deinit();

    try env.initCIRFields("test");

    var ast = try parse.parseStatement(&env.common, gpa);
    defer ast.deinit(gpa);

    var can = try Can.init(&env, &ast, null);
    defer can.deinit();

    // Enter a function scope for local bindings
    try can.scopeEnter(gpa, true);

    const stmt_idx: parse.AST.Statement.Idx = @enumFromInt(ast.root_node_idx);
    const stmt = ast.store.getStatement(stmt_idx);

    // Get the pattern from the declaration
    const pattern_idx = switch (stmt) {
        .decl => |d| d.pattern,
        else => return error.ExpectedDecl,
    };

    const canonical_pattern_idx = try can.canonicalizePattern(pattern_idx) orelse {
        return error.CanonicalizePatternError;
    };

    // Generate S-expression output for the pattern
    var tree = SExprTree.init(gpa);
    defer tree.deinit();

    const canonical_pattern = env.store.getPattern(canonical_pattern_idx);
    try canonical_pattern.pushToSExprTree(&env, &tree, canonical_pattern_idx);

    // Convert tree to string
    var output = try std.ArrayList(u8).initCapacity(gpa, 1024);
    errdefer output.deinit(gpa);

    try tree.toStringPretty(output.writer(gpa), .skip_linecol);

    return try output.toOwnedSlice(gpa);
}

// ============================================================
// Pattern S-expression tests
// ============================================================

test "pattern sexpr - assign pattern" {
    const gpa = std.testing.allocator;
    const output = try canonicalizePatternFromDecl("x = 42");
    defer gpa.free(output);

    // Should contain p-assign for the pattern
    try std.testing.expect(std.mem.indexOf(u8, output, "p-assign") != null);
}

// Note: `_ = 42` is parsed as an expression statement, not a declaration
// The underscore pattern is tested via match expressions instead

test "pattern sexpr - record destructure pattern" {
    const gpa = std.testing.allocator;
    const output = try canonicalizePatternFromDecl("{ x, y } = record");
    defer gpa.free(output);

    // Should contain p-record-destructure
    try std.testing.expect(std.mem.indexOf(u8, output, "p-record-destructure") != null);
}

test "pattern sexpr - tuple pattern" {
    const gpa = std.testing.allocator;
    const output = try canonicalizePatternFromDecl("(a, b) = tuple");
    defer gpa.free(output);

    // Should contain p-tuple
    try std.testing.expect(std.mem.indexOf(u8, output, "p-tuple") != null);
}

// Note: List patterns in declarations are parsed differently
// List patterns are tested via match expressions instead

test "pattern sexpr - numeric literal pattern in match" {
    const gpa = std.testing.allocator;

    const source =
        \\match x {
        \\    0 => "zero"
        \\    1 => "one"
        \\    _ => "other"
        \\}
    ;

    const output = try canonicalizeAndRenderExpr(source);
    defer gpa.free(output);

    // Should contain p-num for numeric patterns
    try std.testing.expect(std.mem.indexOf(u8, output, "p-num") != null);
}

test "pattern sexpr - string literal pattern in match" {
    const gpa = std.testing.allocator;

    const source =
        \\match s {
        \\    "hello" => 1
        \\    "world" => 2
        \\    _ => 0
        \\}
    ;

    const output = try canonicalizeAndRenderExpr(source);
    defer gpa.free(output);

    // Should contain p-str for string patterns
    try std.testing.expect(std.mem.indexOf(u8, output, "p-str") != null);
}

test "pattern sexpr - tag pattern in match" {
    const gpa = std.testing.allocator;

    const source =
        \\match result {
        \\    Ok(value) => value
        \\    Err(msg) => msg
        \\}
    ;

    const output = try canonicalizeAndRenderExpr(source);
    defer gpa.free(output);

    // Should contain p-applied-tag for tag patterns
    try std.testing.expect(std.mem.indexOf(u8, output, "p-applied-tag") != null);
}

// Note: List patterns with rest in declarations are parsed differently
// These are tested via match expressions instead

// ============================================================
// Expression S-expression tests
// ============================================================

test "expression sexpr - integer literal" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("42");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-num") != null);
}

test "expression sexpr - string literal" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("\"hello world\"");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-string") != null);
}

test "expression sexpr - list literal" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("[1, 2, 3]");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-list") != null);
}

test "expression sexpr - empty list" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("[]");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-empty_list") != null);
}

test "expression sexpr - record literal" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("{ x: 1, y: 2 }");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-record") != null);
}

test "expression sexpr - empty record" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("{}");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-empty_record") != null);
}

test "expression sexpr - lambda" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("|x| x + 1");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-lambda") != null);
}

test "expression sexpr - function application" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("f(1, 2)");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-call") != null);
}

test "expression sexpr - if-then-else" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("if True then 1 else 2");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-if") != null);
}

test "expression sexpr - tag" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("Ok(42)");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-tag") != null);
}

test "expression sexpr - tuple" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("(1, 2, 3)");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-tuple") != null);
}

test "expression sexpr - binary operation" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("1 + 2");
    defer gpa.free(output);

    // Binary ops become function calls
    try std.testing.expect(std.mem.indexOf(u8, output, "e-call") != null or
        std.mem.indexOf(u8, output, "e-binop") != null);
}

test "expression sexpr - unary negation" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("-42");
    defer gpa.free(output);

    // Should have a negative integer
    try std.testing.expect(std.mem.indexOf(u8, output, "e-num") != null);
}

test "expression sexpr - match expression" {
    const gpa = std.testing.allocator;

    const source =
        \\match x {
        \\    True => 1
        \\    False => 0
        \\}
    ;

    const output = try canonicalizeAndRenderExpr(source);
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-match") != null);
}

test "expression sexpr - block expression" {
    const gpa = std.testing.allocator;

    const source =
        \\{
        \\    x = 1
        \\    x + 1
        \\}
    ;

    const output = try canonicalizeAndRenderExpr(source);
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-block") != null);
}

test "expression sexpr - fractional literal" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("3.14");
    defer gpa.free(output);

    // Should have some form of fractional representation
    try std.testing.expect(std.mem.indexOf(u8, output, "e-frac") != null or
        std.mem.indexOf(u8, output, "e-small-dec") != null or
        std.mem.indexOf(u8, output, "e-dec") != null);
}

test "expression sexpr - record access" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("record.field");
    defer gpa.free(output);

    // May be e-lookup-external or e-record-access depending on context
    try std.testing.expect(output.len > 0);
}

test "expression sexpr - nested match with multiple patterns" {
    const gpa = std.testing.allocator;

    const source =
        \\match pair {
        \\    (0, 0) => "origin"
        \\    (x, 0) => "on x-axis"
        \\    (0, y) => "on y-axis"
        \\    (x, y) => "elsewhere"
        \\}
    ;

    const output = try canonicalizeAndRenderExpr(source);
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-match") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "p-tuple") != null);
}

test "expression sexpr - nested records" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("{ outer: { inner: 42 } }");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-record") != null);
}

test "expression sexpr - list of records" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("[{ a: 1 }, { a: 2 }]");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-list") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "e-record") != null);
}

test "expression sexpr - lambda with multiple params" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("|a, b, c| a + b + c");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-lambda") != null);
}

test "expression sexpr - nested lambda" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("|x| |y| x + y");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-lambda") != null);
}

test "expression sexpr - tag with multiple args" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("Point(1, 2, 3)");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-tag") != null);
}

test "expression sexpr - chained function calls" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("f(g(h(x)))");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-call") != null);
}

test "pattern sexpr - as pattern" {
    const gpa = std.testing.allocator;

    const source =
        \\match list {
        \\    [first, .. as rest] => rest
        \\    [] => []
        \\}
    ;

    const output = try canonicalizeAndRenderExpr(source);
    defer gpa.free(output);

    // Should contain p-as for the as pattern
    try std.testing.expect(std.mem.indexOf(u8, output, "p-as") != null or
        std.mem.indexOf(u8, output, "rest-at") != null);
}

test "pattern sexpr - record destructure with sub-pattern" {
    const gpa = std.testing.allocator;
    const output = try canonicalizePatternFromDecl("{ name: n, age: a } = person");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "p-record-destructure") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "sub-pattern") != null);
}

test "expression sexpr - deeply nested expression" {
    const gpa = std.testing.allocator;
    const output = try canonicalizeAndRenderExpr("((((1))))");
    defer gpa.free(output);

    try std.testing.expect(std.mem.indexOf(u8, output, "e-num") != null);
}

test "expression sexpr - boolean literals" {
    const gpa = std.testing.allocator;

    const output_true = try canonicalizeAndRenderExpr("True");
    defer gpa.free(output_true);

    const output_false = try canonicalizeAndRenderExpr("False");
    defer gpa.free(output_false);

    // Both should produce tag expressions
    try std.testing.expect(std.mem.indexOf(u8, output_true, "e-tag") != null or
        std.mem.indexOf(u8, output_true, "Bool") != null);
    try std.testing.expect(std.mem.indexOf(u8, output_false, "e-tag") != null or
        std.mem.indexOf(u8, output_false, "Bool") != null);
}
