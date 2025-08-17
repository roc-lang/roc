const std = @import("std");
const testing = std.testing;
const base = @import("base");
const AST2 = @import("../AST2.zig");
const Parser2 = @import("../Parser2.zig");
const tokenize = @import("../tokenize.zig");

fn parseTestFile(allocator: std.mem.Allocator, source: []const u8) !AST2 {
    var ast = try AST2.initCapacity(allocator, 100);
    errdefer ast.deinit(allocator);

    // Create a CommonEnv for tokenization
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    // Tokenize the source
    var messages: [128]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var tokenizer = try tokenize.Tokenizer.init(&env, allocator, source, msg_slice);
    try tokenizer.tokenize(allocator);
    var result = tokenizer.finishAndDeinit(allocator);
    defer result.tokens.deinit(allocator);

    // Parse the tokenized source
    var parser = try Parser2.init(result.tokens, allocator, &ast);
    defer parser.deinit();
    _ = try parser.parseFile();

    return ast;
}

fn parseTestExpr(allocator: std.mem.Allocator, source: []const u8) !AST2 {
    var ast = try AST2.initCapacity(allocator, 100);
    errdefer ast.deinit(allocator);

    // Create a CommonEnv for tokenization
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    // Tokenize the source
    var messages: [128]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var tokenizer = try tokenize.Tokenizer.init(&env, allocator, source, msg_slice);
    try tokenizer.tokenize(allocator);
    var result = tokenizer.finishAndDeinit(allocator);
    defer result.tokens.deinit(allocator);

    // Parse the tokenized source as an expression
    var parser = try Parser2.init(result.tokens, allocator, &ast);
    defer parser.deinit();
    _ = try parser.parseExpr();

    return ast;
}

// ============ Module Header Tests ============

test "parse simple app module header" {
    const allocator = testing.allocator;
    
    // Source from formatting/multiline/app.md
    const source =
        \\app [
        \\    a1!,
        \\    a2!,
        \\] {
        \\    pf: platform "../basic-cli/main.roc",
        \\    a: "a",
        \\}
    ;

    var ast = try parseTestFile(allocator, source);
    defer ast.deinit(allocator);

    // Check that we have an app header
    try testing.expect(ast.header != null);
}

test "parse simple platform module header" {
    const allocator = testing.allocator;
    
    // Simplified platform header
    const source =
        \\platform "foo"
        \\    exposes [bar]
        \\    provides [baz]
    ;

    var ast = try parseTestFile(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.header != null);
}

test "parse simple package module header" {
    const allocator = testing.allocator;
    
    // Source from package_header_nonempty_multiline_1.md
    const source =
        \\package # This comment is here
        \\    [something, SomeType]
        \\    { somePkg: "../main.roc" }
    ;

    var ast = try parseTestFile(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.header != null);
}

test "parse simple module header" {
    const allocator = testing.allocator;
    
    // Source from primitive/expr_int.md
    const source =
        \\module [foo]
        \\foo = 42
    ;

    var ast = try parseTestFile(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.header != null);
}

// ============ Expression Tests (no module header) ============

test "parse integer literal expression" {
    const allocator = testing.allocator;
    const source = "42";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    // Should have parsed something
    try testing.expect(ast.nodes.len() > 0);
}

test "parse string literal expression" {
    const allocator = testing.allocator;
    const source = 
        \\"hello world"
    ;

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse list literal expression" {
    const allocator = testing.allocator;
    const source = "[1, 2, 3]";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse record literal expression" {
    const allocator = testing.allocator;
    const source = "{ x: 1, y: 2 }";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse lambda expression" {
    const allocator = testing.allocator;
    const source = "|x| x + 1";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse tuple expression" {
    const allocator = testing.allocator;
    // Source from expr/tuple_comprehensive.md
    const source = "(1, 2)";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse block expression" {
    const allocator = testing.allocator;
    // Source from expr/tuple_comprehensive.md
    const source = 
        \\{
        \\    x = 10
        \\    y = 20
        \\    x + y
        \\}
    ;

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse if-then-else expression" {
    const allocator = testing.allocator;
    const source = "if True then 1 else 2";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse match expression" {
    const allocator = testing.allocator;
    // Source adapted from match_expr/list_patterns.md
    const source = 
        \\match numbers {
        \\    [] => acc
        \\    [first, ..rest] => 0
        \\}
    ;

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse binary operation" {
    const allocator = testing.allocator;
    const source = "1 + 2 * 3";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse function application" {
    const allocator = testing.allocator;
    const source = "foo(bar, baz)";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse field access" {
    const allocator = testing.allocator;
    const source = "record.field";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

// ============ Error/Diagnostic Tests ============

test "parse malformed if expression missing else" {
    const allocator = testing.allocator;
    // Source from expr_if_missing_else.md
    const source = "if tru 0";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    // Should still parse something (with malformed nodes)
    try testing.expect(ast.nodes.len() > 0);
    // TODO: Once diagnostics are properly stored, check for them
    // try testing.expect(parser.diagnostics.items.len > 0);
}

test "parse empty tuple (should produce diagnostic)" {
    const allocator = testing.allocator;
    // Source from expr/tuple_comprehensive.md
    const source = "()";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    // Should parse (even if invalid)
    try testing.expect(ast.nodes.len() > 0);
    // TODO: Check for empty tuple diagnostic
}

test "parse invalid list rest pattern" {
    const allocator = testing.allocator;
    // Source from match_expr/list_patterns.md
    const source = "[first, ..rest]";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
    // TODO: Check for diagnostic about bad list rest pattern syntax
}

test "parse unclosed string literal" {
    const allocator = testing.allocator;
    const source = 
        \\"hello
    ;

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    // Should still parse something
    try testing.expect(ast.nodes.len() > 0);
}

test "parse unclosed list literal" {
    const allocator = testing.allocator;
    const source = "[1, 2, 3";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse unclosed record literal" {
    const allocator = testing.allocator;
    const source = "{ x: 1, y: 2";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

// ============ Complex Statement Tests ============

test "parse type annotation and definition" {
    const allocator = testing.allocator;
    // Source from type_annotation_basic.md
    const source =
        \\module []
        \\identity : a -> a
        \\identity = |x| x
    ;

    var ast = try parseTestFile(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.header != null);
    try testing.expect(ast.nodes.len() > 0);
}

test "parse multiple definitions in module" {
    const allocator = testing.allocator;
    // Source from type_annotation_basic.md
    const source =
        \\module []
        \\combine : a, b -> (a, b)
        \\combine = |first, second| (first, second)
        \\
        \\addOne : U64 -> U64
        \\addOne = |n| n + 1
    ;

    var ast = try parseTestFile(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.header != null);
    try testing.expect(ast.nodes.len() > 0);
}

test "parse import statements" {
    const allocator = testing.allocator;
    // Source from hello_world.md
    const source =
        \\module []
        \\
        \\import pf.Stdout
        \\
        \\main! = |_| Stdout.line!("Hello, world!")
    ;

    var ast = try parseTestFile(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.header != null);
    try testing.expect(ast.nodes.len() > 0);
}

test "parse nested expressions" {
    const allocator = testing.allocator;
    const source = "((1 + 2) * (3 + 4))";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse chained field access" {
    const allocator = testing.allocator;
    const source = "foo.bar.baz.qux";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse tag literal" {
    const allocator = testing.allocator;
    const source = "True";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}

test "parse tag with payload" {
    const allocator = testing.allocator;
    const source = "Some(42)";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    try testing.expect(ast.nodes.len() > 0);
}