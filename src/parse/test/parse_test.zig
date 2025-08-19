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
    defer {
        // Clean up parser diagnostics in tests
        parser.diagnostics.deinit(allocator);
        parser.deinit();
    }
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
    defer {
        // Clean up parser diagnostics in tests
        parser.diagnostics.deinit(allocator);
        parser.deinit();
    }
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

test "Parser2: simple assignment binop uses correct nodes" {
    const allocator = testing.allocator;
    const source = "module [x]\nx = 42";

    var ast = try parseTestFile(allocator, source);
    defer ast.deinit(allocator);

    // Check nodes
    // Should have: exposes node, x from assignment, 42, binop_equals, block
    try testing.expectEqual(@as(usize, 5), ast.nodes.len());

    // Node 0: x from exposes
    // Node 1: x from assignment LHS
    // Node 2: 42
    // Node 3: binop_equals
    const binop_idx = @as(AST2.Node.Idx, @enumFromInt(3));
    try testing.expectEqual(AST2.Node.Tag.binop_equals, ast.tag(binop_idx));

    // Get the binop operands
    const binop_payload = ast.payload(binop_idx).binop;
    const binop = ast.node_slices.binOp(binop_payload);

    // The binop should have lhs=1 (x from assignment) and rhs=2 (42)
    // NOT lhs=0 (x from exposes) and rhs=1 (x from assignment)
    try testing.expectEqual(@as(i32, 1), @intFromEnum(binop.lhs));
    try testing.expectEqual(@as(i32, 2), @intFromEnum(binop.rhs));

    // Verify the nodes are correct
    try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(binop.lhs));
    try testing.expectEqual(AST2.Node.Tag.num_literal_i32, ast.tag(binop.rhs));
    try testing.expectEqual(@as(i32, 42), ast.payload(binop.rhs).num_literal_i32);
}

test "Parser2: lambda expression parsing" {
    const allocator = testing.allocator;
    const source = "|x| x + 1";

    var ast = try parseTestExpr(allocator, source);
    defer ast.deinit(allocator);

    // Check it's a lambda
    const lambda_idx = @as(AST2.Node.Idx, @enumFromInt(ast.nodes.len() - 1));
    try testing.expectEqual(AST2.Node.Tag.lambda, ast.tag(lambda_idx));

    // Get lambda parts
    const lambda = ast.lambda(lambda_idx);

    // Should have one argument
    var args_iter = ast.lambdaArgs(lambda);
    const arg = args_iter.next() orelse unreachable; // Should have one arg
    try testing.expectEqual(@as(?AST2.Node.Idx, null), args_iter.next()); // Should be no more args

    // The argument should be 'x'
    try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(arg));

    // The body should be a binop_plus
    try testing.expectEqual(AST2.Node.Tag.binop_plus, ast.tag(lambda.body));
}

test "Parser2: unary operator parsing" {
    const allocator = testing.allocator;
    
    // Test unary not - verify it stores operand in block_nodes
    {
        const source = "!foo";
        var ast = try parseTestExpr(allocator, source);
        defer ast.deinit(allocator);
        
        // !foo is parsed as unary_not with foo as operand
        const root_idx = @as(AST2.Node.Idx, @enumFromInt(ast.nodes.len() - 1));
        try testing.expectEqual(AST2.Node.Tag.unary_not, ast.tag(root_idx));
        
        // Verify the operand is stored in block_nodes and we can iterate over it
        const nodes_iter = ast.node_slices.nodes(ast.payload(root_idx).block_nodes);
        var iter = nodes_iter;
        const operand = iter.next() orelse unreachable; // Should have an operand
        try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(operand)); // Should be 'foo'
        try testing.expectEqual(@as(?AST2.Node.Idx, null), iter.next()); // Should be only one operand
    }
}

test "Parser2: return and crash statements" {
    const allocator = testing.allocator;
    
    // Test return statement
    {
        const source = "return 42";
        var ast = try parseTestExpr(allocator, source);
        defer ast.deinit(allocator);
        
        // Should be parsed as .ret with expression
        const root_idx = @as(AST2.Node.Idx, @enumFromInt(ast.nodes.len() - 1));
        try testing.expectEqual(AST2.Node.Tag.ret, ast.tag(root_idx));
        
        // Verify the expression is stored in block_nodes
        const nodes_iter = ast.node_slices.nodes(ast.payload(root_idx).block_nodes);
        var iter = nodes_iter;
        const expr = iter.next() orelse unreachable; // Should have the expression
        try testing.expectEqual(AST2.Node.Tag.num_literal_i32, ast.tag(expr)); // Should be '42'
        try testing.expectEqual(@as(?AST2.Node.Idx, null), iter.next()); // Should be only one expression
    }
    
    // Test crash statement
    {
        const source = "crash \"error message\"";
        var ast = try parseTestExpr(allocator, source);
        defer ast.deinit(allocator);
        
        // Should be parsed as .crash with expression
        const root_idx = @as(AST2.Node.Idx, @enumFromInt(ast.nodes.len() - 1));
        try testing.expectEqual(AST2.Node.Tag.crash, ast.tag(root_idx));
        
        // Verify the expression is stored in block_nodes
        const nodes_iter = ast.node_slices.nodes(ast.payload(root_idx).block_nodes);
        var iter = nodes_iter;
        const expr = iter.next() orelse unreachable; // Should have the expression
        // The string literal could be small or big depending on the content
        try testing.expect(ast.tag(expr) == .str_literal_small or ast.tag(expr) == .str_literal_big);
        try testing.expectEqual(@as(?AST2.Node.Idx, null), iter.next()); // Should be only one expression
    }
}
