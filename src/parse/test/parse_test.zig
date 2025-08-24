const std = @import("std");
const testing = std.testing;
const base = @import("base");
const collections = @import("collections");
const AST2 = @import("../AST2.zig");
const Parser2 = @import("../Parser2.zig");
const tokenize_iter = @import("../tokenize_iter.zig");

fn parseTestFile(allocator: std.mem.Allocator, source: []const u8) !AST2 {
    var ast = try AST2.initCapacity(allocator, 100);
    errdefer ast.deinit(allocator);

    // Create a CommonEnv for tokenization
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    // Create diagnostics buffer
    var messages: [128]tokenize_iter.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Parse using new Parser2 with TokenIterator
    var parser = try Parser2.init(&env, allocator, source, msg_slice, &ast, &byte_slices);
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

    // Create diagnostics buffer
    var messages: [128]tokenize_iter.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // Parse expression using new Parser2 with TokenIterator
    var parser = try Parser2.init(&env, allocator, source, msg_slice, &ast, &byte_slices);
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

test "Parser2: simple assignment binop uses correct nodes" {
    const allocator = testing.allocator;
    const source = "module [x]\nx = 42";

    var ast = try parseTestFile(allocator, source);
    defer ast.deinit(allocator);

    // Check nodes
    // Should have: exposes node, x from assignment, 42, binop_equals, block
    // TODO: Fix extra malformed node being created at position 0
    try testing.expectEqual(@as(usize, 6), ast.nodes.len());

    // Node 0: malformed (TODO: fix this)
    // Node 1: x from exposes
    // Node 2: x from assignment LHS
    // Node 3: 42
    // Node 4: binop_equals
    const binop_idx = @as(AST2.Node.Idx, @enumFromInt(4));
    try testing.expectEqual(AST2.Node.Tag.binop_equals, ast.tag(binop_idx));

    // Get the binop operands
    const binop_payload = ast.payload(binop_idx).binop;
    const binop = ast.node_slices.binOp(binop_payload);

    // The binop should have lhs=2 (x from assignment) and rhs=3 (42)
    // NOT lhs=1 (x from exposes) and rhs=2 (x from assignment)
    try testing.expectEqual(@as(i32, 2), @intFromEnum(binop.lhs));
    try testing.expectEqual(@as(i32, 3), @intFromEnum(binop.rhs));

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
        const payload_ptr = ast.payloadPtr(root_idx);
        const nodes_iter = ast.node_slices.nodes(&payload_ptr.nodes);
        var iter = nodes_iter;
        const operand = iter.next() orelse unreachable; // Should have an operand
        try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(operand)); // Should be 'foo'
        try testing.expectEqual(@as(?AST2.Node.Idx, null), iter.next()); // Should be only one operand
    }
}

test "Parser2: while loop parsing" {
    const allocator = testing.allocator;

    // Test simple while loop
    {
        const source = "while x < 10 { x = x + 1 }";
        var ast = try parseTestExpr(allocator, source);
        defer ast.deinit(allocator);

        // Should be parsed as .while_loop with condition and body
        const root_idx = @as(AST2.Node.Idx, @enumFromInt(ast.nodes.len() - 1));
        try testing.expectEqual(AST2.Node.Tag.while_loop, ast.tag(root_idx));

        // Verify the while loop structure
        const while_val = ast.whileLoop(root_idx);

        // The condition should be a binop_lt
        try testing.expectEqual(AST2.Node.Tag.binop_lt, ast.tag(while_val.condition));

        // The body should be a block
        try testing.expectEqual(AST2.Node.Tag.block, ast.tag(while_val.body));
    }

    // Test while loop with expression body
    {
        const source = "while True doSomething";
        var ast = try parseTestExpr(allocator, source);
        defer ast.deinit(allocator);

        const root_idx = @as(AST2.Node.Idx, @enumFromInt(ast.nodes.len() - 1));
        try testing.expectEqual(AST2.Node.Tag.while_loop, ast.tag(root_idx));

        const while_val = ast.whileLoop(root_idx);

        // The condition should be a tag (True)
        try testing.expectEqual(AST2.Node.Tag.uc, ast.tag(while_val.condition));

        // The body should be a lowercase identifier
        try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(while_val.body));
    }
}

test "Parser2: for loop parsing" {
    const allocator = testing.allocator;

    // Test simple for loop
    {
        const source = "for x in list { x + 1 }";
        var ast = try parseTestExpr(allocator, source);
        defer ast.deinit(allocator);

        // Should be parsed as .for_loop with pattern, iterable, and body
        const root_idx = @as(AST2.Node.Idx, @enumFromInt(ast.nodes.len() - 1));
        try testing.expectEqual(AST2.Node.Tag.for_loop, ast.tag(root_idx));

        // Verify the for loop structure
        const for_val = ast.forLoop(root_idx);

        // The pattern should be a lowercase identifier (x)
        try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(for_val.pattern));

        // The iterable should be a lowercase identifier (list)
        try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(for_val.iterable));

        // The body should be a block
        try testing.expectEqual(AST2.Node.Tag.block, ast.tag(for_val.body));
    }

    // Test for loop with expression body (new syntax)
    {
        const source = "for item in items item";
        var ast = try parseTestExpr(allocator, source);
        defer ast.deinit(allocator);

        const root_idx = @as(AST2.Node.Idx, @enumFromInt(ast.nodes.len() - 1));
        try testing.expectEqual(AST2.Node.Tag.for_loop, ast.tag(root_idx));

        const for_val = ast.forLoop(root_idx);

        // The pattern should be a lowercase identifier (item)
        try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(for_val.pattern));

        // The iterable should be a lowercase identifier (items)
        try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(for_val.iterable));

        // The body should be a lowercase identifier (item)
        try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(for_val.body));
    }

    // Test for loop with pattern destructuring
    {
        const source = "for (x, y) in pairs x + y";
        var ast = try parseTestExpr(allocator, source);
        defer ast.deinit(allocator);

        const root_idx = @as(AST2.Node.Idx, @enumFromInt(ast.nodes.len() - 1));
        try testing.expectEqual(AST2.Node.Tag.for_loop, ast.tag(root_idx));

        const for_val = ast.forLoop(root_idx);

        // The pattern should be a tuple literal (used for tuple patterns too)
        try testing.expectEqual(AST2.Node.Tag.tuple_literal, ast.tag(for_val.pattern));

        // The iterable should be a lowercase identifier (pairs)
        try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(for_val.iterable));

        // The body should be a binop_plus
        try testing.expectEqual(AST2.Node.Tag.binop_plus, ast.tag(for_val.body));
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
        const payload_ptr2 = ast.payloadPtr(root_idx);
        const nodes_iter = ast.node_slices.nodes(&payload_ptr2.nodes);
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
        const payload_ptr2 = ast.payloadPtr(root_idx);
        const nodes_iter = ast.node_slices.nodes(&payload_ptr2.nodes);
        var iter = nodes_iter;
        const expr = iter.next() orelse unreachable; // Should have the expression
        // The string literal could be small or big depending on the content
        try testing.expect(ast.tag(expr) == .str_literal_small or ast.tag(expr) == .str_literal_big);
        try testing.expectEqual(@as(?AST2.Node.Idx, null), iter.next()); // Should be only one expression
    }
}

// ============ Comma/Arrow Parsing Tests ============

test "Parser2: basic function arrow cases" {
    const allocator = testing.allocator;

    // Test -> arrow
    {
        const source = "module []\nfoo : a, b -> c\nfoo = |x, y| x";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        // Should parse successfully - the type annotation should be a function type
        try testing.expect(ast.nodes.len() > 0);
    }

    // Test => arrow
    {
        const source = "module []\nfoo : a, b => c\nfoo = |x, y| x";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        // Should parse successfully
        try testing.expect(ast.nodes.len() > 0);
    }

    // Test multi-parameter function
    {
        const source = "module []\nfoo : a, b, c -> d\nfoo = |x, y, z| x";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        // Should parse successfully
        try testing.expect(ast.nodes.len() > 0);
    }
}

test "Parser2: comma sequence parsing differences" {
    const allocator = testing.allocator;

    // Test case with arrow (should parse as function type)
    const with_arrow = "module []\nfoo : a, b -> c\nfoo = |x, y| x";
    var arrow_ast = try parseTestFile(allocator, with_arrow);
    defer arrow_ast.deinit(allocator);

    // Test case without arrow (behavior depends on parser implementation)
    const without_arrow = "module []\nfoo : a, b, c\nfoo = 1";
    var no_arrow_ast = try parseTestFile(allocator, without_arrow);
    defer no_arrow_ast.deinit(allocator);

    // Both should produce some kind of parse result
    try testing.expect(arrow_ast.nodes.len() > 0);
    try testing.expect(no_arrow_ast.nodes.len() > 0);

    // Document that these cases may parse differently depending on implementation
    // The exact structure depends on how the parser handles comma sequences
}

test "Parser2: explicit tuples vs functions" {
    const allocator = testing.allocator;

    // Explicit tuple - should be valid
    {
        const source = "module []\nfoo : (a, b, c)\nfoo = (1, 2, 3)";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Function in parens
    {
        const source = "module []\nfoo : (a, b -> c)\nfoo = |x, y| x";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Tuple containing function and other type
    {
        const source = "module []\nfoo : (a, b -> c, d)\nfoo = (|x, y| x, 1)";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }
}

test "Parser2: record field parsing with arrows" {
    const allocator = testing.allocator;

    // Field puns
    {
        const source = "module []\nfoo : { a, b }\nfoo = { a: 1, b: 2 }";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Field with function type using ->
    {
        const source = "module []\nfoo : { a : b, c -> d }\nfoo = { a: |x, y| x }";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Field with function type using =>
    {
        const source = "module []\nfoo : { a : b, c => d }\nfoo = { a: |x, y| x }";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Multiple field types with different arrows
    {
        const source = "module []\nfoo : { a : b, c => d, e : f }\nfoo = { a: |x, y| x, e: 1 }";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }
}

test "Parser2: record field arrow cases" {
    const allocator = testing.allocator;

    // Arrow without field name
    {
        const source = "module []\nfoo : { a, b -> c }\nfoo = { a: 1 }";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Arrow at record field level
    {
        const source = "module []\nfoo : { a : (b, c), d -> e }\nfoo = { a: (1, 2) }";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }
}

test "Parser2: list element parsing with arrows" {
    const allocator = testing.allocator;

    // List of single type
    {
        const source = "module []\nfoo : [a]\nfoo = [1]";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // List of functions with ->
    {
        const source = "module []\nfoo : [a, b -> c]\nfoo = [|x, y| x]";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // List of functions with =>
    {
        const source = "module []\nfoo : [a, b => c]\nfoo = [|x, y| x]";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // List of multi-parameter functions
    {
        const source = "module []\nfoo : [a, b, c -> d]\nfoo = [|x, y, z| x]";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }
}

test "Parser2: list comma sequences" {
    const allocator = testing.allocator;

    // Comma sequence in list without arrow
    {
        const source = "module []\nfoo : [a, b, c]\nfoo = [1, 2, 3]";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }
}

test "Parser2: mixed container cases" {
    const allocator = testing.allocator;

    // Tuple containing list of functions
    {
        const source = "module []\nfoo : (a, [b, c -> d])\nfoo = (1, [|x, y| x])";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // List of functions taking tuples
    {
        const source = "module []\nfoo : [a, (b, c) -> d]\nfoo = [|x, (y, z)| x]";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Record with list field containing functions
    {
        const source = "module []\nfoo : { a : [b, c => d] }\nfoo = { a: [|x, y| x] }";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // List of tuples and functions
    {
        const source = "module []\nfoo : [(a, b), c -> d]\nfoo = [((1, 2), |x| x)]";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }
}

test "Parser2: function composition and precedence" {
    const allocator = testing.allocator;

    // Function returning tuple (explicit parens)
    {
        const source = "module []\nfoo : a -> (b, c => d)\nfoo = |x| |y, z| y";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Function taking function and another parameter
    {
        const source = "module []\nfoo : (a -> b), c => d\nfoo = |f, x| f(x)";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Right-associative function types
    {
        const source = "module []\nfoo : a, b -> c -> d\nfoo = |x, y| |z| z";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Mixed arrows with right associativity
    {
        const source = "module []\nfoo : a, b => c => d\nfoo = |x, y| |z| z";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }
}

test "Parser2: complex nested cases" {
    const allocator = testing.allocator;

    // Function taking record with function field
    {
        const source = "module []\nfoo : a, { b : c, d => e } -> f\nfoo = |x, rec| 1";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Function taking list of functions
    {
        const source = "module []\nfoo : [a, b -> c], d => e\nfoo = |list, x| x";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }

    // Record becomes part of top-level function when comma used correctly
    {
        const source = "module []\nfoo : { a : [b, c => d] }, e -> f\nfoo = |rec, x| x";
        var ast = try parseTestFile(allocator, source);
        defer ast.deinit(allocator);

        try testing.expect(ast.nodes.len() > 0);
    }
}

test "Parser2: current comma parsing behavior" {
    const allocator = testing.allocator;

    // Test what the current parser actually does with comma sequences
    {
        const source1 = "module []\nfoo : a, b -> c\nfoo = |x, y| x";
        const source2 = "module []\nfoo : (a, b -> c)\nfoo = |x, y| x";

        var ast1 = try parseTestFile(allocator, source1);
        defer ast1.deinit(allocator);
        var ast2 = try parseTestFile(allocator, source2);
        defer ast2.deinit(allocator);

        // Both should parse successfully
        try testing.expect(ast1.nodes.len() > 0);
        try testing.expect(ast2.nodes.len() > 0);

        // The current parser may not handle comma sequences as intended yet
        // These tests document the current behavior rather than asserting ideal behavior
    }

    // Test explicit tuples (these should definitely work)
    {
        const source1 = "module []\nfoo : (a, b, c)\nfoo = (1, 2, 3)";
        var ast1 = try parseTestFile(allocator, source1);
        defer ast1.deinit(allocator);

        try testing.expect(ast1.nodes.len() > 0);
    }

    // Test basic arrow functions (these should definitely work)
    {
        const source1 = "module []\nfoo : a -> b\nfoo = |x| x";
        var ast1 = try parseTestFile(allocator, source1);
        defer ast1.deinit(allocator);

        try testing.expect(ast1.nodes.len() > 0);
    }
}

test "manual snapshot comparison - one file" {
    const allocator = testing.allocator;

    // This is the source from 001.md
    const source =
        \\module     [
        \\# some crazy formatting
        \\ foo,
        \\     ]
        \\
        \\foo =
        \\
        \\    "one"
    ;

    // std.debug.print("\n=== Manual Snapshot Test ===\n", .{});
    // std.debug.print("Expected problems: NIL (no errors)\n", .{});
    // std.debug.print("Source:\n{s}\n\n", .{source});

    var ast = try AST2.initCapacity(allocator, 100);
    defer ast.deinit(allocator);

    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var messages: [128]tokenize_iter.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var parser = try Parser2.init(&env, allocator, source, msg_slice, &ast, &byte_slices);
    defer parser.deinit();

    // std.debug.print("Parsing...\n", .{});
    _ = parser.parseFile() catch null;

    // std.debug.print("Parsing completed! Diagnostics: {d}\n", .{parser.diagnostics.items.len});

    // for (parser.diagnostics.items, 0..) |diagnostic, i| {
    //     std.debug.print("  [{d}] {s}\n", .{ i, @tagName(diagnostic.tag) });
    // }

    if (parser.diagnostics.items.len == 0) {
        // std.debug.print("RESULT: MATCH - No parsing errors (expected NIL)\n", .{});
    } else {
        // std.debug.print("RESULT: MISMATCH - Found {d} errors (expected NIL)\n", .{parser.diagnostics.items.len});
    }

    // std.debug.print("=== End Manual Test ===\n", .{});
}

test "Parser2: iterative parser simple expression" {
    const allocator = testing.allocator;

    // Test simple identifier
    {
        const source = "foo";
        var ast = try AST2.initCapacity(allocator, 100);
        defer ast.deinit(allocator);

        var env = try base.CommonEnv.init(allocator, source);
        defer env.deinit(allocator);

        var messages: [128]tokenize_iter.Diagnostic = undefined;
        const msg_slice = messages[0..];
        var byte_slices = collections.ByteSlices{ .entries = .{} };
        defer byte_slices.entries.deinit(allocator);

        // Create parser with iterative mode enabled
        var parser = try Parser2.initWithOptions(&env, allocator, source, msg_slice, &ast, &byte_slices);
        defer parser.deinit();

        const result = try parser.parseExpr();

        // Should produce a lowercase identifier
        try testing.expectEqual(AST2.Node.Tag.lc, ast.tag(result));
    }

    // Test binary operation
    {
        const source = "1 + 2";
        var ast = try AST2.initCapacity(allocator, 100);
        defer ast.deinit(allocator);

        var env = try base.CommonEnv.init(allocator, source);
        defer env.deinit(allocator);

        var messages: [128]tokenize_iter.Diagnostic = undefined;
        const msg_slice = messages[0..];
        var byte_slices = collections.ByteSlices{ .entries = .{} };
        defer byte_slices.entries.deinit(allocator);

        // Create parser with iterative mode enabled
        var parser = try Parser2.initWithOptions(&env, allocator, source, msg_slice, &ast, &byte_slices);
        defer parser.deinit();

        const result = try parser.parseExpr();

        // Should produce a binary operation
        try testing.expectEqual(AST2.Node.Tag.binop_plus, ast.tag(result));
    }
}
