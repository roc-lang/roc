const std = @import("std");
const base = @import("base");
const parse = @import("../mod.zig");
const AST = @import("../AST.zig");

fn parseTopLevel(source: []const u8) !AST {
    const allocator = std.testing.allocator;
    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    const ast = try parse.parse(&env, allocator);
    return ast;
}

fn getFirstStatement(ast: *const AST) AST.Statement {
    const file = ast.store.getFile();
    const statements = ast.store.statementSlice(file.statements);
    const first_stmt_idx = statements[0];
    return ast.store.getStatement(first_stmt_idx);
}

test "nominal type with single statement block" {
    const source =
        \\module []
        \\
        \\Foo := [A, B, C].{ x = 5 }
    ;
    var ast = try parseTopLevel(source);
    defer ast.deinit(std.testing.allocator);

    const statement = getFirstStatement(&ast);
    try std.testing.expect(statement == .type_decl);
    try std.testing.expectEqual(AST.TypeDeclKind.nominal, statement.type_decl.kind);
    try std.testing.expect(statement.type_decl.block != null);

    // Verify the block has statements
    const block = statement.type_decl.block.?;
    const block_statements = ast.store.statementSlice(block.statements);
    try std.testing.expect(block_statements.len > 0);
}

test "nominal type with multi-statement block" {
    const source =
        \\module []
        \\
        \\Foo := [A, B, C].{
        \\    x = 5
        \\    y = 10
        \\    z = 15
        \\}
    ;
    var ast = try parseTopLevel(source);
    defer ast.deinit(std.testing.allocator);

    const statement = getFirstStatement(&ast);
    try std.testing.expect(statement == .type_decl);
    try std.testing.expectEqual(AST.TypeDeclKind.nominal, statement.type_decl.kind);
    try std.testing.expect(statement.type_decl.block != null);

    const block = statement.type_decl.block.?;
    const block_statements = ast.store.statementSlice(block.statements);
    try std.testing.expectEqual(@as(usize, 3), block_statements.len);
}

test "nominal type without block" {
    const source =
        \\module []
        \\
        \\Foo := [A, B, C]
    ;
    var ast = try parseTopLevel(source);
    defer ast.deinit(std.testing.allocator);

    const statement = getFirstStatement(&ast);
    try std.testing.expect(statement == .type_decl);
    try std.testing.expectEqual(AST.TypeDeclKind.nominal, statement.type_decl.kind);
    try std.testing.expect(statement.type_decl.block == null);
}

test "nominal type with block containing final expression produces error" {
    const source =
        \\module []
        \\
        \\Foo := [A, B, C].{ x = 5 x }
    ;
    var ast = try parseTopLevel(source);
    defer ast.deinit(std.testing.allocator);

    // Should have parsed but with a diagnostic
    const statement = getFirstStatement(&ast);
    try std.testing.expect(statement == .type_decl);
    try std.testing.expectEqual(AST.TypeDeclKind.nominal, statement.type_decl.kind);

    // Check for diagnostic
    try std.testing.expect(ast.parse_diagnostics.items.len > 0);
    const diag = ast.parse_diagnostics.items[0];
    try std.testing.expectEqual(AST.Diagnostic.Tag.nominal_block_cannot_have_final_expression, diag.tag);
}

test "type alias with block produces error" {
    const source =
        \\module []
        \\
        \\Foo : [A, B, C].{ x = 5 }
    ;
    var ast = try parseTopLevel(source);
    defer ast.deinit(std.testing.allocator);

    const statement = getFirstStatement(&ast);
    if (statement != .type_decl) {
        std.debug.print("\nExpected type_decl, got: {}\n", .{statement});
        return error.TestFailed;
    }

    try std.testing.expectEqual(AST.TypeDeclKind.alias, statement.type_decl.kind);
    try std.testing.expect(statement.type_decl.block == null);

    // Check for diagnostic
    if (ast.parse_diagnostics.items.len == 0) {
        std.debug.print("\nNo diagnostics found!\n", .{});
        return error.TestFailed;
    }
    const diag = ast.parse_diagnostics.items[0];
    if (diag.tag != .type_alias_cannot_have_block) {
        std.debug.print("\nExpected type_alias_cannot_have_block, got: {}\n", .{diag.tag});
        return error.TestFailed;
    }
}

test "nominal type with empty block" {
    const source =
        \\module []
        \\
        \\Foo := [A, B, C].{ }
    ;
    var ast = try parseTopLevel(source);
    defer ast.deinit(std.testing.allocator);

    const statement = getFirstStatement(&ast);
    try std.testing.expect(statement == .type_decl);
    try std.testing.expectEqual(AST.TypeDeclKind.nominal, statement.type_decl.kind);
    try std.testing.expect(statement.type_decl.block != null);

    const block = statement.type_decl.block.?;
    const block_statements = ast.store.statementSlice(block.statements);
    try std.testing.expectEqual(@as(usize, 0), block_statements.len);
}

test "nominal type with nested blocks in statements" {
    const source =
        \\module []
        \\
        \\Foo := [A, B].{
        \\    x = { a = 1 }
        \\}
    ;
    var ast = try parseTopLevel(source);
    defer ast.deinit(std.testing.allocator);

    const statement = getFirstStatement(&ast);
    try std.testing.expect(statement == .type_decl);
    try std.testing.expectEqual(AST.TypeDeclKind.nominal, statement.type_decl.kind);
    try std.testing.expect(statement.type_decl.block != null);
}

test "type alias without block" {
    const source =
        \\module []
        \\
        \\Foo : [A, B, C]
    ;
    var ast = try parseTopLevel(source);
    defer ast.deinit(std.testing.allocator);

    const statement = getFirstStatement(&ast);
    if (statement != .type_decl) {
        std.debug.print("\nExpected type_decl, got: {}\n", .{statement});
        return error.TestFailed;
    }
    try std.testing.expectEqual(AST.TypeDeclKind.alias, statement.type_decl.kind);
    try std.testing.expect(statement.type_decl.block == null);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
}