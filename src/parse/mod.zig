//! Converts Roc source code into an Abstract Syntax Tree (AST) through tokenization and parsing.
//!
//! This module provides the entry point for the parsing phase of compilation, transforming
//! raw source text into a structured AST representation that subsequent compiler phases can process.

const std = @import("std");
const base = @import("base");
const tracy = @import("tracy");

pub const tokenize = @import("tokenize.zig");

const Allocator = std.mem.Allocator;
const CommonEnv = base.CommonEnv;
const Diagnostic = AST.Diagnostic;
const ParseTestError = Allocator.Error || error{TestExpectedEqual};

/// **AST.Parser**
pub const Parser = @import("Parser.zig");

/// **AST.Node**
pub const Node = @import("Node.zig");

/// **AST.NodeStore**
pub const NodeStore = @import("NodeStore.zig");

/// Parser-owned declaration inventory.
pub const DeclIndex = @import("DeclIndex.zig");

/// Parser-owned numeric literal facts.
pub const NumericLiteral = @import("NumericLiteral.zig");

/// Represents the intermediate representation or Abstract Syntax Tree (AST) of a parsed Roc file.
pub const AST = @import("AST.zig");

/// Internal parsing implementation.
fn runTokenDispatch(gpa: Allocator, env: *CommonEnv, parserCall: *const fn (*Parser) Allocator.Error!u32) Allocator.Error!*AST {
    const trace = tracy.trace(@src());
    defer trace.end();

    var messages: [128]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var tokenizer = try tokenize.Tokenizer.init(env, gpa, env.source, msg_slice);
    try tokenizer.tokenize(gpa);
    var result = tokenizer.finishAndDeinit();

    var parser = try Parser.init(result.tokens, gpa);
    defer parser.deinit();

    errdefer result.tokens.deinit(gpa);
    errdefer parser.store.deinit();
    errdefer parser.decl_index.deinit();
    errdefer parser.diagnostics.deinit(gpa);

    const idx = try parserCall(&parser);

    const tokenize_diagnostics_slice = try gpa.dupe(tokenize.Diagnostic, result.messages);
    const tokenize_diagnostics = std.ArrayList(tokenize.Diagnostic).fromOwnedSlice(tokenize_diagnostics_slice);

    // Heap-allocate AST for unified ownership model
    const ast = try gpa.create(AST);
    ast.* = .{
        .gpa = gpa,
        .env = env,
        .tokens = result.tokens,
        .store = parser.store,
        .decl_index = parser.decl_index,
        .root_node_idx = idx,
        .tokenize_diagnostics = tokenize_diagnostics,
        .parse_diagnostics = parser.diagnostics,
    };

    return ast;
}

/// Parses a single Roc file.
///
/// The caller must call `ast.deinit()` when done, which frees all internal
/// allocations AND the AST struct itself.
pub fn file(gpa: Allocator, env: *CommonEnv) Allocator.Error!*AST {
    return try runTokenDispatch(gpa, env, fileRootNode);
}

fn fileRootNode(parser: *Parser) Allocator.Error!u32 {
    try parser.runFile();
    return 0;
}

fn exprRootNode(parser: *Parser) Allocator.Error!u32 {
    const id = try parser.runExpr();
    return @intFromEnum(id);
}

/// Parses a Roc expression - for use in REPL and snapshots.
///
/// The caller must call `ast.deinit()` when done, which frees all internal
/// allocations AND the AST struct itself.
pub fn expr(gpa: Allocator, env: *CommonEnv) Allocator.Error!*AST {
    return try runTokenDispatch(gpa, env, exprRootNode);
}

fn headerRootNode(parser: *Parser) Allocator.Error!u32 {
    const id = try parser.runHeader();
    return @intFromEnum(id);
}

/// Parses a Roc header - for use in snapshots.
///
/// The caller must call `ast.deinit()` when done, which frees all internal
/// allocations AND the AST struct itself.
pub fn header(gpa: Allocator, env: *CommonEnv) Allocator.Error!*AST {
    return try runTokenDispatch(gpa, env, headerRootNode);
}

fn statementRootNode(parser: *Parser) Allocator.Error!u32 {
    const idx = try parser.runStatement();
    return @intFromEnum(idx);
}

/// Parses a single Roc statement - for use in REPL and snapshots.
///
/// The caller must call `ast.deinit()` when done, which frees all internal
/// allocations AND the AST struct itself.
pub fn statement(gpa: Allocator, env: *CommonEnv) Allocator.Error!*AST {
    return try runTokenDispatch(gpa, env, statementRootNode);
}

test "parser tests" {
    std.testing.refAllDecls(@import("AST.zig"));
    std.testing.refAllDecls(@import("Node.zig"));
    std.testing.refAllDecls(@import("NodeStore.zig"));
    std.testing.refAllDecls(@import("DeclIndex.zig"));
    std.testing.refAllDecls(@import("NumericLiteral.zig"));
    std.testing.refAllDecls(@import("Parser.zig"));
    std.testing.refAllDecls(@import("tokenize.zig"));
    std.testing.refAllDecls(@import("test/ast_node_store_test.zig"));
}

test {
    // Import test files to run their tests
    _ = @import("HTML.zig");
    _ = @import("test/ast_node_store_test.zig");
}

test "deeply nested parentheses parse stack-safely" {
    const gpa = std.testing.allocator;

    const open_parens = "(" ** 512;
    const close_parens = ")" ** 512;
    const source = open_parens ++ "1" ++ close_parens;

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try expr(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
}

test "range operators parse as binary operators" {
    const gpa = std.testing.allocator;

    for ([_][]const u8{ "1..<5", "1..=5", "1..<n + 1", "start..=finish" }) |source| {
        var env = try CommonEnv.init(gpa, source);
        defer env.deinit(gpa);

        const ast = try expr(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
    }
}

test "bare .. in expression position is a helpful parse error" {
    const gpa = std.testing.allocator;

    var env = try CommonEnv.init(gpa, "1..5");
    defer env.deinit(gpa);

    const ast = try expr(gpa, &env);
    defer ast.deinit();

    try std.testing.expect(ast.parse_diagnostics.items.len > 0);
    try std.testing.expectEqual(
        AST.Diagnostic.Tag.expr_double_dot_is_not_range,
        ast.parse_diagnostics.items[0].tag,
    );
}

fn vmExprAllocationFailureImpl(allocator: Allocator, tokens: tokenize.TokenizedBuffer) Allocator.Error!void {
    var parser = try Parser.init(tokens, allocator);
    defer parser.store.deinit();
    defer parser.decl_index.deinit();
    defer parser.diagnostics.deinit(allocator);
    defer parser.deinit();

    _ = try parser.runExpr();
}

test "parse error triggers errdefer cleanup" {
    const gpa = std.testing.allocator;
    const source = "((1";

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const messages = try gpa.alloc(tokenize.Diagnostic, 128);
    defer gpa.free(messages);

    var tokenizer = try tokenize.Tokenizer.init(&env, gpa, env.source, messages);
    var tokenizer_finished = false;
    defer if (!tokenizer_finished) tokenizer.deinit(gpa);

    try tokenizer.tokenize(gpa);

    var output = tokenizer.finishAndDeinit();
    tokenizer_finished = true;
    defer output.tokens.deinit(gpa);

    try std.testing.checkAllAllocationFailures(gpa, vmExprAllocationFailureImpl, .{output.tokens});
}

fn expectStatementParsesWithoutDiagnostics(source: []const u8) ParseTestError!void {
    const gpa = std.testing.allocator;

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try statement(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
}

fn expectFileParsesWithoutDiagnostics(source: []const u8) ParseTestError!void {
    const gpa = std.testing.allocator;

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try file(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
}

test "method and static dispatch chains parse stack-safely" {
    try expectStatementParsesWithoutDiagnostics("Dict.from_list([(\"a\", 1), (\"b\", 2)]).get(\"a\")");
    try expectStatementParsesWithoutDiagnostics("lst.map(|_| \"zzz \").join_with(\" \").trim()");
}

test "double question operator parses after static dispatch" {
    try expectStatementParsesWithoutDiagnostics("Try.Ok(\"hello\") ?? \"default\"");
}

test "where clause method function types parse stack-safely" {
    try expectFileParsesWithoutDiagnostics(
        \\A(a) : a where [a.a1 : (a, a) -> Str, a.a2 : (a, a) -> Str]
    );
}

fn vmInitAllocationFailureImpl(allocator: Allocator, tokens: tokenize.TokenizedBuffer) Allocator.Error!void {
    var parser = try Parser.init(tokens, allocator);
    defer parser.store.deinit();
    defer parser.decl_index.deinit();
    defer parser.diagnostics.deinit(allocator);
    defer parser.deinit();
}

test "Parser.init cleans up partial allocations on OOM" {
    const gpa = std.testing.allocator;
    const source = "Test := []";

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const messages = try gpa.alloc(tokenize.Diagnostic, 128);
    defer gpa.free(messages);

    var tokenizer = try tokenize.Tokenizer.init(&env, gpa, env.source, messages);
    var tokenizer_finished = false;
    defer if (!tokenizer_finished) tokenizer.deinit(gpa);

    try tokenizer.tokenize(gpa);

    var output = tokenizer.finishAndDeinit();
    tokenizer_finished = true;
    defer output.tokens.deinit(gpa);

    try std.testing.checkAllAllocationFailures(gpa, vmInitAllocationFailureImpl, .{output.tokens});
}

test "parse diagnostic report handles invalid mutable identifier spelling" {
    const gpa = std.testing.allocator;
    const source =
        \\{
        \\    test_fn = |l| {
        \\        var $total = 0
        \\        for e in l {
        \\            var _$temp = [e]
        \\            $total = $total + e
        \\        }
        \\        $total
        \\    }
        \\    test_fn([1, 2])
        \\}
    ;

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try expr(gpa, &env);
    defer ast.deinit();

    try std.testing.expect(ast.parse_diagnostics.items.len > 0);

    for (ast.parse_diagnostics.items) |diag| {
        var report = try ast.parseDiagnosticToReport(&env, diag, gpa, "test");
        defer report.deinit();
    }
}

test "regression B212: parameterized type arguments accept bare function types" {
    const gpa = std.testing.allocator;
    const source =
        \\BoxedFn : Box(Str -> Str)
        \\BoxedParenFn : Box((Str -> Str))
        \\ResultFn : Result(Str -> Str, Str -> Str)
        \\
        \\main : {}
        \\main = {}
    ;

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try file(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
}

test "parser records top-level type declaration dependencies" {
    const gpa = std.testing.allocator;
    const source =
        \\A : (B, Mod.C) -> D
        \\B : {}
        \\D : {}
    ;

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try file(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const parsed_file = ast.store.getFile();
    const decls = ast.decl_index.scopeDecls(parsed_file.scope);
    for (decls) |decl_idx| {
        const decl = ast.decl_index.decls.items[@intFromEnum(decl_idx)];
        if (decl.kind != .type_alias) continue;
        const name_ident = decl.name_ident orelse continue;
        if (!std.mem.eql(u8, env.getIdent(name_ident), "A")) continue;

        const deps = ast.decl_index.typeDependencies(decl.type_dependencies);
        try std.testing.expectEqual(@as(usize, 3), deps.len);

        const first = ast.decl_index.typeDependencySegments(deps[0]);
        try std.testing.expectEqual(@as(usize, 1), first.len);
        try std.testing.expectEqualStrings("B", env.getIdent(first[0]));

        const second = ast.decl_index.typeDependencySegments(deps[1]);
        try std.testing.expectEqual(@as(usize, 2), second.len);
        try std.testing.expectEqualStrings("Mod", env.getIdent(second[0]));
        try std.testing.expectEqualStrings("C", env.getIdent(second[1]));

        const third = ast.decl_index.typeDependencySegments(deps[2]);
        try std.testing.expectEqual(@as(usize, 1), third.len);
        try std.testing.expectEqualStrings("D", env.getIdent(third[0]));
        return;
    }

    return error.ExpectedTypeDecl;
}

test "parser records nested associated owner paths" {
    const gpa = std.testing.allocator;
    const source =
        \\Parent := [P].{
        \\    Nested := [N].{
        \\        val = 1
        \\    }
        \\}
    ;

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try file(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    var found_value = false;
    for (ast.decl_index.decls.items) |decl| {
        if (decl.kind != .value) continue;
        const name_ident = decl.name_ident orelse continue;
        if (!std.mem.eql(u8, env.getIdent(name_ident), "val")) continue;

        const owner_path = decl.owner_type_path orelse return error.MissingOwnerPath;
        const owner = ast.decl_index.type_paths.items[@intFromEnum(owner_path)];
        try std.testing.expectEqualStrings("Nested", env.getIdent(owner.name));
        const parent_path = owner.parent orelse return error.MissingParentPath;
        const parent = ast.decl_index.type_paths.items[@intFromEnum(parent_path)];
        try std.testing.expectEqualStrings("Parent", env.getIdent(parent.name));

        const assoc_decls = ast.decl_index.assocValueDecls(owner_path, name_ident);
        try std.testing.expectEqual(@as(usize, 1), assoc_decls.count());
        found_value = true;
    }

    try std.testing.expect(found_value);
}

test "parser keeps block-local type paths lexically distinct" {
    const gpa = std.testing.allocator;
    const source =
        \\first = {
        \\    T := [First].{
        \\        Inner := [FirstInner]
        \\    }
        \\    1
        \\}
        \\
        \\second = {
        \\    T := [Second].{
        \\        Inner := [SecondInner]
        \\    }
        \\    2
        \\}
    ;

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try file(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    var first_t_path: ?DeclIndex.TypePathIdx = null;
    var second_t_path: ?DeclIndex.TypePathIdx = null;
    var first_inner_path: ?DeclIndex.TypePathIdx = null;
    var second_inner_path: ?DeclIndex.TypePathIdx = null;

    for (ast.decl_index.decls.items) |decl| {
        const name_ident = decl.name_ident orelse continue;
        const name = env.getIdent(name_ident);
        if (std.mem.eql(u8, name, "T")) {
            if (first_t_path == null) {
                first_t_path = decl.type_path orelse return error.MissingFirstTPath;
            } else if (second_t_path == null) {
                second_t_path = decl.type_path orelse return error.MissingSecondTPath;
            }
        } else if (std.mem.eql(u8, name, "Inner")) {
            if (first_inner_path == null) {
                first_inner_path = decl.type_path orelse return error.MissingFirstInnerPath;
            } else if (second_inner_path == null) {
                second_inner_path = decl.type_path orelse return error.MissingSecondInnerPath;
            }
        }
    }

    try std.testing.expect(first_t_path != null);
    try std.testing.expect(second_t_path != null);
    try std.testing.expect(first_inner_path != null);
    try std.testing.expect(second_inner_path != null);

    try std.testing.expect(@intFromEnum(first_t_path.?) != @intFromEnum(second_t_path.?));
    try std.testing.expect(@intFromEnum(first_inner_path.?) != @intFromEnum(second_inner_path.?));
}

test "parser does not create a type path for malformed associated type headers" {
    const gpa = std.testing.allocator;
    const source =
        \\Outer := [Outer].{
        \\    Broken(a := [Broken]
        \\    ok = 1
        \\}
    ;

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try file(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expect(ast.parse_diagnostics.items.len > 0);

    const outer_ident = env.findIdent("Outer") orelse return error.MissingOuterIdent;
    const broken_ident = env.findIdent("Broken") orelse return error.MissingBrokenIdent;
    try std.testing.expectEqual(null, ast.decl_index.findTypePathBySegments(&.{ outer_ident, broken_ident }));

    for (ast.decl_index.decls.items) |decl| {
        const name_ident = decl.name_ident orelse continue;
        if (!std.mem.eql(u8, env.getIdent(name_ident), "Broken")) continue;

        switch (decl.kind) {
            .type_alias, .nominal, .@"opaque" => return error.MalformedHeaderRecordedTypeDecl,
            else => {},
        }
    }
}
