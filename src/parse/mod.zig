//! Converts Roc source code into an Abstract Syntax Tree (AST) through tokenization and parsing.
//!
//! This module provides the entry point for the parsing phase of compilation, transforming
//! raw source text into a structured AST representation that subsequent compiler phases can process.

const std = @import("std");
const base = @import("base");
const tracy = @import("tracy");

pub const tokenize = @import("tokenize.zig");

const Allocators = base.Allocators;
const CommonEnv = base.CommonEnv;
const Diagnostic = AST.Diagnostic;

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
/// TODO: Future enhancement - consider using allocators.scratch for temporary allocations
/// during parsing (tokenizer scratch, intermediate buffers). Currently only
/// gpa is used.
fn runParse(allocators: *Allocators, env: *CommonEnv, parserCall: *const fn (*Parser) Parser.Error!u32) Parser.Error!*AST {
    const trace = tracy.trace(@src());
    defer trace.end();

    const gpa = allocators.gpa;

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
pub fn parse(allocators: *Allocators, env: *CommonEnv) Parser.Error!*AST {
    return try runParse(allocators, env, parseFileAndReturnIdx);
}

fn parseFileAndReturnIdx(parser: *Parser) Parser.Error!u32 {
    try parser.parseFile();
    return 0;
}

fn parseExprAndReturnIdx(parser: *Parser) Parser.Error!u32 {
    const id = try parser.parseExpr();
    return @intFromEnum(id);
}

/// Parses a Roc expression - for use in REPL and snapshots.
///
/// The caller must call `ast.deinit()` when done, which frees all internal
/// allocations AND the AST struct itself.
pub fn parseExpr(allocators: *Allocators, env: *CommonEnv) Parser.Error!*AST {
    return try runParse(allocators, env, parseExprAndReturnIdx);
}

fn parseHeaderAndReturnIdx(parser: *Parser) Parser.Error!u32 {
    const id = try parser.parseHeader();
    return @intFromEnum(id);
}

/// Parses a Roc header - for use in snapshots.
///
/// The caller must call `ast.deinit()` when done, which frees all internal
/// allocations AND the AST struct itself.
pub fn parseHeader(allocators: *Allocators, env: *CommonEnv) Parser.Error!*AST {
    return try runParse(allocators, env, parseHeaderAndReturnIdx);
}

fn parseStatementAndReturnIdx(parser: *Parser) Parser.Error!u32 {
    const idx = try parser.parseStmt();
    return @intFromEnum(idx);
}

/// Parses a single Roc statement - for use in REPL and snapshots.
///
/// The caller must call `ast.deinit()` when done, which frees all internal
/// allocations AND the AST struct itself.
pub fn parseStatement(allocators: *Allocators, env: *CommonEnv) Parser.Error!*AST {
    return try runParse(allocators, env, parseStatementAndReturnIdx);
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

test "parse error triggers errdefer cleanup" {
    const gpa = std.testing.allocator;

    // Create a deeply nested expression that exceeds MAX_NESTING_LEVELS (128)
    // to trigger the TooNested error and exercise the errdefer cleanup paths
    const open_parens = "(" ** 150;
    const close_parens = ")" ** 150;
    const source = open_parens ++ "1" ++ close_parens;

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    // This should fail with TooNested error
    const result = parseExpr(&allocators, &env);
    try std.testing.expectError(error.TooNested, result);
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

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try parseExpr(&allocators, &env);
    defer ast.deinit();

    try std.testing.expect(ast.parse_diagnostics.items.len > 0);

    for (ast.parse_diagnostics.items) |diag| {
        var report = try ast.parseDiagnosticToReport(&env, diag, gpa, "test");
        defer report.deinit();
    }
}

test "bughunt B212: parameterized type arguments accept bare function types" {
    const gpa = std.testing.allocator;
    const source =
        \\module []
        \\
        \\BoxedFn : Box(Str -> Str)
        \\BoxedParenFn : Box((Str -> Str))
        \\ResultFn : Result(Str -> Str, Str -> Str)
        \\
        \\main : {}
        \\main = {}
    ;

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try parse(&allocators, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
}

test "parser records top-level type declaration dependencies" {
    const gpa = std.testing.allocator;
    const source =
        \\module []
        \\
        \\A : (B, Mod.C) -> D
        \\B : {}
        \\D : {}
    ;

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try parse(&allocators, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const file = ast.store.getFile();
    const decls = ast.decl_index.scopeDecls(file.scope);
    for (decls) |decl_idx| {
        const decl = ast.decl_index.decls.items[@intFromEnum(decl_idx)];
        if (decl.kind != .type_alias) continue;
        const name_ident = decl.name_ident orelse continue;
        if (!std.mem.eql(u8, env.getIdent(name_ident), "A")) continue;

        const deps = ast.decl_index.typeDependencies(decl.type_dependencies);
        try std.testing.expectEqual(@as(usize, 2), deps.len);
        try std.testing.expectEqualStrings("B", env.getIdent(deps[0]));
        try std.testing.expectEqualStrings("D", env.getIdent(deps[1]));
        return;
    }

    return error.ExpectedTypeDecl;
}

test "parser records nested associated owner paths" {
    const gpa = std.testing.allocator;
    const source =
        \\module []
        \\
        \\Parent := [P].{
        \\    Nested := [N].{
        \\        val = 1
        \\    }
        \\}
    ;

    var allocators: Allocators = undefined;
    allocators.initInPlace(gpa);
    defer allocators.deinit();

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try parse(&allocators, &env);
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
        try std.testing.expectEqual(@as(usize, 1), assoc_decls.len);
        found_value = true;
    }

    try std.testing.expect(found_value);
}
