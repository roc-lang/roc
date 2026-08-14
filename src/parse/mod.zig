//! Converts Roc source code into an Abstract Syntax Tree (AST) through tokenization and parsing.
//!
//! This module provides the entry point for the parsing phase of compilation, transforming
//! raw source text into a structured AST representation that subsequent compiler phases can process.

const std = @import("std");
const base = @import("base");
const tracy = @import("tracy");

pub const tokenize = @import("tokenize.zig");

/// Single source of truth for the string/char escape alphabet.
pub const escape = @import("escape.zig");

const Allocator = std.mem.Allocator;
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

fn topLevelStatementRootNode(parser: *Parser) Allocator.Error!u32 {
    const idx = try parser.runTopLevelStatement();
    return @intFromEnum(idx);
}

/// Parses a single Roc statement - for use in REPL and snapshots.
///
/// The caller must call `ast.deinit()` when done, which frees all internal
/// allocations AND the AST struct itself.
pub fn statement(gpa: Allocator, env: *CommonEnv) Allocator.Error!*AST {
    return try runTokenDispatch(gpa, env, statementRootNode);
}

/// Parses a single top-level Roc statement - for use in the REPL, which
/// synthesizes a module and so accepts top-level-only statements like `import`.
///
/// The caller must call `ast.deinit()` when done, which frees all internal
/// allocations AND the AST struct itself.
pub fn statementTopLevel(gpa: Allocator, env: *CommonEnv) Allocator.Error!*AST {
    return try runTokenDispatch(gpa, env, topLevelStatementRootNode);
}

test "parser tests" {
    std.testing.refAllDecls(@import("AST.zig"));
    std.testing.refAllDecls(@import("Node.zig"));
    std.testing.refAllDecls(@import("NodeStore.zig"));
    std.testing.refAllDecls(@import("DeclIndex.zig"));
    std.testing.refAllDecls(@import("NumericLiteral.zig"));
    std.testing.refAllDecls(@import("Parser.zig"));
    std.testing.refAllDecls(@import("tokenize.zig"));
    std.testing.refAllDecls(@import("escape.zig"));
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

test "pipe question suffix precedence distinguishes empty call" {
    // Repro for https://github.com/roc-lang/roc/issues/10510
    const gpa = std.testing.allocator;
    const source = "(a |> f()?, a |> f?, a |> f(x)?)";

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try expr(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const root = ast.store.getExpr(@enumFromInt(ast.root_node_idx));
    try std.testing.expectEqual(.tuple, std.meta.activeTag(root));
    const items = ast.store.exprSlice(root.tuple.items);
    try std.testing.expectEqual(@as(usize, 3), items.len);

    // `a |> f()?` is `(a |> f)?`: the question suffix owns the pipe.
    const called_target = ast.store.getExpr(items[0]);
    try std.testing.expectEqual(.suffix_single_question, std.meta.activeTag(called_target));
    const called_target_pipe = ast.store.getExpr(called_target.suffix_single_question.expr);
    try std.testing.expectEqual(.arrow_call, std.meta.activeTag(called_target_pipe));

    // `a |> f?` is `a |> (f?)`: the pipe owns the question-suffixed target.
    const suffixed_target = ast.store.getExpr(items[1]);
    try std.testing.expectEqual(.arrow_call, std.meta.activeTag(suffixed_target));
    const target = ast.store.getExpr(suffixed_target.arrow_call.right);
    try std.testing.expectEqual(.suffix_single_question, std.meta.activeTag(target));

    // Explicit target arguments are also inside the pipe result being unwrapped.
    const target_with_arg = ast.store.getExpr(items[2]);
    try std.testing.expectEqual(.suffix_single_question, std.meta.activeTag(target_with_arg));
    const target_with_arg_pipe = ast.store.getExpr(target_with_arg.suffix_single_question.expr);
    try std.testing.expectEqual(.arrow_call, std.meta.activeTag(target_with_arg_pipe));
}

test "whitespace-separated postfix after pipe applies to pipe result" {
    // Repro for https://github.com/roc-lang/roc/issues/10517
    const gpa = std.testing.allocator;
    const source = "(a |> f().inside(), a |> f() .spaced(), a |> f()\t.tabbed(), a |> f()\n.line_broken(), a |> f() .field, a |> f()\t.0, a |> f() .?optional)";

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try expr(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const root = ast.store.getExpr(@enumFromInt(ast.root_node_idx));
    try std.testing.expectEqual(.tuple, std.meta.activeTag(root));
    const items = ast.store.exprSlice(root.tuple.items);
    try std.testing.expectEqual(@as(usize, 7), items.len);

    const adjacent = ast.store.getExpr(items[0]);
    try std.testing.expectEqual(.arrow_call, std.meta.activeTag(adjacent));
    try std.testing.expectEqual(.method_call, std.meta.activeTag(ast.store.getExpr(adjacent.arrow_call.right)));

    for (items[1..4]) |item| {
        const whitespace_separated = ast.store.getExpr(item);
        try std.testing.expectEqual(.method_call, std.meta.activeTag(whitespace_separated));
        try std.testing.expectEqual(.arrow_call, std.meta.activeTag(ast.store.getExpr(whitespace_separated.method_call.receiver)));
    }

    const field_access = ast.store.getExpr(items[4]);
    try std.testing.expectEqual(.field_access, std.meta.activeTag(field_access));
    try std.testing.expectEqual(.arrow_call, std.meta.activeTag(ast.store.getExpr(field_access.field_access.receiver)));

    const tuple_access = ast.store.getExpr(items[5]);
    try std.testing.expectEqual(.tuple_access, std.meta.activeTag(tuple_access));
    try std.testing.expectEqual(.arrow_call, std.meta.activeTag(ast.store.getExpr(tuple_access.tuple_access.expr)));

    const optional_field_access = ast.store.getExpr(items[6]);
    try std.testing.expectEqual(.field_access, std.meta.activeTag(optional_field_access));
    try std.testing.expectEqual(.arrow_call, std.meta.activeTag(ast.store.getExpr(optional_field_access.field_access.receiver)));
    try std.testing.expectEqual(
        AST.FieldAccessMode.optional,
        ast.store.fieldAccessSegmentSlice(optional_field_access.field_access.segments)[0].mode,
    );
}

test "uppercase qualified value lookup ignores trivia before dot" {
    const gpa = std.testing.allocator;
    const source =
        "(Blub.go(), Blub\n .go(), Blub\n .Inner\n .go(), " ++
        "Blub\n .go()\n .next(), (Blub).go())";

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try expr(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const root = ast.store.getExpr(@enumFromInt(ast.root_node_idx));
    try std.testing.expectEqual(.tuple, std.meta.activeTag(root));
    const items = ast.store.exprSlice(root.tuple.items);
    try std.testing.expectEqual(@as(usize, 5), items.len);

    const qualifier_counts = [_]usize{ 1, 1, 2 };
    for (items[0..3], qualifier_counts) |item, expected_qualifier_count| {
        const call = ast.store.getExpr(item);
        try std.testing.expectEqual(.apply, std.meta.activeTag(call));

        const lookup = ast.store.getExpr(call.apply.@"fn");
        try std.testing.expectEqual(.ident, std.meta.activeTag(lookup));
        const qualifiers = ast.store.tokenSlice(lookup.ident.qualifiers);
        try std.testing.expectEqual(expected_qualifier_count, qualifiers.len);
        try std.testing.expectEqualStrings(
            "Blub",
            env.getIdent(ast.tokens.resolveIdentifier(@intCast(qualifiers[0])).?),
        );
        if (qualifiers.len == 2) {
            try std.testing.expectEqualStrings(
                "Inner",
                env.getIdent(ast.tokens.resolveIdentifier(@intCast(qualifiers[1])).?),
            );
        }
        try std.testing.expectEqualStrings(
            "go",
            env.getIdent(ast.tokens.resolveIdentifier(lookup.ident.token).?),
        );
    }

    const chained_call = ast.store.getExpr(items[3]);
    try std.testing.expectEqual(.method_call, std.meta.activeTag(chained_call));
    try std.testing.expectEqualStrings(
        "next",
        env.getIdent(ast.tokens.resolveIdentifier(chained_call.method_call.method_token).?),
    );
    const chained_receiver = ast.store.getExpr(chained_call.method_call.receiver);
    try std.testing.expectEqual(.apply, std.meta.activeTag(chained_receiver));
    try std.testing.expectEqual(
        .ident,
        std.meta.activeTag(ast.store.getExpr(chained_receiver.apply.@"fn")),
    );

    const grouped_tag_call = ast.store.getExpr(items[4]);
    try std.testing.expectEqual(.method_call, std.meta.activeTag(grouped_tag_call));
    try std.testing.expectEqualStrings(
        "go",
        env.getIdent(ast.tokens.resolveIdentifier(grouped_tag_call.method_call.method_token).?),
    );
}

test "grouped pipe target ending in a field access starts a new suffix path" {
    const gpa = std.testing.allocator;

    const cases = [_]struct {
        source: []const u8,
        outer_mode: AST.FieldAccessMode,
    }{
        .{ .source = "a |> (b.c).d", .outer_mode = .required },
        .{ .source = "a |> (b.c).?d", .outer_mode = .optional },
    };

    for (cases) |case| {
        var env = try CommonEnv.init(gpa, case.source);
        defer env.deinit(gpa);

        const ast = try expr(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

        const root = ast.store.getExpr(@enumFromInt(ast.root_node_idx));
        try std.testing.expectEqual(.arrow_call, std.meta.activeTag(root));

        const outer = ast.store.getExpr(root.arrow_call.right).field_access;
        const outer_segments = ast.store.fieldAccessSegmentSlice(outer.segments);
        try std.testing.expectEqual(@as(usize, 1), outer_segments.len);
        try std.testing.expectEqual(case.outer_mode, outer_segments[0].mode);

        const inner = ast.store.getExpr(outer.receiver).field_access;
        const inner_segments = ast.store.fieldAccessSegmentSlice(inner.segments);
        try std.testing.expectEqual(@as(usize, 1), inner_segments.len);
        const inner_ident = ast.tokens.resolveIdentifier(inner_segments[0].field_token).?;
        try std.testing.expectEqualStrings("c", env.getIdent(inner_ident));
    }
}

test "optional record type fields preserve their source marker" {
    const gpa = std.testing.allocator;
    const source = "value : { x : U32, y ? : U32, z?: U32 }";

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try statement(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const stmt_idx: AST.Statement.Idx = @enumFromInt(ast.root_node_idx);
    const stmt = ast.store.getStatement(stmt_idx);
    try std.testing.expectEqual(.type_anno, std.meta.activeTag(stmt));

    const anno = ast.store.getTypeAnno(stmt.type_anno.anno);
    try std.testing.expectEqual(.record, std.meta.activeTag(anno));
    const fields = ast.store.annoRecordFieldSlice(anno.record.fields);
    try std.testing.expectEqual(@as(usize, 3), fields.len);

    const x = try ast.store.getAnnoRecordField(fields[0]);
    const y = try ast.store.getAnnoRecordField(fields[1]);
    const z = try ast.store.getAnnoRecordField(fields[2]);
    try std.testing.expectEqualStrings("x", ast.resolve(x.name));
    try std.testing.expectEqualStrings("y", ast.resolve(y.name));
    try std.testing.expectEqualStrings("z", ast.resolve(z.name));
    try std.testing.expectEqual(@as(?AST.Token.Idx, null), x.optional_mark);
    try std.testing.expectEqual(AST.Token.Tag.OpQuestion, ast.tokens.tokenTag(y.optional_mark.?));
    try std.testing.expectEqual(AST.Token.Tag.NoSpaceOpQuestion, ast.tokens.tokenTag(z.optional_mark.?));
}

test "unnamed record type fields cannot be optional" {
    const gpa = std.testing.allocator;

    for ([_][]const u8{
        "value : { _ ?: U32 }",
        "value : { _padding ?: U32 }",
    }) |source| {
        var env = try CommonEnv.init(gpa, source);
        defer env.deinit(gpa);

        const ast = try statement(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 1), ast.parse_diagnostics.items.len);
        try std.testing.expectEqual(
            AST.Diagnostic.Tag.optional_unnamed_record_field,
            ast.parse_diagnostics.items[0].tag,
        );
    }
}

test "optional field access parses as a one-segment field path" {
    const gpa = std.testing.allocator;
    const source = "record.?field";

    var env = try CommonEnv.init(gpa, source);
    defer env.deinit(gpa);

    const ast = try expr(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const expr_idx: AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
    const access = ast.store.getExpr(expr_idx).field_access;
    const segments = ast.store.fieldAccessSegmentSlice(access.segments);
    try std.testing.expectEqual(@as(usize, 1), segments.len);
    try std.testing.expectEqual(AST.FieldAccessMode.optional, segments[0].mode);
    try std.testing.expectEqual(
        AST.Token.Tag.NoSpaceDotQuestionLowerIdent,
        ast.tokens.tokenTag(segments[0].field_token),
    );
    const field_ident = ast.tokens.resolveIdentifier(segments[0].field_token).?;
    try std.testing.expectEqualStrings("field", env.getIdent(field_ident));

    const receiver = ast.store.getExpr(access.receiver);
    try std.testing.expectEqual(.ident, std.meta.activeTag(receiver));
    try std.testing.expectEqualStrings("record", ast.resolve(receiver.ident.token));
}

test "optional field access binds before try propagation and defaulting" {
    const gpa = std.testing.allocator;

    {
        var env = try CommonEnv.init(gpa, "record.?outer.?inner?");
        defer env.deinit(gpa);

        const ast = try expr(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
        const root_idx: AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const root = ast.store.getExpr(root_idx);
        try std.testing.expectEqual(.suffix_single_question, std.meta.activeTag(root));

        const path = ast.store.getExpr(root.suffix_single_question.expr).field_access;
        const segments = ast.store.fieldAccessSegmentSlice(path.segments);
        try std.testing.expectEqual(@as(usize, 2), segments.len);
        try std.testing.expectEqual(AST.FieldAccessMode.optional, segments[0].mode);
        try std.testing.expectEqual(AST.FieldAccessMode.optional, segments[1].mode);
    }

    {
        var env = try CommonEnv.init(gpa, "record.?field ?? fallback");
        defer env.deinit(gpa);

        const ast = try expr(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
        const root_idx: AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const root = ast.store.getExpr(root_idx);
        try std.testing.expectEqual(.bin_op, std.meta.activeTag(root));
        try std.testing.expectEqual(AST.Token.Tag.OpDoubleQuestion, ast.tokens.tokenTag(root.bin_op.operator));
        const path = ast.store.getExpr(root.bin_op.left).field_access;
        try std.testing.expectEqual(AST.FieldAccessMode.optional, ast.store.fieldAccessSegmentSlice(path.segments)[0].mode);
    }
}

test "mixed required and optional field path is one source-ordered AST node" {
    const gpa = std.testing.allocator;
    var env = try CommonEnv.init(gpa, "a.b.?c.d.?e.f");
    defer env.deinit(gpa);

    const ast = try expr(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const root: AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
    const path = ast.store.getExpr(root).field_access;
    const segments = ast.store.fieldAccessSegmentSlice(path.segments);
    try std.testing.expectEqual(@as(usize, 5), segments.len);
    const expected_modes = [_]AST.FieldAccessMode{ .required, .optional, .required, .optional, .required };
    const expected_names = [_][]const u8{ "b", "c", "d", "e", "f" };
    for (segments, expected_modes, expected_names) |segment, expected_mode, expected_name| {
        try std.testing.expectEqual(expected_mode, segment.mode);
        const field_ident = ast.tokens.resolveIdentifier(segment.field_token).?;
        try std.testing.expectEqualStrings(expected_name, env.getIdent(field_ident));
    }
    try std.testing.expectEqualStrings("a", ast.resolve(ast.store.getExpr(path.receiver).ident.token));
    // Reserved root, receiver, and the one path node. No synthetic field-name
    // identifier expressions are allocated.
    try std.testing.expectEqual(@as(usize, 3), ast.store.nodeCount());

    var tree = base.SExprTree.init(gpa);
    defer tree.deinit();
    try ast.store.getExpr(root).pushToSExprTree(gpa, &env, ast, &tree);

    var output: std.Io.Writer.Allocating = .init(gpa);
    defer output.deinit();
    try tree.toStringPretty(&output.writer, .skip_linecol);
    try std.testing.expect(std.mem.find(u8, output.written(), "(mode \"required\")") != null);
    try std.testing.expect(std.mem.find(u8, output.written(), "(mode \"optional\")") != null);
}

test "parentheses and non-field suffixes form field path boundaries" {
    const gpa = std.testing.allocator;

    {
        var env = try CommonEnv.init(gpa, "(a.?b).c");
        defer env.deinit(gpa);

        const ast = try expr(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
        const root: AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const c = ast.store.getExpr(root).field_access;
        const c_segments = ast.store.fieldAccessSegmentSlice(c.segments);
        try std.testing.expectEqual(@as(usize, 1), c_segments.len);
        try std.testing.expectEqual(AST.FieldAccessMode.required, c_segments[0].mode);

        const grouped = ast.store.getExpr(c.receiver).tuple;
        try std.testing.expectEqual(@as(u32, 1), grouped.items.span.len);
        const grouped_expr = ast.store.exprSlice(grouped.items)[0];
        const grouped_path = ast.store.getExpr(grouped_expr).field_access;
        try std.testing.expectEqual(AST.FieldAccessMode.optional, ast.store.fieldAccessSegmentSlice(grouped_path.segments)[0].mode);
    }

    {
        var env = try CommonEnv.init(gpa, "a.?b.0.c");
        defer env.deinit(gpa);

        const ast = try expr(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
        const root: AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const c = ast.store.getExpr(root).field_access;
        const c_segments = ast.store.fieldAccessSegmentSlice(c.segments);
        try std.testing.expectEqual(@as(usize, 1), c_segments.len);
        try std.testing.expectEqual(AST.FieldAccessMode.required, c_segments[0].mode);
        const tuple_access = ast.store.getExpr(c.receiver).tuple_access;
        const inner_path = ast.store.getExpr(tuple_access.expr).field_access;
        try std.testing.expectEqual(AST.FieldAccessMode.optional, ast.store.fieldAccessSegmentSlice(inner_path.segments)[0].mode);
    }
}

test "malformed optional field access reports the accessor diagnostic" {
    const gpa = std.testing.allocator;

    for ([_][]const u8{
        "record.?",
        "record.? field",
        "record.?Field",
    }) |source| {
        var env = try CommonEnv.init(gpa, source);
        defer env.deinit(gpa);

        const ast = try expr(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 1), ast.parse_diagnostics.items.len);
        try std.testing.expectEqual(
            AST.Diagnostic.Tag.expr_dot_suffix_not_allowed,
            ast.parse_diagnostics.items[0].tag,
        );
    }
}

test "optional function fields must be propagated before application" {
    const gpa = std.testing.allocator;

    {
        var env = try CommonEnv.init(gpa, "record.?callback(arg)");
        defer env.deinit(gpa);

        const ast = try expr(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 1), ast.parse_diagnostics.items.len);
        try std.testing.expectEqual(
            AST.Diagnostic.Tag.optional_field_access_cannot_be_called_directly,
            ast.parse_diagnostics.items[0].tag,
        );
    }

    {
        var env = try CommonEnv.init(gpa, "record.?callback?(arg)");
        defer env.deinit(gpa);

        const ast = try expr(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

        const root_idx: AST.Expr.Idx = @enumFromInt(ast.root_node_idx);
        const root = ast.store.getExpr(root_idx);
        try std.testing.expectEqual(.apply, std.meta.activeTag(root));
        const propagated = ast.store.getExpr(root.apply.@"fn");
        try std.testing.expectEqual(.suffix_single_question, std.meta.activeTag(propagated));
        const path = ast.store.getExpr(propagated.suffix_single_question.expr).field_access;
        try std.testing.expectEqual(AST.FieldAccessMode.optional, ast.store.fieldAccessSegmentSlice(path.segments)[0].mode);
    }

    {
        var env = try CommonEnv.init(gpa, "record.?container.callback(arg).field");
        defer env.deinit(gpa);

        const ast = try expr(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

        const field = ast.store.getExpr(@enumFromInt(ast.root_node_idx)).field_access;
        try std.testing.expectEqual(AST.FieldAccessMode.required, ast.store.fieldAccessSegmentSlice(field.segments)[0].mode);
        const method = ast.store.getExpr(field.receiver).method_call;
        const receiver_path = ast.store.getExpr(method.receiver).field_access;
        try std.testing.expectEqual(AST.FieldAccessMode.optional, ast.store.fieldAccessSegmentSlice(receiver_path.segments)[0].mode);
    }

    {
        var env = try CommonEnv.init(gpa, "record.?callback?(arg).field");
        defer env.deinit(gpa);

        const ast = try expr(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

        const field = ast.store.getExpr(@enumFromInt(ast.root_node_idx)).field_access;
        try std.testing.expectEqual(AST.FieldAccessMode.required, ast.store.fieldAccessSegmentSlice(field.segments)[0].mode);
        const apply = ast.store.getExpr(field.receiver).apply;
        const propagated = ast.store.getExpr(apply.@"fn");
        try std.testing.expectEqual(.suffix_single_question, std.meta.activeTag(propagated));
        const path = ast.store.getExpr(propagated.suffix_single_question.expr).field_access;
        try std.testing.expectEqual(AST.FieldAccessMode.optional, ast.store.fieldAccessSegmentSlice(path.segments)[0].mode);
    }
}

test "deep optional field access chains parse stack-safely" {
    const gpa = std.testing.allocator;
    const depth = 4096;

    var source = std.ArrayList(u8).empty;
    defer source.deinit(gpa);
    try source.appendSlice(gpa, "record");
    for (0..depth) |_| try source.appendSlice(gpa, ".?field");

    var env = try CommonEnv.init(gpa, source.items);
    defer env.deinit(gpa);

    const ast = try expr(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const path = ast.store.getExpr(@enumFromInt(ast.root_node_idx)).field_access;
    const segments = ast.store.fieldAccessSegmentSlice(path.segments);
    try std.testing.expectEqual(@as(usize, depth), segments.len);
    for (segments) |segment| {
        try std.testing.expectEqual(AST.FieldAccessMode.optional, segment.mode);
        const field_ident = ast.tokens.resolveIdentifier(segment.field_token).?;
        try std.testing.expectEqualStrings("field", env.getIdent(field_ident));
    }
    try std.testing.expectEqual(.ident, std.meta.activeTag(ast.store.getExpr(path.receiver)));
    try std.testing.expectEqual(@as(usize, 3), ast.store.nodeCount());
}

test "deep mixed field access chains stay flat and source-ordered" {
    const gpa = std.testing.allocator;
    const depth = 4096;

    var source = std.ArrayList(u8).empty;
    defer source.deinit(gpa);
    try source.ensureTotalCapacity(gpa, "record".len + depth * ".?field".len);
    try source.appendSlice(gpa, "record");
    for (0..depth) |i| {
        try source.appendSlice(gpa, if (i % 2 == 0) ".?field" else ".field");
    }

    var env = try CommonEnv.init(gpa, source.items);
    defer env.deinit(gpa);

    const ast = try expr(gpa, &env);
    defer ast.deinit();

    try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
    try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);

    const path = ast.store.getExpr(@enumFromInt(ast.root_node_idx)).field_access;
    const segments = ast.store.fieldAccessSegmentSlice(path.segments);
    try std.testing.expectEqual(@as(usize, depth), segments.len);
    for (segments, 0..) |segment, i| {
        const expected: AST.FieldAccessMode = if (i % 2 == 0) .optional else .required;
        try std.testing.expectEqual(expected, segment.mode);
    }
    try std.testing.expectEqual(.ident, std.meta.activeTag(ast.store.getExpr(path.receiver)));
    try std.testing.expectEqual(@as(usize, 3), ast.store.nodeCount());
}
test "dollar-prefixed record field names are rejected with a single diagnostic" {
    const gpa = std.testing.allocator;

    const Case = struct {
        source: []const u8,
        parse: *const fn (Allocator, *CommonEnv) Allocator.Error!*AST,
    };

    for ([_]Case{
        .{
            .source = "match value { { $field } => \"matched\" }",
            .parse = expr,
        },
        .{
            .source = "app [main!] { $pf: platform \"./platform/main.roc\" }",
            .parse = header,
        },
        .{
            .source = "package [Foo] { $dep: \"../dep/main.roc\" }",
            .parse = header,
        },
    }) |case| {
        var env = try CommonEnv.init(gpa, case.source);
        defer env.deinit(gpa);

        const ast = try case.parse(gpa, &env);
        defer ast.deinit();

        try std.testing.expectEqual(@as(usize, 0), ast.tokenize_diagnostics.items.len);
        try std.testing.expectEqual(@as(usize, 1), ast.parse_diagnostics.items.len);
        try std.testing.expectEqual(
            AST.Diagnostic.Tag.record_field_name_cannot_be_var,
            ast.parse_diagnostics.items[0].tag,
        );
    }
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

        if (decl.kind == .type_alias or decl.kind == .nominal or decl.kind == .@"opaque") {
            return error.MalformedHeaderRecordedTypeDecl;
        }
    }
}
