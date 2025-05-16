const std = @import("std");
const testing = std.testing;

const base = @import("../base.zig");
const tracy = @import("../tracy.zig");
const tokenize = @import("parse/tokenize.zig");
const TokenIndex = tokenize.TokenIndex;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const NodeList = IR.NodeList;
const Diagnostic = IR.Diagnostic;
const Parser = @import("parse/Parser.zig");
const exitOnOom = @import("../collections/utils.zig").exitOnOom;

/// Represents the intermediate representation or Abstract Syntax Tree (AST) of a parsed Roc file.
pub const IR = @import("parse/IR.zig");

fn runParse(env: *base.ModuleEnv, source: []const u8, parserCall: *const fn (*Parser) u32) IR {
    const trace = tracy.trace(@src());
    defer trace.end();

    var messages: [128]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var tokenizer = tokenize.Tokenizer.init(env, source, msg_slice);
    tokenizer.tokenize();
    const result = tokenizer.finishAndDeinit();

    for (result.messages) |msg| {
        _ = env.problems.append(env.gpa, .{ .tokenize = msg });
    }

    var parser = Parser.init(result.tokens);
    defer parser.deinit();

    const idx = parserCall(&parser);

    for (parser.diagnostics.items) |msg| {
        _ = env.problems.append(env.gpa, .{ .parser = msg });
    }

    const errors = parser.diagnostics.toOwnedSlice(env.gpa) catch |err| exitOnOom(err);

    return .{
        .source = source,
        .tokens = result.tokens,
        .store = parser.store,
        .errors = errors,
        .root_node_idx = idx,
    };
}

/// Parses a single Roc file.  The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parse(env: *base.ModuleEnv, source: []const u8) IR {
    return runParse(env, source, parseFileAndReturnIdx);
}

fn parseFileAndReturnIdx(parser: *Parser) u32 {
    parser.parseFile();
    return 0;
}

fn parseExprAndReturnIdx(parser: *Parser) u32 {
    const id = parser.parseExpr();
    return id.id;
}

/// Parses a Roc expression - only for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseExpr(env: *base.ModuleEnv, source: []const u8) IR {
    return runParse(env, source, parseExprAndReturnIdx);
}

fn parseHeaderAndReturnIdx(parser: *Parser) u32 {
    const id = parser.parseHeader();
    return id.id;
}

/// Parses a Roc Header - only for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseHeader(env: *base.ModuleEnv, source: []const u8) IR {
    return runParse(env, source, parseHeaderAndReturnIdx);
}

fn parseStatementAndReturnIdx(parser: *Parser) u32 {
    const statementId = parser.parseStmt();
    if (statementId) |id| {
        return id.id;
    }
    @panic("Statement to parse was not found in AST");
}

/// Parses a Roc statement - only for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseStatement(env: *base.ModuleEnv, source: []const u8) IR {
    return runParse(env, source, parseStatementAndReturnIdx);
}
