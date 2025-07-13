//! Converts Roc source code into an Abstract Syntax Tree (AST) through tokenization and parsing.
//!
//! This module provides the entry point for the parsing phase of compilation, transforming
//! raw source text into a structured AST representation that subsequent compiler phases can process.

const std = @import("std");
const testing = std.testing;

const base = @import("../base.zig");
const tracy = @import("../tracy.zig");
const tokenize = @import("parse/tokenize.zig");
const TokenIndex = tokenize.TokenIndex;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const NodeList = AST.NodeList;
const Diagnostic = AST.Diagnostic;
const Parser = @import("parse/Parser.zig");
const exitOnOom = @import("../collections/utils.zig").exitOnOom;

pub const Node = @import("parse/Node.zig");
pub const NodeStore = @import("parse/NodeStore.zig");

/// Represents the intermediate representation or Abstract Syntax Tree (AST) of a parsed Roc file.
pub const AST = @import("parse/AST.zig");

test {
    _ = @import("parse/test/ast_node_store_test.zig");
}

fn runParse(env: *base.ModuleEnv, source: []const u8, parserCall: *const fn (*Parser) u32) AST {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Calculate and store line starts for diagnostic position calculation
    env.calcLineStarts(source) catch |err| exitOnOom(err);

    var messages: [128]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var tokenizer = tokenize.Tokenizer.init(env, source, msg_slice);
    tokenizer.tokenize();
    const result = tokenizer.finishAndDeinit();

    var parser = Parser.init(result.tokens);
    defer parser.deinit();

    const idx = parserCall(&parser);

    const tokenize_diagnostics_slice = env.gpa.dupe(tokenize.Diagnostic, result.messages) catch |err| exitOnOom(err);
    const tokenize_diagnostics = std.ArrayListUnmanaged(tokenize.Diagnostic).fromOwnedSlice(tokenize_diagnostics_slice);
    const parse_diagnostics = parser.diagnostics;

    return .{
        .env = env,
        .tokens = result.tokens,
        .store = parser.store,
        .root_node_idx = idx,
        .tokenize_diagnostics = tokenize_diagnostics,
        .parse_diagnostics = parse_diagnostics,
    };
}

/// Parses a single Roc file.  The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parse(env: *base.ModuleEnv, source: []const u8) AST {
    return runParse(env, source, parseFileAndReturnIdx);
}

fn parseFileAndReturnIdx(parser: *Parser) u32 {
    parser.parseFile();
    return 0;
}

fn parseExprAndReturnIdx(parser: *Parser) u32 {
    const id = parser.parseExpr();
    return @intFromEnum(id);
}

/// Parses a Roc expression - only for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseExpr(env: *base.ModuleEnv, source: []const u8) AST {
    return runParse(env, source, parseExprAndReturnIdx);
}

fn parseHeaderAndReturnIdx(parser: *Parser) u32 {
    const id = parser.parseHeader();
    return @intFromEnum(id);
}

/// Parses a Roc Header - only for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseHeader(env: *base.ModuleEnv, source: []const u8) AST {
    return runParse(env, source, parseHeaderAndReturnIdx);
}

fn parseStatementAndReturnIdx(parser: *Parser) u32 {
    const maybe_statement_idx = parser.parseStmt();
    if (maybe_statement_idx) |idx| {
        return @intFromEnum(idx);
    }
    @panic("Statement to parse was not found in AST");
}

/// Parses a Roc statement - only for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseStatement(env: *base.ModuleEnv, source: []const u8) AST {
    return runParse(env, source, parseStatementAndReturnIdx);
}
