//! Converts Roc source code into an Abstract Syntax Tree (AST) through tokenization and parsing.
//!
//! This module provides the entry point for the parsing phase of compilation, transforming
//! raw source text into a structured AST representation that subsequent compiler phases can process.

const std = @import("std");
const base = @import("base");
const compile = @import("compile");
const tracy = @import("../tracy.zig");
const tokenize = @import("parse/tokenize.zig");
const TokenIndex = tokenize.TokenIndex;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const NodeList = AST.NodeList;
const Diagnostic = AST.Diagnostic;
const Parser = @import("parse/Parser.zig");

pub const Node = @import("parse/Node.zig");
pub const NodeStore = @import("parse/NodeStore.zig");

/// Represents the intermediate representation or Abstract Syntax Tree (AST) of a parsed Roc file.
pub const AST = @import("parse/AST.zig");

test {
    _ = @import("parse/test/ast_node_store_test.zig");
}

fn runParse(env: *compile.ModuleEnv, parserCall: *const fn (*Parser) std.mem.Allocator.Error!u32) std.mem.Allocator.Error!AST {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Calculate and store line starts for diagnostic position calculation
    try env.calcLineStarts();

    var messages: [128]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var tokenizer = try tokenize.Tokenizer.init(env, env.source, msg_slice);
    try tokenizer.tokenize();
    const result = tokenizer.finishAndDeinit();

    var parser = try Parser.init(result.tokens);
    defer parser.deinit();

    const idx = try parserCall(&parser);

    const tokenize_diagnostics_slice = try env.gpa.dupe(tokenize.Diagnostic, result.messages);
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
pub fn parse(env: *compile.ModuleEnv) std.mem.Allocator.Error!AST {
    return try runParse(env, parseFileAndReturnIdx);
}

fn parseFileAndReturnIdx(parser: *Parser) std.mem.Allocator.Error!u32 {
    try parser.parseFile();
    return 0;
}

fn parseExprAndReturnIdx(parser: *Parser) std.mem.Allocator.Error!u32 {
    const id = try parser.parseExpr();
    return @intFromEnum(id);
}

/// Parses a Roc expression - only for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseExpr(env: *compile.ModuleEnv) std.mem.Allocator.Error!AST {
    return try runParse(env, parseExprAndReturnIdx);
}

fn parseHeaderAndReturnIdx(parser: *Parser) std.mem.Allocator.Error!u32 {
    const id = try parser.parseHeader();
    return @intFromEnum(id);
}

/// Parses a Roc Header - only for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseHeader(env: *compile.ModuleEnv) std.mem.Allocator.Error!AST {
    return try runParse(env, parseHeaderAndReturnIdx);
}

fn parseStatementAndReturnIdx(parser: *Parser) std.mem.Allocator.Error!u32 {
    const maybe_statement_idx = try parser.parseStmt();
    if (maybe_statement_idx) |idx| {
        return @intFromEnum(idx);
    }
    @panic("Statement to parse was not found in AST");
}

/// Parses a single Roc statement for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseStatement(env: *compile.ModuleEnv) std.mem.Allocator.Error!AST {
    return try runParse(env, parseStatementAndReturnIdx);
}
