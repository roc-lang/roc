//! Converts Roc source code into an Abstract Syntax Tree (AST) through tokenization and parsing.
//!
//! This module provides the entry point for the parsing phase of compilation, transforming
//! raw source text into a structured AST representation that subsequent compiler phases can process.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const tracy = @import("tracy");

pub const tokenize = @import("tokenize.zig");
pub const tokenize_iter = @import("tokenize.zig");

const CommonEnv = base.CommonEnv;

/// **AST.Parser**
pub const Parser = @import("Parser.zig");

/// **AST.Node**
pub const Node = @import("Node.zig");

/// Represents the intermediate representation or Abstract Syntax Tree (AST) of a parsed Roc file.
pub const AST = @import("AST.zig");

/// Parses a single Roc file. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parse(env: *CommonEnv, gpa: std.mem.Allocator) !AST {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Initialize AST
    var ast = try AST.initCapacity(gpa, 100);
    errdefer ast.deinit(gpa);

    // Placeholder for diagnostics (not used in new parser)
    var messages: [128]tokenize.Diagnostic = undefined;

    // Create parser and parse the file
    var parser = try Parser.init(env, gpa, env.source, messages[0..], &ast, &ast.byte_slices);
    defer parser.deinit();

    // Parse the entire file
    _ = try parser.parseFile();

    return ast;
}

/// Parses a Roc expression - only for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseExpr(env: *CommonEnv, gpa: std.mem.Allocator) !AST {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Initialize AST
    var ast = try AST.initCapacity(gpa, 50);
    errdefer ast.deinit(gpa);

    // Placeholder for diagnostics
    var messages: [128]tokenize.Diagnostic = undefined;

    // Create parser and parse expression
    var parser = try Parser.init(env, gpa, env.source, messages[0..], &ast, &ast.byte_slices);
    defer parser.deinit();

    ast.root_node_idx = @intCast(@intFromEnum(try parser.parseExprFromSource(messages[0..])));

    return ast;
}

/// Parses a Roc Header - only for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseHeader(env: *CommonEnv, gpa: std.mem.Allocator) !AST {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Initialize AST
    var ast = try AST.initCapacity(gpa, 50);
    errdefer ast.deinit(gpa);

    // Placeholder for diagnostics
    var messages: [128]tokenize.Diagnostic = undefined;

    // Create parser and parse header
    var parser = try Parser.init(env, gpa, env.source, messages[0..], &ast, &ast.byte_slices);
    defer parser.deinit();

    _ = try parser.parseHeader();

    return ast;
}

/// Parses a single Roc statement for use in snapshots. The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parseStatement(env: *CommonEnv, gpa: std.mem.Allocator) !AST {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Initialize AST
    var ast = try AST.initCapacity(gpa, 50);
    errdefer ast.deinit(gpa);

    // Placeholder for diagnostics
    var messages: [128]tokenize.Diagnostic = undefined;

    // Create parser and parse statement
    var parser = try Parser.init(env, gpa, env.source, messages[0..], &ast, &ast.byte_slices);
    defer parser.deinit();

    _ = try parser.parseStmt();

    return ast;
}

test "parser tests" {
    std.testing.refAllDecls(@import("AST.zig"));
    std.testing.refAllDecls(@import("HTML.zig"));
    std.testing.refAllDecls(@import("Node.zig"));
    std.testing.refAllDecls(@import("Parser.zig"));
    std.testing.refAllDecls(@import("tokenize.zig"));
    std.testing.refAllDecls(@import("test/parse_test.zig"));
    std.testing.refAllDecls(@import("test/snapshot_comparison_test.zig"));
    std.testing.refAllDecls(@import("test/test_string_parsing.zig"));
}
