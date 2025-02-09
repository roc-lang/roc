const std = @import("std");

const tokenize = @import("parse/tokenize.zig");
const TokenIndex = tokenize.TokenIndex;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const IR = @import("parse/ir.zig");
const NodeList = IR.NodeList;
const Diagnostic = IR.Diagnostic;
const GenCatData = @import("GenCatData");
const Parser = @import("parse/parser.zig");

source: []const u8,
tokens: TokenizedBuffer,
store: IR.NodeStore,
errors: []const Diagnostic,

/// Parses a single Roc file.  The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parse(allocator: std.mem.Allocator, source: []const u8) !IR {
    var messages: [128]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var gc = try GenCatData.init(allocator);
    defer gc.deinit();
    var tokenizer = try tokenize.Tokenizer.init(source, msg_slice, &gc, allocator);
    try tokenizer.tokenize();
    const result = tokenizer.finish_and_deinit();

    if (result.messages.len > 0) {
        std.debug.print("Found these issues while parsing:\n{any}", .{result.messages});
    }

    var parser = try Parser.init(allocator, result.tokens);
    defer parser.deinit();

    try parser.parseFile();

    const errors = try parser.diagnostics.toOwnedSlice();

    return .{
        .source = source,
        .tokens = result.tokens,
        .store = parser.store,
        .errors = errors,
    };
}
