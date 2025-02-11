const std = @import("std");

const tokenize = @import("parse/tokenize.zig");
const TokenIndex = tokenize.TokenIndex;
const TokenizedBuffer = tokenize.TokenizedBuffer;
pub const IR = @import("parse/IR.zig");
const NodeList = IR.NodeList;
const Diagnostic = IR.Diagnostic;
const GenCatData = @import("GenCatData");
const Parser = @import("parse/Parser.zig");
const exit_on_oom = @import("../collections/exit_on_oom.zig").exit_on_oom;

source: []const u8,
tokens: TokenizedBuffer,
store: IR.NodeStore,
errors: []const Diagnostic,

/// Parses a single Roc file.  The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parse(allocator: std.mem.Allocator, source: []const u8) IR {
    var messages: [128]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var gc = GenCatData.init(allocator) catch exit_on_oom();
    defer gc.deinit();
    var tokenizer = tokenize.Tokenizer.init(source, msg_slice, &gc, allocator);
    tokenizer.tokenize();
    const result = tokenizer.finish_and_deinit();

    if (result.messages.len > 0) {
        std.debug.print("Found these issues while parsing:\n{any}", .{result.messages});
    }

    var parser = Parser.init(allocator, result.tokens);
    defer parser.deinit();

    parser.parseFile();

    const errors = parser.diagnostics.toOwnedSlice() catch exit_on_oom();

    return .{
        .source = source,
        .tokens = result.tokens,
        .store = parser.store,
        .errors = errors,
    };
}
