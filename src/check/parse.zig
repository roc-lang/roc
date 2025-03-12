const std = @import("std");
const testing = std.testing;

const base = @import("../base.zig");
const tokenize = @import("parse/tokenize.zig");
const TokenIndex = tokenize.TokenIndex;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const NodeList = IR.NodeList;
const Diagnostic = IR.Diagnostic;
const Parser = @import("parse/Parser.zig");
const exitOnOom = @import("../collections/utils.zig").exitOnOom;

/// Represents the intermediate representation or Abstract Syntax Tree (AST) of a parsed Roc file.
pub const IR = @import("parse/IR.zig");

/// Parses a single Roc file.  The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parse(env: *base.ModuleEnv, source: []const u8) IR {
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

    parser.parseFile();

    for (parser.diagnostics.items) |msg| {
        _ = env.problems.append(env.gpa, .{ .parser = msg });
    }

    const errors = parser.diagnostics.toOwnedSlice(env.gpa) catch |err| exitOnOom(err);

    return .{
        .source = source,
        .tokens = result.tokens,
        .store = parser.store,
        .errors = errors,
        .newlines = null,
    };
}
