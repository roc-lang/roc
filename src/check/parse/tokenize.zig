const std = @import("std");
const Allocator = std.mem.Allocator;
const collections = @import("../../collections.zig");
const base = @import("../../base.zig");
const tracy = @import("../../tracy.zig");
const reporting = @import("../../reporting.zig");

const Tokenizer = @import("tokenize/Tokenizer.zig");

pub const Token = @import("tokenize/Token.zig");
pub const Diagnostic = @import("tokenize/Diagnostic.zig");
pub const TokenizedBuffer = @import("tokenize/Buffer.zig");

const exitOnOom = collections.utils.exitOnOom;

/// The output type returned by the tokenizer containing tokens and diagnostics.
pub const TokenOutput = Tokenizer.TokenOutput;

/// Tokenize source code and return tokens along with any diagnostics encountered.
pub fn runTokenize(env: *base.ModuleEnv, source: []const u8) TokenOutput {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Calculate and store line starts for diagnostic position calculation
    env.calcLineStarts(source) catch |err| exitOnOom(err);

    var messages: [128]Diagnostic = undefined;
    const msg_slice = messages[0..];
    var tokenizer = Tokenizer.init(env, source, msg_slice);
    tokenizer.tokenize();

    return tokenizer.finishAndDeinit();
}

fn testTokenization(gpa: std.mem.Allocator, input: []const u8, expected: []const Token.Tag) !void {
    var messages: [10]Diagnostic = undefined;

    var env = base.ModuleEnv.init(gpa, input);
    defer env.deinit();

    var tokenizer = Tokenizer.init(&env, input, &messages);
    defer tokenizer.deinit();

    tokenizer.tokenize();
    const tokenizedBuffer = tokenizer.output;
    const tokens = tokenizedBuffer.tokens.items(.tag);

    try std.testing.expectEqual(tokens[tokens.len - 1], Token.Tag.EndOfFile);
    try std.testing.expectEqualSlices(Token.Tag, expected[0..expected.len], tokens[0 .. tokens.len - 1]);

    checkTokenizerInvariants(gpa, input, false);
}

/// Assert the invariants of the tokenizer are held.
pub fn checkTokenizerInvariants(gpa: std.mem.Allocator, input: []const u8, debug: bool) void {
    var env = base.ModuleEnv.init(gpa, input);
    defer env.deinit();

    // Initial tokenization.
    var messages: [32]Diagnostic = undefined;
    var tokenizer = Tokenizer.init(&env, input, &messages);
    tokenizer.tokenize();
    var output = tokenizer.finishAndDeinit();
    defer output.tokens.deinit();

    if (debug) {
        std.debug.print("Original:\n==========\n{s}\n==========\n\n", .{input});
    }

    if (debug) {
        std.debug.print("Before:\n", .{});
        for (0..output.tokens.tokens.len) |token_index| {
            const token = output.tokens.tokens.get(token_index);
            std.debug.print("\t{any}\n", .{token});
        }
        std.debug.print("\n\n", .{});
    }

    // TODO: apply errors from messages to buffer below.
    // For now, just skip on tokenizer finding a failure.
    if (output.messages.len != 0) {
        return;
    }

    var buf2 = rebuildBufferForTesting(input, &output.tokens, gpa) catch |err| switch (err) {
        error.Unsupported => return,
        error.OutOfMemory => std.debug.panic("OOM", .{}),
    };
    defer buf2.deinit(gpa);

    if (debug) {
        std.debug.print("Intermediate:\n==========\n{s}\n==========\n\n", .{buf2.items});
    }

    // Second tokenization.
    tokenizer = Tokenizer.init(&env, buf2.items, &messages);
    tokenizer.tokenize();
    var output2 = tokenizer.finishAndDeinit();
    defer output2.tokens.deinit();

    if (debug) {
        std.debug.print("After:\n", .{});
        for (0..output2.tokens.tokens.len) |token_index| {
            const token = output2.tokens.tokens.get(token_index);
            std.debug.print("\t{any}\n", .{token});
        }
        std.debug.print("\n\n", .{});
    }
    // Assert same.
    var same = output.tokens.tokens.len == output2.tokens.tokens.len;
    for (0..output.tokens.tokens.len) |token_index| {
        if (token_index >= output2.tokens.tokens.len) {
            same = false;
            break;
        }
        const token = output.tokens.tokens.get(token_index);
        const token2 = output2.tokens.tokens.get(token_index);
        const region1 = output.tokens.resolve(@intCast(token_index));
        const region2 = output2.tokens.resolve(@intCast(token_index));
        const length1 = region1.end.offset - region1.start.offset;
        const length2 = region2.end.offset - region2.start.offset;
        same = same and (token.tag == token2.tag);
        same = same and (token.offset == token2.offset);
        same = same and (length1 == length2);
    }

    if (!same) {
        var prefix_len: usize = 0;
        var suffix_len: usize = 0;
        while (prefix_len < output.tokens.tokens.len and prefix_len < output2.tokens.tokens.len) : (prefix_len += 1) {
            const token = output.tokens.tokens.get(prefix_len);
            const token2 = output2.tokens.tokens.get(prefix_len);
            if (token.tag != token2.tag or token.offset != token2.offset) {
                break;
            }
        }

        while (suffix_len < output.tokens.tokens.len - prefix_len and suffix_len < output2.tokens.tokens.len - prefix_len) : (suffix_len += 1) {
            const token = output.tokens.tokens.get(output.tokens.tokens.len - suffix_len - 1);
            const token2 = output2.tokens.tokens.get(output2.tokens.tokens.len - suffix_len - 1);
            if (token.tag != token2.tag or token.offset != token2.offset) {
                break;
            }
        }

        std.debug.print("...\n", .{});
        for (prefix_len..output.tokens.tokens.len - suffix_len) |token_index| {
            const region = output.tokens.resolve(@intCast(token_index));
            const token = output.tokens.tokens.get(token_index);
            std.debug.print("\x1b[31m\t- {any}\x1b[0m: {s}\n", .{ token, input[token.offset..region.end.offset] });
        }
        for (prefix_len..output2.tokens.tokens.len - suffix_len) |token_index| {
            const region = output2.tokens.resolve(@intCast(token_index));
            const token = output2.tokens.tokens.get(token_index);
            std.debug.print("\x1b[32m\t+ {any}\x1b[0m: {s}\n", .{ token, buf2.items[token.offset..region.end.offset] });
        }
        std.debug.print("...\n", .{});

        std.debug.assert(same);
    }
}

fn rebuildBufferForTesting(buf: []const u8, tokens: *TokenizedBuffer, alloc: std.mem.Allocator) !std.ArrayListUnmanaged(u8) {
    // Create an arraylist to store the new buffer.
    var buf2 = try std.ArrayListUnmanaged(u8).initCapacity(alloc, buf.len);
    errdefer buf2.deinit(alloc);

    // Dump back to buffer.
    // Here we are just printing in the simplest way possible.
    var last_end: usize = 0;
    var prev_token_tag: Token.Tag = .EndOfFile; // placeholder
    for (0..tokens.tokens.len) |token_index| {
        const token = tokens.tokens.get(token_index);
        // EndOfFile and NewLine are special, handle them early.
        // Unlike other tokens they do not store a correct offset and length
        // EndOfFile consumes the entire file. Newline stores the indentation level of the next line.
        if (token.tag == .Newline) {
            // Newlines will be copied with other whitespace
            continue;
        }

        // Copy over limited whitespace.
        // TODO: Long term, we should switch to dummy whitespace, but currently, Roc still has WSS.
        for (last_end..token.offset) |i| {
            // Leave tabs and newlines alone, they are special to roc.
            // Replace everything else with spaces.
            if (buf[i] != '\t' and buf[i] != '\r' and buf[i] != '\n' and buf[i] != '#') {
                try buf2.append(alloc, ' ');
            } else {
                try buf2.append(alloc, buf[i]);
            }
        }
        if (token.tag == .EndOfFile) {
            break;
        }
        const region = tokens.resolve(@intCast(token_index));
        std.debug.assert(region.start.offset == token.offset);
        std.debug.assert(region.end.offset >= region.start.offset);
        std.debug.assert(region.end.offset <= buf.len);
        std.debug.assert(region.start.offset >= last_end);
        last_end = region.end.offset;
        const length = region.end.offset - region.start.offset;
        switch (token.tag) {
            .EndOfFile, .Newline => unreachable,

            .Float => {
                try buf2.append(alloc, '0');
                try buf2.append(alloc, '.');
                for (2..length) |_| {
                    try buf2.append(alloc, '1');
                }
            },
            .SingleQuote => {
                try buf2.append(alloc, '\'');
                for (1..length) |_| {
                    try buf2.append(alloc, '~');
                }
                try buf2.append(alloc, '\'');
            },
            .StringStart, .StringEnd => {
                try buf2.append(alloc, '"');
            },
            .MultilineStringStart, .MultilineStringEnd => {
                try buf2.append(alloc, '"');
                try buf2.append(alloc, '"');
                try buf2.append(alloc, '"');
            },
            .StringPart => {
                for (0..length) |_| {
                    try buf2.append(alloc, '~');
                }
            },
            .OpenStringInterpolation => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '$');
                try buf2.append(alloc, '{');
            },
            .CloseStringInterpolation => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '}');
            },

            .UpperIdent => {
                try buf2.append(alloc, 'Z');
                for (1..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .LowerIdent => {
                for (0..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .Underscore => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '_');
            },
            .DotInt => {
                try buf2.append(alloc, '.');
                for (1..length) |_| {
                    try buf2.append(alloc, '1');
                }
            },
            .DotLowerIdent => {
                try buf2.append(alloc, '.');
                for (1..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .DotUpperIdent => {
                try buf2.append(alloc, '.');
                try buf2.append(alloc, 'Z');
                for (2..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .NoSpaceDotInt => {
                try buf2.append(alloc, '.');
                for (1..length) |_| {
                    try buf2.append(alloc, '1');
                }
            },
            .NoSpaceDotLowerIdent => {
                try buf2.append(alloc, '.');
                for (1..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .NoSpaceDotUpperIdent => {
                try buf2.append(alloc, '.');
                try buf2.append(alloc, 'Z');
                for (2..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .NoSpaceOpenRound => {
                try buf2.append(alloc, '(');
            },

            .NamedUnderscore => {
                try buf2.append(alloc, '_');
                for (1..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .OpaqueName => {
                try buf2.append(alloc, '@');
                for (1..length) |_| {
                    try buf2.append(alloc, 'z');
                }
            },
            .Int => {
                // To ensure this value when reprinted tokenizes as an int, add a base if the number is 3 or more characters.
                if (length >= 3) {
                    try buf2.append(alloc, '0');
                    try buf2.append(alloc, 'x');
                    for (2..length) |_| {
                        try buf2.append(alloc, '1');
                    }
                } else {
                    for (0..length) |_| {
                        try buf2.append(alloc, '1');
                    }
                }
            },

            .OpenRound => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '(');
            },
            .CloseRound => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, ')');
            },
            .OpenSquare => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '[');
            },
            .CloseSquare => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, ']');
            },
            .OpenCurly => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '{');
            },
            .CloseCurly => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '}');
            },

            .OpPlus => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '+');
            },
            .OpStar => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '*');
            },
            .OpPizza => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '|');
                try buf2.append(alloc, '>');
            },
            .OpAssign => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '=');
            },
            .OpBinaryMinus => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '-');
            },
            .OpUnaryMinus => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '-');
            },
            .OpNotEquals => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '!');
                try buf2.append(alloc, '=');
            },
            .OpBang => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '!');
            },
            .OpAnd => {
                std.debug.assert(length == 3);
                try buf2.append(alloc, 'a');
                try buf2.append(alloc, 'n');
                try buf2.append(alloc, 'd');
            },
            .OpAmpersand => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '&');
            },
            .OpQuestion, .NoSpaceOpQuestion => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '?');
            },
            .OpDoubleQuestion => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '?');
                try buf2.append(alloc, '?');
            },
            .OpOr => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, 'o');
                try buf2.append(alloc, 'r');
            },
            .OpBar => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '|');
            },
            .OpDoubleSlash => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '/');
                try buf2.append(alloc, '/');
            },
            .OpSlash => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '/');
            },
            .OpPercent => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '%');
            },
            .OpCaret => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '^');
            },
            .OpGreaterThanOrEq => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '>');
                try buf2.append(alloc, '=');
            },
            .OpGreaterThan => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '>');
            },
            .OpLessThanOrEq => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '<');
                try buf2.append(alloc, '=');
            },
            .OpBackArrow => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '<');
                try buf2.append(alloc, '-');
            },
            .OpLessThan => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '<');
            },
            .OpEquals => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '=');
                try buf2.append(alloc, '=');
            },
            .OpColonEqual => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, ':');
                try buf2.append(alloc, '=');
            },

            .Comma => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, ',');
            },
            .Dot => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '.');
            },
            .DoubleDot => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '.');
                try buf2.append(alloc, '.');
            },
            .TripleDot => {
                std.debug.assert(length == 3);
                try buf2.append(alloc, '.');
                try buf2.append(alloc, '.');
                try buf2.append(alloc, '.');
            },
            .DotStar => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '.');
                try buf2.append(alloc, '*');
            },
            .OpColon => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, ':');
            },
            .OpArrow => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '-');
                try buf2.append(alloc, '>');
            },
            .OpFatArrow => {
                std.debug.assert(length == 2);
                try buf2.append(alloc, '=');
                try buf2.append(alloc, '>');
            },
            .OpBackslash => {
                std.debug.assert(length == 1);
                try buf2.append(alloc, '\\');
            },

            .KwApp => {
                try buf2.appendSlice(alloc, "app");
            },
            .KwAs => {
                try buf2.appendSlice(alloc, "as");
            },
            .KwCrash => {
                try buf2.appendSlice(alloc, "crash");
            },
            .KwDbg => {
                try buf2.appendSlice(alloc, "dbg");
            },
            .KwDebug => {
                try buf2.appendSlice(alloc, "debug");
            },
            .KwElse => {
                try buf2.appendSlice(alloc, "else");
            },
            .KwExpect => {
                try buf2.appendSlice(alloc, "expect");
            },
            .KwExposes => {
                try buf2.appendSlice(alloc, "exposes");
            },
            .KwExposing => {
                try buf2.appendSlice(alloc, "exposing");
            },
            .KwFor => {
                try buf2.appendSlice(alloc, "for");
            },
            .KwGenerates => {
                try buf2.appendSlice(alloc, "generates");
            },
            .KwHas => {
                try buf2.appendSlice(alloc, "has");
            },
            .KwHosted => {
                try buf2.appendSlice(alloc, "hosted");
            },
            .KwIf => {
                try buf2.appendSlice(alloc, "if");
            },
            .KwImplements => {
                try buf2.appendSlice(alloc, "implements");
            },
            .KwImport => {
                try buf2.appendSlice(alloc, "import");
            },
            .KwImports => {
                try buf2.appendSlice(alloc, "imports");
            },
            .KwIn => {
                try buf2.appendSlice(alloc, "in");
            },
            .KwInterface => {
                try buf2.appendSlice(alloc, "interface");
            },
            .KwModule => {
                try buf2.appendSlice(alloc, "module");
            },
            .KwPackage => {
                try buf2.appendSlice(alloc, "package");
            },
            .KwPackages => {
                try buf2.appendSlice(alloc, "packages");
            },
            .KwPlatform => {
                try buf2.appendSlice(alloc, "platform");
            },
            .KwProvides => {
                try buf2.appendSlice(alloc, "provides");
            },
            .KwRequires => {
                try buf2.appendSlice(alloc, "requires");
            },
            .KwReturn => {
                try buf2.appendSlice(alloc, "return");
            },
            .KwVar => {
                try buf2.appendSlice(alloc, "var");
            },
            .KwMatch => {
                try buf2.appendSlice(alloc, "match");
            },
            .KwWhere => {
                try buf2.appendSlice(alloc, "where");
            },
            .KwWith => {
                try buf2.appendSlice(alloc, "with");
            },

            // If the input has malformed tokens, we don't want to assert anything about it (yet)
            .MalformedNumberBadSuffix,
            .MalformedNumberUnicodeSuffix,
            .MalformedNumberNoDigits,
            .MalformedNumberNoExponentDigits,
            .MalformedInvalidUnicodeEscapeSequence,
            .MalformedInvalidEscapeSequence,
            .MalformedUnicodeIdent,
            .MalformedDotUnicodeIdent,
            .MalformedNoSpaceDotUnicodeIdent,
            .MalformedUnknownToken,
            .MalformedNamedUnderscoreUnicode,
            .MalformedOpaqueNameUnicode,
            .MalformedOpaqueNameWithoutName,
            => {
                return error.Unsupported;
            },
        }
        prev_token_tag = token.tag;
    }
    return buf2;
}

test "tokenizer" {
    const gpa = std.testing.allocator;
    try testTokenization(gpa, "42", &[_]Token.Tag{.Int});
    try testTokenization(gpa, "3.14", &[_]Token.Tag{.Float});
    try testTokenization(gpa, ".", &[_]Token.Tag{.Dot});
    try testTokenization(gpa, "..", &[_]Token.Tag{.DoubleDot});
    try testTokenization(gpa, "1..2", &[_]Token.Tag{ .Int, .DoubleDot, .Int });
    try testTokenization(gpa, "...", &[_]Token.Tag{.TripleDot});
    try testTokenization(gpa, "-", &[_]Token.Tag{.OpUnaryMinus});
    try testTokenization(gpa, "-42", &[_]Token.Tag{.Int});
    try testTokenization(gpa, "1e10", &[_]Token.Tag{.Float});
    try testTokenization(gpa, "_ident", &[_]Token.Tag{.NamedUnderscore});
    try testTokenization(gpa, "1..2", &[_]Token.Tag{ .Int, .DoubleDot, .Int });
    try testTokenization(gpa, "3...4", &[_]Token.Tag{ .Int, .TripleDot, .Int });
    try testTokenization(gpa, "1. .2", &[_]Token.Tag{ .Int, .Dot, .DotInt });
    try testTokenization(gpa, "1.2.3", &[_]Token.Tag{ .Float, .NoSpaceDotInt });
    try testTokenization(gpa, "match", &[_]Token.Tag{.KwMatch});
    try testTokenization(gpa, "var", &[_]Token.Tag{.KwVar});
    try testTokenization(gpa, "{a, b}", &[_]Token.Tag{ .OpenCurly, .LowerIdent, .Comma, .LowerIdent, .CloseCurly });
    try testTokenization(gpa, "\"abc\"", &[_]Token.Tag{ .StringStart, .StringPart, .StringEnd });
    try testTokenization(gpa, "\"a${b}c\"", &[_]Token.Tag{
        .StringStart,
        .StringPart,
        .OpenStringInterpolation,
        .LowerIdent,
        .CloseStringInterpolation,
        .StringPart,
        .StringEnd,
    });
    try testTokenization(
        gpa,
        \\"""abc
    ,
        &[_]Token.Tag{ .MultilineStringStart, .StringPart },
    );
    try testTokenization(
        gpa,
        \\"""abc
        \\"""def
    ,
        &[_]Token.Tag{ .MultilineStringStart, .StringPart, .Newline, .MultilineStringStart, .StringPart },
    );
    try testTokenization(
        gpa,
        \\"""abc"""
    ,
        &[_]Token.Tag{ .MultilineStringStart, .StringPart, .MultilineStringEnd },
    );
    try testTokenization(
        gpa,
        \\"""a${b}c
        \\"""def
    ,
        &[_]Token.Tag{
            .MultilineStringStart,
            .StringPart,
            .OpenStringInterpolation,
            .LowerIdent,
            .CloseStringInterpolation,
            .StringPart,
            .Newline,
            .MultilineStringStart,
            .StringPart,
        },
    );
}
