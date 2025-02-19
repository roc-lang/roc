/// This is just a silly fuzz test to start getting the infra setup.
/// It shows the basic that other fuzz tests likely should build off of.
///
/// Note: Compiling the fuzz tests requires llvm and does not currently work in our nix shell on all systems.
///
/// To run:
///  1. zig build fuzz-tokenize
///  2. ./zig-out/AFLplusplus/bin/afl-fuzz -i src/fuzz-corpus/tokenize/ -o /tmp/tokenize-out/ zig-out/bin/fuzz-tokenize
///
/// Other afl commands also avilable in `./zig-out/AFLplusplus/bin`
///
const std = @import("std");
const tokenize = @import("check/parse/tokenize.zig");
const GenCatData = @import("GenCatData");

pub export fn zig_fuzz_init() void {}

pub export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    zig_fuzz_test_inner(buf, len, false);
}

pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    // We reinitialize the gpa on every loop of the fuzzer.
    // This enables the gpa to do leak checking on each iteration.
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        _ = gpa_impl.deinit();
    }
    const gpa = gpa_impl.allocator();

    var gcd = GenCatData.init(gpa) catch @panic("OOM");
    defer gcd.deinit();

    var buf_slice = buf[0..@intCast(len)];

    // Initial tokenization.
    var messages: [32]tokenize.Diagnostic = undefined;
    var tokenizer = tokenize.Tokenizer.init(buf_slice, &messages, &gcd, gpa);
    tokenizer.tokenize();
    var output = tokenizer.finish_and_deinit();
    defer output.tokens.deinit();

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

    const buf2 = rebuild_buffer(buf_slice, &output.tokens.tokens, gpa) catch |err| switch (err) {
        error.Unsupported => return,
        error.OutOfMemory => std.debug.panic("OOM", .{}),
    };
    defer buf2.deinit();

    if (debug) {
        std.debug.print("Original:\n==========\n{s}\n==========\n\n", .{buf_slice});
        std.debug.print("Intermediate:\n==========\n{s}\n==========\n\n", .{buf2.items});
    }

    // Second tokenization.
    tokenizer = tokenize.Tokenizer.init(buf2.items, &messages, &gcd, gpa);
    tokenizer.tokenize();
    var output2 = tokenizer.finish_and_deinit();
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
        same = same and (token.tag == token2.tag);
        same = same and (token.offset == token2.offset);
        same = same and (token.length == token2.length);
    }

    if (!same) {
        var prefix_len: usize = 0;
        var suffix_len: usize = 0;
        while (prefix_len < output.tokens.tokens.len and prefix_len < output2.tokens.tokens.len) : (prefix_len += 1) {
            const token = output.tokens.tokens.get(prefix_len);
            const token2 = output2.tokens.tokens.get(prefix_len);
            if (!std.meta.eql(token, token2)) {
                break;
            }
        }

        while (suffix_len < output.tokens.tokens.len - prefix_len and suffix_len < output2.tokens.tokens.len - prefix_len) : (suffix_len += 1) {
            const token = output.tokens.tokens.get(output.tokens.tokens.len - suffix_len - 1);
            const token2 = output2.tokens.tokens.get(output2.tokens.tokens.len - suffix_len - 1);
            if (!std.meta.eql(token, token2)) {
                break;
            }
        }

        std.debug.print("...\n", .{});
        for (prefix_len..output.tokens.tokens.len - suffix_len) |token_index| {
            const token = output.tokens.tokens.get(token_index);
            std.debug.print("\x1b[31m\t- {any}\x1b[0m: {s}\n", .{ token, buf_slice[token.offset..][0..token.length] });
        }
        for (prefix_len..output2.tokens.tokens.len - suffix_len) |token_index| {
            const token = output2.tokens.tokens.get(token_index);
            std.debug.print("\x1b[32m\t+ {any}\x1b[0m: {s}\n", .{ token, buf2.items[token.offset..][0..token.length] });
        }
        std.debug.print("...\n", .{});

        std.debug.assert(same);
    }
}

fn rebuild_buffer(buf: []const u8, tokens: *tokenize.Token.List, alloc: std.mem.Allocator) !std.ArrayList(u8) {
    // Create an arraylist to store the new buffer.
    var buf2 = try std.ArrayList(u8).initCapacity(alloc, buf.len);
    errdefer buf2.deinit();

    // Dump back to buffer.
    // Here we are just printing in the simplest way possible.
    var last_end: usize = 0;
    for (0..tokens.len) |token_index| {
        const token = tokens.get(token_index);
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
                try buf2.append(' ');
            } else {
                try buf2.append(buf[i]);
            }
        }
        if (token.tag == .EndOfFile) {
            break;
        }
        last_end = token.offset + token.length;
        switch (token.tag) {
            .EndOfFile, .Newline => unreachable,

            .Float => {
                try buf2.append('0');
                try buf2.append('.');
                for (2..token.length) |_| {
                    try buf2.append('1');
                }
            },
            .String => {
                try buf2.append('"');
                for (1..token.length - 1) |_| {
                    try buf2.append('~');
                }
                try buf2.append('"');
            },
            .SingleQuote => {
                try buf2.append('\'');
                for (1..token.length) |_| {
                    try buf2.append('~');
                }
                try buf2.append('\'');
            },
            .StringBegin => {
                try buf2.append('"');
                for (1..token.length - 1) |_| {
                    try buf2.append('~');
                }
                try buf2.append('$');
            },
            .StringPart => {
                for (0..token.length - 1) |_| {
                    try buf2.append('~');
                }
                try buf2.append('$');
            },
            .StringEnd => {
                for (1..token.length) |_| {
                    try buf2.append('~');
                }
                try buf2.append('"');
            },
            .SingleQuoteBegin => {
                try buf2.append('\'');
                for (1..token.length - 1) |_| {
                    try buf2.append('~');
                }
                try buf2.append('$');
            },
            .SingleQuotePart => {
                for (0..token.length - 1) |_| {
                    try buf2.append('~');
                }
                try buf2.append('$');
            },
            .SingleQuoteEnd => {
                for (1..token.length) |_| {
                    try buf2.append('~');
                }
                try buf2.append('\'');
            },

            .UpperIdent => {
                try buf2.append('Z');
                for (1..token.length) |_| {
                    try buf2.append('z');
                }
            },
            .LowerIdent => {
                for (0..token.length) |_| {
                    try buf2.append('z');
                }
            },
            .Underscore => {
                std.debug.assert(token.length == 1);
                try buf2.append('_');
            },
            .DotInt => {
                try buf2.append('.');
                for (1..token.length) |_| {
                    try buf2.append('1');
                }
            },
            .DotLowerIdent => {
                try buf2.append('.');
                for (1..token.length) |_| {
                    try buf2.append('z');
                }
            },
            .DotUpperIdent => {
                try buf2.append('.');
                try buf2.append('Z');
                for (2..token.length) |_| {
                    try buf2.append('z');
                }
            },
            .NoSpaceDotInt => {
                try buf2.append('.');
                for (1..token.length) |_| {
                    try buf2.append('1');
                }
            },
            .NoSpaceDotLowerIdent => {
                try buf2.append('.');
                for (1..token.length) |_| {
                    try buf2.append('z');
                }
            },
            .NoSpaceDotUpperIdent => {
                try buf2.append('.');
                try buf2.append('Z');
                for (2..token.length) |_| {
                    try buf2.append('z');
                }
            },
            .NoSpaceOpenRound => {
                try buf2.append('(');
            },

            .NamedUnderscore => {
                try buf2.append('_');
                for (1..token.length) |_| {
                    try buf2.append('z');
                }
            },
            .OpaqueName => {
                try buf2.append('@');
                for (1..token.length) |_| {
                    try buf2.append('z');
                }
            },
            .Int => {
                for (0..token.length) |_| {
                    try buf2.append('1');
                }
            },

            .OpenRound => {
                std.debug.assert(token.length == 1);
                try buf2.append('(');
            },
            .CloseRound => {
                std.debug.assert(token.length == 1);
                try buf2.append(')');
            },
            .OpenSquare => {
                std.debug.assert(token.length == 1);
                try buf2.append('[');
            },
            .CloseSquare => {
                std.debug.assert(token.length == 1);
                try buf2.append(']');
            },
            .OpenCurly => {
                std.debug.assert(token.length == 1);
                try buf2.append('{');
            },
            .CloseCurly => {
                std.debug.assert(token.length == 1);
                try buf2.append('}');
            },

            .OpPlus => {
                std.debug.assert(token.length == 1);
                try buf2.append('+');
            },
            .OpStar => {
                std.debug.assert(token.length == 1);
                try buf2.append('*');
            },
            .OpPizza => {
                std.debug.assert(token.length == 2);
                try buf2.append('|');
                try buf2.append('>');
            },
            .OpAssign => {
                std.debug.assert(token.length == 1);
                try buf2.append('=');
            },
            .OpBinaryMinus => {
                std.debug.assert(token.length == 1);
                try buf2.append('-');
            },
            .OpUnaryMinus => {
                std.debug.assert(token.length == 1);
                try buf2.append('-');
            },
            .OpNotEquals => {
                std.debug.assert(token.length == 2);
                try buf2.append('!');
                try buf2.append('=');
            },
            .OpBang => {
                std.debug.assert(token.length == 1);
                try buf2.append('!');
            },
            .OpAnd => {
                std.debug.assert(token.length == 2);
                try buf2.append('&');
                try buf2.append('&');
            },
            .OpAmpersand => {
                std.debug.assert(token.length == 1);
                try buf2.append('&');
            },
            .OpQuestion => {
                std.debug.assert(token.length == 1);
                try buf2.append('?');
            },
            .OpOr => {
                std.debug.assert(token.length == 2);
                try buf2.append('|');
                try buf2.append('|');
            },
            .OpBar => {
                std.debug.assert(token.length == 1);
                try buf2.append('|');
            },
            .OpDoubleSlash => {
                std.debug.assert(token.length == 2);
                try buf2.append('/');
                try buf2.append('/');
            },
            .OpSlash => {
                std.debug.assert(token.length == 1);
                try buf2.append('/');
            },
            .OpPercent => {
                std.debug.assert(token.length == 1);
                try buf2.append('%');
            },
            .OpCaret => {
                std.debug.assert(token.length == 1);
                try buf2.append('^');
            },
            .OpGreaterThanOrEq => {
                std.debug.assert(token.length == 2);
                try buf2.append('>');
                try buf2.append('=');
            },
            .OpGreaterThan => {
                std.debug.assert(token.length == 1);
                try buf2.append('>');
            },
            .OpLessThanOrEq => {
                std.debug.assert(token.length == 2);
                try buf2.append('<');
                try buf2.append('=');
            },
            .OpBackArrow => {
                std.debug.assert(token.length == 2);
                try buf2.append('<');
                try buf2.append('-');
            },
            .OpLessThan => {
                std.debug.assert(token.length == 1);
                try buf2.append('<');
            },
            .OpEquals => {
                std.debug.assert(token.length == 2);
                try buf2.append('=');
                try buf2.append('=');
            },
            .OpColonEqual => {
                std.debug.assert(token.length == 2);
                try buf2.append(':');
                try buf2.append('=');
            },

            .Comma => {
                std.debug.assert(token.length == 1);
                try buf2.append(',');
            },
            .Dot => {
                std.debug.assert(token.length == 1);
                try buf2.append('.');
            },
            .DoubleDot => {
                std.debug.assert(token.length == 2);
                try buf2.append('.');
                try buf2.append('.');
            },
            .TripleDot => {
                std.debug.assert(token.length == 3);
                try buf2.append('.');
                try buf2.append('.');
                try buf2.append('.');
            },
            .OpColon => {
                std.debug.assert(token.length == 1);
                try buf2.append(':');
            },
            .OpArrow => {
                std.debug.assert(token.length == 2);
                try buf2.append('-');
                try buf2.append('>');
            },
            .OpBackslash => {
                std.debug.assert(token.length == 1);
                try buf2.append('\\');
            },

            .KwApp => {
                try buf2.appendSlice("app");
            },
            .KwAs => {
                try buf2.appendSlice("as");
            },
            .KwCrash => {
                try buf2.appendSlice("crash");
            },
            .KwDbg => {
                try buf2.appendSlice("dbg");
            },
            .KwDebug => {
                try buf2.appendSlice("debug");
            },
            .KwElse => {
                try buf2.appendSlice("else");
            },
            .KwExpect => {
                try buf2.appendSlice("expect");
            },
            .KwExposes => {
                try buf2.appendSlice("exposes");
            },
            .KwGenerates => {
                try buf2.appendSlice("generates");
            },
            .KwHas => {
                try buf2.appendSlice("has");
            },
            .KwHosted => {
                try buf2.appendSlice("hosted");
            },
            .KwIf => {
                try buf2.appendSlice("if");
            },
            .KwImplements => {
                try buf2.appendSlice("implements");
            },
            .KwImport => {
                try buf2.appendSlice("import");
            },
            .KwImports => {
                try buf2.appendSlice("imports");
            },
            .KwInterface => {
                try buf2.appendSlice("interface");
            },
            .KwIs => {
                try buf2.appendSlice("is");
            },
            .KwModule => {
                try buf2.appendSlice("module");
            },
            .KwPackage => {
                try buf2.appendSlice("package");
            },
            .KwPackages => {
                try buf2.appendSlice("packages");
            },
            .KwPlatform => {
                try buf2.appendSlice("platform");
            },
            .KwProvides => {
                try buf2.appendSlice("provides");
            },
            .KwRequires => {
                try buf2.appendSlice("requires");
            },
            .KwThen => {
                try buf2.appendSlice("then");
            },
            .KwTo => {
                try buf2.appendSlice("to");
            },
            .KwWhen => {
                try buf2.appendSlice("when");
            },
            .KwWhere => {
                try buf2.appendSlice("where");
            },
            .KwWith => {
                try buf2.appendSlice("with");
            },

            // If the input has malformed tokens, we don't want to assert anything about it (yet)
            .MalformedIntBadSuffix,
            .MalformedFloatBadSuffix,
            .MalformedInvalidUnicodeEscapeSequence,
            .MalformedInvalidEscapeSequence,
            .MalformedUnicodeIdent,
            .MalformedDotUnicodeIdent,
            .MalformedNoSpaceDotUnicodeIdent,
            .MalformedUnknownToken,
            => {
                return error.Unsupported;
            },
        }
    }
    return buf2;
}
