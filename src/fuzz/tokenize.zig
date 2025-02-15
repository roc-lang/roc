/// This is just a silly fuzz test to start getting the infra setup.
/// It shows the basic that other fuzz tests likely should build off of.
///
/// Note: Compiling the fuzz tests requires llvm and does not currently work in our nix shell on all systems.
///
/// To run:
///  1. zig build fuzz-tokenize
///  2. ./zig-out/AFLplusplus/bin/afl-fuzz -i src/fuzz/tokenize-corpus/ -o /tmp/tokenize-out/ zig-out/bin/fuzz-tokenize
///
/// Other afl commands also avilable in `./zig-out/AFLplusplus/bin`
///
const std = @import("std");
const tokenize = @import("tokenize");
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
    var tokenizer = tokenize.Tokenizer.init(buf_slice, &messages, &gcd, gpa) catch @panic("OOM");
    tokenizer.tokenize() catch {
        defer tokenizer.deinit();
        @panic("OOM");
    };
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

    // Dump back to buffer.
    // Here we are just printing in the simplest way possible.
    var last_end: usize = 0;
    for (0..output.tokens.tokens.len) |token_index| {
        const token = output.tokens.tokens.get(token_index);
        // EndOfFile and NewLine are special, handle them early.
        // Unlike other tokens they do not store a correct offset and length
        // EndOfFile consumes the entire file. Newline stores the indentation level of the next line.
        if (token.tag == .EndOfFile) {
            break;
        }
        if (token.tag == .Newline) {
            // Newlines will be copied with other whitespace
            continue;
        }

        // Copy over limited whitespace.
        // TODO: Long term, we should switch to dummy whitespace, but currently, Roc still has WSS.
        for (last_end..token.offset) |i| {
            // Leave tabs and newlines alone, they are special to roc.
            // Replace everything else with spaces.
            if (buf_slice[i] != '\t' and buf_slice[i] != '\r' and buf_slice[i] != '\n') {
                buf_slice[i] = ' ';
            }
        }
        last_end = token.offset + token.length;

        switch (token.tag) {
            .EndOfFile, .Newline => unreachable,

            .Float => {
                buf_slice[token.offset] = '0';
                buf_slice[token.offset + 1] = '.';
                for (2..token.length) |i| {
                    buf_slice[token.offset + i] = '1';
                }
            },
            .String => {
                buf_slice[token.offset] = '"';
                for (1..token.length - 1) |i| {
                    buf_slice[token.offset + i] = '~';
                }
                buf_slice[token.offset + token.length - 1] = '"';
            },
            .SingleQuote => {
                buf_slice[token.offset] = '\'';
                for (1..token.length) |i| {
                    buf_slice[token.offset + i] = '~';
                }
                buf_slice[token.offset + token.length - 1] = '\'';
            },
            .StringBegin => {
                buf_slice[token.offset] = '"';
                for (1..token.length) |i| {
                    buf_slice[token.offset + i] = '~';
                }
            },
            .StringPart => {
                for (0..token.length) |i| {
                    buf_slice[token.offset + i] = '~';
                }
                // This is technically ambiguous, but good enough for now.
                const next_token = output.tokens.tokens.get(token_index + 1);
                if (next_token.tag != .OpenCurly) {
                    // No following interpolation, end the string.
                    buf_slice[token.offset + token.length - 1] = '"';
                }
            },
            .SingleQuoteBegin => {
                buf_slice[token.offset] = '\'';
                for (1..token.length) |i| {
                    buf_slice[token.offset + i] = '~';
                }
            },
            .SingleQuotePart => {
                for (0..token.length) |i| {
                    buf_slice[token.offset + i] = '~';
                }
                // This is technically ambiguous, but good enough for now.
                const next_token = output.tokens.tokens.get(token_index + 1);
                if (next_token.tag != .OpenCurly) {
                    // No following interpolation, end the string.
                    buf_slice[token.offset + token.length - 1] = '\'';
                }
            },

            .UpperIdent => {
                buf_slice[token.offset] = 'Z';
                for (1..token.length) |i| {
                    buf_slice[token.offset + i] = 'z';
                }
            },
            .LowerIdent => {
                for (0..token.length) |i| {
                    buf_slice[token.offset + i] = 'z';
                }
            },
            .Underscore => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '_';
            },
            .DotInt => {
                buf_slice[token.offset] = '.';
                for (1..token.length) |i| {
                    buf_slice[token.offset + i] = '1';
                }
            },
            .DotLowerIdent => {
                buf_slice[token.offset] = '.';
                for (1..token.length) |i| {
                    buf_slice[token.offset + i] = 'z';
                }
            },
            .DotUpperIdent => {
                buf_slice[token.offset] = '.';
                buf_slice[token.offset + 1] = 'Z';
                for (2..token.length) |i| {
                    buf_slice[token.offset + i] = 'z';
                }
            },
            .NoSpaceDotInt => {
                buf_slice[token.offset] = '.';
                for (1..token.length) |i| {
                    buf_slice[token.offset + i] = '1';
                }
            },
            .NoSpaceDotLowerIdent => {
                buf_slice[token.offset] = '.';
                for (1..token.length) |i| {
                    buf_slice[token.offset + i] = 'z';
                }
            },
            .NoSpaceDotUpperIdent => {
                buf_slice[token.offset] = '.';
                buf_slice[token.offset + 1] = 'Z';
                for (2..token.length) |i| {
                    buf_slice[token.offset + i] = 'z';
                }
            },

            .NamedUnderscore => {
                buf_slice[token.offset] = '_';
                for (1..token.length) |i| {
                    buf_slice[token.offset + i] = 'z';
                }
            },
            .OpaqueName => {
                buf_slice[token.offset] = '@';
                for (1..token.length) |i| {
                    buf_slice[token.offset + i] = 'z';
                }
            },
            .Int => {
                for (0..token.length) |i| {
                    buf_slice[token.offset + i] = '1';
                }
            },

            .OpenRound => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '(';
            },
            .CloseRound => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = ')';
            },
            .OpenSquare => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '[';
            },
            .CloseSquare => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = ']';
            },
            .OpenCurly => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '{';

                // Feels like tokenization is missing the `$` token.
                if (token_index != 0) {
                    const prev_token = output.tokens.tokens.get(token_index - 1);
                    switch (prev_token.tag) {
                        .StringBegin, .StringPart => {
                            buf_slice[token.offset - 1] = '$';
                        },
                        else => {},
                    }
                }
            },
            .CloseCurly => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '}';
            },

            .OpPlus => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '+';
            },
            .OpStar => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '*';
            },
            .OpPizza => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = '|';
                buf_slice[token.offset + 1] = '>';
            },
            .OpAssign => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '=';
            },
            .OpBinaryMinus => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '-';
            },
            .OpUnaryMinus => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '-';
            },
            .OpNotEquals => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = '!';
                buf_slice[token.offset + 1] = '=';
            },
            .OpBang => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '!';
            },
            .OpAnd => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = '&';
                buf_slice[token.offset + 1] = '&';
            },
            .OpAmpersand => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '&';
            },
            .OpQuestion => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '?';
            },
            .OpOr => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = '|';
                buf_slice[token.offset + 1] = '|';
            },
            .OpBar => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '|';
            },
            .OpDoubleSlash => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = '/';
                buf_slice[token.offset + 1] = '/';
            },
            .OpSlash => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '/';
            },
            .OpPercent => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '%';
            },
            .OpCaret => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '^';
            },
            .OpGreaterThanOrEq => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = '>';
                buf_slice[token.offset + 1] = '=';
            },
            .OpGreaterThan => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '>';
            },
            .OpLessThanOrEq => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = '<';
                buf_slice[token.offset + 1] = '=';
            },
            .OpBackArrow => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = '<';
                buf_slice[token.offset + 1] = '-';
            },
            .OpLessThan => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '<';
            },
            .OpEquals => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = '=';
                buf_slice[token.offset + 1] = '=';
            },
            .OpColonEqual => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = ':';
                buf_slice[token.offset + 1] = '=';
            },

            .Comma => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = ',';
            },
            .Dot => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '.';
            },
            .DoubleDot => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = '.';
                buf_slice[token.offset + 1] = '.';
            },
            .TripleDot => {
                std.debug.assert(token.length == 3);
                buf_slice[token.offset] = '.';
                buf_slice[token.offset + 1] = '.';
                buf_slice[token.offset + 2] = '.';
            },
            .OpColon => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = ':';
            },
            .OpArrow => {
                std.debug.assert(token.length == 2);
                buf_slice[token.offset] = '-';
                buf_slice[token.offset + 1] = '>';
            },
            .OpBackslash => {
                std.debug.assert(token.length == 1);
                buf_slice[token.offset] = '\\';
            },

            .KwApp => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "app");
            },
            .KwAs => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "as");
            },
            .KwCrash => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "crash");
            },
            .KwDbg => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "dbg");
            },
            .KwDebug => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "debug");
            },
            .KwElse => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "else");
            },
            .KwExpect => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "expect");
            },
            .KwExposes => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "exposes");
            },
            .KwGenerates => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "generates");
            },
            .KwHas => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "has");
            },
            .KwHosted => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "hosted");
            },
            .KwIf => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "if");
            },
            .KwImplements => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "implements");
            },
            .KwImport => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "import");
            },
            .KwImports => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "imports");
            },
            .KwInterface => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "interface");
            },
            .KwIs => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "is");
            },
            .KwModule => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "module");
            },
            .KwPackage => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "package");
            },
            .KwPackages => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "packages");
            },
            .KwPlatform => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "platform");
            },
            .KwProvides => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "provides");
            },
            .KwRequires => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "requires");
            },
            .KwThen => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "then");
            },
            .KwTo => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "to");
            },
            .KwWhen => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "when");
            },
            .KwWhere => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "where");
            },
            .KwWith => {
                std.mem.copyForwards(u8, buf_slice[token.offset..][0..token.length], "with");
            },
        }
    }

    if (debug) {
        std.debug.print("Intermediate:\n==========\n{s}\n==========\n\n", .{buf_slice});
    }

    // Second tokenization.
    tokenizer = tokenize.Tokenizer.init(buf_slice, &messages, &gcd, gpa) catch @panic("OOM");
    tokenizer.tokenize() catch {
        defer tokenizer.deinit();
        @panic("OOM");
    };
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
    std.debug.assert(output.tokens.tokens.len == output2.tokens.tokens.len);
    for (0..output.tokens.tokens.len) |token_index| {
        const token = output.tokens.tokens.get(token_index);
        const token2 = output2.tokens.tokens.get(token_index);
        std.debug.assert(token.tag == token2.tag);
        std.debug.assert(token.offset == token2.offset);
        std.debug.assert(token.length == token2.length);
    }
}
