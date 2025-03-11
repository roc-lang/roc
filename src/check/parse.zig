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
    };
}

fn lineNum(newlines: std.ArrayList(usize), pos: u32) u32 {
    const pos_usize = @as(usize, @intCast(pos));

    if (newlines.items.len == 0) {
        return 0;
    }

    var lineno: u32 = 0;

    while (lineno + 1 < newlines.items.len) {
        if (newlines.items[lineno + 1] > pos_usize) {
            return lineno;
        }
        lineno += 1;
    }

    return lineno;
}

/// TODO -- I think we should change this to be a method on Diagnostic
/// and then we can have the caller use this to format to a writer
/// this would be helpful for e.g. the snapshot tool which writes to a file instead of stderr
fn tokenizeReport(allocator: std.mem.Allocator, source: []const u8, msgs: []const tokenize.Diagnostic) void {
    std.debug.print("Found the {d} following issues while parsing:\n", .{msgs.len});
    var newlines = std.ArrayList(usize).init(allocator);
    defer newlines.deinit();
    newlines.append(0) catch |err| exitOnOom(err);
    var pos: usize = 0;
    for (source) |c| {
        if (c == '\n') {
            newlines.append(pos) catch |err| exitOnOom(err);
        }
        pos += 1;
    }
    for (msgs) |message| {
        switch (message.tag) {
            .MismatchedBrace => {
                const start_line_num = lineNum(newlines, message.begin);
                const start_col = message.begin - newlines.items[start_line_num];
                const end_line_num = lineNum(newlines, message.end);
                const end_col = message.end - newlines.items[end_line_num];

                const end_index = if (end_line_num + 1 < newlines.items.len)
                    end_line_num + 1
                else
                    end_line_num;

                const src = source[newlines.items[start_line_num]..newlines.items[end_index]];
                var spaces = std.ArrayList(u8).init(allocator);
                defer spaces.deinit();
                for (0..start_col) |_| {
                    spaces.append(' ') catch |err| exitOnOom(err);
                }

                std.debug.print(
                    "TOKENIZE ERROR: ({d}:{d}-{d}:{d}) Expected the correct closing brace here:\n{s}\n{s}^\n",
                    .{ start_line_num, start_col, end_line_num, end_col, src, spaces.items },
                );
            },
            else => {
                std.debug.print("TOKENIZE ERROR: {any}\n", .{message});
            },
        }
    }
}

// TODO move this somewhere better, for now it's here to keep it simple.
fn testSExprHelper(source: []const u8, expected: []const u8) !void {
    var env = base.ModuleEnv.init(testing.allocator);
    defer env.deinit();

    // parse our source
    var parse_ast = parse(&env, source);
    defer parse_ast.deinit();
    std.testing.expectEqualSlices(IR.Diagnostic, &[_]IR.Diagnostic{}, parse_ast.errors) catch {
        std.debug.print("Tokens:\n{any}", .{parse_ast.tokens.tokens.items(.tag)});
        std.debug.panic("Test failed with parse errors", .{});
    };

    // shouldn't be required in future
    parse_ast.store.emptyScratch();

    // buffer to write our SExpr to
    var buf = std.ArrayList(u8).init(testing.allocator);
    defer buf.deinit();

    // convert the AST to our SExpr
    try parse_ast.toSExprStr(&env, buf.writer().any());

    // TODO in future we should just write the SExpr to a file and snapshot it
    // for now we are comparing strings to keep it simple
    try testing.expectEqualStrings(expected, buf.items[0..]);
}

test "example s-expr" {
    const source =
        \\module [foo, bar]
        \\
        \\foo = "hey"
        \\bar = "yo"
    ;

    const expected =
        \\(file
        \\    (header
        \\        (exposed_item (lower_ident 'foo'))
        \\        (exposed_item (lower_ident 'bar')))
        \\    (decl
        \\        (ident 'foo')
        \\        (string 'hey'))
        \\    (decl
        \\        (ident 'bar')
        \\        (string 'yo')))
    ;

    try testSExprHelper(source, expected);
}
