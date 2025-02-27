const std = @import("std");
const testing = std.testing;

const base = @import("../base.zig");
const tokenize = @import("parse/tokenize.zig");
const TokenIndex = tokenize.TokenIndex;
const TokenizedBuffer = tokenize.TokenizedBuffer;
pub const IR = @import("parse/IR.zig");
const NodeList = IR.NodeList;
const Diagnostic = IR.Diagnostic;
const Parser = @import("parse/Parser.zig");
const exitOnOom = @import("../collections/utils.zig").exitOnOom;

/// Parses a single Roc file.  The returned AST should be deallocated by calling deinit
/// after its data is used to create the next IR, or at the end of any test.
pub fn parse(env: *base.ModuleEnv, allocator: std.mem.Allocator, source: []const u8) IR {
    var messages: [128]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var tokenizer = tokenize.Tokenizer.init(env, source, msg_slice, allocator);
    tokenizer.tokenize();
    const result = tokenizer.finish_and_deinit();

    if (result.messages.len > 0) {
        tokenizeReport(allocator, source, result.messages);
    }

    var parser = Parser.init(allocator, result.tokens);
    defer parser.deinit();

    parser.parseFile();

    const errors = parser.diagnostics.toOwnedSlice() catch exitOnOom();

    return .{
        .source = source,
        .tokens = result.tokens,
        .store = parser.store,
        .errors = errors,
    };
}

fn lineNum(newlines: std.ArrayList(usize), pos: u32) u32 {
    const pos_usize = @as(usize, @intCast(pos));
    var lineno: u32 = 0;
    while (lineno < newlines.items.len) {
        if (newlines.items[lineno + 1] > pos_usize) {
            return lineno;
        }
        lineno += 1;
    }
    return lineno;
}

fn tokenizeReport(allocator: std.mem.Allocator, source: []const u8, msgs: []const tokenize.Diagnostic) void {
    std.debug.print("Found the {d} following issues while parsing:\n", .{msgs.len});
    var newlines = std.ArrayList(usize).init(allocator);
    defer newlines.deinit();
    newlines.append(0) catch exitOnOom();
    var pos: usize = 0;
    for (source) |c| {
        if (c == '\n') {
            newlines.append(pos) catch exitOnOom();
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

                const src = source[newlines.items[start_line_num]..newlines.items[end_line_num + 1]];
                var spaces = std.ArrayList(u8).init(allocator);
                defer spaces.deinit();
                for (0..start_col) |_| {
                    spaces.append(' ') catch exitOnOom();
                }

                std.debug.print(
                    "({d}:{d}-{d}:{d}) Expected the correct closing brace here:\n{s}\n{s}^\n",
                    .{ start_line_num, start_col, end_line_num, end_col, src, spaces.toOwnedSlice() catch exitOnOom() },
                );
            },
            else => {
                std.debug.print("MSG: {any}", .{message});
            },
        }
    }
}

// TODO move this somewhere better, for now it's here to keep it simple.
fn testSExprHelper(source: []const u8, expected: []const u8) !void {
    var env = base.ModuleEnv.init(testing.allocator);

    // parse our source
    var parse_ast = parse(&env, testing.allocator, source);
    defer parse_ast.deinit();

    // shouldn't be required in future
    parse_ast.store.emptyScratch();

    // buffer to write our SExpr to
    var buf = std.ArrayList(u8).init(testing.allocator);
    defer buf.deinit();

    // convert the AST to our SExpr
    try parse_ast.toSExprStr(testing.allocator, &env, buf.writer().any());
    defer parse_ast.deinit();

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
        \\(file (header 'foo' 'bar') (decl (ident 'foo') (body (expr '"hey"'))) (decl (ident 'bar') (body (expr '"yo"'))))
    ;

    try testSExprHelper(source, expected);
}
