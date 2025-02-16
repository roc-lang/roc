const std = @import("std");
const IR = @import("check/parse/IR.zig");
const Node = IR.Node;
const tokenizer = @import("check/parse/tokenize.zig");
const TokenizedBuffer = tokenizer.TokenizedBuffer;
const TokenIdx = tokenizer.Token.Idx;
const exitOnOom = @import("./collections/utils.zig").exitOnOom;

const NodeStore = IR.NodeStore;
const FileIdx = NodeStore.FileIdx;
const ExprIdx = NodeStore.ExprIdx;
const PatternIdx = NodeStore.PatternIdx;
const BodyIdx = NodeStore.BodyIdx;
const HeaderIdx = NodeStore.HeaderIdx;
const StatementIdx = NodeStore.StatementIdx;

ast: IR,
gpa: std.mem.Allocator,
buffer: std.ArrayList(u8),
curr_indent: u32,

const Formatter = @This();

/// Creates a new Formatter for the given parse IR.
pub fn init(ast: IR, gpa: std.mem.Allocator) Formatter {
    return .{
        .ast = ast,
        .gpa = gpa,
        .buffer = std.ArrayList(u8).init(gpa),
        .curr_indent = 0,
    };
}

/// Deinits all data owned by the formatter object.
pub fn deinit(fmt: *Formatter) void {
    fmt.buffer.deinit();
}

/// Sets the Formatter up fresh to work on a new parse IR.
pub fn resetWith(fmt: *Formatter, ast: IR) void {
    fmt.curr_indent = 0;
    fmt.buffer.shrinkRetainingCapacity(0);
    fmt.ast = ast;
}

/// Emits a string containing the well-formed source of a Roc parse IR (AST).
/// The resulting string is owned by the caller.
pub fn formatFile(fmt: *Formatter) []const u8 {
    fmt.ast.store.emptyScratch();
    const file = fmt.ast.store.getFile(FileIdx{ .id = 0 });
    fmt.formatHeader(file.header);
    for (file.statements) |s| {
        fmt.ensureNewline();
        fmt.newline();
        fmt.formatStatement(s);
    }
    return fmt.buffer.toOwnedSlice() catch exitOnOom();
}

fn formatStatement(fmt: *Formatter, si: StatementIdx) void {
    const statement = fmt.ast.store.getStatement(si);
    switch (statement) {
        .decl => |d| {
            fmt.formatPattern(d.pattern);
            fmt.buffer.appendSlice(" = ") catch exitOnOom();
            fmt.formatBody(d.body);
        },
        .expr => |e| {
            fmt.formatExpr(e.expr);
        },
        .import => |i| {
            fmt.buffer.appendSlice("import ") catch exitOnOom();
            fmt.formatIdent(i.module_name_tok, i.qualifier_tok);
        },
        else => {
            std.debug.panic("TODO: Handle formatting {s}\n", .{@tagName(statement)});
        },
    }
}

fn formatIdent(fmt: *Formatter, ident: TokenIdx, qualifier: ?TokenIdx) void {
    if (qualifier) |q| {
        fmt.pushTokenText(q);
        fmt.buffer.append('.') catch exitOnOom();
    }
    fmt.pushTokenText(ident);
}

fn formatExpr(fmt: *Formatter, ei: ExprIdx) void {
    const expr = fmt.ast.store.getExpr(ei);
    switch (expr) {
        .apply => |a| {
            fmt.formatExpr(a.@"fn");
            fmt.push('(');
            const args_len = a.args.len;
            var i: usize = 0;
            for (a.args) |arg| {
                fmt.formatExpr(arg);
                i += 1;
                if (i < args_len) {
                    fmt.pushAll(", ");
                }
            }
            fmt.push(')');
        },
        .string => |s| {
            if (s.parts.len == 0) {
                fmt.pushTokenText(s.token);
                return;
            }

            var i: usize = 0;
            while (i < s.parts.len) {
                const e = fmt.ast.store.getExpr(s.parts[i]);
                switch (e) {
                    .string => |str| {
                        fmt.pushTokenText(str.token);
                    },
                    else => {
                        fmt.pushAll("{");
                        fmt.formatExpr(s.parts[i]);
                        fmt.push('}');
                    },
                }
                i += 1;
            }
        },
        .ident => |i| {
            fmt.formatIdent(i.token, i.qualifier);
        },
        .int => |i| {
            fmt.pushTokenText(i.token);
        },
        .list => |l| {
            fmt.push('[');
            var i: usize = 0;
            for (l.items) |item| {
                fmt.formatExpr(item);
                if (i < (l.items.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.push(']');
        },
        .lambda => |l| {
            fmt.push('|');
            var i: usize = 0;
            for (l.args) |arg| {
                fmt.formatPattern(arg);
                if (i < (l.args.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.pushAll("| ");
            fmt.formatBody(l.body);
        },
        .suffix_single_question => |s| {
            fmt.formatExpr(s.expr);
            fmt.push('?');
        },
        .tag => |t| {
            fmt.pushTokenText(t.token);
        },
        .if_then_else => |i| {
            fmt.pushAll("if ");
            fmt.formatExpr(i.condition);
            fmt.push(' ');
            fmt.formatBody(i.then);
            fmt.pushAll(" else ");
            fmt.formatBody(i.@"else");
        },
        .dbg => |d| {
            fmt.pushAll("dbg ");
            fmt.formatExpr(d.expr);
        },
        else => {
            std.debug.panic("TODO: Handle formatting {s}", .{@tagName(expr)});
        },
    }
}

fn formatPattern(fmt: *Formatter, pi: PatternIdx) void {
    const pattern = fmt.ast.store.getPattern(pi);
    switch (pattern) {
        .ident => |i| {
            fmt.formatIdent(i.ident_tok, null);
        },
        .underscore => |_| {
            fmt.push('_');
        },
        else => {
            std.debug.panic("TODO: Handle formatting {s}", .{@tagName(pattern)});
        },
    }
}

fn formatHeader(fmt: *Formatter, hi: HeaderIdx) void {
    const header = fmt.ast.store.getHeader(hi);
    switch (header) {
        .app => |a| {
            fmt.pushAll("app [");
            // Format provides
            var i: usize = 0;
            for (a.provides) |p| {
                fmt.pushTokenText(p);
                i += 1;
                if (i < a.provides.len) {
                    fmt.pushAll(", ");
                }
            }
            fmt.pushAll("] { ");
            fmt.pushTokenText(a.platform_name);
            fmt.pushAll(": platform ");
            fmt.pushTokenText(a.platform);
            fmt.pushAll(" }");
            fmt.newline();
        },
        .module => |m| {
            fmt.pushAll("module [");
            var i: usize = 0;
            for (m.exposes) |p| {
                fmt.pushTokenText(p);
                i += 1;
                if (i < m.exposes.len) {
                    fmt.pushAll(", ");
                }
            }
            fmt.push(']');
            fmt.newline();
        },
        else => {
            std.debug.panic("TODO: Handle formatting {s}", .{@tagName(header)});
        },
    }
}

fn formatBody(fmt: *Formatter, bi: BodyIdx) void {
    const body = fmt.ast.store.getBody(bi);
    if (body.whitespace != null and body.statements.len > 0) {
        fmt.curr_indent += 1;
        fmt.buffer.append('{') catch exitOnOom();
        for (body.statements) |s| {
            fmt.ensureNewline();
            fmt.pushIndent();
            fmt.formatStatement(s);
        }
        fmt.ensureNewline();
        fmt.curr_indent -= 1;
        fmt.pushIndent();
        fmt.buffer.append('}') catch exitOnOom();
    } else if (body.statements.len == 1) {
        fmt.formatStatement(body.statements[0]);
    } else {
        fmt.pushAll("{}");
    }
}

fn ensureNewline(fmt: *Formatter) void {
    const last = fmt.buffer.getLastOrNull() orelse ' ';
    if (fmt.buffer.items.len == 0 or last == '\n') {
        return;
    }
    fmt.newline();
}

fn newline(fmt: *Formatter) void {
    fmt.buffer.append('\n') catch exitOnOom();
}

fn isRegionMultiline(fmt: *Formatter, region: IR.Region) bool {
    for (fmt.ast.tokens.tokens.items[region.start..region.end]) |t| {
        switch (t.tag) {
            .Newline => {
                return true;
            },
            else => {},
        }
    }
    return false;
}

const indent = "    ";

fn push(fmt: *Formatter, c: u8) void {
    fmt.buffer.append(c) catch exitOnOom();
}

fn pushAll(fmt: *Formatter, str: []const u8) void {
    fmt.buffer.appendSlice(str) catch exitOnOom();
}

fn pushIndent(fmt: *Formatter) void {
    if (fmt.curr_indent == 0) {
        return;
    }
    for (0..fmt.curr_indent) |_| {
        fmt.buffer.appendSlice(indent) catch exitOnOom();
    }
}

fn pushTokenText(fmt: *Formatter, ti: TokenIdx) void {
    const t = fmt.ast.tokens.tokens.get(ti);
    var start = t.offset;
    switch (t.tag) {
        .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent => {
            start += 1;
        },
        else => {},
    }
    const text = fmt.ast.source[start..(t.offset + t.length)];
    fmt.buffer.appendSlice(text) catch exitOnOom();
}

fn moduleFmtsSame(source: []const u8) !void {
    const parse = @import("check/parse.zig").parse;
    var test_ast = parse(std.testing.allocator, source);
    defer test_ast.deinit();
    defer std.testing.allocator.free(test_ast.errors);
    try std.testing.expectEqualSlices(IR.Diagnostic, test_ast.errors, &[_]IR.Diagnostic{});
    var formatter = Formatter.init(test_ast, std.testing.allocator);
    defer formatter.deinit();
    const result = formatter.formatFile();
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualSlices(u8, source, result);
}

fn moduleFmtsTo(source: []const u8, to: []const u8) !void {
    const parse = @import("check/parse.zig").parse;
    var test_ast = parse(std.testing.allocator, source);
    defer test_ast.deinit();
    defer std.testing.allocator.free(test_ast.errors);
    try std.testing.expectEqualSlices(IR.Diagnostic, test_ast.errors, &[_]IR.Diagnostic{});
    var formatter = Formatter.init(test_ast, std.testing.allocator);
    defer formatter.deinit();
    const result = formatter.formatFile();
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualSlices(u8, to, result);
}

test "Hello world" {
    try moduleFmtsSame(
        \\app [main!] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\import pf.Stdout
        \\
        \\main! = |_| Stdout.line!("Hello, world!")
    );
}

test "Hello world with block" {
    try moduleFmtsSame(
        \\app [main!] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\import pf.Stdout
        \\
        \\main! = |_| {
        \\    world = "World"
        \\    Stdout.line!("Hello, world!")
        \\}
    );
}

test "Hello world no newlines block" {
    try moduleFmtsTo(
        \\app [main!] { pf: platform "../basic-cli/platform.roc" }
        \\import pf.Stdout
        \\main! = |_| {world = "World" Stdout.line!("Hello, world!")}
    ,
        \\app [main!] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\import pf.Stdout
        \\
        \\main! = |_| {
        \\    world = "World"
        \\    Stdout.line!("Hello, world!")
        \\}
    );
}

test "Plain module" {
    try moduleFmtsSame(
        \\module [hello!, world]
        \\
        \\import pf.Stdout
        \\
        \\hello! = Stdout.line!("Hello")
        \\
        \\world = "World"
    );
    try moduleFmtsTo(
        \\module [hello!, world]
        \\import pf.Stdout
        \\hello! = Stdout.line!("Hello")
        \\world = "World"
    ,
        \\module [hello!, world]
        \\
        \\import pf.Stdout
        \\
        \\hello! = Stdout.line!("Hello")
        \\
        \\world = "World"
    );
}

test "Syntax grab bag" {
    try moduleFmtsSame(
        \\app [main!] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\import pf.Stdout
        \\
        \\add_one_oneline = |num| if num 2 else 5
        \\
        \\add_one = |num| {
        \\    other = 1
        \\    if num {
        \\        dbg some_func()
        \\        0
        \\    } else {
        \\        other
        \\    }
        \\}
        \\
        \\main! = |_| {
        \\    world = "World"
        \\    number = 123
        \\    tag = Blue
        \\    tag_with_payload = Ok(number)
        \\    interpolated = "Hello, ${world}"
        \\    list = [add_one(number), 456, 789]
        \\    Stdout.line!(interpolated)?
        \\    Stdout.line!("How about ${Num.toStr(number)} as a string?")
        \\}
    );
}
