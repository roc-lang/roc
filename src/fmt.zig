const std = @import("std");
const IR = @import("check/parse/IR.zig");
const Node = IR.Node;
const tokenizer = @import("check/parse/tokenize.zig");
const TokenizedBuffer = tokenizer.TokenizedBuffer;
const TokenIndex = tokenizer.Token.List.Idx;

const NodeStore = IR.NodeStore;
const ExprIndex = NodeStore.ExprIndex;
const PatternIndex = NodeStore.PatternIndex;
const BodyIndex = NodeStore.BodyIndex;
const HeaderIndex = NodeStore.HeaderIndex;
const StatementIndex = NodeStore.StatementIndex;

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
pub fn formatFile(fmt: *Formatter) ![]const u8 {
    fmt.ast.store.emptyScratch();
    const file = try fmt.ast.store.getFile(.{ .file = 0 });
    try fmt.formatHeader(file.header);
    for (file.statements) |s| {
        try fmt.ensureNewline();
        try fmt.newline();
        try fmt.formatStatement(s);
    }
    return fmt.buffer.toOwnedSlice();
}

fn formatStatement(fmt: *Formatter, si: StatementIndex) std.mem.Allocator.Error!void {
    const statement = try fmt.ast.store.getStatement(si);
    switch (statement) {
        .decl => |d| {
            try fmt.formatPattern(d.pattern);
            const body = try fmt.ast.store.getBody(d.body);
            if (body.whitespace != null) {
                try fmt.buffer.appendSlice(" =");
            } else {
                try fmt.buffer.appendSlice(" = ");
            }
            try fmt.formatBody(d.body);
        },
        .expr => |e| {
            try fmt.formatExpr(e.expr);
        },
        .import => |i| {
            try fmt.buffer.appendSlice("import ");
            try fmt.formatIdent(i.module_name_tok, i.qualifier_tok);
        },
        else => {
            std.debug.panic("TODO: Handle formatting {s}", .{@tagName(statement)});
        },
    }
}

fn formatIdent(fmt: *Formatter, ident: TokenIndex, qualifier: ?TokenIndex) !void {
    if (qualifier) |q| {
        try fmt.pushTokenText(q);
        try fmt.buffer.append('.');
    }
    try fmt.pushTokenText(ident);
}

fn formatExpr(fmt: *Formatter, ei: ExprIndex) !void {
    const expr = try fmt.ast.store.getExpr(ei);
    switch (expr) {
        .apply => |a| {
            try fmt.formatExpr(a.@"fn");
            try fmt.buffer.append('(');
            const args_len = a.args.len;
            var i: usize = 0;
            for (a.args) |arg| {
                try fmt.formatExpr(arg);
                i += 1;
                if (i < args_len) {
                    try fmt.buffer.append(',');
                }
            }
            try fmt.buffer.append(')');
        },
        .string => |s| {
            try fmt.pushTokenText(s.token);
        },
        .ident => |i| {
            try fmt.formatIdent(i.token, i.qualifier);
        },
        else => {
            std.debug.panic("TODO: Handle formatting {s}", .{@tagName(expr)});
        },
    }
}

fn formatPattern(fmt: *Formatter, pi: PatternIndex) !void {
    const pattern = try fmt.ast.store.getPattern(pi);
    switch (pattern) {
        .ident => |i| {
            try fmt.formatIdent(i.ident_tok, null);
        },
        else => {
            std.debug.panic("TODO: Handle formatting {s}", .{@tagName(pattern)});
        },
    }
}

fn formatHeader(fmt: *Formatter, hi: HeaderIndex) !void {
    const header = try fmt.ast.store.getHeader(hi);
    switch (header) {
        .app => |a| {
            try fmt.buffer.appendSlice("app [");
            // Format provides
            var i: usize = 0;
            for (a.provides) |p| {
                try fmt.pushTokenText(p);
                i += 1;
                if (i < a.provides.len) {
                    try fmt.buffer.append(',');
                }
            }
            try fmt.buffer.appendSlice("] { ");
            try fmt.pushTokenText(a.platform_name);
            try fmt.buffer.appendSlice(": platform ");
            try fmt.pushTokenText(a.platform);
            try fmt.buffer.appendSlice(" }");
            try fmt.newline();
        },
        else => {
            std.debug.panic("TODO: Handle formatting {s}", .{@tagName(header)});
        },
    }
}

fn formatBody(fmt: *Formatter, bi: BodyIndex) !void {
    const body = try fmt.ast.store.getBody(bi);
    if (body.whitespace != null) {
        fmt.curr_indent += 1;
        for (body.statements) |s| {
            try fmt.ensureNewline();
            try fmt.pushIndent();
            try fmt.formatStatement(s);
        }
    } else if (body.statements.len == 1) {
        try fmt.formatStatement(body.statements[0]);
    } else {
        std.debug.panic("TODO: Malformed body format", .{});
    }
}

fn ensureNewline(fmt: *Formatter) !void {
    const last = fmt.buffer.getLastOrNull() orelse ' ';
    if (fmt.buffer.items.len == 0 or last == '\n') {
        return;
    }
    return fmt.newline();
}

fn newline(fmt: *Formatter) !void {
    // std.debug.print("newline\n", .{});
    try fmt.buffer.append('\n');
}

const indent = "    ";

fn pushIndent(fmt: *Formatter) !void {
    if (fmt.curr_indent == 0) {
        return;
    }
    for (0..fmt.curr_indent) |_| {
        try fmt.buffer.appendSlice(indent);
    }
}

fn pushTokenText(fmt: *Formatter, ti: TokenIndex) !void {
    const t = fmt.ast.tokens.tokens.get(ti);
    var start = t.offset;
    switch (t.tag) {
        .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent => {
            start += 1;
        },
        else => {},
    }
    const text = fmt.ast.source[start..(t.offset + t.length)];
    try fmt.buffer.appendSlice(text);
}

test {
    const parse = @import("check/parse.zig").parse;
    const source =
        \\app [main!] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\import pf.Stdout
        \\
        \\main! =
        \\    Stdout.line!("Hello, world!")
    ;
    var test_ast = try parse(std.testing.allocator, source);
    defer test_ast.deinit();
    var formatter = Formatter.init(test_ast, std.testing.allocator);
    defer formatter.deinit();
    const result = try formatter.formatFile();
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualSlices(u8, source, result);
}
