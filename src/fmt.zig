const std = @import("std");
const IR = @import("check/parse/IR.zig");
const Node = IR.Node;
const tokenizer = @import("check/parse/tokenize.zig");
const TokenizedBuffer = tokenizer.TokenizedBuffer;
const TokenIdx = tokenizer.Token.Idx;
const exitOnOom = @import("./collections/utils.zig").exitOnOom;
const base = @import("base.zig");

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
    var newline_behavior: NewlineBehavior = .extra_newline_needed;
    for (file.statements) |s| {
        fmt.ensureNewline();
        if (newline_behavior == .extra_newline_needed) {
            fmt.newline();
        }
        newline_behavior = fmt.formatStatement(s);
    }
    return fmt.buffer.toOwnedSlice() catch exitOnOom();
}

const NewlineBehavior = enum { no_extra_newline, extra_newline_needed };

fn formatStatement(fmt: *Formatter, si: StatementIdx) NewlineBehavior {
    const statement = fmt.ast.store.getStatement(si);
    switch (statement) {
        .decl => |d| {
            fmt.formatPattern(d.pattern);
            fmt.buffer.appendSlice(" = ") catch exitOnOom();
            fmt.formatBody(d.body);
            return .extra_newline_needed;
        },
        .expr => |e| {
            fmt.formatExpr(e.expr);
            return .extra_newline_needed;
        },
        .import => |i| {
            fmt.buffer.appendSlice("import ") catch exitOnOom();
            fmt.formatIdent(i.module_name_tok, i.qualifier_tok);
            return .extra_newline_needed;
        },
        .type_decl => |d| {
            fmt.formatTypeHeader(d.header);
            fmt.pushAll(" : ");
            fmt.formatTypeAnno(d.anno);
            return .extra_newline_needed;
        },
        .type_anno => |t| {
            fmt.pushTokenText(t.name);
            fmt.pushAll(" : ");
            fmt.formatTypeAnno(t.anno);
            return .no_extra_newline;
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
        .string_part => |s| {
            fmt.pushTokenText(s.token);
        },
        .string => |s| {
            fmt.push('"');
            var i: usize = 0;
            while (i < s.parts.len) {
                const e = fmt.ast.store.getExpr(s.parts[i]);
                switch (e) {
                    .string_part => |str| {
                        fmt.pushTokenText(str.token);
                    },
                    else => {
                        fmt.pushAll("${");
                        fmt.formatExpr(s.parts[i]);
                        fmt.push('}');
                    },
                }
                i += 1;
            }
            fmt.push('"');
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
        .tuple => |t| {
            fmt.push('(');
            var i: usize = 0;
            for (t.items) |item| {
                fmt.formatExpr(item);
                if (i < (t.items.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.push(')');
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
        .match => |m| {
            fmt.pushAll("match ");
            fmt.formatExpr(m.expr);
            fmt.pushAll(" {");
            fmt.curr_indent += 1;
            for (m.branches) |b| {
                const branch = fmt.ast.store.getBranch(b);
                fmt.newline();
                fmt.pushIndent();
                fmt.formatPattern(branch.pattern);
                fmt.pushAll(" -> ");
                fmt.formatBody(branch.body);
            }
            fmt.curr_indent -= 1;
            fmt.newline();
            fmt.pushIndent();
            fmt.push('}');
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
        .tag => |t| {
            fmt.formatIdent(t.tag_tok, null);
            if (t.args.len > 0) {
                fmt.push('(');
                var i: usize = 0;
                for (t.args) |arg| {
                    fmt.formatPattern(arg);
                    if (i < (t.args.len - 1)) {
                        fmt.pushAll(", ");
                    }
                    i += 1;
                }
                fmt.push(')');
            }
        },
        .string => |s| {
            fmt.formatExpr(s.expr);
        },
        .number => |n| {
            fmt.formatIdent(n.number_tok, null);
        },
        .record => |r| {
            fmt.pushAll("{ ");
            var i: usize = 0;
            for (r.fields) |field_idx| {
                const field = fmt.ast.store.getPatternRecordField(field_idx);
                if (field.rest) {
                    fmt.pushAll("..");
                    if (field.name != 0) {
                        fmt.pushTokenText(field.name);
                    }
                    continue;
                }
                fmt.pushTokenText(field.name);
                if (field.value) |v| {
                    fmt.pushAll(": ");
                    fmt.formatPattern(v);
                }
                if (i < (r.fields.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.pushAll(" }");
        },
        .list => |l| {
            fmt.push('[');
            var i: usize = 0;
            for (l.patterns) |p| {
                fmt.formatPattern(p);
                if (i < (l.patterns.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.push(']');
        },
        .tuple => |t| {
            fmt.push('(');
            var i: usize = 0;
            for (t.patterns) |p| {
                fmt.formatPattern(p);
                if (i < (t.patterns.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.push(')');
        },
        .list_rest => |r| {
            fmt.pushAll("..");
            if (r.name) |n| {
                fmt.pushAll(" as ");
                fmt.pushTokenText(n);
            }
        },
        .underscore => |_| {
            fmt.push('_');
        },
        .alternatives => |a| {
            var i: usize = 0;
            for (a.patterns) |p| {
                fmt.formatPattern(p);
                if (i < (a.patterns.len - 1)) {
                    fmt.pushAll(" | ");
                }
                i += 1;
            }
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
            fmt.formatExpr(a.platform);
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
    if (body.whitespace != null and body.statements.len > 1) {
        fmt.curr_indent += 1;
        fmt.buffer.append('{') catch exitOnOom();
        for (body.statements) |s| {
            fmt.ensureNewline();
            fmt.pushIndent();
            _ = fmt.formatStatement(s);
        }
        fmt.ensureNewline();
        fmt.curr_indent -= 1;
        fmt.pushIndent();
        fmt.buffer.append('}') catch exitOnOom();
    } else if (body.statements.len == 1) {
        _ = fmt.formatStatement(body.statements[0]);
    } else {
        fmt.pushAll("{}");
    }
}

fn formatTypeHeader(fmt: *Formatter, header: IR.NodeStore.TypeHeaderIdx) void {
    const h = fmt.ast.store.getTypeHeader(header);
    fmt.pushTokenText(h.name);
    if (h.args.len > 0) {
        fmt.push(' ');
        var i: usize = 0;
        for (h.args) |arg| {
            fmt.pushTokenText(arg);
            if (i < (h.args.len - 1)) {
                fmt.push(' ');
            }
            i += 1;
        }
    }
}

fn formatTypeAnno(fmt: *Formatter, anno: IR.NodeStore.TypeAnnoIdx) void {
    const a = fmt.ast.store.getTypeAnno(anno);
    switch (a) {
        .ty_var => |v| {
            fmt.pushTokenText(v.tok);
        },
        .tag => |t| {
            fmt.pushTokenText(t.tok);
            if (t.args.len > 0) {
                fmt.push('(');
                var i: usize = 0;
                for (t.args) |arg| {
                    fmt.formatTypeAnno(arg);
                    if (i < (t.args.len - 1)) {
                        fmt.pushAll(", ");
                    }
                    i += 1;
                }
                fmt.push(')');
            }
        },
        .tuple => |t| {
            fmt.push('(');
            var i: usize = 0;
            for (t.annos) |an| {
                fmt.formatTypeAnno(an);
                if (i < (t.annos.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.push(')');
        },
        .record => |r| {
            if (r.fields.len == 0) {
                fmt.pushAll("{}");
                return;
            }
            fmt.pushAll("{ ");
            var i: usize = 0;
            for (r.fields) |idx| {
                const field = fmt.ast.store.getAnnoRecordField(idx);
                fmt.pushTokenText(field.name);
                fmt.pushAll(" : ");
                fmt.formatTypeAnno(field.ty);
                if (i < (r.fields.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.pushAll(" }");
        },
        .tag_union => |t| {
            fmt.push('[');
            var i: usize = 0;
            for (t.tags) |tag| {
                fmt.formatTypeAnno(tag);
                if (i < (t.tags.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.push(']');
        },
        .@"fn" => |f| {
            var i: usize = 0;
            for (f.args) |idx| {
                fmt.formatTypeAnno(idx);
                if (i < (f.args.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.pushAll(" -> ");
            fmt.formatTypeAnno(f.ret);
        },
        .parens => |p| {
            fmt.push('(');
            fmt.formatTypeAnno(p.anno);
            fmt.push(')');
        },
        .star => |_| {
            fmt.push('*');
        },
        .underscore => |_| {
            fmt.push('_');
        },
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
    const tag = fmt.ast.tokens.tokens.items(.tag)[ti];
    const region = fmt.ast.tokens.resolve(ti);
    var start = region.start.offset;
    switch (tag) {
        .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent => {
            start += 1;
        },
        else => {},
    }

    const text = fmt.ast.source[start..region.end.offset];
    fmt.buffer.appendSlice(text) catch exitOnOom();
}

fn moduleFmtsSame(source: []const u8) !void {
    const parse = @import("check/parse.zig").parse;

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    var env = base.ModuleEnv.init(&arena);

    var parse_ast = parse(&env, std.testing.allocator, source);
    defer parse_ast.deinit();

    // @Anthony / @Josh shouldn't these be added to the ModuleEnv (env) so they are in the arena
    // and then they are cleaned up when the arena is deinitialized at the end of program compilation
    // or included in the cached build
    defer std.testing.allocator.free(parse_ast.errors);

    try std.testing.expectEqualSlices(IR.Diagnostic, &[_]IR.Diagnostic{}, parse_ast.errors);
    var formatter = Formatter.init(parse_ast, std.testing.allocator);
    defer formatter.deinit();
    const result = formatter.formatFile();
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualSlices(u8, source, result);
}

fn moduleFmtsTo(source: []const u8, to: []const u8) !void {
    const parse = @import("check/parse.zig").parse;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    var env = base.ModuleEnv.init(&arena);
    var parse_ast = parse(&env, std.testing.allocator, source);
    defer parse_ast.deinit();
    defer std.testing.allocator.free(parse_ast.errors);
    try std.testing.expectEqualSlices(IR.Diagnostic, parse_ast.errors, &[_]IR.Diagnostic{});
    var formatter = Formatter.init(parse_ast, std.testing.allocator);
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
        \\Map a b : List(a), (a -> b) -> List(b)
        \\
        \\Foo : (Bar, Baz)
        \\
        \\Some a : { foo : Ok(a), bar : Something }
        \\
        \\Maybe a : [Some(a), None]
        \\
        \\SomeFunc a : Maybe(a), a -> Maybe(a)
        \\
        \\add_one_oneline = |num| if num 2 else 5
        \\
        \\add_one : (U64 -> U64)
        \\add_one = |num| {
        \\    other = 1
        \\    if num {
        \\        dbg some_func()
        \\        0
        \\    } else {
        \\        dbg 123
        \\        other
        \\    }
        \\}
        \\
        \\match_time = |a| match a {
        \\    Blue | Green | Red -> {
        \\        x = 12
        \\        x
        \\    }
        \\    lower -> 1
        \\    "foo" -> 100
        \\    "foo" | "bar" -> 200
        \\    [1, 2, 3, .. as rest] -> 123
        \\    [1, 2 | 5, 3, .. as rest] -> 123
        \\    3.14 -> 314
        \\    3.14 | 6.28 -> 314
        \\    (1, 2, 3) -> 123
        \\    (1, 2 | 5, 3) -> 123
        \\    { foo: 1, bar: 2, ..rest } -> 12
        \\    { foo: 1, bar: 2 | 7 } -> 12
        \\    Ok(123) -> 123
        \\    Ok(Some(dude)) -> dude
        \\    TwoArgs("hello", Some("world")) -> 1000
        \\}
        \\
        \\main! : List(String) -> Result({}, _)
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
