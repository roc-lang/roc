//! Formatting logic for Roc modules.

const std = @import("std");
const IR = @import("check/parse/IR.zig");
const Node = IR.Node;
const tokenizer = @import("check/parse/tokenize.zig");
const TokenizedBuffer = tokenizer.TokenizedBuffer;
const TokenIdx = tokenizer.Token.Idx;
const exitOnOom = @import("./collections/utils.zig").exitOnOom;
const base = @import("base.zig");

const NodeStore = IR.NodeStore;
const ExprIdx = NodeStore.ExprIdx;
const PatternIdx = NodeStore.PatternIdx;
const HeaderIdx = NodeStore.HeaderIdx;
const StatementIdx = NodeStore.StatementIdx;

const FormatFlags = enum { debug_binop, no_debug };

ast: IR,
gpa: std.mem.Allocator,
buffer: std.ArrayListUnmanaged(u8) = .{},
curr_indent: u32 = 0,
flags: FormatFlags = .no_debug,

const Formatter = @This();

/// Creates a new Formatter for the given parse IR.
pub fn init(ast: IR) Formatter {
    return .{ .ast = ast, .gpa = ast.store.gpa };
}

/// Deinits all data owned by the formatter object.
pub fn deinit(fmt: *Formatter) void {
    fmt.buffer.deinit(fmt.gpa);
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
    const file = fmt.ast.store.getFile();
    fmt.formatHeader(file.header);
    var newline_behavior: NewlineBehavior = .extra_newline_needed;
    for (fmt.ast.store.statementSlice(file.statements)) |s| {
        fmt.ensureNewline();
        if (newline_behavior == .extra_newline_needed) {
            fmt.newline();
        }
        newline_behavior = fmt.formatStatement(s);
    }
    return fmt.buffer.toOwnedSlice(fmt.gpa) catch |err| exitOnOom(err);
}

const NewlineBehavior = enum { no_extra_newline, extra_newline_needed };

fn formatStatement(fmt: *Formatter, si: StatementIdx) NewlineBehavior {
    const statement = fmt.ast.store.getStatement(si);
    switch (statement) {
        .decl => |d| {
            fmt.formatPattern(d.pattern);
            fmt.buffer.appendSlice(fmt.gpa, " = ") catch |err| exitOnOom(err);
            fmt.formatExpr(d.body);
            return .extra_newline_needed;
        },
        .expr => |e| {
            fmt.formatExpr(e.expr);
            return .extra_newline_needed;
        },
        .import => |i| {
            fmt.buffer.appendSlice(fmt.gpa, "import ") catch |err| exitOnOom(err);
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
        .expect => |e| {
            fmt.pushAll("expect ");
            fmt.formatExpr(e.body);
            return .extra_newline_needed;
        },
        .crash => |c| {
            fmt.pushAll("crash ");
            fmt.formatExpr(c.expr);
            return .extra_newline_needed;
        },
        .@"return" => |r| {
            fmt.pushAll("return ");
            fmt.formatExpr(r.expr);
            return .extra_newline_needed;
        },
    }
}

fn formatIdent(fmt: *Formatter, ident: TokenIdx, qualifier: ?TokenIdx) void {
    if (qualifier) |q| {
        fmt.pushTokenText(q);
        fmt.buffer.append(fmt.gpa, '.') catch |err| exitOnOom(err);
    }
    fmt.pushTokenText(ident);
}

fn formatExpr(fmt: *Formatter, ei: ExprIdx) void {
    const expr = fmt.ast.store.getExpr(ei);
    switch (expr) {
        .apply => |a| {
            fmt.formatExpr(a.@"fn");
            fmt.push('(');
            const args_len = a.args.span.len;
            var i: usize = 0;
            for (fmt.ast.store.exprSlice(a.args)) |arg| {
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
            for (fmt.ast.store.exprSlice(s.parts)) |idx| {
                const e = fmt.ast.store.getExpr(idx);
                switch (e) {
                    .string_part => |str| {
                        fmt.pushTokenText(str.token);
                    },
                    else => {
                        fmt.pushAll("${");
                        fmt.formatExpr(idx);
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
        .field_access => |fa| {
            fmt.formatExpr(fa.left);
            fmt.push('.');
            fmt.formatExpr(fa.right);
        },
        .int => |i| {
            fmt.pushTokenText(i.token);
        },
        .list => |l| {
            fmt.push('[');
            var i: usize = 0;
            for (fmt.ast.store.exprSlice(l.items)) |item| {
                fmt.formatExpr(item);
                if (i < (l.items.span.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.push(']');
        },
        .tuple => |t| {
            fmt.push('(');
            var i: usize = 0;
            for (fmt.ast.store.exprSlice(t.items)) |item| {
                fmt.formatExpr(item);
                if (i < (t.items.span.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.push(')');
        },
        .record => |r| {
            fmt.pushAll("{ ");
            var i: usize = 0;
            for (fmt.ast.store.recordFieldSlice(r.fields)) |fieldIdx| {
                const field = fmt.ast.store.getRecordField(fieldIdx);
                fmt.pushTokenText(field.name);
                if (field.value) |v| {
                    fmt.pushAll(if (field.optional) "? " else ": ");
                    fmt.formatExpr(v);
                }
                if (i < (r.fields.span.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.pushAll(" }");
        },
        .lambda => |l| {
            fmt.push('|');
            var i: usize = 0;
            for (fmt.ast.store.patternSlice(l.args)) |arg| {
                fmt.formatPattern(arg);
                if (i < (l.args.span.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.pushAll("| ");
            fmt.formatExpr(l.body);
        },
        .unary_op => |op| {
            fmt.pushTokenText(op.operator);
            fmt.formatExpr(op.expr);
        },
        .bin_op => |op| {
            if (fmt.flags == .debug_binop) {
                fmt.push('(');
            }
            fmt.formatExpr(op.left);
            fmt.push(' ');
            fmt.pushTokenText(op.operator);
            fmt.push(' ');
            fmt.formatExpr(op.right);
            if (fmt.flags == .debug_binop) {
                fmt.push(')');
            }
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
            fmt.formatExpr(i.then);
            fmt.pushAll(" else ");
            fmt.formatExpr(i.@"else");
        },
        .match => |m| {
            fmt.pushAll("match ");
            fmt.formatExpr(m.expr);
            fmt.pushAll(" {");
            fmt.curr_indent += 1;
            for (fmt.ast.store.whenBranchSlice(m.branches)) |b| {
                const branch = fmt.ast.store.getBranch(b);
                fmt.newline();
                fmt.pushIndent();
                fmt.formatPattern(branch.pattern);
                fmt.pushAll(" -> ");
                fmt.formatExpr(branch.body);
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
        .block => |b| {
            fmt.formatBody(b);
        },
        .ellipsis => |_| {
            fmt.pushAll("...");
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
            if (t.args.span.len > 0) {
                fmt.push('(');
                var i: usize = 0;
                for (fmt.ast.store.patternSlice(t.args)) |arg| {
                    fmt.formatPattern(arg);
                    if (i < (t.args.span.len - 1)) {
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
            for (fmt.ast.store.patternRecordFieldSlice(r.fields)) |field_idx| {
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
                if (i < (r.fields.span.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.pushAll(" }");
        },
        .list => |l| {
            fmt.push('[');
            var i: usize = 0;
            for (fmt.ast.store.patternSlice(l.patterns)) |p| {
                fmt.formatPattern(p);
                if (i < (l.patterns.span.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.push(']');
        },
        .tuple => |t| {
            fmt.push('(');
            var i: usize = 0;
            for (fmt.ast.store.patternSlice(t.patterns)) |p| {
                fmt.formatPattern(p);
                if (i < (t.patterns.span.len - 1)) {
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
            for (fmt.ast.store.patternSlice(a.patterns)) |p| {
                fmt.formatPattern(p);
                if (i < (a.patterns.span.len - 1)) {
                    fmt.pushAll(" | ");
                }
                i += 1;
            }
        },
        .malformed => {
            // TODO how should we format a malformed here?
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
            for (fmt.ast.store.tokenSlice(a.provides)) |p| {
                fmt.pushTokenText(p);
                i += 1;
                if (i < a.provides.span.len) {
                    fmt.pushAll(", ");
                }
            }
            fmt.pushAll("] { ");
            fmt.pushTokenText(a.platform_name);
            fmt.pushAll(": platform ");
            fmt.formatExpr(a.platform);
            if (a.packages.span.len > 0) {
                fmt.push(',');
            }
            i = 0;
            for (fmt.ast.store.recordFieldSlice(a.packages)) |package| {
                const field = fmt.ast.store.getRecordField(package);
                fmt.pushTokenText(field.name);
                if (field.value) |v| {
                    fmt.pushAll(": ");
                    fmt.formatExpr(v);
                }
                if (i < a.packages.span.len) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.pushAll(" }");
            fmt.newline();
        },
        .module => |m| {
            fmt.pushAll("module [");
            var i: usize = 0;
            for (fmt.ast.store.tokenSlice(m.exposes)) |p| {
                fmt.pushTokenText(p);
                i += 1;
                if (i < m.exposes.span.len) {
                    fmt.pushAll(", ");
                }
            }
            fmt.push(']');
            fmt.newline();
        },
        .malformed => {
            // TODO how should we format a malformed here?
        },
        else => {
            std.debug.panic("TODO: Handle formatting {s}", .{@tagName(header)});
        },
    }
}

fn formatBody(fmt: *Formatter, body: IR.NodeStore.Body) void {
    if (body.statements.span.len > 1) {
        fmt.curr_indent += 1;
        fmt.buffer.append(fmt.gpa, '{') catch |err| exitOnOom(err);
        for (fmt.ast.store.statementSlice(body.statements)) |s| {
            fmt.ensureNewline();
            fmt.pushIndent();
            _ = fmt.formatStatement(s);
        }
        fmt.ensureNewline();
        fmt.curr_indent -= 1;
        fmt.pushIndent();
        fmt.buffer.append(fmt.gpa, '}') catch |err| exitOnOom(err);
    } else if (body.statements.span.len == 1) {
        for (fmt.ast.store.statementSlice(body.statements)) |s| {
            _ = fmt.formatStatement(s);
        }
    } else {
        fmt.pushAll("{}");
    }
}

fn formatTypeHeader(fmt: *Formatter, header: IR.NodeStore.TypeHeaderIdx) void {
    const h = fmt.ast.store.getTypeHeader(header);
    fmt.pushTokenText(h.name);
    if (h.args.span.len > 0) {
        fmt.push(' ');
        var i: usize = 0;
        for (fmt.ast.store.tokenSlice(h.args)) |arg| {
            fmt.pushTokenText(arg);
            if (i < (h.args.span.len - 1)) {
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
            if (t.args.span.len > 0) {
                fmt.push('(');
                var i: usize = 0;
                for (fmt.ast.store.typeAnnoSlice(t.args)) |arg| {
                    fmt.formatTypeAnno(arg);
                    if (i < (t.args.span.len - 1)) {
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
            for (fmt.ast.store.typeAnnoSlice(t.annos)) |an| {
                fmt.formatTypeAnno(an);
                if (i < (t.annos.span.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.push(')');
        },
        .record => |r| {
            if (r.fields.span.len == 0) {
                fmt.pushAll("{}");
                return;
            }
            fmt.pushAll("{ ");
            var i: usize = 0;
            for (fmt.ast.store.annoRecordFieldSlice(r.fields)) |idx| {
                const field = fmt.ast.store.getAnnoRecordField(idx);
                fmt.pushTokenText(field.name);
                fmt.pushAll(" : ");
                fmt.formatTypeAnno(field.ty);
                if (i < (r.fields.span.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.pushAll(" }");
        },
        .tag_union => |t| {
            fmt.push('[');
            var i: usize = 0;
            for (fmt.ast.store.typeAnnoSlice(t.tags)) |tag| {
                fmt.formatTypeAnno(tag);
                if (i < (t.tags.span.len - 1)) {
                    fmt.pushAll(", ");
                }
                i += 1;
            }
            fmt.push(']');
        },
        .@"fn" => |f| {
            var i: usize = 0;
            for (fmt.ast.store.typeAnnoSlice(f.args)) |idx| {
                fmt.formatTypeAnno(idx);
                if (i < (f.args.span.len - 1)) {
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
        .underscore => |_| {
            fmt.push('_');
        },
        .malformed => {
            // TODO how should we format a malformed here?
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
    fmt.buffer.append(fmt.gpa, '\n') catch |err| exitOnOom(err);
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
    fmt.buffer.append(fmt.gpa, c) catch |err| exitOnOom(err);
}

fn pushAll(fmt: *Formatter, str: []const u8) void {
    fmt.buffer.appendSlice(fmt.gpa, str) catch |err| exitOnOom(err);
}

fn pushIndent(fmt: *Formatter) void {
    if (fmt.curr_indent == 0) {
        return;
    }
    for (0..fmt.curr_indent) |_| {
        fmt.buffer.appendSlice(fmt.gpa, indent) catch |err| exitOnOom(err);
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
    fmt.buffer.appendSlice(fmt.gpa, text) catch |err| exitOnOom(err);
}

fn moduleFmtsSame(source: []const u8) !void {
    const parse = @import("check/parse.zig").parse;

    const gpa = std.testing.allocator;

    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var parse_ast = parse(&env, source);
    defer parse_ast.deinit();

    // @Anthony / @Josh shouldn't these be added to the ModuleEnv (env) so they are in the arena
    // and then they are cleaned up when the arena is deinitialized at the end of program compilation
    // or included in the cached build
    defer gpa.free(parse_ast.errors);

    try std.testing.expectEqualSlices(IR.Diagnostic, &[_]IR.Diagnostic{}, parse_ast.errors);

    var formatter = Formatter.init(parse_ast);
    defer formatter.deinit();

    const result = formatter.formatFile();
    defer gpa.free(result);

    try std.testing.expectEqualStrings(source, result);
}

const tokenize = @import("check/parse/tokenize.zig");

fn exprFmtsSame(source: []const u8, flags: FormatFlags) !void {
    try exprFmtsTo(source, source, flags);
}
fn exprFmtsTo(source: []const u8, expected: []const u8, flags: FormatFlags) !void {
    const Parser = @import("check/parse/Parser.zig").Parser;

    const gpa = std.testing.allocator;

    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var messages: [1]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];

    var tokenizer_ = tokenize.Tokenizer.init(&env, source, msg_slice);
    tokenizer_.tokenize();
    const result = tokenizer_.finishAndDeinit();

    var parser = Parser.init(result.tokens);
    defer parser.deinit();

    const expr = parser.parseExpr();

    const errors = parser.diagnostics.toOwnedSlice(gpa) catch |err| exitOnOom(err);

    var parse_ast = IR{
        .source = source,
        .tokens = result.tokens,
        .store = parser.store,
        .errors = errors,
    };
    defer parse_ast.deinit();
    defer std.testing.allocator.free(parse_ast.errors);

    std.testing.expectEqualSlices(IR.Diagnostic, &[_]IR.Diagnostic{}, parse_ast.errors) catch {
        std.debug.print("Tokens:\n{any}", .{result.tokens.tokens.items(.tag)});
        std.debug.panic("Test failed with parse errors", .{});
    };

    var formatter = Formatter.init(parse_ast);
    formatter.flags = flags;
    defer formatter.deinit();

    formatter.formatExpr(expr);

    const fmt_result = formatter.buffer.toOwnedSlice(gpa) catch |err| exitOnOom(err);
    defer gpa.free(fmt_result);

    try std.testing.expectEqualStrings(expected, fmt_result);
}

fn moduleFmtsTo(source: []const u8, to: []const u8) !void {
    const gpa = std.testing.allocator;
    const result = try moduleFmtsStable(gpa, source, false);
    defer gpa.free(result);
    try std.testing.expectEqualStrings(to, result);
}

/// Asserts a module when formatted twice in a row results in the same final output.
/// Returns that final output.
pub fn moduleFmtsStable(gpa: std.mem.Allocator, input: []const u8, debug: bool) ![]const u8 {
    if (debug) {
        std.debug.print("Original:\n==========\n{s}\n==========\n\n", .{input});
    }

    const formatted = try parseAndFmt(gpa, input, debug);
    defer gpa.free(formatted);

    const formatted_twice = parseAndFmt(gpa, formatted, debug) catch {
        return error.SecondParseFailed;
    };
    errdefer gpa.free(formatted_twice);

    std.testing.expectEqualStrings(formatted, formatted_twice) catch {
        return error.FormattingNotStable;
    };
    return formatted_twice;
}

fn parseAndFmt(gpa: std.mem.Allocator, input: []const u8, debug: bool) ![]const u8 {
    const parse = @import("check/parse.zig").parse;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var parse_ast = parse(&module_env, input);
    defer parse_ast.deinit();

    // Currently disabled cause SExpr are missing a lot of IR coverage resulting in panics.
    if (debug and false) {
        // shouldn't be required in future
        parse_ast.store.emptyScratch();

        std.debug.print("Parsed SExpr:\n==========\n", .{});
        parse_ast.toSExprStr(&module_env, std.io.getStdErr().writer().any()) catch @panic("Failed to print SExpr");
        std.debug.print("\n==========\n\n", .{});
    }

    std.testing.expectEqualSlices(IR.Diagnostic, parse_ast.errors, &[_]IR.Diagnostic{}) catch {
        return error.ParseFailed;
    };

    var formatter = init(parse_ast);
    defer formatter.deinit();

    const formatted = formatter.formatFile();

    if (debug) {
        std.debug.print("Formatted:\n==========\n{s}\n==========\n\n", .{formatted});
    }
    return formatted;
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
        \\expect blah == 1
        \\
        \\main! : List(String) -> Result({}, _)
        \\main! = |_| {
        \\    world = "World"
        \\    number = 123
        \\    expect blah == 1
        \\    tag = Blue
        \\    return tag
        \\    ...
        \\    match_time(...)
        \\    crash "Unreachable!"
        \\    tag_with_payload = Ok(number)
        \\    interpolated = "Hello, ${world}"
        \\    list = [add_one(number), 456, 789]
        \\    record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
        \\    tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
        \\    bin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
        \\    static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
        \\    Stdout.line!(interpolated)?
        \\    Stdout.line!("How about ${Num.toStr(number)} as a string?")
        \\}
        \\
        \\expect {
        \\    foo = 1
        \\    blah = 1
        \\    blah == foo
        \\}
    );
}

test "First BinOp" {
    const expr = "1 + 2";
    try exprFmtsSame(expr, .no_debug);
}

test "BinOp with higher BP right" {
    const expr = "1 + 2 * 3";
    try exprFmtsSame(expr, .no_debug);
    try exprFmtsTo(expr, "(1 + (2 * 3))", .debug_binop);
    // try exprBinOpIs(expr, .OpStar);
}

test "BinOp omnibus" {
    const expr = "Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5";
    const expr_sloppy = "Err(foo)??12>5*5 or 13+2<5 and 10-1>=16 or 12<=3/5";
    const formatted = "((((Err(foo) ?? 12) > (5 * 5)) or (((13 + 2) < 5) and ((10 - 1) >= 16))) or (12 <= (3 / 5)))";
    try exprFmtsSame(expr, .no_debug);
    try exprFmtsTo(expr_sloppy, expr, .no_debug);
    try exprFmtsTo(expr, formatted, .debug_binop);
    try exprFmtsTo(expr_sloppy, formatted, .debug_binop);
}

test "Dot access super test" {
    const expr = "some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?";
    try exprFmtsSame(expr, .no_debug);
}
