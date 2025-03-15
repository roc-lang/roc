//! Formatting logic for Roc modules.

const std = @import("std");
const parse = @import("check/parse.zig").parse;
const IR = @import("check/parse/IR.zig");
const Node = IR.Node;
const Filesystem = @import("coordinate/Filesystem.zig");
const tokenizer = @import("check/parse/tokenize.zig");
const TokenizedBuffer = tokenizer.TokenizedBuffer;
const TokenIdx = tokenizer.Token.Idx;
const exitOnOom = @import("./collections/utils.zig").exitOnOom;
const fatal = @import("./collections/utils.zig").fatal;
const base = @import("base.zig");
const tracy = @import("tracy.zig");

const NodeStore = IR.NodeStore;
const ExprIdx = NodeStore.ExprIdx;
const PatternIdx = NodeStore.PatternIdx;
const HeaderIdx = NodeStore.HeaderIdx;
const StatementIdx = NodeStore.StatementIdx;

const FormatFlags = enum { debug_binop, no_debug };

/// Formats all roc files in the specified path.
/// Handles both single files and directories
/// Returns the number of files formatted.
pub fn formatPath(gpa: std.mem.Allocator, base_dir: std.fs.Dir, path: []const u8) !usize {
    // TODO: update this to use the filesystem abstraction
    // When doing so, add a mock filesystem and some tests.
    var count: usize = 0;
    // First try as a directory.
    if (base_dir.openDir(path, .{ .iterate = true })) |const_dir| {
        var dir = const_dir;
        defer dir.close();
        // Walk is recursive.
        var walker = try dir.walk(gpa);
        defer walker.deinit();
        while (try walker.next()) |entry| {
            if (entry.kind == .file) {
                if (try formatFilePath(gpa, entry.dir, entry.basename)) {
                    count += 1;
                }
            }
        }
    } else |_| {
        if (try formatFilePath(gpa, base_dir, path)) {
            count += 1;
        }
    }

    return count;
}

fn formatFilePath(gpa: std.mem.Allocator, base_dir: std.fs.Dir, path: []const u8) !bool {
    const format_file_frame = tracy.namedFrame("format_file");
    defer format_file_frame.end();

    // Skip non ".roc" files.
    if (!std.mem.eql(u8, std.fs.path.extension(path), ".roc")) {
        return false;
    }

    const input_file = try base_dir.openFile(path, .{ .mode = .read_only });
    defer input_file.close();

    const contents = try input_file.reader().readAllAlloc(gpa, Filesystem.max_file_size);
    defer gpa.free(contents);

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var parse_ast = parse(&module_env, contents);
    defer parse_ast.deinit();
    if (parse_ast.errors.len > 0) {
        // TODO: pretty print the parse failures.
        const stderr = std.io.getStdErr().writer();
        try stderr.print("Failed to parse '{s}' for formatting.\nErrors:\n", .{path});
        for (parse_ast.errors) |err| {
            try stderr.print("\t{s}\n", .{@tagName(err.tag)});
        }
        fatal("\n", .{});
    }

    const output_file = try base_dir.createFile(path, .{});
    defer output_file.close();

    try formatAst(parse_ast, output_file.writer().any());

    return true;
}

/// Formats and writes out well-formed source of a Roc parse IR (AST).
/// Only returns an error if the underlying writer returns an error.
pub fn formatAst(ast: IR, writer: std.io.AnyWriter) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    var fmt = Formatter.init(ast, writer);

    var ignore_newline_for_first_statement = false;

    fmt.ast.store.emptyScratch();
    const file = fmt.ast.store.getFile();
    const maybe_output = try fmt.formatHeader(file.header);
    if (maybe_output == FormattedOutput.nothing_formatted) {
        ignore_newline_for_first_statement = true;
    }
    var newline_behavior: NewlineBehavior = .extra_newline_needed;
    for (fmt.ast.store.statementSlice(file.statements)) |s| {

        // If there was nothing formatted because the header was malformed,
        // then we don't want to add a newline
        if (ignore_newline_for_first_statement) {
            ignore_newline_for_first_statement = false;
        } else {
            try fmt.ensureNewline();
            if (newline_behavior == .extra_newline_needed) {
                try fmt.newline();
            }
        }

        newline_behavior = try fmt.formatStatement(s);
    }

    try fmt.flush();
}

/// The caller may need to know if anything was formatted, to handle newlines correctly.
const FormattedOutput = enum { something_formatted, nothing_formatted };

const NewlineBehavior = enum { no_extra_newline, extra_newline_needed };

/// Formatter for the roc parse ast.
const Formatter = struct {
    ast: IR,
    buffer: std.io.BufferedWriter(16 * 1024, std.io.AnyWriter),
    curr_indent: u32 = 0,
    flags: FormatFlags = .no_debug,
    // This starts true since beginning of file is considered a newline.
    has_newline: bool = true,

    /// Creates a new Formatter for the given parse IR.
    fn init(ast: IR, writer: std.io.AnyWriter) Formatter {
        return .{
            .ast = ast,
            .buffer = .{ .unbuffered_writer = writer },
        };
    }

    /// Deinits all data owned by the formatter object.
    fn flush(fmt: *Formatter) !void {
        try fmt.buffer.flush();
    }

    fn formatStatement(fmt: *Formatter, si: StatementIdx) !NewlineBehavior {
        const statement = fmt.ast.store.getStatement(si);
        switch (statement) {
            .decl => |d| {
                try fmt.formatPattern(d.pattern);
                try fmt.pushAll(" = ");
                try fmt.formatExpr(d.body);
                return .extra_newline_needed;
            },
            .expr => |e| {
                try fmt.formatExpr(e.expr);
                return .extra_newline_needed;
            },
            .import => |i| {
                try fmt.pushAll("import ");
                try fmt.formatIdent(i.module_name_tok, i.qualifier_tok);
                if (i.alias_tok) |a| {
                    try fmt.pushAll(" as ");
                    try fmt.pushTokenText(a);
                }
                if (i.exposes.span.len > 0) {
                    try fmt.pushAll(" exposing [");
                    var n: usize = 0;
                    for (fmt.ast.store.exposedItemSlice(i.exposes)) |tok| {
                        try fmt.formatExposedItem(tok);
                        if (n < i.exposes.span.len - 1) {
                            try fmt.pushAll(", ");
                        }
                        n += 1;
                    }
                    try fmt.push(']');
                }
                return .extra_newline_needed;
            },
            .type_decl => |d| {
                try fmt.formatTypeHeader(d.header);
                try fmt.pushAll(" : ");
                try fmt.formatTypeAnno(d.anno);
                return .extra_newline_needed;
            },
            .type_anno => |t| {
                try fmt.pushTokenText(t.name);
                try fmt.pushAll(" : ");
                try fmt.formatTypeAnno(t.anno);
                return .no_extra_newline;
            },
            .expect => |e| {
                try fmt.pushAll("expect ");
                try fmt.formatExpr(e.body);
                return .extra_newline_needed;
            },
            .crash => |c| {
                try fmt.pushAll("crash ");
                try fmt.formatExpr(c.expr);
                return .extra_newline_needed;
            },
            .@"return" => |r| {
                try fmt.pushAll("return ");
                try fmt.formatExpr(r.expr);
                return .extra_newline_needed;
            },
            .malformed => {
                return .no_extra_newline;
            },
        }
    }

    fn formatIdent(fmt: *Formatter, ident: TokenIdx, qualifier: ?TokenIdx) !void {
        if (qualifier) |q| {
            try fmt.pushTokenText(q);
            try fmt.push('.');
        }
        try fmt.pushTokenText(ident);
    }

    fn formatExpr(fmt: *Formatter, ei: ExprIdx) anyerror!void {
        const expr = fmt.ast.store.getExpr(ei);
        switch (expr) {
            .apply => |a| {
                try fmt.formatExpr(a.@"fn");
                try fmt.push('(');
                const args_len = a.args.span.len;
                var i: usize = 0;
                for (fmt.ast.store.exprSlice(a.args)) |arg| {
                    try fmt.formatExpr(arg);
                    i += 1;
                    if (i < args_len) {
                        try fmt.pushAll(", ");
                    }
                }
                try fmt.push(')');
            },
            .string_part => |s| {
                try fmt.pushTokenText(s.token);
            },
            .string => |s| {
                try fmt.push('"');
                var i: usize = 0;
                for (fmt.ast.store.exprSlice(s.parts)) |idx| {
                    const e = fmt.ast.store.getExpr(idx);
                    switch (e) {
                        .string_part => |str| {
                            try fmt.pushTokenText(str.token);
                        },
                        else => {
                            try fmt.pushAll("${");
                            try fmt.formatExpr(idx);
                            try fmt.push('}');
                        },
                    }
                    i += 1;
                }
                try fmt.push('"');
            },
            .ident => |i| {
                try fmt.formatIdent(i.token, i.qualifier);
            },
            .field_access => |fa| {
                try fmt.formatExpr(fa.left);
                try fmt.push('.');
                try fmt.formatExpr(fa.right);
            },
            .int => |i| {
                try fmt.pushTokenText(i.token);
            },
            .float => |f| {
                try fmt.pushTokenText(f.token);
            },
            .list => |l| {
                const multiline = fmt.ast.regionIsMultiline(l.region);
                try fmt.push('[');
                if (multiline) {
                    fmt.curr_indent += 1;
                }
                var i: usize = 0;
                for (fmt.ast.store.exprSlice(l.items)) |item| {
                    if (multiline) {
                        try fmt.newline();
                        try fmt.pushIndent();
                    }
                    try fmt.formatExpr(item);
                    if (!multiline and i < (l.items.span.len - 1)) {
                        try fmt.pushAll(", ");
                    }
                    if (multiline) {
                        try fmt.push(',');
                    }
                    i += 1;
                }
                if (multiline) {
                    fmt.curr_indent -= 1;
                    try fmt.newline();
                    try fmt.pushIndent();
                }
                try fmt.push(']');
            },
            .tuple => |t| {
                try fmt.push('(');
                var i: usize = 0;
                for (fmt.ast.store.exprSlice(t.items)) |item| {
                    try fmt.formatExpr(item);
                    if (i < (t.items.span.len - 1)) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                try fmt.push(')');
            },
            .record => |r| {
                try fmt.pushAll("{ ");
                var i: usize = 0;
                for (fmt.ast.store.recordFieldSlice(r.fields)) |fieldIdx| {
                    const field = fmt.ast.store.getRecordField(fieldIdx);
                    try fmt.pushTokenText(field.name);
                    if (field.value) |v| {
                        try fmt.pushAll(if (field.optional) "? " else ": ");
                        try fmt.formatExpr(v);
                    }
                    if (i < (r.fields.span.len - 1)) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                try fmt.pushAll(" }");
            },
            .lambda => |l| {
                try fmt.push('|');
                var i: usize = 0;
                const arg_slice = fmt.ast.store.patternSlice(l.args);

                // TODO -- this is a hack to avoid ambiguity with no arguments,
                // if we parse it again without the space it will be parsed as
                // a logical OR `||` instead
                //
                // desired behaviour described here https://roc.zulipchat.com/#narrow/channel/395097-compiler-development/topic/zig.20compiler.20-.20spike/near/504453049
                if (arg_slice.len == 0) {
                    try fmt.pushAll(" ");
                }

                for (arg_slice) |arg| {
                    try fmt.formatPattern(arg);
                    if (i < (l.args.span.len - 1)) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                try fmt.pushAll("| ");
                try fmt.formatExpr(l.body);
            },
            .unary_op => |op| {
                try fmt.pushTokenText(op.operator);
                try fmt.formatExpr(op.expr);
            },
            .bin_op => |op| {
                if (fmt.flags == .debug_binop) {
                    try fmt.push('(');
                }
                try fmt.formatExpr(op.left);
                try fmt.push(' ');
                try fmt.pushTokenText(op.operator);
                try fmt.push(' ');
                try fmt.formatExpr(op.right);
                if (fmt.flags == .debug_binop) {
                    try fmt.push(')');
                }
            },
            .suffix_single_question => |s| {
                try fmt.formatExpr(s.expr);
                try fmt.push('?');
            },
            .tag => |t| {
                try fmt.pushTokenText(t.token);
            },
            .if_then_else => |i| {
                try fmt.pushAll("if ");
                try fmt.formatExpr(i.condition);
                try fmt.push(' ');
                try fmt.formatExpr(i.then);
                try fmt.pushAll(" else ");
                try fmt.formatExpr(i.@"else");
            },
            .match => |m| {
                try fmt.pushAll("match ");
                try fmt.formatExpr(m.expr);
                try fmt.pushAll(" {");
                fmt.curr_indent += 1;
                for (fmt.ast.store.whenBranchSlice(m.branches)) |b| {
                    const branch = fmt.ast.store.getBranch(b);
                    try fmt.newline();
                    try fmt.pushIndent();
                    try fmt.formatPattern(branch.pattern);
                    try fmt.pushAll(" -> ");
                    try fmt.formatExpr(branch.body);
                }
                fmt.curr_indent -= 1;
                try fmt.newline();
                try fmt.pushIndent();
                try fmt.push('}');
            },
            .dbg => |d| {
                try fmt.pushAll("dbg ");
                try fmt.formatExpr(d.expr);
            },
            .block => |b| {
                try fmt.formatBody(b);
            },
            .ellipsis => |_| {
                try fmt.pushAll("...");
            },
            .malformed => {
                // format nothing for malformed expressions
            },
            else => {
                std.debug.panic("TODO: Handle formatting {s}", .{@tagName(expr)});
            },
        }
    }

    fn formatPattern(fmt: *Formatter, pi: PatternIdx) !void {
        const pattern = fmt.ast.store.getPattern(pi);
        switch (pattern) {
            .ident => |i| {
                try fmt.formatIdent(i.ident_tok, null);
            },
            .tag => |t| {
                try fmt.formatIdent(t.tag_tok, null);
                if (t.args.span.len > 0) {
                    try fmt.push('(');
                    var i: usize = 0;
                    for (fmt.ast.store.patternSlice(t.args)) |arg| {
                        try fmt.formatPattern(arg);
                        if (i < (t.args.span.len - 1)) {
                            try fmt.pushAll(", ");
                        }
                        i += 1;
                    }
                    try fmt.push(')');
                }
            },
            .string => |s| {
                try fmt.formatExpr(s.expr);
            },
            .number => |n| {
                try fmt.formatIdent(n.number_tok, null);
            },
            .record => |r| {
                try fmt.pushAll("{ ");
                var i: usize = 0;
                for (fmt.ast.store.patternRecordFieldSlice(r.fields)) |field_idx| {
                    const field = fmt.ast.store.getPatternRecordField(field_idx);
                    if (field.rest) {
                        try fmt.pushAll("..");
                        if (field.name != 0) {
                            try fmt.pushTokenText(field.name);
                        }
                        continue;
                    }
                    try fmt.pushTokenText(field.name);
                    if (field.value) |v| {
                        try fmt.pushAll(": ");
                        try fmt.formatPattern(v);
                    }
                    if (i < (r.fields.span.len - 1)) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                try fmt.pushAll(" }");
            },
            .list => |l| {
                try fmt.push('[');
                var i: usize = 0;
                for (fmt.ast.store.patternSlice(l.patterns)) |p| {
                    try fmt.formatPattern(p);
                    if (i < (l.patterns.span.len - 1)) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                try fmt.push(']');
            },
            .tuple => |t| {
                try fmt.push('(');
                var i: usize = 0;
                for (fmt.ast.store.patternSlice(t.patterns)) |p| {
                    try fmt.formatPattern(p);
                    if (i < (t.patterns.span.len - 1)) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                try fmt.push(')');
            },
            .list_rest => |r| {
                try fmt.pushAll("..");
                if (r.name) |n| {
                    try fmt.pushAll(" as ");
                    try fmt.pushTokenText(n);
                }
            },
            .underscore => |_| {
                try fmt.push('_');
            },
            .alternatives => |a| {
                var i: usize = 0;
                for (fmt.ast.store.patternSlice(a.patterns)) |p| {
                    try fmt.formatPattern(p);
                    if (i < (a.patterns.span.len - 1)) {
                        try fmt.pushAll(" | ");
                    }
                    i += 1;
                }
            },
            .malformed => {
                // format nothing for malformed patterns
            },
        }
    }

    fn formatExposedItem(fmt: *Formatter, idx: IR.NodeStore.ExposedItemIdx) !void {
        const item = fmt.ast.store.getExposedItem(idx);
        switch (item) {
            .lower_ident => |i| {
                try fmt.pushTokenText(i.ident);
                if (i.as) |a| {
                    try fmt.pushAll(" as ");
                    try fmt.pushTokenText(a);
                }
            },
            .upper_ident => |i| {
                try fmt.pushTokenText(i.ident);
                if (i.as) |a| {
                    try fmt.pushAll(" as ");
                    try fmt.pushTokenText(a);
                }
            },
            .upper_ident_star => |i| {
                try fmt.pushTokenText(i.ident);
                try fmt.pushAll(".*");
            },
        }
    }

    fn formatHeader(fmt: *Formatter, hi: HeaderIdx) !FormattedOutput {
        const trace = tracy.trace(@src());
        defer trace.end();

        const header = fmt.ast.store.getHeader(hi);
        switch (header) {
            .app => |a| {
                try fmt.pushAll("app [");
                // Format provides
                var i: usize = 0;
                for (fmt.ast.store.exposedItemSlice(a.provides)) |p| {
                    try fmt.formatExposedItem(p);
                    if (i < a.provides.span.len - 1) {
                        try fmt.pushAll(", ");
                    }
                }
                try fmt.pushAll("] { ");
                try fmt.pushTokenText(a.platform_name);
                try fmt.pushAll(": platform ");
                try fmt.formatExpr(a.platform);
                if (a.packages.span.len > 0) {
                    try fmt.push(',');
                }
                i = 0;
                for (fmt.ast.store.recordFieldSlice(a.packages)) |package| {
                    const field = fmt.ast.store.getRecordField(package);
                    try fmt.pushTokenText(field.name);
                    if (field.value) |v| {
                        try fmt.pushAll(": ");
                        try fmt.formatExpr(v);
                    }
                    if (i < a.packages.span.len) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                try fmt.pushAll(" }");
                try fmt.newline();
                return FormattedOutput.something_formatted;
            },
            .module => |m| {
                try fmt.pushAll("module [");
                var i: usize = 0;
                for (fmt.ast.store.exposedItemSlice(m.exposes)) |p| {
                    try fmt.formatExposedItem(p);
                    i += 1;
                    if (i < m.exposes.span.len) {
                        try fmt.pushAll(", ");
                    }
                }
                try fmt.push(']');
                try fmt.newline();
                return FormattedOutput.something_formatted;
            },
            .malformed => {
                // we have a malformed header... don't output anything as no header was parsed
                return FormattedOutput.nothing_formatted;
            },
            else => {
                std.debug.panic("TODO: Handle formatting {s}", .{@tagName(header)});
            },
        }
    }

    fn formatBody(fmt: *Formatter, body: IR.NodeStore.Body) !void {
        if (body.statements.span.len > 1) {
            fmt.curr_indent += 1;
            try fmt.push('{');
            for (fmt.ast.store.statementSlice(body.statements)) |s| {
                try fmt.ensureNewline();
                try fmt.pushIndent();
                _ = try fmt.formatStatement(s);
            }
            try fmt.ensureNewline();
            fmt.curr_indent -= 1;
            try fmt.pushIndent();
            try fmt.push('}');
        } else if (body.statements.span.len == 1) {
            for (fmt.ast.store.statementSlice(body.statements)) |s| {
                _ = try fmt.formatStatement(s);
            }
        } else {
            try fmt.pushAll("{}");
        }
    }

    fn formatTypeHeader(fmt: *Formatter, header: IR.NodeStore.TypeHeaderIdx) !void {
        const h = fmt.ast.store.getTypeHeader(header);
        try fmt.pushTokenText(h.name);
        if (h.args.span.len > 0) {
            try fmt.push(' ');
            var i: usize = 0;
            for (fmt.ast.store.tokenSlice(h.args)) |arg| {
                try fmt.pushTokenText(arg);
                if (i < (h.args.span.len - 1)) {
                    try fmt.push(' ');
                }
                i += 1;
            }
        }
    }

    fn formatTypeAnno(fmt: *Formatter, anno: IR.NodeStore.TypeAnnoIdx) !void {
        const a = fmt.ast.store.getTypeAnno(anno);
        switch (a) {
            .ty_var => |v| {
                try fmt.pushTokenText(v.tok);
            },
            .tag => |t| {
                try fmt.pushTokenText(t.tok);
                if (t.args.span.len > 0) {
                    try fmt.push('(');
                    var i: usize = 0;
                    for (fmt.ast.store.typeAnnoSlice(t.args)) |arg| {
                        try fmt.formatTypeAnno(arg);
                        if (i < (t.args.span.len - 1)) {
                            try fmt.pushAll(", ");
                        }
                        i += 1;
                    }
                    try fmt.push(')');
                }
            },
            .tuple => |t| {
                try fmt.push('(');
                var i: usize = 0;
                for (fmt.ast.store.typeAnnoSlice(t.annos)) |an| {
                    try fmt.formatTypeAnno(an);
                    if (i < (t.annos.span.len - 1)) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                try fmt.push(')');
            },
            .record => |r| {
                if (r.fields.span.len == 0) {
                    try fmt.pushAll("{}");
                    return;
                }
                try fmt.pushAll("{ ");
                var i: usize = 0;
                for (fmt.ast.store.annoRecordFieldSlice(r.fields)) |idx| {
                    const field = fmt.ast.store.getAnnoRecordField(idx);
                    try fmt.pushTokenText(field.name);
                    try fmt.pushAll(" : ");
                    try fmt.formatTypeAnno(field.ty);
                    if (i < (r.fields.span.len - 1)) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                try fmt.pushAll(" }");
            },
            .tag_union => |t| {
                try fmt.push('[');
                var i: usize = 0;
                for (fmt.ast.store.typeAnnoSlice(t.tags)) |tag| {
                    try fmt.formatTypeAnno(tag);
                    if (i < (t.tags.span.len - 1)) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                try fmt.push(']');
            },
            .@"fn" => |f| {
                var i: usize = 0;
                for (fmt.ast.store.typeAnnoSlice(f.args)) |idx| {
                    try fmt.formatTypeAnno(idx);
                    if (i < (f.args.span.len - 1)) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                try fmt.pushAll(" -> ");
                try fmt.formatTypeAnno(f.ret);
            },
            .parens => |p| {
                try fmt.push('(');
                try fmt.formatTypeAnno(p.anno);
                try fmt.push(')');
            },
            .underscore => |_| {
                try fmt.push('_');
            },
            .malformed => {
                // format nothing for malformed type annotations
            },
        }
    }

    fn ensureNewline(fmt: *Formatter) !void {
        if (fmt.has_newline) {
            return;
        }
        try fmt.newline();
    }

    fn newline(fmt: *Formatter) !void {
        try fmt.push('\n');
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

    fn push(fmt: *Formatter, c: u8) !void {
        fmt.has_newline = c == '\n';
        try fmt.buffer.writer().writeByte(c);
    }

    fn pushAll(fmt: *Formatter, str: []const u8) !void {
        if (str.len == 0) {
            return;
        }
        fmt.has_newline = str[str.len - 1] == '\n';
        try fmt.buffer.writer().writeAll(str);
    }

    fn pushIndent(fmt: *Formatter) !void {
        if (fmt.curr_indent == 0) {
            return;
        }
        for (0..fmt.curr_indent) |_| {
            try fmt.pushAll(indent);
        }
    }

    fn pushTokenText(fmt: *Formatter, ti: TokenIdx) !void {
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
        try fmt.pushAll(text);
    }
};

fn moduleFmtsSame(source: []const u8) !void {
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

    var result = std.ArrayList(u8).init(gpa);
    defer result.deinit();
    try formatAst(parse_ast, result.writer().any());

    try std.testing.expectEqualStrings(source, result.items);
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

    var fmt_result = std.ArrayList(u8).init(gpa);
    defer fmt_result.deinit();
    var formatter = Formatter.init(parse_ast, fmt_result.writer().any());
    formatter.flags = flags;
    try formatter.formatExpr(expr);
    try formatter.flush();

    try std.testing.expectEqualStrings(expected, fmt_result.items);
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

    var result = std.ArrayList(u8).init(gpa);
    try formatAst(parse_ast, result.writer().any());

    if (debug) {
        std.debug.print("Formatted:\n==========\n{s}\n==========\n\n", .{result.items});
    }
    return result.toOwnedSlice() catch |err| exitOnOom(err);
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
        \\import pf.Stdout exposing [line!, write!]
        \\
        \\import Something exposing [func as function, Type as ValueCategory, Custom.*]
        \\
        \\import BadName as GoodName
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

test "Multiline list formatting" {
    const expr = "[1,2,3,]";
    const expr2 =
        \\[1, 2,
        \\  3]
    ;
    const expected =
        \\[
        \\    1,
        \\    2,
        \\    3,
        \\]
    ;
    try exprFmtsTo(expr, expected, .no_debug);
    try exprFmtsTo(expr2, expected, .no_debug);
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

// TODO: replace this test with one that doesn't interact with the real filesystem.
test "format single file" {
    const gpa = std.testing.allocator;
    const roc_filename = "test.roc";

    const roc_file = try std.fs.cwd().createFile(roc_filename, .{ .read = true });
    defer roc_file.close();
    try roc_file.writeAll(
        \\module []
        \\
        \\foo =      "bar"
    );
    defer std.fs.cwd().deleteFile(roc_filename) catch std.debug.panic("Failed to clean up test.roc", .{});

    const count = try formatPath(gpa, std.fs.cwd(), roc_filename);
    try std.testing.expectEqual(1, count);

    // Reset file position to read formatted roc code
    try roc_file.seekTo(0);
    const formatted_code = try roc_file.reader().readAllAlloc(gpa, Filesystem.max_file_size);
    defer gpa.free(formatted_code);

    try std.testing.expectEqualStrings(
        \\module []
        \\
        \\foo = "bar"
    , formatted_code);
}
