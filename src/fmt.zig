//! Formatting logic for Roc modules.

const std = @import("std");
const parse = @import("check/parse.zig");
const collections = @import("collections.zig");
const Filesystem = @import("coordinate/Filesystem.zig");
const tokenizer = @import("check/parse/tokenize.zig");
const base = @import("base.zig");
const tracy = @import("tracy.zig");

const TokenizedBuffer = tokenizer.TokenizedBuffer;
const Token = tokenizer.Token;
const AST = parse.AST;
const Node = parse.Node;
const NodeStore = parse.NodeStore;

const exitOnOom = collections.utils.exitOnOom;
const fatal = collections.utils.fatal;

const FormatFlags = enum {
    debug_binop,
    no_debug,
};

/// Count of successfully formatted files along with files that failed to format.
pub const SuccessFailCount = struct { success: usize, failure: usize };

/// Formats all roc files in the specified path.
/// Handles both single files and directories
/// Returns the number of files successfully formatted and that failed to format.
pub fn formatPath(gpa: std.mem.Allocator, arena: std.mem.Allocator, base_dir: std.fs.Dir, path: []const u8) !SuccessFailCount {
    // TODO: update this to use the filesystem abstraction
    // When doing so, add a mock filesystem and some tests.
    const stderr = std.io.getStdErr().writer();

    var success_count: usize = 0;
    var failed_count: usize = 0;
    // First try as a directory.
    if (base_dir.openDir(path, .{ .iterate = true })) |const_dir| {
        var dir = const_dir;
        defer dir.close();
        // Walk is recursive.
        var walker = try dir.walk(arena);
        defer walker.deinit();
        while (try walker.next()) |entry| {
            if (entry.kind == .file) {
                if (formatFilePath(gpa, entry.dir, entry.basename)) |_| {
                    success_count += 1;
                } else |err| {
                    if (err != error.NotRocFile) {
                        try stderr.print("Failed to format {s}: {any}\n", .{ entry.path, err });
                        failed_count += 1;
                    }
                }
            }
        }
    } else |_| {
        if (formatFilePath(gpa, base_dir, path)) |_| {
            success_count += 1;
        } else |err| {
            if (err != error.NotRocFile) {
                try stderr.print("Failed to format {s}: {any}\n", .{ path, err });
                failed_count += 1;
            }
        }
    }

    return .{ .success = success_count, .failure = failed_count };
}

fn binarySearch(
    items: []const u32,
    needle: u32,
) ?usize {
    if (items.len == 0) return null;

    var low: usize = 0;
    var high: usize = items.len;

    // Find the insertion point (largest element <= needle)
    while (low < high) {
        // Avoid overflowing in the midpoint calculation
        const mid = low + (high - low) / 2;
        // Compare needle with items[mid]
        if (needle == items[mid]) {
            return mid; // Exact match
        } else if (needle > items[mid]) {
            low = mid + 1; // Look in upper half
        } else {
            high = mid; // Look in lower half
        }
    }

    // At this point, low is the insertion point
    // If low > 0, the largest element <= needle is at low-1
    if (low > 0) {
        // Check if the previous element is <= needle
        if (needle >= items[low - 1]) {
            return low - 1;
        }
    }

    return null; // No element is <= needle
}

/// Formats a single roc file at the specified path.
/// Returns errors on failure and files that don't end in `.roc`
pub fn formatFilePath(gpa: std.mem.Allocator, base_dir: std.fs.Dir, path: []const u8) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Skip non ".roc" files.
    if (!std.mem.eql(u8, std.fs.path.extension(path), ".roc")) {
        return error.NotRocFile;
    }

    const format_file_frame = tracy.namedFrame("format_file");
    defer format_file_frame.end();

    const input_file = try base_dir.openFile(path, .{ .mode = .read_only });
    defer input_file.close();

    const contents = blk: {
        const blk_trace = tracy.traceNamed(@src(), "readAllAlloc");
        defer blk_trace.end();

        if (input_file.stat()) |stat| {
            // Attempt to allocate exactly the right size first.
            // The avoids needless reallocs and saves some perf.
            const size = stat.size;
            const buf = try gpa.alloc(u8, @intCast(size));
            errdefer gpa.free(buf);
            if (try input_file.readAll(buf) != size) {
                // This is unexpected, the file is smaller than the size from stat.
                // It must have been modified inplace.
                // TODO: handle this more gracefully.
                return error.FileSizeChangedDuringRead;
            }
            break :blk buf;
        } else |_| {
            // Fallback on readToEndAlloc.
            const buf = try input_file.readToEndAlloc(gpa, Filesystem.max_file_size);
            break :blk buf;
        }
    };
    defer gpa.free(contents);

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var parse_ast = parse.parse(&module_env, contents);
    defer parse_ast.deinit();
    if (parse_ast.errors.len > 0) {
        parse_ast.toSExprStr(&module_env, std.io.getStdErr().writer().any()) catch @panic("Failed to print SExpr");
        try printParseErrors(gpa, contents, parse_ast);
        return error.ParsingFailed;
    }

    const output_file = try base_dir.createFile(path, .{});
    defer output_file.close();

    try formatAst(parse_ast, output_file.writer().any());
}

fn printParseErrors(gpa: std.mem.Allocator, source: []const u8, parse_ast: AST) !void {
    // compute offsets of each line, looping over bytes of the input
    var line_offsets = std.ArrayList(u32).init(gpa);
    defer line_offsets.deinit();
    try line_offsets.append(0);
    for (source, 0..) |c, i| {
        if (c == '\n') {
            try line_offsets.append(@intCast(i));
        }
    }

    const stderr = std.io.getStdErr().writer();
    try stderr.print("Errors:\n", .{});
    for (parse_ast.errors) |err| {
        const token_offset = parse_ast.tokens.tokens.items(.offset)[err.region.start];
        const line = binarySearch(line_offsets.items, token_offset) orelse unreachable;
        const column = token_offset - line_offsets.items[line];
        const token = parse_ast.tokens.tokens.items(.tag)[err.region.start];
        // TODO: pretty print the parse failures.
        try stderr.print("\t{s}, at token {s} at {d}:{d}\n", .{ @tagName(err.tag), @tagName(token), line + 1, column });
    }
}

fn formatIRNode(ast: AST, writer: std.io.AnyWriter, formatter: *const fn (*Formatter) anyerror!void) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    var fmt = Formatter.init(ast, writer);

    try formatter(&fmt);
    try fmt.flush();
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is a file.
/// Only returns an error if the underlying writer returns an error.
pub fn formatAst(ast: AST, writer: std.io.AnyWriter) !void {
    return formatIRNode(ast, writer, Formatter.formatFile);
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is a header.
/// Only returns an error if the underlying writer returns an error.
pub fn formatHeader(ast: AST, writer: std.io.AnyWriter) !void {
    return formatIRNode(ast, writer, formatHeaderInner);
}

fn formatHeaderInner(fmt: *Formatter) !void {
    return fmt.formatHeader(@enumFromInt(fmt.ast.root_node_idx));
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is a statement.
/// Only returns an error if the underlying writer returns an error.
pub fn formatStatement(ast: AST, writer: std.io.AnyWriter) !void {
    return formatIRNode(ast, writer, formatStatementInner);
}

fn formatStatementInner(fmt: *Formatter) !void {
    return fmt.formatStatement(@enumFromInt(fmt.ast.root_node_idx));
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is an expression.
/// Only returns an error if the underlying writer returns an error.
pub fn formatExpr(ast: AST, writer: std.io.AnyWriter) !void {
    return formatIRNode(ast, writer, formatExprNode);
}

fn formatExprNode(fmt: *Formatter) !void {
    _ = try fmt.formatExpr(@enumFromInt(fmt.ast.root_node_idx));
}

const NewlineBehavior = enum { no_extra_newline, extra_newline_needed };

/// Formatter for the roc parse ast.
const Formatter = struct {
    ast: AST,
    buffer: std.io.BufferedWriter(16 * 1024, std.io.AnyWriter),
    curr_indent: u32 = 0,
    flags: FormatFlags = .no_debug,
    // This starts true since beginning of file is considered a newline.
    has_newline: bool = true,

    /// Creates a new Formatter for the given parse IR.
    fn init(ast: AST, writer: std.io.AnyWriter) Formatter {
        return .{
            .ast = ast,
            .buffer = .{ .unbuffered_writer = writer },
        };
    }

    /// Deinits all data owned by the formatter object.
    fn flush(fmt: *Formatter) !void {
        try fmt.buffer.flush();
    }

    /// Emits a string containing the well-formed source of a Roc parse IR (AST).
    /// The resulting string is owned by the caller.
    pub fn formatFile(fmt: *Formatter) !void {
        fmt.ast.store.emptyScratch();
        const file = fmt.ast.store.getFile();
        const header_region = fmt.ast.store.nodes.items.items(.region)[@intFromEnum(file.header)];
        _ = try fmt.flushCommentsBefore(header_region.start);
        _ = try fmt.formatHeader(file.header);
        const statement_slice = fmt.ast.store.statementSlice(file.statements);
        for (statement_slice) |s| {
            const region = fmt.nodeRegion(@intFromEnum(s));
            _ = try fmt.flushCommentsBefore(region.start);
            try fmt.formatStatement(s);
        }
    }

    fn formatStatement(fmt: *Formatter, si: AST.Statement.Idx) !void {
        const statement = fmt.ast.store.getStatement(si);
        const node_region = fmt.nodeRegion(@intFromEnum(si));
        const multiline = fmt.ast.regionIsMultiline(node_region);
        var flushed = false;
        const orig_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = orig_indent;
        }
        switch (statement) {
            .decl => |d| {
                const pattern_region = fmt.nodeRegion(@intFromEnum(d.pattern));
                _ = try fmt.formatPattern(d.pattern);
                if (multiline and try fmt.flushCommentsAfter(pattern_region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                    try fmt.push('=');
                } else {
                    try fmt.pushAll(" = ");
                }
                const body_region = fmt.nodeRegion(@intFromEnum(d.body));
                if (multiline and try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                _ = try fmt.formatExpr(d.body);
            },
            .@"var" => |v| {
                try fmt.pushAll("var");
                if (multiline and try fmt.flushCommentsBefore(v.name)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.pushTokenText(v.name);
                if (multiline and try fmt.flushCommentsAfter(v.name)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.push('=');
                const body_region = fmt.nodeRegion(@intFromEnum(v.body));
                if (multiline and try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(v.body);
            },
            .expr => |e| {
                _ = try fmt.formatExpr(e.expr);
            },
            .import => |i| {
                try fmt.pushAll("import");
                if (multiline) {
                    flushed = try fmt.flushCommentsBefore(if (i.qualifier_tok) |q| q else i.module_name_tok);
                }
                if (!flushed) {
                    try fmt.push(' ');
                } else {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                try fmt.formatIdent(i.module_name_tok, i.qualifier_tok);
                if (multiline) {
                    flushed = try fmt.flushCommentsAfter(i.module_name_tok);
                }
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                if (i.alias_tok) |a| {
                    if (multiline) {
                        try fmt.pushAll("as");
                        flushed = try fmt.flushCommentsBefore(a);
                        if (!flushed) {
                            try fmt.push(' ');
                        } else {
                            try fmt.pushIndent();
                        }
                    } else {
                        try fmt.pushAll(" as ");
                    }
                    try fmt.pushTokenText(a);
                    if (i.exposes.span.len > 0) {
                        flushed = try fmt.flushCommentsAfter(a);
                    }
                }
                if (i.exposes.span.len > 0) {
                    if (flushed) {
                        try fmt.pushAll("exposing ");
                    } else {
                        try fmt.pushAll(" exposing ");
                    }
                    const items = fmt.ast.store.exposedItemSlice(i.exposes);
                    const items_region = fmt.regionInSlice(AST.ExposedItem.Idx, items);
                    // This is a near copy of formatCollection because to make that function
                    // work correctly, the exposed items have to be in a new Node type that
                    // will have its own region
                    const items_multiline = fmt.ast.regionIsMultiline(items_region);
                    const braces = Braces.square;
                    try fmt.push(braces.start());
                    if (items.len == 0) {
                        try fmt.push(braces.end());
                    } else {
                        if (items_multiline) {
                            fmt.curr_indent += 1;
                        }
                        var x: usize = 0;
                        var arg_region = fmt.nodeRegion(@intFromEnum(items[0]));
                        for (items) |item| {
                            arg_region = fmt.nodeRegion(@intFromEnum(item));
                            if (items_multiline) {
                                _ = try fmt.flushCommentsBefore(arg_region.start);
                                try fmt.pushIndent();
                            }
                            _ = try fmt.formatExposedItem(item);
                            if (!items_multiline and x < (items.len - 1)) {
                                try fmt.pushAll(", ");
                            }
                            if (items_multiline) {
                                try fmt.push(',');
                            }
                            x += 1;
                        }
                        if (items_multiline) {
                            _ = try fmt.flushCommentsAfter(arg_region.end);
                            fmt.curr_indent -= 1;
                            try fmt.pushIndent();
                        }
                        try fmt.push(braces.end());
                    }
                }
            },
            .type_decl => |d| {
                const header_region = fmt.nodeRegion(@intFromEnum(d.header));
                try fmt.formatTypeHeader(d.header);
                if (multiline and try fmt.flushCommentsAfter(header_region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.push(':');
                const anno_region = fmt.nodeRegion(@intFromEnum(d.anno));
                if (multiline and try fmt.flushCommentsBefore(anno_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatTypeAnno(d.anno);
                if (d.where) |w| {
                    _ = try fmt.flushCommentsAfter(anno_region.end);
                    fmt.curr_indent += 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                    try fmt.formatWhereConstraint(w);
                }
            },
            .type_anno => |t| {
                try fmt.pushTokenText(t.name);
                if (multiline and try fmt.flushCommentsAfter(t.name)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.push(':');
                const anno_region = fmt.nodeRegion(@intFromEnum(t.anno));
                if (multiline and try fmt.flushCommentsBefore(anno_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatTypeAnno(t.anno);
                if (t.where) |w| {
                    _ = try fmt.flushCommentsAfter(anno_region.end);
                    fmt.curr_indent += 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                    try fmt.formatWhereConstraint(w);
                }
            },
            .expect => |e| {
                try fmt.pushAll("expect");
                const body_region = fmt.nodeRegion(@intFromEnum(e.body));
                if (multiline and try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(e.body);
            },
            .@"for" => |f| {
                try fmt.pushAll("for");
                const patt_region = fmt.nodeRegion(@intFromEnum(f.patt));
                if (multiline and try fmt.flushCommentsBefore(patt_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatPattern(f.patt);
                if (multiline and try fmt.flushCommentsAfter(patt_region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.pushAll("in");
                const expr_region = fmt.nodeRegion(@intFromEnum(f.expr));
                if (multiline and try fmt.flushCommentsBefore(expr_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(f.expr);
                if (multiline and try fmt.flushCommentsAfter(expr_region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(f.body);
            },
            .crash => |c| {
                try fmt.pushAll("crash");
                const body_region = fmt.nodeRegion(@intFromEnum(c.expr));
                if (multiline and try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(c.expr);
            },
            .@"return" => |r| {
                try fmt.pushAll("return");
                const body_region = fmt.nodeRegion(@intFromEnum(r.expr));
                if (multiline and try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(r.expr);
            },
            .malformed => {
                // Output nothing for malformed node
            },
        }
    }

    fn formatWhereConstraint(fmt: *Formatter, w: AST.Collection.Idx) !void {
        const start_indent = fmt.curr_indent;
        defer fmt.curr_indent = start_indent;
        try fmt.pushAll("where");
        var i: usize = 0;
        const clause_coll = fmt.ast.store.getCollection(w);
        const clauses_multiline = fmt.ast.regionIsMultiline(clause_coll.region);
        const clause_slice = fmt.ast.store.whereClauseSlice(.{ .span = clause_coll.span });
        for (clause_slice) |clause| {
            const clause_region = fmt.nodeRegion(@intFromEnum(clause));
            const flushed_after_clause = try fmt.flushCommentsBefore(clause_region.start);
            if (i == 0 and (clauses_multiline or flushed_after_clause)) {
                fmt.curr_indent += 1;
            } else if (i == 0) {
                try fmt.push(' ');
            }
            if (clauses_multiline or flushed_after_clause) {
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            try fmt.formatWhereClause(clause);
            if ((!clauses_multiline and !flushed_after_clause) and i < clause_slice.len - 1) {
                try fmt.pushAll(", ");
            } else if (clauses_multiline or flushed_after_clause) {
                try fmt.push(',');
            }
            i += 1;
        }
    }

    fn formatIdent(fmt: *Formatter, ident: Token.Idx, qualifier: ?Token.Idx) !void {
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }
        if (qualifier) |q| {
            const multiline = fmt.ast.regionIsMultiline(AST.Region{ .start = q, .end = ident });
            try fmt.pushTokenText(q);
            if (multiline and try fmt.flushCommentsAfter(q)) {
                fmt.curr_indent += 1;
                try fmt.pushIndent();
            }
            const ident_tag = fmt.ast.tokens.tokens.items(.tag)[ident];
            if (ident_tag == .NoSpaceDotUpperIdent or ident_tag == .NoSpaceDotLowerIdent) {
                try fmt.push('.');
            }
        }
        try fmt.pushTokenText(ident);
    }

    const Braces = enum {
        round,
        square,
        curly,
        bar,

        fn start(b: Braces) u8 {
            return switch (b) {
                .round => '(',
                .square => '[',
                .curly => '{',
                .bar => '|',
            };
        }

        fn end(b: Braces) u8 {
            return switch (b) {
                .round => ')',
                .square => ']',
                .curly => '}',
                .bar => '|',
            };
        }
    };

    fn formatCollection(fmt: *Formatter, region: AST.Region, braces: Braces, comptime T: type, items: []T, formatter: fn (*Formatter, T) anyerror!AST.Region) !void {
        const multiline = fmt.ast.regionIsMultiline(region);
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }
        try fmt.push(braces.start());
        if (items.len == 0) {
            try fmt.push(braces.end());
            return;
        }
        if (multiline) {
            fmt.curr_indent += 1;
        } else if (braces == .curly) {
            try fmt.push(' ');
        }
        var i: usize = 0;
        for (items) |item_idx| {
            const item_region = fmt.nodeRegion(@intFromEnum(item_idx));
            if (multiline and try fmt.flushCommentsBefore(item_region.start)) {
                try fmt.ensureNewline();
                try fmt.pushIndent();
            } else if (multiline) {
                try fmt.newline();
                try fmt.pushIndent();
            }
            _ = try formatter(fmt, item_idx);
            if (!multiline and i < (items.len - 1)) {
                try fmt.pushAll(", ");
            }
            if (multiline) {
                try fmt.push(',');
            }
            i += 1;
        }
        if (multiline) {
            _ = try fmt.flushCommentsBefore(region.end);
            fmt.curr_indent -= 1;
            try fmt.ensureNewline();
            try fmt.pushIndent();
        } else if (braces == .curly) {
            try fmt.push(' ');
        }
        try fmt.push(braces.end());
    }

    fn formatRecordField(fmt: *Formatter, idx: AST.RecordField.Idx) !AST.Region {
        const field = fmt.ast.store.getRecordField(idx);
        try fmt.pushTokenText(field.name);
        if (field.value) |v| {
            try fmt.pushAll(if (field.optional) "? " else ": ");
            _ = try fmt.formatExpr(v);
        }

        return field.region;
    }

    const ExprFormatBehavior = enum {
        normal,
        no_indent_on_access,
    };

    fn formatExpr(fmt: *Formatter, ei: AST.Expr.Idx) anyerror!AST.Region {
        return formatExprInner(fmt, ei, .normal);
    }

    fn formatExprInner(fmt: *Formatter, ei: AST.Expr.Idx, format_behavior: ExprFormatBehavior) anyerror!AST.Region {
        const expr = fmt.ast.store.getExpr(ei);
        const region = fmt.nodeRegion(@intFromEnum(ei));
        const multiline = fmt.ast.regionIsMultiline(region);
        const indent_modifier: u32 = if (format_behavior == .no_indent_on_access and fmt.curr_indent > 0) 1 else 0;
        const curr_indent: u32 = fmt.curr_indent - indent_modifier;
        defer {
            fmt.curr_indent = curr_indent;
        }
        switch (expr) {
            .apply => |a| {
                _ = try fmt.formatExpr(a.@"fn");
                try fmt.formatCollection(region, .round, AST.Expr.Idx, fmt.ast.store.exprSlice(a.args), Formatter.formatExpr);
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
                            const part_region = fmt.nodeRegion(@intFromEnum(idx));
                            // Parts don't include the StringInterpolationStart and StringInterpolationEnd tokens
                            // That means they won't include any of the newlines between them and the actual expr.
                            // So we'll widen the region by one token for calculating multliline.
                            // Ideally, we'd also check if the expr itself is multiline, and if we will end up flushing, but
                            // we'll leave it as is for now
                            const part_is_multiline = fmt.ast.regionIsMultiline(AST.Region{ .start = part_region.start - 1, .end = part_region.end + 1 });
                            if (part_is_multiline) {
                                _ = try fmt.flushCommentsBefore(part_region.start);
                                try fmt.ensureNewline();
                                fmt.curr_indent += 1;
                                try fmt.pushIndent();
                            }
                            _ = try fmt.formatExpr(idx);
                            if (part_is_multiline) {
                                _ = try fmt.flushCommentsAfter(part_region.end);
                                try fmt.ensureNewline();
                                fmt.curr_indent -= 1;
                                try fmt.pushIndent();
                            }
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
                _ = try fmt.formatExpr(fa.left);
                const right_region = fmt.nodeRegion(@intFromEnum(fa.right));
                if (multiline and try fmt.flushCommentsBefore(right_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                try fmt.push('.');
                _ = try fmt.formatExprInner(fa.right, .no_indent_on_access);
            },
            .local_dispatch => |ld| {
                _ = try fmt.formatExpr(ld.left);
                if (multiline and try fmt.flushCommentsBefore(ld.operator)) {
                    if (format_behavior == .normal) {
                        fmt.curr_indent += 1;
                    }
                    try fmt.pushIndent();
                }
                try fmt.pushAll("->");
                if (multiline and try fmt.flushCommentsAfter(ld.operator)) {
                    try fmt.pushIndent();
                }
                _ = try fmt.formatExprInner(ld.right, .no_indent_on_access);
            },
            .int => |i| {
                try fmt.pushTokenText(i.token);
            },
            .float => |f| {
                try fmt.pushTokenText(f.token);
            },
            .list => |l| {
                try fmt.formatCollection(region, .square, AST.Expr.Idx, fmt.ast.store.exprSlice(l.items), Formatter.formatExpr);
            },
            .tuple => |t| {
                try fmt.formatCollection(region, .round, AST.Expr.Idx, fmt.ast.store.exprSlice(t.items), Formatter.formatExpr);
            },
            .record => |r| {
                try fmt.formatCollection(region, .curly, AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(r.fields), Formatter.formatRecordField);
            },
            .lambda => |l| {
                const args = fmt.ast.store.patternSlice(l.args);
                const body_region = fmt.nodeRegion(@intFromEnum(l.body));
                const args_region = fmt.regionInSlice(AST.Pattern.Idx, args);
                const args_are_multiline = fmt.ast.regionIsMultiline(AST.Region{ .start = l.region.start, .end = args_region.end });
                try fmt.push('|');
                if (args_are_multiline) {
                    fmt.curr_indent += 1;
                    _ = try fmt.flushCommentsAfter(l.region.start);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }
                var i: usize = 0;
                for (args) |arg| {
                    const arg_region = try fmt.formatPattern(arg);
                    if (args_are_multiline) {
                        try fmt.push(',');
                        _ = try fmt.flushCommentsAfter(arg_region.end);
                        try fmt.ensureNewline();
                        if (i < args.len - 1) {
                            try fmt.pushIndent();
                        }
                    } else if (i < args.len - 1) {
                        try fmt.pushAll(", ");
                    }
                    i += 1;
                }
                if (args_are_multiline) {
                    fmt.curr_indent -= 1;
                }
                try fmt.push('|');
                if (try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(l.body);
            },
            .unary_op => |op| {
                try fmt.pushTokenText(op.operator);
                _ = try fmt.formatExpr(op.expr);
            },
            .bin_op => |op| {
                if (fmt.flags == .debug_binop) {
                    try fmt.push('(');
                    if (multiline) {
                        try fmt.newline();
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    }
                }
                _ = try fmt.formatExpr(op.left);
                var pushed = false;
                if (multiline and try fmt.flushCommentsBefore(op.operator)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                    pushed = true;
                } else {
                    try fmt.push(' ');
                }
                try fmt.pushTokenText(op.operator);
                const right_region = fmt.nodeRegion(@intFromEnum(op.right));
                if (multiline and try fmt.flushCommentsBefore(right_region.start)) {
                    fmt.curr_indent += if (pushed) 0 else 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(op.right);
                if (fmt.flags == .debug_binop) {
                    if (multiline) {
                        fmt.curr_indent -= 1;
                        try fmt.pushIndent();
                    }
                    try fmt.push(')');
                }
            },
            .suffix_single_question => |s| {
                _ = try fmt.formatExpr(s.expr);
                try fmt.push('?');
            },
            .tag => |t| {
                try fmt.pushTokenText(t.token);
            },
            .if_then_else => |i| {
                try fmt.pushAll("if");
                const cond_region = fmt.nodeRegion(@intFromEnum(i.condition));
                var flushed = try fmt.flushCommentsBefore(cond_region.start);
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(i.condition);
                const then_region = fmt.nodeRegion(@intFromEnum(i.then));
                flushed = try fmt.flushCommentsBefore(then_region.start);
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(i.then);
                flushed = try fmt.flushCommentsAfter(then_region.end);
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.pushAll("else");
                const else_region = fmt.nodeRegion(@intFromEnum(i.@"else"));
                flushed = try fmt.flushCommentsBefore(else_region.start);
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(i.@"else");
            },
            .match => |m| {
                try fmt.pushAll("match ");
                _ = try fmt.formatExpr(m.expr);
                try fmt.pushAll(" {");
                fmt.curr_indent += 1;
                const branch_indent = fmt.curr_indent;
                const branches = fmt.ast.store.whenBranchSlice(m.branches);
                if (branches.len == 0) {
                    try fmt.push('}');
                    return region;
                }
                var branch_region = fmt.nodeRegion(@intFromEnum(branches[0]));
                for (branches) |b| {
                    fmt.curr_indent = branch_indent;
                    branch_region = fmt.nodeRegion(@intFromEnum(b));
                    const branch = fmt.ast.store.getBranch(b);
                    _ = try fmt.flushCommentsBefore(branch_region.start);
                    try fmt.pushIndent();
                    const pattern_region = try fmt.formatPattern(branch.pattern);
                    var flushed = try fmt.flushCommentsAfter(pattern_region.end);
                    if (flushed) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                        try fmt.pushAll("=>");
                    } else {
                        try fmt.pushAll(" =>");
                    }
                    const body_region = fmt.nodeRegion(@intFromEnum(branch.body));
                    flushed = try fmt.flushCommentsBefore(body_region.start);
                    if (flushed) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    _ = try fmt.formatExpr(branch.body);
                }
                fmt.curr_indent -= 1;
                try fmt.newline();
                try fmt.pushIndent();
                try fmt.push('}');
            },
            .dbg => |d| {
                try fmt.pushAll("dbg");
                const expr_node = fmt.nodeRegion(@intFromEnum(d.expr));
                if (multiline and try fmt.flushCommentsBefore(expr_node.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(d.expr);
            },
            .block => |b| {
                try fmt.formatBody(b);
            },
            .ellipsis => |_| {
                try fmt.pushAll("...");
            },
            .malformed => {
                // Output nothing for malformed node
            },
            else => {
                std.debug.panic("TODO: Handle formatting {s}", .{@tagName(expr)});
            },
        }
        return region;
    }

    fn formatPatternRecordField(fmt: *Formatter, idx: AST.PatternRecordField.Idx) !AST.Region {
        const field = fmt.ast.store.getPatternRecordField(idx);
        const multiline = fmt.ast.regionIsMultiline(field.region);
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }
        if (field.rest) {
            try fmt.pushAll("..");
            if (multiline and try fmt.flushCommentsBefore(field.name)) {
                fmt.curr_indent += 1;
                try fmt.pushIndent();
            }
            if (field.name != 0) {
                try fmt.pushTokenText(field.name);
            }
        } else {
            try fmt.pushTokenText(field.name);
            if (field.value) |v| {
                if (multiline and try fmt.flushCommentsAfter(field.name)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                try fmt.push(':');
                const v_region = fmt.nodeRegion(@intFromEnum(v));
                if (multiline and try fmt.flushCommentsBefore(v_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatPattern(v);
            }
        }
        return field.region;
    }

    fn formatPattern(fmt: *Formatter, pi: AST.Pattern.Idx) !AST.Region {
        const pattern = fmt.ast.store.getPattern(pi);
        var region = AST.Region{ .start = 0, .end = 0 };
        switch (pattern) {
            .ident => |i| {
                region = i.region;
                try fmt.formatIdent(i.ident_tok, null);
            },
            .tag => |t| {
                region = t.region;
                try fmt.formatIdent(t.tag_tok, null);
                if (t.args.span.len > 0) {
                    try fmt.formatCollection(region, .round, AST.Pattern.Idx, fmt.ast.store.patternSlice(t.args), Formatter.formatPattern);
                }
            },
            .string => |s| {
                region = s.region;
                _ = try fmt.formatExpr(s.expr);
            },
            .number => |n| {
                region = n.region;
                try fmt.formatIdent(n.number_tok, null);
            },
            .record => |r| {
                region = r.region;
                try fmt.formatCollection(region, .curly, AST.PatternRecordField.Idx, fmt.ast.store.patternRecordFieldSlice(r.fields), Formatter.formatPatternRecordField);
            },
            .list => |l| {
                region = l.region;
                try fmt.formatCollection(region, .square, AST.Pattern.Idx, fmt.ast.store.patternSlice(l.patterns), Formatter.formatPattern);
            },
            .tuple => |t| {
                region = t.region;
                try fmt.formatCollection(region, .round, AST.Pattern.Idx, fmt.ast.store.patternSlice(t.patterns), Formatter.formatPattern);
            },
            .list_rest => |r| {
                region = r.region;
                const curr_indent = fmt.curr_indent;
                defer {
                    fmt.curr_indent = curr_indent;
                }
                try fmt.pushAll("..");
                if (r.name) |n| {
                    const multiline = fmt.ast.regionIsMultiline(region);
                    if (multiline and try fmt.flushCommentsAfter(region.start)) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    try fmt.pushAll("as");
                    if (multiline and try fmt.flushCommentsBefore(n)) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    try fmt.pushTokenText(n);
                }
            },
            .underscore => |u| {
                region = u.region;
                try fmt.push('_');
            },
            .alternatives => |a| {
                const curr_indent = fmt.curr_indent;
                defer {
                    fmt.curr_indent = curr_indent;
                }
                region = a.region;
                const multiline = fmt.ast.regionIsMultiline(region);
                var i: usize = 0;
                const patterns = fmt.ast.store.patternSlice(a.patterns);
                for (patterns) |p| {
                    const pattern_region = fmt.nodeRegion(@intFromEnum(p));
                    _ = try fmt.formatPattern(p);
                    fmt.curr_indent = curr_indent;
                    if (i < (a.patterns.span.len - 1)) {
                        if (multiline) {
                            _ = try fmt.flushCommentsAfter(pattern_region.end);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        try fmt.push('|');
                        const next_region = fmt.nodeRegion(@intFromEnum(patterns[i + 1]));
                        const flushed = try fmt.flushCommentsBefore(next_region.start);
                        if (flushed) {
                            fmt.curr_indent += 1;
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                    }
                    i += 1;
                }
            },
            .malformed => {
                // Output nothing for malformed node
            },
        }
        return region;
    }

    fn formatExposedItem(fmt: *Formatter, idx: AST.ExposedItem.Idx) !AST.Region {
        const item = fmt.ast.store.getExposedItem(idx);
        var region = AST.Region{ .start = 0, .end = 0 };
        switch (item) {
            .lower_ident => |i| {
                region = i.region;
                try fmt.pushTokenText(i.ident);
                if (i.as) |a| {
                    try fmt.pushAll(" as ");
                    try fmt.pushTokenText(a);
                }
            },
            .upper_ident => |i| {
                region = i.region;
                try fmt.pushTokenText(i.ident);
                if (i.as) |a| {
                    try fmt.pushAll(" as ");
                    try fmt.pushTokenText(a);
                }
            },
            .upper_ident_star => |i| {
                region = i.region;
                try fmt.pushTokenText(i.ident);
                try fmt.pushAll(".*");
            },
        }

        return region;
    }

    fn formatHeader(fmt: *Formatter, hi: AST.Header.Idx) !void {
        const header = fmt.ast.store.getHeader(hi);
        const start_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = start_indent;
        }
        switch (header) {
            .app => |a| {
                const multiline = fmt.ast.regionIsMultiline(a.region);
                const provides = fmt.ast.store.getCollection(a.provides);
                try fmt.pushAll("app");
                if (multiline and try fmt.flushCommentsAfter(a.region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }

                try fmt.formatCollection(
                    provides.region,
                    .square,
                    AST.ExposedItem.Idx,
                    fmt.ast.store.exposedItemSlice(.{ .span = provides.span }),
                    Formatter.formatExposedItem,
                );

                if (multiline and try fmt.flushCommentsAfter(provides.region.end)) {
                    if (fmt.curr_indent == start_indent) {
                        fmt.curr_indent += 1;
                    }
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                const packages = fmt.ast.store.getCollection(a.packages);
                const packages_multiline = fmt.ast.regionIsMultiline(packages.region);
                try fmt.push('{');
                if (packages_multiline) {
                    fmt.curr_indent += 1;
                } else {
                    try fmt.push(' ');
                }

                var platform_field: ?AST.RecordField.Idx = null;
                var package_fields_list = try std.ArrayListUnmanaged(AST.RecordField.Idx).initCapacity(fmt.ast.store.gpa, 10);
                var i: usize = 0;
                const packages_slice = fmt.ast.store.recordFieldSlice(.{ .span = packages.span });
                for (packages_slice) |package_idx| {
                    if (package_idx == a.platform_idx) {
                        platform_field = package_idx;
                        continue;
                    }
                    try package_fields_list.append(fmt.ast.store.gpa, package_idx);
                }
                i = 0;
                const package_fields = try package_fields_list.toOwnedSlice(fmt.ast.store.gpa);
                defer fmt.ast.store.gpa.free(package_fields);

                if (platform_field) |field_idx| {
                    const field = fmt.ast.store.getRecordField(field_idx);
                    if (packages_multiline and try fmt.flushCommentsBefore(field.region.start)) {
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    } else if (packages_multiline) {
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                    try fmt.pushTokenText(field.name);
                    if (field.value) |v| {
                        try fmt.push(':');
                        try fmt.push(' ');
                        try fmt.pushAll("platform");
                        try fmt.push(' ');
                        _ = try fmt.formatExpr(v);
                    }
                    if (!packages_multiline and package_fields.len > 0) {
                        try fmt.pushAll(", ");
                    }
                    if (packages_multiline) {
                        try fmt.push(',');
                    }
                }
                for (package_fields) |field_idx| {
                    const item_region = fmt.nodeRegion(@intFromEnum(field_idx));
                    if (packages_multiline and try fmt.flushCommentsBefore(item_region.start)) {
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    } else if (packages_multiline) {
                        try fmt.newline();
                        try fmt.pushIndent();
                    }
                    _ = try fmt.formatRecordField(field_idx);
                    if (!packages_multiline and i < package_fields.len - 1) {
                        try fmt.pushAll(", ");
                    } else if (packages_multiline) {
                        try fmt.push(',');
                    }
                }
                if (packages_multiline) {
                    _ = try fmt.flushCommentsBefore(packages.region.end);
                    fmt.curr_indent -= 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }

                try fmt.push('}');
            },
            .module => |m| {
                try fmt.pushAll("module");
                const multiline = fmt.ast.regionIsMultiline(m.region);
                const exposes = fmt.ast.store.getCollection(m.exposes);
                if (multiline and try fmt.flushCommentsBefore(exposes.region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatCollection(
                    exposes.region,
                    .square,
                    AST.ExposedItem.Idx,
                    fmt.ast.store.exposedItemSlice(.{ .span = exposes.span }),
                    Formatter.formatExposedItem,
                );
            },
            .hosted => |h| {
                try fmt.pushAll("hosted");
                const multiline = fmt.ast.regionIsMultiline(h.region);
                const exposes = fmt.ast.store.getCollection(h.exposes);
                if (multiline and try fmt.flushCommentsBefore(exposes.region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatCollection(
                    exposes.region,
                    .square,
                    AST.ExposedItem.Idx,
                    fmt.ast.store.exposedItemSlice(.{ .span = exposes.span }),
                    Formatter.formatExposedItem,
                );
            },
            .package => |p| {
                try fmt.pushAll("package");
                defer {
                    fmt.curr_indent = 0;
                }
                const multiline = fmt.ast.regionIsMultiline(p.region);
                if (multiline) {
                    _ = try fmt.flushCommentsAfter(p.region.start);
                    try fmt.ensureNewline();
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                const exposes = fmt.ast.store.getCollection(p.exposes);
                // TODO: This needs to be extended to the next CloseSquare
                try fmt.formatCollection(
                    exposes.region,
                    .square,
                    AST.ExposedItem.Idx,
                    fmt.ast.store.exposedItemSlice(.{ .span = exposes.span }),
                    Formatter.formatExposedItem,
                );
                if (multiline) {
                    try fmt.newline();
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                const packages = fmt.ast.store.getCollection(p.packages);
                try fmt.formatCollection(
                    packages.region,
                    .curly,
                    AST.RecordField.Idx,
                    fmt.ast.store.recordFieldSlice(.{ .span = packages.span }),
                    Formatter.formatRecordField,
                );
            },
            .platform => |p| {
                const multiline = fmt.ast.regionIsMultiline(p.region);
                try fmt.pushAll("platform");
                if (multiline and try fmt.flushCommentsAfter(p.region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.push('"');
                try fmt.pushTokenText(p.name);
                try fmt.push('"');
                _ = try fmt.flushCommentsAfter(p.name + 1);
                fmt.curr_indent = start_indent + 1; // Reset to always be this
                try fmt.ensureNewline();
                try fmt.pushIndent();

                try fmt.pushAll("requires");
                const rigids = fmt.ast.store.getCollection(p.requires_rigids);
                if (multiline and try fmt.flushCommentsBefore(rigids.region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatCollection(
                    rigids.region,
                    .curly,
                    AST.ExposedItem.Idx,
                    fmt.ast.store.exposedItemSlice(.{ .span = rigids.span }),
                    Formatter.formatExposedItem,
                );
                if (multiline and try fmt.flushCommentsAfter(rigids.region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                // Signatures
                _ = try fmt.formatTypeAnno(p.requires_signatures);
                const signatures_region = fmt.nodeRegion(@intFromEnum(p.requires_signatures));
                if (multiline and try fmt.flushCommentsAfter(signatures_region.end)) {
                    fmt.curr_indent = start_indent + 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.newline();
                    try fmt.pushIndent();
                }
                const exposes = fmt.ast.store.getCollection(p.exposes);
                try fmt.pushAll("exposes");
                if (multiline and try fmt.flushCommentsBefore(exposes.region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatCollection(
                    exposes.region,
                    .square,
                    AST.ExposedItem.Idx,
                    fmt.ast.store.exposedItemSlice(.{ .span = exposes.span }),
                    Formatter.formatExposedItem,
                );
                if (multiline and try fmt.flushCommentsAfter(exposes.region.end)) {
                    fmt.curr_indent = start_indent + 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.newline();
                    try fmt.pushIndent();
                }
                try fmt.pushAll("packages");
                const packages = fmt.ast.store.getCollection(p.packages);
                if (multiline and try fmt.flushCommentsBefore(packages.region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatCollection(
                    packages.region,
                    .curly,
                    AST.RecordField.Idx,
                    fmt.ast.store.recordFieldSlice(.{ .span = packages.span }),
                    Formatter.formatRecordField,
                );
                if (multiline and try fmt.flushCommentsAfter(packages.region.end)) {
                    fmt.curr_indent = start_indent + 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.newline();
                    try fmt.pushIndent();
                }
                try fmt.pushAll("provides");
                const provides = fmt.ast.store.getCollection(p.provides);
                if (multiline and try fmt.flushCommentsBefore(provides.region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatCollection(
                    provides.region,
                    .square,
                    AST.ExposedItem.Idx,
                    fmt.ast.store.exposedItemSlice(.{ .span = provides.span }),
                    Formatter.formatExposedItem,
                );
            },
            .malformed => {},
        }
    }

    fn nodeRegion(fmt: *Formatter, idx: u32) AST.Region {
        return fmt.ast.store.nodes.items.items(.region)[idx];
    }

    fn formatBody(fmt: *Formatter, body: AST.Body) !void {
        const multiline = fmt.ast.regionIsMultiline(body.region);
        if (multiline or body.statements.span.len > 1) {
            fmt.curr_indent += 1;
            try fmt.push('{');
            for (fmt.ast.store.statementSlice(body.statements)) |s| {
                const region = fmt.nodeRegion(@intFromEnum(s));
                _ = try fmt.flushCommentsBefore(region.start);
                try fmt.ensureNewline();
                try fmt.pushIndent();
                try fmt.formatStatement(s);
            }
            _ = try fmt.flushCommentsBefore(body.region.end);
            try fmt.ensureNewline();
            fmt.curr_indent -= 1;
            try fmt.pushIndent();
            try fmt.push('}');
        } else if (body.statements.span.len == 1) {
            for (fmt.ast.store.statementSlice(body.statements)) |s| {
                try fmt.formatStatement(s);
            }
        } else {
            try fmt.pushAll("{}");
        }
    }

    fn formatTypeHeader(fmt: *Formatter, header: AST.TypeHeader.Idx) !void {
        const h = fmt.ast.store.getTypeHeader(header);
        try fmt.pushTokenText(h.name);
        if (h.args.span.len > 0) {
            try fmt.formatCollection(h.region, .round, AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(h.args), Formatter.formatTypeAnno);
        }
    }

    fn formatAnnoRecordField(fmt: *Formatter, idx: AST.AnnoRecordField.Idx) !AST.Region {
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }
        const field = fmt.ast.store.getAnnoRecordField(idx);
        const multiline = fmt.ast.regionIsMultiline(field.region);
        try fmt.pushTokenText(field.name);
        if (multiline and try fmt.flushCommentsAfter(field.name)) {
            fmt.curr_indent += 1;
            try fmt.pushIndent();
        } else {
            try fmt.push(' ');
        }
        try fmt.push(':');
        const anno_region = fmt.nodeRegion(@intFromEnum(field.ty));
        if (multiline and try fmt.flushCommentsBefore(anno_region.start)) {
            fmt.curr_indent += 1;
            try fmt.pushIndent();
        } else {
            try fmt.push(' ');
        }
        _ = try fmt.formatTypeAnno(field.ty);
        return field.region;
    }

    fn formatWhereClause(fmt: *Formatter, idx: AST.WhereClause.Idx) !void {
        const clause = fmt.ast.store.getWhereClause(idx);
        const start_indent = fmt.curr_indent;
        defer fmt.curr_indent = start_indent;
        switch (clause) {
            .alias => |c| {
                const multiline = fmt.ast.regionIsMultiline(c.region);
                try fmt.pushTokenText(c.var_tok);
                if (multiline and try fmt.flushCommentsAfter(c.var_tok)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                try fmt.push('.');
                if (multiline and try fmt.flushCommentsBefore(c.alias_tok)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                try fmt.pushTokenText(c.alias_tok);
            },
            .method => |c| {
                const multiline = fmt.ast.regionIsMultiline(c.region);
                try fmt.pushTokenText(c.var_tok);
                if (multiline and try fmt.flushCommentsAfter(c.var_tok)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                try fmt.push('.');
                try fmt.pushTokenText(c.name_tok);
                const args_coll = fmt.ast.store.getCollection(c.args);
                if (multiline and try fmt.flushCommentsBefore(args_coll.region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                const args = fmt.ast.store.typeAnnoSlice(.{ .span = args_coll.span });
                try fmt.formatCollection(
                    args_coll.region,
                    .round,
                    AST.TypeAnno.Idx,
                    args,
                    Formatter.formatTypeAnno,
                );
                if (multiline and try fmt.flushCommentsAfter(args_coll.region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                    try fmt.pushAll("->");
                } else {
                    try fmt.pushAll(" ->");
                }
                const ret_region = fmt.nodeRegion(@intFromEnum(c.ret_anno));
                if (multiline and try fmt.flushCommentsBefore(ret_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatTypeAnno(c.ret_anno);
            },
            .mod_method => |c| {
                const multiline = fmt.ast.regionIsMultiline(c.region);
                try fmt.pushAll("module(");
                const tags = fmt.ast.tokens.tokens.items(.tag);
                const varNeedsFlush = tags[c.var_tok - 1] == .Newline or (tags.len > (c.var_tok + 1) and tags[c.var_tok + 1] == .Newline);
                if (varNeedsFlush) {
                    _ = try fmt.flushCommentsBefore(c.var_tok);
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                try fmt.pushTokenText(c.var_tok);
                if (varNeedsFlush) {
                    _ = try fmt.flushCommentsAfter(c.var_tok);
                    fmt.curr_indent -= 1;
                    try fmt.pushIndent();
                }
                try fmt.push(')');
                try fmt.push('.');
                try fmt.pushTokenText(c.name_tok);
                const args_coll = fmt.ast.store.getCollection(c.args);
                if (multiline and try fmt.flushCommentsBefore(args_coll.region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                const args = fmt.ast.store.typeAnnoSlice(.{ .span = args_coll.span });
                try fmt.formatCollection(
                    args_coll.region,
                    .round,
                    AST.TypeAnno.Idx,
                    args,
                    Formatter.formatTypeAnno,
                );
                if (multiline and try fmt.flushCommentsAfter(args_coll.region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                    try fmt.pushAll("->");
                } else {
                    try fmt.pushAll(" ->");
                }
                const ret_region = fmt.nodeRegion(@intFromEnum(c.ret_anno));
                if (multiline and try fmt.flushCommentsBefore(ret_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatTypeAnno(c.ret_anno);
            },
            .malformed => {
                // Output nothing for malformed node
            },
        }
    }

    fn formatTypeAnno(fmt: *Formatter, anno: AST.TypeAnno.Idx) !AST.Region {
        const a = fmt.ast.store.getTypeAnno(anno);
        var region = AST.Region{ .start = 0, .end = 0 };
        switch (a) {
            .apply => |app| {
                const slice = fmt.ast.store.typeAnnoSlice(app.args);
                const first = slice[0];
                _ = try fmt.formatTypeAnno(first);
                const rest = slice[1..];
                try fmt.formatCollection(app.region, .round, AST.TypeAnno.Idx, rest, Formatter.formatTypeAnno);
            },
            .ty_var => |v| {
                region = v.region;
                try fmt.pushTokenText(v.tok);
            },
            .ty => |t| {
                try fmt.pushTokenText(t.region.start);
            },
            .mod_ty => |t| {
                try fmt.pushTokenText(t.region.start);
                try fmt.pushAll(".");
                try fmt.pushTokenText(t.region.end);
            },
            .tuple => |t| {
                region = t.region;
                try fmt.formatCollection(t.region, .round, AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(t.annos), Formatter.formatTypeAnno);
            },
            .record => |r| {
                region = r.region;
                try fmt.formatCollection(region, .curly, AST.AnnoRecordField.Idx, fmt.ast.store.annoRecordFieldSlice(r.fields), Formatter.formatAnnoRecordField);
            },
            .tag_union => |t| {
                region = t.region;
                try fmt.formatCollection(t.region, .square, AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(t.tags), Formatter.formatTypeAnno);
            },
            .@"fn" => |f| {
                region = f.region;
                const multiline = fmt.ast.regionIsMultiline(region);

                var i: usize = 0;
                const args = fmt.ast.store.typeAnnoSlice(f.args);
                for (args) |idx| {
                    const arg_region = fmt.nodeRegion(@intFromEnum(idx));
                    if (multiline and i > 0) {
                        _ = try fmt.flushCommentsBefore(arg_region.start);
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                    _ = try fmt.formatTypeAnno(idx);
                    if (i < (f.args.span.len - 1)) {
                        if (multiline) {
                            try fmt.push(',');
                        } else {
                            try fmt.pushAll(", ");
                        }
                    }
                    i += 1;
                }
                try fmt.pushAll(if (f.effectful) " =>" else " ->");
                const ret_region = fmt.nodeRegion(@intFromEnum(f.ret));
                if (multiline and try fmt.flushCommentsBefore(ret_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatTypeAnno(f.ret);
            },
            .parens => |p| {
                region = p.region;
                const multiline = fmt.ast.regionIsMultiline(region);
                try fmt.push('(');
                if (multiline) {
                    _ = try fmt.flushCommentsAfter(region.start);
                    fmt.curr_indent += 1;
                    try fmt.newline();
                    try fmt.pushIndent();
                }
                const anno_region = try fmt.formatTypeAnno(p.anno);
                _ = try fmt.flushCommentsAfter(anno_region.end);
                try fmt.push(')');
            },
            .underscore => |u| {
                region = u.region;
                try fmt.push('_');
            },
            .malformed => {
                // Output nothing for malformed node
            },
        }

        return region;
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

    fn flushCommentsBefore(fmt: *Formatter, tokenIdx: Token.Idx) !bool {
        if (tokenIdx == 0) {
            return false;
        }
        const tags = fmt.ast.tokens.tokens.items(.tag);
        const prevNewline = tokenIdx - 1;
        if (tags[prevNewline] != .Newline) {
            return false;
        }
        var first = prevNewline;
        // Go back as long as we see newlines
        while (first > 0 and tags[first - 1] == .Newline) {
            first -= 1;
        }
        var i = first;
        // Now print them in order
        while (i <= prevNewline) {
            const newline_tok = fmt.ast.tokens.tokens.get(i);
            std.debug.assert(newline_tok.tag == .Newline);
            const start = newline_tok.offset;
            const end = start + newline_tok.extra.length;
            if (end > start) {
                if (i == 0 or i > first) {
                    try fmt.pushIndent();
                    try fmt.push('#');
                } else {
                    try fmt.pushAll(" #");
                }
                const comment_text = fmt.ast.source[start..end];
                if (comment_text[0] != ' ') {
                    try fmt.push(' ');
                }
                try fmt.pushAll(comment_text);
            }
            try fmt.newline();
            i += 1;
        }
        return true;
    }

    fn flushCommentsAfter(fmt: *Formatter, tokenIdx: Token.Idx) !bool {
        const tags = fmt.ast.tokens.tokens.items(.tag);
        var nextNewline = tokenIdx + 1;
        if (nextNewline >= tags.len) {
            return false;
        }
        if (tags[nextNewline] == .Comma) {
            nextNewline += 1;
        }
        if (nextNewline >= tags.len or tags[nextNewline] != .Newline) {
            return false;
        }
        while (nextNewline < tags.len and tags[nextNewline] == .Newline) {
            const newline_tok = fmt.ast.tokens.tokens.get(nextNewline);
            const start = newline_tok.offset;
            const end = start + newline_tok.extra.length;
            if (end > start) {
                try fmt.pushAll(" #");
                const comment_text = fmt.ast.source[start..end];
                if (comment_text[0] != ' ') {
                    try fmt.push(' ');
                }
                try fmt.pushAll(comment_text);
            }
            try fmt.newline();
            nextNewline += 1;
        }
        return true;
    }

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
            try fmt.push('\t');
        }
    }

    fn pushTokenText(fmt: *Formatter, ti: Token.Idx) !void {
        const tag = fmt.ast.tokens.tokens.items(.tag)[ti];
        const region = fmt.ast.tokens.resolve(ti);
        var start = region.start.offset;
        switch (tag) {
            .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent, .DotLowerIdent => {
                start += 1;
            },
            else => {},
        }

        const text = fmt.ast.source[start..region.end.offset];
        try fmt.pushAll(text);
    }

    fn regionInSlice(fmt: *Formatter, comptime T: anytype, slice: []T) AST.Region {
        if (slice.len == 0) {
            return AST.Region.empty();
        }
        const first: usize = @intFromEnum(slice[0]);
        const last: usize = @intFromEnum(slice[slice.len - 1]);
        const first_region = fmt.ast.store.nodes.items.items(.region)[first];
        const last_region = fmt.ast.store.nodes.items.items(.region)[last];
        return first_region.spanAcross(last_region);
    }

    fn displayRegion(fmt: *Formatter, region: AST.Region) void {
        const tags = fmt.ast.tokens.tokens.items(.tag);
        return std.debug.print("[{s}@{d}...{s}@{d}]\n", .{ @tagName(tags[region.start]), region.start, @tagName(tags[region.end]), region.end });
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

    if (parse_ast.errors.len > 0) {
        try printParseErrors(gpa, source, parse_ast);
        std.debug.panic("Test failed with parse errors", .{});
    }

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

    var parse_ast = AST{
        .source = source,
        .tokens = result.tokens,
        .store = parser.store,
        .errors = errors,
    };
    defer parse_ast.deinit();
    defer std.testing.allocator.free(parse_ast.errors);

    std.testing.expectEqualSlices(AST.Diagnostic, &[_]AST.Diagnostic{}, parse_ast.errors) catch {
        std.debug.print("Tokens:\n{any}", .{result.tokens.tokens.items(.tag)});
        std.debug.panic("Test failed with parse errors", .{});
    };

    var fmt_result = std.ArrayList(u8).init(gpa);
    defer fmt_result.deinit();
    var formatter = Formatter.init(parse_ast, fmt_result.writer().any());
    formatter.flags = flags;
    _ = try formatter.formatExpr(expr);
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

    var parse_ast = parse.parse(&module_env, input);
    defer parse_ast.deinit();

    // Currently disabled cause SExpr are missing a lot of IR coverage resulting in panics.
    if (debug and false) {
        // shouldn't be required in future
        parse_ast.store.emptyScratch();

        std.debug.print("Parsed SExpr:\n==========\n", .{});
        parse_ast.toSExprStr(&module_env, std.io.getStdErr().writer().any()) catch @panic("Failed to print SExpr");
        std.debug.print("\n==========\n\n", .{});
    }

    std.testing.expectEqualSlices(AST.Diagnostic, &[_]AST.Diagnostic{}, parse_ast.errors) catch {
        return error.ParseFailed;
    };

    var result = std.ArrayList(u8).init(gpa);
    try formatAst(parse_ast, result.writer().any());

    if (debug) {
        std.debug.print("Formatted:\n==========\n{s}\n==========\n\n", .{result.items});
    }
    return result.toOwnedSlice() catch |err| exitOnOom(err);
}
