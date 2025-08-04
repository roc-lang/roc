//! Formatting logic for Roc modules.

const std = @import("std");
const base = @import("base");
const parse = @import("parse");
const collections = @import("collections");
const compile = @import("compile");

const Filesystem = @import("fs/Filesystem.zig");
const tracy = @import("tracy");

const ModuleEnv = compile.ModuleEnv;
const Token = tokenize.Token;
const Parser = parse.Parser;
const AST = parse.AST;
const Node = parse.Node;
const NodeStore = parse.NodeStore;
const SafeList = collections.SafeList;

const tokenize = parse.tokenize;
const fatal = collections.utils.fatal;

const FormatFlags = enum {
    debug_binop,
    no_debug,
};

/// Report of the result of formatting Roc files including the count of successes, failures, and any files that need to be reformatted
pub const FormattingResult = struct {
    success: usize,
    failure: usize,
    /// Only relevant when using `roc format --check`
    unformatted_files: ?std.ArrayList([]const u8),

    pub fn deinit(self: *@This()) void {
        if (self.unformatted_files) |files| {
            files.deinit();
        }
    }
};

/// Formats all roc files in the specified path.
/// Handles both single files and directories
/// Returns the number of files successfully formatted and that failed to format.
pub fn formatPath(gpa: std.mem.Allocator, arena: std.mem.Allocator, base_dir: std.fs.Dir, path: []const u8, check: bool) !FormattingResult {
    // TODO: update this to use the filesystem abstraction
    // When doing so, add a mock filesystem and some tests.
    const stderr = std.io.getStdErr().writer();

    var success_count: usize = 0;
    var failed_count: usize = 0;
    // Only used for `roc format --check`. If we aren't doing check, don't bother allocating
    var unformatted_files = if (check) std.ArrayList([]const u8).init(gpa) else null;

    // First try as a directory.
    if (base_dir.openDir(path, .{ .iterate = true })) |const_dir| {
        var dir = const_dir;
        defer dir.close();
        // Walk is recursive.
        var walker = try dir.walk(arena);
        defer walker.deinit();
        while (try walker.next()) |entry| {
            if (entry.kind == .file) {
                if (formatFilePath(gpa, entry.dir, entry.basename, if (unformatted_files) |*to_reformat| to_reformat else null)) |_| {
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
        if (formatFilePath(gpa, base_dir, path, if (unformatted_files) |*to_reformat| to_reformat else null)) |_| {
            success_count += 1;
        } else |err| {
            if (err != error.NotRocFile) {
                try stderr.print("Failed to format {s}: {any}\n", .{ path, err });
                failed_count += 1;
            }
        }
    }

    return .{ .success = success_count, .failure = failed_count, .unformatted_files = unformatted_files };
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
pub fn formatFilePath(gpa: std.mem.Allocator, base_dir: std.fs.Dir, path: []const u8, unformatted_files: ?*std.ArrayList([]const u8)) !void {
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

    var module_env = try ModuleEnv.init(gpa, contents);
    defer module_env.deinit();

    var parse_ast: AST = try parse.parse(&module_env);
    defer parse_ast.deinit(gpa);

    // If there are any parsing problems, print them to stderr
    if (parse_ast.parse_diagnostics.items.len > 0) {
        parse_ast.toSExprStr(&module_env, std.io.getStdErr().writer().any()) catch @panic("Failed to print SExpr");
        try printParseErrors(gpa, module_env.source, parse_ast);
        return error.ParsingFailed;
    }

    // Check if the file is formatted without actually formatting it
    if (unformatted_files != null) {
        var formatted = std.ArrayList(u8).init(gpa);
        defer formatted.deinit();
        try formatAst(parse_ast, formatted.writer().any());
        if (!std.mem.eql(u8, formatted.items, module_env.source)) {
            try unformatted_files.?.append(path);
        }
    } else { // Otherwise actually format it
        const output_file = try base_dir.createFile(path, .{});
        defer output_file.close();

        try formatAst(parse_ast, output_file.writer().any());
    }
}

/// Format the contents of stdin and output the result to stdout
pub fn formatStdin(gpa: std.mem.Allocator) !void {
    const contents = try std.io.getStdIn().readToEndAlloc(gpa, Filesystem.max_file_size);
    // ModuleEnv takes ownership of contents
    var module_env = try ModuleEnv.init(gpa, contents);
    defer module_env.deinit();

    var parse_ast: AST = try parse.parse(&module_env);
    defer parse_ast.deinit(gpa);

    // If there are any parsing problems, print them to stderr
    if (parse_ast.parse_diagnostics.items.len > 0) {
        parse_ast.toSExprStr(&module_env, std.io.getStdErr().writer().any()) catch @panic("Failed to print SExpr");
        try printParseErrors(gpa, module_env.source, parse_ast);
        return error.ParsingFailed;
    }

    try formatAst(parse_ast, std.io.getStdOut().writer().any());
}

fn printParseErrors(gpa: std.mem.Allocator, source: []const u8, parse_ast: AST) !void {
    // compute offsets of each line, looping over bytes of the input
    var line_offsets = try SafeList(u32).initCapacity(gpa, 256);
    defer line_offsets.deinit(gpa);
    _ = try line_offsets.append(gpa, 0);
    for (source, 0..) |c, i| {
        if (c == '\n') {
            _ = try line_offsets.append(gpa, @intCast(i));
        }
    }

    const stderr = std.io.getStdErr().writer();
    try stderr.print("Errors:\n", .{});
    for (parse_ast.parse_diagnostics.items) |err| {
        const region = parse_ast.tokens.resolve(@intCast(err.region.start));
        const line = binarySearch(line_offsets.items.items, region.start.offset) orelse unreachable;
        const column = region.start.offset - line_offsets.items.items[line];
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
        for (statement_slice, 0..) |s, i| {
            const region = fmt.nodeRegion(@intFromEnum(s));
            _ = try fmt.flushCommentsBefore(region.start);
            try fmt.ensureNewline();
            try fmt.formatStatement(s);

            if (i == statement_slice.len - 1) {
                // Flush comments before EOF
                _ = try fmt.flushCommentsBefore(region.end);
            }
        }
    }

    fn formatStatement(fmt: *Formatter, si: AST.Statement.Idx) !void {
        const statement = fmt.ast.store.getStatement(si);
        const multiline = fmt.nodeWillBeMultiline(AST.Statement.Idx, si);
        const orig_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = orig_indent;
        }
        switch (statement) {
            .decl => |d| {
                const pattern_region = fmt.nodeRegion(@intFromEnum(d.pattern));
                _ = try fmt.formatPattern(d.pattern);
                if (multiline and try fmt.flushCommentsBefore(pattern_region.end)) {
                    fmt.curr_indent += 1;
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
                var flushed = false;
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
                if (multiline and (i.alias_tok != null or i.exposes.span.len > 0)) {
                    flushed = try fmt.flushCommentsAfter(i.module_name_tok);
                }

                if (i.alias_tok) |a| {
                    if (multiline) {
                        if (flushed) {
                            fmt.curr_indent += 1;
                            try fmt.pushIndent();
                            try fmt.pushAll("as");
                        } else {
                            try fmt.pushAll(" as");
                        }
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
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                        try fmt.pushAll("exposing ");
                    } else {
                        try fmt.pushAll(" exposing ");
                    }
                    const items = fmt.ast.store.exposedItemSlice(i.exposes);
                    const items_region = fmt.regionInSlice(AST.ExposedItem.Idx, items);
                    // This is a near copy of formatCollection because to make that function
                    // work correctly, the exposed items have to be in a new Node type that
                    // will have its own region.
                    // Include the open and close squares.
                    const items_multiline = fmt.ast.regionIsMultiline(.{ .start = items_region.start - 1, .end = items_region.end + 1 }) or
                        fmt.nodesWillBeMultiline(AST.ExposedItem.Idx, items);
                    const braces = Braces.square;
                    try fmt.push(braces.start());
                    if (items.len == 0) {
                        try fmt.push(braces.end());
                    } else {
                        if (items_multiline) {
                            fmt.curr_indent += 1;
                        }
                        for (items, 0..) |item, x| {
                            const arg_region = fmt.nodeRegion(@intFromEnum(item));
                            if (items_multiline) {
                                _ = try fmt.flushCommentsBefore(arg_region.start);
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            }
                            _ = try fmt.formatExposedItem(item);
                            if (items_multiline) {
                                try fmt.push(',');
                            } else if (x < (items.len - 1)) {
                                try fmt.pushAll(", ");
                            }
                        }
                        if (items_multiline) {
                            _ = try fmt.flushCommentsBefore(i.region.end - 1);
                            try fmt.ensureNewline();
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
                if (multiline and try fmt.flushCommentsBefore(header_region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                if (d.kind == .nominal) {
                    try fmt.pushAll(":=");
                } else {
                    try fmt.push(':');
                }
                const anno_region = fmt.nodeRegion(@intFromEnum(d.anno));
                if (multiline and try fmt.flushCommentsBefore(anno_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatTypeAnno(d.anno);
                if (d.where) |w| {
                    if (multiline) {
                        _ = try fmt.flushCommentsBefore(anno_region.end);
                        try fmt.ensureNewline();
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    }
                    try fmt.formatWhereConstraint(w, multiline);
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
                    if (multiline) {
                        _ = try fmt.flushCommentsBefore(anno_region.end);
                        try fmt.ensureNewline();
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    }
                    try fmt.formatWhereConstraint(w, multiline);
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
                if (multiline and try fmt.flushCommentsBefore(patt_region.end)) {
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
                if (multiline and try fmt.flushCommentsBefore(expr_region.end)) {
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
            .dbg => |d| {
                try fmt.pushAll("dbg");
                const body_region = fmt.nodeRegion(@intFromEnum(d.expr));
                if (multiline and try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(d.expr);
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

    fn formatWhereConstraint(fmt: *Formatter, w: AST.Collection.Idx, multiline: bool) !void {
        const start_indent = fmt.curr_indent;
        defer fmt.curr_indent = start_indent;
        const clause_coll = fmt.ast.store.getCollection(w);
        const clause_slice = fmt.ast.store.whereClauseSlice(.{ .span = clause_coll.span });
        const clauses_are_multiline = fmt.collectionWillBeMultiline(AST.WhereClause.Idx, w);

        if (!multiline) {
            try fmt.push(' ');
        }

        try fmt.pushAll("where");

        for (clause_slice, 0..) |clause, i| {
            if (clauses_are_multiline) {
                const clause_region = fmt.nodeRegion(@intFromEnum(clause));
                _ = try fmt.flushCommentsBefore(clause_region.start);
            }
            if (i == 0) {
                if (clauses_are_multiline) {
                    fmt.curr_indent += 1;
                } else {
                    try fmt.push(' ');
                }
            }
            if (clauses_are_multiline) {
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            try fmt.formatWhereClause(clause);
            if (i < clause_slice.len - 1) {
                if (clauses_are_multiline) {
                    try fmt.push(',');
                } else {
                    try fmt.pushAll(", ");
                }
            }
        }
    }

    fn formatIdent(fmt: *Formatter, ident: Token.Idx, qualifier: ?Token.Idx) !void {
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }
        if (qualifier) |q| {
            const multiline = fmt.ast.regionIsMultiline(AST.TokenizedRegion{ .start = q, .end = ident });
            try fmt.pushTokenText(q);
            if (multiline and try fmt.flushCommentsAfter(q)) {
                fmt.curr_indent += 1;
                try fmt.pushIndent();
            }
            const ident_tag = fmt.ast.tokens.tokens.items(.tag)[ident];
            if (ident_tag == .NoSpaceDotUpperIdent or ident_tag == .NoSpaceDotLowerIdent or ident_tag == .DotUpperIdent or ident_tag == .DotLowerIdent) {
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

    fn formatCollection(fmt: *Formatter, region: AST.TokenizedRegion, braces: Braces, comptime T: type, items: []T, formatter: fn (*Formatter, T) anyerror!AST.TokenizedRegion) !void {
        const multiline = fmt.ast.regionIsMultiline(region) or fmt.nodesWillBeMultiline(T, items);
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
        for (items, 0..) |item_idx, i| {
            const item_region = fmt.nodeRegion(@intFromEnum(item_idx));
            if (multiline) {
                _ = try fmt.flushCommentsBefore(item_region.start);
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            _ = try formatter(fmt, item_idx);
            if (multiline) {
                try fmt.push(',');
            } else if (i < (items.len - 1)) {
                try fmt.pushAll(", ");
            }
        }
        if (multiline) {
            _ = try fmt.flushCommentsBefore(region.end - 1);
            fmt.curr_indent -= 1;
            try fmt.ensureNewline();
            try fmt.pushIndent();
        } else if (braces == .curly) {
            try fmt.push(' ');
        }
        try fmt.push(braces.end());
    }

    fn formatRecordField(fmt: *Formatter, idx: AST.RecordField.Idx) !AST.TokenizedRegion {
        const field = fmt.ast.store.getRecordField(idx);
        try fmt.pushTokenText(field.name);
        if (field.value) |v| {
            try fmt.pushAll(": ");
            _ = try fmt.formatExpr(v);
        }

        return field.region;
    }

    const ExprFormatBehavior = enum {
        normal,
        no_indent_on_access,
    };

    fn formatExpr(fmt: *Formatter, ei: AST.Expr.Idx) anyerror!AST.TokenizedRegion {
        return formatExprInner(fmt, ei, .normal);
    }

    fn formatExprInner(fmt: *Formatter, ei: AST.Expr.Idx, format_behavior: ExprFormatBehavior) anyerror!AST.TokenizedRegion {
        const expr = fmt.ast.store.getExpr(ei);
        const region = fmt.nodeRegion(@intFromEnum(ei));
        const multiline = fmt.nodeWillBeMultiline(AST.Expr.Idx, ei);
        const indent_modifier: u32 = @intFromBool(format_behavior == .no_indent_on_access and fmt.curr_indent > 0);
        const curr_indent: u32 = fmt.curr_indent - indent_modifier;
        defer {
            fmt.curr_indent = curr_indent;
        }
        switch (expr) {
            .apply => |a| {
                _ = try fmt.formatExpr(a.@"fn");
                const fn_region = fmt.nodeRegion(@intFromEnum(a.@"fn"));
                const args_region = AST.TokenizedRegion{ .start = fn_region.end, .end = region.end };
                try fmt.formatCollection(args_region, .round, AST.Expr.Idx, fmt.ast.store.exprSlice(a.args), Formatter.formatExpr);
            },
            .string_part => |s| {
                try fmt.pushTokenText(s.token);
            },
            .string => |s| {
                try fmt.push('"');
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
                            const part_is_multiline = fmt.ast.regionIsMultiline(AST.TokenizedRegion{ .start = part_region.start - 1, .end = part_region.end + 1 }) or
                                fmt.nodeWillBeMultiline(AST.Expr.Idx, idx);

                            if (part_is_multiline) {
                                _ = try fmt.flushCommentsBefore(part_region.start);
                                try fmt.ensureNewline();
                                fmt.curr_indent += 1;
                                try fmt.pushIndent();
                            }
                            _ = try fmt.formatExpr(idx);
                            if (part_is_multiline) {
                                _ = try fmt.flushCommentsBefore(part_region.end);
                                try fmt.ensureNewline();
                                fmt.curr_indent -= 1;
                                try fmt.pushIndent();
                            }
                            try fmt.push('}');
                        },
                    }
                }
                try fmt.push('"');
            },
            .single_quote => |s| {
                try fmt.pushTokenText(s.token);
            },
            .ident => |i| {
                const qualifier_tokens = fmt.ast.store.tokenSlice(i.qualifiers);

                for (qualifier_tokens) |tok_idx| {
                    const tok = @as(Token.Idx, @intCast(tok_idx));
                    try fmt.pushTokenText(tok);
                    try fmt.push('.');
                }

                try fmt.pushTokenText(i.token);
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
            .frac => |f| {
                try fmt.pushTokenText(f.token);
            },
            .list => |l| {
                try fmt.formatCollection(region, .square, AST.Expr.Idx, fmt.ast.store.exprSlice(l.items), Formatter.formatExpr);
            },
            .tuple => |t| {
                try fmt.formatCollection(region, .round, AST.Expr.Idx, fmt.ast.store.exprSlice(t.items), Formatter.formatExpr);
            },
            .record => |r| {
                try fmt.push('{');

                const fields = fmt.ast.store.recordFieldSlice(r.fields);
                var has_extension = false;

                // Handle extension if present
                if (r.ext) |ext| {
                    if (multiline) {
                        _ = try fmt.flushCommentsAfter(r.region.start);
                        fmt.curr_indent += 1;
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    try fmt.pushAll("..");
                    _ = try fmt.formatExpr(ext);
                    has_extension = true;

                    try fmt.push(',');
                    if (multiline and fields.len > 0) {
                        try fmt.newline();
                        try fmt.pushIndent();
                    }
                }

                // Format fields
                if (multiline and !has_extension and fields.len > 0) {
                    _ = try fmt.flushCommentsAfter(r.region.start);
                    fmt.curr_indent += 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }

                for (fields, 0..) |field_idx, i| {
                    if (!multiline) {
                        try fmt.push(' ');
                    }
                    const field_region = try fmt.formatRecordField(field_idx);
                    if (multiline) {
                        try fmt.push(',');
                        _ = try fmt.flushCommentsAfter(field_region.end);
                        try fmt.ensureNewline();
                        if (i < fields.len - 1) {
                            try fmt.pushIndent();
                        }
                    } else if (i < fields.len - 1) {
                        try fmt.pushAll(",");
                    }
                }

                if (has_extension or fields.len > 0) {
                    if (multiline) {
                        fmt.curr_indent -= 1;
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                }
                try fmt.push('}');
            },
            .lambda => |l| {
                const args = fmt.ast.store.patternSlice(l.args);
                const body_region = fmt.nodeRegion(@intFromEnum(l.body));
                const args_region = fmt.regionInSlice(AST.Pattern.Idx, args);
                const args_are_multiline = args.len > 0 and
                    (fmt.ast.regionIsMultiline(.{ .start = args_region.start - 1, .end = args_region.end + 1 }) or
                        fmt.nodesWillBeMultiline(AST.Pattern.Idx, args));
                try fmt.push('|');
                if (args_are_multiline) {
                    fmt.curr_indent += 1;
                    _ = try fmt.flushCommentsAfter(l.region.start);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }
                for (args, 0..) |arg, i| {
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
                const qualifier_tokens = fmt.ast.store.tokenSlice(t.qualifiers);

                for (qualifier_tokens) |tok_idx| {
                    const tok = @as(Token.Idx, @intCast(tok_idx));
                    try fmt.pushTokenText(tok);
                    try fmt.push('.');
                }

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
                flushed = try fmt.flushCommentsBefore(then_region.end);
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
                const branches = fmt.ast.store.matchBranchSlice(m.branches);
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
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                    const pattern_region = try fmt.formatPattern(branch.pattern);
                    var flushed = try fmt.flushCommentsBefore(pattern_region.end);
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
                try fmt.formatBlock(b);
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

    fn formatPatternRecordField(fmt: *Formatter, idx: AST.PatternRecordField.Idx) !AST.TokenizedRegion {
        const field = fmt.ast.store.getPatternRecordField(idx);
        const multiline = fmt.nodeWillBeMultiline(AST.PatternRecordField.Idx, idx);
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

    fn formatPattern(fmt: *Formatter, pi: AST.Pattern.Idx) !AST.TokenizedRegion {
        const pattern = fmt.ast.store.getPattern(pi);
        var region = AST.TokenizedRegion{ .start = 0, .end = 0 };
        const multiline = fmt.nodeWillBeMultiline(AST.Pattern.Idx, pi);
        switch (pattern) {
            .ident => |i| {
                region = i.region;
                try fmt.formatIdent(i.ident_tok, null);
            },
            .tag => |t| {
                region = t.region;

                const qualifier_tokens = fmt.ast.store.tokenSlice(t.qualifiers);
                for (qualifier_tokens) |tok_idx| {
                    const tok = @as(Token.Idx, @intCast(tok_idx));
                    try fmt.pushTokenText(tok);
                    try fmt.push('.');
                }

                try fmt.pushTokenText(t.tag_tok);
                if (t.args.span.len > 0) {
                    try fmt.formatCollection(region, .round, AST.Pattern.Idx, fmt.ast.store.patternSlice(t.args), Formatter.formatPattern);
                }
            },
            .string => |s| {
                region = s.region;
                _ = try fmt.formatExpr(s.expr);
            },
            .single_quote => |sq| {
                region = sq.region;
                try fmt.formatIdent(sq.token, null);
            },
            .int => |n| {
                region = n.region;
                try fmt.formatIdent(n.number_tok, null);
            },
            .frac => |n| {
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
                const patterns = fmt.ast.store.patternSlice(a.patterns);
                for (patterns, 0..) |p, i| {
                    const pattern_region = fmt.nodeRegion(@intFromEnum(p));
                    _ = try fmt.formatPattern(p);
                    fmt.curr_indent = curr_indent;
                    if (i < a.patterns.span.len - 1) {
                        if (multiline) {
                            _ = try fmt.flushCommentsBefore(pattern_region.end);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        try fmt.push('|');
                        const next_region = fmt.nodeRegion(@intFromEnum(patterns[i + 1]));
                        if (multiline and try fmt.flushCommentsBefore(next_region.start)) {
                            fmt.curr_indent += 1;
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                    }
                }
            },
            .as => |a| {
                _ = try fmt.formatPattern(a.pattern);
                try fmt.pushAll(" as ");
                try fmt.pushTokenText(a.name);
            },
            .malformed => {
                // Output nothing for malformed node
            },
        }
        return region;
    }

    fn formatExposedItem(fmt: *Formatter, idx: AST.ExposedItem.Idx) !AST.TokenizedRegion {
        const item = fmt.ast.store.getExposedItem(idx);
        var region = AST.TokenizedRegion{ .start = 0, .end = 0 };
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
            .malformed => |m| {
                region = m.region;
                // Don't format malformed exposed items - they'll be reported as errors
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

        const multiline = fmt.nodeWillBeMultiline(AST.Header.Idx, hi);
        switch (header) {
            .app => |a| {
                const provides = fmt.ast.store.getCollection(a.provides);
                try fmt.pushAll("app");
                if (multiline and try fmt.flushCommentsAfter(a.region.start)) {
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

                if (multiline and try fmt.flushCommentsBefore(provides.region.end)) {
                    if (fmt.curr_indent == start_indent) {
                        fmt.curr_indent += 1;
                    }
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                const packages = fmt.ast.store.getCollection(a.packages);
                const packages_multiline = fmt.collectionWillBeMultiline(AST.RecordField.Idx, a.packages);
                try fmt.push('{');
                if (packages_multiline) {
                    fmt.curr_indent += 1;
                } else {
                    try fmt.push(' ');
                }

                var platform_field: ?AST.RecordField.Idx = null;
                var package_fields_list = try std.ArrayListUnmanaged(AST.RecordField.Idx).initCapacity(fmt.ast.store.gpa, 10);
                const packages_slice = fmt.ast.store.recordFieldSlice(.{ .span = packages.span });
                for (packages_slice) |package_idx| {
                    if (package_idx == a.platform_idx) {
                        platform_field = package_idx;
                        continue;
                    }
                    try package_fields_list.append(fmt.ast.store.gpa, package_idx);
                }
                const package_fields = try package_fields_list.toOwnedSlice(fmt.ast.store.gpa);
                defer fmt.ast.store.gpa.free(package_fields);

                if (platform_field) |field_idx| {
                    const field = fmt.ast.store.getRecordField(field_idx);
                    if (packages_multiline) {
                        _ = try fmt.flushCommentsBefore(field.region.start);
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
                    if (packages_multiline) {
                        try fmt.push(',');
                    } else if (package_fields.len > 0) {
                        try fmt.pushAll(", ");
                    }
                }
                for (package_fields, 0..) |field_idx, i| {
                    const item_region = fmt.nodeRegion(@intFromEnum(field_idx));
                    if (packages_multiline) {
                        _ = try fmt.flushCommentsBefore(item_region.start);
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                    _ = try fmt.formatRecordField(field_idx);
                    if (packages_multiline) {
                        try fmt.push(',');
                    } else if (i < package_fields.len - 1) {
                        try fmt.pushAll(", ");
                    }
                }
                if (packages_multiline) {
                    _ = try fmt.flushCommentsBefore(packages.region.end - 1);
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
                if (multiline) {
                    _ = try fmt.flushCommentsAfter(p.region.start);
                    try fmt.ensureNewline();
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                // TODO: This needs to be extended to the next CloseSquare
                const exposes = fmt.ast.store.getCollection(p.exposes);
                const exposesItems = fmt.ast.store.exposedItemSlice(.{ .span = exposes.span });
                try fmt.formatCollection(
                    exposes.region,
                    .square,
                    AST.ExposedItem.Idx,
                    exposesItems,
                    Formatter.formatExposedItem,
                );
                if (multiline) {
                    try fmt.newline();
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                const packages = fmt.ast.store.getCollection(p.packages);
                const packagesItems = fmt.ast.store.recordFieldSlice(.{ .span = packages.span });
                try fmt.formatCollection(
                    packages.region,
                    .curly,
                    AST.RecordField.Idx,
                    packagesItems,
                    Formatter.formatRecordField,
                );
            },
            .platform => |p| {
                try fmt.pushAll("platform");
                if (try fmt.flushCommentsAfter(p.region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.push('"');
                try fmt.pushTokenText(p.name);
                try fmt.push('"');

                _ = try fmt.flushCommentsAfter(p.name + 1);
                try fmt.ensureNewline();
                fmt.curr_indent = start_indent + 1;
                try fmt.pushIndent();

                try fmt.pushAll("requires");
                const rigids = fmt.ast.store.getCollection(p.requires_rigids);
                if (try fmt.flushCommentsBefore(rigids.region.start)) {
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
                if (try fmt.flushCommentsBefore(rigids.region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatTypeAnno(p.requires_signatures);

                const signatures_region = fmt.nodeRegion(@intFromEnum(p.requires_signatures));
                _ = try fmt.flushCommentsBefore(signatures_region.end);
                try fmt.ensureNewline();
                fmt.curr_indent = start_indent + 1;
                try fmt.pushIndent();

                try fmt.pushAll("exposes");
                const exposes = fmt.ast.store.getCollection(p.exposes);
                if (try fmt.flushCommentsBefore(exposes.region.start)) {
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

                _ = try fmt.flushCommentsBefore(exposes.region.end);
                try fmt.ensureNewline();
                fmt.curr_indent = start_indent + 1;
                try fmt.pushIndent();

                try fmt.pushAll("packages");
                const packages = fmt.ast.store.getCollection(p.packages);
                if (try fmt.flushCommentsBefore(packages.region.start)) {
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

                _ = try fmt.flushCommentsBefore(packages.region.end);
                try fmt.ensureNewline();
                fmt.curr_indent = start_indent + 1;
                try fmt.pushIndent();

                try fmt.pushAll("provides");
                const provides = fmt.ast.store.getCollection(p.provides);
                if (try fmt.flushCommentsBefore(provides.region.start)) {
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

    fn nodeRegion(fmt: *Formatter, idx: u32) AST.TokenizedRegion {
        return fmt.ast.store.nodes.items.items(.region)[idx];
    }

    fn formatBlock(fmt: *Formatter, block: AST.Block) !void {
        if (block.statements.span.len > 0) {
            fmt.curr_indent += 1;
            try fmt.push('{');
            for (fmt.ast.store.statementSlice(block.statements), 0..) |s, i| {
                const region = fmt.nodeRegion(@intFromEnum(s));
                _ = try fmt.flushCommentsBefore(region.start);
                try fmt.ensureNewline();
                try fmt.pushIndent();
                try fmt.formatStatement(s);

                if (i == block.statements.span.len - 1) {
                    _ = try fmt.flushCommentsBefore(region.end);
                }
            }
            try fmt.ensureNewline();
            fmt.curr_indent -= 1;
            try fmt.pushIndent();
            try fmt.push('}');
        } else {
            try fmt.pushAll("{}");
        }
    }

    fn formatTypeHeader(fmt: *Formatter, header: AST.TypeHeader.Idx) !void {
        // Check if the type header node is malformed before calling getTypeHeader
        const h = fmt.ast.store.getTypeHeader(header) catch {
            // Handle malformed type header by outputting placeholder text
            try fmt.pushAll("<malformed>");
            return;
        };

        try fmt.pushTokenText(h.name);
        if (h.args.span.len > 0) {
            try fmt.formatCollection(h.region, .round, AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(h.args), Formatter.formatTypeAnno);
        }
    }

    fn formatAnnoRecordField(fmt: *Formatter, idx: AST.AnnoRecordField.Idx) !AST.TokenizedRegion {
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }
        const field = fmt.ast.store.getAnnoRecordField(idx) catch |err| switch (err) {
            error.MalformedNode => {
                // Return empty region for malformed fields - they were already handled during parsing
                return AST.TokenizedRegion{ .start = 0, .end = 0 };
            },
        };
        const multiline = fmt.nodeWillBeMultiline(AST.AnnoRecordField.Idx, idx);
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

        const multiline = fmt.nodeWillBeMultiline(AST.WhereClause.Idx, idx);
        switch (clause) {
            .mod_method => |c| {
                try fmt.pushAll("module(");
                if (multiline and try fmt.flushCommentsBefore(c.var_tok)) {
                    fmt.curr_indent = start_indent + 1;
                    try fmt.pushIndent();
                }
                try fmt.pushTokenText(c.var_tok);
                if (multiline and try fmt.flushCommentsAfter(c.var_tok)) {
                    fmt.curr_indent = start_indent;
                    try fmt.pushIndent();
                }
                try fmt.push(')');
                try fmt.push('.');
                try fmt.pushTokenText(c.name_tok);
                try fmt.pushAll(" :");
                const args_coll = fmt.ast.store.getCollection(c.args);
                const ret_region = fmt.nodeRegion(@intFromEnum(c.ret_anno));

                fmt.curr_indent = start_indent;
                if (args_coll.span.len > 0) {
                    if (multiline and try fmt.flushCommentsBefore(args_coll.region.start)) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    const args = fmt.ast.store.typeAnnoSlice(.{ .span = args_coll.span });
                    // Format function arguments without parentheses (like regular function types)
                    for (args, 0..) |arg_idx, i| {
                        const arg_region = fmt.nodeRegion(@intFromEnum(arg_idx));
                        if (multiline and i > 0) {
                            _ = try fmt.flushCommentsBefore(arg_region.start);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        _ = try fmt.formatTypeAnno(arg_idx);
                        if (i < args.len - 1) {
                            if (multiline) {
                                try fmt.push(',');
                            } else {
                                try fmt.pushAll(", ");
                            }
                        } else {
                            if (multiline and try fmt.flushCommentsAfter(arg_region.end - 1)) {
                                fmt.curr_indent += 1;
                                try fmt.pushIndent();
                                try fmt.pushAll("->");
                            } else {
                                try fmt.pushAll(" ->");
                            }
                        }
                    }
                }
                if (multiline and try fmt.flushCommentsBefore(ret_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatTypeAnno(c.ret_anno);
            },
            .mod_alias => |c| {
                try fmt.pushAll("module(");
                if (multiline and try fmt.flushCommentsBefore(c.var_tok)) {
                    fmt.curr_indent = start_indent + 1;
                    try fmt.pushIndent();
                }
                try fmt.pushTokenText(c.var_tok);
                if (multiline and try fmt.flushCommentsAfter(c.var_tok)) {
                    fmt.curr_indent = start_indent;
                    try fmt.pushIndent();
                }
                try fmt.push(')');
                try fmt.push('.');
                try fmt.pushTokenText(c.name_tok);
            },
            .malformed => {
                // Output nothing for malformed node
            },
        }
    }

    fn formatTypeAnno(fmt: *Formatter, anno: AST.TypeAnno.Idx) !AST.TokenizedRegion {
        const a = fmt.ast.store.getTypeAnno(anno);
        var region = AST.TokenizedRegion{ .start = 0, .end = 0 };
        const multiline = fmt.nodeWillBeMultiline(AST.TypeAnno.Idx, anno);
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
            .underscore_type_var => |utv| {
                region = utv.region;
                try fmt.pushTokenText(utv.tok);
            },
            .ty => |t| {
                const qualifier_tokens = fmt.ast.store.tokenSlice(t.qualifiers);

                for (qualifier_tokens) |tok_idx| {
                    const tok = @as(Token.Idx, @intCast(tok_idx));
                    try fmt.pushTokenText(tok);
                    try fmt.push('.');
                }

                try fmt.pushTokenText(t.token);
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

                const args = fmt.ast.store.typeAnnoSlice(f.args);
                for (args, 0..) |idx, i| {
                    const arg_region = fmt.nodeRegion(@intFromEnum(idx));
                    if (multiline and i > 0) {
                        _ = try fmt.flushCommentsBefore(arg_region.start);
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                    _ = try fmt.formatTypeAnno(idx);
                    if (i < args.len - 1) {
                        if (multiline) {
                            try fmt.push(',');
                        } else {
                            try fmt.pushAll(", ");
                        }
                    }
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
                try fmt.push('(');
                if (multiline) {
                    _ = try fmt.flushCommentsAfter(region.start);
                    fmt.curr_indent += 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }
                const anno_region = try fmt.formatTypeAnno(p.anno);
                _ = try fmt.flushCommentsBefore(anno_region.end);
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
        const start = if (tokenIdx == 0) 0 else fmt.ast.tokens.resolve(tokenIdx - 1).end.offset;
        const end = fmt.ast.tokens.resolve(tokenIdx).start.offset;
        return fmt.flushComments(fmt.ast.env.source[start..end]);
    }

    fn flushCommentsAfter(fmt: *Formatter, tokenIdx: Token.Idx) !bool {
        const start = fmt.ast.tokens.resolve(tokenIdx).end.offset;
        const end = fmt.ast.tokens.resolve(tokenIdx + 1).start.offset;
        return fmt.flushComments(fmt.ast.env.source[start..end]);
    }

    fn flushComments(fmt: *Formatter, between_text: []const u8) !bool {
        var found_comment = false;
        var newline_count: usize = 0;
        var i: usize = 0;
        while (i < between_text.len) {
            if (between_text[i] == '#') {
                // Found a comment, extract it
                const comment_start = i + 1; // Skip the #
                var comment_end = comment_start;
                while (comment_end < between_text.len and between_text[comment_end] != '\n' and between_text[comment_end] != '\r') {
                    comment_end += 1;
                }

                if (comment_end > comment_start) {
                    if (found_comment or newline_count > 0) {
                        try fmt.pushIndent();
                    } else if (!fmt.has_newline) {
                        try fmt.pushAll(" ");
                    }
                    try fmt.push('#');
                    const comment_text = between_text[comment_start..comment_end];
                    if (comment_text.len > 0 and comment_text[0] != ' ') {
                        try fmt.push(' ');
                    }
                    try fmt.pushAll(comment_text);
                    found_comment = true;
                }
                try fmt.newline();
                newline_count += 1;
                i = comment_end;
                if (i < between_text.len and (between_text[i] == '\n' or between_text[i] == '\r')) {
                    i += 1; // Skip any additional newlines
                }
            } else if (between_text[i] == '\n') {
                try fmt.newline();
                newline_count += 1;
                i += 1;
            } else {
                i += 1;
            }
        }

        // Return true if there was a newline, whether or not there was a comment
        return newline_count > 0;
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
            .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent, .DotLowerIdent, .DotUpperIdent => {
                start += 1;
            },
            else => {},
        }

        const text = fmt.ast.env.source[start..region.end.offset];
        try fmt.pushAll(text);
    }

    fn regionInSlice(fmt: *Formatter, comptime T: anytype, slice: []T) AST.TokenizedRegion {
        if (slice.len == 0) {
            return AST.TokenizedRegion.empty();
        }
        const first: usize = @intFromEnum(slice[0]);
        const last: usize = @intFromEnum(slice[slice.len - 1]);
        const first_region = fmt.ast.store.nodes.items.items(.region)[first];
        const last_region = fmt.ast.store.nodes.items.items(.region)[last];
        return first_region.spanAcross(last_region);
    }

    fn displayRegion(fmt: *Formatter, region: AST.TokenizedRegion) void {
        const tags = fmt.ast.tokens.tokens.items(.tag);
        return std.debug.print("[{s}@{d}...{s}@{d}]\n", .{ @tagName(tags[region.start]), region.start, @tagName(tags[region.end - 1]), region.end - 1 });
    }

    fn nodeWillBeMultiline(fmt: *Formatter, comptime T: type, item: T) bool {
        switch (T) {
            AST.Expr.Idx => {
                const expr = fmt.ast.store.getExpr(item);
                if (fmt.ast.regionIsMultiline(expr.to_tokenized_region())) {
                    return true;
                }

                switch (expr) {
                    .block => return true,
                    .tuple => |t| {
                        return fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(t.items));
                    },
                    .apply => |a| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, a.@"fn")) {
                            return true;
                        }

                        return fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(a.args));
                    },
                    .bin_op => |b| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, b.left)) {
                            return true;
                        }

                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, b.right);
                    },
                    .record => |r| {
                        if (r.ext) |ext| {
                            if (fmt.nodeWillBeMultiline(AST.Expr.Idx, ext)) {
                                return true;
                            }
                        }

                        return fmt.nodesWillBeMultiline(AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(r.fields));
                    },
                    .suffix_single_question => |s| {
                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, s.expr);
                    },
                    .unary_op => |u| {
                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, u.expr);
                    },
                    else => return false,
                }
            },
            AST.Pattern.Idx => {
                const pattern = fmt.ast.store.getPattern(item);
                return fmt.ast.regionIsMultiline(pattern.to_tokenized_region());
            },
            AST.PatternRecordField.Idx => {
                const patternRecordField = fmt.ast.store.getPatternRecordField(item);
                if (fmt.ast.regionIsMultiline(patternRecordField.region)) {
                    return true;
                }

                if (patternRecordField.value) |value| {
                    if (fmt.nodeWillBeMultiline(AST.Pattern.Idx, value)) {
                        return true;
                    }
                }

                return false;
            },
            AST.ExposedItem.Idx => {
                const exposedItem = fmt.ast.store.getExposedItem(item);
                return fmt.ast.regionIsMultiline(exposedItem.to_tokenized_region());
            },
            AST.RecordField.Idx => {
                const recordField = fmt.ast.store.getRecordField(item);
                if (fmt.ast.regionIsMultiline(recordField.region)) {
                    return true;
                }

                if (recordField.value) |value| {
                    if (fmt.nodeWillBeMultiline(AST.Expr.Idx, value)) {
                        return true;
                    }
                }

                return false;
            },
            AST.TypeAnno.Idx => {
                const typeAnno = fmt.ast.store.getTypeAnno(item);
                return fmt.ast.regionIsMultiline(typeAnno.to_tokenized_region());
            },
            AST.AnnoRecordField.Idx => {
                const annoRecordField = fmt.ast.store.getAnnoRecordField(item) catch return false;
                if (fmt.ast.regionIsMultiline(annoRecordField.region)) {
                    return true;
                }

                return fmt.nodeWillBeMultiline(AST.TypeAnno.Idx, annoRecordField.ty);
            },
            AST.WhereClause.Idx => {
                const whereClause = fmt.ast.store.getWhereClause(item);
                return fmt.ast.regionIsMultiline(whereClause.to_tokenized_region());
            },
            AST.Statement.Idx => {
                const statement = fmt.ast.store.getStatement(item);
                if (fmt.ast.regionIsMultiline(statement.to_tokenized_region())) {
                    return true;
                }

                switch (statement) {
                    .expr => |e| {
                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, e.expr);
                    },
                    else => return false,
                }
            },
            AST.TypeHeader.Idx => {
                const typeHeader = fmt.ast.store.getTypeHeader(item) catch return false;
                if (fmt.ast.regionIsMultiline(typeHeader.region)) {
                    return true;
                }

                return fmt.nodesWillBeMultiline(AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(typeHeader.args));
            },
            AST.Header.Idx => {
                const header = fmt.ast.store.getHeader(item);
                if (fmt.ast.regionIsMultiline(header.to_tokenized_region())) {
                    return true;
                }

                switch (header) {
                    .package => |p| {
                        if (fmt.collectionWillBeMultiline(AST.ExposedItem.Idx, p.exposes)) {
                            return true;
                        }

                        return fmt.collectionWillBeMultiline(AST.RecordField.Idx, p.packages);
                    },
                    else => return false,
                }
            },
            else => return false,
        }
    }

    fn nodesWillBeMultiline(fmt: *Formatter, comptime T: type, items: []T) bool {
        for (items) |item| {
            if (fmt.nodeWillBeMultiline(T, item)) {
                return true;
            }
        }

        return false;
    }

    fn collectionWillBeMultiline(fmt: *Formatter, comptime T: type, idx: AST.Collection.Idx) bool {
        const collection = fmt.ast.store.getCollection(idx);
        if (fmt.ast.regionIsMultiline(collection.region)) {
            return true;
        }

        switch (T) {
            AST.RecordField.Idx => {
                const record_field_slice = fmt.ast.store.recordFieldSlice(.{ .span = collection.span });
                return fmt.nodesWillBeMultiline(AST.RecordField.Idx, record_field_slice);
            },
            else => return false,
        }
    }
};

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
    var module_env = try ModuleEnv.init(gpa, input);
    defer module_env.deinit();

    var parse_ast = try parse.parse(&module_env);
    defer parse_ast.deinit(gpa);

    // Currently disabled cause SExpr are missing a lot of IR coverage resulting in panics.
    if (debug and false) {
        // shouldn't be required in future
        parse_ast.store.emptyScratch();

        std.debug.print("Parsed SExpr:\n==========\n", .{});
        parse_ast.toSExprStr(module_env, std.io.getStdErr().writer().any()) catch @panic("Failed to print SExpr");
        std.debug.print("\n==========\n\n", .{});
    }

    std.testing.expectEqualSlices(AST.Diagnostic, &[_]AST.Diagnostic{}, parse_ast.parse_diagnostics.items) catch {
        return error.ParseFailed;
    };

    var result = std.ArrayList(u8).init(gpa);
    try formatAst(parse_ast, result.writer().any());

    if (debug) {
        std.debug.print("Formatted:\n==========\n{s}\n==========\n\n", .{result.items});
    }
    return try result.toOwnedSlice();
}
