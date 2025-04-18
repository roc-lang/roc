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

/// Count of successfully formated files along with files that failed to format.
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

    var parse_ast = parse(&module_env, contents);
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

fn printParseErrors(gpa: std.mem.Allocator, source: []const u8, parse_ast: IR) !void {
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

/// Formats and writes out well-formed source of a Roc parse IR (AST).
/// Only returns an error if the underlying writer returns an error.
pub fn formatAst(ast: IR, writer: std.io.AnyWriter) !void {
    const trace = tracy.trace(@src());
    defer trace.end();

    var fmt = Formatter.init(ast, writer);

    var ignore_newline_for_first_statement = false;

    fmt.ast.store.emptyScratch();
    const file = fmt.ast.store.getFile();
    const header_region = fmt.ast.store.nodes.items.items(.region)[file.header.id];
    _ = try fmt.flushCommentsBefore(header_region.start);
    const maybe_output = try fmt.formatHeader(file.header);
    if (maybe_output == FormattedOutput.nothing_formatted) {
        ignore_newline_for_first_statement = true;
    }
    for (fmt.ast.store.statementSlice(file.statements)) |s| {
        const region = fmt.nodeRegion(s.id);
        _ = try fmt.flushCommentsBefore(region.start);
        _ = try fmt.formatStatement(s);
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

    /// Emits a string containing the well-formed source of a Roc parse IR (AST).
    /// The resulting string is owned by the caller.
    pub fn formatFile(fmt: *Formatter) ![]const u8 {
        fmt.ast.store.emptyScratch();
        const file = fmt.ast.store.getFile();
        const header_region = fmt.ast.store.nodes.items.items(.region)[file.header.id];
        _ = try fmt.flushCommentsBefore(header_region.start);
        try fmt.formatHeader(file.header);
        const statement_slice = fmt.ast.store.statementSlice(file.statements);
        for (statement_slice) |s| {
            const region = fmt.nodeRegion(s.id);
            _ = try fmt.flushCommentsBefore(region.start);
            _ = try fmt.formatStatement(s);
        }
        return fmt.buffer.toOwnedSlice(fmt.gpa) catch |err| exitOnOom(err);
    }

    fn formatStatement(fmt: *Formatter, si: StatementIdx) !NewlineBehavior {
        const statement = fmt.ast.store.getStatement(si);
        const node_region = fmt.nodeRegion(si.id);
        const multiline = fmt.ast.regionIsMultiline(node_region);
        var flushed = false;
        const orig_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = orig_indent;
        }
        switch (statement) {
            .decl => |d| {
                const pattern_region = fmt.nodeRegion(d.pattern.id);
                _ = try fmt.formatPattern(d.pattern);
                if (multiline and try fmt.flushCommentsAfter(pattern_region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                    try fmt.push('=');
                } else {
                    try fmt.pushAll(" = ");
                }
                const body_region = fmt.nodeRegion(d.body.id);
                if (multiline and try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                _ = try fmt.formatExpr(d.body);
                return .extra_newline_needed;
            },
            .expr => |e| {
                _ = try fmt.formatExpr(e.expr);
                return .extra_newline_needed;
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
                    const items_region = fmt.regionInSlice(IR.NodeStore.ExposedItemIdx, items);
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
                        var arg_region = fmt.nodeRegion(items[0].id);
                        for (items) |item| {
                            arg_region = fmt.nodeRegion(item.id);
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
                return .extra_newline_needed;
            },
            .type_decl => |d| {
                const header_region = fmt.nodeRegion(d.header.id);
                try fmt.formatTypeHeader(d.header);
                if (multiline and try fmt.flushCommentsAfter(header_region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.push(':');
                const anno_region = fmt.nodeRegion(d.anno.id);
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
                return .extra_newline_needed;
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
                const anno_region = fmt.nodeRegion(t.anno.id);
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
                return .no_extra_newline;
            },
            .expect => |e| {
                try fmt.pushAll("expect");
                const body_region = fmt.nodeRegion(e.body.id);
                if (multiline and try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(e.body);
                return .extra_newline_needed;
            },
            .crash => |c| {
                try fmt.pushAll("crash");
                const body_region = fmt.nodeRegion(c.expr.id);
                if (multiline and try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(c.expr);
                return .extra_newline_needed;
            },
            .@"return" => |r| {
                try fmt.pushAll("return");
                const body_region = fmt.nodeRegion(r.expr.id);
                if (multiline and try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(r.expr);
                return .extra_newline_needed;
            },
            .malformed => {
                // Output nothing for malformed node
                return .no_extra_newline;
            },
        }
    }

    fn formatWhereConstraint(fmt: *Formatter, w: IR.NodeStore.CollectionIdx) !void {
        const start_indent = fmt.curr_indent;
        defer fmt.curr_indent = start_indent;
        try fmt.pushAll("where");
        var i: usize = 0;
        const clause_coll = fmt.ast.store.getCollection(w);
        const clauses_multiline = fmt.ast.regionIsMultiline(clause_coll.region);
        const clause_slice = fmt.ast.store.whereClauseSlice(.{ .span = clause_coll.span });
        for (clause_slice) |clause| {
            const clause_region = fmt.nodeRegion(clause.id);
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

    fn formatIdent(fmt: *Formatter, ident: TokenIdx, qualifier: ?TokenIdx) !void {
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }
        if (qualifier) |q| {
            const multiline = fmt.ast.regionIsMultiline(IR.Region{ .start = q, .end = ident });
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

    fn formatCollection(fmt: *Formatter, region: IR.Region, braces: Braces, comptime T: type, items: []T, formatter: fn (*Formatter, T) anyerror!IR.Region) !void {
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
        for (items) |item| {
            const item_region = fmt.nodeRegion(item.id);
            if (multiline and try fmt.flushCommentsBefore(item_region.start)) {
                try fmt.ensureNewline();
                try fmt.pushIndent();
            } else if (multiline) {
                try fmt.newline();
                try fmt.pushIndent();
            }
            _ = try formatter(fmt, item);
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

    fn formatRecordField(fmt: *Formatter, idx: IR.NodeStore.RecordFieldIdx) !IR.Region {
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

    fn formatExpr(fmt: *Formatter, ei: ExprIdx) anyerror!IR.Region {
        return formatExprInner(fmt, ei, .normal);
    }

    fn formatExprInner(fmt: *Formatter, ei: ExprIdx, format_behavior: ExprFormatBehavior) anyerror!IR.Region {
        const expr = fmt.ast.store.getExpr(ei);
        const region = fmt.nodeRegion(ei.id);
        const multiline = fmt.ast.regionIsMultiline(region);
        const indent_modifier: u32 = if (format_behavior == .no_indent_on_access and fmt.curr_indent > 0) 1 else 0;
        const curr_indent: u32 = fmt.curr_indent - indent_modifier;
        defer {
            fmt.curr_indent = curr_indent;
        }
        switch (expr) {
            .apply => |a| {
                _ = try fmt.formatExpr(a.@"fn");
                try fmt.formatCollection(region, .round, IR.NodeStore.ExprIdx, fmt.ast.store.exprSlice(a.args), Formatter.formatExpr);
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
                            const part_region = fmt.nodeRegion(idx.id);
                            // Parts don't include the StringInterpolationStart and StringInterpolationEnd tokens
                            // That means they won't include any of the newlines between them and the actual expr.
                            // So we'll widen the region by one token for calculating multliline.
                            // Ideally, we'd also check if the expr itself is multiline, and if we will end up flushing, but
                            // we'll leave it as is for now
                            const part_is_multiline = fmt.ast.regionIsMultiline(IR.Region{ .start = part_region.start - 1, .end = part_region.end + 1 });
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
                const right_region = fmt.nodeRegion(fa.right.id);
                if (multiline and try fmt.flushCommentsBefore(right_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                try fmt.push('.');
                _ = try fmt.formatExprInner(fa.right, .no_indent_on_access);
            },
            .int => |i| {
                try fmt.pushTokenText(i.token);
            },
            .float => |f| {
                try fmt.pushTokenText(f.token);
            },
            .list => |l| {
                try fmt.formatCollection(region, .square, IR.NodeStore.ExprIdx, fmt.ast.store.exprSlice(l.items), Formatter.formatExpr);
            },
            .tuple => |t| {
                try fmt.formatCollection(region, .round, IR.NodeStore.ExprIdx, fmt.ast.store.exprSlice(t.items), Formatter.formatExpr);
            },
            .record => |r| {
                try fmt.formatCollection(region, .curly, IR.NodeStore.RecordFieldIdx, fmt.ast.store.recordFieldSlice(r.fields), Formatter.formatRecordField);
            },
            .lambda => |l| {
                const args = fmt.ast.store.patternSlice(l.args);
                const body_region = fmt.nodeRegion(l.body.id);
                const args_region = fmt.regionInSlice(IR.NodeStore.PatternIdx, args);
                const args_are_multiline = fmt.ast.regionIsMultiline(IR.Region{ .start = l.region.start, .end = args_region.end });
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
                const right_region = fmt.nodeRegion(op.right.id);
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
                const cond_region = fmt.nodeRegion(i.condition.id);
                var flushed = try fmt.flushCommentsBefore(cond_region.start);
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                _ = try fmt.formatExpr(i.condition);
                const then_region = fmt.nodeRegion(i.then.id);
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
                const else_region = fmt.nodeRegion(i.@"else".id);
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
                var branch_region = fmt.nodeRegion(branches[0].id);
                for (branches) |b| {
                    fmt.curr_indent = branch_indent;
                    branch_region = fmt.nodeRegion(b.id);
                    const branch = fmt.ast.store.getBranch(b);
                    _ = try fmt.flushCommentsBefore(branch_region.start);
                    try fmt.pushIndent();
                    const pattern_region = try fmt.formatPattern(branch.pattern);
                    var flushed = try fmt.flushCommentsAfter(pattern_region.end);
                    if (flushed) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                        try fmt.pushAll("->");
                    } else {
                        try fmt.pushAll(" ->");
                    }
                    const body_region = fmt.nodeRegion(branch.body.id);
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
                const expr_node = fmt.nodeRegion(d.expr.id);
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

    fn formatPatternRecordField(fmt: *Formatter, idx: IR.NodeStore.PatternRecordFieldIdx) !IR.Region {
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
                const v_region = fmt.nodeRegion(v.id);
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

    fn formatPattern(fmt: *Formatter, pi: PatternIdx) !IR.Region {
        const pattern = fmt.ast.store.getPattern(pi);
        var region = IR.Region{ .start = 0, .end = 0 };
        switch (pattern) {
            .ident => |i| {
                region = i.region;
                try fmt.formatIdent(i.ident_tok, null);
            },
            .tag => |t| {
                region = t.region;
                try fmt.formatIdent(t.tag_tok, null);
                if (t.args.span.len > 0) {
                    try fmt.formatCollection(region, .round, IR.NodeStore.PatternIdx, fmt.ast.store.patternSlice(t.args), Formatter.formatPattern);
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
                try fmt.formatCollection(region, .curly, IR.NodeStore.PatternRecordFieldIdx, fmt.ast.store.patternRecordFieldSlice(r.fields), Formatter.formatPatternRecordField);
            },
            .list => |l| {
                region = l.region;
                try fmt.formatCollection(region, .square, IR.NodeStore.PatternIdx, fmt.ast.store.patternSlice(l.patterns), Formatter.formatPattern);
            },
            .tuple => |t| {
                region = t.region;
                try fmt.formatCollection(region, .round, IR.NodeStore.PatternIdx, fmt.ast.store.patternSlice(t.patterns), Formatter.formatPattern);
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
                    const pattern_region = fmt.nodeRegion(p.id);
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
                        const next_region = fmt.nodeRegion(patterns[i + 1].id);
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

    fn formatExposedItem(fmt: *Formatter, idx: IR.NodeStore.ExposedItemIdx) !IR.Region {
        const item = fmt.ast.store.getExposedItem(idx);
        var region = IR.Region{ .start = 0, .end = 0 };
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

    fn formatHeader(fmt: *Formatter, hi: HeaderIdx) !FormattedOutput {
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
                    IR.NodeStore.ExposedItemIdx,
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

                var platform_field: ?IR.NodeStore.RecordFieldIdx = null;
                var package_fields_list = try std.ArrayListUnmanaged(IR.NodeStore.RecordFieldIdx).initCapacity(fmt.ast.store.gpa, 10);
                var i: usize = 0;
                const packages_slice = fmt.ast.store.recordFieldSlice(.{ .span = packages.span });
                for (packages_slice) |package| {
                    if (package.id == a.platform_idx.id) {
                        platform_field = package;
                        continue;
                    }
                    try package_fields_list.append(fmt.ast.store.gpa, package);
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
                    const item_region = fmt.nodeRegion(field_idx.id);
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

                return .something_formatted;
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
                    IR.NodeStore.ExposedItemIdx,
                    fmt.ast.store.exposedItemSlice(.{ .span = exposes.span }),
                    Formatter.formatExposedItem,
                );
                return .something_formatted;
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
                    IR.NodeStore.ExposedItemIdx,
                    fmt.ast.store.exposedItemSlice(.{ .span = exposes.span }),
                    Formatter.formatExposedItem,
                );
                return .something_formatted;
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
                    IR.NodeStore.ExposedItemIdx,
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
                    IR.NodeStore.RecordFieldIdx,
                    fmt.ast.store.recordFieldSlice(.{ .span = packages.span }),
                    Formatter.formatRecordField,
                );
                return .something_formatted;
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
                    IR.NodeStore.ExposedItemIdx,
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
                const signatures_region = fmt.nodeRegion(p.requires_signatures.id);
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
                    IR.NodeStore.ExposedItemIdx,
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
                    IR.NodeStore.RecordFieldIdx,
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
                    IR.NodeStore.ExposedItemIdx,
                    fmt.ast.store.exposedItemSlice(.{ .span = provides.span }),
                    Formatter.formatExposedItem,
                );
            },
            .malformed => {
                // Output nothing for a malformed node
                return .nothing_formatted;
            },
        }
        return .something_formatted;
    }

    fn nodeRegion(fmt: *Formatter, idx: u32) IR.Region {
        return fmt.ast.store.nodes.items.items(.region)[idx];
    }

    fn formatBody(fmt: *Formatter, body: IR.NodeStore.Body) !void {
        const multiline = fmt.ast.regionIsMultiline(body.region);
        if (multiline or body.statements.span.len > 1) {
            fmt.curr_indent += 1;
            try fmt.push('{');
            for (fmt.ast.store.statementSlice(body.statements)) |s| {
                const region = fmt.nodeRegion(s.id);
                _ = try fmt.flushCommentsBefore(region.start);
                try fmt.ensureNewline();
                try fmt.pushIndent();
                _ = try fmt.formatStatement(s);
            }
            _ = try fmt.flushCommentsBefore(body.region.end);
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
            try fmt.formatCollection(h.region, .round, IR.NodeStore.TypeAnnoIdx, fmt.ast.store.typeAnnoSlice(h.args), Formatter.formatTypeAnno);
        }
    }

    fn formatAnnoRecordField(fmt: *Formatter, idx: IR.NodeStore.AnnoRecordFieldIdx) !IR.Region {
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
        const anno_region = fmt.nodeRegion(field.ty.id);
        if (multiline and try fmt.flushCommentsBefore(anno_region.start)) {
            fmt.curr_indent += 1;
            try fmt.pushIndent();
        } else {
            try fmt.push(' ');
        }
        _ = try fmt.formatTypeAnno(field.ty);
        return field.region;
    }

    fn formatWhereClause(fmt: *Formatter, idx: IR.NodeStore.WhereClauseIdx) !void {
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
                    IR.NodeStore.TypeAnnoIdx,
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
                const ret_region = fmt.nodeRegion(c.ret_anno.id);
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
                    IR.NodeStore.TypeAnnoIdx,
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
                const ret_region = fmt.nodeRegion(c.ret_anno.id);
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

    fn formatTypeAnno(fmt: *Formatter, anno: IR.NodeStore.TypeAnnoIdx) !IR.Region {
        const a = fmt.ast.store.getTypeAnno(anno);
        var region = IR.Region{ .start = 0, .end = 0 };
        switch (a) {
            .apply => |app| {
                const slice = fmt.ast.store.typeAnnoSlice(app.args);
                const first = slice[0];
                _ = try fmt.formatTypeAnno(first);
                const rest = slice[1..];
                try fmt.formatCollection(app.region, .round, IR.NodeStore.TypeAnnoIdx, rest, Formatter.formatTypeAnno);
            },
            .ty_var => |v| {
                region = v.region;
                try fmt.pushTokenText(v.tok);
            },
            .ty => |t| {
                try fmt.pushTokenText(t.tok);
            },
            .mod_ty => |t| {
                try fmt.pushTokenText(t.tok);
                try fmt.pushAll(".");
                try fmt.pushTokenText(t.tok + 1);
            },
            .tuple => |t| {
                region = t.region;
                try fmt.formatCollection(t.region, .round, IR.NodeStore.TypeAnnoIdx, fmt.ast.store.typeAnnoSlice(t.annos), Formatter.formatTypeAnno);
            },
            .record => |r| {
                region = r.region;
                try fmt.formatCollection(region, .curly, IR.NodeStore.AnnoRecordFieldIdx, fmt.ast.store.annoRecordFieldSlice(r.fields), Formatter.formatAnnoRecordField);
            },
            .tag_union => |t| {
                region = t.region;
                try fmt.formatCollection(t.region, .square, IR.NodeStore.TypeAnnoIdx, fmt.ast.store.typeAnnoSlice(t.tags), Formatter.formatTypeAnno);
            },
            .@"fn" => |f| {
                region = f.region;
                const multiline = fmt.ast.regionIsMultiline(region);

                var i: usize = 0;
                const args = fmt.ast.store.typeAnnoSlice(f.args);
                for (args) |idx| {
                    const arg_region = fmt.nodeRegion(idx.id);
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
                const ret_region = fmt.nodeRegion(f.ret.id);
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

    fn flushCommentsBefore(fmt: *Formatter, tokenIdx: TokenIdx) !bool {
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

    fn flushCommentsAfter(fmt: *Formatter, tokenIdx: TokenIdx) !bool {
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
            .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent, .DotLowerIdent => {
                start += 1;
            },
            else => {},
        }

        const text = fmt.ast.source[start..region.end.offset];
        try fmt.pushAll(text);
    }

    fn regionInSlice(fmt: *Formatter, comptime T: anytype, slice: []T) IR.Region {
        if (slice.len == 0) {
            return IR.NodeStore.emptyRegion();
        }
        const first: usize = @intCast(slice[0].id);
        const last: usize = @intCast(slice[slice.len - 1].id);
        const first_region = fmt.ast.store.nodes.items.items(.region)[first];
        const last_region = fmt.ast.store.nodes.items.items(.region)[last];
        return first_region.spanAcross(last_region);
    }

    fn displayRegion(fmt: *Formatter, region: IR.Region) void {
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
        \\# Hello world!
        \\
        \\# Multiline comments?
        \\app [main!] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\import pf.Stdout
        \\
        \\main! = |_| {
        \\    world = "World"
        \\    # Hello
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
    try moduleFmtsSame(
        \\module [hello!, world]
        \\import pf.Stdout
        \\hello! = Stdout.line!("Hello")
        \\world = "World"
    );
}

test "Module - empty" {
    try moduleFmtsSame(
        \\module []
    );
}

test "Module - nonempty singleline" {
    try moduleFmtsSame(
        \\module [something, SomeType]
    );
}

test "Module - nonempty multiline" {
    try moduleFmtsSame(
        \\module [
        \\    something,
        \\    SomeType,
        \\]
    );

    try moduleFmtsSame(
        \\module [ # Comment After exposes open
        \\    something, # Comment after exposed item
        \\    SomeType, # Comment after final exposed item
        \\]
    );

    try moduleFmtsSame(
        \\module # Comment after module keyword
        \\    [ # Comment After exposes open
        \\        something, # Comment after exposed item
        \\        SomeType, # Comment after final exposed item
        \\    ]
    );

    try moduleFmtsTo(
        \\module [something, SomeType,]
    ,
        \\module [
        \\    something,
        \\    SomeType,
        \\]
    );
}

test "Hosted - empty" {
    try moduleFmtsSame(
        \\hosted []
    );
}

test "Hosted - nonempty singleline" {
    try moduleFmtsSame(
        \\hosted [something, SomeType]
    );
}

test "Hosted - nonempty multiline" {
    try moduleFmtsSame(
        \\hosted [
        \\    something,
        \\    SomeType,
        \\]
    );

    try moduleFmtsSame(
        \\hosted [ # Comment After exposes open
        \\    something, # Comment after exposed item
        \\    SomeType, # Comment after final exposed item
        \\]
    );

    try moduleFmtsSame(
        \\hosted # Comment after hosted keyword
        \\    [ # Comment After exposes open
        \\        something, # Comment after exposed item
        \\        SomeType, # Comment after final exposed item
        \\    ]
    );

    try moduleFmtsTo(
        \\hosted [something, SomeType,]
    ,
        \\hosted [
        \\    something,
        \\    SomeType,
        \\]
    );
}

test "Package Header - empty" {
    try moduleFmtsSame(
        \\package [] {}
    );
}

test "App Header - nonempty singleline" {
    try moduleFmtsSame(
        \\app [main!] { pf: platform "../main.roc", other: "../../other/main.roc" }
    );
}

test "App Header - nonempty multiline" {
    try moduleFmtsSame(
        \\app # This comment is here
        \\    [main!]
        \\    { pf: platform "../main.roc", somePkg: "../main.roc" }
    );
    try moduleFmtsTo(
        \\app
        \\    [main!,]
        \\    { pf: platform "../main.roc", somePkg: "../main.roc", }
    ,
        \\app
        \\    [
        \\        main!,
        \\    ]
        \\    {
        \\        pf: platform "../main.roc",
        \\        somePkg: "../main.roc",
        \\    }
    );
    try moduleFmtsSame(
        \\app # Comment after keyword
        \\    [ # Comment after provides open
        \\        main!, # Comment after exposed item
        \\    ]
        \\    { # Comment after packages open
        \\        pf: platform "../main.roc", # Comment after platform
        \\        other: "../../other/main.roc", # Comment after last package
        \\    }
    );
}

test "App Header - platform not first" {
    try moduleFmtsTo(
        \\app
        \\    [main!,]
        \\    { somePkg: "../main.roc", pf: platform "../main.roc", }
    ,
        \\app
        \\    [
        \\        main!,
        \\    ]
        \\    {
        \\        pf: platform "../main.roc",
        \\        somePkg: "../main.roc",
        \\    }
    );
}

test "App Header - provides singleline, packages multiline" {
    try moduleFmtsTo(
        \\app [main!] {
        \\    pf: platform "../main.roc",
        \\    somePkg: "../main.roc"
        \\}
    ,
        \\app [main!] {
        \\    pf: platform "../main.roc",
        \\    somePkg: "../main.roc",
        \\}
    );
    try moduleFmtsTo(
        \\app [main!] {
        \\    somePkg: "../main.roc",
        \\    pf: platform "../main.roc"
        \\}
    ,
        \\app [main!] {
        \\    pf: platform "../main.roc",
        \\    somePkg: "../main.roc",
        \\}
    );
    try moduleFmtsTo(
        \\app [main!] {somePkg: "../main.roc",pf: platform "../main.roc",}
    ,
        \\app [main!] {
        \\    pf: platform "../main.roc",
        \\    somePkg: "../main.roc",
        \\}
    );
}

test "Package Header - nonempty singleline" {
    try moduleFmtsSame(
        \\package [something, SomeType] { somePkg: "../main.roc", other: "../../other/main.roc" }
    );
}

test "Package Header - nonempty multiline" {
    try moduleFmtsSame(
        \\package # This comment is here
        \\    [something, SomeType]
        \\    { somePkg: "../main.roc" }
    );
    try moduleFmtsTo(
        \\package
        \\    [something, SomeType,]
        \\    { somePkg: "../main.roc", }
    ,
        \\package
        \\    [
        \\        something,
        \\        SomeType,
        \\    ]
        \\    {
        \\        somePkg: "../main.roc",
        \\    }
    );
    try moduleFmtsSame(
        \\package # Comment after keyword
        \\    [ # Comment after exposes open
        \\        something, # Comment after exposed item
        \\        SomeType, # Comment after last exposed item
        \\    ]
        \\    { # Comment after packages open
        \\        somePkg: "../main.roc", # Comment after package
        \\        other: "../../other/main.roc", # Comment after last package
        \\    }
    );
}

test "Platform header - empty" {
    try moduleFmtsSame(
        \\platform "foo"
        \\    requires {} {}
        \\    exposes []
        \\    packages {}
        \\    provides []
    );

    try moduleFmtsTo(
        \\platform "foo" requires {} {} exposes [] packages {} provides []
    ,
        \\platform "foo"
        \\    requires {} {}
        \\    exposes []
        \\    packages {}
        \\    provides []
    );
}

test "Platform header - nonempty" {
    try moduleFmtsSame(
        \\platform # Comment after platform keyword
        \\    "foo" # Comment after name
        \\    requires # Coment after requires keyword
        \\        { # Comment after rigids open
        \\            Main, # Comment after rigid member
        \\        } # Comment after rigids close
        \\            { # Comment after signatures open
        \\                main! : List(Str) => {}, # Comment after signature
        \\            } # Comment after signatures clsoe
        \\    exposes # Comment after exposes keyword
        \\        [ # Comment after exposes open
        \\            foo, # Comment after exposed item
        \\        ] # Comment after exposes close
        \\    packages # Comment after packages keyword
        \\        { # Comment after packages open
        \\            some_pkg: "../some_pkg.roc", # Comment after package
        \\        } # Comment after packages close
        \\    provides # Comment after provides keyword
        \\        [ # Comment after provides open
        \\            bar, # Comment after exposed item
        \\        ]
    );
}

test "Where clauses" {
    try moduleFmtsSame(
        \\module [Hash]
        \\
        \\Hash(a) : a
        \\    where
        \\        a.hash(hasher) -> hasher,
        \\        hasher.Hasher,
        \\
        \\Decode(a) : a
        \\    where
        \\        module(a).decode(List(U8)) -> a,
    );

    try moduleFmtsSame(
        \\module [decode]
        \\
        \\import Decode exposing [Decode]
        \\
        \\decodeThings : List(List(U8)) -> List(a)
        \\    where a.Decode
    );

    try moduleFmtsSame(
        \\module [Hash]
        \\
        \\Hash(a) # After header
        \\    : # After colon
        \\        a # After var
        \\            where # After where
        \\                a.hash(hasher) # After method
        \\                    -> # After arrow
        \\                        hasher, # After first clause
        \\                hasher.Hasher,
        \\
        \\Decode(a) : a
        \\    where
        \\        module(a).decode( # After method args open
        \\            List(U8), # After method arg
        \\        ) -> a,
    );

    try moduleFmtsSame(
        \\module [decode]
        \\
        \\import Decode exposing [Decode]
        \\
        \\decodeThings # After member name
        \\    : # After colon
        \\        List(List(U8)) -> List(a) # After anno
        \\            where # after where
        \\                a.Decode,
    );
}

test "Syntax grab bag" {
    try moduleFmtsSame(
        \\# This is a module comment!
        \\app [main!] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\import pf.Stdout exposing [line!, write!]
        \\
        \\import # Comment after import keyword
        \\    pf # Comment after qualifier
        \\        .StdoutMultiline # Comment after ident
        \\        exposing [ # Comment after exposing open
        \\            line!, # Comment after exposed item
        \\            write!, # Another after exposed item
        \\        ] # Comment after exposing close
        \\
        \\import pkg.Something exposing [func as function, Type as ValueCategory, Custom.*]
        \\
        \\import BadName as GoodName
        \\import
        \\    BadNameMultiline
        \\        as
        \\        GoodNameMultiline
        \\
        \\Map(a, b) : List(a), (a -> b) -> List(b)
        \\MapML( # Comment here
        \\    a, # And here
        \\    b,
        \\) # And after the last arg
        \\    : # And after the colon
        \\        List( # Inside Tag args
        \\            a, # After tag arg
        \\        ),
        \\        (a -> b) -> # After arrow
        \\            List( # Inside tag args
        \\                b,
        \\            ) # And after the type decl
        \\
        \\Foo : (Bar, Baz)
        \\
        \\FooMultiline : ( # Comment after pattern tuple open
        \\    Bar, # Comment after pattern tuple item
        \\    Baz, # Another after pattern tuple item
        \\) # Comment after pattern tuple close
        \\
        \\Some(a) : { foo : Ok(a), bar : Something }
        \\SomeMl(a) : { # After record open
        \\    foo : Ok(a), # After field
        \\    bar : Something, # After last field
        \\}
        \\
        \\SomeMultiline(a) : { # Comment after pattern record open
        \\    foo # After field name
        \\        : # Before field anno
        \\            Ok(a), # Comment after pattern record field
        \\    bar : Something, # Another after pattern record field
        \\} # Comment after pattern record close
        \\
        \\Maybe(a) : [Some(a), None]
        \\
        \\MaybeMultiline(a) : [ # Comment after tag union open
        \\    Some(a), # Comment after tag union member
        \\    None, # Another after tag union member
        \\] # Comment after tag union close
        \\
        \\SomeFunc(a) : Maybe(a), a -> Maybe(a)
        \\
        \\add_one_oneline = |num| if num 2 else 5
        \\
        \\add_one : U64 -> U64
        \\add_one = |num| {
        \\    other = 1
        \\    if num {
        \\        dbg # After debug
        \\            some_func() # After debug expr
        \\        0
        \\    } else {
        \\        dbg 123
        \\        other
        \\    }
        \\}
        \\
        \\match_time = |
        \\    a, # After arg
        \\    b,
        \\| # After args
        \\    match a {
        \\        Blue | Green | Red -> {
        \\            x = 12
        \\            x
        \\        }
        \\        Blue # After pattern in alt
        \\        | # Before pattern in alt
        \\            Green
        \\        | Red # After alt pattern
        \\            -> {
        \\                x = 12
        \\                x
        \\            }
        \\        lower # After pattern comment
        \\            -> 1
        \\        "foo" -> # After arrow comment
        \\            100
        \\        "foo" | "bar" -> 200
        \\        [1, 2, 3, .. as rest] # After pattern comment
        \\            -> # After arrow comment
        \\                123 # After branch comment
        \\
        \\        # Just a random comment
        \\
        \\        [1, 2 | 5, 3, .. as rest] -> 123
        \\        [
        \\            1,
        \\            2 | 5,
        \\            3,
        \\            .. # After DoubleDot
        \\                as # Before alias
        \\                    rest, # After last pattern in list
        \\        ] -> 123
        \\        3.14 -> 314
        \\        3.14 | 6.28 -> 314
        \\        (1, 2, 3) -> 123
        \\        (1, 2 | 5, 3) -> 123
        \\        { foo: 1, bar: 2, ..rest } -> 12
        \\        { # After pattern record open
        \\            foo # After pattern record field name
        \\                : # Before pattern record field value
        \\                    1, # After pattern record field
        \\            bar: 2,
        \\            .. # After spread operator
        \\                rest, # After last field
        \\        } -> 12
        \\        { foo: 1, bar: 2 | 7 } -> 12
        \\        {
        \\            foo: 1,
        \\            bar: 2 | 7, # After last record field
        \\        } -> 12
        \\        Ok(123) -> 123
        \\        Ok(Some(dude)) -> dude
        \\        TwoArgs("hello", Some("world")) -> 1000
        \\    }
        \\
        \\expect # Comment after expect keyword
        \\    blah == 1 # Comment after expect statement
        \\
        \\main! : List(String) -> Result({}, _)
        \\main! = |_| { # Yeah I can leave a comment here
        \\    world = "World"
        \\    number = 123
        \\    expect blah == 1
        \\    tag = Blue
        \\    return # Comment after return keyword
        \\        tag # Comment after return statement
        \\
        \\    # Just a random comment!
        \\
        \\    ...
        \\    match_time(
        \\        ..., # Single args with comment
        \\    )
        \\    some_func(
        \\        dbg # After debug
        \\            42, # After debug expr
        \\    )
        \\    crash # Comment after crash keyword
        \\        "Unreachable!" # Comment after crash statement
        \\    tag_with_payload = Ok(number)
        \\    interpolated = "Hello, ${world}"
        \\    list = [
        \\        add_one(
        \\            dbg # After dbg in list
        \\                number, # after dbg expr as arg
        \\        ), # Comment one
        \\        456, # Comment two
        \\        789, # Comment three
        \\    ]
        \\    record = { foo: 123, bar: "Hello", baz: tag, qux: Ok(world), punned }
        \\    tuple = (123, "World", tag, Ok(world), (nested, tuple), [1, 2, 3])
        \\    multiline_tuple = (
        \\        123,
        \\        "World",
        \\        tag1,
        \\        Ok(world), # This one has a comment
        \\        (nested, tuple),
        \\        [1, 2, 3],
        \\    )
        \\    bin_op_result = Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
        \\    static_dispatch_style = some_fn(arg1)?.static_dispatch_method()?.next_static_dispatch_method()?.record_field?
        \\    Stdout.line!(interpolated)?
        \\    Stdout.line!(
        \\        "How about ${ # Comment after string interpolation open
        \\            Num.toStr(number) # Comment after string interpolation expr
        \\        } as a string?",
        \\    )
        \\} # Comment after top-level decl
        \\
        \\expect {
        \\    foo = 1 # This should work too
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

test "Multiline BinOp" {
    const expr =
        \\1 # One
        \\    + # Plus
        \\
        \\    # A comment in between
        \\
        \\    2 # Two
        \\        * # Times
        \\        3
    ;
    try exprFmtsSame(expr, .no_debug);
}

test "Multiline list formatting" {
    const expr = "[1,2,3,]";
    const expected =
        \\[
        \\    1,
        \\    2,
        \\    3,
        \\]
    ;
    try exprFmtsTo(expr, expected, .no_debug);
    const expr2 =
        \\[1, 2, # Foo
        \\  3]
    ;
    const expected2 =
        \\[
        \\    1,
        \\    2, # Foo
        \\    3,
        \\]
    ;
    try exprFmtsTo(expr2, expected2, .no_debug);
    const expr3 = "[[1], [2], [3,4,], [5]]";
    const expected3 =
        \\[
        \\    [1],
        \\    [2],
        \\    [
        \\        3,
        \\        4,
        \\    ],
        \\    [5],
        \\]
    ;
    try exprFmtsTo(expr3, expected3, .no_debug);
    const expr4 =
        \\[ # Open
        \\    1, # First
        \\
        \\    # A comment in the middle
        \\
        \\    2, # Second
        \\    # This comment has no blanks around it
        \\    3, # Third
        \\]
    ;
    try exprFmtsSame(expr4, .no_debug);
    try exprFmtsSame(expected, .no_debug);
    try exprFmtsSame(expected2, .no_debug);
    try exprFmtsSame(expected3, .no_debug);
}

test "if_then_else" {
    try exprFmtsSame(
        \\if bool 1 else 2
    , .no_debug);
    try exprFmtsSame(
        \\if bool {
        \\    1
        \\} else 2
    , .no_debug);
    try exprFmtsSame(
        \\if bool { # Comment after then open
        \\    1 # Comment after expr
        \\} else 2
    , .no_debug);
    try exprFmtsSame(
        \\if bool {
        \\    1
        \\} else {
        \\    2
        \\}
    , .no_debug);
    try exprFmtsSame(
        \\if bool {
        \\    1
        \\} else { # Comment after else open
        \\    2
        \\}
    , .no_debug);
    try exprFmtsSame(
        \\if # Comment after if
        \\    bool
        \\        {
        \\            1
        \\        } else {
        \\            2
        \\        }
    , .no_debug);
    try exprFmtsSame(
        \\if # Comment after if
        \\    bool # Comment after cond
        \\        { # Comment after then open
        \\            1
        \\        } else {
        \\            2
        \\        }
    , .no_debug);
    try exprFmtsSame(
        \\if # Comment after if
        \\    bool # Comment after cond
        \\        { # Comment after then open
        \\            1
        \\        } # Comment after then close
        \\            else # Comment after else
        \\                { # Comment else open
        \\                    2
        \\                }
    , .no_debug);
}

test "string multiline formatting (due to templating, not multiline string literal)" {
    const expr =
        \\"This is a string with ${some_func(a, #This is a comment
        \\b)} lines of text due to the template parts"
    ;
    const expected =
        \\"This is a string with ${
        \\    some_func(
        \\        a, # This is a comment
        \\        b,
        \\    )
        \\} lines of text due to the template parts"
    ;
    try exprFmtsTo(expr, expected, .no_debug);
    try exprFmtsSame(expected, .no_debug);
}

test "record access multiline formatting" {
    const expr =
        \\some_fn(arg1)?
        \\    .static_dispatch_method()?
        \\    .next_static_dispatch_method()?
        \\    .record_field?
    ;
    try exprFmtsSame(expr, .no_debug);
    const exprWithComments =
        \\some_fn(arg1)? # Comment 1
        \\    .static_dispatch_method()? # Comment 2
        \\    .next_static_dispatch_method()? # Comment 3
        \\    .record_field?
    ;
    try exprFmtsSame(exprWithComments, .no_debug);
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
