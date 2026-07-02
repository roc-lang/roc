//! Formatting logic for Roc modules.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const parse = @import("parse");
const collections = @import("collections");
const can = @import("can");

const tracy = @import("tracy");

const ModuleEnv = can.ModuleEnv;
const Token = tokenize.Token;
const AST = parse.AST;
const SafeList = collections.SafeList;

const tokenize = parse.tokenize;

/// Errors that can occur while formatting an already-parsed AST.
pub const FormatAstError = Allocator.Error || std.Io.Writer.Error;
/// Errors that can occur while formatting a Roc source file.
pub const FormatFileError = Allocator.Error || std.Io.File.OpenError || std.Io.File.ReadPositionalError || FormatAstError || error{ NotRocFile, FileSizeChangedDuringRead, ReadFailed, ParsingFailed };
/// Errors that can occur while walking and formatting a path.
pub const FormatPathError = FormatFileError || std.Io.Dir.SelectiveWalker.Error;
/// Errors that can occur while formatting source read from stdin.
pub const FormatStdinError = Allocator.Error || FormatAstError || error{ ReadFailed, ParsingFailed };
/// Errors that can occur while parsing input for formatting.
pub const FormatParseError = Allocator.Error || FormatAstError || error{ParseFailed};
/// Errors that can occur in formatting tests.
pub const FormatTestError = FormatParseError || error{ SecondParseFailed, FormattingNotStable };

const FormatFlags = enum {
    debug_binop,
    no_debug,
};

/// Report of the result of formatting Roc files including the count of successes, failures, and any files that need to be reformatted
pub const FormattingResult = struct {
    success: usize,
    failure: usize,
    /// Only relevant when using `roc format --check`
    unformatted_files: ?std.array_list.Managed([]const u8),

    pub fn deinit(self: *@This()) void {
        if (self.unformatted_files) |files| {
            files.deinit();
        }
    }
};

/// Formats all roc files in the specified path.
/// Handles both single files and directories
/// Returns the number of files successfully formatted and that failed to format.
pub fn formatPath(gpa: std.mem.Allocator, arena: std.mem.Allocator, base_dir: std.Io.Dir, path: []const u8, check: bool, io: std.Io, stderr: *std.Io.Writer) FormatPathError!FormattingResult {
    // TODO: update this to use the filesystem abstraction
    // When doing so, add a mock filesystem and some tests.

    var success_count: usize = 0;
    var failed_count: usize = 0;
    // Only used for `roc format --check`. If we aren't doing check, don't bother allocating
    var unformatted_files = if (check) std.array_list.Managed([]const u8).init(gpa) else null;

    // First try as a directory.
    if (base_dir.openDir(io, path, .{ .iterate = true })) |const_dir| {
        var dir = const_dir;
        defer dir.close(io);
        // Walk is recursive.
        var walker = try dir.walk(arena);
        defer walker.deinit();
        while (try walker.next(io)) |entry| {
            if (entry.kind == .file) {
                if (formatFilePath(gpa, entry.dir, entry.basename, if (unformatted_files) |*to_reformat| to_reformat else null, io, stderr)) |_| {
                    success_count += 1;
                } else |err| switch (err) {
                    error.NotRocFile => {},
                    else => {
                        try stderr.print("Failed to format {s}: {any}\n", .{ entry.path, err });
                        failed_count += 1;
                    },
                }
            }
        }
    } else |_| {
        if (formatFilePath(gpa, base_dir, path, if (unformatted_files) |*to_reformat| to_reformat else null, io, stderr)) |_| {
            success_count += 1;
        } else |err| switch (err) {
            error.NotRocFile => {},
            else => {
                try stderr.print("Failed to format {s}: {any}\n", .{ path, err });
                failed_count += 1;
            },
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
pub fn formatFilePath(gpa: std.mem.Allocator, base_dir: std.Io.Dir, path: []const u8, unformatted_files: ?*std.array_list.Managed([]const u8), io: std.Io, stderr: *std.Io.Writer) FormatFileError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Skip non ".roc" files.
    if (!std.mem.eql(u8, std.fs.path.extension(path), ".roc")) {
        return error.NotRocFile;
    }

    const format_file_frame = tracy.namedFrame("format_file");
    defer format_file_frame.end();

    const input_file = try base_dir.openFile(io, path, .{ .mode = .read_only });
    defer input_file.close(io);

    const contents = blk: {
        const blk_trace = tracy.traceNamed(@src(), "readAllAlloc");
        defer blk_trace.end();

        if (input_file.stat(io)) |stat| {
            // Attempt to allocate exactly the right size first.
            // The avoids needless reallocs and saves some perf.
            const size = stat.size;
            const buf = try gpa.alloc(u8, @intCast(size));
            errdefer gpa.free(buf);
            if (try input_file.readPositionalAll(io, buf, 0) != size) {
                // This is unexpected, the file is smaller than the size from stat.
                // It must have been modified inplace.
                // TODO: handle this more gracefully.
                return error.FileSizeChangedDuringRead;
            }
            break :blk buf;
        } else |_| {
            // Fallback: read using a streaming reader.
            var read_buf: [4096]u8 = undefined;
            var file_reader = input_file.readerStreaming(io, &read_buf);
            var contents_list = std.ArrayList(u8).empty;
            errdefer contents_list.deinit(gpa);
            while (true) {
                const n = file_reader.interface.readSliceShort(contents_list.addManyAsSlice(gpa, 4096) catch return error.OutOfMemory) catch |err| switch (err) {
                    error.ReadFailed => return error.ReadFailed,
                };
                contents_list.shrinkRetainingCapacity(contents_list.items.len - 4096 + n);
                if (n < 4096) break;
            }
            break :blk try contents_list.toOwnedSlice(gpa);
        }
    };
    defer gpa.free(contents);

    var module_env = try ModuleEnv.init(gpa, contents);
    defer module_env.deinit();

    const parse_ast = try parse.file(gpa, &module_env.common);
    defer parse_ast.deinit();

    // If there are any parsing problems, print them to stderr
    if (parse_ast.parse_diagnostics.items.len > 0) {
        try parse_ast.toSExprStr(gpa, &module_env.common, stderr);
        try printParseErrors(gpa, module_env.common.source, parse_ast.*, stderr);
        return error.ParsingFailed;
    }

    // Check if the file is formatted without actually formatting it
    if (unformatted_files != null) {
        var formatted: std.Io.Writer.Allocating = .init(gpa);
        defer formatted.deinit();
        try formatAst(parse_ast.*, &formatted.writer);
        if (!std.mem.eql(u8, formatted.written(), module_env.common.source)) {
            try unformatted_files.?.append(path);
        }
    } else { // Otherwise actually format it
        const output_file = try base_dir.createFile(io, path, .{});
        defer output_file.close(io);
        var output_buffer: [4096]u8 = undefined;
        var output_writer = output_file.writer(io, &output_buffer);
        try formatAst(parse_ast.*, &output_writer.interface);
    }
}

/// Format the contents of stdin and output the result to stdout
pub fn formatStdin(gpa: std.mem.Allocator, io: std.Io, stdin: std.Io.File, stdout: std.Io.File, stderr: *std.Io.Writer) FormatStdinError!void {
    const contents = blk: {
        var read_buf: [4096]u8 = undefined;
        var stdin_reader = stdin.readerStreaming(io, &read_buf);
        var contents_list = std.ArrayList(u8).empty;
        errdefer contents_list.deinit(gpa);
        while (true) {
            const n = stdin_reader.interface.readSliceShort(contents_list.addManyAsSlice(gpa, 4096) catch return error.OutOfMemory) catch |err| switch (err) {
                error.ReadFailed => return error.ReadFailed,
            };
            contents_list.shrinkRetainingCapacity(contents_list.items.len - 4096 + n);
            if (n < 4096) break;
        }
        break :blk try contents_list.toOwnedSlice(gpa);
    };
    defer gpa.free(contents);

    // ModuleEnv retains a reference to contents for diagnostics
    var module_env = try ModuleEnv.init(gpa, contents);
    defer module_env.deinit();

    const parse_ast = try parse.file(gpa, &module_env.common);
    defer parse_ast.deinit();

    // If there are any parsing problems, print them to stderr
    if (parse_ast.parse_diagnostics.items.len > 0) {
        try parse_ast.toSExprStr(gpa, &module_env.common, stderr);
        try printParseErrors(gpa, module_env.common.source, parse_ast.*, stderr);
        return error.ParsingFailed;
    }

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = stdout.writer(io, &stdout_buffer);
    try formatAst(parse_ast.*, &stdout_writer.interface);
}

fn printParseErrors(gpa: std.mem.Allocator, source: []const u8, parse_ast: AST, stderr: *std.Io.Writer) (Allocator.Error || error{WriteFailed})!void {
    // compute offsets of each line, looping over bytes of the input
    var line_offsets = try SafeList(u32).initCapacity(gpa, 256);
    defer line_offsets.deinit(gpa);
    {
        const expected_idx = line_offsets.items.items.len;
        const idx = try line_offsets.append(gpa, 0);
        if (comptime builtin.mode == .Debug) {
            std.debug.assert(@intFromEnum(idx) == expected_idx);
        } else if (@intFromEnum(idx) != expected_idx) {
            unreachable;
        }
    }
    for (source, 0..) |c, i| {
        if (c == '\n') {
            const expected_idx = line_offsets.items.items.len;
            const idx = try line_offsets.append(gpa, @intCast(i));
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(@intFromEnum(idx) == expected_idx);
            } else if (@intFromEnum(idx) != expected_idx) {
                unreachable;
            }
        }
    }

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

fn formatIRNode(ast: AST, writer: *std.Io.Writer, formatter: *const fn (*Formatter) FormatAstError!void) FormatAstError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    var fmt = Formatter.init(ast, writer);

    try formatter(&fmt);
    try fmt.flush();
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is a file.
/// Only returns an error if the underlying writer returns an error.
pub fn formatAst(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatIRNode(ast, writer, Formatter.formatFile);
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is a header.
/// Only returns an error if the underlying writer returns an error.
pub fn formatHeader(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatIRNode(ast, writer, formatHeaderInner);
}

fn formatHeaderInner(fmt: *Formatter) FormatAstError!void {
    return fmt.formatHeader(@enumFromInt(fmt.ast.root_node_idx));
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is a statement.
/// Only returns an error if the underlying writer returns an error.
pub fn formatStatement(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatIRNode(ast, writer, formatStatementInner);
}

fn formatStatementInner(fmt: *Formatter) FormatAstError!void {
    return fmt.formatStatement(@enumFromInt(fmt.ast.root_node_idx));
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is an expression.
/// Only returns an error if the underlying writer returns an error.
pub fn formatExpr(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatIRNode(ast, writer, formatExprNode);
}

fn formatExprNode(fmt: *Formatter) FormatAstError!void {
    try fmt.formatExprDiscard(@enumFromInt(fmt.ast.root_node_idx));
}

/// Formatter for the roc parse ast.
const Formatter = struct {
    ast: AST,
    writer: *std.Io.Writer,
    curr_indent: u32 = 0,
    flags: FormatFlags = .no_debug,
    // This starts true since beginning of file is considered a newline.
    has_newline: bool = true,
    has_multiline_string: bool = false,

    /// Creates a new Formatter for the given parse IR.
    fn init(ast: AST, writer: *std.Io.Writer) Formatter {
        return .{
            .ast = ast,
            .writer = writer,
        };
    }

    /// Deinits all data owned by the formatter object.
    fn flush(fmt: *Formatter) error{WriteFailed}!void {
        try fmt.writer.flush();
    }

    /// Emits a string containing the well-formed source of a Roc parse IR (AST).
    /// The resulting string is owned by the caller.
    pub fn formatFile(fmt: *Formatter) FormatAstError!void {
        fmt.ast.store.emptyScratch();
        const file = fmt.ast.store.getFile();
        const header = fmt.ast.store.getHeader(file.header);
        const header_region = fmt.ast.store.nodes.items.items(.region)[@intFromEnum(file.header)];
        // Only flush comments before the header if it has its own tokens.
        // type_module, default_app, and malformed headers share the first statement's token,
        // so flushing here would duplicate the whitespace handling.
        const header_has_own_tokens = switch (header) {
            .type_module, .default_app, .malformed => false,
            else => true,
        };
        if (header_has_own_tokens) {
            try fmt.flushCommentsBeforeDiscard(header_region.start);
        }
        try fmt.formatHeader(file.header);
        const statement_slice = fmt.ast.store.statementSlice(file.statements);
        var prev_def_info: ?DefInfo = null;
        for (statement_slice) |s| {
            const region = fmt.nodeRegion(@intFromEnum(s));
            const curr_def_info = fmt.defInfo(s);
            // Insert a blank line between two consecutive top-level defs unless
            // the current decl is paired with the previous type_anno of the same name.
            const min_newlines: u8 = if (prev_def_info != null and curr_def_info != null and !isPairedAnnoDecl(prev_def_info.?, curr_def_info.?))
                2
            else
                0;
            _ = try fmt.flushCommentsBeforeMin(region.start, min_newlines);
            try fmt.ensureNewline();
            try fmt.formatStatement(s);
            prev_def_info = curr_def_info;
        }
        try fmt.flushCommentsEOF();
    }

    /// Information about a top-level def, used to decide whether to insert a blank line.
    const DefInfo = struct {
        kind: enum { type_anno, decl, type_decl },
        /// Identifier name for `type_anno` or `decl` with an ident pattern, used
        /// to detect anno+decl pairs that should stay grouped together.
        name: ?[]const u8,
    };

    /// Returns def info for statements considered "defs" at file scope, or null
    /// for statements that should not participate in def-separation logic.
    fn defInfo(fmt: *const Formatter, si: AST.Statement.Idx) ?DefInfo {
        const stmt = fmt.ast.store.getStatement(si);
        return switch (stmt) {
            .type_anno => |t| DefInfo{
                .kind = .type_anno,
                .name = fmt.ast.resolve(t.name),
            },
            .decl => |d| blk: {
                const pattern = fmt.ast.store.getPattern(d.pattern);
                const name: ?[]const u8 = switch (pattern) {
                    .ident => |p| fmt.ast.resolve(p.ident_tok),
                    else => null,
                };
                break :blk DefInfo{ .kind = .decl, .name = name };
            },
            .type_decl => DefInfo{ .kind = .type_decl, .name = null },
            else => null,
        };
    }

    fn isPairedAnnoDecl(prev: DefInfo, curr: DefInfo) bool {
        if (prev.kind != .type_anno or curr.kind != .decl) return false;
        const prev_name = prev.name orelse return false;
        const curr_name = curr.name orelse return false;
        return std.mem.eql(u8, prev_name, curr_name);
    }

    fn formatStatement(fmt: *Formatter, si: AST.Statement.Idx) FormatAstError!void {
        const statement = fmt.ast.store.getStatement(si);
        const multiline = fmt.nodeWillBeMultiline(AST.Statement.Idx, si);
        const orig_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = orig_indent;
        }
        switch (statement) {
            .decl => |d| {
                const pattern_region = fmt.nodeRegion(@intFromEnum(d.pattern));
                try fmt.formatPatternDiscard(d.pattern);
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
                try fmt.formatExprDiscard(d.body);
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
                if (v.body) |body| {
                    if (multiline and try fmt.flushCommentsAfter(v.name)) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    try fmt.push('=');
                    const body_region = fmt.nodeRegion(@intFromEnum(body));
                    if (multiline and try fmt.flushCommentsBefore(body_region.start)) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    try fmt.formatExprDiscard(body);
                }
            },
            .expr => |e| {
                try fmt.formatExprDiscard(e.expr);
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
                const path_result = try fmt.formatModulePath(i.module_name_tok, i.qualifier_tok, i.exposes);
                const last_module_tok = path_result.last_tok;
                if (multiline and (i.alias_tok != null or i.exposes.span.len > 0)) {
                    flushed = try fmt.flushCommentsAfter(last_module_tok);
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
                        // Only preserve newlines between `as` and the alias if there
                        // is an actual comment there. A bare source newline like
                        // `as\n    X1` should normalize to ` as X1`; otherwise we
                        // strand the alias on its own line and (with auto-expose)
                        // glue it directly to `exposing` (see issue #9373).
                        if (fmt.hasCommentBefore(a)) {
                            flushed = try fmt.flushCommentsBefore(a);
                            if (!flushed) {
                                try fmt.push(' ');
                            } else {
                                try fmt.pushIndent();
                            }
                        } else {
                            try fmt.push(' ');
                            flushed = false;
                        }
                    } else {
                        try fmt.pushAll(" as ");
                    }
                    try fmt.pushTokenText(a);
                    if (i.exposes.span.len > 0) {
                        flushed = try fmt.flushCommentsAfter(a);
                    }
                }
                // Output exposing clause if there are exposed items, OR if there was unusual
                // spacing (DotUpperIdent) in the module path - in the latter case, we need
                // to output "exposing []" to prevent auto-expose on re-format.
                const needs_exposing = i.exposes.span.len > 0 or path_result.has_unusual_spacing;
                if (needs_exposing) {
                    if (flushed) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                        try fmt.pushAll("exposing ");
                    } else {
                        try fmt.pushAll(" exposing ");
                    }
                    const items = fmt.ast.store.exposedItemSlice(i.exposes);
                    const braces = Braces.square;
                    try fmt.push(braces.start());
                    if (items.len == 0) {
                        // Empty exposing list - just output []
                        try fmt.push(braces.end());
                    } else {
                        const items_region = fmt.regionInSlice(AST.ExposedItem.Idx, items);
                        // This is a near copy of formatCollection because to make that function
                        // work correctly, the exposed items have to be in a new Node type that
                        // will have its own region.
                        // Include the open and close squares.
                        const items_multiline = fmt.ast.regionIsMultiline(.{ .start = items_region.start - 1, .end = items_region.end + 1 }) or
                            fmt.nodesWillBeMultiline(AST.ExposedItem.Idx, items);
                        if (items_multiline) {
                            fmt.curr_indent += 1;
                        }
                        for (items, 0..) |item, x| {
                            const arg_region = fmt.nodeRegion(@intFromEnum(item));
                            if (items_multiline) {
                                try fmt.flushCommentsBeforeDiscard(arg_region.start);
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            }
                            Formatter.discardRegion(try fmt.formatExposedItem(item));
                            if (items_multiline) {
                                try fmt.push(',');
                            } else if (x < (items.len - 1)) {
                                try fmt.pushAll(", ");
                            }
                        }
                        if (items_multiline) {
                            try fmt.flushCommentsBeforeDiscard(i.region.end - 1);
                            try fmt.ensureNewline();
                            fmt.curr_indent -= 1;
                            try fmt.pushIndent();
                        }
                        try fmt.push(braces.end());
                    }
                }
            },
            .file_import => |fi| {
                try fmt.pushAll("import ");
                try fmt.push('"');
                try fmt.pushTokenText(fi.path_tok);
                try fmt.push('"');
                try fmt.pushAll(" as ");
                try fmt.pushTokenText(fi.name_tok);
                try fmt.pushAll(" : ");
                if (fi.is_bytes) {
                    try fmt.pushAll("List(U8)");
                } else {
                    try fmt.pushAll("Str");
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
                switch (d.kind) {
                    .nominal => try fmt.pushAll(":="),
                    .@"opaque" => try fmt.pushAll("::"),
                    .alias => try fmt.push(':'),
                }
                const anno_region = fmt.nodeRegion(@intFromEnum(d.anno));
                if (multiline and try fmt.flushCommentsBefore(anno_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatTypeAnnoDiscard(d.anno);
                if (d.where) |w| {
                    if (multiline) {
                        try fmt.flushCommentsBeforeDiscard(anno_region.end);
                        try fmt.ensureNewline();
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    }
                    try fmt.formatWhereConstraint(w, multiline);
                }
                if (d.associated) |assoc| {
                    try fmt.pushAll(".");
                    try fmt.push('{');
                    if (assoc.statements.span.len > 0) {
                        fmt.curr_indent += 1;
                        const statements = fmt.ast.store.statementSlice(assoc.statements);
                        for (statements) |stmt_idx| {
                            const stmt_region = fmt.nodeRegion(@intFromEnum(stmt_idx));
                            try fmt.flushCommentsBeforeDiscard(stmt_region.start);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                            try fmt.formatStatement(stmt_idx);
                        }
                        // Flush any trailing comments before the closing brace
                        try fmt.flushCommentsBeforeDiscard(assoc.region.end - 1);
                        try fmt.ensureNewline();
                        fmt.curr_indent -= 1;
                        try fmt.pushIndent();
                    }
                    try fmt.push('}');
                }
            },
            .type_anno => |t| {
                if (t.is_var) {
                    try fmt.pushAll("var ");
                }
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
                try fmt.formatTypeAnnoDiscard(t.anno);
                if (t.where) |w| {
                    if (multiline) {
                        try fmt.flushCommentsBeforeDiscard(anno_region.end);
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
                try fmt.formatExprDiscard(e.body);
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
                try fmt.formatPatternDiscard(f.patt);
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
                try fmt.formatExprDiscard(f.expr);
                if (multiline and try fmt.flushCommentsBefore(expr_region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(f.body);
            },
            .@"while" => |w| {
                try fmt.pushAll("while");
                const cond_region = fmt.nodeRegion(@intFromEnum(w.cond));
                if (multiline and try fmt.flushCommentsBefore(cond_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(w.cond);
                if (multiline and try fmt.flushCommentsBefore(cond_region.end)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(w.body);
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
                try fmt.formatExprDiscard(c.expr);
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
                try fmt.formatExprDiscard(d.expr);
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
                try fmt.formatExprDiscard(r.expr);
            },
            .@"break" => {
                try fmt.pushAll("break");
            },
            .malformed => {
                // Output nothing for malformed node
            },
        }
    }

    fn formatWhereConstraint(fmt: *Formatter, w: AST.Collection.Idx, multiline: bool) FormatAstError!void {
        const start_indent = fmt.curr_indent;
        defer fmt.curr_indent = start_indent;
        const clause_coll = fmt.ast.store.getCollection(w);
        const clause_slice = fmt.ast.store.whereClauseSlice(.{ .span = clause_coll.span });
        const clauses_are_multiline = fmt.collectionWillBeMultiline(AST.WhereClause.Idx, w);

        if (!multiline) {
            try fmt.push(' ');
        }

        try fmt.pushAll("where");

        // Add opening bracket
        if (clauses_are_multiline) {
            try fmt.pushAll(" [");
            fmt.curr_indent += 1;
        } else {
            try fmt.pushAll(" [");
        }

        for (clause_slice, 0..) |clause, i| {
            if (clauses_are_multiline) {
                const clause_region = fmt.nodeRegion(@intFromEnum(clause));
                try fmt.flushCommentsBeforeDiscard(clause_region.start);
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            if (i > 0) {
                if (!clauses_are_multiline) {
                    try fmt.pushAll(", ");
                }
            }
            try fmt.formatWhereClause(clause);
            if (clauses_are_multiline) {
                try fmt.push(',');
            }
        }

        if (clauses_are_multiline) {
            try fmt.ensureNewline();
            fmt.curr_indent -= 1;
            try fmt.pushIndent();
        }
        try fmt.push(']');
    }

    fn formatIdent(fmt: *Formatter, ident: Token.Idx, qualifier: ?Token.Idx) (Allocator.Error || error{WriteFailed})!void {
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }
        if (qualifier) |q| {
            const multiline = fmt.ast.regionIsMultiline(AST.TokenizedRegion{ .start = q, .end = ident + 1 });
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

    /// Formats a module path for an import statement.
    /// For auto-expose imports (like `import A.B.C` becoming `import A.B exposing [C]`),
    /// module_name_tok points to the second-to-last token.
    /// For explicit clause imports (like `import A.B.C as D`), module_name_tok points to
    /// the first token and we iterate through consecutive uppercase tokens.
    const ModulePathResult = struct {
        last_tok: Token.Idx,
        has_unusual_spacing: bool, // True if DotUpperIdent (space before dot) was encountered
    };

    fn formatModulePath(fmt: *Formatter, module_name_tok: Token.Idx, qualifier: ?Token.Idx, exposes: AST.ExposedItem.Span) (Allocator.Error || error{WriteFailed})!ModulePathResult {
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }

        var has_unusual_spacing = false;

        // Get the first exposed token if any (for auto-expose detection)
        var first_exposed_tok: ?Token.Idx = null;
        if (exposes.span.len > 0) {
            const exposed_slice = fmt.ast.store.exposedItemSlice(exposes);
            if (exposed_slice.len > 0) {
                const first_exposed = fmt.ast.store.getExposedItem(exposed_slice[0]);
                first_exposed_tok = switch (first_exposed) {
                    .lower_ident => |i| i.ident,
                    .upper_ident => |i| i.ident,
                    .upper_ident_star => |i| i.ident,
                    .malformed => null,
                };
            }
        }

        // Output qualifier if present
        if (qualifier) |q| {
            try fmt.pushTokenText(q);
            try fmt.push('.');
        }

        // Output the first uppercase token
        try fmt.pushTokenText(module_name_tok);
        var last_tok = module_name_tok;

        // Iterate through consecutive uppercase tokens in the module path.
        // For auto-expose, stop before the exposed token (which is part of the path).
        // For explicit exposes, the exposed token is in the [] list, not the path, so we iterate fully.
        // DotUpperIdent (space before dot) is formatted with a newline to preserve the structure
        // and make malformed code visually obvious.
        var tok = module_name_tok + 1;
        const tags = fmt.ast.tokens.tokens.items(.tag);
        while (tok < tags.len) {
            const tag = tags[tok];
            if (tag != .NoSpaceDotUpperIdent and tag != .DotUpperIdent) {
                break;
            }
            // For auto-expose, stop before the exposed token
            if (first_exposed_tok) |exp_tok| {
                if (tok == exp_tok) break;
            }
            // DotUpperIdent has space before the dot - format with newline to preserve structure
            if (tag == .DotUpperIdent) {
                has_unusual_spacing = true;
                try fmt.ensureNewline();
                fmt.curr_indent += 1;
                try fmt.pushIndent();
                fmt.curr_indent -= 1;
            }
            try fmt.push('.');
            try fmt.pushTokenText(tok);
            last_tok = tok;
            tok += 1;
        }

        return .{ .last_tok = last_tok, .has_unusual_spacing = has_unusual_spacing };
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

    fn formatCollection(fmt: *Formatter, region: AST.TokenizedRegion, braces: Braces, comptime T: type, items: []T, formatter: fn (*Formatter, T) FormatAstError!AST.TokenizedRegion) FormatAstError!void {
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
                try fmt.flushCommentsBeforeDiscard(item_region.start);
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            const formatted_region = try formatter(fmt, item_idx);
            Formatter.discardRegion(formatted_region);
            if (multiline) {
                if (fmt.has_multiline_string) {
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }
                try fmt.push(',');
            } else if (i < (items.len - 1)) {
                try fmt.pushAll(", ");
            }
        }
        if (multiline) {
            try fmt.flushCommentsBeforeDiscard(region.end - 1);
            fmt.curr_indent -= 1;
            try fmt.ensureNewline();
            try fmt.pushIndent();
        } else if (braces == .curly) {
            try fmt.push(' ');
        }
        try fmt.push(braces.end());
    }

    /// Format a record type annotation with an extension (e.g., { name: Str, ..ext } or { name: Str, .. })
    fn formatRecordWithExtension(fmt: *Formatter, fields_span: AST.AnnoRecordField.Span, ext: AST.TypeAnno.RecordExt, record_region: AST.TokenizedRegion) FormatAstError!void {
        const fields = fmt.ast.store.annoRecordFieldSlice(fields_span);
        const record_multiline = fmt.ast.regionIsMultiline(record_region);
        const record_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = record_indent;
        }
        try fmt.push('{');
        if (record_multiline) {
            fmt.curr_indent += 1;
        } else {
            try fmt.push(' ');
        }
        if (fields.len > 0) {
            for (fields, 0..) |field_idx, i| {
                const field_region = fmt.nodeRegion(@intFromEnum(field_idx));
                if (record_multiline) {
                    try fmt.flushCommentsBeforeDiscard(field_region.start);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }
                const formatted_field_region = try @as(fn (*Formatter, AST.AnnoRecordField.Idx) FormatAstError!AST.TokenizedRegion, Formatter.formatAnnoRecordField)(fmt, field_idx);
                Formatter.discardRegion(formatted_field_region);
                if (record_multiline) {
                    try fmt.push(',');
                } else if (i < (fields.len - 1)) {
                    try fmt.pushAll(", ");
                } else {
                    // Last field before extension
                    try fmt.pushAll(", ");
                }
            }
        }
        // Handle the record extension (..ext or ..)
        switch (ext) {
            .named => |named| {
                if (record_multiline) {
                    try fmt.flushCommentsBeforeDiscard(named.region.start);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }
                try fmt.pushAll("..");
                try fmt.formatTypeAnnoDiscard(named.anno);
            },
            .open => |tok| {
                if (record_multiline) {
                    try fmt.flushCommentsBeforeDiscard(tok);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }
                try fmt.pushAll("..");
            },
            .closed => unreachable,
        }
        if (record_multiline) {
            try fmt.push(',');
            try fmt.flushCommentsBeforeDiscard(record_region.end - 1);
            fmt.curr_indent -= 1;
            try fmt.ensureNewline();
            try fmt.pushIndent();
        } else {
            try fmt.push(' ');
        }
        try fmt.push('}');
    }

    fn formatRecordField(fmt: *Formatter, idx: AST.RecordField.Idx) FormatAstError!AST.TokenizedRegion {
        const field = fmt.ast.store.getRecordField(idx);
        try fmt.pushTokenText(field.name);
        if (field.value) |v| {
            try fmt.pushAll(": ");
            try fmt.formatExprDiscard(v);
        }

        return field.region;
    }

    const ExprFormatBehavior = enum {
        normal,
        no_indent_on_access,
    };

    fn formatStringInterpolation(fmt: *Formatter, idx: AST.Expr.Idx) FormatAstError!void {
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
            try fmt.flushCommentsBeforeDiscard(part_region.start);
            try fmt.ensureNewline();
            fmt.curr_indent += 1;
            try fmt.pushIndent();
        }
        try fmt.formatExprDiscard(idx);
        if (part_is_multiline) {
            try fmt.flushCommentsBeforeDiscard(part_region.end);
            try fmt.ensureNewline();
            fmt.curr_indent -= 1;
            try fmt.pushIndent();
        }
        try fmt.push('}');
    }

    fn formatPatternString(fmt: *Formatter, str: anytype) FormatAstError!void {
        try fmt.push('"');
        for (fmt.ast.store.patternStringPartSlice(str.parts)) |part_idx| {
            switch (fmt.ast.store.getPatternStringPart(part_idx)) {
                .text => |text| try fmt.pushTokenText(text.token),
                .capture => |capture| {
                    try fmt.pushAll("${");
                    if (capture.name) |name| {
                        try fmt.pushTokenText(name);
                    } else {
                        try fmt.push('_');
                    }
                    try fmt.push('}');
                },
            }
        }
        try fmt.push('"');
    }

    const FormattedExpr = struct {
        region: AST.TokenizedRegion,
        ends_with_multiline_string_line: bool = false,
    };

    fn formatExprWithInfo(fmt: *Formatter, ei: AST.Expr.Idx) FormatAstError!FormattedExpr {
        return formatExprInner(fmt, ei, .normal);
    }

    fn formatExpr(fmt: *Formatter, ei: AST.Expr.Idx) FormatAstError!AST.TokenizedRegion {
        return (try fmt.formatExprWithInfo(ei)).region;
    }

    fn discardRegion(region: AST.TokenizedRegion) void {
        if (comptime builtin.mode == .Debug) {
            std.debug.assert(region.start <= region.end);
        } else if (region.start > region.end) {
            unreachable;
        }
    }

    fn formatExprDiscard(fmt: *Formatter, ei: AST.Expr.Idx) FormatAstError!void {
        const formatted = try fmt.formatExprWithInfo(ei);
        Formatter.discardRegion(formatted.region);
    }

    fn formatExprInnerDiscard(fmt: *Formatter, ei: AST.Expr.Idx, format_behavior: ExprFormatBehavior) FormatAstError!void {
        const formatted = try fmt.formatExprInner(ei, format_behavior);
        Formatter.discardRegion(formatted.region);
    }

    fn formatPatternDiscard(fmt: *Formatter, pi: AST.Pattern.Idx) FormatAstError!void {
        const region = try fmt.formatPattern(pi);
        Formatter.discardRegion(region);
    }

    fn formatTypeAnnoDiscard(fmt: *Formatter, anno: AST.TypeAnno.Idx) FormatAstError!void {
        const region = try fmt.formatTypeAnno(anno);
        Formatter.discardRegion(region);
    }

    fn flushCommentsBeforeDiscard(fmt: *Formatter, tokenIdx: Token.Idx) error{WriteFailed}!void {
        const flushed = try fmt.flushCommentsBefore(tokenIdx);
        if (flushed) {
            return;
        }
    }

    fn flushCommentsAfterDiscard(fmt: *Formatter, tokenIdx: Token.Idx) error{WriteFailed}!void {
        const flushed = try fmt.flushCommentsAfter(tokenIdx);
        if (flushed) {
            return;
        }
    }

    fn continueAfterMultilineStringLine(fmt: *Formatter, formatted: FormattedExpr) error{WriteFailed}!bool {
        if (!formatted.ends_with_multiline_string_line) {
            return false;
        }

        fmt.curr_indent += 1;
        try fmt.ensureNewline();
        try fmt.pushIndent();
        return true;
    }

    fn formatExprInner(fmt: *Formatter, ei: AST.Expr.Idx, format_behavior: ExprFormatBehavior) FormatAstError!FormattedExpr {
        const expr = fmt.ast.store.getExpr(ei);
        const region = fmt.nodeRegion(@intFromEnum(ei));
        var formatted = FormattedExpr{ .region = region };
        const multiline = fmt.nodeWillBeMultiline(AST.Expr.Idx, ei);
        const indent_modifier: u32 = @intFromBool(format_behavior == .no_indent_on_access and fmt.curr_indent > 0);
        const curr_indent: u32 = fmt.curr_indent - indent_modifier;
        defer {
            fmt.curr_indent = curr_indent;
        }
        switch (expr) {
            .apply => |a| {
                try fmt.formatExprDiscard(a.@"fn");
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
                        else => try fmt.formatStringInterpolation(idx),
                    }
                }
                try fmt.push('"');
            },
            .typed_string => |s| {
                try fmt.push('"');
                for (fmt.ast.store.exprSlice(s.parts)) |idx| {
                    const e = fmt.ast.store.getExpr(idx);
                    switch (e) {
                        .string_part => |str| {
                            try fmt.pushTokenText(str.token);
                        },
                        else => try fmt.formatStringInterpolation(idx),
                    }
                }
                try fmt.push('"');
                try fmt.push('.');
                try fmt.pushAll(fmt.ast.env.getIdent(s.type_ident));
            },
            .multiline_string => |s| {
                if (!fmt.has_newline) {
                    fmt.curr_indent += 1;
                }
                var add_newline = false;
                try fmt.pushAll("\\\\");
                for (fmt.ast.store.exprSlice(s.parts)) |idx| {
                    const e = fmt.ast.store.getExpr(idx);
                    switch (e) {
                        .string_part => |str| {
                            if (add_newline) {
                                // Comments could be located before the MultilineStringStart token, not the StringPart token
                                try fmt.flushCommentsBeforeDiscard(str.region.start - 1);
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                                try fmt.pushAll("\\\\");
                            }

                            add_newline = true;
                            try fmt.pushTokenText(str.token);
                        },
                        else => {
                            add_newline = false;
                            try fmt.formatStringInterpolation(idx);
                        },
                    }
                }
                fmt.has_multiline_string = true;
                formatted.ends_with_multiline_string_line = true;
            },
            .typed_multiline_string => |s| {
                if (!fmt.has_newline) {
                    fmt.curr_indent += 1;
                }
                var add_newline = false;
                try fmt.pushAll("\\\\");
                for (fmt.ast.store.exprSlice(s.parts)) |idx| {
                    const e = fmt.ast.store.getExpr(idx);
                    switch (e) {
                        .string_part => |str| {
                            if (add_newline) {
                                // Comments could be located before the MultilineStringStart token, not the StringPart token
                                try fmt.flushCommentsBeforeDiscard(str.region.start - 1);
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                                try fmt.pushAll("\\\\");
                            }

                            add_newline = true;
                            try fmt.pushTokenText(str.token);
                        },
                        else => {
                            add_newline = false;
                            try fmt.formatStringInterpolation(idx);
                        },
                    }
                }
                // The type suffix lives on its own line after the string body.
                try fmt.ensureNewline();
                try fmt.pushIndent();
                try fmt.push('.');
                try fmt.pushAll(fmt.ast.env.getIdent(s.type_ident));
                fmt.has_multiline_string = true;
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
                // Check if left side is an arrow_call with a plain ident or tag
                // e.g., `0->M .c` should format as multiline to avoid ambiguity with qualified ident
                const left_expr = fmt.ast.store.getExpr(fa.left);
                const needs_newline_before_dot = if (left_expr == .arrow_call) blk: {
                    const ld = left_expr.arrow_call;
                    const ld_right = fmt.ast.store.getExpr(ld.right);
                    break :blk ld_right == .ident or ld_right == .tag;
                } else false;

                const left = try fmt.formatExprWithInfo(fa.left);
                const right_region = fmt.nodeRegion(@intFromEnum(fa.right));
                if (needs_newline_before_dot) {
                    // Force newline to disambiguate from qualified identifier
                    // `0->M .c` becomes `0->M\n\t.c` not `0->M.c` (which parses differently)
                    fmt.curr_indent += 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                } else {
                    const continued = try fmt.continueAfterMultilineStringLine(left);
                    if (!continued and multiline and try fmt.flushCommentsBefore(right_region.start)) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    }
                }
                try fmt.push('.');
                try fmt.formatExprInnerDiscard(fa.right, .no_indent_on_access);
            },
            .method_call => |mc| {
                // Check if left side is an arrow_call with a plain ident or tag
                // e.g., `0->M .c()` should format as multiline to avoid ambiguity with qualified ident
                const left_expr = fmt.ast.store.getExpr(mc.receiver);
                const needs_newline_before_dot = if (left_expr == .arrow_call) blk: {
                    const ld = left_expr.arrow_call;
                    const ld_right = fmt.ast.store.getExpr(ld.right);
                    break :blk ld_right == .ident or ld_right == .tag;
                } else false;

                const receiver = try fmt.formatExprWithInfo(mc.receiver);
                if (needs_newline_before_dot) {
                    // Force newline to disambiguate from qualified identifier.
                    fmt.curr_indent += 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                } else {
                    const continued = try fmt.continueAfterMultilineStringLine(receiver);
                    if (!continued and multiline and try fmt.flushCommentsBefore(mc.method_token)) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    }
                }
                try fmt.push('.');
                try fmt.pushTokenText(mc.method_token);
                // Only the argument list (from the method token onwards) should
                // determine whether the call is multiline. Using the full
                // `mc.region` would include newlines from the receiver chain and
                // wrongly expand short, inline arguments. (See issue #9646)
                const args_region = AST.TokenizedRegion{ .start = mc.method_token + 1, .end = mc.region.end };
                try fmt.formatCollection(args_region, .round, AST.Expr.Idx, fmt.ast.store.exprSlice(mc.args), Formatter.formatExpr);
            },
            .arrow_call => |ld| {
                const left = try fmt.formatExprWithInfo(ld.left);
                if (multiline and try fmt.flushCommentsBefore(ld.operator)) {
                    if (format_behavior == .normal) {
                        fmt.curr_indent += 1;
                    }
                    try fmt.pushIndent();
                } else {
                    _ = try fmt.continueAfterMultilineStringLine(left);
                }
                try fmt.pushAll("->");
                if (multiline and try fmt.flushCommentsAfter(ld.operator)) {
                    try fmt.pushIndent();
                }
                // Always format with parens after `->` for consistency and idempotence.
                // Without parens, `0->b.c` would parse `b.c` as a qualified identifier,
                // but `0->b().c` unambiguously parses as field access on `0->b()`.
                // (See issue #8851)
                const right_expr = fmt.ast.store.getExpr(ld.right);
                if (right_expr == .ident) {
                    // Plain identifier: add () after it
                    try fmt.formatExprInnerDiscard(ld.right, .no_indent_on_access);
                    try fmt.pushAll("()");
                } else if (right_expr == .tag) {
                    // Tag: format normally
                    try fmt.formatExprInnerDiscard(ld.right, .no_indent_on_access);
                } else if (right_expr == .apply) {
                    // The arrow parser strips the outer parens around the fn part
                    // of an `apply` (because `->(...)` consumes the parens directly
                    // rather than producing a tuple), so e.g. `10->(|x| x + 1)()`
                    // parses to apply{fn=lambda, args=()}. Re-add those parens when
                    // the fn would otherwise be ambiguous (see issue #9372).
                    const apply = right_expr.apply;
                    const apply_fn_idx = apply.@"fn";
                    const apply_fn = fmt.ast.store.getExpr(apply_fn_idx);
                    const fn_needs_parens = switch (apply_fn) {
                        .ident, .tag, .apply, .tuple, .field_access, .tuple_access, .method_call, .list, .record, .string, .multiline_string, .string_part, .int, .frac, .typed_int, .typed_frac, .single_quote => false,
                        else => true,
                    };
                    if (fn_needs_parens) {
                        try fmt.push('(');
                        try fmt.formatExprInnerDiscard(apply_fn_idx, .no_indent_on_access);
                        try fmt.push(')');
                        const right_region = fmt.nodeRegion(@intFromEnum(ld.right));
                        const fn_region = fmt.nodeRegion(@intFromEnum(apply_fn_idx));
                        const args_region = AST.TokenizedRegion{ .start = fn_region.end, .end = right_region.end };
                        try fmt.formatCollection(args_region, .round, AST.Expr.Idx, fmt.ast.store.exprSlice(apply.args), Formatter.formatExpr);
                    } else {
                        try fmt.formatExprInnerDiscard(ld.right, .no_indent_on_access);
                    }
                } else {
                    // Lambda or other expression: wrap in parens for round-trip safety
                    try fmt.push('(');
                    try fmt.formatExprInnerDiscard(ld.right, .no_indent_on_access);
                    try fmt.push(')');
                }
            },
            .int => |i| {
                try fmt.pushTokenText(i.token);
            },
            .frac => |f| {
                try fmt.pushTokenText(f.token);
            },
            .typed_int => |ti| {
                try fmt.pushTokenText(ti.token);
                try fmt.push('.');
                try fmt.pushAll(fmt.ast.env.getIdent(ti.type_ident));
            },
            .typed_frac => |tf| {
                try fmt.pushTokenText(tf.token);
                try fmt.push('.');
                try fmt.pushAll(fmt.ast.env.getIdent(tf.type_ident));
            },
            .list => |l| {
                try fmt.formatCollection(region, .square, AST.Expr.Idx, fmt.ast.store.exprSlice(l.items), Formatter.formatExpr);
            },
            .tuple => |t| {
                try fmt.formatCollection(region, .round, AST.Expr.Idx, fmt.ast.store.exprSlice(t.items), Formatter.formatExpr);
            },
            .tuple_access => |ta| {
                // Format: expr.N (e.g., tuple.0, tuple.1)
                const target = try fmt.formatExprWithInfo(ta.expr);
                _ = try fmt.continueAfterMultilineStringLine(target);
                // Get the element index from the token
                const token_text = fmt.ast.resolve(ta.elem_token);
                // Token includes leading dot (e.g., ".0")
                try fmt.pushAll(token_text);
            },
            .record => |r| {
                try fmt.push('{');

                const fields = fmt.ast.store.recordFieldSlice(r.fields);
                var has_extension = false;

                // Handle extension if present
                if (r.ext) |ext| {
                    if (multiline) {
                        fmt.curr_indent += 1;
                        try fmt.flushCommentsAfterDiscard(r.region.start);
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    try fmt.pushAll("..");
                    const ext_region = try fmt.formatExpr(ext);
                    has_extension = true;

                    try fmt.push(',');
                    if (multiline and fields.len > 0) {
                        try fmt.flushCommentsAfterDiscard(ext_region.end);
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                }

                // Format fields
                if (multiline and !has_extension and fields.len > 0) {
                    fmt.curr_indent += 1;
                    try fmt.flushCommentsAfterDiscard(r.region.start);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }

                for (fields, 0..) |field_idx, i| {
                    if (!multiline) {
                        try fmt.push(' ');
                    }
                    const field_region = try fmt.formatRecordField(field_idx);
                    if (multiline) {
                        if (fmt.has_multiline_string) {
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        try fmt.push(',');
                        try fmt.flushCommentsAfterDiscard(field_region.end);
                        if (i == fields.len - 1) {
                            fmt.curr_indent -= 1;
                        }
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    } else if (i < fields.len - 1) {
                        try fmt.pushAll(",");
                    }
                }

                if ((has_extension or fields.len > 0) and !multiline) {
                    try fmt.push(' ');
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
                    try fmt.flushCommentsAfterDiscard(l.region.start);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }
                for (args, 0..) |arg, i| {
                    const arg_region = try fmt.formatPattern(arg);
                    if (args_are_multiline) {
                        try fmt.push(',');
                        try fmt.flushCommentsAfterDiscard(arg_region.end);
                        if (i == args.len - 1) {
                            fmt.curr_indent -= 1;
                        }
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    } else if (i < args.len - 1) {
                        try fmt.pushAll(", ");
                    }
                }
                try fmt.push('|');
                if (try fmt.flushCommentsBefore(body_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(l.body);
            },
            .unary_op => |op| {
                try fmt.pushTokenText(op.operator);
                try fmt.formatExprDiscard(op.expr);
            },
            .bin_op => |op| {
                const op_tag = fmt.ast.tokens.tokens.items(.tag)[op.operator];
                const is_range_op = op_tag == .OpDoubleDotLessThan or op_tag == .OpDoubleDotEquals;
                if (fmt.flags == .debug_binop) {
                    try fmt.push('(');
                    if (multiline) {
                        try fmt.newline();
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    }
                }
                const left = try fmt.formatExprWithInfo(op.left);
                var pushed = false;
                if (try fmt.continueAfterMultilineStringLine(left)) {
                    pushed = true;
                } else if (multiline and try fmt.flushCommentsBefore(op.operator)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                    pushed = true;
                } else if (!is_range_op) {
                    try fmt.push(' ');
                }
                try fmt.pushTokenText(op.operator);
                const right_region = fmt.nodeRegion(@intFromEnum(op.right));
                if (multiline and try fmt.flushCommentsBefore(right_region.start)) {
                    fmt.curr_indent += if (pushed) 0 else 1;
                    try fmt.pushIndent();
                } else if (!is_range_op) {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(op.right);
                if (fmt.flags == .debug_binop) {
                    if (multiline) {
                        fmt.curr_indent -= 1;
                        try fmt.pushIndent();
                    }
                    try fmt.push(')');
                }
            },
            .suffix_single_question => |s| {
                const body = try fmt.formatExprWithInfo(s.expr);
                _ = try fmt.continueAfterMultilineStringLine(body);
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
                // Check if then/else are blocks - blocks use original behavior,
                // non-blocks use base_indent to keep else at the same level as if
                const then_is_block = fmt.ast.store.getExpr(i.then) == .block;
                const else_is_block = fmt.ast.store.getExpr(i.@"else") == .block;
                const has_blocks = then_is_block or else_is_block;

                try fmt.pushAll("if");
                const base_indent = fmt.curr_indent;
                const cond_region = fmt.nodeRegion(@intFromEnum(i.condition));
                var flushed = try fmt.flushCommentsBefore(cond_region.start);
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(i.condition);
                if (!has_blocks) fmt.curr_indent = base_indent;
                const then_region = fmt.nodeRegion(@intFromEnum(i.then));
                flushed = try fmt.flushCommentsBefore(then_region.start);
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(i.then);
                if (!has_blocks) fmt.curr_indent = base_indent;
                flushed = try fmt.flushCommentsBefore(then_region.end);
                if (flushed) {
                    if (has_blocks) fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.pushAll("else");
                if (!has_blocks) fmt.curr_indent = base_indent;
                const else_region = fmt.nodeRegion(@intFromEnum(i.@"else"));
                flushed = try fmt.flushCommentsBefore(else_region.start);
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(i.@"else");
            },
            .if_without_else => |i| {
                // Check if then is a block - blocks use original behavior,
                // non-blocks use base_indent logic
                const then_is_block = fmt.ast.store.getExpr(i.then) == .block;

                try fmt.pushAll("if");
                const base_indent = fmt.curr_indent;
                const cond_region = fmt.nodeRegion(@intFromEnum(i.condition));
                var flushed = try fmt.flushCommentsBefore(cond_region.start);
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(i.condition);
                if (!then_is_block) fmt.curr_indent = base_indent;
                const then_region = fmt.nodeRegion(@intFromEnum(i.then));
                flushed = try fmt.flushCommentsBefore(then_region.start);
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(i.then);
            },
            .match => |m| {
                try fmt.pushAll("match ");
                try fmt.formatExprDiscard(m.expr);
                try fmt.pushAll(" {");
                fmt.curr_indent += 1;
                const branch_indent = fmt.curr_indent;
                const branches = fmt.ast.store.matchBranchSlice(m.branches);
                if (branches.len == 0) {
                    try fmt.push('}');
                    return formatted;
                }
                var branch_region = fmt.nodeRegion(@intFromEnum(branches[0]));
                for (branches) |b| {
                    fmt.curr_indent = branch_indent;
                    branch_region = fmt.nodeRegion(@intFromEnum(b));
                    const branch = fmt.ast.store.getBranch(b);
                    try fmt.flushCommentsBeforeDiscard(branch_region.start);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                    const pattern_region = try fmt.formatPattern(branch.pattern);
                    if (branch.guard) |guard| {
                        try fmt.pushAll(" if ");
                        try fmt.formatExprDiscard(guard);
                    }
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
                    try fmt.formatExprDiscard(branch.body);
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
                try fmt.formatExprDiscard(d.expr);
            },
            .block => |b| {
                try fmt.formatBlock(b);
            },
            .for_expr => |f| {
                try fmt.pushAll("for ");
                try fmt.formatPatternDiscard(f.patt);
                try fmt.pushAll(" in ");
                try fmt.formatExprDiscard(f.expr);
                const body_region = fmt.nodeRegion(@intFromEnum(f.body));
                const flushed = try fmt.flushCommentsBefore(body_region.start);
                if (flushed) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(f.body);
            },
            .ellipsis => {
                try fmt.pushAll("...");
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
                try fmt.formatExprDiscard(r.expr);
            },
            .@"break" => {
                try fmt.pushAll("break");
            },
            .record_builder => |rb| {
                // Format record builder: { field: value, ... }.TypeName
                const fields = fmt.ast.store.recordFieldSlice(rb.fields);

                try fmt.push('{');

                // Format fields like a regular record
                if (multiline and fields.len > 0) {
                    fmt.curr_indent += 1;
                    try fmt.flushCommentsAfterDiscard(rb.region.start);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }

                for (fields, 0..) |field_idx, i| {
                    if (!multiline) {
                        try fmt.push(' ');
                    }
                    const field_region = try fmt.formatRecordField(field_idx);

                    if (i < fields.len - 1) {
                        try fmt.push(',');
                        if (multiline) {
                            try fmt.flushCommentsAfterDiscard(field_region.end);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                    } else if (multiline) {
                        try fmt.push(',');
                        try fmt.flushCommentsAfterDiscard(field_region.end);
                        fmt.curr_indent -= 1;
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                }

                if (fields.len > 0 and !multiline) {
                    try fmt.push(' ');
                }
                try fmt.push('}');

                // Format the type suffix (mapper)
                const mapper_expr = fmt.ast.store.getExpr(rb.mapper);
                switch (mapper_expr) {
                    .tag => |t| {
                        try fmt.push('.');
                        // Format qualifiers if any
                        const qualifiers = fmt.ast.store.tokenSlice(t.qualifiers);
                        for (qualifiers) |qual_tok| {
                            try fmt.pushTokenText(qual_tok);
                            try fmt.push('.');
                        }
                        try fmt.pushTokenText(t.token);
                    },
                    .ident => |id| {
                        try fmt.push('.');
                        // Format qualifiers if any
                        const qualifiers = fmt.ast.store.tokenSlice(id.qualifiers);
                        for (qualifiers) |qual_tok| {
                            try fmt.pushTokenText(qual_tok);
                            try fmt.push('.');
                        }
                        try fmt.pushTokenText(id.token);
                    },
                    else => {
                        // Fallback - shouldn't happen for valid record builders
                        try fmt.push('.');
                        try fmt.formatExprDiscard(rb.mapper);
                    },
                }
            },
            .nominal_apply => |na| {
                // Format nominal value/tuple construction: Type.(arg1, arg2, ...)
                try fmt.formatExprDiscard(na.mapper);
                try fmt.push('.');
                const mapper_region = fmt.nodeRegion(@intFromEnum(na.mapper));
                const args_region = AST.TokenizedRegion{ .start = mapper_region.end, .end = region.end };
                try fmt.formatCollection(args_region, .round, AST.Expr.Idx, fmt.ast.store.exprSlice(na.args), Formatter.formatExpr);
            },
            .nominal_record => |nr| {
                try fmt.formatExprDiscard(nr.mapper);
                try fmt.push('.');
                try fmt.formatExprDiscard(nr.backing);
            },
            .malformed => {
                // Output nothing for malformed node
            },
            else => {
                std.debug.panic("TODO: Handle formatting {s}", .{@tagName(expr)});
            },
        }
        return formatted;
    }

    fn formatPatternRecordField(fmt: *Formatter, idx: AST.PatternRecordField.Idx) FormatAstError!AST.TokenizedRegion {
        const field = fmt.ast.store.getPatternRecordField(idx);
        const multiline = fmt.nodeWillBeMultiline(AST.PatternRecordField.Idx, idx);
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }
        if (field.rest) {
            try fmt.pushAll("..");
            if (field.name) |name_tok| {
                if (multiline and try fmt.flushCommentsBefore(name_tok)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                try fmt.pushTokenText(name_tok);
            }
        } else {
            const name_tok = field.name orelse unreachable;
            try fmt.pushTokenText(name_tok);
            if (field.value) |v| {
                if (multiline and try fmt.flushCommentsAfter(name_tok)) {
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
                try fmt.formatPatternDiscard(v);
            }
        }
        return field.region;
    }

    fn formatPattern(fmt: *Formatter, pi: AST.Pattern.Idx) FormatAstError!AST.TokenizedRegion {
        const pattern = fmt.ast.store.getPattern(pi);
        var region = AST.TokenizedRegion{ .start = 0, .end = 0 };
        const multiline = fmt.nodeWillBeMultiline(AST.Pattern.Idx, pi);
        switch (pattern) {
            .ident => |i| {
                region = i.region;
                try fmt.formatIdent(i.ident_tok, null);
            },
            .var_ident => |i| {
                region = i.region;
                try fmt.pushAll("var ");
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
                if (t.backing_value) {
                    // Nominal-value destructure renders as `Type.(args)` (the `.`
                    // distinguishes it from an ordinary applied-tag `Tag(args)`).
                    try fmt.push('.');
                }
                if (t.args.span.len > 0) {
                    try fmt.formatCollection(region, .round, AST.Pattern.Idx, fmt.ast.store.patternSlice(t.args), Formatter.formatPattern);
                }
            },
            .string => |s| {
                region = s.region;
                try fmt.formatPatternString(s);
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
            .typed_int => |n| {
                region = n.region;
                try fmt.formatIdent(n.number_tok, null);
                try fmt.push('.');
                try fmt.pushAll(fmt.ast.env.getIdent(n.type_ident));
            },
            .typed_frac => |n| {
                region = n.region;
                try fmt.formatIdent(n.number_tok, null);
                try fmt.push('.');
                try fmt.pushAll(fmt.ast.env.getIdent(n.type_ident));
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
                    try fmt.formatPatternDiscard(p);
                    fmt.curr_indent = curr_indent;
                    if (i < a.patterns.span.len - 1) {
                        if (multiline) {
                            try fmt.flushCommentsBeforeDiscard(pattern_region.end);
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
                try fmt.formatPatternDiscard(a.pattern);
                try fmt.pushAll(" as ");
                try fmt.pushTokenText(a.name);
            },
            .malformed => {
                // Output nothing for malformed node
            },
        }
        return region;
    }

    fn formatExposedItem(fmt: *Formatter, idx: AST.ExposedItem.Idx) error{WriteFailed}!AST.TokenizedRegion {
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

    /// Format a targets section in a platform header
    fn formatTargetsSection(fmt: *Formatter, targets_idx: AST.TargetsSection.Idx) (Allocator.Error || error{WriteFailed})!void {
        const targets = fmt.ast.store.getTargetsSection(targets_idx);
        const start_indent = fmt.curr_indent;

        try fmt.pushAll("targets: {");

        var has_content = false;

        // Format inputs_dir: directory directive if present
        if (targets.inputs_dir) |inputs_token| {
            has_content = true;
            try fmt.ensureNewline();
            fmt.curr_indent = start_indent + 1;
            try fmt.pushIndent();
            try fmt.pushAll("inputs_dir: ");
            try fmt.push('"');
            try fmt.pushTokenText(inputs_token);
            try fmt.push('"');
            try fmt.push(',');
        }

        // Format per-target entries
        for (fmt.ast.store.targetEntrySlice(targets.entries)) |entry_idx| {
            has_content = true;
            try fmt.ensureNewline();
            fmt.curr_indent = start_indent + 1;
            try fmt.pushIndent();
            try fmt.formatTargetEntry(entry_idx);
            try fmt.push(',');
        }

        if (has_content) {
            try fmt.ensureNewline();
            fmt.curr_indent = start_indent;
            try fmt.pushIndent();
        }
        try fmt.push('}');
    }

    /// Format a symbol map section: { "roc_main": main_for_host!, ... }
    fn formatSymbolMapSection(fmt: *Formatter, span: AST.SymbolMapEntry.Span, base_indent: u32) (Allocator.Error || error{WriteFailed})!void {
        const entries = fmt.ast.store.symbolMapEntrySlice(span);
        if (entries.len == 0) {
            try fmt.pushAll("{}");
            return;
        }
        if (entries.len <= 2) {
            try fmt.pushAll("{ ");
            for (entries, 0..) |entry_idx, i| {
                if (i > 0) {
                    try fmt.pushAll(", ");
                }
                try fmt.formatSymbolMapEntry(entry_idx);
            }
            try fmt.pushAll(" }");
            return;
        }
        try fmt.push('{');
        for (entries) |entry_idx| {
            try fmt.ensureNewline();
            fmt.curr_indent = base_indent + 1;
            try fmt.pushIndent();
            try fmt.formatSymbolMapEntry(entry_idx);
            try fmt.push(',');
        }
        try fmt.ensureNewline();
        fmt.curr_indent = base_indent;
        try fmt.pushIndent();
        try fmt.push('}');
    }

    /// Format a single symbol map entry: "roc_stdout_line": Stdout.line!
    fn formatSymbolMapEntry(fmt: *Formatter, entry_idx: AST.SymbolMapEntry.Idx) (Allocator.Error || error{WriteFailed})!void {
        const entry = fmt.ast.store.getSymbolMapEntry(entry_idx);
        try fmt.push('"');
        try fmt.pushTokenText(entry.symbol);
        try fmt.push('"');
        try fmt.pushAll(": ");
        if (entry.module) |module_tok| {
            // Emit every token from the module through the function name; for
            // functions on nested type modules (Foo.Idx.get!) the tokens in
            // between are the nested type segments.
            var tok = module_tok;
            while (tok <= entry.func) : (tok += 1) {
                if (tok != module_tok) try fmt.push('.');
                try fmt.pushTokenText(tok);
            }
        } else {
            try fmt.pushTokenText(entry.func);
        }
    }

    /// Format a single target entry: x64linux: { inputs: ["host.o", app], output: Exe }
    fn formatTargetEntry(fmt: *Formatter, entry_idx: AST.TargetEntry.Idx) (Allocator.Error || error{WriteFailed})!void {
        const entry = fmt.ast.store.getTargetEntry(entry_idx);

        // Format target name (e.g., x64linux)
        try fmt.pushTokenText(entry.target);
        try fmt.pushAll(": ");
        try fmt.formatTargetConfig(entry.config);
    }

    fn formatTargetConfig(fmt: *Formatter, config_idx: AST.TargetConfig.Idx) (Allocator.Error || error{WriteFailed})!void {
        const config = fmt.ast.store.getTargetConfig(config_idx);
        const entries = fmt.ast.store.targetConfigEntrySlice(config.entries);
        const base_indent = fmt.curr_indent;

        if (entries.len == 1) {
            const entry = fmt.ast.store.getTargetConfigEntry(entries[0]);
            try fmt.pushAll("{ ");
            try fmt.formatTargetConfigEntry(entry);
            try fmt.pushAll(" }");
            return;
        }

        try fmt.push('{');
        for (entries, 0..) |entry_idx, i| {
            const entry = fmt.ast.store.getTargetConfigEntry(entry_idx);
            try fmt.ensureNewline();
            fmt.curr_indent = base_indent + 1;
            try fmt.pushIndent();
            try fmt.formatTargetConfigEntry(entry);
            if (i < entries.len - 1 or entries.len > 0) {
                try fmt.push(',');
            }
        }

        if (entries.len > 0) {
            try fmt.ensureNewline();
            fmt.curr_indent = base_indent;
            try fmt.pushIndent();
        }
        try fmt.push('}');
    }

    fn formatTargetConfigEntry(fmt: *Formatter, entry: AST.TargetConfigEntry) (Allocator.Error || error{WriteFailed})!void {
        try fmt.pushTokenText(entry.name);
        if (fmt.targetConfigEntryIsPunned(entry)) return;
        try fmt.pushAll(": ");
        try fmt.formatTargetConfigValue(entry.value);
    }

    fn targetConfigEntryIsPunned(fmt: *Formatter, entry: AST.TargetConfigEntry) bool {
        return switch (fmt.ast.store.getTargetConfigValue(entry.value)) {
            .ident => |token| token == entry.name,
            else => false,
        };
    }

    fn formatTargetConfigValue(fmt: *Formatter, value_idx: AST.TargetConfigValue.Idx) (Allocator.Error || error{WriteFailed})!void {
        const value = fmt.ast.store.getTargetConfigValue(value_idx);
        switch (value) {
            .int_literal, .tag_literal, .ident => |token| {
                try fmt.pushTokenText(token);
            },
            .string_literal => |token| {
                try fmt.push('"');
                try fmt.pushTokenText(token);
                try fmt.push('"');
            },
            .list => |span| {
                const values = fmt.ast.store.targetConfigValueSlice(span);
                try fmt.push('[');
                for (values, 0..) |child_idx, i| {
                    try fmt.formatTargetConfigValue(child_idx);
                    if (i < values.len - 1) {
                        try fmt.pushAll(", ");
                    }
                }
                try fmt.push(']');
            },
            .files => |span| {
                const files = fmt.ast.store.targetFileSlice(span);
                try fmt.push('[');
                for (files, 0..) |file_idx, i| {
                    try fmt.formatTargetFile(file_idx);
                    if (i < files.len - 1) {
                        try fmt.pushAll(", ");
                    }
                }
                try fmt.push(']');
            },
            .malformed => {},
        }
    }

    /// Format a single target file entry
    fn formatTargetFile(fmt: *Formatter, file_idx: AST.TargetFile.Idx) error{WriteFailed}!void {
        const file = fmt.ast.store.getTargetFile(file_idx);
        switch (file) {
            .string_literal => |token| {
                try fmt.push('"');
                try fmt.pushTokenText(token);
                try fmt.push('"');
            },
            .special_ident => |token| {
                try fmt.pushTokenText(token);
            },
            .malformed => {
                // Don't format malformed target files - they'll be reported as errors
            },
        }
    }

    fn formatHeader(fmt: *Formatter, hi: AST.Header.Idx) FormatAstError!void {
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
                var package_fields_list = try std.array_list.Managed(AST.RecordField.Idx).initCapacity(fmt.ast.store.gpa, 10);
                const packages_slice = fmt.ast.store.recordFieldSlice(.{ .span = packages.span });
                for (packages_slice) |package_idx| {
                    if (package_idx == a.platform_idx) {
                        platform_field = package_idx;
                        continue;
                    }
                    try package_fields_list.append(package_idx);
                }
                const package_fields = try package_fields_list.toOwnedSlice();
                defer fmt.ast.store.gpa.free(package_fields);

                if (platform_field) |field_idx| {
                    const field = fmt.ast.store.getRecordField(field_idx);
                    if (packages_multiline) {
                        try fmt.flushCommentsBeforeDiscard(field.region.start);
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                    try fmt.pushTokenText(field.name);
                    if (field.value) |v| {
                        try fmt.push(':');
                        try fmt.push(' ');
                        try fmt.pushAll("platform");
                        try fmt.push(' ');
                        try fmt.formatExprDiscard(v);
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
                        try fmt.flushCommentsBeforeDiscard(item_region.start);
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                    const field_region = try fmt.formatRecordField(field_idx);
                    Formatter.discardRegion(field_region);
                    if (packages_multiline) {
                        try fmt.push(',');
                    } else if (i < package_fields.len - 1) {
                        try fmt.pushAll(", ");
                    }
                }
                if (packages_multiline) {
                    try fmt.flushCommentsBeforeDiscard(packages.region.end - 1);
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
                    try fmt.flushCommentsAfterDiscard(p.region.start);
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

                try fmt.flushCommentsAfterDiscard(p.name + 1);
                try fmt.ensureNewline();
                fmt.curr_indent = start_indent + 1;
                try fmt.pushIndent();

                try fmt.pushAll("requires {");
                // Format requires entries with for-clause syntax
                const entries = fmt.ast.store.requiresEntrySlice(p.requires_entries);
                if (entries.len > 0) {
                    try fmt.ensureNewline();
                    fmt.curr_indent = start_indent + 2;
                    for (entries, 0..) |entry_idx, entry_i| {
                        const entry = fmt.ast.store.getRequiresEntry(entry_idx);
                        try fmt.pushIndent();

                        // Format type aliases: [Model : model] for ...
                        // Only output the bracket syntax if there are type aliases
                        const aliases = fmt.ast.store.forClauseTypeAliasSlice(entry.type_aliases);
                        if (aliases.len > 0) {
                            try fmt.push('[');
                            for (aliases, 0..) |alias_idx, alias_i| {
                                const alias = fmt.ast.store.getForClauseTypeAlias(alias_idx);
                                try fmt.pushTokenText(alias.alias_name);
                                try fmt.pushAll(" : ");
                                try fmt.pushTokenText(alias.rigid_name);
                                if (alias_i < aliases.len - 1) {
                                    try fmt.pushAll(", ");
                                }
                            }
                            try fmt.pushAll("] for ");
                        }

                        // Format entrypoint name
                        try fmt.pushTokenText(entry.entrypoint_name);
                        try fmt.pushAll(" : ");

                        // Format type annotation
                        try fmt.formatTypeAnnoDiscard(entry.type_anno);

                        if (entry_i < entries.len - 1) {
                            try fmt.push(',');
                        }
                        try fmt.ensureNewline();
                    }
                    fmt.curr_indent = start_indent + 1;
                    try fmt.pushIndent();
                }
                try fmt.push('}');
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

                try fmt.flushCommentsBeforeDiscard(exposes.region.end);
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

                try fmt.flushCommentsBeforeDiscard(packages.region.end);
                try fmt.ensureNewline();
                fmt.curr_indent = start_indent + 1;
                try fmt.pushIndent();

                try fmt.pushAll("provides ");
                try fmt.formatSymbolMapSection(p.provides, start_indent + 1);

                if (p.hosted.span.len > 0) {
                    try fmt.ensureNewline();
                    fmt.curr_indent = start_indent + 1;
                    try fmt.pushIndent();
                    try fmt.pushAll("hosted ");
                    try fmt.formatSymbolMapSection(p.hosted, start_indent + 1);
                }

                // Format targets section if present
                if (p.targets) |targets_idx| {
                    try fmt.ensureNewline();
                    fmt.curr_indent = start_indent + 1;
                    try fmt.pushIndent();
                    try fmt.formatTargetsSection(targets_idx);
                }
            },
            .type_module => {},
            .default_app => {},
            .malformed => {},
        }
    }

    fn nodeRegion(fmt: *Formatter, idx: u32) AST.TokenizedRegion {
        return fmt.ast.store.nodes.items.items(.region)[idx];
    }

    fn formatBlock(fmt: *Formatter, block: AST.Block) FormatAstError!void {
        if (block.statements.span.len > 0) {
            fmt.curr_indent += 1;
            try fmt.push('{');
            for (fmt.ast.store.statementSlice(block.statements), 0..) |s, i| {
                const region = fmt.nodeRegion(@intFromEnum(s));
                try fmt.flushCommentsBeforeDiscard(region.start);
                try fmt.ensureNewline();
                try fmt.pushIndent();
                try fmt.formatStatement(s);

                if (i == block.statements.span.len - 1) {
                    try fmt.flushCommentsBeforeDiscard(region.end);
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

    fn formatTypeHeader(fmt: *Formatter, header: AST.TypeHeader.Idx) FormatAstError!void {
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

    fn formatAnnoRecordField(fmt: *Formatter, idx: AST.AnnoRecordField.Idx) FormatAstError!AST.TokenizedRegion {
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
        try fmt.formatTypeAnnoDiscard(field.ty);
        return field.region;
    }

    fn formatWhereClause(fmt: *Formatter, idx: AST.WhereClause.Idx) FormatAstError!void {
        const clause = fmt.ast.store.getWhereClause(idx);
        const start_indent = fmt.curr_indent;
        defer fmt.curr_indent = start_indent;

        const multiline = fmt.nodeWillBeMultiline(AST.WhereClause.Idx, idx);
        switch (clause) {
            .mod_method => |c| {
                // Format as: a.method : Type
                try fmt.pushTokenText(c.var_tok);
                if (multiline and try fmt.flushCommentsAfter(c.var_tok)) {
                    fmt.curr_indent = start_indent;
                    try fmt.pushIndent();
                }
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
                            try fmt.flushCommentsBeforeDiscard(arg_region.start);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        try fmt.formatTypeAnnoDiscard(arg_idx);
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
                try fmt.formatTypeAnnoDiscard(c.ret_anno);
            },
            .mod_alias => |c| {
                // Format as: a.TypeAlias
                try fmt.pushTokenText(c.var_tok);
                if (multiline and try fmt.flushCommentsAfter(c.var_tok)) {
                    fmt.curr_indent = start_indent;
                    try fmt.pushIndent();
                }
                try fmt.push('.');
                try fmt.pushTokenText(c.name_tok);
            },
            .malformed => {
                // Output nothing for malformed node
            },
        }
    }

    fn formatTypeAnno(fmt: *Formatter, anno: AST.TypeAnno.Idx) FormatAstError!AST.TokenizedRegion {
        const a = fmt.ast.store.getTypeAnno(anno);
        var region = AST.TokenizedRegion{ .start = 0, .end = 0 };
        const multiline = fmt.nodeWillBeMultiline(AST.TypeAnno.Idx, anno);
        switch (a) {
            .apply => |app| {
                const slice = fmt.ast.store.typeAnnoSlice(app.args);
                const first = slice[0];
                try fmt.formatTypeAnnoDiscard(first);
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
                switch (r.ext) {
                    .closed => {
                        // Regular record without extension - use formatCollection
                        try fmt.formatCollection(region, .curly, AST.AnnoRecordField.Idx, fmt.ast.store.annoRecordFieldSlice(r.fields), Formatter.formatAnnoRecordField);
                    },
                    .open, .named => {
                        // Record with extension - handle specially
                        try fmt.formatRecordWithExtension(r.fields, r.ext, region);
                    },
                }
            },
            .tag_union => |t| {
                region = t.region;
                const tags = fmt.ast.store.typeAnnoSlice(t.tags);
                const is_open = t.ext != .closed;
                const tag_multiline = fmt.ast.regionIsMultiline(region) or fmt.nodesWillBeMultiline(AST.TypeAnno.Idx, tags);
                const tag_indent = fmt.curr_indent;
                defer {
                    fmt.curr_indent = tag_indent;
                }
                try fmt.push('[');
                if (tags.len == 0 and !is_open) {
                    try fmt.push(']');
                } else {
                    if (tag_multiline) {
                        fmt.curr_indent += 1;
                    }
                    for (tags, 0..) |tag_idx, i| {
                        const tag_region = fmt.nodeRegion(@intFromEnum(tag_idx));
                        if (tag_multiline) {
                            try fmt.flushCommentsBeforeDiscard(tag_region.start);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        try fmt.formatTypeAnnoDiscard(tag_idx);
                        if (tag_multiline) {
                            try fmt.push(',');
                        } else if (i < (tags.len - 1) or is_open) {
                            try fmt.pushAll(", ");
                        }
                    }
                    // Handle open tag unions - always format as just ".." (silently drop any named extension)
                    if (is_open) {
                        // Get the token position for flushing comments before the ..
                        const double_dot_token: Token.Idx = switch (t.ext) {
                            .named => |named| named.region.start,
                            .open => |tok| tok,
                            .closed => unreachable, // is_open is true
                        };
                        if (tag_multiline) {
                            try fmt.flushCommentsBeforeDiscard(double_dot_token);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        try fmt.pushAll("..");
                        if (tag_multiline) {
                            try fmt.push(',');
                        }
                    }
                    if (tag_multiline) {
                        try fmt.flushCommentsBeforeDiscard(region.end - 1);
                        fmt.curr_indent -= 1;
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                    try fmt.push(']');
                }
            },
            .@"fn" => |f| {
                region = f.region;

                const args = fmt.ast.store.typeAnnoSlice(f.args);
                for (args, 0..) |idx, i| {
                    const arg_region = fmt.nodeRegion(@intFromEnum(idx));
                    if (multiline and i > 0) {
                        try fmt.flushCommentsBeforeDiscard(arg_region.start);
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                    try fmt.formatTypeAnnoDiscard(idx);
                    if (i < args.len - 1) {
                        if (multiline) {
                            try fmt.push(',');
                        } else {
                            try fmt.pushAll(", ");
                        }
                    }
                }

                if (args.len == 0) {
                    try fmt.pushAll("()");
                }

                try fmt.pushAll(if (f.effectful) " =>" else " ->");
                const ret_region = fmt.nodeRegion(@intFromEnum(f.ret));
                if (multiline and try fmt.flushCommentsBefore(ret_region.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }

                try fmt.formatTypeAnnoDiscard(f.ret);
            },
            .parens => |p| {
                region = p.region;
                try fmt.push('(');
                if (multiline) {
                    try fmt.flushCommentsAfterDiscard(region.start);
                    fmt.curr_indent += 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }
                const anno_region = try fmt.formatTypeAnno(p.anno);
                try fmt.flushCommentsBeforeDiscard(anno_region.end);
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

    fn ensureNewline(fmt: *Formatter) error{WriteFailed}!void {
        if (fmt.has_newline) {
            return;
        }
        try fmt.newline();
    }

    fn newline(fmt: *Formatter) error{WriteFailed}!void {
        try fmt.push('\n');
    }

    fn flushCommentsBefore(fmt: *Formatter, tokenIdx: Token.Idx) error{WriteFailed}!bool {
        return fmt.flushCommentsBeforeMin(tokenIdx, 0);
    }

    /// True iff the source text between the previous token and `tokenIdx`
    /// contains an actual `#` comment. Use this to decide whether to preserve
    /// inter-token whitespace, since `flushCommentsBefore` always emits any
    /// source newlines it finds (which is wrong for places where bare line
    /// breaks should be normalized to a single space).
    fn hasCommentBefore(fmt: *Formatter, tokenIdx: Token.Idx) bool {
        const start = if (tokenIdx == 0) 0 else fmt.ast.tokens.resolve(tokenIdx - 1).end.offset;
        const end = fmt.ast.tokens.resolve(tokenIdx).start.offset;
        return std.mem.findScalar(u8, fmt.ast.env.source[start..end], '#') != null;
    }

    /// Like `flushCommentsBefore`, but ensures at least `min_leading_newlines` newlines
    /// are emitted before any comment or trailing content. Used to insert blank lines
    /// between top-level defs.
    fn flushCommentsBeforeMin(fmt: *Formatter, tokenIdx: Token.Idx, min_leading_newlines: u8) error{WriteFailed}!bool {
        const start = if (tokenIdx == 0) 0 else fmt.ast.tokens.resolve(tokenIdx - 1).end.offset;
        const end = fmt.ast.tokens.resolve(tokenIdx).start.offset;
        return fmt.flushComments(fmt.ast.env.source[start..end], min_leading_newlines);
    }

    fn flushCommentsAfter(fmt: *Formatter, tokenIdx: Token.Idx) error{WriteFailed}!bool {
        const start = fmt.ast.tokens.resolve(tokenIdx).end.offset;
        const end = fmt.ast.tokens.resolve(tokenIdx + 1).start.offset;
        return fmt.flushComments(fmt.ast.env.source[start..end], 0);
    }

    fn flushCommentsEOF(fmt: *Formatter) error{WriteFailed}!void {
        const last_token_idx = if (fmt.ast.tokens.tokens.len >= 2) fmt.ast.tokens.tokens.len - 2 else 0;
        const start = fmt.ast.tokens.resolve(last_token_idx).end.offset;
        const end = fmt.ast.env.source.len;
        const between_text = fmt.ast.env.source[start..end];

        var newline_count_to_apply: usize = 0;
        var i: usize = 0;
        while (i < between_text.len) {
            if (between_text[i] == '#') {
                // Found a comment, extract it
                const comment_start = i + 1; // Skip the #
                var comment_end = comment_start;
                while (comment_end < between_text.len and between_text[comment_end] != '\n' and between_text[comment_end] != '\r') {
                    comment_end += 1;
                }

                if (newline_count_to_apply > 0) {
                    for (0..@min(2, newline_count_to_apply)) |_| {
                        try fmt.newline();
                    }
                } else if (!fmt.has_newline) {
                    try fmt.push(' ');
                }
                try fmt.push('#');
                const comment_text = between_text[comment_start..comment_end];
                // Add space after # unless next char is space or # (preserves ## doc comments and ### separators)
                if (comment_text.len > 0 and comment_text[0] != ' ' and comment_text[0] != '#') {
                    try fmt.push(' ');
                }
                try fmt.pushAll(comment_text);
                newline_count_to_apply = 1; // reset count to allow an additional newline after a comment
                i = comment_end + 1;
            } else if (between_text[i] == '\n') {
                newline_count_to_apply += 1;
                i += 1;
            } else {
                i += 1;
            }
        }

        try fmt.ensureNewline();
    }

    fn flushComments(fmt: *Formatter, between_text: []const u8, min_leading_newlines: u8) error{WriteFailed}!bool {
        var newline_count: usize = 0;
        var prev_was_comment: bool = false;
        // True once we've either upgraded a source newline into a blank line
        // or padded up front to satisfy `min_leading_newlines`. Used to decide
        // whether we still owe a trailing blank line at the end.
        var leading_blank_satisfied: bool = (min_leading_newlines == 0);
        var i: usize = 0;
        while (i < between_text.len) {
            if (between_text[i] == '#') {
                // Found a comment, extract it
                const comment_start = i + 1; // Skip the #
                var comment_end = comment_start;
                while (comment_end < between_text.len and between_text[comment_end] != '\n' and between_text[comment_end] != '\r') {
                    comment_end += 1;
                }

                // If this comment is "standalone" (preceded by at least one
                // newline) AND we still owe the caller a leading blank line,
                // emit it now so the comment sticks to the next statement.
                // Inline comments (no preceding newline) are kept attached
                // to the previous statement and the blank line is emitted
                // afterwards.
                const is_inline = newline_count == 0 and !fmt.has_newline;
                if (!leading_blank_satisfied and !is_inline) {
                    while (newline_count < min_leading_newlines) {
                        try fmt.newline();
                        newline_count += 1;
                    }
                    leading_blank_satisfied = true;
                }

                // Check if it's a doc comment
                const is_doc_comment = comment_start < between_text.len and between_text[comment_start] == '#';
                // If a doc comment directly follows code (only one \n between them,
                // and the previous token wasn't another comment), add a blank line.
                if (is_doc_comment and newline_count == 1 and !prev_was_comment) {
                    try fmt.newline();
                    newline_count += 1;
                }

                if (newline_count > 0 or fmt.has_newline) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.push('#');
                const comment_text = between_text[comment_start..comment_end];
                // Add space after # unless next char is space or # (preserves ## doc comments and ### separators)
                if (comment_text.len > 0 and comment_text[0] != ' ' and comment_text[0] != '#') {
                    try fmt.push(' ');
                }
                try fmt.pushAll(comment_text);
                try fmt.newline();
                newline_count = 1; // reset count to allow an additional newline after a comment
                prev_was_comment = true;
                i = comment_end + 1;
            } else if (between_text[i] == '\n') {
                if (newline_count < 2) {
                    try fmt.newline();
                }
                newline_count += 1;
                // Upgrade the first source newline into a blank line if the
                // caller asked for one and we haven't already satisfied it.
                if (!leading_blank_satisfied and !prev_was_comment and newline_count == 1 and min_leading_newlines >= 2) {
                    try fmt.newline();
                    newline_count = 2;
                    leading_blank_satisfied = true;
                }
                i += 1;
            } else {
                i += 1;
            }
        }

        // If we still owe a blank line (e.g., the only content was an inline
        // comment, or the inter-statement region was empty), pad it on at the
        // end so the next statement is preceded by the requested blank.
        if (!leading_blank_satisfied) {
            while (newline_count < min_leading_newlines) {
                try fmt.newline();
                newline_count += 1;
            }
        }

        // Return true if there was a newline, whether or not there was a comment
        return newline_count > 0;
    }

    fn push(fmt: *Formatter, c: u8) error{WriteFailed}!void {
        if (c != '\t') {
            fmt.has_newline = c == '\n';
        }
        fmt.has_multiline_string = false;
        try fmt.writer.writeByte(c);
    }

    fn pushAll(fmt: *Formatter, str: []const u8) error{WriteFailed}!void {
        if (str.len == 0) {
            return;
        }
        const all_tabs = for (str) |c| {
            if (c != '\t') break false;
        } else true;
        if (!all_tabs) {
            fmt.has_newline = str[str.len - 1] == '\n';
        }
        fmt.has_multiline_string = false;
        try fmt.writer.writeAll(str);
    }

    fn pushIndent(fmt: *Formatter) error{WriteFailed}!void {
        if (fmt.curr_indent == 0 or !fmt.has_newline) {
            return;
        }
        for (0..fmt.curr_indent) |_| {
            try fmt.push('\t');
        }
    }

    fn pushTokenText(fmt: *Formatter, ti: Token.Idx) error{WriteFailed}!void {
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
                    .record_builder => |rb| {
                        return fmt.nodesWillBeMultiline(AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(rb.fields));
                    },
                    .nominal_record => |nr| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, nr.mapper)) {
                            return true;
                        }

                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, nr.backing);
                    },
                    .suffix_single_question => |s| {
                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, s.expr);
                    },
                    .unary_op => |u| {
                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, u.expr);
                    },
                    .field_access => |f| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, f.left)) {
                            return true;
                        }

                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, f.right);
                    },
                    .method_call => |m| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, m.receiver)) {
                            return true;
                        }

                        return fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(m.args));
                    },
                    .nominal_apply => |na| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, na.mapper)) {
                            return true;
                        }

                        return fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(na.args));
                    },
                    .lambda => |l| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, l.body)) {
                            return true;
                        }

                        if (fmt.nodesWillBeMultiline(AST.Pattern.Idx, fmt.ast.store.patternSlice(l.args))) {
                            return true;
                        }

                        return false;
                    },
                    .if_then_else => |i| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, i.condition)) {
                            return true;
                        }

                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, i.then)) {
                            return true;
                        }

                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, i.@"else");
                    },
                    .if_without_else => |i| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, i.condition)) {
                            return true;
                        }

                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, i.then);
                    },
                    .arrow_call => |l| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, l.left)) {
                            return true;
                        }

                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, l.right);
                    },
                    .for_expr => |f| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, f.expr)) {
                            return true;
                        }

                        return fmt.nodeWillBeMultiline(AST.Expr.Idx, f.body);
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
                    .platform => |p| {
                        // Requires entries with for-clause always multiline if present
                        if (p.requires_entries.span.len > 0) {
                            return true;
                        }
                        if (fmt.collectionWillBeMultiline(AST.ExposedItem.Idx, p.exposes)) {
                            return true;
                        }
                        if (fmt.collectionWillBeMultiline(AST.RecordField.Idx, p.packages)) {
                            return true;
                        }

                        return p.provides.span.len > 2 or p.hosted.span.len > 0;
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
            AST.ExposedItem.Idx => {
                const exposed_item_slice = fmt.ast.store.exposedItemSlice(.{ .span = collection.span });
                return fmt.nodesWillBeMultiline(AST.ExposedItem.Idx, exposed_item_slice);
            },
            else => return false,
        }
    }
};

/// Asserts a module when formatted twice in a row results in the same final output.
/// Returns that final output.
pub fn moduleFmtsStable(gpa: std.mem.Allocator, input: []const u8, debug: bool) FormatTestError![]const u8 {
    if (debug) {
        std.debug.print("Original:\n==========\n{s}\n==========\n\n", .{input});
    }

    const formatted = parseAndFmt(gpa, input, debug) catch |err| {
        switch (err) {
            else => return err,
        }
    };
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

fn parseAndFmt(gpa: std.mem.Allocator, input: []const u8, debug: bool) FormatParseError![]const u8 {
    var module_env = try ModuleEnv.init(gpa, input);
    defer module_env.deinit();

    const parse_ast = try parse.file(gpa, &module_env.common);
    defer parse_ast.deinit();

    // Currently disabled cause SExpr are missing a lot of IR coverage resulting in panics.
    if (debug and false) {
        // shouldn't be required in future
        parse_ast.store.emptyScratch();

        std.debug.print("Parsed SExpr:\n==========\n", .{});
        var sexpr_buf: std.Io.Writer.Allocating = .init(gpa);
        defer sexpr_buf.deinit();
        parse_ast.toSExprStr(module_env, &sexpr_buf.writer) catch @panic("Failed to print SExpr");
        std.debug.print("{s}", .{sexpr_buf.written()});
        std.debug.print("\n==========\n\n", .{});
    }

    std.testing.expectEqualSlices(AST.Diagnostic, &[_]AST.Diagnostic{}, parse_ast.parse_diagnostics.items) catch {
        return error.ParseFailed;
    };

    var result: std.Io.Writer.Allocating = .init(gpa);
    defer result.deinit();
    try formatAst(parse_ast.*, &result.writer);

    if (debug) {
        std.debug.print("Formatted:\n==========\n{s}\n==========\n\n", .{result.written()});
    }
    return try result.toOwnedSlice();
}

// Issue #8851: Formatter idempotence tests for arrow call with field access
// These test cases verify that formatting is stable (idempotent) - formatting twice
// produces the same output as formatting once.

test "issue 8851: arrow call with space before field access is idempotent" {
    // a=0->b .c() should format stably with newline to disambiguate
    const result = try moduleFmtsStable(std.testing.allocator, "a=0->b .c()", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = 0->b()\n\t.c()\n", result);
}

test "issue 8851: arrow call with chained zero-arg applies is idempotent" {
    // a = 0->b()().c() should format stably - must preserve ALL levels of function application
    const result = try moduleFmtsStable(std.testing.allocator, "a = 0->b()().c()", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = 0->b()().c()\n", result);
}

test "issue 8851: multiline arrow call with field access is idempotent" {
    // Multiline case from issue comment 1
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a=0->b
        \\      .c()
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = 0->b()\n\t.c()\n", result);
}

test "issue 8851: tuple dispatch with chained zero-arg applies is idempotent" {
    // ()->b()()() from issue comment 2
    const result = try moduleFmtsStable(std.testing.allocator, "a=()->b()()()", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = ()->b()()()\n", result);
}

test "issue 8851: chained field access after arrow call is idempotent" {
    // 0->b .c .d() - multiple field accesses, newline to disambiguate
    const result = try moduleFmtsStable(std.testing.allocator, "a=0->b .c .d()", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = 0->b()\n\t.c.d()\n", result);
}

test "issue 8851: arrow call with uppercase tag (module-like) is idempotent" {
    // 0->M .c - uppercase identifier parses as tag, not ident
    // Dispatching to a tag is invalid, newline disambiguates from qualified identifier
    const result = try moduleFmtsStable(std.testing.allocator, "a=0->M .c", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = 0->M\n\t.c\n", result);
}

test "issue 9785: multiline string followed by tuple access formats to valid source" {
    // https://github.com/roc-lang/roc/issues/9785
    const result = try moduleFmtsStable(std.testing.allocator,
        \\n=\\
        \\.0-||
        \\0
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("n = \\\\\n\t.0 - ||\n\t0\n", result);
}

test "issue 8894: typed integer literal formats correctly" {
    // Typed integer literals like 0.F or 123.U64 should format without panicking
    const result = try moduleFmtsStable(std.testing.allocator, "x = 0.F", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("x = 0.F\n", result);
}

test "issue 8894: typed frac literal formats correctly" {
    // Typed frac literals like 3.14.F64 should format without panicking
    const result = try moduleFmtsStable(std.testing.allocator, "x = 3.14.F64", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("x = 3.14.F64\n", result);
}

test "issue 9646: multiline method chain keeps short args inline without trailing comma" {
    // In a multiline method chain, each method-call argument that fits on one
    // line and has no input trailing comma should stay inline, not get expanded
    // into a multiline call with a trailing comma.
    const result = try moduleFmtsStable(std.testing.allocator,
        \\sprite = Sprite.from_texture(texture)
        \\    .source(Math.rect(1, 2, 3, 4))
        \\    .pos({ x: 5, y: 6 })
        \\    .scale(2)
        \\    .centered()
        \\    .rotation(90)
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "sprite = Sprite.from_texture(texture)\n" ++
            "\t.source(Math.rect(1, 2, 3, 4))\n" ++
            "\t.pos({ x: 5, y: 6 })\n" ++
            "\t.scale(2)\n" ++
            "\t.centered()\n" ++
            "\t.rotation(90)\n",
        result,
    );
}

test "issue 8989: platform header targets section is preserved" {
    // Platform header with targets section should preserve the targets after formatting
    const input =
        \\platform "test-platform"
        \\    requires {}
        \\    exposes []
        \\    packages {}
        \\    provides {}
        \\    targets: {
        \\        inputs_dir: "build/",
        \\        x64linux: { inputs: ["host.o", app] },
        \\        arm64linux: { inputs: ["host.o", app], output: Shared },
        \\    }
    ;
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);
    // The targets section must be preserved in the output
    try std.testing.expect(std.mem.find(u8, result, "targets:") != null);
}

test "blank line inserted between consecutive type annotations" {
    const input =
        \\to_f32 : U32 -> F32
        \\to_f64 : U32 -> F64
        \\to_dec : U32 -> Dec
    ;
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    const expected =
        \\to_f32 : U32 -> F32
        \\
        \\to_f64 : U32 -> F64
        \\
        \\to_dec : U32 -> Dec
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "no blank line between matching type anno and decl, blank between pairs" {
    const input =
        \\to_f64 : U32 -> F64
        \\to_f64 = |x| x
        \\to_dec : U32 -> Dec
        \\to_dec = |x| x
    ;
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    const expected =
        \\to_f64 : U32 -> F64
        \\to_f64 = |x| x
        \\
        \\to_dec : U32 -> Dec
        \\to_dec = |x| x
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "blank line inserted between consecutive value defs" {
    const input =
        \\to_f64 = |x| x
        \\to_dec = |x| x
    ;
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    const expected =
        \\to_f64 = |x| x
        \\
        \\to_dec = |x| x
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "blank line goes before comment that precedes the next def" {
    const input =
        \\foo : Str
        \\foo = "f"
        \\# comment for bar
        \\bar : Str
        \\bar = "b"
    ;
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    const expected =
        \\foo : Str
        \\foo = "f"
        \\
        \\# comment for bar
        \\bar : Str
        \\bar = "b"
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "type_anno followed by non-matching decl gets a blank line" {
    const input =
        \\foo : Str
        \\bar = "b"
    ;
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    const expected =
        \\foo : Str
        \\
        \\bar = "b"
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}

test "blank line inserted before doc comments following code" {
    const input =
        \\foo = 1
        \\## doc
        \\## doc
        \\bar = 2
        \\## doc
        \\## doc
        \\foobar = 12
    ;
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    const expected =
        \\foo = 1
        \\
        \\## doc
        \\## doc
        \\bar = 2
        \\
        \\## doc
        \\## doc
        \\foobar = 12
        \\
    ;
    try std.testing.expectEqualStrings(expected, result);
}
