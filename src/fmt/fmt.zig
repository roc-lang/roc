//! Formatting logic for Roc modules.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const base = @import("base");
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

/// Knobs for formatting that depend on the compiler doing it rather than on
/// the source being formatted.
pub const Options = struct {
    /// Version string of the compiler that is running. When it is a nightly
    /// newer than the one a header pins with `roc: "..."`, formatting rewrites
    /// that pin to name it—see `base.roc_version.shouldUpgrade`.
    ///
    /// Null leaves every pin exactly as written, which is what tools that
    /// format for inspection want: the snapshot tool, the playground and the
    /// formatter's own round-trip tests must not produce output that changes
    /// with whichever compiler built them.
    compiler_version: ?[]const u8 = null,
};

/// Report of the result of formatting Roc files including the count of successes, failures, and any files that need to be reformatted
pub const FormattingResult = struct {
    success: usize,
    failure: usize,
    /// Only relevant when using `roc fmt --check`
    unformatted_files: ?std.array_list.Managed([]const u8),

    pub fn deinit(self: *@This()) void {
        if (self.unformatted_files) |files| {
            files.deinit();
        }
    }
};

/// Parse diagnostics whose recovery AST is an explicit source migration that
/// the formatter owns. Every other parse diagnostic still blocks formatting so
/// a malformed file is never overwritten from a lossy recovery tree.
fn parseDiagnosticsPermitFormatting(diagnostics: []const AST.Diagnostic) bool {
    for (diagnostics) |diagnostic| {
        if (diagnostic.tag != .optional_field_mark_after_colon) return false;
    }
    return true;
}

/// Formats all roc files in the specified path.
/// Handles both single files and directories
/// Returns the number of files successfully formatted and that failed to format.
pub fn formatPath(gpa: std.mem.Allocator, arena: std.mem.Allocator, base_dir: std.Io.Dir, path: []const u8, check: bool, options: Options, io: std.Io, stderr: *std.Io.Writer) FormatPathError!FormattingResult {
    // TODO: update this to use the filesystem abstraction
    // When doing so, add a mock filesystem and some tests.

    var success_count: usize = 0;
    var failed_count: usize = 0;
    // Only used for `roc fmt --check`. If we aren't doing check, don't bother allocating
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
                if (formatFilePath(gpa, entry.dir, entry.basename, if (unformatted_files) |*to_reformat| to_reformat else null, options, io, stderr)) |_| {
                    success_count += 1;
                } else |err| switch (err) {
                    error.NotRocFile => {},
                    error.AccessDenied,
                    error.AntivirusInterference,
                    error.BadPathName,
                    error.Canceled,
                    error.DeviceBusy,
                    error.FileBusy,
                    error.FileLocksUnsupported,
                    error.FileNotFound,
                    error.FileSizeChangedDuringRead,
                    error.FileTooBig,
                    error.InputOutput,
                    error.IsDir,
                    error.LockViolation,
                    error.NameTooLong,
                    error.NetworkNotFound,
                    error.NoDevice,
                    error.NoSpaceLeft,
                    error.NotDir,
                    error.NotOpenForReading,
                    error.OutOfMemory,
                    error.ParsingFailed,
                    error.PathAlreadyExists,
                    error.PermissionDenied,
                    error.PipeBusy,
                    error.ProcessFdQuotaExceeded,
                    error.ReadFailed,
                    error.ReadOnlyFileSystem,
                    error.SymLinkLoop,
                    error.SystemFdQuotaExceeded,
                    error.SystemResources,
                    error.Unexpected,
                    error.Unseekable,
                    error.WouldBlock,
                    error.WriteFailed,
                    => {
                        try stderr.print("Failed to format {s}: {any}\n", .{ entry.path, err });
                        failed_count += 1;
                    },
                }
            }
        }
    } else |_| {
        if (formatFilePath(gpa, base_dir, path, if (unformatted_files) |*to_reformat| to_reformat else null, options, io, stderr)) |_| {
            success_count += 1;
        } else |err| switch (err) {
            error.NotRocFile => {},
            error.AccessDenied,
            error.AntivirusInterference,
            error.BadPathName,
            error.Canceled,
            error.DeviceBusy,
            error.FileBusy,
            error.FileLocksUnsupported,
            error.FileNotFound,
            error.FileSizeChangedDuringRead,
            error.FileTooBig,
            error.InputOutput,
            error.IsDir,
            error.LockViolation,
            error.NameTooLong,
            error.NetworkNotFound,
            error.NoDevice,
            error.NoSpaceLeft,
            error.NotDir,
            error.NotOpenForReading,
            error.OutOfMemory,
            error.ParsingFailed,
            error.PathAlreadyExists,
            error.PermissionDenied,
            error.PipeBusy,
            error.ProcessFdQuotaExceeded,
            error.ReadFailed,
            error.ReadOnlyFileSystem,
            error.SymLinkLoop,
            error.SystemFdQuotaExceeded,
            error.SystemResources,
            error.Unexpected,
            error.Unseekable,
            error.WouldBlock,
            error.WriteFailed,
            => {
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
pub fn formatFilePath(gpa: std.mem.Allocator, base_dir: std.Io.Dir, path: []const u8, unformatted_files: ?*std.array_list.Managed([]const u8), options: Options, io: std.Io, stderr: *std.Io.Writer) FormatFileError!void {
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

    // Explicit formatter migrations may consume their parser recovery AST.
    // Every other parsing problem is reported and leaves the file untouched.
    if (!parseDiagnosticsPermitFormatting(parse_ast.parse_diagnostics.items)) {
        try parse_ast.toSExprStr(gpa, &module_env.common, stderr);
        try printParseErrors(gpa, module_env.common.source, parse_ast.*, stderr);
        return error.ParsingFailed;
    }
    const migrates_optional_field_syntax = parse_ast.parse_diagnostics.items.len != 0;

    // Check if the file is formatted without actually formatting it
    if (unformatted_files != null) {
        var formatted: std.Io.Writer.Allocating = .init(gpa);
        defer formatted.deinit();
        try formatAstWithOptions(parse_ast.*, &formatted.writer, options);
        if (!std.mem.eql(u8, formatted.written(), module_env.common.source)) {
            try unformatted_files.?.append(path);
        }
    } else { // Otherwise actually format it
        const output_file = try base_dir.createFile(io, path, .{});
        defer output_file.close(io);
        var output_buffer: [4096]u8 = undefined;
        var output_writer = output_file.writer(io, &output_buffer);
        try formatAstWithOptions(parse_ast.*, &output_writer.interface, options);
        if (migrates_optional_field_syntax) {
            try stderr.print("Migrated legacy optional field syntax `:?` to `?:` in {s}.\n", .{path});
        }
    }
}

/// Format the contents of stdin and output the result to stdout
pub fn formatStdin(gpa: std.mem.Allocator, options: Options, io: std.Io, stdin: std.Io.File, stdout: std.Io.File, stderr: *std.Io.Writer) FormatStdinError!void {
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

    // Keep stdin behavior identical to file formatting: only explicit source
    // migrations may proceed through a parser recovery AST.
    if (!parseDiagnosticsPermitFormatting(parse_ast.parse_diagnostics.items)) {
        try parse_ast.toSExprStr(gpa, &module_env.common, stderr);
        try printParseErrors(gpa, module_env.common.source, parse_ast.*, stderr);
        return error.ParsingFailed;
    }
    const migrates_optional_field_syntax = parse_ast.parse_diagnostics.items.len != 0;

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = stdout.writer(io, &stdout_buffer);
    try formatAstWithOptions(parse_ast.*, &stdout_writer.interface, options);
    if (migrates_optional_field_syntax) {
        try stderr.writeAll("Migrated legacy optional field syntax `:?` to `?:` from stdin.\n");
    }
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

fn formatIRNode(ast: AST, writer: *std.Io.Writer, options: Options, formatter: *const fn (*Formatter) FormatAstError!void) FormatAstError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    var fmt = try Formatter.init(ast, writer, options);
    defer fmt.deinit();

    try formatter(&fmt);
    try fmt.flush();
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is a file.
/// Only returns an error if the underlying writer returns an error.
pub fn formatAst(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatAstWithOptions(ast, writer, .{});
}

/// `formatAst`, but for callers that know which compiler is running and so can
/// have a header's `roc` version pin brought up to date. See `Options`.
pub fn formatAstWithOptions(ast: AST, writer: *std.Io.Writer, options: Options) FormatAstError!void {
    return formatIRNode(ast, writer, options, Formatter.formatFile);
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is a header.
/// Only returns an error if the underlying writer returns an error.
pub fn formatHeader(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatIRNode(ast, writer, .{}, formatHeaderInner);
}

fn formatHeaderInner(fmt: *Formatter) FormatAstError!void {
    return fmt.formatHeader(@enumFromInt(fmt.ast.root_node_idx));
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is a statement.
/// Only returns an error if the underlying writer returns an error.
pub fn formatStatement(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatIRNode(ast, writer, .{}, formatStatementInner);
}

fn formatStatementInner(fmt: *Formatter) FormatAstError!void {
    return fmt.formatStatement(@enumFromInt(fmt.ast.root_node_idx));
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is an expression.
/// Only returns an error if the underlying writer returns an error.
pub fn formatExpr(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatIRNode(ast, writer, .{}, formatExprNode);
}

fn formatExprNode(fmt: *Formatter) FormatAstError!void {
    try fmt.formatExprDiscard(@enumFromInt(fmt.ast.root_node_idx));
}

/// Formatter for the roc parse ast.
const Formatter = struct {
    const TypeLayout = enum(u8) {
        unknown,
        compact,
        expanded,
    };

    /// A header's `roc` version pin that this run of the formatter is
    /// rewriting, rather than echoing back what the source says.
    const RocVersionUpgrade = struct {
        field: AST.RecordField.Idx,
        version: []const u8,
    };

    ast: AST,
    writer: *std.Io.Writer,
    /// Cached output layout for type annotations and their record fields.
    type_layouts: []TypeLayout,
    options: Options,
    /// Set while formatting a header whose version pin is out of date.
    roc_version_upgrade: ?RocVersionUpgrade = null,
    curr_indent: u32 = 0,
    flags: FormatFlags = .no_debug,
    // This starts true since beginning of file is considered a newline.
    has_newline: bool = true,
    has_multiline_string: bool = false,
    pending_spaces: usize = 0,

    /// Creates a new Formatter for the given parse IR.
    fn init(ast: AST, writer: *std.Io.Writer, options: Options) Allocator.Error!Formatter {
        const type_layouts = try ast.gpa.alloc(TypeLayout, ast.store.nodeCount());
        @memset(type_layouts, .unknown);

        return .{
            .ast = ast,
            .writer = writer,
            .type_layouts = type_layouts,
            .options = options,
        };
    }

    fn deinit(fmt: *Formatter) void {
        fmt.ast.gpa.free(fmt.type_layouts);
    }

    /// Deinits all data owned by the formatter object.
    fn flush(fmt: *Formatter) error{WriteFailed}!void {
        fmt.pending_spaces = 0;
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
            .app, .module, .package, .platform, .hosted => true,
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
                const name: ?[]const u8 = if (std.meta.activeTag(pattern) == .ident)
                    fmt.ast.resolve(pattern.ident.ident_tok)
                else
                    null;
                break :blk DefInfo{ .kind = .decl, .name = name };
            },
            .type_decl => DefInfo{ .kind = .type_decl, .name = null },
            .@"var",
            .expr,
            .crash,
            .dbg,
            .expect,
            .@"for",
            .@"while",
            .@"return",
            .@"break",
            .import,
            .file_import,
            .malformed,
            => null,
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
                    flushed = try fmt.flushCommentsBefore(i.target.start_tok);
                }
                if (!flushed) {
                    try fmt.push(' ');
                } else {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                }
                const path_result = try fmt.formatImportTarget(i.target);
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
                    flushed = false;
                    if (i.exposes.span.len > 0) {
                        flushed = try fmt.flushCommentsAfter(a);
                    }
                }
                const needs_exposing = i.exposes.span.len > 0;
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
                        // Imports store their exposing-list layout on the statement node.
                        const items_multiline = fmt.ast.store.getCollectionLayout(si) == .expanded or
                            fmt.nodesWillBeMultiline(AST.ExposedItem.Idx, items) or fmt.regionHasInteriorComment(i.region);
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
                if (d.kind == .where_alias) {
                    try fmt.formatTypeAnnoDiscard(d.anno);
                    try fmt.push('.');
                    try fmt.formatTypeHeader(d.header);
                    try fmt.pushAll(" :");
                    if (d.where) |w| {
                        if (multiline) {
                            try fmt.ensureNewline();
                            fmt.curr_indent += 1;
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        try fmt.formatWhereConstraint(w, multiline);
                    }
                    return;
                }
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
                    .where_alias => unreachable, // handled above
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
                    const where_multiline = multiline or fmt.collectionWillBeMultiline(AST.WhereClause.Idx, w);
                    if (where_multiline) {
                        try fmt.flushCommentsBeforeDiscard(anno_region.end);
                        try fmt.ensureNewline();
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    }
                    try fmt.formatWhereConstraint(w, where_multiline);
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
                    const where_multiline = multiline or fmt.collectionWillBeMultiline(AST.WhereClause.Idx, w);
                    if (where_multiline) {
                        try fmt.flushCommentsBeforeDiscard(anno_region.end);
                        try fmt.ensureNewline();
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    }
                    try fmt.formatWhereConstraint(w, where_multiline);
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

    /// Formats an explicit import target without whitespace around separators.
    const ModulePathResult = struct {
        last_tok: Token.Idx,
    };

    fn formatImportTarget(fmt: *Formatter, target: AST.ImportTarget) (Allocator.Error || error{WriteFailed})!ModulePathResult {
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }

        const tags = fmt.ast.tokens.tokens.items(.tag);
        const last_tok = target.lastToken();
        var tok = target.start_tok;
        while (tok <= last_tok) : (tok += 1) {
            const tag = tags[tok];
            if (tag == .NoSpaceDotUpperIdent or tag == .DotUpperIdent) {
                try fmt.push('.');
                try fmt.pushTokenText(tok);
            } else if (tag == .OpSlash) {
                try fmt.push('/');
            } else if (tag == .Dot) {
                try fmt.push('.');
            } else if (tag == .DoubleDot) {
                try fmt.pushAll("..");
            } else if (tag == .UpperIdent or tag == .LowerIdent) {
                try fmt.pushTokenText(tok);
            }
        }

        return .{ .last_tok = last_tok };
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

    fn formatCollection(fmt: *Formatter, region: AST.TokenizedRegion, layout: AST.CollectionLayout, braces: Braces, comptime T: type, items: []T, formatter: fn (*Formatter, T) FormatAstError!AST.TokenizedRegion) FormatAstError!void {
        const has_comment = fmt.regionHasInteriorComment(region);
        const multiline = layout == .expanded or fmt.nodesWillBeMultiline(T, items) or has_comment;
        const curr_indent = fmt.curr_indent;
        defer {
            fmt.curr_indent = curr_indent;
        }
        try fmt.push(braces.start());
        if (items.len == 0) {
            if (has_comment) {
                fmt.curr_indent += 1;
                try fmt.flushCommentsBeforeDiscard(fmt.regionClosingToken(region).?);
                fmt.curr_indent -= 1;
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
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

    fn formatApplyArgs(fmt: *Formatter, region: AST.TokenizedRegion, layout: AST.CollectionLayout, args: []AST.Expr.Idx) FormatAstError!void {
        if (try fmt.formatSingleMultilineCollectionArg(region, args)) {
            return;
        }

        try fmt.formatCollection(region, layout, .round, AST.Expr.Idx, args, Formatter.formatExpr);
    }

    fn formatSingleMultilineCollectionArg(fmt: *Formatter, region: AST.TokenizedRegion, args: []AST.Expr.Idx) FormatAstError!bool {
        if (!fmt.hasSingleMultilineCollectionArg(region, args)) {
            return false;
        }

        try fmt.push('(');
        try fmt.formatExprDiscard(args[0]);
        try fmt.push(')');
        return true;
    }

    fn hasSingleMultilineCollectionArg(fmt: *Formatter, region: AST.TokenizedRegion, args: []AST.Expr.Idx) bool {
        if (args.len != 1) {
            return false;
        }

        const arg_idx = args[0];
        const arg = fmt.ast.store.getExpr(arg_idx);
        const arg_tag = std.meta.activeTag(arg);
        if (arg_tag != .record and arg_tag != .list and arg_tag != .tuple) return false;

        if (!fmt.nodeWillBeMultiline(AST.Expr.Idx, arg_idx)) {
            return false;
        }

        const arg_region = fmt.nodeRegion(@intFromEnum(arg_idx));
        if (fmt.hasCommentBefore(arg_region.start)) {
            return false;
        }

        if (region.end > 0 and fmt.hasCommentBefore(region.end - 1)) {
            return false;
        }

        return true;
    }

    /// Format a record type annotation with an extension (e.g., { name: Str, ..ext } or { name: Str, .. })
    fn formatRecordWithExtension(fmt: *Formatter, fields_span: AST.AnnoRecordField.Span, ext: AST.TypeAnno.RecordExt, record_region: AST.TokenizedRegion, layout: AST.CollectionLayout) FormatAstError!void {
        const fields = fmt.ast.store.annoRecordFieldSlice(fields_span);
        const record_multiline = layout == .expanded or fmt.nodesWillBeMultiline(AST.AnnoRecordField.Idx, fields) or
            fmt.regionHasInteriorComment(record_region);
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
                const anno_region = fmt.nodeRegion(@intFromEnum(named.anno));
                if (try fmt.flushCommentsBefore(anno_region.start)) {
                    try fmt.pushIndent();
                }
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

    fn formatRecordFieldWithInfo(fmt: *Formatter, idx: AST.RecordField.Idx) FormatAstError!FormattedExpr {
        const field = fmt.ast.store.getRecordField(idx);
        var ends_with_multiline_string_line = false;
        try fmt.pushTokenText(field.name);
        if (fmt.roc_version_upgrade) |upgrade| {
            if (idx == upgrade.field) {
                // Write the running compiler's version rather than the stale
                // one in the source. Planning the upgrade already parsed that
                // version as a nightly tag, so it is alphanumerics and `-`
                // only and needs no escaping inside the quotes.
                try fmt.pushAll(": \"");
                try fmt.pushAll(upgrade.version);
                try fmt.push('"');
                return .{ .region = field.region, .ends_with_multiline_string_line = false };
            }
        }
        if (field.value) |v| {
            try fmt.pushAll(": ");
            const formatted_value = try fmt.formatExprWithInfo(v);
            ends_with_multiline_string_line = formatted_value.ends_with_multiline_string_line;
        }

        return .{
            .region = field.region,
            .ends_with_multiline_string_line = ends_with_multiline_string_line,
        };
    }

    fn formatRecordField(fmt: *Formatter, idx: AST.RecordField.Idx) FormatAstError!AST.TokenizedRegion {
        return (try fmt.formatRecordFieldWithInfo(idx)).region;
    }

    const ExprFormatBehavior = enum {
        normal,
        no_indent_on_access,
        no_additional_indent_on_access,
    };

    const ExprFormatContext = struct {
        behavior: ExprFormatBehavior = .normal,
        question_suffix_follows: bool = false,
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
        return formatExprInner(fmt, ei, .{});
    }

    fn adjustMultilineAccessIndent(fmt: *Formatter, format_behavior: ExprFormatBehavior) void {
        switch (format_behavior) {
            .normal => fmt.curr_indent += 1,
            .no_indent_on_access => {},
            .no_additional_indent_on_access => if (fmt.curr_indent > 0) {
                fmt.curr_indent -= 1;
            },
        }
    }

    fn continuePipeReceiverPostfix(fmt: *Formatter, token: Token.Idx, format_behavior: ExprFormatBehavior) error{WriteFailed}!void {
        const already_broke = try fmt.flushCommentsBefore(token);
        fmt.adjustMultilineAccessIndent(format_behavior);
        if (!already_broke) try fmt.ensureNewline();
        try fmt.pushIndent();
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
        const formatted = try fmt.formatExprInner(ei, .{ .behavior = format_behavior });
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

    fn formatParenthesizedExpr(fmt: *Formatter, region: ?AST.TokenizedRegion, expr_idx: AST.Expr.Idx, multiline: bool) FormatAstError!FormattedExpr {
        const curr_indent = fmt.curr_indent;
        defer fmt.curr_indent = curr_indent;

        try fmt.push('(');
        if (multiline) {
            fmt.curr_indent += 1;
            if (region != null) {
                const item_region = fmt.nodeRegion(@intFromEnum(expr_idx));
                try fmt.flushCommentsBeforeDiscard(item_region.start);
            }
            try fmt.ensureNewline();
            try fmt.pushIndent();
        }

        const formatted = try fmt.formatExprWithInfo(expr_idx);

        if (multiline) {
            if (region) |r| {
                try fmt.flushCommentsBeforeDiscard(r.end - 1);
            }
            fmt.curr_indent = curr_indent;
            try fmt.ensureNewline();
            try fmt.pushIndent();
        }
        try fmt.push(')');

        return formatted;
    }

    fn formatExprInner(fmt: *Formatter, ei: AST.Expr.Idx, format_context: ExprFormatContext) FormatAstError!FormattedExpr {
        const expr = fmt.ast.store.getExpr(ei);
        const region = fmt.nodeRegion(@intFromEnum(ei));
        var formatted = FormattedExpr{ .region = region };
        const multiline = fmt.nodeWillBeMultiline(AST.Expr.Idx, ei);
        const format_behavior = format_context.behavior;
        const indent_modifier: u32 = @intFromBool(format_behavior != .normal and fmt.curr_indent > 0);
        const curr_indent: u32 = fmt.curr_indent - indent_modifier;
        defer {
            fmt.curr_indent = curr_indent;
        }
        switch (expr) {
            .apply => |a| {
                try fmt.formatExprDiscard(a.@"fn");
                const fn_region = fmt.nodeRegion(@intFromEnum(a.@"fn"));
                const args_region = AST.TokenizedRegion{ .start = fn_region.end, .end = region.end };
                try fmt.formatApplyArgs(args_region, fmt.ast.store.getCollectionLayout(ei), fmt.ast.store.exprSlice(a.args));
            },
            .string_part => |s| {
                try fmt.pushTokenText(s.token);
            },
            .string => |s| {
                try fmt.push('"');
                for (fmt.ast.store.exprSlice(s.parts)) |idx| {
                    const e = fmt.ast.store.getExpr(idx);
                    if (std.meta.activeTag(e) == .string_part) {
                        try fmt.pushTokenText(e.string_part.token);
                    } else {
                        try fmt.formatStringInterpolation(idx);
                    }
                }
                try fmt.push('"');
            },
            .typed_string => |s| {
                try fmt.push('"');
                for (fmt.ast.store.exprSlice(s.parts)) |idx| {
                    const e = fmt.ast.store.getExpr(idx);
                    if (std.meta.activeTag(e) == .string_part) {
                        try fmt.pushTokenText(e.string_part.token);
                    } else {
                        try fmt.formatStringInterpolation(idx);
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
                    if (std.meta.activeTag(e) == .string_part) {
                        const str = e.string_part;
                        if (add_newline) {
                            // Comments could be located before the MultilineStringStart token, not the StringPart token
                            try fmt.flushCommentsBeforeDiscard(str.region.start - 1);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                            try fmt.pushAll("\\\\");
                        }

                        add_newline = true;
                        try fmt.pushTokenText(str.token);
                    } else {
                        add_newline = false;
                        try fmt.formatStringInterpolation(idx);
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
                    if (std.meta.activeTag(e) == .string_part) {
                        const str = e.string_part;
                        if (add_newline) {
                            // Comments could be located before the MultilineStringStart token, not the StringPart token
                            try fmt.flushCommentsBeforeDiscard(str.region.start - 1);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                            try fmt.pushAll("\\\\");
                        }

                        add_newline = true;
                        try fmt.pushTokenText(str.token);
                    } else {
                        add_newline = false;
                        try fmt.formatStringInterpolation(idx);
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
                const receiver_expr = fmt.ast.store.getExpr(fa.receiver);
                const flatten_pipe_receiver = receiver_expr == .arrow_call and multiline;
                const parenthesize_receiver = (receiver_expr == .arrow_call and !flatten_pipe_receiver) or fmt.exprIsNumericAccessReceiver(fa.receiver);
                const expand_parenthesized_receiver = receiver_expr == .arrow_call and
                    fmt.nodeWillBeMultiline(AST.Expr.Idx, fa.receiver);
                const receiver = if (parenthesize_receiver)
                    try fmt.formatParenthesizedExpr(null, fa.receiver, expand_parenthesized_receiver)
                else
                    try fmt.formatExprWithInfo(fa.receiver);

                const access_indent = fmt.curr_indent;
                const segments = fmt.ast.store.fieldAccessSegmentSlice(fa.segments);
                std.debug.assert(segments.len > 0);

                for (segments, 0..) |segment, i| {
                    // Nested field-access nodes used to restore indentation after
                    // every segment. Keep that behavior now that a path is flat.
                    fmt.curr_indent = access_indent;

                    if (i == 0 and flatten_pipe_receiver) {
                        // A multiline pipe receiver keeps its postfix chain on
                        // continuation lines rather than parenthesizing the
                        // pipe (issue 10517).
                        try fmt.continuePipeReceiverPostfix(segment.field_token, format_behavior);
                    } else if (!parenthesize_receiver or i > 0) {
                        const continued = i == 0 and try fmt.continueAfterMultilineStringLine(receiver);
                        if (!continued and multiline and try fmt.flushCommentsBefore(segment.field_token)) {
                            // Only the chain's final segment sits in the caller's
                            // context; interior segments always indent as .normal
                            // (they were nested nodes formatted as .normal when
                            // access paths were binary trees).
                            fmt.adjustMultilineAccessIndent(if (i == segments.len - 1) format_behavior else .normal);
                            try fmt.pushIndent();
                        }
                    }

                    switch (segment.mode) {
                        .required => try fmt.push('.'),
                        .optional => try fmt.pushAll(".?"),
                    }
                    try fmt.pushTokenText(segment.field_token);
                }
            },
            .method_call => |mc| {
                const left_expr = fmt.ast.store.getExpr(mc.receiver);
                const flatten_pipe_receiver = left_expr == .arrow_call and multiline;
                const parenthesize_receiver = (left_expr == .arrow_call and !flatten_pipe_receiver) or fmt.exprIsNumericAccessReceiver(mc.receiver);
                const expand_parenthesized_receiver = left_expr == .arrow_call and
                    fmt.nodeWillBeMultiline(AST.Expr.Idx, mc.receiver);
                const receiver = if (parenthesize_receiver)
                    try fmt.formatParenthesizedExpr(null, mc.receiver, expand_parenthesized_receiver)
                else
                    try fmt.formatExprWithInfo(mc.receiver);
                if (flatten_pipe_receiver) {
                    try fmt.continuePipeReceiverPostfix(mc.method_token, format_behavior);
                } else if (!parenthesize_receiver) {
                    const continued = try fmt.continueAfterMultilineStringLine(receiver);
                    if (!continued and multiline and try fmt.flushCommentsBefore(mc.method_token)) {
                        fmt.adjustMultilineAccessIndent(format_behavior);
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
                try fmt.formatApplyArgs(args_region, fmt.ast.store.getCollectionLayout(ei), fmt.ast.store.exprSlice(mc.args));
            },
            .arrow_call => |ld| {
                const left = try fmt.formatExprWithInfo(ld.left);
                if (multiline) {
                    const already_broke = try fmt.flushCommentsBefore(ld.operator);
                    if (format_behavior == .normal) {
                        fmt.curr_indent += 1;
                    }
                    if (!already_broke) {
                        try fmt.ensureNewline();
                    }
                    try fmt.pushIndent();
                } else {
                    _ = try fmt.continueAfterMultilineStringLine(left);
                    try fmt.push(' ');
                }
                try fmt.pushAll("|>");
                if (multiline and try fmt.flushCommentsAfter(ld.operator)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }

                const right_expr = fmt.ast.store.getExpr(ld.right);
                switch (right_expr) {
                    .ident, .tag => {
                        try fmt.formatExprInnerDiscard(ld.right, .no_indent_on_access);
                    },
                    .apply => |apply| {
                        const apply_fn_idx = apply.@"fn";
                        const apply_fn = fmt.ast.store.getExpr(apply_fn_idx);
                        const args = fmt.ast.store.exprSlice(apply.args);

                        // A direct empty argument list contributes no arguments
                        // beyond the piped value. Remove it unless a following
                        // `?` needs the call syntax to own the completed pipe, or
                        // doing so would expose another application as the RHS.
                        // (`value |> make()()` must remain distinct from
                        // `value |> make()`.)
                        if (args.len == 0 and apply_fn != .apply and !format_context.question_suffix_follows) {
                            const right_region = fmt.nodeRegion(@intFromEnum(ld.right));
                            const closing_token = right_region.end - 1;
                            if (fmt.hasCommentBefore(closing_token) and try fmt.flushCommentsBefore(closing_token)) {
                                try fmt.pushIndent();
                            }
                            const target_needs_parens = !fmt.exprCanStartPipeTargetUnparenthesized(apply_fn_idx);
                            if (target_needs_parens) try fmt.push('(');
                            try fmt.formatExprInnerDiscard(apply_fn_idx, .no_indent_on_access);
                            if (target_needs_parens) try fmt.push(')');
                        } else {
                            // Parenthesize a non-atomic callee before printing its
                            // argument list, preserving chains such as `fn()()`.
                            const fn_needs_parens = !fmt.exprCanStartPipeTargetUnparenthesized(apply_fn_idx);
                            if (fn_needs_parens) {
                                try fmt.push('(');
                                try fmt.formatExprInnerDiscard(apply_fn_idx, .no_indent_on_access);
                                try fmt.push(')');
                                const right_region = fmt.nodeRegion(@intFromEnum(ld.right));
                                const fn_region = fmt.nodeRegion(@intFromEnum(apply_fn_idx));
                                const args_region = AST.TokenizedRegion{ .start = fn_region.end, .end = right_region.end };
                                try fmt.formatApplyArgs(args_region, fmt.ast.store.getCollectionLayout(ld.right), args);
                            } else {
                                try fmt.formatExprInnerDiscard(ld.right, .no_indent_on_access);
                            }
                        }
                    },
                    .int,
                    .frac,
                    .typed_int,
                    .typed_frac,
                    .single_quote,
                    .string_part,
                    .string,
                    .multiline_string,
                    .typed_string,
                    .typed_multiline_string,
                    .list,
                    .tuple,
                    .record,
                    .lambda,
                    .record_updater,
                    .field_access,
                    .method_call,
                    .tuple_access,
                    .arrow_call,
                    .bin_op,
                    .suffix_single_question,
                    .unary_op,
                    .if_then_else,
                    .if_without_else,
                    .match,
                    .dbg,
                    .crash,
                    .record_builder,
                    .nominal_record,
                    .nominal_apply,
                    .ellipsis,
                    .@"break",
                    .@"return",
                    .block,
                    .for_expr,
                    .malformed,
                    => {
                        // A pipe target can start with a name or grouping
                        // parenthesis. Postfix chains rooted in a name are
                        // therefore safe without grouping; all other ASTs need
                        // parentheses so migrating `->` preserves valid syntax.
                        const needs_parens = !fmt.exprCanStartPipeTargetUnparenthesized(ld.right);
                        if (needs_parens) try fmt.push('(');
                        try fmt.formatExprInnerDiscard(ld.right, .no_indent_on_access);
                        if (needs_parens) try fmt.push(')');
                    },
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
                try fmt.formatCollection(region, fmt.ast.store.getCollectionLayout(ei), .square, AST.Expr.Idx, fmt.ast.store.exprSlice(l.items), Formatter.formatExpr);
            },
            .tuple => |t| {
                const items = fmt.ast.store.exprSlice(t.items);
                if (items.len == 1) {
                    const group_multiline = fmt.regionHasInteriorComment(t.region) or fmt.groupedExprWillBeMultiline(items[0]);
                    _ = try fmt.formatParenthesizedExpr(t.region, items[0], group_multiline);
                } else {
                    try fmt.formatCollection(region, fmt.ast.store.getCollectionLayout(ei), .round, AST.Expr.Idx, items, Formatter.formatExpr);
                }
            },
            .tuple_access => |ta| {
                const receiver_expr = fmt.ast.store.getExpr(ta.expr);
                const flatten_pipe_receiver = receiver_expr == .arrow_call and multiline;
                const parenthesize_receiver = (receiver_expr == .arrow_call and !flatten_pipe_receiver) or fmt.exprIsNumericAccessReceiver(ta.expr);
                if (parenthesize_receiver) try fmt.push('(');
                const target = try fmt.formatExprWithInfo(ta.expr);
                _ = try fmt.continueAfterMultilineStringLine(target);
                if (parenthesize_receiver) try fmt.push(')');
                if (flatten_pipe_receiver) {
                    try fmt.continuePipeReceiverPostfix(ta.elem_token, format_behavior);
                }
                // Get the element index from the token
                const token_text = fmt.ast.resolve(ta.elem_token);
                // Token includes leading dot (e.g., ".0")
                try fmt.pushAll(token_text);
            },
            .record => |r| {
                try fmt.push('{');

                const fields = fmt.ast.store.recordFieldSlice(r.fields);
                var has_extension = false;
                const record_multiline = fmt.ast.store.getCollectionLayout(ei) == .expanded or
                    fmt.nodesWillBeMultiline(AST.RecordField.Idx, fields) or fmt.regionHasInteriorComment(r.region);
                const empty_has_comment = r.ext == null and fields.len == 0 and fmt.regionHasInteriorComment(r.region);

                // Handle extension if present
                if (r.ext) |ext| {
                    if (record_multiline) {
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
                    if (record_multiline and fields.len > 0) {
                        try fmt.flushCommentsAfterDiscard(ext_region.end);
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                }

                // Format fields
                if (record_multiline and !has_extension and fields.len > 0) {
                    fmt.curr_indent += 1;
                    try fmt.flushCommentsAfterDiscard(r.region.start);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }

                for (fields, 0..) |field_idx, i| {
                    if (!record_multiline) {
                        try fmt.push(' ');
                    }
                    const formatted_field = try fmt.formatRecordFieldWithInfo(field_idx);
                    if (record_multiline) {
                        if (formatted_field.ends_with_multiline_string_line or fmt.has_multiline_string) {
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        try fmt.push(',');
                        try fmt.flushCommentsAfterDiscard(formatted_field.region.end);
                        if (i == fields.len - 1) {
                            fmt.curr_indent -= 1;
                        }
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    } else if (i < fields.len - 1) {
                        try fmt.pushAll(",");
                    }
                }

                if (empty_has_comment) {
                    fmt.curr_indent += 1;
                    try fmt.flushCommentsBeforeDiscard(fmt.regionClosingToken(r.region).?);
                    fmt.curr_indent -= 1;
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }

                if ((has_extension or fields.len > 0) and !record_multiline) {
                    try fmt.push(' ');
                }
                try fmt.push('}');
            },
            .lambda => |l| {
                const args = fmt.ast.store.patternSlice(l.args);
                const body_region = fmt.nodeRegion(@intFromEnum(l.body));
                const args_are_multiline = args.len > 0 and
                    (fmt.ast.store.getCollectionLayout(ei) == .expanded or
                        fmt.nodesWillBeMultiline(AST.Pattern.Idx, args) or
                        fmt.regionHasInteriorComment(.{ .start = l.region.start, .end = body_region.start }));
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
                const child_behavior: ExprFormatBehavior = switch (format_behavior) {
                    .normal => .normal,
                    .no_indent_on_access, .no_additional_indent_on_access => .no_additional_indent_on_access,
                };
                const child_expr = fmt.ast.store.getExpr(s.expr);
                const pipe_needs_parens = child_expr == .arrow_call and fmt.ast.store.getExpr(child_expr.arrow_call.right) != .apply;
                const body = if (pipe_needs_parens)
                    try fmt.formatParenthesizedExpr(null, s.expr, fmt.nodeWillBeMultiline(AST.Expr.Idx, s.expr))
                else
                    try fmt.formatExprInner(s.expr, .{
                        .behavior = child_behavior,
                        .question_suffix_follows = child_expr == .arrow_call,
                    });
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
            .crash => |c| {
                try fmt.pushAll("crash");
                const expr_node = fmt.nodeRegion(@intFromEnum(c.expr));
                if (multiline and try fmt.flushCommentsBefore(expr_node.start)) {
                    fmt.curr_indent += 1;
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatExprDiscard(c.expr);
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
                const record_multiline = fmt.ast.store.getCollectionLayout(ei) == .expanded or
                    fmt.nodesWillBeMultiline(AST.RecordField.Idx, fields) or fmt.regionHasInteriorComment(rb.region);

                try fmt.push('{');

                // Format fields like a regular record
                if (record_multiline and fields.len > 0) {
                    fmt.curr_indent += 1;
                    try fmt.flushCommentsAfterDiscard(rb.region.start);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }

                for (fields, 0..) |field_idx, i| {
                    if (!record_multiline) {
                        try fmt.push(' ');
                    }
                    const formatted_field = try fmt.formatRecordFieldWithInfo(field_idx);
                    const ends_with_multiline_string_line = formatted_field.ends_with_multiline_string_line or fmt.has_multiline_string;

                    if (i < fields.len - 1) {
                        if (ends_with_multiline_string_line) {
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        try fmt.push(',');
                        if (record_multiline) {
                            try fmt.flushCommentsAfterDiscard(formatted_field.region.end);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                    } else if (record_multiline) {
                        if (ends_with_multiline_string_line) {
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        try fmt.push(',');
                        try fmt.flushCommentsAfterDiscard(formatted_field.region.end);
                        fmt.curr_indent -= 1;
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                }

                if (fields.len > 0 and !record_multiline) {
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
                    .int,
                    .frac,
                    .typed_int,
                    .typed_frac,
                    .single_quote,
                    .string_part,
                    .string,
                    .multiline_string,
                    .typed_string,
                    .typed_multiline_string,
                    .list,
                    .tuple,
                    .record,
                    .lambda,
                    .apply,
                    .record_updater,
                    .field_access,
                    .method_call,
                    .tuple_access,
                    .arrow_call,
                    .bin_op,
                    .suffix_single_question,
                    .unary_op,
                    .if_then_else,
                    .if_without_else,
                    .match,
                    .dbg,
                    .crash,
                    .record_builder,
                    .nominal_record,
                    .nominal_apply,
                    .ellipsis,
                    .@"break",
                    .@"return",
                    .block,
                    .for_expr,
                    .malformed,
                    => {
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
                try fmt.formatCollection(args_region, fmt.ast.store.getCollectionLayout(ei), .round, AST.Expr.Idx, fmt.ast.store.exprSlice(na.args), Formatter.formatExpr);
            },
            .nominal_record => |nr| {
                const mapper = try fmt.formatExprWithInfo(nr.mapper);
                const mapper_region = fmt.nodeRegion(@intFromEnum(nr.mapper));
                if (fmt.hasCommentBefore(mapper_region.end)) {
                    if (try fmt.flushCommentsBefore(mapper_region.end)) {
                        try fmt.pushIndent();
                    }
                } else {
                    _ = try fmt.continueAfterMultilineStringLine(mapper);
                }
                try fmt.push('.');
                try fmt.formatExprDiscard(nr.backing);
            },
            .malformed => {
                // Output nothing for malformed node
            },
            .record_updater => {
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
                    // The `.` distinguishes nominal-value destructuring from an
                    // ordinary applied-tag pattern.
                    try fmt.push('.');
                }
                if (t.record_shorthand) {
                    const args = fmt.ast.store.patternSlice(t.args);
                    std.debug.assert(t.backing_value and args.len == 1);
                    try fmt.formatPatternDiscard(args[0]);
                } else if (t.backing_value or t.has_args) {
                    try fmt.formatCollection(region, fmt.ast.store.getCollectionLayout(pi), .round, AST.Pattern.Idx, fmt.ast.store.patternSlice(t.args), Formatter.formatPattern);
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
                try fmt.formatCollection(region, fmt.ast.store.getCollectionLayout(pi), .curly, AST.PatternRecordField.Idx, fmt.ast.store.patternRecordFieldSlice(r.fields), Formatter.formatPatternRecordField);
            },
            .list => |l| {
                region = l.region;
                try fmt.formatCollection(region, fmt.ast.store.getCollectionLayout(pi), .square, AST.Pattern.Idx, fmt.ast.store.patternSlice(l.patterns), Formatter.formatPattern);
            },
            .tuple => |t| {
                region = t.region;
                try fmt.formatCollection(region, fmt.ast.store.getCollectionLayout(pi), .round, AST.Pattern.Idx, fmt.ast.store.patternSlice(t.patterns), Formatter.formatPattern);
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
                for (fmt.ast.store.tokenSlice(i.qualifiers)) |qualifier| {
                    try fmt.pushTokenText(qualifier);
                    try fmt.push('.');
                }
                try fmt.pushTokenText(i.ident);
                if (i.as) |a| {
                    try fmt.pushAll(" as ");
                    try fmt.pushTokenText(a);
                }
            },
            .upper_ident => |i| {
                region = i.region;
                for (fmt.ast.store.tokenSlice(i.qualifiers)) |qualifier| {
                    try fmt.pushTokenText(qualifier);
                    try fmt.push('.');
                }
                try fmt.pushTokenText(i.ident);
                if (i.as) |a| {
                    try fmt.pushAll(" as ");
                    try fmt.pushTokenText(a);
                }
            },
            .upper_ident_star => |i| {
                region = i.region;
                for (fmt.ast.store.tokenSlice(i.qualifiers)) |qualifier| {
                    try fmt.pushTokenText(qualifier);
                    try fmt.push('.');
                }
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
        const has_comments = fmt.regionHasInteriorComment(span.region);
        const multiline = span.layout == .expanded or has_comments;
        if (entries.len == 0) {
            if (has_comments) {
                try fmt.push('{');
                fmt.curr_indent = base_indent + 1;
                try fmt.flushCommentsBeforeDiscard(fmt.regionClosingToken(span.region).?);
                fmt.curr_indent = base_indent;
                try fmt.ensureNewline();
                try fmt.pushIndent();
                try fmt.push('}');
                return;
            }
            try fmt.pushAll("{}");
            return;
        }
        if (!multiline) {
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
            const entry = fmt.ast.store.getSymbolMapEntry(entry_idx);
            try fmt.flushCommentsBeforeDiscard(entry.region.start);
            try fmt.ensureNewline();
            fmt.curr_indent = base_indent + 1;
            try fmt.pushIndent();
            try fmt.formatSymbolMapEntry(entry_idx);
            try fmt.push(',');
        }
        try fmt.flushCommentsBeforeDiscard(fmt.regionClosingToken(span.region).?);
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
        const value = fmt.ast.store.getTargetConfigValue(entry.value);
        return std.meta.activeTag(value) == .ident and value.ident == entry.name;
    }

    fn formatTargetConfigValue(fmt: *Formatter, value_idx: AST.TargetConfigValue.Idx) (Allocator.Error || error{WriteFailed})!void {
        const value = fmt.ast.store.getTargetConfigValue(value_idx);
        switch (value) {
            .int_literal, .tag_literal, .ident => |token| {
                try fmt.pushTokenText(token);
            },
            .string_literal => |maybe_token| {
                try fmt.push('"');
                if (maybe_token) |token| try fmt.pushTokenText(token);
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
            .string_literal => |maybe_token| {
                try fmt.push('"');
                if (maybe_token) |token| try fmt.pushTokenText(token);
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

    /// Which of the header's dependency-record entries pins a compiler version
    /// that this compiler should replace with its own, if any.
    fn plannedRocVersionUpgrade(fmt: *Formatter, header: AST.Header) ?RocVersionUpgrade {
        const current = fmt.options.compiler_version orelse return null;
        const field_idx = switch (header) {
            .app => |h| h.roc_version,
            .package => |h| h.roc_version,
            .platform => |h| h.roc_version,
            .module, .hosted, .type_module, .default_app, .malformed => null,
        } orelse return null;
        const pinned = fmt.ast.rocVersionText(field_idx) orelse return null;
        if (!base.roc_version.shouldUpgrade(pinned, current)) return null;
        return .{ .field = field_idx, .version = current };
    }

    fn formatHeader(fmt: *Formatter, hi: AST.Header.Idx) FormatAstError!void {
        const header = fmt.ast.store.getHeader(hi);
        const start_indent = fmt.curr_indent;
        fmt.roc_version_upgrade = fmt.plannedRocVersionUpgrade(header);
        defer {
            fmt.curr_indent = start_indent;
            fmt.roc_version_upgrade = null;
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
                    provides.layout,
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
                    const formatted_field = try fmt.formatRecordFieldWithInfo(field_idx);
                    Formatter.discardRegion(formatted_field.region);
                    if (packages_multiline) {
                        if (formatted_field.ends_with_multiline_string_line or fmt.has_multiline_string) {
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
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
                    exposes.layout,
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
                    exposes.layout,
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
                    exposes.layout,
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
                    packages.layout,
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
                    exposes.layout,
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
                    packages.layout,
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

                if (p.hosted.span.len > 0 or fmt.regionHasInteriorComment(p.hosted.region)) {
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
        } else if (fmt.regionHasInteriorComment(block.region)) {
            try fmt.push('{');
            fmt.curr_indent += 1;
            try fmt.flushCommentsBeforeDiscard(fmt.regionClosingToken(block.region).?);
            fmt.curr_indent -= 1;
            try fmt.ensureNewline();
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
            try fmt.formatCollection(h.region, fmt.ast.store.getCollectionLayout(header), .round, AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(h.args), Formatter.formatTypeAnno);
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
        const anno_region = fmt.nodeRegion(@intFromEnum(field.ty));
        const optional_mark_after_colon = if (field.optional_mark) |optional_mark| blk: {
            const marker_precedes_colon = fmt.ast.tokens.tokenTag(optional_mark + 1) == .OpColon;
            if (!marker_precedes_colon) {
                std.debug.assert(optional_mark > 0);
                std.debug.assert(fmt.ast.tokens.tokenTag(optional_mark - 1) == .OpColon);
            }
            break :blk !marker_precedes_colon;
        } else false;
        try fmt.pushTokenText(field.name);
        if (multiline and try fmt.flushCommentsAfter(field.name)) {
            fmt.curr_indent += 1;
            try fmt.pushIndent();
        } else {
            try fmt.push(' ');
        }
        // `name ?: Type`—the `?` before the colon marks the field
        // optional. Legacy `:?` sources format to `?:`. The marker remains its
        // own token boundary so a comment between `?` and `:` is preserved.
        if (field.optional_mark) |optional_mark| {
            try fmt.push('?');
            const preceding_token = if (optional_mark_after_colon) optional_mark - 1 else optional_mark;
            if (multiline and try fmt.flushCommentsAfter(preceding_token)) {
                fmt.curr_indent += 1;
                try fmt.pushIndent();
            }
        }
        try fmt.push(':');
        if (multiline and try fmt.flushCommentsBefore(anno_region.start)) {
            fmt.curr_indent += 1;
            try fmt.pushIndent();
        } else {
            try fmt.push(' ');
        }
        try fmt.formatTypeAnnoDiscard(field.ty);
        // `name : Type ?? default`—a defaulted field's value expression
        // is part of the annotation and must survive formatting (design.md
        // "Defaulted Fields").
        if (field.default_value) |default_idx| {
            const default_region = fmt.nodeRegion(@intFromEnum(default_idx));
            const default_mark = default_region.start - 1;
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(fmt.ast.tokens.tokenTag(default_mark) == .OpDoubleQuestion);
            }
            if (multiline and try fmt.flushCommentsBefore(default_mark)) {
                fmt.curr_indent += 1;
                try fmt.pushIndent();
            } else {
                try fmt.push(' ');
            }
            try fmt.pushAll("??");
            if (multiline and try fmt.flushCommentsAfter(default_mark)) {
                fmt.curr_indent += 1;
                try fmt.pushIndent();
            } else {
                try fmt.push(' ');
            }
            try fmt.formatExprDiscard(default_idx);
        }
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
                                try fmt.pushAll(if (c.effectful) "=>" else "->");
                            } else {
                                try fmt.pushAll(if (c.effectful) " =>" else " ->");
                            }
                        }
                    }
                } else if (c.effectful) {
                    try fmt.pushAll(" () =>");
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
                // Format as: a.WhereAlias
                try fmt.pushTokenText(c.var_tok);
                if (multiline and try fmt.flushCommentsAfter(c.var_tok)) {
                    fmt.curr_indent = start_indent;
                    try fmt.pushIndent();
                }
                try fmt.push('.');
                try fmt.formatTypeAnnoDiscard(c.alias);
            },
            .malformed => {
                // Output nothing for malformed node
            },
        }
    }

    fn formatTypeAnno(fmt: *Formatter, anno: AST.TypeAnno.Idx) FormatAstError!AST.TokenizedRegion {
        const a = fmt.ast.store.getTypeAnno(anno);
        const region = fmt.nodeRegion(@intFromEnum(anno));
        const multiline = fmt.nodeWillBeMultiline(AST.TypeAnno.Idx, anno);
        switch (a) {
            .apply => |app| {
                const slice = fmt.ast.store.typeAnnoSlice(app.args);
                const first = slice[0];
                try fmt.formatTypeAnnoDiscard(first);
                const rest = slice[1..];
                try fmt.formatCollection(app.region, fmt.ast.store.getCollectionLayout(anno), .round, AST.TypeAnno.Idx, rest, Formatter.formatTypeAnno);
            },
            .ty_var => |v| {
                try fmt.pushTokenText(v.tok);
            },
            .underscore_type_var => |utv| {
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
                try fmt.formatCollection(t.region, fmt.ast.store.getCollectionLayout(anno), .round, AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(t.annos), Formatter.formatTypeAnno);
            },
            .record => |r| {
                switch (r.ext) {
                    .closed => {
                        // Regular record without extension - use formatCollection
                        try fmt.formatCollection(region, fmt.ast.store.getCollectionLayout(anno), .curly, AST.AnnoRecordField.Idx, fmt.ast.store.annoRecordFieldSlice(r.fields), Formatter.formatAnnoRecordField);
                    },
                    .open, .named => {
                        // Record with extension - handle specially
                        try fmt.formatRecordWithExtension(r.fields, r.ext, region, fmt.ast.store.getCollectionLayout(anno));
                    },
                }
            },
            .tag_union => |t| {
                const tags = fmt.ast.store.typeAnnoSlice(t.tags);
                const is_open = t.ext != .closed;
                const tag_multiline = fmt.ast.store.getCollectionLayout(anno) == .expanded or
                    fmt.nodesWillBeMultiline(AST.TypeAnno.Idx, tags) or fmt.regionHasInteriorComment(region);
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
                    // Handle open tag unions.
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
                        switch (t.ext) {
                            .named => |named| {
                                const anno_region = fmt.nodeRegion(@intFromEnum(named.anno));
                                if (try fmt.flushCommentsBefore(anno_region.start)) {
                                    try fmt.pushIndent();
                                }
                                try fmt.formatTypeAnnoDiscard(named.anno);
                            },
                            .open => {},
                            .closed => unreachable,
                        }
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
            .underscore => {
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

    fn regionHasInteriorComment(fmt: *Formatter, region: AST.TokenizedRegion) bool {
        if (region.end <= region.start + 1) return false;
        var token = region.start + 1;
        while (token < region.end) : (token += 1) {
            if (fmt.hasCommentBefore(token)) return true;
        }
        return false;
    }

    fn regionClosingToken(fmt: *Formatter, region: AST.TokenizedRegion) ?Token.Idx {
        const tags = fmt.ast.tokens.tokens.items(.tag);

        if (region.end > region.start) {
            const previous = region.end - 1;
            if (Formatter.isClosingDelimiter(tags[previous])) {
                return previous;
            }
        }

        if (region.end < tags.len and Formatter.isClosingDelimiter(tags[region.end])) {
            return region.end;
        }

        return null;
    }

    fn isClosingDelimiter(tag: Token.Tag) bool {
        return tag == .CloseRound or tag == .CloseSquare or tag == .CloseCurly;
    }

    /// Like `flushCommentsBefore`, but ensures at least `min_leading_newlines` newlines
    /// are emitted before any comment or trailing content. Used to insert blank lines
    /// between top-level defs.
    fn flushCommentsBeforeMin(fmt: *Formatter, tokenIdx: Token.Idx, min_leading_newlines: u8) error{WriteFailed}!bool {
        const start = if (tokenIdx == 0) 0 else fmt.ast.tokens.resolve(tokenIdx - 1).end.offset;
        const end = fmt.ast.tokens.resolve(tokenIdx).start.offset;
        return fmt.flushComments(start, fmt.ast.env.source[start..end], min_leading_newlines);
    }

    fn flushCommentsAfter(fmt: *Formatter, tokenIdx: Token.Idx) error{WriteFailed}!bool {
        const start = fmt.ast.tokens.resolve(tokenIdx).end.offset;
        const end = fmt.ast.tokens.resolve(tokenIdx + 1).start.offset;
        return fmt.flushComments(start, fmt.ast.env.source[start..end], 0);
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
                if (!isShebang(start + i, comment_text) and comment_text.len > 0 and comment_text[0] != ' ' and comment_text[0] != '#') {
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

    /// A `#!` at the very start of a file is a shebang, so the formatter must leave
    /// it alone. Inserting the usual space after the `#` would stop the shell from
    /// recognizing it, breaking executable Roc scripts.
    /// `offset` is the absolute source offset of the comment's `#`, and
    /// `comment_text` is everything after that `#` up to the end of the line.
    fn isShebang(offset: usize, comment_text: []const u8) bool {
        return offset == 0 and comment_text.len > 0 and comment_text[0] == '!';
    }

    /// `start_offset` is the absolute source offset that `between_text` begins at.
    fn flushComments(fmt: *Formatter, start_offset: usize, between_text: []const u8, min_leading_newlines: u8) error{WriteFailed}!bool {
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
                if (!isShebang(start_offset + i, comment_text) and comment_text.len > 0 and comment_text[0] != ' ' and comment_text[0] != '#') {
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
        fmt.has_multiline_string = false;
        switch (c) {
            ' ' => {
                fmt.pending_spaces += 1;
                fmt.has_newline = false;
            },
            '\n' => {
                fmt.pending_spaces = 0;
                fmt.has_newline = true;
                try fmt.writer.writeByte(c);
            },
            '\t' => {
                try fmt.flushPendingSpaces();
                try fmt.writer.writeByte(c);
            },
            else => {
                try fmt.flushPendingSpaces();
                fmt.has_newline = false;
                try fmt.writer.writeByte(c);
            },
        }
    }

    fn pushAll(fmt: *Formatter, str: []const u8) error{WriteFailed}!void {
        if (str.len == 0) {
            return;
        }

        fmt.has_multiline_string = false;
        var run_start: usize = 0;
        var i: usize = 0;
        while (i < str.len) {
            switch (str[i]) {
                ' ' => {
                    if (run_start < i) {
                        try fmt.writeStructuralRun(str[run_start..i]);
                    }

                    const spaces_start = i;
                    while (i < str.len and str[i] == ' ') : (i += 1) {}
                    fmt.pending_spaces += i - spaces_start;
                    fmt.has_newline = false;
                    run_start = i;
                },
                '\n' => {
                    if (run_start < i) {
                        try fmt.writeStructuralRun(str[run_start..i]);
                    }

                    fmt.pending_spaces = 0;
                    fmt.has_newline = true;
                    try fmt.writer.writeByte('\n');
                    i += 1;
                    run_start = i;
                },
                else => i += 1,
            }
        }

        if (run_start < str.len) {
            try fmt.writeStructuralRun(str[run_start..]);
        }
    }

    fn writeStructuralRun(fmt: *Formatter, str: []const u8) error{WriteFailed}!void {
        try fmt.flushPendingSpaces();

        const all_tabs = for (str) |c| {
            if (c != '\t') break false;
        } else true;
        if (!all_tabs) {
            fmt.has_newline = false;
        }

        try fmt.writer.writeAll(str);
    }

    fn flushPendingSpaces(fmt: *Formatter) error{WriteFailed}!void {
        if (fmt.pending_spaces == 0) return;

        try fmt.writer.splatByteAll(' ', fmt.pending_spaces);
        fmt.pending_spaces = 0;
    }

    fn pushVerbatim(fmt: *Formatter, str: []const u8) error{WriteFailed}!void {
        if (str.len == 0) return;

        try fmt.flushPendingSpaces();

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
        if (tag == .NoSpaceDotLowerIdent or tag == .NoSpaceDotUpperIdent or tag == .DotLowerIdent or tag == .DotUpperIdent) {
            start += 1;
        } else if (tag == .NoSpaceDotQuestionLowerIdent or tag == .DotQuestionLowerIdent) {
            start += 2;
        }

        const text = fmt.ast.env.source[start..region.end.offset];
        try fmt.pushVerbatim(text);
    }

    fn exprIsNumericAccessReceiver(fmt: *Formatter, expr_idx: AST.Expr.Idx) bool {
        const expr = fmt.ast.store.getExpr(expr_idx);
        const tag = std.meta.activeTag(expr);
        if (tag == .int or tag == .frac or tag == .typed_int or tag == .typed_frac) return true;
        if (tag == .unary_op) return fmt.exprIsNumericAccessReceiver(expr.unary_op.expr);
        return false;
    }

    fn exprCanStartPipeTargetUnparenthesized(fmt: *Formatter, expr_idx: AST.Expr.Idx) bool {
        return switch (fmt.ast.store.getExpr(expr_idx)) {
            .ident, .tag => true,
            .apply => |apply| fmt.exprCanStartPipeTargetUnparenthesized(apply.@"fn"),
            .field_access => |access| fmt.exprCanStartPipeTargetUnparenthesized(access.receiver),
            .method_call => |call| fmt.exprCanStartPipeTargetUnparenthesized(call.receiver),
            .tuple_access => |access| fmt.exprCanStartPipeTargetUnparenthesized(access.expr),
            .nominal_apply => |apply| fmt.exprCanStartPipeTargetUnparenthesized(apply.mapper),
            .suffix_single_question => |suffix| fmt.exprCanStartPipeTargetUnparenthesized(suffix.expr),
            .int,
            .frac,
            .typed_int,
            .typed_frac,
            .single_quote,
            .string_part,
            .string,
            .multiline_string,
            .typed_string,
            .typed_multiline_string,
            .list,
            .tuple,
            .record,
            .lambda,
            .record_updater,
            .arrow_call,
            .bin_op,
            .unary_op,
            .if_then_else,
            .if_without_else,
            .match,
            .dbg,
            .crash,
            .record_builder,
            .nominal_record,
            .ellipsis,
            .block,
            .for_expr,
            .@"break",
            .@"return",
            .malformed,
            => false,
        };
    }

    fn groupedExprWillBeMultiline(fmt: *Formatter, expr_idx: AST.Expr.Idx) bool {
        const expr = fmt.ast.store.getExpr(expr_idx);
        if (expr == .method_call) {
            const method = expr.method_call;
            const receiver_region = fmt.nodeRegion(@intFromEnum(method.receiver));
            if (fmt.ast.regionIsMultiline(.{ .start = receiver_region.start, .end = method.method_token + 1 })) {
                return true;
            }
        }

        const expr_tag = std.meta.activeTag(expr);
        const owns_collection = expr_tag == .list or expr_tag == .tuple or expr_tag == .record or
            expr_tag == .record_builder or expr_tag == .apply or expr_tag == .method_call or
            expr_tag == .nominal_apply or expr_tag == .lambda;
        if (owns_collection and fmt.regionHasInteriorComment(expr.to_tokenized_region())) return true;

        return switch (expr) {
            .block, .multiline_string, .typed_multiline_string => true,
            .list => |l| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(l.items)),
            .tuple => |t| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(t.items)),
            .apply => |a| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                fmt.groupedExprWillBeMultiline(a.@"fn") or
                fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(a.args)),
            .bin_op => |b| fmt.groupedExprWillBeMultiline(b.left) or fmt.groupedExprWillBeMultiline(b.right),
            .record => |r| blk: {
                if (fmt.ast.store.getCollectionLayout(expr_idx) == .expanded) break :blk true;
                if (r.ext) |ext| {
                    if (fmt.groupedExprWillBeMultiline(ext)) break :blk true;
                }
                break :blk fmt.nodesWillBeMultiline(AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(r.fields));
            },
            .record_builder => |rb| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                fmt.nodesWillBeMultiline(AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(rb.fields)),
            .nominal_record => |nr| fmt.groupedExprWillBeMultiline(nr.mapper) or fmt.groupedExprWillBeMultiline(nr.backing),
            .suffix_single_question => |s| fmt.groupedExprWillBeMultiline(s.expr),
            .tuple_access => |t| fmt.groupedExprWillBeMultiline(t.expr),
            .unary_op => |u| fmt.groupedExprWillBeMultiline(u.expr),
            .field_access => |f| (fmt.ast.store.getExpr(f.receiver) == .arrow_call and fmt.nodeWillBeMultiline(AST.Expr.Idx, f.receiver)) or
                fmt.groupedExprWillBeMultiline(f.receiver),
            .method_call => |m| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                (fmt.ast.store.getExpr(m.receiver) == .arrow_call and fmt.nodeWillBeMultiline(AST.Expr.Idx, m.receiver)) or
                fmt.groupedExprWillBeMultiline(m.receiver) or
                fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(m.args)),
            .nominal_apply => |na| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                fmt.groupedExprWillBeMultiline(na.mapper) or
                fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(na.args)),
            .lambda => |l| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                fmt.groupedExprWillBeMultiline(l.body) or
                fmt.nodesWillBeMultiline(AST.Pattern.Idx, fmt.ast.store.patternSlice(l.args)),
            .if_then_else => |i| fmt.groupedExprWillBeMultiline(i.condition) or
                fmt.groupedExprWillBeMultiline(i.then) or
                fmt.groupedExprWillBeMultiline(i.@"else"),
            .if_without_else => |i| fmt.groupedExprWillBeMultiline(i.condition) or fmt.groupedExprWillBeMultiline(i.then),
            .arrow_call => fmt.nodeWillBeMultiline(AST.Expr.Idx, expr_idx),
            .dbg => |d| fmt.groupedExprWillBeMultiline(d.expr),
            .crash => |c| fmt.groupedExprWillBeMultiline(c.expr),
            .@"return" => |r| fmt.groupedExprWillBeMultiline(r.expr),
            .for_expr => |f| fmt.groupedExprWillBeMultiline(f.expr) or fmt.groupedExprWillBeMultiline(f.body),
            .int,
            .frac,
            .typed_int,
            .typed_frac,
            .single_quote,
            .string_part,
            .string,
            .typed_string,
            .tag,
            .record_updater,
            .match,
            .ident,
            .ellipsis,
            .@"break",
            .malformed,
            => false,
        };
    }

    fn nodeWillBeMultiline(fmt: *Formatter, comptime T: type, item: T) bool {
        if (T == AST.Expr.Idx) {
            const expr = fmt.ast.store.getExpr(item);
            if (expr == .method_call) {
                const method = expr.method_call;
                const receiver_region = fmt.nodeRegion(@intFromEnum(method.receiver));
                if (fmt.ast.regionIsMultiline(.{ .start = receiver_region.start, .end = method.method_token + 1 })) {
                    return true;
                }
            }
            const expr_tag = std.meta.activeTag(expr);
            const owns_collection = expr_tag == .list or expr_tag == .tuple or expr_tag == .record or
                expr_tag == .record_builder or expr_tag == .apply or expr_tag == .method_call or
                expr_tag == .nominal_apply or expr_tag == .lambda;
            if (owns_collection and fmt.regionHasInteriorComment(expr.to_tokenized_region())) return true;
            if (!owns_collection and fmt.ast.regionIsMultiline(expr.to_tokenized_region())) {
                return true;
            }

            switch (expr) {
                .block => return true,
                .multiline_string, .typed_multiline_string => return true,
                .list => |l| {
                    return fmt.ast.store.getCollectionLayout(item) == .expanded or
                        fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(l.items));
                },
                .tuple => |t| {
                    return fmt.ast.store.getCollectionLayout(item) == .expanded or
                        fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(t.items));
                },
                .apply => |a| {
                    if (fmt.ast.store.getCollectionLayout(item) == .expanded) return true;
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
                    if (fmt.ast.store.getCollectionLayout(item) == .expanded) return true;
                    if (r.ext) |ext| {
                        if (fmt.nodeWillBeMultiline(AST.Expr.Idx, ext)) {
                            return true;
                        }
                    }

                    return fmt.nodesWillBeMultiline(AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(r.fields));
                },
                .record_builder => |rb| {
                    return fmt.ast.store.getCollectionLayout(item) == .expanded or
                        fmt.nodesWillBeMultiline(AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(rb.fields));
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
                .tuple_access => |t| {
                    return fmt.nodeWillBeMultiline(AST.Expr.Idx, t.expr);
                },
                .unary_op => |u| {
                    return fmt.nodeWillBeMultiline(AST.Expr.Idx, u.expr);
                },
                .field_access => |f| {
                    return fmt.nodeWillBeMultiline(AST.Expr.Idx, f.receiver);
                },
                .method_call => |m| {
                    if (fmt.ast.store.getCollectionLayout(item) == .expanded) return true;
                    if (fmt.nodeWillBeMultiline(AST.Expr.Idx, m.receiver)) {
                        return true;
                    }

                    return fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(m.args));
                },
                .nominal_apply => |na| {
                    if (fmt.ast.store.getCollectionLayout(item) == .expanded) return true;
                    if (fmt.nodeWillBeMultiline(AST.Expr.Idx, na.mapper)) {
                        return true;
                    }

                    return fmt.nodesWillBeMultiline(AST.Expr.Idx, fmt.ast.store.exprSlice(na.args));
                },
                .lambda => |l| {
                    if (fmt.ast.store.getCollectionLayout(item) == .expanded) return true;
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
                .int,
                .frac,
                .typed_int,
                .typed_frac,
                .single_quote,
                .string_part,
                .string,
                .typed_string,
                .tag,
                .record_updater,
                .match,
                .ident,
                .dbg,
                .crash,
                .ellipsis,
                .@"break",
                .@"return",
                .malformed,
                => return false,
            }
        }
        if (T == AST.Pattern.Idx) {
            const pattern = fmt.ast.store.getPattern(item);
            const pattern_has_comment = fmt.regionHasInteriorComment(pattern.to_tokenized_region());
            return switch (pattern) {
                .tag => |t| t.has_args and (pattern_has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                    fmt.nodesWillBeMultiline(AST.Pattern.Idx, fmt.ast.store.patternSlice(t.args))),
                .record => |r| pattern_has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                    fmt.nodesWillBeMultiline(AST.PatternRecordField.Idx, fmt.ast.store.patternRecordFieldSlice(r.fields)),
                .list => |l| pattern_has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                    fmt.nodesWillBeMultiline(AST.Pattern.Idx, fmt.ast.store.patternSlice(l.patterns)),
                .tuple => |t| pattern_has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                    fmt.nodesWillBeMultiline(AST.Pattern.Idx, fmt.ast.store.patternSlice(t.patterns)),
                .ident,
                .var_ident,
                .int,
                .frac,
                .typed_int,
                .typed_frac,
                .string,
                .single_quote,
                .list_rest,
                .underscore,
                .alternatives,
                .as,
                .malformed,
                => fmt.ast.regionIsMultiline(pattern.to_tokenized_region()),
            };
        }
        if (T == AST.PatternRecordField.Idx) {
            const patternRecordField = fmt.ast.store.getPatternRecordField(item);
            if (fmt.regionHasInteriorComment(patternRecordField.region)) {
                return true;
            }

            if (patternRecordField.value) |value| {
                if (fmt.nodeWillBeMultiline(AST.Pattern.Idx, value)) {
                    return true;
                }
            }

            return false;
        }
        if (T == AST.ExposedItem.Idx) {
            const exposedItem = fmt.ast.store.getExposedItem(item);
            return fmt.ast.regionIsMultiline(exposedItem.to_tokenized_region());
        }
        if (T == AST.RecordField.Idx) {
            const recordField = fmt.ast.store.getRecordField(item);
            if (fmt.regionHasInteriorComment(recordField.region)) {
                return true;
            }

            if (recordField.value) |value| {
                if (fmt.nodeWillBeMultiline(AST.Expr.Idx, value)) {
                    return true;
                }
            }

            return false;
        }
        if (T == AST.TypeAnno.Idx) {
            return fmt.typeAnnoWillBeMultiline(item);
        }
        if (T == AST.AnnoRecordField.Idx) {
            return fmt.annoRecordFieldWillBeMultiline(item);
        }
        if (T == AST.WhereClause.Idx) {
            const whereClause = fmt.ast.store.getWhereClause(item);
            return fmt.ast.regionIsMultiline(whereClause.to_tokenized_region());
        }
        if (T == AST.Statement.Idx) {
            const statement = fmt.ast.store.getStatement(item);
            if (fmt.ast.regionIsMultiline(statement.to_tokenized_region())) {
                return true;
            }

            if (std.meta.activeTag(statement) == .expr) {
                return fmt.nodeWillBeMultiline(AST.Expr.Idx, statement.expr.expr);
            }
            return false;
        }
        if (T == AST.TypeHeader.Idx) {
            const typeHeader = fmt.ast.store.getTypeHeader(item) catch return false;
            return fmt.ast.store.getCollectionLayout(item) == .expanded or
                fmt.nodesWillBeMultiline(AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(typeHeader.args));
        }
        if (T == AST.Header.Idx) {
            const header = fmt.ast.store.getHeader(item);
            if (fmt.regionHasInteriorComment(header.to_tokenized_region())) return true;
            switch (header) {
                .app => |a| return fmt.collectionWillBeMultiline(AST.ExposedItem.Idx, a.provides) or
                    fmt.collectionWillBeMultiline(AST.RecordField.Idx, a.packages),
                .module => |m| return fmt.collectionWillBeMultiline(AST.ExposedItem.Idx, m.exposes),
                .hosted => |h| return fmt.collectionWillBeMultiline(AST.ExposedItem.Idx, h.exposes),
                .package => |p| {
                    if (fmt.collectionWillBeMultiline(AST.ExposedItem.Idx, p.exposes)) {
                        return true;
                    }

                    return fmt.collectionWillBeMultiline(AST.RecordField.Idx, p.packages);
                },
                .platform => return true,
                .type_module, .default_app, .malformed => return false,
            }
        }
        return false;
    }

    fn typeAnnoWillBeMultiline(fmt: *Formatter, item: AST.TypeAnno.Idx) bool {
        const cache_entry = &fmt.type_layouts[@intFromEnum(item)];
        switch (cache_entry.*) {
            .compact => return false,
            .expanded => return true,
            .unknown => {},
        }

        const type_anno = fmt.ast.store.getTypeAnno(item);
        const has_comment = fmt.regionHasInteriorComment(type_anno.to_tokenized_region());
        const multiline = switch (type_anno) {
            .apply => |apply| has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                fmt.nodesWillBeMultiline(AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(apply.args)),
            .tuple => |tuple| has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                fmt.nodesWillBeMultiline(AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(tuple.annos)),
            .record => |record| has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                fmt.nodesWillBeMultiline(AST.AnnoRecordField.Idx, fmt.ast.store.annoRecordFieldSlice(record.fields)),
            .tag_union => |tag_union| has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                fmt.nodesWillBeMultiline(AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(tag_union.tags)),
            .@"fn" => |function| has_comment or
                fmt.nodesWillBeMultiline(AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(function.args)) or
                fmt.typeAnnoWillBeMultiline(function.ret),
            .parens => |parens| has_comment or fmt.ast.regionIsMultiline(type_anno.to_tokenized_region()) or
                fmt.typeAnnoWillBeMultiline(parens.anno),
            .ty_var, .underscore_type_var, .underscore, .ty, .malformed => fmt.ast.regionIsMultiline(type_anno.to_tokenized_region()),
        };

        cache_entry.* = if (multiline) .expanded else .compact;
        return multiline;
    }

    fn annoRecordFieldWillBeMultiline(fmt: *Formatter, item: AST.AnnoRecordField.Idx) bool {
        const cache_entry = &fmt.type_layouts[@intFromEnum(item)];
        switch (cache_entry.*) {
            .compact => return false,
            .expanded => return true,
            .unknown => {},
        }

        const field = fmt.ast.store.getAnnoRecordField(item) catch {
            cache_entry.* = .compact;
            return false;
        };
        const multiline = fmt.regionHasInteriorComment(field.region) or fmt.typeAnnoWillBeMultiline(field.ty);

        cache_entry.* = if (multiline) .expanded else .compact;
        return multiline;
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
        if (collection.layout == .expanded or fmt.regionHasInteriorComment(collection.region)) {
            return true;
        }

        if (T == AST.RecordField.Idx) {
            const record_field_slice = fmt.ast.store.recordFieldSlice(.{ .span = collection.span });
            return fmt.nodesWillBeMultiline(AST.RecordField.Idx, record_field_slice);
        }
        if (T == AST.ExposedItem.Idx) {
            const exposed_item_slice = fmt.ast.store.exposedItemSlice(.{ .span = collection.span });
            return fmt.nodesWillBeMultiline(AST.ExposedItem.Idx, exposed_item_slice);
        }
        if (T == AST.WhereClause.Idx) {
            const where_clause_slice = fmt.ast.store.whereClauseSlice(.{ .span = collection.span });
            return fmt.nodesWillBeMultiline(AST.WhereClause.Idx, where_clause_slice);
        }
        return false;
    }
};

/// Asserts a module when formatted twice in a row results in the same final output.
/// Returns that final output.
/// Like `moduleFmtsStable`, but the input is expected to produce exactly
/// `expected_diags` recoverable parse diagnostics (legacy-syntax inputs);
/// the formatted output must still reparse clean and stable.
pub fn moduleFmtsStableWithDiags(gpa: std.mem.Allocator, input: []const u8, debug: bool, expected_diags: usize) FormatTestError![]const u8 {
    if (debug) {
        std.debug.print("Original:\n==========\n{s}\n==========\n\n", .{input});
    }
    const formatted = parseAndFmtCountingDiags(gpa, input, expected_diags) catch |err| return err;
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

/// Assert that formatting `input` as a module is stable (formatting the
/// formatted output changes nothing) and return the formatted source.
pub fn moduleFmtsStable(gpa: std.mem.Allocator, input: []const u8, debug: bool) FormatTestError![]const u8 {
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

fn parseAndFmtCountingDiags(gpa: std.mem.Allocator, input: []const u8, expected_diags: usize) FormatParseError![]const u8 {
    var module_env = try ModuleEnv.init(gpa, input);
    defer module_env.deinit();

    const parse_ast = try parse.file(gpa, &module_env.common);
    defer parse_ast.deinit();

    std.testing.expectEqual(expected_diags, parse_ast.parse_diagnostics.items.len) catch {
        return error.ParseFailed;
    };

    var result: std.Io.Writer.Allocating = .init(gpa);
    defer result.deinit();
    try formatAst(parse_ast.*, &result.writer);
    return result.toOwnedSlice();
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

test "issue 10480: package qualifier preserved in exposed aliased imports" {
    // Repro for https://github.com/roc-lang/roc/issues/10480
    const result = try moduleFmtsStable(std.testing.allocator, "module[o as n,F.s as I]", false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("module [o as n, F.s as I]\n", result);
}

test "issue 10431: wrapped declaration has no trailing whitespace" {
    // Repro for https://github.com/roc-lang/roc/issues/10431
    const result = try moduleFmtsStable(std.testing.allocator,
        \\x =
        \\    1
    , false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("x =\n\t1\n", result);
}

test "issue 10191: leading newline before function parameter formatting is stable" {
    // Repro for https://github.com/roc-lang/roc/issues/10191
    const result = try moduleFmtsStable(std.testing.allocator, "\nm : (S) -> r\n", false);
    defer std.testing.allocator.free(result);
}

test "string token text preserves significant trailing spaces" {
    const regular = try moduleFmtsStable(std.testing.allocator, "regular=\"value \"", false);
    defer std.testing.allocator.free(regular);
    try std.testing.expectEqualStrings("regular = \"value \"\n", regular);

    const multiline = try moduleFmtsStable(std.testing.allocator, "multiline = \\\\first  \n" ++
        "\\\\second", false);
    defer std.testing.allocator.free(multiline);
    try std.testing.expectEqualStrings("multiline = \\\\first  \n\t\\\\second\n", multiline);
}

// Issue #8851: Formatter idempotence tests for arrow call with field access
// These test cases verify that formatting is stable (idempotent) - formatting twice
// produces the same output as formatting once.

test "function type expands when its return type is multiline" {
    const result = try moduleFmtsStable(
        std.testing.allocator,
        "r:(),(->c),(->d)->(c,)",
        false,
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("r : (),\n" ++
        "(() -> c),\n" ++
        "(() -> d) -> (\n" ++
        "\tc,\n" ++
        ")\n", result);
}

test "issue 10335: where clause formatting is idempotent" {
    // Repro for https://github.com/roc-lang/roc/issues/10335
    const result = try moduleFmtsStable(std.testing.allocator, "g:e->e where[e.B,]h=||{{([])}}", false);
    defer std.testing.allocator.free(result);
}

test "issue 10140: nested record function type formatting is idempotent" {
    // Repro for https://github.com/roc-lang/roc/issues/10140
    const result = try moduleFmtsStable(std.testing.allocator,
        \\p:{e:
        \\{n:U
        \\}=>U}=>r
    , false);
    defer std.testing.allocator.free(result);
}

test "optional record type fields format as a leading marker" {
    const result = try moduleFmtsStable(
        std.testing.allocator,
        "value:{x:U32,y?:U32,z ? : U32}",
        false,
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "value : { x : U32, y ?: U32, z ?: U32 }\n",
        result,
    );
}

test "defaulted record type fields keep their default through formatting" {
    // Review H1: the formatter must never drop `?? default`—it is
    // semantics (construction sites that omit the field depend on it).
    const result = try moduleFmtsStable(
        std.testing.allocator,
        "value:{count:U8??10,name:Str ?? \"hi\"}",
        false,
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "value : { count : U8 ?? 10, name : Str ?? \"hi\" }\n",
        result,
    );
}

test "defaulted record field preserves a comment after the default marker" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\value : {
        \\    a : U8 ?? # why
        \\        10,
        \\}
    , false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, "# why"));
}

test "optional mark with a trailing comment formats idempotently" {
    // Review H2: trivia between `?:` and the type is flushed exactly once,
    // so format(format(x)) == format(x). moduleFmtsStable asserts stability.
    const result = try moduleFmtsStable(
        std.testing.allocator,
        "i : { a ?: # after mark\n\tU8 }",
        false,
    );
    defer std.testing.allocator.free(result);
    try std.testing.expect(std.mem.count(u8, result, "# after mark") == 1);
}

test "optional record field preserves a comment before the colon" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\value : {
        \\    a ? # why
        \\        : U8,
        \\}
    , false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, "# why"));
}

test "legacy optional marker after the colon formats to the leading form" {
    // `:?` (and spaced `: ?`) recover as optional fields with a parse
    // diagnostic pointing at `?:`; the formatter canonicalizes them.
    const result = try moduleFmtsStableWithDiags(
        std.testing.allocator,
        "value:{x:U32,y:?U32,z : ? U32}",
        false,
        2,
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "value : { x : U32, y ?: U32, z ?: U32 }\n",
        result,
    );
}

test "legacy optional marker preserves a trailing comment once" {
    const result = try moduleFmtsStableWithDiags(
        std.testing.allocator,
        "value : {\n    a :? # keep me\n        U8,\n}",
        false,
        1,
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "value : {\n\ta ?: # keep me\n\t\tU8,\n}\n",
        result,
    );
}

test "legacy optional marker preserves a comment between colon and marker" {
    const result = try moduleFmtsStableWithDiags(
        std.testing.allocator,
        "value : {\n    a : # keep me\n        ? U8,\n}",
        false,
        1,
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "value : {\n\ta ? # keep me\n\t\t: U8,\n}\n",
        result,
    );
}

test "formatFilePath migrates a legacy optional field marker" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const input = "v : { a :? U8 }";
    const file = try tmp.dir.createFile(io, "legacy.roc", .{});
    try file.writeStreamingAll(io, input);
    file.close(io);

    var stderr: std.Io.Writer.Allocating = .init(gpa);
    defer stderr.deinit();
    try formatFilePath(gpa, tmp.dir, "legacy.roc", null, .{}, io, &stderr.writer);

    const formatted = try tmp.dir.readFileAlloc(io, "legacy.roc", gpa, .limited(1024));
    defer gpa.free(formatted);
    try std.testing.expectEqualStrings("v : { a ?: U8 }\n", formatted);
    try std.testing.expectEqualStrings(
        "Migrated legacy optional field syntax `:?` to `?:` in legacy.roc.\n",
        stderr.written(),
    );
}

test "formatFilePath leaves unrelated parse failures untouched" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const input = "v : { a U8 }";
    const file = try tmp.dir.createFile(io, "invalid.roc", .{});
    try file.writeStreamingAll(io, input);
    file.close(io);

    var stderr: std.Io.Writer.Allocating = .init(gpa);
    defer stderr.deinit();
    try std.testing.expectError(
        error.ParsingFailed,
        formatFilePath(gpa, tmp.dir, "invalid.roc", null, .{}, io, &stderr.writer),
    );

    const after = try tmp.dir.readFileAlloc(io, "invalid.roc", gpa, .limited(1024));
    defer gpa.free(after);
    try std.testing.expectEqualStrings(input, after);
}

test "optional field access formats as a tight postfix accessor" {
    const result = try moduleFmtsStable(
        std.testing.allocator,
        "value=record .?outer.?inner?",
        false,
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("value = record.?outer.?inner?\n", result);
}

test "mixed required and optional field access formats as one tight chain" {
    const result = try moduleFmtsStable(
        std.testing.allocator,
        "value=record .?outer.inner .?leaf.value",
        false,
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("value = record.?outer.inner.?leaf.value\n", result);
}

test "comments between flat field access segments retain one level of indentation" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\value=record # first
        \\.?outer # second
        \\.inner
    , false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "value = record # first\n" ++
            "\t.?outer # second\n" ++
            "\t.inner\n",
        result,
    );
}

test "deep mixed field access chains format stack-safely" {
    const gpa = std.testing.allocator;
    const depth = 4096;

    var source = std.ArrayList(u8).empty;
    defer source.deinit(gpa);
    try source.appendSlice(gpa, "value = record");
    for (0..depth) |i| {
        try source.appendSlice(gpa, if (i % 2 == 0) ".required" else ".?optional");
    }

    const result = try moduleFmtsStable(gpa, source.items, false);
    defer gpa.free(result);

    try source.append(gpa, '\n');
    try std.testing.expectEqualStrings(source.items, result);
}

test "optional field access composes with defaulting without token ambiguity" {
    const result = try moduleFmtsStable(
        std.testing.allocator,
        "value=record.?field??0",
        false,
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("value = record.?field ?? 0\n", result);
}

test "propagated optional function field application formats unambiguously" {
    const result = try moduleFmtsStable(
        std.testing.allocator,
        "value=record .?callback?(arg)",
        false,
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("value = record.?callback?(arg)\n", result);
}

test "compact function argument collections ignore removable source newlines" {
    const cases = [_]struct {
        input: []const u8,
        expected: []const u8,
    }{
        .{
            .input = "p:{e:\nList(\nU)=>U}=>r",
            .expected = "p : { e : List(U) => U } => r\n",
        },
        .{
            .input = "p:{e:\n[A\n]=>U}=>r",
            .expected = "p : { e : [A] => U } => r\n",
        },
    };

    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);
    }
}

test "explicitly expanded function argument collections remain expanded" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\p:{e:{n:U,
        \\}=>U}=>r
    , false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "p : {\n" ++
            "\te : {\n" ++
            "\t\tn : U,\n" ++
            "\t} => U,\n" ++
            "} => r\n",
        result,
    );
}

test "issue 8851: arrow call with space before field access is idempotent" {
    // Preserve the legacy grouping while migrating the arrow to a pipe.
    const result = try moduleFmtsStable(std.testing.allocator, "a=0->b .c()", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = (0 |> b).c()\n", result);
}

test "issue 8851: arrow call with chained zero-arg applies is idempotent" {
    // a = 0->b()().c() should format stably - must preserve ALL levels of function application
    const result = try moduleFmtsStable(std.testing.allocator, "a = 0->b()().c()", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = (0 |> b()()).c()\n", result);
}

test "issue 8851: multiline arrow call with field access is idempotent" {
    // Multiline case from issue comment 1
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a=0->b
        \\      .c()
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "a = 0 |> b\n" ++
            "\t.c()\n",
        result,
    );
}

test "multiline arrow receiver in tuple is idempotent" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a=(0(0->X)
        \\->X .a)
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "a = (\n" ++
            "\t0(0 |> X)\n" ++
            "\t\t|> X\n" ++
            "\t\t.a\n" ++
            ")\n",
        result,
    );
}

test "multiline legacy arrow tuple access stays flat" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\x = value
        \\    -> pair()
        \\    .0
    , false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "x = value\n" ++
            "\t|> pair\n" ++
            "\t.0\n",
        result,
    );
}

test "multiline pipe result postfix preserves boundary comments" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\x = value->pair() # keep with pipe
        \\    .first()
    , false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "x = value |> pair # keep with pipe\n" ++
            "\t.first()\n",
        result,
    );
}

test "integer field receiver separated by carriage return is idempotent" {
    const result = try moduleFmtsStable(std.testing.allocator, "a=(0\r.e)\n", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = ((0).e)\n", result);
}

test "issue 8851: tuple dispatch with chained zero-arg applies is idempotent" {
    // ()->b()()() from issue comment 2
    const result = try moduleFmtsStable(std.testing.allocator, "a=()->b()()()", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = () |> b()()()\n", result);
}

test "issue 8851: chained field access after arrow call is idempotent" {
    // 0->b .c .d() - multiple field accesses, parentheses disambiguate
    const result = try moduleFmtsStable(std.testing.allocator, "a=0->b .c .d()", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = (0 |> b).c.d()\n", result);
}

test "issue 8851: arrow call with uppercase tag (module-like) is idempotent" {
    // 0->M .c - uppercase identifier parses as tag, not ident
    // Dispatching to a tag is invalid, parentheses disambiguate the field access
    const result = try moduleFmtsStable(std.testing.allocator, "a=0->M .c", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = (0 |> M).c\n", result);
}

test "formatter migrates expression arrows without changing type arrows" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\apply : a, (a -> b) -> b
        \\apply = |value, fn| value->fn()
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "apply : a, (a -> b) -> b\n" ++
            "apply = |value, fn| value |> fn\n",
        result,
    );
}

test "formatter migrates legacy arrows with parenthesized lambda targets" {
    const result = try moduleFmtsStable(std.testing.allocator, "a=(10->(|x|x+1),10->(|x|x+1)())", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = (10 |> (|x| x + 1), 10 |> (|x| x + 1))\n", result);
}

test "formatter keeps non-name-rooted legacy arrow targets grouped" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a=x->({f: |v|v}.f)
        \\b=x->((f,g))
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "a = x |> ({ f: |v| v }.f)\n" ++
            "\n" ++
            "b = x |> ((f, g))\n",
        result,
    );
}

test "pipe accepts every surrounding whitespace combination and formatter inserts it" {
    const result = try moduleFmtsStable(std.testing.allocator, "a=(1|>add(2),1 |>add(2),1|> add(2),1 |> add(2))", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = (1 |> add(2), 1 |> add(2), 1 |> add(2), 1 |> add(2))\n", result);
}

test "pipe owns the postfix chain on its right" {
    const result = try moduleFmtsStable(std.testing.allocator, "a=foo|>bar(baz).blah()", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = foo |> bar(baz).blah()\n", result);
}

test "formatter preserves an old arrow's postfix grouping during migration" {
    const result = try moduleFmtsStable(std.testing.allocator, "a=foo->bar(baz).blah()", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = (foo |> bar(baz)).blah()\n", result);
}

test "pipe drops direct empty target argument lists" {
    const result = try moduleFmtsStable(std.testing.allocator, "a=(x|>foo(),x|>Ok(),x|>(|v|v)())", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = (x |> foo, x |> Ok, x |> (|v| v))\n", result);
}

test "pipe keeps comments from removed empty argument lists" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a=x|>foo(
        \\ # keep me
        \\)
        \\
        \\b=x->foo(
        \\ # keep old too
        \\)
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "a = x\n" ++
            "\t|>\n" ++
            "\t# keep me\n" ++
            "\tfoo\n" ++
            "\n" ++
            "b = x\n" ++
            "\t|>\n" ++
            "\t# keep old too\n" ++
            "\tfoo\n",
        result,
    );
}

test "multiline pipes start indented lines" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a=foo
        \\ |>bar(baz)
        \\ |>qux()
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "a = foo\n" ++
            "\t|> bar(baz)\n" ++
            "\t|> qux\n",
        result,
    );
}

test "multiline pipe results keep their postfix chains unparenthesized" {
    const source = "main =\n" ++
        "\t\"./input.txt\"\n" ++
        "\t\t|> Path.from_str()\n" ++
        "\t.read_bytes!()?\n" ++
        "\t\t|> Foo.from_bytes()?\n" ++
        "\t\t|> transform(2, Much)\n" ++
        "\t.to_bytes()?\n" ++
        "\t\t|> Path.write_bytes!(Path.from_str(\"./output.txt\"))\n";
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "main =\n" ++
            "\t\"./input.txt\"\n" ++
            "\t\t|> Path.from_str\n" ++
            "\t\t.read_bytes!()?\n" ++
            "\t\t|> Foo.from_bytes()?\n" ++
            "\t\t|> transform(2, Much)\n" ++
            "\t\t.to_bytes()?\n" ++
            "\t\t|> Path.write_bytes!(Path.from_str(\"./output.txt\"))\n",
        result,
    );
}

test "pipe targets ending in question marks stay unparenthesized" {
    const input = "get_iso_str : List(U8) -> Try(Str, _)\n" ++
        "get_iso_str = |bytes| {\n" ++
        "\tstr = bytes |> Str.from_utf8()?\n" ++
        "\tresponse : { local_time : Str }\n" ++
        "\tresponse = Json.parse(str)?\n" ++
        "\tOk(response.local_time)\n" ++
        "}\n";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(input, result);
}

test "issue 10510: empty call controls pipe question suffix precedence" {
    // Repro for https://github.com/roc-lang/roc/issues/10510
    const result = try moduleFmtsStable(std.testing.allocator,
        \\from_arrow = a->f()?
        \\with_call = a |> f()?
        \\without_call = a |> f?
        \\parenthesized_result = (a |> f)?
        \\chain = a->f()?->g()?->h()?
    , false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "from_arrow = a |> f()?\n" ++
            "\n" ++
            "with_call = a |> f()?\n" ++
            "\n" ++
            "without_call = a |> f?\n" ++
            "\n" ++
            "parenthesized_result = (a |> f)?\n" ++
            "\n" ++
            "chain = a |> f()? |> g()? |> h()?\n",
        result,
    );
}

test "issue 10517: fallible pipe chain stays flat after formatting" {
    // Repro for https://github.com/roc-lang/roc/issues/10517
    const result = try moduleFmtsStable(std.testing.allocator,
        \\expect {
        \\    _result = CircularBuffer.create({ capacity: 3 })
        \\        .write(1)?
        \\        .write(2)?
        \\        .write(3)?
        \\        .read()?
        \\        -> expect_value(1)
        \\        .write(4)?
        \\        .overwrite(5)
        \\        .read()?
        \\        -> expect_value(3)
        \\        .read()?
        \\        -> expect_value(4)
        \\        .read()?
        \\        -> expect_value(5)
        \\
        \\    Bool.True
        \\}
        \\
        \\main! = |_| { Ok({}) }
    , false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "expect {\n" ++
            "\t_result = CircularBuffer.create({ capacity: 3 })\n" ++
            "\t\t.write(1)?\n" ++
            "\t\t.write(2)?\n" ++
            "\t\t.write(3)?\n" ++
            "\t\t.read()?\n" ++
            "\t\t|> expect_value(1)\n" ++
            "\t\t.write(4)?\n" ++
            "\t\t.overwrite(5)\n" ++
            "\t\t.read()?\n" ++
            "\t\t|> expect_value(3)\n" ++
            "\t\t.read()?\n" ++
            "\t\t|> expect_value(4)\n" ++
            "\t\t.read()?\n" ++
            "\t\t|> expect_value(5)\n" ++
            "\n" ++
            "\tBool.True\n" ++
            "}\n" ++
            "\n" ++
            "main! = |_| {\n" ++
            "\tOk({})\n" ++
            "}\n",
        result,
    );
}

test "parenthesized pipe receivers drop direct empty target arguments" {
    const input = "x = (foo |> bar()).baz()";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("x = (foo |> bar).baz()\n", result);
}

test "issue 10478: multiline legacy arrow receiver stays flat" {
    // Repro for https://github.com/roc-lang/roc/issues/10478
    const result = try moduleFmtsStable(std.testing.allocator,
        \\x = a
        \\    .b()
        \\    ->C.d()
        \\    .e()
    , false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "x = a\n" ++
            "\t.b()\n" ++
            "\t|> C.d\n" ++
            "\t.e()\n",
        result,
    );
}

test "multiline pipes preserve comments around the operator" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a=foo # after lhs
        \\ |>bar(baz)
        \\
        \\b=foo|> # after pipe
        \\ bar(baz)
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "a = foo # after lhs\n" ++
            "\t|> bar(baz)\n" ++
            "\n" ++
            "b = foo\n" ++
            "\t|> # after pipe\n" ++
            "\tbar(baz)\n",
        result,
    );
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

test "parenthesized type application with leading newline is idempotent" {
    const result = try moduleFmtsStable(std.testing.allocator, "\ne:[(N())()]", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("\ne : [(N()), ()]\n", result);
}

test "import alias after comment stays separated" {
    const result = try moduleFmtsStable(std.testing.allocator, "import A / B as#\nX", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("import A/B as #\nX\n", result);
}

test "import path spacing is normalized" {
    const result = try moduleFmtsStable(std.testing.allocator, "import Layout / Path as LayoutPath", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("import Layout/Path as LayoutPath\n", result);
}

test "nested import path remains nested after formatting" {
    const result = try moduleFmtsStable(std.testing.allocator, "import Root .Nested .Leaf", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("import Root.Nested.Leaf\n", result);
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

test "effectful where-clause method arrows are preserved" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\uses_tick : a => U64 where [a.tick! : a => U64, a.next! : () => U64]
        \\uses_tick = |x| x.tick!()
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        \\uses_tick : a => U64 where [a.tick! : a => U64, a.next! : () => U64]
        \\uses_tick = |x| x.tick!()
        \\
    , result);
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

test "single multiline collection literal apply args keep call paren tight" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\record_arg = f(
        \\    {
        \\        x: 1,
        \\        y: 2,
        \\    },
        \\)
        \\list_arg = f(
        \\    [
        \\        1,
        \\        2,
        \\    ],
        \\)
        \\tuple_arg = f(
        \\    (
        \\        1,
        \\        2,
        \\    ),
        \\)
    , false);
    defer std.testing.allocator.free(result);

    const expected =
        "record_arg = f({\n" ++
        "\tx: 1,\n" ++
        "\ty: 2,\n" ++
        "})\n" ++
        "\n" ++
        "list_arg = f([\n" ++
        "\t1,\n" ++
        "\t2,\n" ++
        "])\n" ++
        "\n" ++
        "tuple_arg = f((\n" ++
        "\t1,\n" ++
        "\t2,\n" ++
        "))\n";
    try std.testing.expectEqualStrings(expected, result);
}

test "single multiline collection literal method args keep call paren tight" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\sprite = base
        \\    .pos(
        \\        {
        \\            x: 1,
        \\            y: 2,
        \\        },
        \\    )
    , false);
    defer std.testing.allocator.free(result);

    const expected =
        "sprite = base\n" ++
        "\t.pos({\n" ++
        "\t\tx: 1,\n" ++
        "\t\ty: 2,\n" ++
        "\t})\n";
    try std.testing.expectEqualStrings(expected, result);
}

test "trailing commas explicitly control collection layout" {
    const Case = struct {
        input: []const u8,
        expected: []const u8,
    };
    const cases = [_]Case{
        .{
            .input = "x = [\n  1,\n  2\n]",
            .expected = "x = [1, 2]\n",
        },
        .{
            .input = "x = [1, 2,]",
            .expected = "x = [\n\t1,\n\t2,\n]\n",
        },
        .{
            .input = "x = f(\n  1,\n  2\n)",
            .expected = "x = f(1, 2)\n",
        },
        .{
            .input = "x = {\n  a: 1,\n  b: 2\n}",
            .expected = "x = { a: 1, b: 2 }\n",
        },
        .{
            .input = "x = |a, b,| a",
            .expected = "x = |\n\ta,\n\tb,\n| a\n",
        },
        .{
            .input = "x = |a, b,| {}",
            .expected = "x = |\n\ta,\n\tb,\n| {}\n",
        },
        .{
            .input = "import Foo exposing [\n  one,\n  two\n]",
            .expected = "import Foo exposing [one, two]\n",
        },
        .{
            .input = "import Foo exposing [one, two,]",
            .expected = "import Foo exposing [\n\tone,\n\ttwo,\n]\n",
        },
        .{
            .input = "Pair(one, two,) : (one, two,)",
            .expected = "Pair(\n\tone,\n\ttwo,\n) : (\n\tone,\n\ttwo,\n)\n",
        },
    };

    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);
    }
}

test "issue 9939: named open tag union type variable is preserved" {
    const result = try moduleFmtsStable(std.testing.allocator, "T(a) : [..a]", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("T(a) : [..a]\n", result);
}

test "issue 10046: empty nominal destructure lambda argument is idempotent" {
    // Repro for https://github.com/roc-lang/roc/issues/10046
    const result = try moduleFmtsStable(std.testing.allocator, "g=|D.()|0", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("g = |D.()| 0\n", result);
}

test "nominal record destructure shorthand is preserved in every pattern position" {
    const input =
        \\sum_arg = |Point.{x,y}| x+y
        \\sum_let = |point| {
        \\Point.{x,y}=point
        \\x+y
        \\}
        \\sum_match = |point| match point {
        \\Point.{x,y} => x+y
        \\}
    ;
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    const expected =
        "sum_arg = |Point.{ x, y }| x + y\n\n" ++
        "sum_let = |point| {\n" ++
        "\tPoint.{ x, y } = point\n" ++
        "\tx + y\n" ++
        "}\n\n" ++
        "sum_match = |point| match point {\n" ++
        "\tPoint.{ x, y } => x + y\n" ++
        "}\n";
    try std.testing.expectEqualStrings(expected, result);
}

test "issue 9940: comments in empty collections and blocks are preserved" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\test = |{}| {
        \\    # Some informational comment on why this is empty
        \\}
        \\empty_list = [
        \\    # Keeping this list item disabled
        \\]
        \\empty_record = {
        \\    # Keeping this record field disabled
        \\}
    , false);
    defer std.testing.allocator.free(result);

    const expected =
        "test = |{}| {\n" ++
        "\t# Some informational comment on why this is empty\n" ++
        "}\n" ++
        "\n" ++
        "empty_list = [\n" ++
        "\t# Keeping this list item disabled\n" ++
        "]\n" ++
        "\n" ++
        "empty_record = {\n" ++
        "\t# Keeping this record field disabled\n" ++
        "}\n";
    try std.testing.expectEqualStrings(expected, result);
}

test "issue 9940: comments in platform header sections are preserved" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\platform "pf"
        \\    requires {}
        \\    exposes [
        \\        # Stderr,
        \\    ]
        \\    packages {
        \\        # This is where all the package stuff goes
        \\    }
        \\    provides {
        \\        "roc_init": init_for_host!,
        \\        # "roc_generate": generate_for_host!,
        \\    }
        \\    hosted {
        \\        "hosted_stderr_line": Stderr.line!,
        \\        # "hosted_event_queue_enqueue": EventQueue.enqueue!
        \\    }
    , false);
    defer std.testing.allocator.free(result);

    const expected =
        "platform \"pf\"\n" ++
        "\trequires {}\n" ++
        "\texposes [\n" ++
        "\t\t# Stderr,\n" ++
        "\t]\n" ++
        "\tpackages {\n" ++
        "\t\t# This is where all the package stuff goes\n" ++
        "\t}\n" ++
        "\tprovides {\n" ++
        "\t\t\"roc_init\": init_for_host!,\n" ++
        "\t\t# \"roc_generate\": generate_for_host!,\n" ++
        "\t}\n" ++
        "\thosted {\n" ++
        "\t\t\"hosted_stderr_line\": Stderr.line!,\n" ++
        "\t\t# \"hosted_event_queue_enqueue\": EventQueue.enqueue!\n" ++
        "\t}\n";
    try std.testing.expectEqualStrings(expected, result);
}

test "multiline platform symbol map remains multiline after comments are discarded" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\platform"
        \\requires{[R:r]for a:R->R}exposes[]packages{a:""}provides{"":#
        \\h,"":r}
    , false);
    defer std.testing.allocator.free(result);

    const expected =
        "platform \"\"\n" ++
        "\trequires {\n" ++
        "\t\t[R : r] for a : R -> R\n" ++
        "\t}\n" ++
        "\texposes []\n" ++
        "\tpackages { a: \"\" }\n" ++
        "\tprovides {\n" ++
        "\t\t\"\": h,\n" ++
        "\t\t\"\": r,\n" ++
        "\t}\n";
    try std.testing.expectEqualStrings(expected, result);
}

test "issue 10445: package header without dependencies formats successfully" {
    // Repro for https://github.com/roc-lang/roc/issues/10445
    const result = try moduleFmtsStable(std.testing.allocator,
        \\package [
        \\    Date,
        \\    DateTime,
        \\    Duration,
        \\    Time,
        \\    Now,
        \\]
    , false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "package\n" ++
            "\t[\n" ++
            "\t\tDate,\n" ++
            "\t\tDateTime,\n" ++
            "\t\tDuration,\n" ++
            "\t\tTime,\n" ++
            "\t\tNow,\n" ++
            "\t]\n" ++
            "\t{}\n",
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

/// Format `input` as a compiler reporting itself as `compiler_version` would,
/// so that tests of the `roc` version pin do not depend on how this binary was
/// built. Asserts that formatting the result again is a no-op, since a pin
/// that has just been brought up to date must have nothing left to upgrade.
fn fmtAsCompiler(gpa: std.mem.Allocator, input: []const u8, compiler_version: []const u8) FormatTestError![]const u8 {
    const options: Options = .{ .compiler_version = compiler_version };

    var module_env = try ModuleEnv.init(gpa, input);
    defer module_env.deinit();
    const parse_ast = try parse.file(gpa, &module_env.common);
    defer parse_ast.deinit();
    std.testing.expectEqualSlices(AST.Diagnostic, &[_]AST.Diagnostic{}, parse_ast.parse_diagnostics.items) catch {
        return error.ParseFailed;
    };

    var result: std.Io.Writer.Allocating = .init(gpa);
    defer result.deinit();
    try formatAstWithOptions(parse_ast.*, &result.writer, options);

    var stable_env = try ModuleEnv.init(gpa, result.written());
    defer stable_env.deinit();
    const stable_ast = try parse.file(gpa, &stable_env.common);
    defer stable_ast.deinit();
    var stable: std.Io.Writer.Allocating = .init(gpa);
    defer stable.deinit();
    try formatAstWithOptions(stable_ast.*, &stable.writer, options);
    std.testing.expectEqualStrings(result.written(), stable.written()) catch {
        return error.FormattingNotStable;
    };

    return try result.toOwnedSlice();
}

test "fmt upgrades an app's roc version pin to a newer nightly" {
    const result = try fmtAsCompiler(
        std.testing.allocator,
        \\app [main!] { pf: platform "../platform/main.roc", roc: "nightly-2026-July-30-aaaaaaa" }
        \\
        \\main! = |_| {}
    ,
        "nightly-2026-August-1-bbbbbbb",
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        \\app [main!] { pf: platform "../platform/main.roc", roc: "nightly-2026-August-1-bbbbbbb" }
        \\
        \\main! = |_| {}
        \\
    , result);
}

test "fmt upgrades a package's roc version pin" {
    const result = try fmtAsCompiler(
        std.testing.allocator,
        \\package [Foo] { roc: "nightly-2026-July-30-aaaaaaa" }
    ,
        "nightly-2026-August-1-bbbbbbb",
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        \\package [Foo] { roc: "nightly-2026-August-1-bbbbbbb" }
        \\
    , result);
}

test "fmt upgrades a roc version pin written across several lines" {
    const result = try fmtAsCompiler(
        std.testing.allocator,
        "app [main!] {\n" ++
            "\tpf: platform \"../platform/main.roc\",\n" ++
            "\troc: \"nightly-2026-July-30-aaaaaaa\",\n" ++
            "}\n\nmain! = |_| {}",
        "nightly-2026-August-1-bbbbbbb",
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "app [main!] {\n" ++
            "\tpf: platform \"../platform/main.roc\",\n" ++
            "\troc: \"nightly-2026-August-1-bbbbbbb\",\n" ++
            "}\n\nmain! = |_| {}\n",
        result,
    );
}

test "fmt leaves a roc version pin alone when it must not be upgraded" {
    const gpa = std.testing.allocator;
    const cases = [_]struct { pinned: []const u8, running: []const u8 }{
        // Running an older nightly than the pin.
        .{ .pinned = "nightly-2026-August-1-aaaaaaa", .running = "nightly-2026-July-30-bbbbbbb" },
        // A release pin is deliberate, so a nightly must not overwrite it.
        .{ .pinned = "0.1.0", .running = "nightly-2026-July-30-bbbbbbb" },
        // A local development build is not a version a header may pin.
        .{ .pinned = "nightly-2026-July-30-aaaaaaa", .running = "debug-c6dfe61b" },
    };

    for (cases) |case| {
        const input = try std.fmt.allocPrint(gpa, "package [Foo] {{ roc: \"{s}\" }}\n", .{case.pinned});
        defer gpa.free(input);

        const result = try fmtAsCompiler(gpa, input, case.running);
        defer gpa.free(result);

        try std.testing.expectEqualStrings(input, result);
    }
}

test "fmt leaves a roc version pin alone when the compiler is unknown" {
    const input =
        \\package [Foo] { roc: "nightly-2026-July-30-aaaaaaa" }
        \\
    ;
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(input, result);
}

test "fmt upgrades a roc version pin that has a comment written inside it" {
    // The formatter drops a comment written between a header field's `:` and
    // its value whether or not the field is a version pin, so upgrading such a
    // pin loses nothing that would otherwise have survived.
    const input = "package [Foo] {\n" ++
        "\troc: # pinned deliberately\n" ++
        "\t\t\"nightly-2026-July-30-aaaaaaa\",\n" ++
        "}\n";
    const result = try fmtAsCompiler(std.testing.allocator, input, "nightly-2026-August-1-bbbbbbb");
    defer std.testing.allocator.free(result);

    try std.testing.expect(std.mem.find(u8, result, "nightly-2026-August-1-bbbbbbb") != null);
}

test "fmt preserves a shebang on the first line" {
    const input = "#!/usr/bin/env roc\n" ++
        "app [main!] { pf: platform \"./platform/main.roc\" }\n";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(input, result);
}

test "fmt preserves a shebang in a file with no header" {
    const input = "#!/usr/bin/env roc\n" ++
        "x = 1\n";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(input, result);
}

test "fmt spaces out a #! that is not on the first line" {
    // Only the very first line of a file can be a shebang, so `#!` anywhere else
    // is an ordinary comment and gets the usual space after the `#`.
    const input = "x = 1\n" ++
        "#!/usr/bin/env roc\n";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("x = 1\n# !/usr/bin/env roc\n", result);
}
