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
const OpenRows = @import("open_rows.zig").OpenRows;
/// Owned builtin facts reusable across sequential formatter operations.
pub const BuiltinFacts = @import("open_rows.zig").BuiltinFacts;
const StatementScope = @import("open_rows.zig").StatementScope;

/// Errors that can occur while formatting an already-parsed AST.
pub const FormatAstError = Allocator.Error || std.Io.Writer.Error || error{ParsingFailed};
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
    /// Borrowed, invocation-owned builtin facts. Never shared between threads.
    builtin_facts: ?*BuiltinFacts = null,
};

/// Report of the result of formatting Roc files including the count of successes, failures, and any files that need to be reformatted
pub const FormattingResult = struct {
    success: usize,
    failure: usize,
    /// Owned paths, relative to the supplied base directory (or absolute).
    /// Only relevant when using `roc fmt --check`.
    unformatted_files: ?std.array_list.Managed([]const u8),

    pub fn deinit(self: *@This()) void {
        if (self.unformatted_files) |files| {
            for (files.items) |path| files.allocator.free(path);
            files.deinit();
        }
    }
};

/// Carriage-return normalization is an existing explicit formatter migration.
/// The tokenizer records other errors even when their diagnostics are omitted.
fn tokenizationPermitsFormatting(ast: AST) bool {
    return !ast.source_rejected and !ast.tokenize_has_non_carriage_return_errors;
}

/// Parse diagnostics whose recovery AST is an explicit source migration that
/// the formatter owns. Every other parse diagnostic still blocks formatting so
/// a malformed file is never overwritten from a lossy recovery tree.
fn parseDiagnosticsPermitFormatting(diagnostics: []const AST.Diagnostic) bool {
    for (diagnostics) |diagnostic| {
        if (diagnostic.tag != .optional_field_mark_after_colon and diagnostic.tag != .record_field_assignment) return false;
    }
    return true;
}

/// Formats all roc files in the specified path.
/// Handles both single files and directories
/// Returns the number of files successfully formatted and that failed to format.
pub fn formatPath(gpa: std.mem.Allocator, arena: std.mem.Allocator, base_dir: std.Io.Dir, path: []const u8, check: bool, options: Options, io: std.Io, stderr: *std.Io.Writer) FormatPathError!FormattingResult {
    var builtin_facts = BuiltinFacts{ .allocator = gpa };
    defer builtin_facts.deinit();
    var shared_options = options;
    if (shared_options.builtin_facts == null) shared_options.builtin_facts = &builtin_facts;
    var success_count: usize = 0;
    var failed_count: usize = 0;
    // Only used for `roc fmt --check`. If we aren't doing check, don't bother allocating
    var unformatted_files = if (check) std.array_list.Managed([]const u8).init(gpa) else null;
    errdefer if (unformatted_files) |files| {
        for (files.items) |file_path| files.allocator.free(file_path);
        files.deinit();
    };

    // First try as a directory.
    if (base_dir.openDir(io, path, .{ .iterate = true })) |const_dir| {
        var dir = const_dir;
        defer dir.close(io);
        // Walk is recursive.
        var walker = try dir.walk(arena);
        defer walker.deinit();
        while (try walker.next(io)) |entry| {
            if (entry.kind == .file) {
                if (!std.mem.eql(u8, std.fs.path.extension(entry.basename), ".roc")) continue;
                const file_path = try std.fs.path.join(gpa, &.{ path, entry.path });
                defer gpa.free(file_path);
                if (formatFilePath(gpa, base_dir, file_path, if (unformatted_files) |*to_reformat| to_reformat else null, shared_options, io, stderr)) |_| {
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
                        try stderr.print("Failed to format {f}: {any}\n", .{ base.bidi.Display{ .bytes = entry.path }, err });
                        failed_count += 1;
                    },
                }
            }
        }
    } else |_| {
        if (formatFilePath(gpa, base_dir, path, if (unformatted_files) |*to_reformat| to_reformat else null, shared_options, io, stderr)) |_| {
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
                try stderr.print("Failed to format {f}: {any}\n", .{ base.bidi.Display{ .bytes = path }, err });
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
    if (!tokenizationPermitsFormatting(parse_ast.*) or !parseDiagnosticsPermitFormatting(parse_ast.parse_diagnostics.items)) {
        try printParseErrors(gpa, module_env.common.source, parse_ast.*, stderr);
        return error.ParsingFailed;
    }
    var migrates_optional_field_syntax = false;
    var migrates_record_assignments = false;
    for (parse_ast.parse_diagnostics.items) |diagnostic| {
        migrates_optional_field_syntax = migrates_optional_field_syntax or diagnostic.tag == .optional_field_mark_after_colon;
        migrates_record_assignments = migrates_record_assignments or diagnostic.tag == .record_field_assignment;
    }

    // Check if the file is formatted without actually formatting it
    if (unformatted_files != null) {
        var formatted: std.Io.Writer.Allocating = .init(gpa);
        defer formatted.deinit();
        try formatAstWithOptions(parse_ast.*, &formatted.writer, options);
        if (!std.mem.eql(u8, formatted.written(), module_env.common.source)) {
            const files = unformatted_files.?;
            const owned_path = try files.allocator.dupe(u8, path);
            errdefer files.allocator.free(owned_path);
            try files.append(owned_path);
        }
    } else { // Otherwise actually format it
        const output_file = try base_dir.createFile(io, path, .{});
        defer output_file.close(io);
        var output_buffer: [4096]u8 = undefined;
        var output_writer = output_file.writer(io, &output_buffer);
        try formatAstWithOptions(parse_ast.*, &output_writer.interface, options);
        if (migrates_record_assignments) {
            try stderr.print("Corrected record field separators `=` to `:` in {f}.\n", .{base.bidi.Display{ .bytes = path }});
        }
        if (migrates_optional_field_syntax) {
            try stderr.print("Migrated legacy optional field syntax `:?` to `?:` in {f}.\n", .{base.bidi.Display{ .bytes = path }});
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
    if (!tokenizationPermitsFormatting(parse_ast.*) or !parseDiagnosticsPermitFormatting(parse_ast.parse_diagnostics.items)) {
        try printParseErrors(gpa, module_env.common.source, parse_ast.*, stderr);
        return error.ParsingFailed;
    }
    var migrates_optional_field_syntax = false;
    var migrates_record_assignments = false;
    for (parse_ast.parse_diagnostics.items) |diagnostic| {
        migrates_optional_field_syntax = migrates_optional_field_syntax or diagnostic.tag == .optional_field_mark_after_colon;
        migrates_record_assignments = migrates_record_assignments or diagnostic.tag == .record_field_assignment;
    }

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = stdout.writer(io, &stdout_buffer);
    try formatAstWithOptions(parse_ast.*, &stdout_writer.interface, options);
    if (migrates_record_assignments) {
        try stderr.writeAll("Corrected record field separators `=` to `:` from stdin.\n");
    }
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
        if (comptime builtin.mode == .debug) {
            std.debug.assert(@backingInt(idx) == expected_idx);
        } else if (@backingInt(idx) != expected_idx) {
            unreachable;
        }
    }
    for (source, 0..) |c, i| {
        if (c == '\n') {
            const expected_idx = line_offsets.items.items.len;
            const idx = try line_offsets.append(gpa, @intCast(i));
            if (comptime builtin.mode == .debug) {
                std.debug.assert(@backingInt(idx) == expected_idx);
            } else if (@backingInt(idx) != expected_idx) {
                unreachable;
            }
        }
    }

    for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
        var report = try parse_ast.tokenizeDiagnosticToReport(diagnostic, gpa, null);
        defer report.deinit();
        try @import("reporting").renderReportToPlain(&report, stderr, @import("reporting").ReportingConfig.initForTesting());
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
/// Rejects tokenizer errors before emitting source.
pub fn formatAst(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatAstWithOptions(ast, writer, .{});
}

/// `formatAst`, but for callers that know which compiler is running and so can
/// have a header's `roc` version pin brought up to date. See `Options`.
pub fn formatAstWithOptions(ast: AST, writer: *std.Io.Writer, options: Options) FormatAstError!void {
    return formatIRNode(ast, writer, options, Formatter.formatFile);
}

/// The `..` token of every anonymous tag-union extension that formatting the
/// file drops as redundant, in source order. Caller owns the returned slice.
pub fn redundantOpenExtensions(gpa: std.mem.Allocator, ast: AST) FormatAstError![]Token.Idx {
    var discard_buf: [256]u8 = undefined;
    var discard = std.Io.Writer.Discarding.init(&discard_buf);
    var fmt = try Formatter.init(ast, &discard.writer, .{});
    defer fmt.deinit();
    var open_rows = try OpenRows.init(gpa, &fmt.ast);
    defer open_rows.deinit();
    try fmt.formatFileWithOpenRows(&open_rows);

    var dropped = std.ArrayList(Token.Idx).empty;
    errdefer dropped.deinit(gpa);
    for (0..ast.store.nodeCount()) |node_index| {
        const anno_idx: AST.TypeAnno.Idx = @fromBackingInt(@intCast(node_index));
        if (!open_rows.isRedundant(anno_idx)) continue;
        try dropped.append(gpa, ast.store.getTypeAnno(anno_idx).tag_union.ext.open);
    }
    std.mem.sort(Token.Idx, dropped.items, {}, std.sort.asc(Token.Idx));
    return dropped.toOwnedSlice(gpa);
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is a header.
/// Rejects tokenizer errors before emitting source.
pub fn formatHeader(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatIRNode(ast, writer, .{}, formatHeaderInner);
}

fn formatHeaderInner(fmt: *Formatter) FormatAstError!void {
    return fmt.formatHeader(@fromBackingInt(@intCast(fmt.ast.root_node_idx)));
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is a statement.
/// Rejects tokenizer errors before emitting source.
pub fn formatStatement(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatIRNode(ast, writer, .{}, formatStatementInner);
}

fn formatStatementInner(fmt: *Formatter) FormatAstError!void {
    return fmt.formatStatement(@fromBackingInt(@intCast(fmt.ast.root_node_idx)));
}

/// Formats and writes out well-formed source of a Roc parse IR (AST) when the root node is an expression.
/// Rejects tokenizer errors before emitting source.
pub fn formatExpr(ast: AST, writer: *std.Io.Writer) FormatAstError!void {
    return formatIRNode(ast, writer, .{}, formatExprNode);
}

fn formatExprNode(fmt: *Formatter) FormatAstError!void {
    try fmt.formatExprDiscard(@fromBackingInt(@intCast(fmt.ast.root_node_idx)));
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
    /// Ordinary layout predictions, indexed by the shared AST node domain.
    node_layouts: []TypeLayout,
    /// Stacks for layout queries.
    layout_scratch: LayoutEval.Scratch = .{},
    /// Suspended emission workers.
    frames: std.ArrayList(Frame) = .empty,
    /// Grouped predictions normalize source-only newlines independently.
    grouped_layouts: []TypeLayout,
    /// Prefix counts of comments in inter-token gaps.
    comment_prefix: []u32,
    layout_computations: if (builtin.is_test) usize else void = if (builtin.is_test) 0 else {},
    grouped_layout_computations: if (builtin.is_test) usize else void = if (builtin.is_test) 0 else {},
    options: Options,
    /// Set while formatting a header whose version pin is out of date.
    roc_version_upgrade: ?RocVersionUpgrade = null,
    platform_dependency: ?AST.RecordField.Idx = null,
    /// Which anonymous `..` tag-union extensions are dropped as redundant. Set
    /// only while formatting a whole file: whether a `..` is redundant depends
    /// on the declarations, imports and header around it.
    open_rows: ?*OpenRows = null,
    curr_indent: u32 = 0,
    flags: FormatFlags = .no_debug,
    // This starts true since beginning of file is considered a newline.
    has_newline: bool = true,
    has_multiline_string: bool = false,
    pending_spaces: usize = 0,

    /// Creates a new Formatter for the given parse IR.
    fn init(ast: AST, writer: *std.Io.Writer, options: Options) FormatAstError!Formatter {
        if (!tokenizationPermitsFormatting(ast)) return error.ParsingFailed;
        const node_layouts = try ast.gpa.alloc(TypeLayout, ast.store.nodeCount());
        errdefer ast.gpa.free(node_layouts);
        @memset(node_layouts, .unknown);
        const grouped_layouts = try ast.gpa.alloc(TypeLayout, ast.store.nodeCount());
        errdefer ast.gpa.free(grouped_layouts);
        @memset(grouped_layouts, .unknown);
        const comment_prefix = try ast.gpa.alloc(u32, ast.tokens.tokens.len + 1);
        errdefer ast.gpa.free(comment_prefix);
        comment_prefix[0] = 0;
        for (0..ast.tokens.tokens.len) |i| {
            const token: Token.Idx = @intCast(i);
            const start = if (token == 0) 0 else ast.tokens.resolve(token - 1).end.offset;
            const end = ast.tokens.resolve(token).start.offset;
            comment_prefix[i + 1] = comment_prefix[i] + @as(u32, @intFromBool(
                std.mem.findScalar(u8, ast.env.source[start..end], '#') != null,
            ));
        }

        return .{
            .ast = ast,
            .writer = writer,
            .node_layouts = node_layouts,
            .grouped_layouts = grouped_layouts,
            .comment_prefix = comment_prefix,
            .options = options,
        };
    }

    fn deinit(fmt: *Formatter) void {
        fmt.ast.gpa.free(fmt.node_layouts);
        fmt.layout_scratch.deinit(fmt.ast.gpa);
        fmt.frames.deinit(fmt.ast.gpa);
        fmt.ast.gpa.free(fmt.grouped_layouts);
        fmt.ast.gpa.free(fmt.comment_prefix);
    }

    /// Deinits all data owned by the formatter object.
    fn flush(fmt: *Formatter) error{WriteFailed}!void {
        fmt.pending_spaces = 0;
        try fmt.writer.flush();
    }

    /// Emits a string containing the well-formed source of a Roc parse IR (AST).
    /// The resulting string is owned by the caller.
    pub fn formatFile(fmt: *Formatter) FormatAstError!void {
        var open_rows = try OpenRows.init(fmt.ast.gpa, &fmt.ast);
        defer open_rows.deinit();
        open_rows.shared_builtins = fmt.options.builtin_facts;
        try fmt.formatFileWithOpenRows(&open_rows);
    }

    fn formatFileWithOpenRows(fmt: *Formatter, open_rows: *OpenRows) FormatAstError!void {
        fmt.open_rows = open_rows;
        defer fmt.open_rows = null;
        fmt.ast.store.emptyScratch();
        const file = fmt.ast.store.getFile();
        const header = fmt.ast.store.getHeader(file.header);
        const header_region = fmt.ast.store.nodes.items.items(.region)[@backingInt(file.header)];
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
        try fmt.markRedundantOpenRows(statement_slice, .file);
        var prev_def_info: ?DefInfo = null;
        for (statement_slice) |s| {
            const region = fmt.nodeRegion(@backingInt(s));
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

    /// A formatter worker suspended at a child node. Emission follows every
    /// AST edge through these heap frames, so nesting depth is bounded by heap
    /// and output rather than by native stack. Each frame carries what its
    /// worker still owes after its children: the indentation it restores, the
    /// trivia and delimiters it emits, its expression-format context, and the
    /// layout decisions it made before suspending.
    const Frame = union(enum) {
        expr: ExprFrame,
        pattern: PatternFrame,
        pattern_record_field: PatternRecordFieldFrame,
        type_anno: TypeAnnoFrame,
        anno_record_field: AnnoRecordFieldFrame,
        record_field: RecordFieldFrame,
        statement: StatementFrame,
        collection: CollectionFrame,
        parenthesized: ParenthesizedFrame,
        pipe_target_parens: PipeTargetParensFrame,
        interpolation: InterpolationFrame,
        where_constraint: WhereConstraintFrame,
        where_clause: WhereClauseFrame,
    };

    const Step = union(enum) {
        /// Suspend the current frame until this child completes. The child's
        /// result is passed to the current frame when it resumes.
        call: Frame,
        done: FormattedExpr,
    };

    fn call(frame: Frame) Step {
        return .{ .call = frame };
    }

    fn exprFrame(ei: AST.Expr.Idx, context: ExprFormatContext) Frame {
        return .{ .expr = .{ .ei = ei, .context = context } };
    }

    fn patternFrame(pi: AST.Pattern.Idx) Frame {
        return .{ .pattern = .{ .pi = pi } };
    }

    fn typeAnnoFrame(anno: AST.TypeAnno.Idx) Frame {
        return .{ .type_anno = .{ .anno = anno } };
    }

    fn parenthesizedFrame(region: ?AST.TokenizedRegion, expr_idx: AST.Expr.Idx, multiline: bool) Frame {
        return .{ .parenthesized = .{ .region = region, .expr = expr_idx, .multiline = multiline } };
    }

    fn collectionFrame(region: AST.TokenizedRegion, layout: AST.CollectionLayout, braces: Braces, items: CollectionItems) Frame {
        return .{ .collection = .{ .region = region, .layout = layout, .braces = braces, .items = items } };
    }

    /// Runs `root` and every frame it calls to completion. Workers never call
    /// this; they return `Step.call` instead.
    fn run(fmt: *Formatter, root: Frame) FormatAstError!FormattedExpr {
        std.debug.assert(fmt.frames.items.len == 0);
        defer fmt.frames.clearRetainingCapacity();
        try fmt.frames.append(fmt.ast.gpa, root);
        var result = FormattedExpr{ .region = .{ .start = 0, .end = 0 } };
        while (fmt.frames.items.len > 0) {
            const frame = &fmt.frames.items[fmt.frames.items.len - 1];
            switch (try fmt.step(frame, result)) {
                .call => |child| try fmt.frames.append(fmt.ast.gpa, child),
                .done => |done| {
                    result = done;
                    fmt.frames.items.len -= 1;
                },
            }
        }
        return result;
    }

    /// `result` is the most recently completed child's result.
    fn step(fmt: *Formatter, frame: *Frame, result: FormattedExpr) FormatAstError!Step {
        return switch (frame.*) {
            .expr => |*f| fmt.stepExpr(f, result),
            .pattern => |*f| fmt.stepPattern(f, result),
            .pattern_record_field => |*f| fmt.stepPatternRecordField(f, result),
            .type_anno => |*f| fmt.stepTypeAnno(f, result),
            .anno_record_field => |*f| fmt.stepAnnoRecordField(f, result),
            .record_field => |*f| fmt.stepRecordField(f, result),
            .statement => |*f| fmt.stepStatement(f, result),
            .collection => |*f| fmt.stepCollection(f, result),
            .parenthesized => |*f| fmt.stepParenthesized(f, result),
            .pipe_target_parens => |*f| fmt.stepPipeTargetParens(f, result),
            .interpolation => |*f| fmt.stepInterpolation(f, result),
            .where_constraint => |*f| fmt.stepWhereConstraint(f, result),
            .where_clause => |*f| fmt.stepWhereClause(f, result),
        };
    }

    fn formatStatement(fmt: *Formatter, si: AST.Statement.Idx) FormatAstError!void {
        _ = try fmt.run(.{ .statement = .{ .si = si } });
    }

    fn formatExprWithInfo(fmt: *Formatter, ei: AST.Expr.Idx) FormatAstError!FormattedExpr {
        return fmt.run(exprFrame(ei, .{}));
    }

    fn formatExprDiscard(fmt: *Formatter, ei: AST.Expr.Idx) FormatAstError!void {
        Formatter.discardRegion((try fmt.formatExprWithInfo(ei)).region);
    }

    fn formatRecordField(fmt: *Formatter, idx: AST.RecordField.Idx) FormatAstError!AST.TokenizedRegion {
        return (try fmt.run(.{ .record_field = .{ .idx = idx } })).region;
    }

    fn formatTypeAnnoDiscard(fmt: *Formatter, anno: AST.TypeAnno.Idx) FormatAstError!void {
        Formatter.discardRegion((try fmt.run(typeAnnoFrame(anno))).region);
    }

    const StatementFrame = struct {
        si: AST.Statement.Idx,
        phase: u8 = 0,
        multiline: bool = false,
        /// Indentation restored when the statement completes.
        indent: u32 = 0,
        next: usize = 0,
    };

    fn finishStatement(fmt: *Formatter, f: *StatementFrame) Step {
        fmt.curr_indent = f.indent;
        return .{ .done = .{ .region = fmt.nodeRegion(@backingInt(f.si)) } };
    }

    fn stepStatement(fmt: *Formatter, f: *StatementFrame, result: FormattedExpr) FormatAstError!Step {
        const statement = fmt.ast.store.getStatement(f.si);
        if (f.phase == 0) {
            f.multiline = try fmt.nodeWillBeMultiline(AST.Statement.Idx, f.si);
            f.indent = fmt.curr_indent;
        }
        const multiline = f.multiline;
        switch (statement) {
            .decl => |d| switch (f.phase) {
                0 => {
                    f.phase = 1;
                    return call(patternFrame(d.pattern));
                },
                1 => {
                    Formatter.discardRegion(result.region);
                    const pattern_region = fmt.nodeRegion(@backingInt(d.pattern));
                    if (multiline and try fmt.flushContinuationComments(pattern_region.end)) {
                        try fmt.pushIndent();
                        try fmt.push('=');
                    } else {
                        try fmt.pushAll(" = ");
                    }
                    const body_region = fmt.nodeRegion(@backingInt(d.body));
                    if (multiline and try fmt.flushContinuationComments(body_region.start)) {
                        try fmt.pushIndent();
                    }
                    f.phase = 2;
                    return call(exprFrame(d.body, .{}));
                },
                else => return fmt.finishStatement(f),
            },
            .@"var" => |v| {
                if (f.phase != 0) return fmt.finishStatement(f);
                try fmt.pushAll("var");
                if (multiline and try fmt.flushContinuationComments(v.name)) {
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
                    const body_region = fmt.nodeRegion(@backingInt(body));
                    if (multiline and try fmt.flushContinuationComments(body_region.start)) {
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    f.phase = 1;
                    return call(exprFrame(body, .{}));
                }
                return fmt.finishStatement(f);
            },
            .expr => |e| {
                if (f.phase != 0) return fmt.finishStatement(f);
                f.phase = 1;
                return call(exprFrame(e.expr, .{}));
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
                    const list_region = AST.TokenizedRegion{
                        .start = fmt.nodeRegion(@backingInt(items[0])).start - 1,
                        .end = i.region.end,
                    };
                    try fmt.commentBoundary(list_region.start, false);
                    try fmt.formatOrderedCollection(list_region, fmt.ast.store.getCollectionLayout(f.si), .square, AST.ExposedItem.Idx, items, Formatter.formatExposedItem, true);
                }
                return fmt.finishStatement(f);
            },
            .file_import => |fi| {
                try fmt.pushAll("import");
                try fmt.commentBoundary(fi.path_tok - 1, true);
                try fmt.push('"');
                try fmt.pushTokenText(fi.path_tok);
                try fmt.push('"');
                try fmt.commentBoundary(fi.name_tok - 1, true);
                try fmt.pushAll("as");
                try fmt.commentBoundary(fi.name_tok, true);
                try fmt.pushTokenText(fi.name_tok);
                try fmt.commentBoundary(fi.name_tok + 1, true);
                try fmt.push(':');
                try fmt.commentBoundary(fi.name_tok + 2, true);
                if (fi.is_bytes) {
                    try fmt.pushAll("List(U8)");
                } else {
                    try fmt.pushAll("Str");
                }
                return fmt.finishStatement(f);
            },
            .type_decl => |d| {
                const anno_region = fmt.nodeRegion(@backingInt(d.anno));
                sw: switch (f.phase) {
                    0 => {
                        if (d.kind == .where_alias) {
                            f.phase = 1;
                            return call(typeAnnoFrame(d.anno));
                        }
                        f.phase = 3;
                        if (try fmt.typeHeaderFrame(d.header)) |header| return call(header);
                        continue :sw 3;
                    },
                    1 => {
                        // `where_alias`: the annotation was emitted.
                        Formatter.discardRegion(result.region);
                        try fmt.push('.');
                        f.phase = 2;
                        if (try fmt.typeHeaderFrame(d.header)) |header| return call(header);
                        continue :sw 2;
                    },
                    2 => {
                        // `where_alias`: the header was emitted.
                        try fmt.pushAll(" :");
                        if (d.where) |w| {
                            if (multiline) {
                                try fmt.ensureNewline();
                                fmt.curr_indent += 1;
                                try fmt.pushIndent();
                            } else {
                                try fmt.push(' ');
                            }
                            f.phase = 7;
                            return call(.{ .where_constraint = .{ .where = w, .multiline = multiline } });
                        }
                        return fmt.finishStatement(f);
                    },
                    3 => {
                        // The header was emitted.
                        const header_region = fmt.nodeRegion(@backingInt(d.header));
                        if (multiline and try fmt.flushContinuationComments(header_region.end)) {
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
                        if (multiline and try fmt.flushContinuationComments(anno_region.start)) {
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 4;
                        return call(typeAnnoFrame(d.anno));
                    },
                    4 => {
                        Formatter.discardRegion(result.region);
                        if (d.where) |w| {
                            const where_multiline = multiline or try fmt.collectionWillBeMultiline(AST.WhereClause.Idx, w);
                            if (where_multiline) {
                                try fmt.flushCommentsBeforeDiscard(anno_region.end);
                                try fmt.ensureNewline();
                                fmt.curr_indent += 1;
                                try fmt.pushIndent();
                            }
                            f.phase = 5;
                            return call(.{ .where_constraint = .{ .where = w, .multiline = where_multiline } });
                        }
                        continue :sw 5;
                    },
                    5 => {
                        const assoc = d.associated orelse return fmt.finishStatement(f);
                        const open_curly = assoc.region.start;
                        const dot = open_curly - 1;
                        if (fmt.hasCommentBefore(dot) and try fmt.flushCommentsBefore(dot)) {
                            try fmt.pushIndent();
                        }
                        try fmt.push('.');
                        if (fmt.hasCommentBefore(open_curly) and try fmt.flushCommentsBefore(open_curly)) {
                            try fmt.pushIndent();
                        }
                        try fmt.push('{');
                        if (assoc.statements.span.len > 0) {
                            fmt.curr_indent += 1;
                            try fmt.markRedundantOpenRows(fmt.ast.store.statementSlice(assoc.statements), .associated);
                            continue :sw 6;
                        } else if (fmt.regionHasInteriorComment(assoc.region)) {
                            fmt.curr_indent += 1;
                            try fmt.flushCommentsBeforeDiscard(fmt.regionClosingToken(assoc.region).?);
                            fmt.curr_indent -= 1;
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        try fmt.push('}');
                        return fmt.finishStatement(f);
                    },
                    6 => {
                        // Associated statements, one per resumption.
                        const assoc = d.associated.?;
                        const statements = fmt.ast.store.statementSlice(assoc.statements);
                        if (f.next < statements.len) {
                            const stmt_idx = statements[f.next];
                            f.next += 1;
                            const stmt_region = fmt.nodeRegion(@backingInt(stmt_idx));
                            try fmt.flushCommentsBeforeDiscard(stmt_region.start);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                            f.phase = 6;
                            return call(.{ .statement = .{ .si = stmt_idx } });
                        }
                        // Flush any trailing comments before the closing brace
                        try fmt.flushCommentsBeforeDiscard(assoc.region.end - 1);
                        try fmt.ensureNewline();
                        fmt.curr_indent -= 1;
                        try fmt.pushIndent();
                        try fmt.push('}');
                        return fmt.finishStatement(f);
                    },
                    else => return fmt.finishStatement(f),
                }
            },
            .type_anno => |t| switch (f.phase) {
                0 => {
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
                    const anno_region = fmt.nodeRegion(@backingInt(t.anno));
                    if (multiline and try fmt.flushContinuationComments(anno_region.start)) {
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    f.phase = 1;
                    return call(typeAnnoFrame(t.anno));
                },
                1 => {
                    Formatter.discardRegion(result.region);
                    if (t.where) |w| {
                        const anno_region = fmt.nodeRegion(@backingInt(t.anno));
                        const where_multiline = multiline or try fmt.collectionWillBeMultiline(AST.WhereClause.Idx, w);
                        if (where_multiline) {
                            try fmt.flushCommentsBeforeDiscard(anno_region.end);
                            try fmt.ensureNewline();
                            fmt.curr_indent += 1;
                            try fmt.pushIndent();
                        }
                        f.phase = 2;
                        return call(.{ .where_constraint = .{ .where = w, .multiline = where_multiline } });
                    }
                    return fmt.finishStatement(f);
                },
                else => return fmt.finishStatement(f),
            },
            .expect => |e| {
                if (f.phase != 0) return fmt.finishStatement(f);
                try fmt.pushAll("expect");
                const body_region = fmt.nodeRegion(@backingInt(e.body));
                if (multiline and try fmt.flushContinuationComments(body_region.start)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                f.phase = 1;
                return call(exprFrame(e.body, .{}));
            },
            .@"for" => |fs| {
                const patt_region = fmt.nodeRegion(@backingInt(fs.patt));
                const expr_region = fmt.nodeRegion(@backingInt(fs.expr));
                switch (f.phase) {
                    0 => {
                        try fmt.pushAll(forKeyword(fs.kind));
                        if (multiline and try fmt.flushContinuationComments(patt_region.start)) {
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 1;
                        return call(patternFrame(fs.patt));
                    },
                    1 => {
                        Formatter.discardRegion(result.region);
                        if (multiline and try fmt.flushContinuationComments(patt_region.end)) {
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        try fmt.pushAll("in");
                        if (multiline and try fmt.flushContinuationComments(expr_region.start)) {
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 2;
                        return call(exprFrame(fs.expr, .{}));
                    },
                    2 => {
                        if (multiline and try fmt.flushContinuationComments(expr_region.end)) {
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 3;
                        return call(exprFrame(fs.body, .{}));
                    },
                    else => return fmt.finishStatement(f),
                }
            },
            .@"while" => |w| {
                const cond_region = fmt.nodeRegion(@backingInt(w.cond));
                switch (f.phase) {
                    0 => {
                        try fmt.pushAll("while");
                        if (multiline and try fmt.flushContinuationComments(cond_region.start)) {
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 1;
                        return call(exprFrame(w.cond, .{}));
                    },
                    1 => {
                        if (multiline and try fmt.flushContinuationComments(cond_region.end)) {
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 2;
                        return call(exprFrame(w.body, .{}));
                    },
                    else => return fmt.finishStatement(f),
                }
            },
            .crash => |c| {
                if (f.phase != 0) return fmt.finishStatement(f);
                try fmt.pushAll("crash");
                const body_region = fmt.nodeRegion(@backingInt(c.expr));
                if (multiline and try fmt.flushContinuationComments(body_region.start)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                f.phase = 1;
                return call(exprFrame(c.expr, .{}));
            },
            .dbg => |d| {
                if (f.phase != 0) return fmt.finishStatement(f);
                try fmt.pushAll("dbg");
                const body_region = fmt.nodeRegion(@backingInt(d.expr));
                if (multiline and try fmt.flushContinuationComments(body_region.start)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                f.phase = 1;
                return call(exprFrame(d.expr, .{}));
            },
            .@"return" => |r| {
                if (f.phase != 0) return fmt.finishStatement(f);
                try fmt.pushAll("return");
                const body_region = fmt.nodeRegion(@backingInt(r.expr));
                if (multiline and try fmt.flushContinuationComments(body_region.start)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                f.phase = 1;
                return call(exprFrame(r.expr, .{}));
            },
            .@"break" => {
                try fmt.pushAll("break");
                return fmt.finishStatement(f);
            },
            .malformed => {
                // Output nothing for malformed node
                return fmt.finishStatement(f);
            },
        }
    }

    const WhereConstraintFrame = struct {
        where: AST.Collection.Idx,
        multiline: bool,
        phase: u8 = 0,
        clauses_multiline: bool = false,
        indent: u32 = 0,
        next: usize = 0,
    };

    fn stepWhereConstraint(fmt: *Formatter, f: *WhereConstraintFrame, result: FormattedExpr) FormatAstError!Step {
        const clause_coll = fmt.ast.store.getCollection(f.where);
        const clause_slice = fmt.ast.store.whereClauseSlice(.{ .span = clause_coll.span });
        sw: switch (f.phase) {
            0 => {
                f.indent = fmt.curr_indent;
                f.clauses_multiline = try fmt.collectionWillBeMultiline(AST.WhereClause.Idx, f.where);

                if (!f.multiline) {
                    try fmt.push(' ');
                }

                try fmt.pushAll("where");

                // Add opening bracket
                try fmt.commentBoundary(clause_coll.region.start + 1, true);
                try fmt.push('[');
                if (f.clauses_multiline) {
                    fmt.curr_indent += 1;
                }
                continue :sw 1;
            },
            1 => {
                if (f.next < clause_slice.len) {
                    const clause = clause_slice[f.next];
                    if (f.clauses_multiline) {
                        const clause_region = fmt.nodeRegion(@backingInt(clause));
                        try fmt.flushCommentsBeforeDiscard(clause_region.start);
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                    if (f.next > 0) {
                        if (!f.clauses_multiline) {
                            try fmt.pushAll(", ");
                        }
                    }
                    f.phase = 2;
                    return call(.{ .where_clause = .{ .idx = clause } });
                }

                if (f.clauses_multiline) {
                    try fmt.flushCommentsBeforeDiscard(clause_coll.region.end - 1);
                    try fmt.ensureNewline();
                    fmt.curr_indent -= 1;
                    try fmt.pushIndent();
                }
                try fmt.push(']');
                fmt.curr_indent = f.indent;
                return .{ .done = .{ .region = clause_coll.region } };
            },
            else => {
                Formatter.discardRegion(result.region);
                if (f.clauses_multiline) {
                    try fmt.push(',');
                    if (fmt.ast.tokens.tokenTag(result.region.end) == .Comma and fmt.hasCommentBefore(result.region.end)) {
                        try fmt.flushCommentsBeforeDiscard(result.region.end);
                    }
                }
                f.next += 1;
                continue :sw 1;
            },
        }
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
            if (tok > target.start_tok) try fmt.commentBoundary(tok, false);
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

    const SourceGap = struct {
        start: usize,
        end: usize,
    };

    fn OrderedEntry(comptime T: type) type {
        return struct {
            idx: T,
            name: []const u8,
            leading: SourceGap,
            before_separator: SourceGap,
            trailing: SourceGap,

            fn lessThan(_: void, a: @This(), b: @This()) bool {
                const a_upper = a.name.len > 0 and std.ascii.isUpper(a.name[0]);
                const b_upper = b.name.len > 0 and std.ascii.isUpper(b.name[0]);
                if (a_upper != b_upper) return a_upper;
                const order = std.mem.order(u8, a.name, b.name);
                return if (order == .eq) @backingInt(a.idx) < @backingInt(b.idx) else order == .lt;
            }
        };
    }

    fn orderedName(fmt: *Formatter, comptime T: type, idx: T) FormatAstError![]const u8 {
        var name: std.Io.Writer.Allocating = .init(fmt.ast.gpa);
        defer name.deinit();
        if (T == AST.ExposedItem.Idx) {
            const item = fmt.ast.store.getExposedItem(idx);
            switch (item) {
                inline .lower_ident, .upper_ident => |ident| {
                    for (fmt.ast.store.tokenSlice(ident.qualifiers)) |qualifier| {
                        try name.writer.writeAll(std.mem.trimStart(u8, fmt.ast.resolve(qualifier), "."));
                        try name.writer.writeByte('.');
                    }
                    try name.writer.writeAll(std.mem.trimStart(u8, fmt.ast.resolve(ident.ident), "."));
                },
                .upper_ident_star => |ident| {
                    for (fmt.ast.store.tokenSlice(ident.qualifiers)) |qualifier| {
                        try name.writer.writeAll(std.mem.trimStart(u8, fmt.ast.resolve(qualifier), "."));
                        try name.writer.writeByte('.');
                    }
                    try name.writer.writeAll(std.mem.trimStart(u8, fmt.ast.resolve(ident.ident), "."));
                    try name.writer.writeAll(".*");
                },
                .malformed => return error.ParsingFailed,
            }
        } else {
            const token = if (T == AST.RecordField.Idx)
                fmt.ast.store.getRecordField(idx).name
            else if (T == AST.RequiresEntry.Idx)
                fmt.ast.store.getRequiresEntry(idx).entrypoint_name
            else if (T == AST.SymbolMapEntry.Idx)
                fmt.ast.store.getSymbolMapEntry(idx).symbol
            else
                @compileError("unsupported ordered header item");
            try name.writer.writeAll(fmt.ast.resolve(token));
        }
        return try name.toOwnedSlice();
    }

    fn flushSourceGap(fmt: *Formatter, gap: SourceGap, spacing: CommentSpacing) error{WriteFailed}!void {
        if (gap.start == gap.end) return;
        if (gap.start > 0 and fmt.ast.env.source[gap.start - 1] == '\n') try fmt.ensureNewline();
        var start = gap.start;
        if (fmt.has_newline) {
            while (start < gap.end and (fmt.ast.env.source[start] == ' ' or fmt.ast.env.source[start] == '\t' or fmt.ast.env.source[start] == '\r')) : (start += 1) {}
            if (start < gap.end and fmt.ast.env.source[start] == '\n') start += 1;
        }
        _ = try fmt.flushComments(start, fmt.ast.env.source[start..gap.end], spacing);
    }

    /// Plan comment ownership in source order, then emit entries in name order.
    /// Inline comments stay with their preceding entry; standalone comments
    /// stay with their following entry, including at the collection's edges.
    fn formatOrderedCollection(fmt: *Formatter, region: AST.TokenizedRegion, layout: AST.CollectionLayout, braces: Braces, comptime T: type, items: []T, formatter: fn (*Formatter, T) FormatAstError!AST.TokenizedRegion, trailing_comma: bool) FormatAstError!void {
        const multiline = items.len > 0 and layout == .expanded or try fmt.nodesWillBeMultiline(T, items) or fmt.regionHasInteriorComment(region);
        const indent = fmt.curr_indent;
        defer fmt.curr_indent = indent;
        try fmt.push(braces.start());
        if (items.len == 0) {
            if (multiline) {
                fmt.curr_indent += 1;
                _ = try fmt.flushCommentsBeforeWithSpacing(fmt.regionClosingToken(region).?, .{ .after_block_open = true });
                fmt.curr_indent = indent;
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            try fmt.push(braces.end());
            return;
        }

        const Entry = OrderedEntry(T);
        var entries = std.ArrayList(Entry).empty;
        defer {
            for (entries.items) |entry| fmt.ast.gpa.free(entry.name);
            entries.deinit(fmt.ast.gpa);
        }
        try entries.ensureTotalCapacity(fmt.ast.gpa, items.len);
        const opening_end: usize = fmt.ast.tokens.resolve(region.start).end.offset;
        const first_start = fmt.ast.tokens.resolve(fmt.nodeRegion(@backingInt(items[0])).start).start.offset;
        const opening_text = fmt.ast.env.source[opening_end..first_start];
        const opening_line_end = std.mem.findScalar(u8, opening_text, '\n') orelse opening_text.len;
        const has_opening_comment = std.mem.findScalar(u8, opening_text[0..opening_line_end], '#') != null;
        const opening_gap = SourceGap{ .start = opening_end, .end = if (has_opening_comment) opening_end + opening_line_end else opening_end };
        var leading_start: usize = if (has_opening_comment and opening_line_end < opening_text.len) opening_gap.end + 1 else opening_gap.end;
        const closing_start = fmt.ast.tokens.resolve(fmt.regionClosingToken(region).?).start.offset;
        for (items, 0..) |idx, i| {
            const item_region = fmt.nodeRegion(@backingInt(idx));
            const item_start = fmt.ast.tokens.resolve(item_region.start).start.offset;
            const item_end = fmt.ast.tokens.resolve(item_region.end - 1).end.offset;
            const has_comma = fmt.ast.tokens.tokenTag(item_region.end) == .Comma;
            const comma = fmt.ast.tokens.resolve(item_region.end);
            const gap_start = if (has_comma) comma.end.offset else item_end;
            const gap_end = if (i + 1 < items.len)
                fmt.ast.tokens.resolve(fmt.nodeRegion(@backingInt(items[i + 1])).start).start.offset
            else
                closing_start;
            const gap = fmt.ast.env.source[gap_start..gap_end];
            const line_end = std.mem.findScalar(u8, gap, '\n') orelse gap.len;
            const inline_end = if (std.mem.findScalar(u8, gap[0..line_end], '#') != null) gap_start + line_end else gap_start;
            entries.appendAssumeCapacity(.{
                .idx = idx,
                .name = try fmt.orderedName(T, idx),
                .leading = .{ .start = leading_start, .end = item_start },
                .before_separator = .{ .start = item_end, .end = if (has_comma) comma.start.offset else item_end },
                .trailing = .{ .start = gap_start, .end = inline_end },
            });
            leading_start = if (inline_end > gap_start and line_end < gap.len) inline_end + 1 else inline_end;
        }
        const closing_gap = SourceGap{ .start = leading_start, .end = closing_start };
        std.mem.sort(Entry, entries.items, {}, Entry.lessThan);

        if (multiline) fmt.curr_indent += 1 else if (braces == .curly) try fmt.push(' ');
        const item_indent = fmt.curr_indent;
        try fmt.flushSourceGap(opening_gap, .{ .after_block_open = true });
        for (entries.items, 0..) |entry, i| {
            if (multiline) {
                try fmt.flushSourceGap(entry.leading, .{ .after_block_open = i == 0 });
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            Formatter.discardRegion(try formatter(fmt, entry.idx));
            fmt.curr_indent = item_indent;
            if (multiline) {
                if (fmt.has_multiline_string) {
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                }
                if (trailing_comma or i + 1 < entries.items.len) try fmt.push(',');
                if (std.mem.findScalar(u8, fmt.ast.env.source[entry.before_separator.start..entry.before_separator.end], '#') != null) {
                    try fmt.flushSourceGap(entry.before_separator, .{});
                }
                try fmt.flushSourceGap(entry.trailing, .{});
            } else if (i + 1 < entries.items.len) {
                try fmt.pushAll(", ");
            }
        }
        if (multiline) {
            try fmt.flushSourceGap(closing_gap, .{ .before_block_close = true });
            fmt.curr_indent = indent;
            try fmt.ensureNewline();
            try fmt.pushIndent();
        } else if (braces == .curly) try fmt.push(' ');
        try fmt.push(braces.end());
    }

    const CollectionFormatting = struct {
        region: AST.TokenizedRegion,
        braces: Braces,
        multiline: bool,
        indent: u32,
    };

    fn beginCollection(fmt: *Formatter, region: AST.TokenizedRegion, braces: Braces, multiline: bool, empty: bool) FormatAstError!CollectionFormatting {
        const state = CollectionFormatting{ .region = region, .braces = braces, .multiline = multiline, .indent = fmt.curr_indent };
        try fmt.push(braces.start());
        if (empty) {
            if (fmt.regionHasInteriorComment(region)) {
                fmt.curr_indent += 1;
                try fmt.flushCommentsBeforeDiscard(fmt.regionClosingToken(region).?);
                fmt.curr_indent -= 1;
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            try fmt.push(braces.end());
        } else if (multiline) {
            fmt.curr_indent += 1;
        } else if (braces == .curly) {
            try fmt.push(' ');
        }
        return state;
    }

    fn beginCollectionItem(fmt: *Formatter, state: CollectionFormatting, region: AST.TokenizedRegion, first: bool) FormatAstError!void {
        if (state.multiline) {
            _ = try fmt.flushCommentsBeforeWithSpacing(region.start, .{ .after_block_open = first });
            try fmt.ensureNewline();
            try fmt.pushIndent();
        }
    }

    fn endCollectionItem(fmt: *Formatter, state: CollectionFormatting, last: bool) FormatAstError!void {
        if (state.multiline) {
            if (fmt.has_multiline_string) {
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            try fmt.push(',');
        } else if (!last) {
            try fmt.pushAll(", ");
        }
    }

    fn endCollection(fmt: *Formatter, state: CollectionFormatting) FormatAstError!void {
        if (state.multiline) {
            try fmt.flushCommentsBeforeDiscard(state.region.end - 1);
            fmt.curr_indent -= 1;
            try fmt.ensureNewline();
            try fmt.pushIndent();
        } else if (state.braces == .curly) {
            try fmt.push(' ');
        }
        try fmt.push(state.braces.end());
        fmt.curr_indent = state.indent;
    }

    /// The items of a delimited collection, formatted one frame per item.
    const CollectionItems = union(enum) {
        expr: []AST.Expr.Idx,
        pattern: []AST.Pattern.Idx,
        pattern_record_field: []AST.PatternRecordField.Idx,
        type_anno: []AST.TypeAnno.Idx,
        anno_record_field: []AST.AnnoRecordField.Idx,

        fn len(items: CollectionItems) usize {
            return switch (items) {
                inline .expr, .pattern, .pattern_record_field, .type_anno, .anno_record_field => |slice| slice.len,
            };
        }

        fn node(items: CollectionItems, i: usize) u32 {
            return switch (items) {
                inline .expr, .pattern, .pattern_record_field, .type_anno, .anno_record_field => |slice| @backingInt(slice[i]),
            };
        }

        fn frame(items: CollectionItems, i: usize) Frame {
            return switch (items) {
                .expr => |slice| exprFrame(slice[i], .{}),
                .pattern => |slice| patternFrame(slice[i]),
                .pattern_record_field => |slice| .{ .pattern_record_field = .{ .idx = slice[i] } },
                .type_anno => |slice| typeAnnoFrame(slice[i]),
                .anno_record_field => |slice| .{ .anno_record_field = .{ .idx = slice[i] } },
            };
        }
    };

    const CollectionFrame = struct {
        region: AST.TokenizedRegion,
        layout: AST.CollectionLayout,
        braces: Braces,
        items: CollectionItems,
        phase: u8 = 0,
        state: CollectionFormatting = undefined,
        next: usize = 0,
    };

    fn stepCollection(fmt: *Formatter, f: *CollectionFrame, result: FormattedExpr) FormatAstError!Step {
        const len = f.items.len();
        sw: switch (f.phase) {
            0 => {
                const items_multiline = switch (f.items) {
                    inline .expr, .pattern, .pattern_record_field, .type_anno, .anno_record_field => |slice| try fmt.nodesWillBeMultiline(std.meta.Elem(@TypeOf(slice)), slice),
                };
                const multiline = f.layout == .expanded or items_multiline or fmt.regionHasInteriorComment(f.region);
                f.state = try fmt.beginCollection(f.region, f.braces, multiline, len == 0);
                if (len == 0) {
                    fmt.curr_indent = f.state.indent;
                    return .{ .done = .{ .region = f.region } };
                }
                continue :sw 1;
            },
            1 => {
                if (f.next == len) {
                    try fmt.endCollection(f.state);
                    return .{ .done = .{ .region = f.region } };
                }
                try fmt.beginCollectionItem(f.state, fmt.nodeRegion(f.items.node(f.next)), f.next == 0);
                f.phase = 2;
                return call(f.items.frame(f.next));
            },
            else => {
                Formatter.discardRegion(result.region);
                try fmt.endCollectionItem(f.state, f.next + 1 == len);
                if (f.state.multiline and fmt.ast.tokens.tokenTag(result.region.end) == .Comma and fmt.hasCommentBefore(result.region.end)) {
                    try fmt.flushCommentsBeforeDiscard(result.region.end);
                }
                f.next += 1;
                continue :sw 1;
            },
        }
    }

    /// Call arguments, collapsing a lone multiline collection argument into
    /// the call's own parentheses.
    fn applyArgsFrame(fmt: *Formatter, region: AST.TokenizedRegion, layout: AST.CollectionLayout, args: []AST.Expr.Idx) Allocator.Error!Frame {
        if (try fmt.hasSingleMultilineCollectionArg(region, args)) {
            return parenthesizedFrame(null, args[0], false);
        }
        return collectionFrame(region, layout, .round, .{ .expr = args });
    }

    fn hasSingleMultilineCollectionArg(fmt: *Formatter, region: AST.TokenizedRegion, args: []AST.Expr.Idx) Allocator.Error!bool {
        if (args.len != 1) {
            return false;
        }

        const arg_idx = args[0];
        const arg = fmt.ast.store.getExpr(arg_idx);
        const arg_tag = std.meta.activeTag(arg);
        if (arg_tag != .record and arg_tag != .list and arg_tag != .tuple) return false;

        if (!try fmt.nodeWillBeMultiline(AST.Expr.Idx, arg_idx)) {
            return false;
        }

        const arg_region = fmt.nodeRegion(@backingInt(arg_idx));
        if (fmt.hasCommentBefore(arg_region.start)) {
            return false;
        }

        if (region.end > 0 and fmt.hasCommentBefore(region.end - 1)) {
            return false;
        }

        return true;
    }

    const RecordFieldFrame = struct {
        idx: AST.RecordField.Idx,
        phase: u8 = 0,
    };

    fn stepRecordField(fmt: *Formatter, f: *RecordFieldFrame, result: FormattedExpr) FormatAstError!Step {
        const field = fmt.ast.store.getRecordField(f.idx);
        if (f.phase != 0) {
            return .{ .done = .{ .region = field.region, .ends_with_multiline_string_line = result.ends_with_multiline_string_line } };
        }
        try fmt.pushTokenText(field.name);
        if (fmt.roc_version_upgrade) |upgrade| {
            if (f.idx == upgrade.field) {
                // Write the running compiler's version rather than the stale
                // one in the source. Planning the upgrade already parsed that
                // version as a nightly tag, so it is alphanumerics and `-`
                // only and needs no escaping inside the quotes.
                try fmt.pushAll(": \"");
                try fmt.pushAll(upgrade.version);
                try fmt.push('"');
                return .{ .done = .{ .region = field.region } };
            }
        }
        switch (field.value) {
            .supplied => |v| {
                try fmt.commentBoundary(field.name + 1, false);
                try fmt.push(':');
                try fmt.commentBoundary(fmt.nodeRegion(@backingInt(v)).start, true);
                f.phase = 1;
                return call(exprFrame(v, .{}));
            },
            .punned => {},
            .unset => try fmt.pushAll(": _"),
        }
        return .{ .done = .{ .region = field.region } };
    }

    const ExprFormatBehavior = enum {
        normal,
        no_indent_on_access,
        no_additional_indent_on_access,
    };

    const ExprFormatContext = struct {
        behavior: ExprFormatBehavior = .normal,
        question_suffix_follows: bool = false,
        // Follow only the leading callee/receiver until emitted parentheses
        // establish an ordinary expression context. Arguments start fresh.
        starts_pipe_target: bool = false,
    };

    const InterpolationFrame = struct {
        idx: AST.Expr.Idx,
        phase: u8 = 0,
        multiline: bool = false,
    };

    fn stepInterpolation(fmt: *Formatter, f: *InterpolationFrame, result: FormattedExpr) FormatAstError!Step {
        const part_region = fmt.nodeRegion(@backingInt(f.idx));
        if (f.phase == 0) {
            f.phase = 1;
            try fmt.pushAll("${");
            f.multiline = try fmt.interpolationWillBeMultiline(f.idx);
            if (f.multiline) {
                fmt.curr_indent += 1;
                try fmt.flushCommentsBeforeDiscard(part_region.start);
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            return call(exprFrame(f.idx, .{}));
        }
        Formatter.discardRegion(result.region);
        if (f.multiline) {
            try fmt.flushCommentsBeforeDiscard(part_region.end);
            try fmt.ensureNewline();
            fmt.curr_indent -= 1;
            try fmt.pushIndent();
        }
        try fmt.push('}');
        return .{ .done = .{ .region = part_region } };
    }

    fn formatPatternString(fmt: *Formatter, str: anytype) FormatAstError!void {
        try fmt.push('"');
        for (fmt.ast.store.patternStringPartSlice(str.parts)) |part_idx| {
            switch (fmt.ast.store.getPatternStringPart(part_idx)) {
                .text => |text| try fmt.pushTokenText(text.token),
                .capture => |capture| {
                    try fmt.pushAll("${");
                    const indent = fmt.curr_indent;
                    const expanded = fmt.regionHasInteriorComment(capture.region);
                    if (expanded) {
                        fmt.curr_indent += 1;
                        try fmt.commentBoundary(capture.region.start + 1, false);
                    }
                    if (capture.name) |name| {
                        try fmt.pushTokenText(name);
                    } else {
                        try fmt.push('_');
                    }
                    if (expanded) {
                        try fmt.commentBoundary(capture.region.end - 1, false);
                        fmt.curr_indent = indent;
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
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

    fn adjustMultilineAccessIndent(fmt: *Formatter, format_behavior: ExprFormatBehavior) void {
        switch (format_behavior) {
            .normal => fmt.curr_indent += 1,
            .no_indent_on_access => {},
            .no_additional_indent_on_access => if (fmt.curr_indent > 0) {
                fmt.curr_indent -= 1;
            },
        }
    }

    const PostfixLayout = enum {
        compact,
        source,
        continuation,
    };

    /// Owns the trivia before a postfix, including when its receiver gained
    /// parentheses. Compact boundaries discard bare newlines, never comments.
    fn formatPostfixBoundary(fmt: *Formatter, token: Token.Idx, layout: PostfixLayout, format_behavior: ExprFormatBehavior) error{WriteFailed}!void {
        const already_broke = if (layout != .compact or fmt.hasCommentBefore(token))
            try fmt.flushCommentsBefore(token)
        else
            false;
        if (already_broke or layout == .continuation) {
            fmt.adjustMultilineAccessIndent(format_behavior);
            if (!already_broke) try fmt.ensureNewline();
            try fmt.pushIndent();
        }
    }

    fn discardRegion(region: AST.TokenizedRegion) void {
        if (comptime builtin.mode == .debug) {
            std.debug.assert(region.start <= region.end);
        } else if (region.start > region.end) {
            unreachable;
        }
    }

    fn flushCommentsBeforeDiscard(fmt: *Formatter, tokenIdx: Token.Idx) error{WriteFailed}!void {
        const flushed = try fmt.flushCommentsBefore(tokenIdx);
        if (flushed) {
            return;
        }
    }

    /// Item regions end at the separator or closing delimiter. A source comma
    /// has trivia on both sides; an inserted comma has only the closing gap.
    fn flushItemComments(fmt: *Formatter, end: Token.Idx) error{WriteFailed}!void {
        if (fmt.ast.tokens.tokenTag(end) == .Comma) {
            if (fmt.hasCommentBefore(end)) try fmt.flushCommentsBeforeDiscard(end);
            try fmt.flushCommentsAfterDiscard(end);
        } else {
            try fmt.flushCommentsBeforeDiscard(end);
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

    const ParenthesizedFrame = struct {
        /// The source parentheses, whose interior comments the group owns.
        region: ?AST.TokenizedRegion,
        expr: AST.Expr.Idx,
        multiline: bool,
        phase: u8 = 0,
        indent: u32 = 0,
    };

    fn stepParenthesized(fmt: *Formatter, f: *ParenthesizedFrame, result: FormattedExpr) FormatAstError!Step {
        if (f.phase == 0) {
            f.phase = 1;
            f.indent = fmt.curr_indent;
            try fmt.push('(');
            if (f.multiline) {
                fmt.curr_indent += 1;
                if (f.region != null) {
                    const item_region = fmt.nodeRegion(@backingInt(f.expr));
                    try fmt.flushCommentsBeforeDiscard(item_region.start);
                }
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            return call(exprFrame(f.expr, .{}));
        }
        if (f.multiline) {
            if (f.region) |r| {
                try fmt.flushCommentsBeforeDiscard(r.end - 1);
            }
            fmt.curr_indent = f.indent;
            try fmt.ensureNewline();
            try fmt.pushIndent();
        }
        try fmt.push(')');
        fmt.curr_indent = f.indent;
        return .{ .done = result };
    }

    const PipeTargetParensFrame = struct {
        expr: AST.Expr.Idx,
        expand: bool,
        phase: u8 = 0,
        indent: u32 = 0,
    };

    fn stepPipeTargetParens(fmt: *Formatter, f: *PipeTargetParensFrame, result: FormattedExpr) FormatAstError!Step {
        if (f.phase == 0) {
            f.phase = 1;
            f.indent = fmt.curr_indent;
            // Multiline strings consume their whole physical line. Expand direct
            // targets up front; the output state below catches nested terminal
            // strings without a recursive layout prepass.
            try fmt.push('(');
            if (f.expand) {
                fmt.curr_indent += 1;
                try fmt.ensureNewline();
                try fmt.pushIndent();
            }
            return call(exprFrame(f.expr, .{ .behavior = .no_indent_on_access }));
        }
        Formatter.discardRegion(result.region);
        fmt.curr_indent = f.indent;
        if (result.ends_with_multiline_string_line or fmt.has_multiline_string) {
            try fmt.ensureNewline();
            try fmt.pushIndent();
        }
        try fmt.push(')');
        fmt.curr_indent = f.indent;
        return .{ .done = result };
    }

    const ExprFrame = struct {
        ei: AST.Expr.Idx,
        context: ExprFormatContext = .{},
        phase: u8 = 0,
        multiline: bool = false,
        /// Indentation restored when the expression completes.
        indent: u32 = 0,
        formatted: FormattedExpr = undefined,
        locals: ExprLocals = .{ .none = {} },
    };

    /// Decisions and progress an expression keeps across its children.
    const ExprLocals = union {
        none: void,
        parts: struct { next: usize, add_newline: bool },
        postfix: struct { parenthesize_receiver: bool, flatten_pipe_receiver: bool },
        record: struct { multiline: bool, has_extension: bool, empty_has_comment: bool, next: usize },
        lambda: struct { args_multiline: bool, next: usize },
        conditional: struct { base_indent: u32 },
        match: struct { branch_indent: u32, next: usize },
        block: struct { next: usize },
        nominal_record: struct { parenthesize_mapper: bool },
    };

    fn finishExpr(fmt: *Formatter, f: *ExprFrame) Step {
        fmt.curr_indent = f.indent;
        return .{ .done = f.formatted };
    }

    /// Emits string parts in order and returns the next interpolation to
    /// format, or null once every part is emitted.
    fn nextStringPart(fmt: *Formatter, f: *ExprFrame, parts: []AST.Expr.Idx, multiline_string: bool) FormatAstError!?Frame {
        const state = &f.locals.parts;
        while (state.next < parts.len) {
            const idx = parts[state.next];
            state.next += 1;
            const e = fmt.ast.store.getExpr(idx);
            if (std.meta.activeTag(e) != .string_part) {
                state.add_newline = false;
                return .{ .interpolation = .{ .idx = idx } };
            }
            const str = e.string_part;
            if (multiline_string) {
                if (state.add_newline) {
                    // Comments could be located before the MultilineStringStart token, not the StringPart token
                    try fmt.flushCommentsBeforeDiscard(str.region.start - 1);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                    try fmt.pushAll("\\\\");
                }
                state.add_newline = true;
            }
            try fmt.pushTokenText(str.token);
        }
        return null;
    }

    fn stepExpr(fmt: *Formatter, f: *ExprFrame, result: FormattedExpr) FormatAstError!Step {
        const expr = fmt.ast.store.getExpr(f.ei);
        const region = fmt.nodeRegion(@backingInt(f.ei));
        if (f.phase == 0) {
            f.formatted = .{ .region = region };
            f.multiline = try fmt.nodeWillBeMultiline(AST.Expr.Idx, f.ei);
            const indent_modifier: u32 = @intFromBool(f.context.behavior != .normal and fmt.curr_indent > 0);
            f.indent = fmt.curr_indent - indent_modifier;
        }
        const multiline = f.multiline;
        const format_context = f.context;
        const format_behavior = format_context.behavior;
        switch (expr) {
            .apply => |a| switch (f.phase) {
                0 => {
                    f.phase = 1;
                    // A field followed directly by arguments parses as a method
                    // call. Group the callee to preserve application of its value,
                    // including applications nested inside a pipe target.
                    if (fmt.ast.store.getExpr(a.@"fn") == .field_access) {
                        return call(parenthesizedFrame(null, a.@"fn", false));
                    }
                    return call(exprFrame(a.@"fn", .{ .starts_pipe_target = format_context.starts_pipe_target }));
                },
                1 => {
                    Formatter.discardRegion(result.region);
                    f.phase = 2;
                    const fn_region = fmt.nodeRegion(@backingInt(a.@"fn"));
                    const args_region = AST.TokenizedRegion{ .start = fn_region.end, .end = region.end };
                    return call(try fmt.applyArgsFrame(args_region, fmt.ast.store.getCollectionLayout(f.ei), fmt.ast.store.exprSlice(a.args)));
                },
                else => return fmt.finishExpr(f),
            },
            .string_part => |s| {
                try fmt.pushTokenText(s.token);
                return fmt.finishExpr(f);
            },
            .string => |s| {
                if (f.phase == 0) {
                    f.phase = 1;
                    f.locals = .{ .parts = .{ .next = 0, .add_newline = false } };
                    try fmt.push('"');
                }
                if (try fmt.nextStringPart(f, fmt.ast.store.exprSlice(s.parts), false)) |part| return call(part);
                try fmt.push('"');
                return fmt.finishExpr(f);
            },
            .typed_string => |s| {
                if (f.phase == 0) {
                    f.phase = 1;
                    f.locals = .{ .parts = .{ .next = 0, .add_newline = false } };
                    try fmt.push('"');
                }
                if (try fmt.nextStringPart(f, fmt.ast.store.exprSlice(s.parts), false)) |part| return call(part);
                try fmt.push('"');
                try fmt.formatLiteralTypeSuffix(s.type_suffix);
                return fmt.finishExpr(f);
            },
            .multiline_string => |s| {
                if (f.phase == 0) {
                    f.phase = 1;
                    f.locals = .{ .parts = .{ .next = 0, .add_newline = false } };
                    if (!fmt.has_newline) {
                        fmt.curr_indent += 1;
                    }
                    try fmt.pushAll("\\\\");
                }
                if (try fmt.nextStringPart(f, fmt.ast.store.exprSlice(s.parts), true)) |part| return call(part);
                fmt.has_multiline_string = true;
                f.formatted.ends_with_multiline_string_line = true;
                return fmt.finishExpr(f);
            },
            .typed_multiline_string => |s| {
                if (f.phase == 0) {
                    f.phase = 1;
                    f.locals = .{ .parts = .{ .next = 0, .add_newline = false } };
                    if (!fmt.has_newline) {
                        fmt.curr_indent += 1;
                    }
                    try fmt.pushAll("\\\\");
                }
                if (try fmt.nextStringPart(f, fmt.ast.store.exprSlice(s.parts), true)) |part| return call(part);
                // The type suffix lives on its own line after the string body.
                try fmt.ensureNewline();
                try fmt.pushIndent();
                try fmt.formatLiteralTypeSuffix(s.type_suffix);
                fmt.has_multiline_string = true;
                return fmt.finishExpr(f);
            },
            .single_quote => |s| {
                try fmt.pushTokenText(s.token);
                if (s.type_suffix) |type_suffix| {
                    try fmt.formatLiteralTypeSuffix(type_suffix);
                }
                return fmt.finishExpr(f);
            },
            .ident => |i| {
                const qualifier_tokens = fmt.ast.store.tokenSlice(i.qualifiers);
                const needs_parens = format_context.starts_pipe_target and qualifier_tokens.len == 0 and
                    fmt.ast.tokens.tokens.items(.tag)[i.token] == .NamedUnderscore;
                if (needs_parens) try fmt.push('(');

                for (qualifier_tokens) |tok_idx| {
                    const tok = @as(Token.Idx, @intCast(tok_idx));
                    try fmt.pushTokenText(tok);
                    try fmt.push('.');
                }

                try fmt.pushTokenText(i.token);
                if (needs_parens) try fmt.push(')');
                return fmt.finishExpr(f);
            },
            .field_access => |fa| switch (f.phase) {
                0 => {
                    const receiver_expr = fmt.ast.store.getExpr(fa.receiver);
                    const flatten_pipe_receiver = receiver_expr == .arrow_call and multiline and !format_context.starts_pipe_target;
                    const parenthesize_receiver = (receiver_expr == .arrow_call and !flatten_pipe_receiver) or fmt.postfixReceiverNeedsParens(fa.receiver);
                    f.locals = .{ .postfix = .{ .parenthesize_receiver = parenthesize_receiver, .flatten_pipe_receiver = flatten_pipe_receiver } };
                    f.phase = 1;
                    if (parenthesize_receiver) {
                        const expand_parenthesized_receiver = receiver_expr == .arrow_call and
                            try fmt.nodeWillBeMultiline(AST.Expr.Idx, fa.receiver);
                        return call(parenthesizedFrame(null, fa.receiver, expand_parenthesized_receiver));
                    }
                    return call(exprFrame(fa.receiver, .{ .starts_pipe_target = format_context.starts_pipe_target }));
                },
                else => {
                    const receiver = result;
                    const parenthesize_receiver = f.locals.postfix.parenthesize_receiver;
                    const flatten_pipe_receiver = f.locals.postfix.flatten_pipe_receiver;
                    const access_indent = fmt.curr_indent;
                    const segments = fmt.ast.store.fieldAccessSegmentSlice(fa.segments);
                    std.debug.assert(segments.len > 0);

                    for (segments, 0..) |segment, i| {
                        // Nested field-access nodes used to restore indentation after
                        // every segment. Keep that behavior now that a path is flat.
                        fmt.curr_indent = access_indent;

                        const follows_string_line = i == 0 and !parenthesize_receiver and receiver.ends_with_multiline_string_line;
                        const layout: PostfixLayout = if ((i == 0 and flatten_pipe_receiver) or follows_string_line)
                            .continuation
                        else if (multiline and (!parenthesize_receiver or i > 0))
                            .source
                        else
                            .compact;
                        // Only the chain's final segment sits in the caller's
                        // context; interior segments retain their own indentation.
                        const access_behavior = if (follows_string_line or (i < segments.len - 1 and !(i == 0 and flatten_pipe_receiver)))
                            .normal
                        else
                            format_behavior;
                        if (multiline) try fmt.formatPostfixBoundary(segment.field_token, layout, access_behavior);

                        switch (segment.mode) {
                            .required => try fmt.push('.'),
                            .optional => try fmt.pushAll(".?"),
                        }
                        try fmt.pushTokenText(segment.field_token);
                    }
                    return fmt.finishExpr(f);
                },
            },
            .method_call => |mc| switch (f.phase) {
                0 => {
                    const left_expr = fmt.ast.store.getExpr(mc.receiver);
                    const flatten_pipe_receiver = left_expr == .arrow_call and multiline and !format_context.starts_pipe_target;
                    const parenthesize_receiver = (left_expr == .arrow_call and !flatten_pipe_receiver) or fmt.postfixReceiverNeedsParens(mc.receiver);
                    f.locals = .{ .postfix = .{ .parenthesize_receiver = parenthesize_receiver, .flatten_pipe_receiver = flatten_pipe_receiver } };
                    f.phase = 1;
                    if (parenthesize_receiver) {
                        const expand_parenthesized_receiver = left_expr == .arrow_call and
                            try fmt.nodeWillBeMultiline(AST.Expr.Idx, mc.receiver);
                        return call(parenthesizedFrame(null, mc.receiver, expand_parenthesized_receiver));
                    }
                    return call(exprFrame(mc.receiver, .{ .starts_pipe_target = format_context.starts_pipe_target }));
                },
                1 => {
                    const receiver = result;
                    const parenthesize_receiver = f.locals.postfix.parenthesize_receiver;
                    const flatten_pipe_receiver = f.locals.postfix.flatten_pipe_receiver;
                    const follows_string_line = !parenthesize_receiver and receiver.ends_with_multiline_string_line;
                    const layout: PostfixLayout = if (flatten_pipe_receiver or follows_string_line)
                        .continuation
                    else if (multiline and !parenthesize_receiver)
                        .source
                    else
                        .compact;
                    if (multiline) try fmt.formatPostfixBoundary(mc.method_token, layout, if (follows_string_line) .normal else format_behavior);
                    try fmt.push('.');
                    try fmt.pushTokenText(mc.method_token);
                    // Only the argument list (from the method token onwards) should
                    // determine whether the call is multiline. Using the full
                    // `mc.region` would include newlines from the receiver chain and
                    // wrongly expand short, inline arguments. (See issue #9646)
                    const args_region = AST.TokenizedRegion{ .start = mc.method_token + 1, .end = mc.region.end };
                    f.phase = 2;
                    return call(try fmt.applyArgsFrame(args_region, fmt.ast.store.getCollectionLayout(f.ei), fmt.ast.store.exprSlice(mc.args)));
                },
                else => return fmt.finishExpr(f),
            },
            .arrow_call => |ld| switch (f.phase) {
                0 => {
                    f.phase = 1;
                    return call(exprFrame(ld.left, .{}));
                },
                1 => {
                    const left = result;
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

                    // Every target completes the pipe; only a parenthesized
                    // callee with its own argument list resumes at phase 3.
                    f.phase = 2;
                    const right_expr = fmt.ast.store.getExpr(ld.right);
                    const target_context: ExprFormatContext = .{ .behavior = .no_indent_on_access, .starts_pipe_target = true };
                    switch (right_expr) {
                        .ident, .tag => {
                            return call(exprFrame(ld.right, target_context));
                        },
                        .apply => |apply| {
                            const apply_fn_idx = apply.@"fn";
                            const apply_fn = fmt.ast.store.getExpr(apply_fn_idx);
                            const args = fmt.ast.store.exprSlice(apply.args);
                            const fn_is_call = apply_fn == .apply or
                                apply_fn == .method_call or
                                apply_fn == .nominal_apply;
                            const expand_callee = apply_fn == .multiline_string or apply_fn == .typed_multiline_string;

                            // A direct empty argument list contributes no arguments
                            // beyond the piped value. Remove it unless a following
                            // `?` needs the call syntax to own the completed pipe, or
                            // doing so would expose another application as the RHS.
                            // (`value |> make()()` must remain distinct from
                            // `value |> make()`.)
                            if (args.len == 0 and !fn_is_call and !format_context.question_suffix_follows) {
                                const right_region = fmt.nodeRegion(@backingInt(ld.right));
                                const closing_token = right_region.end - 1;
                                if (fmt.hasCommentBefore(closing_token) and try fmt.flushCommentsBefore(closing_token)) {
                                    try fmt.pushIndent();
                                }
                                if (fmt.pipeTargetNeedsParens(apply_fn_idx)) {
                                    return call(.{ .pipe_target_parens = .{ .expr = apply_fn_idx, .expand = expand_callee } });
                                }
                                return call(exprFrame(apply_fn_idx, target_context));
                            }
                            // Parenthesize a non-atomic callee before printing its
                            // argument list, preserving chains such as `fn()()`.
                            if (fmt.pipeTargetNeedsParens(apply_fn_idx)) {
                                f.phase = 3;
                                return call(.{ .pipe_target_parens = .{ .expr = apply_fn_idx, .expand = expand_callee } });
                            }
                            return call(exprFrame(ld.right, target_context));
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
                            // Method-insertion syntax is intentionally ungrouped.
                            // Ordinary complete method calls stay grouped so they
                            // continue to mean "call the method result." Other ASTs
                            // follow the general pipe-target grammar.
                            const needs_parens = switch (ld.target_kind) {
                                .method_call => false,
                                .ordinary => right_expr == .method_call or fmt.pipeTargetNeedsParens(ld.right),
                            };
                            if (needs_parens) {
                                return call(.{ .pipe_target_parens = .{
                                    .expr = ld.right,
                                    .expand = right_expr == .multiline_string or right_expr == .typed_multiline_string,
                                } });
                            }
                            return call(exprFrame(ld.right, target_context));
                        },
                    }
                },
                3 => {
                    // The parenthesized callee of `ld.right` was emitted.
                    const apply = fmt.ast.store.getExpr(ld.right).apply;
                    const right_region = fmt.nodeRegion(@backingInt(ld.right));
                    const fn_region = fmt.nodeRegion(@backingInt(apply.@"fn"));
                    const args_region = AST.TokenizedRegion{ .start = fn_region.end, .end = right_region.end };
                    f.phase = 2;
                    return call(try fmt.applyArgsFrame(args_region, fmt.ast.store.getCollectionLayout(ld.right), fmt.ast.store.exprSlice(apply.args)));
                },
                else => return fmt.finishExpr(f),
            },
            .int => |i| {
                try fmt.pushTokenText(i.token);
                return fmt.finishExpr(f);
            },
            .frac => |fr| {
                try fmt.pushTokenText(fr.token);
                return fmt.finishExpr(f);
            },
            .typed_int => |ti| {
                try fmt.pushTokenText(ti.token);
                try fmt.formatLiteralTypeSuffix(ti.type_suffix);
                return fmt.finishExpr(f);
            },
            .typed_frac => |tf| {
                try fmt.pushTokenText(tf.token);
                try fmt.formatLiteralTypeSuffix(tf.type_suffix);
                return fmt.finishExpr(f);
            },
            .list => |l| switch (f.phase) {
                0 => {
                    f.phase = 1;
                    return call(collectionFrame(region, fmt.ast.store.getCollectionLayout(f.ei), .square, .{ .expr = fmt.ast.store.exprSlice(l.items) }));
                },
                else => return fmt.finishExpr(f),
            },
            .tuple => |t| switch (f.phase) {
                0 => {
                    f.phase = 1;
                    const items = fmt.ast.store.exprSlice(t.items);
                    const layout = fmt.ast.store.getCollectionLayout(f.ei);
                    if (items.len == 1 and layout == .compact) {
                        const group_multiline = try fmt.tupleWillBeMultiline(f.ei, t);
                        return call(parenthesizedFrame(t.region, items[0], group_multiline));
                    }
                    return call(collectionFrame(region, layout, .round, .{ .expr = items }));
                },
                else => return fmt.finishExpr(f),
            },
            .tuple_access => |ta| switch (f.phase) {
                0 => {
                    const receiver_expr = fmt.ast.store.getExpr(ta.expr);
                    const flatten_pipe_receiver = receiver_expr == .arrow_call and multiline and !format_context.starts_pipe_target;
                    const parenthesize_receiver = (receiver_expr == .arrow_call and !flatten_pipe_receiver) or fmt.postfixReceiverNeedsParens(ta.expr);
                    f.locals = .{ .postfix = .{ .parenthesize_receiver = parenthesize_receiver, .flatten_pipe_receiver = flatten_pipe_receiver } };
                    if (parenthesize_receiver) try fmt.push('(');
                    f.phase = 1;
                    return call(exprFrame(ta.expr, .{
                        .starts_pipe_target = !parenthesize_receiver and format_context.starts_pipe_target,
                    }));
                },
                else => {
                    const target = result;
                    if (f.locals.postfix.parenthesize_receiver) try fmt.push(')');
                    const layout: PostfixLayout = if (f.locals.postfix.flatten_pipe_receiver or target.ends_with_multiline_string_line) .continuation else .compact;
                    if (multiline) try fmt.formatPostfixBoundary(ta.elem_token, layout, if (target.ends_with_multiline_string_line) .normal else format_behavior);
                    // Get the element index from the token
                    const token_text = fmt.ast.resolve(ta.elem_token);
                    // Token includes leading dot (e.g., ".0")
                    try fmt.pushAll(token_text);
                    return fmt.finishExpr(f);
                },
            },
            .record => |r| {
                const fields = fmt.ast.store.recordFieldSlice(r.fields);
                sw: switch (f.phase) {
                    0 => {
                        try fmt.push('{');
                        const record_multiline = fmt.ast.store.getCollectionLayout(f.ei) == .expanded or
                            try fmt.nodesWillBeMultiline(AST.RecordField.Idx, fields) or fmt.regionHasInteriorComment(r.region);
                        f.locals = .{ .record = .{
                            .multiline = record_multiline,
                            .has_extension = false,
                            .empty_has_comment = r.ext == null and fields.len == 0 and fmt.regionHasInteriorComment(r.region),
                            .next = 0,
                        } };

                        // Handle extension if present
                        if (r.ext) |ext| {
                            if (record_multiline) {
                                fmt.curr_indent += 1;
                                _ = try fmt.flushCommentsBeforeWithSpacing(r.region.start + 1, .{ .after_block_open = true });
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            } else {
                                try fmt.push(' ');
                            }
                            try fmt.pushAll("..");
                            f.phase = 1;
                            return call(exprFrame(ext, .{}));
                        }
                        continue :sw 2;
                    },
                    1 => {
                        const state = &f.locals.record;
                        const ext_region = result.region;
                        state.has_extension = true;

                        try fmt.push(',');
                        if (state.multiline and fields.len > 0) {
                            try fmt.flushItemComments(ext_region.end);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        continue :sw 2;
                    },
                    2 => {
                        const state = &f.locals.record;
                        // Format fields
                        if (state.multiline and !state.has_extension and fields.len > 0) {
                            fmt.curr_indent += 1;
                            _ = try fmt.flushCommentsBeforeWithSpacing(r.region.start + 1, .{ .after_block_open = true });
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        continue :sw 3;
                    },
                    3 => {
                        const state = &f.locals.record;
                        if (state.next < fields.len) {
                            if (!state.multiline) {
                                try fmt.push(' ');
                            }
                            f.phase = 4;
                            return call(.{ .record_field = .{ .idx = fields[state.next] } });
                        }

                        if (state.empty_has_comment) {
                            fmt.curr_indent += 1;
                            // A comment-only `{ }` parses as an empty record; its braces
                            // trim boundary blank lines exactly as a block's do.
                            _ = try fmt.flushCommentsBeforeWithSpacing(fmt.regionClosingToken(r.region).?, .{
                                .after_block_open = true,
                                .before_block_close = true,
                            });
                            fmt.curr_indent -= 1;
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }

                        if ((state.has_extension or fields.len > 0) and !state.multiline) {
                            try fmt.push(' ');
                        }
                        try fmt.push('}');
                        return fmt.finishExpr(f);
                    },
                    4 => {
                        const state = &f.locals.record;
                        const i = state.next;
                        const formatted_field = result;
                        if (state.multiline) {
                            if (formatted_field.ends_with_multiline_string_line or fmt.has_multiline_string) {
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            }
                            try fmt.push(',');
                            try fmt.flushItemComments(formatted_field.region.end);
                            if (i == fields.len - 1) {
                                fmt.curr_indent -= 1;
                            }
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        } else if (i < fields.len - 1) {
                            try fmt.pushAll(",");
                        }
                        state.next += 1;
                        continue :sw 3;
                    },
                    else => unreachable,
                }
            },
            .lambda => |l| {
                const args = fmt.ast.store.patternSlice(l.args);
                const body_region = fmt.nodeRegion(@backingInt(l.body));
                sw: switch (f.phase) {
                    0 => {
                        const args_are_multiline = args.len > 0 and
                            (fmt.ast.store.getCollectionLayout(f.ei) == .expanded or
                                try fmt.nodesWillBeMultiline(AST.Pattern.Idx, args) or
                                fmt.regionHasInteriorComment(.{ .start = l.region.start, .end = body_region.start }));
                        f.locals = .{ .lambda = .{ .args_multiline = args_are_multiline, .next = 0 } };
                        try fmt.push('|');
                        if (args_are_multiline) {
                            fmt.curr_indent += 1;
                            _ = try fmt.flushCommentsBeforeWithSpacing(fmt.nodeRegion(@backingInt(args[0])).start, .{ .after_block_open = true });
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        continue :sw 1;
                    },
                    1 => {
                        const state = &f.locals.lambda;
                        if (state.next < args.len) {
                            f.phase = 2;
                            return call(patternFrame(args[state.next]));
                        }
                        try fmt.push('|');
                        if (try fmt.flushContinuationComments(body_region.start)) {
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 3;
                        return call(exprFrame(l.body, .{}));
                    },
                    2 => {
                        const state = &f.locals.lambda;
                        const i = state.next;
                        const arg_region = result.region;
                        if (state.args_multiline) {
                            try fmt.push(',');
                            try fmt.flushItemComments(arg_region.end);
                            if (i == args.len - 1) {
                                fmt.curr_indent -= 1;
                            }
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        } else if (i < args.len - 1) {
                            try fmt.pushAll(", ");
                        }
                        state.next += 1;
                        continue :sw 1;
                    },
                    else => return fmt.finishExpr(f),
                }
            },
            .unary_op => |op| switch (f.phase) {
                0 => {
                    try fmt.pushTokenText(op.operator);
                    // Bare line breaks after the operator normalize away, but a
                    // comment there must be kept, with the operand moved below it.
                    const operand_start = fmt.nodeRegion(@backingInt(op.expr)).start;
                    if (fmt.hasCommentBefore(operand_start)) {
                        fmt.curr_indent += 1;
                        _ = try fmt.flushCommentsBefore(operand_start);
                        try fmt.pushIndent();
                    }
                    f.phase = 1;
                    return call(exprFrame(op.expr, .{}));
                },
                else => return fmt.finishExpr(f),
            },
            .bin_op => |op| {
                const op_tag = fmt.ast.tokens.tokens.items(.tag)[op.operator];
                const is_range_op = op_tag == .OpDoubleDotLessThan or op_tag == .OpDoubleDotEquals;
                switch (f.phase) {
                    0 => {
                        if (fmt.flags == .debug_binop) {
                            try fmt.push('(');
                            if (multiline) {
                                try fmt.newline();
                                fmt.curr_indent += 1;
                                try fmt.pushIndent();
                            }
                        }
                        f.phase = 1;
                        return call(exprFrame(op.left, .{}));
                    },
                    1 => {
                        const left = result;
                        var pushed = false;
                        if (try fmt.continueAfterMultilineStringLine(left)) {
                            pushed = true;
                        } else if (multiline and try fmt.flushContinuationComments(op.operator)) {
                            try fmt.pushIndent();
                            pushed = true;
                        } else if (!is_range_op) {
                            try fmt.push(' ');
                        }
                        try fmt.pushTokenText(op.operator);
                        const right_region = fmt.nodeRegion(@backingInt(op.right));
                        if (multiline and try fmt.flushCommentsBefore(right_region.start)) {
                            fmt.curr_indent += if (pushed) 0 else 1;
                            try fmt.pushIndent();
                        } else if (!is_range_op) {
                            try fmt.push(' ');
                        }
                        f.phase = 2;
                        return call(exprFrame(op.right, .{}));
                    },
                    else => {
                        if (fmt.flags == .debug_binop) {
                            if (multiline) {
                                fmt.curr_indent -= 1;
                                try fmt.pushIndent();
                            }
                            try fmt.push(')');
                        }
                        return fmt.finishExpr(f);
                    },
                }
            },
            .suffix_single_question => |s| switch (f.phase) {
                0 => {
                    const child_behavior: ExprFormatBehavior = switch (format_behavior) {
                        .normal => .normal,
                        .no_indent_on_access, .no_additional_indent_on_access => .no_additional_indent_on_access,
                    };
                    const child_expr = fmt.ast.store.getExpr(s.expr);
                    const pipe_needs_parens = child_expr == .arrow_call and fmt.ast.store.getExpr(child_expr.arrow_call.right) != .apply;
                    f.phase = 1;
                    if (pipe_needs_parens) {
                        return call(parenthesizedFrame(null, s.expr, try fmt.nodeWillBeMultiline(AST.Expr.Idx, s.expr)));
                    }
                    return call(exprFrame(s.expr, .{
                        .behavior = child_behavior,
                        .question_suffix_follows = child_expr == .arrow_call,
                        .starts_pipe_target = format_context.starts_pipe_target,
                    }));
                },
                else => {
                    _ = try fmt.continueAfterMultilineStringLine(result);
                    try fmt.push('?');
                    return fmt.finishExpr(f);
                },
            },
            .tag => |t| {
                const qualifier_tokens = fmt.ast.store.tokenSlice(t.qualifiers);

                for (qualifier_tokens) |tok_idx| {
                    const tok = @as(Token.Idx, @intCast(tok_idx));
                    try fmt.pushTokenText(tok);
                    try fmt.push('.');
                }

                try fmt.pushTokenText(t.token);
                return fmt.finishExpr(f);
            },
            .if_then_else => |i| {
                // Check if then/else are blocks - blocks use original behavior,
                // non-blocks use base_indent to keep else at the same level as if
                const then_is_block = fmt.ast.store.getExpr(i.then) == .block;
                const else_is_block = fmt.ast.store.getExpr(i.@"else") == .block;
                const has_blocks = then_is_block or else_is_block;
                const then_region = fmt.nodeRegion(@backingInt(i.then));
                switch (f.phase) {
                    0 => {
                        try fmt.pushAll("if");
                        f.locals = .{ .conditional = .{ .base_indent = fmt.curr_indent } };
                        const cond_region = fmt.nodeRegion(@backingInt(i.condition));
                        if (try fmt.flushCommentsBefore(cond_region.start)) {
                            fmt.curr_indent += 1;
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 1;
                        return call(exprFrame(i.condition, .{}));
                    },
                    1 => {
                        const base_indent = f.locals.conditional.base_indent;
                        if (!has_blocks) fmt.curr_indent = base_indent;
                        if (try fmt.flushCommentsBefore(then_region.start)) {
                            fmt.curr_indent += 1;
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 2;
                        return call(exprFrame(i.then, .{}));
                    },
                    2 => {
                        const base_indent = f.locals.conditional.base_indent;
                        if (!has_blocks) fmt.curr_indent = base_indent;
                        if (try fmt.flushCommentsBefore(then_region.end)) {
                            if (has_blocks) fmt.curr_indent += 1;
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        try fmt.pushAll("else");
                        if (!has_blocks) fmt.curr_indent = base_indent;
                        const else_region = fmt.nodeRegion(@backingInt(i.@"else"));
                        if (try fmt.flushCommentsBefore(else_region.start)) {
                            fmt.curr_indent += 1;
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 3;
                        return call(exprFrame(i.@"else", .{}));
                    },
                    else => return fmt.finishExpr(f),
                }
            },
            .if_without_else => |i| {
                // Check if then is a block - blocks use original behavior,
                // non-blocks use base_indent logic
                const then_is_block = fmt.ast.store.getExpr(i.then) == .block;
                switch (f.phase) {
                    0 => {
                        try fmt.pushAll("if");
                        f.locals = .{ .conditional = .{ .base_indent = fmt.curr_indent } };
                        const cond_region = fmt.nodeRegion(@backingInt(i.condition));
                        if (try fmt.flushCommentsBefore(cond_region.start)) {
                            fmt.curr_indent += 1;
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 1;
                        return call(exprFrame(i.condition, .{}));
                    },
                    1 => {
                        if (!then_is_block) fmt.curr_indent = f.locals.conditional.base_indent;
                        const then_region = fmt.nodeRegion(@backingInt(i.then));
                        if (try fmt.flushCommentsBefore(then_region.start)) {
                            fmt.curr_indent += 1;
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 2;
                        return call(exprFrame(i.then, .{}));
                    },
                    else => return fmt.finishExpr(f),
                }
            },
            .match => |m| {
                const branches = fmt.ast.store.matchBranchSlice(m.branches);
                sw: switch (f.phase) {
                    0 => {
                        try fmt.pushAll("match");
                        try fmt.commentBoundary(fmt.nodeRegion(@backingInt(m.expr)).start, true);
                        f.phase = 1;
                        return call(exprFrame(m.expr, .{}));
                    },
                    1 => {
                        try fmt.commentBoundary(result.region.end, true);
                        try fmt.push('{');
                        fmt.curr_indent += 1;
                        f.locals = .{ .match = .{ .branch_indent = fmt.curr_indent, .next = 0 } };
                        if (branches.len == 0) {
                            try fmt.push('}');
                            return fmt.finishExpr(f);
                        }
                        continue :sw 2;
                    },
                    2 => {
                        const state = &f.locals.match;
                        if (state.next < branches.len) {
                            fmt.curr_indent = state.branch_indent;
                            const branch_region = fmt.nodeRegion(@backingInt(branches[state.next]));
                            const branch = fmt.ast.store.getBranch(branches[state.next]);
                            try fmt.flushCommentsBeforeDiscard(branch_region.start);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                            f.phase = 3;
                            return call(patternFrame(branch.pattern));
                        }
                        fmt.curr_indent = state.branch_indent;
                        try fmt.flushCommentsBeforeDiscard(region.end - 1);
                        // Multiline arms can increase curr_indent beyond the branch level.
                        fmt.curr_indent = state.branch_indent - 1;
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                        try fmt.push('}');
                        return fmt.finishExpr(f);
                    },
                    3 => {
                        const branch = fmt.ast.store.getBranch(branches[f.locals.match.next]);
                        if (branch.guard) |guard| {
                            if (try fmt.flushCommentsBefore(result.region.end)) {
                                try fmt.pushIndent();
                            } else {
                                try fmt.push(' ');
                            }
                            try fmt.pushAll("if");
                            const guard_region = fmt.nodeRegion(@backingInt(guard));
                            if (try fmt.flushCommentsBefore(guard_region.start)) {
                                try fmt.pushIndent();
                            } else {
                                try fmt.push(' ');
                            }
                            f.phase = 4;
                            return call(exprFrame(guard, .{}));
                        }
                        try fmt.formatMatchArrow(result.region.end, branch.body);
                        f.phase = 5;
                        return call(exprFrame(branch.body, .{}));
                    },
                    4 => {
                        const branch = fmt.ast.store.getBranch(branches[f.locals.match.next]);
                        try fmt.formatMatchArrow(fmt.nodeRegion(@backingInt(branch.guard.?)).end, branch.body);
                        f.phase = 5;
                        return call(exprFrame(branch.body, .{}));
                    },
                    5 => {
                        f.locals.match.next += 1;
                        continue :sw 2;
                    },
                    else => unreachable,
                }
            },
            .dbg => |d| switch (f.phase) {
                0 => {
                    try fmt.pushAll("dbg");
                    const expr_node = fmt.nodeRegion(@backingInt(d.expr));
                    if (multiline and try fmt.flushContinuationComments(expr_node.start)) {
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    f.phase = 1;
                    return call(exprFrame(d.expr, .{}));
                },
                else => return fmt.finishExpr(f),
            },
            .crash => |c| switch (f.phase) {
                0 => {
                    try fmt.pushAll("crash");
                    const expr_node = fmt.nodeRegion(@backingInt(c.expr));
                    if (multiline and try fmt.flushContinuationComments(expr_node.start)) {
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    f.phase = 1;
                    return call(exprFrame(c.expr, .{}));
                },
                else => return fmt.finishExpr(f),
            },
            .block => |b| {
                const statements = fmt.ast.store.statementSlice(b.statements);
                sw: switch (f.phase) {
                    0 => {
                        if (statements.len > 0) {
                            fmt.curr_indent += 1;
                            try fmt.push('{');
                            try fmt.markRedundantOpenRows(statements, .block);
                            f.locals = .{ .block = .{ .next = 0 } };
                            continue :sw 1;
                        } else if (fmt.regionHasInteriorComment(b.region)) {
                            try fmt.push('{');
                            fmt.curr_indent += 1;
                            _ = try fmt.flushCommentsBeforeWithSpacing(fmt.regionClosingToken(b.region).?, .{
                                .after_block_open = true,
                                .before_block_close = true,
                            });
                            fmt.curr_indent -= 1;
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                            try fmt.push('}');
                        } else {
                            try fmt.pushAll("{}");
                        }
                        return fmt.finishExpr(f);
                    },
                    1 => {
                        const state = &f.locals.block;
                        if (state.next < statements.len) {
                            const s = statements[state.next];
                            const statement_region = fmt.nodeRegion(@backingInt(s));
                            _ = try fmt.flushCommentsBeforeWithSpacing(statement_region.start, .{ .after_block_open = state.next == 0 });
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                            f.phase = 2;
                            return call(.{ .statement = .{ .si = s } });
                        }
                        try fmt.ensureNewline();
                        fmt.curr_indent -= 1;
                        try fmt.pushIndent();
                        try fmt.push('}');
                        return fmt.finishExpr(f);
                    },
                    2 => {
                        const state = &f.locals.block;
                        if (state.next == statements.len - 1) {
                            const statement_region = fmt.nodeRegion(@backingInt(statements[state.next]));
                            _ = try fmt.flushCommentsBeforeWithSpacing(statement_region.end, .{ .before_block_close = true });
                        }
                        state.next += 1;
                        continue :sw 1;
                    },
                    else => unreachable,
                }
            },
            .for_expr => |fe| switch (f.phase) {
                0 => {
                    try fmt.pushAll(forKeyword(fe.kind));
                    try fmt.push(' ');
                    f.phase = 1;
                    return call(patternFrame(fe.patt));
                },
                1 => {
                    Formatter.discardRegion(result.region);
                    try fmt.pushAll(" in ");
                    f.phase = 2;
                    return call(exprFrame(fe.expr, .{}));
                },
                2 => {
                    const body_region = fmt.nodeRegion(@backingInt(fe.body));
                    if (try fmt.flushCommentsBefore(body_region.start)) {
                        fmt.curr_indent += 1;
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    f.phase = 3;
                    return call(exprFrame(fe.body, .{}));
                },
                else => return fmt.finishExpr(f),
            },
            .ellipsis => {
                try fmt.pushAll("...");
                return fmt.finishExpr(f);
            },
            .@"return" => |r| switch (f.phase) {
                0 => {
                    try fmt.pushAll("return");
                    const body_region = fmt.nodeRegion(@backingInt(r.expr));
                    if (multiline and try fmt.flushContinuationComments(body_region.start)) {
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    f.phase = 1;
                    return call(exprFrame(r.expr, .{}));
                },
                else => return fmt.finishExpr(f),
            },
            .@"break" => {
                try fmt.pushAll("break");
                return fmt.finishExpr(f);
            },
            .record_builder => |rb| {
                // Format record builder: { field: value, ... }.TypeName
                const fields = fmt.ast.store.recordFieldSlice(rb.fields);
                sw: switch (f.phase) {
                    0 => {
                        const record_multiline = fmt.ast.store.getCollectionLayout(f.ei) == .expanded or
                            try fmt.nodesWillBeMultiline(AST.RecordField.Idx, fields) or fmt.regionHasInteriorComment(rb.region);
                        f.locals = .{ .record = .{ .multiline = record_multiline, .has_extension = false, .empty_has_comment = false, .next = 0 } };

                        try fmt.push('{');

                        // Format fields like a regular record
                        if (record_multiline and fields.len > 0) {
                            fmt.curr_indent += 1;
                            try fmt.flushCommentsAfterDiscard(rb.region.start);
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        continue :sw 1;
                    },
                    1 => {
                        const state = &f.locals.record;
                        if (state.next < fields.len) {
                            if (!state.multiline) {
                                try fmt.push(' ');
                            }
                            f.phase = 2;
                            return call(.{ .record_field = .{ .idx = fields[state.next] } });
                        }

                        if (fields.len > 0 and !state.multiline) {
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
                                f.phase = 3;
                                return call(exprFrame(rb.mapper, .{}));
                            },
                        }
                        return fmt.finishExpr(f);
                    },
                    2 => {
                        const state = &f.locals.record;
                        const i = state.next;
                        const formatted_field = result;
                        const ends_with_multiline_string_line = formatted_field.ends_with_multiline_string_line or fmt.has_multiline_string;

                        if (i < fields.len - 1) {
                            if (ends_with_multiline_string_line) {
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            }
                            try fmt.push(',');
                            if (state.multiline) {
                                try fmt.flushItemComments(formatted_field.region.end);
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            }
                        } else if (state.multiline) {
                            if (ends_with_multiline_string_line) {
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            }
                            try fmt.push(',');
                            try fmt.flushItemComments(formatted_field.region.end);
                            fmt.curr_indent -= 1;
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        state.next += 1;
                        continue :sw 1;
                    },
                    else => return fmt.finishExpr(f),
                }
            },
            .nominal_apply => |na| switch (f.phase) {
                0 => {
                    // Format nominal value/tuple construction: Type.(arg1, arg2, ...)
                    f.phase = 1;
                    return call(exprFrame(na.mapper, .{}));
                },
                1 => {
                    try fmt.push('.');
                    const mapper_region = fmt.nodeRegion(@backingInt(na.mapper));
                    const args_region = AST.TokenizedRegion{ .start = mapper_region.end, .end = region.end };
                    f.phase = 2;
                    return call(collectionFrame(args_region, fmt.ast.store.getCollectionLayout(f.ei), .round, .{ .expr = fmt.ast.store.exprSlice(na.args) }));
                },
                else => return fmt.finishExpr(f),
            },
            .nominal_record => |nr| {
                const mapper_region = fmt.nodeRegion(@backingInt(nr.mapper));
                switch (f.phase) {
                    0 => {
                        // Unlike field access, `.{}` stays in a pipe's target even
                        // after a newline. Always group a pipe used as the mapper.
                        const parenthesize_mapper = fmt.ast.store.getExpr(nr.mapper) == .arrow_call or
                            fmt.postfixReceiverNeedsParens(nr.mapper);
                        f.locals = .{ .nominal_record = .{ .parenthesize_mapper = parenthesize_mapper } };
                        f.phase = 1;
                        if (parenthesize_mapper) {
                            const expand = try fmt.groupedExprWillBeMultiline(nr.mapper) or fmt.regionHasInteriorComment(mapper_region);
                            return call(parenthesizedFrame(null, nr.mapper, expand));
                        }
                        return call(exprFrame(nr.mapper, .{}));
                    },
                    1 => {
                        const mapper = result;
                        if (fmt.hasCommentBefore(mapper_region.end)) {
                            if (try fmt.flushCommentsBefore(mapper_region.end)) {
                                try fmt.pushIndent();
                            }
                        } else if (!f.locals.nominal_record.parenthesize_mapper) {
                            _ = try fmt.continueAfterMultilineStringLine(mapper);
                        }
                        try fmt.push('.');
                        f.phase = 2;
                        return call(exprFrame(nr.backing, .{}));
                    },
                    else => return fmt.finishExpr(f),
                }
            },
            .malformed => {
                // Output nothing for malformed node
                return fmt.finishExpr(f);
            },
            .record_updater => {
                std.debug.panic("TODO: Handle formatting {s}", .{@tagName(expr)});
            },
        }
    }

    /// The `=>` after a match branch's pattern (and guard, if any), and the
    /// trivia that leads into its body.
    fn formatMatchArrow(fmt: *Formatter, arrow_boundary: Token.Idx, body: AST.Expr.Idx) error{WriteFailed}!void {
        if (try fmt.flushCommentsBefore(arrow_boundary)) {
            fmt.curr_indent += 1;
            try fmt.pushIndent();
            try fmt.pushAll("=>");
        } else {
            try fmt.pushAll(" =>");
        }
        const body_region = fmt.nodeRegion(@backingInt(body));
        if (try fmt.flushCommentsBefore(body_region.start)) {
            fmt.curr_indent += 1;
            try fmt.pushIndent();
        } else {
            try fmt.push(' ');
        }
    }

    const PatternRecordFieldFrame = struct {
        idx: AST.PatternRecordField.Idx,
        phase: u8 = 0,
        indent: u32 = 0,
    };

    fn stepPatternRecordField(fmt: *Formatter, f: *PatternRecordFieldFrame, result: FormattedExpr) FormatAstError!Step {
        const field = fmt.ast.store.getPatternRecordField(f.idx);
        if (f.phase != 0) {
            Formatter.discardRegion(result.region);
            fmt.curr_indent = f.indent;
            return .{ .done = .{ .region = field.region } };
        }
        const multiline = try fmt.nodeWillBeMultiline(AST.PatternRecordField.Idx, f.idx);
        f.indent = fmt.curr_indent;
        if (field.rest) {
            try fmt.pushAll("..");
            if (field.name) |name_tok| {
                if (multiline and try fmt.flushContinuationComments(name_tok)) {
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
                const v_region = fmt.nodeRegion(@backingInt(v));
                if (multiline and try fmt.flushContinuationComments(v_region.start)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                f.phase = 1;
                return call(patternFrame(v));
            }
        }
        fmt.curr_indent = f.indent;
        return .{ .done = .{ .region = field.region } };
    }

    const PatternFrame = struct {
        pi: AST.Pattern.Idx,
        phase: u8 = 0,
        multiline: bool = false,
        indent: u32 = 0,
        next: usize = 0,
    };

    fn stepPattern(fmt: *Formatter, f: *PatternFrame, result: FormattedExpr) FormatAstError!Step {
        const pattern = fmt.ast.store.getPattern(f.pi);
        if (f.phase == 0) {
            f.multiline = try fmt.nodeWillBeMultiline(AST.Pattern.Idx, f.pi);
        }
        const multiline = f.multiline;
        const region: AST.TokenizedRegion = switch (pattern) {
            .malformed => .{ .start = 0, .end = 0 },
            inline .ident,
            .var_ident,
            .tag,
            .string,
            .single_quote,
            .int,
            .frac,
            .typed_int,
            .typed_frac,
            .record,
            .list,
            .tuple,
            .list_rest,
            .underscore,
            .alternatives,
            .as,
            => |p| p.region,
        };
        const done = Step{ .done = .{ .region = region } };
        switch (pattern) {
            .ident => |i| {
                try fmt.formatIdent(i.ident_tok, null);
                return done;
            },
            .var_ident => |i| {
                try fmt.pushAll("var ");
                try fmt.formatIdent(i.ident_tok, null);
                return done;
            },
            .tag => |t| {
                if (f.phase != 0) {
                    Formatter.discardRegion(result.region);
                    return done;
                }
                f.phase = 1;

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
                    return call(patternFrame(args[0]));
                } else if (t.backing_value or t.has_args) {
                    return call(collectionFrame(region, fmt.ast.store.getCollectionLayout(f.pi), .round, .{ .pattern = fmt.ast.store.patternSlice(t.args) }));
                }
                return done;
            },
            .string => |s| {
                try fmt.formatPatternString(s);
                return done;
            },
            .single_quote => |sq| {
                try fmt.formatIdent(sq.token, null);
                if (sq.type_suffix) |type_suffix| {
                    try fmt.formatLiteralTypeSuffix(type_suffix);
                }
                return done;
            },
            .int => |n| {
                try fmt.formatIdent(n.number_tok, null);
                return done;
            },
            .frac => |n| {
                try fmt.formatIdent(n.number_tok, null);
                return done;
            },
            .typed_int => |n| {
                try fmt.formatIdent(n.number_tok, null);
                try fmt.formatLiteralTypeSuffix(n.type_suffix);
                return done;
            },
            .typed_frac => |n| {
                try fmt.formatIdent(n.number_tok, null);
                try fmt.formatLiteralTypeSuffix(n.type_suffix);
                return done;
            },
            .record => |r| {
                if (f.phase != 0) return done;
                f.phase = 1;
                return call(collectionFrame(region, fmt.ast.store.getCollectionLayout(f.pi), .curly, .{ .pattern_record_field = fmt.ast.store.patternRecordFieldSlice(r.fields) }));
            },
            .list => |l| {
                if (f.phase != 0) return done;
                f.phase = 1;
                return call(collectionFrame(region, fmt.ast.store.getCollectionLayout(f.pi), .square, .{ .pattern = fmt.ast.store.patternSlice(l.patterns) }));
            },
            .tuple => |t| {
                if (f.phase != 0) return done;
                f.phase = 1;
                return call(collectionFrame(region, fmt.ast.store.getCollectionLayout(f.pi), .round, .{ .pattern = fmt.ast.store.patternSlice(t.patterns) }));
            },
            .list_rest => |r| {
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
                    if (multiline and try fmt.flushContinuationComments(n)) {
                        try fmt.pushIndent();
                    } else {
                        try fmt.push(' ');
                    }
                    try fmt.pushTokenText(n);
                }
                return done;
            },
            .underscore => {
                try fmt.push('_');
                return done;
            },
            .alternatives => |a| {
                const patterns = fmt.ast.store.patternSlice(a.patterns);
                sw: switch (f.phase) {
                    0 => {
                        f.indent = fmt.curr_indent;
                        continue :sw 1;
                    },
                    1 => {
                        if (f.next < patterns.len) {
                            f.phase = 2;
                            return call(patternFrame(patterns[f.next]));
                        }
                        fmt.curr_indent = f.indent;
                        return done;
                    },
                    else => {
                        Formatter.discardRegion(result.region);
                        const i = f.next;
                        const pattern_region = fmt.nodeRegion(@backingInt(patterns[i]));
                        fmt.curr_indent = f.indent;
                        if (i < patterns.len - 1) {
                            if (multiline) {
                                try fmt.flushCommentsBeforeDiscard(pattern_region.end);
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            } else {
                                try fmt.push(' ');
                            }
                            try fmt.push('|');
                            const next_region = fmt.nodeRegion(@backingInt(patterns[i + 1]));
                            if (multiline and try fmt.flushContinuationComments(next_region.start)) {
                                try fmt.pushIndent();
                            } else {
                                try fmt.push(' ');
                            }
                        }
                        f.next += 1;
                        continue :sw 1;
                    },
                }
            },
            .as => |a| {
                if (f.phase == 0) {
                    f.phase = 1;
                    return call(patternFrame(a.pattern));
                }
                Formatter.discardRegion(result.region);
                try fmt.commentBoundary(a.name - 1, true);
                try fmt.pushAll("as");
                try fmt.commentBoundary(a.name, true);
                try fmt.pushTokenText(a.name);
                return done;
            },
            .malformed => {
                // Output nothing for malformed node
                return done;
            },
        }
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
                    try fmt.commentBoundary(a - 1, true);
                    try fmt.pushAll("as");
                    try fmt.commentBoundary(a, true);
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
                    try fmt.commentBoundary(a - 1, true);
                    try fmt.pushAll("as");
                    try fmt.commentBoundary(a, true);
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
                try fmt.commentBoundary(i.ident + 1, false);
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
        defer fmt.curr_indent = start_indent;

        try fmt.pushAll("targets: {");

        var has_content = false;

        // Format inputs_dir: directory directive if present
        if (targets.inputs_dir) |inputs_token| {
            has_content = true;
            fmt.curr_indent = start_indent + 1;
            // inputs_token is the StringPart in `inputs_dir: "..."`.
            try fmt.flushCommentsBeforeDiscard(inputs_token - 3);
            try fmt.ensureNewline();
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
            fmt.curr_indent = start_indent + 1;
            const entry = fmt.ast.store.getTargetEntry(entry_idx);
            try fmt.flushCommentsBeforeDiscard(entry.region.start);
            try fmt.ensureNewline();
            try fmt.pushIndent();
            try fmt.formatTargetEntry(entry_idx);
            try fmt.push(',');
            if (fmt.ast.tokens.tokenTag(entry.region.end) == .Comma and fmt.hasCommentBefore(entry.region.end)) try fmt.flushCommentsBeforeDiscard(entry.region.end);
        }

        fmt.curr_indent = start_indent + 1;
        const closing_token = fmt.regionClosingToken(targets.region).?;
        if (has_content or fmt.hasCommentBefore(closing_token)) {
            try fmt.flushCommentsBeforeDiscard(closing_token);
            try fmt.ensureNewline();
            fmt.curr_indent = start_indent;
            try fmt.pushIndent();
        }
        try fmt.push('}');
    }

    /// Format a symbol map section: { "roc_main": main_for_host!, ... }
    fn formatSymbolMapSection(fmt: *Formatter, span: AST.SymbolMapEntry.Span, base_indent: u32) FormatAstError!void {
        fmt.curr_indent = base_indent;
        try fmt.formatOrderedCollection(span.region, span.layout, .curly, AST.SymbolMapEntry.Idx, fmt.ast.store.symbolMapEntrySlice(span), Formatter.formatSymbolMapItem, true);
    }

    fn formatSymbolMapItem(fmt: *Formatter, idx: AST.SymbolMapEntry.Idx) FormatAstError!AST.TokenizedRegion {
        try fmt.formatSymbolMapEntry(idx);
        return fmt.ast.store.getSymbolMapEntry(idx).region;
    }

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
        try fmt.commentBoundary(entry.target + 1, false);
        try fmt.push(':');
        try fmt.commentBoundary(fmt.nodeRegion(@backingInt(entry.config)).start, true);
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

    fn formatTargetConfigValue(fmt: *Formatter, root: AST.TargetConfigValue.Idx) (Allocator.Error || error{WriteFailed})!void {
        // Lists still awaiting values, innermost last.
        const OpenList = struct { values: []AST.TargetConfigValue.Idx, next: usize };
        var open_lists: std.ArrayList(OpenList) = .empty;
        defer open_lists.deinit(fmt.ast.gpa);
        var pending: ?AST.TargetConfigValue.Idx = root;
        while (true) {
            if (pending) |value_idx| {
                pending = null;
                switch (fmt.ast.store.getTargetConfigValue(value_idx)) {
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
                        if (values.len > 0) {
                            try open_lists.append(fmt.ast.gpa, .{ .values = values, .next = 0 });
                            pending = values[0];
                            continue;
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
            // A value is complete; continue its enclosing list.
            if (open_lists.items.len == 0) return;
            const list = &open_lists.items[open_lists.items.len - 1];
            list.next += 1;
            if (list.next < list.values.len) {
                try fmt.pushAll(", ");
                pending = list.values[list.next];
            } else {
                try fmt.push(']');
                open_lists.items.len -= 1;
            }
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

    fn formatPackageDependencyRecord(fmt: *Formatter, packages_idx: AST.Collection.Idx, platform_idx: ?AST.RecordField.Idx) FormatAstError!void {
        const packages = fmt.ast.store.getCollection(packages_idx);
        const previous = fmt.platform_dependency;
        fmt.platform_dependency = platform_idx;
        defer fmt.platform_dependency = previous;
        try fmt.formatOrderedCollection(packages.region, packages.layout, .curly, AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(.{ .span = packages.span }), Formatter.formatDependencyField, true);
    }

    fn formatDependencyField(fmt: *Formatter, idx: AST.RecordField.Idx) FormatAstError!AST.TokenizedRegion {
        if (fmt.platform_dependency != null and fmt.platform_dependency.? == idx) {
            const field = fmt.ast.store.getRecordField(idx);
            try fmt.pushTokenText(field.name);
            try fmt.commentBoundary(field.name + 1, false);
            try fmt.push(':');
            try fmt.commentBoundary(field.name + 2, true);
            try fmt.pushAll("platform");
            try fmt.commentBoundary(fmt.nodeRegion(@backingInt(field.value.supplied)).start, true);
            try fmt.formatExprDiscard(field.value.supplied);
            return field.region;
        }
        return fmt.formatRecordField(idx);
    }

    fn formatHeader(fmt: *Formatter, hi: AST.Header.Idx) FormatAstError!void {
        const header = fmt.ast.store.getHeader(hi);
        const start_indent = fmt.curr_indent;
        fmt.roc_version_upgrade = fmt.plannedRocVersionUpgrade(header);
        defer {
            fmt.curr_indent = start_indent;
            fmt.roc_version_upgrade = null;
        }

        const multiline = try fmt.nodeWillBeMultiline(AST.Header.Idx, hi);
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

                try fmt.formatOrderedCollection(
                    provides.region,
                    provides.layout,
                    .square,
                    AST.ExposedItem.Idx,
                    fmt.ast.store.exposedItemSlice(.{ .span = provides.span }),
                    Formatter.formatExposedItem,
                    true,
                );

                if (multiline and try fmt.flushCommentsBefore(provides.region.end)) {
                    if (fmt.curr_indent == start_indent) {
                        fmt.curr_indent += 1;
                    }
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatPackageDependencyRecord(a.packages, a.platform_idx);
            },
            .module => |m| {
                try fmt.pushAll("module");
                const exposes = fmt.ast.store.getCollection(m.exposes);
                if (multiline and try fmt.flushContinuationComments(exposes.region.start)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatOrderedCollection(
                    exposes.region,
                    exposes.layout,
                    .square,
                    AST.ExposedItem.Idx,
                    fmt.ast.store.exposedItemSlice(.{ .span = exposes.span }),
                    Formatter.formatExposedItem,
                    true,
                );
            },
            .hosted => |h| {
                try fmt.pushAll("hosted");
                const exposes = fmt.ast.store.getCollection(h.exposes);
                if (multiline and try fmt.flushContinuationComments(exposes.region.start)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatOrderedCollection(
                    exposes.region,
                    exposes.layout,
                    .square,
                    AST.ExposedItem.Idx,
                    fmt.ast.store.exposedItemSlice(.{ .span = exposes.span }),
                    Formatter.formatExposedItem,
                    true,
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
                try fmt.formatOrderedCollection(
                    exposes.region,
                    exposes.layout,
                    .square,
                    AST.ExposedItem.Idx,
                    exposesItems,
                    Formatter.formatExposedItem,
                    true,
                );
                if (multiline) {
                    try fmt.flushCommentsBeforeDiscard(fmt.ast.store.getCollection(p.packages).region.start);
                    try fmt.ensureNewline();
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatPackageDependencyRecord(p.packages, p.platform_idx);
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

                try fmt.pushAll("requires ");
                try fmt.formatOrderedCollection(p.requires_entries.region, .expanded, .curly, AST.RequiresEntry.Idx, fmt.ast.store.requiresEntrySlice(p.requires_entries), Formatter.formatRequiresEntry, false);
                const exposes = fmt.ast.store.getCollection(p.exposes);
                try fmt.formatSectionBoundary(exposes.region.start - 1, start_indent + 1);
                try fmt.pushAll("exposes");
                if (try fmt.flushContinuationComments(exposes.region.start)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatOrderedCollection(
                    exposes.region,
                    exposes.layout,
                    .square,
                    AST.ExposedItem.Idx,
                    fmt.ast.store.exposedItemSlice(.{ .span = exposes.span }),
                    Formatter.formatExposedItem,
                    true,
                );

                try fmt.flushCommentsBeforeDiscard(exposes.region.end);
                try fmt.ensureNewline();
                fmt.curr_indent = start_indent + 1;
                try fmt.pushIndent();

                try fmt.pushAll("packages");
                const packages = fmt.ast.store.getCollection(p.packages);
                if (try fmt.flushContinuationComments(packages.region.start)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                try fmt.formatPackageDependencyRecord(p.packages, null);

                try fmt.flushCommentsBeforeDiscard(packages.region.end);
                try fmt.ensureNewline();
                fmt.curr_indent = start_indent + 1;
                try fmt.pushIndent();

                try fmt.pushAll("provides ");
                try fmt.formatSymbolMapSection(p.provides, start_indent + 1);

                if (p.hosted.span.len > 0 or fmt.regionHasInteriorComment(p.hosted.region)) {
                    try fmt.formatSectionBoundary(p.hosted.region.start - 1, start_indent + 1);
                    try fmt.pushAll("hosted ");
                    try fmt.formatSymbolMapSection(p.hosted, start_indent + 1);
                }

                // Format targets section if present
                if (p.targets) |targets_idx| {
                    const targets = fmt.ast.store.getTargetsSection(targets_idx);
                    try fmt.formatSectionBoundary(targets.region.start - 1, start_indent + 1);
                    try fmt.formatTargetsSection(targets_idx);
                }
            },
            .type_module => {},
            .default_app => {},
            .malformed => {},
        }
    }

    fn formatRequiresEntry(fmt: *Formatter, idx: AST.RequiresEntry.Idx) FormatAstError!AST.TokenizedRegion {
        const entry = fmt.ast.store.getRequiresEntry(idx);
        const aliases = fmt.ast.store.forClauseTypeAliasSlice(entry.type_aliases);
        if (aliases.len > 0) {
            try fmt.push('[');
            for (aliases, 0..) |alias_idx, i| {
                const alias = fmt.ast.store.getForClauseTypeAlias(alias_idx);
                try fmt.pushTokenText(alias.alias_name);
                try fmt.pushAll(" : ");
                try fmt.pushTokenText(alias.rigid_name);
                if (i + 1 < aliases.len) try fmt.pushAll(", ");
            }
            try fmt.pushAll("] for ");
        }
        try fmt.pushTokenText(entry.entrypoint_name);
        try fmt.commentBoundary(entry.entrypoint_name + 1, true);
        try fmt.push(':');
        try fmt.commentBoundary(fmt.nodeRegion(@backingInt(entry.type_anno)).start, true);
        try fmt.formatTypeAnnoDiscard(entry.type_anno);
        return entry.region;
    }

    fn formatSectionBoundary(fmt: *Formatter, keyword: Token.Idx, indent: u32) error{WriteFailed}!void {
        fmt.curr_indent = indent;
        try fmt.flushCommentsBeforeDiscard(keyword);
        try fmt.ensureNewline();
        try fmt.pushIndent();
    }

    fn nodeRegion(fmt: *Formatter, idx: u32) AST.TokenizedRegion {
        return fmt.ast.store.nodes.items.items(.region)[idx];
    }

    /// Mark the redundant `..` in one statement list's type annotations before
    /// they are formatted.
    fn markRedundantOpenRows(fmt: *Formatter, statements: []const AST.Statement.Idx, scope: StatementScope) Allocator.Error!void {
        const open_rows = fmt.open_rows orelse return;
        try open_rows.markStatements(statements, scope);
    }

    /// Emits a type header's name and returns the frame for its arguments.
    fn typeHeaderFrame(fmt: *Formatter, header: AST.TypeHeader.Idx) FormatAstError!?Frame {
        // Check if the type header node is malformed before calling getTypeHeader
        const h = fmt.ast.store.getTypeHeader(header) catch {
            // Handle malformed type header by outputting placeholder text
            try fmt.pushAll("<malformed>");
            return null;
        };

        try fmt.pushTokenText(h.name);
        if (h.args.span.len > 0) {
            try fmt.commentBoundary(h.name + 1, false);
            return collectionFrame(h.region, fmt.ast.store.getCollectionLayout(header), .round, .{ .type_anno = fmt.ast.store.typeAnnoSlice(h.args) });
        }
        return null;
    }

    const AnnoRecordFieldFrame = struct {
        idx: AST.AnnoRecordField.Idx,
        phase: u8 = 0,
        multiline: bool = false,
        indent: u32 = 0,
    };

    fn stepAnnoRecordField(fmt: *Formatter, f: *AnnoRecordFieldFrame, result: FormattedExpr) FormatAstError!Step {
        const field = fmt.ast.store.getAnnoRecordField(f.idx) catch |err| switch (err) {
            error.MalformedNode => {
                // Return empty region for malformed fields - they were already handled during parsing
                return .{ .done = .{ .region = .{ .start = 0, .end = 0 } } };
            },
        };
        switch (f.phase) {
            0 => {
                f.indent = fmt.curr_indent;
                f.multiline = try fmt.nodeWillBeMultiline(AST.AnnoRecordField.Idx, f.idx);
                const multiline = f.multiline;
                const anno_region = fmt.nodeRegion(@backingInt(field.ty));
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
                if (multiline and try fmt.flushContinuationComments(anno_region.start)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                f.phase = 1;
                return call(typeAnnoFrame(field.ty));
            },
            1 => {
                Formatter.discardRegion(result.region);
                // `name : Type ?? default`—a defaulted field's value expression
                // is part of the annotation and must survive formatting (design.md
                // "Defaulted Fields").
                if (field.default_value) |default_idx| {
                    const multiline = f.multiline;
                    const default_region = fmt.nodeRegion(@backingInt(default_idx));
                    const default_mark = default_region.start - 1;
                    if (comptime builtin.mode == .debug) {
                        std.debug.assert(fmt.ast.tokens.tokenTag(default_mark) == .OpDoubleQuestion);
                    }
                    if (multiline and try fmt.flushContinuationComments(default_mark)) {
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
                    f.phase = 2;
                    return call(exprFrame(default_idx, .{}));
                }
                fmt.curr_indent = f.indent;
                return .{ .done = .{ .region = field.region } };
            },
            else => {
                Formatter.discardRegion(result.region);
                fmt.curr_indent = f.indent;
                return .{ .done = .{ .region = field.region } };
            },
        }
    }

    const WhereClauseFrame = struct {
        idx: AST.WhereClause.Idx,
        phase: u8 = 0,
        indent: u32 = 0,
    };

    fn stepWhereClause(fmt: *Formatter, f: *WhereClauseFrame, result: FormattedExpr) FormatAstError!Step {
        const region = fmt.nodeRegion(@backingInt(f.idx));
        if (f.phase != 0) {
            Formatter.discardRegion(result.region);
            fmt.curr_indent = f.indent;
            return .{ .done = .{ .region = region } };
        }
        f.phase = 1;
        const clause = fmt.ast.store.getWhereClause(f.idx);
        const start_indent = fmt.curr_indent;
        f.indent = start_indent;

        const multiline = try fmt.nodeWillBeMultiline(AST.WhereClause.Idx, f.idx);
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
                try fmt.commentBoundary(c.name_tok + 1, true);
                try fmt.push(':');
                const anno_region = fmt.nodeRegion(@backingInt(c.anno));
                fmt.curr_indent = start_indent;
                if (multiline and try fmt.flushContinuationComments(anno_region.start)) {
                    try fmt.pushIndent();
                } else {
                    try fmt.push(' ');
                }
                return call(typeAnnoFrame(c.anno));
            },
            .mod_alias => |c| {
                // Format as: a.WhereAlias
                try fmt.pushTokenText(c.var_tok);
                if (multiline and try fmt.flushCommentsAfter(c.var_tok)) {
                    fmt.curr_indent = start_indent;
                    try fmt.pushIndent();
                }
                try fmt.push('.');
                return call(typeAnnoFrame(c.alias));
            },
            .malformed => {
                // Output nothing for malformed node
                return .{ .done = .{ .region = region } };
            },
        }
    }

    const TypeAnnoFrame = struct {
        anno: AST.TypeAnno.Idx,
        phase: u8 = 0,
        multiline: bool = false,
        indent: u32 = 0,
        next: usize = 0,
        /// Whether a record or tag union lays its fields or tags out one per line.
        items_multiline: bool = false,
        /// Whether a tag union's anonymous `..` is dropped as redundant.
        drops_open: bool = false,
    };

    fn stepTypeAnno(fmt: *Formatter, f: *TypeAnnoFrame, result: FormattedExpr) FormatAstError!Step {
        const a = fmt.ast.store.getTypeAnno(f.anno);
        const region = fmt.nodeRegion(@backingInt(f.anno));
        if (f.phase == 0) {
            f.multiline = try fmt.nodeWillBeMultiline(AST.TypeAnno.Idx, f.anno);
        }
        const multiline = f.multiline;
        const done = Step{ .done = .{ .region = region } };
        switch (a) {
            .apply => |app| {
                const slice = fmt.ast.store.typeAnnoSlice(app.args);
                switch (f.phase) {
                    0 => {
                        f.phase = 1;
                        return call(typeAnnoFrame(slice[0]));
                    },
                    1 => {
                        Formatter.discardRegion(result.region);
                        f.phase = 2;
                        return call(collectionFrame(app.region, fmt.ast.store.getCollectionLayout(f.anno), .round, .{ .type_anno = slice[1..] }));
                    },
                    else => return done,
                }
            },
            .ty_var => |v| {
                try fmt.pushTokenText(v.tok);
                return done;
            },
            .underscore_type_var => |utv| {
                try fmt.pushTokenText(utv.tok);
                return done;
            },
            .ty => |t| {
                const qualifier_tokens = fmt.ast.store.tokenSlice(t.qualifiers);

                for (qualifier_tokens) |tok_idx| {
                    const tok = @as(Token.Idx, @intCast(tok_idx));
                    try fmt.pushTokenText(tok);
                    try fmt.push('.');
                }

                try fmt.pushTokenText(t.token);
                return done;
            },
            .tuple => |t| {
                if (f.phase != 0) return done;
                f.phase = 1;
                return call(collectionFrame(t.region, fmt.ast.store.getCollectionLayout(f.anno), .round, .{ .type_anno = fmt.ast.store.typeAnnoSlice(t.annos) }));
            },
            .record => |r| {
                const fields = fmt.ast.store.annoRecordFieldSlice(r.fields);
                sw: switch (f.phase) {
                    0 => switch (r.ext) {
                        .closed => {
                            // Regular record without extension - use a plain collection
                            f.phase = 4;
                            return call(collectionFrame(region, fmt.ast.store.getCollectionLayout(f.anno), .curly, .{ .anno_record_field = fields }));
                        },
                        .open, .named => {
                            // Record with extension (e.g., { name: Str, ..ext } or { name: Str, .. })
                            f.items_multiline = fmt.ast.store.getCollectionLayout(f.anno) == .expanded or
                                try fmt.nodesWillBeMultiline(AST.AnnoRecordField.Idx, fields) or
                                fmt.regionHasInteriorComment(region);
                            f.indent = fmt.curr_indent;
                            try fmt.push('{');
                            if (f.items_multiline) {
                                fmt.curr_indent += 1;
                            } else {
                                try fmt.push(' ');
                            }
                            continue :sw 1;
                        },
                    },
                    1 => {
                        if (f.next < fields.len) {
                            const field_region = fmt.nodeRegion(@backingInt(fields[f.next]));
                            if (f.items_multiline) {
                                _ = try fmt.flushCommentsBeforeWithSpacing(field_region.start, .{ .after_block_open = f.next == 0 });
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            }
                            f.phase = 2;
                            return call(.{ .anno_record_field = .{ .idx = fields[f.next] } });
                        }
                        // Handle the record extension (..ext or ..)
                        switch (r.ext) {
                            .named => |named| {
                                if (f.items_multiline) {
                                    try fmt.flushCommentsBeforeDiscard(named.region.start);
                                    try fmt.ensureNewline();
                                    try fmt.pushIndent();
                                }
                                try fmt.pushAll("..");
                                const anno_region = fmt.nodeRegion(@backingInt(named.anno));
                                if (try fmt.flushCommentsBefore(anno_region.start)) {
                                    try fmt.pushIndent();
                                }
                                f.phase = 3;
                                return call(typeAnnoFrame(named.anno));
                            },
                            .open => |tok| {
                                if (f.items_multiline) {
                                    try fmt.flushCommentsBeforeDiscard(tok);
                                    try fmt.ensureNewline();
                                    try fmt.pushIndent();
                                }
                                try fmt.pushAll("..");
                                continue :sw 3;
                            },
                            .closed => unreachable,
                        }
                    },
                    2 => {
                        Formatter.discardRegion(result.region);
                        if (f.items_multiline) {
                            try fmt.push(',');
                        } else {
                            // Every field, including the last, precedes the extension.
                            try fmt.pushAll(", ");
                        }
                        f.next += 1;
                        continue :sw 1;
                    },
                    3 => {
                        if (f.items_multiline) {
                            try fmt.push(',');
                            try fmt.flushCommentsBeforeDiscard(region.end - 1);
                            fmt.curr_indent -= 1;
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        try fmt.push('}');
                        fmt.curr_indent = f.indent;
                        return done;
                    },
                    else => return done,
                }
            },
            .tag_union => |t| {
                const tags = fmt.ast.store.typeAnnoSlice(t.tags);
                sw: switch (f.phase) {
                    0 => {
                        // An anonymous `..` that means what its absence means is dropped.
                        f.drops_open = if (fmt.open_rows) |open_rows| open_rows.isRedundant(f.anno) else false;
                        const is_open = t.ext != .closed and !f.drops_open;
                        f.items_multiline = fmt.ast.store.getCollectionLayout(f.anno) == .expanded or
                            try fmt.nodesWillBeMultiline(AST.TypeAnno.Idx, tags) or fmt.regionHasInteriorComment(region);
                        f.indent = fmt.curr_indent;
                        try fmt.push('[');
                        if (tags.len == 0 and !is_open) {
                            try fmt.push(']');
                            return done;
                        }
                        if (f.items_multiline) {
                            fmt.curr_indent += 1;
                        }
                        continue :sw 1;
                    },
                    1 => {
                        if (f.next < tags.len) {
                            const tag_region = fmt.nodeRegion(@backingInt(tags[f.next]));
                            if (f.items_multiline) {
                                _ = try fmt.flushCommentsBeforeWithSpacing(tag_region.start, .{ .after_block_open = f.next == 0 });
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            }
                            f.phase = 2;
                            return call(typeAnnoFrame(tags[f.next]));
                        }
                        // A dropped `..` keeps the comments written before it.
                        if (f.drops_open and f.items_multiline and fmt.hasCommentBefore(t.ext.open)) {
                            try fmt.flushCommentsBeforeDiscard(t.ext.open);
                        }
                        // Handle open tag unions.
                        if (t.ext != .closed and !f.drops_open) {
                            // Get the token position for flushing comments before the ..
                            const double_dot_token: Token.Idx = switch (t.ext) {
                                .named => |named| named.region.start,
                                .open => |tok| tok,
                                .closed => unreachable, // is_open is true
                            };
                            if (f.items_multiline) {
                                try fmt.flushCommentsBeforeDiscard(double_dot_token);
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            }
                            try fmt.pushAll("..");
                            switch (t.ext) {
                                .named => |named| {
                                    const anno_region = fmt.nodeRegion(@backingInt(named.anno));
                                    if (try fmt.flushCommentsBefore(anno_region.start)) {
                                        try fmt.pushIndent();
                                    }
                                    f.phase = 3;
                                    return call(typeAnnoFrame(named.anno));
                                },
                                .open => {},
                                .closed => unreachable,
                            }
                            continue :sw 3;
                        }
                        continue :sw 4;
                    },
                    2 => {
                        Formatter.discardRegion(result.region);
                        if (f.items_multiline) {
                            try fmt.push(',');
                            if (fmt.ast.tokens.tokenTag(result.region.end) == .Comma and fmt.hasCommentBefore(result.region.end)) {
                                try fmt.flushCommentsBeforeDiscard(result.region.end);
                            }
                        } else if (f.next < (tags.len - 1) or (t.ext != .closed and !f.drops_open)) {
                            try fmt.pushAll(", ");
                        }
                        f.next += 1;
                        continue :sw 1;
                    },
                    3 => {
                        // The open extension was emitted.
                        if (f.items_multiline) {
                            try fmt.push(',');
                        }
                        continue :sw 4;
                    },
                    4 => {
                        if (f.items_multiline) {
                            // Past a dropped `..` only a comment is carried over;
                            // the line break the `..` ended is not.
                            if (!f.drops_open or fmt.hasCommentBefore(region.end - 1)) {
                                try fmt.flushCommentsBeforeDiscard(region.end - 1);
                            }
                            fmt.curr_indent -= 1;
                            try fmt.ensureNewline();
                            try fmt.pushIndent();
                        }
                        try fmt.push(']');
                        fmt.curr_indent = f.indent;
                        return done;
                    },
                    else => unreachable,
                }
            },
            .@"fn" => |fn_anno| {
                const args = fmt.ast.store.typeAnnoSlice(fn_anno.args);
                sw: switch (f.phase) {
                    0 => continue :sw 1,
                    1 => {
                        if (f.next < args.len) {
                            const arg_region = fmt.nodeRegion(@backingInt(args[f.next]));
                            if (multiline and f.next > 0) {
                                try fmt.flushCommentsBeforeDiscard(arg_region.start);
                                try fmt.ensureNewline();
                                try fmt.pushIndent();
                            }
                            f.phase = 2;
                            return call(typeAnnoFrame(args[f.next]));
                        }

                        if (args.len == 0) {
                            try fmt.pushAll("()");
                        }

                        const ret_start = fmt.nodeRegion(@backingInt(fn_anno.ret)).start;
                        try fmt.commentBoundary(ret_start - 1, true);
                        try fmt.pushAll(if (fn_anno.effectful) "=>" else "->");
                        const ret_region = fmt.nodeRegion(@backingInt(fn_anno.ret));
                        if (multiline and try fmt.flushContinuationComments(ret_region.start)) {
                            try fmt.pushIndent();
                        } else {
                            try fmt.push(' ');
                        }
                        f.phase = 3;
                        return call(typeAnnoFrame(fn_anno.ret));
                    },
                    2 => {
                        Formatter.discardRegion(result.region);
                        if (f.next < args.len - 1) {
                            if (multiline) {
                                try fmt.push(',');
                                if (fmt.hasCommentBefore(result.region.end)) try fmt.flushCommentsBeforeDiscard(result.region.end);
                            } else {
                                try fmt.pushAll(", ");
                            }
                        }
                        f.next += 1;
                        continue :sw 1;
                    },
                    else => return done,
                }
            },
            .parens => |p| switch (f.phase) {
                0 => {
                    try fmt.push('(');
                    if (multiline) {
                        try fmt.flushCommentsAfterDiscard(region.start);
                        fmt.curr_indent += 1;
                        try fmt.ensureNewline();
                        try fmt.pushIndent();
                    }
                    f.phase = 1;
                    return call(typeAnnoFrame(p.anno));
                },
                else => {
                    const anno_region = result.region;
                    try fmt.flushCommentsBeforeDiscard(anno_region.end);
                    try fmt.push(')');
                    return done;
                },
            },
            .underscore => {
                try fmt.push('_');
                return done;
            },
            .malformed => {
                // Output nothing for malformed node
                return done;
            },
        }
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

    /// Emit a syntax boundary's comments without preserving bare source line breaks.
    fn commentBoundary(fmt: *Formatter, token: Token.Idx, space: bool) error{WriteFailed}!void {
        if (fmt.hasCommentBefore(token)) {
            _ = try fmt.flushCommentsBefore(token);
            try fmt.pushIndent();
        } else if (space) {
            try fmt.push(' ');
        }
    }

    /// A continuation's leading comments belong to its indentation level.
    fn flushContinuationComments(fmt: *Formatter, token: Token.Idx) error{WriteFailed}!bool {
        const indent = fmt.curr_indent;
        fmt.curr_indent += 1;
        const broke = try fmt.flushCommentsBefore(token);
        if (!broke) fmt.curr_indent = indent;
        return broke;
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
        return fmt.comment_prefix[region.end] != fmt.comment_prefix[region.start + 1];
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
        return fmt.flushCommentsBeforeWithSpacing(tokenIdx, .{ .min_leading_newlines = min_leading_newlines });
    }

    const CommentSpacing = struct {
        min_leading_newlines: u8 = 0,
        after_block_open: bool = false,
        before_block_close: bool = false,
    };

    fn flushCommentsBeforeWithSpacing(fmt: *Formatter, tokenIdx: Token.Idx, spacing: CommentSpacing) error{WriteFailed}!bool {
        const start = if (tokenIdx == 0) 0 else fmt.ast.tokens.resolve(tokenIdx - 1).end.offset;
        const end = fmt.ast.tokens.resolve(tokenIdx).start.offset;
        return fmt.flushComments(start, fmt.ast.env.source[start..end], spacing);
    }

    fn flushCommentsAfter(fmt: *Formatter, tokenIdx: Token.Idx) error{WriteFailed}!bool {
        const start = fmt.ast.tokens.resolve(tokenIdx).end.offset;
        const end = fmt.ast.tokens.resolve(tokenIdx + 1).start.offset;
        return fmt.flushComments(start, fmt.ast.env.source[start..end], .{});
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
                    fmt.setInlineCommentSeparator();
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
                // The comment's line ending was already counted, including both bytes of CRLF.
                if (i < between_text.len and between_text[comment_end] == '\r' and between_text[i] == '\n') i += 1;
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

    /// Delay whitespace until its following comment or token is known, so block
    /// edges can trim blank lines without changing gaps inside the block.
    /// `start_offset` is the absolute source offset of `between_text`.
    fn flushComments(fmt: *Formatter, start_offset: usize, between_text: []const u8, spacing: CommentSpacing) error{WriteFailed}!bool {
        var newline_count: usize = 0;
        var prev_was_comment = false;
        var leading_blank_satisfied = spacing.min_leading_newlines == 0;
        var i: usize = 0;
        while (i < between_text.len) {
            if (between_text[i] == '#') {
                const comment_start = i + 1;
                var comment_end = comment_start;
                while (comment_end < between_text.len and between_text[comment_end] != '\n' and between_text[comment_end] != '\r') {
                    comment_end += 1;
                }

                // Keep inline comments attached to the preceding token. Any
                // required separation before the next definition follows them.
                const is_inline = newline_count == 0 and !fmt.has_newline;
                if (!leading_blank_satisfied and !is_inline) {
                    newline_count = @max(newline_count, spacing.min_leading_newlines);
                    leading_blank_satisfied = true;
                }
                const is_doc_comment = comment_start < between_text.len and between_text[comment_start] == '#';
                if (is_doc_comment and newline_count == 1 and !prev_was_comment and !spacing.after_block_open) {
                    newline_count = 2;
                }

                const limit: usize = if (spacing.after_block_open and !prev_was_comment) 1 else 2;
                for (0..@min(limit, newline_count)) |_| try fmt.newline();
                if (newline_count > 0 or fmt.has_newline) {
                    try fmt.pushIndent();
                } else {
                    fmt.setInlineCommentSeparator();
                }
                try fmt.push('#');
                const comment_text = between_text[comment_start..comment_end];
                // Preserve shebangs and doc-comment markers.
                if (!isShebang(start_offset + i, comment_text) and comment_text.len > 0 and comment_text[0] != ' ' and comment_text[0] != '#') {
                    try fmt.push(' ');
                }
                try fmt.pushAll(comment_text);
                newline_count = 1;
                prev_was_comment = true;
                i = comment_end + 1;
                // Count the comment's line ending once, including CRLF.
                if (i < between_text.len and between_text[comment_end] == '\r' and between_text[i] == '\n') i += 1;
            } else if (between_text[i] == '\n') {
                newline_count += 1;
                i += 1;
            } else {
                i += 1;
            }
        }

        if (!leading_blank_satisfied) {
            newline_count = @max(newline_count, spacing.min_leading_newlines);
        }
        const limit: usize = if (spacing.before_block_close or (spacing.after_block_open and !prev_was_comment)) 1 else 2;
        for (0..@min(limit, newline_count)) |_| try fmt.newline();
        return newline_count > 0;
    }

    inline fn setInlineCommentSeparator(fmt: *Formatter) void {
        std.debug.assert(!fmt.has_newline);
        fmt.pending_spaces = 1;
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

    fn formatLiteralTypeSuffix(fmt: *Formatter, suffix: AST.LiteralTypeSuffix) error{WriteFailed}!void {
        switch (suffix) {
            .path => |path| {
                for (fmt.ast.store.tokenSlice(path.qualifiers)) |qualifier| {
                    try fmt.pushLiteralTypeSuffixSegment(fmt.ast.tokens.resolveIdentifier(@intCast(qualifier)) orelse unreachable);
                }
                try fmt.pushLiteralTypeSuffixSegment(fmt.ast.tokens.resolveIdentifier(path.final_token) orelse unreachable);
            },
            .deprecated_builtin => |type_name| try fmt.pushLiteralTypeSuffixSegment(type_name),
        }
    }

    fn pushLiteralTypeSuffixSegment(fmt: *Formatter, segment: base.Ident.Idx) error{WriteFailed}!void {
        try fmt.push('.');
        try fmt.pushAll(fmt.ast.env.getIdent(segment));
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

    // Pipe-target grouping is not stored as a tuple node. Restore parentheses
    // when attaching a postfix to an expression whose grammar would otherwise
    // absorb that postfix into its operand or body. Numeric receivers also need
    // parentheses to keep the dot from becoming part of the numeric token.
    // Pipe receivers have their own continuation/grouping rule at the call sites.
    fn postfixReceiverNeedsParens(fmt: *Formatter, expr_idx: AST.Expr.Idx) bool {
        return switch (fmt.ast.store.getExpr(expr_idx)) {
            .int,
            .frac,
            .typed_int,
            .typed_frac,
            .bin_op,
            .unary_op,
            .lambda,
            .if_then_else,
            .if_without_else,
            .dbg,
            .crash,
            .@"return",
            .for_expr,
            => true,
            .ident,
            .tag,
            .single_quote,
            .string_part,
            .string,
            .typed_string,
            .multiline_string,
            .typed_multiline_string,
            .list,
            .tuple,
            .record,
            .record_builder,
            .nominal_record,
            .apply,
            .nominal_apply,
            .record_updater,
            .field_access,
            .method_call,
            .tuple_access,
            .suffix_single_question,
            .arrow_call,
            .match,
            .block,
            .ellipsis,
            .@"break",
            .malformed,
            => false,
        };
    }

    // Whether the complete target/callee needs grouping. Named-underscore
    // heads are grouped during emission, leaving their postfix chain outside.
    fn pipeTargetNeedsParens(fmt: *Formatter, expr_idx: AST.Expr.Idx) bool {
        var current = expr_idx;
        while (true) {
            current = switch (fmt.ast.store.getExpr(current)) {
                .ident, .tag => return false,
                .apply => |apply| apply.@"fn",
                .field_access => |access| access.receiver,
                .method_call => |method| method.receiver,
                .tuple_access => |access| access.expr,
                .nominal_apply => |apply| apply.mapper,
                .suffix_single_question => |suffix| suffix.expr,
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
                => return true,
            };
        }
    }

    /// The node kinds whose output layout is predicted. Grouped expressions
    /// have their own rule: grouping normalizes source-only newlines.
    const LayoutKind = enum {
        expr,
        grouped_expr,
        pattern,
        pattern_record_field,
        record_field,
        type_anno,
        anno_record_field,
        where_clause,
        statement,
        header,
        exposed_item,

        fn of(comptime T: type) LayoutKind {
            return if (T == AST.Expr.Idx)
                .expr
            else if (T == AST.Pattern.Idx)
                .pattern
            else if (T == AST.PatternRecordField.Idx)
                .pattern_record_field
            else if (T == AST.RecordField.Idx)
                .record_field
            else if (T == AST.TypeAnno.Idx)
                .type_anno
            else if (T == AST.AnnoRecordField.Idx)
                .anno_record_field
            else if (T == AST.WhereClause.Idx)
                .where_clause
            else if (T == AST.Statement.Idx)
                .statement
            else if (T == AST.Header.Idx)
                .header
            else if (T == AST.ExposedItem.Idx)
                .exposed_item
            else
                @compileError("no layout rule for " ++ @typeName(T));
        }
    };

    const LayoutQuery = struct {
        kind: LayoutKind,
        node: u32,
    };

    /// Layout rules are disjunctions over a node's own source and its
    /// children's layouts, so a query is an `any` evaluation whose leaves are
    /// child queries. The evaluation keeps pending children on heap stacks,
    /// so the input's nesting never becomes native call depth.
    const LayoutEval = collections.AnyAll.Evaluation(LayoutQuery, LayoutRules);

    const LayoutRules = struct {
        fmt: *Formatter,

        /// A cached layout, or the node's rule: decided by its own source, or
        /// an `any` over the child layouts it lists.
        pub fn enter(rules: *LayoutRules, items: LayoutEval.Items, query: LayoutQuery) Allocator.Error!LayoutEval.Expansion {
            const fmt = rules.fmt;
            switch (fmt.layoutCache(query.kind)[query.node]) {
                .compact => return .{ .value = false },
                .expanded => return .{ .value = true },
                .unknown => {},
            }
            if (try fmt.layoutRule(items, query)) {
                fmt.recordLayout(query, true);
                return .{ .value = true };
            }
            return .{ .group = .any };
        }

        pub fn exit(rules: *LayoutRules, query: LayoutQuery, multiline: ?bool) Allocator.Error!void {
            if (multiline) |value| rules.fmt.recordLayout(query, value);
        }
    };

    /// Where a layout rule sends the child layouts it reads.
    const LayoutSink = union(enum) {
        /// Answer each child now.
        query,
        /// List each child as a leaf of the enclosing evaluation.
        rule: LayoutEval.Items,
    };

    fn layoutCache(fmt: *Formatter, kind: LayoutKind) []TypeLayout {
        return if (kind == .grouped_expr) fmt.grouped_layouts else fmt.node_layouts;
    }

    fn recordLayout(fmt: *Formatter, query: LayoutQuery, multiline: bool) void {
        fmt.layoutCache(query.kind)[query.node] = if (multiline) .expanded else .compact;
        if (builtin.is_test) {
            if (query.kind == .grouped_expr) fmt.grouped_layout_computations += 1 else fmt.layout_computations += 1;
        }
    }

    fn queryLayout(fmt: *Formatter, kind: LayoutKind, node: u32) Allocator.Error!bool {
        switch (fmt.layoutCache(kind)[node]) {
            .compact => return false,
            .expanded => return true,
            .unknown => {},
        }
        var rules = LayoutRules{ .fmt = fmt };
        return LayoutEval.runWith(fmt.ast.gpa, &fmt.layout_scratch, &rules, .{ .kind = kind, .node = node });
    }

    /// A child's layout as a rule reads it: answered now for a query, or
    /// listed as a leaf (and not yet known to be multiline) inside a rule.
    fn childLayout(fmt: *Formatter, sink: LayoutSink, kind: LayoutKind, node: u32) Allocator.Error!bool {
        switch (sink) {
            .query => return fmt.queryLayout(kind, node),
            .rule => |items| {
                try items.add(.{ .kind = kind, .node = node });
                return false;
            },
        }
    }

    fn itemsLayout(fmt: *Formatter, sink: LayoutSink, comptime T: type, items: []const T) Allocator.Error!bool {
        // Requires and symbol-map entries are laid out by their collection alone.
        if (T == AST.RequiresEntry.Idx or T == AST.SymbolMapEntry.Idx) return false;
        for (items) |item| {
            if (try fmt.childLayout(sink, .of(T), @backingInt(item))) {
                return true;
            }
        }
        return false;
    }

    // Compact singleton tuples are grouping parentheses. Predict their emitted
    // layout, which discards source-only line breaks inside the grouped expression.
    fn tupleLayout(fmt: *Formatter, sink: LayoutSink, idx: AST.Expr.Idx, tuple: @FieldType(AST.Expr, "tuple")) Allocator.Error!bool {
        const items = fmt.ast.store.exprSlice(tuple.items);
        const layout = fmt.ast.store.getCollectionLayout(idx);
        if (items.len == 1 and layout == .compact) {
            return fmt.regionHasInteriorComment(tuple.region) or try fmt.childLayout(sink, .grouped_expr, @backingInt(items[0]));
        }
        return layout == .expanded or fmt.regionHasInteriorComment(tuple.region) or
            try fmt.itemsLayout(sink, AST.Expr.Idx, items);
    }

    fn interpolationLayout(fmt: *Formatter, sink: LayoutSink, idx: AST.Expr.Idx) Allocator.Error!bool {
        const region = fmt.nodeRegion(@backingInt(idx));
        return fmt.ast.regionIsMultiline(.{ .start = region.start - 1, .end = region.end + 1 }) or
            try fmt.childLayout(sink, .expr, @backingInt(idx));
    }

    fn stringLayout(fmt: *Formatter, sink: LayoutSink, parts: AST.Expr.Span) Allocator.Error!bool {
        for (fmt.ast.store.exprSlice(parts)) |part| {
            if (fmt.ast.store.getExpr(part) != .string_part and try fmt.interpolationLayout(sink, part)) return true;
        }
        return false;
    }

    fn collectionLayout(fmt: *Formatter, sink: LayoutSink, comptime T: type, idx: AST.Collection.Idx) Allocator.Error!bool {
        const collection = fmt.ast.store.getCollection(idx);
        if (collection.layout == .expanded or fmt.regionHasInteriorComment(collection.region)) {
            return true;
        }

        if (T == AST.RecordField.Idx) {
            return fmt.itemsLayout(sink, T, fmt.ast.store.recordFieldSlice(.{ .span = collection.span }));
        }
        if (T == AST.ExposedItem.Idx) {
            return fmt.itemsLayout(sink, T, fmt.ast.store.exposedItemSlice(.{ .span = collection.span }));
        }
        if (T == AST.WhereClause.Idx) {
            return fmt.itemsLayout(sink, T, fmt.ast.store.whereClauseSlice(.{ .span = collection.span }));
        }
        @compileError("no collection layout rule for " ++ @typeName(T));
    }

    fn nodeWillBeMultiline(fmt: *Formatter, comptime T: type, item: T) Allocator.Error!bool {
        return fmt.queryLayout(.of(T), @backingInt(item));
    }

    fn nodesWillBeMultiline(fmt: *Formatter, comptime T: type, items: []const T) Allocator.Error!bool {
        return fmt.itemsLayout(.query, T, items);
    }

    fn groupedExprWillBeMultiline(fmt: *Formatter, expr_idx: AST.Expr.Idx) Allocator.Error!bool {
        return fmt.queryLayout(.grouped_expr, @backingInt(expr_idx));
    }

    fn tupleWillBeMultiline(fmt: *Formatter, idx: AST.Expr.Idx, tuple: @FieldType(AST.Expr, "tuple")) Allocator.Error!bool {
        return fmt.tupleLayout(.query, idx, tuple);
    }

    fn interpolationWillBeMultiline(fmt: *Formatter, idx: AST.Expr.Idx) Allocator.Error!bool {
        return fmt.interpolationLayout(.query, idx);
    }

    fn collectionWillBeMultiline(fmt: *Formatter, comptime T: type, idx: AST.Collection.Idx) Allocator.Error!bool {
        return fmt.collectionLayout(.query, T, idx);
    }

    /// Whether a node's own source decides that it is multiline. Inside an
    /// evaluation, a false answer lists the child layouts that decide it.
    fn layoutRule(fmt: *Formatter, items: LayoutEval.Items, query: LayoutQuery) Allocator.Error!bool {
        return switch (query.kind) {
            .expr => fmt.exprLayoutRule(items, @fromBackingInt(@intCast(query.node))),
            .grouped_expr => fmt.groupedExprLayoutRule(items, @fromBackingInt(@intCast(query.node))),
            .pattern => fmt.patternLayoutRule(items, @fromBackingInt(@intCast(query.node))),
            .pattern_record_field => fmt.patternRecordFieldLayoutRule(items, @fromBackingInt(@intCast(query.node))),
            .record_field => fmt.recordFieldLayoutRule(items, @fromBackingInt(@intCast(query.node))),
            .type_anno => fmt.typeAnnoLayoutRule(items, @fromBackingInt(@intCast(query.node))),
            .anno_record_field => fmt.annoRecordFieldLayoutRule(items, @fromBackingInt(@intCast(query.node))),
            .where_clause => fmt.whereClauseLayoutRule(items, @fromBackingInt(@intCast(query.node))),
            .statement => fmt.statementLayoutRule(items, @fromBackingInt(@intCast(query.node))),
            .header => fmt.headerLayoutRule(items, @fromBackingInt(@intCast(query.node))),
            .exposed_item => fmt.ast.regionIsMultiline(fmt.ast.store.getExposedItem(@fromBackingInt(@intCast(query.node))).to_tokenized_region()),
        };
    }

    fn exprChild(fmt: *Formatter, sink: LayoutSink, idx: AST.Expr.Idx) Allocator.Error!bool {
        return fmt.childLayout(sink, .expr, @backingInt(idx));
    }

    fn groupedExprChild(fmt: *Formatter, sink: LayoutSink, idx: AST.Expr.Idx) Allocator.Error!bool {
        return fmt.childLayout(sink, .grouped_expr, @backingInt(idx));
    }

    fn groupedExprLayoutRule(fmt: *Formatter, items: LayoutEval.Items, expr_idx: AST.Expr.Idx) Allocator.Error!bool {
        const sink: LayoutSink = .{ .rule = items };
        const expr = fmt.ast.store.getExpr(expr_idx);
        if (expr == .method_call) {
            const method = expr.method_call;
            const receiver_region = fmt.nodeRegion(@backingInt(method.receiver));
            // Inserted receiver parentheses normalize bare boundary newlines.
            // Interior comments still require expansion below.
            if (!fmt.postfixReceiverNeedsParens(method.receiver) and
                fmt.ast.regionIsMultiline(.{ .start = receiver_region.end - 1, .end = method.method_token + 1 }))
            {
                return true;
            }
        }

        const expr_tag = std.meta.activeTag(expr);
        const owns_collection = expr_tag == .list or expr_tag == .tuple or expr_tag == .record or
            expr_tag == .record_builder or expr_tag == .apply or expr_tag == .method_call or
            expr_tag == .nominal_apply or expr_tag == .lambda;
        if (owns_collection and fmt.regionHasInteriorComment(expr.to_tokenized_region())) return true;

        return switch (expr) {
            .block, .multiline_string, .typed_multiline_string, .match => true,
            .string => |str| fmt.stringLayout(sink, str.parts),
            .typed_string => |str| fmt.stringLayout(sink, str.parts),
            .list => |l| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                try fmt.itemsLayout(sink, AST.Expr.Idx, fmt.ast.store.exprSlice(l.items)),
            .tuple => |t| fmt.tupleLayout(sink, expr_idx, t),
            .apply => |a| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                try fmt.groupedExprChild(sink, a.@"fn") or
                try fmt.itemsLayout(sink, AST.Expr.Idx, fmt.ast.store.exprSlice(a.args)),
            .bin_op => |b| try fmt.groupedExprChild(sink, b.left) or try fmt.groupedExprChild(sink, b.right),
            .record => |r| blk: {
                if (fmt.ast.store.getCollectionLayout(expr_idx) == .expanded) break :blk true;
                if (r.ext) |ext| {
                    if (try fmt.groupedExprChild(sink, ext)) break :blk true;
                }
                break :blk fmt.itemsLayout(sink, AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(r.fields));
            },
            .record_builder => |rb| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                try fmt.itemsLayout(sink, AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(rb.fields)),
            .nominal_record => |nr| try fmt.groupedExprChild(sink, nr.mapper) or try fmt.groupedExprChild(sink, nr.backing),
            .suffix_single_question => |s| fmt.groupedExprChild(sink, s.expr),
            .tuple_access => |t| fmt.groupedExprChild(sink, t.expr),
            .unary_op => |u| fmt.groupedExprChild(sink, u.expr),
            .field_access => |f| (fmt.ast.store.getExpr(f.receiver) == .arrow_call and try fmt.exprChild(sink, f.receiver)) or
                try fmt.groupedExprChild(sink, f.receiver),
            .method_call => |m| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                (fmt.ast.store.getExpr(m.receiver) == .arrow_call and try fmt.exprChild(sink, m.receiver)) or
                try fmt.groupedExprChild(sink, m.receiver) or
                try fmt.itemsLayout(sink, AST.Expr.Idx, fmt.ast.store.exprSlice(m.args)),
            .nominal_apply => |na| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                try fmt.groupedExprChild(sink, na.mapper) or
                try fmt.itemsLayout(sink, AST.Expr.Idx, fmt.ast.store.exprSlice(na.args)),
            .lambda => |l| fmt.ast.store.getCollectionLayout(expr_idx) == .expanded or
                try fmt.groupedExprChild(sink, l.body) or
                try fmt.itemsLayout(sink, AST.Pattern.Idx, fmt.ast.store.patternSlice(l.args)),
            .if_then_else => |i| try fmt.groupedExprChild(sink, i.condition) or
                try fmt.groupedExprChild(sink, i.then) or
                try fmt.groupedExprChild(sink, i.@"else"),
            .if_without_else => |i| try fmt.groupedExprChild(sink, i.condition) or try fmt.groupedExprChild(sink, i.then),
            .arrow_call => fmt.exprChild(sink, expr_idx),
            .dbg => |d| fmt.groupedExprChild(sink, d.expr),
            .crash => |c| fmt.groupedExprChild(sink, c.expr),
            .@"return" => |r| fmt.groupedExprChild(sink, r.expr),
            .for_expr => |f| try fmt.groupedExprChild(sink, f.expr) or try fmt.groupedExprChild(sink, f.body),
            .int,
            .frac,
            .typed_int,
            .typed_frac,
            .single_quote,
            .string_part,
            .tag,
            .record_updater,
            .ident,
            .ellipsis,
            .@"break",
            .malformed,
            => false,
        };
    }

    fn exprLayoutRule(fmt: *Formatter, items: LayoutEval.Items, item: AST.Expr.Idx) Allocator.Error!bool {
        const sink: LayoutSink = .{ .rule = items };
        const expr = fmt.ast.store.getExpr(item);
        if (expr == .method_call) {
            const method = expr.method_call;
            const receiver_region = fmt.nodeRegion(@backingInt(method.receiver));
            if (!fmt.postfixReceiverNeedsParens(method.receiver) and
                fmt.ast.regionIsMultiline(.{ .start = receiver_region.end - 1, .end = method.method_token + 1 }))
            {
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
            .block, .match => return true,
            .string => |str| return fmt.stringLayout(sink, str.parts),
            .typed_string => |str| return fmt.stringLayout(sink, str.parts),
            .dbg => |d| return fmt.exprChild(sink, d.expr),
            .crash => |c| return fmt.exprChild(sink, c.expr),
            .@"return" => |r| return fmt.exprChild(sink, r.expr),
            .multiline_string, .typed_multiline_string => return true,
            .list => |l| {
                return fmt.ast.store.getCollectionLayout(item) == .expanded or
                    try fmt.itemsLayout(sink, AST.Expr.Idx, fmt.ast.store.exprSlice(l.items));
            },
            .tuple => |t| {
                return fmt.tupleLayout(sink, item, t);
            },
            .apply => |a| {
                if (fmt.ast.store.getCollectionLayout(item) == .expanded) return true;
                if (try fmt.exprChild(sink, a.@"fn")) {
                    return true;
                }

                return fmt.itemsLayout(sink, AST.Expr.Idx, fmt.ast.store.exprSlice(a.args));
            },
            .bin_op => |b| {
                if (try fmt.exprChild(sink, b.left)) {
                    return true;
                }

                return fmt.exprChild(sink, b.right);
            },
            .record => |r| {
                if (fmt.ast.store.getCollectionLayout(item) == .expanded) return true;
                if (r.ext) |ext| {
                    if (try fmt.exprChild(sink, ext)) {
                        return true;
                    }
                }

                return fmt.itemsLayout(sink, AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(r.fields));
            },
            .record_builder => |rb| {
                return fmt.ast.store.getCollectionLayout(item) == .expanded or
                    try fmt.itemsLayout(sink, AST.RecordField.Idx, fmt.ast.store.recordFieldSlice(rb.fields));
            },
            .nominal_record => |nr| {
                if (try fmt.exprChild(sink, nr.mapper)) {
                    return true;
                }

                return fmt.exprChild(sink, nr.backing);
            },
            .suffix_single_question => |s| {
                return fmt.exprChild(sink, s.expr);
            },
            .tuple_access => |t| {
                return fmt.exprChild(sink, t.expr);
            },
            .unary_op => |u| {
                return fmt.exprChild(sink, u.expr);
            },
            .field_access => |f| {
                return fmt.exprChild(sink, f.receiver);
            },
            .method_call => |m| {
                if (fmt.ast.store.getCollectionLayout(item) == .expanded) return true;
                if (try fmt.exprChild(sink, m.receiver)) {
                    return true;
                }

                return fmt.itemsLayout(sink, AST.Expr.Idx, fmt.ast.store.exprSlice(m.args));
            },
            .nominal_apply => |na| {
                if (fmt.ast.store.getCollectionLayout(item) == .expanded) return true;
                if (try fmt.exprChild(sink, na.mapper)) {
                    return true;
                }

                return fmt.itemsLayout(sink, AST.Expr.Idx, fmt.ast.store.exprSlice(na.args));
            },
            .lambda => |l| {
                if (fmt.ast.store.getCollectionLayout(item) == .expanded) return true;
                if (try fmt.exprChild(sink, l.body)) {
                    return true;
                }

                return fmt.itemsLayout(sink, AST.Pattern.Idx, fmt.ast.store.patternSlice(l.args));
            },
            .if_then_else => |i| {
                if (try fmt.exprChild(sink, i.condition)) {
                    return true;
                }

                if (try fmt.exprChild(sink, i.then)) {
                    return true;
                }

                return fmt.exprChild(sink, i.@"else");
            },
            .if_without_else => |i| {
                if (try fmt.exprChild(sink, i.condition)) {
                    return true;
                }

                return fmt.exprChild(sink, i.then);
            },
            .arrow_call => |l| {
                if (try fmt.exprChild(sink, l.left)) {
                    return true;
                }

                return fmt.exprChild(sink, l.right);
            },
            .for_expr => |f| {
                if (try fmt.exprChild(sink, f.expr)) {
                    return true;
                }

                return fmt.exprChild(sink, f.body);
            },
            .int,
            .frac,
            .typed_int,
            .typed_frac,
            .single_quote,
            .string_part,
            .tag,
            .record_updater,
            .ident,
            .ellipsis,
            .@"break",
            .malformed,
            => return false,
        }
    }

    fn patternLayoutRule(fmt: *Formatter, items: LayoutEval.Items, item: AST.Pattern.Idx) Allocator.Error!bool {
        const sink: LayoutSink = .{ .rule = items };
        const pattern = fmt.ast.store.getPattern(item);
        const pattern_has_comment = fmt.regionHasInteriorComment(pattern.to_tokenized_region());
        return switch (pattern) {
            .tag => |t| t.has_args and (pattern_has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                try fmt.itemsLayout(sink, AST.Pattern.Idx, fmt.ast.store.patternSlice(t.args))),
            .record => |r| pattern_has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                try fmt.itemsLayout(sink, AST.PatternRecordField.Idx, fmt.ast.store.patternRecordFieldSlice(r.fields)),
            .list => |l| pattern_has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                try fmt.itemsLayout(sink, AST.Pattern.Idx, fmt.ast.store.patternSlice(l.patterns)),
            .tuple => |t| pattern_has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                try fmt.itemsLayout(sink, AST.Pattern.Idx, fmt.ast.store.patternSlice(t.patterns)),
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
            .malformed,
            => fmt.ast.regionIsMultiline(pattern.to_tokenized_region()),
            .as => |a| fmt.ast.regionIsMultiline(pattern.to_tokenized_region()) or
                try fmt.childLayout(sink, .pattern, @backingInt(a.pattern)),
            .alternatives => |a| fmt.ast.regionIsMultiline(pattern.to_tokenized_region()) or
                try fmt.itemsLayout(sink, AST.Pattern.Idx, fmt.ast.store.patternSlice(a.patterns)),
        };
    }

    fn whereClauseLayoutRule(fmt: *Formatter, items: LayoutEval.Items, item: AST.WhereClause.Idx) Allocator.Error!bool {
        const sink: LayoutSink = .{ .rule = items };
        const clause = fmt.ast.store.getWhereClause(item);
        if (fmt.ast.regionIsMultiline(clause.to_tokenized_region())) return true;
        return switch (clause) {
            .mod_method => |c| fmt.childLayout(sink, .type_anno, @backingInt(c.anno)),
            .mod_alias => |c| fmt.childLayout(sink, .type_anno, @backingInt(c.alias)),
            .malformed => false,
        };
    }

    fn patternRecordFieldLayoutRule(fmt: *Formatter, items: LayoutEval.Items, item: AST.PatternRecordField.Idx) Allocator.Error!bool {
        const sink: LayoutSink = .{ .rule = items };
        const field = fmt.ast.store.getPatternRecordField(item);
        if (fmt.regionHasInteriorComment(field.region)) {
            return true;
        }

        if (field.value) |value| {
            if (try fmt.childLayout(sink, .pattern, @backingInt(value))) {
                return true;
            }
        }

        return false;
    }

    fn recordFieldLayoutRule(fmt: *Formatter, items: LayoutEval.Items, item: AST.RecordField.Idx) Allocator.Error!bool {
        const sink: LayoutSink = .{ .rule = items };
        const field = fmt.ast.store.getRecordField(item);
        if (fmt.regionHasInteriorComment(field.region)) {
            return true;
        }

        if (field.value == .supplied) {
            if (try fmt.exprChild(sink, field.value.supplied)) {
                return true;
            }
        }

        return false;
    }

    fn typeAnnoLayoutRule(fmt: *Formatter, items: LayoutEval.Items, item: AST.TypeAnno.Idx) Allocator.Error!bool {
        const sink: LayoutSink = .{ .rule = items };
        const type_anno = fmt.ast.store.getTypeAnno(item);
        const has_comment = fmt.regionHasInteriorComment(type_anno.to_tokenized_region());
        return switch (type_anno) {
            .apply => |apply| has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                try fmt.itemsLayout(sink, AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(apply.args)),
            .tuple => |tuple| has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                try fmt.itemsLayout(sink, AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(tuple.annos)),
            .record => |record| has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                try fmt.itemsLayout(sink, AST.AnnoRecordField.Idx, fmt.ast.store.annoRecordFieldSlice(record.fields)),
            .tag_union => |tag_union| has_comment or fmt.ast.store.getCollectionLayout(item) == .expanded or
                try fmt.itemsLayout(sink, AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(tag_union.tags)),
            .@"fn" => |function| has_comment or
                try fmt.itemsLayout(sink, AST.TypeAnno.Idx, fmt.ast.store.typeAnnoSlice(function.args)) or
                try fmt.childLayout(sink, .type_anno, @backingInt(function.ret)),
            .parens => |parens| has_comment or fmt.ast.regionIsMultiline(type_anno.to_tokenized_region()) or
                try fmt.childLayout(sink, .type_anno, @backingInt(parens.anno)),
            .ty_var, .underscore_type_var, .underscore, .ty, .malformed => fmt.ast.regionIsMultiline(type_anno.to_tokenized_region()),
        };
    }

    fn annoRecordFieldLayoutRule(fmt: *Formatter, items: LayoutEval.Items, item: AST.AnnoRecordField.Idx) Allocator.Error!bool {
        const sink: LayoutSink = .{ .rule = items };
        const field = fmt.ast.store.getAnnoRecordField(item) catch return false;
        return fmt.regionHasInteriorComment(field.region) or
            try fmt.childLayout(sink, .type_anno, @backingInt(field.ty)) or
            (if (field.default_value) |value| try fmt.exprChild(sink, value) else false);
    }

    fn statementLayoutRule(fmt: *Formatter, items: LayoutEval.Items, item: AST.Statement.Idx) Allocator.Error!bool {
        const sink: LayoutSink = .{ .rule = items };
        const statement = fmt.ast.store.getStatement(item);
        if (fmt.ast.regionIsMultiline(statement.to_tokenized_region())) {
            return true;
        }

        if (std.meta.activeTag(statement) == .expr) {
            return fmt.exprChild(sink, statement.expr.expr);
        }
        return false;
    }

    fn headerLayoutRule(fmt: *Formatter, items: LayoutEval.Items, item: AST.Header.Idx) Allocator.Error!bool {
        const sink: LayoutSink = .{ .rule = items };
        const header = fmt.ast.store.getHeader(item);
        if (fmt.regionHasInteriorComment(header.to_tokenized_region())) return true;
        switch (header) {
            .app => |a| return try fmt.collectionLayout(sink, AST.ExposedItem.Idx, a.provides) or
                try fmt.collectionLayout(sink, AST.RecordField.Idx, a.packages),
            .module => |m| return fmt.collectionLayout(sink, AST.ExposedItem.Idx, m.exposes),
            .hosted => |h| return fmt.collectionLayout(sink, AST.ExposedItem.Idx, h.exposes),
            .package => |p| {
                if (try fmt.collectionLayout(sink, AST.ExposedItem.Idx, p.exposes)) {
                    return true;
                }

                return fmt.collectionLayout(sink, AST.RecordField.Idx, p.packages);
            },
            .platform => return true,
            .type_module, .default_app, .malformed => return false,
        }
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

fn forKeyword(kind: AST.ForKind) []const u8 {
    return switch (kind) {
        .iter => "for",
        .stream => "for!",
    };
}

test "issue 10480: package qualifier preserved in exposed aliased imports" {
    // Repro for https://github.com/roc-lang/roc/issues/10480
    const result = try moduleFmtsStable(std.testing.allocator, "module[o as n,F.s as I]", false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("module [F.s as I, o as n]\n", result);
}

test "package platform dependency formatting is stable" {
    const result = try moduleFmtsStable(
        std.testing.allocator,
        "package[Wrapper]{pf:platform \"../platform/main.roc\",util:\"../util/main.roc\",roc:\"nightly-2026-08-05-24f0b47\"}",
        false,
    );
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings(
        "package [Wrapper] { pf: platform \"../platform/main.roc\", roc: \"nightly-2026-08-05-24f0b47\", util: \"../util/main.roc\" }\n",
        result,
    );
}

test "package platform dependency sorts names with their comments" {
    const input = "package [Wrapper] {\n" ++
        "\t# Utility dependency\n" ++
        "\tutil: \"../util/main.roc\",\n" ++
        "\t# Platform dependency\n" ++
        "\tpf: platform \"../platform/main.roc\",\n" ++
        "\t# Another dependency\n" ++
        "\textra: \"../extra/main.roc\",\n" ++
        "\t# End of dependencies\n" ++
        "}\n";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("package\n" ++
        "\t[Wrapper]\n" ++
        "\t{\n" ++
        "\t\t# Another dependency\n" ++
        "\t\textra: \"../extra/main.roc\",\n" ++
        "\t\t# Platform dependency\n" ++
        "\t\tpf: platform \"../platform/main.roc\",\n" ++
        "\t\t# Utility dependency\n" ++
        "\t\tutil: \"../util/main.roc\",\n" ++
        "\t\t# End of dependencies\n" ++
        "\t}\n", result);
}

test "package platform dependency sorts inline names" {
    const input = "package [Wrapper] { util: \"../util/main.roc\", pf: platform \"../platform/main.roc\" }\n";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("package [Wrapper] { pf: platform \"../platform/main.roc\", util: \"../util/main.roc\" }\n", result);
}

test "issue 11713: match closing brace aligns after a multiline final arm" {
    // Repro for https://github.com/roc-lang/roc/issues/11713
    const inputs = [_][]const u8{
        "f = |x| {\n" ++
            "\tmatch x {\n" ++
            "\t\tOk(v) => v\n" ++
            "\t\tErr(e) =>\n" ++
            "\t\t\tmatch e {\n" ++
            "\t\t\t\tA => 1\n" ++
            "\t\t\t\tB => 2\n" ++
            "\t\t\t}\n" ++
            "\t}\n" ++
            "}\n" ++
            "\n" ++
            "expect f(Ok(3)) == 3\n",
        "f = |x| match x {\n" ++
            "\tOk(v) => v\n" ++
            "\tErr(e) =>\n" ++
            "\t\tmatch e {\n" ++
            "\t\t\tA => 1\n" ++
            "\t\t\tB => 2\n" ++
            "\t\t}\n" ++
            "}\n",
    };
    for (inputs) |input| {
        const result = try moduleFmtsStable(std.testing.allocator, input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(input, result);
    }
}

test "issue 11773: comments preserved around a method list" {
    // Repro for https://github.com/roc-lang/roc/issues/11773
    const cases = [_]struct { input: []const u8, expected: []const u8 }{
        .{
            .input = "MyModule := {\n\t# TODO: write code\n}.{\n\t# TODO: write code\n}\n",
            .expected = "MyModule := {\n\t# TODO: write code\n}.{\n\t# TODO: write code\n}\n",
        },
        .{
            .input = "MyModule := U64.{ # inline\n}",
            .expected = "MyModule := U64.{ # inline\n}\n",
        },
        .{
            .input = "MyModule := U64.{\n\t# first\n\n\t# second\n}",
            .expected = "MyModule := U64.{\n\t# first\n\n\t# second\n}\n",
        },
        .{
            .input = "Outer := U64.{\n\tInner := U64.{\n\t\t# nested\n\t}\n}",
            .expected = "Outer := U64.{\n\tInner := U64.{\n\t\t# nested\n\t}\n}\n",
        },
        .{
            .input = "MyModule := U64 # before dot\n.{ x = 1 }",
            .expected = "MyModule := U64 # before dot\n.{\n\tx = 1\n}\n",
        },
        .{
            .input = "MyModule := U64 # before dot\n.{}",
            .expected = "MyModule := U64 # before dot\n.{}\n",
        },
        .{
            .input = "MyModule := U64. # after dot\n{ x = 1 }",
            .expected = "MyModule := U64. # after dot\n{\n\tx = 1\n}\n",
        },
        .{
            .input = "Outer := U64.{\n\tInner := U64 # before dot\n\t.{ x = 1 }\n}",
            .expected = "Outer := U64.{\n\tInner := U64 # before dot\n\t.{\n\t\tx = 1\n\t}\n}\n",
        },
        .{
            .input = "Foo(a) := List(a) where [a.eq : a, a -> Bool] # before dot\n.{}",
            .expected = "Foo(a) := List(a)\n\twhere [a.eq : a, a -> Bool] # before dot\n\t.{}\n",
        },
        .{
            .input = "MyModule := U64\n.\n{}",
            .expected = "MyModule := U64.{}\n",
        },
    };
    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);
    }
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

test "formatPath check retains full paths after directory traversal" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.createDirPath(io, "d/sub");

    const unformatted = [_][]const u8{ "d/Long.roc", "d/C.roc", "d/sub/Long.roc" };
    for (unformatted) |path| {
        try tmp.dir.writeFile(io, .{ .sub_path = path, .data = "x  =  1\n" });
    }
    try tmp.dir.writeFile(io, .{ .sub_path = "d/Formatted.roc", .data = "x = 1\n" });
    try tmp.dir.writeFile(io, .{ .sub_path = "d/a.md", .data = "Not Roc" });

    var stderr: std.Io.Writer.Allocating = .init(gpa);
    defer stderr.deinit();
    var result = try formatPath(gpa, gpa, tmp.dir, "d", true, .{}, io, &stderr.writer);
    defer result.deinit();
    try std.testing.expectEqual(4, result.success);
    try std.testing.expectEqual(0, result.failure);
    try std.testing.expectEqualStrings("", stderr.written());
    const files = result.unformatted_files.?.items;
    try std.testing.expectEqual(unformatted.len, files.len);
    for (unformatted) |path| {
        const expected = try gpa.dupe(u8, path);
        defer gpa.free(expected);
        for (expected) |*byte| {
            if (byte.* == '/') byte.* = std.fs.path.sep;
        }
        var matches: usize = 0;
        for (files) |actual| {
            if (std.mem.eql(u8, expected, actual)) matches += 1;
        }
        try std.testing.expectEqual(1, matches);
        const contents = try tmp.dir.readFileAlloc(io, path, gpa, .limited(1024));
        defer gpa.free(contents);
        try std.testing.expectEqualStrings("x  =  1\n", contents);
    }
}

test "formatPath check owns single file paths" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "Long.roc", .data = "x  =  1\n" });
    var path = "Long.roc".*;
    var stderr: std.Io.Writer.Allocating = .init(gpa);
    defer stderr.deinit();
    var result = try formatPath(gpa, gpa, tmp.dir, &path, true, .{}, io, &stderr.writer);
    defer result.deinit();
    @memset(&path, 'X');
    try std.testing.expectEqual(1, result.success);
    try std.testing.expectEqual(0, result.failure);
    const files = result.unformatted_files.?.items;
    try std.testing.expectEqual(1, files.len);
    try std.testing.expectEqualStrings("Long.roc", files[0]);
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

test "issue 11244: comment between a tuple receiver and its field access is idempotent" {
    // Repro for https://github.com/roc-lang/roc/issues/11244
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a=((0#
        \\.0))
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "a = (\n\t(\n\t\t(0) #\n\t\t\t.0\n\t)\n)\n",
        result,
    );
}

test "postfix boundaries preserve comments with inserted and existing receiver parentheses" {
    const gpa = std.testing.allocator;
    const receivers = [_]struct { source: []const u8, expected: []const u8 }{
        .{ .source = "0", .expected = "(0)" },
        .{ .source = "1.2", .expected = "(1.2)" },
        .{ .source = "0.U8", .expected = "(0.U8)" },
        .{ .source = "x", .expected = "x" },
        .{ .source = "(x)", .expected = "(x)" },
    };
    for (receivers) |receiver| {
        for ([_][]const u8{ ".0", ".field", ".?field", ".method()" }) |postfix| {
            for ([_][]const u8{ "\n", "\r\n", "\r" }) |line_ending| {
                const source = try std.fmt.allocPrint(gpa, "a=(({s}# keep{s}{s}))", .{ receiver.source, line_ending, postfix });
                defer gpa.free(source);
                const expected = try std.fmt.allocPrint(gpa, "a = (\n\t(\n\t\t{s} # keep\n\t\t\t{s}\n\t)\n)\n", .{ receiver.expected, postfix });
                defer gpa.free(expected);

                const result = try moduleFmtsStable(gpa, source, false);
                defer gpa.free(result);
                try std.testing.expectEqualStrings(expected, result);
            }
        }
    }
}

test "mixed postfix chain preserves each boundary comment once" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a = (0 # tuple
        \\.0 # field
        \\.field # optional
        \\.?field # method
        \\.method() # tuple again
        \\.1)
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "a = (\n" ++
            "\t(0) # tuple\n" ++
            "\t\t.0 # field\n" ++
            "\t\t.field # optional\n" ++
            "\t\t.?field # method\n" ++
            "\t\t.method() # tuple again\n" ++
            "\t\t.1\n" ++
            ")\n",
        result,
    );
}

test "inserted postfix receiver parentheses normalize whitespace-only boundaries" {
    const gpa = std.testing.allocator;
    for ([_][]const u8{ ".0", ".field", ".?field", ".method()" }) |postfix| {
        for ([_][]const u8{ " ", "\n", "\r\n", "\r" }) |gap| {
            const source = try std.fmt.allocPrint(gpa, "a=((0{s}{s}))", .{ gap, postfix });
            defer gpa.free(source);
            const expected = try std.fmt.allocPrint(gpa, "a = (((0){s}))\n", .{postfix});
            defer gpa.free(expected);
            const result = try moduleFmtsStable(gpa, source, false);
            defer gpa.free(result);
            try std.testing.expectEqualStrings(expected, result);
        }
    }
}

test "postfix after multiline string preserves standalone comments" {
    const gpa = std.testing.allocator;
    for ([_][]const u8{ ".0", ".field", ".method()" }) |postfix| {
        const source = try std.fmt.allocPrint(gpa, "a = \\\\text\n# keep\n{s}\n", .{postfix});
        defer gpa.free(source);
        const expected = try std.fmt.allocPrint(gpa, "a = \\\\text\n# keep\n\t{s}\n", .{postfix});
        defer gpa.free(expected);
        const result = try moduleFmtsStable(gpa, source, false);
        defer gpa.free(result);
        try std.testing.expectEqualStrings(expected, result);
    }
}

test "trailing comments count CRLF as one line ending" {
    const result = try moduleFmtsStable(std.testing.allocator, "a=0 # first\r\n# second\r\n", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = 0 # first\n# second\n", result);
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

test "literal method pipe targets and grouped result calls format stably" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a=[1,2,3]|>[1].concat()
        \\b=1|>1.plus()
        \\c="roc "|>"and roll".with_prefix()
        \\d=x|>(receiver.method())
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "a = [1, 2, 3] |> [1].concat()\n" ++
            "\n" ++
            "b = 1 |> (1).plus()\n" ++
            "\n" ++
            "c = \"roc \" |> \"and roll\".with_prefix()\n" ++
            "\n" ++
            "d = x |> (receiver.method())\n",
        result,
    );
}

test "issue 11160: pipe method receivers preserve expression grouping" {
    const cases = [_]struct { input: []const u8, expected: []const u8 }{
        .{ .input = "t=0|>(0%0).y()", .expected = "t = 0 |> (0 % 0).y()\n" },
        .{ .input = "t=x|>(a+b).y()", .expected = "t = x |> (a + b).y()\n" },
        .{ .input = "t=x|>(-a).y()", .expected = "t = x |> (-a).y()\n" },
        .{ .input = "t=x|>(|v|v).y()", .expected = "t = x |> (|v| v).y()\n" },
        .{ .input = "t=x|>(if a b else c).y()", .expected = "t = x |> (if a b else c).y()\n" },
        .{ .input = "t=x|>(dbg a).y()", .expected = "t = x |> (dbg a).y()\n" },
        .{ .input = "t=x|>(crash \"failed\").y()", .expected = "t = x |> (crash \"failed\").y()\n" },
        .{ .input = "t=x|>((a+b).y())", .expected = "t = x |> ((a + b).y())\n" },
        .{ .input = "t=x|>(a+b).field.y()", .expected = "t = x |> (a + b).field.y()\n" },
        .{ .input = "t=x|>(a+b).0.y()", .expected = "t = x |> (a + b).0.y()\n" },
    };
    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);
    }
}

test "issue 11208: named underscore pipe target keeps its grouping parens" {
    // https://github.com/roc-lang/roc/issues/11208
    // A named underscore is not a valid bare pipe target, so the parens around
    // it have to survive formatting for the output to reparse.
    const result = try moduleFmtsStable(std.testing.allocator, "t=0|>(_0).0", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("t = 0 |> (_0).0\n", result);
}

test "issue 11208: pipe start grouping follows callees and receivers only" {
    const cases = [_]struct { input: []const u8, expected: []const u8 }{
        .{ .input = "t=x|>(_0)", .expected = "t = x |> (_0)\n" },
        .{ .input = "t=x|>(_name)", .expected = "t = x |> (_name)\n" },
        .{ .input = "t=x|>(_0)()", .expected = "t = x |> (_0)\n" },
        .{ .input = "t=x|>(_0)(1)", .expected = "t = x |> (_0)(1)\n" },
        .{ .input = "t=x|>(_0)(1)()", .expected = "t = x |> (_0)(1)()\n" },
        .{ .input = "t=x|>(_0)(1).0", .expected = "t = x |> (_0)(1).0\n" },
        .{ .input = "t=x|>(_0).field", .expected = "t = x |> (_0).field\n" },
        .{ .input = "t=x|>(_0).?field", .expected = "t = x |> (_0).?field\n" },
        .{ .input = "t=x|>(_0).field.0.field", .expected = "t = x |> (_0).field.0.field\n" },
        .{ .input = "t=x|>(_0.0)", .expected = "t = x |> (_0).0\n" },
        .{ .input = "t=x|>(_0)?", .expected = "t = x |> (_0)?\n" },
        .{ .input = "t=x|>(_0)()?", .expected = "t = x |> (_0)()?\n" },
        .{ .input = "t=x|>(_0)|>(_1)", .expected = "t = x |> (_0) |> (_1)\n" },
        .{ .input = "t=x|>(_0)(_1)", .expected = "t = x |> (_0)(_1)\n" },
        .{ .input = "t=x|>(_0).method(_1)", .expected = "t = x |> (_0).method(_1)\n" },
        .{ .input = "t=x|>(_0+_1).method()", .expected = "t = x |> (_0 + _1).method()\n" },
        .{ .input = "t=x|>(f).0", .expected = "t = x |> f.0\n" },
        .{ .input = "t=x|>Mod.f(_0)", .expected = "t = x |> Mod.f(_0)\n" },
        .{ .input = "t=x|>Box.(_0)", .expected = "t = x |> Box.(_0)\n" },
        .{ .input = "t=_0.field.0.method(_1)", .expected = "t = _0.field.0.method(_1)\n" },
        .{ .input = "t=x|>(\n_0\n).0", .expected = "t = x\n\t|> (_0).0\n" },
        .{ .input = "t=x|> # target\n(_0).0", .expected = "t = x\n\t|> # target\n\t(_0).0\n" },
    };
    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);
    }
}

test "issue 11208: pipe grouping preserves method insertion and result calls" {
    const cases = [_]struct {
        input: []const u8,
        expected: []const u8,
        target_kind: AST.PipeTargetKind,
        target_tag: std.meta.Tag(AST.Expr),
    }{
        .{ .input = "t=x|>(_0).method()", .expected = "t = x |> (_0).method()\n", .target_kind = .method_call, .target_tag = .method_call },
        .{ .input = "t=x|>(_0.method())", .expected = "t = x |> (_0.method())\n", .target_kind = .ordinary, .target_tag = .method_call },
        .{ .input = "t=x|>(_0).0.method()", .expected = "t = x |> (_0).0.method()\n", .target_kind = .method_call, .target_tag = .method_call },
        .{ .input = "t=x|>(_0).method()()", .expected = "t = x |> (_0).method()()\n", .target_kind = .ordinary, .target_tag = .apply },
        .{ .input = "t=x|>(_0).method().0", .expected = "t = x |> (_0).method().0\n", .target_kind = .ordinary, .target_tag = .tuple_access },
    };
    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);

        // Valid, stable output could still change which call receives the
        // piped argument. Pin the parser's interpretation on both sides.
        for ([_][]const u8{ case.input, result }) |source| {
            var env = try ModuleEnv.init(std.testing.allocator, source);
            defer env.deinit();
            const ast = try parse.file(std.testing.allocator, &env.common);
            defer ast.deinit();
            try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
            const statements = ast.store.statementSlice(ast.store.getFile().statements);
            try std.testing.expectEqual(@as(usize, 1), statements.len);
            const stmt = ast.store.getStatement(statements[0]);
            const pipe = ast.store.getExpr(stmt.decl.body).arrow_call;
            try std.testing.expectEqual(case.target_kind, pipe.target_kind);
            try std.testing.expectEqual(case.target_tag, std.meta.activeTag(ast.store.getExpr(pipe.right)));
        }
    }
}

test "issue 11747: field-value applications preserve pipe call semantics" {
    const cases = [_]struct {
        input: []const u8,
        expected: []const u8,
        applications: usize = 1,
        question: bool = false,
    }{
        .{ .input = "t=2|>(rec.func)(3)", .expected = "t = 2 |> (rec.func)(3)\n" },
        .{ .input = "t=2|>(rec.inner.func)(3)", .expected = "t = 2 |> (rec.inner.func)(3)\n" },
        .{ .input = "t=2|>(rec.func)(3)(4)", .expected = "t = 2 |> (rec.func)(3)(4)\n", .applications = 2 },
        .{ .input = "t=2|>(rec.func)()()", .expected = "t = 2 |> (rec.func)()()\n", .applications = 2 },
        .{ .input = "t=2|>(rec.func)(3)?", .expected = "t = 2 |> (rec.func)(3)?\n", .question = true },
        .{ .input = "t=2|>(rec.func)()?", .expected = "t = 2 |> (rec.func)()?\n", .question = true },
        .{ .input = "t=2|>(rec.func)", .expected = "t = 2 |> rec.func\n", .applications = 0 },
        .{ .input = "t=2|>(rec.func)()", .expected = "t = 2 |> rec.func\n", .applications = 0 },
        .{ .input = "t=2|>(rec.func)(\n# argument\n3\n)", .expected = "t = 2\n\t|> (rec.func)(\n\t\t# argument\n\t\t3,\n\t)\n" },
    };
    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);

        // Idempotence alone cannot detect a stable rewrite into method dispatch.
        for ([_][]const u8{ case.input, result }, 0..) |source, pass| {
            var env = try ModuleEnv.init(std.testing.allocator, source);
            defer env.deinit();
            const ast = try parse.file(std.testing.allocator, &env.common);
            defer ast.deinit();
            try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
            const statements = ast.store.statementSlice(ast.store.getFile().statements);
            const stmt = ast.store.getStatement(statements[0]);
            var root = ast.store.getExpr(stmt.decl.body);
            if (case.question) root = ast.store.getExpr(root.suffix_single_question.expr);
            const pipe = root.arrow_call;
            try std.testing.expectEqual(AST.PipeTargetKind.ordinary, pipe.target_kind);
            var callee = ast.store.getExpr(pipe.right);
            var applications: usize = 0;
            while (callee == .apply) {
                applications += 1;
                callee = ast.store.getExpr(callee.apply.@"fn");
            }
            try std.testing.expectEqual(.field_access, std.meta.activeTag(callee));
            // A direct empty call is allowed to disappear during formatting.
            if (pass == 1 or case.applications != 0) {
                try std.testing.expectEqual(case.applications, applications);
            }
        }
    }
}

test "issue 11747: field-value grouping preserves ordinary and method calls" {
    const cases = [_]struct { input: []const u8, expected: []const u8 }{
        .{ .input = "t=(rec.func)(3)", .expected = "t = (rec.func)(3)\n" },
        .{ .input = "t=(rec.inner.func)()", .expected = "t = (rec.inner.func)()\n" },
        .{ .input = "t=rec.func(3)", .expected = "t = rec.func(3)\n" },
        .{ .input = "t=2|>rec.func(3)", .expected = "t = 2 |> rec.func(3)\n" },
        .{ .input = "t=2|>Mod.func(3)", .expected = "t = 2 |> Mod.func(3)\n" },
    };
    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);
    }
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

test "issue 11045: pipe keeps empty argument lists on method targets" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\my_const = []
        \\
        \\_ = [1, 2, 3] |> my_const.concat()
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "my_const = []\n\n" ++
            "_ = [1, 2, 3] |> my_const.concat()\n",
        result,
    );
}

test "pipe keeps an empty target application after a method call" {
    const result = try moduleFmtsStable(std.testing.allocator, "a=x|>receiver.method()()", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = x |> receiver.method()()\n", result);
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

test "issue 11043: parenthesized pipe target with a comment-only argument list is idempotent" {
    // Repro for https://github.com/roc-lang/roc/issues/11043
    const result = try moduleFmtsStable(std.testing.allocator,
        \\t=0|>(0)(#
        \\)
    , false);
    defer std.testing.allocator.free(result);
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

test "issue 10817: parenthesized multiline string pipe target stays expanded" {
    // https://github.com/roc-lang/roc/issues/10817
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a=()->(\\
        \\)
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = ()\n\t|> (\n\t\t\\\\\n\t)\n", result);
}

test "parenthesized pipe target preserves multiline string indentation" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\a=()|>(\\first
        \\\\second
        \\)
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = ()\n\t|> (\n\t\t\\\\first\n\t\t\\\\second\n\t)\n", result);
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
            .input = "x = (1,)",
            .expected = "x = (\n\t1,\n)\n",
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

test "issue 11176: grouped expression layout follows formatted children" {
    const cases = [_]struct { input: []const u8, expected: []const u8 }{
        .{ .input = "a=(||||||(0\n.0))", .expected = "a = (|| || || ((0).0))\n" },
        .{ .input = "a=((0\n.0))", .expected = "a = (((0).0))\n" },
        .{ .input = "a=[(0\n.0)]", .expected = "a = [((0).0)]\n" },
        .{ .input = "a=f((0\n.0))", .expected = "a = f(((0).0))\n" },
        .{ .input = "a=((# comment\n0))", .expected = "a = (\n\t( # comment\n\t\t0\n\t)\n)\n" },
        .{ .input = "a=((0,))", .expected = "a = (\n\t(\n\t\t0,\n\t)\n)\n" },
    };
    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);
    }
}

test "issue 11273: nominal record mapped from an arrow call is idempotent" {
    // Repro for https://github.com/roc-lang/roc/issues/11273
    // `->` takes only the ident as its target, so `{}->k` is the mapper of the
    // nominal record. Preserve that grouping while migrating the arrow to a pipe.
    const result = try moduleFmtsStable(std.testing.allocator, "a=({}->k.{})", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("a = (({} |> k).{})\n", result);
}

test "issue 11273: nominal record formatting preserves mapper ownership" {
    const Grouping = struct {
        fn unwrap(ast: *const AST, idx: AST.Expr.Idx) AST.Expr {
            var expr_idx = idx;
            var expr = ast.store.getExpr(idx);
            while (expr == .tuple and ast.store.getCollectionLayout(expr_idx) == .compact) {
                const items = ast.store.exprSlice(expr.tuple.items);
                if (items.len != 1) break;
                expr_idx = items[0];
                expr = ast.store.getExpr(expr_idx);
            }
            return expr;
        }
    };
    const cases = [_]struct {
        input: []const u8,
        mapper_tag: std.meta.Tag(AST.Expr),
        pipe_target: bool = false,
        expected: ?[]const u8 = null,
    }{
        .{ .input = "a=({}->k.{})", .mapper_tag = .arrow_call },
        .{ .input = "a={}->k.{}", .mapper_tag = .arrow_call },
        .{ .input = "a={}\n->k.{}", .mapper_tag = .arrow_call, .expected = "a = (\n\t{}\n\t\t|> k\n).{}\n" },
        .{ .input = "a=({}-> # target\nk.{})", .mapper_tag = .arrow_call },
        .{ .input = "a={}-> # target\nk.{}", .mapper_tag = .arrow_call, .expected = "a = (\n\t{}\n\t\t|> # target\n\t\tk\n).{}\n" },
        .{ .input = "a={}->k # mapper\n.{}", .mapper_tag = .arrow_call, .expected = "a = ({} |> k) # mapper\n.{}\n" },
        .{ .input = "a=({}->k).{field:1}", .mapper_tag = .arrow_call },
        .{ .input = "a=x|>k.{}", .mapper_tag = .ident, .pipe_target = true },
        .{ .input = "a=x|>(a+b).{}", .mapper_tag = .bin_op, .pipe_target = true },
        .{ .input = "a=x|>(a+ # operand\nb).{}", .mapper_tag = .bin_op, .pipe_target = true },
        .{ .input = "a=x|>(-a).{}", .mapper_tag = .unary_op, .pipe_target = true },
        .{ .input = "a=x|>(|v|v).{}", .mapper_tag = .lambda, .pipe_target = true },
        .{ .input = "a=x|>(1).{}", .mapper_tag = .int, .pipe_target = true },
        .{ .input = "a=x|>(_0).{}", .mapper_tag = .ident, .pipe_target = true },
    };
    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        if (case.expected) |expected| try std.testing.expectEqualStrings(expected, result);

        // Idempotence alone can accept a changed expression. On both sides,
        // check whether the record owns the pipe or is the pipe's target.
        for ([_][]const u8{ case.input, result }) |source| {
            var env = try ModuleEnv.init(std.testing.allocator, source);
            defer env.deinit();
            const ast = try parse.file(std.testing.allocator, &env.common);
            defer ast.deinit();
            try std.testing.expectEqual(@as(usize, 0), ast.parse_diagnostics.items.len);
            const statements = ast.store.statementSlice(ast.store.getFile().statements);
            try std.testing.expectEqual(@as(usize, 1), statements.len);
            const stmt = ast.store.getStatement(statements[0]);
            var record = Grouping.unwrap(ast, stmt.decl.body);
            if (case.pipe_target) {
                try std.testing.expect(record == .arrow_call);
                try std.testing.expectEqual(AST.PipeTargetKind.ordinary, record.arrow_call.target_kind);
                record = Grouping.unwrap(ast, record.arrow_call.right);
            }
            try std.testing.expect(record == .nominal_record);
            const mapper = Grouping.unwrap(ast, record.nominal_record.mapper);
            try std.testing.expectEqual(case.mapper_tag, std.meta.activeTag(mapper));
            if (mapper == .arrow_call) {
                try std.testing.expectEqual(AST.PipeTargetKind.ordinary, mapper.arrow_call.target_kind);
                try std.testing.expect(Grouping.unwrap(ast, mapper.arrow_call.left) == .record);
                const target = Grouping.unwrap(ast, mapper.arrow_call.right);
                try std.testing.expect(target == .ident);
                try std.testing.expectEqualStrings("k", ast.resolve(target.ident.token));
            }
            try std.testing.expect(ast.store.getExpr(record.nominal_record.backing) == .record);
        }
    }
}

test "issue 10672: parenthesized single-element tuple formatting is idempotent" {
    // Repro for https://github.com/roc-lang/roc/issues/10672
    const result = try moduleFmtsStable(std.testing.allocator, "a=((0,))", false);
    defer std.testing.allocator.free(result);
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

test "issue 11771: comments in populated platform sections" {
    const expected =
        "platform \"pf\"\n" ++
        "\trequires {\n" ++
        "\t\t# Before requirement.\n" ++
        "\t\tmain! : List(Str) => Try({}, [Exit(I32), ..]),\n" ++
        "\n" ++
        "\t\t## Before another requirement.\n" ++
        "\t\tother! : {} => {}\n" ++
        "\t\t# After requirements.\n" ++
        "\t}\n" ++
        "\texposes []\n" ++
        "\tpackages {}\n" ++
        "\tprovides {\n" ++
        "\t\t# Before provided symbol.\n" ++
        "\t\t\"roc_main\": main_for_host!,\n" ++
        "\n" ++
        "\t\t## After provided symbol.\n" ++
        "\t}\n" ++
        "\thosted {\n" ++
        "\t\t# Before hosted symbol.\n" ++
        "\t\t\"host_write\": write!,\n" ++
        "\t}\n" ++
        "\ttargets: {\n" ++
        "\t\t# Before inputs directory.\n" ++
        "\t\tinputs_dir: \"targets/\",\n" ++
        "\n" ++
        "\t\t## Before target.\n" ++
        "\t\tx64musl: { inputs: [\"libhost.a\", app] },\n" ++
        "\t\t# After targets.\n" ++
        "\t}\n";
    const result = try moduleFmtsStable(std.testing.allocator, expected, false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(expected, result);
}

test "issue 11771: inline platform section comments" {
    const expected =
        "platform \"pf\"\n" ++
        "\trequires { # Required callbacks.\n" ++
        "\t\t[Model : model] for main! : Model => {} # Main callback.\n" ++
        "\t}\n" ++
        "\texposes []\n" ++
        "\tpackages {}\n" ++
        "\tprovides {\n" ++
        "\t\t\"roc_main\": main_for_host!, # Host entrypoint.\n" ++
        "\t}\n" ++
        "\ttargets: {\n" ++
        "\t\t# Target without an inputs_dir directive.\n" ++
        "\t\tx64musl: { inputs: [\"libhost.a\", app] }, # Linux host.\n" ++
        "\t}\n";
    const result = try moduleFmtsStable(std.testing.allocator, expected, false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(expected, result);
}

test "issue 11771: comments in empty platform sections" {
    const expected =
        "platform \"pf\"\n" ++
        "\trequires {\n" ++
        "\t\t## Empty requirements.\n" ++
        "\t}\n" ++
        "\texposes []\n" ++
        "\tpackages {}\n" ++
        "\tprovides {\n" ++
        "\t\t# Empty provides.\n" ++
        "\t}\n" ++
        "\ttargets: {\n" ++
        "\t\t# Empty targets.\n" ++
        "\t}\n";
    const result = try moduleFmtsStable(std.testing.allocator, expected, false);
    defer std.testing.allocator.free(result);
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
        \\platform""
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
            "\t\tNow,\n" ++
            "\t\tTime,\n" ++
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

test "where method annotations preserve whole holes parentheses and nullary arrows" {
    const source =
        \\helper : a -> Str where [a.hole : _, a.parenthesized : (_ -> _), a.nullary : () -> _, a.effect! : () => _]
        \\helper = |_| "ok"
        \\
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(source, result);
}

test "issue 11298: comment between unary ! and its operand inside parens formats idempotently" {
    // Repro for https://github.com/roc-lang/roc/issues/11298
    // The comment between `!` and its operand expands the enclosing parens,
    // so formatting must emit that comment rather than drop it; otherwise the
    // second pass sees no comment and collapses the parens.
    const result = try moduleFmtsStable(std.testing.allocator, "n={(!#\n0)}", false);
    defer std.testing.allocator.free(result);

    const commented = try moduleFmtsStable(std.testing.allocator, "n={(!# keep me\n0)}", false);
    defer std.testing.allocator.free(commented);
    try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, commented, "# keep me"));
}

test "comments after a unary operator stay above its indented operand" {
    const result = try moduleFmtsStable(std.testing.allocator, "x = !# a\n  # b\n  !# c\n    y\n", false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("x = ! # a\n\t# b\n\t! # c\n\t\ty\n", result);
}

test "a bare line break after a unary operator normalizes away" {
    const result = try moduleFmtsStable(std.testing.allocator, "x = !\n    y\n", false);
    defer std.testing.allocator.free(result);

    try std.testing.expectEqualStrings("x = !y\n", result);
}

test "blank lines at block beginning/end are removed" {
    // Repro for https://github.com/roc-lang/roc/issues/11774
    // `roc fmt` must drop blank lines immediately after the opening `{` and
    // immediately before the closing `}` of a block, while preserving interior
    // blank lines between statements.
    const input = "main! = |_args| {\n" ++
        "\n" ++
        "\tStdout.line!(\"Hello world!\")?\n" ++
        "\n" ++
        "\tOk({})\n" ++
        "\n" ++
        "}\n";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);

    const expected = "main! = |_args| {\n" ++
        "\tStdout.line!(\"Hello world!\")?\n" ++
        "\n" ++
        "\tOk({})\n" ++
        "}\n";
    try std.testing.expectEqualStrings(expected, result);
}

test "block boundary spacing preserves interior comments and blank lines" {
    const cases = [_]struct { input: []const u8, expected: []const u8 }{
        .{ .input = "x = {\n\n\n  1\n\n\n}\n", .expected = "x = {\n\t1\n}\n" },
        .{ .input = "x = {\n\n y = {\n\n 1\n\n }\n\n y\n\n}\n", .expected = "x = {\n\ty = {\n\t\t1\n\t}\n\n\ty\n}\n" },
        .{ .input = "x = {\n\n # leading\n\n 1\n\n # trailing\n\n}\n", .expected = "x = {\n\t# leading\n\n\t1\n\n\t# trailing\n}\n" },
        .{ .input = "x = {\n ## docs\n y = 1\n y\n}\n", .expected = "x = {\n\t## docs\n\ty = 1\n\ty\n}\n" },
        .{ .input = "x = { # opening\n\n 1 # result\n\n}\n", .expected = "x = { # opening\n\n\t1 # result\n}\n" },
        .{ .input = "x = || {\n\n ## first\n\n # second\n\n}\n", .expected = "x = || {\n\t## first\n\n\t# second\n}\n" },
        .{ .input = "x = || {\n\n}\n", .expected = "x = || {}\n" },
        .{ .input = "x = {\r\n\r\n # leading\r\n\r\n 1 # result\r\n\r\n}\r\n", .expected = "x = {\n\t# leading\n\n\t1 # result\n}\n" },
    };
    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);
    }
}

test "issue 11928: invalid string escapes never overwrite source or emit partial output" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const sources = [_][]const u8{
        \\welcome = |user_name| "Hi ${user_name}, press \F1 for help."
        ,
        \\welcome = "press \F1 for help."
        ,
        \\welcome = "before \u(ZZ) after"
        ,
        \\welcome = "before \u() after"
        ,
    };
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    for (sources) |source| {
        const file = try tmp.dir.createFile(io, "invalid.roc", .{});
        try file.writeStreamingAll(io, source);
        file.close(io);

        var stderr = std.Io.Writer.Allocating.init(gpa);
        defer stderr.deinit();
        var unformatted = std.array_list.Managed([]const u8).init(gpa);
        defer {
            for (unformatted.items) |path| gpa.free(path);
            unformatted.deinit();
        }
        // Both normal formatting and --check must reject the source, rather
        // than classifying the lossy recovery tree as a formatting change.
        for ([_]?*std.array_list.Managed([]const u8){ null, &unformatted }) |check| {
            try std.testing.expectError(error.ParsingFailed, formatFilePath(gpa, tmp.dir, "invalid.roc", check, .{}, io, &stderr.writer));
            const after = try tmp.dir.readFileAlloc(io, "invalid.roc", gpa, .limited(1024));
            defer gpa.free(after);
            try std.testing.expectEqualStrings(source, after);
        }
        try std.testing.expectEqual(@as(usize, 0), unformatted.items.len);
        try std.testing.expect(std.mem.find(u8, stderr.written(), "escape sequence") != null);

        const stdin = try tmp.dir.openFile(io, "invalid.roc", .{});
        defer stdin.close(io);
        const stdout = try tmp.dir.createFile(io, "stdout", .{});
        defer stdout.close(io);
        try std.testing.expectError(error.ParsingFailed, formatStdin(gpa, .{}, io, stdin, stdout, &stderr.writer));
        const output = try tmp.dir.readFileAlloc(io, "stdout", gpa, .limited(1024));
        defer gpa.free(output);
        try std.testing.expectEqualStrings("", output);

        var env = try ModuleEnv.init(gpa, source);
        defer env.deinit();
        const ast = try parse.file(gpa, &env.common);
        defer ast.deinit();
        const formatters = [_]*const fn (AST, *std.Io.Writer) FormatAstError!void{ formatAst, formatHeader, formatExpr, formatStatement };
        for (formatters) |format| {
            var formatted = std.Io.Writer.Allocating.init(gpa);
            defer formatted.deinit();
            try std.testing.expectError(error.ParsingFailed, format(ast.*, &formatted.writer));
            try std.testing.expectEqualStrings("", formatted.written());
        }
    }
}

test "issue 11928: escaped backslash preserves interpolated string content" {
    const source =
        \\welcome = |user_name| "Hi ${user_name}, press \\F1 for help."
    ;
    const formatted = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(formatted);
    try std.testing.expectEqualStrings(source ++ "\n", formatted);
}

test "bidi tokenizer errors prevent every AST formatting entrypoint from writing" {
    const gpa = std.testing.allocator;
    const formatters = [_]*const fn (AST, *std.Io.Writer) FormatAstError!void{ formatAst, formatHeader, formatExpr, formatStatement };
    for (base.bidi.controls) |control| {
        const source = try std.mem.concat(gpa, u8, &.{ "value = 1 # ", control.utf8 });
        defer gpa.free(source);
        var env = try ModuleEnv.init(gpa, source);
        defer env.deinit();
        const ast = try parse.file(gpa, &env.common);
        defer ast.deinit();
        for (formatters) |format| {
            var output = std.Io.Writer.Allocating.init(gpa);
            defer output.deinit();
            try std.testing.expectError(error.ParsingFailed, format(ast.*, &output.writer));
            try std.testing.expectEqual(@as(usize, 0), output.written().len);
        }
    }
}

test "carriage return migration survives diagnostic overflow without hiding other errors" {
    const gpa = std.testing.allocator;
    const prefix = "value = " ++ @as([140]u8, @splat('\r'));
    const migrated = try moduleFmtsStable(gpa, prefix ++ "42\n", false);
    defer gpa.free(migrated);
    try std.testing.expectEqualStrings("value = 42\n", migrated);

    // These errors follow the full diagnostic buffer. They must still block
    // formatting even though the displayed diagnostics are CRs.
    for ([_][]const u8{ "0X42\n", "42 # \u{202e}\n", "\"press \\F1 for help.\"\n" }) |suffix| {
        const source = try std.mem.concat(gpa, u8, &.{ prefix, suffix });
        defer gpa.free(source);
        var env = try ModuleEnv.init(gpa, source);
        defer env.deinit();
        const ast = try parse.file(gpa, &env.common);
        defer ast.deinit();
        try std.testing.expectEqual(@as(usize, 129), ast.tokenize_diagnostics.items.len);
        var output = std.Io.Writer.Allocating.init(gpa);
        defer output.deinit();
        try std.testing.expectError(error.ParsingFailed, formatAst(ast.*, &output.writer));
        try std.testing.expectEqual(@as(usize, 0), output.written().len);
    }
}

test "builtin facts are reused across directory files and later paths" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.createDirPath(io, "files");
    const source = "value : Str -> Range([E, ..])\nvalue = |_| crash \"unused\"\n";
    for ([_][]const u8{ "files/First.roc", "files/Second.roc", "Later.roc" }) |path| {
        try tmp.dir.writeFile(io, .{ .sub_path = path, .data = source });
    }
    var facts = BuiltinFacts{ .allocator = gpa };
    defer facts.deinit();
    const options = Options{ .builtin_facts = &facts };
    var stderr: std.Io.Writer.Allocating = .init(gpa);
    defer stderr.deinit();
    var first = try formatPath(gpa, gpa, tmp.dir, "files", true, options, io, &stderr.writer);
    defer first.deinit();
    try std.testing.expectEqual(2, first.success);
    try std.testing.expectEqual(0, first.failure);
    try std.testing.expectEqual(2, first.unformatted_files.?.items.len);
    const syntax = facts.syntax.?;
    const position_count = syntax.rows.position_cache.count();
    try std.testing.expect(position_count > 0);
    var later = try formatPath(gpa, gpa, tmp.dir, "Later.roc", true, options, io, &stderr.writer);
    defer later.deinit();
    try std.testing.expectEqual(1, later.success);
    try std.testing.expectEqual(0, later.failure);
    try std.testing.expectEqual(syntax, facts.syntax.?);
    try std.testing.expectEqual(position_count, facts.syntax.?.rows.position_cache.count());
    try std.testing.expectEqualStrings("", stderr.written());
}

test "issue 11889: deeply nested collections compute layouts once per node" {
    const gpa = std.testing.allocator;
    const depth = 2000;
    const input = try gpa.alloc(u8, 4 + depth * 2 + 2);
    defer gpa.free(input);
    @memcpy(input[0..4], "x = ");
    @memset(input[4..][0..depth], '[');
    input[4 + depth] = '1';
    @memset(input[5 + depth ..][0..depth], ']');
    input[input.len - 1] = '\n';

    var env = try ModuleEnv.init(gpa, input);
    defer env.deinit();
    const ast = try parse.file(gpa, &env.common);
    defer ast.deinit();
    try std.testing.expectEqual(0, ast.parse_diagnostics.items.len);
    var output = std.Io.Writer.Allocating.init(gpa);
    defer output.deinit();
    var fmt = try Formatter.init(ast.*, &output.writer, .{});
    defer fmt.deinit();
    try fmt.formatFile();
    try fmt.flush();
    try std.testing.expectEqualStrings(input, output.written());
    try std.testing.expect(fmt.layout_computations >= depth);
    try std.testing.expect(fmt.layout_computations <= ast.store.nodeCount());

    // Grouping has different newline rules and therefore its own memo.
    const statement = ast.store.getStatement(ast.store.statementSlice(ast.store.getFile().statements)[0]);
    const expr = statement.decl.body;
    try std.testing.expect(!try fmt.groupedExprWillBeMultiline(expr));
    const computations = fmt.grouped_layout_computations;
    try std.testing.expect(!try fmt.groupedExprWillBeMultiline(expr));
    try std.testing.expectEqual(computations, fmt.grouped_layout_computations);
    try std.testing.expect(computations <= ast.store.nodeCount());
}

test "comment prefix matches inter-token scans for every region" {
    const gpa = std.testing.allocator;
    const input = "x = [ # first\n [1], # second\n \"# string\", 2 # last\n]\n";
    var env = try ModuleEnv.init(gpa, input);
    defer env.deinit();
    const ast = try parse.file(gpa, &env.common);
    defer ast.deinit();
    try std.testing.expectEqual(0, ast.parse_diagnostics.items.len);
    var output = std.Io.Writer.Allocating.init(gpa);
    defer output.deinit();
    var fmt = try Formatter.init(ast.*, &output.writer, .{});
    defer fmt.deinit();
    for (0..ast.tokens.tokens.len + 1) |start| {
        for (start..ast.tokens.tokens.len + 1) |end| {
            var expected = false;
            var token = start + 1;
            while (token < end) : (token += 1) {
                expected = expected or fmt.hasCommentBefore(@intCast(token));
            }
            try std.testing.expectEqual(expected, fmt.regionHasInteriorComment(.{ .start = @intCast(start), .end = @intCast(end) }));
        }
    }
}

test "nested lists and tuples format on a small native stack" {
    const StackTestError = FormatTestError || std.mem.Allocator.Error || error{TestExpectedEqual};
    const Worker = struct {
        fn run(result: *StackTestError!void) void {
            result.* = check();
        }

        fn check() StackTestError!void {
            const gpa = std.testing.allocator;
            const cases = [_]struct { depth: usize, mixed: bool = false, expanded: bool = false }{
                .{ .depth = 10000 },
                .{ .depth = 10000, .mixed = true },
                .{ .depth = 1000, .expanded = true },
            };
            for (cases) |case| {
                const depth = case.depth;
                const closing_width: usize = if (case.expanded) 2 else 1;
                const input = try gpa.alloc(u8, 4 + depth * (1 + closing_width) + 2);
                defer gpa.free(input);
                @memcpy(input[0..4], "x = ");
                for (0..depth) |i| {
                    const tuple = case.mixed and i % 2 == 0;
                    input[4 + i] = if (tuple) '(' else '[';
                    const closing = 5 + depth + (depth - 1 - i) * closing_width;
                    if (case.expanded) input[closing] = ',';
                    input[closing + closing_width - 1] = if (tuple) ')' else ']';
                }
                input[4 + depth] = '1';
                input[input.len - 1] = '\n';
                const formatted = try moduleFmtsStable(gpa, input, false);
                defer gpa.free(formatted);
                if (case.expanded) {
                    try std.testing.expectEqual(depth, std.mem.count(u8, formatted, "["));
                    try std.testing.expectEqual(depth, std.mem.count(u8, formatted, "]"));
                    try std.testing.expectEqual(depth, std.mem.count(u8, formatted, ","));
                } else {
                    try std.testing.expectEqualStrings(input, formatted);
                }
            }
        }
    };
    var result: StackTestError!void = {};
    const thread = try std.Thread.spawn(.{ .stack_size = 256 * 1024 }, Worker.run, .{&result});
    thread.join();
    try result;
}

test "issue 11936: formatting is stable across nested layouts" {
    const inputs = [_][]const u8{
        "connect : ({ ports : List(U16) ?? [80, 443,] }) -> Str",
        "{\n\n    field } = record\n\nprocess = |\n    Foo(value) as whole,\n    other,\n| value",
        "matched = [match status { Ok(value) => value, Err(_) => 0 }]\n\nmessages = [\"items: ${[1, 2,]}\"]\n\ninspected = [dbg [1, 2,]]",
        "result = match value {\n    Some(item)\n        if is_valid(item) => transform(item)\n    None => default_value\n}",
        "total = (\n    items.keep_if(\n        is_valid\n    ).len()\n)",
        "result = input |> (\n    step_one\n    |> step_two\n).finalize()",
    };
    for (inputs) |input| {
        const result = try moduleFmtsStable(std.testing.allocator, input, false);
        defer std.testing.allocator.free(result);
    }
}

test "issue 11930: first doc comments follow opening delimiters directly" {
    const inputs = [_][]const u8{
        "Doc := {\n\t## First field.\n\thost : Str,\n\t## Second field.\n\tport : U64,\n}",
        "xs = [\n\t## First item.\n\t1,\n]",
        "f = |\n\t## First argument.\n\tfactor,\n| factor * 2",
        "r = {\n\t## First field.\n\ta: 1,\n}",
        "f = || {\n\t## Return value.\n\tx\n}",
    };
    for (inputs) |input| {
        const result = try moduleFmtsStable(std.testing.allocator, input, false);
        defer std.testing.allocator.free(result);
        const first_comment = std.mem.find(u8, result, "\t##").?;
        try std.testing.expect(first_comment >= 2 and result[first_comment - 2] != '\n');
        try std.testing.expect(std.mem.find(u8, result, "\n\t##") != null);
    }
}

test "issue 11929: continuation comments share their expression indentation" {
    const inputs = [_][]const u8{
        "result =\n    # Continuation.\n    List.fold(items, 0, |acc, x| acc + x)",
        "report = |total, tax_rate| \"total: ${\n    # Continuation.\n    Num.to_str(total)\n} (tax ${\n    Num.to_str(tax_rate)\n})\"",
        "f = |a|\n    # Continuation.\n    a + 1",
        "Handler :\n    # Continuation.\n    U64 -> U64",
    };
    for (inputs) |input| {
        const result = try moduleFmtsStable(std.testing.allocator, input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expect(std.mem.find(u8, result, "\n# Continuation.") == null);
        try std.testing.expect(std.mem.find(u8, result, "\n\t# Continuation.") != null);
    }
}

test "issue 11927: comments between all platform sections survive" {
    const input =
        "platform \"\"\n" ++
        "    requires { process_string : Str -> Str }\n" ++
        "    # Before exposes.\n" ++
        "    exposes [Helper, Core]\n" ++
        "    # Before packages.\n" ++
        "    packages {}\n" ++
        "    # Before provides.\n" ++
        "    provides { \"roc_process_string\": process_string_for_host }\n" ++
        "    # Before hosted.\n" ++
        "    hosted { \"print\": print! }\n" ++
        "    # Before targets.\n" ++
        "    targets: { x64mac: { inputs: [app] }, }\n";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);
    for ([_][]const u8{ "exposes", "packages", "provides", "hosted", "targets" }) |section| {
        const comment = try std.fmt.allocPrint(std.testing.allocator, "# Before {s}.", .{section});
        defer std.testing.allocator.free(comment);
        try std.testing.expect(std.mem.count(u8, result, comment) == 1);
    }
}

test "issue 11926: trailing comments survive without a source comma" {
    const inputs = [_][]const u8{
        "retry! = |attempt| {\n    match attempt {\n        Ok(v) => Stdout.line!(\"ok\")\n        Err(e) => Stdout.line!(\"err\") # Last item.\n    }\n}",
        "config = {\n    host: \"localhost\",\n    port: 8080 # Last item.\n}",
        "render : List(a) -> Str where [\n    a.label : a -> Str # Last item.\n]",
        "scale = |\n    factor, # First item.\n    clamp # Last item.\n| factor * clamp",
    };
    for (inputs) |input| {
        const result = try moduleFmtsStable(std.testing.allocator, input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expect(std.mem.count(u8, result, "# Last item.") == 1);
    }
}

test "issue 4140: mistaken record assignment separators format to colons" {
    const cases = [_]struct { input: []const u8, expected: []const u8 }{
        .{ .input = "record = { x = 1, y: 2 }", .expected = "record = { x: 1, y: 2 }\n" },
        .{ .input = "record = { ..old, x = 1 }", .expected = "record = { ..old, x: 1 }\n" },
        .{ .input = "record = { x: 1, y = 2 }", .expected = "record = { x: 1, y: 2 }\n" },
    };
    for (cases) |case| {
        const result = try parseAndFmtCountingDiags(std.testing.allocator, case.input, 1);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);
        const stable = try moduleFmtsStable(std.testing.allocator, result, false);
        defer std.testing.allocator.free(stable);
    }
}

test "issue 3486: headers and import exposures sort types before values" {
    const cases = [_]struct { input: []const u8, expected: []const u8 }{
        .{ .input = "module [z, B, a, A]", .expected = "module [A, B, a, z]\n" },
        .{ .input = "hosted [z, B, a, A]", .expected = "hosted [A, B, a, z]\n" },
        .{ .input = "import Foo exposing [z, B, a, A]", .expected = "import Foo exposing [A, B, a, z]\n" },
        .{ .input = "package [z, B, a, A] { z: \"z\", a: \"a\" }", .expected = "package [A, B, a, z] { a: \"a\", z: \"z\" }\n" },
    };
    for (cases) |case| {
        const result = try moduleFmtsStable(std.testing.allocator, case.input, false);
        defer std.testing.allocator.free(result);
        try std.testing.expectEqualStrings(case.expected, result);
    }
}

test "issue 3486: sorting carries leading and inline comments with their entries" {
    const input = "module [\n    z, # z inline.\n    ## A documentation.\n    A,\n    B, # B inline.\n    a # a inline.\n]";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "module [\n\t## A documentation.\n\tA,\n\tB, # B inline.\n\ta, # a inline.\n\tz, # z inline.\n]\n",
        result,
    );
}

test "issue 3486: platform requires packages and host symbols sort by name" {
    const input = "platform \"p\" requires { z : Str -> Str, a : Str -> Str } exposes [] packages { z: \"z\", a: \"a\" } provides { \"z\": z, \"a\": a } hosted { \"z\": z!, \"a\": a! } targets: {}";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);
    try std.testing.expect(std.mem.find(u8, result, "a : Str").? < std.mem.find(u8, result, "z : Str").?);
    try std.testing.expect(std.mem.find(u8, result, "packages { a:") != null);
    try std.testing.expect(std.mem.find(u8, result, "provides { \"a\": a, \"z\": z }") != null);
    try std.testing.expect(std.mem.find(u8, result, "hosted { \"a\": a!, \"z\": z! }") != null);
}

test "issue 3157: multiline parenthesized call arguments outdent" {
    const result = try moduleFmtsStable(std.testing.allocator,
        \\result = Task.from_result(
        \\    (
        \\        {
        \\            a = binary_op(ctx)
        \\            if a == b {
        \\                -1
        \\            } else {
        \\                0
        \\            }
        \\        }
        \\    )
        \\)
    , false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(
        "result = Task.from_result((\n" ++
            "\t{\n" ++
            "\t\ta = binary_op(ctx)\n" ++
            "\t\tif a == b {\n" ++
            "\t\t\t-1\n" ++
            "\t\t} else {\n" ++
            "\t\t\t0\n" ++
            "\t\t}\n" ++
            "\t}\n" ++
            "))\n",
        result,
    );
}

test "issue 3486: opening delimiter comments stay at the collection boundary" {
    const result = try moduleFmtsStable(std.testing.allocator, "module [ # Header comment.\n z, # z comment.\n a,\n]", false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("module [ # Header comment.\n\ta,\n\tz, # z comment.\n]\n", result);
}

test "issue 11926: multiline string separator whitespace is emitted once" {
    const source = "r = {\n\tx: \\\\value\n\t,\n}\n";
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings(source, result);
}

test "nested expressions, patterns, types, and statements format on a small native stack" {
    const StackTestError = FormatTestError || std.mem.Allocator.Error || error{TestExpectedEqual};
    const Case = struct {
        head: []const u8,
        open: []const u8,
        core: []const u8,
        close: []const u8,
        tail: []const u8 = "",
        depth: usize = 10000,
        /// Whether the input is already formatted. Expanded shapes indent
        /// every level, so they nest less deeply to keep output small.
        canonical: bool = true,
    };
    const cases = [_]Case{
        .{ .head = "x = ", .open = "{ a: ", .core = "1", .close = " }" },
        .{ .head = "x = ", .open = "f(", .core = "1", .close = ")" },
        .{ .head = "x = a", .open = "", .core = "", .close = ".f()" },
        .{ .head = "x = ", .open = "!", .core = "a", .close = "" },
        .{ .head = "x = ", .open = "1 + (", .core = "1", .close = ")" },
        .{ .head = "x = 1", .open = "", .core = "", .close = " + 1" },
        .{ .head = "x = ", .open = "|a| ", .core = "a", .close = "" },
        .{ .head = "x = ", .open = "if a b else ", .core = "c", .close = "" },
        .{ .head = "x = a", .open = "", .core = "", .close = " |> f" },
        .{ .head = "x = a |> f", .open = "", .core = "", .close = "()" },
        .{ .head = "x = ", .open = "f(", .core = "a", .close = "?)" },
        .{ .head = "x = ", .open = "\"${", .core = "a", .close = "}\"" },
        .{ .head = "x = ", .open = "dbg ", .core = "a", .close = "" },
        .{ .head = "x = ", .open = "T.(", .core = "a", .close = ")" },
        .{ .head = "x = ", .open = "[{ a: f(|b| (", .core = "1", .close = ")) }]" },
        .{ .head = "f = |", .open = "[", .core = "a", .close = "]", .tail = "| a" },
        .{ .head = "f = |", .open = "Ok(", .core = "a", .close = ")", .tail = "| a" },
        .{ .head = "f = |", .open = "{ a: ", .core = "b", .close = " }", .tail = "| b" },
        .{ .head = "f = |", .open = "(", .core = "a", .close = ", b)", .tail = "| a" },
        .{ .head = "f = |", .open = "[{ a: Ok((", .core = "b", .close = ", c)) }]", .tail = "| b" },
        .{ .head = "x : ", .open = "List(", .core = "U8", .close = ")" },
        .{ .head = "x : ", .open = "{ a : ", .core = "U8", .close = " }" },
        .{ .head = "x : ", .open = "{ a : ", .core = "U8", .close = ", .. }" },
        .{ .head = "x : ", .open = "(", .core = "U8", .close = ", U8)" },
        .{ .head = "x : ", .open = "[A(", .core = "U8", .close = ")]" },
        .{ .head = "x : ", .open = "(U8 -> ", .core = "U8", .close = ")" },
        .{ .head = "x : ", .open = "List({ a : [A((", .core = "U8", .close = ", U8))] })" },
        .{ .head = "x = ", .open = "{\ny = ", .core = "1", .close = "\ny\n}", .depth = 1000, .canonical = false },
        .{ .head = "x = ", .open = "match a {\n_ => ", .core = "1", .close = "\n}", .depth = 1000, .canonical = false },
        .{ .head = "x = ", .open = "[|a| {\nb = ", .core = "1", .close = "\nb\n}]", .depth = 1000, .canonical = false },
        .{
            .head = "platform \"\"\n\trequires {}\n\texposes []\n\tpackages {}\n\tprovides {}\n\ttargets: {\n\t\tx64mac: {\n\t\t\tinputs: [app],\n\t\t\tother: ",
            .open = "[",
            .core = "1",
            .close = "]",
            .tail = ",\n\t\t},\n\t}",
        },
    };
    const Worker = struct {
        fn run(result: *StackTestError!void) void {
            result.* = check();
        }

        fn check() StackTestError!void {
            const gpa = std.testing.allocator;
            for (cases) |case| {
                var input: std.ArrayList(u8) = .empty;
                defer input.deinit(gpa);
                try input.appendSlice(gpa, case.head);
                for (0..case.depth) |_| try input.appendSlice(gpa, case.open);
                try input.appendSlice(gpa, case.core);
                for (0..case.depth) |_| try input.appendSlice(gpa, case.close);
                try input.appendSlice(gpa, case.tail);
                try input.append(gpa, '\n');
                const formatted = try moduleFmtsStable(gpa, input.items, false);
                defer gpa.free(formatted);
                if (case.canonical) try std.testing.expectEqualStrings(input.items, formatted);
            }
        }
    };
    var result: StackTestError!void = {};
    const thread = try std.Thread.spawn(.{ .stack_size = 256 * 1024 }, Worker.run, .{&result});
    thread.join();
    try result;
}

test "issue 12019: comment before match guard is preserved" {
    const input = "f = |p| match p {\n\tA # This comment will be deleted!!!\n\tif is_ok => 1\n\t_ => 0\n}\n";
    const result = try moduleFmtsStable(std.testing.allocator, input, false);
    defer std.testing.allocator.free(result);
    try std.testing.expect(std.mem.find(u8, result, "# This comment will be deleted!!!") != null);
}

test "issue 12018: expanded tag pattern under as is stable" {
    const result = try moduleFmtsStable(std.testing.allocator, "f = |Ok(x,) as whole| x\n", false);
    defer std.testing.allocator.free(result);
}

test "issue 12018: expanded type application in where clause is stable" {
    const result = try moduleFmtsStable(std.testing.allocator, "f : a -> U64 where [a.h : List(U64,)]\nf = |x| 0\n", false);
    defer std.testing.allocator.free(result);
}

test "issue 12019: comments on both sides of match guard keyword are preserved once" {
    const inputs = [_][]const u8{
        "f = |p| match p {\n\tA # before if\n\tif # after if\n\tis_ok => 1\n\t_ => 0\n}\n",
        "f = |p| match p {\n\tOk(x,) # before if\n\tif # after if\n\tis_ok => x\n\t_ => 0\n}\n",
        "f = |p| match p {\n\tA if # after if\n\tis_ok => 1\n\t_ => 0\n}\n",
    };
    for (inputs) |input| {
        const result = try moduleFmtsStable(std.testing.allocator, input, false);
        defer std.testing.allocator.free(result);
        for ([_][]const u8{ "# before if", "# after if" }) |comment| {
            try std.testing.expectEqual(std.mem.count(u8, input, comment), std.mem.count(u8, result, comment));
        }
    }
}

test "issue 12018: pattern wrappers propagate expanded children" {
    const inputs = [_][]const u8{
        "f = |{ x, } as whole| x\n",
        "f = |[x,] as whole| x\n",
        "f = |(x,) as whole| x\n",
        "f = |p| match p { Ok(x,) | Err(x) => x }\n",
    };
    for (inputs) |input| {
        const result = try moduleFmtsStable(std.testing.allocator, input, false);
        defer std.testing.allocator.free(result);
    }
}

test "issue 12018: where clauses propagate nested expanded types" {
    const inputs = [_][]const u8{
        "f : a -> U64 where [a.h : List(List(U64,))]\nf = |x| 0\n",
        "f : a -> U64 where [a.Alias(U64,)]\nf = |x| 0\n",
        "f : a -> U64 where [a.h : (U64,)]\nf = |x| 0\n",
    };
    for (inputs) |input| {
        const result = try moduleFmtsStable(std.testing.allocator, input, false);
        defer std.testing.allocator.free(result);
    }
}

test "issue 12040: inter-token comments case 01" {
    const source =
        \\module [User]
        \\
        \\User : {
        \\    name : Str # login handle
        \\    , age : U64,
        \\}
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 02" {
    const source =
        \\module [total]
        \\
        \\total = add(price # in cents
        \\    , tax)
        \\
        \\add = |a, b| a + b
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 03" {
    const source =
        \\module [add]
        \\
        \\add : U64 # running total
        \\    , U64 -> U64
        \\
        \\add = |a, b| a + b
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 04" {
    const source =
        \\module [ports]
        \\
        \\ports = [80 # plain HTTP
        \\    , 443]
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 05" {
    const source =
        \\module [Pair]
        \\
        \\Pair(first # left element
        \\    , second) := (first, second)
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 06" {
    const source =
        \\module [rest_of]
        \\
        \\rest_of = |rows| match rows {
        \\    [first # skip the header row
        \\        , .. as rest] => rest
        \\    _ => []
        \\}
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 07" {
    const source =
        \\module [owner_name]
        \\
        \\owner_name = |user| match user {
        \\    { name # display name
        \\        , .. } => name
        \\    _ => "anonymous"
        \\}
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 08" {
    const source =
        \\module [Status]
        \\
        \\Status : [Active # serving traffic
        \\    , Inactive]
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 09" {
    const source =
        \\module [Scores]
        \\
        \\Scores : Dict(Str # player name
        \\    , U64)
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 10" {
    const source =
        \\module [summarize]
        \\
        \\summarize : a -> Str where [a.id : a -> U64 # numeric handle
        \\    , a.label : a -> Str]
        \\
        \\summarize = |_| "none"
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 11" {
    const source =
        \\module [server]
        \\
        \\server = {
        \\    host: # DNS name
        \\        "localhost",
        \\    port: 8080,
        \\}
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 12" {
    const source =
        \\module [server]
        \\
        \\server = {
        \\    host # DNS name
        \\        : "localhost",
        \\    port: 8080,
        \\}
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 13" {
    const source =
        \\module [main]
        \\
        \\import json # vendored parser
        \\    .Parser
        \\
        \\main = 1
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 14" {
    const source =
        \\module [main]
        \\
        \\import Color exposing # only what we need
        \\    [to_str]
        \\
        \\main = 1
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 15" {
    const source =
        \\module [main]
        \\
        \\import Color as Palette exposing # only what we need
        \\    [to_str]
        \\
        \\main = 1
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 16" {
    const source =
        \\module [main]
        \\
        \\import "data.json" as # decoded at compile time
        \\    config : Str
        \\
        \\main = config
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 17" {
    const source =
        \\module [main]
        \\
        \\import "data.json" as config : # decoded at compile time
        \\    Str
        \\
        \\main = config
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 18" {
    const source =
        \\module [main]
        \\
        \\import # decoded at compile time
        \\    "data.json" as config : Str
        \\
        \\main = config
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 19" {
    const source =
        \\module [main]
        \\
        \\import Color exposing [Shape # all shape helpers
        \\    .*]
        \\
        \\main = 1
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 20" {
    const source =
        \\module [describe]
        \\
        \\describe = |shape| match shape # just two cases
        \\    {
        \\        Circle => "circle"
        \\        _ => "other"
        \\    }
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 21" {
    const source =
        \\module [describe]
        \\
        \\describe = |shape| match # normalized earlier
        \\    shape {
        \\        Circle => "circle"
        \\        _ => "other"
        \\    }
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 22" {
    const source =
        \\module [main]
        \\
        \\import Color exposing [to_str as # shorter name
        \\    str_fn]
        \\
        \\main = 1
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 23" {
    const source =
        \\module [main]
        \\
        \\import Color exposing [to_str # shorter name
        \\    as str_fn]
        \\
        \\main = 1
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 24" {
    const source =
        \\module [describe]
        \\
        \\describe = |shape| match shape {
        \\    Circle(radius) as # keep the whole shape
        \\        circ => circ
        \\    _ => shape
        \\}
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 25" {
    const source =
        \\module [describe]
        \\
        \\describe = |shape| match shape {
        \\    Circle(radius) # keep the whole shape
        \\        as circ => circ
        \\    _ => shape
        \\}
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 26" {
    const source =
        \\module [run]
        \\
        \\run! : List(Str) # exit code
        \\    => I32
        \\
        \\run! = |args| 0
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 27" {
    const source =
        \\module [add]
        \\
        \\add : U64 # returns the same type
        \\    -> U64
        \\
        \\add = |x| x
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 28" {
    const source =
        \\module [render]
        \\
        \\render : List(a) -> Str where [a.label # short text
        \\    : a -> Str]
        \\
        \\render = |items| "none"
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 29" {
    const source =
        \\module [Box]
        \\
        \\Box(a) := List(a) where # comparable elements only
        \\    [a.eq : a, a -> Bool]
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 30" {
    const source =
        \\module [render]
        \\
        \\render : List(a) -> Str where # printable elements only
        \\    [a.label : a -> Str]
        \\
        \\render = |items| "none"
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 31" {
    const source =
        \\platform "img-convert"
        \\    requires { convert! : # host callback
        \\        List(U8) => List(U8) }
        \\    exposes []
        \\    packages {}
        \\    provides {}
        \\    targets: {}
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 32" {
    const source =
        \\platform "img-convert"
        \\    requires {}
        \\    exposes []
        \\    packages {}
        \\    provides {}
        \\    targets: { x64mac : # intel macs
        \\        { inputs: [app] } }
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 33" {
    const source =
        \\platform "img-convert"
        \\    requires {}
        \\    exposes []
        \\    packages {}
        \\    provides {}
        \\    targets: {
        \\        x64mac: { inputs: [app] } # intel macs
        \\        , arm64mac: { inputs: [app] },
        \\    }
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 34" {
    const source =
        \\app [main] { pf: platform # local checkout
        \\    "../platform/main.roc" }
        \\
        \\main = 1
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 35" {
    const source =
        \\package [Greeter] # no external deps
        \\    {}
        \\
        \\greeter = 1
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 36" {
    const source =
        \\module [user_id]
        \\
        \\user_id = |path| match path {
        \\    "user-${ # numeric suffix
        \\        id}" => id
        \\    _ => "unknown"
        \\}
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 37" {
    const source =
        \\module [user_id]
        \\
        \\user_id = |path| match path {
        \\    "user-${id # numeric suffix
        \\        }" => id
        \\    _ => "unknown"
        \\}
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}

test "issue 12040: inter-token comments case 38" {
    const source =
        \\module [Pair]
        \\
        \\Pair # generic pair
        \\    (first, second) := (first, second)
    ;
    const result = try moduleFmtsStable(std.testing.allocator, source, false);
    defer std.testing.allocator.free(result);
    var lines = std.mem.splitScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.findScalar(u8, line, '#')) |start| {
            try std.testing.expectEqual(@as(usize, 1), std.mem.count(u8, result, line[start..]));
        }
    }
}
