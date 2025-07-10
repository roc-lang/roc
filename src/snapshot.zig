const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const base = @import("base.zig");
const canonicalize = @import("check/canonicalize.zig");
const types_mod = @import("types.zig");
const types_problem_mod = @import("check/check_types/problem.zig");
const cache = @import("cache/mod.zig");

const Solver = @import("check/check_types.zig");
const CIR = canonicalize.CIR;
const parse = @import("check/parse.zig");
const fmt = @import("fmt.zig");
const types = @import("types.zig");
const reporting = @import("reporting.zig");
const tokenize = @import("check/parse/tokenize.zig");
const SExprTree = @import("base/SExprTree.zig");

const AST = parse.AST;
const Report = reporting.Report;

/// Categories of tokens for syntax highlighting
const TokenCategory = enum {
    keyword,
    identifier,
    string,
    number,
    operator,
    bracket,
    comment,
    punctuation,
    default,

    pub fn toCssClass(self: TokenCategory) []const u8 {
        return switch (self) {
            .keyword => "token-keyword",
            .identifier => "token-identifier",
            .string => "token-string",
            .number => "token-number",
            .operator => "token-operator",
            .bracket => "token-bracket",
            .comment => "token-comment",
            .punctuation => "token-punctuation",
            .default => "token-default",
        };
    }
};

/// Convert a token type to its category for syntax highlighting
fn tokenToCategory(token: tokenize.Token.Tag) TokenCategory {
    return switch (token) {
        // Keywords
        .KwApp, .KwAs, .KwCrash, .KwDbg, .KwElse, .KwExpect, .KwExposes, .KwExposing, .KwFor, .KwGenerates, .KwHas, .KwHosted, .KwIf, .KwImplements, .KwImport, .KwImports, .KwIn, .KwInterface, .KwMatch, .KwModule, .KwPackage, .KwPackages, .KwPlatform, .KwProvides, .KwRequires, .KwReturn, .KwVar, .KwWhere, .KwWith => .keyword,

        // Identifiers
        .UpperIdent, .LowerIdent, .DotLowerIdent, .DotUpperIdent, .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent, .NamedUnderscore, .OpaqueName => .identifier,

        // Strings
        .StringStart, .StringEnd, .StringPart, .MultilineStringStart, .MultilineStringEnd, .SingleQuote => .string,

        // Numbers
        .Float, .Int, .DotInt, .NoSpaceDotInt, .MalformedNumberBadSuffix, .MalformedNumberUnicodeSuffix, .MalformedNumberNoDigits, .MalformedNumberNoExponentDigits => .number,

        // Operators
        .OpPlus, .OpStar, .OpBinaryMinus, .OpUnaryMinus, .OpEquals, .OpNotEquals, .OpAnd, .OpOr, .OpGreaterThan, .OpLessThan, .OpGreaterThanOrEq, .OpLessThanOrEq, .OpAssign, .OpColonEqual, .OpArrow, .OpBackslash, .OpBar, .OpBang, .OpQuestion, .OpColon, .OpPercent, .OpDoubleSlash, .OpCaret, .OpAmpersand, .OpPizza, .OpSlash, .OpDoubleQuestion, .OpBackArrow, .OpFatArrow, .NoSpaceOpQuestion => .operator,

        // Brackets
        .OpenRound, .CloseRound, .OpenSquare, .CloseSquare, .OpenCurly, .CloseCurly => .bracket,

        // Punctuation
        .Comma, .Dot, .DoubleDot, .TripleDot, .Underscore => .punctuation,

        // Everything else
        else => .default,
    };
}

var verbose_log: bool = false;
var prng = std.Random.DefaultPrng.init(1234567890);

const rand = prng.random();

/// Logs a message if verbose logging is enabled.
fn log(comptime fmt_str: []const u8, args: anytype) void {
    if (verbose_log) {
        std.log.info(fmt_str, args);
    }
}

/// Always logs a warning message.
fn warn(comptime fmt_str: []const u8, args: anytype) void {
    std.log.warn(fmt_str, args);
}

/// cli entrypoint for snapshot tool
pub fn main() !void {
    // Use c_allocator for argument parsing
    const gpa = std.heap.c_allocator;

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    var snapshot_paths = std.ArrayList([]const u8).init(gpa);
    defer snapshot_paths.deinit();

    var maybe_fuzz_corpus_path: ?[]const u8 = null;
    var expect_fuzz_corpus_path: bool = false;
    var generate_html: bool = false;
    var debug_mode: bool = false;

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--verbose")) {
            verbose_log = true;
        } else if (std.mem.eql(u8, arg, "--html")) {
            generate_html = true;
        } else if (std.mem.eql(u8, arg, "--debug")) {
            debug_mode = true;
        } else if (std.mem.eql(u8, arg, "--fuzz-corpus")) {
            if (maybe_fuzz_corpus_path != null) {
                std.log.err("`--fuzz-corpus` should only be specified once.", .{});
                std.process.exit(1);
            }
            expect_fuzz_corpus_path = true;
        } else if (expect_fuzz_corpus_path) {
            maybe_fuzz_corpus_path = arg;
            expect_fuzz_corpus_path = false;
        } else if (std.mem.eql(u8, arg, "--help")) {
            const usage =
                \\Usage: roc snapshot [options] [snapshot_paths...]
                \\
                \\Options:
                \\  --verbose       Enable verbose logging
                \\  --html          Generate HTML output files
                \\  --debug         Use GeneralPurposeAllocator for debugging (default: c_allocator)
                \\  --fuzz-corpus <path>  Specify the path to the fuzz corpus
                \\
                \\Arguments:
                \\  snapshot_paths  Paths to snapshot files or directories
            ;
            std.log.info(usage, .{});
            std.process.exit(0);
        } else {
            try snapshot_paths.append(arg);
        }
    }

    if (expect_fuzz_corpus_path) {
        std.log.err("Expected fuzz corpus path, but none was provided", .{});
        std.process.exit(1);
    }

    // Choose allocator for snapshot processing based on debug mode
    var gpa_impl: ?std.heap.GeneralPurposeAllocator(.{}) = null;
    defer if (gpa_impl) |*impl| {
        _ = impl.deinit();
    };

    const snapshot_allocator = if (debug_mode) blk: {
        gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
        break :blk gpa_impl.?.allocator();
    } else std.heap.c_allocator;

    if (maybe_fuzz_corpus_path != null) {
        log("copying SOURCE from snapshots to: {s}", .{maybe_fuzz_corpus_path.?});
        try std.fs.cwd().makePath(maybe_fuzz_corpus_path.?);
    }

    const snapshots_dir = "src/snapshots";
    var total_success: usize = 0;
    var total_failed: usize = 0;
    var timer = std.time.Timer.start() catch unreachable;

    if (snapshot_paths.items.len > 0) {
        for (snapshot_paths.items) |path| {
            const result = try processPath(snapshot_allocator, path, maybe_fuzz_corpus_path, generate_html);
            total_success += result.success;
            total_failed += result.failed;
        }
    } else {
        // process all files in snapshots_dir
        const result = try processPath(snapshot_allocator, snapshots_dir, maybe_fuzz_corpus_path, generate_html);
        total_success = result.success;
        total_failed = result.failed;
    }

    const duration_ms = timer.read() / std.time.ns_per_ms;

    std.log.info("processed {d} snapshots in {d} ms.", .{ total_success, duration_ms });
}

/// Check if a file has a valid snapshot extension
fn isSnapshotFile(path: []const u8) bool {
    return std.mem.endsWith(u8, path, ".md");
}

fn isMultiFileSnapshot(path: []const u8) bool {
    return std.mem.endsWith(u8, path, "_package") or
        std.mem.endsWith(u8, path, "_platform") or
        std.mem.endsWith(u8, path, "_app");
}

fn getMultiFileSnapshotType(path: []const u8) NodeType {
    if (std.mem.endsWith(u8, path, "_package")) return .package;
    if (std.mem.endsWith(u8, path, "_platform")) return .platform;
    if (std.mem.endsWith(u8, path, "_app")) return .app;
    return .file; // fallback, shouldn't happen if isMultiFileSnapshot was checked first
}

fn processMultiFileSnapshot(allocator: Allocator, dir_path: []const u8, generate_html: bool) !void {
    log("Processing multi-file snapshot directory: {s}", .{dir_path});

    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        warn("Failed to open directory {s}: {}", .{ dir_path, err });
        return;
    };
    defer dir.close();

    // First, collect EXPECTED sections from existing .md files
    var expected_sections = std.StringHashMap([]const u8).init(allocator);
    defer {
        var iter = expected_sections.iterator();
        while (iter.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            allocator.free(entry.value_ptr.*);
        }
        expected_sections.deinit();
    }

    var iterator = dir.iterate();
    while (try iterator.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".md")) {
            const full_path = try std.fs.path.join(allocator, &.{ dir_path, entry.name });
            defer allocator.free(full_path);

            if (std.fs.cwd().readFileAlloc(allocator, full_path, 1024 * 1024)) |content| {
                defer allocator.free(content);

                // Extract EXPECTED section
                const expected_header = "# EXPECTED\n";
                if (std.mem.indexOf(u8, content, expected_header)) |start_idx| {
                    const content_start = start_idx + expected_header.len;

                    // Find the next section header
                    var end_idx = content.len;
                    var search_idx = content_start;
                    while (search_idx < content.len - 2) {
                        if (content[search_idx] == '\n' and
                            content[search_idx + 1] == '#' and
                            content[search_idx + 2] == ' ')
                        {
                            end_idx = search_idx + 1;
                            break;
                        }
                        search_idx += 1;
                    }

                    const expected_section = std.mem.trim(u8, content[content_start..end_idx], " \t\r\n");
                    try expected_sections.put(try allocator.dupe(u8, entry.name), try allocator.dupe(u8, expected_section));
                }
            } else |_| {}
        }
    }

    // Delete existing .md files
    iterator = dir.iterate();
    var files_to_delete = std.ArrayList([]u8).init(allocator);
    defer {
        for (files_to_delete.items) |file_path| {
            allocator.free(file_path);
        }
        files_to_delete.deinit();
    }

    while (try iterator.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".md")) {
            const file_path = try allocator.dupe(u8, entry.name);
            try files_to_delete.append(file_path);
        }
    }

    for (files_to_delete.items) |file_name| {
        dir.deleteFile(file_name) catch |err| {
            warn("Failed to delete {s}: {}", .{ file_name, err });
        };
    }

    // Find all .roc files and generate snapshots for each
    iterator = dir.iterate();
    const snapshot_type = getMultiFileSnapshotType(dir_path);

    while (try iterator.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".roc")) {
            const roc_file_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, entry.name });
            defer allocator.free(roc_file_path);

            // Generate snapshot file name (replace .roc with .md)
            const base_name = entry.name[0 .. entry.name.len - 4]; // remove .roc
            const snapshot_file_name = try std.fmt.allocPrint(allocator, "{s}.md", .{base_name});
            defer allocator.free(snapshot_file_name);
            const snapshot_file_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ dir_path, snapshot_file_name });
            defer allocator.free(snapshot_file_path);

            // Read the .roc file content
            const roc_content = std.fs.cwd().readFileAlloc(allocator, roc_file_path, 1024 * 1024) catch |err| {
                warn("Failed to read {s}: {}", .{ roc_file_path, err });
                continue;
            };
            defer allocator.free(roc_content);

            // Create meta section
            const type_name = switch (snapshot_type) {
                .package => "package",
                .platform => "platform",
                .app => "app",
                else => "file",
            };
            const meta = Meta{
                .description = try std.fmt.allocPrint(allocator, "{s} module from {s}", .{ base_name, type_name }),
                .node_type = snapshot_type,
            };
            defer allocator.free(meta.description);

            // Get preserved EXPECTED section if it exists
            const expected_content = expected_sections.get(snapshot_file_name);

            // Process the .roc file as a snapshot
            try processRocFileAsSnapshotWithExpected(allocator, snapshot_file_path, roc_content, meta, expected_content, generate_html);
        }
    }
}

fn processRocFileAsSnapshot(allocator: Allocator, output_path: []const u8, roc_content: []const u8, meta: Meta, generate_html: bool) !void {
    // Try to read existing EXPECTED section if the file exists
    var expected_content: ?[]const u8 = null;
    defer if (expected_content) |content| allocator.free(content);

    if (std.fs.cwd().readFileAlloc(allocator, output_path, 1024 * 1024)) |existing_content| {
        defer allocator.free(existing_content);

        // Extract EXPECTED section manually since extractSections is defined later
        const expected_header = "# EXPECTED\n";
        if (std.mem.indexOf(u8, existing_content, expected_header)) |start_idx| {
            const content_start = start_idx + expected_header.len;

            // Find the next section header
            var end_idx = existing_content.len;
            var search_idx = content_start;
            while (search_idx < existing_content.len - 2) {
                if (existing_content[search_idx] == '\n' and
                    existing_content[search_idx + 1] == '#' and
                    existing_content[search_idx + 2] == ' ')
                {
                    end_idx = search_idx + 1;
                    break;
                }
                search_idx += 1;
            }

            const expected_section = std.mem.trim(u8, existing_content[content_start..end_idx], " \t\r\n");
            expected_content = try allocator.dupe(u8, expected_section);
        }
    } else |_| {
        // File doesn't exist yet, that's fine
    }

    try processRocFileAsSnapshotWithExpected(allocator, output_path, roc_content, meta, expected_content, generate_html);
}

fn processRocFileAsSnapshotWithExpected(allocator: Allocator, output_path: []const u8, roc_content: []const u8, meta: Meta, expected_content: ?[]const u8, generate_html: bool) !void {
    log("Generating snapshot for: {s}", .{output_path});

    // Process the content through the compilation pipeline
    var module_env = base.ModuleEnv.init(allocator);
    defer module_env.deinit();

    // Parse the content
    var ast = parse.parse(&module_env, roc_content);
    defer ast.deinit(allocator);

    // Try canonicalization
    ast.store.emptyScratch();

    // Extract module name from output path
    const basename = std.fs.path.basename(output_path);
    const module_name = if (std.mem.lastIndexOfScalar(u8, basename, '.')) |dot_idx|
        basename[0..dot_idx]
    else
        basename;
    var can_ir = CIR.init(&module_env, module_name);
    defer can_ir.deinit();

    var can = canonicalize.init(&can_ir, &ast, null) catch |err| {
        warn("Canonicalization init failed: {}", .{err});
        return;
    };
    defer can.deinit();

    const maybe_expr_idx: ?CIR.Expr.Idx = null;

    can.canonicalizeFile() catch |err| {
        warn("Canonicalization failed: {}", .{err});
        return;
    };

    // Types (ONCE)
    const empty_modules: []const *CIR = &.{};
    var solver = Solver.init(allocator, &can_ir.env.types, &can_ir, empty_modules) catch |err| {
        warn("Type solver init failed: {}", .{err});
        return;
    };
    defer solver.deinit();

    try solver.checkDefs();

    // Create content structure
    const content = Content{
        .meta = meta,
        .source = roc_content,
        .expected = expected_content,
        .formatted = null,
        .has_canonicalize = true,
    };

    // Buffer all output in memory before writing files
    var md_buffer = std.ArrayList(u8).init(allocator);
    defer md_buffer.deinit();

    var html_buffer = if (generate_html) std.ArrayList(u8).init(allocator) else null;
    defer if (html_buffer) |*buf| buf.deinit();

    var output = DualOutput.init(allocator, &md_buffer, if (html_buffer) |*buf| buf else null);

    // Generate HTML wrapper
    try generateHtmlWrapper(&output, &content);

    // Generate all sections
    try generateMetaSection(&output, &content);
    try generateSourceSection(&output, &content);
    try generateExpectedSection(&output, &content);
    try generateProblemsSection(&output, &ast, &can_ir, &solver, &content, output_path, &module_env);
    try generateTokensSection(&output, &ast, &content, &module_env);
    try generateParseSection(&output, &content, &ast, &module_env);
    try generateFormattedSection(&output, &content, &ast);
    try generateCanonicalizeSection(&output, &content, &can_ir, maybe_expr_idx);
    try generateTypesSection(&output, &content, &can_ir, maybe_expr_idx);

    try generateHtmlClosing(&output);

    // Write the markdown file
    const md_file = std.fs.cwd().createFile(output_path, .{}) catch |err| {
        warn("Failed to create {s}: {}", .{ output_path, err });
        return;
    };
    defer md_file.close();

    try md_file.writeAll(md_buffer.items);

    // Write HTML file
    if (html_buffer) |*buf| {
        const html_path = try std.fmt.allocPrint(allocator, "{s}.html", .{output_path[0 .. output_path.len - 3]});
        defer allocator.free(html_path);

        const html_file = std.fs.cwd().createFile(html_path, .{}) catch |err| {
            warn("Failed to create {s}: {}", .{ html_path, err });
            return;
        };
        defer html_file.close();

        try html_file.writeAll(buf.items);
    }
}

const ProcessResult = struct {
    success: usize,
    failed: usize,
};

fn processPath(gpa: Allocator, path: []const u8, maybe_fuzz_corpus_path: ?[]const u8, generate_html: bool) !ProcessResult {
    var processed_count: usize = 0;
    var failed_count: usize = 0;

    const canonical_path = std.fs.cwd().realpathAlloc(gpa, path) catch |err| {
        std.log.err("failed to resolve path '{s}': {s}", .{ path, @errorName(err) });
        return .{ .success = 0, .failed = 1 };
    };
    defer gpa.free(canonical_path);

    // Try to open as directory first
    if (std.fs.cwd().openDir(canonical_path, .{ .iterate = true })) |dir_handle| {
        var dir = dir_handle;
        defer dir.close();

        // It's a directory
        if (isMultiFileSnapshot(canonical_path)) {
            try processMultiFileSnapshot(gpa, canonical_path, generate_html);
            processed_count += 1;
            return .{ .success = processed_count, .failed = failed_count };
        } else {
            var dir_iterator = dir.iterate();
            while (try dir_iterator.next()) |entry| {
                // Skip hidden files and special directories
                if (entry.name[0] == '.') continue;

                const full_path = try std.fs.path.join(gpa, &[_][]const u8{ canonical_path, entry.name });
                defer gpa.free(full_path);

                if (entry.kind == .directory) {
                    const result = try processPath(gpa, full_path, maybe_fuzz_corpus_path, generate_html);
                    processed_count += result.success;
                    failed_count += result.failed;
                } else if (entry.kind == .file and isSnapshotFile(entry.name)) {
                    if (try processSnapshotFile(gpa, full_path, maybe_fuzz_corpus_path, generate_html)) {
                        processed_count += 1;
                    } else {
                        log("skipped file (not a valid snapshot): {s}", .{full_path});
                        failed_count += 1;
                    }
                }
            }
        }
    } else |dir_err| {
        // Not a directory, try as file
        if (dir_err == error.NotDir) {
            if (isSnapshotFile(canonical_path)) {
                if (try processSnapshotFile(gpa, canonical_path, maybe_fuzz_corpus_path, generate_html)) {
                    processed_count += 1;
                } else {
                    std.log.err("failed to process snapshot file: {s}", .{canonical_path});
                    std.log.err("make sure the file starts with '~~~META' and has valid snapshot format", .{});
                    failed_count += 1;
                }
            } else {
                std.log.err("file '{s}' is not a snapshot file (must end with .md)", .{canonical_path});
            }
        } else {
            std.log.err("failed to access path '{s}': {s}", .{ canonical_path, @errorName(dir_err) });
            return .{ .success = 0, .failed = 1 };
        }
    }

    return .{ .success = processed_count, .failed = failed_count };
}

/// Represents the different sections of a snapshot file.
const Section = union(enum) {
    meta,
    source,
    expected,
    formatted,
    parse,
    canonicalize,
    tokens,
    problems,
    types,

    pub const META = "# META\n~~~ini\n";
    pub const SOURCE = "# SOURCE\n~~~roc\n";
    pub const EXPECTED = "# EXPECTED\n";
    pub const FORMATTED = "# FORMATTED\n~~~roc\n";
    pub const PARSE = "# PARSE\n~~~clojure\n";
    pub const CANONICALIZE = "# CANONICALIZE\n~~~clojure\n";
    pub const TOKENS = "# TOKENS\n~~~zig\n";
    pub const PROBLEMS = "# PROBLEMS\n";
    pub const TYPES = "# TYPES\n~~~clojure\n";

    pub const SECTION_END = "~~~\n";

    fn fromString(str: []const u8) ?Section {
        if (std.mem.startsWith(u8, str, META)) return .meta;
        if (std.mem.startsWith(u8, str, SOURCE)) return .source;
        if (std.mem.startsWith(u8, str, EXPECTED)) return .expected;
        if (std.mem.startsWith(u8, str, FORMATTED)) return .formatted;
        if (std.mem.startsWith(u8, str, PARSE)) return .parse;
        if (std.mem.startsWith(u8, str, CANONICALIZE)) return .canonicalize;
        if (std.mem.startsWith(u8, str, TYPES)) return .types;
        if (std.mem.startsWith(u8, str, TOKENS)) return .tokens;
        if (std.mem.startsWith(u8, str, PROBLEMS)) return .problems;
        return null;
    }

    fn asString(self: Section) []const u8 {
        return switch (self) {
            .meta => META,
            .source => SOURCE,
            .expected => EXPECTED,
            .formatted => FORMATTED,
            .parse => PARSE,
            .canonicalize => CANONICALIZE,
            .tokens => TOKENS,
            .problems => PROBLEMS,
            .types => TYPES,
        };
    }

    /// Captures the start and end positions of a section within the file content
    const Range = struct {
        start: usize,
        end: usize,

        fn empty() Range {
            return .{
                .start = 0,
                .end = 0,
            };
        }

        fn extract(self: Range, content: []const u8) []const u8 {
            if (self.end < self.start) @panic("invalid range");
            return std.mem.trimRight(u8, content[self.start..self.end], "\n");
        }
    };
};

/// The type of node to parse
pub const NodeType = enum {
    file,
    header,
    expr,
    statement,
    package,
    platform,
    app,

    pub const HEADER = "header";
    pub const EXPR = "expr";
    pub const STMT = "statement";
    pub const FILE = "file";
    pub const PACKAGE = "package";
    pub const PLATFORM = "platform";
    pub const APP = "app";

    fn fromString(str: []const u8) !NodeType {
        if (std.mem.eql(u8, str, HEADER)) return .header;
        if (std.mem.eql(u8, str, EXPR)) return .expr;
        if (std.mem.eql(u8, str, STMT)) return .statement;
        if (std.mem.eql(u8, str, FILE)) return .file;
        if (std.mem.eql(u8, str, PACKAGE)) return .package;
        if (std.mem.eql(u8, str, PLATFORM)) return .platform;
        if (std.mem.eql(u8, str, APP)) return .app;
        return Error.InvalidNodeType;
    }

    fn toString(self: NodeType) []const u8 {
        return switch (self) {
            .file => "file",
            .header => "header",
            .expr => "expr",
            .statement => "statement",
            .package => "package",
            .platform => "platform",
            .app => "app",
        };
    }
};

const Meta = struct {
    description: []const u8,
    node_type: NodeType,

    const DESC_START: []const u8 = "description=";
    const TYPE_START: []const u8 = "type=";

    fn fromString(text: []const u8) Error!Meta {
        var lines = std.mem.splitScalar(u8, text, '\n');
        var desc: []const u8 = "";
        var node_type: NodeType = .file;
        while (true) {
            var line = lines.next() orelse break;
            if (std.mem.startsWith(u8, line, DESC_START)) {
                desc = line[(DESC_START.len)..];
            } else if (std.mem.startsWith(u8, line, TYPE_START)) {
                const ty = line[(TYPE_START.len)..];
                node_type = try NodeType.fromString(ty);
            }
        }

        return .{
            .description = desc,
            .node_type = node_type,
        };
    }

    fn format(self: Meta, writer: anytype) !void {
        try writer.writeAll(DESC_START);
        try writer.writeAll(self.description);
        try writer.writeAll("\n");
        try writer.writeAll(TYPE_START);
        try writer.writeAll(self.node_type.toString());
    }

    test "Meta.fromString - only description" {
        const meta = try Meta.fromString(
            \\description=Hello world
        );
        try std.testing.expectEqualStrings(meta.description, "Hello world");
    }
    test "Meta.fromString - desc and file type" {
        const meta = try Meta.fromString(
            \\description=Hello world
            \\type=file
        );
        try std.testing.expectEqualStrings(meta.description, "Hello world");
        try std.testing.expectEqual(meta.node_type, .file);
    }
    test "Meta.fromString - desc and expr type" {
        const meta = try Meta.fromString(
            \\description=Hello world
            \\type=expr
        );
        try std.testing.expectEqualStrings(meta.description, "Hello world");
        try std.testing.expectEqual(meta.node_type, .expr);
    }
    test "Meta.fromString - desc and statement type" {
        const meta = try Meta.fromString(
            \\description=Hello world
            \\type=statement
        );
        try std.testing.expectEqualStrings(meta.description, "Hello world");
        try std.testing.expectEqual(meta.node_type, .statement);
    }
    test "Meta.fromString - desc and header type" {
        const meta = try Meta.fromString(
            \\description=Hello world
            \\type=header
        );
        try std.testing.expectEqualStrings(meta.description, "Hello world");
        try std.testing.expectEqual(meta.node_type, .header);
    }
    test "Meta.fromString - desc and invalid type" {
        const meta = Meta.fromString(
            \\description=Hello world
            \\type=foobar
        );
        try std.testing.expectEqual(meta, Error.InvalidNodeType);
    }
};

/// Content of a snapshot file, references the Metadata and Source sections etc
const Content = struct {
    meta: Meta,
    source: []const u8,
    expected: ?[]const u8,
    formatted: ?[]const u8,
    has_canonicalize: bool,

    fn init(meta: Meta, source: []const u8, expected: ?[]const u8, formatted: ?[]const u8, has_canonicalize: bool) Content {
        return .{
            .meta = meta,
            .source = source,
            .expected = expected,
            .formatted = formatted,
            .has_canonicalize = has_canonicalize,
        };
    }

    fn from_ranges(ranges: std.AutoHashMap(Section, Section.Range), content: []const u8) Error!Content {
        var source: []const u8 = undefined;
        var expected: ?[]const u8 = undefined;
        var formatted: ?[]const u8 = undefined;
        var has_canonicalize: bool = false;

        if (ranges.get(.source)) |value| {
            source = value.extract(content);
        } else {
            return Error.MissingSnapshotSource;
        }

        if (ranges.get(.expected)) |value| {
            expected = value.extract(content);
        } else {
            expected = null;
        }

        if (ranges.get(.formatted)) |value| {
            formatted = value.extract(content);
        } else {
            formatted = null;
        }

        if (ranges.get(.canonicalize)) |_| {
            has_canonicalize = true;
        }

        if (ranges.get(.meta)) |value| {
            const meta_txt = value.extract(content);
            const meta = try Meta.fromString(meta_txt);
            return Content.init(
                meta,
                source,
                expected,
                formatted,
                has_canonicalize,
            );
        } else {
            return Error.MissingSnapshotHeader;
        }
    }
};

const Error = error{ MissingSnapshotHeader, MissingSnapshotSource, InvalidNodeType, BadSectionHeader };

/// Dual output writers for markdown and HTML generation
const DualOutput = struct {
    md_writer: std.ArrayList(u8).Writer,
    html_writer: ?std.ArrayList(u8).Writer,
    gpa: Allocator,

    fn init(gpa: Allocator, md_buffer: *std.ArrayList(u8), html_buffer: ?*std.ArrayList(u8)) DualOutput {
        return .{
            .md_writer = md_buffer.writer(),
            .html_writer = if (html_buffer) |buf| buf.writer() else null,
            .gpa = gpa,
        };
    }

    fn begin_section(self: *DualOutput, name: []const u8) !void {
        try self.md_writer.print("# {s}\n", .{name});
        if (self.html_writer) |writer| {
            try writer.print(
                \\        <div class="section" data-section="{s}">
                \\            <div class="section-content">
            , .{name});
        }
    }

    fn end_section(self: *DualOutput) !void {
        if (self.html_writer) |writer| {
            try writer.writeAll(
                \\            </div>
                \\        </div>
            );
        }
    }

    fn begin_code_block(self: *DualOutput, language: []const u8) !void {
        try self.md_writer.print("~~~{s}\n", .{language});
    }

    fn end_code_block(self: *DualOutput) !void {
        try self.md_writer.writeAll("~~~\n");
    }
};

/// Helper function to escape HTML characters
fn escapeHtmlChar(writer: anytype, char: u8) !void {
    switch (char) {
        '<' => try writer.writeAll("&lt;"),
        '>' => try writer.writeAll("&gt;"),
        '&' => try writer.writeAll("&amp;"),
        '"' => try writer.writeAll("&quot;"),
        '\'' => try writer.writeAll("&#x27;"),
        else => try writer.writeByte(char),
    }
}

/// Generate META section for both markdown and HTML
fn generateMetaSection(output: *DualOutput, content: *const Content) !void {
    try output.begin_section("META");
    try output.begin_code_block("ini");
    try content.meta.format(output.md_writer);
    try output.md_writer.writeAll("\n");

    // HTML META section
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <div class="meta-info">
            \\                    <p><strong>Description:</strong>
        );
        try writer.writeAll(content.meta.description);
        try writer.writeAll("</p>\n                    <p><strong>Type:</strong> ");
        try writer.writeAll(content.meta.node_type.toString());
        try writer.writeAll(
            \\</p>
            \\                </div>
            \\
        );
    }

    try output.end_code_block();
    try output.end_section();
}

/// Generate SOURCE section for both markdown and HTML
fn generateSourceSection(output: *DualOutput, content: *const Content) !void {
    try output.begin_section("SOURCE");
    try output.begin_code_block("roc");
    try output.md_writer.writeAll(content.source);
    if (content.source.len == 0 or content.source[content.source.len - 1] != '\n') {
        try output.md_writer.writeAll("\n");
    }

    // HTML SOURCE section - encode source as JavaScript string
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <div class="source-code" id="source-display">
            \\                </div>
            \\                <script>
            \\                window.rocSourceCode =
        );

        // Escape the source code for JavaScript string literal
        try writer.writeAll("`");
        for (content.source) |char| {
            switch (char) {
                '`' => try writer.writeAll("\\`"),
                '\\' => try writer.writeAll("\\\\"),
                '$' => try writer.writeAll("\\$"),
                '\n' => try writer.writeAll("\\n"),
                '\r' => try writer.writeAll("\\r"),
                '\t' => try writer.writeAll("\\t"),
                else => try writer.writeByte(char),
            }
        }
        try writer.writeAll(
            \\`;
            \\      </script>
            \\
        );
    }

    try output.end_code_block();
    try output.end_section();
}

/// Generate EXPECTED section for both markdown and HTML
fn generateExpectedSection(output: *DualOutput, content: *const Content) !void {
    try output.begin_section("EXPECTED");

    if (content.expected) |expected| {
        try output.md_writer.writeAll(expected);
        try output.md_writer.writeByte('\n');

        // HTML EXPECTED section
        if (output.html_writer) |writer| {
            try writer.writeAll(
                \\                <div class="expected">
            );

            // For HTML, escape the expected content
            for (expected) |char| {
                switch (char) {
                    '<' => try writer.writeAll("&lt;"),
                    '>' => try writer.writeAll("&gt;"),
                    '&' => try writer.writeAll("&amp;"),
                    '"' => try writer.writeAll("&quot;"),
                    '\'' => try writer.writeAll("&#39;"),
                    else => try writer.writeByte(char),
                }
            }

            try writer.writeAll(
                \\
                \\                </div>
                \\
            );
        }
    } else {
        try output.md_writer.writeAll("NIL\n");

        if (output.html_writer) |writer| {
            try writer.writeAll(
                \\                <div class="expected">
                \\                    <p>NIL</p>
                \\                </div>
                \\
            );
        }
    }

    try output.end_section();
}

/// Generate PROBLEMS section for both markdown and HTML
fn generateProblemsSection(output: *DualOutput, parse_ast: *AST, can_ir: *CIR, solver: *Solver, content: *const Content, snapshot_path: []const u8, module_env: *base.ModuleEnv) !void {
    try output.begin_section("PROBLEMS");

    // HTML PROBLEMS section
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <div class="problems">
        );
    }

    var tokenize_problems: usize = 0;
    var parser_problems: usize = 0;
    var canonicalize_problems: usize = 0;
    var check_types_problem: usize = 0;

    // Tokenize Diagnostics
    for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
        tokenize_problems += 1;
        var report: reporting.Report = parse_ast.tokenizeDiagnosticToReport(diagnostic, output.gpa) catch |err| {
            try output.md_writer.print("Error creating tokenize report: {}\n", .{err});
            if (output.html_writer) |writer| {
                try writer.print("                    <p>Error creating tokenize report: {}</p>\n", .{err});
            }
            continue;
        };
        defer report.deinit();

        report.render(output.md_writer.any(), .markdown) catch |err| {
            try output.md_writer.print("Error rendering report: {}\n", .{err});
        };

        if (output.html_writer) |writer| {
            try writer.writeAll("                    <div class=\"problem\">");
            report.render(writer.any(), .markdown) catch |err| {
                try writer.print("Error rendering report: {}", .{err});
            };
            try writer.writeAll("</div>\n");
        }
    }

    // Parser Diagnostics
    for (parse_ast.parse_diagnostics.items) |diagnostic| {
        parser_problems += 1;
        var report: reporting.Report = parse_ast.parseDiagnosticToReport(diagnostic, output.gpa, snapshot_path) catch |err| {
            try output.md_writer.print("Error creating parse report: {}\n", .{err});
            if (output.html_writer) |writer| {
                try writer.print("                    <p>Error creating parse report: {}</p>\n", .{err});
            }
            continue;
        };
        defer report.deinit();

        report.render(output.md_writer.any(), .markdown) catch |err| {
            try output.md_writer.print("Error rendering report: {}\n", .{err});
        };

        if (output.html_writer) |writer| {
            try writer.writeAll("                    <div class=\"problem\">");
            report.render(writer.any(), .markdown) catch |err| {
                try writer.print("Error rendering report: {}", .{err});
            };
            try writer.writeAll("</div>\n");
        }
    }

    // Canonicalization Diagnostics
    const diagnostics = can_ir.getDiagnostics();
    defer output.gpa.free(diagnostics);
    for (diagnostics) |diagnostic| {
        canonicalize_problems += 1;
        var report: reporting.Report = can_ir.diagnosticToReport(diagnostic, output.gpa, content.source, snapshot_path) catch |err| {
            try output.md_writer.print("Error creating canonicalization report: {}\n", .{err});
            if (output.html_writer) |writer| {
                try writer.print("                    <p>Error creating canonicalization report: {}</p>\n", .{err});
            }
            continue;
        };
        defer report.deinit();

        report.render(output.md_writer.any(), .markdown) catch |err| {
            try output.md_writer.print("Error rendering report: {}\n", .{err});
        };

        if (output.html_writer) |writer| {
            try writer.writeAll("                    <div class=\"problem\">");
            report.render(writer.any(), .markdown) catch |err| {
                try writer.print("Error rendering report: {}", .{err});
            };
            try writer.writeAll("</div>\n");
        }
    }

    // Check Types Problems
    var problem_buf = std.ArrayList(u8).init(output.gpa);
    defer problem_buf.deinit();

    var problems_itr = solver.problems.problems.iterIndices();
    while (problems_itr.next()) |problem_idx| {
        check_types_problem += 1;
        const problem = solver.problems.problems.get(problem_idx);
        var report_builder = types_problem_mod.ReportBuilder.init(
            output.gpa,
            module_env,
            can_ir,
            &solver.snapshots,
            content.source,
            snapshot_path,
        );
        defer report_builder.deinit();

        var report: reporting.Report = report_builder.build(problem) catch |err| {
            try output.md_writer.print("Error creating type checking report: {}\n", .{err});
            if (output.html_writer) |writer| {
                try writer.print("                    <p>Error creating type checking report: {}</p>\n", .{err});
            }
            continue;
        };
        defer report.deinit();

        report.render(output.md_writer.any(), .markdown) catch |err| {
            try output.md_writer.print("Error rendering report: {}\n", .{err});
        };

        if (output.html_writer) |writer| {
            try writer.writeAll("                    <div class=\"problem\">");
            report.render(writer.any(), .markdown) catch |err| {
                try writer.print("Error rendering report: {}", .{err});
            };
            try writer.writeAll("</div>\n");
        }
    }

    const nil_problems = tokenize_problems == 0 and parser_problems == 0 and canonicalize_problems == 0 and check_types_problem == 0;

    if (nil_problems) {
        try output.md_writer.writeAll("NIL\n");
        if (output.html_writer) |writer| {
            try writer.writeAll("                    <p>NIL</p>\n");
        }
        log("reported NIL problems", .{});
    } else {
        log("reported {} token problems", .{tokenize_problems});
        log("reported {} parser problems", .{parser_problems});
        log("reported {} canonicalization problems", .{canonicalize_problems});
        log("reported {} type problems", .{check_types_problem});
    }

    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                </div>
            \\
        );
    }

    try output.end_section();
}

/// Generate TOKENS section for both markdown and HTML
fn generateTokensSection(output: *DualOutput, parse_ast: *AST, content: *const Content, module_env: *base.ModuleEnv) !void {
    try output.begin_section("TOKENS");
    try output.begin_code_block("zig");

    // HTML TOKENS section - encode tokens as JavaScript array
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <div class="token-list" id="tokens-display">
            \\                </div>
            \\                <script>
            \\                window.rocTokens = [
        );
    }

    var tokenizedBuffer = parse_ast.tokens;
    const tokens = tokenizedBuffer.tokens.items(.tag);
    for (tokens, 0..) |tok, i| {
        const region = tokenizedBuffer.resolve(@intCast(i));
        const info = try module_env.calcRegionInfo(content.source, region.start.offset, region.end.offset);
        // const category = tokenToCategory(tok);

        // Markdown token output
        try output.md_writer.print("{s}({d}:{d}-{d}:{d}),", .{
            @tagName(tok),
            // add one to display numbers instead of index
            info.start_line_idx + 1,
            info.start_col_idx + 1,
            info.end_line_idx + 1,
            info.end_col_idx + 1,
        });

        // HTML token output as JavaScript array element: [token_kind_str, start_byte, end_byte]
        if (output.html_writer) |writer| {
            try writer.print("                    [\"{s}\", {d}, {d}]", .{
                @tagName(tok),
                region.start.offset,
                region.end.offset,
            });

            // Add comma except for last token
            if (i < tokens.len - 1) {
                try writer.writeAll(",");
            }
        }

        if (tok == .Newline) {
            try output.md_writer.writeAll("\n");
            if (output.html_writer) |writer| {
                try writer.writeAll("\n");
            }
        } else {
            if (output.html_writer) |writer| {
                try writer.writeAll(" ");
            }
        }
    }

    try output.md_writer.writeAll("\n");

    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                ];
            \\                </script>
            \\
        );
    }
    try output.end_code_block();
    try output.end_section();
}

/// Generate PARSE2 section using SExprTree for both markdown and HTML
fn generateParseSection(output: *DualOutput, content: *const Content, parse_ast: *AST, env: *base.ModuleEnv) !void {
    var tree = SExprTree.init(output.gpa);
    defer tree.deinit();

    // Generate SExprTree node based on content type
    switch (content.meta.node_type) {
        .file => {
            const file = parse_ast.store.getFile();
            file.pushToSExprTree(env, parse_ast, &tree);
        },
        .header => {
            const header = parse_ast.store.getHeader(@enumFromInt(parse_ast.root_node_idx));
            header.pushToSExprTree(env, parse_ast, &tree);
        },
        .expr => {
            const expr = parse_ast.store.getExpr(@enumFromInt(parse_ast.root_node_idx));
            expr.pushToSExprTree(env, parse_ast, &tree);
        },
        .statement => {
            const stmt = parse_ast.store.getStatement(@enumFromInt(parse_ast.root_node_idx));
            stmt.pushToSExprTree(env, parse_ast, &tree);
        },
        .package => {
            const file = parse_ast.store.getFile();
            file.pushToSExprTree(env, parse_ast, &tree);
        },
        .platform => {
            const file = parse_ast.store.getFile();
            file.pushToSExprTree(env, parse_ast, &tree);
        },
        .app => {
            const file = parse_ast.store.getFile();
            file.pushToSExprTree(env, parse_ast, &tree);
        },
    }

    // Only generate section if we have content on the stack
    if (tree.stack.items.len > 0) {
        try output.begin_section("PARSE");
        try output.begin_code_block("clojure");

        tree.toStringPretty(output.md_writer.any());
        try output.md_writer.writeAll("\n");

        // Generate HTML output with syntax highlighting
        if (output.html_writer) |writer| {
            try writer.writeAll(
                \\                <pre class="ast-parse">
            );

            tree.toHtml(writer.any());

            try writer.writeAll(
                \\</pre>
                \\
            );
        }

        try output.end_code_block();
        try output.end_section();
    }
}

/// Generate FORMATTED section for both markdown and HTML
fn generateFormattedSection(output: *DualOutput, content: *const Content, parse_ast: *AST) !void {
    var formatted = std.ArrayList(u8).init(output.gpa);
    defer formatted.deinit();

    switch (content.meta.node_type) {
        .file => {
            try fmt.formatAst(parse_ast.*, formatted.writer().any());
        },
        .header => {
            try fmt.formatHeader(parse_ast.*, formatted.writer().any());
        },
        .expr => {
            try fmt.formatExpr(parse_ast.*, formatted.writer().any());
        },
        .statement => {
            try fmt.formatStatement(parse_ast.*, formatted.writer().any());
        },
        .package => {
            try fmt.formatAst(parse_ast.*, formatted.writer().any());
        },
        .platform => {
            try fmt.formatAst(parse_ast.*, formatted.writer().any());
        },
        .app => {
            try fmt.formatAst(parse_ast.*, formatted.writer().any());
        },
    }

    const is_changed = !std.mem.eql(u8, formatted.items, content.source);
    const display_content = if (is_changed) formatted.items else "NO CHANGE";

    try output.begin_section("FORMATTED");
    try output.begin_code_block("roc");

    try output.md_writer.writeAll(display_content);
    try output.md_writer.writeAll("\n");

    // HTML FORMATTED section
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <pre>
        );

        // Escape HTML in formatted content
        for (display_content) |char| {
            try escapeHtmlChar(writer, char);
        }

        try writer.writeAll(
            \\</pre>
            \\
        );
    }
    try output.end_code_block();
    try output.end_section();
}

/// Generate CANONICALIZE section for both markdown and HTML
fn generateCanonicalizeSection(output: *DualOutput, content: *const Content, can_ir: *CIR, maybe_expr_idx: ?CIR.Expr.Idx) !void {
    var tree = SExprTree.init(output.gpa);
    defer tree.deinit();
    can_ir.pushToSExprTree(maybe_expr_idx, &tree, content.source);

    try output.begin_section("CANONICALIZE");
    try output.begin_code_block("clojure");

    tree.toStringPretty(output.md_writer.any());
    try output.md_writer.writeAll("\n");

    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <pre>
        );
        tree.toHtml(writer.any());
        try writer.writeAll(
            \\</pre>
            \\
        );
    }

    try output.end_code_block();
    try output.end_section();
}

/// Generate TYPES section for both markdown and HTML
fn generateTypesSection(output: *DualOutput, content: *const Content, can_ir: *CIR, maybe_expr_idx: ?CIR.Expr.Idx) !void {
    var tree = SExprTree.init(output.gpa);
    defer tree.deinit();
    can_ir.pushTypesToSExprTree(maybe_expr_idx, &tree, content.source);

    try output.begin_section("TYPES");
    try output.begin_code_block("clojure");
    tree.toStringPretty(output.md_writer.any());
    try output.md_writer.writeAll("\n");

    // HTML TYPES section
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\                <pre>
        );
        tree.toHtml(writer.any());
        try writer.writeAll(
            \\</pre>
            \\
        );
    }
    try output.end_code_block();
    try output.end_section();
}

/// Generate TYPES section displaying types store for both markdown and HTML
/// This is used for debugging.
fn generateTypesStoreSection(gpa: std.mem.Allocator, output: *DualOutput, can_ir: *CIR) !void {
    var solved = std.ArrayList(u8).init(output.gpa);
    defer solved.deinit();

    try types_mod.writers.SExprWriter.allVarsToSExprStr(solved.writer().any(), gpa, can_ir.env);

    // Markdown TYPES section
    try output.md_writer.writeAll(Section.TYPES);
    try output.md_writer.writeAll(solved.items);
    try output.md_writer.writeAll("\n");
    try output.md_writer.writeAll(Section.SECTION_END[0 .. Section.SECTION_END.len - 1]);

    // HTML TYPES section
    if (output.html_writer) |writer| {
        try writer.writeAll(
            \\        <div class="section">
            \\            <div class="section-header">TYPES</div>
            \\            <div class="section-content">
            \\                <pre>
        );

        // Escape HTML in types content
        for (solved.items) |char| {
            try escapeHtmlChar(writer, char);
        }

        try writer.writeAll(
            \\</pre>
            \\            </div>
            \\        </div>
            \\
        );
    }
}

/// Generate HTML document structure and JavaScript
fn generateHtmlWrapper(output: *DualOutput, content: *const Content) !void {
    const writer = output.html_writer orelse return;

    // Write HTML document structure
    try writer.writeAll(
        \\<!DOCTYPE html>
        \\<html lang="en">
        \\<head>
        \\    <meta charset="UTF-8">
        \\    <meta name="viewport" content="width=device-width, initial-scale=1.0">
        \\    <title>Roc Snapshot:
    );
    try writer.writeAll(content.meta.description);
    try writer.writeAll(
        \\</title>
        \\    <style>
        \\
    );
    try writer.writeAll(@embedFile("snapshot.css"));
    try writer.writeAll(
        \\    </style>
        \\</head>
        \\<body>
        \\    <!-- Two-column layout (main and only view) -->
        \\    <div class="two-column-layout">
        \\        <div class="left-pane">
        \\            <div class="pane-header">
        \\                <select class="section-dropdown" id="left-selector" onchange="switchLeftPane()">
        \\                    <option value="META">META</option>
        \\                    <option value="SOURCE" selected>SOURCE</option>
        \\                </select>
        \\            </div>
        \\            <div class="pane-content" id="left-pane-content">
        \\                <!-- Left pane content will be shown here -->
        \\            </div>
        \\        </div>
        \\        <div class="right-pane">
        \\            <div class="pane-header">
        \\                <select class="section-dropdown" id="right-selector" onchange="switchRightPane()">
        \\                    <option value="TOKENS" selected>TOKENS</option>
        \\                    <option value="PARSE">PARSE</option>
        \\                    <option value="FORMATTED">FORMATTED</option>
        \\                    <option value="CANONICALIZE">CANONICALIZE</option>
        \\                    <option value="TYPES">TYPES</option>
        \\                </select>
        \\            </div>
        \\            <div class="pane-content" id="right-pane-content">
        \\                <!-- Right pane content will be shown here -->
        \\            </div>
        \\        </div>
        \\    </div>
        \\
        \\    <!-- Hidden sections for data storage -->
        \\    <div id="data-sections" style="display: none;">
    );
}

/// Generate HTML closing tags and JavaScript
fn generateHtmlClosing(output: *DualOutput) !void {
    const writer = output.html_writer orelse return;

    // Close data sections container and add JavaScript
    try writer.writeAll(
        \\    </div>
        \\
        \\    <script>
    );
    // Embed remaining snapshot.js directly into the HTML
    try writer.writeAll(@embedFile("snapshot.js"));
    try writer.writeAll(
        \\    </script>
        \\</body>
        \\</html>
        \\
    );
}

/// Write HTML buffer to file
fn writeHtmlFile(gpa: Allocator, snapshot_path: []const u8, html_buffer: *std.ArrayList(u8)) !void {
    // Convert .md path to .html path
    const html_path = blk: {
        if (std.mem.endsWith(u8, snapshot_path, ".md")) {
            const base_path = snapshot_path[0 .. snapshot_path.len - 3];
            break :blk try std.fmt.allocPrint(gpa, "{s}.html", .{base_path});
        } else {
            break :blk try std.fmt.allocPrint(gpa, "{s}.html", .{snapshot_path});
        }
    };
    defer gpa.free(html_path);

    // Write HTML file
    var html_file = std.fs.cwd().createFile(html_path, .{}) catch |err| {
        log("failed to create HTML file '{s}': {s}", .{ html_path, @errorName(err) });
        return;
    };
    defer html_file.close();
    try html_file.writer().writeAll(html_buffer.items);

    log("generated HTML version: {s}", .{html_path});
}

/// New unified processSnapshotFile function that generates both markdown and HTML simultaneously
fn processSnapshotFileUnified(gpa: Allocator, snapshot_path: []const u8, maybe_fuzz_corpus_path: ?[]const u8, generate_html: bool) !bool {
    // Log the file path that was written to
    log("processing snapshot file: {s}", .{snapshot_path});

    const @"1Mb" = 1024 * 1024;
    const file_content = std.fs.cwd().readFileAlloc(gpa, snapshot_path, @"1Mb") catch |err| {
        std.log.err("failed to read file '{s}': {s}", .{ snapshot_path, @errorName(err) });
        return false;
    };
    defer gpa.free(file_content);

    // Check our file starts with the metadata section
    if (!std.mem.startsWith(u8, file_content, Section.META)) {
        std.log.err("file '{s}' is not a valid snapshot file", .{snapshot_path});
        std.log.err("snapshot files must start with '~~~META'", .{});
        if (file_content.len > 0) {
            const first_line_end = std.mem.indexOfScalar(u8, file_content, '\n') orelse @min(file_content.len, 50);
            const first_line = file_content[0..first_line_end];
            std.log.err("file starts with: '{s}'", .{first_line});
        }
        return false;
    }

    // Parse the file to find section boundaries
    const content = extractSections(gpa, file_content) catch |err| {
        switch (err) {
            Error.MissingSnapshotHeader => {
                std.log.err("file '{s}' is missing the META section header", .{snapshot_path});
                std.log.err("add a META section like: ~~~META\\ndescription=My test\\ntype=expr\\n", .{});
                return false;
            },
            Error.MissingSnapshotSource => {
                std.log.err("file '{s}' is missing the SOURCE section", .{snapshot_path});
                std.log.err("add a SOURCE section like: ~~~SOURCE\\nyour_roc_code_here\\n", .{});
                return false;
            },
            Error.BadSectionHeader => {
                std.log.err("file '{s}' has an invalid section header", .{snapshot_path});
                std.log.err("section headers must be like: ~~~META, ~~~SOURCE, etc.", .{});
                return false;
            },
            else => return err,
        }
    };

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Parse the source code (ONCE)
    var parse_ast = switch (content.meta.node_type) {
        .file => parse.parse(&module_env, content.source),
        .header => parse.parseHeader(&module_env, content.source),
        .expr => parse.parseExpr(&module_env, content.source),
        .statement => parse.parseStatement(&module_env, content.source),
        .package => parse.parse(&module_env, content.source),
        .platform => parse.parse(&module_env, content.source),
        .app => parse.parse(&module_env, content.source),
    };
    defer parse_ast.deinit(gpa);

    parse_ast.store.emptyScratch();

    // Canonicalize the source code (ONCE)
    // Extract module name from snapshot path
    const basename = std.fs.path.basename(snapshot_path);
    const module_name = if (std.mem.lastIndexOfScalar(u8, basename, '.')) |dot_idx|
        basename[0..dot_idx]
    else
        basename;
    var can_ir = CIR.init(&module_env, module_name);
    defer can_ir.deinit();

    var can = try canonicalize.init(&can_ir, &parse_ast, null);
    defer can.deinit();

    var maybe_expr_idx: ?CIR.Expr.Idx = null;

    switch (content.meta.node_type) {
        .file => try can.canonicalizeFile(),
        .header => {
            // TODO: implement canonicalize_header when available
        },
        .expr => {
            const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
            maybe_expr_idx = try can.canonicalizeExpr(expr_idx);
        },
        .statement => {
            // Manually track scratch statements because we aren't using the file entrypoint
            const stmt_idx: AST.Statement.Idx = @enumFromInt(parse_ast.root_node_idx);
            const scratch_statements_start = can_ir.store.scratch_statements.top();
            _ = try can.canonicalizeStatement(stmt_idx);
            can_ir.all_statements = can_ir.store.statementSpanFrom(scratch_statements_start);
        },
        .package => try can.canonicalizeFile(),
        .platform => try can.canonicalizeFile(),
        .app => try can.canonicalizeFile(),
    }

    // Types (ONCE)
    const empty_modules: []const *CIR = &.{};
    var solver = try Solver.init(gpa, &can_ir.env.types, &can_ir, empty_modules);
    defer solver.deinit();

    if (maybe_expr_idx) |expr_idx| {
        _ = try solver.checkExpr(expr_idx);
    } else {
        try solver.checkDefs();
    }

    // Cache round-trip validation - ensure ModuleCache serialization/deserialization works
    {
        // Generate original S-expression for comparison
        var original_tree = SExprTree.init(gpa);
        defer original_tree.deinit();
        CIR.pushToSExprTree(&can_ir, null, &original_tree, content.source);

        var original_sexpr = std.ArrayList(u8).init(gpa);
        defer original_sexpr.deinit();
        original_tree.toStringPretty(original_sexpr.writer().any());

        // Create and serialize MmapCache
        const cache_data = try cache.CacheModule.create(gpa, &module_env, &can_ir);
        defer gpa.free(cache_data);

        // Deserialize back
        var loaded_cache = try cache.CacheModule.fromMappedMemory(cache_data);

        // Restore ModuleEnv and CIR
        // Extract module name from snapshot path
        const cache_basename = std.fs.path.basename(snapshot_path);
        const cache_module_name = if (std.mem.lastIndexOfScalar(u8, cache_basename, '.')) |dot_idx|
            cache_basename[0..dot_idx]
        else
            cache_basename;
        const restored = try loaded_cache.restore(gpa, cache_module_name);
        var restored_module_env = restored.module_env;
        defer restored_module_env.deinit();
        var restored_cir = restored.cir;
        defer restored_cir.deinit();

        // Fix env pointer after struct move
        restored_cir.env = &restored_module_env;

        // Generate S-expression from restored CIR
        var restored_tree = SExprTree.init(gpa);
        defer restored_tree.deinit();
        CIR.pushToSExprTree(&restored_cir, null, &restored_tree, content.source);

        var restored_sexpr = std.ArrayList(u8).init(gpa);
        defer restored_sexpr.deinit();
        restored_tree.toStringPretty(restored_sexpr.writer().any());

        // Compare S-expressions - crash if they don't match
        if (!std.mem.eql(u8, original_sexpr.items, restored_sexpr.items)) {
            std.log.err("Cache round-trip validation failed for snapshot: {s}", .{snapshot_path});
            std.log.err("Original and restored CIR S-expressions don't match!", .{});
            std.log.err("This indicates a bug in MmapCache serialization/deserialization.", .{});
            std.log.err("Original S-expression:\n{s}", .{original_sexpr.items});
            std.log.err("Restored S-expression:\n{s}", .{restored_sexpr.items});
            return false;
        }
    }

    // Buffer all output in memory before writing files
    var md_buffer = std.ArrayList(u8).init(gpa);
    defer md_buffer.deinit();

    var html_buffer = if (generate_html) std.ArrayList(u8).init(gpa) else null;
    defer if (html_buffer) |*buf| buf.deinit();

    var output = DualOutput.init(gpa, &md_buffer, if (html_buffer) |*buf| buf else null);

    // Generate HTML wrapper
    try generateHtmlWrapper(&output, &content);

    // Generate all sections simultaneously
    try generateMetaSection(&output, &content);
    try generateSourceSection(&output, &content);
    try generateExpectedSection(&output, &content);
    try generateProblemsSection(&output, &parse_ast, &can_ir, &solver, &content, snapshot_path, &module_env);
    try generateTokensSection(&output, &parse_ast, &content, &module_env);

    // Generate remaining sections
    try generateParseSection(&output, &content, &parse_ast, &module_env);
    try generateFormattedSection(&output, &content, &parse_ast);
    try generateCanonicalizeSection(&output, &content, &can_ir, maybe_expr_idx);
    try generateTypesSection(&output, &content, &can_ir, maybe_expr_idx);
    // TODO: Include to emit entire types store. Can be helpful for debugging
    // try generateTypesStoreSection(gpa, &output, &can_ir);

    // Generate HTML closing
    try generateHtmlClosing(&output);

    // Write markdown file
    var md_file = std.fs.cwd().createFile(snapshot_path, .{}) catch |err| {
        log("failed to create file '{s}': {s}", .{ snapshot_path, @errorName(err) });
        return false;
    };
    defer md_file.close();
    try md_file.writer().writeAll(md_buffer.items);

    // Write HTML file
    if (html_buffer) |*buf| {
        try writeHtmlFile(gpa, snapshot_path, buf);
    }

    // If flag --fuzz-corpus is passed, write the SOURCE to our corpus
    if (maybe_fuzz_corpus_path != null) {
        const rand_file_name = [_][]const u8{
            maybe_fuzz_corpus_path.?, &[_]u8{
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                rand.intRangeAtMost(u8, 'a', 'z'),
                '.',
                'r',
                'o',
                'c',
            },
        };

        const corpus_file_path = try std.fs.path.join(gpa, &rand_file_name);
        defer gpa.free(corpus_file_path);

        var corpus_file = std.fs.cwd().createFile(corpus_file_path, .{}) catch |err| {
            log("failed to create file in '{s}': {s}", .{ maybe_fuzz_corpus_path.?, @errorName(err) });
            return false;
        };
        defer corpus_file.close();

        try corpus_file.writer().writeAll(content.source);
    }

    return true;
}

fn processSnapshotFile(gpa: Allocator, snapshot_path: []const u8, maybe_fuzz_corpus_path: ?[]const u8, generate_html: bool) !bool {
    return processSnapshotFileUnified(gpa, snapshot_path, maybe_fuzz_corpus_path, generate_html);
}

/// Extracts the sections from a snapshot file
fn extractSections(gpa: Allocator, content: []const u8) !Content {
    var ranges = std.AutoHashMap(Section, Section.Range).init(gpa);
    defer ranges.deinit();

    // Find all section headers and their positions
    var idx: usize = 0;
    while (idx < content.len) {
        // Look for section headers
        if (idx == 0 or (idx > 0 and content[idx - 1] == '\n')) {
            if (Section.fromString(content[idx..])) |section| {
                // Only process META, SOURCE, and EXPECTED sections
                if (section == .meta or section == .source or section == .expected) {
                    const header_len = section.asString().len;
                    const start = idx + header_len;

                    // Find the end of this section
                    var end = content.len;

                    // For sections with ~~~ delimiters (META and SOURCE)
                    if (section == .meta or section == .source) {
                        // Find the closing ~~~
                        var search_idx = start;
                        while (search_idx < content.len - 3) {
                            if (content[search_idx] == '~' and
                                content[search_idx + 1] == '~' and
                                content[search_idx + 2] == '~')
                            {
                                // Set end to the position of ~~~, not after it
                                end = search_idx;
                                break;
                            }
                            search_idx += 1;
                        }
                    } else {
                        // For sections without ~~~ delimiters (EXPECTED)
                        // Find the next section header
                        var search_idx = start;
                        while (search_idx < content.len) {
                            if (search_idx == 0 or (search_idx > 0 and content[search_idx - 1] == '\n')) {
                                if (content[search_idx] == '#' and
                                    search_idx + 1 < content.len and
                                    content[search_idx + 1] == ' ')
                                {
                                    end = search_idx;
                                    break;
                                }
                            }
                            search_idx += 1;
                        }
                    }

                    try ranges.put(section, .{ .start = start, .end = end });

                    // Skip to the end of this section
                    idx = end;
                    continue;
                }
            }
        }
        idx += 1;
    }

    return try Content.from_ranges(ranges, content);
}
