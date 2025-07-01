const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const base = @import("base.zig");
const canonicalize = @import("check/canonicalize.zig");
const types_mod = @import("types.zig");
const Solver = @import("check/check_types.zig");
const CIR = canonicalize.CIR;
const parse = @import("check/parse.zig");
const fmt = @import("fmt.zig");
const types = @import("types.zig");
const reporting = @import("reporting.zig");
const tokenize = @import("check/parse/tokenize.zig");
const SExpr = @import("base/SExpr.zig");

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
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        _ = gpa_impl.deinit();
    }
    const gpa = gpa_impl.allocator();

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    var snapshot_paths = std.ArrayList([]const u8).init(gpa);
    defer snapshot_paths.deinit();

    var maybe_fuzz_corpus_path: ?[]const u8 = null;
    var expect_fuzz_corpus_path: bool = false;

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--verbose")) {
            verbose_log = true;
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

    if (maybe_fuzz_corpus_path != null) {
        log("copying SOURCE from snapshots to: {s}", .{maybe_fuzz_corpus_path.?});
        try std.fs.cwd().makePath(maybe_fuzz_corpus_path.?);
    }

    const snapshots_dir = "src/snapshots";
    var file_count: usize = 0;
    var timer = std.time.Timer.start() catch unreachable;

    if (snapshot_paths.items.len > 0) {
        for (snapshot_paths.items) |path| {
            file_count += try processPath(gpa, path, maybe_fuzz_corpus_path);
        }
    } else {
        // process all files in snapshots_dir
        file_count = try processPath(gpa, snapshots_dir, maybe_fuzz_corpus_path);
    }

    const duration_ms = timer.read() / std.time.ns_per_ms;

    std.log.info("processed {d} snapshots in {d} ms.", .{ file_count, duration_ms });
}

/// Check if a file has a valid snapshot extension
fn isSnapshotFile(filename: []const u8) bool {
    return std.mem.endsWith(u8, filename, ".md");
}

fn processPath(gpa: Allocator, path: []const u8, maybe_fuzz_corpus_path: ?[]const u8) !usize {
    var processed_count: usize = 0;

    const canonical_path = std.fs.cwd().realpathAlloc(gpa, path) catch |err| {
        std.log.err("failed to resolve path '{s}': {s}", .{ path, @errorName(err) });
        return 0;
    };
    defer gpa.free(canonical_path);

    const stat = std.fs.cwd().statFile(canonical_path) catch |err| {
        std.log.err("failed to stat path '{s}': {s}", .{ canonical_path, @errorName(err) });
        return 0;
    };

    if (stat.kind == .directory) {
        var dir = try std.fs.cwd().openDir(path, .{ .iterate = true });
        defer dir.close();

        var dir_iterator = dir.iterate();
        while (try dir_iterator.next()) |entry| {

            // Skip hidden files and special directories
            if (entry.name[0] == '.') continue;

            const full_path = try std.fs.path.join(gpa, &[_][]const u8{ canonical_path, entry.name });
            defer gpa.free(full_path);

            if (entry.kind == .directory) {
                processed_count += try processPath(gpa, full_path, maybe_fuzz_corpus_path);
            } else if (entry.kind == .file and isSnapshotFile(entry.name)) {
                if (try processSnapshotFile(gpa, full_path, maybe_fuzz_corpus_path)) {
                    processed_count += 1;
                } else {
                    log("skipped file (not a valid snapshot): {s}", .{full_path});
                }
            }
        }
    } else if (stat.kind == .file) {
        if (isSnapshotFile(canonical_path)) {
            if (try processSnapshotFile(gpa, canonical_path, maybe_fuzz_corpus_path)) {
                processed_count += 1;
            } else {
                std.log.err("failed to process snapshot file: {s}", .{canonical_path});
                std.log.err("make sure the file starts with '~~~META' and has valid snapshot format", .{});
            }
        } else {
            std.log.err("file '{s}' is not a snapshot file (must end with .md)", .{canonical_path});
        }
    }

    return processed_count;
}

/// Represents the different sections of a snapshot file.
const Section = union(enum) {
    meta,
    source,
    formatted,
    parse,
    canonicalize,
    tokens,
    problems,
    types,

    pub const META = "# META\n~~~ini\n";
    pub const SOURCE = "# SOURCE\n~~~roc\n";
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

    const HEADER = "header";
    const EXPR = "expr";
    const STMT = "statement";
    const FILE = "file";

    fn fromString(ty: []const u8) Error!NodeType {
        if (std.mem.eql(u8, ty, HEADER)) return .header;
        if (std.mem.eql(u8, ty, EXPR)) return .expr;
        if (std.mem.eql(u8, ty, STMT)) return .statement;
        if (std.mem.eql(u8, ty, FILE)) return .file;
        return Error.InvalidNodeType;
    }

    fn toString(self: NodeType) []const u8 {
        return switch (self) {
            .file => "file",
            .header => "header",
            .expr => "expr",
            .statement => "statement",
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
    formatted: ?[]const u8,
    has_canonicalize: bool,

    fn init(meta: Meta, source: []const u8, formatted: ?[]const u8, has_canonicalize: bool) Content {
        return .{
            .meta = meta,
            .source = source,
            .formatted = formatted,
            .has_canonicalize = has_canonicalize,
        };
    }

    fn from_ranges(ranges: std.AutoHashMap(Section, Section.Range), content: []const u8) Error!Content {
        var source: []const u8 = undefined;
        var formatted: ?[]const u8 = undefined;
        var has_canonicalize: bool = false;

        if (ranges.get(.source)) |value| {
            source = value.extract(content);
        } else {
            return Error.MissingSnapshotSource;
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
    html_writer: std.ArrayList(u8).Writer,
    gpa: Allocator,

    fn init(gpa: Allocator, md_buffer: *std.ArrayList(u8), html_buffer: *std.ArrayList(u8)) DualOutput {
        return .{
            .md_writer = md_buffer.writer(),
            .html_writer = html_buffer.writer(),
            .gpa = gpa,
        };
    }

    fn begin_section(self: *DualOutput, name: []const u8) !void {
        try self.md_writer.print("# {s}\n", .{name});
        try self.html_writer.print(
            \\        <div class="section" data-section="{s}">
            \\            <div class="section-content">
        , .{name});
    }

    fn end_section(self: *DualOutput) !void {
        try self.html_writer.writeAll(
            \\            </div>
            \\        </div>
        );
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
    try output.html_writer.writeAll(
        \\                <div class="meta-info">
        \\                    <p><strong>Description:</strong>
    );
    try output.html_writer.writeAll(content.meta.description);
    try output.html_writer.writeAll("</p>\n                    <p><strong>Type:</strong> ");
    try output.html_writer.writeAll(content.meta.node_type.toString());
    try output.html_writer.writeAll(
        \\</p>
        \\                </div>
        \\
    );

    try output.end_code_block();
    try output.end_section();
}

/// Generate SOURCE section for both markdown and HTML
fn generateSourceSection(output: *DualOutput, content: *const Content, parse_ast: *AST) !void {
    try output.begin_section("SOURCE");
    try output.begin_code_block("roc");
    try output.md_writer.writeAll(content.source);
    try output.md_writer.writeAll("\n");

    // HTML SOURCE section with syntax highlighting
    try output.html_writer.writeAll(
        \\                <div class="source-code">
    );
    // Apply syntax highlighting by processing tokens in order
    var tokenizedBuffer = parse_ast.tokens;
    const tokens = tokenizedBuffer.tokens.items(.tag);
    var source_offset: u32 = 0;
    var line_num: u32 = 1;
    var col_num: u32 = 1;

    try output.html_writer.print("<span class=\"source-line\" data-line=\"{d}\">", .{line_num});

    for (tokens, 0..) |tok, i| {
        const region = tokenizedBuffer.resolve(@intCast(i));

        // Output any characters between last token and this token (whitespace, etc.)
        while (source_offset < region.start.offset) {
            const char = content.source[source_offset];
            if (char == '\n') {
                try output.html_writer.writeAll("\n</span>");
                line_num += 1;
                col_num = 1;
                try output.html_writer.print("<span class=\"source-line\" data-line=\"{d}\">", .{line_num});
            } else if (char == ' ') {
                // Render space as regular space for wrapping
                try output.html_writer.writeAll(" ");
                col_num += 1;
            } else if (char == '\t') {
                // Render tab as 4 spaces
                try output.html_writer.writeAll("    ");
                col_num += 4;
            } else {
                try escapeHtmlChar(output.html_writer, char);
                col_num += 1;
            }
            source_offset += 1;
        }

        // Skip newline tokens since we handle newlines in whitespace above
        if (tok == .Newline) {
            continue;
        }

        // Output the token with syntax highlighting
        const category = tokenToCategory(tok);
        const token_text = content.source[region.start.offset..region.end.offset];

        try output.html_writer.print("<span class=\"{s}\" data-token-id=\"{d}\">", .{ category.toCssClass(), i });

        for (token_text) |char| {
            if (char == ' ') {
                try output.html_writer.writeAll(" ");
                col_num += 1;
            } else if (char == '\t') {
                try output.html_writer.writeAll("    ");
                col_num += 4;
            } else {
                try escapeHtmlChar(output.html_writer, char);
                col_num += 1;
            }
        }

        try output.html_writer.writeAll("</span>");
        source_offset = region.end.offset;
    }

    // Output any remaining characters
    while (source_offset < content.source.len) {
        const char = content.source[source_offset];
        if (char == '\n') {
            try output.html_writer.writeAll("\n</span>");
            line_num += 1;
            col_num = 1;
            if (source_offset + 1 < content.source.len) {
                try output.html_writer.print("<span class=\"source-line\" data-line=\"{d}\">", .{line_num});
            }
        } else if (char == ' ') {
            try output.html_writer.writeAll(" ");
            col_num += 1;
        } else if (char == '\t') {
            try output.html_writer.writeAll("    ");
            col_num += 4;
        } else {
            try escapeHtmlChar(output.html_writer, char);
            col_num += 1;
        }
        source_offset += 1;
    }

    try output.html_writer.writeAll("</span>");

    try output.html_writer.writeAll(
        \\
        \\                </div>
        \\
    );
    try output.end_code_block();
    try output.end_section();
}

/// Generate PROBLEMS section for both markdown and HTML
fn generateProblemsSection(output: *DualOutput, parse_ast: *AST, can_ir: *CIR, solver: *Solver, content: *const Content, snapshot_path: []const u8, module_env: *base.ModuleEnv) !void {
    try output.begin_section("PROBLEMS");

    // HTML PROBLEMS section
    try output.html_writer.writeAll(
        \\                <div class="problems">
    );

    var tokenize_problems: usize = 0;
    var parser_problems: usize = 0;
    var canonicalize_problems: usize = 0;
    var check_types_problem: usize = 0;

    // Tokenize Diagnostics
    for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
        tokenize_problems += 1;
        var report: reporting.Report = parse_ast.tokenizeDiagnosticToReport(diagnostic, output.gpa) catch |err| {
            try output.md_writer.print("Error creating tokenize report: {}\n", .{err});
            try output.html_writer.print("                    <p>Error creating tokenize report: {}</p>\n", .{err});
            continue;
        };
        defer report.deinit();

        report.render(output.md_writer.any(), .markdown) catch |err| {
            try output.md_writer.print("Error rendering report: {}\n", .{err});
        };

        try output.html_writer.writeAll("                    <div class=\"problem\">");
        report.render(output.html_writer.any(), .markdown) catch |err| {
            try output.html_writer.print("Error rendering report: {}", .{err});
        };
        try output.html_writer.writeAll("</div>\n");
    }

    // Parser Diagnostics
    for (parse_ast.parse_diagnostics.items) |diagnostic| {
        parser_problems += 1;
        var report: reporting.Report = parse_ast.parseDiagnosticToReport(diagnostic, output.gpa, snapshot_path) catch |err| {
            try output.md_writer.print("Error creating parse report: {}\n", .{err});
            try output.html_writer.print("                    <p>Error creating parse report: {}</p>\n", .{err});
            continue;
        };
        defer report.deinit();

        report.render(output.md_writer.any(), .markdown) catch |err| {
            try output.md_writer.print("Error rendering report: {}\n", .{err});
        };

        try output.html_writer.writeAll("                    <div class=\"problem\">");
        report.render(output.html_writer.any(), .markdown) catch |err| {
            try output.html_writer.print("Error rendering report: {}", .{err});
        };
        try output.html_writer.writeAll("</div>\n");
    }

    // Canonicalization Diagnostics
    const diagnostics = can_ir.getDiagnostics();
    defer output.gpa.free(diagnostics);
    for (diagnostics) |diagnostic| {
        canonicalize_problems += 1;
        var report: reporting.Report = can_ir.diagnosticToReport(diagnostic, output.gpa, content.source, snapshot_path) catch |err| {
            try output.md_writer.print("Error creating canonicalization report: {}\n", .{err});
            try output.html_writer.print("                    <p>Error creating canonicalization report: {}</p>\n", .{err});
            continue;
        };
        defer report.deinit();

        report.render(output.md_writer.any(), .markdown) catch |err| {
            try output.md_writer.print("Error rendering report: {}\n", .{err});
        };

        try output.html_writer.writeAll("                    <div class=\"problem\">");
        report.render(output.html_writer.any(), .markdown) catch |err| {
            try output.html_writer.print("Error rendering report: {}", .{err});
        };
        try output.html_writer.writeAll("</div>\n");
    }

    // Check Types Problems
    var problem_buf = std.ArrayList(u8).init(output.gpa);
    defer problem_buf.deinit();

    var problems_itr = solver.problems.problems.iterIndices();
    while (problems_itr.next()) |problem_idx| {
        check_types_problem += 1;
        const problem = solver.problems.problems.get(problem_idx);
        var report: reporting.Report = problem.buildReport(
            output.gpa,
            &problem_buf,
            module_env,
            can_ir,
            &solver.snapshots,
            content.source,
            snapshot_path,
        ) catch |err| {
            try output.md_writer.print("Error creating type checking report: {}\n", .{err});
            try output.html_writer.print("                    <p>Error creating type checking report: {}</p>\n", .{err});
            continue;
        };
        defer report.deinit();

        report.render(output.md_writer.any(), .markdown) catch |err| {
            try output.md_writer.print("Error rendering report: {}\n", .{err});
        };

        try output.html_writer.writeAll("                    <div class=\"problem\">");
        report.render(output.html_writer.any(), .markdown) catch |err| {
            try output.html_writer.print("Error rendering report: {}", .{err});
        };
        try output.html_writer.writeAll("</div>\n");
    }

    const nil_problems = tokenize_problems == 0 and parser_problems == 0 and canonicalize_problems == 0 and check_types_problem == 0;

    if (nil_problems) {
        try output.md_writer.writeAll("NIL\n");
        try output.html_writer.writeAll("                    <p>NIL</p>\n");
        log("reported NIL problems", .{});
    } else {
        log("reported {} token problems", .{tokenize_problems});
        log("reported {} parser problems", .{parser_problems});
        log("reported {} canonicalization problems", .{canonicalize_problems});
        log("reported {} type problems", .{check_types_problem});
    }

    try output.html_writer.writeAll(
        \\                </div>
        \\
    );

    try output.end_section();
}

/// Generate TOKENS section for both markdown and HTML
fn generateTokensSection(output: *DualOutput, parse_ast: *AST, content: *const Content, module_env: *base.ModuleEnv) !void {
    try output.begin_section("TOKENS");
    try output.begin_code_block("zig");

    try output.html_writer.writeAll(
        \\                <div class="token-list">
    );

    var tokenizedBuffer = parse_ast.tokens;
    const tokens = tokenizedBuffer.tokens.items(.tag);
    for (tokens, 0..) |tok, i| {
        const region = tokenizedBuffer.resolve(@intCast(i));
        const info = try module_env.calcRegionInfo(content.source, region.start.offset, region.end.offset);
        const category = tokenToCategory(tok);

        // Markdown token output
        const region_str = try std.fmt.allocPrint(output.gpa, "{s}({d}:{d}-{d}:{d}),", .{
            @tagName(tok),
            // add one to display numbers instead of index
            info.start_line_idx + 1,
            info.start_col_idx + 1,
            info.end_line_idx + 1,
            info.end_col_idx + 1,
        });
        defer output.gpa.free(region_str);

        try output.md_writer.writeAll(region_str);

        // HTML token output (without line:col ranges)
        try output.html_writer.print("                    <span class=\"token-item {s}\" data-token-id=\"{d}\">{s}</span>", .{
            category.toCssClass(),
            i,
            @tagName(tok),
        });

        if (tok == .Newline) {
            try output.md_writer.writeAll("\n");
            try output.html_writer.writeAll("\n");
            try output.html_writer.writeAll("<br>");
        } else {
            try output.html_writer.writeAll(" ");
        }
    }

    try output.md_writer.writeAll("\n");

    try output.html_writer.writeAll(
        \\                </div>
        \\
    );
    try output.end_code_block();
    try output.end_section();
}

/// Generate PARSE section for both markdown and HTML
fn generateParseSection(output: *DualOutput, content: *const Content, parse_ast: *AST, module_env: *base.ModuleEnv) !void {
    var parse_buffer = std.ArrayList(u8).init(output.gpa);
    defer parse_buffer.deinit();

    // Generate S-expression node based on content type
    var node_opt: ?SExpr = null;
    defer if (node_opt) |*node| node.deinit(output.gpa);

    switch (content.meta.node_type) {
        .file => {
            // Inline the toSExprStr logic for file case
            const file = parse_ast.store.getFile();
            node_opt = file.toSExpr(module_env, parse_ast);
        },
        .header => {
            const header = parse_ast.store.getHeader(@enumFromInt(parse_ast.root_node_idx));
            node_opt = header.toSExpr(module_env, parse_ast);
        },
        .expr => {
            const expr = parse_ast.store.getExpr(@enumFromInt(parse_ast.root_node_idx));
            node_opt = expr.toSExpr(module_env, parse_ast);
        },
        .statement => {
            const stmt = parse_ast.store.getStatement(@enumFromInt(parse_ast.root_node_idx));
            node_opt = stmt.toSExpr(module_env, parse_ast);
        },
    }

    if (node_opt) |node| {
        // Generate markdown output
        node.toStringPretty(parse_buffer.writer().any());

        try output.begin_section("PARSE");
        try output.begin_code_block("clojure");

        try output.md_writer.writeAll(parse_buffer.items);
        try output.md_writer.writeAll("\n");

        // Generate HTML output with syntax highlighting
        try output.html_writer.writeAll(
            \\                <pre class="ast-parse">
        );

        node.toHtml(output.html_writer.any());

        try output.html_writer.writeAll(
            \\</pre>
            \\
        );

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
    }

    const is_changed = !std.mem.eql(u8, formatted.items, content.source);
    const display_content = if (is_changed) formatted.items else "NO CHANGE";

    try output.begin_section("FORMATTED");
    try output.begin_code_block("roc");

    try output.md_writer.writeAll(display_content);
    try output.md_writer.writeAll("\n");

    // HTML FORMATTED section
    try output.html_writer.writeAll(
        \\                <pre>
    );

    // Escape HTML in formatted content
    for (display_content) |char| {
        try escapeHtmlChar(output.html_writer, char);
    }

    try output.html_writer.writeAll(
        \\</pre>
        \\
    );
    try output.end_code_block();
    try output.end_section();
}

/// Generate CANONICALIZE section for both markdown and HTML
fn generateCanonicalizeSection(output: *DualOutput, content: *const Content, can_ir: *CIR, maybe_expr_idx: ?CIR.Expr.Idx) !void {
    var canonicalized = std.ArrayList(u8).init(output.gpa);
    defer canonicalized.deinit();

    try can_ir.toSExprStr(canonicalized.writer().any(), maybe_expr_idx, content.source);

    try output.begin_section("CANONICALIZE");
    try output.begin_code_block("clojure");
    try output.md_writer.writeAll(canonicalized.items);
    try output.md_writer.writeAll("\n");

    // HTML CANONICALIZE section
    try output.html_writer.writeAll(
        \\                <pre>
    );

    // Escape HTML in canonicalized content
    for (canonicalized.items) |char| {
        try escapeHtmlChar(output.html_writer, char);
    }

    try output.html_writer.writeAll(
        \\</pre>
        \\
    );

    try output.end_code_block();
    try output.end_section();
}

/// Generate TYPES section for both markdown and HTML
fn generateTypesSection(output: *DualOutput, content: *const Content, can_ir: *CIR, maybe_expr_idx: ?CIR.Expr.Idx) !void {
    var solved = std.ArrayList(u8).init(output.gpa);
    defer solved.deinit();

    try can_ir.toSexprTypesStr(solved.writer().any(), maybe_expr_idx, content.source);

    try output.begin_section("TYPES");
    try output.begin_code_block("clojure");
    try output.md_writer.writeAll(solved.items);
    try output.md_writer.writeAll("\n");

    // HTML TYPES section
    try output.html_writer.writeAll(
        \\                <pre>
    );

    // Escape HTML in types content
    for (solved.items) |char| {
        try escapeHtmlChar(output.html_writer, char);
    }

    try output.html_writer.writeAll(
        \\</pre>
        \\
    );
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
    try output.html_writer.writeAll(
        \\        <div class="section">
        \\            <div class="section-header">TYPES</div>
        \\            <div class="section-content">
        \\                <pre>
    );

    // Escape HTML in types content
    for (solved.items) |char| {
        try escapeHtmlChar(output.html_writer, char);
    }

    try output.html_writer.writeAll(
        \\</pre>
        \\            </div>
        \\        </div>
        \\
    );
}

/// Generate HTML document structure and JavaScript
fn generateHtmlWrapper(output: *DualOutput, content: *const Content) !void {
    // Write HTML document structure
    try output.html_writer.writeAll(
        \\<!DOCTYPE html>
        \\<html lang="en">
        \\<head>
        \\    <meta charset="UTF-8">
        \\    <meta name="viewport" content="width=device-width, initial-scale=1.0">
        \\    <title>Roc Snapshot:
    );
    try output.html_writer.writeAll(content.meta.description);
    try output.html_writer.writeAll(
        \\</title>
        \\    <style>
        \\
    );
    try output.html_writer.writeAll(@embedFile("snapshot.css"));
    try output.html_writer.writeAll(
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
    // Close data sections container and add JavaScript
    try output.html_writer.writeAll(
        \\    </div>
        \\
        \\    <script>
        \\
    );
    // Embed snapshot.js directly into the HTML
    try output.html_writer.writeAll(@embedFile("snapshot.js"));
    try output.html_writer.writeAll(
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
fn processSnapshotFileUnified(gpa: Allocator, snapshot_path: []const u8, maybe_fuzz_corpus_path: ?[]const u8) !bool {
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
    };
    defer parse_ast.deinit(gpa);

    parse_ast.store.emptyScratch();

    // Canonicalize the source code (ONCE)
    var can_ir = CIR.init(&module_env);
    defer can_ir.deinit();

    var can = try canonicalize.init(&can_ir, &parse_ast);
    defer can.deinit();

    var maybe_expr_idx: ?CIR.Expr.Idx = null;

    switch (content.meta.node_type) {
        .file => try can.canonicalize_file(),
        .header => {
            // TODO: implement canonicalize_header when available
        },
        .expr => {
            const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
            maybe_expr_idx = try can.canonicalize_expr(expr_idx);
        },
        .statement => {
            // Manually track scratch statements because we aren't using the file entrypoint
            const stmt_idx: AST.Statement.Idx = @enumFromInt(parse_ast.root_node_idx);
            const scratch_statements_start = can_ir.store.scratch_statements.top();
            _ = try can.canonicalize_statement(stmt_idx);
            can_ir.all_statements = can_ir.store.statementSpanFrom(scratch_statements_start);
        },
    }

    // Types (ONCE)
    var solver = try Solver.init(gpa, &can_ir.env.types, &can_ir);
    defer solver.deinit();

    if (maybe_expr_idx) |expr_idx| {
        solver.checkExpr(expr_idx);
    } else {
        solver.checkDefs();
    }

    // Buffer all output in memory before writing files
    var md_buffer = std.ArrayList(u8).init(gpa);
    defer md_buffer.deinit();

    var html_buffer = std.ArrayList(u8).init(gpa);
    defer html_buffer.deinit();

    var output = DualOutput.init(gpa, &md_buffer, &html_buffer);

    // Generate HTML wrapper
    try generateHtmlWrapper(&output, &content);

    // Generate all sections simultaneously
    try generateMetaSection(&output, &content);
    try generateSourceSection(&output, &content, &parse_ast);
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
    try writeHtmlFile(gpa, snapshot_path, &html_buffer);

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

fn processSnapshotFile(gpa: Allocator, snapshot_path: []const u8, maybe_fuzz_corpus_path: ?[]const u8) !bool {
    return processSnapshotFileUnified(gpa, snapshot_path, maybe_fuzz_corpus_path);
}

/// Extracts the sections from a snapshot file
fn extractSections(gpa: Allocator, content: []const u8) !Content {
    var ranges = std.AutoHashMap(Section, Section.Range).init(gpa);
    defer ranges.deinit();

    // Ensure content ends with newline for consistent parsing
    const normalized_content = if (content.len > 0 and content[content.len - 1] != '\n') blk: {
        const normalized = try gpa.alloc(u8, content.len + 1);
        @memcpy(normalized[0..content.len], content);
        normalized[content.len] = '\n';
        break :blk normalized;
    } else content;

    defer if (normalized_content.ptr != content.ptr) gpa.free(normalized_content);

    var processed_chars: usize = 0;

    var sections_with_header = std.mem.splitSequence(u8, normalized_content, Section.SECTION_END);

    while (sections_with_header.next()) |section_with_header| {
        const trimmed_section = std.mem.trimLeft(u8, section_with_header, "\n\r \t");
        if (Section.fromString(trimmed_section)) |section| {
            // Only process META and SOURCE sections - ignore everything else
            if (section == .meta or section == .source) {
                const start = processed_chars + section.asString().len;
                // Ensure we don't exceed original content length
                const end = @min(processed_chars + section_with_header.len, content.len);
                try ranges.put(section, .{ .start = start, .end = end });
            }

            processed_chars += section_with_header.len + Section.SECTION_END.len;

            // If we have both META and SOURCE, we can stop parsing to avoid merge conflicts
            if (ranges.contains(.meta) and ranges.contains(.source)) {
                break;
            }
        } else {
            // Ignore invalid sections (like merge conflicts) instead of returning error
            processed_chars += section_with_header.len + Section.SECTION_END.len;
        }
    }

    return try Content.from_ranges(ranges, content);
}
