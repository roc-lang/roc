const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const base = @import("base.zig");
const canonicalize = @import("check/canonicalize.zig");
const Solver = @import("check/check_types.zig");
const CIR = canonicalize.CIR;
const parse = @import("check/parse.zig");
const fmt = @import("fmt.zig");
const types = @import("types.zig");
const reporting = @import("reporting.zig");

const AST = parse.AST;
const Report = reporting.Report;

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

fn processPath(gpa: Allocator, path: []const u8, maybe_fuzz_corpus_path: ?[]const u8) !usize {
    var processed_count: usize = 0;

    const canonical_path = std.fs.cwd().realpathAlloc(gpa, path) catch |err| {
        log("failed to resolve path '{s}': {s}", .{ path, @errorName(err) });
        return 0;
    };
    defer gpa.free(canonical_path);

    const stat = std.fs.cwd().statFile(canonical_path) catch |err| {
        log("failed to stat path '{s}': {s}", .{ canonical_path, @errorName(err) });
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
            } else if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".md")) {
                if (try processSnapshotFile(gpa, full_path, maybe_fuzz_corpus_path)) {
                    processed_count += 1;
                }
            }
        }
    } else if (stat.kind == .file) {
        if (std.mem.endsWith(u8, canonical_path, ".md")) {
            if (try processSnapshotFile(gpa, canonical_path, maybe_fuzz_corpus_path)) {
                processed_count += 1;
            }
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

    pub const META = "# META\n~~~ini\n";
    pub const SOURCE = "# SOURCE\n~~~roc\n";
    pub const FORMATTED = "# FORMATTED\n~~~roc\n";
    pub const PARSE = "# PARSE\n~~~clojure\n";
    pub const CANONICALIZE = "# CANONICALIZE\n~~~clojure\n";
    pub const TOKENS = "# TOKENS\n~~~zig\n";
    pub const PROBLEMS = "# PROBLEMS\n~~~txt\n";
    pub const TYPES = "# TYPES\n~~~txt\n";

    pub const SECTION_END = "~~~\n";

    fn fromString(str: []const u8) ?Section {
        if (std.mem.startsWith(u8, str, META)) return .meta;
        if (std.mem.startsWith(u8, str, SOURCE)) return .source;
        if (std.mem.startsWith(u8, str, FORMATTED)) return .formatted;
        if (std.mem.startsWith(u8, str, PARSE)) return .parse;
        if (std.mem.startsWith(u8, str, CANONICALIZE)) return .canonicalize;
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

fn processSnapshotFile(gpa: Allocator, snapshot_path: []const u8, maybe_fuzz_corpus_path: ?[]const u8) !bool {

    // Log the file path that was written to
    log("processing snapshot file: {s}", .{snapshot_path});

    const @"1Mb" = 1024 * 1024;
    const file_content = std.fs.cwd().readFileAlloc(gpa, snapshot_path, @"1Mb") catch |err| {
        log("failed to read file '{s}': {s}", .{ snapshot_path, @errorName(err) });
        return false;
    };
    defer gpa.free(file_content);

    // Check our file starts with the metadata section
    // so we can skip parsing and later steps if this isn't a snapshot file
    if (!std.mem.startsWith(u8, file_content, Section.META)) {
        log("ignoring non-snapshot file {s}", .{snapshot_path});
        return false;
    }

    // Parse the file to find section boundaries
    const content = extractSections(gpa, file_content) catch |err| {
        switch (err) {
            Error.MissingSnapshotHeader, Error.MissingSnapshotSource, Error.BadSectionHeader => {
                warn("ignoring file {s}: {s}", .{ snapshot_path, @errorName(err) });
                return false;
            },
            else => return err,
        }
    };

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Parse the source cod
    var parse_ast = switch (content.meta.node_type) {
        .file => parse.parse(&module_env, content.source),
        .header => parse.parseHeader(&module_env, content.source),
        .expr => parse.parseExpr(&module_env, content.source),
        .statement => parse.parseStatement(&module_env, content.source),
    };
    defer parse_ast.deinit(gpa);

    // shouldn't be required in future
    parse_ast.store.emptyScratch();

    // Canonicalize the source code

    var can_ir = CIR.init(&module_env);
    defer can_ir.deinit();

    var can = canonicalize.init(&can_ir, &parse_ast);
    defer can.deinit();

    var maybe_expr_idx: ?CIR.Expr.Idx = null;

    switch (content.meta.node_type) {
        .file => can.canonicalize_file(),
        .header => {
            // TODO: implement canonicalize_header when available
        },
        .expr => {
            // For expr snapshots, just canonicalize the root expression directly
            const expr_idx: AST.Expr.Idx = @enumFromInt(parse_ast.root_node_idx);
            maybe_expr_idx = can.canonicalize_expr(expr_idx);
        },
        .statement => {
            // TODO: implement canonicalize_statement when available
        },
    }

    // Types
    var solver = try Solver.init(gpa, &can_ir.env.types, &can_ir);
    defer solver.deinit();

    if (maybe_expr_idx) |expr_idx| {
        solver.checkExpr(expr_idx);
    } else {
        solver.checkDefs();
    }

    // Buffer all output in memory before writing to the snapshot file
    var buffer = std.ArrayList(u8).init(gpa);
    defer buffer.deinit();

    var writer = buffer.writer();

    // Copy original META
    {
        try writer.writeAll(Section.META);
        try content.meta.format(writer);
        try writer.writeAll("\n");
        try writer.writeAll(Section.SECTION_END);
    }

    // Copy original SOURCE
    {
        try writer.writeAll(Section.SOURCE);
        try writer.writeAll(content.source);
        try writer.writeAll("\n");
        try writer.writeAll(Section.SECTION_END);
    }

    // Write out any PROBLEMS
    {
        try writer.writeAll(Section.PROBLEMS);
        try writer.writeAll("\n");

        var tokenize_problems: usize = 0;
        var parser_problems: usize = 0;
        var canonicalize_problems: usize = 0;
        var check_types_problem: usize = 0;

        // Use plain text rendering target

        // Tokenize Diagnostics
        for (parse_ast.tokenize_diagnostics.items) |diagnostic| {
            tokenize_problems += 1;

            // TODO implement `toReport` for tokenize
            // var report: Report = try diagnostic.toReport(gpa, content.source);
            // defer report.deinit();
            // report.render(writer.any(), .plain_text) catch |err| {
            //     try writer.print("Error rendering report: {}\n", .{err});
            //     continue;
            // };

            try diagnostic.toStr(gpa, content.source, writer);
        }

        // Parser Diagnostics
        for (parse_ast.parse_diagnostics.items) |diagnostic| {
            parser_problems += 1;

            // TODO implement `diagnosticToReport` for parser
            // var report: Report = try parse_ast.diagnosticToReport(diagnostic, gpa);
            // defer report.deinit();
            // report.render(writer.any(), .plain_text) catch |err| {
            //     try writer.print("Error rendering report: {}\n", .{err});
            //     continue;
            // };

            const err_msg = try std.fmt.allocPrint(gpa, "PARSER: {s}\n", .{@tagName(diagnostic.tag)});
            defer gpa.free(err_msg);

            try writer.writeAll(err_msg);
        }

        // Canonicalization Diagnostics
        const diagnostics = can_ir.getDiagnostics();
        defer gpa.free(diagnostics);
        for (diagnostics) |diagnostic| {
            canonicalize_problems += 1;

            var report: Report = try can_ir.diagnosticToReport(diagnostic, gpa, content.source, snapshot_path);
            defer report.deinit();
            report.render(writer.any(), .plain_text) catch |err| {
                try writer.print("Error rendering report: {}\n", .{err});
                continue;
            };
        }

        // Check Types Problems

        // Create TypeWriter for converting types to strings
        var problem_buf = std.ArrayList(u8).init(gpa);
        defer problem_buf.deinit();

        var problems_itr = solver.problems.problems.iterIndices();
        while (problems_itr.next()) |problem_idx| {
            check_types_problem += 1;
            const problem = solver.problems.problems.get(problem_idx);
            var report: Report = try problem.buildReport(
                gpa,
                &problem_buf,
                &solver.snapshots,
                &module_env.idents,
            );
            defer report.deinit();
            report.render(writer.any(), .plain_text) catch |err| {
                try writer.print("Error rendering report: {}\n", .{err});
                continue;
            };
        }

        const nil_problems = tokenize_problems == 0 and parser_problems == 0 and canonicalize_problems == 0 and check_types_problem == 0;

        if (nil_problems) {
            try writer.writeAll("NIL\n");
            log("reported NIL problems", .{});
        } else {
            log("reported {} token problems", .{tokenize_problems});
            log("reported {} parser problems", .{parser_problems});
            log("reported {} canonicalization problems", .{canonicalize_problems});
            log("reported {} type problems", .{check_types_problem});
        }
        try writer.writeAll(Section.SECTION_END);
    }

    // Write out any TOKENS
    {
        try writer.writeAll(Section.TOKENS);
        var tokenizedBuffer = parse_ast.tokens;
        const tokens = tokenizedBuffer.tokens.items(.tag);
        for (tokens, 0..) |tok, i| {
            const region = tokenizedBuffer.resolve(@intCast(i));
            const info = try module_env.calcRegionInfo(content.source, region.start.offset, region.end.offset);
            const region_str = try std.fmt.allocPrint(gpa, "{s}({d}:{d}-{d}:{d}),", .{
                @tagName(tok),
                // add one to display numbers instead of index
                info.start_line_idx + 1,
                info.start_col_idx + 1,
                info.end_line_idx + 1,
                info.end_col_idx + 1,
            });
            defer gpa.free(region_str);

            try writer.writeAll(region_str);

            if (tok == .Newline) {
                try writer.writeAll("\n");
            }
        }
        try writer.writeAll("\n");
        try writer.writeAll(Section.SECTION_END);
    }

    // Write PARSE SECTION
    {
        var parse_buffer = std.ArrayList(u8).init(gpa);
        defer parse_buffer.deinit();
        switch (content.meta.node_type) {
            .file => {
                try parse_ast.toSExprStr(&module_env, parse_buffer.writer().any());
            },
            .header => {
                const header = parse_ast.store.getHeader(@enumFromInt(parse_ast.root_node_idx));
                var node = header.toSExpr(&module_env, &parse_ast);
                defer node.deinit(gpa);

                node.toStringPretty(parse_buffer.writer().any());
            },
            .expr => {
                const expr = parse_ast.store.getExpr(@enumFromInt(parse_ast.root_node_idx));
                var node = expr.toSExpr(&module_env, &parse_ast);
                defer node.deinit(gpa);

                node.toStringPretty(parse_buffer.writer().any());
            },
            .statement => {
                const stmt = parse_ast.store.getStatement(@enumFromInt(parse_ast.root_node_idx));
                var node = stmt.toSExpr(&module_env, &parse_ast);
                defer node.deinit(gpa);

                node.toStringPretty(parse_buffer.writer().any());
            },
        }
        try writer.writeAll(Section.PARSE);
        try writer.writeAll(parse_buffer.items);
        try writer.writeAll("\n");
        try writer.writeAll(Section.SECTION_END);
    }

    // Write FORMAT SECTION
    {
        var formatted = std.ArrayList(u8).init(gpa);
        defer formatted.deinit();
        switch (content.meta.node_type) {
            .file => {
                try fmt.formatAst(parse_ast, formatted.writer().any());
            },
            .header => {
                try fmt.formatHeader(parse_ast, formatted.writer().any());
            },
            .expr => {
                try fmt.formatExpr(parse_ast, formatted.writer().any());
            },
            .statement => {
                try fmt.formatStatement(parse_ast, formatted.writer().any());
            },
        }

        try writer.writeAll(Section.FORMATTED);

        if (!std.mem.eql(u8, formatted.items, content.source)) {
            try writer.writeAll(formatted.items);
            try writer.writeAll("\n");
        } else {
            try writer.writeAll("NO CHANGE");
            try writer.writeAll("\n");
        }
        try writer.writeAll(Section.SECTION_END);
    }

    // Write CANONICALIZE SECTION
    {
        var canonicalized = std.ArrayList(u8).init(gpa);
        defer canonicalized.deinit();

        try can_ir.toSExprStr(&module_env, canonicalized.writer().any(), maybe_expr_idx, content.source);

        try writer.writeAll(Section.CANONICALIZE);
        try writer.writeAll(canonicalized.items);
        try writer.writeAll("\n");
        try writer.writeAll(Section.SECTION_END);
    }

    // Write TYPES SECTION
    {
        var solved = std.ArrayList(u8).init(gpa);
        defer solved.deinit();

        try can_ir.toSexprTypesStr(solved.writer().any(), maybe_expr_idx, content.source);

        // Uncomment to print entire types store, helpful for debugging
        // try solved.writer().any().writeAll("\n");
        // try types.SExprWriter.allVarsToSExprStr(solved.writer().any(), gpa, &module_env);

        try writer.writeAll(Section.TYPES);
        try writer.writeAll("\n");
        try writer.writeAll(solved.items);
        try writer.writeAll("\n");
    }

    try writer.writeAll(Section.SECTION_END[0 .. Section.SECTION_END.len - 1]);

    // Now write the buffer to the snapshot file in one go
    var file = std.fs.cwd().createFile(snapshot_path, .{}) catch |err| {
        log("failed to create file '{s}': {s}", .{ snapshot_path, @errorName(err) });
        return false;
    };
    defer file.close();
    try file.writer().writeAll(buffer.items);

    // If flag --fuzz-corpus is passed, so write the SOURCE to our corpus
    if (maybe_fuzz_corpus_path != null) {

        // create a pseudo-random name for our file
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

/// Extracts the sections from a snapshot file
fn extractSections(gpa: Allocator, content: []const u8) !Content {
    var ranges = std.AutoHashMap(Section, Section.Range).init(gpa);
    defer ranges.deinit();

    var processed_chars: usize = 0;

    var sections_with_header = std.mem.splitSequence(u8, content, Section.SECTION_END);

    while (sections_with_header.next()) |section_with_header| {
        const trimmed_section = std.mem.trimLeft(u8, section_with_header, "\n\r \t");
        if (Section.fromString(trimmed_section)) |section| {
            const start = processed_chars + section.asString().len;
            const end = processed_chars + section_with_header.len;
            try ranges.put(section, .{ .start = start, .end = end });

            processed_chars += section_with_header.len + Section.SECTION_END.len;
        } else if (std.mem.trim(u8, section_with_header, "\n\r \t").len != 0) {
            return Error.BadSectionHeader;
        }
    }

    return try Content.from_ranges(ranges, content);
}

test "extractSections" {
    const input =
        \\# META
        \\~~~ini
        \\description=
        \\type=file
        \\~~~
        \\# SOURCE
        \\~~~roc
        \\source code line
        \\~~~
        \\# FORMATTED
        \\~~~roc
        \\formatted output line
        \\~~~
        \\# PARSE
        \\~~~clojure
        \\parse section line
        \\~~~
    ;

    const content = try extractSections(testing.allocator, input);

    var meta_buf: std.ArrayListUnmanaged(u8) = .{};
    defer meta_buf.deinit(std.testing.allocator);
    try content.meta.format(meta_buf.writer(std.testing.allocator));
    try testing.expectEqualStrings(
        \\description=
        \\type=file
    , meta_buf.items[0..]);
    try testing.expectEqualStrings("source code line", content.source);
    try testing.expectEqualStrings("formatted output line", content.formatted.?);
}

test "extractSections complex" {
    const input =
        \\# META
        \\~~~ini
        \\description=Basic example to develop the snapshot methodology
        \\~~~
        \\# SOURCE
        \\~~~roc
        \\module [foo, bar]
        \\
        \\foo = "one"
        \\
        \\bar = "two"
        \\~~~
        \\# PARSE
        \\~~~clojure
        \\(file
        \\    (header
        \\        'foo'
        \\        'bar')
        \\    (decl
        \\        (ident
        \\            'foo')
        \\        (string_part))
        \\    (decl
        \\        (ident
        \\            'bar')
        \\        (string_part)))
        \\~~~
    ;

    const expected_meta =
        \\description=Basic example to develop the snapshot methodology
        \\type=file
    ;

    const expected_source =
        \\module [foo, bar]
        \\
        \\foo = "one"
        \\
        \\bar = "two"
    ;

    const expected_formatted = null;
    const content = try extractSections(testing.allocator, input);

    var meta_buf: std.ArrayListUnmanaged(u8) = .{};
    defer meta_buf.deinit(std.testing.allocator);
    try content.meta.format(meta_buf.writer(std.testing.allocator));
    try testing.expectEqualStrings(expected_meta, meta_buf.items[0..]);
    try testing.expectEqualStrings(expected_source, content.source);
    try testing.expectEqual(expected_formatted, content.formatted);
}
