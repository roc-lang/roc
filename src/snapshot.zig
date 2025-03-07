const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;
const base = @import("base.zig");
const parse = @import("check/parse.zig");
const fmt = @import("fmt.zig");

var verbose_log: bool = false;
var prng = std.rand.DefaultPrng.init(1234567890);

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
            std.log.info("{s}", .{usage});
            std.process.exit(0);
        } else {
            try snapshot_paths.append(arg);
        }
    }

    if (maybe_fuzz_corpus_path != null) {
        log("copying SOURCE from snapshots to: {s}", .{maybe_fuzz_corpus_path.?});
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
            } else if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".txt")) {
                if (try processSnapshotFile(gpa, full_path, maybe_fuzz_corpus_path)) {
                    processed_count += 1;
                }
            }
        }
    } else if (stat.kind == .file) {
        if (std.mem.endsWith(u8, canonical_path, ".txt")) {
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

    pub const META = "~~~META";
    pub const SOURCE = "~~~SOURCE";
    pub const FORMATTED = "~~~FORMATTED";
    pub const PARSE = "~~~PARSE";

    fn next(self: Section) ?Section {
        return switch (self) {
            .meta => .source,
            .source => .formatted,
            .formatted => .parse,
            .parse => null,
        };
    }

    fn fromString(str: []const u8) ?Section {
        if (std.mem.eql(u8, str, META)) return .meta;
        if (std.mem.eql(u8, str, SOURCE)) return .source;
        if (std.mem.eql(u8, str, FORMATTED)) return .formatted;
        if (std.mem.eql(u8, str, PARSE)) return .parse;
        return null;
    }

    fn asString(self: Section) []const u8 {
        return switch (self) {
            .meta => META,
            .source => SOURCE,
            .formatted => FORMATTED,
            .parse => PARSE,
            .None => "",
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

/// Content of a snapshot file, references the Metadata and Source sections etc
const Content = struct {
    meta: []const u8,
    source: []const u8,
    formatted: ?[]const u8,

    fn init(meta: []const u8, source: []const u8, formatted: ?[]const u8) Content {
        return .{
            .meta = meta,
            .source = source,
            .formatted = formatted,
        };
    }

    fn has_formatted_section(self: Content) bool {
        return self.formatted != null;
    }

    fn from_ranges(ranges: std.AutoHashMap(Section, Section.Range), content: []const u8) Error!Content {
        var meta: []const u8 = undefined;
        var source: []const u8 = undefined;
        var formatted: ?[]const u8 = undefined;

        if (ranges.get(.meta)) |value| {
            meta = value.extract(content);
        } else {
            return Error.MissingSnapshotHeader;
        }

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

        return Content.init(
            meta,
            source,
            formatted,
        );
    }
    pub fn format(self: Content, comptime fmt_str: []const u8, _: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
        _ = fmt_str;
        try writer.print("SNAPSHOT CONTENTS\n", .{});
        try writer.print("META:\n{s}\n---\n", .{self.meta});
        try writer.print("SOURCE:\n{s}\n---\n", .{self.source});
        if (self.formatted) |formatted| {
            try writer.print("FORMATTED:\n{s}\n---\n", .{formatted});
        } else {
            try writer.print("FORMATTED: null\n", .{});
        }
    }
};

const Error = error{
    MissingSnapshotHeader,
    MissingSnapshotSource,
};

fn processSnapshotFile(gpa: Allocator, snapshot_path: []const u8, maybe_fuzz_corpus_path: ?[]const u8) !bool {
    const file_content = std.fs.cwd().readFileAlloc(gpa, snapshot_path, 1024 * 1024) catch |err| {
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
            Error.MissingSnapshotHeader, Error.MissingSnapshotSource => {
                warn("ignoring file {s}: {s}", .{ snapshot_path, @errorName(err) });
                return false;
            },
            else => return err,
        }
    };

    // std.debug.print("FILE: {s}\n{}\n", .{ snapshot_path, content });

    // Generate the PARSE section
    var parse_buffer = std.ArrayList(u8).init(gpa);
    defer parse_buffer.deinit();

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Parse the source code
    var parse_ast = parse.parse(&module_env, content.source);
    defer parse_ast.deinit();

    // Format the source code
    var formatter = fmt.init(parse_ast);
    defer formatter.deinit();
    const formatted_output = formatter.formatFile();
    defer gpa.free(formatted_output);

    // shouldn't be required in future
    parse_ast.store.emptyScratch();

    // Write the new AST to the parse section
    try parse_ast.toSExprStr(&module_env, parse_buffer.writer().any());

    // Rewrite the file with updated sections
    var file = std.fs.cwd().createFile(snapshot_path, .{}) catch |err| {
        log("failed to create file '{s}': {s}", .{ snapshot_path, @errorName(err) });
        return false;
    };
    defer file.close();

    try file.writer().writeAll(Section.META);
    try file.writer().writeAll("\n");
    try file.writer().writeAll(content.meta);
    try file.writer().writeAll("\n");

    // If there's an explicit FORMATTED section, keep the source as-is
    // and update the FORMATTED section
    if (content.has_formatted_section()) {
        try file.writer().writeAll(Section.SOURCE);
        try file.writer().writeAll("\n");
        try file.writer().writeAll(content.source);
        try file.writer().writeAll("\n");
        try file.writer().writeAll(Section.FORMATTED);
        try file.writer().writeAll("\n");
        try file.writer().writeAll(formatted_output);
        try file.writer().writeAll("\n");
    } else {
        // Otherwise, update SOURCE directly with the formatted output
        try file.writer().writeAll(Section.SOURCE);
        try file.writer().writeAll("\n");
        try file.writer().writeAll(formatted_output);
        try file.writer().writeAll("\n");
    }

    try file.writer().writeAll(Section.PARSE);
    try file.writer().writeAll("\n");
    try file.writer().writeAll(parse_buffer.items);

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

    log("{s}", .{snapshot_path});

    return true;
}

/// Extracts the sections from a snapshot file
fn extractSections(gpa: Allocator, content: []const u8) !Content {
    var ranges = std.AutoHashMap(Section, Section.Range).init(gpa);
    defer ranges.deinit();

    var current_section: ?Section = null;
    var line_start: usize = 0;

    var lines = std.mem.splitScalar(u8, content, '\n');

    while (true) {
        const line_opt = lines.next();
        if (line_opt == null) break;

        const line = line_opt.?;
        const line_end = line_start + line.len;
        const next_line_start = line_end + 1; // +1 for the newline

        if (Section.fromString(line)) |new_section| {
            // If we were in a section, finalize its range
            if (current_section) |section| {
                var range = ranges.get(section) orelse Section.Range.empty();
                range.end = line_start;
                try ranges.put(section, range);
            }

            // Start the new section
            current_section = new_section;

            // Initialize new section range (starting after this line)
            var range = Section.Range.empty();
            range.start = next_line_start;
            try ranges.put(new_section, range);
        }

        line_start = next_line_start;
    }

    // Handle the last section if there is one
    if (current_section) |section| {
        var range = ranges.get(section) orelse Section.Range.empty();
        range.end = @min(line_start, content.len);
        try ranges.put(section, range);
    }

    return try Content.from_ranges(ranges, content);
}

test "extractSections" {
    const input =
        \\~~~META
        \\meta data line
        \\~~~SOURCE
        \\source code line
        \\~~~FORMATTED
        \\formatted output line
        \\~~~PARSE
        \\parse section line
    ;

    const content = try extractSections(testing.allocator, input);

    try testing.expectEqualStrings("meta data line", content.meta);
    try testing.expectEqualStrings("source code line", content.source);
    try testing.expectEqualStrings("formatted output line", content.formatted.?);
}

test "extractSections complex" {
    const input =
        \\~~~META
        \\description=Basic example to develop the snapshot methodology
        \\~~~SOURCE
        \\module [foo, bar]
        \\
        \\foo = "one"
        \\
        \\bar = "two"
        \\~~~PARSE
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
    ;

    const expected_meta =
        \\description=Basic example to develop the snapshot methodology
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

    try testing.expectEqualStrings(expected_meta, content.meta);
    try testing.expectEqualStrings(expected_source, content.source);
    try testing.expectEqual(expected_formatted, content.formatted);
}
