const std = @import("std");
const base = @import("base.zig");
const parse = @import("check/parse.zig");
const fmt = @import("fmt.zig");

var verbose_log: bool = false;

fn log(comptime fmt_str: []const u8, args: anytype) void {
    if (verbose_log) {
        std.log.info(fmt_str, args);
    }
}

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

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--verbose")) {
            verbose_log = true;
        } else {
            try snapshot_paths.append(arg);
        }
    }

    const snapshots_dir = "src/snapshots";
    var file_count: usize = 0;
    var timer = std.time.Timer.start() catch unreachable;

    if (snapshot_paths.items.len > 0) {
        for (snapshot_paths.items) |path| {
            file_count += try processPath(path, gpa);
        }
    } else {
        // process all files in snapshots_dir
        file_count = try processPath(snapshots_dir, gpa);
    }

    const duration_ms = timer.read() / std.time.ns_per_ms;

    std.log.info("processed {d} snapshots in {d}ms.", .{ file_count, duration_ms });
}

fn processPath(path: []const u8, gpa: std.mem.Allocator) !usize {
    var processed_count: usize = 0;

    const canonical_path = try std.fs.cwd().realpathAlloc(gpa, path);
    defer gpa.free(canonical_path);

    const stat = try std.fs.cwd().statFile(canonical_path);

    if (stat.kind == .directory) {
        var dir = try std.fs.cwd().openDir(path, .{ .iterate = true });
        defer dir.close();

        var dir_iterator = dir.iterate();
        while (try dir_iterator.next()) |entry| {
            const full_path = try std.fmt.allocPrint(gpa, "{s}/{s}", .{ path, entry.name });
            defer gpa.free(full_path);

            if (entry.kind == .directory) {
                processed_count += try processPath(full_path, gpa);
            } else if (entry.kind == .file) {
                processed_count += try processPath(full_path, gpa);
            }
        }
    } else if (stat.kind == .file) {
        if (std.mem.endsWith(u8, path, ".txt")) {
            processSnapshotFile(path, gpa) catch |err| {
                switch (err) {
                    Error.MissingSnapshotHeader, Error.MissingSnapshotSource => {
                        // Ignore non-snapshot files
                        log("ignoring non-snapshot file {s}", .{path});
                    },
                    else => return err,
                }
            };
            processed_count += 1;
        }
    }

    return processed_count;
}

/// Represents the different sections of a snapshot file.
const Section = enum {
    None,
    Meta,
    Source,
    Formatted,
    Parse,

    const META = "~~~META";
    const SOURCE = "~~~SOURCE";
    const FORMATTED = "~~~FORMATTED";
    const PARSE = "~~~PARSE";
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
};

const Error = error{
    MissingSnapshotHeader,
    MissingSnapshotSource,
};

fn processSnapshotFile(snapshot_path: []const u8, gpa: std.mem.Allocator) !void {
    const file_content = try std.fs.cwd().readFileAlloc(gpa, snapshot_path, 1024 * 1024);
    defer gpa.free(file_content);

    // Check our file starts with the metadata section
    // so we can skip parsing and later steps if this isn't a snapshot file
    if (!std.mem.startsWith(u8, file_content, Section.META)) {
        return Error.MissingSnapshotHeader;
    }

    // Parse the file to find section boundaries
    const content = try extractSections(file_content);

    // Generate the PARSE section
    var parse_buffer = std.ArrayList(u8).init(gpa);
    defer parse_buffer.deinit();

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    // Parse the source code
    var parse_ast = parse.parse(&module_env, gpa, content.source);
    defer parse_ast.deinit();

    // Format the source code
    var formatter = fmt.init(parse_ast, gpa);
    defer formatter.deinit();
    const formatted_output = formatter.formatFile();
    defer gpa.free(formatted_output);

    // shouldn't be required in future
    parse_ast.store.emptyScratch();

    // Write the new AST to the parse section
    try parse_ast.toSExprStr(gpa, &module_env, parse_buffer.writer().any());

    // Rewrite the file with updated sections
    var file = try std.fs.cwd().createFile(snapshot_path, .{});
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

    log("{s}", .{snapshot_path});
}

fn extractSections(content: []const u8) !Content {
    var meta_start: ?usize = null;
    var meta_end: ?usize = null;
    var source_start: ?usize = null;
    var source_end: ?usize = null;
    var formatted_start: ?usize = null;
    var formatted_end: ?usize = null;

    var lines = std.mem.tokenizeScalar(u8, content, '\n');
    var current_pos: usize = 0;

    while (lines.next()) |line| {
        const line_with_newline = line.len + 1;

        if (std.mem.eql(u8, line, Section.META)) {
            meta_start = current_pos + line_with_newline;
        } else if (std.mem.eql(u8, line, Section.SOURCE)) {
            if (meta_start != null) meta_end = current_pos;
            source_start = current_pos + line_with_newline;
        } else if (std.mem.eql(u8, line, Section.FORMATTED)) {
            if (source_start != null) source_end = current_pos;
            formatted_start = current_pos + line_with_newline;
        } else if (std.mem.eql(u8, line, Section.PARSE)) {
            if (formatted_start != null) {
                formatted_end = current_pos;
            } else if (source_start != null) {
                source_end = current_pos;
            }
            break;
        }

        current_pos += line_with_newline;
    }

    const missing_header = (meta_start == null) or (meta_end == null);
    const missing_source = (source_start == null) or (source_end == null);
    if (missing_header) {
        return Error.MissingSnapshotHeader;
    } else if (missing_source) {
        return Error.MissingSnapshotSource;
    }

    var formatted: ?[]const u8 = null;
    if (formatted_start != null and formatted_end != null) {
        formatted = content[formatted_start.?..formatted_end.?];
    }

    return Content.init(content[meta_start.?..meta_end.?], content[source_start.?..source_end.?], formatted);
}
