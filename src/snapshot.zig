const std = @import("std");
const base = @import("base.zig");
const parse = @import("check/parse.zig");

var verbose_log: bool = false;

fn log(comptime fmt: []const u8, args: anytype) void {
    if (verbose_log) {
        std.log.info(fmt, args);
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

fn processPath(path: []const u8, allocator: std.mem.Allocator) !usize {
    var processed_count: usize = 0;

    const canonical_path = try std.fs.cwd().realpathAlloc(allocator, path);
    defer allocator.free(canonical_path);

    const stat = try std.fs.cwd().statFile(canonical_path);

    if (stat.kind == .directory) {
        var dir = try std.fs.cwd().openDir(path, .{ .iterate = true });
        defer dir.close();

        var dir_iterator = dir.iterate();
        while (try dir_iterator.next()) |entry| {
            const full_path = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ path, entry.name });
            defer allocator.free(full_path);

            if (entry.kind == .directory) {
                processed_count += try processPath(full_path, allocator);
            } else if (entry.kind == .file) {
                if (std.mem.endsWith(u8, entry.name, ".txt")) {
                    processSnapshotFile(full_path, allocator) catch |err| {
                        if (err == SnapshotError.MissingSnapshotHeader) {
                            // ignore files non-snapshot files
                        } else {
                            return err;
                        }
                    };
                    processed_count += 1;
                } else {
                    // ignore files that do not end with ".txt"
                }
            }
        }
    } else if (stat.kind == .file) {
        if (std.mem.endsWith(u8, path, ".txt")) {
            try processSnapshotFile(path, allocator);
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
    Parse,

    const META_HEADER = "~~~META";
    const SOURCE_HEADER = "~~~SOURCE";
    const PARSE_HEADER = "~~~PARSE";
};

/// Stores the contents for a snapshot to be written to file
const SnapshotContents = struct {
    meta: std.ArrayList(u8),
    source: std.ArrayList(u8),
    parse: std.ArrayList(u8),

    fn init(allocator: std.mem.Allocator) SnapshotContents {
        return .{
            .meta = std.ArrayList(u8).init(allocator),
            .source = std.ArrayList(u8).init(allocator),
            .parse = std.ArrayList(u8).init(allocator),
        };
    }

    fn deinit(self: *SnapshotContents) void {
        self.meta.deinit();
        self.source.deinit();
        self.parse.deinit();
    }
};

const SnapshotError = error{
    MissingSnapshotHeader,
};

fn processSnapshotFile(snapshot_path: []const u8, allocator: std.mem.Allocator) !void {
    var contents = SnapshotContents.init(allocator);
    defer contents.deinit();

    // Parse the file contents
    {
        // Read the file
        const file_content = try std.fs.cwd().readFileAlloc(allocator, snapshot_path, 1024 * 1024);
        defer allocator.free(file_content);

        if (!std.mem.startsWith(u8, file_content, Section.META_HEADER)) {
            return SnapshotError.MissingSnapshotHeader;
        }

        var current_section = Section.None;
        var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
        while (lines.next()) |line| {
            if (std.mem.eql(u8, line, Section.META_HEADER)) {
                current_section = Section.Meta;
                continue;
            } else if (std.mem.eql(u8, line, Section.SOURCE_HEADER)) {
                current_section = Section.Source;
                continue;
            } else if (std.mem.eql(u8, line, Section.PARSE_HEADER)) {
                current_section = Section.Parse;
                continue;
            }

            switch (current_section) {
                .Meta => try contents.meta.writer().print("{s}\n", .{line}),
                .Source => try contents.source.writer().print("{s}\n", .{line}),
                .Parse => {}, // We'll regenerate this section, so we don't need to save it
                .None => {},
            }
        }
    }

    // Generate the PARSE section
    {
        var env = base.ModuleEnv.init(allocator);
        defer env.deinit();

        var parse_ast = parse.parse(&env, allocator, contents.source.items);
        defer parse_ast.deinit();

        // shouldn't be required in future
        parse_ast.store.emptyScratch();

        // Clear the parse contents to ensure we start fresh
        contents.parse.clearRetainingCapacity();

        // Write the new AST to the parse section
        try parse_ast.toSExprStr(allocator, &env, contents.parse.writer().any());
    }

    // Rewrite the file with updated sections
    var file = try std.fs.cwd().createFile(snapshot_path, .{});
    defer file.close();

    try file.writer().writeAll(Section.META_HEADER);
    try file.writer().writeAll("\n");
    try file.writer().writeAll(contents.meta.items);
    try file.writer().writeAll(Section.SOURCE_HEADER);
    try file.writer().writeAll("\n");
    try file.writer().writeAll(contents.source.items);
    try file.writer().writeAll(Section.PARSE_HEADER);
    try file.writer().writeAll("\n");
    try file.writer().writeAll(contents.parse.items);

    log("{s}", .{snapshot_path});
}
