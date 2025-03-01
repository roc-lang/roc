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

    // Check for verbose flag
    var target_snapshot: ?[]const u8 = null;

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "--verbose")) {
            verbose_log = true;
        } else if (target_snapshot == null) {
            target_snapshot = arg;
        }
    }

    const snapshots_dir = "src/snapshots";
    var file_count: usize = 0;
    var timer = std.time.Timer.start() catch unreachable;

    if (target_snapshot != null) {
        // Only for the specified snapshot file
        try processSnapshotFile(target_snapshot.?, gpa);
        file_count += 1;
    } else {
        // For each of the files in the directory
        var dir = try std.fs.cwd().openDir(snapshots_dir, .{ .iterate = true });
        defer dir.close();

        var dir_iterator = dir.iterate();
        while (try dir_iterator.next()) |entry| {
            if (entry.kind != .file) continue;
            if (!std.mem.endsWith(u8, entry.name, ".txt")) continue;

            const full_path = try std.fmt.allocPrint(gpa, "{s}/{s}", .{ snapshots_dir, entry.name });
            defer gpa.free(full_path);

            try processSnapshotFile(full_path, gpa);

            file_count += 1;
        }
    }

    const duration_ms = timer.read() / std.time.ns_per_ms;

    // Print a summary
    std.log.info("processed {d} snapshots in {d}ms.", .{ file_count, duration_ms });
}

/// Represents the different sections of a snapshot file.
const Section = enum {
    None,
    Meta,
    Source,
    Parse,
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

fn processSnapshotFile(snapshot_path: []const u8, allocator: std.mem.Allocator) !void {
    var contents = SnapshotContents.init(allocator);
    defer contents.deinit();

    // Parse the file contents
    {
        // Read the file
        const file_content = try std.fs.cwd().readFileAlloc(allocator, snapshot_path, 1024 * 1024);
        defer allocator.free(file_content);

        var current_section = Section.None;
        var lines = std.mem.tokenizeScalar(u8, file_content, '\n');
        while (lines.next()) |line| {
            if (std.mem.eql(u8, line, "~~~META")) {
                current_section = Section.Meta;
                continue;
            } else if (std.mem.eql(u8, line, "~~~SOURCE")) {
                current_section = Section.Source;
                continue;
            } else if (std.mem.eql(u8, line, "~~~PARSE")) {
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

    try file.writer().writeAll("~~~META\n");
    try file.writer().writeAll(contents.meta.items);
    try file.writer().writeAll("~~~SOURCE\n");
    try file.writer().writeAll(contents.source.items);
    try file.writer().writeAll("~~~PARSE\n");
    try file.writer().writeAll(contents.parse.items);

    log("{s}", .{snapshot_path});
}
