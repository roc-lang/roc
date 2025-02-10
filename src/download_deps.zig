const std = @import("std");
const builtin = @import("builtin");

const USAGE =
    \\Usage: ./download_deps TARGET OUTPUT_PATH
    \\
    \\ Downloads roc deps (like llvm) for the specified TARGET to OUTPUT_PATH/TARGET
    \\
;

const BASE_URL =
    "https://github.com/roc-lang/roc-bootstrap/releases/download";

pub fn main() !void {
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        _ = gpa_impl.deinit();
    }
    const gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const args = try std.process.argsAlloc(arena);

    const stderr = std.io.getStdErr().writer();
    if (args.len < 3) {
        try stderr.writeAll(USAGE);
        std.process.exit(1);
    }

    const target = args[1];
    const out_path = args[2];

    // If the target path `version.txt` exists and matches, cache is all good.
    const target_path = try std.fs.path.join(arena, &.{ out_path, target });
    const version_path = try std.fs.path.join(arena, &.{ target_path, "version.txt" });

    if (std.fs.cwd().readFileAlloc(arena, version_path, 32)) |bytes| {
        if (std.mem.eql(u8, std.mem.trim(u8, bytes, &std.ascii.whitespace), builtin.zig_version_string)) {
            // Cache hit. All good to go.
            std.process.exit(0);
        } else {
            // Wrong version in cache.
            try stderr.print("Found incorrect version of deps (got: {s}, want: {s}).\n", .{ bytes, builtin.zig_version_string });
            try stderr.print("To re-download, delete: {s}\n", .{target_path});
            std.process.exit(1);
        }
    } else |_| {}

    var it = std.mem.splitScalar(u8, target, '-');
    _ = it.next();
    const os = it.next();
    if (os == null) {
        try stderr.print("Failed to parse OS from target: {s}\n", .{target});
        std.process.exit(1);
    }
    const is_windows = std.mem.eql(u8, os.?, "windows");
    const ext = if (is_windows) "zip" else "tar.xz";

    const url = try std.fmt.allocPrint(arena, "{s}/zig-{s}/{s}.{s}", .{ BASE_URL, builtin.zig_version_string, target, ext });

    var compressed_bytes = try std.ArrayList(u8).initCapacity(gpa, 100 * 1024 * 1024);
    defer compressed_bytes.deinit();
    var http = std.http.Client{ .allocator = gpa };
    const result = try http.fetch(.{ .response_storage = .{ .dynamic = &compressed_bytes }, .location = .{ .url = url }, .method = .GET });
    if (result.status != .ok) {
        try stderr.print("Failed to download roc dependencies from: {s}\n", .{url});
        try stderr.print("Bad http response: {}\n", .{result.status});
        std.process.exit(1);
    }

    var stream = std.io.fixedBufferStream(compressed_bytes.items);
    var out_dir = try std.fs.cwd().makeOpenPath(out_path, .{});
    defer out_dir.close();
    if (is_windows) {
        try std.zip.extract(out_dir, stream.seekableStream(), .{});
        std.process.exit(1);
    } else {
        var decompressed = try std.compress.xz.decompress(gpa, stream.reader());
        defer decompressed.deinit();
        try std.tar.pipeToFileSystem(out_dir, decompressed.reader(), .{});
    }
}
