//! Build-time check that a release binary names no directory of the machine
//! that built it.
//!
//! A release must come out byte for byte the same from any checkout of the
//! same commit, so anyone can rebuild a nightly and compare it to the one that
//! was published. A path baked in at build time breaks that: the checkout, the
//! Zig caches and the Zig installation all live somewhere else on every
//! machine. This tool fails when the binary contains any of them, and prints
//! where.
//!
//! `build.zig` cannot name the caches itself, since they are only chosen once
//! the build runs, so they are found here: the local cache is the one the
//! binary was built into (`<cache>/o/<hash>/<binary>`), `zig env` reports the
//! global cache and the Zig library, and `ZIG_LOCAL_PKG_DIR` moves the package
//! directory out of the checkout.
//!
//! Usage: build_path_check <stamp> <binary> <checkout> <zig>

const std = @import("std");

const MainError = std.process.Args.ToSliceError || std.Io.Dir.ReadFileAllocError ||
    std.Io.Dir.RealPathFileAllocError || std.Io.Dir.WriteFileError || std.process.RunError || error{
    ExpectedStampBinaryCheckoutAndZig,
    ZigEnvFailed,
    ParseZon,
    BuildPathInBinary,
};

/// Paths this short also turn up in unrelated bytes, so they are not searched
/// for. No real checkout or cache directory is this short.
const min_path_len = 8;

/// Shown per path, so one leak repeated across many embedded objects does not
/// bury the others.
const max_reported_hits = 5;

/// The part of `zig env`'s output this tool reads.
const ZigEnv = struct {
    lib_dir: []const u8,
    global_cache_dir: []const u8,
};

/// Check `args[2]` for every build-machine directory, writing `args[1]` once it
/// passes so the build caches the result.
pub fn main(init: std.process.Init) MainError!void {
    const gpa = init.gpa;
    const arena = init.arena.allocator();
    const io = init.io;
    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 5) return error.ExpectedStampBinaryCheckoutAndZig;
    const binary_path = args[2];

    var paths: std.ArrayList([]const u8) = .empty;
    try paths.append(arena, try std.Io.Dir.cwd().realPathFileAlloc(io, args[3], arena));
    const binary_real = try std.Io.Dir.cwd().realPathFileAlloc(io, binary_path, arena);
    if (localCacheOf(binary_real)) |cache| try paths.append(arena, cache);
    // `zig env` prints directories under the cwd relative to it.
    const env = try zigEnv(gpa, arena, io, args[4]);
    try paths.append(arena, try std.Io.Dir.cwd().realPathFileAlloc(io, env.global_cache_dir, arena));
    try paths.append(arena, try std.Io.Dir.cwd().realPathFileAlloc(io, env.lib_dir, arena));
    if (init.environ_map.get("ZIG_LOCAL_PKG_DIR")) |dir| {
        try paths.append(arena, try std.Io.Dir.cwd().realPathFileAlloc(io, dir, arena));
    }

    const bytes = try std.Io.Dir.cwd().readFileAlloc(io, binary_path, gpa, .unlimited);
    defer gpa.free(bytes);

    var leaked = false;
    for (paths.items) |path| {
        if (path.len < min_path_len) continue;
        var hits: usize = 0;
        var offset: usize = 0;
        while (std.mem.findPos(u8, bytes, offset, path)) |at| : (offset = at + path.len) {
            if (hits < max_reported_hits) {
                std.debug.print("{s}: build path at offset {d}: {s}\n", .{ binary_path, at, printableAround(bytes, at) });
            }
            hits += 1;
        }
        if (hits > 0) {
            std.debug.print("{s}: {d} occurrence(s) of {s}\n", .{ binary_path, hits, path });
            leaked = true;
        }
    }
    if (leaked) {
        std.debug.print(
            \\A release binary must not contain paths from the machine that built it,
            \\or no one else can rebuild it byte for byte. Embed the file's contents
            \\instead of its path, and strip directories from archive member names
            \\(src/build/archive_member_names.zig).
            \\
        , .{});
        return error.BuildPathInBinary;
    }
    try std.Io.Dir.cwd().writeFile(io, .{ .sub_path = args[1], .data = "" });
}

/// The Zig cache a build artifact at `path` (`<cache>/o/<hash>/<file>`) was
/// written to, or null when it is not in one.
fn localCacheOf(path: []const u8) ?[]const u8 {
    const artifact_dir = std.fs.path.dirname(path) orelse return null;
    const objects_dir = std.fs.path.dirname(artifact_dir) orelse return null;
    if (!std.mem.eql(u8, std.fs.path.basename(objects_dir), "o")) return null;
    return std.fs.path.dirname(objects_dir);
}

fn zigEnv(gpa: std.mem.Allocator, arena: std.mem.Allocator, io: std.Io, zig_exe: []const u8) MainError!ZigEnv {
    const result = try std.process.run(gpa, io, .{ .argv = &.{ zig_exe, "env" } });
    defer gpa.free(result.stderr);
    defer gpa.free(result.stdout);
    if (!result.term.success()) return error.ZigEnvFailed;
    var diagnostics: std.zon.parse.Diagnostics = undefined;
    return std.zon.parse.fromSlice(ZigEnv, .{
        .gpa = gpa,
        .arena = arena,
        .source = try arena.dupeSentinel(u8, result.stdout, 0),
        .diagnostics = &diagnostics,
        .ignore_unknown_fields = true,
    }) catch |err| switch (err) {
        error.ParseZon => {
            diagnostics.log("zig env");
            return err;
        },
        error.OutOfMemory => return err,
    };
}

/// The printable run of bytes around `at`, which for a leaked path is usually
/// the whole path it was part of.
fn printableAround(bytes: []const u8, at: usize) []const u8 {
    var start = at;
    while (start > 0 and std.ascii.isPrint(bytes[start - 1])) start -= 1;
    var end = at;
    while (end < bytes.len and std.ascii.isPrint(bytes[end])) end += 1;
    return bytes[start..end];
}

test "an artifact's local cache is the directory above its o/<hash> directory" {
    try std.testing.expectEqualStrings("/w/roc/.zig-cache", localCacheOf("/w/roc/.zig-cache/o/0123abcd/roc").?);
    try std.testing.expectEqual(@as(?[]const u8, null), localCacheOf("/w/roc/zig-out/bin/roc"));
}

test "the printable run around a hit spans the whole embedded path" {
    const bytes = "\x00\x01/home/runner/work/roc/.zig-cache/o/abc/compiler_rt.obj\x00\xff";
    const at = std.mem.find(u8, bytes, "/home/runner").?;
    try std.testing.expectEqualStrings("/home/runner/work/roc/.zig-cache/o/abc/compiler_rt.obj", printableAround(bytes, at));
}

test "a hit at either end of the binary stays in bounds" {
    try std.testing.expectEqualStrings("/home/runner", printableAround("/home/runner", 0));
    try std.testing.expectEqualStrings("/home/runner", printableAround("\x00/home/runner", 1));
}
