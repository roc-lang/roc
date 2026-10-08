//! Re-stamp a Zig cache that was moved to this machine, so the build finds its
//! entries again.
//!
//! CI builds once per OS and hands `.zig-cache` to the test shards as an
//! artifact. A cache manifest records, for every input file, its size, inode,
//! mtime and content digest. In a shard every file is new: the sources come
//! from a fresh checkout and the cache from an archive, so no recorded inode or
//! mtime matches.
//!
//! Zig is meant to fall back to the content digest in that case. Zig 0.17.0
//! does so for the files a compilation discovers, but not for the inputs a
//! step declares up front: `std.Build.Cache.Manifest.checkInputPath` hashes
//! the file and then compares the result with a digest the caller never
//! supplied. Every C source file and every linked object or library is such
//! an input, so each shard recompiled every binary it ran.
//!
//! This tool does the comparison Zig skips. For each manifest entry whose
//! file still has the recorded content digest, it rewrites the recorded size,
//! inode and mtime to the file's current ones. An entry whose contents differ
//! is left alone, so a changed input is still a cache miss.
//!
//! The canary test at the bottom fails once Zig hashes re-stat'd inputs
//! itself. Delete this tool and its CI step when it does.

const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const Cache = std.Build.Cache;
const ManifestFile = Cache.Manifest.File;

/// How many directories Zig records manifest paths against. Both the compiler
/// and the build runner register them with `Cache.addPrefix` in this order:
/// the working directory, the Zig lib directory, the local cache, the global
/// cache and the build root.
const prefix_count = 5;

const usage =
    \\Usage: restamp-zig-cache [--zig <path>]
    \\
    \\Run from the build root after restoring `.zig-cache` from another machine.
    \\
    \\  --zig <path>   Zig executable whose cache this is (default: zig)
    \\
;

/// What happened to the manifests and entries of one run.
const Counts = struct {
    manifests: usize = 0,
    manifests_rewritten: usize = 0,
    /// Not a complete manifest; Zig treats these as a miss and rewrites them.
    manifests_invalid: usize = 0,
    manifests_unreadable: usize = 0,

    /// Recorded stat already matches the file.
    current: usize = 0,
    /// Contents unchanged; recorded stat updated.
    restamped: usize = 0,
    /// Size or content digest differs from the record.
    changed: usize = 0,
    /// No file at the recorded path, or it could not be read.
    missing: usize = 0,
    /// Modified too recently for its mtime to identify its contents.
    too_recent: usize = 0,
    /// Tracked by stat alone, so there is no content digest to verify.
    metadata_only: usize = 0,
};

/// A path's current stat, and its content digest once something needed it.
const Observed = struct {
    size: u64,
    inode: Io.File.INode,
    mtime: i64,
    is_directory: bool,
    digest: ?Cache.BinDigest = null,
};

/// Stats and digests of the paths seen so far under one set of prefixes.
/// Thousands of manifests name the same standard-library and compiler sources,
/// so each is read once.
const Observations = struct {
    arena: Allocator,
    /// Keyed by prefix index followed by the path below that prefix.
    /// A null value records a path that could not be read.
    map: std.StringHashMapUnmanaged(?Observed) = .empty,
    key_buffer: std.ArrayList(u8) = .empty,

    fn get(self: *Observations, io: Io, prefixes: []const Io.Dir, prefix: usize, path: []const u8) RestampError!?*Observed {
        self.key_buffer.clearRetainingCapacity();
        try self.key_buffer.append(self.arena, @intCast(prefix));
        try self.key_buffer.appendSlice(self.arena, path);
        const gop = try self.map.getOrPut(self.arena, self.key_buffer.items);
        if (!gop.found_existing) {
            gop.key_ptr.* = try self.arena.dupe(u8, self.key_buffer.items);
            gop.value_ptr.* = if (prefixes[prefix].statFile(io, path, .{})) |stat| .{
                .size = stat.size,
                .inode = stat.inode,
                .mtime = @intCast(stat.mtime.toNanoseconds()),
                .is_directory = stat.kind == .directory,
            } else |err| switch (err) {
                error.Canceled => |e| return e,
                else => null,
            };
        }
        return if (gop.value_ptr.*) |*observed| observed else null;
    }
};

const ManifestOutcome = enum { unchanged, rewritten, invalid };

const RestampError = Allocator.Error || Io.Cancelable;

/// Re-stamp the entries of one manifest in place. `contents` is the whole
/// manifest file. Nothing is written to disk here, and `contents` must not be
/// written back unless the result is `.rewritten`.
fn restampManifest(
    io: Io,
    contents: []align(@alignOf(ManifestFile)) u8,
    prefixes: []const Io.Dir,
    observations: *Observations,
    /// Files modified at or after this time are not re-stamped; see `filesystemNow`.
    fs_now: i64,
    counts: *Counts,
) RestampError!ManifestOutcome {
    var tally: Counts = .{};
    var off: usize = 0;
    while (off + 1 < contents.len) {
        const file_off: ManifestFile.Offset = @fromBackingInt(std.math.cast(u32, off) orelse return .invalid);
        const file = file_off.getFallible(contents) catch return .invalid;
        if (file.flags.prefix >= prefixes.len) return .invalid;
        const recorded_path = file_off.pathFallible(contents) catch return .invalid;
        off += ManifestFile.sizeOf(recorded_path.len);

        if (file.flags.metadata_only) {
            tally.metadata_only += 1;
            continue;
        }

        // An empty path names the prefix directory itself.
        const path: []const u8 = if (recorded_path.len == 0) "." else recorded_path;
        const observed = try observations.get(io, prefixes, file.flags.prefix, path) orelse {
            tally.missing += 1;
            continue;
        };
        if (observed.is_directory != file.flags.is_directory) {
            tally.changed += 1;
            continue;
        }
        if (observed.size == file.size and observed.inode == file.inode and observed.mtime == file.mtime) {
            tally.current += 1;
            continue;
        }
        // A directory's size is the file system's business, not its listing's.
        if (!observed.is_directory and observed.size != file.size) {
            tally.changed += 1;
            continue;
        }

        const digest = observed.digest orelse digest: {
            const computed = contentDigest(io, observations.arena, prefixes[file.flags.prefix], path, observed.is_directory) catch |err| switch (err) {
                error.Canceled, error.OutOfMemory => |e| return e,
                else => {
                    tally.missing += 1;
                    continue;
                },
            };
            observed.digest = computed;
            break :digest computed;
        };
        if (!std.mem.eql(u8, &digest, &file.digest)) {
            tally.changed += 1;
            continue;
        }
        if (observed.mtime >= fs_now) {
            tally.too_recent += 1;
            continue;
        }

        file.size = observed.size;
        file.inode = observed.inode;
        file.mtime = observed.mtime;
        tally.restamped += 1;
    }

    // One terminating zero byte distinguishes a finished manifest.
    if (off + 1 != contents.len or contents[off] != 0) return .invalid;

    inline for (.{ "current", "restamped", "changed", "missing", "too_recent", "metadata_only" }) |field| {
        @field(counts, field) += @field(tally, field);
    }
    return if (tally.restamped != 0) .rewritten else .unchanged;
}

/// The digest Zig records for a path: for a file its bytes, and for a
/// directory the sorted names of its direct entries, each prefixed by its kind
/// and terminated by a zero byte.
fn contentDigest(io: Io, gpa: Allocator, parent: Io.Dir, path: []const u8, is_directory: bool) !Cache.BinDigest {
    var hasher = Cache.hasher_init;
    if (is_directory) {
        var dir = try parent.openDir(io, path, .{ .iterate = true });
        defer dir.close(io);

        var entries: std.ArrayList([]const u8) = .empty;
        defer {
            for (entries.items) |entry| gpa.free(entry);
            entries.deinit(gpa);
        }
        var it = dir.iterate();
        while (try it.next(io)) |entry| {
            const kind: u8 = @backingInt(ManifestFile.Kind.fromStat(entry.kind));
            const named = try std.mem.concat(gpa, u8, &.{ &.{kind}, entry.name, &.{0} });
            errdefer gpa.free(named);
            try entries.append(gpa, named);
        }
        std.mem.sortUnstable([]const u8, entries.items, {}, struct {
            fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
                return std.mem.lessThan(u8, lhs, rhs);
            }
        }.lessThan);
        for (entries.items) |entry| hasher.update(entry);
    } else {
        var file = try parent.openFile(io, path, .{ .mode = .read_only });
        defer file.close(io);

        var buffer: [1 << 16]u8 = undefined;
        var offset: u64 = 0;
        while (true) {
            const n = try file.readPositional(io, &.{&buffer}, offset);
            if (n == 0) break;
            hasher.update(buffer[0..n]);
            offset += n;
        }
    }
    var digest: Cache.BinDigest = undefined;
    hasher.final(&digest);
    return digest;
}

/// The file system's idea of the present, as the mtime of a file created now.
///
/// A file modified in the same clock tick could be modified again without its
/// mtime changing, so a stat taken now would not identify its contents. Zig
/// refuses to trust such a stat for the same reason, measured the same way.
fn filesystemNow(io: Io, dir: Io.Dir) !i64 {
    const name = "restamp-timestamp";
    var file = try dir.createFile(io, name, .{ .read = true, .truncate = true });
    const stat = file.stat(io);
    file.close(io);
    try dir.deleteFile(io, name);
    return @intCast((try stat).mtime.toNanoseconds());
}

/// Re-stamp every manifest in `manifest_dir`, the `h` directory of a cache.
fn restampManifestDir(
    io: Io,
    gpa: Allocator,
    manifest_dir: Io.Dir,
    prefixes: []const Io.Dir,
    observations: *Observations,
    fs_now: i64,
    counts: *Counts,
) !void {
    var it = manifest_dir.iterate();
    while (try it.next(io)) |entry| {
        if (entry.kind != .file or entry.name.len != Cache.hex_digest_len) continue;
        counts.manifests += 1;

        var file = manifest_dir.openFile(io, entry.name, .{ .mode = .read_write }) catch |err| switch (err) {
            error.Canceled => |e| return e,
            else => {
                counts.manifests_unreadable += 1;
                continue;
            },
        };
        defer file.close(io);

        const size = std.math.cast(usize, try file.length(io)) orelse return error.FileTooBig;
        const contents = try gpa.alignedAlloc(u8, .of(ManifestFile), size);
        defer gpa.free(contents);
        if (try file.readPositionalAll(io, contents, 0) != size) {
            counts.manifests_unreadable += 1;
            continue;
        }

        switch (try restampManifest(io, contents, prefixes, observations, fs_now, counts)) {
            .unchanged => {},
            .invalid => counts.manifests_invalid += 1,
            .rewritten => {
                try file.writePositionalAll(io, contents, 0);
                counts.manifests_rewritten += 1;
            },
        }
    }
}

/// Where the Zig executable keeps the things manifests refer to.
const ZigEnv = struct {
    lib_dir: []const u8,
    global_cache_dir: []const u8,
};

fn zigEnv(arena: Allocator, io: Io, zig_exe: []const u8) !ZigEnv {
    const result = try std.process.run(arena, io, .{ .argv = &.{ zig_exe, "env" } });
    if (result.term != .exited or result.term.exited != 0) return error.ZigEnvFailed;
    var diagnostics: std.zon.parse.Diagnostics = .{ .errors = &.{} };
    return std.zon.parse.fromSlice(ZigEnv, .{
        .gpa = arena,
        .arena = arena,
        .source = try arena.dupeSentinel(u8, result.stdout, 0),
        .diagnostics = &diagnostics,
        .ignore_unknown_fields = true,
    });
}

/// Re-stamp the local and global Zig caches of the build rooted at the
/// working directory.
pub fn main(init: std.process.Init) !void {
    var arena_state = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();
    const io = init.io;

    var stdout_buffer: [4096]u8 = undefined;
    var stdout_state = Io.File.stdout().writer(io, &stdout_buffer);
    const stdout = &stdout_state.interface;

    var zig_exe: []const u8 = "zig";
    const args = try init.minimal.args.toSlice(arena);
    var arg_index: usize = 1;
    while (arg_index < args.len) : (arg_index += 1) {
        const arg: []const u8 = args[arg_index];
        if (std.mem.eql(u8, arg, "--zig") and arg_index + 1 < args.len) {
            arg_index += 1;
            zig_exe = args[arg_index];
        } else {
            try stdout.writeAll(usage);
            try stdout.flush();
            std.process.exit(if (std.mem.eql(u8, arg, "--help")) 0 else 2);
        }
    }

    const env = try zigEnv(arena, io, zig_exe);
    const cwd = Io.Dir.cwd();
    const local_cache_path = init.environ_map.get("ZIG_LOCAL_CACHE_DIR") orelse ".zig-cache";

    var zig_lib = try cwd.openDir(io, env.lib_dir, .{});
    defer zig_lib.close(io);
    var global_cache = try cwd.openDir(io, env.global_cache_dir, .{});
    defer global_cache.close(io);
    var local_cache = try cwd.openDir(io, local_cache_path, .{});
    defer local_cache.close(io);

    var counts: Counts = .{};

    // A compilation into the global cache (compiler-rt, libc) names that
    // cache as its local one, so each cache is re-stamped against itself.
    var local_real: [std.fs.max_path_bytes]u8 = undefined;
    var global_real: [std.fs.max_path_bytes]u8 = undefined;
    const same_cache = std.mem.eql(
        u8,
        local_real[0..try local_cache.realPath(io, &local_real)],
        global_real[0..try global_cache.realPath(io, &global_real)],
    );
    const caches: []const Io.Dir = if (same_cache) &.{local_cache} else &.{ local_cache, global_cache };
    for (caches) |cache_dir| {
        var manifest_dir = cache_dir.openDir(io, "h", .{ .iterate = true }) catch |err| switch (err) {
            // A cache nothing was built into has no manifests.
            error.FileNotFound => continue,
            else => |e| return e,
        };
        defer manifest_dir.close(io);

        const prefixes = [prefix_count]Io.Dir{ cwd, zig_lib, cache_dir, global_cache, cwd };
        var observations: Observations = .{ .arena = arena };
        const fs_now = try filesystemNow(io, manifest_dir);
        try restampManifestDir(io, std.heap.smp_allocator, manifest_dir, &prefixes, &observations, fs_now, &counts);
    }

    try stdout.print(
        \\Re-stamped {d} of {d} cache manifests ({d} incomplete, {d} unreadable).
        \\Entries: {d} re-stamped, {d} already current, {d} changed, {d} missing, {d} too recent, {d} stat-only.
        \\
    , .{
        counts.manifests_rewritten, counts.manifests,     counts.manifests_invalid, counts.manifests_unreadable,
        counts.restamped,           counts.current,       counts.changed,           counts.missing,
        counts.too_recent,          counts.metadata_only,
    });
    try stdout.flush();
}

const testing = std.testing;

/// A cache in a temporary directory with one declared input file, the way
/// Zig's own cache tests set one up.
const TestCache = struct {
    tmp: testing.TmpDir,
    cwd_path: [:0]u8,
    tmp_path: []u8,
    manifest_dir: Io.Dir,
    cache: Cache,

    const input_name = "input.txt";
    const original_contents = "Hello, world!\n";

    fn init(self: *TestCache) !void {
        const io = testing.io;
        self.tmp = testing.tmpDir(.{});
        errdefer self.tmp.cleanup();
        self.cwd_path = try std.process.currentPathAlloc(io, testing.allocator);
        errdefer testing.allocator.free(self.cwd_path);
        self.tmp_path = try std.fs.path.join(testing.allocator, &.{testing.TmpDir.parent_dir_path});
        errdefer testing.allocator.free(self.tmp_path);

        try self.tmp.dir.writeFile(io, .{ .sub_path = input_name, .data = original_contents });
        try self.waitForClockTick();

        self.manifest_dir = try self.tmp.dir.createDirPathOpen(io, "h", .{ .open_options = .{ .iterate = true } });
        self.cache = .{
            .io = io,
            .gpa = testing.allocator,
            .manifest_dir = self.manifest_dir,
            .cwd = self.cwd_path,
        };
        self.cache.addPrefix(.{ .path = null, .handle = Io.Dir.cwd() });
        self.cache.addPrefix(.{ .path = self.tmp_path, .handle = self.tmp.dir });
    }

    fn deinit(self: *TestCache) void {
        self.manifest_dir.close(testing.io);
        testing.allocator.free(self.tmp_path);
        testing.allocator.free(self.cwd_path);
        self.tmp.cleanup();
    }

    fn prefixes(self: *TestCache) [2]Io.Dir {
        return .{ Io.Dir.cwd(), self.tmp.dir };
    }

    /// The outcome of asking the cache about the input, recording it on a miss.
    fn check(self: *TestCache) !Cache.Manifest.CheckResult {
        var man = self.cache.obtain();
        defer man.deinit();
        man.hash.addBytes("restamp test");
        _ = try man.addInputPath(.{
            .root_dir = .{ .path = self.tmp_path, .handle = self.tmp.dir },
            .sub_path = input_name,
        }, .{});
        var diag: Cache.Manifest.CheckDiagnostic = undefined;
        const result = try man.check(&diag, .none);
        if (result != .hit) {
            _ = man.missDigestHex();
            try man.finalize();
        }
        return result;
    }

    /// Replace the input with a new file: a new inode and mtime.
    fn replaceInput(self: *TestCache, contents: []const u8) !void {
        const io = testing.io;
        try self.tmp.dir.deleteFile(io, input_name);
        try self.tmp.dir.writeFile(io, .{ .sub_path = input_name, .data = contents });
        try self.waitForClockTick();
    }

    /// Wait until a file written now gets a later mtime than one written before.
    fn waitForClockTick(self: *TestCache) !void {
        const io = testing.io;
        const start = try filesystemNow(io, self.tmp.dir);
        while (try filesystemNow(io, self.tmp.dir) == start) {
            try Io.Clock.Duration.sleep(.{ .clock = .boot, .raw = .fromNanoseconds(1) }, io);
        }
    }

    fn restamp(self: *TestCache, fs_now: i64) !Counts {
        var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
        defer arena_state.deinit();
        var observations: Observations = .{ .arena = arena_state.allocator() };
        var counts: Counts = .{};
        const dirs = self.prefixes();
        try restampManifestDir(testing.io, arena_state.allocator(), self.manifest_dir, &dirs, &observations, fs_now, &counts);
        return counts;
    }
};

test "an identical replacement input is a hit after re-stamping" {
    var fixture: TestCache = undefined;
    try fixture.init();
    defer fixture.deinit();

    try testing.expectEqual(.incomplete_manifest, try fixture.check());
    try testing.expectEqual(.hit, try fixture.check());

    try fixture.replaceInput(TestCache.original_contents);
    const counts = try fixture.restamp(try filesystemNow(testing.io, fixture.tmp.dir));
    try testing.expectEqual(@as(usize, 1), counts.manifests_rewritten);
    try testing.expectEqual(@as(usize, 1), counts.restamped);
    try testing.expectEqual(.hit, try fixture.check());
}

test "an input with different contents stays a miss" {
    var fixture: TestCache = undefined;
    try fixture.init();
    defer fixture.deinit();

    try testing.expectEqual(.incomplete_manifest, try fixture.check());

    // Same length, so only the digest can tell the two apart.
    try fixture.replaceInput("Hello, w0rld!\n");
    const counts = try fixture.restamp(try filesystemNow(testing.io, fixture.tmp.dir));
    try testing.expectEqual(@as(usize, 0), counts.manifests_rewritten);
    try testing.expectEqual(@as(usize, 0), counts.restamped);
    try testing.expectEqual(@as(usize, 1), counts.changed);
    try testing.expect((try fixture.check()) != .hit);
}

test "an input modified in the current clock tick is not re-stamped" {
    var fixture: TestCache = undefined;
    try fixture.init();
    defer fixture.deinit();

    try testing.expectEqual(.incomplete_manifest, try fixture.check());

    try fixture.replaceInput(TestCache.original_contents);
    // A "now" no later than the input's mtime makes that mtime untrustworthy.
    const counts = try fixture.restamp(0);
    try testing.expectEqual(@as(usize, 0), counts.restamped);
    try testing.expectEqual(@as(usize, 1), counts.too_recent);
}

test "an unfinished manifest is left alone" {
    var fixture: TestCache = undefined;
    try fixture.init();
    defer fixture.deinit();

    try testing.expectEqual(.incomplete_manifest, try fixture.check());

    // Drop the terminating zero byte, as if the writer had been interrupted.
    var name_buffer: [Cache.hex_digest_len]u8 = undefined;
    var it = fixture.manifest_dir.iterate();
    const name = while (try it.next(testing.io)) |entry| {
        if (entry.name.len == Cache.hex_digest_len) break try std.fmt.bufPrint(&name_buffer, "{s}", .{entry.name});
    } else return error.NoManifest;
    const complete = try fixture.manifest_dir.readFileAlloc(testing.io, name, testing.allocator, .unlimited);
    defer testing.allocator.free(complete);
    try fixture.manifest_dir.writeFile(testing.io, .{ .sub_path = name, .data = complete[0 .. complete.len - 1] });

    try fixture.replaceInput(TestCache.original_contents);
    const counts = try fixture.restamp(try filesystemNow(testing.io, fixture.tmp.dir));
    try testing.expectEqual(@as(usize, 1), counts.manifests_invalid);
    try testing.expectEqual(@as(usize, 0), counts.manifests_rewritten);
}

test "canary: Zig 0.17 misses an identical replacement input on its own" {
    // This is the Zig behavior the tool exists for. When this test fails, Zig
    // compares a re-stat'd input with its recorded digest again: delete this
    // file, its build.zig wiring and the CI step that runs it.
    var fixture: TestCache = undefined;
    try fixture.init();
    defer fixture.deinit();

    try testing.expectEqual(.incomplete_manifest, try fixture.check());
    try fixture.replaceInput(TestCache.original_contents);
    try testing.expect((try fixture.check()) == .contents_changed);
}
