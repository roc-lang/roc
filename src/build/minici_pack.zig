//! Lists what one MiniCI shard's build artifact carries.
//!
//! CI runs `build-ci` once per OS and hands each of that OS's shards an
//! artifact to restore. A shard runs `zig build minici` and then `zig build
//! <job>` for each of its jobs, and every compile step those commands reach
//! has to be a cache hit (MiniCI's cache-reuse canary). So the artifact holds:
//!
//! - the configuration of each `zig build` command the shard runs, which Zig
//!   caches by the command's configuration options, so the shard never runs
//!   the build script;
//! - for each cached step in those commands' graphs that `build-ci` ran, its
//!   manifest and entry, as src/build/cache_roots.zig recorded them in the
//!   graph the jobs use, and everything those manifests record as inputs
//!   that `build-ci` produced (src/build/cache_closure.zig);
//! - what a shard needs before its first `zig` command, and the fetched
//!   packages, which the checkout does not contain.
//!
//! A job with build options of its own builds its own configuration, which
//! `build-ci` never built. The steps its graph shares with the configuration
//! the other jobs use are the same steps with the same cache keys, so its
//! entries are selected from that configuration's graph.

const std = @import("std");
const Io = std.Io;
const minici = @import("minici.zig");
const cache_closure = @import("cache_closure.zig");
const cache_roots = @import("cache_roots");

/// Where Zig 0.17 unpacks fetched packages by default, relative to the build
/// root. The cache records their files as inputs, and building them from
/// source needs every file, so the directory is carried whole.
const package_dir = "zig-pkg";

/// The top-level step CI runs a shard through. CI passes it the options of
/// `minici.configurationArgs` with no build arguments of its own.
const minici_step = "minici";

/// `cache_roots.output_<i>` for each `i`.
const outputs: [cache_roots.output_count][]const u8 = outputs: {
    @setEvalBranchQuota(1000 * (cache_roots.output_count + 1));
    var paths: [cache_roots.output_count][]const u8 = undefined;
    for (&paths, 0..) |*path, i| path.* = @field(cache_roots, std.fmt.comptimePrint("output_{d}", .{i}));
    break :outputs paths;
};

const usage =
    \\Usage:
    \\  minici-pack shard <host> <slot> <slots>
    \\      Print the name of the slot-th (from 0) shard of <host> (a
    \\      `minici.Host` name) that restores a build artifact, or nothing past
    \\      the last one. Fails when the host has more such shards than <slots>.
    \\  minici-pack list <shard> <zig> <list-file>
    \\      Write the paths the shard's artifact carries to <list-file>, one per
    \\      line, relative to the build root. Run it from the build root after
    \\      `zig build build-minici-pack` with the options CI runs `build-ci` with.
    \\
;

fn fail(comptime format: []const u8, args: anytype) noreturn {
    std.debug.print("minici-pack: " ++ format ++ "\n", args);
    std.process.exit(1);
}

fn usageError() noreturn {
    std.debug.print("{s}", .{usage});
    std.process.exit(2);
}

/// The shards of `host` whose jobs need `build-ci`, in `minici.shards`
/// order.
fn hostShards(arena: std.mem.Allocator, host: minici.Host) ![]const minici.Shard {
    var list: std.ArrayList(minici.Shard) = .empty;
    for (minici.shards) |shard| {
        if (shard.host == host and shard.lane.needsBuildCi()) try list.append(arena, shard);
    }
    return list.items;
}

fn realPath(arena: std.mem.Allocator, io: Io, path: []const u8) ![]const u8 {
    return Io.Dir.cwd().realPathFileAlloc(io, path, arena) catch |err|
        fail("cannot resolve `{s}`: {t}", .{ path, err });
}

/// `path` relative to `dir`, with `/` separators, for a tar file list.
fn relativeTo(arena: std.mem.Allocator, path: []const u8, dir: []const u8) ![]const u8 {
    if (!std.mem.startsWith(u8, path, dir) or path.len <= dir.len or !std.fs.path.isSep(path[dir.len])) {
        fail("`{s}` is not inside the build root `{s}`, so the artifact cannot carry it", .{ path, dir });
    }
    const rest = try arena.dupe(u8, path[dir.len + 1 ..]);
    std.mem.replaceScalar(u8, rest, '\\', '/');
    return rest;
}

/// The cache entry a path the build runner resolved lies in. The build runner
/// formats every such path from its local cache directory, as it formatted
/// `cache_roots.local_cache`.
fn entryAt(path: []const u8) cache_closure.Entry {
    const rest = cache_closure.below(path, cache_roots.local_cache) orelse
        fail("`{s}` is not in the local cache `{s}`", .{ path, cache_roots.local_cache });
    return cache_closure.entryOf(rest) catch fail("`{s}` is not in a cache entry", .{path});
}

/// The configuration entry that `zig build` with `options` uses, created if
/// it is not cached yet.
fn configurationEntry(arena: std.mem.Allocator, io: Io, zig_exe: []const u8, options: []const []const u8) !cache_closure.Entry {
    const argv = try std.mem.concat(arena, []const u8, &.{ &.{ zig_exe, "build", "--print-configuration-path" }, options });
    const result = std.process.run(arena, io, .{ .argv = argv }) catch |err| fail("cannot run `{s} build`: {t}", .{ zig_exe, err });
    switch (result.term) {
        .exited => |code| if (code != 0) fail("`zig build --print-configuration-path` exited with {d}:\n{s}", .{ code, result.stderr }),
        .signal, .stopped, .unknown => fail("`zig build --print-configuration-path` did not exit normally:\n{s}", .{result.stderr}),
    }
    const path = std.mem.trim(u8, result.stdout, " \t\r\n");
    const entry = entryAt(path);
    // A configuration Zig did not cache (one whose build script reads
    // something Zig cannot track) is written outside `c`, and every
    // `zig build` would run the build script again.
    if (entry.kind != .c) fail("the configuration for `{s}` is not cached: `{s}`", .{ std.mem.join(arena, " ", options) catch "", path });
    return entry;
}

/// The total size of the files at `path` (a file, or a directory and
/// everything in it).
fn pathBytes(io: Io, path: []const u8) !u64 {
    const cwd = Io.Dir.cwd();
    const stat = try cwd.statFile(io, path, .{});
    if (stat.kind != .directory) return stat.size;
    var dir = try cwd.openDir(io, path, .{ .iterate = true });
    defer dir.close(io);
    var walker = try dir.walk(std.heap.smp_allocator);
    defer walker.deinit();
    var total: u64 = 0;
    while (try walker.next(io)) |entry| {
        if (entry.kind == .file) total += (try entry.dir.statFile(io, entry.basename, .{})).size;
    }
    return total;
}

fn writeList(arena: std.mem.Allocator, io: Io, shard_name: []const u8, zig_exe: []const u8, list_path: []const u8) !void {
    const shard = minici.shardByName(shard_name) orelse fail("unknown MiniCI shard `{s}`", .{shard_name});
    if (!shard.lane.needsBuildCi()) fail("shard `{s}` restores no build artifact", .{shard.name});

    const layout: cache_closure.Layout = .{
        .prefixes = .{
            .cwd = try realPath(arena, io, cache_closure.formattedDirectory(cache_roots.cwd)),
            .zig_lib = try realPath(arena, io, cache_closure.formattedDirectory(cache_roots.zig_lib)),
            .local_cache = try realPath(arena, io, cache_closure.formattedDirectory(cache_roots.local_cache)),
            .global_cache = try realPath(arena, io, cache_closure.formattedDirectory(cache_roots.global_cache)),
            .build_root = try realPath(arena, io, cache_closure.formattedDirectory(cache_roots.build_root)),
        },
        .install_prefix = try realPath(arena, io, cache_closure.formattedDirectory(cache_roots.install_prefix)),
    };
    const build_root = layout.prefixes.build_root;
    if (!std.mem.eql(u8, try realPath(arena, io, "."), build_root)) {
        fail("run from the build root `{s}`", .{build_root});
    }

    // The top-level steps the shard runs, and the configurations it runs
    // them in.
    const jobs = try minici.shardJobs(arena, shard);
    var steps: std.ArrayList([]const u8) = .empty;
    try steps.append(arena, minici_step);
    var configurations: std.StringArrayHashMapUnmanaged([]const []const u8) = .empty;
    const shared = try minici.configurationArgs(arena, &.{});
    try configurations.put(arena, try std.mem.join(arena, " ", shared), shared);
    for (jobs) |job| {
        try steps.append(arena, job.name);
        const options = try minici.configurationArgs(arena, job.build_args);
        try configurations.put(arena, try std.mem.join(arena, " ", options), options);
    }

    var roots: std.ArrayList(cache_closure.Entry) = .empty;
    for (configurations.values()) |options| {
        try roots.append(arena, try configurationEntry(arena, io, zig_exe, options));
    }
    var unaddressable: std.ArrayList(u8) = .empty;
    for (steps.items) |step| {
        const index = for (cache_roots.top_level_steps, 0..) |name, i| {
            if (std.mem.eql(u8, name, step)) break i;
        } else fail("`{s}` is not a top-level build step", .{step});
        var indices = std.mem.tokenizeScalar(u8, cache_roots.top_level_outputs[index], ' ');
        while (indices.next()) |text| {
            const output = std.fmt.parseInt(usize, text, 10) catch unreachable; // cache_roots.zig writes each index with `{d}`
            try roots.append(arena, entryAt(outputs[output]));
        }
        const missing = cache_roots.top_level_unaddressable[index];
        if (missing.len != 0) try unaddressable.print(arena, "  {s}:\n    {s}\n", .{ step, missing });
    }
    // A cached step whose entry nothing names would be left out, and the
    // shard would run it again.
    if (unaddressable.items.len != 0) {
        fail("these cached steps declare no output that names their cache entry, so the artifact cannot carry it:\n{s}", .{unaddressable.items});
    }

    var manifest_dir = try Io.Dir.cwd().openDir(io, try std.fs.path.join(arena, &.{ layout.prefixes.local_cache, "h" }), .{ .iterate = true });
    defer manifest_dir.close(io);
    const index = try cache_closure.indexManifests(arena, io, manifest_dir);
    var diagnostic: cache_closure.SelectDiagnostic = .{};
    const selection = cache_closure.select(arena, io, layout, manifest_dir, &index, roots.items, &diagnostic) catch |err| {
        const entry_name: []const u8 = if (diagnostic.entry) |e| try std.fmt.allocPrint(arena, "{t}/{s}", .{ e.kind, &e.digest }) else "none";
        const manifest_name: []const u8 = if (diagnostic.manifest) |m| try arena.dupe(u8, &m) else "none";
        fail("selecting the cache failed: {t} (entry {s}, manifest h/{s}, path {s})", .{ err, entry_name, manifest_name, diagnostic.path orelse "none" });
    };

    const local_cache = try relativeTo(arena, layout.prefixes.local_cache, build_root);
    const install_prefix = try relativeTo(arena, layout.install_prefix, build_root);
    var paths: std.ArrayList([]const u8) = .empty;
    for (selection.manifests.keys()) |manifest| try paths.append(arena, try std.fmt.allocPrint(arena, "{s}/h/{s}", .{ local_cache, &manifest }));
    for (selection.entries.keys()) |entry| {
        const path = try std.fmt.allocPrint(arena, "{s}/{t}/{s}", .{ local_cache, entry.kind, &entry.digest });
        // A compilation that emits nothing writes no entry directory; its
        // manifest is all a cache hit reads.
        Io.Dir.cwd().access(io, path, .{}) catch |err| switch (err) {
            error.FileNotFound => continue,
            else => |e| return e,
        };
        try paths.append(arena, path);
    }
    for (selection.cached_files.keys()) |cached_file| {
        const rest = try arena.dupe(u8, cached_file);
        std.mem.replaceScalar(u8, rest, '\\', '/');
        try paths.append(arena, try std.fmt.allocPrint(arena, "{s}/{s}", .{ local_cache, rest }));
    }
    for (selection.installed.keys()) |installed| {
        const rest = try arena.dupe(u8, installed);
        std.mem.replaceScalar(u8, rest, '\\', '/');
        try paths.append(arena, try std.fmt.allocPrint(arena, "{s}/{s}", .{ install_prefix, rest }));
    }
    try paths.append(arena, minici.restamp_exe);
    try paths.append(arena, package_dir);
    std.mem.sortUnstable([]const u8, paths.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.lessThan(u8, a, b);
        }
    }.lessThan);

    var bytes: u64 = 0;
    var out: std.ArrayList(u8) = .empty;
    for (paths.items) |path| {
        bytes += pathBytes(io, path) catch |err| fail("cannot read `{s}`: {t}", .{ path, err });
        try out.print(arena, "{s}\n", .{path});
    }
    try Io.Dir.cwd().writeFile(io, .{ .sub_path = list_path, .data = out.items });

    std.debug.print(
        "minici-pack: shard `{s}` ({d} jobs, {d} configurations): {d} manifests, {d} cache entries, {d} installed files; {d:.1} MB before compression\n",
        .{
            shard.name,                                     jobs.len,                  configurations.count(),
            selection.manifests.count(),                    selection.entries.count(), selection.installed.count(),
            @as(f64, @floatFromInt(bytes)) / (1024 * 1024),
        },
    );
}

/// Runs the `shard` or `list` command described in `usage`.
pub fn main(init: std.process.Init) !void {
    var arena_state = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();
    const io = init.io;
    const args = try init.minimal.args.toSlice(arena);
    if (args.len < 2) usageError();
    const command: []const u8 = args[1];

    if (std.mem.eql(u8, command, "shard") and args.len == 5) {
        const host = std.meta.stringToEnum(minici.Host, args[2]) orelse fail("unknown MiniCI host `{s}`", .{args[2]});
        const slot = std.fmt.parseInt(usize, args[3], 10) catch usageError();
        const slots = std.fmt.parseInt(usize, args[4], 10) catch usageError();
        const shards = try hostShards(arena, host);
        if (shards.len > slots) fail("this host has {d} shards that restore a build artifact, more than the {d} slots CI packs", .{ shards.len, slots });
        if (slot < shards.len) {
            var stdout_buffer: [256]u8 = undefined;
            var stdout = Io.File.stdout().writer(io, &stdout_buffer);
            try stdout.interface.print("{s}\n", .{shards[slot].name});
            try stdout.interface.flush();
        }
    } else if (std.mem.eql(u8, command, "list") and args.len == 5) {
        try writeList(arena, io, args[2], args[3], args[4]);
    } else {
        usageError();
    }
}
