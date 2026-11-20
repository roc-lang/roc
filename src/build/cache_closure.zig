//! The part of a Zig cache that a set of cache entries depends on.
//!
//! A MiniCI shard restores the cache `build-ci` filled and expects every step
//! it runs to hit it. A step's cache lookup reads one manifest (`h/<name>`)
//! and, on a hit, the entry that manifest names (`o/<digest>`, or `c/<digest>`
//! for a build configuration). Checking the hit also reads every input the
//! manifest records, and an input that a cached step produced lives in that
//! step's entry, whose manifest is looked up when the producing step runs.
//!
//! `select` starts from the entries of the steps a shard runs and follows
//! manifest inputs to everything they depend on. It reads only what the cache
//! itself records: which manifest produced an entry is the manifest whose
//! final digest is that entry's name, computed as `std.Build.Cache` computes
//! it.

const std = @import("std");
const Io = std.Io;
const Allocator = std.mem.Allocator;
const Cache = std.Build.Cache;
const ManifestFile = Cache.Manifest.File;

/// A cache entry or manifest name, as `std.Build.Cache` writes it.
pub const HexDigest = Cache.HexDigest;

/// The directories of a cache that hold entries a manifest can name.
pub const EntryKind = enum {
    /// `o/<digest>`: the outputs of a compilation, generated file or run.
    o,
    /// `c/<digest>`: a serialized build configuration.
    c,
};

/// One entry directory of the local cache: `<kind>/<digest>`.
pub const Entry = struct {
    kind: EntryKind,
    digest: HexDigest,
};

/// The directories Zig records manifest paths against, in the order the
/// compiler and the build runner register them with `Cache.addPrefix`, each as
/// an absolute, resolved path. Zig records a path against the first of them
/// that contains it, or in full when none does.
pub const Prefixes = struct {
    cwd: []const u8,
    zig_lib: []const u8,
    local_cache: []const u8,
    global_cache: []const u8,
    build_root: []const u8,

    const count = 5;

    fn get(self: Prefixes, index: usize) ?[]const u8 {
        return switch (index) {
            0 => self.cwd,
            1 => self.zig_lib,
            2 => self.local_cache,
            3 => self.global_cache,
            4 => self.build_root,
            else => null,
        };
    }
};

/// Where a selection's files are, as absolute, resolved paths.
pub const Layout = struct {
    prefixes: Prefixes,
    /// The `zig build` install prefix. Files installed there that a selected
    /// manifest records as inputs belong to the selection; Zig hashes a Run
    /// step's artifact by its installed path once it is installed.
    install_prefix: []const u8,
};

/// Where a path that a manifest records lives.
pub const Location = union(enum) {
    /// Inside a cache entry of the local cache.
    entry: Entry,
    /// In the local cache outside every entry directory, the path relative to
    /// the local cache. Zig keeps some files there by their contents rather
    /// than as a step's output, such as each module's generated `builtin.zig`
    /// under `b`, so no manifest produced them and they are carried as they
    /// are.
    cached_file: []const u8,
    /// Under the install prefix; the path relative to the install prefix.
    installed: []const u8,
    /// Neither: the source checkout, the Zig installation or the global cache.
    /// The global cache is per machine, so it is not part of a selection.
    elsewhere,
};

/// Why a path in the local cache names no entry.
pub const LocateError = error{
    /// A path in an entry directory (`o` or `c`) that is not inside an entry
    /// named by a digest, or, for `entryOf`, a path outside every entry
    /// directory.
    NotACacheEntry,
};

/// The directory a path the build runner formatted for an Options module
/// names. `std.Build.Cache.Path.formatEscapeString` writes a directory with
/// no path of its own and no sub path, which is how the build runner
/// registers the current directory, as the empty string.
pub fn formattedDirectory(formatted: []const u8) []const u8 {
    return if (formatted.len == 0) "." else formatted;
}

/// The rest of `path` below the directory `dir`, or null when `path` is not
/// inside `dir`. Both are absolute and resolved.
pub fn below(path: []const u8, dir: []const u8) ?[]const u8 {
    if (path.len <= dir.len) return null;
    if (!std.mem.startsWith(u8, path, dir)) return null;
    if (std.fs.path.isSep(dir[dir.len - 1])) return path[dir.len..];
    if (!std.fs.path.isSep(path[dir.len])) return null;
    return path[dir.len + 1 ..];
}

fn isHexDigest(text: []const u8) bool {
    if (text.len != Cache.hex_digest_len) return false;
    for (text) |c| switch (c) {
        '0'...'9', 'a'...'f' => {},
        else => return false,
    };
    return true;
}

/// The entry that `rest`, a path relative to the local cache, lies in.
pub fn entryOf(rest: []const u8) LocateError!Entry {
    var components = std.mem.tokenizeAny(u8, rest, if (@import("builtin").os.tag == .windows) "/\\" else "/");
    const dir = components.next() orelse return error.NotACacheEntry;
    const kind: EntryKind = if (std.mem.eql(u8, dir, "o"))
        .o
    else if (std.mem.eql(u8, dir, "c"))
        .c
    else
        return error.NotACacheEntry;
    const digest = components.next() orelse return error.NotACacheEntry;
    if (!isHexDigest(digest)) return error.NotACacheEntry;
    return .{ .kind = kind, .digest = digest[0..Cache.hex_digest_len].* };
}

/// Whether `rest`, a path relative to the local cache, is in a directory
/// whose subdirectories are cache entries (`o` or `c`).
fn inEntryDirectory(rest: []const u8) bool {
    var components = std.mem.tokenizeAny(u8, rest, if (@import("builtin").os.tag == .windows) "/\\" else "/");
    const dir = components.next() orelse return false;
    return std.mem.eql(u8, dir, "o") or std.mem.eql(u8, dir, "c");
}

/// Where `path`, absolute and resolved, lives.
pub fn locate(layout: Layout, path: []const u8) LocateError!Location {
    if (below(path, layout.prefixes.local_cache)) |rest| {
        if (inEntryDirectory(rest)) return .{ .entry = try entryOf(rest) };
        return .{ .cached_file = rest };
    }
    if (below(path, layout.install_prefix)) |rest| return .{ .installed = rest };
    return .elsewhere;
}

/// Reads a manifest's input entries one at a time.
const ManifestInputs = struct {
    contents: []const u8,
    offset: usize = 0,

    const Input = struct {
        file: *const ManifestFile,
        path: []const u8,
    };

    /// The next input, null after the last, or `error.InvalidFormat` for a
    /// manifest that is not complete (Zig treats those as a miss).
    fn next(self: *ManifestInputs) error{InvalidFormat}!?Input {
        if (self.offset + 1 == self.contents.len and self.contents[self.offset] == 0) return null;
        if (self.offset + 1 >= self.contents.len) return error.InvalidFormat;
        const file_off: ManifestFile.Offset = @fromBackingInt(std.math.cast(u32, self.offset) orelse return error.InvalidFormat);
        const file = try file_off.getFallibleConst(self.contents);
        if (file.flags.prefix >= Prefixes.count) return error.InvalidFormat;
        const path = try file_off.pathFallible(self.contents);
        self.offset += ManifestFile.sizeOf(path.len);
        return .{ .file = file, .path = path };
    }
};

/// The digest naming the entry that the manifest `name` with `contents`
/// produced: Zig hashes the manifest's input digest (its name) followed by the
/// recorded digest of each input, in order (`Cache.Manifest.finalDigest`).
pub fn finalDigest(name: HexDigest, contents: []const u8) error{InvalidFormat}!HexDigest {
    var input_digest: Cache.BinDigest = undefined;
    _ = std.fmt.hexToBytes(&input_digest, &name) catch return error.InvalidFormat;
    var hasher = Cache.hasher_init;
    hasher.update(&input_digest);
    var inputs: ManifestInputs = .{ .contents = contents };
    while (try inputs.next()) |input| hasher.update(&input.file.digest);
    var digest: Cache.BinDigest = undefined;
    hasher.final(&digest);
    return Cache.binToHex(digest);
}

fn readManifest(arena: Allocator, io: Io, manifest_dir: Io.Dir, name: HexDigest) ![]align(@alignOf(ManifestFile)) u8 {
    var file = try manifest_dir.openFile(io, &name, .{});
    defer file.close(io);
    const size = std.math.cast(usize, try file.length(io)) orelse return error.FileTooBig;
    const contents = try arena.alignedAlloc(u8, .of(ManifestFile), size);
    if (try file.readPositionalAll(io, contents, 0) != size) return error.ManifestChangedWhileReading;
    return contents;
}

/// For every complete manifest in a cache's `h` directory, the manifest that
/// produced each entry: final digest to manifest name.
pub const ManifestIndex = std.AutoHashMapUnmanaged(HexDigest, HexDigest);

/// Reads every manifest in `manifest_dir` and records which entry each one
/// produced.
pub fn indexManifests(arena: Allocator, io: Io, manifest_dir: Io.Dir) !ManifestIndex {
    var index: ManifestIndex = .empty;
    var it = manifest_dir.iterate();
    while (try it.next(io)) |dir_entry| {
        if (dir_entry.kind != .file or !isHexDigest(dir_entry.name)) continue;
        const name = dir_entry.name[0..Cache.hex_digest_len].*;
        const contents = try readManifest(arena, io, manifest_dir, name);
        const digest = finalDigest(name, contents) catch |err| switch (err) {
            // An interrupted write; Zig misses on it and rewrites it.
            error.InvalidFormat => continue,
        };
        try index.put(arena, digest, name);
    }
    return index;
}

/// What `select` found that a set of root entries depends on.
pub const Selection = struct {
    entries: std.AutoArrayHashMapUnmanaged(Entry, void) = .empty,
    /// Names of the manifests in `h`.
    manifests: std.AutoArrayHashMapUnmanaged(HexDigest, void) = .empty,
    /// Installed files, relative to the install prefix.
    installed: std.StringArrayHashMapUnmanaged(void) = .empty,
    /// Files in the local cache outside every entry, relative to the local
    /// cache (see `Location.cached_file`).
    cached_files: std.StringArrayHashMapUnmanaged(void) = .empty,
};

/// Why `select` could not follow a root entry to what it depends on.
pub const SelectError = error{
    /// A root entry that no manifest produced. Every root is the output of a
    /// cached step, whose lookup goes through its manifest.
    RootWithoutManifest,
    /// A manifest named by the index is not complete or not readable.
    InvalidManifest,
} || LocateError || Allocator.Error || Io.Dir.OpenError || Io.File.OpenError || Io.File.ReadPositionalError || Io.File.LengthError || error{ FileTooBig, ManifestChangedWhileReading };

/// What to report about a `select` that failed.
pub const SelectDiagnostic = struct {
    entry: ?Entry = null,
    manifest: ?HexDigest = null,
    path: ?[]const u8 = null,
};

/// Everything that `roots`, and the entries their manifests record as inputs,
/// need from the local cache and the install prefix to be cache hits.
///
/// An input entry that no manifest produced (the build runner writes some
/// files into `o` directly, such as the `dependencies.zig` a configuration
/// manifest records) is selected without one.
pub fn select(
    arena: Allocator,
    io: Io,
    layout: Layout,
    manifest_dir: Io.Dir,
    index: *const ManifestIndex,
    roots: []const Entry,
    diagnostic: *SelectDiagnostic,
) SelectError!Selection {
    var selection: Selection = .{};
    var pending: std.ArrayList(Entry) = .empty;
    for (roots) |root| {
        if (!index.contains(root.digest)) {
            diagnostic.* = .{ .entry = root };
            return error.RootWithoutManifest;
        }
        try pending.append(arena, root);
    }

    while (pending.pop()) |entry| {
        if ((try selection.entries.getOrPut(arena, entry)).found_existing) continue;
        const manifest = index.get(entry.digest) orelse continue;
        try selection.manifests.put(arena, manifest, {});

        const contents = try readManifest(arena, io, manifest_dir, manifest);
        var inputs: ManifestInputs = .{ .contents = contents };
        while (inputs.next() catch {
            diagnostic.* = .{ .entry = entry, .manifest = manifest };
            return error.InvalidManifest;
        }) |input| {
            const prefix = layout.prefixes.get(input.file.flags.prefix).?;
            const path = if (std.fs.path.isAbsolute(input.path))
                input.path
            else
                try std.fs.path.resolveAlloc(arena, &.{ prefix, input.path });
            const location = locate(layout, path) catch |err| {
                diagnostic.* = .{ .entry = entry, .manifest = manifest, .path = path };
                return err;
            };
            switch (location) {
                .entry => |input_entry| try pending.append(arena, input_entry),
                .installed => |rest| try selection.installed.put(arena, rest, {}),
                .cached_file => |rest| try selection.cached_files.put(arena, rest, {}),
                .elsewhere => {},
            }
        }
    }
    return selection;
}

const testing = std.testing;

/// A build root in a temporary directory with a local cache and an install
/// prefix inside it, and a `Cache` that registers the five prefixes Zig does.
const TestCache = struct {
    tmp: testing.TmpDir,
    root: []u8,
    local_cache_path: []u8,
    install_prefix: []u8,
    zig_lib: []u8,
    global_cache: []u8,
    local_cache: Io.Dir,
    manifest_dir: Io.Dir,
    cache: Cache,

    fn init(self: *TestCache) !void {
        const io = testing.io;
        const gpa = testing.allocator;
        self.tmp = testing.tmpDir(.{});
        errdefer self.tmp.cleanup();
        inline for (.{ "local-cache/h", "zig-out/bin", "zig-lib", "global-cache" }) |sub_path| {
            try self.tmp.dir.createDirPath(io, sub_path);
        }
        var buffer: [std.fs.max_path_bytes]u8 = undefined;
        self.root = try gpa.dupe(u8, buffer[0..try self.tmp.dir.realPath(io, &buffer)]);
        self.local_cache_path = try std.fs.path.join(gpa, &.{ self.root, "local-cache" });
        self.install_prefix = try std.fs.path.join(gpa, &.{ self.root, "zig-out" });
        self.zig_lib = try std.fs.path.join(gpa, &.{ self.root, "zig-lib" });
        self.global_cache = try std.fs.path.join(gpa, &.{ self.root, "global-cache" });
        self.local_cache = try self.tmp.dir.openDir(io, "local-cache", .{});
        self.manifest_dir = try self.local_cache.openDir(io, "h", .{ .iterate = true });
        self.cache = .{ .io = io, .gpa = gpa, .manifest_dir = self.manifest_dir, .cwd = self.root };
        self.cache.addPrefix(.{ .path = self.root, .handle = self.tmp.dir });
        self.cache.addPrefix(.{ .path = self.zig_lib, .handle = self.tmp.dir });
        self.cache.addPrefix(.{ .path = self.local_cache_path, .handle = self.local_cache });
        self.cache.addPrefix(.{ .path = self.global_cache, .handle = self.tmp.dir });
        self.cache.addPrefix(.{ .path = self.root, .handle = self.tmp.dir });
    }

    fn deinit(self: *TestCache) void {
        const gpa = testing.allocator;
        self.manifest_dir.close(testing.io);
        self.local_cache.close(testing.io);
        inline for (.{ "root", "local_cache_path", "install_prefix", "zig_lib", "global_cache" }) |field| gpa.free(@field(self, field));
        self.tmp.cleanup();
    }

    fn layout(self: *const TestCache) Layout {
        return .{
            .prefixes = .{
                .cwd = self.root,
                .zig_lib = self.zig_lib,
                .local_cache = self.local_cache_path,
                .global_cache = self.global_cache,
                .build_root = self.root,
            },
            .install_prefix = self.install_prefix,
        };
    }

    /// Records a cached step with `inputs` (paths relative to the build root)
    /// the way a step does on a miss, writes `output` into its entry, and
    /// returns the entry's digest.
    fn addStep(self: *TestCache, key: []const u8, inputs: []const []const u8, output: []const u8) !HexDigest {
        const io = testing.io;
        var man = self.cache.obtain();
        defer man.deinit();
        man.hash.addBytes(key);
        for (inputs) |input| {
            _ = try man.addInputPath(.{ .root_dir = .{ .path = self.root, .handle = self.tmp.dir }, .sub_path = input }, .{});
        }
        var diag: Cache.Manifest.CheckDiagnostic = undefined;
        try testing.expect(try man.check(&diag, .none) != .hit);
        const digest = man.missDigestHex();
        try man.finalize();
        const entry_path = try std.fmt.allocPrint(testing.allocator, "local-cache/o/{s}/{s}", .{ &digest, output });
        defer testing.allocator.free(entry_path);
        try self.tmp.dir.createDirPath(io, std.fs.path.dirname(entry_path).?);
        try self.tmp.dir.writeFile(io, .{ .sub_path = entry_path, .data = key });
        return digest;
    }
};

test "the current directory, formatted empty, is `.`" {
    try std.testing.expectEqualStrings(".", formattedDirectory(""));
    try std.testing.expectEqualStrings("/work/.zig-cache", formattedDirectory("/work/.zig-cache"));
}

test "an entry's manifest is the one whose final digest names it" {
    var fixture: TestCache = undefined;
    try fixture.init();
    defer fixture.deinit();
    try fixture.tmp.dir.writeFile(testing.io, .{ .sub_path = "source.zig", .data = "source" });

    const digest = try fixture.addStep("compile", &.{"source.zig"}, "exe");
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const index = try indexManifests(arena_state.allocator(), testing.io, fixture.manifest_dir);
    try testing.expectEqual(@as(u32, 1), index.count());
    try testing.expect(index.contains(digest));
}

test "select follows inputs into the entries and installed files they come from" {
    var fixture: TestCache = undefined;
    try fixture.init();
    defer fixture.deinit();
    const io = testing.io;
    try fixture.tmp.dir.writeFile(io, .{ .sub_path = "source.zig", .data = "source" });

    // A generator, a library compiled from its output, an executable linking
    // the library, and the executable's installed copy that a run hashes.
    const generated = try fixture.addStep("generate", &.{"source.zig"}, "gen.zig");
    const generated_input = try std.fmt.allocPrint(testing.allocator, "local-cache/o/{s}/gen.zig", .{&generated});
    defer testing.allocator.free(generated_input);
    const lib = try fixture.addStep("lib", &.{ "source.zig", generated_input }, "lib.a");
    const lib_input = try std.fmt.allocPrint(testing.allocator, "local-cache/o/{s}/lib.a", .{&lib});
    defer testing.allocator.free(lib_input);
    const exe = try fixture.addStep("exe", &.{lib_input}, "exe");
    try fixture.tmp.dir.writeFile(io, .{ .sub_path = "zig-out/bin/exe", .data = "exe" });
    const run = try fixture.addStep("run", &.{"zig-out/bin/exe"}, "out.txt");
    // Unrelated to the roots.
    _ = try fixture.addStep("other", &.{"source.zig"}, "other");

    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();
    const index = try indexManifests(arena, io, fixture.manifest_dir);
    try testing.expectEqual(@as(u32, 5), index.count());

    var diagnostic: SelectDiagnostic = .{};
    const selection = try select(arena, io, fixture.layout(), fixture.manifest_dir, &index, &.{
        .{ .kind = .o, .digest = exe },
        .{ .kind = .o, .digest = run },
    }, &diagnostic);

    try testing.expectEqual(@as(usize, 4), selection.entries.count());
    for ([_]HexDigest{ generated, lib, exe, run }) |digest| {
        try testing.expect(selection.entries.contains(.{ .kind = .o, .digest = digest }));
        try testing.expect(selection.manifests.contains(index.get(digest).?));
    }
    try testing.expectEqual(@as(usize, 4), selection.manifests.count());
    try testing.expectEqual(@as(usize, 1), selection.installed.count());
    try testing.expectEqualStrings("bin" ++ std.fs.path.sep_str ++ "exe", selection.installed.keys()[0]);
}

test "select carries the local-cache files outside entries that a manifest records" {
    var fixture: TestCache = undefined;
    try fixture.init();
    defer fixture.deinit();
    const io = testing.io;
    try fixture.tmp.dir.writeFile(io, .{ .sub_path = "source.zig", .data = "source" });
    // The `builtin.zig` Zig generates for a module, kept under `b` by its
    // contents rather than as a step's output.
    const builtin_path = "local-cache/b/0123456789abcdef0123456789abcdef/builtin.zig";
    try fixture.tmp.dir.createDirPath(io, "local-cache/b/0123456789abcdef0123456789abcdef");
    try fixture.tmp.dir.writeFile(io, .{ .sub_path = builtin_path, .data = "builtin" });
    const exe = try fixture.addStep("exe", &.{ "source.zig", builtin_path }, "exe");

    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();
    const index = try indexManifests(arena, io, fixture.manifest_dir);
    var diagnostic: SelectDiagnostic = .{};
    const selection = try select(arena, io, fixture.layout(), fixture.manifest_dir, &index, &.{.{ .kind = .o, .digest = exe }}, &diagnostic);

    try testing.expectEqual(@as(usize, 1), selection.entries.count());
    try testing.expectEqual(@as(usize, 1), selection.cached_files.count());
    const sep = std.fs.path.sep_str;
    try testing.expectEqualStrings("b" ++ sep ++ "0123456789abcdef0123456789abcdef" ++ sep ++ "builtin.zig", selection.cached_files.keys()[0]);
}

test "an input entry with no manifest is selected on its own" {
    var fixture: TestCache = undefined;
    try fixture.init();
    defer fixture.deinit();
    const io = testing.io;
    const written = "0123456789abcdef0123456789abcdef";
    try fixture.tmp.dir.createDirPath(io, "local-cache/o/" ++ written);
    try fixture.tmp.dir.writeFile(io, .{ .sub_path = "local-cache/o/" ++ written ++ "/dependencies.zig", .data = "deps" });
    const config = try fixture.addStep("configure", &.{"local-cache/o/" ++ written ++ "/dependencies.zig"}, "unused");

    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();
    const index = try indexManifests(arena, io, fixture.manifest_dir);
    var diagnostic: SelectDiagnostic = .{};
    const selection = try select(arena, io, fixture.layout(), fixture.manifest_dir, &index, &.{.{ .kind = .c, .digest = config }}, &diagnostic);

    try testing.expectEqual(@as(usize, 2), selection.entries.count());
    try testing.expect(selection.entries.contains(.{ .kind = .c, .digest = config }));
    try testing.expect(selection.entries.contains(.{ .kind = .o, .digest = written.* }));
    try testing.expectEqual(@as(usize, 1), selection.manifests.count());
}

test "a root that no manifest produced is an error" {
    var fixture: TestCache = undefined;
    try fixture.init();
    defer fixture.deinit();

    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();
    const index = try indexManifests(arena, testing.io, fixture.manifest_dir);
    var diagnostic: SelectDiagnostic = .{};
    const root: Entry = .{ .kind = .o, .digest = "0123456789abcdef0123456789abcdef".* };
    try testing.expectError(error.RootWithoutManifest, select(arena, testing.io, fixture.layout(), fixture.manifest_dir, &index, &.{root}, &diagnostic));
    try testing.expectEqual(root, diagnostic.entry.?);
}

test "locate tells cache entries, installed files and everything else apart" {
    const sep = std.fs.path.sep_str;
    const root = if (@import("builtin").os.tag == .windows) "C:\\w" else "/w";
    const layout: Layout = .{
        .prefixes = .{
            .cwd = root,
            .zig_lib = root ++ sep ++ "lib",
            .local_cache = root ++ sep ++ ".zig-cache",
            .global_cache = root ++ sep ++ "global",
            .build_root = root,
        },
        .install_prefix = root ++ sep ++ "zig-out",
    };
    const digest = "0123456789abcdef0123456789abcdef";
    try testing.expectEqual(
        Location{ .entry = .{ .kind = .o, .digest = digest.* } },
        try locate(layout, root ++ sep ++ ".zig-cache" ++ sep ++ "o" ++ sep ++ digest ++ sep ++ "roc"),
    );
    try testing.expectEqual(
        Location{ .entry = .{ .kind = .c, .digest = digest.* } },
        try locate(layout, root ++ sep ++ ".zig-cache" ++ sep ++ "c" ++ sep ++ digest),
    );
    try testing.expectEqualStrings("bin" ++ sep ++ "roc", (try locate(layout, root ++ sep ++ "zig-out" ++ sep ++ "bin" ++ sep ++ "roc")).installed);
    try testing.expectEqual(Location.elsewhere, try locate(layout, root ++ sep ++ "src" ++ sep ++ "main.zig"));
    try testing.expectEqual(Location.elsewhere, try locate(layout, root ++ sep ++ ".zig-cache-other" ++ sep ++ "o"));
    try testing.expectEqualStrings(
        "b" ++ sep ++ digest ++ sep ++ "builtin.zig",
        (try locate(layout, root ++ sep ++ ".zig-cache" ++ sep ++ "b" ++ sep ++ digest ++ sep ++ "builtin.zig")).cached_file,
    );
    try testing.expectEqualStrings("z" ++ sep ++ digest, (try locate(layout, root ++ sep ++ ".zig-cache" ++ sep ++ "z" ++ sep ++ digest)).cached_file);
    try testing.expectError(error.NotACacheEntry, locate(layout, root ++ sep ++ ".zig-cache" ++ sep ++ "o" ++ sep ++ "short"));
}
