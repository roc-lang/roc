//! Packs loaded for one build: the specialization keys they can serve at
//! Monotype reservation and the artifacts the object writer splices in.

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("collections");
const backend = @import("backend");
const check = @import("check");
const compile = @import("compile");
const lir = @import("lir");
const postcheck = @import("postcheck");
const ctx_mod = @import("ctx");
const RocTarget = @import("target.zig").RocTarget;

const Allocator = std.mem.Allocator;
const PackFile = backend.dev.PackFile;
const Common = postcheck.Common;
const CoreCtx = ctx_mod.CoreCtx;
const SourceEvents = @import("builtins").source_events;

/// The pack files of one compiler build, target, and optimization level,
/// under the cache root: `objects-<entry-version hash prefix>/<target>-<opt>/
/// <local|pkg>/<module identity>/<artifact key>.rpk` for a module's complete
/// pack, `partial-<key>-<digest>.rpk` beside it for each pack of offers a
/// build filed under the module, and `artifacts/<digest>.rpk` for each pack
/// of artifacts a build wrote, which the offers name by content address.
/// Offers are filed by module identity so an edited module's previous packs
/// stay beside its new one until the sweep removes them, which is what lets
/// unchanged specializations hit across an edit. Every file is a pure
/// function of its contents' producers, so two writers of one path write
/// identical bytes and rename into place without coordination.
pub const Store = struct {
    allocator: Allocator,
    roc_ctx: CoreCtx,
    root: []u8,
    verbose: bool,
    /// The entry-version hash this compiler stamps on the packs it writes and
    /// admits packs by.
    version_hash: [32]u8,

    pub const InitError = Allocator.Error || error{NoHomeDirectory};

    pub fn init(allocator: Allocator, cache_config: compile.CacheConfig, target: RocTarget, opt_name: []const u8) InitError!Store {
        const version_dir = try cache_config.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);
        const mode = try std.fmt.allocPrint(allocator, "{s}-{s}", .{ @tagName(target), opt_name });
        defer allocator.free(mode);
        const version_hash = compile.cache_versioning.entryVersionHash(.native_pack);
        const namespace = "objects-" ++ std.fmt.bytesToHex(version_hash[0..8].*, .lower);
        const root = try std.fs.path.join(allocator, &.{ version_dir, namespace, mode });
        return .{ .allocator = allocator, .roc_ctx = cache_config.roc_ctx, .root = root, .verbose = cache_config.verbose, .version_hash = version_hash };
    }

    pub fn deinit(self: *Store) void {
        self.allocator.free(self.root);
    }

    fn identityDir(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) Allocator.Error![]u8 {
        return std.fs.path.join(self.allocator, &.{ self.root, @tagName(origin), &std.fmt.bytesToHex(identity, .lower) });
    }

    fn packPath(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8) Allocator.Error![]u8 {
        const name = std.fmt.bytesToHex(key, .lower) ++ ".rpk".*;
        return std.fs.path.join(self.allocator, &.{ self.root, @tagName(origin), &std.fmt.bytesToHex(identity, .lower), &name });
    }

    /// Whether the pack for `key` is already in the store.
    pub fn has(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8) Allocator.Error!bool {
        const path = try self.packPath(origin, identity, key);
        defer self.allocator.free(path);
        return self.roc_ctx.fileExists(path);
    }

    fn programPath(self: *const Store, key: [32]u8) Allocator.Error![]u8 {
        const name = try std.fmt.allocPrint(self.allocator, "{s}.rpo", .{&std.fmt.bytesToHex(key, .lower)});
        defer self.allocator.free(name);
        return std.fs.path.join(self.allocator, &.{ self.root, "programs", name });
    }

    /// The runtime output recorded for the program `key` names, or null.
    /// An unreadable record is a miss.
    pub fn readProgram(self: *const Store, io: std.Io, key: [32]u8, allocator: Allocator) Allocator.Error!?[]u8 {
        const path = try self.programPath(key);
        defer self.allocator.free(path);
        return std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .limited(4 * 1024 * 1024 * 1024)) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => null,
        };
    }

    /// Record the runtime output of the program `key` names.
    pub fn writeProgram(self: *const Store, key: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        const path = try self.programPath(key);
        defer self.allocator.free(path);
        try self.writePath(path, bytes);
    }

    /// Stage the pack beside its final path and rename it into place. A
    /// pack already present is left alone: it holds the same bytes.
    pub fn write(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        const path = try self.packPath(origin, identity, key);
        defer self.allocator.free(path);
        try self.writePath(path, bytes);
    }

    /// Demanded CTFE procedures form an additive offer set, not a complete
    /// module pack. Content identity deduplicates identical publications without
    /// claiming the immutable complete-pack key or rewriting earlier subsets.
    /// The store's target and code-policy namespace applies to both forms.
    pub fn writePartial(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8, bytes: PackFile.Bytes) (Allocator.Error || error{PackWriteFailed})!void {
        const dir = try self.identityDir(origin, identity);
        defer self.allocator.free(dir);
        // The bytes are this compiler's own encoding, so they view.
        const view = PackFile.View.init(bytes, self.version_hash) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.MalformedPack, error.UnsupportedPackVersion => unreachable,
        };
        const digest = view.contentDigest();
        const name = try std.fmt.allocPrint(self.allocator, "partial-{s}-{s}.rpk", .{ &std.fmt.bytesToHex(key, .lower), &std.fmt.bytesToHex(digest, .lower) });
        defer self.allocator.free(name);
        const path = try std.fs.path.join(self.allocator, &.{ dir, name });
        defer self.allocator.free(path);
        try self.writePath(path, bytes);
    }

    fn artifactsDir(self: *const Store) Allocator.Error![]u8 {
        return std.fs.path.join(self.allocator, &.{ self.root, "artifacts" });
    }

    /// File a pack of artifacts under the store's artifacts directory, named
    /// by its content digest. Artifacts are addressed by content, so the
    /// directory is one for every module: a pack of offers names the
    /// artifacts it serves by address and the reader finds each in whichever
    /// pack here carries it. Every build adds one pack holding the artifacts
    /// the store lacked.
    pub fn writeArtifacts(self: *const Store, bytes: PackFile.Bytes) (Allocator.Error || error{PackWriteFailed})!void {
        const dir = try self.artifactsDir();
        defer self.allocator.free(dir);
        const view = PackFile.View.init(bytes, self.version_hash) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.MalformedPack, error.UnsupportedPackVersion => unreachable,
        };
        const digest = view.contentDigest();
        const name = try std.fmt.allocPrint(self.allocator, "{s}.rpk", .{&std.fmt.bytesToHex(digest, .lower)});
        defer self.allocator.free(name);
        const path = try std.fs.path.join(self.allocator, &.{ dir, name });
        defer self.allocator.free(path);
        try self.writePath(path, bytes);
    }

    /// Register every pack of artifacts in the store, so a lookup finds the
    /// carrier of each address the offers it admits reach.
    pub fn loadArtifacts(self: *const Store, io: std.Io, packs: *LoadedPacks) LoadedPacks.LoadError!void {
        const dir = try self.artifactsDir();
        defer self.allocator.free(dir);
        try packs.loadStoreDir(io, dir);
    }

    /// File a pack of discovery summaries (no code) under a module identity
    /// for one code-generation key. It is named apart from code packs, so a
    /// compilation that only asks whether discovery is settled reads no code.
    pub fn writeDiscovery(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        const dir = try self.identityDir(origin, identity);
        defer self.allocator.free(dir);
        var digest: [32]u8 = undefined;
        std.crypto.hash.Blake3.hash(bytes, &digest, .{});
        const name = try std.fmt.allocPrint(self.allocator, "discovery-{s}-{s}.rpk", .{ &std.fmt.bytesToHex(key, .lower), &std.fmt.bytesToHex(digest, .lower) });
        defer self.allocator.free(name);
        const path = try std.fs.path.join(self.allocator, &.{ dir, name });
        defer self.allocator.free(path);
        try self.writePath(path, bytes);
    }

    fn writePath(self: *const Store, path: []const u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        const dir = std.fs.path.dirname(path).?;
        self.roc_ctx.makePath(dir) catch |err| {
            self.reportWriteFailure("create directory", dir, bytes.len, err);
            return error.PackWriteFailed;
        };
        if (self.roc_ctx.fileExists(path)) return;
        const temp_path = self.roc_ctx.writeTempFile(self.allocator, path, bytes) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => |write_err| {
                self.reportWriteFailure("write staging file", path, bytes.len, write_err);
                return error.PackWriteFailed;
            },
        };
        defer self.allocator.free(temp_path);
        errdefer self.roc_ctx.deleteFile(temp_path) catch {};
        // Atomic replacement is intentional: a concurrent winner published
        // identical immutable bytes. Never mask other rename failures.
        self.roc_ctx.rename(temp_path, path) catch |err| {
            self.reportWriteFailure("publish staging file", temp_path, bytes.len, err);
            return error.PackWriteFailed;
        };
    }

    fn reportWriteFailure(self: *const Store, operation: []const u8, path: []const u8, byte_count: usize, err: (CoreCtx.MakePathError || CoreCtx.WriteError || CoreCtx.RenameError)) void {
        if (!self.verbose) return;
        const message = std.fmt.allocPrint(self.allocator, "Failed to {s} for object cache at {s} ({d} bytes): {}\n", .{ operation, path, byte_count, err }) catch return;
        defer self.allocator.free(message);
        self.roc_ctx.writeStderr(message) catch {};
    }

    /// The code packs filed under a module placement, from every revision
    /// of the module: offers are keyed by declaration, so an earlier
    /// revision's packs serve whatever an edit left alone.
    pub fn loadIdentity(self: *const Store, io: std.Io, packs: *LoadedPacks, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) LoadedPacks.LoadError!void {
        const dir = try self.identityDir(origin, identity);
        defer self.allocator.free(dir);
        try packs.loadStoreDir(io, dir);
    }

    /// Index the discovery summaries filed under a module placement, from
    /// every revision of the module, without reading its code packs.
    pub fn loadDiscoveryIdentity(self: *const Store, io: std.Io, packs: *LoadedPacks, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) LoadedPacks.LoadError!void {
        const dir = try self.identityDir(origin, identity);
        defer self.allocator.free(dir);
        try packs.loadDiscoveryDir(io, dir);
    }
};

test "object cache interleaved writers expose only complete final bytes" {
    const Hook = struct {
        store: *Store,
        base: CoreCtx,
        pending: ?[]const u8 = null,
        distinct: bool = false,
        observed_complete: bool = false,

        fn writeTempFile(context: ?*anyopaque, _: std.Io, allocator: Allocator, path: []const u8, bytes: []const u8) (Allocator.Error || CoreCtx.WriteError)![]u8 {
            const self: *@This() = @ptrCast(@alignCast(context.?));
            if (self.pending) |pending| {
                const staging = try self.base.writeTempFile(allocator, path, bytes);
                self.distinct = !std.mem.eql(u8, pending, staging);
                return staging;
            }
            // Suspend writer A after a partial write. Writer B finishes and
            // publishes before A resumes, without threads or timing thresholds.
            const staging = try self.base.writeTempFile(allocator, path, bytes[0..1]);
            errdefer {
                self.base.deleteFile(staging) catch {};
                allocator.free(staging);
            }
            self.pending = staging;
            defer self.pending = null;
            std.debug.assert(!self.base.fileExists(path));
            self.store.writePath(path, bytes) catch return error.IoError;
            const visible = self.base.readFile(path, allocator) catch return error.IoError;
            defer allocator.free(visible);
            self.observed_complete = std.mem.eql(u8, visible, bytes);
            try self.base.writeFile(staging, bytes);
            const after = self.base.readFile(path, allocator) catch return error.IoError;
            defer allocator.free(after);
            self.observed_complete = self.observed_complete and std.mem.eql(u8, after, bytes);
            return staging;
        }
    };
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    const base = CoreCtx.default(allocator, allocator, io);
    var store = Store{ .allocator = allocator, .roc_ctx = base, .root = try allocator.dupe(u8, root), .verbose = false, .version_hash = PackFile.test_version_hash };
    defer store.deinit();
    var hook = Hook{ .store = &store, .base = base };
    store.roc_ctx.ctx = &hook;
    store.roc_ctx.vtable.writeTempFile = &Hook.writeTempFile;
    const identity = @as([32]u8, @splat(1));
    const key = @as([32]u8, @splat(2));
    try store.write(.local, identity, key, "whole completed pack");
    try std.testing.expect(hook.distinct);
    try std.testing.expect(hook.observed_complete);
    hook.distinct = false;
    hook.observed_complete = false;
    const subset = try testPackBytes(allocator, store.version_hash, 1);
    defer allocator.free(subset);
    try store.writePartial(.local, identity, key, subset);
    try std.testing.expect(hook.distinct);
    try std.testing.expect(hook.observed_complete);
    const dir_path = try store.identityDir(.local, identity);
    defer allocator.free(dir_path);
    var dir = try std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true });
    defer dir.close(io);
    var iterator = dir.iterate();
    var count: usize = 0;
    while (try iterator.next(io)) |entry| {
        try std.testing.expect(std.mem.endsWith(u8, entry.name, ".rpk"));
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 2), count);
}

/// A pack with no artifacts and one settled discovery entry keyed by `tag`,
/// so two tags give two distinct packs.
fn testPackBytes(allocator: Allocator, version_hash: [32]u8, tag: u8) PackFile.WriteError!PackFile.Bytes {
    var empty = backend.dev.ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &.{} };
    defer empty.deinit();
    return PackFile.writeOffers(allocator, &empty, &.{}, &.{.{ .key = @splat(tag), .platform_requirement_relation = null }}, &.{}, version_hash);
}

test "partial object publications are additive and do not occupy the complete key" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var store = Store{
        .allocator = allocator,
        .version_hash = PackFile.test_version_hash,
        .roc_ctx = CoreCtx.default(allocator, allocator, io),
        .root = try allocator.dupe(u8, root),
        .verbose = false,
    };
    defer store.deinit();
    const identity = @as([32]u8, @splat(1));
    const key = @as([32]u8, @splat(2));
    const first = try testPackBytes(allocator, store.version_hash, 1);
    defer allocator.free(first);
    try store.writePartial(.local, identity, key, first);
    try store.writePartial(.local, identity, key, first);
    try std.testing.expect(!try store.has(.local, identity, key));
    const dir_path = try store.identityDir(.local, identity);
    defer allocator.free(dir_path);
    var dir = try std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true });
    defer dir.close(io);
    var iterator = dir.iterate();
    var count: usize = 0;
    while (try iterator.next(io)) |_| count += 1;
    try std.testing.expectEqual(@as(usize, 1), count);

    const expanded = try testPackBytes(allocator, store.version_hash, 2);
    defer allocator.free(expanded);
    try store.writePartial(.local, identity, key, expanded);
    try store.write(.local, identity, key, "complete module");
    try std.testing.expect(try store.has(.local, identity, key));
    const path = try store.packPath(.local, identity, key);
    defer allocator.free(path);
    const bytes = try store.roc_ctx.readFile(path, allocator);
    defer allocator.free(bytes);
    try std.testing.expectEqualStrings("complete module", bytes);
    iterator = dir.iterate();
    count = 0;
    while (try iterator.next(io)) |_| count += 1;
    try std.testing.expectEqual(@as(usize, 3), count);
}

test "object cache loading owns decoded packs and commits both indexes atomically" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const identity = lir.ProcIdentity.forTest(43);
    const key = @as([32]u8, @splat(41));
    const set = backend.dev.ProcArtifact.Set{
        .arena = @import("base").SingleThreadArena.init(allocator),
        .artifacts = &.{.{
            .kind = .{ .proc = identity },
            .code = "\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
            .callable_contract = @as([32]u8, @splat(1)),
        }},
    };
    const bytes = try PackFile.write(allocator, &set, &.{.{
        .key = key,
        .artifact = 0,
        // This test producer is a return instruction with no storage reads.
        .demand = .{ .complete = true },
        .rc_borrowed_params = 1,
        .rc_ret_borrowed = false,
        .rc_ret_lenders = 0,
        .rc_read_only_params = 0,
        .rc_ret_unique = false,
        .rc_ret_unique_fields = 0,
        .rc_ret_conditions = &.{},
    }}, PackFile.test_version_hash);
    defer allocator.free(bytes);
    const incompatible = try allocator.dupe(u8, bytes);
    defer allocator.free(incompatible);
    incompatible[16] +%= 1;
    const Attempt = struct {
        fn run(failing: Allocator, dir_path: []const u8, expected_key: [32]u8, expected_identity: lir.ProcIdentity) (LoadedPacks.LoadError || error{ TestExpectedEqual, TestUnexpectedResult, TestExpectedError, TestUnexpectedError })!void {
            var packs = LoadedPacks.init(failing, PackFile.test_version_hash);
            defer packs.deinit();
            packs.loadDirInto(std.testing.io, dir_path, .cache_offers) catch |err|
                return rejected(&packs, expected_key, expected_identity, err);
            try std.testing.expectEqual(@as(usize, 1), packs.packs.items.len);
            packs.indexPacks() catch |err|
                return rejected(&packs, expected_key, expected_identity, err);
            const hit = packs.specCacheLookup().lookup(expected_key, null) orelse return error.TestUnexpectedResult;
            try std.testing.expectEqualSlices(u8, &expected_identity.bytes, &hit.identity);
            try std.testing.expectEqual(@as(u64, 1), hit.rc_borrowed_params);
            try std.testing.expect(packs.spliceSource().find(packs.spliceSource().context, expected_identity) != null);
        }

        fn rejected(packs: *LoadedPacks, key_to_find: [32]u8, identity_to_find: lir.ProcIdentity, err: LoadedPacks.LoadError) (LoadedPacks.LoadError || error{ TestExpectedEqual, TestUnexpectedResult, TestExpectedError, TestUnexpectedError })!void {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expectEqual(err, packs.failure.?);
            try std.testing.expect(packs.state == .unavailable);
            try std.testing.expectError(err, packs.indexPacks());
            try std.testing.expect(packs.specCacheLookup().lookup(key_to_find, null) == null);
            const splice = packs.spliceSource();
            try std.testing.expect(splice.find(splice.context, identity_to_find) == null);
            try std.testing.expectEqual(@as(u64, 0), packs.hits);
            try std.testing.expectEqual(@as(u64, 0), packs.artifacts_served);
            return err;
        }
    };
    for ([_][]const u8{ "old-first", "old-last" }, 0..) |dir_name, order| {
        try tmp.dir.createDirPath(io, dir_name);
        const current_path = try std.fs.path.join(allocator, &.{ dir_name, if (order == 0) "z-current.rpk" else "a-current.rpk" });
        defer allocator.free(current_path);
        const old_path = try std.fs.path.join(allocator, &.{ dir_name, if (order == 0) "a-old.rpk" else "z-old.rpk" });
        defer allocator.free(old_path);
        try tmp.dir.writeFile(io, .{ .sub_path = current_path, .data = bytes });
        try tmp.dir.writeFile(io, .{ .sub_path = old_path, .data = incompatible });
        const dir_path = try tmp.dir.realPathFileAlloc(io, dir_name, allocator);
        defer allocator.free(dir_path);
        // In-place growth under the testing allocator depends on the heap's
        // layout, so the failure check runs over an allocator that refuses it.
        var deterministic = collections.DeterministicAllocator.init(allocator);
        try std.testing.checkAllAllocationFailures(deterministic.allocator(), Attempt.run, .{ dir_path, key, identity });
        try std.testing.expectError(error.UnsupportedPackVersion, LoadedPacks.loadDir(allocator, io, dir_path, PackFile.test_version_hash));
    }
    try tmp.dir.createDirPath(io, "malformed");
    try tmp.dir.writeFile(io, .{ .sub_path = "malformed/broken.rpk", .data = "not a pack" });
    const malformed_path = try tmp.dir.realPathFileAlloc(io, "malformed", allocator);
    defer allocator.free(malformed_path);
    var malformed = LoadedPacks.init(allocator, PackFile.test_version_hash);
    defer malformed.deinit();
    try std.testing.expectError(error.MalformedPack, malformed.loadDirInto(io, malformed_path, .cache_offers));
    try std.testing.expectEqual(error.MalformedPack, malformed.failure.?);
    try std.testing.expect(malformed.specCacheLookup().lookup(key, null) == null);
    try std.testing.expect(malformed.spliceSource().find(malformed.spliceSource().context, identity) == null);
}

test "object cache semantic offers select compatible relations in either order" {
    const key = @as([32]u8, @splat(21));
    const independent_key = @as([32]u8, @splat(22));
    const mixed_key = @as([32]u8, @splat(23));
    const first_relation = @as([32]u8, @splat(31));
    const second_relation = @as([32]u8, @splat(32));
    const Attempt = struct {
        fn run(allocator: Allocator, reverse: bool) (LoadedPacks.LoadError || PackFile.WriteError || error{ TestExpectedEqual, TestUnexpectedResult })!void {
            var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
            defer packs.deinit();
            for (0..3) |position| {
                const index = if (reverse) 2 - position else position;
                const relation: ?[32]u8 = switch (index) {
                    0 => first_relation,
                    1 => second_relation,
                    else => null,
                };
                const identity = lir.ProcIdentity.forTest(@intCast(51 + index));
                const set = backend.dev.ProcArtifact.Set{
                    .arena = @import("base").SingleThreadArena.init(allocator),
                    .artifacts = &.{.{
                        .kind = .{ .proc = identity },
                        .code = "\xc3",
                        .entry = 0,
                        .frame = null,
                        .refs = &.{},
                        .relocations = &.{},
                        .data = &.{},
                        .callable_contract = @as([32]u8, @splat(1)),
                    }},
                };
                const spec: PackFile.SpecEntry = .{
                    .key = if (relation == null) independent_key else key,
                    .artifact = 0,
                    .platform_requirement_relation = relation,
                    .demand = .{ .complete = true },
                    .rc_borrowed_params = 1,
                    .rc_ret_borrowed = false,
                    .rc_ret_lenders = 0,
                    .rc_read_only_params = 1,
                    .rc_ret_unique = false,
                    .rc_ret_unique_fields = 0,
                    .rc_ret_conditions = &.{0x0001_02ff},
                };
                var specs = [_]PackFile.SpecEntry{ spec, spec };
                specs[1].key = mixed_key;
                const bytes = try PackFile.write(allocator, &set, &specs, PackFile.test_version_hash);
                defer allocator.free(bytes);
                try packs.appendPack(bytes);
            }
            try packs.indexPacks();
            const lookup = packs.specCacheLookup();
            for ([_][32]u8{ first_relation, second_relation }, 0..) |relation, index| {
                const hit = lookup.lookup(key, relation) orelse return error.TestUnexpectedResult;
                try std.testing.expectEqualDeep(relation, hit.platform_requirement_relation.?);
                try std.testing.expectEqualSlices(u8, &lir.ProcIdentity.forTest(@intCast(51 + index)).bytes, &hit.identity);
                try std.testing.expectEqualSlices(u32, &.{0x0001_02ff}, hit.rc_ret_conditions);
            }
            // Independent offers are still visible beside incompatible ones.
            const mixed = lookup.lookup(mixed_key, @as([32]u8, @splat(33))) orelse return error.TestUnexpectedResult;
            try std.testing.expect(mixed.platform_requirement_relation == null);
            try std.testing.expectEqualSlices(u8, &lir.ProcIdentity.forTest(53).bytes, &mixed.identity);
            const exact = lookup.lookup(mixed_key, second_relation) orelse return error.TestUnexpectedResult;
            try std.testing.expectEqualDeep(second_relation, exact.platform_requirement_relation.?);
            try std.testing.expect(lookup.lookup(key, null) == null);
            try std.testing.expect(lookup.lookup(key, @as([32]u8, @splat(33))) == null);
            for ([_]?[32]u8{ null, first_relation, second_relation }) |relation| {
                const hit = lookup.lookup(independent_key, relation) orelse return error.TestUnexpectedResult;
                try std.testing.expect(hit.platform_requirement_relation == null);
                try std.testing.expectEqualSlices(u8, &lir.ProcIdentity.forTest(53).bytes, &hit.identity);
            }
            try std.testing.expectEqual(@as(u64, 7), packs.hits);
        }
    };
    var deterministic = collections.DeterministicAllocator.init(std.testing.allocator);
    for ([_]bool{ false, true }) |reverse| {
        try std.testing.checkAllAllocationFailures(deterministic.allocator(), Attempt.run, .{reverse});
    }
}

test "normal native offers attest one exact physical definition and fixed metadata" {
    const allocator = std.testing.allocator;
    const identity = lir.ProcIdentity.forTest(71);
    const first_key = @as([32]u8, @splat(72));
    const second_key = @as([32]u8, @splat(73));
    const Producer = struct {
        fn bytes(a: Allocator, proc_identity: lir.ProcIdentity, key: [32]u8, code: []const u8, borrowed: u64) PackFile.WriteError!PackFile.Bytes {
            const set = backend.dev.ProcArtifact.Set{
                .arena = collections.SingleThreadArena.init(a),
                .artifacts = &.{.{
                    .kind = .{ .proc = proc_identity },
                    .code = code,
                    .entry = 0,
                    .frame = null,
                    .refs = &.{},
                    .relocations = &.{},
                    .data = &.{},
                    .callable_contract = @as([32]u8, @splat(1)),
                }},
            };
            return PackFile.write(a, &set, &.{.{
                .key = key,
                .artifact = 0,
                .demand = .{ .complete = true },
                .rc_borrowed_params = borrowed,
                .rc_ret_borrowed = false,
                .rc_ret_lenders = 0,
                .rc_read_only_params = 0,
                .rc_ret_unique = false,
                .rc_ret_unique_fields = 0,
                .rc_ret_conditions = &.{},
            }}, PackFile.test_version_hash);
        }
    };
    const first_bytes = try Producer.bytes(allocator, identity, first_key, "\xc3", 0);
    defer allocator.free(first_bytes);
    for ([_]struct { code: []const u8, borrowed: u64, accepted: bool }{
        .{ .code = "\xc3", .borrowed = 0, .accepted = true },
        .{ .code = "\x90\xc3", .borrowed = 0, .accepted = false },
        .{ .code = "\xc3", .borrowed = 1, .accepted = false },
    }) |candidate| {
        var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
        defer packs.deinit();
        try packs.retain(first_bytes);
        const splice = packs.spliceSource();
        const first = splice.find(splice.context, identity).?;
        const next = try Producer.bytes(allocator, identity, second_key, candidate.code, candidate.borrowed);
        defer allocator.free(next);
        try packs.retain(next);
        try std.testing.expectEqual(candidate.accepted, packs.specCacheLookup().lookup(second_key, null) != null);
        const selected = splice.find(splice.context, identity).?;
        try std.testing.expect(first.set == selected.set);
        try std.testing.expectEqual(first.index, selected.index);
        try std.testing.expectEqualStrings("\xc3", selected.set.artifacts[selected.index].code);
        try std.testing.expectEqual(@as(u64, 0), packs.specCacheLookup().lookup(first_key, null).?.rc_borrowed_params);
    }
}

test "identity offers serve lookups by identity for the relation they depend on" {
    const allocator = std.testing.allocator;
    const callee_identity = lir.ProcIdentity.forTest(81);
    const caller_identity = lir.ProcIdentity.forTest(82);
    const relation: [32]u8 = @splat(83);
    const Producer = struct {
        fn bytes(a: Allocator, callee: lir.ProcIdentity, caller: lir.ProcIdentity, caller_relation: ?[32]u8) PackFile.WriteError!PackFile.Bytes {
            const leaf = backend.dev.ProcArtifact.Artifact{
                .kind = .{ .proc = callee },
                .code = "\xc3",
                .entry = 0,
                .frame = null,
                .refs = &.{},
                .relocations = &.{},
                .data = &.{},
                .callable_contract = @as([32]u8, @splat(1)),
                .context_contract = .{ .target = .x64musl, .cpu_level = .default, .hot_reload = false, .default_platform_runtime = false },
                .context_dependencies = .{},
            };
            var head = leaf;
            head.kind = .{ .proc = caller };
            head.code = "\xe8\x00\x00\x00\x00\xc3";
            head.refs = &.{.{ .site = 1, .form = .call, .target = 0, .delta = 0, .veneer = null }};
            const set = backend.dev.ProcArtifact.Set{
                .arena = collections.SingleThreadArena.init(a),
                .artifacts = &.{ leaf, head },
            };
            const facts = PackFile.SpecEntry{
                .key = callee.bytes,
                .by_identity = true,
                .artifact = 0,
                .demand = .{ .complete = true },
                .rc_borrowed_params = 0,
                .rc_ret_borrowed = false,
                .rc_ret_lenders = 0,
                .rc_read_only_params = 0,
                .rc_ret_unique = false,
                .rc_ret_unique_fields = 0,
                .rc_ret_conditions = &.{},
            };
            var caller_offer = facts;
            caller_offer.key = caller.bytes;
            caller_offer.artifact = 1;
            caller_offer.platform_requirement_relation = caller_relation;
            caller_offer.rc_borrowed_params = 1;
            return PackFile.write(a, &set, &.{ facts, caller_offer }, PackFile.test_version_hash);
        }
    };
    const bytes = try Producer.bytes(allocator, callee_identity, caller_identity, relation);
    var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
    defer packs.deinit();
    // A store pack is registered by its offers alone.
    try packs.addSource(bytes);
    packs.state = .ready;
    try std.testing.expectEqual(@as(usize, 0), packs.packs.items.len);
    const lookup = packs.specCacheLookup();
    // Nothing is offered under these bytes as a specialization key.
    try std.testing.expect(lookup.lookup(caller_identity.bytes, relation) == null);
    // The caller's code depends on the relation; the callee's does not.
    try std.testing.expect(lookup.lookupIdentity(caller_identity.bytes, null) == null);
    try std.testing.expect(lookup.lookupIdentity(caller_identity.bytes, @splat(84)) == null);
    const caller_hit = lookup.lookupIdentity(caller_identity.bytes, relation) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualSlices(u8, &caller_identity.bytes, &caller_hit.identity);
    try std.testing.expectEqual(@as(u64, 1), caller_hit.rc_borrowed_params);
    // Admitting the caller's closure admitted the callee, which serves on its own.
    try std.testing.expectEqual(@as(usize, 1), packs.packs.items.len);
    try std.testing.expectEqual(@as(usize, 2), packs.packs.items[0].set.artifacts.len);
    const callee_hit = lookup.lookupIdentity(callee_identity.bytes, null) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 0), callee_hit.rc_borrowed_params);
    try std.testing.expectEqual(@as(u64, 2), packs.identity_hits);
    // The splice finds both by identity.
    const splice = packs.spliceSource();
    try std.testing.expect(splice.find(splice.context, caller_identity) != null);
    try std.testing.expect(splice.find(splice.context, callee_identity) != null);
    try std.testing.expect(splice.find(splice.context, lir.ProcIdentity.forTest(85)) == null);
}

test "normal native retained objects keep borrowed exact graphs stable while the universe grows" {
    const allocator = std.testing.allocator;
    var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
    defer packs.deinit();
    var first: ?backend.dev.LocatedArtifact = null;
    for (0..32) |index| {
        const identity = lir.ProcIdentity.forTest(@intCast(index + 1));
        var artifacts = [_]backend.dev.ProcArtifact.Artifact{ .{
            .kind = .{ .proc = identity },
            .code = "\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
            .callable_contract = @as([32]u8, @splat(1)),
        }, .{
            .kind = .{ .proc = lir.ProcIdentity.forTest(100) },
            .code = "\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{},
        } };
        if (index == 0) {
            artifacts[0].code = "\xe8\x00\x00\x00\x00\xc3";
            artifacts[0].refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
            artifacts[0].context_contract = .{ .target = .x64musl, .cpu_level = .default, .hot_reload = false, .default_platform_runtime = false };
        }
        const set = backend.dev.ProcArtifact.Set{
            .arena = collections.SingleThreadArena.init(allocator),
            .artifacts = if (index == 0) &artifacts else artifacts[0..1],
        };
        const bytes = try PackFile.write(allocator, &set, &.{.{
            .key = @as([32]u8, @splat(@intCast(index + 1))),
            .artifact = 0,
            .demand = .{ .complete = true },
            .rc_borrowed_params = 0,
            .rc_ret_borrowed = false,
            .rc_ret_lenders = 0,
            .rc_read_only_params = 0,
            .rc_ret_unique = false,
            .rc_ret_unique_fields = 0,
            .rc_ret_conditions = &.{},
        }}, PackFile.test_version_hash);
        defer allocator.free(bytes);
        try packs.retain(bytes);
        const splice = packs.spliceSource();
        const retained = splice.find(splice.context, identity).?;
        if (index == 0) first = retained;
        const original = splice.find(splice.context, lir.ProcIdentity.forTest(1)).?;
        try std.testing.expect(original.set == first.?.set);
        try std.testing.expectEqual(first.?.index, original.index);
        const caller = first.?.set.artifacts[first.?.index];
        try std.testing.expectEqualStrings("\xe8\x00\x00\x00\x00\xc3", caller.code);
        const callee = first.?.set.artifacts[caller.refs[0].target];
        try std.testing.expectEqualDeep(lir.ProcIdentity.forTest(100), callee.kind.proc);
        try std.testing.expectEqualStrings("\xc3", callee.code);
    }
    try std.testing.expectEqual(@as(usize, 32), packs.indexed_count);
    try std.testing.expectEqual(@as(u32, 32), packs.specs.count());
}

test "normal native source certificates reload only against current checked authority" {
    const allocator = std.testing.allocator;
    var env = try @import("can").ModuleEnv.init(allocator, "hello");
    defer env.deinit();
    // This validator consumes checked source authority, not typed bodies.
    var owner: check.CheckedArtifact.CheckedModuleArtifact = undefined;
    owner.key = .{ .bytes = @splat(31), .module_identity_hash = @splat(32) };
    owner.module_env = .{ .checked_source = &env };
    const identity = lir.ProcIdentity.forTest(33);
    const descriptor = SourceEvents.Descriptor{
        .header = .{ .kind = @backingInt(SourceEvents.Kind.call_enter) },
        .source = .{
            .checked_module = owner.key.bytes,
            .source_identity = owner.key.module_identity_hash,
            .region = .{ .start = 0, .end = 5 },
            .line = 1,
            .column = 1,
            .has_location = 1,
        },
        .procedure_identity = identity.bytes,
    };
    var event_data = @as([@sizeOf(SourceEvents.Descriptor)]u8, @splat(0));
    backend.dev.LirCodeGenMod.encodeSourceEventField(SourceEvents.Descriptor, descriptor, &event_data, .little);
    const set = backend.dev.ProcArtifact.Set{
        .arena = collections.SingleThreadArena.init(allocator),
        .artifacts = &.{.{
            .kind = .{ .proc = identity },
            .code = "\xc3",
            .entry = 0,
            .frame = null,
            .refs = &.{},
            .relocations = &.{},
            .data = &.{.{
                .name = "event",
                .bytes = &event_data,
                .alignment = @alignOf(SourceEvents.Descriptor),
                .symbol_offset = 0,
                .relocations = &.{},
            }},
            .callable_contract = @as([32]u8, @splat(34)),
            .context_contract = .{
                .target = .x64linux,
                .cpu_level = .default,
                .hot_reload = false,
                .default_platform_runtime = false,
            },
            .source_events = &.{.{
                .data_name = "event",
                .descriptor = descriptor,
                .branch_regions = &.{},
            }},
        }},
    };
    const key = @as([32]u8, @splat(35));
    const bytes = try PackFile.write(allocator, &set, &.{.{
        .key = key,
        .artifact = 0,
        .demand = .{ .complete = true },
        .rc_borrowed_params = 0,
        .rc_ret_borrowed = false,
        .rc_ret_lenders = 0,
        .rc_read_only_params = 0,
        .rc_ret_unique = false,
        .rc_ret_unique_fields = 0,
        .rc_ret_conditions = &.{},
    }}, PackFile.test_version_hash);
    defer allocator.free(bytes);
    for ([_]bool{ false, true }) |stale| {
        var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
        defer packs.deinit();
        owner.key.module_identity_hash = if (stale) @splat(36) else @splat(32);
        packs.checked_root = &owner;
        try packs.appendPack(bytes);
        try packs.indexPacks();
        try std.testing.expectEqual(!stale, packs.specCacheLookup().lookup(key, null) != null);
        const splice = packs.spliceSource();
        const object = splice.find(splice.context, identity);
        try std.testing.expectEqual(!stale, object != null);
        if (object) |located| {
            try std.testing.expectEqualStrings("\xc3", located.set.artifacts[located.index].code);
            try std.testing.expectEqualDeep(descriptor, located.set.artifacts[located.index].source_events[0].descriptor);
            // Retiring the declaration cannot invalidate already-pinned facts.
            packs.checked_root = null;
            owner.module_env = undefined;
            try packs.retain(bytes);
            const again = splice.find(splice.context, identity).?;
            try std.testing.expect(located.set == again.set);
            owner.module_env = .{ .checked_source = &env };
        }
    }
}

test "normal native site and literal certificates attest exact checked semantic records" {
    const allocator = std.testing.allocator;
    const Checked = check.CheckedArtifact;
    const Region = @import("base").Region;
    var env = try @import("can").ModuleEnv.init(allocator, "hello");
    defer env.deinit();
    var owner: Checked.CheckedModuleArtifact = undefined;
    owner.key = .{ .bytes = @splat(81), .module_identity_hash = @splat(82) };
    owner.module_env = .{ .checked_source = &env };
    owner.checked_bodies = .{};
    defer owner.checked_bodies.deinit(allocator);
    // No checked type store backs these records; certification never reads their types.
    const fixture_ty: Checked.CheckedTypeId = @fromBackingInt(@intCast(7));
    const pattern: Checked.CheckedPatternId = @fromBackingInt(@intCast(owner.checked_bodies.stored_patterns.items.len));
    try owner.checked_bodies.stored_patterns.append(allocator, .{
        .id = pattern,
        .ty = fixture_ty,
        .source_region = Region.from_raw_offsets(1, 2),
        .data = .underscore,
    });
    var quotes: [2]Checked.CheckedExprId = undefined;
    for (&quotes, 0..) |*quote, index| {
        quote.* = @fromBackingInt(@intCast(owner.checked_bodies.stored_exprs.items.len));
        try owner.checked_bodies.stored_exprs.append(allocator, .{
            .id = quote.*,
            .ty = fixture_ty,
            .source_region = Region.from_raw_offsets(@intCast(index + 1), @intCast(index + 2)),
            .data = .{ .str_from_quote = .{ .literal = @fromBackingInt(@intCast(3)), .plan = @fromBackingInt(@intCast(4)) } },
        });
    }
    try owner.checked_bodies.if_branch_pool.append(allocator, .{ .cond = quotes[0], .body = quotes[1] });
    const if_expr: Checked.CheckedExprId = @fromBackingInt(@intCast(owner.checked_bodies.stored_exprs.items.len));
    try owner.checked_bodies.stored_exprs.append(allocator, .{
        .id = if_expr,
        .ty = fixture_ty,
        .source_region = Region.from_raw_offsets(0, 5),
        .data = .{ .if_ = .{
            .branches = .{ .start = 0, .len = 1 },
            .final_else = quotes[1],
            .warn_unused_branches = true,
        } },
    });
    var sites = [_]Checked.CheckedExhaustivenessSite{.{
        .id = undefined,
        .kind = .destructure,
        .region = Region.from_raw_offsets(0, 5),
        .checked_pattern = pattern,
        .policy = .not_pending,
    }};
    for (&sites, 0..) |*site, index| site.id = @fromBackingInt(@intCast(index));
    owner.exhaustiveness_sites = .{ .sites = &sites };
    var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
    defer packs.deinit();
    packs.checked_root = &owner;
    var certificate = backend.dev.ProcArtifact.SourceEventCertificate{
        .data_name = "site",
        .site_origin = .{ .checked_pattern = @backingInt(pattern) },
        .descriptor = .{
            .header = .{
                .kind = @backingInt(SourceEvents.Kind.branch_taken),
                .byte_size = @sizeOf(SourceEvents.Descriptor) + @sizeOf(SourceEvents.Region),
            },
            .site = .{
                .checked_module = owner.key.bytes,
                .procedure_identity = lir.ProcIdentity.forTest(83).bytes,
                .checked_site = @backingInt(sites[0].id),
                .has_checked_site = 1,
                .kind = @backingInt(SourceEvents.SiteKind.destructure),
                .region = .{ .start = 0, .end = 5 },
                .branches_offset = @sizeOf(SourceEvents.Descriptor),
                .branch_count = 1,
            },
        },
        .branch_regions = &.{Region.from_raw_offsets(1, 2)},
    };
    const artifact = backend.dev.ProcArtifact.Artifact{
        .kind = .{ .proc = lir.ProcIdentity.forTest(83) },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .source_events = (&certificate)[0..1],
    };
    try packs.pinAuthority(certificate);
    try std.testing.expect(LoadedPacks.normalNode(&packs, artifact));
    certificate.descriptor.site.kind = @backingInt(SourceEvents.SiteKind.match);
    try std.testing.expect(!LoadedPacks.normalNode(&packs, artifact));
    certificate.descriptor.site.kind = @backingInt(SourceEvents.SiteKind.destructure);
    certificate.branch_regions = &.{Region.from_raw_offsets(2, 3)};
    try std.testing.expect(!LoadedPacks.normalNode(&packs, artifact));
    certificate.branch_regions = &.{Region.from_raw_offsets(1, 2)};
    certificate.descriptor.site.region.start = 1;
    try std.testing.expect(!LoadedPacks.normalNode(&packs, artifact));
    certificate.descriptor.site.region.start = 0;
    certificate.descriptor.site.has_checked_site = 0;
    certificate.descriptor.site.kind = @backingInt(SourceEvents.SiteKind.if_);
    certificate.descriptor.site.branch_count = 2;
    certificate.descriptor.header.byte_size = @sizeOf(SourceEvents.Descriptor) + 2 * @sizeOf(SourceEvents.Region);
    certificate.site_origin = .{ .checked_expr = @backingInt(if_expr) };
    certificate.branch_regions = &.{ Region.from_raw_offsets(1, 2), Region.from_raw_offsets(2, 3) };
    try packs.pinAuthority(certificate);
    try std.testing.expect(LoadedPacks.normalNode(&packs, artifact));
    certificate.site_origin = .{ .checked_expr = @backingInt(quotes[1]) };
    try packs.pinAuthority(certificate);
    try std.testing.expect(!LoadedPacks.normalNode(&packs, artifact));
    certificate.descriptor = .{
        .header = .{ .kind = @backingInt(SourceEvents.Kind.failure) },
        .source = .{
            .checked_module = owner.key.bytes,
            .source_identity = owner.key.module_identity_hash,
            .has_location = 1,
            .region = .{ .start = 1, .end = 2 },
            .line = 1,
            .column = 2,
        },
        .failure = .{
            .has_literal_rejection = 1,
            .literal_rejection = .{ .checked_module = owner.key.bytes, .checked_expr = @backingInt(quotes[0]), .kind = @backingInt(SourceEvents.LiteralKind.quote) },
        },
    };
    certificate.site_origin = null;
    certificate.branch_regions = &.{};
    try packs.pinAuthority(certificate);
    try std.testing.expect(LoadedPacks.normalNode(&packs, artifact));
    certificate.descriptor.failure.literal_rejection.checked_expr = @backingInt(quotes[1]);
    try packs.pinAuthority(certificate);
    try std.testing.expect(!LoadedPacks.normalNode(&packs, artifact));
    certificate.descriptor.failure.literal_rejection.checked_expr = @backingInt(quotes[0]);
    packs.checked_root = null;
    owner.checked_bodies.stored_exprs.items[0].source_region = Region.from_raw_offsets(3, 4);
    try std.testing.expect(LoadedPacks.normalNode(&packs, artifact));
}

test "object cache write failures preserve quiet behavior and report verbose causes" {
    const Capture = struct {
        stderr: std.ArrayList(u8) = .empty,
        stage: enum { directory, write, rename },
        deleted: usize = 0,

        fn makePath(context: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.MakePathError!void {
            const self: *@This() = @ptrCast(@alignCast(context.?));
            if (self.stage == .directory) return error.ReadOnlyFileSystem;
        }

        fn fileExists(_: ?*anyopaque, _: std.Io, _: []const u8) bool {
            return false;
        }

        fn writeTempFile(context: ?*anyopaque, _: std.Io, allocator: Allocator, path: []const u8, _: []const u8) (Allocator.Error || CoreCtx.WriteError)![]u8 {
            const self: *@This() = @ptrCast(@alignCast(context.?));
            if (self.stage == .write) return error.NoSpaceLeft;
            return std.fmt.allocPrint(allocator, "{s}.owned.tmp", .{path});
        }

        fn deleteFile(context: ?*anyopaque, _: std.Io, path: []const u8) CoreCtx.DeleteError!void {
            const self: *@This() = @ptrCast(@alignCast(context.?));
            std.debug.assert(std.mem.endsWith(u8, path, ".owned.tmp"));
            self.deleted += 1;
        }

        fn rename(context: ?*anyopaque, _: std.Io, _: []const u8, _: []const u8) CoreCtx.RenameError!void {
            const self: *@This() = @ptrCast(@alignCast(context.?));
            if (self.stage == .rename) return error.DiskQuota;
        }

        fn writeStderr(context: ?*anyopaque, _: std.Io, bytes: []const u8) CoreCtx.StdioError!void {
            const self: *@This() = @ptrCast(@alignCast(context.?));
            self.stderr.appendSlice(std.testing.allocator, bytes) catch return error.IoError;
        }
    };
    const allocator = std.testing.allocator;
    for ([_]bool{ false, true }) |verbose| {
        for (std.enums.values(@FieldType(Capture, "stage"))) |stage| {
            var capture = Capture{ .stage = stage };
            defer capture.stderr.deinit(allocator);
            var filesystem = CoreCtx.testing(allocator, allocator);
            filesystem.ctx = &capture;
            filesystem.vtable.makePath = &Capture.makePath;
            filesystem.vtable.fileExists = &Capture.fileExists;
            filesystem.vtable.writeTempFile = &Capture.writeTempFile;
            filesystem.vtable.deleteFile = &Capture.deleteFile;
            filesystem.vtable.rename = &Capture.rename;
            filesystem.vtable.writeStderr = &Capture.writeStderr;
            var store = try Store.init(allocator, .{
                .cache_dir = "cache",
                .roc_ctx = filesystem,
                .verbose = verbose,
            }, .arm64mac, "dev");
            defer store.deinit();
            const pack = try testPackBytes(allocator, store.version_hash, 1);
            defer allocator.free(pack);
            for (0..2) |_| {
                try std.testing.expectError(error.PackWriteFailed, store.write(.local, @as([32]u8, @splat(1)), @as([32]u8, @splat(2)), "pack"));
                try std.testing.expectError(error.PackWriteFailed, store.writePartial(.local, @as([32]u8, @splat(1)), @as([32]u8, @splat(2)), pack));
            }
            try std.testing.expectEqual(@as(usize, if (stage == .rename) 4 else 0), capture.deleted);
            if (verbose) {
                const cause = switch (stage) {
                    .directory => "error.ReadOnlyFileSystem",
                    .write => "error.NoSpaceLeft",
                    .rename => "error.DiskQuota",
                };
                try std.testing.expectEqual(@as(usize, 4), std.mem.count(u8, capture.stderr.items, cause));
                try std.testing.expectEqual(@as(usize, 2), std.mem.count(u8, capture.stderr.items, "(4 bytes)"));
                const pack_size = try std.fmt.allocPrint(allocator, "({d} bytes)", .{pack.len});
                defer allocator.free(pack_size);
                try std.testing.expectEqual(@as(usize, 2), std.mem.count(u8, capture.stderr.items, pack_size));
                try std.testing.expect(std.mem.containsAtLeast(u8, capture.stderr.items, 1, store.root));
            } else {
                try std.testing.expectEqualStrings("", capture.stderr.items);
            }
        }
    }
}

/// What a lazily loaded store needs once checking has produced the module
/// set: the store and the modules in view.
pub const Pending = struct {
    store: *const Store,
    io: std.Io,
    build_env: *compile.BuildEnv,
};

test "pack checked materializer projection uses actual pending inputs and survives source retirement" {
    const allocator = std.testing.allocator;
    const eval = @import("eval");
    var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
    defer packs.deinit();
    const declaration = authority: {
        var builtin_modules = try eval.BuiltinModules.init(allocator);
        defer builtin_modules.deinit();
        var resources = try eval.Inspected.parseProgram(allocator, .module,
            \\Test := [].{
            \\    value : U64
            \\    value = 7.U64
            \\}
            \\main = || Test.value
            \\expect Test.value == 7.U64
        , &.{}, .{ .pre_published_builtin = .{
            .env = builtin_modules.builtin_module.env,
            .indices = builtin_modules.builtin_indices,
            .artifact = &builtin_modules.checked_artifact,
        } });
        defer eval.Inspected.cleanupParseAndCanonical(allocator, resources);
        var uses = std.ArrayList(Common.CheckedRootUse).empty;
        defer uses.deinit(allocator);
        for (resources.checked_artifact.root_requests.compile_time_requests) |request| {
            const root = request.compile_time_root orelse continue;
            try uses.append(allocator, .{ .module = resources.checked_artifact.key.bytes, .root = @backingInt(root) });
        }
        try std.testing.expect(uses.items.len != 0);
        try packs.projectCheckedMaterializers(.{
            .root = check.CheckedArtifact.loweringView(&resources.checked_artifact),
            .imports = &.{check.CheckedArtifact.importedView(&builtin_modules.checked_artifact)},
        }, uses.items, lir.CheckedPipeline.TargetConfig.ordinaryDev());
        try std.testing.expect(packs.materializers.count() != 0);
        var declarations = packs.materializers.valueIterator();
        const expected = declarations.next().?.*;
        // The producer is not in Builtin's visibility and has no published
        // BuildEnv entry. Only the caller's actual prepared input contains it.
        for ([_]bool{ false, true }) |as_relation| {
            var projected = LoadedPacks.init(allocator, PackFile.test_version_hash);
            defer projected.deinit();
            const producer_views = [_]check.CheckedModule.ImportedModuleView{
                check.CheckedArtifact.importedView(&resources.checked_artifact),
            };
            projected.projection_target = lir.CheckedPipeline.TargetConfig.ordinaryDev();
            projected.projection_modules = .{
                .root = check.CheckedArtifact.loweringViewWithRelations(
                    &builtin_modules.checked_artifact,
                    if (as_relation) &producer_views else &.{},
                ),
                .imports = if (as_relation) &.{} else &producer_views,
            };
            defer projected.projection_modules = null;
            const dependency = backend.dev.ProcArtifact.RootMaterializationDependency{
                .binding_name = "binding",
                .header_name = "header",
                .materialization = expected,
            };
            var pending_pack = PackFile.Pack{
                .set = .{
                    .arena = collections.SingleThreadArena.init(allocator),
                    .artifacts = &.{.{
                        .kind = .{ .proc = lir.ProcIdentity.forTest(105) },
                        .code = "\xc3",
                        .entry = 0,
                        .frame = null,
                        .refs = &.{},
                        .relocations = &.{},
                        .data = &.{},
                        .materializations = &.{dependency},
                    }},
                },
                .specs = &.{},
                .edges = &.{&.{}},
                .unresolved = &.{false},
            };
            defer pending_pack.deinit();
            try projected.projectPendingMaterializers(&pending_pack, &.{.{
                .checked_roots = uses.items,
                .literal_reads = &.{},
                .complete = true,
            }});
            try std.testing.expectEqualDeep(expected, projected.materializers.get(expected.wrapper_identity).?);
        }
        break :authority expected;
    };
    // Every checked and type/layout owner above has now been destroyed.
    const header = try lir.Program.rootMaterializationHeaderName(allocator, declaration);
    defer allocator.free(header);
    const binding = try std.fmt.allocPrint(allocator, "{s}__address", .{header});
    defer allocator.free(binding);
    const dependency = backend.dev.ProcArtifact.RootMaterializationDependency{
        .binding_name = binding,
        .header_name = header,
        .materialization = declaration,
    };
    const artifact = backend.dev.ProcArtifact.Artifact{
        .kind = .{ .proc = lir.ProcIdentity.forTest(101) },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{.{
            .name = binding,
            .bytes = &(@as([8]u8, @splat(0))),
            .alignment = 8,
            .symbol_offset = 0,
            .relocations = &.{.{ .offset = 0, .name = header, .addend = 0, .function = false, .external = true }},
        }},
        .materializations = (&dependency)[0..1],
    };
    try backend.dev.ProcArtifact.validateRootMaterializations(allocator, artifact);
    try std.testing.expect(LoadedPacks.normalNode(&packs, artifact));
}

test "pack checked materializer demand is shared across recursive callers and rejects only its closure" {
    const allocator = std.testing.allocator;
    var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
    defer packs.deinit();
    const declaration = lir.Program.RootMaterialization{
        .module = @splat(1),
        .producer = .{ .checked = 2 },
        .representation_digest = @splat(3),
        .layout_digest = @splat(4),
        .wrapper_identity = @splat(5),
    };
    const dependency = backend.dev.ProcArtifact.RootMaterializationDependency{
        .binding_name = "binding",
        .header_name = "header",
        .materialization = declaration,
    };
    var artifacts = [_]backend.dev.ProcArtifact.Artifact{
        .{ .kind = .{ .proc = lir.ProcIdentity.forTest(102) }, .code = "\xc3", .entry = 0, .frame = null, .refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }}, .relocations = &.{}, .data = &.{} },
        .{ .kind = .{ .proc = lir.ProcIdentity.forTest(103) }, .code = "\xc3", .entry = 0, .frame = null, .refs = &.{.{ .site = 0, .form = .call, .target = 0, .delta = 0 }}, .relocations = &.{}, .data = &.{}, .materializations = &.{dependency} },
        .{ .kind = .{ .proc = lir.ProcIdentity.forTest(104) }, .code = "\xc3", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
    };
    var set = backend.dev.ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &artifacts };
    defer set.deinit();
    var closure = try backend.dev.ArtifactClosure.init(allocator, &set);
    defer closure.deinit();
    const edges = try testEdges(allocator, &closure);
    defer freeTestEdges(allocator, edges);
    const summaries = try packs.servingDemandSummaries(set.artifacts, edges, &.{}, &.{}, &.{});
    defer allocator.free(summaries);
    try std.testing.expect(summaries[0].checked_roots.ptr == summaries[1].checked_roots.ptr);
    try std.testing.expectEqual(@as(usize, 1), summaries[0].checked_roots.len);
    try std.testing.expectEqual(@as(usize, 0), summaries[2].checked_roots.len);
    const rejected = try closure.admit(&packs, LoadedPacks.normalNode);
    defer allocator.free(rejected);
    try std.testing.expectEqualSlices(bool, &.{ false, false, true }, rejected);
    try packs.pinMaterializer(declaration);
    const accepted = try closure.admit(&packs, LoadedPacks.normalNode);
    defer allocator.free(accepted);
    try std.testing.expectEqualSlices(bool, &.{ true, true, true }, accepted);
}

test "pack literal projection retains producer declarations after source retirement" {
    const allocator = std.testing.allocator;
    const eval = @import("eval");
    const checked = check.CheckedArtifact;
    var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
    defer packs.deinit();
    const declaration = authority: {
        var builtin_modules = try eval.BuiltinModules.init(allocator);
        defer builtin_modules.deinit();
        var resources = try eval.Inspected.parseProgram(allocator, .module,
            \\main = || Test.value
            \\Test := [].{
            \\    value : U64
            \\    value = Test.helper(0.U64)
            \\    helper : a -> U64
            \\    helper = |_unused| {
            \\        Local(t) := [Local(U64)].{
            \\            from_numeral : Numeral -> Try(Local(t), [InvalidNumeral(Str)])
            \\            from_numeral = |_| Ok(Local(3.U64))
            \\        }
            \\        literal : Local(a)
            \\        literal = 42
            \\        match literal { Local(n) => n }
            \\    }
            \\}
            \\expect Test.value == 3.U64
        , &.{}, .{ .pre_published_builtin = .{
            .env = builtin_modules.builtin_module.env,
            .indices = builtin_modules.builtin_indices,
            .artifact = &builtin_modules.checked_artifact,
        } });
        defer eval.Inspected.cleanupParseAndCanonical(allocator, resources);
        const source_modules = [_]check.TypedCIR.Modules.SourceModule{
            .{ .precompiled = resources.module_env },
            .{ .precompiled = @constCast(builtin_modules.builtin_module.env) },
        };
        var typed_modules = try check.TypedCIR.Modules.init(allocator, &source_modules);
        defer typed_modules.deinit();
        const builtin_view = checked.importedView(&builtin_modules.checked_artifact);
        var pending = try checked.publishFromTypedModule(allocator, &typed_modules, 0, .{
            .module_env_storage = .{ .checked_source = resources.module_env },
            .imports = &.{.{ .module_idx = 1, .key = builtin_modules.checked_artifact.key, .view = builtin_view }},
            .available_artifacts = &.{builtin_view},
            .hoisted_roots = resources.checker.selectedHoistedRoots(),
            .promoted_local_procedures = resources.checker.promotedLocalProcedures(),
            .compile_time_finalizer = eval.CompileTimeFinalization.finalizer(),
            .evaluation_phase = .post_frontend,
            .problem_store = &resources.checker.problems,
        });
        defer pending.deinitRetainingModuleEnv(allocator);
        const modules = lir.CheckedPipeline.CheckedModuleSet{ .root = checked.loweringView(&pending), .imports = &.{builtin_view} };
        var policy = lir.CheckedPipeline.TargetConfig.ordinaryDev();
        policy.checked_module_state = .checking_finalization;
        policy.comptime_value_reads = true;
        policy.literal_roots = true;
        policy.keep_specialization_procs = true;
        var cold = try lir.CheckedPipeline.lowerCheckedModulesToLir(allocator, modules, .{
            .requests = pending.root_requests.compile_time_requests,
        }, policy);
        defer cold.deinit();
        var selected: ?lir.Program.CachedLiteralRecipe = null;
        for (cold.lir_result.spec_procs.items) |spec| {
            const demand = spec.demand orelse continue;
            for (demand.literal_reads) |read| if (std.mem.eql(u8, &read.recipe.module, &pending.key.bytes)) {
                selected = read.recipe;
                break;
            };
            if (selected != null) break;
        }
        const recipe = selected orelse return error.TestUnexpectedResult;
        var expected: ?lir.Program.RootMaterialization = null;
        for (cold.lir_result.store.getProcSpecs()) |proc| {
            const materialization = proc.root_materialization orelse continue;
            if (LoadedPacks.literalRecipeMatchesMaterializer(materialization, recipe)) {
                expected = materialization;
                break;
            }
        }
        try std.testing.expect(expected != null);
        try packs.projectMaterializers(modules, &.{}, &.{recipe}, policy);
        try std.testing.expectEqualDeep(expected.?, packs.materializers.get(expected.?.wrapper_identity).?);
        break :authority expected.?;
    };
    // The checked source, cold producer, and all recipe/type owners are gone.
    const dependency = backend.dev.ProcArtifact.RootMaterializationDependency{
        .binding_name = "binding",
        .header_name = "header",
        .materialization = declaration,
    };
    const artifact = backend.dev.ProcArtifact.Artifact{
        .kind = .{ .proc = lir.ProcIdentity.forTest(151) },
        .code = "\xc3",
        .entry = 0,
        .frame = null,
        .refs = &.{},
        .relocations = &.{},
        .data = &.{},
        .materializations = &.{dependency},
    };
    try std.testing.expect(LoadedPacks.normalNode(&packs, artifact));
}

/// One universe of normal objects, indexed by specialization key and procedure
/// identity. Stable decoded owners allow new demanded objects to join the
/// universe without invalidating any already-borrowed physical graph.
pub const LoadedPacks = struct {
    const AuthorityKey = struct { module: [32]u8, id: u32 };
    const SiteKey = struct {
        module: [32]u8,
        origin: lir.LIR.CheckedSiteOrigin,
        checked_site: ?u32,
    };
    const SourceAuthority = struct { identity: [32]u8, length: usize };
    const SiteAuthority = struct {
        kind: u32,
        region: SourceEvents.Region,
        branches: []const SourceEvents.Region,
    };
    const LiteralAuthority = struct { kind: u32, region: SourceEvents.Region };
    const SelectedProcedure = struct {
        artifact: backend.dev.LocatedArtifact,
        physical: backend.dev.LirCodeGenMod.PhysicalProvision,
        /// The ARC and demand contract the first offer attached.
        hit: Common.SpecCacheHit,
    };

    /// Content identity of an offer's attached contract. Two offers of one
    /// physical definition are compared by it only when both name the same
    /// physical provision.
    fn offerMetadata(hit: Common.SpecCacheHit) [32]u8 {
        var hash = std.crypto.hash.Blake3.init(.{});
        std.hash.autoHashStrat(&hash, hit, .Deep);
        var metadata: [32]u8 = undefined;
        hash.final(&metadata);
        return metadata;
    }
    const OfferKey = struct {
        key: [32]u8,
        platform_requirement_relation: ?[32]u8,
    };

    allocator: Allocator,
    /// The entry-version hash packs are admitted by.
    version_hash: [32]u8,
    packs: std.ArrayList(*PackFile.Pack),
    /// Indexing state of each pack in `packs`, in the same order.
    pack_states: std.ArrayListUnmanaged(Indexed) = .empty,
    indexed_count: usize = 0,
    /// A rejected checking session can never offer or extend its objects
    /// again; the code of specializations it already served stays readable.
    revoked: bool = false,
    specs: std.AutoHashMap(OfferKey, Common.SpecCacheHit),
    /// Specializations with a settled discovery outcome, by the key and
    /// relation they serve.
    discovery: std.AutoHashMapUnmanaged(OfferKey, void) = .empty,
    /// The literal read behind each literal materializer the loaded packs'
    /// code calls, by wrapper identity, so a pack that carries that code on
    /// carries its read too. Each read lives in its pack's arena.
    materializer_reads: std.AutoHashMapUnmanaged([32]u8, lir.Program.CachedLiteralRead) = .empty,
    semantic_valid: std.AutoHashMap(OfferKey, bool),
    artifacts: std.AutoHashMap(lir.ProcIdentity, SelectedProcedure),
    /// Specializations served so far.
    hits: u64 = 0,
    /// Nanoseconds spent admitting closures on lookups, and within that
    /// decoding artifacts and indexing admitted sets; the store's statistics
    /// report them (`ROC_PACK_STATS`).
    admit_ns: i128 = 0,
    decode_ns: i128 = 0,
    index_ns: i128 = 0,
    /// Within `index_ns`: pinning certificate authority, serving-demand
    /// summaries, materializer projection, closure admission, and the
    /// offer rows.
    index_authority_ns: i128 = 0,
    index_summaries_ns: i128 = 0,
    index_project_ns: i128 = 0,
    index_admit_ns: i128 = 0,
    index_specs_ns: i128 = 0,
    /// Nanoseconds the compile-time cache spent capturing and encoding this
    /// program's offers, and within that combining the retained artifacts,
    /// planning the encoding, encoding each owner's pack, and staging it.
    capture_ns: i128 = 0,
    capture_combine_ns: i128 = 0,
    capture_plan_ns: i128 = 0,
    capture_encode_ns: i128 = 0,
    capture_stage_ns: i128 = 0,
    /// Artifacts handed to a splice so far.
    artifacts_served: u64 = 0,
    /// Set until `indexPacks` runs; lookups before that load the pending
    /// store's packs for the modules in view first.
    pending: ?Pending = null,
    /// Current checked declaration authority, including the unpublished root.
    checked_root: ?*const check.CheckedArtifact.CheckedModuleArtifact = null,
    /// Owned projections of immutable checked producer records. Previously
    /// admitted objects never borrow authority from a retiring BuildEnv entry.
    authority_arena: collections.SingleThreadArena,
    sources: std.AutoHashMap([32]u8, SourceAuthority),
    sites: std.AutoHashMap(SiteKey, SiteAuthority),
    literals: std.AutoHashMap(AuthorityKey, LiteralAuthority),
    /// Exact type-only producer projections, copied before checked owners retire.
    materializers: std.AutoHashMap([32]u8, lir.Program.RootMaterialization),
    projection_target: ?lir.CheckedPipeline.TargetConfig = null,
    /// Borrowed only during provider preparation or capture. Pending modules
    /// belong to that caller's input, not the root's published visibility.
    projection_modules: ?lir.CheckedPipeline.CheckedModuleSet = null,
    /// Neither lookup may expose a partial index or retry a failed collection.
    state: enum { pending, ready, unavailable } = .pending,
    failure: ?LoadError = null,
    /// Serializes the one-time loads: lookups can come from parallel
    /// specialization workers, and the first of them loads for all.
    load_mutex: std.atomic.Mutex = .unlocked,
    /// Whether the discovery summaries of the modules in view are indexed.
    /// They load apart from code packs, which load at the first code lookup.
    discovery_loaded: bool = false,
    /// The prepared program's module set, owned here so that the first code
    /// lookup of that program can load and admit packs against it. Its views
    /// borrow checked artifacts the build keeps for the whole command.
    deferred: ?Deferred = null,
    /// Packs a program published while code packs were still unloaded,
    /// indexed with them when they load.
    staged: std.ArrayListUnmanaged(PackFile.Bytes) = .empty,
    /// Store packs, indexed but with their artifacts undecoded. A lookup
    /// that selects one of a pack's offers decodes and admits the closure
    /// of that offer alone, with the closures of the literal producers it
    /// reads from the packs offering them.
    store_packs: std.ArrayListUnmanaged(StorePack) = .empty,
    /// The offers of each key, in load order: each a store pack's spec row.
    candidates: std.AutoHashMapUnmanaged([32]u8, std.ArrayListUnmanaged(Candidate)) = .empty,
    /// The offers of each procedure identity, in load order.
    identity_candidates: std.AutoHashMapUnmanaged([32]u8, std.ArrayListUnmanaged(Candidate)) = .empty,
    /// The serving-closure size of each offer root asked so far, by content
    /// address (`candidateClosureSize`), and the visited set its walk reuses.
    closure_sizes: std.AutoHashMapUnmanaged([32]u8, u32) = .empty,
    closure_scratch: std.AutoHashMapUnmanaged([32]u8, void) = .empty,
    /// Where the store carries each artifact, by content address: the first
    /// registered pack carrying it. Every pack naming the address places the
    /// same bytes, so one carrier serves them all.
    carriers: std.AutoHashMapUnmanaged([32]u8, Carrier) = .empty,
    /// The one set admitted from the store packs, in `packs`, which grows by
    /// a closure at a time and holds each artifact once, by content address,
    /// whichever packs offer or carry it; null until a closure is admitted.
    store_set: ?*PackFile.Pack = null,
    /// Each admitted artifact's index in the store set, by content address.
    position: std.AutoHashMapUnmanaged([32]u8, u32) = .empty,
    /// The offers the store set holds, each once.
    offered: std.AutoHashMapUnmanaged(OfferRow, void) = .empty,
    /// The store set's artifacts, addresses, offers, edges, unresolved flags
    /// and the materializer reads of the packs that contributed to it; the
    /// set's slices view these lists.
    store_artifacts: std.ArrayListUnmanaged(backend.dev.ProcArtifact.Artifact) = .empty,
    store_addresses: std.ArrayListUnmanaged([32]u8) = .empty,
    store_specs: std.ArrayListUnmanaged(PackFile.SpecEntry) = .empty,
    store_edges: std.ArrayListUnmanaged([]const u32) = .empty,
    store_unresolved: std.ArrayListUnmanaged(bool) = .empty,
    store_materializer_reads: std.ArrayListUnmanaged(PackFile.MaterializerRead) = .empty,
    /// Procedures served by identity so far, counted within `hits`.
    identity_hits: u64 = 0,

    /// What an offer is filed under (`PackFile.SpecEntry.by_identity`).
    const OfferKind = enum { key, identity };

    /// The row of a store pack that carries an artifact.
    const Carrier = struct { pack: u32, row: u32 };
    /// An offer: the spec row of a store pack.
    const Candidate = struct { pack: u32, spec: u32 };
    /// What makes two offers of the store set one offer.
    const OfferRow = struct { key: [32]u8, by_identity: bool, artifact: u32, relation: ?[32]u8 };

    const StorePack = struct {
        /// The pack image: bytes this process owns, or the mapped file.
        contents: union(enum) { bytes: PackFile.Bytes, mapped: CoreCtx.MappedFile },
        view: PackFile.View,
        /// The artifacts decoded so far, by their row in the pack. Each
        /// decodes once, however many admitted closures hold it.
        decoded: std.AutoHashMapUnmanaged(u32, backend.dev.ProcArtifact.Artifact) = .empty,
        /// Owns every decoded artifact's contents and offer for the rest of
        /// the load.
        arena: collections.SingleThreadArena,
        /// The offers (by root row) whose closures were admitted or found
        /// unservable.
        admitted_roots: std.AutoHashMapUnmanaged(u32, void) = .empty,
        /// Whether the pack's discovery summaries and materializer reads
        /// joined the store set's, which happens with the first closure
        /// admitted through one of its offers.
        contributed: bool = false,
    };

    /// What indexing has covered of one admitted pack, so a pack that grows
    /// by another closure indexes only the artifacts it gained. An
    /// artifact's serving facts depend on its closure alone, and nothing an
    /// admitted artifact reaches arrives later, so the facts of indexed
    /// artifacts stay as they are.
    const Indexed = struct {
        /// How many of the set's artifacts are indexed.
        artifacts: usize = 0,
        /// How many of the set's offers are indexed. An offer of an artifact
        /// the set already held can arrive after the artifact, from another
        /// pack naming the same address.
        specs: usize = 0,
        /// The serving demand of each indexed artifact.
        summaries: std.ArrayListUnmanaged(ServingDemand) = .empty,
        /// Whether each indexed artifact's closure is admitted.
        valid: std.ArrayListUnmanaged(bool) = .empty,
    };

    const Deferred = struct {
        arena: collections.SingleThreadArena,
        modules: lir.CheckedPipeline.CheckedModuleSet,
        root: *const check.CheckedArtifact.CheckedModuleArtifact,
    };

    pub const LoadError = Allocator.Error || PackFile.ReadError || error{PackDirectoryUnreadable};
    pub const Input = enum {
        /// Independent optional offers; an incompatible file does not veto peers.
        cache_offers,
        /// A requested pack set must be readable under the active contract.
        explicit_packs,
    };

    pub fn init(allocator: Allocator, version_hash: [32]u8) LoadedPacks {
        return .{
            .allocator = allocator,
            .version_hash = version_hash,
            .packs = .empty,
            .authority_arena = collections.SingleThreadArena.init(allocator),
            .sources = std.AutoHashMap([32]u8, SourceAuthority).init(allocator),
            .sites = std.AutoHashMap(SiteKey, SiteAuthority).init(allocator),
            .literals = std.AutoHashMap(AuthorityKey, LiteralAuthority).init(allocator),
            .materializers = std.AutoHashMap([32]u8, lir.Program.RootMaterialization).init(allocator),
            .specs = std.AutoHashMap(OfferKey, Common.SpecCacheHit).init(allocator),
            .semantic_valid = std.AutoHashMap(OfferKey, bool).init(allocator),
            .artifacts = std.AutoHashMap(lir.ProcIdentity, SelectedProcedure).init(allocator),
        };
    }

    /// Load every `.rpk` file in `dir_path` and index the result.
    pub fn loadDir(allocator: Allocator, io: std.Io, dir_path: []const u8, version_hash: [32]u8) LoadError!LoadedPacks {
        var self = LoadedPacks.init(allocator, version_hash);
        errdefer self.deinit();
        try self.loadDirInto(io, dir_path, .explicit_packs);
        try self.indexPacks();
        return self;
    }

    /// Read every `.rpk` file in `dir_path`, in name order, without indexing.
    pub fn loadDirInto(self: *LoadedPacks, io: std.Io, dir_path: []const u8, input: Input) LoadError!void {
        std.debug.assert(self.state == .pending);
        self.loadDirIntoUnrecorded(io, dir_path, input) catch |err| {
            @atomicStore(@TypeOf(self.state), &self.state, .unavailable, .release);
            self.failure = err;
            return err;
        };
    }

    fn loadDirIntoUnrecorded(self: *LoadedPacks, io: std.Io, dir_path: []const u8, input: Input) LoadError!void {
        const allocator = self.allocator;
        var names = std.ArrayList([]u8).empty;
        defer {
            for (names.items) |name| allocator.free(name);
            names.deinit(allocator);
        }
        {
            var dir = std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true }) catch |err| switch (err) {
                error.FileNotFound => if (input == .cache_offers) return else return error.PackDirectoryUnreadable,
                else => return error.PackDirectoryUnreadable,
            };
            defer dir.close(io);
            var entries = dir.iterate();
            while (true) {
                const entry = (entries.next(io) catch return error.PackDirectoryUnreadable) orelse break;
                if (entry.kind != .file) continue;
                if (!std.mem.endsWith(u8, entry.name, ".rpk")) continue;
                try names.ensureUnusedCapacity(allocator, 1);
                names.appendAssumeCapacity(try allocator.dupe(u8, entry.name));
            }
        }
        std.mem.sort([]u8, names.items, {}, nameLessThan);

        for (names.items) |name| {
            const path = try std.fs.path.join(allocator, &.{ dir_path, name });
            defer allocator.free(path);
            const bytes = try readPackFile(allocator, io, path);
            if (input == .cache_offers) {
                self.addSource(bytes) catch |err| switch (err) {
                    error.UnsupportedPackVersion => continue,
                    error.OutOfMemory, error.MalformedPack, error.PackDirectoryUnreadable => return err,
                };
                continue;
            }
            defer allocator.free(bytes);
            self.appendPack(bytes) catch |err| {
                switch (err) {
                    error.UnsupportedPackVersion => if (input == .cache_offers) continue,
                    error.OutOfMemory, error.MalformedPack, error.PackDirectoryUnreadable => {},
                }
                return @as(LoadError!void, err);
            };
        }
    }

    fn appendPack(self: *LoadedPacks, bytes: []align(PackFile.alignment.toByteUnits()) const u8) LoadError!void {
        var pack = try PackFile.read(self.allocator, bytes, self.version_hash);
        errdefer pack.deinit();
        const owner = try self.allocator.create(PackFile.Pack);
        errdefer self.allocator.destroy(owner);
        owner.* = pack;
        // After this succeeds, only LoadedPacks owns the decoded arena,
        // including errors in subsequent files or indexing.
        try self.packs.ensureUnusedCapacity(self.allocator, 1);
        try self.pack_states.ensureUnusedCapacity(self.allocator, 1);
        self.packs.appendAssumeCapacity(owner);
        self.pack_states.appendAssumeCapacity(.{});
    }

    /// Register a store pack's offers without decoding its artifacts. Takes
    /// ownership of `bytes`, also on failure.
    fn addSource(self: *LoadedPacks, bytes: PackFile.Bytes) LoadError!void {
        errdefer self.allocator.free(bytes);
        const view = try PackFile.View.init(bytes, self.version_hash);
        try self.registerSource(.{ .bytes = bytes }, view);
    }

    /// Register a viewed pack's offers as candidates without reading its
    /// artifacts.
    fn registerSource(self: *LoadedPacks, contents: @FieldType(StorePack, "contents"), view: PackFile.View) LoadError!void {
        const id: u32 = @intCast(self.store_packs.items.len);
        if (std.c.getenv("ROC_PACK_TRACE") != null) std.debug.print("register-pack id={d} artifacts={d} references={d} specs={d} digest={x}\n", .{ id, view.artifactCount(), view.rowCount() - view.artifactCount(), view.specCount(), view.contentDigest()[0..8] });
        for (0..view.specCount()) |index| {
            const candidates = if (view.specByIdentity(index)) &self.identity_candidates else &self.candidates;
            const entry = try candidates.getOrPut(self.allocator, view.specKey(index));
            if (!entry.found_existing) entry.value_ptr.* = .empty;
            try entry.value_ptr.append(self.allocator, .{ .pack = id, .spec = @intCast(index) });
        }
        for (0..view.artifactCount()) |row| {
            if (!view.carried(row)) continue;
            const entry = try self.carriers.getOrPut(self.allocator, view.contentAddress(row));
            if (!entry.found_existing) entry.value_ptr.* = .{ .pack = id, .row = @intCast(row) };
        }
        try self.store_packs.append(self.allocator, .{
            .contents = contents,
            .view = view,
            .arena = collections.SingleThreadArena.init(self.allocator),
        });
    }

    /// Read a whole pack file into memory aligned for viewing.
    fn readPackFile(allocator: Allocator, io: std.Io, path: []const u8) LoadError!PackFile.Bytes {
        const file = std.Io.Dir.cwd().openFile(io, path, .{}) catch return error.PackDirectoryUnreadable;
        defer file.close(io);
        const length = file.length(io) catch return error.PackDirectoryUnreadable;
        const bytes = try allocator.alignedAlloc(u8, PackFile.alignment, @intCast(length));
        errdefer allocator.free(bytes);
        const read = file.readPositionalAll(io, bytes, 0) catch return error.PackDirectoryUnreadable;
        if (read != bytes.len) return error.MalformedPack;
        return bytes;
    }

    /// The clock the admission timers read: the store's, when a store is in
    /// view; explicit pack sets are not timed.
    fn now(self: *const LoadedPacks) i128 {
        const pending = self.pending orelse return 0;
        return std.Io.Timestamp.now(pending.io, .awake).nanoseconds;
    }

    /// Admit closures of the offers of `key` (a specialization key, or a
    /// procedure identity for `.identity`) from the store packs offering it,
    /// in load order, until an offer serves the lookup for `relation`, and
    /// transitively the same for the literal producers the admitted offers
    /// read. The first offer admitted for a key is the one the index keeps,
    /// so stopping once it is present serves what admitting every candidate
    /// would.
    fn admitKey(self: *LoadedPacks, kind: OfferKind, key: [32]u8, relation: ?[32]u8) LoadError!void {
        if (self.candidates.count() == 0 and self.identity_candidates.count() == 0) return;
        const started = self.now();
        defer self.admit_ns += self.now() - started;
        var pending = std.ArrayList(PendingOffer).empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ .kind = kind, .key = key });
        while (pending.pop()) |next| {
            const candidates = switch (next.kind) {
                .key => &self.candidates,
                .identity => &self.identity_candidates,
            };
            const list = candidates.get(next.key) orelse continue;
            const ordered = try self.orderCandidatesByClosure(list.items);
            defer self.allocator.free(ordered);
            for (ordered) |candidate| {
                const served = switch (next.kind) {
                    .key => self.specs.contains(.{ .key = next.key, .platform_requirement_relation = relation }),
                    .identity => self.artifacts.contains(.{ .bytes = next.key }),
                };
                if (served) break;
                if (try self.admitClosure(candidate, &pending)) {
                    const index_started = self.now();
                    defer self.index_ns += self.now() - index_started;
                    try self.indexPacksUnrecorded();
                }
            }
        }
    }

    const PendingOffer = struct { kind: OfferKind, key: [32]u8 };

    /// The candidates of one key, smallest serving closure first and
    /// otherwise in load order. Packs offering one key differ in how much of
    /// the key's callee graph they resolved when they were written, and the
    /// closure admitted decides which compile-time roots the program must
    /// provision, so the smallest closure is admitted first (design.md
    /// "Identity Offers").
    fn orderCandidatesByClosure(self: *LoadedPacks, list: []const Candidate) Allocator.Error![]Candidate {
        const ordered = try self.allocator.dupe(Candidate, list);
        errdefer self.allocator.free(ordered);
        if (ordered.len < 2) return ordered;
        // Copies of one artifact have one closure; only distinct roots are sized.
        const first = self.candidateRootAddress(ordered[0]);
        const distinct = for (ordered[1..]) |candidate| {
            if (!std.mem.eql(u8, &self.candidateRootAddress(candidate), &first)) break true;
        } else false;
        if (!distinct) return ordered;
        const sizes = try self.allocator.alloc(u32, ordered.len);
        defer self.allocator.free(sizes);
        for (ordered, sizes) |candidate, *size| size.* = try self.candidateClosureSize(candidate);
        // Insertion sort keeps load order among equal sizes.
        for (1..ordered.len) |i| {
            var j = i;
            while (j > 0 and sizes[j - 1] > sizes[j]) : (j -= 1) {
                std.mem.swap(u32, &sizes[j - 1], &sizes[j]);
                std.mem.swap(Candidate, &ordered[j - 1], &ordered[j]);
            }
        }
        return ordered;
    }

    /// The artifacts a candidate's closure spans: its root and every
    /// artifact reachable over the edges of the packs carrying them, by
    /// content address as admission walks them (`admitClosure`). An address
    /// no loaded pack carries ends its branch; such a candidate is not
    /// servable, and its size only orders it.
    fn candidateRootAddress(self: *const LoadedPacks, candidate: Candidate) [32]u8 {
        const view = &self.store_packs.items[candidate.pack].view;
        return view.contentAddress(view.specArtifact(candidate.spec));
    }

    fn candidateClosureSize(self: *LoadedPacks, candidate: Candidate) Allocator.Error!u32 {
        const root = self.candidateRootAddress(candidate);
        const entry = try self.closure_sizes.getOrPut(self.allocator, root);
        if (entry.found_existing) return entry.value_ptr.*;
        const seen = &self.closure_scratch;
        seen.clearRetainingCapacity();
        var stack = std.ArrayList([32]u8).empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, root);
        var count: u32 = 0;
        while (stack.pop()) |address| {
            if ((try seen.getOrPut(self.allocator, address)).found_existing) continue;
            const carrier = self.carriers.get(address) orelse continue;
            count += 1;
            const carrier_view = &self.store_packs.items[carrier.pack].view;
            for (carrier_view.edges(carrier.row)) |edge| try stack.append(self.allocator, carrier_view.contentAddress(edge));
        }
        entry.value_ptr.* = count;
        return count;
    }

    /// Admit the closure of one offer, if it is not admitted yet, into the
    /// store set: the artifacts the offer reaches that the set lacks are
    /// appended to it, each read from the pack that carries it and
    /// renumbered densely, with references by content address. The set then
    /// offers every spec of the offer's pack whose root it holds, and
    /// admission queues the literal producers those offers and the pack's
    /// materializer reads name. Returns whether the set gained artifacts or
    /// offers, which indexing then covers.
    fn admitClosure(self: *LoadedPacks, candidate: Candidate, pending: *std.ArrayList(PendingOffer)) LoadError!bool {
        const id = candidate.pack;
        const source = &self.store_packs.items[id];
        const view = &source.view;
        const root_row = view.specArtifact(candidate.spec);
        if (source.admitted_roots.contains(root_row)) return false;
        // An offer whose facts or serving closure name a module this program
        // does not contain (an earlier revision's) is never served, so its
        // closure is not admitted either.
        if (!self.offerModulesCurrent(view, candidate.spec)) {
            if (std.c.getenv("ROC_PACK_TRACE") != null) std.debug.print("stale-offer pack={d} key={x}\n", .{ id, view.specKey(candidate.spec)[0..8] });
            try source.admitted_roots.put(self.allocator, root_row, {});
            return false;
        }
        const roots = [_]u32{root_row};

        // The members the set lacks, by address, each with its carrier;
        // null when the closure reaches an artifact no loaded pack carries
        // or one with an unresolved reference, which cannot be served.
        const Member = struct { address: [32]u8, carrier: Carrier };
        var members = std.ArrayList(Member).empty;
        defer members.deinit(self.allocator);
        var seen = std.AutoHashMapUnmanaged([32]u8, void).empty;
        defer seen.deinit(self.allocator);
        var stack = std.ArrayList([32]u8).empty;
        defer stack.deinit(self.allocator);
        for (roots) |root| try stack.append(self.allocator, view.contentAddress(root));
        const servable = walk: while (stack.pop()) |address| {
            if (self.position.contains(address)) continue;
            if ((try seen.getOrPut(self.allocator, address)).found_existing) continue;
            const carrier = self.carriers.get(address) orelse break :walk false;
            const carrier_view = &self.store_packs.items[carrier.pack].view;
            if (carrier_view.unresolved(carrier.row)) break :walk false;
            try members.append(self.allocator, .{ .address = address, .carrier = carrier });
            for (carrier_view.edges(carrier.row)) |edge| try stack.append(self.allocator, carrier_view.contentAddress(edge));
        } else true;
        if (!servable) {
            for (roots) |root| try source.admitted_roots.put(self.allocator, root, {});
            return false;
        }
        if (std.c.getenv("ROC_PACK_TRACE") != null) std.debug.print("admit-closure pack={d} key={x} added={d}\n", .{ id, view.specKey(candidate.spec)[0..8], members.items.len });
        {
            const decode_started = self.now();
            defer self.decode_ns += self.now() - decode_started;
            for (members.items) |member| try self.decodeCarried(member.carrier);
        }
        const pack = self.store_set orelse try self.createStoreSet();
        const base: u32 = @intCast(self.store_artifacts.items.len);
        for (members.items, 0..) |member, offset| try self.position.put(self.allocator, member.address, base + @as(u32, @intCast(offset)));
        const a = pack.set.arena.allocator();
        try self.store_artifacts.ensureUnusedCapacity(self.allocator, members.items.len);
        try self.store_addresses.ensureUnusedCapacity(self.allocator, members.items.len);
        try self.store_edges.ensureUnusedCapacity(self.allocator, members.items.len);
        try self.store_unresolved.ensureUnusedCapacity(self.allocator, members.items.len);
        for (members.items) |member| {
            const carrier = &self.store_packs.items[member.carrier.pack];
            const carrier_view = &carrier.view;
            const row = member.carrier.row;
            const pack_edges = carrier_view.edges(row);
            const callees = try a.alloc(u32, pack_edges.len);
            for (pack_edges, callees) |edge, *callee| callee.* = self.position.get(carrier_view.contentAddress(edge)) orelse return error.MalformedPack;
            self.store_edges.appendAssumeCapacity(callees);
            self.store_unresolved.appendAssumeCapacity(carrier_view.unresolved(row));
            var artifact = carrier.decoded.get(row) orelse unreachable;
            const refs = try a.dupe(backend.dev.ProcArtifact.Reference, artifact.refs);
            for (refs) |*ref| ref.target = self.position.get(carrier_view.contentAddress(ref.target)) orelse return error.MalformedPack;
            artifact.refs = refs;
            const data = try a.dupe(backend.dev.ProcArtifact.DataItem, artifact.data);
            for (data) |*item| {
                const relocations = try a.dupe(backend.dev.ProcArtifact.DataRelocation, item.relocations);
                for (relocations) |*relocation| if (relocation.code_target) |*target| {
                    if (target.* == .local) target.local = self.position.get(carrier_view.contentAddress(target.local)) orelse return error.MalformedPack;
                };
                item.relocations = relocations;
            }
            artifact.data = data;
            self.store_artifacts.appendAssumeCapacity(artifact);
            self.store_addresses.appendAssumeCapacity(member.address);
        }
        var specs_added = false;
        for (0..view.specCount()) |index| {
            const root = view.specArtifact(index);
            if (source.admitted_roots.contains(root)) continue;
            const new = self.position.get(view.contentAddress(root)) orelse continue;
            try source.admitted_roots.put(self.allocator, root, {});
            if (!self.offerModulesCurrent(view, index)) continue;
            // Several packs offer one artifact under one key: the set keeps
            // the first such offer, so a second is not indexed again.
            const offered = try self.offered.getOrPut(self.allocator, .{
                .key = view.specKey(index),
                .by_identity = view.specByIdentity(index),
                .artifact = new,
                .relation = view.specRelation(index),
            });
            if (offered.found_existing) continue;
            var entry = try view.spec(index, source.arena.allocator());
            entry.artifact = new;
            try self.store_specs.append(self.allocator, entry);
            specs_added = true;
            if (entry.demand) |demand| for (demand.literal_reads) |read| try pending.append(self.allocator, .{ .kind = .key, .key = read.recipe.producer_key });
        }
        if (!source.contributed) {
            source.contributed = true;
            const discovery = try view.discovery(source.arena.allocator());
            for (discovery) |entry| {
                try self.discovery.put(self.allocator, .{ .key = entry.key, .platform_requirement_relation = entry.platform_requirement_relation }, {});
            }
            const reads = try view.materializerReads(source.arena.allocator());
            for (reads) |entry| {
                const read = try self.materializer_reads.getOrPut(self.allocator, entry.wrapper_identity);
                if (!read.found_existing) read.value_ptr.* = entry.read;
                try pending.append(self.allocator, .{ .kind = .key, .key = entry.read.recipe.producer_key });
            }
            try self.store_materializer_reads.appendSlice(self.allocator, reads);
        }
        pack.set.artifacts = self.store_artifacts.items;
        pack.set.addresses = self.store_addresses.items;
        pack.specs = self.store_specs.items;
        pack.edges = self.store_edges.items;
        pack.unresolved = self.store_unresolved.items;
        pack.materializer_reads = self.store_materializer_reads.items;
        return members.items.len != 0 or specs_added;
    }

    /// Whether every checked module offer `index` of `view` lists is in the
    /// program being built. The offer lists those modules; its facts
    /// themselves are checked once its closure is indexed.
    fn offerModulesCurrent(self: *const LoadedPacks, view: *const PackFile.View, index: usize) bool {
        if (!self.hasProgram()) return true;
        for (view.specModules(index)) |module| {
            if (self.checkedArtifact(module) == null) return false;
        }
        return true;
    }

    /// The set admitted from the store packs, created empty and registered
    /// among the packs. Its discovery summaries and materializer reads are
    /// indexed as the packs offering them contribute.
    fn createStoreSet(self: *LoadedPacks) LoadError!*PackFile.Pack {
        const owner = try self.allocator.create(PackFile.Pack);
        errdefer self.allocator.destroy(owner);
        owner.* = .{
            .set = .{
                .arena = collections.SingleThreadArena.init(self.allocator),
                .artifacts = &.{},
                .addresses = &.{},
            },
            .specs = &.{},
            .edges = &.{},
            .unresolved = &.{},
        };
        try self.packs.ensureUnusedCapacity(self.allocator, 1);
        try self.pack_states.ensureUnusedCapacity(self.allocator, 1);
        self.packs.appendAssumeCapacity(owner);
        self.pack_states.appendAssumeCapacity(.{});
        self.store_set = owner;
        return owner;
    }

    /// Read the artifact a carrier row holds out of its pack's image, once.
    /// Its code, data and names stay the image's bytes.
    fn decodeCarried(self: *LoadedPacks, carrier: Carrier) LoadError!void {
        const source = &self.store_packs.items[carrier.pack];
        if (source.decoded.contains(carrier.row)) return;
        const artifact = try source.view.artifact(carrier.row, source.arena.allocator());
        try source.decoded.put(self.allocator, carrier.row, artifact);
    }

    /// Retain newly produced normal objects in the same serving universe.
    /// Disk publication remains a separate accepted-checking transaction.
    pub fn retain(self: *LoadedPacks, bytes: []align(PackFile.alignment.toByteUnits()) const u8) LoadError!void {
        std.debug.assert(!self.revoked);
        if (self.state == .unavailable) return self.failure.?;
        // A store's code packs load at the first code lookup, and these join
        // them then; nothing before that could look them up.
        if (self.pending != null) {
            const owned = try self.allocator.alignedAlloc(u8, PackFile.alignment, bytes.len);
            @memcpy(owned, bytes);
            switch (self.state) {
                .pending => try self.staged.append(self.allocator, owned),
                .ready => {
                    while (!self.load_mutex.tryLock()) std.atomic.spinLoopHint();
                    defer self.load_mutex.unlock();
                    self.addSource(owned) catch |err| return self.recordFailure(err);
                },
                .unavailable => unreachable,
            }
            return;
        }
        self.appendPack(bytes) catch |err| return self.recordFailure(err);
        try self.indexPacks();
    }

    /// Index a published pack of discovery summaries.
    pub fn retainDiscovery(self: *LoadedPacks, bytes: []align(PackFile.alignment.toByteUnits()) const u8) LoadError!void {
        std.debug.assert(!self.revoked);
        try self.indexDiscoveryBytes(bytes);
    }

    /// Read every discovery pack in `dir_path` filed for `key` and index its
    /// summaries. Unreadable files are cache misses.
    pub fn loadDiscoveryDir(self: *LoadedPacks, io: std.Io, dir_path: []const u8) LoadError!void {
        var dir = std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true }) catch |err| switch (err) {
            error.FileNotFound => return,
            else => return error.PackDirectoryUnreadable,
        };
        defer dir.close(io);
        var entries = dir.iterate();
        while (true) {
            const entry = (entries.next(io) catch return error.PackDirectoryUnreadable) orelse break;
            if (entry.kind != .file) continue;
            if (!isDiscoveryName(entry.name)) continue;
            const path = try std.fs.path.join(self.allocator, &.{ dir_path, entry.name });
            defer self.allocator.free(path);
            const bytes = try readPackFile(self.allocator, io, path);
            defer self.allocator.free(bytes);
            self.indexDiscoveryBytes(bytes) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.UnsupportedPackVersion, error.MalformedPack => continue,
                error.PackDirectoryUnreadable => unreachable,
            };
        }
    }

    fn indexDiscoveryBytes(self: *LoadedPacks, bytes: []align(PackFile.alignment.toByteUnits()) const u8) LoadError!void {
        var pack = try PackFile.read(self.allocator, bytes, self.version_hash);
        defer pack.deinit();
        for (pack.discovery) |entry| {
            try self.discovery.put(self.allocator, .{ .key = entry.key, .platform_requirement_relation = entry.platform_requirement_relation }, {});
        }
    }

    /// Whether a pack file name is a discovery pack
    /// (`discovery-<key>-<content>.rpk`).
    fn isDiscoveryName(name: []const u8) bool {
        return std.mem.endsWith(u8, name, ".rpk") and std.mem.startsWith(u8, name, "discovery-");
    }

    /// Register every code pack in `dir_path`, from every revision of its
    /// module, reading only each pack's header and offers. Offers are keyed
    /// by declaration, so a pack an earlier revision wrote still serves the
    /// specializations an edit left alone; the pack's artifacts are read
    /// when a lookup selects one of its offers.
    pub fn loadStoreDir(self: *LoadedPacks, io: std.Io, dir_path: []const u8) LoadError!void {
        const allocator = self.allocator;
        var names = std.ArrayList([]u8).empty;
        defer {
            for (names.items) |name| allocator.free(name);
            names.deinit(allocator);
        }
        {
            var dir = std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true }) catch |err| switch (err) {
                error.FileNotFound => return,
                else => return error.PackDirectoryUnreadable,
            };
            defer dir.close(io);
            var entries = dir.iterate();
            while (true) {
                const entry = (entries.next(io) catch return error.PackDirectoryUnreadable) orelse break;
                if (entry.kind != .file) continue;
                if (!std.mem.endsWith(u8, entry.name, ".rpk") or isDiscoveryName(entry.name)) continue;
                try names.ensureUnusedCapacity(allocator, 1);
                names.appendAssumeCapacity(try allocator.dupe(u8, entry.name));
            }
        }
        std.mem.sort([]u8, names.items, {}, nameLessThan);
        for (names.items) |name| {
            const path = try std.fs.path.join(allocator, &.{ dir_path, name });
            defer allocator.free(path);
            const store = (self.pending orelse unreachable).store;
            // The image is mapped where the host allows; otherwise read whole.
            if (store.roc_ctx.mapFilePrivate(path)) |mapped| {
                var mapped_owned = true;
                defer if (mapped_owned) mapped.unmap();
                comptime std.debug.assert(std.heap.page_size_min >= PackFile.alignment.toByteUnits());
                const bytes: []align(PackFile.alignment.toByteUnits()) const u8 = @alignCast(mapped.bytes());
                const view = PackFile.View.init(bytes, self.version_hash) catch |err| switch (err) {
                    error.UnsupportedPackVersion => continue,
                    error.OutOfMemory, error.MalformedPack => return err,
                };
                try self.registerSource(.{ .mapped = mapped }, view);
                mapped_owned = false;
            } else {
                const bytes = try readPackFile(allocator, io, path);
                var bytes_owned = true;
                defer if (bytes_owned) allocator.free(bytes);
                const view = PackFile.View.init(bytes, self.version_hash) catch |err| switch (err) {
                    error.UnsupportedPackVersion => continue,
                    error.OutOfMemory, error.MalformedPack => return err,
                };
                try self.registerSource(.{ .bytes = bytes }, view);
                bytes_owned = false;
            }
        }
    }

    /// Own a copy of a prepared program's module set, so that its first code
    /// lookup can load packs and admit them against it.
    pub fn deferLoad(self: *LoadedPacks, modules: lir.CheckedPipeline.CheckedModuleSet, root: *const check.CheckedArtifact.CheckedModuleArtifact) Allocator.Error!void {
        var arena = collections.SingleThreadArena.init(self.allocator);
        errdefer arena.deinit();
        var copied = modules;
        copied.imports = try arena.allocator().dupe(check.CheckedArtifact.ImportedModuleView, modules.imports);
        copied.root.relation_modules = try arena.allocator().dupe(check.CheckedArtifact.ImportedModuleView, modules.root.relation_modules);
        if (self.deferred) |*previous| previous.arena.deinit();
        self.deferred = .{ .arena = arena, .modules = copied, .root = root };
    }

    /// Index the discovery summaries of the root module and every module it
    /// lowers against, once.
    pub fn ensureDiscoveryLoadedFor(self: *LoadedPacks, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) void {
        if (self.discovery_loaded) return;
        const pending = self.pending orelse return;
        defer @atomicStore(bool, &self.discovery_loaded, true, .release);
        self.loadPendingDiscovery(pending, root_artifact) catch |err| switch (err) {
            error.OutOfMemory => self.unavailable(error.OutOfMemory),
            // Discovery summaries only spare work; an unreadable directory
            // leaves every discovery request to be lowered.
            error.PackDirectoryUnreadable, error.MalformedPack, error.UnsupportedPackVersion => {},
        };
    }

    fn loadPendingDiscovery(self: *LoadedPacks, pending: Pending, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) LoadError!void {
        const placements = try self.pendingPlacements(pending, root_artifact);
        defer self.allocator.free(placements);
        for (placements) |placement| try pending.store.loadDiscoveryIdentity(pending.io, self, placement.origin, placement.identity);
    }

    /// The placements of the root module and every module it lowers against,
    /// each once: a placement holds every revision of its module's packs.
    fn pendingPlacements(self: *LoadedPacks, pending: Pending, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) LoadError![]compile.BuildEnv.PackPlacement {
        var placements = std.ArrayList(compile.BuildEnv.PackPlacement).empty;
        errdefer placements.deinit(self.allocator);
        var seen = std.AutoHashMap([32]u8, void).init(self.allocator);
        defer seen.deinit();
        const artifacts = try pending.build_env.collectVisibleArtifacts(self.allocator, root_artifact);
        defer self.allocator.free(artifacts);
        try placements.ensureTotalCapacity(self.allocator, artifacts.len + 1);
        if (pending.build_env.packPlacementForArtifactKey(root_artifact.key)) |placement| {
            try seen.put(placement.identity, {});
            placements.appendAssumeCapacity(placement);
        }
        for (artifacts) |artifact| {
            const placement = pending.build_env.packPlacementForArtifactKey(artifact.key) orelse continue;
            if ((try seen.getOrPut(placement.identity)).found_existing) continue;
            placements.appendAssumeCapacity(placement);
        }
        return placements.toOwnedSlice(self.allocator);
    }

    fn recordFailure(self: *LoadedPacks, err: LoadError) LoadError {
        @atomicStore(@TypeOf(self.state), &self.state, .unavailable, .release);
        self.failure = err;
        return err;
    }

    /// Index new decoded owners. Existing physical graph addresses stay fixed.
    pub fn indexPacks(self: *LoadedPacks) LoadError!void {
        if (self.state == .unavailable) return self.failure.?;
        self.indexPacksUnrecorded() catch |err| return self.recordFailure(err);
    }

    fn indexPacksUnrecorded(self: *LoadedPacks) LoadError!void {
        for (self.packs.items, self.pack_states.items) |pack, *state| {
            const from = state.artifacts;
            if (from == pack.set.artifacts.len and state.specs == pack.specs.len) continue;
            if (from == 0) {
                for (pack.discovery) |entry| {
                    try self.discovery.put(self.allocator, .{ .key = entry.key, .platform_requirement_relation = entry.platform_requirement_relation }, {});
                }
                for (pack.materializer_reads) |entry| {
                    const read = try self.materializer_reads.getOrPut(self.allocator, entry.wrapper_identity);
                    if (!read.found_existing) read.value_ptr.* = entry.read;
                }
            }
            const authority_started = self.now();
            for (pack.set.artifacts[from..]) |artifact| {
                // A pack's certificates were asserted well-formed when it was
                // written (design.md "Early Compile-Time Object Reuse"), so a
                // malformed one here is a compiler bug, not a cache condition.
                if (builtin.mode == .debug) backend.dev.ProcArtifact.validateSourceEventCertificates(self.allocator, artifact, builtin.cpu.arch.endian()) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.InvalidSourceEventCertificate => std.debug.panic("pack carries a malformed source event certificate", .{}),
                };
                for (artifact.source_events) |certificate| try self.pinAuthority(certificate);
            }
            const summaries_started = self.now();
            self.index_authority_ns += summaries_started - authority_started;
            // The artifacts the set gained since the last indexing, if any,
            // get their serving facts; the offers it gained name artifacts
            // whose facts are then settled.
            if (from != pack.set.artifacts.len) {
                const summaries = try self.servingDemandSummaries(pack.set.artifacts, pack.edges, pack.specs, pack.materializer_reads, state.summaries.items);
                defer self.allocator.free(summaries);
                const project_started = self.now();
                self.index_summaries_ns += project_started - summaries_started;
                try self.projectPendingMaterializers(pack, summaries);
                const admit_started = self.now();
                self.index_project_ns += admit_started - project_started;
                const closed = try self.allocator.alloc(bool, pack.set.artifacts.len);
                defer self.allocator.free(closed);
                @memcpy(closed[0..from], state.valid.items);
                try self.admitNewArtifacts(pack, from, summaries, closed);
                self.index_admit_ns += self.now() - admit_started;
                try state.summaries.appendSlice(self.allocator, summaries[from..]);
                try state.valid.appendSlice(self.allocator, closed[from..]);
                state.artifacts = pack.set.artifacts.len;
            }
            const summaries = state.summaries.items;
            const closed = state.valid.items;
            const specs_started = self.now();
            defer self.index_specs_ns += self.now() - specs_started;
            const new_specs = pack.specs[state.specs..];
            state.specs = pack.specs.len;
            // An addressed set names its definitions by content address; any
            // other set by its graph's namespace and the definition's index.
            const physical_namespace: ?[32]u8 = if (pack.set.addresses != null) null else pack.set.physical_namespace orelse backend.dev.ArtifactClosure.derivedPhysicalNamespace(&pack.set);
            for (new_specs) |spec| {
                const root = pack.set.artifacts[spec.artifact];
                const identity = switch (root.kind) {
                    .proc => |identity| identity,
                    .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => return error.MalformedPack,
                };
                const trace = std.c.getenv("ROC_PACK_TRACE") != null;
                var demand = spec.demand orelse {
                    if (trace) std.debug.print("index-offer key={x} reason=no-demand\n", .{spec.key[0..8]});
                    continue;
                };
                if (!demand.permitsRetirement()) {
                    if (trace) std.debug.print("index-offer key={x} reason=no-retirement\n", .{spec.key[0..8]});
                    continue;
                }
                if (!closed[spec.artifact]) {
                    if (trace) std.debug.print("index-offer key={x} reason=not-closed complete={} artifacts={d}\n", .{ spec.key[0..8], summaries[spec.artifact].complete, pack.set.artifacts.len });
                    continue;
                }
                if (root.callable_contract == null) {
                    if (trace) std.debug.print("index-offer key={x} reason=no-contract\n", .{spec.key[0..8]});
                    continue;
                }
                demand.literal_reads = summaries[spec.artifact].literal_reads;
                if (!self.factsNameCurrentModules(demand, summaries[spec.artifact].checked_roots, spec.comptime_root_reads)) {
                    if (trace) std.debug.print("index-offer key={x} reason=facts-name-other-modules\n", .{spec.key[0..8]});
                    continue;
                }
                if (trace) std.debug.print("index-offer key={x} reason=accepted artifacts={d} roots={d} reads={d} pack={d}\n", .{ spec.key[0..8], pack.set.artifacts.len, summaries[spec.artifact].checked_roots.len, demand.eager_reads.len, (@intFromPtr(state) - @intFromPtr(self.pack_states.items.ptr)) / @sizeOf(@TypeOf(state.*)) });
                const hit: Common.SpecCacheHit = .{
                    .identity = identity.bytes,
                    .demand = demand,
                    .checked_root_uses = summaries[spec.artifact].checked_roots,
                    .platform_requirement_relation = spec.platform_requirement_relation,
                    .comptime_root_reads = spec.comptime_root_reads,
                    .rc_borrowed_params = spec.rc_borrowed_params,
                    .rc_ret_borrowed = spec.rc_ret_borrowed,
                    .rc_ret_lenders = spec.rc_ret_lenders,
                    .rc_read_only_params = spec.rc_read_only_params,
                    .rc_ret_unique = spec.rc_ret_unique,
                    .rc_ret_unique_fields = spec.rc_ret_unique_fields,
                    .rc_ret_conditions = spec.rc_ret_conditions,
                };
                const physical = pack.set.provisionIn(physical_namespace, spec.artifact);
                if (spec.by_identity and !std.mem.eql(u8, &spec.key, &identity.bytes)) return error.MalformedPack;
                const artifact_entry = try self.artifacts.getOrPut(identity);
                if (artifact_entry.found_existing) {
                    // Logical identity cannot redirect a hit to a physical
                    // sibling, nor attach that sibling's ARC/demand contract.
                    if (!std.meta.eql(artifact_entry.value_ptr.physical, physical) or
                        !std.mem.eql(u8, &offerMetadata(artifact_entry.value_ptr.hit), &offerMetadata(hit))) continue;
                } else {
                    artifact_entry.value_ptr.* = .{
                        .artifact = .{ .set = &pack.set, .index = spec.artifact },
                        .physical = physical,
                        .hit = hit,
                    };
                }
                // An identity offer serves lookups by identity alone.
                if (spec.by_identity) continue;
                const gop = try self.specs.getOrPut(.{
                    .key = spec.key,
                    .platform_requirement_relation = spec.platform_requirement_relation,
                });
                if (!gop.found_existing) gop.value_ptr.* = hit;
            }
        }
        self.indexed_count = self.packs.items.len;
        try self.certifyLiteralOffers();
        @atomicStore(@TypeOf(self.state), &self.state, .ready, .release);
    }

    /// The strongly connected components of a set's artifacts from `from`
    /// on, over the edges among them, numbered so that no edge leads into a
    /// lower-numbered component. Artifacts before `from` are settled: nothing
    /// they reach was added, so they take part only as settled callees.
    /// Everything is allocated in `arena`.
    fn newComponents(arena: Allocator, edges: []const []const u32, from: usize) Allocator.Error!collections.scc.Components {
        const count = edges.len - from;
        const succ_starts = try arena.alloc(u32, count + 1);
        const pred_starts = try arena.alloc(u32, count + 1);
        @memset(pred_starts, 0);
        succ_starts[0] = 0;
        for (edges[from..], 0..) |callees, node| {
            var kept: u32 = 0;
            for (callees) |callee| if (callee >= from) {
                kept += 1;
                pred_starts[callee - from + 1] += 1;
            };
            succ_starts[node + 1] = succ_starts[node] + kept;
        }
        for (1..count + 1) |node| pred_starts[node] += pred_starts[node - 1];
        const succs = try arena.alloc(u32, succ_starts[count]);
        const preds = try arena.alloc(u32, pred_starts[count]);
        const cursors = try arena.dupe(u32, pred_starts[0..count]);
        var next: usize = 0;
        for (edges[from..], 0..) |callees, node| {
            for (callees) |callee| if (callee >= from) {
                succs[next] = @intCast(callee - from);
                next += 1;
                preds[cursors[callee - from]] = @intCast(node);
                cursors[callee - from] += 1;
            };
        }
        return collections.scc.compute(arena, succ_starts, succs, pred_starts, preds);
    }

    /// Decide `closed` for the artifacts of `pack` from `from` on: an
    /// artifact's closure is admitted when every artifact in it is an
    /// ordinary node with complete serving demand and declared literal
    /// materializers, every reference resolves in the pack, and every
    /// settled callee is admitted. The decision is made per component, from
    /// the callees up.
    fn admitNewArtifacts(self: *LoadedPacks, pack: *const PackFile.Pack, from: usize, summaries: []const ServingDemand, closed: []bool) Allocator.Error!void {
        var scratch = collections.SingleThreadArena.init(self.allocator);
        defer scratch.deinit();
        const components = try newComponents(scratch.allocator(), pack.edges, from);
        const count = components.count();
        const component_valid = try scratch.allocator().alloc(bool, count);
        var component = count;
        while (component != 0) {
            component -= 1;
            var valid = true;
            const trace = std.c.getenv("ROC_PACK_TRACE") != null;
            for (components.membersOf(component)) |member| {
                const node = from + member;
                const artifact = pack.set.artifacts[node];
                const reason: ?[]const u8 = if (pack.unresolved[node]) "unresolved" else if (!normalNode(self, artifact)) "not-normal" else if (!summaries[node].complete) "incomplete-demand" else if (!literalMaterializersDeclared(artifact, summaries[node].literal_reads)) "undeclared-literal" else null;
                if (reason) |why| {
                    valid = false;
                    if (trace) std.debug.print("reject-artifact node={d} reason={s} kind={s}\n", .{ node, why, @tagName(artifact.kind) });
                }
                for (pack.edges[node]) |callee| {
                    if (callee < from) {
                        if (!closed[callee]) {
                            if (trace and valid) std.debug.print("reject-artifact node={d} reason=settled-callee callee={d}\n", .{ node, callee });
                            valid = false;
                        }
                    } else if (components.component_of[callee - from] != component and !component_valid[components.component_of[callee - from]]) {
                        if (trace and valid) std.debug.print("reject-artifact node={d} reason=callee callee={d}\n", .{ node, callee });
                        valid = false;
                    }
                }
            }
            component_valid[component] = valid;
            for (components.membersOf(component)) |member| closed[from + member] = valid;
        }
    }

    /// Semantic producer edges are additional to machine references. Propagate
    /// missing exact offers backwards once; recursive groups remain closed when
    /// all their declarations and physical definitions are present.
    fn certifyLiteralOffers(self: *LoadedPacks) Allocator.Error!void {
        var arena = collections.SingleThreadArena.init(self.allocator);
        defer arena.deinit();
        const a = arena.allocator();
        const keys = try a.alloc(OfferKey, self.specs.count());
        const hits = try a.alloc(Common.SpecCacheHit, keys.len);
        var index = std.AutoHashMap(OfferKey, u32).init(a);
        var entries = self.specs.iterator();
        for (keys, hits, 0..) |*key, *hit, id| {
            const entry = entries.next().?;
            key.* = entry.key_ptr.*;
            hit.* = entry.value_ptr.*;
            try index.put(key.*, @intCast(id));
        }
        const callers = try a.alloc(std.ArrayList(u32), keys.len);
        @memset(callers, .empty);
        const valid = try a.alloc(bool, keys.len);
        @memset(valid, true);
        var invalid = std.ArrayList(u32).empty;
        for (keys, hits, 0..) |key, hit, id| {
            const demand = hit.demand orelse unreachable;
            for (demand.literal_reads) |read| {
                const dependency = index.get(.{
                    .key = read.recipe.producer_key,
                    .platform_requirement_relation = key.platform_requirement_relation,
                }) orelse index.get(.{
                    .key = read.recipe.producer_key,
                    .platform_requirement_relation = null,
                });
                if (dependency) |callee| {
                    if (!std.mem.eql(u8, &hits[callee].identity, &read.recipe.object_identity)) {
                        valid[id] = false;
                        continue;
                    }
                    try callers[callee].append(a, @intCast(id));
                } else valid[id] = false;
            }
            if (!valid[id]) try invalid.append(a, @intCast(id));
        }
        var cursor: usize = 0;
        while (cursor < invalid.items.len) : (cursor += 1) {
            for (callers[invalid.items[cursor]].items) |caller| {
                if (!valid[caller]) continue;
                valid[caller] = false;
                try invalid.append(a, caller);
            }
        }
        self.semantic_valid.clearRetainingCapacity();
        for (keys, valid) |key, admitted| try self.semantic_valid.put(key, admitted);
    }

    fn normalNode(context: *anyopaque, artifact: backend.dev.ProcArtifact.Artifact) bool {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        // Physical code policy belongs to the store namespace. A serving graph
        // is not classified by which consumer first demanded its procedures.
        if (artifact.root_materialization != null) return false;
        const trace = std.c.getenv("ROC_PACK_TRACE") != null;
        for (artifact.materializations) |dependency| {
            // An earlier revision's packs declare that revision's
            // materializers, which this program does not provide.
            if (self.hasProgram() and self.checkedArtifact(dependency.materialization.module) == null) {
                if (trace) std.debug.print("not-normal reason=materializer-module module={x}\n", .{dependency.materialization.module[0..8]});
                return false;
            }
            if (!self.materializers.contains(dependency.materialization.wrapper_identity)) {
                if (trace) std.debug.print("not-normal reason=materializer-unprojected wrapper={x}\n", .{dependency.materialization.wrapper_identity[0..8]});
                return false;
            }
        }
        for (artifact.source_events) |certificate| {
            const descriptor = certificate.descriptor;
            if (descriptor.source.has_location != 0) {
                const authority = self.sources.get(descriptor.source.checked_module) orelse return false;
                if (!std.mem.eql(u8, &authority.identity, &descriptor.source.source_identity) or
                    !validRegion(descriptor.source.region, authority.length)) return false;
            }
            if (descriptor.header.kind == @backingInt(SourceEvents.Kind.branch_taken) or
                descriptor.header.kind == @backingInt(SourceEvents.Kind.exhaustiveness_failed))
            {
                const key = siteKey(certificate) orelse return false;
                const authority = self.sites.get(key) orelse return false;
                if (descriptor.site.kind != authority.kind or !std.meta.eql(descriptor.site.region, authority.region) or
                    certificate.branch_regions.len != authority.branches.len) return false;
                for (certificate.branch_regions, authority.branches) |region, expected| {
                    if (region.start.offset != expected.start or region.end.offset != expected.end) return false;
                }
            }
            if (descriptor.failure.has_literal_rejection != 0) {
                const literal = descriptor.failure.literal_rejection;
                const authority = self.literals.get(.{ .module = literal.checked_module, .id = literal.checked_expr }) orelse return false;
                if (literal.kind != authority.kind or descriptor.source.has_location == 0 or
                    !std.mem.eql(u8, &descriptor.source.checked_module, &literal.checked_module) or
                    !std.meta.eql(descriptor.source.region, authority.region)) return false;
            }
        }
        return true;
    }

    /// Use only the declared root table and ordinary type/layout lowering.
    /// No source procedure requests or cached layouts enter this projection.
    pub fn projectCheckedMaterializers(
        self: *LoadedPacks,
        modules: lir.CheckedPipeline.CheckedModuleSet,
        uses: []const Common.CheckedRootUse,
        policy: lir.CheckedPipeline.TargetConfig,
    ) lir.CheckedPipeline.LowerResourceError!void {
        return self.projectMaterializers(modules, uses, &.{}, policy);
    }

    pub fn projectMaterializers(
        self: *LoadedPacks,
        modules: lir.CheckedPipeline.CheckedModuleSet,
        uses: []const Common.CheckedRootUse,
        recipes: []const lir.Program.CachedLiteralRecipe,
        policy: lir.CheckedPipeline.TargetConfig,
    ) lir.CheckedPipeline.LowerResourceError!void {
        if (uses.len == 0 and recipes.len == 0) return;
        var target = policy;
        target.spec_cache = null;
        target.root_use_cache = null;
        target.code_provision = .source_bodies;
        target.checked_module_state = .checking_finalization;
        target.completed_scalar_values = null;
        target.layout_request_const_plans = false;
        // A projection can run inside a specialization worker's lookup, which
        // cannot wait on its own executor.
        target.post_check_executor = null;
        target.timing = null;
        const mono = try lir.CheckedPipeline.prepareCheckedModulesMonotype(self.allocator, modules, .{
            .cached_root_declarations = uses,
            .cached_literal_declarations = recipes,
        }, target);
        var solved = try lir.CheckedPipeline.prepareMonotypeToSolved(mono);
        defer solved.deinit();
        var projected = try @import("postcheck").SolvedLirLower.runBorrowed(self.allocator, target.target_usize, &solved.program, .{
            .layout_request_const_plans = false,
            .root_manifest = .{ .roots = &.{}, .literal_roots = false, .runtime_schema_requests = false },
        });
        defer projected.deinit();
        for (projected.lir_result.store.getProcSpecs()) |proc| {
            const declaration = proc.root_materialization orelse continue;
            try self.pinMaterializer(declaration);
        }
        for (projected.lir_result.requested_layouts.items) |request| {
            if (request.materialization) |declaration| try self.pinMaterializer(declaration);
        }
    }

    fn pinMaterializer(self: *LoadedPacks, declaration: lir.Program.RootMaterialization) Allocator.Error!void {
        const entry = try self.materializers.getOrPut(declaration.wrapper_identity);
        if (entry.found_existing) {
            std.debug.assert(std.meta.eql(entry.value_ptr.*, declaration));
            if (!std.meta.eql(entry.value_ptr.*, declaration)) unreachable;
        } else entry.value_ptr.* = declaration;
    }

    fn projectPendingMaterializers(self: *LoadedPacks, pack: *const PackFile.Pack, summaries: []const ServingDemand) Allocator.Error!void {
        const policy = self.projection_target orelse return;
        const modules = self.projection_modules orelse return;
        var unique = std.AutoHashMap(Common.CheckedRootUse, void).init(self.allocator);
        defer unique.deinit();
        var uses = std.ArrayList(Common.CheckedRootUse).empty;
        defer uses.deinit(self.allocator);
        var recipes = std.ArrayList(lir.Program.CachedLiteralRecipe).empty;
        defer recipes.deinit(self.allocator);
        var literal_keys = std.AutoHashMap(LiteralProducer, void).init(self.allocator);
        defer literal_keys.deinit();
        for (pack.set.artifacts, summaries) |artifact, summary| {
            if (!summary.complete) continue;
            for (artifact.materializations) |dependency| {
                const declaration = dependency.materialization;
                if (self.materializers.contains(declaration.wrapper_identity)) continue;
                // Identity directories retain earlier module revisions. Those
                // offers cannot demand storage from this compilation's inputs.
                if (!moduleAvailable(modules, declaration.module)) continue;
                if (declaration.producer == .literal) {
                    for (summary.literal_reads) |read| {
                        if (!literalRecipeMatchesMaterializer(declaration, read.recipe)) continue;
                        const key = LiteralProducer{ .key = read.recipe.producer_key, .identity = read.recipe.object_identity };
                        if ((try literal_keys.getOrPut(key)).found_existing) continue;
                        try recipes.append(self.allocator, read.recipe);
                    }
                    continue;
                }
                const use = Common.CheckedRootUse{ .module = declaration.module, .root = declaration.producer.checked };
                if ((try unique.getOrPut(use)).found_existing) continue;
                try uses.append(self.allocator, use);
            }
        }
        if (uses.items.len == 0 and recipes.items.len == 0) return;
        self.projectMaterializers(modules, uses.items, recipes.items, policy) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            // Invalid checked input cannot certify its materializer. Ordinary
            // siblings without that dependency still undergo normal admission.
            error.HostedFunctionNotBound => return,
        };
    }

    fn moduleAvailable(modules: lir.CheckedPipeline.CheckedModuleSet, identity: [32]u8) bool {
        if (std.mem.eql(u8, &modules.root.module.key.bytes, &identity)) return true;
        for (modules.imports) |view| if (std.mem.eql(u8, &view.key.bytes, &identity)) return true;
        for (modules.root.relation_modules) |view| if (std.mem.eql(u8, &view.key.bytes, &identity)) return true;
        return false;
    }

    const ServingDemand = struct {
        checked_roots: []const Common.CheckedRootUse,
        literal_reads: []const lir.Program.CachedLiteralRead,
        complete: bool,
    };

    const LiteralProducer = struct {
        key: [32]u8,
        identity: [32]u8,
    };

    const LiteralReadKey = struct {
        producer: LiteralProducer,
        /// Keep distinct diagnostic causes without relying on pack-local order.
        cause: [32]u8,
        /// Digest of the read's recipe, so reads of one producer recorded
        /// by different programs stay distinct.
        recipe_digest: [32]u8,
    };

    const LiteralRead = struct {
        key: LiteralReadKey,
        read: lir.Program.CachedLiteralRead,
    };

    fn literalMaterializersDeclared(artifact: backend.dev.ProcArtifact.Artifact, reads: []const lir.Program.CachedLiteralRead) bool {
        for (artifact.materializations) |dependency| {
            const declaration = dependency.materialization;
            if (declaration.producer != .literal) continue;
            var declared = false;
            for (reads) |read| {
                if (literalRecipeMatchesMaterializer(declaration, read.recipe)) {
                    declared = true;
                    break;
                }
            }
            if (!declared) return false;
        }
        return true;
    }

    fn literalRecipeMatchesMaterializer(declaration: lir.Program.RootMaterialization, recipe: lir.Program.CachedLiteralRecipe) bool {
        if (declaration.producer != .literal) return false;
        const producer = declaration.producer.literal;
        return recipe.checked_expr == producer.checked_expr and recipe.kind == producer.kind and
            std.mem.eql(u8, &recipe.module, &declaration.module) and
            std.mem.eql(u8, &recipe.producer_key, &producer.conversion_identity);
    }

    /// The serving demand of every artifact of a set: the checked roots and
    /// literal reads its closure materializes, and whether that closure's
    /// offers are complete. `settled` holds the demand of the set's first
    /// artifacts, which an earlier indexing found; nothing they reach was
    /// added since, so they take part only as settled callees and only the
    /// later artifacts' components are summed. A component's demand is a bit
    /// set over the distinct roots and reads in view, so taking in a
    /// callee's demand costs one word per sixty-four of them.
    fn servingDemandSummaries(
        self: *LoadedPacks,
        artifacts: []const backend.dev.ProcArtifact.Artifact,
        edges: []const []const u32,
        specs: []const PackFile.SpecEntry,
        materializer_reads: []const PackFile.MaterializerRead,
        settled: []const ServingDemand,
    ) Allocator.Error![]const ServingDemand {
        const from = settled.len;
        var scratch = collections.SingleThreadArena.init(self.allocator);
        defer scratch.deinit();
        const a = scratch.allocator();
        const components = try newComponents(a, edges, from);
        const count = components.count();
        const new_count = artifacts.len - from;

        // Every distinct root and read in view, each with its first id, and
        // what each new node contributes by itself.
        var roots = DemandIndex(Common.CheckedRootUse, Common.CheckedRootUse){ .allocator = a };
        var reads = DemandIndex(LiteralReadKey, LiteralRead){ .allocator = a };
        const node_roots = try a.alloc(std.ArrayList(u32), new_count);
        @memset(node_roots, .empty);
        const node_reads = try a.alloc(std.ArrayList(u32), new_count);
        @memset(node_reads, .empty);
        const node_complete = try a.alloc(bool, new_count);
        @memset(node_complete, true);
        const trace = std.c.getenv("ROC_PACK_TRACE") != null;
        var reads_by_wrapper = std.AutoHashMap([32]u8, PackFile.MaterializerRead).init(a);
        for (materializer_reads) |entry| {
            const slot = try reads_by_wrapper.getOrPut(entry.wrapper_identity);
            if (!slot.found_existing) slot.value_ptr.* = entry;
        }
        for (specs) |spec| {
            if (spec.artifact < from) continue;
            const node = spec.artifact - from;
            const demand = spec.demand orelse {
                if (trace) std.debug.print("incomplete node={d} reason=no-demand key={x}\n", .{ spec.artifact, spec.key[0..8] });
                node_complete[node] = false;
                continue;
            };
            if (trace and !demand.permitsRetirement()) std.debug.print("incomplete node={d} reason=no-retirement key={x}\n", .{ spec.artifact, spec.key[0..8] });
            node_complete[node] = node_complete[node] and demand.permitsRetirement();
            for (demand.literal_reads) |read| {
                const key = try literalReadKey(a, read);
                try node_reads[node].append(a, try reads.id(key, .{ .key = key, .read = read }));
            }
        }
        for (artifacts[from..], 0..) |artifact, node| {
            for (artifact.materializations) |dependency| {
                const declaration = dependency.materialization;
                switch (declaration.producer) {
                    .checked => |root| {
                        const use = Common.CheckedRootUse{ .module = declaration.module, .root = root };
                        try node_roots[node].append(a, try roots.id(use, use));
                    },
                    // Code that reads a literal carries that literal's read in
                    // its own pack, so serving it declares the literal even
                    // where no offer's demand reached the read (the body of a
                    // lambda its keyed caller applies). A materializer's
                    // identity fixes the literal's declaration, so the first
                    // read recorded for it stands for every pack's.
                    .literal => if (reads_by_wrapper.get(declaration.wrapper_identity)) |entry| {
                        if (!literalRecipeMatchesMaterializer(declaration, entry.read.recipe)) {
                            if (trace) std.debug.print("incomplete node={d} reason=materializer-read-mismatch\n", .{from + node});
                            node_complete[node] = false;
                        } else {
                            const key = try literalReadKey(a, entry.read);
                            try node_reads[node].append(a, try reads.id(key, .{ .key = key, .read = entry.read }));
                        }
                    },
                }
            }
            for (edges[from + node]) |callee| {
                if (callee >= from) continue;
                const summary = settled[callee];
                if (trace and node_complete[node] and !summary.complete) std.debug.print("incomplete node={d} reason=settled-callee callee={d}\n", .{ from + node, callee });
                node_complete[node] = node_complete[node] and summary.complete;
                for (summary.checked_roots) |use| try node_roots[node].append(a, try roots.id(use, use));
                for (summary.literal_reads) |read| {
                    const key = try literalReadKey(a, read);
                    try node_reads[node].append(a, try reads.id(key, .{ .key = key, .read = read }));
                }
            }
        }

        // Ranks list the distinct items in output order, so a component's
        // lists come out sorted by walking its bits.
        const root_rank = try roots.ranks(a, rootUseLessThan);
        const read_rank = try reads.ranks(a, literalReadLessThan);
        var root_bits = try DemandBits.init(a, count, roots.items.items.len);
        var read_bits = try DemandBits.init(a, count, reads.items.items.len);
        const complete = try a.alloc(bool, count);
        @memset(complete, true);
        for (0..new_count) |node| {
            const component = components.component_of[node];
            complete[component] = complete[component] and node_complete[node];
            for (node_roots[node].items) |id| root_bits.set(component, root_rank[id]);
            for (node_reads[node].items) |id| read_bits.set(component, read_rank[id]);
        }
        var component: usize = count;
        while (component != 0) {
            component -= 1;
            for (components.membersOf(component)) |member| for (edges[from + member]) |callee| {
                if (callee < from) continue;
                const callee_component = components.component_of[callee - from];
                if (callee_component == component) continue;
                root_bits.unionInto(component, callee_component);
                read_bits.unionInto(component, callee_component);
                complete[component] = complete[component] and complete[callee_component];
            };
        }

        const summaries = try a.alloc(ServingDemand, count);
        const authority = self.authority_arena.allocator();
        const by_rank_roots = try a.alloc(Common.CheckedRootUse, roots.items.items.len);
        for (roots.items.items, 0..) |use, id| by_rank_roots[root_rank[id]] = use;
        const by_rank_reads = try a.alloc(LiteralRead, reads.items.items.len);
        for (reads.items.items, 0..) |read, id| by_rank_reads[read_rank[id]] = read;
        for (summaries, 0..) |*summary, id| {
            const uses = try authority.alloc(Common.CheckedRootUse, root_bits.popCount(id));
            var next: usize = 0;
            var root_iterator = root_bits.iterator(id);
            while (root_iterator.next()) |rank| : (next += 1) uses[next] = by_rank_roots[rank];
            const literal_reads = try authority.alloc(lir.Program.CachedLiteralRead, read_bits.popCount(id));
            next = 0;
            var read_iterator = read_bits.iterator(id);
            while (read_iterator.next()) |rank| : (next += 1) {
                const read = by_rank_reads[rank];
                literal_reads[next] = read.read;
            }
            summary.* = .{ .checked_roots = uses, .literal_reads = literal_reads, .complete = complete[id] };
        }
        const nodes = try self.allocator.alloc(ServingDemand, artifacts.len);
        @memcpy(nodes[0..from], settled);
        for (components.component_of, nodes[from..]) |id, *summary| summary.* = summaries[id];
        return nodes;
    }

    /// Distinct demand items, each with the id of its first occurrence.
    fn DemandIndex(comptime Key: type, comptime Item: type) type {
        return struct {
            allocator: Allocator,
            ids: std.AutoHashMapUnmanaged(Key, u32) = .empty,
            keys: std.ArrayList(Key) = .empty,
            items: std.ArrayList(Item) = .empty,

            fn id(self: *@This(), key: Key, item: Item) Allocator.Error!u32 {
                const entry = try self.ids.getOrPut(self.allocator, key);
                if (!entry.found_existing) {
                    entry.value_ptr.* = @intCast(self.items.items.len);
                    try self.keys.append(self.allocator, key);
                    try self.items.append(self.allocator, item);
                }
                return entry.value_ptr.*;
            }

            /// Each id's position in the order `lessThan` gives the keys.
            fn ranks(self: *const @This(), allocator: Allocator, comptime lessThan: fn (void, Key, Key) bool) Allocator.Error![]u32 {
                const order = try allocator.alloc(u32, self.keys.items.len);
                for (order, 0..) |*slot, index| slot.* = @intCast(index);
                const Sort = struct {
                    keys: []const Key,
                    fn before(context: @This(), lhs: u32, rhs: u32) bool {
                        return lessThan({}, context.keys[lhs], context.keys[rhs]);
                    }
                };
                std.mem.sort(u32, order, Sort{ .keys = self.keys.items }, Sort.before);
                const rank = try allocator.alloc(u32, order.len);
                for (order, 0..) |item_id, position| rank[item_id] = @intCast(position);
                return rank;
            }
        };
    }

    /// One bit set per component over `width` demand items.
    const DemandBits = struct {
        words: []u64,
        stride: usize,

        fn init(allocator: Allocator, components: usize, width: usize) Allocator.Error!DemandBits {
            const stride = (width + 63) / 64;
            const words = try allocator.alloc(u64, components * stride);
            @memset(words, 0);
            return .{ .words = words, .stride = stride };
        }

        fn row(self: *const DemandBits, component: usize) []u64 {
            return self.words[component * self.stride ..][0..self.stride];
        }

        fn set(self: *DemandBits, component: usize, rank: u32) void {
            self.row(component)[rank / 64] |= @as(u64, 1) << @intCast(rank % 64);
        }

        fn unionInto(self: *DemandBits, component: usize, callee: usize) void {
            const target = self.row(component);
            for (target, self.row(callee)) |*word, source| word.* |= source;
        }

        fn popCount(self: *const DemandBits, component: usize) usize {
            var total: usize = 0;
            for (self.row(component)) |word| total += @popCount(word);
            return total;
        }

        const Iterator = struct {
            words: []const u64,
            index: usize = 0,
            remaining: u64,

            fn next(self: *Iterator) ?u32 {
                while (self.remaining == 0) {
                    self.index += 1;
                    if (self.index >= self.words.len) return null;
                    self.remaining = self.words[self.index];
                }
                const bit: u32 = @intCast(@ctz(self.remaining));
                self.remaining &= self.remaining - 1;
                return @intCast(self.index * 64 + bit);
            }
        };

        fn iterator(self: *const DemandBits, component: usize) Iterator {
            const words = self.row(component);
            return .{ .words = words, .remaining = if (words.len == 0) 0 else words[0] };
        }
    };

    fn rootUseLessThan(_: void, left: Common.CheckedRootUse, right: Common.CheckedRootUse) bool {
        const order = std.mem.order(u8, &left.module, &right.module);
        return if (order == .eq) left.root < right.root else order == .lt;
    }

    fn literalReadLessThan(_: void, left: LiteralReadKey, right: LiteralReadKey) bool {
        const key_order = std.mem.order(u8, &left.producer.key, &right.producer.key);
        if (key_order != .eq) return key_order == .lt;
        const identity_order = std.mem.order(u8, &left.producer.identity, &right.producer.identity);
        if (identity_order != .eq) return identity_order == .lt;
        return std.mem.order(u8, &left.cause, &right.cause) == .lt;
    }

    /// The key a literal read is distinct under, with its recipe's digest.
    fn literalReadKey(allocator: Allocator, read: lir.Program.CachedLiteralRead) Allocator.Error!LiteralReadKey {
        var cause = std.crypto.hash.Blake3.init(.{});
        std.hash.autoHashStrat(&cause, read.cause, .Deep);
        var cause_digest: [32]u8 = undefined;
        cause.final(&cause_digest);
        return .{
            .producer = .{ .key = read.recipe.producer_key, .identity = read.recipe.object_identity },
            .cause = cause_digest,
            .recipe_digest = try PackFile.recipeDigest(allocator, read.recipe),
        };
    }

    /// Whether every module an offer's demand facts name is in the program
    /// being built. Offers are keyed by declaration, so a pack an earlier
    /// revision of a module wrote can offer a specialization whose facts
    /// name that revision's roots and expressions; those ids mean nothing in
    /// any other revision. A module's key covers its imports' keys, so a
    /// current module's ids name current declarations throughout.
    fn factsNameCurrentModules(
        self: *const LoadedPacks,
        demand: lir.Program.CachedCtfeDemand,
        checked_roots: []const Common.CheckedRootUse,
        comptime_root_reads: []const lir.CheckedPipeline.ComptimeRootRead,
    ) bool {
        if (!self.hasProgram()) return true;
        for (demand.eager_reads) |read| {
            if (self.checkedArtifact(read.module) == null) return false;
            if (read.cause.module) |module| if (self.checkedArtifact(module) == null) return false;
        }
        for (demand.literal_reads) |read| {
            if (read.cause.module) |module| if (self.checkedArtifact(module) == null) return false;
            const recipe = read.recipe;
            if (self.checkedArtifact(recipe.module) == null or
                self.checkedArtifact(recipe.method_scope) == null or
                self.checkedArtifact(recipe.owner_template.artifact.bytes) == null) return false;
            for (recipe.local_contexts) |context| if (self.checkedArtifact(context.module) == null) return false;
        }
        for (checked_roots) |use| if (self.checkedArtifact(use.module) == null) return false;
        for (comptime_root_reads) |read| if (self.checkedArtifact(read.module) == null) return false;
        return true;
    }

    /// Whether a program's modules are in view. Only a store's packs hold
    /// earlier revisions of a module; an explicit pack set is taken as given.
    fn hasProgram(self: *const LoadedPacks) bool {
        return self.pending != null or self.checked_root != null;
    }

    fn checkedArtifact(self: *const LoadedPacks, identity: [32]u8) ?*const check.CheckedArtifact.CheckedModuleArtifact {
        if (self.checked_root) |root| if (std.mem.eql(u8, &root.key.bytes, &identity)) return root;
        const pending = self.pending orelse return null;
        return pending.build_env.activeArtifactByIdentity(identity);
    }

    fn validRegion(region: SourceEvents.Region, source_len: usize) bool {
        return region.start <= region.end and region.end <= source_len;
    }

    fn sourceRegion(region: @import("base").Region) SourceEvents.Region {
        return .{ .start = region.start.offset, .end = region.end.offset };
    }

    fn pinSource(self: *LoadedPacks, module: [32]u8) Allocator.Error!void {
        if (self.sources.contains(module)) return;
        const owner = self.checkedArtifact(module) orelse return;
        try self.sources.put(module, .{
            .identity = owner.key.module_identity_hash,
            .length = owner.moduleEnvConst().getSourceAll().len,
        });
    }

    fn siteKey(certificate: backend.dev.ProcArtifact.SourceEventCertificate) ?SiteKey {
        const site = certificate.descriptor.site;
        return .{
            .module = site.checked_module,
            .origin = certificate.site_origin orelse return null,
            .checked_site = if (site.has_checked_site != 0) site.checked_site else null,
        };
    }

    fn pinAuthority(self: *LoadedPacks, certificate: backend.dev.ProcArtifact.SourceEventCertificate) Allocator.Error!void {
        const descriptor = certificate.descriptor;
        if (descriptor.source.has_location != 0) try self.pinSource(descriptor.source.checked_module);
        if (siteKey(certificate)) |key| try self.pinSite(key);
        if (descriptor.failure.has_literal_rejection != 0) {
            const literal = descriptor.failure.literal_rejection;
            const key = AuthorityKey{ .module = literal.checked_module, .id = literal.checked_expr };
            if (self.literals.contains(key)) return;
            const owner = self.checkedArtifact(key.module) orelse return;
            if (key.id >= owner.checked_bodies.exprCount()) return;
            const expr = owner.checked_bodies.expr(@fromBackingInt(@intCast(key.id)));
            const kind: SourceEvents.LiteralKind = switch (expr.data) {
                .numeral => |numeral| if (numeral.plan != null) .numeral else return,
                .str_from_quote => |quote| if (quote.plan != null) .quote else return,
                .interpolation => |interpolation| if (interpolation.plan != null) .interpolation else return,
                .pending,
                .str_segment,
                .str,
                .bytes_literal,
                .lookup_local,
                .lookup_external,
                .lookup_required,
                .list,
                .empty_list,
                .tuple,
                .match_,
                .if_,
                .call,
                .record,
                .empty_record,
                .block,
                .tag,
                .nominal,
                .zero_argument_tag,
                .closure,
                .lambda,
                .binop,
                .unary_minus,
                .unary_not,
                .field_access,
                .dispatch_call,
                .structural_eq,
                .structural_hash,
                .method_eq,
                .type_dispatch_call,
                .tuple_access,
                .runtime_error,
                .crash,
                .dbg,
                .expect_err,
                .expect,
                .ellipsis,
                .anno_only,
                .break_,
                .return_,
                .row_coerce,
                .for_,
                .hosted_lambda,
                .run_low_level,
                => return,
            };
            try self.literals.put(key, .{ .kind = @backingInt(kind), .region = sourceRegion(expr.source_region) });
        }
    }

    fn pinSite(self: *LoadedPacks, key: SiteKey) Allocator.Error!void {
        if (self.sites.contains(key)) return;
        const owner = self.checkedArtifact(key.module) orelse return;
        const bodies = &owner.checked_bodies;
        const a = self.authority_arena.allocator();
        var branches = std.ArrayList(SourceEvents.Region).empty;
        var kind: SourceEvents.SiteKind = undefined;
        var region: SourceEvents.Region = undefined;
        var destructure_pattern: ?check.CheckedArtifact.CheckedPatternId = null;
        switch (key.origin) {
            .checked_expr => |raw| {
                if (raw >= bodies.exprCount()) return;
                const expr = bodies.expr(@fromBackingInt(@intCast(raw)));
                region = sourceRegion(expr.source_region);
                switch (expr.data) {
                    .match_ => |match| {
                        kind = switch (match.comptime_site_kind) {
                            .match => .match,
                            .destructure => .destructure,
                        };
                        for (match.branches) |branch| {
                            for (branch.patternsSlice(bodies)) |pattern| {
                                try branches.append(a, sourceRegion(bodies.pattern(pattern.pattern).source_region));
                            }
                        }
                        if (kind == .destructure and match.branches.len == 1) {
                            const patterns = match.branches[0].patternsSlice(bodies);
                            if (patterns.len == 1) destructure_pattern = patterns[0].pattern;
                        }
                    },
                    .if_ => |if_expr| {
                        kind = .if_;
                        for (if_expr.branches) |branch| {
                            try branches.append(a, sourceRegion(bodies.expr(branch.cond).source_region));
                        }
                        try branches.append(a, sourceRegion(bodies.expr(if_expr.final_else).source_region));
                    },
                    .pending,
                    .numeral,
                    .str_from_quote,
                    .str_segment,
                    .str,
                    .bytes_literal,
                    .lookup_local,
                    .lookup_external,
                    .lookup_required,
                    .list,
                    .empty_list,
                    .tuple,
                    .call,
                    .record,
                    .empty_record,
                    .block,
                    .tag,
                    .nominal,
                    .zero_argument_tag,
                    .closure,
                    .lambda,
                    .binop,
                    .unary_minus,
                    .unary_not,
                    .field_access,
                    .dispatch_call,
                    .interpolation,
                    .structural_eq,
                    .structural_hash,
                    .method_eq,
                    .type_dispatch_call,
                    .tuple_access,
                    .runtime_error,
                    .crash,
                    .dbg,
                    .expect_err,
                    .expect,
                    .ellipsis,
                    .anno_only,
                    .break_,
                    .return_,
                    .row_coerce,
                    .for_,
                    .hosted_lambda,
                    .run_low_level,
                    => return,
                }
            },
            .checked_pattern => |raw| {
                if (raw >= bodies.patternCount()) return;
                const id: check.CheckedArtifact.CheckedPatternId = @fromBackingInt(@intCast(raw));
                kind = .destructure;
                region = sourceRegion(bodies.pattern(id).source_region);
                destructure_pattern = id;
                try branches.append(a, region);
            },
        }
        if (key.checked_site) |raw| {
            if (raw >= owner.exhaustiveness_sites.sites.len) return;
            const checked_site = owner.exhaustiveness_sites.sites[raw];
            const checked_kind: SourceEvents.SiteKind = switch (checked_site.kind) {
                .match => .match,
                .destructure => .destructure,
            };
            if (kind != checked_kind) return;
            if (kind == .destructure) {
                if (destructure_pattern == null or checked_site.checked_pattern != destructure_pattern) return;
                if (key.origin == .checked_pattern) region = sourceRegion(checked_site.region);
            } else {
                if (key.origin != .checked_expr or checked_site.checked_expr == null or
                    @backingInt(checked_site.checked_expr.?) != key.origin.checked_expr) return;
            }
        }
        try self.sites.put(key, .{ .kind = @backingInt(kind), .region = region, .branches = branches.items });
    }

    /// Load the pending store's packs for the root module and every module
    /// it lowers against, then index. Runs once, at the first lookup after
    /// checking has produced the module set; before that a lookup finds
    /// nothing.
    pub fn ensureLoaded(self: *LoadedPacks) void {
        if (@atomicLoad(@TypeOf(self.state), &self.state, .acquire) != .pending) return;
        while (!self.load_mutex.tryLock()) std.atomic.spinLoopHint();
        defer self.load_mutex.unlock();
        self.ensureLoadedLocked();
    }

    fn ensureLoadedLocked(self: *LoadedPacks) void {
        if (self.state != .pending) return;
        if (self.deferred) |deferred| {
            // Admission projects materializers against the prepared program.
            const saved = self.projection_modules;
            if (saved == null) self.projection_modules = deferred.modules;
            defer self.projection_modules = saved;
            self.ensureLoadedFor(deferred.root);
            return;
        }
        const pending = self.pending orelse return;
        // The first lookups come from the compile-time evaluator's program,
        // which lowers inside checking, after the root module's artifact
        // exists and before the build has declared its artifacts final.
        const root_semantic = pending.build_env.getExecutableRootSemanticData() orelse return;
        const root_artifact = root_semantic.checked_artifact orelse return;
        self.ensureLoadedFor(root_artifact);
    }

    /// Checking supplies its producer root before executable semantic data is
    /// published. Load once from that explicit capability, never during lookup.
    pub fn ensureLoadedFor(self: *LoadedPacks, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) void {
        if (self.state != .pending) return;
        const pending = self.pending orelse return;
        self.checked_root = root_artifact;
        self.ensureDiscoveryLoadedFor(root_artifact);
        self.loadPending(pending, root_artifact) catch |err| {
            self.unavailable(err);
            return;
        };
        for (self.staged.items, 0..) |bytes, index| {
            self.addSource(bytes) catch |err| {
                for (self.staged.items[index + 1 ..]) |rest| self.allocator.free(rest);
                self.staged.clearRetainingCapacity();
                self.unavailable(err);
                return;
            };
        }
        self.staged.clearRetainingCapacity();
        // Store packs are admitted at the lookups that select them.
        self.indexPacks() catch |err| {
            self.unavailable(err);
        };
    }

    fn unavailable(self: *LoadedPacks, err: LoadError) void {
        @atomicStore(@TypeOf(self.state), &self.state, .unavailable, .release);
        self.failure = err;
        std.log.warn("object cache unavailable for this build: {}", .{err});
    }

    fn loadPending(self: *LoadedPacks, pending: Pending, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) LoadError!void {
        const placements = try self.pendingPlacements(pending, root_artifact);
        defer self.allocator.free(placements);
        for (placements) |placement| try pending.store.loadIdentity(pending.io, self, placement.origin, placement.identity);
        try pending.store.loadArtifacts(pending.io, self);
    }

    pub fn deinit(self: *LoadedPacks) void {
        self.sources.deinit();
        self.sites.deinit();
        self.literals.deinit();
        self.materializers.deinit();
        self.authority_arena.deinit();
        self.artifacts.deinit();
        self.specs.deinit();
        self.discovery.deinit(self.allocator);
        self.materializer_reads.deinit(self.allocator);
        self.semantic_valid.deinit();
        if (self.deferred) |*deferred| deferred.arena.deinit();
        for (self.staged.items) |bytes| self.allocator.free(bytes);
        self.staged.deinit(self.allocator);
        for (self.store_packs.items) |*source| {
            switch (source.contents) {
                .bytes => |owned| self.allocator.free(owned),
                .mapped => |mapped| mapped.unmap(),
            }
            source.decoded.deinit(self.allocator);
            source.admitted_roots.deinit(self.allocator);
            source.arena.deinit();
        }
        self.store_packs.deinit(self.allocator);
        self.carriers.deinit(self.allocator);
        self.position.deinit(self.allocator);
        self.offered.deinit(self.allocator);
        self.store_artifacts.deinit(self.allocator);
        self.store_addresses.deinit(self.allocator);
        self.store_specs.deinit(self.allocator);
        self.store_edges.deinit(self.allocator);
        self.store_unresolved.deinit(self.allocator);
        self.store_materializer_reads.deinit(self.allocator);
        var lists = self.candidates.valueIterator();
        while (lists.next()) |list| list.deinit(self.allocator);
        self.candidates.deinit(self.allocator);
        self.closure_sizes.deinit(self.allocator);
        self.closure_scratch.deinit(self.allocator);
        var identity_lists = self.identity_candidates.valueIterator();
        while (identity_lists.next()) |list| list.deinit(self.allocator);
        self.identity_candidates.deinit(self.allocator);
        for (self.packs.items) |pack| {
            pack.deinit();
            self.allocator.destroy(pack);
        }
        self.packs.deinit(self.allocator);
        for (self.pack_states.items) |*state| {
            state.summaries.deinit(self.allocator);
            state.valid.deinit(self.allocator);
        }
        self.pack_states.deinit(self.allocator);
    }

    /// Whether a loaded pack carries the artifact at `address`, so a program
    /// publishing it names it instead of carrying it again.
    pub fn carries(self: *const LoadedPacks, address: [32]u8) bool {
        return self.carriers.contains(address);
    }

    /// The lookup Monotype consults at reservation.
    pub fn specCacheLookup(self: *LoadedPacks) Common.SpecCacheLookup {
        return .{ .context = @ptrCast(self), .find = findSpec, .find_discovery = findDiscovery, .find_identity = findIdentity };
    }

    /// The artifact source the object compiler splices from.
    pub fn spliceSource(self: *LoadedPacks) backend.dev.SpliceSource {
        return .{ .context = @ptrCast(self), .find = findArtifact };
    }

    fn findSpec(context: *anyopaque, key: [32]u8, current_relation: ?[32]u8) ?Common.SpecCacheHit {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        if (self.revoked) return null;
        // Admission changes the index, and lookups come from parallel
        // specialization workers.
        while (!self.load_mutex.tryLock()) std.atomic.spinLoopHint();
        defer self.load_mutex.unlock();
        self.ensureLoadedLocked();
        if (self.state != .ready) return null;
        // Admission projects materializers against the prepared program.
        const saved_modules = self.projection_modules;
        if (saved_modules == null) if (self.deferred) |deferred| {
            self.projection_modules = deferred.modules;
        };
        defer self.projection_modules = saved_modules;
        self.admitKey(.key, key, current_relation) catch |err| {
            self.unavailable(err);
            return null;
        };
        // Retain each relation's offer separately: an incompatible earlier
        // pack must not hide a compatible later pack for the reservation.
        const related = OfferKey{ .key = key, .platform_requirement_relation = current_relation };
        const independent = OfferKey{ .key = key, .platform_requirement_relation = null };
        const selected = if (self.specs.contains(related)) related else independent;
        const hit = self.specs.get(selected) orelse return null;
        if (hit.demand) |demand| if (demand.literal_reads.len != 0) {
            if (!(self.semantic_valid.get(selected) orelse false)) return null;
        };
        self.hits += 1;
        return hit;
    }

    /// The offer of the procedure with `identity`, admitting its closure
    /// from the store packs offering it (design.md "Identity Offers").
    fn findIdentity(context: *anyopaque, identity: [32]u8, current_relation: ?[32]u8) ?Common.SpecCacheHit {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        if (self.revoked) return null;
        while (!self.load_mutex.tryLock()) std.atomic.spinLoopHint();
        defer self.load_mutex.unlock();
        self.ensureLoadedLocked();
        if (self.state != .ready) return null;
        const saved_modules = self.projection_modules;
        if (saved_modules == null) if (self.deferred) |deferred| {
            self.projection_modules = deferred.modules;
        };
        defer self.projection_modules = saved_modules;
        self.admitKey(.identity, identity, current_relation) catch |err| {
            self.unavailable(err);
            return null;
        };
        const selected = self.artifacts.get(.{ .bytes = identity }) orelse return null;
        const hit = selected.hit;
        // Code that depends on a platform/app relation serves only that
        // relation; independent code serves every program.
        if (hit.platform_requirement_relation) |relation| {
            const current = current_relation orelse return null;
            if (!std.mem.eql(u8, &relation, &current)) return null;
        }
        self.hits += 1;
        self.identity_hits += 1;
        return hit;
    }

    fn findDiscovery(context: *anyopaque, key: [32]u8, current_relation: ?[32]u8) bool {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        if (self.revoked) return false;
        if (!@atomicLoad(bool, &self.discovery_loaded, .acquire)) {
            while (!self.load_mutex.tryLock()) std.atomic.spinLoopHint();
            defer self.load_mutex.unlock();
            const root = if (self.deferred) |deferred| deferred.root else self.checked_root orelse return false;
            self.ensureDiscoveryLoadedFor(root);
        }
        if (self.state == .unavailable) return false;
        return self.discovery.contains(.{ .key = key, .platform_requirement_relation = current_relation }) or
            self.discovery.contains(.{ .key = key, .platform_requirement_relation = null });
    }

    /// The code of a specialization this cache served. A revoked session
    /// serves no new specialization, but the code of the ones it served
    /// before the revocation stays available to the program that took them.
    fn findArtifact(context: *anyopaque, identity: lir.ProcIdentity) ?backend.dev.LocatedArtifact {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        if (self.revoked and self.state == .pending) return null;
        while (!self.load_mutex.tryLock()) std.atomic.spinLoopHint();
        defer self.load_mutex.unlock();
        self.ensureLoadedLocked();
        if (self.state != .ready) return null;
        const selected = self.artifacts.get(identity) orelse return null;
        self.artifacts_served += 1;
        return selected.artifact;
    }
};

test "pack literal offers require the full transitive exact producer closure" {
    const allocator = std.testing.allocator;
    var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
    defer packs.deinit();
    const Fixture = struct {
        fn read(key: [32]u8, identity: [32]u8) lir.Program.CachedLiteralRead {
            return testLiteralRead(key, identity);
        }

        fn hit(identity: [32]u8, reads: []const lir.Program.CachedLiteralRead) Common.SpecCacheHit {
            return .{
                .identity = identity,
                .demand = .{ .complete = true, .literal_reads = reads },
                .rc_borrowed_params = 0,
                .rc_ret_borrowed = false,
                .rc_ret_lenders = 0,
                .rc_read_only_params = 0,
                .rc_ret_unique = false,
                .rc_ret_unique_fields = 0,
                .rc_ret_conditions = &.{},
            };
        }
    };
    const first = [_]lir.Program.CachedLiteralRead{Fixture.read(@splat(2), @splat(12))};
    const second = [_]lir.Program.CachedLiteralRead{Fixture.read(@splat(3), @splat(13))};
    try std.testing.expect(try first[0].recipe.valid(std.testing.allocator));
    try packs.specs.put(.{ .key = @splat(1), .platform_requirement_relation = null }, Fixture.hit(@splat(11), &first));
    try packs.specs.put(.{ .key = @splat(2), .platform_requirement_relation = null }, Fixture.hit(@splat(12), &second));
    try packs.specs.put(.{ .key = @splat(4), .platform_requirement_relation = null }, Fixture.hit(@splat(14), &.{}));
    packs.state = .ready;
    try packs.certifyLiteralOffers();
    try std.testing.expect(LoadedPacks.findSpec(&packs, @splat(1), null) == null);
    try std.testing.expect(LoadedPacks.findSpec(&packs, @splat(2), null) == null);
    try std.testing.expect(LoadedPacks.findSpec(&packs, @splat(4), null) != null);
    // A logical key with the wrong physical object identity is not the producer.
    try packs.specs.put(.{ .key = @splat(3), .platform_requirement_relation = null }, Fixture.hit(@splat(99), &.{}));
    try packs.certifyLiteralOffers();
    try std.testing.expect(LoadedPacks.findSpec(&packs, @splat(1), null) == null);
    try packs.specs.put(.{ .key = @splat(3), .platform_requirement_relation = null }, Fixture.hit(@splat(13), &.{}));
    try packs.certifyLiteralOffers();
    try std.testing.expect(LoadedPacks.findSpec(&packs, @splat(1), null) != null);
    const recursive = [_]lir.Program.CachedLiteralRead{Fixture.read(@splat(1), @splat(11))};
    try packs.specs.put(.{ .key = @splat(3), .platform_requirement_relation = null }, Fixture.hit(@splat(13), &recursive));
    try packs.certifyLiteralOffers();
    try std.testing.expect(LoadedPacks.findSpec(&packs, @splat(1), null) != null);
}

/// The literal representations of a `{} -> U64` producer and its `U64` result.
const test_literal_representations = struct {
    const ConstType = check.ConstStore.ConstType;
    const result_types = [_]ConstType{.{ .primitive = .u64 }};
    const number: check.ConstStore.ConstTypeId = @fromBackingInt(@intCast(result_types.len - 1));
    const callable_types = result_types ++ [_]ConstType{.{ .func = .{ .args = .{ .start = 0, .len = 0 }, .ret = number } }};
    const callable_root: check.ConstStore.ConstTypeId = @fromBackingInt(@intCast(callable_types.len - 1));
    const callable: lir.Program.LiteralRepresentation = .{ .root = callable_root, .types = &callable_types };
    const result: lir.Program.LiteralRepresentation = .{ .root = number, .types = &result_types };
};

/// The serving edges of a closure's set, as a pack records them.
fn testEdges(allocator: Allocator, closure: *const backend.dev.ArtifactClosure) Allocator.Error![]const []const u32 {
    const edges = try allocator.alloc([]const u32, closure.set.artifacts.len);
    for (closure.set.artifacts, edges, 0..) |artifact, *callees, index| {
        var list = std.ArrayList(u32).empty;
        for (artifact.refs) |ref| try list.append(allocator, ref.target);
        for (closure.symbolicTargets(@intCast(index))) |target| if (target) |node| try list.append(allocator, node);
        for (closure.dataTargets(@intCast(index))) |target| if (target) |node| try list.append(allocator, node);
        callees.* = try list.toOwnedSlice(allocator);
    }
    return edges;
}

fn freeTestEdges(allocator: Allocator, edges: []const []const u32) void {
    for (edges) |callees| allocator.free(callees);
    allocator.free(edges);
}

fn testLiteralRead(key: [32]u8, identity: [32]u8) lir.Program.CachedLiteralRead {
    return .{
        .recipe = .{
            .module = @splat(7),
            .checked_expr = 0,
            .kind = .numeral,
            .producer_key = key,
            .object_identity = identity,
            .owner_template = .{ .artifact = .{ .bytes = @splat(7) }, .proc_base = @fromBackingInt(@intCast(2)), .template = @fromBackingInt(@intCast(3)) },
            .owner_context_fn_key = .{ .bytes = @splat(8) },
            .current_fn_key = .{ .bytes = @splat(9) },
            .method_scope = @splat(7),
            .checked_callable_ty = @fromBackingInt(@intCast(4)),
            .callable = test_literal_representations.callable,
            .result = test_literal_representations.result,
            .evidence_frames = &.{.{ .scope_id = .root, .parent = null, .roots_start = 0, .roots_len = 0 }},
            .evidence_frame_head = 0,
        },
        .cause = .{ .module = @splat(7), .region = .zero(), .line = 1, .column = 1 },
    };
}

test "pack machine callee literals close empty caller demand and recursive SCCs" {
    const allocator = std.testing.allocator;
    const literal = testLiteralRead(@splat(31), lir.ProcIdentity.forTest(131).bytes);
    const Fixture = struct {
        fn artifact(id: u32, refs: []const backend.dev.ProcArtifact.Reference) backend.dev.ProcArtifact.Artifact {
            return .{
                .kind = .{ .proc = lir.ProcIdentity.forTest(id) },
                .code = "\xe8\x00\x00\x00\x00\xc3",
                .entry = 0,
                .frame = null,
                .refs = refs,
                .relocations = &.{},
                .data = &.{},
                .callable_contract = @splat(1),
                .context_contract = .{
                    .target = .x64linux,
                    .cpu_level = .default,
                    .hot_reload = false,
                    .default_platform_runtime = false,
                },
            };
        }

        fn spec(key: [32]u8, artifact_index: u32, reads: []const lir.Program.CachedLiteralRead) PackFile.SpecEntry {
            return .{
                .key = key,
                .artifact = artifact_index,
                .demand = .{ .complete = true, .literal_reads = reads },
                .rc_borrowed_params = 0,
                .rc_ret_borrowed = false,
                .rc_ret_lenders = 0,
                .rc_read_only_params = 0,
                .rc_ret_unique = false,
                .rc_ret_unique_fields = 0,
                .rc_ret_conditions = &.{},
            };
        }
    };
    for ([_]bool{ false, true }) |recursive| {
        for (0..3) |mode| {
            var artifacts = [_]backend.dev.ProcArtifact.Artifact{
                Fixture.artifact(132, &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }}),
                Fixture.artifact(133, if (recursive) &.{.{ .site = 0, .form = .call, .target = 0, .delta = 0 }} else &.{}),
                Fixture.artifact(134, &.{}),
                Fixture.artifact(if (mode == 1) 999 else 131, &.{}),
            };
            var set = backend.dev.ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &artifacts };
            defer set.deinit();
            const specs = [_]PackFile.SpecEntry{
                Fixture.spec(@splat(32), 0, &.{}),
                Fixture.spec(@splat(33), 1, &.{literal}),
                Fixture.spec(@splat(34), 2, &.{}),
                Fixture.spec(@splat(31), 3, &.{}),
            };
            const bytes = try PackFile.write(allocator, &set, specs[0..if (mode == 0) 3 else 4], PackFile.test_version_hash);
            defer allocator.free(bytes);
            var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
            defer packs.deinit();
            try packs.retain(bytes);
            const caller_key = LoadedPacks.OfferKey{ .key = @splat(32), .platform_requirement_relation = null };
            const callee_key = LoadedPacks.OfferKey{ .key = @splat(33), .platform_requirement_relation = null };
            const caller = packs.specs.get(caller_key).?;
            const callee = packs.specs.get(callee_key).?;
            try std.testing.expectEqual(@as(usize, 1), caller.demand.?.literal_reads.len);
            try std.testing.expectEqualDeep(literal, caller.demand.?.literal_reads[0]);
            if (recursive) try std.testing.expectEqual(true, caller.demand.?.literal_reads.ptr == callee.demand.?.literal_reads.ptr);
            const matched = mode == 2;
            try std.testing.expectEqual(matched, packs.specCacheLookup().lookup(@splat(32), null) != null);
            try std.testing.expectEqual(matched, packs.specCacheLookup().lookup(@splat(33), null) != null);
            try std.testing.expectEqual(true, packs.specCacheLookup().lookup(@splat(34), null) != null);
            if (matched) {
                // Declaration recipes borrow only retained decoded owners,
                // never the caller's source body or this encoded byte buffer.
                const hit = packs.specCacheLookup().lookup(@splat(32), null).?;
                try std.testing.expectEqualDeep(literal, hit.demand.?.literal_reads[0]);
            }
        }
    }
}

test "pack literal materializers require a recipe in their own serving closure" {
    const allocator = std.testing.allocator;
    var packs = LoadedPacks.init(allocator, PackFile.test_version_hash);
    defer packs.deinit();
    const read = testLiteralRead(@splat(41), lir.ProcIdentity.forTest(141).bytes);
    var other_cause = read;
    other_cause.cause.line = 2;
    const declaration = lir.Program.RootMaterialization{
        .module = read.recipe.module,
        .producer = .{ .literal = .{
            .checked_expr = read.recipe.checked_expr,
            .kind = read.recipe.kind,
            .conversion_identity = read.recipe.producer_key,
        } },
        .representation_digest = @splat(42),
        .layout_digest = @splat(43),
        .wrapper_identity = @splat(44),
    };
    const dependency = backend.dev.ProcArtifact.RootMaterializationDependency{
        .binding_name = "binding",
        .header_name = "header",
        .materialization = declaration,
    };
    var artifacts = [_]backend.dev.ProcArtifact.Artifact{
        .{ .kind = .{ .proc = lir.ProcIdentity.forTest(142) }, .code = "\xe8\x00\x00\x00\x00\xc3", .entry = 0, .frame = null, .refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }}, .relocations = &.{}, .data = &.{} },
        .{ .kind = .{ .proc = lir.ProcIdentity.forTest(143) }, .code = "\xc3", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{}, .materializations = &.{dependency} },
        .{ .kind = .{ .proc = lir.ProcIdentity.forTest(144) }, .code = "\xc3", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} },
    };
    var set = backend.dev.ProcArtifact.Set{ .arena = collections.SingleThreadArena.init(allocator), .artifacts = &artifacts };
    defer set.deinit();
    var closure = try backend.dev.ArtifactClosure.init(allocator, &set);
    defer closure.deinit();
    const edges = try testEdges(allocator, &closure);
    defer freeTestEdges(allocator, edges);
    var spec = PackFile.SpecEntry{
        .key = @splat(45),
        .artifact = 2,
        .demand = .{ .complete = true, .literal_reads = &.{read} },
        .rc_borrowed_params = 0,
        .rc_ret_borrowed = false,
        .rc_ret_lenders = 0,
        .rc_read_only_params = 0,
        .rc_ret_unique = false,
        .rc_ret_unique_fields = 0,
        .rc_ret_conditions = &.{},
    };
    const unrelated = try packs.servingDemandSummaries(set.artifacts, edges, (&spec)[0..1], &.{}, &.{});
    defer allocator.free(unrelated);
    try std.testing.expect(!LoadedPacks.literalMaterializersDeclared(artifacts[1], unrelated[1].literal_reads));
    spec.artifact = 1;
    spec.demand.?.literal_reads = &.{ read, read, other_cause };
    const matched = try packs.servingDemandSummaries(set.artifacts, edges, (&spec)[0..1], &.{}, &.{});
    defer allocator.free(matched);
    try std.testing.expect(LoadedPacks.literalMaterializersDeclared(artifacts[1], matched[1].literal_reads));
    try std.testing.expectEqual(@as(usize, 2), matched[0].literal_reads.len);
    try std.testing.expect(matched[0].complete);
    // Two programs record one producer's read with different recipes; a
    // closure holding both keeps both, and the consumer declares the
    // producer from the first (`Monotype.declareCachedLiteral`).
    var differing = read;
    differing.recipe.current_fn_key.bytes[0] ^= 1;
    spec.demand.?.literal_reads = &.{ read, differing };
    const both = try packs.servingDemandSummaries(set.artifacts, edges, (&spec)[0..1], &.{}, &.{});
    defer allocator.free(both);
    try std.testing.expect(both[0].complete and both[1].complete and both[2].complete);
    try std.testing.expectEqual(@as(usize, 2), both[0].literal_reads.len);
    // A read the pack records for the materializer itself declares it in the
    // reading code's own closure, with no offer's demand naming the read.
    spec.artifact = 2;
    spec.demand.?.literal_reads = &.{};
    const recorded = [_]PackFile.MaterializerRead{.{ .wrapper_identity = declaration.wrapper_identity, .read = read }};
    const carried = try packs.servingDemandSummaries(set.artifacts, edges, (&spec)[0..1], &recorded, &.{});
    defer allocator.free(carried);
    try std.testing.expect(LoadedPacks.literalMaterializersDeclared(artifacts[1], carried[1].literal_reads));
    try std.testing.expect(carried[0].complete and carried[1].complete);
    try std.testing.expectEqual(@as(usize, 1), carried[0].literal_reads.len);
    try std.testing.expectEqual(@as(usize, 0), carried[2].literal_reads.len);
    // A recorded read for another producer declares nothing.
    var other_producer = read;
    other_producer.recipe.producer_key[0] ^= 1;
    const mismatched = [_]PackFile.MaterializerRead{.{ .wrapper_identity = declaration.wrapper_identity, .read = other_producer }};
    const refused = try packs.servingDemandSummaries(set.artifacts, edges, (&spec)[0..1], &mismatched, &.{});
    defer allocator.free(refused);
    try std.testing.expect(!refused[1].complete);
    try std.testing.expect(!LoadedPacks.literalMaterializersDeclared(artifacts[1], refused[1].literal_reads));
}

fn nameLessThan(_: void, lhs: []u8, rhs: []u8) bool {
    return std.mem.order(u8, lhs, rhs) == .lt;
}
