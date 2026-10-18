//! Packs loaded for one build: the specialization keys they can serve at
//! Monotype reservation and the artifacts the object writer splices in.

const std = @import("std");
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

/// The pack files of one compiler build, target, and optimization level,
/// under the cache root: `objects-v<format>/<target>-<opt>/<local|pkg>/<module
/// identity>/<artifact key>.rpk`. Packs are filed by module identity so an
/// edited module's previous packs stay beside its new one until the sweep
/// removes them, which is what lets unchanged specializations hit across an
/// edit. Every file is a pure function of the module and its transitive
/// imports, so two writers of one path write identical bytes and rename
/// into place without coordination.
pub const Store = struct {
    allocator: Allocator,
    roc_ctx: CoreCtx,
    root: []u8,
    verbose: bool,

    pub const InitError = Allocator.Error || error{NoHomeDirectory};

    pub fn init(allocator: Allocator, cache_config: compile.CacheConfig, target: RocTarget, opt_name: []const u8) InitError!Store {
        const version_dir = try cache_config.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);
        const mode = try std.fmt.allocPrint(allocator, "{s}-{s}", .{ @tagName(target), opt_name });
        defer allocator.free(mode);
        const namespace = compile.config.objectPackNamespace(PackFile.format_version);
        const root = try std.fs.path.join(allocator, &.{ version_dir, namespace, mode });
        return .{ .allocator = allocator, .roc_ctx = cache_config.roc_ctx, .root = root, .verbose = cache_config.verbose };
    }

    pub fn deinit(self: *Store) void {
        self.allocator.free(self.root);
    }

    /// The directory of every pack filed under a module identity.
    pub fn identityDir(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) Allocator.Error![]u8 {
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

    /// Stage the pack beside its final path and rename it into place. A
    /// pack already present is left alone: it holds the same bytes, which
    /// builds with runtime safety check.
    pub fn write(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        const dir = try self.identityDir(origin, identity);
        defer self.allocator.free(dir);
        self.roc_ctx.makePath(dir) catch |err| {
            self.reportWriteFailure("create directory", dir, bytes.len, err);
            return error.PackWriteFailed;
        };
        const path = try self.packPath(origin, identity, key);
        defer self.allocator.free(path);
        if (self.roc_ctx.fileExists(path)) {
            if (std.debug.runtime_safety) try self.verifyCollision(path, bytes);
            return;
        }
        const temp_path = try std.fmt.allocPrint(self.allocator, "{s}.tmp", .{path});
        defer self.allocator.free(temp_path);
        self.roc_ctx.writeFile(temp_path, bytes) catch |err| {
            self.reportWriteFailure("write staging file", temp_path, bytes.len, err);
            return error.PackWriteFailed;
        };
        self.roc_ctx.rename(temp_path, path) catch |err| {
            self.reportWriteFailure("publish staging file", temp_path, bytes.len, err);
            return error.PackWriteFailed;
        };
    }

    /// A pack's path is a function of its content, so the pack a writer
    /// finds already in place holds the bytes it would have written. A pack
    /// the sweep removed in the meantime has nothing left to compare.
    fn verifyCollision(self: *const Store, path: []const u8, bytes: []const u8) Allocator.Error!void {
        const existing = self.roc_ctx.readFile(path, self.allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return,
        };
        defer self.allocator.free(existing);
        if (!std.mem.eql(u8, existing, bytes)) {
            @import("base").invariant("object-cache pack {s} already holds different bytes", .{path});
        }
    }

    fn reportWriteFailure(self: *const Store, operation: []const u8, path: []const u8, byte_count: usize, err: (CoreCtx.MakePathError || CoreCtx.WriteError || CoreCtx.RenameError)) void {
        if (!self.verbose) return;
        const message = std.fmt.allocPrint(self.allocator, "Failed to {s} for object cache at {s} ({d} bytes): {}\n", .{ operation, path, byte_count, err }) catch return;
        defer self.allocator.free(message);
        self.roc_ctx.writeStderr(message) catch {};
    }

    /// Every pack filed under a module identity, in name order.
    pub fn loadIdentity(self: *const Store, io: std.Io, packs: *LoadedPacks, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) LoadedPacks.LoadError!void {
        const dir = try self.identityDir(origin, identity);
        defer self.allocator.free(dir);
        try packs.loadDirInto(io, dir, .cache_offers);
    }
};

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
        }},
    };
    const bytes = try PackFile.write(allocator, &set, &.{.{
        .key = key,
        .artifact = 0,
        .rc_borrowed_params = 1,
        .rc_ret_borrowed = false,
        .rc_ret_lenders = 0,
        .rc_read_only_params = 0,
        .rc_ret_unique = false,
        .rc_ret_unique_fields = 0,
        .rc_ret_conditions = &.{},
    }});
    defer allocator.free(bytes);
    const incompatible = try allocator.dupe(u8, bytes);
    defer allocator.free(incompatible);
    std.mem.writeInt(u32, incompatible[4..8], PackFile.format_version + 1, .little);
    const Attempt = struct {
        fn run(failing: Allocator, dir_path: []const u8, expected_key: [32]u8, expected_identity: lir.ProcIdentity) (LoadedPacks.LoadError || error{ TestExpectedEqual, TestUnexpectedResult, TestExpectedError, TestUnexpectedError })!void {
            var packs = LoadedPacks.init(failing);
            defer packs.deinit();
            packs.loadDirInto(std.testing.io, dir_path, .cache_offers) catch |err|
                return rejected(&packs, expected_key, err);
            try std.testing.expectEqual(@as(usize, 1), packs.packs.items.len);
            packs.indexPacks() catch |err|
                return rejected(&packs, expected_key, err);
            const hit = packs.specCacheLookup().lookup(expected_key, false) orelse return error.TestUnexpectedResult;
            try std.testing.expectEqualSlices(u8, &expected_identity.bytes, &hit.identity);
            try std.testing.expectEqual(@as(u64, 1), hit.rc_borrowed_params);
            const located = packs.spliceSource().find(packs.spliceSource().context, expected_key) orelse return error.TestUnexpectedResult;
            try std.testing.expectEqual(expected_identity, located.set.artifacts[located.index].kind.proc);
        }

        fn rejected(packs: *LoadedPacks, key_to_find: [32]u8, err: LoadedPacks.LoadError) (LoadedPacks.LoadError || error{ TestExpectedEqual, TestUnexpectedResult, TestExpectedError, TestUnexpectedError })!void {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expectEqual(err, packs.failure.?);
            try std.testing.expect(packs.state == .unavailable);
            try std.testing.expectError(err, packs.indexPacks());
            try std.testing.expect(packs.specCacheLookup().lookup(key_to_find, false) == null);
            const splice = packs.spliceSource();
            try std.testing.expect(splice.find(splice.context, key_to_find) == null);
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
        try std.testing.checkAllAllocationFailures(allocator, Attempt.run, .{ dir_path, key, identity });
        try std.testing.expectError(error.UnsupportedPackVersion, LoadedPacks.loadDir(allocator, io, dir_path));
    }
    try tmp.dir.createDirPath(io, "malformed");
    try tmp.dir.writeFile(io, .{ .sub_path = "malformed/broken.rpk", .data = "not a pack" });
    const malformed_path = try tmp.dir.realPathFileAlloc(io, "malformed", allocator);
    defer allocator.free(malformed_path);
    var malformed = LoadedPacks.init(allocator);
    defer malformed.deinit();
    try std.testing.expectError(error.MalformedPack, malformed.loadDirInto(io, malformed_path, .cache_offers));
    try std.testing.expectEqual(error.MalformedPack, malformed.failure.?);
    try std.testing.expect(malformed.specCacheLookup().lookup(key, false) == null);
    try std.testing.expect(malformed.spliceSource().find(malformed.spliceSource().context, key) == null);
}

test "object cache accepts new offers after indexing without moving served artifacts" {
    const allocator = std.testing.allocator;
    var packs = LoadedPacks.init(allocator);
    defer packs.deinit();
    try packs.indexPacks();
    var first: ?backend.dev.LocatedArtifact = null;
    for (0..32) |index| {
        const identity = lir.ProcIdentity.forTest(@intCast(index));
        const key = @as([32]u8, @splat(@intCast(index)));
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
            }},
        };
        const bytes = try PackFile.write(allocator, &set, &.{.{
            .key = key,
            .artifact = 0,
            .rc_borrowed_params = 0,
            .rc_ret_borrowed = false,
            .rc_ret_lenders = 0,
            .rc_read_only_params = 0,
            .rc_ret_unique = false,
            .rc_ret_unique_fields = 0,
            .rc_ret_conditions = &.{},
        }});
        defer allocator.free(bytes);
        try packs.offerPack(bytes);
        const hit = packs.specCacheLookup().lookup(key, false) orelse return error.TestUnexpectedResult;
        try std.testing.expectEqualSlices(u8, &identity.bytes, &hit.identity);
        const splice = packs.spliceSource();
        const located = splice.find(splice.context, key) orelse return error.TestUnexpectedResult;
        try std.testing.expectEqual(identity, located.set.artifacts[located.index].kind.proc);
        if (first) |served| {
            try std.testing.expectEqual(&packs.packs.items[0].set, served.set);
            try std.testing.expectEqual(lir.ProcIdentity.forTest(0), served.set.artifacts[served.index].kind.proc);
        } else first = located;
    }
}

test "object cache write failures preserve quiet behavior and report verbose causes" {
    const Capture = struct {
        stderr: std.ArrayList(u8) = .empty,
        stage: enum { directory, write, rename },

        fn makePath(context: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.MakePathError!void {
            const self: *@This() = @ptrCast(@alignCast(context.?));
            if (self.stage == .directory) return error.ReadOnlyFileSystem;
        }

        fn fileExists(_: ?*anyopaque, _: std.Io, _: []const u8) bool {
            return false;
        }

        fn writeFile(context: ?*anyopaque, _: std.Io, _: []const u8, _: []const u8) CoreCtx.WriteError!void {
            const self: *@This() = @ptrCast(@alignCast(context.?));
            if (self.stage == .write) return error.NoSpaceLeft;
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
            filesystem.vtable.writeFile = &Capture.writeFile;
            filesystem.vtable.rename = &Capture.rename;
            filesystem.vtable.writeStderr = &Capture.writeStderr;
            var store = try Store.init(allocator, .{
                .cache_dir = "cache",
                .roc_ctx = filesystem,
                .verbose = verbose,
            }, .arm64mac, "dev");
            defer store.deinit();
            for (0..2) |_| {
                try std.testing.expectError(error.PackWriteFailed, store.write(.local, @as([32]u8, @splat(1)), @as([32]u8, @splat(2)), "pack"));
            }
            if (verbose) {
                const cause = switch (stage) {
                    .directory => "error.ReadOnlyFileSystem",
                    .write => "error.NoSpaceLeft",
                    .rename => "error.DiskQuota",
                };
                try std.testing.expectEqual(@as(usize, 2), std.mem.count(u8, capture.stderr.items, cause));
                try std.testing.expectEqual(@as(usize, 2), std.mem.count(u8, capture.stderr.items, "(4 bytes)"));
                try std.testing.expect(std.mem.containsAtLeast(u8, capture.stderr.items, 1, store.root));
            } else {
                try std.testing.expectEqualStrings("", capture.stderr.items);
            }
        }
    }
}

test "the cache sweep finds and ages the packs the store writes" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const cache_root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(cache_root);
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const cache_config: compile.CacheConfig = .{
        .cache_dir = cache_root,
        .roc_ctx = CoreCtx.default(allocator, arena.allocator(), io),
    };
    var store = try Store.init(allocator, cache_config, .x64glibc, "dev");
    defer store.deinit();

    const local_identity = @as([32]u8, @splat(1));
    const pkg_identity = @as([32]u8, @splat(2));
    const key = @as([32]u8, @splat(3));
    try store.write(.local, local_identity, key, "local pack");
    try store.write(.pkg, pkg_identity, key, "pkg pack");
    // A second writer of the same pack finds it in place with its bytes.
    try store.write(.pkg, pkg_identity, key, "pkg pack");
    try std.testing.expect(try store.has(.local, local_identity, key));
    try std.testing.expect(try store.has(.pkg, pkg_identity, key));

    // A namespace of another pack format beside the store's own.
    const version_dir = try cache_config.getVersionCacheDir(allocator);
    defer allocator.free(version_dir);
    const stale_dir = try std.fs.path.join(allocator, &.{ version_dir, compile.config.objectPackNamespace(PackFile.format_version - 1), "x64glibc-dev", "pkg" });
    defer allocator.free(stale_dir);
    try std.Io.Dir.cwd().createDirPath(io, stale_dir);

    const limits: compile.CacheCleanup.Limits = .{
        .object_pack_format_version = PackFile.format_version,
        .max_size_bytes = std.math.maxInt(u64),
    };
    const now_ns = compile.CacheCleanup.nowNs(io);
    var stats: compile.CacheCleanup.CleanupStats = .{};
    compile.CacheCleanup.sweep(io, cache_root, limits, now_ns + 2 * compile.CacheCleanup.Config.LOCAL_PACK_MAX_AGE_NS, &stats);
    try std.testing.expectEqual(@as(u32, 1), stats.stale_namespaces_deleted);
    try std.testing.expectEqual(@as(u32, 1), stats.cache_files_deleted);
    try std.testing.expectError(error.FileNotFound, std.Io.Dir.cwd().access(io, stale_dir, .{}));
    try std.testing.expect(!try store.has(.local, local_identity, key));
    try std.testing.expect(try store.has(.pkg, pkg_identity, key));

    compile.CacheCleanup.sweep(io, cache_root, limits, now_ns + compile.CacheCleanup.Config.PERSISTENT_MAX_AGE_NS + std.time.ns_per_s, &stats);
    try std.testing.expect(!try store.has(.pkg, pkg_identity, key));
}

/// What a lazily loaded store needs once checking has produced the module
/// set: the store and the modules in view.
pub const Pending = struct {
    store: *const Store,
    /// Packs compile-time evaluation published from its own programs
    /// (`compileTimePackKey`), offered after the store's packs. Only a
    /// provider no runtime consumer reads loads them: their code is lowered
    /// under the compile-time LIR policy, not the runtime one.
    compile_time_store: ?*const Store = null,
    /// LLVM-compiled object packs of URL-package modules
    /// (`PackFile.ObjectPack`), which only a consumer whose code is linked
    /// takes.
    linked_store: ?*const Store = null,
    /// Whether the linked store's object packs of the root module itself, the
    /// tier 2 entries a background build compiled for earlier builds of it,
    /// are offered too.
    linked_root_objects: bool = true,
    io: std.Io,
    build_env: *compile.BuildEnv,
};

/// The file name a program's compile-time pack is filed under, beside the
/// root module's runtime packs in the compile-time store. It is distinct from
/// every runtime pack key, so a compile-time pack never stands in for the
/// root module's runtime pack.
pub fn compileTimePackKey(code_generation_key: [32]u8) [32]u8 {
    var hasher = @import("base").Sha256.init(.{});
    hasher.update("roc.compile-time-pack.v1");
    hasher.update(&code_generation_key);
    return hasher.finalResult();
}

/// Every pack in a directory, indexed by specialization key and by procedure
/// identity. Files load in name order so the indexes are deterministic when
/// two packs carry the same entry.
pub const LoadedPacks = struct {
    const LinkedOffer = struct {
        hit: Common.SpecCacheHit,
        pack: u32,
    };

    allocator: Allocator,
    packs: std.ArrayList(*PackFile.Pack),
    specs: std.AutoHashMap([32]u8, Common.SpecCacheHit),
    /// The artifact of each entry `specs` holds, by the same key.
    artifacts: std.AutoHashMap([32]u8, backend.dev.LocatedArtifact),
    /// Object packs, whose entries only a consumer whose code is linked takes.
    object_packs: std.ArrayList(PackFile.ObjectPack) = .empty,
    linked_specs: std.AutoHashMap([32]u8, LinkedOffer),
    /// Per object pack, whether an entry of it served a specialization, so
    /// the program links its object.
    object_packs_used: std.ArrayList(bool) = .empty,
    /// Object packs load at the first lookup that takes a linked entry,
    /// which comes after checking, so a build that writes a module's object
    /// pack once the module is checked serves its own program from it.
    linked_state: enum { pending, ready, unavailable } = .pending,
    /// Specializations served so far.
    hits: u64 = 0,
    /// Artifacts handed to a splice so far.
    artifacts_served: u64 = 0,
    /// Set until `indexPacks` runs; lookups before that load the pending
    /// store's packs for the modules in view first.
    pending: ?Pending = null,
    /// Neither lookup may expose a partial index or retry a failed collection.
    state: enum { pending, ready, unavailable } = .pending,
    failure: ?LoadError = null,

    pub const LoadError = Allocator.Error || PackFile.ReadError || error{PackDirectoryUnreadable};
    pub const Input = enum {
        /// Independent optional offers; an incompatible file does not veto peers.
        cache_offers,
        /// A requested pack set must be readable under the active contract.
        explicit_packs,
    };

    pub fn init(allocator: Allocator) LoadedPacks {
        return .{
            .allocator = allocator,
            .packs = .empty,
            .specs = std.AutoHashMap([32]u8, Common.SpecCacheHit).init(allocator),
            .artifacts = std.AutoHashMap([32]u8, backend.dev.LocatedArtifact).init(allocator),
            .linked_specs = std.AutoHashMap([32]u8, LinkedOffer).init(allocator),
        };
    }

    /// Load every `.rpk` file in `dir_path` and index the result.
    pub fn loadDir(allocator: Allocator, io: std.Io, dir_path: []const u8) LoadError!LoadedPacks {
        var self = LoadedPacks.init(allocator);
        errdefer self.deinit();
        try self.loadDirInto(io, dir_path, .explicit_packs);
        try self.indexPacks();
        return self;
    }

    /// Read every `.rpk` file in `dir_path`, in name order, without indexing.
    pub fn loadDirInto(self: *LoadedPacks, io: std.Io, dir_path: []const u8, input: Input) LoadError!void {
        std.debug.assert(self.state == .pending);
        self.loadDirIntoUnrecorded(io, dir_path, input) catch |err| {
            self.state = .unavailable;
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
            const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .limited(1024 * 1024 * 1024)) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return error.PackDirectoryUnreadable,
            };
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

    /// Read every object pack in `dir_path`, in name order, without
    /// indexing. A missing directory or a pack of another format is no offer.
    fn loadObjectDirIntoUnrecorded(self: *LoadedPacks, io: std.Io, dir_path: []const u8) LoadError!void {
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
                if (!std.mem.endsWith(u8, entry.name, ".rpk")) continue;
                try names.ensureUnusedCapacity(allocator, 1);
                names.appendAssumeCapacity(try allocator.dupe(u8, entry.name));
            }
        }
        std.mem.sort([]u8, names.items, {}, nameLessThan);
        for (names.items) |name| {
            const path = try std.fs.path.join(allocator, &.{ dir_path, name });
            defer allocator.free(path);
            const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .limited(1024 * 1024 * 1024)) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return error.PackDirectoryUnreadable,
            };
            defer allocator.free(bytes);
            var pack = PackFile.readObject(allocator, bytes) catch |err| switch (err) {
                error.UnsupportedPackVersion => continue,
                else => |other| return other,
            };
            errdefer pack.deinit();
            try self.object_packs_used.append(allocator, false);
            errdefer _ = self.object_packs_used.pop();
            try self.object_packs.append(allocator, pack);
        }
    }

    fn appendPack(self: *LoadedPacks, bytes: []const u8) LoadError!void {
        const pack = try self.allocator.create(PackFile.Pack);
        errdefer self.allocator.destroy(pack);
        pack.* = try PackFile.read(self.allocator, bytes);
        errdefer pack.deinit();
        try self.packs.append(self.allocator, pack);
    }

    /// Offer a pack prepared after checking to the same command's runtime
    /// lowering. Decoded packs keep stable addresses, including artifacts
    /// already handed to the compile-time evaluator.
    pub fn offerPack(self: *LoadedPacks, bytes: []const u8) LoadError!void {
        self.ensureLoaded();
        if (self.state == .unavailable) return self.failure.?;
        self.appendPack(bytes) catch |err| {
            self.unavailable(err);
            return err;
        };
        if (self.state == .ready) self.indexPack(self.packs.items[self.packs.items.len - 1]) catch |err| {
            self.unavailable(err);
            return err;
        };
    }

    /// Index every loaded pack. Each decoded pack owns its stable storage.
    pub fn indexPacks(self: *LoadedPacks) LoadError!void {
        if (self.state == .unavailable) return self.failure.?;
        std.debug.assert(self.state == .pending);
        self.indexPacksUnrecorded() catch |err| {
            self.state = .unavailable;
            self.failure = err;
            return err;
        };
    }

    fn indexPacksUnrecorded(self: *LoadedPacks) LoadError!void {
        for (self.packs.items) |pack| try self.indexPack(pack);
        self.state = .ready;
    }

    fn indexPack(self: *LoadedPacks, pack: *const PackFile.Pack) LoadError!void {
        for (pack.specs) |spec| {
            const root = pack.set.artifacts[spec.artifact];
            const identity = switch (root.kind) {
                .proc => |identity| identity,
                .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => return error.MalformedPack,
            };
            if (self.specs.contains(spec.key)) continue;
            try self.artifacts.ensureUnusedCapacity(1);
            try self.specs.ensureUnusedCapacity(1);
            self.artifacts.putAssumeCapacityNoClobber(spec.key, .{ .set = &pack.set, .index = spec.artifact });
            self.specs.putAssumeCapacityNoClobber(spec.key, .{
                .identity = identity.bytes,
                .comptime_root_reads = spec.comptime_root_reads,
                .rc_borrowed_params = spec.rc_borrowed_params,
                .rc_ret_borrowed = spec.rc_ret_borrowed,
                .rc_ret_lenders = spec.rc_ret_lenders,
                .rc_read_only_params = spec.rc_read_only_params,
                .rc_ret_unique = spec.rc_ret_unique,
                .rc_ret_unique_fields = spec.rc_ret_unique_fields,
                .rc_ret_conditions = spec.rc_ret_conditions,
            });
        }
    }

    /// Index every loaded object pack.
    fn indexObjectPacks(self: *LoadedPacks) LoadError!void {
        for (self.object_packs.items, 0..) |pack, pack_index| {
            for (pack.specs) |spec| {
                const gop = try self.linked_specs.getOrPut(spec.key);
                if (!gop.found_existing) gop.value_ptr.* = .{
                    .hit = .{
                        .identity = pack.identities[spec.artifact].bytes,
                        .c_abi_entry = true,
                        .comptime_root_reads = spec.comptime_root_reads,
                        .rc_borrowed_params = spec.rc_borrowed_params,
                        .rc_ret_borrowed = spec.rc_ret_borrowed,
                        .rc_ret_lenders = spec.rc_ret_lenders,
                        .rc_read_only_params = spec.rc_read_only_params,
                        .rc_ret_unique = spec.rc_ret_unique,
                        .rc_ret_unique_fields = spec.rc_ret_unique_fields,
                        .rc_ret_conditions = spec.rc_ret_conditions,
                    },
                    .pack = @intCast(pack_index),
                };
            }
        }
    }

    /// Load and index the pending linked store's object packs for the
    /// URL-package modules in view. Runs once, at the first lookup that
    /// takes a linked entry.
    fn ensureLinkedLoaded(self: *LoadedPacks) void {
        if (self.linked_state != .pending) return;
        self.linked_state = .unavailable;
        const pending = self.pending orelse return;
        const store = pending.linked_store orelse return;
        const root_semantic = pending.build_env.getExecutableRootSemanticData() orelse return;
        const root_artifact = root_semantic.checked_artifact orelse return;
        self.loadLinked(pending, store, root_artifact) catch |err| {
            std.log.warn("optimized object packs unavailable for this build: {}", .{err});
            return;
        };
        self.linked_state = .ready;
    }

    fn loadLinked(self: *LoadedPacks, pending: Pending, store: *const Store, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) LoadError!void {
        const artifacts = try pending.build_env.collectVisibleArtifacts(self.allocator, root_artifact);
        defer self.allocator.free(artifacts);
        for (artifacts) |artifact| {
            if (std.meta.eql(artifact.key, root_artifact.key)) continue;
            const placement = pending.build_env.packPlacementForArtifactKey(artifact.key) orelse continue;
            if (placement.origin != .pkg) continue;
            const dir = try store.identityDir(placement.origin, placement.identity);
            defer self.allocator.free(dir);
            try self.loadObjectDirIntoUnrecorded(pending.io, dir);
        }
        if (pending.linked_root_objects) {
            if (pending.build_env.packPlacementForArtifactKey(root_artifact.key)) |placement| {
                const dir = try store.identityDir(placement.origin, placement.identity);
                defer self.allocator.free(dir);
                try self.loadObjectDirIntoUnrecorded(pending.io, dir);
            }
        }
        try self.indexObjectPacks();
    }

    /// Load the pending store's packs for the root module and every module
    /// it lowers against, then index. Runs once, at the first lookup after
    /// checking has produced the module set; before that a lookup finds
    /// nothing.
    fn ensureLoaded(self: *LoadedPacks) void {
        if (self.state != .pending) return;
        const pending = self.pending orelse return;
        // The first lookups come from the compile-time evaluator's program,
        // which lowers inside checking, after the root module's artifact
        // exists and before the build has declared its artifacts final.
        const root_semantic = pending.build_env.getExecutableRootSemanticData() orelse return;
        const root_artifact = root_semantic.checked_artifact orelse return;
        self.loadPending(pending, root_artifact) catch |err| {
            self.unavailable(err);
            return;
        };
        self.indexPacks() catch |err| {
            self.unavailable(err);
        };
    }

    fn unavailable(self: *LoadedPacks, err: LoadError) void {
        self.state = .unavailable;
        self.failure = err;
        std.log.warn("object cache unavailable for this build: {}", .{err});
    }

    fn loadPending(self: *LoadedPacks, pending: Pending, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) LoadError!void {
        // Runtime packs load first, so an entry both offer is served by the
        // runtime pack.
        for ([_]?*const Store{ pending.store, pending.compile_time_store }) |maybe_store| {
            const store = maybe_store orelse continue;
            if (pending.build_env.packPlacementForArtifactKey(root_artifact.key)) |placement| {
                try store.loadIdentity(pending.io, self, placement.origin, placement.identity);
            }
            const artifacts = try pending.build_env.collectVisibleArtifacts(self.allocator, root_artifact);
            defer self.allocator.free(artifacts);
            for (artifacts) |artifact| {
                const placement = pending.build_env.packPlacementForArtifactKey(artifact.key) orelse continue;
                try store.loadIdentity(pending.io, self, placement.origin, placement.identity);
            }
        }
    }

    pub fn deinit(self: *LoadedPacks) void {
        self.linked_specs.deinit();
        for (self.object_packs.items) |*pack| pack.deinit();
        self.object_packs.deinit(self.allocator);
        self.object_packs_used.deinit(self.allocator);
        self.artifacts.deinit();
        self.specs.deinit();
        for (self.packs.items) |pack| {
            pack.deinit();
            self.allocator.destroy(pack);
        }
        self.packs.deinit(self.allocator);
    }

    /// The lookup Monotype consults at reservation.
    pub fn specCacheLookup(self: *LoadedPacks) Common.SpecCacheLookup {
        return .{ .context = @ptrCast(self), .find = findSpec };
    }

    /// The artifact source the object compiler splices from.
    pub fn spliceSource(self: *LoadedPacks) backend.dev.SpliceSource {
        return .{ .context = @ptrCast(self), .find = findArtifact };
    }

    /// The objects of every object pack that served a specialization, which
    /// the program links beside its own object.
    /// Every optimized object pack of the modules in view, which a program
    /// whose linked executable is built ahead of its code (`roc run`'s host)
    /// links whole. Loads them when no lookup has.
    pub fn loadedObjectPacks(self: *LoadedPacks) []const PackFile.ObjectPack {
        self.ensureLinkedLoaded();
        return self.object_packs.items;
    }

    pub fn linkedObjects(self: *const LoadedPacks, allocator: Allocator) Allocator.Error![]const []const u8 {
        var objects = std.ArrayList([]const u8).empty;
        errdefer objects.deinit(allocator);
        for (self.object_packs.items, self.object_packs_used.items) |pack, used| {
            if (used) try objects.append(allocator, pack.object);
        }
        return try objects.toOwnedSlice(allocator);
    }

    fn findSpec(context: *anyopaque, key: [32]u8, accept_linked: bool) ?Common.SpecCacheHit {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        self.ensureLoaded();
        if (self.state != .ready) return null;
        // An optimized object serves a consumer whose code is linked ahead
        // of the dev artifacts of the same specialization.
        if (accept_linked) {
            self.ensureLinkedLoaded();
            if (self.linked_specs.get(key)) |offer| {
                self.object_packs_used.items[offer.pack] = true;
                self.hits += 1;
                return offer.hit;
            }
        }
        const hit = self.specs.get(key) orelse return null;
        self.hits += 1;
        return hit;
    }

    fn findArtifact(context: *anyopaque, key: [32]u8) ?backend.dev.LocatedArtifact {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        self.ensureLoaded();
        if (self.state != .ready) return null;
        const located = self.artifacts.get(key) orelse return null;
        self.artifacts_served += 1;
        return located;
    }
};

fn nameLessThan(_: void, lhs: []u8, rhs: []u8) bool {
    return std.mem.order(u8, lhs, rhs) == .lt;
}
