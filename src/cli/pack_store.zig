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
        const namespace = std.fmt.comptimePrint("objects-v{d}", .{PackFile.format_version});
        const root = try std.fs.path.join(allocator, &.{ version_dir, namespace, mode });
        return .{ .allocator = allocator, .roc_ctx = cache_config.roc_ctx, .root = root, .verbose = cache_config.verbose };
    }

    pub fn deinit(self: *Store) void {
        self.allocator.free(self.root);
    }

    fn identityDir(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) Allocator.Error![]u8 {
        return std.fs.path.join(self.allocator, &.{ self.root, @tagName(origin), &std.fmt.bytesToHex(identity, .lower) });
    }

    fn packPath(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8) Allocator.Error![]u8 {
        const dir = try self.identityDir(origin, identity);
        defer self.allocator.free(dir);
        const name = try std.fmt.allocPrint(self.allocator, "{s}.rpk", .{&std.fmt.bytesToHex(key, .lower)});
        defer self.allocator.free(name);
        return std.fs.path.join(self.allocator, &.{ dir, name });
    }

    /// Whether the pack for `key` is already in the store.
    pub fn has(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8) Allocator.Error!bool {
        const path = try self.packPath(origin, identity, key);
        defer self.allocator.free(path);
        return self.roc_ctx.fileExists(path);
    }

    /// Stage the pack beside its final path and rename it into place. A
    /// pack already present is left alone: it holds the same bytes.
    pub fn write(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        const dir = try self.identityDir(origin, identity);
        defer self.allocator.free(dir);
        self.roc_ctx.makePath(dir) catch |err| {
            self.reportWriteFailure("create directory", dir, bytes.len, err);
            return error.PackWriteFailed;
        };
        const path = try self.packPath(origin, identity, key);
        defer self.allocator.free(path);
        if (self.roc_ctx.fileExists(path)) return;
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
        .arena = std.heap.ArenaAllocator.init(allocator),
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
        fn run(allocator: Allocator, reverse: bool) (LoadedPacks.LoadError || error{ TestExpectedEqual, TestUnexpectedResult })!void {
            var packs = LoadedPacks.init(allocator);
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
                    .arena = std.heap.ArenaAllocator.init(allocator),
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
                const spec: PackFile.SpecEntry = .{
                    .key = if (relation == null) independent_key else key,
                    .artifact = 0,
                    .platform_requirement_relation = relation,
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
                const bytes = try PackFile.write(allocator, &set, &specs);
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
    for ([_]bool{ false, true }) |reverse| {
        var deterministic = @import("base").DeterministicAllocator.init(std.testing.allocator);
        try std.testing.checkAllAllocationFailures(deterministic.allocator(), Attempt.run, .{reverse});
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

/// What a lazily loaded store needs once checking has produced the module
/// set: the store and the modules in view.
pub const Pending = struct {
    store: *const Store,
    /// Packs compile-time evaluation published from its own programs
    /// (`compileTimePackKey`), offered after the store's packs. Only a
    /// provider no runtime consumer reads loads them: their code is lowered
    /// under the compile-time LIR policy, not the runtime one.
    compile_time_store: ?*const Store = null,
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
    const OfferKey = struct {
        key: [32]u8,
        platform_requirement_relation: ?[32]u8,
    };

    allocator: Allocator,
    packs: std.ArrayList(PackFile.Pack),
    specs: std.AutoHashMap(OfferKey, Common.SpecCacheHit),
    artifacts: std.AutoHashMap(lir.ProcIdentity, backend.dev.LocatedArtifact),
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
            .specs = std.AutoHashMap(OfferKey, Common.SpecCacheHit).init(allocator),
            .artifacts = std.AutoHashMap(lir.ProcIdentity, backend.dev.LocatedArtifact).init(allocator),
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

    fn appendPack(self: *LoadedPacks, bytes: []const u8) LoadError!void {
        var pack = try PackFile.read(self.allocator, bytes);
        errdefer pack.deinit();
        // After this succeeds, only LoadedPacks owns the decoded arena,
        // including errors in subsequent files or indexing.
        try self.packs.append(self.allocator, pack);
    }

    /// Index every loaded pack. The artifact index points into the packs
    /// list, which must not grow afterwards.
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
        for (self.packs.items) |*pack| {
            for (pack.set.artifacts, 0..) |artifact, index| {
                switch (artifact.kind) {
                    .proc => |identity| {
                        const gop = try self.artifacts.getOrPut(identity);
                        if (!gop.found_existing) gop.value_ptr.* = .{ .set = &pack.set, .index = @intCast(index) };
                    },
                    .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => {},
                }
            }
            for (pack.specs) |spec| {
                const root = pack.set.artifacts[spec.artifact];
                const identity = switch (root.kind) {
                    .proc => |identity| identity,
                    .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => return error.MalformedPack,
                };
                const gop = try self.specs.getOrPut(.{
                    .key = spec.key,
                    .platform_requirement_relation = spec.platform_requirement_relation,
                });
                if (!gop.found_existing) gop.value_ptr.* = .{
                    .identity = identity.bytes,
                    .platform_requirement_relation = spec.platform_requirement_relation,
                    .rc_borrowed_params = spec.rc_borrowed_params,
                    .rc_ret_borrowed = spec.rc_ret_borrowed,
                    .rc_ret_lenders = spec.rc_ret_lenders,
                    .rc_read_only_params = spec.rc_read_only_params,
                    .rc_ret_unique = spec.rc_ret_unique,
                    .rc_ret_unique_fields = spec.rc_ret_unique_fields,
                    .rc_ret_conditions = spec.rc_ret_conditions,
                };
            }
        }
        self.state = .ready;
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
        self.artifacts.deinit();
        self.specs.deinit();
        for (self.packs.items) |*pack| pack.deinit();
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

    fn findSpec(context: *anyopaque, key: [32]u8, current_relation: ?[32]u8) ?Common.SpecCacheHit {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        self.ensureLoaded();
        if (self.state != .ready) return null;
        // Retain each relation's offer separately: an incompatible earlier
        // pack must not hide a compatible later pack for the reservation.
        const hit = self.specs.get(.{ .key = key, .platform_requirement_relation = current_relation }) orelse
            self.specs.get(.{ .key = key, .platform_requirement_relation = null }) orelse return null;
        self.hits += 1;
        return hit;
    }

    fn findArtifact(context: *anyopaque, identity: lir.ProcIdentity) ?backend.dev.LocatedArtifact {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        self.ensureLoaded();
        if (self.state != .ready) return null;
        const located = self.artifacts.get(identity) orelse return null;
        self.artifacts_served += 1;
        return located;
    }
};

fn nameLessThan(_: void, lhs: []u8, rhs: []u8) bool {
    return std.mem.order(u8, lhs, rhs) == .lt;
}
