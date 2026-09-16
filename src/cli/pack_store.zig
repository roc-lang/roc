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
/// under the cache root: `objects/<target>-<opt>/<local|pkg>/<module
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

    pub const InitError = Allocator.Error || error{NoHomeDirectory};

    pub fn init(allocator: Allocator, cache_config: compile.CacheConfig, target: RocTarget, opt_name: []const u8) InitError!Store {
        const version_dir = try cache_config.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);
        const mode = try std.fmt.allocPrint(allocator, "{s}-{s}", .{ @tagName(target), opt_name });
        defer allocator.free(mode);
        const root = try std.fs.path.join(allocator, &.{ version_dir, "objects", mode });
        return .{ .allocator = allocator, .roc_ctx = cache_config.roc_ctx, .root = root };
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
        self.roc_ctx.makePath(dir) catch return error.PackWriteFailed;
        const path = try self.packPath(origin, identity, key);
        defer self.allocator.free(path);
        if (self.roc_ctx.fileExists(path)) return;
        const temp_path = try std.fmt.allocPrint(self.allocator, "{s}.tmp", .{path});
        defer self.allocator.free(temp_path);
        self.roc_ctx.writeFile(temp_path, bytes) catch return error.PackWriteFailed;
        self.roc_ctx.rename(temp_path, path) catch return error.PackWriteFailed;
    }

    /// Every pack filed under a module identity, in name order.
    pub fn loadIdentity(self: *const Store, io: std.Io, packs: *LoadedPacks, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) Allocator.Error!void {
        const dir = try self.identityDir(origin, identity);
        defer self.allocator.free(dir);
        packs.loadDirInto(io, dir) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.PackDirectoryUnreadable, error.MalformedPack, error.UnsupportedPackVersion => {},
        };
    }
};

/// What a lazily loaded store needs once checking has produced the module
/// set: the store and the modules in view.
pub const Pending = struct {
    store: *const Store,
    io: std.Io,
    build_env: *compile.BuildEnv,
};

/// Every pack in a directory, indexed by specialization key and by procedure
/// identity. Files load in name order so the indexes are deterministic when
/// two packs carry the same entry.
pub const LoadedPacks = struct {
    allocator: Allocator,
    packs: std.ArrayList(PackFile.Pack),
    specs: std.AutoHashMap([32]u8, Common.SpecCacheHit),
    artifacts: std.AutoHashMap(lir.ProcIdentity, backend.dev.LocatedArtifact),
    /// Specializations served so far.
    hits: u64 = 0,
    /// Set until `indexPacks` runs; lookups before that load the pending
    /// store's packs for the modules in view first.
    pending: ?Pending = null,
    indexed: bool = false,

    pub const LoadError = Allocator.Error || PackFile.ReadError || error{PackDirectoryUnreadable};

    pub fn init(allocator: Allocator) LoadedPacks {
        return .{
            .allocator = allocator,
            .packs = .empty,
            .specs = std.AutoHashMap([32]u8, Common.SpecCacheHit).init(allocator),
            .artifacts = std.AutoHashMap(lir.ProcIdentity, backend.dev.LocatedArtifact).init(allocator),
        };
    }

    /// Load every `.rpk` file in `dir_path` and index the result.
    pub fn loadDir(allocator: Allocator, io: std.Io, dir_path: []const u8) LoadError!LoadedPacks {
        var self = LoadedPacks.init(allocator);
        errdefer self.deinit();
        try self.loadDirInto(io, dir_path);
        try self.indexPacks();
        return self;
    }

    /// Read every `.rpk` file in `dir_path`, in name order, without indexing.
    pub fn loadDirInto(self: *LoadedPacks, io: std.Io, dir_path: []const u8) LoadError!void {
        const allocator = self.allocator;
        var names = std.ArrayList([]u8).empty;
        defer {
            for (names.items) |name| allocator.free(name);
            names.deinit(allocator);
        }
        {
            var dir = std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true }) catch return error.PackDirectoryUnreadable;
            defer dir.close(io);
            var entries = dir.iterate();
            while (true) {
                const entry = (entries.next(io) catch return error.PackDirectoryUnreadable) orelse break;
                if (entry.kind != .file) continue;
                if (!std.mem.endsWith(u8, entry.name, ".rpk")) continue;
                try names.append(allocator, try allocator.dupe(u8, entry.name));
            }
        }
        std.mem.sort([]u8, names.items, {}, nameLessThan);

        for (names.items) |name| {
            const path = try std.fs.path.join(allocator, &.{ dir_path, name });
            defer allocator.free(path);
            const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .limited(1024 * 1024 * 1024)) catch return error.PackDirectoryUnreadable;
            defer allocator.free(bytes);
            const pack = try PackFile.read(allocator, bytes);
            try self.packs.append(allocator, pack);
        }
    }

    /// Index every loaded pack. The artifact index points into the packs
    /// list, which must not grow afterwards.
    pub fn indexPacks(self: *LoadedPacks) LoadError!void {
        self.indexed = true;
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
                const gop = try self.specs.getOrPut(spec.key);
                if (!gop.found_existing) gop.value_ptr.* = .{
                    .identity = identity.bytes,
                    .rc_borrowed_params = spec.rc_borrowed_params,
                    .rc_ret_borrowed = spec.rc_ret_borrowed,
                    .rc_ret_lenders = spec.rc_ret_lenders,
                };
            }
        }
    }

    /// Load the pending store's packs for the root module and every module
    /// it lowers against, then index. Runs once, at the first lookup after
    /// checking has produced the module set; before that a lookup finds
    /// nothing.
    fn ensureLoaded(self: *LoadedPacks) void {
        if (self.indexed) return;
        const pending = self.pending orelse return;
        // The first lookups come from the compile-time evaluator's program,
        // which lowers inside checking, after the root module's artifact
        // exists and before the build has declared its artifacts final.
        const root_semantic = pending.build_env.getExecutableRootSemanticData() orelse return;
        const root_artifact = root_semantic.checked_artifact orelse return;
        self.loadPending(pending, root_artifact) catch |err| {
            std.log.warn("object cache unavailable for this build: {}", .{err});
        };
        self.indexPacks() catch |err| {
            std.log.warn("object cache unavailable for this build: {}", .{err});
        };
    }

    fn loadPending(self: *LoadedPacks, pending: Pending, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) LoadError!void {
        if (pending.build_env.packPlacementForArtifactKey(root_artifact.key)) |placement| {
            try pending.store.loadIdentity(pending.io, self, placement.origin, placement.identity);
        }
        const artifacts = try pending.build_env.collectVisibleArtifacts(self.allocator, root_artifact);
        defer self.allocator.free(artifacts);
        for (artifacts) |artifact| {
            const placement = pending.build_env.packPlacementForArtifactKey(artifact.key) orelse continue;
            try pending.store.loadIdentity(pending.io, self, placement.origin, placement.identity);
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

    fn findSpec(context: *anyopaque, key: [32]u8) ?Common.SpecCacheHit {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        self.ensureLoaded();
        const hit = self.specs.get(key) orelse return null;
        self.hits += 1;
        return hit;
    }

    fn findArtifact(context: *anyopaque, identity: lir.ProcIdentity) ?backend.dev.LocatedArtifact {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        return self.artifacts.get(identity);
    }
};

fn nameLessThan(_: void, lhs: []u8, rhs: []u8) bool {
    return std.mem.order(u8, lhs, rhs) == .lt;
}
