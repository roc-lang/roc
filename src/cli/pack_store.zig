//! Packs loaded for one build: the specialization keys they can serve at
//! Monotype reservation and the artifacts the object writer splices in.

const std = @import("std");
const backend = @import("backend");
const base = @import("base");
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
/// under the cache root. Dev packs use the declared procedure domain; linked
/// packs and program outputs retain the exact target. Packs are filed by
/// module identity so an
/// edited module's previous packs stay beside its new one until the sweep
/// removes them, which is what lets unchanged specializations hit across an
/// edit. Every file is a pure function of the module and its transitive
/// imports, so two writers of one path write identical bytes and rename
/// into place without coordination.
pub const Store = struct {
    allocator: Allocator,
    roc_ctx: CoreCtx,
    /// Exact final-link target and optimization level.
    root: []u8,
    /// Procedure packs and discovery summaries, independently of final output.
    procedure_root: []u8,
    verbose: bool,

    pub const InitError = Allocator.Error || error{NoHomeDirectory};

    pub fn init(allocator: Allocator, cache_config: compile.CacheConfig, target: RocTarget, opt_name: []const u8) InitError!Store {
        return initForStrategy(allocator, cache_config, target, opt_name, .lss);
    }

    pub fn initForStrategy(allocator: Allocator, cache_config: compile.CacheConfig, target: RocTarget, opt_name: []const u8, strategy: base.SpecializationStrategy) InitError!Store {
        const version_dir = try cache_config.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);
        const mode = try std.fmt.allocPrint(allocator, "{s}-{s}", .{ @tagName(target), opt_name });
        defer allocator.free(mode);
        const namespace = compile.config.objectPackNamespace(PackFile.format_version);
        const root = try std.fs.path.join(allocator, &.{ version_dir, namespace, mode });
        errdefer allocator.free(root);
        const procedure_root = if (std.mem.eql(u8, opt_name, "dev")) blk: {
            const domain = backend.dev.NativeProcDomain.init(target, strategy);
            const procedure_mode = try std.fmt.allocPrint(allocator, "proc-{s}-dev", .{std.fmt.bytesToHex(domain.key(), .lower)});
            defer allocator.free(procedure_mode);
            break :blk try std.fs.path.join(allocator, &.{ version_dir, namespace, procedure_mode });
        } else try allocator.dupe(u8, root);
        return .{ .allocator = allocator, .roc_ctx = cache_config.roc_ctx, .root = root, .procedure_root = procedure_root, .verbose = cache_config.verbose };
    }

    pub fn deinit(self: *Store) void {
        self.allocator.free(self.root);
        self.allocator.free(self.procedure_root);
    }

    /// The directory of every pack filed under a module identity.
    pub fn identityDir(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) Allocator.Error![]u8 {
        return std.fs.path.join(self.allocator, &.{ self.procedure_root, @tagName(origin), &std.fmt.bytesToHex(identity, .lower) });
    }

    fn packPath(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8) Allocator.Error![]u8 {
        return self.filePath(origin, identity, key, ".rpk");
    }

    fn filePath(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8, comptime extension: []const u8) Allocator.Error![]u8 {
        const name = std.fmt.bytesToHex(key, .lower) ++ extension[0..extension.len].*;
        return std.fs.path.join(self.allocator, &.{ self.procedure_root, @tagName(origin), &std.fmt.bytesToHex(identity, .lower), &name });
    }

    /// File a program's discovery summaries (design.md "Discovery
    /// Summaries") under a module identity, named by their content, beside
    /// that module's packs. Summaries already filed are left alone.
    pub fn writeDiscovery(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        var name: [32]u8 = undefined;
        std.crypto.hash.sha2.Sha256.hash(bytes, &name, .{});
        const path = try self.filePath(origin, identity, name, discovery_extension);
        defer self.allocator.free(path);
        try self.writePath(path, bytes);
    }

    fn programPath(self: *const Store, key: [32]u8) Allocator.Error![]u8 {
        const name = std.fmt.bytesToHex(key, .lower) ++ ".rpo".*;
        return std.fs.path.join(self.allocator, &.{ self.root, "programs", &name });
    }

    /// The runtime output recorded for the program `key` names, or null.
    /// An unreadable record is a miss.
    pub fn readProgram(self: *const Store, key: [32]u8, allocator: Allocator) Allocator.Error!?[]u8 {
        const path = try self.programPath(key);
        defer self.allocator.free(path);
        return self.roc_ctx.readFile(path, allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => null,
        };
    }

    /// Record the runtime output of the program `key` names. A record
    /// already present holds the same output, and is left alone.
    pub fn writeProgram(self: *const Store, key: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        const path = try self.programPath(key);
        defer self.allocator.free(path);
        try self.writePath(path, bytes);
    }

    /// The object pack an optimized build of the program whose root module
    /// has placement identity `identity` filed, which each optimized build of
    /// that program replaces.
    pub fn optimizedPackPath(self: *const Store, identity: [32]u8) Allocator.Error![]u8 {
        const name = std.fmt.bytesToHex(identity, .lower) ++ ".rpk".*;
        return std.fs.path.join(self.allocator, &.{ self.root, "optimized", &name });
    }

    /// The object pack a URL-package module's artifact `key` names under
    /// placement identity `identity`.
    pub fn packagePackPath(self: *const Store, identity: [32]u8, key: [32]u8) Allocator.Error![]u8 {
        return self.packPath(.pkg, identity, key);
    }

    /// The input key of the optimized pack filed for root `identity`
    /// (`PackFile.ObjectBinding.input_key`), or null when there is none.
    pub fn optimizedInputKey(self: *const Store, identity: [32]u8) Allocator.Error!?[32]u8 {
        const path = try self.optimizedPackPath(identity);
        defer self.allocator.free(path);
        const bytes = self.roc_ctx.readFile(path, self.allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return null,
        };
        defer self.allocator.free(bytes);
        return PackFile.objectInputKey(bytes);
    }

    /// File the optimized pack of root `identity`, replacing the one an
    /// earlier optimized build filed.
    pub fn replaceOptimized(self: *const Store, identity: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        const path = try self.optimizedPackPath(identity);
        defer self.allocator.free(path);
        const dir = std.fs.path.dirname(path).?;
        self.roc_ctx.makePath(dir) catch |err| {
            self.reportWriteFailure("create directory", dir, bytes.len, err);
            return error.PackWriteFailed;
        };
        const temp_path = self.roc_ctx.writeTempFile(self.allocator, path, bytes) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => |write_err| {
                self.reportWriteFailure("write staging file", path, bytes.len, write_err);
                return error.PackWriteFailed;
            },
        };
        defer self.allocator.free(temp_path);
        errdefer self.roc_ctx.deleteFile(temp_path) catch {};
        self.roc_ctx.rename(temp_path, path) catch |err| {
            self.reportWriteFailure("publish staging file", temp_path, bytes.len, err);
            return error.PackWriteFailed;
        };
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
        const path = try self.packPath(origin, identity, key);
        defer self.allocator.free(path);
        try self.writePath(path, bytes);
    }

    /// Stage `bytes` in a file only this writer owns, beside `path`, and
    /// rename it into place, so a reader or another writer never sees a
    /// partly written file.
    fn writePath(self: *const Store, path: []const u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        const dir = std.fs.path.dirname(path).?;
        self.roc_ctx.makePath(dir) catch |err| {
            self.reportWriteFailure("create directory", dir, bytes.len, err);
            return error.PackWriteFailed;
        };
        if (self.roc_ctx.fileExists(path)) {
            if (std.debug.runtime_safety) try self.verifyCollision(path, bytes);
            return;
        }
        const temp_path = self.roc_ctx.writeTempFile(self.allocator, path, bytes) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => |write_err| {
                self.reportWriteFailure("write staging file", path, bytes.len, write_err);
                return error.PackWriteFailed;
            },
        };
        defer self.allocator.free(temp_path);
        errdefer self.roc_ctx.deleteFile(temp_path) catch {};
        // A concurrent writer that published first wrote the same bytes, so
        // replacing its file is harmless.
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

    /// Read the discovery summary files filed under a module identity.
    pub fn loadIdentityDiscovery(self: *const Store, io: std.Io, packs: *LoadedPacks, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) LoadedPacks.LoadError!void {
        const dir = try self.identityDir(origin, identity);
        defer self.allocator.free(dir);
        try packs.loadDirFiles(io, dir, .cache_offers, .discovery);
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
            const hit = packs.specCacheLookup().lookup(expected_key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
            try std.testing.expectEqualSlices(u8, &expected_identity.bytes, &hit.identity);
            try std.testing.expectEqual(@as(u64, 1), hit.rc_borrowed_params);
            // A consumer that cannot splice is never served dev artifacts.
            try std.testing.expect(packs.specCacheLookup().lookup(expected_key, .linked_objects, &.{}) == null);
            const located = packs.spliceSource().find(packs.spliceSource().context, expected_key) orelse return error.TestUnexpectedResult;
            try std.testing.expectEqual(expected_identity, located.set.artifacts[located.index].kind.proc);
        }

        fn rejected(packs: *LoadedPacks, key_to_find: [32]u8, err: LoadedPacks.LoadError) (LoadedPacks.LoadError || error{ TestExpectedEqual, TestUnexpectedResult, TestExpectedError, TestUnexpectedError })!void {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expectEqual(err, packs.failure.?);
            try std.testing.expect(packs.state == .unavailable);
            try std.testing.expectError(err, packs.indexPacks());
            try std.testing.expect(packs.specCacheLookup().lookup(key_to_find, .dev_code, &.{}) == null);
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
        // In-place growth under the testing allocator depends on the heap's
        // layout, so the failure check runs over an allocator that refuses it.
        var deterministic = @import("base").DeterministicAllocator.init(allocator);
        try std.testing.checkAllAllocationFailures(deterministic.allocator(), Attempt.run, .{ dir_path, key, identity });
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
    try std.testing.expect(malformed.specCacheLookup().lookup(key, .dev_code, &.{}) == null);
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
        const hit = packs.specCacheLookup().lookup(key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
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
            for (0..2) |_| {
                try std.testing.expectError(error.PackWriteFailed, store.write(.local, @as([32]u8, @splat(1)), @as([32]u8, @splat(2)), "pack"));
            }
            try std.testing.expectEqual(@as(usize, if (stage == .rename) 2 else 0), capture.deleted);
            if (verbose) {
                const cause = switch (stage) {
                    .directory => "error.ReadOnlyFileSystem",
                    .write => "error.NoSpaceLeft",
                    .rename => "error.DiskQuota",
                };
                try std.testing.expectEqual(@as(usize, 2), std.mem.count(u8, capture.stderr.items, cause));
                try std.testing.expectEqual(@as(usize, 2), std.mem.count(u8, capture.stderr.items, "(4 bytes)"));
                try std.testing.expect(std.mem.containsAtLeast(u8, capture.stderr.items, 1, store.procedure_root));
            } else {
                try std.testing.expectEqualStrings("", capture.stderr.items);
            }
        }
    }
}

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
    const base_ctx = CoreCtx.default(allocator, allocator, io);
    var store = Store{ .allocator = allocator, .roc_ctx = base_ctx, .root = try allocator.dupe(u8, root), .procedure_root = try allocator.dupe(u8, root), .verbose = false };
    defer store.deinit();
    var hook = Hook{ .store = &store, .base = base_ctx };
    store.roc_ctx.ctx = &hook;
    store.roc_ctx.vtable.writeTempFile = &Hook.writeTempFile;
    const identity = @as([32]u8, @splat(1));
    try store.write(.local, identity, @as([32]u8, @splat(2)), "whole completed pack");
    try std.testing.expect(hook.distinct);
    try std.testing.expect(hook.observed_complete);
    const dir_path = try store.identityDir(.local, identity);
    defer allocator.free(dir_path);
    var dir = try std.Io.Dir.cwd().openDir(io, dir_path, .{ .iterate = true });
    defer dir.close(io);
    var iterator = dir.iterate();
    var count: usize = 0;
    while (try iterator.next(io)) |entry| {
        try std.testing.expect(!std.mem.endsWith(u8, entry.name, ".tmp"));
        count += 1;
    }
    try std.testing.expectEqual(@as(usize, 1), count);
}

test "native procedure domain shares packs but isolates final outputs and linked objects" {
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(root);
    const config: compile.CacheConfig = .{ .cache_dir = root, .roc_ctx = CoreCtx.default(a, a, io) };
    var musl = try Store.init(a, config, .x64musl, "dev");
    defer musl.deinit();
    var glibc = try Store.init(a, config, .x64glibc, "dev");
    defer glibc.deinit();
    try std.testing.expectEqualStrings(musl.procedure_root, glibc.procedure_root);
    try std.testing.expect(!std.mem.eql(u8, musl.root, glibc.root));
    const identity: [32]u8 = @splat(1);
    const key: [32]u8 = @splat(2);
    try musl.write(.local, identity, key, "procedure pack");
    try std.testing.expect(try glibc.has(.local, identity, key));
    try musl.writeProgram(key, "musl program output");
    try std.testing.expectEqual(null, try glibc.readProgram(key, a));
    try glibc.writeProgram(key, "glibc program output");
    const bytes = (try musl.readProgram(key, a)).?;
    defer a.free(bytes);
    try std.testing.expectEqualStrings("musl program output", bytes);
    for ([_]RocTarget{ .x64v1glibc, .arm64musl, .x64win }) |target| {
        var incompatible = try Store.init(a, config, target, "dev");
        defer incompatible.deinit();
        try std.testing.expect(!try incompatible.has(.local, identity, key));
    }
    var boxy = try Store.initForStrategy(a, config, .x64glibc, "dev", .boxy);
    defer boxy.deinit();
    try std.testing.expect(!try boxy.has(.local, identity, key));
    for ([_][]const u8{ "speed", "size" }) |level| {
        var left = try Store.init(a, config, .x64musl, level);
        defer left.deinit();
        var right = try Store.init(a, config, .x64glibc, level);
        defer right.deinit();
        try left.write(.pkg, identity, key, "linked-only package");
        try std.testing.expect(!try right.has(.pkg, identity, key));
        const left_path = try left.optimizedPackPath(identity);
        defer a.free(left_path);
        const right_path = try right.optimizedPackPath(identity);
        defer a.free(right_path);
        try std.testing.expect(!std.mem.eql(u8, left_path, right_path));
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

/// The stores of LLVM-compiled object packs a dev build for one target links
/// from, one per optimization level that files them, in the order their
/// offers rank.
pub const LinkedStores = struct {
    stores: [2]Store,

    /// `opt_names` names the levels in rank order.
    pub fn init(allocator: Allocator, cache_config: compile.CacheConfig, target: RocTarget, opt_names: [2][]const u8) Store.InitError!LinkedStores {
        var first = try Store.init(allocator, cache_config, target, opt_names[0]);
        errdefer first.deinit();
        return .{ .stores = .{ first, try Store.init(allocator, cache_config, target, opt_names[1]) } };
    }

    pub fn deinit(self: *LinkedStores) void {
        for (&self.stores) |*store| store.deinit();
    }

    /// A digest of every object pack a program whose root module is
    /// `root_artifact` would load: the build's optimization state. A package
    /// module's pack is named by its input key, and the optimized pack
    /// carries its own, so the digest reads no object.
    pub fn stateDigest(self: *const LinkedStores, build_env: *compile.BuildEnv, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) Allocator.Error![32]u8 {
        const allocator = self.stores[0].allocator;
        const modules = try packageModules(allocator, build_env, root_artifact);
        defer allocator.free(modules);
        var hasher = std.crypto.hash.Blake3.init(.{});
        hasher.update("roc.optimization-state.v1");
        for (&self.stores) |*store| {
            for (modules) |module| {
                const path = try store.packagePackPath(module.identity, module.key);
                defer allocator.free(path);
                if (!store.roc_ctx.fileExists(path)) continue;
                hasher.update(path);
                hasher.update(&.{0});
            }
            const placement = optimizedPackPlacement(build_env, root_artifact) orelse continue;
            const input_key = try store.optimizedInputKey(placement.identity) orelse continue;
            const path = try store.optimizedPackPath(placement.identity);
            defer allocator.free(path);
            hasher.update(path);
            hasher.update(&.{0});
            hasher.update(&input_key);
        }
        var digest: [32]u8 = undefined;
        hasher.final(&digest);
        return digest;
    }
};

/// The placement whose identity names the optimized pack of the program
/// whose root module is `root_artifact`: the app's when the program has one,
/// since every app of a platform shares the platform as its root, and
/// otherwise the root's.
pub fn optimizedPackPlacement(build_env: *compile.BuildEnv, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) ?compile.BuildEnv.PackPlacement {
    if (build_env.getAppSemanticData()) |app| {
        if (app.checked_artifact) |artifact| return build_env.packPlacementForArtifactKey(artifact.key);
    }
    return build_env.packPlacementForArtifactKey(root_artifact.key);
}

/// The key an object pack's app-bound entries are bound to
/// (`PackFile.ObjectBinding.app_key`): every checked module the program whose
/// root module is `root_artifact` lowers against. Code named by site has a
/// current body only its module's content pins, so an edit of any module of
/// the program stops those entries serving.
pub fn programBindingKey(allocator: Allocator, build_env: *compile.BuildEnv, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) Allocator.Error![32]u8 {
    const imports = try build_env.collectImportedArtifactViews(allocator, root_artifact);
    defer allocator.free(imports);
    const relations = try build_env.collectRelationArtifactViews(allocator, root_artifact);
    defer allocator.free(relations);
    var hasher = std.crypto.hash.Blake3.init(.{});
    hasher.update("roc.program-binding.v1");
    hasher.update(&root_artifact.key.bytes);
    hasher.update(&root_artifact.codeGenerationKey().bytes);
    inline for (.{ imports, relations }) |views| {
        var count: [8]u8 = undefined;
        std.mem.writeInt(u64, &count, views.len, .little);
        hasher.update(&count);
        for (views) |view| {
            hasher.update(&view.key.bytes);
            hasher.update(&(view.code_generation_key orelse view.key).bytes);
        }
    }
    var key: [32]u8 = undefined;
    hasher.final(&key);
    return key;
}

/// A dev pack as a program asks for its entries: the pack's artifact set and
/// a digest of the program's modules (`Common.ProgramModules`).
const ProgramPack = struct {
    set: *const backend.dev.ProcArtifact.Set,
    modules: [32]u8,

    fn of(set: *const backend.dev.ProcArtifact.Set, modules: Common.ProgramModules) ProgramPack {
        var hasher = std.crypto.hash.Blake3.init(.{});
        for (modules) |identity| hasher.update(&identity);
        var digest: [32]u8 = undefined;
        hasher.final(&digest);
        return .{ .set = set, .modules = digest };
    }
};

/// A URL-package module in view of a program: its placement identity and the
/// artifact key its object pack is filed under.
const PackageModule = struct {
    identity: [32]u8,
    key: [32]u8,

    fn lessThan(_: void, lhs: PackageModule, rhs: PackageModule) bool {
        return switch (std.mem.order(u8, &lhs.identity, &rhs.identity)) {
            .lt => true,
            .gt => false,
            .eq => std.mem.order(u8, &lhs.key, &rhs.key) == .lt,
        };
    }
};

/// The URL-package modules in view of the program whose root module is
/// `root_artifact`, in identity order, so the packs they offer load in an
/// order no directory listing decides.
fn packageModules(allocator: Allocator, build_env: *compile.BuildEnv, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) Allocator.Error![]PackageModule {
    const artifacts = try build_env.collectVisibleArtifacts(allocator, root_artifact);
    defer allocator.free(artifacts);
    var modules = std.ArrayList(PackageModule).empty;
    errdefer modules.deinit(allocator);
    for (artifacts) |artifact| {
        if (std.meta.eql(artifact.key, root_artifact.key)) continue;
        const placement = build_env.packPlacementForArtifactKey(artifact.key) orelse continue;
        if (placement.origin != .pkg) continue;
        try modules.append(allocator, .{ .identity = placement.identity, .key = artifact.codeGenerationKey().bytes });
    }
    std.mem.sort(PackageModule, modules.items, {}, PackageModule.lessThan);
    return try modules.toOwnedSlice(allocator);
}

/// What a lazily loaded store needs once checking has produced the module
/// set: the store and the modules in view.
pub const Pending = struct {
    /// Dev packs, whose artifacts are spliced; null for a consumer whose
    /// emitter cannot splice them, which only object packs serve.
    store: ?*const Store,
    /// LLVM-compiled object packs (`PackFile.ObjectPack`) that optimized
    /// builds filed, which only a consumer whose code is linked takes.
    linked_stores: ?*const LinkedStores = null,
    io: std.Io,
    build_env: *compile.BuildEnv,
};

/// The command whose compile-time evaluation filed a compile-time pack, and
/// the optimization level its program is specialized for. Each evaluates its
/// own roots of the program under its own Solved policy, so each files its
/// own pack.
pub const CompileTimePublisher = struct {
    command: enum { check, build, test_run },
    opt: []const u8,
};

/// The file name a program's compile-time pack is filed under, beside the
/// root module's runtime packs. It is distinct from every runtime pack key, so
/// the two never replace each other; their code is a dev build's alike.
pub fn compileTimePackKey(code_generation_key: [32]u8, publisher: CompileTimePublisher) [32]u8 {
    var hasher = @import("base").Sha256.init(.{});
    hasher.update("roc.compile-time-pack.v2");
    hasher.update(&code_generation_key);
    hasher.update(@tagName(publisher.command));
    hasher.update(&.{0});
    hasher.update(publisher.opt);
    return hasher.finalResult();
}

/// The file extension of discovery summaries.
const discovery_extension = ".rds";
const discovery_magic = "RDSK";
/// Format version of discovery summary files; a file of another version is
/// no offer.
pub const discovery_format_version: u32 = 1;

/// Encode discovery summaries: the magic, the version, the count, and for
/// each summary its key, its module count, and its module keys, in the
/// order given (`DiscoverySummary.collect` sorts them).
pub fn encodeDiscovery(allocator: Allocator, summaries: []const lir.CheckedPipeline.DiscoverySummary.Summary) Allocator.Error![]u8 {
    var bytes = std.ArrayList(u8).empty;
    errdefer bytes.deinit(allocator);
    try bytes.appendSlice(allocator, discovery_magic);
    var word: [4]u8 = undefined;
    std.mem.writeInt(u32, &word, discovery_format_version, .little);
    try bytes.appendSlice(allocator, &word);
    std.mem.writeInt(u32, &word, @intCast(summaries.len), .little);
    try bytes.appendSlice(allocator, &word);
    for (summaries) |summary| {
        try bytes.appendSlice(allocator, &summary.key);
        std.mem.writeInt(u32, &word, @intCast(summary.modules.len), .little);
        try bytes.appendSlice(allocator, &word);
        for (summary.modules) |module| try bytes.appendSlice(allocator, &module);
    }
    return try bytes.toOwnedSlice(allocator);
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
    /// The object pack that defines each procedure some object pack offers,
    /// by identity: the procedures whose code a consumer whose code is
    /// linked takes from an object rather than from a dev pack.
    linked_identities: std.AutoHashMapUnmanaged(lir.ProcIdentity, u32) = .empty,
    /// Per dev pack, by its artifact set, whether a splice of each artifact
    /// would place code for a procedure an object pack offers
    /// (`linked_identities`).
    dev_reaches_linked: std.AutoHashMapUnmanaged(*const backend.dev.ProcArtifact.Set, []bool) = .empty,
    /// Per dev pack and asking program, whether a splice of each artifact
    /// would place a line or a location in a module version the program does
    /// not lower against (`linesOutsideProgram`).
    lines_outside: std.AutoHashMapUnmanaged(ProgramPack, []bool) = .empty,
    /// Per object pack, whether the program takes one of its entries, so it
    /// links the pack's object; settled from the lowered program
    /// (`settleLinkedObjects`).
    object_packs_used: std.ArrayList(bool) = .empty,
    /// Object packs load at the first lookup that takes a linked entry,
    /// which comes after checking, so a build that writes a module's object
    /// pack once the module is checked serves its own program from it.
    linked_state: enum { pending, ready, unavailable } = .pending,
    /// Specializations served so far.
    hits: u64 = 0,
    /// Artifacts handed to a splice so far.
    artifacts_served: u64 = 0,
    /// Bytes read from procedure, discovery and linked-object files.
    bytes_loaded: u64 = 0,
    /// Set until `indexPacks` runs; lookups before that load the pending
    /// store's packs for the modules in view first.
    pending: ?Pending = null,
    /// The discovery settlements of the loaded summary files, by
    /// specialization key; their module lists live in `discovery_arena`.
    discovery: std.AutoHashMapUnmanaged([32]u8, std.ArrayList(Common.DiscoverySettlement)) = .empty,
    discovery_arena: std.heap.ArenaAllocator,
    /// Discovery summaries load at the first discovery lookup, apart from
    /// the code packs, so a program whose every discovery request is settled
    /// reads no code.
    discovery_state: enum { pending, ready, unavailable } = .pending,
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
            .discovery_arena = std.heap.ArenaAllocator.init(allocator),
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

    /// Which files of a pack directory a load reads.
    const Files = enum {
        /// Code packs.
        code,
        /// Discovery summaries.
        discovery,

        fn extension(self: Files) []const u8 {
            return switch (self) {
                .code => ".rpk",
                .discovery => discovery_extension,
            };
        }
    };

    fn loadDirIntoUnrecorded(self: *LoadedPacks, io: std.Io, dir_path: []const u8, input: Input) LoadError!void {
        try self.loadDirFiles(io, dir_path, input, .code);
    }

    fn loadDirFiles(self: *LoadedPacks, io: std.Io, dir_path: []const u8, input: Input, files: Files) LoadError!void {
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
                if (!std.mem.endsWith(u8, entry.name, files.extension())) continue;
                try names.ensureUnusedCapacity(allocator, 1);
                names.appendAssumeCapacity(try allocator.dupe(u8, entry.name));
            }
        }
        std.mem.sort([]u8, names.items, {}, nameLessThan);

        for (names.items) |name| {
            const path = try std.fs.path.join(allocator, &.{ dir_path, name });
            defer allocator.free(path);
            // The decoded pack aliases the file bytes, which its arena owns.
            var arena = base.SingleThreadArena.init(allocator);
            const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, arena.allocator(), .limited(1024 * 1024 * 1024)) catch |err| {
                arena.deinit();
                return switch (err) {
                    error.OutOfMemory => error.OutOfMemory,
                    else => error.PackDirectoryUnreadable,
                };
            };
            self.bytes_loaded +|= bytes.len;
            switch (files) {
                .discovery => {
                    // The index copies what it keeps out of the file's bytes.
                    defer arena.deinit();
                    try self.appendDiscovery(bytes);
                    continue;
                },
                .code => {},
            }
            self.appendPack(arena, bytes) catch |err| {
                switch (err) {
                    error.UnsupportedPackVersion => if (input == .cache_offers) continue,
                    error.OutOfMemory, error.MalformedPack, error.PackDirectoryUnreadable => {},
                }
                return @as(LoadError!void, err);
            };
        }
    }

    /// Read the object pack at `path` without indexing. A missing file, a
    /// pack of another format, or a pack that offers nothing is no offer.
    fn loadObjectFile(self: *LoadedPacks, io: std.Io, path: []const u8) LoadError!void {
        const allocator = self.allocator;
        // The decoded pack aliases the file bytes, which its arena owns.
        var arena = base.SingleThreadArena.init(allocator);
        const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, arena.allocator(), .limited(1024 * 1024 * 1024)) catch |err| {
            arena.deinit();
            return switch (err) {
                error.OutOfMemory => error.OutOfMemory,
                error.FileNotFound => {},
                else => error.PackDirectoryUnreadable,
            };
        };
        self.bytes_loaded +|= bytes.len;
        var pack = PackFile.readObjectOwned(arena, bytes) catch |err| switch (err) {
            error.UnsupportedPackVersion => return,
            else => |other| return other,
        };
        // A pack that offers nothing records only that its program's code
        // could not be a linked object; it holds no object to link.
        if (pack.specs.len == 0) {
            pack.deinit();
            return;
        }
        errdefer pack.deinit();
        try self.object_packs_used.append(allocator, false);
        errdefer _ = self.object_packs_used.pop();
        try self.object_packs.append(allocator, pack);
    }

    /// Index one discovery summary file. A file of another format version
    /// is no offer.
    fn appendDiscovery(self: *LoadedPacks, bytes: []const u8) LoadError!void {
        if (bytes.len < 12 or !std.mem.eql(u8, bytes[0..4], discovery_magic)) return error.MalformedPack;
        if (std.mem.readInt(u32, bytes[4..8], .little) != discovery_format_version) return;
        const count = std.mem.readInt(u32, bytes[8..12], .little);
        const arena = self.discovery_arena.allocator();
        var offset: usize = 12;
        for (0..count) |_| {
            if (bytes.len - offset < 36) return error.MalformedPack;
            const key = bytes[offset..][0..32].*;
            const module_count = std.mem.readInt(u32, bytes[offset + 32 ..][0..4], .little);
            offset += 36;
            if ((bytes.len - offset) / 32 < module_count) return error.MalformedPack;
            const modules = try arena.alloc([32]u8, module_count);
            for (modules) |*module| {
                module.* = bytes[offset..][0..32].*;
                offset += 32;
            }
            const entry = try self.discovery.getOrPut(self.allocator, key);
            if (!entry.found_existing) entry.value_ptr.* = .empty;
            try entry.value_ptr.append(self.allocator, .{ .modules = modules });
        }
        if (offset != bytes.len) return error.MalformedPack;
    }

    /// Decode `bytes`, which `arena` owns, taking `arena` on failure too.
    fn appendPack(self: *LoadedPacks, arena: base.SingleThreadArena, bytes: []const u8) LoadError!void {
        const pack = self.allocator.create(PackFile.Pack) catch |err| {
            var unowned = arena;
            unowned.deinit();
            return err;
        };
        errdefer self.allocator.destroy(pack);
        pack.* = try PackFile.readOwned(arena, bytes);
        errdefer pack.deinit();
        try self.packs.append(self.allocator, pack);
    }

    /// Offer a pack prepared after checking to the same command's runtime
    /// lowering. Decoded packs keep stable addresses, including artifacts
    /// already handed to the compile-time evaluator.
    pub fn offerPack(self: *LoadedPacks, bytes: []const u8) LoadError!void {
        self.ensureLoaded();
        if (self.state == .unavailable) return self.failure.?;
        var arena = base.SingleThreadArena.init(self.allocator);
        const owned = arena.allocator().dupe(u8, bytes) catch |err| {
            arena.deinit();
            self.unavailable(err);
            return err;
        };
        self.appendPack(arena, owned) catch |err| {
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

    /// Index every loaded object pack, in load order: the first pack to
    /// offer a key or a procedure serves it. An app-bound procedure built for
    /// another version of the program (`programBindingKey`) is not offered.
    fn indexObjectPacks(self: *LoadedPacks, program_key: [32]u8) LoadError!void {
        for (self.object_packs.items, 0..) |*pack, pack_index| {
            for (pack.specs) |spec| {
                if (!pack.servesProgram(spec.artifact, program_key)) continue;
                const linked = try self.linked_identities.getOrPut(self.allocator, pack.identities[spec.artifact]);
                if (!linked.found_existing) linked.value_ptr.* = @intCast(pack_index);
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

    /// Load and index the object packs of the program in view. Runs once,
    /// at the first lookup that takes a linked entry.
    fn ensureLinkedLoaded(self: *LoadedPacks) void {
        if (self.linked_state != .pending) return;
        self.linked_state = .unavailable;
        const pending = self.pending orelse return;
        const stores = pending.linked_stores orelse return;
        const root_semantic = pending.build_env.getExecutableRootSemanticData() orelse return;
        const root_artifact = root_semantic.checked_artifact orelse return;
        self.loadLinked(pending, stores, root_artifact) catch |err| {
            std.log.warn("optimized object packs unavailable for this build: {}", .{err});
            return;
        };
        self.linked_state = .ready;
    }

    /// Each store in rank order offers its packs of the URL-package modules
    /// in view, in identity order, then the optimized pack of the root. Each
    /// is read by its exact path, so the packs a program links and which of
    /// them serves a procedure two offer are functions of the optimization
    /// state alone.
    fn loadLinked(self: *LoadedPacks, pending: Pending, stores: *const LinkedStores, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) LoadError!void {
        const modules = try packageModules(self.allocator, pending.build_env, root_artifact);
        defer self.allocator.free(modules);
        const root_placement = optimizedPackPlacement(pending.build_env, root_artifact);
        for (&stores.stores) |*store| {
            for (modules) |module| {
                const path = try store.packagePackPath(module.identity, module.key);
                defer self.allocator.free(path);
                try self.loadObjectFile(pending.io, path);
            }
            if (root_placement) |placement| {
                const path = try store.optimizedPackPath(placement.identity);
                defer self.allocator.free(path);
                try self.loadObjectFile(pending.io, path);
            }
        }
        try self.indexObjectPacks(try programBindingKey(self.allocator, pending.build_env, root_artifact));
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

    /// Load the discovery summaries filed for the root module and every
    /// module it lowers against. Runs once, at the first discovery lookup
    /// after checking has produced the module set.
    fn ensureDiscoveryLoaded(self: *LoadedPacks) void {
        if (self.discovery_state != .pending) return;
        const pending = self.pending orelse return;
        const root_semantic = pending.build_env.getExecutableRootSemanticData() orelse return;
        const root_artifact = root_semantic.checked_artifact orelse return;
        self.loadPendingDiscovery(pending, root_artifact) catch |err| {
            self.discovery_state = .unavailable;
            std.log.warn("object cache discovery summaries unavailable for this build: {}", .{err});
            return;
        };
        self.discovery_state = .ready;
    }

    fn loadPendingDiscovery(self: *LoadedPacks, pending: Pending, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) LoadError!void {
        const store = pending.store orelse return;
        if (pending.build_env.packPlacementForArtifactKey(root_artifact.key)) |placement| {
            try store.loadIdentityDiscovery(pending.io, self, placement.origin, placement.identity);
        }
        const artifacts = try pending.build_env.collectVisibleArtifacts(self.allocator, root_artifact);
        defer self.allocator.free(artifacts);
        for (artifacts) |artifact| {
            const placement = pending.build_env.packPlacementForArtifactKey(artifact.key) orelse continue;
            try store.loadIdentityDiscovery(pending.io, self, placement.origin, placement.identity);
        }
    }

    fn unavailable(self: *LoadedPacks, err: LoadError) void {
        self.state = .unavailable;
        self.failure = err;
        std.log.warn("object cache unavailable for this build: {}", .{err});
    }

    fn loadPending(self: *LoadedPacks, pending: Pending, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) LoadError!void {
        const store = pending.store orelse return;
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

    pub fn deinit(self: *LoadedPacks) void {
        var settlements = self.discovery.valueIterator();
        while (settlements.next()) |list| list.deinit(self.allocator);
        self.discovery.deinit(self.allocator);
        self.discovery_arena.deinit();
        self.linked_specs.deinit();
        self.linked_identities.deinit(self.allocator);
        var reaching = self.dev_reaches_linked.valueIterator();
        while (reaching.next()) |marks| self.allocator.free(marks.*);
        self.dev_reaches_linked.deinit(self.allocator);
        var outside = self.lines_outside.valueIterator();
        while (outside.next()) |marks| self.allocator.free(marks.*);
        self.lines_outside.deinit(self.allocator);
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
        return .{ .context = @ptrCast(self), .find = findSpec, .find_discovery = findDiscovery };
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

    /// Every object pack that served a specialization, in load order, for a
    /// program that composes their objects into its own.
    pub fn linkedObjectPacks(self: *const LoadedPacks, allocator: Allocator) Allocator.Error![]const *const PackFile.ObjectPack {
        var packs = std.ArrayList(*const PackFile.ObjectPack).empty;
        errdefer packs.deinit(allocator);
        for (self.object_packs.items, self.object_packs_used.items) |*pack, used| {
            if (used) try packs.append(allocator, pack);
        }
        return try packs.toOwnedSlice(allocator);
    }

    fn findSpec(context: *anyopaque, key: [32]u8, takes: Common.SpecCacheTakes, modules: Common.ProgramModules) ?Common.SpecCacheHit {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        self.ensureLoaded();
        if (self.state != .ready) return null;
        // An optimized object serves a consumer whose code is linked ahead
        // of the dev artifacts of the same specialization.
        if (takes.takesLinkedObjects()) {
            self.ensureLinkedLoaded();
            if (self.linked_specs.get(key)) |offer| {
                self.hits += 1;
                return offer.hit;
            }
        }
        if (!takes.takesDevCode()) return null;
        const hit = self.specs.get(key) orelse return null;
        // An entry whose splice would place a line or a location in a module
        // version the asking program does not lower against would place lines
        // that differ from the procedure's own; the program compiles it.
        if (self.linesOutsideProgram(key, modules)) return null;
        // A consumer whose code is linked takes every procedure an object
        // offers from the object. A dev entry whose splice would place dev
        // code for such a procedure, as an entry compile-time evaluation
        // filed does, is not taken, so which procedures link an object's
        // entry depends on the object packs alone, never on which dev packs
        // happen to exist.
        if (takes.refusesDevCodeOfLinkedProcs()) {
            self.ensureLinkedLoaded();
            if (self.devEntryReachesLinked(key)) return null;
        }
        self.hits += 1;
        return hit;
    }

    /// Whether a splice of the dev entry under `key` would place a line or a
    /// location in a module version outside `modules`.
    fn linesOutsideProgram(self: *LoadedPacks, key: [32]u8, modules: Common.ProgramModules) bool {
        const located = self.artifacts.get(key) orelse return false;
        const set = located.set;
        var all_present = true;
        for (set.source_files) |identity| all_present = all_present and Common.programHasModule(modules, identity);
        if (all_present) return false;
        const program_pack = ProgramPack.of(set, modules);
        if (self.lines_outside.get(program_pack)) |marks| return marks[located.index];
        // Refusing the entry is always sound: the program compiles the
        // procedure itself.
        const present = self.allocator.alloc(bool, set.source_files.len) catch return true;
        defer self.allocator.free(present);
        for (set.source_files, present) |identity, *is_present| is_present.* = Common.programHasModule(modules, identity);
        const marks = backend.dev.ProcArtifact.reachingAbsentFiles(self.allocator, set, present) catch return true;
        self.lines_outside.put(self.allocator, program_pack, marks) catch {
            const outside = marks[located.index];
            self.allocator.free(marks);
            return outside;
        };
        return marks[located.index];
    }

    /// Whether the code a splice of the dev entry under `key` places
    /// includes a procedure some object pack offers: the entry's own
    /// procedure, or one its code reaches through the pack.
    fn devEntryReachesLinked(self: *LoadedPacks, key: [32]u8) bool {
        if (self.linked_identities.count() == 0) return false;
        const located = self.artifacts.get(key) orelse return false;
        const marks = self.dev_reaches_linked.get(located.set) orelse marks: {
            // Refusing the entry is always sound: the program compiles the
            // procedure itself.
            const computed = backend.dev.ProcArtifact.reachingIdentities(self.allocator, located.set, &self.linked_identities) catch return true;
            self.dev_reaches_linked.put(self.allocator, located.set, computed) catch {
                const reaches = computed[located.index];
                self.allocator.free(computed);
                return reaches;
            };
            break :marks computed;
        };
        return marks[located.index];
    }

    /// Mark the object packs that define the procedures `proc_specs` take
    /// from objects (`LIR.LirProcSpec.c_abi_entry`): exactly the objects the
    /// program links.
    pub fn settleLinkedObjects(self: *LoadedPacks, proc_specs: []const lir.LIR.LirProcSpec) void {
        @memset(self.object_packs_used.items, false);
        for (proc_specs) |proc| {
            if (!proc.c_abi_entry) continue;
            const pack = self.linked_identities.get(proc.identity) orelse
                base.invariant("a program took a linked entry no object pack offers", .{});
            self.object_packs_used.items[pack] = true;
        }
    }

    fn findDiscovery(context: *anyopaque, key: [32]u8) []const Common.DiscoverySettlement {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        self.ensureDiscoveryLoaded();
        if (self.discovery_state != .ready) return &.{};
        const list = self.discovery.get(key) orelse return &.{};
        return list.items;
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

test "discovery summaries round-trip and a file of another version is no offer" {
    const allocator = std.testing.allocator;
    const Summary = lir.CheckedPipeline.DiscoverySummary.Summary;
    const modules_a = [_][32]u8{ @splat(1), @splat(2) };
    const summaries = [_]Summary{
        .{ .key = @splat(7), .modules = &modules_a },
        .{ .key = @splat(7), .modules = &.{} },
        .{ .key = @splat(9), .modules = modules_a[1..] },
    };
    const bytes = try encodeDiscovery(allocator, &summaries);
    defer allocator.free(bytes);

    var packs = LoadedPacks.init(allocator);
    defer packs.deinit();
    try packs.appendDiscovery(bytes);
    packs.discovery_state = .ready;
    const seven = LoadedPacks.findDiscovery(@ptrCast(&packs), @splat(7));
    try std.testing.expectEqual(@as(usize, 2), seven.len);
    try std.testing.expectEqualSlices([32]u8, &modules_a, seven[0].modules);
    try std.testing.expectEqual(@as(usize, 0), seven[1].modules.len);
    try std.testing.expectEqualSlices([32]u8, modules_a[1..], LoadedPacks.findDiscovery(@ptrCast(&packs), @splat(9))[0].modules);
    try std.testing.expectEqual(@as(usize, 0), LoadedPacks.findDiscovery(@ptrCast(&packs), @splat(3)).len);

    const other_version = try allocator.dupe(u8, bytes);
    defer allocator.free(other_version);
    std.mem.writeInt(u32, other_version[4..8], discovery_format_version + 1, .little);
    var skipped = LoadedPacks.init(allocator);
    defer skipped.deinit();
    try skipped.appendDiscovery(other_version);
    try std.testing.expectEqual(@as(u32, 0), skipped.discovery.count());

    try std.testing.expectError(error.MalformedPack, skipped.appendDiscovery(bytes[0 .. bytes.len - 1]));
}

test "loaded packs offer no entry whose lines name a missing module version or that places code a linked object provides" {
    const allocator = std.testing.allocator;
    const base_mod = @import("base");
    const present_module: [32]u8 = @splat(0xa1);
    const edited_module: [32]u8 = @splat(0xb2);
    const caller = lir.ProcIdentity.forTest(61);
    const callee = lir.ProcIdentity.forTest(62);
    const stale = lir.ProcIdentity.forTest(63);
    const frame: backend.dev.ProcArtifact.Frame = .{ .prologue_size = 1, .stack_alloc = 0, .frame_size = 0, .callee_saved_mask = 0, .epilogue_offset = 1, .uses_frame_pointer = true };
    const set = backend.dev.ProcArtifact.Set{
        .arena = base_mod.SingleThreadArena.init(allocator),
        .artifacts = &.{
            .{ .kind = .{ .proc = caller }, .code = "\xe8\x00\x00\x00\x00\xc3", .entry = 0, .frame = frame, .refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }}, .relocations = &.{}, .data = &.{}, .loc = .{ .file = 0, .line = 1, .column = 1 } },
            .{ .kind = .{ .proc = callee }, .code = "\xc3", .entry = 0, .frame = frame, .refs = &.{}, .relocations = &.{}, .data = &.{}, .loc = .{ .file = 0, .line = 2, .column = 1 } },
            .{ .kind = .{ .proc = stale }, .code = "\xc3", .entry = 0, .frame = frame, .refs = &.{}, .relocations = &.{}, .data = &.{}, .lines = &.{.{ .offset = 0, .loc = .{ .file = 1, .line = 7, .column = 1 }, .region = base_mod.Region.zero() }} },
        },
        .source_files = &.{ present_module, edited_module },
    };
    const Spec = PackFile.SpecEntry;
    const spec = struct {
        fn of(key: u8, artifact: u32) Spec {
            return .{ .key = @splat(key), .artifact = artifact, .rc_borrowed_params = 0, .rc_ret_borrowed = false, .rc_ret_lenders = 0, .rc_read_only_params = 0, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{} };
        }
    }.of;
    const bytes = try PackFile.write(allocator, &set, &.{ spec(1, 0), spec(2, 1), spec(3, 2) });
    defer allocator.free(bytes);

    var packs = LoadedPacks.init(allocator);
    defer packs.deinit();
    var pack_arena = base.SingleThreadArena.init(allocator);
    const pack_bytes = pack_arena.allocator().dupe(u8, bytes) catch |err| {
        pack_arena.deinit();
        return err;
    };
    try packs.appendPack(pack_arena, pack_bytes);
    const modules: Common.ProgramModules = &.{present_module};
    try packs.indexPacks();
    const lookup = packs.specCacheLookup();
    try std.testing.expect(lookup.lookup(@splat(1), .dev_code, modules) != null);
    try std.testing.expect(lookup.lookup(@splat(2), .dev_code, modules) != null);
    // Its lines name a version of a module this program does not have.
    try std.testing.expect(lookup.lookup(@splat(3), .dev_code, modules) == null);

    // Once an object offers the callee, a consumer whose program links
    // objects takes neither the callee's dev code nor the caller's, whose
    // splice places it; a consumer that links nothing still takes both.
    packs.linked_state = .unavailable;
    try packs.linked_identities.put(allocator, callee, 0);
    try std.testing.expect(lookup.lookup(@splat(1), .dev_code, modules) != null);
    try std.testing.expect(lookup.lookup(@splat(1), .dev_code_beside_linked_objects, modules) == null);
    try std.testing.expect(lookup.lookup(@splat(2), .dev_code_and_linked_objects, modules) == null);
}

test "object packs serve in load order, and app-bound entries serve only the program they were built for" {
    const allocator = std.testing.allocator;
    const shared = lir.ProcIdentity.forTest(71);
    const app_code = lir.ProcIdentity.forTest(72);
    const program_key: [32]u8 = @splat(0x5a);
    const Spec = PackFile.SpecEntry;
    const spec = struct {
        fn of(key: u8, artifact: u32) Spec {
            return .{ .key = @splat(key), .artifact = artifact, .rc_borrowed_params = 0, .rc_ret_borrowed = false, .rc_ret_lenders = 0, .rc_read_only_params = 0, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{} };
        }
    }.of;
    // The first pack offers `shared`, and the program's own code bound to
    // `program_key`; the second offers `shared` under the same key.
    const first = try PackFile.writeObject(allocator, "first", &.{ shared, app_code }, &.{ spec(1, 0), spec(2, 1) }, .{ .app_key = program_key, .app_bound = &.{ false, true } });
    defer allocator.free(first);
    const second = try PackFile.writeObject(allocator, "second", &.{shared}, &.{spec(1, 0)}, .{ .app_bound = &.{false} });
    defer allocator.free(second);
    for ([_][32]u8{ program_key, @splat(0x6b) }) |consumer_program| {
        var packs = LoadedPacks.init(allocator);
        defer packs.deinit();
        for ([_][]const u8{ first, second }) |bytes| {
            var pack = try PackFile.readObject(allocator, bytes);
            errdefer pack.deinit();
            try packs.object_packs_used.append(allocator, false);
            try packs.object_packs.append(allocator, pack);
        }
        try packs.indexObjectPacks(consumer_program);
        try packs.indexPacks();
        // A shared producer and CTFE cannot execute a linked-only offer.
        const lookup = packs.specCacheLookup();
        try std.testing.expect(lookup.lookup(@splat(1), .dev_code, &.{}) == null);
        try std.testing.expect(lookup.lookup(@splat(1), .dev_code_beside_linked_objects, &.{}) == null);
        try std.testing.expect(lookup.lookup(@splat(1), .dev_code_and_linked_objects, &.{}) != null);
        try std.testing.expectEqual(@as(u32, 0), packs.linked_specs.get(@splat(1)).?.pack);
        try std.testing.expectEqual(@as(?u32, 0), packs.linked_identities.get(shared));
        const serves_app = std.mem.eql(u8, &consumer_program, &program_key);
        try std.testing.expectEqual(serves_app, packs.linked_specs.get(@splat(2)) != null);
        try std.testing.expectEqual(serves_app, packs.linked_identities.get(app_code) != null);
    }
}

test "an object pack that offers nothing is no offer and contributes no object to link" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const offered = lir.ProcIdentity.forTest(81);
    const spec: PackFile.SpecEntry = .{ .key = @splat(3), .artifact = 0, .rc_borrowed_params = 0, .rc_ret_borrowed = false, .rc_ret_lenders = 0, .rc_read_only_params = 0, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{} };
    const offering = try PackFile.writeObject(allocator, "object", &.{offered}, &.{spec}, .{ .app_bound = &.{false} });
    defer allocator.free(offering);
    // What an optimized build files when its program's code could not be a
    // linked object.
    const empty = try PackFile.writeObject(allocator, "", &.{}, &.{}, .{ .input_key = @splat(9), .app_bound = &.{} });
    defer allocator.free(empty);
    try tmp.dir.writeFile(io, .{ .sub_path = "offering.rpo", .data = offering });
    try tmp.dir.writeFile(io, .{ .sub_path = "empty.rpo", .data = empty });

    var packs = LoadedPacks.init(allocator);
    defer packs.deinit();
    for ([_][]const u8{ "empty.rpo", "offering.rpo" }) |name| {
        const path = try tmp.dir.realPathFileAlloc(io, name, allocator);
        defer allocator.free(path);
        try packs.loadObjectFile(io, path);
    }
    try std.testing.expectEqual(@as(usize, 1), packs.object_packs.items.len);
    try std.testing.expectEqualStrings("object", packs.object_packs.items[0].object);
}
