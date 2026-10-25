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
const Index = @import("pack_index.zig");
/// Producer-owned traversal shared by offer admission and index publication.
pub const SpliceClosure = Index.SpliceClosure;
const CoreCtx = ctx_mod.CoreCtx;

/// Content-addressed packs and discovery summaries in the shared indexed store.
/// Module identity groups remain ordered by artifact key for deterministic offers.
/// Dev packs share a procedure domain; linked packs and outputs use exact targets.
pub const Store = struct {
    allocator: Allocator,
    roc_ctx: CoreCtx,
    /// Exact final-link target and optimization level.
    root: []u8,
    /// Logical scope for procedure packs and discovery summaries.
    procedure_root: []u8,
    native: compile.artifacts.Store,
    variant: [32]u8,
    verbose: bool,

    pub const InitError = Allocator.Error || error{NoHomeDirectory};

    pub fn init(allocator: Allocator, config: compile.CacheConfig, target: RocTarget, opt_name: []const u8) InitError!Store {
        return initForStrategy(allocator, config, target, opt_name, .lss);
    }

    pub fn initForStrategy(allocator: Allocator, config: compile.CacheConfig, target: RocTarget, opt_name: []const u8, strategy: base.SpecializationStrategy) InitError!Store {
        const root = try std.fmt.allocPrint(allocator, "{s}-{s}", .{ @tagName(target), opt_name });
        errdefer allocator.free(root);
        const procedure_root = if (std.mem.eql(u8, opt_name, "dev")) blk: {
            const domain = backend.dev.NativeProcDomain.init(target, strategy);
            break :blk try std.fmt.allocPrint(allocator, "proc-{s}-dev", .{std.fmt.bytesToHex(domain.key(), .lower)});
        } else try allocator.dupe(u8, root);
        errdefer allocator.free(procedure_root);
        const cache_root = try config.getEffectiveCacheDir(allocator);
        defer allocator.free(cache_root);
        return .{ .allocator = allocator, .roc_ctx = config.roc_ctx, .root = root, .procedure_root = procedure_root, .variant = compile.artifacts.hashBytes(opt_name), .native = try compile.artifacts.Store.init(allocator, config.roc_ctx.std_io, cache_root), .verbose = config.verbose };
    }

    pub fn deinit(self: *Store) void {
        self.native.deinit();
        self.allocator.free(self.root);
        self.allocator.free(self.procedure_root);
    }

    fn identity(self: *const Store, family: compile.artifacts.Family, module: [32]u8, key: [32]u8) compile.artifacts.Identity {
        var result: compile.artifacts.Identity = .{ .namespace = undefined, .family = family, .group = undefined, .key = key, .pack_format = PackFile.format_version, .variant = self.variant };
        std.crypto.hash.sha2.Sha256.hash(@import("build_options").compiler_compatibility_id, &result.namespace, .{});
        var hash = std.crypto.hash.sha2.Sha256.init(.{});
        hash.update(switch (family) {
            .local_pack, .package_pack, .local_discovery, .package_discovery => self.procedure_root,
            else => self.root,
        });
        hash.update(&module);
        hash.final(&result.group);
        return result;
    }

    const IndexedKind = enum { payload, metadata, offer, completed, interface };

    fn indexedIdentity(self: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8, key: [32]u8, kind: IndexedKind) compile.artifacts.Identity {
        var id = self.packIdentity(origin, module, key);
        var hash = std.crypto.hash.sha2.Sha256.init(.{});
        hash.update(&id.variant);
        hash.update("roc-indexed-procedure-v1");
        hash.update(@tagName(kind));
        hash.final(&id.variant);
        return id;
    }

    pub fn linkObjectIdentity(self: *const Store, digest: [32]u8) compile.artifacts.Identity {
        var id = self.identity(.program_output, @splat(0), digest);
        var hash = std.crypto.hash.sha2.Sha256.init(.{});
        hash.update(&id.variant);
        hash.update("roc-native-link-object-v1");
        hash.final(&id.variant);
        return id;
    }

    fn readPrefix(self: *const Store, id: compile.artifacts.Identity, len: usize, allocator: Allocator) Allocator.Error!?[]u8 {
        var artifact = (self.native.acquire(id, self.native.now()) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return null,
        }) orelse return null;
        defer artifact.deinit();
        if (artifact.size < len) return null;
        const bytes = try allocator.alloc(u8, len);
        const count = artifact.file.readPositionalAll(self.native.io, bytes, 0) catch {
            allocator.free(bytes);
            return null;
        };
        if (count != len) {
            allocator.free(bytes);
            return null;
        }
        return bytes;
    }

    pub fn readProgramPrefix(self: *const Store, key: [32]u8, len: usize, allocator: Allocator) Allocator.Error!?[]const u8 {
        return self.readPrefix(self.identity(.program_output, @splat(0), key), len, allocator);
    }

    fn packIdentity(self: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8, key: [32]u8) compile.artifacts.Identity {
        return self.identity(if (origin == .local) .local_pack else .package_pack, module, key);
    }

    fn read(self: *const Store, allocator: Allocator, id: compile.artifacts.Identity) Allocator.Error!?[]u8 {
        return self.native.read(allocator, id, self.native.now()) catch |err| switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            else => null,
        };
    }

    fn writeEntry(self: *const Store, id: compile.artifacts.Identity, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        self.native.write(id, bytes, self.native.now()) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.Busy => return,
            else => {
                if (self.verbose) {
                    const message = try std.fmt.allocPrint(self.allocator, "Failed to publish object cache at {s} ({d} bytes): {}\n", .{ self.native.root, bytes.len, err });
                    defer self.allocator.free(message);
                    self.roc_ctx.writeStderr(message) catch {};
                }
                return error.PackWriteFailed;
            },
        };
    }

    pub fn writeDiscovery(self: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        var key: [32]u8 = undefined;
        std.crypto.hash.sha2.Sha256.hash(bytes, &key, .{});
        try self.writeEntry(self.identity(if (origin == .local) .local_discovery else .package_discovery, module, key), bytes);
    }

    pub fn readProgram(self: *const Store, key: [32]u8, allocator: Allocator) Allocator.Error!?[]u8 {
        return self.read(allocator, self.identity(.program_output, @splat(0), key));
    }

    pub fn writeProgram(self: *const Store, key: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        try self.writeEntry(self.identity(.program_output, @splat(0), key), bytes);
    }

    pub fn optimizedInputKey(self: *const Store, module: [32]u8) Allocator.Error!?[32]u8 {
        const bytes = (try self.readPrefix(self.identity(.optimized_pack, module, @splat(0)), PackFile.object_input_key_prefix_len, self.allocator)) orelse return null;
        defer self.allocator.free(bytes);
        return PackFile.objectInputKey(bytes);
    }

    pub fn replaceOptimized(self: *const Store, module: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        try self.writeEntry(self.identity(.optimized_pack, module, @splat(0)), bytes);
    }

    pub fn has(self: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8, key: [32]u8) Allocator.Error!bool {
        var artifact = (self.native.acquire(self.packIdentity(origin, module, key), self.native.now()) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return false,
        }) orelse return false;
        defer artifact.deinit();
        return true;
    }

    pub fn write(self: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8, key: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        try self.writeEntry(self.packIdentity(origin, module, key), bytes);
    }

    pub fn loadIdentity(self: *const Store, _: std.Io, packs: *LoadedPacks, origin: compile.BuildEnv.PackOrigin, module: [32]u8) LoadedPacks.LoadError!void {
        try packs.addIndexProvider(self, origin, module);
    }

    /// Publish immutable artifact bytes first, then independent content-named
    /// offers. Writers never replace a shared manifest, so concurrent additions
    /// cannot lose one another. The caller publishes its completion marker last.
    pub fn writeIndexed(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, set: *const backend.dev.CodeExcerpt.Set, specs: []const PackFile.SpecEntry) (Allocator.Error || error{PackWriteFailed})!void {
        return self.writeIndexedForProviders(origin, identity, set, specs, null);
    }

    /// Publish one producer's shared records and route each offer to its
    /// explicitly stamped provider. Null routes every offer to the producer.
    pub fn writeIndexedForProviders(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, set: *const backend.dev.CodeExcerpt.Set, specs: []const PackFile.SpecEntry, providers: ?[]const compile.BuildEnv.PackPlacement) (Allocator.Error || error{PackWriteFailed})!void {
        const tracing = base.dev_diagnostics.isSet(.pack_stats);
        const start = if (tracing) std.Io.Timestamp.now(self.roc_ctx.std_io, .awake).nanoseconds else 0;
        var metadata_bytes: u64 = 0;
        var payload_bytes: u64 = 0;
        var records_written: u64 = 0;
        var records_reused: u64 = 0;
        defer if (tracing) std.debug.print("pack index publication: offers={d} metadata_bytes={d} payload_bytes={d} records_written={d} records_reused={d} wall_ns={d}\n", .{ specs.len, metadata_bytes, payload_bytes, records_written, records_reused, std.Io.Timestamp.now(self.roc_ctx.std_io, .awake).nanoseconds - start });
        if (providers) |locations| std.debug.assert(locations.len == specs.len);
        if (specs.len == 0) return;
        const allocator = self.allocator;
        var names = try backend.dev.CodeExcerpt.ContentNames.init(allocator, set);
        defer names.deinit();
        var closure = try SpliceClosure.init(allocator, set);
        defer closure.deinit();
        const included = try allocator.alloc(bool, set.excerpts.len);
        defer allocator.free(included);
        @memset(included, false);
        for (specs) |spec| for (try closure.of(spec.excerpt)) |index| {
            included[index] = true;
        };
        var union_indices = std.ArrayList(u32).empty;
        defer union_indices.deinit(allocator);
        for (included, 0..) |include, index| if (include) {
            try union_indices.append(allocator, @intCast(index));
        };
        const hashes = try allocator.alloc([32]u8, set.excerpts.len);
        defer allocator.free(hashes);
        var published = std.AutoHashMap([32]u8, void).init(allocator);
        defer published.deinit();
        for (union_indices.items) |index| {
            const bytes = try Index.payload(allocator, set.excerpts[index]);
            defer allocator.free(bytes);
            const hash = Index.digest(bytes);
            hashes[index] = hash;
            if ((try published.getOrPut(hash)).found_existing) continue;
            if (try self.publishIndexed(self.indexedIdentity(origin, identity, hash, .payload), bytes)) {
                payload_bytes += bytes.len;
                records_written += 1;
            } else {
                records_reused += 1;
            }
        }
        const metadata = try Index.encodeMetadata(allocator, set, specs, union_indices.items, hashes, &names);
        defer allocator.free(metadata);
        const metadata_hash = Index.digest(metadata);
        if (try self.publishIndexed(self.indexedIdentity(origin, identity, metadata_hash, .metadata), metadata)) {
            metadata_bytes += metadata.len;
            records_written += 1;
        } else {
            records_reused += 1;
        }
        for (specs, 0..) |spec, spec_index| {
            const provider = if (providers) |locations| locations[spec_index] else compile.BuildEnv.PackPlacement{ .origin = origin, .identity = identity };
            const bytes = (Index.Offer{ .metadata = metadata_hash, .spec = @intCast(spec_index), .origin = switch (origin) {
                .local => .local,
                .pkg => .pkg,
            }, .owner = identity }).encode();
            var record: [32 + @sizeOf(@TypeOf(bytes))]u8 = undefined;
            @memcpy(record[0..32], &provider.identity);
            @memcpy(record[32..], &bytes);
            if (try self.publishIndexed(self.indexedIdentity(provider.origin, spec.key, Index.digest(&record), .offer), &record)) {
                metadata_bytes += bytes.len;
                records_written += 1;
            } else {
                records_reused += 1;
            }
        }
    }

    /// Whether this exact producer completed indexed publication. A missing or
    /// corrupt marker is a miss; artifact availability is checked per lookup.
    pub fn hasIndexed(self: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8, key: [32]u8) Allocator.Error!bool {
        const bytes = (try self.read(self.allocator, self.indexedIdentity(origin, module, key, .completed))) orelse return false;
        defer self.allocator.free(bytes);
        return std.mem.eql(u8, bytes, "ROCINDEX01");
    }

    pub fn completeIndexed(self: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8, key: [32]u8) (Allocator.Error || error{PackWriteFailed})!void {
        _ = try self.publishIndexed(self.indexedIdentity(origin, module, key, .completed), "ROCINDEX01");
    }

    pub fn loadIdentityDiscovery(self: *const Store, _: std.Io, packs: *LoadedPacks, origin: compile.BuildEnv.PackOrigin, module: [32]u8) LoadedPacks.LoadError!void {
        try self.loadGroup(packs, self.identity(if (origin == .local) .local_discovery else .package_discovery, module, @splat(0)), .discovery);
    }

    fn publishIndexed(self: *const Store, id: compile.artifacts.Identity, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!bool {
        if (try self.read(self.allocator, id)) |existing| {
            defer self.allocator.free(existing);
            if (std.mem.eql(u8, existing, bytes)) return false;
        }
        self.native.write(id, bytes, self.native.now()) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            // Publication is optional, but a partial publication must not get a completion marker.
            else => return error.PackWriteFailed,
        };
        return true;
    }

    fn loadGroup(self: *const Store, packs: *LoadedPacks, id: compile.artifacts.Identity, files: LoadedPacks.Files) LoadedPacks.LoadError!void {
        const keys = self.native.groupKeys(self.allocator, id) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return,
        };
        defer self.allocator.free(keys);
        for (keys) |key| {
            var artifact = (self.native.acquireKey(key, self.native.now()) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => continue,
            }) orelse continue;
            defer artifact.deinit();
            var arena = base.SingleThreadArena.init(self.allocator);
            const bytes = artifact.read(arena.allocator()) catch |err| {
                arena.deinit();
                if (err == error.OutOfMemory) return error.OutOfMemory;
                continue;
            };
            packs.bytes_loaded +|= bytes.len;
            if (files == .discovery) {
                defer arena.deinit();
                try packs.appendDiscovery(bytes);
            } else packs.appendPack(arena, bytes) catch |err| switch (err) {
                error.UnsupportedPackVersion => continue,
                else => return err,
            };
        }
    }
};

test "object cache loading owns decoded packs and commits both indexes atomically" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const identity = lir.ProcIdentity.forTest(43);
    const key = @as([32]u8, @splat(41));
    const set = backend.dev.CodeExcerpt.Set{
        .arena = @import("base").SingleThreadArena.init(allocator),
        .excerpts = &.{.{
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
        .excerpt = 0,
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
            // A consumer that cannot splice is never served dev excerpts.
            try std.testing.expect(packs.specCacheLookup().lookup(expected_key, .linked_objects, &.{}) == null);
            const located = packs.spliceSource().find(packs.spliceSource().context, expected_key) orelse return error.TestUnexpectedResult;
            try std.testing.expectEqual(expected_identity, located.set.excerpts[located.index].kind.proc);
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
            try std.testing.expectEqual(@as(u64, 0), packs.excerpts_served);
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

test "object cache accepts new offers after indexing without moving served excerpts" {
    const allocator = std.testing.allocator;
    var packs = LoadedPacks.init(allocator);
    defer packs.deinit();
    try packs.indexPacks();
    var first: ?backend.dev.LocatedExcerpt = null;
    for (0..32) |index| {
        const identity = lir.ProcIdentity.forTest(@intCast(index));
        const key = @as([32]u8, @splat(@intCast(index)));
        const set = backend.dev.CodeExcerpt.Set{
            .arena = @import("base").SingleThreadArena.init(allocator),
            .excerpts = &.{.{
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
            .excerpt = 0,
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
        try std.testing.expectEqual(identity, located.set.excerpts[located.index].kind.proc);
        if (first) |served| {
            try std.testing.expectEqual(&packs.packs.items[0].set, served.set);
            try std.testing.expectEqual(lir.ProcIdentity.forTest(0), served.set.excerpts[served.index].kind.proc);
        } else first = located;
    }
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
    var interface_producer = LoadedPacks.init(a);
    defer interface_producer.deinit();
    interface_producer.interface_store = &musl;
    try LoadedPacks.recordInterface(&interface_producer, identity, key, "interface relation");
    try interface_producer.publishInterfaces();
    var interface_consumer = LoadedPacks.init(a);
    defer interface_consumer.deinit();
    interface_consumer.interface_store = &glibc;
    const interface_bytes = (try LoadedPacks.loadInterface(&interface_consumer, a, identity, key)).?;
    defer a.free(interface_bytes);
    try std.testing.expectEqualStrings("interface relation", interface_bytes);
    // Indexed offers, metadata, payloads and completion markers share the
    // procedure domain too.
    const offered_key: [32]u8 = @splat(3);
    const artifact: backend.dev.CodeExcerpt.Excerpt = .{ .kind = .{ .proc = lir.ProcIdentity.forTest(4) }, .code = "\xc3", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} };
    const spec: PackFile.SpecEntry = .{ .key = offered_key, .excerpt = 0, .rc_borrowed_params = 0, .rc_ret_borrowed = false, .rc_ret_lenders = 0, .rc_read_only_params = 0, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{} };
    try musl.writeIndexed(.local, identity, &.{ .arena = undefined, .excerpts = &.{artifact} }, &.{spec});
    try musl.completeIndexed(.local, identity, key);
    try std.testing.expect(try glibc.hasIndexed(.local, identity, key));
    {
        var packs = LoadedPacks.init(a);
        defer packs.deinit();
        try glibc.loadIdentity(io, &packs, .local, identity);
        try packs.indexPacks();
        try std.testing.expect(packs.specCacheLookup().lookup(offered_key, .dev_code, &.{}) != null);
    }
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
        try std.testing.expect(!try incompatible.hasIndexed(.local, identity, key));
        var packs = LoadedPacks.init(a);
        defer packs.deinit();
        packs.interface_store = &incompatible;
        try std.testing.expectEqual(null, try LoadedPacks.loadInterface(&packs, a, identity, key));
        try incompatible.loadIdentity(io, &packs, .local, identity);
        try packs.indexPacks();
        try std.testing.expect(packs.specCacheLookup().lookup(offered_key, .dev_code, &.{}) == null);
    }
    var boxy = try Store.initForStrategy(a, config, .x64glibc, "dev", .boxy);
    defer boxy.deinit();
    try std.testing.expect(!try boxy.has(.local, identity, key));
    try std.testing.expect(!try boxy.hasIndexed(.local, identity, key));
    var boxy_interfaces = LoadedPacks.init(a);
    defer boxy_interfaces.deinit();
    boxy_interfaces.interface_store = &boxy;
    try std.testing.expectEqual(null, try LoadedPacks.loadInterface(&boxy_interfaces, a, identity, key));
    for ([_][]const u8{ "speed", "size" }) |level| {
        var left = try Store.init(a, config, .x64musl, level);
        defer left.deinit();
        var right = try Store.init(a, config, .x64glibc, level);
        defer right.deinit();
        try left.write(.pkg, identity, key, "linked-only package");
        try std.testing.expect(!try right.has(.pkg, identity, key));
        const left_key = left.identity(.optimized_pack, identity, identity).indexedKey();
        const right_key = right.identity(.optimized_pack, identity, identity).indexedKey();
        try std.testing.expect(!std.mem.eql(u8, &left_key, &right_key));
    }
}

test "indexed pack families share retention and preserve accepted bytes" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var store = try Store.init(allocator, .{ .cache_dir = root, .roc_ctx = CoreCtx.default(allocator, allocator, io) }, .x64glibc, "dev");
    defer store.deinit();
    const module: [32]u8 = @splat(1);
    const key: [32]u8 = @splat(2);
    try store.write(.local, module, key, "local");
    try store.write(.pkg, module, key, "package");
    const owned = (try store.read(allocator, store.packIdentity(.local, module, key))).?;
    defer allocator.free(owned);
    var now = store.native.now() + 2 * compile.artifacts.day_ns;
    for (0..180) |_| {
        var stats = compile.maintenance.Stats{};
        compile.maintenance.run(&store.native, .{ .object_pack_format_version = PackFile.format_version, .max_size_bytes = std.math.maxInt(u64) }, now, .compulsory, &stats);
        now += std.time.ns_per_s;
    }
    try std.testing.expect(!try store.has(.local, module, key));
    try std.testing.expect(try store.has(.pkg, module, key));
    try std.testing.expectEqualStrings("local", owned);
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
        const selection = try selectionFor(allocator, build_env, root_artifact);
        defer allocator.free(selection.modules);
        return self.selectionDigest(selection);
    }

    /// Optimized object identities selected from a checked root and its imports.
    pub const Selection = struct { modules: []const PackageModule, optimized_identity: ?[32]u8 };

    /// Capture the checked producer's selection for later validation without IR.
    pub fn selectionFor(allocator: Allocator, build_env: *compile.BuildEnv, root: *const check.CheckedArtifact.CheckedModuleArtifact) Allocator.Error!Selection {
        return .{
            .modules = try packageModules(allocator, build_env, root),
            .optimized_identity = if (programPackPlacement(build_env, root)) |placement| placement.identity else null,
        };
    }

    /// Selection was emitted by the checked producer. Its keys let an early
    /// completed-build lookup inspect current optimized stores without IR.
    pub fn selectionDigest(self: *const LinkedStores, selection: Selection) Allocator.Error![32]u8 {
        const allocator = self.stores[0].allocator;
        var hasher = std.crypto.hash.Blake3.init(.{});
        hasher.update("roc.optimization-state.v1");
        for (&self.stores) |*store| {
            for (selection.modules) |module| {
                if (!try store.has(.pkg, module.identity, module.key)) continue;
                hasher.update(&store.packIdentity(.pkg, module.identity, module.key).indexedKey());
            }
            const identity = selection.optimized_identity orelse continue;
            const input_key = try store.optimizedInputKey(identity) orelse continue;
            hasher.update(&store.identity(.optimized_pack, identity, @splat(0)).indexedKey());
            hasher.update(&input_key);
        }
        var digest: [32]u8 = undefined;
        hasher.final(&digest);
        return digest;
    }
};

/// The placement for program-wide packs: the app's when the program has one,
/// otherwise the root's. Every app of a platform shares its executable root;
/// tests need not even load that platform. The app is the common declared owner
/// for native CTFE packs and optimized program packs across these commands.
pub fn programPackPlacement(build_env: *compile.BuildEnv, root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact) ?compile.BuildEnv.PackPlacement {
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
    set: *const backend.dev.CodeExcerpt.Set,
    modules: [32]u8,

    fn of(set: *const backend.dev.CodeExcerpt.Set, modules: Common.ProgramModules) ProgramPack {
        var hasher = std.crypto.hash.Blake3.init(.{});
        for (modules) |identity| hasher.update(&identity);
        var digest: [32]u8 = undefined;
        hasher.final(&digest);
        return .{ .set = set, .modules = digest };
    }
};

/// A URL-package module in view of a program: its placement identity and the
/// artifact key its object pack is filed under.
pub const PackageModule = struct {
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
    /// Explicit-root commands have no executable root. Checking supplies their
    /// complete module set before any specialization asks for an artifact.
    explicit_artifacts: []const *const check.CheckedArtifact.CheckedModuleArtifact = &.{},
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

    const IndexedMetadata = struct {
        pack: *PackFile.Pack,
        hashes: []const [32]u8,
        closure: SpliceClosure,
        materialized: []bool,
    };
    const MissKey = struct {
        key: [32]u8,
        takes: Common.SpecCacheTakes,
        modules: [32]u8,
    };
    const IndexedResult = enum { hit, miss, unavailable };

    /// One checked module/context provider, shared by its procedure requests.
    const InterfaceFamily = struct {
        records: std.AutoArrayHashMapUnmanaged([32]u8, []const u8) = .empty,
        loaded: bool = false,
        dirty: bool = false,
    };
    const Provider = struct { store: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8 };
    const IndexRoot = struct { store: *const Store, origin: compile.BuildEnv.PackOrigin };
    interface_mutex: std.Io.Mutex = .init,
    /// Worker reads update this under interface_mutex; reported after joining.
    interface_bytes_loaded: u64 = 0,
    interface_families: std.AutoHashMapUnmanaged([32]u8, *InterfaceFamily) = .empty,
    interface_store: ?*const Store = null,
    allocator: Allocator,
    index_dirs: std.ArrayList(Provider) = .empty,
    index_roots: std.ArrayList(IndexRoot) = .empty,
    provider_ranks: std.AutoHashMapUnmanaged([32]u8, u32) = .empty,
    metadata: std.AutoHashMapUnmanaged([32]u8, *IndexedMetadata) = .empty,
    negative: std.AutoHashMapUnmanaged(MissKey, void) = .empty,

    payloads: std.AutoHashMapUnmanaged([32]u8, []const u8) = .empty,
    index_stats: IndexStats = .{},
    /// A completed-output probe has priority over procedure discovery. Until
    /// it misses, compile-time demand uses source bodies and cached values.
    defer_code_lookup: bool = false,
    packs: std.ArrayList(*PackFile.Pack),
    specs: std.AutoHashMap([32]u8, Common.SpecCacheHit),
    /// The excerpt of each entry `specs` holds, by the same key.
    excerpts: std.AutoHashMap([32]u8, backend.dev.LocatedExcerpt),
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
    dev_reaches_linked: std.AutoHashMapUnmanaged(*const backend.dev.CodeExcerpt.Set, []bool) = .empty,
    /// Per dev pack and asking program, whether a splice of each excerpt
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
    excerpts_served: u64 = 0,
    /// Bytes read from procedure, discovery and linked-object files.
    bytes_loaded: u64 = 0,
    lookups: u64 = 0,
    /// Set until `indexPacks` runs; lookups before that register the pending
    /// store's index directories for the modules in view first.
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

    pub const IndexStats = struct {
        index_bytes: u64 = 0,
        bucket_opens: u64 = 0,
        bucket_absent: u64 = 0,
        negative_hits: u64 = 0,
        metadata_opens: u64 = 0,
        metadata_bytes: u64 = 0,
        candidates: u64 = 0,
        duplicates: u64 = 0,
        payload_opens: u64 = 0,
        payload_files: u64 = 0,
        payload_bytes: u64 = 0,
        decoded_artifacts: u64 = 0,
        requested_artifacts: u64 = 0,
        served_artifacts: u64 = 0,
        allocation_failures: u64 = 0,
        requested: u64 = 0,
        served: u64 = 0,
        misses: u64 = 0,
        source_ineligible: u64 = 0,
        linked_ineligible: u64 = 0,
        corrupt: u64 = 0,
        missing: u64 = 0,
    };

    pub fn addIndexProvider(self: *LoadedPacks, store: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8) Allocator.Error!void {
        const provider_key = store.packIdentity(origin, module, @splat(0)).indexedKey()[0..32].*;
        if (self.provider_ranks.contains(provider_key)) return;
        try self.index_dirs.ensureUnusedCapacity(self.allocator, 1);
        try self.index_roots.ensureUnusedCapacity(self.allocator, 1);
        try self.provider_ranks.put(self.allocator, provider_key, @intCast(self.index_dirs.items.len));
        self.index_dirs.appendAssumeCapacity(.{ .store = store, .origin = origin, .module = module });
        const root_key = store.packIdentity(origin, @splat(0), @splat(0)).indexedKey()[0..32].*;
        const known = for (self.index_roots.items) |existing| {
            const previous = existing.store.packIdentity(existing.origin, @splat(0), @splat(0)).indexedKey()[0..32].*;
            if (std.mem.eql(u8, &previous, &root_key)) break true;
        } else false;
        if (!known) self.index_roots.appendAssumeCapacity(.{ .store = store, .origin = origin });
        self.refreshIndex();
    }

    /// Start a fresh optional-cache snapshot after publication. This also
    /// invalidates misses when an existing provider acquired new offers.
    /// Other processes' later publications become visible at the next refresh
    /// or in the next compilation; a miss never promises code availability.
    pub fn refreshIndex(self: *LoadedPacks) void {
        self.negative.clearRetainingCapacity();
        var families = self.interface_families.valueIterator();
        while (families.next()) |family| family.*.loaded = false;
    }

    /// Register freshly published offers in the checked program's provider order.
    pub fn offerIndexed(self: *LoadedPacks, store: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) Allocator.Error!void {
        self.ensureLoaded();
        try store.loadIdentity(store.roc_ctx.std_io, self, origin, identity);
        self.refreshIndex();
    }

    fn loadMetadata(self: *LoadedPacks, store: *const Store, origin: compile.BuildEnv.PackOrigin, owner: [32]u8, hash: [32]u8) Allocator.Error!?*IndexedMetadata {
        if (self.metadata.get(hash)) |existing| return existing;
        const allocator = self.allocator;
        self.index_stats.metadata_opens += 1;
        const bytes = (try store.read(allocator, store.indexedIdentity(origin, owner, hash, .metadata))) orelse {
            self.index_stats.missing += 1;
            return null;
        };
        defer allocator.free(bytes);
        self.bytes_loaded +|= bytes.len;
        self.index_stats.index_bytes += bytes.len;
        self.index_stats.metadata_bytes += bytes.len;
        if (!std.mem.eql(u8, &Index.digest(bytes), &hash)) {
            self.index_stats.corrupt += 1;
            return null;
        }
        var decoded = Index.decodeMetadata(allocator, bytes) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                self.index_stats.corrupt += 1;
                return null;
            },
        };
        errdefer decoded.pack.deinit();
        const pack = try allocator.create(PackFile.Pack);
        errdefer allocator.destroy(pack);
        pack.* = decoded.pack;
        const entry = try allocator.create(IndexedMetadata);
        errdefer allocator.destroy(entry);
        var closure = try SpliceClosure.init(allocator, &pack.set);
        errdefer closure.deinit();
        const materialized = try allocator.alloc(bool, pack.set.excerpts.len);
        errdefer allocator.free(materialized);
        @memset(materialized, false);
        entry.* = .{ .pack = pack, .hashes = decoded.hashes, .closure = closure, .materialized = materialized };
        try self.packs.ensureUnusedCapacity(allocator, 1);
        try self.metadata.put(allocator, hash, entry);
        self.packs.appendAssumeCapacity(pack);
        return entry;
    }

    /// Probe direct key buckets, then only candidates belonging to visible
    /// providers. Provider rank remains root-first, independent of disk order.
    fn findIndexed(self: *LoadedPacks, key: [32]u8, takes: Common.SpecCacheTakes, modules: Common.ProgramModules) Allocator.Error!IndexedResult {
        if (self.index_roots.items.len == 0) return .unavailable;
        const allocator = self.allocator;
        const Candidate = struct {
            digest: [32]u8,
            offer: Index.Offer,
            store: *const Store,
            rank: u32,
            fn less(_: void, a: @This(), b: @This()) bool {
                if (a.rank != b.rank) return a.rank < b.rank;
                return std.mem.lessThan(u8, &a.digest, &b.digest);
            }
        };
        var candidates = std.ArrayList(Candidate).empty;
        defer candidates.deinit(allocator);
        var cacheable = true;
        for (self.index_roots.items) |root| {
            self.index_stats.bucket_opens += 1;
            const group = root.store.indexedIdentity(root.origin, key, @splat(0), .offer);
            const keys = root.store.native.groupKeys(allocator, group) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => {
                    cacheable = false;
                    continue;
                },
            };
            defer allocator.free(keys);
            if (keys.len == 0) self.index_stats.bucket_absent += 1;
            for (keys) |catalog_key| {
                var artifact = (root.store.native.acquireKey(catalog_key, root.store.native.now()) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    else => {
                        cacheable = false;
                        continue;
                    },
                }) orelse {
                    cacheable = false;
                    continue;
                };
                defer artifact.deinit();
                if (artifact.size > 1024) {
                    self.index_stats.corrupt += 1;
                    cacheable = false;
                    continue;
                }
                const bytes = artifact.read(allocator) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    else => {
                        cacheable = false;
                        continue;
                    },
                };
                defer allocator.free(bytes);
                self.bytes_loaded +|= bytes.len;
                self.index_stats.index_bytes += bytes.len;
                if (bytes.len < 32 or !std.mem.eql(u8, catalog_key[32..], &Index.digest(bytes))) {
                    self.index_stats.corrupt += 1;
                    cacheable = false;
                    continue;
                }
                const provider = root.store.packIdentity(root.origin, bytes[0..32].*, @splat(0)).indexedKey()[0..32].*;
                const rank = self.provider_ranks.get(provider) orelse continue;
                const offer = Index.Offer.decode(bytes[32..]) catch {
                    self.index_stats.corrupt += 1;
                    cacheable = false;
                    continue;
                };
                try candidates.append(allocator, .{ .digest = Index.digest(bytes[32..]), .offer = offer, .store = root.store, .rank = rank });
            }
        }
        std.mem.sort(Candidate, candidates.items, {}, Candidate.less);
        for (candidates.items, 0..) |candidate, candidate_index| {
            self.index_stats.candidates += 1;
            if (candidate_index != 0) self.index_stats.duplicates += 1;
            const offer = candidate.offer;
            const origin: compile.BuildEnv.PackOrigin = switch (offer.origin) {
                .local => .local,
                .pkg => .pkg,
            };
            const metadata = try self.loadMetadata(candidate.store, origin, offer.owner, offer.metadata) orelse {
                cacheable = false;
                continue;
            };
            const pack = metadata.pack;
            if (offer.spec >= pack.specs.len or !std.mem.eql(u8, &pack.specs[offer.spec].key, &key)) {
                self.index_stats.corrupt += 1;
                cacheable = false;
                continue;
            }
            const spec = pack.specs[offer.spec];
            if (takes == .test_dev_code and !spec.test_compatible) continue;
            const program = ProgramPack.of(&pack.set, modules);
            const absent = self.lines_outside.get(program) orelse blk: {
                const present = try allocator.alloc(bool, pack.set.source_files.len);
                defer allocator.free(present);
                for (pack.set.source_files, present) |identity, *is_present| is_present.* = Common.programHasModule(modules, identity);
                const marks = try backend.dev.CodeExcerpt.reachingAbsentFiles(allocator, &pack.set, present);
                errdefer allocator.free(marks);
                try self.lines_outside.put(allocator, program, marks);
                break :blk marks;
            };
            if (absent[spec.excerpt]) {
                self.index_stats.source_ineligible += 1;
                continue;
            }
            if (takes.refusesDevCodeOfLinkedProcs()) {
                self.ensureLinkedLoaded();
                const linked = self.dev_reaches_linked.get(&pack.set) orelse blk: {
                    const marks = try backend.dev.CodeExcerpt.reachingIdentities(allocator, &pack.set, &self.linked_identities);
                    errdefer allocator.free(marks);
                    try self.dev_reaches_linked.put(allocator, &pack.set, marks);
                    break :blk marks;
                };
                if (linked[spec.excerpt]) {
                    self.index_stats.linked_ineligible += 1;
                    continue;
                }
            }
            const reached = try metadata.closure.of(spec.excerpt);
            self.index_stats.requested_artifacts += reached.len;
            var complete = true;
            for (reached) |index| {
                if (metadata.materialized[index]) continue;
                const payload_bytes = try self.readPayload(candidate.store, origin, offer.owner, metadata.hashes[index]) orelse {
                    complete = false;
                    cacheable = false;
                    break;
                };
                Index.materialize(&@constCast(pack.set.excerpts)[index], payload_bytes) catch {
                    self.index_stats.corrupt += 1;
                    complete = false;
                    cacheable = false;
                    break;
                };
                metadata.materialized[index] = true;
                self.index_stats.decoded_artifacts += 1;
            }
            if (!complete) continue;
            try self.excerpts.ensureUnusedCapacity(1);
            try self.specs.ensureUnusedCapacity(1);
            _ = self.specs.remove(key);
            _ = self.excerpts.remove(key);
            self.indexSpec(pack, spec) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => base.invariant("validated indexed metadata has a procedure root", .{}),
            };
            self.index_stats.served_artifacts += reached.len;
            return .hit;
        }
        return if (cacheable) .miss else .unavailable;
    }

    fn readPayload(self: *LoadedPacks, store: *const Store, origin: compile.BuildEnv.PackOrigin, owner: [32]u8, hash: [32]u8) Allocator.Error!?[]const u8 {
        if (self.payloads.get(hash)) |bytes| return bytes;
        self.index_stats.payload_opens += 1;
        const bytes = (try store.read(self.allocator, store.indexedIdentity(origin, owner, hash, .payload))) orelse {
            self.index_stats.missing += 1;
            return null;
        };
        errdefer self.allocator.free(bytes);
        self.bytes_loaded +|= bytes.len;
        self.index_stats.payload_bytes += bytes.len;
        if (!std.mem.eql(u8, &Index.digest(bytes), &hash)) {
            self.allocator.free(bytes);
            self.index_stats.corrupt += 1;
            return null;
        }
        try self.payloads.put(self.allocator, hash, bytes);
        self.index_stats.payload_files += 1;
        return bytes;
    }

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
            .excerpts = std.AutoHashMap([32]u8, backend.dev.LocatedExcerpt).init(allocator),
            .linked_specs = std.AutoHashMap([32]u8, LinkedOffer).init(allocator),
            .discovery_arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    /// Load explicit diagnostic pack files from a directory.
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
                if (err == error.FileNotFound and input == .cache_offers) continue;
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

    /// Acquire owned bytes before accepting an object-cache offer.
    fn loadStoredObject(self: *LoadedPacks, store: *const Store, id: compile.artifacts.Identity) LoadError!void {
        const allocator = self.allocator;
        var arena = base.SingleThreadArena.init(allocator);
        const bytes = (store.read(arena.allocator(), id) catch |err| {
            arena.deinit();
            return err;
        }) orelse {
            arena.deinit();
            return;
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
        for (pack.specs) |spec| try self.indexSpec(pack, spec);
    }

    fn indexSpec(self: *LoadedPacks, pack: *const PackFile.Pack, spec: PackFile.SpecEntry) LoadError!void {
        const root = pack.set.excerpts[spec.excerpt];
        const identity = switch (root.kind) {
            .proc => |identity| identity,
            .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => return error.MalformedPack,
        };
        if (self.specs.contains(spec.key)) return;
        try self.excerpts.ensureUnusedCapacity(1);
        try self.specs.ensureUnusedCapacity(1);
        self.excerpts.putAssumeCapacityNoClobber(spec.key, .{ .set = &pack.set, .index = spec.excerpt });
        self.specs.putAssumeCapacityNoClobber(spec.key, .{
            .identity = identity.bytes,
            .test_compatible = spec.test_compatible,
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

    /// Index every loaded object pack, in load order: the first pack to
    /// offer a key or a procedure serves it. An app-bound procedure built for
    /// another version of the program (`programBindingKey`) is not offered.
    fn indexObjectPacks(self: *LoadedPacks, program_key: [32]u8) LoadError!void {
        for (self.object_packs.items, 0..) |*pack, pack_index| {
            for (pack.specs) |spec| {
                if (!pack.servesProgram(spec.excerpt, program_key)) continue;
                const linked = try self.linked_identities.getOrPut(self.allocator, pack.identities[spec.excerpt]);
                if (!linked.found_existing) linked.value_ptr.* = @intCast(pack_index);
                const gop = try self.linked_specs.getOrPut(spec.key);
                if (!gop.found_existing) gop.value_ptr.* = .{
                    .hit = .{
                        .identity = pack.identities[spec.excerpt].bytes,
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
        const root_placement = programPackPlacement(pending.build_env, root_artifact);
        for (&stores.stores) |*store| {
            for (modules) |module| {
                try self.loadStoredObject(store, store.packIdentity(.pkg, module.identity, module.key));
            }
            if (root_placement) |placement| {
                try self.loadStoredObject(store, store.identity(.optimized_pack, placement.identity, @splat(0)));
            }
        }
        try self.indexObjectPacks(try programBindingKey(self.allocator, pending.build_env, root_artifact));
    }

    /// Register the pending store's keyed indexes for visible modules, without
    /// enumerating historical keys or reading any metadata or payload. Runs once, at the first lookup after
    /// checking has produced the module set; before that a lookup finds
    /// nothing.
    fn ensureLoaded(self: *LoadedPacks) void {
        if (self.state != .pending) return;
        const pending = self.pending orelse return;
        if (pending.explicit_artifacts.len != 0) {
            self.loadExplicitModules(pending, false) catch |err| {
                self.unavailable(err);
                return;
            };
            self.indexPacks() catch |err| self.unavailable(err);
            return;
        }
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
        if (pending.explicit_artifacts.len != 0) {
            self.loadExplicitModules(pending, true) catch {
                self.discovery_state = .unavailable;
                return;
            };
            self.discovery_state = .ready;
            return;
        }
        const root_semantic = pending.build_env.getExecutableRootSemanticData() orelse return;
        const root_artifact = root_semantic.checked_artifact orelse return;
        self.loadPendingDiscovery(pending, root_artifact) catch |err| {
            self.discovery_state = .unavailable;
            std.log.warn("object cache discovery summaries unavailable for this build: {}", .{err});
            return;
        };
        self.discovery_state = .ready;
    }

    fn loadExplicitModules(self: *LoadedPacks, pending: Pending, discovery: bool) LoadError!void {
        const store = pending.store orelse return;
        for (pending.explicit_artifacts) |artifact| {
            const placement = pending.build_env.packPlacementForArtifactKey(artifact.key) orelse continue;
            if (discovery) {
                try store.loadIdentityDiscovery(pending.io, self, placement.origin, placement.identity);
            } else {
                try store.loadIdentity(pending.io, self, placement.origin, placement.identity);
            }
        }
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
        if (base.dev_diagnostics.isSet(.pack_stats)) {
            const stats = self.index_stats;
            std.debug.print("pack index: index_bytes={d} bucket_opens={d} bucket_absent={d} negative_hits={d} metadata_opens={d} metadata_bytes={d} candidates={d} duplicates={d} payload_opens={d} payload_files={d} payload_bytes={d} decoded_artifacts={d} requested_artifacts={d} served_artifacts={d} allocation_failures={d} requested={d} served={d} misses={d} source_ineligible={d} linked_ineligible={d} corrupt={d} missing={d}\n", .{ stats.index_bytes, stats.bucket_opens, stats.bucket_absent, stats.negative_hits, stats.metadata_opens, stats.metadata_bytes, stats.candidates, stats.duplicates, stats.payload_opens, stats.payload_files, stats.payload_bytes, stats.decoded_artifacts, stats.requested_artifacts, stats.served_artifacts, stats.allocation_failures, stats.requested, stats.served, stats.misses, stats.source_ineligible, stats.linked_ineligible, stats.corrupt, stats.missing });
        }
        var interface_families = self.interface_families.valueIterator();
        while (interface_families.next()) |family| {
            for (family.*.records.values()) |bytes| self.allocator.free(bytes);
            family.*.records.deinit(self.allocator);
            self.allocator.destroy(family.*);
        }
        self.interface_families.deinit(self.allocator);
        self.negative.deinit(self.allocator);
        self.provider_ranks.deinit(self.allocator);
        self.index_roots.deinit(self.allocator);
        var metadata = self.metadata.valueIterator();
        while (metadata.next()) |entry| {
            entry.*.closure.deinit();
            self.allocator.free(entry.*.materialized);
            self.allocator.destroy(entry.*);
        }
        self.metadata.deinit(self.allocator);
        self.index_dirs.deinit(self.allocator);
        var payloads = self.payloads.valueIterator();
        while (payloads.next()) |bytes| self.allocator.free(bytes.*);
        self.payloads.deinit(self.allocator);
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
        self.excerpts.deinit();
        self.specs.deinit();
        for (self.packs.items) |pack| {
            pack.deinit();
            self.allocator.destroy(pack);
        }
        self.packs.deinit(self.allocator);
    }

    /// The lookup Monotype consults at reservation.
    pub fn specCacheLookup(self: *LoadedPacks) Common.SpecCacheLookup {
        return .{ .context = @ptrCast(self), .find = findSpec, .find_discovery = findDiscovery, .interfaces = if (!base.dev_diagnostics.isSet(.disable_persistent_interfaces)) .{ .context = self, .load = loadInterface, .record = recordInterface } else null };
    }

    fn interfaceStore(self: *const LoadedPacks) ?*const Store {
        // The checked-module session binds its store after the runtime
        // capability is declared, before any specialization task runs.
        return if (self.pending) |pending| pending.store else self.interface_store;
    }

    /// Caller holds interface_mutex. One read per family and snapshot, including
    /// absence. Publications update the resident family directly.
    fn interfaceFamily(self: *LoadedPacks, store: *const Store, key: [32]u8) Allocator.Error!*InterfaceFamily {
        const entry = try self.interface_families.getOrPut(self.allocator, key);
        if (!entry.found_existing) {
            errdefer _ = self.interface_families.remove(key);
            const family = try self.allocator.create(InterfaceFamily);
            family.* = .{};
            entry.value_ptr.* = family;
        }
        const family = entry.value_ptr.*;
        if (family.loaded) return family;
        const bytes = (try store.read(self.allocator, store.indexedIdentity(.local, key, @splat(0), .interface))) orelse {
            family.loaded = true;
            return family;
        };
        defer self.allocator.free(bytes);
        self.interface_bytes_loaded +|= bytes.len;
        self.readInterfaceFamily(family, key, bytes) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidSummary => return family,
        };
        family.loaded = true;
        return family;
    }

    const InterfaceDecodeError = Allocator.Error || error{InvalidSummary};

    fn readInterfaceFamily(self: *LoadedPacks, family: *InterfaceFamily, key: [32]u8, bytes: []const u8) InterfaceDecodeError!void {
        if (bytes.len < 76 or !std.mem.eql(u8, bytes[0..8], "RIFACE01") or !std.mem.eql(u8, bytes[8..40], &key)) return error.InvalidSummary;
        if (!std.mem.eql(u8, &Index.digest(bytes[72..]), bytes[40..72])) return error.InvalidSummary;
        const count = std.mem.readInt(u32, bytes[72..76], .little);
        var offset: usize = 76;
        // Validate the whole container before installing any member.
        for (0..count) |_| {
            if (bytes.len - offset < 36) return error.InvalidSummary;
            const len = std.mem.readInt(u32, bytes[offset + 32 ..][0..4], .little);
            offset += 36;
            if (len > bytes.len - offset) return error.InvalidSummary;
            offset += len;
        }
        if (offset != bytes.len) return error.InvalidSummary;
        offset = 76;
        for (0..count) |_| {
            const request = bytes[offset..][0..32].*;
            const len = std.mem.readInt(u32, bytes[offset + 32 ..][0..4], .little);
            offset += 36;
            if (!family.records.contains(request)) {
                const owned = try self.allocator.dupe(u8, bytes[offset..][0..len]);
                errdefer self.allocator.free(owned);
                try family.records.put(self.allocator, request, owned);
            }
            offset += len;
        }
    }

    fn loadInterface(context: *anyopaque, allocator: Allocator, family_key: [32]u8, request: [32]u8) Allocator.Error!?[]u8 {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        if (self.defer_code_lookup) return null;
        const store = self.interfaceStore() orelse return null;
        self.interface_mutex.lockUncancelable(store.roc_ctx.std_io);
        defer self.interface_mutex.unlock(store.roc_ctx.std_io);
        const family = try self.interfaceFamily(store, family_key);
        const bytes = family.records.get(request) orelse return null;
        return try allocator.dupe(u8, bytes);
    }

    fn recordInterface(context: *anyopaque, family_key: [32]u8, request: [32]u8, bytes: []const u8) Allocator.Error!void {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        if (self.defer_code_lookup) return;
        const store = self.interfaceStore() orelse return;
        self.interface_mutex.lockUncancelable(store.roc_ctx.std_io);
        defer self.interface_mutex.unlock(store.roc_ctx.std_io);
        const family = try self.interfaceFamily(store, family_key);
        if (family.records.get(request)) |existing| if (std.mem.eql(u8, existing, bytes)) return;
        const owned = try self.allocator.dupe(u8, bytes);
        errdefer self.allocator.free(owned);
        const entry = try family.records.getOrPut(self.allocator, request);
        if (entry.found_existing) self.allocator.free(entry.value_ptr.*);
        entry.value_ptr.* = owned;
        family.dirty = true;
    }

    /// Publish once per changed module/context provider, within the reported cache
    /// publication phase. Concurrent writers can lose optional variants, never
    /// mix partial records: every replacement is complete and atomic.
    pub fn publishInterfaces(self: *LoadedPacks) Allocator.Error!void {
        const store = self.interfaceStore() orelse return;
        self.interface_mutex.lockUncancelable(store.roc_ctx.std_io);
        defer self.interface_mutex.unlock(store.roc_ctx.std_io);
        var families = self.interface_families.iterator();
        while (families.next()) |entry| {
            const family = entry.value_ptr.*;
            if (!family.dirty) continue;
            var bytes = std.ArrayList(u8).empty;
            defer bytes.deinit(self.allocator);
            try bytes.appendSlice(self.allocator, "RIFACE01");
            try bytes.appendSlice(self.allocator, entry.key_ptr);
            try bytes.appendNTimes(self.allocator, 0, 32);
            var number: [4]u8 = undefined;
            std.mem.writeInt(u32, &number, @intCast(family.records.count()), .little);
            try bytes.appendSlice(self.allocator, &number);
            for (family.records.keys(), family.records.values()) |request, payload| {
                try bytes.appendSlice(self.allocator, &request);
                std.mem.writeInt(u32, &number, @intCast(payload.len), .little);
                try bytes.appendSlice(self.allocator, &number);
                try bytes.appendSlice(self.allocator, payload);
            }
            bytes.items[40..72].* = Index.digest(bytes.items[72..]);
            _ = store.publishIndexed(store.indexedIdentity(.local, entry.key_ptr.*, @splat(0), .interface), bytes.items) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.PackWriteFailed => continue,
            };
            family.dirty = false;
        }
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
        if (self.defer_code_lookup) return null;
        self.lookups +|= 1;
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
        self.index_stats.requested += 1;
        var eligible = self.specs.contains(key);
        if (eligible and takes == .test_dev_code and !self.specs.get(key).?.test_compatible) eligible = false;
        if (eligible and self.linesOutsideProgram(key, modules)) {
            self.index_stats.source_ineligible += 1;
            eligible = false;
        }
        if (eligible and takes.refusesDevCodeOfLinkedProcs()) {
            self.ensureLinkedLoaded();
            if (self.devEntryReachesLinked(key)) {
                self.index_stats.linked_ineligible += 1;
                eligible = false;
            }
        }
        if (!eligible) {
            var hasher = std.crypto.hash.Blake3.init(.{});
            for (modules) |identity| hasher.update(&identity);
            var module_digest: [32]u8 = undefined;
            hasher.final(&module_digest);
            const miss_key = MissKey{ .key = key, .takes = takes, .modules = module_digest };
            if (self.negative.contains(miss_key)) {
                self.index_stats.negative_hits += 1;
                self.index_stats.misses += 1;
                return null;
            }
            const result = self.findIndexed(key, takes, modules) catch blk: {
                self.index_stats.allocation_failures += 1;
                break :blk IndexedResult.unavailable;
            };
            if (result != .hit) {
                if (result == .miss) self.negative.put(self.allocator, miss_key, {}) catch {};
                self.index_stats.misses += 1;
                return null;
            }
        }
        const hit = self.specs.get(key) orelse return null;
        self.hits += 1;
        self.index_stats.served += 1;
        return hit;
    }

    /// Whether a splice of the dev entry under `key` would place a line or a
    /// location in a module version outside `modules`.
    fn linesOutsideProgram(self: *LoadedPacks, key: [32]u8, modules: Common.ProgramModules) bool {
        const located = self.excerpts.get(key) orelse return false;
        return self.artifactLinesOutsideProgram(located, modules);
    }

    fn artifactLinesOutsideProgram(self: *LoadedPacks, located: backend.dev.LocatedExcerpt, modules: Common.ProgramModules) bool {
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
        const marks = backend.dev.CodeExcerpt.reachingAbsentFiles(self.allocator, set, present) catch return true;
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
        const located = self.excerpts.get(key) orelse return false;
        const marks = self.dev_reaches_linked.get(located.set) orelse marks: {
            // Refusing the entry is always sound: the program compiles the
            // procedure itself.
            const computed = backend.dev.CodeExcerpt.reachingIdentities(self.allocator, located.set, &self.linked_identities) catch return true;
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
        if (self.defer_code_lookup) return &.{};
        self.ensureDiscoveryLoaded();
        if (self.discovery_state != .ready) return &.{};
        const list = self.discovery.get(key) orelse return &.{};
        return list.items;
    }

    fn findArtifact(context: *anyopaque, key: [32]u8) ?backend.dev.LocatedExcerpt {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        self.ensureLoaded();
        if (self.state != .ready) return null;
        const located = self.excerpts.get(key) orelse return null;
        self.excerpts_served += 1;
        return located;
    }
};

fn nameLessThan(_: void, lhs: []u8, rhs: []u8) bool {
    return std.mem.order(u8, lhs, rhs) == .lt;
}

test "native test indexed admission pins verified payloads and rejects observation ABI mismatches" {
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(root);
    var store = try Store.init(a, .{ .cache_dir = root, .roc_ctx = CoreCtx.default(a, a, io) }, RocTarget.detectNative(), "dev");
    defer store.deinit();
    const identity: [32]u8 = @splat(7);
    const key: [32]u8 = @splat(11);
    const set: backend.dev.CodeExcerpt.Set = .{
        .arena = undefined,
        .excerpts = &.{.{ .kind = .{ .proc = lir.ProcIdentity.forTest(1) }, .code = "\xc3", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} }},
    };
    var spec: PackFile.SpecEntry = .{
        .key = key,
        .excerpt = 0,
        .test_compatible = true,
        .rc_borrowed_params = 0,
        .rc_ret_borrowed = false,
        .rc_ret_lenders = 0,
        .rc_read_only_params = 0,
        .rc_ret_unique = false,
        .rc_ret_unique_fields = 0,
        .rc_ret_conditions = &.{},
    };
    for ([_]bool{ true, false }) |compatible| {
        spec.test_compatible = compatible;
        spec.key = if (compatible) key else @splat(12);
        try store.writeIndexed(.local, identity, &set, &.{spec});
        // Repeated publication reuses both immutable files.
        try store.writeIndexed(.local, identity, &set, &.{spec});
    }
    var packs = LoadedPacks.init(a);
    defer packs.deinit();
    try store.loadIdentity(io, &packs, .local, identity);
    try packs.indexPacks();
    try std.testing.expectEqual(@as(u64, 0), packs.index_stats.payload_files);
    const lookup = packs.specCacheLookup();
    try std.testing.expect(lookup.lookup(@splat(99), .test_dev_code, &.{}) == null);
    try std.testing.expectEqual(@as(u64, 0), packs.index_stats.payload_files);
    try std.testing.expect(lookup.lookup(@splat(12), .test_dev_code, &.{}) == null);
    try std.testing.expect(lookup.lookup(@splat(12), .dev_code, &.{}) != null);
    try std.testing.expect(lookup.lookup(key, .test_dev_code, &.{}) != null);
    try std.testing.expectEqual(@as(u64, 1), packs.index_stats.payload_files);
    // Eviction cannot revoke a promise already made to specialization.
    const payload_bytes = try Index.payload(a, set.excerpts[0]);
    defer a.free(payload_bytes);
    const payload_id = store.indexedIdentity(.local, identity, Index.digest(payload_bytes), .payload);
    try store.native.invalidate(payload_id);
    const source = packs.spliceSource();
    try std.testing.expect(source.find(source.context, key) != null);
    try std.testing.expect(lookup.lookup(key, .test_dev_code, &.{}) != null);
    var missing = LoadedPacks.init(a);
    defer missing.deinit();
    try store.loadIdentity(io, &missing, .local, identity);
    try missing.indexPacks();
    try std.testing.expect(missing.specCacheLookup().lookup(key, .test_dev_code, &.{}) == null);

    try store.writeIndexed(.local, identity, &set, &.{spec});
    var corrupt = LoadedPacks.init(a);
    defer corrupt.deinit();
    try store.loadIdentity(io, &corrupt, .local, identity);
    try corrupt.indexPacks();
    try store.native.write(payload_id, "interrupted payload", store.native.now());
    try std.testing.expect(corrupt.specCacheLookup().lookup(spec.key, .dev_code, &.{}) == null);
    try std.testing.expectEqual(@as(u64, 0), corrupt.hits);
    try store.writeIndexed(.local, identity, &set, &.{spec});
    var repaired = LoadedPacks.init(a);
    defer repaired.deinit();
    try store.loadIdentity(io, &repaired, .local, identity);
    try repaired.indexPacks();
    try std.testing.expect(repaired.specCacheLookup().lookup(spec.key, .dev_code, &.{}) != null);
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
    const frame: backend.dev.CodeExcerpt.Frame = .{ .prologue_size = 1, .stack_alloc = 0, .frame_size = 0, .callee_saved_mask = 0, .epilogue_offset = 1, .uses_frame_pointer = true };
    const set = backend.dev.CodeExcerpt.Set{
        .arena = base_mod.SingleThreadArena.init(allocator),
        .excerpts = &.{
            .{ .kind = .{ .proc = caller }, .code = "\xe8\x00\x00\x00\x00\xc3", .entry = 0, .frame = frame, .refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }}, .relocations = &.{}, .data = &.{}, .loc = .{ .file = 0, .line = 1, .column = 1 } },
            .{ .kind = .{ .proc = callee }, .code = "\xc3", .entry = 0, .frame = frame, .refs = &.{}, .relocations = &.{}, .data = &.{}, .loc = .{ .file = 0, .line = 2, .column = 1 } },
            .{ .kind = .{ .proc = stale }, .code = "\xc3", .entry = 0, .frame = frame, .refs = &.{}, .relocations = &.{}, .data = &.{}, .lines = &.{.{ .offset = 0, .loc = .{ .file = 1, .line = 7, .column = 1 }, .region = base_mod.Region.zero() }} },
        },
        .source_files = &.{ present_module, edited_module },
    };
    const Spec = PackFile.SpecEntry;
    const spec = struct {
        fn of(key: u8, artifact: u32) Spec {
            return .{ .key = @splat(key), .excerpt = artifact, .rc_borrowed_params = 0, .rc_ret_borrowed = false, .rc_ret_lenders = 0, .rc_read_only_params = 0, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{} };
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
            return .{ .key = @splat(key), .excerpt = artifact, .rc_borrowed_params = 0, .rc_ret_borrowed = false, .rc_ret_lenders = 0, .rc_read_only_params = 0, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{} };
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
    const spec: PackFile.SpecEntry = .{ .key = @splat(3), .excerpt = 0, .rc_borrowed_params = 0, .rc_ret_borrowed = false, .rc_ret_lenders = 0, .rc_read_only_params = 0, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{} };
    const offering = try PackFile.writeObject(allocator, "object", &.{offered}, &.{spec}, .{ .app_bound = &.{false} });
    defer allocator.free(offering);
    // What an optimized build files when its program's code could not be a
    // linked object.
    const empty = try PackFile.writeObject(allocator, "", &.{}, &.{}, .{ .input_key = @splat(9), .app_bound = &.{} });
    defer allocator.free(empty);
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var store = try Store.init(allocator, .{ .cache_dir = root, .roc_ctx = CoreCtx.os(allocator, allocator, io) }, .x64glibc, "dev");
    defer store.deinit();
    const module: [32]u8 = @splat(7);
    try store.write(.pkg, module, @splat(1), empty);
    try store.write(.pkg, module, @splat(2), offering);
    var packs = LoadedPacks.init(allocator);
    defer packs.deinit();
    for ([_]u8{ 1, 2 }) |key| try packs.loadStoredObject(&store, store.packIdentity(.pkg, module, @splat(key)));
    try std.testing.expectEqual(@as(usize, 1), packs.object_packs.items.len);
    try std.testing.expectEqualStrings("object", packs.object_packs.items[0].object);
}

const IndexFixture = struct {
    tmp: std.testing.TmpDir,
    arena: std.heap.ArenaAllocator,
    store: Store,
    const allocator = std.testing.allocator;
    const identity: [32]u8 = @splat(0xa1);

    fn init(self: *@This()) (std.Io.Dir.RealPathFileAllocError || Allocator.Error)!void {
        self.tmp = std.testing.tmpDir(.{});
        self.arena = std.heap.ArenaAllocator.init(allocator);
        const root = try self.tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
        defer allocator.free(root);
        self.store = try Store.init(allocator, .{ .cache_dir = root, .roc_ctx = CoreCtx.os(allocator, self.arena.allocator(), std.testing.io) }, .x64glibc, "dev");
    }

    fn deinit(self: *@This()) void {
        self.store.deinit();
        self.arena.deinit();
        self.tmp.cleanup();
    }

    fn loader(self: *@This()) LoadedPacks.LoadError!LoadedPacks {
        var packs = LoadedPacks.init(allocator);
        errdefer packs.deinit();
        try self.store.loadIdentity(std.testing.io, &packs, .local, identity);
        try packs.indexPacks();
        return packs;
    }

    fn artifact(id: u8, code: []const u8) backend.dev.CodeExcerpt.Excerpt {
        return .{ .kind = .{ .proc = lir.ProcIdentity.forTest(id) }, .code = code, .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} };
    }

    fn spec(key: u8, artifact_index: u32) PackFile.SpecEntry {
        return .{ .key = @splat(key), .excerpt = artifact_index, .rc_borrowed_params = 3, .rc_ret_borrowed = true, .rc_ret_lenders = 2, .rc_read_only_params = 1, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{1} };
    }

    fn publish(self: *@This(), artifacts: []const backend.dev.CodeExcerpt.Excerpt, specs: []const PackFile.SpecEntry, files: []const [32]u8) (Allocator.Error || error{PackWriteFailed})!void {
        try self.store.writeIndexed(.local, identity, &.{ .arena = undefined, .excerpts = artifacts, .source_files = files }, specs);
    }

    fn payloadPath(self: *@This(), artifact_value: backend.dev.CodeExcerpt.Excerpt) Allocator.Error![]u8 {
        const bytes = try Index.payload(allocator, artifact_value);
        defer allocator.free(bytes);
        var artifact_file = (self.store.native.acquire(self.store.indexedIdentity(.local, identity, Index.digest(bytes), .payload), self.store.native.now()) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => unreachable,
        }).?;
        defer artifact_file.deinit();
        return allocator.dupe(u8, artifact_file.path);
    }
};

test "indexed object cache reads only requested closure regardless of unrelated history" {
    var fixture: IndexFixture = undefined;
    try fixture.init();
    defer fixture.deinit();
    const F = IndexFixture;
    const module: [32]u8 = @splat(17);
    var root = F.artifact(1, "caller code");
    root.refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
    root.symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(3) } }};
    root.data = &.{.{ .program_local_name = true, .name = "roc__d0", .bytes = "constant bytes", .alignment = 8, .symbol_offset = 0, .relocations = &.{.{ .offset = 0, .name = "roc__d1", .addend = 0, .function = false }} }};
    root.lines = &.{.{ .offset = 0, .loc = .{ .file = 0, .line = 4, .column = 2 }, .region = base.Region.zero() }};
    var spec = F.spec(1, 0);
    spec.comptime_root_reads = &.{.{ .module = module, .root = 9 }};
    var callee = F.artifact(2, "callee code");
    callee.data = &.{.{ .program_local_name = true, .name = "roc__d1", .bytes = "other bytes", .alignment = 8, .symbol_offset = 0, .relocations = &.{} }};
    const unused_code: [64 * 1024]u8 = @splat(3);
    var unused = F.artifact(4, &unused_code);
    unused.loc = .{ .file = 1, .line = 99, .column = 1 };
    const artifacts = [_]backend.dev.CodeExcerpt.Excerpt{ root, callee, F.artifact(3, "symbolic callee"), unused };
    try fixture.publish(&artifacts, &.{ spec, F.spec(200, 3) }, &.{ module, @splat(250) });
    var first = try fixture.loader();
    defer first.deinit();
    const hit = first.specCacheLookup().lookup(spec.key, .dev_code, &.{module}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 3), hit.rc_borrowed_params);
    try std.testing.expectEqual(@as(u32, 9), hit.comptime_root_reads[0].root);
    try std.testing.expectEqualSlices(u32, &.{1}, hit.rc_ret_conditions);
    const located = first.excerpts.get(spec.key).?;
    try std.testing.expectEqual(@as(usize, 4), located.set.excerpts.len);
    try std.testing.expectEqualStrings("constant bytes", located.set.excerpts[located.index].data[0].bytes);
    try std.testing.expectEqual(@as(u32, 4), located.set.excerpts[located.index].lines[0].loc.line);
    try std.testing.expectEqualStrings(located.set.excerpts[1].data[0].name, located.set.excerpts[located.index].data[0].relocations[0].name);
    try std.testing.expect(std.mem.startsWith(u8, located.set.excerpts[1].data[0].name, "roc__h"));
    try std.testing.expectEqual(@as(u64, 3), first.index_stats.payload_files);
    const before = first.index_stats;
    _ = first.specCacheLookup().lookup(spec.key, .dev_code, &.{module}).?;
    try std.testing.expectEqual(before.payload_opens, first.index_stats.payload_opens);
    try std.testing.expectEqual(before.index_bytes, first.index_stats.index_bytes);
    try std.testing.expectEqual(located.set, first.excerpts.get(spec.key).?.set);

    var large: [16 * 1024]u8 = @splat(7);
    for (2..130) |version| {
        large[0] = @intCast(version);
        const unrelated = F.artifact(@intCast(version), &large);
        try fixture.publish(&.{unrelated}, &.{F.spec(@intCast(version), 0)}, &.{});
    }
    // A malformed legacy pack is deliberately irrelevant to indexed lookup.
    try fixture.store.write(.local, F.identity, @splat(99), "not a pack");
    var after = try fixture.loader();
    defer after.deinit();
    _ = after.specCacheLookup().lookup(spec.key, .dev_code, &.{module}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(before.payload_bytes, after.index_stats.payload_bytes);
    try std.testing.expectEqual(before.payload_opens, after.index_stats.payload_opens);
    try std.testing.expectEqual(before.index_bytes, after.index_stats.index_bytes);
    try std.testing.expectEqual(before.decoded_artifacts, after.index_stats.decoded_artifacts);
    try std.testing.expect(after.specCacheLookup().lookup(@splat(250), .dev_code, &.{module}) == null);
}

test "indexed object cache selects eligible provenance deterministically and reuses reverted history" {
    const F = IndexFixture;
    for ([_]bool{ false, true }) |reverse| {
        var fixture: F = undefined;
        try fixture.init();
        defer fixture.deinit();
        const files = [_][32]u8{ @splat(1), @splat(2) };
        var artifact = F.artifact(1, "same machine code");
        artifact.loc = .{ .file = 0, .line = 12, .column = 1 };
        const spec = F.spec(1, 0);
        for (0..2) |i| {
            const version = if (reverse) 1 - i else i;
            try fixture.publish(&.{artifact}, &.{spec}, files[version .. version + 1]);
        }
        // Each source version can find its own candidate regardless of which
        // filename sorts first or which writer published last.
        var packs = try fixture.loader();
        defer packs.deinit();
        for ([_]usize{ 0, 1, 0 }) |version| {
            _ = packs.specCacheLookup().lookup(spec.key, .dev_code, files[version .. version + 1]) orelse return error.TestUnexpectedResult;
            try std.testing.expectEqualSlices([32]u8, files[version .. version + 1], packs.excerpts.get(spec.key).?.set.source_files);
        }
        try std.testing.expectEqual(@as(u64, 1), packs.index_stats.payload_opens);
        try std.testing.expect(packs.index_stats.source_ineligible > 0);
        try std.testing.expect(packs.specCacheLookup().lookup(spec.key, .dev_code, &.{@splat(3)}) == null);
        _ = packs.specCacheLookup().lookup(spec.key, .dev_code, files[0..1]) orelse return error.TestUnexpectedResult;
        try std.testing.expect(packs.specCacheLookup().lookup(@splat(2), .dev_code, &files) == null);
    }
}

test "indexed object cache requires complete intact payloads and owns hits across eviction" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var root = F.artifact(1, "root");
    root.refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
    const dep = F.artifact(2, "dependency");
    const spec = F.spec(1, 0);
    try fixture.publish(&.{ root, dep }, &.{spec}, &.{});
    var pinned = try fixture.loader();
    defer pinned.deinit();
    _ = pinned.specCacheLookup().lookup(spec.key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    const path = try fixture.payloadPath(dep);
    defer F.allocator.free(path);
    try std.Io.Dir.cwd().deleteFile(std.testing.io, path);
    var missing = try fixture.loader();
    defer missing.deinit();
    try std.testing.expect(missing.specCacheLookup().lookup(spec.key, .dev_code, &.{}) == null);
    try std.testing.expectEqual(@as(u64, 1), missing.index_stats.missing);
    try std.testing.expect(!missing.excerpts.contains(spec.key));
    try std.Io.Dir.cwd().writeFile(std.testing.io, .{ .sub_path = path, .data = "corrupt" });
    var corrupt = try fixture.loader();
    defer corrupt.deinit();
    try std.testing.expect(corrupt.specCacheLookup().lookup(spec.key, .dev_code, &.{}) == null);
    try std.testing.expectEqual(@as(u64, 1), corrupt.index_stats.corrupt);
    // Republishing repairs corrupt immutable optional storage.
    try fixture.publish(&.{ root, dep }, &.{spec}, &.{});
    var repaired = try fixture.loader();
    defer repaired.deinit();
    _ = repaired.specCacheLookup().lookup(spec.key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    // Unavailable selected records are not negative results. Both loaders
    // must see the repaired bytes without requiring a snapshot refresh.
    _ = missing.specCacheLookup().lookup(spec.key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    _ = corrupt.specCacheLookup().lookup(spec.key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try fixture.tmp.dir.deleteTree(std.testing.io, "local");
    _ = pinned.specCacheLookup().lookup(spec.key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    const located = pinned.excerpts.get(spec.key).?;
    try std.testing.expectEqualStrings("dependency", located.set.excerpts[1].code);
}

test "indexed object cache source eligibility follows local dependencies but not site-named symbolic procedures" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var root = F.artifact(1, "root");
    root.symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(2) } }};
    var callee = F.artifact(2, "old site-named callee");
    callee.loc = .{ .file = 0, .line = 1, .column = 1 };
    try fixture.publish(&.{ root, callee }, &.{F.spec(1, 0)}, &.{@splat(7)});
    var packs = try fixture.loader();
    defer packs.deinit();
    _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    root.symbolic_refs = &.{};
    root.refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
    try fixture.publish(&.{ root, callee }, &.{F.spec(2, 0)}, &.{@splat(7)});
    try std.testing.expect(packs.specCacheLookup().lookup(@splat(2), .dev_code, &.{}) == null);
    try std.testing.expectEqual(@as(u64, 1), packs.index_stats.source_ineligible);
}

test "indexed object cache preserves linked precedence and rejects dev closures reaching linked procedures" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var root = F.artifact(1, "root");
    root.refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
    const dep = F.artifact(2, "dependency");
    const spec = F.spec(1, 0);
    try fixture.publish(&.{ root, dep }, &.{spec}, &.{});
    var packs = try fixture.loader();
    defer packs.deinit();
    packs.linked_state = .ready;
    try packs.linked_identities.put(F.allocator, dep.kind.proc, 0);
    try std.testing.expect(packs.specCacheLookup().lookup(spec.key, .dev_code_beside_linked_objects, &.{}) == null);
    try std.testing.expectEqual(@as(u64, 0), packs.index_stats.payload_opens);
    const dev = packs.specCacheLookup().lookup(spec.key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    var linked = dev;
    linked.c_abi_entry = true;
    try packs.linked_specs.put(spec.key, .{ .hit = linked, .pack = 0 });
    const hit = packs.specCacheLookup().lookup(spec.key, .dev_code_and_linked_objects, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expect(hit.c_abi_entry);
}

test "indexed object cache interrupted publication exposes no offer and concurrent writers lose none" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    // Simulate interruption after payload publication but before any offer.
    const unoffered = try Index.payload(F.allocator, F.artifact(1, "code"));
    defer F.allocator.free(unoffered);
    try fixture.store.native.write(fixture.store.indexedIdentity(.local, F.identity, Index.digest(unoffered), .payload), unoffered, fixture.store.native.now());
    var interrupted = try fixture.loader();
    defer interrupted.deinit();
    try std.testing.expect(interrupted.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) == null);
    try std.testing.expectEqual(@as(u64, 0), interrupted.index_stats.payload_opens);

    const Writer = struct {
        store: *const Store,
        key: u8,
        failed: bool = false,
        fn run(self: *@This()) void {
            for (0..8) |_| {
                self.store.writeIndexed(.local, F.identity, &.{ .arena = undefined, .excerpts = &.{F.artifact(self.key, "code")} }, &.{F.spec(self.key, 0)}) catch |err| switch (err) {
                    error.PackWriteFailed => continue, // Optional publication can lose catalog contention.
                    else => {
                        self.failed = true;
                        return;
                    },
                };
            }
        }
    };
    var a = Writer{ .store = &fixture.store, .key = 1 };
    var b = Writer{ .store = &fixture.store, .key = 2 };
    {
        const thread_a = try std.Thread.spawn(.{}, Writer.run, .{&a});
        defer thread_a.join();
        const thread_b = try std.Thread.spawn(.{}, Writer.run, .{&b});
        defer thread_b.join();
        // A concurrent reader sees a miss or a completely materialized hit.
        // Publication becomes visible after the explicit snapshot refresh below.
        for (0..8) |_| {
            if (interrupted.specCacheLookup().lookup(@splat(1), .dev_code, &.{})) |hit| {
                try std.testing.expectEqual(lir.ProcIdentity.forTest(1).bytes, hit.identity);
            }
        }
    }
    try std.testing.expect(!a.failed and !b.failed);
    for ([_]u8{ 1, 2 }) |key| try fixture.publish(&.{F.artifact(key, "code")}, &.{F.spec(key, 0)}, &.{});
    interrupted.refreshIndex();
    for ([_]u8{ 1, 2 }) |key| {
        const hit = interrupted.specCacheLookup().lookup(@splat(key), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
        try std.testing.expectEqual(lir.ProcIdentity.forTest(key).bytes, hit.identity);
    }
    try std.testing.expectEqual(@as(u64, 1), interrupted.index_stats.payload_opens);
}

test "indexed object cache rejects corrupt offers and shared metadata before reading payloads" {
    const F = IndexFixture;
    for ([_]bool{ false, true }) |shared_metadata| {
        var fixture: F = undefined;
        try fixture.init();
        defer fixture.deinit();
        const artifacts = [_]backend.dev.CodeExcerpt.Excerpt{F.artifact(1, "code")};
        const specs = [_]PackFile.SpecEntry{F.spec(1, 0)};
        try fixture.publish(&artifacts, &specs, &.{});
        const group = fixture.store.indexedIdentity(.local, if (shared_metadata) F.identity else @splat(1), @splat(0), if (shared_metadata) .metadata else .offer);
        const keys = try fixture.store.native.groupKeys(F.allocator, group);
        defer F.allocator.free(keys);
        var record = (try fixture.store.native.acquireKey(keys[0], fixture.store.native.now())).?;
        defer record.deinit();
        const corrupted = try record.read(F.allocator);
        defer F.allocator.free(corrupted);
        corrupted[0] ^= 0xff;
        try std.Io.Dir.cwd().writeFile(std.testing.io, .{ .sub_path = record.path, .data = corrupted });
        var packs = try fixture.loader();
        defer packs.deinit();
        try std.testing.expect(packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) == null);
        try std.testing.expectEqual(@as(u64, 1), packs.index_stats.corrupt);
        try std.testing.expectEqual(@as(u64, 0), packs.index_stats.payload_opens);
        try fixture.publish(&artifacts, &specs, &.{});
        _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    }
}

test "indexed object cache duplicate eligible closures select by content independent of publication order" {
    const F = IndexFixture;
    var selected: ?u32 = null;
    for ([_]bool{ false, true }) |reverse| {
        var fixture: F = undefined;
        try fixture.init();
        defer fixture.deinit();
        const callee = F.artifact(2, "callee");
        for (0..2) |i| {
            const swapped = (i == 1) != reverse;
            var caller = F.artifact(1, "caller");
            const refs = [_]backend.dev.CodeExcerpt.Reference{.{ .site = 0, .form = .call, .target = if (swapped) 0 else 1, .delta = 0 }};
            caller.refs = &refs;
            const artifacts = if (swapped) [_]backend.dev.CodeExcerpt.Excerpt{ callee, caller } else [_]backend.dev.CodeExcerpt.Excerpt{ caller, callee };
            try fixture.publish(&artifacts, &.{F.spec(1, if (swapped) 1 else 0)}, &.{});
        }
        var packs = try fixture.loader();
        defer packs.deinit();
        _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
        const located = packs.excerpts.get(@splat(1)).?;
        if (selected) |expected| try std.testing.expectEqual(expected, located.index) else selected = located.index;
        try std.testing.expectEqualStrings("caller", located.set.excerpts[located.index].code);
        const ref = located.set.excerpts[located.index].refs[0];
        try std.testing.expectEqualStrings("callee", located.set.excerpts[ref.target].code);
    }
}

test "indexed object cache defers discovery until a completed-output probe misses" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    try fixture.publish(&.{F.artifact(1, "code")}, &.{F.spec(1, 0)}, &.{});
    var packs = try fixture.loader();
    defer packs.deinit();
    packs.defer_code_lookup = true;
    try std.testing.expect(packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) == null);
    try std.testing.expectEqual(@as(usize, 0), LoadedPacks.findDiscovery(&packs, @splat(1)).len);
    try std.testing.expect(packs.discovery_state == .pending);
    try std.testing.expectEqual(@as(u64, 0), packs.index_stats.index_bytes);
    try std.testing.expectEqual(@as(u64, 0), packs.index_stats.payload_opens);
    packs.defer_code_lookup = false;
    _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 1), packs.index_stats.payload_opens);
}

test "indexed object cache misses probe keys once across many providers and refresh on publication" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var packs = try fixture.loader();
    defer packs.deinit();
    for (1..128) |i| try fixture.store.loadIdentity(std.testing.io, &packs, .local, @splat(@intCast(i)));
    for (0..100) |_| try std.testing.expect(packs.specCacheLookup().lookup(@splat(250), .dev_code, &.{}) == null);
    try std.testing.expectEqual(@as(u64, 1), packs.index_stats.bucket_opens);
    try std.testing.expectEqual(@as(u64, 99), packs.index_stats.negative_hits);
    try fixture.publish(&.{F.artifact(1, "new code")}, &.{F.spec(250, 0)}, &.{});
    // An existing provider's new offer invalidates the snapshot, even though
    // the provider set itself did not change.
    try packs.offerIndexed(&fixture.store, .local, F.identity);
    _ = packs.specCacheLookup().lookup(@splat(250), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 2), packs.index_stats.bucket_opens);
}

test "indexed object cache shares producer metadata and verifies immutable records before skipping writes" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var first = F.artifact(1, "caller");
    var second = F.artifact(2, "caller");
    first.refs = &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }};
    second.refs = first.refs;
    const artifacts = [_]backend.dev.CodeExcerpt.Excerpt{ first, second, F.artifact(3, "shared dependency") };
    const specs = [_]PackFile.SpecEntry{ F.spec(1, 0), F.spec(2, 1) };
    try fixture.publish(&artifacts, &specs, &.{});
    const group = fixture.store.indexedIdentity(.local, F.identity, @splat(0), .metadata);
    const keys = try fixture.store.native.groupKeys(F.allocator, group);
    defer F.allocator.free(keys);
    var before = (try fixture.store.native.acquireKey(keys[0], fixture.store.native.now())).?;
    defer before.deinit();
    try fixture.publish(&artifacts, &specs, &.{});
    var after = (try fixture.store.native.acquireKey(keys[0], fixture.store.native.now())).?;
    defer after.deinit();
    try std.testing.expectEqualStrings(before.path, after.path);
    var packs = try fixture.loader();
    defer packs.deinit();
    _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    _ = packs.specCacheLookup().lookup(@splat(2), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 1), packs.index_stats.metadata_opens);
    try std.testing.expectEqual(@as(u64, 2), packs.index_stats.payload_files);
    try std.testing.expectEqual(@as(u64, 3), packs.index_stats.decoded_artifacts);
    try std.testing.expectEqual(packs.excerpts.get(@splat(1)).?.set, packs.excerpts.get(@splat(2)).?.set);
}

test "indexed object cache routes a package offer to producer storage outside its visible providers" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    const provider: compile.BuildEnv.PackPlacement = .{ .origin = .pkg, .identity = @splat(99) };
    try fixture.store.writeIndexedForProviders(.local, F.identity, &.{ .arena = undefined, .excerpts = &.{F.artifact(1, "package code")} }, &.{F.spec(1, 0)}, &.{provider});
    var packs = LoadedPacks.init(F.allocator);
    defer packs.deinit();
    try fixture.store.loadIdentity(std.testing.io, &packs, provider.origin, provider.identity);
    try packs.indexPacks();
    _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualStrings("package code", packs.excerpts.get(@splat(1)).?.set.excerpts[0].code);
}

test "interface provider capability stays stable before store registration" {
    var packs = LoadedPacks.init(std.testing.allocator);
    defer packs.deinit();
    const before = packs.specCacheLookup();
    var fixture: IndexFixture = undefined;
    try fixture.init();
    defer fixture.deinit();
    packs.interface_store = &fixture.store;
    try std.testing.expect(std.meta.eql(before, packs.specCacheLookup()));
}

test "persistent interface families invalidate misses, share records, and reject corruption" {
    var fixture: IndexFixture = undefined;
    try fixture.init();
    defer fixture.deinit();
    const allocator = std.testing.allocator;
    const family: [32]u8 = @splat(42);
    const first: [32]u8 = @splat(1);
    const second: [32]u8 = @splat(2);
    var producer = LoadedPacks.init(allocator);
    defer producer.deinit();
    producer.interface_store = &fixture.store;
    try std.testing.expectEqual(null, try LoadedPacks.loadInterface(&producer, allocator, family, first));
    try LoadedPacks.recordInterface(&producer, family, first, "one");
    try LoadedPacks.recordInterface(&producer, family, second, "two");
    try producer.publishInterfaces();
    try std.testing.expectEqual(@as(usize, 1), producer.interface_families.count());
    try std.testing.expect(!producer.interface_families.get(family).?.dirty);
    try LoadedPacks.recordInterface(&producer, family, first, "one");
    try std.testing.expect(!producer.interface_families.get(family).?.dirty);
    var consumer = LoadedPacks.init(allocator);
    defer consumer.deinit();
    consumer.interface_store = &fixture.store;
    const one = (try LoadedPacks.loadInterface(&consumer, allocator, family, first)).?;
    defer allocator.free(one);
    try std.testing.expectEqualStrings("one", one);
    const two = (try LoadedPacks.loadInterface(&consumer, allocator, family, second)).?;
    defer allocator.free(two);
    try std.testing.expectEqualStrings("two", two);
    const third: [32]u8 = @splat(3);
    try std.testing.expectEqual(null, try LoadedPacks.loadInterface(&consumer, allocator, family, third));
    try LoadedPacks.recordInterface(&producer, family, third, "three");
    try producer.publishInterfaces();
    try std.testing.expectEqual(null, try LoadedPacks.loadInterface(&consumer, allocator, family, third));
    consumer.refreshIndex();
    const three = (try LoadedPacks.loadInterface(&consumer, allocator, family, third)).?;
    defer allocator.free(three);
    try std.testing.expectEqualStrings("three", three);
    try fixture.store.native.write(fixture.store.indexedIdentity(.local, family, @splat(0), .interface), "RIFACE01truncated", fixture.store.native.now());
    var corrupt = LoadedPacks.init(allocator);
    defer corrupt.deinit();
    corrupt.interface_store = &fixture.store;
    try std.testing.expectEqual(null, try LoadedPacks.loadInterface(&corrupt, allocator, family, first));
    try LoadedPacks.recordInterface(&corrupt, family, first, "repaired");
    try corrupt.publishInterfaces();
    var repaired = LoadedPacks.init(allocator);
    defer repaired.deinit();
    repaired.interface_store = &fixture.store;
    const value = (try LoadedPacks.loadInterface(&repaired, allocator, family, first)).?;
    defer allocator.free(value);
    try std.testing.expectEqualStrings("repaired", value);
}
