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
    /// Logical scope for procedure packs and discovery summaries.
    procedure_root: []u8,
    native: compile.artifacts.Store,
    /// One retention observation for this invocation-owned cache reader.
    read_timestamp: i64,
    variant: [32]u8,
    verbose: bool,
    publication_stats: ?*PublicationStats = null,
    /// Explicit native format/target/ABI domain stamped by Store.init.
    domain: [32]u8 = @splat(0),

    pub const PublicationStats = struct {
        offers_changed: u64 = 0,
        offers_reused: u64 = 0,
        payloads_encoded: u64 = 0,
        payload_bytes_encoded: u64 = 0,
        bytes_hashed: u64 = 0,
        metadata_encoded: u64 = 0,
        metadata_bytes_encoded: u64 = 0,
        records_read: u64 = 0,
        bytes_read: u64 = 0,
        records_written: u64 = 0,
        bytes_written: u64 = 0,
        validated_references: u64 = 0,
        graph_nodes: u64 = 0,
        graph_edges: u64 = 0,
        closure_queries: u64 = 0,
        peak_encoded_bytes: u64 = 0,
    };

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
        const native = try compile.artifacts.Store.init(allocator, config.roc_ctx.std_io, cache_root);
        return .{ .allocator = allocator, .roc_ctx = config.roc_ctx, .root = root, .procedure_root = procedure_root, .variant = compile.artifacts.hashBytes(opt_name), .native = native, .read_timestamp = native.now(), .verbose = config.verbose, .domain = backend.dev.NativeProcDomain.init(target, strategy).key() };
    }

    pub fn deinit(self: *Store) void {
        self.native.deinit();
        self.allocator.free(self.root);
        self.allocator.free(self.procedure_root);
    }

    fn artifactIdentity(self: *const Store, family: compile.artifacts.Family, module: [32]u8, key: [32]u8) compile.artifacts.Identity {
        var result: compile.artifacts.Identity = .{ .namespace = undefined, .family = family, .group = undefined, .key = key, .pack_format = PackFile.format_version, .variant = self.variant };
        base.Sha256.hash(@import("build_options").compiler_compatibility_id, &result.namespace, .{});
        var hash = base.Sha256.init(.{});
        hash.update(switch (family) {
            .local_pack, .package_pack, .local_discovery, .package_discovery => self.procedure_root,
            .checked, .canonicalized, .placement, .literals, .pairing, .executable, .test_result, .wasm_host, .glue_plugin, .optimized_pack, .program_output, .scratch => self.root,
        });
        hash.update(&module);
        hash.final(&result.group);
        return result;
    }

    const IndexedKind = enum { payload, metadata, offer, offer_domain, completed, interface };

    const offer_domain_bytes = "ROCNOFF1";

    fn offerDomainIdentity(self: *const Store, origin: compile.BuildEnv.PackOrigin) compile.artifacts.Identity {
        return self.indexedIdentity(origin, @splat(0), Index.digest(offer_domain_bytes), .offer_domain);
    }

    fn indexedIdentity(self: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8, key: [32]u8, kind: IndexedKind) compile.artifacts.Identity {
        var id = self.packIdentity(origin, module, key);
        var hash = base.Sha256.init(.{});
        hash.update(&id.variant);
        hash.update("roc-indexed-procedure-v1");
        hash.update(if (kind == .interface) "interface-request-v3" else @tagName(kind));
        hash.final(&id.variant);
        return id;
    }

    pub fn linkObjectIdentity(self: *const Store, digest: [32]u8) compile.artifacts.Identity {
        var id = self.artifactIdentity(.program_output, @splat(0), digest);
        var hash = base.Sha256.init(.{});
        hash.update(&id.variant);
        hash.update("roc-native-link-object-v1");
        hash.final(&id.variant);
        return id;
    }

    fn readPrefix(self: *const Store, id: compile.artifacts.Identity, len: usize, allocator: Allocator) Allocator.Error!?[]u8 {
        var artifact = (self.native.acquire(id, self.read_timestamp) catch |err| switch (err) {
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
        return self.readPrefix(self.artifactIdentity(.program_output, @splat(0), key), len, allocator);
    }

    fn packIdentity(self: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8, key: [32]u8) compile.artifacts.Identity {
        return self.artifactIdentity(if (origin == .local) .local_pack else .package_pack, module, key);
    }

    fn read(self: *const Store, allocator: Allocator, id: compile.artifacts.Identity) Allocator.Error!?[]u8 {
        return self.native.read(allocator, id, self.read_timestamp) catch |err| switch (err) {
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
        base.Sha256.hash(bytes, &key, .{});
        try self.writeEntry(self.artifactIdentity(if (origin == .local) .local_discovery else .package_discovery, module, key), bytes);
    }

    pub fn readProgram(self: *const Store, key: [32]u8, allocator: Allocator) Allocator.Error!?[]u8 {
        return self.read(allocator, self.artifactIdentity(.program_output, @splat(0), key));
    }

    pub fn writeProgram(self: *const Store, key: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        try self.writeEntry(self.artifactIdentity(.program_output, @splat(0), key), bytes);
    }

    pub fn optimizedInputKey(self: *const Store, module: [32]u8) Allocator.Error!?[32]u8 {
        const bytes = (try self.readPrefix(self.artifactIdentity(.optimized_pack, module, @splat(0)), PackFile.object_input_key_prefix_len, self.allocator)) orelse return null;
        defer self.allocator.free(bytes);
        return PackFile.objectInputKey(bytes);
    }

    pub fn replaceOptimized(self: *const Store, module: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        try self.writeEntry(self.artifactIdentity(.optimized_pack, module, @splat(0)), bytes);
    }

    pub fn has(self: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8, key: [32]u8) Allocator.Error!bool {
        var artifact = (self.native.acquire(self.packIdentity(origin, module, key), self.read_timestamp) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return false,
        }) orelse return false;
        defer artifact.deinit();
        return true;
    }

    pub fn write(self: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8, key: [32]u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!void {
        try self.writeEntry(self.packIdentity(origin, module, key), bytes);
    }

    pub fn loadIdentity(self: *const Store, _: std.Io, packs: *LoadedPacks, origin: compile.BuildEnv.PackOrigin, module: [32]u8) Allocator.Error!void {
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
        return self.writeIndexedRetained(origin, identity, set, specs, providers, null);
    }

    /// Reuse exact offers already admitted and retained by this compilation.
    pub fn writeIndexedRetained(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, set: *const backend.dev.CodeExcerpt.Set, specs: []const PackFile.SpecEntry, providers: ?[]const compile.BuildEnv.PackPlacement, retained: ?*const LoadedPacks) (Allocator.Error || error{PackWriteFailed})!void {
        var local_stats: PublicationStats = .{};
        const stats = self.publication_stats orelse &local_stats;
        const tracing = base.dev_diagnostics.isSet(.pack_stats);
        const start = if (tracing) std.Io.Timestamp.now(self.roc_ctx.std_io, .awake).nanoseconds else 0;
        defer if (tracing) std.debug.print("pack publication: {any} wall_ns={d}\n", .{ stats.*, std.Io.Timestamp.now(self.roc_ctx.std_io, .awake).nanoseconds - start });
        if (providers) |locations| std.debug.assert(locations.len == specs.len);
        if (specs.len == 0) return;
        const allocator = self.allocator;
        var closure = try SpliceClosure.init(allocator, set);
        defer closure.deinit();
        var graph = try Index.ComponentGraph.init(allocator, &closure);
        defer graph.deinit();
        stats.graph_nodes += set.excerpts.len;
        stats.graph_edges += graph.edge_count;
        const union_indices = try closure.ofEntries(specs);
        stats.graph_nodes += closure.nodes_visited;
        stats.graph_edges += closure.edges_visited;
        stats.closure_queries += closure.queries;
        const included = try allocator.alloc(bool, graph.components.count());
        defer allocator.free(included);
        @memset(included, false);
        const payload_hashes = try allocator.alloc([32]u8, set.excerpts.len);
        defer allocator.free(payload_hashes);
        const component_hashes = try allocator.alloc([32]u8, graph.components.count());
        defer allocator.free(component_hashes);
        var pins = std.ArrayList(compile.artifacts.Artifact).empty;
        defer {
            for (pins.items) |*artifact| artifact.deinit();
            pins.deinit(allocator);
        }
        // Advertise before publishing any offer, and retain that advertisement
        // throughout publication. Concurrent producers write identical bytes.
        var advertised: [@typeInfo(compile.BuildEnv.PackOrigin).@"enum".field_names.len]bool = @splat(false);
        for (specs, 0..) |_, i| {
            const provider_origin = if (providers) |locations| locations[i].origin else origin;
            const slot = @backingInt(provider_origin);
            if (advertised[slot]) continue;
            if (retained == null or !retained.?.retainsOfferAdvertisement(self, provider_origin)) {
                _ = try self.publishIndexedHeld(self.offerDomainIdentity(provider_origin), offer_domain_bytes, stats, &pins);
            }
            advertised[slot] = true;
        }
        var published = std.AutoHashMap([32]u8, void).init(allocator);
        defer published.deinit();
        for (union_indices) |index| {
            included[graph.components.component_of[index]] = true;
            const artifact = set.excerpts[index];
            if (artifact.payload_record) |record| {
                std.debug.assert(record.kind == .native_payload_v1);
                if (!std.mem.eql(u8, &record.owner.domain, &self.domain)) return error.PackWriteFailed;
                payload_hashes[index] = record.digest;
                if ((try published.getOrPut(record.digest)).found_existing) continue;
                const id = self.indexedIdentity(.local, @splat(0), record.digest, .payload);
                if (std.mem.eql(u8, &record.storage_identity, &self.retainedIdentity(id.indexedKey()))) stats.validated_references += 1 else _ = try self.publishIndexedHeld(id, record.bytes, stats, &pins);
                continue;
            }
            const bytes = try Index.payload(allocator, artifact);
            defer allocator.free(bytes);
            stats.payloads_encoded += 1;
            stats.payload_bytes_encoded += bytes.len;
            stats.bytes_hashed += bytes.len;
            stats.peak_encoded_bytes = @max(stats.peak_encoded_bytes, bytes.len);
            const hash = Index.digest(bytes);
            payload_hashes[index] = hash;
            if ((try published.getOrPut(hash)).found_existing) continue;
            const id = self.indexedIdentity(.local, @splat(0), hash, .payload);
            _ = try self.publishIndexedHeld(id, bytes, stats, &pins);
        }
        var names: ?backend.dev.CodeExcerpt.ContentNames = null;
        defer if (names) |*value| value.deinit();
        var remaining = graph.components.count();
        while (remaining != 0) {
            remaining -= 1;
            if (!included[remaining]) continue;
            const c: u32 = @intCast(remaining);
            if (Index.reusableComponent(set, &graph, &closure, c, component_hashes, payload_hashes)) |record| {
                component_hashes[c] = record.digest;
                const id = self.indexedIdentity(.local, @splat(0), record.digest, .metadata);
                if (std.mem.eql(u8, &record.storage_identity, &self.retainedIdentity(id.indexedKey()))) stats.validated_references += 1 else _ = try self.publishIndexedHeld(id, record.bytes, stats, &pins);
                continue;
            }
            if (names == null) names = try backend.dev.CodeExcerpt.ContentNames.init(allocator, set);
            const bytes = try Index.encodeComponent(allocator, set, &graph, &closure, c, component_hashes, payload_hashes, &names.?);
            defer allocator.free(bytes);
            stats.metadata_encoded += 1;
            stats.metadata_bytes_encoded += bytes.len;
            stats.bytes_hashed += bytes.len;
            stats.peak_encoded_bytes = @max(stats.peak_encoded_bytes, bytes.len);
            const hash = Index.digest(bytes);
            component_hashes[c] = hash;
            const id = self.indexedIdentity(.local, @splat(0), hash, .metadata);
            _ = try self.publishIndexedHeld(id, bytes, stats, &pins);
        }
        for (specs, 0..) |spec, i| {
            const provider = if (providers) |locations| locations[i] else compile.BuildEnv.PackPlacement{ .origin = origin, .identity = identity };
            const hash = component_hashes[graph.components.component_of[spec.excerpt]];
            const ordinal = graph.ordinal[spec.excerpt];
            if (retained) |loaded| if (loaded.reusesOffer(self, spec, provider, hash, ordinal)) {
                stats.offers_reused += 1;
                stats.validated_references += 1;
                continue;
            };
            stats.offers_changed += 1;
            const bytes = try Index.encodeOffer(allocator, hash, ordinal, set.excerpts[spec.excerpt].kind.proc, spec);
            defer allocator.free(bytes);
            stats.bytes_hashed += bytes.len;
            const record = try allocator.alloc(u8, 32 + bytes.len);
            defer allocator.free(record);
            @memcpy(record[0..32], &provider.identity);
            @memcpy(record[32..], bytes);
            const id = self.indexedIdentity(provider.origin, spec.key, Index.digest(record), .offer);
            _ = try self.publishIndexedHeld(id, record, stats, &pins);
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
        try self.loadGroup(packs, self.artifactIdentity(if (origin == .local) .local_discovery else .package_discovery, module, @splat(0)), .discovery);
    }

    fn retainedIdentity(self: *const Store, key: [64]u8) [32]u8 {
        var hash = base.Sha256.init(.{});
        hash.update(self.native.root);
        hash.update(&key);
        return hash.finalResult();
    }

    const RetainedArtifact = struct {
        allocator: Allocator,
        artifact: compile.artifacts.Artifact,
        // Artifact.deinit only needs this owned io/allocator, not a live Store.
        store: compile.artifacts.Store,
        fn release(context: *anyopaque) void {
            const self: *@This() = @ptrCast(@alignCast(context));
            self.artifact.deinit();
            self.allocator.destroy(self);
        }
    };

    fn acquireRetained(self: *const Store, owner: *base.retained_cache.Owner, batch: *compile.artifacts.RetentionBatch, key: [64]u8, kind: base.retained_cache.Kind) (Allocator.Error || error{ Unavailable, Corrupt })!*const base.retained_cache.Record {
        const acquired = (batch.acquireKey(key, self.read_timestamp) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.Unavailable,
        }) orelse return error.Unavailable;
        return self.retainAcquired(owner, acquired, key, kind);
    }

    /// Consumes an acquisition on success or failure, preserving its exact
    /// generation lease until the validated record owner releases it.
    fn retainAcquired(self: *const Store, owner: *base.retained_cache.Owner, value: compile.artifacts.Artifact, key: [64]u8, kind: base.retained_cache.Kind) (Allocator.Error || error{ Unavailable, Corrupt })!*const base.retained_cache.Record {
        var acquired = value;
        errdefer acquired.deinit();
        const bytes = acquired.read(owner.allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.Unavailable,
        };
        errdefer owner.allocator.free(bytes);
        const held = try owner.allocator.create(RetainedArtifact);
        errdefer owner.allocator.destroy(held);
        held.* = .{ .allocator = owner.allocator, .artifact = acquired, .store = self.native };
        held.artifact.store = &held.store;
        return owner.take(bytes, key[32..].*, kind, self.retainedIdentity(key), .{ .context = held, .deinit = RetainedArtifact.release });
    }

    fn publishIndexed(self: *const Store, id: compile.artifacts.Identity, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!bool {
        var stats: PublicationStats = .{};
        return self.publishIndexedHeld(id, bytes, self.publication_stats orelse &stats, null);
    }

    fn publishIndexedHeld(self: *const Store, id: compile.artifacts.Identity, bytes: []const u8, stats: *PublicationStats, pins: ?*std.ArrayList(compile.artifacts.Artifact)) (Allocator.Error || error{PackWriteFailed})!bool {
        if (pins) |held| try held.ensureUnusedCapacity(self.allocator, 1);
        stats.records_read += 1;
        if (self.native.acquire(id, self.read_timestamp) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.PackWriteFailed,
        }) |value| {
            var acquired = value;
            var transferred = false;
            defer if (!transferred) acquired.deinit();
            const existing = acquired.read(self.allocator) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return error.PackWriteFailed,
            };
            defer self.allocator.free(existing);
            stats.bytes_read += existing.len;
            if (std.mem.eql(u8, existing, bytes)) {
                if (pins) |held| {
                    held.appendAssumeCapacity(acquired);
                    transferred = true;
                }
                return false;
            }
        }
        self.native.write(id, bytes, self.native.now()) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.PackWriteFailed,
        };
        if (pins) |held| {
            var acquired = (self.native.acquire(id, self.read_timestamp) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return error.PackWriteFailed,
            }) orelse return error.PackWriteFailed;
            errdefer acquired.deinit();
            // Acquisition may race another publisher; validate the generation
            // actually acquired before publishing references to its identity.
            const actual = acquired.read(self.allocator) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return error.PackWriteFailed,
            };
            defer self.allocator.free(actual);
            stats.records_read += 1;
            stats.bytes_read += actual.len;
            if (!std.mem.eql(u8, actual, bytes)) return error.PackWriteFailed;
            held.appendAssumeCapacity(acquired);
        }
        stats.records_written += 1;
        stats.bytes_written += bytes.len;
        return true;
    }

    fn loadGroup(self: *const Store, packs: *LoadedPacks, id: compile.artifacts.Identity, files: LoadedPacks.Files) LoadedPacks.LoadError!void {
        const keys = self.native.groupKeys(self.allocator, id) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return,
        };
        defer self.allocator.free(keys);
        for (keys) |key| {
            var artifact = (self.native.acquireKey(key, self.read_timestamp) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => continue,
            }) orelse continue;
            defer artifact.deinit();
            var arena = base.SingleThreadArena.init(self.allocator);
            const bytes = artifact.read(arena.allocator()) catch |err| {
                arena.deinit();
                switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    else => continue,
                }
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
        .iterator_fusion_scope = true,
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
            // This fault-injection boundary covers loading and atomic indexing.
            // Admission can allocate separately and decline an offer on OOM.
            const hit = packs.specs.get(expected_key) orelse return error.TestUnexpectedResult;
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
        var admitted = LoadedPacks.init(allocator);
        defer admitted.deinit();
        try admitted.loadDirInto(io, dir_path, .cache_offers);
        try admitted.indexPacks();
        const hit = admitted.specCacheLookup().lookup(key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
        try std.testing.expectEqualSlices(u8, &identity.bytes, &hit.identity);
        try std.testing.expect(hit.iterator_fusion_scope);
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

// repro for https://github.com/roc-lang/roc/issues/12626
test "a pack evicted between listing its directory and reading it is a miss, not an unreadable cache" {
    // Another `roc`'s cache sweep deletes the first pack after the listing
    // names it and before the loader opens it.
    const Eviction = struct {
        // Every hooked call receives the testing Io's own context, so the
        // hook's count lives outside it.
        var deleted: usize = 0;

        fn openFile(context: ?*anyopaque, dir: std.Io.Dir, path: []const u8, options: std.Io.Dir.OpenFileOptions) std.Io.File.OpenError!std.Io.File {
            if (deleted == 0 and std.mem.endsWith(u8, path, "a-evicted.rpk")) {
                std.testing.io.vtable.dirDeleteFile(context, dir, path) catch unreachable; // the fixture wrote this pack and only this hook deletes it, once
                deleted += 1;
            }
            return std.testing.io.vtable.dirOpenFile(context, dir, path, options);
        }

        fn packBytes(allocator: Allocator, key: [32]u8, identity: lir.ProcIdentity) Allocator.Error![]u8 {
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
            return PackFile.write(allocator, &set, &.{.{
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
        }
    };
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const evicted_key = @as([32]u8, @splat(1));
    const kept_key = @as([32]u8, @splat(2));
    const offered_key = @as([32]u8, @splat(3));
    const kept_identity = lir.ProcIdentity.forTest(2);
    const offered_identity = lir.ProcIdentity.forTest(3);
    const evicted_bytes = try Eviction.packBytes(allocator, evicted_key, lir.ProcIdentity.forTest(1));
    defer allocator.free(evicted_bytes);
    const kept_bytes = try Eviction.packBytes(allocator, kept_key, kept_identity);
    defer allocator.free(kept_bytes);
    const offered_bytes = try Eviction.packBytes(allocator, offered_key, offered_identity);
    defer allocator.free(offered_bytes);
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "a-evicted.rpk", .data = evicted_bytes });
    try tmp.dir.writeFile(std.testing.io, .{ .sub_path = "b-kept.rpk", .data = kept_bytes });
    const dir_path = try tmp.dir.realPathFileAlloc(std.testing.io, ".", allocator);
    defer allocator.free(dir_path);

    Eviction.deleted = 0;
    var vtable = std.testing.io.vtable.*;
    vtable.dirOpenFile = Eviction.openFile;
    const io: std.Io = .{ .userdata = std.testing.io.userdata, .vtable = &vtable };

    var packs = LoadedPacks.init(allocator);
    defer packs.deinit();
    try packs.loadDirInto(io, dir_path, .cache_offers);
    try std.testing.expectEqual(@as(usize, 1), Eviction.deleted);
    try std.testing.expect(packs.failure == null);
    try std.testing.expectEqual(@as(usize, 1), packs.packs.items.len);
    try packs.indexPacks();
    try std.testing.expect(packs.specCacheLookup().lookup(evicted_key, .dev_code, &.{}) == null);
    const kept = packs.specCacheLookup().lookup(kept_key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualSlices(u8, &kept_identity.bytes, &kept.identity);
    // The build goes on to offer the packs it compiles for the miss.
    try packs.offerPack(offered_bytes);
    const offered = packs.specCacheLookup().lookup(offered_key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualSlices(u8, &offered_identity.bytes, &offered.identity);
}

test "an offer to an unavailable object cache is dropped without failing the build" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "broken.rpk", .data = "not a pack" });
    const dir_path = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(dir_path);
    var packs = LoadedPacks.init(allocator);
    defer packs.deinit();
    try std.testing.expectError(error.MalformedPack, packs.loadDirInto(io, dir_path, .cache_offers));
    const key = @as([32]u8, @splat(7));
    const set = backend.dev.CodeExcerpt.Set{
        .arena = @import("base").SingleThreadArena.init(allocator),
        .excerpts = &.{.{
            .kind = .{ .proc = lir.ProcIdentity.forTest(7) },
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
    try std.testing.expectEqual(@as(usize, 0), packs.packs.items.len);
    try std.testing.expect(packs.specCacheLookup().lookup(key, .dev_code, &.{}) == null);
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
        const left_key = left.artifactIdentity(.optimized_pack, identity, identity).indexedKey();
        const right_key = right.artifactIdentity(.optimized_pack, identity, identity).indexedKey();
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
        var hasher = std.crypto.hash.Blake3.init(.{});
        hasher.update("roc.optimization-state.v1");
        for (&self.stores) |*store| {
            for (selection.modules) |module| {
                if (!try store.has(.pkg, module.identity, module.key)) continue;
                hasher.update(&store.packIdentity(.pkg, module.identity, module.key).indexedKey());
            }
            const identity = selection.optimized_identity orelse continue;
            const input_key = try store.optimizedInputKey(identity) orelse continue;
            hasher.update(&store.artifactIdentity(.optimized_pack, identity, @splat(0)).indexedKey());
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
/// Exclusive phases within indexed native lookup. This diagnostic is owned by
/// the coordinator; interface workers use a different cache entry point.
const NativeLookupProfile = struct {
    const Phase = enum { buckets, offers, decode, metadata, graph, eligibility, payloads, install };
    ns: [8]i128 = @splat(0),
    const Scope = struct {
        profile: ?*NativeLookupProfile = null,
        phase: Phase = .buckets,
        started: i128 = 0,
        fn now() i128 {
            return std.Io.Timestamp.now(std.Io.Threaded.global_single_threaded.io(), .awake).nanoseconds;
        }
        fn begin(profile: *NativeLookupProfile) Scope {
            if (!base.dev_diagnostics.isSet(.spec_resolution_profile)) return .{};
            return .{ .profile = profile, .started = now() };
        }
        fn enter(self: *Scope, phase: Phase) void {
            const profile = self.profile orelse return;
            const current = now();
            profile.ns[@backingInt(self.phase)] += current - self.started;
            self.started = current;
            self.phase = phase;
        }
        fn end(self: *Scope) void {
            self.enter(self.phase);
            self.profile = null;
        }
    };
    fn report(self: *const NativeLookupProfile) void {
        if (!base.dev_diagnostics.isSet(.spec_resolution_profile)) return;
        inline for (@typeInfo(Phase).@"enum".field_names, 0..) |name, i| {
            if (self.ns[i] != 0) std.debug.print("native-lookup-profile phase={s} ns={d}\n", .{ name, self.ns[i] });
        }
    }
};

/// The native packs a build has acquired, with the offers they link.
pub const LoadedPacks = struct {
    const LinkedOffer = struct {
        hit: Common.SpecCacheHit,
        pack: u32,
    };

    const IndexedMetadata = struct {
        record: *const base.retained_cache.Record,
        pack: *PackFile.Pack,
        hashes: []const [32]u8,
        edges: []const Index.Edge,
        start: ?u32 = null,
        materialized: bool = false,
    };
    const MissKey = struct {
        key: [32]u8,
        takes: Common.SpecCacheTakes,
        modules: [32]u8,
    };
    const IndexedResult = enum { hit, miss, unavailable };

    /// A request owns its payload and its synchronization. Independent requests
    /// can read and validate their files concurrently, including in one module.
    const InterfaceRecord = struct {
        mutex: std.Io.Mutex = .init,
        payload: ?[]const u8 = null,
        loaded: bool = false,
        dirty: bool = false,
    };
    const InterfaceFamily = struct {
        records: std.AutoArrayHashMapUnmanaged([32]u8, *InterfaceRecord) = .empty,
    };
    const Provider = struct { store: *const Store, origin: compile.BuildEnv.PackOrigin, module: [32]u8 };
    const IndexRoot = struct { store: *const Store, origin: compile.BuildEnv.PackOrigin, advertised: ?bool = null, advertisement: ?*const base.retained_cache.Record = null };
    interface_mutex: std.Io.Mutex = .init,
    /// Worker reads update these atomically; reported after joining.
    interface_bytes_loaded: u64 = 0,
    interface_invalid_records: u64 = 0,
    interface_families: std.AutoHashMapUnmanaged([32]u8, *InterfaceFamily) = .empty,
    interface_store: ?*const Store = null,
    allocator: Allocator,
    index_dirs: std.ArrayList(Provider) = .empty,
    index_roots: std.ArrayList(IndexRoot) = .empty,
    provider_ranks: std.AutoHashMapUnmanaged([32]u8, u32) = .empty,
    metadata: std.AutoHashMapUnmanaged([32]u8, *IndexedMetadata) = .empty,
    indexed_pack: ?*PackFile.Pack = null,
    indexed_artifacts: std.ArrayList(backend.dev.CodeExcerpt.Excerpt) = .empty,
    indexed_files: std.ArrayList([32]u8) = .empty,
    indexed_file_ids: std.AutoHashMapUnmanaged([32]u8, u32) = .empty,
    negative: std.AutoHashMapUnmanaged(MissKey, void) = .empty,

    record_owner: ?*base.retained_cache.Owner = null,
    retention_batches: std.StringHashMapUnmanaged(*compile.artifacts.RetentionBatch) = .empty,
    record_domain: [32]u8 = @splat(0),
    payloads: std.AutoHashMapUnmanaged([32]u8, *const base.retained_cache.Record) = .empty,
    retained_offers: std.AutoHashMapUnmanaged([32]u8, *const base.retained_cache.Record) = .empty,

    index_stats: IndexStats = .{},
    lookup_profile: NativeLookupProfile = .{},
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
    dev_reaches_linked: std.AutoHashMapUnmanaged(*const backend.dev.CodeExcerpt.Set, std.ArrayList(bool)) = .empty,
    /// Per dev pack and asking program, whether a splice of each artifact
    /// would place a line or a location in a module version the program does
    /// not lower against (`linesOutsideProgram`).
    lines_outside: std.AutoHashMapUnmanaged(ProgramPack, SourceEligibility) = .empty,

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

    const SourceEligibility = struct {
        present_files: std.ArrayList(bool) = .empty,
        marks: std.ArrayList(bool) = .empty,

        fn deinit(self: *SourceEligibility, allocator: Allocator) void {
            self.present_files.deinit(allocator);
            self.marks.deinit(allocator);
        }
    };

    fn recordOwner(self: *LoadedPacks, io: std.Io) Allocator.Error!*base.retained_cache.Owner {
        if (self.record_owner) |owner| return owner;
        const owner = try base.retained_cache.Owner.create(self.allocator, io, self.record_domain);
        self.record_owner = owner;
        return owner;
    }

    fn retentionBatch(self: *LoadedPacks, store: *const Store) Allocator.Error!*compile.artifacts.RetentionBatch {
        if (self.retention_batches.get(store.native.root)) |batch| return batch;
        const owner = try self.recordOwner(store.roc_ctx.std_io);
        const batch = try store.allocator.create(compile.artifacts.RetentionBatch);
        errdefer store.allocator.destroy(batch);
        batch.* = try compile.artifacts.RetentionBatch.init(&store.native);
        errdefer batch.deinit();
        try self.retention_batches.ensureUnusedCapacity(self.allocator, 1);
        try owner.adopt(.{ .context = batch, .deinit = releaseRetentionBatch });
        self.retention_batches.putAssumeCapacity(batch.store.root, batch);
        return batch;
    }

    fn releaseRetentionBatch(context: *anyopaque) void {
        const batch: *compile.artifacts.RetentionBatch = @ptrCast(@alignCast(context));
        const allocator = batch.store.allocator;
        batch.deinit();
        allocator.destroy(batch);
    }

    fn reusesOffer(self: *const LoadedPacks, store: *const Store, spec: PackFile.SpecEntry, provider: compile.BuildEnv.PackPlacement, component: [32]u8, ordinal: u32) bool {
        const offer = self.retained_offers.get(spec.key) orelse return false;
        const old = self.excerpts.get(spec.key) orelse return false;
        const origin = old.set.excerpts[old.index].metadata_origin orelse return false;
        if (!std.mem.eql(u8, &origin.record.owner.domain, &store.domain) or !std.mem.eql(u8, &origin.record.digest, &component) or origin.index != ordinal) return false;
        const hit = self.specs.get(spec.key) orelse return false;
        if (hit.iterator_fusion_scope != spec.iterator_fusion_scope or hit.may_leave_pending != spec.may_leave_pending or hit.rc_borrowed_params != spec.rc_borrowed_params or hit.rc_ret_borrowed != spec.rc_ret_borrowed or
            hit.rc_ret_lenders != spec.rc_ret_lenders or hit.rc_read_only_params != spec.rc_read_only_params or
            hit.rc_ret_unique != spec.rc_ret_unique or hit.rc_ret_unique_fields != spec.rc_ret_unique_fields or
            !std.mem.eql(u32, hit.rc_ret_conditions, spec.rc_ret_conditions) or hit.comptime_root_reads.len != spec.comptime_root_reads.len) return false;
        for (hit.comptime_root_reads, spec.comptime_root_reads) |a, b| if (!std.meta.eql(a, b)) return false;
        if (offer.bytes.len < 32 or !std.mem.eql(u8, offer.bytes[0..32], &provider.identity)) return false;
        const id = store.indexedIdentity(provider.origin, spec.key, offer.digest, .offer);
        return std.mem.eql(u8, &offer.storage_identity, &store.retainedIdentity(id.indexedKey()));
    }

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
        self.record_domain = store.domain;
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

    fn loadOfferAdvertisement(self: *LoadedPacks, root: *IndexRoot) Allocator.Error!bool {
        const store = root.store;
        const owner = try self.recordOwner(store.roc_ctx.std_io);
        const record = store.acquireRetained(owner, try self.retentionBatch(store), store.offerDomainIdentity(root.origin).indexedKey(), .native_offer_domain_v1) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.Unavailable, error.Corrupt => return false,
        };
        if (!std.mem.eql(u8, record.bytes, Store.offer_domain_bytes)) return false;
        root.advertisement = record;
        return true;
    }

    fn retainsOfferAdvertisement(self: *const LoadedPacks, store: *const Store, origin: compile.BuildEnv.PackOrigin) bool {
        const storage = store.retainedIdentity(store.offerDomainIdentity(origin).indexedKey());
        for (self.index_roots.items) |root| if (root.advertisement) |record| {
            if (std.mem.eql(u8, &record.storage_identity, &storage)) return true;
        };
        return false;
    }

    /// Start a fresh optional-cache snapshot after publication. This also
    /// invalidates misses when an existing provider acquired new offers.
    /// Other processes' later publications become visible at the next refresh
    /// or in the next compilation; a miss never promises code availability.
    pub fn refreshIndex(self: *LoadedPacks) void {
        self.negative.clearRetainingCapacity();
        for (self.index_roots.items) |*root| if (root.advertisement == null) {
            root.advertised = null;
        };
        var families = self.interface_families.valueIterator();
        while (families.next()) |family| {
            for (family.*.records.values()) |record| if (record.payload == null) {
                record.loaded = false;
            };
        }
    }

    /// Register freshly published offers in the checked program's provider order.
    pub fn offerIndexed(self: *LoadedPacks, store: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) Allocator.Error!void {
        self.ensureLoaded();
        try store.loadIdentity(store.roc_ctx.std_io, self, origin, identity);
        self.refreshIndex();
    }

    fn releaseRecordPack(context: *anyopaque) void {
        const pack: *PackFile.Pack = @ptrCast(@alignCast(context));
        const a = pack.set.arena.child_allocator;
        pack.deinit();
        a.destroy(pack);
    }

    fn adoptPack(self: *LoadedPacks, value: PackFile.Pack) Allocator.Error!*PackFile.Pack {
        const pack = try self.allocator.create(PackFile.Pack);
        errdefer self.allocator.destroy(pack);
        pack.* = value;
        try self.record_owner.?.adopt(.{ .context = pack, .deinit = releaseRecordPack });
        return pack;
    }

    fn loadMetadata(self: *LoadedPacks, store: *const Store, hash: [32]u8) Allocator.Error!?*IndexedMetadata {
        if (self.metadata.get(hash)) |existing| return existing;
        self.index_stats.metadata_opens += 1;
        const owner = try self.recordOwner(store.roc_ctx.std_io);
        const record = store.acquireRetained(owner, try self.retentionBatch(store), store.indexedIdentity(.local, @splat(0), hash, .metadata).indexedKey(), .native_component_v1) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.Corrupt => {
                self.index_stats.corrupt += 1;
                return null;
            },
            error.Unavailable => {
                self.index_stats.missing += 1;
                return null;
            },
        };
        return self.adoptMetadata(owner, hash, record);
    }

    fn adoptMetadata(self: *LoadedPacks, owner: *base.retained_cache.Owner, hash: [32]u8, record: *const base.retained_cache.Record) Allocator.Error!?*IndexedMetadata {
        const allocator = self.allocator;
        self.bytes_loaded +|= record.bytes.len;
        self.index_stats.index_bytes += record.bytes.len;
        self.index_stats.metadata_bytes += record.bytes.len;
        var decoded = Index.decodeComponent(allocator, record.bytes) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                self.index_stats.corrupt += 1;
                return null;
            },
        };
        var adopted = false;
        defer if (!adopted) decoded.pack.deinit();
        try self.metadata.ensureUnusedCapacity(allocator, 1);
        const entry = try allocator.create(IndexedMetadata);
        errdefer allocator.destroy(entry);
        const pack = try allocator.create(PackFile.Pack);
        errdefer allocator.destroy(pack);
        pack.* = decoded.pack;
        // Allocate through the original arena until ownership transfers.
        const a = decoded.pack.set.arena.allocator();
        const PA = backend.dev.CodeExcerpt;
        const origins = try a.alloc(PA.MetadataOrigin, decoded.pack.set.excerpts.len);
        for (decoded.pack.set.excerpts, origins, 0..) |artifact, *origin, i| {
            const refs = try a.alloc(PA.Binding, artifact.refs.len);
            const symbolic = try a.alloc(?PA.Binding, artifact.symbolic_refs.len);
            @memset(symbolic, null);
            origin.* = .{ .record = record, .source = &pack.set, .index = @intCast(i), .refs = refs, .symbolic = symbolic };
        }
        for (decoded.edges) |edge| {
            const target = PA.Binding{ .component = edge.target.component orelse hash, .index = edge.target.index };
            const origin = &origins[edge.excerpt];
            if (edge.symbolic) @constCast(origin.symbolic)[edge.reference] = target else @constCast(origin.refs)[edge.reference] = target;
        }
        for (@constCast(decoded.pack.set.excerpts), origins) |*artifact, *origin| artifact.metadata_origin = origin;
        pack.* = decoded.pack;
        try owner.adopt(.{ .context = pack, .deinit = releaseRecordPack });
        adopted = true;
        entry.* = .{ .record = record, .pack = pack, .hashes = decoded.hashes, .edges = decoded.edges };
        self.metadata.putAssumeCapacity(hash, entry);
        return entry;
    }

    /// The graph producer supplies exact content addresses. Grouping changes
    /// only catalog acquisition; every payload keeps its own hash validation,
    /// storage generation, and lifetime.
    fn acquireRecords(self: *LoadedPacks, store: *const Store, hashes: []const [32]u8, comptime kind: Store.IndexedKind) Allocator.Error!?[]?*const base.retained_cache.Record {
        const allocator = self.allocator;
        const keys = try allocator.alloc([64]u8, hashes.len);
        defer allocator.free(keys);
        for (hashes, keys) |hash, *key| key.* = store.indexedIdentity(.local, @splat(0), hash, kind).indexedKey();
        const batch = try self.retentionBatch(store);
        const acquired = batch.acquireKeys(allocator, keys, store.read_timestamp) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return null,
        };
        defer allocator.free(acquired);
        defer for (acquired) |*item| if (item.*) |*artifact| artifact.deinit();
        const records = try allocator.alloc(?*const base.retained_cache.Record, hashes.len);
        errdefer allocator.free(records);
        @memset(records, null);
        const owner = try self.recordOwner(store.roc_ctx.std_io);
        for (acquired, keys, records) |*item, key, *record| {
            if (kind == .metadata) self.index_stats.metadata_opens += 1 else self.index_stats.payload_opens += 1;
            const value = item.* orelse {
                self.index_stats.missing += 1;
                continue;
            };
            item.* = null;
            record.* = store.retainAcquired(owner, value, key, if (kind == .metadata) .native_component_v1 else .native_payload_v1) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.Corrupt => blk: {
                    self.index_stats.corrupt += 1;
                    break :blk null;
                },
                error.Unavailable => blk: {
                    self.index_stats.missing += 1;
                    break :blk null;
                },
            };
        }
        return records;
    }

    fn acquireGraphMetadata(self: *LoadedPacks, store: *const Store, root: *IndexedMetadata) Allocator.Error!bool {
        var entries: std.ArrayList(*IndexedMetadata) = .empty;
        defer entries.deinit(self.allocator);
        var hashes: std.ArrayList([32]u8) = .empty;
        defer hashes.deinit(self.allocator);
        var seen: std.AutoHashMapUnmanaged([32]u8, void) = .empty;
        defer seen.deinit(self.allocator);
        try entries.append(self.allocator, root);
        try seen.put(self.allocator, root.record.digest, {});
        var next: usize = 0;
        while (next < entries.items.len) {
            hashes.clearRetainingCapacity();
            while (next < entries.items.len) : (next += 1) {
                const entry = entries.items[next];
                if (entry.start != null) continue;
                for (entry.edges) |edge| {
                    const hash = edge.target.component orelse continue;
                    if ((try seen.getOrPut(self.allocator, hash)).found_existing) continue;
                    if (self.metadata.get(hash)) |known| {
                        try entries.append(self.allocator, known);
                    } else try hashes.append(self.allocator, hash);
                }
            }
            if (hashes.items.len == 0) break;
            const records = try self.acquireRecords(store, hashes.items, .metadata) orelse return false;
            defer self.allocator.free(records);
            const owner = try self.recordOwner(store.roc_ctx.std_io);
            for (hashes.items, records) |hash, record| {
                const entry = try self.adoptMetadata(owner, hash, record orelse return false) orelse return false;
                try entries.append(self.allocator, entry);
            }
        }
        return true;
    }

    /// Append each immutable component once to a coordinator-owned view. Existing
    /// indices never change. Symbolic bindings remain scoped to their producing
    /// component, so unrelated historical definitions cannot capture references.
    fn appendComponent(self: *LoadedPacks, entry: *IndexedMetadata) (Allocator.Error || error{MalformedPack})!void {
        const PA = backend.dev.CodeExcerpt;
        if (self.indexed_pack == null) {
            var value = PackFile.Pack{ .set = .{ .arena = base.SingleThreadArena.init(self.allocator), .excerpts = &.{} }, .specs = &.{} };
            errdefer value.deinit();
            self.indexed_pack = try self.adoptPack(value);
        }
        const pack = self.indexed_pack.?;
        const a = pack.set.arena.allocator();
        defer {
            pack.set.excerpts = self.indexed_artifacts.items;
            pack.set.source_files = self.indexed_files.items;
        }
        const start = std.math.cast(u32, self.indexed_artifacts.items.len) orelse return error.OutOfMemory;
        _ = std.math.cast(u32, self.indexed_artifacts.items.len + entry.pack.set.excerpts.len) orelse return error.OutOfMemory;
        // Validate external indices and symbolic identities before committing.
        for (entry.edges) |edge| {
            const target_set = if (edge.target.component) |hash| &self.metadata.get(hash).?.pack.set else &entry.pack.set;
            if (edge.target.index >= target_set.excerpts.len) return error.MalformedPack;
            if (edge.symbolic) {
                const target = entry.pack.set.excerpts[edge.excerpt].symbolic_refs[edge.reference].target;
                const kind = target_set.excerpts[edge.target.index].kind;
                const matches = switch (target) {
                    .proc => |id| kind == .proc and std.meta.eql(id, kind.proc),
                    .boxy_thunk => |id| kind == .boxy_thunk and std.meta.eql(id, kind.boxy_thunk),
                    .rc_helper => |name| kind == .rc_helper and std.mem.eql(u8, name, kind.rc_helper),
                };
                if (!matches) return error.MalformedPack;
            }
        }
        const files = try a.alloc(u32, entry.pack.set.source_files.len);
        for (entry.pack.set.source_files, files) |identity, *mapped| {
            const slot = try self.indexed_file_ids.getOrPut(self.allocator, identity);
            if (!slot.found_existing) {
                errdefer _ = self.indexed_file_ids.remove(identity);
                try self.indexed_files.append(a, identity);
                slot.value_ptr.* = @intCast(self.indexed_files.items.len - 1);
            }
            mapped.* = slot.value_ptr.*;
        }
        const artifacts = try a.dupe(PA.Excerpt, entry.pack.set.excerpts);
        for (artifacts) |*artifact| {
            artifact.refs = try a.dupe(PA.Reference, artifact.refs);
            artifact.symbolic_refs = try a.dupe(PA.SymbolicReference, artifact.symbolic_refs);
            const lines = try a.dupe(PA.LineEntry, artifact.lines);
            for (lines) |*line| line.loc = PA.rebindLoc(line.loc, files);
            artifact.lines = lines;
            artifact.loc = PA.rebindLoc(artifact.loc, files);
        }
        for (entry.edges) |edge| {
            const base_index = if (edge.target.component) |hash| self.metadata.get(hash).?.start.? else start;
            const target = base_index + edge.target.index;
            const artifact = &artifacts[edge.excerpt];
            if (edge.symbolic) @constCast(artifact.symbolic_refs)[edge.reference].binding = .{ .local = target } else @constCast(artifact.refs)[edge.reference].target = target;
        }
        try self.indexed_artifacts.appendSlice(a, artifacts);
        entry.start = start;
    }

    fn ensureGraph(self: *LoadedPacks, store: *const Store, root: *IndexedMetadata) Allocator.Error!bool {
        if (root.start != null) return true;
        if (!try self.acquireGraphMetadata(store, root)) return false;
        const Frame = struct { entry: *IndexedMetadata, next: usize = 0 };
        var stack = std.ArrayList(Frame).empty;
        defer stack.deinit(self.allocator);
        var visiting = std.AutoHashMap(*IndexedMetadata, void).init(self.allocator);
        defer visiting.deinit();
        try stack.append(self.allocator, .{ .entry = root });
        try visiting.put(root, {});
        while (stack.items.len != 0) {
            const top = &stack.items[stack.items.len - 1];
            if (top.next < top.entry.edges.len) {
                const edge = top.entry.edges[top.next];
                top.next += 1;
                const hash = edge.target.component orelse continue;
                const dependency = try self.loadMetadata(store, hash) orelse return false;
                if (dependency.start != null) continue;
                if ((try visiting.getOrPut(dependency)).found_existing) {
                    self.index_stats.corrupt += 1;
                    return false;
                }
                try stack.append(self.allocator, .{ .entry = dependency });
                continue;
            }
            self.appendComponent(top.entry) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.MalformedPack => {
                    self.index_stats.corrupt += 1;
                    return false;
                },
            };
            _ = visiting.remove(top.entry);
            _ = stack.pop();
        }
        return true;
    }

    fn materializeComponent(self: *LoadedPacks, store: *const Store, root: *IndexedMetadata) Allocator.Error!bool {
        if (root.materialized) return true;
        if (!try self.acquireGraphPayloads(store, root)) return false;
        const Frame = struct { entry: *IndexedMetadata, next: usize = 0 };
        var stack = std.ArrayList(Frame).empty;
        defer stack.deinit(self.allocator);
        try stack.append(self.allocator, .{ .entry = root });
        while (stack.items.len != 0) {
            const top = &stack.items[stack.items.len - 1];
            if (top.next < top.entry.edges.len) {
                const edge = top.entry.edges[top.next];
                top.next += 1;
                const hash = edge.target.component orelse continue;
                const dependency = self.metadata.get(hash).?;
                if (!dependency.materialized) try stack.append(self.allocator, .{ .entry = dependency });
                continue;
            }
            const entry = top.entry;
            for (@constCast(entry.pack.set.excerpts), entry.hashes, 0..) |*artifact, hash, i| {
                if (artifact.payload_record == null) {
                    const record = try self.readPayload(store, hash) orelse return false;
                    Index.materialize(artifact, record.bytes) catch {
                        self.index_stats.corrupt += 1;
                        return false;
                    };
                    artifact.payload_record = record;
                    self.index_stats.decoded_artifacts += 1;
                }
                const current = &self.indexed_artifacts.items[entry.start.? + i];
                current.code = artifact.code;
                current.payload_record = artifact.payload_record;
            }
            entry.materialized = true;
            _ = stack.pop();
        }
        return true;
    }

    fn acquireGraphPayloads(self: *LoadedPacks, store: *const Store, root: *IndexedMetadata) Allocator.Error!bool {
        var entries: std.ArrayList(*IndexedMetadata) = .empty;
        defer entries.deinit(self.allocator);
        var seen_entries: std.AutoHashMapUnmanaged(*IndexedMetadata, void) = .empty;
        defer seen_entries.deinit(self.allocator);
        var hashes: std.AutoArrayHashMapUnmanaged([32]u8, void) = .empty;
        defer hashes.deinit(self.allocator);
        try entries.append(self.allocator, root);
        while (entries.pop()) |entry| {
            if (entry.materialized or (try seen_entries.getOrPut(self.allocator, entry)).found_existing) continue;
            for (entry.hashes) |hash| if (!self.payloads.contains(hash)) {
                try hashes.put(self.allocator, hash, {});
            };
            for (entry.edges) |edge| if (edge.target.component) |hash| {
                try entries.append(self.allocator, self.metadata.get(hash).?);
            };
        }
        if (hashes.count() == 0) return true;
        const records = try self.acquireRecords(store, hashes.keys(), .payload) orelse return false;
        defer self.allocator.free(records);
        for (hashes.keys(), records) |hash, maybe_record| {
            const record = maybe_record orelse return false;
            try self.payloads.put(self.allocator, hash, record);
            self.bytes_loaded +|= record.bytes.len;
            self.index_stats.payload_bytes += record.bytes.len;
            self.index_stats.payload_files += 1;
        }
        return true;
    }

    /// Probe direct key buckets, then only candidates belonging to visible
    /// providers. Provider rank remains root-first, independent of disk order.
    fn findIndexed(self: *LoadedPacks, key: [32]u8, takes: Common.SpecCacheTakes, modules: Common.ProgramModules) Allocator.Error!IndexedResult {
        if (self.index_roots.items.len == 0) return .unavailable;
        var profile = NativeLookupProfile.Scope.begin(&self.lookup_profile);
        defer profile.end();
        const allocator = self.allocator;
        const Candidate = struct {
            digest: [32]u8,
            record: *const base.retained_cache.Record,
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
        for (self.index_roots.items) |*root| {
            profile.enter(.buckets);
            if (root.advertised == null) root.advertised = try self.loadOfferAdvertisement(root);
            if (!root.advertised.?) continue;
            self.index_stats.bucket_opens += 1;
            const group = root.store.indexedIdentity(root.origin, key, @splat(0), .offer);
            const keys = (try self.retentionBatch(root.store)).groupKeys(allocator, group) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => {
                    cacheable = false;
                    continue;
                },
            };
            defer allocator.free(keys);
            if (keys.len == 0) self.index_stats.bucket_absent += 1;
            profile.enter(.offers);
            for (keys) |catalog_key| {
                const owner = try self.recordOwner(root.store.roc_ctx.std_io);
                const record = root.store.acquireRetained(owner, try self.retentionBatch(root.store), catalog_key, .native_offer_v3) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.Corrupt => {
                        self.index_stats.corrupt += 1;
                        cacheable = false;
                        continue;
                    },
                    error.Unavailable => {
                        self.index_stats.missing += 1;
                        cacheable = false;
                        continue;
                    },
                };
                const bytes = record.bytes;
                self.bytes_loaded +|= bytes.len;
                self.index_stats.index_bytes += bytes.len;
                if (bytes.len < 32) {
                    self.index_stats.corrupt += 1;
                    cacheable = false;
                    continue;
                }
                const provider = root.store.packIdentity(root.origin, bytes[0..32].*, @splat(0)).indexedKey()[0..32].*;
                const rank = self.provider_ranks.get(provider) orelse continue;
                try candidates.append(allocator, .{ .digest = record.digest, .record = record, .store = root.store, .rank = rank });
            }
        }
        std.mem.sort(Candidate, candidates.items, {}, Candidate.less);
        for (candidates.items, 0..) |candidate, candidate_index| {
            profile.enter(.decode);
            self.index_stats.candidates += 1;
            if (candidate_index != 0) self.index_stats.duplicates += 1;
            const offer_record = candidate.record;
            const store = candidate.store;
            const bytes = offer_record.bytes[32..];
            var selection = Index.decodeOffer(allocator, bytes) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => {
                    self.index_stats.corrupt += 1;
                    cacheable = false;
                    continue;
                },
            };
            var adopted = false;
            defer if (!adopted) selection.pack.deinit();
            if (takes == .test_dev_code and !selection.pack.specs[0].test_compatible) continue;
            profile.enter(.metadata);
            const metadata = try self.loadMetadata(store, selection.component) orelse {
                cacheable = false;
                continue;
            };
            if (selection.index >= metadata.pack.set.excerpts.len or metadata.pack.set.excerpts[selection.index].kind != .proc or !std.mem.eql(u8, &selection.pack.specs[0].key, &key) or
                !std.meta.eql(selection.pack.set.excerpts[0].kind.proc, metadata.pack.set.excerpts[selection.index].kind.proc))
            {
                self.index_stats.corrupt += 1;
                cacheable = false;
                continue;
            }
            profile.enter(.graph);
            if (!try self.ensureGraph(store, metadata)) {
                cacheable = false;
                continue;
            }
            profile.enter(.eligibility);
            const pack = self.indexed_pack.?;
            var spec = selection.pack.specs[0];
            spec.excerpt = metadata.start.? + selection.index;
            const absent = try self.sourceMarks(&pack.set, modules);
            if (absent[spec.excerpt]) {
                self.index_stats.source_ineligible += 1;
                continue;
            }
            if (takes.refusesDevCodeOfLinkedProcs()) {
                self.ensureLinkedLoaded();
                const linked = try self.linkedMarks(&pack.set);
                if (linked[spec.excerpt]) {
                    self.index_stats.linked_ineligible += 1;
                    continue;
                }
            }
            self.index_stats.requested_artifacts += metadata.pack.set.excerpts.len;
            profile.enter(.payloads);
            if (!try self.materializeComponent(store, metadata)) {
                cacheable = false;
                continue;
            }
            profile.enter(.install);
            _ = try self.adoptPack(selection.pack);
            adopted = true;
            try self.excerpts.ensureUnusedCapacity(1);
            try self.specs.ensureUnusedCapacity(1);
            _ = self.specs.remove(key);
            _ = self.excerpts.remove(key);
            self.indexSpec(pack, spec) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => base.invariant("validated indexed metadata has a procedure root", .{}),
            };
            try self.retained_offers.put(allocator, key, offer_record);
            self.index_stats.served_artifacts += metadata.pack.set.excerpts.len;
            return .hit;
        }
        return if (cacheable) .miss else .unavailable;
    }

    fn readPayload(self: *LoadedPacks, store: *const Store, hash: [32]u8) Allocator.Error!?*const base.retained_cache.Record {
        if (self.payloads.get(hash)) |record| return record;
        self.index_stats.payload_opens += 1;
        const owner = try self.recordOwner(store.roc_ctx.std_io);
        const record = store.acquireRetained(owner, try self.retentionBatch(store), store.indexedIdentity(.local, @splat(0), hash, .payload).indexedKey(), .native_payload_v1) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.Corrupt => {
                self.index_stats.corrupt += 1;
                return null;
            },
            error.Unavailable => {
                self.index_stats.missing += 1;
                return null;
            },
        };
        self.bytes_loaded +|= record.bytes.len;
        self.index_stats.payload_bytes += record.bytes.len;
        try self.payloads.put(self.allocator, hash, record);
        self.index_stats.payload_files += 1;
        return record;
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
                switch (err) {
                    error.FileNotFound => if (input == .cache_offers) continue else return error.PackDirectoryUnreadable,
                    error.OutOfMemory => return error.OutOfMemory,
                    else => return error.PackDirectoryUnreadable,
                }
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
    /// already handed to the compile-time evaluator. An unavailable cache
    /// serves no lookup, so an offer to it has no reader and is dropped.
    /// The offered bytes are a pack this command just encoded, so only
    /// allocation can fail to take them.
    pub fn offerPack(self: *LoadedPacks, bytes: []const u8) Allocator.Error!void {
        self.ensureLoaded();
        if (self.state == .unavailable) return;
        var arena = base.SingleThreadArena.init(self.allocator);
        const owned = arena.allocator().dupe(u8, bytes) catch |err| {
            arena.deinit();
            self.unavailable(err);
            return err;
        };
        self.appendPack(arena, owned) catch |err| return self.offerFailed(err);
        if (self.state == .ready) self.indexPack(self.packs.items[self.packs.items.len - 1]) catch |err| return self.offerFailed(err);
    }

    fn offerFailed(self: *LoadedPacks, err: LoadError) Allocator.Error {
        switch (err) {
            error.OutOfMemory => {
                self.unavailable(err);
                return error.OutOfMemory;
            },
            error.MalformedPack, error.UnsupportedPackVersion, error.PackDirectoryUnreadable => base.invariant("a pack this command encoded could not be offered: {}", .{err}),
        }
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
            .may_leave_pending = spec.may_leave_pending,
            .iterator_fusion_scope = spec.iterator_fusion_scope,
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
                        .may_leave_pending = spec.may_leave_pending,
                        .iterator_fusion_scope = spec.iterator_fusion_scope,
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
                try self.loadStoredObject(store, store.artifactIdentity(.optimized_pack, placement.identity, @splat(0)));
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
        self.lookup_profile.report();
        if (base.dev_diagnostics.isSet(.pack_stats)) {
            const stats = self.index_stats;
            if (self.record_owner) |owner| std.debug.print("pack acquisitions: {any}\n", .{owner.metrics});
            std.debug.print("pack index: index_bytes={d} bucket_opens={d} bucket_absent={d} negative_hits={d} metadata_opens={d} metadata_bytes={d} candidates={d} duplicates={d} payload_opens={d} payload_files={d} payload_bytes={d} decoded_artifacts={d} requested_artifacts={d} served_artifacts={d} allocation_failures={d} requested={d} served={d} misses={d} source_ineligible={d} linked_ineligible={d} corrupt={d} missing={d}\n", .{ stats.index_bytes, stats.bucket_opens, stats.bucket_absent, stats.negative_hits, stats.metadata_opens, stats.metadata_bytes, stats.candidates, stats.duplicates, stats.payload_opens, stats.payload_files, stats.payload_bytes, stats.decoded_artifacts, stats.requested_artifacts, stats.served_artifacts, stats.allocation_failures, stats.requested, stats.served, stats.misses, stats.source_ineligible, stats.linked_ineligible, stats.corrupt, stats.missing });
        }
        if (base.dev_diagnostics.isSet(.pack_stats)) std.debug.print("pack interfaces: loaded_bytes={d} invalid_records={d}\n", .{ self.interface_bytes_loaded, self.interface_invalid_records });
        var interface_families = self.interface_families.valueIterator();
        while (interface_families.next()) |family| {
            for (family.*.records.values()) |record| {
                if (record.payload) |bytes| self.allocator.free(bytes);
                self.allocator.destroy(record);
            }
            family.*.records.deinit(self.allocator);
            self.allocator.destroy(family.*);
        }
        self.interface_families.deinit(self.allocator);
        self.negative.deinit(self.allocator);
        self.provider_ranks.deinit(self.allocator);
        self.index_roots.deinit(self.allocator);
        var metadata = self.metadata.valueIterator();
        while (metadata.next()) |entry| {
            self.allocator.destroy(entry.*);
        }
        self.metadata.deinit(self.allocator);
        self.indexed_file_ids.deinit(self.allocator);
        self.index_dirs.deinit(self.allocator);
        self.payloads.deinit(self.allocator);
        self.retained_offers.deinit(self.allocator);
        self.retention_batches.deinit(self.allocator);
        defer if (self.record_owner) |owner| owner.release();
        var settlements = self.discovery.valueIterator();
        while (settlements.next()) |list| list.deinit(self.allocator);
        self.discovery.deinit(self.allocator);
        self.discovery_arena.deinit();
        self.linked_specs.deinit();
        self.linked_identities.deinit(self.allocator);
        var reaching = self.dev_reaches_linked.valueIterator();
        while (reaching.next()) |marks| marks.deinit(self.allocator);
        self.dev_reaches_linked.deinit(self.allocator);
        var outside = self.lines_outside.valueIterator();
        while (outside.next()) |marks| marks.deinit(self.allocator);
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

    /// Caller holds interface_mutex. Families own requested records and misses;
    /// finding one request never reads or decodes its siblings or their history.
    fn interfaceFamily(self: *LoadedPacks, key: [32]u8) Allocator.Error!*InterfaceFamily {
        const entry = try self.interface_families.getOrPut(self.allocator, key);
        if (!entry.found_existing) {
            errdefer _ = self.interface_families.remove(key);
            const family = try self.allocator.create(InterfaceFamily);
            family.* = .{};
            entry.value_ptr.* = family;
        }
        return entry.value_ptr.*;
    }

    /// The map lock protects allocation and lookup only. Record addresses stay
    /// stable until all specialization workers have joined and this owner dies.
    fn interfaceRecord(self: *LoadedPacks, io: std.Io, family_key: [32]u8, request: [32]u8) Allocator.Error!*InterfaceRecord {
        self.interface_mutex.lockUncancelable(io);
        defer self.interface_mutex.unlock(io);
        const family = try self.interfaceFamily(family_key);
        const entry = try family.records.getOrPut(self.allocator, request);
        if (!entry.found_existing) {
            errdefer _ = family.records.swapRemove(request);
            const record = try self.allocator.create(InterfaceRecord);
            record.* = .{};
            entry.value_ptr.* = record;
        }
        return entry.value_ptr.*;
    }

    const InterfaceDecodeError = Allocator.Error || error{InvalidSummary};
    const interface_header_size = 104;

    fn readInterfaceRecord(family: [32]u8, request: [32]u8, bytes: []const u8) InterfaceDecodeError![]const u8 {
        if (bytes.len < interface_header_size or !std.mem.eql(u8, bytes[0..8], "RIFACE03") or
            !std.mem.eql(u8, bytes[8..40], &family) or !std.mem.eql(u8, bytes[40..72], &request)) return error.InvalidSummary;
        const payload = bytes[interface_header_size..];
        if (!std.mem.eql(u8, bytes[72..104], &Index.digest(payload))) return error.InvalidSummary;
        return payload;
    }

    fn loadInterface(context: *anyopaque, allocator: Allocator, family_key: [32]u8, request: [32]u8) Allocator.Error!?[]u8 {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        if (self.defer_code_lookup) return null;
        const store = self.interfaceStore() orelse return null;
        const record = try self.interfaceRecord(store.roc_ctx.std_io, family_key, request);
        record.mutex.lockUncancelable(store.roc_ctx.std_io);
        defer record.mutex.unlock(store.roc_ctx.std_io);
        if (record.loaded) return if (record.payload) |bytes| try allocator.dupe(u8, bytes) else null;
        const bytes = (try store.read(self.allocator, store.indexedIdentity(.local, family_key, request, .interface))) orelse {
            record.loaded = true;
            return null;
        };
        defer self.allocator.free(bytes);
        _ = @atomicRmw(u64, &self.interface_bytes_loaded, .Add, bytes.len, .monotonic);
        const payload = readInterfaceRecord(family_key, request, bytes) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidSummary => {
                _ = @atomicRmw(u64, &self.interface_invalid_records, .Add, 1, .monotonic);
                record.loaded = true;
                return null;
            },
        };
        record.payload = try self.allocator.dupe(u8, payload);
        record.loaded = true;
        return try allocator.dupe(u8, record.payload.?);
    }

    fn recordInterface(context: *anyopaque, family_key: [32]u8, request: [32]u8, bytes: []const u8) Allocator.Error!void {
        const self: *LoadedPacks = @ptrCast(@alignCast(context));
        if (self.defer_code_lookup) return;
        const store = self.interfaceStore() orelse return;
        const record = try self.interfaceRecord(store.roc_ctx.std_io, family_key, request);
        record.mutex.lockUncancelable(store.roc_ctx.std_io);
        defer record.mutex.unlock(store.roc_ctx.std_io);
        if (record.payload) |existing| if (std.mem.eql(u8, existing, bytes)) return;
        const owned = try self.allocator.dupe(u8, bytes);
        if (record.payload) |old| self.allocator.free(old);
        record.payload = owned;
        record.loaded = true;
        record.dirty = true;
    }

    /// Each request is an independently published immutable summary. Concurrent
    /// producers of unrelated requests cannot overwrite one another's records.
    pub fn publishInterfaces(self: *LoadedPacks) Allocator.Error!void {
        const store = self.interfaceStore() orelse return;
        self.interface_mutex.lockUncancelable(store.roc_ctx.std_io);
        defer self.interface_mutex.unlock(store.roc_ctx.std_io);
        var families = self.interface_families.iterator();
        while (families.next()) |entry| {
            const family = entry.value_ptr.*;
            for (family.records.keys(), family.records.values()) |request, record| {
                record.mutex.lockUncancelable(store.roc_ctx.std_io);
                defer record.mutex.unlock(store.roc_ctx.std_io);
                if (!record.dirty) continue;
                const payload = record.payload.?;
                var bytes = std.ArrayList(u8).empty;
                defer bytes.deinit(self.allocator);
                try bytes.appendSlice(self.allocator, "RIFACE03");
                try bytes.appendSlice(self.allocator, entry.key_ptr);
                try bytes.appendSlice(self.allocator, &request);
                try bytes.appendSlice(self.allocator, &Index.digest(payload));
                try bytes.appendSlice(self.allocator, payload);
                _ = store.publishIndexed(store.indexedIdentity(.local, entry.key_ptr.*, request, .interface), bytes.items) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.PackWriteFailed => continue,
                };
                record.dirty = false;
            }
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
        const marks = self.sourceMarks(located.set, modules) catch return true;
        return marks[located.index];
    }

    fn sourceMarks(self: *LoadedPacks, set: *const backend.dev.CodeExcerpt.Set, modules: Common.ProgramModules) Allocator.Error![]bool {
        const slot = try self.lines_outside.getOrPut(self.allocator, ProgramPack.of(set, modules));
        if (!slot.found_existing) slot.value_ptr.* = .{};
        const facts = slot.value_ptr;
        const first = facts.marks.items.len;
        if (first == set.excerpts.len) return facts.marks.items;
        // File identities and their bindings are immutable as the set grows.
        // Test each new file once, rather than once per source location.
        const old_files = facts.present_files.items.len;
        try facts.present_files.ensureTotalCapacity(self.allocator, set.source_files.len);
        for (set.source_files[old_files..]) |identity| {
            facts.present_files.appendAssumeCapacity(Common.programHasModule(modules, identity));
        }
        // Keep the old prefix on allocation failure; retry only the new suffix.
        try facts.marks.resize(self.allocator, set.excerpts.len);
        errdefer facts.marks.items.len = first;
        const marks = facts.marks.items;
        for (set.excerpts[first..], marks[first..]) |artifact, *marked| {
            marked.* = artifact.loc.hasLocation() and !facts.present_files.items[artifact.loc.file];
            for (artifact.lines) |line| {
                if (line.loc.hasLocation() and !facts.present_files.items[line.loc.file]) marked.* = true;
            }
        }
        try backend.dev.CodeExcerpt.markNewReferrers(self.allocator, set, marks, first, .skip);
        return marks;
    }

    fn linkedMarks(self: *LoadedPacks, set: *const backend.dev.CodeExcerpt.Set) Allocator.Error![]bool {
        const slot = try self.dev_reaches_linked.getOrPut(self.allocator, set);
        if (!slot.found_existing) slot.value_ptr.* = .empty;
        const first = slot.value_ptr.items.len;
        if (first == set.excerpts.len) return slot.value_ptr.items;
        try slot.value_ptr.resize(self.allocator, set.excerpts.len);
        errdefer slot.value_ptr.items.len = first;
        const marks = slot.value_ptr.items;
        for (set.excerpts[first..], marks[first..]) |artifact, *marked| {
            marked.* = artifact.kind == .proc and self.linked_identities.contains(artifact.kind.proc);
            for (artifact.symbolic_refs) |ref| {
                if (ref.target == .proc and self.linked_identities.contains(ref.target.proc)) marked.* = true;
            }
        }
        try backend.dev.CodeExcerpt.markNewReferrers(self.allocator, set, marks, first, .follow);
        return marks;
    }

    /// Whether a dev entry places code supplied by a linked object.
    fn devEntryReachesLinked(self: *LoadedPacks, key: [32]u8) bool {
        if (self.linked_identities.count() == 0) return false;
        const located = self.excerpts.get(key) orelse return false;
        const marks = self.linkedMarks(located.set) catch return true;
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
    // Even external deletion cannot revoke already acquired immutable bytes.
    const payload_bytes = try Index.payload(a, set.excerpts[0]);
    defer a.free(payload_bytes);
    const payload_id = store.indexedIdentity(.local, @splat(0), Index.digest(payload_bytes), .payload);
    {
        var artifact = (try store.native.acquire(payload_id, store.read_timestamp)).?;
        defer artifact.deinit();
        try std.Io.Dir.cwd().deleteFile(io, artifact.path);
    }
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

    fn init(self: *@This()) (std.Io.Dir.RealPathFileAllocError || Store.InitError)!void {
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

    fn excerpt(id: u8, code: []const u8) backend.dev.CodeExcerpt.Excerpt {
        return .{ .kind = .{ .proc = lir.ProcIdentity.forTest(id) }, .code = code, .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} };
    }

    fn spec(key: u8, artifact_index: u32) PackFile.SpecEntry {
        return .{ .key = @splat(key), .excerpt = artifact_index, .rc_borrowed_params = 3, .rc_ret_borrowed = true, .rc_ret_lenders = 2, .rc_read_only_params = 1, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{1} };
    }

    fn publish(self: *@This(), artifacts: []const backend.dev.CodeExcerpt.Excerpt, specs: []const PackFile.SpecEntry, files: []const [32]u8) (Allocator.Error || error{PackWriteFailed})!void {
        try self.store.writeIndexed(.local, identity, &.{ .arena = undefined, .excerpts = artifacts, .source_files = files }, specs);
    }

    fn payloadPath(self: *@This(), value: backend.dev.CodeExcerpt.Excerpt) error{ Busy, CapacityExceeded, Changed, CorruptCatalog, InvalidArtifact, IoFailure, Missing, OutOfMemory, PathTooLong, TestUnexpectedResult }![]u8 {
        const bytes = try Index.payload(allocator, value);
        defer allocator.free(bytes);
        var record = (try self.store.native.acquire(self.store.indexedIdentity(.local, @splat(0), Index.digest(bytes), .payload), self.store.native.now())) orelse return error.TestUnexpectedResult;
        defer record.deinit();
        return allocator.dupe(u8, record.path);
    }
};

test "indexed source eligibility extends file facts and keeps programs independent" {
    const F = IndexFixture;
    var packs = LoadedPacks.init(F.allocator);
    defer packs.deinit();
    var artifacts = [_]backend.dev.CodeExcerpt.Excerpt{ F.excerpt(1, "first"), F.excerpt(2, "second"), F.excerpt(3, "caller") };
    artifacts[0].loc = .{ .file = 0, .line = 1, .column = 1 };
    artifacts[1].lines = &.{.{ .offset = 0, .loc = .{ .file = 1, .line = 2, .column = 1 }, .region = base.Region.zero() }};
    artifacts[2].refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
    const files = [_][32]u8{ @splat(7), @splat(8) };
    var set = backend.dev.CodeExcerpt.Set{ .arena = undefined, .excerpts = artifacts[0..1], .source_files = files[0..1] };
    try std.testing.expectEqualSlices(bool, &.{false}, try packs.sourceMarks(&set, &.{files[0]}));
    set.excerpts = &artifacts;
    set.source_files = &files;
    try std.testing.expectEqualSlices(bool, &.{ false, true, true }, try packs.sourceMarks(&set, &.{files[0]}));
    try std.testing.expectEqualSlices(bool, &.{ true, false, false }, try packs.sourceMarks(&set, &.{files[1]}));
    try std.testing.expectEqualSlices(bool, &.{ false, true, true }, try packs.sourceMarks(&set, &.{files[0]}));
}

test "indexed graph without marked seeds needs no propagation allocation" {
    const F = IndexFixture;
    var artifacts = [_]backend.dev.CodeExcerpt.Excerpt{ F.excerpt(1, "caller"), F.excerpt(2, "callee") };
    artifacts[0].refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
    const set = backend.dev.CodeExcerpt.Set{ .arena = undefined, .excerpts = &artifacts };
    var marked = [_]bool{ false, false };
    var failures = std.testing.FailingAllocator.init(F.allocator, .{ .fail_index = 0 });
    try backend.dev.CodeExcerpt.markNewReferrers(failures.allocator(), &set, &marked, 0, .skip);
    try std.testing.expectEqualSlices(bool, &.{ false, false }, &marked);
    marked[1] = true;
    try backend.dev.CodeExcerpt.markNewReferrers(F.allocator, &set, &marked, 0, .skip);
    try std.testing.expectEqualSlices(bool, &.{ true, true }, &marked);
}

test "indexed object cache reads only requested closure regardless of unrelated history" {
    var fixture: IndexFixture = undefined;
    try fixture.init();
    defer fixture.deinit();
    const F = IndexFixture;
    const module: [32]u8 = @splat(17);
    var root = F.excerpt(1, "caller code");
    root.refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
    root.symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(3) } }};
    root.data = &.{.{ .program_local_name = true, .name = "roc__d0", .bytes = "constant bytes", .alignment = 8, .symbol_offset = 0, .relocations = &.{.{ .offset = 0, .name = "roc__d1", .addend = 0, .function = false }} }};
    root.lines = &.{.{ .offset = 0, .loc = .{ .file = 0, .line = 4, .column = 2 }, .region = base.Region.zero() }};
    var spec = F.spec(1, 0);
    spec.comptime_root_reads = &.{.{ .module = module, .root = 9 }};
    var callee = F.excerpt(2, "callee code");
    callee.data = &.{.{ .program_local_name = true, .name = "roc__d1", .bytes = "other bytes", .alignment = 8, .symbol_offset = 0, .relocations = &.{} }};
    const unused_code: [64 * 1024]u8 = @splat(3);
    var unused = F.excerpt(4, &unused_code);
    unused.loc = .{ .file = 1, .line = 99, .column = 1 };
    const artifacts = [_]backend.dev.CodeExcerpt.Excerpt{ root, callee, F.excerpt(3, "symbolic callee"), unused };
    try fixture.publish(&artifacts, &.{ spec, F.spec(200, 3) }, &.{ module, @splat(250) });
    var first = try fixture.loader();
    defer first.deinit();
    const hit = first.specCacheLookup().lookup(spec.key, .dev_code, &.{module}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 3), hit.rc_borrowed_params);
    try std.testing.expectEqual(@as(u32, 9), hit.comptime_root_reads[0].root);
    try std.testing.expectEqualSlices(u32, &.{1}, hit.rc_ret_conditions);
    const located = first.excerpts.get(spec.key).?;
    try std.testing.expectEqual(@as(usize, 3), located.set.excerpts.len);
    try std.testing.expectEqualStrings("constant bytes", located.set.excerpts[located.index].data[0].bytes);
    try std.testing.expectEqual(@as(u32, 4), located.set.excerpts[located.index].lines[0].loc.line);
    const dependency = located.set.excerpts[located.set.excerpts[located.index].refs[0].target];
    try std.testing.expectEqualStrings(dependency.data[0].name, located.set.excerpts[located.index].data[0].relocations[0].name);
    try std.testing.expect(std.mem.startsWith(u8, dependency.data[0].name, "roc__h"));
    try std.testing.expectEqual(@as(u64, 3), first.index_stats.payload_files);
    const before = first.index_stats;
    _ = first.specCacheLookup().lookup(spec.key, .dev_code, &.{module}).?;
    try std.testing.expectEqual(before.payload_opens, first.index_stats.payload_opens);
    try std.testing.expectEqual(before.index_bytes, first.index_stats.index_bytes);
    try std.testing.expectEqual(located.set, first.excerpts.get(spec.key).?.set);

    var large: [16 * 1024]u8 = @splat(7);
    for (2..130) |version| {
        large[0] = @intCast(version);
        const unrelated = F.excerpt(@intCast(version), &large);
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
        var artifact = F.excerpt(1, "same machine code");
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
            const selected = packs.excerpts.get(spec.key).?;
            try std.testing.expectEqual(files[version], selected.set.source_files[selected.set.excerpts[selected.index].loc.file]);
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
    var root = F.excerpt(1, "root");
    root.refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
    const dep = F.excerpt(2, "dependency");
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
    try std.testing.expectEqualStrings("dependency", located.set.excerpts[located.set.excerpts[located.index].refs[0].target].code);
}

test "indexed object cache source eligibility follows local dependencies but not site-named symbolic procedures" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var root = F.excerpt(1, "root");
    root.symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(2) } }};
    var callee = F.excerpt(2, "old site-named callee");
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
    var root = F.excerpt(1, "root");
    root.refs = &.{.{ .site = 0, .form = .call, .target = 1, .delta = 0 }};
    const dep = F.excerpt(2, "dependency");
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
    const unoffered = try Index.payload(F.allocator, F.excerpt(1, "code"));
    defer F.allocator.free(unoffered);
    try fixture.store.native.write(fixture.store.indexedIdentity(.local, @splat(0), Index.digest(unoffered), .payload), unoffered, fixture.store.native.now());
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
                self.store.writeIndexed(.local, F.identity, &.{ .arena = undefined, .excerpts = &.{F.excerpt(self.key, "code")} }, &.{F.spec(self.key, 0)}) catch |err| switch (err) {
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
    for ([_]u8{ 1, 2 }) |key| try fixture.publish(&.{F.excerpt(key, "code")}, &.{F.spec(key, 0)}, &.{});
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
        const artifacts = [_]backend.dev.CodeExcerpt.Excerpt{F.excerpt(1, "code")};
        const specs = [_]PackFile.SpecEntry{F.spec(1, 0)};
        try fixture.publish(&artifacts, &specs, &.{});
        const group = fixture.store.indexedIdentity(.local, if (shared_metadata) @splat(0) else @splat(1), @splat(0), if (shared_metadata) .metadata else .offer);
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
        const callee = F.excerpt(2, "callee");
        for (0..2) |i| {
            const swapped = (i == 1) != reverse;
            var caller = F.excerpt(1, "caller");
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
    try fixture.publish(&.{F.excerpt(1, "code")}, &.{F.spec(1, 0)}, &.{});
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

test "unadvertised object namespaces skip bucket probes and refresh on publication" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var packs = try fixture.loader();
    defer packs.deinit();
    for (1..128) |i| try fixture.store.loadIdentity(std.testing.io, &packs, .local, @splat(@intCast(i)));
    for (0..100) |_| try std.testing.expect(packs.specCacheLookup().lookup(@splat(250), .dev_code, &.{}) == null);
    try std.testing.expectEqual(@as(u64, 0), packs.index_stats.bucket_opens);
    try std.testing.expectEqual(@as(u64, 99), packs.index_stats.negative_hits);
    try fixture.publish(&.{F.excerpt(1, "new code")}, &.{F.spec(250, 0)}, &.{});
    // An existing provider's new offer invalidates the snapshot, even though
    // the provider set itself did not change.
    try packs.offerIndexed(&fixture.store, .local, F.identity);
    _ = packs.specCacheLookup().lookup(@splat(250), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 1), packs.index_stats.bucket_opens);
}

test "indexed object cache shares payloads and verifies immutable records before skipping writes" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var first = F.excerpt(1, "caller");
    var second = F.excerpt(2, "caller");
    first.refs = &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }};
    second.refs = first.refs;
    const artifacts = [_]backend.dev.CodeExcerpt.Excerpt{ first, second, F.excerpt(3, "shared dependency") };
    const specs = [_]PackFile.SpecEntry{ F.spec(1, 0), F.spec(2, 1) };
    try fixture.publish(&artifacts, &specs, &.{});
    const group = fixture.store.indexedIdentity(.local, @splat(0), @splat(0), .metadata);
    const keys = try fixture.store.native.groupKeys(F.allocator, group);
    defer F.allocator.free(keys);
    try std.testing.expect(keys.len > 0);
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
    try std.testing.expectEqual(@as(u64, 3), packs.index_stats.metadata_opens);
    try std.testing.expectEqual(@as(u64, 2), packs.index_stats.payload_files);
    try std.testing.expectEqual(@as(u64, 3), packs.index_stats.decoded_artifacts);
    const left = packs.excerpts.get(@splat(1)).?;
    const right = packs.excerpts.get(@splat(2)).?;
    try std.testing.expectEqual(left.set.excerpts[left.index].payload_record, right.set.excerpts[right.index].payload_record);
}

test "indexed object cache routes a package offer to producer storage outside its visible providers" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    const provider: compile.BuildEnv.PackPlacement = .{ .origin = .pkg, .identity = @splat(99) };
    try fixture.store.writeIndexedForProviders(.local, F.identity, &.{ .arena = undefined, .excerpts = &.{F.excerpt(1, "package code")} }, &.{F.spec(1, 0)}, &.{provider});
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
    try std.testing.expect(!producer.interface_families.get(family).?.records.get(first).?.dirty);
    try LoadedPacks.recordInterface(&producer, family, first, "one");
    try std.testing.expect(!producer.interface_families.get(family).?.records.get(first).?.dirty);
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
    try fixture.store.native.write(fixture.store.indexedIdentity(.local, family, first, .interface), "RIFACE03truncated", fixture.store.native.now());
    var corrupt = LoadedPacks.init(allocator);
    defer corrupt.deinit();
    corrupt.interface_store = &fixture.store;
    try std.testing.expectEqual(null, try LoadedPacks.loadInterface(&corrupt, allocator, family, first));
    const intact = (try LoadedPacks.loadInterface(&corrupt, allocator, family, second)).?;
    defer allocator.free(intact);
    try std.testing.expectEqualStrings("two", intact);
    try std.testing.expectEqual(@as(u64, 1), corrupt.interface_invalid_records);
    try LoadedPacks.recordInterface(&corrupt, family, first, "repaired");
    try corrupt.publishInterfaces();
    var repaired = LoadedPacks.init(allocator);
    defer repaired.deinit();
    repaired.interface_store = &fixture.store;
    const value = (try LoadedPacks.loadInterface(&repaired, allocator, family, first)).?;
    defer allocator.free(value);
    try std.testing.expectEqualStrings("repaired", value);
}

test "native publication reuses acquired payloads and offers across a one-caller edit" {
    const a = std.testing.allocator;
    const F = IndexFixture;
    const count = 48;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var stats: Store.PublicationStats = .{};
    fixture.store.publication_stats = &stats;
    var artifacts: [count + 1]backend.dev.CodeExcerpt.Excerpt = undefined;
    var specs: [count]PackFile.SpecEntry = undefined;
    const ref = backend.dev.CodeExcerpt.Reference{ .site = 0, .form = .call, .target = count, .delta = 0 };
    for (artifacts[0..count], &specs, 0..) |*artifact, *spec, i| {
        artifact.* = F.excerpt(@intCast(i + 1), "unchanged caller");
        artifact.refs = &.{ref};
        spec.* = F.spec(@intCast(i + 1), @intCast(i));
    }
    artifacts[count] = F.excerpt(100, "shared leaf");
    try fixture.publish(&artifacts, &specs, &.{});
    try std.testing.expectEqual(@as(u64, count + 1), stats.payloads_encoded);
    try std.testing.expectEqual(@as(u64, count + 1), stats.metadata_encoded);
    // One graph inventory and one union walk, independent of caller count.
    try std.testing.expectEqual(@as(u64, (count + 1) * 2), stats.graph_nodes);
    try std.testing.expectEqual(@as(u64, count * 2), stats.graph_edges);

    var loaded = try fixture.loader();
    defer loaded.deinit();
    for (specs) |spec| _ = loaded.specCacheLookup().lookup(spec.key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, count + 1), loaded.index_stats.decoded_artifacts);
    var combined = try backend.dev.CodeExcerpt.combineDistinct(a, &.{loaded.excerpts.get(specs[0].key).?.set});
    defer combined.deinit();
    var closure = try SpliceClosure.init(a, &combined);
    defer closure.deinit();
    for (&specs, 0..) |*spec, i| spec.excerpt = closure.offeredProcExcerpt(lir.ProcIdentity.forTest(@intCast(i + 1))).?;
    stats = .{};
    try fixture.store.writeIndexedRetained(.local, F.identity, &combined, &specs, null, &loaded);
    try std.testing.expectEqual(@as(u64, count), stats.offers_reused);
    try std.testing.expectEqual(@as(u64, 0), stats.payloads_encoded);
    try std.testing.expectEqual(@as(u64, 0), stats.metadata_encoded);
    try std.testing.expectEqual(@as(u64, 0), stats.bytes_hashed);
    try std.testing.expectEqual(@as(u64, 0), stats.records_read);
    try std.testing.expectEqual(@as(u64, 0), stats.records_written);
    try std.testing.expectEqual(@as(u64, (count + 1) * 2), stats.graph_nodes);

    // A byte-changing producer explicitly clears the immutable payload identity.
    const changed = &@constCast(combined.excerpts)[specs[0].excerpt];
    changed.payload_record = null;
    changed.code = "edited caller";
    specs[0].key = @splat(200);
    stats = .{};
    try fixture.store.writeIndexedRetained(.local, F.identity, &combined, &specs, null, &loaded);
    try std.testing.expectEqual(@as(u64, count - 1), stats.offers_reused);
    try std.testing.expectEqual(@as(u64, 1), stats.payloads_encoded);
    try std.testing.expectEqual(@as(u64, 1), stats.metadata_encoded);
    try std.testing.expectEqual(@as(u64, 2 * count + 1), stats.validated_references);
    try std.testing.expectEqual(@as(u64, (count + 1) * 2), stats.graph_nodes);
    try std.testing.expectEqual(@as(u64, 3), stats.records_written);
    var fresh = try fixture.loader();
    defer fresh.deinit();
    for (specs) |spec| _ = fresh.specCacheLookup().lookup(spec.key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    const edited = fresh.excerpts.get(specs[0].key).?;
    try std.testing.expectEqualStrings("edited caller", edited.set.excerpts[edited.index].code);
}

test "native publication unrelated offers do not change existing records" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var stats: Store.PublicationStats = .{};
    fixture.store.publication_stats = &stats;
    const a = F.excerpt(1, "one");
    const b = F.excerpt(2, "two");
    try fixture.publish(&.{a}, &.{F.spec(1, 0)}, &.{});
    stats = .{};
    try fixture.publish(&.{ b, a }, &.{ F.spec(2, 0), F.spec(1, 1) }, &.{});
    try std.testing.expectEqual(@as(u64, 3), stats.records_written);
    stats = .{};
    try fixture.publish(&.{ a, b }, &.{ F.spec(1, 0), F.spec(2, 1) }, &.{});
    try std.testing.expectEqual(@as(u64, 0), stats.records_written);
    stats = .{};
    try fixture.publish(&.{a}, &.{F.spec(1, 0)}, &.{});
    try std.testing.expectEqual(@as(u64, 0), stats.records_written);
}

test "native publication retained files survive cleanup until their last owner ends" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    const artifact = F.excerpt(1, "immutable bytes");
    const spec = F.spec(1, 0);
    try fixture.publish(&.{artifact}, &.{spec}, &.{});
    var loaded = try fixture.loader();
    var live = true;
    defer if (live) loaded.deinit();
    _ = loaded.specCacheLookup().lookup(spec.key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    const source = loaded.excerpts.get(spec.key).?;
    var copy = try backend.dev.CodeExcerpt.combine(F.allocator, &.{source.set});
    var copy_live = true;
    defer if (copy_live) copy.deinit();
    const path = try fixture.payloadPath(artifact);
    defer F.allocator.free(path);
    loaded.deinit();
    live = false;
    const payload = try Index.payload(F.allocator, artifact);
    defer F.allocator.free(payload);
    const id = fixture.store.indexedIdentity(.local, @splat(0), Index.digest(payload), .payload);
    try std.testing.expectError(error.Busy, fixture.store.native.invalidate(id));
    try std.testing.expectEqualStrings("immutable bytes", copy.excerpts[0].code);
    copy.deinit();
    copy_live = false;
    try fixture.store.native.invalidate(id);
    try std.testing.expectError(error.FileNotFound, std.Io.Dir.cwd().openFile(std.testing.io, path, .{}));
    var missing = try fixture.loader();
    defer missing.deinit();
    try std.testing.expect(missing.specCacheLookup().lookup(spec.key, .dev_code, &.{}) == null);
}

test "native publication allocation failures release temporary records and retained owners" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    const Check = struct {
        fn publish(failing: Allocator) error{ AccessDenied, AntivirusInterference, BadPathName, Canceled, DeviceBusy, FileBusy, FileNotFound, FileSystem, FileTooBig, InputOutput, IsDir, NameTooLong, NetworkNotFound, NoDevice, NoHomeDirectory, NoSpaceLeft, NotDir, OperationUnsupported, OutOfMemory, PackWriteFailed, PathAlreadyExists, PermissionDenied, PipeBusy, ProcessFdQuotaExceeded, SymLinkLoop, SystemFdQuotaExceeded, SystemResources, Unexpected, UnrecognizedVolume }!void {
            var fresh: F = undefined;
            try fresh.init();
            defer fresh.deinit();
            var writer = fresh.store;
            writer.allocator = failing;
            try writer.writeIndexed(.local, F.identity, &.{ .arena = undefined, .excerpts = &.{F.excerpt(1, "code")} }, &.{F.spec(1, 0)});
        }
        fn load(failing: Allocator, store: *const Store) error{ MalformedPack, OutOfMemory, PackDirectoryUnreadable, TestExpectedEqual, UnsupportedPackVersion }!void {
            var packs = LoadedPacks.init(failing);
            defer packs.deinit();
            try store.loadIdentity(std.testing.io, &packs, .local, F.identity);
            try packs.indexPacks();
            try std.testing.expectEqual(LoadedPacks.IndexedResult.hit, try packs.findIndexed(@splat(1), .dev_code, &.{}));
        }
        fn copy(failing: Allocator, set: *const backend.dev.CodeExcerpt.Set) error{ OutOfMemory, TestExpectedEqual }!void {
            var owned = try backend.dev.CodeExcerpt.combine(failing, &.{set});
            defer owned.deinit();
            try std.testing.expectEqual(set.excerpts[0].payload_record, owned.excerpts[0].payload_record);
        }
    };
    // In-place growth under the testing allocator depends on the heap's
    // layout, so the failure checks run over an allocator that refuses it.
    var deterministic = @import("base").DeterministicAllocator.init(F.allocator);
    std.testing.checkAllAllocationFailures(deterministic.allocator(), Check.publish, .{}) catch |err| {
        std.debug.print("publication allocation check: {}\n", .{err});
        return err;
    };
    try fixture.publish(&.{F.excerpt(1, "code")}, &.{F.spec(1, 0)}, &.{});
    std.testing.checkAllAllocationFailures(deterministic.allocator(), Check.load, .{&fixture.store}) catch |err| {
        std.debug.print("load allocation check: {}\n", .{err});
        return err;
    };
    var loaded = try fixture.loader();
    defer loaded.deinit();
    _ = loaded.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    std.testing.checkAllAllocationFailures(deterministic.allocator(), Check.copy, .{loaded.excerpts.get(@splat(1)).?.set}) catch |err| {
        std.debug.print("copy allocation check: {}\n", .{err});
        return err;
    };
}

test "native publication union visits shared cyclic dependencies once" {
    const F = IndexFixture;
    const PA = backend.dev.CodeExcerpt;
    const count = 128;
    var artifacts: [count]PA.Excerpt = undefined;
    var refs: [count]PA.Reference = undefined;
    var specs: [count]PackFile.SpecEntry = undefined;
    for (&artifacts, &refs, &specs, 0..) |*artifact, *ref, *spec, i| {
        ref.* = .{ .site = 0, .form = .call, .target = if (i == 0) 1 else 0, .delta = 0 };
        artifact.* = F.excerpt(@intCast(i), "code");
        artifact.refs = @as(*const [1]PA.Reference, ref)[0..];
        spec.* = F.spec(@intCast(i), @intCast(i));
    }
    var closure = try SpliceClosure.init(F.allocator, &.{ .arena = undefined, .excerpts = &artifacts });
    defer closure.deinit();
    const union_indices = try closure.ofEntries(&specs);
    try std.testing.expectEqual(@as(usize, count), union_indices.len);
    try std.testing.expectEqual(@as(usize, count), closure.nodes_visited);
    try std.testing.expectEqual(@as(usize, count), closure.edges_visited);
    try std.testing.expectEqual(@as(usize, 1), closure.queries);
}

test "native publication shares a cyclic component and invalidates exact dependency endpoints" {
    const F = IndexFixture;
    const PA = backend.dev.CodeExcerpt;
    const callers = 32;
    const cycle = 16;
    const count = callers + cycle;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var stats: Store.PublicationStats = .{};
    fixture.store.publication_stats = &stats;
    var artifacts: [count]PA.Excerpt = undefined;
    var refs: [count]PA.Reference = undefined;
    var specs: [callers]PackFile.SpecEntry = undefined;
    for (&artifacts, &refs, 0..) |*artifact, *ref, i| {
        artifact.* = F.excerpt(@intCast(i), "identical payload");
        ref.* = .{ .site = 0, .form = .call, .target = if (i < callers) callers else @intCast(callers + (i - callers + 1) % cycle), .delta = 0 };
        artifact.refs = @as(*const [1]PA.Reference, ref)[0..];
    }
    for (&specs, 0..) |*spec, i| spec.* = F.spec(@intCast(i), @intCast(i));
    try fixture.publish(&artifacts, &specs, &.{});
    try std.testing.expectEqual(@as(u64, callers + 1), stats.metadata_encoded);
    try std.testing.expectEqual(@as(u64, 2 * count), stats.graph_nodes);
    try std.testing.expectEqual(@as(u64, 2 * count), stats.graph_edges);
    var loaded = try fixture.loader();
    defer loaded.deinit();
    for (specs) |spec| _ = loaded.specCacheLookup().lookup(spec.key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    const set = loaded.excerpts.get(specs[0].key).?.set;
    for (specs) |spec| try std.testing.expectEqual(set, loaded.excerpts.get(spec.key).?.set);
    try std.testing.expectEqual(@as(usize, count), set.excerpts.len);
    try std.testing.expectEqual(@as(u64, count), loaded.index_stats.decoded_artifacts);
    try std.testing.expectEqual(@as(u64, callers + 1), loaded.index_stats.metadata_opens);
    try std.testing.expectEqual(@as(u64, 1), loaded.index_stats.payload_files);
    var owned = try PA.combine(F.allocator, &.{set});
    defer owned.deinit();
    for (&specs) |*spec| spec.excerpt = loaded.excerpts.get(spec.key).?.index;
    stats = .{};
    try fixture.store.writeIndexedRetained(.local, F.identity, &owned, &specs, null, &loaded);
    try std.testing.expectEqual(@as(u64, 0), stats.bytes_hashed);
    try std.testing.expectEqual(@as(u64, 0), stats.records_read);
    try std.testing.expectEqual(@as(u64, 0), stats.records_written);
    // Identical code and payload identity do not authorize an old graph edge.
    const root = &@constCast(owned.excerpts)[specs[0].excerpt];
    const previous = root.refs[0].target;
    const next = owned.excerpts[previous].refs[0].target;
    @constCast(root.refs)[0].target = next;
    specs[0].key = @splat(200);
    stats = .{};
    try fixture.store.writeIndexedRetained(.local, F.identity, &owned, &specs, null, &loaded);
    try std.testing.expectEqual(@as(u64, 0), stats.payloads_encoded);
    try std.testing.expectEqual(@as(u64, 1), stats.metadata_encoded);
    try std.testing.expectEqual(@as(u64, callers - 1), stats.offers_reused);
    try std.testing.expectEqual(@as(u64, 2), stats.records_written);
    var fresh = try fixture.loader();
    defer fresh.deinit();
    _ = fresh.specCacheLookup().lookup(specs[0].key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    const hit = fresh.excerpts.get(specs[0].key).?;
    const dependency = hit.set.excerpts[hit.set.excerpts[hit.index].refs[0].target];
    try std.testing.expectEqual(owned.excerpts[next].kind.proc, dependency.kind.proc);
}

test "native publication retains catalog generations through batch completion" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    const bytes = try Index.payload(F.allocator, F.excerpt(1, "code"));
    defer F.allocator.free(bytes);
    const id = fixture.store.indexedIdentity(.local, @splat(0), Index.digest(bytes), .payload);
    var pins = std.ArrayList(compile.artifacts.Artifact).empty;
    defer {
        for (pins.items) |*pin| pin.deinit();
        pins.deinit(F.allocator);
    }
    var stats: Store.PublicationStats = .{};
    _ = try fixture.store.publishIndexedHeld(id, bytes, &stats, &pins);
    try std.testing.expectError(error.Busy, fixture.store.native.invalidate(id));
    const metadata = "publication still in progress";
    _ = try fixture.store.publishIndexedHeld(fixture.store.indexedIdentity(.local, @splat(0), Index.digest(metadata), .metadata), metadata, &stats, &pins);
    try std.testing.expectError(error.Busy, fixture.store.native.invalidate(id));
    for (pins.items) |*pin| pin.deinit();
    pins.clearRetainingCapacity();
    try fixture.store.native.invalidate(id);
    try std.testing.expect(try fixture.store.native.acquire(id, fixture.store.native.now()) == null);
}

test "native publication interruption at each catalog boundary exposes only complete offers" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    const set = backend.dev.CodeExcerpt.Set{ .arena = undefined, .excerpts = &.{F.excerpt(1, "code")} };
    const specs = [_]PackFile.SpecEntry{F.spec(1, 0)};
    var closure = try SpliceClosure.init(F.allocator, &set);
    defer closure.deinit();
    var graph = try Index.ComponentGraph.init(F.allocator, &closure);
    defer graph.deinit();
    var names = try backend.dev.CodeExcerpt.ContentNames.init(F.allocator, &set);
    defer names.deinit();
    const payload = try Index.payload(F.allocator, set.excerpts[0]);
    defer F.allocator.free(payload);
    const hashes = [_][32]u8{Index.digest(payload)};
    var components: [1][32]u8 = undefined;
    const metadata = try Index.encodeComponent(F.allocator, &set, &graph, &closure, 0, &components, &hashes, &names);
    defer F.allocator.free(metadata);
    components[0] = Index.digest(metadata);
    const selection = try Index.encodeOffer(F.allocator, components[0], 0, set.excerpts[0].kind.proc, specs[0]);
    defer F.allocator.free(selection);
    const offer = try F.allocator.alloc(u8, 32 + selection.len);
    defer F.allocator.free(offer);
    @memcpy(offer[0..32], &F.identity);
    @memcpy(offer[32..], selection);
    const identities = [_]compile.artifacts.Identity{
        fixture.store.indexedIdentity(.local, @splat(0), hashes[0], .payload),
        fixture.store.indexedIdentity(.local, @splat(0), components[0], .metadata),
        fixture.store.indexedIdentity(.local, specs[0].key, Index.digest(offer), .offer),
    };
    _ = try fixture.store.publishIndexed(fixture.store.offerDomainIdentity(.local), Store.offer_domain_bytes);
    for (identities, [_][]const u8{ payload, metadata, offer }, 0..) |id, bytes, index| {
        _ = try fixture.store.publishIndexed(id, bytes);
        var packs = try fixture.loader();
        defer packs.deinit();
        const hit = packs.specCacheLookup().lookup(specs[0].key, .dev_code, &.{});
        try std.testing.expectEqual(index == 2, hit != null);
    }
}

test "native publication preserves symbolic bindings across historical definitions" {
    const F = IndexFixture;
    const PA = backend.dev.CodeExcerpt;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var first = F.excerpt(1, "first caller");
    var second = F.excerpt(3, "second caller");
    first.symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(2) } }};
    second.symbolic_refs = first.symbolic_refs;
    try fixture.publish(&.{ first, F.excerpt(2, "old definition") }, &.{F.spec(1, 0)}, &.{});
    try fixture.publish(&.{ second, F.excerpt(2, "new definition") }, &.{F.spec(3, 0)}, &.{});
    var loaded = try fixture.loader();
    defer loaded.deinit();
    for ([_]u8{ 3, 1 }) |key| _ = loaded.specCacheLookup().lookup(@splat(key), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    const old = loaded.excerpts.get(@splat(1)).?;
    const new = loaded.excerpts.get(@splat(3)).?;
    try std.testing.expectEqual(old.set, new.set);
    const old_target = old.set.symbolicDefinition(old.set.excerpts[old.index].symbolic_refs[0]).?;
    const new_target = new.set.symbolicDefinition(new.set.excerpts[new.index].symbolic_refs[0]).?;
    try std.testing.expectEqualStrings("old definition", old.set.excerpts[old_target].code);
    try std.testing.expectEqualStrings("new definition", new.set.excerpts[new_target].code);
    var captured = try PA.combine(F.allocator, &.{old.set});
    defer captured.deinit();
    var stats: Store.PublicationStats = .{};
    fixture.store.publication_stats = &stats;
    const specs = [_]PackFile.SpecEntry{ F.spec(1, old.index), F.spec(3, new.index) };
    try fixture.store.writeIndexedRetained(.local, F.identity, &captured, &specs, null, &loaded);
    try std.testing.expectEqual(@as(u64, 0), stats.bytes_hashed);
    try std.testing.expectEqual(@as(u64, 0), stats.records_written);
    // Rebinding a stable identity to different code preserves the payload but
    // must change this caller's component and selection manifest.
    @constCast(captured.excerpts[old.index].symbolic_refs)[0].binding = .{ .local = new_target };
    stats = .{};
    var changed = specs[0];
    changed.key = @splat(4);
    try fixture.store.writeIndexedRetained(.local, F.identity, &captured, &.{changed}, null, &loaded);
    try std.testing.expectEqual(@as(u64, 0), stats.payloads_encoded);
    try std.testing.expectEqual(@as(u64, 1), stats.metadata_encoded);
    try std.testing.expectEqual(@as(u64, 2), stats.records_written);
}

test "pack readers record retention once per invocation without overwriting newer readers" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var store = try Store.init(allocator, .{ .cache_dir = root, .roc_ctx = CoreCtx.default(allocator, allocator, io) }, .x64glibc, "dev");
    defer store.deinit();
    const id = store.packIdentity(.local, @splat(1), @splat(2));
    try store.native.write(id, "retained", 1);
    const Inspect = struct {
        fn read(s: *Store, identity: compile.artifacts.Identity) error{ Busy, CapacityExceeded, Changed, CorruptCatalog, InvalidArtifact, IoFailure, Missing, OutOfMemory, PathTooLong }!struct { sequence: u64, used: i64 } {
            var counters = compile.artifacts.catalog.Counters{};
            var shard = try compile.artifacts.Shard.openRequired(&s.native, compile.artifacts.Store.shardFor(identity.indexedKey()), false, &counters);
            defer shard.close();
            var tx = shard.index.transaction();
            const record = (try tx.find(.artifacts, identity.indexedKey())).?;
            return .{ .sequence = shard.index.header.sequence, .used = (try compile.artifacts.currentMetadata(&tx, identity.indexedKey(), record.value)).used };
        }
    };
    store.read_timestamp = 20;
    {
        const bytes = (try store.read(allocator, id)).?;
        defer allocator.free(bytes);
        try std.testing.expectEqualStrings("retained", bytes);
    }
    const first = try Inspect.read(&store, id);
    try std.testing.expectEqual(20, first.used);
    {
        const bytes = (try store.read(allocator, id)).?;
        defer allocator.free(bytes);
    }
    const second = try Inspect.read(&store, id);
    try std.testing.expectEqual(first.sequence, second.sequence);
    store.read_timestamp = 30;
    {
        const bytes = (try store.read(allocator, id)).?;
        defer allocator.free(bytes);
    }
    const newer = try Inspect.read(&store, id);
    try std.testing.expectEqual(30, newer.used);
    try std.testing.expect(newer.sequence > second.sequence);
    store.read_timestamp = 20;
    {
        const bytes = (try store.read(allocator, id)).?;
        defer allocator.free(bytes);
    }
    const older = try Inspect.read(&store, id);
    try std.testing.expectEqual(newer.sequence, older.sequence);
    try std.testing.expectEqual(30, older.used);
}

test "interface requests read only their record and independent publications preserve siblings" {
    var fixture: IndexFixture = undefined;
    try fixture.init();
    defer fixture.deinit();
    const allocator = std.testing.allocator;
    const family: [32]u8 = @splat(37);
    var first = LoadedPacks.init(allocator);
    defer first.deinit();
    first.interface_store = &fixture.store;
    var second = LoadedPacks.init(allocator);
    defer second.deinit();
    second.interface_store = &fixture.store;
    const target: [32]u8 = @splat(255);
    try std.testing.expectEqual(null, try LoadedPacks.loadInterface(&second, allocator, family, target));
    for (0..64) |i| {
        const request: [32]u8 = @splat(@intCast(i));
        const payload: [1024]u8 = @splat(@intCast(i));
        try LoadedPacks.recordInterface(&first, family, request, &payload);
    }
    try first.publishInterfaces();
    // This writer observed absence before the unrelated records existed.
    try LoadedPacks.recordInterface(&second, family, target, "requested");
    try second.publishInterfaces();
    var reader = LoadedPacks.init(allocator);
    defer reader.deinit();
    reader.interface_store = &fixture.store;
    const bytes = (try LoadedPacks.loadInterface(&reader, allocator, family, target)).?;
    defer allocator.free(bytes);
    try std.testing.expectEqualStrings("requested", bytes);
    try std.testing.expectEqual(LoadedPacks.interface_header_size + bytes.len, reader.interface_bytes_loaded);
    try std.testing.expectEqual(1, reader.interface_families.get(family).?.records.count());
    const sibling = (try LoadedPacks.loadInterface(&reader, allocator, family, @splat(32))).?;
    defer allocator.free(sibling);
    try std.testing.expectEqualSlices(u8, &@as([1024]u8, @splat(32)), sibling);
}

test "interface requests share same-key reads while independent workers publish and load" {
    const a = std.testing.allocator;
    var fixture: IndexFixture = undefined;
    try fixture.init();
    defer fixture.deinit();
    var producer = LoadedPacks.init(a);
    defer producer.deinit();
    producer.interface_store = &fixture.store;
    var reader = LoadedPacks.init(a);
    defer reader.deinit();
    reader.interface_store = &fixture.store;
    const Worker = struct {
        packs: *LoadedPacks,
        index: u8,
        record: bool,
        failed: bool = false,
        fn run(self: *@This()) void {
            self.work() catch {
                self.failed = true;
            };
        }
        fn work(self: *@This()) error{ OutOfMemory, TestExpectedEqual, TestUnexpectedResult }!void {
            const family: [32]u8 = @splat(61);
            const shared: [32]u8 = @splat(254);
            for (0..32) |i| {
                var key: [32]u8 = @splat(self.index);
                key[0] = @intCast(i);
                if (self.record) {
                    try LoadedPacks.recordInterface(self.packs, family, key, &key);
                    try LoadedPacks.recordInterface(self.packs, family, shared, &shared);
                }
                const bytes = (try LoadedPacks.loadInterface(self.packs, std.testing.allocator, family, key)) orelse return error.TestUnexpectedResult;
                defer std.testing.allocator.free(bytes);
                try std.testing.expectEqualSlices(u8, &key, bytes);
                const common = (try LoadedPacks.loadInterface(self.packs, std.testing.allocator, family, shared)) orelse return error.TestUnexpectedResult;
                defer std.testing.allocator.free(common);
                try std.testing.expectEqualSlices(u8, &shared, common);
            }
        }
    };
    for ([_]*LoadedPacks{ &producer, &reader }, 0..) |packs, pass| {
        var workers: [8]Worker = undefined;
        var threads: [8]std.Thread = undefined;
        var started: usize = 0;
        {
            defer for (threads[0..started]) |thread| thread.join();
            for (&workers, &threads, 0..) |*worker, *thread, i| {
                worker.* = .{ .packs = packs, .index = @intCast(i + 1), .record = pass == 0 };
                thread.* = try std.Thread.spawn(.{}, Worker.run, .{worker});
                started += 1;
            }
        }
        for (workers) |worker| try std.testing.expect(!worker.failed);
        if (pass == 0) try producer.publishInterfaces();
    }
    // Every request is read from storage once, even when all lanes ask for it.
    try std.testing.expectEqual(@as(u64, (8 * 32 + 1) * (LoadedPacks.interface_header_size + 32)), reader.interface_bytes_loaded);
}

test "native offer advertisements reject corruption and preserve snapshot refresh" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    try fixture.publish(&.{F.excerpt(1, "code")}, &.{F.spec(1, 0)}, &.{});
    {
        var marker = (try fixture.store.native.acquire(fixture.store.offerDomainIdentity(.local), fixture.store.read_timestamp)).?;
        defer marker.deinit();
        try std.Io.Dir.cwd().writeFile(std.testing.io, .{ .sub_path = marker.path, .data = "invalid advertisement" });
    }
    var packs = try fixture.loader();
    defer packs.deinit();
    try std.testing.expect(packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) == null);
    try std.testing.expectEqual(@as(u64, 0), packs.index_stats.bucket_opens);
    try fixture.publish(&.{F.excerpt(1, "code")}, &.{F.spec(1, 0)}, &.{});
    try packs.offerIndexed(&fixture.store, .local, F.identity);
    _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 1), packs.index_stats.bucket_opens);
    // The accepted advertisement's lease remains valid even if the name is
    // unlinked externally. It is retained together with the accepted offers.
    {
        var marker = (try fixture.store.native.acquire(fixture.store.offerDomainIdentity(.local), fixture.store.read_timestamp)).?;
        defer marker.deinit();
        try std.Io.Dir.cwd().deleteFile(std.testing.io, marker.path);
    }
    packs.refreshIndex();
    try std.testing.expect(packs.retainsOfferAdvertisement(&fixture.store, .local));
    _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
}

test "empty package namespace avoids per-key searches beside populated local offers" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    try fixture.publish(&.{F.excerpt(1, "local")}, &.{F.spec(1, 0)}, &.{});
    var packs = try fixture.loader();
    defer packs.deinit();
    const provider: compile.BuildEnv.PackPlacement = .{ .origin = .pkg, .identity = @splat(99) };
    try fixture.store.loadIdentity(std.testing.io, &packs, provider.origin, provider.identity);
    _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    for (2..11) |key| try std.testing.expect(packs.specCacheLookup().lookup(@splat(@intCast(key)), .dev_code, &.{}) == null);
    try std.testing.expectEqual(@as(u64, 10), packs.index_stats.bucket_opens);
    try fixture.store.writeIndexedForProviders(.local, F.identity, &.{ .arena = undefined, .excerpts = &.{F.excerpt(2, "package")} }, &.{F.spec(2, 0)}, &.{provider});
    try packs.offerIndexed(&fixture.store, provider.origin, provider.identity);
    _ = packs.specCacheLookup().lookup(@splat(2), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 12), packs.index_stats.bucket_opens);
}
