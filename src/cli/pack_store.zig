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

fn contentPath(allocator: Allocator, dir: []const u8, hash: [32]u8, kind: enum { metadata, payload }) Allocator.Error![]u8 {
    const name = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ std.fmt.bytesToHex(hash, .lower), if (kind == .metadata) "rpm" else "rpa" });
    defer allocator.free(name);
    return std.fs.path.join(allocator, &.{ dir, @tagName(kind), name });
}

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
        return .{ .allocator = allocator, .roc_ctx = cache_config.roc_ctx, .root = root, .procedure_root = procedure_root, .verbose = cache_config.verbose, .domain = backend.dev.NativeProcDomain.init(target, strategy).key() };
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

    /// Publish immutable artifact bytes first, then independent content-named
    /// offers. Writers never replace a shared manifest, so concurrent additions
    /// cannot lose one another. The caller publishes its completion marker last.
    pub fn writeIndexed(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, set: *const backend.dev.ProcArtifact.Set, specs: []const PackFile.SpecEntry) (Allocator.Error || error{PackWriteFailed})!void {
        return self.writeIndexedForProviders(origin, identity, set, specs, null);
    }

    /// Publish one producer's shared records and route each offer to its
    /// explicitly stamped provider. Null routes every offer to the producer.
    pub fn writeIndexedForProviders(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, set: *const backend.dev.ProcArtifact.Set, specs: []const PackFile.SpecEntry, providers: ?[]const compile.BuildEnv.PackPlacement) (Allocator.Error || error{PackWriteFailed})!void {
        return self.writeIndexedRetained(origin, identity, set, specs, providers, null);
    }

    /// Reuse exact offers already admitted and retained by this compilation.
    pub fn writeIndexedRetained(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, set: *const backend.dev.ProcArtifact.Set, specs: []const PackFile.SpecEntry, providers: ?[]const compile.BuildEnv.PackPlacement, retained: ?*const LoadedPacks) (Allocator.Error || error{PackWriteFailed})!void {
        var local_stats: PublicationStats = .{};
        const stats = self.publication_stats orelse &local_stats;
        const tracing = base.dev_diagnostics.isSet(.pack_stats);
        const start = if (tracing) std.Io.Timestamp.now(self.roc_ctx.std_io, .awake).nanoseconds else 0;
        defer if (tracing) std.debug.print("pack publication: {any} wall_ns={d}\n", .{ stats.*, std.Io.Timestamp.now(self.roc_ctx.std_io, .awake).nanoseconds - start });
        if (providers) |locations| std.debug.assert(locations.len == specs.len);
        if (specs.len == 0) return;
        const allocator = self.allocator;
        const dir = try self.identityDir(.local, @splat(0));
        defer allocator.free(dir);
        var closure = try SpliceClosure.init(allocator, set);
        defer closure.deinit();
        var graph = try Index.ComponentGraph.init(allocator, &closure);
        defer graph.deinit();
        stats.graph_nodes += set.artifacts.len;
        stats.graph_edges += graph.edge_count;
        const union_indices = try closure.ofEntries(specs);
        stats.graph_nodes += closure.nodes_visited;
        stats.graph_edges += closure.edges_visited;
        stats.closure_queries += closure.queries;
        const included = try allocator.alloc(bool, graph.components.count());
        defer allocator.free(included);
        @memset(included, false);
        const payload_hashes = try allocator.alloc([32]u8, set.artifacts.len);
        defer allocator.free(payload_hashes);
        const component_hashes = try allocator.alloc([32]u8, graph.components.count());
        defer allocator.free(component_hashes);
        var pins = std.ArrayList(std.Io.File).empty;
        defer {
            for (pins.items) |file| file.close(self.roc_ctx.std_io);
            pins.deinit(allocator);
        }
        var published = std.AutoHashMap([32]u8, void).init(allocator);
        defer published.deinit();
        for (union_indices) |index| {
            included[graph.components.component_of[index]] = true;
            const artifact = set.artifacts[index];
            if (artifact.payload_record) |record| {
                std.debug.assert(record.kind == .native_payload_v1);
                if (!std.mem.eql(u8, &record.owner.domain, &self.domain)) return error.PackWriteFailed;
                payload_hashes[index] = record.digest;
                if ((try published.getOrPut(record.digest)).found_existing) continue;
                const path = try contentPath(allocator, dir, record.digest, .payload);
                defer allocator.free(path);
                if (std.mem.eql(u8, record.path, path)) stats.validated_references += 1 else _ = try self.publishIndexedHeld(path, record.bytes, stats, &pins);
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
            const path = try contentPath(allocator, dir, hash, .payload);
            defer allocator.free(path);
            _ = try self.publishIndexedHeld(path, bytes, stats, &pins);
        }
        var names: ?backend.dev.ProcArtifact.ContentNames = null;
        defer if (names) |*value| value.deinit();
        var remaining = graph.components.count();
        while (remaining != 0) {
            remaining -= 1;
            if (!included[remaining]) continue;
            const c: u32 = @intCast(remaining);
            if (Index.reusableComponent(set, &graph, &closure, c, component_hashes, payload_hashes)) |record| {
                component_hashes[c] = record.digest;
                const path = try contentPath(allocator, dir, record.digest, .metadata);
                defer allocator.free(path);
                if (std.mem.eql(u8, record.path, path)) stats.validated_references += 1 else _ = try self.publishIndexedHeld(path, record.bytes, stats, &pins);
                continue;
            }
            if (names == null) names = try backend.dev.ProcArtifact.ContentNames.init(allocator, set);
            const bytes = try Index.encodeComponent(allocator, set, &graph, &closure, c, component_hashes, payload_hashes, &names.?);
            defer allocator.free(bytes);
            stats.metadata_encoded += 1;
            stats.metadata_bytes_encoded += bytes.len;
            stats.bytes_hashed += bytes.len;
            stats.peak_encoded_bytes = @max(stats.peak_encoded_bytes, bytes.len);
            const hash = Index.digest(bytes);
            component_hashes[c] = hash;
            const path = try contentPath(allocator, dir, hash, .metadata);
            defer allocator.free(path);
            _ = try self.publishIndexedHeld(path, bytes, stats, &pins);
        }
        for (specs, 0..) |spec, i| {
            const provider = if (providers) |locations| locations[i] else compile.BuildEnv.PackPlacement{ .origin = origin, .identity = identity };
            const hash = component_hashes[graph.components.component_of[spec.artifact]];
            const ordinal = graph.ordinal[spec.artifact];
            if (retained) |loaded| if (loaded.reusesOffer(self, spec, provider, hash, ordinal)) {
                stats.offers_reused += 1;
                stats.validated_references += 1;
                continue;
            };
            stats.offers_changed += 1;
            const bytes = try Index.encodeOffer(allocator, hash, ordinal, set.artifacts[spec.artifact].kind.proc, spec);
            defer allocator.free(bytes);
            stats.bytes_hashed += bytes.len;
            const name = std.fmt.bytesToHex(provider.identity, .lower) ++ "-".* ++ std.fmt.bytesToHex(Index.digest(bytes), .lower) ++ ".rpi".*;
            const path = try std.fs.path.join(allocator, &.{ self.procedure_root, @tagName(provider.origin), "offers", &std.fmt.bytesToHex(spec.key, .lower), &name });
            defer allocator.free(path);
            _ = try self.publishIndexedHeld(path, bytes, stats, &pins);
        }
    }

    /// Whether this exact producer completed indexed publication. A missing or
    /// corrupt marker is a miss; artifact availability is checked per lookup.
    pub fn hasIndexed(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8) Allocator.Error!bool {
        const path = try self.filePath(origin, identity, key, ".rpd");
        defer self.allocator.free(path);
        const bytes = self.roc_ctx.readFile(path, self.allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return false,
        };
        defer self.allocator.free(bytes);
        return std.mem.eql(u8, bytes, "ROCINDEX01");
    }

    /// Publish the completion marker only after publishing every offer.
    pub fn completeIndexed(self: *const Store, origin: compile.BuildEnv.PackOrigin, identity: [32]u8, key: [32]u8) (Allocator.Error || error{PackWriteFailed})!void {
        const path = try self.filePath(origin, identity, key, ".rpd");
        defer self.allocator.free(path);
        _ = try self.publishIndexedPath(path, "ROCINDEX01");
    }

    /// Replacing a corrupt optional record is repair, not an invariant failure.
    /// Identical concurrent publishers have identical content-addressed bytes.
    fn publishIndexedPath(self: *const Store, path: []const u8, bytes: []const u8) (Allocator.Error || error{PackWriteFailed})!bool {
        var stats: PublicationStats = .{};
        return self.publishIndexedCounted(path, bytes, self.publication_stats orelse &stats);
    }

    fn publishIndexedCounted(self: *const Store, path: []const u8, bytes: []const u8, stats: *PublicationStats) (Allocator.Error || error{PackWriteFailed})!bool {
        return self.publishIndexedHeld(path, bytes, stats, null);
    }

    fn publishIndexedHeld(self: *const Store, path: []const u8, bytes: []const u8, stats: *PublicationStats, pins: ?*std.ArrayList(std.Io.File)) (Allocator.Error || error{PackWriteFailed})!bool {
        if (pins) |held| try held.ensureUnusedCapacity(self.allocator, 1);
        const io = self.roc_ctx.std_io;
        const indexed = std.mem.endsWith(u8, path, ".rpa") or std.mem.endsWith(u8, path, ".rpm") or std.mem.endsWith(u8, path, ".rpi");
        var read_pin = if (indexed) base.retained_cache.pin(std.Io.Dir.cwd(), io, path, .shared) catch null else null;
        defer if (read_pin) |file| file.close(io);
        stats.records_read += 1;
        // Verify immutable bytes before skipping a write. Corrupt records are
        // repaired, and a vanished record is republished rather than trusted.
        const existing = self.roc_ctx.readFile(path, self.allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => null,
        };
        if (existing) |old| {
            defer self.allocator.free(old);
            stats.bytes_read += old.len;
            if (std.mem.eql(u8, old, bytes) and (!indexed or read_pin != null)) {
                if (pins) |held| {
                    held.appendAssumeCapacity(read_pin.?);
                    read_pin = null;
                }
                return false;
            }
        }
        if (read_pin) |file| {
            file.close(io);
            read_pin = null;
        }
        self.roc_ctx.makePath(std.fs.path.dirname(path).?) catch return error.PackWriteFailed;
        const staged = self.roc_ctx.writeTempFile(self.allocator, path, bytes) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.PackWriteFailed,
        };
        defer self.allocator.free(staged);
        defer self.roc_ctx.deleteFile(staged) catch {};
        var guard = if (indexed) base.retained_cache.Guard.acquire(std.Io.Dir.cwd(), io, path) catch return error.PackWriteFailed else null;
        defer if (guard) |*held| held.release();
        // Existing generations cannot be replaced while an acquisition pins
        // them. The mapping guard prevents a stale open from unlinking or
        // replacing a newer generation at the same path.
        var existing_file = if (indexed) std.Io.Dir.cwd().openFile(io, path, .{}) catch |err| switch (err) {
            error.FileNotFound => null,
            else => return error.PackWriteFailed,
        } else null;
        defer if (existing_file) |file| file.close(io);
        if (existing_file) |file| if (!(file.tryLock(io, .exclusive) catch return error.PackWriteFailed)) {
            // A concurrent publisher may have installed these exact bytes
            // after our initial miss and a reader may already own them.
            if (!(file.tryLock(io, .shared) catch return error.PackWriteFailed)) return error.PackWriteFailed;
            guard.?.release();
            guard = null;
            var reader = file.reader(io, &.{});
            stats.records_read += 1;
            const raced = reader.interface.allocRemaining(self.allocator, .limited(1024 * 1024 * 1024)) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => return error.PackWriteFailed,
            };
            defer self.allocator.free(raced);
            stats.bytes_read += raced.len;
            if (std.mem.eql(u8, raced, bytes)) {
                if (pins) |held| {
                    held.appendAssumeCapacity(file);
                    existing_file = null;
                }
                return false;
            }
            return error.PackWriteFailed;
        };
        // Pin our staging generation before it becomes visible. The batch
        // retains it until every referring metadata record and offer is ready.
        var staged_pin = if (pins != null) std.Io.Dir.cwd().openFile(io, staged, .{}) catch return error.PackWriteFailed else null;
        defer if (staged_pin) |file| file.close(io);
        if (staged_pin) |file| if (!(file.tryLock(io, .shared) catch return error.PackWriteFailed)) return error.PackWriteFailed;
        self.roc_ctx.rename(staged, path) catch return error.PackWriteFailed;
        if (pins) |held| {
            held.appendAssumeCapacity(staged_pin.?);
            staged_pin = null;
        }
        stats.records_written += 1;
        stats.bytes_written += bytes.len;
        return true;
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

    /// Register a keyed offer index without reading or enumerating history.
    pub fn loadIdentity(self: *const Store, io: std.Io, packs: *LoadedPacks, origin: compile.BuildEnv.PackOrigin, identity: [32]u8) Allocator.Error!void {
        const dir = try self.identityDir(origin, identity);
        defer self.allocator.free(dir);
        packs.record_domain = self.domain;
        try packs.addIndexDir(io, dir);
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

test "native procedure domain shares packs and interfaces but isolates final outputs and linked objects" {
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
    const artifact: backend.dev.ProcArtifact.Artifact = .{ .kind = .{ .proc = lir.ProcIdentity.forTest(4) }, .code = "\xc3", .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} };
    const spec: PackFile.SpecEntry = .{ .key = offered_key, .artifact = 0, .rc_borrowed_params = 0, .rc_ret_borrowed = false, .rc_ret_lenders = 0, .rc_read_only_params = 0, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{} };
    try musl.writeIndexed(.local, identity, &.{ .arena = undefined, .artifacts = &.{artifact} }, &.{spec});
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

    const artifact = IndexFixture.artifact(1, "indexed code");
    const spec = IndexFixture.spec(1, 0);
    try store.writeIndexed(.local, local_identity, &.{ .arena = undefined, .artifacts = &.{artifact} }, &.{spec});

    const limits: compile.CacheCleanup.Limits = .{
        .object_pack_format_version = PackFile.format_version,
        .max_size_bytes = std.math.maxInt(u64),
    };
    const now_ns = compile.CacheCleanup.nowNs(io);
    var stats: compile.CacheCleanup.CleanupStats = .{};
    compile.CacheCleanup.sweep(io, cache_root, limits, now_ns + 2 * compile.CacheCleanup.Config.LOCAL_PACK_MAX_AGE_NS, &stats);
    try std.testing.expectEqual(@as(u32, 1), stats.stale_namespaces_deleted);
    try std.testing.expectEqual(@as(u32, 4), stats.cache_files_deleted);
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

    /// One checked module/context provider, shared by its procedure requests.
    const InterfaceFamily = struct {
        records: std.AutoArrayHashMapUnmanaged([32]u8, []const u8) = .empty,
        loaded: bool = false,
        dirty: bool = false,
    };
    interface_mutex: std.Io.Mutex = .init,
    /// Worker reads update this under interface_mutex; reported after joining.
    interface_bytes_loaded: u64 = 0,
    interface_families: std.AutoHashMapUnmanaged([32]u8, *InterfaceFamily) = .empty,
    interface_store: ?*const Store = null,
    allocator: Allocator,
    index_dirs: std.ArrayList([]const u8) = .empty,
    index_io: ?std.Io = null,
    index_roots: std.ArrayList([]const u8) = .empty,
    provider_ranks: std.StringHashMapUnmanaged(u32) = .empty,
    metadata: std.AutoHashMapUnmanaged([32]u8, *IndexedMetadata) = .empty,
    indexed_pack: ?*PackFile.Pack = null,
    indexed_artifacts: std.ArrayList(backend.dev.ProcArtifact.Artifact) = .empty,
    indexed_files: std.ArrayList([32]u8) = .empty,
    indexed_file_ids: std.AutoHashMapUnmanaged([32]u8, u32) = .empty,
    negative: std.AutoHashMapUnmanaged(MissKey, void) = .empty,

    record_owner: ?*base.retained_cache.Owner = null,
    record_domain: [32]u8 = @splat(0),
    payloads: std.AutoHashMapUnmanaged([32]u8, *const base.retained_cache.Record) = .empty,
    retained_offers: std.AutoHashMapUnmanaged([32]u8, *const base.retained_cache.Record) = .empty,

    index_stats: IndexStats = .{},
    /// A completed-output probe has priority over procedure discovery. Until
    /// it misses, compile-time demand uses source bodies and cached values.
    defer_code_lookup: bool = false,
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
    dev_reaches_linked: std.AutoHashMapUnmanaged(*const backend.dev.ProcArtifact.Set, std.ArrayList(bool)) = .empty,
    /// Per dev pack and asking program, whether a splice of each artifact
    /// would place a line or a location in a module version the program does
    /// not lower against (`linesOutsideProgram`).
    lines_outside: std.AutoHashMapUnmanaged(ProgramPack, std.ArrayList(bool)) = .empty,
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

    fn recordOwner(self: *LoadedPacks, io: std.Io) Allocator.Error!*base.retained_cache.Owner {
        if (self.record_owner) |owner| return owner;
        const owner = try base.retained_cache.Owner.create(self.allocator, io, self.record_domain);
        self.record_owner = owner;
        return owner;
    }

    fn reusesOffer(self: *const LoadedPacks, store: *const Store, spec: PackFile.SpecEntry, provider: compile.BuildEnv.PackPlacement, component: [32]u8, ordinal: u32) bool {
        const offer = self.retained_offers.get(spec.key) orelse return false;
        const old = self.artifacts.get(spec.key) orelse return false;
        const origin = old.set.artifacts[old.index].metadata_origin orelse return false;
        if (!std.mem.eql(u8, &origin.record.owner.domain, &store.domain) or !std.mem.eql(u8, &origin.record.digest, &component) or origin.index != ordinal) return false;
        const hit = self.specs.get(spec.key) orelse return false;
        if (hit.rc_borrowed_params != spec.rc_borrowed_params or hit.rc_ret_borrowed != spec.rc_ret_borrowed or
            hit.rc_ret_lenders != spec.rc_ret_lenders or hit.rc_read_only_params != spec.rc_read_only_params or
            hit.rc_ret_unique != spec.rc_ret_unique or hit.rc_ret_unique_fields != spec.rc_ret_unique_fields or
            !std.mem.eql(u32, hit.rc_ret_conditions, spec.rc_ret_conditions) or hit.comptime_root_reads.len != spec.comptime_root_reads.len) return false;
        for (hit.comptime_root_reads, spec.comptime_root_reads) |a, b| if (!std.meta.eql(a, b)) return false;
        const name = std.fs.path.basename(offer.path);
        if (name.len != 133 or !std.mem.eql(u8, name[0..64], &std.fmt.bytesToHex(provider.identity, .lower))) return false;
        const bucket = std.fs.path.dirname(offer.path).?;
        const offers = std.fs.path.dirname(bucket).?;
        const origin_dir = std.fs.path.dirname(offers).?;
        return std.mem.eql(u8, std.fs.path.basename(origin_dir), @tagName(provider.origin)) and
            std.mem.eql(u8, std.fs.path.dirname(origin_dir).?, store.procedure_root);
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

    pub fn addIndexDir(self: *LoadedPacks, io: std.Io, dir: []const u8) Allocator.Error!void {
        if (self.provider_ranks.contains(dir)) return;
        try self.index_dirs.ensureUnusedCapacity(self.allocator, 1);
        try self.index_roots.ensureUnusedCapacity(self.allocator, 1);
        const owned = try self.allocator.dupe(u8, dir);
        errdefer self.allocator.free(owned);
        try self.provider_ranks.put(self.allocator, owned, @intCast(self.index_dirs.items.len));
        self.index_dirs.appendAssumeCapacity(owned);
        const root = std.fs.path.dirname(owned).?;
        const known = for (self.index_roots.items) |existing| {
            if (std.mem.eql(u8, existing, root)) break true;
        } else false;
        if (!known) self.index_roots.appendAssumeCapacity(root);
        self.index_io = io;
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

    fn loadMetadata(self: *LoadedPacks, io: std.Io, module_dir: []const u8, hash: [32]u8) Allocator.Error!?*IndexedMetadata {
        if (self.metadata.get(hash)) |existing| return existing;
        const allocator = self.allocator;
        const path = try contentPath(allocator, module_dir, hash, .metadata);
        defer allocator.free(path);
        self.index_stats.metadata_opens += 1;
        const owner = try self.recordOwner(io);
        const record = owner.acquire(path, hash, .native_component_v1) catch |err| switch (err) {
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
        const PA = backend.dev.ProcArtifact;
        const origins = try a.alloc(PA.MetadataOrigin, decoded.pack.set.artifacts.len);
        for (decoded.pack.set.artifacts, origins, 0..) |artifact, *origin, i| {
            const refs = try a.alloc(PA.Binding, artifact.refs.len);
            const symbolic = try a.alloc(?PA.Binding, artifact.symbolic_refs.len);
            @memset(symbolic, null);
            origin.* = .{ .record = record, .source = &pack.set, .index = @intCast(i), .refs = refs, .symbolic = symbolic };
        }
        for (decoded.edges) |edge| {
            const target = PA.Binding{ .component = edge.target.component orelse hash, .index = edge.target.index };
            const origin = &origins[edge.artifact];
            if (edge.symbolic) @constCast(origin.symbolic)[edge.reference] = target else @constCast(origin.refs)[edge.reference] = target;
        }
        for (@constCast(decoded.pack.set.artifacts), origins) |*artifact, *origin| artifact.metadata_origin = origin;
        pack.* = decoded.pack;
        try owner.adopt(.{ .context = pack, .deinit = releaseRecordPack });
        adopted = true;
        entry.* = .{ .record = record, .pack = pack, .hashes = decoded.hashes, .edges = decoded.edges };
        self.metadata.putAssumeCapacity(hash, entry);
        return entry;
    }

    /// Append each immutable component once to a coordinator-owned view. Existing
    /// indices never change. Symbolic bindings remain scoped to their producing
    /// component, so unrelated historical definitions cannot capture references.
    fn appendComponent(self: *LoadedPacks, entry: *IndexedMetadata) (Allocator.Error || error{MalformedPack})!void {
        const PA = backend.dev.ProcArtifact;
        if (self.indexed_pack == null) {
            var value = PackFile.Pack{ .set = .{ .arena = base.SingleThreadArena.init(self.allocator), .artifacts = &.{} }, .specs = &.{} };
            errdefer value.deinit();
            self.indexed_pack = try self.adoptPack(value);
        }
        const pack = self.indexed_pack.?;
        const a = pack.set.arena.allocator();
        defer {
            pack.set.artifacts = self.indexed_artifacts.items;
            pack.set.source_files = self.indexed_files.items;
        }
        const start = std.math.cast(u32, self.indexed_artifacts.items.len) orelse return error.OutOfMemory;
        _ = std.math.cast(u32, self.indexed_artifacts.items.len + entry.pack.set.artifacts.len) orelse return error.OutOfMemory;
        // Validate external indices and symbolic identities before committing.
        for (entry.edges) |edge| {
            const target_set = if (edge.target.component) |hash| &self.metadata.get(hash).?.pack.set else &entry.pack.set;
            if (edge.target.index >= target_set.artifacts.len) return error.MalformedPack;
            if (edge.symbolic) {
                const target = entry.pack.set.artifacts[edge.artifact].symbolic_refs[edge.reference].target;
                const kind = target_set.artifacts[edge.target.index].kind;
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
        const artifacts = try a.dupe(PA.Artifact, entry.pack.set.artifacts);
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
            const artifact = &artifacts[edge.artifact];
            if (edge.symbolic) @constCast(artifact.symbolic_refs)[edge.reference].binding = .{ .local = target } else @constCast(artifact.refs)[edge.reference].target = target;
        }
        try self.indexed_artifacts.appendSlice(a, artifacts);
        entry.start = start;
    }

    fn ensureGraph(self: *LoadedPacks, io: std.Io, module_dir: []const u8, root: *IndexedMetadata) Allocator.Error!bool {
        if (root.start != null) return true;
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
                const dependency = try self.loadMetadata(io, module_dir, hash) orelse return false;
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

    fn materializeComponent(self: *LoadedPacks, io: std.Io, module_dir: []const u8, root: *IndexedMetadata) Allocator.Error!bool {
        if (root.materialized) return true;
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
            for (@constCast(entry.pack.set.artifacts), entry.hashes, 0..) |*artifact, hash, i| {
                if (artifact.payload_record == null) {
                    const record = try self.readPayload(io, module_dir, hash) orelse return false;
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

    /// Probe direct key buckets, then only candidates belonging to visible
    /// providers. Provider rank remains root-first, independent of disk order.
    fn findIndexed(self: *LoadedPacks, key: [32]u8, takes: Common.SpecCacheTakes, modules: Common.ProgramModules) Allocator.Error!IndexedResult {
        const io = self.index_io orelse return .unavailable;
        const allocator = self.allocator;
        const Candidate = struct {
            name: []u8,
            bucket: []const u8,
            rank: u32,
            fn less(_: void, a: @This(), b: @This()) bool {
                if (a.rank != b.rank) return a.rank < b.rank;
                return std.mem.lessThan(u8, a.name, b.name);
            }
        };
        var candidates = std.ArrayList(Candidate).empty;
        var buckets = std.ArrayList([]u8).empty;
        defer {
            for (candidates.items) |candidate| allocator.free(candidate.name);
            candidates.deinit(allocator);
            for (buckets.items) |bucket| allocator.free(bucket);
            buckets.deinit(allocator);
        }
        var cacheable = true;
        for (self.index_roots.items) |index_root| {
            try buckets.ensureUnusedCapacity(allocator, 1);
            const bucket = try std.fs.path.join(allocator, &.{ index_root, "offers", &std.fmt.bytesToHex(key, .lower) });
            buckets.appendAssumeCapacity(bucket);
            self.index_stats.bucket_opens += 1;
            var dir = std.Io.Dir.cwd().openDir(io, bucket, .{ .iterate = true }) catch |err| switch (err) {
                error.FileNotFound => {
                    self.index_stats.bucket_absent += 1;
                    continue;
                },
                else => {
                    cacheable = false;
                    continue;
                },
            };
            defer dir.close(io);
            var it = dir.iterate();
            while (it.next(io) catch blk: {
                cacheable = false;
                break :blk null;
            }) |entry| {
                if (entry.kind != .file or entry.name.len != 133 or entry.name[64] != '-' or !std.mem.endsWith(u8, entry.name, ".rpi")) continue;
                const module_dir = try std.fs.path.join(allocator, &.{ index_root, entry.name[0..64] });
                defer allocator.free(module_dir);
                const rank = self.provider_ranks.get(module_dir) orelse continue;
                try candidates.ensureUnusedCapacity(allocator, 1);
                candidates.appendAssumeCapacity(.{ .name = try allocator.dupe(u8, entry.name), .bucket = bucket, .rank = rank });
            }
        }
        std.mem.sort(Candidate, candidates.items, {}, Candidate.less);
        for (candidates.items, 0..) |candidate, candidate_index| {
            self.index_stats.candidates += 1;
            if (candidate_index != 0) self.index_stats.duplicates += 1;
            const path = try std.fs.path.join(allocator, &.{ candidate.bucket, candidate.name });
            defer allocator.free(path);
            var offer_digest: [32]u8 = undefined;
            _ = std.fmt.hexToBytes(&offer_digest, candidate.name[65..129]) catch {
                cacheable = false;
                continue;
            };
            const owner = try self.recordOwner(io);
            const offer_record = owner.acquire(path, offer_digest, .native_offer_v3) catch |err| switch (err) {
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
            const bytes = offer_record.bytes;
            self.bytes_loaded +|= bytes.len;
            self.index_stats.index_bytes += bytes.len;
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
            const provider_dir = self.index_dirs.items[candidate.rank];
            const namespace = std.fs.path.dirname(std.fs.path.dirname(provider_dir).?).?;
            const module_dir = try std.fs.path.join(allocator, &.{ namespace, "local", &std.fmt.bytesToHex(@as([32]u8, @splat(0)), .lower) });
            defer allocator.free(module_dir);
            const metadata = try self.loadMetadata(io, module_dir, selection.component) orelse {
                cacheable = false;
                continue;
            };
            if (selection.index >= metadata.pack.set.artifacts.len or metadata.pack.set.artifacts[selection.index].kind != .proc or !std.mem.eql(u8, &selection.pack.specs[0].key, &key) or
                !std.meta.eql(selection.pack.set.artifacts[0].kind.proc, metadata.pack.set.artifacts[selection.index].kind.proc))
            {
                self.index_stats.corrupt += 1;
                cacheable = false;
                continue;
            }
            if (!try self.ensureGraph(io, module_dir, metadata)) {
                cacheable = false;
                continue;
            }
            const pack = self.indexed_pack.?;
            var spec = selection.pack.specs[0];
            spec.artifact = metadata.start.? + selection.index;
            const absent = try self.sourceMarks(&pack.set, modules);
            if (absent[spec.artifact]) {
                self.index_stats.source_ineligible += 1;
                continue;
            }
            if (takes.refusesDevCodeOfLinkedProcs()) {
                self.ensureLinkedLoaded();
                const linked = try self.linkedMarks(&pack.set);
                if (linked[spec.artifact]) {
                    self.index_stats.linked_ineligible += 1;
                    continue;
                }
            }
            self.index_stats.requested_artifacts += metadata.pack.set.artifacts.len;
            if (!try self.materializeComponent(io, module_dir, metadata)) {
                cacheable = false;
                continue;
            }
            _ = try self.adoptPack(selection.pack);
            adopted = true;
            try self.artifacts.ensureUnusedCapacity(1);
            try self.specs.ensureUnusedCapacity(1);
            _ = self.specs.remove(key);
            _ = self.artifacts.remove(key);
            self.indexSpec(pack, spec) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                else => base.invariant("validated indexed metadata has a procedure root", .{}),
            };
            try self.retained_offers.put(allocator, key, offer_record);
            self.index_stats.served_artifacts += metadata.pack.set.artifacts.len;
            return .hit;
        }
        return if (cacheable) .miss else .unavailable;
    }

    fn readPayload(self: *LoadedPacks, io: std.Io, module_dir: []const u8, hash: [32]u8) Allocator.Error!?*const base.retained_cache.Record {
        if (self.payloads.get(hash)) |record| return record;
        const name = std.fmt.bytesToHex(hash, .lower) ++ ".rpa".*;
        const path = try std.fs.path.join(self.allocator, &.{ module_dir, "payload", &name });
        defer self.allocator.free(path);
        self.index_stats.payload_opens += 1;
        const owner = try self.recordOwner(io);
        const record = owner.acquire(path, hash, .native_payload_v1) catch |err| switch (err) {
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
        for (pack.specs) |spec| try self.indexSpec(pack, spec);
    }

    fn indexSpec(self: *LoadedPacks, pack: *const PackFile.Pack, spec: PackFile.SpecEntry) LoadError!void {
        const root = pack.set.artifacts[spec.artifact];
        const identity = switch (root.kind) {
            .proc => |identity| identity,
            .rc_helper, .boxy_thunk, .entrypoint, .message_pool_run, .branch_island => return error.MalformedPack,
        };
        if (self.specs.contains(spec.key)) return;
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

    /// Register the pending store's keyed indexes for visible modules, without
    /// enumerating historical keys or reading any metadata or payload. Runs once, at the first lookup after
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
        if (base.dev_diagnostics.isSet(.pack_stats)) {
            const stats = self.index_stats;
            if (self.record_owner) |owner| std.debug.print("pack acquisitions: {any}\n", .{owner.metrics});
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
            self.allocator.destroy(entry.*);
        }
        self.metadata.deinit(self.allocator);
        self.indexed_file_ids.deinit(self.allocator);
        for (self.index_dirs.items) |dir| self.allocator.free(dir);
        self.index_dirs.deinit(self.allocator);
        self.payloads.deinit(self.allocator);
        self.retained_offers.deinit(self.allocator);
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
        return .{ .context = @ptrCast(self), .find = findSpec, .find_discovery = findDiscovery, .interfaces = if (!base.dev_diagnostics.isSet(.disable_persistent_interfaces)) .{ .context = self, .load = loadInterface, .record = recordInterface } else null };
    }

    fn interfaceStore(self: *const LoadedPacks) ?*const Store {
        // The checked-module session binds its store after the runtime
        // capability is declared, before any specialization task runs.
        return if (self.pending) |pending| pending.store else self.interface_store;
    }

    fn interfacePath(store: *const Store, allocator: Allocator, family: [32]u8) Allocator.Error![]u8 {
        const name = std.fmt.bytesToHex(family, .lower) ++ ".ris".*;
        return std.fs.path.join(allocator, &.{ store.procedure_root, "interfaces", &name });
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
        const path = try interfacePath(store, self.allocator, key);
        defer self.allocator.free(path);
        const bytes = std.Io.Dir.cwd().readFileAlloc(store.roc_ctx.std_io, path, self.allocator, .limited(64 * 1024 * 1024)) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.FileNotFound => {
                family.loaded = true;
                return family;
            },
            else => return family,
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
            const path = try interfacePath(store, self.allocator, entry.key_ptr.*);
            defer self.allocator.free(path);
            _ = store.publishIndexedPath(path, bytes.items) catch |err| switch (err) {
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
        const located = self.artifacts.get(key) orelse return false;
        const marks = self.sourceMarks(located.set, modules) catch return true;
        return marks[located.index];
    }

    fn sourceMarks(self: *LoadedPacks, set: *const backend.dev.ProcArtifact.Set, modules: Common.ProgramModules) Allocator.Error![]bool {
        const slot = try self.lines_outside.getOrPut(self.allocator, ProgramPack.of(set, modules));
        if (!slot.found_existing) slot.value_ptr.* = .empty;
        const first = slot.value_ptr.items.len;
        if (first == set.artifacts.len) return slot.value_ptr.items;
        // Keep the old prefix on allocation failure; retry only the new suffix.
        try slot.value_ptr.resize(self.allocator, set.artifacts.len);
        errdefer slot.value_ptr.items.len = first;
        const marks = slot.value_ptr.items;
        for (set.artifacts[first..], marks[first..]) |artifact, *marked| {
            marked.* = artifact.loc.hasLocation() and !Common.programHasModule(modules, set.source_files[artifact.loc.file]);
            for (artifact.lines) |line| {
                if (line.loc.hasLocation() and !Common.programHasModule(modules, set.source_files[line.loc.file])) marked.* = true;
            }
        }
        try backend.dev.ProcArtifact.markNewReferrers(self.allocator, set, marks, first, .skip);
        return marks;
    }

    fn linkedMarks(self: *LoadedPacks, set: *const backend.dev.ProcArtifact.Set) Allocator.Error![]bool {
        const slot = try self.dev_reaches_linked.getOrPut(self.allocator, set);
        if (!slot.found_existing) slot.value_ptr.* = .empty;
        const first = slot.value_ptr.items.len;
        if (first == set.artifacts.len) return slot.value_ptr.items;
        try slot.value_ptr.resize(self.allocator, set.artifacts.len);
        errdefer slot.value_ptr.items.len = first;
        const marks = slot.value_ptr.items;
        for (set.artifacts[first..], marks[first..]) |artifact, *marked| {
            marked.* = artifact.kind == .proc and self.linked_identities.contains(artifact.kind.proc);
            for (artifact.symbolic_refs) |ref| {
                if (ref.target == .proc and self.linked_identities.contains(ref.target.proc)) marked.* = true;
            }
        }
        try backend.dev.ProcArtifact.markNewReferrers(self.allocator, set, marks, first, .follow);
        return marks;
    }

    /// Whether a dev entry places code supplied by a linked object.
    fn devEntryReachesLinked(self: *LoadedPacks, key: [32]u8) bool {
        if (self.linked_identities.count() == 0) return false;
        const located = self.artifacts.get(key) orelse return false;
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
        // Distinct roots expose any procedure record filed under the exact
        // final-output target instead of the procedure domain.
        const output_root = try std.fs.path.join(allocator, &.{ root, "output" });
        errdefer allocator.free(output_root);
        self.store = .{
            .allocator = allocator,
            .roc_ctx = CoreCtx.default(allocator, self.arena.allocator(), std.testing.io),
            .root = output_root,
            .procedure_root = try std.fs.path.join(allocator, &.{ root, "procedures" }),
            .verbose = false,
        };
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

    fn artifact(id: u8, code: []const u8) backend.dev.ProcArtifact.Artifact {
        return .{ .kind = .{ .proc = lir.ProcIdentity.forTest(id) }, .code = code, .entry = 0, .frame = null, .refs = &.{}, .relocations = &.{}, .data = &.{} };
    }

    fn spec(key: u8, artifact_index: u32) PackFile.SpecEntry {
        return .{ .key = @splat(key), .artifact = artifact_index, .rc_borrowed_params = 3, .rc_ret_borrowed = true, .rc_ret_lenders = 2, .rc_read_only_params = 1, .rc_ret_unique = false, .rc_ret_unique_fields = 0, .rc_ret_conditions = &.{1} };
    }

    fn publish(self: *@This(), artifacts: []const backend.dev.ProcArtifact.Artifact, specs: []const PackFile.SpecEntry, files: []const [32]u8) (Allocator.Error || error{PackWriteFailed})!void {
        try self.store.writeIndexed(.local, identity, &.{ .arena = undefined, .artifacts = artifacts, .source_files = files }, specs);
    }

    fn payloadPath(self: *@This(), artifact_value: backend.dev.ProcArtifact.Artifact) Allocator.Error![]u8 {
        const bytes = try Index.payload(allocator, artifact_value);
        defer allocator.free(bytes);
        const dir = try self.store.identityDir(.local, @splat(0));
        defer allocator.free(dir);
        const name = std.fmt.bytesToHex(Index.digest(bytes), .lower) ++ ".rpa".*;
        return std.fs.path.join(allocator, &.{ dir, "payload", &name });
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
    const artifacts = [_]backend.dev.ProcArtifact.Artifact{ root, callee, F.artifact(3, "symbolic callee"), unused };
    try fixture.publish(&artifacts, &.{ spec, F.spec(200, 3) }, &.{ module, @splat(250) });
    var first = try fixture.loader();
    defer first.deinit();
    const hit = first.specCacheLookup().lookup(spec.key, .dev_code, &.{module}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 3), hit.rc_borrowed_params);
    try std.testing.expectEqual(@as(u32, 9), hit.comptime_root_reads[0].root);
    try std.testing.expectEqualSlices(u32, &.{1}, hit.rc_ret_conditions);
    const located = first.artifacts.get(spec.key).?;
    try std.testing.expectEqual(@as(usize, 3), located.set.artifacts.len);
    try std.testing.expectEqualStrings("constant bytes", located.set.artifacts[located.index].data[0].bytes);
    try std.testing.expectEqual(@as(u32, 4), located.set.artifacts[located.index].lines[0].loc.line);
    const dependency = located.set.artifacts[located.set.artifacts[located.index].refs[0].target];
    try std.testing.expectEqualStrings(dependency.data[0].name, located.set.artifacts[located.index].data[0].relocations[0].name);
    try std.testing.expect(std.mem.startsWith(u8, dependency.data[0].name, "roc__h"));
    try std.testing.expectEqual(@as(u64, 3), first.index_stats.payload_files);
    const before = first.index_stats;
    _ = first.specCacheLookup().lookup(spec.key, .dev_code, &.{module}).?;
    try std.testing.expectEqual(before.payload_opens, first.index_stats.payload_opens);
    try std.testing.expectEqual(before.index_bytes, first.index_stats.index_bytes);
    try std.testing.expectEqual(located.set, first.artifacts.get(spec.key).?.set);

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
            const selected = packs.artifacts.get(spec.key).?;
            try std.testing.expectEqual(files[version], selected.set.source_files[selected.set.artifacts[selected.index].loc.file]);
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
    try std.testing.expect(!missing.artifacts.contains(spec.key));
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
    const located = pinned.artifacts.get(spec.key).?;
    try std.testing.expectEqualStrings("dependency", located.set.artifacts[located.set.artifacts[located.index].refs[0].target].code);
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
    const RefuseIndex = struct {
        fn rename(_: ?*anyopaque, io: std.Io, old: []const u8, new: []const u8) CoreCtx.RenameError!void {
            if (std.mem.endsWith(u8, new, ".rpi")) return error.AccessDenied;
            try std.Io.Dir.cwd().rename(old, std.Io.Dir.cwd(), new, io);
        }
    };
    const real_rename = fixture.store.roc_ctx.vtable.rename;
    fixture.store.roc_ctx.vtable.rename = RefuseIndex.rename;
    try std.testing.expectError(error.PackWriteFailed, fixture.publish(&.{F.artifact(1, "code")}, &.{F.spec(1, 0)}, &.{}));
    fixture.store.roc_ctx.vtable.rename = real_rename;
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
                self.store.writeIndexed(.local, F.identity, &.{ .arena = undefined, .artifacts = &.{F.artifact(self.key, "code")} }, &.{F.spec(self.key, 0)}) catch {
                    self.failed = true;
                    return;
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
        const artifacts = [_]backend.dev.ProcArtifact.Artifact{F.artifact(1, "code")};
        const specs = [_]PackFile.SpecEntry{F.spec(1, 0)};
        try fixture.publish(&artifacts, &specs, &.{});
        const module_dir = try fixture.store.identityDir(.local, @splat(0));
        defer F.allocator.free(module_dir);
        const bucket = if (shared_metadata)
            try std.fs.path.join(F.allocator, &.{ module_dir, "metadata" })
        else
            try std.fs.path.join(F.allocator, &.{ fixture.store.procedure_root, "local", "offers", &std.fmt.bytesToHex(@as([32]u8, @splat(1)), .lower) });
        defer F.allocator.free(bucket);
        var dir = try std.Io.Dir.cwd().openDir(std.testing.io, bucket, .{ .iterate = true });
        defer dir.close(std.testing.io);
        var it = dir.iterate();
        const entry = (try it.next(std.testing.io)).?;
        try dir.writeFile(std.testing.io, .{ .sub_path = entry.name, .data = "truncated metadata" });
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
            const refs = [_]backend.dev.ProcArtifact.Reference{.{ .site = 0, .form = .call, .target = if (swapped) 0 else 1, .delta = 0 }};
            caller.refs = &refs;
            const artifacts = if (swapped) [_]backend.dev.ProcArtifact.Artifact{ callee, caller } else [_]backend.dev.ProcArtifact.Artifact{ caller, callee };
            try fixture.publish(&artifacts, &.{F.spec(1, if (swapped) 1 else 0)}, &.{});
        }
        var packs = try fixture.loader();
        defer packs.deinit();
        _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
        const located = packs.artifacts.get(@splat(1)).?;
        if (selected) |expected| try std.testing.expectEqual(expected, located.index) else selected = located.index;
        try std.testing.expectEqualStrings("caller", located.set.artifacts[located.index].code);
        const ref = located.set.artifacts[located.index].refs[0];
        try std.testing.expectEqualStrings("callee", located.set.artifacts[ref.target].code);
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

test "indexed object cache shares payloads and verifies immutable records before skipping writes" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var first = F.artifact(1, "caller");
    var second = F.artifact(2, "caller");
    first.refs = &.{.{ .site = 0, .form = .call, .target = 2, .delta = 0 }};
    second.refs = first.refs;
    const artifacts = [_]backend.dev.ProcArtifact.Artifact{ first, second, F.artifact(3, "shared dependency") };
    const specs = [_]PackFile.SpecEntry{ F.spec(1, 0), F.spec(2, 1) };
    try fixture.publish(&artifacts, &specs, &.{});
    const RefuseWrites = struct {
        fn rename(_: ?*anyopaque, _: std.Io, _: []const u8, _: []const u8) CoreCtx.RenameError!void {
            return error.AccessDenied;
        }
    };
    fixture.store.roc_ctx.vtable.rename = RefuseWrites.rename;
    try fixture.publish(&artifacts, &specs, &.{});
    var packs = try fixture.loader();
    defer packs.deinit();
    _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    _ = packs.specCacheLookup().lookup(@splat(2), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u64, 3), packs.index_stats.metadata_opens);
    try std.testing.expectEqual(@as(u64, 2), packs.index_stats.payload_files);
    try std.testing.expectEqual(@as(u64, 3), packs.index_stats.decoded_artifacts);
    const left = packs.artifacts.get(@splat(1)).?;
    const right = packs.artifacts.get(@splat(2)).?;
    try std.testing.expectEqual(left.set, right.set);
    try std.testing.expectEqual(left.set.artifacts[left.index].payload_record, right.set.artifacts[right.index].payload_record);
}

test "indexed object cache routes a package offer to producer storage outside its visible providers" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    const provider: compile.BuildEnv.PackPlacement = .{ .origin = .pkg, .identity = @splat(99) };
    try fixture.store.writeIndexedForProviders(.local, F.identity, &.{ .arena = undefined, .artifacts = &.{F.artifact(1, "package code")} }, &.{F.spec(1, 0)}, &.{provider});
    var packs = LoadedPacks.init(F.allocator);
    defer packs.deinit();
    try fixture.store.loadIdentity(std.testing.io, &packs, provider.origin, provider.identity);
    try packs.indexPacks();
    _ = packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqualStrings("package code", packs.artifacts.get(@splat(1)).?.set.artifacts[0].code);
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
    const path = try LoadedPacks.interfacePath(&fixture.store, allocator, family);
    defer allocator.free(path);
    try std.Io.Dir.cwd().writeFile(std.testing.io, .{ .sub_path = path, .data = "RIFACE01truncated" });
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

test "native publication reuses acquired payloads and offers across a one-caller edit" {
    const a = std.testing.allocator;
    const F = IndexFixture;
    const count = 48;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var stats: Store.PublicationStats = .{};
    fixture.store.publication_stats = &stats;
    var artifacts: [count + 1]backend.dev.ProcArtifact.Artifact = undefined;
    var specs: [count]PackFile.SpecEntry = undefined;
    const ref = backend.dev.ProcArtifact.Reference{ .site = 0, .form = .call, .target = count, .delta = 0 };
    for (artifacts[0..count], &specs, 0..) |*artifact, *spec, i| {
        artifact.* = F.artifact(@intCast(i + 1), "unchanged caller");
        artifact.refs = &.{ref};
        spec.* = F.spec(@intCast(i + 1), @intCast(i));
    }
    artifacts[count] = F.artifact(100, "shared leaf");
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
    var combined = try backend.dev.ProcArtifact.combineDistinct(a, &.{loaded.artifacts.get(specs[0].key).?.set});
    defer combined.deinit();
    var closure = try SpliceClosure.init(a, &combined);
    defer closure.deinit();
    for (&specs, 0..) |*spec, i| spec.artifact = closure.offeredProcArtifact(lir.ProcIdentity.forTest(@intCast(i + 1))).?;
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
    const changed = &@constCast(combined.artifacts)[specs[0].artifact];
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
    const edited = fresh.artifacts.get(specs[0].key).?;
    try std.testing.expectEqualStrings("edited caller", edited.set.artifacts[edited.index].code);
}

test "native publication unrelated offers do not change existing records" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var stats: Store.PublicationStats = .{};
    fixture.store.publication_stats = &stats;
    const a = F.artifact(1, "one");
    const b = F.artifact(2, "two");
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
    const artifact = F.artifact(1, "immutable bytes");
    const spec = F.spec(1, 0);
    try fixture.publish(&.{artifact}, &.{spec}, &.{});
    var loaded = try fixture.loader();
    var live = true;
    defer if (live) loaded.deinit();
    _ = loaded.specCacheLookup().lookup(spec.key, .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    const source = loaded.artifacts.get(spec.key).?;
    var copy = try backend.dev.ProcArtifact.combine(F.allocator, &.{source.set});
    var copy_live = true;
    defer if (copy_live) copy.deinit();
    const path = try fixture.payloadPath(artifact);
    defer F.allocator.free(path);
    loaded.deinit();
    live = false;
    try std.testing.expect(!base.retained_cache.remove(std.Io.Dir.cwd(), std.testing.io, path));
    try std.testing.expectEqualStrings("immutable bytes", copy.artifacts[0].code);
    copy.deinit();
    copy_live = false;
    try std.testing.expect(base.retained_cache.remove(std.Io.Dir.cwd(), std.testing.io, path));
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
        fn publish(failing: Allocator) !void {
            var fresh: F = undefined;
            try fresh.init();
            defer fresh.deinit();
            var writer = fresh.store;
            writer.allocator = failing;
            try writer.writeIndexed(.local, F.identity, &.{ .arena = undefined, .artifacts = &.{F.artifact(1, "code")} }, &.{F.spec(1, 0)});
        }
        fn load(failing: Allocator, store: *const Store) !void {
            var packs = LoadedPacks.init(failing);
            defer packs.deinit();
            try store.loadIdentity(std.testing.io, &packs, .local, F.identity);
            try packs.indexPacks();
            try std.testing.expectEqual(LoadedPacks.IndexedResult.hit, try packs.findIndexed(@splat(1), .dev_code, &.{}));
        }
        fn copy(failing: Allocator, set: *const backend.dev.ProcArtifact.Set) !void {
            var owned = try backend.dev.ProcArtifact.combine(failing, &.{set});
            defer owned.deinit();
            try std.testing.expectEqual(set.artifacts[0].payload_record, owned.artifacts[0].payload_record);
        }
    };
    std.testing.checkAllAllocationFailures(F.allocator, Check.publish, .{}) catch |err| {
        std.debug.print("publication allocation check: {}\n", .{err});
        return err;
    };
    try fixture.publish(&.{F.artifact(1, "code")}, &.{F.spec(1, 0)}, &.{});
    try std.testing.checkAllAllocationFailures(F.allocator, Check.load, .{&fixture.store});
    var loaded = try fixture.loader();
    defer loaded.deinit();
    _ = loaded.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    std.testing.checkAllAllocationFailures(F.allocator, Check.copy, .{loaded.artifacts.get(@splat(1)).?.set}) catch |err| {
        std.debug.print("copy allocation check: {}\n", .{err});
        return err;
    };
}

test "native publication union visits shared cyclic dependencies once" {
    const F = IndexFixture;
    const PA = backend.dev.ProcArtifact;
    const count = 128;
    var artifacts: [count]PA.Artifact = undefined;
    var refs: [count]PA.Reference = undefined;
    var specs: [count]PackFile.SpecEntry = undefined;
    for (&artifacts, &refs, &specs, 0..) |*artifact, *ref, *spec, i| {
        ref.* = .{ .site = 0, .form = .call, .target = if (i == 0) 1 else 0, .delta = 0 };
        artifact.* = F.artifact(@intCast(i), "code");
        artifact.refs = @as(*const [1]PA.Reference, ref)[0..];
        spec.* = F.spec(@intCast(i), @intCast(i));
    }
    var closure = try SpliceClosure.init(F.allocator, &.{ .arena = undefined, .artifacts = &artifacts });
    defer closure.deinit();
    const union_indices = try closure.ofEntries(&specs);
    try std.testing.expectEqual(@as(usize, count), union_indices.len);
    try std.testing.expectEqual(@as(usize, count), closure.nodes_visited);
    try std.testing.expectEqual(@as(usize, count), closure.edges_visited);
    try std.testing.expectEqual(@as(usize, 1), closure.queries);
}

test "native publication shares a cyclic component and invalidates exact dependency endpoints" {
    const F = IndexFixture;
    const PA = backend.dev.ProcArtifact;
    const callers = 32;
    const cycle = 16;
    const count = callers + cycle;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var stats: Store.PublicationStats = .{};
    fixture.store.publication_stats = &stats;
    var artifacts: [count]PA.Artifact = undefined;
    var refs: [count]PA.Reference = undefined;
    var specs: [callers]PackFile.SpecEntry = undefined;
    for (&artifacts, &refs, 0..) |*artifact, *ref, i| {
        artifact.* = F.artifact(@intCast(i), "identical payload");
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
    const set = loaded.artifacts.get(specs[0].key).?.set;
    for (specs) |spec| try std.testing.expectEqual(set, loaded.artifacts.get(spec.key).?.set);
    try std.testing.expectEqual(@as(usize, count), set.artifacts.len);
    try std.testing.expectEqual(@as(u64, count), loaded.index_stats.decoded_artifacts);
    try std.testing.expectEqual(@as(u64, callers + 1), loaded.index_stats.metadata_opens);
    try std.testing.expectEqual(@as(u64, 1), loaded.index_stats.payload_files);
    var owned = try PA.combine(F.allocator, &.{set});
    defer owned.deinit();
    for (&specs) |*spec| spec.artifact = loaded.artifacts.get(spec.key).?.index;
    stats = .{};
    try fixture.store.writeIndexedRetained(.local, F.identity, &owned, &specs, null, &loaded);
    try std.testing.expectEqual(@as(u64, 0), stats.bytes_hashed);
    try std.testing.expectEqual(@as(u64, 0), stats.records_read);
    try std.testing.expectEqual(@as(u64, 0), stats.records_written);
    // Identical code and payload identity do not authorize an old graph edge.
    const root = &@constCast(owned.artifacts)[specs[0].artifact];
    const previous = root.refs[0].target;
    const next = owned.artifacts[previous].refs[0].target;
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
    const hit = fresh.artifacts.get(specs[0].key).?;
    const dependency = hit.set.artifacts[hit.set.artifacts[hit.index].refs[0].target];
    try std.testing.expectEqual(owned.artifacts[next].kind.proc, dependency.kind.proc);
}

test "native publication pins dependencies until all offers are visible" {
    const F = IndexFixture;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    const payload_path = try fixture.payloadPath(F.artifact(1, "code"));
    defer F.allocator.free(payload_path);
    const Cleaner = struct {
        payload: []const u8,
        attempts: usize = 0,
        write: @TypeOf(@as(CoreCtx, undefined).vtable.writeTempFile),
        fn stage(ctx: ?*anyopaque, io: std.Io, allocator: Allocator, path: []const u8, bytes: []const u8) (Allocator.Error || CoreCtx.WriteError)![]u8 {
            const self: *@This() = @ptrCast(@alignCast(ctx.?));
            if (std.mem.endsWith(u8, path, ".rpm") or std.mem.endsWith(u8, path, ".rpi")) {
                self.attempts += 1;
                if (base.retained_cache.remove(std.Io.Dir.cwd(), io, self.payload)) return error.AccessDenied;
            }
            return self.write(null, io, allocator, path, bytes);
        }
    };
    var cleaner = Cleaner{ .payload = payload_path, .write = fixture.store.roc_ctx.vtable.writeTempFile };
    fixture.store.roc_ctx.ctx = &cleaner;
    fixture.store.roc_ctx.vtable.writeTempFile = Cleaner.stage;
    try fixture.publish(&.{F.artifact(1, "code")}, &.{F.spec(1, 0)}, &.{});
    try std.testing.expectEqual(@as(usize, 2), cleaner.attempts);
    try std.testing.expect(base.retained_cache.remove(std.Io.Dir.cwd(), std.testing.io, payload_path));
}

test "native publication failure at every record boundary exposes only complete offers" {
    const F = IndexFixture;
    const Fail = struct {
        suffix: []const u8,
        fn rename(ctx: ?*anyopaque, io: std.Io, old: []const u8, new: []const u8) CoreCtx.RenameError!void {
            const self: *const @This() = @ptrCast(@alignCast(ctx.?));
            if (std.mem.endsWith(u8, new, self.suffix)) return error.AccessDenied;
            try std.Io.Dir.cwd().rename(old, std.Io.Dir.cwd(), new, io);
        }
    };
    for ([_][]const u8{ ".rpa", ".rpm", ".rpi" }) |suffix| {
        var fixture: F = undefined;
        try fixture.init();
        defer fixture.deinit();
        var fail = Fail{ .suffix = suffix };
        fixture.store.roc_ctx.ctx = &fail;
        fixture.store.roc_ctx.vtable.rename = Fail.rename;
        try std.testing.expectError(error.PackWriteFailed, fixture.publish(&.{F.artifact(1, "code")}, &.{F.spec(1, 0)}, &.{}));
        var packs = try fixture.loader();
        defer packs.deinit();
        try std.testing.expect(packs.specCacheLookup().lookup(@splat(1), .dev_code, &.{}) == null);
        try std.testing.expect(!try fixture.store.hasIndexed(.local, F.identity, @splat(1)));
    }
}

test "native publication preserves symbolic bindings across historical definitions" {
    const F = IndexFixture;
    const PA = backend.dev.ProcArtifact;
    var fixture: F = undefined;
    try fixture.init();
    defer fixture.deinit();
    var first = F.artifact(1, "first caller");
    var second = F.artifact(3, "second caller");
    first.symbolic_refs = &.{.{ .site = 0, .form = .call, .target = .{ .proc = lir.ProcIdentity.forTest(2) } }};
    second.symbolic_refs = first.symbolic_refs;
    try fixture.publish(&.{ first, F.artifact(2, "old definition") }, &.{F.spec(1, 0)}, &.{});
    try fixture.publish(&.{ second, F.artifact(2, "new definition") }, &.{F.spec(3, 0)}, &.{});
    var loaded = try fixture.loader();
    defer loaded.deinit();
    for ([_]u8{ 3, 1 }) |key| _ = loaded.specCacheLookup().lookup(@splat(key), .dev_code, &.{}) orelse return error.TestUnexpectedResult;
    const old = loaded.artifacts.get(@splat(1)).?;
    const new = loaded.artifacts.get(@splat(3)).?;
    try std.testing.expectEqual(old.set, new.set);
    const old_target = old.set.symbolicDefinition(old.set.artifacts[old.index].symbolic_refs[0]).?;
    const new_target = new.set.symbolicDefinition(new.set.artifacts[new.index].symbolic_refs[0]).?;
    try std.testing.expectEqualStrings("old definition", old.set.artifacts[old_target].code);
    try std.testing.expectEqualStrings("new definition", new.set.artifacts[new_target].code);
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
    @constCast(captured.artifacts[old.index].symbolic_refs)[0].binding = .{ .local = new_target };
    stats = .{};
    var changed = specs[0];
    changed.key = @splat(4);
    try fixture.store.writeIndexedRetained(.local, F.identity, &captured, &.{changed}, null, &loaded);
    try std.testing.expectEqual(@as(u64, 0), stats.payloads_encoded);
    try std.testing.expectEqual(@as(u64, 1), stats.metadata_encoded);
    try std.testing.expectEqual(@as(u64, 2), stats.records_written);
}
