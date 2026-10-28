//! Native, indexed artifact storage. All payloads, including unfinished ones,
//! have a catalog slot before creation. Shard locks protect only metadata and
//! acquisition; payload production never holds a catalog lock.

const std = @import("std");
const base = @import("base");
pub const catalog = @import("cache_index.zig");
const Io = std.Io;
const Dir = Io.Dir;
const Allocator = std.mem.Allocator;
const put = catalog.put;
const get = catalog.get;

/// Storage-format namespace shared by all compiler compatibility identities.
pub const format_directory = "artifacts-v2";
/// Fixed number of independently coordinated catalogs.
pub const shard_count = 16;
/// One minute in the timestamp units used by retention.
pub const minute_ns: i64 = 60 * std.time.ns_per_s;
/// One day in the timestamp units used by retention.
pub const day_ns: i64 = 24 * 60 * minute_ns;

/// Exhaustive retention policy for compiler-owned artifacts. Sources materialized
/// for tooling, downloaded packages, and installed applications are not artifacts.
pub const Family = enum(u8) {
    checked,
    canonicalized,
    placement,
    literals,
    pairing,
    executable,
    test_result,
    wasm_host,
    glue_plugin,
    local_pack,
    package_pack,
    optimized_pack,
    local_discovery,
    package_discovery,
    program_output,
    scratch,

    pub fn age(self: Family) i64 {
        return switch (self) {
            .scratch => 5 * minute_ns,
            .local_pack, .local_discovery, .program_output => day_ns,
            .checked,
            .canonicalized,
            .placement,
            .literals,
            .pairing,
            .executable,
            .test_result,
            .wasm_host,
            .glue_plugin,
            .package_pack,
            .optimized_pack,
            .package_discovery,
            => 30 * day_ns,
        };
    }

    pub fn isPack(self: Family) bool {
        return switch (self) {
            .local_pack, .package_pack, .optimized_pack, .local_discovery, .package_discovery, .program_output => true,
            .checked, .canonicalized, .placement, .literals, .pairing, .executable, .test_result, .wasm_host, .glue_plugin, .scratch => false,
        };
    }

    fn expiresByUse(self: Family) bool {
        return self.isPack();
    }
};

/// Explicit semantic identity, independent of physical payload placement.
pub const Identity = struct {
    namespace: [32]u8,
    family: Family,
    group: [32]u8 = @splat(0),
    key: [32]u8,
    pack_format: u32 = 0,
    variant: [32]u8 = @splat(0),

    pub fn indexedKey(self: Identity) catalog.Key {
        var hash = base.Sha256.init(.{});
        hash.update("roc-artifact-group-v2");
        hash.update(&self.namespace);
        hash.update(&self.variant);
        hash.update(&.{@backingInt(self.family)});
        hash.update(&self.group);
        var result: catalog.Key = undefined;
        hash.final(result[0..32]);
        @memcpy(result[32..], &self.key);
        return result;
    }
};

/// Recoverable storage, coordination, and allocation failures.
pub const Error = catalog.Error || error{ Busy, Missing, Changed, InvalidArtifact, PathTooLong, OutOfMemory };

/// Hash explicit identity components with SHA-256.
pub fn hashBytes(bytes: []const u8) [32]u8 {
    var result: [32]u8 = undefined;
    base.Sha256.hash(bytes, &result, .{});
    return result;
}

/// Publication and reclamation phases of an owned physical slot.
pub const State = enum(u8) { staging, published, retiring };

/// Persisted ownership, retention, and accounting for one payload generation.
pub const Metadata = struct {
    state: State,
    family: Family,
    pack_format: u32,
    namespace: [32]u8,
    generation: u64,
    size: u64,
    created: i64,
    used: i64,
    /// Lower bound published in the recency tree. Only maintenance advances it.
    indexed_used: i64,
    key: catalog.Key,
    slot: u64,
    variant: [32]u8 = @splat(0),

    pub fn encode(self: Metadata) catalog.Value {
        var bytes: catalog.Value = @splat(0);
        @memcpy(bytes[0..8], "ROCART02");
        bytes[8] = @backingInt(self.state);
        bytes[9] = @backingInt(self.family);
        put(u32, &bytes, 12, self.pack_format);
        @memcpy(bytes[16..48], &self.namespace);
        put(u64, &bytes, 48, self.generation);
        put(u64, &bytes, 56, self.size);
        put(i64, &bytes, 64, self.created);
        put(i64, &bytes, 72, self.used);
        @memcpy(bytes[80..144], &self.key);
        put(u64, &bytes, 144, self.slot);
        @memcpy(bytes[152..184], &self.variant);
        put(i64, &bytes, 184, self.indexed_used);
        return bytes;
    }

    pub fn decode(bytes: catalog.Value) Error!Metadata {
        if (!std.mem.eql(u8, bytes[0..8], "ROCART02") or get(i64, &bytes, 184) > get(i64, &bytes, 72)) return error.CorruptCatalog;
        return .{
            .state = std.enums.fromInt(State, bytes[8]) orelse return error.CorruptCatalog,
            .family = std.enums.fromInt(Family, bytes[9]) orelse return error.CorruptCatalog,
            .pack_format = get(u32, &bytes, 12),
            .namespace = bytes[16..48].*,
            .generation = get(u64, &bytes, 48),
            .size = get(u64, &bytes, 56),
            .created = get(i64, &bytes, 64),
            .used = get(i64, &bytes, 72),
            .indexed_used = get(i64, &bytes, 184),
            .key = bytes[80..144].*,
            .slot = get(u64, &bytes, 144),
            .variant = bytes[152..184].*,
        };
    }

    pub fn expired(self: Metadata, now: i64, pack_format: u32) bool {
        if (self.family.isPack() and self.pack_format != pack_format) return true;
        const stamp = if (self.state == .staging or !self.family.expiresByUse()) self.created else self.used;
        const age: i128 = @as(i128, now) - stamp;
        return age > if (self.state == .staging) 5 * minute_ns else self.family.age();
    }

    pub fn recency(self: Metadata, slot: u64) catalog.Key {
        var key: catalog.Key = @splat(0);
        std.mem.writeInt(u64, key[0..8], @as(u64, @bitCast(self.indexed_used)) ^ (@as(u64, 1) << 63), .big);
        std.mem.writeInt(u64, key[8..16], slot, .big);
        return key;
    }
};

/// Shared artifact namespace; operations acquire individual shard ownership.
pub const Store = struct {
    allocator: Allocator,
    io: Io,
    root: []u8,

    pub fn init(allocator: Allocator, io: Io, cache_root: []const u8) Allocator.Error!Store {
        return .{ .allocator = allocator, .io = io, .root = try std.fs.path.join(allocator, &.{ cache_root, format_directory }) };
    }

    pub fn deinit(self: *Store) void {
        self.allocator.free(self.root);
    }

    pub fn now(self: *const Store) i64 {
        return @intCast(std.math.clamp(Io.Timestamp.now(self.io, .real).nanoseconds, std.math.minInt(i64), std.math.maxInt(i64)));
    }

    pub fn openRoot(self: *const Store, create: bool) Error!Dir {
        if (create) Dir.cwd().createDirPath(self.io, self.root) catch return error.IoFailure;
        return Dir.cwd().openDir(self.io, self.root, .{}) catch |err| switch (err) {
            error.FileNotFound => error.Missing,
            else => error.IoFailure,
        };
    }

    pub fn shardFor(key: catalog.Key) u4 {
        return @truncate(key[0]);
    }

    pub fn reserve(self: *const Store, identity: Identity, size: u64, timestamp: i64) Error!Staging {
        return self.reserveWith(identity, size, timestamp, true);
    }

    fn reserveWith(self: *const Store, identity: Identity, size: u64, timestamp: i64, required: bool) Error!Staging {
        const key = identity.indexedKey();
        var counters = catalog.Counters{};
        var shard = try Shard.openWith(self, shardFor(key), true, &counters, required);
        defer shard.close();
        var tx = shard.index.transaction();
        if (tx.header.user[0] == std.math.maxInt(u64)) return error.CapacityExceeded;
        tx.header.user[0] += 1;
        const generation = tx.header.user[0];
        var metadata: Metadata = .{
            .state = .staging,
            .family = identity.family,
            .pack_format = identity.pack_format,
            .namespace = identity.namespace,
            .variant = identity.variant,
            .generation = generation,
            .size = size,
            .created = timestamp,
            .used = timestamp,
            .indexed_used = timestamp,
            .key = key,
            .slot = 0,
        };
        const slot = try tx.putValue(.owners, storageKey(generation), metadata.encode());
        metadata.slot = slot;
        _ = try tx.putValue(.owners, storageKey(generation), metadata.encode());
        var path_buffer: [128]u8 = undefined;
        const stem = try slotStem(&path_buffer, slot);
        // Directory fanout is fixed by ID bytes, independent of artifact names.
        shard.dir.createDirPath(self.io, std.fs.path.dirname(stem).?) catch return error.IoFailure;
        var lease_buffer: [140]u8 = undefined;
        const lease_path = std.fmt.bufPrint(&lease_buffer, "{s}.lease", .{stem}) catch return error.PathTooLong;
        const lease = try leaseFile(shard.dir, self.io, lease_path, false, true);
        errdefer lease.close(self.io);
        tx.header.user[2] = std.math.add(u64, tx.header.user[2], size) catch return error.CapacityExceeded;
        try tx.commit();
        const path = try self.payloadPath(shard.number, slot, ".pending");
        return .{ .store = self, .key = key, .slot = slot, .generation = generation, .lease = lease, .path = path };
    }

    pub fn write(self: *const Store, identity: Identity, bytes: []const u8, timestamp: i64) Error!void {
        var staging = try self.reserve(identity, bytes.len, timestamp);
        defer staging.deinit();
        const file = Dir.cwd().createFile(self.io, staging.path, .{ .exclusive = true }) catch return error.IoFailure;
        {
            defer file.close(self.io);
            file.writeStreamingAll(self.io, bytes) catch return error.IoFailure;
        }
        try staging.publish();
    }

    /// Copy into our registered staging name directly. Generic atomic-copy
    /// helpers may create unregistered sibling temporary files on interruption.
    pub fn importFile(self: *const Store, identity: Identity, source_path: []const u8) Error!void {
        const source = Dir.cwd().openFile(self.io, source_path, .{}) catch return error.IoFailure;
        defer source.close(self.io);
        const info = source.stat(self.io) catch return error.IoFailure;
        if (info.kind != .file) return error.InvalidArtifact;
        var staging = try self.reserve(identity, info.size, self.now());
        defer staging.deinit();
        {
            const destination = Dir.cwd().createFile(self.io, staging.path, .{ .exclusive = true, .permissions = info.permissions }) catch return error.IoFailure;
            defer destination.close(self.io);
            var buffer: [64 * 1024]u8 = undefined;
            var offset: u64 = 0;
            while (offset < info.size) {
                const chunk = buffer[0..@min(buffer.len, info.size - offset)];
                const count = source.readPositionalAll(self.io, chunk, offset) catch return error.IoFailure;
                if (count != chunk.len) return error.InvalidArtifact;
                destination.writePositionalAll(self.io, chunk, offset) catch return error.IoFailure;
                offset += count;
            }
        }
        try staging.publish();
    }

    pub fn acquire(self: *const Store, identity: Identity, timestamp: i64) Error!?Artifact {
        return self.acquireKey(identity.indexedKey(), timestamp);
    }

    pub fn acquireKey(self: *const Store, key: catalog.Key, timestamp: i64) Error!?Artifact {
        return self.acquireKeyObserved(key, timestamp, null);
    }

    fn acquireKeyObserved(self: *const Store, key: catalog.Key, timestamp: i64, deferred: ?*?RetentionBatch.Observation) Error!?Artifact {
        var counters = catalog.Counters{};
        var shard = Shard.openRequired(self, shardFor(key), false, &counters) catch |err| switch (err) {
            error.Missing, error.Busy => return null,
            else => return err,
        };
        defer shard.close();
        return self.acquireFromShard(key, timestamp, deferred, &shard);
    }

    fn acquireFromShard(self: *const Store, key: catalog.Key, timestamp: i64, deferred: ?*?RetentionBatch.Observation, shard: *Shard) Error!?Artifact {
        var tx = shard.index.transaction();
        const found = (try tx.find(.artifacts, key)) orelse return null;
        var metadata = try currentMetadata(&tx, key, found.value);
        if (metadata.state != .published or metadata.family == .scratch) return null;
        const path = try self.payloadPath(shard.number, metadata.slot, ".data");
        errdefer self.allocator.free(path);
        var stem_buffer: [128]u8 = undefined;
        const stem = try slotStem(&stem_buffer, metadata.slot);
        var lease_buffer: [140]u8 = undefined;
        const relative_lease = std.fmt.bufPrint(&lease_buffer, "{s}.lease", .{stem}) catch return error.PathTooLong;
        const lease = leaseFile(shard.dir, self.io, relative_lease, true, false) catch |err| switch (err) {
            error.Missing, error.Busy => {
                self.allocator.free(path);
                return null;
            },
            else => return err,
        };
        errdefer lease.close(self.io);
        var data_buffer: [140]u8 = undefined;
        const relative_data = std.fmt.bufPrint(&data_buffer, "{s}.data", .{stem}) catch return error.PathTooLong;
        const file = shard.dir.openFile(self.io, relative_data, .{}) catch |err| {
            return switch (err) {
                error.FileNotFound => blk: {
                    self.allocator.free(path);
                    lease.close(self.io);
                    break :blk null;
                },
                else => error.IoFailure,
            };
        };
        errdefer file.close(self.io);
        // Acquire storage before releasing metadata protection. An accepted
        // reader owns this open file even if maintenance unlinks its name.
        if (metadata.used < timestamp) {
            if (deferred) |observation| {
                observation.* = .{ .key = key, .slot = metadata.slot, .generation = metadata.generation, .timestamp = timestamp };
            } else {
                metadata.used = timestamp;
                try tx.replaceValue(metadata.slot, storageKey(metadata.generation), metadata.encode());
                try tx.commit();
            }
        }
        const actual_size = file.length(self.io) catch return error.IoFailure;
        return .{ .store = self, .file = file, .lease = lease, .path = path, .size = actual_size };
    }

    /// Invalidate a rejected cache offer under the same generation validation
    /// used by maintenance. A reader holding its lease keeps accepted storage.
    pub fn invalidate(self: *const Store, identity: Identity) Error!void {
        const key = identity.indexedKey();
        var counters = catalog.Counters{};
        var shard = try Shard.openRequired(self, shardFor(key), false, &counters);
        defer shard.close();
        var tx = shard.index.transaction();
        const entry = (try tx.find(.artifacts, key)) orelse return;
        const metadata = try currentMetadata(&tx, key, entry.value);
        _ = try shard.retire(key, metadata.generation);
    }

    pub fn read(self: *const Store, allocator: Allocator, identity: Identity, timestamp: i64) Error!?[]u8 {
        var artifact = (try self.acquire(identity, timestamp)) orelse return null;
        defer artifact.deinit();
        return try artifact.read(allocator);
    }

    /// Group placement is explicit in the indexed key. Enumerating one group
    /// visits only its shard, under one metadata transaction; unrelated groups
    /// and historical compiler namespaces require no probes.
    pub fn groupKeys(self: *const Store, allocator: Allocator, identity: Identity) Error![]catalog.Key {
        var keys: std.ArrayList(catalog.Key) = .empty;
        errdefer keys.deinit(allocator);
        const group_key = identity.indexedKey();
        var counters = catalog.Counters{};
        var shard = Shard.openRequired(self, shardFor(group_key), false, &counters) catch |err| switch (err) {
            error.Missing => return keys.toOwnedSlice(allocator),
            else => return err,
        };
        defer shard.close();
        return groupKeysInShard(allocator, group_key, &shard);
    }

    fn groupKeysInShard(allocator: Allocator, group_key: catalog.Key, shard: *Shard) Error![]catalog.Key {
        var keys: std.ArrayList(catalog.Key) = .empty;
        errdefer keys.deinit(allocator);
        var tx = shard.index.transaction();
        var cursor: catalog.Key = @splat(0);
        @memcpy(cursor[0..32], group_key[0..32]);
        while (try tx.lowerBound(.artifacts, cursor)) |entry| {
            if (!std.mem.eql(u8, entry.key[0..32], group_key[0..32])) break;
            const metadata = try Metadata.decode(entry.value);
            if (metadata.state == .published) try keys.append(allocator, entry.key);
            cursor = entry.key;
            if (!advanceKey(&cursor)) break;
        }
        return keys.toOwnedSlice(allocator);
    }

    pub fn payloadPath(self: *const Store, shard: u4, slot: u64, suffix: []const u8) Error![]u8 {
        var stem_buffer: [128]u8 = undefined;
        const stem = try slotStem(&stem_buffer, slot);
        return std.fmt.allocPrint(self.allocator, "{s}/{x:0>2}/{s}{s}", .{ self.root, shard, stem, suffix });
    }
};

/// Bounded retention observations for a reader retaining generation leases.
/// Callers flush before releasing their leases. A failed acquisition may close
/// its lease early; exact generation validation discards an obsolete observation.
/// The batch owns its store path and may outlive the Store that created it.
pub const RetentionBatch = struct {
    const Observation = struct { key: catalog.Key, slot: u64, generation: u64, timestamp: i64 };
    store: Store,
    pending: [shard_count][catalog.max_changes]Observation = undefined,
    counts: [shard_count]u8 = @splat(0),
    shards: [shard_count]?Shard = @splat(null),
    caches: [shard_count]catalog.ReadCache = @splat(.{}),
    counters: catalog.Counters = .{},

    pub fn init(store: *const Store) Allocator.Error!RetentionBatch {
        return .{ .store = .{ .allocator = store.allocator, .io = store.io, .root = try store.allocator.dupe(u8, store.root) } };
    }

    pub fn deinit(self: *RetentionBatch) void {
        // Retention publication is optional cache I/O. A failed write never
        // turns validated code into a failed compilation; leases remain held
        // until this flush attempt has finished.
        self.flush() catch {};
        for (&self.shards) |*entry| if (entry.*) |*shard| shard.close();
        self.store.deinit();
    }

    pub fn acquireKey(self: *RetentionBatch, key: catalog.Key, timestamp: i64) Error!?Artifact {
        const number = Store.shardFor(key);
        if (self.counts[number] == catalog.max_changes) try self.flushShard(number);
        var observation: ?Observation = null;
        const shard = self.acquireShard(number) catch |err| switch (err) {
            error.Missing => return null,
            else => return err,
        };
        defer shard.lock.unlock(self.store.io);
        const artifact = try self.store.acquireFromShard(key, timestamp, &observation, shard);
        if (observation) |value| {
            self.pending[number][self.counts[number]] = value;
            self.counts[number] += 1;
        }
        return artifact;
    }

    /// Acquire a producer-supplied group under bounded shard ownership. Each
    /// result still owns its individual generation lease and open payload.
    /// The output has the same order as keys; absence remains per-record.
    pub fn acquireKeys(self: *RetentionBatch, allocator: Allocator, keys: []const catalog.Key, timestamp: i64) Error![]?Artifact {
        const result = try allocator.alloc(?Artifact, keys.len);
        errdefer allocator.free(result);
        @memset(result, null);
        errdefer for (result) |*item| if (item.*) |*artifact| artifact.deinit();
        const order = try allocator.alloc(usize, keys.len);
        defer allocator.free(order);
        var counts: [shard_count]usize = @splat(0);
        for (keys) |key| counts[Store.shardFor(key)] += 1;
        var starts: [shard_count + 1]usize = @splat(0);
        for (counts, 0..) |count, i| starts[i + 1] = starts[i] + count;
        var cursors = starts[0..shard_count].*;
        for (keys, 0..) |key, i| {
            const number = Store.shardFor(key);
            order[cursors[number]] = i;
            cursors[number] += 1;
        }
        for (0..shard_count) |raw_number| {
            const number: u4 = @intCast(raw_number);
            var next = starts[number];
            const end = starts[raw_number + 1];
            while (next < end) {
                if (self.counts[number] == catalog.max_changes) try self.flushShard(number);
                const shard = self.acquireShard(number) catch |err| switch (err) {
                    error.Missing => break,
                    else => return err,
                };
                defer shard.lock.unlock(self.store.io);
                // Reuse the retention transaction's existing bound, also when
                // every item is absent or already has a current timestamp.
                const limit = next + @min(end - next, catalog.max_changes - self.counts[number]);
                while (next < limit) : (next += 1) {
                    const index = order[next];
                    var observation: ?Observation = null;
                    result[index] = try self.store.acquireFromShard(keys[index], timestamp, &observation, shard);
                    if (observation) |value| {
                        self.pending[number][self.counts[number]] = value;
                        self.counts[number] += 1;
                    }
                }
            }
        }
        return result;
    }

    fn acquireShard(self: *RetentionBatch, number: u4) Error!*Shard {
        if (self.shards[number]) |*shard| {
            shard.lock.lock(self.store.io, .exclusive) catch return error.IoFailure;
            errdefer shard.lock.unlock(self.store.io);
            try shard.index.refresh();
            return shard;
        }
        var shard = try Shard.openRequired(&self.store, number, false, &self.counters);
        shard.index.read_cache = &self.caches[number];
        self.shards[number] = shard;
        return &self.shards[number].?;
    }

    pub fn groupKeys(self: *RetentionBatch, allocator: Allocator, identity: Identity) Error![]catalog.Key {
        const group_key = identity.indexedKey();
        const shard = self.acquireShard(Store.shardFor(group_key)) catch |err| switch (err) {
            error.Missing => return allocator.alloc(catalog.Key, 0),
            else => return err,
        };
        defer shard.lock.unlock(self.store.io);
        return Store.groupKeysInShard(allocator, group_key, shard);
    }

    pub fn flush(self: *RetentionBatch) Error!void {
        for (0..shard_count) |number| try self.flushShard(@intCast(number));
    }

    fn flushShard(self: *RetentionBatch, number: u4) Error!void {
        const count = self.counts[number];
        if (count == 0) return;
        const shard = try self.acquireShard(number);
        defer shard.lock.unlock(self.store.io);
        var tx = shard.index.transaction();
        for (self.pending[number][0..count]) |observation| {
            // A rejected payload may have released its lease. Never apply that
            // observation to a replacement generation reusing the same slot.
            if (observation.slot == 0 or observation.slot >= tx.header.next) continue;
            const row = try tx.node(observation.slot);
            const expected = storageKey(observation.generation);
            if (row.kind != .leaf or !std.mem.eql(u8, &row.key, &expected)) continue;
            var metadata = try Metadata.decode(row.value);
            if (metadata.slot != observation.slot or metadata.generation != observation.generation or
                !std.mem.eql(u8, &metadata.key, &observation.key)) return error.CorruptCatalog;
            if (metadata.state != .published or metadata.used >= observation.timestamp) continue;
            metadata.used = observation.timestamp;
            try tx.replaceValue(metadata.slot, expected, metadata.encode());
        }
        if (tx.count != 0) try tx.commit();
        self.counts[number] = 0;
    }
};

/// The artifact index supplies the immutable generation and physical owner slot.
/// Its copied retention fields are not authoritative. Reads and maintenance use
/// the owner leaf, whose use timestamp a foreground hit updates in one write.
pub fn currentMetadata(tx: *catalog.Transaction, key: catalog.Key, pointer: catalog.Value) Error!Metadata {
    const address = try Metadata.decode(pointer);
    if (!std.mem.eql(u8, &address.key, &key)) return error.CorruptCatalog;
    const owner = try tx.node(address.slot);
    const expected = storageKey(address.generation);
    if (owner.kind != .leaf or !std.mem.eql(u8, &owner.key, &expected)) return error.CorruptCatalog;
    const metadata = try Metadata.decode(owner.value);
    if (metadata.slot != address.slot or metadata.generation != address.generation or
        !std.mem.eql(u8, &metadata.key, &key)) return error.CorruptCatalog;
    return metadata;
}

test "artifact reads update only the authoritative owner and replacement uses repaired recency" {
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(path);
    var store = try Store.init(a, io, path);
    defer store.deinit();
    const identity: Identity = .{ .namespace = @splat(1), .family = .local_pack, .key = @splat(2) };
    const key = identity.indexedKey();
    try store.write(identity, "old", 10);
    {
        var read = (try store.acquire(identity, 100)).?;
        defer read.deinit();
    }
    {
        var counters = catalog.Counters{};
        var shard = try Shard.openRequired(&store, Store.shardFor(key), false, &counters);
        defer shard.close();
        var tx = shard.index.transaction();
        const pointer = (try tx.find(.artifacts, key)).?.value;
        try std.testing.expectEqual(10, (try Metadata.decode(pointer)).used);
        const current = try currentMetadata(&tx, key, pointer);
        try std.testing.expectEqual(100, current.used);
        try std.testing.expectEqual(10, current.indexed_used);
        try std.testing.expect(!current.expired(day_ns + 50, identity.pack_format));
        try shard.refreshRecency(current);
        var refreshed = shard.index.transaction();
        const after = try currentMetadata(&refreshed, key, pointer);
        try std.testing.expectEqual(100, after.indexed_used);
        var next = after;
        next.used = 200;
        try refreshed.replaceValue(next.slot, storageKey(next.generation), next.encode());
        try std.testing.expectEqual(@as(usize, 1), refreshed.count);
        try refreshed.commit();
    }
    try store.write(identity, "new", 300);
    const bytes = (try store.read(a, identity, 400)).?;
    defer a.free(bytes);
    try std.testing.expectEqualStrings("new", bytes);
}

/// POSIX flock belongs to the open file description and survives inheritance.
/// Windows byte-range locks belong to the locking process; use sharing denial
/// for leases instead, which lasts until the last inherited handle is closed.
fn leaseFile(dir: Dir, io: Io, path: []const u8, shared: bool, create: bool) Error!Io.File {
    if (comptime @import("builtin").os.tag == .windows) {
        const w = std.os.windows;
        var wide: [140]u16 = undefined;
        if (path.len > wide.len) return error.PathTooLong;
        for (path, 0..) |byte, i| wide[i] = if (byte == '/') '\\' else byte;
        var status: w.IO_STATUS_BLOCK = undefined;
        var handle: w.HANDLE = undefined;
        const result = w.ntdll.NtCreateFile(&handle, .{ .STANDARD = .{ .SYNCHRONIZE = true }, .GENERIC = .{ .READ = true, .WRITE = !shared } }, &.{ .RootDirectory = dir.handle, .ObjectName = @constCast(&w.UNICODE_STRING.init(wide[0..path.len])) }, &status, null, .{ .NORMAL = true }, if (shared) .{ .READ = true } else .{}, if (create) .OPEN_IF else .OPEN, .{ .NON_DIRECTORY_FILE = true, .IO = .SYNCHRONOUS_NONALERT }, null, 0);
        if (result == .SUCCESS) return .{ .handle = handle, .flags = .{ .nonblocking = false } };
        if (result == .SHARING_VIOLATION) return error.Busy;
        if (result == .OBJECT_NAME_NOT_FOUND or result == .OBJECT_PATH_NOT_FOUND) return error.Missing;
        return error.IoFailure;
    } else {
        return (if (create)
            dir.createFile(io, path, .{ .read = true, .truncate = false, .lock = .exclusive, .lock_nonblocking = true })
        else
            dir.openFile(io, path, .{ .mode = if (shared) .read_only else .read_write, .lock = if (shared) .shared else .exclusive, .lock_nonblocking = true })) catch |err| switch (err) {
            error.WouldBlock => error.Busy,
            error.FileNotFound => error.Missing,
            else => error.IoFailure,
        };
    }
}

/// A catalog and its held kernel coordination lock.
pub const Shard = struct {
    io: Io,
    dir: Dir,
    lock: Io.File,
    index: catalog.Index,
    number: u4,

    /// Repair one stale recency lower bound. This is one bounded maintenance
    /// unit, separate from eviction; a freshly read artifact is never selected
    /// using the older position it still occupies in the recency index.
    pub fn refreshRecency(self: *Shard, metadata: Metadata) Error!void {
        var tx = self.index.transaction();
        var current = metadata;
        if (!try tx.remove(.recency, current.recency(current.slot))) return error.CorruptCatalog;
        current.indexed_used = current.used;
        try tx.replaceValue(current.slot, storageKey(current.generation), current.encode());
        var value: catalog.Value = @splat(0);
        @memcpy(value[0..64], &current.key);
        _ = try tx.putValue(.recency, current.recency(current.slot), value);
        try tx.commit();
    }

    /// Opportunistic maintenance never waits for a foreground transaction.
    pub fn open(store: *const Store, number: u4, create: bool, counters: *catalog.Counters) Error!Shard {
        return openWith(store, number, create, counters, false);
    }

    /// Foreground readers and publishers wait for short metadata transactions.
    /// Contention is not absence, corruption, or a reason to discard a write.
    pub fn openRequired(store: *const Store, number: u4, create: bool, counters: *catalog.Counters) Error!Shard {
        return openWith(store, number, create, counters, true);
    }

    fn openWith(store: *const Store, number: u4, create: bool, counters: *catalog.Counters, required: bool) Error!Shard {
        var root = try store.openRoot(create);
        defer root.close(store.io);
        var name_buffer: [2]u8 = undefined;
        const name = std.fmt.bufPrint(&name_buffer, "{x:0>2}", .{number}) catch return error.PathTooLong;
        if (create) root.createDirPath(store.io, name) catch return error.IoFailure;
        const dir = root.openDir(store.io, name, .{}) catch |err| switch (err) {
            error.FileNotFound => return error.Missing,
            else => return error.IoFailure,
        };
        errdefer dir.close(store.io);
        // macOS can fail concurrent non-exclusive creations of one name with
        // FileNotFound. Exactly one exclusive creation succeeds; every opener
        // then locks the existing file without creating it.
        if (dir.createFile(store.io, "coordination", .{ .exclusive = true })) |created| {
            created.close(store.io);
        } else |err| switch (err) {
            error.PathAlreadyExists => {},
            else => return error.IoFailure,
        }
        const lock = dir.openFile(store.io, "coordination", .{ .mode = .read_write, .lock = .exclusive, .lock_nonblocking = !required }) catch |err| switch (err) {
            error.WouldBlock => return error.Busy,
            else => return error.IoFailure,
        };
        errdefer lock.close(store.io);
        return .{ .io = store.io, .dir = dir, .lock = lock, .index = try catalog.Index.open(dir, store.io, counters), .number = number };
    }

    pub fn close(self: *Shard) void {
        self.index.close();
        self.lock.close(self.io);
        self.dir.close(self.io);
    }

    /// A retiring slot is only freed after all names have been removed. The
    /// lease identity is kept until retirement, while the shard lock excludes
    /// anyone from opening that slot's lease for a new generation.
    pub fn retire(self: *Shard, key: catalog.Key, generation: u64) Error!bool {
        var tx = self.index.transaction();
        const found = (try tx.find(.owners, storageKey(generation))) orelse return false;
        var metadata = try Metadata.decode(found.value);
        if (metadata.generation != generation or !std.mem.eql(u8, &metadata.key, &key) or metadata.slot != found.id) return error.CorruptCatalog;
        var stem_buffer: [128]u8 = undefined;
        const stem = try slotStem(&stem_buffer, found.id);
        var lease_buffer: [140]u8 = undefined;
        const lease_path = std.fmt.bufPrint(&lease_buffer, "{s}.lease", .{stem}) catch return error.PathTooLong;
        const lease = try leaseFile(self.dir, self.io, lease_path, false, false);
        defer lease.close(self.io);
        if (metadata.state != .retiring) {
            const counter: usize = if (metadata.state == .staging) 2 else 1;
            if (tx.header.user[counter] < metadata.size) return error.CorruptCatalog;
            tx.header.user[counter] -= metadata.size;
            tx.header.user[3] = std.math.add(u64, tx.header.user[3], metadata.size) catch return error.CapacityExceeded;
            if (metadata.state == .published and !try tx.remove(.recency, metadata.recency(found.id))) return error.CorruptCatalog;
            if (try tx.find(.artifacts, key)) |current| {
                if ((try Metadata.decode(current.value)).generation == generation) _ = try tx.remove(.artifacts, key);
            }
            metadata.state = .retiring;
            _ = try tx.putValue(.owners, storageKey(generation), metadata.encode());
            try tx.commit();
        }
        if (metadata.family == .scratch and !try drainScratch(self, stem, metadata.generation)) return false;
        for ([_][]const u8{ ".pending", ".data", ".pending.txt", ".cursor" }) |suffix| {
            var buffer: [140]u8 = undefined;
            const path = std.fmt.bufPrint(&buffer, "{s}{s}", .{ stem, suffix }) catch return error.PathTooLong;
            self.dir.deleteFile(self.io, path) catch |err| switch (err) {
                error.FileNotFound => {},
                else => return error.IoFailure,
            };
        }
        var finish = self.index.transaction();
        if (finish.header.user[3] < metadata.size) return error.CorruptCatalog;
        finish.header.user[3] -= metadata.size;
        if (!try finish.remove(.owners, storageKey(generation))) return error.CorruptCatalog;
        try finish.commit();
        // Keep the lease filename stable across slot reuse. Empty directories
        // and this small control record never participate in size eviction.
        return true;
    }
};

/// Registered unpublished payload whose exclusive lease pins its slot.
pub const Staging = struct {
    store: *const Store,
    key: catalog.Key,
    slot: u64,
    generation: u64,
    lease: Io.File,
    path: []u8,
    /// Publication releases the lease while the shard lock still excludes
    /// readers, so no reader ever finds a published slot leased exclusively.
    published: bool = false,

    /// Closing a failed staging operation leaves a registered, abandoned slot.
    /// Maintenance can reclaim it after the staging window; no finalizer must
    /// run for its ownership to become recoverable.
    pub fn deinit(self: *Staging) void {
        if (!self.published) self.lease.close(self.store.io);
        self.store.allocator.free(self.path);
    }

    pub fn publish(self: *Staging) Error!void {
        var counters = catalog.Counters{};
        var shard = try Shard.openRequired(self.store, Store.shardFor(self.key), false, &counters);
        defer shard.close();
        var tx = shard.index.transaction();
        const found = (try tx.find(.owners, storageKey(self.generation))) orelse return error.Changed;
        var metadata = try Metadata.decode(found.value);
        if (found.id != self.slot or metadata.generation != self.generation or metadata.state != .staging) return error.Changed;
        const stat = Dir.cwd().statFile(self.store.io, self.path, .{}) catch return error.IoFailure;
        if (stat.kind != .file or stat.size != metadata.size) return error.InvalidArtifact;
        const final_path = try self.store.payloadPath(shard.number, self.slot, ".data");
        defer self.store.allocator.free(final_path);
        Dir.cwd().rename(self.path, Dir.cwd(), final_path, self.store.io) catch return error.IoFailure;
        if (try tx.find(.artifacts, self.key)) |previous| {
            var old = try currentMetadata(&tx, self.key, previous.value);
            if (old.state != .published or tx.header.user[1] < old.size) return error.CorruptCatalog;
            if (!try tx.remove(.recency, old.recency(old.slot))) return error.CorruptCatalog;
            tx.header.user[1] -= old.size;
            tx.header.user[3] = std.math.add(u64, tx.header.user[3], old.size) catch return error.CapacityExceeded;
            old.state = .retiring;
            _ = try tx.putValue(.owners, storageKey(old.generation), old.encode());
        }
        metadata.state = .published;
        if (tx.header.user[2] < metadata.size) return error.CorruptCatalog;
        tx.header.user[2] -= metadata.size;
        tx.header.user[1] = std.math.add(u64, tx.header.user[1], metadata.size) catch return error.CapacityExceeded;
        _ = try tx.putValue(.artifacts, self.key, metadata.encode());
        _ = try tx.putValue(.owners, storageKey(self.generation), metadata.encode());
        var value: catalog.Value = @splat(0);
        @memcpy(value[0..64], &self.key);
        _ = try tx.putValue(.recency, metadata.recency(self.slot), value);
        try tx.commit();
        self.lease.close(self.store.io);
        self.published = true;
    }
};

/// Acquired payload file and shared lease, both owned until deinit.
pub const Artifact = struct {
    store: *const Store,
    file: Io.File,
    lease: Io.File,
    path: []u8,
    size: u64,

    pub fn deinit(self: *Artifact) void {
        self.file.close(self.store.io);
        self.lease.close(self.store.io);
        self.store.allocator.free(self.path);
    }

    pub fn read(self: *const Artifact, allocator: Allocator) Error![]u8 {
        const size = std.math.cast(usize, self.size) orelse return error.OutOfMemory;
        const bytes = try allocator.alloc(u8, size);
        errdefer allocator.free(bytes);
        const len = self.file.readPositionalAll(self.store.io, bytes, 0) catch return error.IoFailure;
        if (len != bytes.len) return error.InvalidArtifact;
        return bytes;
    }
};

/// One CLI-owned scratch directory. The lease, not the timestamp, establishes
/// whether its contents may still be used. All files in an abandoned directory
/// are eligible, so bounded destructive traversal can always consume its first
/// entry without persisting an opaque iterator or rescanning a retained prefix.
pub const Scratch = struct {
    store: *Store,
    staging: Staging,
    keep_until_expired: bool = false,

    pub fn create(allocator: Allocator, io: Io, cache_root: []const u8, namespace: [32]u8) Error!Scratch {
        const store = try allocator.create(Store);
        errdefer allocator.destroy(store);
        store.* = try Store.init(allocator, io, cache_root);
        errdefer store.deinit();
        var nonce: [32]u8 = undefined;
        io.random(&nonce);
        var staging = try store.reserveWith(.{ .namespace = namespace, .family = .scratch, .key = nonce }, 0, store.now(), true);
        errdefer staging.deinit();
        // Native application children use this same lease. Merely closing the
        // parent's copy never unlocks a file description still held by a child.
        if (comptime @import("builtin").os.tag == .windows) {
            const w = std.os.windows;
            if (w.ntdll.NtSetInformationObject(staging.lease.handle, .HandleFlag, &w.OBJECT.HANDLE_FLAG{ .INHERIT = true }, @sizeOf(w.OBJECT.HANDLE_FLAG)) != .SUCCESS) return error.IoFailure;
        } else {
            if (std.posix.errno(std.posix.system.fcntl(staging.lease.handle, std.posix.F.SETFD, @as(u32, 0))) != .SUCCESS) return error.IoFailure;
        }
        Dir.cwd().createDir(io, staging.path, .default_dir) catch return error.IoFailure;
        return .{ .store = store, .staging = staging };
    }

    pub fn path(self: *const Scratch) []const u8 {
        return self.staging.path;
    }

    pub fn deinit(self: *Scratch) void {
        const allocator = self.store.allocator;
        const key = self.staging.key;
        const generation = self.staging.generation;
        self.staging.deinit();
        defer {
            self.store.deinit();
            allocator.destroy(self.store);
        }
        if (self.keep_until_expired) return;
        // Normal completion makes the entry immediately eligible. A crash
        // leaves it eligible after the abandoned-work window instead.
        var counters = catalog.Counters{};
        if (Shard.open(self.store, Store.shardFor(key), false, &counters)) |opened| {
            var shard = opened;
            defer shard.close();
            var tx = shard.index.transaction();
            if (tx.find(.owners, storageKey(generation)) catch null) |entry| {
                if (Metadata.decode(entry.value)) |value| {
                    var metadata = value;
                    metadata.state = .retiring;
                    _ = tx.putValue(.owners, storageKey(generation), metadata.encode()) catch 0;
                    tx.commit() catch {};
                } else |_| {}
            }
        } else |_| {}
    }
};

/// One filesystem leaf or empty directory per call. The cursor has a fixed
/// byte limit, checksum, and generation. Invalid cursors restart at the owned
/// root; they never provide deletion authority.
fn drainScratch(shard: *Shard, stem: []const u8, generation: u64) Error!bool {
    var path_buffer: [140]u8 = undefined;
    const scratch_path = std.fmt.bufPrint(&path_buffer, "{s}.pending", .{stem}) catch return error.PathTooLong;
    var root = shard.dir.openDir(shard.io, scratch_path, .{ .follow_symlinks = false }) catch |err| switch (err) {
        error.FileNotFound => return true,
        else => return error.IoFailure,
    };
    var root_closed = false;
    defer if (!root_closed) root.close(shard.io);
    var cursor_name_buffer: [140]u8 = undefined;
    const cursor_name = std.fmt.bufPrint(&cursor_name_buffer, "{s}.cursor", .{stem}) catch return error.PathTooLong;
    var cursor: [std.fs.max_path_bytes]u8 = @splat(0);
    const capacity = cursor.len - 32;
    var relative: []const u8 = "";
    if (shard.dir.openFile(shard.io, cursor_name, .{})) |file| {
        defer file.close(shard.io);
        const len = file.readPositionalAll(shard.io, &cursor, 0) catch return error.IoFailure;
        if (len == cursor.len and get(u64, &cursor, 0) == std.hash.Wyhash.hash(0, cursor[8..]) and get(u64, &cursor, 8) == generation) {
            const path_len = get(u64, &cursor, 16);
            if (path_len <= capacity) relative = cursor[32..][0..@intCast(path_len)];
        }
    } else |err| switch (err) {
        error.FileNotFound => {},
        else => return error.IoFailure,
    }
    // Validate every component and prohibit following any symlink on the
    // route from the registered scratch root to the current directory.
    var directory = root;
    var owns_directory = false;
    defer if (owns_directory) directory.close(shard.io);
    var parts = std.mem.splitScalar(u8, relative, '/');
    var valid_relative = true;
    while (parts.next()) |part| {
        if (part.len == 0) {
            if (relative.len != 0) valid_relative = false;
            break;
        }
        if (std.mem.eql(u8, part, ".") or std.mem.eql(u8, part, "..") or std.mem.findScalar(u8, part, '\\') != null or std.mem.findScalar(u8, part, 0) != null or std.mem.findScalar(u8, part, ':') != null) {
            valid_relative = false;
            break;
        }
        const child = directory.openDir(shard.io, part, .{ .follow_symlinks = false }) catch {
            valid_relative = false;
            break;
        };
        if (owns_directory) directory.close(shard.io);
        directory = child;
        owns_directory = true;
    }
    if (!valid_relative) {
        relative = "";
    } else {
        var iterable = directory.openDir(shard.io, ".", .{ .iterate = true, .follow_symlinks = false }) catch return error.IoFailure;
        var iterable_closed = false;
        defer if (!iterable_closed) iterable.close(shard.io);
        var iterator = iterable.iterate();
        if (iterator.next(shard.io) catch return error.IoFailure) |entry| {
            if (entry.kind == .directory) {
                var next: [std.fs.max_path_bytes]u8 = undefined;
                const name = std.fmt.bufPrint(&next, "{s}{s}{s}", .{ relative, if (relative.len == 0) "" else "/", entry.name }) catch return error.PathTooLong;
                if (name.len > capacity) return error.PathTooLong;
                @memcpy(cursor[32..][0..name.len], name);
                relative = cursor[32..][0..name.len];
            } else {
                directory.deleteFile(shard.io, entry.name) catch |err| switch (err) {
                    error.FileNotFound => {},
                    else => return error.IoFailure,
                };
            }
        } else if (relative.len == 0) {
            iterable.close(shard.io);
            iterable_closed = true;
            root.close(shard.io);
            root_closed = true;
            shard.dir.deleteDir(shard.io, scratch_path) catch |err| switch (err) {
                error.FileNotFound => {},
                else => return error.IoFailure,
            };
            shard.dir.deleteFile(shard.io, cursor_name) catch {};
            return true;
        } else {
            iterable.close(shard.io);
            iterable_closed = true;
            if (owns_directory) {
                directory.close(shard.io);
                owns_directory = false;
            }
            root.deleteDir(shard.io, relative) catch |err| switch (err) {
                error.FileNotFound => {},
                else => return error.IoFailure,
            };
            relative = if (std.mem.findScalarLast(u8, relative, '/')) |index| relative[0..index] else "";
        }
    }
    put(u64, &cursor, 8, generation);
    put(u64, &cursor, 16, relative.len);
    if (relative.len != 0) std.mem.copyForwards(u8, cursor[32..][0..relative.len], relative);
    @memset(cursor[32 + relative.len ..], 0);
    put(u64, &cursor, 0, std.hash.Wyhash.hash(0, cursor[8..]));
    // The ownership lease serializes this disposable cursor. A torn write
    // simply restarts at the root; the already removed leaves stay removed.
    shard.dir.writeFile(shard.io, .{ .sub_path = cursor_name, .data = &cursor }) catch return error.IoFailure;
    return false;
}

/// Encode a physical slot in a fixed-depth directory fanout.
pub fn slotStem(buffer: *[128]u8, slot: u64) Error![]const u8 {
    var bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &bytes, slot, .big);
    return std.fmt.bufPrint(buffer, "data/{x:0>2}/{x:0>2}/{x:0>2}/{x:0>2}/{x:0>2}/{x:0>2}/{x:0>2}/{x:0>2}", .{
        bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    }) catch return error.PathTooLong;
}

/// Order allocation generations lexicographically for resumable traversal.
pub fn storageKey(generation: u64) catalog.Key {
    var key: catalog.Key = @splat(0);
    std.mem.writeInt(u64, key[0..8], generation, .big);
    return key;
}

/// Advance a fixed-width ordered cursor; false means the key space ended.
pub fn advanceKey(key: *catalog.Key) bool {
    var index: usize = key.len;
    while (index != 0) {
        index -= 1;
        key[index] +%= 1;
        if (key[index] != 0) return true;
    }
    return false;
}

test "publication owns staging and an accepted reader survives eviction" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var store = try Store.init(allocator, io, root);
    defer store.deinit();
    const identity: Identity = .{ .namespace = @splat(1), .family = .checked, .key = @splat(2) };
    var stage = try store.reserve(identity, 5, 1);
    try Dir.cwd().writeFile(io, .{ .sub_path = stage.path, .data = "hello" });
    try std.testing.expect((try store.acquire(identity, 2)) == null);
    {
        var counters = catalog.Counters{};
        var shard = try Shard.open(&store, Store.shardFor(stage.key), false, &counters);
        defer shard.close();
        try std.testing.expectError(error.Busy, shard.retire(stage.key, stage.generation));
    }
    try stage.publish();
    const generation = stage.generation;
    stage.deinit();
    var reader = (try store.acquire(identity, 3)).?;
    {
        var counters = catalog.Counters{};
        var shard = try Shard.open(&store, Store.shardFor(identity.indexedKey()), false, &counters);
        defer shard.close();
        try std.testing.expectError(error.Busy, shard.retire(identity.indexedKey(), generation));
    }
    const bytes = try reader.read(allocator);
    defer allocator.free(bytes);
    try std.testing.expectEqualStrings("hello", bytes);
    reader.deinit();
    var counters = catalog.Counters{};
    var shard = try Shard.open(&store, Store.shardFor(identity.indexedKey()), false, &counters);
    try std.testing.expect(try shard.retire(identity.indexedKey(), generation));
    shard.close();
    try std.testing.expect((try store.acquire(identity, 4)) == null);
}

test "shard ownership is nonblocking and lock-file existence does not imply ownership" {
    const io = std.testing.io;
    const allocator = std.testing.allocator;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var store = try Store.init(allocator, io, root);
    defer store.deinit();
    var counters = catalog.Counters{};
    var first = try Shard.open(&store, 0, true, &counters);
    try std.testing.expectError(error.Busy, Shard.open(&store, 0, false, &counters));
    first.close();
    var second = try Shard.open(&store, 0, false, &counters);
    second.close();
}

test "interleaved publication and retirement preserve generations and acquired storage" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var store = try Store.init(allocator, io, root);
    defer store.deinit();
    const id: Identity = .{ .namespace = @splat(1), .family = .checked, .key = @splat(2) };
    var first = try store.reserve(id, 3, 1);
    defer first.deinit();
    var second = try store.reserve(id, 3, 2);
    var second_open = true;
    defer if (second_open) second.deinit();
    try std.testing.expect(!std.mem.eql(u8, first.path, second.path));
    try Dir.cwd().writeFile(io, .{ .sub_path = first.path, .data = "o" });
    try std.testing.expectError(error.InvalidArtifact, first.publish());
    try Dir.cwd().writeFile(io, .{ .sub_path = second.path, .data = "new" });
    try second.publish();
    // Readers accept a published generation while its producer is still open.
    var accepted = (try store.acquire(id, 2)).?;
    defer accepted.deinit();
    second.deinit();
    second_open = false;
    try Dir.cwd().writeFile(io, .{ .sub_path = first.path, .data = "old" });
    try first.publish();
    const bytes = try accepted.read(allocator);
    defer allocator.free(bytes);
    try std.testing.expectEqualStrings("new", bytes);
    var counters = catalog.Counters{};
    var shard = try Shard.open(&store, Store.shardFor(first.key), false, &counters);
    defer shard.close();
    try std.testing.expectError(error.Busy, shard.retire(second.key, second.generation));
    var tx = shard.index.transaction();
    try std.testing.expectEqual(@as(u64, 3), tx.header.user[1]);
    try std.testing.expectEqual(@as(u64, 3), tx.header.user[3]);
    const current = try Metadata.decode((try tx.find(.artifacts, first.key)).?.value);
    try std.testing.expectEqual(first.generation, current.generation);
}

test "file import registers exact bytes and permissions before publication" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    try tmp.dir.writeFile(io, .{ .sub_path = "executable", .data = "executable bytes" });
    const source = try tmp.dir.realPathFileAlloc(io, "executable", allocator);
    defer allocator.free(source);
    var store = try Store.init(allocator, io, root);
    defer store.deinit();
    const id: Identity = .{ .namespace = @splat(1), .family = .executable, .key = @splat(2) };
    try store.importFile(id, source);
    const bytes = (try store.read(allocator, id, store.now())).?;
    defer allocator.free(bytes);
    try std.testing.expectEqualStrings("executable bytes", bytes);
}

test "scratch ownership survives the parent closing its inherited lease" {
    if (comptime @import("builtin").os.tag == .windows) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var store = try Store.init(allocator, io, root);
    defer store.deinit();
    var scratch = try Scratch.create(allocator, io, root, @splat(1));
    var scratch_open = true;
    defer if (scratch_open) scratch.deinit();
    const key = scratch.staging.key;
    const generation = scratch.staging.generation;
    var child = try std.process.spawn(io, .{
        .argv = &.{ "/bin/sh", "-c", "printf r; read line" },
        .inherit_files = &.{scratch.staging.lease},
        .stdin = .pipe,
        .stdout = .pipe,
    });
    defer child.kill(io);
    var ready: [1]u8 = undefined;
    try std.testing.expectEqual(@as(usize, 1), try child.stdout.?.readStreaming(io, &.{&ready}));
    scratch.deinit();
    scratch_open = false;
    var counters = catalog.Counters{};
    {
        var shard = try Shard.open(&store, Store.shardFor(key), false, &counters);
        defer shard.close();
        try std.testing.expectError(error.Busy, shard.retire(key, generation));
    }
    try child.stdin.?.writeStreamingAll(io, "done\n");
    try std.testing.expectEqual(std.process.Child.Term{ .exited = 0 }, try child.wait(io));
    var shard = try Shard.open(&store, Store.shardFor(key), false, &counters);
    defer shard.close();
    try std.testing.expect(try shard.retire(key, generation));
}

test "foreground artifact contention never becomes a cache miss or lost publication" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var store = try Store.init(allocator, io, root);
    defer store.deinit();
    const id: Identity = .{ .namespace = @splat(17), .family = .checked, .key = @splat(23) };
    try store.write(id, "validated bytes", 1);
    const workers = 8;
    const Shared = struct {
        store: *const Store,
        identity: Identity,
        ready: std.atomic.Value(usize) = .init(0),
        start: std.atomic.Value(bool) = .init(false),
        failed: std.atomic.Value(bool) = .init(false),
        fn run(self: *@This()) void {
            _ = self.ready.fetchAdd(1, .release);
            while (!self.start.load(.acquire)) std.atomic.spinLoopHint();
            for (0..16) |iteration| {
                const bytes = (self.store.read(self.store.allocator, self.identity, @intCast(iteration + 2)) catch {
                    self.failed.store(true, .release);
                    return;
                }) orelse {
                    self.failed.store(true, .release);
                    return;
                };
                defer self.store.allocator.free(bytes);
                if (!std.mem.eql(u8, bytes, "validated bytes")) self.failed.store(true, .release);
                self.store.write(self.identity, "validated bytes", @intCast(iteration + 2)) catch {
                    self.failed.store(true, .release);
                    return;
                };
            }
        }
    };
    var shared = Shared{ .store = &store, .identity = id };
    var threads: [workers]std.Thread = undefined;
    var spawned: usize = 0;
    defer {
        shared.start.store(true, .release);
        for (threads[0..spawned]) |thread| thread.join();
    }
    for (&threads) |*thread| {
        thread.* = try std.Thread.spawn(.{}, Shared.run, .{&shared});
        spawned += 1;
    }
    while (shared.ready.load(.acquire) != workers) std.atomic.spinLoopHint();
    shared.start.store(true, .release);
    for (threads) |thread| thread.join();
    spawned = 0;
    try std.testing.expect(!shared.failed.load(.acquire));
    const bytes = (try store.read(allocator, id, 20)).?;
    defer allocator.free(bytes);
    try std.testing.expectEqualStrings("validated bytes", bytes);
}

test "artifact groups route all members to one shard and enumerate only published members" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realPathFileAlloc(io, ".", allocator);
    defer allocator.free(root);
    var store = try Store.init(allocator, io, root);
    defer store.deinit();
    const identity: Identity = .{ .namespace = @splat(7), .family = .local_pack, .group = @splat(11), .key = @splat(0) };
    const shard = Store.shardFor(identity.indexedKey());
    for (0..32) |i| {
        var member = identity;
        member.key[0] = @intCast(31 - i);
        try std.testing.expectEqual(shard, Store.shardFor(member.indexedKey()));
        var stage = try store.reserve(member, 1, 1);
        defer stage.deinit();
        try Dir.cwd().writeFile(io, .{ .sub_path = stage.path, .data = "x" });
        try stage.publish();
    }
    var unfinished = identity;
    unfinished.key[0] = 32;
    var stage = try store.reserve(unfinished, 1, 1);
    defer stage.deinit();
    var other = identity;
    other.group = @splat(12);
    var other_stage = try store.reserve(other, 1, 1);
    defer other_stage.deinit();
    try Dir.cwd().writeFile(io, .{ .sub_path = other_stage.path, .data = "y" });
    try other_stage.publish();
    const keys = try store.groupKeys(allocator, identity);
    defer allocator.free(keys);
    try std.testing.expectEqual(32, keys.len);
    for (keys, 0..) |key, i| {
        try std.testing.expectEqual(i, key[32]);
        var reader = (try store.acquireKey(key, 2)).?;
        defer reader.deinit();
        const bytes = try reader.read(allocator);
        defer allocator.free(bytes);
        try std.testing.expectEqualStrings("x", bytes);
    }
    const other_keys = try store.groupKeys(allocator, other);
    defer allocator.free(other_keys);
    try std.testing.expectEqual(1, other_keys.len);
}

test "retained reads batch one exact owner update per generation before releasing leases" {
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(path);
    var store = try Store.init(a, io, path);
    defer store.deinit();
    var batch = try RetentionBatch.init(&store);
    defer batch.deinit();
    var ids: [6]Identity = undefined;
    var held: [6]?Artifact = @splat(null);
    defer for (&held) |*item| if (item.*) |*artifact| artifact.deinit();
    for (&ids, 0..) |*id, i| {
        id.* = .{ .namespace = @splat(7), .family = .local_pack, .group = @splat(5), .key = @splat(@intCast(i)) };
        try store.write(id.*, "payload", 10);
    }
    for (ids, &held) |id, *item| item.* = (try batch.acquireKey(id.indexedKey(), 20)).?;
    const key = ids[0].indexedKey();
    var counters = catalog.Counters{};
    var sequence: u64 = undefined;
    {
        var shard = try Shard.openRequired(&store, Store.shardFor(key), false, &counters);
        defer shard.close();
        sequence = shard.index.header.sequence;
        var tx = shard.index.transaction();
        for (ids) |id| {
            const k = id.indexedKey();
            const ptr = (try tx.find(.artifacts, k)).?;
            const m = try currentMetadata(&tx, k, ptr.value);
            try std.testing.expectEqual(10, m.used);
            try std.testing.expectError(error.Busy, shard.retire(k, m.generation));
        }
    }
    try batch.flush();
    {
        var shard = try Shard.openRequired(&store, Store.shardFor(key), false, &counters);
        defer shard.close();
        try std.testing.expectEqual(sequence + 1, shard.index.header.sequence);
        var tx = shard.index.transaction();
        for (ids) |id| {
            const k = id.indexedKey();
            const ptr = (try tx.find(.artifacts, k)).?;
            try std.testing.expectEqual(20, (try currentMetadata(&tx, k, ptr.value)).used);
        }
    }
    // A newer concurrent reader wins even when an older batch flushes last.
    var older = (try batch.acquireKey(key, 25)).?;
    defer older.deinit();
    var newer = (try store.acquireKey(key, 30)).?;
    defer newer.deinit();
    try batch.flush();
    {
        var shard = try Shard.openRequired(&store, Store.shardFor(key), false, &counters);
        defer shard.close();
        var tx = shard.index.transaction();
        const ptr = (try tx.find(.artifacts, key)).?;
        try std.testing.expectEqual(30, (try currentMetadata(&tx, key, ptr.value)).used);
    }
}

test "retention batches discard observations of a retired generation" {
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(path);
    var store = try Store.init(a, io, path);
    defer store.deinit();
    var batch = try RetentionBatch.init(&store);
    defer batch.deinit();
    const id: Identity = .{ .namespace = @splat(8), .family = .local_pack, .key = @splat(3) };
    const key = id.indexedKey();
    try store.write(id, "rejected", 10);
    var rejected = (try batch.acquireKey(key, 40)).?;
    rejected.deinit();
    var counters = catalog.Counters{};
    {
        var shard = try Shard.openRequired(&store, Store.shardFor(key), false, &counters);
        defer shard.close();
        var tx = shard.index.transaction();
        const ptr = (try tx.find(.artifacts, key)).?;
        try std.testing.expect(try shard.retire(key, (try currentMetadata(&tx, key, ptr.value)).generation));
    }
    try store.write(id, "replacement", 20);
    try batch.flush();
    {
        var shard = try Shard.openRequired(&store, Store.shardFor(key), false, &counters);
        defer shard.close();
        var tx = shard.index.transaction();
        const ptr = (try tx.find(.artifacts, key)).?;
        try std.testing.expectEqual(20, (try currentMetadata(&tx, key, ptr.value)).used);
    }
}

test "grouped acquisitions preserve order absence duplicates and generation leases" {
    const a = std.testing.allocator;
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const path = try tmp.dir.realPathFileAlloc(io, ".", a);
    defer a.free(path);
    var store = try Store.init(a, io, path);
    defer store.deinit();
    var identities: [80]Identity = undefined;
    for (&identities, 0..) |*id, i| {
        id.* = .{ .namespace = @splat(7), .family = .local_pack, .group = @splat(if (i < 64) 1 else @intCast(i)), .key = @splat(@intCast(i)) };
        try store.write(id.*, &.{@intCast(i)}, 1);
    }
    var keys: [83]catalog.Key = undefined;
    for (identities, 0..) |id, i| keys[79 - i] = id.indexedKey();
    keys[80] = identities[0].indexedKey();
    var missing = identities[0];
    missing.key = @splat(255);
    keys[81] = missing.indexedKey();
    keys[82] = identities[79].indexedKey();
    var batch = try RetentionBatch.init(&store);
    defer batch.deinit();
    const held = try batch.acquireKeys(a, &keys, 100);
    defer a.free(held);
    defer for (held) |*item| if (item.*) |*artifact| artifact.deinit();
    try std.testing.expect(held[81] == null);
    for (held, 0..) |item, i| {
        if (i == 81) continue;
        const bytes = try item.?.read(a);
        defer a.free(bytes);
        const expected: u8 = if (i == 80) 0 else if (i == 82) 79 else @intCast(79 - i);
        try std.testing.expectEqualSlices(u8, &.{expected}, bytes);
    }
    try batch.flush();
    try store.write(identities[0], "replacement", 200);
    const old = try held[80].?.read(a);
    defer a.free(old);
    try std.testing.expectEqualSlices(u8, &.{0}, old);
    const current = (try store.read(a, identities[0], 300)).?;
    defer a.free(current);
    try std.testing.expectEqualStrings("replacement", current);
    try std.testing.expectError(error.Busy, store.invalidate(identities[79]));
}
