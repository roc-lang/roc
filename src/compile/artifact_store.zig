//! Native, indexed artifact storage. All payloads, including unfinished ones,
//! have a catalog slot before creation. Shard locks protect only metadata and
//! acquisition; payload production never holds a catalog lock.

const std = @import("std");
pub const catalog = @import("cache_index.zig");
const Io = std.Io;
const Dir = Io.Dir;
const Allocator = std.mem.Allocator;
const put = catalog.put;
const get = catalog.get;

pub const format_directory = "artifacts-v1";
pub const shard_count = 16;
pub const minute_ns: i64 = 60 * std.time.ns_per_s;
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
            else => false,
        };
    }

    fn expiresByUse(self: Family) bool {
        return self.isPack();
    }
};

pub const Identity = struct {
    namespace: [32]u8,
    family: Family,
    group: [32]u8 = @splat(0),
    key: [32]u8,
    pack_format: u32 = 0,
    variant: [32]u8 = @splat(0),

    pub fn indexedKey(self: Identity) catalog.Key {
        var hash = std.crypto.hash.sha2.Sha256.init(.{});
        hash.update("roc-artifact-group-v1");
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

pub const Error = catalog.Error || error{ Busy, Missing, Changed, InvalidArtifact, PathTooLong, OutOfMemory };

pub fn hashBytes(bytes: []const u8) [32]u8 {
    var result: [32]u8 = undefined;
    std.crypto.hash.sha2.Sha256.hash(bytes, &result, .{});
    return result;
}

pub const State = enum(u8) { staging, published, retiring };

pub const Metadata = struct {
    state: State,
    family: Family,
    pack_format: u32,
    namespace: [32]u8,
    generation: u64,
    size: u64,
    created: i64,
    used: i64,
    key: catalog.Key,
    slot: u64,
    variant: [32]u8 = @splat(0),

    pub fn encode(self: Metadata) catalog.Value {
        var bytes: catalog.Value = @splat(0);
        @memcpy(bytes[0..8], "ROCART01");
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
        return bytes;
    }

    pub fn decode(bytes: catalog.Value) Error!Metadata {
        if (!std.mem.eql(u8, bytes[0..8], "ROCART01")) return error.CorruptCatalog;
        return .{
            .state = std.enums.fromInt(State, bytes[8]) orelse return error.CorruptCatalog,
            .family = std.enums.fromInt(Family, bytes[9]) orelse return error.CorruptCatalog,
            .pack_format = get(u32, &bytes, 12),
            .namespace = bytes[16..48].*,
            .generation = get(u64, &bytes, 48),
            .size = get(u64, &bytes, 56),
            .created = get(i64, &bytes, 64),
            .used = get(i64, &bytes, 72),
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
        std.mem.writeInt(u64, key[0..8], @as(u64, @bitCast(self.used)) ^ (@as(u64, 1) << 63), .big);
        std.mem.writeInt(u64, key[8..16], slot, .big);
        return key;
    }
};

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
        return @truncate(key[32]);
    }

    pub fn reserve(self: *const Store, identity: Identity, size: u64, timestamp: i64) Error!Staging {
        return self.reserveWith(identity, size, timestamp, false);
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
        var counters = catalog.Counters{};
        var shard = Shard.open(self, shardFor(key), false, &counters) catch |err| switch (err) {
            error.Missing, error.Busy => return null,
            else => return err,
        };
        defer shard.close();
        var tx = shard.index.transaction();
        const found = (try tx.find(.artifacts, key)) orelse return null;
        var metadata = try Metadata.decode(found.value);
        if (metadata.state != .published or metadata.family == .scratch) return null;
        const path = try self.payloadPath(shard.number, metadata.slot, ".data");
        errdefer self.allocator.free(path);
        var stem_buffer: [128]u8 = undefined;
        const stem = try slotStem(&stem_buffer, metadata.slot);
        var lease_buffer: [140]u8 = undefined;
        const relative_lease = std.fmt.bufPrint(&lease_buffer, "{s}.lease", .{stem}) catch return error.PathTooLong;
        const lease = leaseFile(shard.dir, self.io, relative_lease, true, false) catch |err| {
            if (err == error.Missing or err == error.Busy) {
                self.allocator.free(path);
                return null;
            }
            return err;
        };
        errdefer lease.close(self.io);
        const file = Dir.cwd().openFile(self.io, path, .{}) catch |err| {
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
            if (!try tx.remove(.recency, metadata.recency(metadata.slot))) return error.CorruptCatalog;
            metadata.used = timestamp;
            _ = try tx.putValue(.artifacts, key, metadata.encode());
            _ = try tx.putValue(.owners, storageKey(metadata.generation), metadata.encode());
            var value: catalog.Value = @splat(0);
            @memcpy(value[0..64], &key);
            _ = try tx.putValue(.recency, metadata.recency(metadata.slot), value);
            try tx.commit();
        }
        const actual_size = file.length(self.io) catch return error.IoFailure;
        return .{ .store = self, .file = file, .lease = lease, .path = path, .size = actual_size };
    }

    /// Invalidate a rejected cache offer under the same generation validation
    /// used by maintenance. A reader holding its lease keeps accepted storage.
    pub fn invalidate(self: *const Store, identity: Identity) Error!void {
        const key = identity.indexedKey();
        var counters = catalog.Counters{};
        var shard = try Shard.open(self, shardFor(key), false, &counters);
        defer shard.close();
        var tx = shard.index.transaction();
        const entry = (try tx.find(.artifacts, key)) orelse return;
        const metadata = try Metadata.decode(entry.value);
        _ = try shard.retire(key, metadata.generation);
    }

    pub fn read(self: *const Store, allocator: Allocator, identity: Identity, timestamp: i64) Error!?[]u8 {
        var artifact = (try self.acquire(identity, timestamp)) orelse return null;
        defer artifact.deinit();
        return try artifact.read(allocator);
    }

    /// Enumerating offers is foreground work. The key index supplies stable
    /// ordering within a shard; the caller sorts the combined keys as required
    /// by the existing deterministic pack-selection contract.
    pub fn groupKeys(self: *const Store, allocator: Allocator, identity: Identity) Error![]catalog.Key {
        var keys: std.ArrayList(catalog.Key) = .empty;
        errdefer keys.deinit(allocator);
        const group = identity.indexedKey()[0..32].*;
        for (0..shard_count) |number| {
            var cursor: catalog.Key = @splat(0);
            @memcpy(cursor[0..32], &group);
            while (true) {
                var counters = catalog.Counters{};
                var shard = Shard.open(self, @intCast(number), false, &counters) catch |err| switch (err) {
                    error.Missing, error.Busy => break,
                    else => return err,
                };
                defer shard.close();
                var tx = shard.index.transaction();
                const entry = (try tx.lowerBound(.artifacts, cursor)) orelse break;
                if (!std.mem.eql(u8, entry.key[0..32], &group)) break;
                const metadata = try Metadata.decode(entry.value);
                if (metadata.state == .published) try keys.append(allocator, entry.key);
                cursor = entry.key;
                if (!advanceKey(&cursor)) break;
            }
        }
        std.mem.sort(catalog.Key, keys.items, {}, struct {
            fn less(_: void, a: catalog.Key, b: catalog.Key) bool {
                return std.mem.order(u8, &a, &b) == .lt;
            }
        }.less);
        return keys.toOwnedSlice(allocator);
    }

    pub fn payloadPath(self: *const Store, shard: u4, slot: u64, suffix: []const u8) Error![]u8 {
        var stem_buffer: [128]u8 = undefined;
        const stem = try slotStem(&stem_buffer, slot);
        return std.fmt.allocPrint(self.allocator, "{s}/{x:0>2}/{s}{s}", .{ self.root, shard, stem, suffix });
    }
};

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
        return switch (result) {
            .SUCCESS => .{ .handle = handle, .flags = .{ .nonblocking = false } },
            .SHARING_VIOLATION => error.Busy,
            .OBJECT_NAME_NOT_FOUND, .OBJECT_PATH_NOT_FOUND => error.Missing,
            else => error.IoFailure,
        };
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

pub const Shard = struct {
    io: Io,
    dir: Dir,
    lock: Io.File,
    index: catalog.Index,
    number: u4,

    pub fn open(store: *const Store, number: u4, create: bool, counters: *catalog.Counters) Error!Shard {
        return openWith(store, number, create, counters, false);
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
        const lock = dir.createFile(store.io, "coordination", .{ .read = true, .truncate = false, .lock = .exclusive, .lock_nonblocking = !required }) catch |err| switch (err) {
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

pub const Staging = struct {
    store: *const Store,
    key: catalog.Key,
    slot: u64,
    generation: u64,
    lease: Io.File,
    path: []u8,

    /// Closing a failed staging operation leaves a registered, abandoned slot.
    /// Maintenance can reclaim it after the staging window; no finalizer must
    /// run for its ownership to become recoverable.
    pub fn deinit(self: *Staging) void {
        self.lease.close(self.store.io);
        self.store.allocator.free(self.path);
    }

    pub fn publish(self: *Staging) Error!void {
        var counters = catalog.Counters{};
        var shard = try Shard.open(self.store, Store.shardFor(self.key), false, &counters);
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
            var old = try Metadata.decode(previous.value);
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
    }
};

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
            relative = if (std.mem.lastIndexOfScalar(u8, relative, '/')) |index| relative[0..index] else "";
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

pub fn slotStem(buffer: *[128]u8, slot: u64) Error![]const u8 {
    var bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &bytes, slot, .big);
    return std.fmt.bufPrint(buffer, "data/{x:0>2}/{x:0>2}/{x:0>2}/{x:0>2}/{x:0>2}/{x:0>2}/{x:0>2}/{x:0>2}", .{
        bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
    }) catch return error.PathTooLong;
}

pub fn storageKey(generation: u64) catalog.Key {
    var key: catalog.Key = @splat(0);
    std.mem.writeInt(u64, key[0..8], generation, .big);
    return key;
}

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
    // Publication ownership is exclusive until the producer closes it.
    second.deinit();
    second_open = false;
    var accepted = (try store.acquire(id, 2)).?;
    defer accepted.deinit();
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
