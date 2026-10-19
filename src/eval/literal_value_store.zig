//! The literal value store: completed literal-root values persisted across
//! compilations (design.md "Literal Value Store").
//!
//! One entry per literal root store key holds the root's value as a ConstStore
//! tree, the procedure identity of the conversion that produced it, and the
//! modules whose compile-time roots the conversion read. A compilation that
//! holds a module of every one of those keys restores the value in place of
//! lowering and evaluating the conversion.

const std = @import("std");
const check = @import("check");
const collections = @import("collections");
const ctx_mod = @import("ctx");
const postcheck = @import("postcheck");
const Common = postcheck.Common;

const Allocator = std.mem.Allocator;
const CoreCtx = ctx_mod.CoreCtx;
const ConstStore = check.ConstStore.ConstStore;
const ConstNodeId = check.ConstStore.ConstNodeId;
const CompactWriter = collections.CompactWriter;
const alignment = collections.SERIALIZATION_ALIGNMENT;

const magic = "roclitv1".*;

/// The fixed part of an entry file. The read modules follow it, then the
/// serialized ConstStore at `store_offset`, which is aligned for
/// deserialization in place.
const Header = extern struct {
    magic: [8]u8,
    version_hash: [32]u8,
    conversion_identity: [32]u8,
    root_node: u32,
    read_count: u32,
    store_offset: u32,
    store_len: u32,
};

/// The store for one compilation session: its cache directory and the entries
/// read so far. Entries are read lazily and kept for the session, since the
/// program's restore expressions reference their blob bytes.
pub const LiteralValueStore = struct {
    allocator: Allocator,
    roc_ctx: CoreCtx,
    root: []u8,
    version_hash: [32]u8,
    /// Every key asked so far: a loaded entry, or null for a miss, so a key
    /// is read from disk once per compilation.
    entries: std.AutoHashMap([32]u8, ?*Entry),
    verbose: bool,
    hits: u32 = 0,
    misses: u32 = 0,
    writes: u32 = 0,

    pub const Entry = struct {
        bytes: []align(alignment.toByteUnits()) u8,
        store: ConstStore,
        root: ConstNodeId,
        conversion_identity: [32]u8,
        read_modules: []const [32]u8,
    };

    pub const InitError = Allocator.Error || error{NoHomeDirectory};

    pub fn init(allocator: Allocator, cache_config: anytype, version_hash: [32]u8) InitError!*LiteralValueStore {
        const version_dir = try cache_config.getVersionCacheDir(allocator);
        defer allocator.free(version_dir);
        const namespace = "literals-" ++ std.fmt.bytesToHex(version_hash[0..8].*, .lower);
        const root = try std.fs.path.join(allocator, &.{ version_dir, namespace });
        errdefer allocator.free(root);
        const self = try allocator.create(LiteralValueStore);
        self.* = .{
            .allocator = allocator,
            .roc_ctx = cache_config.roc_ctx,
            .root = root,
            .version_hash = version_hash,
            .entries = std.AutoHashMap([32]u8, ?*Entry).init(allocator),
            .verbose = cache_config.verbose,
        };
        return self;
    }

    pub fn deinit(self: *LiteralValueStore) void {
        if (@import("builtin").link_libc and std.c.getenv("ROC_PACK_STATS") != null) {
            std.debug.print("literal value store: hits {d} misses {d} writes {d}\n", .{ self.hits, self.misses, self.writes });
        }
        var it = self.entries.valueIterator();
        while (it.next()) |maybe| if (maybe.*) |entry| {
            entry.store.deinit();
            self.allocator.free(entry.read_modules);
            self.allocator.free(entry.bytes);
            self.allocator.destroy(entry);
        };
        self.entries.deinit();
        self.allocator.free(self.root);
        self.allocator.destroy(self);
    }

    pub fn lookup(self: *LiteralValueStore) Common.LiteralValueLookup {
        return .{ .context = self, .find = find };
    }

    fn find(context: *anyopaque, key: [32]u8) ?Common.LiteralValueHit {
        const self: *LiteralValueStore = @ptrCast(@alignCast(context));
        const entry = self.load(key) catch return null;
        const loaded = entry orelse return null;
        return .{
            .conversion_identity = loaded.conversion_identity,
            .read_modules = loaded.read_modules,
            .store = &loaded.store,
            .root = loaded.root,
        };
    }

    fn entryPath(self: *const LiteralValueStore, key: [32]u8) Allocator.Error![]u8 {
        const name: [68]u8 = std.fmt.bytesToHex(key, .lower) ++ ".rlv".*;
        return std.fs.path.join(self.allocator, &.{ self.root, &name });
    }

    /// The entry for `key`, read once. An unreadable or malformed file is a
    /// miss: the store is a cache, and the compilation evaluates the root.
    fn load(self: *LiteralValueStore, key: [32]u8) Allocator.Error!?*Entry {
        const slot = try self.entries.getOrPut(key);
        if (slot.found_existing) return slot.value_ptr.*;
        slot.value_ptr.* = null;
        const path = try self.entryPath(key);
        defer self.allocator.free(path);
        const raw = self.roc_ctx.readFile(path, self.allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {
                self.misses += 1;
                return null;
            },
        };
        defer self.allocator.free(raw);
        const entry = try self.decode(raw);
        if (entry == null) self.misses += 1 else self.hits += 1;
        slot.value_ptr.* = entry;
        return entry;
    }

    fn decode(self: *LiteralValueStore, raw: []const u8) Allocator.Error!?*Entry {
        if (raw.len < @sizeOf(Header)) return null;
        var header: Header = undefined;
        @memcpy(std.mem.asBytes(&header), raw[0..@sizeOf(Header)]);
        if (!std.mem.eql(u8, &header.magic, &magic)) return null;
        if (!std.mem.eql(u8, &header.version_hash, &self.version_hash)) return null;
        const reads_start = @sizeOf(Header);
        const reads_len = @as(usize, header.read_count) * 32;
        if (raw.len < reads_start + reads_len) return null;
        if (header.store_offset < reads_start + reads_len or header.store_offset % alignment.toByteUnits() != 0) return null;
        const store_end = @as(usize, header.store_offset) + header.store_len;
        if (store_end > raw.len or header.store_len < @sizeOf(ConstStore.Serialized)) return null;
        const bytes = try self.allocator.alignedAlloc(u8, alignment, header.store_len);
        errdefer self.allocator.free(bytes);
        @memcpy(bytes, raw[header.store_offset..store_end]);
        const serialized: *const ConstStore.Serialized = @ptrCast(@alignCast(bytes.ptr));
        check.artifact_serialize.validateSerialized(ConstStore.Serialized, serialized, bytes.len) catch {
            self.allocator.free(bytes);
            return null;
        };
        const read_modules = try self.allocator.alloc([32]u8, header.read_count);
        errdefer self.allocator.free(read_modules);
        for (read_modules, 0..) |*module, index| module.* = raw[reads_start + index * 32 ..][0..32].*;
        const entry = try self.allocator.create(Entry);
        entry.* = .{
            .bytes = bytes,
            .store = serialized.deserializeWithAllocator(@intFromPtr(bytes.ptr), self.allocator),
            .root = @fromBackingInt(header.root_node),
            .conversion_identity = header.conversion_identity,
            .read_modules = read_modules,
        };
        if (@backingInt(entry.root) >= entry.store.values.items.len) {
            entry.store.deinit();
            self.allocator.free(read_modules);
            self.allocator.free(bytes);
            self.allocator.destroy(entry);
            return null;
        }
        return entry;
    }

    /// Persist a completed literal root's value. A failure to write leaves the
    /// store as it was; the next compilation evaluates the root again.
    pub fn write(
        self: *LiteralValueStore,
        key: [32]u8,
        conversion_identity: [32]u8,
        read_modules: []const [32]u8,
        store: *const ConstStore,
        root: ConstNodeId,
    ) Allocator.Error!void {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_allocator = arena.allocator();
        var writer = CompactWriter.init();
        defer writer.deinit(arena_allocator);
        const serialized = try writer.appendAlloc(arena_allocator, ConstStore.Serialized);
        try serialized.serialize(store, arena_allocator, &writer);
        const reads_start = @sizeOf(Header);
        const store_offset = std.mem.alignForward(usize, reads_start + read_modules.len * 32, alignment.toByteUnits());
        const total = store_offset + writer.total_bytes;
        const bytes = try self.allocator.alignedAlloc(u8, alignment, total);
        defer self.allocator.free(bytes);
        @memset(bytes, 0);
        const header = Header{
            .magic = magic,
            .version_hash = self.version_hash,
            .conversion_identity = conversion_identity,
            .root_node = @backingInt(root),
            .read_count = @intCast(read_modules.len),
            .store_offset = @intCast(store_offset),
            .store_len = @intCast(writer.total_bytes),
        };
        @memcpy(bytes[0..@sizeOf(Header)], std.mem.asBytes(&header));
        for (read_modules, 0..) |module, index| bytes[reads_start + index * 32 ..][0..32].* = module;
        _ = writer.writeToBuffer(bytes[store_offset..]) catch unreachable;
        const path = try self.entryPath(key);
        defer self.allocator.free(path);
        self.roc_ctx.makePath(self.root) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return,
        };
        if (self.roc_ctx.fileExists(path)) return;
        const temp_path = self.roc_ctx.writeTempFile(self.allocator, path, bytes) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return,
        };
        defer self.allocator.free(temp_path);
        self.roc_ctx.rename(temp_path, path) catch {
            self.roc_ctx.deleteFile(temp_path) catch {};
            return;
        };
        self.writes += 1;
    }
};

test "literal value store round-trips an entry" {
    const gpa = std.testing.allocator;
    var store = ConstStore.init(gpa);
    defer store.deinit();
    const a = try store.append(.{ .scalar = .{ .u64 = 7 } });
    const items = try gpa.dupe(ConstNodeId, &.{a});
    defer gpa.free(items);
    const list = try store.append(.{ .list = .{ .nodes = items } });

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    var writer = CompactWriter.init();
    defer writer.deinit(arena.allocator());
    const serialized = try writer.appendAlloc(arena.allocator(), ConstStore.Serialized);
    try serialized.serialize(&store, arena.allocator(), &writer);
    const bytes = try gpa.alignedAlloc(u8, alignment, writer.total_bytes);
    defer gpa.free(bytes);
    _ = try writer.writeToBuffer(bytes);
    const loaded_ptr: *const ConstStore.Serialized = @ptrCast(@alignCast(bytes.ptr));
    try check.artifact_serialize.validateSerialized(ConstStore.Serialized, loaded_ptr, bytes.len);
    var loaded = loaded_ptr.deserializeWithAllocator(@intFromPtr(bytes.ptr), gpa);
    defer loaded.deinit();
    const value = loaded.get(list);
    try std.testing.expect(value == .list);
    try std.testing.expectEqual(@as(usize, 1), value.list.nodes.len);
    try std.testing.expectEqual(@as(u64, 7), loaded.get(value.list.nodes[0]).scalar.u64);
}
