//! Successful, silent literal discovery of closed test roots, keyed by the
//! checked producer's semantic dependency identity. This is independent of
//! test execution: an application build can settle a test's discovery too.
const std = @import("std");
const base = @import("base");
const check = @import("check");
const roc_target = @import("roc_target");
const CacheManager = @import("cache_manager.zig").CacheManager;
const Allocator = std.mem.Allocator;
const checked = check.CheckedArtifact;
const Key = [32]u8;
const magic = "ROC_DISCOVERY_ROOTS_1";
const scope = "semantic-test-root-discovery-v1";

/// The cache key for one test root's discovery result, covering its checked
/// identity, the native target, and the checking context.
pub fn rootKey(artifact: *const checked.CheckedModuleArtifact, request: checked.RootRequest) Key {
    const root = request.compile_time_root orelse base.invariant("semantic validation root lacks checked identity", .{});
    var hash = base.Sha256.init(.{});
    hash.update(scope);
    hash.update(@tagName(roc_target.RocTarget.detectNative()));
    hash.update(&artifact.compile_time_root_identities.test_rows[@backingInt(root)].value);
    if (artifact.checking_context_identity.platform_app_relation) |relation| hash.update(&relation.bytes);
    return hash.finalResult();
}

/// One small manifest per source placement. Its entries are independent
/// semantic facts, so retaining another test's fact never depends on the
/// source version or the order/number of tests in this compilation.
pub const Cache = struct {
    allocator: Allocator,
    manager: ?*CacheManager,
    namespace: []const u8 = scope,
    groups: std.AutoHashMapUnmanaged(Key, Group) = .empty,
    hits: u32 = 0,
    const Group = struct {
        previous: std.AutoHashMapUnmanaged(Key, void) = .empty,
        current: std.AutoHashMapUnmanaged(Key, void) = .empty,
        fn deinit(self: *Group, allocator: Allocator) void {
            self.previous.deinit(allocator);
            self.current.deinit(allocator);
        }
    };

    pub fn deinit(self: *Cache) void {
        var groups = self.groups.valueIterator();
        while (groups.next()) |group| group.deinit(self.allocator);
        self.groups.deinit(self.allocator);
    }

    /// Register a root this compilation will either discover or restore.
    /// The caller may omit only discovery, never runtime execution, on a hit.
    pub fn lookup(self: *Cache, placement: Key, key: Key) Allocator.Error!bool {
        const manager = self.manager orelse return false;
        if (!manager.config.enabled) return false;
        const entry = try self.groups.getOrPut(self.allocator, placement);
        if (!entry.found_existing) {
            entry.value_ptr.* = .{};
            if (manager.loadFamilyBytes(.local_discovery, placement, self.namespace)) |bytes| {
                defer manager.allocator.free(bytes);
                if (decode(bytes, placement)) |keys| {
                    var offset: usize = 0;
                    while (offset < keys.len) : (offset += 32) try entry.value_ptr.previous.put(self.allocator, keys[offset..][0..32].*, {});
                }
            }
        }
        return entry.value_ptr.previous.contains(key);
    }

    pub fn retain(self: *Cache, placement: Key, key: Key) Allocator.Error!void {
        _ = try self.lookup(placement, key);
        if (self.groups.getPtr(placement)) |group| try group.current.put(self.allocator, key, {});
    }

    pub fn register(self: *Cache, placement: Key, key: Key) Allocator.Error!bool {
        const hit = try self.lookup(placement, key);
        try self.retain(placement, key);
        return hit;
    }

    /// Called only after all registered roots completed literal discovery,
    /// checking reported no error, and compile-time evaluation was silent.
    pub fn publish(self: *Cache) Allocator.Error!void {
        const manager = self.manager orelse return;
        var groups = self.groups.iterator();
        while (groups.next()) |entry| {
            var keys = std.ArrayList(Key).empty;
            defer keys.deinit(self.allocator);
            var current = entry.value_ptr.current.keyIterator();
            while (current.next()) |key| try keys.append(self.allocator, key.*);
            std.mem.sort(Key, keys.items, {}, struct {
                fn less(_: void, a: Key, b: Key) bool {
                    return std.mem.order(u8, &a, &b) == .lt;
                }
            }.less);
            var bytes = std.ArrayList(u8).empty;
            defer bytes.deinit(self.allocator);
            try bytes.appendSlice(self.allocator, magic);
            try bytes.appendSlice(self.allocator, entry.key_ptr);
            try bytes.appendSlice(self.allocator, std.mem.sliceAsBytes(keys.items));
            var hash = base.Sha256.init(.{});
            hash.update(bytes.items);
            try bytes.appendSlice(self.allocator, &hash.finalResult());
            manager.storeFamilyBytes(.local_discovery, entry.key_ptr.*, bytes.items, self.namespace, "root validation");
        }
    }
};

fn decode(bytes: []const u8, placement: Key) ?[]const u8 {
    const header = magic.len + 32;
    if (bytes.len < header + 32 or (bytes.len - header - 32) % 32 != 0) return null;
    if (!std.mem.eql(u8, bytes[0..magic.len], magic)) return null;
    if (!std.mem.eql(u8, bytes[magic.len..][0..32], &placement)) return null;
    const end = bytes.len - 32;
    var hash = base.Sha256.init(.{});
    hash.update(bytes[0..end]);
    if (!std.mem.eql(u8, &hash.finalResult(), bytes[end..])) return null;
    return bytes[header..end];
}
