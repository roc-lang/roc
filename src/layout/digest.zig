//! Content digests for committed layouts.
//!
//! A `layout.Idx` is a dense per-program index: two programs that commit the
//! same layouts in a different order number them differently, so anything
//! named after an index (a refcount helper symbol, a cache key) is local to
//! one program. This module gives every committed layout a digest that depends
//! only on the layout's structure, so equal layouts digest identically in every
//! program and on every target.
//!
//! An acyclic layout digests its own encoding together with its children's
//! digests. A layout committed as part of a recursive graph already carries the
//! store's canonical recursive-graph key, the bisimulation-reduced identity of
//! the node's position in its group, and digests that key instead; that is
//! what terminates the walk through cycles. Derived data such as sizes, offsets,
//! and refcount summaries is excluded: it is a function of the structure and
//! the target, and the digest must not depend on the target.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const graph_mod = @import("graph.zig");
const layout_mod = @import("layout.zig");
const Store = @import("store.zig").Store;

const Allocator = std.mem.Allocator;
const TypeDigestHasher = base.TypeDigestHasher;
const Idx = layout_mod.Idx;

/// SHA-256 content digest of one committed layout.
pub const Digest = [TypeDigestHasher.digest_length]u8;

/// Number of hex characters of a digest rendered into a symbol name (128 bits).
pub const symbol_hex_length = 32;

const domain = "roc.layout.digest.v1";

/// Memoized content digests over one committed layout store.
pub const Digests = struct {
    allocator: Allocator,
    store: *const Store,
    memo: collections.DenseMap(Idx, Digest),
    /// Canonical recursive-graph key of every cyclic committed layout.
    recursive_keys: collections.DenseMap(Idx, RecursiveKey),
    /// Layouts whose structural digest is being computed, to turn an
    /// unexpected cycle into a diagnostic rather than unbounded recursion.
    visiting: collections.DenseMap(Idx, void),

    const RecursiveKey = [32]u8;

    pub fn init(allocator: Allocator, store: *const Store) Allocator.Error!Digests {
        var recursive_keys = collections.DenseMap(Idx, RecursiveKey).init(allocator);
        errdefer recursive_keys.deinit();
        try recursive_keys.ensureTotalCapacity(store.interned_recursive_graphs.count());
        var it = store.interned_recursive_graphs.iterator();
        while (it.next()) |entry| {
            // Hash-map iteration order is not canonical; if several keys ever
            // name one layout, the smallest key wins so the digest is stable.
            const gop = recursive_keys.getOrPutAssumeCapacity(entry.value_ptr.*);
            if (!gop.found_existing or bytesLessThan(entry.key_ptr, gop.value_ptr)) {
                gop.value_ptr.* = entry.key_ptr.*;
            }
        }
        return .{
            .allocator = allocator,
            .store = store,
            .memo = collections.DenseMap(Idx, Digest).init(allocator),
            .recursive_keys = recursive_keys,
            .visiting = collections.DenseMap(Idx, void).init(allocator),
        };
    }

    pub fn deinit(self: *Digests) void {
        self.memo.deinit();
        self.recursive_keys.deinit();
        self.visiting.deinit();
    }

    /// Content digest of one committed layout. A layout's digest hashes
    /// its children's, so children are digested first; layouts wait on an
    /// explicit stack, so layout nesting never becomes native call depth.
    pub fn get(self: *Digests, idx: Idx) Allocator.Error!Digest {
        if (self.memo.get(idx)) |digest| return digest;
        const Pending = struct { idx: Idx, expanded: bool = false };
        var pending: std.ArrayList(Pending) = .empty;
        defer pending.deinit(self.allocator);
        errdefer for (pending.items) |item| {
            if (item.expanded) _ = self.visiting.remove(item.idx);
        };
        try pending.append(self.allocator, .{ .idx = idx });
        while (pending.items.len != 0) {
            const top = &pending.items[pending.items.len - 1];
            const current = top.idx;
            if (self.memo.contains(current)) {
                _ = pending.pop();
                continue;
            }
            var hasher = TypeDigestHasher.init();
            writeBytes(&hasher, domain);
            if (self.recursive_keys.get(current)) |key| {
                writeBytes(&hasher, "recursive");
                hasher.update(&key);
            } else if (!top.expanded) {
                const gop = try self.visiting.getOrPut(current);
                if (gop.found_existing) {
                    base.invariant("layout digest: cyclic layout {d} has no recursive-graph key", .{@backingInt(current)});
                }
                top.expanded = true;
                try self.pushChildren(&pending, current);
                continue;
            } else {
                _ = self.visiting.remove(current);
                try self.writeStructural(&hasher, current);
            }
            try self.memo.put(current, hasher.finalResult());
            _ = pending.pop();
        }
        return self.memo.get(idx).?;
    }

    /// Push the children of `idx` whose digests are not yet known.
    fn pushChildren(self: *Digests, pending: anytype, idx: Idx) Allocator.Error!void {
        const layout = self.store.getLayout(idx);
        switch (layout.tag) {
            .scalar, .box_of_zst, .list_of_zst, .erased_box, .erased_callable, .zst => {},
            .box, .list, .ptr => try self.pushChild(pending, layout.getIdx()),
            .closure => try self.pushChild(pending, layout.getClosure().captures_layout_idx),
            .struct_ => {
                const fields = self.store.struct_fields.sliceRange(self.store.getStructData(layout.getStruct().idx).getFields());
                for (0..fields.len) |field_index| try self.pushChild(pending, fields.get(field_index).layout);
            },
            .tag_union => {
                const variants = self.store.getTagUnionVariants(self.store.getTagUnionData(layout.getTagUnion().idx));
                for (0..variants.len) |variant_index| try self.pushChild(pending, variants.get(variant_index).payload_layout);
            },
        }
    }

    fn pushChild(self: *Digests, pending: anytype, child: Idx) Allocator.Error!void {
        if (self.memo.contains(child)) return;
        try pending.append(self.allocator, .{ .idx = child });
    }

    fn writeStructural(self: *Digests, hasher: *TypeDigestHasher, idx: Idx) Allocator.Error!void {
        const layout = self.store.getLayout(idx);
        writeBytes(hasher, @tagName(layout.tag));
        switch (layout.tag) {
            .scalar => {
                const scalar = layout.getScalar();
                writeBytes(hasher, @tagName(scalar.tag));
                switch (scalar.tag) {
                    .int => writeBytes(hasher, @tagName(scalar.getInt())),
                    .frac => writeBytes(hasher, @tagName(scalar.getFrac())),
                    .vector => writeBytes(hasher, @tagName(scalar.getVector())),
                    .str, .opaque_ptr => {},
                }
            },
            .box, .list, .ptr => try self.writeChild(hasher, layout.getIdx()),
            .closure => try self.writeChild(hasher, layout.getClosure().captures_layout_idx),
            .box_of_zst, .list_of_zst, .erased_box, .erased_callable, .zst => {},
            .struct_ => {
                const struct_layout = layout.getStruct();
                writeBytes(hasher, @tagName(struct_layout.sort_key));
                const fields = self.store.struct_fields.sliceRange(self.store.getStructData(struct_layout.idx).getFields());
                writeU32(hasher, @intCast(fields.len));
                for (0..fields.len) |field_index| {
                    const field = fields.get(field_index);
                    writeU32(hasher, field.index);
                    hasher.update(&[_]u8{@intFromBool(field.is_padding)});
                    try self.writeChild(hasher, field.layout);
                }
            },
            .tag_union => {
                const tag_union = layout.getTagUnion();
                writeBytes(hasher, @tagName(tag_union.sort_key));
                const data = self.store.getTagUnionData(tag_union.idx);
                const variants = self.store.getTagUnionVariants(data);
                writeU32(hasher, @intCast(variants.len));
                for (0..variants.len) |variant_index| {
                    try self.writeChild(hasher, variants.get(variant_index).payload_layout);
                }
            },
        }
    }

    /// Hash a child whose digest `get` computed before its parent's.
    fn writeChild(self: *Digests, hasher: *TypeDigestHasher, child: Idx) Allocator.Error!void {
        const digest = self.memo.get(child).?;
        hasher.update(&digest);
    }
};

/// One store's content digests, memoized across every query for the store's
/// lifetime. Code generators name a helper per layout, so per-query memos
/// would re-digest a nested layout once per enclosing layout; this memo
/// digests each layout once. Concurrent readers of an immutable store share
/// it under its lock.
pub const DigestCache = struct {
    lock: std.atomic.Mutex = .unlocked,
    digests: ?Digests = null,
    /// `interned_recursive_graphs.count()` when `digests` was built. A new
    /// recursive-graph key can name an already-digested layout, so a change
    /// rebuilds the memo.
    recursive_graph_count: usize = 0,

    pub fn create(allocator: Allocator) Allocator.Error!*DigestCache {
        const cache = try allocator.create(DigestCache);
        cache.* = .{};
        return cache;
    }

    pub fn destroy(self: *DigestCache, allocator: Allocator) void {
        self.invalidate();
        allocator.destroy(self);
    }

    /// Forget every memoized digest, for a store whose committed layouts changed in place.
    pub fn invalidate(self: *DigestCache) void {
        if (self.digests) |*digests| digests.deinit();
        self.digests = null;
    }

    pub fn get(self: *DigestCache, store: *const Store, idx: Idx) Allocator.Error!Digest {
        while (!self.lock.tryLock()) std.atomic.spinLoopHint();
        defer self.lock.unlock();
        const recursive_graph_count = store.interned_recursive_graphs.count();
        if (self.digests != null and self.recursive_graph_count != recursive_graph_count) self.invalidate();
        if (self.digests == null) {
            self.digests = try Digests.init(store.allocator, store);
            self.recursive_graph_count = recursive_graph_count;
        }
        const digests = &self.digests.?;
        // The store may have moved since the memo was built.
        digests.store = store;
        return try digests.get(idx);
    }
};

/// The leading `symbol_hex_length` hex characters of a digest, for symbol names.
pub fn symbolHex(digest: Digest) [symbol_hex_length]u8 {
    return std.fmt.bytesToHex(digest[0 .. symbol_hex_length / 2].*, .lower);
}

fn writeBytes(hasher: *TypeDigestHasher, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeU32(hasher: *TypeDigestHasher, value: u32) void {
    var buffer: [4]u8 = undefined;
    buffer[0] = @truncate(value);
    buffer[1] = @truncate(value >> 8);
    buffer[2] = @truncate(value >> 16);
    buffer[3] = @truncate(value >> 24);
    hasher.update(&buffer);
}

fn bytesLessThan(left: *const [32]u8, right: *const [32]u8) bool {
    for (left, right) |l, r| {
        if (l != r) return l < r;
    }
    return false;
}

fn digestsEqual(left: Digest, right: Digest) bool {
    for (left, right) |l, r| {
        if (l != r) return false;
    }
    return true;
}

test "scalar layouts digest stably and distinctly" {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var digests = try Digests.init(testing.allocator, &store);
    defer digests.deinit();

    try testing.expect(digestsEqual(try digests.get(.u8), try digests.get(.u8)));
    try testing.expect(!digestsEqual(try digests.get(.u8), try digests.get(.u16)));
    try testing.expect(!digestsEqual(try digests.get(.u64), try digests.get(.i64)));
    try testing.expect(!digestsEqual(try digests.get(.str), try digests.get(.u64)));
}

test "layout digests do not depend on commit order" {
    const testing = std.testing;
    const StructField = layout_mod.StructField;
    const fields = [_]StructField{
        .{ .index = 0, .layout = .u8 },
        .{ .index = 1, .layout = .str },
        .{ .index = 2, .layout = .u64 },
    };

    var first = try Store.init(testing.allocator, .u64);
    defer first.deinit();
    const first_struct = try first.putStructFields(&fields);
    const first_list = try first.insertList(first_struct);

    var second = try Store.init(testing.allocator, .u64);
    defer second.deinit();
    // Unrelated layouts committed first shift every later index.
    _ = try second.insertList(.str);
    _ = try second.insertBox(.u64);
    const second_struct = try second.putStructFields(&fields);
    const second_list = try second.insertList(second_struct);
    try testing.expect(first_list != second_list);

    var first_digests = try Digests.init(testing.allocator, &first);
    defer first_digests.deinit();
    var second_digests = try Digests.init(testing.allocator, &second);
    defer second_digests.deinit();
    try testing.expect(digestsEqual(try first_digests.get(first_struct), try second_digests.get(second_struct)));
    try testing.expect(digestsEqual(try first_digests.get(first_list), try second_digests.get(second_list)));
    try testing.expect(!digestsEqual(try first_digests.get(first_struct), try first_digests.get(first_list)));
}

fn commitConsList(store: *Store) Allocator.Error!Idx {
    // `Node := [Nil, Cons(Box(Node))]`: one union whose Cons payload boxes the union itself.
    var graph = graph_mod.Graph{};
    defer graph.deinit(std.testing.allocator);
    const union_node = try graph.reserveNode(std.testing.allocator);
    const box_node = try graph.reserveNode(std.testing.allocator);
    graph.setNode(box_node, .{ .box = .{ .local = union_node } });
    const refs = try graph.appendRefs(std.testing.allocator, &[_]graph_mod.Ref{
        .{ .canonical = .zst },
        .{ .local = box_node },
    });
    graph.setNode(union_node, .{ .tag_union = refs });
    var commit = try store.commitGraph(&graph, .{ .local = union_node });
    defer commit.deinit(std.testing.allocator);
    return commit.root_idx;
}

test "recursive layouts digest through their recursive-graph keys" {
    const testing = std.testing;
    var first = try Store.init(testing.allocator, .u64);
    defer first.deinit();
    const first_root = try commitConsList(&first);

    var second = try Store.init(testing.allocator, .u64);
    defer second.deinit();
    _ = try second.insertList(.u8);
    const second_root = try commitConsList(&second);
    try testing.expect(first_root != second_root);

    var first_digests = try Digests.init(testing.allocator, &first);
    defer first_digests.deinit();
    var second_digests = try Digests.init(testing.allocator, &second);
    defer second_digests.deinit();
    const first_digest = try first_digests.get(first_root);
    try testing.expect(digestsEqual(first_digest, try second_digests.get(second_root)));
    // The boxed payload inside the cycle digests too, and differs from its union.
    const box_idx = first.getTagUnionVariants(first.getTagUnionData(first.getLayout(first_root).getTagUnion().idx)).get(1).payload_layout;
    try testing.expect(!digestsEqual(first_digest, try first_digests.get(box_idx)));
}
