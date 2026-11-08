//! Content digests for committed layouts.
//!
//! A `layout.Idx` is a dense per-program index: two programs that commit the
//! same layouts in a different order number them differently, so anything
//! named after an index (a refcount helper symbol, a cache key) is local to
//! one program. This module gives every committed layout a digest that depends
//! only on the layout's structure, so equal layouts digest identically in every
//! program and on every target.
//!
//! A layout digests as its infinite unfolding, through the shared
//! recursive-type reduction (`base.recursive_digest`): an acyclic layout
//! digests its encoding together with its children's digests, and a layout
//! on a cycle digests its position in its group's coarsest bisimulation. The
//! digest is therefore a function of the runtime representation alone,
//! whichever graph a program committed it from, in whichever order: a
//! capture record that boxes a recursive record explicitly digests like one
//! whose recursion box the layout commit inserted. The layout store gives
//! each representation one index by these digests (see
//! `Store.commitWorkingGraph`), so equal digests name equal layouts within a
//! store and across stores. Derived data such as sizes, offsets, and refcount
//! summaries is excluded: it is a function of the structure and the target,
//! and the digest must not depend on the target.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const graph_mod = @import("graph.zig");
const layout_mod = @import("layout.zig");
const Store = @import("store.zig").Store;

const Allocator = std.mem.Allocator;
const TypeDigestHasher = base.TypeDigestHasher;
const recursive_digest = base.recursive_digest;
const Idx = layout_mod.Idx;

/// SHA-256 content digest of one committed layout.
pub const Digest = recursive_digest.Identity;

/// Number of hex characters of a digest rendered into a symbol name (128 bits).
pub const symbol_hex_length = 32;

const domain = "roc.layout.digest.v2";

/// Memoized content digests over one committed layout store.
pub const Digests = struct {
    allocator: Allocator,
    store: *const Store,
    memo: collections.DenseMap(Idx, Digest),
    /// Every recursive group an earlier query settled, so a layout queried
    /// later that rolls one out, or ties another spelling of it, digests as
    /// that group's positions.
    settled: recursive_digest.Settled,
    /// The reduction of the current query, kept between queries for its
    /// capacity.
    reduction: recursive_digest.Graph,
    /// Reduction node of every layout the current query digests.
    nodes: collections.DenseMap(Idx, u32),
    /// The layouts the current query digests, in reduction node order.
    order: std.ArrayList(Idx) = .empty,

    pub fn init(allocator: Allocator, store: *const Store) Digests {
        return .{
            .allocator = allocator,
            .store = store,
            .memo = collections.DenseMap(Idx, Digest).init(allocator),
            .settled = recursive_digest.Settled.init(allocator, domain),
            .reduction = recursive_digest.Graph.init(allocator),
            .nodes = collections.DenseMap(Idx, u32).init(allocator),
        };
    }

    pub fn deinit(self: *Digests) void {
        self.memo.deinit();
        self.settled.deinit();
        self.reduction.deinit();
        self.nodes.deinit();
        self.order.deinit(self.allocator);
    }

    /// Content digest of one committed layout. Every layout it reaches whose
    /// digest is not yet memoized is reduced in the same query, so a cycle's
    /// group settles before any rolled-out copy of it that leads into it,
    /// and all of them are memoized together.
    pub fn get(self: *Digests, idx: Idx) Allocator.Error!Digest {
        if (self.memo.get(idx)) |digest| return digest;
        self.reduction.reset();
        self.nodes.clearRetainingCapacity();
        self.order.clearRetainingCapacity();

        try self.addLayout(idx);
        var next: usize = 0;
        while (next < self.order.items.len) : (next += 1) {
            var children = Children.init(self.store, self.order.items[next]);
            while (children.next()) |child| {
                if (self.memo.contains(child) or self.nodes.contains(child)) continue;
                try self.addLayout(child);
            }
        }
        for (self.order.items, 0..) |layout_idx, node| {
            try self.encode(layout_idx, @intCast(node));
        }
        try self.reduction.resolve(&self.settled, null);

        try self.memo.ensureUnusedCapacity(self.order.items.len);
        for (self.order.items, 0..) |layout_idx, node| {
            self.memo.putAssumeCapacity(layout_idx, self.reduction.digest(@intCast(node)));
        }
        return self.memo.get(idx).?;
    }

    fn addLayout(self: *Digests, idx: Idx) Allocator.Error!void {
        const node = try self.reduction.addNode();
        std.debug.assert(node == self.order.items.len);
        try self.nodes.put(idx, node);
        try self.order.append(self.allocator, idx);
    }

    /// Hand one layout to the reduction: its content is its encoding with
    /// every child as a bare slot, and its children are the reduction's
    /// nodes or the memoized digests of layouts digested earlier.
    fn encode(self: *Digests, idx: Idx, node: u32) Allocator.Error!void {
        self.reduction.beginNode(node);
        var hasher = TypeDigestHasher.init();
        const layout = self.store.getLayout(idx);
        writeBytes(&hasher, @tagName(layout.tag));
        switch (layout.tag) {
            .scalar => {
                const scalar = layout.getScalar();
                writeBytes(&hasher, @tagName(scalar.tag));
                switch (scalar.tag) {
                    .int => writeBytes(&hasher, @tagName(scalar.getInt())),
                    .frac => writeBytes(&hasher, @tagName(scalar.getFrac())),
                    .vector => writeBytes(&hasher, @tagName(scalar.getVector())),
                    .str, .opaque_ptr => {},
                }
            },
            .box, .list, .ptr => try self.putChild(&hasher, layout.getIdx()),
            .closure => try self.putChild(&hasher, layout.getClosure().captures_layout_idx),
            .box_of_zst, .list_of_zst, .erased_callable, .zst, .hole, .dynamic => {},
            .struct_ => {
                const struct_layout = layout.getStruct();
                writeBytes(&hasher, @tagName(struct_layout.sort_key));
                const fields = self.store.struct_fields.sliceRange(self.store.getStructData(struct_layout.idx).getFields());
                writeU32(&hasher, @intCast(fields.len));
                for (0..fields.len) |field_index| {
                    const field = fields.get(field_index);
                    writeU32(&hasher, field.index);
                    hasher.update(&[_]u8{@intFromBool(field.is_padding)});
                    try self.putChild(&hasher, field.layout);
                }
            },
            .tag_union => {
                const tag_union = layout.getTagUnion();
                writeBytes(&hasher, @tagName(tag_union.sort_key));
                const variants = self.store.getTagUnionVariants(self.store.getTagUnionData(tag_union.idx));
                writeU32(&hasher, @intCast(variants.len));
                for (0..variants.len) |variant_index| {
                    try self.putChild(&hasher, variants.get(variant_index).payload_layout);
                }
            },
        }
        self.reduction.endNode(node, hasher.finalResult());
    }

    fn putChild(self: *Digests, hasher: *TypeDigestHasher, child: Idx) Allocator.Error!void {
        hasher.update(&.{0xff});
        try self.reduction.putChild(if (self.memo.get(child)) |digest|
            .{ .settled = digest }
        else
            .{ .node = self.nodes.get(child).? });
    }

    /// The child layouts of one layout, in encoding order.
    const Children = struct {
        store: *const Store,
        layout: layout_mod.Layout,
        next_index: usize = 0,

        fn init(store: *const Store, idx: Idx) Children {
            return .{ .store = store, .layout = store.getLayout(idx) };
        }

        fn next(self: *Children) ?Idx {
            const index = self.next_index;
            self.next_index += 1;
            const layout = self.layout;
            return switch (layout.tag) {
                .scalar, .box_of_zst, .list_of_zst, .erased_callable, .zst, .hole, .dynamic => null,
                .box, .list, .ptr => if (index == 0) layout.getIdx() else null,
                .closure => if (index == 0) layout.getClosure().captures_layout_idx else null,
                .struct_ => blk: {
                    const fields = self.store.struct_fields.sliceRange(self.store.getStructData(layout.getStruct().idx).getFields());
                    break :blk if (index < fields.len) fields.get(index).layout else null;
                },
                .tag_union => blk: {
                    const variants = self.store.getTagUnionVariants(self.store.getTagUnionData(layout.getTagUnion().idx));
                    break :blk if (index < variants.len) variants.get(index).payload_layout else null;
                },
            };
        }
    };
};

/// One store's content digests, memoized across every query for the store's
/// lifetime. Code generators name a helper per layout, so per-query memos
/// would re-digest a nested layout once per enclosing layout; this memo
/// digests each layout once. Concurrent readers of an immutable store share
/// it: a memoized digest is read under a shared lock, and only a digest that
/// must be computed takes the lock exclusively.
pub const DigestCache = struct {
    /// Number of shared holders, or `exclusive` while one writer holds it.
    lock_state: std.atomic.Value(u32) = .init(0),
    digests: ?Digests = null,

    const exclusive = std.math.maxInt(u32);

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

    /// Forget the memoized digests that depend on `idx`, whose layout changed
    /// in place. A memoized digest reaches every layout it depends on through
    /// `Digests.get`, which memoizes each of those layouts before the digest
    /// that depends on them, so a memo without `idx` holds no digest that
    /// depends on it and nothing needs forgetting.
    pub fn invalidateLayout(self: *DigestCache, idx: Idx) void {
        const digests = if (self.digests) |*memoized| memoized else return;
        if (digests.memo.contains(idx)) self.invalidate();
    }

    pub fn get(self: *DigestCache, store: *const Store, idx: Idx) Allocator.Error!Digest {
        if (self.memoizedDigest(idx)) |digest| return digest;

        self.lockExclusive();
        defer self.lock_state.store(0, .release);
        if (self.digests == null) self.digests = Digests.init(store.allocator, store);
        const digests = &self.digests.?;
        // The store may have moved since the memo was built.
        digests.store = store;
        return try digests.get(idx);
    }

    /// The memoized digest of `idx`, read under the shared lock.
    fn memoizedDigest(self: *DigestCache, idx: Idx) ?Digest {
        self.lockShared();
        defer _ = self.lock_state.fetchSub(1, .release);
        const digests = if (self.digests) |*current| current else return null;
        return digests.memo.get(idx);
    }

    fn lockShared(self: *DigestCache) void {
        var state = self.lock_state.load(.monotonic);
        while (true) {
            if (state == exclusive) {
                std.atomic.spinLoopHint();
                state = self.lock_state.load(.monotonic);
                continue;
            }
            state = self.lock_state.cmpxchgWeak(state, state + 1, .acquire, .monotonic) orelse return;
        }
    }

    fn lockExclusive(self: *DigestCache) void {
        while (self.lock_state.cmpxchgWeak(0, exclusive, .acquire, .monotonic) != null) {
            std.atomic.spinLoopHint();
        }
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
    var digests = Digests.init(testing.allocator, &store);
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

    var first_digests = Digests.init(testing.allocator, &first);
    defer first_digests.deinit();
    var second_digests = Digests.init(testing.allocator, &second);
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

test "recursive layouts digest the same in stores that number them differently" {
    const testing = std.testing;
    var first = try Store.init(testing.allocator, .u64);
    defer first.deinit();
    const first_root = try commitConsList(&first);

    var second = try Store.init(testing.allocator, .u64);
    defer second.deinit();
    _ = try second.insertList(.u8);
    const second_root = try commitConsList(&second);
    try testing.expect(first_root != second_root);

    var first_digests = Digests.init(testing.allocator, &first);
    defer first_digests.deinit();
    var second_digests = Digests.init(testing.allocator, &second);
    defer second_digests.deinit();
    const first_digest = try first_digests.get(first_root);
    try testing.expect(digestsEqual(first_digest, try second_digests.get(second_root)));
    // The boxed payload inside the cycle digests too, and differs from its union.
    const box_idx = first.getTagUnionVariants(first.getTagUnionData(first.getLayout(first_root).getTagUnion().idx)).get(1).payload_layout;
    try testing.expect(!digestsEqual(first_digest, try first_digests.get(box_idx)));
}

fn commitBinaryTree(store: *Store) Allocator.Error!Idx {
    // `Tree := [Leaf, Branch(Box(Tree), Box(Tree))]`.
    var graph = graph_mod.Graph{};
    defer graph.deinit(std.testing.allocator);
    const union_node = try graph.reserveNode(std.testing.allocator);
    const box_node = try graph.reserveNode(std.testing.allocator);
    const branch_node = try graph.reserveNode(std.testing.allocator);
    graph.setNode(box_node, .{ .box = .{ .local = union_node } });
    graph.setNode(branch_node, .{ .struct_ = try graph.appendFields(std.testing.allocator, &[_]graph_mod.Field{
        .{ .index = 0, .child = .{ .local = box_node } },
        .{ .index = 1, .child = .{ .local = box_node } },
    }) });
    graph.setNode(union_node, .{ .tag_union = try graph.appendRefs(std.testing.allocator, &[_]graph_mod.Ref{
        .{ .canonical = .zst },
        .{ .local = branch_node },
    }) });
    var commit = try store.commitGraph(&graph, .{ .local = union_node });
    defer commit.deinit(std.testing.allocator);
    return commit.root_idx;
}

test "memoized digests stay valid across later recursive commits" {
    const testing = std.testing;
    // Digest a recursive layout, then commit another one into the same store.
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    const list_root = try commitConsList(&store);
    const list_digest = try store.contentDigest(list_root);
    const tree_root = try commitBinaryTree(&store);
    const tree_digest = try store.contentDigest(tree_root);

    // A store that commits the two in the other order, digesting only at the end.
    var reordered = try Store.init(testing.allocator, .u64);
    defer reordered.deinit();
    const reordered_tree = try commitBinaryTree(&reordered);
    const reordered_list = try commitConsList(&reordered);
    try testing.expect(digestsEqual(list_digest, try reordered.contentDigest(reordered_list)));
    try testing.expect(digestsEqual(tree_digest, try reordered.contentDigest(reordered_tree)));
    try testing.expect(digestsEqual(list_digest, try store.contentDigest(list_root)));
    try testing.expect(!digestsEqual(list_digest, tree_digest));
}

test "captures boxed explicitly and by the commit share a layout that digests" {
    const testing = std.testing;
    // `Rec := { items: List(U64), step: [A, B({ rec: Rec })], peek: [C, D({ r: Box(Rec) })] }`:
    // the commit boxes `step`'s capture of `Rec`, which then has the same
    // layout as `peek`'s explicitly boxed capture.
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var graph = graph_mod.Graph{};
    defer graph.deinit(testing.allocator);
    const rec_node = try graph.reserveNode(testing.allocator);
    const items_node = try graph.reserveNode(testing.allocator);
    const step_node = try graph.reserveNode(testing.allocator);
    const step_capture = try graph.reserveNode(testing.allocator);
    const peek_node = try graph.reserveNode(testing.allocator);
    const peek_capture = try graph.reserveNode(testing.allocator);
    const peek_box = try graph.reserveNode(testing.allocator);
    graph.setNode(items_node, .{ .list = .{ .canonical = .u64 } });
    graph.setNode(rec_node, .{ .struct_ = try graph.appendFields(testing.allocator, &[_]graph_mod.Field{
        .{ .index = 0, .child = .{ .local = items_node } },
        .{ .index = 1, .child = .{ .local = step_node } },
        .{ .index = 2, .child = .{ .local = peek_node } },
    }) });
    graph.setNode(step_capture, .{ .struct_ = try graph.appendFields(testing.allocator, &[_]graph_mod.Field{
        .{ .index = 0, .child = .{ .local = rec_node } },
    }) });
    graph.setNode(step_node, .{ .tag_union = try graph.appendRefs(testing.allocator, &[_]graph_mod.Ref{
        .{ .canonical = .zst },
        .{ .local = step_capture },
    }) });
    graph.setNode(peek_box, .{ .box = .{ .local = rec_node } });
    graph.setNode(peek_capture, .{ .struct_ = try graph.appendFields(testing.allocator, &[_]graph_mod.Field{
        .{ .index = 0, .child = .{ .local = peek_box } },
    }) });
    graph.setNode(peek_node, .{ .tag_union = try graph.appendRefs(testing.allocator, &[_]graph_mod.Ref{
        .{ .canonical = .zst },
        .{ .local = peek_capture },
    }) });
    var commit = try store.commitGraph(&graph, .{ .local = rec_node });
    defer commit.deinit(testing.allocator);

    const capture_layout = commit.value_layouts[@backingInt(step_capture)];
    try testing.expectEqual(capture_layout, commit.value_layouts[@backingInt(peek_capture)]);
    for (commit.value_layouts) |layout_idx| _ = try store.contentDigest(layout_idx);
    try testing.expect(!digestsEqual(try store.contentDigest(commit.root_idx), try store.contentDigest(capture_layout)));
}

/// `Tree := { children: [Leaf, Branch({ tree: Tree })] }`, with the recursion
/// boxes written into the graph when `explicit_boxes`, and inserted by the
/// commit otherwise.
fn commitBoxedTree(store: *Store, explicit_boxes: bool) Allocator.Error!Idx {
    const allocator = std.testing.allocator;
    var graph = graph_mod.Graph{};
    defer graph.deinit(allocator);
    const tree_node = try graph.reserveNode(allocator);
    const union_node = try graph.reserveNode(allocator);
    const branch_node = try graph.reserveNode(allocator);
    var union_ref: graph_mod.Ref = .{ .local = union_node };
    var tree_ref: graph_mod.Ref = .{ .local = tree_node };
    if (explicit_boxes) {
        const union_box = try graph.reserveNode(allocator);
        graph.setNode(union_box, .{ .box = union_ref });
        union_ref = .{ .local = union_box };
        const tree_box = try graph.reserveNode(allocator);
        graph.setNode(tree_box, .{ .box = tree_ref });
        tree_ref = .{ .local = tree_box };
    }
    graph.setNode(tree_node, .{ .struct_ = try graph.appendFields(allocator, &[_]graph_mod.Field{
        .{ .index = 0, .child = union_ref },
    }) });
    graph.setNode(branch_node, .{ .struct_ = try graph.appendFields(allocator, &[_]graph_mod.Field{
        .{ .index = 0, .child = tree_ref },
    }) });
    graph.setNode(union_node, .{ .tag_union = try graph.appendRefs(allocator, &[_]graph_mod.Ref{
        .{ .canonical = .zst },
        .{ .local = branch_node },
    }) });
    var commit = try store.commitGraph(&graph, .{ .local = tree_node });
    defer commit.deinit(allocator);
    return commit.root_idx;
}

test "layouts with explicit and commit-inserted recursion boxes digest identically" {
    const testing = std.testing;
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    const inserted = try commitBoxedTree(&store, false);
    const inserted_digest = try store.contentDigest(inserted);
    // The second spelling commits to the layout the first one created.
    try testing.expectEqual(inserted, try commitBoxedTree(&store, true));

    // Another store commits the spellings in the other order.
    var reordered = try Store.init(testing.allocator, .u64);
    defer reordered.deinit();
    const reordered_explicit = try commitBoxedTree(&reordered, true);
    const reordered_inserted = try commitBoxedTree(&reordered, false);
    try testing.expect(digestsEqual(inserted_digest, try reordered.contentDigest(reordered_explicit)));
    try testing.expect(digestsEqual(inserted_digest, try reordered.contentDigest(reordered_inserted)));
}
