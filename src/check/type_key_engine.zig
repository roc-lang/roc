//! The canonical type-key engine shared by the checked-type key encoders.
//!
//! A key is the SHA-256 of a type's canonical encoding: the same bytes for
//! every type that denotes the same (possibly infinite) tree up to a
//! consistent renaming of its identity variables. An adapter describes one
//! graph node at a time as a run of bytes and ordered children; this engine
//! turns those descriptions into keys for every node reachable from a
//! request, in time close to linear in the graph rather than in the sum of
//! every node's reachable graph.
//!
//! Three rules make that possible while keeping keys canonical:
//!
//! * Node classes. Content nodes that denote the same tree share a class, so a
//!   recursive type written rolled (`R = [Nil, Cons(I64, R)]`) and unrolled
//!   one step (`S = [Nil, Cons(I64, R)]`) have one class and one key.
//!   Identity variables are atoms: a class of their own each.
//! * Relative references. A repeated identity variable is written as the
//!   distance back to its definition, and a cycle as the distance up the
//!   walk, so an encoding does not depend on what precedes it.
//! * Child references. A child outside the walked node's strongly connected
//!   component is written as a reference to its own key, plus the identity
//!   variables it shares with what the walk already defined. The child's
//!   remaining variables are defined by the reference, in the child's own
//!   order.
//!
//! Every choice depends only on the type itself, so two types equal up to
//! renaming produce identical bytes, and a reference's key stands for exactly
//! the bytes it replaces.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const Allocator = std.mem.Allocator;
const TypeDigestHasher = base.TypeDigestHasher;

/// No node, class, or map entry.
pub const nil: u32 = std.math.maxInt(u32);

/// A persistent (path-copying) treap from `u32` keys to `u32` values. Maps
/// share structure, so a node's variable order map is reused by every walk
/// that references the node without copying it. A handle adds `delta` to
/// every stored value, which shifts a whole map in constant time.
pub const PersistentMap = struct {
    const Node = struct {
        key: u32,
        value: u32,
        prio: u32,
        left: u32,
        right: u32,
        size: u32,
    };

    pub const Handle = struct {
        root: u32 = nil,
        delta: u32 = 0,

        pub fn shifted(self: Handle, by: u32) Handle {
            return .{ .root = self.root, .delta = self.delta +% by };
        }
    };

    pub const Entry = struct { key: u32, value: u32 };

    nodes: std.ArrayListUnmanaged(Node) = .empty,
    walk_stack: std.ArrayListUnmanaged(u32) = .empty,

    pub fn deinit(self: *PersistentMap, gpa: Allocator) void {
        self.nodes.deinit(gpa);
        self.walk_stack.deinit(gpa);
    }

    pub fn clearRetainingCapacity(self: *PersistentMap) void {
        self.nodes.clearRetainingCapacity();
    }

    pub fn count(self: *const PersistentMap, handle: Handle) u32 {
        return if (handle.root == nil) 0 else self.nodes.items[handle.root].size;
    }

    pub fn get(self: *const PersistentMap, handle: Handle, key: u32) ?u32 {
        var index = handle.root;
        while (index != nil) {
            const node = self.nodes.items[index];
            if (key == node.key) return node.value +% handle.delta;
            index = if (key < node.key) node.left else node.right;
        }
        return null;
    }

    /// Map `key` to `value` in a new version of `handle`'s map, replacing an
    /// existing entry.
    pub fn put(self: *PersistentMap, gpa: Allocator, handle: Handle, key: u32, value: u32) Allocator.Error!Handle {
        const stored = value -% handle.delta;
        const root = if (self.get(handle, key) != null)
            try self.replaceAt(gpa, handle.root, key, stored)
        else
            try self.insertAt(gpa, handle.root, key, stored, priority(key));
        return .{ .root = root, .delta = handle.delta };
    }

    /// Like `put`, leaving an existing entry as it is.
    pub fn putIfAbsent(self: *PersistentMap, gpa: Allocator, handle: Handle, key: u32, value: u32) Allocator.Error!Handle {
        if (self.get(handle, key) != null) return handle;
        return .{ .root = try self.insertAt(gpa, handle.root, key, value -% handle.delta, priority(key)), .delta = handle.delta };
    }

    /// Append every entry of `handle`'s map to `out`, in key order.
    pub fn appendEntries(self: *PersistentMap, gpa: Allocator, handle: Handle, out: *std.ArrayListUnmanaged(Entry)) Allocator.Error!void {
        self.walk_stack.clearRetainingCapacity();
        var index = handle.root;
        while (true) {
            while (index != nil) {
                try self.walk_stack.append(gpa, index);
                index = self.nodes.items[index].left;
            }
            const top = self.walk_stack.pop() orelse return;
            const node = self.nodes.items[top];
            try out.append(gpa, .{ .key = node.key, .value = node.value +% handle.delta });
            index = node.right;
        }
    }

    fn priority(key: u32) u32 {
        return std.hash.int(key);
    }

    fn sizeOf(self: *const PersistentMap, index: u32) u32 {
        return if (index == nil) 0 else self.nodes.items[index].size;
    }

    fn newNode(self: *PersistentMap, gpa: Allocator, key: u32, value: u32, prio: u32, left: u32, right: u32) Allocator.Error!u32 {
        const index: u32 = @intCast(self.nodes.items.len);
        try self.nodes.append(gpa, .{
            .key = key,
            .value = value,
            .prio = prio,
            .left = left,
            .right = right,
            .size = 1 + self.sizeOf(left) + self.sizeOf(right),
        });
        return index;
    }

    fn higher(prio: u32, key: u32, other: Node) bool {
        return prio > other.prio or (prio == other.prio and key < other.key);
    }

    const Split = struct { lo: u32, hi: u32 };

    /// Split a subtree that does not contain `key` into keys below and above
    /// it. Depth is the treap's expected logarithmic height.
    fn split(self: *PersistentMap, gpa: Allocator, index: u32, key: u32) Allocator.Error!Split {
        if (index == nil) return .{ .lo = nil, .hi = nil };
        const node = self.nodes.items[index];
        if (key < node.key) {
            const parts = try self.split(gpa, node.left, key);
            return .{ .lo = parts.lo, .hi = try self.newNode(gpa, node.key, node.value, node.prio, parts.hi, node.right) };
        }
        const parts = try self.split(gpa, node.right, key);
        return .{ .lo = try self.newNode(gpa, node.key, node.value, node.prio, node.left, parts.lo), .hi = parts.hi };
    }

    fn insertAt(self: *PersistentMap, gpa: Allocator, index: u32, key: u32, value: u32, prio: u32) Allocator.Error!u32 {
        if (index == nil) return try self.newNode(gpa, key, value, prio, nil, nil);
        const node = self.nodes.items[index];
        if (higher(prio, key, node)) {
            const parts = try self.split(gpa, index, key);
            return try self.newNode(gpa, key, value, prio, parts.lo, parts.hi);
        }
        if (key < node.key) {
            const left = try self.insertAt(gpa, node.left, key, value, prio);
            return try self.newNode(gpa, node.key, node.value, node.prio, left, node.right);
        }
        const right = try self.insertAt(gpa, node.right, key, value, prio);
        return try self.newNode(gpa, node.key, node.value, node.prio, node.left, right);
    }

    fn replaceAt(self: *PersistentMap, gpa: Allocator, index: u32, key: u32, value: u32) Allocator.Error!u32 {
        const node = self.nodes.items[index];
        if (key == node.key) return try self.newNode(gpa, key, value, node.prio, node.left, node.right);
        if (key < node.key) {
            const left = try self.replaceAt(gpa, node.left, key, value);
            return try self.newNode(gpa, node.key, node.value, node.prio, left, node.right);
        }
        const right = try self.replaceAt(gpa, node.right, key, value);
        return try self.newNode(gpa, node.key, node.value, node.prio, node.left, right);
    }
};

/// How the walk treats a node.
pub const NodeKind = enum(u8) {
    /// Structure: encoded in place and eligible for class merging.
    content,
    /// A type variable identified by its own node. Its first occurrence in a
    /// walk writes the description (a header, then its constraints); later
    /// occurrences write a reference back to it.
    identity,
    /// Context-free bytes with no children, written wherever the node occurs.
    leaf,
};

/// One piece of a description: a byte run of `Store.bytes`, or a child node.
pub const Item = struct {
    start: u32,
    len: u32,
    child: u32,

    pub fn isChild(self: Item) bool {
        return self.child != nil;
    }
};

/// A node's description as an adapter produced it.
pub const Desc = struct {
    kind: NodeKind,
    items_start: u32,
    items_len: u32,
    /// Every byte run of the description, contiguous in `Store.bytes`.
    bytes_start: u32,
    bytes_len: u32,
    /// A leaf standing for an identity variable that keeps its store identity
    /// (a checker-local anchor): it makes the type contain identities.
    counts_identity: bool,
    /// An identity with no constraints is always written in place.
    constraint_count: u32,
    /// The node's own content is erroneous (see `Sink.contains_error`).
    contains_error: bool,
};

/// Descriptions of every node a request reached, in one flat store.
pub const Store = struct {
    bytes: std.ArrayListUnmanaged(u8) = .empty,
    items: std.ArrayListUnmanaged(Item) = .empty,
    descs: std.ArrayListUnmanaged(Desc) = .empty,

    pub fn deinit(self: *Store, gpa: Allocator) void {
        self.bytes.deinit(gpa);
        self.items.deinit(gpa);
        self.descs.deinit(gpa);
    }

    pub fn clearRetainingCapacity(self: *Store) void {
        self.bytes.clearRetainingCapacity();
        self.items.clearRetainingCapacity();
        self.descs.clearRetainingCapacity();
    }

    pub fn itemsOf(self: *const Store, desc: Desc) []const Item {
        return self.items.items[desc.items_start..][0..desc.items_len];
    }

    pub fn bytesOf(self: *const Store, item: Item) []const u8 {
        return self.bytes.items[item.start..][0..item.len];
    }
};

/// The interface an adapter writes one node's description through. Bytes
/// accumulate into one run until a child ends it.
pub const Sink = struct {
    gpa: Allocator,
    store: *Store,
    run_start: u32,
    items_start: u32,
    /// Traversals that only need children and flags skip the bytes.
    record_bytes: bool = true,
    counts_identity: bool = false,
    constraint_count: u32 = 0,
    /// Set by an adapter whose node's own content is erroneous, so a
    /// traversal can report errors reachable through the same children the
    /// key encodes.
    contains_error: bool = false,

    pub fn bytes(self: *Sink, data: []const u8) Allocator.Error!void {
        if (!self.record_bytes) return;
        try self.store.bytes.appendSlice(self.gpa, data);
    }

    pub fn byte(self: *Sink, value: u8) Allocator.Error!void {
        if (!self.record_bytes) return;
        try self.store.bytes.append(self.gpa, value);
    }

    pub fn boolean(self: *Sink, value: bool) Allocator.Error!void {
        try self.byte(if (value) 1 else 0);
    }

    /// Unsigned LEB128, matching the key encoding's integers.
    pub fn varint(self: *Sink, value: u32) Allocator.Error!void {
        if (!self.record_bytes) return;
        var rest = value;
        while (rest >= 0x80) : (rest >>= 7) try self.byte(@as(u8, @truncate(rest)) | 0x80);
        try self.byte(@truncate(rest));
    }

    /// A length-prefixed byte string.
    pub fn text(self: *Sink, data: []const u8) Allocator.Error!void {
        if (!self.record_bytes) return;
        try self.varint(@intCast(data.len));
        try self.bytes(data);
    }

    /// A child at this point of the description. `node` must already be the
    /// adapter's canonical node for the child.
    pub fn child(self: *Sink, node: u32) Allocator.Error!void {
        try self.flushRun();
        try self.store.items.append(self.gpa, .{ .start = 0, .len = 0, .child = node });
    }

    fn flushRun(self: *Sink) Allocator.Error!void {
        const end: u32 = @intCast(self.store.bytes.items.len);
        if (end > self.run_start) {
            try self.store.items.append(self.gpa, .{ .start = self.run_start, .len = end - self.run_start, .child = nil });
        }
        self.run_start = end;
    }
};

/// Describe `node` through `adapter`, appending the description to `store`.
pub fn describe(
    comptime Adapter: type,
    adapter: *Adapter,
    gpa: Allocator,
    store: *Store,
    node: u32,
    record_bytes: bool,
) Allocator.Error!Desc {
    const bytes_start: u32 = @intCast(store.bytes.items.len);
    var sink = Sink{
        .gpa = gpa,
        .store = store,
        .run_start = bytes_start,
        .items_start = @intCast(store.items.items.len),
        .record_bytes = record_bytes,
    };
    const kind = try adapter.describe(node, &sink);
    try sink.flushRun();
    const desc = Desc{
        .kind = kind,
        .items_start = sink.items_start,
        .items_len = @as(u32, @intCast(store.items.items.len)) - sink.items_start,
        .bytes_start = bytes_start,
        .bytes_len = @as(u32, @intCast(store.bytes.items.len)) - bytes_start,
        .counts_identity = sink.counts_identity,
        .constraint_count = sink.constraint_count,
        .contains_error = sink.contains_error,
    };
    try store.descs.append(gpa, desc);
    return desc;
}

/// Append the identity nodes reachable from `root` in depth-first
/// first-encounter order over description children, entering each
/// identity's constraints.
pub fn appendIdentityOrder(
    comptime Adapter: type,
    adapter: *Adapter,
    gpa: Allocator,
    root: u32,
    out: *std.ArrayListUnmanaged(u32),
) Allocator.Error!void {
    var store = Store{};
    defer store.deinit(gpa);
    var visited = std.AutoHashMapUnmanaged(u32, void).empty;
    defer visited.deinit(gpa);
    const Frame = struct { desc: Desc, item: u32 };
    var frames = std.ArrayListUnmanaged(Frame).empty;
    defer frames.deinit(gpa);

    var next: ?u32 = adapter.resolve(root);
    while (true) {
        if (next) |node| {
            next = null;
            if (!(try visited.getOrPut(gpa, node)).found_existing) {
                const desc = try describe(Adapter, adapter, gpa, &store, node, false);
                if (desc.kind == .identity) try out.append(gpa, node);
                if (desc.kind != .leaf) try frames.append(gpa, .{ .desc = desc, .item = 0 });
            }
        }
        if (frames.items.len == 0) return;
        const frame = &frames.items[frames.items.len - 1];
        const items = store.itemsOf(frame.desc);
        if (frame.item >= items.len) {
            frames.items.len -= 1;
            continue;
        }
        const item = items[frame.item];
        frame.item += 1;
        if (item.isChild()) next = item.child;
    }
}

/// Everything a walk computed about a class: its key and, when it contains
/// identity variables, their order in its own encoding.
pub const Summary = struct {
    key: [32]u8,
    /// Identity variables the class's encoding defines.
    nvars: u32,
    /// Each defined variable's class to its index in that order.
    order: PersistentMap.Handle,
    contains_identity: bool,
    /// Whether the encoding is free of identities and the class is not part
    /// of a cycle, so a function over such children has the key of their
    /// composed references.
    composable: bool,
};

/// Tags the engine itself writes, taken from the adapter's encoding.
pub const Tags = struct {
    identity_ref: u8,
    cycle: u8,
    child_key: u8,
    child_key_mapped: u8,
};

/// Keys for the graph an `Adapter` describes: it resolves a node to its
/// canonical node (`resolve`) and writes one node's description through a
/// `Sink` (`describe`). Classes and keys persist until `reset`.
pub fn Engine(comptime Adapter: type) type {
    return struct {
        const Self = @This();

        const Class = struct {
            /// A member node; every member has the same description shape.
            rep: u32,
            /// The representative's description.
            desc: u32,
            kind: NodeKind,
            /// Start in `class_children` of the representative's step words
            /// (see `appendStepWords`). A leaf or content class has them from
            /// creation; an identity class gets them when first keyed.
            children: u32 = nil,
            /// Hash of the class's one-step structure, for `class_by_step`.
            step_hash: u64 = 0,
            /// Tarjan index while the class is being keyed.
            tarjan: u32 = nil,
            key_scc: u32 = nil,
            summary: u32 = nil,
            /// The class lies on a cycle of classes: it denotes an infinite
            /// tree that contains itself.
            on_cycle: bool = false,
        };

        gpa: Allocator,
        tags: Tags,
        store: Store = .{},
        node_info: collections.DenseMap(u32, NodeInfo),
        classes: std.ArrayListUnmanaged(Class) = .empty,
        class_children: std.ArrayListUnmanaged(u32) = .empty,
        /// Leaf and content classes by their one-step structure: the
        /// representative's kind, byte runs, and child classes. Looked up by
        /// a node's own description, so no signature is materialized.
        class_by_step: std.HashMapUnmanaged(u32, void, StepContext, std.hash_map.default_max_load_percentage) = .empty,
        /// Classes of cyclic components by their canonical walk.
        class_by_walk: std.HashMapUnmanaged(Span, u32, SpanContext, std.hash_map.default_max_load_percentage) = .empty,
        walk_bytes: std.ArrayListUnmanaged(u8) = .empty,
        summaries: std.ArrayListUnmanaged(Summary) = .empty,
        scc_cyclic: std.ArrayListUnmanaged(bool) = .empty,
        maps: PersistentMap = .{},
        /// Steps every walk has taken: items written and variable-map entries
        /// visited. A deterministic measure of keying cost for tests.
        work: u64 = 0,

        // Request scratch.
        pending: std.ArrayListUnmanaged(u32) = .empty,
        tarjan_low: std.ArrayListUnmanaged(u32) = .empty,
        tarjan_on_stack: std.ArrayListUnmanaged(bool) = .empty,
        tarjan_stack: std.ArrayListUnmanaged(u32) = .empty,
        tarjan_frames: std.ArrayListUnmanaged(TarjanFrame) = .empty,
        scc_members: std.ArrayListUnmanaged(u32) = .empty,
        signature: std.ArrayListUnmanaged(u8) = .empty,
        step_words: std.ArrayListUnmanaged(u32) = .empty,
        local_of: collections.DenseMap(u32, u32),
        state_of_class: collections.DenseMap(u32, u32),
        state_nodes: std.ArrayListUnmanaged(u32) = .empty,
        state_class: std.ArrayListUnmanaged(u32) = .empty,
        local_class: std.ArrayListUnmanaged(u32) = .empty,
        local_globals: std.ArrayListUnmanaged(u32) = .empty,
        block_existing: std.ArrayListUnmanaged(u32) = .empty,
        next_local_class: std.ArrayListUnmanaged(u32) = .empty,
        local_groups: std.StringHashMapUnmanaged(u32) = .empty,
        local_group_arena: std.heap.ArenaAllocator,
        dfs_index: collections.DenseMap(u32, u32),
        dfs_frames: std.ArrayListUnmanaged(DfsFrame) = .empty,
        buf: std.ArrayListUnmanaged(u8) = .empty,
        walk_frames: std.ArrayListUnmanaged(WalkFrame) = .empty,
        active: std.ArrayListUnmanaged(u32) = .empty,
        entries: std.ArrayListUnmanaged(PersistentMap.Entry) = .empty,
        pairs: std.ArrayListUnmanaged(PersistentMap.Entry) = .empty,

        /// What the engine knows about one node: its description, then its
        /// class, and while it is being classified its Tarjan index.
        const NodeInfo = struct {
            desc: u32,
            class: u32 = nil,
            tarjan: u32 = nil,
        };

        const Span = struct { start: u32, len: u32 };

        /// Hashes interned walks by their bytes in `walk_bytes`.
        const SpanContext = struct {
            bytes: []const u8,

            pub fn hash(self: SpanContext, span: Span) u64 {
                return std.hash.Wyhash.hash(0, self.bytes[span.start..][0..span.len]);
            }

            pub fn eql(self: SpanContext, a: Span, b: Span) bool {
                return sliceEql(u8, self.bytes[a.start..][0..a.len], self.bytes[b.start..][0..b.len]);
            }
        };

        /// Looks interned walks up by candidate bytes.
        const SignatureContext = struct {
            bytes: []const u8,

            pub fn hash(_: SignatureContext, signature: []const u8) u64 {
                return std.hash.Wyhash.hash(0, signature);
            }

            pub fn eql(self: SignatureContext, signature: []const u8, span: Span) bool {
                return sliceEql(u8, signature, self.bytes[span.start..][0..span.len]);
            }
        };

        /// Hashes a class in `class_by_step` by its stored step hash.
        const StepContext = struct {
            classes: []const Class,

            pub fn hash(self: StepContext, class: u32) u64 {
                return self.classes[class].step_hash;
            }

            pub fn eql(_: StepContext, a: u32, b: u32) bool {
                return a == b;
            }
        };

        /// A node's one-step structure: its description and its step words.
        const Step = struct {
            desc: Desc,
            words: []const u32,
            hash: u64,
        };

        /// Looks a one-step structure up in `class_by_step`.
        const StepLookup = struct {
            engine: *const Self,

            pub fn hash(_: StepLookup, step: Step) u64 {
                return step.hash;
            }

            pub fn eql(self: StepLookup, step: Step, class: u32) bool {
                const engine = self.engine;
                const record = engine.classes.items[class];
                const desc = engine.store.descs.items[record.desc];
                if (desc.kind != step.desc.kind or desc.items_len != step.desc.items_len) return false;
                if (!sliceEql(u32, engine.class_children.items[record.children..][0..desc.items_len], step.words)) return false;
                return sliceEql(u8, engine.descBytes(desc), engine.descBytes(step.desc));
            }
        };

        /// A step word of a child item is its class; of a byte run, its
        /// length with this bit set. Class ids stay below it.
        const run_bit: u32 = 1 << 31;

        const TarjanFrame = struct { node: u32, item: u32 };
        const DfsFrame = struct { node: u32, item: u32 };
        const WalkFrame = struct { class: u32, item: u32 };

        pub fn init(gpa: Allocator, tags: Tags) Self {
            return .{
                .gpa = gpa,
                .tags = tags,
                .node_info = collections.DenseMap(u32, NodeInfo).init(gpa),
                .local_of = collections.DenseMap(u32, u32).init(gpa),
                .state_of_class = collections.DenseMap(u32, u32).init(gpa),
                .local_group_arena = std.heap.ArenaAllocator.init(gpa),
                .dfs_index = collections.DenseMap(u32, u32).init(gpa),
            };
        }

        pub fn deinit(self: *Self) void {
            const gpa = self.gpa;
            self.store.deinit(gpa);
            self.node_info.deinit();
            self.classes.deinit(gpa);
            self.class_children.deinit(gpa);
            self.class_by_step.deinit(gpa);
            self.class_by_walk.deinit(gpa);
            self.walk_bytes.deinit(gpa);
            self.summaries.deinit(gpa);
            self.scc_cyclic.deinit(gpa);
            self.maps.deinit(gpa);
            self.pending.deinit(gpa);
            self.tarjan_low.deinit(gpa);
            self.tarjan_on_stack.deinit(gpa);
            self.tarjan_stack.deinit(gpa);
            self.tarjan_frames.deinit(gpa);
            self.scc_members.deinit(gpa);
            self.signature.deinit(gpa);
            self.step_words.deinit(gpa);
            self.local_of.deinit();
            self.state_of_class.deinit();
            self.state_nodes.deinit(gpa);
            self.state_class.deinit(gpa);
            self.local_class.deinit(gpa);
            self.local_globals.deinit(gpa);
            self.block_existing.deinit(gpa);
            self.next_local_class.deinit(gpa);
            self.local_groups.deinit(gpa);
            self.local_group_arena.deinit();
            self.dfs_index.deinit();
            self.dfs_frames.deinit(gpa);
            self.buf.deinit(gpa);
            self.walk_frames.deinit(gpa);
            self.active.deinit(gpa);
            self.entries.deinit(gpa);
            self.pairs.deinit(gpa);
        }

        /// Forget every description, class, and key. Required whenever the
        /// adapter's graph may have changed since the last request.
        pub fn reset(self: *Self) void {
            self.store.clearRetainingCapacity();
            self.node_info.clearRetainingCapacity();
            self.classes.clearRetainingCapacity();
            self.class_children.clearRetainingCapacity();
            self.class_by_step.clearRetainingCapacity();
            self.class_by_walk.clearRetainingCapacity();
            self.walk_bytes.clearRetainingCapacity();
            self.summaries.clearRetainingCapacity();
            self.scc_cyclic.clearRetainingCapacity();
            self.maps.clearRetainingCapacity();
        }

        /// The summary of `root`'s class, computing every class and key
        /// reachable from it that is not already known.
        pub fn summarize(self: *Self, adapter: *Adapter, root: u32) Allocator.Error!Summary {
            const node = adapter.resolve(root);
            try self.classify(adapter, node);
            const class = self.nodeClass(node);
            try self.summarizeClass(class);
            return self.summaries.items[self.classes.items[class].summary];
        }

        // Descriptions //

        fn descOf(self: *Self, adapter: *Adapter, node: u32) Allocator.Error!Desc {
            const entry = try self.node_info.getOrPut(node);
            if (entry.found_existing) return self.store.descs.items[entry.value_ptr.desc];
            entry.value_ptr.* = .{ .desc = @intCast(self.store.descs.items.len) };
            // Describing never touches `node_info`, but an error must not
            // leave a node without its description.
            errdefer _ = self.node_info.remove(node);
            return try describe(Adapter, adapter, self.gpa, &self.store, node, true);
        }

        fn info(self: *const Self, node: u32) *const NodeInfo {
            return self.node_info.getPtrConst(node).?;
        }

        fn nodeClass(self: *const Self, node: u32) u32 {
            return self.info(node).class;
        }

        fn hasClass(self: *const Self, node: u32) bool {
            const entry = self.node_info.getPtrConst(node) orelse return false;
            return entry.class != nil;
        }

        fn setClass(self: *Self, node: u32, class: u32) void {
            self.node_info.getPtr(node).?.class = class;
        }

        fn classDesc(self: *const Self, class: u32) Desc {
            return self.store.descs.items[self.classes.items[class].desc];
        }

        // Classes //

        /// Give every node reachable from `root` a class. Content nodes are
        /// grouped by strongly connected component (identity edges are not
        /// followed: a variable is an atom), then merged by what they denote.
        fn classify(self: *Self, adapter: *Adapter, root: u32) Allocator.Error!void {
            self.pending.clearRetainingCapacity();
            try self.pending.append(self.gpa, root);
            while (self.pending.pop()) |node| {
                if (self.hasClass(node)) continue;
                const desc = try self.descOf(adapter, node);
                if (desc.kind != .content) {
                    try self.classifyAtom(node, desc);
                    continue;
                }
                try self.tarjanClasses(adapter, node);
            }
        }

        /// Identity variables and leaves. An identity's constraint children
        /// are classified as their own roots.
        fn classifyAtom(self: *Self, node: u32, desc: Desc) Allocator.Error!void {
            switch (desc.kind) {
                .identity => {
                    self.setClass(node, try self.newClass(node));
                    for (self.store.itemsOf(desc)) |item| {
                        if (item.isChild()) try self.pending.append(self.gpa, item.child);
                    }
                },
                .leaf => self.setClass(node, try self.internStep(node)),
                .content => unreachable,
            }
        }

        fn newClass(self: *Self, rep: u32) Allocator.Error!u32 {
            const class: u32 = @intCast(self.classes.items.len);
            const desc = self.info(rep).desc;
            try self.classes.append(self.gpa, .{ .rep = rep, .desc = desc, .kind = self.store.descs.items[desc].kind });
            return class;
        }

        /// The class of a leaf or of a content node whose children all have
        /// classes: an existing class with the same one-step structure, or a
        /// new one with `node` as its representative.
        fn internStep(self: *Self, node: u32) Allocator.Error!u32 {
            const step = try self.stepOf(node);
            const entry = try self.class_by_step.getOrPutContextAdapted(
                self.gpa,
                step,
                StepLookup{ .engine = self },
                StepContext{ .classes = self.classes.items },
            );
            if (entry.found_existing) return entry.key_ptr.*;
            errdefer self.class_by_step.removeByPtr(entry.key_ptr);
            const class = try self.newClass(node);
            errdefer _ = self.classes.pop();
            try self.setStep(class, step);
            entry.key_ptr.* = class;
            return class;
        }

        /// Register `class` (whose members' children all have classes) under
        /// its one-step structure, so a node written as an unrolled copy of
        /// it joins it.
        fn registerStep(self: *Self, class: u32) Allocator.Error!void {
            if (self.classes.items[class].children != nil) return;
            const step = try self.stepOf(self.classes.items[class].rep);
            try self.setStep(class, step);
            const entry = try self.class_by_step.getOrPutContextAdapted(
                self.gpa,
                step,
                StepLookup{ .engine = self },
                StepContext{ .classes = self.classes.items },
            );
            std.debug.assert(!entry.found_existing);
            entry.key_ptr.* = class;
        }

        /// The one-step structure of a node whose children all have classes,
        /// valid until the next call.
        fn stepOf(self: *Self, node: u32) Allocator.Error!Step {
            const desc = self.nodeDesc(node);
            self.step_words.clearRetainingCapacity();
            try self.appendStepWords(&self.step_words, desc);
            var hasher = std.hash.Wyhash.init(@backingInt(desc.kind));
            const words = self.step_words.items;
            hasher.update(@as([*]const u8, @ptrCast(words.ptr))[0 .. words.len * @sizeOf(u32)]);
            hasher.update(self.descBytes(desc));
            return .{ .desc = desc, .words = self.step_words.items, .hash = hasher.final() };
        }

        fn setStep(self: *Self, class: u32, step: Step) Allocator.Error!void {
            const start: u32 = @intCast(self.class_children.items.len);
            try self.class_children.appendSlice(self.gpa, step.words);
            self.classes.items[class].children = start;
            self.classes.items[class].step_hash = step.hash;
        }

        fn appendStepWords(self: *Self, out: *std.ArrayListUnmanaged(u32), desc: Desc) Allocator.Error!void {
            try out.ensureUnusedCapacity(self.gpa, desc.items_len);
            for (self.store.itemsOf(desc)) |item| {
                out.appendAssumeCapacity(if (item.isChild()) self.nodeClass(item.child) else run_bit | item.len);
            }
        }

        fn descBytes(self: *const Self, desc: Desc) []const u8 {
            return self.store.bytes.items[desc.bytes_start..][0..desc.bytes_len];
        }

        /// The class of a cyclic block with this canonical walk, created with
        /// `rep` as its representative when there is none.
        fn internWalk(self: *Self, walk_signature: []const u8, rep: u32) Allocator.Error!u32 {
            const entry = try self.class_by_walk.getOrPutContextAdapted(
                self.gpa,
                walk_signature,
                SignatureContext{ .bytes = self.walk_bytes.items },
                SpanContext{ .bytes = self.walk_bytes.items },
            );
            if (entry.found_existing) return entry.value_ptr.*;
            errdefer self.class_by_walk.removeByPtr(entry.key_ptr);
            const start: u32 = @intCast(self.walk_bytes.items.len);
            try self.walk_bytes.appendSlice(self.gpa, walk_signature);
            errdefer self.walk_bytes.shrinkRetainingCapacity(start);
            const class = try self.newClass(rep);
            entry.key_ptr.* = .{ .start = start, .len = @intCast(walk_signature.len) };
            entry.value_ptr.* = class;
            return class;
        }

        /// Tarjan indices live in `NodeInfo.tarjan`. Only a node without a
        /// class is ever looked up by them, and every node this visits has a
        /// class by the time it returns, so indices never need clearing.
        fn tarjanClasses(self: *Self, adapter: *Adapter, start: u32) Allocator.Error!void {
            self.clearTarjan();
            try self.tarjanNodeVisit(start);
            while (self.tarjan_frames.items.len > 0) {
                const frame = &self.tarjan_frames.items[self.tarjan_frames.items.len - 1];
                const desc = self.nodeDesc(frame.node);
                var descended = false;
                while (frame.item < desc.items_len) {
                    // Describing a child grows the store, so read the item
                    // afresh each time.
                    const item = self.store.itemsOf(desc)[frame.item];
                    frame.item += 1;
                    if (!item.isChild()) continue;
                    const child = item.child;
                    const child_desc = try self.descOf(adapter, child);
                    const child_info = self.info(child);
                    if (child_info.class != nil) continue;
                    if (child_desc.kind != .content) {
                        try self.classifyAtom(child, child_desc);
                        continue;
                    }
                    if (child_info.tarjan != nil) {
                        if (self.tarjan_on_stack.items[child_info.tarjan]) {
                            const own = self.info(frame.node).tarjan;
                            self.tarjan_low.items[own] = @min(self.tarjan_low.items[own], child_info.tarjan);
                        }
                        continue;
                    }
                    try self.tarjanNodeVisit(child);
                    descended = true;
                    break;
                }
                if (descended) continue;

                const done = self.tarjan_frames.pop().?;
                const own = self.info(done.node).tarjan;
                if (self.tarjan_frames.items.len > 0) {
                    const parent = self.info(self.tarjan_frames.items[self.tarjan_frames.items.len - 1].node).tarjan;
                    self.tarjan_low.items[parent] = @min(self.tarjan_low.items[parent], self.tarjan_low.items[own]);
                }
                if (self.tarjan_low.items[own] != own) continue;
                self.scc_members.clearRetainingCapacity();
                while (true) {
                    const member = self.tarjan_stack.pop().?;
                    self.tarjan_on_stack.items[self.info(member).tarjan] = false;
                    try self.scc_members.append(self.gpa, member);
                    if (member == done.node) break;
                }
                try self.classifyComponent();
            }
        }

        fn clearTarjan(self: *Self) void {
            self.tarjan_low.clearRetainingCapacity();
            self.tarjan_on_stack.clearRetainingCapacity();
            self.tarjan_stack.clearRetainingCapacity();
            self.tarjan_frames.clearRetainingCapacity();
        }

        /// Push `node` (a node, or a class when keying) as Tarjan's next
        /// visit and return its index.
        fn tarjanPush(self: *Self, node: u32) Allocator.Error!u32 {
            const index: u32 = @intCast(self.tarjan_low.items.len);
            try self.tarjan_low.append(self.gpa, index);
            try self.tarjan_on_stack.append(self.gpa, true);
            try self.tarjan_stack.append(self.gpa, node);
            try self.tarjan_frames.append(self.gpa, .{ .node = node, .item = 0 });
            return index;
        }

        fn tarjanNodeVisit(self: *Self, node: u32) Allocator.Error!void {
            self.node_info.getPtr(node).?.tarjan = try self.tarjanPush(node);
        }

        fn nodeDesc(self: *const Self, node: u32) Desc {
            return self.store.descs.items[self.info(node).desc];
        }

        fn hasSelfEdge(self: *const Self, node: u32) bool {
            for (self.store.itemsOf(self.nodeDesc(node))) |item| {
                if (item.child == node) return true;
            }
            return false;
        }

        fn classifyComponent(self: *Self) Allocator.Error!void {
            const members = self.scc_members.items;
            if (members.len == 1 and !self.hasSelfEdge(members[0])) {
                self.setClass(members[0], try self.internStep(members[0]));
                return;
            }
            try self.classifyCycle();
        }

        /// Where a refinement state's child leads: another state, or a class
        /// that stays fixed.
        const StateChild = union(enum) { state: u32, fixed: u32 };

        fn stateChild(self: *const Self, node: u32) StateChild {
            if (self.local_of.get(node)) |state| return .{ .state = state };
            const class = self.nodeClass(node);
            if (self.state_of_class.get(class)) |state| return .{ .state = state };
            return .{ .fixed = class };
        }

        /// A refinement state's description: a member's own, or an existing
        /// class's representative's.
        fn stateDesc(self: *const Self, state: u32) Desc {
            return self.nodeDesc(self.state_nodes.items[state]);
        }

        /// Classify a cyclic component. A member denotes an infinite tree that
        /// contains itself, so an existing class it equals lies on a cycle of
        /// classes too: either one this component reaches through classes on
        /// cycles, which refinement considers alongside the members, or an
        /// independent copy of the same structure, which the canonical walk
        /// of the members' new classes finds. Members equal to an existing
        /// class join it; the rest form new classes named by their canonical
        /// walk. Every new class's one-step structure is registered too, so a
        /// node written as an unrolled copy of the cycle joins its class.
        fn classifyCycle(self: *Self) Allocator.Error!void {
            const members = self.scc_members.items;
            self.local_of.clearRetainingCapacity();
            self.state_of_class.clearRetainingCapacity();
            self.state_nodes.clearRetainingCapacity();
            self.state_class.clearRetainingCapacity();
            for (members, 0..) |member, index| {
                try self.local_of.put(member, @intCast(index));
                try self.state_nodes.append(self.gpa, member);
                try self.state_class.append(self.gpa, nil);
            }
            // Existing on-cycle classes reachable through on-cycle classes.
            var scan: usize = 0;
            while (scan < self.state_nodes.items.len) : (scan += 1) {
                for (self.store.itemsOf(self.stateDesc(@intCast(scan)))) |item| {
                    if (!item.isChild() or self.local_of.contains(item.child)) continue;
                    const class = self.nodeClass(item.child);
                    if (!self.classes.items[class].on_cycle or self.state_of_class.contains(class)) continue;
                    try self.state_of_class.put(class, @intCast(self.state_nodes.items.len));
                    try self.state_nodes.append(self.gpa, self.classes.items[class].rep);
                    try self.state_class.append(self.gpa, class);
                }
            }

            const state_count = self.state_nodes.items.len;
            try self.local_class.resize(self.gpa, state_count);
            try self.next_local_class.resize(self.gpa, state_count);
            var block_count = try self.partition(true);
            while (true) {
                const refined = try self.partition(false);
                if (refined == block_count) break;
                block_count = refined;
            }

            // A block holding an existing class is that class; the others
            // are new.
            try self.local_globals.resize(self.gpa, block_count);
            const globals = self.local_globals.items;
            @memset(globals, nil);
            for (self.state_class.items, 0..) |class, state| {
                if (class == nil) continue;
                const block = self.local_class.items[state];
                std.debug.assert(globals[block] == nil or globals[block] == class);
                globals[block] = class;
            }
            // Walks name new blocks structurally whatever order they are
            // named in, referring only to existing classes by id.
            try self.block_existing.resize(self.gpa, block_count);
            @memcpy(self.block_existing.items, globals);
            for (members, 0..) |member, state| {
                const block = self.local_class.items[state];
                if (globals[block] != nil) continue;
                self.signature.clearRetainingCapacity();
                try self.appendCycleWalk(@intCast(state));
                globals[block] = try self.internWalk(self.signature.items, member);
                self.classes.items[globals[block]].on_cycle = true;
            }
            for (members, 0..) |member, state| {
                self.setClass(member, globals[self.local_class.items[state]]);
            }
            // Register each class's one-step structure for unrolled copies.
            for (members) |member| try self.registerStep(self.nodeClass(member));
        }

        /// One refinement round over the states. The first round groups
        /// states by their own bytes and fixed children; later rounds by their
        /// current block and their state children's blocks. Returns the number
        /// of blocks.
        fn partition(self: *Self, first: bool) Allocator.Error!u32 {
            self.local_groups.clearRetainingCapacity();
            _ = self.local_group_arena.reset(.retain_capacity);
            var count: u32 = 0;
            for (0..self.state_nodes.items.len) |state| {
                self.signature.clearRetainingCapacity();
                if (!first) try appendVarint(&self.signature, self.gpa, self.local_class.items[state]);
                for (self.store.itemsOf(self.stateDesc(@intCast(state)))) |item| {
                    if (item.isChild()) {
                        switch (self.stateChild(item.child)) {
                            .state => |child_state| {
                                try self.signature.append(self.gpa, 3);
                                if (!first) try appendVarint(&self.signature, self.gpa, self.local_class.items[child_state]);
                            },
                            .fixed => |class| {
                                try self.signature.append(self.gpa, 2);
                                try appendVarint(&self.signature, self.gpa, class);
                            },
                        }
                    } else if (first) {
                        try self.signature.append(self.gpa, 1);
                        try appendVarint(&self.signature, self.gpa, item.len);
                        try self.signature.appendSlice(self.gpa, self.store.bytesOf(item));
                    }
                }
                const group = try self.local_groups.getOrPut(self.gpa, self.signature.items);
                if (!group.found_existing) {
                    group.key_ptr.* = try self.local_group_arena.allocator().dupe(u8, self.signature.items);
                    group.value_ptr.* = count;
                    count += 1;
                }
                self.next_local_class.items[state] = group.value_ptr.*;
            }
            @memcpy(self.local_class.items, self.next_local_class.items);
            return count;
        }

        /// The canonical walk of the new blocks from `start`: first visits in
        /// child order, repeated blocks by their visit index, and blocks that
        /// are existing classes or fixed children by their class.
        fn appendCycleWalk(self: *Self, start: u32) Allocator.Error!void {
            self.dfs_index.clearRetainingCapacity();
            self.dfs_frames.clearRetainingCapacity();
            try self.signature.append(self.gpa, 'Y');
            try self.dfsEnter(start);
            while (self.dfs_frames.items.len > 0) {
                const frame = &self.dfs_frames.items[self.dfs_frames.items.len - 1];
                const items = self.store.itemsOf(self.stateDesc(frame.node));
                if (frame.item >= items.len) {
                    _ = self.dfs_frames.pop();
                    continue;
                }
                const item = items[frame.item];
                frame.item += 1;
                if (!item.isChild()) {
                    try self.signature.append(self.gpa, 1);
                    try appendVarint(&self.signature, self.gpa, item.len);
                    try self.signature.appendSlice(self.gpa, self.store.bytesOf(item));
                    continue;
                }
                const class: u32 = switch (self.stateChild(item.child)) {
                    .fixed => |class| class,
                    .state => |child_state| blk: {
                        const block = self.local_class.items[child_state];
                        if (self.block_existing.items[block] != nil) break :blk self.block_existing.items[block];
                        if (self.dfs_index.get(block)) |visit| {
                            try self.signature.append(self.gpa, 4);
                            try appendVarint(&self.signature, self.gpa, visit);
                        } else {
                            try self.signature.append(self.gpa, 5);
                            try self.dfsEnter(child_state);
                        }
                        continue;
                    },
                };
                try self.signature.append(self.gpa, 2);
                try appendVarint(&self.signature, self.gpa, class);
            }
        }

        fn dfsEnter(self: *Self, state: u32) Allocator.Error!void {
            try self.dfs_index.put(self.local_class.items[state], @intCast(self.dfs_index.count()));
            try self.dfs_frames.append(self.gpa, .{ .node = state, .item = 0 });
        }

        // Keys //

        /// The step words of `class`'s representative's items, in item
        /// order: a child's class, or a byte run tagged with `run_bit`.
        fn childClasses(self: *const Self, class: u32) []const u32 {
            const record = self.classes.items[class];
            return self.class_children.items[record.children..][0..self.store.descs.items[record.desc].items_len];
        }

        /// An identity class's step words, once its constraints' children
        /// have classes.
        fn fillChildClasses(self: *Self, class: u32) Allocator.Error!void {
            if (self.classes.items[class].children != nil) return;
            const start: u32 = @intCast(self.class_children.items.len);
            try self.appendStepWords(&self.class_children, self.classDesc(class));
            self.classes.items[class].children = start;
        }

        fn tarjanClassVisit(self: *Self, class: u32) Allocator.Error!void {
            try self.fillChildClasses(class);
            self.classes.items[class].tarjan = try self.tarjanPush(class);
        }

        /// Key every class reachable from `start` that has none, one strongly
        /// connected component of the class graph at a time, children first.
        /// Here identity edges count: an identity's constraints are part of
        /// its encoding.
        ///
        /// Tarjan indices live in `Class.tarjan`. Only a class without a
        /// summary is ever looked up by them, and every class this visits has
        /// one by the time it returns, so indices never need clearing.
        fn summarizeClass(self: *Self, start: u32) Allocator.Error!void {
            if (self.classes.items[start].summary != nil) return;
            self.clearTarjan();
            try self.tarjanClassVisit(start);
            while (self.tarjan_frames.items.len > 0) {
                const frame = &self.tarjan_frames.items[self.tarjan_frames.items.len - 1];
                const children = self.childClasses(frame.node);
                var descended = false;
                while (frame.item < children.len) {
                    const child = children[frame.item];
                    frame.item += 1;
                    if (child & run_bit != 0) continue;
                    const child_class = self.classes.items[child];
                    if (child_class.summary != nil) continue;
                    if (child_class.tarjan != nil) {
                        if (self.tarjan_on_stack.items[child_class.tarjan]) {
                            const own = self.classes.items[frame.node].tarjan;
                            self.tarjan_low.items[own] = @min(self.tarjan_low.items[own], child_class.tarjan);
                        }
                        continue;
                    }
                    try self.tarjanClassVisit(child);
                    descended = true;
                    break;
                }
                if (descended) continue;

                const done = self.tarjan_frames.pop().?;
                const own = self.classes.items[done.node].tarjan;
                if (self.tarjan_frames.items.len > 0) {
                    const parent = self.classes.items[self.tarjan_frames.items[self.tarjan_frames.items.len - 1].node].tarjan;
                    self.tarjan_low.items[parent] = @min(self.tarjan_low.items[parent], self.tarjan_low.items[own]);
                }
                if (self.tarjan_low.items[own] != own) continue;
                self.scc_members.clearRetainingCapacity();
                while (true) {
                    const member = self.tarjan_stack.pop().?;
                    self.tarjan_on_stack.items[self.classes.items[member].tarjan] = false;
                    try self.scc_members.append(self.gpa, member);
                    if (member == done.node) break;
                }
                const scc: u32 = @intCast(self.scc_cyclic.items.len);
                const cyclic = self.scc_members.items.len > 1 or self.classHasSelfEdge(self.scc_members.items[0]);
                try self.scc_cyclic.append(self.gpa, cyclic);
                for (self.scc_members.items) |member| self.classes.items[member].key_scc = scc;
                for (self.scc_members.items) |member| {
                    const summary = try self.walk(member);
                    self.classes.items[member].summary = @intCast(self.summaries.items.len);
                    try self.summaries.append(self.gpa, summary);
                }
            }
        }

        fn classHasSelfEdge(self: *const Self, class: u32) bool {
            for (self.childClasses(class)) |child| {
                if (child == class) return true;
            }
            return false;
        }

        const WalkState = struct {
            defined: u32 = 0,
            order: PersistentMap.Handle = .{},
            contains_identity: bool = false,
        };

        /// The canonical encoding of `root` and its key. Classes of `root`'s
        /// component are written in place; every other child is a reference
        /// to its key.
        fn walk(self: *Self, root: u32) Allocator.Error!Summary {
            self.buf.clearRetainingCapacity();
            self.walk_frames.clearRetainingCapacity();
            self.active.clearRetainingCapacity();
            var state = WalkState{};
            const scc = self.classes.items[root].key_scc;
            const desc = self.classDesc(root);
            switch (desc.kind) {
                .leaf => {
                    try self.appendLeaf(desc);
                    state.contains_identity = desc.counts_identity;
                },
                .identity => try self.defineIdentity(&state, root),
                .content => {
                    try self.active.append(self.gpa, root);
                    try self.walk_frames.append(self.gpa, .{ .class = root, .item = 0 });
                },
            }
            while (self.walk_frames.items.len > 0) {
                const frame = &self.walk_frames.items[self.walk_frames.items.len - 1];
                const frame_desc = self.classDesc(frame.class);
                const items = self.store.itemsOf(frame_desc);
                if (frame.item >= items.len) {
                    if (frame_desc.kind == .content) _ = self.active.pop();
                    _ = self.walk_frames.pop();
                    continue;
                }
                const item = items[frame.item];
                frame.item += 1;
                self.work += 1;
                if (!item.isChild()) {
                    try self.buf.appendSlice(self.gpa, self.store.bytesOf(item));
                    continue;
                }
                try self.writeChild(&state, scc, self.childClasses(frame.class)[frame.item - 1]);
            }
            return .{
                .key = TypeDigestHasher.hash(self.buf.items),
                .nvars = state.defined,
                .order = state.order,
                .contains_identity = state.contains_identity,
                .composable = !state.contains_identity and !self.scc_cyclic.items[scc],
            };
        }

        fn appendLeaf(self: *Self, desc: Desc) Allocator.Error!void {
            for (self.store.itemsOf(desc)) |item| try self.buf.appendSlice(self.gpa, self.store.bytesOf(item));
        }

        fn defineIdentity(self: *Self, state: *WalkState, class: u32) Allocator.Error!void {
            state.order = try self.maps.put(self.gpa, state.order, class, state.defined);
            state.defined += 1;
            state.contains_identity = true;
            try self.walk_frames.append(self.gpa, .{ .class = class, .item = 0 });
        }

        fn writeChild(self: *Self, state: *WalkState, scc: u32, class: u32) Allocator.Error!void {
            const desc = self.classDesc(class);
            switch (desc.kind) {
                .leaf => {
                    try self.appendLeaf(desc);
                    if (desc.counts_identity) state.contains_identity = true;
                },
                .identity => {
                    if (self.maps.get(state.order, class)) |index| {
                        try self.buf.append(self.gpa, self.tags.identity_ref);
                        try appendVarint(&self.buf, self.gpa, state.defined - index);
                        state.contains_identity = true;
                    } else if (self.classes.items[class].key_scc == scc or desc.constraint_count == 0) {
                        try self.defineIdentity(state, class);
                    } else {
                        try self.writeReference(state, class);
                    }
                },
                .content => {
                    if (self.classes.items[class].key_scc != scc) return try self.writeReference(state, class);
                    // Only classes of the walked component are ever active.
                    for (self.active.items, 0..) |active, depth| {
                        if (active != class) continue;
                        try self.buf.append(self.gpa, self.tags.cycle);
                        try appendVarint(&self.buf, self.gpa, @intCast(self.active.items.len - depth));
                        return;
                    }
                    try self.active.append(self.gpa, class);
                    try self.walk_frames.append(self.gpa, .{ .class = class, .item = 0 });
                },
            }
        }

        /// Refer to a class outside the component by its key. Its variables
        /// the walk already defined are listed as (child index, distance)
        /// pairs; the rest become defined here, in the child's order.
        fn writeReference(self: *Self, state: *WalkState, class: u32) Allocator.Error!void {
            const summary = self.summaries.items[self.classes.items[class].summary];
            if (summary.contains_identity) state.contains_identity = true;
            if (summary.nvars == 0) {
                try self.buf.append(self.gpa, self.tags.child_key);
                try self.buf.appendSlice(self.gpa, &summary.key);
                return;
            }

            // Shared variables: iterate the smaller map, look up in the larger.
            self.pairs.clearRetainingCapacity();
            self.entries.clearRetainingCapacity();
            const child_count = self.maps.count(summary.order);
            const own_count = self.maps.count(state.order);
            if (child_count <= own_count) {
                try self.maps.appendEntries(self.gpa, summary.order, &self.entries);
                for (self.entries.items) |entry| {
                    if (self.maps.get(state.order, entry.key)) |index| {
                        try self.pairs.append(self.gpa, .{ .key = entry.value, .value = state.defined - index });
                    }
                }
            } else {
                try self.maps.appendEntries(self.gpa, state.order, &self.entries);
                for (self.entries.items) |entry| {
                    if (self.maps.get(summary.order, entry.key)) |local| {
                        try self.pairs.append(self.gpa, .{ .key = local, .value = state.defined - entry.value });
                    }
                }
            }
            self.work += self.entries.items.len;
            std.mem.sort(PersistentMap.Entry, self.pairs.items, {}, struct {
                fn lessThan(_: void, a: PersistentMap.Entry, b: PersistentMap.Entry) bool {
                    return a.key < b.key;
                }
            }.lessThan);

            try self.buf.append(self.gpa, self.tags.child_key_mapped);
            try self.buf.appendSlice(self.gpa, &summary.key);
            try appendVarint(&self.buf, self.gpa, summary.nvars);
            try appendVarint(&self.buf, self.gpa, @intCast(self.pairs.items.len));
            for (self.pairs.items) |pair| {
                try appendVarint(&self.buf, self.gpa, pair.key);
                try appendVarint(&self.buf, self.gpa, pair.value);
            }

            // The child's variables take the next `nvars` indices; a variable
            // already defined keeps its earlier index. Merge the smaller map
            // into the larger.
            const placed = summary.order.shifted(state.defined);
            self.entries.clearRetainingCapacity();
            if (own_count >= child_count) {
                try self.maps.appendEntries(self.gpa, placed, &self.entries);
                for (self.entries.items) |entry| {
                    state.order = try self.maps.putIfAbsent(self.gpa, state.order, entry.key, entry.value);
                }
            } else {
                try self.maps.appendEntries(self.gpa, state.order, &self.entries);
                var merged = placed;
                for (self.entries.items) |entry| {
                    merged = try self.maps.put(self.gpa, merged, entry.key, entry.value);
                }
                state.order = merged;
            }
            self.work += self.entries.items.len;
            state.defined += summary.nvars;
        }

        /// The number of classes known so far.
        pub fn classCount(self: *const Self) u32 {
            return @intCast(self.classes.items.len);
        }

        /// The class of an already summarized node.
        pub fn classOf(self: *const Self, node: u32) ?u32 {
            const entry = self.node_info.getPtrConst(node) orelse return null;
            return if (entry.class == nil) null else entry.class;
        }

        /// Re-derive the encoding of a class whose component is keyed. The
        /// returned bytes are valid until the next walk.
        pub fn encodingOf(self: *Self, class: u32) Allocator.Error![]const u8 {
            std.debug.assert(self.classes.items[class].summary != nil);
            _ = try self.walk(class);
            return self.buf.items;
        }
    };
}

fn sliceEql(comptime T: type, a: []const T, b: []const T) bool {
    if (a.len != b.len) return false;
    for (a, b) |x, y| {
        if (x != y) return false;
    }
    return true;
}

/// Unsigned LEB128, the key encoding's integers.
pub fn appendVarint(buf: *std.ArrayListUnmanaged(u8), gpa: Allocator, value: u32) Allocator.Error!void {
    var rest = value;
    while (rest >= 0x80) : (rest >>= 7) try buf.append(gpa, @as(u8, @truncate(rest)) | 0x80);
    try buf.append(gpa, @truncate(rest));
}
