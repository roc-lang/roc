//! Content identities for lowered procedures.
//!
//! A procedure's symbol has to name the same code in every program, so it
//! cannot be built from any per-program numbering. The identity rendered here
//! depends only on what determines the procedure's compiled bytes: the lifted
//! function's checked source identity (`Lifted.Program.fnSourceDigest`), the
//! Lambda Mono ABI choices, and the solved argument, result, and capture types
//! with their lambda sets.
//! The outer callable set describes where a function value flows, not the
//! code of the selected procedure; its source and captures already name that
//! selection. Nested callable sets still determine dispatch and representation.
//!
//! A lambda-set member is rendered as the member function's source identity
//! followed by its captures and its own solved function type, because two
//! specializations of one lambda that differ only in the lambda sets inside
//! their captures or arguments are different procedures. That descent can lead
//! back to a type already being rendered, for example a stream whose `next`
//! closure returns a stream holding that same closure, and recursive nominal
//! types are cycles in the graph by nature.
//!
//! Two programs draw the same recursive type as different graphs: one may
//! hold a single node per knot while another rolls the knot out a level
//! before tying it back, depending on which parts of the type each program's
//! lambda solving touched. The identity must not see that difference, so a
//! type digests as its bisimulation class rather than as its graph. Every
//! request runs the same reduction Monotype's type digests use: the reachable
//! subgraph is decomposed into strongly connected components, which resolve
//! in reverse topological order; an acyclic node hashes its encoding with its
//! children's finished digests, and a cyclic component is refined to its
//! coarsest bisimulation partition, rendered once as a group in label order
//! with in-group references written as ranks, and every member digests as its
//! rank against the group. Each member's one-step unfolding (its encoding
//! with every child as a finished digest) is remembered, so an acyclic node
//! that is a rolled-out copy of a group position folds to that position's
//! digest instead of getting one of its own.
//!
//! Every digest is a function of the infinite type alone, so the `Memo` shared
//! across procedures remembers every type it has rendered: a solved type graph
//! shares subtypes heavily (one record type reached from hundreds of
//! lambda-set members), and rendering each occurrence anew would grow with
//! the number of paths through the graph instead of its size.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const collections = @import("collections");

const Common = @import("common.zig");
const MonoType = @import("monotype/type.zig");
const Lifted = @import("monotype_lifted/ast.zig");
const SolvedType = @import("lambda_solved/type.zig");
const names = check.CheckedNames;
const TypeDigestHasher = base.TypeDigestHasher;
const Allocator = std.mem.Allocator;

/// SHA-256 content identity of one procedure.
pub const Identity = [TypeDigestHasher.digest_length]u8;

const domain = "roc.proc.identity.v3";

/// Everything one solved program has rendered so far, shared by every
/// identity rendered over it.
pub const Memo = struct {
    /// Digest of every type rendered so far, keyed by its root variable.
    digests: collections.DenseMap(SolvedType.TypeVarId, Identity),
    /// The one-step unfolding of every cyclic group position rendered so far,
    /// mapped to that position's digest.
    unfoldings: std.AutoHashMap(Identity, Identity),

    pub fn init(allocator: Allocator) Memo {
        return .{
            .digests = collections.DenseMap(SolvedType.TypeVarId, Identity).init(allocator),
            .unfoldings = std.AutoHashMap(Identity, Identity).init(allocator),
        };
    }

    pub fn deinit(self: *Memo) void {
        self.digests.deinit();
        self.unfoldings.deinit();
    }
};

/// Renders specialization identities over one solved program.
pub const Renderer = struct {
    allocator: Allocator,
    types: SolvedType.Store.View,
    names: *const names.NameStore,
    /// Solved function type of every lifted function, indexed by `Lifted.FnId`.
    fn_tys: []const SolvedType.TypeVarId,
    /// `Lifted.Program.fnSourceDigest` of every lifted function that has a
    /// checked source, indexed by `Lifted.FnId`.
    source_digests: []const ?Identity,
    fn_by_symbol: *const std.AutoHashMap(Common.Symbol, Lifted.FnId),
    memo: *Memo,

    /// Identity of the specialization of `source` at `solved_fn_ty` with the
    /// given captures and Lambda Mono ABI choices. Two specializations with
    /// the same identity are one procedure: everything that determines the
    /// procedure's code is rendered here, so Direct LIR lowers one proc per
    /// identity.
    pub fn specIdentity(
        self: *const Renderer,
        source: Lifted.FnId,
        solved_fn_ty: SolvedType.TypeVarId,
        captures: []const SolvedType.Capture,
        capture_abi: []const u8,
        return_reuse: []const u8,
    ) Allocator.Error!Identity {
        var hasher = TypeDigestHasher.init();
        writeBytes(&hasher, domain);
        hasher.update(&self.sourceDigest(source));
        writeBytes(&hasher, capture_abi);
        writeBytes(&hasher, return_reuse);
        const signature = switch (self.types.get(self.renderedRoot(solved_fn_ty))) {
            .func => |func| func,
            else => Common.invariant("procedure identity requires a solved function signature"),
        };
        writeBytes(&hasher, "args");
        const args = self.types.span(signature.args);
        writeU32(&hasher, @intCast(args.len));
        for (args) |arg| hasher.update(&try self.typeDigest(arg));
        writeBytes(&hasher, "ret");
        hasher.update(&try self.typeDigest(signature.ret));
        writeBytes(&hasher, "captures");
        writeU32(&hasher, @intCast(captures.len));
        for (captures) |capture| hasher.update(&try self.typeDigest(capture.ty));
        return hasher.finalResult();
    }

    fn sourceDigest(self: *const Renderer, fn_id: Lifted.FnId) Identity {
        return self.source_digests[@intFromEnum(fn_id)] orelse
            Common.invariant("lifted function without a checked source template reached procedure identity rendering");
    }

    /// The variable a type renders as: its root, through any transparent
    /// aliases, which have no runtime identity of their own.
    fn renderedRoot(self: *const Renderer, ty: SolvedType.TypeVarId) SolvedType.TypeVarId {
        var root = self.types.root(ty);
        var content = self.types.get(root);
        while (transparentAliasBacking(content)) |backing| {
            root = self.types.root(backing);
            content = self.types.get(root);
        }
        return root;
    }

    /// Digest of the infinite type `ty` denotes.
    fn typeDigest(self: *const Renderer, ty: SolvedType.TypeVarId) Allocator.Error!Identity {
        const root = self.renderedRoot(ty);
        if (self.memo.digests.get(root)) |digest| return digest;
        var engine = Engine.init(self);
        defer engine.deinit();
        return try engine.run(root);
    }
};

/// A child position in a node's encoding: another node of the current
/// reduction, or a type whose digest is already known.
const Child = union(enum) {
    node: u32,
    digest: Identity,
};

/// One piece of a node's encoding, in order: literal bytes or a child.
const Item = union(enum) {
    bytes: struct { start: u32, len: u32 },
    child: Child,
};

const Node = struct {
    ty: SolvedType.TypeVarId,
    item_start: u32 = 0,
    item_len: u32 = 0,
    child_start: u32 = 0,
    child_len: u32 = 0,
    index: u32 = unvisited,
    lowlink: u32 = unvisited,
    on_stack: bool = false,
    digest: ?Identity = null,
};

const unvisited = std.math.maxInt(u32);
const no_position = std.math.maxInt(u32);

/// One reduction: the subgraph reachable from a requested type through
/// types the memo does not hold yet.
const Engine = struct {
    renderer: *const Renderer,
    gpa: Allocator,
    nodes: std.ArrayList(Node) = .empty,
    node_of_ty: collections.DenseMap(SolvedType.TypeVarId, u32),
    items: std.ArrayList(Item) = .empty,
    children: std.ArrayList(Child) = .empty,
    bytes: std.ArrayList(u8) = .empty,
    /// Nodes discovered but not yet encoded.
    pending: std.ArrayList(u32) = .empty,

    fn init(renderer: *const Renderer) Engine {
        return .{
            .renderer = renderer,
            .gpa = renderer.allocator,
            .node_of_ty = collections.DenseMap(SolvedType.TypeVarId, u32).init(renderer.allocator),
        };
    }

    fn deinit(self: *Engine) void {
        self.nodes.deinit(self.gpa);
        self.node_of_ty.deinit();
        self.items.deinit(self.gpa);
        self.children.deinit(self.gpa);
        self.bytes.deinit(self.gpa);
        self.pending.deinit(self.gpa);
    }

    fn run(self: *Engine, root: SolvedType.TypeVarId) Allocator.Error!Identity {
        const root_node = try self.discover(root);
        while (self.pending.pop()) |node| try self.encode(node);
        try self.resolveAll();
        return self.nodes.items[root_node].digest orelse
            Common.invariant("procedure identity reduction left its root type without a digest");
    }

    /// The node for `ty`, created and queued for encoding if new.
    fn discover(self: *Engine, ty: SolvedType.TypeVarId) Allocator.Error!u32 {
        const gop = try self.node_of_ty.getOrPut(ty);
        if (gop.found_existing) return gop.value_ptr.*;
        const index: u32 = @intCast(self.nodes.items.len);
        gop.value_ptr.* = index;
        try self.nodes.append(self.gpa, .{ .ty = ty });
        try self.pending.append(self.gpa, index);
        return index;
    }

    /// The child position for `ty`: a finished digest when the memo holds
    /// it, otherwise a node of this reduction.
    fn childFor(self: *Engine, ty: SolvedType.TypeVarId) Allocator.Error!Child {
        const root = self.renderer.renderedRoot(ty);
        if (self.renderer.memo.digests.get(root)) |digest| return .{ .digest = digest };
        return .{ .node = try self.discover(root) };
    }

    fn putBytes(self: *Engine, bytes: []const u8) Allocator.Error!void {
        const start: u32 = @intCast(self.bytes.items.len);
        try self.bytes.appendSlice(self.gpa, bytes);
        try self.items.append(self.gpa, .{ .bytes = .{ .start = start, .len = @intCast(bytes.len) } });
    }

    fn putU32(self: *Engine, value: u32) Allocator.Error!void {
        var buffer: [4]u8 = undefined;
        encodeU32(&buffer, value);
        try self.putBytes(&buffer);
    }

    fn putChild(self: *Engine, ty: SolvedType.TypeVarId) Allocator.Error!void {
        const child = try self.childFor(ty);
        try self.items.append(self.gpa, .{ .child = child });
        try self.children.append(self.gpa, child);
    }

    fn putSpan(self: *Engine, span: SolvedType.Span) Allocator.Error!void {
        const tys = self.renderer.types.span(span);
        try self.putU32(@intCast(tys.len));
        for (tys) |ty| try self.putChild(ty);
    }

    fn putMembers(self: *Engine, members: SolvedType.Span) Allocator.Error!void {
        const renderer = self.renderer;
        try self.putBytes("lambda_set");
        const member_slice = renderer.types.memberSpan(members);
        try self.putU32(@intCast(member_slice.len));
        for (member_slice) |member| {
            const fn_id = renderer.fn_by_symbol.get(member.lambda) orelse
                Common.invariant("lambda-set member referenced a lifted function with no identity");
            try self.putBytes(&renderer.sourceDigest(fn_id));
            const captures = renderer.types.captureSpan(member.captures);
            try self.putU32(@intCast(captures.len));
            for (captures) |capture| try self.putChild(capture.ty);
            try self.putChild(renderer.fn_tys[@intFromEnum(fn_id)]);
        }
    }

    /// Record the encoding of one node: its content's non-reference values
    /// and its ordered children.
    fn encode(self: *Engine, node: u32) Allocator.Error!void {
        const renderer = self.renderer;
        const item_start: u32 = @intCast(self.items.items.len);
        const child_start: u32 = @intCast(self.children.items.len);
        const content = renderer.types.get(self.nodes.items[node].ty);
        switch (content) {
            .mono, .link, .unbound, .forall => Common.invariant("unresolved Lambda Solved type reached procedure identity rendering"),
            .primitive => |primitive| {
                try self.putBytes("primitive");
                try self.putBytes(@tagName(primitive));
            },
            .zst => try self.putBytes("zst"),
            .erased => |erased| {
                try self.putBytes("erased");
                try self.putBytes(&erased.source_fn_ty.bytes);
                try self.putMembers(erased.members);
            },
            .func => |func| {
                try self.putBytes("func");
                try self.putSpan(func.args);
                try self.putChild(func.callable);
                try self.putChild(func.ret);
            },
            .list => |elem| {
                try self.putBytes("list");
                try self.putChild(elem);
            },
            .box => |elem| {
                try self.putBytes("box");
                try self.putChild(elem);
            },
            .tuple => |elems| {
                try self.putBytes("tuple");
                try self.putSpan(elems);
            },
            .record => |fields| {
                try self.putBytes("record");
                const field_slice = renderer.types.fieldSpan(fields);
                try self.putU32(@intCast(field_slice.len));
                for (field_slice) |field| {
                    try self.putBytes(renderer.names.recordFieldLabelText(field.name));
                    var default_hasher = TypeDigestHasher.init();
                    MonoType.writeFieldDefaultDigest(renderer.names, &default_hasher, field.default);
                    try self.putBytes(&default_hasher.finalResult());
                    if (field.value_ty) |value_ty| {
                        try self.putBytes("field-optional-value");
                        try self.putChild(value_ty);
                    } else {
                        try self.putBytes("field-inline-value");
                    }
                    try self.putChild(field.ty);
                }
            },
            .tag_union => |tags| {
                try self.putBytes("tag_union");
                const tag_slice = renderer.types.tagSpan(tags);
                try self.putU32(@intCast(tag_slice.len));
                for (tag_slice) |tag| {
                    try self.putBytes(renderer.names.tagLabelText(tag.name));
                    try self.putSpan(tag.payloads);
                }
            },
            .named => |named| {
                try self.putBytes("named");
                try self.putBytes(&named.named_type.module.bytes);
                try self.putBytes(renderer.names.moduleIdentityBytes(named.def.module));
                if (named.def.source_decl) |decl| {
                    try self.putBytes("source-decl");
                    try self.putU32(decl);
                } else {
                    try self.putBytes("no-source-decl");
                }
                try self.putBytes(renderer.names.typeNameText(named.def.type_name));
                try self.putBytes(@tagName(named.kind));
                if (named.builtin_owner) |owner| {
                    try self.putBytes("builtin");
                    try self.putBytes(@tagName(owner));
                } else {
                    try self.putBytes("not-builtin");
                }
                try self.putSpan(named.args);
                // A nominal type's representation is its backing, and the
                // backing can hold lambda sets the arguments never mention,
                // so the same name at the same arguments can still be two
                // different procedures' worth of code.
                if (named.backing) |backing| {
                    try self.putBytes("backing");
                    try self.putChild(backing.ty);
                } else {
                    try self.putBytes("no-backing");
                }
            },
            .lambda_set => |members| try self.putMembers(members),
        }
        const entry = &self.nodes.items[node];
        entry.item_start = item_start;
        entry.item_len = @intCast(self.items.items.len - item_start);
        entry.child_start = child_start;
        entry.child_len = @intCast(self.children.items.len - child_start);
    }

    fn itemsOf(self: *const Engine, node: u32) []const Item {
        const entry = self.nodes.items[node];
        return self.items.items[entry.item_start .. entry.item_start + entry.item_len];
    }

    fn childrenOf(self: *const Engine, node: u32) []const Child {
        const entry = self.nodes.items[node];
        return self.children.items[entry.child_start .. entry.child_start + entry.child_len];
    }

    fn bytesOf(self: *const Engine, start: u32, len: u32) []const u8 {
        return self.bytes.items[start .. start + len];
    }

    /// Iterative Tarjan discovery over every node. Components pop in
    /// reverse topological order, so every component a node reaches is
    /// resolved before the node's own.
    fn resolveAll(self: *Engine) Allocator.Error!void {
        const Frame = struct { node: u32, next_child: u32 };
        var frames: std.ArrayList(Frame) = .empty;
        defer frames.deinit(self.gpa);
        var scc_stack: std.ArrayList(u32) = .empty;
        defer scc_stack.deinit(self.gpa);
        var members: std.ArrayList(u32) = .empty;
        defer members.deinit(self.gpa);
        var next_index: u32 = 0;

        for (0..self.nodes.items.len) |start_index| {
            const start: u32 = @intCast(start_index);
            if (self.nodes.items[start].index != unvisited) continue;
            try self.visit(start, &next_index, &scc_stack);
            try frames.append(self.gpa, .{ .node = start, .next_child = 0 });
            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const node = frame.node;
                const children = self.childrenOf(node);
                if (frame.next_child < children.len) {
                    const child = children[frame.next_child];
                    frame.next_child += 1;
                    const target = switch (child) {
                        .digest => continue,
                        .node => |target| target,
                    };
                    const target_entry = &self.nodes.items[target];
                    if (target_entry.index == unvisited) {
                        try self.visit(target, &next_index, &scc_stack);
                        try frames.append(self.gpa, .{ .node = target, .next_child = 0 });
                    } else if (target_entry.on_stack) {
                        const entry = &self.nodes.items[node];
                        entry.lowlink = @min(entry.lowlink, target_entry.index);
                    }
                    continue;
                }
                _ = frames.pop();
                const entry = self.nodes.items[node];
                if (frames.items.len != 0) {
                    const parent = &self.nodes.items[frames.items[frames.items.len - 1].node];
                    parent.lowlink = @min(parent.lowlink, entry.lowlink);
                }
                if (entry.lowlink == entry.index) {
                    members.clearRetainingCapacity();
                    while (true) {
                        const member = scc_stack.pop() orelse
                            Common.invariant("procedure identity reduction popped past its component root");
                        self.nodes.items[member].on_stack = false;
                        try members.append(self.gpa, member);
                        if (member == node) break;
                    }
                    try self.resolveComponent(members.items);
                }
            }
        }
    }

    fn visit(self: *Engine, node: u32, next_index: *u32, scc_stack: *std.ArrayList(u32)) Allocator.Error!void {
        const entry = &self.nodes.items[node];
        entry.index = next_index.*;
        entry.lowlink = next_index.*;
        entry.on_stack = true;
        next_index.* += 1;
        try scc_stack.append(self.gpa, node);
    }

    fn resolveComponent(self: *Engine, members: []const u32) Allocator.Error!void {
        if (members.len == 1 and !self.hasSelfEdge(members[0])) {
            const node = members[0];
            const key = self.unfoldingKey(node);
            const digest = self.renderer.memo.unfoldings.get(key) orelse key;
            try self.finalize(node, digest);
            return;
        }
        try self.resolveCyclicComponent(members);
    }

    fn hasSelfEdge(self: *const Engine, node: u32) bool {
        for (self.childrenOf(node)) |child| switch (child) {
            .node => |target| if (target == node) return true,
            .digest => {},
        };
        return false;
    }

    fn childDigest(self: *const Engine, child: Child) Identity {
        return switch (child) {
            .digest => |digest| digest,
            .node => |target| self.nodes.items[target].digest orelse
                Common.invariant("procedure identity reduction read a child digest before resolving it"),
        };
    }

    /// The node's encoding with every child as its finished digest.
    fn unfoldingKey(self: *const Engine, node: u32) Identity {
        var hasher = TypeDigestHasher.init();
        writeBytes(&hasher, "node");
        for (self.itemsOf(node)) |item| switch (item) {
            .bytes => |range| writeBytes(&hasher, self.bytesOf(range.start, range.len)),
            .child => |child| {
                writeBytes(&hasher, "child");
                hasher.update(&self.childDigest(child));
            },
        };
        return hasher.finalResult();
    }

    fn finalize(self: *Engine, node: u32, digest: Identity) Allocator.Error!void {
        const entry = &self.nodes.items[node];
        if (entry.digest != null) Common.invariant("procedure identity reduction resolved a type twice");
        entry.digest = digest;
        try self.renderer.memo.digests.put(entry.ty, digest);
    }

    /// Reduce one cyclic component by bisimulation refinement over content
    /// labels, order the reduced positions by their final labels, and digest
    /// every member as its position in that one group rendering.
    fn resolveCyclicComponent(self: *Engine, members: []const u32) Allocator.Error!void {
        const member_count = members.len;
        const position_of_node = try self.gpa.alloc(u32, self.nodes.items.len);
        defer self.gpa.free(position_of_node);
        @memset(position_of_node, no_position);
        for (members, 0..) |node, pos| position_of_node[node] = @intCast(pos);

        // Refine to the stable bisimulation partition. Every member starts
        // from its content label: its encoding with every out-of-component
        // child as a finished digest and every in-component child as a bare
        // marker. Each round relabels a member by its own label followed by
        // the labels of its ordered in-component children until a round
        // stops separating members. A label is a pure function of the
        // member's unfolding to the current depth, so two members carry
        // equal labels exactly when they are bisimilar, and the labels are
        // identical for bisimilar positions of any two knots regardless of
        // how many nodes either knot uses.
        var labels = try self.gpa.alloc(Identity, member_count);
        defer self.gpa.free(labels);
        var next_labels = try self.gpa.alloc(Identity, member_count);
        defer self.gpa.free(next_labels);
        var distinct_labels = std.AutoHashMap(Identity, u32).init(self.gpa);
        defer distinct_labels.deinit();
        for (members, 0..) |node, pos| {
            var hasher = TypeDigestHasher.init();
            writeBytes(&hasher, "label");
            for (self.itemsOf(node)) |item| switch (item) {
                .bytes => |range| writeBytes(&hasher, self.bytesOf(range.start, range.len)),
                .child => |child| {
                    const in_component = switch (child) {
                        .digest => false,
                        .node => |target| position_of_node[target] != no_position,
                    };
                    if (in_component) {
                        writeBytes(&hasher, "in-component");
                    } else {
                        writeBytes(&hasher, "child");
                        hasher.update(&self.childDigest(child));
                    }
                },
            };
            labels[pos] = hasher.finalResult();
            try distinct_labels.put(labels[pos], no_position);
        }
        var label_count: u32 = distinct_labels.count();
        while (true) {
            distinct_labels.clearRetainingCapacity();
            for (members, 0..) |node, pos| {
                var hasher = TypeDigestHasher.init();
                hasher.update(&labels[pos]);
                for (self.childrenOf(node)) |child| {
                    const target = switch (child) {
                        .digest => continue,
                        .node => |target| target,
                    };
                    const target_pos = position_of_node[target];
                    if (target_pos == no_position) continue;
                    hasher.update(&labels[target_pos]);
                }
                next_labels[pos] = hasher.finalResult();
                try distinct_labels.put(next_labels[pos], no_position);
            }
            const next_count: u32 = distinct_labels.count();
            const stable = next_count == label_count;
            std.mem.swap([]Identity, &labels, &next_labels);
            label_count = next_count;
            if (stable) break;
        }

        // Group order: the reduced positions sorted by label. Labels are
        // intrinsic to the infinite type, so the order is too.
        const block_count = label_count;
        const sorted_labels = try self.gpa.alloc(Identity, block_count);
        defer self.gpa.free(sorted_labels);
        {
            var it = distinct_labels.keyIterator();
            var next: usize = 0;
            while (it.next()) |label| : (next += 1) sorted_labels[next] = label.*;
            std.debug.assert(next == block_count);
        }
        std.mem.sort(Identity, sorted_labels, {}, struct {
            fn lessThan(_: void, lhs: Identity, rhs: Identity) bool {
                return std.mem.order(u8, &lhs, &rhs) == .lt;
            }
        }.lessThan);
        for (sorted_labels, 0..) |label, rank| {
            const slot = distinct_labels.getPtr(label) orelse unreachable;
            slot.* = @intCast(rank);
        }
        const rank_of_member = try self.gpa.alloc(u32, member_count);
        defer self.gpa.free(rank_of_member);
        const block_rep = try self.gpa.alloc(u32, block_count);
        defer self.gpa.free(block_rep);
        @memset(block_rep, no_position);
        for (members, 0..) |node, pos| {
            const rank = distinct_labels.get(labels[pos]) orelse unreachable;
            rank_of_member[pos] = rank;
            // The partition is stable, so any member represents its position.
            if (block_rep[rank] == no_position) block_rep[rank] = node;
        }

        // One group rendering: every reduced position in label order with
        // in-component references written as ranks. Every member then
        // digests as its rank against the rendering's digest.
        var group_hasher = TypeDigestHasher.init();
        writeBytes(&group_hasher, "group");
        writeU32(&group_hasher, block_count);
        for (block_rep) |rep| {
            writeBytes(&group_hasher, "position");
            for (self.itemsOf(rep)) |item| switch (item) {
                .bytes => |range| writeBytes(&group_hasher, self.bytesOf(range.start, range.len)),
                .child => |child| {
                    const in_component: ?u32 = switch (child) {
                        .digest => null,
                        .node => |target| if (position_of_node[target] == no_position) null else rank_of_member[position_of_node[target]],
                    };
                    if (in_component) |rank| {
                        writeBytes(&group_hasher, "rank");
                        writeU32(&group_hasher, rank);
                    } else {
                        writeBytes(&group_hasher, "child");
                        group_hasher.update(&self.childDigest(child));
                    }
                },
            };
        }
        const group_digest = group_hasher.finalResult();
        for (members, 0..) |node, pos| {
            var hasher = TypeDigestHasher.init();
            writeBytes(&hasher, "group-member");
            hasher.update(&group_digest);
            writeU32(&hasher, rank_of_member[pos]);
            try self.finalize(node, hasher.finalResult());
        }
        // A rolled-out copy of any position unfolds to the same one-step
        // encoding, so it folds to that position's digest.
        for (members) |node| {
            const digest = self.nodes.items[node].digest orelse unreachable;
            try self.renderer.memo.unfoldings.put(self.unfoldingKey(node), digest);
        }
    }
};

fn transparentAliasBacking(content: SolvedType.Content) ?SolvedType.TypeVarId {
    if (std.meta.activeTag(content) != .named or content.named.kind != .alias) return null;
    return (content.named.backing orelse Common.invariant("transparent alias reached procedure identity rendering without a backing type")).ty;
}

fn writeBytes(hasher: *TypeDigestHasher, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn encodeU32(buffer: *[4]u8, value: u32) void {
    buffer[0] = @truncate(value);
    buffer[1] = @truncate(value >> 8);
    buffer[2] = @truncate(value >> 16);
    buffer[3] = @truncate(value >> 24);
}

fn writeU32(hasher: *TypeDigestHasher, value: u32) void {
    var buffer: [4]u8 = undefined;
    encodeU32(&buffer, value);
    hasher.update(&buffer);
}

test "procedure identity excludes outer callable sets but retains nested callable sets" {
    const allocator = std.testing.allocator;
    var types = SolvedType.Store.init(allocator);
    defer types.deinit();
    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();
    var memo = Memo.init(allocator);
    defer memo.deinit();
    var fn_by_symbol = std.AutoHashMap(Common.Symbol, Lifted.FnId).init(allocator);
    defer fn_by_symbol.deinit();
    const first: Common.Symbol = @enumFromInt(0);
    const second: Common.Symbol = @enumFromInt(1);
    try fn_by_symbol.put(first, @enumFromInt(0));
    try fn_by_symbol.put(second, @enumFromInt(1));
    const scalar = try types.add(.{ .primitive = .i64 });
    const args = try types.addSpan(&.{ scalar, scalar });
    const singleton = try types.add(.{ .lambda_set = try types.addMembers(&.{
        .{ .lambda = first, .captures = .empty() },
    }) });
    const joined = try types.add(.{ .lambda_set = try types.addMembers(&.{
        .{ .lambda = first, .captures = .empty() },
        .{ .lambda = second, .captures = .empty() },
    }) });
    const alone = try types.add(.{ .func = .{ .args = args, .ret = scalar, .callable = singleton } });
    const beside_lambda = try types.add(.{ .func = .{ .args = args, .ret = scalar, .callable = joined } });
    const takes_alone = try types.add(.{ .func = .{ .args = try types.addSpan(&.{alone}), .ret = scalar, .callable = singleton } });
    const takes_joined = try types.add(.{ .func = .{ .args = try types.addSpan(&.{beside_lambda}), .ret = scalar, .callable = singleton } });
    const returns_alone = try types.add(.{ .func = .{ .args = args, .ret = alone, .callable = singleton } });
    const returns_joined = try types.add(.{ .func = .{ .args = args, .ret = beside_lambda, .callable = singleton } });
    const renderer = Renderer{
        .allocator = allocator,
        .types = types.view(),
        .names = &name_store,
        .fn_tys = &.{ alone, beside_lambda },
        .source_digests = &.{ @splat(1), @splat(2) },
        .fn_by_symbol = &fn_by_symbol,
        .memo = &memo,
    };
    const identity = try renderer.specIdentity(@enumFromInt(0), alone, &.{}, "finite", "none");
    try std.testing.expectEqual(identity, try renderer.specIdentity(@enumFromInt(0), beside_lambda, &.{}, "finite", "none"));
    // Function values still distinguish the members their dispatch can select.
    try std.testing.expect(!std.mem.eql(u8, &try renderer.typeDigest(alone), &try renderer.typeDigest(beside_lambda)));
    for ([_][2]SolvedType.TypeVarId{ .{ takes_alone, takes_joined }, .{ returns_alone, returns_joined } }) |pair| {
        const left = try renderer.specIdentity(@enumFromInt(0), pair[0], &.{}, "finite", "none");
        const right = try renderer.specIdentity(@enumFromInt(0), pair[1], &.{}, "finite", "none");
        try std.testing.expect(!std.mem.eql(u8, &left, &right));
    }
    var capture = SolvedType.Capture{ .local = @enumFromInt(0), .symbol = first, .binder = null, .ty = alone };
    const captures_alone = try renderer.specIdentity(@enumFromInt(0), alone, &.{capture}, "finite", "none");
    capture.ty = beside_lambda;
    const captures_joined = try renderer.specIdentity(@enumFromInt(0), alone, &.{capture}, "finite", "none");
    try std.testing.expect(!std.mem.eql(u8, &captures_alone, &captures_joined));
    try std.testing.expect(!std.mem.eql(u8, &identity, &try renderer.specIdentity(@enumFromInt(1), alone, &.{}, "finite", "none")));
    try std.testing.expect(!std.mem.eql(u8, &identity, &try renderer.specIdentity(@enumFromInt(0), alone, &.{}, "erased", "none")));
    try std.testing.expect(!std.mem.eql(u8, &identity, &try renderer.specIdentity(@enumFromInt(0), alone, &.{}, "finite", "reuse")));
}
