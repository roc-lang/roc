//! Monotype's representation slot equality-closure engine (reunify.md section
//! 10.2 and 10.4).
//!
//! This engine owns representation slots and drives the rule-bearing
//! representation relation to a fixpoint. It is built and directly tested here
//! but is NOT yet wired into production lowering: the Monotype graph still owns
//! production instantiation and temporary representation storage. The engine
//! demonstrates the section 10.4 termination contract on its own store so the
//! relation can move off the graph in a later slice.
//!
//! What the API deliberately cannot do (reunify.md section 10.2): it has no
//! operation to create a logical unknown, bind a scheme variable, add or
//! remove a field or tag, open or close a row, default a literal, resolve
//! dispatch, or change nominal identity. Those operations simply do not exist
//! on this type. `relate` also refuses two slots whose logical tokens differ,
//! so it cannot join logically unequal inputs. The only things the engine does
//! are: create a slot, relate two slots under a declared rule, relate a
//! nominal's backing at a sanctioned edge, and read the equivalence class of a
//! slot.
//!
//! Termination (reunify.md section 10.4). Every relate step either:
//!   - resolves to a pair already in one equivalence class (idempotent, no new
//!     work), or
//!   - finds the pair already in flight through the active-pair map (a cycle,
//!     closed with no new work), or
//!   - finds the join's derived identity already produced through the memo
//!     (links without descending again), or
//!   - links two distinct roots and descends into a bounded set of child
//!     relations over structurally smaller components or already-registered
//!     recursive pairs.
//! A join's derived identity is `(rule, logical token, sorted producer-atom
//! set)`, inserted into the memo BEFORE descending into backing relations, so
//! recursive backings close and revisiting one join cannot mint another
//! identity. The progress measure is the finite tuple of (distinct
//! union-find roots, unseen derived identities, in-flight relation edges):
//! every successful step strictly reduces the number of distinct roots or
//! consumes an edge without adding work, and the finite producer-atom set
//! bounds the number of derived identities, so the relation always reaches a
//! fixpoint.

const std = @import("std");
const Allocator = std.mem.Allocator;

const policy = @import("representation_policy.zig");

/// A fixed logical identity token carried by a slot. In this slice it is an
/// opaque integer supplied by the test harness; Slice 5 replaces it with the
/// real interned logical identity. `relate` requires equal tokens on both
/// operands, so the engine never joins logically unequal representations.
pub const LogicalToken = enum(u64) {
    /// The token a caller uses for a position whose logical identity is not
    /// derivable on its own — a backing-less iterator's stand-in backing. Every
    /// such position shares this token, so a rule that relates two of them still
    /// relates, while rules that keep distinct identities apart still do.
    stand_in = 0,
    _,
};

/// Identity of a representation slot owned by this engine.
pub const RepresentationSlotId = enum(u32) { _ };

/// Identity of one producer atom in a finite discovered component. Joins move
/// upward over a domain built from these atoms; a join never manufactures a
/// new atom.
pub const ProducerAtom = enum(u32) { _ };

/// The declared representation relations (reunify.md section 10.3). Every
/// `relate` call cites one of these. Nominal-backing relation is intentionally
/// absent: it is a distinct API (`relateNominalBacking`) rather than a peer
/// join, because a nominal is not logically equal to its backing and `relate`
/// requires equal logical tokens.
pub const RepresentationRule = enum(u8) {
    /// A public iterator meets its minted representation; the minted side
    /// stands. Commutative in the resulting partition, idempotent,
    /// associative.
    iterator_public_minted,
    /// A forced-dynamic iterator meets a public or minted one; the
    /// forced-dynamic side stands. Commutative in the resulting partition,
    /// idempotent, associative.
    iterator_forced_dynamic,
    /// Two distinct minted owners of one iterator declaration join under the
    /// iterator owner, relating item and backing. Idempotent and associative;
    /// the resulting partition is order-independent, though the representative
    /// prefers the left operand.
    iterator_minted_join,
    /// Two same-identity generated evidence owners: the higher-scored backing
    /// stands, without relating the backings. Idempotent and associative; the
    /// resulting partition is order-independent.
    generated_evidence_selection,
    /// A shared child component (an item type or a paired backing) reduces to
    /// representation equality: if both children are iterators it re-enters the
    /// matching iterator rule, otherwise it relates them as equal
    /// representations. Commutative, associative, idempotent.
    component_equality,
};

/// The only sanctioned edges at which a nominal's backing is related, re-stated
/// from the shared policy so callers of this engine cite one.
pub const NominalBackingEdge = policy.NominalBackingEdge;

/// The shape a slot represents. This is the engine's own compact model of the
/// representation forms that join; it is not a store type from another stage.
pub const SlotShape = union(enum) {
    /// A named iterator with a public item slot and a backing slot.
    iterator: struct {
        descriptor: policy.NamedDescriptor,
        item: RepresentationSlotId,
        backing: RepresentationSlotId,
    },
    /// A generated evidence owner whose backing is selected by score.
    evidence: struct {
        score: u8,
    },
    /// A one-child wrapper; models a box backing that references an iterator,
    /// as in the issue-10170 recursive `rest` shape.
    wrapper: RepresentationSlotId,
    /// An opaque leaf; models a primitive item. Two leaves with equal logical
    /// tokens must carry equal atoms.
    leaf: u64,
};

const Slot = struct {
    logical: LogicalToken,
    producer: ProducerAtom,
    shape: SlotShape,
    parent: RepresentationSlotId,
};

const PairId = struct {
    lo: u32,
    hi: u32,
};

const DerivedId = struct {
    rule: u8,
    logical: u64,
    atom_lo: u32,
    atom_hi: u32,
};

/// A relate step failed because the two operands are logically unequal. This
/// is a precondition violation surfaced as an error so tests can prove the
/// engine refuses logically unequal inputs.
pub const RelateError = error{LogicallyUnequal} || Allocator.Error;

/// The representation slot store plus the relation's closure state.
pub const Engine = struct {
    allocator: Allocator,
    slots: std.ArrayList(Slot),
    active: std.AutoHashMapUnmanaged(PairId, void),
    derived: std.AutoHashMapUnmanaged(DerivedId, void),

    pub fn init(allocator: Allocator) Engine {
        return .{
            .allocator = allocator,
            .slots = .empty,
            .active = .empty,
            .derived = .empty,
        };
    }

    pub fn deinit(self: *Engine) void {
        self.slots.deinit(self.allocator);
        self.active.deinit(self.allocator);
        self.derived.deinit(self.allocator);
    }

    /// Create a slot carrying a fixed logical identity token. The token is
    /// fixed at creation and never changes.
    pub fn createSlot(
        self: *Engine,
        logical: LogicalToken,
        producer: ProducerAtom,
        shape: SlotShape,
    ) Allocator.Error!RepresentationSlotId {
        const id: RepresentationSlotId = @enumFromInt(@as(u32, @intCast(self.slots.items.len)));
        try self.slots.append(self.allocator, .{
            .logical = logical,
            .producer = producer,
            .shape = shape,
            .parent = id,
        });
        return id;
    }

    /// Read a slot's current shape (following the union-find root). Read-only.
    pub fn shapeOf(self: *Engine, id: RepresentationSlotId) SlotShape {
        return self.slotConst(self.find(id)).shape;
    }

    /// The equivalence-class representative of a slot.
    pub fn find(self: *Engine, id: RepresentationSlotId) RepresentationSlotId {
        var current = id;
        while (true) {
            const parent = self.slotConst(current).parent;
            if (parent == current) return current;
            // Path halving keeps `find` near-flat without a second pass.
            const grandparent = self.slotConst(parent).parent;
            self.slotMut(current).parent = grandparent;
            current = grandparent;
        }
    }

    /// Whether two slots share one equivalence class.
    pub fn related(self: *Engine, a: RepresentationSlotId, b: RepresentationSlotId) bool {
        return self.find(a) == self.find(b);
    }

    /// Relate two slots under a declared rule, closing the relation to a
    /// fixpoint. Refuses logically unequal operands.
    pub fn relate(
        self: *Engine,
        left: RepresentationSlotId,
        right: RepresentationSlotId,
        rule: RepresentationRule,
    ) RelateError!void {
        const l = self.find(left);
        const r = self.find(right);
        if (l == r) return;
        if (self.slotConst(l).logical != self.slotConst(r).logical) {
            return error.LogicallyUnequal;
        }

        // Resolve a component-equality relation between two iterators to the
        // tier rule they classify under up front, so the active-pair guard and
        // the derived memo key on the actual rule and this pair is processed
        // exactly once.
        const effective = self.effectiveRule(rule, l, r);

        const pair = pairId(l, r);
        if (self.active.contains(pair)) return;

        const derived = self.derivedId(effective, l, r);
        if (self.derived.contains(derived)) {
            // This join's derived identity is already produced; join the roots
            // without descending into its child relations again.
            self.link(self.representativeFor(effective, l, r), l, r);
            return;
        }
        try self.derived.put(self.allocator, derived, {});

        try self.active.put(self.allocator, pair, {});
        defer _ = self.active.remove(pair);

        try self.step(effective, l, r);
    }

    fn effectiveRule(
        self: *Engine,
        rule: RepresentationRule,
        l: RepresentationSlotId,
        r: RepresentationSlotId,
    ) RepresentationRule {
        if (rule != .component_equality) return rule;
        const left = self.slotConst(l).shape;
        const right = self.slotConst(r).shape;
        if (left == .iterator and right == .iterator) {
            return switch (policy.iteratorTierRelation(left.iterator.descriptor, right.iterator.descriptor)) {
                .ordinary => .component_equality,
                .public_minted => .iterator_public_minted,
                .forced_dynamic => .iterator_forced_dynamic,
                .minted_join => .iterator_minted_join,
            };
        }
        return .component_equality;
    }

    /// Relate a nominal's backing at a sanctioned edge. Distinct from `relate`:
    /// the nominal keeps its own logical identity and the backing is a
    /// separately typed slot, so the two need not share a logical token. This
    /// engine models the relation as relating the backing slot to the nominal's
    /// backing child; the nominal value position is never replaced by the bare
    /// backing.
    pub fn relateNominalBacking(
        self: *Engine,
        nominal: RepresentationSlotId,
        backing: RepresentationSlotId,
        edge: NominalBackingEdge,
    ) RelateError!void {
        // Each sanctioned edge relates the backing the same way in this engine;
        // the edge is required so every nominal-backing relation cites one of
        // the four sanctioned edge classes.
        switch (edge) {
            .construction, .destruction, .inspection, .runtime_layout => {},
        }
        const root = self.find(nominal);
        const nominal_backing = switch (self.slotConst(root).shape) {
            .iterator => |iter| iter.backing,
            .wrapper => |child| child,
            else => return,
        };
        // The backing child and the incoming backing are the same
        // representation, related as equal components.
        try self.relate(nominal_backing, backing, .component_equality);
    }

    fn step(
        self: *Engine,
        rule: RepresentationRule,
        l: RepresentationSlotId,
        r: RepresentationSlotId,
    ) RelateError!void {
        switch (rule) {
            .iterator_public_minted,
            .iterator_forced_dynamic,
            .iterator_minted_join,
            => try self.stepIterator(rule, l, r),
            .generated_evidence_selection => try self.stepEvidence(l, r),
            .component_equality => try self.stepComponent(l, r),
        }
    }

    fn stepIterator(
        self: *Engine,
        rule: RepresentationRule,
        l: RepresentationSlotId,
        r: RepresentationSlotId,
    ) RelateError!void {
        const left = switch (self.slotConst(l).shape) {
            .iterator => |iter| iter,
            else => return error.LogicallyUnequal,
        };
        const right = switch (self.slotConst(r).shape) {
            .iterator => |iter| iter,
            else => return error.LogicallyUnequal,
        };

        const join = policy.iteratorJoin(left.descriptor, right.descriptor);
        std.debug.assert(join.relation == tierFor(rule));

        self.link(self.representativeFor(rule, l, r), l, r);

        if (join.relate_item) {
            try self.relate(left.item, right.item, .component_equality);
        }
        switch (join.relate_backing) {
            // The forced-dynamic step callable joins only in Lambda Solved; this
            // Monotype-side engine leaves the pair separate.
            .leave_separate, .step_callable_flow => {},
            .relate_pair => try self.relate(left.backing, right.backing, .component_equality),
        }
    }

    fn stepEvidence(
        self: *Engine,
        l: RepresentationSlotId,
        r: RepresentationSlotId,
    ) RelateError!void {
        const left = switch (self.slotConst(l).shape) {
            .evidence => |ev| ev,
            else => return error.LogicallyUnequal,
        };
        const right = switch (self.slotConst(r).shape) {
            .evidence => |ev| ev,
            else => return error.LogicallyUnequal,
        };
        // The higher-scored backing stands; the backings are not related.
        switch (policy.chooseGeneratedEvidenceBacking(left.score, right.score)) {
            .left => self.link(.left, l, r),
            .right => self.link(.right, l, r),
        }
    }

    fn stepComponent(
        self: *Engine,
        l: RepresentationSlotId,
        r: RepresentationSlotId,
    ) RelateError!void {
        const left_shape = self.slotConst(l).shape;
        const right_shape = self.slotConst(r).shape;
        switch (left_shape) {
            // Two iterators reach component equality only when they classify as
            // ordinary (equal identity); a tier join was routed to its rule by
            // `effectiveRule`. Equal-identity iterators relate item and backing
            // directly.
            .iterator => |left| switch (right_shape) {
                .iterator => |right| {
                    self.link(.left, l, r);
                    try self.relate(left.item, right.item, .component_equality);
                    try self.relate(left.backing, right.backing, .component_equality);
                },
                else => return error.LogicallyUnequal,
            },
            .wrapper => |left_child| switch (right_shape) {
                .wrapper => |right_child| {
                    self.link(.left, l, r);
                    try self.relate(left_child, right_child, .component_equality);
                },
                else => return error.LogicallyUnequal,
            },
            .leaf => |left_atom| switch (right_shape) {
                .leaf => |right_atom| {
                    if (left_atom != right_atom) return error.LogicallyUnequal;
                    self.link(.left, l, r);
                },
                else => return error.LogicallyUnequal,
            },
            .evidence => switch (right_shape) {
                .evidence => try self.stepEvidence(l, r),
                else => return error.LogicallyUnequal,
            },
        }
    }

    fn representativeFor(
        self: *Engine,
        rule: RepresentationRule,
        l: RepresentationSlotId,
        r: RepresentationSlotId,
    ) policy.Representative {
        return switch (rule) {
            .iterator_public_minted,
            .iterator_forced_dynamic,
            .iterator_minted_join,
            => blk: {
                const left = self.slotConst(l).shape.iterator;
                const right = self.slotConst(r).shape.iterator;
                break :blk policy.iteratorJoin(left.descriptor, right.descriptor).representative;
            },
            .generated_evidence_selection => blk: {
                const left = self.slotConst(l).shape.evidence;
                const right = self.slotConst(r).shape.evidence;
                break :blk policy.chooseGeneratedEvidenceBacking(left.score, right.score);
            },
            .component_equality => .left,
        };
    }

    fn link(
        self: *Engine,
        representative: policy.Representative,
        l: RepresentationSlotId,
        r: RepresentationSlotId,
    ) void {
        const winner = switch (representative) {
            .left => l,
            .right => r,
        };
        const loser = if (winner == l) r else l;
        self.slotMut(loser).parent = winner;
    }

    fn derivedId(
        self: *Engine,
        rule: RepresentationRule,
        l: RepresentationSlotId,
        r: RepresentationSlotId,
    ) DerivedId {
        const producer_l: u32 = @intFromEnum(self.slotConst(l).producer);
        const producer_r: u32 = @intFromEnum(self.slotConst(r).producer);
        return .{
            .rule = @intFromEnum(rule),
            .logical = @intFromEnum(self.slotConst(l).logical),
            .atom_lo = @min(producer_l, producer_r),
            .atom_hi = @max(producer_l, producer_r),
        };
    }

    fn slotConst(self: *Engine, id: RepresentationSlotId) *const Slot {
        return &self.slots.items[@intFromEnum(id)];
    }

    fn slotMut(self: *Engine, id: RepresentationSlotId) *Slot {
        return &self.slots.items[@intFromEnum(id)];
    }
};

fn pairId(a: RepresentationSlotId, b: RepresentationSlotId) PairId {
    const x: u32 = @intFromEnum(a);
    const y: u32 = @intFromEnum(b);
    return .{ .lo = @min(x, y), .hi = @max(x, y) };
}

fn tierFor(rule: RepresentationRule) policy.IteratorTierRelation {
    return switch (rule) {
        .iterator_public_minted => .public_minted,
        .iterator_forced_dynamic => .forced_dynamic,
        .iterator_minted_join => .minted_join,
        .generated_evidence_selection, .component_equality => .ordinary,
    };
}

// --- Tests: accepted/rejected pairs, algebra properties, termination ---

const testing = std.testing;

const MonoType = @import("monotype/type.zig");

const IterOptions = struct {
    representation: MonoType.IteratorRepresentation,
    generated: ?u8 = null,
    source_decl: ?u32 = 7,
    type_name: u32 = 2,
};

fn iterDescriptor(options: IterOptions) policy.NamedDescriptor {
    var def: MonoType.TypeDef = .{
        .module = @enumFromInt(1),
        .type_name = @enumFromInt(options.type_name),
        .source_decl = options.source_decl,
        .iterator_representation = options.representation,
    };
    if (options.generated) |byte| {
        def.generated = .{ .bytes = [_]u8{byte} ** 32 };
    }
    return .{
        .kind = .@"opaque",
        .def = def,
        .builtin_owner = .iter,
        .arg_count = 1,
        .backing_use = .inspectable,
    };
}

test "relate refuses logically unequal operands" {
    var engine = Engine.init(testing.allocator);
    defer engine.deinit();

    const a = try engine.createSlot(@enumFromInt(1), @enumFromInt(1), .{ .leaf = 1 });
    const b = try engine.createSlot(@enumFromInt(2), @enumFromInt(2), .{ .leaf = 1 });

    try testing.expectError(error.LogicallyUnequal, engine.relate(a, b, .component_equality));
    try testing.expect(!engine.related(a, b));
}

test "public and minted iterators join and relate their items" {
    var engine = Engine.init(testing.allocator);
    defer engine.deinit();

    const logical: LogicalToken = @enumFromInt(10);
    const public_item = try engine.createSlot(logical, @enumFromInt(1), .{ .leaf = 42 });
    const public_backing = try engine.createSlot(logical, @enumFromInt(2), .{ .leaf = 0 });
    const public = try engine.createSlot(logical, @enumFromInt(3), .{ .iterator = .{
        .descriptor = iterDescriptor(.{ .representation = .none }),
        .item = public_item,
        .backing = public_backing,
    } });

    const minted_item = try engine.createSlot(logical, @enumFromInt(4), .{ .leaf = 42 });
    const minted_backing = try engine.createSlot(logical, @enumFromInt(5), .{ .leaf = 0 });
    const minted = try engine.createSlot(logical, @enumFromInt(6), .{ .iterator = .{
        .descriptor = iterDescriptor(.{ .representation = .minted, .generated = 0x9A }),
        .item = minted_item,
        .backing = minted_backing,
    } });

    try engine.relate(public, minted, .iterator_public_minted);

    try testing.expect(engine.related(public, minted));
    try testing.expect(engine.related(public_item, minted_item));
    // The minted side stands.
    try testing.expectEqual(engine.find(minted), engine.find(public));
    // Public-minted keeps the backings separate.
    try testing.expect(!engine.related(public_backing, minted_backing));
}

test "generated evidence selection keeps the higher score and leaves backings apart" {
    var engine = Engine.init(testing.allocator);
    defer engine.deinit();

    const logical: LogicalToken = @enumFromInt(20);
    const low = try engine.createSlot(logical, @enumFromInt(1), .{ .evidence = .{ .score = 1 } });
    const high = try engine.createSlot(logical, @enumFromInt(2), .{ .evidence = .{ .score = 2 } });

    try engine.relate(low, high, .generated_evidence_selection);
    try testing.expect(engine.related(low, high));
    try testing.expectEqual(engine.find(high), engine.find(low));

    // Equal scores keep the left operand.
    const tie_left = try engine.createSlot(logical, @enumFromInt(3), .{ .evidence = .{ .score = 5 } });
    const tie_right = try engine.createSlot(logical, @enumFromInt(4), .{ .evidence = .{ .score = 5 } });
    try engine.relate(tie_left, tie_right, .generated_evidence_selection);
    try testing.expectEqual(engine.find(tie_left), engine.find(tie_right));
}

test "issue 10170: self-recursive minted backings join and terminate" {
    var engine = Engine.init(testing.allocator);
    defer engine.deinit();

    const logical: LogicalToken = @enumFromInt(30);
    const list = try buildRecursiveMinted(&engine, logical, 1, 0x6A);
    const concat = try buildRecursiveMinted(&engine, logical, 2, 0x6B);

    try engine.relate(list, concat, .iterator_minted_join);

    try testing.expect(engine.related(list, concat));
    // The recursive backings joined without minting a fresh identity.
    const list_backing = engine.shapeOf(list).iterator.backing;
    const concat_backing = engine.shapeOf(concat).iterator.backing;
    try testing.expect(engine.related(list_backing, concat_backing));
}

test "mutually recursive minted backings join and terminate" {
    var engine = Engine.init(testing.allocator);
    defer engine.deinit();

    const logical: LogicalToken = @enumFromInt(40);
    const a = try buildMutualPair(&engine, logical, 10, 0x7A, 0x7B);
    const b = try buildMutualPair(&engine, logical, 20, 0x7C, 0x7D);

    try engine.relate(a.first, b.first, .iterator_minted_join);

    try testing.expect(engine.related(a.first, b.first));
    try testing.expect(engine.related(a.second, b.second));
}

test "minted-join partition is order-independent across randomized worklist orders" {
    var seed: u64 = 0;
    while (seed < 8) : (seed += 1) {
        var prng = std.Random.DefaultPrng.init(seed);
        const random = prng.random();

        var engine = Engine.init(testing.allocator);
        defer engine.deinit();

        const logical: LogicalToken = @enumFromInt(50);
        var iters: [4]RepresentationSlotId = undefined;
        for (&iters, 0..) |*slot, index| {
            const item = try engine.createSlot(logical, @enumFromInt(@as(u32, @intCast(200 + index))), .{ .leaf = 7 });
            const backing = try engine.createSlot(logical, @enumFromInt(@as(u32, @intCast(300 + index))), .{ .leaf = 0 });
            slot.* = try engine.createSlot(logical, @enumFromInt(@as(u32, @intCast(index))), .{ .iterator = .{
                .descriptor = iterDescriptor(.{ .representation = .minted, .generated = @intCast(0x80 + index) }),
                .item = item,
                .backing = backing,
            } });
        }

        // Relate every pair in a randomized order; the closure must land them
        // all in one class regardless.
        var edges: [6][2]usize = .{ .{ 0, 1 }, .{ 1, 2 }, .{ 2, 3 }, .{ 0, 2 }, .{ 1, 3 }, .{ 0, 3 } };
        shuffle(random, &edges);
        for (edges) |edge| {
            try engine.relate(iters[edge[0]], iters[edge[1]], .iterator_minted_join);
        }

        for (iters[1..]) |slot| {
            try testing.expect(engine.related(iters[0], slot));
        }
    }
}

test "relate is idempotent" {
    var engine = Engine.init(testing.allocator);
    defer engine.deinit();

    const logical: LogicalToken = @enumFromInt(60);
    const a = try buildRecursiveMinted(&engine, logical, 1, 0x11);
    const b = try buildRecursiveMinted(&engine, logical, 2, 0x22);

    try engine.relate(a, b, .iterator_minted_join);
    const roots_after_first = distinctRoots(&engine);
    try engine.relate(a, b, .iterator_minted_join);
    try engine.relate(b, a, .iterator_minted_join);
    try testing.expectEqual(roots_after_first, distinctRoots(&engine));
}

test "relateNominalBacking relates the backing without requiring equal logical tokens" {
    var engine = Engine.init(testing.allocator);
    defer engine.deinit();

    const nominal_logical: LogicalToken = @enumFromInt(70);
    const backing_logical: LogicalToken = @enumFromInt(71);

    const inner = try engine.createSlot(backing_logical, @enumFromInt(1), .{ .leaf = 3 });
    const nominal = try engine.createSlot(nominal_logical, @enumFromInt(2), .{ .wrapper = inner });
    const backing = try engine.createSlot(backing_logical, @enumFromInt(3), .{ .leaf = 3 });

    try engine.relateNominalBacking(nominal, backing, .inspection);
    try testing.expect(engine.related(inner, backing));
    // The nominal is not merged into its backing.
    try testing.expect(!engine.related(nominal, backing));
}

fn buildRecursiveMinted(
    engine: *Engine,
    logical: LogicalToken,
    producer: u32,
    generated: u8,
) Allocator.Error!RepresentationSlotId {
    const item = try engine.createSlot(logical, @enumFromInt(1000 + producer), .{ .leaf = 0xF00D });
    // The backing slot is allocated next, and the iterator right after it, so
    // the iterator's id is one past the current length.
    const iter_id: RepresentationSlotId = @enumFromInt(@as(u32, @intCast(engine.slots.items.len + 1)));
    // The backing is a box that references the iterator itself.
    const backing = try engine.createSlot(logical, @enumFromInt(2000 + producer), .{ .wrapper = iter_id });
    const built = try engine.createSlot(logical, @enumFromInt(producer), .{ .iterator = .{
        .descriptor = iterDescriptor(.{ .representation = .minted, .generated = generated }),
        .item = item,
        .backing = backing,
    } });
    std.debug.assert(built == iter_id);
    return iter_id;
}

const MutualPair = struct {
    first: RepresentationSlotId,
    second: RepresentationSlotId,
};

fn buildMutualPair(
    engine: *Engine,
    logical: LogicalToken,
    producer: u32,
    generated_first: u8,
    generated_second: u8,
) Allocator.Error!MutualPair {
    const first_item = try engine.createSlot(logical, @enumFromInt(3000 + producer), .{ .leaf = 0xBEEF });
    const second_item = try engine.createSlot(logical, @enumFromInt(3100 + producer), .{ .leaf = 0xBEEF });

    // Order of the next four slots: first_backing, first, second_backing,
    // second. The iterators sit at length+1 and length+3.
    const first_id: RepresentationSlotId = @enumFromInt(@as(u32, @intCast(engine.slots.items.len + 1)));
    const second_id: RepresentationSlotId = @enumFromInt(@as(u32, @intCast(engine.slots.items.len + 3)));

    // first.backing -> box -> second ; second.backing -> box -> first.
    const first_backing = try engine.createSlot(logical, @enumFromInt(4000 + producer), .{ .wrapper = second_id });
    const first = try engine.createSlot(logical, @enumFromInt(producer), .{ .iterator = .{
        .descriptor = iterDescriptor(.{ .representation = .minted, .generated = generated_first }),
        .item = first_item,
        .backing = first_backing,
    } });
    std.debug.assert(first == first_id);

    const second_backing = try engine.createSlot(logical, @enumFromInt(5000 + producer), .{ .wrapper = first_id });
    const second = try engine.createSlot(logical, @enumFromInt(producer + 1), .{ .iterator = .{
        .descriptor = iterDescriptor(.{ .representation = .minted, .generated = generated_second }),
        .item = second_item,
        .backing = second_backing,
    } });
    std.debug.assert(second == second_id);

    return .{ .first = first, .second = second };
}

fn distinctRoots(engine: *Engine) usize {
    var count: usize = 0;
    for (0..engine.slots.items.len) |index| {
        const id: RepresentationSlotId = @enumFromInt(@as(u32, @intCast(index)));
        if (engine.find(id) == id) count += 1;
    }
    return count;
}

fn shuffle(random: std.Random, edges: [][2]usize) void {
    var index: usize = edges.len;
    while (index > 1) {
        index -= 1;
        const swap_with = random.intRangeLessThan(usize, 0, index + 1);
        const tmp = edges[index];
        edges[index] = edges[swap_with];
        edges[swap_with] = tmp;
    }
}
