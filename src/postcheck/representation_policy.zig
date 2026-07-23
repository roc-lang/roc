//! Shared representation-relation policy for post-check stages (reunify.md
//! section 10).
//!
//! Checking owns logical type equality, but it does not create every runtime
//! representation. The Monotype stage mints iterator chains, forced-dynamic
//! fixed points, and generated evidence backings; when two values with the
//! same logical type but different explicit representations meet, a decision
//! must be made about which representation stands and which child components
//! must be related further. That decision is the same regardless of which
//! stage stores the types, so it lives here as pure functions over immutable
//! descriptors. No store, no slot storage, and no unification live in this
//! module: each function reads copied-out descriptor fields and returns an
//! explicit decision plus the child relations the caller must schedule with
//! its own storage.
//!
//! Two stages read this policy through their own thin adapters:
//!
//! - Monotype's instantiation graph translates its graph nodes into
//!   `NamedDescriptor`s, reads the returned decision, and applies it with
//!   graph node ids.
//! - Lambda Solved translates its `TypeVarId` content into `NamedDescriptor`s
//!   and applies the same decision with type-var ids.
//!
//! Neither stage imports the other's store, and this module imports neither
//! store: it depends only on the immutable type-definition shapes in
//! `monotype/type.zig` and the builtin-owner enum in checking.

const std = @import("std");
const check = @import("check");

const MonoType = @import("monotype/type.zig");

const static_dispatch = check.StaticDispatchRegistry;
const names = check.CheckedNames;

/// Immutable view of a named type's representation-relevant fields, copied out
/// of a stage store. The policy never dereferences a store; it reads only
/// these fields.
pub const NamedDescriptor = struct {
    /// nominal / opaque / alias.
    kind: MonoType.NamedKind,
    /// Declared identity plus the recorded iterator tier, mint depth, and
    /// generated-owner digest.
    def: MonoType.TypeDef,
    /// The exact builtin producer/adapter that owns this representation.
    builtin_owner: ?static_dispatch.BuiltinOwner,
    /// Number of type arguments; index 0 is the public item type for an
    /// iterator.
    arg_count: usize,
    /// Backing read authority, or null when the type carries no backing.
    backing_use: ?MonoType.BackingUse,
};

/// The representation-tier relation between two named iterator types. Equal
/// identities and unrelated named types take ordinary named-type handling.
pub const IteratorTierRelation = enum(u8) {
    ordinary,
    public_minted,
    forced_dynamic,
    minted_join,
};

/// Which operand's representation stands after a join. The loser links to the
/// representative in the caller's store.
pub const Representative = enum(u8) {
    left,
    right,
};

/// What the caller must do with the two backing structures after an iterator
/// join.
pub const BackingFollowUp = enum(u8) {
    /// Leave the two backings unrelated. The public-and-minted meet keeps each
    /// side's backing as is; the representative already carries the minted
    /// backing.
    leave_separate,
    /// Relate the paired backing structures directly. Equal-shape minted
    /// backings must agree, and relating them closes recursive `rest`
    /// references before the pair is drained (the issue-10170 shape).
    relate_pair,
    /// The backings hold a step callable whose flow only Lambda Solved
    /// relates. Monotype leaves them separate because the two representations
    /// differ (an inline step versus a dynamic boxed step); Lambda Solved
    /// relates the pair so the step lambda sets merge. Each stage applies its
    /// own iterator-backing rule for this case, matching the section 12 split
    /// where callable flow is Lambda Solved's alone.
    step_callable_flow,
};

/// The full outcome of an iterator tier join: the tier, the representative,
/// and the child relations the caller must schedule.
pub const IteratorJoin = struct {
    relation: IteratorTierRelation,
    representative: Representative,
    /// Relate the public item type (argument index 0) of the two operands.
    relate_item: bool,
    relate_backing: BackingFollowUp,
};

/// Classify the representation-tier relation between two named types. This is
/// the single source for the tier decision shared by both stages.
///
/// Compatibility requires the same named kind and declared identity, an
/// iterator builtin owner consistent on both sides, and equal declared item
/// type at the call site. Generated identity, tier, and mint depth are
/// explicit descriptor inputs; they are never inferred from backing shape or
/// names.
pub fn iteratorTierRelation(left: NamedDescriptor, right: NamedDescriptor) IteratorTierRelation {
    if (left.kind != right.kind) return .ordinary;
    if (left.def.module != right.def.module or
        left.def.type_name != right.def.type_name or
        left.def.source_decl != right.def.source_decl)
    {
        return .ordinary;
    }
    if (!iteratorOwnerPair(left.builtin_owner, right.builtin_owner)) return .ordinary;

    const left_representation = left.def.iterator_representation;
    const right_representation = right.def.iterator_representation;
    if ((left_representation == .forced_dynamic) != (right_representation == .forced_dynamic)) {
        return .forced_dynamic;
    }
    if ((left_representation == .minted and right_representation == .none) or
        (left_representation == .none and right_representation == .minted))
    {
        return .public_minted;
    }
    if (left_representation == .minted and
        right_representation == .minted and
        !optionalDigestEql(left.def.generated, right.def.generated))
    {
        return .minted_join;
    }
    return .ordinary;
}

/// The full join outcome for a pair of named descriptors. Returns
/// `relation == .ordinary` when the pair takes ordinary named-type handling
/// (the caller then relates arguments and backings its own way).
///
/// Directional by tier, not by traversal order:
/// - `public_minted` keeps the minted side (both directions collapse to
///   minted);
/// - `forced_dynamic` keeps the forced-dynamic side (both directions collapse
///   to forced dynamic);
/// - `minted_join` keeps the iterator-owner side, preferring the left operand
///   when both are iterator owners, so distinct minted owners join under one
///   declared owner without dropping a step implementation.
pub fn iteratorJoin(left: NamedDescriptor, right: NamedDescriptor) IteratorJoin {
    const relation = iteratorTierRelation(left, right);
    return switch (relation) {
        .ordinary => .{
            .relation = .ordinary,
            .representative = .left,
            .relate_item = false,
            .relate_backing = .leave_separate,
        },
        .public_minted => .{
            .relation = .public_minted,
            .representative = if (left.def.iterator_representation == .minted) .left else .right,
            .relate_item = true,
            .relate_backing = .leave_separate,
        },
        .forced_dynamic => .{
            .relation = .forced_dynamic,
            .representative = if (left.def.iterator_representation == .forced_dynamic) .left else .right,
            .relate_item = true,
            .relate_backing = .step_callable_flow,
        },
        .minted_join => .{
            .relation = .minted_join,
            .representative = mintedOwnerRepresentative(left),
            .relate_item = true,
            .relate_backing = .relate_pair,
        },
    };
}

/// Select the backing for two same-identity generated evidence owners
/// (`FieldNames`, `FieldName`, `ParseTagUnionSpec`, and kin). One declared
/// rule: the higher declared score stands; the operand order never decides.
///
/// Equal scores must mean exactly equivalent backings, so the tie rule is a
/// declared deterministic preference for the left operand rather than an
/// operand-order accident. Slice 0 measured zero equal-score ties in the
/// corpora, so this preference has no current behavior to preserve; it is
/// declared now so no future tie can silently change an outcome.
///
/// Iterators are excluded from score selection: their backings carry step
/// callable information that must join, so they take an iterator tier join
/// instead (see `iteratorJoin`).
pub fn chooseGeneratedEvidenceBacking(left_score: u8, right_score: u8) Representative {
    return if (right_score > left_score) .right else .left;
}

/// Whether a builtin owner is a generated evidence owner whose same-identity
/// instances carry independent backing rows selected by score rather than
/// related, and is not an iterator (iterators join their backings). One
/// definition, read by every stage.
pub fn evidenceOwnerUsesScoreSelection(owner: ?static_dispatch.BuiltinOwner) bool {
    const resolved = owner orelse return false;
    return MonoType.generatedEvidenceOwnerUsesBacking(resolved) and !static_dispatch.isIteratorOwner(resolved);
}

/// The only sanctioned edges at which a nominal's backing is related. Ordinary
/// nominal equality compares identity and arguments; the backing is a
/// separately typed slot related only at one of these edges. This is a
/// distinct relation from a peer join because a nominal is not logically equal
/// to its backing, so the peer-join entry points (which require equal logical
/// identity) cannot express it. The generic try-the-backing-on-head-mismatch
/// path is not one of these edges: it is scheduled for deletion in Slice 7 and
/// must never be extracted into this policy (reunify.md section 10.5, Slice 0
/// report section 1.2).
pub const NominalBackingEdge = enum(u8) {
    construction,
    destruction,
    inspection,
    runtime_layout,
};

fn mintedOwnerRepresentative(left: NamedDescriptor) Representative {
    const owner = left.builtin_owner orelse return .right;
    return if (static_dispatch.isIteratorOwner(owner)) .left else .right;
}

fn iteratorOwnerPair(
    left: ?static_dispatch.BuiltinOwner,
    right: ?static_dispatch.BuiltinOwner,
) bool {
    const owner = left orelse right orelse return false;
    if (!static_dispatch.isIteratorOwner(owner)) return false;
    if (left) |left_owner| {
        if (left_owner != owner) return false;
    }
    if (right) |right_owner| {
        if (right_owner != owner) return false;
    }
    return true;
}

fn optionalDigestEql(left: ?names.TypeDigest, right: ?names.TypeDigest) bool {
    if (left == null and right == null) return true;
    if (left == null or right == null) return false;
    return std.mem.eql(u8, left.?.bytes[0..], right.?.bytes[0..]);
}

test "public and minted iterators collapse to the minted side, backings kept separate" {
    const left = testDescriptor(.{ .representation = .none });
    const right = testDescriptor(.{ .representation = .minted, .generated = 0x11 });

    const join = iteratorJoin(left, right);
    try std.testing.expectEqual(IteratorTierRelation.public_minted, join.relation);
    try std.testing.expectEqual(Representative.right, join.representative);
    try std.testing.expect(join.relate_item);
    try std.testing.expectEqual(BackingFollowUp.leave_separate, join.relate_backing);

    // The relation is symmetric in classification and picks the minted side
    // regardless of operand order.
    const swapped = iteratorJoin(right, left);
    try std.testing.expectEqual(IteratorTierRelation.public_minted, swapped.relation);
    try std.testing.expectEqual(Representative.left, swapped.representative);
}

test "forced dynamic meets minted, forced-dynamic side stands, step callable flow deferred" {
    const dynamic = testDescriptor(.{ .representation = .forced_dynamic });
    const minted = testDescriptor(.{ .representation = .minted, .generated = 0x22 });

    const join = iteratorJoin(dynamic, minted);
    try std.testing.expectEqual(IteratorTierRelation.forced_dynamic, join.relation);
    try std.testing.expectEqual(Representative.left, join.representative);
    try std.testing.expect(join.relate_item);
    try std.testing.expectEqual(BackingFollowUp.step_callable_flow, join.relate_backing);

    const swapped = iteratorJoin(minted, dynamic);
    try std.testing.expectEqual(Representative.right, swapped.representative);
}

test "distinct minted owners join under the iterator owner and relate backings" {
    const list = testDescriptor(.{ .representation = .minted, .generated = 0x33 });
    const concat = testDescriptor(.{ .representation = .minted, .generated = 0x44 });

    const join = iteratorJoin(list, concat);
    try std.testing.expectEqual(IteratorTierRelation.minted_join, join.relation);
    try std.testing.expectEqual(Representative.left, join.representative);
    try std.testing.expect(join.relate_item);
    try std.testing.expectEqual(BackingFollowUp.relate_pair, join.relate_backing);
}

test "equal minted identity and unrelated types are ordinary" {
    const one = testDescriptor(.{ .representation = .minted, .generated = 0x55 });
    try std.testing.expectEqual(IteratorTierRelation.ordinary, iteratorTierRelation(one, one));

    var other = testDescriptor(.{ .representation = .minted, .generated = 0x55 });
    other.def.type_name = @enumFromInt(999);
    try std.testing.expectEqual(IteratorTierRelation.ordinary, iteratorTierRelation(one, other));
}

test "generated evidence backing selection is score-directed with a left-operand tie rule" {
    try std.testing.expectEqual(Representative.right, chooseGeneratedEvidenceBacking(1, 2));
    try std.testing.expectEqual(Representative.left, chooseGeneratedEvidenceBacking(2, 1));
    // Declared deterministic tie: equal scores keep the left operand.
    try std.testing.expectEqual(Representative.left, chooseGeneratedEvidenceBacking(2, 2));
}

test "iterator owners are excluded from score selection" {
    try std.testing.expect(evidenceOwnerUsesScoreSelection(.fields));
    try std.testing.expect(evidenceOwnerUsesScoreSelection(.parse_tag_union_spec));
    try std.testing.expect(!evidenceOwnerUsesScoreSelection(.iter));
    try std.testing.expect(!evidenceOwnerUsesScoreSelection(.stream));
    try std.testing.expect(!evidenceOwnerUsesScoreSelection(.list));
    try std.testing.expect(!evidenceOwnerUsesScoreSelection(null));
}

const TestDescriptorOptions = struct {
    representation: MonoType.IteratorRepresentation,
    generated: ?u8 = null,
    builtin_owner: ?static_dispatch.BuiltinOwner = .iter,
};

fn testDescriptor(options: TestDescriptorOptions) NamedDescriptor {
    var def: MonoType.TypeDef = .{
        .module = @enumFromInt(1),
        .type_name = @enumFromInt(2),
        .source_decl = 3,
        .iterator_representation = options.representation,
    };
    if (options.generated) |byte| {
        def.generated = .{ .bytes = [_]u8{byte} ** 32 };
    }
    return .{
        .kind = .@"opaque",
        .def = def,
        .builtin_owner = options.builtin_owner,
        .arg_count = 1,
        .backing_use = .inspectable,
    };
}
