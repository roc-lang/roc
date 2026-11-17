//! Canonical deep-RC helper plans derived from canonical layout identities.

const std = @import("std");
const base = @import("base");
const digest_mod = @import("digest.zig");
const builtins = @import("builtins");

const layout_mod = @import("./layout.zig");
const Store = @import("./store.zig").Store;

const Idx = layout_mod.Idx;
const StructIdx = layout_mod.StructIdx;
const TagUnionIdx = layout_mod.TagUnionIdx;

/// Runtime ops table a host-shaped final-drop callback receives.
pub const RocOps = builtins.utils.RocOps;

const callback_abi = builtins.rc_callback_abi;

/// ABI for compiled incref helpers.
pub const RcIncrefFn = callback_abi.RcIncrefFn;
/// ABI for compiled decref helpers.
pub const RcDecrefFn = callback_abi.RcDecrefFn;
/// ABI for compiled free helpers.
pub const RcFreeFn = callback_abi.RcFreeFn;
/// ABI for compiled host-shaped final-drop adapters.
pub const HostDropFn = callback_abi.HostDropFn;

/// Shared RC helper operation kind.
///
/// `host_drop` is not a refcount operation over a layout. It names the
/// generated adapter that performs the layout's `decref` while presenting the
/// published `Payload.on_drop` signature, so it is only ever valid in an
/// erased callable's final-drop slot.
///
/// `decref_queued` is not one either. It names the helper that performs the
/// layout's `decref` as a step of a teardown already in progress: it takes
/// that teardown's queue (`builtins.utils.DropQueue`) and passes it to every
/// queued teardown it reaches. Only plans name it, for a layout on a cycle
/// of its type.
///
/// RC statements carry the other three.
pub const RcOp = enum(u3) {
    incref,
    decref,
    free,
    host_drop,
    decref_queued,

    /// The refcount operation a helper performs, with the host-shaped adapter
    /// resolved to the operation it forwards.
    pub fn performed(self: RcOp) PerformedOp {
        return switch (self) {
            .incref => .incref,
            .decref, .host_drop, .decref_queued => .decref,
            .free => .free,
        };
    }
};

/// A refcount operation over a layout.
///
/// This is `RcOp` without `host_drop` and `decref_queued`, which are calling
/// conventions rather than operations and so never reach plan resolution.
pub const PerformedOp = enum { incref, decref, free };

/// Parameters of the generated helper for `op`, in order.
///
/// Backends build their own signature representations from this rather than
/// spelling the parameter list again, so a helper always matches the pointer
/// type the builtins call it through.
pub fn abiParams(op: RcOp) []const callback_abi.Param {
    return switch (op) {
        .incref => callback_abi.incref_params,
        .decref, .free => callback_abi.drop_params,
        .host_drop => callback_abi.host_drop_params,
        .decref_queued => callback_abi.queued_drop_params,
    };
}

/// Canonical identity for an RC helper.
pub const HelperKey = struct {
    op: RcOp,
    layout_idx: Idx,

    /// How many low bits `encode` uses; a cache that tags an encoded key
    /// places its tag above them.
    pub const encoded_bits = 32 + @bitSizeOf(RcOp);

    /// Pack the helper key into a stable integer for backend caches.
    pub fn encode(self: HelperKey) u64 {
        const op_raw: u32 = @backingInt(self.op);
        const layout_raw: u32 = @intCast(@backingInt(self.layout_idx));
        return (@as(u64, op_raw) << 32) | layout_raw;
    }

    pub fn decode(raw: u64) HelperKey {
        return .{
            .op = @fromBackingInt(@intCast(@as(std.meta.Tag(RcOp), @intCast((raw >> 32) & ((1 << @bitSizeOf(RcOp)) - 1))))),
            .layout_idx = @fromBackingInt(@intCast(@as(u32, @truncate(raw)))),
        };
    }
};

/// Whether a generated helper performs atomic refcount operations.
pub const Atomicity = enum {
    atomic,
    single_thread,
};

/// Symbol name of a generated refcount helper. It depends only on the
/// operation, the atomicity, and the content digest of the layout, never on a
/// program-local layout index, so objects compiled separately name and share
/// the same helper.
pub fn symbolName(allocator: std.mem.Allocator, store: *const Store, key: HelperKey, atomicity: Atomicity) std.mem.Allocator.Error![]u8 {
    return symbolNameForDigest(allocator, key.op, try store.contentDigest(key.layout_idx), atomicity);
}

/// `symbolName` for a layout whose digest is already known.
pub fn symbolNameForDigest(allocator: std.mem.Allocator, op: RcOp, layout_digest: digest_mod.Digest, atomicity: Atomicity) std.mem.Allocator.Error![]u8 {
    const hex = digest_mod.symbolHex(layout_digest);
    return switch (atomicity) {
        .atomic => std.fmt.allocPrint(allocator, "roc__rc_{s}_{s}", .{ @tagName(op), &hex }),
        .single_thread => std.fmt.allocPrint(allocator, "roc__rc_{s}_single_{s}", .{ @tagName(op), &hex }),
    };
}

/// RC plan for a struct-like layout whose children all use the same nested op.
pub const StructPlan = struct {
    struct_idx: StructIdx,
    child_op: RcOp,
};

/// RC plan for a tag union whose payloads all use the same nested op.
pub const TagUnionPlan = struct {
    tag_union_idx: TagUnionIdx,
    child_op: RcOp,
};

/// RC plan for list storage plus optional element callback.
pub const ListPlan = struct {
    elem_alignment: u32,
    elem_width: usize,
    child: ?HelperKey,
};

/// RC plan for box payload teardown plus optional payload callback.
pub const BoxPlan = struct {
    elem_alignment: u32,
    child: ?HelperKey,
};

/// RC plan for the teardown of a list that lies on a cycle of its type: its
/// elements hold, at some depth, more lists of this layout. The teardown is
/// queued (`builtins.utils.DropQueue`), and `child` is always a
/// `decref_queued` helper, which receives the queue.
pub const QueuedListPlan = struct {
    elem_alignment: u32,
    elem_width: usize,
    child: HelperKey,
};

/// RC plan for the teardown of a box that lies on a cycle of its type; see
/// `QueuedListPlan`.
pub const QueuedBoxPlan = struct {
    elem_alignment: u32,
    child: HelperKey,
};

/// One child step inside a struct RC helper.
pub const FieldPlan = struct {
    offset: u32,
    child: HelperKey,
};

/// Canonical RC plan derived from a canonical layout id.
pub const Plan = union(enum) {
    noop,
    str_incref,
    str_decref,
    str_free,
    list_incref: ListPlan,
    list_decref: ListPlan,
    list_free: ListPlan,
    list_decref_queued: QueuedListPlan,
    list_free_queued: QueuedListPlan,
    box_incref,
    box_decref: BoxPlan,
    box_free: BoxPlan,
    box_decref_queued: QueuedBoxPlan,
    box_free_queued: QueuedBoxPlan,
    erased_callable_incref,
    erased_callable_decref,
    erased_callable_free,
    struct_: StructPlan,
    tag_union: TagUnionPlan,
    closure: HelperKey,
};

/// Reads canonical layouts and turns them into canonical RC helper plans.
pub const Resolver = struct {
    store: *const Store,

    /// Create an RC helper resolver for one shared layout store.
    pub fn init(store: *const Store) Resolver {
        return .{ .store = store };
    }

    /// Whether a nested value (field/payload) participates in reference
    /// counting.
    fn nestedContainsRefcounted(self: *const Resolver, l: layout_mod.Layout) bool {
        return self.store.layoutContainsRefcounted(l);
    }

    /// Plan the RC behavior for a canonical helper key.
    ///
    /// The plan is `.noop` exactly when `Store.layoutContainsRefcounted` is
    /// false for the key's layout. Builtins rely on this: a backend passes an
    /// item or payload RC callback exactly when the builtin's
    /// "items are refcounted" flag, which is that same predicate, is set.
    ///
    /// A `host_drop` adapter and a `decref_queued` helper perform their
    /// layout's `decref`, so they plan as one; they differ from it only in
    /// the signature the backend gives the generated function. A helper
    /// passes the queue it received to each `decref_queued` child and each
    /// queued teardown its plan names, and a helper that received none
    /// passes null.
    ///
    /// A teardown that would otherwise nest one native frame per level of a
    /// value is planned as a queued one: the list and box layouts that lie on
    /// a cycle of their type (`Store.layoutOnCycle`). Every cycle of a type
    /// passes through one of them, so the frames a teardown nests are
    /// bounded by the type and never by the value.
    pub fn plan(self: *const Resolver, key: HelperKey) Plan {
        const op = key.op.performed();
        const l = self.store.getLayout(key.layout_idx);
        if (self.store.layoutHoldsHole(key.layout_idx)) {
            base.invariant("layout/ARC invariant violated: a value holding a demand hole has no refcount plan", .{});
        }
        if (!self.nestedContainsRefcounted(l)) {
            return .noop;
        }

        return switch (l.tag) {
            .zst, .ptr, .box_of_zst => base.invariant(
                "layout/ARC invariant violated: a layout without refcounted data reached RC planning",
                .{},
            ),
            .hole => base.invariant("layout/ARC invariant violated: a demand hole has no refcount plan", .{}),
            .dynamic => base.invariant("layout/ARC invariant violated: a dynamic local is reference counted through its runtime descriptor, never a layout plan", .{}),
            .scalar => switch (l.getScalar().tag) {
                .str => switch (op) {
                    .incref => .str_incref,
                    .decref => .str_decref,
                    .free => .str_free,
                },
                .int, .frac, .opaque_ptr, .vector => base.invariant(
                    "layout/ARC invariant violated: a non-Str scalar reached RC planning",
                    .{},
                ),
            },
            .list, .list_of_zst => switch (op) {
                .incref => .{ .list_incref = self.listPlan(key.layout_idx) },
                .decref => if (self.queuedListPlan(key.layout_idx)) |queued| .{ .list_decref_queued = queued } else .{ .list_decref = self.listPlan(key.layout_idx) },
                .free => if (self.queuedListPlan(key.layout_idx)) |queued| .{ .list_free_queued = queued } else .{ .list_free = self.listPlan(key.layout_idx) },
            },
            .box => switch (op) {
                .incref => .box_incref,
                .decref => if (self.queuedBoxPlan(key.layout_idx)) |queued| .{ .box_decref_queued = queued } else .{ .box_decref = self.boxPlan(key.layout_idx) },
                .free => if (self.queuedBoxPlan(key.layout_idx)) |queued| .{ .box_free_queued = queued } else .{ .box_free = self.boxPlan(key.layout_idx) },
            },
            .erased_callable => switch (op) {
                .incref => .erased_callable_incref,
                .decref => .erased_callable_decref,
                .free => .erased_callable_free,
            },
            .struct_ => .{ .struct_ = .{
                .struct_idx = l.getStruct().idx,
                .child_op = nestedDropOp(op),
            } },
            .tag_union => .{ .tag_union = .{
                .tag_union_idx = l.getTagUnion().idx,
                .child_op = nestedDropOp(op),
            } },
            .closure => .{ .closure = self.childKey(nestedDropOp(op), l.getClosure().captures_layout_idx) },
        };
    }

    /// Return the number of fields visited by a struct helper.
    pub fn structFieldCount(self: *const Resolver, struct_plan: StructPlan) u32 {
        return self.store.getStructData(struct_plan.struct_idx).fields.count;
    }

    /// Return the child step for one refcounted struct field, if any.
    pub fn structFieldPlan(self: *const Resolver, struct_plan: StructPlan, field_index: u32) ?FieldPlan {
        // Padding spacers hold uninitialized bytes, never a refcounted value.
        if (self.store.getStructFieldIsPadding(struct_plan.struct_idx, @intCast(field_index))) return null;
        const field_layout_idx = self.store.getStructFieldLayout(struct_plan.struct_idx, @intCast(field_index));
        const field_layout = self.store.getLayout(field_layout_idx);
        if (!self.nestedContainsRefcounted(field_layout)) return null;
        if (self.store.getStructFieldSize(struct_plan.struct_idx, @intCast(field_index)) == 0) return null;

        return .{
            .offset = self.store.getStructFieldOffset(struct_plan.struct_idx, @intCast(field_index)),
            .child = self.childKey(struct_plan.child_op, field_layout_idx),
        };
    }

    /// Return the number of payload variants in a tag-union helper.
    pub fn tagUnionVariantCount(self: *const Resolver, tag_plan: TagUnionPlan) u32 {
        const tu_data = self.store.getTagUnionData(tag_plan.tag_union_idx);
        return @intCast(self.store.getTagUnionVariants(tu_data).len);
    }

    /// Return the byte offset of the discriminant for a tag-union helper.
    pub fn tagUnionDiscriminantOffset(self: *const Resolver, tag_plan: TagUnionPlan) u32 {
        return self.store.getTagUnionDiscriminantOffset(tag_plan.tag_union_idx);
    }

    /// Return the discriminant width for a tag-union helper.
    pub fn tagUnionDiscriminantSize(self: *const Resolver, tag_plan: TagUnionPlan) u8 {
        return self.store.getTagUnionData(tag_plan.tag_union_idx).discriminant_size;
    }

    /// Return the total size of the tag-union layout.
    pub fn tagUnionTotalSize(self: *const Resolver, tag_plan: TagUnionPlan) u32 {
        return self.store.getTagUnionSize(tag_plan.tag_union_idx);
    }

    /// Return the payload helper for one variant, if that payload contains RC data.
    pub fn tagUnionVariantPlan(self: *const Resolver, tag_plan: TagUnionPlan, variant_index: u32) ?HelperKey {
        const tu_data = self.store.getTagUnionData(tag_plan.tag_union_idx);
        const variants = self.store.getTagUnionVariants(tu_data);
        const payload_layout_idx = variants.get(variant_index).payload_layout;
        const payload_layout = self.store.getLayout(payload_layout_idx);
        if (!self.nestedContainsRefcounted(payload_layout)) return null;
        if (self.store.layoutSizeAlign(payload_layout).size == 0) return null;

        return self.childKey(tag_plan.child_op, payload_layout_idx);
    }

    /// The helper a struct, tag union or closure helper calls for a child
    /// it performs `op` on. A drop of a child on a cycle of its type goes to
    /// the child's `decref_queued` helper, so the queue of a teardown in
    /// progress reaches every queued teardown below it.
    fn childKey(self: *const Resolver, op: RcOp, layout_idx: Idx) HelperKey {
        return .{
            .op = if (op == .decref and self.store.layoutOnCycle(layout_idx)) .decref_queued else op,
            .layout_idx = layout_idx,
        };
    }

    fn queuedListPlan(self: *const Resolver, list_layout_idx: Idx) ?QueuedListPlan {
        if (!self.store.layoutOnCycle(list_layout_idx)) return null;
        const abi = self.store.builtinListAbi(list_layout_idx);
        return .{
            .elem_alignment = abi.elem_alignment,
            .elem_width = abi.elem_size,
            // A list on a cycle holds refcounted elements: the cycle
            // leads back to the list itself.
            .child = .{ .op = .decref_queued, .layout_idx = abi.elem_layout_idx.? },
        };
    }

    fn queuedBoxPlan(self: *const Resolver, box_layout_idx: Idx) ?QueuedBoxPlan {
        if (!self.store.layoutOnCycle(box_layout_idx)) return null;
        const abi = self.store.builtinBoxAbi(box_layout_idx);
        return .{
            .elem_alignment = abi.elem_alignment,
            .child = .{ .op = .decref_queued, .layout_idx = abi.elem_layout_idx.? },
        };
    }

    fn listPlan(self: *const Resolver, list_layout_idx: Idx) ListPlan {
        const abi = self.store.builtinListAbi(list_layout_idx);
        return .{
            .elem_alignment = abi.elem_alignment,
            .elem_width = abi.elem_size,
            .child = if (abi.contains_refcounted and abi.elem_layout_idx != null)
                .{
                    .op = .decref,
                    .layout_idx = abi.elem_layout_idx.?,
                }
            else
                null,
        };
    }

    fn boxPlan(self: *const Resolver, box_layout_idx: Idx) BoxPlan {
        const abi = self.store.builtinBoxAbi(box_layout_idx);
        return .{
            .elem_alignment = abi.elem_alignment,
            .child = if (abi.contains_refcounted and abi.elem_layout_idx != null)
                .{
                    .op = .decref,
                    .layout_idx = abi.elem_layout_idx.?,
                }
            else
                null,
        };
    }

    fn nestedDropOp(op: PerformedOp) RcOp {
        return switch (op) {
            .incref => .incref,
            .decref, .free => .decref,
        };
    }
};
