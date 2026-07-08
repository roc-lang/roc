//! Canonical deep-RC helper plans derived from canonical layout identities.

const builtins = @import("builtins");

const layout_mod = @import("./layout.zig");
const Store = @import("./store.zig").Store;

const Idx = layout_mod.Idx;
const StructIdx = layout_mod.StructIdx;
const TagUnionIdx = layout_mod.TagUnionIdx;

/// Runtime ops table passed through shared RC helpers.
pub const RocOps = builtins.utils.RocOps;
/// ABI for compiled incref helpers.
pub const RcIncrefFn = *const fn (?[*]u8, isize, *RocOps) callconv(.c) void;
/// ABI for compiled decref helpers.
pub const RcDecrefFn = *const fn (?[*]u8, *RocOps) callconv(.c) void;
/// ABI for compiled free helpers.
pub const RcFreeFn = *const fn (?[*]u8, *RocOps) callconv(.c) void;

/// Shared RC helper operation kind.
pub const RcOp = enum(u2) {
    incref,
    decref,
    free,
};

/// Canonical identity for an RC helper.
pub const HelperKey = struct {
    op: RcOp,
    layout_idx: Idx,

    /// Pack the helper key into a stable integer for backend caches.
    pub fn encode(self: HelperKey) u64 {
        const op_raw: u32 = @intFromEnum(self.op);
        const layout_raw: u32 = @intCast(@intFromEnum(self.layout_idx));
        return (@as(u64, op_raw) << 32) | layout_raw;
    }
};

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
    box_incref,
    box_decref: BoxPlan,
    box_free: BoxPlan,
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
    /// When true, a container's per-element/field/payload RC treats a
    /// `box_of_zst` element as refcounted, so an erased box carried inside a
    /// list/struct/tag is increfed on clone and decrefed on drop. The
    /// descriptor-guided boxy runtime sets this so its concrete RC path matches
    /// the interpreter, which drives the same runtime. It never changes the
    /// plans ARC consults to decide which RC statements to emit, so the lowered
    /// program is unaffected.
    erased_box_refcounted: bool = false,

    /// Create an RC helper resolver for one shared layout store.
    pub fn init(store: *const Store) Resolver {
        return .{ .store = store };
    }

    /// Create a resolver that treats erased boxes (`box_of_zst`) as refcounted
    /// container elements/fields/payloads.
    pub fn initErasedBox(store: *const Store) Resolver {
        return .{ .store = store, .erased_box_refcounted = true };
    }

    /// Whether a nested value (field/payload) participates in reference
    /// counting for this resolver's mode.
    fn nestedContainsRefcounted(self: *const Resolver, l: layout_mod.Layout) bool {
        return if (self.erased_box_refcounted)
            self.store.layoutContainsRcErasedBox(l)
        else
            self.store.layoutContainsRefcounted(l);
    }

    /// Build a helper key from an operation and layout id.
    pub fn makeKey(_: *const Resolver, op: RcOp, layout_idx: Idx) HelperKey {
        return .{ .op = op, .layout_idx = layout_idx };
    }

    /// Plan the RC behavior for a canonical helper key.
    pub fn plan(self: *const Resolver, helper_key: HelperKey) Plan {
        const l = self.store.getLayout(helper_key.layout_idx);
        switch (l.tag) {
            // An erased box carries a real refcounted heap allocation even
            // though its payload layout is zero-sized, so it participates in
            // reference counting exactly like a box.
            .box_of_zst => {},
            else => if (!self.nestedContainsRefcounted(l)) return .noop,
        }

        return switch (l.tag) {
            // ptr is never refcounted, so the early return above already handled it.
            .zst, .ptr => .noop,
            .scalar => if (l.getScalar().tag == .str)
                switch (helper_key.op) {
                    .incref => .str_incref,
                    .decref => .str_decref,
                    .free => .str_free,
                }
            else
                .noop,
            .list, .list_of_zst => switch (helper_key.op) {
                .incref => .{ .list_incref = self.listPlan(helper_key.layout_idx) },
                .decref => .{ .list_decref = self.listPlan(helper_key.layout_idx) },
                .free => .{ .list_free = self.listPlan(helper_key.layout_idx) },
            },
            .box, .box_of_zst => switch (helper_key.op) {
                .incref => .box_incref,
                .decref => .{ .box_decref = self.boxPlan(helper_key.layout_idx) },
                .free => .{ .box_free = self.boxPlan(helper_key.layout_idx) },
            },
            .erased_callable => switch (helper_key.op) {
                .incref => .erased_callable_incref,
                .decref => .erased_callable_decref,
                .free => .erased_callable_free,
            },
            .struct_ => .{ .struct_ = .{
                .struct_idx = l.getStruct().idx,
                .child_op = nestedDropOp(helper_key.op),
            } },
            .tag_union => .{ .tag_union = .{
                .tag_union_idx = l.getTagUnion().idx,
                .child_op = nestedDropOp(helper_key.op),
            } },
            .closure => .{ .closure = .{
                .op = nestedDropOp(helper_key.op),
                .layout_idx = l.getClosure().captures_layout_idx,
            } },
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
            .child = .{
                .op = struct_plan.child_op,
                .layout_idx = field_layout_idx,
            },
        };
    }

    /// Return the number of payload variants in a tag-union helper.
    pub fn tagUnionVariantCount(self: *const Resolver, tag_plan: TagUnionPlan) u32 {
        const tu_data = self.store.getTagUnionData(tag_plan.tag_union_idx);
        return @intCast(self.store.getTagUnionVariants(tu_data).len);
    }

    /// Return the byte offset of the discriminant for a tag-union helper.
    pub fn tagUnionDiscriminantOffset(self: *const Resolver, tag_plan: TagUnionPlan) u16 {
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

        return .{
            .op = tag_plan.child_op,
            .layout_idx = payload_layout_idx,
        };
    }

    fn listPlan(self: *const Resolver, list_layout_idx: Idx) ListPlan {
        const abi = self.store.builtinListAbi(list_layout_idx);
        const elem_refcounted = abi.contains_refcounted or
            (self.erased_box_refcounted and abi.elem_layout_idx != null and
                self.store.getLayout(abi.elem_layout_idx.?).tag == .box_of_zst);
        return .{
            .elem_alignment = abi.elem_alignment,
            .elem_width = abi.elem_size,
            .child = if (elem_refcounted and abi.elem_layout_idx != null)
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
        const elem_refcounted = abi.contains_refcounted or
            (self.erased_box_refcounted and abi.elem_layout_idx != null and
                self.store.getLayout(abi.elem_layout_idx.?).tag == .box_of_zst);
        return .{
            .elem_alignment = abi.elem_alignment,
            .child = if (elem_refcounted and abi.elem_layout_idx != null)
                .{
                    .op = .decref,
                    .layout_idx = abi.elem_layout_idx.?,
                }
            else
                null,
        };
    }

    fn nestedDropOp(op: RcOp) RcOp {
        return switch (op) {
            .incref => .incref,
            .decref, .free => .decref,
        };
    }
};
