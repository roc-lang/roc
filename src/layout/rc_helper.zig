//! Canonical deep-RC helper plans derived from canonical layout identities.

const builtins = @import("builtins");

const layout_mod = @import("./layout.zig");
const Store = @import("./store.zig").Store;

const Layout = layout_mod.Layout;
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
    list_incref,
    list_decref: ListPlan,
    list_free: ListPlan,
    box_incref,
    box_decref: BoxPlan,
    box_free: BoxPlan,
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

    /// Build a helper key from an operation and layout id.
    pub fn makeKey(_: *const Resolver, op: RcOp, layout_idx: Idx) HelperKey {
        return .{ .op = op, .layout_idx = layout_idx };
    }

    /// Plan the RC behavior for a canonical helper key.
    pub fn plan(self: *const Resolver, helper_key: HelperKey) Plan {
        const l = self.store.getLayout(helper_key.layout_idx);
        if (!self.store.layoutContainsRefcounted(l)) return .noop;

        return switch (l.tag) {
            .zst => .noop,
            .scalar => if (l.getScalar().tag == .str)
                switch (helper_key.op) {
                    .incref => .str_incref,
                    .decref => .str_decref,
                    .free => .str_free,
                }
            else
                .noop,
            .list, .list_of_zst => switch (helper_key.op) {
                .incref => .list_incref,
                .decref => .{ .list_decref = self.listPlan(l) },
                .free => .{ .list_free = self.listPlan(l) },
            },
            .box, .box_of_zst => switch (helper_key.op) {
                .incref => .box_incref,
                .decref => .{ .box_decref = self.boxPlan(l) },
                .free => .{ .box_free = self.boxPlan(l) },
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
        const field_layout_idx = self.store.getStructFieldLayout(struct_plan.struct_idx, @intCast(field_index));
        const field_layout = self.store.getLayout(field_layout_idx);
        if (!self.store.layoutContainsRefcounted(field_layout)) return null;
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
        return self.store.getTagUnionData(tag_plan.tag_union_idx).discriminant_offset;
    }

    /// Return the discriminant width for a tag-union helper.
    pub fn tagUnionDiscriminantSize(self: *const Resolver, tag_plan: TagUnionPlan) u8 {
        return self.store.getTagUnionData(tag_plan.tag_union_idx).discriminant_size;
    }

    /// Return the total size of the tag-union layout.
    pub fn tagUnionTotalSize(self: *const Resolver, tag_plan: TagUnionPlan) u32 {
        return self.store.getTagUnionData(tag_plan.tag_union_idx).size;
    }

    /// Return the payload helper for one variant, if that payload contains RC data.
    pub fn tagUnionVariantPlan(self: *const Resolver, tag_plan: TagUnionPlan, variant_index: u32) ?HelperKey {
        const tu_data = self.store.getTagUnionData(tag_plan.tag_union_idx);
        const variants = self.store.getTagUnionVariants(tu_data);
        const payload_layout_idx = variants.get(variant_index).payload_layout;
        const payload_layout = self.store.getLayout(payload_layout_idx);
        if (!self.store.layoutContainsRefcounted(payload_layout)) return null;
        if (self.store.layoutSizeAlign(payload_layout).size == 0) return null;

        return .{
            .op = tag_plan.child_op,
            .layout_idx = payload_layout_idx,
        };
    }

    fn listPlan(self: *const Resolver, l: Layout) ListPlan {
        const info = self.store.getListInfo(l);
        return .{
            .elem_alignment = info.elem_alignment,
            .elem_width = info.elem_size,
            .child = if (info.contains_refcounted)
                .{
                    .op = .decref,
                    .layout_idx = info.elem_layout_idx,
                }
            else
                null,
        };
    }

    fn boxPlan(self: *const Resolver, l: Layout) BoxPlan {
        const info = self.store.getBoxInfo(l);
        return .{
            .elem_alignment = info.elem_alignment,
            .child = if (info.contains_refcounted)
                .{
                    .op = .decref,
                    .layout_idx = info.elem_layout_idx,
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
