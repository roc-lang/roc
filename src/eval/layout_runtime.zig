//! Operations on concrete values and explicit layout/ARC plans, shared by
//! both runtime strategies. No descriptor or dictionary state lives here.
const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout_mod = @import("layout");
const builtins = @import("builtins");
const lir_value = @import("value.zig");
const Value = lir_value.Value;
const LayoutHelper = lir_value.LayoutHelper;
const Layout = layout_mod.Layout;
const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;
const RcOp = layout_mod.RcOp;
const RcAtomicity = builtins.utils.RcAtomicity;
const Allocator = std.mem.Allocator;
const debugPrint = if (builtin.os.tag == .freestanding) struct {
    fn print(comptime _: []const u8, _: anytype) void {}
}.print else std.debug.print;
/// The error set shared by the Boxy runtime and the LIR interpreter.
pub const Error = error{
    OutOfMemory,
    RuntimeError,
    DivisionByZero,
    Crash,
    ExpectErr,
    UnsupportedHostedFunction,
    InvalidHostedFunctionSignature,
};
/// A newly allocated tag union value: `outer` is the value of the union's
/// layout, and `base` is the storage its payload and discriminant live in,
/// which is the box's data when the union layout is boxed.
pub const AllocatedTag = struct {
    outer: Value,
    base: Value,
    base_layout: layout_mod.Idx,
};
/// What allocating the data of a box layout needs.
pub const BoxAllocInfo = struct {
    elem_layout: layout_mod.Idx,
    elem_size: u32,
    elem_alignment: u32,
    contains_rc: bool,
};
/// A list value read at its explicit resolved list layout.
pub const ResolvedListBase = struct {
    value: Value,
    layout: layout_mod.Idx,
};
/// A tag union value read at its unboxed tag union layout.
pub const ResolvedTagUnionBase = struct {
    value: Value,
    layout: layout_mod.Idx,
};
/// Whether a scalar layout is an unsigned integer.
pub fn isUnsigned(layout_idx: layout_mod.Idx) bool {
    return layout_idx == .u8 or layout_idx == .u16 or layout_idx == .u32 or
        layout_idx == .u64 or layout_idx == .u128;
}
/// The RC operation applied to the items a value holds when `op` is applied
/// to the value itself.
pub fn nestedDropOp(op: RcOp) RcOp {
    return switch (op) {
        .incref => .incref,
        .decref, .free => .decref,
        .host_drop => hostDropInRcStatement(),
    };
}
/// Reads a `RocStr` from a value's bytes.
pub fn valueToRocStr(val: Value) RocStr {
    var rs: RocStr = undefined;
    @memcpy(std.mem.asBytes(&rs), val.ptr[0..@sizeOf(RocStr)]);
    return rs;
}
/// Reads a `RocList` from a value's bytes.
pub fn valueToRocList(val: Value) RocList {
    var rl: RocList = undefined;
    @memcpy(std.mem.asBytes(&rl), val.ptr[0..@sizeOf(RocList)]);
    return rl;
}

/// A view of only the data concrete value execution needs.
pub const LayoutRuntime = struct {
    layout_store: *const layout_mod.Store,
    helper: LayoutHelper,
    roc_ops: *RocOps,
    scratch: Allocator,
    fn invariantFailed(_: *const LayoutRuntime, comptime fmt: []const u8, args: anytype) noreturn {
        base.invariant(fmt, args);
    }

    fn invariantFailedError(self: *const LayoutRuntime, comptime fmt: []const u8, args: anytype) Error {
        self.invariantFailed(fmt, args);
    }

    pub fn readPointerInt(self: *const LayoutRuntime, value: Value) usize {
        return switch (self.layout_store.targetUsize()) {
            .u32 => value.read(u32),
            .u64 => value.read(usize),
        };
    }

    pub fn writePointerInt(self: *const LayoutRuntime, value: Value, raw_ptr: usize) void {
        switch (self.layout_store.targetUsize()) {
            .u32 => value.write(u32, @intCast(raw_ptr)),
            .u64 => value.write(usize, raw_ptr),
        }
    }

    pub fn readBoxedDataPointer(self: *const LayoutRuntime, boxed: Value) ?[*]u8 {
        const raw_ptr = self.readPointerInt(boxed);
        const tag_mask: usize = if (self.layout_store.targetUsize().size() == 8) 0b111 else 0b11;
        const data_ptr = raw_ptr & ~tag_mask;
        if (data_ptr == 0) return null;
        return @ptrFromInt(data_ptr);
    }

    pub fn writeBoxedDataPointer(self: *const LayoutRuntime, boxed: Value, data_ptr: ?[*]u8) void {
        const raw_ptr: usize = if (data_ptr) |ptr| @intFromPtr(ptr) else 0;
        self.writePointerInt(boxed, raw_ptr);
    }

    pub fn normalizeValueToLayout(
        self: *const LayoutRuntime,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Value {
        if (actual_layout == expected_layout) return value;

        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        switch (actual_layout_val.tag) {
            .box => {
                if (actual_layout_val.getIdx() == expected_layout) {
                    const data_ptr = self.readBoxedDataPointer(value) orelse self.invariantFailed(
                        "LIR/interpreter invariant violated: expected boxed layout {d} to contain data for inner layout {d}, but observed null box pointer",
                        .{ @backingInt(actual_layout), @backingInt(expected_layout) },
                    );
                    return .{ .ptr = data_ptr };
                }
            },
            .hole => base.invariant("a demand hole reached concrete value execution", .{}),
            .box_of_zst => if (expected_layout == .zst) return Value.zst,
            .erased_box,
            .scalar,
            .list,
            .list_of_zst,
            .struct_,
            .closure,
            .erased_callable,
            .zst,
            .tag_union,
            .ptr,
            => {},
        }

        return value;
    }

    pub fn resolveListBaseValue(
        self: *const LayoutRuntime,
        list_val: Value,
        list_layout: layout_mod.Idx,
    ) ResolvedListBase {
        const resolved_layout = self.layout_store.resolvedListLayoutIdx(list_layout) orelse self.invariantFailed(
            "LIR/interpreter invariant violated: expected explicit resolved list layout for layout {d}",
            .{@backingInt(list_layout)},
        );
        return .{
            .value = self.normalizeValueToLayout(list_val, list_layout, resolved_layout),
            .layout = resolved_layout,
        };
    }

    pub fn valueToRocListForLayout(
        self: *const LayoutRuntime,
        list_val: Value,
        list_layout: layout_mod.Idx,
    ) RocList {
        return valueToRocList(self.resolveListBaseValue(list_val, list_layout).value);
    }

    pub fn listElemLayout(self: *const LayoutRuntime, list_layout: layout_mod.Idx) layout_mod.Idx {
        const resolved_layout = self.layout_store.resolvedListLayoutIdx(list_layout) orelse self.invariantFailed(
            "LIR/interpreter invariant violated: expected explicit resolved list layout for layout {d}",
            .{@backingInt(list_layout)},
        );
        const l = self.layout_store.getLayout(resolved_layout);
        if (l.tag == .list) return l.getIdx();
        return .zst;
    }

    pub fn resolveTagUnionBaseValue(
        self: *const LayoutRuntime,
        union_val: Value,
        union_layout: layout_mod.Idx,
    ) ResolvedTagUnionBase {
        const union_layout_val = self.layout_store.getLayout(union_layout);
        if (union_layout_val.tag == .box) {
            const inner_layout = union_layout_val.getIdx();
            const data_ptr = self.readBoxedDataPointer(union_val) orelse self.invariantFailed(
                "LIR/interpreter invariant violated: boxed tag union layout {d} had null data pointer for inner layout {d}",
                .{ @backingInt(union_layout), @backingInt(inner_layout) },
            );
            return .{
                .value = .{ .ptr = data_ptr },
                .layout = inner_layout,
            };
        }

        return .{
            .value = union_val,
            .layout = union_layout,
        };
    }

    pub fn tagPayloadLayout(self: *const LayoutRuntime, union_layout: layout_mod.Idx, discriminant: u32) layout_mod.Idx {
        const l = self.layout_store.getLayout(union_layout);
        return switch (l.tag) {
            .tag_union => blk: {
                const tu_data = self.layout_store.getTagUnionData(l.getTagUnion().idx);
                const variants = self.layout_store.getTagUnionVariants(tu_data);
                break :blk if (discriminant < variants.len) variants.get(discriminant).payload_layout else .zst;
            },
            .box => blk: {
                const inner_layout = self.layout_store.getLayout(l.getIdx());
                if (inner_layout.tag != .tag_union) break :blk .zst;
                const tu_data = self.layout_store.getTagUnionData(inner_layout.getTagUnion().idx);
                const variants = self.layout_store.getTagUnionVariants(tu_data);
                break :blk if (discriminant < variants.len) variants.get(discriminant).payload_layout else .zst;
            },
            .scalar,
            .box_of_zst,
            .erased_box,
            .list,
            .list_of_zst,
            .struct_,
            .closure,
            .erased_callable,
            .zst,
            .hole,
            .ptr,
            => .zst,
        };
    }

    pub fn valuesEqual(
        self: *const LayoutRuntime,
        a: Value,
        b: Value,
        layout_idx: layout_mod.Idx,
    ) Error!bool {
        const layout_val = self.layout_store.getLayout(layout_idx);
        return switch (layout_val.tag) {
            .zst, .box_of_zst => true,
            .hole => base.invariant("a demand hole reached concrete value execution", .{}),
            .scalar => switch (layout_val.getScalar().tag) {
                .str => builtins.str.strEqual(valueToRocStr(a), valueToRocStr(b)),
                .frac => switch (self.helper.sizeOf(layout_idx)) {
                    4 => a.read(f32) == b.read(f32),
                    8 => a.read(f64) == b.read(f64),
                    16 => a.read(i128) == b.read(i128),
                    else => return self.invariantFailedError(
                        "LIR/interpreter invariant violated: fractional layout {d} has unsupported size {d}",
                        .{ @backingInt(layout_idx), self.helper.sizeOf(layout_idx) },
                    ),
                },
                .int => switch (self.helper.sizeOf(layout_idx)) {
                    1 => if (isUnsigned(layout_idx)) a.read(u8) == b.read(u8) else a.read(i8) == b.read(i8),
                    2 => if (isUnsigned(layout_idx)) a.read(u16) == b.read(u16) else a.read(i16) == b.read(i16),
                    4 => if (isUnsigned(layout_idx)) a.read(u32) == b.read(u32) else a.read(i32) == b.read(i32),
                    8 => if (isUnsigned(layout_idx)) a.read(u64) == b.read(u64) else a.read(i64) == b.read(i64),
                    16 => if (isUnsigned(layout_idx)) a.read(u128) == b.read(u128) else a.read(i128) == b.read(i128),
                    else => return self.invariantFailedError(
                        "LIR/interpreter invariant violated: scalar layout {d} has unsupported size {d}",
                        .{ @backingInt(layout_idx), self.helper.sizeOf(layout_idx) },
                    ),
                },
                .opaque_ptr => switch (self.helper.sizeOf(layout_idx)) {
                    4 => a.read(u32) == b.read(u32),
                    8 => a.read(usize) == b.read(usize),
                    else => return self.invariantFailedError(
                        "LIR/interpreter invariant violated: opaque pointer layout {d} has unsupported size {d}",
                        .{ @backingInt(layout_idx), self.helper.sizeOf(layout_idx) },
                    ),
                },
                .vector => a.read(u128) == b.read(u128),
            },
            .erased_box => return self.invariantFailedError(
                "LIR/interpreter invariant violated: equality on erased box layout {d} survived lowering",
                .{@backingInt(layout_idx)},
            ),
            .box => blk: {
                const a_ptr = self.readBoxedDataPointer(a);
                const b_ptr = self.readBoxedDataPointer(b);
                if (a_ptr == null or b_ptr == null) break :blk a_ptr == null and b_ptr == null;
                break :blk try self.valuesEqual(.{ .ptr = a_ptr.? }, .{ .ptr = b_ptr.? }, layout_val.getIdx());
            },
            .erased_callable => return self.invariantFailedError(
                "LIR/interpreter invariant violated: equality on erased callable layout {d} survived lowering",
                .{@backingInt(layout_idx)},
            ),
            .ptr => return self.invariantFailedError(
                "LIR/interpreter invariant violated: equality on compiler-internal ptr layout {d}",
                .{@backingInt(layout_idx)},
            ),
            .struct_ => blk: {
                const struct_data = self.layout_store.getStructData(layout_val.getStruct().idx);
                const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
                var field_index: usize = 0;
                while (field_index < fields.len) : (field_index += 1) {
                    const field = fields.get(@intCast(field_index));
                    // Padding spacers hold uninitialized bytes; they are not part
                    // of a value's identity and must never be compared.
                    if (field.is_padding) continue;
                    const field_layout = field.layout;
                    if (self.helper.sizeOf(field_layout) == 0) continue;
                    const field_offset = self.layout_store.getStructFieldOffsetByOriginalIndex(
                        layout_val.getStruct().idx,
                        field.index,
                    );
                    if (!try self.valuesEqual(a.offset(field_offset), b.offset(field_offset), field_layout)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .tag_union => blk: {
                const a_base = self.resolveTagUnionBaseValue(a, layout_idx);
                const b_base = self.resolveTagUnionBaseValue(b, layout_idx);
                const a_disc = self.helper.readTagDiscriminant(a_base.value, a_base.layout);
                const b_disc = self.helper.readTagDiscriminant(b_base.value, b_base.layout);
                if (a_disc != b_disc) break :blk false;
                const payload_layout = self.tagPayloadLayout(a_base.layout, a_disc);
                if (self.helper.sizeOf(payload_layout) == 0) break :blk true;
                break :blk try self.valuesEqual(a_base.value, b_base.value, payload_layout);
            },
            .list_of_zst => self.valueToRocListForLayout(a, layout_idx).len() == self.valueToRocListForLayout(b, layout_idx).len(),
            .list => blk: {
                const a_list = self.valueToRocListForLayout(a, layout_idx);
                const b_list = self.valueToRocListForLayout(b, layout_idx);
                if (a_list.len() != b_list.len()) break :blk false;
                const elem_layout = self.listElemLayout(layout_idx);
                const elem_size = self.helper.sizeOf(elem_layout);
                if (elem_size == 0) break :blk true;
                const a_bytes = a_list.bytes orelse break :blk b_list.bytes == null;
                const b_bytes = b_list.bytes orelse break :blk false;
                var i: usize = 0;
                while (i < a_list.len()) : (i += 1) {
                    const offset = i * elem_size;
                    if (!try self.valuesEqual(.{ .ptr = a_bytes + offset }, .{ .ptr = b_bytes + offset }, elem_layout)) {
                        break :blk false;
                    }
                }
                break :blk true;
            },
            .closure => return self.invariantFailedError(
                "LIR/interpreter invariant violated: function equality survived lowering",
                .{},
            ),
        };
    }

    pub fn unwrapSingleFieldPayloadLayout(self: *const LayoutRuntime, layout_idx: layout_mod.Idx) ?layout_mod.Idx {
        const layout_val = self.layout_store.getLayout(layout_idx);
        if (layout_val.tag != .struct_) return null;

        const struct_data = self.layout_store.getStructData(layout_val.getStruct().idx);
        const fields = self.layout_store.struct_fields.sliceRange(struct_data.getFields());
        if (fields.len != 1) return null;

        const field = fields.get(0);
        if (field.index != 0) return null;
        return field.layout;
    }

    pub fn readSwitchValue(self: *const LayoutRuntime, value: Value, layout_idx: layout_mod.Idx) Error!u64 {
        const layout_val = self.layout_store.getLayout(layout_idx);
        if (layout_val.tag == .tag_union) {
            if (self.helper.sizeOf(layout_idx) == 0) return 0;
            const tu_info = self.layout_store.getTagUnionInfo(layout_val);
            return tu_info.readDiscriminant(value.ptr);
        }
        return switch (self.helper.sizeOf(layout_idx)) {
            0 => 0,
            1 => value.read(u8),
            2 => value.read(u16),
            4 => value.read(u32),
            8 => value.read(u64),
            else => {
                if (builtin.mode == .debug) {
                    const layout_val_dbg = self.layout_store.getLayout(layout_idx);
                    debugPrint(
                        "LIR/interpreter bad switch layout idx={d} tag={s} size={d}\n",
                        .{ @backingInt(layout_idx), @tagName(layout_val_dbg.tag), self.helper.sizeOf(layout_idx) },
                    );
                }
                return self.invariantFailedError(
                    "LIR/interpreter invariant violated: switch condition layout {d} is not a supported scalar width",
                    .{@backingInt(layout_idx)},
                );
            },
        };
    }

    pub fn rcHelperForLayout(self: *const LayoutRuntime, op: RcOp, layout_idx: layout_mod.Idx) layout_mod.RcHelper {
        const layout_val = self.layout_store.getLayout(layout_idx);
        if (layout_val.tag == .closure) {
            return self.rcHelperForLayout(nestedDropOp(op), layout_val.getClosure().captures_layout_idx);
        }
        return .{ .op = op, .layout_idx = layout_idx };
    }

    pub fn performConcreteRc(self: *const LayoutRuntime, hooks: anytype, op: RcOp, layout_idx: layout_mod.Idx, val: Value, count: u32, atomicity: RcAtomicity) void {
        const helper = self.rcHelperForLayout(op, layout_idx);
        self.performRcHelperIfNeeded(hooks, helper, val, count, atomicity);
    }

    pub fn performRcHelperIfNeeded(self: *const LayoutRuntime, hooks: anytype, helper: layout_mod.RcHelper, val: Value, count: u32, atomicity: RcAtomicity) void {
        const plan = hooks.rcPlanFor(helper);
        if (plan == .noop) return;
        self.performRcPlan(hooks, plan, val, count, atomicity);
    }

    pub fn performRcPlan(self: *const LayoutRuntime, hooks: anytype, rc_plan: layout_mod.RcHelperPlan, val: Value, count: u32, atomicity: RcAtomicity) void {
        const utils = builtins.utils;
        switch (rc_plan) {
            .noop => {},
            .str_incref => {
                const rs = valueToRocStr(val);
                rs.increfWithAtomicity(count, atomicity, self.roc_ops);
            },
            .str_decref, .str_free => {
                const rs = valueToRocStr(val);
                rs.decrefWithAtomicity(atomicity, self.roc_ops);
            },
            .list_incref => |list_plan| {
                const rl = valueToRocList(val);
                const has_child = list_plan.child != null;
                rl.increfWithAtomicity(@intCast(count), has_child, atomicity, self.roc_ops);
            },
            inline .list_decref, .list_free => |list_plan| {
                const rl = valueToRocList(val);
                const has_child = list_plan.child != null;
                const alloc_ptr = rl.getAllocationDataPtr(self.roc_ops);
                // Before freeing the list, decref all child elements (mirrors RocList.decref logic)
                if (list_plan.child) |child_key| {
                    if (rl.isUnique(self.roc_ops)) {
                        self.decrefListElements(hooks, rl, list_plan, child_key, count, atomicity);
                    }
                }
                builtins.utils.decref(
                    alloc_ptr,
                    rl.capacity_or_alloc_ptr,
                    @intCast(list_plan.elem_alignment),
                    has_child,
                    atomicity,
                    self.roc_ops,
                );
            },
            .box_incref => {
                const alloc_ptr = val.read(?[*]u8);
                utils.increfDataPtr(alloc_ptr, @intCast(count), atomicity, self.roc_ops);
            },
            .box_decref => |box_plan| {
                const alloc_ptr = val.read(?[*]u8);
                const has_child = box_plan.child != null;
                if (box_plan.child) |child_key| {
                    if (alloc_ptr != null and builtins.utils.isUnique(alloc_ptr, self.roc_ops)) {
                        const data_ptr = self.readBoxedDataPointer(val) orelse {
                            utils.decrefDataPtr(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, atomicity, self.roc_ops);
                            return;
                        };
                        const child_val = Value{ .ptr = data_ptr };
                        self.performRcPlan(hooks, hooks.rcPlanFor(child_key), child_val, count, atomicity);
                    }
                }
                utils.decrefDataPtr(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, atomicity, self.roc_ops);
            },
            .box_free => |box_plan| {
                const alloc_ptr = val.read(?[*]u8);
                const has_child = box_plan.child != null;
                if (box_plan.child) |child_key| {
                    if (alloc_ptr != null and builtins.utils.isUnique(alloc_ptr, self.roc_ops)) {
                        const data_ptr = self.readBoxedDataPointer(val) orelse {
                            utils.freeDataPtrC(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, self.roc_ops);
                            return;
                        };
                        const child_val = Value{ .ptr = data_ptr };
                        self.performRcPlan(hooks, hooks.rcPlanFor(child_key), child_val, count, atomicity);
                    }
                }
                utils.freeDataPtrC(alloc_ptr, @intCast(box_plan.elem_alignment), has_child, self.roc_ops);
            },
            .erased_callable_incref => {
                const alloc_ptr = val.read(?[*]u8);
                builtins.utils.increfDataPtr(alloc_ptr, @intCast(count), atomicity, self.roc_ops);
            },
            .erased_callable_decref => {
                const alloc_ptr = val.read(?[*]u8);
                self.performErasedCallableFinalDropIfUnique(alloc_ptr, .decref, count);
                builtins.utils.decrefDataPtr(
                    alloc_ptr,
                    builtins.erased_callable.payload_alignment,
                    builtins.erased_callable.allocation_has_refcounted_children,
                    atomicity,
                    self.roc_ops,
                );
            },
            .erased_callable_free => {
                const alloc_ptr = val.read(?[*]u8);
                self.performErasedCallableFinalDrop(alloc_ptr, .free, count);
                builtins.utils.freeDataPtrC(
                    alloc_ptr,
                    builtins.erased_callable.payload_alignment,
                    builtins.erased_callable.allocation_has_refcounted_children,
                    self.roc_ops,
                );
            },
            .struct_ => |struct_plan| {
                const field_count = self.layout_store.rcHelperStructFieldCount(struct_plan);
                var i: u32 = 0;
                while (i < field_count) : (i += 1) {
                    const field_plan = hooks.rcStructFieldPlan(struct_plan, i) orelse continue;
                    const field_val = Value{ .ptr = val.ptr + field_plan.offset };
                    self.performRcPlan(hooks, hooks.rcPlanFor(field_plan.child), field_val, count, atomicity);
                }
            },
            .tag_union => |tag_plan| {
                const variant_count = self.layout_store.rcHelperTagUnionVariantCount(tag_plan);
                if (variant_count == 0) return;

                const disc: u32 = blk: {
                    const tu_data = self.layout_store.getTagUnionData(tag_plan.tag_union_idx);
                    const disc_offset = tu_data.discriminant_offset.get(self.layout_store.targetUsize());
                    break :blk switch (tu_data.discriminant_size) {
                        0 => 0,
                        1 => val.offset(disc_offset).read(u8),
                        2 => val.offset(disc_offset).read(u16),
                        4 => val.offset(disc_offset).read(u32),
                        else => unreachable, // a u32 variant count needs at most 4 bytes
                    };
                };

                if (disc < variant_count) {
                    if (hooks.rcTagVariantPlan(tag_plan, disc)) |child_key| {
                        // Payload is always at offset 0 in the tag union.
                        self.performRcPlan(hooks, hooks.rcPlanFor(child_key), val, count, atomicity);
                    }
                }
            },
            .closure => |child_key| {
                self.performRcPlan(hooks, hooks.rcPlanFor(child_key), val, count, atomicity);
            },
        }
    }

    pub fn decrefListElements(
        self: *const LayoutRuntime,
        hooks: anytype,
        rl: builtins.list.RocList,
        list_plan: layout_mod.RcListPlan,
        child_key: layout_mod.RcHelperKey,
        count: u32,
        atomicity: RcAtomicity,
    ) void {
        if (rl.getAllocationDataPtr(self.roc_ops)) |source| {
            const elem_count = rl.getAllocationElementCount(true, self.roc_ops);
            const child_plan = hooks.rcPlanFor(child_key);
            var i: usize = 0;
            while (i < elem_count) : (i += 1) {
                const element_ptr = source + i * list_plan.elem_width;
                const element_val = Value{ .ptr = element_ptr };
                self.performRcPlan(hooks, child_plan, element_val, count, atomicity);
            }
        }
    }

    pub fn performErasedCallableFinalDropIfUnique(
        self: *const LayoutRuntime,
        data_ptr: ?[*]u8,
        op: layout_mod.RcOp,
        count: u32,
    ) void {
        if (data_ptr == null) return;
        if (!builtins.utils.isUnique(data_ptr, self.roc_ops)) return;
        self.performErasedCallableFinalDrop(data_ptr, op, count);
    }

    pub fn performErasedCallableFinalDrop(
        self: *const LayoutRuntime,
        data_ptr: ?[*]u8,
        _: layout_mod.RcOp,
        _: u32,
    ) void {
        const ptr = data_ptr orelse return;
        const payload = builtins.erased_callable.payloadPtr(ptr);
        if (payload.on_drop) |on_drop| {
            on_drop(builtins.erased_callable.capturePtr(ptr), self.roc_ops);
        }
    }

    pub fn materializeLocalValue(
        self: *const LayoutRuntime,
        hooks: anytype,
        value: Value,
        target_layout: layout_mod.Idx,
    ) Error!Value {
        const size = self.helper.sizeOf(target_layout);
        if (size == 0) return Value.zst;

        if (value.isZst()) return try hooks.allocValue(target_layout);
        const storage = try hooks.allocValueUninit(target_layout);
        storage.copyFrom(value, size);
        return storage;
    }

    pub fn boxAllocInfo(self: *const LayoutRuntime, hooks: anytype, box_layout: Layout) BoxAllocInfo {
        return switch (box_layout.tag) {
            .box => blk: {
                const elem_layout = box_layout.getIdx();
                const elem_layout_val = self.layout_store.getLayout(elem_layout);
                break :blk .{
                    .elem_layout = elem_layout,
                    .elem_size = self.layout_store.layoutSize(elem_layout_val),
                    .elem_alignment = @intCast(elem_layout_val.alignment(self.layout_store.targetUsize()).toByteUnits()),
                    .contains_rc = hooks.layoutContainsRc(elem_layout),
                };
            },
            .hole => base.invariant("a demand hole reached concrete value execution", .{}),
            .box_of_zst => .{
                .elem_layout = .zst,
                .elem_size = 0,
                .elem_alignment = 1,
                .contains_rc = false,
            },
            .erased_box,
            .scalar,
            .list,
            .list_of_zst,
            .struct_,
            .closure,
            .erased_callable,
            .zst,
            .tag_union,
            .ptr,
            => self.invariantFailed(
                "LIR/interpreter invariant violated: expected box layout, got {s}",
                .{@tagName(box_layout.tag)},
            ),
        };
    }

    pub fn allocTagValue(self: *const LayoutRuntime, hooks: anytype, union_layout: layout_mod.Idx) Error!AllocatedTag {
        const union_layout_val = self.layout_store.getLayout(union_layout);
        if (union_layout_val.tag == .box) {
            const box_info = self.boxAllocInfo(hooks, union_layout_val);
            const data_ptr = try hooks.allocRocDataWithRc(
                box_info.elem_size,
                box_info.elem_alignment,
                box_info.contains_rc,
            );
            @memset(data_ptr[0..box_info.elem_size], 0);
            const boxed = try hooks.allocValue(union_layout);
            if (self.layout_store.targetUsize().size() == 8) {
                boxed.write(usize, @intFromPtr(data_ptr));
            } else {
                boxed.write(u32, @intCast(@intFromPtr(data_ptr)));
            }
            return .{
                .outer = boxed,
                .base = .{ .ptr = data_ptr },
                .base_layout = union_layout_val.getIdx(),
            };
        }

        const outer = try hooks.allocValue(union_layout);
        return .{
            .outer = outer,
            .base = outer,
            .base_layout = union_layout,
        };
    }

    pub fn allocBoxOfZstValue(self: *const LayoutRuntime, hooks: anytype, layout_idx: layout_mod.Idx) Error!Value {
        const boxed = try hooks.allocValue(layout_idx);
        const target_usize = self.layout_store.targetUsize();
        if (target_usize.size() == 8) {
            boxed.write(usize, 0);
        } else {
            boxed.write(u32, 0);
        }
        return boxed;
    }

    pub fn rocListToValue(self: *const LayoutRuntime, hooks: anytype, rl: RocList, ret_layout: layout_mod.Idx) Error!Value {
        const ret_layout_val = self.layout_store.getLayout(ret_layout);
        switch (ret_layout_val.tag) {
            .box => {
                const box_info = self.boxAllocInfo(hooks, ret_layout_val);
                const data_ptr = try hooks.allocRocDataWithRc(
                    box_info.elem_size,
                    box_info.elem_alignment,
                    box_info.contains_rc,
                );
                @memcpy(data_ptr[0..@sizeOf(RocList)], std.mem.asBytes(&rl));

                const boxed = try hooks.allocValue(ret_layout);
                const target_usize = self.layout_store.targetUsize();
                if (target_usize.size() == 8) {
                    boxed.write(usize, @intFromPtr(data_ptr));
                } else {
                    boxed.write(u32, @intCast(@intFromPtr(data_ptr)));
                }
                return boxed;
            },
            .box_of_zst => return try self.allocBoxOfZstValue(hooks, ret_layout),
            .erased_box => return self.invariantFailedError(
                "LIR/interpreter invariant violated: converting a RocList to erased box layout {d} requires a payload descriptor",
                .{@backingInt(ret_layout)},
            ),
            .hole => base.invariant("a demand hole reached concrete value execution", .{}),
            .scalar,
            .list,
            .list_of_zst,
            .struct_,
            .closure,
            .erased_callable,
            .zst,
            .tag_union,
            .ptr,
            => {
                const val = try hooks.allocValue(ret_layout);
                @memcpy(val.ptr[0..@sizeOf(RocList)], std.mem.asBytes(&rl));
                return val;
            },
        }
    }

    pub fn boxBox(self: *const LayoutRuntime, hooks: anytype, arg: Value, ret_layout: layout_mod.Idx) Error!Value {
        const ret_layout_val = self.layout_store.getLayout(ret_layout);
        switch (ret_layout_val.tag) {
            .box_of_zst => return try self.allocBoxOfZstValue(hooks, ret_layout),
            .erased_box => return self.invariantFailedError(
                "LIR/interpreter invariant violated: boxing into erased box layout {d} requires a payload descriptor",
                .{@backingInt(ret_layout)},
            ),
            .box => {
                const box_info = self.boxAllocInfo(hooks, ret_layout_val);
                const elem_size = box_info.elem_size;
                const elem_align = box_info.elem_alignment;
                const data_ptr = try hooks.allocRocDataWithRc(elem_size, elem_align, box_info.contains_rc);
                if (elem_size > 0) {
                    @memcpy(data_ptr[0..elem_size], arg.ptr[0..elem_size]);
                }
                const boxed = try hooks.allocValue(ret_layout);
                const target_usize = self.layout_store.targetUsize();
                if (target_usize.size() == 8) {
                    boxed.write(usize, @intFromPtr(data_ptr));
                } else {
                    boxed.write(u32, @intCast(@intFromPtr(data_ptr)));
                }
                return boxed;
            },
            .hole => base.invariant("a demand hole reached concrete value execution", .{}),
            .scalar,
            .list,
            .list_of_zst,
            .struct_,
            .closure,
            .erased_callable,
            .zst,
            .tag_union,
            .ptr,
            => return error.RuntimeError,
        }
    }

    pub fn coerceExplicitRefValueToLayout(
        self: *const LayoutRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        if (actual_layout == expected_layout) return value;
        // A demand-shaped procedure reads a list or box of holes only as its
        // header, which the value already is (design.md "Specialization By
        // Demand").
        if (try self.layout_store.acceptsAtHoles(self.scratch, actual_layout, expected_layout)) return value;

        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        const actual_is_box = actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst or actual_layout_val.tag == .erased_box;
        const expected_is_box = expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst or expected_layout_val.tag == .erased_box;
        if (actual_is_box or expected_is_box) {
            return try self.coerceExplicitNominalValueToLayout(hooks, value, actual_layout, expected_layout);
        }
        const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
        const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
        if (actual_is_list or expected_is_list) {
            return try self.coerceExplicitListValueToLayout(value, actual_layout, expected_layout);
        }

        if (actual_layout_val.tag == .struct_ or expected_layout_val.tag == .struct_ or
            actual_layout_val.tag == .tag_union or expected_layout_val.tag == .tag_union)
        {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: explicit ref reinterpret reached aggregate coercion path actual={d} ({s}) expected={d} ({s})",
                .{
                    @backingInt(actual_layout),
                    @tagName(actual_layout_val.tag),
                    @backingInt(expected_layout),
                    @tagName(expected_layout_val.tag),
                },
            );
        }

        return self.normalizeValueToLayout(value, actual_layout, expected_layout);
    }

    pub fn coerceExplicitListValueToLayout(
        self: *const LayoutRuntime,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        if (builtin.mode == .debug) {
            const actual_layout_val = self.layout_store.getLayout(actual_layout);
            const expected_layout_val = self.layout_store.getLayout(expected_layout);
            const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
            const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
            if (!actual_is_list or !expected_is_list) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: explicit list reinterpret expected list layouts, got actual={d} ({s}) expected={d} ({s})",
                    .{
                        @backingInt(actual_layout),
                        @tagName(actual_layout_val.tag),
                        @backingInt(expected_layout),
                        @tagName(expected_layout_val.tag),
                    },
                );
            }
        }

        return value;
    }

    pub fn coerceExplicitNominalValueToLayout(
        self: *const LayoutRuntime,
        hooks: anytype,
        value: Value,
        actual_layout: layout_mod.Idx,
        expected_layout: layout_mod.Idx,
    ) Error!Value {
        const actual_layout_val = self.layout_store.getLayout(actual_layout);
        const expected_layout_val = self.layout_store.getLayout(expected_layout);
        if (actual_layout_val.tag == .zst and expected_layout_val.tag == .box_of_zst) {
            return try self.allocBoxOfZstValue(hooks, expected_layout);
        }
        if (actual_layout_val.tag == .box_of_zst and expected_layout_val.tag == .zst) {
            return Value.zst;
        }

        {
            // Concrete values cross the erased ABI boundary boxed: a non-box
            // value expected as a box is wrapped into a fresh allocation, and
            // a boxed value expected concretely is read back out of it.
            const actual_val = self.layout_store.getLayout(actual_layout);
            const expected_val = self.layout_store.getLayout(expected_layout);
            const actual_is_box = actual_val.tag == .box or actual_val.tag == .box_of_zst or actual_val.tag == .erased_box;
            const expected_is_box = expected_val.tag == .box or expected_val.tag == .erased_box;
            const actual_is_erased_ptr = actual_val.tag == .scalar and actual_val.getScalar().tag == .opaque_ptr;
            const expected_is_erased_ptr = expected_val.tag == .scalar and expected_val.getScalar().tag == .opaque_ptr;
            if (expected_is_box and !actual_is_box and !actual_is_erased_ptr) {
                const sa = self.helper.sizeAlignOf(actual_layout);
                const boxed = try hooks.allocValue(expected_layout);
                if (sa.size == 0) {
                    self.writeBoxedDataPointer(boxed, null);
                    return boxed;
                }
                const data_ptr = try hooks.allocRocDataWithRc(
                    sa.size,
                    @intCast(sa.alignment.toByteUnits()),
                    hooks.layoutContainsRc(actual_layout),
                );
                @memcpy(data_ptr[0..sa.size], value.ptr[0..sa.size]);
                self.writeBoxedDataPointer(boxed, data_ptr);
                return boxed;
            }
            if (actual_is_box and !expected_is_box and !expected_is_erased_ptr) {
                const size = self.helper.sizeOf(expected_layout);
                if (size == 0) return Value.zst;
                const data_ptr = self.readBoxedDataPointer(value) orelse self.invariantFailed(
                    "LIR/interpreter invariant violated: erased boundary unbox found a null box for layout {d}",
                    .{@backingInt(expected_layout)},
                );
                const result = try hooks.allocValue(expected_layout);
                result.copyFrom(.{ .ptr = data_ptr }, size);
                return result;
            }
        }
        if (builtin.mode == .debug) {
            const actual_is_box = actual_layout_val.tag == .box or actual_layout_val.tag == .box_of_zst or actual_layout_val.tag == .erased_box;
            const expected_is_box = expected_layout_val.tag == .box or expected_layout_val.tag == .box_of_zst or expected_layout_val.tag == .erased_box;
            const actual_is_erased_ptr = actual_layout_val.tag == .scalar and actual_layout_val.getScalar().tag == .opaque_ptr;
            const expected_is_erased_ptr = expected_layout_val.tag == .scalar and expected_layout_val.getScalar().tag == .opaque_ptr;
            const actual_is_list = actual_layout_val.tag == .list or actual_layout_val.tag == .list_of_zst;
            const expected_is_list = expected_layout_val.tag == .list or expected_layout_val.tag == .list_of_zst;
            if (actual_is_list or expected_is_list) {
                if (actual_is_list and expected_is_list) return try self.coerceExplicitListValueToLayout(value, actual_layout, expected_layout);
                if (!actual_is_box and !expected_is_box) {
                    self.invariantFailed(
                        "LIR/interpreter invariant violated: explicit nominal reinterpret expected both layouts to be lists when either side is a list, got actual={d} ({s}) expected={d} ({s})",
                        .{
                            @backingInt(actual_layout),
                            @tagName(actual_layout_val.tag),
                            @backingInt(expected_layout),
                            @tagName(expected_layout_val.tag),
                        },
                    );
                }
            }
            const boxing_compatible =
                (actual_is_box == expected_is_box) or
                (actual_is_box and expected_is_erased_ptr) or
                (expected_is_box and actual_is_erased_ptr);
            if (!boxing_compatible) {
                self.invariantFailed(
                    "LIR/interpreter invariant violated: explicit nominal reinterpret expected non-list layouts on the same side of layout boxing, got actual={d} ({s}) expected={d} ({s})",
                    .{
                        @backingInt(actual_layout),
                        @tagName(actual_layout_val.tag),
                        @backingInt(expected_layout),
                        @tagName(expected_layout_val.tag),
                    },
                );
            }
        }
        if (expected_layout_val.tag == .box_of_zst) {
            return self.invariantFailedError(
                "LIR/interpreter invariant violated: non-zero-sized layout {d} targeted canonical Box({{}}) layout {d}",
                .{ @backingInt(actual_layout), @backingInt(expected_layout) },
            );
        }
        return value;
    }

    pub fn writeStructFieldValue(
        self: *const LayoutRuntime,
        hooks: anytype,
        struct_base: Value,
        field_offset: usize,
        expected_layout: layout_mod.Idx,
        actual_value: Value,
        actual_layout: layout_mod.Idx,
    ) Error!void {
        const field_size = self.helper.sizeOf(expected_layout);
        if (field_size == 0) return;
        const coerced = try self.coerceExplicitRefValueToLayout(hooks, actual_value, actual_layout, expected_layout);
        struct_base.offset(field_offset).copyFrom(coerced, field_size);
    }

    pub fn i128LiteralValue(self: *const LayoutRuntime, hooks: anytype, value: i128, layout_idx: layout_mod.Idx) Error!Value {
        const val = try hooks.allocValue(layout_idx);
        const size = self.helper.sizeOf(layout_idx);
        const bits: u128 = @bitCast(value);
        switch (size) {
            1 => val.write(u8, @truncate(bits)),
            2 => val.write(u16, @truncate(bits)),
            4 => val.write(u32, @truncate(bits)),
            8 => val.write(u64, @truncate(bits)),
            16 => val.write(i128, value),
            else => return error.RuntimeError,
        }
        return val;
    }
};

fn hostDropInRcStatement() noreturn {
    base.invariant("{s}", .{"LIR/interpreter invariant violated: RC statement carried a host-shaped drop adapter"});
}
