//! The descriptor operations compiled code performs through the runtime
//! rather than inline: component addresses by index or through a nominal's
//! backing, value construction, representation adaptation by coercion plan,
//! and the list operations a `.boxy` generic body emits with an element
//! descriptor (design.md "Dynamic Values In LIR"). Every field and variant
//! is reached by a position the compiler computed; nothing here looks one up
//! by name. `exports.zig` exposes each as a `roc_desc_*` C entry point.

const std = @import("std");
const builtins = @import("builtins");
const desc_mod = @import("desc.zig");
const walk = @import("walk.zig");

const RocOps = builtins.host_abi.RocOps;
const RocList = builtins.list.RocList;
const UpdateMode = builtins.utils.UpdateMode;
const list = builtins.list;
const utils = builtins.utils;
const erased_callable = builtins.erased_callable;

const TypeDesc = desc_mod.TypeDesc;
const Variant = desc_mod.Variant;
const AdaptPlan = desc_mod.AdaptPlan;

const invariant = walk.invariant;

/// A nominal shares its backing's layout; field and tag lookups read the
/// backing's entries.
pub fn backing(desc: *const TypeDesc) *const TypeDesc {
    var current = desc;
    while (current.kind == .nominal) current = current.child.?;
    return current;
}

fn recordOf(desc: *const TypeDesc, ops: *RocOps) *const TypeDesc {
    const record = backing(desc);
    if (record.kind != .record and record.kind != .tuple) invariant(ops, "a field address of a descriptor that is not a record or tuple");
    return record;
}

fn unionOf(desc: *const TypeDesc, ops: *RocOps) *const TypeDesc {
    const union_desc = backing(desc);
    if (union_desc.kind != .tag_union) invariant(ops, "a tag of a descriptor that is not a tag union");
    return union_desc;
}

/// The offset of argument `index` of `variant` within its union's bytes.
fn payloadOffset(variant: *const Variant, index: u32) u32 {
    if (variant.payload_count == 1) return 0;
    return variant.payload.fieldBySemanticIndex(index).offset;
}

fn payloadDesc(variant: *const Variant, index: u32) *const TypeDesc {
    if (variant.payload_count == 1) return variant.payload;
    return variant.payload.fieldBySemanticIndex(index).desc;
}

/// The address of the value a component slot `desc` describes holds: the
/// slot itself, or the payload of the box a recursive type's layout puts
/// in the slot.
pub fn componentAddr(desc: *const TypeDesc, slot: [*]u8) [*]u8 {
    if (desc.kind != .recursive_box) return slot;
    return boxPayloadAddr(desc.child.?, @ptrCast(@alignCast(slot)));
}

/// Move the value at `src` into the component slot `desc` describes,
/// boxing it when the slot is a recursive type's box.
fn writeComponent(desc: *const TypeDesc, slot: [*]u8, src: [*]const u8, ops: *RocOps) void {
    if (desc.kind == .recursive_box) return buildBox(desc.child.?, @ptrCast(@alignCast(slot)), src, ops);
    walk.copyBytes(slot, src, desc.size);
}

fn writeDiscriminant(desc: *const TypeDesc, bytes: [*]u8, discriminant: u32) void {
    const at = bytes + desc.discriminant_offset;
    switch (desc.discriminant_size) {
        0 => {},
        1 => at[0] = @intCast(discriminant),
        2 => std.mem.writeInt(u16, at[0..2], @intCast(discriminant), .little),
        4 => std.mem.writeInt(u32, at[0..4], discriminant, .little),
        8 => std.mem.writeInt(u64, at[0..8], discriminant, .little),
        else => unreachable, // the compiler only produces 0, 1, 2, 4, or 8
    }
}

// Component addresses.

/// The address of field `field_index` of the record or tuple at `base`.
pub fn fieldAddr(desc: *const TypeDesc, base: [*]u8, field_index: u32, ops: *RocOps) [*]u8 {
    const record = recordOf(desc, ops);
    if (field_index >= record.count) invariant(ops, "a field index past its record's fields");
    const field = record.fieldBySemanticIndex(field_index);
    return componentAddr(field.desc, base + field.offset);
}

/// The discriminant of the tag union value at `base`.
pub fn tagDiscriminant(desc: *const TypeDesc, base: [*]const u8, ops: *RocOps) u32 {
    const union_desc = backing(desc);
    if (union_desc.kind != .tag_union and union_desc.kind != .bool) invariant(ops, "a discriminant of a descriptor that is not a tag union");
    return union_desc.readDiscriminant(base);
}

/// The address of payload `payload_index` of the value's active variant.
/// Lowering establishes the variant before projecting its typed payload;
/// the stored discriminant indexes the union's variants.
pub fn tagPayloadAddr(desc: *const TypeDesc, base: [*]u8, payload_index: u32, ops: *RocOps) [*]u8 {
    const union_desc = backing(desc);
    if (union_desc.kind != .tag_union) invariant(ops, "a payload of a descriptor that is not a tag union");
    const variant = &union_desc.variants()[union_desc.readDiscriminant(base)];
    if (payload_index >= variant.payload_count) invariant(ops, "a tag argument index past its tag's arguments");
    return componentAddr(payloadDesc(variant, payload_index), base + payloadOffset(variant, payload_index));
}

/// The address of item `index` of a list whose items `elem` describes.
pub fn listElemAddr(elem: *const TypeDesc, roc_list: *const RocList, index: u64) [*]u8 {
    if (elem.size == 0) return walk.zeroSizedPtr();
    return roc_list.bytes.? + @as(usize, @intCast(index)) * elem.size;
}

/// The payload of a box whose payload `desc` describes. A box of a
/// zero-sized payload holds no allocation.
pub fn boxPayloadAddr(desc: *const TypeDesc, box: *const usize) [*]u8 {
    // A box of a function is the callable itself (design.md "Function Values").
    if (desc.kind == .erased_callable and desc.child == null) return @ptrCast(@constCast(box));
    if (desc.flags.sequence) return builtins.boxed_sequence.recordPtr(@ptrFromInt(box.*));
    const word = box.* & ~utils.DATA_PTR_TAG_MASK;
    if (desc.size == 0 or word == 0) return walk.zeroSizedPtr();
    return @ptrFromInt(word);
}

// Construction. Every value operand is the address of its bytes.

/// Write the record `desc` describes to `dst` from its fields' bytes.
pub fn buildRecord(desc: *const TypeDesc, dst: [*]u8, fields: []const [*]const u8, ops: *RocOps) void {
    const record = recordOf(desc, ops);
    if (fields.len > record.count) invariant(ops, "a record built from more fields than its descriptor has");
    var initialized_size: u32 = 0;
    for (fields, 0..) |src, index| {
        const field = record.fieldBySemanticIndex(@intCast(index));
        writeComponent(field.desc, dst + field.offset, src, ops);
        initialized_size += field.desc.size;
    }
    // Keep operand and recursive-box allocation order. Dense records are
    // already complete; only layouts with unwritten bytes need a gap walk.
    std.debug.assert(initialized_size <= record.size);
    if (initialized_size == record.size) return;
    var end: u32 = 0;
    for (record.fields()) |field| {
        if (field.field_index >= fields.len) continue;
        if (end != field.offset) @memset(dst[end..field.offset], 0);
        end = field.offset + field.desc.size;
    }
    if (end != record.size) @memset(dst[end..record.size], 0);
}

/// Build the record `desc` describes at `dst` from the record `base`
/// (described by `base_desc`) by the update plan `plan`
/// (`desc.update_value_flag`): each field the plan gives a value takes that
/// one of `values`, moved in, and every other field the base's field the
/// plan names, shared. `base` is released.
pub fn updateRecord(base_desc: *const TypeDesc, base: [*]u8, desc: *const TypeDesc, dst: [*]u8, plan: [*]const u32, values: []const [*]const u8, ops: *RocOps) void {
    const record = recordOf(desc, ops);
    const base_fields = recordOf(base_desc, ops).fields();
    var end: u32 = 0;
    for (record.fields(), 0..) |*field, slot| {
        if (field.is_padding != 0) continue;
        if (end != field.offset) @memset(dst[end..field.offset], 0);
        end = field.offset + field.desc.size;
        const entry = plan[slot];
        if (entry & desc_mod.update_value_flag != 0) {
            const position = entry & ~desc_mod.update_value_flag;
            if (position >= values.len) invariant(ops, "an update plan names a value past the update's values");
            writeComponent(field.desc, dst + field.offset, values[position], ops);
            continue;
        }
        if (entry >= base_fields.len) invariant(ops, "an update plan names a field past its base's fields");
        const source = &base_fields[entry];
        walk.copyBytes(dst + field.offset, base + source.offset, field.desc.size);
        walk.incref(field.desc, dst + field.offset, 1, ops);
    }
    if (end != record.size) @memset(dst[end..record.size], 0);
    walk.decref(base_desc, base, ops);
}

/// Write the variant whose discriminant is `discriminant` of `desc`'s union
/// to `dst` from its payloads' bytes.
pub fn buildTag(desc: *const TypeDesc, discriminant: u32, dst: [*]u8, payloads: []const [*]const u8, ops: *RocOps) void {
    const union_desc = unionOf(desc, ops);
    if (discriminant >= union_desc.count) invariant(ops, "a tag's discriminant is past its union's variants");
    const variant = &union_desc.variants()[discriminant];
    if (payloads.len != variant.payload_count) invariant(ops, "a tag built with a different number of arguments than its descriptor has");
    if (payloads.len == 1) {
        // A single payload starts at zero and writeComponent initializes
        // its entire slot, including a recursive box's pointer. Preserve
        // zeroes in the unused payload space, alignment gaps, and tail.
        const payload = variant.payload;
        writeComponent(payload, dst, payloads[0], ops);
        @memset(dst[payload.size..union_desc.size], 0);
    } else {
        @memset(dst[0..union_desc.size], 0);
        for (payloads, 0..) |src, index| {
            writeComponent(payloadDesc(variant, @intCast(index)), dst + payloadOffset(variant, @intCast(index)), src, ops);
        }
    }
    writeDiscriminant(union_desc, dst, variant.discriminant);
}

/// Write a list of the given items, which `elem` describes, to `dst`.
pub fn buildList(elem: *const TypeDesc, dst: *RocList, elems: []const [*]const u8, ops: *RocOps) void {
    if (elems.len == 0 or elem.size == 0) {
        dst.* = zstList(elems.len);
        return;
    }
    const data = utils.allocateWithRefcount(elem.size * elems.len, elem.alignment, elem.flags.contains_refcounted, ops);
    for (elems, 0..) |src, index| {
        walk.copyBytes(data + index * elem.size, src, elem.size);
    }
    dst.* = .{ .bytes = data, .length = elems.len, .capacity_or_alloc_ptr = RocList.encodeCapacity(elems.len) };
}

/// Write a box holding the payload `desc` describes to `dst`.
pub fn buildBox(desc: *const TypeDesc, dst: *usize, payload: [*]const u8, ops: *RocOps) void {
    // A box of a function is the callable itself (design.md "Function Values"), which
    // the box takes over.
    if (desc.kind == .erased_callable and desc.child == null) {
        dst.* = @as(*align(1) const usize, @ptrCast(payload)).*;
        return;
    }
    if (desc.flags.sequence) {
        const record = recordOf(desc, ops);
        const fields = record.fields();
        if (fields.len != 2) invariant(ops, "builtin sequence descriptor must contain its two-field record");
        dst.* = @intFromPtr(builtins.boxed_sequence.pack(payload, desc.size, fields[1].offset, ops));
        return;
    }
    if (desc.size == 0) {
        dst.* = 0;
        return;
    }
    const data = utils.allocateWithRefcount(desc.size, desc.alignment, desc.flags.contains_refcounted, ops);
    walk.copyBytes(data, payload, desc.size);
    dst.* = @intFromPtr(data);
}

/// A box at `dst` whose payload `desc` describes, zeroed.
pub fn allocSlot(desc: *const TypeDesc, dst: *usize, ops: *RocOps) void {
    if (desc.size == 0) {
        dst.* = 0;
        return;
    }
    const data = utils.allocateWithRefcount(desc.size, desc.alignment, desc.flags.contains_refcounted, ops);
    @memset(data[0..desc.size], 0);
    dst.* = @intFromPtr(data);
}

/// Move the value at `value` into the zeroed payload of the box `slot`
/// holds, and write that box to `dst`.
pub fn storeSlot(desc: *const TypeDesc, slot: *const usize, value: [*]const u8, dst: *usize) void {
    if (desc.size != 0) {
        const data: [*]u8 = @ptrFromInt(slot.*);
        walk.copyBytes(data, value, desc.size);
    }
    dst.* = slot.*;
}

// Representation adaptation.

/// One value of an adaptation: the source bytes rebuilt at `dst` by `plan`.
const AdaptWork = struct { plan: *const AdaptPlan, src: [*]const u8, dst: [*]u8 };

/// Rebuild the value `src` (borrowed) at `dst` as an owned value by the
/// coercion plan `plan` (`desc.AdaptPlan`), which the compiler built for the
/// pair of descriptors the coercion converts between: record fields move by
/// offset, variants re-tag by discriminant, and presence slots wrap or
/// unwrap, a `#Missing` slot crashing where its destination requires the
/// field. Lists and boxes whose contents change storage are rebuilt;
/// everything else is copied and retained.
pub fn adapt(plan: *const AdaptPlan, dst: [*]u8, src: [*]const u8, ops: *RocOps) void {
    var stack: walk.Stack(AdaptWork, 16) = undefined;
    stack.init();
    defer stack.deinit(ops);
    stack.push(.{ .plan = plan, .src = src, .dst = dst }, ops);
    while (stack.pop()) |work| {
        const node = work.plan;
        switch (node.kind) {
            .copy => {
                const t = node.to.?;
                walk.copyBytes(work.dst, work.src, t.size);
                walk.incref(t, work.dst, 1, ops);
            },
            .wrap => {
                const t = node.to.?;
                @memset(work.dst[0..t.size], 0);
                writeDiscriminant(t, work.dst, node.discriminant);
                stack.push(.{ .plan = node.child.?, .src = work.src, .dst = work.dst }, ops);
            },
            .unwrap => {
                if (node.from.?.readDiscriminant(work.src) != node.discriminant) ops.crash("an optional record field that is missing reached a position that requires it");
                stack.push(.{ .plan = node.child.?, .src = work.src, .dst = work.dst }, ops);
            },
            .missing => writeDiscriminant(node.to.?, work.dst, node.discriminant),
            .retag => {
                const t = node.to.?;
                const source = node.from.?.readDiscriminant(work.src);
                if (source >= node.count) invariant(ops, "a re-tagged value's discriminant is past its plan's variants");
                const target = node.variants()[source];
                if (target.discriminant == desc_mod.AdaptVariant.absent) invariant(ops, "a re-tagged variant is not in its destination's row");
                @memset(work.dst[0..t.size], 0);
                writeDiscriminant(t, work.dst, target.discriminant);
                if (target.plan) |payload| stack.push(.{ .plan = payload, .src = work.src, .dst = work.dst }, ops);
            },
            .record => {
                @memset(work.dst[0..node.to.?.size], 0);
                for (node.fields()) |field| {
                    stack.push(.{ .plan = field.plan, .src = work.src + field.from_offset, .dst = work.dst + field.to_offset }, ops);
                }
            },
            .list => {
                const source: *const RocList = @ptrCast(@alignCast(work.src));
                const target: *RocList = @ptrCast(@alignCast(work.dst));
                const from_elem = node.from.?.child.?;
                const to_elem = node.to.?.child.?;
                const len = source.len();
                if (len == 0 or to_elem.size == 0) {
                    target.* = zstList(len);
                    continue;
                }
                const data = utils.allocateWithRefcount(to_elem.size * len, to_elem.alignment, to_elem.flags.contains_refcounted, ops);
                target.* = .{ .bytes = data, .length = len, .capacity_or_alloc_ptr = RocList.encodeCapacity(len) };
                // A list of zero-sized elements has no bytes (`zstList`):
                // each element reads none, so any address stands for it.
                const elements: [*]u8 = source.bytes orelse elements: {
                    if (from_elem.size != 0) invariant(ops, "a list of sized elements has no bytes");
                    break :elements data;
                };
                for (0..len) |index| {
                    stack.push(.{ .plan = node.child.?, .src = elements + index * from_elem.size, .dst = data + index * to_elem.size }, ops);
                }
            },
            .box => {
                const payload_to = node.to.?.child.?;
                const target: *usize = @ptrCast(@alignCast(work.dst));
                if (payload_to.size == 0) {
                    target.* = 0;
                    continue;
                }
                const word = @as(*const usize, @ptrCast(@alignCast(work.src))).* & ~utils.DATA_PTR_TAG_MASK;
                const data = utils.allocateWithRefcount(payload_to.size, payload_to.alignment, payload_to.flags.contains_refcounted, ops);
                target.* = @intFromPtr(data);
                stack.push(.{ .plan = node.child.?, .src = @ptrFromInt(word), .dst = data }, ops);
            },
            .mismatch => invariant(ops, "adapted descriptors describe different kinds of value"),
        }
    }
}

/// Bytes before a static callable's payload: alignment padding, then the
/// refcount word immediately before the payload.
pub const static_callable_header: usize = erased_callable.capture_alignment;

// List operations through an element descriptor.

/// The list operations compiled code performs with an element descriptor.
pub const ListOp = enum(u32) {
    capacity,
    slack_unique,
    owned_unique,
    get_unsafe,
    append_unsafe,
    concat,
    append_range_within,
    copy_range_within,
    append_range_within_unsafe,
    append_sublist,
    prepend,
    swap,
    map_can_reuse,
    item_size_at_most,
    map_cast_unsafe,
    map_extract_unsafe,
    map_write_unsafe,
    sublist,
    sublist_borrowed,
    drop_at,
    replace_unsafe,
    set,
    set_in_place_unsafe,
    with_capacity,
    reserve,
    reserve_for_append,
    clear,
    release_excess_capacity,
    take_first,
    take_last,
    reverse,
};

/// One list operation's operands, which compiled code lays out in its frame.
pub const ListOpRequest = extern struct {
    op: ListOp,
    /// Bit 0: the first list operand is unique, bit 1: the second is.
    unique_args: u32,
    elem: *const TypeDesc,
    /// The result's bytes: a list, an element, a `U64`, or a `Bool`. For
    /// `replace_unsafe`, its list field.
    out: [*]u8,
    /// `replace_unsafe` only: its value field, when its result record has a
    /// static layout.
    out_value: ?[*]u8,
    /// `replace_unsafe` only: the descriptor of its result record when that
    /// record is dynamic; `out` is then the record's bytes.
    ret_desc: ?*const TypeDesc,
    list: ?*const RocList,
    /// `concat`'s second list, `append_sublist`'s source.
    list2: ?*const RocList,
    /// The element operand's bytes.
    element: ?[*]u8,
    /// The integer operands, in the operation's operand order.
    n: [3]u64,
};

/// A list of `len` zero-sized items and no spare capacity.
fn zstList(len: usize) RocList {
    return RocList.zeroSized(len, len);
}

fn zstSublistLen(size: usize, start: u64, len: u64) usize {
    if (size == 0 or len == 0 or start >= @as(u64, @intCast(size))) return 0;
    return @intCast(@min(len, @as(u64, @intCast(size - @as(usize, @intCast(start))))));
}

const ElemFacts = struct {
    alignment: u32,
    width: usize,
    rc: bool,
    context: ?*anyopaque,
    inc: list.Inc,
    dec: list.Dec,

    fn of(elem: *const TypeDesc) ElemFacts {
        const rc = elem.flags.contains_refcounted;
        return .{
            .alignment = if (elem.size == 0) 1 else elem.alignment,
            .width = elem.size,
            .rc = rc,
            .context = if (rc) @ptrCast(@constCast(elem)) else null,
            .inc = if (rc) &walk.elemIncref else &list.rcNone,
            .dec = if (rc) &walk.elemDecref else &list.rcNone,
        };
    }
};

fn mode(unique_args: u32, bit: u5) UpdateMode {
    return if ((unique_args >> bit) & 1 != 0) .InPlace else .Immutable;
}

fn writeList(out: [*]u8, value: RocList) void {
    @as(*align(1) RocList, @ptrCast(out)).* = value;
}

fn writeU64(out: [*]u8, value: u64) void {
    std.mem.writeInt(u64, out[0..8], value, .little);
}

/// Run the list operation `req` names over items its descriptor describes.
pub fn listOp(req: *const ListOpRequest, ops: *RocOps) void {
    const e = ElemFacts.of(req.elem);
    const a: RocList = if (req.list) |l| l.* else RocList.empty();
    const m0 = mode(req.unique_args, 0);
    switch (req.op) {
        .capacity => writeU64(req.out, @intCast(a.getCapacity())),
        .slack_unique => writeU64(req.out, list.listSlackUnique(a, ops)),
        .owned_unique => writeU64(req.out, list.listOwnedUnique(a, ops)),
        .get_unsafe, .map_extract_unsafe => {
            if (e.width == 0 or a.bytes == null) return;
            const at = a.bytes.? + @as(usize, @intCast(req.n[0])) * e.width;
            walk.copyBytes(req.out, at, e.width);
        },
        .append_unsafe => writeList(req.out, if (e.width == 0)
            a.zeroSizedKeeping(a.len() + 1)
        else
            list.listAppendUnsafe(a, req.element, e.width, &walk.elemCopy)),
        .concat => {
            const b = req.list2.?.*;
            writeList(req.out, if (e.width == 0)
                a.zeroSizedKeeping(a.len() + b.len())
            else
                list.listConcat(a, b, e.alignment, e.width, e.rc, e.context, e.inc, e.context, e.dec, m0, mode(req.unique_args, 1), ops));
        },
        .append_range_within => {
            const count = req.n[1];
            writeList(req.out, if (e.width == 0)
                a.zeroSizedKeeping(a.len() + @as(usize, @intCast(count)))
            else if (count == 0)
                a
            else
                list.listAppendRangeWithin(a, req.n[0], count, e.alignment, e.width, e.rc, e.context, e.inc, e.context, e.dec, m0, ops));
        },
        .copy_range_within => {
            const count = req.n[2];
            writeList(req.out, if (e.width == 0 or count == 0)
                a
            else
                list.listCopyRangeWithin(a, req.n[0], req.n[1], count, e.alignment, e.width, e.rc, e.context, e.inc, e.context, e.dec, ops));
        },
        .append_range_within_unsafe => {
            const count = req.n[1];
            writeList(req.out, if (e.width == 0)
                a.zeroSizedKeeping(a.len() + @as(usize, @intCast(count)))
            else if (count == 0)
                a
            else
                list.listAppendRangeWithinUnsafe(a, req.n[0], count, e.width, e.rc, e.context, e.inc, ops));
        },
        .append_sublist => {
            const count = req.n[1];
            writeList(req.out, if (e.width == 0)
                a.zeroSizedKeeping(a.len() + @as(usize, @intCast(count)))
            else
                list.listAppendSublist(a, req.list2.?.*, req.n[0], count, e.alignment, e.width, e.rc, e.context, e.inc, e.context, e.dec, m0, ops));
        },
        .prepend => writeList(req.out, if (e.width == 0)
            a.zeroSizedKeeping(a.len() + 1)
        else
            list.listPrepend(a, e.alignment, req.element, e.width, e.rc, e.context, e.inc, e.context, e.dec, m0, &walk.elemCopy, ops)),
        .swap => writeList(req.out, if (e.width == 0)
            a
        else
            list.listSwap(a, e.alignment, e.width, req.n[0], req.n[1], e.rc, e.context, e.inc, e.context, e.dec, m0, &walk.elemCopy, ops)),
        .map_can_reuse => req.out[0] = @intFromBool(list.listMapCanReuse(a, ops)),
        .item_size_at_most => req.out[0] = @intFromBool(e.width <= req.n[0]),
        .map_cast_unsafe => writeList(req.out, a),
        .map_write_unsafe => {
            if (e.width != 0) {
                const at = a.bytes.? + @as(usize, @intCast(req.n[0])) * e.width;
                walk.copyBytes(at, req.element.?, e.width);
            }
            writeList(req.out, a);
        },
        .clear => writeList(req.out, if (e.width == 0)
            a.zeroSizedKeeping(0)
        else
            list.listClear(a, e.alignment, e.width, e.rc, e.context, e.dec, m0, ops)),
        .sublist, .sublist_borrowed, .take_first, .take_last => {
            const start: u64, const len: u64 = switch (req.op) {
                .sublist, .sublist_borrowed => .{ req.n[0], req.n[1] },
                .take_first => .{ 0, req.n[0] },
                .take_last => blk: {
                    const size: u64 = @intCast(a.len());
                    const take = req.n[0];
                    break :blk .{ if (take >= size) 0 else size - take, take };
                },
                .clear, .capacity, .slack_unique, .owned_unique, .get_unsafe, .append_unsafe, .concat, .append_range_within, .copy_range_within, .append_range_within_unsafe, .append_sublist, .prepend, .swap, .map_can_reuse, .item_size_at_most, .map_cast_unsafe, .map_extract_unsafe, .map_write_unsafe, .drop_at, .replace_unsafe, .set, .set_in_place_unsafe, .with_capacity, .reserve, .reserve_for_append, .release_excess_capacity, .reverse => unreachable, // the enclosing prong admits only these five
            };
            if (e.width == 0) {
                writeList(req.out, zstList(zstSublistLen(a.len(), start, len)));
            } else if (req.op == .sublist_borrowed) {
                writeList(req.out, list.listSublistBorrowed(a, e.width, start, len, e.rc, ops));
            } else {
                writeList(req.out, list.listSublist(a, e.alignment, e.width, e.rc, start, len, e.context, e.dec, m0, ops));
            }
        },
        .drop_at => {
            if (e.width == 0) {
                const size = a.len();
                writeList(req.out, a.zeroSizedKeeping(if (req.n[0] >= @as(u64, @intCast(size))) size else size -| 1));
            } else {
                writeList(req.out, list.listDropAt(a, e.alignment, e.width, e.rc, req.n[0], e.context, e.inc, e.context, e.dec, m0, ops));
            }
        },
        .replace_unsafe => {
            if (e.width == 0) {
                const list_out = if (req.ret_desc) |ret_desc| req.out + recordOf(ret_desc, ops).fieldBySemanticIndex(0).offset else req.out;
                writeList(list_out, a);
                return;
            }
            // The result is the record `{ list : List(a), value : a }`.
            var list_out = req.out;
            var value_out: [*]u8 = undefined;
            if (req.ret_desc) |ret_desc| {
                const record = recordOf(ret_desc, ops);
                if (record.count != 2) invariant(ops, "a list_replace_unsafe result descriptor is not a two-field record");
                value_out = req.out + record.fieldBySemanticIndex(1).offset;
                list_out = req.out + record.fieldBySemanticIndex(0).offset;
            } else {
                value_out = req.out_value.?;
            }
            writeList(list_out, if (m0 == .InPlace)
                list.listReplaceInPlace(a, req.n[0], req.element, e.width, value_out, &walk.elemCopy)
            else
                list.listReplace(a, e.alignment, req.n[0], req.element, e.width, e.rc, e.context, e.inc, e.context, e.dec, value_out, &walk.elemCopy, ops));
        },
        .set, .set_in_place_unsafe => {
            if (e.width == 0) {
                writeList(req.out, a);
                return;
            }
            // `set` does not return the displaced element; `listSet` releases it.
            const set_mode: UpdateMode = if (req.op == .set_in_place_unsafe) .InPlace else m0;
            writeList(req.out, list.listSet(a, e.alignment, req.n[0], req.element, e.width, e.rc, e.context, e.inc, e.context, e.dec, set_mode, &walk.elemCopy, ops));
        },
        .with_capacity => writeList(req.out, if (e.width == 0)
            RocList.zeroSized(0, req.n[0])
        else
            list.listWithCapacity(req.n[0], e.alignment, e.width, e.rc, null, &list.rcNone, ops)),
        .reserve, .reserve_for_append => {
            if (e.width == 0) {
                writeList(req.out, a.zeroSizedReserving(req.n[0]));
                return;
            }
            const reserve = if (req.op == .reserve) &list.listReserve else &list.listReserveForAppend;
            writeList(req.out, reserve(a, e.alignment, req.n[0], e.width, e.rc, e.context, e.inc, e.context, e.dec, m0, ops));
        },
        .release_excess_capacity => writeList(req.out, if (e.width == 0)
            zstList(a.len())
        else
            list.listReleaseExcessCapacity(a, e.alignment, e.width, e.rc, e.context, e.inc, e.context, e.dec, m0, ops)),
        .reverse => writeList(req.out, if (e.width == 0)
            a
        else
            list.listReverse(a, e.alignment, e.width, e.rc, e.context, e.inc, e.context, e.dec, m0, &walk.elemCopy, ops)),
    }
}
