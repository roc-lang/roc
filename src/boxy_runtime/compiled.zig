//! The descriptor operations compiled code performs through the runtime
//! rather than inline: component addresses found by name or through a
//! nominal's backing, value construction, dictionaries, and the list
//! operations a `.boxy` generic body emits with an element descriptor
//! (design.md "Dynamic Values In LIR", "Dictionaries"). `exports.zig` exposes
//! each as a `roc_desc_*` C entry point.

const std = @import("std");
const builtins = @import("builtins");
const desc_mod = @import("desc.zig");
const compose = @import("compose.zig");
const walk = @import("walk.zig");

const RocOps = builtins.host_abi.RocOps;
const RocList = builtins.list.RocList;
const UpdateMode = builtins.utils.UpdateMode;
const list = builtins.list;
const utils = builtins.utils;
const erased_callable = builtins.erased_callable;

const TypeDesc = desc_mod.TypeDesc;
const Name = desc_mod.Name;
const Field = desc_mod.Field;
const Variant = desc_mod.Variant;

const invariant = compose.invariant;

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

fn variantNamed(desc: *const TypeDesc, name: *const Name, ops: *RocOps) struct { union_desc: *const TypeDesc, variant: *const Variant } {
    const union_desc = backing(desc);
    if (union_desc.kind != .tag_union) invariant(ops, "a tag of a descriptor that is not a tag union");
    for (union_desc.variants()) |*variant| {
        if (variant.name.id == name.id) return .{ .union_desc = union_desc, .variant = variant };
    }
    invariant(ops, "a tag that is not in its union's descriptor");
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
    @memcpy(slot[0..desc.size], src[0..desc.size]);
}

fn writeDiscriminant(desc: *const TypeDesc, bytes: [*]u8, discriminant: u32) void {
    const at = bytes + desc.discriminant_offset;
    switch (desc.discriminant_size) {
        0 => {},
        1 => at[0] = @intCast(discriminant),
        2 => std.mem.writeInt(u16, at[0..2], @intCast(discriminant), .little),
        4 => std.mem.writeInt(u32, at[0..4], discriminant, .little),
        8 => std.mem.writeInt(u64, at[0..8], discriminant, .little),
        else => unreachable, // composition and the compiler only produce 0, 1, 2, 4, or 8
    }
}

// Component addresses.

pub fn fieldAddr(desc: *const TypeDesc, base: [*]u8, semantic_index: u32, ops: *RocOps) [*]u8 {
    const record = recordOf(desc, ops);
    if (semantic_index >= record.count) invariant(ops, "a field index past its record's fields");
    const field = record.fieldBySemanticIndex(semantic_index);
    return componentAddr(field.desc, base + field.offset);
}

pub fn fieldAddrNamed(desc: *const TypeDesc, base: [*]u8, name: *const Name, ops: *RocOps) [*]u8 {
    const record = recordOf(desc, ops);
    for (record.fields()) |*field| {
        const field_name = field.name orelse continue;
        if (field_name.id == name.id) return componentAddr(field.desc, base + field.offset);
    }
    invariant(ops, "a field name that is not in its record's descriptor");
}

pub fn tagDiscriminant(desc: *const TypeDesc, base: [*]const u8, ops: *RocOps) u32 {
    const union_desc = backing(desc);
    if (union_desc.kind != .tag_union and union_desc.kind != .bool) invariant(ops, "a discriminant of a descriptor that is not a tag union");
    return union_desc.readDiscriminant(base);
}

pub fn tagDiscriminantOf(desc: *const TypeDesc, name: *const Name, ops: *RocOps) u32 {
    return variantNamed(desc, name, ops).variant.discriminant;
}

pub fn tagPayloadAddr(desc: *const TypeDesc, base: [*]u8, name: *const Name, payload_index: u32, ops: *RocOps) [*]u8 {
    const variant = variantNamed(desc, name, ops).variant;
    if (payload_index >= variant.payload_count) invariant(ops, "a tag argument index past its tag's arguments");
    return componentAddr(payloadDesc(variant, payload_index), base + payloadOffset(variant, payload_index));
}

pub fn listElemAddr(elem: *const TypeDesc, roc_list: *const RocList, index: u64) [*]u8 {
    if (elem.size == 0) return walk.zeroSizedPtr();
    return roc_list.bytes.? + @as(usize, @intCast(index)) * elem.size;
}

/// The payload of a box whose payload `desc` describes. A box of a
/// zero-sized payload holds no allocation.
pub fn boxPayloadAddr(desc: *const TypeDesc, box: *const usize) [*]u8 {
    const word = box.* & ~utils.DATA_PTR_TAG_MASK;
    if (desc.size == 0 or word == 0) return walk.zeroSizedPtr();
    return @ptrFromInt(word);
}

// Construction. Every value operand is the address of its bytes.

pub fn buildRecord(desc: *const TypeDesc, dst: [*]u8, fields: []const [*]const u8, ops: *RocOps) void {
    const record = recordOf(desc, ops);
    if (fields.len > record.count) invariant(ops, "a record built from more fields than its descriptor has");
    @memset(dst[0..record.size], 0);
    for (fields, 0..) |src, index| {
        const field = record.fieldBySemanticIndex(@intCast(index));
        writeComponent(field.desc, dst + field.offset, src, ops);
    }
}

pub fn buildTag(desc: *const TypeDesc, name: *const Name, dst: [*]u8, payloads: []const [*]const u8, ops: *RocOps) void {
    const found = variantNamed(desc, name, ops);
    if (payloads.len != found.variant.payload_count) invariant(ops, "a tag built with a different number of arguments than its descriptor has");
    @memset(dst[0..found.union_desc.size], 0);
    for (payloads, 0..) |src, index| {
        writeComponent(payloadDesc(found.variant, @intCast(index)), dst + payloadOffset(found.variant, @intCast(index)), src, ops);
    }
    writeDiscriminant(found.union_desc, dst, found.variant.discriminant);
}

pub fn buildList(elem: *const TypeDesc, dst: *RocList, elems: []const [*]const u8, ops: *RocOps) void {
    if (elems.len == 0 or elem.size == 0) {
        dst.* = zstList(elems.len);
        return;
    }
    const data = utils.allocateWithRefcount(elem.size * elems.len, elem.alignment, elem.flags.contains_refcounted, ops);
    for (elems, 0..) |src, index| {
        @memcpy((data + index * elem.size)[0..elem.size], src[0..elem.size]);
    }
    dst.* = .{ .bytes = data, .length = elems.len, .capacity_or_alloc_ptr = RocList.encodeCapacity(elems.len) };
}

pub fn buildBox(desc: *const TypeDesc, dst: *usize, payload: [*]const u8, ops: *RocOps) void {
    if (desc.size == 0) {
        dst.* = 0;
        return;
    }
    const data = utils.allocateWithRefcount(desc.size, desc.alignment, desc.flags.contains_refcounted, ops);
    @memcpy(data[0..desc.size], payload[0..desc.size]);
    dst.* = @intFromPtr(data);
}

// Representation adaptation.

/// One value of an adaptation: the source bytes `from` describes, rebuilt at
/// `dst` as `to` describes.
const AdaptWork = struct { from: *const TypeDesc, to: *const TypeDesc, src: [*]const u8, dst: [*]u8 };

/// Rebuild the value `src` (borrowed), which `from` describes, at `dst` as
/// the owned value `to` describes. The two descriptors describe one type
/// whose record fields may differ in storage (design.md "Field Kinds"): a
/// field `from` stores inline and `to` in a presence slot is wrapped
/// `#Present`, and a presence slot `to` stores inline is unwrapped, which a
/// `#Missing` slot cannot be. Lists and boxes whose contents change storage
/// are rebuilt; everything else is copied and retained.
pub fn adapt(from: *const TypeDesc, to: *const TypeDesc, dst: [*]u8, src: [*]const u8, ops: *RocOps) void {
    var stack: walk.Stack(AdaptWork, 16) = .{};
    defer stack.deinit(ops);
    stack.push(.{ .from = from, .to = to, .src = src, .dst = dst }, ops);
    while (stack.pop()) |work| {
        const f = work.from;
        const t = work.to;
        if (f == t) {
            @memcpy(work.dst[0..t.size], work.src[0..t.size]);
            walk.incref(t, work.dst, 1, ops);
            continue;
        }
        if (t.flags.presence_slot and !f.flags.presence_slot) {
            // Wrap: `#Present` is the slot's second variant.
            const present = &t.variants()[1];
            @memset(work.dst[0..t.size], 0);
            writeDiscriminant(t, work.dst, present.discriminant);
            stack.push(.{ .from = f, .to = present.payload, .src = work.src, .dst = work.dst }, ops);
            continue;
        }
        if (f.flags.presence_slot and !t.flags.presence_slot) {
            const present = &f.variants()[1];
            if (f.readDiscriminant(work.src) != present.discriminant) ops.crash("an optional record field that is missing reached a position that requires it");
            stack.push(.{ .from = present.payload, .to = t, .src = work.src, .dst = work.dst }, ops);
            continue;
        }
        // A nominal shares its backing's representation.
        if (f.kind == .nominal and t.kind != .nominal) {
            stack.push(.{ .from = f.child.?, .to = t, .src = work.src, .dst = work.dst }, ops);
            continue;
        }
        if (t.kind == .nominal and f.kind != .nominal) {
            stack.push(.{ .from = f, .to = t.child.?, .src = work.src, .dst = work.dst }, ops);
            continue;
        }
        if (f.kind != t.kind) invariant(ops, "adapted descriptors describe different kinds of value");
        switch (t.kind) {
            .zst, .scalar, .str, .bool, .erased_callable, .opaque_unknown => {
                @memcpy(work.dst[0..t.size], work.src[0..t.size]);
                walk.incref(t, work.dst, 1, ops);
            },
            .nominal => stack.push(.{ .from = f.child.?, .to = t.child.?, .src = work.src, .dst = work.dst }, ops),
            .record, .tuple => {
                @memset(work.dst[0..t.size], 0);
                for (t.fields()) |*to_field| {
                    if (to_field.is_padding != 0) continue;
                    // Fields match by name, tuple elements by position.
                    const from_field: ?*const Field = if (t.kind == .tuple) f.fieldBySemanticIndex(to_field.semantic_index) else for (f.fields()) |*candidate| {
                        if (candidate.name != null and candidate.name.?.id == to_field.name.?.id) break candidate;
                    } else null;
                    if (from_field) |source| {
                        stack.push(.{ .from = source.desc, .to = to_field.desc, .src = work.src + source.offset, .dst = work.dst + to_field.offset }, ops);
                    } else {
                        // An optional field the source lacks is missing.
                        if (!to_field.desc.flags.presence_slot) invariant(ops, "an adapted record lacks a field its destination requires");
                        writeDiscriminant(to_field.desc, work.dst + to_field.offset, to_field.desc.variants()[0].discriminant);
                    }
                }
            },
            .tag_union => {
                // The destination's variant is the source's by tag name; a
                // label a joined row holds at several payload types names
                // the variant whose payload describes the same values.
                const source = &f.variants()[f.readDiscriminant(work.src)];
                const target = targetVariant(t, source, ops);
                @memset(work.dst[0..t.size], 0);
                writeDiscriminant(t, work.dst, target.discriminant);
                if (source.payload_count != 0) stack.push(.{ .from = source.payload, .to = target.payload, .src = work.src, .dst = work.dst }, ops);
            },
            .list => {
                const source: *const RocList = @ptrCast(@alignCast(work.src));
                const target: *RocList = @ptrCast(@alignCast(work.dst));
                const from_elem = f.child.?;
                const to_elem = t.child.?;
                const len = source.len();
                if (len == 0 or to_elem.size == 0) {
                    target.* = zstList(len);
                    continue;
                }
                const data = utils.allocateWithRefcount(to_elem.size * len, to_elem.alignment, to_elem.flags.contains_refcounted, ops);
                target.* = .{ .bytes = data, .length = len, .capacity_or_alloc_ptr = RocList.encodeCapacity(len) };
                for (0..len) |index| {
                    stack.push(.{ .from = from_elem, .to = to_elem, .src = source.bytes.? + index * from_elem.size, .dst = data + index * to_elem.size }, ops);
                }
            },
            .box, .recursive_box => {
                const payload_to = t.child.?;
                const target: *usize = @ptrCast(@alignCast(work.dst));
                if (payload_to.size == 0) {
                    target.* = 0;
                    continue;
                }
                const word = @as(*const usize, @ptrCast(@alignCast(work.src))).* & ~utils.DATA_PTR_TAG_MASK;
                const data = utils.allocateWithRefcount(payload_to.size, payload_to.alignment, payload_to.flags.contains_refcounted, ops);
                target.* = @intFromPtr(data);
                stack.push(.{ .from = f.child.?, .to = payload_to, .src = @ptrFromInt(word), .dst = data }, ops);
            },
        }
    }
}

/// The variant of the tag union `to` a value of variant `source` is
/// re-tagged as: the one with its name and payload count, and among several,
/// the one whose payload has the source payload's shape.
fn targetVariant(to: *const TypeDesc, source: *const Variant, ops: *RocOps) *const Variant {
    var found: ?*const Variant = null;
    for (to.variants()) |*candidate| {
        if (candidate.name.id != source.name.id or candidate.payload_count != source.payload_count) continue;
        if (found != null) {
            if (!walk.sameShape(candidate.payload, source.payload, ops)) continue;
            if (walk.sameShape(found.?.payload, source.payload, ops)) invariant(ops, "a re-tagged variant matches two destination variants");
        }
        found = candidate;
    }
    return found orelse invariant(ops, "a re-tagged variant is not in its destination's row");
}

// Dictionaries.

/// Dictionary callables and dictionary records, interned by constructor and
/// operand words. Both are immortal.
var dictionary_cache: compose.Cache = .{};

const dict_compose_tag: u64 = 1;
const dict_record_tag: u64 = 2;

/// Bytes before a static callable's payload: alignment padding, then the
/// refcount word immediately before the payload.
pub const static_callable_header: usize = erased_callable.capture_alignment;

/// An immortal erased callable whose function is `callable_fn` (a worker's
/// erased-record entry) and whose capture bytes are `capture_prefix` zero
/// bytes followed by `captures`, one word each, interned by function,
/// prefix, and captures.
pub fn dictCompose(callable_fn: erased_callable.CallableFnPtr, capture_prefix: u32, captures: []const usize, immortal_ops: *RocOps) [*]u8 {
    var key_buffer: [16]u64 = undefined;
    var scratch: ?[*]u64 = null;
    const key_len = 3 + captures.len;
    const key: []u64 = if (key_len <= key_buffer.len) key_buffer[0..key_len] else blk: {
        const words: [*]u64 = @ptrCast(@alignCast(immortal_ops.allocRaw(key_len * @sizeOf(u64), @alignOf(u64))));
        scratch = words;
        break :blk words[0..key_len];
    };
    defer if (scratch) |words| immortal_ops.dealloc(@ptrCast(words), @alignOf(u64));
    key[0] = dict_compose_tag;
    key[1] = @intFromPtr(callable_fn);
    key[2] = capture_prefix;
    for (captures, key[3..]) |capture, *word| word.* = capture;
    if (dictionary_cache.lookup(key)) |existing| return @ptrCast(@constCast(existing));

    const capture_size = capture_prefix + captures.len * @sizeOf(usize);
    const payload_size = erased_callable.payloadSize(capture_size);
    const bytes: [*]u8 = compose.immortalBytes(immortal_ops, static_callable_header + payload_size, erased_callable.payload_alignment);
    const data = bytes + static_callable_header;
    @as(*isize, @ptrCast(@alignCast(data - @sizeOf(isize)))).* = utils.REFCOUNT_STATIC_DATA;
    erased_callable.payloadPtr(data).* = .{ .callable_fn_ptr = callable_fn, .on_drop = null };
    const capture_bytes = data + erased_callable.capture_offset;
    @memset(capture_bytes[0..capture_prefix], 0);
    const words: [*]align(1) usize = @ptrCast(capture_bytes + capture_prefix);
    for (captures, 0..) |capture, index| words[index] = capture;
    return @ptrCast(@constCast(dictionary_cache.intern(key, @ptrCast(@alignCast(data)), immortal_ops)));
}

/// An immortal record of method callables, interned by its callables.
pub fn dictRecord(methods: []const usize, immortal_ops: *RocOps) [*]const usize {
    var key_buffer: [16]u64 = undefined;
    var scratch: ?[*]u64 = null;
    const key_len = 1 + methods.len;
    const key: []u64 = if (key_len <= key_buffer.len) key_buffer[0..key_len] else blk: {
        const words: [*]u64 = @ptrCast(@alignCast(immortal_ops.allocRaw(key_len * @sizeOf(u64), @alignOf(u64))));
        scratch = words;
        break :blk words[0..key_len];
    };
    defer if (scratch) |words| immortal_ops.dealloc(@ptrCast(words), @alignOf(u64));
    key[0] = dict_record_tag;
    for (methods, key[1..]) |method, *word| word.* = method;
    if (dictionary_cache.lookup(key)) |existing| return @ptrCast(@alignCast(existing));

    const record: [*]usize = @ptrCast(@alignCast(compose.immortalBytes(immortal_ops, @max(methods.len, 1) * @sizeOf(usize), @alignOf(usize))));
    @memcpy(record[0..methods.len], methods);
    return @ptrCast(@alignCast(dictionary_cache.intern(key, @ptrCast(@alignCast(record)), immortal_ops)));
}

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

fn zstList(len: usize) RocList {
    return .{ .bytes = null, .length = len, .capacity_or_alloc_ptr = 0 };
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
            @memcpy(req.out[0..e.width], at[0..e.width]);
        },
        .append_unsafe => writeList(req.out, if (e.width == 0)
            zstList(a.len() + 1)
        else
            list.listAppendUnsafe(a, req.element, e.width, &list.copy_fallback)),
        .concat => {
            const b = req.list2.?.*;
            writeList(req.out, if (e.width == 0)
                zstList(a.len() + b.len())
            else
                list.listConcat(a, b, e.alignment, e.width, e.rc, e.context, e.inc, e.context, e.dec, m0, mode(req.unique_args, 1), ops));
        },
        .append_range_within => {
            const count = req.n[1];
            writeList(req.out, if (e.width == 0)
                zstList(a.len() + @as(usize, @intCast(count)))
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
                zstList(a.len() + @as(usize, @intCast(count)))
            else if (count == 0)
                a
            else
                list.listAppendRangeWithinUnsafe(a, req.n[0], count, e.width, e.rc, e.context, e.inc, ops));
        },
        .append_sublist => {
            const count = req.n[1];
            writeList(req.out, if (e.width == 0)
                zstList(a.len() + @as(usize, @intCast(count)))
            else
                list.listAppendSublist(a, req.list2.?.*, req.n[0], count, e.alignment, e.width, e.rc, e.context, e.inc, e.context, e.dec, m0, ops));
        },
        .prepend => writeList(req.out, if (e.width == 0)
            zstList(a.len() + 1)
        else
            list.listPrepend(a, e.alignment, req.element, e.width, e.rc, e.context, e.inc, e.context, e.dec, m0, &list.copy_fallback, ops)),
        .swap => writeList(req.out, if (e.width == 0)
            zstList(a.len())
        else
            list.listSwap(a, e.alignment, e.width, req.n[0], req.n[1], e.rc, e.context, e.inc, e.context, e.dec, m0, &list.copy_fallback, ops)),
        .map_can_reuse => req.out[0] = @intFromBool(list.listMapCanReuse(a, ops)),
        .item_size_at_most => req.out[0] = @intFromBool(e.width <= req.n[0]),
        .map_cast_unsafe => writeList(req.out, a),
        .map_write_unsafe => {
            if (e.width != 0) {
                const at = a.bytes.? + @as(usize, @intCast(req.n[0])) * e.width;
                @memcpy(at[0..e.width], req.element.?[0..e.width]);
            }
            writeList(req.out, a);
        },
        .sublist, .sublist_borrowed, .take_first, .take_last, .clear => {
            const start: u64, const len: u64 = switch (req.op) {
                .sublist, .sublist_borrowed => .{ req.n[0], req.n[1] },
                .take_first => .{ 0, req.n[0] },
                .take_last => blk: {
                    const size: u64 = @intCast(a.len());
                    const take = req.n[0];
                    break :blk .{ if (take >= size) 0 else size - take, take };
                },
                .clear => .{ 0, 0 },
                else => unreachable, // the enclosing prong admits only these five
            };
            if (e.width == 0) {
                writeList(req.out, zstList(if (req.op == .clear) 0 else zstSublistLen(a.len(), start, len)));
            } else if (req.op == .sublist_borrowed) {
                writeList(req.out, list.listSublistBorrowed(a, e.width, start, len, e.rc, ops));
            } else {
                writeList(req.out, list.listSublist(a, e.alignment, e.width, e.rc, start, len, e.context, e.dec, m0, ops));
            }
        },
        .drop_at => {
            if (e.width == 0) {
                const size = a.len();
                writeList(req.out, zstList(if (req.n[0] >= @as(u64, @intCast(size))) size else size -| 1));
            } else {
                writeList(req.out, list.listDropAt(a, e.alignment, e.width, e.rc, req.n[0], e.context, e.inc, e.context, e.dec, m0, ops));
            }
        },
        .replace_unsafe => {
            if (e.width == 0) {
                const list_out = if (req.ret_desc) |ret_desc| req.out + recordOf(ret_desc, ops).fieldBySemanticIndex(0).offset else req.out;
                writeList(list_out, zstList(a.len()));
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
                list.listReplaceInPlace(a, req.n[0], req.element, e.width, value_out, &list.copy_fallback)
            else
                list.listReplace(a, e.alignment, req.n[0], req.element, e.width, e.rc, e.context, e.inc, e.context, e.dec, value_out, &list.copy_fallback, ops));
        },
        .set, .set_in_place_unsafe => {
            if (e.width == 0) {
                writeList(req.out, zstList(a.len()));
                return;
            }
            // The displaced element is released here: `set` does not return it.
            const old: [*]u8 = @ptrCast(ops.allocRaw(e.width, e.alignment));
            defer ops.dealloc(@ptrCast(old), e.alignment);
            const result = if (req.op == .set_in_place_unsafe or m0 == .InPlace)
                list.listReplaceInPlace(a, req.n[0], req.element, e.width, old, &list.copy_fallback)
            else
                list.listReplace(a, e.alignment, req.n[0], req.element, e.width, e.rc, e.context, e.inc, e.context, e.dec, old, &list.copy_fallback, ops);
            if (e.rc) walk.decref(req.elem, old, ops);
            writeList(req.out, result);
        },
        .with_capacity => writeList(req.out, if (e.width == 0)
            zstList(0)
        else
            list.listWithCapacity(req.n[0], e.alignment, e.width, e.rc, null, &list.rcNone, ops)),
        .reserve, .reserve_for_append => {
            if (e.width == 0) {
                writeList(req.out, zstList(a.len()));
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
            zstList(a.len())
        else
            list.listReverse(a, e.alignment, e.width, e.rc, e.context, e.inc, e.context, e.dec, m0, &list.copy_fallback, ops)),
    }
}

// Immortal memory for the compiler's own process.

/// Where composed descriptors and dictionaries are allocated. They are
/// immortal. A platform's runtime allocates them through its host, whose
/// allocations live as long as the process. In the compiler's process,
/// evaluations come and go with their own allocators while the composition
/// caches are process-global, so composition allocates from process-lifetime
/// memory and delivers a crash to the evaluation running on its thread.
pub fn immortalOps() *RocOps {
    if (comptime builtins.host_abi.host_role == .platform) return builtins.in_process_host.ops();
    return &InProcessImmortals.ops;
}

const InProcessImmortals = struct {
    var lock: std.atomic.Mutex = .unlocked;
    var arena: std.heap.ArenaAllocator = .init(std.heap.page_allocator);
    var ops: RocOps = .{
        .env = undefined,
        .roc_alloc = &alloc,
        .roc_dealloc = &dealloc,
        .roc_realloc = &realloc,
        .roc_dbg = &dbg,
        .roc_expect_failed = &expectFailed,
        .roc_crashed = &crashed,
        .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
    };

    fn alloc(_: *RocOps, length: usize, alignment: usize) callconv(.c) *anyopaque {
        while (!lock.tryLock()) std.atomic.spinLoopHint();
        defer lock.unlock();
        return arena.allocator().rawAlloc(length, .fromByteUnits(alignment), @returnAddress()) orelse
            builtins.in_process_host.ops().crash("out of memory composing a Boxy descriptor");
    }

    /// Process-lifetime memory is never released.
    fn dealloc(_: *RocOps, _: *anyopaque, _: usize) callconv(.c) void {}

    fn realloc(_: *RocOps, _: *anyopaque, _: usize, _: usize) callconv(.c) *anyopaque {
        invariant(builtins.in_process_host.ops(), "descriptor composition reallocated immortal memory");
    }

    fn dbg(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
        builtins.in_process_host.ops().dbg(bytes[0..len]);
    }

    fn expectFailed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
        builtins.in_process_host.ops().expectFailed(bytes[0..len]);
    }

    fn crashed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
        builtins.in_process_host.ops().crash(bytes[0..len]);
    }
};
