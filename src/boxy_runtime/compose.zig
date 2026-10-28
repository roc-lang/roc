//! Descriptor composition and its immortal interning cache.
//!
//! A generic body composes the descriptor of a type that mentions one of its
//! type variables from static descriptors and the descriptors it received
//! (design.md "Type Descriptors"). Composition lays the type out with the
//! layout store's own shape code (`layout_shape`), so a composed descriptor
//! has byte-for-byte the shape the store commits for the same closed type.
//!
//! Every constructor interns its result keyed by the constructor and the
//! identities of its operands (descriptor addresses and name ids): composing
//! again with the same operands returns the same pointer and allocates
//! nothing. Descriptors and the cache live in memory allocated through
//! `RocOps` and are never freed.

const std = @import("std");
const builtins = @import("builtins");
const shape = @import("layout_shape");
const desc_mod = @import("desc.zig");

const field_order = shape.field_order;
const SortKey = shape.SortKey;
const RocOps = builtins.host_abi.RocOps;
const RocStr = builtins.str.RocStr;
const RocList = builtins.list.RocList;

const TypeDesc = desc_mod.TypeDesc;
const Field = desc_mod.Field;
const Variant = desc_mod.Variant;
const Name = desc_mod.Name;
const Methods = desc_mod.Methods;
const Kind = desc_mod.Kind;
const Scalar = desc_mod.Scalar;
const Flags = desc_mod.Flags;

const pointer_size: u32 = @sizeOf(usize);

// Leaf descriptors.

fn leaf(kind: Kind, scalar: Scalar, size: u32, alignment: u32, sort_key: SortKey, contains_refcounted: bool) TypeDesc {
    return .{
        .size = size,
        .alignment = alignment,
        .kind = kind,
        .scalar = scalar,
        .sort_key = @backingInt(sort_key),
        .flags = .{ .contains_refcounted = contains_refcounted },
        .count = 0,
        .discriminant_offset = 0,
        .discriminant_size = 0,
        .child = null,
        .entries = null,
        .name = null,
        .methods = null,
    };
}

const zst_leaf: TypeDesc = leaf(.zst, .u8, 0, 1, .align_1, false);
const str_leaf: TypeDesc = leaf(.str, .u8, RocStr.word_count * pointer_size, pointer_size, .pointer, true);
const erased_callable_leaf: TypeDesc = leaf(.erased_callable, .u8, pointer_size, pointer_size, .pointer, true);
const opaque_unknown_leaf: TypeDesc = leaf(.opaque_unknown, .u8, pointer_size, pointer_size, .pointer, true);
const bool_leaf: TypeDesc = blk: {
    var bool_desc = leaf(.bool, .u8, 1, 1, .align_1, false);
    bool_desc.count = 2;
    bool_desc.discriminant_size = 1;
    break :blk bool_desc;
};

fn scalarSize(scalar: Scalar) u32 {
    return switch (scalar) {
        .u8, .i8 => 1,
        .u16, .i16 => 2,
        .u32, .i32, .f32 => 4,
        .u64, .i64, .f64 => 8,
        .u128, .i128, .dec => 16,
        .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => 16,
        .opaque_ptr => pointer_size,
    };
}

const scalar_leaves = blk: {
    const scalars = @typeInfo(Scalar).@"enum".field_names;
    var leaves: [scalars.len]TypeDesc = undefined;
    for (&leaves, 0..) |*slot, index| {
        const scalar: Scalar = @fromBackingInt(index);
        const size = scalarSize(scalar);
        const sort_key: SortKey = if (scalar == .opaque_ptr) .pointer else SortKey.fromAlignBytes(size);
        slot.* = leaf(.scalar, scalar, size, size, sort_key, false);
    }
    break :blk leaves;
};

/// The zero-sized descriptor: the empty record, the empty tuple, a
/// payload-free variant's payload.
pub fn zstDesc() *const TypeDesc {
    return &zst_leaf;
}

pub fn strDesc() *const TypeDesc {
    return &str_leaf;
}

pub fn boolDesc() *const TypeDesc {
    return &bool_leaf;
}

pub fn erasedCallableDesc() *const TypeDesc {
    return &erased_callable_leaf;
}

pub fn opaqueUnknownDesc() *const TypeDesc {
    return &opaque_unknown_leaf;
}

/// A scalar's descriptor. A SIMD vector's carries no `to_inspect`; the
/// compiler emits vector descriptors with theirs.
pub fn scalarDesc(scalar: Scalar) *const TypeDesc {
    return &scalar_leaves[@backingInt(scalar)];
}

// Constructor operands. These are `extern` so the C entry points take arrays
// of them directly.

/// One record field: its label and its value's descriptor.
pub const FieldInput = extern struct {
    name: *const Name,
    desc: *const TypeDesc,
};

/// One field of a declared-order record. A padding field has no name.
pub const DeclaredFieldInput = extern struct {
    name: ?*const Name,
    desc: *const TypeDesc,
    /// 0 or 1.
    is_padding: u32,
};

/// One tag: its name and its arguments' descriptors.
pub const VariantInput = extern struct {
    name: *const Name,
    payload_count: u32,
    payloads: [*]const *const TypeDesc,

    fn payloadSlice(self: VariantInput) []const *const TypeDesc {
        return self.payloads[0..self.payload_count];
    }
};

/// The process-global composition cache, through which the C entry points
/// compose.
pub var process_cache: Cache = .{};

/// An immortal interning table from constructor keys to descriptors.
pub const Cache = struct {
    lock: std.atomic.Value(u32) = .init(0),
    entries: ?[*]Entry = null,
    /// A power of two, or 0 before the first insertion.
    capacity: u32 = 0,
    len: u32 = 0,

    const Entry = struct {
        hash: u64,
        key: [*]const u64,
        key_len: u32,
        value: ?*const TypeDesc,
    };

    fn acquire(self: *Cache) void {
        while (self.lock.cmpxchgWeak(0, 1, .acquire, .monotonic) != null) {
            std.atomic.spinLoopHint();
        }
    }

    fn release(self: *Cache) void {
        self.lock.store(0, .release);
    }

    fn find(self: *const Cache, hash: u64, key: []const u64) ?*const TypeDesc {
        const entries = self.entries orelse return null;
        const mask = self.capacity - 1;
        var index: u32 = @truncate(hash & mask);
        while (true) : (index = (index + 1) & mask) {
            const entry = &entries[index];
            const value = entry.value orelse return null;
            if (entry.hash == hash and entry.key_len == key.len and keysEqual(entry.key[0..entry.key_len], key)) return value;
        }
    }

    pub fn lookup(self: *Cache, key: []const u64) ?*const TypeDesc {
        const hash = keyHash(key);
        self.acquire();
        defer self.release();
        return self.find(hash, key);
    }

    /// Intern `value` under `key`, or return the descriptor another thread
    /// interned under it first.
    pub fn intern(self: *Cache, key: []const u64, value: *const TypeDesc, ops: *RocOps) *const TypeDesc {
        const hash = keyHash(key);
        self.acquire();
        defer self.release();
        if (self.find(hash, key)) |existing| return existing;
        if ((self.len + 1) * 4 > self.capacity * 3) self.grow(ops);
        const stored = immortal(u64, key.len, ops);
        @memcpy(stored[0..key.len], key);
        self.place(.{ .hash = hash, .key = stored, .key_len = @intCast(key.len), .value = value });
        self.len += 1;
        return value;
    }

    fn place(self: *Cache, entry: Entry) void {
        const entries = self.entries.?;
        const mask = self.capacity - 1;
        var index: u32 = @truncate(entry.hash & mask);
        while (entries[index].value != null) index = (index + 1) & mask;
        entries[index] = entry;
    }

    fn grow(self: *Cache, ops: *RocOps) void {
        const old_entries = self.entries;
        const old_capacity = self.capacity;
        const new_capacity: u32 = if (old_capacity == 0) 64 else old_capacity * 2;
        const bytes = @as(usize, new_capacity) * @sizeOf(Entry);
        const fresh: [*]Entry = @ptrCast(@alignCast(ops.allocRaw(bytes, @alignOf(Entry))));
        for (fresh[0..new_capacity]) |*entry| entry.value = null;
        self.entries = fresh;
        self.capacity = new_capacity;
        if (old_entries) |old| {
            for (old[0..old_capacity]) |entry| {
                if (entry.value != null) self.place(entry);
            }
            ops.dealloc(@ptrCast(old), @alignOf(Entry));
        }
    }
};

fn keysEqual(a: []const u64, b: []const u64) bool {
    for (a, b) |x, y| {
        if (x != y) return false;
    }
    return true;
}

fn keyHash(key: []const u64) u64 {
    return std.hash.Wyhash.hash(0x5bd1_e995_9e37_79b9, std.mem.sliceAsBytes(key));
}

fn immortal(comptime T: type, count: usize, ops: *RocOps) [*]T {
    return @ptrCast(@alignCast(ops.allocRaw(@max(count, 1) * @sizeOf(T), @alignOf(T))));
}

/// Temporary storage for `count` items: inline when small, otherwise
/// allocated through `RocOps` and released by `deinit`.
fn Scratch(comptime T: type, comptime inline_count: usize) type {
    return struct {
        inline_items: [inline_count]T = undefined,
        heap: ?[*]T = null,

        fn get(self: *@This(), count: usize, ops: *RocOps) []T {
            if (count <= inline_count) return self.inline_items[0..count];
            const items: [*]T = @ptrCast(@alignCast(ops.allocRaw(count * @sizeOf(T), @alignOf(T))));
            self.heap = items;
            return items[0..count];
        }

        fn deinit(self: *@This(), ops: *RocOps) void {
            if (self.heap) |items| ops.dealloc(@ptrCast(items), @alignOf(T));
        }
    };
}

const KeyTag = enum(u64) {
    list = 1,
    box,
    recursive_box,
    record,
    declared_record,
    tuple,
    tag_union,
    args,
    nominal,
    presence_slot,
};

const no_name: u64 = std.math.maxInt(u64);

fn ptrWord(ptr: anytype) u64 {
    return @intFromPtr(ptr);
}

fn nameWord(name: ?*const Name) u64 {
    return if (name) |n| n.id else no_name;
}

fn nameOrder(a: *const Name, b: *const Name) std.math.Order {
    if (a.id == b.id) return .eq;
    return std.mem.order(u8, a.text(), b.text());
}

fn sortKeyOf(d: *const TypeDesc) SortKey {
    return @fromBackingInt(@as(u3, @intCast(d.sort_key)));
}

// Constructors.

/// `List(elem)`. A list of a zero-sized element is still three words.
pub fn list(cache: *Cache, ops: *RocOps, elem: *const TypeDesc) *const TypeDesc {
    return container(cache, ops, .list, elem, true);
}

/// `Box(payload)`. A box of a zero-sized payload is one word and holds no
/// allocation.
pub fn box(cache: *Cache, ops: *RocOps, payload: *const TypeDesc) *const TypeDesc {
    return container(cache, ops, .box, payload, payload.size != 0);
}

/// The implicit box a layout inserts to break a recursive type.
pub fn recursiveBox(cache: *Cache, ops: *RocOps, payload: *const TypeDesc) *const TypeDesc {
    return container(cache, ops, .recursive_box, payload, payload.size != 0);
}

fn container(cache: *Cache, ops: *RocOps, comptime kind: Kind, child: *const TypeDesc, contains_refcounted: bool) *const TypeDesc {
    const tag: KeyTag = switch (kind) {
        .list => .list,
        .box => .box,
        .recursive_box => .recursive_box,
        else => @compileError("not a container kind"),
    };
    const key = [_]u64{ @backingInt(tag), ptrWord(child) };
    if (cache.lookup(&key)) |existing| return existing;
    const size: u32 = if (kind == .list) RocList.word_count * pointer_size else pointer_size;
    const built = immortal(TypeDesc, 1, ops);
    built[0] = leaf(kind, .u8, size, pointer_size, .pointer, contains_refcounted);
    built[0].child = child;
    return cache.intern(&key, &built[0], ops);
}

/// A structural record. Fields may come in any order: a field's semantic
/// index is its label's rank in byte-wise text order, and fields are laid
/// out by descending sort key, then ascending semantic index.
pub fn record(cache: *Cache, ops: *RocOps, fields: []const FieldInput) *const TypeDesc {
    var key_scratch: Scratch(u64, 64) = .{};
    defer key_scratch.deinit(ops);
    const key = key_scratch.get(2 + 2 * fields.len, ops);
    key[0] = @backingInt(KeyTag.record);
    key[1] = fields.len;
    for (fields, 0..) |field, index| {
        key[2 + 2 * index] = field.name.id;
        key[3 + 2 * index] = ptrWord(field.desc);
    }
    if (cache.lookup(key)) |existing| return existing;

    var rank_scratch: Scratch(u32, 32) = .{};
    defer rank_scratch.deinit(ops);
    const by_name = rank_scratch.get(fields.len, ops);
    sortByName(FieldInput, fields, by_name, ops);

    var member_scratch: Scratch(Member, 32) = .{};
    defer member_scratch.deinit(ops);
    const members = member_scratch.get(fields.len, ops);
    for (by_name, 0..) |input_index, rank| {
        const field = fields[input_index];
        members[input_index] = .{ .name = field.name, .desc = field.desc, .semantic_index = @intCast(rank), .is_padding = false };
    }
    const built = buildStruct(ops, .record, .structural, members);
    return cache.intern(key, built, ops);
}

/// A record of `fields` together with the fields of the record `ext`
/// describes: an open row's explicit fields and its remainder's.
pub fn recordExtended(cache: *Cache, ops: *RocOps, fields: []const FieldInput, ext: *const TypeDesc) *const TypeDesc {
    const ext_fields: []const desc_mod.Field = switch (ext.kind) {
        .record => ext.fields(),
        .zst => &.{},
        else => invariant(ops, "a record row's remainder descriptor is not a record"),
    };
    var scratch: Scratch(FieldInput, 32) = .{};
    defer scratch.deinit(ops);
    const all = scratch.get(fields.len + ext_fields.len, ops);
    @memcpy(all[0..fields.len], fields);
    var count = fields.len;
    for (ext_fields) |field| {
        if (field.is_padding != 0) continue;
        all[count] = .{ .name = field.name orelse invariant(ops, "a record row's remainder has an unnamed field"), .desc = field.desc };
        count += 1;
    }
    return record(cache, ops, all[0..count]);
}

/// A tag union of `variants` together with the variants of the tag union
/// `ext` describes: an open row's explicit tags and its remainder's.
pub fn tagUnionExtended(cache: *Cache, ops: *RocOps, variants: []const VariantInput, ext: *const TypeDesc) *const TypeDesc {
    const ext_variants: []const desc_mod.Variant = switch (ext.kind) {
        .tag_union => ext.variants(),
        .zst => &.{},
        else => invariant(ops, "a tag row's remainder descriptor is not a tag union"),
    };
    var scratch: Scratch(VariantInput, 32) = .{};
    defer scratch.deinit(ops);
    const all = scratch.get(variants.len + ext_variants.len, ops);
    @memcpy(all[0..variants.len], variants);
    var payload_count: usize = 0;
    for (ext_variants) |variant| payload_count += variant.payload_count;
    var payload_scratch: Scratch(*const TypeDesc, 32) = .{};
    defer payload_scratch.deinit(ops);
    const payloads = payload_scratch.get(payload_count, ops);
    var next: usize = 0;
    var count: usize = variants.len;
    for (ext_variants) |variant| {
        // A row's remainder can carry a tag the row names: the rows are one
        // union, so it is that tag.
        const named = for (variants) |known| {
            if (known.name.id == variant.name.id) break known;
        } else null;
        if (named) |known| {
            if (known.payload_count != variant.payload_count) invariant(ops, "a tag row and its remainder give one tag different arities");
            continue;
        }
        const input = &all[count];
        count += 1;
        const start = next;
        switch (variant.payload_count) {
            0 => {},
            1 => {
                payloads[next] = variant.payload;
                next += 1;
            },
            else => {
                var index: u32 = 0;
                while (index < variant.payload_count) : (index += 1) {
                    payloads[next] = variant.payload.fieldBySemanticIndex(index).desc;
                    next += 1;
                }
            },
        }
        input.* = .{ .name = variant.name, .payload_count = variant.payload_count, .payloads = payloads.ptr + start };
    }
    return tagUnion(cache, ops, all[0..count]);
}

/// A nominal record laid out in declared order with C-style padding, as a
/// record with `_` fields is. `fields` is in declared order; a named field's
/// semantic index is its label's rank among the named fields, and padding
/// fields rank after them in declared order.
pub fn declaredRecord(cache: *Cache, ops: *RocOps, fields: []const DeclaredFieldInput) *const TypeDesc {
    var key_scratch: Scratch(u64, 64) = .{};
    defer key_scratch.deinit(ops);
    const key = key_scratch.get(2 + 3 * fields.len, ops);
    key[0] = @backingInt(KeyTag.declared_record);
    key[1] = fields.len;
    for (fields, 0..) |field, index| {
        key[2 + 3 * index] = nameWord(field.name);
        key[3 + 3 * index] = ptrWord(field.desc);
        key[4 + 3 * index] = field.is_padding;
    }
    if (cache.lookup(key)) |existing| return existing;

    var named_count: u32 = 0;
    for (fields) |field| {
        if (field.is_padding != 0) continue;
        if (field.name == null) invariant(ops, "a declared-order record's named field has no name");
        named_count += 1;
    }

    var named_scratch: Scratch(FieldInput, 32) = .{};
    defer named_scratch.deinit(ops);
    const named = named_scratch.get(named_count, ops);
    var named_positions_scratch: Scratch(u32, 32) = .{};
    defer named_positions_scratch.deinit(ops);
    const named_positions = named_positions_scratch.get(named_count, ops);
    {
        var next: u32 = 0;
        for (fields, 0..) |field, index| {
            if (field.is_padding != 0) continue;
            named[next] = .{ .name = field.name.?, .desc = field.desc };
            named_positions[next] = @intCast(index);
            next += 1;
        }
    }
    var rank_scratch: Scratch(u32, 32) = .{};
    defer rank_scratch.deinit(ops);
    const by_name = rank_scratch.get(named_count, ops);
    sortByName(FieldInput, named, by_name, ops);

    var member_scratch: Scratch(Member, 32) = .{};
    defer member_scratch.deinit(ops);
    const members = member_scratch.get(fields.len, ops);
    for (by_name, 0..) |named_index, rank| {
        const position = named_positions[named_index];
        members[position] = .{ .name = fields[position].name, .desc = fields[position].desc, .semantic_index = @intCast(rank), .is_padding = false };
    }
    var padding_rank = named_count;
    for (fields, 0..) |field, position| {
        if (field.is_padding == 0) continue;
        members[position] = .{ .name = null, .desc = field.desc, .semantic_index = padding_rank, .is_padding = true };
        padding_rank += 1;
    }
    const built = buildStruct(ops, .record, .declared, members);
    return cache.intern(key, built, ops);
}

/// A tuple: element `i` has semantic index `i`; elements are laid out
/// structurally.
pub fn tuple(cache: *Cache, ops: *RocOps, elems: []const *const TypeDesc) *const TypeDesc {
    return positional(cache, ops, .tuple, elems);
}

/// The packed argument record of an erased call: the arguments in declared
/// order, each at its natural alignment (`layout.erased_call_abi.plan`).
pub fn args(cache: *Cache, ops: *RocOps, descs: []const *const TypeDesc) *const TypeDesc {
    return positional(cache, ops, .args, descs);
}

fn positional(cache: *Cache, ops: *RocOps, comptime tag: KeyTag, descs: []const *const TypeDesc) *const TypeDesc {
    var key_scratch: Scratch(u64, 64) = .{};
    defer key_scratch.deinit(ops);
    const key = key_scratch.get(2 + descs.len, ops);
    key[0] = @backingInt(tag);
    key[1] = descs.len;
    for (descs, 0..) |d, index| key[2 + index] = ptrWord(d);
    if (cache.lookup(key)) |existing| return existing;

    var member_scratch: Scratch(Member, 32) = .{};
    defer member_scratch.deinit(ops);
    const members = member_scratch.get(descs.len, ops);
    for (descs, 0..) |d, index| members[index] = .{ .name = null, .desc = d, .semantic_index = @intCast(index), .is_padding = false };
    const order: Order = if (tag == .args) .declared else .structural;
    const built = buildStruct(ops, .tuple, order, members);
    return cache.intern(key, built, ops);
}

/// An optional record field's presence slot over `payload`: the tag union
/// `[missing, present(payload)]`, marked as a presence slot.
pub fn presenceSlot(cache: *Cache, ops: *RocOps, missing: *const Name, present: *const Name, payload: *const TypeDesc) *const TypeDesc {
    const key = [_]u64{ @backingInt(KeyTag.presence_slot), missing.id, present.id, ptrWord(payload) };
    if (cache.lookup(&key)) |existing| return existing;
    const payloads = [_]*const TypeDesc{payload};
    const variants = [_]VariantInput{
        .{ .name = missing, .payload_count = 0, .payloads = &payloads },
        .{ .name = present, .payload_count = 1, .payloads = &payloads },
    };
    const union_desc = tagUnion(cache, ops, &variants);
    const built = immortal(TypeDesc, 1, ops);
    built[0] = union_desc.*;
    built[0].flags.presence_slot = true;
    return cache.intern(&key, &built[0], ops);
}

/// A closed tag union. Tags may come in any order: a tag's discriminant is
/// its name's rank in byte-wise text order.
pub fn tagUnion(cache: *Cache, ops: *RocOps, variants: []const VariantInput) *const TypeDesc {
    var key_len: usize = 2;
    for (variants) |variant| key_len += 2 + variant.payload_count;
    var key_scratch: Scratch(u64, 64) = .{};
    defer key_scratch.deinit(ops);
    const key = key_scratch.get(key_len, ops);
    key[0] = @backingInt(KeyTag.tag_union);
    key[1] = variants.len;
    {
        var at: usize = 2;
        for (variants) |variant| {
            key[at] = variant.name.id;
            key[at + 1] = variant.payload_count;
            at += 2;
            for (variant.payloadSlice()) |payload| {
                key[at] = ptrWord(payload);
                at += 1;
            }
        }
    }
    if (cache.lookup(key)) |existing| return existing;

    var rank_scratch: Scratch(u32, 32) = .{};
    defer rank_scratch.deinit(ops);
    const by_name = rank_scratch.get(variants.len, ops);
    sortByName(VariantInput, variants, by_name, ops);

    const discriminant_size = shape.discriminantSize(variants.len);
    var measure = shape.TagUnionMeasure.init(discriminant_size);
    const block = allocWithEntries(Variant, variants.len, ops);
    var contains_refcounted = false;
    for (by_name, 0..) |input_index, discriminant| {
        const input = variants[input_index];
        const payload = switch (input.payload_count) {
            0 => zstDesc(),
            1 => input.payloads[0],
            else => tuple(cache, ops, input.payloadSlice()),
        };
        measure.addPayload(.{ .size = payload.size, .alignment = payload.alignment, .sort_key = sortKeyOf(payload) });
        contains_refcounted = contains_refcounted or payload.flags.contains_refcounted;
        block.entries[discriminant] = .{
            .discriminant = @intCast(discriminant),
            .payload_count = input.payload_count,
            .name = input.name,
            .payload = payload,
        };
    }
    const metrics = measure.finish();
    const zero_sized = metrics.size == 0;
    block.desc.* = .{
        .size = metrics.size,
        .alignment = if (zero_sized) 1 else metrics.alignment,
        .kind = .tag_union,
        .scalar = .u8,
        .sort_key = @backingInt(if (zero_sized) SortKey.align_1 else metrics.sort_key),
        .flags = .{ .contains_refcounted = contains_refcounted },
        .count = @intCast(variants.len),
        .discriminant_offset = metrics.discriminant_offset,
        .discriminant_size = discriminant_size,
        .child = null,
        .entries = if (variants.len == 0) null else @ptrCast(block.entries),
        .name = null,
        .methods = null,
    };
    return cache.intern(key, block.desc, ops);
}

/// A nominal type over `backing`, whose layout it shares.
pub fn nominal(
    cache: *Cache,
    ops: *RocOps,
    backing: *const TypeDesc,
    name: ?*const Name,
    methods: ?*const Methods,
    inspect_opaque: bool,
) *const TypeDesc {
    const key = [_]u64{ @backingInt(KeyTag.nominal), ptrWord(backing), nameWord(name), if (methods) |m| ptrWord(m) else 0, @intFromBool(inspect_opaque) };
    if (cache.lookup(&key)) |existing| return existing;
    const built = immortal(TypeDesc, 1, ops);
    built[0] = .{
        .size = backing.size,
        .alignment = backing.alignment,
        .kind = .nominal,
        .scalar = .u8,
        .sort_key = backing.sort_key,
        .flags = .{ .contains_refcounted = backing.flags.contains_refcounted, .inspect_opaque = inspect_opaque },
        .count = 0,
        .discriminant_offset = 0,
        .discriminant_size = 0,
        .child = backing,
        .entries = null,
        .name = name,
        .methods = methods,
    };
    return cache.intern(&key, &built[0], ops);
}

// Struct building.

const Order = enum { structural, declared };

const Member = struct {
    name: ?*const Name,
    desc: *const TypeDesc,
    semantic_index: u32,
    is_padding: bool,
};

fn WithEntries(comptime Entry: type) type {
    return struct { desc: *TypeDesc, entries: [*]Entry };
}

/// One immortal block holding a descriptor followed by its entries.
fn allocWithEntries(comptime Entry: type, count: usize, ops: *RocOps) WithEntries(Entry) {
    const entries_offset = std.mem.alignForward(usize, @sizeOf(TypeDesc), @alignOf(Entry));
    const total = entries_offset + count * @sizeOf(Entry);
    const bytes: [*]u8 = @ptrCast(ops.allocRaw(total, @max(@alignOf(TypeDesc), @alignOf(Entry))));
    return .{
        .desc = @ptrCast(@alignCast(bytes)),
        .entries = @ptrCast(@alignCast(bytes + entries_offset)),
    };
}

/// Lay out `members` (indexed by input position) as one struct and build its
/// descriptor. Semantic indices must be a permutation of the positions.
fn buildStruct(ops: *RocOps, kind: Kind, order: Order, members: []const Member) *const TypeDesc {
    var order_scratch: Scratch(u32, 32) = .{};
    defer order_scratch.deinit(ops);
    const memory_order = order_scratch.get(members.len, ops);
    switch (order) {
        .declared => for (memory_order, 0..) |*slot, index| {
            slot.* = @intCast(index);
        },
        .structural => {
            var keys_scratch: Scratch(field_order.StructuralField, 32) = .{};
            defer keys_scratch.deinit(ops);
            const keys = keys_scratch.get(members.len, ops);
            for (members, keys) |member, *key| key.* = .{
                .sort_key = if (member.is_padding) .align_1 else sortKeyOf(member.desc),
                .semantic_index = member.semantic_index,
            };
            field_order.computeStructuralFieldOrder(keys, memory_order);
        },
    }

    const block = allocWithEntries(Field, members.len, ops);
    var measure: shape.StructMeasure = .{};
    var contains_refcounted = false;
    for (memory_order, 0..) |member_index, slot| {
        const member = members[member_index];
        const offset = measure.place(.{
            .size = member.desc.size,
            .alignment = member.desc.alignment,
            .sort_key = sortKeyOf(member.desc),
            .is_padding = member.is_padding,
        });
        if (!member.is_padding) contains_refcounted = contains_refcounted or member.desc.flags.contains_refcounted;
        block.entries[slot] = .{
            .semantic_index = member.semantic_index,
            .offset = offset,
            .is_padding = @intFromBool(member.is_padding),
            .semantic_order = std.math.maxInt(u32),
            .name = member.name,
            .desc = member.desc,
        };
    }
    for (block.entries[0..members.len], 0..) |entry, slot| {
        if (entry.semantic_index >= members.len or block.entries[entry.semantic_index].semantic_order != std.math.maxInt(u32)) {
            invariant(ops, "a composed struct's semantic indices are not a permutation of its fields");
        }
        block.entries[entry.semantic_index].semantic_order = @intCast(slot);
    }

    const metrics = measure.finish();
    const zero_sized = metrics.size == 0;
    block.desc.* = .{
        .size = metrics.size,
        .alignment = if (zero_sized) 1 else metrics.alignment,
        .kind = kind,
        .scalar = .u8,
        .sort_key = @backingInt(if (zero_sized) SortKey.align_1 else metrics.sort_key),
        .flags = .{ .contains_refcounted = contains_refcounted, .declared_order = order == .declared },
        .count = @intCast(members.len),
        .discriminant_offset = 0,
        .discriminant_size = 0,
        .child = null,
        .entries = if (members.len == 0) null else @ptrCast(block.entries),
        .name = null,
        .methods = null,
    };
    return block.desc;
}

/// Fill `out` with the indices of `items` ordered by name text; a duplicate
/// name is a compiler invariant violation.
fn sortByName(comptime T: type, items: []const T, out: []u32, ops: *RocOps) void {
    for (out, 0..) |*slot, index| slot.* = @intCast(index);
    const Ctx = struct {
        items: []const T,
        fn lessThan(ctx: @This(), a: u32, b: u32) bool {
            return nameOrder(ctx.items[a].name, ctx.items[b].name) == .lt;
        }
    };
    std.sort.pdq(u32, out, Ctx{ .items = items }, Ctx.lessThan);
    var index: usize = 1;
    while (index < out.len) : (index += 1) {
        if (nameOrder(items[out[index - 1]].name, items[out[index]].name) == .eq) {
            invariant(ops, "a composed record or tag union names the same label twice");
        }
    }
}

/// A violated compiler invariant. Builds with runtime safety crash through
/// `ops` naming it; optimized builds treat the path as unreachable.
pub fn invariant(ops: *RocOps, comptime message: []const u8) noreturn {
    if (std.debug.runtime_safety) ops.crash("boxy runtime invariant violated: " ++ message);
    unreachable; // only builds without runtime safety reach this, where design.md makes a violated invariant undefined
}
