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
//! nothing. Descriptors and the cache live in immortal memory
//! (`immortalBytes`): a platform's for the life of the process, the
//! compiler's own for the life of the program composing them
//! (`compiled.InProcessState`).

const std = @import("std");
const builtin = @import("builtin");
const builtins = @import("builtins");
const raw_pages = @import("raw_pages");
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

/// The descriptor of `Str`.
pub fn strDesc() *const TypeDesc {
    return &str_leaf;
}

/// The descriptor of `Bool`.
pub fn boolDesc() *const TypeDesc {
    return &bool_leaf;
}

/// The descriptor every function value shares.
pub fn erasedCallableDesc() *const TypeDesc {
    return &erased_callable_leaf;
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

/// A platform's composition cache, through which its C entry points
/// compose. The compiler's process composes into the running program's
/// `compiled.InProcessState` instead.
pub var process_cache: Cache = .{};

/// An immortal interning table from constructor keys to descriptors.
///
/// A composition's inputs are immortal descriptors, so its key is a few
/// words of pointers and tags, and a hit is the common case: a generic body
/// composes the same descriptors on every call. Readers therefore probe the
/// current table without taking the lock. A table is only ever added to,
/// each entry's `value` is published last, and a table that growth replaced
/// stays alive (immortal, like everything here), so a reader holding it sees
/// a consistent, possibly stale, table and a miss there retries under the
/// lock against the current one. Only insertion and growth take the lock.
pub const Cache = struct {
    lock: std.atomic.Value(u32) = .init(0),
    table: std.atomic.Value(?*const Table) = .init(null),
    len: u32 = 0,

    const Table = struct {
        /// A power of two.
        capacity: u32,
        entries: [*]Entry,
    };

    const Entry = struct {
        hash: u64,
        key: [*]const u64,
        key_len: u32,
        /// Written last with release ordering: a reader that sees it sees
        /// the key words it was stored under.
        value: std.atomic.Value(?*const TypeDesc),
    };

    fn acquire(self: *Cache) void {
        while (self.lock.cmpxchgWeak(0, 1, .acquire, .monotonic) != null) {
            std.atomic.spinLoopHint();
        }
    }

    fn release(self: *Cache) void {
        self.lock.store(0, .release);
    }

    fn find(table: *const Table, hash: u64, key: []const u64) ?*const TypeDesc {
        const mask = table.capacity - 1;
        var index: u32 = @truncate(hash & mask);
        while (true) : (index = (index + 1) & mask) {
            const entry = &table.entries[index];
            const value = entry.value.load(.acquire) orelse return null;
            if (entry.hash == hash and entry.key_len == key.len and keysEqual(entry.key[0..entry.key_len], key)) return value;
        }
    }

    pub fn lookup(self: *Cache, key: []const u64) ?*const TypeDesc {
        const table = self.table.load(.acquire) orelse return null;
        return find(table, keyHash(key), key);
    }

    /// Intern `value` under `key`, or return the descriptor another thread
    /// interned under it first.
    pub fn intern(self: *Cache, key: []const u64, value: *const TypeDesc, ops: *RocOps) *const TypeDesc {
        const hash = keyHash(key);
        self.acquire();
        defer self.release();
        if (self.table.load(.monotonic)) |table| {
            if (find(table, hash, key)) |existing| return existing;
            if ((self.len + 1) * 4 > table.capacity * 3) self.grow(ops);
        } else self.grow(ops);
        const table = self.table.load(.monotonic).?;
        const stored = immortal(u64, key.len, ops);
        @memcpy(stored[0..key.len], key);
        place(table, hash, stored, @intCast(key.len), value);
        self.len += 1;
        return value;
    }

    /// Store an entry in `table`'s first free slot on its probe path,
    /// publishing its value last.
    fn place(table: *const Table, hash: u64, key: [*]const u64, key_len: u32, value: *const TypeDesc) void {
        const mask = table.capacity - 1;
        var index: u32 = @truncate(hash & mask);
        while (table.entries[index].value.load(.monotonic) != null) index = (index + 1) & mask;
        const entry = &table.entries[index];
        entry.hash = hash;
        entry.key = key;
        entry.key_len = key_len;
        entry.value.store(value, .release);
    }

    /// Publish a table twice the size holding every entry of the current
    /// one, which stays alive for the readers still probing it.
    fn grow(self: *Cache, ops: *RocOps) void {
        const old = self.table.load(.monotonic);
        const old_capacity: u32 = if (old) |table| table.capacity else 0;
        const new_capacity: u32 = if (old_capacity == 0) 64 else old_capacity * 2;
        const fresh: *Table = @ptrCast(@alignCast(immortalBytes(ops, @sizeOf(Table), @alignOf(Table))));
        const entries: [*]Entry = @ptrCast(@alignCast(immortalBytes(ops, @as(usize, new_capacity) * @sizeOf(Entry), @alignOf(Entry))));
        for (entries[0..new_capacity]) |*entry| entry.value = .init(null);
        fresh.* = .{ .capacity = new_capacity, .entries = entries };
        if (old) |table| {
            for (table.entries[0..table.capacity]) |*entry| {
                const value = entry.value.load(.monotonic) orelse continue;
                place(fresh, entry.hash, entry.key, entry.key_len, value);
            }
        }
        self.table.store(fresh, .release);
    }
};

fn keysEqual(a: []const u64, b: []const u64) bool {
    for (a, b) |x, y| {
        if (x != y) return false;
    }
    return true;
}

/// A mix of the key's words: keys are a handful of tags and pointers, so
/// each word is folded in with a multiply and a shift.
fn keyHash(key: []const u64) u64 {
    var hash: u64 = 0x9e37_79b9_7f4a_7c15;
    for (key) |word| {
        hash = (hash ^ word) *% 0xbf58_476d_1ce4_e5b9;
        hash ^= hash >> 31;
    }
    return hash;
}

fn immortal(comptime T: type, count: usize, ops: *RocOps) [*]T {
    return @ptrCast(@alignCast(immortalBytes(ops, @max(count, 1) * @sizeOf(T), @alignOf(T))));
}

/// Bytes that live as long as the process: a composed descriptor or
/// dictionary, or the composition cache. In a platform build every `RocOps`
/// allocation goes to the host, whose allocator owns the values a program
/// releases, so immortal memory there comes from process-lifetime pages;
/// elsewhere `ops` is itself an immortal allocator.
pub fn immortalBytes(ops: *RocOps, size: usize, alignment: usize) [*]u8 {
    if (comptime builtins.host_abi.host_role == .platform) return PlatformImmortals.alloc(ops, size, alignment);
    return @ptrCast(ops.allocRaw(size, alignment));
}

/// Process-lifetime pages carved front to back. Nothing here is ever freed,
/// so a bump pointer over whole pages is all the bookkeeping it needs.
const PlatformImmortals = struct {
    var lock: std.atomic.Mutex = .unlocked;
    var next: usize = 0;
    var end: usize = 0;

    const min_chunk_bytes = 64 * 1024;

    /// Fresh pages. A program's object has no standard library to reach
    /// the kernel through, except on wasm, where memory only grows.
    fn mapChunk(len: usize) ?[*]u8 {
        if (comptime builtin.cpu.arch.isWasm()) return std.heap.page_allocator.rawAlloc(len, .fromByteUnits(std.heap.pageSize()), @returnAddress());
        return raw_pages.map(len);
    }

    fn alloc(ops: *RocOps, size: usize, alignment: usize) [*]u8 {
        while (!lock.tryLock()) std.atomic.spinLoopHint();
        defer lock.unlock();
        var start = std.mem.alignForward(usize, next, alignment);
        if (next == 0 or size > end - @min(start, end)) {
            const page = std.heap.pageSize();
            const chunk_len = std.mem.alignForward(usize, @max(size + alignment, min_chunk_bytes), page);
            const chunk = mapChunk(chunk_len) orelse ops.crash("out of memory composing a Boxy descriptor");
            next = @intFromPtr(chunk);
            end = next + chunk_len;
            start = std.mem.alignForward(usize, next, alignment);
        }
        next = start + size;
        return @ptrFromInt(start);
    }
};

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
    recursive,
};

/// The key words of a name: its content id (design.md "Name Identity"), so
/// a composition keyed by a name is one composition whichever name node
/// names it.
const name_key_words = 4;

fn ptrWord(ptr: anytype) u64 {
    return @intFromPtr(ptr);
}

/// Write `name`'s key words at `key[at..]`, or the words of no name.
fn putNameWords(key: []u64, at: usize, name: ?*const Name) void {
    if (name) |n| {
        key[at..][0..name_key_words].* = n.id;
    } else {
        key[at..][0..name_key_words].* = @splat(std.math.maxInt(u64));
    }
}

fn nameOrder(a: *const Name, b: *const Name) std.math.Order {
    if (a.eql(b)) return .eq;
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
/// allocation. A box of a function is the function's erased callable, which
/// is an allocation of its own, with the function as its `child` so the value
/// still inspects as a box.
pub fn box(cache: *Cache, ops: *RocOps, payload: *const TypeDesc) *const TypeDesc {
    if (payload.kind == .erased_callable and payload.child == null) {
        const key = [_]u64{ @backingInt(KeyTag.box), ptrWord(payload) };
        if (cache.lookup(&key)) |existing| return existing;
        const built = immortal(TypeDesc, 1, ops);
        built[0] = payload.*;
        built[0].child = payload;
        return cache.intern(&key, &built[0], ops);
    }
    return container(cache, ops, .box, payload, payload.size != 0);
}

/// The implicit box a layout inserts to break a recursive type. Its payload
/// may be a recursive member's open placeholder, which is never zero-sized.
pub fn recursiveBox(cache: *Cache, ops: *RocOps, payload: *const TypeDesc) *const TypeDesc {
    return container(cache, ops, .recursive_box, payload, payload.size != 0);
}

/// The `size` of a recursive member's placeholder until it is closed.
const open_size = std.math.maxInt(u32);

/// The key words of a recursive member: `key` names its type and `vars`
/// are its type variables' descriptors. Caller deinits the scratch.
fn recursiveKey(scratch: *Scratch(u64, 16), ops: *RocOps, key: *const u8, vars: []const *const TypeDesc) []const u64 {
    const words = scratch.get(2 + vars.len, ops);
    words[0] = @backingInt(KeyTag.recursive);
    words[1] = ptrWord(key);
    for (vars, words[2..]) |desc, *word| word.* = ptrWord(desc);
    return words;
}

/// Begin composing a recursive member's descriptor: the finished descriptor
/// interned under `key` and `vars`, or a placeholder for its components'
/// recursive boxes to point at until `recursiveClose` fills it.
pub fn recursiveOpen(cache: *Cache, ops: *RocOps, key: *const u8, vars: []const *const TypeDesc) *const TypeDesc {
    var scratch: Scratch(u64, 16) = .{};
    defer scratch.deinit(ops);
    if (cache.lookup(recursiveKey(&scratch, ops, key, vars))) |existing| return existing;
    const shell = immortal(TypeDesc, 1, ops);
    shell[0] = leaf(.zst, .u8, 0, 1, .align_1, false);
    shell[0].size = open_size;
    return &shell[0];
}

/// Fill the placeholder `shell` with the member's composed descriptor
/// `desc` and intern it, unless `recursiveOpen` returned it finished.
pub fn recursiveClose(cache: *Cache, ops: *RocOps, shell: *const TypeDesc, desc: *const TypeDesc, key: *const u8, vars: []const *const TypeDesc) *const TypeDesc {
    if (shell.size != open_size) return shell;
    if (desc.size == open_size) invariant(ops, "a recursive member's composed descriptor is itself an open placeholder");
    const filled: *TypeDesc = @constCast(shell);
    filled.* = desc.*;
    var scratch: Scratch(u64, 16) = .{};
    defer scratch.deinit(ops);
    _ = cache.intern(recursiveKey(&scratch, ops, key, vars), shell, ops);
    return shell;
}

fn container(cache: *Cache, ops: *RocOps, comptime kind: Kind, child: *const TypeDesc, contains_refcounted: bool) *const TypeDesc {
    const tag: KeyTag = switch (kind) {
        .list => .list,
        .box => .box,
        .recursive_box => .recursive_box,
        .zst, .scalar, .str, .bool, .record, .tuple, .tag_union, .erased_callable, .nominal => @compileError("not a container kind"),
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
    const key = key_scratch.get(2 + (name_key_words + 1) * fields.len, ops);
    key[0] = @backingInt(KeyTag.record);
    key[1] = fields.len;
    for (fields, 0..) |field, index| {
        const at = 2 + (name_key_words + 1) * index;
        putNameWords(key, at, field.name);
        key[at + name_key_words] = ptrWord(field.desc);
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
        members[input_index] = .{ .name = field.name, .desc = field.desc, .field_index = @intCast(rank), .is_padding = false };
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
        .scalar, .str, .bool, .list, .box, .tuple, .tag_union, .erased_callable, .nominal, .recursive_box => invariant(ops, "a record row's remainder descriptor is not a record"),
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
        .scalar, .str, .bool, .list, .box, .record, .tuple, .erased_callable, .nominal, .recursive_box => invariant(ops, "a tag row's remainder descriptor is not a tag union"),
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
            if (known.name.eql(variant.name)) break known;
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
    const words_per_field = name_key_words + 2;
    const key = key_scratch.get(2 + words_per_field * fields.len, ops);
    key[0] = @backingInt(KeyTag.declared_record);
    key[1] = fields.len;
    for (fields, 0..) |field, index| {
        const at = 2 + words_per_field * index;
        putNameWords(key, at, field.name);
        key[at + name_key_words] = ptrWord(field.desc);
        key[at + name_key_words + 1] = field.is_padding;
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
        members[position] = .{ .name = fields[position].name, .desc = fields[position].desc, .field_index = @intCast(rank), .is_padding = false };
    }
    var padding_rank = named_count;
    for (fields, 0..) |field, position| {
        if (field.is_padding == 0) continue;
        members[position] = .{ .name = null, .desc = field.desc, .field_index = padding_rank, .is_padding = true };
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
    for (descs, 0..) |d, index| members[index] = .{ .name = null, .desc = d, .field_index = @intCast(index), .is_padding = false };
    const order: Order = if (tag == .args) .declared else .structural;
    const built = buildStruct(ops, .tuple, order, members);
    return cache.intern(key, built, ops);
}

/// An optional record field's presence slot over `payload`: the tag union
/// `[missing, present(payload)]`, marked as a presence slot.
pub fn presenceSlot(cache: *Cache, ops: *RocOps, missing: *const Name, present: *const Name, payload: *const TypeDesc) *const TypeDesc {
    var key: [2 + 2 * name_key_words]u64 = undefined;
    key[0] = @backingInt(KeyTag.presence_slot);
    putNameWords(&key, 1, missing);
    putNameWords(&key, 1 + name_key_words, present);
    key[1 + 2 * name_key_words] = ptrWord(payload);
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
    for (variants) |variant| key_len += name_key_words + 1 + variant.payload_count;
    var key_scratch: Scratch(u64, 64) = .{};
    defer key_scratch.deinit(ops);
    const key = key_scratch.get(key_len, ops);
    key[0] = @backingInt(KeyTag.tag_union);
    key[1] = variants.len;
    {
        var at: usize = 2;
        for (variants) |variant| {
            putNameWords(key, at, variant.name);
            key[at + name_key_words] = variant.payload_count;
            at += name_key_words + 1;
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
    var key: [4 + name_key_words]u64 = undefined;
    key[0] = @backingInt(KeyTag.nominal);
    key[1] = ptrWord(backing);
    putNameWords(&key, 2, name);
    key[2 + name_key_words] = if (methods) |m| ptrWord(m) else 0;
    key[3 + name_key_words] = @intFromBool(inspect_opaque);
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
    field_index: u32,
    is_padding: bool,
};

fn WithEntries(comptime Entry: type) type {
    return struct { desc: *TypeDesc, entries: [*]Entry };
}

/// One immortal block holding a descriptor followed by its entries.
fn allocWithEntries(comptime Entry: type, count: usize, ops: *RocOps) WithEntries(Entry) {
    const entries_offset = std.mem.alignForward(usize, @sizeOf(TypeDesc), @alignOf(Entry));
    const total = entries_offset + count * @sizeOf(Entry);
    const bytes: [*]u8 = immortalBytes(ops, total, @max(@alignOf(TypeDesc), @alignOf(Entry)));
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
                .semantic_index = member.field_index,
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
            .field_index = member.field_index,
            .offset = offset,
            .is_padding = @intFromBool(member.is_padding),
            .field_slot = std.math.maxInt(u32),
            .name = member.name,
            .desc = member.desc,
        };
    }
    for (block.entries[0..members.len], 0..) |entry, slot| {
        if (entry.field_index >= members.len or block.entries[entry.field_index].field_slot != std.math.maxInt(u32)) {
            invariant(ops, "a composed struct's semantic indices are not a permutation of its fields");
        }
        block.entries[entry.field_index].field_slot = @intCast(slot);
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
