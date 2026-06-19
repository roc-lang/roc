//! A serialization-friendly string interner that assigns **dense serial ids**.
//!
//! This is the relocatable replacement for the `std.ArrayList([]const u8)` +
//! `std.StringHashMap` pairs that back `CanonicalNameStore`. Like
//! `base.SmallStringInterner` it stores all text in one flat byte buffer and an
//! open-addressing hash table as a flat array of integers — both
//! `SafeList`-backed, so the whole interner relocates with a **constant** number
//! of base-pointer fixups (3), independent of how many names it holds.
//!
//! Unlike `SmallStringInterner` (whose id is a byte offset), this interner hands
//! out **serial** ids (0, 1, 2, … in insertion order) via a separate `ranges`
//! array. `CanonicalNameStore`'s `*NameId`s are serial indices today, and are
//! used internally as such (e.g. `NominalTypeKey` equality, proc-base key
//! encoding); preserving serial semantics keeps the conversion behavior-
//! identical rather than relying on byte-offset ids being incidentally
//! consistent.
//!
//! After deserialization the interner is frozen (`supports_inserts = false`):
//! `lookup` (text → id) and `getText` (id → text) both work with NO rebuild,
//! because the probing table is part of the serialized data and is relocated in
//! place. `enableRuntimeInserts` can re-open it for insertion if ever needed.

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("collections");

const Allocator = std.mem.Allocator;
const CompactWriter = collections.CompactWriter;
const SafeList = collections.SafeList;

const NameInterner = @This();

/// `{start, len}` into `bytes` for one interned string. POD / relocation-invariant.
pub const Range = extern struct {
    start: u32,
    len: u32,
};

/// All interned text, concatenated. No separators needed; `ranges` holds bounds.
bytes: SafeList(u8) = .{},
/// Serial id -> byte range. `ranges.len()` is the number of interned strings.
ranges: SafeList(Range) = .{},
/// Open-addressing (linear-probing) hash table: slot -> (serial id + 1).
/// A value of 0 means the slot is empty. Stored as a flat array of u32 so it
/// relocates with a single base-pointer fixup.
index: SafeList(u32) = .{},
/// True while this interner owns growable memory; false after deserialize
/// (memory then points into the serialization buffer and must not be freed/grown).
supports_inserts: bool = true,

const initial_index_capacity: usize = 16;

/// Initialize an empty, insertable interner sized for roughly `capacity` names.
pub fn initCapacity(gpa: Allocator, capacity: usize) Allocator.Error!NameInterner {
    const target = ((capacity * 5) / 4) + 1;
    const index_cap = std.math.ceilPowerOfTwo(usize, @max(target, initial_index_capacity)) catch initial_index_capacity;

    var self = NameInterner{
        .bytes = .{},
        .ranges = .{},
        .index = .{},
        .supports_inserts = true,
    };
    self.bytes = try SafeList(u8).initCapacity(gpa, capacity * 4);
    self.ranges = try SafeList(Range).initCapacity(gpa, capacity);
    self.index = try SafeList(u32).initCapacity(gpa, index_cap);
    try self.index.items.ensureTotalCapacityPrecise(gpa, index_cap);
    self.index.items.items.len = index_cap;
    @memset(self.index.items.items, 0);
    return self;
}

/// Free owned memory. No-op for deserialized (frozen) interners, whose memory is
/// owned by the serialization buffer.
pub fn deinit(self: *NameInterner, gpa: Allocator) void {
    if (!self.supports_inserts) return;
    self.bytes.deinit(gpa);
    self.ranges.deinit(gpa);
    self.index.deinit(gpa);
}

/// Number of interned strings.
pub fn count(self: *const NameInterner) u32 {
    return @intCast(self.ranges.items.items.len);
}

fn textAt(self: *const NameInterner, id: u32) []const u8 {
    const r = self.ranges.items.items[id];
    return self.bytes.items.items[r.start .. r.start + r.len];
}

/// Text for a serial id.
pub fn getText(self: *const NameInterner, id: u32) []const u8 {
    return self.textAt(id);
}

const Found = struct { id: ?u32, slot: usize };

fn findSlot(self: *const NameInterner, string: []const u8) Found {
    const table_size = self.index.items.items.len;
    if (table_size == 0) return .{ .id = null, .slot = 0 };
    const hash = std.hash.Fnv1a_32.hash(string);
    var slot: usize = @intCast(hash % @as(u32, @intCast(table_size)));
    while (true) {
        const v = self.index.items.items[slot];
        if (v == 0) return .{ .id = null, .slot = slot };
        const id = v - 1;
        if (std.mem.eql(u8, string, self.textAt(id))) return .{ .id = id, .slot = slot };
        slot = (slot + 1) % table_size;
    }
}

/// Look up a string's serial id without modifying the interner. Safe on frozen
/// (deserialized) interners.
pub fn lookup(self: *const NameInterner, string: []const u8) ?u32 {
    return self.findSlot(string).id;
}

fn rehash(self: *NameInterner, gpa: Allocator) Allocator.Error!void {
    const new_size = self.index.items.items.len * 2;
    self.index.deinit(gpa);
    self.index = try SafeList(u32).initCapacity(gpa, new_size);
    try self.index.items.ensureTotalCapacityPrecise(gpa, new_size);
    self.index.items.items.len = new_size;
    @memset(self.index.items.items, 0);

    var id: u32 = 0;
    while (id < self.ranges.items.items.len) : (id += 1) {
        const found = self.findSlot(self.textAt(id));
        self.index.items.items[found.slot] = id + 1;
    }
}

/// Intern `string`, returning its serial id. Deduplicates: equal text always
/// returns the same id.
pub fn insert(self: *NameInterner, gpa: Allocator, string: []const u8) Allocator.Error!u32 {
    if (self.index.items.items.len != 0) {
        if (self.findSlot(string).id) |id| return id;
    }

    if (builtin.mode == .Debug) {
        std.debug.assert(self.supports_inserts);
    } else if (!self.supports_inserts) {
        unreachable;
    }

    // Lazily allocate the probing table on first insert so a default-initialized
    // (`.{}`) interner is valid and `init` paths can stay non-failing.
    if (self.index.items.items.len == 0) {
        try self.index.items.ensureTotalCapacityPrecise(gpa, initial_index_capacity);
        self.index.items.items.len = initial_index_capacity;
        @memset(self.index.items.items, 0);
    } else if ((self.count() + 1) * 5 >= self.index.items.items.len * 4) {
        try self.rehash(gpa);
    }

    const id: u32 = self.count();
    const start: u32 = @intCast(self.bytes.items.items.len);
    _ = try self.bytes.appendSlice(gpa, string);
    _ = try self.ranges.append(gpa, .{ .start = start, .len = @intCast(string.len) });

    const slot = self.findSlot(string).slot;
    self.index.items.items[slot] = id + 1;
    return id;
}

/// Re-open a deserialized interner for insertion by copying its data into fresh,
/// growable memory.
pub fn enableRuntimeInserts(self: *NameInterner, gpa: Allocator) Allocator.Error!void {
    if (self.supports_inserts) return;

    var new_bytes = try SafeList(u8).initCapacity(gpa, self.bytes.items.items.len);
    _ = try new_bytes.appendSlice(gpa, self.bytes.items.items);
    var new_ranges = try SafeList(Range).initCapacity(gpa, self.ranges.items.items.len);
    _ = try new_ranges.appendSlice(gpa, self.ranges.items.items);
    var new_index = try SafeList(u32).initCapacity(gpa, self.index.items.items.len);
    try new_index.items.ensureTotalCapacityPrecise(gpa, self.index.items.items.len);
    new_index.items.items.len = self.index.items.items.len;
    @memcpy(new_index.items.items, self.index.items.items);

    self.bytes = new_bytes;
    self.ranges = new_ranges;
    self.index = new_index;
    self.supports_inserts = true;
}

/// Add this interner's offset to its three base pointers. The number of fixups
/// (3, minus any empty lists) is fixed by the type, never by name count.
pub fn relocate(self: *NameInterner, offset: isize) void {
    self.bytes.relocate(offset);
    self.ranges.relocate(offset);
    self.index.relocate(offset);
}

/// Relocatable serialized form. Exactly 3 relocatable base pointers.
pub const Serialized = extern struct {
    bytes: SafeList(u8).Serialized,
    ranges: SafeList(Range).Serialized,
    index: SafeList(u32).Serialized,
    entry_count: u32,
    _padding: u32 = 0,

    pub fn serialize(
        self: *Serialized,
        interner: *const NameInterner,
        gpa: Allocator,
        writer: *CompactWriter,
    ) Allocator.Error!void {
        try self.bytes.serialize(&interner.bytes, gpa, writer);
        try self.ranges.serialize(&interner.ranges, gpa, writer);
        try self.index.serialize(&interner.index, gpa, writer);
        self.entry_count = interner.count();
        self._padding = 0;
    }

    pub fn deserialize(self: *const Serialized, base: usize) NameInterner {
        return .{
            .bytes = self.bytes.deserializeInto(base),
            .ranges = self.ranges.deserializeInto(base),
            .index = self.index.deserializeInto(base),
            .supports_inserts = false,
        };
    }
};

const testing = std.testing;

test "NameInterner: serial ids, dedup, lookup, getText" {
    const gpa = testing.allocator;
    var it = try NameInterner.initCapacity(gpa, 4);
    defer it.deinit(gpa);

    const a = try it.insert(gpa, "List");
    const b = try it.insert(gpa, "Dict");
    const c = try it.insert(gpa, "List"); // dup

    try testing.expectEqual(@as(u32, 0), a);
    try testing.expectEqual(@as(u32, 1), b);
    try testing.expectEqual(@as(u32, 0), c); // same id as first "List"
    try testing.expectEqual(@as(u32, 2), it.count());

    try testing.expectEqualStrings("List", it.getText(0));
    try testing.expectEqualStrings("Dict", it.getText(1));
    try testing.expectEqual(@as(?u32, 1), it.lookup("Dict"));
    try testing.expectEqual(@as(?u32, null), it.lookup("Set"));
}

test "NameInterner: default-empty interner lazily initializes on first insert" {
    const gpa = testing.allocator;
    var it: NameInterner = .{}; // no initCapacity — mirrors CanonicalNameStore.init's `.empty`
    defer it.deinit(gpa);

    try testing.expectEqual(@as(u32, 0), it.count());
    try testing.expectEqual(@as(?u32, null), it.lookup("List")); // lookup on empty table is safe

    try testing.expectEqual(@as(u32, 0), try it.insert(gpa, "List"));
    try testing.expectEqual(@as(u32, 1), try it.insert(gpa, "Dict"));
    try testing.expectEqual(@as(u32, 0), try it.insert(gpa, "List"));
    try testing.expectEqualStrings("Dict", it.getText(1));
    try testing.expectEqual(@as(?u32, 0), it.lookup("List"));
}

test "NameInterner: grows past initial table capacity preserving ids/lookup" {
    const gpa = testing.allocator;
    var it = try NameInterner.initCapacity(gpa, 2);
    defer it.deinit(gpa);

    var buf: [16]u8 = undefined;
    var i: u32 = 0;
    while (i < 500) : (i += 1) {
        const id = try it.insert(gpa, try std.fmt.bufPrint(&buf, "n{d}", .{i}));
        try testing.expectEqual(i, id); // serial
    }
    try testing.expectEqual(@as(u32, 500), it.count());
    // all still resolve after multiple rehashes
    i = 0;
    while (i < 500) : (i += 1) {
        const name = try std.fmt.bufPrint(&buf, "n{d}", .{i});
        try testing.expectEqual(@as(?u32, i), it.lookup(name));
        try testing.expectEqualStrings(name, it.getText(i));
    }
}

fn roundTrip(gpa: Allocator, src: *const NameInterner) !struct { buffer: []align(16) u8, it: NameInterner } {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const aa = arena.allocator();

    var writer = CompactWriter.init();
    const hdr = try writer.appendAlloc(aa, NameInterner.Serialized);
    try hdr.serialize(src, aa, &writer);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    _ = try writer.writeToBuffer(buffer);

    const ser: *const NameInterner.Serialized = @ptrCast(@alignCast(buffer.ptr));
    return .{ .buffer = buffer, .it = ser.deserialize(@intFromPtr(buffer.ptr)) };
}

test "NameInterner: lookup + getText survive serialize/relocate with no rebuild" {
    const gpa = testing.allocator;
    var it = try NameInterner.initCapacity(gpa, 8);
    defer it.deinit(gpa);
    const list = try it.insert(gpa, "List");
    const dict = try it.insert(gpa, "Dict");
    const map = try it.insert(gpa, "map");

    var rt = try roundTrip(gpa, &it);
    defer gpa.free(rt.buffer);

    try testing.expect(!rt.it.supports_inserts);
    try testing.expectEqualStrings("List", rt.it.getText(list));
    try testing.expectEqualStrings("Dict", rt.it.getText(dict));
    try testing.expectEqualStrings("map", rt.it.getText(map));
    try testing.expectEqual(@as(?u32, list), rt.it.lookup("List"));
    try testing.expectEqual(@as(?u32, dict), rt.it.lookup("Dict"));
    try testing.expectEqual(@as(?u32, null), rt.it.lookup("Set"));
}

fn nonEmptyBasePointers(it: *const NameInterner) usize {
    var n: usize = 0;
    if (it.bytes.items.capacity != 0) n += 1;
    if (it.ranges.items.capacity != 0) n += 1;
    if (it.index.items.capacity != 0) n += 1;
    return n;
}

test "NameInterner: relocation fixup count is constant in number of names" {
    const gpa = testing.allocator;

    var small = try NameInterner.initCapacity(gpa, 2);
    defer small.deinit(gpa);
    _ = try small.insert(gpa, "List");

    var large = try NameInterner.initCapacity(gpa, 8);
    defer large.deinit(gpa);
    var buf: [16]u8 = undefined;
    var i: u32 = 0;
    while (i < 4000) : (i += 1) _ = try large.insert(gpa, try std.fmt.bufPrint(&buf, "T{d}", .{i}));

    var rt_small = try roundTrip(gpa, &small);
    defer gpa.free(rt_small.buffer);
    var rt_large = try roundTrip(gpa, &large);
    defer gpa.free(rt_large.buffer);

    try testing.expectEqual(nonEmptyBasePointers(&rt_small.it), nonEmptyBasePointers(&rt_large.it));
    try testing.expectEqual(@as(usize, 3), nonEmptyBasePointers(&rt_large.it));
    try testing.expectEqualStrings("T3999", rt_large.it.getText(rt_large.it.lookup("T3999").?));
}
