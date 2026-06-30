//! A serialization-friendly string interner that assigns **dense serial ids**.
//!
//! It backs `CanonicalNameStore`'s name kinds with relocatable storage. Like
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
const InternedBytes = @import("InternedBytes.zig");

const SerialStringInterner = @This();

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
const Index = InternedBytes.Index(Policy);

/// Initialize an empty, insertable interner sized for roughly `capacity` names.
pub fn initCapacity(gpa: Allocator, capacity: usize) Allocator.Error!SerialStringInterner {
    const target = ((capacity * 5) / 4) + 1;
    const index_cap = std.math.ceilPowerOfTwo(usize, @max(target, initial_index_capacity)) catch initial_index_capacity;

    var self = SerialStringInterner{
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
pub fn deinit(self: *SerialStringInterner, gpa: Allocator) void {
    if (!self.supports_inserts) return;
    self.bytes.deinit(gpa);
    self.ranges.deinit(gpa);
    self.index.deinit(gpa);
}

/// Number of interned strings.
pub fn count(self: *const SerialStringInterner) u32 {
    return @intCast(self.ranges.items.items.len);
}

fn textAt(self: *const SerialStringInterner, id: u32) []const u8 {
    const r = self.ranges.items.items[id];
    return self.bytes.items.items[r.start .. r.start + r.len];
}

/// Text for a serial id.
pub fn getText(self: *const SerialStringInterner, id: u32) []const u8 {
    return self.textAt(id);
}

/// Id encoding for the shared `InternedBytes`: dense serial ids (0, 1, 2, …)
/// stored via the `ranges` array, with hash-table cells holding `id + 1` (so 0 is
/// the empty-slot sentinel).
const Policy = struct {
    pub const Id = u32;
    pub const Cell = u32;
    pub const empty_cell: Cell = 0;
    pub const initial_index_capacity: usize = SerialStringInterner.initial_index_capacity;

    pub fn count(self: *const SerialStringInterner) u32 {
        return @intCast(self.ranges.items.items.len);
    }
    pub fn entryCount(self: *const SerialStringInterner, _: *const Index) u32 {
        return @intCast(self.ranges.items.items.len);
    }
    pub fn cellForId(id: Id) Cell {
        return id + 1;
    }
    pub fn idFromCell(cell: Cell) Id {
        return cell - 1;
    }
    pub fn textForId(self: *const SerialStringInterner, id: Id) []const u8 {
        return self.textAt(id);
    }
    pub fn appendEntry(self: *SerialStringInterner, gpa: Allocator, string: []const u8) Allocator.Error!Id {
        assertSupportsInserts(self.supports_inserts);
        const id: u32 = @intCast(self.ranges.items.items.len);
        const start: u32 = @intCast(self.bytes.items.items.len);
        _ = try self.bytes.appendSlice(gpa, string);
        _ = try self.ranges.append(gpa, .{ .start = start, .len = @intCast(string.len) });
        return id;
    }
    pub fn hash(string: []const u8) u64 {
        return InternedBytes.hash(string);
    }
};

fn assertSupportsInserts(supports_inserts: bool) void {
    if (supports_inserts) return;

    if (comptime builtin.mode == .Debug) {
        std.debug.panic("SerialStringInterner invariant violated: attempted to insert into frozen interner", .{});
    }
    unreachable;
}

/// Look up a string's serial id without modifying the interner. Safe on frozen
/// (deserialized) interners.
pub fn lookup(self: *const SerialStringInterner, string: []const u8) ?u32 {
    const index = Index.fromCells(self.index, self.count());
    return index.lookup(self, string);
}

/// Intern `string`, returning its serial id. Deduplicates: equal text always
/// returns the same id.
pub fn insert(self: *SerialStringInterner, gpa: Allocator, string: []const u8) Allocator.Error!u32 {
    var index = Index.fromCells(self.index, self.count());
    defer {
        self.index = index.cells;
    }
    return index.insert(self, gpa, string);
}

/// Re-open a deserialized interner for insertion by copying its data into fresh,
/// growable memory.
pub fn enableRuntimeInserts(self: *SerialStringInterner, gpa: Allocator) Allocator.Error!void {
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

/// Relocatable serialized form. Exactly 3 relocatable base pointers.
pub const Serialized = extern struct {
    bytes: SafeList(u8).Serialized,
    ranges: SafeList(Range).Serialized,
    index: SafeList(u32).Serialized,
    entry_count: u32,
    _padding: u32 = 0,

    pub fn serialize(
        self: *Serialized,
        interner: *const SerialStringInterner,
        gpa: Allocator,
        writer: *CompactWriter,
    ) Allocator.Error!void {
        try self.bytes.serialize(&interner.bytes, gpa, writer);
        try self.ranges.serialize(&interner.ranges, gpa, writer);
        try self.index.serialize(&interner.index, gpa, writer);
        self.entry_count = interner.count();
        self._padding = 0;
    }

    pub fn deserialize(self: *const Serialized, base: usize) SerialStringInterner {
        return .{
            .bytes = self.bytes.deserializeInto(base),
            .ranges = self.ranges.deserializeInto(base),
            .index = self.index.deserializeInto(base),
            .supports_inserts = false,
        };
    }
};

const testing = std.testing;

test "SerialStringInterner: serial ids, dedup, lookup, getText" {
    const gpa = testing.allocator;
    var it = try SerialStringInterner.initCapacity(gpa, 4);
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

test "SerialStringInterner: default-empty interner lazily initializes on first insert" {
    const gpa = testing.allocator;
    var it: SerialStringInterner = .{}; // no initCapacity — mirrors CanonicalNameStore.init's `.empty`
    defer it.deinit(gpa);

    try testing.expectEqual(@as(u32, 0), it.count());
    try testing.expectEqual(@as(?u32, null), it.lookup("List")); // lookup on empty table is safe

    try testing.expectEqual(@as(u32, 0), try it.insert(gpa, "List"));
    try testing.expectEqual(@as(u32, 1), try it.insert(gpa, "Dict"));
    try testing.expectEqual(@as(u32, 0), try it.insert(gpa, "List"));
    try testing.expectEqualStrings("Dict", it.getText(1));
    try testing.expectEqual(@as(?u32, 0), it.lookup("List"));
}

test "SerialStringInterner: grows past initial table capacity preserving ids/lookup" {
    const gpa = testing.allocator;
    var it = try SerialStringInterner.initCapacity(gpa, 2);
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

fn roundTrip(gpa: Allocator, src: *const SerialStringInterner) (Allocator.Error || error{BufferTooSmall})!struct { buffer: []align(16) u8, it: SerialStringInterner } {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const aa = arena.allocator();

    var writer = CompactWriter.init();
    const hdr = try writer.appendAlloc(aa, SerialStringInterner.Serialized);
    try hdr.serialize(src, aa, &writer);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    _ = try writer.writeToBuffer(buffer);

    const ser: *const SerialStringInterner.Serialized = @ptrCast(@alignCast(buffer.ptr));
    return .{ .buffer = buffer, .it = ser.deserialize(@intFromPtr(buffer.ptr)) };
}

test "SerialStringInterner: lookup + getText survive serialize/deserialize with no re-insert" {
    const gpa = testing.allocator;
    var it = try SerialStringInterner.initCapacity(gpa, 8);
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

test "SerialStringInterner: enableRuntimeInserts copies frozen data and permits insertion" {
    const gpa = testing.allocator;

    var it = try SerialStringInterner.initCapacity(gpa, 4);
    defer it.deinit(gpa);

    const list = try it.insert(gpa, "List");

    var rt = try roundTrip(gpa, &it);
    defer gpa.free(rt.buffer);

    try rt.it.enableRuntimeInserts(gpa);
    defer rt.it.deinit(gpa);

    try testing.expect(rt.it.supports_inserts);
    try testing.expectEqual(@as(?u32, list), rt.it.lookup("List"));
    const dict = try rt.it.insert(gpa, "Dict");
    try testing.expectEqual(@as(u32, 1), dict);
    try testing.expectEqual(@as(?u32, dict), rt.it.lookup("Dict"));
    try testing.expectEqualStrings("Dict", rt.it.getText(dict));
}

fn nonEmptyBasePointers(it: *const SerialStringInterner) usize {
    var n: usize = 0;
    if (it.bytes.items.capacity != 0) n += 1;
    if (it.ranges.items.capacity != 0) n += 1;
    if (it.index.items.capacity != 0) n += 1;
    return n;
}

test "SerialStringInterner: relocation fixup count is constant in number of names" {
    const gpa = testing.allocator;

    var small = try SerialStringInterner.initCapacity(gpa, 2);
    defer small.deinit(gpa);
    _ = try small.insert(gpa, "List");

    var large = try SerialStringInterner.initCapacity(gpa, 8);
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
