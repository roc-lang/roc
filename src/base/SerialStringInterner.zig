//! A serialization-friendly string interner that assigns **dense serial ids**.
//!
//! It backs `CanonicalNameStore`'s name kinds with relocatable storage. Like
//! `base.SmallStringInterner` it stores all text in one flat byte buffer and an
//! open-addressing hash table as a flat array of integers—both
//! `SafeList`-backed, so the whole interner relocates with a **constant** number
//! of base-pointer fixups (3), independent of how many names it holds.
//!
//! Unlike `SmallStringInterner` (whose id is a byte offset), this interner hands
//! out **serial** ids (0, 1, 2, … in insertion order) via a separate `ends`
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
//!
//! An interner other threads read while it grows is shared: every name
//! interned into it from then on goes through `insertShared`, which one
//! thread at a time runs under `writer`, while every reader reads without a
//! lock. Those names live in `growth`, beside a copy of the names interned
//! before; `bytes`, `ends` and `index` keep exactly the names interned before
//! the first shared insert, which are the names it serializes.

const std = @import("std");
const invariant = @import("invariant.zig").invariant;
const collections = @import("collections");

const Allocator = std.mem.Allocator;
const CompactWriter = collections.CompactWriter;
const SafeList = collections.SafeList;
const InternedBytes = @import("InternedBytes.zig");

const SerialStringInterner = @This();

/// All interned text, concatenated in id order. No separators needed; `ends`
/// holds bounds.
bytes: SafeList(u8) = .{},
/// Serial id -> end offset of its text in `bytes`. A string's text starts
/// where the previous id's ends, or at 0 for id 0. `ends.len()` is the
/// number of interned strings.
ends: SafeList(u32) = .{},
/// Open-addressing (linear-probing) hash table: slot -> (serial id + 1).
/// A value of 0 means the slot is empty. Stored as a flat array of u32 so it
/// relocates with a single base-pointer fixup.
index: SafeList(u32) = .{},
/// True while this interner owns growable memory; false after deserialize
/// (memory then points into the serialization buffer and must not be freed/grown).
supports_inserts: bool = true,
/// Every name, once a name was interned while other threads read the
/// interner (`insertShared`); null before. Every read goes through it once it
/// is set.
growth: std.atomic.Value(?*Growth) = .init(null),
/// Held by the one thread at a time that interns into a shared interner.
writer: std.atomic.Mutex = .unlocked,

const initial_index_capacity: usize = 16;

/// The names of a shared interner. Its columns grow while other threads read
/// them (`collections.PublishedList`). A name's text and end are published
/// before its probing cell, so a reader that finds the cell reads the name.
/// A grown probing table replaces the current one and the old one stays
/// alive, so a reader probing it never reads freed memory; it misses only
/// names interned after it loaded the table.
const Growth = struct {
    bytes: collections.PublishedList(u8),
    ends: collections.PublishedList(u32),
    table: std.atomic.Value(*const Table),
    /// Every table `table` has named, freed with the interner.
    tables: std.ArrayList(*Table) = .empty,

    const Table = struct { cells: []u32 };

    fn textAt(self: *const Growth, id: u32) []const u8 {
        const ends = self.ends.items();
        const start = if (id == 0) 0 else ends[id - 1];
        return self.bytes.items()[start..ends[id]];
    }

    fn lookup(self: *const Growth, string: []const u8) ?u32 {
        const table = self.table.load(.acquire);
        const mask = table.cells.len - 1;
        var slot: usize = @intCast(InternedBytes.hash(string) & mask);
        while (true) {
            const cell = @atomicLoad(u32, &table.cells[slot], .acquire);
            if (cell == 0) return null;
            if (std.mem.eql(u8, string, self.textAt(cell - 1))) return cell - 1;
            slot = (slot + 1) & mask;
        }
    }

    /// Place `id` in `cells`, which hold no name with its text. Only the
    /// writer calls this.
    fn place(cells: []u32, text: []const u8, id: u32) void {
        const mask = cells.len - 1;
        var slot: usize = @intCast(InternedBytes.hash(text) & mask);
        while (cells[slot] != 0) slot = (slot + 1) & mask;
        @atomicStore(u32, &cells[slot], id + 1, .release);
    }

    /// Publish a table of `size` cells holding the first `names` names.
    fn publishTable(self: *Growth, gpa: Allocator, size: usize, names: u32) Allocator.Error!void {
        try self.tables.ensureUnusedCapacity(gpa, 1);
        const table = try gpa.create(Table);
        errdefer gpa.destroy(table);
        const cells = try gpa.alloc(u32, size);
        @memset(cells, 0);
        for (0..names) |id| place(cells, self.textAt(@intCast(id)), @intCast(id));
        table.* = .{ .cells = cells };
        self.tables.appendAssumeCapacity(table);
        self.table.store(table, .release);
    }

    /// Intern `string`, which the growth does not hold. Only the writer
    /// calls this.
    fn append(self: *Growth, gpa: Allocator, string: []const u8) Allocator.Error!u32 {
        const id = std.math.cast(u32, self.ends.count()) orelse return error.OutOfMemory;
        if (id == std.math.maxInt(u32)) return error.OutOfMemory;
        const start = std.math.cast(u32, self.bytes.count()) orelse return error.OutOfMemory;
        const len = std.math.cast(u32, string.len) orelse return error.OutOfMemory;
        const end = std.math.add(u32, start, len) catch return error.OutOfMemory;
        const size = self.table.load(.monotonic).cells.len;
        if (InternedBytes.wouldExceedMaxLoad(@as(usize, id) + 1, size)) try self.publishTable(gpa, size * 2, id);
        try self.bytes.own(gpa);
        try self.ends.own(gpa);
        try self.ends.ensureUnusedCapacity(gpa, 1);
        try self.bytes.appendSlice(gpa, string);
        self.ends.appendAssumeCapacity(end);
        place(@constCast(self.table.load(.monotonic).cells), string, id);
        return id;
    }

    fn destroy(self: *Growth, gpa: Allocator) void {
        self.bytes.deinit(gpa);
        self.ends.deinit(gpa);
        for (self.tables.items) |table| {
            gpa.free(table.cells);
            gpa.destroy(table);
        }
        self.tables.deinit(gpa);
        gpa.destroy(self);
    }
};
const Index = InternedBytes.Index(Policy);

/// Initialize an empty, insertable interner sized for roughly `capacity` names.
pub fn initCapacity(gpa: Allocator, capacity: usize) Allocator.Error!SerialStringInterner {
    const target = ((capacity * 5) / 4) + 1;
    const index_cap = std.math.ceilPowerOfTwo(usize, @max(target, initial_index_capacity)) catch initial_index_capacity;

    var self = SerialStringInterner{
        .bytes = .{},
        .ends = .{},
        .index = .{},
        .supports_inserts = true,
    };
    self.bytes = try SafeList(u8).initCapacity(gpa, capacity * 4);
    self.ends = try SafeList(u32).initCapacity(gpa, capacity);
    self.index = try SafeList(u32).initCapacity(gpa, index_cap);
    try self.index.items.ensureTotalCapacityPrecise(gpa, index_cap);
    self.index.items.items.len = index_cap;
    @memset(self.index.items.items, 0);
    return self;
}

/// Free owned memory. No-op for deserialized (frozen) interners, whose memory is
/// owned by the serialization buffer.
pub fn deinit(self: *SerialStringInterner, gpa: Allocator) void {
    if (self.growth.load(.monotonic)) |growth| growth.destroy(gpa);
    self.growth = .init(null);
    if (!self.supports_inserts) return;
    self.bytes.deinit(gpa);
    self.ends.deinit(gpa);
    self.index.deinit(gpa);
}

/// Number of interned strings.
pub fn count(self: *const SerialStringInterner) u32 {
    if (self.growth.load(.acquire)) |growth| return @intCast(growth.ends.count());
    return @intCast(self.ends.items.items.len);
}

/// Immutable append boundary for the durable id and byte arrays.
pub const EpochBoundary = struct {
    ids: u32,
    bytes: u32,
};

/// Capture the current durable boundary without exposing growable storage.
pub fn epochBoundary(self: *const SerialStringInterner) EpochBoundary {
    if (self.growth.load(.acquire)) |growth| return .{
        .ids = @intCast(growth.ends.count()),
        .bytes = @intCast(growth.bytes.count()),
    };
    return .{
        .ids = @intCast(self.ends.items.items.len),
        .bytes = @intCast(self.bytes.items.items.len),
    };
}

fn textAt(self: *const SerialStringInterner, id: u32) []const u8 {
    if (self.growth.load(.acquire)) |growth| return growth.textAt(id);
    const ends = self.ends.items.items;
    const start = if (id == 0) 0 else ends[id - 1];
    return self.bytes.items.items[start..ends[id]];
}

/// Text for a serial id.
pub fn getText(self: *const SerialStringInterner, id: u32) []const u8 {
    return self.textAt(id);
}

/// Id encoding for the shared `InternedBytes`: dense serial ids (0, 1, 2, …)
/// stored via the `ends` array, with hash-table cells holding `id + 1` (so 0 is
/// the empty-slot sentinel).
const Policy = struct {
    pub const Id = u32;
    pub const Cell = u32;
    pub const empty_cell: Cell = 0;
    pub const initial_index_capacity: usize = SerialStringInterner.initial_index_capacity;

    pub fn count(self: *const SerialStringInterner) u32 {
        return @intCast(self.ends.items.items.len);
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
        // Ids, byte offsets, and lengths are 32-bit, and index cells store
        // `id + 1`; an interner past that range has exhausted the
        // representable memory.
        const id = std.math.cast(u32, self.ends.items.items.len) orelse return error.OutOfMemory;
        if (id == std.math.maxInt(u32)) return error.OutOfMemory;
        const start = std.math.cast(u32, self.bytes.items.items.len) orelse return error.OutOfMemory;
        const len = std.math.cast(u32, string.len) orelse return error.OutOfMemory;
        const end = std.math.add(u32, start, len) catch return error.OutOfMemory;
        try self.ends.items.ensureUnusedCapacity(gpa, 1);
        _ = try self.bytes.appendSlice(gpa, string);
        _ = try self.ends.append(gpa, end);
        return id;
    }
    pub fn hash(string: []const u8) u64 {
        return InternedBytes.hash(string);
    }
};

fn assertSupportsInserts(supports_inserts: bool) void {
    if (supports_inserts) return;
    invariant("SerialStringInterner invariant violated: attempted to insert into frozen interner", .{});
}

/// Look up a string's serial id without modifying the interner. Safe on frozen
/// (deserialized) interners.
pub fn lookup(self: *const SerialStringInterner, string: []const u8) ?u32 {
    if (self.growth.load(.acquire)) |growth| return growth.lookup(string);
    const index = Index.fromCells(self.index, self.count());
    return index.lookup(self, string);
}

/// Intern `string`, returning its serial id. Deduplicates: equal text always
/// returns the same id.
pub fn insert(self: *SerialStringInterner, gpa: Allocator, string: []const u8) Allocator.Error!u32 {
    std.debug.assert(self.growth.load(.monotonic) == null);
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

    self.* = try self.clone(gpa);
}

/// Intern `string` into an interner other threads read while it grows.
/// One writer at a time interns, under `writer`; it never waits on anything
/// else while holding it.
pub fn insertShared(self: *SerialStringInterner, gpa: Allocator, string: []const u8) Allocator.Error!u32 {
    if (self.lookup(string)) |id| return id;
    while (!self.writer.tryLock()) std.atomic.spinLoopHint();
    defer self.writer.unlock();
    const growth = self.growth.load(.monotonic) orelse try self.startGrowth(gpa);
    if (growth.lookup(string)) |id| return id;
    return growth.append(gpa, string);
}

/// Begin the growth of a shared interner over the names it holds. Only the
/// writer calls this.
fn startGrowth(self: *SerialStringInterner, gpa: Allocator) Allocator.Error!*Growth {
    const growth = try gpa.create(Growth);
    errdefer gpa.destroy(growth);
    growth.* = .{
        .bytes = .borrowing(self.bytes.items.items),
        .ends = .borrowing(self.ends.items.items),
        .table = undefined,
    };
    errdefer growth.tables.deinit(gpa);
    const base_count: u32 = @intCast(self.ends.items.items.len);
    var size = @max(initial_index_capacity, self.index.items.items.len);
    while (InternedBytes.wouldExceedMaxLoad(@as(usize, base_count) + 1, size)) size *= 2;
    try growth.publishTable(gpa, size, base_count);
    self.growth.store(growth, .release);
    return growth;
}

/// Preserve serial IDs and the lookup index while taking independent ownership.
pub fn clone(self: *const SerialStringInterner, gpa: Allocator) Allocator.Error!SerialStringInterner {
    if (self.growth.load(.acquire)) |growth| {
        const total: u32 = @intCast(growth.ends.count());
        var result = try initCapacity(gpa, total);
        errdefer result.deinit(gpa);
        for (0..total) |id| _ = try result.insert(gpa, growth.textAt(@intCast(id)));
        return result;
    }
    var new_bytes = try SafeList(u8).initCapacity(gpa, self.bytes.items.items.len);
    errdefer new_bytes.deinit(gpa);
    _ = try new_bytes.appendSlice(gpa, self.bytes.items.items);
    var new_ends = try SafeList(u32).initCapacity(gpa, self.ends.items.items.len);
    errdefer new_ends.deinit(gpa);
    _ = try new_ends.appendSlice(gpa, self.ends.items.items);
    var new_index = try SafeList(u32).initCapacity(gpa, self.index.items.items.len);
    errdefer new_index.deinit(gpa);
    try new_index.items.ensureTotalCapacityPrecise(gpa, self.index.items.items.len);
    new_index.items.items.len = self.index.items.items.len;
    @memcpy(new_index.items.items, self.index.items.items);

    return .{
        .bytes = new_bytes,
        .ends = new_ends,
        .index = new_index,
        .supports_inserts = true,
    };
}

/// Relocatable serialized form. Exactly 3 relocatable base pointers.
pub const Serialized = extern struct {
    bytes: SafeList(u8).Serialized,
    ends: SafeList(u32).Serialized,
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
        try self.ends.serialize(&interner.ends, gpa, writer);
        try self.index.serialize(&interner.index, gpa, writer);
        self.entry_count = interner.count();
        self._padding = 0;
    }

    pub fn deserialize(self: *const Serialized, base: usize) SerialStringInterner {
        return .{
            .bytes = self.bytes.deserializeInto(base),
            .ends = self.ends.deserializeInto(base),
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
    var it: SerialStringInterner = .{}; // no initCapacity—mirrors CanonicalNameStore.init's `.empty`
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
    if (it.ends.items.capacity != 0) n += 1;
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

test "SerialStringInterner: shared inserts extend a loaded interner and keep its serialized names" {
    const gpa = testing.allocator;
    var it = try SerialStringInterner.initCapacity(gpa, 4);
    defer it.deinit(gpa);
    const list = try it.insert(gpa, "List");

    var rt = try roundTrip(gpa, &it);
    defer gpa.free(rt.buffer);
    defer rt.it.deinit(gpa);

    try testing.expectEqual(list, try rt.it.insertShared(gpa, "List"));
    var buf: [16]u8 = undefined;
    var i: u32 = 0;
    while (i < 200) : (i += 1) {
        try testing.expectEqual(i + 1, try rt.it.insertShared(gpa, try std.fmt.bufPrint(&buf, "n{d}", .{i})));
    }
    try testing.expectEqual(@as(u32, 201), rt.it.count());
    try testing.expectEqualStrings("List", rt.it.getText(list));
    i = 0;
    while (i < 200) : (i += 1) {
        const name = try std.fmt.bufPrint(&buf, "n{d}", .{i});
        try testing.expectEqual(@as(?u32, i + 1), rt.it.lookup(name));
        try testing.expectEqualStrings(name, rt.it.getText(i + 1));
    }
    // The serialized names are the ones interned before sharing.
    try testing.expectEqual(@as(usize, 1), rt.it.ends.items.items.len);

    var copy = try rt.it.clone(gpa);
    defer copy.deinit(gpa);
    try testing.expectEqual(@as(u32, 201), copy.count());
    try testing.expectEqual(@as(?u32, 200), copy.lookup("n199"));
}
