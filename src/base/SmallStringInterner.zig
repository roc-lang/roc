//! An interner for short and likely repeated strings in in a Roc file.
//!
//! This interner deduplicates its string values because they are
//! expected to be small and often repeated since they tend to represent
//! the same value being referenced in many places. The indices assigned
//! to each interned string are serial, starting at 1, meaning they can be
//! used for arrays with values corresponding 1-to-1 to interned values,
//! e.g. regions.

const std = @import("std");
const invariant = @import("invariant.zig").invariant;
const builtin = @import("builtin");
const collections = @import("collections");

const CompactWriter = collections.CompactWriter;
const InternedBytes = @import("InternedBytes.zig");

const SmallStringInterner = @This();

/// The text of every interned string, concatenated in insertion order.
bytes: collections.SafeList(u8) = .{},
/// End offset in `bytes` of each string, by index. Slot 0 holds the 0 that
/// the first string starts at, so string `i` spans `ends[i - 1]..ends[i]`.
ends: collections.SafeList(u32) = .{},
/// A hash table using linear probing to map hashes to string indices.
/// A value of .unused (0) indicates an empty slot.
index: collections.SafeList(Idx) = .{},
/// The high hash byte of the string in each occupied `index` slot, so a probe
/// compares text only when this byte matches.
fingerprints: collections.SafeList(u8) = .{},
/// The current number of entries in the hash table.
entry_count: u32 = 0,
/// Flag to track whether this interner supports inserts (true) or is deserialized (false).
/// Deserialized interners have memory owned by the deserialization buffer, so:
/// - deinit() must NOT free memory (would double-free)
/// - insert operations are invalid (buffer is immutable)
supports_inserts: bool = true,

/// A unique index for a deduped string in this interner.
pub const Idx = enum(u32) {
    unused = 0,
    _,
};

/// Initialize a `SmallStringInterner` with the specified capacity.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!SmallStringInterner {
    // TODO: tune this. Rough assumption that average small string is 4 bytes.
    const bytes_per_string = 4;

    // Calculate hash table size based on load factor of 80% (multiply by 5, divide by 4)
    const hash_table_size = @as(u32, @intCast(((capacity * 5) / 4) + 1));
    // Round up to next power of 2 for better modulo performance
    const hash_table_capacity = std.math.ceilPowerOfTwo(u32, hash_table_size) catch hash_table_size;

    var bytes = try collections.SafeList(u8).initCapacity(gpa, capacity * bytes_per_string);
    errdefer bytes.deinit(gpa);

    var ends = try collections.SafeList(u32).initCapacity(gpa, capacity + 1);
    errdefer ends.deinit(gpa);
    _ = ends.appendAssumeCapacity(0);

    var index = try collections.SafeList(Idx).initCapacity(gpa, hash_table_capacity);
    errdefer index.deinit(gpa);
    index.items.items.len = hash_table_capacity;
    @memset(index.items.items, .unused);

    var fingerprints = try collections.SafeList(u8).initCapacity(gpa, hash_table_capacity);
    errdefer fingerprints.deinit(gpa);
    fingerprints.items.items.len = hash_table_capacity;
    @memset(fingerprints.items.items, 0);

    return .{
        .bytes = bytes,
        .ends = ends,
        .index = index,
        .fingerprints = fingerprints,
        .entry_count = 0,
    };
}

const Index = InternedBytes.Index(Policy);

/// Id encoding for the shared `InternedBytes`: the public `Idx` is the
/// string's serial index, stored directly in each hash-table cell
/// (`Idx.unused` == 0 marks an empty slot; serial indices start at 1).
const Policy = struct {
    pub const Id = Idx;
    pub const Cell = Idx;
    pub const empty_cell: Cell = .unused;
    // `initCapacity` always pre-allocates the table, so the core's lazy-alloc path is
    // never taken; this only matters for a default-`.{}` interner, which this type
    // does not support (`ends` must hold the leading 0).
    pub const initial_index_capacity: usize = 16;
    pub const use_fingerprints = true;

    pub fn count(self: *const SmallStringInterner) u32 {
        return self.entry_count;
    }
    pub fn cellForId(id: Id) Cell {
        return id;
    }
    pub fn idFromCell(cell: Cell) Id {
        return cell;
    }
    pub fn textForId(self: *const SmallStringInterner, id: Id) []const u8 {
        return self.getText(id);
    }
    pub fn appendEntry(self: *SmallStringInterner, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Id {
        assertSupportsInserts(self.supports_inserts);
        // Every offset in `ends` is a u32, so the text past that range has
        // exhausted the representable memory.
        const start = self.bytes.items.items.len;
        const end = std.math.cast(u32, start + string.len) orelse return error.OutOfMemory;
        const new_idx: Idx = @fromBackingInt(std.math.cast(u32, self.ends.items.items.len) orelse return error.OutOfMemory);
        // Reserve both columns first, so a failed allocation leaves no text
        // that the next entry's span would absorb.
        try self.bytes.items.ensureUnusedCapacity(gpa, string.len);
        try self.ends.items.ensureUnusedCapacity(gpa, 1);
        self.bytes.items.appendSliceAssumeCapacity(string);
        _ = self.ends.appendAssumeCapacity(end);
        self.entry_count += 1;
        return new_idx;
    }
    pub fn hash(string: []const u8) u64 {
        return InternedBytes.hash(string);
    }
};

fn assertSupportsInserts(supports_inserts: bool) void {
    if (supports_inserts) return;

    if (comptime builtin.mode == .debug) {
        invariant("SmallStringInterner invariant violated: attempted to insert into frozen interner", .{});
    }
    unreachable;
}

/// Enable inserts on a deserialized interner for runtime use.
/// Normally deserialized interners are read-only, but the interpreter needs to
/// insert new identifiers at runtime for type name translation and method lookup.
/// This copies the deserialized data into newly allocated memory that can be grown.
/// Call this after deserialization but before using the interner for runtime operations.
pub fn enableRuntimeInserts(self: *SmallStringInterner, gpa: std.mem.Allocator) std.mem.Allocator.Error!void {
    // Skip if already supports inserts (avoid memory leak from double-call)
    if (self.supports_inserts) {
        return;
    }
    self.* = try self.clone(gpa);
}

/// Free all memory consumed by this interner.
/// Will invalidate all slices referencing the interner.
/// NOTE: For deserialized interners, this is a no-op since memory is owned by the deserialization buffer.
pub fn deinit(self: *SmallStringInterner, gpa: std.mem.Allocator) void {
    // Deserialized interners have supports_inserts = false.
    // Their memory is owned by the deserialization buffer, so we must not free it.
    if (!self.supports_inserts) {
        return;
    }
    self.bytes.deinit(gpa);
    self.ends.deinit(gpa);
    self.index.deinit(gpa);
    self.fingerprints.deinit(gpa);
}

fn cloneColumn(comptime T: type, gpa: std.mem.Allocator, column: *const collections.SafeList(T)) std.mem.Allocator.Error!collections.SafeList(T) {
    var copy = try collections.SafeList(T).initCapacity(gpa, column.items.items.len);
    copy.items.appendSliceAssumeCapacity(column.items.items);
    return copy;
}

/// Clone this interner into fresh owned memory that supports inserts.
pub fn clone(self: *const SmallStringInterner, gpa: std.mem.Allocator) std.mem.Allocator.Error!SmallStringInterner {
    var bytes = try cloneColumn(u8, gpa, &self.bytes);
    errdefer bytes.deinit(gpa);
    var ends = try cloneColumn(u32, gpa, &self.ends);
    errdefer ends.deinit(gpa);
    var index = try cloneColumn(Idx, gpa, &self.index);
    errdefer index.deinit(gpa);
    const fingerprints = try cloneColumn(u8, gpa, &self.fingerprints);

    return .{
        .bytes = bytes,
        .ends = ends,
        .index = index,
        .fingerprints = fingerprints,
        .entry_count = self.entry_count,
        .supports_inserts = true,
    };
}

fn probeIndex(self: *const SmallStringInterner) Index {
    return Index.fromParts(self.index, self.fingerprints, self.entry_count);
}

/// Find a string in the hash table using linear probing.
/// Returns the Idx if found, or the slot index where it should be inserted if not found.
pub fn findStringOrSlot(self: *const SmallStringInterner, string: []const u8) InternedBytes.FindResult(Idx) {
    return self.probeIndex().findStringOrSlot(self, Policy.hash(string), string);
}

/// Add a string to this interner, returning a unique, serial index.
pub fn insert(self: *SmallStringInterner, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
    var index = self.probeIndex();
    defer {
        self.index = index.cells;
        self.fingerprints = index.fingerprints;
        self.entry_count = index.len;
    }
    return index.insert(self, gpa, string);
}

/// Check if a string is already interned in this interner, used for generating unique names.
pub fn contains(self: *const SmallStringInterner, string: []const u8) bool {
    return self.probeIndex().contains(self, string);
}

/// Look up a string in this interner and return its index if found.
/// Unlike insert, this never modifies the interner (no resize, no insertion).
/// Useful for deserialized interners that cannot be grown.
pub fn lookup(self: *const SmallStringInterner, string: []const u8) ?Idx {
    return self.probeIndex().lookup(self, string);
}

/// Whether `idx` refers to an entry within this interner's data. Index 0 is
/// the reserved "unused" sentinel, and any index past the entry count was
/// never produced by this interner.
pub fn isInBounds(self: *const SmallStringInterner, idx: Idx) bool {
    const serial = @backingInt(idx);
    return serial != 0 and serial <= self.entry_count;
}

/// Get a reference to the text for an interned string.
pub fn getText(self: *const SmallStringInterner, idx: Idx) []u8 {
    const serial: usize = @backingInt(idx);
    const ends = self.ends.items.items;
    return self.bytes.items.items[ends[serial - 1]..ends[serial]];
}

/// Serialized representation of a SmallStringInterner
/// Uses extern struct to guarantee consistent field layout across optimization levels.
pub const Serialized = extern struct {
    bytes: collections.SafeList(u8).Serialized,
    ends: collections.SafeList(u32).Serialized,
    /// The probe table (runtime field `index`).
    hash_table: collections.SafeList(Idx).Serialized,
    fingerprints: collections.SafeList(u8).Serialized,
    entry_count: u32,
    /// Padding to maintain struct alignment
    _padding: u32 = 0,

    /// Serialize a SmallStringInterner into this Serialized struct, appending data to the writer
    pub fn serialize(
        self: *Serialized,
        interner: *const SmallStringInterner,
        allocator: std.mem.Allocator,
        writer: *CompactWriter,
    ) std.mem.Allocator.Error!void {
        try self.bytes.serialize(&interner.bytes, allocator, writer);
        try self.ends.serialize(&interner.ends, allocator, writer);
        try self.hash_table.serialize(&interner.index, allocator, writer);
        try self.fingerprints.serialize(&interner.fingerprints, allocator, writer);
        self.entry_count = interner.entry_count;
        self._padding = 0;
    }

    /// Deserialize into a SmallStringInterner value (no in-place modification of cache buffer).
    /// The base parameter is the base address of the serialized buffer in memory.
    pub fn deserializeInto(self: *const Serialized, base: usize) SmallStringInterner {
        return SmallStringInterner{
            .bytes = self.bytes.deserializeInto(base),
            .ends = self.ends.deserializeInto(base),
            .index = self.hash_table.deserializeInto(base),
            .fingerprints = self.fingerprints.deserializeInto(base),
            .entry_count = self.entry_count,
            // Mark as not supporting inserts - deserialized interners have memory owned by the buffer
            .supports_inserts = false,
        };
    }
};

test "SmallStringInterner empty CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create an empty SmallStringInterner with proper initialization
    var original = try SmallStringInterner.initCapacity(gpa, 0);
    defer original.deinit(gpa);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_empty_interner.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter with arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, SmallStringInterner.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize - empty interner should still work
    // The SmallStringInterner struct is at the beginning of the buffer
    const deserialized_serialized: *const SmallStringInterner.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized = &deserialized_value;

    // Verify empty - no text, and `ends` holds only the leading 0
    try std.testing.expectEqual(@as(usize, 0), deserialized.bytes.len());
    try std.testing.expectEqual(@as(usize, 1), deserialized.ends.len());
    try std.testing.expectEqual(@as(u32, 0), deserialized.entry_count);
}

test "SmallStringInterner basic CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create an interner with some strings
    var original = try SmallStringInterner.initCapacity(gpa, 10);
    defer original.deinit(gpa);

    // Insert test strings
    const test_strings = [_][]const u8{
        "hello",
        "world",
        "foo",
        "bar",
        "baz",
        "test string",
        "another test",
        "", // empty string
        "duplicate",
        "duplicate", // Should reuse the same index
    };

    var indices = std.ArrayList(SmallStringInterner.Idx).empty;
    defer indices.deinit(gpa);

    for (test_strings) |str| {
        const idx = try original.insert(gpa, str);
        try indices.append(gpa, idx);
    }

    // Verify duplicate detection worked
    try std.testing.expectEqual(indices.items[8], indices.items[9]);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_basic_interner.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter with arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, SmallStringInterner.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_serialized: *const SmallStringInterner.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized = &deserialized_value;

    // Verify all strings are accessible and correct
    for (test_strings[0..9], 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.getText(idx);
        try std.testing.expectEqualStrings(expected_str, actual_str);
    }

    // Verify the entry count is preserved after deserialization
    try std.testing.expectEqual(@as(u32, 9), deserialized.entry_count);
}

test "SmallStringInterner with populated hashmap CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create interner and populate it
    var original = try SmallStringInterner.initCapacity(gpa, 20);
    defer original.deinit(gpa);

    // Insert many strings to ensure the hash map is well populated
    const test_data = [_]struct { str: []const u8, expected_idx: u32 }{
        .{ .str = "first", .expected_idx = 1 }, // Serial indices start at 1
        .{ .str = "second", .expected_idx = 2 },
        .{ .str = "third", .expected_idx = 3 },
        .{ .str = "first", .expected_idx = 1 }, // duplicate
        .{ .str = "fourth", .expected_idx = 4 },
        .{ .str = "fifth", .expected_idx = 5 },
        .{ .str = "second", .expected_idx = 2 }, // duplicate
        .{ .str = "sixth", .expected_idx = 6 },
        .{ .str = "seventh", .expected_idx = 7 },
        .{ .str = "eighth", .expected_idx = 8 },
    };

    for (test_data) |data| {
        const idx = try original.insert(gpa, data.str);
        try std.testing.expectEqual(@as(u32, data.expected_idx), @backingInt(idx));
    }

    // Verify the hash table is populated
    try std.testing.expect(original.entry_count > 0);
    const original_entry_count = original.entry_count;

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_hashmap_interner.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, SmallStringInterner.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_serialized: *const SmallStringInterner.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized = &deserialized_value;

    // Verify the entry count is preserved after deserialization
    try std.testing.expectEqual(original_entry_count, deserialized.entry_count);

    // But all strings should still be accessible
    // Note: Index 0 is reserved, so serial indices start at 1
    try std.testing.expectEqualStrings("first", deserialized.getText(@fromBackingInt(@intCast(1))));
    try std.testing.expectEqualStrings("second", deserialized.getText(@fromBackingInt(@intCast(2))));
    try std.testing.expectEqualStrings("third", deserialized.getText(@fromBackingInt(@intCast(3))));
    try std.testing.expectEqualStrings("fourth", deserialized.getText(@fromBackingInt(@intCast(4))));
    try std.testing.expectEqualStrings("fifth", deserialized.getText(@fromBackingInt(@intCast(5))));
    try std.testing.expectEqualStrings("sixth", deserialized.getText(@fromBackingInt(@intCast(6))));
    try std.testing.expectEqualStrings("seventh", deserialized.getText(@fromBackingInt(@intCast(7))));
    try std.testing.expectEqualStrings("eighth", deserialized.getText(@fromBackingInt(@intCast(8))));

    // Verify the original had entries
    try std.testing.expect(original_entry_count > 0);
}

test "SmallStringInterner CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create and populate interner
    var original = try SmallStringInterner.initCapacity(gpa, 5);
    defer original.deinit(gpa);

    const idx1 = try original.insert(gpa, "test1");
    const idx2 = try original.insert(gpa, "test2");
    try std.testing.expect(@backingInt(idx1) < @backingInt(idx2));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_frozen_interner.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, SmallStringInterner.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_serialized: *const SmallStringInterner.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized = &deserialized_value;

    // Verify strings are still accessible
    // Note: Index 0 is reserved for the unused marker, so strings start at index 1
    try std.testing.expectEqualStrings("test1", deserialized.getText(@fromBackingInt(@intCast(1))));
    try std.testing.expectEqualStrings("test2", deserialized.getText(@fromBackingInt(@intCast(2))));
}

test "SmallStringInterner edge cases CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Test with strings of various lengths and special characters
    var original = try SmallStringInterner.initCapacity(gpa, 15);
    defer original.deinit(gpa);

    const edge_cases = [_][]const u8{
        "", // empty string
        "a", // single char
        "ab", // two chars
        "hello world with spaces",
        "special\ncharacters\ttabs",
        "unicode: 你好世界", // UTF-8
        "very_long_string_that_is_much_longer_than_average_to_test_capacity_handling",
        "\x00embedded", // string starting with a zero byte
        "end_with_space ",
        " start_with_space",
    };

    var indices = std.ArrayList(SmallStringInterner.Idx).empty;
    defer indices.deinit(gpa);

    for (edge_cases) |str| {
        const idx = try original.insert(gpa, str);
        try indices.append(gpa, idx);
    }

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_edge_interner.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, SmallStringInterner.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_serialized: *const SmallStringInterner.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized = &deserialized_value;

    // Verify all edge cases
    for (edge_cases, 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.getText(idx);

        try std.testing.expectEqualStrings(expected_str, actual_str);
    }
}

fn roundTripSerialized(gpa: std.mem.Allocator, src: *const SmallStringInterner) (std.mem.Allocator.Error || error{BufferTooSmall})!struct { buffer: []align(16) u8, interner: SmallStringInterner } {
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    const serialized = try writer.appendAlloc(arena_allocator, SmallStringInterner.Serialized);
    try serialized.serialize(src, arena_allocator, &writer);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    _ = try writer.writeToBuffer(buffer);

    const serialized_ptr: *const SmallStringInterner.Serialized = @ptrCast(@alignCast(buffer.ptr));
    return .{ .buffer = buffer, .interner = serialized_ptr.deserializeInto(@intFromPtr(buffer.ptr)) };
}

test "SmallStringInterner lookup survives deserialize with no rebuild" {
    const gpa = std.testing.allocator;

    var original = try SmallStringInterner.initCapacity(gpa, 8);
    defer original.deinit(gpa);

    const alpha = try original.insert(gpa, "alpha");
    const beta = try original.insert(gpa, "beta");
    const gamma = try original.insert(gpa, "gamma");

    var roundtrip = try roundTripSerialized(gpa, &original);
    defer gpa.free(roundtrip.buffer);

    try std.testing.expect(!roundtrip.interner.supports_inserts);
    try std.testing.expectEqual(alpha, roundtrip.interner.lookup("alpha").?);
    try std.testing.expectEqual(beta, roundtrip.interner.lookup("beta").?);
    try std.testing.expectEqual(gamma, roundtrip.interner.lookup("gamma").?);
    try std.testing.expectEqual(@as(?Idx, null), roundtrip.interner.lookup("delta"));
}

test "SmallStringInterner enableRuntimeInserts copies frozen data and permits insertion" {
    const gpa = std.testing.allocator;

    var original = try SmallStringInterner.initCapacity(gpa, 4);
    defer original.deinit(gpa);

    const alpha = try original.insert(gpa, "alpha");

    var roundtrip = try roundTripSerialized(gpa, &original);
    defer gpa.free(roundtrip.buffer);

    try roundtrip.interner.enableRuntimeInserts(gpa);
    defer roundtrip.interner.deinit(gpa);

    try std.testing.expect(roundtrip.interner.supports_inserts);
    try std.testing.expectEqual(alpha, roundtrip.interner.lookup("alpha").?);

    const beta = try roundtrip.interner.insert(gpa, "beta");
    try std.testing.expectEqual(beta, roundtrip.interner.lookup("beta").?);
    try std.testing.expectEqualStrings("beta", roundtrip.interner.getText(beta));
}

test "SmallStringInterner multiple interners CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create multiple interners to test alignment and offset handling
    var interner1 = try SmallStringInterner.initCapacity(gpa, 5);
    defer interner1.deinit(gpa);

    var interner2 = try SmallStringInterner.initCapacity(gpa, 5);
    defer interner2.deinit(gpa);

    var interner3 = try SmallStringInterner.initCapacity(gpa, 5);
    defer interner3.deinit(gpa);

    // Populate with different strings
    const idx1_1 = try interner1.insert(gpa, "interner1_string1");
    const idx1_2 = try interner1.insert(gpa, "interner1_string2");

    const idx2_1 = try interner2.insert(gpa, "interner2_string1");
    const idx2_2 = try interner2.insert(gpa, "interner2_string2");
    const idx2_3 = try interner2.insert(gpa, "interner2_string3");

    const idx3_1 = try interner3.insert(gpa, "interner3_string1");

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_multiple_interners.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    // Serialize all three into one buffer, recording where each struct starts.
    // appendAlloc pads to the struct's alignment before writing it, so pad first
    // and then record the offset.
    try writer.padToAlignment(arena_allocator, @alignOf(SmallStringInterner));
    const offset1 = writer.total_bytes;
    try (try writer.appendAlloc(arena_allocator, SmallStringInterner.Serialized)).serialize(&interner1, arena_allocator, &writer);

    try writer.padToAlignment(arena_allocator, @alignOf(SmallStringInterner));
    const offset2 = writer.total_bytes;
    try (try writer.appendAlloc(arena_allocator, SmallStringInterner.Serialized)).serialize(&interner2, arena_allocator, &writer);

    try writer.padToAlignment(arena_allocator, @alignOf(SmallStringInterner));
    const offset3 = writer.total_bytes;
    try (try writer.appendAlloc(arena_allocator, SmallStringInterner.Serialized)).serialize(&interner3, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize all three; every serialized pointer is an offset from the
    // start of the whole buffer, so each root relocates by the same base address.
    const deserialized1_serialized: *const SmallStringInterner.Serialized = @ptrCast(@alignCast(buffer.ptr + offset1));
    var deserialized1_value = deserialized1_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized1 = &deserialized1_value;

    const deserialized2_serialized: *const SmallStringInterner.Serialized = @ptrCast(@alignCast(buffer.ptr + offset2));
    var deserialized2_value = deserialized2_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized2 = &deserialized2_value;

    const deserialized3_serialized: *const SmallStringInterner.Serialized = @ptrCast(@alignCast(buffer.ptr + offset3));
    var deserialized3_value = deserialized3_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized3 = &deserialized3_value;

    // Verify interner 1
    try std.testing.expectEqualStrings("interner1_string1", deserialized1.getText(idx1_1));
    try std.testing.expectEqualStrings("interner1_string2", deserialized1.getText(idx1_2));
    try std.testing.expectEqual(@as(u32, 2), deserialized1.entry_count);

    // Verify interner 2
    try std.testing.expectEqualStrings("interner2_string1", deserialized2.getText(idx2_1));
    try std.testing.expectEqualStrings("interner2_string2", deserialized2.getText(idx2_2));
    try std.testing.expectEqualStrings("interner2_string3", deserialized2.getText(idx2_3));
    try std.testing.expectEqual(@as(u32, 3), deserialized2.entry_count);

    // Verify interner 3
    try std.testing.expectEqualStrings("interner3_string1", deserialized3.getText(idx3_1));
    try std.testing.expectEqual(@as(u32, 1), deserialized3.entry_count);
}
