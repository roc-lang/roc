//! An interner for short and likely repeated strings in in a Roc file.
//!
//! This interner deduplicates its string values because they are
//! expected to be small and often repeated since they tend to represent
//! the same value being referenced in many places. The indices assigned
//! to each interned string are serial, meaning they can be used for
//! arrays with values corresponding 1-to-1 to interned values, e.g. regions.

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("collections");

const CompactWriter = collections.CompactWriter;
const InternedBytes = @import("InternedBytes.zig");

const SmallStringInterner = @This();

/// The raw underlying bytes for all strings.
/// Since strings are small, they are simply null terminated.
/// This uses only 1 byte to encode the size and is cheap to scan.
bytes: collections.SafeList(u8) = .{},
/// A hash table using linear probing to map hashes to string indices.
/// Each slot contains an Idx pointing to the start of a string in bytes.
/// A value of .unused (0) indicates an empty slot.
index: collections.SafeList(Idx) = .{},
/// The current number of entries in the hash table.
entry_count: u32 = 0,
/// Flag to track whether this interner supports inserts (true) or is deserialized (false).
/// Deserialized interners have memory owned by the deserialization buffer, so:
/// - deinit() must NOT free memory (would double-free)
/// - insert operations are invalid (buffer is immutable)
supports_inserts: bool = true,
/// Number of open rollback savepoints. This is runtime-only state and is never
/// serialized; `u16` uses padding already present in the runtime struct.
savepoint_depth: u16 = 0,

/// A unique index for a deduped string in this interner.
pub const Idx = enum(u32) {
    unused = 0,
    _,
};

/// An owned snapshot of the append boundary and exact probe-table cells.
/// Savepoints must be closed exactly once and in LIFO order on `owner`.
pub const Savepoint = struct {
    allocator: std.mem.Allocator,
    owner: *SmallStringInterner,
    depth: u16,
    bytes_len: usize,
    entry_count: u32,
    index_cells: []Idx,
};

fn assertAppendIndex(expected: usize, idx: collections.SafeList(u8).Idx) void {
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(@intFromEnum(idx) == expected);
    } else if (@intFromEnum(idx) != expected) {
        unreachable;
    }
}

fn assertAppendRange(expected_start: usize, expected_len: u32, range: collections.SafeList(u8).Range) void {
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(@intFromEnum(range.start) == expected_start);
        std.debug.assert(range.count == expected_len);
    } else if (@intFromEnum(range.start) != expected_start or range.count != expected_len) {
        unreachable;
    }
}

/// Initialize a `SmallStringInterner` with the specified capacity.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!SmallStringInterner {
    // TODO: tune this. Rough assumption that average small string is 4 bytes.
    const bytes_per_string = 4;

    // Calculate hash table size based on load factor of 80% (multiply by 5, divide by 4)
    const hash_table_size = @as(u32, @intCast(((capacity * 5) / 4) + 1));
    // Round up to next power of 2 for better modulo performance
    const hash_table_capacity = std.math.ceilPowerOfTwo(u32, hash_table_size) catch hash_table_size;

    var self = SmallStringInterner{
        .bytes = collections.SafeList(u8){},
        .index = collections.SafeList(Idx){},
        .entry_count = 0,
    };

    // Properly initialize the bytes array to ensure clean state
    self.bytes = try collections.SafeList(u8).initCapacity(gpa, capacity * bytes_per_string);

    // Start with at least one byte to ensure Idx.unused (0) never points to valid data
    {
        const expected_idx = self.bytes.items.items.len;
        const idx = try self.bytes.append(gpa, 0);
        assertAppendIndex(expected_idx, idx);
    }

    // Initialize hash table with all zeros (Idx.unused)
    self.index = try collections.SafeList(Idx).initCapacity(gpa, hash_table_capacity);
    try self.index.items.ensureTotalCapacityPrecise(gpa, hash_table_capacity);
    self.index.items.items.len = hash_table_capacity;
    @memset(self.index.items.items, .unused);

    return self;
}

const Index = InternedBytes.Index(Policy);

/// Id encoding for the shared `InternedBytes`: the public `Idx` IS the byte
/// offset of a null-terminated string in `bytes`, and is stored directly in each
/// hash-table cell (`Idx.unused` == 0 marks an empty slot—offset 0 is the reserved
/// leading byte, so it never names a real entry).
const Policy = struct {
    pub const Id = Idx;
    pub const Cell = Idx;
    pub const empty_cell: Cell = .unused;
    // `initCapacity` always pre-allocates the table, so the core's lazy-alloc path is
    // never taken; this only matters for a default-`.{}` interner, which this type
    // does not support (offset 0 must be the reserved byte).
    pub const initial_index_capacity: usize = 16;

    pub fn count(self: *const SmallStringInterner) u32 {
        return self.entry_count;
    }
    pub fn entryCount(self: *const SmallStringInterner, _: *const Index) u32 {
        return self.entry_count;
    }
    pub fn cellForId(id: Id) Cell {
        return id;
    }
    pub fn idFromCell(cell: Cell) Id {
        return cell;
    }
    pub fn textForId(self: *const SmallStringInterner, id: Id) []const u8 {
        return std.mem.sliceTo(self.bytes.items.items[@intFromEnum(id)..], 0);
    }
    pub fn appendEntry(self: *SmallStringInterner, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Id {
        assertSupportsInserts(self.supports_inserts);
        const append_len = std.math.add(usize, string.len, 1) catch return error.OutOfMemory;
        try self.bytes.items.ensureUnusedCapacity(gpa, append_len);
        const new_offset: Idx = @enumFromInt(self.bytes.len());
        {
            const expected_start = self.bytes.items.items.len;
            self.bytes.items.appendSliceAssumeCapacity(string);
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(self.bytes.items.items.len == expected_start + string.len);
            }
        }
        {
            const expected_idx = self.bytes.items.items.len;
            self.bytes.items.appendAssumeCapacity(0);
            if (comptime builtin.mode == .Debug) {
                std.debug.assert(self.bytes.items.items.len == expected_idx + 1);
            }
        }
        self.entry_count += 1;
        return new_offset;
    }
    pub fn hash(string: []const u8) u64 {
        return InternedBytes.hash(string);
    }
};

fn assertSupportsInserts(supports_inserts: bool) void {
    if (supports_inserts) return;

    if (comptime builtin.mode == .Debug) {
        std.debug.panic("SmallStringInterner invariant violated: attempted to insert into frozen interner", .{});
    }
    unreachable;
}

fn assertSavepointTop(self: *SmallStringInterner, savepoint: *const Savepoint) void {
    assertSupportsInserts(self.supports_inserts);
    if (savepoint.owner == self and savepoint.depth != 0 and self.savepoint_depth == savepoint.depth) {
        return;
    }

    if (comptime builtin.mode == .Debug) {
        std.debug.panic("SmallStringInterner invariant violated: savepoints must close in LIFO order on their owning interner", .{});
    }
    unreachable;
}

/// Snapshot the current logical state. Opening a savepoint allocates one exact
/// copy of the probe table; all inserts while it is open must use the same
/// allocator that owns this interner.
pub fn createSavepoint(self: *SmallStringInterner, gpa: std.mem.Allocator) std.mem.Allocator.Error!Savepoint {
    assertSupportsInserts(self.supports_inserts);
    const depth = std.math.add(u16, self.savepoint_depth, 1) catch return error.OutOfMemory;
    const index_cells = try gpa.dupe(Idx, self.index.items.items);

    self.savepoint_depth = depth;
    return .{
        .allocator = gpa,
        .owner = self,
        .depth = depth,
        .bytes_len = self.bytes.items.items.len,
        .entry_count = self.entry_count,
        .index_cells = index_cells,
    };
}

/// Close the newest savepoint while keeping all mutations made since it opened.
pub fn commitSavepoint(self: *SmallStringInterner, savepoint: *Savepoint) void {
    assertSavepointTop(self, savepoint);
    savepoint.allocator.free(savepoint.index_cells);
    self.savepoint_depth -= 1;
    savepoint.* = undefined;
}

/// Undo every insert since the newest savepoint opened. The durable byte store
/// is append-only, and the saved table already owns every pre-savepoint cell, so
/// rollback only shrinks and copies; it never allocates.
pub fn rollbackToSavepoint(self: *SmallStringInterner, savepoint: *Savepoint) void {
    assertSavepointTop(self, savepoint);
    const valid_suffix = savepoint.bytes_len <= self.bytes.items.items.len and
        savepoint.entry_count <= self.entry_count and
        savepoint.index_cells.len <= self.index.items.items.len;
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(valid_suffix);
    } else if (!valid_suffix) {
        unreachable;
    }

    self.bytes.items.shrinkRetainingCapacity(savepoint.bytes_len);
    self.entry_count = savepoint.entry_count;
    self.index.items.shrinkRetainingCapacity(savepoint.index_cells.len);
    @memcpy(self.index.items.items, savepoint.index_cells);

    savepoint.allocator.free(savepoint.index_cells);
    self.savepoint_depth -= 1;
    savepoint.* = undefined;
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

    // Copy the bytes array into newly allocated memory using proper ArrayList API
    const bytes_slice = self.bytes.items.items;
    var new_bytes = collections.SafeList(u8){};
    try new_bytes.items.ensureTotalCapacity(gpa, bytes_slice.len);
    try new_bytes.items.appendSlice(gpa, bytes_slice);
    self.bytes = new_bytes;

    // Copy the index array into newly allocated memory using proper ArrayList API
    const index_slice = self.index.items.items;
    var new_index = collections.SafeList(Idx){};
    try new_index.items.ensureTotalCapacity(gpa, index_slice.len);
    try new_index.items.appendSlice(gpa, index_slice);
    self.index = new_index;

    // Mark as supporting inserts so deinit will free the memory
    self.supports_inserts = true;
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
    self.index.deinit(gpa);
}

/// Clone this interner into fresh owned memory that supports inserts.
pub fn clone(self: *const SmallStringInterner, gpa: std.mem.Allocator) std.mem.Allocator.Error!SmallStringInterner {
    var bytes = collections.SafeList(u8){};
    errdefer bytes.deinit(gpa);
    try bytes.items.ensureTotalCapacity(gpa, self.bytes.items.items.len);
    try bytes.items.appendSlice(gpa, self.bytes.items.items);

    var index = collections.SafeList(Idx){};
    errdefer index.deinit(gpa);
    try index.items.ensureTotalCapacity(gpa, self.index.items.items.len);
    try index.items.appendSlice(gpa, self.index.items.items);

    return .{
        .bytes = bytes,
        .index = index,
        .entry_count = self.entry_count,
        .supports_inserts = true,
    };
}

/// Find a string in the hash table using linear probing.
/// Returns the Idx if found, or the slot index where it should be inserted if not found.
pub fn findStringOrSlot(self: *const SmallStringInterner, string: []const u8) InternedBytes.FindResult(Idx) {
    const index = Index.fromCells(self.index, self.entry_count);
    return index.findStringOrSlot(self, Policy.hash(string), string);
}

/// Add a string to this interner, returning a unique, serial index.
pub fn insert(self: *SmallStringInterner, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
    var index = Index.fromCells(self.index, self.entry_count);
    defer {
        self.index = index.cells;
        self.entry_count = index.len;
    }
    return index.insert(self, gpa, string);
}

/// Check if a string is already interned in this interner, used for generating unique names.
pub fn contains(self: *const SmallStringInterner, string: []const u8) bool {
    const index = Index.fromCells(self.index, self.entry_count);
    return index.contains(self, string);
}

/// Look up a string in this interner and return its index if found.
/// Unlike insert, this never modifies the interner (no resize, no insertion).
/// Useful for deserialized interners that cannot be grown.
pub fn lookup(self: *const SmallStringInterner, string: []const u8) ?Idx {
    const index = Index.fromCells(self.index, self.entry_count);
    return index.lookup(self, string);
}

/// Whether `idx` refers to an entry within this interner's data. Offset 0 is the
/// reserved "unused" sentinel, and any offset at or beyond the bytes buffer was
/// never produced by this interner—so an Idx from another store fails this.
pub fn isInBounds(self: *const SmallStringInterner, idx: Idx) bool {
    const offset = @intFromEnum(idx);
    return offset != 0 and offset < self.bytes.items.items.len;
}

/// Whether `idx` is exactly the start of a complete interned entry. Unlike
/// `isInBounds`, this rejects offsets into the middle of another string and
/// the offset of a terminating NUL.
pub fn validateExactIdx(self: *const SmallStringInterner, idx: Idx) bool {
    const raw = @intFromEnum(idx);
    const data = self.bytes.items.items;
    if (raw == 0 or raw >= data.len or data[raw - 1] != 0) return false;
    return std.mem.findScalar(u8, data[raw..], 0) != null;
}

/// Validate the complete byte-entry and open-addressed-index invariants before
/// any persisted identifier offset is dereferenced. Allocation-free so cache
/// loading can reject malformed tables without first trusting them.
pub fn validateSemanticState(self: *const SmallStringInterner) error{CorruptArtifact}!void {
    const data = self.bytes.items.items;
    const cells = self.index.items.items;
    if (data.len == 0 or data[0] != 0 or cells.len == 0 or
        !std.math.isPowerOfTwo(cells.len) or self.entry_count >= cells.len)
    {
        return error.CorruptArtifact;
    }

    var byte_entry_count: usize = 0;
    var entry_start: usize = 1;
    while (entry_start < data.len) {
        const terminator_offset = std.mem.findScalar(u8, data[entry_start..], 0) orelse
            return error.CorruptArtifact;
        byte_entry_count += 1;
        entry_start += terminator_offset + 1;
    }
    if (entry_start != data.len or byte_entry_count != self.entry_count) {
        return error.CorruptArtifact;
    }

    // Validate every candidate cell before the probe replay below dereferences
    // even one of them. The table has one occupied cell per byte entry and is
    // not full, so every bounded probe either finds its entry or reaches an
    // empty terminator.
    var indexed_entry_count: usize = 0;
    for (cells) |cell| {
        if (cell == .unused) continue;
        if (!self.validateExactIdx(cell)) return error.CorruptArtifact;
        indexed_entry_count += 1;
    }
    if (indexed_entry_count != self.entry_count) return error.CorruptArtifact;

    // Replay the actual lookup for every byte entry. Its first equal-text cell
    // must name that exact byte offset. Since there are N byte entries and
    // exactly N occupied cells, these N distinct successful lookups prove a
    // bijection without an unconditional pairwise cell/text scan: a duplicate
    // cell necessarily omits another entry, and equal text at two offsets makes
    // one of those offsets encounter the other one first.
    entry_start = 1;
    while (entry_start < data.len) {
        const terminator_offset = std.mem.findScalar(u8, data[entry_start..], 0) orelse
            return error.CorruptArtifact;
        const raw_wanted = std.math.cast(u32, entry_start) orelse
            return error.CorruptArtifact;
        const wanted: Idx = @enumFromInt(raw_wanted);
        const text = data[entry_start .. entry_start + terminator_offset];
        const mask = cells.len - 1;
        var probe: usize = @intCast(Policy.hash(text) & @as(u64, @intCast(mask)));
        var probes_remaining = cells.len;
        while (probes_remaining > 0) : (probes_remaining -= 1) {
            const candidate = cells[probe];
            if (candidate == .unused) return error.CorruptArtifact;
            if (std.mem.eql(u8, text, Policy.textForId(self, candidate))) {
                if (candidate != wanted) return error.CorruptArtifact;
                break;
            }
            probe = (probe + 1) & mask;
        } else return error.CorruptArtifact;

        entry_start += terminator_offset + 1;
    }
}

/// Get a reference to the text for an interned string.
pub fn getText(self: *const SmallStringInterner, idx: Idx) []u8 {
    const bytes_slice = self.bytes.items.items;
    const start = @intFromEnum(idx);
    return std.mem.sliceTo(bytes_slice[start..], 0);
}

/// Serialize this interner to the given CompactWriter. The resulting interner
/// in the writer's buffer will have offsets instead of pointers. Calling any
/// methods on it or dereferencing its internal "pointers" (which are now
/// offsets) is illegal behavior!
pub fn serialize(
    self: *const SmallStringInterner,
    allocator: std.mem.Allocator,
    writer: *CompactWriter,
) std.mem.Allocator.Error!*const SmallStringInterner {
    // First, write the struct
    const offset_self = try writer.appendAlloc(allocator, SmallStringInterner);

    // Then serialize the bytes and probe-table SafeLists and update the struct
    const serialized_bytes = try self.bytes.serialize(allocator, writer);
    const serialized_index = try self.index.serialize(allocator, writer);

    offset_self.* = .{
        .bytes = serialized_bytes.*,
        .index = serialized_index.*,
        .entry_count = self.entry_count,
    };

    // Return the version of Self that's in the writer's buffer
    return @constCast(offset_self);
}

/// Add the given offset to the memory addresses of all pointers in `self`.
pub fn relocate(self: *SmallStringInterner, offset: isize) void {
    self.bytes.relocate(offset);
    self.index.relocate(offset);
}

/// Serialized representation of a SmallStringInterner
/// Uses extern struct to guarantee consistent field layout across optimization levels.
pub const Serialized = extern struct {
    bytes: collections.SafeList(u8).Serialized,
    /// The probe table (runtime field `index`); kept named `hash_table` here so the
    /// on-disk `ModuleEnv` layout hash is unchanged by the field rename.
    hash_table: collections.SafeList(Idx).Serialized,
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
        // Serialize the bytes SafeList
        try self.bytes.serialize(&interner.bytes, allocator, writer);
        // Serialize the probe table
        try self.hash_table.serialize(&interner.index, allocator, writer);
        // Copy simple values directly
        self.entry_count = interner.entry_count;
        self._padding = 0;
    }

    /// Deserialize into a SmallStringInterner value (no in-place modification of cache buffer).
    /// The base parameter is the base address of the serialized buffer in memory.
    pub fn deserializeInto(self: *const Serialized, base: usize) SmallStringInterner {
        return SmallStringInterner{
            .bytes = self.bytes.deserializeInto(base),
            .index = self.hash_table.deserializeInto(base),
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

    const serialized = try original.serialize(arena_allocator, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate - empty interner should still work
    // The SmallStringInterner struct is at the beginning of the buffer
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty - bytes starts with one zero byte, hash_table should be empty
    try std.testing.expectEqual(@as(usize, 1), deserialized.bytes.len());
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

    const serialized = try original.serialize(arena_allocator, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

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
        .{ .str = "first", .expected_idx = 1 }, // First string starts at index 1 (after initial 0 byte)
        .{ .str = "second", .expected_idx = 7 },
        .{ .str = "third", .expected_idx = 14 },
        .{ .str = "first", .expected_idx = 1 }, // duplicate
        .{ .str = "fourth", .expected_idx = 20 },
        .{ .str = "fifth", .expected_idx = 27 },
        .{ .str = "second", .expected_idx = 7 }, // duplicate
        .{ .str = "sixth", .expected_idx = 33 },
        .{ .str = "seventh", .expected_idx = 39 },
        .{ .str = "eighth", .expected_idx = 47 },
    };

    for (test_data) |data| {
        const idx = try original.insert(gpa, data.str);
        try std.testing.expectEqual(@as(u32, data.expected_idx), @intFromEnum(idx));
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

    const serialized = try original.serialize(arena_allocator, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the entry count is preserved after deserialization
    try std.testing.expectEqual(original_entry_count, deserialized.entry_count);

    // But all strings should still be accessible
    // Note: Index 0 is reserved, so all indices are offset by 1
    try std.testing.expectEqualStrings("first", deserialized.getText(@enumFromInt(1)));
    try std.testing.expectEqualStrings("second", deserialized.getText(@enumFromInt(7)));
    try std.testing.expectEqualStrings("third", deserialized.getText(@enumFromInt(14)));
    try std.testing.expectEqualStrings("fourth", deserialized.getText(@enumFromInt(20)));
    try std.testing.expectEqualStrings("fifth", deserialized.getText(@enumFromInt(27)));
    try std.testing.expectEqualStrings("sixth", deserialized.getText(@enumFromInt(33)));
    try std.testing.expectEqualStrings("seventh", deserialized.getText(@enumFromInt(39)));
    try std.testing.expectEqualStrings("eighth", deserialized.getText(@enumFromInt(47)));

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
    try std.testing.expect(@intFromEnum(idx1) < @intFromEnum(idx2));

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

    const serialized = try original.serialize(arena_allocator, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify strings are still accessible
    // Note: Index 0 is reserved for the unused marker, so strings start at index 1
    try std.testing.expectEqualStrings("test1", deserialized.getText(@enumFromInt(1)));
    try std.testing.expectEqualStrings("test2", deserialized.getText(@enumFromInt(7)));
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
        "\x00embedded", // string starting with null (though this might not work)
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

    const serialized = try original.serialize(arena_allocator, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate
    const deserialized = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all edge cases
    for (edge_cases, 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.getText(idx);

        // Special case: strings starting with null byte will be truncated to empty string
        // This is a limitation of null-terminated string storage
        if (expected_str.len > 0 and expected_str[0] == '\x00') {
            try std.testing.expectEqualStrings("", actual_str);
        } else {
            try std.testing.expectEqualStrings(expected_str, actual_str);
        }
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

test "SmallStringInterner semantic validation accepts empty and wrapped collision chains" {
    const gpa = std.testing.allocator;

    var empty = try SmallStringInterner.initCapacity(gpa, 0);
    defer empty.deinit(gpa);
    try empty.validateSemanticState();
    const empty_text = try empty.insert(gpa, "");
    try std.testing.expectEqualStrings("", empty.getText(empty_text));
    try empty.validateSemanticState();

    var interner = try SmallStringInterner.initCapacity(gpa, 8);
    defer interner.deinit(gpa);
    const mask = interner.index.items.items.len - 1;

    // Find two distinct real texts with the last table slot as their home.
    // Inserting only those two forces the second probe to wrap to slot zero.
    var ids: [2]Idx = undefined;
    var found: usize = 0;
    var candidate_number: usize = 0;
    while (candidate_number < 4096 and found < ids.len) : (candidate_number += 1) {
        var buffer: [32]u8 = undefined;
        const text = try std.fmt.bufPrint(&buffer, "wrapped-ident-{d}", .{candidate_number});
        const home: usize = @intCast(Policy.hash(text) & @as(u64, @intCast(mask)));
        if (home != mask) continue;
        ids[found] = try interner.insert(gpa, text);
        found += 1;
    }
    try std.testing.expectEqual(ids.len, found);
    try std.testing.expectEqual(ids[0], interner.index.items.items[mask]);
    try std.testing.expectEqual(ids[1], interner.index.items.items[0]);
    try interner.validateSemanticState();
}

test "SmallStringInterner semantic validation rejects non-bijective probe tables" {
    const gpa = std.testing.allocator;

    // Omitting the only entry is rejected directly, independently of the
    // coordinated duplicate-cell case below.
    {
        var interner = try SmallStringInterner.initCapacity(gpa, 1);
        defer interner.deinit(gpa);
        const id = try interner.insert(gpa, "missing");
        for (interner.index.items.items) |*cell| {
            if (cell.* == id) cell.* = .unused;
        }
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
    }

    // Repeating one exact cell while omitting another keeps the occupied count
    // unchanged, but the omitted byte entry cannot resolve to itself.
    {
        var interner = try SmallStringInterner.initCapacity(gpa, 2);
        defer interner.deinit(gpa);
        const first = try interner.insert(gpa, "alpha");
        const second = try interner.insert(gpa, "bravo");
        var second_slot: ?usize = null;
        for (interner.index.items.items, 0..) |cell, slot| {
            if (cell == second) second_slot = slot;
        }
        interner.index.items.items[second_slot orelse return error.TestUnexpectedResult] = first;
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
    }

    // Two distinct byte offsets with equal text are not two interner entries,
    // even when both cells form an otherwise valid collision chain.
    {
        var interner = try SmallStringInterner.initCapacity(gpa, 2);
        defer interner.deinit(gpa);
        const first = try interner.insert(gpa, "alpha");
        const second = try interner.insert(gpa, "bravo");
        const first_start: usize = @intFromEnum(first);
        const second_start: usize = @intFromEnum(second);
        @memcpy(
            interner.bytes.items.items[second_start..][0.."alpha".len],
            interner.bytes.items.items[first_start..][0.."alpha".len],
        );
        @memset(interner.index.items.items, .unused);
        const mask = interner.index.items.items.len - 1;
        const home: usize = @intCast(Policy.hash("alpha") & @as(u64, @intCast(mask)));
        interner.index.items.items[home] = first;
        interner.index.items.items[(home + 1) & mask] = second;
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
    }

    // Moving a cell beyond an empty home slot violates first-empty linear
    // probing even though the cell and occupied count remain individually valid.
    {
        var interner = try SmallStringInterner.initCapacity(gpa, 1);
        defer interner.deinit(gpa);
        const id = try interner.insert(gpa, "misplaced");
        const mask = interner.index.items.items.len - 1;
        const home: usize = @intCast(Policy.hash("misplaced") & @as(u64, @intCast(mask)));
        try std.testing.expectEqual(id, interner.index.items.items[home]);
        interner.index.items.items[home] = .unused;
        interner.index.items.items[(home + 1) & mask] = id;
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
    }
}

test "SmallStringInterner semantic validation rejects malformed bytes cells counts and capacity" {
    const gpa = std.testing.allocator;

    {
        var interner = try SmallStringInterner.initCapacity(gpa, 1);
        defer interner.deinit(gpa);
        _ = try interner.insert(gpa, "alpha");
        interner.bytes.items.items[0] = 1;
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
    }
    {
        var interner = try SmallStringInterner.initCapacity(gpa, 1);
        defer interner.deinit(gpa);
        _ = try interner.insert(gpa, "alpha");
        interner.bytes.items.items[interner.bytes.items.items.len - 1] = 'x';
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
    }
    {
        var interner = try SmallStringInterner.initCapacity(gpa, 1);
        defer interner.deinit(gpa);
        const id = try interner.insert(gpa, "alpha");
        const cell = for (interner.index.items.items, 0..) |candidate, slot| {
            if (candidate == id) break &interner.index.items.items[slot];
        } else return error.TestUnexpectedResult;
        cell.* = @enumFromInt(@intFromEnum(id) + 1);
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
        cell.* = @enumFromInt(@as(u32, @intCast(interner.bytes.items.items.len)));
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
    }
    {
        var interner = try SmallStringInterner.initCapacity(gpa, 1);
        defer interner.deinit(gpa);
        _ = try interner.insert(gpa, "alpha");
        interner.entry_count += 1;
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
    }
    {
        var interner = try SmallStringInterner.initCapacity(gpa, 1);
        defer interner.deinit(gpa);
        _ = try interner.insert(gpa, "alpha");
        interner.entry_count = @intCast(interner.index.items.items.len);
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
    }
    {
        var interner = try SmallStringInterner.initCapacity(gpa, 2);
        defer interner.deinit(gpa);
        _ = try interner.insert(gpa, "alpha");
        interner.index.items.items.len -= 1;
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
    }
    {
        var interner: SmallStringInterner = .{};
        try std.testing.expectError(error.CorruptArtifact, interner.validateSemanticState());
    }
}

test "SmallStringInterner semantic validation handles a realistic identifier table" {
    const gpa = std.testing.allocator;
    const identifier_count = 24_000;
    var interner = try SmallStringInterner.initCapacity(gpa, identifier_count);
    defer interner.deinit(gpa);

    var identifier_number: usize = 0;
    while (identifier_number < identifier_count) : (identifier_number += 1) {
        var buffer: [32]u8 = undefined;
        const text = try std.fmt.bufPrint(&buffer, "identifier-{d}", .{identifier_number});
        _ = try interner.insert(gpa, text);
    }

    try std.testing.expectEqual(@as(u32, identifier_count), interner.entry_count);
    try interner.validateSemanticState();
    try std.testing.expect(interner.lookup("identifier-0") != null);
    try std.testing.expect(interner.lookup("identifier-23999") != null);
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
    _ = try interner1.serialize(arena_allocator, &writer);

    try writer.padToAlignment(arena_allocator, @alignOf(SmallStringInterner));
    const offset2 = writer.total_bytes;
    _ = try interner2.serialize(arena_allocator, &writer);

    try writer.padToAlignment(arena_allocator, @alignOf(SmallStringInterner));
    const offset3 = writer.total_bytes;
    _ = try interner3.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate all three; every serialized pointer is an offset from the
    // start of the whole buffer, so each root relocates by the same base address.
    const deserialized1 = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + offset1)));
    deserialized1.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized2 = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + offset2)));
    deserialized2.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized3 = @as(*SmallStringInterner, @ptrCast(@alignCast(buffer.ptr + offset3)));
    deserialized3.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

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

test "SmallStringInterner savepoints compose nested commit and rollback" {
    const gpa = std.testing.allocator;
    var interner = try SmallStringInterner.initCapacity(gpa, 1);
    defer interner.deinit(gpa);

    const seed = try interner.insert(gpa, "seed");
    const baseline_bytes = try gpa.dupe(u8, interner.bytes.items.items);
    defer gpa.free(baseline_bytes);
    const baseline_index = try gpa.dupe(Idx, interner.index.items.items);
    defer gpa.free(baseline_index);
    const baseline_count = interner.entry_count;

    var outer = try interner.createSavepoint(gpa);
    var outer_open = true;
    errdefer if (outer_open) interner.rollbackToSavepoint(&outer);
    _ = try interner.insert(gpa, "outer-discarded");
    var inner = try interner.createSavepoint(gpa);
    var inner_open = true;
    errdefer if (inner_open) interner.rollbackToSavepoint(&inner);
    _ = try interner.insert(gpa, "inner-committed");
    interner.commitSavepoint(&inner);
    inner_open = false;
    try std.testing.expect(interner.lookup("inner-committed") != null);

    interner.rollbackToSavepoint(&outer);
    outer_open = false;
    try std.testing.expectEqual(@as(?Idx, seed), interner.lookup("seed"));
    try std.testing.expectEqual(@as(?Idx, null), interner.lookup("outer-discarded"));
    try std.testing.expectEqual(@as(?Idx, null), interner.lookup("inner-committed"));
    try std.testing.expectEqual(baseline_count, interner.entry_count);
    try std.testing.expectEqualSlices(u8, baseline_bytes, interner.bytes.items.items);
    try std.testing.expectEqualSlices(Idx, baseline_index, interner.index.items.items);

    var committed_outer = try interner.createSavepoint(gpa);
    var committed_outer_open = true;
    errdefer if (committed_outer_open) interner.rollbackToSavepoint(&committed_outer);
    const kept = try interner.insert(gpa, "outer-kept");
    const kept_bytes = try gpa.dupe(u8, interner.bytes.items.items);
    defer gpa.free(kept_bytes);
    const kept_index = try gpa.dupe(Idx, interner.index.items.items);
    defer gpa.free(kept_index);
    const kept_count = interner.entry_count;

    var rolled_back_inner = try interner.createSavepoint(gpa);
    var rolled_back_inner_open = true;
    errdefer if (rolled_back_inner_open) interner.rollbackToSavepoint(&rolled_back_inner);
    _ = try interner.insert(gpa, "inner-discarded");
    interner.rollbackToSavepoint(&rolled_back_inner);
    rolled_back_inner_open = false;
    interner.commitSavepoint(&committed_outer);
    committed_outer_open = false;

    try std.testing.expectEqual(@as(?Idx, seed), interner.lookup("seed"));
    try std.testing.expectEqual(@as(?Idx, kept), interner.lookup("outer-kept"));
    try std.testing.expectEqual(@as(?Idx, null), interner.lookup("inner-discarded"));
    try std.testing.expectEqual(kept_count, interner.entry_count);
    try std.testing.expectEqualSlices(u8, kept_bytes, interner.bytes.items.items);
    try std.testing.expectEqualSlices(Idx, kept_index, interner.index.items.items);
    try std.testing.expectEqual(@as(u16, 0), interner.savepoint_depth);
    try interner.validateSemanticState();
}

test "SmallStringInterner savepoint rollback makes induced OOM atomic" {
    const gpa = std.testing.allocator;
    const candidate = [_]u8{'x'} ** 64;
    var saw_mutation_oom = false;
    var reached_success = false;

    for (0..8) |fail_index| {
        var interner = try SmallStringInterner.initCapacity(gpa, 1);
        _ = try interner.insert(gpa, "a");

        const before_bytes = try gpa.dupe(u8, interner.bytes.items.items);
        defer gpa.free(before_bytes);
        const before_index = try gpa.dupe(Idx, interner.index.items.items);
        defer gpa.free(before_index);
        const before_count = interner.entry_count;

        var failing = std.testing.FailingAllocator.init(gpa, .{
            .fail_index = fail_index,
            .resize_fail_index = 0,
        });
        const failing_gpa = failing.allocator();
        defer interner.deinit(failing_gpa);

        var savepoint = interner.createSavepoint(failing_gpa) catch |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            try std.testing.expectEqual(before_count, interner.entry_count);
            try std.testing.expectEqualSlices(u8, before_bytes, interner.bytes.items.items);
            try std.testing.expectEqualSlices(Idx, before_index, interner.index.items.items);
            try std.testing.expectEqual(@as(u16, 0), interner.savepoint_depth);
            continue;
        };

        const insertion = interner.insert(failing_gpa, &candidate);
        if (insertion) |_| {
            interner.rollbackToSavepoint(&savepoint);
            reached_success = true;
        } else |err| {
            interner.rollbackToSavepoint(&savepoint);
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            saw_mutation_oom = true;
        }

        try std.testing.expectEqual(before_count, interner.entry_count);
        try std.testing.expectEqualSlices(u8, before_bytes, interner.bytes.items.items);
        try std.testing.expectEqualSlices(Idx, before_index, interner.index.items.items);
        try std.testing.expectEqual(@as(?Idx, null), interner.lookup(&candidate));
        try std.testing.expectEqual(@as(u16, 0), interner.savepoint_depth);
        try interner.validateSemanticState();
        if (reached_success) break;
    }

    try std.testing.expect(saw_mutation_oom);
    try std.testing.expect(reached_success);
}
