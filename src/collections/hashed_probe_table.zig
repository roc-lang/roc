//! An open-addressed dedup index over the entries of a store, kept in the
//! store's own columns so a loaded store deduplicates exactly as the store it
//! was written from.
//!
//! Each cell names one entry (by index plus one, zero for an empty cell) and
//! records the entry's 32-bit hash, so growing the table rehashes from the
//! cells alone and a probe compares an entry only when its hash matches. The
//! table never holds more entries than half its cells, its length is a power
//! of two, and every hash and every placement depends only on the entries and
//! the order they were inserted, so equal stores hold byte-identical tables.
//!
//! The cells are a `SafeList`: a table loaded from a cache entry borrows the
//! entry's bytes, takes new cells in place, and copies the cells on its first
//! growth. `View` probes a table it never writes, which lets an overlay store
//! deduplicate against the table of a store it extends without touching it.

const std = @import("std");

const SafeList = @import("safe_list.zig").SafeList;
const CompactWriter = @import("CompactWriter.zig");
const serde_validation = @import("serde_validation.zig");

const Allocator = std.mem.Allocator;

const initial_cell_count: usize = 16;

/// The table.
pub const HashedProbeTable = struct {
    cells: SafeList(Cell) = .{},
    len: u32 = 0,

    const Self = @This();

    /// One cell: the entry it names, plus one (zero for an empty cell), and
    /// that entry's hash.
    pub const Cell = extern struct {
        entry_plus_one: u32 = 0,
        hash: u32 = 0,
    };

    /// A read-only probe over a table's cells.
    pub const View = struct {
        cells: []const Cell = &.{},

        pub const empty: View = .{};

        /// The entry whose hash is `hash` and that `context.eql` accepts, or
        /// null. `context.eql(entry: u32) bool` compares the needle the hash
        /// was computed from with entry `entry`.
        pub fn find(self: View, hash: u32, context: anytype) ?u32 {
            return self.probe(hash, context).entry;
        }

        const Probe = struct { entry: ?u32, slot: usize };

        fn probe(self: View, hash: u32, context: anytype) Probe {
            const cell_count = self.cells.len;
            if (cell_count == 0) return .{ .entry = null, .slot = 0 };
            const mask = cell_count - 1;
            var slot: usize = hash & mask;
            while (true) {
                const cell = self.cells[slot];
                if (cell.entry_plus_one == 0) return .{ .entry = null, .slot = slot };
                if (cell.hash == hash and context.eql(cell.entry_plus_one - 1)) {
                    return .{ .entry = cell.entry_plus_one - 1, .slot = slot };
                }
                slot = (slot + 1) & mask;
            }
        }
    };

    /// Serialized form: the cell column and the entry count, loaded in place.
    pub const Serialized = extern struct {
        cells: SafeList(Cell).Serialized = .{ .offset = 0, .len = 0, .capacity = 0 },
        len: u32 = 0,
        _padding: u32 = 0,

        pub fn serialize(self: *Serialized, table: *const Self, allocator: Allocator, writer: *CompactWriter) Allocator.Error!void {
            try self.cells.serialize(&table.cells, allocator, writer);
            self.len = table.len;
            self._padding = 0;
        }

        /// Reject a table a probe could walk forever, or that its own load
        /// limit forbids. Constant time: no cell is read.
        pub fn validateShape(self: *const Serialized) error{CorruptArtifact}!void {
            if (!serde_validation.probeTableShapeValid(self.cells.len, self.len)) return error.CorruptArtifact;
            if (@as(u64, self.len) * 2 > self.cells.len) return error.CorruptArtifact;
        }

        /// A table whose cells borrow the serialized cells at `base`.
        pub fn deserializeInto(self: *const Serialized, base: usize) Self {
            return .{ .cells = self.cells.deserializeInto(base), .len = self.len };
        }
    };

    pub fn deinit(self: *Self, gpa: Allocator) void {
        self.cells.deinit(gpa);
        self.* = .{};
    }

    pub fn view(self: *const Self) View {
        return .{ .cells = self.cells.items() };
    }

    pub fn count(self: *const Self) u32 {
        return self.len;
    }

    /// See `View.find`.
    pub fn find(self: *const Self, hash: u32, context: anytype) ?u32 {
        return self.view().find(hash, context);
    }

    /// Record `entry`, whose hash is `hash`, which no entry of the table
    /// equals. On failure the table is unchanged.
    pub fn insertNew(self: *Self, gpa: Allocator, hash: u32, entry: u32) Allocator.Error!void {
        std.debug.assert(entry != std.math.maxInt(u32));
        if ((@as(usize, self.len) + 1) * 2 > self.cells.items().len) try self.grow(gpa);
        const slot = self.emptySlot(hash);
        self.cells.items()[slot] = .{ .entry_plus_one = entry + 1, .hash = hash };
        self.len += 1;
    }

    fn emptySlot(self: *const Self, hash: u32) usize {
        const cells = self.cells.items();
        const mask = cells.len - 1;
        var slot: usize = hash & mask;
        while (cells[slot].entry_plus_one != 0) slot = (slot + 1) & mask;
        return slot;
    }

    fn grow(self: *Self, gpa: Allocator) Allocator.Error!void {
        const old_cells = self.cells;
        const new_count = @max(initial_cell_count, old_cells.items().len * 2);
        var cells = try SafeList(Cell).initCapacity(gpa, new_count);
        cells.appendNTimesAssumeCapacity(.{}, new_count);
        self.cells = cells;
        for (old_cells.items()) |cell| {
            if (cell.entry_plus_one == 0) continue;
            self.cells.items()[self.emptySlot(cell.hash)] = cell;
        }
        // A borrowed column frees nothing.
        var released = old_cells;
        released.deinit(gpa);
    }
};

const testing = std.testing;

const WordsContext = struct {
    words: []const []const u8,
    needle: []const u8,

    pub fn eql(self: WordsContext, entry: u32) bool {
        return std.mem.eql(u8, self.words[entry], self.needle);
    }
};

fn wordHash(word: []const u8) u32 {
    return @truncate(std.hash.Wyhash.hash(0, word));
}

test "HashedProbeTable finds inserted entries and grows at half load" {
    const gpa = testing.allocator;
    var table = HashedProbeTable{};
    defer table.deinit(gpa);
    var words: [40][]const u8 = undefined;
    var buffers: [40][8]u8 = undefined;
    for (&words, &buffers, 0..) |*word, *buffer, i| {
        word.* = std.fmt.bufPrint(buffer, "w{d}", .{i}) catch unreachable;
        const context = WordsContext{ .words = words[0..i], .needle = word.* };
        try testing.expectEqual(@as(?u32, null), table.find(wordHash(word.*), context));
        try table.insertNew(gpa, wordHash(word.*), @intCast(i));
        try testing.expect(@as(usize, table.len) * 2 <= table.cells.items().len);
    }
    for (words, 0..) |word, i| {
        const context = WordsContext{ .words = &words, .needle = word };
        try testing.expectEqual(@as(?u32, @intCast(i)), table.find(wordHash(word), context));
    }
}

test "HashedProbeTable resolves full hash collisions by comparing entries" {
    const gpa = testing.allocator;
    var table = HashedProbeTable{};
    defer table.deinit(gpa);
    const words = [_][]const u8{ "alpha", "beta", "gamma" };
    for (words, 0..) |_, i| try table.insertNew(gpa, 7, @intCast(i));
    for (words, 0..) |word, i| {
        try testing.expectEqual(@as(?u32, @intCast(i)), table.find(7, WordsContext{ .words = &words, .needle = word }));
    }
    try testing.expectEqual(@as(?u32, null), table.find(7, WordsContext{ .words = &words, .needle = "delta" }));
}

test "HashedProbeTable loads in place, inserts in place, and copies on growth" {
    const gpa = testing.allocator;
    var table = HashedProbeTable{};
    defer table.deinit(gpa);
    const words = [_][]const u8{ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j" };
    for (words[0..3], 0..) |word, i| try table.insertNew(gpa, wordHash(word), @intCast(i));

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    const header = try writer.appendAlloc(gpa, HashedProbeTable.Serialized);
    try header.serialize(&table, gpa, &writer);
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);
    const loaded_header: *const HashedProbeTable.Serialized = @ptrCast(@alignCast(buffer.ptr));
    try serde_validation.validateSerializedRelocations(HashedProbeTable.Serialized, loaded_header, buffer.len);

    var loaded = loaded_header.deserializeInto(@intFromPtr(buffer.ptr));
    defer loaded.deinit(gpa);
    try testing.expectEqualSlices(u8, std.mem.sliceAsBytes(table.cells.items()), std.mem.sliceAsBytes(loaded.cells.items()));
    for (words[0..3], 0..) |word, i| {
        try testing.expectEqual(@as(?u32, @intCast(i)), loaded.find(wordHash(word), WordsContext{ .words = &words, .needle = word }));
    }
    const borrowed_cells = loaded.cells.items().ptr;
    try loaded.insertNew(gpa, wordHash(words[3]), 3);
    try testing.expectEqual(borrowed_cells, loaded.cells.items().ptr);
    for (words[4..], 4..) |word, i| try loaded.insertNew(gpa, wordHash(word), @intCast(i));
    try testing.expect(loaded.cells.items().ptr != borrowed_cells);
    for (words, 0..) |word, i| {
        try testing.expectEqual(@as(?u32, @intCast(i)), loaded.find(wordHash(word), WordsContext{ .words = &words, .needle = word }));
    }
}

test "HashedProbeTable.Serialized.validateShape rejects tables a probe cannot trust" {
    var serialized = HashedProbeTable.Serialized{ .cells = .{ .offset = 0, .len = 16, .capacity = 16 }, .len = 8 };
    try serialized.validateShape();
    serialized.len = 9;
    try testing.expectError(error.CorruptArtifact, serialized.validateShape());
    serialized.cells.len = 12;
    serialized.len = 3;
    try testing.expectError(error.CorruptArtifact, serialized.validateShape());
    serialized.cells.len = 0;
    serialized.len = 1;
    try testing.expectError(error.CorruptArtifact, serialized.validateShape());
    serialized.len = 0;
    try serialized.validateShape();
}
