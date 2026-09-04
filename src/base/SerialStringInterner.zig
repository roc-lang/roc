//! A serialization-friendly string interner that assigns **dense serial ids**.
//!
//! It backs `CanonicalNameStore`'s name kinds with relocatable storage. Like
//! `base.SmallStringInterner` it stores all text in one flat byte buffer and an
//! open-addressing hash table as a flat array of integers—both
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
/// Number of open rollback savepoints. This is runtime-only state and is never
/// serialized; `u16` uses padding already present in the runtime struct.
savepoint_depth: u16 = 0,

/// An owned snapshot of the append boundaries and exact probe-table cells.
/// Savepoints must be closed exactly once and in LIFO order on `owner`.
pub const Savepoint = struct {
    allocator: Allocator,
    owner: *SerialStringInterner,
    depth: u16,
    bytes_len: usize,
    ranges_len: usize,
    index_cells: []u32,
};

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

/// Immutable append boundary for the durable id and byte arrays.
pub const EpochBoundary = struct {
    ids: u32,
    bytes: u32,
};

/// Capture the current durable boundary without exposing growable storage.
pub fn epochBoundary(self: *const SerialStringInterner) EpochBoundary {
    return .{
        .ids = @intCast(self.ranges.items.items.len),
        .bytes = @intCast(self.bytes.items.items.len),
    };
}

/// Owned immutable suffix between two durable boundaries.
///
/// Hash-index cells are derived insertion state, not durable data. Range
/// starts are normalized to this segment's byte allocation so no pointer or
/// offset retains the mutable source interner.
pub const EpochDelta = struct {
    allocator: Allocator,
    begin: EpochBoundary,
    end: EpochBoundary,
    bytes: []u8,
    ranges: []Range,

    pub fn capture(
        allocator: Allocator,
        source: *const SerialStringInterner,
        begin: EpochBoundary,
        end: EpochBoundary,
    ) Allocator.Error!EpochDelta {
        requireEpochBoundary(source, begin);
        requireEpochBoundary(source, end);
        std.debug.assert(begin.ids <= end.ids);
        std.debug.assert(begin.bytes <= end.bytes);

        const owned_bytes = try allocator.dupe(
            u8,
            source.bytes.items.items[begin.bytes..end.bytes],
        );
        errdefer allocator.free(owned_bytes);
        const source_ranges = source.ranges.items.items[begin.ids..end.ids];
        const owned_ranges = try allocator.alloc(Range, source_ranges.len);
        errdefer allocator.free(owned_ranges);
        for (source_ranges, owned_ranges) |source_range, *owned_range| {
            std.debug.assert(source_range.start >= begin.bytes);
            std.debug.assert(source_range.start + source_range.len <= end.bytes);
            owned_range.* = .{
                .start = source_range.start - begin.bytes,
                .len = source_range.len,
            };
        }
        return .{
            .allocator = allocator,
            .begin = begin,
            .end = end,
            .bytes = owned_bytes,
            .ranges = owned_ranges,
        };
    }

    /// Resolve a workspace-local serial id when it belongs to this segment.
    pub fn getText(self: *const EpochDelta, id: u32) ?[]const u8 {
        if (id < self.begin.ids or id >= self.end.ids) return null;
        const range = self.ranges[id - self.begin.ids];
        return self.bytes[range.start..][0..range.len];
    }

    /// Reserve every allocation needed to append this exact suffix.
    pub fn prepareAppend(
        self: *const EpochDelta,
        destination: *SerialStringInterner,
        allocator: Allocator,
    ) Allocator.Error!void {
        std.debug.assert(std.meta.eql(destination.epochBoundary(), self.begin));
        if (self.begin.ids == self.end.ids) return;
        try destination.bytes.items.ensureTotalCapacity(allocator, self.end.bytes);
        try destination.ranges.items.ensureTotalCapacity(allocator, self.end.ids);
        var index = Index.fromCells(destination.index, destination.count());
        defer destination.index = index.cells;
        try index.ensureTotalCapacity(destination, allocator, self.end.ids);
    }

    /// Append after `prepareAppend`; no logical mutation can fail.
    pub fn appendPrepared(
        self: *const EpochDelta,
        destination: *SerialStringInterner,
        allocator: Allocator,
    ) void {
        std.debug.assert(std.meta.eql(destination.epochBoundary(), self.begin));
        var index = Index.fromCells(destination.index, destination.count());
        defer destination.index = index.cells;
        var id = self.begin.ids;
        while (id < self.end.ids) : (id += 1) {
            const inserted = index.insert(
                destination,
                allocator,
                self.getText(id).?,
            ) catch unreachable;
            std.debug.assert(inserted == id);
        }
        std.debug.assert(std.meta.eql(destination.epochBoundary(), self.end));
    }

    /// Append this suffix after an identical prefix while preserving serial ids.
    pub fn appendTo(
        self: *const EpochDelta,
        destination: *SerialStringInterner,
        allocator: Allocator,
    ) Allocator.Error!void {
        try self.prepareAppend(destination, allocator);
        self.appendPrepared(destination, allocator);
    }

    pub fn deinit(self: *EpochDelta) void {
        self.allocator.free(self.ranges);
        self.allocator.free(self.bytes);
        self.* = undefined;
    }
};

fn requireEpochBoundary(self: *const SerialStringInterner, mark: EpochBoundary) void {
    std.debug.assert(mark.ids <= self.ranges.items.items.len);
    std.debug.assert(mark.bytes <= self.bytes.items.items.len);
    const expected_bytes: u32 = if (mark.ids == 0)
        0
    else blk: {
        const range = self.ranges.items.items[mark.ids - 1];
        break :blk range.start + range.len;
    };
    std.debug.assert(mark.bytes == expected_bytes);
}

fn textAt(self: *const SerialStringInterner, id: u32) []const u8 {
    const r = self.ranges.items.items[id];
    return self.bytes.items.items[r.start .. r.start + r.len];
}

/// Text for a serial id.
pub fn getText(self: *const SerialStringInterner, id: u32) []const u8 {
    return self.textAt(id);
}

/// Validate every logical index before a cache consumer calls `getText` or
/// `lookup`. Relocation validation proves only that the three backing slices
/// lie in the artifact; it does not prove that range scalars or hash cells are
/// safe. `expected_text_len` is used by fixed-width identity tables.
pub fn validateSemanticState(
    self: *const SerialStringInterner,
    expected_text_len: ?usize,
) error{CorruptArtifact}!void {
    const bytes = self.bytes.items.items;
    const ranges = self.ranges.items.items;
    const cells = self.index.items.items;

    var next_start: usize = 0;
    for (ranges) |range| {
        const range_start: usize = range.start;
        const range_len: usize = range.len;
        if (range_start != next_start or range_len > bytes.len - next_start) {
            return error.CorruptArtifact;
        }
        if (expected_text_len) |expected| {
            if (range_len != expected) return error.CorruptArtifact;
        }
        next_start += range_len;
    }
    if (next_start != bytes.len) return error.CorruptArtifact;

    if (ranges.len == 0) {
        for (cells) |cell| {
            if (cell != 0) return error.CorruptArtifact;
        }
        return;
    }
    if (cells.len == 0 or !std.math.isPowerOfTwo(cells.len)) return error.CorruptArtifact;

    var populated: usize = 0;
    for (cells) |cell| {
        if (cell == 0) continue;
        populated += 1;
        if (@as(usize, cell) > ranges.len) return error.CorruptArtifact;
    }
    // Linear probing requires an empty cell to terminate, and one table cell
    // for every serial id.
    if (populated != ranges.len or populated >= cells.len) return error.CorruptArtifact;

    // Every candidate id is now proven safe to dereference. Replay the actual
    // bounded lookup for each serial id and require its first equal-text cell
    // to name that exact id. Together with the equal occupied/entry counts,
    // these distinct successful lookups prove a bijection: duplicate cells
    // imply an omitted id, while duplicate text makes one id resolve to the
    // other. No separate all-cells occurrence scan is needed.
    for (ranges, 0..) |range, wanted_id| {
        const text = bytes[range.start..][0..range.len];
        const text_hash = InternedBytes.hash(text);
        var slot: usize = @intCast(text_hash & @as(u64, @intCast(cells.len - 1)));
        var remaining = cells.len;
        while (remaining > 0) : (remaining -= 1) {
            const cell = cells[slot];
            if (cell == 0) return error.CorruptArtifact;
            const candidate_id: usize = @as(usize, cell) - 1;
            const candidate = ranges[candidate_id];
            if (std.mem.eql(u8, text, bytes[candidate.start..][0..candidate.len])) {
                if (candidate_id != wanted_id) return error.CorruptArtifact;
                break;
            }
            slot = (slot + 1) & (cells.len - 1);
        } else return error.CorruptArtifact;
    }
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

fn assertSavepointTop(self: *SerialStringInterner, savepoint: *const Savepoint) void {
    assertSupportsInserts(self.supports_inserts);
    if (savepoint.owner == self and savepoint.depth != 0 and self.savepoint_depth == savepoint.depth) {
        return;
    }

    if (comptime builtin.mode == .Debug) {
        std.debug.panic("SerialStringInterner invariant violated: savepoints must close in LIFO order on their owning interner", .{});
    }
    unreachable;
}

/// Snapshot the current logical state. Opening a savepoint allocates one exact
/// copy of the probe table; all inserts while it is open must use the same
/// allocator that owns this interner.
pub fn createSavepoint(self: *SerialStringInterner, gpa: Allocator) Allocator.Error!Savepoint {
    assertSupportsInserts(self.supports_inserts);
    const depth = std.math.add(u16, self.savepoint_depth, 1) catch return error.OutOfMemory;
    const index_cells = try gpa.dupe(u32, self.index.items.items);

    self.savepoint_depth = depth;
    return .{
        .allocator = gpa,
        .owner = self,
        .depth = depth,
        .bytes_len = self.bytes.items.items.len,
        .ranges_len = self.ranges.items.items.len,
        .index_cells = index_cells,
    };
}

/// Close the newest savepoint while keeping all mutations made since it opened.
pub fn commitSavepoint(self: *SerialStringInterner, savepoint: *Savepoint) void {
    assertSavepointTop(self, savepoint);
    savepoint.allocator.free(savepoint.index_cells);
    self.savepoint_depth -= 1;
    savepoint.* = undefined;
}

/// Undo every insert since the newest savepoint opened. The durable byte/range
/// stores are append-only, and the saved table already owns every pre-savepoint
/// cell, so rollback only shrinks and copies; it never allocates.
pub fn rollbackToSavepoint(self: *SerialStringInterner, savepoint: *Savepoint) void {
    assertSavepointTop(self, savepoint);
    const valid_suffix = savepoint.bytes_len <= self.bytes.items.items.len and
        savepoint.ranges_len <= self.ranges.items.items.len and
        savepoint.index_cells.len <= self.index.items.items.len;
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(valid_suffix);
    } else if (!valid_suffix) {
        unreachable;
    }

    self.bytes.items.shrinkRetainingCapacity(savepoint.bytes_len);
    self.ranges.items.shrinkRetainingCapacity(savepoint.ranges_len);
    self.index.items.shrinkRetainingCapacity(savepoint.index_cells.len);
    @memcpy(self.index.items.items, savepoint.index_cells);

    savepoint.allocator.free(savepoint.index_cells);
    self.savepoint_depth -= 1;
    savepoint.* = undefined;
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

/// Add the given offset to the memory addresses of all pointers in `self`.
/// Used by serialized compiler artifacts whose internal pointers are stored
/// relative to the artifact buffer.
pub fn relocate(self: *SerialStringInterner, offset: isize) void {
    self.bytes.relocate(offset);
    self.ranges.relocate(offset);
    self.index.relocate(offset);
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

test "SerialStringInterner semantic validation accepts empty and wrapped collision chains" {
    const gpa = testing.allocator;

    var default_empty: SerialStringInterner = .{};
    try default_empty.validateSemanticState(null);

    var initialized_empty = try SerialStringInterner.initCapacity(gpa, 0);
    defer initialized_empty.deinit(gpa);
    try initialized_empty.validateSemanticState(null);
    const empty_text = try initialized_empty.insert(gpa, "");
    try testing.expectEqualStrings("", initialized_empty.getText(empty_text));
    try initialized_empty.validateSemanticState(0);

    // Empty stores historically accept any all-zero cell slice, including a
    // non-power-of-two preallocation, because no lookup can dereference it.
    var odd_capacity_empty = try SerialStringInterner.initCapacity(gpa, 0);
    defer odd_capacity_empty.deinit(gpa);
    odd_capacity_empty.index.items.items.len = 3;
    try odd_capacity_empty.validateSemanticState(null);

    var interner = try SerialStringInterner.initCapacity(gpa, 8);
    defer interner.deinit(gpa);
    const mask = interner.index.items.items.len - 1;

    // Find two distinct real texts with the last table slot as their home.
    // Inserting only those two forces the second probe to wrap to slot zero.
    var ids: [2]u32 = undefined;
    var found: usize = 0;
    var candidate_number: usize = 0;
    while (candidate_number < 4096 and found < ids.len) : (candidate_number += 1) {
        var buffer: [32]u8 = undefined;
        const text = try std.fmt.bufPrint(&buffer, "wrapped-name-{d}", .{candidate_number});
        const home: usize = @intCast(InternedBytes.hash(text) & @as(u64, @intCast(mask)));
        if (home != mask) continue;
        ids[found] = try interner.insert(gpa, text);
        found += 1;
    }
    try testing.expectEqual(ids.len, found);
    try testing.expectEqual(ids[0] + 1, interner.index.items.items[mask]);
    try testing.expectEqual(ids[1] + 1, interner.index.items.items[0]);
    try interner.validateSemanticState(null);
}

test "SerialStringInterner semantic validation rejects non-bijective probe tables" {
    const gpa = testing.allocator;

    // Repeating one exact cell while omitting another preserves the occupied
    // count but leaves one serial id unable to resolve to itself.
    {
        var interner = try SerialStringInterner.initCapacity(gpa, 2);
        defer interner.deinit(gpa);
        const first = try interner.insert(gpa, "alpha");
        const second = try interner.insert(gpa, "bravo");
        var second_slot: ?usize = null;
        for (interner.index.items.items, 0..) |cell, slot| {
            if (cell == second + 1) second_slot = slot;
        }
        interner.index.items.items[second_slot orelse return error.TestUnexpectedResult] = first + 1;
        try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(null));
    }

    // Two ids with equal text cannot both own the first equal-text probe cell.
    {
        var interner = try SerialStringInterner.initCapacity(gpa, 2);
        defer interner.deinit(gpa);
        const first = try interner.insert(gpa, "alpha");
        const second = try interner.insert(gpa, "bravo");
        const first_range = interner.ranges.items.items[first];
        const second_range = interner.ranges.items.items[second];
        @memcpy(
            interner.bytes.items.items[second_range.start..][0..second_range.len],
            interner.bytes.items.items[first_range.start..][0..first_range.len],
        );
        @memset(interner.index.items.items, 0);
        const mask = interner.index.items.items.len - 1;
        const home: usize = @intCast(InternedBytes.hash("alpha") & @as(u64, @intCast(mask)));
        interner.index.items.items[home] = first + 1;
        interner.index.items.items[(home + 1) & mask] = second + 1;
        try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(null));
    }

    // A valid id placed beyond an empty home cell violates lookup placement.
    {
        var interner = try SerialStringInterner.initCapacity(gpa, 1);
        defer interner.deinit(gpa);
        const id = try interner.insert(gpa, "misplaced");
        const mask = interner.index.items.items.len - 1;
        const home: usize = @intCast(InternedBytes.hash("misplaced") & @as(u64, @intCast(mask)));
        try testing.expectEqual(id + 1, interner.index.items.items[home]);
        interner.index.items.items[home] = 0;
        interner.index.items.items[(home + 1) & mask] = id + 1;
        try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(null));
    }
}

test "SerialStringInterner semantic validation rejects corrupt ranges and probe tables" {
    const gpa = testing.allocator;
    var interner = try SerialStringInterner.initCapacity(gpa, 2);
    defer interner.deinit(gpa);
    const first = [_]u8{0x11} ** 32;
    const second = [_]u8{0x22} ** 32;
    _ = try interner.insert(gpa, &first);
    _ = try interner.insert(gpa, &second);
    try interner.validateSemanticState(32);

    const saved_first_range = interner.ranges.items.items[0];
    interner.ranges.items.items[0].start = 1;
    try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(32));
    interner.ranges.items.items[0] = saved_first_range;
    interner.ranges.items.items[0].len = 31;
    try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(32));
    interner.ranges.items.items[0] = saved_first_range;

    const saved_cells = try gpa.dupe(u32, interner.index.items.items);
    defer gpa.free(saved_cells);
    const occupied = for (interner.index.items.items, 0..) |cell, index| {
        if (cell != 0) break index;
    } else return error.TestUnexpectedResult;
    interner.index.items.items[occupied] = @intCast(interner.ranges.items.items.len + 1);
    try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(32));
    @memcpy(interner.index.items.items, saved_cells);

    @memset(interner.index.items.items, 1);
    try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(32));
    @memcpy(interner.index.items.items, saved_cells);
    try interner.validateSemanticState(32);
}

test "SerialStringInterner semantic validation rejects range coverage counts and capacity corruption" {
    const gpa = testing.allocator;

    {
        var interner = try SerialStringInterner.initCapacity(gpa, 2);
        defer interner.deinit(gpa);
        _ = try interner.insert(gpa, "alpha");
        _ = try interner.insert(gpa, "bravo");
        interner.ranges.items.items[1].start += 1;
        try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(null));
    }
    {
        var interner = try SerialStringInterner.initCapacity(gpa, 1);
        defer interner.deinit(gpa);
        _ = try interner.insert(gpa, "alpha");
        interner.ranges.items.items[0].len = std.math.maxInt(u32);
        try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(null));
    }
    {
        var interner = try SerialStringInterner.initCapacity(gpa, 1);
        defer interner.deinit(gpa);
        _ = try interner.insert(gpa, "alpha");
        _ = try interner.bytes.append(gpa, 'x');
        try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(null));
    }
    {
        var interner = try SerialStringInterner.initCapacity(gpa, 2);
        defer interner.deinit(gpa);
        _ = try interner.insert(gpa, "alpha");
        interner.index.items.items.len -= 1;
        try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(null));
    }
    {
        var interner = try SerialStringInterner.initCapacity(gpa, 1);
        defer interner.deinit(gpa);
        _ = try interner.insert(gpa, "alpha");
        @memset(interner.index.items.items, 1);
        try testing.expectError(error.CorruptArtifact, interner.validateSemanticState(null));
    }
}

test "SerialStringInterner semantic validation handles a large canonical name table" {
    const gpa = testing.allocator;
    const name_count = 12_000;
    var interner = try SerialStringInterner.initCapacity(gpa, name_count);
    defer interner.deinit(gpa);

    var name_number: usize = 0;
    while (name_number < name_count) : (name_number += 1) {
        var buffer: [32]u8 = undefined;
        const text = try std.fmt.bufPrint(&buffer, "canonical-name-{d}", .{name_number});
        _ = try interner.insert(gpa, text);
    }

    try testing.expectEqual(@as(u32, name_count), interner.count());
    try interner.validateSemanticState(null);
    try testing.expectEqual(@as(?u32, 0), interner.lookup("canonical-name-0"));
    try testing.expectEqual(@as(?u32, name_count - 1), interner.lookup("canonical-name-11999"));
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

test "SerialStringInterner epoch deltas own consecutive immutable suffixes" {
    const gpa = testing.allocator;
    var source: SerialStringInterner = .{};
    const start = source.epochBoundary();
    var empty = try EpochDelta.capture(gpa, &source, start, start);
    defer empty.deinit();
    try testing.expectEqual(@as(usize, 0), empty.bytes.len);
    try testing.expectEqual(@as(usize, 0), empty.ranges.len);

    try testing.expectEqual(@as(u32, 0), try source.insert(gpa, "alpha"));
    try testing.expectEqual(@as(u32, 1), try source.insert(gpa, ""));
    const middle = source.epochBoundary();
    var first = try EpochDelta.capture(gpa, &source, start, middle);
    defer first.deinit();

    try testing.expectEqual(@as(u32, 2), try source.insert(gpa, "omega"));
    const end = source.epochBoundary();
    var second = try EpochDelta.capture(gpa, &source, middle, end);
    defer second.deinit();

    // Force source growth, then destroy it before reading either owned segment.
    var index: usize = 0;
    while (index < 256) : (index += 1) {
        var buffer: [32]u8 = undefined;
        const text = try std.fmt.bufPrint(&buffer, "growth-{d}", .{index});
        _ = try source.insert(gpa, text);
    }
    source.deinit(gpa);

    try testing.expectEqualStrings("alpha", first.getText(0).?);
    try testing.expectEqualStrings("", first.getText(1).?);
    try testing.expect(first.getText(2) == null);
    try testing.expectEqualStrings("omega", second.getText(2).?);

    var destination: SerialStringInterner = .{};
    defer destination.deinit(gpa);
    try empty.appendTo(&destination, gpa);
    try first.appendTo(&destination, gpa);
    try second.appendTo(&destination, gpa);
    try testing.expectEqualStrings("alpha", destination.getText(0));
    try testing.expectEqualStrings("", destination.getText(1));
    try testing.expectEqualStrings("omega", destination.getText(2));
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

test "SerialStringInterner: savepoints compose nested commit and rollback" {
    const gpa = testing.allocator;
    var interner = try SerialStringInterner.initCapacity(gpa, 12);
    defer interner.deinit(gpa);

    var name_buffer: [16]u8 = undefined;
    for (0..12) |i| {
        _ = try interner.insert(gpa, try std.fmt.bufPrint(&name_buffer, "seed-{d}", .{i}));
    }
    const baseline_bytes = try gpa.dupe(u8, interner.bytes.items.items);
    defer gpa.free(baseline_bytes);
    const baseline_ranges = try gpa.dupe(Range, interner.ranges.items.items);
    defer gpa.free(baseline_ranges);
    const baseline_index = try gpa.dupe(u32, interner.index.items.items);
    defer gpa.free(baseline_index);
    const baseline_count = interner.count();

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
    try testing.expect(interner.lookup("inner-committed") != null);

    interner.rollbackToSavepoint(&outer);
    outer_open = false;
    try testing.expectEqual(@as(?u32, 0), interner.lookup("seed-0"));
    try testing.expectEqual(@as(?u32, null), interner.lookup("outer-discarded"));
    try testing.expectEqual(@as(?u32, null), interner.lookup("inner-committed"));
    try testing.expectEqual(baseline_count, interner.count());
    try testing.expectEqualSlices(u8, baseline_bytes, interner.bytes.items.items);
    try testing.expectEqualSlices(Range, baseline_ranges, interner.ranges.items.items);
    try testing.expectEqualSlices(u32, baseline_index, interner.index.items.items);

    var committed_outer = try interner.createSavepoint(gpa);
    var committed_outer_open = true;
    errdefer if (committed_outer_open) interner.rollbackToSavepoint(&committed_outer);
    const kept = try interner.insert(gpa, "outer-kept");
    const kept_bytes = try gpa.dupe(u8, interner.bytes.items.items);
    defer gpa.free(kept_bytes);
    const kept_ranges = try gpa.dupe(Range, interner.ranges.items.items);
    defer gpa.free(kept_ranges);
    const kept_index = try gpa.dupe(u32, interner.index.items.items);
    defer gpa.free(kept_index);
    const kept_count = interner.count();

    var rolled_back_inner = try interner.createSavepoint(gpa);
    var rolled_back_inner_open = true;
    errdefer if (rolled_back_inner_open) interner.rollbackToSavepoint(&rolled_back_inner);
    _ = try interner.insert(gpa, "inner-discarded");
    interner.rollbackToSavepoint(&rolled_back_inner);
    rolled_back_inner_open = false;
    interner.commitSavepoint(&committed_outer);
    committed_outer_open = false;

    try testing.expectEqual(@as(?u32, 0), interner.lookup("seed-0"));
    try testing.expectEqual(@as(?u32, kept), interner.lookup("outer-kept"));
    try testing.expectEqual(@as(?u32, null), interner.lookup("inner-discarded"));
    try testing.expectEqual(kept_count, interner.count());
    try testing.expectEqualSlices(u8, kept_bytes, interner.bytes.items.items);
    try testing.expectEqualSlices(Range, kept_ranges, interner.ranges.items.items);
    try testing.expectEqualSlices(u32, kept_index, interner.index.items.items);
    try testing.expectEqual(@as(u16, 0), interner.savepoint_depth);
    try interner.validateSemanticState(null);
}

test "SerialStringInterner: savepoint rollback makes induced OOM atomic" {
    const gpa = testing.allocator;
    const candidate = [_]u8{'x'} ** 64;
    var saw_mutation_oom = false;
    var reached_success = false;

    for (0..8) |fail_index| {
        var interner = try SerialStringInterner.initCapacity(gpa, 2);
        _ = try interner.insert(gpa, "a");
        _ = try interner.insert(gpa, "b");

        const before_bytes = try gpa.dupe(u8, interner.bytes.items.items);
        defer gpa.free(before_bytes);
        const before_ranges = try gpa.dupe(Range, interner.ranges.items.items);
        defer gpa.free(before_ranges);
        const before_index = try gpa.dupe(u32, interner.index.items.items);
        defer gpa.free(before_index);
        const before_count = interner.count();

        var failing = testing.FailingAllocator.init(gpa, .{
            .fail_index = fail_index,
            .resize_fail_index = 0,
        });
        const failing_gpa = failing.allocator();
        defer interner.deinit(failing_gpa);

        var savepoint = interner.createSavepoint(failing_gpa) catch |err| {
            try testing.expectEqual(error.OutOfMemory, err);
            try testing.expect(failing.has_induced_failure);
            try testing.expectEqual(before_count, interner.count());
            try testing.expectEqualSlices(u8, before_bytes, interner.bytes.items.items);
            try testing.expectEqualSlices(Range, before_ranges, interner.ranges.items.items);
            try testing.expectEqualSlices(u32, before_index, interner.index.items.items);
            try testing.expectEqual(@as(u16, 0), interner.savepoint_depth);
            continue;
        };

        const insertion = interner.insert(failing_gpa, &candidate);
        if (insertion) |_| {
            interner.rollbackToSavepoint(&savepoint);
            reached_success = true;
        } else |err| {
            interner.rollbackToSavepoint(&savepoint);
            try testing.expectEqual(error.OutOfMemory, err);
            try testing.expect(failing.has_induced_failure);
            saw_mutation_oom = true;
        }

        try testing.expectEqual(before_count, interner.count());
        try testing.expectEqualSlices(u8, before_bytes, interner.bytes.items.items);
        try testing.expectEqualSlices(Range, before_ranges, interner.ranges.items.items);
        try testing.expectEqualSlices(u32, before_index, interner.index.items.items);
        try testing.expectEqual(@as(?u32, null), interner.lookup(&candidate));
        try testing.expectEqual(@as(u16, 0), interner.savepoint_depth);
        try interner.validateSemanticState(null);
        if (reached_success) break;
    }

    try testing.expect(saw_mutation_oom);
    try testing.expect(reached_success);
}
