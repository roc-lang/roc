//! Literal values persisted across compilations of one program (design.md
//! "Compile-Time Value Reuse Across Edits").
//!
//! A program's per-program literal roots belong to no checked module, so no
//! checked cache entry holds their values. One file per program root module
//! holds them instead: each entry is a literal root's value identity, its value
//! graph, and the compile-time roots its evaluation read, named by module and
//! site with the value identity each had. A compilation loads the file its
//! program's root module last wrote, restores every literal root whose value
//! identity names an entry whose reads still apply, and rewrites the file with
//! the values of its own literal roots, so the file holds only the current
//! program's literals.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const collections = @import("collections");
const lir = @import("lir");

const Common = struct {
    const LiteralValueLookup = lir.CheckedPipeline.LiteralValueLookup;
    const LiteralValueHit = lir.CheckedPipeline.LiteralValueHit;
    const LiteralValueRead = lir.CheckedPipeline.LiteralValueRead;
};
const Allocator = std.mem.Allocator;
const ConstStore = check.ConstStore.ConstStore;
const ConstNodeId = check.ConstStore.ConstNodeId;
const CompactWriter = collections.CompactWriter;
const alignment = collections.SERIALIZATION_ALIGNMENT;

const magic = "roc-literal-values-v1\x00".*;
const header_len = magic.len + 32 + 4 * 4;
const entry_len = 32 + 3 * 4;
const read_len = 4 + 3 * 32;

/// One stored literal value: its value identity, its value graph's root in
/// the file's const store, and its range of reads.
pub const Entry = struct {
    identity: [32]u8,
    root: ConstNodeId,
    reads_start: u32,
    reads_len: u32,
};

/// What one literal root of the compilation did, for `ROC_CTFE_REUSE_TRACE`
/// and tests.
pub const Record = struct {
    pub const Outcome = enum {
        /// Its conversion ran and its value has no identity.
        unnamed,
        /// Its conversion ran.
        converted,
        /// It restored a stored value.
        restored,
    };

    identity: ?[32]u8,
    outcome: Outcome,
    /// Its value went into the file this compilation writes.
    stored: bool,
    /// A compiler with runtime safety evaluated its conversion and compared
    /// the result with the restored value.
    verified: bool,
    /// The literal's source, owned.
    text: []u8,
};

/// The values the file a compilation loaded holds.
const Previous = struct {
    bytes: []align(alignment.toByteUnits()) u8,
    store: ConstStore,
    entries: []Entry,
    reads: []Common.LiteralValueRead,
    by_identity: std.AutoHashMapUnmanaged([32]u8, u32) = .empty,

    fn deinit(self: *Previous, allocator: Allocator) void {
        self.by_identity.deinit(allocator);
        allocator.free(self.reads);
        allocator.free(self.entries);
        self.store.deinit();
        allocator.free(self.bytes);
    }
};

/// The literal values of one program root module: those the file held when
/// the compilation started, and those the compilation writes.
pub const LiteralValues = struct {
    allocator: Allocator,
    version_hash: [32]u8,
    previous: ?Previous = null,
    next: ConstStore,
    next_entries: std.ArrayList(Entry) = .empty,
    next_reads: std.ArrayList(Common.LiteralValueRead) = .empty,
    next_identities: std.AutoHashMapUnmanaged([32]u8, void) = .empty,
    records: std.ArrayList(Record) = .empty,

    pub fn init(allocator: Allocator, version_hash: [32]u8) LiteralValues {
        return .{ .allocator = allocator, .version_hash = version_hash, .next = ConstStore.init(allocator) };
    }

    pub fn deinit(self: *LiteralValues) void {
        if (self.previous) |*previous| previous.deinit(self.allocator);
        self.next.deinit();
        self.next_entries.deinit(self.allocator);
        self.next_reads.deinit(self.allocator);
        self.next_identities.deinit(self.allocator);
        for (self.records.items) |record| self.allocator.free(record.text);
        self.records.deinit(self.allocator);
    }

    /// The previous values, by identity, for Monotype to restore.
    pub fn lookup(self: *const LiteralValues) Common.LiteralValueLookup {
        return .{ .context = self, .find = find };
    }

    fn find(context: *const anyopaque, identity: [32]u8) ?Common.LiteralValueHit {
        const self: *const LiteralValues = @ptrCast(@alignCast(context));
        const previous = &(self.previous orelse return null);
        const index = previous.by_identity.get(identity) orelse return null;
        const entry = previous.entries[index];
        return .{
            .store = &previous.store,
            .root = entry.root,
            .reads = previous.reads[entry.reads_start..][0..entry.reads_len],
        };
    }

    /// Adopt the contents of a file an earlier compilation wrote. A file
    /// with another version, or one that is truncated or malformed, holds
    /// nothing: the file is a cache, and its literal roots convert again.
    pub fn load(self: *LiteralValues, raw: []const u8) Allocator.Error!void {
        std.debug.assert(self.previous == null);
        if (raw.len < header_len) return;
        if (!std.mem.eql(u8, raw[0..magic.len], &magic)) return;
        if (!std.mem.eql(u8, raw[magic.len..][0..32], &self.version_hash)) return;
        var cursor: usize = magic.len + 32;
        const entry_count = readU32(raw, &cursor);
        const read_count = readU32(raw, &cursor);
        const store_offset = readU32(raw, &cursor);
        const store_len = readU32(raw, &cursor);
        const tables_end = @as(u64, header_len) + @as(u64, entry_count) * entry_len + @as(u64, read_count) * read_len;
        if (tables_end > store_offset or store_offset % alignment.toByteUnits() != 0) return;
        if (@as(u64, store_offset) + store_len != raw.len or store_len < @sizeOf(ConstStore.Serialized)) return;

        const bytes = try self.allocator.alignedAlloc(u8, alignment, store_len);
        var bytes_owned = true;
        defer if (bytes_owned) self.allocator.free(bytes);
        @memcpy(bytes, raw[store_offset..][0..store_len]);
        const serialized: *const ConstStore.Serialized = @ptrCast(@alignCast(bytes.ptr));
        check.validateSerialized(ConstStore.Serialized, serialized, bytes.len) catch return;

        const entries = try self.allocator.alloc(Entry, entry_count);
        var entries_owned = true;
        defer if (entries_owned) self.allocator.free(entries);
        for (entries) |*entry| {
            entry.identity = raw[cursor..][0..32].*;
            cursor += 32;
            entry.root = @fromBackingInt(readU32(raw, &cursor));
            entry.reads_start = readU32(raw, &cursor);
            entry.reads_len = readU32(raw, &cursor);
            if (@as(u64, entry.reads_start) + entry.reads_len > read_count) return;
        }
        const reads = try self.allocator.alloc(Common.LiteralValueRead, read_count);
        var reads_owned = true;
        defer if (reads_owned) self.allocator.free(reads);
        for (reads) |*read| {
            const flags = readU32(raw, &cursor);
            if (flags > 1) return;
            read.* = .{
                .in_top_module = flags == 1,
                .module = raw[cursor..][0..32].*,
                .site = raw[cursor + 32 ..][0..32].*,
                .value = raw[cursor + 64 ..][0..32].*,
            };
            cursor += 96;
        }

        var previous = Previous{
            .bytes = bytes,
            .store = serialized.deserializeWithAllocator(@intFromPtr(bytes.ptr), self.allocator),
            .entries = entries,
            .reads = reads,
        };
        bytes_owned = false;
        entries_owned = false;
        reads_owned = false;
        errdefer previous.deinit(self.allocator);
        for (entries, 0..) |entry, index| {
            if (!try previous.store.validValueGraph(entry.root)) {
                previous.deinit(self.allocator);
                return;
            }
            const slot = try previous.by_identity.getOrPut(self.allocator, entry.identity);
            if (!slot.found_existing) slot.value_ptr.* = @intCast(index);
        }
        self.previous = previous;
    }

    /// The stored entry `identity` names in the loaded file.
    pub fn previousEntry(self: *const LiteralValues, identity: [32]u8) ?Common.LiteralValueHit {
        return find(self, identity);
    }

    /// Write a literal root's value into the file this compilation writes:
    /// the value graph at `root` of `source`, and its reads.
    pub fn store(
        self: *LiteralValues,
        identity: [32]u8,
        source: *const ConstStore,
        root: ConstNodeId,
        reads: []const Common.LiteralValueRead,
    ) Allocator.Error!void {
        const seen = try self.next_identities.getOrPut(self.allocator, identity);
        if (seen.found_existing) return;
        const copied = try self.next.cloneValueFrom(source, root);
        const start: u32 = @intCast(self.next_reads.items.len);
        try self.next_reads.appendSlice(self.allocator, reads);
        try self.next_entries.append(self.allocator, .{
            .identity = identity,
            .root = copied,
            .reads_start = start,
            .reads_len = @intCast(reads.len),
        });
    }

    /// Carry a restored entry of the loaded file into the file this
    /// compilation writes.
    pub fn keep(self: *LiteralValues, identity: [32]u8) Allocator.Error!void {
        const hit = self.previousEntry(identity) orelse
            base.invariant("a restored literal value had no entry in the literal value file", .{});
        try self.store(identity, hit.store, hit.root, hit.reads);
    }

    pub fn addRecord(self: *LiteralValues, entry: Record) Allocator.Error!void {
        try self.records.append(self.allocator, entry);
    }

    /// The bytes of the file this compilation writes. Owned.
    pub fn encode(self: *const LiteralValues, allocator: Allocator) Allocator.Error![]u8 {
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        const arena_allocator = arena.allocator();
        var writer = CompactWriter.init();
        defer writer.deinit(arena_allocator);
        const serialized = try writer.appendAlloc(arena_allocator, ConstStore.Serialized);
        try serialized.serialize(&self.next, arena_allocator, &writer);

        const tables_end = header_len + self.next_entries.items.len * entry_len + self.next_reads.items.len * read_len;
        const store_offset = std.mem.alignForward(usize, tables_end, alignment.toByteUnits());
        const bytes = try allocator.alloc(u8, store_offset + writer.total_bytes);
        errdefer allocator.free(bytes);
        @memset(bytes, 0);
        var cursor: usize = 0;
        writeBytes(bytes, &cursor, &magic);
        writeBytes(bytes, &cursor, &self.version_hash);
        writeU32(bytes, &cursor, try countU32(self.next_entries.items.len));
        writeU32(bytes, &cursor, try countU32(self.next_reads.items.len));
        writeU32(bytes, &cursor, try countU32(store_offset));
        writeU32(bytes, &cursor, try countU32(writer.total_bytes));
        for (self.next_entries.items) |entry| {
            writeBytes(bytes, &cursor, &entry.identity);
            writeU32(bytes, &cursor, @backingInt(entry.root));
            writeU32(bytes, &cursor, entry.reads_start);
            writeU32(bytes, &cursor, entry.reads_len);
        }
        for (self.next_reads.items) |read| {
            writeU32(bytes, &cursor, @intFromBool(read.in_top_module));
            writeBytes(bytes, &cursor, &read.module);
            writeBytes(bytes, &cursor, &read.site);
            writeBytes(bytes, &cursor, &read.value);
        }
        const store_bytes = try allocator.alignedAlloc(u8, alignment, writer.total_bytes);
        defer allocator.free(store_bytes);
        _ = writer.writeToBuffer(store_bytes) catch unreachable;
        @memcpy(bytes[store_offset..], store_bytes);
        return bytes;
    }
};

fn countU32(value: usize) Allocator.Error!u32 {
    return std.math.cast(u32, value) orelse error.OutOfMemory;
}

fn readU32(raw: []const u8, cursor: *usize) u32 {
    const value = std.mem.readInt(u32, raw[cursor.*..][0..4], .little);
    cursor.* += 4;
    return value;
}

fn writeU32(bytes: []u8, cursor: *usize, value: u32) void {
    std.mem.writeInt(u32, bytes[cursor.*..][0..4], value, .little);
    cursor.* += 4;
}

fn writeBytes(bytes: []u8, cursor: *usize, source: []const u8) void {
    @memcpy(bytes[cursor.*..][0..source.len], source);
    cursor.* += source.len;
}

test "literal values round-trip through their file" {
    const gpa = std.testing.allocator;
    const version: [32]u8 = @splat(7);
    var values = LiteralValues.init(gpa, version);
    defer values.deinit();
    var scratch = ConstStore.init(gpa);
    defer scratch.deinit();
    const item = try scratch.append(.{ .scalar = .{ .u64 = 7 } });
    const list = try scratch.append(.{ .list = .{ .nodes = &.{item} } });
    const identity: [32]u8 = @splat(1);
    const reads = [_]Common.LiteralValueRead{.{ .in_top_module = true, .module = @splat(0), .site = @splat(2), .value = @splat(3) }};
    try values.store(identity, &scratch, list, &reads);
    const bytes = try values.encode(gpa);
    defer gpa.free(bytes);

    var loaded = LiteralValues.init(gpa, version);
    defer loaded.deinit();
    try loaded.load(bytes);
    const hit = loaded.previousEntry(identity).?;
    try std.testing.expect(try hit.store.graphEql(hit.root, &scratch, list));
    try std.testing.expectEqual(@as(usize, 1), hit.reads.len);
    try std.testing.expect(hit.reads[0].in_top_module);
    try std.testing.expectEqualSlices(u8, &reads[0].site, &hit.reads[0].site);

    // Another version, or a truncated file, holds nothing.
    var other = LiteralValues.init(gpa, @splat(8));
    defer other.deinit();
    try other.load(bytes);
    try std.testing.expect(other.previousEntry(identity) == null);
    var truncated = LiteralValues.init(gpa, version);
    defer truncated.deinit();
    try truncated.load(bytes[0 .. bytes.len - 1]);
    try std.testing.expect(truncated.previousEntry(identity) == null);
}
