//! Strings written inline in Roc code, e.g. `x = "abc"`.

const std = @import("std");
const invariant = @import("invariant.zig").invariant;
const Allocator = std.mem.Allocator;
const collections = @import("collections");
const testing = std.testing;

const CompactWriter = collections.CompactWriter;
const InternedBytes = @import("InternedBytes.zig");

/// The index of this string in a `Store`.
pub const Idx = enum(u32) {
    none = 0,
    _,

    pub fn isNone(self: Idx) bool {
        return self == .none;
    }
};

/// Durable storage for string literals.
pub const Store = struct {
    /// An Idx points to the first byte of the string. The entry immediately
    /// before it stores the byte length and maximum required runtime alignment
    /// in canonical little-endian form:
    ///
    /// | len: u32 little-endian | alignment: u32 little-endian | bytes... |
    ///
    /// This store is checked/cache data, not runtime static data. Target-specific
    /// string emission is responsible for building any runtime `RocStr` headers
    /// or static refcount words later.
    ///
    /// Note:
    /// Later we could change from fixed u32-s to variable lengthed
    /// sizes, encoded in reverse where for example,
    /// the first 7 bit would signal the length, the last bit would signal that the length
    /// continues to the previous byte
    buffer: collections.SafeList(u8) = .{},

    const len_size = @sizeOf(u32);
    const alignment_size = @sizeOf(u32);
    const entry_header_size = len_size + alignment_size;

    pub const Entry = struct {
        encoded_start: u32,
        idx: Idx,
        bytes: []const u8,
        alignment: u32,
    };

    pub const Iterator = struct {
        store: *const Store,
        pos: usize = 0,

        pub fn next(self: *Iterator) ?Entry {
            const buffer_items = self.store.buffer.items();

            while (true) {
                if (self.pos + entry_header_size > buffer_items.len) return null;

                const str_len = std.mem.readInt(u32, buffer_items[self.pos..][0..len_size], .little);
                const entry_alignment = std.mem.readInt(u32, buffer_items[self.pos + len_size ..][0..alignment_size], .little);
                const content_start = self.pos + entry_header_size;
                if (str_len > buffer_items.len - content_start) return null;
                const content_end = content_start + str_len;

                self.pos = content_end;
                return .{
                    .encoded_start = @intCast(content_start - entry_header_size),
                    .idx = @fromBackingInt(@intCast(@as(u32, @intCast(content_start)))),
                    .bytes = buffer_items[content_start..content_end],
                    .alignment = entry_alignment,
                };
            }
        }
    };

    /// Intiizalizes a `Store` with capacity `bytes` of space.
    /// Note this specifically is the number of bytes for storing strings.
    /// The string `hello, world!` will use 14 bytes including the null terminator.
    pub fn initCapacityBytes(gpa: std.mem.Allocator, bytes: usize) std.mem.Allocator.Error!Store {
        return .{
            .buffer = try collections.SafeList(u8).initCapacity(gpa, bytes),
        };
    }

    pub fn iterator(self: *const Store) Iterator {
        return .{ .store = self };
    }

    /// Number of bytes in the encoded entry buffer.
    pub fn byteCount(self: *const Store) u32 {
        return checkedU32(self.buffer.items().len, "string literal store byte count");
    }

    /// Whether `offset` is between complete encoded entries.
    pub fn isEntryBoundary(self: *const Store, offset: u32) bool {
        if (offset == 0) return true;
        var iterator_ = self.iterator();
        while (iterator_.next() != null) {
            if (iterator_.pos == offset) return true;
            if (iterator_.pos > offset) return false;
        }
        return false;
    }

    /// Encoded entries beginning at a previously captured boundary.
    pub fn encodedBytesFrom(self: *const Store, offset: u32) []const u8 {
        std.debug.assert(self.isEntryBoundary(offset));
        return self.buffer.items()[offset..];
    }

    /// Reserve encoded storage without changing the logical store.
    pub fn ensureUnusedEncodedCapacity(
        self: *Store,
        gpa: std.mem.Allocator,
        additional: usize,
    ) std.mem.Allocator.Error!void {
        try self.buffer.ensureUnusedCapacity(gpa, additional);
    }

    /// Append complete encoded entries after capacity has been reserved.
    pub fn appendEncodedAssumeCapacity(self: *Store, bytes: []const u8) void {
        _ = self.buffer.appendSliceAssumeCapacity(bytes);
    }

    /// Deinitialize a `Store`'s memory.
    pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
        self.buffer.deinit(gpa);
    }

    /// Clone this store into fresh owned memory.
    pub fn clone(self: *const Store, gpa: std.mem.Allocator) std.mem.Allocator.Error!Store {
        return .{
            .buffer = try self.buffer.clone(gpa),
        };
    }

    fn appendFresh(self: *Store, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
        // Lengths and content offsets are u32, so a store past that range has
        // exhausted the representable memory.
        const str_len = std.math.cast(u32, string.len) orelse return error.OutOfMemory;
        const entry_len = std.math.add(usize, entry_header_size, string.len) catch return error.OutOfMemory;
        const entry_end = std.math.add(usize, self.buffer.items().len, entry_len) catch return error.OutOfMemory;
        if (entry_end > std.math.maxInt(u32)) return error.OutOfMemory;

        // One reservation covers the header and the text, so the entry is
        // appended whole or not at all.
        try self.buffer.ensureUnusedCapacity(gpa, entry_len);

        var header: [entry_header_size]u8 = undefined;
        std.mem.writeInt(u32, header[0..len_size], str_len, .little);
        std.mem.writeInt(u32, header[len_size..][0..alignment_size], 1, .little);
        _ = self.buffer.appendSliceAssumeCapacity(&header);

        const idx: u32 = @intCast(self.buffer.items().len);
        _ = self.buffer.appendSliceAssumeCapacity(string);

        return @fromBackingInt(idx);
    }

    /// Get a string literal's text from this `Store`.
    pub fn get(self: *const Store, idx: Idx) []u8 {
        const idx_usize: usize = @backingInt(idx);
        const len_start = idx_usize - entry_header_size;
        const str_len = std.mem.readInt(u32, self.buffer.items()[len_start..][0..len_size], .little);
        return self.buffer.items()[idx_usize .. idx_usize + str_len];
    }

    pub fn alignment(self: *const Store, idx: Idx) u32 {
        const idx_usize: usize = @backingInt(idx);
        const alignment_start = idx_usize - alignment_size;
        return std.mem.readInt(u32, self.buffer.items()[alignment_start..][0..alignment_size], .little);
    }

    fn requireAlignment(self: *Store, idx: Idx, required: u32) void {
        if (required == 0 or !std.math.isPowerOfTwo(required)) {
            invariant("StringLiteral invariant violated: string literal alignment must be a nonzero power of two", .{});
        }
        const idx_usize: usize = @backingInt(idx);
        const alignment_start = idx_usize - alignment_size;
        const slot = self.buffer.items()[alignment_start..][0..alignment_size];
        const current = std.mem.readInt(u32, slot, .little);
        if (required > current) std.mem.writeInt(u32, slot, required, .little);
    }

    /// Serialized representation of a Store
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
        buffer: collections.SafeList(u8).Serialized,

        /// Serialize a Store into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            store: *const Store,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!void {
            // Serialize the byte buffer
            try self.buffer.serialize(&store.buffer, allocator, writer);
        }

        /// Deserialize into a Store value (no in-place modification of cache buffer).
        /// The base parameter is the base address of the serialized buffer in memory.
        pub fn deserializeInto(self: *const Serialized, base: usize) Store {
            return Store{
                .buffer = self.buffer.deserializeInto(base),
            };
        }
    };
};

/// The dedup table of a `StringLiteral.Store`: an open-addressed index from
/// string bytes to the entry holding them. It is serialized beside the store,
/// so a loaded store keeps deduplicating exactly as the store it was written
/// from would.
pub const BuilderState = struct {
    index: InternedBytes.Index(StringLiteralPolicy) = .{},

    /// Serialized form: the table's cell and fingerprint columns and its
    /// entry count, loaded in place.
    pub const Serialized = extern struct {
        cells: collections.SafeList(Idx).Serialized,
        fingerprints: collections.SafeList(u8).Serialized,
        len: u32,
        _padding: u32 = 0,

        /// Serialize `builder` into this struct, appending its columns to `writer`.
        pub fn serialize(
            self: *Serialized,
            builder: *const BuilderState,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!void {
            try self.cells.serialize(&builder.index.cells, allocator, writer);
            try self.fingerprints.serialize(&builder.index.fingerprints, allocator, writer);
            self.len = builder.index.len;
            self._padding = 0;
        }

        /// A table whose columns borrow the serialized columns at `base`.
        pub fn deserializeInto(self: *const Serialized, base: usize) BuilderState {
            return .{ .index = .{
                .cells = self.cells.deserializeInto(base),
                .fingerprints = self.fingerprints.deserializeInto(base),
                .len = self.len,
            } };
        }
    };

    pub fn deinit(self: *BuilderState, gpa: std.mem.Allocator) void {
        self.index.deinit(gpa);
    }

    pub fn clone(self: *const BuilderState, gpa: std.mem.Allocator) std.mem.Allocator.Error!BuilderState {
        return .{ .index = try self.index.clone(gpa) };
    }

    pub fn insert(self: *BuilderState, store: *Store, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
        return try self.insertAligned(store, gpa, string, 1);
    }

    pub fn insertAligned(self: *BuilderState, store: *Store, gpa: std.mem.Allocator, string: []const u8, alignment: u32) std.mem.Allocator.Error!Idx {
        var owner = BuilderOwner{ .store = store };
        const id = try self.index.insert(&owner, gpa, string);
        store.requireAlignment(id, alignment);
        return id;
    }

    /// Reserve the transient index before externally appended encoded entries
    /// become visible in `store`.
    pub fn ensureAdditionalCapacity(
        self: *BuilderState,
        store: *Store,
        gpa: std.mem.Allocator,
        additional: usize,
    ) std.mem.Allocator.Error!void {
        var owner = BuilderOwner{ .store = store };
        const total_count = std.math.add(
            usize,
            @as(usize, self.index.count()),
            additional,
        ) catch return error.OutOfMemory;
        try self.index.ensureTotalCapacity(
            &owner,
            gpa,
            total_count,
        );
    }

    /// Register one already-appended entry without allocating.
    pub fn registerExistingAssumeCapacity(
        self: *BuilderState,
        store: *Store,
        id: Idx,
    ) void {
        var owner = BuilderOwner{ .store = store };
        self.index.insertExistingAssumeCapacity(&owner, store.get(id), id);
    }
};

const BuilderOwner = struct {
    store: *Store,
};

const StringLiteralPolicy = struct {
    pub const Id = Idx;
    pub const Cell = Idx;
    pub const empty_cell: Cell = .none;
    pub const initial_index_capacity: usize = 16;
    pub const use_fingerprints = true;

    pub fn cellForId(id: Id) Cell {
        return id;
    }

    pub fn idFromCell(cell: Cell) Id {
        return cell;
    }

    pub fn textForId(owner: anytype, id: Id) []const u8 {
        return owner.store.get(id);
    }

    pub fn appendEntry(owner: anytype, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Id {
        return owner.store.appendFresh(gpa, string);
    }

    pub fn hash(string: []const u8) u64 {
        return InternedBytes.hash(string);
    }
};

fn checkedU32(value: usize, comptime invariant_name: []const u8) u32 {
    if (value > std.math.maxInt(u32)) {
        invariant("{s} exceeded u32 storage invariant", .{invariant_name});
    }
    return @intCast(value);
}

fn expectedNextStringContentStart(previous_end: *usize, string_len: usize) u32 {
    const content_start = previous_end.* + Store.entry_header_size;
    previous_end.* = content_start + string_len;
    return @intCast(content_start);
}

test "insert" {
    const gpa = std.testing.allocator;

    var interner = Store{};
    defer interner.deinit(gpa);
    var builder = BuilderState{};
    defer builder.deinit(gpa);

    const str_1 = "abc".*;
    const str_2 = "defg".*;
    const idx_1 = try builder.insert(&interner, gpa, &str_1);
    const idx_2 = try builder.insert(&interner, gpa, &str_2);

    try std.testing.expectEqualStrings("abc", interner.get(idx_1));
    try std.testing.expectEqualStrings("defg", interner.get(idx_2));
}

test "builder deduplicates exact string literal bytes" {
    const gpa = std.testing.allocator;

    var store = Store{};
    defer store.deinit(gpa);
    var builder = BuilderState{};
    defer builder.deinit(gpa);

    const empty1 = try builder.insert(&store, gpa, "");
    const empty2 = try builder.insert(&store, gpa, "");
    const binary1 = try builder.insert(&store, gpa, "\x00\x01abc");
    const binary2 = try builder.insert(&store, gpa, "\x00\x01abc");
    const binary3 = try builder.insert(&store, gpa, "\x00\x01abd");

    try testing.expectEqual(empty1, empty2);
    try testing.expectEqual(binary1, binary2);
    try testing.expect(binary1 != binary3);
    try testing.expectEqualStrings("", store.get(empty1));
    try testing.expectEqualStrings("\x00\x01abc", store.get(binary1));
    try testing.expectEqualStrings("\x00\x01abd", store.get(binary3));
}

test "store uses exact portable length-prefixed bytes" {
    const gpa = std.testing.allocator;

    var store = Store{};
    defer store.deinit(gpa);
    var builder = BuilderState{};
    defer builder.deinit(gpa);

    const empty = try builder.insert(&store, gpa, "");
    const abc = try builder.insert(&store, gpa, "abc");
    const binary = try builder.insert(&store, gpa, "\x00\x01abc");

    try testing.expectEqual(@as(u32, 8), @backingInt(empty));
    try testing.expectEqual(@as(u32, 16), @backingInt(abc));
    try testing.expectEqual(@as(u32, 27), @backingInt(binary));
    try testing.expectEqualStrings("", store.get(empty));
    try testing.expectEqualStrings("abc", store.get(abc));
    try testing.expectEqualStrings("\x00\x01abc", store.get(binary));

    const expected = [_]u8{
        0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
        0x03, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00,
        'a',  'b',  'c',  0x05, 0x00, 0x00, 0x00, 0x01,
        0x00, 0x00, 0x00, 0x00, 0x01, 'a',  'b',  'c',
    };
    try testing.expectEqualSlices(u8, &expected, store.buffer.items());
}

test "deduplicated literal backing keeps maximum requested alignment" {
    const gpa = std.testing.allocator;

    var store = Store{};
    defer store.deinit(gpa);
    var builder = BuilderState{};
    defer builder.deinit(gpa);

    const plain = try builder.insert(&store, gpa, "shared bytes");
    const aligned = try builder.insertAligned(&store, gpa, "shared bytes", 16);

    try testing.expectEqual(plain, aligned);
    try testing.expectEqual(@as(u32, 16), store.alignment(plain));
}

test "Store empty CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create an empty Store
    var original = Store{};
    defer original.deinit(gpa);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_empty_stringlit.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter with arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, Store.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized = &deserialized_value;

    // Verify empty
    try std.testing.expectEqual(@as(usize, 0), deserialized.buffer.len());
}

test "Store basic CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create original store and add some strings
    var original = Store{};
    defer original.deinit(gpa);
    var builder = BuilderState{};
    defer builder.deinit(gpa);

    const idx1 = try builder.insert(&original, gpa, "hello");
    const idx2 = try builder.insert(&original, gpa, "world");
    const idx3 = try builder.insert(&original, gpa, "foo bar baz");

    // Verify original values
    try std.testing.expectEqualStrings("hello", original.get(idx1));
    try std.testing.expectEqualStrings("world", original.get(idx2));
    try std.testing.expectEqualStrings("foo bar baz", original.get(idx3));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_basic_stringlit.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using CompactWriter with arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, Store.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized = &deserialized_value;

    // Verify the strings are accessible
    try std.testing.expectEqualStrings("hello", deserialized.get(idx1));
    try std.testing.expectEqualStrings("world", deserialized.get(idx2));
    try std.testing.expectEqualStrings("foo bar baz", deserialized.get(idx3));
}

test "Store comprehensive CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    var original = Store{};
    defer original.deinit(gpa);
    var builder = BuilderState{};
    defer builder.deinit(gpa);

    // Test various string types
    const test_strings = [_][]const u8{
        "", // empty string
        "a", // single character
        "hello world", // simple string
        "🦎🚀✨", // emojis
        "日本語テキスト", // non-Latin script
        "\x00\x01\x02\x03", // binary data
        "line1\nline2\r\nline3", // line breaks
        "tab\tseparated\tvalues", // tabs
        "quotes: 'single' and \"double\"", // quotes
        repeated: {
            const pattern = "very long string ";
            var result: [pattern.len * (50)]@TypeOf(pattern[0]) = undefined;
            for (0..(50)) |i| @memcpy(result[i * pattern.len ..][0..pattern.len], pattern);
            break :repeated &result;
        }, // long string
    };

    var indices = std.ArrayList(Idx).empty;
    defer indices.deinit(gpa);

    for (test_strings) |str| {
        const idx = try builder.insert(&original, gpa, str);
        try indices.append(gpa, idx);
    }

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_comprehensive_stringlit.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, Store.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized = &deserialized_value;

    // Verify all strings
    for (test_strings, 0..) |expected_str, i| {
        const idx = indices.items[i];
        const actual_str = deserialized.get(idx);
        try std.testing.expectEqualStrings(expected_str, actual_str);
    }
}

test "Store CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create and populate store
    var original = Store{};
    defer original.deinit(gpa);
    var builder = BuilderState{};
    defer builder.deinit(gpa);

    const idx1 = try builder.insert(&original, gpa, "test1");
    const idx2 = try builder.insert(&original, gpa, "test2");
    try std.testing.expect(@backingInt(idx1) < @backingInt(idx2));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_frozen_stringlit.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, Store.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized = &deserialized_value;

    // Verify the strings are accessible
    try std.testing.expectEqualStrings("test1", deserialized.get(idx1));
    try std.testing.expectEqualStrings("test2", deserialized.get(idx2));
}

test "Store.Serialized roundtrip" {
    const gpa = std.testing.allocator;

    // Create original store and add some strings
    var original = Store{};
    defer original.deinit(gpa);
    var builder = BuilderState{};
    defer builder.deinit(gpa);

    const idx1 = try builder.insert(&original, gpa, "hello");
    const idx2 = try builder.insert(&original, gpa, "world");
    const idx3 = try builder.insert(&original, gpa, "foo bar baz");

    // Create a CompactWriter and arena
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    const io = std.testing.io;
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile(io, "test.compact", .{ .read = true });
    defer tmp_file.close(io);

    var writer = CompactWriter.init();
    defer writer.deinit(arena_alloc);

    // Allocate and serialize using the Serialized struct
    const serialized_ptr = try writer.appendAlloc(arena_alloc, Store.Serialized);
    try serialized_ptr.serialize(&original, arena_alloc, &writer);

    // Write to file
    try writer.writeGather(tmp_file, io);

    // Read back
    const file_size = try tmp_file.length(io);
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);
    _ = try tmp_file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_ptr = @as(*Store.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const store = deserialized_ptr.deserializeInto(@intFromPtr(buffer.ptr));

    // Verify the strings are accessible
    try std.testing.expectEqualStrings("hello", store.get(idx1));
    try std.testing.expectEqualStrings("world", store.get(idx2));
    try std.testing.expectEqualStrings("foo bar baz", store.get(idx3));
}

test "Store edge case indices CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    var original = Store{};
    defer original.deinit(gpa);
    var builder = BuilderState{};
    defer builder.deinit(gpa);

    // The index returned points to the first byte of the string content.
    // Test various scenarios that might stress the index calculation.
    var previous_end: usize = 0;

    const idx1 = try builder.insert(&original, gpa, "first");
    try std.testing.expectEqual(expectedNextStringContentStart(&previous_end, "first".len), @backingInt(idx1));

    const idx2 = try builder.insert(&original, gpa, "second");
    try std.testing.expectEqual(expectedNextStringContentStart(&previous_end, "second".len), @backingInt(idx2));

    const idx3 = try builder.insert(&original, gpa, "");
    try std.testing.expectEqual(expectedNextStringContentStart(&previous_end, "".len), @backingInt(idx3));

    const long_str = @as([1000]u8, @splat('x'));
    const idx4 = try builder.insert(&original, gpa, &long_str);
    try std.testing.expectEqual(expectedNextStringContentStart(&previous_end, long_str.len), @backingInt(idx4));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const io = std.testing.io;
    const file = try tmp_dir.dir.createFile(io, "test_edge_indices_stringlit.dat", .{ .read = true });
    defer file.close(io);

    // Serialize using arena allocator
    var arena = collections.SingleThreadArena.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    const serialized = try writer.appendAlloc(arena_allocator, Store.Serialized);
    try serialized.serialize(&original, arena_allocator, &writer);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Deserialize
    const deserialized_serialized: *const Store.Serialized = @ptrCast(@alignCast(buffer.ptr));
    var deserialized_value = deserialized_serialized.deserializeInto(@intFromPtr(buffer.ptr));
    const deserialized = &deserialized_value;

    // Verify all strings with their exact indices
    try std.testing.expectEqualStrings("first", deserialized.get(idx1));
    try std.testing.expectEqualStrings("second", deserialized.get(idx2));
    try std.testing.expectEqualStrings("", deserialized.get(idx3));
    try std.testing.expectEqualStrings(&long_str, deserialized.get(idx4));
}

test "a loaded dedup table deduplicates and appends exactly like the table it was written from" {
    const gpa = std.testing.allocator;

    var store = Store{};
    defer store.deinit(gpa);
    var builder = BuilderState{};
    defer builder.deinit(gpa);
    var names: [40][16]u8 = undefined;
    for (&names, 0..) |*buf, i| _ = try builder.insert(&store, gpa, try std.fmt.bufPrint(buf, "literal {d}", .{i}));

    var writer = CompactWriter.init();
    defer writer.deinit(gpa);
    const Header = extern struct { store: Store.Serialized, builder: BuilderState.Serialized };
    const header = try writer.appendAlloc(gpa, Header);
    try header.store.serialize(&store, gpa, &writer);
    try header.builder.serialize(&builder, gpa, &writer);
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, writer.total_bytes);
    defer gpa.free(buffer);
    _ = try writer.writeToBuffer(buffer);

    const loaded_header: *const Header = @ptrCast(@alignCast(buffer.ptr));
    var loaded_store = loaded_header.store.deserializeInto(@intFromPtr(buffer.ptr));
    defer loaded_store.deinit(gpa);
    var loaded_builder = loaded_header.builder.deserializeInto(@intFromPtr(buffer.ptr));
    defer loaded_builder.deinit(gpa);

    // Existing strings come back with their original index, with no rehash.
    for (&names, 0..) |*buf, i| {
        const text = try std.fmt.bufPrint(buf, "literal {d}", .{i});
        try std.testing.expectEqual(try builder.insert(&store, gpa, text), try loaded_builder.insert(&loaded_store, gpa, text));
    }
    // New strings get the index the original store would give them.
    for (0..40) |i| {
        var buf: [16]u8 = undefined;
        const text = try std.fmt.bufPrint(&buf, "fresh {d}", .{i});
        try std.testing.expectEqual(try builder.insert(&store, gpa, text), try loaded_builder.insert(&loaded_store, gpa, text));
    }
    try std.testing.expectEqualSlices(u8, store.buffer.items(), loaded_store.buffer.items());
}
