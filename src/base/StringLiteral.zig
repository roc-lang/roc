//! Strings written inline in Roc code, e.g. `x = "abc"`.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
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
    /// before it stores the string length in canonical little-endian form:
    ///
    /// | len: u32 little-endian | bytes... |
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
    buffer: Buffer = .{},

    const len_size = @sizeOf(u32);
    const entry_header_size = len_size;

    pub const Entry = struct {
        idx: Idx,
        bytes: []const u8,
    };

    pub const Iterator = struct {
        store: *const Store,
        pos: usize = 0,

        pub fn next(self: *Iterator) ?Entry {
            const buffer_items = self.store.buffer.items.items;

            while (true) {
                if (self.pos + entry_header_size > buffer_items.len) return null;

                const str_len = std.mem.readInt(u32, buffer_items[self.pos..][0..len_size], .little);
                const content_start = self.pos + entry_header_size;
                if (str_len > buffer_items.len - content_start) return null;
                const content_end = content_start + str_len;

                self.pos = content_end;
                return .{
                    .idx = @enumFromInt(@as(u32, @intCast(content_start))),
                    .bytes = buffer_items[content_start..content_end],
                };
            }
        }
    };

    pub const Buffer = struct {
        items: std.ArrayList(u8) = .empty,

        const SerializedDataRef = struct {
            offset: usize,
            len: usize,
            capacity: usize,
        };

        pub const Serialized = extern struct {
            offset: i64,
            len: u64,
            capacity: u64,

            pub fn serialize(
                self: *@This(),
                buffer: *const Buffer,
                allocator: std.mem.Allocator,
                writer: *CompactWriter,
            ) std.mem.Allocator.Error!void {
                const data_ref = try buffer.writeData(allocator, writer);

                self.offset = @intCast(data_ref.offset);
                self.len = @intCast(data_ref.len);
                self.capacity = @intCast(data_ref.capacity);
            }

            pub fn deserializeInto(self: *const @This(), base: usize) Buffer {
                if (self.capacity == 0) {
                    return Buffer{};
                }

                const items_ptr: [*]u8 = @ptrFromInt(base +% @as(usize, @intCast(self.offset)));

                return Buffer{
                    .items = .{
                        .items = items_ptr[0..@intCast(self.len)],
                        .capacity = @intCast(self.capacity),
                    },
                };
            }
        };

        pub fn initCapacity(gpa: std.mem.Allocator, bytes: usize) std.mem.Allocator.Error!Buffer {
            return .{
                .items = try std.ArrayList(u8).initCapacity(gpa, bytes),
            };
        }

        pub fn deinit(self: *Buffer, gpa: std.mem.Allocator) void {
            self.items.deinit(gpa);
        }

        pub fn clone(self: *const Buffer, gpa: std.mem.Allocator) std.mem.Allocator.Error!Buffer {
            return .{
                .items = try self.items.clone(gpa),
            };
        }

        pub fn len(self: *const Buffer) usize {
            return self.items.items.len;
        }

        pub fn append(self: *Buffer, gpa: std.mem.Allocator, byte: u8) std.mem.Allocator.Error!usize {
            const start = self.items.items.len;
            try self.items.append(gpa, byte);
            return start;
        }

        pub fn appendSlice(self: *Buffer, gpa: std.mem.Allocator, bytes: []const u8) std.mem.Allocator.Error!usize {
            const start = self.items.items.len;
            try self.items.appendSlice(gpa, bytes);
            return start;
        }

        pub fn serialize(
            self: *const Buffer,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!*const Buffer {
            const offset_self = try writer.appendAlloc(allocator, Buffer);
            offset_self.* = try self.toOffsetBuffer(allocator, writer);
            return @constCast(offset_self);
        }

        pub fn relocate(self: *Buffer, offset: isize) void {
            if (self.items.capacity == 0) return;

            const old_addr: isize = @intCast(@intFromPtr(self.items.items.ptr));
            const new_addr = @as(usize, @intCast(old_addr + offset));
            self.items.items.ptr = @ptrFromInt(new_addr);
        }

        pub fn fromMappedSlice(items: []u8, capacity: usize) Buffer {
            return .{
                .items = .{
                    .items = items,
                    .capacity = capacity,
                },
            };
        }

        fn toOffsetBuffer(
            self: *const Buffer,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!Buffer {
            const data_ref = try self.writeData(allocator, writer);

            if (data_ref.capacity == 0) {
                return Buffer{};
            }

            const items_ptr: [*]u8 = @ptrFromInt(data_ref.offset);

            return Buffer{
                .items = .{
                    .items = items_ptr[0..data_ref.len],
                    .capacity = data_ref.capacity,
                },
            };
        }

        fn writeData(
            self: *const Buffer,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!SerializedDataRef {
            if (self.items.items.len == 0) {
                return .{ .offset = 0, .len = 0, .capacity = 0 };
            }

            const data_offset = writer.total_bytes;
            const bytes: []const u8 = self.items.items;
            _ = try writer.appendSlice(allocator, bytes);

            return .{
                .offset = data_offset,
                .len = self.items.items.len,
                .capacity = self.items.items.len,
            };
        }
    };

    /// Intiizalizes a `Store` with capacity `bytes` of space.
    /// Note this specifically is the number of bytes for storing strings.
    /// The string `hello, world!` will use 14 bytes including the null terminator.
    pub fn initCapacityBytes(gpa: std.mem.Allocator, bytes: usize) std.mem.Allocator.Error!Store {
        return .{
            .buffer = try Buffer.initCapacity(gpa, bytes),
        };
    }

    pub fn iterator(self: *const Store) Iterator {
        return .{ .store = self };
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
        const str_len = checkedU32(string.len, "string literal length");

        var str_len_bytes: [len_size]u8 = undefined;
        std.mem.writeInt(u32, &str_len_bytes, str_len, .little);
        {
            const expected_start = self.buffer.items.items.len;
            const start = try self.buffer.appendSlice(gpa, &str_len_bytes);
            assertAppendRange(expected_start, str_len_bytes.len, start, str_len_bytes.len);
        }

        const string_content_start = self.buffer.len();
        const idx = checkedU32(string_content_start, "string literal content offset");

        {
            const expected_start = self.buffer.items.items.len;
            const start = try self.buffer.appendSlice(gpa, string);
            assertAppendRange(expected_start, string.len, start, string.len);
        }

        return @enumFromInt(idx);
    }

    /// Get a string literal's text from this `Store`.
    pub fn get(self: *const Store, idx: Idx) []u8 {
        const idx_usize: usize = @intFromEnum(idx);
        const len_start = idx_usize - entry_header_size;
        const str_len = std.mem.readInt(u32, self.buffer.items.items[len_start..][0..len_size], .little);
        return self.buffer.items.items[idx_usize .. idx_usize + str_len];
    }

    /// Serialize this Store to the given CompactWriter. The resulting Store
    /// in the writer's buffer will have offsets instead of pointers. Calling any
    /// methods on it or dereferencing its internal "pointers" (which are now
    /// offsets) is illegal behavior!
    pub fn serialize(
        self: *const Store,
        allocator: std.mem.Allocator,
        writer: *CompactWriter,
    ) std.mem.Allocator.Error!*const Store {
        // First, write the Store struct itself
        const offset_self = try writer.appendAlloc(allocator, Store);

        // Then serialize the byte buffer and update the struct
        offset_self.* = .{
            .buffer = (try self.buffer.serialize(allocator, writer)).*,
        };

        return @constCast(offset_self);
    }

    /// Add the given offset to the memory addresses of all pointers in `self`.
    pub fn relocate(self: *Store, offset: isize) void {
        self.buffer.relocate(offset);
    }

    /// Serialized representation of a Store
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
        buffer: Buffer.Serialized,

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

/// Transient dedup state for constructing a `StringLiteral.Store`.
pub const BuilderState = struct {
    index: InternedBytes.Index(StringLiteralPolicy) = .{},

    pub fn deinit(self: *BuilderState, gpa: std.mem.Allocator) void {
        self.index.deinit(gpa);
    }

    pub fn clone(self: *const BuilderState, gpa: std.mem.Allocator) std.mem.Allocator.Error!BuilderState {
        return .{ .index = try self.index.clone(gpa) };
    }

    pub fn insert(self: *BuilderState, store: *Store, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!Idx {
        var owner = BuilderOwner{ .store = store };
        return self.index.insert(&owner, gpa, string);
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

    pub fn entryCount(_: anytype, index: *const InternedBytes.Index(StringLiteralPolicy)) u32 {
        return index.len;
    }

    pub fn hash(string: []const u8) u64 {
        return InternedBytes.hash(string);
    }
};

fn checkedU32(value: usize, comptime invariant_name: []const u8) u32 {
    if (value > std.math.maxInt(u32)) {
        if (comptime builtin.mode == .Debug) {
            std.debug.panic("{s} exceeded u32 storage invariant", .{invariant_name});
        }
        unreachable;
    }
    return @intCast(value);
}

fn assertAppendRange(expected_start: usize, expected_len: usize, actual_start: usize, actual_len: usize) void {
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(actual_start == expected_start);
        std.debug.assert(actual_len == expected_len);
    } else if (actual_start != expected_start or actual_len != expected_len) {
        unreachable;
    }
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

    try testing.expectEqual(@as(u32, 4), @intFromEnum(empty));
    try testing.expectEqual(@as(u32, 8), @intFromEnum(abc));
    try testing.expectEqual(@as(u32, 15), @intFromEnum(binary));
    try testing.expectEqualStrings("", store.get(empty));
    try testing.expectEqualStrings("abc", store.get(abc));
    try testing.expectEqualStrings("\x00\x01abc", store.get(binary));

    const expected = [_]u8{
        0x00, 0x00, 0x00, 0x00,
        0x03, 0x00, 0x00, 0x00,
        'a',  'b',  'c',  0x05,
        0x00, 0x00, 0x00, 0x00,
        0x01, 'a',  'b',  'c',
    };
    try testing.expectEqualSlices(u8, &expected, store.buffer.items.items);
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

    const serialized = try original.serialize(arena_allocator, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

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

    const serialized = try original.serialize(arena_allocator, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

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
        "very long string " ** 50, // long string
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

    const serialized = try original.serialize(arena_allocator, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

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
    try std.testing.expect(@intFromEnum(idx1) < @intFromEnum(idx2));

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

    const serialized = try original.serialize(arena_allocator, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

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
    try std.testing.expectEqual(expectedNextStringContentStart(&previous_end, "first".len), @intFromEnum(idx1));

    const idx2 = try builder.insert(&original, gpa, "second");
    try std.testing.expectEqual(expectedNextStringContentStart(&previous_end, "second".len), @intFromEnum(idx2));

    const idx3 = try builder.insert(&original, gpa, "");
    try std.testing.expectEqual(expectedNextStringContentStart(&previous_end, "".len), @intFromEnum(idx3));

    const long_str = "x" ** 1000;
    const idx4 = try builder.insert(&original, gpa, long_str);
    try std.testing.expectEqual(expectedNextStringContentStart(&previous_end, long_str.len), @intFromEnum(idx4));

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

    const serialized = try original.serialize(arena_allocator, &writer);
    try std.testing.expect(@intFromPtr(serialized) != 0);

    // Write to file
    try writer.writeGather(file, io);

    // Read back
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", writer.total_bytes);
    defer gpa.free(buffer);

    _ = try file.readPositionalAll(io, buffer, 0);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr)));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all strings with their exact indices
    try std.testing.expectEqualStrings("first", deserialized.get(idx1));
    try std.testing.expectEqualStrings("second", deserialized.get(idx2));
    try std.testing.expectEqualStrings("", deserialized.get(idx3));
    try std.testing.expectEqualStrings(long_str, deserialized.get(idx4));
}
