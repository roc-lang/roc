//! Any text in a Roc source file that has significant content.
//!
//! During tokenization, all variable names, record field names, type names, etc. are interned
//! into a deduplicated collection, the [Ident.Store]. On interning, each Ident gets a unique ID
//! that represents that string, which can be used to look up the string value in the [Ident.Store]
//! in constant time. Storing IDs in each IR instead of strings also uses less memory in the IRs.

const std = @import("std");
const mod = @import("mod.zig");
const Region = @import("Region.zig");
const serialization = @import("serialization");
const collections = @import("collections");

const IdentInterner = mod.IdentInterner;

const Ident = @This();

/// The original text of the identifier.
raw_text: []const u8,

/// Attributes of the identifier such as if it is effectful, ignored, or reassignable.
attributes: Attributes,

/// Create a new identifier from a string.
pub fn for_text(text: []const u8) Ident {
    return Ident{
        .raw_text = text,
        .attributes = Attributes.fromString(text),
    };
}

/// Errors that can occur when creating an identifier from text.
pub const Error = error{
    /// The identifier text is empty.
    EmptyText,
    /// The identifier text contains null bytes.
    ContainsNullByte,
    /// The identifier text contains control characters that could cause issues.
    ContainsControlCharacters,
};

/// Create a new identifier from a byte slice with validation.
/// Returns an error if the bytes are malformed or the Ident is invalid.
pub fn from_bytes(bytes: []const u8) Error!Ident {
    // Validate the bytes
    if (bytes.len == 0) {
        return Error.EmptyText;
    }

    // Check for null bytes (causes crashes in string interner)
    if (std.mem.indexOfScalar(u8, bytes, 0) != null) {
        return Error.ContainsNullByte;
    }

    // Check for other problematic control characters including space, tab, newline, and carriage return
    for (bytes) |byte| {
        if (byte < 32 or byte == ' ' or byte == '\t' or byte == '\n' or byte == '\r') {
            return Error.ContainsControlCharacters;
        }
    }

    return Ident{
        .raw_text = bytes,
        .attributes = Attributes.fromString(bytes),
    };
}

/// The index from the store, with the attributes packed into unused bytes.
///
/// With 29-bits for the ID we can store up to 536,870,912 identifiers.
pub const Idx = packed struct(u32) {
    attributes: Attributes,
    idx: u29,
};

/// Identifier attributes such as if it is effectful, ignored, or reassignable.
pub const Attributes = packed struct(u3) {
    effectful: bool,
    ignored: bool,
    reassignable: bool,

    pub fn fromString(text: []const u8) Attributes {
        return .{
            .effectful = std.mem.endsWith(u8, text, "!"),
            .ignored = std.mem.startsWith(u8, text, "_"),
            .reassignable = false,
        };
    }
};

/// An interner for identifier names.
pub const Store = struct {
    interner: IdentInterner,
    attributes: collections.SafeList(Attributes) = .{},

    /// Initialize the memory for an `Ident.Store` with a specific capaicty.
    pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!Store {
        return .{
            .interner = try IdentInterner.initCapacity(gpa, capacity),
        };
    }

    /// Deinitialize the memory for an `Ident.Store`.
    pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
        self.interner.deinit(gpa);
        self.attributes.deinit(gpa);
    }

    /// Insert a new identifier into the store.
    pub fn insert(self: *Store, gpa: std.mem.Allocator, ident: Ident) std.mem.Allocator.Error!Idx {
        const idx = try self.interner.insert(gpa, ident.raw_text);

        return Idx{
            .attributes = ident.attributes,
            .idx = @as(u29, @intCast(@intFromEnum(idx))),
        };
    }

    /// Get the text for an identifier. (They are always stored as lowercase,
    /// so this is more efficient than getUppercase, which must do a conversion.)
    pub fn getLowercase(self: *const Store, idx: Idx) []u8 {
        return self.interner.getLowercase(@enumFromInt(@as(u32, idx.idx)));
    }

    /// Get the text with first char converted to uppercase if lowercase.
    /// Caller must free the returned slice if it was allocated.
    pub fn getUppercase(self: *const Store, allocator: std.mem.Allocator, idx: Idx) ![]u8 {
        return self.interner.getUppercase(allocator, @enumFromInt(@as(u32, idx.idx)));
    }

    /// Check if an identifier text already exists in the store.
    pub fn contains(self: *const Store, text: []const u8) bool {
        return self.interner.contains(text);
    }

    /// Find an identifier by its string, returning its index if it exists.
    /// This is different from insert in that it's guaranteed not to modify the store.
    pub fn findByString(self: *const Store, text: []const u8) ?Idx {
        // Look up in the interner without inserting
        const result = self.interner.findStringOrSlot(text);
        const interner_idx = result.idx orelse return null;

        // Create an Idx with inferred attributes from the text
        return Idx{
            .attributes = Attributes.fromString(text),
            .idx = @as(u29, @intCast(@intFromEnum(interner_idx))),
        };
    }

    /// Freeze the identifier store, preventing any new entries from being added.
    pub fn freeze(self: *Store) void {
        self.interner.freeze();
    }
    /// Calculate the size needed to serialize this Ident.Store
    pub fn serializedSize(self: *const Store) usize {
        var size: usize = 0;

        // IdentInterner components
        size += @sizeOf(u32); // bytes_len
        size += self.interner.bytes.len(); // bytes data
        size = std.mem.alignForward(usize, size, @alignOf(u32)); // align for next u32

        // Align to SERIALIZATION_ALIGNMENT to maintain alignment for subsequent data
        return std.mem.alignForward(usize, size, serialization.SERIALIZATION_ALIGNMENT);
    }

    /// Serialize this Ident.Store into the provided buffer
    pub fn serializeInto(self: *const Store, buffer: []u8, gpa: std.mem.Allocator) ![]u8 {
        const size = self.serializedSize();
        if (buffer.len < size) return error.BufferTooSmall;

        var offset: usize = 0;

        // Serialize interner bytes
        const bytes_len = @as(u32, @intCast(self.interner.bytes.len()));
        @as(*u32, @ptrCast(@alignCast(buffer.ptr + offset))).* = bytes_len;
        offset += @sizeOf(u32);
        if (bytes_len > 0) {
            @memcpy(buffer[offset .. offset + bytes_len], self.interner.bytes.items.items);
            offset += bytes_len;
        }
        offset = std.mem.alignForward(usize, offset, @alignOf(u32));

        _ = gpa; // suppress unused parameter warning

        // Zero out any padding bytes
        if (offset < size) {
            @memset(buffer[offset..size], 0);
        }

        return buffer[0..size];
    }

    /// Deserialize an Ident.Store from the provided buffer
    pub fn deserializeFrom(buffer: []const u8, gpa: std.mem.Allocator) !Store {
        if (buffer.len < @sizeOf(u32)) return error.BufferTooSmall;

        var offset: usize = 0;

        // Deserialize interner bytes
        const bytes_len = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;
        offset += @sizeOf(u32);
        var bytes = collections.SafeList(u8){};
        if (bytes_len > 0) {
            if (offset + bytes_len > buffer.len) return error.BufferTooSmall;
            bytes = try collections.SafeList(u8).initCapacity(gpa, bytes_len);
            _ = try bytes.appendSlice(gpa, buffer[offset .. offset + bytes_len]);
            offset += bytes_len;
        }
        offset = std.mem.alignForward(usize, offset, @alignOf(u32));

        // Create empty hash table
        const hash_table = collections.SafeList(IdentInterner.Idx){};

        // Construct the interner
        const interner = IdentInterner{
            .bytes = bytes,
            .hash_table = hash_table,
            .entry_count = 0,
            .frozen = if (std.debug.runtime_safety) false else {},
        };

        return Store{
            .interner = interner,
        };
    }

    /// Serialize this Store to the given CompactWriter. The resulting Store
    /// in the writer's buffer will have offsets instead of pointers. Calling any
    /// methods on it or dereferencing its internal "pointers" (which are now
    /// offsets) is illegal behavior!
    pub fn serialize(
        self: *const Store,
        allocator: std.mem.Allocator,
        writer: *collections.CompactWriter,
    ) std.mem.Allocator.Error!*const Store {
        // First, write the Store struct itself
        const offset_self = try writer.appendAlloc(allocator, Store);

        // Then serialize the sub-structures and update the struct
        offset_self.* = .{
            .interner = (try self.interner.serialize(allocator, writer)).*,
            .attributes = (try self.attributes.serialize(allocator, writer)).*,
        };

        return @constCast(offset_self);
    }

    /// Add the given offset to the memory addresses of all pointers in `self`.
    pub fn relocate(self: *Store, offset: isize) void {
        self.interner.relocate(offset);
        self.attributes.relocate(offset);
    }
};

test "from_bytes validates empty text" {
    const result = Ident.from_bytes("");
    try std.testing.expectError(Error.EmptyText, result);
}

test "from_bytes validates null bytes" {
    const text_with_null = "hello\x00world";
    const result = Ident.from_bytes(text_with_null);
    try std.testing.expectError(Error.ContainsNullByte, result);
}

test "from_bytes validates control characters" {
    const text_with_control = "hello\x01world";
    const result = Ident.from_bytes(text_with_control);
    try std.testing.expectError(Error.ContainsControlCharacters, result);
}

test "from_bytes disallows common whitespace" {
    const text_with_space = "hello world";
    const result = Ident.from_bytes(text_with_space);
    try std.testing.expect(result == Error.ContainsControlCharacters);

    const text_with_tab = "hello\tworld";
    const result2 = Ident.from_bytes(text_with_tab);
    try std.testing.expect(result2 == Error.ContainsControlCharacters);

    const text_with_newline = "hello\nworld";
    const result3 = Ident.from_bytes(text_with_newline);
    try std.testing.expect(result3 == Error.ContainsControlCharacters);

    const text_with_cr = "hello\rworld";
    const result4 = Ident.from_bytes(text_with_cr);
    try std.testing.expect(result4 == Error.ContainsControlCharacters);
}

test "from_bytes creates valid identifier" {
    const result = try Ident.from_bytes("valid_name!");
    try std.testing.expectEqualStrings("valid_name!", result.raw_text);
    try std.testing.expect(result.attributes.effectful == true);
    try std.testing.expect(result.attributes.ignored == false);
    try std.testing.expect(result.attributes.reassignable == false);
}

test "from_bytes creates ignored identifier" {
    const result = try Ident.from_bytes("_ignored");
    try std.testing.expectEqualStrings("_ignored", result.raw_text);
    try std.testing.expect(result.attributes.effectful == false);
    try std.testing.expect(result.attributes.ignored == true);
    try std.testing.expect(result.attributes.reassignable == false);
}

test "Ident.Store empty CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create an empty Store
    var original = try Store.initCapacity(gpa, 0);
    defer original.deinit(gpa);

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty
    try testing.expectEqual(@as(usize, 1), deserialized.interner.bytes.len());
    try testing.expectEqual(@as(usize, 0), deserialized.attributes.len());
}

test "Ident.Store basic CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create original store and add some identifiers
    var original = try Store.initCapacity(gpa, 16);
    defer original.deinit(gpa);

    const ident1 = Ident.for_text("hello");
    const ident2 = Ident.for_text("world!");
    const ident3 = Ident.for_text("_ignored");

    const idx1 = try original.insert(gpa, ident1);
    const idx2 = try original.insert(gpa, ident2);
    const idx3 = try original.insert(gpa, ident3);

    // Verify the attributes in the indices
    try testing.expect(!idx1.attributes.effectful);
    try testing.expect(!idx1.attributes.ignored);
    try testing.expect(idx2.attributes.effectful);
    try testing.expect(!idx2.attributes.ignored);
    try testing.expect(!idx3.attributes.effectful);
    try testing.expect(idx3.attributes.ignored);

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the identifiers are accessible
    try testing.expectEqualStrings("hello", deserialized.getLowercase(idx1));
    try testing.expectEqualStrings("world!", deserialized.getLowercase(idx2));
    try testing.expectEqualStrings("_ignored", deserialized.getLowercase(idx3));

    // Verify the interner's hash map is empty after deserialization
    try testing.expectEqual(@as(usize, 0), deserialized.interner.strings.count());
}

test "Ident.Store frozen state CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create and populate store
    var original = try Store.initCapacity(gpa, 5);
    defer original.deinit(gpa);

    _ = try original.insert(gpa, Ident.for_text("test1"));
    _ = try original.insert(gpa, Ident.for_text("test2"));

    // Freeze the store
    original.freeze();

    // Verify interner is frozen
    if (std.debug.runtime_safety) {
        try testing.expect(original.interner.frozen);
    }

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_frozen_store.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify frozen state is preserved
    if (std.debug.runtime_safety) {
        try testing.expect(deserialized.interner.frozen);
    }
}

test "Ident.Store comprehensive CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create store with various identifiers
    var original = try Store.initCapacity(gpa, 20);
    defer original.deinit(gpa);

    // Test various identifier types and edge cases
    const test_idents = [_]struct { text: []const u8, expected_idx: u32 }{
        .{ .text = "hello", .expected_idx = 0 },
        .{ .text = "world!", .expected_idx = 6 },
        .{ .text = "_ignored", .expected_idx = 13 },
        .{ .text = "a", .expected_idx = 22 }, // single character
        .{ .text = "very_long_identifier_name_that_might_cause_issues", .expected_idx = 24 },
        .{ .text = "effectful!", .expected_idx = 75 },
        .{ .text = "_", .expected_idx = 86 }, // Just underscore
        .{ .text = "CamelCase", .expected_idx = 88 },
        .{ .text = "snake_case", .expected_idx = 98 },
        .{ .text = "SCREAMING_CASE", .expected_idx = 109 },
        .{ .text = "hello", .expected_idx = 0 }, // duplicate, should reuse
    };

    var indices = std.ArrayList(Idx).init(gpa);
    defer indices.deinit();

    for (test_idents) |test_ident| {
        const ident = Ident.for_text(test_ident.text);
        const idx = try original.insert(gpa, ident);
        try indices.append(idx);
        try testing.expectEqual(test_ident.expected_idx, idx.idx);
    }

    // Add some hardcoded names
    const unique1 = try original.insert(gpa, Ident.for_text("unique1"));
    const unique2 = try original.insert(gpa, Ident.for_text("unique2"));

    // Verify the interner's hash map is populated
    try testing.expect(original.interner.strings.count() > 0);

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_comprehensive_store.dat", .{ .read = true });
    defer file.close();

    // Serialize
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try original.serialize(gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate
    const deserialized = @as(*Store, @ptrCast(@alignCast(buffer.ptr + writer.total_bytes - @sizeOf(Store))));
    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all identifiers (skip duplicate at end)
    for (test_idents[0..10], 0..) |test_ident, i| {
        const idx = indices.items[i];
        const text = deserialized.getLowercase(idx);
        try testing.expectEqualStrings(test_ident.text, text);
    }

    // Verify unique names
    try testing.expectEqualStrings("unique1", deserialized.getLowercase(unique1));
    try testing.expectEqualStrings("unique2", deserialized.getLowercase(unique2));

    // Verify the interner's hash map is empty after deserialization
    try testing.expectEqual(@as(usize, 0), deserialized.interner.strings.count());
}

test "Ident.Store multiple stores CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create multiple stores to test alignment
    var store1 = try Store.initCapacity(gpa, 5);
    defer store1.deinit(gpa);

    var store2 = try Store.initCapacity(gpa, 5);
    defer store2.deinit(gpa);

    var store3 = try Store.initCapacity(gpa, 5);
    defer store3.deinit(gpa);

    // Populate stores differently
    const idx1_1 = try store1.insert(gpa, Ident.for_text("store1_ident"));
    _ = try store1.insert(gpa, Ident.for_text("store1_unique"));

    const idx2_1 = try store2.insert(gpa, Ident.for_text("store2_ident!"));
    const idx2_2 = try store2.insert(gpa, Ident.for_text("_store2_ignored"));
    store2.freeze();

    const idx3_1 = try store3.insert(gpa, Ident.for_text("store3"));

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_multiple_stores.dat", .{ .read = true });
    defer file.close();

    // Serialize all three
    var writer = collections.CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(gpa);

    _ = try store1.serialize(gpa, &writer);
    const offset1 = writer.total_bytes - @sizeOf(Store);

    _ = try store2.serialize(gpa, &writer);
    const offset2 = writer.total_bytes - @sizeOf(Store);

    _ = try store3.serialize(gpa, &writer);
    const offset3 = writer.total_bytes - @sizeOf(Store);

    // Write to file
    try writer.writeGather(gpa, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, 16, file_size);
    defer gpa.free(buffer);

    _ = try file.read(buffer);

    // Cast and relocate all three
    const deserialized1 = @as(*Store, @ptrCast(@alignCast(buffer.ptr + offset1)));
    deserialized1.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized2 = @as(*Store, @ptrCast(@alignCast(buffer.ptr + offset2)));
    deserialized2.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    const deserialized3 = @as(*Store, @ptrCast(@alignCast(buffer.ptr + offset3)));
    deserialized3.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify store 1
    try testing.expectEqualStrings("store1_ident", deserialized1.getLowercase(idx1_1));

    // Verify store 2 (frozen)
    try testing.expectEqualStrings("store2_ident!", deserialized2.getLowercase(idx2_1));
    try testing.expectEqualStrings("_store2_ignored", deserialized2.getLowercase(idx2_2));
    if (std.debug.runtime_safety) {
        try testing.expect(deserialized2.interner.frozen);
    }

    // Verify store 3
    try testing.expectEqualStrings("store3", deserialized3.getLowercase(idx3_1));

    // Verify all have empty hash maps
    try testing.expectEqual(@as(usize, 0), deserialized1.interner.strings.count());
    try testing.expectEqual(@as(usize, 0), deserialized2.interner.strings.count());
    try testing.expectEqual(@as(usize, 0), deserialized3.interner.strings.count());
}
