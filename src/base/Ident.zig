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

const SmallStringInterner = mod.SmallStringInterner;

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
    interner: SmallStringInterner,
    attributes: std.ArrayListUnmanaged(Attributes) = .{},
    next_unique_name: u32 = 0,

    /// Initialize the memory for an `Ident.Store` with a specific capaicty.
    pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!Store {
        return .{
            .interner = try SmallStringInterner.initCapacity(gpa, capacity),
        };
    }

    /// Deinitialize the memory for an `Ident.Store`.
    pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
        self.interner.deinit(gpa);
    }

    /// Insert a new identifier into the store.
    pub fn insert(self: *Store, gpa: std.mem.Allocator, ident: Ident) std.mem.Allocator.Error!Idx {
        const idx = try self.interner.insert(gpa, ident.raw_text);

        return Idx{
            .attributes = ident.attributes,
            .idx = @as(u29, @intCast(@intFromEnum(idx))),
        };
    }

    /// Generate a new identifier that is unique within this module.
    ///
    /// We keep a counter per `Ident.Store` that gets incremented each
    /// time this method is called. The new ident is named based on said
    /// counter, which cannot overlap with user-defined idents since those
    /// cannot start with a digit.
    pub fn genUnique(self: *Store, gpa: std.mem.Allocator) std.mem.Allocator.Error!Idx {
        var id = self.next_unique_name;
        self.next_unique_name += 1;

        // Manually render the text into a buffer to avoid allocating
        // a string, as the string interner will copy the text anyway.

        var digit_index: u8 = 9;
        // The max u32 value is 4294967295 which is 10 digits
        var str_buffer = [_]u8{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
        while (id > 0) {
            const digit = id % 10;
            str_buffer[digit_index] = @as(u8, @intCast(digit)) + '0';

            id = (id - digit) / 10;
            digit_index -= 1;
        }

        const name = str_buffer[digit_index + 1 ..];

        const idx = try self.interner.insert(gpa, name, Region.zero());

        const attributes = Attributes{
            .effectful = false,
            .ignored = false,
            .reassignable = false,
        };

        try self.attributes.append(gpa, attributes);

        return Idx{
            .attributes = attributes,
            .idx = @truncate(@intFromEnum(idx)),
        };
    }

    /// Get the text for an identifier.
    pub fn getText(self: *const Store, idx: Idx) []u8 {
        return self.interner.getText(@enumFromInt(@as(u32, idx.idx)));
    }

    /// Check if an identifier text already exists in the store.
    pub fn contains(self: *const Store, text: []const u8) bool {
        return self.interner.contains(text);
    }

    /// Find an identifier by its string, returning its index if it exists.
    /// This is different from insert in that it's guaranteed not to modify the store.
    pub fn findByString(self: *const Store, text: []const u8) ?Idx {
        // Look up in the interner without inserting
        const interner_idx = self.interner.strings.get(text) orelse return null;

        // Create an Idx with inferred attributes from the text
        return Idx{
            .attributes = Attributes.fromString(text),
            .idx = @as(u29, @intCast(@intFromEnum(interner_idx))),
        };
    }

    /// Calculate the size needed to serialize this Ident.Store
    pub fn serializedSize(self: *const Store) usize {
        var size: usize = 0;

        // SmallStringInterner components
        size += @sizeOf(u32); // bytes_len
        size += self.interner.bytes.items.len; // bytes data
        size = std.mem.alignForward(usize, size, @alignOf(u32)); // align for next u32

        size += @sizeOf(u32); // next_unique_name

        // Align to SERIALIZATION_ALIGNMENT to maintain alignment for subsequent data
        return std.mem.alignForward(usize, size, serialization.SERIALIZATION_ALIGNMENT);
    }

    /// Serialize this Ident.Store into the provided buffer
    pub fn serializeInto(self: *const Store, buffer: []u8, gpa: std.mem.Allocator) ![]u8 {
        const size = self.serializedSize();
        if (buffer.len < size) return error.BufferTooSmall;

        var offset: usize = 0;

        // Serialize interner bytes
        const bytes_len = @as(u32, @intCast(self.interner.bytes.items.len));
        @as(*u32, @ptrCast(@alignCast(buffer.ptr + offset))).* = bytes_len;
        offset += @sizeOf(u32);
        if (bytes_len > 0) {
            @memcpy(buffer[offset .. offset + bytes_len], self.interner.bytes.items);
            offset += bytes_len;
        }
        offset = std.mem.alignForward(usize, offset, @alignOf(u32));

        // Serialize next_unique_name
        @as(*u32, @ptrCast(@alignCast(buffer.ptr + offset))).* = self.next_unique_name;
        offset += @sizeOf(u32);

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
        var bytes = std.ArrayListUnmanaged(u8){};
        if (bytes_len > 0) {
            if (offset + bytes_len > buffer.len) return error.BufferTooSmall;
            try bytes.appendSlice(gpa, buffer[offset .. offset + bytes_len]);
            offset += bytes_len;
        }
        offset = std.mem.alignForward(usize, offset, @alignOf(u32));

        // Deserialize next_unique_name
        if (offset + @sizeOf(u32) > buffer.len) return error.BufferTooSmall;
        const next_unique_name = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;

        // Create empty strings hash table (used only for deduplication during insertion)
        const strings = std.StringHashMapUnmanaged(SmallStringInterner.Idx){};

        // Construct the interner
        const interner = SmallStringInterner{
            .bytes = bytes,
            .strings = strings,
            .frozen = if (std.debug.runtime_safety) false else {},
        };

        return Store{
            .interner = interner,
            .next_unique_name = next_unique_name,
        };
    }

    /// Freeze the identifier store, preventing any new entries from being added.
    pub fn freeze(self: *Store) void {
        self.interner.freeze();
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

test "Ident.Store serialization round-trip" {
    const gpa = std.testing.allocator;

    // Create original store and add some identifiers
    var original_store = try Store.initCapacity(gpa, 16);
    defer original_store.deinit(gpa);

    const ident1 = Ident.for_text("hello");
    const ident2 = Ident.for_text("world!");
    const ident3 = Ident.for_text("_ignored");

    const idx1 = try original_store.insert(gpa, ident1, Region.zero());
    const idx2 = try original_store.insert(gpa, ident2, Region.zero());
    const idx3 = try original_store.insert(gpa, ident3, Region.zero());

    // Serialize
    const serialized_size = original_store.serializedSize();
    const buffer = try gpa.alignedAlloc(u8, @alignOf(u32), serialized_size);
    defer gpa.free(buffer);

    const serialized = try original_store.serializeInto(buffer, gpa);
    try std.testing.expectEqual(serialized_size, serialized.len);

    // Deserialize
    var restored_store = try Store.deserializeFrom(serialized, gpa);
    defer restored_store.deinit(gpa);

    // Verify the identifiers are identical
    try std.testing.expectEqualStrings("hello", restored_store.getText(idx1));
    try std.testing.expectEqualStrings("world!", restored_store.getText(idx2));
    try std.testing.expectEqualStrings("_ignored", restored_store.getText(idx3));

    // Verify attributes are preserved
    try std.testing.expect(restored_store.getText(idx1)[0] != '_'); // not ignored
    try std.testing.expect(restored_store.getText(idx2)[restored_store.getText(idx2).len - 1] == '!'); // effectful
    try std.testing.expect(restored_store.getText(idx3)[0] == '_'); // ignored

    // Verify next_unique_name is preserved
    try std.testing.expectEqual(original_store.next_unique_name, restored_store.next_unique_name);

    // Verify structural integrity
    try std.testing.expectEqual(original_store.attributes.items.len, restored_store.attributes.items.len);
    try std.testing.expectEqual(original_store.interner.bytes.items.len, restored_store.interner.bytes.items.len);
    try std.testing.expectEqual(original_store.interner.outer_indices.items.len, restored_store.interner.outer_indices.items.len);
}

test "Ident.Store serialization comprehensive" {
    const gpa = std.testing.allocator;

    var store = try Store.initCapacity(gpa, 8);
    defer store.deinit(gpa);

    // Test various identifier types and edge cases
    const ident1 = Ident.for_text("hello");
    const ident2 = Ident.for_text("world!");
    const ident3 = Ident.for_text("_ignored");
    const ident4 = Ident.for_text("a"); // single character
    const ident5 = Ident.for_text("very_long_identifier_name_that_might_cause_issues"); // long name
    const region = Region.zero();

    _ = try store.insert(gpa, ident1, region);
    _ = try store.insert(gpa, ident2, region);
    _ = try store.insert(gpa, ident3, region);
    _ = try store.insert(gpa, ident4, region);
    _ = try store.insert(gpa, ident5, region);

    // Add some unique names
    _ = try store.genUnique(gpa);
    _ = try store.genUnique(gpa);

    // Test serialization
    try serialization.testing.testSerialization(Store, &store, gpa);
}

test "Ident.Store empty store serialization" {
    const gpa = std.testing.allocator;

    var empty_store = try Store.initCapacity(gpa, 0);
    defer empty_store.deinit(gpa);

    try serialization.testing.testSerialization(Store, &empty_store, gpa);
}
