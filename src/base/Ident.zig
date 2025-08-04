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
const CompactWriter = serialization.CompactWriter;
const collections = @import("collections");

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

/// A unique identifier index that can represent either small inlined identifiers
/// or references to identifiers stored in the string interner.
///
/// Small identifiers (up to 4 ASCII characters starting with a letter) are stored
/// inline using packed bit fields. Larger identifiers are stored in the interner
/// and referenced by index.
pub const Idx = enum(u32) {
    _,

    pub fn attributes(self: *const @This()) Attributes {
        return self.toInner().attributes();
    }

    pub fn format(
        self: Idx,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        return self.toInner().format(fmt, options, writer);
    }

    pub fn try_inline(string: []const u8) ?@This() {
        const inner = InnerIdx.try_inline(string) orelse return null;
        return Idx.fromInner(inner);
    }

    pub fn toU32(self: Idx) u32 {
        const result = @intFromEnum(self);
        return result;
    }

    pub fn fromU32(value: u32) Idx {
        const result = @as(Idx, @enumFromInt(value));
        return result;
    }

    pub fn invalid() Idx {
        // Create an invalid identifier with index 0
        return @enumFromInt(0);
    }

    pub fn toInner(self: Idx) InnerIdx {
        const enum_val = @intFromEnum(self);
        const inner = @as(InnerIdx, @bitCast(enum_val));
        return inner;
    }

    fn fromInner(inner: InnerIdx) Idx {
        const bits = @as(u32, @bitCast(inner));
        return @as(Idx, @enumFromInt(bits));
    }

    /// Get the underlying interner index for big identifiers (for testing purposes)
    /// Returns null for small/inline identifiers
    pub fn getInternerIdx(self: Idx) ?u32 {
        const inner = self.toInner();
        if (inner.is_small) {
            return null;
        } else {
            return @as(u32, inner.data.big.idx);
        }
    }
};

const InnerIdx = packed struct(u32) {
    is_small: bool,
    data: packed union { small: SmallIdx, big: BigIdx },

    pub fn attributes(self: *const @This()) Attributes {
        switch (self.toVariant()) {
            .small => |small| return small.attributes(),
            .big => |big| return big.attributes,
        }
    }

    pub fn getIdx(self: *const @This()) u28 {
        if (self.is_small) {
            // Small identifiers don't have a real index in the store
            return 0;
        } else {
            return self.data.big.idx;
        }
    }

    // Support for std.testing.expectEqual by implementing format
    pub fn format(
        self: InnerIdx,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self.toVariant()) {
            .small => |small| {
                // Small identifiers don't have a real index in the store
                try writer.print("Ident.Idx({s})", .{small.asString()});
            },
            .big => |big| {
                // Format as the underlying u32 value for debugging
                try writer.print("Ident.Idx({d})", .{big.idx});
            },
        }
    }

    const Variant = union(enum) {
        small: SmallIdx,
        big: BigIdx,
    };

    fn toVariant(self: InnerIdx) Variant {
        if (self.is_small) {
            return .{ .small = self.data.small };
        } else {
            return .{ .big = self.data.big };
        }
    }

    /// Given a nonempty ident string that does not start with a digit, try to construct an inline Idx.
    fn try_inline(string: []const u8) ?@This() {
        std.debug.assert(string.len > 0);
        // Ident strings must never start with digits.
        std.debug.assert(string[0] < '0' or string[0] > '9');

        const unused = string[0] == '_'; // If it starts with _, this identifier is unused - e.g. `_foo`.
        const reused = string[string.len - 1] == '_'; // If it ends with _, this is reused via `var` - e.g. `foo_`.

        // Solo underscore (or single-character with special attributes) should be treated as big identifier
        if (string.len == 1 and (unused or reused)) {
            return null;
        }

        // If it ends with `!` (with or without an underscore after it), it's effectful.
        const fx_check_index = string.len - @as(usize, @intFromBool(reused));
        const fx = fx_check_index > 0 and string[fx_check_index - 1] == '!';

        // Skip the underscore prefix if there was one.
        const first_non_attr_index: usize = @intFromBool(unused);
        const char0 = string[first_non_attr_index];

        // Get the total number of attributes (unused, reused, effectful)
        const unused_count = @as(usize, @intFromBool(unused));
        const reused_count = @as(usize, @intFromBool(reused));
        const fx_count = @as(usize, @intFromBool(fx));
        const attr_count: usize = unused_count + reused_count + fx_count;

        // Length without underscore/bang prefix/suffix.
        const len = string.len - attr_count;

        // Treat double underscore prefix as a big idx. This will be a warning anyway,
        // so it should come up almost never in practice, and this avoids edge cases.
        if (len > 4 or char0 == '_' or char0 < 'A' or char0 > 'z') {
            return null;
        }

        // All the other ASCII chars between `Z` and `a` are invalid identifiers (`[`, `\`, `]`, `^`, '`'),
        // so tokenization should not have let them get through here.
        std.debug.assert((char0 >= 'A' and char0 <= 'Z') or (char0 >= 'a' and char0 <= 'z'));

        // len would only be 0 here for inputs of `__` or `_!_`, but we should have already early-returned for those.
        std.debug.assert(len > 0);

        // Get the characters after the first one
        var char1: u8 = 0;
        var char2: u8 = 0;
        var char3: u8 = 0;

        if (len >= 2) {
            char1 = string[first_non_attr_index + 1];
        }
        if (len >= 3) {
            char2 = string[first_non_attr_index + 2];
        }
        if (len >= 4) {
            char3 = string[first_non_attr_index + 3];
        }

        // If any char is outside the ASCII range (e.g. it's Unicode), this is a big index.
        // (We don't need to test for lower bounds because tokenization would have already verified that.)
        if (char1 > 'z' or char2 > 'z' or char3 > 'z') {
            return null;
        }

        const small_idx = SmallIdx.create(@as(u7, @intCast(char0)), @as(u7, @intCast(char1)), @as(u7, @intCast(char2)), @as(u7, @intCast(char3)), unused, reused, fx);

        return .{
            .is_small = true,
            .data = .{
                .small = small_idx,
            },
        };
    }
};

/// The index from the store, with the attributes packed into unused bytes.
///
/// With 29-bits for the ID we can store up to 536,870,912 identifiers.
const BigIdx = packed struct(u31) {
    attributes: Attributes,
    idx: u28,
};

const SmallIdx = packed struct(u31) {
    // Store as a simple integer to avoid bit packing issues
    bits: u31,

    // Bit layout: [char0:7][char1:7][char2:7][char3:7][unused:1][reused:1][effectful:1]

    pub fn char0(self: *const @This()) u7 {
        const result = @as(u7, @intCast(self.bits & 0x7F));
        return result;
    }

    pub fn char1(self: *const @This()) u7 {
        const result = @as(u7, @intCast((self.bits >> 7) & 0x7F));
        return result;
    }

    pub fn char2(self: *const @This()) u7 {
        const result = @as(u7, @intCast((self.bits >> 14) & 0x7F));
        return result;
    }

    pub fn char3(self: *const @This()) u7 {
        const result = @as(u7, @intCast((self.bits >> 21) & 0x7F));
        return result;
    }

    pub fn is_unused(self: *const @This()) bool {
        return (self.bits >> 28) & 1 != 0;
    }

    pub fn is_reused(self: *const @This()) bool {
        return (self.bits >> 29) & 1 != 0;
    }

    pub fn is_effectful(self: *const @This()) bool {
        return (self.bits >> 30) & 1 != 0;
    }

    pub fn create(c0: u7, c1: u7, c2: u7, c3: u7, unused: bool, reused: bool, effectful: bool) @This() {
        var bits: u31 = 0;
        bits |= @as(u31, c0);
        bits |= @as(u31, c1) << 7;
        bits |= @as(u31, c2) << 14;
        bits |= @as(u31, c3) << 21;
        bits |= @as(u31, @intFromBool(unused)) << 28;
        bits |= @as(u31, @intFromBool(reused)) << 29;
        bits |= @as(u31, @intFromBool(effectful)) << 30;

        return .{ .bits = bits };
    }

    fn attributes(self: *const @This()) Attributes {
        return .{
            .effectful = self.is_effectful(),
            .ignored = self.is_unused(),
            .reassignable = self.is_reused(),
        };
    }

    /// Reconstruct the text from the packed character fields by value.
    /// Uses a thread-local rotating buffer to handle multiple concurrent accesses.
    fn getTextFromValue(small_idx: @This()) []u8 {
        // Thread-local rotating buffers to handle multiple concurrent identifiers
        // Max length: 1 (underscore prefix) + 4 (chars) + 1 (exclamation) + 1 (underscore suffix) = 7
        const S = struct {
            var buffers: [16][7]u8 = [_][7]u8{[_]u8{0} ** 7} ** 16;
            var current_index: usize = 0;
        };

        const buffer = &S.buffers[S.current_index];
        S.current_index = (S.current_index + 1) % S.buffers.len;

        var len: usize = 0;

        // Add underscore prefix if unused
        if (small_idx.is_unused()) {
            buffer[len] = '_';
            len += 1;
        }

        // Add main characters (char0 is always present since len > 0)
        buffer[len] = @as(u8, @intCast(small_idx.char0()));
        len += 1;

        if (small_idx.char1() != 0) {
            buffer[len] = @as(u8, @intCast(small_idx.char1()));
            len += 1;
        }
        if (small_idx.char2() != 0) {
            buffer[len] = @as(u8, @intCast(small_idx.char2()));
            len += 1;
        }
        if (small_idx.char3() != 0) {
            buffer[len] = @as(u8, @intCast(small_idx.char3()));
            len += 1;
        }

        // Add exclamation if effectful
        if (small_idx.is_effectful()) {
            buffer[len] = '!';
            len += 1;
        }

        // Add underscore suffix if reused
        if (small_idx.is_reused()) {
            buffer[len] = '_';
            len += 1;
        }

        return buffer[0..len];
    }

    /// Reconstruct the text from the packed character fields.
    /// Uses a thread-local rotating buffer to handle multiple concurrent accesses.
    pub fn getText(self: *const @This()) []u8 {
        // Thread-local rotating buffers to handle multiple concurrent identifiers
        // Max length: 1 (underscore prefix) + 4 (chars) + 1 (exclamation) + 1 (underscore suffix) = 7
        const S = struct {
            var buffers: [16][7]u8 = [_][7]u8{[_]u8{0} ** 7} ** 16;
            var current_index: usize = 0;
        };

        const buffer = &S.buffers[S.current_index];
        S.current_index = (S.current_index + 1) % S.buffers.len;

        var len: usize = 0;

        // Add underscore prefix if unused
        if (self.is_unused()) {
            buffer[len] = '_';
            len += 1;
        }

        // Add the core characters
        buffer[len] = @as(u8, @intCast(self.char0()));
        len += 1;

        if (self.char1() != 0) {
            buffer[len] = @as(u8, @intCast(self.char1()));
            len += 1;
        }
        if (self.char2() != 0) {
            buffer[len] = @as(u8, @intCast(self.char2()));
            len += 1;
        }
        if (self.char3() != 0) {
            buffer[len] = @as(u8, @intCast(self.char3()));
            len += 1;
        }

        // Add exclamation suffix if effectful
        if (self.is_effectful()) {
            buffer[len] = '!';
            len += 1;
        }

        // Add underscore suffix if reused
        if (self.is_reused()) {
            buffer[len] = '_';
            len += 1;
        }

        return buffer[0..len];
    }

    /// Write the text from the packed character fields to a provided buffer.
    /// Returns the slice of the buffer that was written to.
    /// Buffer must be at least 7 bytes (max: 1 underscore + 4 chars + 1 exclamation + 1 underscore).
    pub fn writeTextToBuffer(self: *const @This(), buffer: []u8) []u8 {
        std.debug.assert(buffer.len >= 7);

        var len: usize = 0;

        // Add underscore prefix if unused
        if (self.is_unused()) {
            buffer[len] = '_';
            len += 1;
        }

        // Add main characters (char0 is always present since len > 0)
        buffer[len] = @as(u8, @intCast(self.char0()));
        len += 1;

        if (self.char1() != 0) {
            buffer[len] = @as(u8, @intCast(self.char1()));
            len += 1;
        }
        if (self.char2() != 0) {
            buffer[len] = @as(u8, @intCast(self.char2()));
            len += 1;
        }
        if (self.char3() != 0) {
            buffer[len] = @as(u8, @intCast(self.char3()));
            len += 1;
        }

        // Add exclamation if effectful
        if (self.is_effectful()) {
            buffer[len] = '!';
            len += 1;
        }

        // Add underscore suffix if reused
        if (self.is_reused()) {
            buffer[len] = '_';
            len += 1;
        }

        return buffer[0..len];
    }

    /// Reconstruct the text from the packed character fields, allocating a new string.
    /// This is safer than getText when multiple identifiers are being processed concurrently.
    fn getTextFromValueAlloc(small_idx: @This(), allocator: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        // Calculate the length first
        var len: usize = 0;
        if (small_idx.is_unused()) len += 1; // underscore prefix
        len += 1; // char0 is always present
        if (small_idx.char1() != 0) len += 1;
        if (small_idx.char2() != 0) len += 1;
        if (small_idx.char3() != 0) len += 1;
        if (small_idx.is_effectful()) len += 1; // exclamation
        if (small_idx.is_reused()) len += 1; // underscore suffix

        // Allocate the buffer
        const buffer = try allocator.alloc(u8, len);
        var idx: usize = 0;

        // Add underscore prefix if unused
        if (small_idx.is_unused()) {
            buffer[idx] = '_';
            idx += 1;
        }

        // Add the core characters
        buffer[idx] = @as(u8, @intCast(small_idx.char0()));
        idx += 1;

        if (small_idx.char1() != 0) {
            buffer[idx] = @as(u8, @intCast(small_idx.char1()));
            idx += 1;
        }
        if (small_idx.char2() != 0) {
            buffer[idx] = @as(u8, @intCast(small_idx.char2()));
            idx += 1;
        }
        if (small_idx.char3() != 0) {
            buffer[idx] = @as(u8, @intCast(small_idx.char3()));
            idx += 1;
        }

        // Add exclamation suffix if effectful
        if (small_idx.is_effectful()) {
            buffer[idx] = '!';
            idx += 1;
        }

        // Add underscore suffix if reused
        if (small_idx.is_reused()) {
            buffer[idx] = '_';
            idx += 1;
        }

        return buffer;
    }

    fn asString(self: *const @This()) []u8 {
        return self.getText();
    }
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
    attributes: collections.SafeList(Attributes) = .{},
    next_unique_name: u32 = 0,

    /// Serialized representation of an Ident.Store
    pub const Serialized = struct {
        interner: SmallStringInterner.Serialized,
        attributes: collections.SafeList(Attributes).Serialized,
        next_unique_name: u32,

        /// Serialize a Store into this Serialized struct, appending data to the writer
        pub fn serialize(
            self: *Serialized,
            store: *const Store,
            allocator: std.mem.Allocator,
            writer: *CompactWriter,
        ) std.mem.Allocator.Error!void {
            try self.interner.serialize(&store.interner, allocator, writer);
            try self.attributes.serialize(&store.attributes, allocator, writer);
            self.next_unique_name = store.next_unique_name;
        }

        /// Deserialize this Serialized struct into a Store
        pub fn deserialize(self: *Serialized, offset: i64) *Store {
            // Ident.Store.Serialized should be at least as big as Ident.Store
            std.debug.assert(@sizeOf(Serialized) >= @sizeOf(Store));

            // Overwrite ourself with the deserialized version, and return our pointer after casting it to Self.
            const store = @as(*Store, @ptrFromInt(@intFromPtr(self)));

            store.* = Store{
                .interner = self.interner.deserialize(offset).*,
                .attributes = self.attributes.deserialize(offset).*,
                .next_unique_name = self.next_unique_name,
            };

            return store;
        }
    };

    /// Initialize the memory for an `Ident.Store` with a specific capaicty.
    pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!Store {
        return .{
            .interner = try SmallStringInterner.initCapacity(gpa, capacity),
        };
    }

    /// Deinitialize the memory for an `Ident.Store`.
    pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
        self.interner.deinit(gpa);
        self.attributes.deinit(gpa);
    }

    /// Insert a new nonempty identifier into the store.
    pub fn insert(self: *Store, gpa: std.mem.Allocator, ident: Ident) std.mem.Allocator.Error!Idx {
        std.debug.assert(ident.raw_text.len > 0);
        // First try to create a small identifier
        // Only try inline if the string doesn't start with a digit
        if (ident.raw_text[0] < '0' or ident.raw_text[0] > '9') {
            if (Idx.try_inline(ident.raw_text)) |small_idx| {
                return small_idx;
            }
        }

        // Fall back to big identifier in the interner
        const idx = try self.interner.insert(gpa, ident.raw_text);

        const big_idx = BigIdx{
            .attributes = ident.attributes,
            .idx = @as(u28, @intCast(@intFromEnum(idx))),
        };
        return Idx.fromInner(.{
            .is_small = false,
            .data = .{ .big = big_idx },
        });
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
        // Special case for 0
        if (id == 0) {
            str_buffer[digit_index] = '0';
            digit_index -= 1;
        } else {
            while (id > 0) {
                const digit = id % 10;
                str_buffer[digit_index] = @as(u8, @intCast(digit)) + '0';

                id = (id - digit) / 10;
                digit_index -= 1;
            }
        }

        const name = str_buffer[digit_index + 1 ..];

        const idx = try self.interner.insert(gpa, name);

        const attributes = Attributes{
            .effectful = false,
            .ignored = false,
            .reassignable = false,
        };

        _ = try self.attributes.append(gpa, attributes);

        const big_idx = BigIdx{
            .attributes = attributes,
            .idx = @as(u28, @intCast(@intFromEnum(idx))),
        };
        return Idx.fromInner(.{
            .is_small = false,
            .data = .{ .big = big_idx },
        });
    }

    /// Get the text for an identifier.
    pub fn getText(self: *const Store, idx: Idx) []u8 {
        const inner = idx.toInner();
        if (inner.is_small) {
            // For small identifiers, reconstruct the text from packed characters
            return SmallIdx.getTextFromValue(inner.data.small);
        } else {
            // For big identifiers, look up in the interner
            const big = inner.data.big;
            return self.interner.getText(@enumFromInt(@as(u32, big.idx)));
        }
    }

    /// Write the text for an identifier to a provided buffer.
    /// Returns the slice of the buffer that was written to.
    /// Buffer must be at least 256 bytes for big identifiers, or 7 bytes for small identifiers.
    pub fn writeTextToBuffer(self: *const Store, idx: Idx, buffer: []u8) []u8 {
        const inner = idx.toInner();
        if (inner.is_small) {
            // For small identifiers, write directly to buffer
            // Create a copy to avoid alignment issues with packed struct
            const small_copy = inner.data.small;
            return small_copy.writeTextToBuffer(buffer);
        } else {
            // For big identifiers, copy from the interner
            const big = inner.data.big;
            const text = self.interner.getText(@enumFromInt(@as(u32, big.idx)));
            std.debug.assert(buffer.len >= text.len);
            @memcpy(buffer[0..text.len], text);
            return buffer[0..text.len];
        }
    }

    /// Get the text for an identifier, allocating a new string for small identifiers.
    /// This is safer than getText when multiple identifiers are being processed concurrently.
    pub fn getTextAlloc(self: *const Store, idx: Idx, allocator: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
        const inner = idx.toInner();
        if (inner.is_small) {
            // For small identifiers, allocate and reconstruct the text from packed characters
            return try SmallIdx.getTextFromValueAlloc(inner.data.small, allocator);
        } else {
            // For big identifiers, clone the text from the interner
            const big = inner.data.big;
            const text = self.interner.getText(@enumFromInt(@as(u32, big.idx)));
            return try allocator.dupe(u8, text);
        }
    }

    /// Check if an identifier text already exists in the store.
    pub fn contains(self: *const Store, text: []const u8) bool {
        return self.interner.contains(text);
    }

    /// Find an identifier by its nonempty string, returning its index if it exists.
    /// This is different from insert in that it's guaranteed not to modify the store.
    pub fn findByString(self: *const Store, text: []const u8) ?Idx {
        std.debug.assert(text.len > 0);
        // First try to create a small identifier
        // Only try inline if the string doesn't start with a digit
        if (text[0] < '0' or text[0] > '9') {
            if (Idx.try_inline(text)) |small_idx| {
                return small_idx;
            }
        }

        // Look up in the interner without inserting
        const result = self.interner.findStringOrSlot(text);
        const interner_idx = result.idx orelse return null;

        // Create an Idx with inferred attributes from the text
        const big_idx = BigIdx{
            .attributes = Attributes.fromString(text),
            .idx = @as(u28, @intCast(@intFromEnum(interner_idx))),
        };
        return Idx.fromInner(.{
            .is_small = false,
            .data = .{ .big = big_idx },
        });
    }

    /// Freeze the identifier store, preventing any new entries from being added.
    pub fn freeze(self: *Store) void {
        self.interner.freeze();
    }
    /// Calculate the size needed to serialize this Ident.Store
    pub fn serializedSize(self: *const Store) usize {
        var size: usize = 0;

        // SmallStringInterner components
        size += @sizeOf(u32); // bytes_len
        size += self.interner.bytes.len(); // bytes data
        size = std.mem.alignForward(usize, size, @alignOf(u32)); // align for next u32

        size += @sizeOf(u32); // next_unique_name

        // Align to SERIALIZATION_ALIGNMENT to maintain alignment for subsequent data
        return std.mem.alignForward(usize, size, serialization.SERIALIZATION_ALIGNMENT);
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

        // Deserialize next_unique_name
        if (offset + @sizeOf(u32) > buffer.len) return error.BufferTooSmall;
        const next_unique_name = @as(*const u32, @ptrCast(@alignCast(buffer.ptr + offset))).*;

        // Create empty hash table
        const hash_table = collections.SafeList(SmallStringInterner.Idx){};

        // Construct the interner
        const interner = SmallStringInterner{
            .bytes = bytes,
            .hash_table = hash_table,
            .entry_count = 0,
            .frozen = if (std.debug.runtime_safety) false else {},
        };

        return Store{
            .interner = interner,
            .next_unique_name = next_unique_name,
        };
    }

    /// Serialize this Store to the given CompactWriter. The resulting Store
    /// in the writer's buffer will have offsets instead of pointers. Calling any
    /// methods on it or dereferencing its internal "pointers" (which are now
    /// offsets) is illegal behavior!
    pub fn serialize(
        self: *const Store,
        allocator: std.mem.Allocator,
        writer: *serialization.CompactWriter,
    ) std.mem.Allocator.Error!*const Store {
        // First, write the Store struct itself
        const offset_self = try writer.appendAlloc(allocator, Store);

        // Then serialize the sub-structures and update the struct
        offset_self.* = .{
            .interner = (try self.interner.serialize(allocator, writer)).*,
            .attributes = (try self.attributes.serialize(allocator, writer)).*,
            .next_unique_name = self.next_unique_name,
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
    try testing.expectEqual(@as(usize, 0), deserialized.interner.bytes.len());
    try testing.expectEqual(@as(usize, 0), deserialized.interner.strings.count());
    try testing.expectEqual(@as(usize, 0), deserialized.attributes.len());
    try testing.expectEqual(@as(u32, 0), deserialized.next_unique_name);
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
    try testing.expectEqualStrings("hello", deserialized.getText(idx1));
    try testing.expectEqualStrings("world!", deserialized.getText(idx2));
    try testing.expectEqualStrings("_ignored", deserialized.getText(idx3));

    // Verify next_unique_name is preserved
    try testing.expectEqual(original.next_unique_name, deserialized.next_unique_name);

    // Verify the interner's hash map is empty after deserialization
    try testing.expectEqual(@as(usize, 0), deserialized.interner.strings.count());
}

test "Ident.Store with genUnique CompactWriter roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create store and generate unique identifiers
    var original = try Store.initCapacity(gpa, 10);
    defer original.deinit(gpa);

    // Add some regular identifiers
    const ident1 = Ident.for_text("regular");
    const idx1 = try original.insert(gpa, ident1);

    // Generate unique identifiers
    const unique1 = try original.genUnique(gpa);
    const unique2 = try original.genUnique(gpa);
    const unique3 = try original.genUnique(gpa);

    // Verify unique names are correct
    try testing.expectEqualStrings("0", original.getText(unique1));
    try testing.expectEqualStrings("1", original.getText(unique2));
    try testing.expectEqualStrings("2", original.getText(unique3));

    // Verify next_unique_name was incremented
    try testing.expectEqual(@as(u32, 3), original.next_unique_name);

    // Create a temp file
    const tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_unique_store.dat", .{ .read = true });
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

    // Verify all identifiers
    try testing.expectEqualStrings("regular", deserialized.getText(idx1));
    try testing.expectEqualStrings("0", deserialized.getText(unique1));
    try testing.expectEqualStrings("1", deserialized.getText(unique2));
    try testing.expectEqualStrings("2", deserialized.getText(unique3));

    // Verify next_unique_name is preserved
    try testing.expectEqual(@as(u32, 3), deserialized.next_unique_name);
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
        try testing.expectEqual(test_ident.expected_idx, idx.getIdx());
    }

    // Add some unique names
    const unique1 = try original.genUnique(gpa);
    const unique2 = try original.genUnique(gpa);

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
        const text = deserialized.getText(idx);
        try testing.expectEqualStrings(test_ident.text, text);
    }

    // Verify unique names
    try testing.expectEqualStrings("0", deserialized.getText(unique1));
    try testing.expectEqualStrings("1", deserialized.getText(unique2));

    // Verify the interner's hash map is empty after deserialization
    try testing.expectEqual(@as(usize, 0), deserialized.interner.strings.count());

    // Verify next_unique_name
    try testing.expectEqual(@as(u32, 2), deserialized.next_unique_name);
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
    _ = try store1.genUnique(gpa);

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
    try testing.expectEqualStrings("store1_ident", deserialized1.getText(idx1_1));
    try testing.expectEqual(@as(u32, 1), deserialized1.next_unique_name);

    // Verify store 2 (frozen)
    try testing.expectEqualStrings("store2_ident!", deserialized2.getText(idx2_1));
    try testing.expectEqualStrings("_store2_ignored", deserialized2.getText(idx2_2));
    if (std.debug.runtime_safety) {
        try testing.expect(deserialized2.interner.frozen);
    }

    // Verify store 3
    try testing.expectEqualStrings("store3", deserialized3.getText(idx3_1));

    // Verify all have empty hash maps
    try testing.expectEqual(@as(usize, 0), deserialized1.interner.strings.count());
    try testing.expectEqual(@as(usize, 0), deserialized2.interner.strings.count());
    try testing.expectEqual(@as(usize, 0), deserialized3.interner.strings.count());
}
