//! Any text in a Roc source file that has significant content.
//!
//! During tokenization, all variable names, record field names, type names, etc. are interned
//! into a deduplicated collection, the [Ident.Store]. On interning, each Ident gets a unique ID
//! that represents that string, which can be used to look up the string value in the [Ident.Store]
//! in constant time. Storing IDs in each IR instead of strings also uses less memory in the IRs.

const std = @import("std");
const collections = @import("../collections.zig");
const Region = @import("Region.zig");
const ModuleImport = @import("ModuleImport.zig");

const SmallStringInterner = collections.SmallStringInterner;
const exitOnOom = collections.utils.exitOnOom;

const Ident = @This();

/// The original text of the identifier.
raw_text: []const u8,

/// Attributes of the identifier such as if it is effectful, ignored, or reassignable.
attributes: Attributes,

/// Create a new identifier from a string.
pub fn for_text(text: []const u8) Ident {
    // Parse identifier attributes from the text
    const is_ignored = std.mem.startsWith(u8, text, "_");
    const is_effectful = std.mem.endsWith(u8, text, "!");
    // TODO: parse reassignable attribute (var keyword handling)

    return Ident{
        .raw_text = text,
        .attributes = Attributes{
            .effectful = is_effectful,
            .ignored = is_ignored,
            .reassignable = false,
        },
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

    // Parse identifier attributes from the bytes
    const is_ignored = std.mem.startsWith(u8, bytes, "_");
    const is_effectful = std.mem.endsWith(u8, bytes, "!");
    // TODO: parse reassignable attribute (var keyword handling)

    return Ident{
        .raw_text = bytes,
        .attributes = Attributes{
            .effectful = is_effectful,
            .ignored = is_ignored,
            .reassignable = false,
        },
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
};

/// An interner for identifier names.
pub const Store = struct {
    interner: SmallStringInterner = .{},
    /// The index of the local module import that this ident comes from.
    ///
    /// By default, this is set to index 0, the primary module being compiled.
    /// This needs to be set when the ident is first seen during canonicalization
    /// before doing anything else.
    exposing_modules: std.ArrayListUnmanaged(ModuleImport.Idx) = .{},
    attributes: std.ArrayListUnmanaged(Attributes) = .{},
    next_unique_name: u32 = 0,

    /// Initialize the memory for an `Ident.Store` with a specific capaicty.
    pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) Store {
        return .{
            .interner = SmallStringInterner.initCapacity(gpa, capacity),
            .exposing_modules = std.ArrayListUnmanaged(ModuleImport.Idx).initCapacity(gpa, capacity) catch |err| exitOnOom(err),
            .attributes = std.ArrayListUnmanaged(Attributes).initCapacity(gpa, capacity) catch |err| exitOnOom(err),
        };
    }

    /// Deinitialize the memory for an `Ident.Store`.
    pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
        self.interner.deinit(gpa);
        self.exposing_modules.deinit(gpa);
        self.attributes.deinit(gpa);
    }

    /// Insert a new identifier into the store.
    pub fn insert(self: *Store, gpa: std.mem.Allocator, ident: Ident, region: Region) Idx {
        const idx = self.interner.insert(gpa, ident.raw_text, region);
        self.exposing_modules.append(gpa, @enumFromInt(0)) catch |err| exitOnOom(err);
        self.attributes.append(gpa, ident.attributes) catch |err| exitOnOom(err);

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
    pub fn genUnique(self: *Store, gpa: std.mem.Allocator) Idx {
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

        const name = str_buffer[digit_index..];

        const idx = self.interner.insert(gpa, name, Region.zero());
        self.exposing_modules.append(gpa, @enumFromInt(0)) catch |err| exitOnOom(err);

        const attributes = Attributes{
            .effectful = false,
            .ignored = false,
            .reassignable = false,
        };

        self.attributes.append(gpa, attributes) catch |err| exitOnOom(err);

        return Idx{
            .attributes = attributes,
            .idx = @truncate(@intFromEnum(idx)),
        };
    }

    /// Checks whether two identifiers have the same text.
    ///
    /// This runs in constant time because it just checks if both idents
    /// point to the same deduped string.
    pub fn identsHaveSameText(
        self: *const Store,
        first_idx: Idx,
        second_idx: Idx,
    ) bool {
        return self.interner.indicesHaveSameText(
            @enumFromInt(@as(u32, first_idx.idx)),
            @enumFromInt(@as(u32, second_idx.idx)),
        );
    }

    /// Get the text for an identifier.
    pub fn getText(self: *const Store, idx: Idx) []u8 {
        return self.interner.getText(@enumFromInt(@as(u32, idx.idx)));
    }

    /// Get the region for an identifier.
    pub fn getRegion(self: *const Store, idx: Idx) Region {
        return self.interner.getRegion(@enumFromInt(@as(u32, idx.idx)));
    }

    // TODO: should this get moved out of here and into canonicalization?
    //
    /// Get the index of the imported module for an identifier.
    pub fn getExposingModule(self: *const Store, idx: Idx) ModuleImport.Idx {
        return self.exposing_modules.items[@as(usize, idx.idx)];
    }

    /// Set the module import that exposes this ident.
    ///
    /// NOTE: This should be called as soon as an ident is encountered during
    /// canonicalization to make sure that we don't have to worry if the exposing
    /// module is zero because it hasn't been set yet or if it's actually zero.
    pub fn setExposingModule(self: *const Store, idx: Idx, exposing_module: ModuleImport.Idx) void {
        self.exposing_modules.items[@as(usize, idx.idx)] = exposing_module;
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
