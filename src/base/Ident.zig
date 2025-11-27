//! Any text in a Roc source file that has significant content.
//!
//! During tokenization, all variable names, record field names, type names, etc. are interned
//! into a deduplicated collection, the [Ident.Store]. On interning, each Ident gets a unique ID
//! that represents that string, which can be used to look up the string value in the [Ident.Store]
//! in constant time. Storing IDs in each IR instead of strings also uses less memory in the IRs.

const std = @import("std");
const builtin = @import("builtin");
const serialization = @import("serialization");
const collections = @import("collections");

const Region = @import("Region.zig");
const SmallStringInterner = @import("SmallStringInterner.zig");

const CompactWriter = collections.CompactWriter;

const Ident = @This();

/// Whether to enable debug store tracking. This adds runtime checks to verify
/// that Idx values are only looked up in the store that created them.
const enable_store_tracking = builtin.mode == .Debug;

/// Method name for parsing integers from digit lists - used by numeric literal type checking
pub const FROM_INT_DIGITS_METHOD_NAME = "from_int_digits";
/// Method name for parsing decimals from digit lists - used by numeric literal type checking
pub const FROM_DEC_DIGITS_METHOD_NAME = "from_dec_digits";
/// Method name for addition - used by + operator desugaring
pub const PLUS_METHOD_NAME = "plus";
/// Method name for negation - used by unary - operator desugaring
pub const NEGATE_METHOD_NAME = "negate";

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

    /// Sentinel value representing no/unset ident
    pub const NONE: Idx = .{ .attributes = .{ .effectful = true, .ignored = true, .reassignable = true }, .idx = std.math.maxInt(u29) };

    pub fn isNone(self: Idx) bool {
        return self.idx == NONE.idx and @as(u3, @bitCast(self.attributes)) == @as(u3, @bitCast(NONE.attributes));
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
            .reassignable = std.mem.startsWith(u8, text, "$"),
        };
    }
};

/// Debug-only info for store provenance tracking.
const StoreDebugInfo = struct {
    store_id: []const u8,
    known_idxs: std.AutoHashMapUnmanaged(u32, void),
};

/// Global counter for generating unique store IDs.
/// This counter survives struct copies because the ID is stored in the Store struct itself.
/// Using u32 for cross-platform compatibility (wasm32 doesn't support 64-bit atomics).
var debug_store_id_counter: if (enable_store_tracking) std.atomic.Value(u32) else void =
    if (enable_store_tracking) std.atomic.Value(u32).init(1) else {};

/// Global map from Store's unique debug_id to debug info.
/// Protected by a mutex for thread safety.
var debug_store_map: if (enable_store_tracking) std.AutoHashMapUnmanaged(u32, StoreDebugInfo) else void = if (enable_store_tracking) .{} else {};

/// Mutex protecting the debug_store_map.
var debug_store_mutex: if (enable_store_tracking) std.Thread.Mutex else void = if (enable_store_tracking) .{} else {};

/// An interner for identifier names.
pub const Store = struct {
    interner: SmallStringInterner,
    attributes: collections.SafeList(Attributes) = .{},
    next_unique_name: u32 = 0,

    /// Debug-only: unique ID for this store instance.
    /// This ID is assigned on first insert and survives struct copies.
    /// 0 means unassigned.
    debug_id: if (enable_store_tracking) u32 else void = if (enable_store_tracking) 0 else {},

    /// Debug-only: get or assign a unique ID for this store.
    fn getOrAssignDebugId(self: *Store, src: std.builtin.SourceLocation) u32 {
        if (enable_store_tracking) {
            if (self.debug_id == 0) {
                // If this store already has idents (e.g., deserialized), we can't
                // fully track it because existing idents weren't registered.
                // Keep debug_id at 0 to skip verification for this store.
                if (self.interner.entry_count > 0) {
                    return 0;
                }

                // Assign a new unique ID
                self.debug_id = debug_store_id_counter.fetchAdd(1, .monotonic);

                // Register in the global map with source location info
                const store_id = std.fmt.allocPrint(std.heap.page_allocator, "{s}:{d}:{d}", .{
                    src.file,
                    src.line,
                    src.column,
                }) catch "unknown";

                debug_store_map.put(std.heap.page_allocator, self.debug_id, .{
                    .store_id = store_id,
                    .known_idxs = .{},
                }) catch {};
            }
            return self.debug_id;
        } else {
            return 0;
        }
    }

    /// Debug-only: unregister this store from the global debug map.
    fn unregisterFromTracking(self: *Store) void {
        if (enable_store_tracking) {
            if (self.debug_id == 0) return; // Never registered

            debug_store_mutex.lock();
            defer debug_store_mutex.unlock();

            if (debug_store_map.fetchRemove(self.debug_id)) |entry| {
                // Free the heap-allocated store_id (if it's not the static "unknown" string)
                if (entry.value.store_id.ptr != @as([*]const u8, "unknown".ptr)) {
                    std.heap.page_allocator.free(entry.value.store_id);
                }
                // Copy the known_idxs to make it mutable for deinit
                var known_idxs = entry.value.known_idxs;
                known_idxs.deinit(std.heap.page_allocator);
            }
        }
    }

    /// Debug-only: track an Idx as belonging to this store.
    fn trackIdx(self: *Store, idx: Idx, src: std.builtin.SourceLocation) void {
        if (enable_store_tracking) {
            debug_store_mutex.lock();
            defer debug_store_mutex.unlock();

            const debug_id = self.getOrAssignDebugId(src);
            if (debug_store_map.getPtr(debug_id)) |info| {
                // We don't fail on OOM in debug tracking - just skip tracking
                info.known_idxs.put(std.heap.page_allocator, @bitCast(idx), {}) catch {};
            }
        }
    }

    /// Debug-only: verify an Idx belongs to this store.
    fn verifyIdx(self: *const Store, idx: Idx) void {
        if (enable_store_tracking) {
            if (self.debug_id == 0) {
                // Store was never registered (e.g., deserialized store).
                // Skip verification.
                return;
            }

            debug_store_mutex.lock();
            defer debug_store_mutex.unlock();

            const info = debug_store_map.get(self.debug_id) orelse {
                // Store not in map (shouldn't happen if debug_id != 0)
                return;
            };

            const idx_bits: u32 = @bitCast(idx);
            if (!info.known_idxs.contains(idx_bits)) {
                std.debug.panic(
                    "Ident.Idx lookup in wrong store: Idx {d} (0x{x}) not found in store '{s}' (debug_id={d}). " ++
                        "This Idx was created by a different store.",
                    .{ idx.idx, idx_bits, info.store_id, self.debug_id },
                );
            }
        }
    }

    /// Check if an Idx was created by this store.
    /// In debug builds with store tracking enabled, this checks the known_idxs set.
    /// In release builds or when tracking is disabled, this returns true (assumes valid).
    /// Use this to determine which store to use for lookups when idents may come from
    /// multiple sources (e.g., during type unification with builtins).
    pub fn containsIdx(self: *const Store, idx: Idx) bool {
        if (enable_store_tracking) {
            if (self.debug_id == 0) {
                // Store was never registered (e.g., deserialized store).
                // Can't verify, assume true.
                return true;
            }

            debug_store_mutex.lock();
            defer debug_store_mutex.unlock();

            const info = debug_store_map.get(self.debug_id) orelse {
                // Store not in map
                return true;
            };

            const idx_bits: u32 = @bitCast(idx);
            return info.known_idxs.contains(idx_bits);
        } else {
            // No tracking, can't determine - assume true
            return true;
        }
    }

    /// Serialized representation of an Ident.Store
    /// Uses extern struct to guarantee consistent field layout across optimization levels.
    pub const Serialized = extern struct {
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
            // Note: Serialized may be smaller than the runtime struct.
            // We deserialize by overwriting the Serialized memory with the runtime struct.
            const store = @as(*Store, @ptrFromInt(@intFromPtr(self)));

            store.* = Store{
                .interner = self.interner.deserialize(offset).*,
                .attributes = self.attributes.deserialize(offset).*,
                .next_unique_name = self.next_unique_name,
            };

            // Note: We don't register deserialized stores for debug tracking.
            // This is fine because the debug tracking is meant to catch bugs during fresh compilation.

            return store;
        }
    };

    /// Initialize the memory for an `Ident.Store` with a specific capacity.
    pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!Store {
        return .{
            .interner = try SmallStringInterner.initCapacity(gpa, capacity),
        };
    }

    /// Deinitialize the memory for an `Ident.Store`.
    pub fn deinit(self: *Store, gpa: std.mem.Allocator) void {
        self.interner.deinit(gpa);
        self.attributes.deinit(gpa);
        self.unregisterFromTracking();
    }

    /// Insert a new identifier into the store.
    pub fn insert(self: *Store, gpa: std.mem.Allocator, ident: Ident) std.mem.Allocator.Error!Idx {
        const idx = try self.interner.insert(gpa, ident.raw_text);

        const result = Idx{
            .attributes = ident.attributes,
            .idx = @as(u29, @intCast(@intFromEnum(idx))),
        };

        self.trackIdx(result, @src());

        return result;
    }

    /// Look up an identifier in the store without inserting.
    /// Returns the index if found, null if not found.
    /// Unlike insert, this never modifies the store (no resize, no insertion).
    /// Useful for deserialized stores that cannot be grown.
    pub fn lookup(self: *const Store, ident: Ident) ?Idx {
        const idx = self.interner.lookup(ident.raw_text) orelse return null;

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

        const result = Idx{
            .attributes = attributes,
            .idx = @truncate(@intFromEnum(idx)),
        };

        self.trackIdx(result, @src());

        return result;
    }

    /// Get the text for an identifier.
    pub fn getText(self: *const Store, idx: Idx) []u8 {
        self.verifyIdx(idx);
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
        const result = self.interner.findStringOrSlot(text);
        const interner_idx = result.idx orelse return null;

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
        size += self.interner.bytes.len(); // bytes data
        size = std.mem.alignForward(usize, size, @alignOf(u32)); // align for next u32

        size += @sizeOf(u32); // next_unique_name

        // Align to SERIALIZATION_ALIGNMENT to maintain alignment for subsequent data
        return std.mem.alignForward(usize, size, collections.SERIALIZATION_ALIGNMENT.toByteUnits());
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
    try std.testing.expectError(Ident.Error.EmptyText, result);
}

test "from_bytes validates null bytes" {
    const text_with_null = "hello\x00world";
    const result = Ident.from_bytes(text_with_null);
    try std.testing.expectError(Ident.Error.ContainsNullByte, result);
}

test "from_bytes validates control characters" {
    const text_with_control = "hello\x01world";
    const result = Ident.from_bytes(text_with_control);
    try std.testing.expectError(Ident.Error.ContainsControlCharacters, result);
}

test "from_bytes disallows common whitespace" {
    const text_with_space = "hello world";
    const result = Ident.from_bytes(text_with_space);
    try std.testing.expect(result == Ident.Error.ContainsControlCharacters);

    const text_with_tab = "hello\tworld";
    const result2 = Ident.from_bytes(text_with_tab);
    try std.testing.expect(result2 == Ident.Error.ContainsControlCharacters);

    const text_with_newline = "hello\nworld";
    const result3 = Ident.from_bytes(text_with_newline);
    try std.testing.expect(result3 == Ident.Error.ContainsControlCharacters);

    const text_with_cr = "hello\rworld";
    const result4 = Ident.from_bytes(text_with_cr);
    try std.testing.expect(result4 == Ident.Error.ContainsControlCharacters);
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

test "from_bytes creates reassignable identifier" {
    const result = try Ident.from_bytes("$reusable");
    try std.testing.expectEqualStrings("$reusable", result.raw_text);
    try std.testing.expect(result.attributes.effectful == false);
    try std.testing.expect(result.attributes.ignored == false);
    try std.testing.expect(result.attributes.reassignable == true);
}

test "Ident.Store empty CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create an empty Store
    var original = try Ident.Store.initCapacity(gpa, 0);
    defer original.deinit(gpa);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_empty_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter with arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    _ = try original.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();

    // Ensure file size matches what we wrote
    try std.testing.expectEqual(@as(u64, @intCast(writer.total_bytes)), file_size);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    const bytes_read = try file.read(buffer);
    try std.testing.expectEqual(writer.total_bytes, bytes_read);

    // Cast and relocate
    // The Store struct should be at the beginning (it was appended first)
    const deserialized = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr)));

    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify empty - interner always has at least 1 byte (0) to ensure Idx.unused doesn't point to valid data
    try std.testing.expectEqual(@as(usize, 1), deserialized.interner.bytes.len());
    try std.testing.expectEqual(@as(u32, 0), deserialized.interner.entry_count);
    try std.testing.expectEqual(@as(usize, 0), deserialized.attributes.len());
    try std.testing.expectEqual(@as(u32, 0), deserialized.next_unique_name);
}

test "Ident.Store basic CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create original store and add some identifiers
    var original = try Ident.Store.initCapacity(gpa, 16);
    defer original.deinit(gpa);

    const ident1 = Ident.for_text("hello");
    const ident2 = Ident.for_text("world!");
    const ident3 = Ident.for_text("_ignored");

    const idx1 = try original.insert(gpa, ident1);
    const idx2 = try original.insert(gpa, ident2);
    const idx3 = try original.insert(gpa, ident3);

    // Verify the attributes in the indices
    try std.testing.expect(!idx1.attributes.effectful);
    try std.testing.expect(!idx1.attributes.ignored);
    try std.testing.expect(idx2.attributes.effectful);
    try std.testing.expect(!idx2.attributes.ignored);
    try std.testing.expect(!idx3.attributes.effectful);
    try std.testing.expect(idx3.attributes.ignored);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_basic_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using CompactWriter with arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    _ = try original.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();

    // Ensure file size matches what we wrote
    try std.testing.expectEqual(@as(u64, @intCast(writer.total_bytes)), file_size);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    const bytes_read = try file.read(buffer);
    try std.testing.expectEqual(writer.total_bytes, bytes_read);

    // Cast and relocate
    // The Store struct should be at the beginning (it was appended first)
    const deserialized = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr)));

    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Check the bytes length for validation
    const bytes_len = deserialized.interner.bytes.len();
    const idx1_value = @intFromEnum(@as(SmallStringInterner.Idx, @enumFromInt(@as(u32, idx1.idx))));

    // Verify the index is valid
    if (bytes_len <= idx1_value) {
        return error.InvalidIndex;
    }

    // Verify the identifiers are accessible
    try std.testing.expectEqualStrings("hello", deserialized.getText(idx1));
    try std.testing.expectEqualStrings("world!", deserialized.getText(idx2));
    try std.testing.expectEqualStrings("_ignored", deserialized.getText(idx3));

    // Verify next_unique_name is preserved
    try std.testing.expectEqual(original.next_unique_name, deserialized.next_unique_name);

    // Verify the interner's entry count is preserved
    try std.testing.expectEqual(@as(u32, 3), deserialized.interner.entry_count);
}

test "Ident.Store with genUnique CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create store and generate unique identifiers
    var original = try Ident.Store.initCapacity(gpa, 10);
    defer original.deinit(gpa);

    // Add some regular identifiers
    const ident1 = Ident.for_text("regular");
    const idx1 = try original.insert(gpa, ident1);

    // Generate unique identifiers
    const unique1 = try original.genUnique(gpa);
    const unique2 = try original.genUnique(gpa);
    const unique3 = try original.genUnique(gpa);

    // Verify unique names are correct
    try std.testing.expectEqualStrings("0", original.getText(unique1));
    try std.testing.expectEqualStrings("1", original.getText(unique2));
    try std.testing.expectEqualStrings("2", original.getText(unique3));

    // Verify next_unique_name was incremented
    try std.testing.expectEqual(@as(u32, 3), original.next_unique_name);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_unique_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    _ = try original.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();

    // Ensure file size matches what we wrote
    try std.testing.expectEqual(@as(u64, @intCast(writer.total_bytes)), file_size);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    const bytes_read = try file.read(buffer);
    try std.testing.expectEqual(writer.total_bytes, bytes_read);

    // Cast and relocate
    // The Store struct should be at the beginning (it was appended first)
    const deserialized = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr)));

    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all identifiers

    try std.testing.expectEqualStrings("regular", deserialized.getText(idx1));
    try std.testing.expectEqualStrings("0", deserialized.getText(unique1));
    try std.testing.expectEqualStrings("1", deserialized.getText(unique2));
    try std.testing.expectEqualStrings("2", deserialized.getText(unique3));

    // Verify next_unique_name is preserved
    try std.testing.expectEqual(@as(u32, 3), deserialized.next_unique_name);
}

test "Ident.Store CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create and populate store
    var original = try Ident.Store.initCapacity(gpa, 5);
    defer original.deinit(gpa);

    _ = try original.insert(gpa, Ident.for_text("test1"));
    _ = try original.insert(gpa, Ident.for_text("test2"));

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_frozen_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    _ = try original.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();

    // Ensure file size matches what we wrote
    try std.testing.expectEqual(@as(u64, @intCast(writer.total_bytes)), file_size);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    const bytes_read = try file.read(buffer);
    try std.testing.expectEqual(writer.total_bytes, bytes_read);

    // Cast and relocate
    // The Store struct should be at the beginning (it was appended first)
    const deserialized = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr)));

    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));
}

test "Ident.Store comprehensive CompactWriter roundtrip" {
    const gpa = std.testing.allocator;

    // Create store with various identifiers
    var original = try Ident.Store.initCapacity(gpa, 20);
    defer original.deinit(gpa);

    // Test various identifier types and edge cases
    // Note: SmallStringInterner starts with a 0 byte at index 0, so strings start at index 1
    const test_idents = [_]struct { text: []const u8, expected_idx: u32 }{
        .{ .text = "hello", .expected_idx = 1 },
        .{ .text = "world!", .expected_idx = 7 },
        .{ .text = "_ignored", .expected_idx = 14 },
        .{ .text = "a", .expected_idx = 23 }, // single character
        .{ .text = "very_long_identifier_name_that_might_cause_issues", .expected_idx = 25 },
        .{ .text = "effectful!", .expected_idx = 75 },
        .{ .text = "_", .expected_idx = 86 }, // Just underscore
        .{ .text = "CamelCase", .expected_idx = 88 },
        .{ .text = "snake_case", .expected_idx = 98 },
        .{ .text = "SCREAMING_CASE", .expected_idx = 109 },
        .{ .text = "hello", .expected_idx = 1 }, // duplicate, should reuse
    };

    var indices = std.ArrayList(Ident.Idx).empty;
    defer indices.deinit(gpa);

    for (test_idents) |test_ident| {
        const ident = Ident.for_text(test_ident.text);
        const idx = try original.insert(gpa, ident);
        try indices.append(gpa, idx);
        // Verify the index matches expectation
        try std.testing.expectEqual(test_ident.expected_idx, idx.idx);
    }

    // Add some unique names
    const unique1 = try original.genUnique(gpa);
    const unique2 = try original.genUnique(gpa);

    // Verify the interner's hash map is populated
    try std.testing.expect(original.interner.entry_count > 0);

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file = try tmp_dir.dir.createFile("test_comprehensive_store.dat", .{ .read = true });
    defer file.close();

    // Serialize using arena allocator
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_allocator = arena.allocator();

    var writer = CompactWriter.init();
    defer writer.deinit(arena_allocator);

    _ = try original.serialize(arena_allocator, &writer);

    // Write to file
    try writer.writeGather(arena_allocator, file);

    // Read back
    try file.seekTo(0);
    const file_size = try file.getEndPos();

    // Ensure file size matches what we wrote
    try std.testing.expectEqual(@as(u64, @intCast(writer.total_bytes)), file_size);

    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.@"16", @as(usize, @intCast(file_size)));
    defer gpa.free(buffer);

    const bytes_read = try file.read(buffer);
    try std.testing.expectEqual(writer.total_bytes, bytes_read);

    // Cast and relocate
    // The Store struct should be at the beginning (it was appended first)
    const deserialized = @as(*Ident.Store, @ptrCast(@alignCast(buffer.ptr)));

    deserialized.relocate(@as(isize, @intCast(@intFromPtr(buffer.ptr))));

    // Verify all identifiers (skip duplicate at end)
    for (test_idents[0..10], 0..) |test_ident, i| {
        const idx = indices.items[i];
        const text = deserialized.getText(idx);
        try std.testing.expectEqualStrings(test_ident.text, text);
    }

    // Verify unique names
    try std.testing.expectEqualStrings("0", deserialized.getText(unique1));
    try std.testing.expectEqualStrings("1", deserialized.getText(unique2));

    // Verify the interner's entry count is preserved
    // We inserted 10 unique strings + 2 generated unique names = 12 total
    try std.testing.expectEqual(@as(u32, 12), deserialized.interner.entry_count);

    // Verify next_unique_name
    try std.testing.expectEqual(@as(u32, 2), deserialized.next_unique_name);
}
