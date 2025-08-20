//! CommonEnv provides shared environment state that is common across all modules.
//! This includes identifier and string literal interning, exposed items tracking,
//! source code management, and diagnostic information like line start positions.
//! It serves as a central repository for data that needs to be accessed across
//! different phases of compilation.

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("collections");
const serialization = @import("serialization");

const Ident = @import("Ident.zig");
const StringLiteral = @import("StringLiteral.zig");
const RegionInfo = @import("RegionInfo.zig");
const Region = @import("Region.zig");
const SExprTree = @import("SExprTree.zig");
const SafeList = collections.SafeList;
const ExposedItems = collections.ExposedItems;
const CompactWriter = collections.CompactWriter;

const CommonEnv = @This();

idents: Ident.Store,
// ident_ids_for_slicing: SafeList(Ident.Idx),
strings: StringLiteral.Store,
/// The items (a combination of types and values) that this module exposes
exposed_items: ExposedItems,
/// Line starts for error reporting. We retain only start and offset positions in the IR
/// and then use these line starts to calculate the line number and column number as required.
/// this is a more compact representation at the expense of extra computation only when generating error diagnostics.
line_starts: SafeList(u32),
/// The source code of this module.
source: []const u8,

pub fn init(gpa: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!CommonEnv {
    return CommonEnv{
        .idents = try Ident.Store.initCapacity(gpa, 1024),
        .strings = try StringLiteral.Store.initCapacityBytes(gpa, 4096),
        .exposed_items = ExposedItems.init(),
        .line_starts = try RegionInfo.findLineStarts(gpa, source),
        .source = source,
    };
}

pub fn deinit(self: *CommonEnv, gpa: std.mem.Allocator) void {
    self.idents.deinit(gpa);
    self.strings.deinit(gpa);
    self.exposed_items.deinit(gpa);
    self.line_starts.deinit(gpa);
}

/// Add the given offset to the memory addresses of all pointers in `self`.
pub fn relocate(self: *CommonEnv, offset: isize) void {
    // Relocate all sub-structures
    self.idents.relocate(offset);
    self.strings.relocate(offset);
    self.exposed_items.relocate(offset);
    self.line_starts.relocate(offset);
    // Note: source is not relocated - it should be set manually
}

/// Serialize this CommonEnv to the given CompactWriter.
/// IMPORTANT: The returned pointer points to memory inside the writer!
/// Attempting to dereference this pointer or calling any methods on it
/// is illegal behavior!
pub fn serialize(
    self: *const CommonEnv,
    allocator: std.mem.Allocator,
    writer: *CompactWriter,
) std.mem.Allocator.Error!*const CommonEnv {
    // First, write the CommonEnv struct itself
    const offset_self = try writer.appendAlloc(allocator, CommonEnv);

    // Then serialize the sub-structures and update the struct
    offset_self.* = .{
        .idents = (try self.idents.serialize(allocator, writer)).*,
        .strings = (try self.strings.serialize(allocator, writer)).*,
        .exposed_items = (try self.exposed_items.serialize(allocator, writer)).*,
        .line_starts = (try self.line_starts.serialize(allocator, writer)).*,
        .source = "", // Will be set when deserializing
    };

    return @constCast(offset_self);
}

/// Freezes the identifier and string interners, preventing further modifications.
/// This is used to ensure thread safety when sharing the environment across threads.
pub fn freezeInterners(self: *CommonEnv) void {
    self.idents.freeze();
    self.strings.freeze();
}

/// Serialized representation of ModuleEnv
pub const Serialized = struct {
    idents: Ident.Store.Serialized,
    strings: StringLiteral.Store.Serialized,
    exposed_items: ExposedItems.Serialized,
    line_starts: SafeList(u32).Serialized,
    source: []const u8, // Serialized as zeros, provided during deserialization

    /// Serialize a ModuleEnv into this Serialized struct, appending data to the writer
    pub fn serialize(
        self: *Serialized,
        env: *const CommonEnv,
        allocator: std.mem.Allocator,
        writer: *CompactWriter,
    ) !void {
        self.source = ""; // Empty slice

        // Serialize each component using its Serialized struct
        try self.idents.serialize(&env.idents, allocator, writer);
        try self.strings.serialize(&env.strings, allocator, writer);
        try self.exposed_items.serialize(&env.exposed_items, allocator, writer);
        try self.line_starts.serialize(&env.line_starts, allocator, writer);
    }

    /// Deserialize a CommonEnv from the buffer, updating the CommonEnv in place
    pub fn deserialize(
        self: *Serialized,
        offset: i64,
        source: []const u8,
    ) *CommonEnv {
        // CommonEnv.Serialized should be at least as big as CommonEnv
        std.debug.assert(@sizeOf(Serialized) >= @sizeOf(CommonEnv));

        // Overwrite ourself with the deserialized version, and return our pointer after casting it to CommonEnv.
        const env = @as(*CommonEnv, @ptrFromInt(@intFromPtr(self)));

        env.* = CommonEnv{
            .idents = self.idents.deserialize(offset).*,
            // .ident_ids_for_slicing = self.ident_ids_for_slicing.deserialize(offset).*,
            .strings = self.strings.deserialize(offset).*,
            .exposed_items = self.exposed_items.deserialize(offset).*,
            .line_starts = self.line_starts.deserialize(offset).*,
            .source = source,
        };

        return env;
    }
};

/// Inserts an identifier into the store and returns its index.
pub fn insertIdent(self: *CommonEnv, gpa: std.mem.Allocator, ident: Ident) std.mem.Allocator.Error!Ident.Idx {
    return try self.idents.insert(gpa, ident);
}

/// Searches for an identifier by its text content, returning its index if found.
pub fn findIdent(self: *const CommonEnv, text: []const u8) ?Ident.Idx {
    return self.idents.findByString(text);
}

/// Retrieves the text of an identifier by its index.
pub fn getIdent(self: *const CommonEnv, idx: Ident.Idx) []const u8 {
    return self.idents.getText(idx);
}

/// Returns a const reference to the identifier store.
pub fn getIdentStore(self: *const CommonEnv) *const Ident.Store {
    return &self.idents;
}

/// Inserts a string literal into the store and returns its index.
pub fn insertString(self: *CommonEnv, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!StringLiteral.Idx {
    return try self.strings.insert(gpa, string);
}

/// Retrieves a string literal by its index.
pub fn getString(self: *const CommonEnv, idx: StringLiteral.Idx) []const u8 {
    return self.strings.get(idx);
}

/// Returns a mutable reference to the string literal store.
pub fn getStringStore(self: *CommonEnv) *StringLiteral.Store {
    return &self.strings;
}

/// Adds an identifier to the exposed items list by its index.
pub fn addExposedById(self: *CommonEnv, gpa: std.mem.Allocator, ident_idx: Ident.Idx) !void {
    return try self.exposed_items.addExposedById(gpa, @bitCast(ident_idx));
}

/// Retrieves the node index associated with an exposed identifier.
pub fn getNodeIndexById(self: *const CommonEnv, allocator: std.mem.Allocator, ident_idx: Ident.Idx) ?u16 {
    return self.exposed_items.getNodeIndexById(allocator, @bitCast(ident_idx));
}

/// Associates a node index with an exposed identifier.
pub fn setNodeIndexById(self: *CommonEnv, gpa: std.mem.Allocator, ident_idx: Ident.Idx, node_idx: u16) !void {
    return try self.exposed_items.setNodeIndexById(gpa, @bitCast(ident_idx), node_idx);
}

/// Get region info for a given region
pub fn getRegionInfo(self: *const CommonEnv, region: Region) !RegionInfo {
    return RegionInfo.position(
        self.source,
        self.line_starts.items.items,
        region.start.offset,
        region.end.offset,
    );
}

/// Returns diagnostic position information for the given region.
/// This is a standalone utility function that takes the source text as a parameter
/// to avoid storing it in the cacheable IR structure.
pub fn calcRegionInfo(self: *const CommonEnv, region: Region) RegionInfo {
    const empty = RegionInfo{
        .start_line_idx = 0,
        .start_col_idx = 0,
        .end_line_idx = 0,
        .end_col_idx = 0,
    };

    // In the Can IR, regions store byte offsets directly, not token indices.
    // We can use these offsets directly to calculate the diagnostic position.
    const source = self.source;

    const info = RegionInfo.position(
        source,
        self.line_starts.items.items,
        region.start.offset,
        region.end.offset,
    ) catch {
        // Return a zero position if we can't calculate it
        return empty;
    };

    return info;
}

/// Returns the entire source code content.
pub fn getSourceAll(self: *const CommonEnv) []const u8 {
    return self.source;
}

/// Calculate and store line starts from the source text
pub fn calcLineStarts(self: *CommonEnv, gpa: std.mem.Allocator) !void {
    // Reset line_starts by creating a new SafeList
    self.line_starts.deinit(gpa);
    self.line_starts = try collections.SafeList(u32).initCapacity(gpa, 256);

    // if the source is empty, we're done
    if (self.getSourceAll().len == 0) {
        return;
    }

    // the first line starts at offset 0
    _ = try self.line_starts.append(gpa, 0);

    // find all newlines in the source, save their offset
    var pos: u32 = 0;
    for (self.getSourceAll()) |c| {
        if (c == '\n') {
            // next line starts after the newline in the current position
            _ = try self.line_starts.append(gpa, pos + 1);
        }
        pos += 1;
    }
}

/// Returns all line start positions for source code position mapping.
pub fn getLineStartsAll(self: *const CommonEnv) []const u32 {
    return self.line_starts.items.items;
}

/// Get the source text for a given region
pub fn getSource(self: *const CommonEnv, region: Region) []const u8 {
    return self.source[region.start.offset..region.end.offset];
}

/// Get the source line for a given region
pub fn getSourceLine(self: *const CommonEnv, region: Region) ![]const u8 {
    const region_info = try self.getRegionInfo(region);
    const line_start = self.line_starts.items.items[region_info.start_line_idx];
    const line_end = if (region_info.start_line_idx + 1 < self.line_starts.items.items.len)
        self.line_starts.items.items[region_info.start_line_idx + 1]
    else
        self.source.len;

    return self.source[line_start..line_end];
}

test "CommonEnv.Serialized roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const source = "hello world\ntest line 2\n";

    // Create original CommonEnv with some test data
    var original = try CommonEnv.init(gpa, source);
    defer original.deinit(gpa);

    // Add some test data
    const hello_idx = try original.insertIdent(gpa, Ident.for_text("hello"));
    const world_idx = try original.insertIdent(gpa, Ident.for_text("world"));

    _ = try original.insertString(gpa, "test string");
    try original.addExposedById(gpa, hello_idx);

    _ = try original.line_starts.append(gpa, 0);
    _ = try original.line_starts.append(gpa, 10);
    _ = try original.line_starts.append(gpa, 20);

    // Create a CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    // Create temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile("test.compact", .{ .read = true });
    defer tmp_file.close();

    // Serialize using the proper Serialized struct pattern
    const serialized = try writer.appendAlloc(gpa, CommonEnv.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, tmp_file);

    // Read back with proper alignment
    const file_size = try tmp_file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
    defer gpa.free(buffer);
    _ = try tmp_file.pread(buffer, 0);

    // The Serialized struct is at the beginning of the buffer
    const deserialized_ptr = @as(*CommonEnv.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const env = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), source);

    // Verify the data was preserved
    try testing.expectEqualStrings("hello", env.getIdent(hello_idx));
    try testing.expectEqualStrings("world", env.getIdent(world_idx));

    try testing.expectEqual(@as(usize, 1), env.exposed_items.count());
    try testing.expectEqual(@as(usize, 3), env.line_starts.len());
    try testing.expectEqual(@as(u32, 0), env.line_starts.items.items[0]);
    try testing.expectEqual(@as(u32, 10), env.line_starts.items.items[1]);
    try testing.expectEqual(@as(u32, 20), env.line_starts.items.items[2]);

    try testing.expectEqualStrings(source, env.source);
}

test "CommonEnv.Serialized roundtrip with empty data" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const source = "";

    // Create original CommonEnv with no data
    var original = try CommonEnv.init(gpa, source);
    defer original.deinit(gpa);

    // Create a CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    // Create temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile("test_empty.compact", .{ .read = true });
    defer tmp_file.close();

    // Serialize using the proper Serialized struct pattern
    const serialized = try writer.appendAlloc(gpa, CommonEnv.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, tmp_file);

    // Read back with proper alignment
    const file_size = try tmp_file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
    defer gpa.free(buffer);
    _ = try tmp_file.pread(buffer, 0);

    // The Serialized struct is at the beginning of the buffer
    const deserialized_ptr = @as(*CommonEnv.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const env = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), source);

    // Verify empty state is preserved
    try testing.expectEqual(@as(u32, 0), env.idents.interner.entry_count);
    try testing.expectEqual(@as(usize, 0), env.exposed_items.count());
    try testing.expectEqual(@as(usize, 0), env.line_starts.len());
    try testing.expectEqualStrings(source, env.source);
}

test "CommonEnv.Serialized roundtrip with large data" {
    const testing = std.testing;
    const gpa = testing.allocator;

    // Create a larger source with many lines
    var source_builder = std.ArrayList(u8).init(gpa);
    defer source_builder.deinit();

    for (0..100) |i| {
        try source_builder.writer().print("Line {}: This is a test line with some content\n", .{i});
    }
    const source = source_builder.items;

    // Create original CommonEnv with large test data
    var original = try CommonEnv.init(gpa, source);
    defer original.deinit(gpa);

    // Add many identifiers
    var ident_indices = std.ArrayList(Ident.Idx).init(gpa);
    defer ident_indices.deinit();

    for (0..50) |i| {
        var ident_name = std.ArrayList(u8).init(gpa);
        defer ident_name.deinit();
        try ident_name.writer().print("ident_{}", .{i});
        const idx = try original.insertIdent(gpa, Ident.for_text(ident_name.items));
        try ident_indices.append(idx);
    }

    // Add many strings and track their indices
    var string_indices = std.ArrayList(StringLiteral.Idx).init(gpa);
    defer string_indices.deinit();

    for (0..25) |i| {
        var string_content = std.ArrayList(u8).init(gpa);
        defer string_content.deinit();
        try string_content.writer().print("string_literal_{}", .{i});
        const idx = try original.insertString(gpa, string_content.items);
        try string_indices.append(idx);
    }

    // Add some exposed items
    try original.addExposedById(gpa, ident_indices.items[0]);
    try original.addExposedById(gpa, ident_indices.items[10]);
    try original.addExposedById(gpa, ident_indices.items[25]);

    // Add line starts for the source
    try original.calcLineStarts(gpa);

    // Create a CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    // Create temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile("test_large.compact", .{ .read = true });
    defer tmp_file.close();

    // Serialize using the proper Serialized struct pattern
    const serialized = try writer.appendAlloc(gpa, CommonEnv.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, tmp_file);

    // Read back with proper alignment
    const file_size = try tmp_file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
    defer gpa.free(buffer);
    _ = try tmp_file.pread(buffer, 0);

    // The Serialized struct is at the beginning of the buffer
    const deserialized_ptr = @as(*CommonEnv.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const env = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), source);

    // Verify large data was preserved
    try testing.expectEqual(@as(u32, 50), env.idents.interner.entry_count);
    try testing.expectEqual(@as(usize, 3), env.exposed_items.count());
    try testing.expectEqual(@as(usize, 101), env.line_starts.len()); // 100 lines + 1 for first line at offset 0
    try testing.expectEqualStrings(source, env.source);

    // Verify some specific identifiers
    try testing.expectEqualStrings("ident_0", env.getIdent(ident_indices.items[0]));
    try testing.expectEqualStrings("ident_25", env.getIdent(ident_indices.items[25]));
    try testing.expectEqualStrings("ident_49", env.getIdent(ident_indices.items[49]));

    // Verify some specific strings using the actual indices returned
    try testing.expectEqualStrings("string_literal_0", env.getString(string_indices.items[0]));
    try testing.expectEqualStrings("string_literal_12", env.getString(string_indices.items[12]));
    try testing.expectEqualStrings("string_literal_24", env.getString(string_indices.items[24]));

    // Verify line starts
    try testing.expectEqual(@as(u32, 0), env.line_starts.items.items[0]);
    // Calculate the actual expected value for the second line start
    const first_line = "Line 0: This is a test line with some content\n";
    const expected_second_line_start = first_line.len;
    try testing.expectEqual(@as(u32, expected_second_line_start), env.line_starts.items.items[1]);
}

test "CommonEnv.Serialized roundtrip with special characters" {
    const testing = std.testing;
    const gpa = testing.allocator;

    const source = "Hello\nWorld\nTest\nLine\n";

    // Create original CommonEnv with special characters
    var original = try CommonEnv.init(gpa, source);
    defer original.deinit(gpa);

    // Add identifiers with special characters
    const unicode_idx = try original.insertIdent(gpa, Ident.for_text("café"));
    const emoji_idx = try original.insertIdent(gpa, Ident.for_text("🚀"));
    const special_idx = try original.insertIdent(gpa, Ident.for_text("test_123"));

    // Add strings with special characters and track their indices
    const string1_idx = try original.insertString(gpa, "Hello, 世界!");
    const string2_idx = try original.insertString(gpa, "Test\nwith\nnewlines");
    const string3_idx = try original.insertString(gpa, "Tab\there");

    try original.addExposedById(gpa, unicode_idx);
    try original.addExposedById(gpa, emoji_idx);

    // Add line starts
    try original.calcLineStarts(gpa);

    // Create a CompactWriter
    var writer = CompactWriter.init();
    defer writer.deinit(gpa);

    // Create temp file
    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile("test_special.compact", .{ .read = true });
    defer tmp_file.close();

    // Serialize using the proper Serialized struct pattern
    const serialized = try writer.appendAlloc(gpa, CommonEnv.Serialized);
    try serialized.serialize(&original, gpa, &writer);

    // Write to file
    try writer.writeGather(gpa, tmp_file);

    // Read back with proper alignment
    const file_size = try tmp_file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, CompactWriter.SERIALIZATION_ALIGNMENT, @intCast(file_size));
    defer gpa.free(buffer);
    _ = try tmp_file.pread(buffer, 0);

    // The Serialized struct is at the beginning of the buffer
    const deserialized_ptr = @as(*CommonEnv.Serialized, @ptrCast(@alignCast(buffer.ptr)));
    const env = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))), source);

    // Verify special characters were preserved
    try testing.expectEqualStrings("café", env.getIdent(unicode_idx));
    try testing.expectEqualStrings("🚀", env.getIdent(emoji_idx));
    try testing.expectEqualStrings("test_123", env.getIdent(special_idx));

    try testing.expectEqualStrings("Hello, 世界!", env.getString(string1_idx));
    try testing.expectEqualStrings("Test\nwith\nnewlines", env.getString(string2_idx));
    try testing.expectEqualStrings("Tab\there", env.getString(string3_idx));

    try testing.expectEqual(@as(usize, 2), env.exposed_items.count());
    try testing.expectEqual(@as(usize, 5), env.line_starts.len()); // 4 lines + 1 for first line at offset 0
    try testing.expectEqualStrings(source, env.source);
}
