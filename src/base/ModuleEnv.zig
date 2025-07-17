//! The common state for a module: any data useful over the full lifetime of its compilation that lives beyond individual IR's.
//!
//! Stores all interned data like idents, strings, and problems.
//!
//! This reduces the size of this module's IRs as they can store references to this
//! interned (and deduplicated) data instead of storing the values themselves.

const std = @import("std");
const types_mod = @import("../types.zig");
const collections = @import("../collections.zig");
const Ident = @import("Ident.zig");
const StringLiteral = @import("StringLiteral.zig");
const RegionInfo = @import("RegionInfo.zig");
const Region = @import("Region.zig");
const relocate_mod = @import("relocate.zig");
const writeAlignedData = @import("write_aligned.zig").writeAlignedData;

const Self = @This();

gpa: std.mem.Allocator,
idents: Ident.Store = .{},
ident_ids_for_slicing: collections.SafeList(Ident.Idx),
strings: StringLiteral.Store,
types: types_mod.Store,
/// Map of exposed items by their string representation (not interned)
/// This is built during canonicalization and preserved for later use
exposed_by_str: collections.SafeStringHashMap(void),
/// Map of exposed item names to their CIR node indices (stored as u16)
/// This is populated during canonicalization to allow cross-module lookups
exposed_nodes: collections.SafeStringHashMap(u16),

/// Line starts for error reporting. We retain only start and offset positions in the IR
/// and then use these line starts to calculate the line number and column number as required.
/// this is a more compact representation at the expense of extra computation only when generating error diagnostics.
line_starts: collections.SafeList(u32),

/// The source code of this module.
source: []const u8,

/// Initialize the module environment.
pub fn init(gpa: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!Self {
    // TODO: maybe wire in smarter default based on the initial input text size.

    return Self{
        .gpa = gpa,
        .idents = try Ident.Store.initCapacity(gpa, 1024),
        .ident_ids_for_slicing = try collections.SafeList(Ident.Idx).initCapacity(gpa, 256),
        .strings = try StringLiteral.Store.initCapacityBytes(gpa, 4096),
        .types = try types_mod.Store.initCapacity(gpa, 2048, 512),
        .exposed_by_str = try collections.SafeStringHashMap(void).initCapacity(gpa, 64),
        .exposed_nodes = try collections.SafeStringHashMap(u16).initCapacity(gpa, 64),
        .line_starts = try collections.SafeList(u32).initCapacity(gpa, 256),
        .source = source,
    };
}

/// Deinitialize the module environment.
pub fn deinit(self: *Self) void {
    self.idents.deinit(self.gpa);
    self.ident_ids_for_slicing.deinit(self.gpa);
    self.strings.deinit(self.gpa);
    self.types.deinit();
    self.line_starts.deinit(self.gpa);
    self.exposed_by_str.deinit(self.gpa);
    self.exposed_nodes.deinit(self.gpa);

    self.gpa.free(self.source);
}

/// Calculate and store line starts from the source text
pub fn calcLineStarts(self: *Self, source: []const u8) !void {
    // Reset line_starts by creating a new SafeList
    self.line_starts.deinit(self.gpa);
    self.line_starts = try collections.SafeList(u32).initCapacity(self.gpa, 256);

    // if the source is empty, we're done
    if (source.len == 0) {
        return;
    }

    // the first line starts at offset 0
    _ = try self.line_starts.append(self.gpa, 0);

    // find all newlines in the source, save their offset
    var pos: u32 = 0;
    for (source) |c| {
        if (c == '\n') {
            // next line starts after the newline in the current position
            _ = try self.line_starts.append(self.gpa, pos + 1);
        }
        pos += 1;
    }
}

/// Get diagnostic position information for a given range
pub fn calcRegionInfo(self: *const Self, source: []const u8, begin: u32, end: u32) !RegionInfo {
    return RegionInfo.position(source, self.line_starts.items.items, begin, end);
}

/// Relocate all pointers in this ModuleEnv by the given offset.
/// This is useful when transferring the ModuleEnv across address spaces.
pub fn relocate(self: *Self, offset: isize) void {
    // Note: gpa is not relocated as it's typically a vtable pointer

    // Relocate idents
    self.idents.relocate(offset);

    // Relocate ident_ids_for_slicing
    self.ident_ids_for_slicing.relocate(offset);

    // Relocate strings
    self.strings.relocate(offset);

    // Relocate types
    self.types.relocate(offset);

    // Relocate exposed_by_str
    self.exposed_by_str.relocate(offset);

    // Relocate exposed_nodes
    self.exposed_nodes.relocate(offset);

    // Relocate line_starts
    self.line_starts.relocate(offset);

    // Relocate source pointer
    if (self.source.len > 0) {
        const old_ptr = @intFromPtr(self.source.ptr);
        const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
        self.source.ptr = @ptrFromInt(new_ptr);
    }
}

/// Calculate the exact size needed to serialize this ModuleEnv
pub fn serializedSize(self: *const Self) usize {
    var size: usize = 0;

    // ModuleEnv struct itself
    size += @sizeOf(Self);

    // Ident store data
    size = std.mem.alignForward(usize, size, @alignOf(@TypeOf(self.idents.interner.bytes.items)));
    size += self.idents.interner.bytes.items.len;

    size = std.mem.alignForward(usize, size, @alignOf(@TypeOf(self.idents.interner.outer_indices.items)));
    size += self.idents.interner.outer_indices.items.len * @sizeOf(@TypeOf(self.idents.interner.outer_indices.items[0]));

    size = std.mem.alignForward(usize, size, @alignOf(@TypeOf(self.idents.interner.regions.items)));
    size += self.idents.interner.regions.items.len * @sizeOf(@TypeOf(self.idents.interner.regions.items[0]));

    // StringIdx.Table hash map data
    if (self.idents.interner.strings.metadata) |_| {
        const metadata_size = self.idents.interner.strings.capacity();
        size = std.mem.alignForward(usize, size, @alignOf(@TypeOf(self.idents.interner.strings.metadata)));
        size += metadata_size;
        const entry_size = metadata_size * @sizeOf(collections.SmallStringInterner.StringIdx) + metadata_size * @sizeOf(void);
        size += entry_size;
    }

    // Ident attributes
    size = std.mem.alignForward(usize, size, @alignOf(@TypeOf(self.idents.attributes.items)));
    size += self.idents.attributes.items.len * @sizeOf(@TypeOf(self.idents.attributes.items[0]));

    // ident_ids_for_slicing
    size = std.mem.alignForward(usize, size, @alignOf(@TypeOf(self.ident_ids_for_slicing.items.items)));
    size += self.ident_ids_for_slicing.items.items.len * @sizeOf(@TypeOf(self.ident_ids_for_slicing.items.items[0]));

    // strings
    size = std.mem.alignForward(usize, size, @alignOf(@TypeOf(self.strings.buffer.items)));
    size += self.strings.buffer.items.len;

    // types store
    size += self.types.serializedSize();

    // exposed_by_str
    size += self.exposed_by_str.serializedSize();

    // exposed_nodes
    size += self.exposed_nodes.serializedSize();

    // line_starts
    size = std.mem.alignForward(usize, size, @alignOf(@TypeOf(self.line_starts.items.items)));
    size += self.line_starts.items.items.len * @sizeOf(@TypeOf(self.line_starts.items.items[0]));

    // source
    size = std.mem.alignForward(usize, size, @alignOf(u8));
    size += self.source.len;

    return size;
}

/// Serialize this ModuleEnv into the provided buffer
/// Buffer must be at least serializedSize() bytes and properly aligned
pub fn serializeInto(self: *const Self, buffer: []u8) !usize {
    var write_offset: usize = 0;

    // Write ModuleEnv struct with placeholder pointers
    const env_ptr = @as(*Self, @ptrCast(@alignCast(buffer.ptr)));
    write_offset += @sizeOf(Self);

    // Serialize idents
    const idents_result = try self.serializeIdentsAt(buffer, &write_offset);

    // Serialize ident_ids_for_slicing
    const ident_ids_offset = if (self.ident_ids_for_slicing.items.items.len > 0) blk: {
        const data = std.mem.sliceAsBytes(self.ident_ids_for_slicing.items.items);
        const offset = writeAlignedData(buffer, &write_offset, data, @alignOf(@TypeOf(self.ident_ids_for_slicing.items.items[0])));
        break :blk offset;
    } else 0;

    // Serialize strings
    const strings_buffer_offset = if (self.strings.buffer.items.len > 0) blk: {
        const offset = writeAlignedData(buffer, &write_offset, self.strings.buffer.items, @alignOf(u8));
        break :blk offset;
    } else 0;

    // Serialize types
    const types_bytes_written = try self.types.serializeInto(buffer[write_offset..]);
    write_offset += types_bytes_written;

    // Serialize exposed_by_str
    _ = try self.exposed_by_str.serializeInto(buffer[write_offset..]);
    write_offset += self.exposed_by_str.serializedSize();

    // Serialize exposed_nodes
    _ = try self.exposed_nodes.serializeInto(buffer[write_offset..]);
    write_offset += self.exposed_nodes.serializedSize();

    // Serialize line_starts
    const line_starts_offset = if (self.line_starts.items.items.len > 0) blk: {
        const data = std.mem.sliceAsBytes(self.line_starts.items.items);
        const offset = writeAlignedData(buffer, &write_offset, data, @alignOf(@TypeOf(self.line_starts.items.items[0])));
        break :blk offset;
    } else 0;

    // Serialize source
    const source_offset = if (self.source.len > 0) blk: {
        const offset = writeAlignedData(buffer, &write_offset, self.source, @alignOf(u8));
        break :blk offset;
    } else 0;

    // Now set up the ModuleEnv struct with file offsets as pointers
    env_ptr.* = Self{
        .gpa = std.mem.Allocator{
            .ptr = @ptrFromInt(1),
            .vtable = @ptrFromInt(1),
        }, // Will be set by deserializer
        .idents = .{
            .interner = .{
                .bytes = .{ .items = if (self.idents.interner.bytes.items.len > 0) @as([*]u8, @ptrFromInt(idents_result.bytes_offset))[0..self.idents.interner.bytes.items.len] else @as([*]u8, @ptrFromInt(@alignOf(u8)))[0..0], .capacity = self.idents.interner.bytes.capacity },
                .strings = .{ .metadata = if (idents_result.strings_metadata_offset > 0) @ptrFromInt(idents_result.strings_metadata_offset) else null, .size = self.idents.interner.strings.size, .available = self.idents.interner.strings.available, .header = self.idents.interner.strings.header },
                .outer_indices = .{ .items = if (self.idents.interner.outer_indices.items.len > 0) @as([*]collections.SmallStringInterner.StringIdx, @ptrFromInt(idents_result.indices_offset))[0..self.idents.interner.outer_indices.items.len] else @as([*]collections.SmallStringInterner.StringIdx, @ptrFromInt(@alignOf(collections.SmallStringInterner.StringIdx)))[0..0], .capacity = self.idents.interner.outer_indices.capacity },
                .regions = .{ .items = if (self.idents.interner.regions.items.len > 0) @as([*]Region, @ptrFromInt(idents_result.regions_offset))[0..self.idents.interner.regions.items.len] else @as([*]Region, @ptrFromInt(@alignOf(Region)))[0..0], .capacity = self.idents.interner.regions.capacity },
            },
            .attributes = .{ .items = if (self.idents.attributes.items.len > 0) @as([*]Ident.Attributes, @ptrFromInt(idents_result.attributes_offset))[0..self.idents.attributes.items.len] else @as([*]Ident.Attributes, @ptrFromInt(@alignOf(Ident.Attributes)))[0..0], .capacity = self.idents.attributes.capacity },
            .next_unique_name = self.idents.next_unique_name,
        },
        .ident_ids_for_slicing = .{ .items = .{ .items = if (self.ident_ids_for_slicing.items.items.len > 0) @as([*]Ident.Idx, @ptrFromInt(ident_ids_offset))[0..self.ident_ids_for_slicing.items.items.len] else @as([*]Ident.Idx, @ptrFromInt(@alignOf(Ident.Idx)))[0..0], .capacity = self.ident_ids_for_slicing.items.capacity } },
        .strings = .{ .buffer = .{ .items = if (self.strings.buffer.items.len > 0) @as([*]u8, @ptrFromInt(strings_buffer_offset))[0..self.strings.buffer.items.len] else @as([*]u8, @ptrFromInt(@alignOf(u8)))[0..0], .capacity = self.strings.buffer.capacity } },
        .types = .{
            .slots = .{ .backing = &.{}, .len = 0 },
            .gpa = std.mem.Allocator{ .ptr = @ptrFromInt(1), .vtable = @ptrFromInt(1) },
        }, // Complex structure, will be set up by types.serializeInto
        .exposed_by_str = .{
            .metadata = null,
            .size = 0,
            .available = 0,
            .header = .{ .values = .{ .metadata = .{ .count = 0, .capacity = 0, .fingerprint = 0 } } },
        }, // Will be set up by hash map serialization
        .exposed_nodes = .{
            .metadata = null,
            .size = 0,
            .available = 0,
            .header = .{ .values = .{ .metadata = .{ .count = 0, .capacity = 0, .fingerprint = 0 } } },
        }, // Will be set up by hash map serialization
        .line_starts = .{ .items = .{ .items = if (self.line_starts.items.items.len > 0) @as([*]u32, @ptrFromInt(line_starts_offset))[0..self.line_starts.items.items.len] else @as([*]u32, @ptrFromInt(@alignOf(u32)))[0..0], .capacity = self.line_starts.items.capacity } },
        .source = if (self.source.len > 0) @as([*]const u8, @ptrFromInt(source_offset))[0..self.source.len] else @as([*]const u8, @ptrFromInt(1))[0..0],
    };

    return write_offset;
}

const IdentsSerializationResult = struct {
    bytes_offset: usize,
    indices_offset: usize,
    regions_offset: usize,
    strings_metadata_offset: usize,
    attributes_offset: usize,
};

fn serializeIdentsAt(self: *const Self, buffer: []u8, write_offset: *usize) !IdentsSerializationResult {
    var result = IdentsSerializationResult{
        .bytes_offset = 0,
        .indices_offset = 0,
        .regions_offset = 0,
        .strings_metadata_offset = 0,
        .attributes_offset = 0,
    };

    // Serialize interner bytes
    if (self.idents.interner.bytes.items.len > 0) {
        result.bytes_offset = writeAlignedData(buffer, write_offset, self.idents.interner.bytes.items, @alignOf(u8));
    }

    // Serialize interner outer_indices
    if (self.idents.interner.outer_indices.items.len > 0) {
        const data = std.mem.sliceAsBytes(self.idents.interner.outer_indices.items);
        result.indices_offset = writeAlignedData(buffer, write_offset, data, @alignOf(@TypeOf(self.idents.interner.outer_indices.items[0])));
    }

    // Serialize interner regions
    if (self.idents.interner.regions.items.len > 0) {
        const data = std.mem.sliceAsBytes(self.idents.interner.regions.items);
        result.regions_offset = writeAlignedData(buffer, write_offset, data, @alignOf(@TypeOf(self.idents.interner.regions.items[0])));
    }

    // Serialize StringIdx.Table
    if (self.idents.interner.strings.metadata) |metadata| {
        write_offset.* = std.mem.alignForward(usize, write_offset.*, @alignOf(@TypeOf(metadata)));
        result.strings_metadata_offset = write_offset.*;

        const capacity = self.idents.interner.strings.capacity();
        const metadata_size = capacity;

        // Write metadata bytes
        @memcpy(buffer[write_offset.*..][0..metadata_size], @as([*]const u8, @ptrCast(metadata))[0..metadata_size]);
        write_offset.* += metadata_size;

        // Write the keys and values data that follows metadata
        const entry_size = capacity * @sizeOf(collections.SmallStringInterner.StringIdx) + capacity * @sizeOf(void);
        const data_ptr = @as([*]const u8, @ptrCast(metadata)) + metadata_size;
        @memcpy(buffer[write_offset.*..][0..entry_size], data_ptr[0..entry_size]);
        write_offset.* += entry_size;
    }

    // Serialize attributes
    if (self.idents.attributes.items.len > 0) {
        const data = std.mem.sliceAsBytes(self.idents.attributes.items);
        result.attributes_offset = writeAlignedData(buffer, write_offset, data, @alignOf(@TypeOf(self.idents.attributes.items[0])));
    }

    return result;
}

test "serialization is deterministic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create a minimal test ModuleEnv
    var env = try Self.init(allocator, "test");
    defer env.deinit();

    // Serialize twice to different buffers
    const size = env.serializedSize();
    const buffer1 = try allocator.alloc(u8, size);
    defer allocator.free(buffer1);
    const buffer2 = try allocator.alloc(u8, size);
    defer allocator.free(buffer2);

    const written1 = try env.serializeInto(buffer1);
    const written2 = try env.serializeInto(buffer2);

    // Both should write the same amount
    try testing.expectEqual(written1, written2);

    // Both buffers should be byte-for-byte identical
    try testing.expectEqualSlices(u8, buffer1[0..written1], buffer2[0..written2]);
}
