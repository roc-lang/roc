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

const Self = @This();

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

pub fn init(gpa: std.mem.Allocator, source: []const u8) std.mem.Allocator.Error!Self {
    return Self{
        .idents = try Ident.Store.initCapacity(gpa, 1024),
        .strings = try StringLiteral.Store.initCapacityBytes(gpa, 4096),
        .exposed_items = ExposedItems.init(),
        .line_starts = try SafeList(u32).initCapacity(gpa, 256),
        .source = source,
    };
}

pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
    self.idents.deinit(gpa);
    self.strings.deinit(gpa);
    self.exposed_items.deinit(gpa);
    self.line_starts.deinit(gpa);
}

/// Add the given offset to the memory addresses of all pointers in `self`.
pub fn relocate(self: *Self, offset: isize) void {
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
    self: *const Self,
    allocator: std.mem.Allocator,
    writer: *CompactWriter,
) std.mem.Allocator.Error!*const Self {
    // First, write the CommonEnv struct itself
    const offset_self = try writer.appendAlloc(allocator, Self);

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

pub fn freezeInterners(self: *Self) void {
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
        env: *const Self,
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

    /// Deserialize a ModuleEnv from the buffer, updating the ModuleEnv in place
    pub fn deserialize(
        self: *Serialized,
        offset: i64,
        source: []const u8,
    ) *Self {
        // ModuleEnv.Serialized should be at least as big as ModuleEnv
        std.debug.assert(@sizeOf(Serialized) >= @sizeOf(Self));

        // Overwrite ourself with the deserialized version, and return our pointer after casting it to Self.
        const env = @as(*Self, @ptrFromInt(@intFromPtr(self)));

        env.* = Self{
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

pub fn insertIdent(self: *Self, gpa: std.mem.Allocator, ident: Ident) std.mem.Allocator.Error!Ident.Idx {
    return try self.idents.insert(gpa, ident);
}

pub fn findIdent(self: *const Self, text: []const u8) ?Ident.Idx {
    return self.idents.findByString(text);
}

pub fn getIdent(self: *const Self, idx: Ident.Idx) []const u8 {
    return self.getIdent(idx);
}

pub fn getIdentStore(self: *const Self) *const Ident.Store {
    return &self.idents;
}

pub fn insertString(self: *Self, gpa: std.mem.Allocator, string: []const u8) std.mem.Allocator.Error!StringLiteral.Idx {
    return try self.strings.insert(gpa, string);
}

pub fn getString(self: *const Self, idx: StringLiteral.Idx) []const u8 {
    return self.strings.get(idx);
}

pub fn getStringStore(self: *Self) *StringLiteral.Store {
    return &self.strings;
}

pub fn addExposedById(self: *Self, gpa: std.mem.Allocator, ident_idx: Ident.Idx) !void {
    return try self.exposed_items.addExposedById(gpa, @bitCast(ident_idx));
}

pub fn getNodeIndexById(self: *const Self, allocator: std.mem.Allocator, ident_idx: Ident.Idx) ?u16 {
    return self.exposed_items.getNodeIndexById(allocator, @bitCast(ident_idx));
}

pub fn setNodeIndexById(self: *Self, gpa: std.mem.Allocator, ident_idx: Ident.Idx, node_idx: u16) !void {
    return try self.exposed_items.setNodeIndexById(gpa, @bitCast(ident_idx), node_idx);
}

/// Get region info for a given region
pub fn getRegionInfo(self: *const Self, region: Region) !RegionInfo {
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
pub fn calcRegionInfo(self: *const Self, region: Region) RegionInfo {
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

pub fn getSourceAll(self: *const Self) []const u8 {
    return self.source;
}

/// Calculate and store line starts from the source text
pub fn calcLineStarts(self: *Self, gpa: std.mem.Allocator) !void {
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

pub fn getLineStartsAll(self: *const Self) []const u32 {
    return self.line_starts.items.items;
}

/// Get the source text for a given region
pub fn getSource(self: *const Self, region: Region) []const u8 {
    return self.source[region.start.offset..region.end.offset];
}

/// Get the source line for a given region
pub fn getSourceLine(self: *const Self, region: Region) ![]const u8 {
    const region_info = try self.getRegionInfo(region);
    const line_start = self.line_starts.items.items[region_info.start_line_idx];
    const line_end = if (region_info.start_line_idx + 1 < self.line_starts.items.items.len)
        self.line_starts.items.items[region_info.start_line_idx + 1]
    else
        self.source.len;

    return self.source[line_start..line_end];
}
