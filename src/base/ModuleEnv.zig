//! The common state for a module: any data useful over the full lifetime of its compilation that lives beyond individual IR's.
//!
//! Stores all interned data like idents, strings, and problems.
//!
//! This reduces the size of this module's IRs as they can store references to this
//! interned (and deduplicated) data instead of storing the values themselves.

const std = @import("std");
const types_mod = @import("types");
const collections = @import("collections");
const serialization = @import("serialization");
const Ident = @import("Ident.zig");
const StringLiteral = @import("StringLiteral.zig");
const RegionInfo = @import("RegionInfo.zig");

const Self = @This();

gpa: std.mem.Allocator,
idents: Ident.Store,
ident_ids_for_slicing: collections.SafeList(Ident.Idx),
strings: StringLiteral.Store,
types: types_mod.Store,
/// Tracks exposed items by their names and associated CIR node indices
/// Uses a sorted array for efficient serialization and relocation
exposed_items: collections.ExposedItems,

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
        .exposed_items = collections.ExposedItems.init(),
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
    self.exposed_items.deinit(self.gpa);
}

/// Calculate and store line starts from the source text
pub fn calcLineStarts(self: *Self) !void {
    // Reset line_starts by creating a new SafeList
    self.line_starts.deinit(self.gpa);
    self.line_starts = try collections.SafeList(u32).initCapacity(self.gpa, 256);

    // if the source is empty, we're done
    if (self.source.len == 0) {
        return;
    }

    // the first line starts at offset 0
    _ = try self.line_starts.append(self.gpa, 0);

    // find all newlines in the source, save their offset
    var pos: u32 = 0;
    for (self.source) |c| {
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

/// Freeze all interners in this module environment, preventing any new entries from being added.
/// This should be called after canonicalization is complete, so that
/// we know it's safe to serialize/deserialize the part of the interner
/// that goes from ident to string, because we don't go from string to ident
/// (or add new entries) in any of the later stages of compilation.
pub fn freezeInterners(self: *Self) void {
    self.idents.freeze();
    self.strings.freeze();
    // Sort and deduplicate exposed items at the end of canonicalization
    self.exposed_items.ensureSorted(self.gpa);
}

/// Append this ModuleEnv to an iovec writer for serialization
pub fn appendToIovecs(self: *const Self, writer: *serialization.IovecWriter) !usize {
    const iovec_serialize = serialization.iovec_serialize;
    
    // Create a mutable copy of self that we can modify
    var env_copy = self.*;
    
    // Create a buffer for the final serialized struct
    const env_copy_buffer = try writer.allocator.alloc(u8, @sizeOf(Self));
    
    // Track this allocation so it gets freed when writer is deinitialized
    try writer.owned_buffers.append(env_copy_buffer);
    
    // Serialize complex structures - they handle their own pointers using duplicate-and-mutate
    // We don't need to update any pointers for these as they manage themselves
    _ = try self.idents.appendToIovecs(writer);
    _ = try self.strings.appendToIovecs(writer);
    _ = try self.types.appendToIovecs(writer);
    _ = try self.exposed_items.appendToIovecs(writer);
    
    // Serialize simple array data and update pointers in our copy
    const ident_ids_offset = if (self.ident_ids_for_slicing.items.items.len > 0) blk: {
        const bytes = std.mem.sliceAsBytes(self.ident_ids_for_slicing.items.items);
        // Use Ident.Idx as the type to ensure proper alignment
        const offset = try writer.appendBytes(Ident.Idx, bytes);
        break :blk offset;
    } else 0;
    
    const line_starts_offset = if (self.line_starts.items.items.len > 0) blk: {
        const bytes = std.mem.sliceAsBytes(self.line_starts.items.items);
        // Use u32 as the type to ensure proper alignment
        const offset = try writer.appendBytes(u32, bytes);
        break :blk offset;
    } else 0;
    
    const source_offset = if (self.source.len > 0) blk: {
        const offset = try writer.appendBytes(u8, self.source);
        break :blk offset;
    } else 0;
    
    // Update pointers in our copy
    env_copy.ident_ids_for_slicing.items.items.ptr = if (ident_ids_offset == 0)
        @ptrFromInt(iovec_serialize.EMPTY_ARRAY_SENTINEL)
    else
        @ptrFromInt(ident_ids_offset);
    env_copy.ident_ids_for_slicing.items.items.len = self.ident_ids_for_slicing.items.items.len;
    
    env_copy.line_starts.items.items.ptr = if (line_starts_offset == 0)
        @ptrFromInt(iovec_serialize.EMPTY_ARRAY_SENTINEL)
    else
        @ptrFromInt(line_starts_offset);
    env_copy.line_starts.items.items.len = self.line_starts.items.items.len;
    
    env_copy.source.ptr = if (source_offset == 0)
        @ptrFromInt(iovec_serialize.EMPTY_ARRAY_SENTINEL)
    else
        @ptrFromInt(source_offset);
    env_copy.source.len = self.source.len;
    
    // Copy the modified struct to the buffer
    @memcpy(env_copy_buffer, std.mem.asBytes(&env_copy));
    
    // NOW add the copy to iovecs after all pointers have been converted to offsets
    const struct_offset = try writer.appendBytes(Self, env_copy_buffer);
    
    return struct_offset;
}

/// Relocate all pointers in this ModuleEnv by the given offset.
/// This is useful when transferring the ModuleEnv across address spaces.
pub fn relocate(self: *Self, offset: isize) void {
    // Note: gpa is not relocated as it's typically a vtable pointer

    // Relocate ident_ids_for_slicing
    self.ident_ids_for_slicing.relocate(offset);
    
    // Relocate other fields
    self.idents.relocate(offset);
    self.strings.relocate(offset);
    self.types.relocate(offset);
    self.exposed_items.relocate(offset);
    self.line_starts.relocate(offset);
    
    // Relocate source string
    if (self.source.len > 0) {
        const old_ptr = @intFromPtr(self.source.ptr);
        // Skip relocation if this is a sentinel value
        if (old_ptr != serialization.iovec_serialize.EMPTY_ARRAY_SENTINEL) {
            const new_ptr = @as(usize, @intCast(@as(isize, @intCast(old_ptr)) + offset));
            self.source.ptr = @ptrFromInt(new_ptr);
        }
    }
}
