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
const exitOnOom = collections.utils.exitOnOom;

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

/// Initialize the module environment.
pub fn init(gpa: std.mem.Allocator) Self {
    // TODO: maybe wire in smarter default based on the initial input text size.

    return Self{
        .gpa = gpa,
        .idents = Ident.Store.initCapacity(gpa, 1024),
        .ident_ids_for_slicing = collections.SafeList(Ident.Idx).initCapacity(gpa, 256),
        .strings = StringLiteral.Store.initCapacityBytes(gpa, 4096),
        .types = types_mod.Store.initCapacity(gpa, 2048, 512),
        .exposed_by_str = collections.SafeStringHashMap(void).init(),
        .exposed_nodes = collections.SafeStringHashMap(u16).init(),
        .line_starts = collections.SafeList(u32).initCapacity(gpa, 256),
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
}

/// Calculate and store line starts from the source text
pub fn calcLineStarts(self: *Self, source: []const u8) !void {
    // Reset line_starts by creating a new SafeList
    self.line_starts.deinit(self.gpa);
    self.line_starts = collections.SafeList(u32).initCapacity(self.gpa, 256);

    // if the source is empty, we're done
    if (source.len == 0) {
        return;
    }

    // the first line starts at offset 0
    _ = self.line_starts.append(self.gpa, 0);

    // find all newlines in the source, save their offset
    var pos: u32 = 0;
    for (source) |c| {
        if (c == '\n') {
            // next line starts after the newline in the current position
            _ = self.line_starts.append(self.gpa, pos + 1);
        }
        pos += 1;
    }
}

/// Get diagnostic position information for a given range
pub fn calcRegionInfo(self: *const Self, source: []const u8, begin: u32, end: u32) !RegionInfo {
    return RegionInfo.position(source, self.line_starts.items.items, begin, end);
}
