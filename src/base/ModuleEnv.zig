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

/// Line starts for error reporting. We retain only start and offset positions in the IR
/// and then use these line starts to calculate the line number and column number as required.
/// this is a more compact representation at the expense of extra computation only when generating error diagnostics.
line_starts: std.ArrayList(u32),

/// Initialize the module environment.
pub fn init(gpa: std.mem.Allocator) Self {
    // TODO: maybe wire in smarter default based on the initial input text size.

    return Self{
        .gpa = gpa,
        .idents = Ident.Store.initCapacity(gpa, 1024),
        .ident_ids_for_slicing = collections.SafeList(Ident.Idx).initCapacity(gpa, 256),
        .strings = StringLiteral.Store.initCapacityBytes(gpa, 4096),
        .types = types_mod.Store.initCapacity(gpa, 2048, 512),
        .line_starts = std.ArrayList(u32).init(gpa),
    };
}

/// Deinitialize the module environment.
pub fn deinit(self: *Self) void {
    self.idents.deinit(self.gpa);
    self.ident_ids_for_slicing.deinit(self.gpa);
    self.strings.deinit(self.gpa);
    self.types.deinit();
    self.line_starts.deinit();
}

/// Calculate and store line starts from the source text
pub fn calcLineStarts(self: *Self, source: []const u8) !void {
    self.line_starts.clearRetainingCapacity();
    self.line_starts = try RegionInfo.findLineStarts(self.gpa, source);
}

/// Get diagnostic position information for a given range
pub fn calcRegionInfo(self: *Self, source: []const u8, begin: u32, end: u32) !RegionInfo {
    return RegionInfo.position(source, self.line_starts.items, begin, end);
}
