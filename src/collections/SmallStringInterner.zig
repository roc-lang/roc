//! An interner for short and likely repeated strings in in a Roc file.
//!
//! This interner deduplicates its string values because they are
//! expected to be small and often repeated since they tend to represent
//! the same value being referenced in many places. The indices assigned
//! to each interned string are serial, meaning they can be used for
//! arrays with values corresponding 1-to-1 to interned values, e.g. regions.

const std = @import("std");
const utils = @import("./utils.zig");
const Region = @import("../base/Region.zig");

const exitOnOom = utils.exitOnOom;

const Self = @This();

/// The raw underlying bytes for all strings.
/// Since strings are small, they are simply null terminated.
/// This uses only 1 byte to encode the size and is cheap to scan.
bytes: std.ArrayListUnmanaged(u8) = .{},
/// A deduplicated set of strings indices referencing into bytes.
/// The key is the offset into bytes.
strings: StringIdx.Table = .{},
/// A unique ID for every string. This is fundamentally an index into bytes.
/// It also maps 1:1 with a region at the same index.
outer_indices: std.ArrayListUnmanaged(StringIdx) = .{},
regions: std.ArrayListUnmanaged(Region) = .{},

/// A unique index for a deduped string in this interner.
pub const Idx = enum(u32) { _ };

/// Initialize a `SmallStringInterner` with the specified capacity.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) Self {
    // TODO: tune this. Rough assumption that average small string is 4 bytes.
    const bytes_per_string = 4;

    var self = Self{
        .bytes = std.ArrayListUnmanaged(u8).initCapacity(gpa, capacity * bytes_per_string) catch |err| exitOnOom(err),
        .strings = .{},
        .outer_indices = std.ArrayListUnmanaged(StringIdx).initCapacity(gpa, capacity) catch |err| exitOnOom(err),
        .regions = std.ArrayListUnmanaged(Region).initCapacity(gpa, capacity) catch |err| exitOnOom(err),
    };
    self.strings.ensureTotalCapacityContext(gpa, @intCast(capacity), StringIdx.TableContext{ .bytes = &self.bytes }) catch |err| exitOnOom(err);
    return self;
}

/// Free all memory consumed by this interner.
/// Will invalidate all slices referencing the interner.
pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
    self.bytes.deinit(gpa);
    self.strings.deinit(gpa);
    self.outer_indices.deinit(gpa);
    self.regions.deinit(gpa);
}

/// Add a string to this interner, returning a unique, serial index.
pub fn insert(self: *Self, gpa: std.mem.Allocator, string: []const u8, region: Region) Idx {
    const entry = self.strings.getOrPutContextAdapted(
        gpa,
        string,
        StringIdx.TableAdapter{ .bytes = &self.bytes },
        StringIdx.TableContext{ .bytes = &self.bytes },
    ) catch |err| exitOnOom(err);
    if (entry.found_existing) return self.addOuterIdForStringIndex(gpa, entry.key_ptr.*, region);

    self.bytes.ensureUnusedCapacity(gpa, string.len + 1) catch |err| exitOnOom(err);
    const string_offset: StringIdx = @enumFromInt(self.bytes.items.len);

    self.bytes.appendSliceAssumeCapacity(string);
    self.bytes.appendAssumeCapacity(0);

    entry.key_ptr.* = string_offset;
    return self.addOuterIdForStringIndex(gpa, string_offset, region);
}

fn addOuterIdForStringIndex(self: *Self, gpa: std.mem.Allocator, string_offset: StringIdx, region: Region) Idx {
    const len: Idx = @enumFromInt(@as(u32, @truncate(self.outer_indices.items.len)));
    self.outer_indices.append(gpa, string_offset) catch |err| exitOnOom(err);
    self.regions.append(gpa, region) catch |err| exitOnOom(err);

    return len;
}

/// Check if two indices have the same text in constant time.
pub fn indicesHaveSameText(
    self: *const Self,
    first_idx: Idx,
    second_idx: Idx,
) bool {
    const first_string_offset = self.outer_indices.items[@as(usize, @intFromEnum(first_idx))];
    const second_string_offset = self.outer_indices.items[@as(usize, @intFromEnum(second_idx))];
    return first_string_offset == second_string_offset;
}

/// Get a reference to the text for an interned string.
pub fn getText(self: *const Self, idx: Idx) []u8 {
    const string_offset = self.outer_indices.items[@as(usize, @intFromEnum(idx))];

    return std.mem.sliceTo(self.bytes.items[@intFromEnum(string_offset)..], 0);
}

/// Get the region for an interned string.
pub fn getRegion(self: *const Self, idx: Idx) Region {
    return self.regions.items[@as(usize, @intFromEnum(idx))];
}

const StringIdx = enum(u32) {
    _,

    // This uses an unmanaged hash map due to context management requirements.
    // It enables us to ensure that an update context is always used with the newest pointer to the underlying bytes allocation.
    const Table = std.HashMapUnmanaged(StringIdx, void, TableContext, std.hash_map.default_max_load_percentage);

    /// These are copied straight out of the zig standard library.
    /// They are simply modified to give us control over the hash function and bytes allocation.
    const TableContext = struct {
        bytes: *const std.ArrayListUnmanaged(u8),

        pub fn eql(_: @This(), a: StringIdx, b: StringIdx) bool {
            return a == b;
        }

        pub fn hash(ctx: @This(), key: StringIdx) u64 {
            return std.hash_map.hashString(std.mem.sliceTo(ctx.bytes.items[@intFromEnum(key)..], 0));
        }
    };

    const TableAdapter = struct {
        bytes: *const std.ArrayListUnmanaged(u8),

        pub fn eql(ctx: @This(), a: []const u8, b: StringIdx) bool {
            return std.mem.eql(u8, a, std.mem.sliceTo(ctx.bytes.items[@intFromEnum(b)..], 0));
        }

        pub fn hash(_: @This(), adapted_key: []const u8) u64 {
            std.debug.assert(std.mem.indexOfScalar(u8, adapted_key, 0) == null);
            return std.hash_map.hashString(adapted_key);
        }
    };
};
