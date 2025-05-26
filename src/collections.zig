/// Utilities for managing collections.
pub const utils = @import("collections/utils.zig");

const std = @import("std");

/// The highest alignment any Roc type can have.
/// This is used as the base alignment for the allocation used
/// in the interpreter for stack allocations.
pub const max_roc_alignment: std.mem.Alignment = .@"16";

pub const SafeList = @import("collections/safe_list.zig").SafeList;

pub const SafeMultiList = @import("collections/safe_list.zig").SafeMultiList;

pub const SmallStringInterner = @import("collections/SmallStringInterner.zig");

/// A range that is guaranteed to have at least one element
pub const NonEmptyRange = struct {
    start: u32,
    /// count must be >= 1
    count: u32,

    pub fn init(start: u32, count: u32) !NonEmptyRange {
        if (count == 0) {
            return error.EmptyRange;
        }
        return NonEmptyRange{ .start = start, .count = count };
    }

    /// Convert to a regular range (for compatibility with existing code)
    pub fn toRange(self: NonEmptyRange) Range {
        return Range{ .start = self.start, .count = self.count };
    }
};

/// A regular range (re-exported for convenience)
pub const Range = struct {
    start: u32,
    count: u32,
};
