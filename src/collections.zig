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
