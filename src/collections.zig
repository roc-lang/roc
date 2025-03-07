/// Utilities for managing collections.
pub const utils = @import("collections/utils.zig");

/// re-export the SafeList
pub const SafeList = @import("collections/safe_list.zig").SafeList;
/// re-export the SafeMultiList
pub const SafeMultiList = @import("collections/safe_list.zig").SafeMultiList;
/// re-export the SmallStringInterner
pub const SmallStringInterner = @import("collections/SmallStringInterner.zig");
