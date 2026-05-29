//! Stub kept solely to satisfy a stale `@import("cache_module.zig")` in
//! coordinator.zig. The on-disk module cache that originally lived here was
//! removed; the surviving symbols here keep the type structure that the
//! coordinator builds against without backing any real cache behavior.

const std = @import("std");

/// Placeholder for the removed module-cache wrapper. Only its nested
/// `CacheData` type is referenced (as a buffer storage type) by coordinator.zig.
pub const CacheModule = struct {
    /// Owned byte buffer placeholder for a cached module image; unused.
    pub const CacheData = struct {
        bytes: []u8 = &.{},

        /// Free the underlying byte buffer if non-empty.
        pub fn deinit(self: *CacheData, allocator: std.mem.Allocator) void {
            if (self.bytes.len > 0) allocator.free(self.bytes);
        }
    };
};
