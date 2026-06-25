//! Common test utilities for bundle tests

const std = @import("std");
const Allocator = std.mem.Allocator;

/// Iterator for file paths used in tests
pub const FilePathIterator = struct {
    paths: []const []const u8,
    index: usize = 0,

    pub fn next(self: *FilePathIterator) Allocator.Error!?[]const u8 {
        if (self.index >= self.paths.len) return null;
        const path = self.paths[self.index];
        self.index += 1;
        return path;
    }
};
