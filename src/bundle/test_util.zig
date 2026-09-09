//! Common test utilities for bundle tests

const std = @import("std");
const Allocator = std.mem.Allocator;
const bundle = @import("bundle.zig");

/// Iterator for file paths used in tests
pub const FilePathIterator = struct {
    paths: []const []const u8,
    index: usize = 0,

    pub fn next(self: *FilePathIterator) Allocator.Error!?bundle.Entry {
        if (self.index >= self.paths.len) return null;
        const path = self.paths[self.index];
        self.index += 1;
        return .{ .source_path = path, .archive_path = path };
    }
};

/// Iterator for explicit source and archive path pairs used in tests.
pub const EntryIterator = struct {
    entries: []const bundle.Entry,
    index: usize = 0,

    pub fn next(self: *EntryIterator) Allocator.Error!?bundle.Entry {
        if (self.index >= self.entries.len) return null;
        const entry = self.entries[self.index];
        self.index += 1;
        return entry;
    }
};
