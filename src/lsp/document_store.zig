const std = @import("std");

/// Stores the latest contents of each open text document.
pub const DocumentStore = struct {
    allocator: std.mem.Allocator,
    entries: std.StringHashMap(Document),

    /// Snapshot of a document's contents and version.
    pub const Document = struct {
        text: []u8,
        version: i64,
    };

    /// Creates an empty store backed by the provided allocator.
    pub fn init(allocator: std.mem.Allocator) DocumentStore {
        return .{ .allocator = allocator, .entries = std.StringHashMap(Document).init(allocator) };
    }

    /// Releases all tracked documents and frees associated memory.
    pub fn deinit(self: *DocumentStore) void {
        var it = self.entries.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.text);
        }
        self.entries.deinit();
        self.* = undefined;
    }

    /// Inserts or replaces the document at `uri` with the given text and version.
    pub fn upsert(self: *DocumentStore, uri: []const u8, version: i64, text: []const u8) !void {
        const gop = try self.entries.getOrPut(uri);
        if (!gop.found_existing) {
            gop.key_ptr.* = try self.allocator.dupe(u8, uri);
        } else {
            self.allocator.free(gop.value_ptr.text);
        }

        gop.value_ptr.* = .{
            .text = try self.allocator.dupe(u8, text),
            .version = version,
        };
    }

    /// Removes a document from the store, if present.
    pub fn remove(self: *DocumentStore, uri: []const u8) void {
        if (self.entries.fetchRemove(uri)) |removed| {
            self.allocator.free(removed.key);
            self.allocator.free(removed.value.text);
        }
    }

    /// Returns the stored document (if any). The returned slice references memory owned by the store.
    pub fn get(self: *DocumentStore, uri: []const u8) ?Document {
        if (self.entries.get(uri)) |doc| {
            return doc;
        }
        return null;
    }
};
