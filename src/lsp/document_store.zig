//! Document storage for tracking open text documents in the LSP server.

const std = @import("std");
const LineIndex = @import("line_index.zig").LineIndex;
const Allocator = std.mem.Allocator;

/// Stores the latest contents of each open text document.
pub const DocumentStore = struct {
    allocator: std.mem.Allocator,
    entries: std.StringHashMap(Document),

    /// Snapshot of a document's contents and version.
    pub const Document = struct {
        text: []u8,
        line_starts: []u32,
        version: i64,

        pub fn lineIndex(self: *const Document) LineIndex {
            return .{ .source = self.text, .line_starts = self.line_starts };
        }
    };

    pub const Range = struct {
        start_line: usize,
        start_character: usize,
        end_line: usize,
        end_character: usize,
    };

    /// A text change with an optional range (UTF-16 positions, inclusive-exclusive).
    pub const ContentChange = struct {
        text: []const u8,
        range: ?Range = null,
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
            self.allocator.free(entry.value_ptr.line_starts);
            self.allocator.free(entry.value_ptr.text);
        }
        self.entries.deinit();
        self.* = undefined;
    }

    /// Inserts or replaces the document at `uri` with the given text and version.
    pub fn upsert(self: *DocumentStore, uri: []const u8, version: i64, text: []const u8) Allocator.Error!void {
        const new_text = try self.allocator.dupe(u8, text);
        errdefer self.allocator.free(new_text);
        const new_line_starts = try buildLineStarts(self.allocator, new_text);
        errdefer self.allocator.free(new_line_starts);

        if (self.entries.getPtr(uri)) |entry| {
            self.allocator.free(entry.line_starts);
            self.allocator.free(entry.text);
            entry.* = .{
                .text = new_text,
                .line_starts = new_line_starts,
                .version = version,
            };
            return;
        }

        const owned_uri = try self.allocator.dupe(u8, uri);
        errdefer self.allocator.free(owned_uri);
        const gop = try self.entries.getOrPut(uri);
        if (gop.found_existing) {
            // DocumentStore is not concurrent, but keep this defensive branch
            // atomic if that changes in the future.
            self.allocator.free(gop.value_ptr.line_starts);
            self.allocator.free(gop.value_ptr.text);
            self.allocator.free(owned_uri);
        } else {
            gop.key_ptr.* = owned_uri;
        }

        gop.value_ptr.* = .{
            .text = new_text,
            .line_starts = new_line_starts,
            .version = version,
        };
    }

    /// Removes a document from the store, if present.
    pub fn remove(self: *DocumentStore, uri: []const u8) void {
        if (self.entries.fetchRemove(uri)) |removed| {
            self.allocator.free(removed.key);
            self.allocator.free(removed.value.line_starts);
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

    /// Applies a range replacement to an existing document using UTF-16 positions.
    pub fn applyRangeReplacement(self: *DocumentStore, uri: []const u8, version: i64, range: Range, new_text: []const u8) (Allocator.Error || error{ NoChanges, DocumentNotFound, InvalidRange, InvalidPosition })!void {
        const change = ContentChange{ .text = new_text, .range = range };
        try self.applyContentChanges(uri, version, &.{change});
    }

    /// Applies one or more content changes in order, mirroring LSP incremental edits.
    pub fn applyContentChanges(self: *DocumentStore, uri: []const u8, version: i64, changes: []const ContentChange) (Allocator.Error || error{ NoChanges, DocumentNotFound, InvalidRange, InvalidPosition })!void {
        if (changes.len == 0) return error.NoChanges;

        const entry = self.entries.getPtr(uri) orelse return error.DocumentNotFound;

        var current = try self.allocator.dupe(u8, entry.text);
        var current_starts = self.allocator.dupe(u32, entry.line_starts) catch |err| {
            self.allocator.free(current);
            return err;
        };
        var current_owned = true;
        defer if (current_owned) {
            self.allocator.free(current);
            self.allocator.free(current_starts);
        };

        for (changes) |change| {
            const index = LineIndex{ .source = current, .line_starts = current_starts };
            const updated = try self.applyChangeToText(&index, change);
            const updated_starts = buildLineStarts(self.allocator, updated) catch |err| {
                self.allocator.free(updated);
                return err;
            };
            self.allocator.free(current);
            self.allocator.free(current_starts);
            current = updated;
            current_starts = updated_starts;
        }

        self.allocator.free(entry.line_starts);
        self.allocator.free(entry.text);
        entry.text = current;
        entry.line_starts = current_starts;
        entry.version = version;
        current_owned = false;
    }

    fn applyChangeToText(self: *DocumentStore, index: *const LineIndex, change: ContentChange) (Allocator.Error || error{ InvalidRange, InvalidPosition })![]u8 {
        if (change.range) |range| {
            return replaceRange(self.allocator, index, range, change.text);
        } else {
            return self.allocator.dupe(u8, change.text);
        }
    }

    fn replaceRange(allocator: std.mem.Allocator, index: *const LineIndex, range: Range, new_text: []const u8) (Allocator.Error || error{ InvalidRange, InvalidPosition })![]u8 {
        const start_line = std.math.cast(u32, range.start_line) orelse return error.InvalidPosition;
        const start_character = std.math.cast(u32, range.start_character) orelse return error.InvalidPosition;
        const end_line = std.math.cast(u32, range.end_line) orelse return error.InvalidPosition;
        const end_character = std.math.cast(u32, range.end_character) orelse return error.InvalidPosition;
        const start_offset: usize = @intCast(index.utf16ToByte(start_line, start_character, .exact) orelse return error.InvalidPosition);
        const end_offset: usize = @intCast(index.utf16ToByte(end_line, end_character, .exact) orelse return error.InvalidPosition);
        const text = index.source;
        if (start_offset > end_offset or end_offset > text.len) return error.InvalidRange;

        const replaced = end_offset - start_offset;
        const new_len = text.len - replaced + new_text.len;
        var buffer = try allocator.alloc(u8, new_len);
        errdefer allocator.free(buffer);

        @memcpy(buffer[0..start_offset], text[0..start_offset]);
        @memcpy(buffer[start_offset .. start_offset + new_text.len], new_text);
        @memcpy(buffer[start_offset + new_text.len ..], text[end_offset..]);

        return buffer;
    }
};

fn buildLineStarts(allocator: Allocator, source: []const u8) Allocator.Error![]u32 {
    var count: usize = 1;
    for (source) |byte| {
        if (byte == '\n') count += 1;
    }

    const starts = try allocator.alloc(u32, count);
    starts[0] = 0;
    var line: usize = 1;
    for (source, 0..) |byte, offset| {
        if (byte == '\n') {
            starts[line] = @intCast(offset + 1);
            line += 1;
        }
    }
    return starts;
}
