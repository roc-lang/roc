//! Document storage for tracking open text documents in the LSP server.

const std = @import("std");
const position = @import("position.zig");
const Allocator = std.mem.Allocator;

/// Stores the latest contents of each open text document.
pub const DocumentStore = struct {
    allocator: std.mem.Allocator,
    entries: std.StringHashMap(Entry),

    /// Snapshot of a document's contents and version.
    pub const Document = struct {
        text: []u8,
        version: i64,
    };

    /// A stored document. Edits are applied to `text` in place, so its
    /// capacity is kept across changes.
    const Entry = struct {
        text: std.ArrayList(u8),
        version: i64,
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
        return .{ .allocator = allocator, .entries = std.StringHashMap(Entry).init(allocator) };
    }

    /// Releases all tracked documents and frees associated memory.
    pub fn deinit(self: *DocumentStore) void {
        var it = self.entries.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.text.deinit(self.allocator);
        }
        self.entries.deinit();
        self.* = undefined;
    }

    /// Inserts or replaces the document at `uri` with the given text and version.
    pub fn upsert(self: *DocumentStore, uri: []const u8, version: i64, text: []const u8) Allocator.Error!void {
        if (self.entries.getPtr(uri)) |entry| {
            entry.text.clearRetainingCapacity();
            try entry.text.appendSlice(self.allocator, text);
            entry.version = version;
            return;
        }

        const owned_uri = try self.allocator.dupe(u8, uri);
        errdefer self.allocator.free(owned_uri);
        var owned_text: std.ArrayList(u8) = .empty;
        errdefer owned_text.deinit(self.allocator);
        try owned_text.appendSlice(self.allocator, text);
        try self.entries.putNoClobber(owned_uri, .{ .text = owned_text, .version = version });
    }

    /// Removes a document from the store, if present.
    pub fn remove(self: *DocumentStore, uri: []const u8) void {
        if (self.entries.fetchRemove(uri)) |removed| {
            self.allocator.free(removed.key);
            var text = removed.value.text;
            text.deinit(self.allocator);
        }
    }

    /// Returns the stored document (if any). The returned slice references memory owned by the store
    /// and stays valid until the document is next changed or removed.
    pub fn get(self: *DocumentStore, uri: []const u8) ?Document {
        const entry = self.entries.getPtr(uri) orelse return null;
        return .{ .text = entry.text.items, .version = entry.version };
    }

    /// Applies a range replacement to an existing document using UTF-16 positions.
    pub fn applyRangeReplacement(self: *DocumentStore, uri: []const u8, version: i64, range: Range, new_text: []const u8) (Allocator.Error || error{ NoChanges, DocumentNotFound, InvalidRange, InvalidPosition })!void {
        const change = ContentChange{ .text = new_text, .range = range };
        try self.applyContentChanges(uri, version, &.{change});
    }

    /// Applies one or more content changes in order, mirroring LSP incremental edits.
    ///
    /// The batch applies as a whole or not at all: a change whose range does
    /// not exist in the text the earlier changes produced leaves the document
    /// as it was.
    pub fn applyContentChanges(self: *DocumentStore, uri: []const u8, version: i64, changes: []const ContentChange) (Allocator.Error || error{ NoChanges, DocumentNotFound, InvalidRange, InvalidPosition })!void {
        if (changes.len == 0) return error.NoChanges;

        const entry = self.entries.getPtr(uri) orelse return error.DocumentNotFound;

        // Each change validates its range before touching the text, so a
        // single change needs no copy to restore from; a longer batch keeps one
        // copy of the original for when a later change fails.
        var original: ?[]u8 = null;
        defer if (original) |text| self.allocator.free(text);
        if (changes.len > 1) original = try self.allocator.dupe(u8, entry.text.items);
        errdefer if (original) |text| {
            entry.text.clearRetainingCapacity();
            entry.text.appendSliceAssumeCapacity(text);
        };

        for (changes) |change| {
            try self.applyChange(&entry.text, change);
        }

        entry.version = version;
    }

    fn applyChange(self: *DocumentStore, text: *std.ArrayList(u8), change: ContentChange) (Allocator.Error || error{ InvalidRange, InvalidPosition })!void {
        const range = change.range orelse {
            text.clearRetainingCapacity();
            return text.appendSlice(self.allocator, change.text);
        };
        const start_offset = try positionToOffset(text.items, range.start_line, range.start_character);
        const end_offset = try positionToOffset(text.items, range.end_line, range.end_character);
        if (start_offset > end_offset or end_offset > text.items.len) return error.InvalidRange;

        try text.replaceRange(self.allocator, start_offset, end_offset - start_offset, change.text);
    }

    fn positionToOffset(text: []const u8, line: usize, character_utf16: usize) (Allocator.Error || error{InvalidPosition})!usize {
        var current_line: usize = 0;
        var index: usize = 0;
        while (current_line < line) : (current_line += 1) {
            const newline_index = std.mem.findScalarPos(u8, text, index, '\n') orelse return error.InvalidPosition;
            index = newline_index + 1;
        }

        // Only this line, never the rest of the document. A newline is an
        // ordinary code unit to the conversion below, so a `character` past the
        // end of its line would otherwise resolve into a later line and the
        // edit would swallow everything in between.
        const line_end = if (std.mem.findScalarPos(u8, text, index, '\n')) |newline_index|
            newline_index + 1
        else
            text.len;
        const line_text = position.trimEol(text[index..line_end]);

        // The same conversion the query path uses, in its strict mode: an edit
        // applied at a miscounted position would corrupt the document.
        const column = position.utf16ColumnToByteOffset(line_text, character_utf16, .exact) orelse
            return error.InvalidPosition;
        return index + column;
    }
};
