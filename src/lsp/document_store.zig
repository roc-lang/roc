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

    /// Applies a range replacement to an existing document using UTF-16 positions.
    pub fn applyRangeReplacement(self: *DocumentStore, uri: []const u8, version: i64, range: Range, new_text: []const u8) !void {
        const change = ContentChange{ .text = new_text, .range = range };
        try self.applyContentChanges(uri, version, &.{change});
    }

    /// Applies one or more content changes in order, mirroring LSP incremental edits.
    pub fn applyContentChanges(self: *DocumentStore, uri: []const u8, version: i64, changes: []const ContentChange) !void {
        if (changes.len == 0) return error.NoChanges;

        const entry = self.entries.getPtr(uri) orelse return error.DocumentNotFound;

        var current = try self.allocator.dupe(u8, entry.text);
        var current_owned = true;
        defer if (current_owned) self.allocator.free(current);

        for (changes) |change| {
            const updated = try self.applyChangeToText(current, change);
            self.allocator.free(current);
            current = updated;
        }

        self.allocator.free(entry.text);
        entry.text = current;
        entry.version = version;
        current_owned = false;
    }

    fn applyChangeToText(self: *DocumentStore, text: []const u8, change: ContentChange) ![]u8 {
        if (change.range) |range| {
            return replaceRange(self.allocator, text, range, change.text);
        } else {
            return self.allocator.dupe(u8, change.text);
        }
    }

    fn replaceRange(allocator: std.mem.Allocator, text: []const u8, range: Range, new_text: []const u8) ![]u8 {
        const start_offset = try positionToOffset(text, range.start_line, range.start_character);
        const end_offset = try positionToOffset(text, range.end_line, range.end_character);
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

    fn positionToOffset(text: []const u8, line: usize, character_utf16: usize) !usize {
        var current_line: usize = 0;
        var index: usize = 0;
        while (current_line < line) : (current_line += 1) {
            const newline_index = std.mem.indexOfScalarPos(u8, text, index, '\n') orelse return error.InvalidPosition;
            index = newline_index + 1;
        }

        var utf16_units: usize = 0;
        var it = std.unicode.Utf8Iterator{ .bytes = text[index..], .i = 0 };
        while (utf16_units < character_utf16) {
            const slice = it.nextCodepointSlice() orelse return error.InvalidPosition;
            const cp = std.unicode.utf8Decode(slice) catch return error.InvalidPosition;
            utf16_units += if (cp <= 0xFFFF) 1 else 2;
        }

        if (utf16_units != character_utf16) return error.InvalidPosition;
        return index + it.i;
    }
};
