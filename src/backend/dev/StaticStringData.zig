//! Readonly data records for LIR string and byte-list literal backing.

const std = @import("std");
const base = @import("base");
const lir = @import("lir");
const RocTarget = @import("roc_target").RocTarget;

const StaticDataExport = @import("StaticDataExport.zig").StaticDataExport;
const StaticDataRelocation = @import("StaticDataExport.zig").StaticDataRelocation;

const Allocator = std.mem.Allocator;

/// Codegen lookup entry for a LIR literal backing.
pub const Entry = struct {
    id: base.StringLiteral.Idx,
    symbol_name: []const u8,
};

/// Readonly data exports and lookup entries for literal backing.
pub const Table = struct {
    allocator: Allocator,
    exports: []StaticDataExport,
    entries: []Entry,

    pub fn deinit(self: *Table) void {
        for (self.exports) |static_export| {
            self.allocator.free(static_export.symbol_name);
            self.allocator.free(static_export.bytes);
            for (static_export.relocations) |relocation| {
                if (relocation.owns_target_symbol_name) self.allocator.free(relocation.target_symbol_name);
            }
            self.allocator.free(static_export.relocations);
        }
        self.allocator.free(self.exports);
        self.allocator.free(self.entries);
        self.* = .{
            .allocator = self.allocator,
            .exports = &.{},
            .entries = &.{},
        };
    }

    pub fn find(self: *const Table, id: base.StringLiteral.Idx) ?Entry {
        for (self.entries) |entry| {
            if (entry.id == id) return entry;
        }
        return null;
    }
};

/// Build readonly data exports for all string-store backings in a LIR store.
pub fn build(allocator: Allocator, store: *const lir.LirStore, target: RocTarget) Allocator.Error!Table {
    const word_size: u32 = @intCast(target.ptrBitWidth() / 8);

    var exports = std.ArrayList(StaticDataExport).empty;
    var entries = std.ArrayList(Entry).empty;
    var exports_live = true;
    var entries_live = true;
    errdefer {
        if (exports_live) {
            for (exports.items) |static_export| {
                allocator.free(static_export.symbol_name);
                allocator.free(static_export.bytes);
                allocator.free(static_export.relocations);
            }
            exports.deinit(allocator);
        }
        if (entries_live) entries.deinit(allocator);
    }

    var strings = store.strings.iterator();
    while (strings.next()) |entry| {
        const symbol_name = try std.fmt.allocPrint(allocator, "roc__static_str_{d}", .{@intFromEnum(entry.idx)});
        var symbol_owned = true;
        errdefer if (symbol_owned) allocator.free(symbol_name);

        const data_offset = staticDataPtrOffset(word_size, word_size, false);
        const bytes = try allocator.alloc(u8, data_offset + entry.bytes.len);
        var bytes_owned = true;
        errdefer if (bytes_owned) allocator.free(bytes);
        @memset(bytes, 0);
        writeSignedWord(word_size, bytes, data_offset - word_size, 0);
        @memcpy(bytes[data_offset..][0..entry.bytes.len], entry.bytes);

        const relocations = try allocator.alloc(StaticDataRelocation, 0);
        var relocations_owned = true;
        errdefer if (relocations_owned) allocator.free(relocations);

        try exports.append(allocator, .{
            .symbol_name = symbol_name,
            .bytes = bytes,
            .symbol_offset = data_offset,
            .alignment = word_size,
            .is_global = false,
            .relocations = relocations,
        });
        symbol_owned = false;
        bytes_owned = false;
        relocations_owned = false;
        try entries.append(allocator, .{
            .id = entry.idx,
            .symbol_name = symbol_name,
        });
    }

    const owned_exports = try exports.toOwnedSlice(allocator);
    exports_live = false;
    errdefer {
        for (owned_exports) |static_export| {
            allocator.free(static_export.symbol_name);
            allocator.free(static_export.bytes);
            allocator.free(static_export.relocations);
        }
        allocator.free(owned_exports);
    }
    const owned_entries = try entries.toOwnedSlice(allocator);
    entries_live = false;

    return .{
        .allocator = allocator,
        .exports = owned_exports,
        .entries = owned_entries,
    };
}

fn staticDataPtrOffset(word_size: u32, element_alignment: u32, contains_refcounted: bool) u32 {
    const required_space = if (contains_refcounted) word_size * 2 else word_size;
    return @intCast(std.mem.alignForward(usize, required_space, element_alignment));
}

fn writeSignedWord(word_size: u32, bytes: []u8, offset: u32, value: isize) void {
    switch (word_size) {
        4 => std.mem.writeInt(i32, bytes[offset..][0..4], @intCast(value), .little),
        8 => std.mem.writeInt(i64, bytes[offset..][0..8], @intCast(value), .little),
        else => unreachable,
    }
}

test "build emits all literal backing with static refcount headers" {
    const allocator = std.testing.allocator;

    var store = lir.LirStore.init(allocator);
    defer store.deinit();

    const small = try store.insertString("small");
    const large = try store.insertString("this string is longer than twenty three bytes");

    var table = try build(allocator, &store, .x64linux);
    defer table.deinit();

    try std.testing.expectEqual(@as(usize, 2), table.exports.len);
    try std.testing.expectEqual(@as(usize, 2), table.entries.len);
    try std.testing.expect(table.find(small) != null);
    try std.testing.expect(table.find(large) != null);

    for (table.entries) |entry| {
        const text = store.getString(entry.id);
        const static_export = for (table.exports) |candidate| {
            if (std.mem.eql(u8, candidate.symbol_name, entry.symbol_name)) break candidate;
        } else return error.MissingStaticDataExport;

        try std.testing.expectEqual(@as(u32, 8), static_export.symbol_offset);
        try std.testing.expectEqual(@as(usize, 8 + text.len), static_export.bytes.len);
        try std.testing.expectEqual(@as(i64, 0), std.mem.readInt(i64, static_export.bytes[0..8], .little));
        try std.testing.expectEqualSlices(u8, text, static_export.bytes[8..]);
    }
}
