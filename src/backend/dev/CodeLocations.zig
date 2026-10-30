//! Where an object's code came from: a compact table over its code offsets
//! that travels with the object in the per-module Boxy object cache
//! (design.md "Compile-Time Failure Sites"). Generated code carries nothing
//! extra, so a program that does not fail pays only for keeping these bytes.
//! A failure finds its statement and meaning from the addresses it left, by
//! binary search over the encoded rows, with no decoding step.
//!
//! Layout, all little-endian: four u32 row counts (regions, lines, failure
//! sites, source files), then each kind of row, then the source file names.

const std = @import("std");
const base = @import("base");
const lir = @import("lir");

const Allocator = std.mem.Allocator;
const FailureMeaning = lir.LIR.FailureMeaning;

const Self = @This();

const header_size = 4 * @sizeOf(u32);
const region_size = 2 * @sizeOf(u32);
const line_size = 6 * @sizeOf(u32);
const site_size = 2 * @sizeOf(u32) + 32 + 2 * @sizeOf(u32);
const file_size = 32 + 4 * @sizeOf(u32);

/// One encoded table, whose bytes `validate` accepted.
bytes: []const u8,

/// A source line table row.
pub const Line = struct {
    offset: u32,
    loc: base.SourceLoc,
    region: base.Region,
};

/// A call whose failure means more than a crash at its statement.
pub const Site = struct {
    offset: u32,
    meaning: FailureMeaning,
};

/// A half-open range of the object's code that line rows may cover.
pub const CodeRange = struct {
    start: u32,
    end: u32,
};

/// What `encode` writes. Regions, lines and sites are sorted by offset.
pub const Input = struct {
    regions: []const CodeRange,
    lines: []const Line,
    sites: []const Site,
    files: []const base.SourceFileEntry,
};

/// The table of `input`, owned by the caller.
pub fn encode(allocator: Allocator, input: Input) Allocator.Error![]u8 {
    var bytes: std.ArrayList(u8) = .empty;
    errdefer bytes.deinit(allocator);
    const word = struct {
        fn append(list: *std.ArrayList(u8), gpa: Allocator, value: u32) Allocator.Error!void {
            try list.appendSlice(gpa, &std.mem.toBytes(std.mem.nativeToLittle(u32, value)));
        }
    }.append;
    try word(&bytes, allocator, @intCast(input.regions.len));
    try word(&bytes, allocator, @intCast(input.lines.len));
    try word(&bytes, allocator, @intCast(input.sites.len));
    try word(&bytes, allocator, @intCast(input.files.len));
    for (input.regions) |region| {
        try word(&bytes, allocator, region.start);
        try word(&bytes, allocator, region.end);
    }
    for (input.lines) |line| {
        try word(&bytes, allocator, line.offset);
        try word(&bytes, allocator, line.loc.file);
        try word(&bytes, allocator, line.loc.line);
        try word(&bytes, allocator, line.loc.column);
        try word(&bytes, allocator, line.region.start.offset);
        try word(&bytes, allocator, line.region.end.offset);
    }
    for (input.sites) |site| {
        const row: struct { tag: u32, module: [32]u8, first: u32, second: u32 } = switch (site.meaning) {
            .checked_error => .{ .tag = 0, .module = @splat(0), .first = 0, .second = 0 },
            .literal_rejection => |literal| .{ .tag = 1, .module = literal.module, .first = literal.checked_expr, .second = @backingInt(literal.kind) },
            .exhaustiveness => |exhaustiveness| .{
                .tag = 2,
                .module = exhaustiveness.module,
                .first = @intFromBool(exhaustiveness.checked_site != null),
                .second = if (exhaustiveness.checked_site) |checked_site| @backingInt(checked_site) else 0,
            },
            .failed_value => |value| switch (value.producer) {
                .checked => |root| .{ .tag = 3, .module = value.module, .first = 0, .second = @backingInt(root) },
                .literal => |root| .{ .tag = 3, .module = value.module, .first = 1, .second = @backingInt(root) },
            },
        };
        try word(&bytes, allocator, site.offset);
        try word(&bytes, allocator, row.tag);
        try bytes.appendSlice(allocator, &row.module);
        try word(&bytes, allocator, row.first);
        try word(&bytes, allocator, row.second);
    }
    var name_offset: u32 = 0;
    for (input.files) |file| {
        try bytes.appendSlice(allocator, &file.module_identity);
        try word(&bytes, allocator, name_offset);
        try word(&bytes, allocator, @intCast(file.name.len));
        name_offset += @intCast(file.name.len);
        try word(&bytes, allocator, name_offset);
        try word(&bytes, allocator, @intCast(file.qualified_name.len));
        name_offset += @intCast(file.qualified_name.len);
    }
    for (input.files) |file| {
        try bytes.appendSlice(allocator, file.name);
        try bytes.appendSlice(allocator, file.qualified_name);
    }
    return try bytes.toOwnedSlice(allocator);
}

/// Whether `bytes` is a table every lookup can read: a cached file can be
/// truncated or altered, and that is a cache miss.
pub fn validate(bytes: []const u8) bool {
    if (bytes.len < header_size) return false;
    const table = Self{ .bytes = bytes };
    const rows_size = @as(u64, table.count(.regions)) * region_size + @as(u64, table.count(.lines)) * line_size +
        @as(u64, table.count(.sites)) * site_size + @as(u64, table.count(.files)) * file_size;
    if (header_size + rows_size > bytes.len) return false;
    const names_start = table.namesStart();
    for (0..table.count(.sites)) |index| {
        const row = table.siteRow(index);
        const tag = wordAt(bytes, row + @sizeOf(u32));
        if (tag > 3) return false;
        if (tag == 1 and std.enums.fromInt(lir.LIR.LiteralRejectionKind, wordAt(bytes, row + 2 * @sizeOf(u32) + 32 + @sizeOf(u32))) == null) return false;
    }
    const names_len = bytes.len - names_start;
    for (0..table.count(.files)) |index| {
        const row = table.fileRow(index) + 32;
        for ([_]usize{ 0, 2 * @sizeOf(u32) }) |at| {
            const end = @as(u64, wordAt(bytes, row + at)) + wordAt(bytes, row + at + @sizeOf(u32));
            if (end > names_len) return false;
        }
    }
    const files = table.count(.files);
    for (0..table.count(.lines)) |index| {
        const file = wordAt(bytes, table.lineRow(index) + @sizeOf(u32));
        if (file != base.SourceLoc.no_file and file >= files) return false;
    }
    return true;
}

/// The line row of the statement whose code holds `offset`: the last row at
/// or before it inside the code region holding it.
pub fn lineEntryAt(self: Self, offset: u32) ?Line {
    const region_index = self.lastAtOrBefore(self.count(.regions), offset, regionStart);
    const region = region_index orelse return null;
    const region_row = self.regionRow(region);
    const region_start = wordAt(self.bytes, region_row);
    if (offset >= wordAt(self.bytes, region_row + @sizeOf(u32))) return null;
    const line_index = self.lastAtOrBefore(self.count(.lines), offset, lineStart) orelse return null;
    const row = self.lineRow(line_index);
    const line = wordAt(self.bytes, row);
    if (line < region_start) return null;
    return .{
        .offset = line,
        .loc = .{
            .file = wordAt(self.bytes, row + 1 * @sizeOf(u32)),
            .line = wordAt(self.bytes, row + 2 * @sizeOf(u32)),
            .column = wordAt(self.bytes, row + 3 * @sizeOf(u32)),
        },
        .region = base.Region.from_raw_offsets(wordAt(self.bytes, row + 4 * @sizeOf(u32)), wordAt(self.bytes, row + 5 * @sizeOf(u32))),
    };
}

/// What failing at the call returning to `return_offset` means, when it means
/// more than a crash at its statement.
pub fn failureMeaningAt(self: Self, return_offset: u32) ?FailureMeaning {
    const index = self.lastAtOrBefore(self.count(.sites), return_offset, siteStart) orelse return null;
    const row = self.siteRow(index);
    if (wordAt(self.bytes, row) != return_offset) return null;
    const module = self.bytes[row + 2 * @sizeOf(u32) ..][0..32].*;
    const first = wordAt(self.bytes, row + 2 * @sizeOf(u32) + 32);
    const second = wordAt(self.bytes, row + 2 * @sizeOf(u32) + 32 + @sizeOf(u32));
    return switch (wordAt(self.bytes, row + @sizeOf(u32))) {
        0 => .checked_error,
        1 => .{ .literal_rejection = .{
            .module = module,
            .checked_expr = first,
            .kind = std.enums.fromInt(lir.LIR.LiteralRejectionKind, second).?,
        } },
        2 => .{ .exhaustiveness = .{
            .module = module,
            .checked_site = if (first != 0) @fromBackingInt(second) else null,
        } },
        else => .{ .failed_value = .{
            .module = module,
            .producer = if (first == 0) .{ .checked = @fromBackingInt(second) } else .{ .literal = @fromBackingInt(second) },
        } },
    };
}

/// One source file the table's line rows name.
pub fn sourceFile(self: Self, index: u32) base.SourceFileEntry {
    const row = self.fileRow(index);
    const names = self.bytes[self.namesStart()..];
    const name = row + 32;
    return .{
        .module_identity = self.bytes[row..][0..32].*,
        .name = names[wordAt(self.bytes, name)..][0..wordAt(self.bytes, name + @sizeOf(u32))],
        .qualified_name = names[wordAt(self.bytes, name + 2 * @sizeOf(u32))..][0..wordAt(self.bytes, name + 3 * @sizeOf(u32))],
    };
}

const Rows = enum(u32) { regions, lines, sites, files };

fn count(self: Self, rows: Rows) u32 {
    return wordAt(self.bytes, @backingInt(rows) * @sizeOf(u32));
}

fn regionRow(_: Self, index: usize) usize {
    return header_size + index * region_size;
}

fn lineRow(self: Self, index: usize) usize {
    return header_size + @as(usize, self.count(.regions)) * region_size + index * line_size;
}

fn siteRow(self: Self, index: usize) usize {
    return self.lineRow(self.count(.lines)) + index * site_size;
}

fn fileRow(self: Self, index: usize) usize {
    return self.siteRow(self.count(.sites)) + index * file_size;
}

fn namesStart(self: Self) usize {
    return self.fileRow(self.count(.files));
}

fn regionStart(self: Self, index: usize) u32 {
    return wordAt(self.bytes, self.regionRow(index));
}

fn lineStart(self: Self, index: usize) u32 {
    return wordAt(self.bytes, self.lineRow(index));
}

fn siteStart(self: Self, index: usize) u32 {
    return wordAt(self.bytes, self.siteRow(index));
}

/// The last of `rows` sorted rows starting at or before `offset`.
fn lastAtOrBefore(self: Self, rows: u32, offset: u32, comptime start: fn (Self, usize) u32) ?usize {
    var low: usize = 0;
    var high: usize = rows;
    while (low < high) {
        const middle = low + (high - low) / 2;
        if (start(self, middle) <= offset) low = middle + 1 else high = middle;
    }
    return if (low == 0) null else low - 1;
}

fn wordAt(bytes: []const u8, at: usize) u32 {
    return std.mem.readInt(u32, bytes[at..][0..4], .little);
}

test "a table answers what the code generator's own tables answer" {
    const module: [32]u8 = @splat(7);
    const encoded = try encode(std.testing.allocator, .{
        .regions = &.{ .{ .start = 16, .end = 64 }, .{ .start = 80, .end = 96 } },
        .lines = &.{
            .{ .offset = 16, .loc = .{ .file = 0, .line = 3, .column = 1 }, .region = base.Region.from_raw_offsets(10, 20) },
            .{ .offset = 32, .loc = .{ .file = 1, .line = 9, .column = 5 }, .region = base.Region.from_raw_offsets(30, 40) },
        },
        .sites = &.{
            .{ .offset = 40, .meaning = .{ .exhaustiveness = .{ .module = module, .checked_site = null } } },
            .{ .offset = 48, .meaning = .{ .failed_value = .{ .module = module, .producer = .{ .literal = @fromBackingInt(4) } } } },
        },
        .files = &.{
            .{ .name = "Lib", .qualified_name = "pf.Lib", .module_identity = module },
            .{ .name = "Util", .qualified_name = "Util", .module_identity = @splat(9) },
        },
    });
    defer std.testing.allocator.free(encoded);
    try std.testing.expect(validate(encoded));
    try std.testing.expect(!validate(encoded[0 .. encoded.len - 1]));
    const table = Self{ .bytes = encoded };

    try std.testing.expectEqual(@as(?Line, null), table.lineEntryAt(8));
    try std.testing.expectEqual(@as(u32, 3), table.lineEntryAt(31).?.loc.line);
    try std.testing.expectEqual(@as(u32, 9), table.lineEntryAt(63).?.loc.line);
    try std.testing.expectEqual(@as(?Line, null), table.lineEntryAt(70));
    try std.testing.expectEqual(@as(?Line, null), table.lineEntryAt(85));
    try std.testing.expectEqual(@as(u32, 30), table.lineEntryAt(40).?.region.start.offset);

    try std.testing.expectEqual(@as(?FailureMeaning, null), table.failureMeaningAt(44));
    try std.testing.expectEqual(module, table.failureMeaningAt(40).?.exhaustiveness.module);
    try std.testing.expectEqual(@as(u32, 4), @backingInt(table.failureMeaningAt(48).?.failed_value.producer.literal));

    try std.testing.expectEqualStrings("Util", table.sourceFile(1).name);
    try std.testing.expectEqualStrings("pf.Lib", table.sourceFile(0).qualified_name);
}
