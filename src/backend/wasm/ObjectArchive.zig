//! Unix archive reader for wasm object inputs.

const std = @import("std");

const archive_magic = "!<arch>\n";
const wasm_magic = "\x00asm";
const header_len = 60;
const header_end = "`\n";

/// Errors returned while reading Unix archive bytes.
pub const ParseError = error{
    InvalidArchiveMagic,
    InvalidArchiveHeader,
    InvalidArchiveSize,
    InvalidArchiveName,
    UnexpectedEnd,
};

/// One member entry from a Unix archive.
pub const Member = struct {
    name: []const u8,
    bytes: []const u8,
};

/// Return whether the bytes start with the Unix archive magic header.
pub fn isArchive(bytes: []const u8) bool {
    return bytes.len >= archive_magic.len and std.mem.eql(u8, bytes[0..archive_magic.len], archive_magic);
}

/// Return whether the bytes start with the wasm binary magic header.
pub fn isWasmObject(bytes: []const u8) bool {
    return bytes.len >= 8 and std.mem.eql(u8, bytes[0..wasm_magic.len], wasm_magic);
}

/// Streaming reader over Unix archive members.
pub const Iterator = struct {
    bytes: []const u8,
    cursor: usize,
    string_table: ?[]const u8,

    pub fn init(bytes: []const u8) ParseError!Iterator {
        if (!isArchive(bytes)) return error.InvalidArchiveMagic;
        return .{
            .bytes = bytes,
            .cursor = archive_magic.len,
            .string_table = null,
        };
    }

    pub fn next(self: *Iterator) ParseError!?Member {
        while (self.cursor < self.bytes.len) {
            if (self.cursor + header_len > self.bytes.len) return error.UnexpectedEnd;

            const header = self.bytes[self.cursor..][0..header_len];
            self.cursor += header_len;

            if (!std.mem.eql(u8, header[58..60], header_end)) return error.InvalidArchiveHeader;

            const member_size = try parseDecimal(header[48..58]);
            if (self.cursor + member_size > self.bytes.len) return error.UnexpectedEnd;

            const member_bytes = self.bytes[self.cursor..][0..member_size];
            self.cursor += member_size;
            if ((member_size & 1) == 1) {
                if (self.cursor >= self.bytes.len) return error.UnexpectedEnd;
                self.cursor += 1;
            }

            const member = try self.decodeMember(header[0..16], member_bytes);
            if (std.mem.eql(u8, member.name, "//")) {
                self.string_table = member.bytes;
                continue;
            }
            if (isMetadataMember(member.name)) continue;
            return member;
        }

        return null;
    }

    fn decodeMember(self: *const Iterator, raw_name_field: []const u8, member_bytes: []const u8) ParseError!Member {
        const raw_name = trimRightSpaces(raw_name_field);

        if (std.mem.startsWith(u8, raw_name, "#1/")) {
            const name_len = try parseDecimal(raw_name[3..]);
            if (name_len > member_bytes.len) return error.UnexpectedEnd;
            return .{
                .name = member_bytes[0..name_len],
                .bytes = member_bytes[name_len..],
            };
        }

        if (std.mem.eql(u8, raw_name, "/") or
            std.mem.eql(u8, raw_name, "//") or
            std.mem.eql(u8, raw_name, "/SYM64/"))
        {
            return .{ .name = raw_name, .bytes = member_bytes };
        }

        if (raw_name.len > 1 and raw_name[0] == '/') {
            const offset = try parseDecimal(raw_name[1..]);
            const table = self.string_table orelse return error.InvalidArchiveName;
            if (offset >= table.len) return error.InvalidArchiveName;

            var end = offset;
            while (end < table.len and table[end] != '\n') : (end += 1) {}

            return .{
                .name = stripNameTerminator(table[offset..end]),
                .bytes = member_bytes,
            };
        }

        return .{
            .name = stripNameTerminator(raw_name),
            .bytes = member_bytes,
        };
    }
};

fn parseDecimal(bytes: []const u8) ParseError!usize {
    const trimmed = std.mem.trim(u8, bytes, " ");
    if (trimmed.len == 0) return error.InvalidArchiveSize;
    return std.fmt.parseInt(usize, trimmed, 10) catch error.InvalidArchiveSize;
}

fn trimRightSpaces(bytes: []const u8) []const u8 {
    var end = bytes.len;
    while (end > 0 and bytes[end - 1] == ' ') : (end -= 1) {}
    return bytes[0..end];
}

fn stripNameTerminator(bytes: []const u8) []const u8 {
    var end = bytes.len;
    while (end > 0 and bytes[end - 1] == 0) : (end -= 1) {}
    if (end > 0 and bytes[end - 1] == '/') end -= 1;
    return bytes[0..end];
}

fn isMetadataMember(name: []const u8) bool {
    return std.mem.eql(u8, name, "/") or
        std.mem.eql(u8, name, "/SYM64/") or
        std.mem.eql(u8, name, "__.SYMDEF") or
        std.mem.eql(u8, name, "__.SYMDEF SORTED");
}

fn appendArchiveMember(bytes: *std.ArrayList(u8), name_field: []const u8, member_bytes: []const u8) (std.mem.Allocator.Error || error{NoSpaceLeft})!void {
    var header: [header_len]u8 = undefined;
    @memset(header[0..], ' ');

    std.mem.copyForwards(u8, header[0..name_field.len], name_field);
    _ = try std.fmt.bufPrint(header[48..58], "{d}", .{member_bytes.len});
    std.mem.copyForwards(u8, header[58..60], header_end);

    try bytes.appendSlice(std.testing.allocator, &header);
    try bytes.appendSlice(std.testing.allocator, member_bytes);
    if ((member_bytes.len & 1) == 1) try bytes.append(std.testing.allocator, '\n');
}

test "object archive iterator reads GNU long-name members" {
    var bytes: std.ArrayList(u8) = .empty;
    defer bytes.deinit(std.testing.allocator);

    try bytes.appendSlice(std.testing.allocator, archive_magic);
    try appendArchiveMember(&bytes, "/", &.{ 0, 1, 2, 3 });
    try appendArchiveMember(&bytes, "//", "long/object_one.o/\nother.o/\n");
    try appendArchiveMember(&bytes, "/0", wasm_magic ++ "\x01\x00\x00\x00one");
    try appendArchiveMember(&bytes, "short.o/", wasm_magic ++ "\x01\x00\x00\x00two");

    var iter = try Iterator.init(bytes.items);
    const first = (try iter.next()).?;
    try std.testing.expectEqualStrings("long/object_one.o", first.name);
    try std.testing.expect(isWasmObject(first.bytes));

    const second = (try iter.next()).?;
    try std.testing.expectEqualStrings("short.o", second.name);
    try std.testing.expect(isWasmObject(second.bytes));

    try std.testing.expectEqual(@as(?Member, null), try iter.next());
}

test "object archive iterator reads BSD extended-name members" {
    var bytes: std.ArrayList(u8) = .empty;
    defer bytes.deinit(std.testing.allocator);

    const name = "extended-name.o";
    const object_bytes = wasm_magic ++ "\x01\x00\x00\x00body";

    var member_bytes: std.ArrayList(u8) = .empty;
    defer member_bytes.deinit(std.testing.allocator);
    try member_bytes.appendSlice(std.testing.allocator, name);
    try member_bytes.appendSlice(std.testing.allocator, object_bytes);

    try bytes.appendSlice(std.testing.allocator, archive_magic);
    try appendArchiveMember(&bytes, "#1/15", member_bytes.items);

    var iter = try Iterator.init(bytes.items);
    const member = (try iter.next()).?;
    try std.testing.expectEqualStrings(name, member.name);
    try std.testing.expectEqualSlices(u8, object_bytes, member.bytes);
    try std.testing.expectEqual(@as(?Member, null), try iter.next());
}

test "object archive iterator rejects missing long-name table" {
    var bytes: std.ArrayList(u8) = .empty;
    defer bytes.deinit(std.testing.allocator);

    try bytes.appendSlice(std.testing.allocator, archive_magic);
    try appendArchiveMember(&bytes, "/0", wasm_magic ++ "\x01\x00\x00\x00");

    var iter = try Iterator.init(bytes.items);
    try std.testing.expectError(error.InvalidArchiveName, iter.next());
}

test "object archive iterator rejects non-archive bytes" {
    try std.testing.expectError(error.InvalidArchiveMagic, Iterator.init("not an archive"));
}
