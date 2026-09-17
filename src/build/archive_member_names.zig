//! Build-time rewrite of a static archive's member names to bare file names.
//!
//! Zig names each member of a static library after the path of the object it
//! packed, so the shim archives the `roc` binary embeds carried member names
//! such as `.zig-cache/o/<hash>/libroc_interpreter_shim_zcu.o` and
//! `/home/<user>/.cache/zig/o/<hash>/compiler_rt.o`: a Zig cache key and the
//! build machine's home directory, shipped inside every Linux `roc`. A member
//! name is only a name -- nothing resolves it against a filesystem -- so this
//! tool keeps the file name and drops the directories. It rewrites the GNU and
//! COFF layout Zig emits for Linux and Windows (a `/` symbol index holding one
//! member offset per symbol, an optional COFF second linker member holding
//! every member offset, and a `//` long-name table) and regenerates every
//! offset those indexes hold. BSD archives (macOS) already store bare names
//! inline and pass through unchanged. Every other byte of every member is
//! kept, so the archive links exactly as before.
//!
//! Usage: archive_member_names <linux|macos|windows> <input> <output>

const std = @import("std");

const Os = enum { linux, macos, windows };

const RewriteError = error{
    NotAnArchive,
    MalformedArchive,
    UnknownIndexOffset,
    OutOfMemory,
    NoSpaceLeft,
    Overflow,
    InvalidCharacter,
};

const MainError = RewriteError || std.process.Args.ToSliceError ||
    std.Io.Dir.ReadFileAllocError || std.Io.Dir.WriteFileError || error{
    ExpectedOsInputAndOutputPaths,
    UnsupportedTarget,
};

/// Rewrite the archive at `args[2]` for the OS named by `args[1]`, writing the
/// result to `args[3]`.
pub fn main(init: std.process.Init) MainError!void {
    const gpa = init.gpa;
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 4) return error.ExpectedOsInputAndOutputPaths;
    const os = std.meta.stringToEnum(Os, args[1]) orelse return error.UnsupportedTarget;
    const bytes = try std.Io.Dir.cwd().readFileAlloc(init.io, args[2], gpa, .unlimited);
    defer gpa.free(bytes);
    const rewritten = stripDirectories(gpa, bytes, os) catch |err| {
        std.debug.print("FAILED: {s}: {s}\n", .{ args[2], @errorName(err) });
        return err;
    };
    defer gpa.free(rewritten);
    try std.Io.Dir.cwd().writeFile(init.io, .{ .sub_path = args[3], .data = rewritten });
}

const magic = "!<arch>\n";
const header_len = 60;
/// Longest name the 16-byte header field holds inline alongside its `/` terminator.
const max_inline_name = 15;

const Member = struct {
    /// Offset of this member's 60-byte header in the archive it was parsed from.
    header_offset: usize,
    header: *const [header_len]u8,
    data: []const u8,
    /// The member's name with any archive encoding undone. Index members keep
    /// their reserved names (`/`, `//`, `/SYM64/`).
    name: []const u8,
    kind: Kind,

    const Kind = enum {
        /// GNU/COFF symbol index: big-endian symbol count, one big-endian
        /// member offset per symbol, then NUL-terminated names.
        symbol_index,
        /// The 64-bit form of `symbol_index` (`/SYM64/`), with u64 fields.
        symbol_index_64,
        /// COFF second linker member: little-endian member count, one
        /// little-endian offset per member, then the symbol table.
        coff_second_index,
        long_names,
        object,
    };
};

const Parsed = struct {
    members: []Member,
    /// True for a BSD archive, whose names live inline in each member and are
    /// already bare.
    bsd: bool,
};

fn parse(gpa: std.mem.Allocator, bytes: []const u8, os: Os) RewriteError!Parsed {
    if (!std.mem.startsWith(u8, bytes, magic)) return error.NotAnArchive;
    var members = std.ArrayList(Member).empty;
    errdefer members.deinit(gpa);
    var long_names: []const u8 = &.{};
    var symbol_indexes: usize = 0;
    var offset: usize = magic.len;
    while (offset < bytes.len) {
        if (bytes.len - offset < header_len) return error.MalformedArchive;
        const header = bytes[offset..][0..header_len];
        if (!std.mem.eql(u8, header[58..60], "`\n")) return error.MalformedArchive;
        const raw_name = std.mem.trimEnd(u8, header[0..16], " ");
        const size = try std.fmt.parseInt(usize, std.mem.trimEnd(u8, header[48..58], " "), 10);
        if (bytes.len - offset - header_len < size) return error.MalformedArchive;
        const data = bytes[offset + header_len ..][0..size];
        if (std.mem.startsWith(u8, raw_name, "#1/") or std.mem.startsWith(u8, raw_name, "__.SYMDEF")) {
            members.deinit(gpa);
            return .{ .members = &.{}, .bsd = true };
        }
        var member: Member = .{ .header_offset = offset, .header = header, .data = data, .name = raw_name, .kind = .object };
        if (std.mem.eql(u8, raw_name, "/")) {
            symbol_indexes += 1;
            member.kind = switch (symbol_indexes) {
                1 => .symbol_index,
                2 => if (os == .windows) .coff_second_index else return error.MalformedArchive,
                else => return error.MalformedArchive,
            };
        } else if (std.mem.eql(u8, raw_name, "/SYM64/")) {
            member.kind = .symbol_index_64;
        } else if (std.mem.eql(u8, raw_name, "//")) {
            member.kind = .long_names;
            long_names = data;
        } else if (raw_name.len > 1 and raw_name[0] == '/' and std.ascii.isDigit(raw_name[1])) {
            const start = try std.fmt.parseInt(usize, raw_name[1..], 10);
            if (start > long_names.len) return error.MalformedArchive;
            const rest = long_names[start..];
            const terminator: u8 = if (os == .windows) 0 else '\n';
            const len = std.mem.findScalar(u8, rest, terminator) orelse return error.MalformedArchive;
            member.name = std.mem.trimEnd(u8, rest[0..len], "/");
        } else {
            member.name = std.mem.trimEnd(u8, raw_name, "/");
        }
        try members.append(gpa, member);
        offset += header_len + size + (size & 1);
    }
    return .{ .members = try members.toOwnedSlice(gpa), .bsd = false };
}

fn baseName(name: []const u8) RewriteError![]const u8 {
    const base = if (std.mem.lastIndexOfAny(u8, name, "/\\")) |i| name[i + 1 ..] else name;
    if (base.len == 0) return error.MalformedArchive;
    return base;
}

/// Write `size` into a header's 10-character decimal size field.
fn setHeaderSize(header: *[header_len]u8, size: usize) RewriteError!void {
    @memset(header[48..58], ' ');
    _ = try std.fmt.bufPrint(header[48..58], "{d}", .{size});
}

/// Rewrite every member name of `bytes` to its bare file name and return the
/// new archive. Index members keep their contents except for the member
/// offsets they hold, which are recomputed for the new layout.
pub fn stripDirectories(gpa: std.mem.Allocator, bytes: []const u8, os: Os) RewriteError![]u8 {
    const parsed = try parse(gpa, bytes, os);
    if (parsed.bsd) return gpa.dupe(u8, bytes);
    const members = parsed.members;
    defer gpa.free(members);

    // Long names go into a fresh `//` table; the original one is dropped.
    var long_names = std.ArrayList(u8).empty;
    defer long_names.deinit(gpa);
    const name_fields = try gpa.alloc([16]u8, members.len);
    defer gpa.free(name_fields);
    for (members, name_fields) |member, *field| {
        @memset(field, ' ');
        switch (member.kind) {
            .symbol_index, .symbol_index_64, .coff_second_index => @memcpy(field[0..member.name.len], member.name),
            .long_names => {},
            .object => {
                const base = try baseName(member.name);
                if (base.len <= max_inline_name) {
                    @memcpy(field[0..base.len], base);
                    field[base.len] = '/';
                } else {
                    _ = try std.fmt.bufPrint(field, "/{d}", .{long_names.items.len});
                    try long_names.appendSlice(gpa, base);
                    try long_names.appendSlice(gpa, if (os == .windows) "\x00" else "/\n");
                }
            },
        }
    }

    // Lay out the new archive: every member in its original order, the `//`
    // table with its new contents (or gone, when no name needs it).
    const new_offsets = try gpa.alloc(usize, members.len);
    defer gpa.free(new_offsets);
    var total: usize = magic.len;
    for (members, 0..) |member, i| {
        new_offsets[i] = total;
        const size = if (member.kind == .long_names) long_names.items.len else member.data.len;
        if (member.kind == .long_names and size == 0) continue;
        total += header_len + size + (size & 1);
    }

    const out = try gpa.alloc(u8, total);
    errdefer gpa.free(out);
    @memcpy(out[0..magic.len], magic);
    for (members, 0..) |member, i| {
        const size = if (member.kind == .long_names) long_names.items.len else member.data.len;
        if (member.kind == .long_names and size == 0) continue;
        const header = out[new_offsets[i]..][0..header_len];
        header.* = member.header.*;
        const body = out[new_offsets[i] + header_len ..][0..size];
        switch (member.kind) {
            .long_names => {
                try setHeaderSize(header, size);
                @memcpy(body, long_names.items);
            },
            .object => {
                header[0..16].* = name_fields[i];
                @memcpy(body, member.data);
            },
            .symbol_index, .symbol_index_64, .coff_second_index => @memcpy(body, member.data),
        }
        if (size & 1 != 0) out[new_offsets[i] + header_len + size] = '\n';
    }

    // Every offset an index holds names a member header in the old layout;
    // move each to that member's new header.
    for (members, 0..) |member, i| {
        const body = out[new_offsets[i] + header_len ..][0..member.data.len];
        switch (member.kind) {
            .symbol_index => try remapOffsets(u32, .big, body, members, new_offsets),
            .symbol_index_64 => try remapOffsets(u64, .big, body, members, new_offsets),
            .coff_second_index => try remapOffsets(u32, .little, body, members, new_offsets),
            .long_names, .object => {},
        }
    }
    return out;
}

/// An index body starts with a count, then that many member offsets.
fn remapOffsets(comptime T: type, comptime endian: std.builtin.Endian, body: []u8, members: []const Member, new_offsets: []const usize) RewriteError!void {
    const width = @sizeOf(T);
    if (body.len < width) return error.MalformedArchive;
    const count = std.mem.readInt(T, body[0..width], endian);
    if (count > (body.len - width) / width) return error.MalformedArchive;
    var i: usize = 0;
    while (i < count) : (i += 1) {
        const field = body[width * (i + 1) ..][0..width];
        const old = std.mem.readInt(T, field, endian);
        const new = for (members, new_offsets) |member, new_offset| {
            if (member.header_offset == old) break new_offset;
        } else return error.UnknownIndexOffset;
        std.mem.writeInt(T, field, @intCast(new), endian);
    }
}

const TestObject = struct { name: []const u8, data: []const u8 };
const TestSymbol = struct { name: []const u8, object: usize };

fn testHeader(out: *[header_len]u8, name_field: []const u8, size: usize) !void {
    const header = try std.fmt.bufPrint(out, "{s:<16}{d:<12}{d:<6}{d:<6}{o:<8}{d:<10}`\n", .{ name_field, @as(u32, 0), @as(u32, 0), @as(u32, 0), @as(u32, 0o644), size });
    try std.testing.expectEqual(header_len, header.len);
}

/// Build a GNU (or, with `coff_second_index`, COFF) archive the way Zig lays
/// one out: `/` index, optional second linker member, `//` long names, objects.
fn testArchive(gpa: std.mem.Allocator, os: Os, objects: []const TestObject, symbols: []const TestSymbol, coff_second_index: bool) ![]u8 {
    var long_names = std.ArrayList(u8).empty;
    defer long_names.deinit(gpa);
    var fields = std.ArrayList([16]u8).empty;
    defer fields.deinit(gpa);
    for (objects) |object| {
        var field: [16]u8 = @splat(' ');
        if (object.name.len <= max_inline_name) {
            @memcpy(field[0..object.name.len], object.name);
            field[object.name.len] = '/';
        } else {
            _ = try std.fmt.bufPrint(&field, "/{d}", .{long_names.items.len});
            try long_names.appendSlice(gpa, object.name);
            try long_names.appendSlice(gpa, if (os == .windows) "\x00" else "/\n");
        }
        try fields.append(gpa, field);
    }

    var index_size: usize = 4 + 4 * symbols.len;
    for (symbols) |symbol| index_size += symbol.name.len + 1;
    var second_size: usize = 0;
    if (coff_second_index) {
        second_size = 4 + 4 * objects.len + 4 + 2 * symbols.len;
        for (symbols) |symbol| second_size += symbol.name.len + 1;
    }
    var offset: usize = magic.len + header_len + index_size + (index_size & 1);
    if (coff_second_index) offset += header_len + second_size + (second_size & 1);
    if (long_names.items.len != 0) offset += header_len + long_names.items.len + (long_names.items.len & 1);
    const object_offsets = try gpa.alloc(usize, objects.len);
    defer gpa.free(object_offsets);
    for (objects, object_offsets) |object, *object_offset| {
        object_offset.* = offset;
        offset += header_len + object.data.len + (object.data.len & 1);
    }

    var out = std.ArrayList(u8).empty;
    errdefer out.deinit(gpa);
    try out.appendSlice(gpa, magic);
    var header: [header_len]u8 = undefined;
    try testHeader(&header, "/", index_size);
    try out.appendSlice(gpa, &header);
    var word: [4]u8 = undefined;
    std.mem.writeInt(u32, &word, @intCast(symbols.len), .big);
    try out.appendSlice(gpa, &word);
    for (symbols) |symbol| {
        std.mem.writeInt(u32, &word, @intCast(object_offsets[symbol.object]), .big);
        try out.appendSlice(gpa, &word);
    }
    for (symbols) |symbol| {
        try out.appendSlice(gpa, symbol.name);
        try out.append(gpa, 0);
    }
    if (index_size & 1 != 0) try out.append(gpa, '\n');
    if (coff_second_index) {
        try testHeader(&header, "/", second_size);
        try out.appendSlice(gpa, &header);
        std.mem.writeInt(u32, &word, @intCast(objects.len), .little);
        try out.appendSlice(gpa, &word);
        for (object_offsets) |object_offset| {
            std.mem.writeInt(u32, &word, @intCast(object_offset), .little);
            try out.appendSlice(gpa, &word);
        }
        std.mem.writeInt(u32, &word, @intCast(symbols.len), .little);
        try out.appendSlice(gpa, &word);
        for (symbols) |symbol| {
            std.mem.writeInt(u16, word[0..2], @intCast(symbol.object + 1), .little);
            try out.appendSlice(gpa, word[0..2]);
        }
        for (symbols) |symbol| {
            try out.appendSlice(gpa, symbol.name);
            try out.append(gpa, 0);
        }
        if (second_size & 1 != 0) try out.append(gpa, '\n');
    }
    if (long_names.items.len != 0) {
        try testHeader(&header, "//", long_names.items.len);
        try out.appendSlice(gpa, &header);
        try out.appendSlice(gpa, long_names.items);
        if (long_names.items.len & 1 != 0) try out.append(gpa, '\n');
    }
    for (objects, fields.items) |object, field| {
        try testHeader(&header, &field, object.data.len);
        try out.appendSlice(gpa, &header);
        try out.appendSlice(gpa, object.data);
        if (object.data.len & 1 != 0) try out.append(gpa, '\n');
    }
    return out.toOwnedSlice(gpa);
}

/// The member an index offset points at, by position in `members`.
fn memberAt(members: []const Member, offset: usize) !usize {
    for (members, 0..) |member, i| if (member.header_offset == offset) return i;
    return error.UnknownIndexOffset;
}

fn expectStripped(os: Os, objects: []const TestObject, symbols: []const TestSymbol, coff_second_index: bool) !void {
    const gpa = std.testing.allocator;
    const input = try testArchive(gpa, os, objects, symbols, coff_second_index);
    defer gpa.free(input);
    const output = try stripDirectories(gpa, input, os);
    defer gpa.free(output);

    const parsed = try parse(gpa, output, os);
    defer gpa.free(parsed.members);
    var object_positions = std.ArrayList(usize).empty;
    defer object_positions.deinit(gpa);
    for (parsed.members, 0..) |member, i| {
        if (member.kind == .object) try object_positions.append(gpa, i);
    }
    try std.testing.expectEqual(objects.len, object_positions.items.len);
    for (objects, object_positions.items) |object, position| {
        const member = parsed.members[position];
        try std.testing.expectEqualStrings(try baseName(object.name), member.name);
        try std.testing.expectEqualStrings(object.data, member.data);
    }
    for (parsed.members) |member| {
        if (member.kind == .long_names) {
            // Every long-name entry is a bare name.
            var entries = std.mem.splitScalar(u8, member.data, if (os == .windows) 0 else '\n');
            while (entries.next()) |entry| {
                if (entry.len == 0) continue;
                try std.testing.expect(std.mem.indexOfAny(u8, std.mem.trimEnd(u8, entry, "/"), "/\\") == null);
            }
        }
    }
    // Every index offset lands on the header of the same object it named before.
    for (parsed.members) |member| {
        switch (member.kind) {
            .symbol_index => {
                const count = std.mem.readInt(u32, member.data[0..4], .big);
                try std.testing.expectEqual(symbols.len, count);
                for (symbols, 0..) |symbol, i| {
                    const target = std.mem.readInt(u32, member.data[4 + 4 * i ..][0..4], .big);
                    try std.testing.expectEqual(object_positions.items[symbol.object], try memberAt(parsed.members, target));
                }
            },
            .coff_second_index => {
                const count = std.mem.readInt(u32, member.data[0..4], .little);
                try std.testing.expectEqual(objects.len, count);
                for (0..count) |i| {
                    const target = std.mem.readInt(u32, member.data[4 + 4 * i ..][0..4], .little);
                    try std.testing.expectEqual(object_positions.items[i], try memberAt(parsed.members, target));
                }
            },
            .symbol_index_64, .long_names, .object => {},
        }
    }
    // A second pass changes nothing.
    const again = try stripDirectories(gpa, output, os);
    defer gpa.free(again);
    try std.testing.expectEqualSlices(u8, output, again);
}

test "GNU archive: cache-directory and home-directory member names become bare names" {
    try expectStripped(.linux, &.{
        .{ .name = ".zig-cache/o/628ece199f4b4ab0747401fb9101049e/roc_builtins.o", .data = "builtins!" },
        .{ .name = "/home/someone/.cache/zig/o/8088feaf6aa22e4277e59a7d9313d3bd/compiler_rt.o", .data = "rt" },
        .{ .name = ".zig-cache/o/07b0e6608fb3e23f19c2490d38b8a9f0/libroc_interpreter_shim_zcu.o", .data = "shim" },
    }, &.{
        .{ .name = "roc_entrypoint", .object = 2 },
        .{ .name = "__addtf3", .object = 1 },
        .{ .name = "roc_builtins_thing", .object = 0 },
        .{ .name = "roc_shim_get_ops", .object = 2 },
    }, false);
}

test "GNU archive: names that already fit inline keep the archive free of a long-name table" {
    const gpa = std.testing.allocator;
    const input = try testArchive(gpa, .linux, &.{
        .{ .name = "a/b/short.o", .data = "1" },
        .{ .name = "x.o", .data = "22" },
    }, &.{.{ .name = "sym", .object = 0 }}, false);
    defer gpa.free(input);
    const output = try stripDirectories(gpa, input, .linux);
    defer gpa.free(output);
    try std.testing.expect(std.mem.indexOf(u8, output, "//") == null);
    try std.testing.expect(std.mem.indexOf(u8, output, "short.o/") != null);
    try expectStripped(.linux, &.{
        .{ .name = "a/b/short.o", .data = "1" },
        .{ .name = "x.o", .data = "22" },
    }, &.{.{ .name = "sym", .object = 0 }}, false);
}

test "COFF archive: both linker members are remapped and backslash paths are stripped" {
    try expectStripped(.windows, &.{
        .{ .name = ".zig-cache\\o\\dd12985c8b4eea73e64a4de0b39c6086\\roc_interpreter_shim_zcu.obj", .data = "obj-one" },
        .{ .name = "C:\\Users\\someone\\AppData\\Local\\zig\\o\\8088feaf6aa22e4277e59a7d9313d3bd\\compiler_rt.obj", .data = "obj-two" },
    }, &.{
        .{ .name = "roc_entrypoint", .object = 0 },
        .{ .name = "__chkstk", .object = 1 },
    }, true);
}

test "BSD archive passes through unchanged" {
    const gpa = std.testing.allocator;
    const name = "roc_builtins.o\x00\x00";
    const data = "object";
    var header: [header_len]u8 = undefined;
    try testHeader(&header, "#1/16", name.len + data.len);
    const input = magic ++ header ++ name ++ data;
    const output = try stripDirectories(gpa, input, .macos);
    defer gpa.free(output);
    try std.testing.expectEqualSlices(u8, input, output);
}

test "malformed input fails closed" {
    const gpa = std.testing.allocator;
    try std.testing.expectError(error.NotAnArchive, stripDirectories(gpa, "not an archive", .linux));
    try std.testing.expectError(error.MalformedArchive, stripDirectories(gpa, magic ++ "truncated header", .linux));
    // An index offset that names no member must not be silently kept.
    var archive = try testArchive(gpa, .linux, &.{.{ .name = "dir/object.o", .data = "x" }}, &.{.{ .name = "sym", .object = 0 }}, false);
    defer gpa.free(archive);
    std.mem.writeInt(u32, archive[magic.len + header_len + 4 ..][0..4], 7, .big);
    try std.testing.expectError(error.UnknownIndexOffset, stripDirectories(gpa, archive, .linux));
}
