//! The import library of a DLL a fast dev link writes: a module definition
//! listing the DLL's exports, read from its export directory, which LLVM's
//! COFF import library writer turns into the library a linker reads to call
//! into the DLL (`builder.writeImportLibrary`).
const std = @import("std");
const HostPrelink = @import("HostPrelink.zig");

const Allocator = std.mem.Allocator;

/// Why a DLL's module definition cannot be read.
pub const Error = Allocator.Error || error{InvalidExecutable};

/// The module definition of the DLL `bytes`, named `dll_name`: its name and
/// each export by name, an export outside executable code marked `DATA`.
pub fn moduleDefinition(allocator: Allocator, dll_name: []const u8, bytes: []const u8) Error![]u8 {
    var parsed: HostPrelink.Executable.Parsed = undefined;
    const exe = try parsed.parse(bytes);
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    try out.print(allocator, "LIBRARY \"{s}\"\nEXPORTS\n", .{dll_name});
    const directory = exe.directories[export_directory];
    if (directory.size == 0) return try out.toOwnedSlice(allocator);
    const table = sliceAtRva(bytes, exe, directory.rva, export_directory_size) orelse return error.InvalidExecutable;
    const function_count = readU32(table, 20);
    const name_count = readU32(table, 24);
    const functions = sliceAtRva(bytes, exe, readU32(table, 28), @as(u64, function_count) * 4) orelse return error.InvalidExecutable;
    const names = sliceAtRva(bytes, exe, readU32(table, 32), @as(u64, name_count) * 4) orelse return error.InvalidExecutable;
    const ordinals = sliceAtRva(bytes, exe, readU32(table, 36), @as(u64, name_count) * 2) orelse return error.InvalidExecutable;
    for (0..name_count) |index| {
        const name_bytes = sliceAtRva(bytes, exe, readU32(names, index * 4), 1) orelse return error.InvalidExecutable;
        const name_start = @intFromPtr(name_bytes.ptr) - @intFromPtr(bytes.ptr);
        const name = std.mem.sliceTo(bytes[name_start..], 0);
        const ordinal = std.mem.readInt(u16, ordinals[index * 2 ..][0..2], .little);
        if (ordinal >= function_count) return error.InvalidExecutable;
        const rva = readU32(functions, @as(usize, ordinal) * 4);
        const code = if (sectionOf(exe, rva)) |section| section.characteristics & IMAGE_SCN_MEM_EXECUTE != 0 else return error.InvalidExecutable;
        try out.print(allocator, "  {s}{s}\n", .{ name, if (code) "" else " DATA" });
    }
    return try out.toOwnedSlice(allocator);
}

fn sectionOf(exe: HostPrelink.Executable, rva: u32) ?HostPrelink.Executable.Section {
    for (exe.sections) |section| {
        if (rva >= section.rva and rva - section.rva < @max(section.virtual_size, section.raw_size)) return section;
    }
    return null;
}

/// The `len` file bytes at `rva`, which lie in one section's raw data.
fn sliceAtRva(bytes: []const u8, exe: HostPrelink.Executable, rva: u32, len: u64) ?[]const u8 {
    const section = sectionOf(exe, rva) orelse return null;
    const within = rva - section.rva;
    if (within + len > section.raw_size) return null;
    const offset = @as(u64, section.raw_offset) + within;
    if (offset + len > bytes.len) return null;
    return bytes[@intCast(offset)..][0..@intCast(len)];
}

fn readU32(bytes: []const u8, at: usize) u32 {
    return std.mem.readInt(u32, bytes[at..][0..4], .little);
}

const export_directory = 0;
const export_directory_size = 40;
const IMAGE_SCN_MEM_EXECUTE: u32 = 0x2000_0000;

test "a DLL's module definition lists its exports, data marked" {
    const allocator = std.testing.allocator;
    // One code section holding `entry`, one data section holding `table`
    // and the export directory.
    const text: [16]u8 = @splat(0xcc);
    var data: [0x100]u8 = @splat(0);
    const data_rva: u32 = 0x2000;
    // The export directory at data + 0x10, its tables after it.
    const dir = data[0x10..][0..export_directory_size];
    std.mem.writeInt(u32, dir[20..24], 2, .little);
    std.mem.writeInt(u32, dir[24..28], 2, .little);
    std.mem.writeInt(u32, dir[28..32], data_rva + 0x40, .little);
    std.mem.writeInt(u32, dir[32..36], data_rva + 0x50, .little);
    std.mem.writeInt(u32, dir[36..40], data_rva + 0x60, .little);
    std.mem.writeInt(u32, data[0x40..0x44], 0x1000, .little);
    std.mem.writeInt(u32, data[0x44..0x48], data_rva + 0x80, .little);
    std.mem.writeInt(u32, data[0x50..0x54], data_rva + 0x70, .little);
    std.mem.writeInt(u32, data[0x54..0x58], data_rva + 0x78, .little);
    std.mem.writeInt(u16, data[0x60..0x62], 0, .little);
    std.mem.writeInt(u16, data[0x62..0x64], 1, .little);
    @memcpy(data[0x70..0x76], "entry\x00");
    @memcpy(data[0x78..0x7e], "table\x00");
    const dll = try HostPrelink.TestExecutable.encode(allocator, &.{
        .{ .name = ".text", .bytes = &text, .characteristics = IMAGE_SCN_MEM_EXECUTE },
        .{ .name = ".rdata", .bytes = &data, .characteristics = 0 },
    }, &.{});
    defer allocator.free(dll);
    // The export directory entry: the PE header at 0x80, the optional
    // header after the 4-byte signature and the 20-byte file header.
    const directories = 0x80 + 4 + 20 + 112;
    std.mem.writeInt(u32, dll[directories..][0..4], data_rva + 0x10, .little);
    std.mem.writeInt(u32, dll[directories + 4 ..][0..4], export_directory_size, .little);
    const def = try moduleDefinition(allocator, "app.dll", dll);
    defer allocator.free(def);
    try std.testing.expectEqualStrings("LIBRARY \"app.dll\"\nEXPORTS\n  entry\n  table DATA\n", def);
}
