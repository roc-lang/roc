//! Stack probe support for Windows linking.
//!
//! When linking Zig-compiled code (like platform hosts) on Windows using lld-link,
//! we need to provide the ___chkstk_ms function which is used for stack probing
//! on functions with large stack frames.
//!
//! This module generates a minimal COFF object file containing the ___chkstk_ms
//! implementation for x86_64 Windows.

const std = @import("std");

/// x86_64 machine code for ___chkstk_ms (stack probe without SP adjustment)
/// This probes stack pages in 4KB increments to ensure they are committed.
///
/// Assembly (AT&T syntax):
///     pushq  %rcx
///     pushq  %rax
///     cmpq   $0x1000, %rax
///     leaq   24(%rsp), %rcx
///     jb     1f
/// 2:
///     subq   $0x1000, %rcx
///     testq  %rcx, (%rcx)
///     subq   $0x1000, %rax
///     cmpq   $0x1000, %rax
///     ja     2b
/// 1:
///     subq   %rax, %rcx
///     testq  %rcx, (%rcx)
///     popq   %rax
///     popq   %rcx
///     retq
const CHKSTK_MS_CODE = [_]u8{
    0x51, // push rcx
    0x50, // push rax
    0x48, 0x3d, 0x00, 0x10, 0x00, 0x00, // cmp rax, 0x1000
    0x48, 0x8d, 0x4c, 0x24, 0x18, // lea rcx, [rsp+24]
    0x72, 0x18, // jb 1f (+24 bytes)
    // 2:
    0x48, 0x81, 0xe9, 0x00, 0x10, 0x00, 0x00, // sub rcx, 0x1000
    0x48, 0x85, 0x09, // test [rcx], rcx
    0x48, 0x2d, 0x00, 0x10, 0x00, 0x00, // sub rax, 0x1000
    0x48, 0x3d, 0x00, 0x10, 0x00, 0x00, // cmp rax, 0x1000
    0x77, 0xe8, // ja 2b (-24 bytes)
    // 1:
    0x48, 0x29, 0xc1, // sub rcx, rax
    0x48, 0x85, 0x09, // test [rcx], rcx
    0x58, // pop rax
    0x59, // pop rcx
    0xc3, // ret
};

/// COFF constants
const COFF = struct {
    const IMAGE_FILE_MACHINE_AMD64: u16 = 0x8664;
    const IMAGE_SCN_CNT_CODE: u32 = 0x00000020;
    const IMAGE_SCN_MEM_EXECUTE: u32 = 0x20000000;
    const IMAGE_SCN_MEM_READ: u32 = 0x40000000;
    const IMAGE_SCN_ALIGN_16BYTES: u32 = 0x00500000;
    const IMAGE_SYM_CLASS_EXTERNAL: u8 = 2;
    const IMAGE_SYM_DTYPE_FUNCTION: u16 = 0x20;
};

/// Generate a minimal COFF object file containing ___chkstk_ms for x86_64 Windows.
/// Returns the object file bytes.
pub fn generateStackProbeObject(allocator: std.mem.Allocator) ![]u8 {
    var output: std.ArrayList(u8) = .{};
    errdefer output.deinit(allocator);

    const symbol_name = "___chkstk_ms";
    const code_size: u32 = CHKSTK_MS_CODE.len;

    // Layout:
    // - COFF Header (20 bytes)
    // - Section Header (40 bytes)
    // - .text section content (code_size bytes)
    // - Symbol table (18 bytes)
    // - String table (4 + symbol_name.len + 1 bytes)

    const header_size: u32 = 20;
    const section_header_size: u32 = 40;
    const text_offset: u32 = header_size + section_header_size;
    const symtab_offset: u32 = text_offset + code_size;
    const strtab_size: u32 = 4 + @as(u32, @intCast(symbol_name.len)) + 1;

    // Helper to write little-endian integers
    const writeU16 = struct {
        fn f(out: *std.ArrayList(u8), alloc: std.mem.Allocator, val: u16) !void {
            var buf: [2]u8 = undefined;
            std.mem.writeInt(u16, &buf, val, .little);
            try out.appendSlice(alloc, &buf);
        }
    }.f;
    const writeU32 = struct {
        fn f(out: *std.ArrayList(u8), alloc: std.mem.Allocator, val: u32) !void {
            var buf: [4]u8 = undefined;
            std.mem.writeInt(u32, &buf, val, .little);
            try out.appendSlice(alloc, &buf);
        }
    }.f;
    const writeI16 = struct {
        fn f(out: *std.ArrayList(u8), alloc: std.mem.Allocator, val: i16) !void {
            var buf: [2]u8 = undefined;
            std.mem.writeInt(i16, &buf, val, .little);
            try out.appendSlice(alloc, &buf);
        }
    }.f;

    // COFF Header
    try writeU16(&output, allocator, COFF.IMAGE_FILE_MACHINE_AMD64); // Machine
    try writeU16(&output, allocator, 1); // NumberOfSections
    try writeU32(&output, allocator, 0); // TimeDateStamp
    try writeU32(&output, allocator, symtab_offset); // PointerToSymbolTable
    try writeU32(&output, allocator, 1); // NumberOfSymbols
    try writeU16(&output, allocator, 0); // SizeOfOptionalHeader
    try writeU16(&output, allocator, 0); // Characteristics

    // Section Header (.text)
    var sect_name: [8]u8 = std.mem.zeroes([8]u8);
    @memcpy(sect_name[0..5], ".text");
    try output.appendSlice(allocator, &sect_name); // Name
    try writeU32(&output, allocator, 0); // VirtualSize
    try writeU32(&output, allocator, 0); // VirtualAddress
    try writeU32(&output, allocator, code_size); // SizeOfRawData
    try writeU32(&output, allocator, text_offset); // PointerToRawData
    try writeU32(&output, allocator, 0); // PointerToRelocations
    try writeU32(&output, allocator, 0); // PointerToLinenumbers
    try writeU16(&output, allocator, 0); // NumberOfRelocations
    try writeU16(&output, allocator, 0); // NumberOfLinenumbers
    try writeU32(&output, allocator, COFF.IMAGE_SCN_CNT_CODE | COFF.IMAGE_SCN_MEM_EXECUTE | COFF.IMAGE_SCN_MEM_READ | COFF.IMAGE_SCN_ALIGN_16BYTES); // Characteristics

    // .text section content
    try output.appendSlice(allocator, &CHKSTK_MS_CODE);

    // Symbol table entry (18 bytes)
    // Name field: use string table (name > 8 chars)
    try writeU32(&output, allocator, 0); // Zeroes (indicates long name)
    try writeU32(&output, allocator, 4); // Offset into string table
    try writeU32(&output, allocator, 0); // Value (offset in section)
    try writeI16(&output, allocator, 1); // SectionNumber (1 = .text)
    try writeU16(&output, allocator, COFF.IMAGE_SYM_DTYPE_FUNCTION); // Type
    try output.append(allocator, COFF.IMAGE_SYM_CLASS_EXTERNAL); // StorageClass
    try output.append(allocator, 0); // NumberOfAuxSymbols

    // String table
    try writeU32(&output, allocator, strtab_size); // Size
    try output.appendSlice(allocator, symbol_name); // Symbol name
    try output.append(allocator, 0); // Null terminator

    return output.toOwnedSlice(allocator);
}

/// Write the stack probe object file to a path.
pub fn writeStackProbeObject(allocator: std.mem.Allocator, path: []const u8) !void {
    const obj_bytes = try generateStackProbeObject(allocator);
    defer allocator.free(obj_bytes);

    try std.fs.cwd().writeFile(.{
        .sub_path = path,
        .data = obj_bytes,
    });
}

test "generate stack probe object" {
    const obj_bytes = try generateStackProbeObject(std.testing.allocator);
    defer std.testing.allocator.free(obj_bytes);

    // Verify COFF magic (machine type)
    const machine = std.mem.readInt(u16, obj_bytes[0..2], .little);
    try std.testing.expectEqual(COFF.IMAGE_FILE_MACHINE_AMD64, machine);

    // Verify we have content
    try std.testing.expect(obj_bytes.len > 60);
}
