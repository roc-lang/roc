//! Object File Reader for extracting executable code sections.
//!
//! This module provides simple readers for ELF, Mach-O, and COFF object files
//! to extract the .text section containing executable machine code.
//!
//! This is used by the LLVM backend to extract compiled code from object files
//! for JIT execution, providing a unified execution path with the dev backend.

const std = @import("std");

/// Errors that can occur during object file parsing.
pub const Error = error{
    InvalidObjectFile,
    TextSectionNotFound,
    UnsupportedFormat,
};

/// Extract the executable code section (.text) from an object file.
/// Returns a slice pointing into the object_bytes buffer.
pub fn extractCodeSection(object_bytes: []const u8) Error![]const u8 {
    if (object_bytes.len < 4) {
        return Error.InvalidObjectFile;
    }

    // Detect format by magic number
    const magic = object_bytes[0..4];

    // ELF magic: 0x7F 'E' 'L' 'F'
    if (magic[0] == 0x7F and magic[1] == 'E' and magic[2] == 'L' and magic[3] == 'F') {
        return extractElfTextSection(object_bytes);
    }

    // Mach-O 64-bit magic: 0xFEEDFACF (little-endian)
    if (magic[0] == 0xCF and magic[1] == 0xFA and magic[2] == 0xED and magic[3] == 0xFE) {
        return extractMachOTextSection(object_bytes);
    }

    // COFF: Check for valid machine types (no magic, but machine field is at offset 0)
    // x86_64: 0x8664, i386: 0x14C, ARM64: 0xAA64
    const machine = std.mem.readInt(u16, magic[0..2], .little);
    if (machine == 0x8664 or machine == 0x14C or machine == 0xAA64) {
        return extractCoffTextSection(object_bytes);
    }

    return Error.UnsupportedFormat;
}

//
// ELF Parser
//

fn extractElfTextSection(bytes: []const u8) Error![]const u8 {
    // ELF64 header structure (we only support 64-bit)
    if (bytes.len < 64) return Error.InvalidObjectFile;

    // Verify 64-bit ELF (e_ident[4] == 2)
    if (bytes[4] != 2) return Error.UnsupportedFormat;

    // Read relevant header fields
    const e_shoff = std.mem.readInt(u64, bytes[40..48], .little); // Section header offset
    const e_shentsize = std.mem.readInt(u16, bytes[58..60], .little); // Section header entry size
    const e_shnum = std.mem.readInt(u16, bytes[60..62], .little); // Number of section headers
    const e_shstrndx = std.mem.readInt(u16, bytes[62..64], .little); // Section name string table index

    if (e_shoff == 0 or e_shnum == 0) return Error.TextSectionNotFound;
    if (e_shoff + @as(u64, e_shnum) * e_shentsize > bytes.len) return Error.InvalidObjectFile;

    // Get the section name string table
    const shstrtab_offset = e_shoff + @as(u64, e_shstrndx) * e_shentsize;
    if (shstrtab_offset + 64 > bytes.len) return Error.InvalidObjectFile;
    const shstrtab_hdr = bytes[@intCast(shstrtab_offset)..];
    const strtab_off = std.mem.readInt(u64, shstrtab_hdr[24..32], .little);
    const strtab_size = std.mem.readInt(u64, shstrtab_hdr[32..40], .little);

    if (strtab_off + strtab_size > bytes.len) return Error.InvalidObjectFile;
    const strtab = bytes[@intCast(strtab_off)..@intCast(strtab_off + strtab_size)];

    // Search for .text section
    var i: u16 = 0;
    while (i < e_shnum) : (i += 1) {
        const sh_offset = e_shoff + @as(u64, i) * e_shentsize;
        if (sh_offset + 64 > bytes.len) continue;

        const sh = bytes[@intCast(sh_offset)..];
        const sh_name = std.mem.readInt(u32, sh[0..4], .little);

        // Get section name from string table
        if (sh_name < strtab.len) {
            const name_start = strtab[sh_name..];
            const name_end = std.mem.indexOfScalar(u8, name_start, 0) orelse name_start.len;
            const name = name_start[0..name_end];

            // Look for .text section or .text.* sections (when function_sections is enabled)
            // Prefer .text._roc_eval or .text.roc_eval over other .text.* sections
            if (std.mem.eql(u8, name, ".text._roc_eval") or std.mem.eql(u8, name, ".text.roc_eval")) {
                const sec_offset = std.mem.readInt(u64, sh[24..32], .little);
                const sec_size = std.mem.readInt(u64, sh[32..40], .little);

                if (sec_offset + sec_size > bytes.len) return Error.InvalidObjectFile;
                return bytes[@intCast(sec_offset)..@intCast(sec_offset + sec_size)];
            }
        }
    }

    // Second pass: fall back to .text if no roc_eval section found
    i = 0;
    while (i < e_shnum) : (i += 1) {
        const sh_offset = e_shoff + @as(u64, i) * e_shentsize;
        if (sh_offset + 64 > bytes.len) continue;

        const sh = bytes[@intCast(sh_offset)..];
        const sh_name = std.mem.readInt(u32, sh[0..4], .little);

        if (sh_name < strtab.len) {
            const name_start = strtab[sh_name..];
            const name_end = std.mem.indexOfScalar(u8, name_start, 0) orelse name_start.len;
            const name = name_start[0..name_end];

            if (std.mem.eql(u8, name, ".text")) {
                const sec_offset = std.mem.readInt(u64, sh[24..32], .little);
                const sec_size = std.mem.readInt(u64, sh[32..40], .little);

                if (sec_size > 0 and sec_offset + sec_size <= bytes.len) {
                    return bytes[@intCast(sec_offset)..@intCast(sec_offset + sec_size)];
                }
            }
        }
    }

    return Error.TextSectionNotFound;
}

//
// Mach-O Parser
//

fn extractMachOTextSection(bytes: []const u8) Error![]const u8 {
    // Mach-O 64-bit header is 32 bytes
    if (bytes.len < 32) return Error.InvalidObjectFile;

    const ncmds = std.mem.readInt(u32, bytes[16..20], .little); // Number of load commands
    const sizeofcmds = std.mem.readInt(u32, bytes[20..24], .little); // Size of load commands

    if (32 + sizeofcmds > bytes.len) return Error.InvalidObjectFile;

    // Iterate through load commands
    var offset: usize = 32; // Start after header
    var cmd_idx: u32 = 0;
    while (cmd_idx < ncmds and offset + 8 <= bytes.len) : (cmd_idx += 1) {
        const cmd = std.mem.readInt(u32, bytes[offset..][0..4], .little);
        const cmdsize = std.mem.readInt(u32, bytes[offset + 4 ..][0..4], .little);

        if (cmdsize < 8 or offset + cmdsize > bytes.len) return Error.InvalidObjectFile;

        // LC_SEGMENT_64 = 0x19
        if (cmd == 0x19) {
            // Segment command: 72 bytes header + sections
            if (cmdsize < 72) {
                offset += cmdsize;
                continue;
            }

            const segname = bytes[offset + 8 .. offset + 24];
            const nsects = std.mem.readInt(u32, bytes[offset + 64 ..][0..4], .little);

            // Check if this is __TEXT segment
            const is_text_segment = std.mem.startsWith(u8, segname, "__TEXT");

            // Iterate sections in this segment
            var sect_offset = offset + 72; // After segment_command_64 header
            var sect_idx: u32 = 0;
            while (sect_idx < nsects and sect_offset + 80 <= bytes.len) : (sect_idx += 1) {
                const sectname = bytes[sect_offset .. sect_offset + 16];

                // Look for __text section in __TEXT segment
                if (is_text_segment and std.mem.startsWith(u8, sectname, "__text")) {
                    const sec_offset = std.mem.readInt(u32, bytes[sect_offset + 48 ..][0..4], .little);
                    const sec_size = std.mem.readInt(u64, bytes[sect_offset + 40 ..][0..8], .little);

                    if (@as(u64, sec_offset) + sec_size > bytes.len) return Error.InvalidObjectFile;
                    return bytes[sec_offset..@intCast(@as(u64, sec_offset) + sec_size)];
                }

                sect_offset += 80; // sizeof(section_64)
            }
        }

        offset += cmdsize;
    }

    return Error.TextSectionNotFound;
}

//
// COFF Parser
//

fn extractCoffTextSection(bytes: []const u8) Error![]const u8 {
    // COFF header is 20 bytes
    if (bytes.len < 20) return Error.InvalidObjectFile;

    const num_sections = std.mem.readInt(u16, bytes[2..4], .little);
    const optional_header_size = std.mem.readInt(u16, bytes[16..18], .little);

    // Section headers start after COFF header + optional header
    const section_table_offset: usize = 20 + optional_header_size;

    if (section_table_offset + @as(usize, num_sections) * 40 > bytes.len) {
        return Error.InvalidObjectFile;
    }

    // Each section header is 40 bytes
    var i: u16 = 0;
    while (i < num_sections) : (i += 1) {
        const sh_offset = section_table_offset + @as(usize, i) * 40;
        const sh = bytes[sh_offset..];

        // Section name is first 8 bytes (null-padded)
        const name = sh[0..8];
        const name_end = std.mem.indexOfScalar(u8, name, 0) orelse 8;

        if (std.mem.eql(u8, name[0..name_end], ".text")) {
            const sec_size = std.mem.readInt(u32, sh[16..20], .little);
            const sec_offset = std.mem.readInt(u32, sh[20..24], .little);

            if (@as(usize, sec_offset) + sec_size > bytes.len) return Error.InvalidObjectFile;
            return bytes[sec_offset .. sec_offset + sec_size];
        }
    }

    return Error.TextSectionNotFound;
}

//
// Tests
//

test "detect ELF magic" {
    // Minimal ELF header (just magic)
    const elf_bytes = [_]u8{ 0x7F, 'E', 'L', 'F', 2, 1, 1, 0 } ++ [_]u8{0} ** 56;
    const result = extractCodeSection(&elf_bytes);
    // Should fail to find .text but not fail on format detection
    try std.testing.expectError(Error.TextSectionNotFound, result);
}

test "detect Mach-O magic" {
    // Minimal Mach-O 64-bit header
    var macho_bytes = [_]u8{0} ** 64;
    macho_bytes[0] = 0xCF;
    macho_bytes[1] = 0xFA;
    macho_bytes[2] = 0xED;
    macho_bytes[3] = 0xFE;
    const result = extractCodeSection(&macho_bytes);
    try std.testing.expectError(Error.TextSectionNotFound, result);
}

test "detect COFF by machine type" {
    // Minimal COFF header for x86_64
    var coff_bytes = [_]u8{0} ** 64;
    coff_bytes[0] = 0x64; // Machine type low byte
    coff_bytes[1] = 0x86; // Machine type high byte (0x8664 = x86_64)
    const result = extractCodeSection(&coff_bytes);
    try std.testing.expectError(Error.TextSectionNotFound, result);
}

test "reject unknown format" {
    const unknown_bytes = [_]u8{ 0x00, 0x00, 0x00, 0x00 } ++ [_]u8{0} ** 60;
    const result = extractCodeSection(&unknown_bytes);
    try std.testing.expectError(Error.UnsupportedFormat, result);
}
