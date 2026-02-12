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

/// Result of extracting code with entry point information.
pub const CodeWithEntry = struct {
    code: []const u8,
    entry_offset: usize,
};

/// Extract the .text section and find the roc_eval entry point offset.
pub fn extractCodeSectionWithEntry(object_bytes: []const u8) Error!CodeWithEntry {
    const code = try extractCodeSection(object_bytes);
    const entry_offset = findRocEvalOffset(object_bytes, code) catch 0;
    return .{ .code = code, .entry_offset = entry_offset };
}

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
// Symbol Table Lookup
//

/// Find the offset of roc_eval within the extracted code section.
/// Falls back to 0 if the symbol is not found.
fn findRocEvalOffset(object_bytes: []const u8, code: []const u8) Error!usize {
    if (object_bytes.len < 4) return Error.InvalidObjectFile;

    const magic = object_bytes[0..4];

    // ELF
    if (magic[0] == 0x7F and magic[1] == 'E' and magic[2] == 'L' and magic[3] == 'F') {
        return findRocEvalOffsetElf(object_bytes, code);
    }

    // Mach-O 64-bit
    if (magic[0] == 0xCF and magic[1] == 0xFA and magic[2] == 0xED and magic[3] == 0xFE) {
        return findRocEvalOffsetMachO(object_bytes, code);
    }

    // COFF
    const machine = std.mem.readInt(u16, magic[0..2], .little);
    if (machine == 0x8664 or machine == 0x14C or machine == 0xAA64) {
        return findRocEvalOffsetCoff(object_bytes);
    }

    return 0;
}

/// Find roc_eval offset in an ELF object file.
/// Parses .symtab section and its associated string table.
fn findRocEvalOffsetElf(bytes: []const u8, code: []const u8) Error!usize {
    if (bytes.len < 64) return 0;
    if (bytes[4] != 2) return 0; // Only 64-bit ELF

    const e_shoff = std.mem.readInt(u64, bytes[40..48], .little);
    const e_shentsize = std.mem.readInt(u16, bytes[58..60], .little);
    const e_shnum = std.mem.readInt(u16, bytes[60..62], .little);

    if (e_shoff == 0 or e_shnum == 0) return 0;

    // Find the .text section address for offset calculation
    const e_shstrndx = std.mem.readInt(u16, bytes[62..64], .little);
    const shstrtab_offset = e_shoff + @as(u64, e_shstrndx) * e_shentsize;
    if (shstrtab_offset + 64 > bytes.len) return 0;
    const shstrtab_hdr = bytes[@intCast(shstrtab_offset)..];
    const shstrtab_off = std.mem.readInt(u64, shstrtab_hdr[24..32], .little);
    const shstrtab_size = std.mem.readInt(u64, shstrtab_hdr[32..40], .little);
    if (shstrtab_off + shstrtab_size > bytes.len) return 0;
    const shstrtab = bytes[@intCast(shstrtab_off)..@intCast(shstrtab_off + shstrtab_size)];

    // Find the address of the code section (the .text section we extracted)
    const code_start = @intFromPtr(code.ptr) - @intFromPtr(bytes.ptr);
    var text_section_addr: u64 = 0;

    // Find SHT_SYMTAB (type=2) and its associated strtab
    var symtab_off: u64 = 0;
    var symtab_size: u64 = 0;
    var symtab_link: u32 = 0;

    var i: u16 = 0;
    while (i < e_shnum) : (i += 1) {
        const sh_offset = e_shoff + @as(u64, i) * e_shentsize;
        if (sh_offset + 64 > bytes.len) continue;
        const sh = bytes[@intCast(sh_offset)..];
        const sh_type = std.mem.readInt(u32, sh[4..8], .little);
        const sh_name_idx = std.mem.readInt(u32, sh[0..4], .little);

        // Check if this is the .text section that corresponds to our code
        if (sh_name_idx < shstrtab.len) {
            const name_start = shstrtab[sh_name_idx..];
            const name_end = std.mem.indexOfScalar(u8, name_start, 0) orelse name_start.len;
            const name = name_start[0..name_end];
            const sec_offset = std.mem.readInt(u64, sh[24..32], .little);
            if (sec_offset == code_start) {
                text_section_addr = std.mem.readInt(u64, sh[16..24], .little);
            }
            // Also check by name if we have .text
            if (std.mem.eql(u8, name, ".text") or
                std.mem.eql(u8, name, ".text._roc_eval") or
                std.mem.eql(u8, name, ".text.roc_eval"))
            {
                if (text_section_addr == 0) {
                    text_section_addr = std.mem.readInt(u64, sh[16..24], .little);
                }
            }
        }

        if (sh_type == 2) { // SHT_SYMTAB
            symtab_off = std.mem.readInt(u64, sh[24..32], .little);
            symtab_size = std.mem.readInt(u64, sh[32..40], .little);
            symtab_link = std.mem.readInt(u32, sh[40..44], .little);
        }
    }

    if (symtab_off == 0 or symtab_size == 0) return 0;

    // Get the associated string table
    const strtab_sh_offset = e_shoff + @as(u64, symtab_link) * e_shentsize;
    if (strtab_sh_offset + 64 > bytes.len) return 0;
    const strtab_sh = bytes[@intCast(strtab_sh_offset)..];
    const strtab_off = std.mem.readInt(u64, strtab_sh[24..32], .little);
    const strtab_size = std.mem.readInt(u64, strtab_sh[32..40], .little);
    if (strtab_off + strtab_size > bytes.len) return 0;
    const strtab = bytes[@intCast(strtab_off)..@intCast(strtab_off + strtab_size)];

    // Each Elf64_Sym is 24 bytes: st_name(4), st_info(1), st_other(1), st_shndx(2), st_value(8), st_size(8)
    const sym_count = symtab_size / 24;
    var sym_i: u64 = 0;
    while (sym_i < sym_count) : (sym_i += 1) {
        const sym_offset = symtab_off + sym_i * 24;
        if (sym_offset + 24 > bytes.len) continue;
        const sym = bytes[@intCast(sym_offset)..];
        const st_name = std.mem.readInt(u32, sym[0..4], .little);
        const st_value = std.mem.readInt(u64, sym[8..16], .little);

        if (st_name < strtab.len) {
            const name_start = strtab[st_name..];
            const name_end = std.mem.indexOfScalar(u8, name_start, 0) orelse name_start.len;
            const name = name_start[0..name_end];
            if (std.mem.eql(u8, name, "roc_eval") or std.mem.eql(u8, name, "_roc_eval")) {
                // Compute offset within the extracted code
                if (st_value >= text_section_addr) {
                    return @intCast(st_value - text_section_addr);
                }
                return @intCast(st_value);
            }
        }
    }

    return 0;
}

/// Find roc_eval offset in a Mach-O object file.
/// Parses LC_SYMTAB load command to find symbol table and string table.
fn findRocEvalOffsetMachO(bytes: []const u8, code: []const u8) Error!usize {
    if (bytes.len < 32) return 0;

    const ncmds = std.mem.readInt(u32, bytes[16..20], .little);
    const sizeofcmds = std.mem.readInt(u32, bytes[20..24], .little);
    if (32 + sizeofcmds > bytes.len) return 0;

    // Find __text section address
    var text_section_addr: u64 = 0;
    const code_file_offset = @intFromPtr(code.ptr) - @intFromPtr(bytes.ptr);

    // First pass: find LC_SEGMENT_64 to get __text section address
    var offset: usize = 32;
    var cmd_idx: u32 = 0;
    while (cmd_idx < ncmds and offset + 8 <= bytes.len) : (cmd_idx += 1) {
        const cmd = std.mem.readInt(u32, bytes[offset..][0..4], .little);
        const cmdsize = std.mem.readInt(u32, bytes[offset + 4 ..][0..4], .little);
        if (cmdsize < 8 or offset + cmdsize > bytes.len) break;

        if (cmd == 0x19) { // LC_SEGMENT_64
            if (cmdsize >= 72) {
                const nsects = std.mem.readInt(u32, bytes[offset + 64 ..][0..4], .little);
                var sect_offset = offset + 72;
                var sect_idx: u32 = 0;
                while (sect_idx < nsects and sect_offset + 80 <= bytes.len) : (sect_idx += 1) {
                    const sectname = bytes[sect_offset .. sect_offset + 16];
                    if (std.mem.startsWith(u8, sectname, "__text")) {
                        text_section_addr = std.mem.readInt(u64, bytes[sect_offset + 32 ..][0..8], .little);
                        _ = code_file_offset;
                    }
                    sect_offset += 80;
                }
            }
        }
        offset += cmdsize;
    }

    // Second pass: find LC_SYMTAB
    offset = 32;
    cmd_idx = 0;
    while (cmd_idx < ncmds and offset + 8 <= bytes.len) : (cmd_idx += 1) {
        const cmd = std.mem.readInt(u32, bytes[offset..][0..4], .little);
        const cmdsize = std.mem.readInt(u32, bytes[offset + 4 ..][0..4], .little);
        if (cmdsize < 8 or offset + cmdsize > bytes.len) break;

        if (cmd == 0x02) { // LC_SYMTAB
            if (cmdsize >= 24) {
                const symoff = std.mem.readInt(u32, bytes[offset + 8 ..][0..4], .little);
                const nsyms = std.mem.readInt(u32, bytes[offset + 12 ..][0..4], .little);
                const stroff = std.mem.readInt(u32, bytes[offset + 16 ..][0..4], .little);
                const strsize = std.mem.readInt(u32, bytes[offset + 20 ..][0..4], .little);

                if (@as(u64, stroff) + strsize > bytes.len) return 0;
                const strtab = bytes[stroff..@intCast(@as(u64, stroff) + strsize)];

                // Each nlist_64 is 16 bytes: n_strx(4), n_type(1), n_sect(1), n_desc(2), n_value(8)
                var sym_i: u32 = 0;
                while (sym_i < nsyms) : (sym_i += 1) {
                    const sym_offset = @as(u64, symoff) + @as(u64, sym_i) * 16;
                    if (sym_offset + 16 > bytes.len) continue;
                    const sym = bytes[@intCast(sym_offset)..];
                    const n_strx = std.mem.readInt(u32, sym[0..4], .little);
                    const n_value = std.mem.readInt(u64, sym[8..16], .little);

                    if (n_strx < strtab.len) {
                        const name_start = strtab[n_strx..];
                        const name_end = std.mem.indexOfScalar(u8, name_start, 0) orelse name_start.len;
                        const name = name_start[0..name_end];
                        if (std.mem.eql(u8, name, "_roc_eval") or std.mem.eql(u8, name, "roc_eval")) {
                            if (text_section_addr > 0 and n_value >= text_section_addr) {
                                return @intCast(n_value - text_section_addr);
                            }
                            return @intCast(n_value);
                        }
                    }
                }
            }
        }
        offset += cmdsize;
    }

    return 0;
}

/// Find roc_eval offset in a COFF object file.
/// Parses the COFF symbol table at the end of the file.
fn findRocEvalOffsetCoff(bytes: []const u8) Error!usize {
    if (bytes.len < 20) return 0;

    // COFF header: symbol table pointer is at offset 8, count at offset 12
    const sym_table_offset = std.mem.readInt(u32, bytes[8..12], .little);
    const num_symbols = std.mem.readInt(u32, bytes[12..16], .little);

    if (sym_table_offset == 0 or num_symbols == 0) return 0;

    // String table is right after symbol table. Each symbol is 18 bytes.
    const strtab_offset = @as(u64, sym_table_offset) + @as(u64, num_symbols) * 18;

    // Each COFF symbol is 18 bytes: Name(8), Value(4), SectionNumber(2), Type(2), StorageClass(1), NumberOfAuxSymbols(1)
    var sym_i: u32 = 0;
    while (sym_i < num_symbols) : (sym_i += 1) {
        const sym_offset = @as(u64, sym_table_offset) + @as(u64, sym_i) * 18;
        if (sym_offset + 18 > bytes.len) continue;
        const sym = bytes[@intCast(sym_offset)..];

        // Get symbol name (first 8 bytes)
        // If first 4 bytes are zero, it's a long name stored in string table
        const name_zeros = std.mem.readInt(u32, sym[0..4], .little);
        var name: []const u8 = undefined;
        if (name_zeros == 0) {
            // Long name: offset into string table
            const str_offset = std.mem.readInt(u32, sym[4..8], .little);
            const abs_offset = strtab_offset + str_offset;
            if (abs_offset < bytes.len) {
                const name_start = bytes[@intCast(abs_offset)..];
                const name_end = std.mem.indexOfScalar(u8, name_start, 0) orelse name_start.len;
                name = name_start[0..name_end];
            } else {
                continue;
            }
        } else {
            // Short name: inline in the 8-byte field
            const name_end = std.mem.indexOfScalar(u8, sym[0..8], 0) orelse 8;
            name = sym[0..name_end];
        }

        if (std.mem.eql(u8, name, "roc_eval") or std.mem.eql(u8, name, "_roc_eval")) {
            const value = std.mem.readInt(u32, sym[8..12], .little);
            return @intCast(value);
        }

        // Skip auxiliary symbols
        const num_aux = sym[17];
        sym_i += num_aux;
    }

    return 0;
}

//
// ELF Relocation Support
//
// The LLVM backend links builtins.bc into the generated module. After compilation
// to an ELF object file, there are unresolved relocations for function calls and
// data references. This function extracts all loadable sections, lays them out
// contiguously, and applies all aarch64 ELF relocations so the code can execute.
//

const Allocator = std.mem.Allocator;
const builtin = @import("builtin");

/// Result of extracting and relocating ELF code.
pub const RelocatedCode = struct {
    /// The relocated code buffer (caller owns, allocated with provided allocator)
    code: []u8,
    /// Offset within code where roc_eval entry point is
    entry_offset: usize,
};

/// Extract all loadable sections from an ELF object file, lay them out
/// in a contiguous buffer, apply all aarch64 relocations, and return
/// the result ready for execution.
pub fn extractAndRelocateElf(allocator: Allocator, object_bytes: []const u8) !RelocatedCode {
    if (object_bytes.len < 64) return error.InvalidObjectFile;
    if (object_bytes[0] != 0x7F or object_bytes[1] != 'E' or object_bytes[2] != 'L' or object_bytes[3] != 'F')
        return error.UnsupportedFormat;
    if (object_bytes[4] != 2) return error.UnsupportedFormat; // 64-bit only

    const e_shoff = std.mem.readInt(u64, object_bytes[40..48], .little);
    const e_shentsize = std.mem.readInt(u16, object_bytes[58..60], .little);
    const e_shnum = std.mem.readInt(u16, object_bytes[60..62], .little);
    const e_shstrndx = std.mem.readInt(u16, object_bytes[62..64], .little);

    if (e_shoff == 0 or e_shnum == 0) return error.TextSectionNotFound;

    // Get the section name string table
    const shstrtab = getSectionData(object_bytes, e_shoff, e_shentsize, e_shstrndx) orelse return error.InvalidObjectFile;

    // --- Phase 1: Identify all loadable sections and compute layout ---

    const SectionInfo = struct {
        index: u16,
        file_offset: u64,
        size: u64,
        alignment: u64,
        buf_offset: u64, // offset in our output buffer
        is_nobits: bool, // .bss - no file data, just zero
    };

    var sections: [64]SectionInfo = undefined;
    var section_count: usize = 0;
    var text_section_idx: ?u16 = null;

    var i: u16 = 0;
    while (i < e_shnum) : (i += 1) {
        const sh = getShdr(object_bytes, e_shoff, e_shentsize, i) orelse continue;
        const sh_type = std.mem.readInt(u32, sh[4..8], .little);
        const sh_flags = std.mem.readInt(u64, sh[8..16], .little);
        const sh_offset = std.mem.readInt(u64, sh[24..32], .little);
        const sh_size = std.mem.readInt(u64, sh[32..40], .little);
        const sh_addralign = std.mem.readInt(u64, sh[48..56], .little);

        // SHF_ALLOC (0x2) = section occupies memory during execution
        if (sh_flags & 0x2 == 0) continue;
        if (sh_size == 0) continue;

        // Track the .text section
        const sh_name_idx = std.mem.readInt(u32, sh[0..4], .little);
        if (sh_name_idx < shstrtab.len) {
            const name = getSectionName(shstrtab, sh_name_idx);
            if (std.mem.eql(u8, name, ".text")) {
                text_section_idx = i;
            }
        }

        if (section_count >= sections.len) continue; // safety limit
        sections[section_count] = .{
            .index = i,
            .file_offset = sh_offset,
            .size = sh_size,
            .alignment = if (sh_addralign > 0) sh_addralign else 1,
            .buf_offset = 0, // computed below
            .is_nobits = (sh_type == 8), // SHT_NOBITS
        };
        section_count += 1;
    }

    if (text_section_idx == null) return error.TextSectionNotFound;

    // Compute layout: place sections sequentially with proper alignment
    var total_size: u64 = 0;
    for (sections[0..section_count]) |*sec| {
        total_size = std.mem.alignForward(u64, total_size, sec.alignment);
        sec.buf_offset = total_size;
        total_size += sec.size;
    }

    // --- Phase 2: Allocate buffer and copy sections ---

    const buf = try allocator.alloc(u8, @intCast(total_size));
    errdefer allocator.free(buf);
    @memset(buf, 0); // zero-fill (.bss and any padding)

    for (sections[0..section_count]) |sec| {
        if (sec.is_nobits) continue;
        if (sec.file_offset + sec.size > object_bytes.len) continue;
        const dest = buf[@intCast(sec.buf_offset)..@intCast(sec.buf_offset + sec.size)];
        const src = object_bytes[@intCast(sec.file_offset)..@intCast(sec.file_offset + sec.size)];
        @memcpy(dest, src);
    }

    // Build section index → buffer offset map
    var sec_buf_offsets: [256]u64 = [_]u64{0} ** 256;
    for (sections[0..section_count]) |sec| {
        if (sec.index < sec_buf_offsets.len) {
            sec_buf_offsets[sec.index] = sec.buf_offset;
        }
    }

    // --- Phase 3: Parse symbol table ---

    var symtab_off: u64 = 0;
    var symtab_size: u64 = 0;
    var symtab_link: u32 = 0;

    i = 0;
    while (i < e_shnum) : (i += 1) {
        const sh = getShdr(object_bytes, e_shoff, e_shentsize, i) orelse continue;
        const sh_type = std.mem.readInt(u32, sh[4..8], .little);
        if (sh_type == 2) { // SHT_SYMTAB
            symtab_off = std.mem.readInt(u64, sh[24..32], .little);
            symtab_size = std.mem.readInt(u64, sh[32..40], .little);
            symtab_link = std.mem.readInt(u32, sh[40..44], .little);
            break;
        }
    }

    // Get the symbol string table
    const sym_strtab = getSectionData(object_bytes, e_shoff, e_shentsize, @intCast(symtab_link)) orelse return error.InvalidObjectFile;
    const sym_count = symtab_size / 24;

    // --- Phase 4: Apply relocations ---

    // Find .rela.text sections (SHT_RELA = 4)
    i = 0;
    while (i < e_shnum) : (i += 1) {
        const sh = getShdr(object_bytes, e_shoff, e_shentsize, i) orelse continue;
        const sh_type = std.mem.readInt(u32, sh[4..8], .little);
        if (sh_type != 4) continue; // SHT_RELA

        const sh_info = std.mem.readInt(u32, sh[44..48], .little); // target section index
        const rela_off = std.mem.readInt(u64, sh[24..32], .little);
        const rela_size = std.mem.readInt(u64, sh[32..40], .little);
        const rela_entsize = std.mem.readInt(u64, sh[56..64], .little);
        if (rela_entsize == 0) continue;

        // Find the buffer offset for the target section
        var target_buf_offset: ?u64 = null;
        for (sections[0..section_count]) |sec| {
            if (sec.index == sh_info) {
                target_buf_offset = sec.buf_offset;
                break;
            }
        }
        if (target_buf_offset == null) continue;

        const rela_count = rela_size / rela_entsize;
        var r: u64 = 0;
        while (r < rela_count) : (r += 1) {
            const ent_off = rela_off + r * rela_entsize;
            if (ent_off + 24 > object_bytes.len) continue;
            const rela = object_bytes[@intCast(ent_off)..];

            const r_offset = std.mem.readInt(u64, rela[0..8], .little);
            const r_info = std.mem.readInt(u64, rela[8..16], .little);
            const r_addend = std.mem.readInt(i64, rela[16..24], .little);
            const r_type: u32 = @truncate(r_info & 0xFFFFFFFF);
            const r_sym: u32 = @truncate(r_info >> 32);

            // Resolve symbol address
            const sym_addr = resolveSymbol(
                object_bytes,
                symtab_off,
                sym_count,
                sym_strtab,
                r_sym,
                &sec_buf_offsets,
                buf,
            ) orelse continue; // skip unresolvable symbols in non-executed paths

            // P = address of relocation site in buffer
            const patch_off: u64 = target_buf_offset.? + r_offset;
            if (patch_off + 4 > buf.len) continue;
            const buf_addr = @intFromPtr(buf.ptr);
            const p_addr: i64 = @intCast(buf_addr + patch_off);
            const s_addr: i64 = @intCast(sym_addr);
            const a: i64 = r_addend;

            applyRelocation(buf, @intCast(patch_off), r_type, s_addr, p_addr, a);
        }
    }

    // --- Phase 5: Find roc_eval entry point ---

    var entry_offset: usize = 0;
    const text_buf_offset = sec_buf_offsets[text_section_idx.?];

    var s: u64 = 0;
    while (s < sym_count) : (s += 1) {
        const sym_off = symtab_off + s * 24;
        if (sym_off + 24 > object_bytes.len) continue;
        const sym = object_bytes[@intCast(sym_off)..];
        const st_name = std.mem.readInt(u32, sym[0..4], .little);
        const st_value = std.mem.readInt(u64, sym[8..16], .little);
        const st_shndx = std.mem.readInt(u16, sym[6..8], .little);

        if (st_name < sym_strtab.len) {
            const name = getSectionName(sym_strtab, st_name);
            if (std.mem.eql(u8, name, "roc_eval") or std.mem.eql(u8, name, "_roc_eval")) {
                if (st_shndx < sec_buf_offsets.len) {
                    entry_offset = @intCast(sec_buf_offsets[st_shndx] + st_value - text_buf_offset + text_buf_offset);
                } else {
                    entry_offset = @intCast(text_buf_offset + st_value);
                }
                break;
            }
        }
    }

    return .{ .code = buf, .entry_offset = entry_offset };
}

/// Resolve a symbol index to its absolute address in the output buffer.
fn resolveSymbol(
    object_bytes: []const u8,
    symtab_off: u64,
    sym_count: u64,
    strtab: []const u8,
    sym_idx: u32,
    sec_buf_offsets: *const [256]u64,
    buf: []const u8,
) ?usize {
    if (sym_idx >= sym_count) return null;
    const sym_off = symtab_off + @as(u64, sym_idx) * 24;
    if (sym_off + 24 > object_bytes.len) return null;
    const sym = object_bytes[@intCast(sym_off)..];
    const st_shndx = std.mem.readInt(u16, sym[6..8], .little);
    const st_value = std.mem.readInt(u64, sym[8..16], .little);

    if (st_shndx == 0) {
        // SHN_UNDEF — undefined symbol, try dlsym
        const st_name = std.mem.readInt(u32, sym[0..4], .little);
        if (st_name < strtab.len) {
            const name = getSectionName(strtab, st_name);
            return dlsymLookup(name);
        }
        return null;
    }
    if (st_shndx >= 0xFF00) return null; // SHN_ABS, SHN_COMMON, etc.

    if (st_shndx < sec_buf_offsets.len) {
        return @intFromPtr(buf.ptr) + @as(usize, @intCast(sec_buf_offsets[st_shndx] + st_value));
    }
    return null;
}

/// Look up an external symbol via dlsym.
fn dlsymLookup(name: []const u8) ?usize {
    // Need a null-terminated copy for dlsym
    var name_buf: [256]u8 = undefined;
    if (name.len >= name_buf.len) return null;
    @memcpy(name_buf[0..name.len], name);
    name_buf[name.len] = 0;
    const name_z: [*:0]const u8 = @ptrCast(&name_buf);

    switch (builtin.os.tag) {
        .macos, .ios, .linux, .freebsd, .openbsd, .netbsd => {
            // RTLD_DEFAULT = search all loaded shared libraries
            const RTLD_DEFAULT: ?*anyopaque = switch (builtin.os.tag) {
                .macos, .ios => @ptrFromInt(@as(usize, @bitCast(@as(isize, -2)))),
                else => null,
            };
            const addr = std.c.dlsym(RTLD_DEFAULT, name_z);
            if (addr) |a| return @intFromPtr(a);
            return null;
        },
        else => return null,
    }
}

/// Apply a single aarch64 ELF relocation.
fn applyRelocation(buf: []u8, off: usize, r_type: u32, s: i64, p: i64, a: i64) void {
    switch (r_type) {
        // R_AARCH64_CALL26 (283) and R_AARCH64_JUMP26 (282)
        282, 283 => {
            if (off + 4 > buf.len) return;
            const offset_val: i64 = (s + a - p) >> 2;
            var inst = std.mem.readInt(u32, buf[off..][0..4], .little);
            inst = (inst & 0xFC000000) | (@as(u32, @bitCast(@as(i32, @truncate(offset_val)))) & 0x03FFFFFF);
            std.mem.writeInt(u32, buf[off..][0..4], inst, .little);
        },
        // R_AARCH64_ADR_PREL_PG_HI21 (275)
        275 => {
            if (off + 4 > buf.len) return;
            const val: i64 = ((s + a) & ~@as(i64, 0xFFF)) - (p & ~@as(i64, 0xFFF));
            const imm: i32 = @truncate(val >> 12);
            const imm_u: u32 = @bitCast(imm);
            var inst = std.mem.readInt(u32, buf[off..][0..4], .little);
            // ADRP encoding: immlo in bits [30:29], immhi in bits [23:5]
            const immlo: u32 = imm_u & 0x3;
            const immhi: u32 = (imm_u >> 2) & 0x7FFFF;
            inst = (inst & 0x9F00001F) | (immlo << 29) | (immhi << 5);
            std.mem.writeInt(u32, buf[off..][0..4], inst, .little);
        },
        // R_AARCH64_ADD_ABS_LO12_NC (277)
        277 => {
            if (off + 4 > buf.len) return;
            const imm: u32 = @truncate(@as(u64, @bitCast(s + a)) & 0xFFF);
            var inst = std.mem.readInt(u32, buf[off..][0..4], .little);
            inst = (inst & 0xFFC003FF) | (imm << 10);
            std.mem.writeInt(u32, buf[off..][0..4], inst, .little);
        },
        // R_AARCH64_LDST64_ABS_LO12_NC (286)
        286 => {
            if (off + 4 > buf.len) return;
            const imm: u32 = @truncate((@as(u64, @bitCast(s + a)) & 0xFFF) >> 3);
            var inst = std.mem.readInt(u32, buf[off..][0..4], .little);
            inst = (inst & 0xFFC003FF) | (imm << 10);
            std.mem.writeInt(u32, buf[off..][0..4], inst, .little);
        },
        // R_AARCH64_LDST128_ABS_LO12_NC (299)
        299 => {
            if (off + 4 > buf.len) return;
            const imm: u32 = @truncate((@as(u64, @bitCast(s + a)) & 0xFFF) >> 4);
            var inst = std.mem.readInt(u32, buf[off..][0..4], .little);
            inst = (inst & 0xFFC003FF) | (imm << 10);
            std.mem.writeInt(u32, buf[off..][0..4], inst, .little);
        },
        // R_AARCH64_LDST16_ABS_LO12_NC (284)
        284 => {
            if (off + 4 > buf.len) return;
            const imm: u32 = @truncate((@as(u64, @bitCast(s + a)) & 0xFFF) >> 1);
            var inst = std.mem.readInt(u32, buf[off..][0..4], .little);
            inst = (inst & 0xFFC003FF) | (imm << 10);
            std.mem.writeInt(u32, buf[off..][0..4], inst, .little);
        },
        // R_AARCH64_LDST32_ABS_LO12_NC (285)
        285 => {
            if (off + 4 > buf.len) return;
            const imm: u32 = @truncate((@as(u64, @bitCast(s + a)) & 0xFFF) >> 2);
            var inst = std.mem.readInt(u32, buf[off..][0..4], .little);
            inst = (inst & 0xFFC003FF) | (imm << 10);
            std.mem.writeInt(u32, buf[off..][0..4], inst, .little);
        },
        // R_AARCH64_LDST8_ABS_LO12_NC (278)
        278 => {
            if (off + 4 > buf.len) return;
            const imm: u32 = @truncate(@as(u64, @bitCast(s + a)) & 0xFFF);
            var inst = std.mem.readInt(u32, buf[off..][0..4], .little);
            inst = (inst & 0xFFC003FF) | (imm << 10);
            std.mem.writeInt(u32, buf[off..][0..4], inst, .little);
        },
        // R_AARCH64_ABS64 (257)
        257 => {
            if (off + 8 > buf.len) return;
            const val: u64 = @bitCast(s + a);
            std.mem.writeInt(u64, buf[off..][0..8], val, .little);
        },
        // R_AARCH64_PREL32 (261)
        261 => {
            if (off + 4 > buf.len) return;
            const val: i32 = @truncate(s + a - p);
            std.mem.writeInt(i32, buf[off..][0..4], val, .little);
        },
        else => {}, // ignore unknown relocation types
    }
}

/// Get a section header from the ELF section header table.
fn getShdr(bytes: []const u8, e_shoff: u64, e_shentsize: u16, index: u16) ?[]const u8 {
    const off = e_shoff + @as(u64, index) * e_shentsize;
    if (off + 64 > bytes.len) return null;
    return bytes[@intCast(off)..];
}

/// Get data for a section by index.
fn getSectionData(bytes: []const u8, e_shoff: u64, e_shentsize: u16, index: u16) ?[]const u8 {
    const sh = getShdr(bytes, e_shoff, e_shentsize, index) orelse return null;
    const offset = std.mem.readInt(u64, sh[24..32], .little);
    const size = std.mem.readInt(u64, sh[32..40], .little);
    if (offset + size > bytes.len) return null;
    return bytes[@intCast(offset)..@intCast(offset + size)];
}

/// Get a null-terminated name from a string table.
fn getSectionName(strtab: []const u8, name_idx: u32) []const u8 {
    if (name_idx >= strtab.len) return "";
    const start = strtab[name_idx..];
    const end = std.mem.indexOfScalar(u8, start, 0) orelse start.len;
    return start[0..end];
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
