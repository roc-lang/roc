//! Object file writer for the dev backend.
//!
//! This module takes generated machine code and produces relocatable
//! object files in platform-specific formats (ELF, Mach-O, COFF).

const std = @import("std");
const Allocator = std.mem.Allocator;

const object = @import("object/mod.zig");
const RocTarget = @import("roc_target").RocTarget;
const Relocation = @import("Relocation.zig").Relocation;

/// Generate an object file from code and relocations.
///
/// This is the main entry point for the dev backend. It takes generated
/// machine code and produces a relocatable object file.
pub fn generateObjectFile(
    allocator: Allocator,
    target: RocTarget,
    code: []const u8,
    rodata: []const u8,
    symbols: []const Symbol,
    relocations: []const Relocation,
    rodata_relocations: []const DataRelocation,
    output: *std.ArrayList(u8),
) (Allocator.Error || error{UnsupportedTarget})!void {
    return generateObjectFileWithDebug(allocator, target, code, rodata, symbols, relocations, rodata_relocations, null, output);
}

/// DWARF debug sections to include in the object file.
pub const DebugSections = struct {
    line: []const u8,
    abbrev: []const u8,
    info: []const u8,
    line_relocs: []const object.DebugReloc,
    info_relocs: []const object.DebugReloc,
};

/// Like `generateObjectFile`, with DWARF debug sections (ELF and Mach-O;
/// COFF dev objects do not carry debug info yet).
pub fn generateObjectFileWithDebug(
    allocator: Allocator,
    target: RocTarget,
    code: []const u8,
    rodata: []const u8,
    symbols: []const Symbol,
    relocations: []const Relocation,
    rodata_relocations: []const DataRelocation,
    debug: ?DebugSections,
    output: *std.ArrayList(u8),
) (Allocator.Error || error{UnsupportedTarget})!void {
    const cpu_arch = target.toCpuArch();
    const os_tag = target.toOsTag();

    switch (os_tag) {
        .linux, .freebsd, .openbsd, .netbsd => {
            const elf_arch: object.elf.Architecture = switch (cpu_arch) {
                .x86_64 => .x86_64,
                .aarch64 => .aarch64,
                else => return error.UnsupportedTarget,
            };
            var elf = try object.ElfWriter.init(allocator, elf_arch);
            defer elf.deinit();

            try elf.setCode(code);
            try elf.setRodata(rodata);
            if (debug) |d| try elf.setDebugSections(d.line, d.abbrev, d.info, d.line_relocs, d.info_relocs);

            // Add symbols
            for (symbols) |sym| {
                const sym_idx = try elf.addSymbol(.{
                    .name = sym.name,
                    .section = if (sym.is_external) .undef else elfSection(sym.section),
                    .offset = sym.offset,
                    .size = sym.size,
                    .is_global = sym.is_global or sym.is_external,
                    .is_function = sym.is_function,
                });

                // Add relocations for this symbol
                // x86_64 R_X86_64_PLT32 needs addend -4 (PC-relative from end of 4-byte field)
                const reloc_addend: i64 = if (cpu_arch == .x86_64) -4 else 0;
                for (relocations) |rel| {
                    switch (rel) {
                        .linked_function => |f| if (std.mem.eql(u8, f.name, sym.name)) {
                            try elf.addTextRelocation(rel.getOffset(), sym_idx, reloc_addend);
                        },
                        .linked_data => |d| if (std.mem.eql(u8, d.name, sym.name)) {
                            try elf.addTextDataRelocation(rel.getOffset(), sym_idx, d.kind);
                        },
                        else => {},
                    }
                }

                for (rodata_relocations) |rel| {
                    if (std.mem.eql(u8, rel.target_symbol_name, sym.name)) {
                        try elf.addRodataRelocation(rel.offset, sym_idx, rel.addend);
                    }
                }
            }

            try elf.write(output);
        },
        .macos => {
            const macho_arch: object.macho.Architecture = switch (cpu_arch) {
                .x86_64 => .x86_64,
                .aarch64 => .aarch64,
                else => return error.UnsupportedTarget,
            };
            var macho = try object.MachOWriter.init(allocator, macho_arch);
            defer macho.deinit();

            try macho.setCode(code);
            try macho.setRodata(rodata);
            if (debug) |d| try macho.setDebugSections(d.line, d.abbrev, d.info, d.line_relocs, d.info_relocs);

            // Add symbols (underscore prefix for C ABI is added in MachOWriter.write())
            for (symbols) |sym| {
                const is_macho_external = sym.is_global or sym.is_external or symbolIsRelocationTarget(sym.name, relocations, rodata_relocations);
                const sym_idx = try macho.addSymbol(.{
                    .name = sym.name,
                    .section = if (sym.is_external) 0 else machoSectionNumber(sym.section),
                    .offset = sym.offset,
                    .is_external = is_macho_external,
                });

                // Add relocations for this symbol
                for (relocations) |rel| {
                    switch (rel) {
                        .linked_function => |f| if (std.mem.eql(u8, f.name, sym.name)) {
                            try macho.addTextRelocation(@intCast(rel.getOffset()), sym_idx, is_macho_external);
                        },
                        .linked_data => |d| if (std.mem.eql(u8, d.name, sym.name)) {
                            try macho.addTextDataRelocation(@intCast(rel.getOffset()), sym_idx, is_macho_external, d.kind);
                        },
                        else => {},
                    }
                }

                for (rodata_relocations) |rel| {
                    if (std.mem.eql(u8, rel.target_symbol_name, sym.name)) {
                        try macho.addRodataRelocation(@intCast(rel.offset), sym_idx, is_macho_external, rel.addend);
                    }
                }
            }

            try macho.write(output);
        },
        .windows => {
            const coff_arch: object.coff.Architecture = switch (cpu_arch) {
                .x86_64 => .x86_64,
                .aarch64 => .aarch64,
                else => return error.UnsupportedTarget,
            };
            var coff_writer = try object.CoffWriter.init(allocator, coff_arch);
            defer coff_writer.deinit();

            try coff_writer.setCode(code);
            try coff_writer.setRodata(rodata);

            // Add symbols and function info for unwind tables
            for (symbols) |sym| {
                const sym_idx = try coff_writer.addSymbol(.{
                    .name = sym.name,
                    .section = if (sym.is_external) .undef else coffSection(sym.section),
                    .offset = @intCast(sym.offset),
                    .is_global = sym.is_global or sym.is_external,
                    .is_function = sym.is_function,
                });

                // Add relocations for this symbol
                for (relocations) |rel| {
                    switch (rel) {
                        .linked_function => |f| if (std.mem.eql(u8, f.name, sym.name)) {
                            try coff_writer.addTextRelocation(@intCast(rel.getOffset()), sym_idx);
                        },
                        .linked_data => |d| if (std.mem.eql(u8, d.name, sym.name)) {
                            try coff_writer.addTextDataRelocation(@intCast(rel.getOffset()), sym_idx, d.kind);
                        },
                        else => {},
                    }
                }

                for (rodata_relocations) |rel| {
                    if (std.mem.eql(u8, rel.target_symbol_name, sym.name)) {
                        try coff_writer.addRdataRelocation(@intCast(rel.offset), sym_idx, rel.addend);
                    }
                }

                // Add function info for Windows unwind tables.
                const has_unwind_info = sym.prologue_size != 0 or
                    sym.stack_alloc != 0 or
                    sym.frame_size != 0 or
                    sym.callee_saved_mask != 0 or
                    sym.epilogue_offset != 0;
                if ((coff_arch == .x86_64 or coff_arch == .aarch64) and sym.is_function and !sym.is_external and has_unwind_info) {
                    try coff_writer.addFunctionInfo(.{
                        .start_offset = @intCast(sym.offset),
                        .end_offset = @intCast(sym.offset + sym.size),
                        .prologue_size = sym.prologue_size,
                        .frame_reg_offset = 0, // RSP offset not scaled for our simple case
                        .uses_frame_pointer = sym.uses_frame_pointer,
                        .stack_alloc = sym.stack_alloc,
                        .frame_size = sym.frame_size,
                        .callee_saved_mask = sym.callee_saved_mask,
                        .epilogue_offset = sym.epilogue_offset,
                    });
                }
            }

            try coff_writer.write(output);
        },
        else => return error.UnsupportedTarget,
    }
}

/// Symbol information for object file generation
pub const Symbol = struct {
    name: []const u8,
    section: Section = .text,
    offset: u64,
    size: u64,
    is_global: bool,
    is_function: bool,
    is_external: bool,
    // Unwind metadata for Windows object files.
    prologue_size: u32 = 0,
    stack_alloc: u32 = 0,
    frame_size: u32 = 0,
    callee_saved_mask: u32 = 0,
    epilogue_offset: u32 = 0,
    uses_frame_pointer: bool = true,
};

/// One absolute pointer relocation inside the readonly data section.
pub const DataRelocation = struct {
    offset: u64,
    target_symbol_name: []const u8,
    addend: i64 = 0,
};

/// Logical object section used by the dev object writer facade.
pub const Section = enum {
    text,
    rodata,
    undef,
};

fn machoSectionNumber(section: Section) u8 {
    return switch (section) {
        .text => 1,
        .rodata => 2,
        .undef => 0,
    };
}

fn elfSection(section: Section) object.elf.Section {
    return switch (section) {
        .text => .text,
        .rodata => .rodata,
        .undef => .undef,
    };
}

fn coffSection(section: Section) object.coff.Section {
    return switch (section) {
        .text => .text,
        .rodata => .rdata,
        .undef => .undef,
    };
}

fn symbolIsRelocationTarget(
    name: []const u8,
    relocations: []const Relocation,
    rodata_relocations: []const DataRelocation,
) bool {
    for (relocations) |rel| {
        switch (rel) {
            .linked_function => |function| if (std.mem.eql(u8, function.name, name)) return true,
            .linked_data => |data| if (std.mem.eql(u8, data.name, name)) return true,
            else => {},
        }
    }
    for (rodata_relocations) |rel| {
        if (std.mem.eql(u8, rel.target_symbol_name, name)) return true;
    }
    return false;
}

// Tests

test "generate x86_64 linux object" {
    const allocator = std.testing.allocator;

    // Simple x86_64 code: ret
    const code = &[_]u8{0xC3};

    const symbols = &[_]Symbol{
        .{
            .name = "test_func",
            .offset = 0,
            .size = 1,
            .is_global = true,
            .is_function = true,
            .is_external = false,
        },
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);

    try generateObjectFile(
        allocator,
        .x64linux,
        code,
        &.{},
        symbols,
        &.{},
        &.{},
        &output,
    );

    // Verify ELF magic
    try std.testing.expectEqualSlices(u8, "\x7fELF", output.items[0..4]);
}

test "generate x86_64 macos object" {
    const allocator = std.testing.allocator;

    // Simple x86_64 code: ret
    const code = &[_]u8{0xC3};

    // Symbol name without underscore - prefix is added automatically for Mach-O
    const symbols = &[_]Symbol{
        .{
            .name = "test_func",
            .offset = 0,
            .size = 1,
            .is_global = true,
            .is_function = true,
            .is_external = false,
        },
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);

    try generateObjectFile(
        allocator,
        .x64mac,
        code,
        &.{},
        symbols,
        &.{},
        &.{},
        &output,
    );

    // Verify Mach-O magic
    const magic = std.mem.readInt(u32, output.items[0..4], .little);
    try std.testing.expectEqual(@as(u32, 0xFEEDFACF), magic);
}

test "generate aarch64 linux object" {
    const allocator = std.testing.allocator;

    // Simple aarch64 code: ret (RET instruction)
    const code = &[_]u8{ 0xC0, 0x03, 0x5F, 0xD6 };

    const symbols = &[_]Symbol{
        .{
            .name = "test_func",
            .offset = 0,
            .size = 4,
            .is_global = true,
            .is_function = true,
            .is_external = false,
        },
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);

    try generateObjectFile(
        allocator,
        .arm64linux,
        code,
        &.{},
        symbols,
        &.{},
        &.{},
        &output,
    );

    // Verify ELF magic
    try std.testing.expectEqualSlices(u8, "\x7fELF", output.items[0..4]);
}

test "generate x86_64 windows object" {
    const allocator = std.testing.allocator;

    // Simple x86_64 code: ret
    const code = &[_]u8{0xC3};

    const symbols = &[_]Symbol{
        .{
            .name = "test_func",
            .offset = 0,
            .size = 1,
            .is_global = true,
            .is_function = true,
            .is_external = false,
        },
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);

    try generateObjectFile(
        allocator,
        .x64win,
        code,
        &.{},
        symbols,
        &.{},
        &.{},
        &output,
    );

    // Verify COFF machine type (x86_64 = 0x8664)
    const machine = std.mem.readInt(u16, output.items[0..2], .little);
    try std.testing.expectEqual(@as(u16, 0x8664), machine);
}

test "generate aarch64 windows object" {
    const allocator = std.testing.allocator;

    // Simple aarch64 code: ret
    const code = &[_]u8{ 0xC0, 0x03, 0x5F, 0xD6 };

    const symbols = &[_]Symbol{
        .{
            .name = "test_func",
            .offset = 0,
            .size = 4,
            .is_global = true,
            .is_function = true,
            .is_external = false,
        },
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);

    try generateObjectFile(
        allocator,
        .arm64win,
        code,
        &.{},
        symbols,
        &.{},
        &.{},
        &output,
    );

    // Verify COFF machine type (ARM64 = 0xAA64)
    const machine = std.mem.readInt(u16, output.items[0..2], .little);
    try std.testing.expectEqual(@as(u16, 0xAA64), machine);
}

test "generate aarch64 windows object with unwind sections" {
    const allocator = std.testing.allocator;

    const code = &[_]u8{
        0xFD, 0x7B, 0xBA, 0xA9, // stp x29, x30, [sp, #-96]!
        0xFD, 0x03, 0x00, 0x91, // mov x29, sp
        0xF3, 0x53, 0x01, 0xA9, // stp x19, x20, [sp, #16]
        0xF3, 0x53, 0x41, 0xA9, // ldp x19, x20, [sp, #16]
        0xFD, 0x7B, 0xC6, 0xA8, // ldp x29, x30, [sp], #96
        0xC0, 0x03, 0x5F, 0xD6, // ret
    };

    const x19_bit = @as(u32, 1) << 19;
    const x20_bit = @as(u32, 1) << 20;
    const symbols = &[_]Symbol{
        .{
            .name = "test_func",
            .offset = 0,
            .size = code.len,
            .is_global = true,
            .is_function = true,
            .is_external = false,
            .prologue_size = 12,
            .frame_size = 96,
            .callee_saved_mask = x19_bit | x20_bit,
            .epilogue_offset = 12,
        },
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);

    try generateObjectFile(
        allocator,
        .arm64win,
        code,
        &.{},
        symbols,
        &.{},
        &.{},
        &output,
    );

    const machine = std.mem.readInt(u16, output.items[0..2], .little);
    try std.testing.expectEqual(@as(u16, 0xAA64), machine);

    const num_sections = std.mem.readInt(u16, output.items[2..4], .little);
    try std.testing.expectEqual(@as(u16, 3), num_sections);
}

test "static strings are emitted into readonly object sections for native targets" {
    const required = "readonly string literal longer than thirty bytes";
    const forbidden = "INTERMEDIATE_ONLY_SHOULD_NOT_BE_EMITTED";

    const targets = [_]RocTarget{
        .x64mac,
        .arm64mac,
        .x64musl,
        .arm64musl,
        .x64glibc,
        .arm64glibc,
        .x64win,
        .arm64win,
        .x64freebsd,
        .x64openbsd,
        .x64netbsd,
        .x64elf,
    };

    for (targets) |target| {
        try expectReadonlyObjectDataForTarget(target, required, forbidden);
    }
}

fn expectReadonlyObjectDataForTarget(target: RocTarget, required: []const u8, forbidden: []const u8) (Allocator.Error || error{ UnsupportedTarget, InvalidObjectFile, SectionNotFound, TestUnexpectedResult })!void {
    const allocator = std.testing.allocator;
    const code = switch (target.toCpuArch()) {
        .aarch64 => &[_]u8{ 0xC0, 0x03, 0x5F, 0xD6 }, // ret
        .x86_64 => &[_]u8{0xC3}, // ret
        else => return error.UnsupportedTarget,
    };
    const rodata = required;
    const symbols = [_]Symbol{
        .{
            .name = "roc__static_string",
            .section = .rodata,
            .offset = 0,
            .size = rodata.len,
            .is_global = true,
            .is_function = false,
            .is_external = false,
        },
    };

    var output = std.ArrayList(u8).empty;
    defer output.deinit(allocator);

    try generateObjectFile(
        allocator,
        target,
        code,
        rodata,
        &symbols,
        &.{},
        &.{},
        &output,
    );

    const readonly = try readonlySection(target, output.items);
    try std.testing.expect(std.mem.find(u8, readonly, required) != null);
    try std.testing.expect(std.mem.find(u8, output.items, forbidden) == null);
}

fn readonlySection(target: RocTarget, object_bytes: []const u8) error{ UnsupportedTarget, InvalidObjectFile, SectionNotFound }![]const u8 {
    return switch (target.toOsTag()) {
        .macos => try machoSection(object_bytes, "__const"),
        .windows => try coffSectionData(object_bytes, ".rdata"),
        .linux, .freebsd, .openbsd, .netbsd => try elfSectionData(object_bytes, ".rodata"),
        else => error.UnsupportedTarget,
    };
}

fn elfSectionData(bytes: []const u8, wanted_name: []const u8) error{ InvalidObjectFile, UnsupportedTarget, SectionNotFound }![]const u8 {
    if (bytes.len < 64) return error.InvalidObjectFile;
    if (!std.mem.eql(u8, bytes[0..4], "\x7fELF")) return error.InvalidObjectFile;
    if (bytes[4] != 2) return error.UnsupportedTarget;

    const e_shoff = std.mem.readInt(u64, bytes[40..48], .little);
    const e_shentsize = std.mem.readInt(u16, bytes[58..60], .little);
    const e_shnum = std.mem.readInt(u16, bytes[60..62], .little);
    const e_shstrndx = std.mem.readInt(u16, bytes[62..64], .little);
    if (e_shoff == 0 or e_shnum == 0) return error.SectionNotFound;
    if (e_shoff + @as(u64, e_shnum) * e_shentsize > bytes.len) return error.InvalidObjectFile;
    if (e_shstrndx >= e_shnum) return error.InvalidObjectFile;

    const shstr_hdr_offset = e_shoff + @as(u64, e_shstrndx) * e_shentsize;
    if (shstr_hdr_offset + 64 > bytes.len) return error.InvalidObjectFile;
    const shstr_hdr = bytes[@intCast(shstr_hdr_offset)..];
    const shstr_offset = std.mem.readInt(u64, shstr_hdr[24..32], .little);
    const shstr_size = std.mem.readInt(u64, shstr_hdr[32..40], .little);
    if (shstr_offset + shstr_size > bytes.len) return error.InvalidObjectFile;
    const shstr = bytes[@intCast(shstr_offset)..@intCast(shstr_offset + shstr_size)];

    var i: u16 = 0;
    while (i < e_shnum) : (i += 1) {
        const sh_offset = e_shoff + @as(u64, i) * e_shentsize;
        if (sh_offset + 64 > bytes.len) return error.InvalidObjectFile;
        const sh = bytes[@intCast(sh_offset)..];
        const name_offset = std.mem.readInt(u32, sh[0..4], .little);
        if (name_offset >= shstr.len) return error.InvalidObjectFile;
        const name_tail = shstr[name_offset..];
        const name_len = std.mem.findScalar(u8, name_tail, 0) orelse return error.InvalidObjectFile;
        const name = name_tail[0..name_len];
        if (!std.mem.eql(u8, name, wanted_name)) continue;

        const section_offset = std.mem.readInt(u64, sh[24..32], .little);
        const section_size = std.mem.readInt(u64, sh[32..40], .little);
        if (section_offset + section_size > bytes.len) return error.InvalidObjectFile;
        return bytes[@intCast(section_offset)..@intCast(section_offset + section_size)];
    }

    return error.SectionNotFound;
}

fn machoSection(bytes: []const u8, wanted_name: []const u8) error{ InvalidObjectFile, SectionNotFound }![]const u8 {
    if (bytes.len < 32) return error.InvalidObjectFile;
    if (std.mem.readInt(u32, bytes[0..4], .little) != 0xfeedfacf) return error.InvalidObjectFile;

    const ncmds = std.mem.readInt(u32, bytes[16..20], .little);
    var command_offset: usize = 32;
    var command_index: u32 = 0;
    while (command_index < ncmds) : (command_index += 1) {
        if (command_offset + 8 > bytes.len) return error.InvalidObjectFile;
        const cmd = std.mem.readInt(u32, bytes[command_offset..][0..4], .little);
        const cmdsize = std.mem.readInt(u32, bytes[command_offset..][4..8], .little);
        if (cmdsize < 8 or command_offset + cmdsize > bytes.len) return error.InvalidObjectFile;

        if (cmd == 0x19) {
            if (cmdsize < 72) return error.InvalidObjectFile;
            const nsects = std.mem.readInt(u32, bytes[command_offset + 64 ..][0..4], .little);
            var section_offset = command_offset + 72;
            var section_index: u32 = 0;
            while (section_index < nsects) : (section_index += 1) {
                if (section_offset + 80 > bytes.len) return error.InvalidObjectFile;
                const raw_name = bytes[section_offset..][0..16];
                const name_len = std.mem.findScalar(u8, raw_name, 0) orelse raw_name.len;
                const name = raw_name[0..name_len];
                if (std.mem.eql(u8, name, wanted_name)) {
                    const size = std.mem.readInt(u64, bytes[section_offset + 40 ..][0..8], .little);
                    const offset = std.mem.readInt(u32, bytes[section_offset + 48 ..][0..4], .little);
                    if (@as(u64, offset) + size > bytes.len) return error.InvalidObjectFile;
                    const start: usize = @intCast(offset);
                    const end: usize = @intCast(@as(u64, offset) + size);
                    return bytes[start..end];
                }
                section_offset += 80;
            }
        }

        command_offset += cmdsize;
    }

    return error.SectionNotFound;
}

fn coffSectionData(bytes: []const u8, wanted_name: []const u8) error{ InvalidObjectFile, SectionNotFound }![]const u8 {
    if (bytes.len < 20) return error.InvalidObjectFile;
    const number_of_sections = std.mem.readInt(u16, bytes[2..4], .little);
    const optional_header_size = std.mem.readInt(u16, bytes[16..18], .little);
    var section_offset: usize = 20 + optional_header_size;

    var section_index: u16 = 0;
    while (section_index < number_of_sections) : (section_index += 1) {
        if (section_offset + 40 > bytes.len) return error.InvalidObjectFile;
        const raw_name = bytes[section_offset..][0..8];
        const name_len = std.mem.findScalar(u8, raw_name, 0) orelse raw_name.len;
        const name = raw_name[0..name_len];
        if (std.mem.eql(u8, name, wanted_name)) {
            const size = std.mem.readInt(u32, bytes[section_offset + 16 ..][0..4], .little);
            const offset = std.mem.readInt(u32, bytes[section_offset + 20 ..][0..4], .little);
            if (@as(u64, offset) + @as(u64, size) > bytes.len) return error.InvalidObjectFile;
            const start: usize = @intCast(offset);
            const end: usize = @intCast(@as(u64, offset) + @as(u64, size));
            return bytes[start..end];
        }
        section_offset += 40;
    }

    return error.SectionNotFound;
}
