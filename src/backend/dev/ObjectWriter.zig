//! Object file writer for the dev backend.
//!
//! This module takes generated machine code and produces relocatable
//! object files in platform-specific formats (ELF, Mach-O, COFF).

const std = @import("std");
const Allocator = std.mem.Allocator;

const object = @import("object/mod.zig");
const roc_target = @import("roc_target");
const RocTarget = roc_target.RocTarget;
const Relocation = @import("Relocation.zig").Relocation;
const IndexedRelocation = @import("Relocation.zig").IndexedRelocation;
const SymbolTable = @import("SymbolTable.zig");

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
    var table: SymbolTable.Table = .{};
    defer table.deinit(allocator);
    for (symbols, 0..) |symbol, ordinal| {
        const id = try table.intern(allocator, symbol.name);
        std.debug.assert(@intFromEnum(id) == ordinal);
    }
    var indexed = try std.ArrayList(IndexedRelocation).initCapacity(allocator, relocations.len);
    defer indexed.deinit(allocator);
    for (relocations) |rel| {
        indexed.appendAssumeCapacity(switch (rel) {
            .linked_function => |f| .{ .linked_function = .{ .offset = f.offset, .symbol = table.indices.get(f.name).? } },
            .linked_data => |d| .{ .linked_data = .{ .offset = d.offset, .symbol = table.indices.get(d.name).?, .kind = d.kind } },
            .local_data => |d| .{ .local_data = d },
            .jmp_to_return => |j| .{ .jmp_to_return = j },
        });
    }
    const indexed_data = try allocator.alloc(IndexedDataRelocation, rodata_relocations.len);
    defer allocator.free(indexed_data);
    for (rodata_relocations, indexed_data) |rel, *dest| dest.* = .{
        .offset = rel.offset,
        .symbol = table.indices.get(rel.target_symbol_name).?,
        .addend = rel.addend,
    };
    return generateIndexedObjectFileWithDebug(allocator, target, code, rodata, symbols, indexed.items, indexed_data, debug, output);
}

/// Write producer-resolved relocations. Every target indexes the supplied symbol column.
pub fn generateIndexedObjectFileWithDebug(
    allocator: Allocator,
    target: RocTarget,
    code: []const u8,
    rodata: []const u8,
    symbols: []const Symbol,
    relocations: []const IndexedRelocation,
    rodata_relocations: []const IndexedDataRelocation,
    debug: ?DebugSections,
    output: *std.ArrayList(u8),
) (Allocator.Error || error{UnsupportedTarget})!void {
    const target_indices = try allocator.alloc(u32, symbols.len);
    defer allocator.free(target_indices);
    const cpu_arch = target.toCpuArch();
    const os_tag = target.toOsTag();

    switch (roc_target.classifyOs(os_tag)) {
        .linux, .freebsd, .openbsd, .netbsd => {
            const elf_arch: object.elf.Architecture = if (cpu_arch == .x86_64)
                .x86_64
            else if (cpu_arch == .aarch64)
                .aarch64
            else
                return error.UnsupportedTarget;
            var elf = try object.ElfWriter.init(allocator, elf_arch, elfOsabi(os_tag));
            defer elf.deinit();

            elf.setCode(code);
            elf.setRodata(rodata);
            if (debug) |d| elf.setDebugSections(d.line, d.abbrev, d.info, d.line_relocs, d.info_relocs);

            // Add symbols
            for ([_]bool{ false, true }) |global| {
                for (symbols, 0..) |sym, ordinal| {
                    if ((sym.is_global or sym.is_external) != global) continue;
                    const sym_idx = try elf.addSymbol(.{
                        .name = sym.name,
                        .section = if (sym.is_external) .undef else elfSection(sym.section),
                        .offset = sym.offset,
                        .size = sym.size,
                        .is_global = sym.is_global or sym.is_external,
                        .is_function = sym.is_function,
                        .is_hidden = sym.is_hidden,
                    });

                    target_indices[ordinal] = sym_idx;
                }
            }
            for (relocations) |rel| {
                switch (rel) {
                    .linked_function => |f| try elf.addTextRelocation(rel.getOffset(), target_indices[@intFromEnum(f.symbol)], if (cpu_arch == .x86_64) -4 else 0),
                    .linked_data => |d| try elf.addTextDataRelocation(rel.getOffset(), target_indices[@intFromEnum(d.symbol)], d.kind),
                    .local_data, .jmp_to_return => {},
                }
            }
            for (rodata_relocations) |rel| try elf.addRodataRelocation(rel.offset, target_indices[@intFromEnum(rel.symbol)], rel.addend);

            try elf.write(output);
        },
        .macos => {
            const macho_arch: object.macho.Architecture = if (cpu_arch == .x86_64)
                .x86_64
            else if (cpu_arch == .aarch64)
                .aarch64
            else
                return error.UnsupportedTarget;
            var macho = try object.MachOWriter.init(allocator, macho_arch);
            defer macho.deinit();

            macho.setCode(code);
            macho.setRodata(rodata);
            if (debug) |d| macho.setDebugSections(d.line, d.abbrev, d.info, d.line_relocs, d.info_relocs);

            const referenced = try allocator.alloc(bool, symbols.len);
            defer allocator.free(referenced);
            @memset(referenced, false);
            for (relocations) |rel| switch (rel) {
                .linked_function => |f| {
                    referenced[@intFromEnum(f.symbol)] = true;
                },
                .linked_data => |d| {
                    referenced[@intFromEnum(d.symbol)] = true;
                },
                .local_data, .jmp_to_return => {},
            };
            for (rodata_relocations) |rel| referenced[@intFromEnum(rel.symbol)] = true;

            // Add symbols (underscore prefix for C ABI is added in MachOWriter.write())
            for (symbols, 0..) |sym, ordinal| {
                const is_macho_external = sym.is_global or sym.is_external or referenced[ordinal];
                const sym_idx = try macho.addSymbol(.{
                    .name = sym.name,
                    .section = if (sym.is_external) 0 else machoSectionNumber(sym.section),
                    .offset = sym.offset,
                    .is_external = is_macho_external,
                });

                target_indices[ordinal] = sym_idx;
            }
            for (relocations) |rel| {
                switch (rel) {
                    .linked_function => |f| try macho.addTextRelocation(@intCast(rel.getOffset()), target_indices[@intFromEnum(f.symbol)], true),
                    .linked_data => |d| try macho.addTextDataRelocation(@intCast(rel.getOffset()), target_indices[@intFromEnum(d.symbol)], true, d.kind),
                    .local_data, .jmp_to_return => {},
                }
            }
            for (rodata_relocations) |rel| try macho.addRodataRelocation(@intCast(rel.offset), target_indices[@intFromEnum(rel.symbol)], true, rel.addend);

            try macho.write(output);
        },
        .windows => {
            const coff_arch: object.coff.Architecture = if (cpu_arch == .x86_64)
                .x86_64
            else if (cpu_arch == .aarch64)
                .aarch64
            else
                return error.UnsupportedTarget;
            var coff_writer = try object.CoffWriter.init(allocator, coff_arch);
            defer coff_writer.deinit();

            coff_writer.setCode(code);
            coff_writer.setRodata(rodata);

            // Add symbols and function info for unwind tables
            for (symbols, 0..) |sym, ordinal| {
                const sym_idx = try coff_writer.addSymbol(.{
                    .name = sym.name,
                    .section = if (sym.is_external) .undef else coffSection(sym.section),
                    .offset = @intCast(sym.offset),
                    .is_global = sym.is_global or sym.is_external,
                    .is_function = sym.is_function,
                });

                target_indices[ordinal] = sym_idx;
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
            for (relocations) |rel| {
                switch (rel) {
                    .linked_function => |f| try coff_writer.addTextRelocation(@intCast(rel.getOffset()), target_indices[@intFromEnum(f.symbol)]),
                    .linked_data => |d| try coff_writer.addTextDataRelocation(@intCast(rel.getOffset()), target_indices[@intFromEnum(d.symbol)], d.kind),
                    .local_data, .jmp_to_return => {},
                }
            }
            for (rodata_relocations) |rel| try coff_writer.addRdataRelocation(@intCast(rel.offset), target_indices[@intFromEnum(rel.symbol)], rel.addend);

            try coff_writer.write(output);
        },
        .other => return error.UnsupportedTarget,
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
    is_hidden: bool = false,
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

/// An absolute pointer relocation carrying its producer-assigned target.
pub const IndexedDataRelocation = struct {
    offset: u64,
    symbol: SymbolTable.Id,
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

/// The OSABI a dev-backend object declares for a target OS, matching what LLVM
/// writes for the same triple so both backends' objects agree in one link.
fn elfOsabi(os_tag: std.Target.Os.Tag) object.elf.Osabi {
    return switch (roc_target.classifyOs(os_tag)) {
        .freebsd => .freebsd,
        .openbsd => .openbsd,
        .macos, .windows, .linux, .netbsd, .other => .none,
    };
}

fn coffSection(section: Section) object.coff.Section {
    return switch (section) {
        .text => .text,
        .rodata => .rdata,
        .undef => .undef,
    };
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

test "ELF objects declare the OSABI their target's linker looks for" {
    const cases = [_]struct { target: RocTarget, osabi: u8 }{
        .{ .target = .x64openbsd, .osabi = 12 },
        .{ .target = .x64freebsd, .osabi = 9 },
        .{ .target = .x64netbsd, .osabi = 0 },
        .{ .target = .x64musl, .osabi = 0 },
    };

    for (cases) |case| {
        var output: std.ArrayList(u8) = .empty;
        defer output.deinit(std.testing.allocator);

        try generateObjectFile(
            std.testing.allocator,
            case.target,
            &[_]u8{0xC3}, // ret
            &.{},
            &.{},
            &.{},
            &.{},
            &output,
        );

        // e_ident[EI_OSABI]
        try std.testing.expectEqual(case.osabi, output.items[7]);
    }
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
    const cpu_arch = target.toCpuArch();
    const code = if (cpu_arch == .aarch64)
        &[_]u8{ 0xC0, 0x03, 0x5F, 0xD6 } // ret
    else if (cpu_arch == .x86_64)
        &[_]u8{0xC3} // ret
    else
        return error.UnsupportedTarget;
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
    return switch (roc_target.classifyOs(target.toOsTag())) {
        .macos => try machoSection(object_bytes, "__const"),
        .windows => try coffSectionData(object_bytes, ".rdata"),
        .linux, .freebsd, .openbsd, .netbsd => try elfSectionData(object_bytes, ".rodata"),
        .other => error.UnsupportedTarget,
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

// repro for https://github.com/roc-lang/roc/issues/11129
//
// Writing an object must cost about as much as the number of symbols plus the
// number of relocations. Resolving each relocation's target by comparing it
// against every symbol name instead costs symbols x relocations, so an
// application whose string literals are all distinct -- one readonly data
// symbol per literal, one relocation per use -- pays quadratic cost in the
// number of literals.
//
// The assertion is a growth ratio rather than a wall-clock budget so it does
// not depend on machine speed: growing the object 16x must grow the work
// roughly 16x, not 16x16.
test "object writing cost grows with symbols plus relocations, not their product" {
    const allocator = std.testing.allocator;

    const small_count = 500;
    const large_count = 8000;
    const growth = large_count / small_count;
    // Cost proportional to symbols + relocations grows by `growth`; cost
    // proportional to symbols x relocations grows by `growth * growth`. Leave a
    // wide margin above linear growth while staying far below quadratic.
    const budget_factor = growth * 4;

    const small_ns = @max(1, try fastestObjectWriteNs(allocator, small_count));
    const large_ns = try fastestObjectWriteNs(allocator, large_count);

    if (large_ns > small_ns * budget_factor) {
        std.debug.print(
            "growing the object {d}x grew object writing {d}x ({d} ns for {d} symbols, {d} ns for {d} symbols): " ++
                "relocation targets are being resolved by scanning every symbol\n",
            .{ growth, large_ns / small_ns, small_ns, small_count, large_ns, large_count },
        );
        return error.ObjectWritingScalesWithSymbolsTimesRelocations;
    }
}

/// Prefix shared by the generated data symbol names in the scaling test. A
/// shared prefix and a fixed width are what string literal symbols look like,
/// and they keep every name comparison the same cost.
const scaling_test_symbol_prefix = "roc__str_lit_";

/// Errors the scaling test's timing helpers can surface: allocating the inputs,
/// formatting a symbol name, and writing the object itself.
const ObjectWriteTimingError = Allocator.Error || error{ NoSpaceLeft, UnsupportedTarget };

/// Fastest of several runs of `timeObjectWriteWithDistinctDataSymbols`, so that
/// a scheduling hiccup during one run cannot decide the measured growth.
fn fastestObjectWriteNs(allocator: Allocator, count: usize) ObjectWriteTimingError!u64 {
    var fastest: u64 = std.math.maxInt(u64);
    for (0..3) |_| {
        fastest = @min(fastest, try timeObjectWriteWithDistinctDataSymbols(allocator, count));
    }
    return fastest;
}

/// Nanoseconds spent writing an object with `count` distinct readonly data
/// symbols, each referenced by exactly one text relocation.
fn timeObjectWriteWithDistinctDataSymbols(allocator: Allocator, count: usize) ObjectWriteTimingError!u64 {
    const name_width = scaling_test_symbol_prefix.len + 7;

    const name_bytes = try allocator.alloc(u8, count * name_width);
    defer allocator.free(name_bytes);

    const symbols = try allocator.alloc(Symbol, count);
    defer allocator.free(symbols);

    const relocations = try allocator.alloc(Relocation, count);
    defer allocator.free(relocations);

    const rodata = try allocator.alloc(u8, count * 8);
    defer allocator.free(rodata);
    @memset(rodata, 0);

    const code = try allocator.alloc(u8, count * 8);
    defer allocator.free(code);
    @memset(code, 0);

    for (0..count) |i| {
        const name = try std.fmt.bufPrint(
            name_bytes[i * name_width ..][0..name_width],
            scaling_test_symbol_prefix ++ "{d:0>7}",
            .{i},
        );
        symbols[i] = .{
            .name = name,
            .section = .rodata,
            .offset = i * 8,
            .size = 8,
            .is_global = false,
            .is_function = false,
            .is_external = false,
        };
        relocations[i] = .{ .linked_data = .{ .offset = i * 8, .name = name } };
    }

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(allocator);

    const io = std.testing.io;
    const started_ns = std.Io.Timestamp.now(io, .awake).nanoseconds;
    try generateObjectFile(allocator, .x64linux, code, rodata, symbols, relocations, &.{}, &output);
    const finished_ns = std.Io.Timestamp.now(io, .awake).nanoseconds;

    return @intCast(@max(0, finished_ns - started_ns));
}

test "indexed object relocations preserve targets through format symbol ordering" {
    const allocator = std.testing.allocator;
    const symbols = [_]Symbol{
        .{ .name = "external_function", .offset = 0, .size = 0, .is_global = true, .is_function = true, .is_external = true },
        .{ .name = "global_data", .section = .rodata, .offset = 8, .size = 8, .is_global = true, .is_function = false, .is_external = false },
        .{ .name = "local_data", .section = .rodata, .offset = 16, .size = 8, .is_global = false, .is_function = false, .is_external = false },
        .{ .name = "unused_local", .offset = 24, .size = 8, .is_global = false, .is_function = true, .is_external = false },
    };
    var table: SymbolTable.Table = .{};
    defer table.deinit(allocator);
    var ids: [symbols.len]SymbolTable.Id = undefined;
    for (symbols, &ids) |symbol, *id| id.* = try table.intern(allocator, symbol.name);
    const data_relocations = [_]IndexedDataRelocation{
        .{ .offset = 0, .symbol = ids[2], .addend = 3 },
        .{ .offset = 24, .symbol = ids[0] },
    };
    for ([_]RocTarget{ .x64linux, .arm64linux, .x64mac, .arm64mac, .x64win, .arm64win }) |target| {
        const is_x64 = target.toCpuArch() == .x86_64;
        const relocations = [_]IndexedRelocation{
            .{ .linked_data = .{ .offset = 8, .symbol = ids[2], .kind = if (is_x64) .rel32 else .page21 } },
            .{ .linked_function = .{ .offset = 0, .symbol = ids[0] } },
            .{ .linked_data = .{ .offset = 12, .symbol = ids[2], .kind = if (is_x64) .rel32 else .pageoff12 } },
            .{ .linked_data = .{ .offset = 16, .symbol = ids[1] } },
        };
        var output: std.ArrayList(u8) = .empty;
        defer output.deinit(allocator);
        try generateIndexedObjectFileWithDebug(allocator, target, &([_]u8{0} ** 32), &([_]u8{0} ** 32), &symbols, &relocations, &data_relocations, null, &output);
        const decoded = try TestObjectTables.read(target, output.items);
        const expected = [_][]const u8{ "local_data", "external_function", "local_data", "global_data" };
        try std.testing.expectEqual(@as(usize, 4), decoded.text.len / decoded.relocation_size);
        try std.testing.expectEqual(@as(usize, 2), decoded.data.len / decoded.relocation_size);
        const expected_types: [4]usize = switch (decoded.format) {
            .elf => if (is_x64) .{ 2, 4, 2, 1 } else .{ 275, 283, 277, 257 },
            .macho => if (is_x64) .{ 1, 2, 1, 0 } else .{ 3, 2, 4, 0 },
            .coff => if (is_x64) .{ 4, 4, 4, 1 } else .{ 4, 3, 6, 14 },
        };
        for (expected, relocations, 0..) |name, relocation, i| {
            const record = decoded.text[i * decoded.relocation_size ..][0..decoded.relocation_size];
            try std.testing.expectEqual(relocation.getOffset(), decoded.offset(record));
            try std.testing.expectEqualStrings(name, decoded.targetName(record));
            try std.testing.expectEqual(expected_types[i], decoded.relocationType(record));
        }
        try std.testing.expectEqualStrings("local_data", decoded.targetName(decoded.data[0..decoded.relocation_size]));
        try std.testing.expectEqualStrings("external_function", decoded.targetName(decoded.data[decoded.relocation_size..]));
        if (decoded.format == .elf) {
            try std.testing.expectEqual(@as(i64, 3), std.mem.readInt(i64, decoded.data[16..24], .little));
            try std.testing.expectEqual(@as(i64, if (target.toCpuArch() == .x86_64) -4 else 0), std.mem.readInt(i64, decoded.text[decoded.relocation_size + 16 ..][0..8], .little));
        } else {
            const bytes = try readonlySection(target, output.items);
            try std.testing.expectEqual(@as(u64, 3), std.mem.readInt(u64, bytes[0..8], .little));
        }
    }
}

/// Independent views of the serialized symbol and relocation tables used by tests.
const TestObjectTables = struct {
    format: enum { elf, macho, coff },
    text: []const u8,
    data: []const u8,
    symbols: []const u8,
    strings: []const u8,
    relocation_size: usize,

    fn read(target: RocTarget, bytes: []const u8) error{ InvalidObjectFile, UnsupportedTarget, SectionNotFound }!TestObjectTables {
        switch (roc_target.classifyOs(target.toOsTag())) {
            .linux => return .{
                .format = .elf,
                .text = try elfSectionData(bytes, ".rela.text"),
                .data = try elfSectionData(bytes, ".rela.rodata"),
                .symbols = try elfSectionData(bytes, ".symtab"),
                .strings = try elfSectionData(bytes, ".strtab"),
                .relocation_size = 24,
            },
            .windows => {
                const symoff = read32(bytes, 8);
                const count = read32(bytes, 12);
                const textoff = read32(bytes, 20 + 24);
                const textcount = read16(bytes, 20 + 32);
                const dataoff = read32(bytes, 60 + 24);
                const datacount = read16(bytes, 60 + 32);
                return .{ .format = .coff, .text = bytes[textoff..][0 .. textcount * 10], .data = bytes[dataoff..][0 .. datacount * 10], .symbols = bytes[symoff..][0 .. count * 18], .strings = bytes[symoff + count * 18 ..], .relocation_size = 10 };
            },
            .macos => {
                var text: []const u8 = &.{};
                var data: []const u8 = &.{};
                var symbols: []const u8 = &.{};
                var strings: []const u8 = &.{};
                var command: usize = 32;
                for (0..read32(bytes, 16)) |_| {
                    const kind = read32(bytes, command);
                    if (kind == 0x19) {
                        for (0..read32(bytes, command + 64)) |i| {
                            const section = bytes[command + 72 + i * 80 ..][0..80];
                            const relocoff = read32(section, 56);
                            const count = read32(section, 60);
                            if (std.mem.eql(u8, std.mem.sliceTo(section[0..16], 0), "__text")) text = bytes[relocoff..][0 .. count * 8];
                            if (std.mem.eql(u8, std.mem.sliceTo(section[0..16], 0), "__const")) data = bytes[relocoff..][0 .. count * 8];
                        }
                    } else if (kind == 2) {
                        symbols = bytes[read32(bytes, command + 8)..][0 .. read32(bytes, command + 12) * 16];
                        strings = bytes[read32(bytes, command + 16)..][0..read32(bytes, command + 20)];
                    }
                    command += read32(bytes, command + 4);
                }
                return .{ .format = .macho, .text = text, .data = data, .symbols = symbols, .strings = strings, .relocation_size = 8 };
            },
            .freebsd, .openbsd, .netbsd, .other => return error.UnsupportedTarget,
        }
    }

    fn targetName(self: TestObjectTables, relocation: []const u8) []const u8 {
        const index = switch (self.format) {
            .elf => read32(relocation, 12),
            .coff => read32(relocation, 4),
            .macho => read32(relocation, 4) & 0x00ff_ffff,
        };
        const name_offset = switch (self.format) {
            .elf => read32(self.symbols, index * 24),
            .macho => read32(self.symbols, index * 16),
            .coff => read32(self.symbols, index * 18 + 4),
        };
        const name = std.mem.sliceTo(self.strings[name_offset..], 0);
        return if (self.format == .macho) name[1..] else name;
    }

    fn relocationType(self: TestObjectTables, record: []const u8) usize {
        return switch (self.format) {
            .elf => read32(record, 8),
            .coff => read16(record, 8),
            .macho => read32(record, 4) >> 28,
        };
    }

    fn offset(self: TestObjectTables, record: []const u8) u64 {
        return if (self.format == .elf) std.mem.readInt(u64, record[0..8], .little) else read32(record, 0);
    }

    fn read32(bytes: []const u8, at: usize) usize {
        return std.mem.readInt(u32, bytes[at..][0..4], .little);
    }

    fn read16(bytes: []const u8, at: usize) usize {
        return std.mem.readInt(u16, bytes[at..][0..2], .little);
    }
};

test "static data object collection scales linearly and preserves cyclic targets" {
    const allocator = std.testing.allocator;
    const small = @max(1, try fastestStaticObjectNs(allocator, 500));
    const large = try fastestStaticObjectNs(allocator, 8000);
    try std.testing.expect(large <= small * 64);
}

fn fastestStaticObjectNs(allocator: Allocator, count: usize) (@import("ObjectFileCompiler.zig").CompilationError || error{ NoSpaceLeft, InvalidObjectFile, SectionNotFound, TestExpectedEqual })!u64 {
    const Compiler = @import("ObjectFileCompiler.zig");
    const exports = try allocator.alloc(Compiler.StaticDataExport, count);
    defer allocator.free(exports);
    const relocations = try allocator.alloc(Compiler.StaticDataRelocation, count);
    defer allocator.free(relocations);
    const names = try allocator.alloc([32]u8, count);
    defer allocator.free(names);
    for (exports, names, 0..) |*data_export, *name, i| data_export.* = .{
        .symbol_name = try std.fmt.bufPrint(name, "static_target_{d:0>7}", .{i}),
        .bytes = &([_]u8{0} ** 8),
        .alignment = 8,
        .is_global = i % 2 == 0,
    };
    for (relocations, exports, 0..) |*relocation, *data_export, i| {
        const next = (i + 1) % count;
        relocation.* = .{
            .offset = 0,
            .target_symbol_name = exports[next].symbol_name,
            .target = .{ .data_symbol = @enumFromInt(next) },
            .addend = @intCast(i % 8),
        };
        data_export.relocations = relocations[i..][0..1];
    }
    var compiler = Compiler.ObjectFileCompiler.init(allocator);
    var fastest: u64 = std.math.maxInt(u64);
    for (0..3) |_| {
        const start = std.Io.Timestamp.now(std.testing.io, .awake).nanoseconds;
        var result = try compiler.compileStaticDataObject(exports, .x64linux);
        defer result.deinit();
        const end = std.Io.Timestamp.now(std.testing.io, .awake).nanoseconds;
        fastest = @min(fastest, @as(u64, @intCast(@max(0, end - start))));
        const decoded = try TestObjectTables.read(.x64linux, result.object_bytes);
        try std.testing.expectEqual(count, decoded.data.len / decoded.relocation_size);
        for (0..count) |i| {
            const record = decoded.data[i * decoded.relocation_size ..][0..decoded.relocation_size];
            try std.testing.expectEqual(@as(u64, @intCast(i * 8)), decoded.offset(record));
            try std.testing.expectEqualStrings(exports[(i + 1) % count].symbol_name, decoded.targetName(record));
            try std.testing.expectEqual(@as(i64, @intCast(i % 8)), std.mem.readInt(i64, record[16..24], .little));
        }
    }
    return fastest;
}

test "object encoding preserves borrowed sections through addend patches and allocation failure" {
    try std.testing.checkAllAllocationFailures(std.testing.allocator, exerciseBorrowedObjectSections, .{});
}

fn exerciseBorrowedObjectSections(allocator: Allocator) (Allocator.Error || error{ UnsupportedTarget, InvalidObjectFile, SectionNotFound, TestExpectedEqual })!void {
    for ([_]RocTarget{ .x64linux, .arm64linux, .x64mac, .arm64mac, .x64win, .arm64win }) |target| {
        var code = [_]u8{0} ** 16;
        var data = [_]u8{0} ** 16;
        var debug_line = [_]u8{0} ** 16;
        var debug_info = [_]u8{0} ** 16;
        const symbols = [_]Symbol{.{ .name = "local_target", .section = .rodata, .offset = 8, .size = 8, .is_global = false, .is_function = false, .is_external = false }};
        const data_relocations = [_]DataRelocation{.{ .offset = 0, .target_symbol_name = "local_target", .addend = 7 }};
        const debug: DebugSections = .{
            .line = &debug_line,
            .abbrev = &.{0},
            .info = &debug_info,
            .line_relocs = &.{.{ .section_offset = 0, .target = .text, .width = .eight, .addend = 3 }},
            .info_relocs = &.{
                .{ .section_offset = 0, .target = .debug_abbrev, .width = .four, .addend = 1 },
                .{ .section_offset = 8, .target = .debug_line, .width = .eight, .addend = 2 },
            },
        };
        var output: std.ArrayList(u8) = .empty;
        defer output.deinit(allocator);
        try generateObjectFileWithDebug(allocator, target, &code, &data, &symbols, &.{}, &data_relocations, debug, &output);
        try std.testing.expectEqualSlices(u8, &([_]u8{0} ** 16), &code);
        try std.testing.expectEqualSlices(u8, &([_]u8{0} ** 16), &data);
        try std.testing.expectEqualSlices(u8, &([_]u8{0} ** 16), &debug_line);
        try std.testing.expectEqualSlices(u8, &([_]u8{0} ** 16), &debug_info);
        const section = try readonlySection(target, output.items);
        const expected_addend: u64 = if (target.toOsTag() == .linux) 0 else 7;
        try std.testing.expectEqual(expected_addend, std.mem.readInt(u64, section[0..8], .little));
        if (target.toOsTag() == .macos) {
            const info = try machoSection(output.items, "__debug_info");
            try std.testing.expectEqual(@as(u32, code.len + data.len + debug_line.len + 1), std.mem.readInt(u32, info[0..4], .little));
            try std.testing.expectEqual(@as(u64, code.len + data.len + 2), std.mem.readInt(u64, info[8..16], .little));
        }
    }
}
