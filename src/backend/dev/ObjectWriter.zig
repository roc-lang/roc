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
) !void {
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

            // Add symbols (underscore prefix for C ABI is added in MachOWriter.write())
            for (symbols) |sym| {
                const sym_idx = try macho.addSymbol(.{
                    .name = sym.name,
                    .section = if (sym.is_external) 0 else machoSectionNumber(sym.section),
                    .offset = sym.offset,
                    .is_external = sym.is_global or sym.is_external,
                });

                // Add relocations for this symbol
                for (relocations) |rel| {
                    switch (rel) {
                        .linked_function => |f| if (std.mem.eql(u8, f.name, sym.name)) {
                            try macho.addTextRelocation(@intCast(rel.getOffset()), sym_idx, sym.is_external);
                        },
                        .linked_data => |d| if (std.mem.eql(u8, d.name, sym.name)) {
                            try macho.addTextDataRelocation(@intCast(rel.getOffset()), sym_idx, d.kind);
                        },
                        else => {},
                    }
                }

                for (rodata_relocations) |rel| {
                    if (std.mem.eql(u8, rel.target_symbol_name, sym.name)) {
                        try macho.addRodataRelocation(@intCast(rel.offset), sym_idx, true, rel.addend);
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

    var output: std.ArrayList(u8) = .{};
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

    var output: std.ArrayList(u8) = .{};
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

    var output: std.ArrayList(u8) = .{};
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

    var output: std.ArrayList(u8) = .{};
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

    var output: std.ArrayList(u8) = .{};
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

    var output: std.ArrayList(u8) = .{};
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
