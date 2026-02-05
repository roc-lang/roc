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
    symbols: []const Symbol,
    relocations: []const Relocation,
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

            // Add symbols
            for (symbols) |sym| {
                const sym_idx = try elf.addSymbol(.{
                    .name = sym.name,
                    .section = if (sym.is_external) .undef else .text,
                    .offset = sym.offset,
                    .size = sym.size,
                    .is_global = sym.is_global,
                    .is_function = sym.is_function,
                });

                // Add relocations for this symbol
                for (relocations) |rel| {
                    const rel_name = switch (rel) {
                        .linked_function => |f| f.name,
                        .linked_data => |d| d.name,
                        else => continue,
                    };
                    if (std.mem.eql(u8, rel_name, sym.name)) {
                        try elf.addTextRelocation(rel.getOffset(), sym_idx, 0);
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

            // Add symbols (underscore prefix for C ABI is added in MachOWriter.write())
            for (symbols) |sym| {
                const sym_idx = try macho.addSymbol(.{
                    .name = sym.name,
                    .section = if (sym.is_external) 0 else 1, // 0 = NO_SECT, 1 = __text
                    .offset = sym.offset,
                    .is_external = sym.is_global or sym.is_external,
                });

                // Add relocations for this symbol
                for (relocations) |rel| {
                    const rel_name = switch (rel) {
                        .linked_function => |f| f.name,
                        .linked_data => |d| d.name,
                        else => continue,
                    };
                    if (std.mem.eql(u8, rel_name, sym.name)) {
                        try macho.addTextRelocation(@intCast(rel.getOffset()), sym_idx, sym.is_external);
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

            // Add symbols and function info for unwind tables
            for (symbols) |sym| {
                const sym_idx = try coff_writer.addSymbol(.{
                    .name = sym.name,
                    .section = if (sym.is_external) .undef else .text,
                    .offset = @intCast(sym.offset),
                    .is_global = sym.is_global,
                    .is_function = sym.is_function,
                });

                // Add relocations for this symbol
                for (relocations) |rel| {
                    const rel_name = switch (rel) {
                        .linked_function => |f| f.name,
                        .linked_data => |d| d.name,
                        else => continue,
                    };
                    if (std.mem.eql(u8, rel_name, sym.name)) {
                        try coff_writer.addTextRelocation(@intCast(rel.getOffset()), sym_idx);
                    }
                }

                // Add function info for Windows x64 unwind tables
                if (coff_arch == .x86_64 and sym.is_function and !sym.is_external) {
                    try coff_writer.addFunctionInfo(.{
                        .start_offset = @intCast(sym.offset),
                        .end_offset = @intCast(sym.offset + sym.size),
                        .prologue_size = sym.prologue_size,
                        .frame_reg_offset = 0, // RSP offset not scaled for our simple case
                        .uses_frame_pointer = sym.uses_frame_pointer,
                        .stack_alloc = sym.stack_alloc,
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
    offset: u64,
    size: u64,
    is_global: bool,
    is_function: bool,
    is_external: bool,
    // Unwind info for Windows x64
    prologue_size: u8 = 0,
    stack_alloc: u32 = 0,
    uses_frame_pointer: bool = true,
};

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
        symbols,
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
        symbols,
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
        symbols,
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
        symbols,
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
        symbols,
        &.{},
        &output,
    );

    // Verify COFF machine type (ARM64 = 0xAA64)
    const machine = std.mem.readInt(u16, output.items[0..2], .little);
    try std.testing.expectEqual(@as(u16, 0xAA64), machine);
}
