//! Unified dev backend for generating native object files.
//!
//! This module integrates code generation with object file writing
//! to produce complete relocatable object files from CIR.

const std = @import("std");
const Allocator = std.mem.Allocator;

const object = @import("object/mod.zig");
const x86_64 = @import("x86_64/mod.zig");
const aarch64 = @import("aarch64/mod.zig");
const Relocation = @import("Relocation.zig").Relocation;

/// Target architecture for code generation
pub const Architecture = enum {
    x86_64,
    aarch64,

    pub fn fromTarget(target: anytype) Architecture {
        const arch = if (@hasField(@TypeOf(target), "cpu"))
            target.cpu.arch
        else if (@hasField(@TypeOf(target), "arch"))
            target.arch
        else
            @compileError("Unknown target type");

        return switch (arch) {
            .x86_64 => .x86_64,
            .aarch64 => .aarch64,
            else => @panic("Unsupported architecture for dev backend"),
        };
    }
};

/// Target operating system
pub const OperatingSystem = enum {
    linux,
    macos,
    windows,

    pub fn fromTarget(target: anytype) OperatingSystem {
        const os_tag = if (@hasField(@TypeOf(target), "os"))
            target.os.tag
        else if (@hasField(@TypeOf(target), "os_tag"))
            target.os_tag
        else
            @compileError("Unknown target type");

        return switch (os_tag) {
            .linux => .linux,
            .macos => .macos,
            .windows => .windows,
            else => .linux, // Default to Linux for other Unix-like systems
        };
    }
};

/// Generate an object file from code and relocations.
///
/// This is the main entry point for the dev backend. It takes generated
/// machine code and produces a relocatable object file.
pub fn generateObjectFile(
    allocator: Allocator,
    arch: Architecture,
    os: OperatingSystem,
    code: []const u8,
    symbols: []const Symbol,
    relocations: []const Relocation,
    output: *std.ArrayList(u8),
) !void {
    switch (os) {
        .linux => {
            var elf = try object.ElfWriter.init(allocator, switch (arch) {
                .x86_64 => .x86_64,
                .aarch64 => .aarch64,
            });
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
            var macho = try object.MachOWriter.init(allocator, switch (arch) {
                .x86_64 => .x86_64,
                .aarch64 => .aarch64,
            });
            defer macho.deinit();

            try macho.setCode(code);

            // Add symbols
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
            var coff_writer = try object.CoffWriter.init(allocator, switch (arch) {
                .x86_64 => .x86_64,
                .aarch64 => .aarch64,
            });
            defer coff_writer.deinit();

            try coff_writer.setCode(code);

            // Add symbols
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
            }

            try coff_writer.write(output);
        },
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
        .x86_64,
        .linux,
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

    const symbols = &[_]Symbol{
        .{
            .name = "_test_func",
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
        .x86_64,
        .macos,
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
        .aarch64,
        .linux,
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
        .x86_64,
        .windows,
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
        .aarch64,
        .windows,
        code,
        symbols,
        &.{},
        &output,
    );

    // Verify COFF machine type (ARM64 = 0xAA64)
    const machine = std.mem.readInt(u16, output.items[0..2], .little);
    try std.testing.expectEqual(@as(u16, 0xAA64), machine);
}
