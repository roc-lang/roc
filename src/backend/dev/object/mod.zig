//! Object file generation for the dev backend.
//!
//! This module provides writers for different object file formats:
//! - ELF: Linux and other Unix-like systems
//! - Mach-O: macOS and iOS
//! - COFF: Windows
//!
//! Each writer takes generated machine code and relocations and produces
//! a relocatable object file that can be linked with other objects.

const std = @import("std");

pub const elf = @import("elf.zig");
pub const macho = @import("macho.zig");
pub const coff = @import("coff.zig");

pub const ElfWriter = elf.ElfWriter;
pub const MachOWriter = macho.MachOWriter;
pub const CoffWriter = coff.CoffWriter;

/// Target object format based on operating system
pub const ObjectFormat = enum {
    elf,
    macho,
    coff,

    pub fn fromTarget(target: anytype) ObjectFormat {
        // Check if it's a Zig target or base.target.Target
        const os_tag = if (@hasField(@TypeOf(target), "os"))
            target.os.tag
        else if (@hasField(@TypeOf(target), "os_tag"))
            target.os_tag
        else
            @compileError("Unknown target type");

        return switch (os_tag) {
            .linux, .freebsd, .openbsd, .netbsd => .elf,
            .macos, .ios, .tvos, .watchos => .macho,
            .windows => .coff,
            else => .elf, // Default to ELF
        };
    }
};

test "object module imports" {
    std.testing.refAllDecls(@This());
}
