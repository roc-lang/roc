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

/// A section referenced by a field in a DWARF debug section.
pub const DebugRelocTarget = enum {
    text,
    debug_line,
    debug_abbrev,
};

/// The encoded width of a relocated field in a DWARF debug section.
pub const DebugRelocWidth = enum {
    four,
    eight,
};

/// One field in a DWARF debug section that the object linker must relocate
/// to `target section start + addend`.
pub const DebugReloc = struct {
    section_offset: u32,
    target: DebugRelocTarget,
    width: DebugRelocWidth,
    addend: u64,
};

/// The DWARF debug sections an object carries, borrowed from the caller, and
/// the relocations saying what each address field inside them refers to.
pub const DebugSections = struct {
    line: []const u8 = &.{},
    abbrev: []const u8 = &.{},
    info: []const u8 = &.{},
    line_relocs: []const DebugReloc = &.{},
    info_relocs: []const DebugReloc = &.{},
};

pub const ElfWriter = elf.ElfWriter;
pub const MachOWriter = macho.MachOWriter;
pub const CoffWriter = coff.CoffWriter;

test "object module imports" {
    std.testing.refAllDecls(@This());
}
