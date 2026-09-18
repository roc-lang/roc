//! Relocatable ELF objects (`ET_REL`) for x86-64 and aarch64.
//!
//! The relocation arithmetic is adapted from the Zig compiler's
//! `src/link/Elf/Atom.zig` at https://codeberg.org/ziglang/zig, licensed
//! under the MIT license. Thanks, Zig team! The equations use the ELF
//! psABI names: S the target address, A the addend, P the field's address,
//! and G the address of the target's GOT slot.

const std = @import("std");
const loader = @import("mod.zig");
const aarch64 = @import("aarch64.zig");

const Allocator = std.mem.Allocator;
const LoadError = loader.LoadError;

const ehdr_size = 64;
const shdr_size = 64;
const sym_size = 24;
const rela_size = 24;

const ET_REL: u16 = 1;
const EM_X86_64: u16 = 62;
const EM_AARCH64: u16 = 183;

const SHT_SYMTAB: u32 = 2;
const SHT_RELA: u32 = 4;
const SHT_NOBITS: u32 = 8;
const SHT_REL: u32 = 9;

const SHF_WRITE: u64 = 0x1;
const SHF_ALLOC: u64 = 0x2;
const SHF_EXECINSTR: u64 = 0x4;
const SHF_TLS: u64 = 0x400;

const SHN_UNDEF: u16 = 0;
const SHN_LORESERVE: u16 = 0xff00;
const SHN_ABS: u16 = 0xfff1;
const SHN_COMMON: u16 = 0xfff2;

const STB_GLOBAL: u8 = 1;
const STB_WEAK: u8 = 2;

pub fn matches(bytes: []const u8) bool {
    return bytes.len >= ehdr_size and std.mem.eql(u8, bytes[0..4], "\x7fELF") and bytes[4] == 2 and bytes[5] == 1;
}

fn readU16(bytes: []const u8, offset: usize) u16 {
    return std.mem.readInt(u16, bytes[offset..][0..2], .little);
}

fn readU32(bytes: []const u8, offset: usize) u32 {
    return std.mem.readInt(u32, bytes[offset..][0..4], .little);
}

fn readU64(bytes: []const u8, offset: usize) u64 {
    return std.mem.readInt(u64, bytes[offset..][0..8], .little);
}

fn slice(bytes: []const u8, offset: u64, len: u64) LoadError![]const u8 {
    const start = std.math.cast(usize, offset) orelse return error.MalformedObject;
    const size = std.math.cast(usize, len) orelse return error.MalformedObject;
    if (start > bytes.len or size > bytes.len - start) return error.MalformedObject;
    return bytes[start..][0..size];
}

fn cString(table: []const u8, offset: u64) LoadError![]const u8 {
    const start = std.math.cast(usize, offset) orelse return error.MalformedObject;
    if (start >= table.len) return error.MalformedObject;
    const end = std.mem.indexOfScalarPos(u8, table, start, 0) orelse return error.MalformedObject;
    return table[start..end];
}

const Shdr = struct {
    name: u32,
    kind: u32,
    flags: u64,
    offset: u64,
    size: u64,
    link: u32,
    info: u32,
    addralign: u64,
    entsize: u64,
};

fn readShdr(bytes: []const u8, shoff: u64, index: usize) LoadError!Shdr {
    const raw = try slice(bytes, shoff + index * shdr_size, shdr_size);
    return .{
        .name = readU32(raw, 0),
        .kind = readU32(raw, 4),
        .flags = readU64(raw, 8),
        .offset = readU64(raw, 24),
        .size = readU64(raw, 32),
        .link = readU32(raw, 40),
        .info = readU32(raw, 44),
        .addralign = readU64(raw, 48),
        .entsize = readU64(raw, 56),
    };
}

pub fn parse(arena: Allocator, bytes: []const u8) LoadError!loader.Parsed {
    if (!matches(bytes)) return error.UnsupportedObject;
    if (readU16(bytes, 16) != ET_REL) return error.UnsupportedObject;
    const arch: std.Target.Cpu.Arch = switch (readU16(bytes, 18)) {
        EM_X86_64 => .x86_64,
        EM_AARCH64 => .aarch64,
        else => return error.UnsupportedObject,
    };
    const shoff = readU64(bytes, 40);
    if (readU16(bytes, 58) != shdr_size) return error.MalformedObject;
    const shnum: usize = readU16(bytes, 60);
    const shstrndx: usize = readU16(bytes, 62);
    if (shstrndx >= shnum) return error.MalformedObject;
    const shstr_hdr = try readShdr(bytes, shoff, shstrndx);
    const shstrtab = try slice(bytes, shstr_hdr.offset, shstr_hdr.size);

    // Allocatable sections become image sections; everything else is either
    // metadata read here or debug information the loader never maps.
    const section_map = try arena.alloc(?u32, shnum);
    @memset(section_map, null);
    var sections: std.ArrayList(loader.Section) = .empty;
    var symtab_index: ?usize = null;
    for (0..shnum) |index| {
        const shdr = try readShdr(bytes, shoff, index);
        if (shdr.kind == SHT_SYMTAB) {
            if (symtab_index != null) return error.MalformedObject;
            symtab_index = index;
        }
        if (shdr.flags & SHF_ALLOC == 0) continue;
        if (shdr.flags & SHF_TLS != 0) {
            std.debug.print("relocatable loader: object has a thread-local section {s}\n", .{try cString(shstrtab, shdr.name)});
            return error.UnsupportedObject;
        }
        const kind: loader.SectionKind = if (shdr.flags & SHF_EXECINSTR != 0)
            .code
        else if (shdr.flags & SHF_WRITE != 0)
            .writable
        else
            .readonly;
        const section_bytes: []const u8 = if (shdr.kind == SHT_NOBITS) &.{} else try slice(bytes, shdr.offset, shdr.size);
        section_map[index] = @intCast(sections.items.len);
        try sections.append(arena, .{
            .name = try cString(shstrtab, shdr.name),
            .kind = kind,
            .bytes = section_bytes,
            .size = shdr.size,
            .alignment = shdr.addralign,
        });
    }

    var symbols: std.ArrayList(loader.Symbol) = .empty;
    if (symtab_index) |index| {
        const symtab = try readShdr(bytes, shoff, index);
        if (symtab.entsize != sym_size) return error.MalformedObject;
        const strtab_hdr = try readShdr(bytes, shoff, symtab.link);
        const strtab = try slice(bytes, strtab_hdr.offset, strtab_hdr.size);
        const raw = try slice(bytes, symtab.offset, symtab.size);
        const count = raw.len / sym_size;
        try symbols.ensureTotalCapacityPrecise(arena, count);
        for (0..count) |i| {
            const entry = raw[i * sym_size ..][0..sym_size];
            const name = try cString(strtab, readU32(entry, 0));
            const info = entry[4];
            const binding: u8 = info >> 4;
            const shndx = readU16(entry, 6);
            const value = readU64(entry, 8);
            var symbol = loader.Symbol{
                .name = name,
                .kind = .undefined,
                .value = value,
                .weak = binding == STB_WEAK,
                .global = binding == STB_GLOBAL or binding == STB_WEAK,
            };
            if (i == 0) {
                // The null symbol: a section-relative relocation with no
                // symbol names it, and it contributes zero.
                symbol.kind = .absolute;
                symbol.value = 0;
            } else if (shndx == SHN_UNDEF) {
                symbol.kind = .undefined;
            } else if (shndx == SHN_ABS) {
                symbol.kind = .absolute;
            } else if (shndx == SHN_COMMON) {
                std.debug.print("relocatable loader: common symbol {s} is not supported\n", .{name});
                return error.UnsupportedObject;
            } else if (shndx >= SHN_LORESERVE or shndx >= shnum) {
                return error.MalformedObject;
            } else if (section_map[shndx]) |mapped| {
                symbol.kind = .defined;
                symbol.section = mapped;
            } else {
                symbol.kind = .unmapped;
            }
            symbols.appendAssumeCapacity(symbol);
        }
    }

    var relocations: std.ArrayList(loader.Relocation) = .empty;
    for (0..shnum) |index| {
        const shdr = try readShdr(bytes, shoff, index);
        if (shdr.kind == SHT_REL) {
            std.debug.print("relocatable loader: REL relocation sections are not supported\n", .{});
            return error.UnsupportedObject;
        }
        if (shdr.kind != SHT_RELA) continue;
        if (shdr.info >= shnum) return error.MalformedObject;
        const target_section = section_map[shdr.info] orelse continue;
        if (shdr.entsize != rela_size) return error.MalformedObject;
        const raw = try slice(bytes, shdr.offset, shdr.size);
        const count = raw.len / rela_size;
        try relocations.ensureUnusedCapacity(arena, count);
        for (0..count) |i| {
            const entry = raw[i * rela_size ..][0..rela_size];
            const info = readU64(entry, 8);
            const symbol_index: u32 = @intCast(info >> 32);
            if (symbol_index >= symbols.items.len) return error.MalformedObject;
            relocations.appendAssumeCapacity(.{
                .section = target_section,
                .offset = readU64(entry, 0),
                .target = .{ .symbol = symbol_index },
                .kind = @truncate(info),
                .addend = @bitCast(readU64(entry, 16)),
            });
        }
    }

    return .{
        .format = .elf,
        .arch = arch,
        .sections = sections.items,
        .symbols = symbols.items,
        .relocations = relocations.items,
    };
}

const R_X86_64 = std.elf.R_X86_64;
const R_AARCH64 = std.elf.R_AARCH64;

/// The enum member with `value`, or null for a value the enum does not name.
fn enumFromInt(comptime E: type, value: u32) ?E {
    inline for (@typeInfo(E).@"enum".fields) |field| {
        if (field.value == value) return @enumFromInt(field.value);
    }
    return null;
}

pub fn needsGot(arch: std.Target.Cpu.Arch, kind: u32) bool {
    return switch (arch) {
        .x86_64 => switch (enumFromInt(R_X86_64, kind) orelse return false) {
            .GOTPCREL, .GOTPCRELX, .REX_GOTPCRELX, .GOT32, .GOTPC32, .GOTPC64 => true,
            else => false,
        },
        .aarch64 => switch (enumFromInt(R_AARCH64, kind) orelse return false) {
            .ADR_GOT_PAGE, .LD64_GOT_LO12_NC => true,
            else => false,
        },
        else => false,
    };
}

pub fn isBranch(arch: std.Target.Cpu.Arch, kind: u32) bool {
    return switch (arch) {
        .x86_64 => switch (enumFromInt(R_X86_64, kind) orelse return false) {
            .PLT32, .PC32 => true,
            else => false,
        },
        .aarch64 => switch (enumFromInt(R_AARCH64, kind) orelse return false) {
            .CALL26, .JUMP26 => true,
            else => false,
        },
        else => false,
    };
}

pub fn apply(ctx: *loader.Apply, arch: std.Target.Cpu.Arch) LoadError!void {
    return switch (arch) {
        .x86_64 => applyX86_64(ctx),
        .aarch64 => applyAarch64(ctx),
        else => error.UnsupportedObject,
    };
}

fn fits(comptime T: type, value: i64) LoadError!T {
    return std.math.cast(T, value) orelse error.RelocationOutOfRange;
}

fn applyX86_64(ctx: *loader.Apply) LoadError!void {
    const kind = enumFromInt(R_X86_64, ctx.reloc.kind) orelse {
        std.debug.print("relocatable loader: unknown x86-64 relocation type {d}\n", .{ctx.reloc.kind});
        return error.UnsupportedRelocation;
    };
    const P = ctx.placeAddress();
    const A = ctx.addendValue();
    const S = ctx.symbolAddress();
    switch (kind) {
        .NONE => {},
        .@"64" => ctx.write(u64, @bitCast(S + A)),
        .PC64 => ctx.write(u64, @bitCast(S + A - P)),
        .PC32, .PLT32 => {
            // A direct call when the target is in reach, else through a stub.
            if (std.math.cast(i32, S + A - P)) |disp| {
                ctx.write(i32, disp);
            } else {
                const stub: i64 = @intCast(try ctx.stub());
                ctx.write(i32, try fits(i32, stub + A - P));
            }
        },
        .GOTPCREL, .GOTPCRELX, .REX_GOTPCRELX => {
            const G: i64 = @intCast(try ctx.gotSlot());
            ctx.write(i32, try fits(i32, G + A - P));
        },
        .@"32" => ctx.write(u32, try fits(u32, S + A)),
        .@"32S" => ctx.write(i32, try fits(i32, S + A)),
        else => return ctx.unsupported(@tagName(kind)),
    }
}

fn applyAarch64(ctx: *loader.Apply) LoadError!void {
    const kind = enumFromInt(R_AARCH64, ctx.reloc.kind) orelse {
        std.debug.print("relocatable loader: unknown aarch64 relocation type {d}\n", .{ctx.reloc.kind});
        return error.UnsupportedRelocation;
    };
    const P = ctx.placeAddress();
    const A = ctx.addendValue();
    const S = ctx.symbolAddress();
    if (ctx.code.len < 4) return error.MalformedObject;
    const inst = ctx.code[0..4];
    switch (kind) {
        .NONE => {},
        .ABS64 => ctx.write(u64, @bitCast(S + A)),
        .ABS32 => ctx.write(u32, try fits(u32, S + A)),
        .PREL64 => ctx.write(u64, @bitCast(S + A - P)),
        .PREL32 => ctx.write(u32, @bitCast(try fits(i32, S + A - P))),
        .CALL26, .JUMP26 => {
            const disp: i28 = std.math.cast(i28, S + A - P) orelse blk: {
                const stub: i64 = @intCast(try ctx.stub());
                break :blk try fits(i28, stub + A - P);
            };
            aarch64.writeBranchImm(inst, disp);
        },
        .CONDBR19 => aarch64.writeCondBranchImm(inst, try fits(i21, S + A - P)),
        .ADR_PREL_LO21 => aarch64.writeAdrImm(inst, try fits(i21, S + A - P)),
        .ADR_PREL_PG_HI21 => aarch64.writeAdrImm(inst, aarch64.calcNumberOfPages(P, S + A) catch return error.RelocationOutOfRange),
        .ADD_ABS_LO12_NC => aarch64.writeAddImm(inst, @truncate(@as(u64, @bitCast(S + A)))),
        .LDST8_ABS_LO12_NC, .LDST16_ABS_LO12_NC, .LDST32_ABS_LO12_NC, .LDST64_ABS_LO12_NC, .LDST128_ABS_LO12_NC => {
            const scale: u4 = switch (kind) {
                .LDST8_ABS_LO12_NC => 0,
                .LDST16_ABS_LO12_NC => 1,
                .LDST32_ABS_LO12_NC => 2,
                .LDST64_ABS_LO12_NC => 3,
                .LDST128_ABS_LO12_NC => 4,
                else => unreachable,
            };
            const low: u12 = @truncate(@as(u64, @bitCast(S + A)));
            const unit: u12 = @as(u12, 1) << scale;
            if (low % unit != 0) return error.MalformedObject;
            aarch64.writeLoadStoreImm(inst, low / unit);
        },
        .ADR_GOT_PAGE => {
            const G: i64 = @intCast(try ctx.gotSlot());
            aarch64.writeAdrImm(inst, aarch64.calcNumberOfPages(P, G + A) catch return error.RelocationOutOfRange);
        },
        .LD64_GOT_LO12_NC => {
            const G: i64 = @intCast(try ctx.gotSlot());
            const low: u12 = @truncate(@as(u64, @bitCast(G + A)));
            if (low % 8 != 0) return error.MalformedObject;
            aarch64.writeLoadStoreImm(inst, low / 8);
        },
        else => return ctx.unsupported(@tagName(kind)),
    }
}
