//! Relocatable COFF objects for x64 and ARM64.
//!
//! The x64 relocation arithmetic is adapted from the Zig compiler's
//! `src/link/Coff.zig` at https://codeberg.org/ziglang/zig, licensed under
//! the MIT license. Thanks, Zig team! COFF keeps every addend in the patched
//! field or instruction, so each relocation reads its addend back before
//! writing.
//!
//! On Windows the loader also registers the object's `.pdata` unwind table
//! with the process, so unwinding and exception dispatch can pass through
//! the loaded code.

const std = @import("std");
const builtin = @import("builtin");
const loader = @import("mod.zig");
const memory = @import("memory.zig");
const aarch64 = @import("aarch64.zig");

const Allocator = std.mem.Allocator;
const LoadError = loader.LoadError;

const header_size = 20;
const section_header_size = 40;
const symbol_size = 18;
const reloc_size = 10;

const IMAGE_FILE_MACHINE_AMD64: u16 = 0x8664;
const IMAGE_FILE_MACHINE_ARM64: u16 = 0xaa64;

const IMAGE_SCN_CNT_CODE: u32 = 0x20;
const IMAGE_SCN_CNT_UNINITIALIZED_DATA: u32 = 0x80;
const IMAGE_SCN_LNK_INFO: u32 = 0x200;
const IMAGE_SCN_LNK_REMOVE: u32 = 0x800;
const IMAGE_SCN_LNK_NRELOC_OVFL: u32 = 0x01000000;
const IMAGE_SCN_MEM_DISCARDABLE: u32 = 0x02000000;
const IMAGE_SCN_MEM_EXECUTE: u32 = 0x20000000;
const IMAGE_SCN_MEM_WRITE: u32 = 0x80000000;

const IMAGE_SYM_UNDEFINED: i16 = 0;
const IMAGE_SYM_ABSOLUTE: i16 = -1;
const IMAGE_SYM_DEBUG: i16 = -2;

const IMAGE_SYM_CLASS_EXTERNAL: u8 = 2;
const IMAGE_SYM_CLASS_STATIC: u8 = 3;
const IMAGE_SYM_CLASS_WEAK_EXTERNAL: u8 = 105;

pub fn matches(bytes: []const u8) bool {
    if (bytes.len < header_size) return false;
    const machine = std.mem.readInt(u16, bytes[0..2], .little);
    return (machine == IMAGE_FILE_MACHINE_AMD64 or machine == IMAGE_FILE_MACHINE_ARM64) and
        std.mem.readInt(u16, bytes[16..18], .little) == 0;
}

fn readU16(bytes: []const u8, offset: usize) u16 {
    return std.mem.readInt(u16, bytes[offset..][0..2], .little);
}

fn readU32(bytes: []const u8, offset: usize) u32 {
    return std.mem.readInt(u32, bytes[offset..][0..4], .little);
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

/// An eight-byte name field: inline when it fits, else `/offset` into the
/// string table for sections and a zero prefix plus offset for symbols.
fn shortName(raw: *const [8]u8) []const u8 {
    return std.mem.sliceTo(raw, 0);
}

pub fn parse(arena: Allocator, bytes: []const u8) LoadError!loader.Parsed {
    if (!matches(bytes)) return error.UnsupportedObject;
    const arch: std.Target.Cpu.Arch = switch (readU16(bytes, 0)) {
        IMAGE_FILE_MACHINE_AMD64 => .x86_64,
        IMAGE_FILE_MACHINE_ARM64 => .aarch64,
        else => return error.UnsupportedObject,
    };
    const section_count: usize = readU16(bytes, 2);
    const symtab_offset = readU32(bytes, 8);
    const symbol_count = readU32(bytes, 12);
    const symtab = try slice(bytes, symtab_offset, @as(u64, symbol_count) * symbol_size);
    const strtab_offset = @as(u64, symtab_offset) + @as(u64, symbol_count) * symbol_size;
    const strtab: []const u8 = if (strtab_offset + 4 <= bytes.len) blk: {
        const strtab_len = readU32(bytes, @intCast(strtab_offset));
        if (strtab_len < 4) break :blk &.{};
        break :blk try slice(bytes, strtab_offset, strtab_len);
    } else &.{};

    const RawSection = struct {
        characteristics: u32,
        reloc_offset: u32,
        reloc_count: u32,
        mapped: ?u32,
    };
    var raw_sections: std.ArrayList(RawSection) = .empty;
    try raw_sections.ensureTotalCapacityPrecise(arena, section_count);
    var sections: std.ArrayList(loader.Section) = .empty;
    const headers = try slice(bytes, header_size, @as(u64, section_count) * section_header_size);
    for (0..section_count) |i| {
        const hdr = headers[i * section_header_size ..][0..section_header_size];
        const name_field = hdr[0..8];
        const name = if (name_field[0] == '/')
            try cString(strtab, std.fmt.parseInt(u32, std.mem.sliceTo(name_field[1..], 0), 10) catch return error.MalformedObject)
        else
            shortName(name_field);
        const virtual_size = readU32(hdr, 8);
        const raw_size = readU32(hdr, 16);
        const raw_offset = readU32(hdr, 20);
        const characteristics = readU32(hdr, 36);
        const skip = characteristics & (IMAGE_SCN_LNK_INFO | IMAGE_SCN_LNK_REMOVE | IMAGE_SCN_MEM_DISCARDABLE) != 0;
        const is_nobits = characteristics & IMAGE_SCN_CNT_UNINITIALIZED_DATA != 0;
        const size: u64 = if (is_nobits) @max(virtual_size, raw_size) else raw_size;
        const kind: loader.SectionKind = if (characteristics & (IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE) != 0)
            .code
        else if (characteristics & IMAGE_SCN_MEM_WRITE != 0)
            .writable
        else
            .readonly;
        const align_field: u32 = (characteristics >> 20) & 0xf;
        const alignment: u64 = if (align_field == 0) 16 else @as(u64, 1) << @intCast(align_field - 1);
        const section_bytes: []const u8 = if (is_nobits or raw_offset == 0) &.{} else try slice(bytes, raw_offset, raw_size);
        var mapped: ?u32 = null;
        if (!skip) {
            mapped = @intCast(sections.items.len);
            try sections.append(arena, .{
                .name = name,
                .kind = kind,
                .bytes = section_bytes,
                .size = size,
                .alignment = alignment,
            });
        }
        raw_sections.appendAssumeCapacity(.{
            .characteristics = characteristics,
            .reloc_offset = readU32(hdr, 24),
            .reloc_count = readU16(hdr, 32),
            .mapped = mapped,
        });
    }

    // Relocations index the symbol table including auxiliary records, so
    // every record gets an entry; auxiliary ones are placeholders.
    var symbols: std.ArrayList(loader.Symbol) = .empty;
    try symbols.ensureTotalCapacityPrecise(arena, symbol_count);
    var i: usize = 0;
    while (i < symbol_count) : (i += 1) {
        const entry = symtab[i * symbol_size ..][0..symbol_size];
        const name = if (readU32(entry, 0) == 0)
            try cString(strtab, readU32(entry, 4))
        else
            shortName(entry[0..8]);
        const value = readU32(entry, 8);
        const section_number: i16 = @bitCast(readU16(entry, 12));
        const storage_class = entry[16];
        const aux_count = entry[17];
        var symbol = loader.Symbol{
            .name = name,
            .kind = .unmapped,
            .value = value,
            .global = storage_class == IMAGE_SYM_CLASS_EXTERNAL or storage_class == IMAGE_SYM_CLASS_WEAK_EXTERNAL,
            .weak = storage_class == IMAGE_SYM_CLASS_WEAK_EXTERNAL,
        };
        if (section_number == IMAGE_SYM_UNDEFINED) {
            if (value != 0 and storage_class == IMAGE_SYM_CLASS_EXTERNAL) {
                std.debug.print("relocatable loader: common symbol {s} is not supported\n", .{name});
                return error.UnsupportedObject;
            }
            symbol.kind = .undefined;
        } else if (section_number == IMAGE_SYM_ABSOLUTE) {
            symbol.kind = .absolute;
        } else if (section_number > 0) {
            const index: usize = @intCast(section_number - 1);
            if (index >= raw_sections.items.len) return error.MalformedObject;
            if (raw_sections.items[index].mapped) |mapped| {
                symbol.kind = .defined;
                symbol.section = mapped;
            }
        }
        symbols.appendAssumeCapacity(symbol);
        for (0..aux_count) |_| {
            i += 1;
            if (i >= symbol_count) return error.MalformedObject;
            symbols.appendAssumeCapacity(.{ .name = "", .kind = .unmapped });
        }
    }

    var relocations: std.ArrayList(loader.Relocation) = .empty;
    for (raw_sections.items) |raw| {
        const mapped = raw.mapped orelse continue;
        if (raw.reloc_count == 0) continue;
        var count: u64 = raw.reloc_count;
        var first: u64 = 0;
        if (raw.characteristics & IMAGE_SCN_LNK_NRELOC_OVFL != 0) {
            // The first entry carries the real count.
            const overflow = try slice(bytes, raw.reloc_offset, reloc_size);
            count = readU32(overflow, 0);
            first = 1;
        }
        const entries = try slice(bytes, raw.reloc_offset, count * reloc_size);
        try relocations.ensureUnusedCapacity(arena, @intCast(count - first));
        var k: u64 = first;
        while (k < count) : (k += 1) {
            const entry = entries[@intCast(k * reloc_size)..][0..reloc_size];
            const symbol_index = readU32(entry, 4);
            if (symbol_index >= symbols.items.len) return error.MalformedObject;
            relocations.appendAssumeCapacity(.{
                .section = mapped,
                .offset = readU32(entry, 0),
                .target = .{ .symbol = symbol_index },
                .kind = readU16(entry, 8),
                .addend = 0,
            });
        }
    }

    return .{
        .format = .coff,
        .arch = arch,
        .sections = sections.items,
        .symbols = symbols.items,
        .relocations = relocations.items,
    };
}

const Amd64Kind = enum(u16) {
    absolute = 0,
    addr64 = 1,
    addr32 = 2,
    addr32nb = 3,
    rel32 = 4,
    rel32_1 = 5,
    rel32_2 = 6,
    rel32_3 = 7,
    rel32_4 = 8,
    rel32_5 = 9,
    section = 10,
    secrel = 11,
    _,
};

const Arm64Kind = enum(u16) {
    absolute = 0,
    addr32 = 1,
    addr32nb = 2,
    branch26 = 3,
    pagebase_rel21 = 4,
    rel21 = 5,
    pageoffset_12a = 6,
    pageoffset_12l = 7,
    secrel = 8,
    secrel_low12a = 9,
    secrel_high12a = 10,
    secrel_low12l = 11,
    token = 12,
    section = 13,
    addr64 = 14,
    branch19 = 15,
    branch14 = 16,
    rel32 = 17,
    _,
};

pub fn isBranch(arch: std.Target.Cpu.Arch, kind: u32) bool {
    return switch (arch) {
        .x86_64 => switch (@as(Amd64Kind, @enumFromInt(@as(u16, @truncate(kind))))) {
            .rel32, .rel32_1, .rel32_2, .rel32_3, .rel32_4, .rel32_5 => true,
            else => false,
        },
        .aarch64 => @as(Arm64Kind, @enumFromInt(@as(u16, @truncate(kind)))) == .branch26,
        else => false,
    };
}

pub fn apply(ctx: *loader.Apply, arch: std.Target.Cpu.Arch) LoadError!void {
    return switch (arch) {
        .x86_64 => applyAmd64(ctx),
        .aarch64 => applyArm64(ctx),
        else => error.UnsupportedObject,
    };
}

fn fits(comptime T: type, value: i64) LoadError!T {
    return std.math.cast(T, value) orelse error.RelocationOutOfRange;
}

/// The target's offset within its own section, for SECREL fields.
fn sectionRelative(ctx: *loader.Apply) LoadError!i64 {
    const target_section: usize = switch (ctx.reloc.target) {
        .symbol => |index| blk: {
            const symbol = ctx.loader.parsed.symbols[index];
            if (symbol.kind != .defined) return error.MalformedObject;
            break :blk ctx.loader.parsed.sections[symbol.section].address;
        },
        .section => |section| ctx.loader.parsed.sections[section.index].address,
    };
    return @as(i64, @intCast(ctx.target)) - @as(i64, @intCast(target_section));
}

fn applyAmd64(ctx: *loader.Apply) LoadError!void {
    const kind: Amd64Kind = @enumFromInt(@as(u16, @truncate(ctx.reloc.kind)));
    const P = ctx.placeAddress();
    const S = ctx.symbolAddress();
    switch (kind) {
        .absolute => {},
        .addr64 => {
            if (ctx.code.len < 8) return error.MalformedObject;
            const A: i64 = @bitCast(ctx.read(u64));
            ctx.write(u64, @bitCast(S + A));
        },
        .addr32nb => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const A: i64 = ctx.read(u32);
            const base: i64 = @intCast(ctx.imageBase());
            ctx.write(u32, try fits(u32, S + A - base));
        },
        .rel32, .rel32_1, .rel32_2, .rel32_3, .rel32_4, .rel32_5 => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const trailing: i64 = @intFromEnum(kind) - @intFromEnum(Amd64Kind.rel32);
            const A: i64 = ctx.read(i32);
            const next = P + 4 + trailing;
            if (std.math.cast(i32, S + A - next)) |disp| {
                ctx.write(i32, disp);
            } else {
                const stub: i64 = @intCast(try ctx.stub());
                ctx.write(i32, try fits(i32, stub + A - next));
            }
        },
        .secrel => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const A: i64 = ctx.read(u32);
            ctx.write(u32, try fits(u32, try sectionRelative(ctx) + A));
        },
        .addr32 => return ctx.unsupported("IMAGE_REL_AMD64_ADDR32"),
        .section => return ctx.unsupported("IMAGE_REL_AMD64_SECTION"),
        _ => return ctx.unsupported("IMAGE_REL_AMD64 (unknown)"),
    }
}

fn applyArm64(ctx: *loader.Apply) LoadError!void {
    const kind: Arm64Kind = @enumFromInt(@as(u16, @truncate(ctx.reloc.kind)));
    const P = ctx.placeAddress();
    const S = ctx.symbolAddress();
    switch (kind) {
        .absolute => {},
        .addr64 => {
            if (ctx.code.len < 8) return error.MalformedObject;
            const A: i64 = @bitCast(ctx.read(u64));
            ctx.write(u64, @bitCast(S + A));
        },
        .addr32nb => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const A: i64 = ctx.read(u32);
            const base: i64 = @intCast(ctx.imageBase());
            ctx.write(u32, try fits(u32, S + A - base));
        },
        .rel32 => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const A: i64 = ctx.read(i32);
            ctx.write(i32, try fits(i32, S + A - (P + 4)));
        },
        .branch26 => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const inst = ctx.code[0..4];
            const A: i64 = aarch64.readBranchImm(inst);
            const disp: i28 = std.math.cast(i28, S + A - P) orelse blk: {
                const stub: i64 = @intCast(try ctx.stub());
                break :blk try fits(i28, stub + A - P);
            };
            aarch64.writeBranchImm(inst, disp);
        },
        .pagebase_rel21 => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const inst = ctx.code[0..4];
            const A: i64 = @as(i64, aarch64.readAdrImm(inst)) << 12;
            aarch64.writeAdrImm(inst, aarch64.calcNumberOfPages(P, S + A) catch return error.RelocationOutOfRange);
        },
        .pageoffset_12a => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const inst = ctx.code[0..4];
            const A: i64 = aarch64.readAddImm(inst);
            aarch64.writeAddImm(inst, @truncate(@as(u64, @bitCast(S + A))));
        },
        .pageoffset_12l => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const inst = ctx.code[0..4];
            const scale = aarch64.loadStoreScale(inst);
            const A: i64 = @as(i64, aarch64.readAddImm(inst)) << scale;
            aarch64.writePageOffset(inst, @bitCast(S + A)) catch return error.MalformedObject;
        },
        .secrel => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const A: i64 = ctx.read(u32);
            ctx.write(u32, try fits(u32, try sectionRelative(ctx) + A));
        },
        .addr32, .rel21, .secrel_low12a, .secrel_high12a, .secrel_low12l, .token, .section, .branch19, .branch14 => return ctx.unsupported(@tagName(kind)),
        _ => return ctx.unsupported("IMAGE_REL_ARM64 (unknown)"),
    }
}

/// The unwind table the loader registered with Windows, if any.
pub const UnwindRegistration = struct {
    entries: ?*anyopaque = null,

    pub fn deinit(self: *UnwindRegistration) void {
        if (builtin.os.tag != .windows) return;
        if (self.entries) |entries| _ = ntdll.RtlDeleteFunctionTable(entries);
        self.entries = null;
    }
};

const ntdll = struct {
    extern "ntdll" fn RtlAddFunctionTable(FunctionTable: *anyopaque, EntryCount: u32, BaseAddress: u64) callconv(.winapi) u8;
    extern "ntdll" fn RtlDeleteFunctionTable(FunctionTable: *anyopaque) callconv(.winapi) u8;
};

/// Register the image's `.pdata` with the process so the unwinder knows the
/// loaded functions. A no-op away from Windows.
pub fn registerUnwind(parsed: loader.Parsed, mapping: *const memory.Mapping) UnwindRegistration {
    if (builtin.os.tag != .windows) return .{};
    const entry_size: usize = if (parsed.arch == .x86_64) 12 else 8;
    for (parsed.sections) |section| {
        if (!std.mem.eql(u8, section.name, ".pdata") or section.size == 0) continue;
        const count: u32 = @intCast(section.size / entry_size);
        const entries: *anyopaque = @ptrFromInt(section.address);
        if (ntdll.RtlAddFunctionTable(entries, count, @intFromPtr(mapping.base)) == 0) return .{};
        return .{ .entries = entries };
    }
    return .{};
}
