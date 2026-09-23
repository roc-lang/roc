//! Relocatable Mach-O objects (`MH_OBJECT`) for x86-64 and arm64.
//!
//! Relocation parsing and arithmetic are adapted from the Zig compiler's
//! `src/link/MachO/Object.zig` and `src/link/MachO/Atom.zig` at
//! https://codeberg.org/ziglang/zig, licensed under the MIT license. Thanks,
//! Zig team!
//!
//! Mach-O keeps a relocation's addend in the patched field (or, on arm64,
//! in a preceding `ARM64_RELOC_ADDEND` entry) and can aim a relocation at a
//! section rather than a symbol; the parser turns both into the loader's
//! symbol-or-section targets with explicit addends. Symbol names lose their
//! leading underscore so the resolver and lookups see C names.

const std = @import("std");
const loader = @import("mod.zig");
const aarch64 = @import("aarch64.zig");

const Allocator = std.mem.Allocator;
const LoadError = loader.LoadError;

const MH_MAGIC_64: u32 = 0xfeedfacf;
const MH_OBJECT: u32 = 1;
const CPU_TYPE_X86_64: u32 = 0x01000007;
const CPU_TYPE_ARM64: u32 = 0x0100000c;
const LC_SEGMENT_64: u32 = 0x19;
const LC_SYMTAB: u32 = 0x2;

const header_size = 32;
const segment_command_size = 72;
const section_size = 80;
const nlist_size = 16;
const reloc_size = 8;

const SECTION_TYPE: u32 = 0xff;
const S_ZEROFILL: u32 = 0x1;
const S_THREAD_LOCAL_REGULAR: u32 = 0x11;
const S_THREAD_LOCAL_INIT_FUNCTION_POINTERS: u32 = 0x15;
const S_ATTR_DEBUG: u32 = 0x02000000;
const S_ATTR_PURE_INSTRUCTIONS: u32 = 0x80000000;
const S_ATTR_SOME_INSTRUCTIONS: u32 = 0x00000400;

const N_STAB: u8 = 0xe0;
const N_TYPE: u8 = 0x0e;
const N_EXT: u8 = 0x01;
const N_UNDF: u8 = 0x0;
const N_ABS: u8 = 0x2;
const N_SECT: u8 = 0xe;
const N_WEAK_REF: u16 = 0x0040;
const N_WEAK_DEF: u16 = 0x0080;

pub fn matches(bytes: []const u8) bool {
    return bytes.len >= header_size and std.mem.readInt(u32, bytes[0..4], .little) == MH_MAGIC_64;
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

fn fixedName(raw: *const [16]u8) []const u8 {
    return std.mem.sliceTo(raw, 0);
}

fn cString(table: []const u8, offset: u64) LoadError![]const u8 {
    const start = std.math.cast(usize, offset) orelse return error.MalformedObject;
    if (start >= table.len) return error.MalformedObject;
    const end = std.mem.indexOfScalarPos(u8, table, start, 0) orelse return error.MalformedObject;
    return table[start..end];
}

/// A C symbol name as the rest of the loader sees it: without the leading
/// underscore Mach-O adds.
fn cName(name: []const u8) []const u8 {
    return if (name.len > 0 and name[0] == '_') name[1..] else name;
}

const RawSection = struct {
    header: loader.Section,
    reloff: u32,
    nreloc: u32,
    /// The index the loader assigned, or null for a section it does not map.
    mapped: ?u32,
};

const RelocInfo = struct {
    address: i32,
    symbolnum: u24,
    pcrel: bool,
    length: u2,
    external: bool,
    kind: u4,

    fn read(raw: []const u8) RelocInfo {
        const word = readU32(raw, 4);
        return .{
            .address = @bitCast(readU32(raw, 0)),
            .symbolnum = @truncate(word),
            .pcrel = (word >> 24) & 1 == 1,
            .length = @truncate(word >> 25),
            .external = (word >> 27) & 1 == 1,
            .kind = @truncate(word >> 28),
        };
    }
};

pub fn parse(arena: Allocator, bytes: []const u8) LoadError!loader.Parsed {
    if (!matches(bytes)) return error.UnsupportedObject;
    const arch: std.Target.Cpu.Arch = switch (readU32(bytes, 4)) {
        CPU_TYPE_X86_64 => .x86_64,
        CPU_TYPE_ARM64 => .aarch64,
        else => return error.UnsupportedObject,
    };
    if (readU32(bytes, 12) != MH_OBJECT) return error.UnsupportedObject;
    const ncmds = readU32(bytes, 16);
    const sizeofcmds = readU32(bytes, 20);
    const commands = try slice(bytes, header_size, sizeofcmds);

    // Sections are numbered from one across all segments, in load command
    // order; relocations and symbols refer to them by that number.
    var raw_sections: std.ArrayList(RawSection) = .empty;
    var symoff: u32 = 0;
    var nsyms: u32 = 0;
    var stroff: u32 = 0;
    var strsize: u32 = 0;
    var offset: usize = 0;
    for (0..ncmds) |_| {
        if (offset + 8 > commands.len) return error.MalformedObject;
        const cmd = readU32(commands, offset);
        const cmdsize = readU32(commands, offset + 4);
        if (cmdsize < 8 or offset + cmdsize > commands.len) return error.MalformedObject;
        const command = commands[offset..][0..cmdsize];
        switch (cmd) {
            LC_SEGMENT_64 => {
                if (cmdsize < segment_command_size) return error.MalformedObject;
                const nsects = readU32(command, 64);
                if (cmdsize < segment_command_size + nsects * section_size) return error.MalformedObject;
                for (0..nsects) |i| {
                    const sect = command[segment_command_size + i * section_size ..][0..section_size];
                    const sectname = fixedName(sect[0..16]);
                    const segname = fixedName(sect[16..32]);
                    const addr = readU64(sect, 32);
                    const size = readU64(sect, 40);
                    const file_offset = readU32(sect, 48);
                    const align_log2 = readU32(sect, 52);
                    const flags = readU32(sect, 64);
                    const section_type = flags & SECTION_TYPE;
                    if (section_type >= S_THREAD_LOCAL_REGULAR and section_type <= S_THREAD_LOCAL_INIT_FUNCTION_POINTERS) {
                        std.debug.print("relocatable loader: object has a thread-local section {s},{s}\n", .{ segname, sectname });
                        return error.UnsupportedObject;
                    }
                    const is_debug = flags & S_ATTR_DEBUG != 0;
                    const is_code = flags & (S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS) != 0 or
                        (std.mem.eql(u8, segname, "__TEXT") and std.mem.eql(u8, sectname, "__text"));
                    const kind: loader.SectionKind = if (is_code)
                        .code
                    else if (std.mem.startsWith(u8, segname, "__DATA"))
                        .writable
                    else
                        .readonly;
                    const section_bytes: []const u8 = if (section_type == S_ZEROFILL) &.{} else try slice(bytes, file_offset, size);
                    try raw_sections.append(arena, .{
                        .header = .{
                            .name = sectname,
                            .kind = kind,
                            .bytes = section_bytes,
                            .size = size,
                            .alignment = @as(u64, 1) << @intCast(@min(align_log2, 31)),
                            .object_addr = addr,
                        },
                        .reloff = readU32(sect, 56),
                        .nreloc = readU32(sect, 60),
                        .mapped = if (is_debug) null else 0,
                    });
                }
            },
            LC_SYMTAB => {
                if (cmdsize < 24) return error.MalformedObject;
                symoff = readU32(command, 8);
                nsyms = readU32(command, 12);
                stroff = readU32(command, 16);
                strsize = readU32(command, 20);
            },
            else => {},
        }
        offset += cmdsize;
    }

    var sections: std.ArrayList(loader.Section) = .empty;
    for (raw_sections.items) |*raw| {
        if (raw.mapped == null) continue;
        raw.mapped = @intCast(sections.items.len);
        try sections.append(arena, raw.header);
    }

    const strtab = try slice(bytes, stroff, strsize);
    const symtab = try slice(bytes, symoff, @as(u64, nsyms) * nlist_size);
    var symbols: std.ArrayList(loader.Symbol) = .empty;
    try symbols.ensureTotalCapacityPrecise(arena, nsyms);
    for (0..nsyms) |i| {
        const entry = symtab[i * nlist_size ..][0..nlist_size];
        const n_strx = readU32(entry, 0);
        const n_type = entry[4];
        const n_sect = entry[5];
        const n_desc = readU16(entry, 6);
        const n_value = readU64(entry, 8);
        var symbol = loader.Symbol{
            .name = cName(try cString(strtab, n_strx)),
            .kind = .unmapped,
            .value = n_value,
            .global = n_type & N_EXT != 0,
            .weak = n_desc & (N_WEAK_REF | N_WEAK_DEF) != 0,
        };
        if (n_type & N_STAB == 0) {
            switch (n_type & N_TYPE) {
                N_UNDF => symbol.kind = .undefined,
                N_ABS => symbol.kind = .absolute,
                N_SECT => {
                    if (n_sect == 0 or n_sect > raw_sections.items.len) return error.MalformedObject;
                    const raw = raw_sections.items[n_sect - 1];
                    if (raw.mapped) |mapped| {
                        if (n_value < raw.header.object_addr) return error.MalformedObject;
                        symbol.kind = .defined;
                        symbol.section = mapped;
                        symbol.value = n_value - raw.header.object_addr;
                    }
                },
                else => {},
            }
        }
        symbols.appendAssumeCapacity(symbol);
    }

    var relocations: std.ArrayList(loader.Relocation) = .empty;
    for (raw_sections.items) |raw| {
        const mapped = raw.mapped orelse continue;
        if (raw.nreloc == 0) continue;
        const entries = try slice(bytes, raw.reloff, @as(u64, raw.nreloc) * reloc_size);
        try relocations.ensureUnusedCapacity(arena, raw.nreloc);
        var i: usize = 0;
        var pending_subtractor: ?loader.Target = null;
        while (i < raw.nreloc) : (i += 1) {
            var rel = RelocInfo.read(entries[i * reloc_size ..][0..reloc_size]);
            if (rel.address < 0) return error.MalformedObject;
            const rel_offset: u64 = @intCast(rel.address);
            if (rel_offset >= raw.header.size) return error.MalformedObject;
            const field = raw.header.bytes[@intCast(rel_offset)..];

            // The addend lives in the field, except for arm64 page
            // relocations, which carry it in a preceding ADDEND entry.
            var addend: i64 = 0;
            var signed_adjust: i64 = 0;
            switch (arch) {
                .x86_64 => {
                    addend = readFieldAddend(field, rel.length) orelse return error.MalformedObject;
                    signed_adjust = switch (@as(X86_64Kind, @enumFromInt(rel.kind))) {
                        .signed_1 => 1,
                        .signed_2 => 2,
                        .signed_4 => 4,
                        else => 0,
                    };
                },
                .aarch64 => switch (@as(Arm64Kind, @enumFromInt(rel.kind))) {
                    .addend => {
                        const raw_addend: u24 = rel.symbolnum;
                        addend = @as(i24, @bitCast(raw_addend));
                        i += 1;
                        if (i >= raw.nreloc) return error.MalformedObject;
                        rel = RelocInfo.read(entries[i * reloc_size ..][0..reloc_size]);
                        switch (@as(Arm64Kind, @enumFromInt(rel.kind))) {
                            .page21, .pageoff12 => {},
                            else => return error.MalformedObject,
                        }
                    },
                    .unsigned => addend = readFieldAddend(field, rel.length) orelse return error.MalformedObject,
                    else => {},
                },
                else => unreachable,
            }

            const kind_is_subtractor = switch (arch) {
                .x86_64 => @as(X86_64Kind, @enumFromInt(rel.kind)) == .subtractor,
                .aarch64 => @as(Arm64Kind, @enumFromInt(rel.kind)) == .subtractor,
                else => unreachable,
            };
            if (kind_is_subtractor) {
                if (!rel.external) return error.MalformedObject;
                if (rel.symbolnum >= symbols.items.len) return error.MalformedObject;
                pending_subtractor = .{ .symbol = rel.symbolnum };
                continue;
            }

            // A section-relative relocation encodes the target's address in
            // the object's own address space; find the section it lands in.
            var target: loader.Target = undefined;
            var final_addend: i64 = undefined;
            if (rel.external) {
                if (rel.symbolnum >= symbols.items.len) return error.MalformedObject;
                target = .{ .symbol = rel.symbolnum };
                final_addend = addend - (if (arch == .x86_64 and rel.pcrel) @as(i64, 4) else 0);
            } else {
                const nsect = rel.symbolnum;
                if (nsect == 0 or nsect > raw_sections.items.len) return error.MalformedObject;
                const target_raw = raw_sections.items[nsect - 1];
                const target_mapped = target_raw.mapped orelse return error.MalformedObject;
                const extra: i64 = if (rel.pcrel) (if (arch == .x86_64) signed_adjust + 4 else 0) else 0;
                const taddr: i64 = if (rel.pcrel)
                    @as(i64, @intCast(raw.header.object_addr)) + rel.address + addend + extra
                else
                    addend;
                if (taddr < @as(i64, @intCast(target_raw.header.object_addr))) return error.MalformedObject;
                target = .{ .section = .{ .index = target_mapped, .offset = @intCast(taddr - @as(i64, @intCast(target_raw.header.object_addr))) } };
                final_addend = if (rel.pcrel) -extra else 0;
            }

            relocations.appendAssumeCapacity(.{
                .section = mapped,
                .offset = rel_offset,
                .target = target,
                .kind = rel.kind,
                .addend = final_addend,
                .subtractor = pending_subtractor,
                .pcrel = rel.pcrel,
                .length = rel.length,
            });
            pending_subtractor = null;
        }
    }

    return .{
        .format = .macho,
        .arch = arch,
        .sections = sections.items,
        .symbols = symbols.items,
        .relocations = relocations.items,
    };
}

fn readFieldAddend(field: []const u8, length: u2) ?i64 {
    return switch (length) {
        0 => if (field.len < 1) null else @as(i8, @bitCast(field[0])),
        1 => if (field.len < 2) null else std.mem.readInt(i16, field[0..2], .little),
        2 => if (field.len < 4) null else std.mem.readInt(i32, field[0..4], .little),
        3 => if (field.len < 8) null else std.mem.readInt(i64, field[0..8], .little),
    };
}

const X86_64Kind = enum(u4) {
    unsigned = 0,
    signed = 1,
    branch = 2,
    got_load = 3,
    got = 4,
    subtractor = 5,
    signed_1 = 6,
    signed_2 = 7,
    signed_4 = 8,
    tlv = 9,
    _,
};

const Arm64Kind = enum(u4) {
    unsigned = 0,
    subtractor = 1,
    branch26 = 2,
    page21 = 3,
    pageoff12 = 4,
    got_load_page21 = 5,
    got_load_pageoff12 = 6,
    pointer_to_got = 7,
    tlvp_load_page21 = 8,
    tlvp_load_pageoff12 = 9,
    addend = 10,
    _,
};

pub fn needsGot(arch: std.Target.Cpu.Arch, kind: u32) bool {
    return switch (arch) {
        .x86_64 => switch (@as(X86_64Kind, @enumFromInt(@as(u4, @truncate(kind))))) {
            .got_load, .got => true,
            else => false,
        },
        .aarch64 => switch (@as(Arm64Kind, @enumFromInt(@as(u4, @truncate(kind))))) {
            .got_load_page21, .got_load_pageoff12, .pointer_to_got => true,
            else => false,
        },
        else => false,
    };
}

pub fn isBranch(arch: std.Target.Cpu.Arch, kind: u32) bool {
    return switch (arch) {
        .x86_64 => @as(X86_64Kind, @enumFromInt(@as(u4, @truncate(kind)))) == .branch,
        .aarch64 => @as(Arm64Kind, @enumFromInt(@as(u4, @truncate(kind)))) == .branch26,
        else => false,
    };
}

pub fn apply(ctx: *loader.Apply, arch: std.Target.Cpu.Arch) LoadError!void {
    return switch (arch) {
        .x86_64 => applyX86_64(ctx),
        .aarch64 => applyArm64(ctx),
        else => error.UnsupportedObject,
    };
}

fn fits(comptime T: type, value: i64) LoadError!T {
    return std.math.cast(T, value) orelse error.RelocationOutOfRange;
}

fn writeUnsigned(ctx: *loader.Apply, value: i64) LoadError!void {
    switch (ctx.reloc.length) {
        3 => ctx.write(u64, @bitCast(value)),
        2 => ctx.write(u32, @bitCast(@as(i32, @truncate(value)))),
        else => return error.MalformedObject,
    }
}

fn applyX86_64(ctx: *loader.Apply) LoadError!void {
    const kind: X86_64Kind = @enumFromInt(@as(u4, @truncate(ctx.reloc.kind)));
    const P = ctx.placeAddress();
    const A = ctx.addendValue();
    const S = ctx.symbolAddress();
    switch (kind) {
        .unsigned => try writeUnsigned(ctx, S + A - try ctx.subtractorAddress()),
        .branch => {
            if (std.math.cast(i32, S + A - P)) |disp| {
                ctx.write(i32, disp);
            } else {
                const stub: i64 = @intCast(try ctx.stub());
                ctx.write(i32, try fits(i32, stub + A - P));
            }
        },
        .signed, .signed_1, .signed_2, .signed_4 => ctx.write(i32, try fits(i32, S + A - P)),
        .got_load, .got => {
            const G: i64 = @intCast(try ctx.gotSlot());
            ctx.write(i32, try fits(i32, G + A - P));
        },
        .subtractor => {},
        .tlv => return ctx.unsupported("X86_64_RELOC_TLV"),
        _ => return ctx.unsupported("X86_64_RELOC (unknown)"),
    }
}

fn applyArm64(ctx: *loader.Apply) LoadError!void {
    const kind: Arm64Kind = @enumFromInt(@as(u4, @truncate(ctx.reloc.kind)));
    const P = ctx.placeAddress();
    const A = ctx.addendValue();
    const S = ctx.symbolAddress();
    switch (kind) {
        .unsigned => try writeUnsigned(ctx, S + A - try ctx.subtractorAddress()),
        .subtractor, .addend => {},
        .branch26 => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const disp: i28 = std.math.cast(i28, S + A - P) orelse blk: {
                const stub: i64 = @intCast(try ctx.stub());
                break :blk try fits(i28, stub + A - P);
            };
            aarch64.writeBranchImm(ctx.code[0..4], disp);
        },
        .page21 => {
            if (ctx.code.len < 4) return error.MalformedObject;
            aarch64.writeAdrImm(ctx.code[0..4], aarch64.calcNumberOfPages(P, S + A) catch return error.RelocationOutOfRange);
        },
        .pageoff12 => {
            if (ctx.code.len < 4) return error.MalformedObject;
            aarch64.writePageOffset(ctx.code[0..4], @bitCast(S + A)) catch return error.MalformedObject;
        },
        .got_load_page21 => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const G: i64 = @intCast(try ctx.gotSlot());
            aarch64.writeAdrImm(ctx.code[0..4], aarch64.calcNumberOfPages(P, G + A) catch return error.RelocationOutOfRange);
        },
        .got_load_pageoff12 => {
            if (ctx.code.len < 4) return error.MalformedObject;
            const G: i64 = @intCast(try ctx.gotSlot());
            const low: u12 = @truncate(@as(u64, @bitCast(G + A)));
            if (low % 8 != 0) return error.MalformedObject;
            aarch64.writeLoadStoreImm(ctx.code[0..4], low / 8);
        },
        .pointer_to_got => {
            const G: i64 = @intCast(try ctx.gotSlot());
            ctx.write(i32, try fits(i32, G + A - P));
        },
        .tlvp_load_page21, .tlvp_load_pageoff12 => return ctx.unsupported("ARM64_RELOC_TLVP_LOAD"),
        _ => return ctx.unsupported("ARM64_RELOC (unknown)"),
    }
}
