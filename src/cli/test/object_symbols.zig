//! Reads the symbols and code relocations of a 64-bit little-endian
//! relocatable object (ELF, Mach-O or COFF), so a test can check what a
//! compiled object leaves for its linker to resolve and which functions its
//! code calls.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// A failure to read an object.
pub const Error = Allocator.Error || error{ MalformedObject, UnsupportedObject };

/// One relocation in a code section: the function the relocated code lies in,
/// and the symbol it refers to. A relocation that refers to a section rather
/// than a symbol names the function at the offset it refers to.
pub const Reference = struct {
    source: []const u8,
    target: []const u8,
};

/// What an object leaves for its linker and what its code refers to. Mach-O
/// names are given without their leading underscore.
pub const Object = struct {
    /// Global symbols the object refers to but does not define.
    undefined: []const []const u8,
    /// Symbols the object defines, with whether each is local to the object.
    definitions: []const Definition,
    /// Every relocation in a code section.
    references: []const Reference,

    /// Whether the object defines `name` as a local symbol.
    pub fn definesLocally(self: Object, name: []const u8) bool {
        for (self.definitions) |definition| {
            if (definition.local and std.mem.eql(u8, definition.name, name)) return true;
        }
        return false;
    }

    /// Whether the object defines `name` at all.
    pub fn defines(self: Object, name: []const u8) bool {
        for (self.definitions) |definition| {
            if (std.mem.eql(u8, definition.name, name)) return true;
        }
        return false;
    }
};

/// A symbol an object defines.
pub const Definition = struct {
    name: []const u8,
    local: bool,
};

/// A defined symbol's position, for finding the function a relocation lies in.
const Placed = struct {
    section: u32,
    offset: u64,
    name: []const u8,

    fn lessThan(_: void, a: Placed, b: Placed) bool {
        if (a.section != b.section) return a.section < b.section;
        return a.offset < b.offset;
    }
};

/// The last symbol at or before `offset` in `section`.
fn symbolAt(placed: []const Placed, section: u32, offset: u64) ?[]const u8 {
    var found: ?[]const u8 = null;
    for (placed) |entry| {
        if (entry.section != section) continue;
        if (entry.offset > offset) break;
        found = entry.name;
    }
    return found;
}

/// Read `bytes` as an object. Every returned name is allocated in `arena`.
pub fn read(arena: Allocator, bytes: []const u8) Error!Object {
    if (bytes.len >= 4 and std.mem.eql(u8, bytes[0..4], "\x7fELF")) return readElf(arena, bytes);
    if (bytes.len >= 4 and std.mem.readInt(u32, bytes[0..4], .little) == std.macho.MH_MAGIC_64) return readMachO(arena, bytes);
    if (bytes.len >= 2) {
        const machine = std.mem.readInt(u16, bytes[0..2], .little);
        if (machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.AMD64) or machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.ARM64)) return readCoff(arena, bytes);
    }
    return error.UnsupportedObject;
}

fn slice(bytes: []const u8, offset: u64, len: u64) Error![]const u8 {
    if (offset > bytes.len or len > bytes.len - offset) return error.MalformedObject;
    return bytes[@intCast(offset)..][0..@intCast(len)];
}

fn value(comptime T: type, bytes: []const u8, offset: u64) Error!T {
    return std.mem.bytesToValue(T, (try slice(bytes, offset, @sizeOf(T)))[0..@sizeOf(T)]);
}

fn cString(bytes: []const u8, offset: u64) Error![]const u8 {
    const rest = try slice(bytes, offset, bytes.len - @min(offset, bytes.len));
    const end = std.mem.findScalar(u8, rest, 0) orelse return error.MalformedObject;
    return rest[0..end];
}

fn readElf(arena: Allocator, bytes: []const u8) Error!Object {
    const elf = std.elf;
    const header = try value(elf.Elf64_Ehdr, bytes, 0);
    if (header.e_ident[elf.EI_CLASS] != elf.ELFCLASS64 or header.e_ident[elf.EI_DATA] != elf.ELFDATA2LSB) return error.UnsupportedObject;
    if (header.e_shentsize != @sizeOf(elf.Elf64_Shdr)) return error.MalformedObject;
    const sections = try arena.alloc(elf.Elf64_Shdr, header.e_shnum);
    for (sections, 0..) |*section, index| {
        section.* = try value(elf.Elf64_Shdr, bytes, header.e_shoff + index * @sizeOf(elf.Elf64_Shdr));
    }

    var undefined_names: std.ArrayList([]const u8) = .empty;
    var definitions: std.ArrayList(Definition) = .empty;
    var placed: std.ArrayList(Placed) = .empty;
    var symbols: []const elf.Elf64_Sym = &.{};
    var symbol_names: []const []const u8 = &.{};
    for (sections) |section| {
        if (section.sh_type != elf.SHT_SYMTAB) continue;
        if (section.sh_link >= sections.len) return error.MalformedObject;
        const strings = sections[section.sh_link];
        const count = section.sh_size / @sizeOf(elf.Elf64_Sym);
        const table = try arena.alloc(elf.Elf64_Sym, @intCast(count));
        const names = try arena.alloc([]const u8, @intCast(count));
        for (table, names, 0..) |*symbol, *name, index| {
            symbol.* = try value(elf.Elf64_Sym, bytes, section.sh_offset + index * @sizeOf(elf.Elf64_Sym));
            name.* = try cString(bytes, strings.sh_offset + symbol.st_name);
            const binding = symbol.st_info >> 4;
            const kind = symbol.st_info & 0xf;
            if (name.len == 0) continue;
            if (symbol.st_shndx == elf.SHN_UNDEF) {
                if (binding != elf.STB_LOCAL) try undefined_names.append(arena, name.*);
                continue;
            }
            if (symbol.st_shndx >= elf.SHN_LORESERVE) continue;
            if (kind != elf.STT_FUNC and kind != elf.STT_OBJECT) continue;
            try definitions.append(arena, .{ .name = name.*, .local = binding == elf.STB_LOCAL });
            try placed.append(arena, .{ .section = symbol.st_shndx, .offset = symbol.st_value, .name = name.* });
        }
        symbols = table;
        symbol_names = names;
    }
    std.mem.sort(Placed, placed.items, {}, Placed.lessThan);

    var references: std.ArrayList(Reference) = .empty;
    for (sections) |section| {
        if (section.sh_type != elf.SHT_RELA) continue;
        if (section.sh_info >= sections.len) return error.MalformedObject;
        if (sections[section.sh_info].sh_flags & elf.SHF_EXECINSTR == 0) continue;
        const count = section.sh_size / @sizeOf(elf.Elf64_Rela);
        for (0..@intCast(count)) |index| {
            const rela = try value(elf.Elf64_Rela, bytes, section.sh_offset + index * @sizeOf(elf.Elf64_Rela));
            const symbol_index: usize = @intCast(rela.r_info >> 32);
            if (symbol_index >= symbols.len) return error.MalformedObject;
            const source = symbolAt(placed.items, section.sh_info, rela.r_offset) orelse continue;
            const symbol = symbols[symbol_index];
            const target = if (symbol.st_info & 0xf == elf.STT_SECTION)
                symbolAt(placed.items, symbol.st_shndx, @intCast(@max(rela.r_addend, 0))) orelse continue
            else
                symbol_names[symbol_index];
            try references.append(arena, .{ .source = source, .target = target });
        }
    }

    return .{ .undefined = undefined_names.items, .definitions = definitions.items, .references = references.items };
}

fn readMachO(arena: Allocator, bytes: []const u8) Error!Object {
    const macho = std.macho;
    const command_count = try value(u32, bytes, 16);

    const CodeSection = struct { address: u64, relocations_offset: u32, relocations: u32 };
    var code_sections: std.ArrayList(CodeSection) = .empty;
    var symbol_names: []const []const u8 = &.{};
    var undefined_names: std.ArrayList([]const u8) = .empty;
    var definitions: std.ArrayList(Definition) = .empty;
    var placed: std.ArrayList(Placed) = .empty;

    var command_offset: u64 = @sizeOf(macho.mach_header_64);
    for (0..command_count) |_| {
        const command = try value(macho.load_command, bytes, command_offset);
        if (command.cmd == .SEGMENT_64) {
            const segment = try value(macho.segment_command_64, bytes, command_offset);
            for (0..segment.nsects) |index| {
                const section = try value(macho.section_64, bytes, command_offset + @sizeOf(macho.segment_command_64) + index * @sizeOf(macho.section_64));
                const instructions = macho.S_ATTR_PURE_INSTRUCTIONS | macho.S_ATTR_SOME_INSTRUCTIONS;
                if (section.flags & instructions == 0) continue;
                try code_sections.append(arena, .{ .address = section.addr, .relocations_offset = section.reloff, .relocations = section.nreloc });
            }
        } else if (command.cmd == .SYMTAB) {
            const symtab = try value(macho.symtab_command, bytes, command_offset);
            const names = try arena.alloc([]const u8, symtab.nsyms);
            for (names, 0..) |*name, index| {
                // nlist_64: n_strx u32, n_type u8, n_sect u8, n_desc u16, n_value u64.
                const entry = try slice(bytes, @as(u64, symtab.symoff) + index * 16, 16);
                const raw = try cString(bytes, @as(u64, symtab.stroff) + std.mem.readInt(u32, entry[0..4], .little));
                const symbol_type = entry[4];
                const symbol_value = std.mem.readInt(u64, entry[8..16], .little);
                name.* = if (raw.len != 0 and raw[0] == '_') raw[1..] else raw;
                if (symbol_type & macho.N_STAB != 0) continue;
                const external = symbol_type & macho.N_EXT != 0;
                const kind = symbol_type & macho.N_TYPE;
                if (kind == macho.N_UNDF) {
                    if (external) try undefined_names.append(arena, name.*);
                } else if (kind == macho.N_SECT) {
                    // Assembler-local labels mark section starts, not functions.
                    if (raw.len == 0 or raw[0] == 'l' or raw[0] == 'L') continue;
                    try definitions.append(arena, .{ .name = name.*, .local = !external });
                    try placed.append(arena, .{ .section = 0, .offset = symbol_value, .name = name.* });
                }
            }
            symbol_names = names;
        }
        command_offset += command.cmdsize;
    }
    std.mem.sort(Placed, placed.items, {}, Placed.lessThan);

    var references: std.ArrayList(Reference) = .empty;
    for (code_sections.items) |section| {
        for (0..section.relocations) |index| {
            // relocation_info: r_address i32, then r_symbolnum:24 r_pcrel:1 r_length:2 r_extern:1 r_type:4.
            const entry = try slice(bytes, @as(u64, section.relocations_offset) + index * 8, 8);
            const address = std.mem.readInt(i32, entry[0..4], .little);
            const info = std.mem.readInt(u32, entry[4..8], .little);
            const is_extern = (info >> 27) & 1 != 0;
            const symbol_index = info & 0xff_ffff;
            if (!is_extern or address < 0) continue;
            if (symbol_index >= symbol_names.len) return error.MalformedObject;
            const source = symbolAt(placed.items, 0, section.address + @as(u64, @intCast(address))) orelse continue;
            try references.append(arena, .{ .source = source, .target = symbol_names[symbol_index] });
        }
    }

    return .{ .undefined = undefined_names.items, .definitions = definitions.items, .references = references.items };
}

fn readCoff(arena: Allocator, bytes: []const u8) Error!Object {
    const section_count = try value(u16, bytes, 2);
    const symbol_table_offset = try value(u32, bytes, 8);
    const symbol_count = try value(u32, bytes, 12);
    const optional_header_size = try value(u16, bytes, 16);
    const string_table_offset = @as(u64, symbol_table_offset) + @as(u64, symbol_count) * 18;

    const external_class: u8 = 2;
    const static_class: u8 = 3;
    const function_type: u16 = 0x20;

    var undefined_names: std.ArrayList([]const u8) = .empty;
    var definitions: std.ArrayList(Definition) = .empty;
    var placed: std.ArrayList(Placed) = .empty;
    const names = try arena.alloc([]const u8, symbol_count);
    @memset(names, "");
    // A section symbol stands for the start of its section.
    const section_of_symbol = try arena.alloc(?u32, symbol_count);
    @memset(section_of_symbol, null);

    var index: u32 = 0;
    while (index < symbol_count) {
        const entry = try slice(bytes, @as(u64, symbol_table_offset) + @as(u64, index) * 18, 18);
        const name = if (std.mem.readInt(u32, entry[0..4], .little) == 0)
            try cString(bytes, string_table_offset + std.mem.readInt(u32, entry[4..8], .little))
        else
            entry[0 .. std.mem.findScalar(u8, entry[0..8], 0) orelse 8];
        const symbol_value = std.mem.readInt(u32, entry[8..12], .little);
        const section_number = std.mem.readInt(i16, entry[12..14], .little);
        const symbol_type = std.mem.readInt(u16, entry[14..16], .little);
        const storage_class = entry[16];
        const aux_count = entry[17];
        names[index] = name;
        if (section_number == 0 and storage_class == external_class and symbol_value == 0) {
            try undefined_names.append(arena, name);
        } else if (section_number > 0 and (storage_class == external_class or storage_class == static_class)) {
            const section: u32 = @intCast(section_number);
            if (storage_class == static_class and aux_count != 0 and symbol_type != function_type) {
                section_of_symbol[index] = section;
            } else {
                try definitions.append(arena, .{ .name = name, .local = storage_class == static_class });
                try placed.append(arena, .{ .section = section, .offset = symbol_value, .name = name });
            }
        }
        index += 1 + aux_count;
    }
    std.mem.sort(Placed, placed.items, {}, Placed.lessThan);

    const code_characteristic: u32 = 0x20;
    var references: std.ArrayList(Reference) = .empty;
    for (0..section_count) |section_index| {
        const header_offset = 20 + @as(u64, optional_header_size) + section_index * 40;
        const relocations_offset = try value(u32, bytes, header_offset + 24);
        const relocation_count = try value(u16, bytes, header_offset + 32);
        const characteristics = try value(u32, bytes, header_offset + 36);
        if (characteristics & code_characteristic == 0) continue;
        const section: u32 = @intCast(section_index + 1);
        for (0..relocation_count) |relocation_index| {
            const relocation = try slice(bytes, @as(u64, relocations_offset) + relocation_index * 10, 10);
            const address = std.mem.readInt(u32, relocation[0..4], .little);
            const symbol_index = std.mem.readInt(u32, relocation[4..8], .little);
            if (symbol_index >= symbol_count) return error.MalformedObject;
            const source = symbolAt(placed.items, section, address) orelse continue;
            const target = if (section_of_symbol[symbol_index]) |target_section|
                symbolAt(placed.items, target_section, 0) orelse continue
            else
                names[symbol_index];
            try references.append(arena, .{ .source = source, .target = target });
        }
    }

    return .{ .undefined = undefined_names.items, .definitions = definitions.items, .references = references.items };
}
