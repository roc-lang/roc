//! Mach-O object file writer for the dev backend.
//!
//! This module writes Mach-O object files for macOS and iOS from
//! generated machine code and relocations.
//!
//! Reference: https://github.com/apple-oss-distributions/xnu/blob/main/EXTERNAL_HEADERS/mach-o/loader.h

const std = @import("std");
const Allocator = std.mem.Allocator;
const Relocation = @import("../Relocation.zig").Relocation;

/// Mach-O constants
const MachO = struct {
    // Magic numbers
    const MH_MAGIC_64 = 0xfeedfacf;

    // File types
    const MH_OBJECT = 0x1;

    // CPU types
    const CPU_TYPE_X86_64 = 0x01000007;
    const CPU_TYPE_ARM64 = 0x0100000c;

    // CPU subtypes
    const CPU_SUBTYPE_X86_64_ALL = 0x3;
    const CPU_SUBTYPE_ARM64_ALL = 0x0;

    // File flags
    const MH_SUBSECTIONS_VIA_SYMBOLS = 0x2000;

    // Load command types
    const LC_SEGMENT_64 = 0x19;
    const LC_SYMTAB = 0x2;
    const LC_DYSYMTAB = 0xb;
    const LC_BUILD_VERSION = 0x32;

    // Section types
    const S_REGULAR = 0x0;
    const S_ATTR_PURE_INSTRUCTIONS = 0x80000000;
    const S_ATTR_SOME_INSTRUCTIONS = 0x00000400;

    // Symbol types
    const N_EXT = 0x01;
    const N_UNDF = 0x0;
    const N_SECT = 0xe;

    // Relocation types (x86_64)
    const X86_64_RELOC_BRANCH = 2;
    const X86_64_RELOC_SIGNED = 1;

    // Relocation types (arm64)
    const ARM64_RELOC_BRANCH26 = 2;

    // Platform types
    const PLATFORM_MACOS = 1;
};

/// Mach-O 64-bit header (32 bytes)
const MachHeader64 = extern struct {
    magic: u32,
    cputype: i32,
    cpusubtype: i32,
    filetype: u32,
    ncmds: u32,
    sizeofcmds: u32,
    flags: u32,
    reserved: u32,
};

/// Load command header
const LoadCommand = extern struct {
    cmd: u32,
    cmdsize: u32,
};

/// 64-bit segment command (72 bytes)
const SegmentCommand64 = extern struct {
    cmd: u32,
    cmdsize: u32,
    segname: [16]u8,
    vmaddr: u64,
    vmsize: u64,
    fileoff: u64,
    filesize: u64,
    maxprot: i32,
    initprot: i32,
    nsects: u32,
    flags: u32,
};

/// 64-bit section (80 bytes)
const Section64 = extern struct {
    sectname: [16]u8,
    segname: [16]u8,
    addr: u64,
    size: u64,
    offset: u32,
    @"align": u32,
    reloff: u32,
    nreloc: u32,
    flags: u32,
    reserved1: u32,
    reserved2: u32,
    reserved3: u32,
};

/// Symbol table command
const SymtabCommand = extern struct {
    cmd: u32,
    cmdsize: u32,
    symoff: u32,
    nsyms: u32,
    stroff: u32,
    strsize: u32,
};

/// Dynamic symbol table command
const DysymtabCommand = extern struct {
    cmd: u32,
    cmdsize: u32,
    ilocalsym: u32,
    nlocalsym: u32,
    iextdefsym: u32,
    nextdefsym: u32,
    iundefsym: u32,
    nundefsym: u32,
    tocoff: u32,
    ntoc: u32,
    modtaboff: u32,
    nmodtab: u32,
    extrefsymoff: u32,
    nextrefsyms: u32,
    indirectsymoff: u32,
    nindirectsyms: u32,
    extreloff: u32,
    nextrel: u32,
    locreloff: u32,
    nlocrel: u32,
};

/// 64-bit nlist (symbol table entry)
const Nlist64 = extern struct {
    n_strx: u32,
    n_type: u8,
    n_sect: u8,
    n_desc: i16,
    n_value: u64,
};

/// Relocation entry (8 bytes)
const RelocationInfo = extern struct {
    r_address: i32,
    r_info: u32, // r_symbolnum:24, r_pcrel:1, r_length:2, r_extern:1, r_type:4

    fn init(address: u32, symbolnum: u24, pcrel: bool, length: u2, is_extern: bool, reloc_type: u4) RelocationInfo {
        const info: u32 = @as(u32, symbolnum) |
            (@as(u32, @intFromBool(pcrel)) << 24) |
            (@as(u32, length) << 25) |
            (@as(u32, @intFromBool(is_extern)) << 27) |
            (@as(u32, reloc_type) << 28);
        return .{
            .r_address = @bitCast(address),
            .r_info = info,
        };
    }
};

/// Target architecture
pub const Architecture = enum {
    x86_64,
    aarch64,

    fn cpuType(self: Architecture) i32 {
        return switch (self) {
            .x86_64 => MachO.CPU_TYPE_X86_64,
            .aarch64 => MachO.CPU_TYPE_ARM64,
        };
    }

    fn cpuSubtype(self: Architecture) i32 {
        return switch (self) {
            .x86_64 => MachO.CPU_SUBTYPE_X86_64_ALL,
            .aarch64 => MachO.CPU_SUBTYPE_ARM64_ALL,
        };
    }

    fn branchRelocType(self: Architecture) u4 {
        return switch (self) {
            .x86_64 => MachO.X86_64_RELOC_BRANCH,
            .aarch64 => MachO.ARM64_RELOC_BRANCH26,
        };
    }
};

/// Symbol definition
pub const Symbol = struct {
    name: []const u8,
    section: u8, // 0 = undefined, 1 = __text, etc.
    offset: u64,
    is_external: bool,
};

/// Mach-O object file writer
pub const MachOWriter = struct {
    const Self = @This();

    allocator: Allocator,
    arch: Architecture,

    // Code section
    text: std.ArrayList(u8),

    // Symbols and relocations
    symbols: std.ArrayList(Symbol),
    text_relocs: std.ArrayList(TextReloc),

    // String table
    strtab: std.ArrayList(u8),

    const TextReloc = struct {
        offset: u32,
        symbol_idx: u32,
        is_extern: bool,
    };

    pub fn init(allocator: Allocator, arch: Architecture) !Self {
        var self = Self{
            .allocator = allocator,
            .arch = arch,
            .text = .{},
            .symbols = .{},
            .text_relocs = .{},
            .strtab = .{},
        };

        // String table starts with space + null (Mach-O convention)
        try self.strtab.append(allocator, ' ');
        try self.strtab.append(allocator, 0);

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.text.deinit(self.allocator);
        self.symbols.deinit(self.allocator);
        self.text_relocs.deinit(self.allocator);
        self.strtab.deinit(self.allocator);
    }

    /// Set code section contents
    pub fn setCode(self: *Self, code: []const u8) !void {
        self.text.clearRetainingCapacity();
        try self.text.appendSlice(self.allocator, code);
    }

    /// Add a symbol
    pub fn addSymbol(self: *Self, symbol: Symbol) !u32 {
        const idx: u32 = @intCast(self.symbols.items.len);
        try self.symbols.append(self.allocator, symbol);
        return idx;
    }

    /// Add an external (undefined) symbol
    pub fn addExternalSymbol(self: *Self, name: []const u8) !u32 {
        return self.addSymbol(.{
            .name = name,
            .section = 0, // NO_SECT
            .offset = 0,
            .is_external = true,
        });
    }

    /// Add a text relocation
    pub fn addTextRelocation(self: *Self, offset: u32, symbol_idx: u32, is_extern: bool) !void {
        try self.text_relocs.append(self.allocator, .{
            .offset = offset,
            .symbol_idx = symbol_idx,
            .is_extern = is_extern,
        });
    }

    /// Add string to string table
    fn addString(self: *Self, str: []const u8) !u32 {
        const offset: u32 = @intCast(self.strtab.items.len);
        try self.strtab.appendSlice(self.allocator, str);
        try self.strtab.append(self.allocator, 0);
        return offset;
    }

    /// Write the Mach-O object file
    pub fn write(self: *Self, output: *std.ArrayList(u8)) !void {
        // Calculate sizes
        const header_size: u32 = @sizeOf(MachHeader64);
        const segment_cmd_size: u32 = @sizeOf(SegmentCommand64) + @sizeOf(Section64);
        const symtab_cmd_size: u32 = @sizeOf(SymtabCommand);
        const dysymtab_cmd_size: u32 = @sizeOf(DysymtabCommand);
        const total_cmd_size: u32 = segment_cmd_size + symtab_cmd_size + dysymtab_cmd_size;

        // Calculate offsets
        const text_offset: u32 = header_size + total_cmd_size;
        const text_size: u32 = @intCast(self.text.items.len);

        const reloc_offset: u32 = text_offset + text_size;
        const reloc_size: u32 = @intCast(self.text_relocs.items.len * @sizeOf(RelocationInfo));

        const symtab_offset: u32 = reloc_offset + reloc_size;

        // Count symbol types for dysymtab
        var num_local: u32 = 0;
        var num_extdef: u32 = 0;
        var num_undef: u32 = 0;

        for (self.symbols.items) |sym| {
            if (sym.section == 0) {
                num_undef += 1;
            } else if (sym.is_external) {
                num_extdef += 1;
            } else {
                num_local += 1;
            }
        }

        const num_syms: u32 = @intCast(self.symbols.items.len);
        const symtab_size: u32 = num_syms * @sizeOf(Nlist64);

        const strtab_offset: u32 = symtab_offset + symtab_size;

        // Build string table with symbol names
        for (self.symbols.items) |*sym| {
            _ = try self.addString(sym.name);
        }
        const strtab_size: u32 = @intCast(self.strtab.items.len);

        // Write Mach-O header
        const header = MachHeader64{
            .magic = MachO.MH_MAGIC_64,
            .cputype = self.arch.cpuType(),
            .cpusubtype = self.arch.cpuSubtype(),
            .filetype = MachO.MH_OBJECT,
            .ncmds = 3, // segment, symtab, dysymtab
            .sizeofcmds = total_cmd_size,
            .flags = MachO.MH_SUBSECTIONS_VIA_SYMBOLS,
            .reserved = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&header));

        // Write segment command
        const segname: [16]u8 = std.mem.zeroes([16]u8);
        // Empty segment name is typical for object files

        const segment_cmd = SegmentCommand64{
            .cmd = MachO.LC_SEGMENT_64,
            .cmdsize = segment_cmd_size,
            .segname = segname,
            .vmaddr = 0,
            .vmsize = text_size,
            .fileoff = text_offset,
            .filesize = text_size,
            .maxprot = 7, // rwx
            .initprot = 7,
            .nsects = 1,
            .flags = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&segment_cmd));

        // Write __text section
        var sectname: [16]u8 = std.mem.zeroes([16]u8);
        @memcpy(sectname[0..6], "__text");
        var sectsegname: [16]u8 = std.mem.zeroes([16]u8);
        @memcpy(sectsegname[0..6], "__TEXT");

        const text_section = Section64{
            .sectname = sectname,
            .segname = sectsegname,
            .addr = 0,
            .size = text_size,
            .offset = text_offset,
            .@"align" = 4, // 2^4 = 16 byte alignment
            .reloff = if (self.text_relocs.items.len > 0) reloc_offset else 0,
            .nreloc = @intCast(self.text_relocs.items.len),
            .flags = MachO.S_ATTR_PURE_INSTRUCTIONS | MachO.S_ATTR_SOME_INSTRUCTIONS,
            .reserved1 = 0,
            .reserved2 = 0,
            .reserved3 = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&text_section));

        // Write symtab command
        const symtab_cmd = SymtabCommand{
            .cmd = MachO.LC_SYMTAB,
            .cmdsize = symtab_cmd_size,
            .symoff = symtab_offset,
            .nsyms = num_syms,
            .stroff = strtab_offset,
            .strsize = strtab_size,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&symtab_cmd));

        // Write dysymtab command
        const dysymtab_cmd = DysymtabCommand{
            .cmd = MachO.LC_DYSYMTAB,
            .cmdsize = dysymtab_cmd_size,
            .ilocalsym = 0,
            .nlocalsym = num_local,
            .iextdefsym = num_local,
            .nextdefsym = num_extdef,
            .iundefsym = num_local + num_extdef,
            .nundefsym = num_undef,
            .tocoff = 0,
            .ntoc = 0,
            .modtaboff = 0,
            .nmodtab = 0,
            .extrefsymoff = 0,
            .nextrefsyms = 0,
            .indirectsymoff = 0,
            .nindirectsyms = 0,
            .extreloff = 0,
            .nextrel = 0,
            .locreloff = 0,
            .nlocrel = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&dysymtab_cmd));

        // Write text section content
        try output.appendSlice(self.allocator, self.text.items);

        // Write relocations
        for (self.text_relocs.items) |rel| {
            const reloc = RelocationInfo.init(
                rel.offset,
                @intCast(rel.symbol_idx),
                true, // PC-relative
                2, // 32-bit (2^2 = 4 bytes)
                rel.is_extern,
                self.arch.branchRelocType(),
            );
            try output.appendSlice(self.allocator, std.mem.asBytes(&reloc));
        }

        // Write symbol table
        var str_offset: u32 = 2; // Skip initial " \0"
        for (self.symbols.items) |sym| {
            const n_type: u8 = if (sym.section == 0)
                MachO.N_UNDF | MachO.N_EXT
            else if (sym.is_external)
                MachO.N_SECT | MachO.N_EXT
            else
                MachO.N_SECT;

            const nlist = Nlist64{
                .n_strx = str_offset,
                .n_type = n_type,
                .n_sect = sym.section,
                .n_desc = 0,
                .n_value = sym.offset,
            };
            try output.appendSlice(self.allocator, std.mem.asBytes(&nlist));

            str_offset += @intCast(sym.name.len + 1);
        }

        // Write string table
        try output.appendSlice(self.allocator, self.strtab.items);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "create minimal macho object" {
    var writer = try MachOWriter.init(std.testing.allocator, .x86_64);
    defer writer.deinit();

    // Add test code (ret instruction)
    try writer.setCode(&[_]u8{0xC3});

    // Add a symbol
    _ = try writer.addSymbol(.{
        .name = "_test_func",
        .section = 1, // __text
        .offset = 0,
        .is_external = true,
    });

    var output: std.ArrayList(u8) = .{};
    defer output.deinit(std.testing.allocator);

    try writer.write(&output);

    // Check Mach-O magic (little endian)
    const magic = std.mem.readInt(u32, output.items[0..4], .little);
    try std.testing.expectEqual(MachO.MH_MAGIC_64, magic);
}

test "macho with external call" {
    var writer = try MachOWriter.init(std.testing.allocator, .x86_64);
    defer writer.deinit();

    // call external; ret
    try writer.setCode(&[_]u8{ 0xE8, 0x00, 0x00, 0x00, 0x00, 0xC3 });

    const ext_idx = try writer.addExternalSymbol("_external_func");
    try writer.addTextRelocation(1, ext_idx, true);

    var output: std.ArrayList(u8) = .{};
    defer output.deinit(std.testing.allocator);

    try writer.write(&output);

    const magic = std.mem.readInt(u32, output.items[0..4], .little);
    try std.testing.expectEqual(MachO.MH_MAGIC_64, magic);
}
