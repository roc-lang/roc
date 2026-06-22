//! Mach-O object file writer for the dev backend.
//!
//! This module writes Mach-O object files for macOS and iOS from
//! generated machine code and relocations.
//!
//! Reference: https://github.com/apple-oss-distributions/xnu/blob/main/EXTERNAL_HEADERS/mach-o/loader.h

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;
const DataRelocationKind = @import("../Relocation.zig").DataRelocationKind;
const roc_target = @import("roc_target");

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

    // Load command types
    const LC_SEGMENT_64 = 0x19;
    const LC_SYMTAB = 0x2;
    const LC_DYSYMTAB = 0xb;
    const LC_BUILD_VERSION = 0x32;

    // Platform constants for LC_BUILD_VERSION
    const PLATFORM_MACOS = 1;

    // Section types
    const S_ATTR_PURE_INSTRUCTIONS = 0x80000000;
    const S_ATTR_DEBUG = 0x02000000;
    const S_ATTR_SOME_INSTRUCTIONS = 0x00000400;

    // Symbol types
    const N_EXT = 0x01;
    const N_UNDF = 0x0;
    const N_SECT = 0xe;

    // Relocation types (x86_64)
    const X86_64_RELOC_UNSIGNED = 0;
    const X86_64_RELOC_SIGNED = 1;
    const X86_64_RELOC_BRANCH = 2;

    // Relocation types (arm64)
    const ARM64_RELOC_UNSIGNED = 0;
    const ARM64_RELOC_BRANCH26 = 2;
    const ARM64_RELOC_PAGE21 = 3;
    const ARM64_RELOC_PAGEOFF12 = 4;
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

/// Build version command (24 bytes)
const BuildVersionCommand = extern struct {
    cmd: u32,
    cmdsize: u32,
    platform: u32,
    minos: u32, // X.Y.Z encoded as xxxx.yy.zz
    sdk: u32,
    ntools: u32,
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

    // Read-only data section
    rodata: std.ArrayList(u8),

    // Symbols and relocations
    symbols: std.ArrayList(Symbol),
    text_relocs: std.ArrayList(TextReloc),
    rodata_relocs: std.ArrayList(DataReloc),

    // DWARF debug sections; address fields are stored as object-space
    // addresses with section-based UNSIGNED relocations against __text.
    debug_line: std.ArrayList(u8),
    debug_abbrev: std.ArrayList(u8),
    debug_info: std.ArrayList(u8),
    debug_line_relocs: std.ArrayList(u32),
    debug_info_relocs: std.ArrayList(u32),

    // String table
    strtab: std.ArrayList(u8),

    const TextReloc = struct {
        offset: u32,
        symbol_idx: u32,
        is_extern: bool,
        pcrel: bool,
        length: u2,
        reloc_type: u4,
    };

    const TextDataReloc = struct {
        pcrel: bool,
        length: u2,
        reloc_type: u4,
    };

    const DataReloc = struct {
        offset: u32,
        symbol_idx: u32,
        is_extern: bool,
    };

    pub fn init(allocator: Allocator, arch: Architecture) Allocator.Error!Self {
        var self = Self{
            .allocator = allocator,
            .arch = arch,
            .text = .empty,
            .rodata = .empty,
            .symbols = .empty,
            .text_relocs = .empty,
            .rodata_relocs = .empty,
            .debug_line = .empty,
            .debug_abbrev = .empty,
            .debug_info = .empty,
            .debug_line_relocs = .empty,
            .debug_info_relocs = .empty,
            .strtab = .empty,
        };

        // String table starts with space + null (Mach-O convention)
        try self.strtab.append(allocator, ' ');
        try self.strtab.append(allocator, 0);

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.text.deinit(self.allocator);
        self.rodata.deinit(self.allocator);
        self.symbols.deinit(self.allocator);
        self.text_relocs.deinit(self.allocator);
        self.rodata_relocs.deinit(self.allocator);
        self.debug_line.deinit(self.allocator);
        self.debug_abbrev.deinit(self.allocator);
        self.debug_info.deinit(self.allocator);
        self.debug_line_relocs.deinit(self.allocator);
        self.debug_info_relocs.deinit(self.allocator);
        self.strtab.deinit(self.allocator);
    }

    /// Set the DWARF debug section contents. Each relocation is an offset of
    /// an 8-byte address field whose stored value is the object-space text
    /// address (the addend), fixed up via a section-based UNSIGNED
    /// relocation against __text.
    pub fn setDebugSections(
        self: *Self,
        debug_line: []const u8,
        debug_abbrev: []const u8,
        debug_info: []const u8,
        line_relocs: []const @import("mod.zig").DebugReloc,
        info_relocs: []const @import("mod.zig").DebugReloc,
    ) Allocator.Error!void {
        try self.debug_line.appendSlice(self.allocator, debug_line);
        try self.debug_abbrev.appendSlice(self.allocator, debug_abbrev);
        try self.debug_info.appendSlice(self.allocator, debug_info);
        // __text has address zero in the object, so the stored object-space
        // address equals the text offset itself.
        for (line_relocs) |rel| {
            std.mem.writeInt(u64, self.debug_line.items[rel.section_offset..][0..8], rel.addend, .little);
            try self.debug_line_relocs.append(self.allocator, rel.section_offset);
        }
        for (info_relocs) |rel| {
            std.mem.writeInt(u64, self.debug_info.items[rel.section_offset..][0..8], rel.addend, .little);
            try self.debug_info_relocs.append(self.allocator, rel.section_offset);
        }
    }

    /// Set code section contents
    pub fn setCode(self: *Self, code: []const u8) Allocator.Error!void {
        self.text.clearRetainingCapacity();
        try self.text.appendSlice(self.allocator, code);
    }

    /// Set read-only data section contents.
    pub fn setRodata(self: *Self, rodata: []const u8) Allocator.Error!void {
        self.rodata.clearRetainingCapacity();
        try self.rodata.appendSlice(self.allocator, rodata);
    }

    /// Allocate space in the rodata section for a constant value.
    /// Returns the offset within rodata and a pointer to write the value.
    pub fn allocateRodata(self: *Self, size: usize, alignment: usize) Allocator.Error!struct { offset: usize, ptr: [*]u8 } {
        // Align current position
        const current_len = self.rodata.items.len;
        const aligned_offset = std.mem.alignForward(usize, current_len, alignment);
        const padding = aligned_offset - current_len;

        // Add padding and space for the value
        try self.rodata.appendNTimes(self.allocator, 0, padding + size);

        return .{
            .offset = aligned_offset,
            .ptr = self.rodata.items.ptr + aligned_offset,
        };
    }

    /// Add a symbol
    pub fn addSymbol(self: *Self, symbol: Symbol) Allocator.Error!u32 {
        const idx: u32 = @intCast(self.symbols.items.len);
        try self.symbols.append(self.allocator, symbol);
        return idx;
    }

    /// Add an external (undefined) symbol
    pub fn addExternalSymbol(self: *Self, name: []const u8) Allocator.Error!u32 {
        return self.addSymbol(.{
            .name = name,
            .section = 0, // NO_SECT
            .offset = 0,
            .is_external = true,
        });
    }

    /// Add a text relocation
    pub fn addTextRelocation(self: *Self, offset: u32, symbol_idx: u32, is_extern: bool) Allocator.Error!void {
        try self.text_relocs.append(self.allocator, .{
            .offset = offset,
            .symbol_idx = symbol_idx,
            .is_extern = is_extern,
            .pcrel = true,
            .length = 2,
            .reloc_type = self.arch.branchRelocType(),
        });
    }

    /// Add a data-symbol relocation in the text section.
    pub fn addTextDataRelocation(self: *Self, offset: u32, symbol_idx: u32, is_extern: bool, kind: DataRelocationKind) Allocator.Error!void {
        const reloc: TextDataReloc = switch (kind) {
            .abs64 => .{
                .pcrel = false,
                .length = @as(u2, 3),
                .reloc_type = switch (self.arch) {
                    .x86_64 => MachO.X86_64_RELOC_UNSIGNED,
                    .aarch64 => MachO.ARM64_RELOC_UNSIGNED,
                },
            },
            .rel32 => .{
                .pcrel = true,
                .length = @as(u2, 2),
                .reloc_type = switch (self.arch) {
                    .x86_64 => MachO.X86_64_RELOC_SIGNED,
                    .aarch64 => unreachable,
                },
            },
            .page21 => .{
                .pcrel = true,
                .length = @as(u2, 2),
                .reloc_type = switch (self.arch) {
                    .x86_64 => unreachable,
                    .aarch64 => MachO.ARM64_RELOC_PAGE21,
                },
            },
            .pageoff12 => .{
                .pcrel = false,
                .length = @as(u2, 2),
                .reloc_type = switch (self.arch) {
                    .x86_64 => unreachable,
                    .aarch64 => MachO.ARM64_RELOC_PAGEOFF12,
                },
            },
        };
        try self.text_relocs.append(self.allocator, .{
            .offset = offset,
            .symbol_idx = symbol_idx,
            .is_extern = is_extern,
            .pcrel = reloc.pcrel,
            .length = reloc.length,
            .reloc_type = reloc.reloc_type,
        });
    }

    /// Add an absolute pointer relocation in the read-only data section.
    pub fn addRodataRelocation(self: *Self, offset: u32, symbol_idx: u32, is_extern: bool, addend: i64) Allocator.Error!void {
        if (offset + 8 > self.rodata.items.len) unreachable;
        const relocated_value = if (is_extern) addend else self.localRelocationValue(symbol_idx, addend);
        std.mem.writeInt(i64, self.rodata.items[offset..][0..8], relocated_value, .little);
        try self.rodata_relocs.append(self.allocator, .{
            .offset = offset,
            .symbol_idx = symbol_idx,
            .is_extern = is_extern,
        });
    }

    /// Add string to string table
    fn addString(self: *Self, str: []const u8) Allocator.Error!u32 {
        const offset: u32 = @intCast(self.strtab.items.len);
        try self.strtab.appendSlice(self.allocator, str);
        try self.strtab.append(self.allocator, 0);
        return offset;
    }

    /// Write the Mach-O object file
    pub fn write(self: *Self, output: *std.ArrayList(u8)) Allocator.Error!void {
        // Calculate sizes
        const header_size: u32 = @sizeOf(MachHeader64);
        const section_count: u32 = 5;
        const segment_cmd_size: u32 = @sizeOf(SegmentCommand64) + section_count * @sizeOf(Section64);
        const symtab_cmd_size: u32 = @sizeOf(SymtabCommand);
        const dysymtab_cmd_size: u32 = @sizeOf(DysymtabCommand);
        const build_version_cmd_size: u32 = @sizeOf(BuildVersionCommand);
        const total_cmd_size: u32 = segment_cmd_size + symtab_cmd_size + dysymtab_cmd_size + build_version_cmd_size;

        // Calculate offsets
        const text_offset: u32 = header_size + total_cmd_size;
        const text_size: u32 = @intCast(self.text.items.len);

        const rodata_offset: u32 = text_offset + text_size;
        const rodata_size: u32 = @intCast(self.rodata.items.len);

        const text_reloc_offset: u32 = rodata_offset + rodata_size;
        const text_reloc_size: u32 = @intCast(self.text_relocs.items.len * @sizeOf(RelocationInfo));
        const rodata_reloc_offset: u32 = text_reloc_offset + text_reloc_size;
        const rodata_reloc_size: u32 = @intCast(self.rodata_relocs.items.len * @sizeOf(RelocationInfo));

        const debug_line_offset: u32 = rodata_reloc_offset + rodata_reloc_size;
        const debug_line_size: u32 = @intCast(self.debug_line.items.len);
        const debug_abbrev_offset: u32 = debug_line_offset + debug_line_size;
        const debug_abbrev_size: u32 = @intCast(self.debug_abbrev.items.len);
        const debug_info_offset: u32 = debug_abbrev_offset + debug_abbrev_size;
        const debug_info_size: u32 = @intCast(self.debug_info.items.len);
        const debug_line_reloc_offset: u32 = debug_info_offset + debug_info_size;
        const debug_line_reloc_size: u32 = @intCast(self.debug_line_relocs.items.len * @sizeOf(RelocationInfo));
        const debug_info_reloc_offset: u32 = debug_line_reloc_offset + debug_line_reloc_size;
        const debug_info_reloc_size: u32 = @intCast(self.debug_info_relocs.items.len * @sizeOf(RelocationInfo));

        const symtab_offset: u32 = debug_info_reloc_offset + debug_info_reloc_size;

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

        var symbol_order = try std.ArrayList(u32).initCapacity(self.allocator, num_syms);
        defer symbol_order.deinit(self.allocator);

        const symbol_index_remap = try self.allocator.alloc(u32, num_syms);
        defer self.allocator.free(symbol_index_remap);

        try appendSymbolOrder(self.allocator, &symbol_order, symbol_index_remap, self.symbols.items, .local);
        try appendSymbolOrder(self.allocator, &symbol_order, symbol_index_remap, self.symbols.items, .external_definition);
        try appendSymbolOrder(self.allocator, &symbol_order, symbol_index_remap, self.symbols.items, .undefined);

        // Build string table with symbol names (Mach-O C ABI requires underscore prefix)
        for (symbol_order.items) |original_idx| {
            const sym = &self.symbols.items[@as(usize, @intCast(original_idx))];
            try self.strtab.append(self.allocator, '_');
            _ = try self.addString(sym.name);
        }
        const strtab_size: u32 = @intCast(self.strtab.items.len);

        // Write Mach-O header. The dev backend currently emits one text section
        // with direct offsets for some internal calls, so this object must be a
        // single dead-strip atom until those internal edges are represented as
        // relocations.
        const header = MachHeader64{
            .magic = MachO.MH_MAGIC_64,
            .cputype = self.arch.cpuType(),
            .cpusubtype = self.arch.cpuSubtype(),
            .filetype = MachO.MH_OBJECT,
            .ncmds = 4, // segment, symtab, dysymtab, build_version
            .sizeofcmds = total_cmd_size,
            .flags = 0,
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
            .vmsize = text_size + rodata_size + debug_line_size + debug_abbrev_size + debug_info_size,
            .fileoff = text_offset,
            .filesize = text_size + rodata_size,
            .maxprot = 7, // rwx
            .initprot = 7,
            .nsects = section_count,
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
            .reloff = if (self.text_relocs.items.len > 0) text_reloc_offset else 0,
            .nreloc = @intCast(self.text_relocs.items.len),
            .flags = MachO.S_ATTR_PURE_INSTRUCTIONS | MachO.S_ATTR_SOME_INSTRUCTIONS,
            .reserved1 = 0,
            .reserved2 = 0,
            .reserved3 = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&text_section));

        var const_sectname: [16]u8 = std.mem.zeroes([16]u8);
        @memcpy(const_sectname[0..7], "__const");
        var const_segname: [16]u8 = std.mem.zeroes([16]u8);
        @memcpy(const_segname[0..6], "__DATA");

        const rodata_section = Section64{
            .sectname = const_sectname,
            .segname = const_segname,
            .addr = text_size,
            .size = rodata_size,
            .offset = rodata_offset,
            .@"align" = 4,
            .reloff = if (self.rodata_relocs.items.len > 0) rodata_reloc_offset else 0,
            .nreloc = @intCast(self.rodata_relocs.items.len),
            .flags = 0,
            .reserved1 = 0,
            .reserved2 = 0,
            .reserved3 = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&rodata_section));

        var dwarf_segname: [16]u8 = std.mem.zeroes([16]u8);
        @memcpy(dwarf_segname[0..7], "__DWARF");
        const debug_addr_base: u64 = @as(u64, text_size) + rodata_size;

        var dbg_line_name: [16]u8 = std.mem.zeroes([16]u8);
        @memcpy(dbg_line_name[0..12], "__debug_line");
        const debug_line_section = Section64{
            .sectname = dbg_line_name,
            .segname = dwarf_segname,
            .addr = debug_addr_base,
            .size = debug_line_size,
            .offset = debug_line_offset,
            .@"align" = 0,
            .reloff = if (self.debug_line_relocs.items.len > 0) debug_line_reloc_offset else 0,
            .nreloc = @intCast(self.debug_line_relocs.items.len),
            .flags = MachO.S_ATTR_DEBUG,
            .reserved1 = 0,
            .reserved2 = 0,
            .reserved3 = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&debug_line_section));

        var dbg_abbrev_name: [16]u8 = std.mem.zeroes([16]u8);
        @memcpy(dbg_abbrev_name[0..14], "__debug_abbrev");
        const debug_abbrev_section = Section64{
            .sectname = dbg_abbrev_name,
            .segname = dwarf_segname,
            .addr = debug_addr_base + debug_line_size,
            .size = debug_abbrev_size,
            .offset = debug_abbrev_offset,
            .@"align" = 0,
            .reloff = 0,
            .nreloc = 0,
            .flags = MachO.S_ATTR_DEBUG,
            .reserved1 = 0,
            .reserved2 = 0,
            .reserved3 = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&debug_abbrev_section));

        var dbg_info_name: [16]u8 = std.mem.zeroes([16]u8);
        @memcpy(dbg_info_name[0..12], "__debug_info");
        const debug_info_section = Section64{
            .sectname = dbg_info_name,
            .segname = dwarf_segname,
            .addr = debug_addr_base + debug_line_size + debug_abbrev_size,
            .size = debug_info_size,
            .offset = debug_info_offset,
            .@"align" = 0,
            .reloff = if (self.debug_info_relocs.items.len > 0) debug_info_reloc_offset else 0,
            .nreloc = @intCast(self.debug_info_relocs.items.len),
            .flags = MachO.S_ATTR_DEBUG,
            .reserved1 = 0,
            .reserved2 = 0,
            .reserved3 = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&debug_info_section));

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

        // Write build version command (required for modern macOS).
        // minos/sdk format: nibbles are xxxx.yy.zz (e.g. 0x000b0000 = 11.0.0).
        const build_version_cmd = BuildVersionCommand{
            .cmd = MachO.LC_BUILD_VERSION,
            .cmdsize = build_version_cmd_size,
            .platform = MachO.PLATFORM_MACOS,
            .minos = roc_target.macos_deployment.macho_encoded_version,
            .sdk = roc_target.macos_deployment.macho_encoded_version,
            .ntools = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&build_version_cmd));

        // Write text section content
        try output.appendSlice(self.allocator, self.text.items);

        // Write read-only data section content
        try output.appendSlice(self.allocator, self.rodata.items);

        // Write relocations
        for (self.text_relocs.items) |rel| {
            const reloc = RelocationInfo.init(
                rel.offset,
                self.relocationSymbolNumber(rel.symbol_idx, rel.is_extern, symbol_index_remap),
                rel.pcrel,
                rel.length,
                rel.is_extern,
                rel.reloc_type,
            );
            try output.appendSlice(self.allocator, std.mem.asBytes(&reloc));
        }

        for (self.rodata_relocs.items) |rel| {
            const reloc_type: u4 = switch (self.arch) {
                .x86_64 => MachO.X86_64_RELOC_UNSIGNED,
                .aarch64 => MachO.ARM64_RELOC_UNSIGNED,
            };
            const reloc = RelocationInfo.init(
                rel.offset,
                self.relocationSymbolNumber(rel.symbol_idx, rel.is_extern, symbol_index_remap),
                false, // absolute pointer
                3, // 64-bit (2^3 = 8 bytes)
                rel.is_extern,
                reloc_type,
            );
            try output.appendSlice(self.allocator, std.mem.asBytes(&reloc));
        }

        // Write debug sections and their relocations
        try output.appendSlice(self.allocator, self.debug_line.items);
        try output.appendSlice(self.allocator, self.debug_abbrev.items);
        try output.appendSlice(self.allocator, self.debug_info.items);
        const unsigned_reloc_type: u4 = switch (self.arch) {
            .x86_64 => MachO.X86_64_RELOC_UNSIGNED,
            .aarch64 => MachO.ARM64_RELOC_UNSIGNED,
        };
        for (self.debug_line_relocs.items) |offset| {
            const reloc = RelocationInfo.init(offset, 1, false, 3, false, unsigned_reloc_type);
            try output.appendSlice(self.allocator, std.mem.asBytes(&reloc));
        }
        for (self.debug_info_relocs.items) |offset| {
            const reloc = RelocationInfo.init(offset, 1, false, 3, false, unsigned_reloc_type);
            try output.appendSlice(self.allocator, std.mem.asBytes(&reloc));
        }

        // Write symbol table
        var str_offset: u32 = 2; // Skip initial " \0"
        for (symbol_order.items) |original_idx| {
            const sym = self.symbols.items[@as(usize, @intCast(original_idx))];
            const n_type: u8 = if (sym.section == 0)
                MachO.N_UNDF | MachO.N_EXT
            else if (sym.is_external)
                MachO.N_SECT | MachO.N_EXT
            else
                MachO.N_SECT;
            const section_addr: u64 = switch (sym.section) {
                0 => 0,
                1 => 0,
                2 => text_size,
                else => unreachable,
            };

            const nlist = Nlist64{
                .n_strx = str_offset,
                .n_type = n_type,
                .n_sect = sym.section,
                .n_desc = 0,
                .n_value = section_addr + sym.offset,
            };
            try output.appendSlice(self.allocator, std.mem.asBytes(&nlist));

            str_offset += @intCast(sym.name.len + 2); // +1 underscore prefix, +1 null terminator
        }

        // Write string table
        try output.appendSlice(self.allocator, self.strtab.items);
    }

    const SymbolTableClass = enum {
        local,
        external_definition,
        undefined,
    };

    fn symbolTableClass(symbol: Symbol) SymbolTableClass {
        if (symbol.section == 0) return .undefined;
        if (symbol.is_external) return .external_definition;
        return .local;
    }

    fn appendSymbolOrder(
        allocator: Allocator,
        symbol_order: *std.ArrayList(u32),
        symbol_index_remap: []u32,
        symbols: []const Symbol,
        class: SymbolTableClass,
    ) Allocator.Error!void {
        for (symbols, 0..) |symbol, original_idx| {
            if (symbolTableClass(symbol) != class) continue;

            symbol_index_remap[original_idx] = @intCast(symbol_order.items.len);
            try symbol_order.append(allocator, @intCast(original_idx));
        }
    }

    fn relocationSymbolNumber(self: *const Self, symbol_idx: u32, is_extern: bool, symbol_index_remap: []const u32) u24 {
        if (is_extern) return @intCast(symbol_index_remap[@as(usize, @intCast(symbol_idx))]);

        const section = self.symbols.items[symbol_idx].section;
        if (builtin.mode == .Debug and section == 0) {
            std.debug.panic("Mach-O invariant violated: local relocation targets undefined symbol {d}", .{symbol_idx});
        }
        if (section == 0) unreachable;
        return @intCast(section);
    }

    fn localRelocationValue(self: *const Self, symbol_idx: u32, addend: i64) i64 {
        const symbol = self.symbols.items[symbol_idx];
        if (builtin.mode == .Debug and symbol.section == 0) {
            std.debug.panic("Mach-O invariant violated: local relocation value requested for undefined symbol {d}", .{symbol_idx});
        }
        if (symbol.section == 0) unreachable;

        const section_addr: u64 = switch (symbol.section) {
            1 => 0,
            2 => @as(u64, @intCast(self.text.items.len)),
            else => unreachable,
        };
        const base = section_addr + symbol.offset;
        return @as(i64, @intCast(base)) + addend;
    }
};

// Tests

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

    var output: std.ArrayList(u8) = .empty;
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

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(std.testing.allocator);

    try writer.write(&output);

    const magic = std.mem.readInt(u32, output.items[0..4], .little);
    try std.testing.expectEqual(MachO.MH_MAGIC_64, magic);
}
