//! ELF object file writer for the dev backend.
//!
//! This module writes ELF (Executable and Linkable Format) object files
//! from generated machine code and relocations. It produces relocatable
//! object files (.o) that can be linked with other objects to create
//! executables or shared libraries.
//!
//! Reference: https://refspecs.linuxfoundation.org/elf/elf.pdf

const std = @import("std");
const Allocator = std.mem.Allocator;
const DataRelocationKind = @import("../Relocation.zig").DataRelocationKind;

/// ELF file header constants
const ELF = struct {
    // ELF identification
    const MAGIC = "\x7fELF".*;
    const CLASS_64 = 2;
    const DATA_LSB = 1; // Little endian
    const VERSION_CURRENT = 1;
    const OSABI_NONE = 0;

    // ELF type
    const ET_REL = 1; // Relocatable file

    // Machine types
    const EM_X86_64 = 62;
    const EM_AARCH64 = 183;

    // Section header types
    const SHT_PROGBITS = 1;
    const SHT_SYMTAB = 2;
    const SHT_STRTAB = 3;
    const SHT_RELA = 4;

    // Section flags
    const SHF_ALLOC = 0x2;
    const SHF_EXECINSTR = 0x4;
    const SHF_INFO_LINK = 0x40;

    // Symbol binding
    const STB_LOCAL = 0;
    const STB_GLOBAL = 1;

    // Symbol type
    const STT_NOTYPE = 0;
    const STT_OBJECT = 1;
    const STT_FUNC = 2;
    const STT_SECTION = 3;

    // Special section indices
    const SHN_UNDEF = 0;

    // x86_64 relocation types
    const R_X86_64_64 = 1;
    const R_X86_64_PC32 = 2;
    const R_X86_64_PLT32 = 4;

    // aarch64 relocation types
    const R_AARCH64_ABS64 = 257;
    const R_AARCH64_ADR_PREL_PG_HI21 = 275;
    const R_AARCH64_ADD_ABS_LO12_NC = 277;
    const R_AARCH64_CALL26 = 283;
};

/// ELF64 file header (64 bytes)
const Elf64_Ehdr = extern struct {
    e_ident: [16]u8,
    e_type: u16,
    e_machine: u16,
    e_version: u32,
    e_entry: u64,
    e_phoff: u64,
    e_shoff: u64,
    e_flags: u32,
    e_ehsize: u16,
    e_phentsize: u16,
    e_phnum: u16,
    e_shentsize: u16,
    e_shnum: u16,
    e_shstrndx: u16,
};

/// ELF64 section header (64 bytes)
const Elf64_Shdr = extern struct {
    sh_name: u32,
    sh_type: u32,
    sh_flags: u64,
    sh_addr: u64,
    sh_offset: u64,
    sh_size: u64,
    sh_link: u32,
    sh_info: u32,
    sh_addralign: u64,
    sh_entsize: u64,
};

/// ELF64 symbol table entry (24 bytes)
const Elf64_Sym = extern struct {
    st_name: u32,
    st_info: u8,
    st_other: u8,
    st_shndx: u16,
    st_value: u64,
    st_size: u64,
};

/// ELF64 relocation entry with addend (24 bytes)
const Elf64_Rela = extern struct {
    r_offset: u64,
    r_info: u64,
    r_addend: i64,
};

/// Target architecture for ELF generation
pub const Architecture = enum {
    x86_64,
    aarch64,

    fn machine(self: Architecture) u16 {
        return switch (self) {
            .x86_64 => ELF.EM_X86_64,
            .aarch64 => ELF.EM_AARCH64,
        };
    }
};

/// Symbol definition for the object file
pub const Symbol = struct {
    name: []const u8,
    section: Section,
    offset: u64,
    size: u64,
    is_global: bool,
    is_function: bool,
};

/// Section types
pub const Section = enum {
    text,
    data,
    rodata,
    bss,
    undef, // External symbol
};

/// ELF object file writer
pub const ElfWriter = struct {
    const Self = @This();

    allocator: Allocator,
    arch: Architecture,

    // Section contents
    text: std.ArrayList(u8),
    data: std.ArrayList(u8),
    rodata: std.ArrayList(u8),

    // Symbol table
    symbols: std.ArrayList(Symbol),

    // Relocations for .text section
    text_relocs: std.ArrayList(TextReloc),
    rodata_relocs: std.ArrayList(TextReloc),

    // DWARF debug sections plus their text-relative address relocations.
    debug_line: std.ArrayList(u8),
    debug_abbrev: std.ArrayList(u8),
    debug_info: std.ArrayList(u8),
    debug_line_relocs: std.ArrayList(DebugReloc),
    debug_info_relocs: std.ArrayList(DebugReloc),

    // String tables
    strtab: std.ArrayList(u8),
    shstrtab: std.ArrayList(u8),

    const TextReloc = struct {
        offset: u64, // Offset in .text where relocation applies
        symbol_idx: u32, // Index into symbol table
        reloc_type: u32, // Architecture-specific relocation type
        addend: i64,
    };

    const DebugReloc = struct {
        /// Offset of the 8-byte address field within its debug section.
        offset: u64,
        /// Offset from the start of the text section.
        addend: i64,
    };

    const TextDataReloc = struct {
        kind: u32,
        addend: i64,
    };

    pub fn init(allocator: Allocator, arch: Architecture) Allocator.Error!Self {
        var self = Self{
            .allocator = allocator,
            .arch = arch,
            .text = .empty,
            .data = .empty,
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
            .shstrtab = .empty,
        };

        // Initialize string tables with null byte
        try self.strtab.append(allocator, 0);
        try self.shstrtab.append(allocator, 0);

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.text.deinit(self.allocator);
        self.data.deinit(self.allocator);
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
        self.shstrtab.deinit(self.allocator);
    }

    /// Set the code section contents
    pub fn setCode(self: *Self, code: []const u8) Allocator.Error!void {
        self.text.clearRetainingCapacity();
        try self.text.appendSlice(self.allocator, code);
    }

    /// Set the read-only data section contents.
    pub fn setRodata(self: *Self, rodata: []const u8) Allocator.Error!void {
        self.rodata.clearRetainingCapacity();
        try self.rodata.appendSlice(self.allocator, rodata);
    }

    /// Set the DWARF debug section contents. Relocation addends are offsets
    /// from the start of the text section; each relocated field is 8 bytes.
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
        for (line_relocs) |rel| {
            try self.debug_line_relocs.append(self.allocator, .{
                .offset = rel.section_offset,
                .addend = @intCast(rel.addend),
            });
        }
        for (info_relocs) |rel| {
            try self.debug_info_relocs.append(self.allocator, .{
                .offset = rel.section_offset,
                .addend = @intCast(rel.addend),
            });
        }
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

    /// Add a symbol to the object file
    pub fn addSymbol(self: *Self, symbol: Symbol) Allocator.Error!u32 {
        const idx: u32 = @intCast(self.symbols.items.len);
        try self.symbols.append(self.allocator, symbol);
        return idx;
    }

    /// Add an external symbol reference
    pub fn addExternalSymbol(self: *Self, name: []const u8) Allocator.Error!u32 {
        return self.addSymbol(.{
            .name = name,
            .section = .undef,
            .offset = 0,
            .size = 0,
            .is_global = true,
            .is_function = true,
        });
    }

    /// Add an absolute pointer relocation to the rodata section.
    pub fn addRodataRelocation(self: *Self, offset: u64, symbol_idx: u32, addend: i64) Allocator.Error!void {
        const reloc_type: u32 = switch (self.arch) {
            .x86_64 => ELF.R_X86_64_64,
            .aarch64 => ELF.R_AARCH64_ABS64,
        };

        try self.rodata_relocs.append(self.allocator, .{
            .offset = offset,
            .symbol_idx = symbol_idx,
            .reloc_type = reloc_type,
            .addend = addend,
        });
    }

    /// Add a relocation to the text section
    pub fn addTextRelocation(self: *Self, offset: u64, symbol_idx: u32, addend: i64) Allocator.Error!void {
        const reloc_type: u32 = switch (self.arch) {
            .x86_64 => ELF.R_X86_64_PLT32,
            .aarch64 => ELF.R_AARCH64_CALL26,
        };

        try self.text_relocs.append(self.allocator, .{
            .offset = offset,
            .symbol_idx = symbol_idx,
            .reloc_type = reloc_type,
            .addend = addend,
        });
    }

    /// Add a data-address relocation to the text section.
    pub fn addTextDataRelocation(self: *Self, offset: u64, symbol_idx: u32, kind: DataRelocationKind) Allocator.Error!void {
        const reloc: TextDataReloc = switch (kind) {
            .abs64 => .{
                .kind = switch (self.arch) {
                    .x86_64 => ELF.R_X86_64_64,
                    .aarch64 => ELF.R_AARCH64_ABS64,
                },
                .addend = @as(i64, 0),
            },
            .rel32 => .{
                .kind = switch (self.arch) {
                    .x86_64 => ELF.R_X86_64_PC32,
                    .aarch64 => unreachable,
                },
                .addend = @as(i64, -4),
            },
            .page21 => .{
                .kind = switch (self.arch) {
                    .x86_64 => unreachable,
                    .aarch64 => ELF.R_AARCH64_ADR_PREL_PG_HI21,
                },
                .addend = @as(i64, 0),
            },
            .pageoff12 => .{
                .kind = switch (self.arch) {
                    .x86_64 => unreachable,
                    .aarch64 => ELF.R_AARCH64_ADD_ABS_LO12_NC,
                },
                .addend = @as(i64, 0),
            },
        };

        try self.text_relocs.append(self.allocator, .{
            .offset = offset,
            .symbol_idx = symbol_idx,
            .reloc_type = reloc.kind,
            .addend = reloc.addend,
        });
    }

    /// Add a string to the string table, return its offset
    fn addString(self: *Self, table: *std.ArrayList(u8), str: []const u8) Allocator.Error!u32 {
        const offset: u32 = @intCast(table.items.len);
        try table.appendSlice(self.allocator, str);
        try table.append(self.allocator, 0); // Null terminator
        return offset;
    }

    /// Write the ELF object file to a buffer
    pub fn write(self: *Self, output: *std.ArrayList(u8)) Allocator.Error!void {
        // Section indices
        const SHIDX_TEXT = 1;
        const SHIDX_RODATA = 2;
        const SHIDX_SYMTAB = 5;
        const SHIDX_STRTAB = 6;
        const SHIDX_SHSTRTAB = 7;
        const SHIDX_DEBUG_LINE = 8;
        const SHIDX_DEBUG_INFO = 10;
        const NUM_SECTIONS = 13;

        // Add section names to shstrtab
        const shname_text = try self.addString(&self.shstrtab, ".text");
        const shname_rodata = try self.addString(&self.shstrtab, ".rodata");
        const shname_rela_text = try self.addString(&self.shstrtab, ".rela.text");
        const shname_rela_rodata = try self.addString(&self.shstrtab, ".rela.rodata");
        const shname_symtab = try self.addString(&self.shstrtab, ".symtab");
        const shname_strtab = try self.addString(&self.shstrtab, ".strtab");
        const shname_shstrtab = try self.addString(&self.shstrtab, ".shstrtab");
        const shname_debug_line = try self.addString(&self.shstrtab, ".debug_line");
        const shname_debug_abbrev = try self.addString(&self.shstrtab, ".debug_abbrev");
        const shname_debug_info = try self.addString(&self.shstrtab, ".debug_info");
        const shname_rela_debug_line = try self.addString(&self.shstrtab, ".rela.debug_line");
        const shname_rela_debug_info = try self.addString(&self.shstrtab, ".rela.debug_info");

        // Build symbol table
        var symtab: std.ArrayList(u8) = .empty;
        defer symtab.deinit(self.allocator);

        // First symbol is always null
        try symtab.appendSlice(self.allocator, &std.mem.zeroes([24]u8));

        // Section symbol for .text (index 1), used by debug relocations.
        const text_section_sym = Elf64_Sym{
            .st_name = 0,
            .st_info = (ELF.STB_LOCAL << 4) | ELF.STT_SECTION,
            .st_other = 0,
            .st_shndx = SHIDX_TEXT,
            .st_value = 0,
            .st_size = 0,
        };
        try symtab.appendSlice(self.allocator, std.mem.asBytes(&text_section_sym));
        const TEXT_SECTION_SYM_IDX: u64 = 1;

        // Count local symbols (for sh_info)
        var num_locals: u32 = 2; // Null symbol plus the .text section symbol

        // Add symbols
        for (self.symbols.items) |sym| {
            const name_offset = try self.addString(&self.strtab, sym.name);

            const st_info: u8 = blk: {
                const bind: u8 = if (sym.is_global) ELF.STB_GLOBAL else ELF.STB_LOCAL;
                const sym_type: u8 = if (sym.is_function) ELF.STT_FUNC else if (sym.section == .rodata) ELF.STT_OBJECT else ELF.STT_NOTYPE;
                break :blk (bind << 4) | sym_type;
            };

            const st_shndx: u16 = switch (sym.section) {
                .text => SHIDX_TEXT,
                .data => 0, // Would be data section index
                .rodata => SHIDX_RODATA,
                .bss => 0,
                .undef => ELF.SHN_UNDEF,
            };

            const elf_sym = Elf64_Sym{
                .st_name = name_offset,
                .st_info = st_info,
                .st_other = 0,
                .st_shndx = st_shndx,
                .st_value = sym.offset,
                .st_size = sym.size,
            };

            try symtab.appendSlice(self.allocator, std.mem.asBytes(&elf_sym));

            if (!sym.is_global) {
                num_locals += 1;
            }
        }

        // Build relocation tables
        var rela_text: std.ArrayList(u8) = .empty;
        defer rela_text.deinit(self.allocator);

        for (self.text_relocs.items) |rel| {
            // Symbol index is +2: null symbol plus the .text section symbol
            const r_info: u64 = (@as(u64, rel.symbol_idx + 2) << 32) | rel.reloc_type;

            const elf_rela = Elf64_Rela{
                .r_offset = rel.offset,
                .r_info = r_info,
                .r_addend = rel.addend,
            };

            try rela_text.appendSlice(self.allocator, std.mem.asBytes(&elf_rela));
        }

        var rela_rodata: std.ArrayList(u8) = .empty;
        defer rela_rodata.deinit(self.allocator);

        for (self.rodata_relocs.items) |rel| {
            // Symbol index is +2: null symbol plus the .text section symbol
            const r_info: u64 = (@as(u64, rel.symbol_idx + 2) << 32) | rel.reloc_type;

            const elf_rela = Elf64_Rela{
                .r_offset = rel.offset,
                .r_info = r_info,
                .r_addend = rel.addend,
            };

            try rela_rodata.appendSlice(self.allocator, std.mem.asBytes(&elf_rela));
        }

        const abs64_reloc_type: u32 = switch (self.arch) {
            .x86_64 => ELF.R_X86_64_64,
            .aarch64 => ELF.R_AARCH64_ABS64,
        };
        var rela_debug_line: std.ArrayList(u8) = .empty;
        defer rela_debug_line.deinit(self.allocator);
        for (self.debug_line_relocs.items) |rel| {
            const elf_rela = Elf64_Rela{
                .r_offset = rel.offset,
                .r_info = (TEXT_SECTION_SYM_IDX << 32) | abs64_reloc_type,
                .r_addend = rel.addend,
            };
            try rela_debug_line.appendSlice(self.allocator, std.mem.asBytes(&elf_rela));
        }
        var rela_debug_info: std.ArrayList(u8) = .empty;
        defer rela_debug_info.deinit(self.allocator);
        for (self.debug_info_relocs.items) |rel| {
            const elf_rela = Elf64_Rela{
                .r_offset = rel.offset,
                .r_info = (TEXT_SECTION_SYM_IDX << 32) | abs64_reloc_type,
                .r_addend = rel.addend,
            };
            try rela_debug_info.appendSlice(self.allocator, std.mem.asBytes(&elf_rela));
        }

        // Calculate offsets
        const ehdr_size: u64 = @sizeOf(Elf64_Ehdr);

        // Section data starts after headers
        var offset: u64 = ehdr_size;

        // Align sections
        const text_offset = alignUp(offset, 16);
        offset = text_offset + self.text.items.len;

        const rodata_offset = alignUp(offset, 16);
        offset = rodata_offset + self.rodata.items.len;

        const rela_text_offset = alignUp(offset, 8);
        offset = rela_text_offset + rela_text.items.len;

        const rela_rodata_offset = alignUp(offset, 8);
        offset = rela_rodata_offset + rela_rodata.items.len;

        const symtab_offset = alignUp(offset, 8);
        offset = symtab_offset + symtab.items.len;

        const strtab_offset = offset;
        offset = strtab_offset + self.strtab.items.len;

        const shstrtab_offset = offset;
        offset = shstrtab_offset + self.shstrtab.items.len;

        const debug_line_offset = offset;
        offset = debug_line_offset + self.debug_line.items.len;
        const debug_abbrev_offset = offset;
        offset = debug_abbrev_offset + self.debug_abbrev.items.len;
        const debug_info_offset = offset;
        offset = debug_info_offset + self.debug_info.items.len;
        const rela_debug_line_offset = alignUp(offset, 8);
        offset = rela_debug_line_offset + rela_debug_line.items.len;
        const rela_debug_info_offset = alignUp(offset, 8);
        offset = rela_debug_info_offset + rela_debug_info.items.len;

        const shdr_offset = alignUp(offset, 8);

        // Write ELF header
        var ehdr = Elf64_Ehdr{
            .e_ident = undefined,
            .e_type = ELF.ET_REL,
            .e_machine = self.arch.machine(),
            .e_version = ELF.VERSION_CURRENT,
            .e_entry = 0,
            .e_phoff = 0,
            .e_shoff = shdr_offset,
            .e_flags = 0,
            .e_ehsize = @sizeOf(Elf64_Ehdr),
            .e_phentsize = 0,
            .e_phnum = 0,
            .e_shentsize = @sizeOf(Elf64_Shdr),
            .e_shnum = NUM_SECTIONS,
            .e_shstrndx = SHIDX_SHSTRTAB,
        };

        // Set e_ident
        @memcpy(ehdr.e_ident[0..4], &ELF.MAGIC);
        ehdr.e_ident[4] = ELF.CLASS_64;
        ehdr.e_ident[5] = ELF.DATA_LSB;
        ehdr.e_ident[6] = ELF.VERSION_CURRENT;
        ehdr.e_ident[7] = ELF.OSABI_NONE;
        @memset(ehdr.e_ident[8..16], 0);

        try output.appendSlice(self.allocator, std.mem.asBytes(&ehdr));

        // Pad to text section
        try self.padTo(output, text_offset);
        try output.appendSlice(self.allocator, self.text.items);

        try self.padTo(output, rodata_offset);
        try output.appendSlice(self.allocator, self.rodata.items);

        // Pad to rela sections
        try self.padTo(output, rela_text_offset);
        try output.appendSlice(self.allocator, rela_text.items);

        try self.padTo(output, rela_rodata_offset);
        try output.appendSlice(self.allocator, rela_rodata.items);

        // Pad to symtab
        try self.padTo(output, symtab_offset);
        try output.appendSlice(self.allocator, symtab.items);

        // strtab (no padding needed)
        try output.appendSlice(self.allocator, self.strtab.items);

        // shstrtab
        try output.appendSlice(self.allocator, self.shstrtab.items);

        // Debug sections
        try output.appendSlice(self.allocator, self.debug_line.items);
        try output.appendSlice(self.allocator, self.debug_abbrev.items);
        try output.appendSlice(self.allocator, self.debug_info.items);
        try self.padTo(output, rela_debug_line_offset);
        try output.appendSlice(self.allocator, rela_debug_line.items);
        try self.padTo(output, rela_debug_info_offset);
        try output.appendSlice(self.allocator, rela_debug_info.items);

        // Pad to section headers
        try self.padTo(output, shdr_offset);

        // Write section headers
        // 0: NULL section
        try output.appendSlice(self.allocator, &std.mem.zeroes([64]u8));

        // 1: .text
        const shdr_text = Elf64_Shdr{
            .sh_name = shname_text,
            .sh_type = ELF.SHT_PROGBITS,
            .sh_flags = ELF.SHF_ALLOC | ELF.SHF_EXECINSTR,
            .sh_addr = 0,
            .sh_offset = text_offset,
            .sh_size = self.text.items.len,
            .sh_link = 0,
            .sh_info = 0,
            .sh_addralign = 16,
            .sh_entsize = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_text));

        // 2: .rodata
        const shdr_rodata = Elf64_Shdr{
            .sh_name = shname_rodata,
            .sh_type = ELF.SHT_PROGBITS,
            .sh_flags = ELF.SHF_ALLOC,
            .sh_addr = 0,
            .sh_offset = rodata_offset,
            .sh_size = self.rodata.items.len,
            .sh_link = 0,
            .sh_info = 0,
            .sh_addralign = 16,
            .sh_entsize = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_rodata));

        // 3: .rela.text
        const shdr_rela = Elf64_Shdr{
            .sh_name = shname_rela_text,
            .sh_type = ELF.SHT_RELA,
            .sh_flags = ELF.SHF_INFO_LINK,
            .sh_addr = 0,
            .sh_offset = rela_text_offset,
            .sh_size = rela_text.items.len,
            .sh_link = SHIDX_SYMTAB, // Associated symbol table
            .sh_info = SHIDX_TEXT, // Section to which relocs apply
            .sh_addralign = 8,
            .sh_entsize = @sizeOf(Elf64_Rela),
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_rela));

        // 4: .rela.rodata
        const shdr_rela_rodata = Elf64_Shdr{
            .sh_name = shname_rela_rodata,
            .sh_type = ELF.SHT_RELA,
            .sh_flags = ELF.SHF_INFO_LINK,
            .sh_addr = 0,
            .sh_offset = rela_rodata_offset,
            .sh_size = rela_rodata.items.len,
            .sh_link = SHIDX_SYMTAB, // Associated symbol table
            .sh_info = SHIDX_RODATA, // Section to which relocs apply
            .sh_addralign = 8,
            .sh_entsize = @sizeOf(Elf64_Rela),
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_rela_rodata));

        // 5: .symtab
        const shdr_symtab = Elf64_Shdr{
            .sh_name = shname_symtab,
            .sh_type = ELF.SHT_SYMTAB,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = symtab_offset,
            .sh_size = symtab.items.len,
            .sh_link = SHIDX_STRTAB, // Associated string table
            .sh_info = num_locals, // Index of first non-local symbol
            .sh_addralign = 8,
            .sh_entsize = @sizeOf(Elf64_Sym),
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_symtab));

        // 6: .strtab
        const shdr_strtab = Elf64_Shdr{
            .sh_name = shname_strtab,
            .sh_type = ELF.SHT_STRTAB,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = strtab_offset,
            .sh_size = self.strtab.items.len,
            .sh_link = 0,
            .sh_info = 0,
            .sh_addralign = 1,
            .sh_entsize = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_strtab));

        // 7: .shstrtab
        const shdr_shstrtab = Elf64_Shdr{
            .sh_name = shname_shstrtab,
            .sh_type = ELF.SHT_STRTAB,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = shstrtab_offset,
            .sh_size = self.shstrtab.items.len,
            .sh_link = 0,
            .sh_info = 0,
            .sh_addralign = 1,
            .sh_entsize = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_shstrtab));

        // 8: .debug_line
        const shdr_debug_line = Elf64_Shdr{
            .sh_name = shname_debug_line,
            .sh_type = ELF.SHT_PROGBITS,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = debug_line_offset,
            .sh_size = self.debug_line.items.len,
            .sh_link = 0,
            .sh_info = 0,
            .sh_addralign = 1,
            .sh_entsize = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_debug_line));

        // 9: .debug_abbrev
        const shdr_debug_abbrev = Elf64_Shdr{
            .sh_name = shname_debug_abbrev,
            .sh_type = ELF.SHT_PROGBITS,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = debug_abbrev_offset,
            .sh_size = self.debug_abbrev.items.len,
            .sh_link = 0,
            .sh_info = 0,
            .sh_addralign = 1,
            .sh_entsize = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_debug_abbrev));

        // 10: .debug_info
        const shdr_debug_info = Elf64_Shdr{
            .sh_name = shname_debug_info,
            .sh_type = ELF.SHT_PROGBITS,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = debug_info_offset,
            .sh_size = self.debug_info.items.len,
            .sh_link = 0,
            .sh_info = 0,
            .sh_addralign = 1,
            .sh_entsize = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_debug_info));

        // 11: .rela.debug_line
        const shdr_rela_debug_line = Elf64_Shdr{
            .sh_name = shname_rela_debug_line,
            .sh_type = ELF.SHT_RELA,
            .sh_flags = ELF.SHF_INFO_LINK,
            .sh_addr = 0,
            .sh_offset = rela_debug_line_offset,
            .sh_size = rela_debug_line.items.len,
            .sh_link = SHIDX_SYMTAB,
            .sh_info = SHIDX_DEBUG_LINE,
            .sh_addralign = 8,
            .sh_entsize = @sizeOf(Elf64_Rela),
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_rela_debug_line));

        // 12: .rela.debug_info
        const shdr_rela_debug_info = Elf64_Shdr{
            .sh_name = shname_rela_debug_info,
            .sh_type = ELF.SHT_RELA,
            .sh_flags = ELF.SHF_INFO_LINK,
            .sh_addr = 0,
            .sh_offset = rela_debug_info_offset,
            .sh_size = rela_debug_info.items.len,
            .sh_link = SHIDX_SYMTAB,
            .sh_info = SHIDX_DEBUG_INFO,
            .sh_addralign = 8,
            .sh_entsize = @sizeOf(Elf64_Rela),
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_rela_debug_info));
    }

    fn padTo(self: *Self, output: *std.ArrayList(u8), target: u64) Allocator.Error!void {
        const current: u64 = @intCast(output.items.len);
        if (current < target) {
            const padding: usize = @intCast(target - current);
            try output.appendNTimes(self.allocator, 0, padding);
        }
    }
};

fn alignUp(value: u64, alignment: u64) u64 {
    return (value + alignment - 1) & ~(alignment - 1);
}

// Tests

test "create minimal elf object" {
    var writer = try ElfWriter.init(std.testing.allocator, .x86_64);
    defer writer.deinit();

    // Add some test code (ret instruction)
    try writer.setCode(&[_]u8{0xC3});

    // Add a symbol for the function
    _ = try writer.addSymbol(.{
        .name = "test_func",
        .section = .text,
        .offset = 0,
        .size = 1,
        .is_global = true,
        .is_function = true,
    });

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(std.testing.allocator);

    try writer.write(&output);

    // Check ELF magic
    try std.testing.expectEqualSlices(u8, "\x7fELF", output.items[0..4]);

    // Check it's 64-bit
    try std.testing.expectEqual(@as(u8, 2), output.items[4]);

    // Check it's little endian
    try std.testing.expectEqual(@as(u8, 1), output.items[5]);
}

test "elf with external symbol" {
    var writer = try ElfWriter.init(std.testing.allocator, .x86_64);
    defer writer.deinit();

    // Simple code: call to external function (placeholder)
    try writer.setCode(&[_]u8{ 0xE8, 0x00, 0x00, 0x00, 0x00, 0xC3 });

    // Add external symbol
    const ext_idx = try writer.addExternalSymbol("external_func");

    // Add relocation for the call
    try writer.addTextRelocation(1, ext_idx, -4);

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(std.testing.allocator);

    try writer.write(&output);

    // Should produce valid ELF
    try std.testing.expectEqualSlices(u8, "\x7fELF", output.items[0..4]);
}
