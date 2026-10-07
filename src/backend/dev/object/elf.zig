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
const object = @import("mod.zig");
const DebugReloc = object.DebugReloc;

/// ELF file header constants
const ELF = struct {
    // ELF identification
    const MAGIC = "\x7fELF".*;
    const CLASS_64 = 2;
    const DATA_LSB = 1; // Little endian
    const VERSION_CURRENT = 1;

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
    const SHT_NOBITS = 8;

    // Section flags
    const SHF_WRITE = 0x1;
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

    // Symbol visibility
    const STV_DEFAULT = 0;
    const STV_HIDDEN = 2;

    // Special section indices
    const SHN_UNDEF = 0;

    // x86_64 relocation types
    const R_X86_64_64 = 1;
    const R_X86_64_PC32 = 2;
    const R_X86_64_PLT32 = 4;
    const R_X86_64_32 = 10;

    // aarch64 relocation types
    const R_AARCH64_ABS64 = 257;
    const R_AARCH64_ABS32 = 258;
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
    sh_flags: u64 = 0,
    sh_addr: u64 = 0,
    sh_offset: u64,
    sh_size: u64,
    sh_link: u32 = 0,
    sh_info: u32 = 0,
    sh_addralign: u64,
    sh_entsize: u64 = 0,
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
    is_hidden: bool = false,
};

/// Section types
pub const Section = enum {
    text,
    data,
    rodata,
    bss,
    undef, // External symbol
};

/// The `EI_OSABI` byte an object declares.
///
/// A linker reads this from its input objects to decide which OS-specific
/// program headers the output needs: `ld.lld` emits OpenBSD's `PT_OPENBSD_*`
/// headers only when it infers that OSABI from an input, and it takes the value
/// from the first input that declares anything other than `none`. These values
/// match what LLVM writes for the same triples, so the two backends' objects
/// agree when they meet in one link.
pub const Osabi = enum(u8) {
    none = 0,
    freebsd = 9,
    openbsd = 12,
};

/// ELF object file writer
pub const ElfWriter = struct {
    const Self = @This();

    allocator: Allocator,
    arch: Architecture,
    osabi: Osabi,

    // Borrowed section contents, valid until write completes
    text: []const u8,
    rodata: []const u8,
    /// Size of `.bss`, which the file declares without storing bytes.
    zero_fill_size: u64,

    // Symbol table
    symbols: std.ArrayList(Symbol),

    // Relocations for .text section
    text_relocs: std.ArrayList(TextReloc),
    rodata_relocs: std.ArrayList(TextReloc),

    /// DWARF debug sections plus their explicit cross-section relocations.
    debug: object.DebugSections = .{},

    // String tables
    shstrtab: std.ArrayList(u8),

    const TextReloc = struct {
        offset: u64, // Offset in .text where relocation applies
        symbol_idx: u32, // Index into symbol table
        reloc_type: u32, // Architecture-specific relocation type
        addend: i64,
    };

    const TextDataReloc = struct {
        kind: u32,
        addend: i64,
    };

    pub fn init(allocator: Allocator, arch: Architecture, osabi: Osabi) Allocator.Error!Self {
        var self = Self{
            .allocator = allocator,
            .arch = arch,
            .osabi = osabi,
            .text = &.{},
            .rodata = &.{},
            .zero_fill_size = 0,
            .symbols = .empty,
            .text_relocs = .empty,
            .rodata_relocs = .empty,
            .shstrtab = .empty,
        };

        errdefer self.deinit();

        // Initialize string tables with null byte
        try self.shstrtab.append(allocator, 0);

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.symbols.deinit(self.allocator);
        self.text_relocs.deinit(self.allocator);
        self.rodata_relocs.deinit(self.allocator);
        self.shstrtab.deinit(self.allocator);
    }

    /// Borrow the code section contents until write completes
    pub fn setCode(self: *Self, code: []const u8) void {
        self.text = code;
    }

    /// Borrow read-only data section contents until write completes.
    pub fn setZeroFill(self: *Self, size: u64) void {
        self.zero_fill_size = size;
    }

    pub fn setRodata(self: *Self, rodata: []const u8) void {
        self.rodata = rodata;
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
        const SHIDX_DEBUG_ABBREV = 9;
        const SHIDX_DEBUG_INFO = 10;
        const SHIDX_BSS = 13;
        const NUM_SECTIONS = 14;

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
        const shname_bss = try self.addString(&self.shstrtab, ".bss");

        const debug_target_sections = [_]u16{ SHIDX_TEXT, SHIDX_DEBUG_LINE, SHIDX_DEBUG_ABBREV };
        const WRITER_SYMBOL_OFFSET: u32 = 1 + debug_target_sections.len;
        var num_locals: u32 = WRITER_SYMBOL_OFFSET;
        const symtab_size = (self.symbols.items.len + WRITER_SYMBOL_OFFSET) * @sizeOf(Elf64_Sym);
        var string_bytes: usize = 1; // The string table starts with a null byte.
        for (self.symbols.items) |symbol| string_bytes += symbol.name.len + 1;
        const rela_text_size = self.text_relocs.items.len * @sizeOf(Elf64_Rela);
        const rela_rodata_size = self.rodata_relocs.items.len * @sizeOf(Elf64_Rela);
        const rela_debug_line_size = self.debug.line_relocs.len * @sizeOf(Elf64_Rela);
        const rela_debug_info_size = self.debug.info_relocs.len * @sizeOf(Elf64_Rela);

        // Calculate offsets
        const ehdr_size: u64 = @sizeOf(Elf64_Ehdr);

        // Section data starts after headers
        var offset: u64 = ehdr_size;

        // Align sections
        const text_offset = alignUp(offset, 16);
        offset = text_offset + self.text.len;

        const rodata_offset = alignUp(offset, 16);
        offset = rodata_offset + self.rodata.len;

        const rela_text_offset = alignUp(offset, 8);
        offset = rela_text_offset + rela_text_size;

        const rela_rodata_offset = alignUp(offset, 8);
        offset = rela_rodata_offset + rela_rodata_size;

        const symtab_offset = alignUp(offset, 8);
        offset = symtab_offset + symtab_size;

        const strtab_offset = offset;
        offset = strtab_offset + string_bytes;

        const shstrtab_offset = offset;
        offset = shstrtab_offset + self.shstrtab.items.len;

        const debug_line_offset = offset;
        offset = debug_line_offset + self.debug.line.len;
        const debug_abbrev_offset = offset;
        offset = debug_abbrev_offset + self.debug.abbrev.len;
        const debug_info_offset = offset;
        offset = debug_info_offset + self.debug.info.len;
        const rela_debug_line_offset = alignUp(offset, 8);
        offset = rela_debug_line_offset + rela_debug_line_size;
        const rela_debug_info_offset = alignUp(offset, 8);
        offset = rela_debug_info_offset + rela_debug_info_size;

        const shdr_offset = alignUp(offset, 8);

        std.debug.assert(output.items.len == 0);
        const object_size: usize = @intCast(shdr_offset + NUM_SECTIONS * @sizeOf(Elf64_Shdr));
        try output.ensureTotalCapacityPrecise(self.allocator, object_size);

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
        ehdr.e_ident[7] = @intFromEnum(self.osabi);
        @memset(ehdr.e_ident[8..16], 0);

        output.appendSliceAssumeCapacity(std.mem.asBytes(&ehdr));

        // Pad to text section
        padTo(output, text_offset);
        output.appendSliceAssumeCapacity(self.text);

        padTo(output, rodata_offset);
        output.appendSliceAssumeCapacity(self.rodata);

        // Pad to rela sections
        padTo(output, rela_text_offset);
        for (self.text_relocs.items) |rel| {
            const r_info: u64 = (@as(u64, rel.symbol_idx + WRITER_SYMBOL_OFFSET) << 32) | rel.reloc_type;

            const elf_rela = Elf64_Rela{
                .r_offset = rel.offset,
                .r_info = r_info,
                .r_addend = rel.addend,
            };

            output.appendSliceAssumeCapacity(std.mem.asBytes(&elf_rela));
        }

        padTo(output, rela_rodata_offset);
        for (self.rodata_relocs.items) |rel| {
            const r_info: u64 = (@as(u64, rel.symbol_idx + WRITER_SYMBOL_OFFSET) << 32) | rel.reloc_type;

            const elf_rela = Elf64_Rela{
                .r_offset = rel.offset,
                .r_info = r_info,
                .r_addend = rel.addend,
            };

            output.appendSliceAssumeCapacity(std.mem.asBytes(&elf_rela));
        }

        // Pad to symtab
        padTo(output, symtab_offset);
        // First symbol is always null
        output.appendSliceAssumeCapacity(&std.mem.zeroes([24]u8));

        // Section symbols used by DWARF cross-section relocations.
        for (debug_target_sections) |section_index| {
            const section_sym = Elf64_Sym{
                .st_name = 0,
                .st_info = (ELF.STB_LOCAL << 4) | ELF.STT_SECTION,
                .st_other = 0,
                .st_shndx = section_index,
                .st_value = 0,
                .st_size = 0,
            };
            output.appendSliceAssumeCapacity(std.mem.asBytes(&section_sym));
        }

        // Count local symbols (for sh_info)

        // Add symbols
        var name_offset: u32 = 1;
        for (self.symbols.items) |sym| {
            const st_info: u8 = blk: {
                const bind: u8 = if (sym.is_global) ELF.STB_GLOBAL else ELF.STB_LOCAL;
                const sym_type: u8 = if (sym.is_function) ELF.STT_FUNC else if (sym.section == .rodata or sym.section == .bss) ELF.STT_OBJECT else ELF.STT_NOTYPE;
                break :blk (bind << 4) | sym_type;
            };

            const st_shndx: u16 = switch (sym.section) {
                .text => SHIDX_TEXT,
                .data => 0, // Would be data section index
                .rodata => SHIDX_RODATA,
                .bss => SHIDX_BSS,
                .undef => ELF.SHN_UNDEF,
            };

            const elf_sym = Elf64_Sym{
                .st_name = name_offset,
                .st_info = st_info,
                .st_other = if (sym.is_hidden) ELF.STV_HIDDEN else ELF.STV_DEFAULT,
                .st_shndx = st_shndx,
                .st_value = sym.offset,
                .st_size = sym.size,
            };

            output.appendSliceAssumeCapacity(std.mem.asBytes(&elf_sym));
            name_offset += @intCast(sym.name.len + 1);

            if (!sym.is_global) {
                num_locals += 1;
            }
        }

        // strtab (no padding needed)
        output.appendAssumeCapacity(0);
        for (self.symbols.items) |sym| {
            output.appendSliceAssumeCapacity(sym.name);
            output.appendAssumeCapacity(0);
        }

        // shstrtab
        output.appendSliceAssumeCapacity(self.shstrtab.items);

        // Debug sections
        output.appendSliceAssumeCapacity(self.debug.line);
        output.appendSliceAssumeCapacity(self.debug.abbrev);
        output.appendSliceAssumeCapacity(self.debug.info);
        padTo(output, rela_debug_line_offset);
        appendDebugRelocations(self.arch, self.debug.line_relocs, output);
        padTo(output, rela_debug_info_offset);
        appendDebugRelocations(self.arch, self.debug.info_relocs, output);

        // Pad to section headers
        padTo(output, shdr_offset);

        // Write section headers
        // 0: NULL section
        output.appendSliceAssumeCapacity(&std.mem.zeroes([64]u8));

        // 1: .text
        appendShdr(output, .{ .sh_name = shname_text, .sh_type = ELF.SHT_PROGBITS, .sh_flags = ELF.SHF_ALLOC | ELF.SHF_EXECINSTR, .sh_offset = text_offset, .sh_size = self.text.len, .sh_addralign = 16 });

        // 2: .rodata
        appendShdr(output, .{ .sh_name = shname_rodata, .sh_type = ELF.SHT_PROGBITS, .sh_flags = ELF.SHF_ALLOC, .sh_offset = rodata_offset, .sh_size = self.rodata.len, .sh_addralign = 16 });

        // 3: .rela.text
        appendShdr(output, .{ .sh_name = shname_rela_text, .sh_type = ELF.SHT_RELA, .sh_flags = ELF.SHF_INFO_LINK, .sh_offset = rela_text_offset, .sh_size = rela_text_size, .sh_link = SHIDX_SYMTAB, .sh_info = SHIDX_TEXT, .sh_addralign = 8, .sh_entsize = @sizeOf(Elf64_Rela) });

        // 4: .rela.rodata
        appendShdr(output, .{ .sh_name = shname_rela_rodata, .sh_type = ELF.SHT_RELA, .sh_flags = ELF.SHF_INFO_LINK, .sh_offset = rela_rodata_offset, .sh_size = rela_rodata_size, .sh_link = SHIDX_SYMTAB, .sh_info = SHIDX_RODATA, .sh_addralign = 8, .sh_entsize = @sizeOf(Elf64_Rela) });

        // 5: .symtab
        appendShdr(output, .{ .sh_name = shname_symtab, .sh_type = ELF.SHT_SYMTAB, .sh_offset = symtab_offset, .sh_size = symtab_size, .sh_link = SHIDX_STRTAB, .sh_info = num_locals, .sh_addralign = 8, .sh_entsize = @sizeOf(Elf64_Sym) });

        // 6: .strtab
        appendShdr(output, .{ .sh_name = shname_strtab, .sh_type = ELF.SHT_STRTAB, .sh_offset = strtab_offset, .sh_size = string_bytes, .sh_addralign = 1 });

        // 7: .shstrtab
        appendShdr(output, .{ .sh_name = shname_shstrtab, .sh_type = ELF.SHT_STRTAB, .sh_offset = shstrtab_offset, .sh_size = self.shstrtab.items.len, .sh_addralign = 1 });

        // 8: .debug_line
        appendShdr(output, .{ .sh_name = shname_debug_line, .sh_type = ELF.SHT_PROGBITS, .sh_offset = debug_line_offset, .sh_size = self.debug.line.len, .sh_addralign = 1 });

        // 9: .debug_abbrev
        appendShdr(output, .{ .sh_name = shname_debug_abbrev, .sh_type = ELF.SHT_PROGBITS, .sh_offset = debug_abbrev_offset, .sh_size = self.debug.abbrev.len, .sh_addralign = 1 });

        // 10: .debug_info
        appendShdr(output, .{ .sh_name = shname_debug_info, .sh_type = ELF.SHT_PROGBITS, .sh_offset = debug_info_offset, .sh_size = self.debug.info.len, .sh_addralign = 1 });

        // 11: .rela.debug_line
        appendShdr(output, .{ .sh_name = shname_rela_debug_line, .sh_type = ELF.SHT_RELA, .sh_flags = ELF.SHF_INFO_LINK, .sh_offset = rela_debug_line_offset, .sh_size = rela_debug_line_size, .sh_link = SHIDX_SYMTAB, .sh_info = SHIDX_DEBUG_LINE, .sh_addralign = 8, .sh_entsize = @sizeOf(Elf64_Rela) });

        // 12: .rela.debug_info
        appendShdr(output, .{ .sh_name = shname_rela_debug_info, .sh_type = ELF.SHT_RELA, .sh_flags = ELF.SHF_INFO_LINK, .sh_offset = rela_debug_info_offset, .sh_size = rela_debug_info_size, .sh_link = SHIDX_SYMTAB, .sh_info = SHIDX_DEBUG_INFO, .sh_addralign = 8, .sh_entsize = @sizeOf(Elf64_Rela) });

        // 13: .bss, declared by size alone; SHT_NOBITS stores no bytes, so
        // its file offset only has to be inside the file.
        appendShdr(output, .{ .sh_name = shname_bss, .sh_type = ELF.SHT_NOBITS, .sh_flags = ELF.SHF_ALLOC | ELF.SHF_WRITE, .sh_offset = shdr_offset, .sh_size = self.zero_fill_size, .sh_addralign = 16 });
        std.debug.assert(output.items.len == object_size);
    }

    fn appendShdr(output: *std.ArrayList(u8), shdr: Elf64_Shdr) void {
        output.appendSliceAssumeCapacity(std.mem.asBytes(&shdr));
    }

    fn padTo(output: *std.ArrayList(u8), target: u64) void {
        const current: u64 = @intCast(output.items.len);
        if (current < target) {
            const padding: usize = @intCast(target - current);
            output.appendNTimesAssumeCapacity(0, padding);
        }
    }
};

fn appendDebugRelocations(
    arch: Architecture,
    relocs: []const DebugReloc,
    output: *std.ArrayList(u8),
) void {
    for (relocs) |rel| {
        const target_symbol_index: u64 = switch (rel.target) {
            .text => 1,
            .debug_line => 2,
            .debug_abbrev => 3,
        };
        const reloc_type: u32 = switch (arch) {
            .x86_64 => switch (rel.width) {
                .four => ELF.R_X86_64_32,
                .eight => ELF.R_X86_64_64,
            },
            .aarch64 => switch (rel.width) {
                .four => ELF.R_AARCH64_ABS32,
                .eight => ELF.R_AARCH64_ABS64,
            },
        };
        const elf_rela = Elf64_Rela{
            .r_offset = rel.section_offset,
            .r_info = (target_symbol_index << 32) | reloc_type,
            .r_addend = @intCast(rel.addend),
        };
        output.appendSliceAssumeCapacity(std.mem.asBytes(&elf_rela));
    }
}

fn alignUp(value: u64, alignment: u64) u64 {
    return (value + alignment - 1) & ~(alignment - 1);
}

// Tests

test "create minimal elf object" {
    var writer = try ElfWriter.init(std.testing.allocator, .x86_64, .none);
    defer writer.deinit();

    // Add some test code (ret instruction)
    writer.setCode(&[_]u8{0xC3});

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
    var writer = try ElfWriter.init(std.testing.allocator, .x86_64, .none);
    defer writer.deinit();

    // Simple code: call to external function (placeholder)
    writer.setCode(&[_]u8{ 0xE8, 0x00, 0x00, 0x00, 0x00, 0xC3 });

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

test "DWARF relocations preserve target section and field width" {
    const relocs = [_]DebugReloc{
        .{
            .section_offset = 6,
            .target = .debug_abbrev,
            .width = .four,
            .addend = 0,
        },
        .{
            .section_offset = 24,
            .target = .text,
            .width = .eight,
            .addend = 32,
        },
        .{
            .section_offset = 40,
            .target = .debug_line,
            .width = .four,
            .addend = 0,
        },
    };

    var output: std.ArrayList(u8) = .empty;
    defer output.deinit(std.testing.allocator);
    try output.ensureTotalCapacityPrecise(std.testing.allocator, relocs.len * @sizeOf(Elf64_Rela));
    appendDebugRelocations(.x86_64, &relocs, &output);

    try std.testing.expectEqual(@as(usize, 3 * @sizeOf(Elf64_Rela)), output.items.len);
    const abbrev = std.mem.bytesToValue(Elf64_Rela, output.items[0..@sizeOf(Elf64_Rela)]);
    const text = std.mem.bytesToValue(Elf64_Rela, output.items[@sizeOf(Elf64_Rela)..][0..@sizeOf(Elf64_Rela)]);
    const line = std.mem.bytesToValue(Elf64_Rela, output.items[2 * @sizeOf(Elf64_Rela) ..][0..@sizeOf(Elf64_Rela)]);

    try std.testing.expectEqual(@as(u64, 6), abbrev.r_offset);
    try std.testing.expectEqual((@as(u64, 3) << 32) | ELF.R_X86_64_32, abbrev.r_info);
    try std.testing.expectEqual(@as(u64, 24), text.r_offset);
    try std.testing.expectEqual((@as(u64, 1) << 32) | ELF.R_X86_64_64, text.r_info);
    try std.testing.expectEqual(@as(i64, 32), text.r_addend);
    try std.testing.expectEqual(@as(u64, 40), line.r_offset);
    try std.testing.expectEqual((@as(u64, 2) << 32) | ELF.R_X86_64_32, line.r_info);
}
