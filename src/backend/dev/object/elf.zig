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
const Relocation = @import("../Relocation.zig").Relocation;

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
    const SHT_NULL = 0;
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
    const STT_FUNC = 2;
    const STT_SECTION = 3;

    // Special section indices
    const SHN_UNDEF = 0;
    const SHN_ABS = 0xfff1;

    // x86_64 relocation types
    const R_X86_64_PC32 = 2;
    const R_X86_64_PLT32 = 4;
    const R_X86_64_GOTPCREL = 9;

    // aarch64 relocation types
    const R_AARCH64_CALL26 = 283;
    const R_AARCH64_JUMP26 = 282;
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

    // String tables
    strtab: std.ArrayList(u8),
    shstrtab: std.ArrayList(u8),

    const TextReloc = struct {
        offset: u64, // Offset in .text where relocation applies
        symbol_idx: u32, // Index into symbol table
        reloc_type: u32, // Architecture-specific relocation type
        addend: i64,
    };

    pub fn init(allocator: Allocator, arch: Architecture) !Self {
        var self = Self{
            .allocator = allocator,
            .arch = arch,
            .text = .{},
            .data = .{},
            .rodata = .{},
            .symbols = .{},
            .text_relocs = .{},
            .strtab = .{},
            .shstrtab = .{},
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
        self.strtab.deinit(self.allocator);
        self.shstrtab.deinit(self.allocator);
    }

    /// Set the code section contents
    pub fn setCode(self: *Self, code: []const u8) !void {
        self.text.clearRetainingCapacity();
        try self.text.appendSlice(self.allocator, code);
    }

    /// Add a symbol to the object file
    pub fn addSymbol(self: *Self, symbol: Symbol) !u32 {
        const idx: u32 = @intCast(self.symbols.items.len);
        try self.symbols.append(self.allocator, symbol);
        return idx;
    }

    /// Add an external symbol reference
    pub fn addExternalSymbol(self: *Self, name: []const u8) !u32 {
        return self.addSymbol(.{
            .name = name,
            .section = .undef,
            .offset = 0,
            .size = 0,
            .is_global = true,
            .is_function = true,
        });
    }

    /// Add a relocation to the text section
    pub fn addTextRelocation(self: *Self, offset: u64, symbol_idx: u32, addend: i64) !void {
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

    /// Add a string to the string table, return its offset
    fn addString(self: *Self, table: *std.ArrayList(u8), str: []const u8) !u32 {
        const offset: u32 = @intCast(table.items.len);
        try table.appendSlice(self.allocator, str);
        try table.append(self.allocator, 0); // Null terminator
        return offset;
    }

    /// Write the ELF object file to a buffer
    pub fn write(self: *Self, output: *std.ArrayList(u8)) !void {
        // Section indices
        const SHIDX_TEXT = 1;
        const SHIDX_SYMTAB = 3;
        const SHIDX_STRTAB = 4;
        const SHIDX_SHSTRTAB = 5;
        const NUM_SECTIONS = 6;

        // Add section names to shstrtab
        const shname_text = try self.addString(&self.shstrtab, ".text");
        const shname_rela_text = try self.addString(&self.shstrtab, ".rela.text");
        const shname_symtab = try self.addString(&self.shstrtab, ".symtab");
        const shname_strtab = try self.addString(&self.shstrtab, ".strtab");
        const shname_shstrtab = try self.addString(&self.shstrtab, ".shstrtab");

        // Build symbol table
        var symtab: std.ArrayList(u8) = .{};
        defer symtab.deinit(self.allocator);

        // First symbol is always null
        try symtab.appendSlice(self.allocator, &std.mem.zeroes([24]u8));

        // Count local symbols (for sh_info)
        var num_locals: u32 = 1; // Start at 1 for null symbol

        // Add symbols
        for (self.symbols.items) |sym| {
            const name_offset = try self.addString(&self.strtab, sym.name);

            const st_info: u8 = blk: {
                const bind: u8 = if (sym.is_global) ELF.STB_GLOBAL else ELF.STB_LOCAL;
                const sym_type: u8 = if (sym.is_function) ELF.STT_FUNC else ELF.STT_NOTYPE;
                break :blk (bind << 4) | sym_type;
            };

            const st_shndx: u16 = switch (sym.section) {
                .text => SHIDX_TEXT,
                .data => 0, // Would be data section index
                .rodata => 0,
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

        // Build relocation table
        var rela: std.ArrayList(u8) = .{};
        defer rela.deinit(self.allocator);

        for (self.text_relocs.items) |rel| {
            // Symbol index is +1 because of null symbol at index 0
            const r_info: u64 = (@as(u64, rel.symbol_idx + 1) << 32) | rel.reloc_type;

            const elf_rela = Elf64_Rela{
                .r_offset = rel.offset,
                .r_info = r_info,
                .r_addend = rel.addend,
            };

            try rela.appendSlice(self.allocator, std.mem.asBytes(&elf_rela));
        }

        // Calculate offsets
        const ehdr_size: u64 = @sizeOf(Elf64_Ehdr);

        // Section data starts after headers
        var offset: u64 = ehdr_size;

        // Align sections
        const text_offset = alignUp(offset, 16);
        offset = text_offset + self.text.items.len;

        const rela_offset = alignUp(offset, 8);
        offset = rela_offset + rela.items.len;

        const symtab_offset = alignUp(offset, 8);
        offset = symtab_offset + symtab.items.len;

        const strtab_offset = offset;
        offset = strtab_offset + self.strtab.items.len;

        const shstrtab_offset = offset;
        offset = shstrtab_offset + self.shstrtab.items.len;

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

        // Pad to rela section
        try self.padTo(output, rela_offset);
        try output.appendSlice(self.allocator, rela.items);

        // Pad to symtab
        try self.padTo(output, symtab_offset);
        try output.appendSlice(self.allocator, symtab.items);

        // strtab (no padding needed)
        try output.appendSlice(self.allocator, self.strtab.items);

        // shstrtab
        try output.appendSlice(self.allocator, self.shstrtab.items);

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

        // 2: .rela.text
        const shdr_rela = Elf64_Shdr{
            .sh_name = shname_rela_text,
            .sh_type = ELF.SHT_RELA,
            .sh_flags = ELF.SHF_INFO_LINK,
            .sh_addr = 0,
            .sh_offset = rela_offset,
            .sh_size = rela.items.len,
            .sh_link = SHIDX_SYMTAB, // Associated symbol table
            .sh_info = SHIDX_TEXT, // Section to which relocs apply
            .sh_addralign = 8,
            .sh_entsize = @sizeOf(Elf64_Rela),
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&shdr_rela));

        // 3: .symtab
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

        // 4: .strtab
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

        // 5: .shstrtab
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
    }

    fn padTo(self: *Self, output: *std.ArrayList(u8), target: u64) !void {
        const current = output.items.len;
        if (current < target) {
            const padding = target - current;
            try output.appendNTimes(self.allocator, 0, padding);
        }
    }
};

fn alignUp(value: u64, alignment: u64) u64 {
    return (value + alignment - 1) & ~(alignment - 1);
}

// ============================================================================
// Tests
// ============================================================================

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

    var output: std.ArrayList(u8) = .{};
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

    var output: std.ArrayList(u8) = .{};
    defer output.deinit(std.testing.allocator);

    try writer.write(&output);

    // Should produce valid ELF
    try std.testing.expectEqualSlices(u8, "\x7fELF", output.items[0..4]);
}
