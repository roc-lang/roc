//! COFF object file writer for the dev backend.
//!
//! This module writes COFF (Common Object File Format) object files for Windows
//! from generated machine code and relocations. It produces relocatable object
//! files (.obj) that can be linked with other objects to create executables.
//!
//! Reference: https://docs.microsoft.com/en-us/windows/win32/debug/pe-format

const std = @import("std");
const Allocator = std.mem.Allocator;

/// COFF file format constants
const COFF = struct {
    // Machine types
    const IMAGE_FILE_MACHINE_AMD64 = 0x8664;
    const IMAGE_FILE_MACHINE_ARM64 = 0xAA64;

    // Section flags
    const IMAGE_SCN_CNT_CODE = 0x00000020;
    const IMAGE_SCN_MEM_EXECUTE = 0x20000000;
    const IMAGE_SCN_MEM_READ = 0x40000000;
    const IMAGE_SCN_ALIGN_16BYTES = 0x00500000;

    // Symbol storage class
    const IMAGE_SYM_CLASS_EXTERNAL = 2;
    const IMAGE_SYM_CLASS_STATIC = 3;

    // Symbol type
    const IMAGE_SYM_TYPE_NULL = 0;
    const IMAGE_SYM_DTYPE_FUNCTION = 0x20;

    // Special section numbers
    const IMAGE_SYM_UNDEFINED = 0;

    // x86_64 relocation types
    const IMAGE_REL_AMD64_REL32 = 0x0004;

    // ARM64 relocation types
    const IMAGE_REL_ARM64_BRANCH26 = 0x0003;
};

/// COFF File Header (20 bytes)
const CoffHeader = extern struct {
    machine: u16,
    number_of_sections: u16,
    time_date_stamp: u32,
    pointer_to_symbol_table: u32,
    number_of_symbols: u32,
    size_of_optional_header: u16,
    characteristics: u16,
};

/// COFF Section Header (40 bytes)
const SectionHeader = extern struct {
    name: [8]u8,
    virtual_size: u32,
    virtual_address: u32,
    size_of_raw_data: u32,
    pointer_to_raw_data: u32,
    pointer_to_relocations: u32,
    pointer_to_line_numbers: u32,
    number_of_relocations: u16,
    number_of_line_numbers: u16,
    characteristics: u32,
};

/// COFF Relocation (10 bytes)
const CoffRelocation = extern struct {
    virtual_address: u32,
    symbol_table_index: u32,
    type: u16,
};

/// COFF Symbol Table Entry (18 bytes)
const CoffSymbol = extern struct {
    name: extern union {
        short_name: [8]u8,
        long_name: extern struct {
            zeroes: u32,
            offset: u32,
        },
    },
    value: u32,
    section_number: i16,
    type: u16,
    storage_class: u8,
    number_of_aux_symbols: u8,
};

/// Target architecture for COFF generation
pub const Architecture = enum {
    x86_64,
    aarch64,

    fn machine(self: Architecture) u16 {
        return switch (self) {
            .x86_64 => COFF.IMAGE_FILE_MACHINE_AMD64,
            .aarch64 => COFF.IMAGE_FILE_MACHINE_ARM64,
        };
    }

    fn branchRelocType(self: Architecture) u16 {
        return switch (self) {
            .x86_64 => COFF.IMAGE_REL_AMD64_REL32,
            .aarch64 => COFF.IMAGE_REL_ARM64_BRANCH26,
        };
    }
};

/// Symbol definition for the object file
pub const Symbol = struct {
    name: []const u8,
    section: Section,
    offset: u32,
    is_global: bool,
    is_function: bool,
};

/// Section types
pub const Section = enum {
    text,
    data,
    rdata,
    bss,
    undef, // External symbol
};

/// COFF object file writer
pub const CoffWriter = struct {
    const Self = @This();

    allocator: Allocator,
    arch: Architecture,

    // Section contents
    text: std.ArrayList(u8),
    data: std.ArrayList(u8),
    rdata: std.ArrayList(u8),

    // Symbol table
    symbols: std.ArrayList(Symbol),

    // Relocations for .text section
    text_relocs: std.ArrayList(TextReloc),

    // String table (for long symbol names)
    strtab: std.ArrayList(u8),

    const TextReloc = struct {
        offset: u32, // Offset in .text where relocation applies
        symbol_idx: u32, // Index into symbol table
        reloc_type: u16, // Relocation type
    };

    pub fn init(allocator: Allocator, arch: Architecture) !Self {
        var self = Self{
            .allocator = allocator,
            .arch = arch,
            .text = .{},
            .data = .{},
            .rdata = .{},
            .symbols = .{},
            .text_relocs = .{},
            .strtab = .{},
        };

        // String table starts with 4-byte size (will be filled in later)
        // We use a placeholder for now
        try self.strtab.appendSlice(allocator, &[_]u8{ 0, 0, 0, 0 });

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.text.deinit(self.allocator);
        self.data.deinit(self.allocator);
        self.rdata.deinit(self.allocator);
        self.symbols.deinit(self.allocator);
        self.text_relocs.deinit(self.allocator);
        self.strtab.deinit(self.allocator);
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
            .is_global = true,
            .is_function = true,
        });
    }

    /// Add a relocation to the text section
    pub fn addTextRelocation(self: *Self, offset: u32, symbol_idx: u32) !void {
        try self.text_relocs.append(self.allocator, .{
            .offset = offset,
            .symbol_idx = symbol_idx,
            .reloc_type = self.arch.branchRelocType(),
        });
    }

    /// Add a string to the string table, return its offset
    /// COFF string table offsets start at 4 (after the size field)
    fn addString(self: *Self, str: []const u8) !u32 {
        const offset: u32 = @intCast(self.strtab.items.len);
        try self.strtab.appendSlice(self.allocator, str);
        try self.strtab.append(self.allocator, 0); // Null terminator
        return offset;
    }

    /// Write the COFF object file to a buffer
    pub fn write(self: *Self, output: *std.ArrayList(u8)) !void {
        // Section indices (1-based in COFF)
        const SECT_TEXT: i16 = 1;

        // Calculate layout
        const header_size: u32 = @sizeOf(CoffHeader);
        const section_header_size: u32 = @sizeOf(SectionHeader);
        const num_sections: u16 = 1; // Just .text for now

        // Calculate offsets
        const section_headers_offset: u32 = header_size;
        const text_offset: u32 = section_headers_offset + section_header_size * num_sections;
        const text_size: u32 = @intCast(self.text.items.len);

        // Align text section to 16 bytes and add relocations after
        const text_end: u32 = text_offset + text_size;
        const reloc_offset: u32 = text_end;
        const reloc_size: u32 = @intCast(self.text_relocs.items.len * @sizeOf(CoffRelocation));

        // Symbol table comes after relocations
        const symtab_offset: u32 = reloc_offset + reloc_size;
        const num_symbols: u32 = @intCast(self.symbols.items.len);

        // Build symbol table entries and string table
        var symtab: std.ArrayList(u8) = .{};
        defer symtab.deinit(self.allocator);

        for (self.symbols.items) |sym| {
            var coff_sym = CoffSymbol{
                .name = undefined,
                .value = sym.offset,
                .section_number = switch (sym.section) {
                    .text => SECT_TEXT,
                    .data => 0, // Would be section 2
                    .rdata => 0,
                    .bss => 0,
                    .undef => COFF.IMAGE_SYM_UNDEFINED,
                },
                .type = if (sym.is_function) COFF.IMAGE_SYM_DTYPE_FUNCTION else COFF.IMAGE_SYM_TYPE_NULL,
                .storage_class = if (sym.is_global) COFF.IMAGE_SYM_CLASS_EXTERNAL else COFF.IMAGE_SYM_CLASS_STATIC,
                .number_of_aux_symbols = 0,
            };

            // Handle symbol name (short names <= 8 bytes go directly, longer ones go to string table)
            if (sym.name.len <= 8) {
                @memset(&coff_sym.name.short_name, 0);
                @memcpy(coff_sym.name.short_name[0..sym.name.len], sym.name);
            } else {
                const str_offset = try self.addString(sym.name);
                coff_sym.name.long_name.zeroes = 0;
                coff_sym.name.long_name.offset = str_offset;
            }

            try symtab.appendSlice(self.allocator, std.mem.asBytes(&coff_sym));
        }

        // Update string table size (first 4 bytes)
        const strtab_size: u32 = @intCast(self.strtab.items.len);
        @memcpy(self.strtab.items[0..4], std.mem.asBytes(&strtab_size));

        // Write COFF header
        const header = CoffHeader{
            .machine = self.arch.machine(),
            .number_of_sections = num_sections,
            .time_date_stamp = 0, // Can be set to actual timestamp if needed
            .pointer_to_symbol_table = symtab_offset,
            .number_of_symbols = num_symbols,
            .size_of_optional_header = 0, // No optional header for .obj files
            .characteristics = 0,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&header));

        // Write .text section header
        var sect_name: [8]u8 = std.mem.zeroes([8]u8);
        @memcpy(sect_name[0..5], ".text");

        const text_header = SectionHeader{
            .name = sect_name,
            .virtual_size = 0, // Not used in object files
            .virtual_address = 0,
            .size_of_raw_data = text_size,
            .pointer_to_raw_data = if (text_size > 0) text_offset else 0,
            .pointer_to_relocations = if (self.text_relocs.items.len > 0) reloc_offset else 0,
            .pointer_to_line_numbers = 0,
            .number_of_relocations = @intCast(self.text_relocs.items.len),
            .number_of_line_numbers = 0,
            .characteristics = COFF.IMAGE_SCN_CNT_CODE |
                COFF.IMAGE_SCN_MEM_EXECUTE |
                COFF.IMAGE_SCN_MEM_READ |
                COFF.IMAGE_SCN_ALIGN_16BYTES,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&text_header));

        // Write .text section content
        try output.appendSlice(self.allocator, self.text.items);

        // Write relocations
        for (self.text_relocs.items) |rel| {
            const coff_reloc = CoffRelocation{
                .virtual_address = rel.offset,
                .symbol_table_index = rel.symbol_idx,
                .type = rel.reloc_type,
            };
            try output.appendSlice(self.allocator, std.mem.asBytes(&coff_reloc));
        }

        // Write symbol table
        try output.appendSlice(self.allocator, symtab.items);

        // Write string table
        try output.appendSlice(self.allocator, self.strtab.items);
    }
};

// Tests

test "create minimal coff object" {
    var writer = try CoffWriter.init(std.testing.allocator, .x86_64);
    defer writer.deinit();

    // Add test code (ret instruction)
    try writer.setCode(&[_]u8{0xC3});

    // Add a symbol for the function
    _ = try writer.addSymbol(.{
        .name = "test_func",
        .section = .text,
        .offset = 0,
        .is_global = true,
        .is_function = true,
    });

    var output: std.ArrayList(u8) = .{};
    defer output.deinit(std.testing.allocator);

    try writer.write(&output);

    // Check machine type (x86_64 = 0x8664)
    const machine = std.mem.readInt(u16, output.items[0..2], .little);
    try std.testing.expectEqual(COFF.IMAGE_FILE_MACHINE_AMD64, machine);

    // Check number of sections
    const num_sections = std.mem.readInt(u16, output.items[2..4], .little);
    try std.testing.expectEqual(@as(u16, 1), num_sections);
}

test "coff with external symbol" {
    var writer = try CoffWriter.init(std.testing.allocator, .x86_64);
    defer writer.deinit();

    // Simple code: call to external function (placeholder)
    try writer.setCode(&[_]u8{ 0xE8, 0x00, 0x00, 0x00, 0x00, 0xC3 });

    // Add external symbol
    const ext_idx = try writer.addExternalSymbol("external_func");

    // Add relocation for the call
    try writer.addTextRelocation(1, ext_idx);

    var output: std.ArrayList(u8) = .{};
    defer output.deinit(std.testing.allocator);

    try writer.write(&output);

    // Should produce valid COFF
    const machine = std.mem.readInt(u16, output.items[0..2], .little);
    try std.testing.expectEqual(COFF.IMAGE_FILE_MACHINE_AMD64, machine);
}

test "coff with long symbol name" {
    var writer = try CoffWriter.init(std.testing.allocator, .x86_64);
    defer writer.deinit();

    try writer.setCode(&[_]u8{0xC3});

    // Add a symbol with a name longer than 8 characters
    _ = try writer.addSymbol(.{
        .name = "this_is_a_very_long_symbol_name",
        .section = .text,
        .offset = 0,
        .is_global = true,
        .is_function = true,
    });

    var output: std.ArrayList(u8) = .{};
    defer output.deinit(std.testing.allocator);

    try writer.write(&output);

    // Should produce valid COFF
    const machine = std.mem.readInt(u16, output.items[0..2], .little);
    try std.testing.expectEqual(COFF.IMAGE_FILE_MACHINE_AMD64, machine);
}

test "coff aarch64" {
    var writer = try CoffWriter.init(std.testing.allocator, .aarch64);
    defer writer.deinit();

    // ARM64 ret instruction
    try writer.setCode(&[_]u8{ 0xC0, 0x03, 0x5F, 0xD6 });

    _ = try writer.addSymbol(.{
        .name = "test_func",
        .section = .text,
        .offset = 0,
        .is_global = true,
        .is_function = true,
    });

    var output: std.ArrayList(u8) = .{};
    defer output.deinit(std.testing.allocator);

    try writer.write(&output);

    // Check machine type (ARM64 = 0xAA64)
    const machine = std.mem.readInt(u16, output.items[0..2], .little);
    try std.testing.expectEqual(COFF.IMAGE_FILE_MACHINE_ARM64, machine);
}
