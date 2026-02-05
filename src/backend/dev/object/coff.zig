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
    const IMAGE_SCN_CNT_INITIALIZED_DATA = 0x00000040;
    const IMAGE_SCN_MEM_EXECUTE = 0x20000000;
    const IMAGE_SCN_MEM_READ = 0x40000000;
    const IMAGE_SCN_ALIGN_4BYTES = 0x00300000;
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
    const IMAGE_REL_AMD64_ADDR32NB = 0x0003; // 32-bit address w/o base (RVA)

    // ARM64 relocation types
    const IMAGE_REL_ARM64_BRANCH26 = 0x0003;

    // x64 Unwind operation codes
    const UWOP_PUSH_NONVOL = 0; // Push a nonvolatile register
    const UWOP_ALLOC_LARGE = 1; // Allocate large stack area
    const UWOP_ALLOC_SMALL = 2; // Allocate small stack area (8-128 bytes)
    const UWOP_SET_FPREG = 3; // Set frame pointer register

    // x64 register numbers for unwind info
    const UNWIND_REG_RBP = 5;
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
/// Note: We cannot use this struct directly with asBytes due to padding.
/// Use writeSymbol() to write exactly 18 bytes.
const CoffSymbol = struct {
    name_bytes: [8]u8, // Either short name or (zeroes:u32, offset:u32)
    value: u32,
    section_number: i16,
    type: u16,
    storage_class: u8,
    number_of_aux_symbols: u8,

    fn setShortName(self: *CoffSymbol, name: []const u8) void {
        @memset(&self.name_bytes, 0);
        @memcpy(self.name_bytes[0..name.len], name);
    }

    fn setLongName(self: *CoffSymbol, str_offset: u32) void {
        // First 4 bytes are zeroes (indicates long name)
        std.mem.writeInt(u32, self.name_bytes[0..4], 0, .little);
        // Next 4 bytes are offset into string table
        std.mem.writeInt(u32, self.name_bytes[4..8], str_offset, .little);
    }

    fn writeToBuffer(self: *const CoffSymbol, buffer: *std.ArrayList(u8), allocator: std.mem.Allocator) !void {
        // Write exactly 18 bytes
        try buffer.appendSlice(allocator, &self.name_bytes); // 8 bytes
        var value_bytes: [4]u8 = undefined;
        std.mem.writeInt(u32, &value_bytes, self.value, .little);
        try buffer.appendSlice(allocator, &value_bytes); // 4 bytes
        var section_bytes: [2]u8 = undefined;
        std.mem.writeInt(i16, &section_bytes, self.section_number, .little);
        try buffer.appendSlice(allocator, &section_bytes); // 2 bytes
        var type_bytes: [2]u8 = undefined;
        std.mem.writeInt(u16, &type_bytes, self.type, .little);
        try buffer.appendSlice(allocator, &type_bytes); // 2 bytes
        try buffer.append(allocator, self.storage_class); // 1 byte
        try buffer.append(allocator, self.number_of_aux_symbols); // 1 byte
        // Total: 18 bytes
    }
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

/// Function info for generating unwind data (.pdata/.xdata on Windows x64)
/// This is required for proper exception handling and debugger stack walking.
pub const FunctionInfo = struct {
    start_offset: u32, // Offset of function start in .text
    end_offset: u32, // Offset of function end in .text (one past last byte)
    prologue_size: u8, // Size of function prologue in bytes
    frame_reg_offset: u8, // Offset of frame register from RSP (scaled by 16)
    uses_frame_pointer: bool, // Whether function uses RBP as frame pointer
    stack_alloc: u32, // Stack allocation size (for UWOP_ALLOC_*)
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

    // Function info for unwind data (Windows x64 only)
    functions: std.ArrayList(FunctionInfo),

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
            .functions = .{},
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
        self.functions.deinit(self.allocator);
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

    /// Add function info for unwind data generation (Windows x64)
    /// This must be called for each function to enable proper exception handling.
    pub fn addFunctionInfo(self: *Self, info: FunctionInfo) !void {
        try self.functions.append(self.allocator, info);
    }

    /// Add a string to the string table, return its offset
    /// COFF string table offsets start at 4 (after the size field)
    fn addString(self: *Self, str: []const u8) !u32 {
        const offset: u32 = @intCast(self.strtab.items.len);
        try self.strtab.appendSlice(self.allocator, str);
        try self.strtab.append(self.allocator, 0); // Null terminator
        return offset;
    }

    /// Write a COFF relocation entry (10 bytes) manually to avoid struct padding issues
    fn writeRelocation(self: *Self, output: *std.ArrayList(u8), virtual_address: u32, symbol_idx: u32, reloc_type: u16) !void {
        var buf: [10]u8 = undefined;
        std.mem.writeInt(u32, buf[0..4], virtual_address, .little);
        std.mem.writeInt(u32, buf[4..8], symbol_idx, .little);
        std.mem.writeInt(u16, buf[8..10], reloc_type, .little);
        try output.appendSlice(self.allocator, &buf);
    }

    /// Write the COFF object file to a buffer
    pub fn write(self: *Self, output: *std.ArrayList(u8)) !void {
        // Section indices (1-based in COFF)
        const SECT_TEXT: i16 = 1;

        // Check if we need unwind sections (Windows x64 only with functions defined)
        const need_unwind = self.arch == .x86_64 and self.functions.items.len > 0;
        // Section indices: 1=.text, 2=.pdata, 3=.xdata
        const SECT_XDATA: i16 = if (need_unwind) 3 else 0;

        // Calculate layout
        const header_size: u32 = @sizeOf(CoffHeader);
        const section_header_size: u32 = @sizeOf(SectionHeader);
        const num_sections: u16 = if (need_unwind) 3 else 1; // .text, .pdata, .xdata

        // Calculate .pdata and .xdata sizes
        // .pdata: 12 bytes per RUNTIME_FUNCTION (BeginAddress, EndAddress, UnwindData)
        const pdata_size: u32 = if (need_unwind) @intCast(self.functions.items.len * 12) else 0;

        // .xdata: UNWIND_INFO for each function
        // Minimal UNWIND_INFO: 4 bytes header + 2 bytes per unwind code (padded to 4-byte alignment)
        var xdata_size: u32 = 0;
        if (need_unwind) {
            for (self.functions.items) |func| {
                // Calculate unwind code count for this function
                var code_count: u32 = 0;
                if (func.uses_frame_pointer) {
                    code_count += 1; // UWOP_SET_FPREG
                    code_count += 1; // UWOP_PUSH_NONVOL for RBP
                }
                if (func.stack_alloc > 0) {
                    if (func.stack_alloc <= 128) {
                        code_count += 1; // UWOP_ALLOC_SMALL
                    } else if (func.stack_alloc <= 512 * 1024 - 8) {
                        code_count += 2; // UWOP_ALLOC_LARGE with 1 extra slot
                    } else {
                        code_count += 3; // UWOP_ALLOC_LARGE with 2 extra slots
                    }
                }
                // UNWIND_INFO header (4 bytes) + unwind codes (2 bytes each, padded to 4 bytes)
                const codes_size = (code_count * 2 + 3) & ~@as(u32, 3); // Round up to 4 bytes
                xdata_size += 4 + codes_size;
            }
        }

        // Calculate offsets
        const section_headers_offset: u32 = header_size;
        const text_offset: u32 = section_headers_offset + section_header_size * num_sections;
        const text_size: u32 = @intCast(self.text.items.len);

        // .pdata follows .text
        const pdata_offset: u32 = text_offset + text_size;
        // .xdata follows .pdata
        const xdata_offset: u32 = pdata_offset + pdata_size;

        // Relocations follow all section data
        // Note: COFF relocations are exactly 10 bytes (not @sizeOf which may include padding)
        const reloc_entry_size: u32 = 10;
        const text_reloc_offset: u32 = xdata_offset + xdata_size;
        const text_reloc_size: u32 = @as(u32, @intCast(self.text_relocs.items.len)) * reloc_entry_size;

        // .pdata relocations (3 per RUNTIME_FUNCTION: BeginAddress, EndAddress, UnwindData)
        const pdata_reloc_offset: u32 = text_reloc_offset + text_reloc_size;
        const pdata_reloc_count: u32 = if (need_unwind) @intCast(self.functions.items.len * 3) else 0;
        const pdata_reloc_size: u32 = pdata_reloc_count * reloc_entry_size;

        // Symbol table comes after all relocations
        const symtab_offset: u32 = pdata_reloc_offset + pdata_reloc_size;

        // Add section symbols for relocations to reference (these must be added before counting)
        // We need symbols for .text, .pdata (for EndAddress relocs), .xdata (for UnwindData relocs)
        const text_section_sym_idx: u32 = @intCast(self.symbols.items.len);
        try self.symbols.append(self.allocator, .{
            .name = ".text",
            .section = .text,
            .offset = 0,
            .is_global = false,
            .is_function = false,
        });

        var xdata_section_sym_idx: u32 = 0;
        if (need_unwind) {
            xdata_section_sym_idx = @intCast(self.symbols.items.len);
            try self.symbols.append(self.allocator, .{
                .name = ".xdata",
                .section = .rdata, // .xdata uses rdata characteristics but we track it separately
                .offset = 0,
                .is_global = false,
                .is_function = false,
            });
        }

        const num_symbols: u32 = @intCast(self.symbols.items.len);

        // Build symbol table entries and string table
        var symtab: std.ArrayList(u8) = .{};
        defer symtab.deinit(self.allocator);

        for (self.symbols.items, 0..) |sym, idx| {
            // Determine section number - handle special section symbols
            const section_number: i16 = blk: {
                // Check if this is the .xdata section symbol
                if (need_unwind and idx == xdata_section_sym_idx) {
                    break :blk SECT_XDATA;
                }
                // Check if this is the .text section symbol
                if (idx == text_section_sym_idx) {
                    break :blk SECT_TEXT;
                }
                // Regular symbol
                break :blk switch (sym.section) {
                    .text => SECT_TEXT,
                    .data => 0, // Would be section 2 if we had .data
                    .rdata => if (need_unwind) SECT_XDATA else 0,
                    .bss => 0,
                    .undef => COFF.IMAGE_SYM_UNDEFINED,
                };
            };

            var coff_sym = CoffSymbol{
                .name_bytes = undefined,
                .value = sym.offset,
                .section_number = section_number,
                .type = if (sym.is_function) COFF.IMAGE_SYM_DTYPE_FUNCTION else COFF.IMAGE_SYM_TYPE_NULL,
                .storage_class = if (sym.is_global) COFF.IMAGE_SYM_CLASS_EXTERNAL else COFF.IMAGE_SYM_CLASS_STATIC,
                .number_of_aux_symbols = 0,
            };

            // Handle symbol name (short names <= 8 bytes go directly, longer ones go to string table)
            if (sym.name.len <= 8) {
                coff_sym.setShortName(sym.name);
            } else {
                const str_offset = try self.addString(sym.name);
                coff_sym.setLongName(str_offset);
            }

            try coff_sym.writeToBuffer(&symtab, self.allocator);
        }

        // Update string table size (first 4 bytes) - write as little-endian u32
        const strtab_size: u32 = @intCast(self.strtab.items.len);
        std.mem.writeInt(u32, self.strtab.items[0..4], strtab_size, .little);

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
            .pointer_to_relocations = if (self.text_relocs.items.len > 0) text_reloc_offset else 0,
            .pointer_to_line_numbers = 0,
            .number_of_relocations = @intCast(self.text_relocs.items.len),
            .number_of_line_numbers = 0,
            .characteristics = COFF.IMAGE_SCN_CNT_CODE |
                COFF.IMAGE_SCN_MEM_EXECUTE |
                COFF.IMAGE_SCN_MEM_READ |
                COFF.IMAGE_SCN_ALIGN_16BYTES,
        };
        try output.appendSlice(self.allocator, std.mem.asBytes(&text_header));

        // Write .pdata section header (if needed)
        if (need_unwind) {
            var pdata_name: [8]u8 = std.mem.zeroes([8]u8);
            @memcpy(pdata_name[0..6], ".pdata");

            const pdata_header = SectionHeader{
                .name = pdata_name,
                .virtual_size = 0,
                .virtual_address = 0,
                .size_of_raw_data = pdata_size,
                .pointer_to_raw_data = pdata_offset,
                .pointer_to_relocations = if (pdata_reloc_count > 0) pdata_reloc_offset else 0,
                .pointer_to_line_numbers = 0,
                .number_of_relocations = @intCast(pdata_reloc_count),
                .number_of_line_numbers = 0,
                .characteristics = COFF.IMAGE_SCN_CNT_INITIALIZED_DATA |
                    COFF.IMAGE_SCN_MEM_READ |
                    COFF.IMAGE_SCN_ALIGN_4BYTES,
            };
            try output.appendSlice(self.allocator, std.mem.asBytes(&pdata_header));

            // Write .xdata section header
            var xdata_name: [8]u8 = std.mem.zeroes([8]u8);
            @memcpy(xdata_name[0..6], ".xdata");

            const xdata_header = SectionHeader{
                .name = xdata_name,
                .virtual_size = 0,
                .virtual_address = 0,
                .size_of_raw_data = xdata_size,
                .pointer_to_raw_data = xdata_offset,
                .pointer_to_relocations = 0, // .xdata has no relocations
                .pointer_to_line_numbers = 0,
                .number_of_relocations = 0,
                .number_of_line_numbers = 0,
                .characteristics = COFF.IMAGE_SCN_CNT_INITIALIZED_DATA |
                    COFF.IMAGE_SCN_MEM_READ |
                    COFF.IMAGE_SCN_ALIGN_4BYTES,
            };
            try output.appendSlice(self.allocator, std.mem.asBytes(&xdata_header));
        }

        // Write .text section content
        try output.appendSlice(self.allocator, self.text.items);

        // Write .pdata section content (RUNTIME_FUNCTION entries)
        if (need_unwind) {
            for (self.functions.items) |func| {
                // RUNTIME_FUNCTION: BeginAddress, EndAddress, UnwindData
                // These will be fixed up by relocations at link time
                // For now, write the offsets within their respective sections
                var runtime_func: [12]u8 = undefined;
                std.mem.writeInt(u32, runtime_func[0..4], func.start_offset, .little); // BeginAddress
                std.mem.writeInt(u32, runtime_func[4..8], func.end_offset, .little); // EndAddress
                std.mem.writeInt(u32, runtime_func[8..12], 0, .little); // UnwindData (offset in .xdata, set below)
                try output.appendSlice(self.allocator, &runtime_func);
            }
        }

        // Write .xdata section content (UNWIND_INFO structures)
        var xdata_offsets: std.ArrayList(u32) = .{};
        defer xdata_offsets.deinit(self.allocator);

        if (need_unwind) {
            var current_xdata_offset: u32 = 0;
            for (self.functions.items) |func| {
                try xdata_offsets.append(self.allocator, current_xdata_offset);

                // Calculate unwind codes for this function
                var unwind_codes: std.ArrayList(u8) = .{};
                defer unwind_codes.deinit(self.allocator);

                // Track prologue offset for each code
                const prolog_offset: u8 = func.prologue_size;

                // Build unwind codes in reverse order of execution
                // (first in array = last prologue instruction)

                // Stack allocation (if any) - happens last in prologue
                if (func.stack_alloc > 0) {
                    if (func.stack_alloc <= 128) {
                        // UWOP_ALLOC_SMALL: OpInfo = (size - 8) / 8
                        const op_info: u8 = @intCast((func.stack_alloc - 8) / 8);
                        try unwind_codes.append(self.allocator, prolog_offset); // Offset in prolog
                        try unwind_codes.append(self.allocator, (op_info << 4) | COFF.UWOP_ALLOC_SMALL);
                    } else if (func.stack_alloc <= 512 * 1024 - 8) {
                        // UWOP_ALLOC_LARGE with OpInfo=0: size/8 in next slot
                        try unwind_codes.append(self.allocator, prolog_offset);
                        try unwind_codes.append(self.allocator, COFF.UWOP_ALLOC_LARGE); // OpInfo=0
                        // Next slot contains size/8 as 16-bit value
                        const size_scaled: u16 = @intCast(func.stack_alloc / 8);
                        try unwind_codes.append(self.allocator, @truncate(size_scaled));
                        try unwind_codes.append(self.allocator, @truncate(size_scaled >> 8));
                    } else {
                        // UWOP_ALLOC_LARGE with OpInfo=1: full 32-bit size in next 2 slots
                        try unwind_codes.append(self.allocator, prolog_offset);
                        try unwind_codes.append(self.allocator, (1 << 4) | COFF.UWOP_ALLOC_LARGE);
                        // Next 2 slots contain full 32-bit size
                        try unwind_codes.append(self.allocator, @truncate(func.stack_alloc));
                        try unwind_codes.append(self.allocator, @truncate(func.stack_alloc >> 8));
                        try unwind_codes.append(self.allocator, @truncate(func.stack_alloc >> 16));
                        try unwind_codes.append(self.allocator, @truncate(func.stack_alloc >> 24));
                    }
                }

                // Set frame pointer (if used) - happens in middle of prologue
                if (func.uses_frame_pointer) {
                    // UWOP_SET_FPREG: no OpInfo, frame reg and offset are in header
                    // Prolog offset should be where MOV RBP,RSP happens (after PUSH RBP)
                    const set_fpreg_offset: u8 = if (prolog_offset > 4) 4 else prolog_offset;
                    try unwind_codes.append(self.allocator, set_fpreg_offset);
                    try unwind_codes.append(self.allocator, COFF.UWOP_SET_FPREG);
                }

                // Push RBP (if frame pointer used) - happens first in prologue
                if (func.uses_frame_pointer) {
                    // UWOP_PUSH_NONVOL: OpInfo = register number (RBP = 5)
                    try unwind_codes.append(self.allocator, 1); // Offset 1 (after PUSH RBP)
                    try unwind_codes.append(self.allocator, (COFF.UNWIND_REG_RBP << 4) | COFF.UWOP_PUSH_NONVOL);
                }

                // Write UNWIND_INFO header
                const code_count: u8 = @intCast(unwind_codes.items.len / 2);
                const version_flags: u8 = 1; // Version 1, no flags
                const frame_reg: u8 = if (func.uses_frame_pointer) COFF.UNWIND_REG_RBP else 0;
                const frame_offset: u8 = func.frame_reg_offset; // Scaled offset

                try output.append(self.allocator, version_flags);
                try output.append(self.allocator, func.prologue_size);
                try output.append(self.allocator, code_count);
                try output.append(self.allocator, (frame_offset << 4) | frame_reg);

                // Write unwind codes
                try output.appendSlice(self.allocator, unwind_codes.items);

                // Pad to 4-byte alignment
                const total_size = 4 + unwind_codes.items.len;
                const padded_size = (total_size + 3) & ~@as(usize, 3);
                const padding = padded_size - total_size;
                for (0..padding) |_| {
                    try output.append(self.allocator, 0);
                }

                current_xdata_offset += @intCast(padded_size);
            }
        }

        // Write .text relocations (10 bytes each: u32 offset, u32 symbol_idx, u16 type)
        for (self.text_relocs.items) |rel| {
            try self.writeRelocation(output, rel.offset, rel.symbol_idx, rel.reloc_type);
        }

        // Write .pdata relocations (3 per RUNTIME_FUNCTION)
        if (need_unwind) {
            for (self.functions.items, 0..) |_, func_idx| {
                const pdata_entry_offset: u32 = @intCast(func_idx * 12);
                const xdata_offset_for_func = xdata_offsets.items[func_idx];

                // Relocation for BeginAddress (offset 0 in entry) -> .text section
                try self.writeRelocation(output, pdata_entry_offset + 0, text_section_sym_idx, COFF.IMAGE_REL_AMD64_ADDR32NB);

                // Relocation for EndAddress (offset 4 in entry) -> .text section
                try self.writeRelocation(output, pdata_entry_offset + 4, text_section_sym_idx, COFF.IMAGE_REL_AMD64_ADDR32NB);

                // Relocation for UnwindData (offset 8 in entry) -> .xdata section
                try self.writeRelocation(output, pdata_entry_offset + 8, xdata_section_sym_idx, COFF.IMAGE_REL_AMD64_ADDR32NB);

                // Update the UnwindData field in .pdata with the correct offset
                // We need to patch the .pdata content we wrote earlier
                // Calculate where in output buffer this entry is
                const pdata_content_start = pdata_offset;
                const unwind_data_field_offset = pdata_content_start + pdata_entry_offset + 8;
                std.mem.writeInt(u32, output.items[unwind_data_field_offset..][0..4], xdata_offset_for_func, .little);
            }
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
