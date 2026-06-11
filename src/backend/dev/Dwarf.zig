//! DWARF debug section serialization for dev-backend object files.
//!
//! Builds `.debug_line`, `.debug_abbrev`, and `.debug_info` byte buffers
//! (DWARF version 4) from the line entries recorded during code generation
//! and the LIR store's source file table. Address fields are emitted as
//! zeroes plus a relocation against the text section, so the per-format
//! object writers can encode them natively.

const std = @import("std");
const base = @import("base");
const Allocator = std.mem.Allocator;

const LineEntry = @import("LirCodeGen.zig").LineEntry;

/// One pointer-sized address field inside a debug section that must be
/// relocated to `text section start + addend`.
pub const AddressReloc = @import("object/mod.zig").DebugReloc;

/// One subprogram entry for `.debug_info`.
pub const ProcEntry = struct {
    name: []const u8,
    code_start: u64,
    code_size: u64,
    loc: base.SourceLoc,
};

/// The serialized DWARF sections plus the relocations they need.
pub const Sections = struct {
    debug_line: []u8,
    debug_abbrev: []u8,
    debug_info: []u8,
    line_relocs: []AddressReloc,
    info_relocs: []AddressReloc,

    pub fn deinit(self: *Sections, gpa: Allocator) void {
        gpa.free(self.debug_line);
        gpa.free(self.debug_abbrev);
        gpa.free(self.debug_info);
        gpa.free(self.line_relocs);
        gpa.free(self.info_relocs);
    }
};

const DW_LNS_copy = 1;
const DW_LNS_advance_pc = 2;
const DW_LNS_advance_line = 3;
const DW_LNS_set_file = 4;
const DW_LNS_set_column = 5;
const DW_LNE_end_sequence = 1;
const DW_LNE_set_address = 2;

const DW_TAG_compile_unit = 0x11;
const DW_TAG_subprogram = 0x2e;
const DW_AT_name = 0x03;
const DW_AT_stmt_list = 0x10;
const DW_AT_low_pc = 0x11;
const DW_AT_high_pc = 0x12;
const DW_AT_language = 0x13;
const DW_AT_producer = 0x25;
const DW_AT_decl_file = 0x3a;
const DW_AT_decl_line = 0x3b;
const DW_FORM_addr = 0x01;
const DW_FORM_data2 = 0x05;
const DW_FORM_data8 = 0x07;
const DW_FORM_string = 0x08;
const DW_FORM_udata = 0x0f;
const DW_FORM_sec_offset = 0x17;
const DW_LANG_C99 = 0x000c;

/// Serializes the three DWARF sections. `source_files` supplies the file
/// table (entries match `SourceLoc.file` indices), `line_entries` must be in
/// ascending code-offset order, and `code_size` is the total text size.
pub fn build(
    gpa: Allocator,
    producer: []const u8,
    source_files: []const []const u8,
    line_entries: []const LineEntry,
    procs: []const ProcEntry,
    code_size: u64,
) Allocator.Error!Sections {
    var line_relocs: std.ArrayList(AddressReloc) = .empty;
    errdefer line_relocs.deinit(gpa);
    var info_relocs: std.ArrayList(AddressReloc) = .empty;
    errdefer info_relocs.deinit(gpa);

    const debug_line = try buildLineSection(gpa, source_files, line_entries, code_size, &line_relocs);
    errdefer gpa.free(debug_line);
    const debug_abbrev = try buildAbbrevSection(gpa);
    errdefer gpa.free(debug_abbrev);
    const cu_name = if (source_files.len > 0) source_files[0] else "roc";
    const debug_info = try buildInfoSection(gpa, producer, cu_name, procs, code_size, &info_relocs);
    errdefer gpa.free(debug_info);

    return .{
        .debug_line = debug_line,
        .debug_abbrev = debug_abbrev,
        .debug_info = debug_info,
        .line_relocs = try line_relocs.toOwnedSlice(gpa),
        .info_relocs = try info_relocs.toOwnedSlice(gpa),
    };
}

fn appendUleb(buf: *std.ArrayList(u8), gpa: Allocator, value: u64) Allocator.Error!void {
    var v = value;
    while (true) {
        const byte: u8 = @intCast(v & 0x7f);
        v >>= 7;
        if (v == 0) {
            try buf.append(gpa, byte);
            return;
        }
        try buf.append(gpa, byte | 0x80);
    }
}

fn appendSleb(buf: *std.ArrayList(u8), gpa: Allocator, value: i64) Allocator.Error!void {
    var v = value;
    while (true) {
        const byte: u8 = @intCast(@as(u64, @bitCast(v)) & 0x7f);
        v >>= 7;
        const sign_clear = v == 0 and (byte & 0x40) == 0;
        const sign_set = v == -1 and (byte & 0x40) != 0;
        if (sign_clear or sign_set) {
            try buf.append(gpa, byte);
            return;
        }
        try buf.append(gpa, byte | 0x80);
    }
}

fn appendInt(buf: *std.ArrayList(u8), gpa: Allocator, comptime T: type, value: T) Allocator.Error!void {
    var bytes: [@sizeOf(T)]u8 = undefined;
    std.mem.writeInt(T, &bytes, value, .little);
    try buf.appendSlice(gpa, &bytes);
}

fn buildLineSection(
    gpa: Allocator,
    source_files: []const []const u8,
    line_entries: []const LineEntry,
    code_size: u64,
    relocs: *std.ArrayList(AddressReloc),
) Allocator.Error![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(gpa);

    // Unit header; the length fields are patched once sizes are known.
    try appendInt(&buf, gpa, u32, 0); // unit_length
    try appendInt(&buf, gpa, u16, 4); // version
    const header_length_at = buf.items.len;
    try appendInt(&buf, gpa, u32, 0); // header_length
    const header_start = buf.items.len;
    try buf.append(gpa, 1); // minimum_instruction_length
    try buf.append(gpa, 1); // maximum_operations_per_instruction
    try buf.append(gpa, 1); // default_is_stmt
    try buf.append(gpa, @bitCast(@as(i8, -5))); // line_base
    try buf.append(gpa, 14); // line_range
    try buf.append(gpa, 13); // opcode_base
    try buf.appendSlice(gpa, &[_]u8{ 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 1 }); // standard_opcode_lengths
    try buf.append(gpa, 0); // include_directories terminator
    for (source_files) |file| {
        try buf.appendSlice(gpa, file);
        try buf.append(gpa, 0);
        try appendUleb(&buf, gpa, 0); // directory index
        try appendUleb(&buf, gpa, 0); // mtime
        try appendUleb(&buf, gpa, 0); // length
    }
    try buf.append(gpa, 0); // file_names terminator
    const header_len: u32 = @intCast(buf.items.len - header_start);
    std.mem.writeInt(u32, buf.items[header_length_at..][0..4], header_len, .little);

    // Line program: one sequence covering the whole text section.
    try buf.append(gpa, 0); // extended opcode
    try appendUleb(&buf, gpa, 9);
    try buf.append(gpa, DW_LNE_set_address);
    try relocs.append(gpa, .{ .section_offset = @intCast(buf.items.len), .addend = 0 });
    try appendInt(&buf, gpa, u64, 0);

    var address: u64 = 0;
    var file: u64 = 1;
    var line: i64 = 1;
    var column: u64 = 0;
    for (line_entries) |entry| {
        const entry_file: u64 = @as(u64, entry.loc.file) + 1;
        if (entry_file != file) {
            try buf.append(gpa, DW_LNS_set_file);
            try appendUleb(&buf, gpa, entry_file);
            file = entry_file;
        }
        if (entry.loc.column != column) {
            try buf.append(gpa, DW_LNS_set_column);
            try appendUleb(&buf, gpa, entry.loc.column);
            column = entry.loc.column;
        }
        const line_delta = @as(i64, entry.loc.line) - line;
        if (line_delta != 0) {
            try buf.append(gpa, DW_LNS_advance_line);
            try appendSleb(&buf, gpa, line_delta);
            line = entry.loc.line;
        }
        const pc_delta = @as(u64, entry.offset) - address;
        if (pc_delta != 0) {
            try buf.append(gpa, DW_LNS_advance_pc);
            try appendUleb(&buf, gpa, pc_delta);
            address = entry.offset;
        }
        try buf.append(gpa, DW_LNS_copy);
    }

    if (code_size > address) {
        try buf.append(gpa, DW_LNS_advance_pc);
        try appendUleb(&buf, gpa, code_size - address);
    }
    try buf.append(gpa, 0); // extended opcode
    try appendUleb(&buf, gpa, 1);
    try buf.append(gpa, DW_LNE_end_sequence);

    const unit_len: u32 = @intCast(buf.items.len - 4);
    std.mem.writeInt(u32, buf.items[0..4], unit_len, .little);
    return buf.toOwnedSlice(gpa);
}

fn buildAbbrevSection(gpa: Allocator) Allocator.Error![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(gpa);

    // Abbrev 1: compile unit with children.
    try appendUleb(&buf, gpa, 1);
    try appendUleb(&buf, gpa, DW_TAG_compile_unit);
    try buf.append(gpa, 1); // DW_CHILDREN_yes
    const cu_attrs = [_][2]u8{
        .{ DW_AT_producer, DW_FORM_string },
        .{ DW_AT_language, DW_FORM_data2 },
        .{ DW_AT_name, DW_FORM_string },
        .{ DW_AT_low_pc, DW_FORM_addr },
        .{ DW_AT_high_pc, DW_FORM_data8 },
        .{ DW_AT_stmt_list, DW_FORM_sec_offset },
    };
    for (cu_attrs) |attr| {
        try appendUleb(&buf, gpa, attr[0]);
        try appendUleb(&buf, gpa, attr[1]);
    }
    try appendUleb(&buf, gpa, 0);
    try appendUleb(&buf, gpa, 0);

    // Abbrev 2: subprogram, no children.
    try appendUleb(&buf, gpa, 2);
    try appendUleb(&buf, gpa, DW_TAG_subprogram);
    try buf.append(gpa, 0); // DW_CHILDREN_no
    const sp_attrs = [_][2]u8{
        .{ DW_AT_name, DW_FORM_string },
        .{ DW_AT_low_pc, DW_FORM_addr },
        .{ DW_AT_high_pc, DW_FORM_data8 },
        .{ DW_AT_decl_file, DW_FORM_udata },
        .{ DW_AT_decl_line, DW_FORM_udata },
    };
    for (sp_attrs) |attr| {
        try appendUleb(&buf, gpa, attr[0]);
        try appendUleb(&buf, gpa, attr[1]);
    }
    try appendUleb(&buf, gpa, 0);
    try appendUleb(&buf, gpa, 0);

    try appendUleb(&buf, gpa, 0); // section terminator
    return buf.toOwnedSlice(gpa);
}

fn buildInfoSection(
    gpa: Allocator,
    producer: []const u8,
    cu_name: []const u8,
    procs: []const ProcEntry,
    code_size: u64,
    relocs: *std.ArrayList(AddressReloc),
) Allocator.Error![]u8 {
    var buf: std.ArrayList(u8) = .empty;
    errdefer buf.deinit(gpa);

    try appendInt(&buf, gpa, u32, 0); // unit_length, patched below
    try appendInt(&buf, gpa, u16, 4); // version
    try appendInt(&buf, gpa, u32, 0); // debug_abbrev_offset
    try buf.append(gpa, 8); // address_size

    // Compile unit DIE.
    try appendUleb(&buf, gpa, 1);
    try buf.appendSlice(gpa, producer);
    try buf.append(gpa, 0);
    try appendInt(&buf, gpa, u16, DW_LANG_C99);
    try buf.appendSlice(gpa, cu_name);
    try buf.append(gpa, 0);
    try relocs.append(gpa, .{ .section_offset = @intCast(buf.items.len), .addend = 0 });
    try appendInt(&buf, gpa, u64, 0); // low_pc
    try appendInt(&buf, gpa, u64, code_size); // high_pc (length form)
    try appendInt(&buf, gpa, u32, 0); // stmt_list

    for (procs) |proc| {
        try appendUleb(&buf, gpa, 2);
        try buf.appendSlice(gpa, proc.name);
        try buf.append(gpa, 0);
        try relocs.append(gpa, .{ .section_offset = @intCast(buf.items.len), .addend = proc.code_start });
        try appendInt(&buf, gpa, u64, 0); // low_pc
        try appendInt(&buf, gpa, u64, proc.code_size); // high_pc (length form)
        const decl_file: u64 = if (proc.loc.hasLocation()) @as(u64, proc.loc.file) + 1 else 0;
        try appendUleb(&buf, gpa, decl_file);
        try appendUleb(&buf, gpa, if (proc.loc.hasLocation()) proc.loc.line else 0);
    }

    try appendUleb(&buf, gpa, 0); // children terminator

    const unit_len: u32 = @intCast(buf.items.len - 4);
    std.mem.writeInt(u32, buf.items[0..4], unit_len, .little);
    return buf.toOwnedSlice(gpa);
}

test "line section round-trips through std.debug parsing constants" {
    const gpa = std.testing.allocator;
    const files = [_][]const u8{"main"};
    const entries = [_]LineEntry{
        .{ .offset = 0, .loc = .{ .file = 0, .line = 3, .column = 1 } },
        .{ .offset = 16, .loc = .{ .file = 0, .line = 4, .column = 5 } },
    };
    const procs = [_]ProcEntry{
        .{ .name = "roc_proc_0", .code_start = 0, .code_size = 32, .loc = .{ .file = 0, .line = 3, .column = 1 } },
    };
    var sections = try build(gpa, "roc test", &files, &entries, &procs, 32);
    defer sections.deinit(gpa);

    try std.testing.expect(sections.debug_line.len > 0);
    try std.testing.expect(sections.debug_abbrev.len > 0);
    try std.testing.expect(sections.debug_info.len > 0);
    try std.testing.expectEqual(@as(usize, 1), sections.line_relocs.len);
    try std.testing.expectEqual(@as(usize, 2), sections.info_relocs.len);
    // unit_length covers the rest of each section exactly.
    try std.testing.expectEqual(
        sections.debug_line.len - 4,
        std.mem.readInt(u32, sections.debug_line[0..4], .little),
    );
    try std.testing.expectEqual(
        sections.debug_info.len - 4,
        std.mem.readInt(u32, sections.debug_info[0..4], .little),
    );
}
