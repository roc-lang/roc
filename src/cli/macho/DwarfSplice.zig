//! Splices DWARF debug sections from a relocatable Mach-O object into a
//! linked executable so the executable is self-contained for debuggers.
//!
//! ld64-style linkers leave DWARF behind in object files and emit a stabs
//! debug map instead. This takes the `__DWARF` sections straight from the
//! object the compiler just produced, resolves their text relocations using
//! the executable's symbol table, and inserts them as a `__DWARF` segment
//! between the last mapped segment and `__LINKEDIT` (which shifts toward the
//! end of the file, since codesigning requires the symbol/string tables to be
//! the last content). The segment is recorded with vmaddr/vmsize/prot of zero
//! so the loader never maps it; only debuggers read it.

const std = @import("std");
const macho = std.macho;
const Io = std.Io;
const Allocator = std.mem.Allocator;

const SpliceError = error{
    NotMacho64,
    NotObjectFile,
    UnexpectedEof,
    MissingSymtab,
    MissingLinkedit,
    MissingTextSection,
    UnsupportedRelocation,
    RelocationOutOfBounds,
    NoLoadCommandSpace,
    UnanchoredTextAddress,
} || Allocator.Error || Io.Dir.ReadFileAllocError || Io.File.OpenError || Io.File.WritePositionalError;

const seg_cmd_size = @sizeOf(macho.segment_command_64);
const sect_size = @sizeOf(macho.section_64);
const header_size = @sizeOf(macho.mach_header_64);

const ObjSection = struct {
    header: macho.section_64,
    ordinal: u32,
};

const TextSymbol = struct {
    obj_addr: u64,
    exe_addr: ?u64,

    fn lessThan(_: void, lhs: TextSymbol, rhs: TextSymbol) bool {
        return lhs.obj_addr < rhs.obj_addr;
    }
};

/// Splice the object's `__DWARF` sections into the executable. The caller
/// must re-sign the executable afterwards. Returns without modifying the
/// executable when the object carries no DWARF sections.
pub fn spliceDwarf(gpa: Allocator, io: Io, exe_path: []const u8, obj_path: []const u8) SpliceError!void {
    const obj_bytes = try Io.Dir.cwd().readFileAlloc(io, obj_path, gpa, .limited(std.math.maxInt(u32)));
    defer gpa.free(obj_bytes);
    const exe_bytes = try Io.Dir.cwd().readFileAlloc(io, exe_path, gpa, .limited(std.math.maxInt(u32)));
    defer gpa.free(exe_bytes);

    const obj_header = try readHeader(obj_bytes);
    if (obj_header.filetype != macho.MH_OBJECT) return SpliceError.NotObjectFile;
    const exe_header = try readHeader(exe_bytes);

    // Collect the object's sections (with global ordinals) and symtab.
    var obj_sections: std.ArrayList(ObjSection) = .empty;
    defer obj_sections.deinit(gpa);
    var obj_symtab: ?macho.symtab_command = null;
    {
        var it = LoadCommandWalker.init(obj_bytes, obj_header);
        var ordinal: u32 = 1;
        while (try it.next()) |lc| {
            switch (lc.cmd.cmd) {
                .SEGMENT_64 => {
                    const seg = std.mem.bytesToValue(macho.segment_command_64, lc.bytes[0..seg_cmd_size]);
                    var sect_off: usize = seg_cmd_size;
                    var i: u32 = 0;
                    while (i < seg.nsects) : (i += 1) {
                        if (sect_off + sect_size > lc.bytes.len) return SpliceError.UnexpectedEof;
                        const sect = std.mem.bytesToValue(macho.section_64, lc.bytes[sect_off..][0..sect_size]);
                        try obj_sections.append(gpa, .{ .header = sect, .ordinal = ordinal });
                        ordinal += 1;
                        sect_off += sect_size;
                    }
                },
                .SYMTAB => obj_symtab = std.mem.bytesToValue(macho.symtab_command, lc.bytes[0..@sizeOf(macho.symtab_command)]),
                else => {},
            }
        }
    }
    const symtab = obj_symtab orelse return SpliceError.MissingSymtab;

    var dwarf_sections: std.ArrayList(ObjSection) = .empty;
    defer dwarf_sections.deinit(gpa);
    var text_section: ?ObjSection = null;
    for (obj_sections.items) |sect| {
        if (std.mem.eql(u8, sect.header.segName(), "__DWARF")) {
            try dwarf_sections.append(gpa, sect);
        } else if (std.mem.eql(u8, sect.header.segName(), "__TEXT") and
            std.mem.eql(u8, sect.header.sectName(), "__text"))
        {
            text_section = sect;
        }
    }
    if (dwarf_sections.items.len == 0) return;
    const text = text_section orelse return SpliceError.MissingTextSection;

    // Symbol name -> final address from the executable's symbol table.
    var exe_symbols = std.StringHashMap(u64).init(gpa);
    defer exe_symbols.deinit();
    try collectExeSymbols(&exe_symbols, exe_bytes, exe_header);

    // Every defined symbol in the object's __text section, sorted by object
    // address. Each one anchors the byte range up to the next symbol; ranges
    // whose symbol did not survive into the executable were dead-stripped.
    const obj_strtab_end = symtab.stroff + symtab.strsize;
    if (obj_strtab_end > obj_bytes.len) return SpliceError.UnexpectedEof;
    var text_symbols: std.ArrayList(TextSymbol) = .empty;
    defer text_symbols.deinit(gpa);
    const obj_nlists = try readNlists(gpa, obj_bytes, symtab);
    defer gpa.free(obj_nlists);
    for (obj_nlists) |nl| {
        if (nl.n_type.bits.is_stab != 0) continue;
        if (nl.n_type.bits.type != .sect) continue;
        if (nl.n_sect != text.ordinal) continue;
        const name = strtabName(obj_bytes, symtab, nl.n_strx) orelse continue;
        try text_symbols.append(gpa, .{
            .obj_addr = nl.n_value,
            .exe_addr = exe_symbols.get(name),
        });
    }
    std.mem.sort(TextSymbol, text_symbols.items, {}, TextSymbol.lessThan);

    // Lay out the DWARF blob: each section copied (with relocations applied),
    // 8-aligned within the segment.
    var blob: std.ArrayList(u8) = .empty;
    defer blob.deinit(gpa);
    var out_sections = try gpa.alloc(macho.section_64, dwarf_sections.items.len);
    defer gpa.free(out_sections);
    for (dwarf_sections.items, 0..) |sect, i| {
        const aligned_len = std.mem.alignForward(usize, blob.items.len, 8);
        try blob.appendNTimes(gpa, 0, aligned_len - blob.items.len);
        const blob_offset = blob.items.len;

        const src_start: usize = sect.header.offset;
        const src_len: usize = @intCast(sect.header.size);
        if (src_start + src_len > obj_bytes.len) return SpliceError.UnexpectedEof;
        try blob.appendSlice(gpa, obj_bytes[src_start..][0..src_len]);
        const data = blob.items[blob_offset..][0..src_len];

        try applyRelocations(
            data,
            obj_bytes,
            sect.header,
            obj_sections.items,
            obj_nlists,
            symtab,
            text,
            text_symbols.items,
            &exe_symbols,
        );

        var out = sect.header;
        out.addr = blob_offset;
        out.size = src_len;
        out.offset = 0; // patched below once dwarfstart is known
        out.@"align" = 3;
        out.reloff = 0;
        out.nreloc = 0;
        out_sections[i] = out;
    }

    try rewriteExecutable(gpa, io, exe_path, exe_bytes, exe_header, blob.items, out_sections);
}

fn readHeader(bytes: []const u8) SpliceError!macho.mach_header_64 {
    if (bytes.len < header_size) return SpliceError.UnexpectedEof;
    const header = std.mem.bytesToValue(macho.mach_header_64, bytes[0..header_size]);
    if (header.magic != macho.MH_MAGIC_64) return SpliceError.NotMacho64;
    return header;
}

const LoadCommandWalker = struct {
    bytes: []const u8,
    offset: usize,
    remaining: u32,
    end: usize,

    const Entry = struct {
        cmd: macho.load_command,
        bytes: []const u8,
        file_offset: usize,
    };

    fn init(bytes: []const u8, header: macho.mach_header_64) LoadCommandWalker {
        return .{
            .bytes = bytes,
            .offset = header_size,
            .remaining = header.ncmds,
            .end = header_size + header.sizeofcmds,
        };
    }

    fn next(self: *LoadCommandWalker) SpliceError!?Entry {
        if (self.remaining == 0) return null;
        if (self.offset + @sizeOf(macho.load_command) > self.end or self.end > self.bytes.len) {
            return SpliceError.UnexpectedEof;
        }
        const lc = std.mem.bytesToValue(macho.load_command, self.bytes[self.offset..][0..@sizeOf(macho.load_command)]);
        if (lc.cmdsize < @sizeOf(macho.load_command) or self.offset + lc.cmdsize > self.end) {
            return SpliceError.UnexpectedEof;
        }
        const entry = Entry{
            .cmd = lc,
            .bytes = self.bytes[self.offset..][0..lc.cmdsize],
            .file_offset = self.offset,
        };
        self.offset += lc.cmdsize;
        self.remaining -= 1;
        return entry;
    }
};

fn readNlists(gpa: Allocator, bytes: []const u8, symtab: macho.symtab_command) SpliceError![]macho.nlist_64 {
    const start: usize = symtab.symoff;
    const len = @as(usize, symtab.nsyms) * @sizeOf(macho.nlist_64);
    if (start + len > bytes.len) return SpliceError.UnexpectedEof;
    const out = try gpa.alloc(macho.nlist_64, symtab.nsyms);
    for (out, 0..) |*nl, i| {
        nl.* = std.mem.bytesToValue(macho.nlist_64, bytes[start + i * @sizeOf(macho.nlist_64) ..][0..@sizeOf(macho.nlist_64)]);
    }
    return out;
}

fn strtabName(bytes: []const u8, symtab: macho.symtab_command, n_strx: u32) ?[]const u8 {
    if (n_strx == 0) return null;
    const start: usize = symtab.stroff + n_strx;
    if (start >= bytes.len) return null;
    const end = std.mem.findScalarPos(u8, bytes, start, 0) orelse return null;
    if (end > symtab.stroff + symtab.strsize) return null;
    return bytes[start..end];
}

fn collectExeSymbols(
    map: *std.StringHashMap(u64),
    bytes: []const u8,
    header: macho.mach_header_64,
) SpliceError!void {
    var it = LoadCommandWalker.init(bytes, header);
    while (try it.next()) |lc| {
        if (lc.cmd.cmd != .SYMTAB) continue;
        const symtab = std.mem.bytesToValue(macho.symtab_command, lc.bytes[0..@sizeOf(macho.symtab_command)]);
        const start: usize = symtab.symoff;
        var i: usize = 0;
        while (i < symtab.nsyms) : (i += 1) {
            const off = start + i * @sizeOf(macho.nlist_64);
            if (off + @sizeOf(macho.nlist_64) > bytes.len) return SpliceError.UnexpectedEof;
            const nl = std.mem.bytesToValue(macho.nlist_64, bytes[off..][0..@sizeOf(macho.nlist_64)]);
            if (nl.n_type.bits.is_stab != 0) continue;
            if (nl.n_type.bits.type != .sect) continue;
            const name = strtabName(bytes, symtab, nl.n_strx) orelse continue;
            try map.put(name, nl.n_value);
        }
        return;
    }
    return SpliceError.MissingSymtab;
}

/// Map an object-space `__text` address to its final executable address, or
/// null when the containing function was dead-stripped.
fn mapTextAddress(text_symbols: []const TextSymbol, obj_addr: u64) SpliceError!?u64 {
    if (text_symbols.len == 0) return SpliceError.UnanchoredTextAddress;
    var lo: usize = 0;
    var hi: usize = text_symbols.len;
    while (lo < hi) {
        const mid = lo + (hi - lo) / 2;
        if (text_symbols[mid].obj_addr <= obj_addr) {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    if (lo == 0) return SpliceError.UnanchoredTextAddress;
    const anchor = text_symbols[lo - 1];
    const exe_addr = anchor.exe_addr orelse return null;
    return exe_addr + (obj_addr - anchor.obj_addr);
}

fn applyRelocations(
    data: []u8,
    obj_bytes: []const u8,
    sect: macho.section_64,
    obj_sections: []const ObjSection,
    obj_nlists: []const macho.nlist_64,
    symtab: macho.symtab_command,
    text: ObjSection,
    text_symbols: []const TextSymbol,
    exe_symbols: *const std.StringHashMap(u64),
) SpliceError!void {
    var i: usize = 0;
    while (i < sect.nreloc) : (i += 1) {
        const off: usize = sect.reloff + i * @sizeOf(macho.relocation_info);
        if (off + @sizeOf(macho.relocation_info) > obj_bytes.len) return SpliceError.UnexpectedEof;
        const reloc = std.mem.bytesToValue(macho.relocation_info, obj_bytes[off..][0..@sizeOf(macho.relocation_info)]);

        // DWARF sections only carry absolute pointer-sized fixups.
        if (reloc.r_pcrel != 0 or reloc.r_type != 0 or reloc.r_address < 0) {
            return SpliceError.UnsupportedRelocation;
        }
        const field_size: usize = switch (reloc.r_length) {
            2 => 4,
            3 => 8,
            else => return SpliceError.UnsupportedRelocation,
        };
        const field_off: usize = @intCast(reloc.r_address);
        if (field_off + field_size > data.len) return SpliceError.RelocationOutOfBounds;

        const stored: u64 = if (field_size == 8)
            std.mem.readInt(u64, data[field_off..][0..8], .little)
        else
            std.mem.readInt(u32, data[field_off..][0..4], .little);

        var value: u64 = undefined;
        if (reloc.r_extern == 1) {
            if (reloc.r_symbolnum >= obj_nlists.len) return SpliceError.UnexpectedEof;
            const nl = obj_nlists[reloc.r_symbolnum];
            const name = strtabName(obj_bytes, symtab, nl.n_strx) orelse return SpliceError.UnsupportedRelocation;
            // The stored bytes are the addend; a stripped target tombstones
            // the whole field to zero.
            value = if (exe_symbols.get(name)) |addr| addr +% stored else 0;
        } else if (reloc.r_symbolnum == text.ordinal) {
            // The stored bytes are an object-space address inside __text.
            value = (try mapTextAddress(text_symbols, stored)) orelse 0;
        } else {
            // References between DWARF sections are section-relative offsets
            // in the final image; the object stores them as object-space
            // addresses.
            const target = targetSection(obj_sections, reloc.r_symbolnum) orelse return SpliceError.UnsupportedRelocation;
            if (!std.mem.eql(u8, target.header.segName(), "__DWARF")) return SpliceError.UnsupportedRelocation;
            value = stored -% target.header.addr;
        }

        if (field_size == 8) {
            std.mem.writeInt(u64, data[field_off..][0..8], value, .little);
        } else {
            const narrow = std.math.cast(u32, value) orelse return SpliceError.RelocationOutOfBounds;
            std.mem.writeInt(u32, data[field_off..][0..4], narrow, .little);
        }
    }
}

fn targetSection(sections: []const ObjSection, ordinal: u32) ?ObjSection {
    for (sections) |sect| {
        if (sect.ordinal == ordinal) return sect;
    }
    return null;
}

fn rewriteExecutable(
    gpa: Allocator,
    io: Io,
    exe_path: []const u8,
    exe_bytes: []const u8,
    exe_header: macho.mach_header_64,
    blob: []const u8,
    dwarf_sects: []macho.section_64,
) SpliceError!void {
    const page_size: u64 = if (exe_header.cputype == macho.CPU_TYPE_ARM64) 0x4000 else 0x1000;

    // Find __LINKEDIT and the first section content offset (the load command
    // region must fit the new segment command in the gap before it).
    var linkedit_fileoff: ?u64 = null;
    var min_content_offset: u64 = std.math.maxInt(u64);
    {
        var it = LoadCommandWalker.init(exe_bytes, exe_header);
        while (try it.next()) |lc| {
            if (lc.cmd.cmd != .SEGMENT_64) continue;
            const seg = std.mem.bytesToValue(macho.segment_command_64, lc.bytes[0..seg_cmd_size]);
            if (std.mem.eql(u8, seg.segName(), "__LINKEDIT")) linkedit_fileoff = seg.fileoff;
            var sect_off: usize = seg_cmd_size;
            var i: u32 = 0;
            while (i < seg.nsects) : (i += 1) {
                const sect = std.mem.bytesToValue(macho.section_64, lc.bytes[sect_off..][0..sect_size]);
                if (sect.offset != 0 and sect.size != 0) {
                    min_content_offset = @min(min_content_offset, sect.offset);
                }
                sect_off += sect_size;
            }
        }
    }
    const linkedit_start = linkedit_fileoff orelse return SpliceError.MissingLinkedit;

    const new_cmd_size: u32 = @intCast(seg_cmd_size + dwarf_sects.len * sect_size);
    const cmds_end = header_size + exe_header.sizeofcmds;
    if (cmds_end + new_cmd_size > min_content_offset) return SpliceError.NoLoadCommandSpace;

    const dwarfstart = std.mem.alignForward(u64, linkedit_start, 8);
    const linkstart = std.mem.alignForward(u64, dwarfstart + blob.len, page_size);
    const link_shift = linkstart - linkedit_start;

    // Compose the new file in memory.
    var out: std.ArrayList(u8) = .empty;
    defer out.deinit(gpa);
    try out.ensureTotalCapacity(gpa, @as(usize, @intCast(linkstart)) + (exe_bytes.len - @as(usize, @intCast(linkedit_start))));
    out.appendSliceAssumeCapacity(exe_bytes[0..@intCast(dwarfstart)]);
    out.appendSliceAssumeCapacity(blob);
    out.appendNTimesAssumeCapacity(0, @intCast(linkstart - (dwarfstart + blob.len)));
    out.appendSliceAssumeCapacity(exe_bytes[@intCast(linkedit_start)..]);

    // Shift every load command field that holds a __LINKEDIT file offset.
    {
        var it = LoadCommandWalker.init(exe_bytes, exe_header);
        while (try it.next()) |lc| {
            const base = lc.file_offset;
            switch (lc.cmd.cmd) {
                .SEGMENT_64 => {
                    const seg = std.mem.bytesToValue(macho.segment_command_64, lc.bytes[0..seg_cmd_size]);
                    if (seg.fileoff >= linkedit_start and std.mem.eql(u8, seg.segName(), "__LINKEDIT")) {
                        // fileoff: u64 at offset 40 of segment_command_64.
                        shiftField(u64, out.items, base + 40, link_shift);
                    }
                },
                .SYMTAB => {
                    // symoff at 8, stroff at 16.
                    shiftField(u32, out.items, base + 8, link_shift);
                    shiftField(u32, out.items, base + 16, link_shift);
                },
                .DYSYMTAB => {
                    // tocoff 32, modtaboff 40, extrefsymoff 48, indirectsymoff 56,
                    // extreloff 64, locreloff 72.
                    shiftField(u32, out.items, base + 32, link_shift);
                    shiftField(u32, out.items, base + 40, link_shift);
                    shiftField(u32, out.items, base + 48, link_shift);
                    shiftField(u32, out.items, base + 56, link_shift);
                    shiftField(u32, out.items, base + 64, link_shift);
                    shiftField(u32, out.items, base + 72, link_shift);
                },
                .CODE_SIGNATURE,
                .SEGMENT_SPLIT_INFO,
                .FUNCTION_STARTS,
                .DATA_IN_CODE,
                .DYLIB_CODE_SIGN_DRS,
                .LINKER_OPTIMIZATION_HINT,
                .DYLD_EXPORTS_TRIE,
                .DYLD_CHAINED_FIXUPS,
                => {
                    // linkedit_data_command dataoff at 8.
                    shiftField(u32, out.items, base + 8, link_shift);
                },
                .DYLD_INFO, .DYLD_INFO_ONLY => {
                    // rebase_off 8, bind_off 16, weak_bind_off 24,
                    // lazy_bind_off 32, export_off 40.
                    shiftField(u32, out.items, base + 8, link_shift);
                    shiftField(u32, out.items, base + 16, link_shift);
                    shiftField(u32, out.items, base + 24, link_shift);
                    shiftField(u32, out.items, base + 32, link_shift);
                    shiftField(u32, out.items, base + 40, link_shift);
                },
                else => {},
            }
        }
    }

    // Append the __DWARF segment load command. vmaddr/vmsize/prot stay zero
    // so the loader never maps the segment.
    {
        const seg = macho.segment_command_64{
            .cmdsize = new_cmd_size,
            .segname = makeName("__DWARF"),
            .vmaddr = 0,
            .vmsize = 0,
            .fileoff = dwarfstart,
            .filesize = blob.len,
            .maxprot = .{},
            .initprot = .{},
            .nsects = @intCast(dwarf_sects.len),
            .flags = 0,
        };
        var cursor = cmds_end;
        @memcpy(out.items[cursor..][0..seg_cmd_size], std.mem.asBytes(&seg));
        cursor += seg_cmd_size;
        for (dwarf_sects) |*sect| {
            sect.offset = @intCast(dwarfstart + sect.addr);
            @memcpy(out.items[cursor..][0..sect_size], std.mem.asBytes(sect));
            cursor += sect_size;
        }
        const new_ncmds = exe_header.ncmds + 1;
        const new_sizeofcmds = exe_header.sizeofcmds + new_cmd_size;
        std.mem.writeInt(u32, out.items[16..20], new_ncmds, .little);
        std.mem.writeInt(u32, out.items[20..24], new_sizeofcmds, .little);
    }

    var file = try Io.Dir.cwd().openFile(io, exe_path, .{ .mode = .read_write });
    defer file.close(io);
    try file.writePositionalAll(io, out.items, 0);
}

fn shiftField(comptime T: type, bytes: []u8, offset: usize, shift: u64) void {
    const current = std.mem.readInt(T, bytes[offset..][0..@sizeOf(T)], .little);
    if (current == 0) return;
    std.mem.writeInt(T, bytes[offset..][0..@sizeOf(T)], current + @as(T, @intCast(shift)), .little);
}

fn makeName(name: []const u8) [16]u8 {
    var out = [_]u8{0} ** 16;
    @memcpy(out[0..name.len], name);
    return out;
}
