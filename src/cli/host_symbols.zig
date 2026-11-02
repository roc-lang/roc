//! Pre-link verification that the platform's host inputs define the symbols
//! compiled Roc code references.
//!
//! Apps reference hosted functions and the fixed runtime set (roc_alloc and
//! friends) as extern symbols the host satisfies at link time. LLVM output
//! references hosted symbols weakly, so a missing host implementation would
//! otherwise surface only as a null call at runtime; the runtime set and dev
//! output would surface as raw linker errors. Scanning the host inputs'
//! symbol tables up front turns both into a proper diagnostic.

const std = @import("std");
const shim_symbols = @import("builtins").shim_symbols;

const Allocator = std.mem.Allocator;

/// Validate an offset and length from an object file before forming a slice.
fn checkedSlice(bytes: anytype, offset: usize, size: usize) ?[]const u8 {
    if (comptime @TypeOf(bytes) == Input) return bytes.read(offset, size);
    const end = std.math.add(usize, offset, size) catch return null;
    if (end > bytes.len) return null;
    const slice: []const u8 = bytes;
    return slice[offset..end];
}

fn checkedTableSize(count: usize, entry_size: usize) ?usize {
    return std.math.mul(usize, count, entry_size) catch null;
}

/// The fixed runtime symbols every symbol-ABI host defines.
pub const runtime_symbols: [shim_symbols.runtime_set.len][]const u8 = blk: {
    var symbols: [shim_symbols.runtime_set.len][]const u8 = undefined;
    for (shim_symbols.runtime_set, 0..) |name, index| {
        symbols[index] = name;
    }
    break :blk symbols;
};

/// Outcome of scanning the host inputs for a set of needed symbols.
pub const ScanResult = struct {
    /// Needed symbols no scanned input defines. Slices alias the caller's
    /// `needed` strings.
    missing: []const []const u8,
    /// Whether inspection was complete for the requested query. Once all
    /// required symbols are found, unvisited inputs/members are irrelevant.
    /// When false, `missing` is not authoritative and must not be diagnosed;
    /// the linker has the final say.
    all_inputs_scanned: bool,
};

/// Scan each host input's symbol tables and report which of `needed` none of
/// them define. Inputs may be ar archives (ELF, Mach-O, COFF members), bare
/// object files of those formats, or wasm objects/archives.
pub fn scanHostInputs(
    arena: Allocator,
    io: std.Io,
    host_input_paths: []const []const u8,
    needed: []const []const u8,
) Allocator.Error!ScanResult {
    const result = try inspect(arena, io, host_input_paths, needed, false);
    return .{ .missing = result.missing, .all_inputs_scanned = result.all_inputs_scanned };
}

fn stringLessThan(_: void, a: []const u8, b: []const u8) bool {
    return std.mem.order(u8, a, b) == .lt;
}

/// Collect the symbol names the host inputs declare as exports, so a shared
/// library link can force-include and export exactly those on every target.
/// This mirrors each platform linker's own "what gets exported" rule, so a
/// host declares its public API the same way it would for any shared library:
///   - ELF: defined GLOBAL/WEAK symbols with DEFAULT or PROTECTED visibility
///     (the host's `.hidden` runtime glue is excluded).
///   - Mach-O: defined external symbols that are not private-external.
///   - COFF: names in `.drectve` `/export:` directives (the COFF symbol table
///     carries no visibility, so the directive—what `__declspec(dllexport)`
///     emits—is the only export signal).
/// wasm shared output is handled separately via the wasm export section.
/// Names are duped into `arena`. Inputs in an unrecognized format are skipped;
/// the linker keeps the final say, so a miss degrades to prior behavior rather
/// than dropping a real export hard.
pub fn collectHostExports(
    arena: Allocator,
    io: std.Io,
    host_input_paths: []const []const u8,
) Allocator.Error![]const []const u8 {
    return (try inspect(arena, io, host_input_paths, &.{}, true)).exports;
}

const ExportSink = struct {
    arena: Allocator,
    seen: *std.StringHashMap(void),
    exports: *std.ArrayList([]const u8),

    fn add(self: ExportSink, name: []const u8) Allocator.Error!void {
        if (name.len == 0) return;
        if (self.seen.contains(name)) return;
        const owned = try self.arena.dupe(u8, name);
        try self.seen.put(owned, {});
        try self.exports.append(self.arena, owned);
    }
};

fn collectInput(arena: Allocator, bytes: []const u8, seen: *std.StringHashMap(void), exports: *std.ArrayList([]const u8)) Allocator.Error!void {
    var remaining = std.StringHashMap(void).init(arena);
    _ = try inspectInput(bytes, &remaining, .{ .arena = arena, .seen = seen, .exports = exports });
}

fn collectObject(bytes: anytype, sink: ExportSink) Allocator.Error!void {
    if (bytes.len >= 4) {
        const magic = std.mem.readInt(u32, (checkedSlice(bytes, 0, 4) orelse return)[0..4], .little);
        if (std.mem.eql(u8, (checkedSlice(bytes, 0, 4) orelse return)[0..4], "\x7fELF")) return collectElfObject(bytes, sink);
        if (magic == std.macho.MH_MAGIC_64) return collectMachoObject(bytes, sink);
        if (std.mem.eql(u8, (checkedSlice(bytes, 0, 4) orelse return)[0..4], "\x00asm")) return; // wasm handled elsewhere
    }
    if (bytes.len >= 2) {
        const machine = std.mem.readInt(u16, (checkedSlice(bytes, 0, 2) orelse return)[0..2], .little);
        if (machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.AMD64) or
            machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.ARM64) or
            machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.I386))
        {
            return collectCoffObject(bytes, sink);
        }
    }
}

fn collectElfObject(bytes: anytype, sink: ExportSink) Allocator.Error!void {
    const elf = std.elf;
    if (bytes.len < @sizeOf(elf.Elf64_Ehdr)) return;
    const ehdr = std.mem.bytesAsValue(elf.Elf64_Ehdr, (checkedSlice(bytes, 0, @sizeOf(elf.Elf64_Ehdr)) orelse return));
    if (ehdr.e_ident[elf.EI_CLASS] != elf.ELFCLASS64 or ehdr.e_ident[elf.EI_DATA] != elf.ELFDATA2LSB) return;

    const shoff = std.math.cast(usize, ehdr.e_shoff) orelse return;
    const shnum: usize = ehdr.e_shnum;
    const shentsize: usize = ehdr.e_shentsize;
    if (shnum == 0 or shentsize < @sizeOf(elf.Elf64_Shdr)) return; // extended section numbering is unsupported
    const section_bytes = checkedTableSize(shnum, shentsize) orelse return;
    const sections = checkedSlice(bytes, shoff, section_bytes) orelse return;

    var i: usize = 0;
    while (i < shnum) : (i += 1) {
        const shdr = std.mem.bytesAsValue(elf.Elf64_Shdr, sections[i * shentsize ..][0..@sizeOf(elf.Elf64_Shdr)]);
        if (shdr.sh_type != elf.SHT_SYMTAB) continue;
        countTable(bytes);

        const strtab_index: usize = shdr.sh_link;
        if (strtab_index >= shnum) return;
        const strtab_hdr = std.mem.bytesAsValue(elf.Elf64_Shdr, sections[strtab_index * shentsize ..][0..@sizeOf(elf.Elf64_Shdr)]);
        const strtab_off = std.math.cast(usize, strtab_hdr.sh_offset) orelse return;
        const strtab_size = std.math.cast(usize, strtab_hdr.sh_size) orelse return;
        const strtab = checkedSlice(bytes, strtab_off, strtab_size) orelse return;

        const sym_off = std.math.cast(usize, shdr.sh_offset) orelse return;
        const sym_size = std.math.cast(usize, shdr.sh_size) orelse return;
        const symbols = checkedSlice(bytes, sym_off, sym_size) orelse return;
        const sym_count = symbols.len / @sizeOf(elf.Elf64_Sym);

        var s: usize = 0;
        while (s < sym_count) : (s += 1) {
            const sym = std.mem.bytesAsValue(elf.Elf64_Sym, symbols[s * @sizeOf(elf.Elf64_Sym) ..][0..@sizeOf(elf.Elf64_Sym)]);
            if (sym.st_shndx == elf.SHN_UNDEF) continue;
            const binding = sym.st_info >> 4;
            if (binding != elf.STB_GLOBAL and binding != elf.STB_WEAK) continue;
            // Only DEFAULT or PROTECTED visibility symbols are exported from a
            // shared object; HIDDEN/INTERNAL are the host's internals.
            const visibility: u3 = @intCast(sym.st_other & 0x3);
            if (visibility != @backingInt(elf.STV.DEFAULT) and visibility != @backingInt(elf.STV.PROTECTED)) continue;
            const name_off: usize = sym.st_name;
            if (name_off >= strtab.len) continue;
            const name = std.mem.sliceTo(strtab[name_off..], 0);
            try sink.add(name);
        }
    }
}

fn collectMachoObject(bytes: anytype, sink: ExportSink) Allocator.Error!void {
    const macho = std.macho;
    if (bytes.len < @sizeOf(macho.mach_header_64)) return;
    const header = std.mem.bytesAsValue(macho.mach_header_64, (checkedSlice(bytes, 0, @sizeOf(macho.mach_header_64)) orelse return));

    var offset: usize = @sizeOf(macho.mach_header_64);
    var cmd_index: u32 = 0;
    while (cmd_index < header.ncmds) : (cmd_index += 1) {
        const cmd_bytes = checkedSlice(bytes, offset, @sizeOf(macho.load_command)) orelse return;
        const cmd = std.mem.bytesAsValue(macho.load_command, cmd_bytes);
        if (cmd.cmdsize < @sizeOf(macho.load_command)) return;
        const full_cmd = checkedSlice(bytes, offset, cmd.cmdsize) orelse return;
        if (cmd.cmd == .SYMTAB) {
            countTable(bytes);
            if (full_cmd.len < @sizeOf(macho.symtab_command)) return;
            const symtab = std.mem.bytesAsValue(macho.symtab_command, full_cmd[0..@sizeOf(macho.symtab_command)]);

            const str_off: usize = symtab.stroff;
            const str_size: usize = symtab.strsize;
            const strtab = checkedSlice(bytes, str_off, str_size) orelse return;

            const sym_off: usize = symtab.symoff;
            const sym_count: usize = symtab.nsyms;
            const symbols_size = checkedTableSize(sym_count, @sizeOf(macho.nlist_64)) orelse return;
            const symbols = checkedSlice(bytes, sym_off, symbols_size) orelse return;

            var s: usize = 0;
            while (s < sym_count) : (s += 1) {
                const nlist = std.mem.bytesAsValue(macho.nlist_64, symbols[s * @sizeOf(macho.nlist_64) ..][0..@sizeOf(macho.nlist_64)]);
                if (nlist.n_type.bits.is_stab != 0) continue;
                if (!nlist.n_type.bits.ext) continue;
                // Private-external symbols (the host's `.hidden`) are not exported.
                if (nlist.n_type.bits.pext) continue;
                if (nlist.n_type.bits.type != .sect) continue;
                const name_off: usize = nlist.n_strx;
                if (name_off >= strtab.len) continue;
                const raw = std.mem.sliceTo(strtab[name_off..], 0);
                // Mach-O C symbols carry a leading underscore; the export-table
                // name the loader resolves is the unmangled form.
                const name = if (raw.len > 1 and raw[0] == '_') raw[1..] else raw;
                try sink.add(name);
            }
        }
        offset = std.math.add(usize, offset, cmd.cmdsize) catch return;
    }
}

/// Collect `/export:` operands from a COFF object's `.drectve` section. This is
/// the only place COFF records export intent (the symbol table has no
/// visibility), and it is exactly what `__declspec(dllexport)` emits.
fn collectCoffObject(bytes: anytype, sink: ExportSink) Allocator.Error!void {
    if (bytes.len < 20) return;
    const num_sections: usize = std.mem.readInt(u16, (checkedSlice(bytes, 2, 2) orelse return)[0..2], .little);
    const optional_header_size: usize = std.mem.readInt(u16, (checkedSlice(bytes, 16, 2) orelse return)[0..2], .little);
    const section_table_off = std.math.add(usize, 20, optional_header_size) catch return;
    const section_header_size = 40;
    const section_table_size = checkedTableSize(num_sections, section_header_size) orelse return;
    const sections = checkedSlice(bytes, section_table_off, section_table_size) orelse return;

    var i: usize = 0;
    while (i < num_sections) : (i += 1) {
        const sh = sections[i * section_header_size ..][0..section_header_size];
        const name = std.mem.sliceTo(sh[0..8], 0);
        if (!std.mem.eql(u8, name, ".drectve")) continue;

        const size_of_raw_data: usize = std.mem.readInt(u32, sh[16..20], .little);
        const ptr_to_raw_data: usize = std.mem.readInt(u32, sh[20..24], .little);
        if (ptr_to_raw_data == 0 or size_of_raw_data == 0) continue;
        const text = checkedSlice(bytes, ptr_to_raw_data, size_of_raw_data) orelse continue;
        try collectDrectveExports(text, sink);
    }
}

fn collectDrectveExports(text: []const u8, sink: ExportSink) Allocator.Error!void {
    var it = std.mem.tokenizeAny(u8, text, " \t\r\n");
    while (it.next()) |raw_token| {
        var token = raw_token;
        if (token.len > 0 and (token[0] == '/' or token[0] == '-')) token = token[1..];
        if (token.len < "export:".len) continue;
        if (!std.ascii.eqlIgnoreCase(token[0.."export:".len], "export:")) continue;

        var name = token["export:".len..];
        // `/export:exportName=internalName` and `/export:name,@ord,DATA`: the
        // exported (loader-visible) name is the part before `=` or `,`.
        if (std.mem.findAny(u8, name, "=,")) |cut| name = name[0..cut];
        try sink.add(name);
    }
}

/// A defined global symbol satisfies a needed name directly or with one
/// leading underscore stripped (Mach-O and 32-bit COFF mangle C names that
/// way).
fn defineSymbol(remaining: *std.StringHashMap(void), name: []const u8) void {
    if (remaining.remove(name)) return;
    if (name.len > 1 and name[0] == '_') {
        _ = remaining.remove(name[1..]);
    }
}

/// Scan one input (archive or object). Returns false when the format is not
/// recognized, so the caller knows the result is not authoritative.
fn scanInput(bytes: []const u8, remaining: *std.StringHashMap(void)) Allocator.Error!bool {
    return inspectInput(bytes, remaining, null);
}

fn scanObject(bytes: anytype, remaining: *std.StringHashMap(void)) bool {
    if (bytes.len >= 4) {
        const magic = std.mem.readInt(u32, (checkedSlice(bytes, 0, 4) orelse return false)[0..4], .little);
        if (bytes.len >= 4 and std.mem.eql(u8, (checkedSlice(bytes, 0, 4) orelse return false)[0..4], "\x7fELF")) {
            return scanElfObject(bytes, remaining);
        }
        if (magic == std.macho.MH_MAGIC_64) {
            return scanMachoObject(bytes, remaining);
        }
        if (std.mem.eql(u8, (checkedSlice(bytes, 0, 4) orelse return false)[0..4], "\x00asm")) {
            return scanWasmObject(bytes, remaining);
        }
    }
    if (bytes.len >= 2) {
        const machine = std.mem.readInt(u16, (checkedSlice(bytes, 0, 2) orelse return false)[0..2], .little);
        if (machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.AMD64) or
            machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.ARM64) or
            machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.I386))
        {
            return scanCoffObject(bytes, remaining);
        }
    }
    return false;
}

fn scanElfObject(bytes: anytype, remaining: *std.StringHashMap(void)) bool {
    const elf = std.elf;
    if (bytes.len < @sizeOf(elf.Elf64_Ehdr)) return false;
    const ehdr = std.mem.bytesAsValue(elf.Elf64_Ehdr, (checkedSlice(bytes, 0, @sizeOf(elf.Elf64_Ehdr)) orelse return false));
    if (ehdr.e_ident[elf.EI_CLASS] != elf.ELFCLASS64 or ehdr.e_ident[elf.EI_DATA] != elf.ELFDATA2LSB) return false;

    const shoff = std.math.cast(usize, ehdr.e_shoff) orelse return false;
    const shnum: usize = ehdr.e_shnum;
    const shentsize: usize = ehdr.e_shentsize;
    if (shnum == 0 or shentsize < @sizeOf(elf.Elf64_Shdr)) return false; // extended section numbering is unsupported
    const section_bytes = checkedTableSize(shnum, shentsize) orelse return false;
    const sections = checkedSlice(bytes, shoff, section_bytes) orelse return false;

    var i: usize = 0;
    while (i < shnum) : (i += 1) {
        if (remaining.count() == 0) return true;
        const shdr = std.mem.bytesAsValue(elf.Elf64_Shdr, sections[i * shentsize ..][0..@sizeOf(elf.Elf64_Shdr)]);
        if (shdr.sh_type != elf.SHT_SYMTAB) continue;
        countTable(bytes);

        const strtab_index: usize = shdr.sh_link;
        if (strtab_index >= shnum) return false;
        const strtab_hdr = std.mem.bytesAsValue(elf.Elf64_Shdr, sections[strtab_index * shentsize ..][0..@sizeOf(elf.Elf64_Shdr)]);
        const strtab_off = std.math.cast(usize, strtab_hdr.sh_offset) orelse return false;
        const strtab_size = std.math.cast(usize, strtab_hdr.sh_size) orelse return false;
        const strtab = checkedSlice(bytes, strtab_off, strtab_size) orelse return false;

        const sym_off = std.math.cast(usize, shdr.sh_offset) orelse return false;
        const sym_size = std.math.cast(usize, shdr.sh_size) orelse return false;
        const symbols = checkedSlice(bytes, sym_off, sym_size) orelse return false;
        const sym_count = symbols.len / @sizeOf(elf.Elf64_Sym);

        var s: usize = 0;
        while (s < sym_count) : (s += 1) {
            if (remaining.count() == 0) return true;
            const sym = std.mem.bytesAsValue(elf.Elf64_Sym, symbols[s * @sizeOf(elf.Elf64_Sym) ..][0..@sizeOf(elf.Elf64_Sym)]);
            if (sym.st_shndx == elf.SHN_UNDEF) continue;
            const binding = sym.st_info >> 4;
            if (binding != elf.STB_GLOBAL and binding != elf.STB_WEAK) continue;
            const name_off: usize = sym.st_name;
            if (name_off >= strtab.len) continue;
            const name = std.mem.sliceTo(strtab[name_off..], 0);
            if (name.len > 0) defineSymbol(remaining, name);
        }
    }
    return true;
}

fn scanMachoObject(bytes: anytype, remaining: *std.StringHashMap(void)) bool {
    const macho = std.macho;
    if (bytes.len < @sizeOf(macho.mach_header_64)) return false;
    const header = std.mem.bytesAsValue(macho.mach_header_64, (checkedSlice(bytes, 0, @sizeOf(macho.mach_header_64)) orelse return false));

    var offset: usize = @sizeOf(macho.mach_header_64);
    var cmd_index: u32 = 0;
    while (cmd_index < header.ncmds) : (cmd_index += 1) {
        if (remaining.count() == 0) return true;
        const cmd_bytes = checkedSlice(bytes, offset, @sizeOf(macho.load_command)) orelse return false;
        const cmd = std.mem.bytesAsValue(macho.load_command, cmd_bytes);
        if (cmd.cmdsize < @sizeOf(macho.load_command)) return false;
        const full_cmd = checkedSlice(bytes, offset, cmd.cmdsize) orelse return false;
        if (cmd.cmd == .SYMTAB) {
            countTable(bytes);
            if (full_cmd.len < @sizeOf(macho.symtab_command)) return false;
            const symtab = std.mem.bytesAsValue(macho.symtab_command, full_cmd[0..@sizeOf(macho.symtab_command)]);

            const str_off: usize = symtab.stroff;
            const str_size: usize = symtab.strsize;
            const strtab = checkedSlice(bytes, str_off, str_size) orelse return false;

            const sym_off: usize = symtab.symoff;
            const sym_count: usize = symtab.nsyms;
            const symbols_size = checkedTableSize(sym_count, @sizeOf(macho.nlist_64)) orelse return false;
            const symbols = checkedSlice(bytes, sym_off, symbols_size) orelse return false;

            var s: usize = 0;
            while (s < sym_count) : (s += 1) {
                if (remaining.count() == 0) return true;
                const nlist = std.mem.bytesAsValue(macho.nlist_64, symbols[s * @sizeOf(macho.nlist_64) ..][0..@sizeOf(macho.nlist_64)]);
                if (nlist.n_type.bits.is_stab != 0) continue;
                if (!nlist.n_type.bits.ext) continue;
                if (nlist.n_type.bits.type != .sect) continue;
                const name_off: usize = nlist.n_strx;
                if (name_off >= strtab.len) continue;
                const name = std.mem.sliceTo(strtab[name_off..], 0);
                if (name.len > 0) defineSymbol(remaining, name);
            }
        }
        offset = std.math.add(usize, offset, cmd.cmdsize) catch return false;
    }
    return true;
}

fn scanCoffObject(bytes: anytype, remaining: *std.StringHashMap(void)) bool {
    // COFF object header: Machine(2) NumberOfSections(2) TimeDateStamp(4)
    // PointerToSymbolTable(4) NumberOfSymbols(4) SizeOfOptionalHeader(2)
    // Characteristics(2)
    if (bytes.len < 20) return false;
    countTable(bytes);
    const symtab_offset: usize = std.mem.readInt(u32, (checkedSlice(bytes, 8, 4) orelse return false)[0..4], .little);
    const symbol_count: usize = std.mem.readInt(u32, (checkedSlice(bytes, 12, 4) orelse return false)[0..4], .little);
    const symbol_size = 18;
    const symbols_size = checkedTableSize(symbol_count, symbol_size) orelse return false;
    const symbols = checkedSlice(bytes, symtab_offset, symbols_size) orelse return false;

    // The string table immediately follows the symbol table; its first four
    // bytes are its total size (including those bytes).
    const strtab_offset = std.math.add(usize, symtab_offset, symbols_size) catch return false;
    var strtab: []const u8 = &.{};
    if (checkedSlice(bytes, strtab_offset, 4)) |size_bytes| {
        const strtab_size: usize = std.mem.readInt(u32, size_bytes[0..4], .little);
        if (strtab_size >= 4) {
            if (checkedSlice(bytes, strtab_offset, strtab_size)) |table| strtab = table;
        }
    }

    var s: usize = 0;
    while (s < symbol_count) : (s += 1) {
        if (remaining.count() == 0) return true;
        const record = symbols[s * symbol_size ..][0..symbol_size];
        const aux_count: usize = record[17];
        const section_number = std.mem.readInt(i16, record[12..14], .little);
        const storage_class = record[16];

        // IMAGE_SYM_CLASS_EXTERNAL with a real section = defined global.
        if (storage_class == 2 and section_number > 0) {
            if (std.mem.readInt(u32, record[0..4], .little) == 0) {
                // Long name: bytes 4..8 are an offset into the string table.
                const name_off: usize = std.mem.readInt(u32, record[4..8], .little);
                if (name_off < strtab.len) {
                    const name = std.mem.sliceTo(strtab[name_off..], 0);
                    if (name.len > 0) defineSymbol(remaining, name);
                }
            } else {
                const name = std.mem.sliceTo(record[0..8], 0);
                if (name.len > 0) defineSymbol(remaining, name);
            }
        }

        s += aux_count;
    }
    return true;
}

/// Scan a wasm object's linking section for defined function/global/data
/// symbols.
fn scanWasmObject(bytes: anytype, remaining: *std.StringHashMap(void)) bool {
    if (bytes.len < 8) return false;
    var offset: usize = 8; // magic + version

    while (offset < bytes.len) {
        if (remaining.count() == 0) return true;
        const section_id = (checkedSlice(bytes, offset, 1) orelse return false)[0];
        offset += 1;
        const section_size = readLeb32(bytes, &offset) orelse return false;
        const section_end = std.math.add(usize, offset, section_size) catch return false;
        if (section_end > bytes.len) return false;

        if (section_id == 0) {
            // Custom section: name then payload.
            var pos = offset;
            const name_len = readLeb32(bytes, &pos) orelse return false;
            if (pos > section_end or name_len > section_end - pos) return false;
            const name = checkedSlice(bytes, pos, name_len) orelse return false;
            pos = std.math.add(usize, pos, name_len) catch return false;
            if (std.mem.eql(u8, name, "linking")) {
                countTable(bytes);
                if (!scanWasmLinking(checkedSlice(bytes, pos, section_end - pos) orelse return false, remaining)) return false;
            }
        }
        offset = section_end;
    }
    return true;
}

fn scanWasmLinking(payload: []const u8, remaining: *std.StringHashMap(void)) bool {
    var pos: usize = 0;
    _ = readLeb32(payload, &pos) orelse return false; // version

    while (pos < payload.len) {
        if (remaining.count() == 0) return true;
        const subsection_type = payload[pos];
        pos += 1;
        const subsection_size = readLeb32(payload, &pos) orelse return false;
        const subsection_end = std.math.add(usize, pos, subsection_size) catch return false;
        if (subsection_end > payload.len) return false;

        if (subsection_type == 8) { // WASM_SYMBOL_TABLE
            const count = readLeb32(payload, &pos) orelse return false;
            var i: usize = 0;
            while (i < count) : (i += 1) {
                if (remaining.count() == 0) return true;
                if (pos >= subsection_end) return false;
                const kind = payload[pos];
                pos += 1;
                const flags = readLeb32(payload, &pos) orelse return false;
                const undefined_flag = flags & 0x10 != 0;
                const explicit_name = flags & 0x40 != 0;

                switch (kind) {
                    // function, global, event, table: index, then name when
                    // defined (or explicitly named).
                    0, 2, 3, 5 => {
                        _ = readLeb32(payload, &pos) orelse return false;
                        if (!undefined_flag or explicit_name) {
                            const name_len = readLeb32(payload, &pos) orelse return false;
                            const name = checkedSlice(payload[0..subsection_end], pos, name_len) orelse return false;
                            pos = std.math.add(usize, pos, name_len) catch return false;
                            if (!undefined_flag) defineSymbol(remaining, name);
                        }
                    },
                    // data: name, then segment/offset/size when defined.
                    1 => {
                        const name_len = readLeb32(payload, &pos) orelse return false;
                        const name = checkedSlice(payload[0..subsection_end], pos, name_len) orelse return false;
                        pos = std.math.add(usize, pos, name_len) catch return false;
                        if (!undefined_flag) {
                            defineSymbol(remaining, name);
                            _ = readLeb32(payload, &pos) orelse return false;
                            _ = readLeb32(payload, &pos) orelse return false;
                            _ = readLeb32(payload, &pos) orelse return false;
                        }
                    },
                    // section: section index only.
                    4 => {
                        _ = readLeb32(payload, &pos) orelse return false;
                    },
                    else => return false,
                }
            }
        }
        pos = subsection_end;
    }
    return true;
}

fn readLeb32(bytes: anytype, pos: *usize) ?usize {
    var result: u32 = 0;
    var shift: u5 = 0;
    while (pos.* < bytes.len) {
        const byte = (checkedSlice(bytes, pos.*, 1) orelse return null)[0];
        pos.* += 1;
        if (shift == 28 and byte & 0x70 != 0) return null;
        result |= @as(u32, byte & 0x7f) << shift;
        if (byte & 0x80 == 0) return result;
        if (shift >= 28) return null;
        shift += 7;
    }
    return null;
}

test "readLeb32 rejects values above u32" {
    var pos: usize = 0;
    try std.testing.expect(readLeb32(&.{ 0xff, 0xff, 0xff, 0xff, 0x1f }, &pos) == null);
    pos = 0;
    try std.testing.expectEqual(@as(?usize, std.math.maxInt(u32)), readLeb32(&.{ 0xff, 0xff, 0xff, 0xff, 0x0f }, &pos));
}

test "scanArArchive walks members and respects alignment" {
    // Minimal two-member archive with one fake (unscannable) member.
    var buf = std.ArrayList(u8).empty;
    defer buf.deinit(std.testing.allocator);
    try buf.appendSlice(std.testing.allocator, "!<arch>\n");
    // Member: name "x.o", size 3 (odd, exercises 2-byte alignment padding).
    try buf.appendSlice(std.testing.allocator, "x.o             0           0     0     644     3         `\n");
    try buf.appendSlice(std.testing.allocator, "abc\n");

    var remaining = std.StringHashMap(void).init(std.testing.allocator);
    defer remaining.deinit();
    try remaining.put(shim_symbols.roc_alloc, {});

    // The member isn't a recognized object format, so the scan reports
    // non-authoritative.
    try std.testing.expect(!try scanInput(buf.items, &remaining));
    try std.testing.expectEqual(@as(u32, 1), remaining.count());
}

test "defineSymbol strips one leading underscore" {
    var remaining = std.StringHashMap(void).init(std.testing.allocator);
    defer remaining.deinit();
    try remaining.put(shim_symbols.roc_alloc, {});
    defineSymbol(&remaining, "_roc_alloc");
    try std.testing.expectEqual(@as(u32, 0), remaining.count());
}

test "checked object ranges reject arithmetic overflow" {
    const bytes = [_]u8{ 1, 2, 3, 4 };
    try std.testing.expectEqualSlices(u8, &.{ 2, 3 }, checkedSlice(&bytes, 1, 2).?);
    try std.testing.expect(checkedSlice(&bytes, std.math.maxInt(usize), 2) == null);
    try std.testing.expect(checkedSlice(&bytes, 3, 2) == null);
    try std.testing.expect(checkedTableSize(std.math.maxInt(usize), 2) == null);
}

test "ELF scanner rejects overflowing and unsupported section ranges" {
    const elf = std.elf;
    const header_size = @sizeOf(elf.Elf64_Ehdr);
    const section_size = @sizeOf(elf.Elf64_Shdr);
    const symbol_size = @sizeOf(elf.Elf64_Sym);
    var base: [header_size + 2 * section_size + symbol_size + 4]u8 = @splat(0);
    const header = std.mem.bytesAsValue(elf.Elf64_Ehdr, base[0..header_size]);
    @memcpy(header.e_ident[0..4], "\x7fELF");
    header.e_ident[elf.EI_CLASS] = elf.ELFCLASS64;
    header.e_ident[elf.EI_DATA] = elf.ELFDATA2LSB;
    header.e_shoff = header_size;
    header.e_shnum = 2;
    header.e_shentsize = section_size;
    const symtab = std.mem.bytesAsValue(elf.Elf64_Shdr, base[header_size..][0..section_size]);
    symtab.sh_type = elf.SHT_SYMTAB;
    symtab.sh_link = 1;
    symtab.sh_offset = header_size + 2 * section_size;
    symtab.sh_size = symbol_size;
    const strtab = std.mem.bytesAsValue(elf.Elf64_Shdr, base[header_size + section_size ..][0..section_size]);
    strtab.sh_offset = header_size + 2 * section_size + symbol_size;
    strtab.sh_size = 4;

    var remaining = std.StringHashMap(void).init(std.testing.allocator);
    defer remaining.deinit();
    try remaining.put("unresolved", {});
    try std.testing.expect(try scanInput(&base, &remaining));

    for (0..7) |case_index| {
        var bad = base;
        const bad_header = std.mem.bytesAsValue(elf.Elf64_Ehdr, bad[0..header_size]);
        const bad_symtab = std.mem.bytesAsValue(elf.Elf64_Shdr, bad[header_size..][0..section_size]);
        const bad_strtab = std.mem.bytesAsValue(elf.Elf64_Shdr, bad[header_size + section_size ..][0..section_size]);
        switch (case_index) {
            0 => bad_header.e_shoff = std.math.maxInt(u64) - section_size + 1,
            1 => bad_header.e_shnum = 0, // extended section numbering requires section zero
            2 => bad_symtab.sh_link = 2,
            3 => bad_strtab.sh_offset = std.math.maxInt(u64) - 1,
            4 => bad_strtab.sh_size = std.math.maxInt(u64),
            5 => bad_symtab.sh_offset = std.math.maxInt(u64) - 1,
            6 => bad_symtab.sh_size = std.math.maxInt(u64),
            else => return error.TestUnexpectedResult,
        }
        try std.testing.expect(!try scanInput(&bad, &remaining));
        var seen = std.StringHashMap(void).init(std.testing.allocator);
        defer seen.deinit();
        var exports = std.ArrayList([]const u8).empty;
        defer exports.deinit(std.testing.allocator);
        try collectInput(std.testing.allocator, &bad, &seen, &exports);
    }
}

test "Mach-O scanner rejects malformed load commands and symbol ranges" {
    const macho = std.macho;
    const header_size = @sizeOf(macho.mach_header_64);
    const command_size = @sizeOf(macho.symtab_command);
    var base: [header_size + command_size + @sizeOf(macho.nlist_64) + 4]u8 = @splat(0);
    const header = std.mem.bytesAsValue(macho.mach_header_64, base[0..header_size]);
    header.magic = macho.MH_MAGIC_64;
    header.ncmds = 1;
    const command = std.mem.bytesAsValue(macho.symtab_command, base[header_size..][0..command_size]);
    command.cmd = .SYMTAB;
    command.cmdsize = command_size;
    command.symoff = header_size + command_size;
    command.nsyms = 1;
    command.stroff = header_size + command_size + @sizeOf(macho.nlist_64);
    command.strsize = 4;

    var remaining = std.StringHashMap(void).init(std.testing.allocator);
    defer remaining.deinit();
    try remaining.put("unresolved", {});
    try std.testing.expect(try scanInput(&base, &remaining));
    for (0..4) |case_index| {
        var bad = base;
        const bad_command = std.mem.bytesAsValue(macho.symtab_command, bad[header_size..][0..command_size]);
        switch (case_index) {
            0 => bad_command.cmdsize = 0,
            1 => bad_command.cmdsize = @sizeOf(macho.load_command),
            2 => bad_command.symoff = std.math.maxInt(u32),
            3 => bad_command.stroff = std.math.maxInt(u32),
            else => return error.TestUnexpectedResult,
        }
        try std.testing.expect(!try scanInput(&bad, &remaining));
    }

    var bad_name = base;
    const nlist_off = header_size + command_size;
    const nlist = std.mem.bytesAsValue(macho.nlist_64, bad_name[nlist_off..][0..@sizeOf(macho.nlist_64)]);
    nlist.n_type.bits.ext = true;
    nlist.n_type.bits.type = .sect;
    nlist.n_strx = std.math.maxInt(u32);
    try remaining.put("missing", {});
    try std.testing.expect(try scanInput(&bad_name, &remaining));
    try std.testing.expect(remaining.contains("missing"));
}

/// Work performed by inspection, independent of linker work. Copied bytes
/// count actual positional reads, not logical input sizes or filesystem I/O.
pub const Counters = struct {
    copied_bytes: u64 = 0,
    archive_members: u64 = 0,
    symbol_tables: u64 = 0,
    peak_retained_bytes: u64 = 0,
};

/// Shared verification/export result; all names outlive scanner buffers.
pub const Inspection = struct {
    missing: []const []const u8,
    exports: []const []const u8,
    all_inputs_scanned: bool,
    counters: Counters,
};

/// Mutable local inputs have no authoritative content key. Inspect them on
/// every build; do not hash a whole archive merely to cache its small index.
/// Both queries share one file handle and one set of retained ranges per
/// object. Export discovery always visits the complete archive.
pub fn inspect(arena: Allocator, io: std.Io, paths: []const []const u8, needed: []const []const u8, want_exports: bool) Allocator.Error!Inspection {
    var remaining = std.StringHashMap(void).init(arena);
    defer remaining.deinit();
    for (needed) |name| try remaining.put(name, {});
    var seen = std.StringHashMap(void).init(arena);
    defer seen.deinit();
    var exports: std.ArrayList([]const u8) = .empty;
    const sink: ?ExportSink = if (want_exports) .{ .arena = arena, .seen = &seen, .exports = &exports } else null;
    var counters: Counters = .{};
    var complete = true;
    for (paths) |path| {
        if (remaining.count() == 0 and !want_exports) break;
        const file = std.Io.Dir.cwd().openFile(io, path, .{}) catch {
            complete = false;
            continue;
        };
        defer file.close(io);
        const before = file.stat(io) catch {
            complete = false;
            continue;
        };
        const size = std.math.cast(usize, before.size) orelse {
            complete = false;
            continue;
        };
        if (before.kind != .file) {
            complete = false;
            continue;
        }
        // Scanner buffers must be freed before linking, even when the result
        // allocator is the CLI's invocation-long arena.
        var storage = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer storage.deinit();
        var source: FileSource = .{ .file = file, .io = io, .storage = &storage, .counters = &counters };
        const input: Input = .{ .source = &source, .offset = 0, .len = size };
        if (!try inspectInput(input, &remaining, sink)) complete = false;
        if (source.oom) return error.OutOfMemory;
        if (source.failed) complete = false;
        const after = file.stat(io) catch {
            complete = false;
            continue;
        };
        // This is a concurrent-write guard, never a persistent content key.
        if (!sameFileState(before, after)) complete = false;
    }
    var missing: std.ArrayList([]const u8) = .empty;
    var it = remaining.keyIterator();
    while (it.next()) |key| try missing.append(arena, key.*);
    std.mem.sort([]const u8, missing.items, {}, stringLessThan);
    return .{ .missing = missing.items, .exports = exports.items, .all_inputs_scanned = complete, .counters = counters };
}

/// The link owner checks this state around inspection and LLD. Publishers
/// replace local inputs atomically; an in-place edit during a build is an
/// input error, not a cached success. Access time is deliberately excluded.
pub fn sameFileState(a: std.Io.File.Stat, b: std.Io.File.Stat) bool {
    return a.inode == b.inode and a.size == b.size and a.kind == b.kind and
        a.mtime.nanoseconds == b.mtime.nanoseconds and a.ctime.nanoseconds == b.ctime.nanoseconds;
}

const FileSource = struct {
    file: std.Io.File,
    io: std.Io,
    storage: *std.heap.ArenaAllocator,
    counters: *Counters,
    ranges: std.ArrayList(Range) = .empty,
    retained: u64 = 0,
    failed: bool = false,
    oom: bool = false,

    const Range = struct { offset: u64, bytes: []const u8 };

    fn reset(self: *FileSource) void {
        _ = self.storage.reset(.retain_capacity);
        self.ranges = .empty;
        self.retained = 0;
    }
};

/// A bounded view of a file or archive member. Every read borrows storage
/// until the current member's inspection completes. No mapping of mutable
/// files: truncation is a short read, never SIGBUS. Object code/debug sections
/// are skipped entirely; only the format's inspection tables are read.
const Input = struct {
    source: *FileSource,
    offset: u64,
    len: usize,

    fn sub(self: Input, offset: usize, len: usize) ?Input {
        if (offset > self.len or len > self.len - offset) return null;
        return .{ .source = self.source, .offset = self.offset + offset, .len = len };
    }

    fn read(self: Input, offset: usize, len: usize) ?[]const u8 {
        const view = self.sub(offset, len) orelse return null;
        if (len == 0) return &.{};
        const source = self.source;
        for (source.ranges.items) |range| {
            if (view.offset >= range.offset and view.offset - range.offset <= range.bytes.len) {
                const start: usize = @intCast(view.offset - range.offset);
                if (len <= range.bytes.len - start) return range.bytes[start..][0..len];
            }
        }
        const allocator = source.storage.allocator();
        const bytes = allocator.alloc(u8, len) catch {
            source.oom = true;
            return null;
        };
        const count = source.file.readPositionalAll(source.io, bytes, view.offset) catch {
            source.failed = true;
            return null;
        };
        source.counters.copied_bytes += count;
        if (count != len) {
            source.failed = true;
            return null;
        }
        source.retained += len;
        source.counters.peak_retained_bytes = @max(source.counters.peak_retained_bytes, source.retained);
        source.ranges.append(allocator, .{ .offset = view.offset, .bytes = bytes }) catch {
            source.oom = true;
            return null;
        };
        return bytes;
    }
};

fn countTable(bytes: anytype) void {
    if (comptime @TypeOf(bytes) == Input) bytes.source.counters.symbol_tables += 1;
}

fn subInput(bytes: anytype, offset: usize, len: usize) ?(if (@TypeOf(bytes) == Input) Input else []const u8) {
    if (comptime @TypeOf(bytes) == Input) return bytes.sub(offset, len);
    return checkedSlice(bytes, offset, len);
}

fn inspectObject(input: anytype, remaining: *std.StringHashMap(void), sink: ?ExportSink) Allocator.Error!bool {
    const complete = if (remaining.count() != 0) scanObject(input, remaining) else true;
    if (sink) |exports| try collectObject(input, exports);
    return complete;
}

fn inspectInput(input: anytype, remaining: *std.StringHashMap(void), sink: ?ExportSink) Allocator.Error!bool {
    const magic = checkedSlice(input, 0, @min(8, input.len)) orelse return false;
    if (!std.mem.eql(u8, magic, "!<arch>\n")) return inspectObject(input, remaining, sink);
    var offset: usize = 8;
    var complete = true;
    while (offset < input.len) {
        if (remaining.count() == 0 and sink == null) break;
        defer if (comptime @TypeOf(input) == Input) input.source.reset();
        const header = checkedSlice(input, offset, 60) orelse return false;
        if (!std.mem.eql(u8, header[58..60], "`\n")) return false;
        const size = std.fmt.parseInt(usize, std.mem.trim(u8, header[48..58], " "), 10) catch return false;
        offset = std.math.add(usize, offset, 60) catch return false;
        var member = subInput(input, offset, size) orelse return false;
        var name = std.mem.trimEnd(u8, header[0..16], " ");
        if (std.mem.startsWith(u8, name, "#1/")) {
            const name_len = std.fmt.parseInt(usize, name[3..], 10) catch return false;
            name = std.mem.trimEnd(u8, checkedSlice(member, 0, name_len) orelse return false, "\x00");
            member = subInput(member, name_len, member.len - name_len) orelse return false;
        }
        const is_index = std.mem.eql(u8, name, "/") or std.mem.eql(u8, name, "//") or
            std.mem.eql(u8, name, "/SYM64/") or std.mem.startsWith(u8, name, "__.SYMDEF");
        if (!is_index and member.len > 0) {
            if (comptime @TypeOf(input) == Input) input.source.counters.archive_members += 1;
            if (!try inspectObject(member, remaining, sink)) complete = false;
        }
        offset = std.math.add(usize, offset, size) catch return false;
        offset = std.math.add(usize, offset, offset & 1) catch return false;
        if (offset > input.len) return false;
    }
    return complete;
}

fn testElf(symbol: []const u8, weak: bool) [512]u8 {
    var bytes: [512]u8 = @splat(0);
    const elf = std.elf;
    const header = std.mem.bytesAsValue(elf.Elf64_Ehdr, bytes[0..64]);
    @memcpy(header.e_ident[0..4], "\x7fELF");
    header.e_ident[elf.EI_CLASS] = elf.ELFCLASS64;
    header.e_ident[elf.EI_DATA] = elf.ELFDATA2LSB;
    header.e_shoff = 64;
    header.e_shnum = 2;
    header.e_shentsize = 64;
    const symtab = std.mem.bytesAsValue(elf.Elf64_Shdr, bytes[64..128]);
    symtab.sh_type = elf.SHT_SYMTAB;
    symtab.sh_link = 1;
    symtab.sh_offset = 192;
    symtab.sh_size = 24;
    const strings = std.mem.bytesAsValue(elf.Elf64_Shdr, bytes[128..192]);
    strings.sh_offset = 216;
    strings.sh_size = symbol.len + 2;
    const sym = std.mem.bytesAsValue(elf.Elf64_Sym, bytes[192..216]);
    sym.st_shndx = 1;
    sym.st_info = @as(u8, if (weak) elf.STB_WEAK else elf.STB_GLOBAL) << 4;
    sym.st_name = 1;
    @memcpy(bytes[217..][0..symbol.len], symbol);
    return bytes;
}

fn testArHeader(size: usize) std.fmt.BufPrintError![60]u8 {
    var header: [60]u8 = @splat(' ');
    @memcpy(header[0..4], "x.o/");
    _ = try std.fmt.bufPrint(header[48..58], "{d}", .{size});
    @memcpy(header[58..60], "`\n");
    return header;
}

test "file inspection reads only tables, stops exactly, and enumerates all exports" {
    const io = std.testing.io;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const file = try tmp.dir.createFile(io, "host.a", .{ .read = true });
    defer file.close(io);
    const large_size = 128 * 1024 * 1024;
    try file.writePositionalAll(io, "!<arch>\n", 0);
    try file.writePositionalAll(io, &try testArHeader(large_size), 8);
    try file.writePositionalAll(io, &testElf("first", false), 68);
    try file.writePositionalAll(io, &try testArHeader(512), 68 + large_size);
    try file.writePositionalAll(io, &testElf("last", true), 128 + large_size);
    const path = try tmp.dir.realPathFileAlloc(io, "host.a", a);
    const result = try inspect(a, io, &.{path}, &.{"first"}, false);
    try std.testing.expect(result.all_inputs_scanned);
    try std.testing.expectEqual(@as(usize, 0), result.missing.len);
    try std.testing.expectEqual(@as(u64, 1), result.counters.archive_members);
    try std.testing.expect(result.counters.copied_bytes < 1024);
    try std.testing.expect(result.counters.peak_retained_bytes < 1024);
    const full = try inspect(a, io, &.{path}, &.{ "first", "last", "z_missing", "a_missing" }, true);
    try std.testing.expect(full.all_inputs_scanned);
    try std.testing.expectEqual(@as(u64, 2), full.counters.archive_members);
    try std.testing.expectEqualStrings("a_missing", full.missing[0]);
    try std.testing.expectEqualStrings("z_missing", full.missing[1]);
    try std.testing.expectEqual(@as(usize, 2), full.exports.len);
    try std.testing.expectEqualStrings("first", full.exports[0]);
    try std.testing.expectEqualStrings("last", full.exports[1]);
    try std.testing.expect(full.counters.copied_bytes < 2048);
    // Same-size in-place edits are rescanned, without a timestamp cache key.
    try file.writePositionalAll(io, &testElf("other", false), 68);
    const changed = try inspect(a, io, &.{path}, &.{"first"}, false);
    try std.testing.expectEqual(@as(usize, 1), changed.missing.len);
    // Truncation is an ordinary incomplete scan, not a mapping fault.
    try file.setLength(io, 80);
    const truncated = try inspect(a, io, &.{path}, &.{"first"}, false);
    try std.testing.expect(!truncated.all_inputs_scanned);
}

test "inspection handles multiple objects, replacement, empty and removed inputs" {
    const io = std.testing.io;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(io, .{ .sub_path = "a.o", .data = &testElf("one", false) });
    try tmp.dir.writeFile(io, .{ .sub_path = "b.o", .data = &testElf("two", true) });
    const paths = [_][]const u8{ try tmp.dir.realPathFileAlloc(io, "a.o", a), try tmp.dir.realPathFileAlloc(io, "b.o", a) };
    const found = try inspect(a, io, &paths, &.{ "one", "two" }, false);
    try std.testing.expect(found.all_inputs_scanned);
    try std.testing.expectEqual(@as(usize, 0), found.missing.len);
    try tmp.dir.writeFile(io, .{ .sub_path = "new.o", .data = &testElf("new", false) });
    try tmp.dir.rename("new.o", tmp.dir, "a.o", io);
    const replaced = try inspect(a, io, &paths, &.{"one"}, false);
    try std.testing.expect(replaced.all_inputs_scanned);
    try std.testing.expectEqual(@as(usize, 1), replaced.missing.len);
    try tmp.dir.writeFile(io, .{ .sub_path = "a.o", .data = "" });
    try std.testing.expect(!(try inspect(a, io, &paths, &.{"one"}, false)).all_inputs_scanned);
    try tmp.dir.deleteFile(io, "a.o");
    try std.testing.expect(!(try inspect(a, io, &paths, &.{"one"}, false)).all_inputs_scanned);
}

test "Mach-O and COFF inspection preserves exported and private symbols" {
    const io = std.testing.io;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var macho_bytes: [160]u8 = @splat(0);
    const macho = std.macho;
    const header = std.mem.bytesAsValue(macho.mach_header_64, macho_bytes[0..32]);
    header.magic = macho.MH_MAGIC_64;
    header.ncmds = 1;
    const command = std.mem.bytesAsValue(macho.symtab_command, macho_bytes[32..56]);
    command.cmd = .SYMTAB;
    command.cmdsize = 24;
    command.symoff = 56;
    command.nsyms = 2;
    command.stroff = 88;
    command.strsize = 20;
    for (0..2) |i| {
        const symbol = std.mem.bytesAsValue(macho.nlist_64, macho_bytes[56 + i * 16 ..][0..16]);
        symbol.n_type.bits.ext = true;
        symbol.n_type.bits.pext = i == 1;
        symbol.n_type.bits.type = .sect;
        symbol.n_strx = if (i == 0) 1 else 9;
    }
    @memcpy(macho_bytes[89..97], "_public\x00");
    @memcpy(macho_bytes[97..106], "_private\x00");
    try tmp.dir.writeFile(io, .{ .sub_path = "macho.o", .data = &macho_bytes });
    const macho_path = try tmp.dir.realPathFileAlloc(io, "macho.o", a);
    const m = try inspect(a, io, &.{macho_path}, &.{ "public", "private" }, true);
    try std.testing.expect(m.all_inputs_scanned);
    try std.testing.expectEqual(@as(usize, 0), m.missing.len);
    try std.testing.expectEqual(@as(usize, 1), m.exports.len);
    try std.testing.expectEqualStrings("public", m.exports[0]);

    var coff: [128]u8 = @splat(0);
    std.mem.writeInt(u16, coff[0..2], 0x8664, .little);
    std.mem.writeInt(u16, coff[2..4], 1, .little);
    std.mem.writeInt(u32, coff[8..12], 60, .little);
    std.mem.writeInt(u32, coff[12..16], 1, .little);
    @memcpy(coff[20..28], ".drectve");
    std.mem.writeInt(u32, coff[36..40], 15, .little);
    std.mem.writeInt(u32, coff[40..44], 82, .little);
    @memcpy(coff[60..67], "public\x00");
    std.mem.writeInt(i16, coff[72..74], 1, .little);
    coff[76] = 2; // external definition
    std.mem.writeInt(u32, coff[78..82], 4, .little);
    @memcpy(coff[82..97], " /export:public");
    try tmp.dir.writeFile(io, .{ .sub_path = "coff.o", .data = &coff });
    const coff_path = try tmp.dir.realPathFileAlloc(io, "coff.o", a);
    const c = try inspect(a, io, &.{coff_path}, &.{"public"}, true);
    try std.testing.expect(c.all_inputs_scanned);
    try std.testing.expectEqual(@as(usize, 0), c.missing.len);
    try std.testing.expectEqual(@as(usize, 1), c.exports.len);
    try std.testing.expectEqualStrings("public", c.exports[0]);
}

test "wasm linking symbols are inspected without reading other sections" {
    const io = std.testing.io;
    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();
    const a = arena.allocator();
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const bytes = "\x00asm\x01\x00\x00\x00\x00\x14\x07linking\x02\x08\x09\x01\x00\x00\x00\x04wasm";
    try tmp.dir.writeFile(io, .{ .sub_path = "host.wasm", .data = bytes });
    const path = try tmp.dir.realPathFileAlloc(io, "host.wasm", a);
    const result = try inspect(a, io, &.{path}, &.{"wasm"}, false);
    try std.testing.expect(result.all_inputs_scanned);
    try std.testing.expectEqual(@as(usize, 0), result.missing.len);
}

test "required symbols decide whether an archive tail must be inspected" {
    var bytes: [8 + 60 + 512 + 3]u8 = @splat(0);
    @memcpy(bytes[0..8], "!<arch>\n");
    @memcpy(bytes[8..68], &try testArHeader(512));
    @memcpy(bytes[68..580], &testElf("found", true));
    @memcpy(bytes[580..], "bad");
    var remaining = std.StringHashMap(void).init(std.testing.allocator);
    defer remaining.deinit();
    try remaining.put("found", {});
    try std.testing.expect(try scanInput(&bytes, &remaining));
    try remaining.put("missing", {});
    try std.testing.expect(!try scanInput(&bytes, &remaining));
}

test "scanArArchive stops traversing members once every needed symbol is found" {
    // Keep the issue's original repro: an unrecognized second member must
    // not make a fully answered verification query non-authoritative.
    var bytes: [8 + 60 + 512 + 60 + 4]u8 = @splat(0);
    @memcpy(bytes[0..8], "!<arch>\n");
    @memcpy(bytes[8..68], &try testArHeader(512));
    @memcpy(bytes[68..580], &testElf(shim_symbols.roc_alloc, false));
    @memcpy(bytes[580..640], &try testArHeader(4));
    @memcpy(bytes[640..], "abcd");
    var remaining = std.StringHashMap(void).init(std.testing.allocator);
    defer remaining.deinit();
    try remaining.put(shim_symbols.roc_alloc, {});
    try std.testing.expect(try scanInput(&bytes, &remaining));
    try std.testing.expectEqual(@as(u32, 0), remaining.count());
}

test "verification stops before later symbol tables once its query is answered" {
    var bytes = testElf("found", false);
    const header = std.mem.bytesAsValue(std.elf.Elf64_Ehdr, bytes[0..64]);
    // A third, malformed table after the valid symbol. Move the valid symbol
    // and its strings past the expanded section-header table first.
    @memmove(bytes[256..320], bytes[192..256]);
    @memset(bytes[192..256], 0);
    header.e_shnum = 3;
    std.mem.bytesAsValue(std.elf.Elf64_Shdr, bytes[64..128]).sh_offset += 64;
    std.mem.bytesAsValue(std.elf.Elf64_Shdr, bytes[128..192]).sh_offset += 64;
    const bad = std.mem.bytesAsValue(std.elf.Elf64_Shdr, bytes[192..256]);
    bad.sh_type = std.elf.SHT_SYMTAB;
    bad.sh_link = 99;
    var remaining = std.StringHashMap(void).init(std.testing.allocator);
    defer remaining.deinit();
    try remaining.put("found", {});
    try std.testing.expect(try scanInput(&bytes, &remaining));
    try remaining.put("missing", {});
    try std.testing.expect(!try scanInput(&bytes, &remaining));
}

test "open inspection handles detect atomic replacement and survive mid-read truncation" {
    const io = std.testing.io;
    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    var storage = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer storage.deinit();
    var file = try tmp.dir.createFile(io, "host.o", .{ .read = true });
    defer file.close(io);
    try file.writePositionalAll(io, &testElf("original", false), 0);
    const before = try file.stat(io);
    var counters: Counters = .{};
    var source: FileSource = .{ .file = file, .io = io, .storage = &storage, .counters = &counters };
    const input: Input = .{ .source = &source, .offset = 0, .len = 512 };
    try std.testing.expectEqualStrings("\x7fELF", input.read(0, 4).?);
    // Publish while inspection owns the old handle. Reads stay on one inode;
    // the link owner's path-state guard detects the changed publication.
    try tmp.dir.writeFile(io, .{ .sub_path = "next.o", .data = &testElf("replaced", false) });
    try tmp.dir.rename("next.o", tmp.dir, "host.o", io);
    try std.testing.expect(!sameFileState(before, try tmp.dir.statFile(io, "host.o", .{})));
    try std.testing.expectEqualStrings("original", input.read(217, 8).?);
    // A writer that instead truncates the retained inode causes a short read,
    // not a fault or a complete verification result.
    try file.setLength(io, 4);
    try std.testing.expect(input.read(64, 64) == null);
    try std.testing.expect(source.failed);
    try std.testing.expect(!sameFileState(before, try file.stat(io)));
}
