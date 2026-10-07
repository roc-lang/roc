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
fn checkedSlice(bytes: []const u8, offset: usize, size: usize) ?[]const u8 {
    const end = std.math.add(usize, offset, size) catch return null;
    if (end > bytes.len) return null;
    return bytes[offset..end];
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
    /// Whether every host input was in a format the scanner understands.
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
    var remaining = std.StringHashMap(void).init(arena);
    for (needed) |symbol| {
        try remaining.put(symbol, {});
    }

    var all_scanned = true;

    for (host_input_paths) |path| {
        if (remaining.count() == 0) break;

        const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, arena, .limited(512 * 1024 * 1024)) catch {
            all_scanned = false;
            continue;
        };

        if (!scanInput(bytes, &remaining)) {
            all_scanned = false;
        }
    }

    var missing = std.ArrayList([]const u8).empty;
    var it = remaining.keyIterator();
    while (it.next()) |key| {
        try missing.append(arena, key.*);
    }
    std.mem.sort([]const u8, missing.items, {}, stringLessThan);

    return .{
        .missing = missing.items,
        .all_inputs_scanned = all_scanned,
    };
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
    var seen = std.StringHashMap(void).init(arena);
    var exports = std.ArrayList([]const u8).empty;

    for (host_input_paths) |path| {
        const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, arena, .limited(512 * 1024 * 1024)) catch continue;
        try collectInput(arena, bytes, &seen, &exports);
    }

    return exports.items;
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

fn collectInput(
    arena: Allocator,
    bytes: []const u8,
    seen: *std.StringHashMap(void),
    exports: *std.ArrayList([]const u8),
) Allocator.Error!void {
    const sink = ExportSink{ .arena = arena, .seen = seen, .exports = exports };
    if (std.mem.startsWith(u8, bytes, "!<arch>\n")) {
        try collectArArchive(bytes, sink);
        return;
    }
    try collectObject(bytes, sink);
}

/// Walk an ar archive's members and collect exports from each object member,
/// reusing the same member layout handling as scanArArchive.
fn collectArArchive(bytes: []const u8, sink: ExportSink) Allocator.Error!void {
    var offset: usize = "!<arch>\n".len;

    while (checkedSlice(bytes, offset, 60)) |header| {
        const name_field = std.mem.trimEnd(u8, header[0..16], " ");
        const size_field = std.mem.trimEnd(u8, header[48..58], " ");
        const member_size = std.fmt.parseInt(usize, size_field, 10) catch return;
        offset = std.math.add(usize, offset, 60) catch return;
        var member = checkedSlice(bytes, offset, member_size) orelse return;
        var member_name = name_field;

        if (std.mem.startsWith(u8, name_field, "#1/")) {
            const name_len = std.fmt.parseInt(usize, name_field[3..], 10) catch return;
            if (name_len > member.len) return;
            member_name = std.mem.trimEnd(u8, member[0..name_len], "\x00");
            member = member[name_len..];
        }

        const is_index = std.mem.eql(u8, member_name, "/") or
            std.mem.eql(u8, member_name, "//") or
            std.mem.eql(u8, member_name, "/SYM64/") or
            std.mem.startsWith(u8, member_name, "__.SYMDEF");

        if (!is_index and member.len > 0) {
            try collectObject(member, sink);
        }

        offset = std.math.add(usize, offset, member_size) catch return;
        offset = std.math.add(usize, offset, offset & 1) catch return; // members are 2-byte aligned
    }
}

fn collectObject(bytes: []const u8, sink: ExportSink) Allocator.Error!void {
    if (bytes.len >= 4) {
        const magic = std.mem.readInt(u32, bytes[0..4], .little);
        if (std.mem.eql(u8, bytes[0..4], "\x7fELF")) return collectElfObject(bytes, sink);
        if (magic == std.macho.MH_MAGIC_64) return collectMachoObject(bytes, sink);
        if (std.mem.eql(u8, bytes[0..4], "\x00asm")) return; // wasm handled elsewhere
    }
    if (bytes.len >= 2) {
        const machine = std.mem.readInt(u16, bytes[0..2], .little);
        if (machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.AMD64) or
            machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.ARM64) or
            machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.I386))
        {
            return collectCoffObject(bytes, sink);
        }
    }
}

fn collectElfObject(bytes: []const u8, sink: ExportSink) Allocator.Error!void {
    const elf = std.elf;
    if (bytes.len < @sizeOf(elf.Elf64_Ehdr)) return;
    const ehdr = std.mem.bytesAsValue(elf.Elf64_Ehdr, bytes[0..@sizeOf(elf.Elf64_Ehdr)]);
    if (ehdr.e_ident[elf.EI_CLASS] != elf.ELFCLASS64) return;

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

fn collectMachoObject(bytes: []const u8, sink: ExportSink) Allocator.Error!void {
    const macho = std.macho;
    if (bytes.len < @sizeOf(macho.mach_header_64)) return;
    const header = std.mem.bytesAsValue(macho.mach_header_64, bytes[0..@sizeOf(macho.mach_header_64)]);

    var offset: usize = @sizeOf(macho.mach_header_64);
    var cmd_index: u32 = 0;
    while (cmd_index < header.ncmds) : (cmd_index += 1) {
        const cmd_bytes = checkedSlice(bytes, offset, @sizeOf(macho.load_command)) orelse return;
        const cmd = std.mem.bytesAsValue(macho.load_command, cmd_bytes);
        if (cmd.cmdsize < @sizeOf(macho.load_command)) return;
        const full_cmd = checkedSlice(bytes, offset, cmd.cmdsize) orelse return;
        if (cmd.cmd == .SYMTAB) {
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
fn collectCoffObject(bytes: []const u8, sink: ExportSink) Allocator.Error!void {
    if (bytes.len < 20) return;
    const num_sections: usize = std.mem.readInt(u16, bytes[2..4], .little);
    const optional_header_size: usize = std.mem.readInt(u16, bytes[16..18], .little);
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
fn scanInput(bytes: []const u8, remaining: *std.StringHashMap(void)) bool {
    if (std.mem.startsWith(u8, bytes, "!<arch>\n")) {
        return scanArArchive(bytes, remaining);
    }
    return scanObject(bytes, remaining);
}

fn scanObject(bytes: []const u8, remaining: *std.StringHashMap(void)) bool {
    if (bytes.len >= 4) {
        const magic = std.mem.readInt(u32, bytes[0..4], .little);
        if (bytes.len >= 4 and std.mem.eql(u8, bytes[0..4], "\x7fELF")) {
            return scanElfObject(bytes, remaining);
        }
        if (magic == std.macho.MH_MAGIC_64) {
            return scanMachoObject(bytes, remaining);
        }
        if (std.mem.eql(u8, bytes[0..4], "\x00asm")) {
            return scanWasmObject(bytes, remaining);
        }
    }
    if (bytes.len >= 2) {
        const machine = std.mem.readInt(u16, bytes[0..2], .little);
        if (machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.AMD64) or
            machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.ARM64) or
            machine == @backingInt(std.coff.IMAGE.FILE.MACHINE.I386))
        {
            return scanCoffObject(bytes, remaining);
        }
    }
    return false;
}

/// Walk an ar archive's members and scan each object member. The symbol
/// index members ("/", "//", "__.SYMDEF"...) are metadata, not objects;
/// scanning the members directly handles every index flavor uniformly.
fn scanArArchive(bytes: []const u8, remaining: *std.StringHashMap(void)) bool {
    var offset: usize = "!<arch>\n".len;
    var all_scanned = true;

    while (checkedSlice(bytes, offset, 60)) |header| {
        const name_field = std.mem.trimEnd(u8, header[0..16], " ");
        const size_field = std.mem.trimEnd(u8, header[48..58], " ");
        const member_size = std.fmt.parseInt(usize, size_field, 10) catch return false;
        offset = std.math.add(usize, offset, 60) catch return false;
        var member = checkedSlice(bytes, offset, member_size) orelse return false;
        var member_name = name_field;

        // BSD ar stores long names inline at the start of the member data.
        if (std.mem.startsWith(u8, name_field, "#1/")) {
            const name_len = std.fmt.parseInt(usize, name_field[3..], 10) catch return false;
            if (name_len > member.len) return false;
            member_name = std.mem.trimEnd(u8, member[0..name_len], "\x00");
            member = member[name_len..];
        }

        const is_index = std.mem.eql(u8, member_name, "/") or
            std.mem.eql(u8, member_name, "//") or
            std.mem.eql(u8, member_name, "/SYM64/") or
            std.mem.startsWith(u8, member_name, "__.SYMDEF");

        if (!is_index and member.len > 0) {
            if (!scanObject(member, remaining)) {
                all_scanned = false;
            }
        }

        offset = std.math.add(usize, offset, member_size) catch return false;
        offset = std.math.add(usize, offset, offset & 1) catch return false; // members are 2-byte aligned
    }

    return all_scanned;
}

fn scanElfObject(bytes: []const u8, remaining: *std.StringHashMap(void)) bool {
    const elf = std.elf;
    if (bytes.len < @sizeOf(elf.Elf64_Ehdr)) return false;
    const ehdr = std.mem.bytesAsValue(elf.Elf64_Ehdr, bytes[0..@sizeOf(elf.Elf64_Ehdr)]);
    if (ehdr.e_ident[elf.EI_CLASS] != elf.ELFCLASS64) return false;

    const shoff = std.math.cast(usize, ehdr.e_shoff) orelse return false;
    const shnum: usize = ehdr.e_shnum;
    const shentsize: usize = ehdr.e_shentsize;
    if (shnum == 0 or shentsize < @sizeOf(elf.Elf64_Shdr)) return false; // extended section numbering is unsupported
    const section_bytes = checkedTableSize(shnum, shentsize) orelse return false;
    const sections = checkedSlice(bytes, shoff, section_bytes) orelse return false;

    var i: usize = 0;
    while (i < shnum) : (i += 1) {
        const shdr = std.mem.bytesAsValue(elf.Elf64_Shdr, sections[i * shentsize ..][0..@sizeOf(elf.Elf64_Shdr)]);
        if (shdr.sh_type != elf.SHT_SYMTAB) continue;

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

fn scanMachoObject(bytes: []const u8, remaining: *std.StringHashMap(void)) bool {
    const macho = std.macho;
    if (bytes.len < @sizeOf(macho.mach_header_64)) return false;
    const header = std.mem.bytesAsValue(macho.mach_header_64, bytes[0..@sizeOf(macho.mach_header_64)]);

    var offset: usize = @sizeOf(macho.mach_header_64);
    var cmd_index: u32 = 0;
    while (cmd_index < header.ncmds) : (cmd_index += 1) {
        const cmd_bytes = checkedSlice(bytes, offset, @sizeOf(macho.load_command)) orelse return false;
        const cmd = std.mem.bytesAsValue(macho.load_command, cmd_bytes);
        if (cmd.cmdsize < @sizeOf(macho.load_command)) return false;
        const full_cmd = checkedSlice(bytes, offset, cmd.cmdsize) orelse return false;
        if (cmd.cmd == .SYMTAB) {
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

fn scanCoffObject(bytes: []const u8, remaining: *std.StringHashMap(void)) bool {
    // COFF object header: Machine(2) NumberOfSections(2) TimeDateStamp(4)
    // PointerToSymbolTable(4) NumberOfSymbols(4) SizeOfOptionalHeader(2)
    // Characteristics(2)
    if (bytes.len < 20) return false;
    const symtab_offset: usize = std.mem.readInt(u32, bytes[8..12], .little);
    const symbol_count: usize = std.mem.readInt(u32, bytes[12..16], .little);
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
fn scanWasmObject(bytes: []const u8, remaining: *std.StringHashMap(void)) bool {
    if (bytes.len < 8) return false;
    var offset: usize = 8; // magic + version

    while (offset < bytes.len) {
        const section_id = bytes[offset];
        offset += 1;
        const section_size = readLeb32(bytes, &offset) orelse return false;
        const section_end = std.math.add(usize, offset, section_size) catch return false;
        if (section_end > bytes.len) return false;

        if (section_id == 0) {
            // Custom section: name then payload.
            var pos = offset;
            const name_len = readLeb32(bytes, &pos) orelse return false;
            const name = checkedSlice(bytes[0..section_end], pos, name_len) orelse return false;
            pos = std.math.add(usize, pos, name_len) catch return false;
            if (std.mem.eql(u8, name, "linking")) {
                if (!scanWasmLinking(bytes[pos..section_end], remaining)) return false;
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
        const subsection_type = payload[pos];
        pos += 1;
        const subsection_size = readLeb32(payload, &pos) orelse return false;
        const subsection_end = std.math.add(usize, pos, subsection_size) catch return false;
        if (subsection_end > payload.len) return false;

        if (subsection_type == 8) { // WASM_SYMBOL_TABLE
            const count = readLeb32(payload, &pos) orelse return false;
            var i: usize = 0;
            while (i < count) : (i += 1) {
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

fn readLeb32(bytes: []const u8, pos: *usize) ?usize {
    var result: u32 = 0;
    var shift: u5 = 0;
    while (pos.* < bytes.len) {
        const byte = bytes[pos.*];
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
    try std.testing.expect(!scanArArchive(buf.items, &remaining));
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
    try std.testing.expect(scanInput(&base, &remaining));

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
            else => unreachable,
        }
        try std.testing.expect(!scanInput(&bad, &remaining));
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
    try std.testing.expect(scanInput(&base, &remaining));
    for (0..4) |case_index| {
        var bad = base;
        const bad_command = std.mem.bytesAsValue(macho.symtab_command, bad[header_size..][0..command_size]);
        switch (case_index) {
            0 => bad_command.cmdsize = 0,
            1 => bad_command.cmdsize = @sizeOf(macho.load_command),
            2 => bad_command.symoff = std.math.maxInt(u32),
            3 => bad_command.stroff = std.math.maxInt(u32),
            else => unreachable,
        }
        try std.testing.expect(!scanInput(&bad, &remaining));
    }

    var bad_name = base;
    const nlist_off = header_size + command_size;
    const nlist = std.mem.bytesAsValue(macho.nlist_64, bad_name[nlist_off..][0..@sizeOf(macho.nlist_64)]);
    nlist.n_type.bits.ext = true;
    nlist.n_type.bits.type = .sect;
    nlist.n_strx = std.math.maxInt(u32);
    try remaining.put("missing", {});
    try std.testing.expect(scanInput(&bad_name, &remaining));
    try std.testing.expect(remaining.contains("missing"));
}
