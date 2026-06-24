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

const Allocator = std.mem.Allocator;

/// The fixed runtime symbols every symbol-ABI host defines.
pub const runtime_symbols = [_][]const u8{
    "roc_alloc",
    "roc_dealloc",
    "roc_realloc",
    "roc_dbg",
    "roc_expect_failed",
    "roc_crashed",
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
///     carries no visibility, so the directive — what `__declspec(dllexport)`
///     emits — is the only export signal).
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

    while (offset + 60 <= bytes.len) {
        const header = bytes[offset .. offset + 60];
        const name_field = std.mem.trimEnd(u8, header[0..16], " ");
        const size_field = std.mem.trimEnd(u8, header[48..58], " ");
        const member_size = std.fmt.parseInt(usize, size_field, 10) catch return;
        offset += 60;
        if (offset + member_size > bytes.len) return;

        var member = bytes[offset .. offset + member_size];
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

        offset += member_size;
        if (offset % 2 == 1) offset += 1; // members are 2-byte aligned
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
        if (machine == @intFromEnum(std.coff.IMAGE.FILE.MACHINE.AMD64) or
            machine == @intFromEnum(std.coff.IMAGE.FILE.MACHINE.ARM64) or
            machine == @intFromEnum(std.coff.IMAGE.FILE.MACHINE.I386))
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

    const shoff: usize = @intCast(ehdr.e_shoff);
    const shnum: usize = ehdr.e_shnum;
    const shentsize: usize = ehdr.e_shentsize;
    if (shentsize < @sizeOf(elf.Elf64_Shdr)) return;
    if (shoff + shnum * shentsize > bytes.len) return;

    var i: usize = 0;
    while (i < shnum) : (i += 1) {
        const shdr = std.mem.bytesAsValue(elf.Elf64_Shdr, bytes[shoff + i * shentsize ..][0..@sizeOf(elf.Elf64_Shdr)]);
        if (shdr.sh_type != elf.SHT_SYMTAB) continue;

        const strtab_index: usize = shdr.sh_link;
        if (strtab_index >= shnum) return;
        const strtab_hdr = std.mem.bytesAsValue(elf.Elf64_Shdr, bytes[shoff + strtab_index * shentsize ..][0..@sizeOf(elf.Elf64_Shdr)]);
        const strtab_off: usize = @intCast(strtab_hdr.sh_offset);
        const strtab_size: usize = @intCast(strtab_hdr.sh_size);
        if (strtab_off + strtab_size > bytes.len) return;
        const strtab = bytes[strtab_off .. strtab_off + strtab_size];

        const sym_off: usize = @intCast(shdr.sh_offset);
        const sym_size: usize = @intCast(shdr.sh_size);
        if (sym_off + sym_size > bytes.len) return;
        const sym_count = sym_size / @sizeOf(elf.Elf64_Sym);

        var s: usize = 0;
        while (s < sym_count) : (s += 1) {
            const sym = std.mem.bytesAsValue(elf.Elf64_Sym, bytes[sym_off + s * @sizeOf(elf.Elf64_Sym) ..][0..@sizeOf(elf.Elf64_Sym)]);
            if (sym.st_shndx == elf.SHN_UNDEF) continue;
            const binding = sym.st_info >> 4;
            if (binding != elf.STB_GLOBAL and binding != elf.STB_WEAK) continue;
            // Only DEFAULT or PROTECTED visibility symbols are exported from a
            // shared object; HIDDEN/INTERNAL are the host's internals.
            const visibility: u3 = @intCast(sym.st_other & 0x3);
            if (visibility != @intFromEnum(elf.STV.DEFAULT) and visibility != @intFromEnum(elf.STV.PROTECTED)) continue;
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
        if (offset + @sizeOf(macho.load_command) > bytes.len) return;
        const cmd = std.mem.bytesAsValue(macho.load_command, bytes[offset..][0..@sizeOf(macho.load_command)]);
        if (cmd.cmd == .SYMTAB) {
            if (offset + @sizeOf(macho.symtab_command) > bytes.len) return;
            const symtab = std.mem.bytesAsValue(macho.symtab_command, bytes[offset..][0..@sizeOf(macho.symtab_command)]);

            const str_off: usize = symtab.stroff;
            const str_size: usize = symtab.strsize;
            if (str_off + str_size > bytes.len) return;
            const strtab = bytes[str_off .. str_off + str_size];

            const sym_off: usize = symtab.symoff;
            const sym_count: usize = symtab.nsyms;
            if (sym_off + sym_count * @sizeOf(macho.nlist_64) > bytes.len) return;

            var s: usize = 0;
            while (s < sym_count) : (s += 1) {
                const nlist = std.mem.bytesAsValue(macho.nlist_64, bytes[sym_off + s * @sizeOf(macho.nlist_64) ..][0..@sizeOf(macho.nlist_64)]);
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
        offset += cmd.cmdsize;
    }
}

/// Collect `/export:` operands from a COFF object's `.drectve` section. This is
/// the only place COFF records export intent (the symbol table has no
/// visibility), and it is exactly what `__declspec(dllexport)` emits.
fn collectCoffObject(bytes: []const u8, sink: ExportSink) Allocator.Error!void {
    if (bytes.len < 20) return;
    const num_sections: usize = std.mem.readInt(u16, bytes[2..4], .little);
    const optional_header_size: usize = std.mem.readInt(u16, bytes[16..18], .little);
    const section_table_off = 20 + optional_header_size;
    const section_header_size = 40;
    if (section_table_off + num_sections * section_header_size > bytes.len) return;

    var i: usize = 0;
    while (i < num_sections) : (i += 1) {
        const sh = bytes[section_table_off + i * section_header_size ..][0..section_header_size];
        const name = std.mem.sliceTo(sh[0..8], 0);
        if (!std.mem.eql(u8, name, ".drectve")) continue;

        const size_of_raw_data: usize = std.mem.readInt(u32, sh[16..20], .little);
        const ptr_to_raw_data: usize = std.mem.readInt(u32, sh[20..24], .little);
        if (ptr_to_raw_data == 0 or size_of_raw_data == 0) continue;
        if (ptr_to_raw_data + size_of_raw_data > bytes.len) continue;

        const text = bytes[ptr_to_raw_data .. ptr_to_raw_data + size_of_raw_data];
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
        if (machine == @intFromEnum(std.coff.IMAGE.FILE.MACHINE.AMD64) or
            machine == @intFromEnum(std.coff.IMAGE.FILE.MACHINE.ARM64) or
            machine == @intFromEnum(std.coff.IMAGE.FILE.MACHINE.I386))
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

    while (offset + 60 <= bytes.len) {
        const header = bytes[offset .. offset + 60];
        const name_field = std.mem.trimEnd(u8, header[0..16], " ");
        const size_field = std.mem.trimEnd(u8, header[48..58], " ");
        const member_size = std.fmt.parseInt(usize, size_field, 10) catch return false;
        offset += 60;
        if (offset + member_size > bytes.len) return false;

        var member = bytes[offset .. offset + member_size];
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

        offset += member_size;
        if (offset % 2 == 1) offset += 1; // members are 2-byte aligned
    }

    return all_scanned;
}

fn scanElfObject(bytes: []const u8, remaining: *std.StringHashMap(void)) bool {
    const elf = std.elf;
    if (bytes.len < @sizeOf(elf.Elf64_Ehdr)) return false;
    const ehdr = std.mem.bytesAsValue(elf.Elf64_Ehdr, bytes[0..@sizeOf(elf.Elf64_Ehdr)]);
    if (ehdr.e_ident[elf.EI_CLASS] != elf.ELFCLASS64) return false;

    const shoff: usize = @intCast(ehdr.e_shoff);
    const shnum: usize = ehdr.e_shnum;
    const shentsize: usize = ehdr.e_shentsize;
    if (shentsize < @sizeOf(elf.Elf64_Shdr)) return false;
    if (shoff + shnum * shentsize > bytes.len) return false;

    var i: usize = 0;
    while (i < shnum) : (i += 1) {
        const shdr = std.mem.bytesAsValue(elf.Elf64_Shdr, bytes[shoff + i * shentsize ..][0..@sizeOf(elf.Elf64_Shdr)]);
        if (shdr.sh_type != elf.SHT_SYMTAB) continue;

        const strtab_index: usize = shdr.sh_link;
        if (strtab_index >= shnum) return false;
        const strtab_hdr = std.mem.bytesAsValue(elf.Elf64_Shdr, bytes[shoff + strtab_index * shentsize ..][0..@sizeOf(elf.Elf64_Shdr)]);
        const strtab_off: usize = @intCast(strtab_hdr.sh_offset);
        const strtab_size: usize = @intCast(strtab_hdr.sh_size);
        if (strtab_off + strtab_size > bytes.len) return false;
        const strtab = bytes[strtab_off .. strtab_off + strtab_size];

        const sym_off: usize = @intCast(shdr.sh_offset);
        const sym_size: usize = @intCast(shdr.sh_size);
        if (sym_off + sym_size > bytes.len) return false;
        const sym_count = sym_size / @sizeOf(elf.Elf64_Sym);

        var s: usize = 0;
        while (s < sym_count) : (s += 1) {
            const sym = std.mem.bytesAsValue(elf.Elf64_Sym, bytes[sym_off + s * @sizeOf(elf.Elf64_Sym) ..][0..@sizeOf(elf.Elf64_Sym)]);
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
        if (offset + @sizeOf(macho.load_command) > bytes.len) return false;
        const cmd = std.mem.bytesAsValue(macho.load_command, bytes[offset..][0..@sizeOf(macho.load_command)]);
        if (cmd.cmd == .SYMTAB) {
            if (offset + @sizeOf(macho.symtab_command) > bytes.len) return false;
            const symtab = std.mem.bytesAsValue(macho.symtab_command, bytes[offset..][0..@sizeOf(macho.symtab_command)]);

            const str_off: usize = symtab.stroff;
            const str_size: usize = symtab.strsize;
            if (str_off + str_size > bytes.len) return false;
            const strtab = bytes[str_off .. str_off + str_size];

            const sym_off: usize = symtab.symoff;
            const sym_count: usize = symtab.nsyms;
            if (sym_off + sym_count * @sizeOf(macho.nlist_64) > bytes.len) return false;

            var s: usize = 0;
            while (s < sym_count) : (s += 1) {
                const nlist = std.mem.bytesAsValue(macho.nlist_64, bytes[sym_off + s * @sizeOf(macho.nlist_64) ..][0..@sizeOf(macho.nlist_64)]);
                if (nlist.n_type.bits.is_stab != 0) continue;
                if (!nlist.n_type.bits.ext) continue;
                if (nlist.n_type.bits.type != .sect) continue;
                const name_off: usize = nlist.n_strx;
                if (name_off >= strtab.len) continue;
                const name = std.mem.sliceTo(strtab[name_off..], 0);
                if (name.len > 0) defineSymbol(remaining, name);
            }
        }
        offset += cmd.cmdsize;
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
    if (symtab_offset + symbol_count * symbol_size > bytes.len) return false;

    // The string table immediately follows the symbol table; its first four
    // bytes are its total size (including those bytes).
    const strtab_offset = symtab_offset + symbol_count * symbol_size;
    var strtab: []const u8 = &.{};
    if (strtab_offset + 4 <= bytes.len) {
        const strtab_size: usize = std.mem.readInt(u32, bytes[strtab_offset..][0..4], .little);
        if (strtab_size >= 4 and strtab_offset + strtab_size <= bytes.len) {
            strtab = bytes[strtab_offset .. strtab_offset + strtab_size];
        }
    }

    var s: usize = 0;
    while (s < symbol_count) : (s += 1) {
        const record = bytes[symtab_offset + s * symbol_size ..][0..symbol_size];
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
        const section_end = offset + section_size;
        if (section_end > bytes.len) return false;

        if (section_id == 0) {
            // Custom section: name then payload.
            var pos = offset;
            const name_len = readLeb32(bytes, &pos) orelse return false;
            if (pos + name_len > section_end) return false;
            const name = bytes[pos .. pos + name_len];
            pos += name_len;
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
        const subsection_end = pos + subsection_size;
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
                            if (pos + name_len > subsection_end) return false;
                            const name = payload[pos .. pos + name_len];
                            pos += name_len;
                            if (!undefined_flag) defineSymbol(remaining, name);
                        }
                    },
                    // data: name, then segment/offset/size when defined.
                    1 => {
                        const name_len = readLeb32(payload, &pos) orelse return false;
                        if (pos + name_len > subsection_end) return false;
                        const name = payload[pos .. pos + name_len];
                        pos += name_len;
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
        result |= @as(u32, byte & 0x7f) << shift;
        if (byte & 0x80 == 0) return result;
        if (shift >= 28) return null;
        shift += 7;
    }
    return null;
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
    try remaining.put("roc_alloc", {});

    // The member isn't a recognized object format, so the scan reports
    // non-authoritative.
    try std.testing.expect(!scanArArchive(buf.items, &remaining));
    try std.testing.expectEqual(@as(u32, 1), remaining.count());
}

test "defineSymbol strips one leading underscore" {
    var remaining = std.StringHashMap(void).init(std.testing.allocator);
    defer remaining.deinit();
    try remaining.put("roc_alloc", {});
    defineSymbol(&remaining, "_roc_alloc");
    try std.testing.expectEqual(@as(u32, 0), remaining.count());
}
