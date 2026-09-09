//! Build-time preparation and verification of the machine-code shim's complete symbol contract.
//! This tool is a dependency of embedding/installing the archive, never a part
//! of Roc's runtime. Unknown imports and non-interface exports are errors even
//! when weak or hidden: a platform must not supply or interpose Roc internals.
const std = @import("std");
const symbols = @import("shim_symbols");
const Os = enum { linux, macos, windows };

const ArchiveError = std.fmt.ParseIntError || error{
    DuplicateExport,
    UnexpectedExport,
    UnexpectedImport,
    NoObjectMembers,
    MissingExport,
    MalformedObject,
    NotAnArchive,
    MalformedArchive,
    NotElf,
    UnsupportedElf,
    NotRelocatable,
    MissingSymbolTable,
    NotMachO,
    MalformedSymbol,
    IndirectSymbol,
    UnsupportedCoff,
    CoffCompositionRequiresOneObject,
    UnsupportedCoffDefinition,
};
const PrepareError = ArchiveError || std.mem.Allocator.Error || error{NoSpaceLeft};
const MainError = PrepareError || std.process.Args.ToSliceError ||
    std.Io.Dir.ReadFileAllocError || std.Io.Dir.WriteFileError || error{
    ExpectedOsInputAndOutputPaths,
    UnsupportedTarget,
};

const exports = [_][]const u8{
    symbols.roc_shim_get_ops,
    symbols.roc_entrypoint,
    symbols.roc_shim_default_main,
};
const imports = symbols.runtime_set ++ .{
    symbols.roc_shim_hosted_fns,
    symbols.roc_shim_hosted_count,
};

// Explicit OS ABI dependencies, not symbols discovered from the built archive.
// These standard C memory/math operations are also emitted as libcalls by
// Zig's backends. They belong to the platform C ABI, unlike __clear_cache and
// compiler-rt arithmetic helpers. IO itself uses direct syscalls on Linux.
const c_imports = [_][]const u8{
    "memcpy", "memmove", "memset", "strlen", "sqrt", "sqrtf", "ceil", "ceilf", "floor", "floorf",
    "fmod",   "fmodf",   "trunc",  "truncf",
};
const linux_imports = c_imports ++ .{"__tls_get_addr"};
const darwin_imports = [_][]const u8{
    // libSystem's IO, mapping, synchronization, TLS and stack-protector ABI.
    "bzero",                 "_NSGetExecutablePath", "__bzero",                "__error",               "__stack_chk_fail",
    "__stack_chk_guard",     "__ulock_wait",         "__ulock_wake",           "_tlv_bootstrap",        "abort",
    "close",                 "close$NOCANCEL",       "mmap",                   "mprotect",              "munmap",
    "openat",                "os_unfair_lock_lock",  "os_unfair_lock_trylock", "os_unfair_lock_unlock", "pread",
    "pthread_threadid_np",   "read",                 "sigaction",              "sigemptyset",           "write",
    "sys_icache_invalidate",
};
const windows_imports = c_imports ++ .{
    // Kernel32/ntdll APIs used by the mapping and minimal std.Io implementations.
    "CloseHandle",                       "CreateFileW",                "FlushInstructionCache",   "GetLastError",
    "GetModuleFileNameW",                "MapViewOfFile",              "NtAllocateVirtualMemory", "NtFreeVirtualMemory",
    "ReadFile",                          "RtlAcquireSRWLockExclusive", "RtlExitUserProcess",      "RtlReleaseSRWLockExclusive",
    "RtlTryAcquireSRWLockExclusive",     "RtlWaitOnAddress",           "RtlWakeAddressAll",       "RtlWakeAddressSingle",
    "UnmapViewOfFile",                   "VirtualProtect",             "WriteFile",               "MapViewOfFileEx",
    "RtlRemoveVectoredExceptionHandler",
    // The Windows CRT initializes the module's native TLS index.
    "_tls_index",                 "_fltused",
};
const Check = struct {
    os: Os,
    found: [exports.len]bool = @splat(false),
    members: usize = 0,

    fn symbol(self: *Check, name: []const u8, defined: bool, global: bool) ArchiveError!void {
        if (!global) return;
        if (defined) {
            for (exports, 0..) |expected, i| {
                if (std.mem.eql(u8, name, expected)) {
                    if (self.found[i]) return error.DuplicateExport;
                    self.found[i] = true;
                    return;
                }
            }
            if (!@import("builtin").is_test) std.debug.print("unexpected global definition: {s}\n", .{name});
            return error.UnexpectedExport;
        }
        if (contains(&imports, name)) return;
        const permitted = switch (self.os) {
            .linux => contains(&linux_imports, name),
            .macos => contains(&darwin_imports, name) or contains(&c_imports, name),
            .windows => contains(&windows_imports, if (std.mem.startsWith(u8, name, "__imp_")) name[6..] else name),
        };
        if (!permitted) {
            if (!@import("builtin").is_test) std.debug.print("unexpected external dependency: {s}\n", .{name});
            return error.UnexpectedImport;
        }
    }

    fn finish(self: *const Check) ArchiveError!void {
        if (self.members == 0) return error.NoObjectMembers;
        for (exports, self.found) |name, found| {
            if (!found) {
                if (!@import("builtin").is_test) std.debug.print("missing shim export: {s}\n", .{name});
                return error.MissingExport;
            }
        }
    }
};

fn contains(names: []const []const u8, name: []const u8) bool {
    for (names) |expected| if (std.mem.eql(u8, expected, name)) return true;
    return false;
}

/// Prepare and verify an input archive, writing output only after its contract passes.
pub fn main(init: std.process.Init) MainError!void {
    const gpa = init.gpa;
    const args = try init.minimal.args.toSlice(init.arena.allocator());
    if (args.len != 4) return error.ExpectedOsInputAndOutputPaths;
    const os = std.meta.stringToEnum(Os, args[1]) orelse return error.UnsupportedTarget;
    const bytes = try std.Io.Dir.cwd().readFileAlloc(init.io, args[2], gpa, .unlimited);
    defer gpa.free(bytes);
    const prepared = if (os == .windows) try prepareCoffArchive(gpa, bytes) else bytes;
    defer if (prepared.ptr != bytes.ptr) gpa.free(prepared);
    var check: Check = .{ .os = os };
    scanArchive(prepared, &check) catch |err| {
        std.debug.print("FAILED: {s}: {s}\n", .{ args[2], @errorName(err) });
        return err;
    };
    try check.finish();
    try std.Io.Dir.cwd().writeFile(init.io, .{ .sub_path = args[3], .data = prepared });
}

// All byte access is checked. Truncated files, missing symbol tables, unknown
// archive members and unsupported encodings must fail closed, never pass a
// vacuous check. Shipped native targets are 64-bit little endian.
fn slice(bytes: []const u8, offset: usize, len: usize) error{MalformedObject}![]const u8 {
    if (offset > bytes.len or len > bytes.len - offset) return error.MalformedObject;
    return bytes[offset..][0..len];
}
fn int(comptime T: type, bytes: []const u8, offset: usize) error{MalformedObject}!T {
    return std.mem.readInt(T, (try slice(bytes, offset, @sizeOf(T)))[0..@sizeOf(T)], .little);
}
fn string(bytes: []const u8, offset: usize) error{MalformedObject}![]const u8 {
    const rest = try slice(bytes, offset, bytes.len -| offset);
    const len = std.mem.findScalar(u8, rest, 0) orelse return error.MalformedObject;
    return rest[0..len];
}

fn scanArchive(bytes: []const u8, check: *Check) ArchiveError!void {
    try walkArchive(bytes, check.os, check, struct {
        fn visit(state: *Check, member: []const u8) ArchiveError!void {
            switch (state.os) {
                .linux => try scanElf(member, state),
                .macos => try scanMachO(member, state),
                .windows => try scanCoff(member, state),
            }
            state.members += 1;
        }
    }.visit);
}

fn walkArchive(bytes: []const u8, os: Os, context: anytype, comptime visit: anytype) ArchiveError!void {
    if (!std.mem.startsWith(u8, bytes, "!<arch>\n")) return error.NotAnArchive;
    var offset: usize = 8;
    var long_names: []const u8 = &.{};
    while (offset < bytes.len) {
        const header = try slice(bytes, offset, 60);
        if (!std.mem.eql(u8, header[58..60], "`\n")) return error.MalformedArchive;
        var name = std.mem.trimEnd(u8, header[0..16], " ");
        const size = try std.fmt.parseInt(usize, std.mem.trimEnd(u8, header[48..58], " "), 10);
        offset += 60;
        var member = try slice(bytes, offset, size);
        if (std.mem.startsWith(u8, name, "#1/")) {
            const len = try std.fmt.parseInt(usize, name[3..], 10);
            name = std.mem.trimEnd(u8, try slice(member, 0, len), "\x00");
            member = member[len..];
        } else if (std.mem.eql(u8, name, "//")) {
            long_names = member;
        } else if (name.len > 1 and name[0] == '/' and std.ascii.isDigit(name[1])) {
            const start = try std.fmt.parseInt(usize, name[1..], 10);
            const rest = try slice(long_names, start, long_names.len -| start);
            const terminator: u8 = if (os == .windows) 0 else '\n';
            const len = std.mem.findScalar(u8, rest, terminator) orelse return error.MalformedArchive;
            name = std.mem.trimEnd(u8, rest[0..len], "/");
        }
        const index = std.mem.eql(u8, name, "/") or std.mem.eql(u8, name, "//") or
            std.mem.eql(u8, name, "/SYM64/") or std.mem.eql(u8, name, "__.SYMDEF") or
            std.mem.eql(u8, name, "__.SYMDEF SORTED") or std.mem.eql(u8, name, "__.SYMDEF_64") or
            std.mem.eql(u8, name, "__.SYMDEF_64 SORTED");
        if (!index) try visit(context, member);
        offset += size;
        if (offset % 2 != 0) {
            _ = try slice(bytes, offset, 1);
            offset += 1;
        }
    }
}

fn scanElf(bytes: []const u8, check: *Check) ArchiveError!void {
    if (!std.mem.startsWith(u8, bytes, "\x7fELF")) return error.NotElf;
    if (try int(u8, bytes, 4) != 2 or try int(u8, bytes, 5) != 1) return error.UnsupportedElf;
    if (try int(u16, bytes, 16) != @intFromEnum(std.elf.ET.REL)) return error.NotRelocatable;
    const shoff = try int(u64, bytes, 40);
    const shsize = try int(u16, bytes, 58);
    const shnum = try int(u16, bytes, 60);
    if (shsize != 64 or shnum == 0) return error.MalformedObject;
    const sections = try slice(bytes, shoff, @as(usize, shnum) * shsize);
    var found_table = false;
    for (0..shnum) |i| {
        const sh = sections[i * shsize ..][0..64];
        if (try int(u32, sh, 4) != std.elf.SHT_SYMTAB) continue;
        found_table = true;
        const link = try int(u32, sh, 40);
        if (link >= shnum) return error.MalformedObject;
        const strsh = sections[@as(usize, link) * shsize ..][0..64];
        if (try int(u32, strsh, 4) != std.elf.SHT_STRTAB) return error.MalformedObject;
        const strings = try slice(bytes, try int(u64, strsh, 24), try int(u64, strsh, 32));
        if (try int(u64, sh, 56) != 24) return error.MalformedObject;
        const table = try slice(bytes, try int(u64, sh, 24), try int(u64, sh, 32));
        if (table.len == 0 or table.len % 24 != 0) return error.MalformedObject;
        for (0..table.len / 24) |s| {
            const sym = table[s * 24 ..][0..24];
            const bind = sym[4] >> 4;
            if (bind == std.elf.STB_LOCAL) continue;
            const name = try string(strings, try int(u32, sym, 0));
            try check.symbol(name, try int(u16, sym, 6) != std.elf.SHN_UNDEF, true);
        }
    }
    if (!found_table) return error.MissingSymbolTable;
}

fn scanMachO(bytes: []const u8, check: *Check) ArchiveError!void {
    if (try int(u32, bytes, 0) != std.macho.MH_MAGIC_64) return error.NotMachO;
    if (try int(u32, bytes, 12) != std.macho.MH_OBJECT) return error.NotRelocatable;
    const count = try int(u32, bytes, 16);
    const commands = try slice(bytes, 32, try int(u32, bytes, 20));
    var offset: usize = 0;
    var found_table = false;
    for (0..count) |_| {
        const cmd = try int(u32, commands, offset);
        const size = try int(u32, commands, offset + 4);
        if (size < 8) return error.MalformedObject;
        const command = try slice(commands, offset, size);
        if (cmd == @intFromEnum(std.macho.LC.SYMTAB)) {
            found_table = true;
            const table = try slice(bytes, try int(u32, command, 8), @as(usize, try int(u32, command, 12)) * 16);
            const strings = try slice(bytes, try int(u32, command, 16), try int(u32, command, 20));
            if (table.len == 0) return error.MissingSymbolTable;
            for (0..table.len / 16) |i| {
                const sym = table[i * 16 ..][0..16];
                const kind = sym[4];
                if (kind & 0xe0 != 0 or kind & 1 == 0) continue; // STAB or local
                const name = try string(strings, try int(u32, sym, 0));
                if (name.len == 0 or name[0] != '_') return error.MalformedSymbol;
                const symbol_kind = kind & 0x0e;
                if (symbol_kind == 0x0a) return error.IndirectSymbol;
                try check.symbol(name[1..], symbol_kind != 0 or try int(u64, sym, 8) != 0, true);
            }
        }
        offset += size;
    }
    if (offset != commands.len or !found_table) return error.MissingSymbolTable;
}

fn scanCoff(bytes: []const u8, check: *Check) ArchiveError!void {
    const machine = try int(u16, bytes, 0);
    if (machine != 0x8664 and machine != 0xaa64) return error.UnsupportedCoff;
    if (try int(u16, bytes, 16) != 0) return error.NotRelocatable;
    const start = try int(u32, bytes, 8);
    const count = try int(u32, bytes, 12);
    if (start == 0 or count == 0) return error.MissingSymbolTable;
    const table = try slice(bytes, start, @as(usize, count) * 18);
    const strstart = @as(usize, start) + table.len;
    const strings = try slice(bytes, strstart, try int(u32, bytes, strstart));
    if (strings.len < 4) return error.MalformedObject;
    var i: usize = 0;
    while (i < count) {
        const sym = table[i * 18 ..][0..18];
        const storage = sym[16];
        if (storage == 2 or storage == 105) { // EXTERNAL or WEAK_EXTERNAL
            const name = if (try int(u32, sym, 0) == 0)
                try string(strings, try int(u32, sym, 4))
            else
                std.mem.sliceTo(sym[0..8], 0);
            try check.symbol(name, try int(u16, sym, 12) != 0 or try int(u32, sym, 8) != 0, true);
        }
        i += 1 + @as(usize, sym[17]);
        if (i > count) return error.MalformedObject;
    }
}

test "complete contract rejects new imports and private exports" {
    var check: Check = .{ .os = .linux };
    try check.symbol("__clear_cache", true, false);
    try std.testing.expectError(error.UnexpectedImport, check.symbol("__clear_cache", false, true));
    try std.testing.expectError(error.UnexpectedImport, check.symbol("unknown_future_helper", false, true));
    try std.testing.expectError(error.UnexpectedExport, check.symbol("unknown_future_helper", true, true));
    for (imports) |name| try check.symbol(name, false, true);
    for (exports) |name| try check.symbol(name, true, true);
    try std.testing.expectError(error.DuplicateExport, check.symbol(exports[0], true, true));
    check.members = 1;
    try check.finish();
}

test "OS dependencies are explicit and target specific" {
    var linux: Check = .{ .os = .linux };
    var darwin: Check = .{ .os = .macos };
    var windows: Check = .{ .os = .windows };
    try std.testing.expectError(error.UnexpectedImport, linux.symbol("sys_icache_invalidate", false, true));
    try darwin.symbol("sys_icache_invalidate", false, true);
    try windows.symbol("__imp_FlushInstructionCache", false, true);
    try std.testing.expectError(error.UnexpectedImport, windows.symbol("__imp_unknown", false, true));
}

test "empty and malformed archives cannot pass" {
    var check: Check = .{ .os = .linux };
    try std.testing.expectError(error.NotAnArchive, scanArchive("", &check));
    try scanArchive("!<arch>\n", &check);
    try std.testing.expectError(error.NoObjectMembers, check.finish());
    try std.testing.expectError(error.MalformedObject, scanArchive("!<arch>\ntruncated", &check));
    check.members = 1;
    try std.testing.expectError(error.MissingExport, check.finish());
}

/// LLVM's MSVC target gives constant pools and imported-address cells external
/// COMDAT definitions. These are compiler-owned, so localize their binding
/// before they can meet platform inputs. This is a single-object composition:
/// another object would require a relocatable link before localization.
fn prepareCoffArchive(gpa: std.mem.Allocator, bytes: []const u8) PrepareError![]u8 {
    const Member = struct {
        object: ?[]const u8 = null,
        fn visit(self: *@This(), member: []const u8) ArchiveError!void {
            if (self.object != null) return error.CoffCompositionRequiresOneObject;
            self.object = member;
        }
    };
    var member: Member = .{};
    try walkArchive(bytes, .windows, &member, Member.visit);
    const object = try gpa.dupe(u8, member.object orelse return error.NoObjectMembers);
    defer gpa.free(object);
    try localizeCoff(object);

    // Rebuild the archive index so it advertises only the three public roots.
    // COFF linkers accept the standard first (big-endian) linker member; no
    // long-name table or second linker member is needed for a single short name.
    var index_size: usize = 4 + 4 * exports.len;
    for (exports) |name| index_size += name.len + 1;
    const object_header_offset = 8 + 60 + std.mem.alignForward(usize, index_size, 2);
    const out = try gpa.alloc(u8, object_header_offset + 60 + std.mem.alignForward(usize, object.len, 2));
    errdefer gpa.free(out);
    @memset(out, '\n');
    @memcpy(out[0..8], "!<arch>\n");
    try archiveHeader(out[8..][0..60], "/", index_size);
    const index = out[68..][0..index_size];
    std.mem.writeInt(u32, index[0..4], exports.len, .big);
    var names_offset: usize = 4 + 4 * exports.len;
    for (exports, 0..) |name, i| {
        std.mem.writeInt(u32, index[4 + i * 4 ..][0..4], @intCast(object_header_offset), .big);
        @memcpy(index[names_offset..][0..name.len], name);
        index[names_offset + name.len] = 0;
        names_offset += name.len + 1;
    }
    try archiveHeader(out[object_header_offset..][0..60], "shim.obj/", object.len);
    @memcpy(out[object_header_offset + 60 ..][0..object.len], object);
    return out;
}

fn archiveHeader(out: *[60]u8, name: []const u8, size: usize) error{ NoSpaceLeft, MalformedArchive }!void {
    const header = try std.fmt.bufPrint(out, "{s:<16}{d:<12}{d:<6}{d:<6}{o:<8}{d:<10}`\n", .{ name, @as(u32, 0), @as(u32, 0), @as(u32, 0), @as(u32, 0o644), size });
    if (header.len != out.len) return error.MalformedArchive;
}

fn localizeCoff(bytes: []u8) ArchiveError!void {
    const machine = try int(u16, bytes, 0);
    if (machine != 0x8664 and machine != 0xaa64) return error.UnsupportedCoff;
    if (try int(u16, bytes, 16) != 0) return error.NotRelocatable;
    const section_count = try int(u16, bytes, 2);
    _ = try slice(bytes, 20, @as(usize, section_count) * 40);
    // This archive has one compiler-owned object. Each section now belongs to
    // that object alone; none of its private definitions participates in COMDAT
    // selection with platform code. Relocations and symbol indices stay intact.
    for (0..section_count) |section| {
        const offset = 20 + section * 40 + 36;
        putInt(u32, bytes, offset, (try int(u32, bytes, offset)) & ~@as(u32, 0x1000));
    }
    const start = try int(u32, bytes, 8);
    const count = try int(u32, bytes, 12);
    if (start == 0 or count == 0) return error.MissingSymbolTable;
    const table = try slice(bytes, start, @as(usize, count) * 18);
    const strstart = @as(usize, start) + table.len;
    const strings = try slice(bytes, strstart, try int(u32, bytes, strstart));
    if (strings.len < 4) return error.MalformedObject;
    var i: usize = 0;
    while (i < count) {
        const offset = start + i * 18;
        const sym = bytes[offset..][0..18];
        const section = try int(i16, sym, 12);
        const storage = sym[16];
        const aux = sym[17];
        if (i + 1 + aux > count) return error.MalformedObject;
        if ((storage == 2 or storage == 105) and (section != 0 or try int(u32, sym, 8) != 0)) {
            const name = if (try int(u32, sym, 0) == 0)
                try string(strings, try int(u32, sym, 4))
            else
                std.mem.sliceTo(sym[0..8], 0);
            if (!contains(&exports, name)) {
                if (section <= 0 or section > section_count or storage != 2) return error.UnsupportedCoffDefinition;
                sym[16] = 3; // STATIC: relocations keep referring to the same index.
            }
        }
        if (storage == 3 and section > 0 and try int(u16, sym, 14) == 0 and aux == 1) {
            // Section-definition auxiliary record: clear COMDAT selection and
            // its associative section number along with the section header flag.
            putInt(u16, bytes, offset + 18 + 12, 0);
            bytes[offset + 18 + 14] = 0;
            putInt(u16, bytes, offset + 18 + 16, 0);
        }
        i += 1 + @as(usize, aux);
    }
}

fn putInt(comptime T: type, bytes: []u8, offset: usize, value: T) void {
    std.mem.writeInt(T, bytes[offset..][0..@sizeOf(T)], value, .little);
}

test "ELF parser checks weak imports, hidden exports, and missing tables" {
    const name = "__clear_cache";
    var bytes = [_]u8{0} ** (64 + 3 * 64 + 2 * 24 + 1 + name.len + 1);
    @memcpy(bytes[0..6], "\x7fELF\x02\x01");
    putInt(u16, &bytes, 16, 1); // ET_REL
    putInt(u64, &bytes, 40, 64);
    putInt(u16, &bytes, 58, 64);
    putInt(u16, &bytes, 60, 3);
    const sym_section = 64 + 64;
    const str_section = sym_section + 64;
    const table = 64 + 3 * 64;
    const sym = table + 24;
    const strings = table + 48;
    putInt(u32, &bytes, sym_section + 4, std.elf.SHT_SYMTAB);
    putInt(u64, &bytes, sym_section + 24, table);
    putInt(u64, &bytes, sym_section + 32, 48);
    putInt(u32, &bytes, sym_section + 40, 2);
    putInt(u64, &bytes, sym_section + 56, 24);
    putInt(u32, &bytes, str_section + 4, std.elf.SHT_STRTAB);
    putInt(u64, &bytes, str_section + 24, strings);
    putInt(u64, &bytes, str_section + 32, name.len + 2);
    putInt(u32, &bytes, sym, 1);
    bytes[sym + 4] = @as(u8, std.elf.STB_WEAK) << 4;
    @memcpy(bytes[strings + 1 ..][0..name.len], name);
    var check: Check = .{ .os = .linux };
    try std.testing.expectError(error.UnexpectedImport, scanElf(&bytes, &check));
    putInt(u16, &bytes, sym + 6, 1);
    bytes[sym + 5] = 2; // STV_HIDDEN does not make a definition private.
    try std.testing.expectError(error.UnexpectedExport, scanElf(&bytes, &check));
    bytes[sym + 4] = @as(u8, std.elf.STB_LOCAL) << 4;
    try scanElf(&bytes, &check);
    putInt(u32, &bytes, sym_section + 4, std.elf.SHT_NULL);
    try std.testing.expectError(error.MissingSymbolTable, scanElf(&bytes, &check));
}

test "Mach-O parser checks private externs and weak references" {
    const name = "___clear_cache"; // Includes Mach-O's leading underscore.
    var bytes = [_]u8{0} ** (32 + 24 + 16 + 1 + name.len + 1);
    putInt(u32, &bytes, 0, std.macho.MH_MAGIC_64);
    putInt(u32, &bytes, 12, std.macho.MH_OBJECT);
    putInt(u32, &bytes, 16, 1);
    putInt(u32, &bytes, 20, 24);
    putInt(u32, &bytes, 32, @intFromEnum(std.macho.LC.SYMTAB));
    putInt(u32, &bytes, 36, 24);
    putInt(u32, &bytes, 40, 56);
    putInt(u32, &bytes, 44, 1);
    putInt(u32, &bytes, 48, 72);
    putInt(u32, &bytes, 52, name.len + 2);
    putInt(u32, &bytes, 56, 1);
    bytes[60] = 1; // N_EXT | N_UNDF
    putInt(u16, &bytes, 62, 0x40); // N_WEAK_REF
    @memcpy(bytes[73..][0..name.len], name);
    var check: Check = .{ .os = .macos };
    try std.testing.expectError(error.UnexpectedImport, scanMachO(&bytes, &check));
    bytes[60] = 0x1f; // N_PEXT | N_EXT | N_SECT is still globally bound.
    try std.testing.expectError(error.UnexpectedExport, scanMachO(&bytes, &check));
    bytes[60] = 0x0e;
    try scanMachO(&bytes, &check);
    try std.testing.expectError(error.MalformedObject, scanMachO(bytes[0 .. bytes.len - 1], &check));
}

test "COFF parser checks weak externs and skips auxiliary records" {
    const name = "__clear_cache";
    var bytes = [_]u8{0} ** (20 + 36 + 4 + name.len + 1);
    putInt(u16, &bytes, 0, 0xaa64);
    putInt(u32, &bytes, 8, 20);
    putInt(u32, &bytes, 12, 2);
    putInt(u32, &bytes, 24, 4);
    bytes[36] = 105; // WEAK_EXTERNAL
    bytes[37] = 1; // Followed by one auxiliary record.
    putInt(u32, &bytes, 56, name.len + 5);
    @memcpy(bytes[60..][0..name.len], name);
    var check: Check = .{ .os = .windows };
    try std.testing.expectError(error.UnexpectedImport, scanCoff(&bytes, &check));
    bytes[36] = 2; // EXTERNAL
    putInt(u16, &bytes, 32, 1);
    try std.testing.expectError(error.UnexpectedExport, scanCoff(&bytes, &check));
    bytes[36] = 3; // STATIC
    try scanCoff(&bytes, &check);
    bytes[37] = 2;
    try std.testing.expectError(error.MalformedObject, scanCoff(&bytes, &check));
}

test "COFF preparation preserves relocations and indexes only public roots" {
    const names = exports ++ .{"compiler_private_constant"};
    const table_start = 20 + 40 + 8 + 10;
    const strings_start = table_start + names.len * 18;
    var bytes = [_]u8{0} ** 512;
    putInt(u16, &bytes, 0, 0x8664);
    putInt(u16, &bytes, 2, 1);
    putInt(u32, &bytes, 8, table_start);
    putInt(u32, &bytes, 12, names.len);
    @memcpy(bytes[20..26], ".rdata");
    putInt(u32, &bytes, 20 + 16, 8);
    putInt(u32, &bytes, 20 + 20, 60);
    putInt(u32, &bytes, 20 + 24, 68);
    putInt(u16, &bytes, 20 + 32, 1);
    putInt(u32, &bytes, 20 + 36, 0x40001040); // read-only initialized COMDAT
    putInt(u64, &bytes, 60, 0x12345678);
    putInt(u32, &bytes, 68 + 4, 3); // Relocation still addresses symbol index 3.
    putInt(u16, &bytes, 68 + 8, 1); // IMAGE_REL_AMD64_ADDR64
    var strsize: usize = 4;
    for (names, 0..) |name, i| {
        const offset = table_start + i * 18;
        putInt(u32, &bytes, offset + 4, @intCast(strsize));
        putInt(u16, &bytes, offset + 12, 1);
        bytes[offset + 16] = 2;
        @memcpy(bytes[strings_start + strsize ..][0..name.len], name);
        strsize += name.len + 1;
    }
    putInt(u32, &bytes, strings_start, @intCast(strsize));
    const object = bytes[0 .. strings_start + strsize];
    const original = bytes;
    try localizeCoff(object);
    try std.testing.expectEqual(@as(u8, 3), bytes[table_start + 3 * 18 + 16]);
    for (0..exports.len) |i| try std.testing.expectEqual(@as(u8, 2), bytes[table_start + i * 18 + 16]);
    try std.testing.expectEqualSlices(u8, original[60..78], bytes[60..78]);
    try std.testing.expectEqual(@as(u32, 0), (try int(u32, object, 56)) & 0x1000);
    var check: Check = .{ .os = .windows, .members = 1 };
    try scanCoff(object, &check);
    try check.finish();

    var archive = [_]u8{'\n'} ** 600;
    @memcpy(archive[0..8], "!<arch>\n");
    try archiveHeader(archive[8..68], "input.obj/", object.len);
    @memcpy(archive[68..][0..object.len], object);
    const prepared = try prepareCoffArchive(std.testing.allocator, archive[0 .. 68 + std.mem.alignForward(usize, object.len, 2)]);
    defer std.testing.allocator.free(prepared);
    try std.testing.expectEqual(@as(u32, exports.len), std.mem.readInt(u32, prepared[68..72], .big));
    const member_offset = std.mem.readInt(u32, prepared[72..76], .big);
    const names_bytes = prepared[68 + 4 + 4 * exports.len .. member_offset];
    try std.testing.expect(std.mem.find(u8, names_bytes, "compiler_private_constant") == null);
    var roundtrip: Check = .{ .os = .windows };
    try scanArchive(prepared, &roundtrip);
    try roundtrip.finish();
}
