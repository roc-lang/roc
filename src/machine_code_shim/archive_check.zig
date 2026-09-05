//! Checks that the machine-code shim keeps compiler-private support local.
//!
//! `roc <app>.roc` links this archive with the platform's own inputs and
//! nothing else -- no compiler-rt object of the compiler's own. In particular,
//! `__zig_probe_stack` belongs to Zig's compiler-rt, and a platform host need
//! not be a Zig host. The shim therefore has to resolve that support locally:
//! leaving it undefined reproduces https://github.com/roc-lang/roc/issues/11059,
//! while exposing it globally lets a compiler-owned symbol participate in the
//! platform link.
//!
//! Usage: machine_code_shim_archive_check <archive path>
//!
//! Reads ELF members, which is the format on the target this is wired up for.

const std = @import("std");

const elf = std.elf;

const ar_magic = "!<arch>\n";

const ArchiveCheckError = std.process.Args.ToSliceError ||
    std.Io.Dir.ReadFileAllocError ||
    error{
        MissingPath,
        NotAnArchive,
        NoObjectMembers,
        CompilerPrivateSymbolEscaped,
    };

/// Compiler-private symbols the shim archive may neither leave unresolved nor
/// expose to the surrounding platform link.
const compiler_private = [_]struct { name: []const u8, why: []const u8 }{
    .{
        .name = "__zig_probe_stack",
        .why = "only Zig's compiler-rt defines it, and run mode links no compiler-rt beside the shim",
    },
};

const Use = struct {
    /// Archive member that references the symbol without defining it.
    referenced_by: ?[]const u8 = null,
    /// Archive member that exposes the compiler-owned definition globally.
    globally_defined_by: ?[]const u8 = null,
};

/// Fails when the archive named by the first argument lets a compiler-private
/// symbol escape as undefined or globally visible.
pub fn main(init: std.process.Init) ArchiveCheckError!void {
    var arena_impl = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const args = try init.minimal.args.toSlice(arena);
    const path = if (args.len >= 2) args[1] else {
        std.debug.print("Usage: machine_code_shim_archive_check <archive path>\n", .{});
        return error.MissingPath;
    };

    const bytes = try std.Io.Dir.cwd().readFileAlloc(init.io, path, arena, .unlimited);
    if (bytes.len < ar_magic.len or !std.mem.eql(u8, bytes[0..ar_magic.len], ar_magic)) {
        std.debug.print("FAILED: {s} does not start with the ar archive magic\n", .{path});
        return error.NotAnArchive;
    }

    var uses = [_]Use{.{}} ** compiler_private.len;
    const members_scanned = scanArchive(bytes, &uses);
    if (members_scanned == 0) {
        std.debug.print("FAILED: {s} holds no ELF object members\n", .{path});
        return error.NoObjectMembers;
    }

    var found_escape = false;
    for (compiler_private, uses) |symbol, use| {
        if (use.referenced_by) |member| {
            found_escape = true;
            std.debug.print(
                "FAILED: {s} references {s} without a local definition ({s}), from member {s}\n",
                .{ path, symbol.name, symbol.why, member },
            );
        }
        if (use.globally_defined_by) |member| {
            found_escape = true;
            std.debug.print(
                "FAILED: {s} exposes compiler-owned symbol {s} from member {s}; it must be local to the referencing object\n",
                .{ path, symbol.name, member },
            );
        }
    }
    if (found_escape) return error.CompilerPrivateSymbolEscaped;

    std.debug.print("SUCCESS: {s} keeps checked compiler-private support local\n", .{path});
}

/// Walk the archive's members, recording where each checked symbol is
/// undefined or globally defined. Returns the number of ELF object members
/// read.
fn scanArchive(bytes: []const u8, uses: *[compiler_private.len]Use) usize {
    var members_scanned: usize = 0;
    var long_names: []const u8 = &.{};
    var offset: usize = ar_magic.len;

    while (offset + 60 <= bytes.len) {
        const header = bytes[offset .. offset + 60];
        const name_field = std.mem.trimEnd(u8, header[0..16], " ");
        const size_field = std.mem.trimEnd(u8, header[48..58], " ");
        const member_size = std.fmt.parseInt(usize, size_field, 10) catch return members_scanned;
        offset += 60;
        if (offset + member_size > bytes.len) return members_scanned;

        var member = bytes[offset .. offset + member_size];
        var member_name = name_field;

        // BSD-style long member names are stored in the member's first bytes.
        if (std.mem.startsWith(u8, name_field, "#1/")) {
            const name_len = std.fmt.parseInt(usize, name_field[3..], 10) catch return members_scanned;
            if (name_len > member.len) return members_scanned;
            member_name = std.mem.trimEnd(u8, member[0..name_len], "\x00");
            member = member[name_len..];
        }

        // GNU-style long member names live in the "//" member, referenced as
        // "/<offset into that member>".
        if (std.mem.eql(u8, member_name, "//")) {
            long_names = member;
        } else if (member_name.len > 1 and member_name[0] == '/' and std.ascii.isDigit(member_name[1])) {
            if (std.fmt.parseInt(usize, member_name[1..], 10) catch null) |name_off| {
                if (name_off < long_names.len) {
                    member_name = std.mem.trimEnd(u8, std.mem.sliceTo(long_names[name_off..], '\n'), "/");
                }
            }
        }

        const is_index = std.mem.eql(u8, member_name, "/") or
            std.mem.eql(u8, member_name, "//") or
            std.mem.eql(u8, member_name, "/SYM64/") or
            std.mem.startsWith(u8, member_name, "__.SYMDEF");

        if (!is_index and member.len >= 4 and std.mem.eql(u8, member[0..4], "\x7fELF")) {
            scanElfMember(member, member_name, uses);
            members_scanned += 1;
        }

        offset += member_size;
        if (offset % 2 == 1) offset += 1; // members are 2-byte aligned
    }

    return members_scanned;
}

fn scanElfMember(bytes: []const u8, member_name: []const u8, uses: *[compiler_private.len]Use) void {
    if (bytes.len <= elf.EI_CLASS) return;
    const class = bytes[elf.EI_CLASS];
    if (class == elf.ELFCLASS32) {
        scanElfClass(elf.Elf32_Ehdr, elf.Elf32_Shdr, elf.Elf32_Sym, bytes, member_name, uses);
    } else if (class == elf.ELFCLASS64) {
        scanElfClass(elf.Elf64_Ehdr, elf.Elf64_Shdr, elf.Elf64_Sym, bytes, member_name, uses);
    }
}

fn scanElfClass(
    comptime Ehdr: type,
    comptime Shdr: type,
    comptime Sym: type,
    bytes: []const u8,
    member_name: []const u8,
    uses: *[compiler_private.len]Use,
) void {
    if (bytes.len < @sizeOf(Ehdr)) return;
    const ehdr = std.mem.bytesAsValue(Ehdr, bytes[0..@sizeOf(Ehdr)]);

    const shoff: usize = @intCast(ehdr.e_shoff);
    const shnum: usize = ehdr.e_shnum;
    const shentsize: usize = ehdr.e_shentsize;
    if (shentsize < @sizeOf(Shdr)) return;
    if (shoff + shnum * shentsize > bytes.len) return;

    var i: usize = 0;
    while (i < shnum) : (i += 1) {
        const shdr = std.mem.bytesAsValue(Shdr, bytes[shoff + i * shentsize ..][0..@sizeOf(Shdr)]);
        if (shdr.sh_type != elf.SHT_SYMTAB) continue;

        const strtab_index: usize = shdr.sh_link;
        if (strtab_index >= shnum) return;
        const strtab_hdr = std.mem.bytesAsValue(Shdr, bytes[shoff + strtab_index * shentsize ..][0..@sizeOf(Shdr)]);
        const strtab_off: usize = @intCast(strtab_hdr.sh_offset);
        const strtab_size: usize = @intCast(strtab_hdr.sh_size);
        if (strtab_off + strtab_size > bytes.len) return;
        const strtab = bytes[strtab_off .. strtab_off + strtab_size];

        const sym_off: usize = @intCast(shdr.sh_offset);
        const sym_size: usize = @intCast(shdr.sh_size);
        if (sym_off + sym_size > bytes.len) return;
        const sym_count = sym_size / @sizeOf(Sym);

        var s: usize = 0;
        while (s < sym_count) : (s += 1) {
            const sym = std.mem.bytesAsValue(Sym, bytes[sym_off + s * @sizeOf(Sym) ..][0..@sizeOf(Sym)]);
            const name_off: usize = sym.st_name;
            if (name_off >= strtab.len) continue;
            const name = std.mem.sliceTo(strtab[name_off..], 0);

            for (compiler_private, uses) |symbol, *use| {
                if (!std.mem.eql(u8, name, symbol.name)) continue;
                if (sym.st_shndx == elf.SHN_UNDEF) {
                    if (use.referenced_by == null) use.referenced_by = member_name;
                } else if (sym.st_bind() != elf.STB_LOCAL) {
                    if (use.globally_defined_by == null) use.globally_defined_by = member_name;
                }
            }
        }
    }
}
