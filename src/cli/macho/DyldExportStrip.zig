//! Removes the dyld export trie and weak-binding info from a Mach-O
//! executable, so that dyld stops walking them on every launch.
//!
//! A Mach-O executable statically linking LLVM, LLD, Binaryen, zlib and zstd
//! carries tens of thousands of export-trie entries and thousands of weak
//! external C++ symbols, and dyld processes all of them before main() runs
//! (hundreds of millions of instructions per launch; roc-lang/roc#10992).
//! None of it is consumed at runtime: the roc CLI loads only libSystem (which
//! defines no C++ weak symbols to coalesce with), the temporary dylibs it
//! dlopens bind only to libSystem under two-level namespace, and its
//! dlsym(RTLD_DEFAULT) lookups resolve builtins through an in-process table.
//!
//! The rewrite zeroes the export and weak-bind ranges recorded in
//! LC_DYLD_INFO/LC_DYLD_INFO_ONLY (and LC_DYLD_EXPORTS_TRIE if present) and
//! clears the MH_WEAK_DEFINES and MH_BINDS_TO_WEAK header flags that gate
//! dyld's weak-coalescing pass. This is the same end state Apple's linker
//! produces for `-no_exported_symbols` plus hidden visibility. It applies
//! only to executables; a dylib's exports are its interface and must stay.

const std = @import("std");
const macho = std.macho;
const Allocator = std.mem.Allocator;
const AdHocResign = @import("AdHocResign.zig");

/// Errors from `stripHeader`.
pub const StripError = error{
    NotMacho64,
    NotExecutable,
    TruncatedLoadCommands,
    UnsupportedDyldLayout,
};

/// A byte range of the stripped file whose contents are no longer referenced
/// by any load command.
pub const DeadRange = struct {
    /// File offset of the start of the range.
    off: u32,
    /// Length of the range in bytes.
    size: u32,
};

/// What `stripHeader` removed: the now-unreferenced byte ranges that held
/// export info and the weak-bind opcodes. Any range may be empty when the
/// corresponding info was already absent.
pub const Summary = struct {
    /// Range LC_DYLD_INFO(_ONLY) recorded for the export trie.
    export_info: DeadRange,
    /// Range LC_DYLD_EXPORTS_TRIE recorded for the export trie.
    export_trie: DeadRange,
    /// Range that held the weak-bind opcodes.
    weak_bind_info: DeadRange,
};

/// Rewrite the Mach-O header and load commands at the start of `bytes`:
/// zero the export and weak-bind ranges out of the dyld info load commands
/// and clear MH_WEAK_DEFINES|MH_BINDS_TO_WEAK. `bytes` must hold at least
/// the full header and load commands; the rest of the file is not needed.
/// Idempotent: stripping an already-stripped image succeeds and reports
/// empty ranges.
pub fn stripHeader(bytes: []u8) StripError!Summary {
    if (bytes.len < @sizeOf(macho.mach_header_64)) return error.NotMacho64;
    const header: *align(1) macho.mach_header_64 = @ptrCast(bytes.ptr);
    if (header.magic != macho.MH_MAGIC_64) return error.NotMacho64;
    if (header.filetype != macho.MH_EXECUTE) return error.NotExecutable;

    const cmds_end = @sizeOf(macho.mach_header_64) + @as(usize, header.sizeofcmds);
    if (bytes.len < cmds_end) return error.TruncatedLoadCommands;

    var summary: Summary = .{
        .export_info = .{ .off = 0, .size = 0 },
        .export_trie = .{ .off = 0, .size = 0 },
        .weak_bind_info = .{ .off = 0, .size = 0 },
    };

    var offset: usize = @sizeOf(macho.mach_header_64);
    var i: u32 = 0;
    while (i < header.ncmds) : (i += 1) {
        if (offset + @sizeOf(macho.load_command) > cmds_end) return error.TruncatedLoadCommands;
        const lc: *align(1) macho.load_command = @ptrCast(bytes.ptr + offset);
        if (lc.cmdsize < @sizeOf(macho.load_command) or offset + lc.cmdsize > cmds_end) {
            return error.TruncatedLoadCommands;
        }
        switch (lc.cmd) {
            .DYLD_INFO, .DYLD_INFO_ONLY => {
                if (lc.cmdsize < @sizeOf(macho.dyld_info_command)) return error.TruncatedLoadCommands;
                const info: *align(1) macho.dyld_info_command = @ptrCast(lc);
                summary.export_info = .{ .off = info.export_off, .size = info.export_size };
                summary.weak_bind_info = .{ .off = info.weak_bind_off, .size = info.weak_bind_size };
                info.export_off = 0;
                info.export_size = 0;
                info.weak_bind_off = 0;
                info.weak_bind_size = 0;
            },
            .DYLD_EXPORTS_TRIE => {
                if (lc.cmdsize < @sizeOf(macho.linkedit_data_command)) return error.TruncatedLoadCommands;
                const trie: *align(1) macho.linkedit_data_command = @ptrCast(lc);
                summary.export_trie = .{ .off = trie.dataoff, .size = trie.datasize };
                trie.dataoff = 0;
                trie.datasize = 0;
            },
            // Chained fixups encode binds in a format this rewrite does not
            // parse, so weak binds could survive it undetected. The linker
            // producing the roc CLI emits classic LC_DYLD_INFO_ONLY; if that
            // ever changes, fail the build loudly instead of shipping a
            // binary that silently keeps its launch cost.
            .DYLD_CHAINED_FIXUPS => return error.UnsupportedDyldLayout,
            .NONE,
            .SEGMENT,
            .SYMTAB,
            .SYMSEG,
            .THREAD,
            .UNIXTHREAD,
            .LOADFVMLIB,
            .IDFVMLIB,
            .IDENT,
            .FVMFILE,
            .PREPAGE,
            .DYSYMTAB,
            .LOAD_DYLIB,
            .ID_DYLIB,
            .LOAD_DYLINKER,
            .ID_DYLINKER,
            .PREBOUND_DYLIB,
            .ROUTINES,
            .SUB_FRAMEWORK,
            .SUB_UMBRELLA,
            .SUB_CLIENT,
            .SUB_LIBRARY,
            .TWOLEVEL_HINTS,
            .PREBIND_CKSUM,
            .LOAD_WEAK_DYLIB,
            .SEGMENT_64,
            .ROUTINES_64,
            .UUID,
            .RPATH,
            .CODE_SIGNATURE,
            .SEGMENT_SPLIT_INFO,
            .REEXPORT_DYLIB,
            .LAZY_LOAD_DYLIB,
            .ENCRYPTION_INFO,
            .LOAD_UPWARD_DYLIB,
            .VERSION_MIN_MACOSX,
            .VERSION_MIN_IPHONEOS,
            .FUNCTION_STARTS,
            .DYLD_ENVIRONMENT,
            .MAIN,
            .DATA_IN_CODE,
            .SOURCE_VERSION,
            .DYLIB_CODE_SIGN_DRS,
            .ENCRYPTION_INFO_64,
            .LINKER_OPTION,
            .LINKER_OPTIMIZATION_HINT,
            .VERSION_MIN_TVOS,
            .VERSION_MIN_WATCHOS,
            .NOTE,
            .BUILD_VERSION,
            _,
            => {},
        }
        offset += lc.cmdsize;
    }

    header.flags &= ~@as(u32, macho.MH_WEAK_DEFINES | macho.MH_BINDS_TO_WEAK);
    return summary;
}

/// Errors from `stripFile`.
pub const StripFileError = StripError || AdHocResign.Error || Allocator.Error || std.Io.File.OpenError || std.Io.File.ReadPositionalError || std.Io.File.WritePositionalError || error{DeadRangeOutOfBounds};

/// Strip the executable at `path` in place: rewrite its header and load
/// commands via `stripHeader`, zero the dead byte ranges the load commands
/// no longer reference (zeroed pages cost nothing after compression, and
/// leftover symbol names would be misleading to inspection tools), and
/// rewrite the ad-hoc code signature the edits invalidated.
pub fn stripFile(io: std.Io, gpa: Allocator, arena: Allocator, path: []const u8) StripFileError!Summary {
    const summary = blk: {
        var file = try std.Io.Dir.cwd().openFile(io, path, .{ .mode = .read_write });
        defer file.close(io);

        const file_len = try file.length(io);

        var header: macho.mach_header_64 = undefined;
        const header_n = try file.readPositionalAll(io, std.mem.asBytes(&header), 0);
        if (header_n != @sizeOf(macho.mach_header_64)) return error.NotMacho64;
        if (header.magic != macho.MH_MAGIC_64) return error.NotMacho64;

        const prefix_len = @sizeOf(macho.mach_header_64) + @as(usize, header.sizeofcmds);
        const prefix = try arena.alloc(u8, prefix_len);
        const prefix_n = try file.readPositionalAll(io, prefix, 0);
        if (prefix_n != prefix_len) return error.TruncatedLoadCommands;

        const summary = try stripHeader(prefix);
        try file.writePositionalAll(io, prefix, 0);

        for ([_]DeadRange{ summary.export_info, summary.export_trie, summary.weak_bind_info }) |range| {
            if (range.size == 0) continue;
            if (range.off < prefix_len) return error.DeadRangeOutOfBounds;
            if (@as(u64, range.off) + @as(u64, range.size) > file_len) return error.DeadRangeOutOfBounds;
            const zeros = try arena.alloc(u8, range.size);
            @memset(zeros, 0);
            try file.writePositionalAll(io, zeros, range.off);
        }
        break :blk summary;
    };

    try AdHocResign.resign(io, gpa, arena, path);
    return summary;
}

const testing = std.testing;

/// Byte layout used by the tests: header, one LC_DYLD_INFO_ONLY, one
/// LC_DYLD_EXPORTS_TRIE.
fn testImage(buf: []u8, filetype: u32, flags: u32) void {
    @memset(buf, 0);
    const header: *align(1) macho.mach_header_64 = @ptrCast(buf.ptr);
    header.magic = macho.MH_MAGIC_64;
    header.filetype = filetype;
    header.flags = flags;
    header.ncmds = 2;
    header.sizeofcmds = @sizeOf(macho.dyld_info_command) + @sizeOf(macho.linkedit_data_command);

    const info: *align(1) macho.dyld_info_command = @ptrCast(buf.ptr + @sizeOf(macho.mach_header_64));
    info.cmd = .DYLD_INFO_ONLY;
    info.cmdsize = @sizeOf(macho.dyld_info_command);
    info.rebase_off = 100;
    info.rebase_size = 10;
    info.bind_off = 110;
    info.bind_size = 20;
    info.weak_bind_off = 130;
    info.weak_bind_size = 40;
    info.lazy_bind_off = 170;
    info.lazy_bind_size = 5;
    info.export_off = 175;
    info.export_size = 80;

    const trie: *align(1) macho.linkedit_data_command = @ptrCast(buf.ptr + @sizeOf(macho.mach_header_64) + @sizeOf(macho.dyld_info_command));
    trie.cmd = .DYLD_EXPORTS_TRIE;
    trie.cmdsize = @sizeOf(macho.linkedit_data_command);
    trie.dataoff = 255;
    trie.datasize = 33;
}

test "stripHeader zeroes export and weak-bind info and clears weak flags" {
    var buf: [512]u8 = undefined;
    testImage(&buf, macho.MH_EXECUTE, macho.MH_PIE | macho.MH_WEAK_DEFINES | macho.MH_BINDS_TO_WEAK);

    const summary = try stripHeader(&buf);
    try testing.expectEqual(@as(u32, 175), summary.export_info.off);
    try testing.expectEqual(@as(u32, 80), summary.export_info.size);
    try testing.expectEqual(@as(u32, 255), summary.export_trie.off);
    try testing.expectEqual(@as(u32, 33), summary.export_trie.size);
    try testing.expectEqual(@as(u32, 130), summary.weak_bind_info.off);
    try testing.expectEqual(@as(u32, 40), summary.weak_bind_info.size);

    const header: *align(1) macho.mach_header_64 = @ptrCast(&buf);
    try testing.expectEqual(@as(u32, macho.MH_PIE), header.flags);

    const info: *align(1) macho.dyld_info_command = @ptrCast(buf[@sizeOf(macho.mach_header_64)..].ptr);
    try testing.expectEqual(@as(u32, 0), info.export_off);
    try testing.expectEqual(@as(u32, 0), info.export_size);
    try testing.expectEqual(@as(u32, 0), info.weak_bind_off);
    try testing.expectEqual(@as(u32, 0), info.weak_bind_size);
    // Rebase, bind and lazy-bind info stay untouched.
    try testing.expectEqual(@as(u32, 100), info.rebase_off);
    try testing.expectEqual(@as(u32, 20), info.bind_size);
    try testing.expectEqual(@as(u32, 5), info.lazy_bind_size);
}

test "stripHeader is idempotent" {
    var buf: [512]u8 = undefined;
    testImage(&buf, macho.MH_EXECUTE, macho.MH_WEAK_DEFINES | macho.MH_BINDS_TO_WEAK);

    _ = try stripHeader(&buf);
    const second = try stripHeader(&buf);
    try testing.expectEqual(@as(u32, 0), second.export_info.size);
    try testing.expectEqual(@as(u32, 0), second.export_trie.size);
    try testing.expectEqual(@as(u32, 0), second.weak_bind_info.size);
}

test "stripHeader rejects dylibs" {
    var buf: [512]u8 = undefined;
    testImage(&buf, macho.MH_DYLIB, 0);
    try testing.expectError(error.NotExecutable, stripHeader(&buf));
}

test "stripHeader rejects chained fixups" {
    var buf: [512]u8 = undefined;
    testImage(&buf, macho.MH_EXECUTE, 0);
    const info: *align(1) macho.load_command = @ptrCast(buf[@sizeOf(macho.mach_header_64)..].ptr);
    info.cmd = .DYLD_CHAINED_FIXUPS;
    try testing.expectError(error.UnsupportedDyldLayout, stripHeader(&buf));
}

test "stripHeader rejects non-Mach-O input" {
    var buf: [512]u8 = @splat(0);
    try testing.expectError(error.NotMacho64, stripHeader(&buf));
    try testing.expectError(error.NotMacho64, stripHeader(buf[0..4]));
}

test "stripHeader rejects load commands past sizeofcmds" {
    var buf: [512]u8 = undefined;
    testImage(&buf, macho.MH_EXECUTE, 0);
    const header: *align(1) macho.mach_header_64 = @ptrCast(&buf);
    header.sizeofcmds = @sizeOf(macho.dyld_info_command); // second command now out of bounds
    try testing.expectError(error.TruncatedLoadCommands, stripHeader(&buf));
}
