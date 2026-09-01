//! Rewrites a Mach-O binary's ad-hoc code signature in place after other
//! post-link edits have invalidated it. On macOS 14+ the kernel SIGKILLs
//! (exit 137) binaries whose signature does not match their contents, so any
//! step that patches a signed binary must finish by rewriting the signature.

const std = @import("std");
const macho = std.macho;
const Allocator = std.mem.Allocator;
const CodeSignature = @import("vendor_macho").CodeSignature;

/// Identifier embedded in every signature this module writes. Constant so
/// that identical binaries sign to identical bytes.
pub const deterministic_identifier = "roc";

/// Errors from `resign`.
pub const Error = Allocator.Error || CodeSignature.WriteError || std.Io.File.OpenError || std.Io.File.ReadPositionalError || std.Io.File.WritePositionalError || error{
    CodeSignatureNotAtEnd,
    InvalidCodeSignatureSize,
    MissingLinkeditSegment,
    MissingTextSegment,
    NonResizable,
    NotMacho64,
    UnexpectedEof,
};

/// Rewrite the ad-hoc code signature of the Mach-O binary at `path`. The
/// signature blob is the last content in the file, recorded by
/// LC_CODE_SIGNATURE; the page hashes are recomputed over everything before
/// it and a fresh linker-style ad-hoc signature is written into that extent.
/// A binary with no LC_CODE_SIGNATURE is left untouched: the linker did not
/// sign it (ld64.lld only ad-hoc signs arm64 by default), so patching
/// invalidated nothing and the kernel does not require a signature.
pub fn resign(io: std.Io, gpa: Allocator, arena: Allocator, path: []const u8) Error!void {
    var file = try std.Io.Dir.cwd().openFile(io, path, .{ .mode = .read_write });
    defer file.close(io);

    var header: macho.mach_header_64 = undefined;
    const header_n = try file.readPositionalAll(io, std.mem.asBytes(&header), 0);
    if (header_n != @sizeOf(macho.mach_header_64)) return error.UnexpectedEof;
    if (header.magic != macho.MH_MAGIC_64) return error.NotMacho64;

    const cmds_buf = try arena.alignedAlloc(u8, .of(macho.segment_command_64), header.sizeofcmds);
    const cmds_n = try file.readPositionalAll(io, cmds_buf, @sizeOf(macho.mach_header_64));
    if (cmds_n != header.sizeofcmds) return error.UnexpectedEof;

    var cs_cmd: ?*align(8) macho.linkedit_data_command = null;
    var text_seg: ?*align(8) macho.segment_command_64 = null;
    var linkedit_seg: ?*align(8) macho.segment_command_64 = null;

    var offset: usize = 0;
    var i: u32 = 0;
    while (i < header.ncmds) : (i += 1) {
        if (offset + @sizeOf(macho.load_command) > cmds_buf.len) return error.UnexpectedEof;
        const lc: *align(8) macho.load_command = @ptrCast(@alignCast(cmds_buf.ptr + offset));
        if (lc.cmd == .CODE_SIGNATURE) {
            cs_cmd = @ptrCast(lc);
        } else if (lc.cmd == .SEGMENT_64) {
            const seg: *align(8) macho.segment_command_64 = @ptrCast(lc);
            if (std.mem.eql(u8, seg.segName(), "__TEXT")) {
                text_seg = seg;
            } else if (std.mem.eql(u8, seg.segName(), "__LINKEDIT")) {
                linkedit_seg = seg;
            }
        }
        offset += lc.cmdsize;
    }

    const cs = cs_cmd orelse return;
    const text = text_seg orelse return error.MissingTextSegment;
    const linkedit = linkedit_seg orelse return error.MissingLinkeditSegment;

    const page_size: u16 = if (header.cputype == macho.CPU_TYPE_ARM64) 0x4000 else 0x1000;
    const ident = deterministic_identifier;

    // The signature hashes every page before LC_CODE_SIGNATURE's dataoff,
    // including page 0 with the load commands. Its exact size is known up
    // front (one CodeDirectory blob, no special slots), so any load command
    // size changes must be written back before hashing.
    const hash_size = std.crypto.hash.sha2.Sha256.digest_length;
    const total_pages = std.mem.alignForward(usize, cs.dataoff, page_size) / page_size;
    const exact_size = @sizeOf(macho.SuperBlob) + @sizeOf(macho.BlobIndex) +
        @sizeOf(macho.CodeDirectory) + ident.len + 1 + total_pages * hash_size;

    const old_datasize = cs.datasize;
    const old_sig_end: u64 = @as(u64, cs.dataoff) + @as(u64, old_datasize);
    if (try file.length(io) != old_sig_end) return error.CodeSignatureNotAtEnd;

    if (exact_size != old_datasize) {
        cs.datasize = @intCast(exact_size);
        if (exact_size > old_datasize) {
            const grow: u64 = @intCast(exact_size - old_datasize);
            linkedit.filesize += grow;
        } else {
            const shrink: u64 = @intCast(old_datasize - exact_size);
            if (linkedit.filesize < shrink) return error.InvalidCodeSignatureSize;
            linkedit.filesize -= shrink;
        }
        linkedit.vmsize = std.mem.alignForward(u64, linkedit.filesize, page_size);
        try file.writePositionalAll(io, cmds_buf, @sizeOf(macho.mach_header_64));
    }

    var code_sig = CodeSignature.init(page_size);
    defer code_sig.deinit(gpa);
    code_sig.code_directory.ident = ident;

    var sig_bytes: std.Io.Writer.Allocating = .init(gpa);
    defer sig_bytes.deinit();
    try code_sig.writeAdhocSignature(gpa, io, .{
        .file = file,
        .exec_seg_base = text.fileoff,
        .exec_seg_limit = text.filesize,
        .file_size = cs.dataoff,
        .dylib = header.filetype == macho.MH_DYLIB,
    }, &sig_bytes.writer);

    const sig = sig_bytes.written();
    std.debug.assert(sig.len == exact_size);
    try file.writePositionalAll(io, sig, cs.dataoff);
    const new_sig_end: u64 = @as(u64, cs.dataoff) + @as(u64, @intCast(sig.len));
    try file.setLength(io, new_sig_end);
}
