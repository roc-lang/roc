//! Build helper: compress `echo.wasm` into a `.zst` archive, reusing the same
//! zstd streaming compressor that powers `roc bundle` (see `src/bundle/`).
//!
//! Usage: echo_wasm_archive <input.wasm> <output.wasm.zst>
//!
//! Invoked by `zig build build-echo-wasm-archive`, which builds `echo.wasm`
//! first and then runs this tool to emit `zig-out/lib/echo.wasm.zst`.

const std = @import("std");
const bundle = @import("bundle");

const ArchiveError = std.process.Args.ToSliceError ||
    std.Io.Dir.ReadFileAllocError ||
    std.Io.Dir.WriteFileError ||
    std.Io.Writer.Error ||
    std.mem.Allocator.Error;

/// Reads the input wasm path (argv[1]), zstd-compresses it with the shared
/// `roc bundle` compressor, and writes the archive to the output path (argv[2]).
pub fn main(init: std.process.Init) ArchiveError!void {
    const io = init.io;

    var gpa_impl: std.heap.DebugAllocator(.{}) = .init;
    defer _ = gpa_impl.deinit();
    var gpa = gpa_impl.allocator();

    var arena_impl = std.heap.ArenaAllocator.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    const stderr_file: std.Io.File = .stderr();

    const args = try init.minimal.args.toSlice(arena);
    if (args.len != 3) {
        stderr_file.writeStreamingAll(io, "Usage: echo_wasm_archive <input.wasm> <output.wasm.zst>\n") catch {};
        std.process.exit(2);
    }

    const input_path = args[1];
    const output_path = args[2];

    const input = try std.Io.Dir.cwd().readFileAlloc(io, input_path, arena, .unlimited);

    // Compress into memory using the shared zstd streaming compressor. The
    // BLAKE3 hash it also computes is unused here; we only want the bytes.
    var out: std.Io.Writer.Allocating = .init(arena);

    var compressor = try bundle.streaming_writer.CompressingHashWriter.init(
        &gpa,
        bundle.DEFAULT_COMPRESSION_LEVEL,
        &out.writer,
        bundle.allocForZstd,
        bundle.freeForZstd,
    );
    defer compressor.deinit();

    try compressor.interface.writeAll(input);
    try compressor.finish();

    const compressed = out.written();

    try std.Io.Dir.cwd().writeFile(io, .{
        .sub_path = output_path,
        .data = compressed,
    });
}
