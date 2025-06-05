const std = @import("std");
const base = @import("src/base.zig");
const tokenize = @import("src/check/parse/tokenize.zig");
const Cursor = tokenize.Cursor;
const Tokenizer = tokenize.Tokenizer;
const Diagnostic = tokenize.Diagnostic;
const build_options = @import("build_options");
const fs = std.fs;

const RUNS = 1000;
const FILE_PATH = "src/BigFile.roc";

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;

    try stdout.print("=== chompTrivia Performance Test ===\n", .{});
    try stdout.print("SIMD Enabled: {}\n", .{comptime Cursor.isSimdEnabled()});
    try stdout.print("SIMD Width: {} bytes\n\n", .{comptime Cursor.getSimdWidth()});

    try stdout.print("Testing with {s}\n", .{FILE_PATH});
    try stdout.print("Time (µs) | Throughput (MB/s) | SIMD Chunks | File Size (bytes)\n", .{});
    try stdout.print("------------------------------------------------------------\n", .{});

    // Read the test file
    const file = try fs.cwd().openFile(FILE_PATH, .{});
    defer file.close();
    const file_size = try file.getEndPos();
    const file_data = try file.readToEndAlloc(allocator, file_size);
    defer allocator.free(file_data);

    var timer = try std.time.Timer.start();
    var total_simd_chunks: u64 = 0;
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        _ = gpa_impl.deinit();
    }
    const gpa = gpa_impl.allocator();

    // Tokenize multiple times
    const start = timer.read();
    for (0..RUNS) |_| {
        var module_env = base.ModuleEnv.init(gpa);
        defer module_env.deinit();

        // Create tokenizer
        var token_buffer = tokenize.TokenizedBuffer.initCapacity(&module_env, 1024);
        defer token_buffer.deinit();

        var messages = std.ArrayList(Diagnostic).init(allocator);
        defer messages.deinit();

        var tokenizer = Tokenizer.init(
            &module_env,
            file_data,
            messages.items,
        );
        defer tokenizer.deinit();

        // Tokenize the entire file
        tokenizer.tokenize();

        // Count SIMD chunks - we need to access the cursor to get this
        total_simd_chunks += tokenizer.cursor.getChompTriviaSimdChunksProcessed();
    }
    const end = timer.read();

    // Summarise results
    const total_ns = end - start;
    const ns_per_run = total_ns / RUNS;
    const us_per_run = @as(f64, @floatFromInt(ns_per_run)) / 1000.0;
    const throughput_mbps = @as(f64, @floatFromInt(file_size)) * 1000.0 / @as(f64, @floatFromInt(ns_per_run));
    const avg_simd_chunks = total_simd_chunks / RUNS;

    try stdout.print("{:>9.2} | {:>17.2} | {:>11} | {:>17}\n", .{
        us_per_run,
        throughput_mbps,
        avg_simd_chunks,
        file_size,
    });
}
