const std = @import("std");
const tokenize = @import("src/check/parse/tokenize.zig");
const Cursor = tokenize.Cursor;
const Diagnostic = tokenize.Diagnostic;
const build_options = @import("build_options");
const fs = std.fs;

const RUNS = 10000;
const SIZES = [_]usize{
    16,
    64,
    256,
    1024,
    4096,
    16384,
    // 65536, integer overflow in tokenize
};

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const allocator = std.heap.page_allocator;

    try stdout.print("=== chompTrivia Performance Test ===\n", .{});
    try stdout.print("SIMD Enabled: {}\n", .{comptime Cursor.isSimdEnabled()});
    try stdout.print("SIMD Width: {} bytes\n\n", .{comptime Cursor.getSimdWidth()});

    // Test with a real Roc file
    try stdout.print("Testing with a real Roc file (src/BigFile.roc):\n", .{});
    try stdout.print("Time (µs) | Throughput (MB/s) | SIMD Chunks | File Size (bytes)\n", .{});
    try stdout.print("------------------------------------------------------------\n", .{});

    // Read the BigFile.roc
    const file_path = "src/BigFile.roc";
    const file = try fs.cwd().openFile(file_path, .{});
    defer file.close();

    const file_size = try file.getEndPos();
    const file_data = try file.readToEndAlloc(allocator, file_size);
    defer allocator.free(file_data);

    // Warmup
    for (0..10) |_| {
        var messages = std.ArrayList(Diagnostic).init(allocator);
        defer messages.deinit();

        var cursor = Cursor.init(file_data, messages.items);
        _ = cursor.chompTrivia();
    }

    var timer = try std.time.Timer.start();
    var total_simd_chunks: u64 = 0;

    const start = timer.read();
    for (0..RUNS) |_| {
        var messages = std.ArrayList(Diagnostic).init(allocator);
        defer messages.deinit();

        var cursor = Cursor.init(file_data, messages.items);
        _ = cursor.chompTrivia();
        total_simd_chunks += cursor.getChompTriviaSimdChunksProcessed();
    }
    const end = timer.read();

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

    // try stdout.print("Testing with all spaces:\n", .{});
    // try stdout.print("Size (bytes) | Time (µs) | Throughput (MB/s) | SIMD Chunks\n", .{});
    // try stdout.print("------------------------------------------------------------\n", .{});

    // for (SIZES) |size| {
    //     // Create test data
    //     const data = try allocator.alloc(u8, size);
    //     defer allocator.free(data);
    //     @memset(data, ' ');

    //     // Warmup
    //     for (0..10) |_| {
    //         var messages = std.ArrayList(Diagnostic).init(allocator);
    //         defer messages.deinit();

    //         var cursor = Cursor.init(data, messages.items);
    //         _ = cursor.chompTrivia();
    //     }

    //     var timer = try std.time.Timer.start();
    //     var total_simd_chunks: u64 = 0;

    //     const start = timer.read();
    //     for (0..RUNS) |_| {
    //         var messages = std.ArrayList(Diagnostic).init(allocator);
    //         defer messages.deinit();

    //         var cursor = Cursor.init(data, messages.items);
    //         _ = cursor.chompTrivia();
    //         total_simd_chunks += cursor.getChompTriviaSimdChunksProcessed();
    //     }
    //     const end = timer.read();

    //     const total_ns = end - start;
    //     const ns_per_run = total_ns / RUNS;
    //     const us_per_run = @as(f64, @floatFromInt(ns_per_run)) / 1000.0;
    //     const throughput_mbps = @as(f64, @floatFromInt(size)) * 1000.0 / @as(f64, @floatFromInt(ns_per_run));
    //     const avg_simd_chunks = total_simd_chunks / RUNS;

    //     try stdout.print("{:>12} | {:>9.2} | {:>17.2} | {:>11}\n", .{
    //         size,
    //         us_per_run,
    //         throughput_mbps,
    //         avg_simd_chunks,
    //     });
    // }

    // // Test mixed spaces and tabs
    // try stdout.print("\nTesting with mixed spaces and tabs:\n", .{});
    // try stdout.print("Size (bytes) | Time (µs) | Throughput (MB/s) | SIMD Chunks\n", .{});
    // try stdout.print("------------------------------------------------------------\n", .{});

    // for (SIZES) |size| {
    //     const data = try allocator.alloc(u8, size);
    //     defer allocator.free(data);

    //     // Create alternating pattern
    //     for (0..size) |i| {
    //         data[i] = if (i % 2 == 0) ' ' else '\t';
    //     }

    //     var timer = try std.time.Timer.start();
    //     var total_simd_chunks: u64 = 0;

    //     const start = timer.read();
    //     for (0..RUNS) |_| {
    //         var messages = std.ArrayList(Diagnostic).init(allocator);
    //         defer messages.deinit();

    //         var cursor = Cursor.init(data, messages.items);
    //         _ = cursor.chompTrivia();
    //         total_simd_chunks += cursor.getChompTriviaSimdChunksProcessed();
    //     }
    //     const end = timer.read();

    //     const total_ns = end - start;
    //     const ns_per_run = total_ns / RUNS;
    //     const us_per_run = @as(f64, @floatFromInt(ns_per_run)) / 1000.0;
    //     const throughput_mbps = @as(f64, @floatFromInt(size)) * 1000.0 / @as(f64, @floatFromInt(ns_per_run));
    //     const avg_simd_chunks = total_simd_chunks / RUNS;

    //     try stdout.print("{:>12} | {:>9.2} | {:>17.2} | {:>11}\n", .{
    //         size,
    //         us_per_run,
    //         throughput_mbps,
    //         avg_simd_chunks,
    //     });
    // }
}
