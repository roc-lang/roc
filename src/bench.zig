//! Benchmarking utility for the Roc compiler, exposed as --z-* arguments in the main binary.

const std = @import("std");
const base = @import("base");
const tracy = @import("tracy.zig");
const fmt = @import("fmt.zig");
const collections = @import("collections");

const tokenize = @import("check/parse/tokenize.zig");
const parse = @import("check/parse.zig");
const ModuleEnv = @import("compile/ModuleEnv.zig");

const Allocator = std.mem.Allocator;

const RocFile = struct {
    path: []const u8,
    content: []const u8,
};

const FileMetrics = struct {
    total_bytes: usize,
    total_lines: usize,
};

const BenchmarkResults = struct {
    path: []const u8,
    num_files: usize,
    total_bytes: usize,
    total_lines: usize,
    total_tokens: u64,
    num_iterations: usize,
    total_time: u64,
};

fn benchParseOrTokenize(comptime is_parse: bool, gpa: Allocator, path: []const u8) !void {
    const operation_name = if (is_parse) "parse" else "tokenizer";
    std.debug.print("Benchmarking {s} on '{s}'\n", .{ operation_name, path });

    // Find all .roc files (from file or directory)
    var roc_files = std.ArrayList(RocFile).init(gpa);
    defer {
        for (roc_files.items) |roc_file| {
            gpa.free(roc_file.path);
            gpa.free(roc_file.content);
        }
        roc_files.deinit();
    }

    try collectRocFiles(gpa, path, &roc_files);

    if (roc_files.items.len == 0) {
        std.debug.print("No .roc files found in '{s}'\n", .{path});
        return;
    }

    std.debug.print("Found {} .roc files\n", .{roc_files.items.len});

    // Calculate total metrics
    const metrics = calculateMetrics(roc_files.items);

    std.debug.print("Total: {} bytes, {} lines\n", .{ metrics.total_bytes, metrics.total_lines });

    // Create a module environment for tokenization (reused for tokenizer, created per-iteration for parser)
    var env: ?ModuleEnv = if (!is_parse) try ModuleEnv.init(gpa, "") else null;
    defer if (env) |*e| e.deinit();

    // Benchmark parameters
    const num_iterations = 100;
    var total_time: u64 = 0;
    var total_tokens: u64 = 0;

    std.debug.print("Running {} iterations...\n", .{num_iterations});

    // Benchmark loop
    for (0..num_iterations) |_| {
        const start_time = std.time.nanoTimestamp();

        var iteration_tokens: u64 = 0;

        // Process all files in this iteration
        for (roc_files.items) |roc_file| {
            if (is_parse) {
                // Parse mode

                // ModuleEnv takes ownership of the source code, so we need to dupe it each iteration
                const source_copy = try gpa.dupe(u8, roc_file.content);
                var parse_env = try ModuleEnv.init(gpa, source_copy);

                var ir = try parse.parse(&parse_env);
                iteration_tokens += ir.tokens.tokens.len;
                ir.deinit(gpa);
                parse_env.deinit();
            } else {
                // Tokenize mode
                var messages: [128]tokenize.Diagnostic = undefined;
                const msg_slice = messages[0..];

                var tokenizer = try tokenize.Tokenizer.init(&env.?, roc_file.content, msg_slice);
                try tokenizer.tokenize();
                var result = tokenizer.finishAndDeinit();
                iteration_tokens += result.tokens.tokens.len;
                result.tokens.deinit();
            }
        }

        const end_time = std.time.nanoTimestamp();

        total_time += @intCast(end_time - start_time);
        total_tokens = iteration_tokens;

        std.debug.print(".", .{});
    }
    std.debug.print("\n", .{});

    const results = BenchmarkResults{
        .path = path,
        .num_files = roc_files.items.len,
        .total_bytes = metrics.total_bytes,
        .total_lines = metrics.total_lines,
        .total_tokens = total_tokens,
        .num_iterations = num_iterations,
        .total_time = total_time,
    };

    const result_name = if (is_parse) "Parse" else "Tokenizer";
    printBenchmarkResults(result_name, results);
}

/// Benchmarks the parsing of Roc files.
pub fn benchParse(gpa: Allocator, path: []const u8) !void {
    try benchParseOrTokenize(true, gpa, path);
}

/// Benchmarks the tokenization of Roc files.
pub fn benchTokenizer(gpa: Allocator, path: []const u8) !void {
    try benchParseOrTokenize(false, gpa, path);
}

fn collectRocFiles(gpa: Allocator, path: []const u8, roc_files: *std.ArrayList(RocFile)) !void {
    // Check if path is a file or directory
    const stat = std.fs.cwd().statFile(path) catch |err| {
        fatal("Failed to access '{s}': {}", .{ path, err });
    };

    switch (stat.kind) {
        .file => {
            if (std.mem.endsWith(u8, path, ".roc")) {
                try addRocFile(gpa, path, roc_files);
            } else {
                fatal("File '{s}' is not a .roc file", .{path});
            }
        },
        .directory => {
            try findRocFiles(gpa, path, roc_files);
        },
        else => {
            fatal("Path '{s}' is not a file or directory", .{path});
        },
    }
}

fn addRocFile(gpa: Allocator, file_path: []const u8, roc_files: *std.ArrayList(RocFile)) !void {
    const file = std.fs.cwd().openFile(file_path, .{}) catch |err| {
        std.debug.print("Warning: Failed to open file '{s}': {}\n", .{ file_path, err });
        return;
    };
    defer file.close();

    const file_size = try file.getEndPos();
    if (file_size > 0xffff_ffff) {
        std.debug.print("Warning: File '{s}' is too large to process ({} bytes), skipping\n", .{ file_path, file_size });
        return;
    }
    const file_size_usize: usize = @intCast(file_size);
    const content = try file.readToEndAlloc(gpa, file_size_usize);
    const owned_path = try gpa.dupe(u8, file_path);

    try roc_files.append(.{
        .path = owned_path,
        .content = content,
    });
}

fn findRocFiles(gpa: Allocator, dir_path: []const u8, roc_files: *std.ArrayList(RocFile)) !void {
    var dir = std.fs.cwd().openDir(dir_path, .{ .iterate = true }) catch |err| {
        fatal("Failed to open directory '{s}': {}", .{ dir_path, err });
    };
    defer dir.close();

    var iterator = dir.iterate();
    while (try iterator.next()) |entry| {
        const full_path = try std.fs.path.join(gpa, &[_][]const u8{ dir_path, entry.name });
        defer gpa.free(full_path);

        switch (entry.kind) {
            .file => {
                if (std.mem.endsWith(u8, entry.name, ".roc")) {
                    try addRocFile(gpa, full_path, roc_files);
                }
            },
            .directory => {
                // Recursively search subdirectories
                try findRocFiles(gpa, full_path, roc_files);
            },
            else => {
                // Ignore other file types
            },
        }
    }
}

fn calculateMetrics(roc_files: []const RocFile) FileMetrics {
    var total_bytes: usize = 0;
    var total_lines: usize = 0;

    for (roc_files) |roc_file| {
        total_bytes += roc_file.content.len;
        var line_count: usize = 1;
        for (roc_file.content) |byte| {
            if (byte == '\n') {
                line_count += 1;
            }
        }
        total_lines += line_count;
    }

    return FileMetrics{
        .total_bytes = total_bytes,
        .total_lines = total_lines,
    };
}

fn printBenchmarkResults(benchmark_name: []const u8, results: BenchmarkResults) void {
    // Calculate timing statistics
    const avg_time_ns = results.total_time / results.num_iterations;
    const avg_time_us = avg_time_ns / 1000;
    const avg_time_ms = avg_time_us / 1000;

    // Calculate throughput
    const tokens_per_second = if (avg_time_ns > 0)
        (@as(f64, @floatFromInt(results.total_tokens)) * 1_000_000_000.0) / @as(f64, @floatFromInt(avg_time_ns))
    else
        0.0;

    const bytes_per_second = if (avg_time_ns > 0)
        (@as(f64, @floatFromInt(results.total_bytes)) * 1_000_000_000.0) / @as(f64, @floatFromInt(avg_time_ns))
    else
        0.0;

    const lines_per_second = if (avg_time_ns > 0)
        (@as(f64, @floatFromInt(results.total_lines)) * 1_000_000_000.0) / @as(f64, @floatFromInt(avg_time_ns))
    else
        0.0;

    const files_per_second = if (avg_time_ns > 0)
        (@as(f64, @floatFromInt(results.num_files)) * 1_000_000_000.0) / @as(f64, @floatFromInt(avg_time_ns))
    else
        0.0;

    std.debug.print("\n=== {s} Benchmark Results ===\n", .{benchmark_name});
    std.debug.print("Path: {s}\n", .{results.path});
    std.debug.print("Files: {}\n", .{results.num_files});
    std.debug.print("Total size: {} bytes\n", .{results.total_bytes});
    std.debug.print("Total lines: {}\n", .{results.total_lines});
    std.debug.print("Tokens generated: {}\n", .{results.total_tokens});
    std.debug.print("Iterations: {}\n", .{results.num_iterations});
    std.debug.print("\nTiming:\n", .{});
    std.debug.print("  Average time: {} ms ({} μs, {} ns)\n", .{ avg_time_ms, avg_time_us, avg_time_ns });
    std.debug.print("  Total time: {d:.2} ms\n", .{@as(f64, @floatFromInt(results.total_time)) / 1_000_000.0});
    std.debug.print("\nThroughput:\n", .{});
    std.debug.print("  {d:.0} files/second\n", .{files_per_second});
    std.debug.print("  {d:.0} tokens/second\n", .{tokens_per_second});
    std.debug.print("  {d:.0} lines/second\n", .{lines_per_second});
    std.debug.print("  {d:.2} MB/second\n", .{bytes_per_second / (1024.0 * 1024.0)});
    std.debug.print("  {d:.2} bytes/token\n", .{@as(f64, @floatFromInt(results.total_bytes)) / @as(f64, @floatFromInt(results.total_tokens))});
}

/// Log a fatal error and exit the process with a non-zero code.
pub fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.io.getStdErr().writer().print(format, args) catch unreachable;
    if (tracy.enable) {
        tracy.waitForShutdown() catch unreachable;
    }
    std.process.exit(1);
}
