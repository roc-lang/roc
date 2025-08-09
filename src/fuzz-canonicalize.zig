//! Fuzzing for the Roc canonicalization phase
//!
//! This fuzzer tests the canonicalization phase of the Roc compiler, which transforms
//! the parsed AST into a canonical intermediate representation (CIR). The fuzzer is
//! designed to bias toward inputs that successfully parse and pass through canonicalization
//! without errors, as these provide better code coverage for finding edge cases.
//!
//! ## Building
//!
//! Note: Compiling the fuzz tests requires llvm and does not currently work in our nix shell on all systems.
//!
//! To build with fuzzing support:
//!   zig build -Dfuzz
//!
//! To build just the repro executable (no AFL++ required):
//!   zig build repro-canonicalize
//!
//! ## Running
//!
//! To run the fuzzer:
//!  1. zig build -Dfuzz
//!  2. zig build snapshot -- --fuzz-corpus /tmp/corpus  # Optional: generate corpus from snapshots
//!  3. ./zig-out/AFLplusplus/bin/afl-fuzz -i src/fuzz-corpus/canonicalize -o /tmp/canonicalize-out/ zig-out/bin/fuzz-canonicalize
//!
//! To reproduce a crash:
//!   ./zig-out/bin/repro-canonicalize /tmp/canonicalize-out/default/crashes/id:000000...
//!   # Or with verbose output:
//!   ./zig-out/bin/repro-canonicalize --verbose /tmp/canonicalize-out/default/crashes/id:000000...
//!
//! ## Corpus
//!
//! The initial corpus in src/fuzz-corpus/canonicalize/ contains simple valid Roc programs
//! that pass parsing and canonicalization. This helps the fuzzer start with good examples
//! that explore more of the canonicalization code paths rather than getting stuck on
//! parse errors.
//!
//! Other afl commands also available in `./zig-out/AFLplusplus/bin`

const std = @import("std");
const compile = @import("compile");
const BuildEnv = compile.BuildEnv;
const reporting = @import("reporting");

/// Hook for AFL++ to initialize the fuzz test environment.
pub export fn zig_fuzz_init() void {}

/// Hook for AFL++ to run the fuzz test.
pub export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    zig_fuzz_test_inner(buf, len, false);
}

/// Inner implementation that can be called with debug flag for repro executable
pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    // We reinitialize the gpa on every loop of the fuzzer.
    // This enables the gpa to do leak checking on each iteration.
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        _ = gpa_impl.deinit();
    }
    const gpa = gpa_impl.allocator();

    const input = buf[0..@intCast(len)];

    if (debug) {
        std.debug.print("Input:\n==========\n{s}\n==========\n\n", .{input});
    }

    // Write input to a temporary file
    const tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_file_path = "fuzz_input.roc";
    try tmp_dir.dir.writeFile(tmp_file_path, input);

    // Get absolute path
    var path_buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const abs_path = try tmp_dir.dir.realpath(tmp_file_path, &path_buf);

    // Process the input through BuildEnv
    var build_env = BuildEnv.init(gpa, .single_threaded, 1);
    defer build_env.deinit();

    build_env.build(abs_path) catch |err| {
        switch (err) {
            error.OutOfMemory => @panic("OOM"),
            error.NoSpaceLeft => @panic("No Space Left"),
            error.TooNested => @panic("Too much nesting"),
            else => {},
        }
    };

    // Drain reports
    const drained = try build_env.drainReports();
    defer {
        for (drained) |mod| {
            gpa.free(mod.abs_path);
            for (mod.reports) |*report| {
                var mut_report = report;
                mut_report.deinit();
            }
            gpa.free(mod.reports);
        }
        gpa.free(drained);
    }

    if (debug) {
        var total_reports: usize = 0;
        for (drained) |mod| {
            total_reports += mod.reports.len;
        }
        std.debug.print("Processing completed with {} reports\n", .{total_reports});
        if (total_reports > 0) {
            std.debug.print("Reports:\n", .{});
            for (drained) |mod| {
                for (mod.reports) |report| {
                    std.debug.print("  - {s}\n", .{report.title});
                }
            }
        }
    }

    // Success! The input passed through the full compiler pipeline
    // The fuzzer will be biased toward inputs that reach this point
    // since they provide more code coverage
}
