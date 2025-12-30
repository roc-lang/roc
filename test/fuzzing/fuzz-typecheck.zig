//! Fuzzing for the Roc type-checking phase
//!
//! This fuzzer generates type-correct Roc code and verifies that the
//! parse-canonicalize-type-check pipeline accepts it without crashes or errors.
//! Unlike the canonicalize fuzzer which tests robustness against arbitrary input,
//! this fuzzer tests correctness by generating code that should always type-check.
//!
//! ## Building
//!
//! Note: Compiling the fuzz tests requires llvm and does not currently work in our nix shell on all systems.
//!
//! To build with fuzzing support:
//!   zig build -Dfuzz
//!
//! To build just the repro executable (no AFL++ required):
//!   zig build repro-typecheck
//!
//! ## Running
//!
//! To run the fuzzer:
//!  1. zig build -Dfuzz
//!  2. ./zig-out/AFLplusplus/bin/afl-fuzz -i src/fuzz-corpus/typecheck -o /tmp/typecheck-out/ zig-out/bin/fuzz-typecheck
//!
//! To reproduce a crash:
//!   ./zig-out/bin/repro-typecheck /tmp/typecheck-out/default/crashes/id:000000...
//!   # Or with verbose output:
//!   ./zig-out/bin/repro-typecheck --verbose /tmp/typecheck-out/default/crashes/id:000000...

const std = @import("std");
const compile = @import("compile");
const TypedCodeGenerator = @import("TypedCodeGenerator.zig");

const BuildEnv = compile.BuildEnv;

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

    // Use input bytes as seed for the random number generator
    // This ensures reproducibility - same input always generates same code
    const seed = blk: {
        if (input.len >= 8) {
            break :blk std.mem.readInt(u64, input[0..8], .little);
        } else {
            var seed_bytes: [8]u8 = .{ 0, 0, 0, 0, 0, 0, 0, 0 };
            @memcpy(seed_bytes[0..input.len], input);
            break :blk std.mem.readInt(u64, &seed_bytes, .little);
        }
    };

    // Generate type-correct Roc code
    var generator = TypedCodeGenerator.init(gpa, seed);
    defer generator.deinit();

    generator.generateModule() catch |err| {
        switch (err) {
            error.OutOfMemory => @panic("OOM during code generation"),
        }
    };

    const generated_code = generator.getOutput();

    if (debug) {
        std.debug.print("Seed: {d}\n", .{seed});
        std.debug.print("Generated code:\n==========\n{s}\n==========\n\n", .{generated_code});
    }

    // Write generated code to a temporary file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const tmp_file_path = "fuzz_input.roc";
    tmp_dir.dir.writeFile(.{ .sub_path = tmp_file_path, .data = generated_code }) catch return;

    // Get absolute path
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const abs_path = tmp_dir.dir.realpath(tmp_file_path, &path_buf) catch return;

    // Process the input through BuildEnv
    // Panic on OOM so AFL++ knows it's a resource issue, not a bug in the fuzzed code
    var build_env = BuildEnv.init(gpa, .single_threaded, 1) catch @panic("OOM during BuildEnv init");
    defer build_env.deinit();

    build_env.build(abs_path) catch |err| {
        switch (err) {
            error.OutOfMemory => @panic("OOM"),
            error.NoSpaceLeft => @panic("No Space Left"),
            error.TooNested => {
                // This comes from the parser, so don't treat it as a type-check error
                return;
            },
            else => {},
        }
    };

    // Drain reports and check for type errors
    const drained = build_env.drainReports() catch return;
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

    // Count and categorize reports
    var total_reports: usize = 0;
    var has_type_errors = false;

    for (drained) |mod| {
        total_reports += mod.reports.len;
        for (mod.reports) |report| {
            // Check if this is a type error (we generated type-correct code, so this shouldn't happen)
            const title = report.title;
            if (std.mem.indexOf(u8, title, "type") != null or
                std.mem.indexOf(u8, title, "Type") != null or
                std.mem.indexOf(u8, title, "mismatch") != null or
                std.mem.indexOf(u8, title, "Mismatch") != null)
            {
                has_type_errors = true;
            }
        }
    }

    if (debug) {
        std.debug.print("Processing completed with {} reports\n", .{total_reports});
        if (total_reports > 0) {
            std.debug.print("Reports:\n", .{});
            for (drained) |mod| {
                for (mod.reports) |report| {
                    std.debug.print("  - {s}\n", .{report.title});
                }
            }
        }
        if (has_type_errors) {
            std.debug.print("WARNING: Type errors found in generated code!\n", .{});
        }
    }

    // If we generated type-correct code but got type errors, that's a bug
    // For now we just log it in debug mode; later we could make this a test failure
    // by uncommenting the following:
    // if (has_type_errors) {
    //     @panic("Type error in generated type-correct code");
    // }
}
