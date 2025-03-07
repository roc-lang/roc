//! Note: Compiling the fuzz tests requires llvm and does not currently work in our nix shell on all systems.
//!
//! To run:
//!  1. zig build fuzz-parse
//!  2. ./zig-out/AFLplusplus/bin/afl-fuzz -i src/fuzz-corpus/parse/ -o /tmp/parse-out/ zig-out/bin/fuzz-parse
//!
//! Other afl commands also available in `./zig-out/AFLplusplus/bin`

const std = @import("std");
const fmt = @import("fmt.zig");
const parse = @import("check/parse.zig");
const base = @import("base.zig");

/// Hook for AFL++ to initialize the fuzz test environment.
pub export fn zig_fuzz_init() void {}

/// Hook for AFL++ to run the fuzz test.
pub export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    zig_fuzz_test_inner(buf, len, false);
}

/// todo
pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    // We reinitialize the gpa on every loop of the fuzzer.
    // This enables the gpa to do leak checking on each iteration.
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        _ = gpa_impl.deinit();
    }
    const gpa = gpa_impl.allocator();

    const input = buf[0..@intCast(len)];

    const result = fmt.moduleFmtsStable(gpa, input, debug) catch |err|
        switch (err) {
        error.ParseFailed => {
            // No issue. Just bad input we couldn't parse.
            return;
        },
        error.SecondParseFailed => {
            @panic("Parsing of formatter output failed");
        },
        error.FormattingNotStable => {
            @panic("Formatting not stable");
        },
    };
    gpa.free(result);
}
