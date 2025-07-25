//! Note: Compiling the fuzz tests requires llvm and does not currently work in our nix shell on all systems.
//!
//! To run:
//!  1. zig build fuzz-tokenize
//!  2. zig build snapshot -- --fuzz-corpus /tmp/corpus
//!  3. ./zig-out/AFLplusplus/bin/afl-fuzz -i /tmp/corpus -o /tmp/tokenize-out/ zig-out/bin/fuzz-tokenize
//!
//! Other afl commands also available in `./zig-out/AFLplusplus/bin`

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const parse = @import("parse");

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

    const buf_slice = buf[0..@intCast(len)];

    parse.tokenize.checkTokenizerInvariants(gpa, buf_slice, debug) catch {
        @panic("Out of memory");
    };
}
