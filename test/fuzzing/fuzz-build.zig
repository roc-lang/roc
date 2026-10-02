//! Fuzzing for build lowering using the shared typed source generator.
//!
//! To build just the repro executable:
//!   zig build build-repro-build
//!
//! To run one input:
//!   ./zig-out/bin/repro-build --verbose <seed_file>
//!
//! To run with AFL++:
//!   zig build -Dfuzz
//!   mkdir -p /tmp/roc-build-corpus
//!   printf '\0' > /tmp/roc-build-corpus/seed
//!   ./zig-out/AFLplusplus/bin/afl-fuzz -t 480000+ -i /tmp/roc-build-corpus -o /tmp/roc-build-out zig-out/bin/fuzz-build

const BuildFuzzDriver = @import("BuildFuzzDriver.zig");

pub export fn zig_fuzz_init() void {}

pub export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    zig_fuzz_test_inner(buf, len, false);
}

pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    BuildFuzzDriver.run(buf, len, debug, .well_typed);
}
