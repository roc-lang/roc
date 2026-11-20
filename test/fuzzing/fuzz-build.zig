//! Fuzzing for build lowering using the shared typed source generator.
//!
//! To build just the repro executable, which runs every fuzz target:
//!   zig build build-repro
//!
//! To run one input:
//!   ./zig-out/bin/repro build --verbose <seed_file>
//!
//! To run with AFL++:
//!   zig build -Dfuzz
//!   mkdir -p /tmp/roc-build-corpus
//!   printf '\0' > /tmp/roc-build-corpus/seed
//!   ./zig-out/AFLplusplus/bin/afl-fuzz -t 480000+ -i /tmp/roc-build-corpus -o /tmp/roc-build-out zig-out/bin/fuzz-build

const BuildFuzzDriver = @import("BuildFuzzDriver.zig");

/// Prepares the fuzz test environment once per process.
pub fn zig_fuzz_init() void {}

pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    BuildFuzzDriver.run(buf, len, debug, .well_typed);
}
