//! Fuzzing for build lowering of rejected programs: the shared typed source
//! generator writes a few expressions at a wrong type, and the program must
//! still lower, with checked crashes in place of whatever checking rejected.
//!
//! To build just the repro executable, which runs every fuzz target:
//!   zig build build-repro
//!
//! To run one input:
//!   ./zig-out/bin/repro build-errors --verbose <seed_file>
//!
//! To run with AFL++:
//!   zig build -Dfuzz
//!   mkdir -p /tmp/roc-build-errors-corpus
//!   printf '\0' > /tmp/roc-build-errors-corpus/seed
//!   ./zig-out/AFLplusplus/bin/afl-fuzz -t 480000+ -i /tmp/roc-build-errors-corpus -o /tmp/roc-build-errors-out zig-out/bin/fuzz-build-errors

const BuildFuzzDriver = @import("BuildFuzzDriver.zig");

/// Prepares the fuzz test environment once per process.
pub fn zig_fuzz_init() void {}

pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    BuildFuzzDriver.run(buf, len, debug, .type_errors);
}
