//! Test root for build-helper units that live outside every module test root.
//!
//! test_harness.zig and stack_probe.zig are consumed as module imports by
//! executables (test runners, embedded_lld), so their tests are only collected
//! when this file is used as a `zig test` root.

const std = @import("std");

test {
    std.testing.refAllDecls(@import("test_harness.zig"));
    std.testing.refAllDecls(@import("stack_probe.zig"));
}
