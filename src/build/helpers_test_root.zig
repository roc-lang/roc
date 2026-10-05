//! Test root for build-helper units that live outside every module test root.
//!
//! These helpers are consumed by executables or the build graph, so their tests
//! are only collected when this file is used as a `zig test` root.

const std = @import("std");

test {
    std.testing.refAllDecls(@import("test_harness.zig"));
    std.testing.refAllDecls(@import("stack_probe.zig"));
    std.testing.refAllDecls(@import("nix_path.zig"));
}
