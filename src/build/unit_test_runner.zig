//! Zig's stock test runner with stack-trace capture controlled by Roc's
//! `-Ddebug-gpa-traces` option.
//!
//! `std.testing.allocator` always uses a leak-checking DebugAllocator with ten
//! stack frames. Capturing those frames through Mach-O/DWARF dominates
//! allocation-heavy test runtime on macOS arm64. The default remains traceless
//! while preserving leak, double-free, and safety checks; pass
//! `-Ddebug-gpa-traces` when allocation sites are needed for diagnosis.
//!
//! Zig exposes this through the root module's global `allow_stack_tracing`
//! option, so the default also suppresses panic and error-return stack traces
//! in unit-test binaries. The opt-in flag restores all of those traces.

const std = @import("std");
const build_options = @import("build_options");
const default_test_runner = @import("zig_default_test_runner");

/// Inherit Zig's stock test-runner options and only override stack tracing.
pub const std_options: std.Options = options: {
    var options = default_test_runner.std_options;
    options.allow_stack_tracing = build_options.debug_gpa_traces;
    break :options options;
};

/// Delegate test discovery, execution, and reporting to Zig's stock runner.
pub fn main(init: std.process.Init.Minimal) void {
    default_test_runner.main(init);
}
