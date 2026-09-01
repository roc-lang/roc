//! The stack budget for every thread that executes Roc code or deeply
//! recursive compiler passes.
//!
//! `build.zig` applies `roc_stack_size` to the compiler executables'
//! `stack_size`, which becomes the PT_GNU_STACK program header; Zig's start
//! code raises RLIMIT_STACK to match, so the main thread gets this budget.
//! Every spawned thread that can run Roc code (test-runner workers,
//! compile-time evaluation workers, compile coordinator workers) must pass the
//! same value to `std.Thread.spawn`, so a Roc program's recursion depth limit
//! never depends on which thread the work happens to be scheduled on.
//!
//! This file must stay dependency-free: build.zig imports it by path.

/// Stack size, in bytes, for the main thread and for every spawned thread
/// that executes Roc code.
pub const roc_stack_size: usize = 64 * 1024 * 1024;
