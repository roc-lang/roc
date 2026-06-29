//! Deep-nesting stack-safety stress test for the iterative checker conversion.
//!
//! This test PASSES as of the e_block migration, which flattened the block
//! `final_expr` spine onto the work stack. Before that migration the checker
//! recursed natively for `e_block`, so a program with thousands of nested blocks
//! drove deep native recursion and overflowed the native stack (SIGSEGV via the
//! compiler-rt stack probe) — long before the interim depth guard
//! (`MAX_CHECK_RECURSION_DEPTH = 8192`) could convert it into a returned
//! `error.OutOfMemory`. It now serves as a regression guard: a future change
//! that reintroduces native recursion on the e_block spine would make it
//! SIGSEGV again. Do NOT weaken this test or the depth guard to "fix" such a
//! regression — its job is to fail loudly if the spine stops being iterative.
//!
//! Depth note: empirically on this codebase, parse+canonicalization (which still
//! recurse for nested blocks despite the work-stack front end) themselves
//! overflow the native stack around ~8.5k nested blocks, while the type checker
//! overflows far shallower (already by ~1.5k). The depth below is chosen to sit
//! comfortably in the window where parse+canon succeed but the checker overflows:
//! large enough that the checker reliably fails today, small enough that it will
//! genuinely complete once the checker is made iterative (parse+canon have ~2x
//! headroom at this depth). See the task report for the threshold sweep.

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

/// Nesting depth for the stress program. Sits in the window between the checker's
/// native-overflow point (~1.5k) and parse+canon's (~8.5k): deep enough to fail
/// reliably today, shallow enough to pass once the checker is iterative.
///
/// CI fragility: those ceilings depend on the native stack size. A runner with a
/// smaller stack could push canon's ceiling below this value, making the test
/// SIGSEGV in canon even AFTER the block migration (unwinnable). If that happens,
/// lower this depth (it only needs to clear the checker's ~1.5k ceiling), or raise
/// it once parse+canon are also made iterative.
const NESTING_DEPTH: usize = 4_000;

/// Build `main! = |_args| { { { ... { 0 } ... } } }` nested `depth` deep, purely
/// via nested blocks — the e_block spine this phase makes stack-safe.
fn nestedBlocks(gpa: std.mem.Allocator, depth: usize) ![]u8 {
    var buf = std.ArrayList(u8).empty;
    errdefer buf.deinit(gpa);
    try buf.appendSlice(gpa, "main! = |_args| {\n");
    var i: usize = 0;
    while (i < depth) : (i += 1) try buf.appendSlice(gpa, "{\n");
    try buf.appendSlice(gpa, "0\n");
    i = 0;
    while (i < depth) : (i += 1) try buf.appendSlice(gpa, "}\n");
    try buf.appendSlice(gpa, "}\n");
    return buf.toOwnedSlice(gpa);
}

test "deep nesting: nested blocks type-check without native stack overflow" {
    const gpa = std.testing.allocator;
    const src = try nestedBlocks(gpa, NESTING_DEPTH);
    defer gpa.free(src);

    var test_env = try TestEnv.init("Deep", src);
    defer test_env.deinit();
    // Success criterion: checking completes (no crash, no error). We don't assert
    // a specific type — only that the pipeline finishes. Before block migration
    // the checker overflows the native stack here (SIGSEGV); after migration it
    // completes.
    const diags = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diags);
}
