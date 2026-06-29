//! Deep-nesting stack-safety stress tests for the checker.
//!
//! ## Two nesting shapes — and why they differ
//!
//! Blocks can nest in two structurally different ways, and the checker walks them
//! with different native-stack characteristics:
//!
//!   1. final_expr nesting — `{ { { 0 } } }`: each block's body IS another block.
//!      The checker walks this `final_expr` spine on an explicit work stack
//!      (`checkExprIter`), so it uses O(1) native stack depth regardless of how
//!      deep the nesting is. `nestedBlocks` / `NESTING_DEPTH` exercise this and
//!      pass at depth 4000.
//!
//!   2. statement nesting — `x0 = { x1 = { x2 = { ... 0 ... } } }`: each level is a
//!      declaration binding whose RHS is the next nested block. This path runs
//!      through `checkBlockStatements`, which recurses (`checkBlockStatements →
//!      checkExpr → checkExprIter` per level), so it uses O(depth) native frames.
//!      `stmtNestedBlocks` / `STMT_NESTING_DEPTH` exercise this and pass only up to
//!      a safe depth.
//!
//! ## Regression-guard intent (shape 1)
//!
//! If the `e_block` final_expr spine ever stops being walked iteratively, thousands
//! of final_expr-nested blocks overflow the native stack (SIGSEGV via the
//! compiler-rt stack probe) — long before the interim depth guard
//! (`MAX_CHECK_RECURSION_DEPTH`) can convert it into a returned `error.OutOfMemory`.
//! The shape-1 test guards against exactly that: do NOT weaken it or the depth
//! guard to "fix" such a regression — its job is to fail loudly if the spine stops
//! being iterative.
//!
//! ## Measured ceilings (this codebase, native stack)
//!
//! Empirically (depth sweep):
//!   - parse + canonicalization recurse for nested blocks and overflow the native
//!     stack around ~8.5k blocks (statement-nested shape: completes at 8000, SEGVs
//!     by 8500).
//!   - the checker walks final_expr nesting in O(1) (passes well past 4000, the
//!     value chosen for `NESTING_DEPTH`), but overflows STATEMENT-nested blocks in
//!     the low hundreds of levels (around 300–600) — far below canon's ~8.5k. So
//!     for statement nesting the CHECKER, not parse/canon, is the limiting factor.
//!
//! ## Honest coverage note
//!
//! These tests do NOT prove the checker is O(1) for all block nesting — only for
//! the final_expr spine. Statement nesting remains O(depth) and is guarded only up
//! to `STMT_NESTING_DEPTH` (deliberately well under its crash point, since a test
//! that overflowed would SIGSEGV the entire test runner, not just one test).
//! Reifying `checkBlockStatements` onto the work stack would make statement nesting
//! O(1) as well, after which `STMT_NESTING_DEPTH` could be raised to match
//! `NESTING_DEPTH`.

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

/// Nesting depth for the FINAL_EXPR stress program (shape 1; see the module doc
/// comment). The checker walks this spine on the work stack (O(1) native depth),
/// so it passes here comfortably; parse+canon (which recurse) clear ~8.5k for this
/// shape, leaving ~2x headroom at 4000.
///
/// CI fragility: the parse+canon ceiling depends on the native stack size. A
/// runner with a much smaller stack could push canon's ceiling below this value,
/// making the test SIGSEGV in canon (unwinnable until parse+canon are also walked
/// iteratively). If that happens, lower this depth.
const NESTING_DEPTH: usize = 4_000;

/// Build `main! = |_args| { { { ... { 0 } ... } } }` nested `depth` deep, purely
/// via nested blocks (the final_expr spine the checker walks iteratively).
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
    // a specific type — only that the pipeline finishes. The checker walks this
    // final_expr spine on the work stack, so it completes without overflowing the
    // native stack however deep the nesting goes.
    const diags = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diags);
}

/// Nesting depth for the STATEMENT-nested stress program (see the module doc
/// comment, "Two nesting shapes"). Unlike `NESTING_DEPTH` above, this shape is NOT
/// O(1) on the native stack: `checkBlockStatements` recurses, so each statement-RHS
/// block level adds a few native frames (`checkBlockStatements → checkExpr →
/// checkExprIter`). On this codebase the checker overflows the native stack
/// (SIGSEGV via the compiler-rt stack probe) for this shape in the low hundreds of
/// levels (around 300–600), far below parse+canon's ~8.5k ceiling for the same
/// shape (canon completes at 8000, SEGVs by 8500) — so the CHECKER is the
/// bottleneck for statement nesting.
///
/// This depth is set well under that ceiling with comfortable margin, so the test
/// passes and stays robust against CI runners with smaller native stacks. It is a
/// floor-guard: it proves statement-nested blocks check correctly at least this
/// deep, NOT that they are O(1). Reifying `checkBlockStatements` onto the work
/// stack would make this shape O(1) like `nestedBlocks`, after which this depth
/// could be raised to match `NESTING_DEPTH`.
const STMT_NESTING_DEPTH: usize = 150;

/// Build `main! = |_args| { x0 = { x1 = { ... { 0 } ... } } }` nested `depth`
/// deep via STATEMENT RHS: each level is a single declaration binding whose RHS
/// is the next nested block, with the binding referenced as the block's final
/// expression. This exercises the recursive `checkBlockStatements` path (contrast
/// `nestedBlocks`, which nests purely via the iterative `final_expr` spine).
fn stmtNestedBlocks(gpa: std.mem.Allocator, depth: usize) ![]u8 {
    var buf = std.ArrayList(u8).empty;
    errdefer buf.deinit(gpa);
    var numbuf: [24]u8 = undefined;
    try buf.appendSlice(gpa, "main! = |_args| ");
    var i: usize = 0;
    while (i < depth) : (i += 1) {
        try buf.appendSlice(gpa, "{\nx");
        try buf.appendSlice(gpa, try std.fmt.bufPrint(&numbuf, "{d}", .{i}));
        try buf.appendSlice(gpa, " = ");
    }
    try buf.appendSlice(gpa, "{\n0\n}\n");
    i = depth;
    while (i > 0) {
        i -= 1;
        try buf.appendSlice(gpa, "x");
        try buf.appendSlice(gpa, try std.fmt.bufPrint(&numbuf, "{d}", .{i}));
        try buf.appendSlice(gpa, "\n}\n");
    }
    return buf.toOwnedSlice(gpa);
}

test "deep nesting: statement-nested blocks (partial coverage)" {
    const gpa = std.testing.allocator;
    const src = try stmtNestedBlocks(gpa, STMT_NESTING_DEPTH);
    defer gpa.free(src);

    var test_env = try TestEnv.init("DeepStmt", src);
    defer test_env.deinit();
    // Success criterion: checking completes (no crash). This is a FLOOR guard for
    // statement-nested blocks, which are O(depth) on the native stack because the
    // recursive `checkBlockStatements` is not walked iteratively. It passes at this
    // safe depth; it would SIGSEGV in the low hundreds of levels (see
    // STMT_NESTING_DEPTH). Do NOT raise STMT_NESTING_DEPTH toward that ceiling — a
    // depth that overflows would crash the whole test runner, not just this test.
    // Reifying checkBlockStatements onto the work stack would make this shape O(1),
    // after which the depth could match NESTING_DEPTH.
    const diags = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diags);
}
