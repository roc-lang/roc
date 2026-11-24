//! Regression test for https://github.com/roc-lang/roc/issues/13419.
//!
//! Calling many single-use monomorphizations with a known record of functions
//! must specialize each monomorphization out of line instead of collapsing all
//! of them and their loops into one unbounded caller procedure.

const std = @import("std");
const lir = @import("lir");
const layout = @import("layout");
const harness = @import("lower_to_lir_harness.zig");

const mega_collapse_app =
    \\Ops(a) : { parse : U64 -> a, score : a -> U64 }
    \\
    \\run_loop : U64, Ops(a) -> U64
    \\run_loop = |n, ops| {
    \\    var $acc = 0.U64
    \\    var $i = 0.U64
    \\    while $i < n {
    \\        v = (ops.parse)($i)
    \\        $acc = $acc + (ops.score)(v)
    \\        $i = $i + 1
    \\    }
    \\    $acc
    \\}
    \\
    \\check_one : Str, U64, Ops(a) -> U64
    \\check_one = |label, n, ops| {
    \\    a = run_loop(n, ops)
    \\    b = run_loop(n + 1, ops)
    \\    c = (ops.score)((ops.parse)(n + 2))
    \\    d = (ops.score)((ops.parse)(n + 3))
    \\    e = (ops.score)((ops.parse)(n + 4))
    \\    f = (ops.score)((ops.parse)(n + 5))
    \\    g = (ops.score)((ops.parse)(n + 6))
    \\    h = (ops.score)((ops.parse)(n + 7))
    \\    i = (ops.score)((ops.parse)(n + 8))
    \\    j = (ops.score)((ops.parse)(n + 9))
    \\    k = (ops.score)((ops.parse)(n + 10))
    \\    l = (ops.score)((ops.parse)(n + 11))
    \\    a + b + c + d + e + f + g + h + i + j + k + l + label.count_utf8_bytes()
    \\}
    \\
    \\check_all : U64 -> U64
    \\check_all = |n| {
    \\    r0 = check_one("u8", n, { parse: |x| x.to_u8_wrap(), score: |v| v.to_u64() })
    \\    r1 = check_one("i8", n, { parse: |x| x.to_i8_wrap(), score: |v| v.to_u64_wrap() })
    \\    r2 = check_one("u16", n, { parse: |x| x.to_u16_wrap(), score: |v| v.to_u64() })
    \\    r3 = check_one("i16", n, { parse: |x| x.to_i16_wrap(), score: |v| v.to_u64_wrap() })
    \\    r4 = check_one("u32", n, { parse: |x| x.to_u32_wrap(), score: |v| v.to_u64() })
    \\    r5 = check_one("i32", n, { parse: |x| x.to_i32_wrap(), score: |v| v.to_u64_wrap() })
    \\    r6 = check_one("u64", n, { parse: |x| x.plus_wrap(1), score: |v| v.plus_wrap(2) })
    \\    r7 = check_one("i64", n, { parse: |x| x.to_i64_wrap(), score: |v| v.to_u64_wrap() })
    \\    r8 = check_one("u128", n, { parse: |x| x.to_u128(), score: |v| v.to_u64_wrap() })
    \\    r9 = check_one("i128", n, { parse: |x| x.to_i128(), score: |v| v.to_u64_wrap() })
    \\    r0 + r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9
    \\}
    \\
    \\main! = |args| {
    \\    echo!(check_all(args.len()).to_str())
    \\    Ok({})
    \\}
;

fn expectSeparateSpecializedCheckOneProcs(
    store: *const lir.LirStore,
    _: *const layout.Store,
) harness.LowerToLirHarnessError!void {
    var check_one_procs: usize = 0;
    var run_loop_procs: usize = 0;
    for (0..store.procSpecCount()) |proc_index| {
        const proc_id: lir.LIR.LirProcSpecId = @fromBackingInt(@intCast(proc_index));
        const spec = store.getProcSpecImmediate(proc_id);
        if (spec.body == .none) continue;
        const name = store.procDebugName(proc_id) orelse continue;
        if (std.mem.eql(u8, name, "check_one")) check_one_procs += 1;
        if (std.mem.eql(u8, name, "run_loop")) run_loop_procs += 1;
    }
    // All 10 monomorphizations of `check_one` (and their `run_loop` workers)
    // stay out of line instead of collapsing into `check_all` / `main!`.
    try std.testing.expectEqual(@as(usize, 10), check_one_procs);
    try std.testing.expectEqual(@as(usize, 10), run_loop_procs);
}

test "issue 13419: many single-use monomorphizations with known callable records stay separate LIR procedures" {
    try harness.expectLirInspectionWithOptions(
        mega_collapse_app,
        .{ .inline_mode = .wrappers, .spec_constr_clone_inlining = .all_calls },
        expectSeparateSpecializedCheckOneProcs,
    );
}

test "issue 13419: bounded single-use inlining is deterministic across SpecConstr schedules" {
    try harness.expectSpecConstrParallelismDeterministicLir(
        mega_collapse_app,
        .{ .inline_mode = .wrappers, .spec_constr_clone_inlining = .all_calls },
        &.{},
    );
}
