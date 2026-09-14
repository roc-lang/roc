//! End-to-end coverage for deterministic ordinary-specialization handoff.

const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");
const expectLowersToLirWithOptions = harness.expectLowersToLirWithOptions;
const expectPreparedFiniteCaptureFreeDirectCallsParallelismDeterministicLir =
    harness.expectPreparedFiniteCaptureFreeDirectCallsParallelismDeterministicLir;
const expectSolvedLirWorkerMetadataParallelismDeterministicLir =
    harness.expectSolvedLirWorkerMetadataParallelismDeterministicLir;
const expectSolvedLirCapturingBodyParallelismDeterministicLir =
    harness.expectSolvedLirCapturingBodyParallelismDeterministicLir;
const expectSpecializationParallelismDeterministicLir = harness.expectSpecializationParallelismDeterministicLir;
const expectEagerIteratorSpecializationParallelismDeterministicLir =
    harness.expectEagerIteratorSpecializationParallelismDeterministicLir;
const expectProcedureRootParallelismDeterministicLir = harness.expectProcedureRootParallelismDeterministicLir;

test "solved-LIR parallel metrics reset and report exact batch accounting" {
    const app_body =
        \\main! = |_args| Ok({})
    ;
    var metrics: lir.CheckedPipeline.SolvedLirParallelMetrics = .{
        .task_waves = 11,
        .tasks_submitted = 22,
        .tasks_committed = 33,
    };

    try expectLowersToLirWithOptions(app_body, .{
        .specialization_workers = 1,
        .solved_lir_parallel_metrics_out = &metrics,
    });
    try std.testing.expectEqual(@as(u64, 0), metrics.task_waves);
    try std.testing.expectEqual(@as(u64, 0), metrics.tasks_submitted);
    try std.testing.expectEqual(@as(u64, 0), metrics.tasks_committed);
    try std.testing.expectEqual(@as(u64, 0), metrics.workspace_initializations);
    try std.testing.expectEqual(@as(u64, 0), metrics.workspace_reuses);

    try expectLowersToLirWithOptions(app_body, .{
        .specialization_workers = 2,
        .parallel_procedure_root_fixture = true,
        .solved_lir_parallel_metrics_out = &metrics,
    });
    try std.testing.expect(metrics.task_waves > 0);
    try std.testing.expect(metrics.tasks_submitted > 0);
    try std.testing.expectEqual(metrics.tasks_submitted, metrics.tasks_committed);
    try std.testing.expectEqual(
        metrics.tasks_submitted,
        metrics.workspace_initializations + metrics.workspace_reuses,
    );

    try expectLowersToLirWithOptions(app_body, .{
        .specialization_workers = 1,
        .solved_lir_parallel_metrics_out = &metrics,
    });
    try std.testing.expectEqual(@as(u64, 0), metrics.task_waves);
    try std.testing.expectEqual(@as(u64, 0), metrics.tasks_submitted);
    try std.testing.expectEqual(@as(u64, 0), metrics.tasks_committed);
    try std.testing.expectEqual(@as(u64, 0), metrics.workspace_initializations);
    try std.testing.expectEqual(@as(u64, 0), metrics.workspace_reuses);
}

test "multiple procedure-use roots lower deterministically in parallel" {
    try expectProcedureRootParallelismDeterministicLir(
        \\main! = |_args| Ok({})
    );
}

test "multiple ordinary specialization epochs lower deterministically in parallel" {
    // `through` has independent scalar, list, and record specializations. Each
    // discovers its generic call to `identity`, exercising a second wave plus
    // worker-local type spans and field names while worker schedules vary.
    try expectSpecializationParallelismDeterministicLir(
        \\identity = |value| value
        \\
        \\through = |value| identity(value)
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    number = through(42)
        \\    text = through("epoch")
        \\    items = through([1, 2])
        \\    record = through({ label: "parallel" })
        \\    echo!(Str.inspect({ number, text, items, record }))
        \\    Ok({})
        \\}
    );
}

test "prepared finite capture-free direct calls lower in deterministic discovery waves" {
    try expectPreparedFiniteCaptureFreeDirectCallsParallelismDeterministicLir();
}

test "Solved-LIR worker metadata relocates deterministically" {
    try expectSolvedLirWorkerMetadataParallelismDeterministicLir();
}

test "Solved-LIR finite capturing bodies lower deterministically on workers" {
    try expectSolvedLirCapturingBodyParallelismDeterministicLir();
}

test "Solved-LIR runtime finite dispatch literals and patterns commit deterministically on workers" {
    // Runtime arguments keep the selected finite closure and string/list
    // patterns alive; constant-only initialization would not exercise admission.
    try harness.expectRuntimeWorkerParallelismDeterministicLir(.{ .app_body =
        \\choose = |flag, offset| if flag {
        \\    |value| value + offset
        \\} else {
        \\    |value| value * offset
        \\}
        \\
        \\dispatch = |text, count| {
        \\    adjustment = match text {
        \\        "worker" => 3.U64
        \\        "" => 1.U64
        \\        _ => 2.U64
        \\    }
        \\    bytes = if count == 0 Str.to_utf8("serial") else Str.to_utf8("parallel")
        \\    first = match bytes {
        \\        [byte, ..] => byte.to_u64()
        \\        [] => 0.U64
        \\    }
        \\    callback = choose(count == 0, adjustment)
        \\    callback(count) + first
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    text = match args {
        \\        [first, ..] => first
        \\        [] => ""
        \\    }
        \\    result = dispatch(text, args.len().to_u64())
        \\    if result == 0 Ok({}) else Err(Exit(1))
        \\}
    }, .{}, &.{ .indirect_call, .match, .literal }, null);
}

test "Solved-LIR runtime loops joins try and crash commit deterministically on workers" {
    // The early return and nested branch force loop exits and joins. Both the
    // fallible edge and crash depend on input, rather than a static initializer.
    try harness.expectRuntimeWorkerParallelismDeterministicLir(.{ .app_body =
        \\validate : U64 -> Try(U64, [Exit(I8)])
        \\validate = |limit| if limit == 100 Err(Exit(2)) else Ok(limit)
        \\
        \\walk : U64 -> Try(U64, [Exit(I8)])
        \\walk = |limit| {
        \\    checked = if limit == 99 {
        \\        crash "runtime worker crash"
        \\    } else {
        \\        validate(limit)?
        \\    }
        \\    var $index = 0.U64
        \\    var $total = 0.U64
        \\    while $index < checked {
        \\        if $index == 7 {
        \\            return Ok($total)
        \\        } else {
        \\            increment = if $index == 2 3.U64 else 1.U64
        \\            $total = $total + increment
        \\            $index = $index + 1
        \\        }
        \\    }
        \\    Ok($total)
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    match walk(args.len().to_u64()) {
        \\        Ok(result) => if result == 0 Ok({}) else Err(Exit(1))
        \\        Err(Exit(code)) => Err(Exit(code))
        \\    }
        \\}
    }, .{}, &.{ .loop, .match, .literal }, null);
}

test "Solved-LIR runtime nested nominal record and interpolation patterns commit deterministically on workers" {
    // Nested payloads and both ends of the string pattern depend on host input.
    // Wrappers-only inlining preserves the worker-owned match procedures.
    try harness.expectRuntimeWorkerParallelismDeterministicLir(.{ .app_body =
        \\Envelope := [Message({ label: Str, payload: [Count(U64), Missing] }), Empty]
        \\
        \\decode : Envelope -> U64
        \\decode = |envelope| match envelope {
        \\    Message({ label: "prefix${capture}suffix", payload: Count(count) }) => count + capture.to_utf8().len().to_u64()
        \\    Message({ label: "prefix${capture}", payload: Missing }) => capture.to_utf8().len().to_u64()
        \\    Message({ label: _, payload: Count(count) }) => count
        \\    Message({ label: _, payload: Missing }) => 1
        \\    Empty => 0
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    label = match args {
        \\        [first, ..] => first
        \\        [] => ""
        \\    }
        \\    count = args.len().to_u64()
        \\    payload = if count == 1 Missing else Count(count)
        \\    envelope : Envelope
        \\    envelope = if count == 0 Empty else Message({ label, payload })
        \\    result = decode(envelope)
        \\    if result == 0 Ok({}) else Err(Exit(1))
        \\}
    }, .{ .inline_mode = .wrappers }, &.{ .match, .literal }, null);
}

test "Solved-LIR runtime recursive local closure commits deterministically on workers" {
    // The recursive closure captures a runtime offset and escapes through a
    // finite choice, retaining both its recursive binding and an indirect call.
    try harness.expectRuntimeWorkerParallelismDeterministicLir(.{ .app_body =
        \\make_counter = |offset, flag| {
        \\    count : U64 -> U64
        \\    count = |remaining| if remaining == 0 offset else count(remaining - 1) + 1
        \\    if flag count else |remaining| remaining + offset
        \\}
        \\
        \\run = |size| {
        \\    counter = make_counter(size, size > 1)
        \\    counter(size)
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |args| {
        \\    result = run(args.len().to_u64())
        \\    if result == 0 Ok({}) else Err(Exit(1))
        \\}
    }, .{ .inline_mode = .wrappers }, &.{ .indirect_call, .capturing }, null);
}

test "iterator-producing callees complete in worker-owned specialization drafts" {
    try expectEagerIteratorSpecializationParallelismDeterministicLir(
        \\make_iter : List(U64) -> [Ready(Iter(U64))]
        \\make_iter = |items| Ready(items.iter().map(|item| item + 1))
        \\
        \\consume_a = |items| match make_iter(items) {
        \\    Ready(iter) => {
        \\        var $sum = 0.U64
        \\        for x in iter {
        \\            $sum = $sum + x
        \\        }
        \\        $sum
        \\    }
        \\}
        \\
        \\consume_b = |items| match make_iter(items) {
        \\    Ready(iter) => {
        \\        var $sum = 0.U64
        \\        for x in iter {
        \\            $sum = $sum + x
        \\        }
        \\        $sum
        \\    }
        \\}
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    total = consume_a([1, 2, 3]) + consume_b([1, 2, 3])
        \\    if total == 18 { Ok({}) } else { Err(Exit(1)) }
        \\}
    );
}

test "LIR pass workers deterministically rewrite recursive aggregate loops" {
    try harness.expectLirPassParallelismDeterministicLir(.{ .app_body =
        \\walk : U64, { sum: U64, count: U64 } -> U64
        \\walk = |n, state| if n == 0 {
        \\    state.sum + state.count
        \\} else {
        \\    walk(n - 1, { sum: state.sum + n, count: state.count + 1 })
        \\}
        \\
        \\walk_again : U64, U64 -> U64
        \\walk_again = |n, total| if n == 0 total else walk_again(n - 1, total + n)
        \\
        \\build : U64 -> List(U64)
        \\build = |n| {
        \\    var $values = []
        \\    var $i = 0
        \\    while $i < n {
        \\        $values = $values.append($i)
        \\        $i = $i + 1
        \\    }
        \\    $values
        \\}
        \\
        \\main! = |args| {
        \\    n = args.len()
        \\    echo!(Str.inspect((walk(n, { sum: 0, count: 0 }), walk_again(n, 0), build(n))))
        \\    Ok({})
        \\}
    }, .{}, &.{ .trmc, .scalarize, .loop_append });
}

test "self-tail proofs survive deterministic parallel body handoff" {
    try expectSpecializationParallelismDeterministicLir(
        \\walk : U64, U64 -> U64
        \\walk = |n, acc| if n == 0 { acc } else { walk(n - 1, acc + n) }
        \\
        \\main! : List(Str) => Try({}, [Exit(I8), ..])
        \\main! = |_args| {
        \\    echo!(walk(2000, 0).to_str())
        \\    Ok({})
        \\}
    );
}
