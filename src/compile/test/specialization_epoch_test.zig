//! End-to-end coverage for deterministic ordinary-specialization handoff.

const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");
const expectLowersToLirWithOptions = harness.expectLowersToLirWithOptions;
const expectPreparedFiniteCaptureFreeDirectCallsParallelismDeterministicLir =
    harness.expectPreparedFiniteCaptureFreeDirectCallsParallelismDeterministicLir;
const expectSpecializationParallelismDeterministicLir = harness.expectSpecializationParallelismDeterministicLir;
const expectProcedureRootParallelismDeterministicLir = harness.expectProcedureRootParallelismDeterministicLir;

test "solved-LIR parallel metrics reset and report retry-free batch accounting" {
    const app_body =
        \\main! = |_args| Ok({})
    ;
    var metrics: lir.CheckedPipeline.SolvedLirParallelMetrics = .{
        .task_waves = 11,
        .tasks_submitted = 22,
        .tasks_committed = 33,
        .tasks_retried_serial = 44,
    };

    try expectLowersToLirWithOptions(app_body, .{
        .specialization_workers = 1,
        .solved_lir_parallel_metrics_out = &metrics,
    });
    try std.testing.expectEqual(@as(u64, 0), metrics.task_waves);
    try std.testing.expectEqual(@as(u64, 0), metrics.tasks_submitted);
    try std.testing.expectEqual(@as(u64, 0), metrics.tasks_committed);
    try std.testing.expectEqual(@as(u64, 0), metrics.tasks_retried_serial);
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
    try std.testing.expectEqual(@as(u64, 0), metrics.tasks_retried_serial);
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
    try std.testing.expectEqual(@as(u64, 0), metrics.tasks_retried_serial);
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
