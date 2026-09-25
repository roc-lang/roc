//! Boxy planning of a coerced function used as a value (design.md "Row
//! Subsumption", "Result-Row Widening Adapter"): every lookup of a coerced
//! function that is not the callee of a direct call resolves to a
//! compiler-generated adapter worker keyed by that lookup, and a direct call
//! resolves to the function's own worker.

const std = @import("std");
const postcheck = @import("postcheck");
const harness = @import("lower_to_lir_harness.zig");

const Plan = postcheck.Boxy.Plan;

fn adapterWorkerCount(plan: *const Plan.ProgramPlan) usize {
    var count: usize = 0;
    for (plan.workers.items) |worker| {
        if (worker.source == .coerced_use_adapter) count += 1;
    }
    return count;
}

/// Every adapter makes exactly one direct call, keyed by its own lookup and
/// passing its own arguments through.
fn expectAdapterCallsAreWellFormed(plan: *const Plan.ProgramPlan) harness.LowerToLirHarnessError!void {
    for (plan.workers.items) |worker| {
        const adapter = switch (worker.source) {
            .coerced_use_adapter => |adapter| adapter,
            .procedure_template,
            .procedure_binding,
            .procedure_use,
            .nested_expr,
            .generated_codec,
            .generated_field_iterator,
            .generated_interpolation_step,
            => continue,
        };
        var calls: usize = 0;
        for (plan.direct_calls.items) |call| {
            if (call.caller != worker.id) continue;
            calls += 1;
            try std.testing.expect(std.meta.eql(call.call, adapter.use));
            for (plan.callOperandSlice(call.operands), 0..) |operand, index| {
                try std.testing.expectEqual(Plan.CallOperand{ .adapter_param = @intCast(index) }, operand);
            }
        }
        try std.testing.expectEqual(@as(usize, 1), calls);
    }
}

fn expectOneAdapter(plan: *const Plan.ProgramPlan) harness.LowerToLirHarnessError!void {
    try std.testing.expectEqual(@as(usize, 1), adapterWorkerCount(plan));
    try expectAdapterCallsAreWellFormed(plan);
}

fn expectNoAdapter(plan: *const Plan.ProgramPlan) harness.LowerToLirHarnessError!void {
    try std.testing.expectEqual(@as(usize, 0), adapterWorkerCount(plan));
}

test "row subsumption boxy: a local callable alias of a coerced function is an adapter worker" {
    // `run` generalizes the re-opened row, so its two uses instantiate it at
    // different widths; the adapter's scheme is `run`'s, which is what lets
    // each use's substitution apply to it.
    try harness.expectLowersToLirWithOptions(
        \\fwd : [B, D] -> [B, D]
        \\fwd = |t| t
        \\show_wide : [A, B, C, D] -> Str
        \\show_wide = |v| match v { A => "A", B => "B", C => "C", D => "D" }
        \\show_narrow : [B, D] -> Str
        \\show_narrow = |v| match v { B => "B", D => "D" }
        \\main! = |_args| {
        \\    run = fwd
        \\    echo!(show_wide(run(B)))
        \\    echo!(show_narrow(run(D)))
        \\    Ok({})
        \\}
    , .{ .boxy_plan_inspect = expectOneAdapter });
}

test "row subsumption boxy: a top-level callable alias of a coerced function is an adapter worker" {
    try harness.expectLowersToLirWithOptions(
        \\fwd : [B, D] -> [B, D]
        \\fwd = |t| t
        \\run = fwd
        \\show_wide : [A, B, C, D] -> Str
        \\show_wide = |v| match v { A => "A", B => "B", C => "C", D => "D" }
        \\main! = |_args| {
        \\    echo!(show_wide(run(B)))
        \\    Ok({})
        \\}
    , .{ .boxy_plan_inspect = expectOneAdapter });
}

test "row subsumption boxy: a coerced function passed as a value is an adapter worker" {
    try harness.expectLowersToLirWithOptions(
        \\fwd : [B, D] -> [B, D]
        \\fwd = |t| t
        \\apply : ([B, D] -> [A, B, C, D]), [B, D] -> [A, B, C, D]
        \\apply = |f, x| f(x)
        \\show_wide : [A, B, C, D] -> Str
        \\show_wide = |v| match v { A => "A", B => "B", C => "C", D => "D" }
        \\main! = |_args| {
        \\    echo!(show_wide(apply(fwd, D)))
        \\    Ok({})
        \\}
    , .{ .boxy_plan_inspect = expectOneAdapter });
}

test "row subsumption boxy: a direct call of a coerced function makes no adapter" {
    // The direct call's own return boundary re-tags the result.
    try harness.expectLowersToLirWithOptions(
        \\fwd : [B, D] -> [B, D]
        \\fwd = |t| t
        \\show_wide : [A, B, C, D] -> Str
        \\show_wide = |v| match v { A => "A", B => "B", C => "C", D => "D" }
        \\main! = |_args| {
        \\    echo!(show_wide(fwd(B)))
        \\    Ok({})
        \\}
    , .{ .boxy_plan_inspect = expectNoAdapter });
}

test "row subsumption boxy: an uncoerced function passed as a value makes no adapter" {
    // `made` constructs its result, so nothing is coerced and no lookup of it
    // re-opens a row.
    try harness.expectLowersToLirWithOptions(
        \\made : [B, D] -> [B, D]
        \\made = |_| B
        \\apply : ([B, D] -> [A, B, C, D]), [B, D] -> [A, B, C, D]
        \\apply = |f, x| f(x)
        \\show_wide : [A, B, C, D] -> Str
        \\show_wide = |v| match v { A => "A", B => "B", C => "C", D => "D" }
        \\main! = |_args| {
        \\    echo!(show_wide(apply(made, D)))
        \\    Ok({})
        \\}
    , .{ .boxy_plan_inspect = expectNoAdapter });
}

test "row subsumption boxy: a use of a coerced constant makes no adapter" {
    // A coerced VALUE's widened use is re-tagged where the constant is
    // restored, not by an adapter worker.
    try harness.expectLowersToLirWithOptions(
        \\Closed := { d : [B, D] }
        \\v : [B, D]
        \\v = Closed.{ d: D }.d
        \\show_wide : [A, B, C, D] -> Str
        \\show_wide = |x| match x { A => "A", B => "B", C => "C", D => "D" }
        \\main! = |_args| {
        \\    echo!(show_wide(v))
        \\    Ok({})
        \\}
    , .{ .boxy_plan_inspect = expectNoAdapter });
}
