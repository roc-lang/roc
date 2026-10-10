//! Boxy plans one generated codec constructor worker per checked structural
//! codec derivation, whichever path reaches it: a direct structural
//! `parser_for`/`encoder_for` call or structural callable-dictionary evidence
//! (`structuralCodecWorkerSource`).

const std = @import("std");
const postcheck = @import("postcheck");
const harness = @import("lower_to_lir_harness.zig");

const Plan = postcheck.Boxy.Plan;

fn expectOneWorkerPerCodecDerivation(plan: *const Plan.ProgramPlan) harness.LowerToLirHarnessError!void {
    var constructors: usize = 0;
    for (plan.workers.items, 0..) |worker, index| {
        if (worker.source != .generated_codec) continue;
        const codec = worker.source.generated_codec;
        if (codec.kind != .parser_constructor and codec.kind != .encoder_constructor) continue;
        const derivation = codec.contract_derivation orelse continue;
        constructors += 1;
        for (plan.workers.items[index + 1 ..]) |other| {
            if (other.source != .generated_codec) continue;
            const other_codec = other.source.generated_codec;
            if (other_codec.kind != codec.kind) continue;
            const other_derivation = other_codec.contract_derivation orelse continue;
            if (other_derivation != derivation) continue;
            if (!std.meta.eql(other_codec.shape.module, codec.shape.module)) continue;
            std.debug.print("one structural codec derivation planned two constructor workers\n", .{});
            return error.TestUnexpectedResult;
        }
    }
    try std.testing.expect(constructors > 0);
}

test "structural codec: one derivation reached by dictionary evidence at several uses plans one worker" {
    const source =
        \\to_json = |a, b| Json.to_str({ a, b })
        \\forward = |a, b| to_json(a, b)
        \\main! = |args| {
        \\    echo!(forward(args, {}))
        \\    echo!(forward(True, args))
        \\    echo!(to_json(args, {}))
        \\    Ok({})
        \\}
    ;
    try harness.expectLowersToLirWithOptions(source, .{ .boxy_plan_inspect = expectOneWorkerPerCodecDerivation });
}
