//! Regression for worker-dependent specialization identities in issue #11449.

const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

const two_record_json_parses =
    \\main! = |args| {
    \\    input = Str.join_with(args, ",")
    \\    decoded : Try({ s : Str, a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
    \\    decoded = Json.parse(input)
    \\    skipped : Try({ a : U64 }, [InvalidJson(Str), MissingRequiredField(Str)])
    \\    skipped = Json.parse(input)
    \\    if decoded == Ok({ s: "x", a: 7 }) and skipped == Ok({ a: 7 }) { Ok({}) } else { Err(Exit(1)) }
    \\}
;

test "issue 11449: worker count does not change emitted variant symbols" {
    try harness.expectArcParallelismDeterministicLir(
        .{ .app_body = two_record_json_parses },
        .{},
        false,
    );
}

test "issue 11449: equal JSON helper requests share Monotype bodies" {
    for ([_]usize{ 1, 2, 4 }) |workers| {
        try harness.expectLowersToLirWithOptions(two_record_json_parses, .{
            .specialization_workers = workers,
            .proc_debug_names = true,
            .prepared_inspect = expectSharedJsonHelpers,
        });
    }
}

fn expectSharedJsonHelpers(prepared: *const lir.CheckedPipeline.PreparedMonotype) harness.LowerToLirHarnessError!void {
    const program = prepared.program.view();
    for ([_][]const u8{
        "Builtin.Encoding.JsonEncoding.skip_record_field",
        "Builtin.Encoding.JsonEncoding.parse_record_start",
    }) |helper| {
        var count: usize = 0;
        for (program.defs) |def| {
            const debug_name = program.procDebugName(def.symbol) orelse continue;
            if (std.mem.eql(u8, program.names.exportNameText(debug_name), helper)) count += 1;
        }
        try std.testing.expectEqual(@as(usize, 1), count);
    }
}
