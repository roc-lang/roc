//! Regression test for https://github.com/roc-lang/roc/issues/13007: records
//! and tuples built from `where`-constrained type variables encode inside the
//! generic function that constrains them, and lower under both strategies.

const expectLowersToLirWithOptions = @import("lower_to_lir_harness.zig").expectLowersToLirWithOptions;

test "issue 13007: generic record and tuple encoders lower" {
    const source =
        \\in_record : a -> Str where [a.Json.Encodable([])]
        \\in_record = |x| Json.to_str({ value: x })
        \\
        \\in_tuple : a, b -> Str where [a.Json.Encodable([]), b.Json.Encodable([])]
        \\in_tuple = |x, y| Json.to_str((x, y))
        \\
        \\main! = |_args| {
        \\    echo!(in_record(1.U64))
        \\    echo!(in_tuple(1.U64, "a"))
        \\    Ok({})
        \\}
    ;
    try expectLowersToLirWithOptions(source, .{ .specialization_strategy = .lss });
    try expectLowersToLirWithOptions(source, .{ .specialization_strategy = .boxy });
}
