//! Regression tests for https://github.com/roc-lang/roc/issues/13007: a record
//! or tuple whose components are `where`-constrained type variables has a
//! derived `encoder_for` inside the generic function that constrains them,
//! because every component satisfies the requirement through that function's
//! own evidence.

const TestEnv = @import("TestEnv.zig");

test "issue 13007: a record of a constrained type variable derives encoder_for" {
    var env = try TestEnv.init("Test",
        \\in_record : a -> Str where [a.Json.Encodable([])]
        \\in_record = |x| Json.to_str({ value: x })
        \\
        \\used = in_record(1.U64)
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("used", "Str");
}

test "issue 13007: a tuple of constrained type variables derives encoder_for" {
    var env = try TestEnv.init("Test",
        \\in_tuple : a, b -> Str where [a.Json.Encodable([]), b.Json.Encodable([])]
        \\in_tuple = |x, y| Json.to_str((x, y))
        \\
        \\used = in_tuple(1.U64, "a")
    );
    defer env.deinit();
    try env.assertNoErrors();
    try env.assertDefType("used", "Str");
}
