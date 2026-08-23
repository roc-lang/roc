//! Type-checking tests for reusable `Range` values and range syntax. The
//! operators dispatch to `range_exclusive_to` / `range_inclusive_to` on the
//! lower bound, while reversal additionally requires the corresponding
//! `_from` method.

const TestEnv = @import("./TestEnv.zig");

test "exclusive range syntax defaults unannotated literals to Range(Dec)" {
    var test_env = try TestEnv.initExpr("Test", "0..<3");
    defer test_env.deinit();
    try test_env.assertLastDefType("Range(Dec)");
}

test "inclusive range syntax defaults unannotated literals to Range(Dec)" {
    var test_env = try TestEnv.initExpr("Test", "0..=3");
    defer test_env.deinit();
    try test_env.assertLastDefType("Range(Dec)");
}

test "Range is auto-imported and an annotation pins its numeric type" {
    const source =
        \\r : Range(U8)
        \\r = 0..<10
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Range(U8)");
}

test "inclusive Range annotation pins its numeric type" {
    const source =
        \\r : Range(F32)
        \\r = 1..=3
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Range(F32)");
}

test "exclusive syntax over generic operands requires range_exclusive_to" {
    const source =
        \\f = |start, finish| start..<finish
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefTypeContains("range_exclusive_to");
}

test "inclusive syntax over generic operands requires range_inclusive_to" {
    const source =
        \\f = |start, finish| start..=finish
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefTypeContains("range_inclusive_to");
}

test "range bounds must unify with each other" {
    const source =
        \\bad = 1..<"five"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertFirstTypeError("Type Mismatch");
}

test "inclusive range bounds must unify with each other" {
    const source =
        \\bad = 1..="five"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertFirstTypeError("Type Mismatch");
}

test "numeric types expose exclusive and inclusive to constructors" {
    const source =
        \\exclusive = U32.range_exclusive_to(0, 10)
        \\inclusive = I64.range_inclusive_to(-3, 3)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("exclusive", "Range(U32)");
    try test_env.assertDefType("inclusive", "Range(I64)");
}

test "non-float numeric types expose from constructors" {
    const source =
        \\exclusive = 10.U64.range_exclusive_from(5)
        \\inclusive = 10.Dec.range_inclusive_from(5)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("exclusive", "Range(U64)");
    try test_env.assertDefType("inclusive", "Range(Dec)");
}

test "Range custom supports a third-party numeric type" {
    const source =
        \\Distance := [Meters(U64)]
        \\r : Range(Distance)
        \\r = Range.custom({
        \\    lower: Meters(2),
        \\    upper: Meters(12),
        \\    step: Meters(2),
        \\    upper_bound: Inclusive,
        \\    direction: To,
        \\    len_if_known: Known(6),
        \\})
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Range(Distance)");
}

test "Range step_by accepts and returns the range's numeric type" {
    const source =
        \\r = (0.U16..=10).step_by(3)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Range(U16)");
}

test "integer ranges support forward and reverse iteration" {
    const source =
        \\forward = (0.I32..<10).iter()
        \\reverse = (0.I32..<10).iter_rev()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("forward", "Iter(I32)");
    try test_env.assertDefType("reverse", "Iter(I32)");
}

test "float ranges support forward iteration" {
    const source =
        \\forward = (0.F64..<10).iter()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("forward", "Iter(F64)");
}

test "float ranges reject reverse iteration because they have no from constructor" {
    const source =
        \\reverse = (0.F64..<10).iter_rev()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertFirstTypeError("Missing Method");
}
