//! Type-checking tests for range expressions (`..<` / `..=`), which desugar in
//! canonicalization to `range_exclusive` / `range_inclusive` method calls on the
//! start bound; also covers the `Iter` range constructors directly.

const TestEnv = @import("./TestEnv.zig");

test "Iter.range_exclusive of unannotated literals defaults to Iter(Dec)" {
    var test_env = try TestEnv.initExpr("Test", "Iter.range_exclusive(0, 3)");
    defer test_env.deinit();
    try test_env.assertLastDefType("Iter(Dec)");
}

test "Iter.range_inclusive of unannotated literals defaults to Iter(Dec)" {
    var test_env = try TestEnv.initExpr("Test", "Iter.range_inclusive(0, 3)");
    defer test_env.deinit();
    try test_env.assertLastDefType("Iter(Dec)");
}

test "Iter.range_exclusive annotation pins the type to Iter(U8)" {
    const source =
        \\r : Iter(U8)
        \\r = Iter.range_exclusive(0, 10)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Iter(U8)");
}

test "Iter.range_inclusive annotation pins the type to Iter(U8)" {
    const source =
        \\r : Iter(U8)
        \\r = Iter.range_inclusive(0, 255)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Iter(U8)");
}

test "Iter.range_exclusive over generic operands carries its where-constraints" {
    const source =
        \\f = |a, b| Iter.range_exclusive(a, b)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefTypeContains("range_exclusive");
}

test "Iter.range_inclusive over generic operands carries its where-constraints" {
    const source =
        \\f = |a, b| Iter.range_inclusive(a, b)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefTypeContains("range_inclusive");
}

test "exclusive range over generic operands requires a range_exclusive method" {
    const source =
        \\f = |start, finish| start..<finish
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefTypeContains("range_exclusive");
}

test "inclusive range over generic operands requires a range_inclusive method" {
    const source =
        \\f = |start, finish| start..=finish
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefTypeContains("range_inclusive");
}

test "range bounds must unify with each other" {
    const source =
        \\bad = 1..<"five"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    // `1` is an unannotated numeral, so the failed bound-unify also leaves its
    // `from_numeral` constraint undischarged. That yields two errors: TYPE
    // MISMATCH (the bound mismatch, primary/actionable) followed by a cascade
    // MISSING METHOD (numeral defaulting can't pick a type). This is the same
    // shared numeral-defaulting cascade `<` exhibits — it is not specific to
    // ranges. Assert on the first (primary) one.
    try test_env.assertFirstTypeError("Type Mismatch");
}

test "inclusive range bounds must unify with each other" {
    // Same path as the exclusive case, but through `..=` / `range_inclusive`,
    // guarding against the two operators' error paths diverging in a future
    // refactor.
    const source =
        \\bad = 1..="five"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertFirstTypeError("Type Mismatch");
}

test "range on annotated float operands types as Iter(F64)" {
    const source =
        \\start : F64
        \\start = 0
        \\r = start..<3
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Iter(F64)");
}

test "annotation on the range result pins the bound type to F32" {
    const source =
        \\r : Iter(F32)
        \\r = 1..=3
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Iter(F32)");
}

test "numeric types expose range_exclusive as an associated function" {
    const source =
        \\r = U32.range_exclusive(0, 10)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Iter(U32)");
}

test "numeric types expose range_inclusive as an associated function" {
    const source =
        \\r = I64.range_inclusive(-3, 3)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Iter(I64)");
}
