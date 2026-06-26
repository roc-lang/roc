//! Type-checking tests for range expressions (`..<` / `..=`), which desugar in
//! canonicalization to `Iter.exclusive_range` / `Iter.inclusive_range`
//! constructor calls returning Iter(num); also covers those constructors directly.

const TestEnv = @import("./TestEnv.zig");

test "exclusive_range of unannotated literals defaults to Iter(Dec)" {
    var test_env = try TestEnv.initExpr("Test", "Iter.exclusive_range(0, 3)");
    defer test_env.deinit();
    try test_env.assertLastDefType("Iter(Dec)");
}

test "inclusive_range of unannotated literals defaults to Iter(Dec)" {
    var test_env = try TestEnv.initExpr("Test", "Iter.inclusive_range(0, 3)");
    defer test_env.deinit();
    try test_env.assertLastDefType("Iter(Dec)");
}

test "exclusive_range annotation pins the type to Iter(U8)" {
    const source =
        \\r : Iter(U8)
        \\r = Iter.exclusive_range(0, 10)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Iter(U8)");
}

test "inclusive_range annotation pins the type to Iter(U8)" {
    const source =
        \\r : Iter(U8)
        \\r = Iter.inclusive_range(0, 255)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Iter(U8)");
}

test "exclusive_range over generic operands carries its where-constraints" {
    const source =
        \\f = |a, b| Iter.exclusive_range(a, b)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefTypeContains("is_lt");
}

test "inclusive_range over generic operands carries its where-constraints" {
    const source =
        \\f = |a, b| Iter.inclusive_range(a, b)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefTypeContains("is_lte");
}

test "exclusive range of unannotated literals defaults like a numeral" {
    var test_env = try TestEnv.initExpr("Test", "0..<3");
    defer test_env.deinit();
    try test_env.assertLastDefType("Iter(Dec)");
}

test "inclusive range of unannotated literals defaults like a numeral" {
    var test_env = try TestEnv.initExpr("Test", "0..=3");
    defer test_env.deinit();
    try test_env.assertLastDefType("Iter(Dec)");
}

test "annotation on the range result pins the bound type" {
    const source =
        \\r : Iter(U8)
        \\r = 0..<10
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("r", "Iter(U8)");
}

test "range over generic operands carries the constructor's where-constraint" {
    const source =
        \\f = |start, finish| start..<finish
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefTypeContains("is_lt");
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
    // Same path as the exclusive case, but through `..=` /
    // `Iter.inclusive_range`, guarding against the two operators' error paths
    // diverging in a future refactor.
    const source =
        \\bad = 1..="five"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertFirstTypeError("Type Mismatch");
}

test "range over a non-numeric nominal reports a missing-constraint error" {
    const source =
        \\bad = "a"..<"z"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    // Calibrated against the desugared `Iter.exclusive_range` call: Str does not
    // satisfy the constructor's `is_lt` where-constraint.
    try test_env.assertFirstTypeError("Missing Method");
}

test "for loop consumes a range" {
    const source =
        \\total : U64
        \\total = {
        \\    var sum_ = 0
        \\    for i in 1..=5 {
        \\        sum_ = sum_ + i
        \\    }
        \\    sum_
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertDefType("total", "U64");
}
