//! Type-checking tests for range expressions (`..<` / `..=`).
//! Ranges desugar to `until`/`to` static dispatch returning Iter(num).

const TestEnv = @import("./TestEnv.zig");

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

test "range over generic operands carries an until where-constraint" {
    const source =
        \\f = |start, finish| start..<finish
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefTypeContains("until");
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
    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "inclusive range bounds must unify with each other" {
    // Same path as the exclusive case, but through `..=` / `to`, guarding
    // against the two operators' error paths diverging in a future refactor.
    const source =
        \\bad = 1..="five"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "range over a non-numeric nominal reports missing until method" {
    const source =
        \\bad = "a"..<"z"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("MISSING METHOD");
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
