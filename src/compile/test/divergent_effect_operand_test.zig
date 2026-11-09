//! A divergent expression that a consumer evaluates only for its effect (a
//! `match` scrutinee, a field access's receiver) lowers at its own type: its
//! branches still produce values of that type up to where it diverges, and
//! only the consumer's unreachable continuation has the consumer's type.

const harness = @import("lower_to_lir_harness.zig");

test "a match over an if whose condition is rejected lowers the if's nominal branches at the if's type" {
    try harness.expectLowersToLirWithOptions(
        \\W := U64
        \\
        \\f : U64 -> U64
        \\f = |_x| match if ("x") (W.(57)) else (W.(83)) {
        \\    W.(n) => n
        \\}
        \\
        \\main! = |_args| {
        \\    _ = f(1)
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Type Mismatch" });
}

test "a match over an if whose condition is rejected lowers the if's tag branches at the if's type" {
    try harness.expectLowersToLirWithOptions(
        \\f : U64 -> U64
        \\f = |_x| match if ("x") (Ak(57, "a")) else (Ak(83, "b")) {
        \\    Ak(n, _s) => n
        \\}
        \\
        \\main! = |_args| {
        \\    _ = f(1)
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Type Mismatch" });
}

test "a field access on an if whose condition is rejected lowers the if's record branches at the if's type" {
    try harness.expectLowersToLirWithOptions(
        \\f : U64 -> U64
        \\f = |_x| (if ("x") ({ a: 57 }) else ({ a: 83 })).a
        \\
        \\main! = |_args| {
        \\    _ = f(1)
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Type Mismatch" });
}

test "a divergent block whose final if has a branch with a value lowers that branch at the if's type" {
    try harness.expectLowersToLirWithOptions(
        \\f : Bool -> Bool
        \\f = |b| {
        \\    if (b) (if (nope.r0) (True) else (False)) else (crash "no")
        \\}
        \\
        \\x = f(True)
        \\
        \\main! = |_args| {
        \\    _ = x
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Name Not In Scope" });
}

test "a divergent pattern statement lowers its initializer's branches at the initializer's type" {
    try harness.expectLowersToLirWithOptions(
        \\f : Bool -> { a : U64 }
        \\f = |b| {
        \\    y = if (b) (if (nope) ({ a: 1 }) else ({ a: 2 })) else (crash "no")
        \\    y
        \\}
        \\
        \\x = f(True)
        \\
        \\main! = |_args| {
        \\    _ = x
        \\    Ok({})
        \\}
    , .{ .allow_user_errors = true, .expected_report_title = "Name Not In Scope" });
}
