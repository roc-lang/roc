//! End-to-end tests for exhaustiveness and redundancy checking.
//!
//! These tests verify that:
//! - Non-exhaustive matches produce NON-EXHAUSTIVE MATCH errors
//! - Redundant patterns produce REDUNDANT PATTERN warnings
//! - Exhaustive matches pass without errors

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

// Basic Tag Union Exhaustiveness

test "exhaustive - single wildcard is exhaustive" {
    const source =
        \\result = match Ok(42i64) {
        \\    _ => 0i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "exhaustive - all tag variants covered for Try" {
    const source =
        \\x : Try(I64, Str)
        \\x = Ok(42i64)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\    Err(_) => 0i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "non-exhaustive - missing Err case" {
    const source =
        \\x : Try(I64, Str)
        \\x = Ok(42i64)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
}

test "exhaustive - wildcard covers remaining variants" {
    const source =
        \\x : Try(I64, Str)
        \\x = Ok(42i64)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\    _ => 0i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

// List Pattern Exhaustiveness

test "exhaustive - list with rest pattern covers all" {
    const source =
        \\x : List(I64)
        \\x = [1i64, 2i64, 3i64]
        \\
        \\result = match x {
        \\    [] => 0i64
        \\    [_, ..] => 1i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "exhaustive - fixed length list patterns" {
    const source =
        \\x : List(I64)
        \\x = [1i64, 2i64]
        \\
        \\result = match x {
        \\    [] => 0i64
        \\    [_] => 1i64
        \\    [_, _] => 2i64
        \\    [_, _, ..] => 3i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

// Literal Pattern Exhaustiveness

test "non-exhaustive - integer literals need wildcard" {
    const source =
        \\x : I64
        \\x = 42i64
        \\
        \\result = match x {
        \\    1 => "one"
        \\    2 => "two"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
}

test "exhaustive - integer literals with wildcard" {
    const source =
        \\x : I64
        \\x = 42i64
        \\
        \\result = match x {
        \\    1 => "one"
        \\    2 => "two"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

// Redundancy Checking

test "redundant - pattern after wildcard" {
    const source =
        \\x : Try(I64, Str)
        \\x = Ok(42i64)
        \\
        \\result = match x {
        \\    _ => 0i64
        \\    Ok(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

test "redundant - second wildcard" {
    const source =
        \\x : I64
        \\x = 42i64
        \\
        \\result = match x {
        \\    _ => 1i64
        \\    _ => 2i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

test "redundant - duplicate integer literal" {
    const source =
        \\x : I64
        \\x = 42i64
        \\
        \\result = match x {
        \\    1 => "one"
        \\    1 => "one again"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

// Record Pattern Exhaustiveness

test "exhaustive - record destructuring" {
    const source =
        \\x : { name: Str, age: I64 }
        \\x = { name: "Alice", age: 30i64 }
        \\
        \\result = match x {
        \\    { name: _, age } => age
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "exhaustive - record with all fields as wildcards" {
    // Matching all fields with wildcards should be exhaustive
    const source =
        \\x : { name: Str, age: I64 }
        \\x = { name: "Alice", age: 30i64 }
        \\
        \\result = match x {
        \\    { name: _, age: _ } => 1i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "exhaustive - record with literal field guard" {
    // Matching record with specific literal value requires wildcard for other cases
    const source =
        \\x : { name: Str, age: I64 }
        \\x = { name: "Alice", age: 30i64 }
        \\
        \\result = match x {
        \\    { name: _, age: 30 } => "thirty"
        \\    { name: _, age: _ } => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

test "non-exhaustive - record with literal field missing wildcard" {
    // Matching record field with only literal value is not exhaustive
    const source =
        \\x : { name: Str, age: I64 }
        \\x = { name: "Alice", age: 30i64 }
        \\
        \\result = match x {
        \\    { name: _, age: 30 } => "thirty"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
}

test "exhaustive - record with tag field all cases" {
    // Record containing a tag field - all tag cases must be covered
    const source =
        \\x : { value: Try(I64, Str) }
        \\x = { value: Ok(42i64) }
        \\
        \\result = match x {
        \\    { value: Ok(n) } => n
        \\    { value: Err(_) } => 0i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "non-exhaustive - record with tag field missing case" {
    // Record containing a tag field - missing Err case
    const source =
        \\x : { value: Try(I64, Str) }
        \\x = { value: Ok(42i64) }
        \\
        \\result = match x {
        \\    { value: Ok(n) } => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
}

// Nested Pattern Exhaustiveness with Try

test "exhaustive - nested Try patterns fully covered" {
    const source =
        \\x : Try(Try(I64, Str), Str)
        \\x = Ok(Ok(42i64))
        \\
        \\result = match x {
        \\    Ok(Ok(n)) => n
        \\    Ok(Err(_)) => -1i64
        \\    Err(_) => -2i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "non-exhaustive - nested Try missing inner Err" {
    const source =
        \\x : Try(Try(I64, Str), Str)
        \\x = Ok(Ok(42i64))
        \\
        \\result = match x {
        \\    Ok(Ok(n)) => n
        \\    Err(_) => -2i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
}

// Tuple exhaustiveness

test "exhaustive - tuple with Try" {
    const source =
        \\x : (Try(I64, Str), I64)
        \\x = (Ok(42i64), 1i64)
        \\
        \\result = match x {
        \\    (Ok(n), _) => n
        \\    (Err(_), _) => 0i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

// Uninhabited Types - Empty Tag Union Handling
// These tests verify that empty tag unions (like []) are recognized as uninhabited,
// making patterns on them unnecessary.

test "exhaustive - empty error type means only Ok needed" {
    // When the error type is an empty tag union [], the Err case is uninhabited
    // So a match with only Ok should be exhaustive
    const source =
        \\x : Try(I64, [])
        \\x = Ok(42i64)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "exhaustive - nested Try with empty inner error" {
    const source =
        \\x : Try(Try(I64, []), Str)
        \\x = Ok(Ok(42i64))
        \\
        \\result = match x {
        \\    Ok(Ok(n)) => n
        \\    Err(_) => 0i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Ok(Err(_)) is uninhabited (inner error is []), so this is exhaustive
    try test_env.assertLastDefType("I64");
}

test "exhaustive - doubly nested empty errors" {
    const source =
        \\x : Try(Try(I64, []), [])
        \\x = Ok(Ok(42i64))
        \\
        \\result = match x {
        \\    Ok(Ok(n)) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Both Err cases are uninhabited
    try test_env.assertLastDefType("I64");
}

test "non-exhaustive - non-empty error type requires Err case" {
    // This is a regression test - non-empty error types should still require Err
    const source =
        \\x : Try(I64, Str)
        \\x = Ok(42i64)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Str is NOT empty, so Err is required
    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
}

test "redundant - second wildcard after first on empty error type" {
    // When matching on a type where one constructor is uninhabited,
    // patterns after the first one should be redundant
    const source =
        \\x : Try(I64, [])
        \\x = Ok(42i64)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\    Ok(_) => 0i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // The second Ok pattern is redundant (same constructor already matched)
    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

test "redundant - wildcard after complete coverage on type with empty variant" {
    // When Ok covers everything (because Err is empty), a following wildcard is redundant
    const source =
        \\x : Try(I64, [])
        \\x = Ok(42i64)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\    _ => 0i64
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // The wildcard is redundant because Ok already covers all inhabited cases
    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

test "redundant - Err pattern first on empty error type is unreachable" {
    // When the error type is empty [], an Err(_) pattern can never match
    // because no Err values can exist. This should be flagged as redundant.
    const source =
        \\x : Try(I64, [])
        \\x = Ok(42i64)
        \\
        \\result = match x {
        \\    Err(_) => 0i64
        \\    Ok(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

// Additional Inhabitedness Edge Cases
// These tests verify correct handling of various uninhabited type scenarios.

test "exhaustive - triply nested empty errors" {
    // 3+ levels of nesting with empty types
    const source =
        \\x : Try(Try(Try(I64, []), []), [])
        \\x = Ok(Ok(Ok(42i64)))
        \\
        \\result = match x {
        \\    Ok(Ok(Ok(n))) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // All three Err cases are uninhabited at every level
    try test_env.assertLastDefType("I64");
}

test "exhaustive - record with uninhabited field means record is uninhabited" {
    // A record where one field has an uninhabited type is itself uninhabited
    const source =
        \\x : [HasEmpty({ value: I64, empty: [] }), Normal(I64)]
        \\x = Normal(42i64)
        \\
        \\result = match x {
        \\    Normal(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // HasEmpty is uninhabited because its record contains an uninhabited field
    try test_env.assertLastDefType("I64");
}

test "exhaustive - tuple with uninhabited element means tuple is uninhabited" {
    // A tuple where one element has an uninhabited type is itself uninhabited
    const source =
        \\x : [HasEmpty((I64, [])), Normal(I64)]
        \\x = Normal(42i64)
        \\
        \\result = match x {
        \\    Normal(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // HasEmpty is uninhabited because its tuple contains an uninhabited element
    try test_env.assertLastDefType("I64");
}

test "exhaustive - tag with mixed inhabited and uninhabited args" {
    // A tag with multiple args where one is uninhabited makes the whole tag uninhabited
    const source =
        \\x : [Mixed(I64, [], Str), Normal(I64)]
        \\x = Normal(42i64)
        \\
        \\result = match x {
        \\    Normal(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Mixed is uninhabited because one of its args ([]) is uninhabited
    try test_env.assertLastDefType("I64");
}

test "exhaustive - deeply nested uninhabited in record field" {
    // Uninhabited type nested deeply inside record structure
    const source =
        \\x : [Wrapper({ inner: { deep: Try(I64, []) } })]
        \\x = Wrapper({ inner: { deep: Ok(42i64) } })
        \\
        \\result = match x {
        \\    Wrapper({ inner: { deep: Ok(n) } }) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // The deeply nested Err case is uninhabited
    try test_env.assertLastDefType("I64");
}

test "redundant - pattern on tag with direct empty arg" {
    // Matching on a tag whose argument is directly an empty tag union
    const source =
        \\x : [HasEmpty([]), Normal(I64)]
        \\x = Normal(42i64)
        \\
        \\result = match x {
        \\    HasEmpty(_) => 0i64
        \\    Normal(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // HasEmpty pattern is unreachable because [] is uninhabited
    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

test "non-exhaustive - not all inhabited tags covered with empty arg" {
    // Ensure we still require patterns for inhabited tags even when some are uninhabited
    const source =
        \\x : [A(I64), B([]), C(Str)]
        \\x = A(42i64)
        \\
        \\result = match x {
        \\    A(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // B is uninhabited but C is still inhabited and needs a pattern
    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
}
