//! End-to-end tests for exhaustiveness and redundancy checking.
//!
//! These tests verify that:
//! - Non-exhaustive matches produce NON EXHAUSTIVE MATCH errors
//! - Redundant patterns produce REDUNDANT PATTERN warnings
//! - Exhaustive matches pass without errors

const std = @import("std");
const TestEnv = @import("TestEnv.zig");

// Basic Tag Union Exhaustiveness

test "exhaustive - single wildcard is exhaustive" {
    const source =
        \\result : I64
        \\result = match Ok(42) {
        \\    _ => 0
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "exhaustive - all tag variants covered for Try" {
    const source =
        \\x : Try(I64, Str)
        \\x = Ok(42)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\    Err(_) => 0
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "exhaustive - wildcard covers remaining variants" {
    const source =
        \\x : Try(I64, Str)
        \\x = Ok(42)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\    _ => 0
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
        \\x = [1, 2, 3]
        \\
        \\result : I64
        \\result = match x {
        \\    [] => 0
        \\    [_, ..] => 1
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "exhaustive - fixed length list patterns" {
    const source =
        \\x : List(I64)
        \\x = [1, 2]
        \\
        \\result : I64
        \\result = match x {
        \\    [] => 0
        \\    [_] => 1
        \\    [_, _] => 2
        \\    [_, _, ..] => 3
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
        \\x = 42
        \\
        \\result = match x {
        \\    1 => "one"
        \\    2 => "two"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

test "non-exhaustive - unannotated integer literal match needs wildcard" {
    const source =
        \\f = |n| match n {
        \\    30 => "thirty"
        \\}
        \\
        \\result = f(31)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

test "exhaustive - integer literals with wildcard" {
    const source =
        \\x : I64
        \\x = 42
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

test "non-exhaustive - guarded-only match needs unguarded fallback" {
    const source =
        \\result = match 1.I64 {
        \\    x if x == 2.I64 => x
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

// Redundancy Checking

test "redundant - pattern after wildcard" {
    const source =
        \\x : Try(I64, Str)
        \\x = Ok(42)
        \\
        \\result = match x {
        \\    _ => 0
        \\    Ok(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("Redundant Pattern");
}

test "redundant - second wildcard" {
    const source =
        \\x : I64
        \\x = 42
        \\
        \\result = match x {
        \\    _ => 1
        \\    _ => 2
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("Redundant Pattern");
}

test "redundant - duplicate integer literal" {
    const source =
        \\x : I64
        \\x = 42
        \\
        \\result = match x {
        \\    1 => "one"
        \\    1 => "one again"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("Redundant Pattern");
}

// Record Pattern Exhaustiveness

test "exhaustive - record destructuring" {
    const source =
        \\x : { name: Str, age: I64 }
        \\x = { name: "Alice", age: 30 }
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
        \\x = { name: "Alice", age: 30 }
        \\
        \\result : I64
        \\result = match x {
        \\    { name: _, age: _ } => 1
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
        \\x = { name: "Alice", age: 30 }
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
        \\x = { name: "Alice", age: 30 }
        \\
        \\result = match x {
        \\    { name: _, age: 30 } => "thirty"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

test "exhaustive - record with tag field all cases" {
    // Record containing a tag field - all tag cases must be covered
    const source =
        \\x : { value: Try(I64, Str) }
        \\x = { value: Ok(42) }
        \\
        \\result : I64
        \\result = match x {
        \\    { value: Ok(n) } => n
        \\    { value: Err(_) } => 0
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
        \\x = { value: Ok(42) }
        \\
        \\result = match x {
        \\    { value: Ok(n) } => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

// Nested Pattern Exhaustiveness with Try

test "exhaustive - nested Try patterns fully covered" {
    const source =
        \\x : Try(Try(I64, Str), Str)
        \\x = Ok(Ok(42))
        \\
        \\result : I64
        \\result = match x {
        \\    Ok(Ok(n)) => n
        \\    Ok(Err(_)) => -1
        \\    Err(_) => -2
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "non-exhaustive - nested Try missing inner Err" {
    const source =
        \\x : Try(Try(I64, Str), Str)
        \\x = Ok(Ok(42))
        \\
        \\result = match x {
        \\    Ok(Ok(n)) => n
        \\    Err(_) => -2
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Non Exhaustive Match", "Ok(Err(_))");
}

test "non-exhaustive - missing tag with multiple payloads" {
    const source =
        \\classify : [Empty, Pair(Bool, Bool)] -> Str
        \\classify = |value| match value {
        \\    Empty => "empty"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeProblemMsgContains("Non Exhaustive Match", "Pair(_, _)");
}

// Tuple exhaustiveness

test "exhaustive - tuple with Try" {
    const source =
        \\x : (Try(I64, Str), I64)
        \\x = (Ok(42), 1)
        \\
        \\result : I64
        \\result = match x {
        \\    (Ok(n), _) => n
        \\    (Err(_), _) => 0
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
        \\x = Ok(42)
        \\
        \\result : I64
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
        \\x = Ok(Ok(42))
        \\
        \\result : I64
        \\result = match x {
        \\    Ok(Ok(n)) => n
        \\    Err(_) => 0
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
        \\x = Ok(Ok(42))
        \\
        \\result : I64
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
        \\x = Ok(42)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Str is NOT empty, so Err is required
    try test_env.assertOneTypeError("Non Exhaustive Match");
}

test "redundant - second wildcard after first on empty error type" {
    // When matching on a type where one constructor is uninhabited,
    // patterns after the first one should be redundant
    const source =
        \\x : Try(I64, [])
        \\x = Ok(42)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\    Ok(_) => 0
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // The second Ok pattern is redundant (same constructor already matched)
    try test_env.assertFirstTypeError("Redundant Pattern");
}

test "redundant - wildcard after complete coverage on type with empty variant" {
    // When Ok covers everything (because Err is empty), a following wildcard is redundant
    const source =
        \\x : Try(I64, [])
        \\x = Ok(42)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\    _ => 0
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // The wildcard is redundant because Ok already covers all inhabited cases
    try test_env.assertFirstTypeError("Redundant Pattern");
}

test "unmatchable - Err pattern first on empty error type is unreachable" {
    // When the error type is empty [], an Err(_) pattern can never match
    // because no Err values can exist. This should be flagged as unmatchable.
    const source =
        \\x : Try(I64, [])
        \\x = Ok(42)
        \\
        \\result = match x {
        \\    Err(_) => 0
        \\    Ok(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("Unmatchable Pattern");
}

test "exhaustive - inferred wildcard error type means only Ok needed" {
    const source =
        \\x : Try(I64, _)
        \\x = Ok(42)
        \\
        \\result : I64
        \\result = match x {
        \\    Ok(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "exhaustive - direct Try.Ok match only needs Ok" {
    const source =
        \\result : Str
        \\result = match Try.Ok("blah") {
        \\    Ok(foo) => foo
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

test "exhaustive - an Err pattern on an inferred wildcard error type leaves the payload undecided" {
    const source =
        \\x : Try(I64, _)
        \\x = Ok(42)
        \\
        \\result = match x {
        \\    Err(_) => 0
        \\    Ok(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "exhaustive - structural tag with inferred wildcard payload is not required" {
    const source =
        \\x : [Something, Other(_)]
        \\x = Something
        \\
        \\result : I64
        \\result = match x {
        \\    Something => 1
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "non-exhaustive - structural tag with ordinary payload is required" {
    const source =
        \\x : [Something, Other(U8)]
        \\x = Something
        \\
        \\result = match x {
        \\    Something => 1
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

test "destructure - Ok on inferred wildcard error type is exhaustive" {
    const source =
        \\result : Str
        \\result = {
        \\    x : Try(Str, _)
        \\    x = Ok("blah")
        \\
        \\    Ok(foo) = x
        \\
        \\    foo
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

test "destructure - direct Try.Ok is exhaustive" {
    const source =
        \\result : Str
        \\result = {
        \\    Ok(foo) = Try.Ok("blah")
        \\
        \\    foo
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

test "non-exhaustive destructure - Ok on concrete error type can miss Err" {
    const source =
        \\result = {
        \\    x : Try(Str, Str)
        \\    x = if True { Ok("blah") } else { Err("bad") }
        \\
        \\    Ok(foo) = x
        \\
        \\    foo
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("Non Exhaustive Destructure");
}

// Additional Inhabitedness Edge Cases
// These tests verify correct handling of various uninhabited type scenarios.

test "exhaustive - record with uninhabited field means record is uninhabited" {
    // A record where one field has an uninhabited type is itself uninhabited
    const source =
        \\x : [HasEmpty({ value: I64, empty: [] }), Normal(I64)]
        \\x = Normal(42)
        \\
        \\result : I64
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
        \\x = Normal(42)
        \\
        \\result : I64
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
        \\x = Normal(42)
        \\
        \\result : I64
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
        \\x = Wrapper({ inner: { deep: Ok(42) } })
        \\
        \\result : I64
        \\result = match x {
        \\    Wrapper({ inner: { deep: Ok(n) } }) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // The deeply nested Err case is uninhabited
    try test_env.assertLastDefType("I64");
}

test "unmatchable - pattern on tag with direct empty arg" {
    // Matching on a tag whose argument is directly an empty tag union
    const source =
        \\x : [HasEmpty([]), Normal(I64)]
        \\x = Normal(42)
        \\
        \\result = match x {
        \\    HasEmpty(_) => 0
        \\    Normal(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // HasEmpty pattern is unreachable because [] is uninhabited
    try test_env.assertFirstTypeError("Unmatchable Pattern");
}

test "non-exhaustive - not all inhabited tags covered with empty arg" {
    // Ensure we still require patterns for inhabited tags even when some are uninhabited
    const source =
        \\x : [A(I64), B([]), C(Str)]
        \\x = A(42)
        \\
        \\result = match x {
        \\    A(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // B is uninhabited but C is still inhabited and needs a pattern
    try test_env.assertOneTypeError("Non Exhaustive Match");
}

// List element inhabitedness tests
// A List([]) can only be the empty list, since no elements of type [] can exist.

test "exhaustive - List of empty type is still inhabited" {
    // List([]) is inhabited because the empty list [] exists.
    // Since no non-empty list of [] can be constructed, only matching [] is exhaustive.
    const source =
        \\x : List([])
        \\x = []
        \\
        \\result : I64
        \\result = match x {
        \\    [] => 0
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "unmatchable - second pattern on List of empty type" {
    // When both [] and [_, ..] are present, the [_, ..] is unmatchable
    // because no non-empty list of [] can exist
    const source =
        \\x : List([])
        \\x = []
        \\
        \\result = match x {
        \\    [] => 0
        \\    [_, ..] => 1
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("Unmatchable Pattern");
}

test "unmatchable - non-empty list pattern on List of empty type" {
    // A non-empty list pattern on List([]) is unreachable
    // because you can't construct a list with elements of type []
    const source =
        \\x : List([])
        \\x = []
        \\
        \\result = match x {
        \\    [_, ..] => 1
        \\    [] => 0
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("Unmatchable Pattern");
}

// Extension Chain Tests
//
// These tests verify that the exhaustiveness checker correctly handles tag unions
// whose tags may be split across extension chains during type unification.
// Extension chains are an internal representation detail where unifying
// [A, B] with [B, C] might produce [B, ..ext] where ext = [A, ..] or similar.
//
// The implementation follows extension chains in:
// - buildUnionFromTagUnion: collects all tags for exhaustiveness checking
// - getCtorArgTypes: finds argument types for a tag at any position
// - isTagUnionInhabited: checks if any tag is inhabited across the chain
//
// These tests exercise the code paths that would be affected by extension chains,
// even though the specific internal representation may vary.

test "exhaustive - union with many tags requires all inhabited to be matched" {
    // This tests that all tags are found even if they might be split across extensions
    const source =
        \\x : [A(I64), B(Str), C(U64), D(F64)]
        \\x = A(1)
        \\
        \\result : I64
        \\result = match x {
        \\    A(_) => 1
        \\    B(_) => 2
        \\    C(_) => 3
        \\    D(_) => 4
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "non-exhaustive - missing tag from union with many tags" {
    // Verifies we detect missing tags even if they're at different positions
    const source =
        \\x : [A(I64), B(Str), C(U64), D(F64)]
        \\x = A(1)
        \\
        \\result = match x {
        \\    A(n) => 1
        \\    B(_) => 2
        \\    D(_) => 4
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

test "exhaustive - union with mix of inhabited and uninhabited tags" {
    // Tests that uninhabited tags are properly skipped across the whole union
    const source =
        \\x : [A(I64), B([]), C(Str), D([]), E(U64)]
        \\x = A(1)
        \\
        \\result : I64
        \\result = match x {
        \\    A(_) => 1
        \\    C(_) => 3
        \\    E(_) => 5
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // B and D are uninhabited (argument is []), so only A, C, E need patterns
    try test_env.assertLastDefType("I64");
}

test "non-exhaustive - missing inhabited tag when uninhabited are present" {
    // Verifies we still require inhabited tags even when uninhabited exist
    const source =
        \\x : [A(I64), B([]), C(Str), D([]), E(U64)]
        \\x = A(1)
        \\
        \\result = match x {
        \\    A(n) => 1
        \\    C(_) => 3
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // E is inhabited but not matched
    try test_env.assertOneTypeError("Non Exhaustive Match");
}

test "exhaustive - tags at various positions all uninhabited" {
    // Tests that a union is exhausted when all remaining tags are uninhabited
    const source =
        \\x : [A([]), B([]), C(I64), D([]), E([])]
        \\x = C(42)
        \\
        \\result : I64
        \\result = match x {
        \\    C(n) => n
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Only C is inhabited, so matching just C is exhaustive
    try test_env.assertLastDefType("I64");
}

// Record field ordering tests
// These verify that record patterns are matched by field NAME, not position.
// Without field-name-based matching, patterns with different field orders
// would be incorrectly compared positionally, causing wrong results.

test "exhaustive - record patterns with fields in different order" {
    // Two patterns that match the same record fields but list them in different order.
    // With positional matching this would fail because:
    // - Row 1: { name: "Alice", age: _ } -> args[0]=name, args[1]=age
    // - Row 2: { age: 30, name: _ } -> args[0]=age, args[1]=name
    // Position 0 would incorrectly compare "Alice" (name) with 30 (age).
    const source =
        \\x : { name: Str, age: I64 }
        \\x = { name: "Alice", age: 30 }
        \\
        \\result = match x {
        \\    { name: "Alice", age: _ } => "first"
        \\    { age: 30, name: _ } => "second"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

test "redundant - record second pattern unreachable with different field order" {
    // When two patterns match the same conditions but with fields in different order,
    // the second should be flagged as redundant. This tests that field-name matching
    // correctly identifies that { age: _, name: _ } covers the same as { name: _, age: _ }.
    const source =
        \\x : { name: Str, age: I64 }
        \\x = { name: "Alice", age: 30 }
        \\
        \\result = match x {
        \\    { name: _, age: _ } => "first"
        \\    { age: _, name: _ } => "second"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Second pattern should be redundant - it's the same as the first just reordered
    try test_env.assertOneTypeError("Redundant Pattern");
}

test "exhaustive - record patterns with different field subsets" {
    // Patterns that destructure different subsets of fields with `..` should work correctly.
    // The exhaustiveness checker should treat the `..`-covered fields as wildcards.
    const source =
        \\x : { name: Str, age: I64, score: I64 }
        \\x = { name: "Alice", age: 30, score: 100 }
        \\
        \\result = match x {
        \\    { name: "Alice", .. } => "alice"
        \\    { age: 30, .. } => "thirty"
        \\    { score: 100, .. } => "perfect"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

// Regression test for issue #8931: list with only rest pattern should not be exhaustive
// Pattern [e, ..] doesn't cover empty list case
test "non-exhaustive - list with only rest pattern missing empty case" {
    const source =
        \\f : List(a) -> List(a)
        \\f = |input| {
        \\    match input {
        \\        [e, ..] => [e]
        \\    }
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // The pattern [e, ..] only matches non-empty lists, missing the empty list case
    try test_env.assertOneTypeError("Non Exhaustive Match");
}

// Regression test for issue #8935: wrong exhaustiveness error for match on tuple
// The patterns ([], _), (_, 0), ([_, ..], _) together cover all cases:
// - [] with any U64 (first pattern)
// - [_, ..] with any U64 (third pattern)
// Together these cover all lists. The middle pattern (_, 0) is just an optimization.

test "exhaustive - tuple with list and integer patterns" {
    const source =
        \\x : (List(I64), I64)
        \\x = ([1, 2, 3], 42)
        \\
        \\result : I64
        \\result = match x {
        \\    ([], _) => 0
        \\    (_, 0) => 1
        \\    ([_head, ..], _) => 2
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // This should be exhaustive - patterns 1 and 3 together cover all lists
    try test_env.assertLastDefType("I64");
}

// Repro for https://github.com/roc-lang/roc/issues/11939
// [True, ..], [.., False], and [] leave lists such as [False, True] uncovered.
test "non-exhaustive - prefix and suffix list patterns missing a mixed list" {
    const source =
        \\classify : List(Bool) -> Str
        \\classify = |flags| match flags {
        \\    [True, ..] => "starts with true"
        \\    [.., False] => "ends with false"
        \\    [] => "empty"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

// Lists of every length at least `max_prefix + max_suffix` must be checked with
// prefix and suffix elements in separate columns, so prefix and suffix patterns
// never stand in for each other.

test "exhaustive - prefix and suffix list patterns with the mixed lists covered" {
    const source =
        \\classify : List(Bool) -> Str
        \\classify = |flags| match flags {
        \\    [True, ..] => "starts with true"
        \\    [.., False] => "ends with false"
        \\    [False, .., True] => "false then true"
        \\    [] => "empty"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "non-exhaustive - prefix and suffix list patterns report the uncovered mixed lists" {
    const source =
        \\classify : List(Bool) -> Str
        \\classify = |flags| match flags {
        \\    [True, ..] => "starts with true"
        \\    [.., False] => "ends with false"
        \\    [] => "empty"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(
        \\**Non Exhaustive Match**
        \\This match expression doesn't cover all possible cases.
        \\```roc
        \\classify = |flags| match flags {
        \\```
        \\                   ^^^^^
        \\
        \\The value being matched on has type:
        \\        _List(Bool)_
        \\
        \\Missing patterns:
        \\        [False, .., True]
        \\
        \\Hint: Add branches to handle these cases, or use `_` to match anything.
        \\
        \\
    );
}

test "non-exhaustive - exact list pattern does not cover longer mixed lists" {
    const source =
        \\classify : List(Bool) -> Str
        \\classify = |flags| match flags {
        \\    [True, ..] => "starts with true"
        \\    [.., False] => "ends with false"
        \\    [False, True] => "false then true"
        \\    [] => "empty"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(
        \\**Non Exhaustive Match**
        \\This match expression doesn't cover all possible cases.
        \\```roc
        \\classify = |flags| match flags {
        \\```
        \\                   ^^^^^
        \\
        \\The value being matched on has type:
        \\        _List(Bool)_
        \\
        \\Missing patterns:
        \\        [False, _, .., True]
        \\
        \\Hint: Add branches to handle these cases, or use `_` to match anything.
        \\
        \\
    );
}

test "non-exhaustive - longer prefix and suffix list patterns" {
    const source =
        \\classify : List(Bool) -> Str
        \\classify = |flags| match flags {
        \\    [True, True, ..] => "a"
        \\    [.., False, False] => "b"
        \\    [_, False, ..] => "c"
        \\    [.., True, _] => "d"
        \\    [] => "e"
        \\    [_] => "f"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(
        \\**Non Exhaustive Match**
        \\This match expression doesn't cover all possible cases.
        \\```roc
        \\classify = |flags| match flags {
        \\```
        \\                   ^^^^^
        \\
        \\The value being matched on has type:
        \\        _List(Bool)_
        \\
        \\Missing patterns:
        \\        [False, True]
        \\        [False, True, .., False, True]
        \\
        \\Hint: Add branches to handle these cases, or use `_` to match anything.
        \\
        \\
    );
}

test "exhaustive - wildcard after prefix and suffix list patterns is not redundant" {
    const source =
        \\classify : List(Bool) -> Str
        \\classify = |flags| match flags {
        \\    [True, ..] => "starts with true"
        \\    [.., False] => "ends with false"
        \\    [] => "empty"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "exhaustive - prefix list pattern after prefix and suffix list patterns is not redundant" {
    const source =
        \\classify : List(Bool) -> Str
        \\classify = |flags| match flags {
        \\    [True, ..] => "starts with true"
        \\    [.., False] => "ends with false"
        \\    [] => "empty"
        \\    [False, ..] => "starts with false"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

test "redundant - single-element list covered by prefix and suffix list patterns" {
    const source =
        \\classify : List(Bool) -> Str
        \\classify = |flags| match flags {
        \\    [True, ..] => "starts with true"
        \\    [.., False] => "ends with false"
        \\    [False, .., True] => "false then true"
        \\    [] => "empty"
        \\    [_] => "one"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeWarning("Redundant Pattern");
}

test "non-exhaustive - prefix and suffix patterns on a nested list" {
    const source =
        \\classify : List(List(Bool)) -> Str
        \\classify = |rows| match rows {
        \\    [[True, ..], ..] => "a"
        \\    [[.., False], ..] => "b"
        \\    [[], ..] => "c"
        \\    [] => "d"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(
        \\**Non Exhaustive Match**
        \\This match expression doesn't cover all possible cases.
        \\```roc
        \\classify = |rows| match rows {
        \\```
        \\                  ^^^^^
        \\
        \\The value being matched on has type:
        \\        _List(List(Bool))_
        \\
        \\Missing patterns:
        \\        [[False, .., True], ..]
        \\
        \\Hint: Add branches to handle these cases, or use `_` to match anything.
        \\
        \\
    );
}

test "non-exhaustive - prefix and suffix list patterns inside a tuple" {
    const source =
        \\classify : (List(Bool), Bool) -> Str
        \\classify = |pair| match pair {
        \\    ([True, ..], True) => "a"
        \\    ([.., False], _) => "b"
        \\    ([], _) => "c"
        \\    (_, False) => "d"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(
        \\**Non Exhaustive Match**
        \\This match expression doesn't cover all possible cases.
        \\```roc
        \\classify = |pair| match pair {
        \\```
        \\                  ^^^^^
        \\
        \\The value being matched on has type:
        \\        _(List(Bool), Bool)_
        \\
        \\Missing patterns:
        \\        ([False, .., True], True)
        \\
        \\Hint: Add branches to handle these cases, or use `_` to match anything.
        \\
        \\
    );
}

test "non-exhaustive - missing tuple reports the uncovered value of each element" {
    const source =
        \\classify : (Bool, Bool, Bool) -> Str
        \\classify = |triple| match triple {
        \\    (True, _, _) => "a"
        \\    (False, True, _) => "b"
        \\    (False, False, True) => "c"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeErrorMsg(
        \\**Non Exhaustive Match**
        \\This match expression doesn't cover all possible cases.
        \\```roc
        \\classify = |triple| match triple {
        \\```
        \\                    ^^^^^
        \\
        \\The value being matched on has type:
        \\        _(Bool, Bool, Bool)_
        \\
        \\Missing patterns:
        \\        (False, False, False)
        \\
        \\Hint: Add branches to handle these cases, or use `_` to match anything.
        \\
        \\
    );
}

// Builtin inhabitedness: a builtin type has values exactly when its declared
// backing does, like any other nominal type. A user type that shares a
// builtin's name is an ordinary type.

/// Checks `source`, expecting only builtin-shadowing warnings and no type problems.
fn expectShadowingOnlyAndNoTypeProblems(source: []const u8) TestEnv.TestEnvError!void {
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    const diagnostics = try test_env.module_env.getDiagnostics();
    defer test_env.gpa.free(diagnostics);
    for (diagnostics) |diagnostic| {
        try std.testing.expectEqual(std.meta.activeTag(diagnostic), .builtin_type_shadowed_warning);
    }
    try std.testing.expectEqual(@as(usize, 0), test_env.checker.problems.problems.items.len);
}

test "exhaustive - an empty user nominal named like a number needs no Err" {
    try expectShadowingOnlyAndNoTypeProblems(
        \\U8 :: []
        \\
        \\f : Try(Str, U8) -> Str
        \\f = |t| match t {
        \\    Ok(s) => s
        \\}
    );
}

test "exhaustive - an empty user alias named like a number needs no Err" {
    try expectShadowingOnlyAndNoTypeProblems(
        \\U8 : []
        \\
        \\f : Try(Str, U8) -> Str
        \\f = |t| match t {
        \\    Ok(s) => s
        \\}
    );
}

test "exhaustive - an empty user nominal with an ordinary name needs no Err" {
    try expectShadowingOnlyAndNoTypeProblems(
        \\V8 :: []
        \\
        \\f : Try(Str, V8) -> Str
        \\f = |t| match t {
        \\    Ok(s) => s
        \\}
    );
}

test "non-exhaustive - the builtin U8 error payload requires Err" {
    const source =
        \\f : Try(Str, U8) -> Str
        \\f = |t| match t {
        \\    Ok(s) => s
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

test "non-exhaustive - a builtin number reached through a user alias requires Err" {
    const source =
        \\Byte : U8
        \\
        \\f : Try(Str, Byte) -> Str
        \\f = |t| match t {
        \\    Ok(s) => s
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

/// Checks that a `match` with only an `Ok` branch on `Try(Str, <error_type>)`
/// is rejected for its missing `Err` branch, which is what an error type that
/// has values requires.
fn expectErrBranchRequired(comptime error_type: []const u8) TestEnv.TestEnvError!void {
    const body =
        \\f = |t| match t {
        \\    Ok(s) => s
        \\}
    ;
    const source = "f : Try(Str, " ++ error_type ++ ") -> Str\n" ++ body;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("Non Exhaustive Match");
}

test "non-exhaustive - every builtin number error payload requires Err" {
    inline for (.{ "U8", "I8", "U16", "I16", "U32", "I32", "U64", "I64", "U128", "I128", "F32", "F64", "Dec" }) |number| {
        try expectErrBranchRequired(number);
    }
}

test "non-exhaustive - a builtin number inside a structure requires Err" {
    try expectErrBranchRequired("{ code : U16, message : Str }");
    try expectErrBranchRequired("(I64, F64)");
    try expectErrBranchRequired("[Code(U8)]");
}

test "non-exhaustive - a compiler-provided builtin error payload requires Err" {
    try expectErrBranchRequired("Str");
    try expectErrBranchRequired("U8x16");
    try expectErrBranchRequired("Box(U8)");
}

test "non-exhaustive - a collection of an empty type requires Err because the empty collection exists" {
    try expectErrBranchRequired("List([])");
    try expectErrBranchRequired("Set([])");
    try expectErrBranchRequired("Dict([], Str)");
    try expectErrBranchRequired("Dict(Str, [])");
    try expectErrBranchRequired("Iter([])");
}

// `Box(item)` has a value exactly when `item` does, exactly like the user
// wrapper `MyBox(a) := [B(a)]`.

/// Checks that a `match` with only an `Ok` branch on `Try(Str, <error_type>)`
/// is accepted, which is what an error type without values allows.
fn expectNoErrBranchNeeded(comptime declarations: []const u8, comptime error_type: []const u8) TestEnv.TestEnvError!void {
    const body =
        \\f = |t| match t {
        \\    Ok(s) => s
        \\}
    ;
    const source = declarations ++ "f : Try(Str, " ++ error_type ++ ") -> Str\n" ++ body;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertNoErrors();
}

/// Checks that an `Err` branch on `Try(Str, <error_type>)` is reported as
/// unmatchable.
fn expectErrBranchUnmatchable(comptime declarations: []const u8, comptime error_type: []const u8) TestEnv.TestEnvError!void {
    const body =
        \\f = |t| match t {
        \\    Ok(s) => s
        \\    Err(_) => ""
        \\}
    ;
    const source = declarations ++ "f : Try(Str, " ++ error_type ++ ") -> Str\n" ++ body;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeWarning("Unmatchable Pattern");
}

const my_box = "MyBox(a) := [B(a)]\n\n";

test "exhaustive - a Box of an empty type needs no Err, like a user wrapper" {
    try expectNoErrBranchNeeded("", "Box([])");
    try expectNoErrBranchNeeded(my_box, "MyBox([])");
}

test "unmatchable - an Err branch on a Box of an empty type, like a user wrapper" {
    try expectErrBranchUnmatchable("", "Box([])");
    try expectErrBranchUnmatchable(my_box, "MyBox([])");
}

test "non-exhaustive - a Box of a type with values requires Err, like a user wrapper" {
    try expectErrBranchRequired("Box(Str)");
    try expectErrBranchRequired("Box(List([]))");
    try expectErrBranchRequired("Box({})");
}

test "exhaustive - a Box is empty through nested structure" {
    try expectNoErrBranchNeeded("", "Box(Box([]))");
    try expectNoErrBranchNeeded("", "Box({ a : [], b : Str })");
    try expectNoErrBranchNeeded("", "Box((Str, []))");
    try expectNoErrBranchNeeded("E := []\n\n", "Box(E)");
    try expectNoErrBranchNeeded("", "{ boxed : Box([]) }");
}

test "non-exhaustive - a list of boxes of an empty type requires Err" {
    try expectErrBranchRequired("List(Box([]))");
}

// A free `_` in a `Box` payload is decided exactly as one in the payload of the
// user wrapper `MyBox(a) := [B(a)]`: a use decides it, and a top-level value no
// use decides closes it to `[]` when the module finishes.

/// Checks `body`, with `W` standing for the wrapper, once for `Box` and once
/// for the user twin, expecting both to be accepted.
fn expectBoxAndTwinAccepted(comptime body: []const u8) TestEnv.TestEnvError!void {
    inline for (.{ .{ "", "Box" }, .{ my_box, "MyBox" } }) |wrapper| {
        const source = wrapper[0] ++ comptime replaceWrapper(body, wrapper[1]);
        var test_env = try TestEnv.init("Test", source);
        defer test_env.deinit();

        try test_env.assertNoErrors();
    }
}

fn replaceWrapper(comptime body: []const u8, comptime wrapper: []const u8) []const u8 {
    comptime var result: []const u8 = "";
    comptime var start: usize = 0;
    inline for (body, 0..) |byte, index| {
        if (byte == '@') {
            result = result ++ body[start..index] ++ wrapper;
            start = index + 1;
        }
    }
    return result ++ body[start..];
}

const absent_payload_top_level =
    \\x : Try(Str, @(PAYLOAD))
    \\x = Ok("a")
    \\
    \\y : Try(Str, @(U8))
    \\y = x
;

const absent_payload_local =
    \\f : Str -> Try(Str, @(U8))
    \\f = |s| {
    \\    x : Try(Str, @(PAYLOAD))
    \\    x = Ok(s)
    \\    x
    \\}
;

const absent_payload_if_branches =
    \\f : Str, @(U8) -> Try(Str, @(U8))
    \\f = |s, b| {
    \\    x : Try(Str, @(PAYLOAD))
    \\    x = Ok(s)
    \\    if s == "" Err(b) else x
    \\}
;

fn withPayload(comptime body: []const u8, comptime payload: []const u8) []const u8 {
    comptime var result: []const u8 = "";
    comptime var start: usize = 0;
    comptime var index: usize = 0;
    inline while (index < body.len) : (index += 1) {
        if (index + 7 <= body.len and std.mem.eql(u8, body[index..][0..7], "PAYLOAD")) {
            result = result ++ body[start..index] ++ payload;
            start = index + 7;
        }
    }
    return result ++ body[start..];
}

test "a use decides an Err payload's Box(_) like a user wrapper's" {
    try expectBoxAndTwinAccepted(comptime withPayload(absent_payload_top_level, "_"));
    try expectBoxAndTwinAccepted(comptime withPayload(absent_payload_local, "_"));
    try expectBoxAndTwinAccepted(comptime withPayload(absent_payload_if_branches, "_"));
}

test "a Box of a written payload type with values is accepted like a user wrapper's" {
    try expectBoxAndTwinAccepted(comptime withPayload(absent_payload_top_level, "U8"));
    try expectBoxAndTwinAccepted(comptime withPayload(absent_payload_local, "U8"));
    try expectBoxAndTwinAccepted(comptime withPayload(absent_payload_if_branches, "U8"));
}

test "a top-level value's undecided Box(_) Err payload closes to Box([]) like a user wrapper's" {
    inline for (.{ .{ "", "Box" }, .{ my_box, "MyBox" } }) |wrapper| {
        var test_env = try TestEnv.init("Test", wrapper[0] ++ "x : Try(Str, " ++ wrapper[1] ++ "(_))\nx = Ok(\"a\")\n");
        defer test_env.deinit();

        try test_env.assertDefType("x", "Try(Str, " ++ wrapper[1] ++ "([]))");
    }
}
