//! End-to-end tests for exhaustiveness and redundancy checking.
//!
//! These tests verify that:
//! - Non-exhaustive matches produce NON-EXHAUSTIVE MATCH errors
//! - Redundant patterns produce REDUNDANT PATTERN warnings
//! - Exhaustive matches pass without errors

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

test "non-exhaustive - missing Err case" {
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

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
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

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
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

    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
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

    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
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

    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
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

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
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

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
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

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
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
    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
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
    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
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
    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
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

    try test_env.assertFirstTypeError("UNMATCHABLE PATTERN");
}

// Additional Inhabitedness Edge Cases
// These tests verify correct handling of various uninhabited type scenarios.

test "exhaustive - triply nested empty errors" {
    // 3+ levels of nesting with empty types
    const source =
        \\x : Try(Try(Try(I64, []), []), [])
        \\x = Ok(Ok(Ok(42)))
        \\
        \\result : I64
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
    try test_env.assertFirstTypeError("UNMATCHABLE PATTERN");
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
    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
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

    try test_env.assertFirstTypeError("UNMATCHABLE PATTERN");
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

    try test_env.assertFirstTypeError("UNMATCHABLE PATTERN");
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

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
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
    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
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
    try test_env.assertOneTypeError("REDUNDANT PATTERN");
}

test "exhaustive - record patterns with different field subsets" {
    // Patterns that destructure different subsets of fields should work correctly.
    // The exhaustiveness checker should understand that unmentioned fields are wildcards.
    const source =
        \\x : { name: Str, age: I64, score: I64 }
        \\x = { name: "Alice", age: 30, score: 100 }
        \\
        \\result = match x {
        \\    { name: "Alice" } => "alice"
        \\    { age: 30 } => "thirty"
        \\    { score: 100 } => "perfect"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

// Float and Decimal Literal Pattern Tests
// These tests exercise the pattern conversion for floating point literals.

test "exhaustive - F32 literal patterns with wildcard" {
    // Matching on F32 with specific literals requires a wildcard for exhaustiveness
    const source =
        \\x : F32
        \\x = 1.0f32
        \\
        \\result = match x {
        \\    1.0f32 => "one"
        \\    2.0f32 => "two"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

test "non-exhaustive - F32 literal patterns without wildcard" {
    // F32 literals alone are not exhaustive
    const source =
        \\x : F32
        \\x = 1.0f32
        \\
        \\result = match x {
        \\    1.0f32 => "one"
        \\    2.0f32 => "two"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
}

test "exhaustive - F64 literal patterns with wildcard" {
    // Matching on F64 with specific literals requires a wildcard for exhaustiveness
    const source =
        \\x : F64
        \\x = 3.14f64
        \\
        \\result = match x {
        \\    0.0f64 => "zero"
        \\    3.14f64 => "pi"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

test "non-exhaustive - F64 literal patterns without wildcard" {
    // F64 literals alone are not exhaustive
    const source =
        \\x : F64
        \\x = 3.14f64
        \\
        \\result = match x {
        \\    0.0f64 => "zero"
        \\    3.14f64 => "pi"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
}

test "exhaustive - Dec literal patterns with wildcard" {
    // Matching on Dec with specific literals requires a wildcard for exhaustiveness
    const source =
        \\x : Dec
        \\x = 1.0dec
        \\
        \\result = match x {
        \\    1.0dec => "one"
        \\    2.0dec => "two"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

test "non-exhaustive - Dec literal patterns without wildcard" {
    // Dec literals alone are not exhaustive
    const source =
        \\x : Dec
        \\x = 1.0dec
        \\
        \\result = match x {
        \\    1.0dec => "one"
        \\    2.0dec => "two"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
}

test "redundant - duplicate F32 literal patterns" {
    // Same F32 literal matched twice is redundant
    const source =
        \\x : F32
        \\x = 1.0f32
        \\
        \\result = match x {
        \\    1.0f32 => "first"
        \\    1.0f32 => "second"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

test "redundant - duplicate Dec literal patterns" {
    // Same Dec literal matched twice is redundant
    const source =
        \\x : Dec
        \\x = 1.0dec
        \\
        \\result = match x {
        \\    1.0dec => "first"
        \\    1.0dec => "second"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

// Type Mismatch Tests for Match Branches
// These tests exercise the incompatible_match_branches error path.

test "type mismatch - incompatible match branches return different types" {
    // When match branches return incompatible types, should trigger type mismatch
    const source =
        \\x : Try(I64, Str)
        \\x = Ok(42i64)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\    Err(s) => s
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // The branches return I64 and Str which don't unify
    try test_env.assertFirstTypeError("INCOMPATIBLE MATCH BRANCHES");
}

test "type mismatch - if branches with incompatible types" {
    // If expression with branches returning incompatible types
    const source =
        \\x = if Bool.true { 42i64 } else { "string" }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INCOMPATIBLE IF BRANCHES");
}

// Additional Type Error Tests

test "type mismatch - boolean and with non-Bool left side" {
    // Using 'and' with non-Bool on left side
    const source =
        \\x : I64
        \\x = 42i64
        \\
        \\b : Bool
        \\b = Bool.true
        \\
        \\result = x and b
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID BOOL OPERATION");
}

test "type mismatch - boolean or with non-Bool right side" {
    // Using 'or' with non-Bool on right side
    const source =
        \\x : I64
        \\x = 42i64
        \\
        \\b : Bool
        \\b = Bool.true
        \\
        \\result = b or x
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID BOOL OPERATION");
}

test "type mismatch - function call with wrong number of args" {
    // Calling function with wrong arity
    const source =
        \\f = |x, y| x + y
        \\result = f(1i64)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TOO FEW ARGUMENTS");
}

test "type mismatch - function call with wrong arg type" {
    // Calling function with wrong argument type
    const source =
        \\f : I64 -> I64
        \\f = |x| x + 1
        \\result = f("string")
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
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
    try test_env.assertOneTypeError("NON-EXHAUSTIVE MATCH");
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
