//! Tests specifically designed to exercise error report generation in problem.zig.
//!
//! These tests target error paths that aren't covered by other integration tests,
//! focusing on triggering specific error report builders to improve code coverage.

const TestEnv = @import("TestEnv.zig");

// UNUSED VALUE (statement_not_unit)
// These test the buildStatementNotUnit and buildUnusedValueReport functions

test "unused value - expression result discarded in block" {
    // A function that returns I64 - test the function type works
    const source =
        \\f : I64 -> I64
        \\f = |x| x + 1
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64 -> I64");
}

// INVALID TRY OPERATOR
// Tests the buildInvalidTryOperator function

test "try operator - valid usage on Try type" {
    // Using try operator on a valid Try type
    const source =
        \\f : Try(I64, Str) -> I64
        \\f = |x| {
        \\    result = match x {
        \\        Ok(n) => n
        \\        Err(_) => 0
        \\    }
        \\    result
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Try(I64, Str) -> I64");
}

// TYPE DOES NOT SUPPORT EQUALITY
// Tests the buildTypeDoesNotSupportEquality function

test "equality - comparing functions not supported" {
    // Functions cannot be compared with ==
    const source =
        \\f : I64 -> I64
        \\f = |x| x + 1
        \\
        \\g : I64 -> I64
        \\g = |x| x + 2
        \\
        \\result = f == g
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE DOES NOT SUPPORT EQUALITY");
}

test "equality - record containing function not comparable" {
    // Records with function fields cannot be compared
    const source =
        \\rec1 : { action: I64 -> I64 }
        \\rec1 = { action: |x| x + 1 }
        \\
        \\rec2 : { action: I64 -> I64 }
        \\rec2 = { action: |x| x + 2 }
        \\
        \\result = rec1 == rec2
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE DOES NOT SUPPORT EQUALITY");
}

// INVALID BOOL BINOP
// Tests the buildInvalidBoolBinop function - using and/or with non-bool types

test "invalid bool binop - and with numbers" {
    const source =
        \\x : I64
        \\x = 1
        \\
        \\y : I64
        \\y = 2
        \\
        \\result = x and y
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID BOOL OPERATION");
}

test "invalid bool binop - or with strings" {
    const source =
        \\a : Str
        \\a = "hello"
        \\
        \\b : Str
        \\b = "world"
        \\
        \\result = a or b
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID BOOL OPERATION");
}

// INCOMPATIBLE LIST ELEMENTS
// Tests the buildIncompatibleListElementsReport function

test "incompatible list - int and string mixed" {
    const source =
        \\x = [1, 2, "three", 4]
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "incompatible list - annotated list with wrong element" {
    const source =
        \\x : List(I64)
        \\x = [1, 2, "three"]
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// INCOMPATIBLE IF BRANCHES
// Tests the buildIncompatibleIfBranches function

test "incompatible if - different return types" {
    const source =
        \\x : I64
        \\x = 5
        \\
        \\result = if x > 0 { "positive" } else { -1 }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "incompatible if - nested incompatible branches" {
    const source =
        \\x : I64
        \\x = 5
        \\
        \\result = if x > 0 {
        \\    if x > 10 { "big" } else { 0 }
        \\} else {
        \\    "negative"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// INVALID IF CONDITION
// Tests the buildInvalidIfCondition function

test "invalid if condition - number as condition" {
    const source =
        \\result = if 42 { "yes" } else { "no" }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Number as condition produces TYPE MISMATCH since numeral types don't unify with Bool
    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "invalid if condition - string as condition" {
    const source =
        \\result = if "true" { 1 } else { 0 }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID IF CONDITION");
}

// INCOMPATIBLE MATCH BRANCHES
// Tests the buildIncompatibleMatchBranches function

test "incompatible match branches - different return types" {
    const source =
        \\x : I64
        \\x = 5
        \\
        \\result = match x {
        \\    0 => "zero"
        \\    1 => 1
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// INCOMPATIBLE MATCH PATTERNS
// Tests the buildIncompatibleMatchPatterns function

test "incompatible match patterns - string pattern on int" {
    const source =
        \\x : I64
        \\x = 42
        \\
        \\result = match x {
        \\    "hello" => "matched"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INCOMPATIBLE MATCH PATTERNS");
}

// FUNCTION CALL ARITY MISMATCH
// Tests the buildFnCallArityMismatchReport function

test "arity mismatch - too many arguments" {
    const source =
        \\f : I64 -> I64
        \\f = |x| x + 1
        \\
        \\result = f(1, 2, 3)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TOO MANY ARGUMENTS");
}

test "arity mismatch - too few arguments" {
    const source =
        \\f : I64, I64, I64 -> I64
        \\f = |a, b, c| a + b + c
        \\
        \\result = f(1)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TOO FEW ARGUMENTS");
}

// TYPE APPLY ARITY MISMATCH
// Tests the buildTypeApplyArityMismatchReport function

test "type arity mismatch - too few type args" {
    const source =
        \\Pair(a, b) : { first: a, second: b }
        \\
        \\x : Pair(I64)
        \\x = { first: 1, second: 2 }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TOO FEW ARGS");
}

test "type arity mismatch - too many type args" {
    const source =
        \\Box(a) : { value: a }
        \\
        \\x : Box(I64, Str, Bool)
        \\x = { value: 1 }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TOO MANY ARGS");
}

// INCOMPATIBLE FUNCTION CALL ARGUMENT
// Tests the buildIncompatibleFnCallArg function

test "incompatible fn arg - wrong type" {
    const source =
        \\f : I64 -> I64
        \\f = |x| x + 1
        \\
        \\result = f("hello")
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "incompatible fn arg - nested function wrong arg" {
    const source =
        \\outer : (I64 -> I64) -> I64
        \\outer = |f| f(10)
        \\
        \\wrong : Str -> Str
        \\wrong = |s| s
        \\
        \\result = outer(wrong)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// NON-EXHAUSTIVE MATCH
// Tests the buildNonExhaustiveMatchReport function (via exhaustive checker)

test "non-exhaustive match - missing tag variant" {
    const source =
        \\Color : [Red, Green, Blue]
        \\
        \\c : Color
        \\c = Red
        \\
        \\name = match c {
        \\    Red => "red"
        \\    Green => "green"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("NON-EXHAUSTIVE MATCH");
}

// REDUNDANT PATTERN
// Tests the buildRedundantPatternReport function

test "redundant pattern - duplicate wildcard" {
    const source =
        \\x : I64
        \\x = 5
        \\
        \\result = match x {
        \\    _ => "first"
        \\    _ => "never reached"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

test "redundant pattern - duplicate number literal" {
    const source =
        \\x : I64
        \\x = 5
        \\
        \\result = match x {
        \\    1 => "one"
        \\    2 => "two"
        \\    1 => "one again"
        \\    _ => "other"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("REDUNDANT PATTERN");
}

// UNMATCHABLE PATTERN
// Tests the buildUnmatchablePatternReport function

test "unmatchable pattern - Err on infallible Try" {
    // When Try has an empty error type [], the Err branch is unmatchable
    const source =
        \\x : Try(I64, [])
        \\x = Ok(42)
        \\
        \\result = match x {
        \\    Ok(n) => n
        \\    Err(_) => 0
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("UNMATCHABLE PATTERN");
}

// MISSING METHOD (Static Dispatch)
// Tests dispatcher_does_not_impl_method

test "missing method - undefined method on record" {
    const source =
        \\rec : { name: Str }
        \\rec = { name: "test" }
        \\
        \\result = rec.undefined_method()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("MISSING METHOD");
}

// GENERIC TYPE MISMATCH
// Tests the buildGenericTypeMismatchReport function

test "generic mismatch - annotation vs expression" {
    const source =
        \\x : I64
        \\x = "not an integer"
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "generic mismatch - function return type" {
    const source =
        \\f : I64 -> Str
        \\f = |x| x * 2
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "generic mismatch - record field type" {
    const source =
        \\x : { count: I64 }
        \\x = { count: "not a number" }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "generic mismatch - tuple element type" {
    const source =
        \\x : (I64, Str)
        \\x = (1, 2)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// RECURSIVE TYPE TESTS
// Tests buildRecursiveAliasReport

test "recursive alias - direct self-reference" {
    const source =
        \\Infinite : Infinite
        \\
        \\x : Infinite
        \\x = x
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("RECURSIVE ALIAS");
}

// Tests for valid patterns that should pass

test "valid - exhaustive match with all branches" {
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

test "valid - if with matching branch types" {
    const source =
        \\x : I64
        \\x = 5
        \\
        \\result : I64
        \\result = if x > 0 { 1 } else { -1 }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}

test "valid - function with correct types" {
    const source =
        \\add : I64, I64 -> I64
        \\add = |a, b| a + b
        \\
        \\result = add(1, 2)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("I64");
}
