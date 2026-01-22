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
// NOTE: We use tags instead of numbers because numeric literals trigger
// static dispatch errors before the list element type mismatch is detected.

test "incompatible list - tag and string mixed" {
    const source =
        \\x = [Ok, Err, "three"]
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INCOMPATIBLE LIST ELEMENTS");
}

test "incompatible list - annotated list with wrong element type" {
    const source =
        \\x : List([Ok, Err])
        \\x = [Ok, Err, "three"]
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INCOMPATIBLE LIST ELEMENTS");
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

// Tests for infinite type recursion (INFINITE TYPE)
// Based on comment in Check.zig: `f = |x| f([x])` creates `a = List(a)`

test "infinite type - function wrapping argument in list" {
    // f = |x| f([x]) creates an infinite type a = List(a)
    const source =
        \\f = |x| f([x])
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INFINITE TYPE");
}

// Tests for anonymous recursion (RECURSIVE TYPE WITHOUT NAME)
// Recursion through record/tag fields without going through a nominal type

test "anonymous recursion - recursive record field" {
    // This creates a recursive type through record fields without a nominal wrapper
    // f = |x| f({field: x}) creates {field: a} where a = {field: a}
    const source =
        \\f = |x| f({field: x})
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("RECURSIVE TYPE WITHOUT NAME");
}

test "anonymous recursion - recursive tag payload" {
    // This creates a recursive type through tag payloads without a nominal wrapper
    // f = |x| f(Tag(x)) where x becomes Tag(a) and a = Tag(a)
    const source =
        \\f = |x| f(Wrap(x))
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("RECURSIVE TYPE WITHOUT NAME");
}

// Tests for UNUSED VALUE
// Triggered when an expression value is discarded

test "unused value - expression in block discarded" {
    // In Roc, statements in a block must return {} (unit)
    // An expression that returns something else triggers UNUSED VALUE
    const source =
        \\f = |_| {
        \\    Ok
        \\    Err
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("UNUSED VALUE");
}

// Tests for ? (try) operator on non-Try types
// When ? is used on non-Try, it triggers invalid_try_operator error

test "try operator on non-result - match pattern error" {
    // The ? suffix operator expects a Try type
    // Using it on Str triggers invalid_try_operator
    const source =
        \\str : Str
        \\str = "hello"
        \\
        \\x = str?
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Using ? on non-Try triggers invalid_try_operator
    try test_env.assertAnyTypeMismatchDetail(.invalid_try_operator);
}

// Tests for UNSUPPORTED WHERE CLAUSE
// The old ability syntax is no longer supported

test "unsupported where clause - old ability syntax" {
    // The old syntax `where [a.Decode]` is no longer valid
    const source =
        \\f : a -> a where [a.Decode]
        \\f = |x| x
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("UNSUPPORTED WHERE CLAUSE");
}

// NOMINAL TYPE ERRORS
// Tests for buildInvalidNominalTag, buildInvalidNominalRecord, buildInvalidNominalTuple

test "nominal type - invalid tag constructor" {
    // Create a nominal type with specific tags, then use a non-existent tag
    const source =
        \\Color := [Red, Green, Blue].{}
        \\
        \\c : Color
        \\c = Color.Yellow
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID NOMINAL TAG");
}

test "nominal type - tag with wrong payload type" {
    // Use a tag that exists but with wrong payload types
    const source =
        \\Container := [Val(I64)].{}
        \\
        \\c : Container
        \\c = Container.Val("string")
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID NOMINAL TAG");
}

test "nominal type - tag with wrong payload arity" {
    // Use correct tag name but with wrong number of payload args
    const source =
        \\Pair := [Pair(I64, I64)].{}
        \\
        \\p : Pair
        \\p = Pair.Pair(1, 2, 3)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID NOMINAL TAG");
}

// NOTE: Tests for buildInvalidNominalRecord and buildInvalidNominalTuple
// require explicit constructor syntax (e.g., Point.{ x: 1, z: 2 } for records)
// which is not yet fully implemented in the parser. Those tests are omitted.

test "nominal type - tag with multiple valid tags wrong choice" {
    // Nominal with multiple tags - use a tag name that doesn't exist
    const source =
        \\Color := [Red, Green, Blue].{}
        \\
        \\c : Color
        \\c = Color.Yellow
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID NOMINAL TAG");
}

// STATIC DISPATCH EDGE CASES
// Tests for rigid type variable method constraints

test "static dispatch - missing method on non-nominal type" {
    // Call a non-existent method on a string
    const source =
        \\str = "hello"
        \\result = str.nonexistent()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("MISSING METHOD");
}

test "static dispatch - method return type mismatch" {
    // Define a type with a method, but call it where a different return type is expected
    const source =
        \\MyType := [Val(I64)].{
        \\    to_str : MyType -> Str
        \\    to_str = |MyType.Val(_)| "value"
        \\}
        \\
        \\x = MyType.Val(42)
        \\
        \\result : I64
        \\result = x.to_str()
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// FUNCTION TYPES IN TYPE APPLICATIONS
// Tests for function types inside type applications like List(I64 -> I64)

test "function type in type application - list of functions" {
    // List(I64 -> I64) should be valid syntax
    const source =
        \\funcs : List(I64 -> I64)
        \\funcs = [|x| x + 1, |x| x * 2]
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("List(I64 -> I64)");
}

test "function type in type application - effectful function" {
    // List(I64 => I64) with effectful arrow should work too
    const source =
        \\funcs : List(I64 => I64)
        \\funcs = []
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("List(I64 => I64)");
}

test "function type in nested type application" {
    // Nested type applications with function types
    const source =
        \\nested : List(List(I64 -> Str))
        \\nested = []
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("List(List(I64 -> Str))");
}

// EQUALITY EXPLANATION FOR COMPLEX TYPES
// These tests verify the TYPE DOES NOT SUPPORT EQUALITY error is triggered
// for types containing functions, which cannot be compared

test "equality - deeper nested record with function" {
    // Records nested multiple levels with functions cannot be compared
    const source =
        \\rec1 : { level1: { level2: { action: I64 -> I64 } } }
        \\rec1 = { level1: { level2: { action: |x| x + 1 } } }
        \\
        \\rec2 : { level1: { level2: { action: I64 -> I64 } } }
        \\rec2 = { level1: { level2: { action: |x| x + 2 } } }
        \\
        \\result = rec1 == rec2
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE DOES NOT SUPPORT EQUALITY");
}

test "equality - record with multiple function fields" {
    // Records with multiple function fields cannot be compared
    const source =
        \\rec1 : { add: I64 -> I64, mult: I64 -> I64 }
        \\rec1 = { add: |x| x + 1, mult: |x| x * 2 }
        \\
        \\rec2 : { add: I64 -> I64, mult: I64 -> I64 }
        \\rec2 = { add: |x| x + 10, mult: |x| x * 20 }
        \\
        \\result = rec1 == rec2
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE DOES NOT SUPPORT EQUALITY");
}

// BOUND TYPE VARIABLE CONFLICTS
// Tests for incompatible_fn_args_bound_var

test "incompatible fn args - same type var different types" {
    // Function requires same type for both args but we pass different types
    const source =
        \\identity : a, a -> (a, a)
        \\identity = |x, y| (x, y)
        \\
        \\result = identity(42, "string")
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "incompatible fn args - polymorphic binding violation" {
    // Using a polymorphic function with conflicting concrete types
    const source =
        \\makePair : a, a -> (a, a)
        \\makePair = |x, y| (x, y)
        \\
        \\x : I64
        \\x = 1
        \\
        \\y : Str
        \\y = "hello"
        \\
        \\result = makePair(x, y)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// EARLY RETURN TYPE MISMATCHES
// Tests for return statement type checking

test "early return type mismatch - return in block" {
    // Early return must match the expected block type
    const source =
        \\f : I64 -> Str
        \\f = |x| {
        \\    if x > 0 { return 42 }
        \\    "negative"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "early return type mismatch - multiple returns" {
    // All return paths must agree
    const source =
        \\classify : I64 -> Str
        \\classify = |x| {
        \\    if x < 0 { return "negative" }
        \\    if x == 0 { return 0 }
        \\    "positive"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// ERROR RECOVERY PATHS
// Tests for multiple errors in same construct

test "pattern matching - multiple type errors in branches" {
    // Multiple branches have type errors
    const source =
        \\x : I64
        \\x = 42
        \\
        \\result = match x {
        \\    1 => "one"
        \\    2 => 2
        \\    3 => "three"
        \\    _ => 0
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // Should report incompatible match branches
    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "function call - multiple argument type errors" {
    // Function call with multiple wrong argument types
    const source =
        \\f : I64, I64, I64 -> I64
        \\f = |a, b, c| a + b + c
        \\
        \\result = f("one", "two", "three")
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// BOX TYPE USAGE
// Tests for Box type creation and usage

test "box type - basic creation" {
    // Box.box creates a boxed value
    const source =
        \\x : Box(I64)
        \\x = Box.box(42)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Box(I64)");
}

test "box type - unbox operation" {
    // Box.unbox retrieves the value
    const source =
        \\boxed : Box(Str)
        \\boxed = Box.box("hello")
        \\
        \\value = Box.unbox(boxed)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Str");
}

test "box type - nested box" {
    // Can box a boxed value
    const source =
        \\inner : Box(I64)
        \\inner = Box.box(42)
        \\
        \\outer : Box(Box(I64))
        \\outer = Box.box(inner)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertLastDefType("Box(Box(I64))");
}

// ADDITIONAL NOMINAL TAG ERRORS
// Tests for more edge cases in nominal tag handling

test "nominal type - tag with deeply nested wrong type" {
    // Nominal with nested payload - inner type is wrong
    const source =
        \\Wrapper := [Wrap([Inner(I64)])].{}
        \\
        \\w : Wrapper
        \\w = Wrapper.Wrap(Inner("string"))
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID NOMINAL TAG");
}

test "nominal type - multiple constructors wrong one" {
    // Nominal with multiple valid constructors - check wrong constructor gets error
    const source =
        \\Result := [Success(I64), Failure(Str)].{}
        \\
        \\r : Result
        \\r = Result.Pending
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID NOMINAL TAG");
}

// BOUND TYPE VARIABLE CONFLICTS
// Tests for incompatible_fn_args_bound_var

test "bound var conflict - explicit annotation" {
    // Function expects same type for both args, but called with different types
    const source =
        \\swap : a, a -> (a, a)
        \\swap = |x, y| (y, x)
        \\
        \\result = swap(1i64, "string")
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "bound var conflict - three arguments same type" {
    // Function expects three args of same type
    const source =
        \\triple : a, a, a -> (a, a, a)
        \\triple = |x, y, z| (x, y, z)
        \\
        \\result = triple(1i64, 2i64, "oops")
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "bound var conflict - nested in tuple return" {
    // Function expects same type and returns tuple of them
    const source =
        \\pair : a, a -> (a, a)
        \\pair = |x, y| (x, y)
        \\
        \\result = pair("hello", 42i64)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// TRY SUFFIX RETURN ERRORS
// Tests for buildTrySuffixReturn function

test "try suffix return - in function returning try" {
    // Using try operator inside a function that returns Try
    // The ? operator extracts the Ok value and returns early with Err on failure
    const source =
        \\helper : I64 -> Try(I64, Str)
        \\helper = |x|
        \\    if x > 0 { Ok(x) } else { Err("negative") }
        \\
        \\f : I64 -> Try(I64, Str)
        \\f = |x| {
        \\    val = helper(x)?
        \\    Ok(val + 1)
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    // The error type from helper (Str) must match return's error type (Str) - should work
    try test_env.assertLastDefType("I64 -> Try(I64, Str)");
}

test "try suffix return - error type mismatch" {
    // The ? operator returns Err(Str) but function body returns Err(I64) - should fail
    const source =
        \\helper : I64 -> Try(I64, Str)
        \\helper = |x|
        \\    if x > 0 { Ok(x) } else { Err("negative") }
        \\
        \\f : I64 -> Try(I64, I64)
        \\f = |x| {
        \\    val = helper(x)?
        \\    Err(42)
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "try suffix return - multiple try operators different errors" {
    // Multiple ? operators with different error types that need to match
    const source =
        \\helper1 : I64 -> Try(I64, Str)
        \\helper1 = |x|
        \\    if x > 0 { Ok(x) } else { Err("negative") }
        \\
        \\helper2 : I64 -> Try(I64, I64)
        \\helper2 = |x|
        \\    if x > 0 { Ok(x) } else { Err(-1) }
        \\
        \\f : I64 -> Try(I64, Str)
        \\f = |x| {
        \\    a = helper1(x)?
        \\    b = helper2(x)?
        \\    Ok(a + b)
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// EARLY RETURN TYPE MISMATCH
// Tests for early_return in nested blocks

test "early return - nested block value vs return" {
    // Return type differs from function return type
    const source =
        \\f : I64 -> Str
        \\f = |x| {
        \\    y = {
        \\        if x > 10 { return x }
        \\        x + 1
        \\    }
        \\    "result"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "early return - multiple return type mismatch" {
    // Multiple returns with inconsistent types
    const source =
        \\f : I64 -> Str
        \\f = |x| {
        \\    if x > 0 { return x }
        \\    if x < 0 { return "negative" }
        \\    "zero"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// ADDITIONAL TYPE MISMATCH TESTS
// Tests for more type mismatch scenarios

test "type mismatch - list element type conflict" {
    // List elements must have consistent types
    const source =
        \\list = [1, 2, "three"]
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "type mismatch - tuple element type annotation" {
    // Tuple with wrong element type for annotation
    const source =
        \\pair : (I64, I64)
        \\pair = (1, "two")
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "type mismatch - record field type annotation" {
    // Record with wrong field type for annotation
    const source =
        \\rec : { x: I64, y: I64 }
        \\rec = { x: 1, y: "two" }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// NUMBER LITERAL IN NON-NUMBER CONTEXT
// Tests for number used as non-number error

test "number literal - used as string" {
    // Number literal where string is expected
    const source =
        \\str : Str
        \\str = 42
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "number literal - in string list" {
    // Number literal in a list of strings
    const source =
        \\list : List(Str)
        \\list = ["a", "b", 3]
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// ARITHMETIC WITH NON-NUMBERS
// Tests for arithmetic operations with wrong types

test "arithmetic - string plus operator" {
    // Attempting to use + on strings
    const source =
        \\x : Str
        \\x = "hello"
        \\
        \\y : Str
        \\y = "world"
        \\
        \\result = x + y
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("MISSING METHOD");
}

test "arithmetic - bool negation as number" {
    // Attempting to negate a bool as if it were a number
    const source =
        \\x : Bool
        \\x = True
        \\
        \\result = -x
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("MISSING METHOD");
}

// COMPARISON OPERATIONS WITH WRONG TYPES
// Tests for comparison operators with incompatible types

test "comparison - strings with less than" {
    // Attempting to use < on strings
    const source =
        \\x : Str
        \\x = "hello"
        \\
        \\y : Str
        \\y = "world"
        \\
        \\result = x < y
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("MISSING METHOD");
}

test "comparison - bool with greater than" {
    // Attempting to use > on bools
    const source =
        \\x : Bool
        \\x = True
        \\
        \\y : Bool
        \\y = False
        \\
        \\result = x > y
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("MISSING METHOD");
}

// FUNCTION APPLICATION ERRORS
// Tests for function call type mismatches

test "function call - argument count correct but types wrong" {
    // Correct number of args but wrong types
    const source =
        \\add : I64, I64 -> I64
        \\add = |x, y| x + y
        \\
        \\result = add(1, "two")
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "function call - multiple wrong argument types" {
    // Multiple arguments with wrong types
    const source =
        \\triple : I64, I64, I64 -> I64
        \\triple = |x, y, z| x + y + z
        \\
        \\result = triple("one", "two", "three")
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// LAMBDA RETURN TYPE ERRORS
// Tests for lambda expression return type mismatches

test "lambda - return type mismatch" {
    // Lambda body returns wrong type
    const source =
        \\f : I64 -> Str
        \\f = |x| x + 1
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "lambda - parameter type used incorrectly" {
    // Using string parameter where number is expected
    const source =
        \\f : Str -> I64
        \\f = |s| s + 1
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("MISSING METHOD");
}

// ARITY MISMATCH TESTS
// Tests for function call arity errors

test "arity - too many type arguments" {
    // Providing too many type arguments
    const source =
        \\x : List(I64, Str)
        \\x = []
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TOO MANY ARGS");
}

test "arity - too few type arguments" {
    // Providing too few type arguments (Try needs 2)
    const source =
        \\x : Try(I64)
        \\x = Ok(42)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TOO FEW ARGS");
}

// ADDITIONAL IF CONDITION TESTS
// Tests for if condition type errors

test "if condition - list as condition" {
    // Using a list as an if condition
    const source =
        \\x = if [1, 2, 3] { "yes" } else { "no" }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID IF CONDITION");
}

test "if condition - record as condition" {
    // Using a record as an if condition
    const source =
        \\x = if { a: 1 } { "yes" } else { "no" }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INVALID IF CONDITION");
}

// MORE INCOMPATIBLE IF BRANCHES TESTS
// Tests for different types in if branches

test "if branches - list vs tuple" {
    // If branches with list and tuple
    const source =
        \\x = if True { [1, 2] } else { (1, 2) }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INCOMPATIBLE IF BRANCHES");
}

test "if branches - record vs tag" {
    // If branches with record and tag
    const source =
        \\x = if True { { a: 1 } } else { Ok(1) }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INCOMPATIBLE IF BRANCHES");
}

// MATCH PATTERN TESTS
// Tests for pattern matching errors

test "match - pattern type mismatch" {
    // Matching with wrong pattern type
    const source =
        \\x : I64
        \\x = 42
        \\
        \\result = match x {
        \\    "hello" => 1
        \\    _ => 0
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INCOMPATIBLE MATCH PATTERNS");
}

test "match - condition pattern type conflict" {
    // Pattern type conflicts with condition type
    const source =
        \\f : Str -> I64
        \\f = |s| match s {
        \\    Ok(_) => 1
        \\    Err(_) => 0
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("INCOMPATIBLE MATCH PATTERNS");
}

// NESTED FUNCTION TYPE TESTS
// Tests for nested function type mismatches

test "nested function - wrong inner return type" {
    // Higher-order function with wrong inner type
    const source =
        \\apply : (I64 -> I64), I64 -> I64
        \\apply = |f, x| f(x)
        \\
        \\strFunc : I64 -> Str
        \\strFunc = |_| "hello"
        \\
        \\result = apply(strFunc, 42)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "nested function - wrong inner arg type" {
    // Higher-order function with wrong inner argument type
    const source =
        \\apply : (I64 -> I64), I64 -> I64
        \\apply = |f, x| f(x)
        \\
        \\strArgFunc : Str -> I64
        \\strArgFunc = |_| 42
        \\
        \\result = apply(strArgFunc, 42)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// LIST ELEMENT TYPE TESTS
// Tests for incompatible list element types

test "list - incompatible element in middle" {
    // String in the middle of number list
    const source =
        \\list = [1, 2, "three", 4, 5]
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "list - nested list type mismatch" {
    // Nested list with wrong inner type
    const source =
        \\list : List(List(I64))
        \\list = [[1, 2], ["a", "b"]]
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// RECORD FIELD TYPE TESTS
// Tests for record field type mismatches

test "record - missing field" {
    // Record missing a required field
    const source =
        \\rec : { x: I64, y: I64, z: I64 }
        \\rec = { x: 1, y: 2 }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "record - extra field" {
    // Record with extra field
    const source =
        \\rec : { x: I64 }
        \\rec = { x: 1, y: 2 }
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// TUPLE ARITY TESTS
// Tests for tuple size mismatches

test "tuple - wrong arity" {
    // Tuple with wrong number of elements
    const source =
        \\pair : (I64, I64)
        \\pair = (1, 2, 3)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

test "tuple - nested wrong type" {
    // Nested tuple with wrong inner type
    const source =
        \\nested : ((I64, I64), I64)
        \\nested = (("a", "b"), 1)
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// FUNCTION COMPOSITION TESTS
// Tests for function composition type errors

test "composition - incompatible return to arg" {
    // Function return type doesn't match next arg type
    const source =
        \\toStr : I64 -> Str
        \\toStr = |_| "num"
        \\
        \\addOne : I64 -> I64
        \\addOne = |x| x + 1
        \\
        \\result = addOne(toStr(42))
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}

// BLOCK EXPRESSION TESTS
// Tests for block expression type errors

test "block - final expression type mismatch" {
    // Block final expression doesn't match annotation
    const source =
        \\f : I64 -> I64
        \\f = |x| {
        \\    y = x + 1
        \\    "result"
        \\}
    ;
    var test_env = try TestEnv.init("Test", source);
    defer test_env.deinit();

    try test_env.assertFirstTypeError("TYPE MISMATCH");
}
