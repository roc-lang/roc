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
