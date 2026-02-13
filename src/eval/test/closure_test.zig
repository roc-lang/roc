//! Comprehensive tests for closures, captures, and lambda lifting.
//!
//! These tests verify that the full pipeline (type-check → mono → lambda lift → codegen)
//! correctly handles closures with captures, functions returning functions,
//! higher-order functions, and lambda set dispatch.
//!
//! The Roc compilation strategy requires:
//! - Every lambda becomes a top-level Procedure with captures as an explicit parameter
//! - Lambda sets are defunctionalized: call sites switch on a discriminant to pick
//!   which Procedure to call and extract the corresponding capture payload
//! - No heap-allocated closures — captures live in tagged union payloads on the stack

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectI64 = helpers.runExpectI64;
const runExpectStr = helpers.runExpectStr;

// =============================================================================
// TIER 1: Basic closure with captures
// =============================================================================

test "closure: lambda capturing one local variable" {
    const code =
        \\{
        \\    y = 10
        \\    f = |x| x + y
        \\    f(5)
        \\}
    ;
    try runExpectI64(code, 15, .no_trace);
}

test "closure: lambda capturing two local variables" {
    const code =
        \\{
        \\    a = 3
        \\    b = 7
        \\    f = |x| x + a + b
        \\    f(10)
        \\}
    ;
    try runExpectI64(code, 20, .no_trace);
}

test "closure: lambda capturing a string" {
    const code =
        \\{
        \\    greeting = "Hello"
        \\    f = |name| Str.concat(greeting, name)
        \\    f(" World")
        \\}
    ;
    try runExpectStr(code, "Hello World", .no_trace);
}

test "closure: lambda capturing multiple strings" {
    const code =
        \\{
        \\    prefix = "Hello"
        \\    suffix = "!"
        \\    f = |name| Str.concat(Str.concat(prefix, name), suffix)
        \\    f(" World")
        \\}
    ;
    try runExpectStr(code, "Hello World!", .no_trace);
}

// =============================================================================
// TIER 2: Functions returning functions (closure escaping defining scope)
// =============================================================================

test "closure: function returning a closure (make_adder)" {
    const code =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    add5(10)
        \\}
    ;
    try runExpectI64(code, 15, .no_trace);
}

test "closure: function returning a closure, called twice" {
    const code =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    a = add5(10)
        \\    b = add5(20)
        \\    a + b
        \\}
    ;
    try runExpectI64(code, 45, .no_trace);
}

test "closure: two different closures from same factory" {
    const code =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add3 = make_adder(3)
        \\    add7 = make_adder(7)
        \\    add3(10) + add7(10)
        \\}
    ;
    try runExpectI64(code, 30, .no_trace);
}

test "closure: function returning a closure over string" {
    const code =
        \\{
        \\    make_greeter = |greeting| |name| Str.concat(greeting, name)
        \\    greet = make_greeter("Hi ")
        \\    greet("Alice")
        \\}
    ;
    try runExpectStr(code, "Hi Alice", .no_trace);
}

test "closure: two-level deep closure (function returning function returning function)" {
    const code =
        \\{
        \\    make_op = |a| |b| |x| x + a + b
        \\    add_3_and_4 = make_op(3)(4)
        \\    add_3_and_4(10)
        \\}
    ;
    try runExpectI64(code, 17, .no_trace);
}

// =============================================================================
// TIER 3: Higher-order functions with closure arguments
// =============================================================================

test "closure: passing closure to higher-order function" {
    const code =
        \\{
        \\    apply = |f, x| f(x)
        \\    y = 10
        \\    apply(|x| x + y, 5)
        \\}
    ;
    try runExpectI64(code, 15, .no_trace);
}

test "closure: passing two different closures to same HOF" {
    const code =
        \\{
        \\    apply = |f, x| f(x)
        \\    a = 10
        \\    b = 20
        \\    r1 = apply(|x| x + a, 5)
        \\    r2 = apply(|x| x + b, 5)
        \\    r1 + r2
        \\}
    ;
    try runExpectI64(code, 40, .no_trace);
}

test "closure: HOF calling closure argument twice" {
    const code =
        \\{
        \\    apply_twice = |f, x| f(f(x))
        \\    y = 3
        \\    apply_twice(|x| x + y, 10)
        \\}
    ;
    try runExpectI64(code, 16, .no_trace);
}

test "closure: HOF with closure returning string" {
    const code =
        \\{
        \\    apply = |f, x| f(x)
        \\    prefix = "Hello "
        \\    apply(|name| Str.concat(prefix, name), "World")
        \\}
    ;
    try runExpectStr(code, "Hello World", .no_trace);
}

// =============================================================================
// TIER 4: Polymorphic functions with closures
// =============================================================================

test "closure: polymorphic identity applied to closure result" {
    const code =
        \\{
        \\    id = |x| x
        \\    y = 10
        \\    f = |x| x + y
        \\    id(f(5))
        \\}
    ;
    try runExpectI64(code, 15, .no_trace);
}

test "closure: polymorphic function used with both int and string closures" {
    const code =
        \\{
        \\    apply = |f, x| f(x)
        \\    n = 10
        \\    prefix = "Hi "
        \\    num_result = apply(|x| x + n, 5)
        \\    str_result = apply(|s| Str.concat(prefix, s), "Bob")
        \\    if (num_result > 0) str_result else ""
        \\}
    ;
    try runExpectStr(code, "Hi Bob", .no_trace);
}

// =============================================================================
// TIER 5: Closure over closure (nested captures)
// =============================================================================

test "closure: closure capturing another closure" {
    const code =
        \\{
        \\    y = 5
        \\    inner = |x| x + y
        \\    outer = |x| inner(x) * 2
        \\    outer(10)
        \\}
    ;
    try runExpectI64(code, 30, .no_trace);
}

test "closure: closure capturing a factory-produced closure" {
    const code =
        \\{
        \\    make_adder = |n| |x| x + n
        \\    add5 = make_adder(5)
        \\    double_add5 = |x| add5(x) * 2
        \\    double_add5(10)
        \\}
    ;
    try runExpectI64(code, 30, .no_trace);
}

// =============================================================================
// TIER 6: Multiple closures with different captures at same call site
// (lambda set dispatch - the core of defunctionalization)
// =============================================================================

test "closure: if-else choosing between two closures with different captures" {
    const code =
        \\{
        \\    a = 10
        \\    b = 20
        \\    f = if (True) |x| x + a else |x| x + b
        \\    f(5)
        \\}
    ;
    try runExpectI64(code, 15, .no_trace);
}

test "closure: if-else choosing between two closures, false branch" {
    const code =
        \\{
        \\    a = 10
        \\    b = 20
        \\    f = if (False) |x| x + a else |x| x + b
        \\    f(5)
        \\}
    ;
    try runExpectI64(code, 25, .no_trace);
}

test "closure: if-else choosing between closures with different capture counts" {
    const code =
        \\{
        \\    a = 10
        \\    b = 20
        \\    c = 30
        \\    f = if (True) |x| x + a else |x| x + b + c
        \\    f(5)
        \\}
    ;
    try runExpectI64(code, 15, .no_trace);
}

// =============================================================================
// TIER 7: Closure used in data structures
// =============================================================================

test "closure: closure stored in record field then called" {
    const code =
        \\{
        \\    y = 10
        \\    rec = { f: |x| x + y }
        \\    rec.f(5)
        \\}
    ;
    try runExpectI64(code, 15, .no_trace);
}

test "closure: two closures in record, each with own captures" {
    const code =
        \\{
        \\    a = 10
        \\    b = 20
        \\    rec = { add_a: |x| x + a, add_b: |x| x + b }
        \\    rec.add_a(5) + rec.add_b(5)
        \\}
    ;
    try runExpectI64(code, 40, .no_trace);
}

// =============================================================================
// TIER 8: Composition and chaining
// =============================================================================

test "closure: compose two functions" {
    const code =
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    double = |x| x * 2
        \\    add1 = |x| x + 1
        \\    double_then_add1 = compose(add1, double)
        \\    double_then_add1(5)
        \\}
    ;
    try runExpectI64(code, 11, .no_trace);
}

test "closure: compose with captures" {
    const code =
        \\{
        \\    compose = |f, g| |x| f(g(x))
        \\    a = 3
        \\    b = 7
        \\    add_a = |x| x + a
        \\    add_b = |x| x + b
        \\    add_both = compose(add_a, add_b)
        \\    add_both(10)
        \\}
    ;
    try runExpectI64(code, 20, .no_trace);
}

test "closure: pipe (flip of compose)" {
    const code =
        \\{
        \\    pipe = |x, f| f(x)
        \\    y = 10
        \\    pipe(5, |x| x + y)
        \\}
    ;
    try runExpectI64(code, 15, .no_trace);
}

// =============================================================================
// TIER 9: Recursive closures and self-reference
// =============================================================================

test "closure: recursive function in let binding" {
    // factorial via named recursion
    const code =
        \\{
        \\    factorial = |n| if (n <= 1) 1 else n * factorial(n - 1)
        \\    factorial(5)
        \\}
    ;
    try runExpectI64(code, 120, .no_trace);
}

test "closure: mutual recursion between two closures" {
    const code =
        \\{
        \\    is_even = |n| if (n == 0) True else is_odd(n - 1)
        \\    is_odd = |n| if (n == 0) False else is_even(n - 1)
        \\    if (is_even(4)) 1 else 0
        \\}
    ;
    try runExpectI64(code, 1, .no_trace);
}

// =============================================================================
// TIER 10: Extremely complex / stress tests
// =============================================================================

test "closure: triple-nested closure factory" {
    // make_op returns a closure that returns a closure that returns a closure
    const code =
        \\{
        \\    level1 = |a| |b| |c| |x| x + a + b + c
        \\    level2 = level1(1)
        \\    level3 = level2(2)
        \\    level4 = level3(3)
        \\    level4(10)
        \\}
    ;
    try runExpectI64(code, 16, .no_trace);
}

test "closure: closure capturing another closure that captures a third" {
    const code =
        \\{
        \\    a = 1
        \\    f = |x| x + a
        \\    b = 2
        \\    g = |x| f(x) + b
        \\    c = 3
        \\    h = |x| g(x) + c
        \\    h(10)
        \\}
    ;
    try runExpectI64(code, 16, .no_trace);
}

test "closure: HOF receiving closure, returning closure that captures the argument closure" {
    // transform takes a function and returns a new function that applies it twice
    const code =
        \\{
        \\    make_doubler = |f| |x| f(f(x))
        \\    add3 = |x| x + 3
        \\    double_add3 = make_doubler(add3)
        \\    double_add3(10)
        \\}
    ;
    try runExpectI64(code, 16, .no_trace);
}

test "closure: HOF receiving closure with captures, returning closure that captures it" {
    const code =
        \\{
        \\    n = 5
        \\    add_n = |x| x + n
        \\    make_doubler = |f| |x| f(f(x))
        \\    double_add_n = make_doubler(add_n)
        \\    double_add_n(10)
        \\}
    ;
    try runExpectI64(code, 20, .no_trace);
}

test "closure: chained closure factories with accumulating captures" {
    const code =
        \\{
        \\    step1 = |a| |b| |c| a + b + c
        \\    step2 = step1(100)
        \\    step3 = step2(20)
        \\    step3(3)
        \\}
    ;
    try runExpectI64(code, 123, .no_trace);
}

test "closure: polymorphic HOF with closures capturing different types" {
    // apply is polymorphic, used with int closure then string closure
    const code =
        \\{
        \\    apply = |f, x| f(x)
        \\    offset = 100
        \\    prefix = "Result: "
        \\    num = apply(|x| x + offset, 23)
        \\    if (num > 0) apply(|s| Str.concat(prefix, s), "yes") else "no"
        \\}
    ;
    try runExpectStr(code, "Result: yes", .no_trace);
}

test "closure: closure over bool used in conditional" {
    const code =
        \\{
        \\    flag = True
        \\    choose = |a, b| if (flag) a else b
        \\    choose(42, 0)
        \\}
    ;
    try runExpectI64(code, 42, .no_trace);
}

test "closure: deeply nested blocks each adding captures" {
    const code =
        \\{
        \\    a = 1
        \\    r1 = {
        \\        b = 2
        \\        r2 = {
        \\            c = 3
        \\            f = |x| x + a + b + c
        \\            f(10)
        \\        }
        \\        r2
        \\    }
        \\    r1
        \\}
    ;
    try runExpectI64(code, 16, .no_trace);
}

test "closure: same variable captured by multiple independent closures" {
    const code =
        \\{
        \\    shared = 10
        \\    f = |x| x + shared
        \\    g = |x| x * shared
        \\    f(5) + g(3)
        \\}
    ;
    try runExpectI64(code, 45, .no_trace);
}

test "closure: closure returning a string that includes a captured string" {
    const code =
        \\{
        \\    make_greeter = |greeting|
        \\        |name|
        \\            Str.concat(Str.concat(greeting, ", "), name)
        \\    hello = make_greeter("Hello")
        \\    hi = make_greeter("Hi")
        \\    r1 = hello("Alice")
        \\    r2 = hi("Bob")
        \\    Str.concat(Str.concat(r1, " and "), r2)
        \\}
    ;
    try runExpectStr(code, "Hello, Alice and Hi, Bob", .no_trace);
}

test "closure: applying the same closure to different arguments" {
    const code =
        \\{
        \\    base = 100
        \\    f = |x| x + base
        \\    a = f(1)
        \\    b = f(2)
        \\    c = f(3)
        \\    a + b + c
        \\}
    ;
    try runExpectI64(code, 306, .no_trace);
}

test "closure: immediately invoked closure with capture" {
    const code =
        \\{
        \\    y = 42
        \\    (|x| x + y)(8)
        \\}
    ;
    try runExpectI64(code, 50, .no_trace);
}

test "closure: closure that ignores its argument but uses capture" {
    const code =
        \\{
        \\    val = 99
        \\    f = |_| val
        \\    f(0)
        \\}
    ;
    try runExpectI64(code, 99, .no_trace);
}

test "closure: closure that ignores capture and uses argument" {
    const code =
        \\{
        \\    _unused = 999
        \\    f = |x| x + 1
        \\    f(41)
        \\}
    ;
    try runExpectI64(code, 42, .no_trace);
}

// =============================================================================
// TIER 11: Monomorphic identity — isolating polymorphic specialization
// =============================================================================

test "closure: monomorphic Str identity (no polymorphism)" {
    // Same as the failing "polymorphic identity function" test but with
    // identity annotated as Str -> Str, so no specialization is needed.
    const code =
        \\{
        \\    identity : Str -> Str
        \\    identity = |val| val
        \\    identity("Hello")
        \\}
    ;
    try runExpectStr(code, "Hello", .no_trace);
}

test "closure: monomorphic Dec identity (no polymorphism)" {
    const code =
        \\{
        \\    identity : Dec -> Dec
        \\    identity = |val| val
        \\    num = identity(5)
        \\    num
        \\}
    ;
    try runExpectI64(code, 5, .no_trace);
}

test "closure: monomorphic Str identity with if-else (exact failing scenario but monomorphic)" {
    // Exact structure of the failing test, but identity is annotated Str -> Str
    // and we use a separate Dec function for the number
    const code =
        \\{
        \\    str_id : Str -> Str
        \\    str_id = |val| val
        \\    num = 5
        \\    str = str_id("Hello")
        \\    if (num > 0) str else ""
        \\}
    ;
    try runExpectStr(code, "Hello", .no_trace);
}
