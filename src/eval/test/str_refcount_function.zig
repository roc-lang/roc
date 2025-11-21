//! String refcounting tests - Phase 7: Function Calls with Strings
//!
//! These tests verify string refcounting when strings are passed to/returned from functions.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectStr = helpers.runExpectStr;
const runExpectInt = helpers.runExpectInt;

// Basic function calls

test "str refcount function - identity function inline" {
    // Verify passing string to identity function
    try runExpectStr(
        \\(|s| s)("hello")
    , "hello", .no_trace);
}

test "str refcount function - identity function with variable" {
    // Verify passing string variable to function
    try runExpectStr(
        \\{
        \\    x = "test"
        \\    f = |s| s
        \\    f(x)
        \\}
    , "test", .no_trace);
}

test "str refcount function - function returns string literal" {
    // Verify function returning string literal (with dummy parameter)
    try runExpectStr(
        \\{
        \\    f = |_| "returned"
        \\    f(42)
        \\}
    , "returned", .no_trace);
}

test "str refcount function - function returns parameter" {
    // Verify function returns parameter correctly
    try runExpectStr(
        \\{
        \\    x = "param"
        \\    f = |s| s
        \\    result = f(x)
        \\    result
        \\}
    , "param", .no_trace);
}

// Multiple parameters

test "str refcount function - two string parameters return first" {
    // Verify function with two string params
    try runExpectStr(
        \\{
        \\    x = "first"
        \\    y = "second"
        \\    f = |a, b| a
        \\    f(x, y)
        \\}
    , "first", .no_trace);
}

test "str refcount function - two string parameters return second" {
    // Verify returning second parameter
    try runExpectStr(
        \\{
        \\    x = "first"
        \\    y = "second"
        \\    f = |a, b| b
        \\    f(x, y)
        \\}
    , "second", .no_trace);
}

test "str refcount function - same string passed twice" {
    // Verify same string as both parameters
    try runExpectStr(
        \\{
        \\    x = "same"
        \\    f = |a, b| a
        \\    f(x, x)
        \\}
    , "same", .no_trace);
}

// Closures (capture)

test "str refcount function - closure captures string" {
    // Verify closure capturing string from outer scope (with dummy parameter)
    try runExpectStr(
        \\{
        \\    x = "captured"
        \\    f = |_| x
        \\    f(0)
        \\}
    , "captured", .no_trace);
}

test "str refcount function - closure captures and uses parameter" {
    // Verify closure captures outer string, uses parameter
    try runExpectStr(
        \\{
        \\    x = "outer"
        \\    f = |y| y
        \\    f("inner")
        \\}
    , "inner", .no_trace);
}

test "str refcount function - closure captures string used with parameter" {
    // Verify closure uses both captured and parameter (returns captured)
    try runExpectStr(
        \\{
        \\    x = "captured"
        \\    f = |y| x
        \\    f("ignored")
        \\}
    , "captured", .no_trace);
}

test "str refcount function - multiple captures" {
    // Verify closure capturing multiple strings (with dummy parameter)
    try runExpectStr(
        \\{
        \\    x = "first"
        \\    y = "second"
        \\    f = |_| x
        \\    f(0)
        \\}
    , "first", .no_trace);
}

// Function called multiple times

test "str refcount function - call same function twice" {
    // Verify calling function multiple times
    try runExpectStr(
        \\{
        \\    f = |s| s
        \\    x = "test"
        \\    a = f(x)
        \\    b = f(x)
        \\    a
        \\}
    , "test", .no_trace);
}

test "str refcount function - call same function twice return second" {
    // Verify second call result
    try runExpectStr(
        \\{
        \\    f = |s| s
        \\    x = "test"
        \\    a = f(x)
        \\    b = f(x)
        \\    b
        \\}
    , "test", .no_trace);
}

test "str refcount function - closure called multiple times" {
    // Verify closure called multiple times (with dummy parameter)
    try runExpectStr(
        \\{
        \\    x = "captured"
        \\    f = |_| x
        \\    a = f(0)
        \\    b = f(1)
        \\    a
        \\}
    , "captured", .no_trace);
}

// Functions with conditionals

test "str refcount function - function with if-else" {
    // Verify function containing conditional
    try runExpectStr(
        \\{
        \\    f = |cond, a, b| if cond a else b
        \\    f(True, "then", "else")
        \\}
    , "then", .no_trace);
}

test "str refcount function - function with if-else returns else" {
    // Verify else branch in function
    try runExpectStr(
        \\{
        \\    f = |cond, a, b| if cond a else b
        \\    f(False, "then", "else")
        \\}
    , "else", .no_trace);
}

// Edge cases

test "str refcount function - empty string parameter" {
    // Verify empty string as parameter
    try runExpectStr(
        \\(|s| s)("")
    , "", .no_trace);
}

test "str refcount function - large string parameter" {
    // Verify large heap-allocated string as parameter
    const large = "This is a very long string that exceeds small string optimization";
    try runExpectStr(
        \\(|s| s)("This is a very long string that exceeds small string optimization")
    , large, .no_trace);
}

test "str refcount function - small string optimization" {
    // Verify small string (inline storage) as parameter
    try runExpectStr(
        \\(|s| s)("small")
    , "small", .no_trace);
}

test "str refcount function - function ignores string parameter" {
    // Verify unused parameter is properly decreffed
    try runExpectStr(
        \\{
        \\    f = |s| "returned"
        \\    f("ignored")
        \\}
    , "returned", .no_trace);
}

// Aliasing before function call

test "str refcount function - aliased string as parameter" {
    // Verify aliased string works as parameter
    try runExpectStr(
        \\{
        \\    x = "original"
        \\    y = x
        \\    f = |s| s
        \\    f(y)
        \\}
    , "original", .no_trace);
}

test "str refcount function - multiple aliases passed to function" {
    // Verify multiple aliases of same string as parameters
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    y = x
        \\    z = x
        \\    f = |a, b| a
        \\    f(y, z)
        \\}
    , "shared", .no_trace);
}

// Functions stored and called later

test "str refcount function - store function then call" {
    // Verify storing function in variable
    try runExpectStr(
        \\{
        \\    f = |s| s
        \\    g = f
        \\    g("test")
        \\}
    , "test", .no_trace);
}

test "str refcount function - closure stored then called" {
    // Verify storing closure (with dummy parameter)
    try runExpectStr(
        \\{
        \\    x = "captured"
        \\    f = |_| x
        \\    g = f
        \\    g(0)
        \\}
    , "captured", .no_trace);
}

// Complex patterns

test "str refcount function - function returns closure result" {
    // Verify function calling another function (with dummy parameters)
    try runExpectStr(
        \\{
        \\    x = "value"
        \\    inner = |_| x
        \\    outer = |_| inner(0)
        \\    outer(0)
        \\}
    , "value", .no_trace);
}

test "str refcount function - pass function result to another function" {
    // Verify passing one function's result to another (with dummy parameter)
    try runExpectStr(
        \\{
        \\    f = |_| "result"
        \\    g = |s| s
        \\    g(f(0))
        \\}
    , "result", .no_trace);
}
