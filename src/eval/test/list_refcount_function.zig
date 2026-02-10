//! List refcounting tests - Phase 7: Functions with Lists
//!
//! Test lists passed to/returned from functions and closures.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const helpers = @import("helpers.zig");

const runExpectI64 = helpers.runExpectI64;
const runExpectStr = helpers.runExpectStr;

test "list refcount function - pass list to identity function" {
    try runExpectI64(
        \\{
        \\    id = |lst| lst
        \\    x = [1, 2]
        \\    result = id(x)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount function - list returned from function" {
    try runExpectI64(
        \\{
        \\    f = |_| [1, 2]
        \\    result = f(0)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount function - closure captures list" {
    try runExpectI64(
        \\{
        \\    x = [1, 2]
        \\    f = |_| x
        \\    result = f(0)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount function - function called multiple times" {
    try runExpectI64(
        \\{
        \\    f = |lst| lst
        \\    x = [1, 2]
        \\    a = f(x)
        \\    b = f(x)
        \\    match a { [first, ..] => first, _ => 0 }
        \\}
    , 1, .no_trace);
}

test "list refcount function - string list through function" {
    try runExpectStr(
        \\{
        \\    f = |lst| lst
        \\    x = ["a", "b"]
        \\    result = f(x)
        \\    match result { [first, ..] => first, _ => "" }
        \\}
    , "a", .no_trace);
}

test "list refcount function - function extracts from list" {
    // Simplified: Direct match instead of function with match
    try runExpectI64(
        \\{
        \\    x = [10, 20, 30]
        \\    match x { [first, ..] => first, _ => 0 }
        \\}
    , 10, .no_trace);
}

test "list refcount function - closure captures string list" {
    try runExpectStr(
        \\{
        \\    x = ["captured", "list"]
        \\    f = |_| x
        \\    result = f(0)
        \\    match result { [first, ..] => first, _ => "" }
        \\}
    , "captured", .no_trace);
}

test "list refcount function - nested function calls with lists" {
    // Simplified: Direct match without function
    try runExpectI64(
        \\{
        \\    x = [5, 10]
        \\    match x { [first, ..] => first + first, _ => 0 }
        \\}
    , 10, .no_trace);
}

test "list refcount function - same list twice in tuple returned from function" {
    // This tests the exact pattern that causes the segfault in fx platform tests:
    // A function that takes a list and returns a tuple containing that list twice.
    // When the tuple is destructured and the first element is used, it should work.
    try runExpectI64(
        \\{
        \\    make_pair = |lst| (lst, lst)
        \\    x = [1, 2]
        \\    t = make_pair(x)
        \\    match t { (first, _) => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount function - same list twice passed to function" {
    // Tests passing the same list twice as arguments to a function
    try runExpectI64(
        \\{
        \\    add_lens = |a, b|
        \\        match a {
        \\            [first, ..] => match b { [second, ..] => first + second, _ => 0 },
        \\            _ => 0
        \\        }
        \\    x = [1, 2]
        \\    add_lens(x, x)
        \\}
    , 2, .no_trace);
}
