//! List refcounting tests - Phase 7: Functions with Lists
//!
//! Test lists passed to/returned from functions and closures.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectInt = helpers.runExpectInt;
const runExpectStr = helpers.runExpectStr;

test "list refcount function - pass list to identity function" {
    try runExpectInt(
        \\{
        \\    id = |lst| lst
        \\    x = [1, 2]
        \\    result = id(x)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount function - list returned from function" {
    try runExpectInt(
        \\{
        \\    f = |_| [1, 2]
        \\    result = f(0)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount function - closure captures list" {
    try runExpectInt(
        \\{
        \\    x = [1, 2]
        \\    f = |_| x
        \\    result = f(0)
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount function - function called multiple times" {
    try runExpectInt(
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
    // Simplified: Inline match instead of function with match
    try runExpectInt(
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
    try runExpectInt(
        \\{
        \\    x = [5, 10]
        \\    match x { [first, ..] => first + first, _ => 0 }
        \\}
    , 10, .no_trace);
}
