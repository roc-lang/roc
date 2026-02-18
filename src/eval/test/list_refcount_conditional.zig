//! List refcounting tests - Phase 6: Conditionals with Lists
//!
//! Test lists in if-else and conditional expressions.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const helpers = @import("helpers.zig");

const runExpectI64 = helpers.runExpectI64;
const runExpectStr = helpers.runExpectStr;

test "list refcount conditional - simple if-else with lists" {
    try runExpectI64(
        \\{
        \\    x = [1, 2]
        \\    result = if True {x} else {[3, 4]}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount conditional - return else branch" {
    try runExpectI64(
        \\{
        \\    x = [1, 2]
        \\    result = if False {x} else {[3, 4]}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
    , 7, .no_trace);
}

test "list refcount conditional - same list in both branches" {
    try runExpectI64(
        \\{
        \\    x = [1, 2]
        \\    result = if True {x} else {x}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount conditional - unused branch decreffed" {
    try runExpectI64(
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    result = if True {x} else {y}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount conditional - nested conditionals" {
    try runExpectI64(
        \\{
        \\    x = [1]
        \\    result = if True {if False {x} else {[2]}} else {[3]}
        \\    match result { [a] => a, _ => 0 }
        \\}
    , 2, .no_trace);
}

test "list refcount conditional - string lists in conditionals" {
    try runExpectStr(
        \\{
        \\    x = ["a", "b"]
        \\    result = if True {x} else {["c"]}
        \\    match result { [first, ..] => first, _ => "" }
        \\}
    , "a", .no_trace);
}

test "list refcount conditional - inline list literals" {
    try runExpectI64(
        \\{
        \\    result = if True {[10, 20]} else {[30, 40]}
        \\    match result { [a, b] => a + b, _ => 0 }
        \\}
    , 30, .no_trace);
}

test "list refcount conditional - empty list in branch" {
    try runExpectI64(
        \\{
        \\    result = if True {[]} else {[1, 2]}
        \\    match result { [] => 42, _ => 0 }
        \\}
    , 42, .no_trace);
}
