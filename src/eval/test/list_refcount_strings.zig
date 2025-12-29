//! List refcounting tests - Phase 4: Lists with Refcounted Elements (Strings)
//!
//! This phase introduces two-level refcounting:
//! - List container must be refcounted
//! - String elements must be refcounted
//!
//! This is where list refcounting gets complex. We must verify:
//! 1. List container refcount is correct
//! 2. Each string element's refcount is incremented when added to list
//! 3. String element refcount is decremented when list is freed
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const helpers = @import("helpers.zig");

const runExpectStr = helpers.runExpectStr;

test "list refcount strings - single string in list" {
    // String refcount should increment when added to list
    try runExpectStr(
        \\{
        \\    x = "hi"
        \\    lst = [x]
        \\    match lst { [s] => s, _ => "" }
        \\}
    , "hi", .no_trace);
}

test "list refcount strings - multiple strings in list" {
    // Each string's refcount should increment
    try runExpectStr(
        \\{
        \\    x = "a"
        \\    y = "b"
        \\    lst = [x, y]
        \\    match lst { [first, ..] => first, _ => "" }
        \\}
    , "a", .no_trace);
}

test "list refcount strings - return second string" {
    try runExpectStr(
        \\{
        \\    x = "a"
        \\    y = "b"
        \\    lst = [x, y]
        \\    match lst { [_, second] => second, _ => "" }
        \\}
    , "b", .no_trace);
}

test "list refcount strings - same string multiple times" {
    // Same string in multiple list slots - refcount incremented per slot
    try runExpectStr(
        \\{
        \\    x = "hi"
        \\    lst = [x, x, x]
        \\    match lst { [first, ..] => first, _ => "" }
        \\}
    , "hi", .no_trace);
}

test "list refcount strings - empty string in list" {
    // Empty string edge case
    try runExpectStr(
        \\{
        \\    x = ""
        \\    lst = [x]
        \\    match lst { [s] => s, _ => "fallback" }
        \\}
    , "", .no_trace);
}

test "list refcount strings - small vs large strings in list" {
    // Mix of small (inline) and large (heap) strings
    try runExpectStr(
        \\{
        \\    small = "hi"
        \\    large = "This is a very long string that will be heap allocated for sure"
        \\    lst = [small, large]
        \\    match lst { [first, ..] => first, _ => "" }
        \\}
    , "hi", .no_trace);
}

test "list refcount strings - return large string" {
    try runExpectStr(
        \\{
        \\    small = "hi"
        \\    large = "This is a very long string that will be heap allocated for sure"
        \\    lst = [small, large]
        \\    match lst { [_, second] => second, _ => "" }
        \\}
    , "This is a very long string that will be heap allocated for sure", .no_trace);
}

test "list refcount strings - list of string literals" {
    // Direct string literals in list
    try runExpectStr(
        \\match ["a", "b", "c"] { [first, ..] => first, _ => "" }
    , "a", .no_trace);
}

test "list refcount strings - list of string literals return second" {
    try runExpectStr(
        \\match ["a", "b", "c"] { [_, second, ..] => second, _ => "" }
    , "b", .no_trace);
}

test "list refcount strings - empty list then string list" {
    // Multiple lists with different types
    try runExpectStr(
        \\{
        \\    empty = []
        \\    strings = ["x", "y"]
        \\    match strings { [first, ..] => first, _ => "" }
        \\}
    , "x", .no_trace);
}

test "list refcount strings - string list aliased" {
    // Alias a string list
    try runExpectStr(
        \\{
        \\    lst1 = ["a", "b"]
        \\    lst2 = lst1
        \\    match lst2 { [first, ..] => first, _ => "" }
        \\}
    , "a", .no_trace);
}

test "list refcount strings - string list aliased return from original" {
    try runExpectStr(
        \\{
        \\    lst1 = ["a", "b"]
        \\    lst2 = lst1
        \\    match lst1 { [first, ..] => first, _ => "" }
        \\}
    , "a", .no_trace);
}

test "list refcount strings - string list shadowed" {
    // Shadow a string list - old list and its strings should be decreffed
    try runExpectStr(
        \\{
        \\    lst = ["old1", "old2"]
        \\    lst = ["new1", "new2"]
        \\    match lst { [first, ..] => first, _ => "" }
        \\}
    , "new1", .no_trace);
}

test "list refcount strings - three string lists" {
    try runExpectStr(
        \\{
        \\    a = ["a1", "a2"]
        \\    b = ["b1", "b2"]
        \\    c = ["c1", "c2"]
        \\    match b { [first, ..] => first, _ => "" }
        \\}
    , "b1", .no_trace);
}

test "list refcount strings - extract string from nested match" {
    try runExpectStr(
        \\{
        \\    lst = ["x", "y", "z"]
        \\    match lst {
        \\        [first, .. as rest] => match rest {
        \\            [second, ..] => second,
        \\            _ => ""
        \\        },
        \\        _ => ""
        \\    }
        \\}
    , "y", .no_trace);
}
