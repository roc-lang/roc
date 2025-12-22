//! List refcounting tests - Phase 8: Pattern Matching with Lists
//!
//! Test lists in pattern matching/destructuring contexts.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectI64 = helpers.runExpectI64;
const runExpectStr = helpers.runExpectStr;

test "list refcount pattern - destructure list from record" {
    try runExpectI64(
        \\{
        \\    r = {lst: [1, 2]}
        \\    match r { {lst} => match lst { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount pattern - wildcard discards list" {
    try runExpectI64(
        \\{
        \\    pair = {a: [1, 2], b: [3, 4]}
        \\    match pair { {a, b: _} => match a { [x, y] => x + y, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount pattern - list rest pattern" {
    try runExpectI64(
        \\match [1, 2, 3, 4] { [first, .. as rest] => match rest { [second, ..] => first + second, _ => 0 }, _ => 0 }
    , 3, .no_trace);
}

test "list refcount pattern - string list rest pattern" {
    try runExpectStr(
        \\match ["a", "b", "c"] { [first, .. as rest] => match rest { [second, ..] => second, _ => "" }, _ => "" }
    , "b", .no_trace);
}

test "list refcount pattern - nested list patterns" {
    try runExpectI64(
        \\{
        \\    data = {values: [10, 20, 30]}
        \\    match data { {values} => match values { [a, b, c] => a + b + c, _ => 0 }, _ => 0 }
        \\}
    , 60, .no_trace);
}

test "list refcount pattern - tag with list extracted" {
    try runExpectI64(
        \\match Some([5, 10]) { Some(lst) => match lst { [a, b] => a + b, _ => 0 }, None => 0 }
    , 15, .no_trace);
}

test "list refcount pattern - empty list pattern" {
    try runExpectI64(
        \\match {lst: []} { {lst} => match lst { [] => 42, _ => 0 }, _ => 0 }
    , 42, .no_trace);
}
