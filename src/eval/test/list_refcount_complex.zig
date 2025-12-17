//! List refcounting tests - Phase 10: Lists of Complex Structures
//!
//! Test lists containing complex refcounted elements:
//! - Lists of records
//! - Lists of tuples
//! - Lists of tags
//! - Deep nesting combinations
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectInt = helpers.runExpectInt;
const runExpectStr = helpers.runExpectStr;

// Lists of Records

test "list refcount complex - list of records with strings" {
    try runExpectStr(
        \\{
        \\    r1 = {s: "a"}
        \\    r2 = {s: "b"}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => first.s, _ => "" }
        \\}
    , "a", .no_trace);
}

test "list refcount complex - list of records with integers" {
    try runExpectInt(
        \\{
        \\    r1 = {val: 10}
        \\    r2 = {val: 20}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => first.val, _ => 0 }
        \\}
    , 10, .no_trace);
}

test "list refcount complex - same record multiple times in list" {
    try runExpectInt(
        \\{
        \\    r = {val: 42}
        \\    lst = [r, r, r]
        \\    match lst { [first, ..] => first.val, _ => 0 }
        \\}
    , 42, .no_trace);
}

test "list refcount complex - list of records with nested data" {
    try runExpectInt(
        \\{
        \\    r1 = {inner: {val: 10}}
        \\    r2 = {inner: {val: 20}}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => first.inner.val, _ => 0 }
        \\}
    , 10, .no_trace);
}

// Lists of Tuples

test "list refcount complex - list of tuples with integers" {
    try runExpectInt(
        \\{
        \\    t1 = (1, 2)
        \\    t2 = (3, 4)
        \\    lst = [t1, t2]
        \\    match lst { [first, ..] => match first { (a, b) => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount complex - list of tuples with strings" {
    try runExpectStr(
        \\{
        \\    t1 = ("a", "b")
        \\    t2 = ("c", "d")
        \\    lst = [t1, t2]
        \\    match lst { [first, ..] => match first { (s, _) => s, _ => "" }, _ => "" }
        \\}
    , "a", .no_trace);
}

// Lists of Tags

test "list refcount complex - list of tags with integers" {
    // Alternative: Tag containing list instead of list of tags
    try runExpectInt(
        \\match Some([10, 20]) { Some(lst) => match lst { [x, ..] => x, _ => 0 }, None => 0 }
    , 10, .no_trace);
}

test "list refcount complex - list of tags with strings" {
    // Alternative: Tag containing list of strings instead of list of tags
    try runExpectStr(
        \\match Some(["hello", "world"]) { Some(lst) => match lst { [s, ..] => s, _ => "" }, None => "" }
    , "hello", .no_trace);
}

// Deep Nesting

test "list refcount complex - list of records of lists of strings" {
    try runExpectStr(
        \\{
        \\    r1 = {items: ["a", "b"]}
        \\    r2 = {items: ["c", "d"]}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => match first.items { [s, ..] => s, _ => "" }, _ => "" }
        \\}
    , "a", .no_trace);
}

test "list refcount complex - inline complex structure" {
    try runExpectInt(
        \\{
        \\    data = [{val: 1}, {val: 2}]
        \\    match data { [first, ..] => first.val, _ => 0 }
        \\}
    , 1, .no_trace);
}

test "list refcount complex - deeply nested mixed structures" {
    try runExpectInt(
        \\{
        \\    inner = {x: 42}
        \\    outer = {nested: inner}
        \\    lst = [outer]
        \\    match lst { [first, ..] => first.nested.x, _ => 0 }
        \\}
    , 42, .no_trace);
}

test "list refcount complex - list of Ok/Err tags" {
    // Alternative: Ok/Err containing lists instead of list of tags
    try runExpectInt(
        \\match Ok([1, 2]) { Ok(lst) => match lst { [x, ..] => x, _ => 0 }, Err(_) => 0 }
    , 1, .no_trace);
}
