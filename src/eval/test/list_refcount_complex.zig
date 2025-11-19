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

// ===== Lists of Records =====

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
        \\    r1 = {nums: [1, 2]}
        \\    r2 = {nums: [3, 4]}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => match first.nums { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

// ===== Lists of Tuples =====

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

// ===== Lists of Tags =====

test "list refcount complex - list of tags with integers" {
    try runExpectInt(
        \\{
        \\    t1 = Some(10)
        \\    t2 = Some(20)
        \\    lst = [t1, t2]
        \\    match lst { [first, ..] => match first { Some(x) => x, None => 0 }, _ => 0 }
        \\}
    , 10, .no_trace);
}

test "list refcount complex - list of tags with strings" {
    try runExpectStr(
        \\{
        \\    t1 = Some("hello")
        \\    t2 = Some("world")
        \\    lst = [t1, t2]
        \\    match lst { [first, ..] => match first { Some(s) => s, None => "" }, _ => "" }
        \\}
    , "hello", .no_trace);
}

// ===== Deep Nesting =====

test "list refcount complex - list of records of lists of strings" {
    try runExpectStr(
        \\{
        \\    r1 = {words: ["a", "b"]}
        \\    r2 = {words: ["c"]}
        \\    lst = [r1, r2]
        \\    match lst { [first, ..] => match first.words { [s, ..] => s, _ => "" }, _ => "" }
        \\}
    , "a", .no_trace);
}

test "list refcount complex - inline complex structure" {
    try runExpectInt(
        \\match [{val: [1, 2]}, {val: [3, 4]}] {
        \\    [first, ..] => match first.val { [a, b] => a + b, _ => 0 },
        \\    _ => 0
        \\}
    , 3, .no_trace);
}

test "list refcount complex - deeply nested mixed structures" {
    try runExpectStr(
        \\{
        \\    inner = ["x"]
        \\    rec = {data: inner}
        \\    lst = [rec, rec]
        \\    match lst { [first, ..] => match first.data { [s] => s, _ => "" }, _ => "" }
        \\}
    , "x", .no_trace);
}

test "list refcount complex - list of Ok/Err tags" {
    try runExpectInt(
        \\{
        \\    t1 = Ok(1)
        \\    t2 = Ok(2)
        \\    t3 = Err(0)
        \\    lst = [t1, t2, t3]
        \\    match lst { [first, ..] => match first { Ok(x) => x, Err(_) => 0 }, _ => 0 }
        \\}
    , 1, .no_trace);
}
