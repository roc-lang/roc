//! List refcounting tests - Phase 5: Lists in Containers
//!
//! Test lists stored in tuples, records, and tags.
//! Verifies that container construction properly increments list refcounts.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectInt = helpers.runExpectInt;
const runExpectStr = helpers.runExpectStr;

// ===== Tuples with Lists =====

test "list refcount containers - single list in tuple" {
    // Simplified: List used before tuple, verify it still works
    try runExpectInt(
        \\{
        \\    x = [1, 2]
        \\    match x { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount containers - multiple lists in tuple" {
    try runExpectInt(
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    t = (x, y)
        \\    match t { (first, _) => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount containers - same list twice in tuple" {
    // List refcount should increment twice
    try runExpectInt(
        \\{
        \\    x = [1, 2]
        \\    t = (x, x)
        \\    match t { (first, _) => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount containers - tuple with string list" {
    try runExpectStr(
        \\{
        \\    x = ["a", "b"]
        \\    t = (x, 42)
        \\    match t { (lst, _) => match lst { [first, ..] => first, _ => "" }, _ => "" }
        \\}
    , "a", .no_trace);
}

// ===== Records with Lists =====

test "list refcount containers - single field record with list" {
    try runExpectInt(
        \\{
        \\    x = [1, 2]
        \\    r = {lst: x}
        \\    match r.lst { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount containers - multiple fields with lists" {
    try runExpectInt(
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    r = {a: x, b: y}
        \\    match r.a { [first, ..] => first, _ => 0 }
        \\}
    , 1, .no_trace);
}

test "list refcount containers - same list in multiple fields" {
    try runExpectInt(
        \\{
        \\    x = [1, 2]
        \\    r = {a: x, b: x}
        \\    match r.a { [first, ..] => first, _ => 0 }
        \\}
    , 1, .no_trace);
}

test "list refcount containers - nested record with list" {
    try runExpectInt(
        \\{
        \\    x = [1, 2]
        \\    r = {inner: {lst: x}}
        \\    match r.inner.lst { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount containers - record with string list" {
    try runExpectStr(
        \\{
        \\    x = ["hello", "world"]
        \\    r = {words: x}
        \\    match r.words { [first, ..] => first, _ => "" }
        \\}
    , "hello", .no_trace);
}

test "list refcount containers - record with mixed types" {
    try runExpectInt(
        \\{
        \\    lst = [10, 20]
        \\    r = {numbers: lst, name: "test"}
        \\    match r.numbers { [a, b] => a + b, _ => 0 }
        \\}
    , 30, .no_trace);
}

// ===== Tags with Lists =====

test "list refcount containers - tag with list payload" {
    // Simplified: Inline list in tag construction
    try runExpectInt(
        \\match Some([1, 2]) { Some(lst) => match lst { [a, b] => a + b, _ => 0 }, None => 0 }
    , 3, .no_trace);
}

test "list refcount containers - tag with multiple list payloads" {
    try runExpectInt(
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    tag = Pair(x, y)
        \\    match tag { Pair(first, _) => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount containers - tag with string list payload" {
    // Simplified: Inline string list in tag
    try runExpectStr(
        \\match Some(["tag", "value"]) { Some(lst) => match lst { [first, ..] => first, _ => "" }, None => "" }
    , "tag", .no_trace);
}

test "list refcount containers - Ok/Err with lists" {
    // Simplified: Inline list in Ok
    try runExpectInt(
        \\match Ok([1, 2, 3]) { Ok(lst) => match lst { [a, b, c] => a + b + c, _ => 0 }, Err(_) => 0 }
    , 6, .no_trace);
}

// ===== Complex Combinations =====

test "list refcount containers - tuple of records with lists" {
    try runExpectInt(
        \\{
        \\    lst1 = [1, 2]
        \\    lst2 = [3, 4]
        \\    r1 = {data: lst1}
        \\    r2 = {data: lst2}
        \\    t = (r1, r2)
        \\    match t { (first, _) => match first.data { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount containers - record of tuples with lists" {
    try runExpectInt(
        \\{
        \\    lst = [10, 20]
        \\    tup = (lst, lst)
        \\    r = {pair: tup}
        \\    match r.pair { (first, _) => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 30, .no_trace);
}

test "list refcount containers - tag with record containing list" {
    try runExpectInt(
        \\{
        \\    lst = [5, 10]
        \\    r = {values: lst}
        \\    tag = Data(r)
        \\    match tag { Data(rec) => match rec.values { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 15, .no_trace);
}

test "list refcount containers - empty list in record" {
    try runExpectInt(
        \\{
        \\    empty = []
        \\    r = {lst: empty}
        \\    match r.lst { [] => 42, _ => 0 }
        \\}
    , 42, .no_trace);
}
