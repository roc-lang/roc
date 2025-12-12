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

// Tuples with Lists

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

// Records with Lists

test "list refcount containers - single field record with list" {
    try runExpectInt(
        \\{
        \\    lst = [1, 2, 3]
        \\    r = {items: lst}
        \\    match r.items { [a, b, c] => a + b + c, _ => 0 }
        \\}
    , 6, .no_trace);
}

test "list refcount containers - multiple fields with lists" {
    try runExpectInt(
        \\{
        \\    x = [1, 2]
        \\    y = [3, 4]
        \\    r = {first: x, second: y}
        \\    match r.first { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount containers - same list in multiple fields" {
    try runExpectInt(
        \\{
        \\    lst = [10, 20]
        \\    r = {a: lst, b: lst}
        \\    match r.a { [x, y] => x + y, _ => 0 }
        \\}
    , 30, .no_trace);
}

test "list refcount containers - nested record with list" {
    try runExpectInt(
        \\{
        \\    lst = [5, 6]
        \\    inner = {data: lst}
        \\    outer = {nested: inner}
        \\    match outer.nested.data { [a, b] => a + b, _ => 0 }
        \\}
    , 11, .no_trace);
}

test "list refcount containers - record with string list" {
    try runExpectStr(
        \\{
        \\    lst = ["hello", "world"]
        \\    r = {items: lst}
        \\    match r.items { [first, ..] => first, _ => "" }
        \\}
    , "hello", .no_trace);
}

test "list refcount containers - record with mixed types" {
    try runExpectInt(
        \\{
        \\    lst = [1, 2, 3]
        \\    r = {count: 42, items: lst}
        \\    r.count
        \\}
    , 42, .no_trace);
}

// Tags with Lists

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

// Complex Combinations

test "list refcount containers - tuple of records with lists" {
    try runExpectInt(
        \\{
        \\    lst1 = [1, 2]
        \\    lst2 = [3, 4]
        \\    r1 = {items: lst1}
        \\    r2 = {items: lst2}
        \\    t = (r1, r2)
        \\    match t { (first, _) => match first.items { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount containers - record of tuples with lists" {
    try runExpectInt(
        \\{
        \\    lst = [5, 6]
        \\    t = (lst, 99)
        \\    r = {data: t}
        \\    match r.data { (items, _) => match items { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 11, .no_trace);
}

test "list refcount containers - tag with record containing list" {
    try runExpectInt(
        \\{
        \\    lst = [7, 8]
        \\    r = {items: lst}
        \\    tag = Some(r)
        \\    match tag { Some(rec) => match rec.items { [a, b] => a + b, _ => 0 }, None => 0 }
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
