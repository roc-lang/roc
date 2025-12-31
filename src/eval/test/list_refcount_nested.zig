//! List refcounting tests - Phase 9: Nested Lists
//!
//! Lists within lists create recursive refcounting.
//!
//! This tests the most complex refcounting scenario:
//! - Outer list container refcount
//! - Inner list elements refcount (each inner list is refcounted)
//! - Potential string elements in inner lists (third level!)
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const helpers = @import("helpers.zig");

const runExpectI64 = helpers.runExpectI64;
const runExpectStr = helpers.runExpectStr;

test "list refcount nested - simple nested list" {
    // Inner list refcount should increment when added to outer
    try runExpectI64(
        \\{
        \\    inner = [1, 2]
        \\    outer = [inner]
        \\    match outer { [lst] => match lst { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount nested - multiple inner lists" {
    try runExpectI64(
        \\{
        \\    a = [1, 2]
        \\    b = [3, 4]
        \\    outer = [a, b]
        \\    match outer { [first, ..] => match first { [x, y] => x + y, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount nested - same inner list multiple times" {
    try runExpectI64(
        \\{
        \\    inner = [1, 2]
        \\    outer = [inner, inner, inner]
        \\    match outer { [first, ..] => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount nested - two levels inline" {
    try runExpectI64(
        \\match [[1, 2], [3, 4]] { [first, ..] => match first { [a, b] => a + b, _ => 0 }, _ => 0 }
    , 3, .no_trace);
}

test "list refcount nested - three levels" {
    try runExpectI64(
        \\{
        \\    a = [1]
        \\    b = [a]
        \\    c = [b]
        \\    match c { [lst] => match lst { [lst2] => match lst2 { [x] => x, _ => 0 }, _ => 0 }, _ => 0 }
        \\}
    , 1, .no_trace);
}

test "list refcount nested - empty inner list" {
    try runExpectI64(
        \\{
        \\    inner = []
        \\    outer = [inner]
        \\    match outer { [lst] => match lst { [] => 42, _ => 0 }, _ => 0 }
        \\}
    , 42, .no_trace);
}

test "list refcount nested - list of string lists" {
    try runExpectStr(
        \\{
        \\    a = ["x", "y"]
        \\    b = ["z"]
        \\    outer = [a, b]
        \\    match outer { [first, ..] => match first { [s, ..] => s, _ => "" }, _ => "" }
        \\}
    , "x", .no_trace);
}

test "list refcount nested - inline string lists" {
    try runExpectStr(
        \\match [["a", "b"], ["c"]] { [first, ..] => match first { [s, ..] => s, _ => "" }, _ => "" }
    , "a", .no_trace);
}

test "list refcount nested - nested then aliased" {
    try runExpectI64(
        \\{
        \\    inner = [1, 2]
        \\    outer = [inner]
        \\    outer2 = outer
        \\    match outer2 { [lst] => match lst { [a, b] => a + b, _ => 0 }, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount nested - access second inner list" {
    try runExpectI64(
        \\{
        \\    a = [1, 2]
        \\    b = [3, 4]
        \\    outer = [a, b]
        \\    match outer { [_, second] => match second { [x, y] => x + y, _ => 0 }, _ => 0 }
        \\}
    , 7, .no_trace);
}

test "list refcount nested - deeply nested inline" {
    try runExpectI64(
        \\match [[[1]]] { [lst] => match lst { [lst2] => match lst2 { [x] => x, _ => 0 }, _ => 0 }, _ => 0 }
    , 1, .no_trace);
}

test "list refcount nested - mixed nested and flat" {
    try runExpectI64(
        \\match [[1, 2], [3]] { [first, second] => {
        \\    a = match first { [x, ..] => x, _ => 0 }
        \\    b = match second { [y] => y, _ => 0 }
        \\    a + b
        \\}, _ => 0 }
    , 4, .no_trace);
}
