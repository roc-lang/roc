//! Minimal reproduction for list-of-tags bug
//!
//! This file demonstrates a bug where lists containing tag values:
//! 1. Fail to pattern match correctly (return fallback value)
//! 2. Leak memory (list allocation not cleaned up)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectInt = helpers.runExpectInt;
const runExpectStr = helpers.runExpectStr;

// ===== FAILING TESTS (demonstrating the bug) =====

test "BUG - list of Some tags with integers" {
    // This should return 10 but returns 0 (fallback)
    // Also leaks memory
    try runExpectInt(
        \\match [Some(10), Some(20)] {
        \\    [first, ..] => match first {
        \\        Some(x) => x,
        \\        None => 0
        \\    },
        \\    _ => 0
        \\}
    , 10, .trace); // Enable trace to see what's happening
}

test "BUG - list of Some tags with strings" {
    // This should return "hello" but returns ""
    // Also leaks memory
    try runExpectStr(
        \\match [Some("hello"), Some("world")] {
        \\    [first, ..] => match first {
        \\        Some(s) => s,
        \\        None => ""
        \\    },
        \\    _ => ""
        \\}
    , "hello", .no_trace);
}

test "BUG - list of Ok/Err tags" {
    // This should return 1 but returns 0
    // Also leaks memory
    try runExpectInt(
        \\match [Ok(1), Ok(2)] {
        \\    [first, ..] => match first {
        \\        Ok(x) => x,
        \\        Err(_) => 0
        \\    },
        \\    _ => 0
        \\}
    , 1, .no_trace);
}

// ===== WORKING TESTS (for comparison) =====

test "WORKS - tag containing list of integers" {
    // Inverse pattern: tag contains list instead of list of tags
    try runExpectInt(
        \\match Some([10, 20]) {
        \\    Some(lst) => match lst {
        \\        [x, ..] => x,
        \\        _ => 0
        \\    },
        \\    None => 0
        \\}
    , 10, .no_trace);
}

test "WORKS - list of integers (no tags)" {
    // Lists of non-tag values work fine
    try runExpectInt(
        \\match [10, 20] {
        \\    [first, ..] => first,
        \\    _ => 0
        \\}
    , 10, .no_trace);
}

test "WORKS - list of records" {
    // Lists of records work fine (even though records are also refcounted)
    try runExpectInt(
        \\match [{val: 10}, {val: 20}] {
        \\    [first, ..] => first.val,
        \\    _ => 0
        \\}
    , 10, .no_trace);
}

test "WORKS - list of strings" {
    // Lists of strings work fine (strings are refcounted)
    try runExpectStr(
        \\match ["a", "b"] {
        \\    [first, ..] => first,
        \\    _ => ""
        \\}
    , "a", .no_trace);
}
