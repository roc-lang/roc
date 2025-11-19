//! Minimal test to reproduce the "list of tags" bug
//!
//! Bug: Lists containing tag values fail to pattern match correctly in the interpreter and leak memory.
//! Issue: When extracting a tag from a list, the tag's layout information appears to be lost,
//!        causing pattern matching to fail and returning fallback values.

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectInt = helpers.runExpectInt;
const runExpectStr = helpers.runExpectStr;

// This is the minimal reproduction from the bug document
test "list of tags - minimal repro" {
    try runExpectInt(
        \\match [Some(10), Some(20)] {
        \\    [first, ..] => match first {
        \\        Some(x) => x,
        \\        None => 0
        \\    },
        \\    _ => 0
        \\}
    , 10, .no_trace);
}

// Even simpler: just a single tag in a list
test "list of tags - single element" {
    try runExpectInt(
        \\match [Some(42)] {
        \\    [x] => match x {
        \\        Some(n) => n,
        \\        None => 0
        \\    },
        \\    _ => 0
        \\}
    , 42, .no_trace);
}

// Test with variables to see if that makes a difference
test "list of tags - with variables" {
    try runExpectInt(
        \\{
        \\    t1 = Some(10)
        \\    t2 = Some(20)
        \\    lst = [t1, t2]
        \\    match lst {
        \\        [first, ..] => match first {
        \\            Some(x) => x,
        \\            None => 0
        \\        },
        \\        _ => 0
        \\    }
        \\}
    , 10, .no_trace);
}

// Confirm that tags containing lists DO work (from the bug doc)
test "tag containing list - should work" {
    try runExpectInt(
        \\match Some([10, 20]) {
        \\    Some(lst) => match lst { [x, ..] => x, _ => 0 },
        \\    None => 0
        \\}
    , 10, .no_trace);
}

// Confirm that lists of integers DO work
test "list of integers - should work" {
    try runExpectInt(
        \\match [10, 20] {
        \\    [first, ..] => first,
        \\    _ => 0
        \\}
    , 10, .no_trace);
}

// Confirm that lists of records DO work
test "list of records - should work" {
    try runExpectInt(
        \\match [{val: 10}, {val: 20}] {
        \\    [first, ..] => first.val,
        \\    _ => 0
        \\}
    , 10, .no_trace);
}
