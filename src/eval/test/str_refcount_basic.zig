//! Basic string refcounting tests - Phase 1
//!
//! These tests verify the most fundamental string operations:
//! - Simple assignment and lookup
//! - Small vs large string handling
//! - Empty strings
//! - Multiple non-interfering bindings
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectStr = helpers.runExpectStr;

test "str refcount basic - simple assignment and return" {
    // Most basic test: assign a string to a variable and return it
    try runExpectStr(
        \\{
        \\    x = "hi"
        \\    x
        \\}
    , "hi", .no_trace);
}

test "str refcount basic - small string assignment" {
    // Test small string optimization (< 24 bytes fits inline)
    try runExpectStr(
        \\{
        \\    x = "small"
        \\    x
        \\}
    , "small", .no_trace);
}

test "str refcount basic - large string assignment" {
    // Test heap-allocated strings (>= 24 bytes requires allocation)
    const large_str = "This is a large string that exceeds small string optimization";
    try runExpectStr(
        \\{
        \\    x = "This is a large string that exceeds small string optimization"
        \\    x
        \\}
    , large_str, .no_trace);
}

test "str refcount basic - empty string assignment" {
    // Test empty string edge case
    try runExpectStr(
        \\{
        \\    x = ""
        \\    x
        \\}
    , "", .no_trace);
}

test "str refcount basic - multiple non-interfering bindings" {
    // Test that multiple string variables don't interfere with each other
    try runExpectStr(
        \\{
        \\    x = "first"
        \\    y = "second"
        \\    x
        \\}
    , "first", .no_trace);
}

test "str refcount basic - return second of two bindings" {
    // Similar to above but return the second binding
    try runExpectStr(
        \\{
        \\    x = "first"
        \\    y = "second"
        \\    y
        \\}
    , "second", .no_trace);
}

test "str refcount basic - three independent bindings" {
    // Test with three independent strings to ensure no interference
    try runExpectStr(
        \\{
        \\    a = "one"
        \\    b = "two"
        \\    c = "three"
        \\    b
        \\}
    , "two", .no_trace);
}

test "str refcount basic - mix of small and large strings" {
    // Test mixed string sizes in same scope
    try runExpectStr(
        \\{
        \\    small = "hi"
        \\    large = "This is a very long string that will be heap allocated for sure"
        \\    small
        \\}
    , "hi", .no_trace);
}

test "str refcount basic - return large from mixed sizes" {
    // Return the large string from mixed sizes
    const large_str = "This is a very long string that will be heap allocated for sure";
    try runExpectStr(
        \\{
        \\    small = "hi"
        \\    large = "This is a very long string that will be heap allocated for sure"
        \\    large
        \\}
    , large_str, .no_trace);
}
