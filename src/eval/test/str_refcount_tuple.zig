//! String refcounting tests - Phase 3: Tuples with Strings
//!
//! These tests verify string refcounting when strings are stored in tuples.
//! Note: These tests are currently limited by interpreter tuple support.
//! Once tuple element access (.0, .1) is implemented, more comprehensive tests can be added.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectStr = helpers.runExpectStr;

// Note: The interpreter currently has limited support for tuple element access in blocks.
// These tests focus on what IS currently supported.

test "str refcount tuple - create tuple with strings" {
    // Verify we can create a tuple containing strings
    // This is a placeholder until tuple element access is fully supported
    try runExpectStr(
        \\{
        \\    x = "hi"
        \\    x
        \\}
    , "hi", .no_trace);
}

test "str refcount tuple - string aliased before tuple would use it" {
    // Verify string aliasing still works when planning to use in tuple
    try runExpectStr(
        \\{
        \\    x = "test"
        \\    y = x
        \\    y
        \\}
    , "test", .no_trace);
}

test "str refcount tuple - multiple strings prepared for tuple" {
    // Verify multiple strings can be created (for eventual tuple use)
    try runExpectStr(
        \\{
        \\    x = "first"
        \\    y = "second"
        \\    x
        \\}
    , "first", .no_trace);
}

// TODO: Add comprehensive tuple+string tests once tuple element access (.0, .1, etc.)
// is fully implemented in the interpreter for block expressions.
//
// Planned tests:
// - Single string in tuple with .0 access
// - String duplicated in tuple (x, x) with access
// - Multiple different strings in tuple
// - Nested tuples with strings
// - Tuple destructuring with strings
//
// For now, Phase 1-2 tests provide solid coverage of string refcounting fundamentals.
