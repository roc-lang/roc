//! String refcounting tests - Phase 5: Tag Unions with Strings
//!
//! These tests verify string refcounting when strings are stored in tag union payloads.
//! Note: These tests are currently limited by interpreter match expression support.
//! Match expressions with strings in blocks are not yet fully supported.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectStr = helpers.runExpectStr;
const runExpectInt = helpers.runExpectInt;

// Note: The interpreter currently has limited support for match expressions in blocks.
// These tests focus on what IS currently supported - inline match expressions.

// Basic tag construction and pattern matching

test "str refcount tags - simple tag with string payload" {
    // Verify we can create and match a tag with string payload
    try runExpectStr(
        \\match Ok("hello") { Ok(s) => s, Err(_) => "" }
    , "hello", .no_trace);
}

test "str refcount tags - inline string in tag" {
    // Verify inline string literal in tag
    try runExpectStr(
        \\match Ok("direct") { Ok(s) => s, Err(_) => "" }
    , "direct", .no_trace);
}

test "str refcount tags - Err tag with string" {
    // Verify Err tag with string payload
    try runExpectStr(
        \\match Err("error") { Ok(_) => "", Err(e) => e }
    , "error", .no_trace);
}

test "str refcount tags - empty string in tag" {
    // Verify empty string in tag
    try runExpectStr(
        \\match Ok("") { Ok(s) => s, Err(_) => "fallback" }
    , "", .no_trace);
}

test "str refcount tags - large string in tag" {
    // Verify large heap-allocated string in tag
    const large = "This is a very long string that exceeds small string optimization";
    try runExpectStr(
        \\match Ok("This is a very long string that exceeds small string optimization") { Ok(s) => s, Err(_) => "" }
    , large, .no_trace);
}

test "str refcount tags - small string optimization in tag" {
    // Verify small string (inline storage) in tag
    try runExpectStr(
        \\match Err("small") { Ok(_) => "", Err(e) => e }
    , "small", .no_trace);
}

test "str refcount tags - match returns from Err branch" {
    // Verify Err branch string is returned correctly
    try runExpectStr(
        \\match Err("boom") { Ok(_) => "ok", Err(e) => e }
    , "boom", .no_trace);
}

// Pattern matching with discard

test "str refcount tags - discard string payload" {
    // Verify discarded payload is properly decreffed
    try runExpectInt(
        \\match Ok("unused") { Ok(_) => 42, Err(_) => 0 }
    , 42, .no_trace);
}

test "str refcount tags - discard in Err branch" {
    // Verify Err branch wildcard
    try runExpectInt(
        \\match Err("ignored") { Ok(_) => 0, Err(_) => 99 }
    , 99, .no_trace);
}

// TODO: Add comprehensive tag+string tests once match expressions are fully
// supported in block expressions.
//
// Planned tests:
// - Tag construction in blocks with string variables
// - String aliasing before tag construction
// - Multiple strings in tag payloads (tuples, records)
// - Same string in multiple tag instances
// - Nested tag structures
// - Partial destructuring with discard
// - Complex sharing patterns
//
// For now, Phase 1-4 tests provide solid coverage of string refcounting fundamentals.
