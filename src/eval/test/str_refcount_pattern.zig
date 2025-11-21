//! String refcounting tests - Phase 8: Pattern Matching with Strings
//!
//! These tests verify string refcounting when strings are destructured via pattern matching.
//! Note: Standalone pattern bindings (e.g., `(a, b) = (x, y)`) are not yet supported.
//! These tests use match expressions for destructuring.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectStr = helpers.runExpectStr;

// Note: The interpreter currently supports pattern matching primarily through match expressions.
// Standalone destructuring patterns in let bindings are not yet fully supported.

// Tuple destructuring via match

test "str refcount pattern - tuple destructure extract first" {
    // Verify tuple destructuring extracts string correctly
    try runExpectStr(
        \\match ("first", "second") { (a, b) => a, _ => "" }
    , "first", .no_trace);
}

test "str refcount pattern - tuple destructure extract second" {
    // Verify extracting second element
    try runExpectStr(
        \\match ("first", "second") { (a, b) => b, _ => "" }
    , "second", .no_trace);
}

test "str refcount pattern - tuple destructure with wildcard" {
    // Verify wildcard properly decrefs unused element
    try runExpectStr(
        \\match ("used", "unused") { (a, _) => a, _ => "" }
    , "used", .no_trace);
}

test "str refcount pattern - tuple destructure both wildcards" {
    // Verify both elements decreffed when not used
    try runExpectStr(
        \\match ("unused1", "unused2") { (_, _) => "result", _ => "" }
    , "result", .no_trace);
}

test "str refcount pattern - tuple with same string twice" {
    // Verify destructuring tuple with duplicated string
    try runExpectStr(
        \\{
        \\    x = "dup"
        \\    t = (x, x)
        \\    match t { (a, b) => a, _ => "" }
        \\}
    , "dup", .no_trace);
}

// Nested tuple destructuring

test "str refcount pattern - nested tuple destructure" {
    // Verify nested tuple destructuring
    try runExpectStr(
        \\match (("inner", "other"), "outer") { ((a, b), c) => a, _ => "" }
    , "inner", .no_trace);
}

test "str refcount pattern - nested tuple extract middle" {
    // Verify extracting from nested position
    try runExpectStr(
        \\match (("first", "second"), "third") { ((a, b), c) => b, _ => "" }
    , "second", .no_trace);
}

test "str refcount pattern - nested tuple extract outer" {
    // Verify extracting outer element
    try runExpectStr(
        \\match (("inner", "other"), "outer") { ((a, b), c) => c, _ => "" }
    , "outer", .no_trace);
}

test "str refcount pattern - nested tuple with wildcards" {
    // Verify wildcards in nested structure
    try runExpectStr(
        \\match (("used", "unused"), "also_unused") { ((a, _), _) => a, _ => "" }
    , "used", .no_trace);
}

// Record destructuring via match

test "str refcount pattern - record destructure single field" {
    // Verify record destructuring extracts string
    try runExpectStr(
        \\match {s: "value"} { {s} => s, _ => "" }
    , "value", .no_trace);
}

test "str refcount pattern - record destructure multiple fields" {
    // Verify destructuring multiple fields
    try runExpectStr(
        \\match {a: "first", b: "second"} { {a, b} => a, _ => "" }
    , "first", .no_trace);
}

test "str refcount pattern - record destructure return second field" {
    // Verify returning second field
    try runExpectStr(
        \\match {a: "first", b: "second"} { {a, b} => b, _ => "" }
    , "second", .no_trace);
}

test "str refcount pattern - partial record destructure" {
    // Verify partial destructuring decrefs unused fields
    try runExpectStr(
        \\match {a: "used", b: "unused"} { {a} => a, _ => "" }
    , "used", .no_trace);
}

test "str refcount pattern - record with duplicate strings" {
    // Verify record with same string in multiple fields
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    r = {a: x, b: x}
        \\    match r { {a, b} => a, _ => "" }
        \\}
    , "shared", .no_trace);
}

// Nested record destructuring

test "str refcount pattern - nested record destructure" {
    // Verify nested record destructuring
    try runExpectStr(
        \\match {outer: {inner: "value"}} { {outer} => match outer { {inner} => inner, _ => "" }, _ => "" }
    , "value", .no_trace);
}

// Mixed tuple and record destructuring

test "str refcount pattern - tuple with record element" {
    // Verify destructuring tuple containing record
    try runExpectStr(
        \\match ({a: "rec"}, "str") { (r, s) => s, _ => "" }
    , "str", .no_trace);
}

test "str refcount pattern - record with tuple element" {
    // Verify destructuring record containing tuple
    try runExpectStr(
        \\match {t: ("first", "second"), s: "other"} { {t, s} => s, _ => "" }
    , "other", .no_trace);
}

// Complex destructuring patterns

test "str refcount pattern - multiple match branches with strings" {
    // Verify different match branches (using wildcard patterns only)
    try runExpectStr(
        \\{
        \\    x = ("first", "second")
        \\    match x { (a, b) => b, _ => "" }
        \\}
    , "second", .no_trace);
}

test "str refcount pattern - match with tag destructuring" {
    // Verify tag destructuring with string payload
    try runExpectStr(
        \\match Ok("payload") { Ok(s) => s, Err(_) => "" }
    , "payload", .no_trace);
}

test "str refcount pattern - match tag with tuple payload" {
    // Verify destructuring tag with tuple containing strings
    try runExpectStr(
        \\match Ok(("first", "second")) { Ok((a, b)) => b, Err(_) => "" }
    , "second", .no_trace);
}

test "str refcount pattern - match tag with record payload" {
    // Verify destructuring tag with record containing strings
    try runExpectStr(
        \\match Err({msg: "error"}) { Ok(_) => "", Err({msg}) => msg }
    , "error", .no_trace);
}

// TODO: Add tests for standalone destructuring patterns once let-binding patterns
// are fully supported in the interpreter:
// - Direct tuple destructuring: `(a, b) = (x, y)`
// - Direct record destructuring: `{s} = {s: x}`
// - Nested destructuring in let bindings
//
// For now, Phase 5-7 tests combined with these match-based destructuring tests
// provide solid coverage of string refcounting in pattern contexts.
