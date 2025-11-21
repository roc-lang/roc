//! String refcounting tests - Phase 6: Conditionals with Strings
//!
//! These tests verify string refcounting when strings are used in if-else expressions.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectStr = helpers.runExpectStr;
const runExpectInt = helpers.runExpectInt;

// Basic conditionals with strings

test "str refcount conditional - simple if-else with string" {
    // Verify string returned from then branch
    try runExpectStr(
        \\if True "then" else "else"
    , "then", .no_trace);
}

test "str refcount conditional - return from else branch" {
    // Verify string returned from else branch
    try runExpectStr(
        \\if False "then" else "else"
    , "else", .no_trace);
}

test "str refcount conditional - strings in blocks" {
    // Verify strings work in block contexts
    try runExpectStr(
        \\{
        \\    x = "selected"
        \\    y = "not_selected"
        \\    if True x else y
        \\}
    , "selected", .no_trace);
}

test "str refcount conditional - else branch selected from block" {
    // Verify else branch with string from block
    try runExpectStr(
        \\{
        \\    x = "not_selected"
        \\    y = "selected"
        \\    if False x else y
        \\}
    , "selected", .no_trace);
}

// Same string in both branches

test "str refcount conditional - same string both branches" {
    // Verify same string literal in both branches
    try runExpectStr(
        \\if True "same" else "same"
    , "same", .no_trace);
}

test "str refcount conditional - same variable both branches" {
    // Verify same variable in both branches
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    if True x else x
        \\}
    , "shared", .no_trace);
}

test "str refcount conditional - same variable both branches else taken" {
    // Verify same variable when else branch taken
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    if False x else x
        \\}
    , "shared", .no_trace);
}

// Unused branch refcounting

test "str refcount conditional - unused then branch decreffed" {
    // Verify then branch value is properly decreffed when not taken
    try runExpectStr(
        \\{
        \\    x = "not_taken"
        \\    y = "taken"
        \\    if False x else y
        \\}
    , "taken", .no_trace);
}

test "str refcount conditional - unused else branch decreffed" {
    // Verify else branch value is properly decreffed when not taken
    try runExpectStr(
        \\{
        \\    x = "taken"
        \\    y = "not_taken"
        \\    if True x else y
        \\}
    , "taken", .no_trace);
}

// Nested conditionals

test "str refcount conditional - nested if-else then-then" {
    // Verify nested conditionals with strings
    try runExpectStr(
        \\if True (if True "inner_then" else "inner_else") else "outer_else"
    , "inner_then", .no_trace);
}

test "str refcount conditional - nested if-else then-else" {
    // Verify nested then-else path
    try runExpectStr(
        \\if True (if False "inner_then" else "inner_else") else "outer_else"
    , "inner_else", .no_trace);
}

test "str refcount conditional - nested if-else else" {
    // Verify nested else path
    try runExpectStr(
        \\if False (if True "inner_then" else "inner_else") else "outer_else"
    , "outer_else", .no_trace);
}

test "str refcount conditional - nested with variables" {
    // Verify nested conditionals with variables
    try runExpectStr(
        \\{
        \\    a = "a"
        \\    b = "b"
        \\    c = "c"
        \\    if True (if False a else b) else c
        \\}
    , "b", .no_trace);
}

// Edge cases

test "str refcount conditional - empty string in then" {
    // Verify empty string in then branch
    try runExpectStr(
        \\if True "" else "else"
    , "", .no_trace);
}

test "str refcount conditional - empty string in else" {
    // Verify empty string in else branch
    try runExpectStr(
        \\if False "then" else ""
    , "", .no_trace);
}

test "str refcount conditional - large string in conditional" {
    // Verify large heap-allocated string in conditional
    const large = "This is a very long string that exceeds small string optimization";
    try runExpectStr(
        \\if True "This is a very long string that exceeds small string optimization" else "Short"
    , large, .no_trace);
}

test "str refcount conditional - large string in else" {
    // Verify large string from else branch
    const large = "This is a very long string that exceeds small string optimization";
    try runExpectStr(
        \\if False "Short" else "This is a very long string that exceeds small string optimization"
    , large, .no_trace);
}

test "str refcount conditional - small string optimization" {
    // Verify small string (inline storage)
    try runExpectStr(
        \\if True "small" else "tiny"
    , "small", .no_trace);
}

// Complex patterns

test "str refcount conditional - aliased strings in branches" {
    // Verify aliased strings work in conditionals
    try runExpectStr(
        \\{
        \\    x = "original"
        \\    y = x
        \\    z = x
        \\    if True y else z
        \\}
    , "original", .no_trace);
}

test "str refcount conditional - string used before and after conditional" {
    // Verify string used outside conditional is preserved
    try runExpectStr(
        \\{
        \\    x = "preserved"
        \\    y = if True "conditional" else "other"
        \\    x
        \\}
    , "preserved", .no_trace);
}

test "str refcount conditional - conditional result stored" {
    // Verify storing conditional result
    try runExpectStr(
        \\{
        \\    x = "a"
        \\    y = "b"
        \\    result = if True x else y
        \\    result
        \\}
    , "a", .no_trace);
}

test "str refcount conditional - conditional result stored else" {
    // Verify storing else result
    try runExpectStr(
        \\{
        \\    x = "a"
        \\    y = "b"
        \\    result = if False x else y
        \\    result
        \\}
    , "b", .no_trace);
}

// Conditionals with complex boolean expressions

test "str refcount conditional - with and condition true" {
    // Verify conditional with and expression
    try runExpectStr(
        \\if True and True "both_true" else "not_both"
    , "both_true", .no_trace);
}

test "str refcount conditional - with and condition false" {
    // Verify and expression false case
    try runExpectStr(
        \\if True and False "both_true" else "not_both"
    , "not_both", .no_trace);
}

test "str refcount conditional - with or condition true" {
    // Verify conditional with or expression
    try runExpectStr(
        \\if False or True "any_true" else "both_false"
    , "any_true", .no_trace);
}

test "str refcount conditional - with or condition false" {
    // Verify or expression false case
    try runExpectStr(
        \\if False or False "any_true" else "both_false"
    , "both_false", .no_trace);
}

test "str refcount conditional - with negation true" {
    // Verify conditional with negation
    try runExpectStr(
        \\if !False "negated" else "not_negated"
    , "negated", .no_trace);
}

test "str refcount conditional - with negation false" {
    // Verify negation false case
    try runExpectStr(
        \\if !True "negated" else "not_negated"
    , "not_negated", .no_trace);
}
