//! Tests for compile-time constant evaluation in the dev backend
//!
//! These tests verify that constants are evaluated at compile time and their
//! values are correctly used when generating code for the main expression.

const std = @import("std");
const helpers = @import("helpers.zig");

const runExpectI64 = helpers.runExpectI64;
const runExpectBool = helpers.runExpectBool;
const runExpectStr = helpers.runExpectStr;

// ============================================================================
// Basic constant evaluation tests
// ============================================================================

test "constant eval - simple integer constant" {
    // A simple constant should be evaluated at compile time
    // and its value used when referenced
    try runExpectI64(
        \\{
        \\    x = 42
        \\    x
        \\}
    , 42, .no_trace);
}

test "constant eval - constant used in arithmetic" {
    // Constants should be usable in arithmetic expressions
    try runExpectI64(
        \\{
        \\    x = 10
        \\    x + 5
        \\}
    , 15, .no_trace);
}

test "constant eval - constant used multiple times" {
    // A constant evaluated once should be usable multiple times
    try runExpectI64(
        \\{
        \\    x = 7
        \\    x + x + x
        \\}
    , 21, .no_trace);
}

test "constant eval - multiple constants" {
    // Multiple constants should each be evaluated
    try runExpectI64(
        \\{
        \\    a = 10
        \\    b = 20
        \\    c = 12
        \\    a + b + c
        \\}
    , 42, .no_trace);
}

test "constant eval - constant arithmetic expression" {
    // Constants with arithmetic expressions should be folded
    try runExpectI64(
        \\{
        \\    x = 1 + 2
        \\    x
        \\}
    , 3, .no_trace);

    try runExpectI64(
        \\{
        \\    x = 6 * 7
        \\    x
        \\}
    , 42, .no_trace);
}

test "constant eval - constant referencing another constant" {
    // Constants can reference other constants that were already evaluated
    try runExpectI64(
        \\{
        \\    x = 10
        \\    y = x + 5
        \\    y
        \\}
    , 15, .no_trace);
}

test "constant eval - chain of constant references" {
    // A chain of constants where each references the previous
    try runExpectI64(
        \\{
        \\    a = 1
        \\    b = a + 1
        \\    c = b + 1
        \\    d = c + 1
        \\    d
        \\}
    , 4, .no_trace);
}

test "constant eval - constant in conditional" {
    // Constants should work correctly in conditionals
    try runExpectI64(
        \\{
        \\    threshold = 50
        \\    value = 75
        \\    if value > threshold 1 else 0
        \\}
    , 1, .no_trace);

    try runExpectI64(
        \\{
        \\    threshold = 50
        \\    value = 25
        \\    if value > threshold 1 else 0
        \\}
    , 0, .no_trace);
}

// ============================================================================
// Negative numbers and edge cases
// ============================================================================

test "constant eval - negative constant" {
    try runExpectI64(
        \\{
        \\    x = -42
        \\    x
        \\}
    , -42, .no_trace);
}

test "constant eval - zero constant" {
    try runExpectI64(
        \\{
        \\    x = 0
        \\    x + 100
        \\}
    , 100, .no_trace);
}

test "constant eval - large constant" {
    try runExpectI64(
        \\{
        \\    x = 1000000000
        \\    x
        \\}
    , 1000000000, .no_trace);
}

// ============================================================================
// Boolean constants
// ============================================================================

test "constant eval - boolean true constant" {
    try runExpectBool(
        \\{
        \\    x = True
        \\    x
        \\}
    , true, .no_trace);
}

test "constant eval - boolean false constant" {
    try runExpectBool(
        \\{
        \\    x = False
        \\    x
        \\}
    , false, .no_trace);
}

test "constant eval - boolean constant in condition" {
    try runExpectI64(
        \\{
        \\    flag = True
        \\    if flag 42 else 0
        \\}
    , 42, .no_trace);

    try runExpectI64(
        \\{
        \\    flag = False
        \\    if flag 42 else 0
        \\}
    , 0, .no_trace);
}

// ============================================================================
// Constants with complex expressions
// ============================================================================

test "constant eval - constant with nested arithmetic" {
    try runExpectI64(
        \\{
        \\    x = (1 + 2) * (3 + 4)
        \\    x
        \\}
    , 21, .no_trace);
}

test "constant eval - constant with comparison result" {
    try runExpectBool(
        \\{
        \\    x = 10
        \\    y = 20
        \\    result = x < y
        \\    result
        \\}
    , true, .no_trace);
}

test "constant eval - constant used in record field" {
    try runExpectI64(
        \\{
        \\    value = 42
        \\    record = { x: value }
        \\    record.x
        \\}
    , 42, .no_trace);
}

// ============================================================================
// Multiple uses of evaluated constants
// ============================================================================

test "constant eval - same constant in multiple expressions" {
    // The constant should be evaluated once and reused
    try runExpectI64(
        \\{
        \\    base = 10
        \\    a = base + 1
        \\    b = base + 2
        \\    c = base + 3
        \\    a + b + c
        \\}
    , 36, .no_trace); // (10+1) + (10+2) + (10+3) = 11 + 12 + 13 = 36
}

test "constant eval - computed constant used in final expression" {
    try runExpectI64(
        \\{
        \\    width = 5
        \\    height = 4
        \\    area = width * height
        \\    area
        \\}
    , 20, .no_trace);
}

// ============================================================================
// String constants
// ============================================================================

test "constant eval - string constant" {
    try runExpectStr(
        \\{
        \\    greeting = "hello"
        \\    greeting
        \\}
    , "hello", .no_trace);
}

// Note: String equality (x == y for strings) is not yet implemented in the dev backend
// so we can't test "constant eval - string constant used twice" with a bool comparison
