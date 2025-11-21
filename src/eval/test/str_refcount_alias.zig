//! String aliasing refcounting tests - Phase 2
//!
//! These tests verify string refcounting when the same string is referenced multiple times:
//! - Variable aliasing (multiple names for same string)
//! - Returning different aliases
//! - Multiple levels of aliasing
//! - Shadowing behavior
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectStr = helpers.runExpectStr;

test "str refcount alias - variable aliasing" {
    // Test that aliasing a string increments its refcount
    try runExpectStr(
        \\{
        \\    x = "test"
        \\    y = x
        \\    y
        \\}
    , "test", .no_trace);
}

test "str refcount alias - return original after aliasing" {
    // Test that both the original and alias are valid
    try runExpectStr(
        \\{
        \\    x = "test"
        \\    y = x
        \\    x
        \\}
    , "test", .no_trace);
}

test "str refcount alias - triple aliasing" {
    // Test multiple levels of aliasing
    try runExpectStr(
        \\{
        \\    x = "test"
        \\    y = x
        \\    z = y
        \\    z
        \\}
    , "test", .no_trace);
}

test "str refcount alias - return middle of triple alias" {
    // Return the middle alias
    try runExpectStr(
        \\{
        \\    x = "test"
        \\    y = x
        \\    z = y
        \\    y
        \\}
    , "test", .no_trace);
}

test "str refcount alias - return first of triple alias" {
    // Return the original
    try runExpectStr(
        \\{
        \\    x = "test"
        \\    y = x
        \\    z = y
        \\    x
        \\}
    , "test", .no_trace);
}

test "str refcount alias - large string aliasing" {
    // Test aliasing with heap-allocated strings
    const large_str = "This is a very long string that will definitely be heap allocated";
    try runExpectStr(
        \\{
        \\    x = "This is a very long string that will definitely be heap allocated"
        \\    y = x
        \\    y
        \\}
    , large_str, .no_trace);
}

test "str refcount alias - empty string aliasing" {
    // Test aliasing with empty string
    try runExpectStr(
        \\{
        \\    x = ""
        \\    y = x
        \\    y
        \\}
    , "", .no_trace);
}

test "str refcount alias - shadowing with different string" {
    // Test that shadowing properly decrefs the old value
    try runExpectStr(
        \\{
        \\    x = "first"
        \\    x = "second"
        \\    x
        \\}
    , "second", .no_trace);
}

test "str refcount alias - shadowing after aliasing" {
    // Shadow after creating an alias
    try runExpectStr(
        \\{
        \\    x = "first"
        \\    y = x
        \\    x = "second"
        \\    x
        \\}
    , "second", .no_trace);
}

test "str refcount alias - alias still valid after shadowing" {
    // Verify the alias is still valid after shadowing the original
    try runExpectStr(
        \\{
        \\    x = "first"
        \\    y = x
        \\    x = "second"
        \\    y
        \\}
    , "first", .no_trace);
}

test "str refcount alias - multiple aliases of same string" {
    // Create multiple aliases simultaneously
    try runExpectStr(
        \\{
        \\    x = "test"
        \\    y = x
        \\    z = x
        \\    z
        \\}
    , "test", .no_trace);
}

test "str refcount alias - four-way aliasing return last" {
    // Test with four references, returning the last one
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    a = x
        \\    b = x
        \\    c = x
        \\    c
        \\}
    , "shared", .no_trace);
}

test "str refcount alias - four-way aliasing return first" {
    // Test with four references, returning the first
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    a = x
        \\    b = x
        \\    c = x
        \\    x
        \\}
    , "shared", .no_trace);
}

test "str refcount alias - four-way aliasing return second" {
    // Test with four references, returning the second
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    a = x
        \\    b = x
        \\    c = x
        \\    a
        \\}
    , "shared", .no_trace);
}

test "str refcount alias - simpler four aliases" {
    // Simplest four-way test - all aliases, return third
    try runExpectStr(
        \\{
        \\    a = "s"
        \\    b = a
        \\    c = a
        \\    d = a
        \\    c
        \\}
    , "s", .no_trace);
}

test "str refcount alias - four-way with longer string" {
    // Test with longer string
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    a = x
        \\    b = x
        \\    c = x
        \\    c
        \\}
    , "shared", .no_trace);
}

test "str refcount alias - five-way return second-to-last" {
    // Test with five references, returning second-to-last
    try runExpectStr(
        \\{
        \\    x = "test"
        \\    a = x
        \\    b = x
        \\    c = x
        \\    d = x
        \\    c
        \\}
    , "test", .no_trace);
}

test "str refcount alias - four-way different names" {
    // Test with different variable names
    try runExpectStr(
        \\{
        \\    w = "shared"
        \\    y = w
        \\    z = w
        \\    q = w
        \\    z
        \\}
    , "shared", .no_trace);
}

test "str refcount alias - test with just b" {
    // Simple test with b
    try runExpectStr(
        \\{
        \\    b = "test"
        \\    b
        \\}
    , "test", .no_trace);
}

test "str refcount alias - THIS IS THE PROBLEMATIC TEST" {
    // Test with four references, returning a middle one
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    a = x
        \\    b = x
        \\    c = x
        \\    b
        \\}
    , "shared", .no_trace);
}

test "str refcount alias - test x,y,b,c return b" {
    // Try with y instead of a
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    y = x
        \\    b = x
        \\    c = x
        \\    b
        \\}
    , "shared", .no_trace);
}

test "str refcount alias - test a,b,c,d return b" {
    // Try without x at all - returns binding 1
    try runExpectStr(
        \\{
        \\    a = "shared"
        \\    b = a
        \\    c = a
        \\    d = a
        \\    b
        \\}
    , "shared", .no_trace);
}

test "str refcount alias - test a,b,c,d return d" {
    // Try returning binding 3
    try runExpectStr(
        \\{
        \\    a = "shared"
        \\    b = a
        \\    c = a
        \\    d = a
        \\    d
        \\}
    , "shared", .no_trace);
}

test "str refcount alias - simple two-way with b" {
    // Simplest test with b
    try runExpectStr(
        \\{
        \\    a = "test"
        \\    b = a
        \\    b
        \\}
    , "test", .no_trace);
}

test "str refcount alias - three-way a,b,c return b" {
    // Three-way with b in middle
    try runExpectStr(
        \\{
        \\    a = "test"
        \\    b = a
        \\    c = a
        \\    b
        \\}
    , "test", .no_trace);
}

test "str refcount alias - five-way a,b,c,d,e return b" {
    // Five-way with b
    try runExpectStr(
        \\{
        \\    a = "test"
        \\    b = a
        \\    c = a
        \\    d = a
        \\    e = a
        \\    b
        \\}
    , "test", .no_trace);
}

test "str refcount alias - minimal 4-way just return b at end" {
    // Absolute minimal test - 4 bindings, return b last
    try runExpectStr(
        \\{
        \\    a = "x"
        \\    b = a
        \\    c = a
        \\    d = a
        \\    b
        \\}
    , "x", .no_trace);
}

test "str refcount alias - 4-way return second variable bb" {
    // Try with bb instead of b
    try runExpectStr(
        \\{
        \\    a = "x"
        \\    bb = a
        \\    c = a
        \\    d = a
        \\    bb
        \\}
    , "x", .no_trace);
}

test "str refcount alias - 4-way with different order aaa,b,ccc,ddd" {
    // Try with varied length names
    try runExpectStr(
        \\{
        \\    aaa = "x"
        \\    b = aaa
        \\    ccc = aaa
        \\    ddd = aaa
        \\    b
        \\}
    , "x", .no_trace);
}

test "str refcount alias - original pattern but with x string" {
    // Original failing pattern but with "x" instead of "shared"
    try runExpectStr(
        \\{
        \\    x = "x"
        \\    a = x
        \\    b = x
        \\    c = x
        \\    b
        \\}
    , "x", .no_trace);
}

test "str refcount alias - source named y instead of x" {
    // Test if it's about the source being named x
    try runExpectStr(
        \\{
        \\    y = "x"
        \\    a = y
        \\    b = y
        \\    c = y
        \\    b
        \\}
    , "x", .no_trace);
}

test "str refcount alias - source named z" {
    // Another non-x source name
    try runExpectStr(
        \\{
        \\    z = "x"
        \\    a = z
        \\    b = z
        \\    c = z
        \\    b
        \\}
    , "x", .no_trace);
}

test "str refcount alias - source named w" {
    // Test another late-alphabet letter
    try runExpectStr(
        \\{
        \\    w = "x"
        \\    a = w
        \\    b = w
        \\    c = w
        \\    b
        \\}
    , "x", .no_trace);
}

test "str refcount alias - source named m" {
    // Test middle of alphabet
    try runExpectStr(
        \\{
        \\    m = "x"
        \\    a = m
        \\    b = m
        \\    c = m
        \\    b
        \\}
    , "x", .no_trace);
}

test "str refcount alias - a,b,c,d with string shared again" {
    // Same variables as passing test, but with "shared" string
    try runExpectStr(
        \\{
        \\    a = "shared"
        \\    b = a
        \\    c = a
        \\    d = a
        \\    b
        \\}
    , "shared", .no_trace);
}

test "str refcount alias - x,a,b,c with single char s" {
    // Original failing pattern with single char
    try runExpectStr(
        \\{
        \\    x = "s"
        \\    a = x
        \\    b = x
        \\    c = x
        \\    b
        \\}
    , "s", .no_trace);
}

test "str refcount alias - DEBUG change x to y with s" {
    // Change first variable from x to y
    try runExpectStr(
        \\{
        \\    y = "s"
        \\    a = y
        \\    b = y
        \\    c = y
        \\    b
        \\}
    , "s", .no_trace);
}

test "str refcount alias - DEBUG change string from s to t" {
    // Change string from s to t
    try runExpectStr(
        \\{
        \\    x = "t"
        \\    a = x
        \\    b = x
        \\    c = x
        \\    b
        \\}
    , "t", .no_trace);
}
